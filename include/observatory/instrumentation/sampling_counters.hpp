#pragma once

#include <stdexcept>
#include <fstream>
#include <map>
#include <list>

#include <unistd.h>
#include <stddef.h>
#include <syscall.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <string.h>

#include <elf.h>

#include <linux/perf_event.h>

namespace observatory {

struct Chunk {
    void* data;
    size_t size;
};


class BoundedRecorder;
class ThreadAwareRecorder;


namespace internal {

inline size_t page_size() {
    static const size_t pgsize = ::sysconf(_SC_PAGE_SIZE);
    return pgsize;
}

// Trivial wrapper around perf_event_open() syscall to provide type-checking
// for args.
inline int
sys_perf_event_open(struct perf_event_attr *attr, pid_t pid, int cpu,
                    int group_fd, unsigned long flags)
{
    return syscall(__NR_perf_event_open, attr, pid, cpu, group_fd, flags);
}

inline pid_t
sys_gettid()
{
    return syscall(__NR_gettid);
}

// Struct definitions from the in-kernel description at
// `perf.data-file-format.txt`.
struct perf_file_section {
    uint64_t offset;
    uint64_t size;
} __attribute__((packed));

struct perf_file_attr {
    struct perf_event_attr attr;
    struct perf_file_section ids;
} __attribute__((packed));

struct perf_file_header {
    uint64_t magic; // int when writing; char magic[8] when reading from disk
    uint64_t size;
    uint64_t attr_size;
    struct perf_file_section attrs;
    struct perf_file_section data;
    struct perf_file_section event_types;
    uint64_t flags;
    uint64_t flags1[3];
} __attribute__((packed));

struct perf_buildid_event {
    struct perf_event_header header;
    pid_t pid; // No idea why this is in here.
    // The build id inserted by gcc (and printed by `perf buildid-list`) is
    // only 20 byte, not sure how it can tell the end.
    uint8_t build_id[24];
    // char filename[];
} __attribute__((packed));

struct perf_mmap_event {
    struct perf_event_header header;
    uint32_t pid, tid;
    uint64_t addr;
    uint64_t len;
    uint64_t pgoff;
    // char filename[];
} __attribute__((packed));

constexpr int PERF_RECORD_HEADER_BUILD_ID = 67;


// Mini ELF parser to read build id from note section.
inline std::string read_buildid(const char* file)
{
    if (file[0] == '[') { // [vdso] or [kernel.kallsyms]
        return ""; // FIXME - read correct id from /sys/[...]/notes
    }

    std::ifstream ifs(file);
    char eident[16];
    ifs.read(eident, 16);
    if (strncmp(eident, "\x7f""ELF", 4))
        // No need to throw here, just print an error and skip this
        throw std::runtime_error("not an elf file");

    if (eident[EI_CLASS] != ELFCLASS64)
        throw std::runtime_error("sorry, 32-bit elf not yet implemented ._.");

    ifs.seekg(offsetof(Elf64_Ehdr, e_shoff));
    Elf64_Off shoff;
    ifs.read(reinterpret_cast<char*>(&shoff), sizeof(shoff));

    ifs.seekg(offsetof(Elf64_Ehdr, e_shnum));
    Elf64_Half shnum;
    ifs.read(reinterpret_cast<char*>(&shnum), sizeof(shnum));

    ifs.seekg(shoff);

    Elf64_Off note_offset = 0;

    for (int i=0; i<shnum; ++i) {
        Elf64_Off section = shoff + i*sizeof(Elf64_Shdr);

        ifs.seekg(section + offsetof(Elf64_Shdr, sh_size));
        Elf64_Word size;
        ifs.read(reinterpret_cast<char*>(&size), sizeof(size));

        ifs.seekg(section + offsetof(Elf64_Shdr, sh_type));
        Elf64_Word type;
        ifs.read(reinterpret_cast<char*>(&type), sizeof(type));

        // We're too poor to afford a real ELF parser, so just take the first
        // note section that's 0x24 bytes long.
        if (type == SHT_NOTE && size == 0x24) {
            ifs.seekg(section + offsetof(Elf64_Shdr, sh_offset));
            ifs.read(
                reinterpret_cast<char*>(&note_offset), sizeof(note_offset));

            break;
        }
    }

    if (!note_offset)
        // No need to throw, just print warning and return empty string.
        throw std::runtime_error("no note section found");

    uint32_t note_header[3]; // a tuple (namesz, descsz, type) of 4-byte words
    ifs.seekg(note_offset);
    ifs.read(reinterpret_cast<char*>(&note_header[0]), sizeof(uint32_t));
    ifs.read(reinterpret_cast<char*>(&note_header[1]), sizeof(uint32_t));
    ifs.read(reinterpret_cast<char*>(&note_header[2]), sizeof(uint32_t));

    // Default format is 4-byte name "GNU", 20-byte build id and
    // type 3 == NT_GNU_BUILD_ID
    if (note_header[0] != 4 || note_header[1] != 20 || note_header[2] != 3)
       throw std::runtime_error("sorry, can't parse build-id note :/");

    char note[24];
    ifs.read(note, sizeof(note));

    return std::string(note+4, note+24);
}

inline void write_perfdata(
    std::ofstream& f,
    const struct perf_event_attr& attr,
    std::list<Chunk> dataChunks) // FIXME - list is mostly an artifact
{
    pid_t pid = ::getpid();
    // Not sure if setting the correct tid would break things.
    // pid_t tid = internal::gettid();

    struct perf_file_header header = {0};
    // "PERFILE2". Written as int because its used for endian-ness detection
    header.magic = 0x32454c4946524550ULL;
    header.size = sizeof(struct perf_file_header);
    header.attr_size = sizeof(struct perf_event_attr);
    // According to the perf source code, header.event_types is ignored.

    // Should be zero for a valid perf.data file, but maybe the caller is
    // embedding this in the middle of a tar archive or similar.
    int initial_position = f.tellp();
    f.write(reinterpret_cast<const char*>(&header), sizeof(header));

    // The perf infrastructure requires some information to be able to
    // connect addresses to symbol names.
    //  1) mmap information, i.e. which parts of which files are loaded where
    //     into executable memory
    //  2) build ids, i.e. unique identifiers of participating binaries
    // For the second part, perf implements a small ELF parser to read the id's
    // from the binaries itself (where the compiler places them in NOTE
    // sections).
    // For the first part, i think it will either listen for the SAMPLE_MMAP
    // events from the kernel, or alternatively read /proc/pid/maps when
    // attaching to an existing process.
    // We attempt to copy the second approach here, i.e. parsing /proc/pid/maps
    // to synthesize the required data.

    int attrs_offset = f.tellp();
    struct perf_file_attr file_attr = {0};
    file_attr.attr = attr;
    file_attr.ids.offset = 0; // todo
    file_attr.ids.size = 0; // todo
    // FIXME - add section containing event_id_
    f.write(reinterpret_cast<const char*>(&file_attr), sizeof(file_attr));

    int data_offset = f.tellp();
    std::ifstream ifs("/proc/self/maps");
    std::string line;
    while (std::getline(ifs, line)) {
        // todo - what do `number` and `segments` mean?
        uint64_t start, end, offset, number;
        char segments[128];
        char long_name[256];
        char r, w, x, p, newline;
        int items = sscanf(line.c_str(),
            "%lx-%lx %c%c%c%c %lx %s %lu%256[^\n]%c",
            &start, &end, &r, &w, &x, &p, &offset,
            reinterpret_cast<char*>(&segments), &number,
            reinterpret_cast<char*>(&long_name), &newline);

        if (items == 9) { // no filename
            continue;
        } else if (items != 10) { // eof
            break;
        } else if (x != 'x') { // not an exectuable mapping
            continue;
        }

        // Assume that we don't have whitespace *after* the filename.
        char* name_start = strrchr(long_name, ' ') + 1;

        struct perf_mmap_event event = {0};
        event.header.type = PERF_RECORD_MMAP;
        event.header.misc = 0;
        event.header.size =
            sizeof(struct perf_mmap_event) + strlen(name_start) + 1;

        event.pid = pid;
        // I don't really understand the significance of the tid here, but all
        // perf dumps seems to just set it to the same is pid anyways?
        event.tid = pid;
        event.addr = start;
        event.len = end - start;
        event.pgoff = offset;
        f.write(reinterpret_cast<const char*>(&event), sizeof(event));
        f.write(name_start, strlen(name_start) + 1);

        // We can also put build id's into a PERF_HEADER_BUILD_ID header,
        // but injecting events seems a little bit more comfortable.
        std::string buildid = read_buildid(name_start);
        struct perf_buildid_event buildid_event = {0};
        buildid_event.header.type = PERF_RECORD_HEADER_BUILD_ID;
        buildid_event.header.misc = 0;
        buildid_event.header.size =
            sizeof(struct perf_buildid_event) + strlen(name_start) + 1;

        buildid_event.pid = pid;
        memcpy(&buildid_event.build_id, buildid.c_str(), buildid.size());
        f.write(reinterpret_cast<const char*>(&buildid_event),
            sizeof(buildid_event));

        f.write(name_start, strlen(name_start) + 1);
    }
    ifs.close();

    for (auto chunk : dataChunks) {
        f.write(static_cast<const char*>(chunk.data), chunk.size);
    }

    int final_offset = f.tellp();

    // Now fix up the file section data in the header.
    header.attrs.offset = attrs_offset;
    header.attrs.size = data_offset - attrs_offset;
    header.data.offset = data_offset;
    header.data.size = final_offset - data_offset;

    int attr_section_offset = offsetof(struct perf_file_header, attrs);
    f.seekp(initial_position + attr_section_offset, std::ios_base::beg);
    f.write(reinterpret_cast<const char*>(&header.attrs),
        sizeof(struct perf_file_section));

    f.write(reinterpret_cast<const char*>(&header.data),
        sizeof(struct perf_file_section));

    f.seekp(0, std::ios_base::end);
}


/*
uint64_t pagealign_down(uint64_t addr)
{
    size_t page_size = internal::page_size();
    return addr & (~(page_size-1));
}
*/

} // namespace internal

// Ideally, `drain()` is called while the recorder is stopped,
// otherwise data that is concurrently written might get missed.

enum CounterType {
    Instructions = PERF_COUNT_HW_INSTRUCTIONS,
    Cycles = PERF_COUNT_HW_CPU_CYCLES,
};


class BoundedRecorder {
public:
    // FIXME - I think 5k cycles is the default used by perf, but verify that.
    BoundedRecorder(size_t period = 5000, CounterType = CounterType::Cycles);
    ~BoundedRecorder();

    void enable();
    void disable();

    // Useful for stand-alone use.
    void drain(const char* f);

    // Useful for use within other containers.
    size_t extractChunk(void* data, size_t maxlen);

private:
    BoundedRecorder(const BoundedRecorder&) = delete;
    BoundedRecorder& operator=(const BoundedRecorder&) = delete;

    friend class ThreadAwareRecorder;

    // SAMPLE_TID is important to correlate mmap information with callchain
    // samples. (since a counter can track multiple processes)
    // Theoretically IP is included in CALLCHAIN, but `perf script` complains
    // if it doesn't get the ip as a separate field.
    // TODO(bevers): Figure out if STREAM_ID is necessary, and what it does.
    static const uint64_t SAMPLE_TYPE =
        PERF_SAMPLE_IDENTIFIER | PERF_SAMPLE_TIME | PERF_SAMPLE_IP
        | PERF_SAMPLE_CALLCHAIN | PERF_SAMPLE_TID;

  struct perf_event_attr pea_; // const

  int fd_;
  // Not sure what exactly the event id represents, but the kernel assigns one
  // to each fd and perf can read it.
  int event_id_;

  struct perf_event_mmap_page *base_;
  size_t mmap_size_;

  char* ring_base_;
  uint64_t ring_size_;
  volatile __u64 *head_;
  volatile __u64 *tail_;
};


inline
BoundedRecorder::BoundedRecorder(size_t period, CounterType type)
{
    pea_ = {0}; // todo - is this *really* the correct syntax?

    // When using the "dummy" counter, no eventes get recorded.
    // I *think* how it works is that the period is set in terms
    // of the counter itself, i.e. we now sample every 5k cpu
    // instructions.
    pea_.type = PERF_TYPE_HARDWARE;
    pea_.config = static_cast<perf_hw_id>(type);
    pea_.sample_period = period;
    pea_.sample_type = BoundedRecorder::SAMPLE_TYPE;
    pea_.size = sizeof(struct perf_event_attr);
    pea_.exclude_callchain_kernel = 1;
    pea_.exclude_kernel = 1; // FIXME - Not sure what exactly this excludes.
    pea_.disabled = 1;

    // We already set up all the stuff here, so in `start()` we just have
    // to enable the counter.
    const int pid = 0; // Attach to current task. todo - thread or process?
    const int cpu = -1; // Count on all cpu's
    const int group_fd = -1; // Create counter in a new group
    const int flags = PERF_FLAG_FD_CLOEXEC;
    fd_ = internal::sys_perf_event_open(&pea_, pid,  cpu, group_fd, flags);

    if (fd_ < 0) {
        // todo - print all this stuff into the exception
        printf("Error %d: %s\n", errno, strerror(errno));
        if (errno == EACCES) {
            printf("Try setting /proc/sys/kernel/perf_event_paranoid to 1\n");
        }
        if (errno == ENOSYS) {
            printf("No CONFIG_PERF_EVENTS=y kernel support configured?\n");
        }
        throw std::runtime_error("perf_event_open()");
    }

    // todo - check return value
    ioctl(fd_, PERF_EVENT_IOC_ID, &event_id_);

    // We basically want to get the buffer as big as possible, but at some
    // point the kernel just refuses, so just take 512KiB for now.
    size_t bytes = 512u * 1024u;
    size_t pgsize = internal::page_size();
    size_t pages = bytes / pgsize;
    if (pages * pgsize < bytes) {
        ++pages;
    }
    mmap_size_ = pgsize * (pages + 1); // n data pages and one control page

    // Passing PROT_WRITE implies that the `tail` entry should be user-writable,
    // and the kernel will not overwrite data until older data has been written.
    // This implies that we will lose all overrun data, but at least we know
    // that the start of the buffer will always be the start of a valid event
    // when we write to file at the end.
    void* mapping = mmap(
        nullptr, mmap_size_, PROT_READ | PROT_WRITE, MAP_SHARED, fd_, 0);

    if (mapping == MAP_FAILED) {
        throw std::runtime_error("mmap");
    }

    base_ = static_cast<struct perf_event_mmap_page*>(mapping);
    head_ = const_cast<volatile __u64*>(&base_->data_head);
    tail_ = const_cast<volatile __u64*>(&base_->data_tail);
    ring_base_ = static_cast<char*>(mapping) + base_->data_offset;
    ring_size_ = base_->data_size;
}

inline BoundedRecorder::~BoundedRecorder()
{
    close(fd_);
    // Note that the map must still be valid during `drain()`, so this is
    // almost the earliest we could do it.
    munmap(base_, mmap_size_);
}

inline void BoundedRecorder::enable()
{
    int error = ioctl(fd_, PERF_EVENT_IOC_ENABLE, 0);
    if (error == -1) {
        perror("ioctl enable");
        throw std::runtime_error("ioctl enable");
    }
}

inline void BoundedRecorder::disable()
{
    // FIXME - error handling
    ioctl(fd_, PERF_EVENT_IOC_DISABLE, 0);
}

inline void BoundedRecorder::drain(const char* f)
{
    std::ofstream file(f, std::ios_base::out | std::ios_base::binary);
    auto head = *head_, tail = *tail_;
    std::list<Chunk> clist { Chunk {ring_base_ + tail, head - tail} };
    internal::write_perfdata(file, pea_, clist);

    // FIXME - The above is buggy as soon as we start updating the tail,
    //         switch to something like the below then.
    //
    // bool empty = file.tellp() == 0;
    // auto head = *head_, tail = *tail_;
    // if (head < tail) {
    //     file.write(ring_base_ + tail, ring_size_ - tail);
    //     tail = 0;
    // }
    // file.write(ring_base_ + tail, head - tail);
    // *tail_ = head;
    // file.close();
}


// Returns true iff more data is available
inline size_t BoundedRecorder::extractChunk(void* target, size_t maxlen)
{
    size_t copied = 0;
    auto head = *head_, tail = *tail_;

    // We can't use the double ringbuffer trick since perf injects 1 page
    // of control data at the start of the mmap area :/
    // Therefore, use this special case instead.
    if (head < tail) {
        size_t size = ring_size_ - tail;
        if (size > maxlen) { // size can't be 0
            size = maxlen;
        }

        memcpy(target, ring_base_ + tail, size);
        copied += size;

        maxlen -= size;
        *tail_ = tail = 0;
    }

    size_t size = head - tail;
    if (size > maxlen) {
        size = maxlen;
    }

    memcpy(target, ring_base_ + tail, size);
    *tail_ = head;

    return copied + size;
}

// FIXME - document usage and limitations.
class ThreadAwareRecorder {
public:
    ThreadAwareRecorder(
        size_t period = 5000,
        CounterType = CounterType::Cycles);

    void enable();
    void disable();
    void drain(const char* filename);

private:
    // To be forwarded to thread-specific recorders.
    size_t period;
    CounterType type;

    struct MemoryArea {
        char* data; // Stored as `char*` for pointer arithmetic.
        size_t used;
        size_t reserved;
    };

    std::map<pid_t, MemoryArea> areas;
    std::map<pid_t, BoundedRecorder> recorders;
};


inline
ThreadAwareRecorder::ThreadAwareRecorder(size_t period, CounterType type)
  : period(period)
  , type(type)
{
}


inline void ThreadAwareRecorder::enable()
{
    pid_t tid = internal::sys_gettid();

    std::cout << "Enabling recorder for thread " << tid << std::endl;

    // FIXME - add synchronization
    bool exists = recorders.count(tid);

    if (!exists) {
        auto& area = areas[tid];
        // FIXME - dont hardcode size
        area.reserved = 8*1024*1024;
        area.used = 0;
        area.data = static_cast<char*>(malloc(area.reserved));
    }
    recorders[tid].enable();
}


inline void ThreadAwareRecorder::disable()
{
    // FIXME - add synchronization
    // FIXME - check if recorder exists
    pid_t tid = internal::sys_gettid();

    std::cout << "Disabling recorder for thread " << tid << std::endl;

    auto& recorder = recorders.at(tid);
    recorder.disable();
    auto& area = areas.at(tid);
    area.used += recorder.extractChunk(
        area.data + area.used, area.reserved - area.used);
}


inline void ThreadAwareRecorder::drain(const char* filename)
{
    if (recorders.empty()) {
        throw std::runtime_error("cannot drain w/o recording");
    }

    // Ideally the caller wouldn't leave any running,
    // but it can't always be avoided, and the call is
    // idempotent anyways.
    for (auto& kv : recorders) {
        kv.second.disable();
    }

    std::ofstream f(filename);

    // NOTE - We use the same pea for all events, which seems
    // safe right now but will probably break at some point.
    std::list<Chunk> chunks;
    for (auto kv : areas) {
        chunks.push_back(Chunk {kv.second.data, kv.second.used});
        std::cout << kv.second.data << " for " << kv.second.used << " bytes\n";
    }
    internal::write_perfdata(f, recorders.begin()->second.pea_, chunks);
}

} // namespace observatory
