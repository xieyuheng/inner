---
title: the linux programming interface
author: michael kerrisk
year: 2010
---

# background and concepts

## 1: history and standards

- unlike most commercial unix implementations,
  linux separates implementation from distribution.
  - x - one more level of market.

## 2: fundamental concepts

## 3: system programming concepts

- system calls and c library.

- whenever we make a system call or call a library function,
  we should always check the return status of the call
  in order to determine if it was successful.

# fundamental features of the system programming interface

## 4: file i/o: the universal i/o model

### intro

- all system calls for performing i/o
  refer to open files using a file descriptor,
  a (usually small) nonnegative integer.

  universal i/o model means
  to view everything as file
  - regular-file
  - terminal
  - socket
  - fifo
  - pipe
  - device

  each process has its own set of file descriptors.

### note everything as file

- x -
  'everything as file' is like
  to optimize the syntax[the language] of API of devices
  for file storage device.

### open

``` c
#include <sys/stat.h>
#include <fcntl.h>
int open(const char *pathname, int flags);
int open(const char *pathname, int flags, mode_t mode);
// Returns file descriptor on success, or –1 on error
```

### read

``` c
#include <unistd.h>
ssize_t read(int fd, void *buffer, size_t count);
// Returns number of bytes read, 0 on EOF, or –1 on error
```

### write

``` c
#include <unistd.h>
ssize_t write(int fd, void *buffer, size_t count);
// Returns number of bytes written, or –1 on error
```

### close

``` c
#include <unistd.h>
int close(int fd);
// Returns 0 on success, or –1 on error
```

### lseek

``` c
#include <unistd.h>
off_t lseek(int fd, off_t offset, int whence);
// Returns new file offset if successful, or –1 on error
```

### ioctl -- for operations outside the universal i/o model

``` c
#include <sys/ioctl.h>
int ioctl(int fd, int request, ... /* argp */);
// Value returned on success depends on request, or –1 on error
```

## 5: file i/o: further details

### atomicity and race conditions

- all system calls are executed atomically.
  all of the steps in a system call
  are completed as a single operation,
  without being interrupted by another process or thread.

- race conditions [race hazards]
  is a situation where
  the result produced by two processes (or threads)
  operating on shared resources
  depends, in an unexpected way,
  on the relative order
  in which the processes gain access to the CPU(s).

- O_CREAT and O_EXCL flags should be used for open(),
  if the function needs to know the file is created by itself.
  [exclusive-create]

- O_APPEND flag should be used for open(),
  to let the seek to the end of the file
  and the write operation happen atomically.

### fcntl -- control operations on an open file descriptor

``` c
#include <fcntl.h>
int fcntl(int fd, int cmd, ...);
// Return on success depends on cmd, or –1 on error
```

### file flag

``` c
int flags, accessMode;

flags = fcntl(fd, F_GETFL); /* Third argument is not required */
if (flags == -1)
  errExit("fcntl");

if (flags & O_SYNC)
  printf("writes are synchronized\n");

// need mask O_ACCMODE to check
// O_RDONLY (0)
// O_WRONLY (1)
// O_RDWR (2)
accessMode = flags & O_ACCMODE;
if (accessMode == O_WRONLY || accessMode == O_RDWR)
  printf("file is writable\n");

// use fcntl() F_SETFL command
// to modify O_APPEND O_NONBLOCK O_NOATIME O_ASYNC O_DIRECT

int flags;
flags = fcntl(fd, F_GETFL);
if (flags == -1)
  errExit("fcntl");
flags |= O_APPEND;
if (fcntl(fd, F_SETFL, flags) == -1)
  errExit("fcntl");
```

### file descriptors and open files

- file-descriptor to open-file correspondence is many-to-one
  - x -
    thus to view fd as file is not faithful to unix' design
- three data structures about file
  maintained by the kernel :
  1. process file descriptor table
     - close-on-exec file-flag
     - a reference to the open file description
       [link to system open file description table]
       reasons for many-to-one link :
       - dup()
       - dup2()
       - fcntl()
  2. system open file description table
     - file offset
     - all file-flags
     - the file access mode
       - read-only
       - write-only
       - read-write
     - settings relating to signal-driven i/o
     - a reference to the i-node object for this file
       [link to file system i-node table]
       reasons for many-to-one link :
       - fork()
       - local socket -- UNIX domain socket
  3. file system i-node table
     - file type (e.g., regular file, socket, or FIFO)
       and permissions
     - a pointer to a list of locks held on this file
     - various properties of the file,
       including its size and timestamps
       relating to different types of file operations
- implications of the data structures :
  - two different file descriptors that
    refer to the same open file description
    share a file offset value.
  - changes to the open file flags
    (e.g., O_APPEND, O_NONBLOCK, and O_ASYNC)
    by fcntl() F_GETFL and F_SETFL operations
    are shared by different file descriptors.
- examples :
  - open the same file twice :
    descriptor-0 -> description-0 -> i-node-0
    descriptor-1 -> description-1 -> i-node-0
    - different offset and file-flag
  - dup() and dup2() :
    descriptor-0 -> description-0 -> i-node-0
    descriptor-1 -> description-0 -> i-node-0
    - share offset and file-flag

### duplicating file descriptors

``` c
#include <unistd.h>
int dup(int oldfd);
// Returns (new) file descriptor on success, or –1 on error

#include <unistd.h>
int dup2(int oldfd, int newfd);
// Returns (new) file descriptor on success, or –1 on error
// if newfd is taken dup2() closes it first

newfd = fcntl(oldfd, F_DUPFD, startfd);
```

- x -
  a file descriptor is like a named variable
  which stores a file description.

  because some function uses specific file descriptors
  [specific named variables],

  we can change the function's behavior
  by setting these specific file descriptors
  [specific named variables],

  dup() and dup2() are just for this kind of variable settings.

- k -
  I do not think the little features
  like text input output redirection,
  are that worth, to be traded with
  the simplicity of core data structure of the kernel.

- x -
  let us ignore this in our file io API.

### file i/o at a specified offset: pread() and pwrite()

- 'p' can be viewed as 'positional'.
  thus
  positional-read and positional-write

``` c
#include <unistd.h>
ssize_t pread(int fd, void *buf, size_t count, off_t offset);
// Returns number of bytes read, 0 on EOF, or –1 on error
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset);
// Returns number of bytes written, or –1 on error
```

- to perform file I/O at specific offset,
  rather than at the current offset.
  and the file offset is unchanged by these calls.

- for both pread() and pwrite(),
  the file referred to by fd must be seekable.

- these system calls can be particularly useful
  in multithreaded applications.
  - system-calls are atomic,
    thus avoid race conditions.

### scatter-gather i/o: readv() and writev()

``` c
#include <sys/uio.h>
ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
// Returns number of bytes read, 0 on EOF, or –1 on error
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
// Returns number of bytes written, or –1 on error

struct iovec {
  void *iov_base; /* Start address of buffer */
  size_t iov_len; /* Number of bytes to transfer to/from buffer */
};
```

- instead of accepting a single buffer of data to be read or written,
  these functions transfer multiple buffers of data in a single system call.
  The set of buffers to be transferred is defined by the array iov.
  The integer count specifies the number of elements in iov.

### note about atomicity

- x -
  instead of provide those system-calls for atomicity reasons,
  the kernel should design a general mechanism
  to ensure atomicity of users' functions.

### truncating a file: truncate() and ftruncate()

- the truncate() and ftruncate() system calls
  set the size of a file to the value specified by length.

``` c
#include <unistd.h>
int truncate(const char *pathname, off_t length);
int ftruncate(int fd, off_t length); // does not change file offset
// Both return 0 on success, or –1 on error
```

- If the file is longer than length,
  the excess data is lost.
  If the file is currently shorter than length,
  it is extended by padding with a sequence of null bytes or a hole.

### nonblocking i/o

- the O_NONBLOCK flag serves two purposes:

  1. If the file can’t be opened immediately,
     then open() returns an error
     instead of blocking.

     One case where open() can block is with FIFOs

  2. After a successful open(),
     subsequent I/O operations are also nonblocking.

     If an I/O system call can’t complete immediately,
     then either a partial data transfer is performed
     or the system call fails with one of the errors
     EAGAIN or EWOULDBLOCK.
     Which error is returned depends on the system call.
     On Linux, as on many UNIX implementations,
     these two error constants are synonymous.

- O_NONBLOCK is generally ignored for regular files,
  because the kernel buffer cache ensures that
  I/O on regular files does not block,

  However, O_NONBLOCK does have an effect for regular files
  when mandatory file locking is employed.

### i/o on large files

- x -
  I ignore this for now.

### the /dev/fd directory

- this interface is to be used in shell.

### creating temporary files

- files which are removed when the program terminates.

- mkstemp()
  generates a unique filename based on a template
  opens the file and return a fd

  open with O_EXCL flag

  with read and write permissions for the file owner
  (and no permissions for other users)

  ``` c
  #include <stdlib.h>
  int mkstemp(char *template);
  // Returns file descriptor on success, or –1 on error
  ```

- tmpfile()

  The temporary file is automatically deleted when it is closed.
  To do this, tmpfile() makes an internal call to unlink()
  to remove the filename immediately after opening the file.

  ``` c
  #include <stdio.h>
  FILE *tmpfile(void);
  // Returns file pointer on success, or NULL on error
  ```

## 6: processes

- a process is an abstract entity,
  defined by the kernel,
  to which system resources are allocated
  in order to execute a program.

## 7: memory allocation
## 8: users and groups
## 9: process credentials
## 10: time
## 11: system limits and options
## 12: system and process information

# more advanced features of the system programming interface

## 13: file i/o buffering

### kernel buffering of file i/o: the buffer cache

- System calls for controlling kernel buffering of file I/O

- The fsync() system call causes the buffered data
  and all metadata associated with the open file descriptor fd
  to be flushed to disk.

  An fsync() call returns
  only after the transfer to the disk device
  (or at least its cache) has completed.

``` c
#include <unistd.h>
int fsync(int fd);
// Returns 0 on success, or –1 on error
```

``` c
#include <unistd.h>
int fdatasync(int fd);
// Returns 0 on success, or –1 on error
```

``` c
#include <unistd.h>
void sync(void);
```

- Specifying the O_SYNC flag when calling open()
  makes all subsequent output synchronous.

## 14: file systems

### device special files (devices)

- Character devices

  Terminals and keyboards

  handle data on a character-by-character basis.

- Block devices handle data a block at a time.

  disks and tape drives

  The size of a block depends on the type of device,
  but is typically some multiple of 512 bytes.

### I-nodes

- A file system’s i-node table contains one i-node for each file.
  I-nodes are identified numerically
  by their sequential location in the i-node table.

  information maintained in an i-node :

  - File type
    e.g., regular file, directory, symbolic link, character device.

  - Owner
    (also referred to as the user ID or UID) for the file.

  - Group
    (also referred to as the group ID or GID) for the file.

  - Access permissions for three categories of user:
    owner (sometimes referred to as user),
    group,
    and other (the rest of the world).

  - Three timestamps:
    time of last access to the file
    time of last modification of the file
    time of last status change

  - Number of hard links to the file.

  - Size of the file in bytes.

  - Number of blocks actually allocated to the file,
    measured in units of 512-byte blocks.
    There may not be a simple correspondence
    between this number and the size of the file in bytes,
    since a file can contain holes,
    and thus require fewer allocated blocks
    than would be expected according to its nominal size in bytes.

  - Pointers to the data blocks of the file.

## 15: file attributes

### retrieving file information: stat()

- retrieve information about a file,
  mostly drawn from the file i-node.

``` c
#include <sys/stat.h>
int stat(const char *pathname, struct stat *statbuf);
int lstat(const char *pathname, struct stat *statbuf);
int fstat(int fd, struct stat *statbuf);
// All return 0 on success, or –1 on error

struct stat {
  dev_t     st_dev;     // IDs of device on which file resides
  ino_t     st_ino;     // I-node number of file
  mode_t    st_mode;    // File type and permissions
  nlink_t   st_nlink;   // Number of (hard) links to file
  uid_t     st_uid;     // User ID of file owner
  gid_t     st_gid;     // Group ID of file owner
  dev_t     st_rdev;    // IDs for device special files
  off_t     st_size;    // Total file size (bytes)
  blksize_t st_blksize; // Optimal block size for I/O (bytes)
  blkcnt_t  st_blocks;  // Number of (512B) blocks allocated
  time_t    st_atime;   // Time of last file access
  time_t    st_mtime;   // Time of last file modification
  time_t    st_ctime;   // Time of last status change
};
```

- The stat() and lstat() system calls
  don’t require permissions on the file itself.
  However, execute (search) permission
  is required on all of the parent directories
  specified in pathname.

### checking file accessibility: access()

- The access() system call
  checks the accessibility of the file specified in pathname
  based on a process’s real user
  and group IDs (and supplementary group IDs).

``` c
#include <unistd.h>
int access(const char *pathname, int mode);
// Returns 0 if all permissions are granted, otherwise –1
```

- If pathname is a symbolic link, access() dereferences it.

- The time gap between a call to access()
  and a subsequent operation on a file
  means that there is no guarantee that
  the information returned by access() will still be true
  at the time of the later operation
  (no matter how brief the interval).

  This situation could lead to security holes in some application designs.

## 16: extended attributes
## 17: access control lists
## 18: directories and links
## 19: monitoring file events
## 20: signals: fundamental concepts

### process default actions on signals

- The signal is ignored;
  that is, it is discarded by the kernel
  and has no effect on the process.
  (The process never even knows that it occurred.)

- The process is terminated (killed).
  This is sometimes referred to as abnormal process termination,
  as opposed to the normal process termination that occurs
  when a process terminates using exit().

- A core dump file is generated,
  and the process is terminated.
  A core dump file contains an image of
  the virtual memory of the process,
  which can be loaded into a debugger
  in order to inspect the state of the process
  at the time that it terminated.

- The process is stopped
  execution of the process is suspended.

- Execution of the process is resumed
  after previously being stopped.

### signal types and default actions

- SIGABRT
  A process is sent this signal when it calls the abort() function
  By default, this signal terminates the process with a core dump.

- SIGBUS
  This signal ("bus error") is generated
  to indicate certain kinds of memory access errors.
  One such error can occur when using memory mappings
  created with mmap(),
  if we attempt to access an address
  that lies beyond the end of the underlying memory-mapped file.

- SIGFPE
  This signal is generated for certain types of arithmetic errors,
  such as divide-by-zero.
  The suffix FPE is an abbreviation for floating-point exception,
  although this signal can also be generated
  for integer arithmetic errors.

- SIGILL
  This signal is sent to a process if it tries to execute an illegal
  (i.e., incorrectly formed) machine-language instruction.

- SIGINT
  When the user types the terminal interrupt character
  (usually Control-C),
  the terminal driver sends this signal to the foreground process group.
  The default action for this signal is to terminate the process.

- SIGIO

- SIGIOT
  On Linux, this is a synonym for SIGABRT.
  On some other UNIX implementations,
  this signal indicates an implementation-defined hardware fault.

- SIGKILL
  This is the sure kill signal.
  It can’t be blocked, ignored, or caught by a handler,
  and thus always terminates a process.

- SIGPIPE
  This signal is generated when a process tries to write to a pipe,
  a FIFO, or a socket
  for which there is no corresponding reader process.
  This normally occurs because the reading process
  has closed its file descriptor for the IPC channel.

- SIGPROF
  The kernel generates this signal
  upon the expiration of a profiling timer
  set by a call to setitimer().
  A profiling timer is one that counts the CPU time used by a process.
  Unlike a virtual timer (see SIGVTALRM below),
  a profiling timer counts CPU time used
  in both user mode and kernel mode.

- SIGQUIT
  When the user types the quit character
  (usually Control-\) on the keyboard,
  this signal is sent to the foreground process group.
  By default, this signal terminates a process
  and causes it to produce a core dump.

- SIGSEGV
  This very popular signal is generated
  when a program makes an invalid memory reference.
  A memory reference may be invalid because
  1. the referenced page doesn’t exist
     e.g., it lies in an unmapped area
     somewhere between the heap and the stack,
  2. the process tried to update a location in read-only memory
     e.g., the program text segment
     or a region of mapped memory marked read-only,
  3. the process tried to access a part of kernel memory
     while running in user mode.

  In C, these events often result from
  1. dereferencing a pointer containing a bad address
     e.g., an uninitialized pointer
  2. passing an invalid argument in a function call.

  The name of this signal derives from
  the term segmentation violation.

- SIGSTOP
  This is the sure stop signal.
  It can’t be blocked, ignored, or caught by a handler;
  thus, it always stops a process.

- SIGSYS
  This signal is generated if a process makes a "bad" system call.
  This means that the process executed an instruction
  that was interpreted as a system call trap,
  but the associated system call number was not valid.

- SIGTERM
  This is the standard signal used for terminating a process
  and is the default signal sent by the kill and killall commands.

  A well-designed application will have a handler for SIGTERM
  that causes the application to exit gracefully,
  cleaning up temporary files and releasing other resources beforehand.

  Killing a process with SIGKILL bypasses the SIGTERM handler.

- SIGTRAP
  This signal is used to implement debugger breakpoints
  and system call tracing,
  as performed by strace(1).
  See the ptrace(2) manual page for further information.

- SIGTSTP
  This is the job-control stop signal,
  sent to stop the foreground process group
  when the user types the suspend character
  (usually Control-Z) on the keyboard.

  The name of this signal derives from "terminal stop."

- SIGTTIN
  When running under a job-control shell,
  the terminal driver sends this signal to a background process group
  when it attempts to read() from the terminal.
  This signal stops a process by default.

- SIGTTOU
  This signal serves an analogous purpose to SIGTTIN,
  but for terminal output by background jobs.
  This signal stops a process by default.

- SIGUNUSED
  As the name implies, this signal is unused.
  On Linux 2.4 and later,
  this signal name is synonymous with SIGSYS on many architectures.
  In other words, this signal number is no longer unused
  on those architectures,
  although the signal name remains for backward compatibility.

- SIGURG
  This signal is sent to a process
  to indicate the presence of out-of-band
  (also known as urgent) data on a socket.

- SIGUSR1 and SIGUSR1
  This signal and SIGUSR2 are available
  for programmer-defined purposes.
  The kernel never generates these signals for a process.
  Processes may use these signals to notify one another of events
  or to synchronize with each other.

- SIGVTALRM
  The kernel generates this signal
  upon expiration of a virtual timer set by a call to setitimer().
  A virtual timer is one that counts the user-mode CPU time
  used by a process.

- SIGWINCH
  In a windowing environment,
  this signal is sent to the foreground process group
  when the terminal window size changes.
  By installing a handler for this signal,
  programs such as vi and less can know
  to redraw their output after a change in window size.

- SIGXCPU
  This signal is sent to a process
  when it exceeds its CPU time resource limit (RLIMIT_CPU).

- SIGXFSZ
  This signal is sent to a process
  if it attempts (using write() or truncate())
  to increase the size of a file
  beyond the process’s file size resource limit (RLIMIT_FSIZE).

### >< changing signal dispositions: signal()

- signal()
  the original API
  simpler than sigaction().
  has variations in the behavior across UNIX implementations

- sigaction()
  more functionality

``` c
#include <signal.h>
void (*signal(int sig, void (*handler)(int))) (int);
// Returns previous signal disposition on success, or SIG_ERR on error

void
handler(int sig) {
  /* Code for the handler */
}

void (*oldHandler)(int);

oldHandler = signal(SIGINT, newHandler);

if (oldHandler == SIG_ERR)
  errExit("signal");

/* Do something else here. During this time, if SIGINT is
   delivered, newHandler will be used to handle the signal. */

if (signal(SIGINT, oldHandler) == SIG_ERR)
  errExit("signal");

// It is not possible to use signal()
// to retrieve the current disposition of a signal
// without at the same time changing that disposition.
// To do that, we must use sigaction().


// We can make the prototype for signal() much more comprehensible
// by using the following type definition for a pointer
// to a signal handler function:
typedef void (*sighandler_t)(int);

// This enables us to rewrite the prototype for signal() as follows:
sighandler_t signal(int sig, sighandler_t handler);
```

- Instead of specifying the address of a function
  as the handler argument of signal(),
  we can specify one of the following values :

  - SIG_DFL
    Reset the disposition of the signal to its default.
    This is useful for undoing the effect
    of an earlier call to signal()
    that changed the disposition for the signal.

  - SIG_IGN
    Ignore the signal.
    If the signal is generated for this process,
    the kernel silently discards it.
    The process never even knows that the signal occurred.

  return value of signal()
  also might be SIG_DFL or SIG_IGN.

### Sending Signals: kill()

- One process can send a signal to another process
  using the kill() system call,
  which is the analog of the kill shell command.

  The term 'kill' was chosen
  because the default action of most of the signals
  that were available on early UNIX implementations
  was to terminate the process.

``` c
#include <signal.h>
int kill(pid_t pid, int sig);
// Returns 0 on success, or –1 on error
```

### Checking for the Existence of a Process

- The kill() system call can serve another purpose.
  If the sig argument is specified as 0
  (the so-called null signal),
  then no signal is sent.
  Instead, kill() merely performs error checking
  to see if the process can be signaled.

- Various other techniques can also be used to check
  whether a particular process is running.

### Displaying Signal Descriptions

- Each signal has an associated printable description.
  These descriptions are listed in the array sys_siglist.
  For example, we can refer to sys_siglist[SIGPIPE]
  to get the description for SIGPIPE (broken pipe).

  However, rather than using the sys_siglist array directly,
  the strsignal() function is preferable.

``` c
#define _BSD_SOURCE
#include <signal.h>

extern const char *const sys_siglist[];

#define _GNU_SOURCE
#include <string.h>

char *strsignal(int sig);
// Returns pointer to signal description string
```

- The psignal() function displays (on standard error)
  the string given in its argument msg,
  followed by a colon,
  and then the signal description corresponding to sig.
  Like strsignal(), psignal() is locale-sensitive.

``` c
#include <signal.h>
void psignal(int sig, const char *msg);
```

### Signal Sets

### The Signal Mask (Blocking Signal Delivery)

### Pending Signals

### Signals Are Not Queued

### Changing Signal Dispositions: sigaction()

``` c
#include <signal.h>
int sigaction(int sig, const struct sigaction *act, struct sigaction *oldact);
// Returns 0 on success, or –1 on error
```

### Waiting for a Signal: pause()

## 21: signals: signal handlers

### Designing Signal Handlers

## 22: signals: advanced features
## 23: timers and sleeping

# processes, programs, and threads

## 24: process creation
## 25: process termination
## 26: monitoring child processes
## 27: program execution
## 28: process creation and program execution in more detail
## 29: threads: introduction
## 30: threads: thread synchronization
## 31: threads: thread safety and per-thread storage
## 32: threads: thread cancellation
## 33: threads: further details

# advanced process and program topics

## 34: process groups, sessions, and job control
## 35: process priorities and scheduling
## 36: process resources
## 37: daemons
## 38: writing secure privileged programs
## 39: capabilities
## 40: login accounting
## 41: fundamentals of shared libraries
## 42: advanced features of shared libraries

# interprocess communication (ipc)

## 43: interprocess communication overview
## 44: pipes and fifos
## 45: introduction to system v ipc
## 46: system v message queues
## 47: system v semaphores
## 48: system v shared memory
## 49: memory mappings
## 50: virtual memory operations
## 51: introduction to posix ipc
## 52: posix message queues
## 53: posix semaphores
## 54: posix shared memory
## 55: file locking

# sockets and network programming

## 56: sockets: introduction

### overview

- client and server

  - each application creates a socket.
    A socket is the thing that allows communication,
    and both client and server require one.

  - the server binds its socket to a well-known address (name)
    so that clients can locate it.

- A socket is created using the socket() system call,
  fd = socket(domain, type, protocol);

- in the internet domain :

  | internet domain stream socket   | SOCK_STREAM |
  | Transmission Control Protocol   | TCP         |
  |---------------------------------|-------------|
  | internet domain datagram socket | SOCK_DGRAM  |
  | User Datagram Protocol          | UDP         |

- socket system calls :
  #include <sys/socket.h>

### socket

- int socket(int domain, int type, int protocol);
  Returns file descriptor on success, or –1 on error.

  creates a new socket.

  domain := AF_UNIX | AF_INET | AF_INET6
  type := SOCK_STREAM | SOCK_DGRAM
  protocol := 0 [for now]

  for example,
  protocol = IPPROTO_RAW for raw sockets (SOCK_RAW)
  but protocol = 0 for now

### bind

- int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

  Returns 0 on success, or –1 on error

  binds a socket to an address.
  usually, a server employs this call
  to bind its socket to a well-known address
  so that clients can locate the socket.

- struct sockaddr
  ``` c
  struct sockaddr {
    // Address family (AF_* constant)
    sa_family_t sa_family;

    // Socket address
    // (size varies according to socket domain)
    char sa_data[14];
  };
  ```

- UNIX domain sockets use pathnames.
- Internet domain sockets use IP address + port number.

### stream sockets

#### phone analog of stream sockets

| socket(domain, type, protocol); | setup phone        |
| bind(sockfd, addr, addrlen);    | to have a number   |
| listen(sockfd, backlog);        | ready to be called |
|---------------------------------+--------------------|
| connect(sockfd, addr, addrlen); | dialing number     |
| accept(sockfd, addr, addrlen);  | pick up the phone  |

server : socket -- bind -- listen -- accept -- (send and recv) -- close
client : socket -- connect -- (send and recv) -- close

#### listen

- int listen(int sockfd, int backlog);

  Returns 0 on success, or –1 on error

  allows a stream socket to accept
  incoming connections from other sockets.

#### accept

- int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);

  Returns file descriptor on success, or –1 on error

  accepts a connection from a peer application
  on a listening stream socket,
  and optionally returns the address of the peer socket.

  If there are no pending connections when accept() is called,
  the call blocks until a connection request arrives.

  accept(sockfd, addr, addrlen) creates a new socket,
  and it is this new socket
  that is connected to the peer socket
  that performed the connect(sockfd, addr, addrlen).

  the listening socket remains open,
  and can be used to accept further connections.
  [phone analog breaks]

  accept(sockfd, addr, addrlen);
  set the addr to the addr of the peer socket.

#### connect()

- int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);

  Returns 0 on success, or –1 on error

  establishes a connection with another socket.

  If connect() fails and we wish to reattempt the connection,
  then SUSv3 specifies that
  the portable method of doing so
  is to close the socket,
  create a new socket,
  and reattempt the connection with the new socket.

### datagram sockets

#### postal analog of datagram sockets

server : socket -- bind -- (sendto and recvfrom) -- close
client : socket -- (sendto and recvfrom) -- close

#### recvfrom and sendto

- ssize_t recvfrom(
  int sockfd,
  void *buffer,
  size_t length,
  int flags,
  struct sockaddr *src_addr,
  socklen_t *addrlen);

  Returns number of bytes received, 0 on EOF, or –1 on error

- ssize_t sendto(
  int sockfd,
  const void *buffer,
  size_t length,
  int flags,
  const struct sockaddr *dest_addr,
  socklen_t addrlen );

  Returns number of bytes sent, or –1 on error

## 57: sockets: unix domain
## 58: sockets: fundamentals of tcp/ip networks
## 59: sockets: internet domains

### 59.15 further information

## 60: sockets: server design
## 61: sockets: advanced topics

# advanced i/o topics

## 62: terminals
## 63: alternative i/o models
## 64: pseudoterminals

# appendix

## a: tracing system calls
## b: parsing command-line options
## c: casting the null pointer
## d: kernel configuration
## e: further sources of information
## f: solutions to selected exercises
