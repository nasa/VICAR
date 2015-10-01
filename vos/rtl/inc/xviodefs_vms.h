/* VMS-specific structure definitions for I/O */

#ifndef XVIODEFS_VMS_H
#define XVIODEFS_VMS_H

#if ALPHA_ARCH
#pragma member_alignment save
#pragma nomember_alignment
#endif

/* Alpha must have unaligned transfer_count below! */

struct diskIOSB
{
   unsigned short status;
   unsigned long transfer_count;
   short device_info;
};

#if ALPHA_ARCH
#pragma member_alignment restore
#endif

struct tapeIOSB
{
   unsigned short status;
   unsigned short transfer_count;
   long device_info;
};

/* struct diskstate, etc. contain things like channel number, current */
/* position, rab & fab if used, iosb, etc. */

struct diskstate {
   int blocksize;
   int io_event;
   int channel;
   struct diskIOSB iosb;
   int allocation;
   char *name;
};

struct tapestate {
   int blocksize;
   int io_event;
   int channel;
   struct tapeIOSB iosb;
   int tindex;
};

struct ansistate {
   int blocksize;
   int io_event;
   int channel;
   struct tapeIOSB iosb;
   long position;
   int phys_blocksize;
};

struct decnetstate {int x;};

struct memorystate {
   char *start;
   long size;
};

struct arraystate {
   struct diskstate disk; /* must be first as it uses many disk i/o routines */
   char *start;
   long size;
};

struct devstate {
   int device_type;
   int transfer_count;
   int async_pending;
   union {
      struct diskstate disk;
      struct tapestate tape;
      struct ansistate ansi;
      struct decnetstate decnet;
      struct memorystate memory;
      struct arraystate array;
   } dev;
};

#endif /* XVIODEFS_VMS_H */
