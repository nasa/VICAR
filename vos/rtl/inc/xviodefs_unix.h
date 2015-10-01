#ifndef XVIODEFS_UNIX_H
#define XVIODEFS_UNIX_H

#include "xvmaininc.h"
#include <sys/types.h>		/* for off_t */
#include <unistd.h>
#include <stdio.h>

/* Unix-specific structure definitions for I/O */

struct diskstate {
   int channel;
   int blocksize;
   V2_OFFSET pos;
   int transfer_count;
   V2_OFFSET allocation;
};

struct arraystate {
   struct diskstate disk;
   char *start;
   V2_OFFSET size;
};  

struct tapestate {
   int channel;
   int blocksize;
   V2_OFFSET pos;
   int transfer_count;
   int tindex;
};

struct devstate {
   int device_type;
   int transfer_count;
   V2_OFFSET file_offset;	/* # of bytes to skip at beginning of file */
   union {
      struct diskstate  disk;
      struct arraystate array;
      struct tapestate  tape;
   } dev;
};  

#endif /* XVIODEFS_UNIX_H */
