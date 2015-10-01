#ifndef DECLARES_H
#define DECLARES_H

#if UNIX_OS
#include <sys/types.h>		/* for off_t */
#endif

#include "defines.h"
#include "xviodefs.h"		/* Assume xvmaininc.h is already included */

typedef union {
  char *s;
  int i;
  int *ip;
} VALUE_TYPE;

struct UNIT_TABLE {
   int (*validation)(VALUE_TYPE);
				/* routine to call for validation	*/
   char init;			/* YES: init the unit_table to		*/
				/* default per entry to routine.	*/
   char type;			/* STRING,REAL,INTEGER			*/
   unsigned char size;		/* of element value in bytes		*/
   char nelements;		/* number of values to element		*/
   char mode;			/* INPUT or OUTPUT, that is, the element*/
				/* is input or output to the unit_table	*/
   unsigned char access;	/* indicates from which calls,open,read,*/
				/* write,add,get,the param is accessible*/
   char name[NAMESIZE+1];	/* the name of the element		*/
};

/* The value can be a string pointer, an integer, or an integer pointer.*/
/* 'value' is used when the type doesn't matter, so it must be the	*/
/* largest member of the union so everything will be copied. If the	*/
/* value is a string pointer, it points either to the default table, or	*/
/* to dynamically allocated memory.					*/

typedef union VALUE_TABLE {
   char *value;		/* generic value, must be biggest value in union */
   char *pvalue;
   int ivalue;
   int *ipvalue;
   void **ppvalue;
} VALUE_TABLE;

typedef struct VALUE_TABLE_INIT {   /* needed cuz unions can't be initialized */
   char *xxx;
} VALUE_TABLE_INIT;

/* This structure relates unit numbers to parameter names and	*/
/* instance numbers from the command line.			*/

struct UNITS {
   unsigned unit : 1;
   unsigned was_once_open : 1;
   char name[PARAM_NAME_LENGTH];
   int instance;
};

typedef VALUE_TABLE UNIT_TABLE_ENTRY[N_UNIT_TABLE_ENTRIES];
typedef VALUE_TABLE LABEL_TABLE_ENTRY[N_LABEL_TABLE_ENTRIES];

struct HISTORY {
   char name[8];
   char value[32];
};

struct v2_complex {
   float r;
   float i;
};

/* Data format translation information */

struct trans;
typedef int (*trans_fn)(void *from, void *to, int len, struct trans *trans);
struct trans {
   trans_fn transfn;		/* composite translation function */
   trans_fn transfn1;		/* step 1 of translation */
   trans_fn transfn2;		/* step 2 of translation */
   int spixsize;		/* source pixel size */
   int dpixsize;		/* destination pixel size */
   int midpixsize;		/* intermediate pixel size for 2-stage trans */
   int notransamt;  /* # of bytes at beg of rec to not trans (binary prefix) */
};

/* Buffer state information */

struct bufstate {
   char *buffer;	/* I/O buffer */
   V2_OFFSET bufsize;	/* size of buffer (blocksize*nblocksinbuf = bufsize) */
   int buf_extra;	/* extra size of buffer for wasted space on tapes */
   int recsize;		/* size of a record */
   int blocksize;	/* size of a logical block */
   V2_OFFSET blockno;	/* block # of start of buffer */
   int nblocksinbuf;	/* # of blocks in the buffer */
   int first_complete_rec; /* record # of first complete record in buffer */
   int last_complete_rec; /* record # of last complete record in buffer */
   int first_rec;	/* record # of first partial record in buffer */
   int first_rec_pos;	/* position in record of first byte of buffer */
   int last_rec;	/* record # of last partial record in buffer */
   int last_rec_pos;	/* position in buffer of start of last partial record */
   int last_rec_len;	/* # of bytes of last partial record in buffer */
   int rec_offset;	/* pos in buffer of first complete record */
			/* also the # of bytes in first partial record */
   V2_OFFSET eof_record; /* last record + 1 of the file (first unused rec) */
			/* -1 if unknown (like for tapes) */
			/* eof_record is V2_OFFSET cuz it's in bytes before */
			/* initialize_buffer is called */
   unsigned long flags;
   struct trans read_trans;
   struct trans write_trans;
   struct devstate devstate;	/* device state info */
};

struct PARM_LINE {		/* headers for PARMS files */
   int version;			/* Revision # of structure		*/
   int val_len;			/* Total length of value		*/
   short p_count;		/* TCL parm count			*/
   short p_size;		/* max size of each string (if string)	*/
   char p_type;			/* type: V_INTEGER,V_REAL,V_STRING,V_REAL8 */
   unsigned char name_len;	/* length of name (which follows header)*/
				/* including null terminator		*/
};

#if RTL_USE_COMPRESSION
struct basic_bufs { /* buffers used in BASIC compression and decompression */
   unsigned char *encoded_buf; /* buffer used to store encoded data */
   unsigned char *unencoded_buf; /* buffer used to store unencoded data */
   unsigned char *tmp_buf; /* buffer used to store transition data */
   int encoded_buf_size;
   int unencoded_buf_size;
   int tmp_buf_size;
   struct trans read_trans;
   struct trans write_trans;
};
#endif

#endif /* DECLARES_H */
