/**
 **  ibisdefs.h
 **
 **   Private definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISDEFS
#define _H_IBISDEFS

/* Conversion from external IBIS/record ID to internal pointer.  On	*/
/* machines where sizeof(int)>=sizeof(pointer), just use the pointer.	*/
/* On other machines (64 bits, e.g. AXP-UNIX), we must use a table and	*/
/* return an offset into it, rather than the pointer itself.  This is	*/
/* to avoid changing the public interface to support 64 bit machines, 	*/
/* as "int" for the IBIS ID is everywhere.				*/
/* Note that the table case does not check for the validity of the id,	*/
/* but neither did the original code (used directly as a pointer).	*/

#if POINTERS_64_BITS_OS
#define IBIS_FETCH_ID(id) (_ibis_id_table[id])
#define IBIS_FETCH_REC_ID(id) (_ibis_rec_id_table[id])
#define MAX_NUM_IBIS_IDS 1000
#define MAX_NUM_IBIS_REC_IDS 10000
#else
#define IBIS_FETCH_ID(id) ((XIBIS *)id)
#define IBIS_FETCH_REC_ID(id) ((XREC *)id)
#endif

/* private label properties */
#define IFILE_SEGMENT   "segment" 
#define IFILE_BLOCKSIZE "blocksize" 
#define IFILE_COFFSET   "coffset" 
#define IFILE_ASCII_LEN "ascii_len" 
#define ILABEL_NL        "nlb" 
#define ILABEL_HOST      "bhost" 
#define ILABEL_INTFMT    "bintfmt" 
#define ILABEL_REALFMT   "brealfmt" 
#define ILABEL_BTYPE     "bltype" 
#define IBTYPE_IBIS      "IBIS" 

#define MAX_COL I2_MAX_COL     /* Current somewhat arbitrary Max # columns */
#define MAX_OLD_COL I2_MAX_COL_OLD  /* Limit for IBIS-1 files */
#define EXTRA_COL 64     /* Extra column space for shuffling columns */
#define OLD_BLKSIZE 512L
#define NEW_BLKSIZE 512L
#define MAX_GRP_NAME (I2_MAX_GRP_NAME-1)
#define MAX_TYPE_NAME  (I2_MAX_TYPE_NAME-1)
#define MAX_VALUE_NAME (I2_MAX_VALUE_NAME-1)
#define I_TEMP_BUFSIZE 50000

#define CHECK_UNIT( ibis ) \
	if (!_i_find_value( x_ibis, (list_value) (ibis))) \
		return( IBIS_FILE_IS_NOT_IBIS )
#define CHECK_WRITE( ibis ) \
	if ((ibis)->flags & FLAG_MODE_READ) \
		return( IBIS_FILE_OPENED_READONLY )
#define CHECK_COLUMN( ibis, column ) \
	if ((column) <= 0 || (column) > (ibis)->nc ) \
		return (IBIS_NO_SUCH_COLUMN);
#define IS_LOCKED( ibis, col ) \
	((ibis)->column[(col)-1]->locked)
#define CHECK_LOCK( ibis, col ) \
	if (IS_LOCKED(ibis, col)) \
		return (IBIS_COLUMN_LOCKED);

#define CHECK_WRITE_FOR( ibis ) \
	if ((ibis)->flags & FLAG_MODE_READ) \
		{ *status=IBIS_FILE_OPENED_READONLY; return; }
#define CHECK_COLUMN_FOR( ibis, column ) \
	if ((column) <= 0 || (column) > (ibis)->nc ) \
		{ *status=IBIS_NO_SUCH_COLUMN; return; }
#define CHECK_LOCK_FOR( ibis, col ) \
	if ((ibis)->column[(col)-1]->locked) \
		{ *status=IBIS_COLUMN_LOCKED; return; }

/* 
 * useful macros giving the smallest
 * multiple of <alignv> greater than or equal to <minval>
 */
 
#define IBYTE_ALIGN 8
#define HOW_MANY( minval, alignv)  (((minval)+(alignv)-1)/(alignv))
#define ALIGN_UP( minval, alignv)  (HOW_MANY(minval,alignv)*(alignv))

/*
 *  Translation will be necessary only if:
 *     1) The file/host translation format-names are not equal; 
 *     2) The format is ASCII and this is an IBIS-1 file; or
 *     3) The formats are equal & non-ASCII, but the file/host
 *        translation method is nontrivial. In the docs for
 *        zvtrans it is noted that this is indicated by a nonzero
 *        value in the first integer entry of the trans object.
 *
 *	NEED_TRANS() gives the definition:
 */
	
#define NEED_TRANS( ibis, col ) \
	((col)->trans != NULL || \
	   ((col)->format >= FMT_ASCII && (ibis->flags & FLAG_FILE_OLD) ) || \
	   ((col)->format < FMT_ASCII  && (col)->format > FMT_NONE && \
	   	((ibis)->trans[(col)->format].outtrans.opaque[0] != 0)) )

#define TRANS_USED( ibis, col) \
  (((col)->trans) ? (col)->trans : \
   (ibis)->trans + ((col)->format > FMT_ASCII? \
     FMT_ASCII : (col)->format))

/**
 ** Format Codes
 **/
 
typedef enum {
	FMT_BASE=0,
	FMT_DEFAULT=0,
	FMT_NONE,
	FMT_BYTE,
	FMT_HALF,
	FMT_FULL,
	FMT_REAL,
	FMT_DOUB,
	FMT_COMP,
	FMT_ASCII,
	FMT_LAST=FMT_ASCII+256  /* This is where the max-ASCII is set */
} fmt_type;


/**
 ** Info flags and masks for XIBIS structure
 **/
 
typedef enum {
	FLAG_MOD_RECORDS = 1L,
	FLAG_MOD_LABELS  = FLAG_MOD_RECORDS	<<1,
	FLAG_FILE_OPEN	 = FLAG_MOD_LABELS	<<1,
	FLAG_FILE_OLD    = FLAG_FILE_OPEN 	<<1,
	FLAG_FILE_IMAGE  = FLAG_FILE_OLD 	<<1,
	FLAG_AUTO_INIT   = FLAG_FILE_IMAGE 	<<1,
	FLAG_ORG_ROW     = FLAG_AUTO_INIT 	<<1,
	FLAG_ORG_COLUMN  = FLAG_ORG_ROW 	<<1,
	FLAG_MODE_READ   = FLAG_ORG_COLUMN 	<<1,
	FLAG_MODE_WRITE  = FLAG_MODE_READ 	<<1,
	FLAG_MODE_UPDATE = FLAG_MODE_WRITE 	<<1,
	FLAG_LAST	 	 = FLAG_MODE_UPDATE <<1
} flag_type;

typedef enum {
	FLAG_REC_REFRESH=1,
	FLAG_REC_GAPS=2
} recflag_type;

typedef enum {
	MASK_MODS = FLAG_MOD_RECORDS	| FLAG_MOD_LABELS,
	MASK_ORG  = FLAG_ORG_ROW	| FLAG_ORG_COLUMN,
	MASK_MODE = FLAG_MODE_READ	| FLAG_MODE_WRITE	| FLAG_MODE_UPDATE
} mask_type;


/* messages which may be sent to records */
typedef enum {
	NOTICE_FLUSHING_ROWS,
	NOTICE_NEED_ROWS,
	NOTICE_OPEN,
	NOTICE_CLOSING
} notice_type;

#endif /* _H_IBISDEFS */

