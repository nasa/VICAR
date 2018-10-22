/**
 **  ibisstructs.h
 **
 **   Structure definitions for IBIS subroutine library.
 **/
 
#ifndef _H_IBISSTRUCTS
#define _H_IBISSTRUCTS

#include "ibisdeclares.h"

struct XIBIS {

	/* basic properties */

	int unit;		/* VICAR unit number			*/
	int nc;			/* number of active columns		*/
	int nr;			/* number of active rows		*/
	int nl;		  	/* VICAR 'NL'				*/
	int ns;		  	/* VICAR 'NS'				*/
	fmt_type pix_fmt;	/* VICAR pixel data 'FORMAT'		*/
	int recsize;	  	/* VICAR 'RECSIZE'			*/
	int blocksize;  	/* Usable portion of each VICAR line	*/
	int numblocks;  	/* number of blocks (VICAR NL)		*/
	int segment;		/* columnsize = datasize*segment(org=col)*/
				/* rowsize = segment(org=row)		*/
	int extent;		/* max of offset+datasize over all cols */

	/* Auxiliary information */

	flag_type   flags;      /* flags indicating file condition	*/
	char        type[MAX_GRP_NAME+1]; /* IBIS subtype               */
	
	
	/* lists of "members" */
	
	XCOL **column;		/* Array of active columns (MAXCOL) 	*/
	List *record;		/* list of currently opened records	*/
	List *groups;		/* list of groups 			*/
	List *units;		/* list of units 			*/
	List *formats;		/* list of formats 			*/
	List *locals;		/* list of local groups 	*/
	List *gaps;		/* Available free space in file		*/

	/* Implementation-dependent methods */
	
	t_rowmethod	*rowmethod;
	t_colmethod	*colmethod;
	t_recmethod	*recmethod;
	t_filemethod	*filemethod;
	t_labmethod	*labmethod;
	
	/* format translators & related info */
	
	char            pix_host[MAX_GRP_NAME+1];/* VICAR Image Data host format        */
	char            hostfmt[MAX_GRP_NAME+1]; /* internal host format for conversion */
	char            intfmt[MAX_GRP_NAME+1];  /* internal host format for conversion */
	char            realfmt[MAX_GRP_NAME+1]; /* internal host format for conversion */
	char       	*fmt;		  /* holds fmt_len-char input format array 	*/
	int       	fmt_len;	  /* length of input format array elements 	*/
	fmt_type       	default_fmt;	  /* default value for fmt			*/
	trans_type      *trans;		  /* translators for file (BYTE...ASCII)	*/
	int		*format_size;	  /* internal host file data sizes 		*/
};

struct XCOL {
	int id;			/*  column number 			*/
	int offset;      	/*  offset in file to col		*/
	int locked;		/*  # of column translation locks 	*/
	fmt_type format; 	/*  column data format   		*/
	trans_type *trans;	/*  specified translation		*/
	XGROUP *unit;		/*  UNITS group (must be unique)	*/
};

struct XREC {
        XIBIS* ibis;		/* IBIS file owning record		*/
	int cur_row;		/* current active row			*/
	int top_row;		/* first row in buffer			*/
	int num_rows;		/* #rows in buffer			*/
	int mod_row;		/* first modified row (0=top of buffer)	*/
	int num_mods;		/* # modified rows	(0 if none)	*/
	int num_cols;		/* #cols				*/
	int recsize;		/* size of whole record	(in file)	*/
	int flags;		/* 1 =refresh,2=gaps			*/
	XCOL **column;		/* columns used in record		*/
	fmt_type format;	/* current translation type 		*/
	trans_type *trans;	/* translators for record 		*/
	char *inspace; 		/* memory allocated for inbuffer	*/
	char *outspace; 	/* memory allocated for outbuffer	*/
	char **inbuffer;	/* input data buffer			*/
	char **outbuffer;	/* output data buffer			*/
	List *collaborators;	/* other records sharing common columns */
	t_recmethod *method;	/* methods inherited from file		*/
};

struct t_recmethod {
	XMethod dofile; /* Performs read,write of buffer */
	XMethod notice; /* handle a message from somewhere */
};

struct t_rowmethod {
	XMethod dofile; /* Performs read,write,clear */
	XMethod new;
	XMethod delete;
	XMethod getsize; /* returns bytes-per-row of buffer passed to dofile */
};

struct t_colmethod {
	XMethod dofile; /* Performs read,write,clear */
	XMethod new;
	XMethod delete;
};

struct t_filemethod{
	XMethod open;
	XMethod init;
	XMethod clear;
	XMethod close;
};

struct t_labmethod{
	XMethod pre_open;	/* set up struct prior to open		*/
	XMethod post_open;	/* set up struct after open		*/
	XMethod flush;		/* sync struct values with file		*/
};

struct XGROUP {
	char name[MAX_GRP_NAME+1];
	List *columns; 		/* list of columns with group name 	*/
};

struct XGAP {
	int offset; 		/* offset to start (in COFFSET units)	*/
	int end; 		/* offset to end of gap			*/
};

typedef struct zv_trans {	/* External definition is "12 ints".	*/
	int opaque[12];		/* Internal definition is opaque to RTL */
} zv_trans;

struct trans_type {
	zv_trans intrans;  /* put these first so they're aligned well */
	zv_trans outtrans;
	int *format_size; /* file format sizes */
	int new;	/* new IBIS */
	fmt_type infmt;
	fmt_type outfmt;
};

#endif /* _H_IBISSTRUCTS */
