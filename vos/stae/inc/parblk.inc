/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*	Parameter block include file.
 *
 * 	A parameter block is the contiguous block that contains parameters
 *	sent by TM to a subprocess.  The subprocess returns a parameter 
 *      block to TM for	dynamic parameters.
 *
 *	The parameter block structure consists of a header followed by
 *	a block of  storage which is allocated by the "restricted
 *	allocation" package.  This   "pool" contains copies of VARIABLE
 *	structures and values.  The links are set   to "relative" links
 *	by the restricted allocation package before transmission  to a
 *	subprocess or disk. 
 *
 *	When a PARBLK is written to a mailbox or disk, it is usually
 *	"truncated" to its "logical" length.  For example, if only
 *	the first n bytes of the pool are being used,  then the number
 *	of bytes transmitted is  "(parblk.pool - &parblk) + n".
 *
 *	CHANGE LOG
 *	27-NOV-82  new PARBLK strategy to keep messages small....palm
 *	19-JAN-83  new symbols: P_KILLED, etc....palm
 *	23-JAN-83  PARHDR structure and disk block format...palm
 *	27-JAN-83  P_NAMESIZ defined to be NAMESIZ...palm
 *	10-FEB-83  new 'mode', 'spare', 'ctxptr' ...palm
 *	20-MAR-83  move p_ and m_ codes to PGMINC.INC...dm
 *	25_MAR-83  used extra2 to hold host eror code...dm
 *	30-mar-83	Remove MODE ...palm
 *	29-Apr-83	Made hostcode explicit field.  See pr266...nhe
 *	31-oct-83	New P_BYTES value for PR 564...palm
 *	18-feb-84	Use COMPACT_COUNT so parblk format stays
 *			same from v1.1 to v1.2...palm
 *	05-dec-84 	Update comments on par file format...nhe
 *	27-mar-87  	New DEF_PARBLK macro, etc., so you can declare
 *		   	PARBLKs of any size.  Also, we define
 *			NP_PARBLK (no pool) and LARGE_PARBLK...palm
 *	29-mar-87  	New P_MODE_ABORT and P_MODE_RESTRICT...palm
 *	03-apr-87	Clean up P_MODE_*...palm
 *	06-dec-87	Add new filename field to PARHDR so that
 *			PAR files can be concatenated into one
 *			file and some name recognition is 
 *			possible...palm
 *	01-feb-89	Major re-work, invalidating existing PAR
 *			and RES files: new spare in PARBLK header,
 *			new sentinel string...palm
 *	02-aug-89	PS/2 port...ljn
 *	24-feb-92	Bumped LARGE_P_BYTES to MAX_COMPACT_UCOUNT to
 *			give room for complex variables as TAE Plus may have..krw
 *	24-feb-92	Had to change COMPACT_COUNT blksiz to COMPACT_UCOUNT.krw
 *			Same for PARHDR too.
 *      05-may-92       Increased LARGE_PARBLK size to 5*COMPACT_UCOUNT and
 *		        changed blksize and recsize to LONG...tpl
 */

#ifndef I_PARBLK		/* prevent double include		*/
#define I_PARBLK	0

#include	"symtab.inc"	/* TM symbol table			*/


/*	Macro to generate a PARBLK definition given pool size		*/

#define DEF_PARBLK(STRUCT, POOL_SIZE)    struct STRUCT\
	{\
	CODE		msgtyp;	  /* message type - MSGDEF include file */\
	BIT_FIELD_ALIGN\
	unsigned	last : 1; /* 1 = last block of symbol table	*/\
	BIT_FIELD_ALIGN\
	LONG		blksiz;	  /* block size in bytes		*/\
	COMPACT_UCOUNT	numvar;   /* not used 				*/\
	CODE		actmod;	  /* activation mode			*/\
	COMPACT_COUNT	vers;	  /* stuff version here			*/\
	struct SYMTAB	symtab;	  /* symbol table for all variables	*/\
	CODE		extra1;	  /* this & next for local use		*/\
	CODE		extra2;   \
	CODE		hostcode;  /* host code				*/\
	CODE		mode;	   /* used outboard for current mode	*/\
	GENPTR		ctxptr;	   /* used outboard for context ptr	*/\
	TEXT		spare[23]; /* reserved for future		*/\
    	ALIGN		pool[1+(POOL_SIZE-1)/sizeof(ALIGN)];\
	}

#define	P_BYTES		5300	/* size of storage pool  		*/
#define LARGE_P_BYTES	5*MAX_COMPACT_UCOUNT /* size of a large storage pool		*/

DEF_PARBLK(PARBLK, P_BYTES);		/* TAE-standard PARBLK 		*/
DEF_PARBLK(LARGE_PARBLK, LARGE_P_BYTES);/* very large PARBLK		*/
DEF_PARBLK(NP_PARBLK, 1);		/* "No Pool" PARBLK		*/

#define P_NAMESIZ	NAMESIZ		/* maximum number of chars in a parm	*/


/*   The following bit flags are used for the PARBLK.mode field   */

#define P_MODE_ABORT	 0x0001		/* abort on error 		*/
#define P_MODE_CONT	 0x0002		/* continue on error		*/
#define P_MODE_RESTRICT  0x0004		/* use r_alloc/r_dealloc	*/
#define P_MODE_TAE_ALLOC 0x0008		/* use tae_alloc/tae_free	*/


#include "pgminc.inc"			/* P_ and M_ codes		*/

/*
 *	Disk-resident parameter file format.  A parameter file is a sequential
 *	file created by f_open and written by f_bwrite.  There are n
 *	records: the first is a PARHDR structure (defined below) and the
 *	n-1 following records are truncated PARBLKs.
 *
 *	The PARHDR record has a sentinel string which is always set to
 *	P_SENTINEL.   This is an integrity check; for example, when tutor
 *	is asked to RESTORE a file, tutor can check that the file is
 *	really a parameter file.
 *
 *	The header record also contains the length of the second record.
 *	This allows a reading program to allocate memory for the read
 *	buffer.
 */

#define	OLD_P_SENTINEL	"<<parameter-file>>"	/* before major change        */
#define	P_SENTINEL	"<<new par-file>>"	/* after valid size change    */
#define	P_BIGSENTINEL	"<<big par-file>>"	/* allow size > COMPACT_UCOUNT*/
#define	P_SENTSIZ	18
#define P_TIMESIZ	17			/* xx-jan-yyyy hh.mm	*/

    struct PARHDR 
	{
	TEXT	sentinel[P_SENTSIZ+1];	/* sentinel string		*/
	LONG	recsize;		/* bytes in next record		*/
	TEXT	datetime[P_TIMESIZ+1];	/* date/time of file creation	*/
	TEXT	filename[FNAMESIZ+1];	/* original file name component	*/
	TEXT	spare[78];		/* reserved for future		*/
	};
#endif
