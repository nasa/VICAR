/****************************************************************************
 *      Copyright (c) 1993, 1994
 *      Century Computing, Inc.
 *      ALL RIGHTS RESERVED
 *
 *      The software (programs, data bases and/or documentation) on or in
 *      any media can not be reproduced, disclosed, or used except under
 *      the terms of the TAE Plus Software License Agreement.
 *
 ***************************************************************************/



/* LN CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]ZEXIT.C;1 */
/* TDM CHECKOUT FILE_TIME= 4-MAY-1984 16:18 DUA0:[TAEV1.OLB]ZEXIT.C;13 */
/*
 * 	Z_EXIT. Functions to formally terminate the application process.
 * 
 *
 * 	CHANGE LOG:
 *	
 *	23-sep-83	PR 476: change exit call to _exit call...palm
 *	11-oct-83	Fix unix compilation errors...palm
 *	05-mar-84	Conditionalize EXIT
 *	21-aug-84	Make procexit a separate routine (to be called
 *			from p_mput() or m_msg() on handshake failure)...dm
 *	12-may-87	Port fix of exit codes for UNIX, dimension in q_init 
 *			calling seq (PR1184)...ljn
 *
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include "syninc.inc"
#include "taeintproto.h"

FUNCTION  VOID  procexit
(
    CODE	exit_code			/* normal or abnormal	*/

 );


/*
 *	z_exit. Terminate application process.
 */
 
FUNCTION  VOID  z_exit
(
    FUNINT		sfi,			/* in: value for $sfi	*/
    TEXT		*skey			/* in: value for $sky	*/

 )
    {
#define ALDIM(bytes) 	1+((bytes-1)/sizeof (ALIGN)) 
#define HEAD_SIZ (sizeof(struct PARBLK) - P_BYTES + 8)  /* 8 for align safety*/
#define ZBLKDIM		(3*sizeof(struct VARIABLE) + 2*STRINGSIZ + HEAD_SIZ)
    						/* block dimension	*/

    IMPORT TEXT		savekey[];		/* key for last message	*/
    TEXT		*keyptr[1];		/* pointer to key string */
    TAEINT		sfival[1];
    ALIGN		block[ALDIM(ZBLKDIM)];	/* parameter block to send */
    struct	PARBLK  *termblk;		/* pointer to parameter block*/
    CODE		code;

    keyptr[0] = skey;				/* assume key present	*/
    if (NULLSTR(skey))
	keyptr[0] = savekey;			/* else, give old key	*/
    sfival[0] = sfi;
    termblk = (struct PARBLK *)block;		/* cast pointer		*/
    q_init(termblk, ZBLKDIM - HEAD_SIZ, P_ABORT); /* initialize the block*/
    q_intg(termblk, "$SFI", 1, sfival, P_ADD);	/* put sfi in block	*/
    q_string(termblk, "$SKEY", 1, keyptr, P_ADD);  /* put skey in block */
    code = q_out(termblk);			/* send block to tm	*/
    procexit(code);
    return;
    }


/*
 *	procexit. Exit the process.
 *
 *	Note on exiting: in VMS, we call _exit to avoid conflicting with
 *	FORTRAN's EXIT routine.   In UNIX, there is no such confusion
 *	since FORTRAN's EXIT is reall _exit_; also, in UNIX, _exit
 *	does not flush terminal buffers.
 */

FUNCTION  VOID  procexit
(
    CODE	exit_code			/* normal or abnormal	*/

 )
    {
#ifdef UNIX
#define EXIT exit
#define NORMAL_EXIT 0
#define ABNORMAL_EXIT 1
#else
#define EXIT _exit
#define NORMAL_EXIT NORMAL
#define ABNORMAL_EXIT ABNORMAL
#endif

    if (exit_code == SUCCESS)
	EXIT (NORMAL_EXIT);			/* terminate normally	*/
    EXIT (ABNORMAL_EXIT);			/* terminate abnormally */
    }

