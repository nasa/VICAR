/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* LN CHECKOUT FILE_TIME= 9-SEP-1986 14:15 DUA1:[TAEV1.OLB]MPARM.C;1 */
/* TDM CHECKOUT FILE_TIME= 4-MAY-1984 11:01 DUA0:[TAEV1.OLB]MPARM.C;31 */
/* TDM CHECKOUT FILE_TIME=23-MAR-1984 10:12 DUA0:[TAEV1.OLB]MPARM.C;30 */
/* TDM CHECKOUT FILE_TIME=15-MAR-1984 10:44 DUA0:[TAEV1.OLB]MPARM.C;29 */
/* TDM CHECKOUT FILE_TIME= 5-MAR-1984 17:20 DUA0:[TAEV1.OLB]MPARM.C;28 */
/* TPEB CHECKOUT FILE_TIME=25-OCT-1983 18:00 DUA0:[TAEV1.OLB]MPARM.C;25 */
/*
 *	p_mput . Send a message to the terminal monitor for logging.
 *
 * 	NOTE: any time this module is updated, remember to check the
 *	      corresponding c-callable module mmsg.c for update.
 *
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	25-oct-83	Fixed pr 475 (outboard name conflicts) by
 *			renaming msgout...palm
 *	27-feb-84	Add XMPUT handshake...peb
 *	05-mar-84	Fix xzsavl common block for UNIX...dm
 *	11-mar-84	Do not call p_inim for receiving handsake msg...dm
 *	19-mar-84	Allow messages to be upto STRINGSIZ...dm
 *	12-apr-84	Move XZSAVL struct to a separate C module...dm
 *	21-aug-84	Fix loop on handshake error (PR #790)...dm
 *	22-aug-84	Change gstlun() name, update for UNIX...dm
 *	12-may-87	Port fix of dimension in q_init calling seq 
 *			(PR1184)...ljn
 *	03-apr-88	Apollo conversion: macro for WRTSTD() call...ljn
 *	14-feb-89	If key string null, omit brackets in msg...palm
 */

#include	"taeconf.inp"
#include	"parblk.inc"
#include	"tminc.inc"
#include	"terminc.inc"
#ifdef STRLEN
#undef STRLEN
#endif
#include	"forstr.inp"
#include "taeintproto.h"

extern  GLOBAL 	TEXT savekey[KEYSIZ+1];		/* last message key */

static    TEXT	stdbuf[OUTMSGSIZ+KEYSIZ+4];     /* standard output buffer */


FUNCTION    VOID  p_msgout
(
 TEXT	msgbuf[OUTMSGSIZ+1],
 TEXT	msgkey[KEYSIZ+1]
);



FUNCTION  CODE  p_mput
(
 TEXT	message[],			/* In: message to be logged */
 TEXT	key[]				/* In: message key */
)
    {
    CODE 	code;

/* the following avoids declaring a full PARBLK just to send a message	*/
#define ALDIM(bytes) 	1+((bytes-1)/sizeof (ALIGN)) 
#define HEAD_SIZ (sizeof(struct PARBLK) - P_BYTES + 8) /* 8 for align safety*/
#define MBLKDIM	(3*sizeof(struct VARIABLE) + 3*OUTMSGSIZ + HEAD_SIZ)
						/* block size in bytes	*/
    ALIGN	block[ALDIM(MBLKDIM)];		/* parameter block to send */
    struct	PARBLK  *parblk;		/* pointer to parameter block*/

    TEXT	*vector[1];			/* pointer to value */
    TEXT	msgbuf[OUTMSGSIZ+1];
    TEXT	msgkey[KEYSIZ+1];

    
    parblk = (struct PARBLK *) block;
    q_init(parblk, MBLKDIM - HEAD_SIZ, P_ABORT);	/* initialize block */
    bytmov((GENPTR)message, (GENPTR)msgbuf, OUTMSGSIZ); /* copy locally	*/
    if (s_length(message) >= OUTMSGSIZ)			/* if too long-	*/
	msgbuf[OUTMSGSIZ] = EOS;			/* truncate	*/
    vector[0] = (TEXT *) msgbuf;
    code = q_string(parblk, "MESSAGE", 1, vector, P_ADD); /* add message */
    if (code == SUCCESS)
	{
	if (!key || s_equal (key, " "))			/* note: one blank */
	    msgkey[0] = EOS;				/* as close as FOR */
	else						/* can get to null */
	    s_bcopy (key, msgkey, KEYSIZ);
        vector[0] = (TEXT *) msgkey;			/* pointer to key   */
	code = q_string(parblk, "KEY", 1, vector, P_ADD); /* add key 	    */
	}
    if (code == SUCCESS)
	{
	code = q_sndp(parblk, M_HLOGMSG);		/* send message     */
	if (code == SUCCESS)
	    {
	    code = c_rcvp((GENPTR)parblk, sizeof(block)); /* rcv handsh blk */
	    (*parblk).hostcode = code;			/* save host code   */
	    if (code != SUCCESS)
		{	
		t_write(
	"[TAE-MSGHANDS] Unable to confirm message reception by TM", T_STDCC);
		procexit(code);
	    	}
	    }
	}
    s_copy(msgkey, savekey);				/* save the key     */
    p_msgout(msgbuf, msgkey);				/* write to output  */
    return(code);
    }

/*
 *	p_msgout. write message to standard output device, if not terminal.
 *
 */

FUNCTION    VOID  p_msgout
(
 TEXT	msgbuf[OUTMSGSIZ+1],
 TEXT	msgkey[KEYSIZ+1]
)
    {
    int		lun;
    TAEINT	length;

    getlun_tae(&lun);		
    if (lun >=0)		/* if lun assigned, i.e. a file */
	{
	stdbuf[0] = EOS;    
	if (msgkey && msgkey[0] && !s_equal (msgkey, " "))
	    {
	    s_copy("[", stdbuf);		/* add the key		*/
	    s_append(msgkey, stdbuf);
	    s_append("] ", stdbuf);
	    }
	s_append(msgbuf, stdbuf);	/* add the message	*/
	length = s_length(stdbuf);	/* get length		*/
//	wrtstd_(stdbuf, &length);	/* pass c string to stdout */
	}
    return;
    }
