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
#include "taeintproto.h"


    GLOBAL 	TEXT savekey[KEYSIZ+1];		/* last message key */

#define 	OUTMSGSIZ   STRINGSIZ		/* size of output message */ 


    FUNCTION  CODE  p_mput(

    TEXT	message[],			/* In: message to be logged */
    TEXT	key[]				/* In: message key */
    ) 
    {
    CODE 	code = 0;

    TEXT	msgbuf[OUTMSGSIZ+1];
    TEXT	msgkey[KEYSIZ+1];

    
    bytmov((GENPTR)message, (GENPTR)msgbuf, OUTMSGSIZ); /* copy locally	*/
    if (s_length(message) >= OUTMSGSIZ)			/* if too long-	*/
	msgbuf[OUTMSGSIZ] = EOS;			/* truncate	*/
    if (!key || s_equal (key, " "))			/* note: one blank */
	msgkey[0] = EOS;				/* as close as FOR */
    else						/* can get to null */
	s_bcopy (key, msgkey, KEYSIZ);
    msgkey[KEYSIZ] = EOS;

    s_copy(msgkey, savekey);				/* save the key     */
    p_msgout(msgbuf, msgkey);				/* write to output  */
    return(code);
    }

/*
 *	p_msgout. write message to standard output device, if not terminal.
 *
 */

    FUNCTION    VOID  p_msgout(
 
    TEXT	msgbuf[OUTMSGSIZ+1],
    TEXT	msgkey[KEYSIZ+1]
    )

    {

    m_msgout(msgbuf, msgkey);
    }

