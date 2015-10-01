/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TDM CHECKOUT FILE_TIME=26-MAR-1984 14:31 DUA0:[TAEV1.OLB]MMSG.C;7 */
/*
 *	m_msg. Send a message (from a 'c' application program )to the 
 *		terminal monitor for logging.
 *
 *	NOTE: THIS MODULE MUST BE MAINTAINED PARALLEL TO MPARM.C
 *
 *	CHANGE LOG:
 *
 *	11-mar-84	Change handshake implementation.
 *	21-aug-84	Fix error loop on handshake failure (PR #790)...dm
 *	14-sep-86	Fix bug in procexit calling seq...dm
 *	29-jan-87	Fix dimension in q_init calling seq...peb
 *	14-may-89	if null key, do not display...palm 
 *
 */

#include	"taeconf.inp"
#include	<stdio.h>
#include "taeintproto.h"


extern GLOBAL 	TEXT savekey[KEYSIZ+1];		/* last message key 	*/

#define 	OUTMSGSIZ   STRINGSIZ		/* size of output message */ 
static    TEXT	stdbuf[OUTMSGSIZ+KEYSIZ+4];     /* standard output buffer */




    FUNCTION  CODE  m_msg(


    TEXT	message[],			/* In: message to be logged */
    TEXT	key[]				/* In: message key */
    )
 
    {
    TEXT	msgbuf[OUTMSGSIZ+1];
    TEXT	msgkey[KEYSIZ+1];

    bytmov((GENPTR)message, (GENPTR)msgbuf, OUTMSGSIZ);	/* copy locally	*/
    if (s_length(message) > OUTMSGSIZ)			/* if too long-	*/
	msgbuf[OUTMSGSIZ] = EOS;			/* truncate	*/
    bytmov((GENPTR)key, (GENPTR)msgkey, KEYSIZ);
    if (s_length(key) > KEYSIZ)
        msgkey[KEYSIZ] = EOS;

    s_copy(msgkey, savekey);				/* save the key     */
    m_msgout(msgbuf, msgkey);				/* write to output  */
    return(SUCCESS);
    }

/*
 *	m_msgout. write message to standard output device, if not terminal.
 *
 */

    FUNCTION    VOID  m_msgout(
 
    TEXT	msgbuf[OUTMSGSIZ+1],
    TEXT	msgkey[KEYSIZ+1]
    )

    {

	stdbuf[0] = EOS;    
	if (msgkey && msgkey[0] != 0)		/* only if key exists   */
	    {
	    s_copy("[", stdbuf);		/* add the key		*/
	    s_append(msgkey, stdbuf);
	    s_append("] ", stdbuf);
	    }
	s_append(msgbuf, stdbuf);	/* add the message	*/
	s_append("\n", stdbuf);		/* add new line character */
	printf("%s", stdbuf);   	/* write to stdout	*/    
    }
