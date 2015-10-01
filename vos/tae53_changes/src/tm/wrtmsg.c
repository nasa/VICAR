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



/*TDM         CHECKOUT FILE_TIME=11-JUL-1983 20:44 DUA0:[TAEV1.TM]WRTMSG.C;2 */
/* WRTMSG  SOURCE FILE.
 *
 * Write messages to standard output device ( CRT or hardcopy terminal )
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	14-jul-83	Deleted include of PRIMINC...dm
 *	10-otc-83	Fixed unix lint errors...palm
 *
 */


#include	"stdh.inp"
#include	"taeconf.inp"
#include	"tmhost.inp"
#include	"terminc.inc"
#include	"tminc.inc"
#include "taeintproto.h"




/*
 * wrterr  - write an error message.
 */

FUNCTION  VOID  wrterr
(
    TEXT	msg[],			/* IN: error message to output */
    TEXT	key[],			/* IN: message key */
    uintptr_t	A1,		        /* IN: integers or string
					   pointers */
    uintptr_t   A2,
    uintptr_t   A3,
    uintptr_t   A4,
    uintptr_t   A5

 )
    {
    IMPORT  COUNT  termlines;		/* number of lines on crt screen */
    IMPORT  CODE   termtype;		/* terminal type */

    if (termtype == T_CRT)
	m_cput((termlines - ERRLIN), msg, key, A1, A2, A3, A4, A5);  /* write to crt */
    else
	m_put(msg, key, A1, A2, A3, A4, A5);		/* write to terminal */
    return;
    }

/*
 * wrtpmt - write a prompt message to terminal
 */

FUNCTION  VOID  wrtpmt
(
    TEXT	msg[]				/* IN: prompt message */
  
 )
    {
    IMPORT  COUNT  termlines;		/* number of lines on crt screen */
    IMPORT  CODE   termtype;		/* terminal type */

 
    if (termtype == T_CRT)			/* terminal is CRT */
	{
	t_lclear(termlines - PMTLIN, 1);			/* clear prompt line */
        t_output((termlines - PMTLIN), 1, msg);		/* write prompt  */
	}
    else
	t_write(msg, T_PROMPT);			/* write to terminal */ 
    return;
    }

/*
 * wrttxt - display text on terminal 
 */

FUNCTION  VOID  wrttxt
(
    FUNINT	line,			/* IN: line number to write to */
    FUNINT	col,			/* IN: column number to write to */
    TEXT	text[],			/* IN: message text */
    FUNINT	blank			/* IN: True if rest to be blanked */

 )
    {
    IMPORT  CODE   termtype;		/* terminal type */

    if (termtype == T_CRT)
	{
	if (blank) t_lclear(line, 1);		/* write a blank line */
        t_output(line, col, text);		/* output with no cleanup */
	}
    else 
	t_write(text, T_STDCC);			/* write with carriage return */
    return;
    }
