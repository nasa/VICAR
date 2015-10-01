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


#include "taeintproto.h"

#ifndef TERMLIB
int dummy;		/* otherwise, `ar' complains about no symbols */
#else
/*	BSD termlib simulation.	
 * 
 *	This module contains entry points to simulate the
 *	termlib library calls of BSD UNIX.  
 *
 *	In BSD UNIX, the terminal characteristics are read
 *	from the /etc/termcap file during initialization.
 *	Here, we hard-code the capabilities in the
 *	INTCAP and STRCAP structures--see the example for
 *	vt100 below.
 *
 *	To edit this module for a new terminal type:
 *
 *	1. Define and initialize a INTCAP structure for the terminal.
 *	2. Define and initialize a STRCAP structure for the terminal.
 *	3. Remember that INTCAP and STRCAP structures must have
 *	   a null capability name as the last entry; see the vt100
 *	   example.
 *	3. Add an entry to the terms structure with the name of the
 *	   terminal and pointers to the INTCAP and STRCAP structures.
 *
 *	Restriction: as currently coded, the tputs function below
 *	does not support delays. 
 *
 *	After compiling this module, there are two ways to link with it:
 *	
 *	1. Make this a separate library and reference at load time.
 *
 *	2. Put this module in $TOLB/lib.a and remove all references
 *	   to termlib (e.g., -ltermlib) in the TAE make files.
 *
 *	CHANGE LOG:
 *	07-apr-88	PR1211: Increment line and column parms in tgoto...ljn
 *	10-nov-92	PR1732: Add definitions for xterm, iris-ansi and 
 *			iris-ansi-net for SGI.  See limitation below...cew
 *	08-jan-93	PR1771: Added definitions for aixterm and aixterm-m for
 *			IBM. We really need to change over to terminfo 
 *			compatibility for SYSV...krw
 *	09-mar-93	PR1857: Add definition for hpterm for hp...swd
 * 	13-may-94	PR-2728: Added definition for xcterm...dgf
 *	10-jun-94	PR2750: Revised tgetent to always return vt100 specs.
 *			this helps with TAE Plus porting...krw
 *
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/


/*	struct definitions	*/
#ifdef ORIGINAL
    struct TERMS
	{
	TEXT		*name;		/* terminal name	*/
	struct INTCAP	*intptr;	/* integer capabilities	*/
	struct STRCAP	*strptr;	/* string capabilities	*/
	};
#endif

    struct INTCAP 		/* integer capabilities			*/
	{
	TEXT	*name;		/* capability name	*/
	TEXT	value;		/* capability value	*/
	};

    struct STRCAP		/* string capabilities			*/
        {
	TEXT	*name;		/* capability name			*/
	TEXT	*value;		/* capability value			*/
	};

    static struct INTCAP *curint;	/* current INTCAP	*/
    static struct STRCAP *curstr;	/* current STRCAP	*/

/*	tables for specific terminals		*/

    static struct INTCAP vt100_intcap[] = {
    	{"co", 80},				/* columns		*/
	{"li", 24},				/* lines		*/
	{"", 0}  };				/* terminator		*/

    static struct STRCAP vt100_strcap[] = {
        {"cm", "\033[%d;%dH"},			/* cursor movement	*/
	{"ch", "\033[%dC"},			/* horiz cursor movement*/
	{"cl", "\033[;H\033[2J"},		/* screen clear		*/
	{"cr", "\015"},				/* carriage return	*/
	{"k1", "\033OP"},			/* GOLD key		*/
	{"k2", "\033OQ"},			/* F2			*/
	{"k3", "\033OR"},			/* F3			*/
	{"k4", "\033OS"},			/* F4			*/
	{"so", "\033[7m"},			/* highlight on		*/
	{"se", "\033[m"},			/* highlight off	*/
	{"ku", "\033OA"},			/* up arrow		*/
	{"kd", "\033OB"},			/* down arrow		*/
	{"kr", "\033OC"},			/* right arrow		*/
	{"kl", "\033OD"},			/* left arrow		*/
	{"ce", "\033[K"},			/* clear line		*/
	{"is", "\033[1;24r\033[24;1H"},		/* init			*/
	{"ks", "\033[?1h\033="},		/* keyboard init	*/
	{"", 0} };				/* terminator		*/

#ifdef ORIGINAL
/* PR 1732:  xterm, iris-ansi, iris-ansi-net are added so that you don't get
 * the unknown terminal type error.  A limitation of this package in general
 * is that the number columns and lines is hard-coded.
 * If the user uses taetm, they must reset their terminal (by issuing the
 * X command "resize" after exiting taetm.
 *
 * PR 1857:  hpterm added so that taewb and applications can be started in
 * the background (e.g. taewb &) on the HP.  Note that the vt100 codes are
 * used here for expediency, but do not work for terminal window control
 * in the hpterm.  Applications that need terminal control (e.g. taetm)
 * must be run in an xterm, not an hpterm.
 */
   static struct TERMS terms[] = {
	"vt100", vt100_intcap, vt100_strcap,	/* vt100		*/
	"xterm", vt100_intcap, vt100_strcap,	/* xterm		*/
	"iris-ansi", vt100_intcap, vt100_strcap, /* iris-ansi		*/
	"iris-ansi-net", vt100_intcap, vt100_strcap, /* iris-ansi-net	*/
	"aixterm", vt100_intcap, vt100_strcap, 	/* IBM aixterm		*/
        "xcterm",  vt100_intcap, vt100_strcap, /* xcterm (rtu) */
	"aixterm-m", vt100_intcap, vt100_strcap,/* IBM aixterm (monochrome)*/
        "hpterm", vt100_intcap, vt100_strcap,   /* hpterm               */
/* SEE NOTE BELOW in tgetent */
						/* put others here	*/
	""};					/* terminator entry	*/
#endif

/*	tgetent.	Set terminal type
 */

FUNCTION CODE tgetent 
(
    TEXT	bp[],		/* out: storage area (not used)	*/
    TEXT	name[]		/* in: name of terminal		*/

 )
    {
#ifdef ORIGINAL
    COUNT	i;

    for (i=0; terms[i].name[0] != 0; i++)	/* find terminal name	*/
        {
	if (s_equal (name, terms[i].name))
	    {
	    curstr = terms[i].strptr;		/* set current pointers	*/
	    curint = terms[i].intptr;
	    return (1);				/* return success	*/
	    }
	}
    return (-1);
#else
/* MAJOR KLUDGE.... to aid in TAE Plus porting, let's let ALL 
 * terminals map to a vt100. All known xterm based X Window terminals
 * work with the vt100 set.
 * We kept the original code here to support old TAE Classic code.
 */
    curstr = vt100_strcap;
    curint = vt100_intcap;
    return (1);
#endif
    }

/*	tgoto.	Move to cursor position
 *
 *	NOTE: UNIX assumes a [0,0] for the line/column origin.
 *	The ANSI standard, on the other hand, has [1,1] as the origin.
 */
FUNCTION TEXT *tgoto 
(
    TEXT	cm[],		/* in: "cm" string		*/
    FUNINT	destcol,	/* in: column			*/
    FUNINT	destline	/* in: line 			*/

 )
    {
    static  TEXT  goto_string[100];

    sprintf (goto_string, cm, destline+1, destcol+1);
    return (goto_string);
    }

/*	tgetstr.	Get capability string.
 */

FUNCTION TEXT *tgetstr 
(
    TEXT	id[],		/* in: name of capability	*/
    TEXT	**area		/* in/out: advancing buffer ptr	*/

 )
    {
    COUNT		i;
    TEXT		*a;
    struct STRCAP	*s;

    for (s=curstr;  (*s).name[0] != 0; s++)	/* find capability	*/
        {
	if (s_equal ((*s).name, id))
	    {
	    i = s_copy ((*s).value, *area);	/* copy to area		*/
	    a = *area;				/* save current		*/
	    *area += i + 1;			/* advance area		*/
	    return (a);				/* return cap pointer	*/
	    }
        }
    return (NULL);				/* no such capability	*/
   }

/*	tgetint.	Get integer capabilty.
 */

FUNCTION int tgetnum 
(
    TEXT	id[]		/* in: integer capability name	*/	

 )
    {
    struct INTCAP	*s;

    for (s=curint;  (*s).name[0] != 0; s++)	/* find capability	*/
        {
	if (s_equal ((*s).name, id))
           return ((*s).value);
        }
    return (-1);				/* no such capability	*/
    }

/*	tgetflag. 	Get capability existence.
 */

FUNCTION BOOL tgetflag 
(
    TEXT	id[]		/* in: capability name		*/

 )
    {
    int		code;

    code = tgetnum (id);
    return (code != -1);
    }

/*	tputs.		Send string with delay interpretation.
 *
 *	Delays are not simulated here.
 */

FUNCTION VOID tputs 
(
    FAST TEXT	*cp,		/* in: string to write	*/
    FUNINT	affcnt,		/* in: ignored		*/
    VOID	(*outc)(TEXT)	/* in: output routine	*/

 )
    {
    while (*cp != EOS)
        (*outc) (*cp++);
    return;
    }
#endif
