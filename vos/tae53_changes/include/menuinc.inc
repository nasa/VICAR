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



/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:45 DUA1:[TAEV2.INC]MENUINC.INC;3 */
/* TKW CHECKOUT FILE_TIME= 9-SEP-1986 14:14 DUA0:[TAE.V14.INC]MENUINC.INC;1 */
/*
 * Menu specific definitions & structures.
 *
 * CHANGE LOG:
 *	24-mar-83	New MCB format for fsblock...palm
 *
 *	13-nov-86	Moved some of the structures and defines from
 *			MENU.C to here, for the TAE_FACELIFT...krw
 *	06-aug-87	New fields in CURMEN for new menu editor...palm
 *			Made some CURMEN fields TINY to save space...palm
 */

#ifndef	I_MENUINC		/* prevent double include	*/
#define I_MENUINC	0

#include "fileinc.inp"

/*
 * Define the structure MCBLK (menu control block)
 */

    struct	 MCBLK
	{
	struct	MCBLK	*link;			/* link to previous block */
	struct  FSBLOCK	fsblock;		/* file spec block	  */
	struct	POSCTX	pcmen;			/* posctx of first menu rec */
	struct	POSCTX	pchlp;			/* posctx of first help rec */
	BOOL 		open;			/* mdf open flag */
	};

/*
 * define the local constants
 */

#define 	MAXMENUENT 	50   		/* max # of entries in a men */	

#define 	MN_PROC		1		/* entry type=proc 	     */
#define 	MN_MENU		2		/* entry type=menu 	     */
#define		MN_COMMAND	3		/* entry type=TCL command    */

#define		CMDTM		1		/* command processed by TM   */
#define		CMDMENU		2		/* command processed by MENU */

#define		M_ALLERR	11		/* menu stack allocation err */
#define		M_OPNERR	12		/* mdf open error 	     */
#define		M_NESTERR	13		/* exceeds max menu nesting  */

#define  NESTING(v)	(*v).v_count			/* # of components   */
#define  LAST_ENTRY(v)	SVAL(*v, (*v).v_count-1)	/* last component    */


/*
 * Define the structure CURMEN (current menu data block )
 * 
 */

    struct CURMEN
	{
	BOOL		open;			/* TRUE if mdf open	     */
	BOOL		finit;			/* TRUE if block in use	     */
	COUNT		nent;			/* no. of entries in mdf     */
	COUNT		index;			/* current editor index	     */
	TEXT		*entryline[MAXMENUENT];	/* first line of entry text  */
	UTINY		linenr[MAXMENUENT];	/* line number of entry	     */
	UTINY		type[MAXMENUENT];	/* entry type 		     */
	struct   POSCTX pos[MAXMENUENT];	/* position context of entry */
	struct	 TXTSTOR	ttlblk;		/* title strings 	     */
        struct	 TXTSTOR	nameblk;	/* proc/menu name strings    */
	};
/*
 * Define the structure MCMDTAB
 */

    struct MCMDTAB
	{
	TEXT		cmdnm[NAMESIZ+1];	/* command name */
	TINY		abbchar;		/* number of abbreviated char */
	CODE		cmdgrp;			/* command group (tm/menu) */
	TINY		numparm;		/* num of parameters */
	BOOL		cmdmode;		/* true if user mode = cmd  */
	TINY		restricted;		/* restricted when menu_closed */
	CODE		(*cproc)(TEXT*, struct CURMEN*);
		/* cmd processing routine */
	};

#endif
