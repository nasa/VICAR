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



/*
 * block to control output from help-related functions.
 * Various help structs and defines.
 *
 * CHANGE LOG:
 *
 *	19-oct-84	TCL 117: Add compiled PDF flag...peb
 *	22-apr-85	Add remote ctx pointer...dm
 *	29-apr-85	Fix misplaced #endif statement...dm
 *	00-sep-86	Added everything other than help block...ljn
 *	24-mar-88	Delete TAE_RCJM conditional...ljn
 *	02-aug-89	PS/2 port...ljn
 *	23-may-90	Remove RCJM stuff by referring to old TAE_RCJM...ljn
 *
 */


#ifndef  I_HELPINC			/* prevent double include */
#define  I_HELPINC	0

#include	"taeconf.inp"
#include 	"tminc.inc"

/*
 * definitions
 */

#define BORDER_CHAR 	'_'			/* character for border at bottom	*/
#define DIVIDE_LINE     '|'			/* character for division line */
#define MAX_HELP_PAGES 	99			/* max number of help pages		*/

/* constants defining help screen formatting		*/

#define H_HDR_SIZE	2		/* header line + blank line		*/
#define H_TRAIL_SIZE	4		/* number of lines in help trailer	*/

#define NULL_CMD	1000		/* corresponds to table index + 1000 */
#define	PAGE_CMD	1001
#define	EXIT_CMD	1002


/*
 * Information related to each displayed help page
 */

    struct PAGEBLK		
	{
	struct POSCTX 	mainpos;	/* pos ctx in main file     	*/
	struct POSCTX	incpos;		/* pos ctx in include file  	*/
	BOOL	inblock;		/* page start in if/else block? */
	};

    struct  HELPBLK
	{
	BIT_FIELD_ALIGN
	unsigned compiled : 1;		/* TRUE if help is on a compiled PDF */
	BIT_FIELD_ALIGN
	TEXT	nextcmd[NEXTSIZ+1];	/* buffer for next command from help */
	TEXT	errmsg[STRINGSIZ+1];	/* error message from help	     */
	TEXT	errkey[KEYSIZ+1];	/* error key from help		     */
	TEXT	helpobj[STRINGSIZ+1];	/* help object: input to help for    */
					/* error message formatting	     */
	};

#endif
