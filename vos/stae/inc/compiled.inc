/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* Compiled PDF include file.  
 *
 *	CHANGE LOG:
 *
 *
 */

#ifndef I_COMPILED			/* prevent double include		*/
#define I_COMPILED	0

/*	Format of a compiled PDF file	*/

/*	Record 1: contains one of the following strings in text form:
 *
 *	process-compiled
 *	procedure-compiled
 *	parmset-compiled
 *	global-compiled
 */

/*	Record 2: contains the following structure:	*/

#define CP_TIMESIZ	17		/* length of date/time thru secs*/
#define	C_HEADER	0x40		/* record type for...		*/

    struct CP_HEADER {
    	CODE	type;			/* type of record		*/
	TEXT	version[CP_TIMESIZ+1];	/* version of TM - to mark file	*/
	TEXT	timedate[CP_TIMESIZ+1];	/* time/date of compilation	*/
	TEXT	execute[FSPECSIZ+1];	/* EXECUTE = value		*/
	BOOL	interrupt;		/* TRUE if OPTIONS=INTERRUPT	*/
	COUNT	body_line;		/* line number of BODY		*/
	struct POSCTX body;		/* position context for body	*/
	struct POSCTX title;		/* position of .title		*/
	struct POSCTX help;		/* position of .help		*/
	struct POSCTX level1;		/* position of .level1		*/
	struct POSCTX level2;		/* position of .level2		*/
	struct POSCTX before;		/* position of "before" record	*/
	struct POSCTX after;		/* position of "after" record	*/
	TEXT	spare[30];		/* for future			*/
	};

/*	Record 3 - n:  contains the following
 *
 *	The CP_SUBCMD record is repeated for each subcommand.
 *	The last CP_SUBCMD record has a null string for .subcmd.
 *	(If there are no subcommands, then the first CP_SUBCMD
 *	record has a null string for .subcmd).
 */


#define C_SUBCMD	0x41		/* record type for...		*/

    struct CP_SUBCMD {
        CODE	type;			/* record type			*/
    	TEXT	subcmd[SUBCMDSIZ+1];	/* subcommand name		*/
        BOOL	defalt;			/* TRUE if default 		*/
        struct POSCTX pos_parblk;	/* position of PARBLK		*/
	TEXT	spare[10];		/* for future			*/
        };

/* 	The next section of the compiled PDF file consists of PARBLK
 *	record groups.   A PARBLK record group is one or more
 *	records in PARBLK format where the last record in a group
 *	has parblk.last set TRUE.   The parblk.msgtyp field has
 *	the following values:
 */

#define	C_BEFORE	0x42		/* PARBLK of common variables	*/
					/* before all SUBCMDs		*/
#define C_AFTER		0x43		/* PARBLK of common variables	*/
					/* after all SUBCMDs		*/
#define C_SUBPAR	0x44		/* variables for a subcommand	*/
					/* (see local _SUBCMD)		*/

/*
 * 	A REFGBL VARIABLE in a PARBLK has the following format:	
 *
 *	v_name = name of the referenced global
 *	v_type = ignored
 *	v_class = V_GLOBAL
 *	v_count = 0
 *	v_cvp = NULL
 *
 *	A NAME type parameter in a PARBLK has the following format:
 *
 *	Standard fields, except that v_ref points to a VARIABLE
 *	within the current PARBLK.  If the referenced 
 *	PARBLK VARIABLE is of type V_GLOBAL, then only the name
 *	of the global is significant.
 */

/*	Following the PARBLK records, text records begin.   For 
 *	a procedure, the first text record is BODY, followed
 *	by a line-for-line copy of the procedure body, with
 *	leading whitespace suppressed and comments removed.
 *	Following the end-proc, the help text begins.
 *
 *	For PDFs other than procedures, the help text immediately
 *	follows the last PARBLK record.
 */	

/*
 *	Restrictions on PDFs submitted for compilation:
 *
 *	1. Substitution is not allowed in the pre-body section.
 *
 *	2.  A DEFGBL in a PDF is not compiled--error message generated
 *
 *	3.  For PDFs with subcommands, variables may not be declared 
 *	outside of SUBCMD brackets execept before all SUBCMDs and
 *	after all SUBCMDs.
 *
 *	4. NAME parameters ???
 *
 *	5. De-referencing to a global (de-referencing is resolved
 *	by the low-level, syntax routines.
 */

#endif
