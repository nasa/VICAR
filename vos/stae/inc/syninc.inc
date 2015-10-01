/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* SYNTAX - Include file of syntax package constants.
 *
 *	CHANGE LOG:
 *	
 *	20-aug-83	New S_NULLVAL return code...palm
 *	29-aug-83	Delete S_NULLVAL; add S_KEYWORD...palm
 *	27-sep-83	Add S_DEREF...palm
 *	06-dec-84	Add S_QALPHA...jtm
 *	07-nov-85	PR 929: Move QUAL_SYM from SYNTAX.C to here...dab
 */

#ifndef I_SYNTAX		/* prevent double include		*/
#define I_SYNTAX	0

#define SYNINC_BASE	300	/* arbitrary constants start here	*/
				/* (must be greater than 256)		*/
#define	S_ALPHA		301	/* alphanumeric token			*/
#define	S_COMSEP	302	/* comma separator (possibly surrounded by white)*/
#define	S_DIGIT		303	/* digit character type			*/
#define	S_LETTER	304	/* letter character type		*/
#define	S_QUOTED	305	/* quoted string token			*/
#define	S_RPAREN	306	/* right parenthesis token		*/
#define	S_SYNERR	307	/* syntax error				*/
#define	S_WHITE		308	/* white space				*/
#define S_KEYWORD	309	/* getkey return code			*/
#define S_DEREF		310	/* getval return: de-reference found	*/
#define	S_NONE		310	/* no keyword in this parm position	*/
#define	S_START		311	/* start of values search		*/
#define S_QALPHA	313	/* alphanumeric name with an embedded "." */
#define QUAL_SYM	'|'	/* qualifier delimiter			*/

#define	EMSIZ	80			/* largest allowed error message	*/

    struct SYNBLK			/* syntax block				*/
	{
	TEXT	*curchr;		/* current character in cmd stream	*/
	TEXT	*inichr;		/* initialize character (start)		*/
	BOOL	inpar;			/* TRUE if within parentheses		*/
	COUNT	lstcg;			/* general last terminator character	*/
	COUNT	lstcp;			/* last terminator char for w/in parens	*/
	TEXT	*errchr;		/* char where syntax error detected	*/
	TEXT	errmsg[EMSIZ+1];	/* error message text			*/
	};

#endif
