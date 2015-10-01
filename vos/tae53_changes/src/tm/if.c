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



/*	Conditional Processing Routines
 *	-------------------------------
 *
 *	Processes the following kinds of conditional proc statements:
 *			
 *			- IF
 *			- ELSE
 *			- END-IF
 *			- ELSE-IF
 *
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	8-aug-83	Add check for one-line if when in search mode...jtm
 *	27-sep-83	Fixed nesting error on one-line IF's, if found
 *			in a false part...jtm
 *	10-oct-83	Fixed unix lint errors...palm
 *	07-feb-84	Y_BATCH-->Y_ABI...nhe
 *	14-feb-84	Fix 68000 portability error on valptr...palm
 *	22-jul-87	Add get_condcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make condcmd table GLOBAL; see explanation in
 *			intrinsic.c...palm	
 *	28-jun-90	Removed get_condcmd()...ljn
 *	22-oct-92	Prototype of tae_alloc unecessary and Ultrix 4.3
 *			does not like it...rt
 */

#include	"stdh.inp"	/* standard C definitions		*/
#include	"taeconf.inp"	/* TAE configuration definitions	*/
#include 	"tmhost.inp"	/* host-dependent defs			*/
#include 	"symtab.inc"	/* symbol table				*/
#include	"tminc.inc"	/* TM-only host-independent definitions	*/
#include	"syninc.inc"	/* Syntax block definitions		*/
#include "taeintproto.h"



/* commands defined in this file:	
 *
 *	Note: the definition of the END-IF command is in another module
 *	because of the requirement that all subcommands be consecutively
 *	defined with their commmands.
 */

CODE if_do (struct CONTXT*, struct CONTXT*);
CODE else_do (struct CONTXT*, struct CONTXT*);
CODE elseif_do (struct CONTXT*, struct CONTXT*);
FUNCTION BOOL true_part 
(
    struct CONTXT	*pctx		/* in: proc context			*/

 );
FUNCTION CODE s_updt 
(
    struct	CONTXT	 *pctx,		/* IN/OUT: Proc context block */
    CODE		typval,		/* IN: Value to pushed onto type stack */
    CODE		lifval		/* IN: Value to be pushed onto local if
 *					       stack */

 );

#define Y_GENIF	Y_PROC|Y_BODY|Y_FALSE|Y_CMD|Y_ABI

    GLOBAL struct ITRCMD condcmd[] = 	/* miscellaneous commands	*/
	{
{0, "ELSE",	"",	Y_GENIF|Y_PROCSYN,	0,	NULL,	else_do },
{0, "ELSE",	"IF",	Y_GENIF,	      	0,	NULL,	elseif_do },
{0, "IF",	"",	Y_GENIF|Y_SEARCH,	0,	NULL,	if_do	},
{0, ""}   					/* TERMINATOR ENTRY: REQUIRED */
	};


/*	chk_cmd - Check IF statement to determine if command is on
 *		  the line (i.e. a single line IF). If a command is 
 *		  found on the IF after the logical expression, then an 
 *		  EOS is placed in the command stream immediately 
 *		  following the balancing right parentheses. The
 *		  character overwritten by the EOS is also returned.
 *
 *	RETURN CODE:
 *		
 *		SUCCESS
 *		FAIL - chk_cmd outputs the error message. Caller should
 *		       simply return to his caller with an error code
 *		       of DO_RETURN.
 *
 * CHANGE LOG:
 *
 *	25-mar-83	Initial release -- jtm
 */
FUNCTION	CODE chk_cmd 
(
    struct	CONTXT	*cmdctx,	/* IN/OUT: Pointer to cmd context blk */
    GENPTR		*cmdstr,	/* OUT: Ptr to new EOS in cmd line, if 
 *						any. Else NULL. */
    TEXT		firstc[]	/* OUT: Character overwritten by EOS */
 )
    {
/* LOCAL variables */

    FAST CODE	code;
    BOOL	intext;
    FAST COUNT	parenct;
    TEXT	*cmdptr, *startptr;
    TEXT	token [TOKESIZ+1];
    struct SYNBLK *sbptr;

/* FUNCTION DEFINITIONS */

    *cmdstr = NULL;
    sbptr = (*cmdctx).sb;
    startptr = (*sbptr).curchr;			/* Save sb pointer */
    if ((code = gettok(sbptr, token)) == S_WHITE)
	code = gettok (sbptr, token);
    if (code != '(')
	{
	tmmsg (PROCFAIL, "Missing parentheses on IF.",
	       "TAE-NOPARENS",0,0,0,0,0);
	return (DO_RETURN);
	}
    intext = FALSE;				/* Not in text string */
    parenct = 1;
    for (cmdptr = (*sbptr).curchr; 
	 parenct != 0; cmdptr++)		/* Look for balancing parens */
	{
	if (intext == FALSE)
	    {
	    if (*cmdptr == '(')			/* Nested one deeper */
		parenct++;
	    else if (*cmdptr == ')')		/* Nested one less */
		parenct--;
	    else if (*cmdptr == '\"')		/* Entering text string? */
		intext = TRUE;			/* Yes ignore until out. */
	    else if (*cmdptr == EOS)
		{
		tmmsg (PROCFAIL, "Unbalanced parentheses on IF.",
		       "TAE-UNBALPAR",0,0,0,0,0);
		return (FAIL);		
		}
	    }
	else
	    if (*cmdptr == '\"')		/* Coming out of text string */
		intext = FALSE;
	}

/* Now check for a one line command */

    (*sbptr).curchr = cmdptr;
    if ((code = gettok (sbptr, token)) == S_WHITE)
	code = gettok (sbptr, token);
    (*sbptr).curchr = startptr;			/* Return to original cmd pointer */
    if (code != EOS)
	{
	*cmdstr = cmdptr;		/* Save cmd start for caller */
	firstc[0] = *cmdptr;		/* Save first character */
	*cmdptr = EOS;			/* Truncate cmd buf after expression */
	return (SUCCESS);		/* Return pointer to command */
	}
    else
	return (SUCCESS);		/* No command on line */
    }


/*	if_do - Perform processing of an "IF" conditional procedure
 *	       	command line. This function evaluates the conditional
 *	       	to determine it's truth, and manipulates the local and
 *	       	global if stacks accordingly.
 *			
 *	Function return codes:
 *		
 *		DO_SUCCESS
 *		DO_RETURN
 *
 */

FUNCTION CODE if_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Stack to access */
    struct	CONTXT	*cmdctx	/* IF command line ctxt (w/synblk) */

 )
    {
/*	Local Variables */
    
    FAST	CODE	status, code;		/* Return code */
    FAST	CODE	locond;
    struct	SEARCH	 *srch;			/* Pointer to search blk */
    GENPTR		valptr;			/* Pointer to the result of 
 *						   the expression evaluator */
    COUNT		count;			
    CODE		term;			/* Command string terminator */
    GENPTR		cmdptr;			/* Ptr to single line cmd, 
 *						   if any */
    TEXT		firstc[1];		/* First char of cmd on IF */
    struct	SYNBLK	*sbptr;

    status = chk_cmd (cmdctx, &cmdptr, firstc);	/* Check for command on this line */
    if (status != SUCCESS) return (DO_RETURN);

    if ((*proctx).srchblk != NULL)		/* If in search mode */
	{
	if (cmdptr != NULL) return (DO_SUCCESS);	/* If it's a one-line 
 *							   if, no end-if expected */
	srch = (struct SEARCH *) (*proctx).srchblk;
	status = pushti ((*srch).srchstk, TYPE_IF);
	if (status != SUCCESS)
	    {
	    tmmsg (PROCFAIL, "More than %d IF/FOR/LOOP nested constructs.",
	    	   "TAE-MAXNEST",(uintptr_t)MAXNEST,0,0,0,0);
	    return (DO_RETURN);
	    }
	return (DO_SUCCESS);
	}
    if (true_part(proctx))
	{
	status = evalexp ((*cmdctx).sb, proctx, 
		 V_INTEGER, &valptr, &count, &term);
    
	if (status != SUCCESS)
	    return (DO_RETURN);			 /* Error was reported at
     *							   lower level	*/
	locond = *((TAEINT*) valptr) ? TRUE : FALSE;
	tae_free (valptr);				
	if (term != EOS)
	    {
	    tmmsg (PROCFAIL, "Unexpected characters at end of command line.",
		   "TAE-UNEXCH",0,0,0,0,0);
	    return (DO_RETURN);
	    }

	if (cmdptr != NULL)
	    {
	    sbptr = (*cmdctx).sb;
	    *cmdptr = firstc[0];		/* Restore first char */
	    s_copy (cmdptr, (*sbptr).inichr);	/* Move cmd in sb */
	    if (true_part(proctx) && locond)
		{
		initok (sbptr, (*sbptr).inichr);
		code = prccmd ((*sbptr).curchr, proctx);
		return (code);
		}
	    else
		return (DO_SUCCESS);
	    }
	}
    else
	{
	if (cmdptr != NULL) return (DO_SUCCESS);/* Ignore one-line IF */
	locond = FALSE;
	}
    status = s_updt (proctx, TYPE_IF, locond);	/* Update stacks accordingly */
    if (status == SUCCESS)
	return (DO_SUCCESS);
    else
	return (DO_RETURN);
    }

/*  else_do - 	Perform processing of an "ELSE" conditional procedure
 *	       	command line. This function simply inverts the value at
 *	       	the top of the local if stack and re-computes the value 
 *		for the top of the global stack.
 *			
 *	Function return codes:
 *		
 *		DO_SUCCESS
 *		DO_RETURN		- Local If or Global If stack is empty
 *
 */

FUNCTION CODE else_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Stack to access */
    struct	CONTXT	*cmdctx	/* IF command line context (w/ syntax block) */
 )
    {
/*	Local Variables */
    
    FAST	CODE	status;			/* Return code */
    FAST	CODE	locond;
    FAST	CODE	glcond;
    FAST	CODE	  type;

/*	Check top of type stack to make sure an "ELSE" is valid here */

    type = toptin ((*proctx).typstk);
    if (type == STACK_FAULT)
	{
	tmierr (1);			/* Error: Type stack empty in else_do */

	return (DO_RETURN);
	}
    else if (type != TYPE_IF && type != TYPE_ELSEIFND 
	     && type != TYPE_ELSEIFCHK)
	{
	tmmsg (PROCFAIL,"Unexpected ELSE command.", 
	       "TAE-XTRAELSE",0,0,0,0,0);
	return (DO_RETURN);
	}

    status = chkend ((*cmdctx).sb);	/* Make sure the rest of the line 
 *					   is blank */
    if (status != SUCCESS)
	{
	tmmsg (PROCFAIL, "Unexpected characters at end of command line.",
	       "TAE-UNEXCH",0,0,0,0,0);
	return (DO_RETURN);
	}
    
/*	ELSE is valid, Now get value from top of global IF stack and 
 *	throw away (since we'll recompute anyway) */

    glcond = poptin ((*proctx).glbifs);	
    if (glcond == STACK_FAULT)
	{
	tmierr (2);		/* Error: Global IF stack empty in else_do. */
	return (DO_RETURN);
	}

/*	Now get top of local stack */

    locond = poptin ((*proctx).locifs);	
    if (locond == STACK_FAULT)
	{
	tmierr (3);		/* Error: Local IF stack empty in else_do. */
	return (DO_RETURN);
	}

/*	Finally, begin case on construct type and situation */

    if (type == TYPE_IF)
	
/*
 *	*************************************************
 *	* This is the ELSE part of an IF/ELSE construct *
 *	*************************************************
 */
	{
	locond = (locond == TRUE)? FALSE : TRUE ;	/* Invert local IF value */
	status = s_updt (proctx, TYPE_ELSE, locond);
	}

    else if (type == TYPE_ELSEIFND)
/*
 *	**********************************************************
 *	* This is the ELSE part of an IF/ELSE-IF/ELSE construct, *
 *	* and the IF part or one of the ELSE-IF parts was TRUE,  *
 *	* so the following command lines should not be processed *
 *	**********************************************************
 */
	{
	type = poptin ((*proctx).typstk);	/* Get rid of ELSE-IF type */
	status = s_updt (proctx, TYPE_ELSE, FALSE);
	}

    else if (type == TYPE_ELSEIFCHK)
	
/*	**************************************************************
 *	* This is the ELSE part of an IF/ELSE-IF/ELSE construct, and *
 *	* none of the preceeding blocks were processed, so this one  *
 *	* should by definition.					     *
 *	**************************************************************
 */
	{
	type = poptin ((*proctx).typstk);	/* Get rid of ELSE-IF type */
	status = s_updt (proctx, TYPE_ELSE, TRUE);
	}
    return (DO_SUCCESS);
    }

/*	endif_do - Perform processing of an "ENDIF" conditional procedure
 *	       	command line. This function simply pops the conditions 
 *		currently at the tops of the local and global stacks.
 *			
 *	Function return codes:
 *		
 *		DO_SUCCESS
 *		DO_RETURN
 */

FUNCTION CODE endif_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Stack to access */
    struct	CONTXT	*cmdctx	/* IF command line context (w/ syntax block) */
 )
    {
/*	Local Variables */
    
    FAST	CODE	status;			/* Return code */
    FAST 	CODE	  type;			/* Type of conditional construct */
    struct	SEARCH	 *srch;			/* Pointer to search blk */

    if ((*proctx).srchblk != NULL)	/* If in search mode */
	{
	srch = (struct SEARCH *) (*proctx).srchblk;
	if (toptin ((*srch).srchstk) != TYPE_NONE)
	    {
	    status = poptin ((*srch).srchstk);
	    return (DO_SUCCESS);
	    }
	}

    type = toptin ((*proctx).typstk);	/* Find out if endif is valid */
    if (type == STACK_FAULT)
	{
	tmierr (4);			/* Error: Type stack empty in endif_do. */
	return (DO_RETURN);
	}

    else if ((type == TYPE_LOOP) 
	  || (type == TYPE_FOR)  
	  || (type == TYPE_NONE))
	{
	tmmsg (PROCFAIL,"END-IF found with no matching IF.",
	       "TAE-XTRENDIF",0,0,0,0,0);
	return (DO_RETURN);
	}
    status = chkend ((*cmdctx).sb);	/* Make sure the rest of the line 
 *					   is blank */
    if (status != SUCCESS)
	{
	tmmsg (PROCFAIL, "Unexpected characters at end of command line.",
	       "TAE-UNEXCH",0,0,0,0,0);
	return (DO_RETURN);
	}

    status = poptin ((*proctx).locifs);	/* Pop value from local stack */
    if (status == STACK_FAULT)
	{
	tmierr (5);			/* Error: Locifs empty in endif_do. */
	return (DO_RETURN);
	}
    status = poptin ((*proctx).glbifs);		/* Pop value from top of ...
    						   global stack */
    if (status == STACK_FAULT)
	{
	tmierr (6);			/* Error: Glbifs stack empty in endif_do. */
	return (DO_RETURN);
	}

    type = poptin ((*proctx).typstk);		/* Finally, pop value from type stack */
    if (type == TYPE_ELSE || 
	type == TYPE_ELSEIFCHK ||
	type == TYPE_ELSEIFND)
	type = poptin ((*proctx).typstk);	/* Pop again to get the IF */

    return (DO_SUCCESS);
    }

/*	elseif_do -	Perform processing of an "ELSE-IF" conditional 
 *			procedure command line.
 *
 *	This function will evaluate the conditional expression even if 
 *	the resulting value is determined to be false based on previous 
 *	command lines (e.g. another, preceding ELSE-IF found to be true),
 * 	and will manipulate the TYPE, GLOBAL IF, and LOCAL IF stacks 
 *	accordingly.
 *
 *	FUNCTION RETURN CODES:
 *
 *		DO_SUCCESS
 *		DO_RETURN	
 */

FUNCTION CODE elseif_do 
(
    struct	CONTXT	*proctx,		/* IN/OUT: Proc context block */
    struct	CONTXT	*cmdctx		/* IF cmd line context (w/syntax block) */
 )
    {

    FAST 	CODE	status;		/* Return codes from called functions */
    FAST	CODE	  type;		/* Type of construct currently within */
    FAST 	CODE	active;		/* Result of conditiona; expression
 *    					   evaluation	*/
    FAST 	CODE	locond = 0; /* Value obtained from local IF stack */
    FAST	CODE	glcond = 0; /* Value obtained from global IF stack */
    GENPTR		valptr;		/* Pointer to the result of 
 *					   the expression evaluator */
    COUNT		count;			
    CODE		term;		/* Command string terminator */

    
/*	Collect data about environment from stacks */

    type = toptin ((*proctx).typstk);	/* Currently active construct */

    if (type != TYPE_NONE)
	{
	locond = poptin ((*proctx).locifs);	/* Get top of local if stack */
	glcond = poptin ((*proctx).glbifs);	/* Get top of global if stack */
	if ((locond == STACK_FAULT) || (glcond == STACK_FAULT))
	    {
	    tmierr (7);			/* Error: IF stack underflow in elseif_do. */
	    return (DO_RETURN);
	    }
	}
    if (glcond == TRUE)
	active = FALSE;
    else
	{
	if (true_part(proctx))		/* If there's a chance this else-if
 *					   could make a true part */
	    {
	    status = evalexp ((*cmdctx).sb, proctx, 
	    	     V_INTEGER, &valptr, &count, 
		     &term);
    
	    if (status != SUCCESS)
		return (DO_RETURN);		 /* Error was reported at
	 *				 	    lower level	*/
	    active = *((TAEINT*) valptr) ? TRUE : FALSE;
	    tae_free (valptr);				
	    if (term != EOS)
		{
		tmmsg (PROCFAIL, "Unexpected characters at end of command line.",
		       "TAE-UNEXCH",0,0,0,0,0);
		return (DO_RETURN);
		}
	    }
	else
	    active = FALSE;
	}

/*	Now perform processing based on data collected above */

    if (type == TYPE_IF)

/*			*********************************
 *			* FOUND FIRST "ELSE-IF" IN AN 	*
 *			* "IF" CONSTRUCT		*
 *			*********************************
 */
	{
	if (locond == TRUE)

	    /* IF PART WAS TRUE */

	    status = s_updt (proctx, TYPE_ELSEIFND, FALSE);	/* Set up stacks so nothing is
 *						processed until next endif */
	
	else
	    {
	    if (active == TRUE)

	    /* IF PART WAS FALSE, BUT CONDTIONAL EXPRESSION IS TRUE */

		status = s_updt (proctx, TYPE_ELSEIFND, TRUE); /* Set up stacks to allow 
 *						   processing of succeeding 
 *						   lines (if global allows it */

	    else
		
		/* IF PART WAS FALSE, AND THIS ELSE-IF EXPRESSION IS FALSE */
		
		status = s_updt (proctx, TYPE_ELSEIFCHK, FALSE); /* Set up to disallow 
 *						   processing follwing lines,
 *						   but allow succeeding ELSE-IF's
 *						   to be fuond true */
	    }
	}
    else if (type == TYPE_ELSEIFCHK || type == TYPE_ELSEIFND)

	/*		****************************************
 *			* ONE OR MORE PRECEDING ELSE-IFs EXIST *
 *			****************************************
 */
	{
	type = poptin ((*proctx).typstk);	/* Pop old else-if type from stack */
	if (type == TYPE_ELSEIFND)
	    
	    /* ONE OF THE PRECEDING ELSE-IFs WAS TRUE */

	    status = s_updt (proctx, TYPE_ELSEIFND, FALSE);	/* Set up stacks so nothing is
 *						processed until next endif */
	else

	    /* NONE OF THE PRECEDING ELSE-IFs WERE TRUE */

	    {
	    if (active == TRUE)

		/* THIS CONDTIONAL EXPRESSION IS TRUE */

		status = s_updt (proctx, TYPE_ELSEIFND, TRUE); /* Set up stacks to allow 
 *						   processing of succeeding 
 *						   lines (if global allows it */

	    else
		
		/* THIS ELSE-IF EXPRESSION IS FALSE */
		
		status = s_updt (proctx, TYPE_ELSEIFCHK, FALSE); /* Set up to disallow 
 *						   processing follwing lines,
 *						   but allow succeeding ELSE-IF's
 *						   to be fuond true */

	    }
	}
    else

/*			**********************************
 *			* ERROR - ELSE-IF OUT OF CONTEXT *
 *			**********************************
 */
	{
	tmmsg (PROCFAIL, "ELSE-IF found with no matching IF.",
	       "TAE-XTRELSIF",0,0,0,0,0);
	return (DO_RETURN);
	}
    return (DO_SUCCESS);
    }

/*	s_updt   -	Update TYPE, GLOBAL IF, and LOCAL IF stacks
 *
 *	This function updates the three stacks used to control the processing
 *	of command lines within conditional constructs in a procedure.
 *	s_updt is passed the values to be placed on the TYPE and LOCAL IF stacks,
 *	and it computes the value to placed on the GLOBAL IF stack.
 *
 *	FUNCTION RETURN CODES:
 *
 *		SUCCESS
 *		FAIL		
 */

FUNCTION CODE s_updt 
(
    struct	CONTXT	 *pctx,		/* IN/OUT: Proc context block */
    CODE		typval,		/* IN: Value to pushed onto type stack */
    CODE		lifval		/* IN: Value to be pushed onto local if
 *					       stack */

 )
     {

    FAST 	CODE	status;		/* Return codes from called functions */
    FAST	CODE 	glcond;		/* Global condition from top of 
 *					   global if stack */
    FAST	CODE	global;		/* Computed global value */
    FAST	CODE	 trash;	

    status = pushti ((*pctx).typstk, typval);	/* Push the type on type stack */
    if ( status == STACK_FAULT)
	{
	tmierr (8);			/* Error: typstk overflow in s_updt. */
	return (FAIL);
	}

    status = pushti ((*pctx).locifs, lifval);	/* Push local condition value
    						   on the local stack */
    if (status == STACK_FAULT)
	{
	trash = poptin ((*pctx).typstk);	/* Restore type stack */
	tmmsg (PROCFAIL,"More than %d IF/FOR/LOOP constructs.",
	       "TAE-MAXNEST", (uintptr_t)MAXNEST,0,0,0,0);
	return (FAIL);
	}
    glcond = toptin ((*pctx).glbifs);		/* Get value from top of ...
    						   global stack */
    if (glcond == STACK_FAULT)
	{
	tmierr (9);			/* Error: glbifs underflow in s_updt. */
	return (FAIL);
	}
    global = lifval && glcond;			/* Compute the new value for ...
    						   the top of the global stack */
    status = pushti ((*pctx).glbifs, global);	/* Now push on global satck */
    if (status == STACK_FAULT)
	{
	tmierr (10);			/* Error: glbifs overflow in s_updt. */
	return (FAIL);
	}
    return (SUCCESS);
    }

/*	true_part. TRUE if the current proc statement is on the true
 *	side of an IF/THEN/ELSE-IF/ELSE.
 */

FUNCTION BOOL true_part 
(
    struct CONTXT	*pctx		/* in: proc context			*/

 )
    {
    CODE		code;

    if ((code = toptin((*pctx).glbifs)) == STACK_FAULT)
	{
	tmierr(101);			/* glblifs underflow in true_part */
	return(FALSE);
	}
    return(code);
    }
