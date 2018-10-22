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

/*static char loops_cVersion[] = "@(#)loops.c	33.1 8/26/94";*/


/*	LOOP PROCESSING ROUTINES
 *	------------------------
 *
 *	Processes the following TAE Command Language Statements:
 *
 *			- FOR
 *			- END-FOR
 *			- GOTO
 *			- BREAK
 *			- NEXT
 *			- LOOP
 *			- END-LOOP
 *			- label. This routine checks to see if a
 *			  label matches the label in a goto statement.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	21-jul-83	Added check of variable name size...jtm
 *	4-aug-83	Fix bug related to goto from inside a for loop
 *			to somewhere outside the loop...jtm
 *	10-oct-83	Fix UNIX lint/compilation errors...palm
 *	26-oct-83	Change allowable # of values on FOR from 
 *			MAXFORVAL+1 to MAXFORVAL (pr 553) ...jtm
 *	07-feb-84	Y_BATCH-->Y_ABI...nhe	
 *	14-feb-84	Fix 68000 portability error on evalexp call...palm
 *	04-may-84	VALUE_x to xVAL ...ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	17-sep-84	Add process for error code V_BADAMBIG...lia
 *	12-dec-84	TCL 97: save line number of command for a GOTO, BREAK
 *			or NEXT in the SEARCH block...lia
 *	17-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ? ...peb
 *	19-dec-84	Delete search block for cmd NEXT if END-FOR found...lia
 *	28-dec-84	PR 905: moved ICBLOCK structure into TMINC.INC...lia
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	22-jul-87	Add get_loopcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	31-aug-87	Fix abort caused by extra end-loop or end-for 
 *			encountered in search mode...lia
 *	03-dec-87	If label named _DEFAULT encountered during GOTO
 *			search, then take it as match to anything...palm
 *	24-feb-88	Make GOTO parm have max length of LABELSIZ...palm
 *	28-jun-90	Removed get_loopcmd()...ln
 *	22-oct-92	Prototype of tae_alloc uncessary and Ultrix 4.3 does
 *			not like it...rt
 *	06-may-94	pr-2685: FAST changed to FAST int as needed...swd
 */

#include	"stdh.inp"		/* standard C definitions		*/
#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"
#include	"symtab.inc"
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"syninc.inc"
#include "taeintproto.h"



/* Memory-resident PDF data for proc invocation syntax 
 * commands performed in this file.
 *
 * The RESIDVAR structure is explained in the TMINC include file.
 */

    static struct RESIDVAR ptgoto[] =
	{
	/* name    type      k  m maxc    size     dc val      dvp*/

	  {"LABEL",   V_STRING, 0, 1, 1,    LABELSIZ,   0, NULL,    NULL}
	};

/* Table of intrinsic commands in this group:	*/

    CODE	for_do   (struct CONTXT*,struct CONTXT*);
    CODE	loop_do  (struct CONTXT*,struct CONTXT*);
    CODE	break_do (struct CONTXT*,struct CONTXT*);
    CODE	next_do  (struct CONTXT*,struct CONTXT*);
    CODE	goto_do  (struct CONTXT*,struct CONTXT*);

#define Y_FOR		Y_PROC | Y_BODY | Y_SEARCH | Y_CMD | Y_ABI
#define Y_GOTO		Y_PROC | Y_BODY | Y_PROCSYN | Y_ONFAIL | Y_CMD | Y_ABI

    GLOBAL struct ITRCMD loopcmd[] = 	/* loop commands		*/
	{
{0, "FOR",	"",		     Y_FOR,	0,	NULL,	for_do	},
{0, "LOOP",	"",	       Y_LOOPBOUND,	0,	NULL,	loop_do	},
{0, "NEXT",	"",		  Y_RETURN,	0,	NULL,	next_do	},
{0, "BREAK",	"",		  Y_RETURN,	0,	NULL,	break_do},
{0, "GOTO",	"", 		    Y_GOTO,	1,	ptgoto,	goto_do	},

{0, ""}    					/* TERMINATOR ENTRY*/
	};


FUNCTION 	VOID	delicb 
(
    struct	ICBLOCK		*icb		/*IN:OUT: Pointer to ICB */

 );
FUNCTION 	CODE	getfval 
(
    struct	VARIABLE  *symptr,	/*IN: Pointer to FOR variable */
    struct 	SYNBLK	      *sb,	/*IN/OUT: Syntax block containing amd stream */
    struct	CONTXT	    *pctx,	/*IN: Pointer to proc context */
    struct	ICBLOCK	     *icb	/*IN/OUT: Pointer to ICB */

 );


/*  bldicb -	Given a LOOP or FOR command, bldicb constructs an iteration 
 *		control block and places it on the iteration control list. 
 *		For a FOR command, bldicb uses the expression evaluator 
 *		function (evalexp) to obtain the binary values of the 
 *		list elements.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		pointer to created iteration control block
 *		NULL. Couldn't create full icb.
 */
FUNCTION 	struct ICBLOCK	*bldicb 
(
    CODE		    type,	/* IN: Type of icb to construct */
    struct	CONTXT	   *pctx,	/* IN/OUT: Current proc context block */
    struct 	SYNBLK	     *sb,	/* IN/OUT: Syntax block w/ cmd stream */
    struct 	VARIABLE *symptr	/* IN: Pointer to variable 
 *					   struct for variable in FOR statement */


 )
    {
/* 	LOCAL VARIABLES */

    IMPORT	struct SFILE prcfil;	/* File positioning context data */
    struct	ICBLOCK  *icbptr;	/* Pointer to newly created ICB */
    FAST int		    code;

/*	FUNCTION DEFINITIONS */


/*	***************************************************
 *	* ALLOCATE MEMORY FOR THE ITERATION CONTROL BLOCK *
 *	***************************************************
 */
    icbptr = (struct ICBLOCK *) tae_alloc (1, sizeof (struct ICBLOCK));
    if (icbptr == NULL)
	{
	overr();
	return (NULL);
	}    
/*
 *	*******************************************
 *	* INITIALIZE THE HEADER FIELDS OF THE ICB *
 *	*******************************************
 */
    (*icbptr).type = type;				/* Save loop type */
    f_movpos (&prcfil.posctx, &(*icbptr).posctx);	/* Save PDF position context for END */
    (*icbptr).pdf_line = (*pctx).pdf_line;		/* Save PDF line # of loop statement */
    if (type == TYPE_LOOP) return (icbptr);		/* If LOOP, then icb is now complete */

    initab (&(*icbptr).forlist);			/* Initialize the linked list of FOR values */
    (*icbptr).v = symptr;				/* Save pointer to variable */
    
/*	******************************************
 *	* GET THE VALUES FROM THE COMMAND STREAM *
 *	******************************************
 */
    code = getfval (symptr, sb, pctx, icbptr);
    if (code != SUCCESS) 
	{
	delicb (icbptr);				/* Get rid of dyn memory */
	return (NULL);
	}
    return (icbptr);
    }


/*  chk_label -	Perform processing of a label found on a TAE Command 
 *		line. This function checks to see if currently 
 *		searching for a label in the current proc. If so,
 *		and this label matches the label we're searching for,
 *		then the search is terminated. If not, this function 
 *		does nothing.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		NONE
 */
FUNCTION 	VOID	chk_label 
(
    struct 	CONTXT	*proctx	/* IN/OUT: Current context block */

 )
    {
    
/*	Local variables */

    struct  SEARCH	*srch;		/* Pointer to search block */
    BOOL		default_match; 

/*	FUNCTION DEFINTIIONS */

    if ((*proctx).srchblk != NULL)
	{
	srch = (struct SEARCH *) (*proctx).srchblk;
	if ((*srch).type == SRCH_GOTO)
	    {

	    /* NOTE: any normal GOTO search matches the _DEFAULT label */

	    default_match = !(*srch).onfailcmd && 
			 s_equal ((*proctx).label, "_DEFAULT");
	    if (default_match || s_equal ((*srch).label, (*proctx).label))
		{
		tae_free ((*proctx).srchblk);	/* Deallocate structure */
		(*proctx).srchblk = NULL;	/* Clear pointer to structure */
		}
	    }
	}
    return;
    }


/*  cleanlp -	Deallocates the dynamic memory associated with all iteration
 *		control blocks for a proc. This function is called upon 
 *		procedure termination. delicb marches through the icb linked 
 *		list, deletes dynamic memory associated with each VARIABLE 
 *		structure in the icb (if any), and then the icb itself. 
 *		
 *	FUNCTION RETURN CODES:
 *	
 *		NONE
 */
FUNCTION 	VOID	cleanlp 
(
    GENPTR	ptr			   /*IN:OUT: Pointer to ICB link list*/

 )
    {
/* 	LOCAL VARIABLES */

    struct ICBLOCK 	*nexticb;	
    struct ICBLOCK 	    *icb;	


    for (icb = (struct ICBLOCK *) ptr; icb != NULL; icb = nexticb) /* Delete all icbs */
	{
	while ((*icb).forlist.link != NULL)	   /* Delete variables in icb */
	    {
	    delvar (&(*icb).forlist, (*icb).forlist.link);
	    }
	nexticb = (*icb).i_link;	   /* Get the address of next icb */
	tae_free (icb);			   /* Now free the current one */
	}
    return;
    }


/*  delicb -	Deallocates the dynamic memory associated with an iteration
 *		control block. delicb deletes all dynamic memory associated
 *		with the VARIABLE structures used to store values for a FOR
 *		statement, if any.
 *		
 *	FUNCTION RETURN CODES:
 *	
 *		NONE
 */
FUNCTION 	VOID	delicb 
(
    struct	ICBLOCK		*icb		/*IN:OUT: Pointer to ICB */

 )
    {
/* 	LOCAL VARIABLES */

    while ((*icb).forlist.link != NULL)		/* Delete all variable structures */
	{
	delvar (&(*icb).forlist, (*icb).forlist.link);
	}
    tae_free (icb);
    return;
    }

/*  break_do -	Perform processing of a "BREAK" TAE Command language
 *		command. This function creates a search block
 *		block and points to it from the context block.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	break_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by break_do) */

 )
    {
    
/*	Local variables */

    struct  SEARCH	*srch;		/* Pointer to search block created by break_do */

/*	FUNCTION DEFINTIIONS */

    if ((toptin ((*proctx).typstk)) == TYPE_NONE)
	{
	tmmsg (PROCFAIL, "BREAK or NEXT outside a LOOP or FOR construct.",
	       "TAE-BADESC",0,0,0,0,0);
	return (DO_RETURN);
	}
    srch = (struct SEARCH *) tae_alloc (1, sizeof (struct SEARCH));
    if (srch == NULL)
	{
	overr();
	return (DO_RETURN);
	}
    (*srch).type = SRCH_BREAK;
    (*srch).cmd_line = (*proctx).pdf_line;
    (*srch).onfailcmd = (*proctx).onfailcmd;
    stk_in ((*srch).srchstk, MAXNEST, 
			TYPE_NONE);	/* Initialize type stack */
    (*proctx).srchblk = (GENPTR) srch;	/* Put search block pointer in ctx */    
    return (DO_SUCCESS);
    }

/*  endfor_do -	Perform processing of a "END-FOR" TAE Command language
 *		command. This function processes the icb at the top as
 *		follows:
 *		
 *			- if the value list in the icb has been 
 *			  exhausted, it pops the icb from the icb list.
 *		
 *			- else, it updates the variable specified in the
 *			  FOR statement with the next value in the list
 *			  and repositions the PDF file to the statement
 *			  at the top of the loop.
 *			  Finally, it deletes the value from the list.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	endfor_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block */

 )
    {
    
/*	Local variables */

    IMPORT	struct SFILE prcfil;	/* File positioning context data */
    FAST int   		   code;	/* Return code	*/
    struct  ICBLOCK	*icbptr;	/* Pointer to icb created by bldicb */
    struct  VARIABLE	*forval;	/* Pointer to variable structure containing
 *					   a FOR value */
    char   record [MAXSTRSIZ+1];	/* For throwing away an extra pdf line after 
 *					   repositioning */
    struct  SEARCH	  *srch;	/* Pointer to search block */

    icbptr = (struct ICBLOCK *) (*proctx).icbptr;    /* Get icb pointer */
    if ((*proctx).srchblk != NULL)		    /* If in search mode */
	{
	srch = (struct SEARCH *) (*proctx).srchblk;
	if (toptin((*srch).srchstk) == TYPE_NONE)
	    {
	    if ((*srch).type == SRCH_GOTO || 	    /* Finished with loop ... */
	        (*srch).type == SRCH_BREAK) 	    /* (unless NEXT is type)  */
		{
		if (icbptr == NULL)  	 /* ignore extra end-for in search mode */
		    return (DO_SUCCESS);
		while ((*icbptr).forlist.link!=NULL) /* Clean up for list */
		    {
		    forval = (struct VARIABLE *) (*icbptr).forlist.link;
		    delvar (&(*icbptr).forlist, forval);
		    }
		}
	    if ((*srch).type == SRCH_BREAK || (*srch).type == SRCH_NEXT)
		{
		tae_free ((*proctx).srchblk);  	    /* Free srch block storage */
		(*proctx).srchblk = NULL;	    /* No longer in search */
		}
	    }
	else
	    {
	    code = poptin ((*srch).srchstk);	/* Pop type stack w/o chking */
	    return (DO_SUCCESS);
	    }
	}

    if ((toptin ((*proctx).typstk)) != TYPE_FOR)
	{
	tmmsg (PROCFAIL, "END-FOR without matching FOR.", 
	       "TAE-XTRAENDFOR",0,0,0,0,0);
	return (DO_RETURN);
	}
    if (icbptr == NULL) 
	{
	tmierr (12);	/* Error: Iteration control block lost in endfor_do */
	return (DO_RETURN);
	}
    if ((*icbptr).type != TYPE_FOR)
	{
	tmierr (13);	/* Error: Wrong ICB type found in endfor_do */
	return (DO_RETURN);
	}
    if ((*icbptr).forlist.link == NULL)
	{
/*		**************************************************
 *		* LIST OF VALUES EXHAUSTED, REMOVE ICB FROM LINK *
 *		**************************************************
 */
	(*proctx).icbptr = (GENPTR) (*icbptr).i_link;	/* take out of list */
	delicb (icbptr);				/* Deallocate memory */
	code = poptin ((*proctx).typstk);		/* Pop FOR from stack */
	return (DO_SUCCESS);			
	}
    else
/*		**********************************************************
 *		* LIST NOT EXHAUSTED, UPDATE VARIABLE FOR NEXT ITERATION *
 *		**********************************************************
 */
	{
	forval = (struct VARIABLE *) (*icbptr).forlist.link;
	code = set_value ((*icbptr).v, 			/* Set variable to new value */
		(*forval).v_cvp, (*forval).v_count );
	if (code != SUCCESS)
	    {
	    overr();
	    return (DO_RETURN);
	    }
	delvar (&(*icbptr).forlist, forval);		/* delete value from list */
	f_setpos (&prcfil, &(*icbptr).posctx);		/* Reposition pdf file */
	code = f_read (&prcfil, record);		/* Skip one record */
        (*proctx).pdf_line = (*icbptr).pdf_line;	/* restore line #  */
	return (DO_SUCCESS);
	}
    }	

/*  endloop_do -	Perform processing of a "END-LOOP" TAE Command language
 *		command. This function reads the icb at the top of the 
 *		icb list and repositions the pdf file context to the 
 *		statement at the top of the loop.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	endloop_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by endloop_do) */

 )
    {
    
/*	Local variables */

    IMPORT	struct SFILE prcfil;	/* File positioning context data */
    FAST int		        code;	/* Return code	*/
    struct	ICBLOCK	     *icbptr;	/* Pointer to icb created by bldicb */
    char	record [MAXSTRSIZ+1];	/* For throwing away an extra pdf line after 
 *					   repositioning */
    struct 	SEARCH	       *srch;	/* Pointer to search block */

    icbptr = (struct ICBLOCK *) (*proctx).icbptr;	/* Get icb pointer */
    if ((*proctx).srchblk != NULL)		    /* If in search mode */
	{
	srch = (struct SEARCH *) (*proctx).srchblk;
	if (toptin((*srch).srchstk) == TYPE_NONE)
	    {
	    if ((*srch).type == SRCH_GOTO ||	    /* If goto or break, ... */
		(*srch).type == SRCH_BREAK)    	    /* terminate loop. */ 
		{
		if (icbptr != NULL) 
		    {
		    (*proctx).icbptr = (GENPTR) (*icbptr).i_link;     
		    delicb (icbptr);			 /* Deallocate memory */
		    code = poptin ((*proctx).typstk);	 /* Pop LOOP from stack */
		    }
		if ((*srch).type == SRCH_BREAK)
		    {
		    tae_free ((*proctx).srchblk);  	 /* Free srch block storage */
		    (*proctx).srchblk = NULL;		 /* No longer in search */
		    }
		return (DO_SUCCESS);			
		}
	    else				    /* Must be NEXT search */
		{
		tae_free ((*proctx).srchblk);  	    /* Free srch block storage */
		(*proctx).srchblk = NULL;	    /* No longer in search */
		}
	    }
	else
	    {
	    code = poptin ((*srch).srchstk);	/* Pop type stack w/o chking */
	    return (DO_SUCCESS);
	    }

	}
    if ((toptin ((*proctx).typstk)) != TYPE_LOOP)
	{
	tmmsg (PROCFAIL, "END-LOOP without matching LOOP.", 
	       "TAE-XTRAENDLP",0,0,0,0,0);
	return (DO_RETURN);
	}
    if (icbptr == NULL) 
	{
	tmierr (14);	/* Error: Iteration control block lost in endloop_do. */
	return (DO_RETURN);
	}
    if ((*icbptr).type != TYPE_LOOP)
	{
	tmierr (15);	/* Error: Wrong ICB type found in endloop_do. */
	return (DO_RETURN);
	}
    f_setpos (&prcfil, &(*icbptr).posctx);	/* Reposition pdf file */
    code = f_read (&prcfil, record);		/* Skip one record */
    (*proctx).pdf_line = (*icbptr).pdf_line;	/* restore line #  */
    return (DO_SUCCESS);
    }	



/*  for_do -	Perform processing of a "FOR" TAE Command language
 *		command. This function processes the tokens on the 
 *		command stream after the FOR command, which is stripped
 *		and processed by the caller. The symbol is extracted 
 *		from the command stream and the values are converted 
 *		into internal format and stored in a VARIABLE structure 
 *		linked list. The linked list of values is placed in an
 *		"Iteration Control Block (ICB)" along with other loop 
 *		control data. The ICB is then placed in a linked list
 *		pointed to by the proc context block.
 *
 *		The variable is then set to the value at the head of the 
 *		value list. 
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	for_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block */

 )
    {
    
/*	Local variables */

    FAST int		    code;	/* Return code	*/
    char	token[TOKESIZ+1];
    struct	VARIABLE *symptr;	/* Pointer to a variable structure
 *    					   in a symbol table. */
    struct	ICBLOCK  *icbptr;	/* Pointer to icb created by bldicb */
    struct	VARIABLE *forval;	/* Pointer to variable structure containing
 *					   a FOR value */
    struct	SEARCH	   *srch;	/* Pointer to search block */

/*	FUNCTION DEFINTIIONS */


    if ((*proctx).srchblk != NULL)		   /* In search, push type only */
	{
	srch = (struct SEARCH *) (*proctx).srchblk; /* Get ptr to search block */
	if ((pushti ((*srch).srchstk, TYPE_FOR)) !=
		SUCCESS)
	    {
	    tmmsg (PROCFAIL, "More than %d IF/FOR/LOOP constructs.",
	    	   "TAE-MAXNEST", (uintptr_t) MAXNEST,0,0,0,0);
	    return (DO_RETURN);
	    }	    
	return (DO_SUCCESS);
	}

    if ((pushti ((*proctx).typstk, TYPE_FOR)) != SUCCESS) /* Push type on type stack */
	{
	tmmsg (PROCFAIL, "More than %d IF/FOR/LOOP constructs.",
	       "TAE-MAXNEST", (uintptr_t)MAXNEST,0,0,0,0);
	return (DO_RETURN);
	}

    if ((code = gettok ((*cmdctx).sb, token)) == S_WHITE) 
    	code = gettok ((*cmdctx).sb, token);	/* Get the first non-white 
 *						   token on the line, which
 *						   should be a symbol. */

    if (s_length (token) > F_Q_NAMESIZ)
	{
	tmmsg (PROCFAIL, "Variable name too long on FOR. Maximum length allowed is %d.",
	       "TAE-FORVARBAD", (uintptr_t)F_Q_NAMESIZ,0,0,0,0);
	goto err_ret;
	}
    symptr = search (token, proctx);		/* Search symbol tables for
 *						   token */
    if (symptr == NULL)
	{
	tmmsg (PROCFAIL, "Reference to undefined variable '%s'.",
	       "TAE-UNDEFVAR", (uintptr_t)token,0,0,0,0);
	goto err_ret;
	}
    if ((code = gettok ((*cmdctx).sb, token)) == S_WHITE)
	code = gettok ((*cmdctx).sb, token);	/* Get next token, should be '=' */
    if (!(s_equal(token,"=")))
	{
	tmmsg (PROCFAIL, "Missing equal sign or incorrectly formatted FOR or LET.",
	       "TAE-MISEQ",0,0,0,0,0);
	goto err_ret;
	}
    icbptr = bldicb (TYPE_FOR, proctx, (*cmdctx).sb, symptr);
    if (icbptr == NULL) goto err_ret;

    forval = (*icbptr).forlist.link;			/* Get pointer to first value in list */
    code = set_value (symptr, (*forval).v_cvp, (*forval).v_count); /* Set variable to first value in list */
    if (code != SUCCESS)
	{
	overr();
	delicb (icbptr);
	goto err_ret;
	}
    delvar (&(*icbptr).forlist, forval); 		/* Delete value from list */

    (*icbptr).i_link = (struct ICBLOCK *) (*proctx).icbptr;		
    (*proctx).icbptr = (GENPTR) icbptr;
    return (DO_SUCCESS);
    
err_ret:
    code = poptin ((*proctx).typstk);		/* Pop the TYPE_FOR pushed */
    return (DO_RETURN); 
    }

/*  goto_do -	Perform processing of a "GOTO" TAE Command language
 *		command. This function creates a search block
 *		block and points to it from the context block.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	goto_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by goto_do) */

 )
    {
    
/*	Local variables */

    struct  SEARCH	*srch;		/* Pointer to search block created by break_do */
    struct  VARIABLE 	   *v;		/* Pointer to LABEL variable struct */

/*	FUNCTION DEFINTIIONS */


    srch = (struct SEARCH *) tae_alloc (1, sizeof (struct SEARCH));
    if (srch == NULL)
	{
	overr();
	return (DO_RETURN);
	}
    (*srch).type = SRCH_GOTO;
    (*srch).cmd_line = (*proctx).pdf_line;
    stk_in ((*srch).srchstk, MAXNEST, TYPE_NONE);	/* Initialize type stack */

    (*proctx).srchblk = (GENPTR) srch;		/* Put search blk ptr in ctx */
    v = lookex (&(*cmdctx).parmst, "LABEL");	/* Get label */
    s_copy (SVAL(*v,0), (*srch).label);		/* Copy label to srch struct */
    (*srch).onfailcmd = (*proctx).onfailcmd;	/* Keep onfail cmd flag */
    return (DO_SUCCESS);
    }



/*  loop_do -	Perform processing of a "LOOP" TAE Command language
 *		command. This function creates an iteration control
 *		block and adds it to the iteration control block list
 *		in the context block.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	loop_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by loop_do) */

 )
    {
    
/*	Local variables */

    FAST int		    code;	/* Return code	*/
    struct	ICBLOCK	 *icbptr;	/* Pointer to icb created by bldicb */
    struct	SEARCH     *srch;	/* Pointer to search block */

/*	FUNCTION DEFINTIIONS */


    if ((*proctx).srchblk != NULL)	/* In search mode, push type only */
	{
	srch = (struct SEARCH *) (*proctx).srchblk;
	if ((pushti ((*srch).srchstk, TYPE_LOOP)) !=
		SUCCESS)
	    {
	    tmmsg (PROCFAIL, "More than %d IF/FOR/LOOP constructs.",
	    	   "TAE-MAXNEST", (uintptr_t)MAXNEST,0,0,0,0);
	    return (DO_RETURN);
	    }	    
	return (DO_SUCCESS);
	}

    if ((pushti ((*proctx).typstk, TYPE_LOOP)) != SUCCESS) /* Push type on type stack */
	{
	tmmsg (PROCFAIL, "More than %d IF/FOR/LOOP constructs.",
	       "TAE-MAXNEST", (uintptr_t)MAXNEST,0,0,0,0);
	return (DO_RETURN);
	}
    icbptr = bldicb (TYPE_LOOP, proctx, (*cmdctx).sb, NULL);
    if (icbptr == NULL)
	{
	code = poptin ((*proctx).typstk);	/* Take type back off type stack */
	return (DO_RETURN);
	}
    (*icbptr).i_link = (struct ICBLOCK *) (*proctx).icbptr;	/* Put icb on list */
    (*proctx).icbptr = (GENPTR) icbptr;
    return (DO_SUCCESS);    
    }

/*  next_do  -	Perform processing of a "NEXT" TAE Command language
 *		command. This function creates a search block
 *		block and points to it from the context block.
 *
 *	FUNCTION RETURN CODES:
 *	
 *		DO_SUCCESS
 *		DO_RETURN
 */
FUNCTION 	CODE	next_do 
(
    struct 	CONTXT	*proctx,	/* IN/OUT: Current context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: New context block (not used by next_do) */

 )
    {
    
/*	Local variables */

    struct  SEARCH	*srch;		/* Pointer to search block created by break_do */

/*	FUNCTION DEFINTIIONS */

    if (toptin ((*proctx).typstk) == TYPE_NONE)
	{
	tmmsg (PROCFAIL, "BREAK or NEXT outside a LOOP or FOR construct.",
	       "TAE-BADESC",0,0,0,0,0);
	return (DO_RETURN);
	}
    srch = (struct SEARCH *) tae_alloc (1, sizeof (struct SEARCH));
    if (srch == NULL)
	{
	overr();
	return (DO_RETURN);
	}
    (*srch).type = SRCH_NEXT;
    (*srch).cmd_line = (*proctx).pdf_line;
    (*srch).onfailcmd = (*proctx).onfailcmd;		/* Keep onfail cmd flag */
    stk_in ((*srch).srchstk, MAXNEST, TYPE_NONE);	/* Initialize type stack */

    (*proctx).srchblk = (GENPTR) srch;	/* Put search block pointer in ctx */    
    return (DO_SUCCESS);
    }

/*  getfval -	Given the FOR command line after the '=', and the VARIABLE
 *		structure of the symbol, parse the remaining portion of the
 *		command stream, constructing a validated list of FOR values.
 *		This function uses the expression evaluator function to 
 *		obtain binary values of FOR list elements.
 *		
 *	FUNCTION RETURN CODES:
 *	
 *		SUCCESS
 *		FAIL
 */

FUNCTION 	CODE	getfval 
(
    struct	VARIABLE  *symptr,	/*IN: Pointer to FOR variable */
    struct 	SYNBLK	      *sb,	/*IN/OUT: Syntax block containing amd stream */
    struct	CONTXT	    *pctx,	/*IN: Pointer to proc context */
    struct	ICBLOCK	     *icb	/*IN/OUT: Pointer to ICB */

 )
    {
/*	LOCAL VARIABLES */

    FAST int		   count;
    COUNT		   vcount;
    struct VARIABLE	 *forval;
    CODE		val_type;
    CODE	 	    term;
    FAST int	 	    code;
    GENPTR		valptr;

/*	FUNCTION DEFINITIONS */


/* 	Stay in the following loop until:
 *
 *		- an EOS is encountered, indicating the end of the command stream
 *		- a syntax error is encountered by evalexp (code != SUCCESS)
 *		- more than MAXFORVAL values are found.
 */
    for (count = 0; count < MAXFORVAL; count++)
	{
	forval = allvar (&(*icb).forlist);		/* Allocate a variable for a value */
	if (forval == NULL)
	    {
	    overr();
	    return (FAIL);
	    }

        val_type = (*symptr).v_type;	

	code = evalexp (sb, pctx, val_type, 
	       &valptr, &vcount, &term); 	/* Get a FOR value */
	if (code != SUCCESS) return (FAIL);
        (*forval).v_count = vcount;
	(*forval).v_cvp = valptr;
	(*forval).v_name[0] = EOS; 		/* Name field never used */
	(*forval).v_type = val_type;		/* Same type as FOR variable */
	(*forval).v_class = V_LOCAL;		/* Make it local symbol, really doesn't matter */
						/* Check new value for validity */
	code = chk_vector(symptr, (*forval).v_type, (*forval).v_cvp,
			  (*forval).v_count,  FALSE);
	if (code != SUCCESS) 
	    {
	    if (code == V_BADTYPE)
		tmmsg (PROCFAIL,
    			"Type of #%d value does not match type of variable on left-hand side.", 
		       "TAE-MIXTYPE", (uintptr_t)count+1,0,0,0,0);
	    else if (code == V_BADCOUNT)
		tmmsg (PROCFAIL, "Too many or too few values for #%d value.",
		       "TAE-BADVALCT", (uintptr_t)count+1,0,0,0,0);
	    else if (code == V_BADSIZE)
		tmmsg (PROCFAIL, "String in value #%d too long.", 
		       "TAE-LONGSTR", (uintptr_t)count+1,0,0,0,0);
	    else if (code == V_BADVALID)
		tmmsg (PROCFAIL, 
    			"Value #%d is not one of the valid values for '%s'.", 
		       "TAE-INVVAL", (uintptr_t)count+1, 
		       (uintptr_t)(*symptr).v_name,0,0,0);
	    else if (code == V_BADFILE)
		tmmsg (PROCFAIL, "File in value #%d not found or incorrect file specification.",
		       "TAE-BADFILE", (uintptr_t)count+1,0,0,0,0);
	    else if (code == V_BADAMBIG)
		tmmsg (PROCFAIL, "Value #%d is an ambiguous value for '%s'.",
		       "TAE-AMBIGVAL", (uintptr_t)count+1, 
		       (uintptr_t)(*symptr).v_name,0,0,0);
	    else
		tmierr (11);	/* Error: Bad code from chk_vector in getfval */

	    return (FAIL);
	    }
	if (term == EOS) return (SUCCESS);
	}
    tmmsg (PROCFAIL, "Greater than %d values on FOR.", "TAE-TOOMNYFOR", 
    	   (uintptr_t)MAXFORVAL,0,0,0,0);
    return (FAIL);
    }
