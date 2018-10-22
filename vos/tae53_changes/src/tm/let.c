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



/*	Procedure LET command Processing
 *	--------------------------------
 *
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *
 *	21-jul-83	Add check of variable name size...jtm
 *	10-oct-83	Fix unix lint errors...palm
 *	26-oct-83	Allow LET to increment count by one (pr 562)...jtm
 *	21-nov-83	Better checks for index+1 setting...palm
 *	23-nov-83	Add check for long expression in index...jtm
 *	07-feb-84	Y_BATCH-->Y_ABI...nhe
 *	14-feb-84	Fix 68000 portability error for valptr...palm
 *	02-aug-84	Fix let_do error checking (PR 396)...peb
 *	17-sep-84	Add process for error code V_BADAMBIG...lia
 *	06-dec-84	Add check for qualified name...jtm
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	30-mar-87	fix following bug: if $count(x) is n and you set
 *			x(n+1) to something, x(n+1) does not get translated
 *			by trans_valid because v_count was not set until
 *			after the set_component call...palm	
 *	22-jul-87	Add get_letcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	28-jun-90	Removed get_letcmd()...ljn
 */

#include	"taeconf.inp"	/* TAE configuration definitions	*/
#include	"symtab.inc"	/* Symbol table				*/
#include	"tminc.inc"	/* TM-only host-independent definitions	*/
#include	"syninc.inc"
#include "taeintproto.h"


/*	Macros		*/

#define	GETC_CTX (!(*(cmdctx->sb->curchr)) ? 0 : *(cmdctx->sb->curchr++))
#define	NEXTC_CTX (*(cmdctx->sb->curchr))


/* FUNCTIONS */

/* Table of intrinsic commands in this group:	*/

#define Y_LET Y_BODY|Y_CMD|Y_ABI

FUNCTION CODE let_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Proc context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: LET cmd context (w/synblk) */


 );
    GLOBAL struct ITRCMD letcmd[] = 	/* let commands			*/
	{
{0, "LET",	"",	Y_LET, 0,	NULL,	let_do },
{0, ""}						/* TERMINATOR ENTRY*/
	};


/*    let_do - 	This function processes the LET procedure command.
 *
 *	Function return codes:
 *		
 *		DO_SUCCESS
 *		DO_CHECK
 *
 */
FUNCTION CODE let_do 
(
    struct	CONTXT	*proctx,	/* IN/OUT: Proc context block */
    struct	CONTXT	*cmdctx	/* IN/OUT: LET cmd context (w/synblk) */


 )
    {
/*	Local Variables */
    
    FAST	CODE	code;

    struct	VARIABLE	*symptr;
    TEXT	token[TOKESIZ+1];
    COUNT	count;
    GENPTR	valptr;    		/* Pointer to vector of values */
    CODE	term;
    struct 	SYNBLK	token_sb;	/* Syntax block for evaluation of indices */
    FAST	CODE	i;
    CODE	index;

/*
 *		**********************************************
 *		* First, extract the first field, which must *
 *		* be a defined symbol.			     *
 *		**********************************************
 */

    if ((code = gettok ((*cmdctx).sb, token)) == S_WHITE)
    	       code = gettok ((*cmdctx).sb, token); /* Extract variable from syntax block */
    if ((code != S_ALPHA) && (code != S_QALPHA))
	{
	tmmsg (PROCFAIL,"Invalid form for LET.  Use 'LET variable = expression'.",
	       "TAE-BADLET",0,0,0,0,0);
	return (DO_CHECK);

	}
    /*		SEARCH SYMBOL TABLE 		*/

    if (s_length (token) > F_Q_NAMESIZ)
	{
	tmmsg (PROCFAIL, "Variable name too long on LET. Maximum length allowed is %d.",
	       "TAE-LONGNAME", (uintptr_t) F_Q_NAMESIZ,0,0,0,0);
	return (DO_CHECK);
	}
    symptr = search (token, proctx);
    if (symptr == NULL)
	{
	tmmsg (PROCFAIL,"Reference to undefined variable '%s'.",
	       "TAE-UNDEFVAR",	(uintptr_t)token,0,0,0,0);
	return (DO_CHECK);
	}

    /* NOW GET THE ASSIGNMENT CHARACTER */

    code = fndsep ((*cmdctx).sb);
    if (code == S_COMSEP)
	{
	tmmsg (PROCFAIL,"Invalid form for LET.  Use 'LET variable = expression'.",
	       "TAE-BADLET",0,0,0,0,0);
	return (DO_CHECK);
	}
/*
 *		**********************************************
 *		* Get index value, if present		     *
 *		**********************************************
 */

    if ((NEXTC_CTX) == '=')
	{
    	index = 0;				/* Indicate no index for later */
    	}
    else if ((NEXTC_CTX) == '(')
	{
	for (i = 0; (NEXTC_CTX) != '=';)
	    {
	    if ((NEXTC_CTX) == EOS)
		{
		  tmmsg (PROCFAIL, "No value on LET.", "TAE-NOLETVAL",0,0,0,0,0);
		return (DO_CHECK);
		}
	    else if (i >= TOKESIZ)
	        {
	        tmmsg (PROCFAIL, 
	        "Expression in index too long.  Max is %d characters.",
		       "TAE-LONGINDX", (uintptr_t)TOKESIZ,0,0,0,0);
	        return (DO_CHECK);
	        }	        
	    token[i++] = GETC_CTX;		/* Save character for evaluator */
	    }
	token[i] = EOS;	    			
	initok (&token_sb, token);		
	code = evalexp (&token_sb, proctx, V_INTEGER, 
			&valptr, &count, &term);

	if (code != SUCCESS)
	    return (DO_CHECK);
	index = *((TAEINT*) valptr);		/* Get the index value */
	tae_free (valptr);			/* And free space now */

/*
 *		**********************************************
 *		* Index value present; check for validity    *
 *		**********************************************
 */

	if (count != 1) goto bad_index;		/* index must be singly valued */

	if ((*symptr).v_count == -1) 
	    {
	    if (index != 1) goto bad_index;	/* if currently no value, 
 *						   index must be 1          */
	    if ((*symptr).v_minc > 1)		/* mincount must be 1	    */
	 	goto bad_valct;
	    }
	else if (index > (*symptr).v_count+1)
		goto bad_index;			/* new index cannot increment
 *						   current count by more than 1 */

	if (index<1 || index>(*symptr).v_maxc)	
	    goto bad_index;
	}
    else
	{
	tmmsg (PROCFAIL,"Missing equal sign or incorrectly formatted FOR or LET.",
	       "TAE-MISEQ",0,0,0,0,0);
	return (DO_CHECK);
	}
    
    /* FINALLY, GET THE VALUE(S) */

    code = GETC_CTX;				/* Eat the equal sign */  
    code = evalexp ((*cmdctx).sb, proctx, (*symptr).v_type, 
	    	    &valptr, &count, &term);
    
    if (code != SUCCESS)
	return (DO_CHECK);
    if (term != EOS) 
	{
	tmmsg (PROCFAIL, "Unexpected characters at end of command line.",
	       "TAE-UNEXCH",0,0,0,0,0);
	return (DO_CHECK);
	}
    if (index == 0)
	code = chk_vector (symptr, (*symptr).v_type, valptr,
			   count, FALSE); 		/* Check new values */
    else
  	{
	if (count != 1)
	    {
	    tmmsg (PROCFAIL, 
	    "Multiple values or the null value not allowed with indexing.",
		   "TAE-BADIVAL",0,0,0,0,0);
	    delval(valptr, (*symptr).v_type, count);
	    return (DO_CHECK);
	    }
	code = chk_component (symptr, (*symptr).v_type, valptr);
	}
    if (code != SUCCESS) 
	{
	if (code == V_BADTYPE)
	    tmmsg (PROCFAIL,"Type of value does not match type of variable on left.",
		   "TAE-MIXTYPE",0,0,0,0,0);
	else if (code == V_BADCOUNT)
	    tmmsg (PROCFAIL, "Too many or too few values for variable '%s'.",
		   "TAE-BADVALCT", (uintptr_t)(*symptr).v_name,0,0,0,0);
	else if (code == V_BADSIZE)
	    tmmsg (PROCFAIL, "String too long.  Max string size for '%s' is %d.",
		   "TAE-LONGSTR", (uintptr_t)(*symptr).v_name, 
		   (uintptr_t)(*symptr).v_size,0,0,0);
	else if (code == V_BADVALID)
	    tmmsg (PROCFAIL, "Value not one of the valid values defined for '%s'.",
		   "TAE-INVVAL", (uintptr_t)(*symptr).v_name,0,0,0,0);
	else if (code == V_BADFILE)
	    tmmsg (PROCFAIL, "File not found or incorrect file specification.",
		   "TAE-BADFILE",0,0,0,0,0);
	else if (code == V_BADAMBIG)
	    tmmsg (PROCFAIL, "Ambiguous value for '%s'.", "TAE-AMBIGVAL",
		   (uintptr_t)(*symptr).v_name,0,0,0,0);
	else
	    tmierr (16);/* Error: Bad error code from chk_component in let_do */

 	delval(valptr, (*symptr).v_type, count);	/* deallocate value vector */
	return (DO_CHECK);
	}
    if (index == 0)
	code = set_value (symptr, valptr, count);
    else
	{
	if (index == (*symptr).v_count+1)
	    (*symptr).v_count++;			/* Increment count */
	else if ((*symptr).v_count == -1)
	    (*symptr).v_count = 1;			/* set count to one */
	code = set_component (symptr, valptr, index);
	}
    if (code != SUCCESS)
	{
	delval(valptr, (*symptr).v_type, count);	/* deallocate value vector */
        overr();
	return (DO_CHECK);
	}
    delval(valptr, (*symptr).v_type, count);	/* deallocate value vector */
    return (DO_SUCCESS);			/* allows $SFI to be set */

bad_index:
    tmmsg (PROCFAIL, "Invalid subscript on left side.", "TAE-BADLETSUBS",
	   0,0,0,0,0);
    return (DO_CHECK);

bad_valct:
    tmmsg (PROCFAIL, "Too few values for variable '%s'.",
	   "TAE-TOOFEWVAL", (uintptr_t)(*symptr).v_name,0,0,0,0);
    return (DO_CHECK);
    }
