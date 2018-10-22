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



/* TPAM CHECKOUT FILE_TIME= 8-AUG-1985 11:25 DUA1:[TAEV1.TM]PARSER.C;3 */
/* TJH CHECKOUT FILE_TIME= 3-JUL-1984 13:03 DUA1:[TAEV1.TM]PARSER.C;1 */
/* TNHE CHECKOUT FILE_TIME=18-OCT-1983 19:08 DUA0:[TAEV1.TM]PARSER.C;66 */

/*  
 * This is the SHIFT/REDUCE PARSING MACHINE.  Given a set of state 
 * tables in PSTATE format, and the pointer to the input stream, PARSE will
 * parse the input according the PSTATE tables, (trigerring "action routines"
 * in the process) until an EOS has been read from the input, or an error 
 * state is reached or the 'ACCEPT' state is reached.  
 *
 * This machine is based on "LR Parsing" by Aho and Ullman in the 6/74 
 * ACM Computing Surveys.
 *
 * See the end of this source file for additional background notes.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *
 *	25-aug-83	Added logic for nullables...jtm
 *	08-sep-83	Fixed dimension with SIZ to SIZ+1...palm
 *	10-oct-83	Fixed unix lint errors...palm
 *	18-oct-83	Fix unix problems with PSTATE...palm
 *	16-jun-84	For better error messages...nhe
 *      08-apr-85       Additions for underflow or overflow conditions ...joh
 *	09-aug-85	Optimized reduce operations...palm
 *	11-nov-88	Decrement op in popvstack pulled from MOVE_STRUCT...ljn
 *	19-apr-94	Concurrent port: prototyped getprim...dgf
 *
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"tminc.inc"	/* see note at end of this source	*/
#include	"syninc.inc"	/* syntax package			*/
#include 	"expinc.inc"	/* for the expression package		*/
#include "taeintproto.h"

    GLOBAL	COUNT	v36pa = 0;

#ifdef masscomp
CODE            getprim();
FUNCTION VOID deal_pval();
#endif

#ifndef testmain


FUNCTION VOID inivstack
(
    FUNINT		maxentries	/* in: maximimum number of entries allowed */

 );
FUNCTION CODE shift
(
    struct	SYNBLK	*sb,
    struct PSTATE	*statelist[],	/* in: array of pointers to states */
    struct	VALUE	*value,		/* out: value of the primitive  */
    CODE		*symbtype,	/* out: type of the primitive	*/
    CODE		*lookahead	/* out: the peeked-at next symbol */

 );
FUNCTION BOOL reduce
(
    struct PSTATE *statelist[],		/* in: list of state pointers	*/
    CODE	lookahead,		/* in: the 'look-ahead' symbol 	*/
    					/*  required by for a reduction */
    struct VALUE *value,		/* in: value of last symbol (for action routines)	*/
    CODE	*newsymb,		/* out: the new language symbol */
    					/*      after the reduction	*/
    CODE	*status,		/* out: FAIL means s/w problem	*/
    struct ERRMSG *(*errmsg)		/* out: error message		*/

 );
FUNCTION CODE gotostate
(
    struct PSTATE	*statelist[],	/* in: list of state pointers	*/
    FAST PA_CODE	symbol		/* in: the symbol that we just got */

 );

/*
 *	Globals
 */
#define MAXSTATE 30		/* maximum number of states in state stack */
    TINY st_stack[MAXSTATE+STACK_OVRHD]; /* The stack of indexes of pointers to states	*/
/* nb: probably want to allocate this at run time			*/

#define	MAX_VAL_NEST	MAXVAL		/* maximum number of value structures. */
    					/* same as max parenthetical nesting */
    GLOBAL COUNT	valtop;		/* index to top of stack	*/
    COUNT	valmax;		/* maximum number of entries in value stack */

    GLOBAL struct	VALUE	valstack[MAX_VAL_NEST];	/* stack of value structures */

/* NOTE: No error recovery within the line is attempted			*/


/*
 * Parse the line starting at the current syntax block pointer until
 * end-of-string, using state tables provided by the caller.  Trigger any
 * 'action' routines as indicated by the provided state table.
 *
 * Returns PA_SUCC or PA_FAIL or PA_EOS (failure on unexpected EOS)
 */
FUNCTION CODE parser
(
    struct SYNBLK	*sb,		/* in: syntax block		*/
    struct PSTATE	*statelist[],	/* in: list of state pointers	*/
    FUNINT		state0,		/* in: index of pointer to start state	*/
    					/* (W.R.T. the statelist)	*/
    struct ERRMSG	*(*errmsg)	/* out: ptr to error msg struct	*/

 )
    {
    CODE	code, status;
    CODE	newsymb;	/* new language symbol reduced to	*/
    CODE	symbtype;
    CODE	lookahead;	/* next primitive symbol from input	*/
    struct 	VALUE value;	/* value of primitive from input	*/



    *errmsg = NULL;			/* init ptr to error msg	*/
    inivstack(MAX_VAL_NEST);		/* initialize value stack	*/
    stk_in(st_stack, MAXSTATE, state0);	/* init stack, push state0	*/
    do	{
    	code = shift(sb, statelist, &value, &symbtype, &lookahead);	/* do a shift operation	*/
    	if (code==FAIL) 
    	    {
    	    if (symbtype==EOS)
    		return(PA_EOS);
    	    return(PA_FAIL);
    	    }
	else 
	    if (code==PA_ERRFLOW)
		return(PA_ERRFLOW);  
  	if (symbtype==PA_NUM || symbtype==PA_NAME 
	    || symbtype==PA_QUOTED || symbtype==PA_NULL)
    	    {
    	    code = pushvstack(&value);		/* if not an operator push the value */
    	    if (code == FAIL)
    		return(PA_FAIL);	/* assume user over-nested		*/
    	    }
    	while ( reduce(statelist, lookahead, &value, &newsymb, &status, errmsg))  /* reduce as much as possible */
    	    {
    	    if (status == FAIL) return(PA_FAIL);	/* error message set up by action */
    	    if (newsymb == PA_ACCEPT || newsymb == PA_ERROR) break;	/* we're done		*/
    	    code = gotostate(statelist, newsymb);	/* fix state stack according to */
    							/* new symbol			*/
    	    if (code == FAIL) 
    		{
    		tmierr(TMI_XSTATE);		/* fatal error: new state undeterminable */
    		return(PA_FAIL);
    		}
    	    }
    	}
    while( newsymb != PA_ACCEPT && newsymb != PA_ERROR);	/* continue until we hit the 'accept' sym */
    return(PA_SUCC);
    }

/*	parser_init.   Initialize a collection of state 
 *	tables.   The processing here consists of
 *	building the "reduce bit map" from the list of
 * 	valid reduce codes for each state.
 */

FUNCTION void parser_init 
(
    struct PSTATE *statelist[],		/* in: vector of state ptrs	*/
    FUNINT	   n			/* in: entries in vector	*/

 )
    {
    COUNT	i;
    struct PSTATE	*pstate;
    FAST PA_CODE 	*cv;		/* code_vector pointer		*/
    FAST COUNT		index, bit;	

    for (i=0; i < n; i++)
        {
	pstate = statelist[i];		/* current state		*/
        if (pstate == NULL)		/* if none, continue		*/
      	    continue;
        cv = (*pstate).redvec;		/* pointer to reduce vector	*/
        if (cv == NULL)			/* if init not needed:		*/
    	    continue;			/* by-pass bit map contruction	*/
	for (;  *cv >= 0;  cv++)
	    {	
    	    if (*cv >= PA_HIGHCODE)	/* tables built wrong?		*/
    		tmierr (TMI_BADCODE);
	    index = (*cv)/(sizeof(int)*8);
	    bit = (*cv) % (sizeof(int)*8);
	    (*pstate).reduce_map[index] |=  (1<<bit);	    /* set bit	*/
	    }
	}
    }

/*
 *  Go to the next state dictated by the state table
 *
 *  Return SUCCESS or FAIL
 */
FUNCTION CODE gotostate
(
    struct PSTATE	*statelist[],	/* in: list of state pointers	*/
    FAST PA_CODE	symbol		/* in: the symbol that we just got */

 )
    {
    COUNT		next;		/* index to next state		*/
    FAST struct SHFENT	*shifter;	/* pointer to a shift pair	*/
    CODE		code;
    struct PSTATE	*currst;	/* pointer to current PSTATE	*/

    currst = statelist[toptin(st_stack)];	/* get state pointer	*/
    shifter = (*currst).shfent;			/* shift list pointer	*/
    if (shifter == NULL)			/* if none, no shift	*/
	return (FAIL);
    for (;  (*shifter).currsymb >= 0;  shifter++)
    	{
    	if ((*shifter).currsymb == symbol)
    	    {
    	    next = (*shifter).nextstate; 
            code = pushti(st_stack, next);	/* push the new state   */
    	    if (code == STACK_FAULT) 
    		{
    		tmierr(TMI_OVERPUSH);		/* stack overflow	*/
    		return(FAIL);
    		}
    	    return(SUCCESS);
    	    }
    	}
    return(FAIL);
    }


/*
 * inivstack - initialize a value stack
 */
FUNCTION VOID inivstack
(
    FUNINT		maxentries	/* in: maximimum number of entries allowed */

 )
    {
    valtop = -1;			/* top means current top entry; none now */
    valmax = maxentries-1;		/* -1 because of zero indexing	*/
    return;
    }

/*
 *  Peek at, that is, get without disturbing the syntax block, the next 
 *  primitive in the input stream.
 */
FUNCTION CODE peekpri
(
    struct	SYNBLK	*sb		/* in: the syntax block		*/

 )
    {
    TEXT	*save;			/* pointer to a syntax block pos */
    CODE	code;
    struct VALUE dummy;

    save = (*sb).curchr;		/* save current position	*/
    code = getprim(sb, &dummy);		/* get the next primitive	*/
    deal_pval(&dummy, code);		/* deallocate any string	*/
    (*sb).curchr = save;		/* restore the current position	*/
    return(code);
    }

/*
 * popvstack - pop an element off the top of the value stack
 *
 * return FAIL or SUCCESS
 */
FUNCTION CODE popvstack
(
    struct	VALUE	*value		/* out: the value popped	*/

 )
    {
    if (valtop < 0) return(FAIL);	/* stack underflow		*/
    MOVE_STRUCT(valstack[valtop], *value); /* move structure and decrement */
    --valtop;
    return(SUCCESS);
    }

/*
 * pushvstack - push a value on the value stack
 *
 * return SUCCESS or FAIL
 */
FUNCTION CODE pushvstack
(
    struct	VALUE	*value		/* in: the structure to push	*/

 )
    {
    if (valtop >= valmax) return(FAIL); /* stack overflow		*/
    MOVE_STRUCT(*value, valstack[++valtop]);	/* increment and move structure */
    return(SUCCESS);
    }

/*
 *  Try a reduction using the current state.  Trigger action routine if
 *  the current state so indicates.
 *
 *  Return TRUE if a reduction took place
 */
FUNCTION BOOL reduce
(
    struct PSTATE *statelist[],		/* in: list of state pointers	*/
    CODE	lookahead,		/* in: the 'look-ahead' symbol 	*/
    					/*  required by for a reduction */
    struct VALUE *value,		/* in: value of last symbol (for action routines)	*/
    CODE	*newsymb,		/* out: the new language symbol */
    					/*      after the reduction	*/
    CODE	*status,		/* out: FAIL means s/w problem	*/
    struct ERRMSG *(*errmsg)		/* out: error message		*/

 )
    {
    FAST struct PSTATE *currst;		/* pointer to current state	*/
    COUNT	j;
    FAST COUNT	index, bit;
    CODE	code;

    *newsymb = PA_NOSYM;			/* assume no symbol	*/
    *status = SUCCESS;				/* assume success	*/
    currst = statelist[toptin(st_stack)];	/* current state	*/
    index = lookahead/(sizeof(int)*8) ;		/* bit mapping		*/
    bit =   lookahead % (sizeof(int)*8) ;	/* bit mapping		*/
    if ((*currst).reduce_map[index] & (1<<bit))	/* time to reduce?	*/
        {
	for (j=1; j <= (*currst).numpop; j++) 		/* pop n times	*/
	    if (poptin(st_stack) == STACK_FAULT)
		{
		tmierr(TMI_OVERPOP);			/* Stack underflow*/
		*status = FAIL;
		return(TRUE);
		}
	if ((*currst).action != NULL)
	    {
	    code = (*(*currst).action)(errmsg); 	/* call function */
	    if (code==FAIL) 
		{
		*status = FAIL;
		return(TRUE);
		}
	    }
	*newsymb = (*currst).newsymb;  			/* new symbol	*/
	return(TRUE);		   			/* we reduced	*/
	}
    return(FALSE);					/* no reduction */
    }

/* 
 *  Shift in the next symbol from the input stream		
 *
 *  Return SUCCESS or FAIL
 */
FUNCTION CODE shift
(
    struct	SYNBLK	*sb,
    struct PSTATE	*statelist[],	/* in: array of pointers to states */
    struct	VALUE	*value,		/* out: value of the primitive  */
    CODE		*symbtype,	/* out: type of the primitive	*/
    CODE		*lookahead	/* out: the peeked-at next symbol */

 )
    {
    CODE		code;

    *symbtype= getprim(sb, value);	/* get a primary symbol and value */
    if (*symbtype == PA_ERROR)		/* assume GETPRIM prints error */
	return(FAIL);  
    if (*symbtype == PA_ERRFLOW)	/* underflow or overflow */
	return(PA_ERRFLOW);  
    *lookahead = peekpri(sb);		/* look at the next symbol in line */
    if (*lookahead == PA_ERROR) 
    	code = FAIL;
    else if (*lookahead == PA_ERRFLOW)
	code = PA_ERRFLOW;
    else
    	code = gotostate(statelist, *symbtype);	/* fix state stack for next state	  */
    if (code == FAIL || code == PA_ERRFLOW)
    	deal_pval(value, *symbtype);	/* deallocate any string	*/
    return(code);
    }

#else
/*
 * Test for PARSER: parse "i1+i2" or "i1+i2+..." where "i-n" is an integer.
 */
    FUNCTION main ()

    {
    IMPORT	TEXT vrsion[];
    TEXT	string[STRINGSIZ+1];
    CODE	parse();
    struct	SYNBLK sb;
    CODE	code;
    COUNT	dummy;

/* define indexes for our list of pointers to states			*/
#define	s0pt	0
#define	s1pt	1
#define	s2pt	2
#define	s3pt	3
#define	s4pt	4
#define EV_SUM	PA_ACCEPT + 100
/* reference the action routines					*/
    CODE doneact(), sumact();
/* define the state structures						*/
    static struct SHFENT shift0[] = {
    			   EV_SUM, s1pt,  /* to s1 on EV_SUM		*/
    			   PA_NUM, s4pt,  /* to s4 on a number		*/
    			   PA_TABTERM};
    static struct PSTATE s0 = {0, 0, NULL, NULL, shift0};



    static PA_CODE red1[] = {EOS, PA_TABTERM};  	/* reduce if EOS */
    static struct SHFENT shift1[] = {'+', s2pt, PA_TABTERM};
    static struct PSTATE s1 = {1, PA_ACCEPT, doneact, red1, shift1};


    static struct SHFENT shift2[] = {PA_NUM, s3pt, PA_TABTERM};
    static struct PSTATE s2 = {0, 0, NULL, NULL, shift2};

    static PA_CODE red3[] = {EOS, '+', PA_TABTERM};  
    static struct PSTATE s3 = {3, EV_SUM, sumact, red3, NULL};

    static PA_CODE red4[] = {EOS, '+', PA_TABTERM};
    static struct PSTATE s4 = {1, EV_SUM, NULL, red4, NULL};

/* list of state pointers						*/
    struct PSTATE *statelist[]= {&s0, &s1, &s2, &s3, &s4};


    parser_init (statelist, sizeof(statelist)/sizeof(struct PSTATE *));
    printf(" Parser test, version %s\n", vrsion);
    printf(" Enter a string  ");
    t_init(&dummy, &dummy, &dummy);
    t_read(string, &dummy);
    for (; string[0]!='@';)	/* exit on '@'				*/
    	{
    	initok(&sb, string);
    	code = parser(&sb, statelist, s0pt);	/* call the parser	*/
    	printf("Parse complete. Success/fail: %d\n", code);
        printf(" Enter a string  ");
        t_read(string, &dummy);
    	}
    }

/* Action routine on end of parse recognized				*/
    FUNCTION CODE doneact()

    {
    printf(" doneact called \n");
    return(SUCCESS);
    }

/* Action routine on 'sum' reduction				*/
    FUNCTION CODE sumact()

    {
    printf(" sumact called \n");
    return(SUCCESS);
    }

#endif


/* NOTES:
 *
 *	- action routines provided by the caller and they keep the running
 *	  total for 'value'
 *
 *	- the reasons for the making the caller pass a table of state pointers
 *	  (the "state list") are:
 *		
 *		o C does not allow compile-time initialization of forward
 *		  pointers, therefore, we cannot have pointers directly
 *		  in the PSTATE structure.
 *
 *		o The two alternatives are to have a pointer to a pointer
 *		  or a pointer to an index table.  The former requires
 *		  either an external module or extensive run-time init.
 *		  We have chosen the latter.
 */
