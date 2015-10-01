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



/* TPAM CHECKOUT FILE_TIME=15-JUL-1987 16:36 DUA1:[TAEV2.OLB.GENLIB]TAEUTIL.C;6 */
/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:46 DUA1:[TAEV2.OLB.GENLIB]TAEUTIL.C;5 */
/* TDM CHECKOUT FILE_TIME= 5-MAR-1984 16:56 DUA0:[TAEV1.OLB]TAEUTIL.C;13 */
/* TDM CHECKOUT FILE_TIME= 6-JUL-1983 20:11 DUA0:[TAEV1.OLB]TAEUTIL.C;11 */
/* TAE general utility functions.
 * The functions in this source file are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	06-feb-84	Add function app_cmd() as a primitive...dm
 *	05-mar-84	Add s_sh2i...dm
 *	06-mar-84	Add flush_cmd...dm
 *	27-nov-84	Move par_q_parse() here from parmgen.c...peb
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *
 **************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	07-mar-85	Place functions m_pfmt and chk_parblk here...dm
 *	31-mar-85	Place functions irange and rrange here...dm
 **************************************************************************
 *
 *	26-jun-85	Avoid sscanf calls (bugs in V2.0 library) and
 *			better performance anyway...palm
 *	29-mar-87	Remove check for PARBLK.blksiz so that we can
 *			receive a PARBLK of any size....palm
 *	29-mar-87	Remove check on number of variables based on
 *			P_BYTES so that we can do a PARBLK of any size...palm
 *	14-jul-87	Allow locals with qualifiers...palm
 *	04-aug-87	Remove par_q_parse.  It has been replaced with
 *			the crack_name function in PARMGEN...palm
 *	19-oct-88	Take "#" as first line of command to mean null
 *			command so that PDFs can be executed from the
 *			shell with a PDF of the form:
 *				#! /tae/syslib/tm_tc
 *				procedure
 *				...
 *	13-may-89	don't display msg key if no key...palm
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tminc.inc"
#include	"parblk.inc"
#include "taeintproto.h"


FUNCTION static CODE chk_p_var
(
 struct PARBLK	*p,		/* in/out: PARBLK pointer	*/
 struct VARIABLE *v,		/* in/out: variable to check	*/
 GENPTR		pool1,		/* in:  beginning of var pool area*/
 GENPTR		pool2,		/* in:  end of var pool area*/
 COUNT		*parms		/* in/out: number of parms so far*/
 );


/*	chk_parblk.   Check integrity of a PARBLK structure.
 *
 *	We assume that the pointers in the block have already been
 *	made absolute.   PARBLKs from the outside world (disk or
 *	user process) should be checked to avoid TM crashes because
 *	of programmer error.
 *
 *	Note that the parblk is actually "in/out" here because
 *	we stuff v_name[NAMESIZ] with EOS to maximize the length
 *	of the variable name.
 *
 *	Checks special pointers (i.e., v_dvp, v_valid, v_pdf).
 */

FUNCTION CODE chk_parblk 
(
 struct PARBLK	*p		/* in/out: PARBLK pointer	*/
 )
    {
    GENPTR		pool1;
    GENPTR		pool2;	
    COUNT		parms;
    struct VARIABLE	*v;
    COUNT		flag;
    CODE		code;

    flag = 0;
    parms = 0;
    pool1 = (GENPTR)((*p).pool);		/* low pool limit	*/
    pool2 = (GENPTR) p  +   (*p).blksiz;	/* high limit		*/    
    for (v=(*p).symtab.link; v != NULL; v=(*v).v_link)
        {
	code = chk_p_var(p, v, pool1, pool2, &parms); /* check this variable...	*/
						/* 'parms' bumped by chk_p_var	*/
	if (code != SUCCESS)
	    goto var_chk;
	}
    return (SUCCESS);
    var_chk:		flag++;		/* 1 */
    return (FAIL);
    }

/*
 *	chk_p_var - check one variable in a PARBLK.
 *
 *	The 'parms' argument is incremented on behalf of the caller
 *	for each variable checked.  This is done because parameter qualifiers
 *	are actually themselves variables and cause recursive calls 
 *	to chk_p_var.
 */

FUNCTION static CODE chk_p_var
(
 struct PARBLK	*p,		/* in/out: PARBLK pointer	*/
 struct VARIABLE *v,		/* in/out: variable to check	*/
 GENPTR		pool1,		/* in:  beginning of var pool area*/
 GENPTR		pool2,		/* in:  end of var pool area*/
 COUNT		*parms		/* in/out: number of parms so far*/
 )
    {
    GENPTR		gp;
    COUNT		flag;
    COUNT		i;
    struct VARIABLE	*vq;		/* parameter qualifier		*/
    CODE		code;

#define GOOD_PTR(ptr)  (pool1 <= (GENPTR) ptr  &&  (GENPTR) ptr <= pool2)

    (*parms)++;
    flag = 0;
    if (!GOOD_PTR(v))
	goto link;
    (*v).v_name[NAMESIZ] = EOS;		/* max name size	*/
    if ((*v).v_type != V_INTEGER &&
	(*v).v_type != V_REAL    &&
	(*v).v_type != V_STRING  &&
	(*v).v_type != V_NAME)		/* V_NAME OK in compiled*/
	goto type;

/* current value related checks	*/

    if ((*v).v_type == V_NAME)
	{
	if ((*v).v_class != V_PARM)
	    goto type;
	if (!(*v).v_pv12)		/* if var from old TAE...		*/
	    goto name_v_ref;		/* TYPE=NAME didn't go to PARBLKs	*/
	if ((*v).v_nref != NULL  &&  !GOOD_PTR((*v).v_nref))
	    goto nameref;
	}
    else
	{
	if ((*v).v_count > MAXVAL)
	    goto count;
	if ((*v).v_count > 0)		/* check cvp if count > 0 */
	    if (!GOOD_PTR((*v).v_cvp))
		goto value;
	if ((*v).v_type == V_STRING)
	    {	
	    for (i=0; i < (*v).v_count; i++)
		{
		gp = (GENPTR) SVAL(*v, i);
		if (!GOOD_PTR(gp))
		    goto string;
		if (s_length(SVAL(*v,i)) > STRINGSIZ)
		    goto length;
		}
	    }

/* default value related checks	*/

	if ((*v).v_dcount > MAXVAL)
	    goto dcount;
	if ((*v).v_dcount > 0)		/* check cvp if count > 0 */
	    if (!GOOD_PTR((*v).v_dvp))
		goto dvalue;
	if ((*v).v_type == V_STRING)
	    {	
	    for (i=0; i < (*v).v_dcount; i++)
		{
		gp = (GENPTR) DSVAL(*v, i);
		if (!GOOD_PTR(gp))
		    goto dstring;
		if (s_length(DSVAL(*v,i)) > STRINGSIZ)
		    goto dlength;
		}
	    }
	if (!(*v).v_pv12)				/* if from old TAE version */
	    (*v).v_valid = NULL;			/* clear bad valid pointer */
	if ((*v).v_valid != NULL  &&  !GOOD_PTR((*v).v_valid))
	    goto valid;
	}
    if ((*v).v_class != V_GLOBAL &&
	(*v).v_class != V_PARM &&
	(*v).v_class != V_LOCAL)
	goto class;
    if ((*v).v_class == V_GLOBAL)
	if ((*v).v_pdf != NULL  &&  !GOOD_PTR((*v).v_pdf))
	    goto defpdf;
    if ((*v).v_pv12)					/* not if from old TAE	*/
	{						/* parameter qualifiers	*/
	for (vq = (*v).v_qualst.link; vq != NULL; vq = (*vq).v_link)
	    {
	    code = chk_p_var(p, vq, pool1, pool2, parms); /* recursive - chk qual*/
	    if (code != SUCCESS)
		goto parmqual;
	    }
	}
    return (SUCCESS);

    /* for debug, break on 'fail' and look at flag and v	*/

    name_v_ref:		flag++;		/* 17*/
    		        flag++;		/* 16*/
    parmqual:		flag++;		/* 15*/
    nameref:		flag++;		/* 14*/
    dcount:		flag++;		/* 13*/
    dvalue:		flag++;		/* 12*/
    dstring:		flag++;		/* 11*/
    dlength:		flag++;		/* 10*/
    valid:		flag++;		/* 9 */
    class:		flag++;		/* 8 */
    defpdf:		flag++;		/* 7 */
    link:		flag++;		/* 6 */
    type:		flag++;		/* 5 */
    count:		flag++;		/* 4 */
    value:		flag++;		/* 3 */
    string:		flag++;		/* 2 */
    length:		flag++;		/* 1 */
    		return (FAIL);
    }

/*
 *  Raise num to exp (an integer)
 */

/* app_cmd - Work towards building a full TAE command string, by 
 * appending the next single input line to the accumulated string so far. 
 * Strips comments and trailing blanks.
 * "+" as last non-comment character on input line indicates continuation.
 *
 * Function returns:
 *		CONTINUE, if another continuation line is expected,
 * 		SUCCESS,  if this is the last line of the command,
 *		FAIL,     if there is not enough room in the command string.
 *
 * NOTE: This function the single input line as well as the accumulated
 *	 command string.
 *
 */

FUNCTION CODE app_cmd 
(
 TEXT	inbuf[],		/* in/out: single input line buffer  */
 TEXT	cmdstr[],		/* in/out: accumulated cmd string    */
 BOOL	inquote		/* in/out: TRUE if quote in effect   */
 )
    {
    BOOL	cont;			/* TRUE if next line is continuation */
    FAST	COUNT	i;
    COUNT	cmdlen;			/* length of input cmdstr	     */

    if (!cmdstr[0] && inbuf[0] == '#')		/* if "#" is first character */
	return (SUCCESS);			/* then treat as null cmd    */
    for (i = 0; inbuf[i] != EOS; i++)
	{
	if (!inquote)				/* non-quoted  '!' 	    */
	    if (inbuf[i] == '!') break;
	if (inbuf[i] == '\"') inquote = !inquote;	/* toggle flag	    */
	}
    inbuf[i] = EOS;
    for (i--; i >= 0; i--)			/* strip trail blanks	    */
	{
	if (inbuf[i] != ' ' && inbuf[i] != '\t')
	    break;
	inbuf[i] = EOS;
	}
    if (i >= 0)					/* EOS at i+1		    */
	cont = (inbuf[i] == '+');
    else
	cont = FALSE;
    cmdlen = s_length(cmdstr);
    if (i + 1 + cmdlen > CMDLINSIZ)		/* if resultant str too big */
	return(FAIL);
    if (cont) inbuf[i] = ' ';			/* replace with blank 	    */
    s_copy(inbuf, &cmdstr[cmdlen]);		/* append to full string    */
    return(cont ? CONTINUE : SUCCESS);
    }

/*
 * m_pfmt. Format the record with the message
 */
 
FUNCTION VOID  m_pfmt
(
 TEXT 	control[],		/* in: control string		*/
 TEXT	key[],			/* in: message key		*/
 TEXT	mputrec[],		/* out: output string		*/
 uintptr_t a1,			/* in: integers or string ptrs	*/
 uintptr_t a2,
 uintptr_t a3,
 uintptr_t a4,
 uintptr_t a5
)
    {
    TEXT loccon[2*STRINGSIZ+KEYSIZ+4];	/* local version of control	*/

    loccon[0] = EOS;
    if (key  && key[0] && !s_equal (key, " "))  /* if substantial key	*/
	{
	s_copy("[", loccon);		        /* prefix the message key */
	s_append(key, loccon);		
	s_append("] ", loccon);
	}
    s_append(control, loccon);

/***************** Note, *********************
This isn't strictly a safe operation. We are passing uintptr_t for things
that might be a pointer or a int. uintptr_t is large enough, but sprintf
might fail because it isn't the type expected. Unfortunately, there is
no portable way to pass a variable number of arguments through a function
- a1 through a5 have to have a type. This works under gcc, and is likely
to work on all systems. But be aware that this is a potential problem.
***********************************************/

    sprintf(mputrec, loccon, a1, a2, a3, a4, a5);	/* temporary for...	*/
    if (s_length(mputrec) > STRINGSIZ)			/* allow upto STRINGSIZ */
	s_copy("...", &mputrec[STRINGSIZ-3]);		/* truncate with ...*/
    return;
    }

/* irange - decode an integer string of format "low:high"  or "low".
 */

FUNCTION CODE irange
(
 TEXT	string[],		/* integer range string		*/
 TAEINT	*ilow,			/* out: low value		*/
 TAEINT	*ihigh			/* out: high value		*/
 )
    {
    COUNT	i;
    TEXT	lstring[STRINGSIZ+1];
    TAEINT	itemp;


    *ilow = *ihigh = 0;			/* init in case error		*/
    s_copy(string, lstring);		/* local copy			*/
    i = s_index(string, ':');
    if (i < 0)				/* if no colon present		*/
	{
	if (s_s2i(lstring, ilow) != SUCCESS) return (FAIL);
	*ihigh = *ilow;
	}
    else
        {
        if (s_s2i(&(lstring[i+1]), ihigh) != SUCCESS) return (FAIL);
        lstring[i] = EOS;		/* terminate at colon		*/
        if (s_s2i(lstring, ilow) != SUCCESS) return(FAIL);
	if (*ilow > *ihigh)		/* reverse if out of order	*/
	    {
	    itemp = *ilow;
	    *ilow = *ihigh;
	    *ihigh = itemp;
	    }
        }
    return (SUCCESS);
    }


/* rrange - decode a real string of format "low:high"  or "low".
 */

FUNCTION CODE rrange
(
 TEXT	string[],		/* integer range string		*/
 TAEFLOAT	*rlow,			/* out: lower value		*/
 TAEFLOAT	*rhigh			/* out: high value		*/
 )
    {
    COUNT	i;
    TEXT	lstring[STRINGSIZ+1];
    TAEFLOAT	rtemp;


    *rlow = *rhigh = 0;			/* init incase error		*/
    s_copy(string, lstring);		/* local copy			*/
    i = s_index(string, ':');
    if (i < 0)				/* if no colon present		*/
	{
	if (s_s2r(lstring, rlow) != SUCCESS) return (FAIL);
	*rhigh = *rlow;
	}
    else
        {
        if (s_s2r(&(lstring[i+1]), rhigh) != SUCCESS) return (FAIL);
        lstring[i] = EOS;		/* terminate at colon		*/
        if (s_s2r(lstring, rlow) != SUCCESS) return(FAIL);
	if (*rlow > *rhigh)		/* reverse if out of order	*/
	    {
	    rtemp = *rlow;
	    *rlow = *rhigh;
	    *rhigh = rtemp;
	    }
        }
    return (SUCCESS);
    }

/*
 *	s_sh2i		Hex string to integer conversion
 */
FUNCTION CODE s_sh2i
(
 TEXT	s[],			/* hexadecimal numeric string	*/
 LONG	*i			/* out: the integer		*/
 )
    {
    LONG	accumulator;

    accumulator = 0;
    while (1)
	{
	if (*s <= '9' && *s >= '0')
	    accumulator = 16*accumulator + *s - '0';
	else if (*s <= 'f' && *s >= 'a')
	    accumulator = 16*accumulator + *s - 'a' + 10;
	else if (*s <= 'F' && *s >= 'A')
	    accumulator = 16*accumulator + *s - 'A' + 10;
	else
	    break;
        s++;
        }
    if (*s != EOS)
	{
	*i = 0;
	return (FAIL);			/* bad character found	*/
	}
    *i = accumulator;
    return (SUCCESS);    
    }

/*
 *	flush_cmd. Flush the current command string.
 *	NOTE: This routine should be called after the command 
 *	string is detected to be too long.
 */

FUNCTION  VOID  flush_cmd
(
 struct SFILE 	*sptr,		/* IN: sfile of the command file    */
 TEXT		inbuf[],	/* IN/OUT: last line read from file */
 TAEINT		*numrec	/* OUT: number of records read	    */
 )
    {
    TEXT	dummystr[STRINGSIZ+1];		/* scratch buffer	   */
    CODE	code;


/*  call app_cmd for each record read, just to know when the string ends.
 */
    *numrec = 0;
    dummystr[0] = EOS;	
    while ((code = app_cmd(inbuf, dummystr, FALSE))== CONTINUE)
  	{					
	code = f_read(sptr, inbuf);		/* read next line	   */
  	if (code != SUCCESS) 
	    break;
	(*numrec)++;
	dummystr[0] = EOS;			/* throw away the record   */
	}
    return;
    }
