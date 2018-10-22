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
 *	rqparmgen. rq_ functions to build and manipulate V block 
 *		   for remote parms.
 *
 *	Note: The functions in this source module are very similar
 *	to those in the qparmgen module. However we can not use 
 *	qparmgen because of the following:
 *
 *	    - it deals with PARM types only (no GLOBAL/LOCAL variables)
 *	    - does not allow to specify VALID values for a variable
 *	    - uses xmput for error reporting which assumes the user is a
 *	      subprocess of TAE, which may not be true, as in the case
 *	      of the "file server" process.
 *
 *	Also, we do not perform much error checking here because it is 
 *	assumed that the input variables are already checked before this
 *	package is called to put them in the V_block pool.
 *
 *****************************************************************************
 *
 *	CHANGE LOG:
 *
 *	25-mar-85	Initial release...dm
 *	31-mar-85	Fixed portability errors...dm
 *	05-nov-86	Eliminate TAE_RCJM check...nhe
 *	24-jun-87	Correct treatment of min count in rq_addvar...nhe
 *	26-jun-87	Correct max string size loop in rq_addvar...nhe
 *			and correct handling of nullables in rq_addvar
 *      04-apr-88       Added string size in call to q_alloc...tpl
 *	26-jan-89	New POINTER_VALIDS logic...palm
 *	06-may-91	Replaced a NULL with 0 (see VMS def of NULL)...ljn
 *	06-may-92	Used new sentinel P_BIGSENTINEL...tpl
 *
 *****************************************************************************/


#include		"taeconf.inp"		/* TAE configuration defs     */
#include		"tminc.inc"		/* Variable struct defs	      */
#include		"parblk.inc"		/* parameter block defs	      */
#include		"syninc.inc"		/* syntax package defs        */
#include		"resinc.inc"		/* restricted allocation defs */
#include "taeintproto.h"


FUNCTION  static  CODE  trans_val_p
(
 ALIGN		*pool,		/* in/out: pool address	      	*/
 struct  VARIABLE *v,		/* in/out: variable structure   */
 TEXT		*valvec[]	/* in: values in string format  */
);
FUNCTION  static  CODE  trans_vld_p
(
 ALIGN		*pool,		/* in/out: pool address	      	*/
 struct  VARIABLE *v,		/* in/out: variable structure   */
 TEXT		*valvec[],	/* in: valid values as strings  */
 FUNINT		validcnt	/* in: count of valid strings   */
 );




/* 	rq_bldhdr. Build the par file header
 */

FUNCTION  VOID rq_bldhdr
(
    GENPTR		p			/* out: par file header	   */
 )
    {
    struct  PARHDR	*ph;

    ph = (struct PARHDR *) p;
    s_copy(P_BIGSENTINEL, (*ph).sentinel);
    (*ph).recsize = (*(struct PARBLK *)p).blksiz;  /* not really known now    */
    s_copy("TIME", (*ph).datetime);		   /* dummy date/time for now */
    return;
    }

/*
 *	rq_init.  Initialize remote V-block.
 */

FUNCTION VOID rq_init
(
 struct PARBLK	*p,		/* PARBLK to initialize		*/
 FUNINT		pool_size	/* bytes allocated in p.pool	*/
)
    {

    (*p).msgtyp = 0;
    (*p).last   = 1;
    (*p).blksiz = 0;			
    (*p).numvar = 0;			/* NOTE: not maintained		*/
    (*p).actmod = 0;
    (*p).vers   = 0;
    (*p).symtab.link = NULL;
    r_init((*p).pool, pool_size);	/* initialize for r_ package    */
    (*p).symtab.link = NULL;
    (*p).mode = P_CONT;
    return;
    }

/*
 *	rq_addvar. Add a remote variable to the V_block.
 *
 *	Note: all input parameters are in TEXT form because that is how
 *	they are  transmitted across the node.
 */
	
FUNCTION   CODE  rq_addvar
(
 struct PARBLK	*p,		/* in/out: V-block		  */
 TEXT		name[],		/* in: variable name		  */
 TEXT		class[],	/* in: variable class		  */
 TEXT		type[],		/* in: variable type		  */
 TEXT		access[],	/* in: type of access if a file   */	
 TEXT		count[],	/* in: variable count		  */
 TEXT		values[],	/* in: string with current values */
 TEXT		valids[]	/* in: string with valid values	  */
 )
    {
    CODE		rv_class;	/* class			  */
    CODE		rv_type;	/* type				  */
    COUNT		rv_count, rv_mincnt, rv_maxcnt;	/* counts	  */
    COUNT		rv_size;
    CODE		rv_filemode = 0;
    BOOL		rv_file;
    BOOL		rv_nullable = FALSE;
    TEXT		*valvec[MAXVAL];	/* ptr to allocated values */
    TEXT		tempstr[STRINGSIZ+1];
    struct  SYNBLK	sb;			/* syntax block		   */
    struct  VARIABLE	*v;
    struct  VARIABLE	*pc, *backlink = 0;
    COUNT		start;
    COUNT		i, j, n;
    COUNT		validcnt;
    CODE		code;
    TEXT		*nullable[MAXVAL];	/* nullable string	*/

/*********  			CLASS			***********/
    if (s_equal(class, "PARM"))
	rv_class = V_PARM;
    else if (s_equal(class, "GLOBAL"))
	rv_class = V_GLOBAL;
    else
	rv_class = V_LOCAL;

/********* 			TYPE			************/
    rv_file = FALSE;
    if (s_equal(type, "INTEGER"))
	rv_type = V_INTEGER;
    else if (s_equal(type, "REAL"))
	rv_type = V_REAL;
    else if (s_equal(type, "FILE"))
	{
	rv_type = V_STRING;
	rv_size = MAXSTRSIZ;
	rv_file = TRUE;
	}

/*			type (continued..)				*/	
    else
	{
	rv_type = V_STRING;
	rv_size = MAXSTRSIZ;			/* default		*/
	start = s_index(type, ',');		/* look for ","   	*/
	if (start > 0)				/* size specified	*/
	    {
	    for (i=start+1,j=0 ; type[i]>='0' && type[i] <='9'; i++, j++)
		tempstr[j] = type[i];
	    tempstr[j+1] = 0;
	    code = s_s2i(tempstr, &rv_size);
	    }
	}
/**********		ACCESS			*********************/
    if (rv_file)
	{
	if (s_equal(access, "IN")) rv_filemode = V_IN;
	else if (s_equal(access, "OUT")) rv_filemode = V_OUT;
	else if (s_equal(access, "INOUT")) rv_filemode = V_INOUT;
	else if (s_equal(access, "NOCHECK")) rv_filemode = V_NOCHECK;
	}

/***********		MAX/MIN COUNT			 ********************/

    if (NULLSTR(count))
	rv_maxcnt = rv_mincnt = 1;			/* default	     */
    else
	{
    	initok(&sb, count);
        code = getval(&sb, nullable, MAXVAL, &n);
	rv_nullable = (*nullable[0] == '0');	/* i.e., if 1st element is "0"*/
    	free_val (nullable, n);
	irange(count, &rv_mincnt, &rv_maxcnt);		/* get min/max range */
	}
	

/***********		CURRENT  COUNT AND VALUES 	*****************/
    
    initok(&sb, values);
    code = getval(&sb, valvec, rv_maxcnt, &rv_count);	/* get value strings */
    if (code == 0)
	return (FAIL);


/* 	Allocate a variable structure in the restricted pool and move values */

    v = q_alloc(&(*p).symtab, (*p).pool, name, rv_type, rv_count, 0);
    if (v == NULL)				/* no more room		     */
	goto rpalloc_err;			/* restricted pool alloc err */
    (*v).v_class = rv_class;			/* initialize		     */
    (*v).v_type = rv_type;
    (*v).v_count = rv_count;
    (*v).v_minc = rv_mincnt;
    (*v).v_maxc = rv_maxcnt;
    (*v).v_file = rv_file;
    (*v).v_size = rv_size;
    (*v).v_filemode = rv_filemode;
    (*v).v_nullable = rv_nullable;
    (*v).v_dvp = NULL;
    (*v).v_dcount = 0;
    (*v).v_valid = NULL;

    (*v).v_count = rv_count; 
    code = trans_val_p((*p).pool, v, valvec);
    free_val(valvec, rv_count);			/* free dynamic storage	*/
    if (code == P_NOROOM) 
	goto rpalloc_err;

/************		GET VALID VALUES/RANGES		******************/
	
    if (!NULLSTR(valids))			/* valid strings present    */
	{
	initok(&sb, valids);
	code = getval(&sb, valvec, MAXVAL, &validcnt);
	if (code == FAIL)
	    return (FAIL);
	code = trans_vld_p((*p).pool, v, valvec, validcnt);
	free_val(valvec, validcnt);		/* free dynamic storage	*/
	if (code == P_NOROOM) 
	    goto rpalloc_err;
	}

/**********		PARAMETER  QUALIFIERS	*********************/
/*
 *	for now, no parameter qualifiers are sent across the node   
 */

    return (SUCCESS);

rpalloc_err:					/* restricted pool alloc err */
    for(pc=(struct VARIABLE *) &(*p).symtab; (*pc).v_link != NULL; 
	  backlink=pc, pc=(*pc).v_link);	/* find end of chain	     */
    (*backlink).v_link = NULL;			/* unlink last entry in chain */
    return(P_NOROOM);
    }


/*
 *	trans_val_p. Transfer values to restricted pool.
 *
 *	NOTE: This routine converts the input values from string form
 *	to appropriate integer/real/string from and moves them to
 *	the v_cvp vector which is already allocates in the pool.
 *
 *	Return Codes:
 *		SUCCESS
 *		P_NOROOM (for string variables only)
 *
 */
   
FUNCTION  static  CODE  trans_val_p
(
 ALIGN		*pool,		/* in/out: pool address	      	*/
 struct  VARIABLE *v,		/* in/out: variable structure   */
 TEXT		*valvec[]	/* in: values in string format  */
 )
    {
    COUNT		i;
    TEXT		*loc_vector[MAXVAL];	/* address of string in pool */

    if ((*v).v_type == V_INTEGER)
	{
	for (i = 0; i < (*v).v_count; i++)
	    {
	    if (NULLSTR(valvec[i]))
		IVAL(*v, i)= 0;
	    else
		s_s2i(valvec[i], &IVAL(*v, i));	
	    }
	}
    else if ((*v).v_type == V_REAL)
	{
	for (i=0; i < (*v).v_count; i++)
	    {
	    if (NULLSTR(valvec[i]))
		RVAL(*v, i) = 0.0;
	    else					/* convert to binary	*/
		s_s2r(valvec[i], &RVAL(*v, i));
	    }
	}
    else if ((*v).v_type == V_STRING)
	{
	for (i=0; i < (*v).v_count; i++)
	    {
	    loc_vector[i] = q_save(pool, valvec[i]);
	    if (loc_vector[i] == NULL)
		return (P_NOROOM);
	    SVAL(*v, i) = loc_vector[i];		/* new value vector */
	    }
	}
    return (SUCCESS);
    }


/*
 *	trans_vld_p. Transfer valid ranges/values to restricted pool.
 *
 *	NOTE: This routine converts the input values from string form
 *	to appropriate integer/real/string from and moves them to
 *	the v_valid  structure which is allocates in the pool.
 *
 *	Return Codes:
 *		SUCCESS
 *		P_NOROOM 
 *
 */
   
FUNCTION  static  CODE  trans_vld_p
(
 ALIGN		*pool,		/* in/out: pool address	      	*/
 struct  VARIABLE *v,		/* in/out: variable structure   */
 TEXT		*valvec[],	/* in: valid values as strings  */
 FUNINT		validcnt	/* in: count of valid strings   */
)
    {
    COUNT		i;
    COUNT		vld_size = 0;		/* count of valis struct     */
    struct  I_VALID	*ivalid;
    struct  R_VALID	*rvalid;
    struct  S_VALID	*svalid;
    TEXT		*s;

/*	allocate valid structure in pool		*/
    if ((*v).v_type == V_INTEGER)
	vld_size = sizeof (struct I_VALID) + 
		(validcnt-1)*sizeof (struct I_RANGE);
    else if ((*v).v_type == V_REAL)
	vld_size = sizeof (struct R_VALID) + 
		(validcnt-1)*sizeof (struct R_RANGE);
    else if ((*v).v_type == V_STRING)
	vld_size = sizeof (struct S_VALID) + 
		(validcnt-1)*sizeof (struct S_RANGE);

    (*v).v_valid = r_alloc(pool, vld_size);
    if ((*v).v_valid == NULL) 
	return (P_NOROOM);			/* allocation failure */


/* 	convert valid strings to proper format and move to pool */

    if ((*v).v_type == V_INTEGER)
	{
	ivalid = (struct I_VALID *) (*v).v_valid; 
   	for (i=0; i < validcnt; i++)
	    irange (valvec[i], &((*ivalid).range[i].low),
	        &((*ivalid).range[i].high));
	(*ivalid).count = validcnt;
	}
    else if ((*v).v_type == V_REAL)
	{
	rvalid = (struct R_VALID *) (*v).v_valid; 
	for (i=0; i < validcnt; i++)
	    rrange (valvec[i], &((*rvalid).range[i].low),
	        &((*rvalid).range[i].high));
	(*rvalid).count = validcnt;
	}
    else if ((*v).v_type == V_STRING)
	{
	svalid = (struct S_VALID *) (*v).v_valid;
	for (i=0; i < validcnt; i++)
	    {
#ifdef POINTER_VALIDS
	    s = r_alloc (pool, s_length(valvec[i]+1));
	    if (s == NULL)
		return (P_NOROOM);
	    (*svalid).slist[i].string = s;
#endif
	    s_copy(valvec[i], (*svalid).slist[i].string);
	    }
	(*svalid).count = validcnt;
	}
    return (SUCCESS);
    }
