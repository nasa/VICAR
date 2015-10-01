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



/*	TCL Functions
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	10-oct-83	Fix unix lint/compilation errors...palm
 *	23-nov-83	PR 589 fixes...nhe
 *	13-mar-84	Add $EXIST...palm
 *	15-mar-84	Replace $EXIST with $GLOBAL...palm
 *	30-may-86	Add comments concerning VALUE.null...palm
 *	05-jun-88	$ASFI and $ASKEY...ljn
 *	13-nov-90	Add $PANEL (see tclwindows.c); inival() made glbl...ljn
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tminc.inc"	/* For TM				*/
#include	"expinc.inc"	/* for expressions			*/
#include	"asyncinc.inc"
#include "taeintproto.h"

    GLOBAL	COUNT	v020fn = 0;

CODE flt_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE fix_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE cnt_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE strl_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE global_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE asfi_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE askey_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
CODE panel_func( struct VALUE *, struct CONTXT *, struct VALUE *,
	       struct ERRMSG **);
FUNCTION VOID inival
(
    struct VALUE	*value			/* in/out: the value	*/
 );

/* Function descriptors */
    struct TCLFUNC tcl_func[]=
    {
      {"$ASFI",  asfi_func, 1, {V_STRING}},
      {"$ASKEY", askey_func, 1, {V_STRING}},
      {"$FLOAT", flt_func, 1, {V_INTEGER}},
      {"$FIX",   fix_func, 1, {V_INTEGER}},
      {"$COUNT", cnt_func, 1, {V_NAME}},
      {"$STRLEN",strl_func,1, {V_STRING}},
      {"$GLOBAL",global_func, 1, {V_NAME}},
      {"$PANEL", panel_func, 1, {V_NAME}}
    };

#define NUM_FUNCS sizeof(tcl_func)/sizeof(struct TCLFUNC)

/*	Globals		*/

/*	Error Messages	*/

    struct ERRMSG	er_nojob =
      {"Job '%s' not in job list.", "TAE-NOJOB", {EOS}};
    struct ERRMSG	er_nonex = 
	{"Reference to undefined variable '%s'.", "TAE-UNDEFVAR", {EOS}};
    struct ERRMSG	er_notstring = 
	{"Function argument is not a string.", "TAE-NOTSTR", {EOS}};
    struct ERRMSG		er_mixvar = 
    	{"Mixed data types.", "TAE-MIXVAR", {EOS}};
    struct ERRMSG	er_badar = 
    		{"Arithmetic operation yields overflow or underflow.",
    					"TAE-ARIERR", {EOS}};
    struct ERRMSG er_notname =
    		{"Function argument is not a variable name.",
    					"TAE-NOTNAME", {EOS}};

/* 	NOTE: all function routines have the same calling 
	sequence and are called from the expression evaluator.

	CAUTION: if you are writing a function that can return
	a null value (i.e., a v_count of zero a.k.a. "--"),
	then you must set numval.null appropriately.

	The expression evaluator sets numval.null FALSE before
	calling a function, so the default is non-null.
 */


/*
 * asfi_func - get the $SFI of an async job
 */
FUNCTION CODE asfi_func 
(
    struct VALUE	*strval,	/* in: value containing jobname */
    struct CONTXT	*context,	/* in: TM context		*/
    struct VALUE	*numval,	/* out: value containing the sfi*/
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct ACB	   *a; 	/* find job in acb list	*/
    TEXT		*jobname;

    if ((*strval).type != V_STRING)
    	{
    	*errmsg = &er_notstring;
    	return(FAIL);
    	}
    jobname = (*strval).uval.strpt;		/* jobname 		*/
    a = find_job (jobname);
    if (a == NULL)
	{
	*errmsg = &er_nojob;
    	s_copy(jobname, er_nojob.variable);
	return (FAIL);
	}
    inival(numval);				/* set some defaults	*/
    (*numval).type = V_INTEGER;
    (*numval).uval.intval = (*a).sfi;
    return (SUCCESS);
   }


/*
 * askey_func - get the $SKEY of an async job
 */
FUNCTION CODE askey_func 
(
    struct VALUE	*strval1,	/* in: value containing jobname */
    struct CONTXT	*context,	/* in: TM context		*/
    struct VALUE	*strval2,	/* out: value containing skey	*/
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    struct ACB	   *a; 	/* find job in acb list	*/
    TEXT		*jobname;

    if ((*strval1).type != V_STRING)
    	{
    	*errmsg = &er_notstring;
    	return(FAIL);
    	}
    jobname = (*strval1).uval.strpt;		/* jobname 		*/
    a = find_job (jobname);
    if (a == NULL)
	{
	*errmsg = &er_nojob;
    	s_copy(jobname, er_nojob.variable);
	return (FAIL);
	}
    inival(strval2);				/* set some defaults	*/
    (*strval2).type = V_STRING;
    (*strval2).uval.strpt = s_save((*a).skey);
    return (SUCCESS);
   }


/*
 * cnt_func - get the number of values in the named variable
 *
 * returns SUCCESS or FAIL
 */
FUNCTION CODE cnt_func
(
    struct VALUE	*nameval,	/* in: value containing the name	*/
    struct CONTXT	*context,	/* in: TM context			*/
    struct VALUE	*numval,	/* out: value containing the number	*/
    struct ERRMSG	*(*errmsg)	/* out: error message			*/

 )
    {
    TEXT		*name;
    struct VARIABLE	*varptr;

    /* fix crash on bad $COUNT argument	*/
    if ((*nameval).type != V_STRING && (*nameval).type != V_NAME)
        {
	*errmsg = &er_notname;
	return (FAIL);
	}
    name = (*nameval).uval.strpt;		/* name of variable	*/
    varptr = search(name, context);		/* find it		*/
    if (varptr==NULL)
    	{
    	*errmsg = &er_nonex;
    	s_copy(name, er_nonex.variable);
    	return(FAIL);
    	}
    inival(numval);				/* set some defaults	*/
    (*numval).uval.intval = (*varptr).v_count;	/* get the count	*/
    (*numval).type = V_INTEGER;
    return(SUCCESS);
    }

/*
 *	global_func - return 1 if variable exists, 0 otherwise
 *
 *	returns SUCCESS or FAIL
 */
FUNCTION CODE global_func
(
    struct VALUE	*nameval,	/* in: value containing the name*/
    struct CONTXT	*context,	/* in: TM context		*/
    struct VALUE	*numval,	/* out: value containing the number*/
    struct ERRMSG	*(*errmsg)	/* out: error message		*/

 )
    {
    IMPORT struct SYMTAB glbtab;
    TEXT		*name;
    struct VARIABLE	*varptr;

    /* fix crash on bad $COUNT argument	*/
    if ((*nameval).type != V_STRING && (*nameval).type != V_NAME)
        {
	*errmsg = &er_notname;
	return (FAIL);
	}
    inival(numval);				/* set some defaults	*/
    (*numval).type = V_INTEGER;
    name = (*nameval).uval.strpt;		/* name of variable	*/
    varptr = lookex (&glbtab, name);		/* find it as global	*/
    (*numval).uval.intval = (varptr == NULL) ? 0 : 1;
    return(SUCCESS);
    }

/*
 * fix_func - Convert a floating point to a fixed point 
 *
 * returns SUCCESS or FAIL
 */
    FUNCTION CODE fix_func(realval, context, intval, errmsg)

    struct VALUE	*realval;	/* in: value containing the real	*/
    struct CONTXT	*context;	/* in: TM context			*/
    struct VALUE	*intval;	/* out: value containing the integer	*/
    struct ERRMSG	*(*errmsg);	/* out: error message			*/

    {
    CODE		code;

    inival(intval);				/* set some defaults		*/
    if ((*realval).origin == O_CON || (*realval).type == V_REAL)  /* input really real? */
    	{
    	code = int_fl2i((*realval).uval.realval, &(*intval).uval.intval);
    	if (code != SUCCESS)
    	    {
    	    *errmsg = &er_badar;		/* conversion failure		*/
    	    return(FAIL);
    	    }
    	}
    else if ((*realval).type == V_INTEGER)	/* input was already fixed	*/
    	(*intval).uval.intval = (*realval).uval.intval;
    else
    	{
    	*errmsg = &er_mixvar;			/* not integer or real		*/
    	return(FAIL);
    	}
    (*intval).type = V_INTEGER;
    return(SUCCESS);
    }

/*
 * flt_func - Convert a fixed point to a floating point
 *
 * returns SUCCESS or FAIL
 */
    FUNCTION CODE flt_func(intval, context, realval, errmsg)

    struct VALUE	*intval;	/* in: value containing the integer	*/
    struct CONTXT	*context;	/* in: TM context			*/
    struct VALUE	*realval;	/* out: value containing the real	*/
    struct ERRMSG	*(*errmsg);	/* out: error message			*/

    {
    if ((*intval).origin == O_CON || (*intval).type==V_REAL)
	(*realval).uval.realval = (*intval).uval.realval;  /* was already real */
    else if ((*intval).type == V_INTEGER)	/* actually an integer?		*/
	(*realval).uval.realval = (*intval).uval.intval; /* convert		*/
    else
    	{
    	*errmsg = &er_mixvar;			/* not integer or real		*/
    	return(FAIL);
    	}
    inival(realval);				/* set some defaults		*/
    (*realval).type = V_REAL;
    return(SUCCESS);
    }

/*
 * inival - set some default value elements
 */
FUNCTION VOID inival
(
    struct VALUE	*value			/* in/out: the value	*/
 )
    {
    (*value).marked = FALSE;
    (*value).closed = FALSE;
    (*value).origin = O_COMP;			/* computed value	*/
    (*value).type = V_STRING;
    return;
    }

/*
 * fn_search - Find the descriptor for the specified TCL function
 *
 */
FUNCTION struct TCLFUNC *fn_search
(
    TEXT		name[]		/* in: name of the function to find	*/

 )
    {
    COUNT		i;

    for (i=0; i<NUM_FUNCS; i++)
    	if (s_equal(name, tcl_func[i].name))
    	    return(&tcl_func[i]);
    return(NULL);		/* couldn't find it */
    }

/*
 * strl_func - Get the length of a string variable
 *
 * returns SUCCESS or FAIL
 */
    FUNCTION CODE strl_func(strval, context, numval, errmsg)

    struct VALUE	*strval;	/* in: value containing the str val	*/
    struct CONTXT	*context;	/* in: TM context			*/
    struct VALUE	*numval;	/* out: value containing the length	*/
    struct ERRMSG	*(*errmsg);	/* out: error message			*/

    {
    if ((*strval).type != V_STRING)
    	{
    	*errmsg = &er_notstring;
    	return(FAIL);
    	}
    inival(numval);				/* set some defaults		*/
    (*numval).uval.intval = s_length((*strval).uval.strpt);
    (*numval).type = V_INTEGER;
    return(SUCCESS);
    }
