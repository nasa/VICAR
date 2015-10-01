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



/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:49 DUA1:[TAEV2.TM]SETCHECK.C;3 */
/* TDB CHECKOUT FILE_TIME=29-JUL-1985 15:40 DUA1:[TAEV1.TM]SETCHECK.C;2 */
/* TDM CHECKOUT FILE_TIME=18-SEP-1984 14:47 DUA0:[TAEV1.TM]SETCHECK.C;19 */
/* TLM CHECKOUT FILE_TIME= 4-MAY-1984 18:01 DUA0:[TAEV1.TM]SETCHECK.C;18 */
/* TCS CHECKOUT FILE_TIME=21-NOV-1983 16:17 DUA0:[TAEV1.TM]SETCHECK.C;15 */
/*TPAM        CHECKOUT FILE_TIME= 4-AUG-1983 09:53 DUA0:[TAEV1.TM]SETCHECK.C;12 */
/*TDM         CHECKOUT FILE_TIME=11-JUL-1983 20:03 DUA0:[TAEV1.TM]SETCHECK.C;7 */

/*
 *	Value checking and setting functions.
 *
 *	CAUTION: these functions are intended for manipulating values.
 *	They may not be used for setting a NAME parameter to point
 *	to another variable.  (Note that RESOLVE is done for every v.)
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	04-aug-83	Added trs_value() ...dm
 *	20-aug-83	Nullables...palm
 *	10-oct-83	Fix unix lint errors...palm
 *	21-nov-83	fix set_componenent for setting string for
 *			one greater index...palm
 *	27-apr-84	add set_string function...ces
 *	04-may-84	VALUE_x to xVAL ... ces
 *	17-sep-84	add error code V_BADAMBIG to chk_vector &
 *			chk_component...lia
 *	25-jul-85	Fix UNIX lint compilation errors...dm
 *	03-oct-85	PR 946: 'chk_vector' bypasses file verification call
 *			to 'file_ins' if compiling a proc...dab
 *	17-aug-87	As safety (and to fix bug), always null a string
 *			value vector entry after freeing to prevent
 *			future accidental free...palm
 *      24-nov-92       PR1761: Added same fix for PR1440, PR1717 (in valid.c)
 *			to set_check...krw
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"tminc.inc"	/* TAE specific structs and defs	*/
#include	"symtab.inc"	/* TM symbol table			*/
#include "taeintproto.h"

GLOBAL struct VARIABLE *switch_gbl;    /* pointer to $SWITCH variable   */


/*
 *	chk_vector.  Verify that a value vector is compatible with a
 *	VARIABLE structure.
 *
 *	Return codes:
 *		SUCCESS -- ok
 *		V_BADTYPE -- type mismatch
 *		V_BADCOUNT -- count too large or too small
 *		V_BADSIZE  -- a string is larger than v_size
 *		V_BADVALID -- value not compatible with VALID list
 *		V_BADFILE  -- FILEMODE verification failure
 *		V_BADAMBIG -- value is ambiguous
 */

FUNCTION CODE chk_vector
(
    struct VARIABLE	*v,	/* in: VARIABLE to check against	*/
    FUNINT		type,	/* in: type of value			*/
    GENPTR		value,	/* in: pointer to value vector to check	*/
    FUNINT		count,	/* in: count of value vector		*/
    BOOL		compiling  /* in: compiling a proc ? */

 )
    {
    COUNT		i;
    CODE		code;
    TEXT		**svalue;
    TEXT		msgkey[STRINGSIZ+1];
    TEXT		msg[STRINGSIZ+1];

    v = RESOLVE(v);			/* resolve indirection		*/
    if (type != (*v).v_type)
	return(V_BADTYPE);
    if (count == -1)			/* setting to "no value" is ok	*/
	;
    else if (count == 0)		/* must have v_nullable for null*/
	{
	if (!(*v).v_nullable)
	    return (V_BADCOUNT);
	}
    else
	if (count < (*v).v_minc  || count > (*v).v_maxc)
	    return (V_BADCOUNT);
    if (type == V_STRING)
	{				/* check string sizes		*/
        if (!(switch_gbl &&
             (IVAL(*switch_gbl, 0) & SW_NO_VALIDATE))) 
					/* user requested to turn*/
        				/* off validation of values     */
	    {
	    svalue = (TEXT **) value;
	    for (i=0; i < count; i++)
	        if (s_length(svalue[i]) > (*v).v_size)
		    return (V_BADSIZE);
	    }
	}
    code = chk_valid(v, value, count);
    if (code == AMBIG)
	return(V_BADAMBIG);
    else if (code != SUCCESS)
	return(V_BADVALID);

    /* TBD: the nicely formatted message from file_ins is not used	*/
    if (!compiling)		/* if compiling, check file info at run-time */
	if (file_ins(v, value, count, msgkey, msg) != SUCCESS)
	    return(V_BADFILE);

    return (SUCCESS);
    }

/*
 *	chk_component.  Verify that a index value is compatible with a
 *	VARIABLE structure.
 *
 *	Return codes:
 *		SUCCESS -- ok
 *		V_BADTYPE -- type mismatch
 *		V_BADSIZE  -- a string is larger than v_size
 *		V_BADVALID -- value not compatible with VALID list
 *		V_BADFILE  -- FILEMODE verification failure
 *		V_BADAMBIG -- value ambiguous
 */

FUNCTION CODE chk_component
(
    struct VARIABLE	*v,	/* in: VARIABLE to check against	*/
    FUNINT		type,	/* in: type of value			*/
    GENPTR		value	/* in: pointer to value vector to check	*/

 )
    {
    CODE		code;
    TEXT		**svalue;
    TEXT		msg[STRINGSIZ+1];
    TEXT		msgkey[STRINGSIZ+1];

    v = RESOLVE(v);			/* resolve indirection		*/
    if (type != (*v).v_type)
	return(V_BADTYPE);
    if (type == V_STRING)
	{				/* check string sizes		*/
        if (!(switch_gbl &&
             (IVAL(*switch_gbl, 0) & SW_NO_VALIDATE))) 
					/* user requested to turn*/
        				/* off validation of values     */
	    {
	    svalue = (TEXT **) value;
	    if (s_length (svalue[0]) > (*v).v_size)
	        return (V_BADSIZE);
	    }
	}
    code = chk_valid(v, value, 1);
    if (code == AMBIG)
	return(V_BADAMBIG);
    else if (code != SUCCESS)
	return(V_BADVALID);

    /* TBD: the nicely formatted message from file_ins is not used	*/
    if (file_ins(v, value, 1, msgkey, msg) != SUCCESS)
	return(V_BADFILE);	

    return (SUCCESS);
    }

/*
 *	fill_value.  Make a VARIABLE structure have a v_count of
 *	"count" by filling "null" values between the current
 *	v_count and "count".  A "null" value is zero for numerics
 *	and  a null string for V_STRING variables.
 *
 *	Return codes:
 *		SUCCESS
 *		FAIL -- not enough dynamic memory to create null strings
 */

FUNCTION CODE fill_value
(
    struct VARIABLE	*v,	/* in/out: VARIABLE structure	*/
    FUNINT		count	/* in: new value count		*/

 )
    {
    COUNT	i,j;
    TEXT	*sptr;

    v = RESOLVE(v);			/* resolve indirection	*/
    count = min((*v).v_maxc, count);	/* safety catch		*/
    i = ((*v).v_count < 0) ? 0 : (*v).v_count;
    for ( ; i < count; i++)
	{
        if ((*v).v_type == V_INTEGER)
    	    IVAL(*v,i) = 0;
        else if ((*v).v_type == V_REAL)
    	    RVAL(*v,i) = 0;
        else if ((*v).v_type == V_STRING)
	    {
	    sptr = s_save("");		/* make dynamic null string	*/
	    if (sptr == NULL)		/* clean up previous doings	*/
		{
		j = ((*v).v_count < 0) ? 0 : (*v).v_count;
		for ( ; j < i; j++)
		   s_free(SVAL(*v,j));
		return(FAIL);
		}
    	    SVAL(*v,i) = sptr;
	    }
         }
    (*v).v_count = count;
    return(SUCCESS);
    }	

/*
 *	set_component.  Move a value to a VARIABLE.
 *	CAUTION: we assume the value has been blessed by chk_component.
 *
 *	If string type VARIABLE, the strings pointed to by the caller's value
 *	vector are copied into dynamic memory.  The caller may de-allocate
 *	the strings pointed to by his value vector without disturbing
 *	the VARIABLE's integrity.
 *
 *	Return codes:
 *		SUCCESS -- ok
 *		FAIL    -- no memory for string copy
 *
 */

FUNCTION CODE set_component
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		index		/* in: TCL index (value of 1 to  count)	*/

 )
    {
    TEXT	*sptr[MAXVAL];		/* string pointer vector	*/
    TEXT	**svalue;		/* casted value vector		*/
    TAEFLOAT	*fltptr;
    TAEINT	*intptr;

    v = RESOLVE(v);			/* resolve indirection		*/
    if ((*v).v_type == V_REAL)
	{
	fltptr = (TAEFLOAT *) value;
	RVAL (*v, index -1) = *fltptr;
	}
    else if ((*v).v_type == V_INTEGER)
	{
	intptr = (TAEINT *) value;
	IVAL (*v, index-1) = *intptr;
	}
    else
	{
	/* strategy for strings: build a local value vector (sptr).  Once
	   it is successfully built, then move it in.  This way if we run
	   out of memory, we haven't destroyed v's current value.
        */

	svalue = (TEXT **) value;	
	sptr[0] = s_save(svalue[0]);
	if (sptr[0] == NULL)		/* not enough memory to store	*/
	    return (FAIL);		

	if (index <= (*v).v_count)		/* if component exists	*/
            s_free(SVAL(*v, index-1));	/* free existing string	*/
	SVAL (*v, index-1) = sptr[0];
	if (trans_valid(v) != SUCCESS)	/* translate to VALID form	*/
	    return(FAIL);
    	}
    return (SUCCESS);
    }

/*	set_string - set string value of a variable.
 *	The string is assumed to be valid and the variable
 *	to be singly valued.  This function does not police the
 *	string.
 *	Note: The value, i.e., string is TEXT, not a pointer to a
 *	value vector.
 */

FUNCTION CODE set_string 
(
    struct VARIABLE	*v,		/* in/out:  variable to set		*/
    TEXT		string[]	/* in:  value to set variable to		*/

 )
    {

    CODE		code;

    code = set_value(v, (GENPTR)&string, 1);		/* set variable value */
    return(code);
    }

/*
 *	set_value.  Move a value to a VARIABLE.
 *	CAUTION: we assume the value vector has been blessed by chk_vector.
 *
 *	If string type VARIABLE, the strings pointed to by the caller's value
 *	vector are copied into dynamic memory.  The caller may de-allocate
 *	the strings pointed to by his value vector without disturbing
 *	the VARIABLE's integrity.
 *
 *	A count of -1 is acceptable:  this means give variable "no value"
 *
 *	Return codes:
 *		SUCCESS -- ok
 *		FAIL    -- no memory for string copy
 *
 */

FUNCTION CODE set_value
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		count		/* in: vector count		*/

 )
    {
    TEXT	*sptr[MAXVAL];		/* string pointer vector	*/
    COUNT	i,j;		
    TEXT	**svalue;		/* casted value vector		*/

    v = RESOLVE (v);			/* resolve indirection		*/
    if ((*v).v_type == V_REAL)
	{
	(*v).v_count = count;
        if (count > 0)
	    bytmov(value, (*v).v_cvp, count * sizeof(TAEFLOAT));
	}
    else if ((*v).v_type == V_INTEGER)
	{
	(*v).v_count = count;
        if (count > 0)
	    bytmov(value, (*v).v_cvp, count * sizeof(TAEINT));
	}
    else
	{
	/* strategy for strings: build a local value vector (sptr).  Once
	   it is successfully built, then move it in.  This way if we run
	   out of memory, we haven't destroyed v's current value.
        */

	svalue = (TEXT **) value;	
	for (i=0; i < count; i++)
	    {
	    sptr[i] = s_save(svalue[i]);
	    if (sptr[i] == NULL)	
		{			/* not enough memory to store	*/
		for (j=0; j < i; j++)	/* free everything so far	*/
		    s_free(sptr[j]);
		return (FAIL);		
		}
	    }
	for (i=0; i < (*v).v_count; i++)
	    {
	    s_free(SVAL(*v, i));	/* free existing strings	*/
	    SVAL(*v, i) = NULL;		/* flag that it's freed         */
	    }
        if (count > 0)
	    bytmov((GENPTR) sptr, (*v).v_cvp, count * sizeof(TEXT *));
	(*v).v_count = count;
	if (trans_valid(v) != SUCCESS)	/* translate to VALID form	*/
	    return(FAIL);
    	}
    return (SUCCESS);
    }

/*
 * 	trs_value.Transfer input string value vector to current value vector.
 *
 * 	It assumes that the input string values are already dynamically
 *	allocated, and are not going to be deallocated by the caller.
 *
 *	return codes:   SUCCESS --ok
 *			FAIL  -- not a string parameter
 */

FUNCTION CODE trs_value
(
    struct VARIABLE	*v,		/* structure to be updated	*/
    GENPTR		value,		/* in: pointer to value vector	*/
    FUNINT		count		/* in: vector count		*/

 )
    {
    COUNT		i;

    v = RESOLVE (v);			/* resolve indirection		*/
    if ((*v).v_type != V_STRING)
	return (FAIL);			/* wrong type			*/
    else
	{
	for (i=0; i < (*v).v_count; i++)
	    {
	    s_free(SVAL(*v, i));	/* free existing strings	*/
	    SVAL(*v, i) = NULL;		/* flag that it's been freed    */
	    }
        if (count > 0)
	    bytmov(value, (*v).v_cvp, count * sizeof(TEXT *));
					/* copy input string addresses	*/
	(*v).v_count = count;
	if (trans_valid(v) != SUCCESS)	/* translate to VALID form	*/
	    return(FAIL);
	}
    return (SUCCESS);
    }
