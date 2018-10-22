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



/* TLA CHECKOUT FILE_TIME=30-AUG-1984 15:15 DUA0:[TAEV1.TM]VALID.C;21 */
/* TLM CHECKOUT FILE_TIME= 4-MAY-1984 19:09 DUA0:[TAEV1.TM]VALID.C;20 */
/*TPAM        CHECKOUT FILE_TIME=21-AUG-1983 12:06 DUA0:[TAEV1.TM]VALID.C;16 */
/*TPAM        CHECKOUT FILE_TIME=11-JUL-1983 20:38 DUA0:[TAEV1.TM]VALID.C;15 */

/*
 *	VALID processing.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	21-aug-83	Nullables...palm
 *	29-aug-83	Keyword parameter support...palm
 *	29-sep-83	Make v_size = NAMESIZ if VALID exists so that so
 *			much tutor screen space won't be wasted...palm
 *	11-oct-83	Use VALIDSIZ for max valid string...palm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	29-aug-84	Add check for exact match for string value...lia
 *	25-jul-85	Delete obsolete  exact_valid()...dm
 *	26-jan-89	New POINTER_VALIDS logic; set v_size to be
 *			max lenght of all valids...palm
 *	12-may-92	PR1411: fix bug where not all string values are "valid"
 *			in trans_valid...krw
 *	21-sep-92	PR1516: change to 26-jan-89 fix, to only update v_size
 *			if current v_size is < max length of all valids...krw
 *	02-nov-92	PR1440, PR1717: if $SWITCH has SW_NO_VALIDATE set,
 *			then do not validate values with valids...krw
 *	24-nov-92	PR1761: Added same fix for PR1440, PR1717 to
 *			trans_valid...krw
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tminc.inc"	
#include "taeintproto.h"


GLOBAL struct VARIABLE *switch_gbl;    /* pointer to $SWITCH variable   */

FUNCTION static CODE keyword_check
(
    struct SYMTAB	*st,		/* in: symbol table		*/
    struct VARIABLE	*v,		/* in: VARIABLE containing key	*/
    TEXT		key[]		/* in: keyword to check		*/

 );


/*
 *	bld_valid.  Build x_VALID structure for a VARIABLE.
 */

FUNCTION CODE bld_valid
(
    struct SYMTAB	*st,		/* in: symbol table of var	*/
					/* (for keyword validation)	*/
    struct VARIABLE	*v,		/* in/out: VARIABLE structure 	*/
    TEXT		*value[],	/* in: ranges in string format	*/
    FUNINT		count		/* in: count of value vector	*/

 )
    {
    struct I_VALID	*ivalid;
    struct R_VALID	*rvalid;
    struct S_VALID	*svalid;
    COUNT		i;
    CODE		code;
    COUNT		length; 
    COUNT		maxLength;


    if ((*v).v_type == V_NAME)
        goto bad_valid;
    (*v).v_valid = allleg(v, count);		/* allocate VALID struct*/
    if ((*v).v_valid == NULL)
        goto memory;
    if ((*v).v_type == V_INTEGER)
	{
	ivalid = (struct I_VALID *) (*v).v_valid;
   	for (i=0; i < count; i++)
	    {
	    code = irange (value[i], &((*ivalid).range[i].low),
	    			           &((*ivalid).range[i].high));
	    if (code != SUCCESS) goto bad_range;
	    }
	}
    else if ((*v).v_type == V_REAL)
	{
	rvalid = (struct R_VALID *) (*v).v_valid;
	for (i=0; i < count; i++)
	    {
	    code = rrange (value[i], &((*rvalid).range[i].low),
	    			           &((*rvalid).range[i].high));
	    if (code != SUCCESS) goto bad_range;
	    }
	}
    else if ((*v).v_type == V_STRING)
	{
	svalid = (struct S_VALID *) (*v).v_valid;
	maxLength = 0;
	for (i=0; i < count; i++)
	    {
	    length = s_length (value[i]);
	    if (length > maxLength)
		maxLength = length;
	    if (length > VALIDSIZ)		/* limit on valid strings */
		goto bad_vstr;
#ifdef POINTER_VALIDS
	    (*svalid).slist[i].string = (TEXT *) tae_alloc (1, length+1);
#endif
	    s_copy (value[i], (*svalid).slist[i].string);
	    if (keyword_check(st, v, value[i]) != SUCCESS)
		return (FAIL);
	    }
	/* if current size is less than size of longest valid, then */
	/* set the current size to the max valid length */
	if ((*v).v_size < maxLength)
	    (*v).v_size = maxLength;
	}
    return (SUCCESS);

bad_valid:
    tmmsg(PROCFAIL,
          "VALID specification not allowed for NAME parameter '%s'.",
          "TAE-NOVALID", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    return(FAIL);

memory:
    overr();
    return(FAIL);

bad_range:
    tmmsg(PROCFAIL, "Incorrect range in VALID specification for '%s'.",
          "TAE-VALIDERR", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    return(FAIL);

bad_vstr:
    tmmsg(PROCFAIL, "VALID error for '%s'.  String greater than %d characters.",
	  "TAE-LONGVSTR", (uintptr_t) (*v).v_name, (uintptr_t) VALIDSIZ,
	  0, 0, 0);
    return (FAIL);

    }

/*	keyword_check.   Check that a keyword does not conflict with
 *	existing keywords in the symbol table.
 */

FUNCTION static CODE keyword_check
(
    struct SYMTAB	*st,		/* in: symbol table		*/
    struct VARIABLE	*v,		/* in: VARIABLE containing key	*/
    TEXT		key[]		/* in: keyword to check		*/

 )
    {
    struct VARIABLE	*cv;		/* current VARIABLE		*/
    struct S_VALID 	*valid;		/* ptr to valid structure	*/
    COUNT		i;

    if ((*v).v_class != V_PARM  ||  !(*v).v_keyword)
	return (SUCCESS);
    for (cv=(*st).link; cv != NULL; cv=(*cv).v_link)
        {
	if (cv == v)
	    continue;			/* don't self-check		*/
	if (!(*cv).v_keyword)
	    continue;			/* check only keyword types	*/
	valid = (struct S_VALID *) (*cv).v_valid;
        for (i=0; i < (*valid).count; i++)		
	    if (s_equal(key, (*valid).slist[i].string))
	        {
	        tmmsg(PROCFAIL,
	    	      "Duplicate keyword for parameters '%s' and '%s'.",
	    	      "TAE-KEYCONFLICT", (uintptr_t) (*cv).v_name, 
		      (uintptr_t) (*v).v_name, 0, 0, 0);
	        return (FAIL);
	        }
        }
    return (SUCCESS);
    }	

/*
 *	chk_valid.   Verify that a proposed value is compatible with VALID.
 *	Caller must do error message.
 *
 *	Return codes:
 *		SUCCESS -- valid
 *		FAIL	-- not valid
 *		AMBIG   -- found but ambiguous (strings only)
 *
 */

FUNCTION CODE chk_valid
(
    struct VARIABLE	*v,		/* VARIABLE to check		*/
    GENPTR		value,		/* genptr to value vector	*/
    FUNINT		count		/* count of value		*/

 )
    {
    COUNT		i;
    COUNT		j;
    struct I_VALID	*ivalid;
    struct R_VALID	*rvalid;
    struct S_VALID	*svalid;
    TAEINT		*ival;		/* integer value vector ptr	*/
    TAEFLOAT		*rval;		/* real value vector ptr	*/
    TEXT		**sval;		/* string value vector ptr	*/
    CODE		code;
    COUNT		matches;

    if (switch_gbl && 
	(IVAL(*switch_gbl, 0) & SW_NO_VALIDATE)) /* user requested to turn*/
	return (SUCCESS);		/* off validation of values     */

    if ((*v).v_valid == NULL) return (SUCCESS);
    if ((*v).v_type == V_INTEGER)
	{
	ivalid = (struct I_VALID *) (*v).v_valid;
	ival = (TAEINT *) value;	
	for (i=0; i < count; i++)	/* for each value		*/
	    {
	    code = FAIL;		/* assume no range found	*/
	    for (j=0; j < (*ivalid).count; j++)
	        if (ival[i]  >=  (*ivalid).range[j].low   &&
	             ival[i]  <=  (*ivalid).range[j].high)
		    {
		    code = SUCCESS;
		    break;
		    }
	    if (code == FAIL) return (FAIL);
	    }
        }
    else if ((*v).v_type == V_REAL)
	{
	rvalid = (struct R_VALID *) (*v).v_valid;
	rval = (TAEFLOAT *) value;
	for (i=0; i < count; i++)	/* for each value		*/
	    {
	    code = FAIL;		/* assume no range found	*/
	    for (j=0; j < (*rvalid).count; j++)
	        if (rval[i]  >=  (*rvalid).range[j].low  &&
	            rval[i]  <=  (*rvalid).range[j].high)
		    {
		    code = SUCCESS;
		    break;
	  	    }
	    if (code == FAIL) return (FAIL);
	    }
        }
    else if ((*v).v_type == V_STRING)
	{
	svalid = (struct S_VALID *) (*v).v_valid;
	sval = (TEXT **) value;
	for (i=0; i < count; i++)	/* look for left substring	*/
	    {
	    matches = 0;		/* number of matches		*/
	    for (j=0; j < (*svalid).count; j++)
	        {
	        if (s_equal(sval[i], (*svalid).slist[j].string))
		    {
		    matches = 1;
		    break;
		    }
	        if(s_lseq(sval[i], (*svalid).slist[j].string))
		    matches++;
		}
	    if (matches == 0)
		return(FAIL);		/* not found 		*/
	    if (matches > 1)
		return (AMBIG);		/* ambiguous		*/
	    }
        }
    return (SUCCESS);
    }

/*
 *	trans_valid.	Transform string values to VALID form.
 *
 *	Each string in a string value vector is converted to the
 *	corresponding exact match VALID component.
 *	We assume that the value vector has already been declared
 *	valid by chk_valid.
 *	
 */

FUNCTION CODE trans_valid
(
    struct VARIABLE	*v		/* VARIABLE to transform	*/

 )
    {
    COUNT	i;
    COUNT	j;
    COUNT	item;
    struct S_VALID  *svalid;
    TEXT	*s;

    if (switch_gbl && 
	(IVAL(*switch_gbl, 0) & SW_NO_VALIDATE)) /* user requested to turn*/
	return (SUCCESS);		/* off validation of values     */

    if ((*v).v_type != V_STRING) return(SUCCESS);
    svalid = (struct S_VALID *) (*v).v_valid;
    if (svalid == NULL) return(SUCCESS);
    item = -1;
    for (i=0; i < (*v).v_count; i++)		/* find each in S_VALID	*/
	{
	for (j=0; j < (*svalid).count; j++)
	    {
	    if (s_equal(SVAL(*v, i), (*svalid).slist[j].string))
		{
		item = j;
		break;
		}
	    if (s_lseq(SVAL(*v, i), (*svalid).slist[j].string))
		item = j;
	    }
	if (item >= 0)
	    {
	    s = s_save((*svalid).slist[item].string);
	    if (s == NULL) return (FAIL);
	    s_free(SVAL(*v, i));		/* free old string	*/
	    SVAL(*v, i) = s;			/* ptr to transf'd str	*/
	    item = -1;				/* reset for next string */
	    }		/* note: if string not found, take it as it is  */
	}
    return(SUCCESS);
    }

/*	key_trans.    Translate a keyword into the associated variable
 *	structure.
 *
 *	Return codes:
 *		SUCCESS -- found
 *		FAIL -- not found
 *		AMBIG -- ambiguous
 */

FUNCTION CODE key_trans
(
    struct SYMTAB	*parmst,	/* in: parameter symbol table	*/
    TEXT		key[],		/* in: keyword value		*/
    struct VARIABLE	**v		/* out: VARIABLE pointer	*/

 )
    {
    struct S_VALID  *valid;
    struct VARIABLE *cv;
    COUNT	    matches;
    COUNT	    i;

    matches = 0;
    for (cv=(*parmst).link; cv != NULL; cv=(*cv).v_link)
        {
	if (!(*cv).v_keyword)
	    continue;			/* only examine keyword parms	*/
	valid = (struct S_VALID *) (*cv).v_valid;
        if (valid == NULL)
	    continue;			/* safety check			*/
	for (i=0; i < (*valid).count; i++)	
	    {
	    if (s_equal(key, (*valid).slist[i].string))
	        {
	        *v = cv;
	        return (SUCCESS);
	        }
	    if (s_lseq(key, (*valid).slist[i].string))
	        {
	        *v = cv;
	        matches++;
	    	}
	    }
 	}
    if (matches == 0)
	return (FAIL);
    if (matches == 1)
	return (SUCCESS);
    return (AMBIG);
    }		
