/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 * Package to convert numeric variables from string format to other formats
 * and vice versa.
 *
 *	CHANGE LOG:
 *
 *	28-mar-84	Fix reserved operand fault in s_s2r...palm
 *	15-may-84	Deall string on "+hello" in s_s2i & r...nhe
 *	10-sep-84	Add logic to check for valid real in s_s2r...lia
 *	26-jun-85	Avoid sscanf calls (bugs in 2.0 library) and
 *			better performance anyway...palm
 *	10-sep-86	PR 1111: Trap integer overflow in 's-s2i1'. Created
 *			's_s2r1' for analogous floating point values...dab
 *	08-may-87	Remove FAST declaration of 'accumulator' 
 *			in s_s2i1, s_s2r1 because of & usage...ljn
 *	01-feb-88	Changed %g to %.12g in s_r2s...ljn
 *	11-sep-89	fix s_s2i for smallest number...palm 
 *	01-jul-91	caste SMALLEST_TAEINT to TAEINT (apollo)...ljn
 *	01-sep-92	PR1627: in s_r2s, don't add ".0" if 
 *			scientific notation...crb
 */



#include	"taeconf.inp"
#include	"expinc.inc"		/* expression evaluation support	*/
#include	"symtab.inc"		/* symbol table definitions		*/
#include	"syninc.inc"		/* syntax support			*/
#include "taeintproto.h"

    

/* s_d2s - convert a delimited packed string to a C language string.
 * The delimited packed string is an unterminated sequence of
 * consecutive characters.  The 1st character is the opening delimiter.
 * This character appearing again in the sequence is the closing delimiter.
 * The delimiters themselves are not moved into the output string.
 */

    FUNCTION VOID s_d2s (

    GENPTR		d,		/* in:  the delimited packed characters	*/
    TEXT		*s		/* out: the converted standard C string	*/
    )

    {
    char		delim;		/* the delimiting character		*/
    TEXT		*c;

    c     = (TEXT *) d;
    delim = *c++;
    while (*c != delim)
	*s++ = *c++;
    *s = EOS;
    }

/*
 *   s_i2s.  Convert binary integer to string form.
 */

    FUNCTION CODE s_i2s(

    TAEINT i,
    TEXT *s
    )

    {
    sprintf(s, "%d", i);
    return(SUCCESS);
    }

/*
 *    s_r2s.  Convert real to string format.
 *	      Use c language g format.
 */

    FUNCTION CODE s_r2s(

    TAEFLOAT 	r,
    TEXT 	*s
    )

    {
    BOOL	scientific;
    
    if (r == 0.0)
	s_copy("0.0", s);	/* because sprintf handles 0 strangely */
    else
	sprintf(s, "%.12g", r);
    scientific = s_index(s, 'e') >= 0;
    if (s_index(s, '.') < 0 && !scientific)
	{
	/* no decimal point in resulting val and not sci notation */
	s_append(".0", s);
	}
    return (SUCCESS);
    }

/*
 *   s_s2i.  Convert decimal string to binary integer.
 *
 */

    FUNCTION CODE s_s2i(

    TEXT	*s,
    TAEINT	*i
    )

    {
    CODE		code;
    struct VALUE	value;
    struct SYNBLK	sb;
    BOOL		negative;

    negative = FALSE;
    initok(&sb, s);			/* setup a syntax block		*/
    code = getprim(&sb, &value);	/* get primitive value		*/
    if (code != PA_NUM) {	/* check for string to deall	*/
	if (code == '-') {
	  negative = TRUE;
	  code = getprim(&sb, &value);
	} else {
	  if (code == '+') {
	    code = getprim(&sb, &value);
	  } else {
	    deal_pval(&value, code);		/* deallocate any string */
	    return(FAIL);
	  }
	}
    }
    if (code != PA_NUM || value.type != V_INTEGER)
    	{
	deal_pval(&value, code);		/* deallocate any string */
    	return(FAIL);
    	}
    if (negative)
	{
	if (-value.uval.realval < (TAEINT)SMALLEST_TAEINT)
	    return (FAIL);
	*i = -value.uval.realval;
	}
    else
	{
	if (value.uval.realval > LARGEST_TAEINT)
	    return (FAIL);
	*i = value.uval.realval;
	}
    if (getprim(&sb, &value) != EOS) return(FAIL);	/* something more, error 	*/
    return(SUCCESS);
    }

/* 
 * s_s2i1	convert a simple integer
 *	
 * This routine is required because getprim still needs to call it.
 * An infinite recursion loop would result if s_s2i was called.
 * Routine is necessary to assist getprim "action" routines which
 * compute integer and real values by parsing each part (integer,
 * fraction, exponent) of number and computing resulting value.
 */
    FUNCTION CODE s_s2i1(

    TEXT	*s,
    TAEINT	*i
    )

    {
    TAEINT	accumulator;
    CODE	code;

    accumulator = 0;
    for (; *s >= '0' && *s <= '9';   s++)
	{
	code = int_mult (accumulator, (TAEINT) 10, &accumulator);
	if (code != SUCCESS) return(PA_ERRFLOW);
	code = int_add (accumulator, (TAEINT) (*s - '0'), &accumulator);
	if (code != SUCCESS) return(PA_ERRFLOW);
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
 *	s_r2ws.  Convert TAEFLOAT to string.
 */

    FUNCTION CODE s_r2ws(

    TAEFLOAT	taefloat,	/* input real			*/
    TEXT	string[]	/* output string		*/
    )

    {
    sprintf(string, "%.12e", taefloat);
    return (SUCCESS);
    }

/*
 *    s_s2r.  Convert decimal string to TAEFLOAT format.
 */

    FUNCTION CODE s_s2r(

    TEXT	*s,
    TAEFLOAT	*r		/* assume TAEFLOAT is DOUBLE  */
    )

    {
    CODE		code;
    struct VALUE	value;
    struct SYNBLK	sb;
    BOOL		negative;		/* true if number is negative	*/

    negative = FALSE;				/* assume positive number	*/
    initok(&sb, s);				/* setup a syntax block		*/
    value.uval.realval = 0.0;			/* avoid reserved operand 	*/
    code = getprim(&sb, &value);		/* get primitive value		*/
    if (code != PA_NUM)	{			/* check for an allocated string*/
      if (code == '-') {
	negative = TRUE;
	code = getprim(&sb, &value);
      } else { 
	if (code == '+')
	  code = getprim(&sb, &value);
    	else {
	  deal_pval(&value, code);	/* deallocate any string	*/
	  return(FAIL);
	}
      }
    }
    if (code != PA_NUM || (value.type != V_REAL && value.type != V_INTEGER))
    	{
	deal_pval(&value, code);		/* deallocate any string */
    	return(FAIL);
    	}
    *r = value.uval.realval;			/* use implicit conversion	*/
    if (negative) *r = -*r;
    if (getprim(&sb, &value) != EOS) return(FAIL);	/* something more, error 	*/
    return(SUCCESS);
    }

/* 
 * s_s2r1	convert a simple real
 *	
 * This routine is required because getprim still needs to call it.
 * An infinite recursion loop would result if s_s2r was called.
 * Routine is necessary to assist getprim "action" routines which
 * compute integer and real values by parsing each part (integer,
 * fraction, exponent) of number and computing resulting value.
 */
    FUNCTION CODE s_s2r1(

    TEXT	*s,
    TAEFLOAT	*x
    )

    {
    TAEFLOAT	accumulator;
    CODE	code;

    accumulator = 0.0;
    for (; *s >= '0' && *s <= '9';   s++)
	{
	code = fl_mult (accumulator, (TAEFLOAT) 10.0, &accumulator);
	if (code != SUCCESS) return(PA_ERRFLOW);
	code = fl_add (accumulator, (TAEFLOAT) (*s - '0'), &accumulator);
	if (code != SUCCESS) return(PA_ERRFLOW);
	}
    if (*s != EOS)
	{
	*x = 0.0;
	return (FAIL);			/* bad character found	*/
	}
    *x = accumulator;
    return (SUCCESS);    
    }
