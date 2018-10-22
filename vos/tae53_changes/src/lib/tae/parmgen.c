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



/* TPAM CHECKOUT FILE_TIME=15-JUL-1987 16:34 DUA1:[TAEV2.OLB.GENLIB]PARMGEN.C;2 */
/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:45 DUA1:[TAEV2.OLB.GENLIB]PARMGEN.C;1 */
/* TPEB CHECKOUT FILE_TIME=20-NOV-1984 19:43 DUA0:[TAEV1.OLB]PARMGEN.C;10 */
/* TPEB CHECKOUT FILE_TIME= 4-MAY-1984 19:55 DUA0:[TAEV1.OLB]PARMGEN.C;9 */
/* TDM CHECKOUT FILE_TIME=11-OCT-1983 17:35 DUA0:[TAEV1.OLB]PARMGEN.C;5 */
/* Written in C, functions in this package should be called (probably via
 * bridge routines) by application programs to retrieve parameters from
 * a parameter block initialized as discussed below.
 *
 * A caller supplied parameter block is initialized either with
 *
 *	p_inim - receives parameters from the terminal monitor (TM), or
 *	p_rdb - reads parameters from a specially formatted disk file.
 *
 * Note that the above two functions are in other source files.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Deleted special conversion of v_filemode...palm
 *	11-oct-83	Unix compilation errors...palm
 *	24-oct-83	New p_ function calling sequences...dm
 *	19-mar-84	New p_find...palm
 *	03-may-84	Move IMPORT into functions...lim
 *	04-may-84	VALUE_x to xVAL ... ces
 *	16-nov-84	TCL 67: Handle requests for parm qualifiers...peb
 *	27-nov-84	TCL 67: p_var_parse avail to qparmgen...peb
 *	27-nov-84	TCL 67: p_var_parse to taeutil.c (w/ name change)...peb
 *	14-jul-87	Allow qualifiers for locals...palm
 *	04-aug-87	Allow infinite levels of qualification.  New
 *			p_lookup function.  New p_get_*...palm
 *      22-mar-91       smarter p_attr pr905...tpl
 */

#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include "syninc.inc"
#include "taeintproto.h"

FUNCTION struct VARIABLE *p_fvar
(
 struct PARBLK	*block,		/* in:  parameter block		*/
 TEXT		name[]		/* in:  parameter name		*/
 );
FUNCTION struct VARIABLE *p_lookup 
(
 struct SYMTAB *symtab,		/* in: symbol table to search */
 TEXT	      name[]		/* in: name of variable	      */
 );
/*      fast, case insensitive string compare...  */

extern TEXT s_table[];                  /* upper case conversion table */

#ifndef STR_EQUAL
#define STR_EQUAL(s1, s2) \
  (s_table[(int) *(s1)] == s_table[(int) *(s2)] ? s_equal(s1, s2) : FALSE)
#endif


/* p_attr - get attributes for a parameter.
 * The value(s) of the parameter is retrieved via the other p_ calls.
 * Return codes:
 *
 *	SUCCESS
 *	P_BADNAME	if the parameter name was bad.
 */

FUNCTION CODE p_attr
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  name of the parameter		*/
 CODE		*type,		/* out: parameter  type			*/
 COUNT		*n,		/* out: number of values for the parm	*/
 BOOL		*dflt,		/* out: TRUE if parm was defaulted	*/
 CODE		*access 	/* out: access mode if a file		*/
				/* P_IN, P_OUT, P_INOUT, P_NONE		*/
 )

    {
    IMPORT TEXT		pm_name[], pk_name[];
    struct VARIABLE	*v;		/* variable in parm block		*/

    if ((v = p_fvar(block, name)) == NULL) goto p__bnerr;	/* point to parm*/
    *dflt = (*v).v_default;		/* default in var is already TRUE/FALSE	*/
    *n 	  = (*v).v_count;
    if ( (*v).v_nullable && *n==-1 )*n=0;

    *type = (*v).v_file ? P_FILE : (*v).v_type; 	/* file or other types */
    *access = (*v).v_file ? (*v).v_filemode : P_NONE;	/* none if not file    */
    return(SUCCESS);

p__bnerr:
    x_error((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return(P_BADNAME);
    }

/*
 *	p_get_root.
 *
 *	Parse the caller-specified, qualified TCL variable name,
 *	extracting the simple root name.  For example, if
 *	name is "X.Y.Z", then root becomes "X" and remainder
 *	becomes "Y.Z".
 *
 *	This logic avoids s_ function calls for efficiency.
 */

FUNCTION VOID p_get_root
(
 TEXT	name[],			/* in:  (qualified) var name	*/
 TEXT	root[NAMESIZ+1],	/* out: first component name	*/
 TEXT	remainder[STRINGSIZ+1]	/* out: remainder of name 	*/
 )
    {

    while (*name != EOS   &&   *name != '.')	/* find 1st period   */
        *root++ = *name++;			/* and copy	     */
    *root = EOS;				/* terminate 	     */
    if (*name == '.')				/* if qualified ?    */
        {
	name++;					/* move past period  */
	while (*name != EOS)
	    *remainder++ = *name++;		/* and copy remainder*/
        }
    *remainder = EOS;				/* terminate remainder*/
    return;
    }

/*	p_get_leaf.  
 *
 *	Parse a (possibly qualified) TCL variable name
 *	and extract the simple leaf name.  For example, if
 *	name is "X.Y.Z", then leaf becomes "Z" and
 *	remainder becomes "X.Y".
 *
 */

FUNCTION VOID p_get_leaf
(
 TEXT	name[],			/* in: full qualified name 	*/
 TEXT	leaf[NAMESIZ+1],	/* out:	simple right-hand name	*/
 TEXT	remainder[STRINGSIZ+1]	/* out: left-hand portion	*/
 )
    {
    FAST TEXT	*s;			/* general text pointer		*/

    for (s = s_length(name) + name - 1;  s >= name;  s--)
        if (*s == '.')					
    	    break;			/* find last period		*/
    s_bcopy (s+1, leaf, NAMESIZ);	
    while (name < s)			/* copy rest till last period	*/
	*remainder++ = *name++;		
    *remainder = EOS;
    }

/*	p_fvar - find variable in parameter block.  Return VARIABLE pointer.
 */

FUNCTION struct VARIABLE *p_fvar
(
 struct PARBLK	*block,		/* in:  parameter block		*/
 TEXT		name[]		/* in:  parameter name		*/
 )
    {
    struct VARIABLE	*v;		/* a variable in parm block	*/

    v = p_lookup (&(*block).symtab, name);    
    return (v);
    }

/*	p_find.    Returns variable pointer 
 *		   and aborts if no such variable.
 */

struct VARIABLE  *p_find
(
 struct PARBLK	*block,		/* in: parblk			*/
 TEXT		name[]		/* in: name of parameter	*/
 )
    {
    IMPORT TEXT		pm_name[], pk_name[];
    struct VARIABLE	*v;

    v = p_fvar (block, name);
    if (v == NULL)
      x_error ((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return (v);
    }

/*	p_lookup.    Find variable in symbol table.
 *
 *	Infinite levels of qualifiers are handled.
 */

FUNCTION struct VARIABLE *p_lookup 
(
 struct SYMTAB *symtab,		/* in: symbol table to search */
 TEXT	      name[]		/* in: name of variable	      */
 )
    {
    struct VARIABLE *v;
    TEXT root_name[STRINGSIZ+1];		/* simple root variable name  */
    TEXT rem_name[STRINGSIZ+1];			/* remainder of var name      */

    p_get_root (name, root_name, rem_name); 	/* crack qual'd name	*/
    for (v=(*symtab).link; v != NULL; v=(*v).v_link)
	if (STR_EQUAL((*v).v_name, root_name))
	    break;
    if (v == NULL)
	return(NULL);			/* not found			*/
    if (NULLSTR(rem_name))		/* did name have qualifier ?	*/
	return(v);			/* if not, return the var	*/
    if (!(*v).v_pv12)			/* old TAE PARBLK?		*/
	return(NULL);			/* if so, no qualifiers	        */
    v = p_lookup (&(*v).v_qualst, rem_name);	/* recurse for quals	*/
    return (v);
    }

/* p_intg - get an integer parameter.
 * Return codes:
 *
 *	SUCCESS
 *	P_BADNAME 	if the parmeter name is bad.
 *	P_BADTYPE	if the parameter is not of type V_INTEGER.
 *	P_BADCOUNT	if the caller supplied array is not large enough to
 *			hold all the values.
 */

FUNCTION CODE p_intg 
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 FUNINT		dimen,		/* in:  dimension of intg		*/
 TAEINT		intg[],		/* out: receives the values		*/
 COUNT		*count		/* out: value count			*/
 )
    {
    IMPORT TEXT		pm_name[], pk_name[], pm_type[], pk_type[],
    			pm_dim[], pk_dim[];
    struct VARIABLE	*v;		/* variable in parm block		*/
    COUNT		i;

    if ((v = p_fvar(block, name)) == NULL) goto p__bnerr;
    if ((*v).v_type != V_INTEGER) goto p__bterr;	
    if ((*v).v_count > dimen) goto p__bcerr;	
    *count = (*v).v_count;
    for (i = 0; i < (*v).v_count; i++)
	intg[i] = IVAL(*v, i);
    return(SUCCESS);

p__bnerr:
    x_error((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return(P_BADNAME);

p__bterr:
    x_error((*block).mode, pm_type, pk_type, (uintptr_t) name, 0, 0);
    return(P_BADTYPE);

p__bcerr:
    x_error((*block).mode, pm_dim, pk_dim, (uintptr_t) name, 0, 0);
    return(P_BADCOUNT);
    }

/*	p_real - get a real parameter.
 *
 * 	Return codes:
 *
 *	SUCCESS
 *	P_BADNAME 	if the parmeter name is bad.
 *	P_BADTYPE	if the parameter is not of type V_REAL
 *	P_BADCOUNT	if the caller supplied array is not large enough to
 *			hold all the values.
 */

FUNCTION CODE p_real
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 FUNINT		dimen,		/* in:  dimension of intg		*/
 TAEFLOAT	real[],		/* out: receives the values		*/
 COUNT		*count		/* out: value count			*/
 )
    {
    IMPORT TEXT		pm_name[], pk_name[], pm_type[], pk_type[],
    			pm_dim[], pk_dim[];
    struct VARIABLE	*v;		/* variable in parm block		*/
    COUNT		i;

    if ((v = p_fvar(block, name)) == NULL) goto p__bnerr;	
    if ((*v).v_type != V_REAL) goto p__bterr;	
    if ((*v).v_count > dimen) goto p__bcerr;	
    *count = (*v).v_count;
    for (i = 0; i < (*v).v_count; i++)
	real[i] = RVAL(*v, i);
    return(SUCCESS);

p__bnerr:
    x_error((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return(P_BADNAME);

p__bterr:
    x_error((*block).mode, pm_type, pk_type, (uintptr_t) name, 0, 0);
    return(P_BADTYPE);

p__bcerr:
    x_error((*block).mode, pm_dim, pk_dim, (uintptr_t) name, 0, 0);
    return(P_BADCOUNT);
    }

/* p_string  - get a string parameter.
 *
 * 	A pointer to a vector of string pointers and the count
 *	of the vector are returned as output parameters.
 *
 * Return codes:
 *
 *	SUCCESS
 *	P_BADNAME	if the parameter name is bad.
 *	P_BADTYPE	if the parameter is not of type V_STRING.
 */

FUNCTION CODE p_string
(
 struct PARBLK	*block,		/* in:  parameter block			*/
 TEXT		name[],		/* in:  parameter name			*/
 TEXT 		***sptr,	/* out: pointer to string vector	*/
 COUNT		*count		/* out: number of strings		*/
 )
    {
    IMPORT TEXT		pm_name[], pk_name[], 
    			pm_type[], pk_type[];
    struct VARIABLE	*v;		/* variable in parm block		*/

    if ((v = p_fvar(block, name)) == NULL) goto p__bnerr;	/* point to parm*/
    if ((*v).v_type != V_STRING) goto p__bterr;	/* error if not string		*/
    *sptr = (TEXT **) (*v).v_cvp;	/* value pointer			*/
    *count = (*v).v_count;		/* current value count			*/
    return(SUCCESS);


p__bnerr:
    x_error((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return(P_BADNAME);

p__bterr:
    x_error((*block).mode, pm_type, pk_type, (uintptr_t) name, 0, 0);
    return(P_BADTYPE);

    }

/*
 *	p_str66.  Get strings for FORTRAN-66 callers.  The strings may be
 *	passed to the caller in a format determined by the convert argument.
 *
 *	This is used when the output strings are placed end-to-end in a
 *	FORTRAN array, each string starting on a new TAEINT boundary.
 *	The convert function (an argument) is typically p_s2ca1 or p_s2p;
 *	the convert function must return a COUNT of the number of FORTRAN elements
 *	used in the transfer.  (p_str66 is called by bridges for XPSTR, XPSTRP,
 *	and XRSTR.)
 *
 */

FUNCTION CODE p_str66
(
 struct PARBLK	*block,		/* in: parameter block			*/
 TEXT		name[],		/* in: name of string parameter		*/
 COUNT		(*convert) (char*, TAEINT*),	
				/* in: conversion function		*/
 COUNT		dimen,		/* in: dimension of out_string		*/
 TAEINT		out_string[]	/* out: strings in "convert" format	*/
)
    {
    IMPORT TEXT		pm_name[], pk_name[], 
    			pm_dim[], pk_dim[];
    struct VARIABLE	*v;
    VV_STRING		*s;		/* pointer to string value vector	*/
    COUNT		j;
    COUNT		i;

    v = p_fvar(block, name);		/* find parameter			*/
    if (v == NULL) goto p__bnerr;	/* not found				*/
    s = (VV_STRING *) (*v).v_cvp;		/* s = pointer to value vector		*/
    j = 0;				/* current index in out_string		*/    
    for (i=0; i < (*v).v_count; i++)
	{
	if (j >= dimen) goto p__bcerr;	/* out_string not dimensioned correctly	*/
	j += (*convert) ((*s)[i], &out_string[j]);  /* convert and update j		*/
	}
    if ((*v).v_maxc > 1) 			/* if multi-valued, then make eog	*/	
	{
	if (j >= dimen) goto p__bcerr;	/* out_string not dimensioned correctly	*/
	j += (*convert) ("\035", &out_string[j]);   /* supply eog string	*/
	}
    if (j-1 >= dimen) goto p__bcerr;	/* final dimension check		*/
    return(SUCCESS);



p__bnerr:
    x_error((*block).mode, pm_name, pk_name, (uintptr_t) name, 0, 0);
    return(P_BADNAME);

p__bcerr:
    x_error((*block).mode, pm_dim, pk_dim, (uintptr_t) name, 0, 0);
    return(P_BADCOUNT);
    }

/*
 * 	p_herr. Return host error code.
 */

FUNCTION  VOID  p_herr
(
 struct  PARBLK *block,		/* in: parameter block  */
 CODE		*hcode		/* out: host error code	*/
)
    {
    *hcode = (*block).hostcode;		/* retreive the host code */
    return;
    }

