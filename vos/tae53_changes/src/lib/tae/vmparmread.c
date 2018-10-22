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



/* Written in C, functions in this package should be called 
 * by application programs to retrieve parameters from
 * a no-pool parameter block initialized as discussed below.
 *
 * A caller supplied parameter block is initialized either with
 *
 *	Vm_ReadFromTm - receives parameters from the terminal monitor (TM), or
 *	Vm_ReadFromDisk - reads parameters from a specially formatted disk file.
 *
 * Note that the above two functions are in other source files.
 *
 * CHANGE LOG:
 *
 *            27-APR-87 Created...tpl
 *            22-Jul-92 PR1519: Label Vm_Find as FUNCTION...kbs
 */

#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include        "vminc.inc"
#include "taeintproto.h"



/* Vm_GetAttribute - get attributes for a parameter.
 * The value(s) of the parameter is retrieved via the other Vm_ calls.
 * Return codes:
 *
 *	SUCCESS
 *	P_BADNAME	if the parameter name was bad.
 */

FUNCTION CODE Vm_GetAttribute 
(
    Id                  h,	/* in:  no-pool parblk handle	*/
    TEXT		name[],	/* in:  name of the parameter	*/
    CODE		*type,	/* out: parameter  type		*/
    COUNT		*n, /* out: number of values for the parm	*/
    BOOL		*dflt, /* out: TRUE if parm was defaulted	*/
    CODE		*access	/* out: access mode if a file		*/
				/* Vm_IN, Vm_OUT, Vm_INOUT, Vm_NONE */
 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;
    return(p_attr (p, name, type, n, dflt, access) );

    }

/*	Vm_FindVar - find variable in parameter block.  Return VARIABLE pointer.
 */

FUNCTION struct VARIABLE *Vm_FindVar 
(
    Id                  h,              /* in:  no-pool parblk handle */
    const TEXT		*name		/* in:  parameter name	      */

 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;
    return( p_fvar ( p, (TEXT *) name ) ); /* only parms have qualifiers */
    }

/*	Vm_Find.    Returns variable pointer 
 *		   and aborts if no such variable.
 */

FUNCTION struct VARIABLE  *Vm_Find 
(
    GENPTR              h,              /* in: no-pool parblk handle    */
    TEXT		name[]		/* in: name of parameter	*/

 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;

    return ( p_find ( p, name ) );
    }
        

/* Vm_GetIntg - get an integer parameter.
 * Return codes:
 *
 *	SUCCESS
 *	P_BADNAME 	if the parmeter name is bad.
 *	P_BADTYPE	if the parameter is not of type V_INTEGER.
 *	P_BADCOUNT	if the caller supplied array is not large enough to
 *			hold all the values.
 */

FUNCTION CODE Vm_GetIntg 
(
    GENPTR              h,              /* in:  no-pool parblk handle   */
    TEXT		name[],		/* in:  parameter name		*/
    FUNINT		dimen,		/* in:  dimension of intg	*/
    TAEINT		intg[],		/* out: receives the values	*/
    COUNT		*count		/* out: value count		*/

 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;

    return( p_intg ( p, name, dimen, intg, count ) );

    }

/*	Vm_GetReal - get a real parameter.
 *
 * 	Return codes:
 *
 *	SUCCESS
 *	P_BADNAME 	if the parmeter name is bad.
 *	P_BADTYPE	if the parameter is not of type V_REAL
 *	P_BADCOUNT	if the caller supplied array is not large enough to
 *			hold all the values.
 */

FUNCTION CODE Vm_GetReal 
(
    GENPTR              h,              /* in:  no-pool parblk handle   */
    TEXT		name[],		/* in:  parameter name		*/
    FUNINT		dimen,		/* in:  dimension of intg	*/
    TAEFLOAT		real[],		/* out: receives the values	*/
    COUNT		*count		/* out: value count		*/

 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;

    return( p_real ( p, name, dimen, real, count ) );

    }

/* Vm_GetString  - get a string parameter.
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

FUNCTION CODE Vm_GetString  
(
    GENPTR              h,              /* in:  no-pool parblk handle   */
    TEXT		name[],		/* in:  parameter name		*/
    TEXT 		***sptr,	/* out: pointer to string vector*/
    COUNT		*count		/* out: number of strings	*/

 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;

    return( p_string ( p, name, sptr, count ) );

    }

/*
 * 	Vm_GetHostError. Return host error code.
 */

FUNCTION  VOID  Vm_GetHostError
(
    Id                  h,		/* in: no-pool parblk handle*/
    CODE		*hcode		/* out: host error code	    */


 )
    {
    struct VM_STRUCT    *vm;
    struct PARBLK       *p;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *)&(*vm).npblk;

    *hcode = (*p).hostcode;		/* retreive the host code */
    return;
    }
