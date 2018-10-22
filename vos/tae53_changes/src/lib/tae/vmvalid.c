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



/* TPL CHECKOUT FILE_TIME=15-JUL-1987 19:18 DUA1:[TAEV2.OLB.GENLIB]VMVALID.C;1 */
/*
 *	Vm_ functions to manipulate valid list in a no-pool V-block.
 *
 *	CHANGE LOG:
 *      23-APR-87	Created...tpl
 *      24-JUL-87	Changed q_validstring to q_validstr...tpl
 *	02-dec-88	Vm_SetOneValidString...tni
 *	22-jul-92	PR1519: Label Vm_SetOneValidString for ADA and
 *			label Vm_AllocValid,Vm_ValidSize as UNSUPPORTED...kbs
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include        "vminc.inc"         /* Dynamic valid definitions           */
#include "syninc.inc"
#include "taeintproto.h"



/*
 *	Vm_SetValidIntg.	Place integer values into object context
 */

    FUNCTION CODE Vm_SetValidIntg(h, name, count, low, high)

    GENPTR	        h;		/* in: handle                   */
    TEXT		name[];		/* in: variable name		*/
    FUNINT 		count;		/* in: count of variable	*/
    TAEINT		low[];		/* in: value vector		*/
    TAEINT		high[];		/* in: value vector		*/

    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;        
    return(q_validintg( p, name, count, low, high) );
    }

/*
 *	Vm_SetValidReal.	Place real (TAEFLOAT) values into context.
 */

    FUNCTION CODE Vm_SetValidReal(h, name, count, low, high)

    GENPTR	        h;		/* in: handle                   */
    TEXT		name[];		/* in: variable name		*/
    FUNINT 		count;		/* in: count of variable	*/
    TAEFLOAT		low[];		/* in: reals			*/
    TAEFLOAT		high[];		/* in: reals			*/

    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;        
    return(q_validreal(p, name, count, low, high) );
    }

/* 
 *	Vm_SetValidString.  Set string values into context.
 */

    FUNCTION CODE Vm_SetValidString(h, name, count, vector)

    GENPTR	        h;		/* in: handle                   */
    TEXT		name[];		/* in: variable name		*/
    FUNINT 		count;		/* in: count of vector		*/
					/* (0 means set count = 0)	*/
    TEXT		*vector[];	/* in: vector of string ptrs	*/

    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;        
    return( q_validstr( p, name, count, vector) );
    }		

/* 
 *	Vm_SetOneValidString.  Set one string value into context.
 */

ADA_FUNCTION CODE Vm_SetOneValidString
(
    GENPTR	        h,		/* in: handle                   */
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		idx,		/* in: which value number	*/
					/* (0 means set count = 0)	*/
    TEXT		onestr[]	/* in: one string ptr		*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;        
    return( q_validonestr( p, name, idx, onestr) );
    }		

/*   Vm_AllocValid.  Allocate valid structure.
 *   Caller must cast the returned pointer into pointer to proper VALID.
 */

UNSUPPORTED GENPTR Vm_AllocValid
(
 FAST struct VARIABLE *v,	/* in/out: ptr to var adding valid struct*/
 FUNINT lcount		/* in:  number of valid ranges	     */
 )
    {
    COUNT	bytes;
    FAST TEXT *p;
    FAST struct I_VALID *q;			

    bytes = Vm_ValidSize ((*v).v_type, lcount);
    (*v).v_valid = p = tae_alloc(1, bytes);	
    q = (struct I_VALID *) p;			/* any cast to x_VALID ok */
    (*q).count = lcount;		
    return(p);
    }

/*	Vm_ValidSize.   Return size of valid structure.
 */

UNSUPPORTED COUNT Vm_ValidSize 
(    
    FAST  FUNINT type,		/* in: V_INTEGER, V_REAL, V_STRING	*/
    FAST  FUNINT count		/* in: number of ranges			*/

     )
    {
    FAST COUNT	basic = 0;  /* size of valid COUNT plus one range	*/
    FAST COUNT  increment = 0;	/* size of additional ranges		*/

    if (type == V_INTEGER)
	{
	basic = sizeof(struct I_VALID); 
        increment =  sizeof(struct I_RANGE) ;
	}
    else if (type == V_REAL)
	{
	basic= sizeof(struct R_VALID) ;
	increment = sizeof(struct R_RANGE) ;
	}
    else if (type == V_STRING)
	{
	basic = sizeof(struct S_VALID);
	increment = sizeof(struct S_RANGE) ;
	}
    else 
        /*??????*/;                            /* bad type */
    if (count <= 0)
        return (basic);
    return (basic + (count-1)*increment);
    }
