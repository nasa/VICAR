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
 *      Vm functions to get a copy or pointer to the valid list.
 *
 *      CHANGE LOG:
 *                27-AUG-90 Created...rt
 *		  14-SEP-90 Changed Vm_GetValidIntg/Real...rt
 *                07-DEC-90 Changed Vm_GetValidIntg/Real/String so they return
 *                          0 for count when there are no valids...rt
 *   04-jan-91	added Vm_GetValidIntg/Real_noalloc for ADA bindings...cew
 *   08-jan-91  added Vm_GetOneValidString for ADA bindings...cew
 *   19-mar-91  return when encounter error...tpl pr 878
 *   22-jul-92  PR1519: Label Vm_GetOneValidString & *noalloc as ADA_FUNCTION...kbs
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include        "vminc.inc"	/* Dynamic valid definitions		*/
#include "taeintproto.h"


/************************************************************************ 
 *	Vm_GetValidString.  Get pointer to string values into context.
 *		This function sends back the count of the valids and
 *		the POINTER to the valid list to the caller.  Unlike the
 *		other Vm_GetValidXxxx routines, this routine does NOT
 *		send back a copy of the valids.  This allows the programmer
 *		to look at the valids without going through the overhead
 *		of copying the valids.
 */

FUNCTION CODE Vm_GetValidString
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		*count,		/* out: count of vector		*/
    TEXT		***vector	/* out: vector of string ptrs	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct S_VALID	*origvalid;	/* Pointer to original svalid	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	      */
    if (tmpvar == NULL)				  /* Is it a variable name?   */
        return (P_BADNAME);
    if ((*tmpvar).v_type != V_STRING)		  /* Is it a string variable? */
        return (P_BADTYPE);
    origvalid = (struct S_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct S_VALID *) NULL)     /* Count set to 0 and       */
        {                                         /* vector set to NULL if    */
	*count = 0;                               /* there were no valids.    */
	*vector = NULL;
	return (SUCCESS);
        }
    *count = (*origvalid).count;                  /* Set the count	      */
    *vector = (TEXT **) (*origvalid).slist; 
    return (SUCCESS);
    }

/************************************************************************ 
 * Vm_GetOneValidString.  Get pointer to one string value valid.
 *		This function sends back the POINTER to the valid at index.
 */

ADA_FUNCTION CODE Vm_GetOneValidString
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		index,		/* in: index of valid to get	*/
    TEXT		*str		/* out: string ptr		*/
 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct S_VALID	*origvalid;	/* Pointer to original svalid	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	      */
    if (tmpvar == NULL)				  /* Is it a variable name?   */
        return(P_BADNAME);
    if ((*tmpvar).v_type != V_STRING)		  /* Is it a string variable? */
        return(P_BADTYPE);
    origvalid = (struct S_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct S_VALID *) NULL)     /* Count set to 0 and       */
        {                                         /* vector set to NULL if    */
	str[0] = EOS;          			  /* there were no valids.    */
	return (SUCCESS);
        }
    if (index > (*origvalid).count)

        {
	str[0] = EOS;
	return (P_BADCOUNT);
	}
    s_copy ((TEXT *) (*origvalid).slist[index].string, str);
    return (SUCCESS);
    }

/************************************************************************ 
 *	Vm_GetValidReal.  Get copy of real valid ranges into context.
 *		This function sends back the count of the valids and
 *		pointers to a copy of the valid ranges to the caller,
 */

FUNCTION CODE Vm_GetValidReal
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		*count,		/* out: count of vector		*/
    TAEFLOAT		**low,		/* out: pointer to low valids	*/
    TAEFLOAT	        **high		/* out: pointer to high valids	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct R_VALID	*origvalid;	/* Pointer to original rvalid	*/
    COUNT  lcv;				/* Loop control variable	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	     */
    if (tmpvar == NULL)				  /* Is it a variable name?  */
        return(P_BADNAME);
    if ((*tmpvar).v_type != V_REAL)		  /* Is it a float variable? */
        return(P_BADTYPE);
    origvalid = (struct R_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct R_VALID *) NULL)     /* Count set to 0 and low  */
        {                                         /* and high set to NULL if */
	(*low) = (*high) = NULL;                  /* there were no valids    */
	(*count) = 0;
	return (SUCCESS);
        }
    (*count) = (*origvalid).count;                /* Set the count	     */
                                                  /* Allocate range space    */
    (*low)  = (TAEFLOAT *) tae_alloc ((*count), sizeof(TAEFLOAT));
    (*high) = (TAEFLOAT *) tae_alloc ((*count), sizeof(TAEFLOAT));
                                                  /* Copy low range valids   */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        (*low)[lcv] = (*origvalid).range[lcv].low;
                                                  /* Copy high range valids  */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        (*high)[lcv] = (*origvalid).range[lcv].high;

    return (SUCCESS);
    }

/************************************************************************ 
 * Vm_GetValidReal_noalloc.  Get copy of real valid ranges into context.
 *		This function sends back the count of the valids and
 *		filled arrays of the valid ranges to the caller,
 *              This is identical to Vm_GetValidReal except it does not
 *		allocate space.
 */

ADA_FUNCTION CODE Vm_GetValidReal_noalloc
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		*count,		/* out: count of vector		*/
    TAEFLOAT		low[],		/* out: pointer to low valids	*/
    TAEFLOAT	        high[]		/* out: pointer to high valids	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct R_VALID	*origvalid;	/* Pointer to original rvalid	*/
    COUNT  lcv;				/* Loop control variable	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	     */
    if (tmpvar == NULL)				  /* Is it a variable name?  */
        return(P_BADNAME);
    if ((*tmpvar).v_type != V_REAL)		  /* Is it a float variable? */
        return(P_BADTYPE);
    origvalid = (struct R_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct R_VALID *) NULL)     /* Count set to 0 if no    */
        {                                         /* no valids. low and high */
/*	(*low) = (*high) = NULL; */               /* are not set to 0 since  */
	(*count) = 0;				  /* they point to allocated */
	return (SUCCESS);			  /* space.                  */
        }
    (*count) = (*origvalid).count;                /* Set the count	     */

                                                  /* Copy low range valids   */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        low[lcv] = (*origvalid).range[lcv].low;
                                                  /* Copy high range valids  */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        high[lcv] = (*origvalid).range[lcv].high;

    return (SUCCESS);
    }

/************************************************************************ 
 *	Vm_GetValidIntg.  Get copy of the integer valid ranges into context.
 *		This function sends back the count of the valids and
 *		pointers to a copy of the valid ranges to the caller.
 */

FUNCTION CODE Vm_GetValidIntg
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		*count,		/* out: count of vector		*/
    TAEINT		**low,		/* out: pointer to low valids	*/
    TAEINT	        **high		/* out: pointer to high valids	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct I_VALID	*origvalid;	/* Pointer to original rvalid	*/
    COUNT lcv;				/* Loop control variable	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	    */
    if (tmpvar == NULL)				  /* Is it a variable name? */
        return(P_BADNAME);
    if ((*tmpvar).v_type != V_INTEGER)		  /* Is it an int variable? */
        return(P_BADTYPE);
    origvalid = (struct I_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct I_VALID *) NULL)     /* Count set to 0 and low */
      {                                           /* and high set to NULL if*/
	(*low) = (*high) = NULL;                  /* there were no valids   */
	*count = 0;
	return (SUCCESS);
      }
    *count = (*origvalid).count;                  /* Set the count	    */
                                                  /* Allocate range space    */
    *low  = (TAEINT *) tae_alloc ((*count), sizeof(TAEINT));
    *high = (TAEINT *) tae_alloc ((*count), sizeof(TAEINT));
                                                  /* Copy low range valids   */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        (*low)[lcv] = (*origvalid).range[lcv].low;
                                                  /* Copy high range valids  */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        (*high)[lcv] = (*origvalid).range[lcv].high;

    return (SUCCESS);
    }




/************************************************************************ 
 * Vm_GetValidIntg_noalloc.  Get copy of the integer valid ranges into context.
 *		This function sends back the count of the valids and
 *		filled arrays of the valid ranges to the caller.
 *              This is identical to Vm_GetValidIntg except it does not
 *		allocate space.
 */

ADA_FUNCTION CODE Vm_GetValidIntg_noalloc
(
    GENPTR	        h,		/* in: handle			*/
    TEXT		name[],		/* in: variable name		*/
    FUNINT 		*count,		/* out: count of vector		*/
    TAEINT		low[],		/* out: pointer to low valids	*/
    TAEINT	        high[]		/* out: pointer to high valids	*/

 )
    {
    struct PARBLK       *p;
    struct VM_STRUCT    *vm;
    struct VARIABLE	*tmpvar;	/* Pointer to original variable	*/
    struct I_VALID	*origvalid;	/* Pointer to original rvalid	*/
    COUNT lcv;				/* Loop control variable	*/

    vm = (struct VM_STRUCT *)h;
    p = (struct PARBLK *) &(*vm).npblk;
    tmpvar = (struct VARIABLE *) p_fvar(p, name); /* Get variable name	    */
    if (tmpvar == NULL)				  /* Is it a variable name? */
        return(P_BADNAME);
    if ((*tmpvar).v_type != V_INTEGER)		  /* Is it an int variable? */
        return(P_BADTYPE);
    origvalid = (struct I_VALID *)(*tmpvar).v_valid;
    if (origvalid == (struct I_VALID *) NULL)     /* Count set to 0 if no    */
      {                                           /* valids. low and high    */
/*	(*low) = (*high) = NULL; */               /* are not set to 0 since  */
	*count = 0;				  /* they point to allocated */
	return (SUCCESS);			  /* space.                  */
      }
    *count = (*origvalid).count;                  /* Set the count	    */

                                                  /* Copy low range valids   */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        low[lcv] = (*origvalid).range[lcv].low;
                                                  /* Copy high range valids  */
    for (lcv = 0 ; lcv < (*count); lcv ++)
        high[lcv] = (*origvalid).range[lcv].high;

    return (SUCCESS);
    }

