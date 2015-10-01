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
 * Change Log:
 *
 * 07-apr-92	Changed call to Vm_Alloc to Vm_AllocVar in Vm_CopyVarToVm. 
 *		We were getting	the current value pointer allocated twice. 
 *		Also the original call was missing a parameter!..krw
 * 22-jul-92	PR1519: Label Vm_CopyVarToVm & Vm_Parblk UNSUPPORTED...kbs
 */ 
#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include "taeintproto.h"

/*	New Vm_ functions.  
 */
#include	"vminc.inc"

/*	Vm_CopyVarToVm.    Copy a VARIABLE into a Vm object. 
 *
 */

UNSUPPORTED VOID Vm_CopyVarToVm 
(
    Id			vmid,		/* Vm object to receive...	*/
    struct VARIABLE	*v		/* a copy of this variable	*/
	
 )
    {
    struct VM_STRUCT *vmHandle;
    struct VARIABLE  *destv;		/* destination of copy		*/
    FAST struct VARIABLE *pc;		/* current pointer               */

    vmHandle = (struct VM_STRUCT *) vmid;

    destv = Vm_AllocVar(&(*vmHandle).npblk.symtab);

				/* Link the symbol into the vm's symtab */
    for(pc=(struct VARIABLE *)&(*vmHandle).npblk.symtab; 
		(*pc).v_link != NULL; pc=(*pc).v_link)
        ;                               /* find end of chain            */
    (*pc).v_link = destv;               /* link new entry to chain      */

    Vm_CopyVar (v, destv);
    }

/*	Vm_ForEach.   Execute a callback for each variable.
 *
 */

FUNCTION CODE Vm_ForEach 
(
    Id		vmid,			/* vm object of interest	*/
    CODE	(*callback) (struct VARIABLE  *, GENPTR),	
				/* function to call for each	*/
    GENPTR	context		/* context to pass to function	*/

 )
    {
    struct VM_STRUCT *vmHandle;
    struct VARIABLE  *v, *vnext;
    CODE	code = 0;

    vmHandle = (struct VM_STRUCT *) vmid;
    for (v=(*vmHandle).npblk.symtab.link; v != NULL; v=vnext)
        {
	vnext = (*v).v_link;		/* capture it here in case deletion */
	code = (*callback) (v, context);
	if (code != 0)			/* non-zero return terminates early */
	    break;
	}
    return (code);			/* caller gets callback's last return */
    }


/*	return PARBLK pointer for callers that need both worlds     */

UNSUPPORTED struct PARBLK	*Vm_Parblk 
(
    Id		vmid			/* vm object of interest	*/

 )
    {
    struct VM_STRUCT *vmHandle;
    struct PARBLK	*parblk;

    vmHandle = (struct VM_STRUCT *) vmid;
    parblk = (struct PARBLK *) & (*vmHandle).npblk;
    return (parblk);
    }

/*	Vm_Copy.	Make full copy of a Vm object.
 *
 *	This adds all of the VARIABLEs from the source Vm
 *	to the target Vm.  The target Vm must have already
 *	been created.
 */

FUNCTION static CODE copyOne 
(
    struct VARIABLE 	*v,		/* variable to copy	*/
    Id			target		/* Vm id of target      */

 )
    {
    Vm_CopyVarToVm (target, v);
    return (0);
    }

FUNCTION VOID Vm_Copy 
(
    struct VM_STRUCT	*target,	/* destination Vm id */
    struct VM_STRUCT	*source	/* source Vm id      */

 )
    {
    
      Vm_ForEach ((Id) source, copyOne, (GENPTR) target);
    return;
    }
 
