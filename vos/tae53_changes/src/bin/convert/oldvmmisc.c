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



#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"

#include "taeintproto.h"

FUNCTION struct VARIABLE *Vm_Alloc(
        struct SYMTAB   *symtab,        /* in: symtab to link to        */
        TEXT            name[],         /* in: name of variable         */
        FUNINT          type,           /* in: variable type            */
        FUNINT          count,          /* in: number of values         */
        FUNINT              strsiz         /* in: string max size          */
);

/*	New Vm_ functions.  
 *
 *	TBD: move these to TAE$GENLIB where they belong.
 */
#include	"vminc.inc"


#include "taeintproto.h"

/*	Vm_CopyVarToVm.    Copy a VARIABLE into a Vm object. 
 *
 */

FUNCTION VOID Vm_CopyVarToVm (

    Id			vmid,		/* Vm object to receive...	*/
    struct VARIABLE	*v		/* a copy of this variable	*/
    )
	
    {
    struct VM_STRUCT *vmHandle;
    struct VARIABLE  *destv;		/* destination of copy		*/

    vmHandle = (struct VM_STRUCT *) vmid;
    destv = Vm_Alloc (&(*vmHandle).npblk.symtab,  
    			 (*v).v_name, 
    			 (*v).v_type,
    			 (*v).v_count,
			0);	/* no idea if this is right... rgd 2/2010 */
    Vm_CopyVar (v, destv);
    }

/*	Vm_ForEach.   Execute a callback for each variable.
 *
 */

FUNCTION CODE Vm_ForEach (

    Id		vmid,			/* vm object of interest	*/
    CODE	(*callback) (struct VARIABLE *v, GENPTR context),	/* function to call for each	*/
    GENPTR	context		/* context to pass to function	*/
)

    {
    struct VM_STRUCT *vmHandle;
    struct VARIABLE  *v, *vnext;
    CODE	code;

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

FUNCTION struct PARBLK	*Vm_Parblk (

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

FUNCTION static CODE copyOne (struct VARIABLE *, Id);

FUNCTION VOID Vm_Copy (

    struct VM_STRUCT	*target,	/* destination Vm id */
    struct VM_STRUCT	*source		/* source Vm id      */
)

    {
    
    Vm_ForEach ((Id)source, copyOne, (GENPTR)target);
    return;
    }
 
FUNCTION static CODE copyOne (

    struct VARIABLE 	*v,		/* variable to copy	*/
    Id			target		/* Vm id of target      */
)
    {
    Vm_CopyVarToVm (target, v);
    return (0);
    }
