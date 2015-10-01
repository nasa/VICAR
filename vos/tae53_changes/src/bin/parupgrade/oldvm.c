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



#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"oldparblk.inc"	/* parameter block definitions		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include        "oldvminc.inc"         /* Dynamic allocation definitions      */

#include "taeintproto.h"

    static FUNCTION VOID init_new(

    struct OLD_VM_STRUCT *h,           /* parblk handle                */
    FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
    );


/*
 *	Vm_New.  Create a no-pool parblk and return handle
 */

    FUNCTION GENPTR Old_Vm_New(

    FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
    )

    {
    struct OLD_VM_STRUCT *h;           /* parblk handle                */
    COUNT	l,c;
    CODE	code;

    /* TBD: remove t_init call in Vm_init.  Needed now to make m_put work. */
    t_init(&l, &c, &code);

    /*
        allocate memory space for the header 
    */
    h = (struct OLD_VM_STRUCT *)tae_alloc ( 1, sizeof(struct OLD_VM_STRUCT) ); 
    init_new (h, mode);
    return( (GENPTR)h );
    }


/*
 *	init_new.  Initialize no-pool parblk and return handle
 */

    static FUNCTION VOID init_new(

    struct OLD_VM_STRUCT *h,           /* parblk handle                */
    FUNINT		mode		/* P_MODE_ABORT or P_MODE_CONT	*/
    )

    {
    struct OLD_NP_PARBLK *np;
    np = &( (*h).npblk );
    (*np).last   = 1;
    (*np).symtab.link = NULL;

    if ( mode == P_ABORT )
        (*np).mode = P_MODE_ABORT | P_MODE_TAE_ALLOC;
    else if ( mode == P_CONT )
        (*np).mode = P_MODE_CONT | P_MODE_TAE_ALLOC;
    else
        {
        mode = mode & (P_MODE_ABORT | P_MODE_CONT);
        (*np).mode = mode | P_MODE_TAE_ALLOC;
        }
    return;
    }

/*
 *        Old_Vm_Free - free the no-pool parblk structure
 *
 *
 */
    FUNCTION VOID Old_Vm_Free (

    GENPTR      h              /* IN: handle of no-pool parblk to be free */
    )

    {
    struct VM_STRUCT *vm;

    vm = (struct VM_STRUCT *)h;
    tae_free ( h );                            
    return;
    }
