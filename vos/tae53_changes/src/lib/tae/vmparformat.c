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
 * Parameter formatting routines.
 *
 * CHANGE LOG:
 *
 *      27-APR-87  Created...tpl
 *      22-jul-92  PR1519: Label functions as CLASSIC_FUNCTION...kbs
 *	22-oct-92  Prototyping tae_alloc is unnecessary and Ultrix 4.3 does not
 *		   like it...rt
 ***********************************************************************
 */

#include	"stdh.inp"		/* system standard */
#include	"taeconf.inp"		/* TAE configuration */
#include	"parblk.inc"		/* parameter block definitions */
#include	"resinc.inc"		/* restricted allocation package */
#include	"tminc.inc"		/* TM  related definitions */
#include        "vminc.inc"                
#include "taeintproto.h"


/*
 *	 Vm_InitFormat:   Initialise for parameter block formatting
 */
    CLASSIC_FUNCTION  VOID  Vm_InitFormat(h)

    GENPTR     h;			/* IN: handle to parblk */

    {
    struct  FORCTX	*forctx;			/* pointer to pool */
    struct  VM_STRUCT   *vm;

    vm = (struct VM_STRUCT *)h;
    forctx = (struct FORCTX *) tae_alloc(1, sizeof (struct FORCTX));
						/* allocate storage */
    (*forctx).nxtind = -1;			/* set indexes for first call*/
    (*forctx).nxtvar = (*vm).npblk.symtab.link;
    (*vm).npblk.ctxptr = (GENPTR) forctx ;		/* save ptr in parblk */
    return;
    }

/*
 * 	Vm_FormatVar :  Format parameters in the parameter block.
 */

CLASSIC_FUNCTION  CODE  Vm_FormatVar
(
    GENPTR              h,                      /* IN: no-pool parblk handle */
    TEXT		line[],			/* OUT: formatted parameter rec */
    COUNT		length			/* IN: line length */ 

 )
    {
    CODE 		code;
    struct VM_STRUCT    *vm;

    vm = (struct VM_STRUCT *)h;
    code = m_forp((struct FORCTX * )(*vm).npblk.ctxptr, line, length);
    if ( code == FP_EOP && (*vm).npblk.ctxptr != NULL ) 
            tae_free( (*vm).npblk.ctxptr );	/* end of parameter list     */
    return(code);
    }
