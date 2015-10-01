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



/* TPL CHECKOUT FILE_TIME=31-AUG-1987 10:15 DUA1:[TAEV2.OLB.GENLIB]VMPARMTASK.C;8 */
/* TPL CHECKOUT FILE_TIME= 7-AUG-1987 10:39 DUA1:[TAEV2.OLB.GENLIB]VMPARMTASK.C;2 */
/* TPL CHECKOUT FILE_TIME=15-JUL-1987 19:18 DUA1:[TAEV2.OLB.GENLIB]VMPARMTASK.C;1 */
/*    A caller supplied parameter block is initialized with
 *    Vm_ReadFromTM - receives parameters from the terminal monitor (TM), or
 *
 *
 * The caller may then retrieve individual parameters and their attributes
 * using calls in other source files.
 *
 *
 * CHANGE LOG:
 * 06-AUG-1987	Initialize link in vm_readfromtm...tpl
 * 26-AUG-1987	Use LARGE_PARBLK instead of just PARBLK...tpl
 * 10-SEP-1987	Return correct code if tutor was terminated...tpl
 * 11-AUG-1988	Called x_error with the proper mode bit...tpl
 * 29-SEP-1988	Update return codes per documentation...nhe
 * 22-JUL-1992	PR1519: Label functions as CLASSIC...kbs
 * 22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3 does not
 *		like it...rt
 */

#include	"stdh.inp"
#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include        "pgminc.inc"
#include        "vminc.inc"
#include "taeintproto.h"


extern GLOBAL TEXT     tae_runtyp[STRINGSIZ+1];	/* run type of process      */
extern GLOBAL TEXT     tae_sess[STRINGSIZ+1]; /* for sharing session ID...*/


/* Vm_ReadFromTM - receive parameter block from TM.
 *
 * In addition to receiving the parameter block, Vm_ReadFromTM performs other
 * initialization steps on block.
 *
 *
 * Return codes:
 *
 *	P_KILLED if user terminated (exit in dyntut or CONTORL/C)
 *	P_FAIL for general failures
 *	SUCCESS
 */

CLASSIC_FUNCTION CODE Vm_ReadFromTM 
(

    GENPTR              h              /* out: handle                  */ 
 )
    {
    struct LARGE_PARBLK	block;		/* parm block to be initialized	*/
    struct VM_STRUCT    *vm;
    IMPORT TEXT		pm_killed[], pk_killed[];
    CODE		code;
    struct  VARIABLE    *v;
    struct  VARIABLE    *last_var;
    struct  VARIABLE    *vp;
    struct  NP_PARBLK   *np;

    vm = (struct VM_STRUCT *)h;
    np = &( (*vm).npblk );                    /* clean-up link just in case */
    (*np).last   = 1;
    (*np).symtab.link = NULL;

    code = c_rcvp((GENPTR)&block, LARGE_P_BYTES);

    (*vm).npblk.hostcode = code;		/* save host error code	*/

    if (code != SUCCESS )
 	{
	x_error((*vm).npblk.mode,"Unable to receive parameters from TAE monitor. Code %d.",
		"TAE-PRMRCV", (uintptr_t) code, 0, 0);
    	return ( P_FAIL );
        }

    /*
     *	The following function converts all pointers in PARBLK from relative
     *	to absolute pointers so that the block may be easily manipulated.
     */
    makeabs(&block.symtab, block.pool);


    if ( (block.msgtyp == M_KILLED) || (block.msgtyp == M_DYNEXIT) )
	{
	  x_error((*vm).npblk.mode, pm_killed, pk_killed, 0, 0, 0);
        return (P_KILLED);
	}
    else
        {
	if (block.msgtyp == M_INIPAR) 	/* is this first PARBLK?    */
    	    {
	      v = p_fvar ((struct PARBLK* )&block, 
			  "$RUNTYPE");	/* find $RUNTYPE global	    */
	    if (v != NULL)
	        s_copy (SVAL(*v,0), tae_runtyp);  /* save for later use	    */
	    v = p_fvar ((struct PARBLK*) &block, 
			"$SESSION");	/* find $SESSION global	    */
	    if (v != NULL)
	        s_copy (SVAL(*v,0), tae_sess);	/* save for later use	    */
	    }

        for (vp=block.symtab.link; 
                vp != NULL; vp = (*vp).v_link )
	    {
            v = Vm_AllocVar (&(*vm).npblk.symtab);
            if (Vm_SpCopyVar(vp, v) == FAIL) 	/* copy the variable	    */
        	    return ( P_FAIL );


                                                /* find last var            */	
            for (last_var=(struct VARIABLE *)&(*vm).npblk.symtab;
                          (*last_var).v_link != NULL; 
                          last_var = (*last_var).v_link )
                ;
            (*last_var).v_link = v;
	    }
        
        }
        return ( SUCCESS );
    }
/*
 *	Vm_DynTutor.  Send V-block to TM for dynamic parameters.
 */

CLASSIC_FUNCTION CODE Vm_DynTutor
(
    GENPTR              h,
    TEXT		pdfspec[],	/* pdf file spec		*/
    FUNINT		mode		/* M_SUBPDF or M_FULLPDF	*/

 )
    {
    struct LARGE_PARBLK p;

    Vm_st2blk ( h, &p, LARGE_P_BYTES );
    return ( q_dynp ((struct PARBLK*) &p, pdfspec, mode ) );
    }

/*
 * 	Vm_SetTCLVar. Send output V-block to TAE monitor.
 *	
 */

CLASSIC_FUNCTION  CODE  Vm_SetTCLVar
(
    GENPTR              h              /* handle to a nopool parblk    */

 )
    {
    struct LARGE_PARBLK p;

    Vm_st2blk ( h, &p, LARGE_P_BYTES );
    return ( q_out ((struct PARBLK*) &p) );
    }
