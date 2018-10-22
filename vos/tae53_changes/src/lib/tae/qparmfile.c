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



/* TPL CHECKOUT FILE_TIME=27-MAY-1987 11:45 DUA1:[TAEV2.OLB.GENLIB]QPARMFILE.C;1 */
/* TDM CHECKOUT FILE_TIME= 6-JUL-1983 15:58 DUA0:[TAEV1.OLB]QPARMFILE.C;1 */
/*
 *	q_ functions to write a V-block to disk.
 *	The other q_ functions are located in other source files.
 *
 *	CHANGE LOG:
 *
 *	25-oct-83	fix q_wrtb return codes to match with PGM...dm
 *	03-may-84	move IMPORT declaration...lim
 *      24-jun-87       Write multi-record if parblk is big...tpl
 *	14-feb-88	PARBLK to LARGE_PARBLK in q_wrtb...ljn
 *	13-oct-88	Break out a routine to write the records...nhe
 *	09-feb-89	Zero parhdr record before writing; zero header
 *			portion of parblk when initializing...palm
 *	06-may-92	Used ne sentinel P_BIGSENTINEL...tpl
 * 30-sep-92	PR1644: Ada has trouble with LARGE_PARBLK allocated on the
 *		stack. Now dynamically allocated and freed in q_wrtblk...krw
 *
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"parblk.inc"	/* parameter block definitions		*/
#include	"resinc.inc"	/* restricted allocation package	*/
#include	"symtab.inc"	/* TM symbol table			*/
#include 	"fileinc.inp"	/* f_ package includes			*/
#include "taeintproto.h"


/*
 *	q_wrtb.   Write V-block to disk.
 *
 *	TBD: set date/time in PARHDR structure for binary files. q_wrtb.
 */

FUNCTION CODE q_wrtb
(
 TEXT	filespec[],	/* in: file specification		*/
 FUNINT	lun,		/* in: lun to use			*/
 struct PARBLK *p		/* in: V-block to write			*/
 )    
    {
    struct PARHDR	ph;	/* parameter file header record		*/
    struct SFILE	f;	/* file i/o block			*/
    CODE		code;
    
    code = f_opnspc(&f, lun, filespec, "", "", PAR_TYPE, F_WRITE);
    (*p).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save host error code	*/
    if (code != SUCCESS)
        return (P_FAIL);

/*  build and write header record...					*/

    zero_block ((GENPTR)&ph, sizeof (ph));
    s_copy(P_BIGSENTINEL, ph.sentinel);
    ph.recsize = (*p).blksiz;           

    s_copy("TIME", ph.datetime);	/* dummy date/time for now	*/
    code = f_bwrite(&f, (GENPTR)&ph,  sizeof(struct PARHDR));
    (*p).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save host error code	*/
    if (code != SUCCESS)
        goto close_ret;
    code = q_wrtblk (&f, p);		/* write block to the file 	*/
    f_close (&f, F_KEEP);
    return (code);

close_ret:
    f_close(&f, F_DELETE);
    x_error((*p).mode, "Error writing parameter file '%s'. %s", "TAE-PFWRT", 
	    (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
    return (P_FAIL);
    }

/*
 *	q_wrtblk.  Write the parblk to file pointed by by f.
 *
 */
FUNCTION CODE q_wrtblk
(
 struct SFILE	*f,		/* in: opened file		*/
 struct PARBLK 	*p		/* in: PARBLK to write		*/
 )
    {
    struct LARGE_PARBLK *pout;
    CODE		code;
    struct SYMTAB	*sym;
    struct VARIABLE	*v;
    CODE		mode;

    mode = ((*p).mode == P_CONT)? P_CONT : P_ABORT;
    pout = (struct LARGE_PARBLK *) tae_alloc (1, sizeof (struct LARGE_PARBLK));
    q_init((struct PARBLK*) pout, sizeof(struct LARGE_PARBLK), mode); /* init storage space */
    (*pout).last = FALSE;

/* Pack all the variables in 'symtab' into as many parblks as necessary.  */

    sym = &(*p).symtab;
    for (v=(*sym).link; v!=NULL; v = (*v).v_link)
        {
        if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
		continue;
        code = Vm_ParblkOut(f, pout, v);	      /* add variable to file */
        if (code != SUCCESS)
		{
		tae_free(pout);
		return (code);
		}
        }
    (*pout).last = TRUE;
    makerel(&(*pout).symtab, (*pout).pool);	      /* make ptrs relative   */
    (*pout).blksiz = r_top((*pout).pool) - (GENPTR) pout;
    code = f_bwrite(f, (GENPTR) pout, (*pout).blksiz);
    (*pout).hostcode = (code == SUCCESS) ? code : (*f).host_code;	/* save host error code	*/
    if (code != SUCCESS)
        {
        f_close(f, F_DELETE);
        x_error( (*pout).mode, "Error writing parameter file '%s'. %s", "TAE-PFWRT", 
		 (uintptr_t) (*f).full_spec, (uintptr_t) (*f).errmsg, 0);
	tae_free(pout);
        return (P_FAIL);
        }

    tae_free(pout);
    return (SUCCESS);
    }
