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



/* Subroutine package to read parameters from a file.
 * Written in C, functions in this package should be called (probably via
 * bridge routines) by application programs.
 *
 * A caller supplied parameter block is initialized either with
 *
 *	p_inim - receives parameters from the terminal monitor (TM), or
 *	p_rdb - reads parameters from a specially formatted disk file.
 *
 * Of the above initialization routines, only p_rdb is in this source file.
 *
 * The caller may then retrieve individual parameters and their attributes
 * using calls in the parm package.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	25-oct-83	Match p_rdb return codes with PGM...dm
 *	18-nov-83	Add P_ERROR return code...dm
 *	14-oct-85	Modify p_rdb to read files with multiple PARBLKs...lia
 *	1-apr-87	Allow bytes in caller PARBLK to be > 32K...palm
 *      07-jul-87       Added restrict storage mode bit...tpl
 *      09-sep-87       Call q_init to set up the parblk if the par file
 *                      is a null parfile...tpl
 *	08-feb-89	Check for old formatted par hdr...palm
 *	17-feb-89	Fix TUTIL -> TLIB in error message...palm
 *	25-apr-91	New VMS TAE logicals...ljn
 *	06-may-92	Used new sentinel P_BIGSENTINEL...tpl
 *	25-may-93	$TAEBIN/$TAEPLAT changed to simply $TAEBIN...kbs
 *
 */

#include	"taeconf.inp"
#include	"symtab.inc"
#include	"parblk.inc"
#include 	"fileinc.inp"
#include	"resinc.inc"
#include "taeintproto.h"

#ifdef VMS
#define PARCONVERT $TAEPDF:parconvert
#else
#define PARCONVERT $TAEPDF/parconvert
#endif


/*
 *	p_rdb.   Read parameter file from disk.
 *
 */

FUNCTION CODE p_rdb
(
 TEXT		filespec[],	/* in: file to read		*/
 COUNT		lun,		/* in: lun to use		*/
 struct PARBLK	*p,		/* out: PARBLK to read		*/
 FUNINT         bytes,		/* in: bytes in PARBLK		*/
 FUNINT		mode		/* in: P_CONT, P_ABORT		*/
 )
    {
    struct SFILE    	f;		/* SFILE structure		*/
    struct PARHDR	ph;    		/* p-file header record		*/
    struct PARBLK	*pblkptr;
    struct VARIABLE	*vp;
    GENPTR		bufptr, ptr;
    LONG 		bufsiz;		/* size of buffer in bytes	*/
    LONG 		pool_size, bytes_used;
    CODE		code;
    COUNT		recsize;
    BOOL                nullpar;

    code = f_opnspc(&f, lun, filespec, "", "", PAR_TYPE, F_READ);
    (*p).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save error code  */
    if (code != SUCCESS)
        goto bad_open;
    code = f_bread(&f, (GENPTR)&ph, sizeof(struct PARHDR), &recsize);  /* read header */
    (*p).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save error code  */
    if (code != SUCCESS)
        goto io_error;
    if (s_equal (ph.sentinel,OLD_P_SENTINEL) )
	goto old_format;
    if (s_equal (ph.sentinel,P_SENTINEL) )
	goto v51_format;
    if (!s_equal(ph.sentinel, P_BIGSENTINEL) )
       goto bad_format;
    if (bytes <= 0) goto bad_size;
    bufptr = (GENPTR)p;
    bufsiz = bytes;			/* bytes remaining in PARBLK */
    nullpar = TRUE;
    while (FOREVER)
	{				/* read one record w/ PARBLK */
	code = f_bread(&f, bufptr, min (bufsiz, MAX_COMPACT_COUNT), &recsize);	
	if (code != SUCCESS)
	    break;
        nullpar = FALSE;
	pblkptr = (struct PARBLK *) bufptr;
	makeabs(&(*pblkptr).symtab, (*pblkptr).pool);	/* make pointers absolute	*/
	if (pblkptr == p)				/* for 1st PARBLK read	*/
	    {
	    pool_size = bufsiz - ((GENPTR)(*p).pool - (GENPTR)p);
	    r_newsiz((*p).pool, pool_size);		/* set to new size for XQ allocation	*/
	    }
	else
	    {						/* for additional PARBLK read */
	    ptr = r_alloc((*p).pool, recsize);		/* reserve space in pool for record read */
	    for (vp = (struct VARIABLE *) &(*p).symtab;
	    	(*vp).v_link != NULL; vp = (*vp).v_link)
	    	;					/* find end of symbol table */
	    (*vp).v_link = (*pblkptr).symtab.link;	/* link up symbol tables into one */
	    }
	bytes_used = ALIGNS(recsize) * sizeof(ALIGN);
	bufsiz -= bytes_used;
	if (bufsiz <= 0) goto bad_size;
	bufptr += bytes_used;
	}
    if (code != F_EOF)
	{
	(*p).hostcode = f.host_code;			/* save error code	*/
	goto io_error;
	}
    
    f_close(&f, F_KEEP);
    if ( nullpar )                                      /* is it a null parfile */
         {
         q_init ( p, bytes, mode );
         return(SUCCESS);
         }
    (*p).hostcode = SUCCESS;
    if ( mode == P_ABORT )
        (*p).mode = P_MODE_ABORT | P_MODE_RESTRICT;
    else if ( mode == P_CONT )
        (*p).mode = P_MODE_CONT | P_MODE_RESTRICT;
    else
        {
        (*p).mode = (mode & (P_MODE_ABORT | P_MODE_CONT ) )
                    | P_MODE_RESTRICT;
        }
    return(SUCCESS);
    
bad_open:
    x_error(mode, "Unable to open parameter file '%s'.  %s.", "TAE-PFOPN",
	    (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
    return(P_FAIL);			/* failure indication		*/

io_error:
    f_close(&f, F_KEEP);
    x_error(mode, "Error reading parameter file '%s'.  %s.", "TAE-PFRD",
	    (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
    return(P_FAIL);					/* general failure */

bad_size:
    f_close(&f, F_KEEP);
    x_error(mode, "Invalid or insufficient block dimension for a read.",
	    "TAE-BADSIZE", 0, 0, 0);
    return(P_FAIL);

old_format:
    f_close (&f, F_KEEP);
    x_error(mode,
          "File '%s' has obsolete format.  Use PARCONVERT.",
	    "TAE-OLDPARFILE", (uintptr_t) filespec, 0, 0);
    return (P_ERROR);

v51_format:
    f_close (&f, F_KEEP);
    x_error(mode, 
#ifdef UNIX
           "File '%s' has obsolete format. Use $TAEBIN/parupgrade.",
#else
           "File '%s' has obsolete format. Use tae$lib:parupgrade.",
#endif
	    "TAE-OLDPARFILE", (uintptr_t) filespec, 0, 0);
    return (P_ERROR);
bad_format:
    f_close(&f, F_KEEP);
    x_error(mode, "Parameter file '%s' is not correctly formatted.", 
	    "TAE-VBADPFILE", (uintptr_t) filespec, 0, 0);
    return(P_ERROR);					/* bad  V_block   */
    }
