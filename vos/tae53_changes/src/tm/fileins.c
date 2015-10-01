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
 *	Installation Exit for File Verification.
 */

#include	"stdh.inp"	/* system standard  (REQUIRED)		*/
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"fileinc.inp"	/* file package				*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/
#include "taeintproto.h"

/*
 *  Change log
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	08-sep-83	Changed V_NOTFILE to V_NOCHECK...palm
 *	10-aug-87	Look at $FILEVER global to decide whether
 *			to verify or not.  This avoids the mess of
 *			the old NOFILEINS module.  We don't create 
 *			$FILEVER in TMINIT because (a) it takes up
 *			SAVE-GLOBALS parblk space and (b) general
 *			clutter...palm
 */


/*

  At strategic places in TM, function calls are made to allow for 
  installation-specific logic in TM.EXE.  These functions are called 
  "installation exits".


  An installation may re-LINK TM, replacing this module with 
  installation-specific code.

*/

/*	file_ins.   File verification.
 *
  file_ins is called to verify a file variable before assignment.
  If an error is detected, file_ins returns FAIL and sets msgkey and
  msg so that the caller may produce an error message.

  If the file verifies successfully, file_ins returns SUCCESS.

  file_ins should always return SUCCESS if (*v).v_filemode
  is V_NOCHECK.
 *
 */

#define HOST_ESCAPE_CHAR '%'

FUNCTION CODE file_ins 
(
    struct VARIABLE *v,	  	  /* in: VARIABLE definition	*/
    GENPTR 		vector,	  /* in: value vector		*/
    FUNINT		count,	  /* in: value count		*/
    TEXT		msgkey[], /* out: message key (if error)*/
    TEXT		msg[]	  /* out: message (if error)	*/

 )
    {

    IMPORT struct SYMTAB glbtab;
    struct VARIABLE	*filever;
    BOOL		escape;		/* TRUE if host escape seen	*/
    TEXT		filename[STRINGSIZ+1];
    TEXT		**files;			/* file string ptrs	*/
    COUNT		i;
    struct SFILE	sfile;
    BOOL		verify_disabled;

    filever = lookex (&glbtab, "$FILEVER");
    verify_disabled = filever == NULL 		     || 
		      (*filever).v_type != V_STRING  ||
		      (*filever).v_count <= 0 	     ||
		      !s_equal (SVAL(*filever,0), "YES") ;
    if (!(*v).v_file  ||  (*v).v_filemode == V_NOCHECK || verify_disabled)
        return (SUCCESS);
    files = (TEXT **)vector;				/* cast to pt to pt	*/
    for (i=0; i<count; i++)
    	{
    	if (*(files[i]) == HOST_ESCAPE_CHAR)		/* 1st char is host-escape	*/
    	    {
    	    s_copy(&((files[i])[1]), filename);		/* strip escape char	*/
    	    escape = TRUE;
    	    }
    	else
    	    escape = FALSE;
        if ((*v).v_filemode == V_IN)		/* file should exist	*/
    	    if (escape)				/* host escape		*/
    		{
    		if (f_ophf(&sfile, ACCESSLUN, filename, F_READ) != SUCCESS)
    		    goto iopenerr;
    		f_close(&sfile, F_KEEP);
    		}
    	    else				/* no escape char	*/
    		{
#ifdef CAT_MAN				/* following if catalog manager */
#define XASUCCESS 1
    		xahost(files[i], dummy, &code);	/* using FORTRAN */
    		if (code != XASUCCESS)
    		    goto iopenerr;
#else
    		if (f_ophf(&sfile, ACCESSLUN, files[i], F_READ) != SUCCESS)
    		    goto iopenerr;
    		f_close(&sfile, F_KEEP);
#endif 
    		}
    	else if ((*v).v_filemode == V_INOUT)	/* file should exist	*/
    	    if (escape)				/* host escape		*/
    		{
    		if (f_ophf(&sfile, ACCESSLUN, filename, F_EXTEND) != SUCCESS)
    		    goto iopenerr;
    		f_close(&sfile, F_KEEP);
    		}
    	    else
    		{
#ifdef CAT_MAN
    		xahost(files[i], dummy, &code);		/* using FORTRAN */
    		if (code != XASUCCESS)
    		    goto iopenerr;
#else
    		if (f_ophf(&sfile, ACCESSLUN, files[i], F_EXTEND) != SUCCESS)
    		    goto iopenerr;
    		f_close(&sfile, F_KEEP);
#endif 
    		}
    	else if ((*v).v_filemode == V_OUT)
    		;			/* FUTURE: file name syntax check	*/
    	}
    return(SUCCESS);

iopenerr:
    s_copy("Unable to access IN or INOUT file '", msg);
    s_append(filename, msg);
    s_append("'", msg);
    s_copy("TAE-INVIFILE", msgkey);
    return(FAIL);
    }
