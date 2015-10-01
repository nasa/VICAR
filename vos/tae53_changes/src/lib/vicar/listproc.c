/* listproc.
 *
 *
 *	CHANGE LOG:
 *
 *
 */

#include "stdh.inp"		/* standard C definitions		*/
#include "taeconf.inp"		/* TAE configuration definitions	*/
#include "fileinc.inp"		/* file & file position context structs	*/
#include "tmhost.inp"		/* host-dependent defs			*/
#include "symtab.inc"		/* symbol table				*/
#include "tminc.inc"		/* TM-only host-independent definitions	*/

#include "vicartae.inc"		/* VICAR-specific definitions		*/
#include "taeintproto.h"

/* listproc_do.  List the specified proc to stdout.
 */

FUNCTION CODE listproc_do
(
    struct CONTXT	*cpctx,		/* in:  current proc context	*/
    struct CONTXT	*npctx		/* in:  context of listproc cmd	*/

 )
    {
    struct VARIABLE	*v;
    TEXT		proc[STRINGSIZ+1];
    CODE		code;
    struct SFILE	file;
    struct FSBLOCK	fsblock;
    TEXT		record[STRINGSIZ+1];
    TEXT		errstr[STRINGSIZ+1];

    v  = lookex(&((*npctx).parmst), "PROC");	/* point to proc to list */
    s_copy(SVAL(*v, 0), proc);		/* copy value locally		*/
    f_crack(proc, "", "", "", &fsblock, errstr);
    if (NULLSTR(fsblock.type) || s_equal(fsblock.type, "CPD"))
	s_copy("PDF", fsblock.type);		/* Force PDF not CPD file */
    f_spec(&fsblock, proc);
    code = hierarchy(&file, proc, PDF_TYPE, HELPLUN, &fsblock, errstr);
    if (code != SUCCESS) goto open_err;
    while ((code = f_read(&file, record)) != F_EOF) /* for every line in file */
	{
	if (code != SUCCESS) goto read_err;
	put_stdout(record);
	}
    put_stdout("***********************************************************");
    f_close(&file, F_KEEP);
    return(DO_SUCCESS);

open_err:
    if (code == F_FILERR)
	{
	tmmsg(PROCFAIL, "Proc '%s':  %s.", 
	      "TAE-PDFRD", (uintptr_t) proc, (uintptr_t) file.errmsg, 0, 0, 0);
	return(DO_CHECK);
	}
    else if (code == F_NOFILE)
	{
	tmmsg(PROCFAIL, "Unable to locate proc '%s'.", "TAE-NOPROC",
	      (uintptr_t) proc, 0, 0, 0, 0);
	return(DO_CHECK);
	}

read_err:
    tmmsg(PROCFAIL, "Read error in proc '%s':  %s",
	  "TAE-RDERR", (uintptr_t) proc, (uintptr_t) file.errmsg, 0, 0, 0);
    f_close(&file, F_KEEP);
    return(DO_CHECK);
    }
