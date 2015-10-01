/* Processes the MOUNT and DISMOUNT commands under Unix.  Since there	*/
/* is no concept of tape mounting or device allocation under Unix,	*/
/* these routines do pretty much what ALLOC and DEALLOC do in terms	*/
/* of setting up the TAE global variables.  There are no ALLOC or	*/
/* DEALLOC commands under Unix.  These commands could be implemented	*/
/* outsize of TAE for Unix, but they are in here to maintain more	*/
/* parallelism with the VMS version.					*/

#include "stdh.inp"		/* system standard  (REQUIRED)		*/
#include "taeconf.inp"		/* TAE configuration (REQUIRED)		*/
#include "symtab.inc"		/* TM symbol table			*/
#include "tmhost.inp"		/* TM host-dependent definitions	*/
#include "tminc.inc"		/* TM definitions			*/
#include "taeintproto.h"
#include "vicartae.inc"		/* VICAR-specific definitions		*/

/*
    TCL globals for tape handling:

    $TAPES, $TFILE, and $TREC are columns of a table for allocated
    and/or mounted tapes.  Each element of $TAPES is of form 
    "symbolicname=devicename".  $TFILE is the current physical position,
    zero if position unknown, and -1 if the tape is not mounted.
    $TREC is the record position.

    The globals are implicitly passed to every process so the process
    knows the tape position.  The globals are returned as output
    values from each process.

    At BOT, $TFILE is 1 and $TREC is 1.

 */

    GLOBAL struct VARIABLE *tapes_gbl;
    GLOBAL struct VARIABLE *tfile_gbl;
    GLOBAL struct VARIABLE *trec_gbl;


/*	mount_do.  Intrinsic processing for the MOUNT command.
 */

FUNCTION CODE mount_do
(
    struct CONTXT *procctx,	/* in/out: enclosing proc contxt	*/
    struct CONTXT *cmdctx	/* in/out: command contxt		*/

 )
    {
    struct VARIABLE *d, *n;	/* variables for DEVICE and NAME	*/
    COUNT i, index;
    TEXT **tapes_cvp, string[STRINGSIZ+1];
    TEXT name_upper[STRINGSIZ+1];
    TEXT *skey_vv[1];
    IMPORT struct VARIABLE *skey_gbl;

    d = lookex(&(*cmdctx).parmst, "DEVICE");
    n = lookex(&(*cmdctx).parmst, "NAME");
    make_upper_case(name_upper, SVAL(*n,0));
    tapes_cvp = (TEXT **) (*tapes_gbl).v_cvp;	/* $TAPES value pointer	*/
    i = i_search_name (tapes_cvp, (*tapes_gbl).v_count, name_upper);
    if (i >= 0)
        {
    	tmmsg(PROCFAIL, "Symbolic name '%s' in use.", "TAE-INUSE", 
              (uintptr_t) name_upper, 0, 0, 0, 0);
        return (DO_CHECK);
        }
    i = i_search_device (tapes_cvp, (*tapes_gbl).v_count, SVAL(*d,0));
    if (i >= 0)
        {
	tmmsg(PROCFAIL, "'%s' already allocated.", "TAE-ALLOC", 
	      (uintptr_t) SVAL(*d,0), 0, 0, 0, 0);
	return (DO_CHECK);
	}

/*	add new entry to tape tables		*/

    s_copy (name_upper, string);		/* build $TAPE entry	*/
    s_append("=", string);
    s_append(SVAL(*d,0), string);
    index = (*tapes_gbl).v_count;		/* current table count	*/
    if (index >= (*tapes_gbl).v_maxc)		/* if no room in table	*/
        {
	tmmsg(PROCFAIL, "More than %d tapes in use.", "TAE-MANYTAPES", 
	      (uintptr_t) (*tapes_gbl).v_maxc, 0, 0, 0, 0);
	return (DO_CHECK);
	}
    (*tapes_gbl).v_count ++;
    SVAL(*tapes_gbl, index) = s_save(string);
    index = (*tfile_gbl).v_count;
    if (index < (*tfile_gbl).v_maxc)
        {
        (*tfile_gbl).v_count++;
        IVAL(*tfile_gbl, index) = 1;
        }
    index = (*trec_gbl).v_count;
    if (index < (*trec_gbl).v_maxc)
        {
        (*trec_gbl).v_count++;
        IVAL(*trec_gbl, index) = 1;		/* initial record pos	*/
        }

    /* note that $SKEY is set to device name */

    skey_vv[0] = SVAL(*d,0);			/* value vector	for...  */
    set_value (skey_gbl, (GENPTR)skey_vv, 1);	/* set $SKEY		*/
    return (DO_CHECK);				/* already written 	*/
    }						/* success message	*/

/*	dismount_do.  Intrinsic processing for the DISMOUNT command.
 */

FUNCTION CODE dismount_do
(
 struct CONTXT *procctx,	/* in/out: enclosing proc contxt	*/
 struct CONTXT *cmdctx	/* in/out: command contxt		*/
 )
    {

    struct VARIABLE *d, *n;	/* variables for DEVICE and NAME	*/
    TEXT  full_name[TAPEDEVSIZE+1];	/* full device name allocated	*/
    COUNT i, index, n_index = 0, d_index = 0;
    TEXT **tapes_cvp, e_name[TAPENAMESIZE+1];

    d = lookex(&(*cmdctx).parmst, "DEVICE");
    n = lookex(&(*cmdctx).parmst, "NAME");
    tapes_cvp = (TEXT **) (*tapes_gbl).v_cvp;	/* $TAPES value pointer	*/
    if ((*d).v_count != 0)			/* DEVICE present	*/
        {
	d_index = i_search_device (tapes_cvp, (*tapes_gbl).v_count, SVAL(*d,0));
	if (d_index < 0)
	    {
	    tmmsg(PROCFAIL, "Device name '%s' not mounted.", "TAE-BADDEV",
		  (uintptr_t) SVAL(*d,0), 0, 0, 0, 0);
	    return (DO_CHECK);
	    }
        }
    if ((*n).v_count != 0)			/* NAME present		*/
	{
        n_index = i_search_name (tapes_cvp, (*tapes_gbl).v_count,
			         SVAL(*n,0));
	if (n_index < 0)
	    {
            make_upper_case(e_name, SVAL(*n,0));
	    tmmsg (PROCFAIL, "Undefined symbolic device '%s'.", "TAE-SYMBOL", 
	    	   (uintptr_t) e_name, 0, 0, 0, 0);
	    return (DO_CHECK);
	    }
	i_crack (tapes_cvp[n_index], e_name, full_name);    /* get device */
	}
    if ((*d).v_count != 0  &&  (*n).v_count != 0)	/* NAME and DEVICE 	 */
        {
	if (n_index != d_index)
	    {
	      tmmsg(PROCFAIL, "Inconsistent NAME and DEVICE.", "TAE-INCONS", 
		    0, 0, 0, 0, 0);
	    return (DO_CHECK);
	    }
	index = n_index;
	}
    else if ((*d).v_count == 0  &&  (*n).v_count == 0) /* neither present	*/
        {
	  tmmsg(PROCFAIL, "No device identified.", "TAE-NODEVID", 
		0, 0, 0, 0, 0);
	return (DO_CHECK);
	}
    else if ((*d).v_count != 0)			/* DEVICE only present	*/
        index = d_index;
    else 					/* NAME only present	*/
        index = n_index;			    

/*	remove entry from tape tables	*/

    tae_free(SVAL(*tapes_gbl, index));
    for (i=index; i < (*tapes_gbl).v_count - 1; i++)
	{
        SVAL(*tapes_gbl, i) = SVAL(*tapes_gbl, i+1);
        IVAL(*tfile_gbl, i) = IVAL(*tfile_gbl, i+1);
        IVAL(*trec_gbl, i) = IVAL(*trec_gbl, i+1);
        }
    (*tapes_gbl).v_count--;
    (*tfile_gbl).v_count--;
    (*trec_gbl).v_count--;

    return (DO_CHECK);
    }
