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
//static char vmparmfile_cVersion[] = "@(#)vmparmfile.c	33.1 8/26/94";


/* TPAM CHECKOUT FILE_TIME=10-SEP-1987 18:03 DUA1:[TAEV2.OLB.GENLIB]VMPARMFILE.C;8 */
/* TPL CHECKOUT FILE_TIME=31-AUG-1987 10:14 DUA1:[TAEV2.OLB.GENLIB]VMPARMFILE.C;7 */
/* TPL CHECKOUT FILE_TIME= 7-AUG-1987 10:39 DUA1:[TAEV2.OLB.GENLIB]VMPARMFILE.C;2 */
/* TPL CHECKOUT FILE_TIME=15-JUL-1987 19:17 DUA1:[TAEV2.OLB.GENLIB]VMPARMFILE.C;1 */
/*
 *	This file contains the function to perform save and restore
 *	of TCL variables from a file and put them into dynamic memory
 *      parblk.
 *
 *	CHANGE LOG:
 *            06-AUG-87 Initialize link in vm_readfromdisk...tpl
 *            26-AUG-87 Use LARGE_PARBLK instead of just PARBLK...tpl
 *            10-SEP-87 qualifier for all variables not just parms...tpl
 *	      14-dec-87	Add Vm_WriteVm and Vm_ReadVm; zero PARHDR before
 *		writing; set PARBLK.last correctly...palm
 *	      29-mar-88	Correct s_bcopy to ph.filename...palm
 *	      29-sep-88 Change Vm_ReadFrom/WriteToDisk to return P_FAIL...nhe
 *	      14-oct-88 Change Vm_WriteVm to call new q_wrtblk...nhe
 *	      02-feb-89	New Vm_CopyValid argument...palm
 *	      08-feb-89	Look for old style formatted par file...palm
 *	      17-feb-89 Fix error message mentioning $TUTIL...palm
 *	      02-aug-89	Init a var to avoid compiler warning...ljn
 *	      20-aug-91	Minor efficiency in Vm_ReadVm. Removed search for
 *			last variable in the symbol table...krw
 *	      06-may-92 Used new sentinel P_BIGSENTINEL...tpl
 *	      22-jul-92	PR1519: Label several functions as UNSUPPORTED...kbs
 * 22-sep-92	PR1644: in Vm_ReadVm tae_alloc the LARGE_PARBLK rather than
 *		using the stack. Some languages (Ada) have problems with large
 *		stack allocations...krw
 * 25-may-93	$TAEBIN/$TAEPLAT changed to $TAEBIN; $TLIB to $TAEBIN...kbs
 * 28-apr-94	pr-2690 (SCO port): In Vm_ReadVm, tae_alloc LARGE_PARBLK only 
 *		once, use static ptr.  Avoid growth of taewb to 37 meg...swd
 * 03-jun-94	pr-2741 (conc port): Concurrent needs pr-2690 fix, too...swd
 */

#include	"taeconf.inp"		/* TAE configuration definitions */
#include	"tminc.inc"		/* TM host-independent defs	 */
#include	"symtab.inc"		/* Symbol table			 */
#include	"parblk.inc"		/* Parameter block defs		 */
#include	"fileinc.inp"		/* file primitive		 */
#include        "vminc.inc"
#include "taeintproto.h"


static UNSUPPORTED GENPTR Vm_ValueMove 
(
    struct VARIABLE	*rvar,		/* in:  basic struct of new variable	*/
    GENPTR		old_vec,	/* in:  val vector to move from		*/
    ALIGN		*s		/* in/out: restricted storage block	*/

 );
UNSUPPORTED struct VARIABLE * Vm_VarMove
(
    struct VARIABLE *p,		/* input: VARIABLE struct to move	*/
    ALIGN	    *s		/* input: restricted storage block	*/

 );



/*
 *	Vm_ReadFromDisk.   Read parameter file from disk and add the
 *                variables to an existing vm object.
 *
 */

    FUNCTION CODE Vm_ReadFromDisk(h, filespec)
   
    GENPTR              h;              /* in: existing vm object	*/
    TEXT		filespec[];	/* in: file to read		*/
    
    {
    struct SFILE    	f;		/* SFILE structure		*/
    struct PARHDR	ph;    		/* p-file header record		*/
    CODE		code;
    COUNT		recsize;
    struct  VM_STRUCT   *vm;
    struct  NP_PARBLK 	*np;

    vm = (struct VM_STRUCT *)h;
    np = &( (*vm).npblk );   

    /*	the following seems a bad idea because Vm_ReadFromDisk would be
	more useful if we ADDED to the Vm rather than replaced it.
 	However, for compatibility with existing callers, we have
 	to start from scratch.
    */

    Vm_FreeTable (&(*np).symtab);	
   
    code = f_opnspc(&f, 1, filespec, "", "", PAR_TYPE, F_READ);
    (*np).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save error code  */
    if (code != SUCCESS)
        goto bad_open;
    code = f_bread(&f, (GENPTR)&ph, sizeof(struct PARHDR), &recsize);  /* read header */
    (*np).hostcode = (code == SUCCESS) ? code : f.host_code;	/* save error code  */
    if (code != SUCCESS)
        goto bad_read;
    if (s_equal (ph.sentinel, OLD_P_SENTINEL))
	goto old_format;
    if (s_equal (ph.sentinel, P_SENTINEL))
	goto v51_format;
    if (!s_equal(ph.sentinel, P_BIGSENTINEL))
       goto bad_format;
    code = Vm_ReadVm (vm, &f);		/* if error, x_error already called */
    f_close (&f, F_KEEP);
    return ((code==SUCCESS) ? SUCCESS : P_FAIL);

bad_open:
    x_error((*vm).npblk.mode, "Unable to open parameter file '%s'.  %s.", 
	    "TAE-PFOPN", (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
    return (P_FAIL);			

bad_read:
    f_close(&f, F_KEEP);
    x_error((*vm).npblk.mode, "Error reading parameter file '%s'.  %s.", 
	    "TAE-PFRD", (uintptr_t) filespec, (uintptr_t) f.errmsg, 0);
    return(P_FAIL);		

old_format:
    f_close(&f, F_KEEP);
    x_error((*vm).npblk.mode, 
#ifdef UNIX
	   "File '%s' has obsolete format.  Use $TAEBIN/parconvert.", 
#else
	   "File '%s' has obsolete format.  Use tae$lib:parconvert.", 
#endif
	    "TAE-OLDPARFILE", (uintptr_t) filespec, 0, 0);
    return(P_FAIL);	
v51_format:
    f_close(&f, F_KEEP);
    x_error((*vm).npblk.mode,
#ifdef UNIX
           "File '%s' has obsolete format. Use $TAEBIN/parupgrade.",
#else
           "File '%s' has obsolete format. Use tae$lib:parupgrade.",
#endif
	    "TAE-OLDPARFILE", (uintptr_t) filespec, 0, 0);
    return(P_FAIL);

bad_format:
    f_close(&f, F_KEEP);
    x_error((*vm).npblk.mode, "Parameter file '%s' is not correctly formatted.",
	    "TAE-VBADPFILE", (uintptr_t) filespec, 0, 0);
    return(P_FAIL);	
    }

/*	Vm_ReadVm.    Read into an existing vm object from an opened file.
 *	We read until we see a PARBLK record with p.last set.
 *	When calling this, the PAR file must be positioned
 *	at a PARBLK record.
 *
 *	A concatenated PAR file is the result of $COPYing (or cat'ing under
 *	UNIX) several standard PAR files into one file.  When reading, we know
 *	the "sub-file" names because PARHDR now has a "PARHDR.filename"
 *	field.   We know when to expect a new PARHDR because the
 *	PARBLK.last flag is set TRUE for the last PARBLK record of
 *	a sub-file.   Only PAR files written since we implemented PARBLK.last
 *	and PARHDR.filename can be concatenated.
 */

UNSUPPORTED CODE Vm_ReadVm 
(

 struct 	VM_STRUCT	*vm,		/* in: existing vm 	    */
 struct SFILE		*f		/* in: opened file
						   context  */
)

    {
#if defined(sco) || defined(masscomp)
    static struct	LARGE_PARBLK	*p = NULL;
#else
    struct		LARGE_PARBLK	*p;
#endif
    COUNT			recsize;
    CODE			code;
    struct VARIABLE		*vp, *v, *last_var;

#if defined(sco) || defined(masscomp)
    if (!p) p = (struct LARGE_PARBLK *)tae_alloc(1, sizeof (struct LARGE_PARBLK));
#else
    p = (struct LARGE_PARBLK *)tae_alloc(1, sizeof (struct LARGE_PARBLK));
#endif

    while (FOREVER)
	{					/* read each record 	*/
	code = f_bread(f, (GENPTR) p, sizeof(struct LARGE_PARBLK), &recsize);
   	if (code == F_EOF) 
	    break;
	else if (code != SUCCESS)
	    goto bad_read;
	makeabs(&(*p).symtab, (*p).pool);	/* make pointers absolute */
	if (chk_parblk((struct PARBLK*) p) != SUCCESS)
	    goto bad_format;			/* check internal integrity */

	/* find last variable in the existing vm object */
        for (last_var=(struct VARIABLE *)&(*vm).npblk.symtab;
                          (*last_var).v_link != NULL;
                          last_var = (*last_var).v_link )
                ;
        for (vp=(*p).symtab.link; vp != NULL; vp = (*vp).v_link)
	    {
            v = Vm_AllocVar (&(*vm).npblk.symtab);
            if (Vm_SpCopyVar(vp, v) == FAIL) 	/* copy the variable	    */
		    {
#if !(defined(sco) || defined(masscomp))
		    tae_free(p);
#endif
        	    return (FAIL);		
		    }
            last_var = (*last_var).v_link = v;	/* link into the symtab */
	    }
        if ((*p).last)				/* last record for this vm */
	    break;
	}
#if !(defined(sco) || defined(masscomp))
    tae_free (p);
#endif
    return (SUCCESS);

bad_read:
    tae_free(p);
    x_error((*vm).npblk.mode, "Error reading parameter file '%s'.  %s.", 
	    "TAE-PFRD", (uintptr_t) (*f).full_spec, (uintptr_t) (*f).errmsg, 0);
    return(FAIL);		

bad_format:
    tae_free(p);
    x_error((*vm).npblk.mode, "Parameter file '%s' is not correctly formatted.", 
	    "TAE-VBADPFILE", (uintptr_t) (*f).full_spec, 0, 0);
    return (FAIL);	

    }

/*
 *  Vm_WriteToDisk - Save the variables into a standard save files
 *
 *  returns SUCCESS/P_FAIL; error msg in sf
 *
 */
FUNCTION CODE Vm_WriteToDisk
(
    Id          h,		/* in: V-block to write			*/
    TEXT	filespec[]	/* in: file specification		*/
    
 )
    {
    TAEINT	lun = 0;	/* dummy lun to use			*/
    struct PARHDR	ph;	/* parameter file header record		*/
    struct SFILE	sf;	/* file i/o block			*/
    CODE		code;
    struct  VM_STRUCT   *vm;
    struct  FSBLOCK	fsblock;
    TEXT		ignore[STRINGSIZ+1];

    vm = (struct VM_STRUCT *)h;
    code = f_opnspc(&sf, lun, filespec, "", "", PAR_TYPE, F_WRITE);
    if (code != SUCCESS)
	{
	x_error ((*vm).npblk.mode, "Error opening PAR file '%s'. '%s'.",
		 "TAE-PAROPN", (uintptr_t) filespec, (uintptr_t) sf.errmsg, 0);
	return (P_FAIL);
	}

/* 	Write the header	*/

    zero_block ((GENPTR) &ph, sizeof (ph));
    s_copy(P_BIGSENTINEL, ph.sentinel);
    ph.recsize = sizeof (struct LARGE_PARBLK);
    s_copy("TIME", ph.datetime);		/* dummy date/time for now */
    f_crack (sf.full_spec, "", "", "", &fsblock, ignore);
    s_bcopy (fsblock.name, ph.filename, sizeof (ph.filename) - 1);
    code = f_bwrite(&sf, (GENPTR) &ph, sizeof(struct PARHDR));	
    if (code != SUCCESS)
    	return (P_FAIL);
    code = Vm_WriteVm (vm, &sf);
    f_close (&sf, F_KEEP);
    return ((code==SUCCESS) ? SUCCESS : P_FAIL);

    }

/*	Vm_WriteVm.	Write a vm to an opened file.
 * 
 */

UNSUPPORTED CODE Vm_WriteVm 
(
 struct	VM_STRUCT	*vm,		/* vm object to write	*/
 struct	SFILE		*sf		/* opened file		*/
 )
    {
    CODE			code;

    /* We let q_wrtblk do the work so we don't have to repeat the code */
    code = q_wrtblk (sf, (struct PARBLK*) &(*vm).npblk);
    return (code);
    }

/*
 *  Vm_ParblkOut - Accumulate variables in a parblk, and write it when full
 *
 *  returns SUCCESS or FAIL; error code in sfile
 *
 */
UNSUPPORTED CODE Vm_ParblkOut
(
 struct SFILE	*sf,		/* in: file context		*/
 struct LARGE_PARBLK	*par,		/* in/out: parblk		*/
 struct VARIABLE	*var		/* in: the variable to add	*/
 )
    {
    struct VARIABLE	*vcur;		/* current variable pointer	*/
    CODE		code;
    struct VARIABLE	*v;		/* scratch VARIABLE pointer	 */


    vcur = Vm_VarMove(var, (*par).pool);     /* move to pool */
    if ((*par).symtab.link == NULL  &&  vcur == NULL)
        return (FAIL);			/* no room for even one VAR	*/
    else if (vcur == NULL) 		/* no more room, write it	*/
    	{
    	makerel(&(*par).symtab, (*par).pool);
	(*par).blksiz = r_top((*par).pool) - (GENPTR)par ;
    	code = f_bwrite(sf, (GENPTR) par, (*par).blksiz);	
    	if (code != SUCCESS)
    	    return (code);
	r_init((*par).pool, LARGE_P_BYTES);		/* init storage area	*/
	(*par).symtab.link = NULL;		/* in case no variables	*/
    	code = Vm_ParblkOut(sf, par, var);
        if (code != SUCCESS)
		return (code);
    	}
    else
    	{
	for (v = (struct VARIABLE *) & (*par).symtab;
	     (*v).v_link != NULL;  v = (*v).v_link)
	     ;					/* find end of chain	*/
	(*v).v_link = vcur;			/* and link to old last */
        (*vcur).v_link = NULL;			/* this is last in chain*/
    	}
    return(SUCCESS);
    }

/*
 *	Vm_st2blk.  Build PARBLK for one symbol table only.
 *
 *	Return codes:
 *		SUCCESS
 *		FAIL -- not enough room in PARBLK for all the parms
 */
    
UNSUPPORTED CODE Vm_st2blk
(
 GENPTR              h,              /* in: handle to nopool parblk  */
 struct LARGE_PARBLK	*p,		/* out: PARBLK			*/
 FUNINT              pool_size      /* in: pool size of parblk      */
 )
    {
    struct SYMTAB	*symtab;	/* symbol table to use	*/
    struct VM_STRUCT    *vm;            /* ptr to current nopool parblk  */
    struct VARIABLE     *pv;		/* ptr to current PARBLK VARIABLE*/
    struct VARIABLE     *v;		/* ptr to current input VARIABLE */

    vm = (struct VM_STRUCT *)h;
    symtab = & (*vm).npblk.symtab;
   
    r_init((*p).pool, pool_size);	/* initialize pool		*/
    (*p).symtab.link = NULL;		/* PARBLK initially empty	*/
    pv = (struct VARIABLE *) &(*p).symtab;
    for (v=(*symtab).link; v != NULL; v=(*v).v_link)
        {
        if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
        pv = (*pv).v_link = Vm_VarMove(v, (*p).pool);  /* move and link in	*/
        if (pv == NULL) return (FAIL);
        }
    (*p).last = TRUE;
    return (SUCCESS);
    }

/*
 *	Vm_VarMove.   Move VARIABLE structure to restricted storage and
 *	return its pointer.
 */

UNSUPPORTED struct VARIABLE * Vm_VarMove
(
    struct VARIABLE *p,		/* input: VARIABLE struct to move	*/
    ALIGN	    *s		/* input: restricted storage block	*/

 )
    {
    struct VARIABLE *rp;	/* pointer into restricted storage	*/
    struct VARIABLE *up;	/* unresolved version of p		*/
    struct VARIABLE *tv;	/* target variable in loop		*/
    struct VARIABLE *v;		/* source variable in loop		*/
    TINY	    class;	/* V_GLOBAL, V_PARM, etc.		*/
    struct R_VALID  *vdin;	/* pointer to valid structure in p	*/
    COUNT	    bytes;

    up = p;			/* unresolved version of p		*/
    p = RESOLVE(up);	/* resolve indirection			*/
    rp = (struct VARIABLE *) r_alloc(s, sizeof(struct VARIABLE));
    if (rp == NULL)
	return(NULL);
    MOVE_STRUCT(*p, *rp);	/* move structure to restricted storage	*/
    (*rp).v_default = (*up).v_default;		/* in case V_NAME	*/
    s_copy ((*up).v_name, (*rp).v_name);	/* use unresolved name	*/
    (*rp).v_link   = NULL;	/* end of chain for now			*/
    class = (*rp).v_class;
    (*rp).v_pv12 = TRUE;

    /* handle name parameters (for compiled procs)	*/

    if ((*rp).v_type == V_NAME)		/* implies VM_NREF specials bit set	*/
	{				/* need to copy v_nref			*/
	if ((*p).v_nref == NULL)
	    (*rp).v_nref = NULL;
	else
	    {
	    (*rp).v_nref = (TEXT *)r_alloc(s, s_length((*p).v_nref)+1);
	    if ((*rp).v_nref == NULL) return(NULL);
	    s_copy((*p).v_nref, (*rp).v_nref);
	    }
	return(rp);
	}

    (*rp).v_valid  = NULL;	/* formality				*/
    (*rp).v_dvp    = NULL;	/* formality				*/
    (*rp).v_dcount = 0;		/* formality				*/
    (*rp).v_qualst.link = NULL;		/* no parm qualifs yet		*/
    if (class == V_PARM)	    
	{
	(*rp).v_wasname = ((*up).v_type == V_NAME);
	(*rp).v_tp  = NULL;			/* formality			*/
	}

    /* now move the qualifier symbol table	*/

    tv = (struct VARIABLE *) &(*rp).v_qualst;
    for (v=(*p).v_qualst.link; v != NULL; v=(*v).v_link)
        {
        if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
        	continue;
	tv = (*tv).v_link = Vm_VarMove(v, s);	/* recursive - move and link in	*/
	if (tv == NULL) return(NULL);
	}

    /* allocate, fetch, and move values	 */

    if (class == V_PARM  &&  (*rp).v_deref)	/* if parm DEFAULT deref'd*/
	{
	(*rp).v_cvp = (TEXT *)r_alloc(s, s_length((*p).v_cvp)+1);
	if ((*rp).v_cvp == NULL) return(NULL);
	s_copy((*p).v_cvp, (*rp).v_cvp);
	}
    else
	{
	(*rp).v_cvp = Vm_ValueMove(rp, (*p).v_cvp, s);	/* move current vals	*/
	if ((*rp).v_cvp == NULL) return(NULL);
	}
        if ( (*p).v_dvp == NULL )
            (*rp).v_dvp = NULL;
        else
            (*rp).v_dvp = Vm_ValueMove(rp, (*p).v_dvp, s);	/* move default vals*/

    /* now the other special parts	*/

    if ( (*p).v_valid != NULL ) /* if valid requested*/
	{
	vdin = (struct R_VALID *) (*p).v_valid;
	bytes = Vm_ValidSize((*rp).v_type, (*vdin).count);
	(*rp).v_valid = r_alloc(s, bytes);
	if ((*rp).v_valid == NULL) return(NULL);
	if (Vm_CopyValid((*p).v_type, (*p).v_valid, (*rp).v_valid, 
			 (GENPTR) s)!=SUCCESS)
	    return (NULL);
	}
    if (class == V_GLOBAL)
	{
	(*rp).v_pdf = NULL;			/* formality			*/
	if ( (*p).v_pdf != NULL )
	    {
	    (*rp).v_pdf = (struct DEFPDF *) r_alloc(s, sizeof(struct DEFPDF));
	    if ((*rp).v_pdf == NULL) return(NULL);
	    MOVE_STRUCT(*(*p).v_pdf, *(*rp).v_pdf);
	    }
	}
    return (rp);
    }

/*
 *	Vm_ValueMove.  Move a value vector into restricted storage and
 *	return its pointer.
 */

static UNSUPPORTED GENPTR Vm_ValueMove 
(
    struct VARIABLE	*rvar,		/* in:  basic struct of new variable	*/
    GENPTR		old_vec,	/* in:  val vector to move from		*/
    ALIGN		*s		/* in/out: restricted storage block	*/

 )
    {
    COUNT	vsize;		/* size of one element			*/
    GENPTR	newval;		/* value pointer in storage		*/
    GENPTR	oldval;		/* original value pointer		*/
    TEXT **	sp;		/* pointer to current string pointer	*/
    TEXT *	rs;		/* string storage (restricted)		*/
    COUNT	i;

    vsize = Vm_ValueSize((*rvar).v_type);	/* size of one value component	     */
    /* if variable has no value, allocate zero bytes:			     */
    vsize = ((*rvar).v_count < 0) ? 0 : (*rvar).v_count * vsize;	
    oldval = old_vec;			/* pointer to existing values	     */
    newval = r_alloc(s, vsize);		/* pointer to restricted values	     */
    if (newval == NULL) return(NULL);
    bytmov(oldval, newval, vsize);	/* move values to restricted	     */
    if ((*rvar).v_type == V_STRING)
	{				/* handle string values...	     */
	sp = (TEXT **) newval;		/* pointer to first string pointer   */
	for (i=0; i < (*rvar).v_count; i++, sp++)
	    {
	    if (*sp == NULL) continue;
	    rs = r_alloc(s, s_length(*sp)+1);	/* allow for EOS	     */
	    if (rs == NULL) return(NULL);
	    s_copy(*sp, rs);		/* copy string to restrictd storage  */
	    *sp = rs;			/* update newval pointer	     */
	    }
	}
    return(newval);
    }

