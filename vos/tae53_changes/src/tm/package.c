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



/* TPL CHECKOUT FILE_TIME=21-AUG-1987 21:30 DUA1:[TAEV2.TM.COMMON]PACKAGE.C;5 */
/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:49 DUA1:[TAEV2.TM]PACKAGE.C;4 */
/* TNHE CHECKOUT FILE_TIME= 9-SEP-1986 14:17 DUA1:[TAEV1.TM]PACKAGE.C;2 */
/* TLA CHECKOUT FILE_TIME=20-NOV-1984 19:47 DUA1:[TAEV1.TM]PACKAGE.C;1 */
/* TPEB CHECKOUT FILE_TIME=17-SEP-1984 13:19 DUA0:[TAEV1.TM]PACKAGE.C;20 */
/* TPEB CHECKOUT FILE_TIME= 7-MAY-1984 16:07 DUA0:[TAEV1.TM]PACKAGE.C;19 */
/*TPAM        CHECKOUT FILE_TIME=11-JUL-1983 19:59 DUA0:[TAEV1.TM]PACKAGE.C;15 */
/*
 *	package.   Copies all current variables into a storage block
 *	so that the block may be transfered into another address space.
 *
 *	CAUTION: VARIABLE structures allocated in restricted storage
 *	are slightly different than normal VARIABLEs: the value vector
 *	is allocated with v_count elements, not v_maxc elements (as we
 *	do for dynamically allocated VARIABLEs).   We do this to save
 *	precious space in the interprocess mailbox.
 *
 *	CHANGE LOG:
 *	
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	20-aug-83	Nullables; PR 242 fixed;...palm
 *	11-oct-83	Make vmove external...palm
 *	15-feb-84	Remove the clearing of .msgtyp...palm
 *	07-may-84	'toprefs'-->'numrefs'...nhe
 *	20-aug-84	PR 639:  add "specials" to vmove()...peb
 *	10-sep-84	PR 639 (cont.): new seqs to vmove calls...peb
 *	07-nov-84	TCL 67: vmove to move parameter qualifiers...peb
 *	09-nov-84	PR 880: maintain v_wasname bit...palm
 *	27-dec-84	PR 863: set v_pv12 flag in vmove()...lia
 *	09-sep-86	PR 1116: globals & locals for int procs...nhe
 *	28-mar-87	Add new package argument: the pool size so that
 *			it can be called with different sized PARBLKs...palm
 *	13-jul-87	Locals with qualifiers.  Here, the change is to
 *			always package the v_qualst symbol table, not
 *			just for V_PARMs...palm
 *      08-sep-87       Add size argument to pack_parm...tpl
 *	26-feb-88	Fix vmove's recursive call to pass on the specials
 *			flags so that qualifier's valids get copied; previously 
 *			valids of qualifiers were not getting SAVEd or 
 *			COMPILEd...palm
 *	01-dec-88	New mode flag to package...palm
 *	26-jan-89	Handle new POINTER_VALIDS...palm
 *	02-feb-89	Fix above...palm
 */

#include	"taeconf.inp"
#include	"resinc.inc"		/* inc for restricted allocation*/
#include	"symtab.inc"
#include	"parblk.inc"		/* parameter block def		*/
#include	"fileinc.inp"		/* file primitive		*/
#include	"tminc.inc"
#include "taeintproto.h"


FUNCTION struct VARIABLE * vmove
(
    struct VARIABLE *p,		/* input: VARIABLE struct to move	*/
    ALIGN	    *s,		/* input: restricted storage block	*/
    FUNINT          specials	/* input: bits indicating special...	*/
    				/*	  parts of VARIABLE to move	*/

 );

/*
 *	package.  Builds a PARBLK from context block.  A PARBLK is transmitted
 *	to a subprocess.  Note that we handle internal procs, adding variables
 *	from outer nesting as we go.  For performance reasons, we do not
 *	check for duplicate names; we assume the receiver can unravel
 *	in proper order (i.e., fifo)
 *
 */
FUNCTION CODE package
(
   	struct CONTXT	*ctx,		/* input: current context block	*/
    	struct PARBLK 	*par,		/* output: parameter block	*/
	FUNINT	        pool_size,	/* input: size of PARBLK.pool   */
	FUNINT		scope		/* input: VM_VALID or zero      */

 )
    {
    struct VARIABLE	*p;		/* working pointer		*/
    struct VARIABLE	*pcur;		/* current VARIABLE struct	*/
    COUNT		i; 
    struct CONTXT	*tctx;		/* temp'y current context block	*/

    r_init((*par).pool, pool_size);	/* init storage area		*/
    (*par).symtab.link = NULL;		/* in case no variables		*/
    pcur = (struct VARIABLE *) &(*par).symtab;  
    /* first loop on internal procs, collecting parms, locals, ref globals */
    for (tctx=ctx; s_equal((*tctx).pdf.libr,"/LOCAL/"); tctx=(*tctx).backlink)
    	{
	for (p=(*tctx).parmst.link; p!=NULL; p=(*p).v_link)
	    {	
	    if ((*p).v_type == V_NAME && (*p).v_ref == NULL)
		continue;
	    pcur = (*pcur).v_link = vmove(p, (*par).pool, scope);  /* move and link in*/
	    if (pcur == NULL) return (FAIL);
	    }
	for (p=(*tctx).locst.link; p!=NULL; p=(*p).v_link)
	    {				/* for each local variable:	*/
	    pcur = (*pcur).v_link = vmove(p, (*par).pool, scope);  /* move and link in	*/
	    if (pcur == NULL) return (FAIL);
	    }
	for (i=0; i < (*tctx).numrefs; i++)
	    {				/* for each ref'd global	*/
	    pcur = (*pcur).v_link = vmove((*tctx).refs[i], (*par).pool, scope);  /* move and link in*/
	    if (pcur == NULL) return (FAIL);
	    }
    	} 
    /* now collect them for parent proc (i.e., the PDF level)		*/
    for (p=(*tctx).parmst.link; p!=NULL; p=(*p).v_link)
	{	
	if ((*p).v_type == V_NAME && (*p).v_ref == NULL)
	    continue;
	pcur = (*pcur).v_link = vmove(p, (*par).pool, scope);  /* move and link in*/
	if (pcur == NULL) return (FAIL);
	}
    for (p=(*tctx).locst.link; p!=NULL; p=(*p).v_link)
	{				/* for each local variable:	*/
	pcur = (*pcur).v_link = vmove(p, (*par).pool, scope);  /* move and link in	*/
	if (pcur == NULL) return (FAIL);
	}
    for (i=0; i < (*tctx).numrefs; i++)
	{				/* for each ref'd global	*/
	pcur = (*pcur).v_link = vmove((*tctx).refs[i], (*par).pool, scope);  /* move and link in*/
	if (pcur == NULL) return (FAIL);
	}
    (*par).last = TRUE;			/* only one block for now	*/
    makerel(&(*par).symtab, (*par).pool);/* make all pool pointers relative*/
    (*par).blksiz = r_top((*par).pool) - (GENPTR)par ;  /* logical block size */
    return(SUCCESS);			
    }

/*
 *	pack_parm.  Build PARBLK for one symbol table only.  This
 *	is typically called by TUTOR to build a PARBLK for the
 *	SAVE command.
 *
 *	Return codes:
 *		SUCCESS
 *		FAIL -- not enough room in PARBLK for all the parms
 */
    
FUNCTION CODE pack_parm
(
    struct SYMTAB	*symtab,	/* in: symbol table to use	*/
    struct PARBLK	*p,		/* out: PARBLK			*/
    FUNINT              pool_size      /* in: pool space size          */
 )
    {
    struct VARIABLE *pv;		/* ptr to current PARBLK VARIABLE*/
    struct VARIABLE *v;			/* ptr to current input VARIABLE */

    r_init((*p).pool, pool_size);		/* initialize pool		*/
    (*p).symtab.link = NULL;		/* PARBLK initially empty	*/
    pv = (struct VARIABLE *) & (*p).symtab;
    for (v=(*symtab).link; v != NULL; v=(*v).v_link)
        {
        if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
        pv = (*pv).v_link = vmove(v, (*p).pool, 0);  /* move and link in	*/
        if (pv == NULL) return (FAIL);
        }
    (*p).last = TRUE;
    makerel(&(*p).symtab, (*p).pool);	/* make pointers relative	*/
    (*p).blksiz = r_top((*p).pool) - (GENPTR) p;
    return (SUCCESS);
    }

/*
 *	valmove.  Move a value vector into restricted storage and
 *	return its pointer.
 */

FUNCTION GENPTR valmove 
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

    vsize = valsize((*rvar).v_type);	/* size of one value component	     */
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

/*
 *	vmove.   Move VARIABLE structure to restricted storage and
 *	return its pointer.  Allows special parts of the variable
 *	structure to be moved, by request.  The allowed special parts
 *	are the PDF name (VM_PDF),
 *	the valid structure (VM_VALID), the default value pointer (VM_DVP),
 *	and the name parm name reference for compiled procs (VM_NMREF).
 *	The parameter qualifiers are not considered special parts
 *	but are always moved.
 */

FUNCTION struct VARIABLE * vmove
(
    struct VARIABLE *p,		/* input: VARIABLE struct to move	*/
    ALIGN	    *s,		/* input: restricted storage block	*/
    FUNINT          specials	/* input: bits indicating special...	*/
    				/*	  parts of VARIABLE to move	*/

 )
    {
    struct VARIABLE *rp;	/* pointer into restricted storage	*/
    struct VARIABLE *up;	/* unresolved version of p		*/
    struct VARIABLE *tv;	/* target variable in loop		*/
    struct VARIABLE *v;		/* source variable in loop		*/
    TINY	    class;	/* V_GLOBAL, V_PARM, etc.		*/
    struct R_VALID  *vdin;	/* pointer to valid structure in p	*/
    COUNT	    bytes;
    COUNT	i;
    struct S_VALID *validIn;
    struct S_VALID *validOut;

    if (specials & VM_NMREF)	/* don't resolve in this case		*/
	up = p;
    else
	{
	up = p;			/* unresolved version of p		*/
	p = RESOLVE(up);	/* resolve indirection			*/
	}
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

    /* now move the parameter qualifier symbol table	*/

    tv = (struct VARIABLE *) &(*rp).v_qualst;
    for (v=(*p).v_qualst.link; v != NULL; v=(*v).v_link)
	{
	if ((*v).v_type == V_NAME && (*v).v_ref == NULL)
	    continue;
	tv = (*tv).v_link = vmove(v, s, specials);   /* recursive */
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
	(*rp).v_cvp = valmove(rp, (*p).v_cvp, s);	/* move current vals	*/
	if ((*rp).v_cvp == NULL) return(NULL);
	}
    if ((specials & VM_DVP)  &&  (*p).v_dvp != NULL)	/* if dflts requested*/
	{
	(*rp).v_dvp = valmove(rp, (*p).v_dvp, s);	/* move default vals*/
	if ((*rp).v_dvp == NULL) return(NULL);
	}

    /* now the other special parts	*/

    if ((specials & VM_VALID)  &&  (*p).v_valid != NULL) /* if valid requested*/
	{
	vdin = (struct R_VALID *) (*p).v_valid;
	bytes = valid_size((*rp).v_type, (*vdin).count);
	(*rp).v_valid = r_alloc(s, bytes);
	if ((*rp).v_valid == NULL) return(NULL);
#ifdef POINTER_VALIDS
	validIn  = (struct S_VALID *) (*p).v_valid;
	validOut = (struct S_VALID *) (*rp).v_valid;
        bytmov ((GENPTR)validIn, (GENPTR)validOut, bytes);		/* handles .count */
	if ((*p).v_type == V_STRING)
	    {
            for (i=0; i < (*validIn).count; i++)
		{
		TEXT *str = r_alloc (s, s_length((*validIn).slist[i].string)+1);
		if (str == NULL)
		    return (NULL);
		(*validOut).slist[i].string = str; 
		s_copy((*validIn).slist[i].string, str);
		}
	    }
#else
	cpy_vld((*p).v_type, (*p).v_valid, (*rp).v_valid);
#endif
	}
    if (class == V_GLOBAL)
	{
	(*rp).v_pdf = NULL;			/* formality			*/
	if ((specials & VM_PDF)  &&  (*p).v_pdf != NULL)
	    {
	    (*rp).v_pdf = (struct DEFPDF *) r_alloc(s, sizeof(struct DEFPDF));
	    if ((*rp).v_pdf == NULL) return(NULL);
	    MOVE_STRUCT(*(*p).v_pdf, *(*rp).v_pdf);
	    }
	}
    return (rp);
    }
