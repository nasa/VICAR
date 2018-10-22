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



/* TPAM CHECKOUT FILE_TIME=27-MAY-1987 11:46 DUA1:[TAEV2.OLB.GENLIB]TRANSSYM.C;3 */
/* TPEB CHECKOUT FILE_TIME=17-SEP-1984 13:15 DUA0:[TAEV1.OLB]TRANSSYM.C;9 */
/* TPEB CHECKOUT FILE_TIME=12-OCT-1983 11:58 DUA0:[TAEV1.OLB]TRANSSYM.C;8 */
/*TDM         CHECKOUT FILE_TIME=13-JUL-1983 16:00 DUA0:[TAEV1.OLB]TRANSSYM.C;5 */
/*
 *	transsym.  Contains functions for transforming symbol tables
 *	to/from relative and absolute pointers.  
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	12-aug-83	Check pointers before using them in makeabs()...dm
 *	09-sep-83	Allow null v_cvp in makeabs...palm
 *	12-oct-83	Fix GENPTR declarations...palm
 *	14-sep-84	PR 639: makerel, makeabs to save special ptrs...peb
 *	07-nov-84	Alphabetize, TCL 67: handle parm qualifiers...peb
 *	13-nov-84	TCL 117: Handle v_nref...peb
 *	20-nov-84	TCL 67: Use v_pv12...peb
 *	29-mar-87	In makeabs, we use LARGE_P_BYTES for the estimated
 *			max pool size so we can take large PARBLKs...palm
 *	14-jul-87	Process qualifiers unconditionally, not just
 *			for PARMs...palm
 *      07-sep-88       Removed upper bound check because ULTRIX
 *                      does not like it...tpl
 *	26-jan-89	new logic for POINTER_VALIDS, the new valid
 *			structure...palm
 */

#include	"taeconf.inp"
#include	"tminc.inc"
#include	"resinc.inc"		/* inc for restricted allocation*/
#include	"parblk.inc"		/* parameter block structure    */
#include "taeintproto.h"




/*
 *	makeabs. Converts all pointers in a symbol table to absolute so
 *	that the table can be easily manipulated.  We assume that the
 *	symbol table was previously made "all pointers relative".
 *
 *	If a bad link is found, we stop the makesabs process to avoid
 *	crashing.   Such can only happen for blocks from the outside
 *	world (disk or subproces) and, for such blocks, the chk_parblk
 *	function will catch the error.
 *
 *	Handles the special pointers.
 *
 *	TAE versions 1.2 and earlier did use parameter qualifier symbol
 *	tables, nor parameters with type=NAME.  If the v_pv12 bit of 
 *	the variable structure is TRUE, the variable was created by a TAE
 *	version post V 1.2.
 */

FUNCTION VOID makeabs
(
	struct SYMTAB	*symtab,		/* input/output: symbol table*/
	ALIGN		*s			/* input: pooled storage area*/
	
 )
	{
	struct VARIABLE	*v;			/* working VARIABLE pointer  */
	COUNT		i;
    	GENPTR		pool1;
	GENPTR		vgen;
	TEXT 	   	**vc;			/* ptr to string ptr vector  */
	GENPTR		cvp;
	GENPTR		gen;
	struct S_VALID  *svalid;
	BOOL		deref_default;		/* TRUE if deref'd PARM...	*/
						/* DEFAULT -- compiled PDFs only*/
        
	pool1 = (GENPTR)s ;			/* lower limit of block	     */
	SET_ABS((*symtab).link, s, struct VARIABLE *); /* set initial link   */
	for (v=(*symtab).link; v != NULL; v=(*v).v_link)
	    {
	    vgen = (GENPTR) v;			/* generic form of v for...  */
	    if (vgen < pool1)
		break;				/* stop here to avoid crash  */
	    SET_ABS((*v).v_link, s, struct VARIABLE *);
	    deref_default = ((*v).v_class == V_PARM  &&  (*v).v_deref);
	    if ((*v).v_class == V_GLOBAL)	/* fix reqd on defin PDF ptr?*/
		{
		SET_ABS((*v).v_pdf, s, struct DEFPDF *);
		gen = (GENPTR) (*v).v_pdf;
		if (gen != NULL && (gen < pool1 ))
		    break;		
		}
	    if ((*v).v_type == V_NAME)		/* NAME parms only from compiled*/
		{
		if (!(*v).v_pv12)		/* no NAME parms from old TAE vers*/
		    break;
		SET_ABS((*v).v_nref, s, TEXT *);
		gen = (GENPTR) (*v).v_nref;
		if (gen != NULL && (gen < pool1))
		    break;		
		}
	    else				/* not NAME parm		*/
		{
		SET_ABS((*v).v_valid, s, GENPTR);
		gen = (*v).v_valid;
		if (gen != NULL && (gen < pool1 ))
		    break;		
#ifdef POINTER_VALIDS
		if ((*v).v_type == V_STRING && (*v).v_valid)
		    {
	 	    svalid = (struct S_VALID *) (*v).v_valid;	
		    for (i=0; i < (*svalid).count; i++)
			{
			SET_ABS((*svalid).slist[i].string, s, TEXT *);
			gen = (*svalid).slist[i].string;
			if (gen < pool1)
			    break;
			}
		    }
#endif
		SET_ABS((*v).v_cvp, s, GENPTR);	/* now the current value     */
		cvp = (*v).v_cvp;
		if (cvp != NULL && (cvp < pool1 ))
		    break;			/* stop here to avoid crash  */
		if ((*v).v_type == V_STRING  &&  !deref_default)
		    {
		    vc = (TEXT **) cvp;		/* ptr to current string ptr */
		    for (i=0; i < (*v).v_count; i++)
			{
			SET_ABS(vc[i], s, TEXT *);
			if ((GENPTR) vc[i] < pool1 )
			    break;		
			}
		    }
		SET_ABS((*v).v_dvp, s, GENPTR);	/* now the default value     */
		cvp = (*v).v_dvp;
		if (cvp != NULL && (cvp < pool1 ))
		    break;			/* stop here to avoid crash  */
		if ((*v).v_type == V_STRING  &&  !deref_default)
		    {
		    vc = (TEXT **) cvp;		/* ptr to current string ptr */
		    for (i=0; i < (*v).v_dcount; i++)
			{
			SET_ABS(vc[i], s, TEXT *);
			if ((GENPTR) vc[i] < pool1 )
			    break;		
			}
		    }
		}
	    if ((*v).v_pv12 && (*v).v_qualst.link != NULL)
		makeabs(&(*v).v_qualst, s);	/* recursive - parm qual sym tab*/
	    }
        return;
	}		

/*
 *	makerel.   Make all pointers in a chain of VARIABLE structures relative.
 *	Handles special pointers (i.e., pointer to defining
 *	PDF for globals, valid structures, default value pointers).
 *	
 */

FUNCTION VOID makerel
(
    struct SYMTAB 	*sym,		/* symbol table to set relative.	*/
    ALIGN		*s		/* pointer to storage area		*/

 )
    {
    struct S_VALID  *svalid;
    struct VARIABLE *p;
    struct VARIABLE *v;
    TEXT            **sp;		/* pointer into string value ptrs	*/
    COUNT	    i;
    BOOL	    deref_default;	/* TRUE if deref'd PARM DEAFULT...	*/
					/* -- compiled PDFs only		*/

    v = (*sym).link;			/* pointer to first VARIABLE 		*/
    SET_REL((*sym).link, s);		/* set initial link relative		*/
    for (; v !=NULL; v=p)		/* for each VARIABLE			*/
	{
	p = (*v).v_link;		/* next VARIABLE (save from...)		*/
	SET_REL((*v).v_link, s);	/* make this link relative		*/
	deref_default = ((*v).v_class == V_PARM  &&  (*v).v_deref);
	if ((*v).v_class == V_GLOBAL)	/* if pointer to defining PDF		*/
	    SET_REL((*v).v_pdf, s);
	if ((*v).v_type == V_NAME)	/* NAME parms for compiled PDFs only	*/
	    SET_REL((*v).v_nref, s);	/* ptr to name of ref'd variable	*/
	else				/* not NAME parm			*/
	    {
#ifdef POINTER_VALIDS
	    if ((*v).v_type == V_STRING && (*v).v_valid)
		{
		svalid = (struct S_VALID *) (*v).v_valid;	
		for (i=0; i < (*svalid).count; i++)
		    SET_REL((*svalid).slist[i].string, s);
		}
#endif
	    SET_REL((*v).v_valid, s);		/* fix pointer to VALID struct	*/
	    if ((*v).v_type == V_STRING  &&  !deref_default)
		{				/* fix each string pointer...	*/
		sp = (TEXT **) (*v).v_cvp;	/* pointer to first string pointer*/
		for (i=0; i < (*v).v_count; i++)
		    SET_REL(sp[i], s);
		sp = (TEXT **) (*v).v_dvp;	/* ptr to default str ptrs	*/
		for (i=0; i < (*v).v_dcount; i++)
		    SET_REL(sp[i], s);
		}
	    SET_REL((*v).v_cvp, s);		/* make value pointer relative	*/
	    SET_REL((*v).v_dvp, s);		/* and for default val ptr	*/
	    }
	if ((*v).v_pv12)
	    makerel(&(*v).v_qualst, s);	/* recursive -- parm qual sym tab*/
	}
    return;
    }	
