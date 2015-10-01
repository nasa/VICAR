/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/*
 *	TM subroutines.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	19-jul-83	Move parcmp function here from tutor.c...peb
 *	21-aug-83	Nullables...palm
 *	09-sep-83	More thorough chk_parblk (for cvp)...palm
 *	10-oct-83	Fix unix lint/compilation errors...palm
 *	18-oct-83	Fix lingering & problem L760...palm
 *	23-nov-83	Add routine put_outmsg, invoke it from m_put...dm
 *	15-feb-84	Fix opn_stdout bug, update hierarchy for mdf's...dm
 *	26-feb-84	Increase m_pfmt local buffer size to 2*STRINGSIZ...dm
 *	01-mar-84	Fix chk_parblk to check bytsiz against
 *			sizeof (PARBLK) not P_BYTES...palm
 *	01-mar-84	Add list_missing...palm
 *	06-mar-84	Check count for STDOUT qualif...peb
 *	19-mar-84	Remove blank line in oprattn()...dm
 *	26-apr-84	Add conditional for UNIX...dm
 *	04-may-84	VALUE_x to xVAL, replace getgst with s_copy, and
 *			add set_string ... ces
 *	07-may-84	Move the IMPORT declarations...lim
 *	17-may-84	New hierarchy and f_crack calling sequences...palm
 *	24-jul-84	Check $SYSLIB/$USERLIB set to NULL...lia
 *	02-aug-84	"Terminal Monitor" --> "TAE Monitor" (PR396)...peb
 *	15-aug-84	Remove msg argument from addproc calls...lia
 *	17-sep-84	PR 639: chk_parblk to check special ptrs...peb
 *	11-oct-84	Check run_type (not termtype) in put_outmsg as
 *			implemented for UNIX 1.2...dm
 *	25-oct-84	TCL 117: tmmsg -- no line # for compiled PDFs...peb
 *	26-oct-84	TCL 117: hierarchy -- user spec'd dflt type...peb
 *	29-oct-84	TCL 117: hierarchy - no dflt type for explic libr...peb
 *	14-nov-84	TCL 117: chk_parblk - allow v_type=NAME...peb
 *	20-nov-84	TCL 67: use v_pv12...peb
 *	25-nov-84	TCL 67: add upd_p_qlf function...peb
 *	28-nov-84	TCL 67: parcmp,parmrg to handle parm qualifs...peb
 *	04-dec-84	Conditionalize functions needed by rcjm agent...dm
 *	11-dec-84	TCL 97: check onfail cmd in execution flag for error
 *			message...lia
 *	18-dec-84	Shorten appended msg for onfail cmd...lia
 *	20-dec-84	In set_p_qlf, make sure VARIABLE from post V 1.2...peb
 *	22-dec-84	PR 863: in chk_p_var add logic to clear bad valid flag
 *			from PARBLK created by TAE version 1.2 or before...lia
 *
 ******************************************************************************
 * CHNGES MADE IN THE RCJM TREE:
 *
 *	07-mar-85	Remove m_pfmt and chk_parblk to tae library...dm
 *
 ****************************************************************************
 *
 *	25-jul-85	Fix UNIX lint compilation errors...dm
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	14-oct-85	Fix rest_parm() to read multiple PARBLKs if needed...lia
 *	08-nov-85	PR 1088: Changed 'upd_p_qlf' to allow positional params
 *			to be specified after qualifiers...dab
 *	11-sep-86	Change error message for bad onfail command...nhe
 *	13-nov-86	Added TAE_FACELIFT code...krw
 *	27-mar-87	Honor VALIDs in parmrg so that outboard process can
 *			set VALIDs dynamically (TAE V2.2 feature)...palm
 *	27-mar-87	Fixed parcmp to compare against proper valids...palm
 *	28-mar-87	New search_vector...palm
 *	23-apr-87	In parmrg, only do VALID merging when requested.
 *			This restricts valid merging to dynget...palm
 *	15-jul-87	PR 1252: Allow greater than 132 bytes of 
 *			qualifiers...lia
 *	14-aug-87	Allow locals to be merged into a parm with the
 *			same name, but no complaints if the corresponding
 *			parm does not exist; also, more judicious use
 *			of a "placeholder" type in parcmp...palm
 *      20-nov-87       Display error in menu panel if menu mode...tpl
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl
 *	12-feb-88	PR 1414: Use LARGE_PARBLK in rest_parm...ljn
 *	24-feb-88	PR 1504: Change label arg in cmd_parse call...ljn
 *	03-jun-88	Suppress tmmsg output if local _MESSAGE is defined
 *			and place the message there instead...palm
 *	26-jan-89	New POINTER_VALIDS logic...palm
 *	08-feb-89	Use new parhdrRead function...palm
 *	14-feb-89	tmmsg: don't set lastkey unles substantial key...palm 
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	03-nov-89	Added compiling parameter to chk_vector call...krw
 *	27-jun-90	Remove Facelift code...ljn
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *	13-may-93	LARGE_PARBLK is larger then max VMS record size.
 *			Use Vm_parbwrite and Vm_parbread instead of f_bwrite
 *			and f_bread to work-around this limitation...cew
 *	16-feb-94	PR2621: we were calling delvar then referencing
 *			deleted VARIABLE to get its link.  This is illegal.
 */

#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"tmhost.inp"	/* host-dependent			*/
#include	"parblk.inc"	/* Parameter block			*/
#include	"terminc.inc"	/* terminal support			*/
#include	"syninc.inc"	/* syntax package structs & defs	*/
#include 	"tminc.inc"	/* TM defs				*/
#include "taeintproto.h"

FUNCTION  VOID  oprattn(void);

FUNCTION static CODE open_try
(
    struct SFILE	*sfile,			/* in/out: SFILE to use	*/
    FUNINT		lun,			/* in: lun to use	*/
    struct FSBLOCK	*fsblock		/* in: file spec block	*/

 );

FUNCTION CODE opn_stdout
(
    TEXT		spec[],			/* in: file spec		*/
    FUNINT		access			/* in: standard open access	*/

 );
    static TEXT		msg_mmov[] = "TAE Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/


/* 
 *	Generate and check a special VARIABLE.v_class flag for use
 *	by parcmp in generating an "unknown" class.  Parcmp uses
 *	this as a flag that the variable is not a candidate for
 *	merging.
 */

#define NO_CLASS  (V_PARM+10)
#if NO_CLASS == V_PARM || NO_CLASS == V_GLOBAL || NO_CLASS == V_LOCAL
    intentional compilation error: NO_CLASS value not selected very well.
#endif


/*	Similar exercise for SELECTED_TYPE_BIT for using
 *	VARIABLE.v_type as a flag.
 */

#define SELECTED_TYPE_BIT  0x40
#if (V_REAL|V_INTEGER|V_STRING|V_NAME) & SELECTED_TYPE_BIT
	intentional compilation error: SELECTED_TYPE_BIT not a good value.
#endif



#ifndef TAE_REM_AGENT
/* chg_stdout - Change the standard output file for new process or procedure
 *
 * Returns SUCCESS or FAIL; on FAIL prints an error message
 */
FUNCTION CODE chg_stdout
	(
    struct CONTXT	*procctx		/* in: current proc context	*/

	 )
    {
    IMPORT struct SFILE	  *pstdo_fil;		/* pointer to stdout SFILE	*/
    IMPORT struct SFILE	  stdo_fil;		/* stdout SFILE			*/

    CODE		code;
    struct VARIABLE	*p;


/* Close the old file and, for procedures, open the new		*/

    if((*procctx).proctype == Y_PROCESS)
    	{
        if (pstdo_fil != NULL)
    	    {
    	    f_close(pstdo_fil, F_KEEP);
    	    pstdo_fil = NULL;		/* force dynamic param stdout to term */
    	    }
    	}
    else if ((*procctx).proctype == Y_PROCEDURE)
    	{
        p = lookex(&(*procctx).locst, "_STDOUT");
    	if (s_equal(SVAL(*p,1), "CREATE"))	/* new file?		*/
    	    {
    	    if (pstdo_fil != NULL)
    		f_close(pstdo_fil, F_KEEP);	/* close old		*/
    	    code = opn_stdout(SVAL(*p,0), F_WRITE);	/* open new	*/
    	    if (code != SUCCESS)
    		{
    		tmmsg(PROCFAIL, "Unable to open standard output. %s.",
		      "TAE-STDOOPN", (uintptr_t) stdo_fil.errmsg, 0, 0, 0, 0);
    		return(FAIL);
    		}
    	    }
    	}
    return(SUCCESS);
    }


/*
 *  list_missing.   List missing parameter names.
 *
 *	Return codes:
 *		SUCCESS  -- ok and no message generated
 *		FAIL -- some mandatory parm missing and message generated
 */

FUNCTION  CODE  list_missing 
(
    struct  SYMTAB  *symtab		/* in: pointer to symbol table	*/

 )
    {

    TEXT	mislst[STRINGSIZ+1];
    COUNT	m;

    m = misprm(symtab, mislst);		/* get missing parameters	*/
    if (m != 0 )			/* if any missing 		*/
        {
	tmmsg(PROCFAIL, "Missing parameter%s:  %s.", "TAE-MISPAR",
	      (uintptr_t) (m > 1 ? "s" : ""),  (uintptr_t) mislst, 0, 0, 0);
        return (FAIL);
        }
    return(SUCCESS);			/* none missing			*/
    }


/*	parcmp. Compare variables in symtab st1 to symtab st2.
 *
 *	No value merging is done here (this prevents partial merging
 *	before we know all symbols in 1st sym tab are OK).  Any
 *	superfluous or mismatched st1 PARM variables are flagged with
 *	v_class == NO_CLASS so that they won't be used in the
 *	actual "parameter merge" (parmrg).
 *
 *	Variables of class V_LOCAL or V_GLOBAL are used if a 
 *	target variable exists.  If no target, we do not 
 *	complain.  For V_PARMs, we do complain if no target. The thinking
 *	here is that if you name the wrong PAR file on a restore,
 *	you should be told in some way;  if, however, a local
 *	named _PROC doesn't match a target variable, we don't 
 *	want an error message.
 *
 *	BEWARE: this sets "bad" VARIABLEs in st1 to have v_class = NO_CLASS
 *	so that parmrg will ignore such VARIABLEs.  So after calling
 *	this, your st1 symbol table may have "bad" variables that might 
 *	crash TM.
 *
 *	Return codes:
 *
 *		SUCCESS
 *		SOME_REJECTED if any st1 variables were superfluous 
 *			      or mismatched.
 */

FUNCTION CODE parcmp 
(
    struct SYMTAB	*st1,		/* in:  1st symbol table		*/
    struct SYMTAB	*st2,		/* in:  2st symbol table		*/
    TEXT		errmsg[STRINGSIZ+1]	/* out: error msg string	*/

 )
    {
    IMPORT COUNT	termcols;	/* number of columns if CRT		*/
    IMPORT CODE		termtype;	/* terminal type		*/
    struct VARIABLE	*v1, *v2;
    BOOL		badpar;
    CODE		code;
    CODE		chk_code;
    CODE		qual_code;
    TEXT		dummy_msg[STRINGSIZ+1];
    BOOL		still_adding;	/* TRUE if still adding to error msg */
    COUNT		len;
    COUNT		maxlen;

    code = SUCCESS;
    still_adding = TRUE;
    maxlen = (termtype == T_CRT)  ?  termcols : STRINGSIZ;
    maxlen -= KEYSIZ + 1;
    s_copy("Not used or invalid: ", errmsg);
    for (v1 = (*st1).link; v1 != NULL; v1 = (*v1).v_link)
	{
        v2 = lookex (st2, (*v1).v_name);	/* get target variable */
        if (v2 == NULL && (*v1).v_class != V_PARM)
    	    continue;				/* if not parm: no complaint  */
    	if (v2 == NULL)
	    badpar = TRUE;			/* if parm: complain          */
	else
	    {
    	    if ((*v2).v_type == V_NAME)		/* ignore if NAME parm	      */
    		continue;
	    if ((*v1).v_count < 0)
		{
		badpar = FALSE;
		if ((*v1).v_type != (*v2).v_type)
		    badpar = TRUE;
		}
	    else
	        {
		chk_code = chk_vector(v2, (*v1).v_type, (GENPTR)(*v1).v_cvp,
		    (*v1).v_count, FALSE);
		/*	note: we set badpar=TRUE on VALID mismatch     */
		badpar = chk_code != V_BADVALID  &&  
			 chk_code != V_BADAMBIG  &&
			 chk_code != SUCCESS ;
	        }
	    if (!badpar  &&  (*v1).v_pv12  &&  (*v2).v_pv12)	/* if parms...	*/
		{				/* from post V1.2 TAE		*/
		qual_code = parcmp(&(*v1).v_qualst,	/* recursive - parm quals*/
				&(*v2).v_qualst, dummy_msg);
		if (qual_code != SUCCESS)	/* append differently for quals	*/
		    {
		      if (still_adding) {
			if (s_length((*v1).v_name) + s_length(errmsg) + 18
			    > maxlen)
			    {
			    s_append("...", errmsg);
			    still_adding = FALSE;
			    }
			else
			    {
			    s_append((*v1).v_name, errmsg);
			    s_append(" qualifier(s)", errmsg);
			    s_append(", ", errmsg);
			    }
		    code = SOME_REJECTED;
		    (*v1).v_class = NO_CLASS;	/* force unrecognized
						   class */
		      }
		    }
		}
	    }
	if (badpar)
	    {
	      if (still_adding) {
		if (s_length((*v1).v_name) + s_length(errmsg) + 5 > maxlen)
		    {
		    s_append("...", errmsg);
		    still_adding = FALSE;
		    }
		else
		    {
		    s_append((*v1).v_name, errmsg);
		    s_append(", ", errmsg);
		    }
	    (*v1).v_class = NO_CLASS;	/* force an unrecognized class so it's not merged*/
	    code = SOME_REJECTED;
	      }
	    }
	}
    if (code == SOME_REJECTED  &&  still_adding)
	{
	len = s_length(errmsg);
	errmsg[len-2] = EOS;			/* strip trailing "' "		*/
	}
    return(code);
    }

/*	parmrg. Merge parameters from symtab st1 into symtab st2.
 *
 *	If mode is SUBSET then parms from st2 which are not
 *	in 1st are deleted.  No error checking: parcmp must have been
 *	called first. 
 *
 *	Returns FAIL only on memory overflow.
 */

FUNCTION CODE parmrg 
(
    struct SYMTAB	*st1,		/* in:  1st symbol table		*/
    struct SYMTAB	*st2,		/* in/out: sym tab to merge into	*/
    FUNINT		mode		/* in:  FULLSET or SUBSET	*/
					/* VM_VALID may be or'd here to */
					/* request VALID merging	*/

#if  (VM_VALID & FULLSET) || (VM_VALID & SUBSET)
	This is an intentional compilation error to account for
	the fact that VM_VALID, FULLSET, and SUBSET cannot have
	any conflicting bits: we allow VM_VALID as a bit flag
	in the mode argument.
#endif

 )
    {
    struct VARIABLE	*v1, *v2;
    struct S_VALID	*valid1;
    struct S_VALID	*valid2;
    BOOL		settable;
    CODE		code;
    COUNT		i;
    COUNT		maxLength, length;
    TEXT		*s1;
    TEXT		*s2;
	
    for (v1 = (*st1).link; v1 != NULL; v1 = (*v1).v_link)
	{
	v2 = lookex(st2, (*v1).v_name);
        if (v2 == NULL || (*v1).v_class == NO_CLASS)	 /* if v1 not useable */
    	    continue;		
	if ((valid1 = (struct S_VALID *) (*v1).v_valid) != NULL  
		&& mode&VM_VALID)
	    {
#ifdef	POINTER_VALIDS
	    valid2 = (struct S_VALID *) (*v2).v_valid;
	    if (valid2  &&  (*v2).v_type == V_STRING)
		for (i=0; i < (*valid2).count; i++)
		    tae_free ((*valid2).slist[i].string);
#endif
	    tae_free ((*v2).v_valid);		/* free v2 valids if any*/
	    valid2 = (struct S_VALID *) allleg ((struct VARIABLE *) v2, 
			     (*valid1).count);  /* alloc/link new struct*/
#ifdef POINTER_VALIDS
	    if ((*v2).v_type == V_STRING)
		{
	        (*valid2).count = (*valid1).count;
		maxLength = 0;
	        for (i=0; i < (*valid2).count; i++)
		    {
		    s1 = (*valid1).slist[i].string;
		    length = s_length (s1);
		    if (length > maxLength) 
			maxLength = length;
		    s2 = (*valid2).slist[i].string = tae_alloc (1, length+1);
		    s_copy (s1, s2);
	            }
	        (*v2).v_size = maxLength;
		}
	    else				/* for real and integer */
	      bytmov ((GENPTR) valid1, (GENPTR) valid2, 
				valid_size ((*v2).v_type, (*valid1).count));
#else
	    bytmov (valid1, valid2, 
				valid_size ((*v2).v_type, (*valid1).count));
#endif 
	    }
	settable = ((*v2).v_type != V_NAME   &&   (*v1).v_count >= -1);
	if (settable)
	    {
	    code = set_value(v2, (*v1).v_cvp, (*v1).v_count);
	    if (code != SUCCESS) goto over_err;
            if ((*v1).v_class == V_PARM && (*v2).v_class == V_PARM)
		(*v2).v_default = (*v1).v_default;	
	    if ((*v1).v_pv12 &&  (*v2).v_pv12)
		{
		code = parmrg(&(*v1).v_qualst,
			&(*v2).v_qualst, mode);	/* recursive - merge parm quals	*/
		if (code != SUCCESS)
		    return(FAIL);
		}
	    }
	if ((mode&~VM_VALID) == SUBSET)
	    (*v2).v_type |= SELECTED_TYPE_BIT;	/* temp flag for "selected"  */
	}
    if ((mode&~VM_VALID) == SUBSET)		/* if SUBSET, exclude some   */
	{
	for (v2 = (*st2).link; v2 != NULL;)
	    {
	    struct VARIABLE *v_link = (*v2).v_link; /* PR2621 */
	    if ((*v2).v_type & SELECTED_TYPE_BIT) 	/* if selected       */
		(*v2).v_type &= ~SELECTED_TYPE_BIT;	/* turn off flag     */
	    else
		delvar(st2, v2);		/* remove exluded variable   */
	    v2 = v_link;
	    }
	}
    return(SUCCESS);

over_err:
    tmmsg(PROCFAIL, msg_mmov, key_mmov, 0, 0, 0, 0, 0);
    return(FAIL);
    }

/*
 *	pos_scroll. Position for scroll mode
 */

    FUNCTION  VOID  pos_scroll(void)

    {

    IMPORT  COUNT  termlines;		/* number of lines on terminal  */

    t_pos(termlines, 1);		/* position at bottom of screen */
#ifdef UNIX
    t_write("", T_STDCC);		/* scroll up one line		*/
#endif
    return;
    }


/*	re_stdout - reset standard output
 *
 *	returns SUCCESS or FAIL
 *
 */
FUNCTION CODE re_stdout
(
    struct CONTXT	*cmdctx	/* in: just completed 'new' context	*/

 )
    {
    IMPORT struct SFILE	  *pstdo_fil;	/* pointer to stdout SFILE	*/
    IMPORT struct SFILE	  stdo_fil;	/* stdout SFILE			*/

    struct CONTXT	*procctx;	/* the context we're returning to	*/
    CODE		code;
    CODE		proctype;
    struct VARIABLE	*p;


    procctx = (*cmdctx).backlink;		/* point to previous context	*/
    if ((proctype = (*cmdctx).proctype) == Y_PROCEDURE)
    	{
    	p = lookex(&(*cmdctx).locst, "_STDOUT");
    	if (!(s_equal(SVAL(*p,1), "CREATE"))) /* if we had not opened a new file	*/
    	    return(SUCCESS);
    	if (pstdo_fil != NULL)			/* close the 'new' stdout	*/
    	    f_close(pstdo_fil, F_KEEP);		/* close if not a terminal	*/
    	}
    if (proctype != Y_PROCEDURE && proctype != Y_PROCESS)
    	return(SUCCESS);

/* Now re-open	*/

    p = lookex(&(*procctx).locst, "_STDOUT");	/* point to old stdout		*/
    code = opn_stdout(SVAL(*p,0), F_EXTEND);
    if (code != SUCCESS)
    	{
        tmmsg(PROCFAIL, "Unable to open standard output. '%s'.",
	      "TAE-STDOOPN", (uintptr_t) stdo_fil.errmsg, 0, 0, 0, 0);
        return(FAIL);
    	}
    return(SUCCESS);
    }

/*	rest_parm - perform parameter block restore.
 *
 *	Read parameter file from disk.
 *
 *	NOTE:  errmsg output is policed to STRINGSIZ
 */

FUNCTION CODE rest_parm
(
    TEXT		restspec[],	/* in:  restore file spec or null	*/
    struct CONTXT	*pctx,		/* in/out:  proc context		*/
    TEXT		errmsg[]	/* out: returned error message if fail	*/

 )
    {
    struct LARGE_PARBLK	p;		/* PARBLK to read		*/
    struct SFILE    	f;		/* SFILE structure		*/
    struct PARHDR	ph;    		/* p-file header record		*/
    BOOL		rejected;
    COUNT		recsize;
    CODE 		code;
    CODE		cmprcode;	/* code returned by parcmp	*/
    TEXT		msg[STRINGSIZ+1];
    TEXT		errkey[STRINGSIZ+1];

    s_copy("Error restoring parameter file", errmsg);	/* default	*/
    code = f_opnspc(&f, SAVELUN, restspec, "", (*pctx).pdf.name,
	            PAR_TYPE, F_READ);
    if (code != SUCCESS) goto bad_open;
    if (parhdrRead (&f, &ph, errmsg, errkey) != SUCCESS) 
	return (FAIL);
    rejected = FALSE;
    while (FOREVER)
	{				/* read each record		*/
#if defined(vms) || defined(__VMS)
	code = Vm_parbread(&f, (GENPTR)&p, sizeof(p), &recsize);
#else
	code = f_bread(&f, (GENPTR)&p, sizeof(p), &recsize);
#endif
	if (code != SUCCESS) break;
	makeabs(&p.symtab, p.pool);	/* make pointers absolute	*/
 	if (chk_parblk((struct PARBLK*) &p) != SUCCESS)	/* check internal integrity	*/
	    goto bad_format;
	cmprcode = parcmp(&p.symtab, &(*pctx).parmst, msg);
	if (parmrg(&p.symtab, &(*pctx).parmst, FULLSET) != SUCCESS)
	    goto over_err;
	if (cmprcode == SOME_REJECTED)
	    rejected = TRUE;
	}
    if (code != F_EOF) goto io_error;
    f_close(&f, F_KEEP);
    if (rejected)
	{
	s_copy(msg, errmsg);
	return(SOME_REJECTED);
	}
    return(SUCCESS);

bad_open:
    s_copy("Unable to open parameter file. ", errmsg);
    if ((s_length(errmsg) + s_length(f.errmsg)) < STRINGSIZ)
    	s_append(f.errmsg, errmsg);
    return(FAIL);

io_error:
    f_close(&f, F_KEEP);
    s_copy("Error reading parameter file. ", errmsg);
    if ((s_length(errmsg) + s_length(f.errmsg)) < STRINGSIZ)
        s_append(f.errmsg, errmsg);
    return(FAIL);

bad_format:
    f_close(&f, F_KEEP);
    s_copy("Parameter file is not correctly formatted. ", errmsg);
    return(FAIL);

over_err:
    f_close(&f, F_KEEP);
    return(FAIL);
    }

/*	save_parm - Save a parameter block
 *
 *	Write V-block to disk.
 *
 *	NOTE: errmsg output is policed to STRINGSIZ.
 */

FUNCTION CODE save_parm 
(
    TEXT		savespec[],	/* in:  save file spec		*/
    struct CONTXT	*pctx,		/* in:  proc context		*/
    CODE		access,		/* in:  F_WRITE or F_OVER	*/
					/* (F_OVER used for SAVE LAST)	*/
    TEXT		errmsg[]	/* out: error message if FAIL	*/

 )
    {
    struct SFILE	f;		/* file i/o block		*/
    CODE		code;

    code = f_opnspc(&f, SAVELUN, savespec, "",  (*pctx).pdf.name,
	            PAR_TYPE, access);
    if (code != SUCCESS)
	goto open_err;
    code = save_pfile (&f, 0, NULL, 0, &(*pctx).parmst, 0, 0);
    if (code != SUCCESS)
	goto write_err;
    f_close (&f, F_KEEP);
    return (SUCCESS);

open_err:
    s_copy("Unable to open save file. ", errmsg);
    if ((s_length(errmsg) + s_length(f.errmsg)) < STRINGSIZ)
        s_append(f.errmsg, errmsg);
    return(FAIL);

write_err:
    f_close(&f, F_DELETE);
    s_copy("Error writing save file. ", errmsg);
    if ((s_length(errmsg) + s_length(f.errmsg)) < STRINGSIZ)
        s_append(f.errmsg, errmsg);
    return (FAIL);
    }

/*	search_vector.   Returns TRUE if string value vector
 *	contains a certain string value.
 */

FUNCTION BOOL search_vector 
(
    	TEXT	*string_vv[],		/* in: string value vector */
    	FUNINT	count,			/* in: number of strings   */
    	TEXT	search_string[]	/* in: string to search for */

 )
    {
    COUNT	i;

    for (i=0; i < count; i++)
        {
        if (s_equal (search_string, string_vv[i]))
    	    return (TRUE);
        }
    return (FALSE);
    }

/*
 *	upd_p_qlf - update parameter qualifiers from command line.
 *	The command line syntax block is assumed to be positioned such
 *	that the next non-white character is the opening qualifier
 *	separator, if there are any qualifiers for this parm.
 *
 *	Note that routine tut_pqlf_upd is similar to this routine. The
 *	difference is that this routine outputs error messages to terminal
 *	directly whereas the other routine "holds" them since tutoring
 */

FUNCTION CODE upd_p_qlf 
(
    struct SYNBLK	*sb,		/* in/out: command line syntax block	*/
    struct VARIABLE	*v		/* in/out: parm which the quals apply to*/

 )
    {
    CODE		code;
    TEXT		qualstr[CMDLINSIZ+1];
    struct SYNBLK	loc_sb;

    code = getqlf(sb, qualstr);		/* get parameter qualifiers		*/
    if (code == S_NONE)
	return(SUCCESS);		/* nothing to do if no quals		*/
    if (code != SUCCESS)
	goto qual_err;
    if (!(*v).v_pv12)
	goto notallowed_error;
    fndsep(sb);				/* get past trailing separator if present */
    initok(&loc_sb, qualstr);		/* init parm qual stream for syntax pkg	*/
    code = updtab(&(*v).v_qualst, &loc_sb);	/* update parm qual st using stream*/
    if (code != SUCCESS)
	return (FAIL);
    return(SUCCESS);

qual_err:
    tmmsg(PROCFAIL, (*sb).errmsg, "TAE-BADPQUAL", 0, 0, 0, 0, 0);
    return (FAIL);

notallowed_error:
    tmmsg(PROCFAIL, "Parameter qualifiers are not defined for this proc.",
	  "TAE-BADPQUAL", 0, 0, 0, 0, 0);
    return (FAIL);
    }

/*	set_stdout - set the _STDOUT local variable value
 *
 *	return SUCCESS or FAIL
 *
 * Note:    _STDOUT is a 2-valued string variable; the first value is the
 *	     specification of the standard output file and the second is
 *	     "CREATE" or "APPEND"
 */
FUNCTION CODE set_stdout 
(
    struct CONTXT	*procctx,	/* in: outgoing proc context		*/
    struct CONTXT	*cmdctx	/* in/out: new proc context		*/

 )
    {
    struct VARIABLE	*newstdp, *oldstdp;	/* old and new _STDOUT		*/
    struct VARIABLE	*quptr;			/* STDOUT qualifier		*/
    TEXT		*standout[2];		/* standout vector		*/
    struct FSBLOCK	fsb;			/* file block			*/
    TEXT		fspec[FSPECSIZ+1];
    TEXT		errstr[STRINGSIZ+1];

    newstdp = lookex(&(*cmdctx).locst, "_STDOUT");	/* point to the variable to be set	*/
    quptr = lookex(&(*cmdctx).qualst, "STDOUT");	/* point to qualifier		*/
    if ((*quptr).v_count != 0)				/* i.e., user specified STDOUT	*/
    	{
    	f_crack(SVAL(*quptr,0), "",(*cmdctx).pdf.name, LIS_TYPE, &fsb, errstr);
    	f_spec(&fsb, fspec);			/* get the full spec	*/
    	standout[0] = fspec;
    	standout[1] = "CREATE";
    	}
     else			/* no explicit STDOUT from user				*/
    	{
    	oldstdp = lookex(&(*procctx).locst, "_STDOUT");	/* use the old one	*/
    	standout[0] = SVAL(*oldstdp,0);
    	standout[1] = "APPEND";
    	}
    if (set_value(newstdp, (GENPTR)standout, 2) != SUCCESS)
    	{
    	overr();
    	return(FAIL);
    	}

    return(SUCCESS);
    }
#endif


/*
 *	hierarchy.   Implements hierarchy search on specified file.
 *
 *	RETURNS:
 *		SUCCESS -- file opend for read.
 *		FAIL    -- file spec syntax error
 *		F_FILERR -- file found but cannot be opened (protection?)
 *		F_NOFILE -- file not found in hierachy search
 *
 *	Any TAE libraries may have a default proc file type specified as
 *	characters appended to the library specification
 *	(in the appropriate TCL global variable) by an intervening "-".
 *	If the requested file type is PDF_TYPE (i.e., a request for a proc),
 *	and the caller has not explicitly specified the file type,
 *	each library's default type is used in the search.
 *	If the library has no default proc type, PDF_TYPE is used.
 *
 *	When the caller explicitly specifies the library, even if the library
 *	is in TAE's search list, the library specific proc default file type
 *	is not used.  The "type" argument is used as the default file type
 *	in this case.
 */

FUNCTION CODE hierarchy
(
    struct SFILE	*sfile,		/* out: SFILE to use for open		*/
    TEXT		verbstring[],	/* in: command string: may be filespec	*/
    TEXT		type[],		/* in: default file type		*/
    FUNINT		lun,		/* in: lun to use for file open		*/
    struct FSBLOCK	*fsblock,	/* out: FSBLOCK for PDF where found	*/
    TEXT	errstr[STRINGSIZ+1]	/* out: 'at or near' string		*/

 )
    {
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/
    IMPORT  struct  VARIABLE	*usrl_gbl;	/* pointer to $USERLIB 	*/
    IMPORT  struct  VARIABLE	*sysl_gbl;	/* pointer to $SYSLIB 	*/

    COUNT		i;
    struct VARIABLE	*v;
    CODE		code;
    BOOL		explic_type;	/* TRUE if file type explicitly spec'd	*/
    TEXT	lib_dflt_type[FSPECSIZ+1];	/* dflt file type for lib	*/
    TEXT	libr[FSPECSIZ+1];	/* current library in search		*/
    struct SYNBLK	sb;		/* syntax block -- for cmd_parse()	*/
    TEXT	save_type[FSPECSIZ+1];
    TEXT	dummy_type[FSPECSIZ+1];

    code = f_crack(verbstring, "", "", type, fsblock, errstr);
    if (code != SUCCESS)
	return (FAIL);
    if (NULLSTR((*fsblock).libr))		/* if no explicit library		*/
	{
	f_type(verbstring, dummy_type); 	/* type explicitly specd?	*/
	explic_type = (NULLSTR(dummy_type))  ?  FALSE : TRUE;
	s_copy((*fsblock).type, save_type);	/* dflt if no lib specific dflt	*/
	if ((*usrl_gbl).v_count > 0)
	    {
	    cmd_parse(&sb, SVAL(*usrl_gbl,0), NULL,
		libr, lib_dflt_type);		/* get user lib spec & dflt type*/
	    s_copy(libr, (*fsblock).libr);
	    if (s_equal(type, PDF_TYPE)  &&	/* if proc search...		*/
		!explic_type             &&	/* & caller's type not explicit	*/
		!NULLSTR(lib_dflt_type))	/* & user lib has a default type*/
		s_copy(lib_dflt_type, (*fsblock).type);	/* use user lib dflt type*/
	    code = open_try(sfile, lun, fsblock);
	    if (code == SUCCESS  ||  code == F_FILERR)
		return(code);
	    }
	v = apl_gbl;
	if (v != NULL)
	    {					/* try each string in $APLIB*/
	    for (i=0; i < (*v).v_count; i++)
		{
		s_copy(save_type, (*fsblock).type);	/* in case dflt lost	*/
		cmd_parse(&sb, SVAL(*v, i), NULL,
		    libr, lib_dflt_type);	/* get appl lib spec & dflt type*/
		s_copy(libr, (*fsblock).libr);
		if (s_equal(type, PDF_TYPE)  &&	/* if proc search...		*/
		    !explic_type             &&	/* & caller's type not explicit	*/
		    !NULLSTR(lib_dflt_type))	/* & lib has a default type	*/
		    s_copy(lib_dflt_type, (*fsblock).type); /* use lib dflt type*/
		code = open_try(sfile, lun, fsblock);
	        if (code == SUCCESS  ||  code == F_FILERR)
    		    return(code);
		}			    	
	    }
        if (s_equal(type, MDF_TYPE))		/* if search on mdf's   */
	    s_copy(MENULIB, (*fsblock).libr);	/* try TAE menu library */
	else					/* else, (for procs)	*/
	    {
	    if ((*sysl_gbl).v_count <= 0)
		return (F_NOFILE);
	    s_copy(save_type, (*fsblock).type);	/* in case default type lost	*/
	    cmd_parse(&sb, SVAL(*sysl_gbl,0), NULL,
		libr, lib_dflt_type);		/* get sys lib spec & dflt type*/
	    s_copy(libr, (*fsblock).libr);
	    if (s_equal(type, PDF_TYPE)  &&	/* if proc search...		*/
		!explic_type             &&	/* & caller's type not explicit	*/
		!NULLSTR(lib_dflt_type))	/* & sys lib has a default type*/
		s_copy(lib_dflt_type, (*fsblock).type);	/* use sys lib dflt type*/
	    }
	code = open_try(sfile, lun, fsblock);
	if (code == SUCCESS  ||  code == F_FILERR)
    	    return(code);
        else
	    return (F_NOFILE);
	}
    else	
	{					/* explicit library	*/
	code = open_try(sfile, lun, fsblock);
	return (code);
	}
    }

/*
 *	open_try.   Attempt to open file.
 *	If library is null, we do not try open.  This is a courtesy to
 *	$APLIB list, where null means skip it.
 */

FUNCTION static CODE open_try
(
    struct SFILE	*sfile,			/* in/out: SFILE to use	*/
    FUNINT		lun,			/* in: lun to use	*/
    struct FSBLOCK	*fsblock		/* in: file spec block	*/

 )
    {
    CODE 		code;

    if (NULLSTR((*fsblock).libr))
        return (F_NOFILE);			/* skip null libraries	*/
    code = f_opnblk(sfile, lun, fsblock, F_READ);
    return (code);
    }

/*
 *	m_put.   Writes message to output devices.
 *
 */

FUNCTION VOID m_put
(
    TEXT 	control[],		/* in: control string		*/
    TEXT	key[],			/* in: message key		*/
    uintptr_t	a1,		/* in: integers or string ptrs	*/
    uintptr_t   a2,
    uintptr_t   a3,
    uintptr_t   a4,
    uintptr_t   a5
 )
    {
    IMPORT CODE		 termtype;	/* terminal type		*/
    IMPORT TEXT 	 lastkey[];		/* most recent message key	*/
    IMPORT struct SFILE	 *pstdo_fil;	/* pointer to stdout SFILE	   */

    CODE	code;
    TEXT	mputrec[3*STRINGSIZ+1];	

    m_pfmt(control, key, mputrec, a1, a2, a3, a4, a5);	/* format message    */
    if (key && key[0])					/* if substantial key */
        s_copy(key, lastkey);				/* save key for help */
#ifdef SESSION_LOG
    slwrite ("MS: ", mputrec);				/* session log	     */
#endif
    code = put_outmsg(mputrec);				/* output message    */
    if (code != SUCCESS)			/* write error to stdo file  */
    	{
    	m_pfmt("Error writing to standard output. %s", "TAE-STDOWRT",
	       mputrec, (uintptr_t) (*pstdo_fil).errmsg, 0, 0, 0, 0);
    	if (termtype != T_NOTTERM) t_write(mputrec, T_PROMPT);

#ifdef SESSION_LOG
	slwrite ("MS: ", mputrec);			/* log	error message */
#endif
#ifdef SESSION_LOG2
        sl2write(mputrec,FALSE);
#endif
    	}    	
    return;
    }

/*
 *	m_cput.   Writes message to standard CRT output.
 */

FUNCTION VOID m_cput 
(
    FUNINT	line,			/* in: line number to write message  */
    TEXT 	control[],		/* in: control string		*/
    TEXT	key[],			/* in: message key		*/
    uintptr_t	a1,		/* in: integers or string ptrs	*/
    uintptr_t   a2,
    uintptr_t   a3,
    uintptr_t   a4,
    uintptr_t   a5
 )
    {
    IMPORT TEXT lastkey[];		/* most recent message key	*/
    TEXT	mputrec[3*STRINGSIZ+1];	


    m_pfmt(control, key, mputrec, a1, a2, a3, a4, a5);	 /* format the record */
    t_output(line, 1, mputrec);			/* write on column 1 */
    oprattn();					/* call for operator attn */

#ifdef SESSION_LOG
    slwrite ("MS: ", mputrec);			/* session log	*/
#endif
    if (key && key[0])				/* if substance...   */
        s_copy(key, lastkey);			/* save key for help */
    return;
    }


/*
 *	oprattn - call for operator attention (with a pause and/or bell)
 * 	NOTE:  $MESSAGE value		Action
 *	       --------------		----------
 *		ATTN			pause/bell
 *		PAUSE			pause/silent
 *		BELL			bell
 *		SILENT			silent
 */

    FUNCTION  	VOID  oprattn(void)

    {
    IMPORT  struct  VARIABLE	*msg_gbl;	/* pointer to $MESSAGE 	*/

    TEXT	dummy[STRINGSIZ+1];
    CODE	term;

    s_copy (SVAL(*msg_gbl, 0), dummy);
    if (s_lseq(dummy, "ATTN") || s_lseq(dummy, "BELL"))
	t_bell();			/* sound bell	*/
    if (s_lseq(dummy, "ATTN") || s_lseq(dummy, "PAUSE"))  /* wait implied */
	t_read(dummy, &term);		/* wait till operator responds */
    return;
    }

/*	opn_stdout - Open the standard output file
 *
 *	Returns SUCCESS or FAIL
 *
 * Updates the C globals stdo_fil (the stdout SFILE) and pstdo_fil (the
 * pointer to stdo_fil).
 */
FUNCTION CODE opn_stdout
(
    TEXT		spec[],			/* in: file spec		*/
    FUNINT		access			/* in: standard open access	*/

 )
    {
    IMPORT struct SFILE	  *pstdo_fil;		/* pointer to stdout SFILE	*/
    IMPORT struct SFILE	  stdo_fil;		/* stdout SFILE			*/

    CODE		code;


    code = SUCCESS;
    if (!(s_equal(spec, TERMINAL)))	/* open if not terminal		*/
	{
	code = f_opnspc(&stdo_fil, STDOUTLUN, spec, "", "",
	       LIS_TYPE, access);
	if (code == SUCCESS)
	    pstdo_fil = &stdo_fil;	/* point to new stdout		*/
	}
    else
	pstdo_fil = NULL;		/* terminal => null SFILE ptr	*/
    return(code);
    }

/*	
 *	overr - Report memory overflow error.
 */

    FUNCTION VOID overr (void)

    {
    tmmsg(PROCFAIL, "TAE Monitor internal memory overflow.  Command ignored.",
	  "TAE-MEMOVR", 0, 0, 0, 0, 0);
    return;
    }

/*
 *	put_outmsg. Put a message string on output devices.
 *	
 *	This routine writes a string  to the terminal, and, also
 *	to standard output file if that is different from terminal.
 *
 *	NOTE:  For UNIX new line written after message.
 *
 *	Return codes: SUCCESS or code returned from f_write
 *
 */

FUNCTION  CODE  put_outmsg
(
    TEXT	string[]		/* in: text string  to output	 */

 )
    {
    IMPORT struct SFILE	 *pstdo_fil;	/* pointer to stdout SFILE	  */
    IMPORT CODE		 run_type;	/* INTER, BATCH, or ASYNC	  */

    CODE	code;

    if (run_type == INTER)
    	{
        t_write(string, T_PROMPT);		/* write to terminal	   */
#ifdef UNIX
	t_write("\n", T_NULL);
#endif
        oprattn();				/* call operator attention */
    	}
    code = SUCCESS;				/* assume success	   */
#ifdef SESSION_LOG2
    sl2write(string,FALSE);                     /* 2nd session log write */
#endif
    if (pstdo_fil != NULL)			/* standard output is file */
    	code = f_write(pstdo_fil, string);	/* so, write to it	   */
    return (code);
    }


/*	put_stdout - Write a record to the standard output
 *
 *	No return codes
 */

FUNCTION VOID put_stdout
(
    TEXT		record[]	/* in: record to put		*/

 )
    {
    IMPORT struct SFILE	  *pstdo_fil;	/* pointer to stdout SFILE	*/
    IMPORT struct SFILE	  stdo_fil;	/* stdout SFILE			*/

    CODE		code;
    struct SFILE	*save;


    if (pstdo_fil == NULL)		/* i.e., no file open		*/
    	t_write(record, T_STDCC);	/* only cc is STDCC		*/
    else
    	{
    	code = f_write(pstdo_fil, record);
    	if (code != SUCCESS)
    	    {
    	    save = pstdo_fil;		/* save the SFILE ptr		*/
    	    pstdo_fil = NULL;		/* force error message to term	*/
    	    tmmsg(PROCFAIL, "Error writing to standard output. '%s'",
		  "TAE-STDOWRT", (uintptr_t) stdo_fil.errmsg, 0, 0, 0, 0);
    	    pstdo_fil = save;		/* restore SFILE ptr		*/
    	    }
    	}
#ifdef SESSION_LOG2
    sl2write(record,FALSE);             /* 2nd session log write */
#endif
    return;
    }

/*	tmierr - report TM internal error.
 *	Calls to this routine from different places in TM should each use
 *	a unique number as the argument, to identify the location of the
 *	internal error.
 */

FUNCTION VOID tmierr 
(
    FUNINT		number		/* in:  error number		*/

 )
    {
      tmmsg(PROCFAIL, "TAE internal error - %d", "TAE-INTERR", 
	    (uintptr_t) number, 0, 0, 0, 0);
    return;
    }

/*	tmmsg - Standard TAE monitor message function.
 *	Sets global status indicators ($SFI and $SKEY).
 */

FUNCTION VOID tmmsg
(
    FUNINT		scode,		/* in:  status code to set		*/
    TEXT		msg[],		/* in:  message control text		*/
    TEXT		key[],		/* in:  message key			*/
    uintptr_t		a1,	/* in:  ints or str ptrs	*/
    uintptr_t           a2,
    uintptr_t           a3,
    uintptr_t           a4,
    uintptr_t           a5
 )
    {
    IMPORT struct VARIABLE *sfi_gbl;		/* pointer to $SFI variable	*/
    IMPORT struct VARIABLE *skey_gbl;		/* pointer to $SKEY variable	*/
    IMPORT struct CONTXT   *curproc;


    TEXT	record[2*STRINGSIZ+1];		/* to hold two lines */
    struct	VARIABLE *_message;
    TEXT	mputrec[3*STRINGSIZ+1];	
    TEXT	*svv[1];			/* value vector		*/


    IVAL(*sfi_gbl,0) = scode;		/* set $SFI global variable	*/
    if (key)
        set_string (skey_gbl, key);
    s_copy (msg, record);
    if ((*curproc).onfailcmd)
	s_append ("\n\rProblem in _ONFAIL command.", record);
    if ((*curproc).prclevel > 0  &&
	(!(*curproc).compiled  ||  (*curproc).inbody))	/* no line # if...	*/
	{						/* pre-body & compiled	*/
	s_append (";\n\r", record);
	addproc (curproc, record);	/* add proc name, line		*/
	}

    /*	
	If the current proc has a _MESSAGE local variable, then
        the error message display is suppressed and the
	message is placed in the _MESSAGE variable.  This
	allows a TCL procedure to suppress error messages.
    */

    _message = lookex (&(*curproc).locst, "_MESSAGE");
    if (_message)
	{ 
        m_pfmt(record, key, mputrec, a1, a2, a3, a4, a5);	/* format */
	mputrec[(*_message).v_size] = EOS;			/* clip   */
	svv[0] = mputrec;				
	if (chk_vector (_message, V_STRING, (GENPTR) svv, 1, FALSE) == SUCCESS)
	  set_value (_message, (GENPTR) svv, 1);
	}
    else
        m_put(record, key, a1, a2, a3, a4, a5);
    return;
    }

#ifdef XXXXX    /* not currently used -- we flip curproc judiciously	*/
/*	tmmsg1. Special TAE monitor message function.
 *
 *	This is similar to tmmsg, however, the error is reported for
 *	the previous level context.  tmmsg1
 *	is used for errors like "missing parameters" where the current
 *	proc context is a PDF erroneously activated by the previous
 *	level.
 */

FUNCTION VOID tmmsg1 
(
    FUNINT		scode,		/* in:  status code to set		*/
    TEXT		msg[],		/* in:  message control text		*/
    TEXT		key[],		/* in:  message key			*/
    uintptr_t		a1,	/* in:  ints or str ptrs	*/
    uintptr_t a2,
    uintptr_t a3,
    uintptr_t a4,
    uintptr_t a5

 )
    {
    IMPORT struct VARIABLE *sfi_gbl;		/* pointer to $SFI variable	*/
    IMPORT struct VARIABLE *skey_gbl;		/* pointer to $SKEY variable	*/
    IMPORT struct CONTXT   *curproc;

    TEXT	record[2*STRINGSIZ+1];		/* to hold two lines */

    IVAL(*sfi_gbl,0) = scode;		/* set $SFI global variable	*/
    set_string (skey_gbl, key);
    s_copy (msg, record);
    s_append (";\n\r", record);
    addproc ((*curproc).backlink, record);	/* add proc name, line	*/
    m_put(record, key, a1, a2, a3, a4, a5);
    return;
    }
#endif
