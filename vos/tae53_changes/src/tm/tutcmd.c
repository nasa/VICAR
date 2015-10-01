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
 * This file contains functions to perform specific commands while
 * in tutor mode of TAE.  These functions are either called from the main
 * tutor function, tutor, or are called by other functions in this source file.
 *
 * This file is extracted from tutor.c during the tutor.c breakup of 6-dec-84.
 * See full explanation in tutor.c.
 *
 * The functions in this source file are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	07-dec-84	Add tutqual dummy function for clean link...peb
 *	08-dec-84	Start recursive tutor in tutqual()...peb
 *	11-dec-84	Add noscreen preface for parm qual tutor display...peb
 *	25-jan-85	Add mode help for qualifier space...peb
 *	27-jan-85	Fix prev mode QUALIFY cmd infinite loop...peb
 **
 **** RCJM SPECIFIC CHANGES *************
 *
 *	23-apr-85	Initialize helpblk.remctx field for remote help...dm
 *
 ********* MERGE WITH FOLLOWING TAE-V1.3 CHANGES...dm(24-may-85)
 *
 *	25-mar-85	Add subcmd argument in call to left_pbld...rcs
 *
 ****************************************************************************
 *
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	31-jul-86	Added code for TAE_FACELIFT...KRW
 *	20-aug-85	Modify setting of variable pointer in tutpage...joh
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	21-oct-85	PR 992: Changed routine 'tut_runparms' to parse tutor
 *			RUN command qualifiers & to do RESTORE function. A new
 *			routine 'tut_updtab' extracted from 'tut-runparms'...dab
 *	01-nov-85	PR 953: Don't prevent RUN of asynch jobs when in proc
 *			interrupt mode. Routine 'tutrun' changed...dab
 *	08-nov-85	PR 929/1088: Allow param quals to be specified from tut
 *			RUN and "parm=" commands. New routine 'tut_pqlf_upd'
 *			called from routines 'tut_updtab' and 'tutprmeq'. Also,
 *			fixed 'tut_pqlf_upd' to allow positional params after
 *			qualifiers...dab
 *	19-jan-87	PR1181: change hardcoded help filespecs to lower...palm
 *	11-feb-87	Merged TAE FACELIFT with V1.4 TAE...krw
 *
 **************************  Post TAE 2.1 changes ***************************
 *
 *	19-may-87	Updated for TAE-Plus philosophy. Checked global 
 *			variable DisplayId to determine VT100 or Window 
 *			mode screen operations...dm
 *	12-jun-87	New function wrap_save_restore()...dm
 *      12-aug-87       New remote call...tpl
 *	01-oct-87	Call SetParmError to indicate error in parm input...dm
 *	02-dec-87	Declare DisplayId as GENPTR...dm
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl 
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	01-feb-89	VAX C complained about nested comment...ljn
 *	01-may-89	remove check for process running in intmode...palm
 *			(now done by run_proc)...palm 
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	23-may-90	Remove RCJM stuff by referring to old TAE_RCJM...ljn
 *	28-jun-90	Remove Facelift code...ljn
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"dirinc.inc"		/* includes for d_ package		*/
#include	"helpinc.inc"
#include	"syninc.inc"		/* syntax package defs & structs	*/
#include "taeintproto.h"



    IMPORT  struct TUTCTX tutctx;	/* tutor & micro-editor context	*/


    static TEXT		msg_mmov[] = "Terminal Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/
    static TEXT		msg_hrde[] = "Error reading help file. %s";
    static TEXT		key_hrde[] = "TAE-RDERR";	/* help file read error in tutor*/
    static TEXT		msg_ambi[] = "Ambiguous parameter abbreviation, '%s'.";
    static TEXT		key_ambi[] = "TAE-AMBIGPAR";
    static TEXT		msg_unre[] = "Unrecognized parameter name, '%s'.";
    static TEXT		key_unre[] = "TAE-TUNRECPAR";
    static TEXT		msg_ambs[] = "Ambiguous subcommand abbreviation, '%s'.";
    static TEXT		key_ambs[] = "TAE-AMBIGSUB";
    static TEXT		msg_unrs[] = "Unrecognized subcommand, '%s'.";
    static TEXT		key_unrs[] = "TAE-UNRECSUB";
    static TEXT		msg_nscr[] = "%s command only available in NOSCREEN tutor.";
    static TEXT		key_nscr[] = "TAE-TNSCR";
    static TEXT		msg_sbin[] = "Subscript must be an integer constant.";
    static TEXT		key_sbin[] = "TAE-TNISUBSC";

FUNCTION static CODE tut_updtab 
(
    FAST struct SYMTAB	*symtab,	/* in/out: symbol table		*/
    FAST struct SYNBLK  *sb,		/* in/out: syntax block		*/
    struct CONTXT	*pctx		/* in: proc context		*/

 );


/*
 *	ini_p_child - initialize a proc context as a child of the passed
 *	in parent proc context.  This is from tutoring on the qualifiers
 *	of a single parameter from the parent proc context.
 *
 *	The primary thing that is done by this function is to remove the
 *	parameter qualifier symbol table from the parameter variable structure
 *	and attach it as the parameter symbol table of the child proc context.
 */

FUNCTION static struct VARIABLE *ini_p_child 
(
    struct CONTXT	*parent,	/* in/out: parent proc context		*/
    TEXT		parm_name[],	/* in:  name of the qualified parameter	*/
    struct CONTXT	*child		/* out: child proc context		*/

 )
    {
    struct VARIABLE	*v;

    lookab (&(*parent).parmst, parm_name, &v);	/* at this point, success guaranteed*/
    if (!(*v).v_pv12  ||  (*v).v_qualst.link == NULL)
	{
	  tutmsg ("'%s' has no qualifiers defined.", "TAE-TNOQUAL", 
		  (uintptr_t) (*v).v_name, 0, 0, 0, 0);
	return (NULL);
	}
    zero_block ((GENPTR) child, sizeof(struct CONTXT));
    (*child).parmst.link = (*v).v_qualst.link;	/* link parm qual st into child	*/
    (*child).prclevel = (*parent).prclevel;
    (*child).special  = NOT_SPECIAL;
    (*v).v_qualst.link = NULL;			/* will be put back later	*/
    s_copy ((*parent).pdf.name, (*child).pdf.name);	/* proc name - for noscreen prompt*/
    return (v);
    }

/*
 *	ini_t_child - initialize tutor context as child assuming the context
 *	currently holds the values of the parent.  This establishes the
 *	tutor context for parameter qualifier symbol table.
 */

FUNCTION static CODE ini_t_child 
(
    struct TUTCTX	*tutctx,	/* in/out: becomes the child tutor context*/
    struct CONTXT	*proc_child,	/* in:  the child proc context (already complete)*/
    TEXT		parm_name[]	/* in:  name of parm to be qualified	*/

 )
    {
    (*tutctx).edtcmd    = FALSE;
    (*tutctx).start     = TRUE;
    (*tutctx).highact   = FALSE;
    (*tutctx).vcur      = (*proc_child).parmst.link;	/* point to 1st qual	*/
    (*tutctx).index     = 0;
    (*tutctx).curpag    = 1;
    (*tutctx).ctx       = proc_child;
    (*tutctx).title.numline = 0;
    (*tutctx).title.tp  = NULL;
    (*tutctx).dispreq   = TRUE;
    (*tutctx).lastpag   = 0;
    (*tutctx).srch1cplt = FALSE;
    (*tutctx).qualtut   = TRUE;
    (*tutctx).prev_cmd[0] = EOS;
    (*tutctx).prev_mode = FALSE;
    s_copy (parm_name, (*tutctx).parmname);
    return(SUCCESS);
    }

/*
 *	repair_parent - repair the parent proc context (the parameter
 *	qualifier symbol table has been removed for the indicated parameter --
 *	it needs to be put back).
 */

FUNCTION static VOID repair_parent 
(
    struct CONTXT	*child,		/* in/out: child proc context		*/
    TEXT		parm_name[],	/* in:  name of parm whose qual st is missing*/
    struct CONTXT	*parent	/* in/out: parent proc context to repair*/

 )
    {
    struct VARIABLE	*v;

    lookab (&(*parent).parmst, parm_name, &v);	/* at this point - guaranteed to work*/
    (*v).v_qualst.link = (*child).parmst.link;	/* put back where it belongs	*/
    (*child).parmst.link = NULL;	/* so qual st doesn't get deallocated	*/
    return;
    }

/*
 *	tut_pqlf_upd - update parameter qualifiers from command line.
 *	The command line syntax block is assumed to be positioned such
 *	that the next non-white character is the opening qualifier
 *	separator, if there are any qualifiers for this parm.
 *
 *	Note that routine upd_p_qlf is similar to this routine. The
 *	difference is that that routine outputs error messages to terminal
 *	directly whereas this routine "holds" them since tutoring.
 */

FUNCTION CODE tut_pqlf_upd 
(
    struct SYNBLK	*sb,		/* in/out: command line syntax block	*/
    struct VARIABLE	*v,		/* in/out: parm which the quals apply to*/
    struct CONTXT	*pctx		/* in: proc context				*/

 )
    {
    CODE		code;
    TEXT		qualstr[STRINGSIZ+1];
    struct SYNBLK	loc_sb;

    code = getqlf(sb, qualstr);		/* get parameter qualifiers		*/
    if (code == S_NONE)
	return(SUCCESS);		/* nothing to do if no quals		*/
    if (code != SUCCESS)
	goto qual_err;
    if (!(*v).v_pv12)
	goto notallowed_error;
    fndsep(sb);				/* get past trailing separator if present	*/
    initok(&loc_sb, qualstr);		/* init parm qual stream for syntax pkg	*/
    code = tut_updtab(&(*v).v_qualst, &loc_sb, pctx);	/* update parm qual st using stream*/
    if (code != SUCCESS)
	return (FAIL);
    return(SUCCESS);

qual_err:
    hold_msg ((*sb).errmsg, "TAE-BADPQUAL", 0, 0, 0, 0, 0);
    return (FAIL);

notallowed_error:
    hold_msg ("Parameter qualifiers are not defined for this proc.",
	      "TAE-BADPQUAL", 0, 0, 0, 0, 0);
    return (FAIL);
    }

/*	
 *	tut_runparms.   Set parameter values from tutor run command.
 *	First parses and validates any command qualifiers and then
 *	does the same for actual command parameters.
 *
 *	Returns:
 *		SUCCESS -- all parameters specified are okay
 *		FAIL -- invalid variable specification found
 */

FUNCTION static CODE tut_runparms 
(
    FAST struct CONTXT	*pctx,		/* in/out: proc context		*/
    TEXT		cmdstr[]	/* RUN command string		*/

 )
    {
    CODE	code;
    CODE	qcode;			/* qualifier string parsing code */
    struct SYNBLK	sb;		/* entire command string syntax block */
    struct SYNBLK	qualsb;		/* qualifier string syntax block */
    TEXT	token[TOKESIZ+1];
    TEXT	qualstr[CMDLINSIZ+1];	/* qualifier string */
    TEXT	errmsg[STRINGSIZ+1];	/* error message text */
    struct VARIABLE	*v;


/*  initialize syntax block for the command string			*/

    initok (&sb, cmdstr);		/* create block			*/
    code = gettok (&sb, token);		/* get past the 'RUN'		*/
    if (code == S_WHITE)
        code = gettok (&sb, token);

/*  check for RUN command qualifiers and do RESTORE if necessary	*/

    qcode = getqlf (&sb, qualstr);	/* get qualifier string */
    if (qcode != SUCCESS  &&  qcode != S_NONE)
	goto bad_qual;
    if (qcode != S_NONE)		/* do we have any qualifiers ? */
	{
	initok (&qualsb, qualstr);	/* ... yes */
	/* update qualifier symbol table */
	if ((tut_updtab (&(*pctx).qualst, &qualsb, pctx)) != SUCCESS)
	    return (FAIL);
	else
	    {
	    /* do RESTORE before using command line parameter values */
	    v = lookex (&(*pctx).qualst, "RESTORE");
	    if (!(*v).v_default)
		if (rest_parm (SVAL(*v,0), pctx, errmsg) != SUCCESS)
		    goto bad_pfile;
	    }
	}

/* Retrieve parameters */

    code = tut_updtab (&(*pctx).parmst, &sb, pctx);
    return (code);

bad_qual:
    hold_msg (sb.errmsg, "TAE-QUALERR", 0, 0, 0, 0, 0);
    return (FAIL);

bad_pfile:
    hold_msg (errmsg, "TAE_BADPFILE", 0, 0, 0, 0, 0);
    return (FAIL);

    }

/*	tut_set_value.   Set a parameter value.
 *
 *	Note: error messages are placed in help_msg and held_key
 *	because sometimes the caller must re-paint before showing
 *	the error.
 */

FUNCTION CODE tut_set_value 
(
    struct CONTXT	*pctx,		/* in: proc context		*/
    struct VARIABLE	*v,		/* in/out: VARIABLE to set	*/
    FUNINT		subscr,		/* in: component (0 = all)	*/
    FUNINT		count,		/* in: count of...		*/
    TEXT		*value[]	/* in: value array 		*/
					/* (strings are de-allocated)	*/

 )
    {
    union				/* union of value vectors	*/
	{
	TAEINT		ival[MAXVAL];
	TAEFLOAT	rval[MAXVAL];
	} vect;
    struct VARIABLE	*vref;
    CODE		type;
    BOOL		fillreq = FALSE;	/* true if value fill required		*/
    COUNT		fillcnt = 0;	/* value count to fill to		*/
    CODE		code;
    COUNT		i;
    GENPTR		valvec = 0;		/* value vector pointer		*/

/*	Initial checks		*/

    type = (*v).v_type;
    if (type == V_NAME)
	{
	if (subscr > 1)
	    goto subl_err;
	}
    else
	{
	if (subscr > (*v).v_maxc)		/* if subscript too large	*/
	    goto subl_err;
	if (subscr > (*v).v_count)
	    {
	    fillreq = TRUE;
	    fillcnt = max((*v).v_minc, subscr);
	    }
	else
	    fillreq = FALSE;
	}

/*
 *	Set value when subscript exists:
 */
    if (subscr > 0)				/* subscript present?	*/
	{
	if (count == -1)			/* defaults requested?	*/
	    {
	    if (type == V_NAME)
		{				/* NAME parameter:	*/
		if ((*v).v_dref == NULL)
		    goto deflt_err;
		(*v).v_ref = (*v).v_dref;
		}
	    else				/* direct valued parm:	*/
		{
		if ((*v).v_dcount < 0  ||  subscr > (*v).v_dcount)
		    goto deflt_err;
		if (fillreq)
		    fill_value(v, fillcnt);		/* fill from old count to new	*/
		if (type == V_INTEGER)
		    IVAL(*v, subscr-1) = DIVAL(*v, subscr-1);
		else if (type == V_REAL)
		    RVAL(*v, subscr-1) = DRVAL(*v, subscr-1);
		else if (type == V_STRING)
		    {
    		    valvec = (GENPTR) &DSVAL(*v, subscr-1);
		    code = set_component(v, valvec, subscr);
    		    if (code != SUCCESS)
			goto over_err;
		    }
		}
	    }
	else if (count == 0)				/* null value:	*/
	    {
	      hold_msg ("Components are not nullable.", "TAE-COMPNULL",
			0, 0, 0, 0, 0);
	    goto fail_exit;
	    }
	else						/* value present: */
	    {
	    if (type == V_INTEGER)
		{
		if (s_s2i(value[0], &(vect.ival[0])) != SUCCESS)
		    goto nonint_err;			/* convert to integer*/
    		valvec = (GENPTR) vect.ival;
		}
	    else if (type == V_REAL)
		{
		if (s_s2r(value[0], &(vect.rval[0])) != SUCCESS)
		    goto nonrl_err;			/* convert to real*/
    		valvec = (GENPTR) vect.rval;
		}
	    else if (type == V_STRING)
    		valvec = (GENPTR) value;
	    else if (type == V_NAME)
		{
		vref = search(value[0], (*pctx).backlink);
		if (vref == NULL)
		    goto invalid_name;
		(*v).v_ref = vref;
		}
    	    if (type != V_NAME)
    		{				/* set value from valvec: */
     		code = chk_component (v, type, valvec);
		if (code != SUCCESS)
		    goto valid_err;
		if (fillreq)
		    fill_value (v, fillcnt);
		code = set_component (v, valvec, subscr);
		if (code != SUCCESS)
		    goto over_err;
		}
	    (*v).v_default = FALSE;	/* no longer defaulted 		*/
	    }
	}

/*
 *	Set value when no subscript exists:
 */

    else
	{
	if (count == -1)		/* default requested		*/
	    {
	    if (type == V_NAME)
		{
		if ((*v).v_dref == NULL)
		    goto deflt_err;
		(*v).v_ref = (*v).v_dref;
		}
	    else
		{
		if ((*v).v_dcount < 0)
		    goto deflt_err;
		code = set_value(v, (*v).v_dvp, (*v).v_dcount);
    		if (code != SUCCESS)
		    goto over_err;
		}
	    (*v).v_default = TRUE;
	    }
	else				/* assume values specified explicitly	*/
	    {
	    if (type == V_NAME)
		{
		if (count != 1)
		    {
		    code = V_BADCOUNT;
		    goto valid_err;
		    }
		vref = search(value[0], (*pctx).backlink);
		if (vref == NULL)
		    goto invalid_name;
		(*v).v_ref = vref;
		}
	    else if (type == V_INTEGER)
		{
		for (i = 0; i < count; i++)
		    {
		    if (NULLSTR(value[i]))
			vect.ival[i] = 0;
		    else
			if (s_s2i(value[i], &(vect.ival[i])) != SUCCESS)
			    goto nonint_err;
		    }
		valvec = (GENPTR) vect.ival;
		}
	    else if (type == V_REAL)
		{
		for (i = 0; i < count; i++)
		    {
		    if (NULLSTR(value[i]))
			vect.rval[i] = 0.0;
		    else
			if (s_s2r(value[i], &(vect.rval[i])) != SUCCESS)
			    goto nonrl_err;
		    }
		valvec = (GENPTR) vect.rval;
		}
	    else if (type == V_STRING)
		valvec = (GENPTR) value;
	    if (type != V_NAME)
		{			/* set the value from valvec:	*/
		code = chk_vector (v, type, valvec, count, FALSE);
		if (code != SUCCESS)
		    goto valid_err;
		code = set_value (v, valvec, count);
		if (code != SUCCESS)
		    goto over_err;
		}
	    (*v).v_default = FALSE;
	    }
	}		
    free_val (value, count);		/* free the value strings	*/
    return (SUCCESS);

subl_err:
    hold_msg ("Subscript too large for variable '%s'.",
	      "TAE-BIGSUBS", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto fail_exit;

deflt_err:
    if (subscr == 0)
	hold_msg ("No default value defined for '%s'.",
		  "TAE-TNODEF", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    else
	hold_msg ("No default value defined for '%s(%d)'.",
		  "TAE-TNODEF", (uintptr_t) (*v).v_name, (uintptr_t) subscr,
		  0, 0, 0);
    goto fail_exit;

over_err:
    hold_msg (msg_mmov, key_mmov, 0, 0, 0, 0, 0);	
				/* report memory overflow	*/
    goto fail_exit;

nonint_err:
    hold_msg ("Invalid integer value%s for '%s'", "TAE-INVINT",
	      (uintptr_t) (count>1 ? "s":""), (uintptr_t) (*v).v_name, 0, 0, 0);
    goto fail_exit;

nonrl_err:
    hold_msg ("Invalid real value%s for '%s'", "TAE-INVREAL",
	      (uintptr_t) (count>1 ? "s":""), (uintptr_t) (*v).v_name, 0, 0, 0);
    goto fail_exit;

invalid_name:
    hold_msg ("Reference to undefined variable '%s'.",
	      "TAE-UNDEFVAR", (uintptr_t) value[0], 0, 0, 0, 0);
    goto fail_exit;

valid_err:
    if (code == V_BADCOUNT)
	{
	if ((*v).v_type == V_NAME)
	    hold_msg ("Incorrect value count.  '%s' may have only one reference.",
		      "TAE-TOOFEW", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
        else if (count == 0)
	    hold_msg ("Parameter '%s' is not nullable.",
		      "TAE-NOTNULL", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
	else
	    hold_msg ("Incorrect count.  '%s' must have at least %d values.",
		    "TAE-TOOFEW", (uintptr_t) (*v).v_name, 
		      (uintptr_t) (*v).v_minc, 0, 0, 0);
	}
    else if (code == V_BADSIZE)
	hold_msg ("String too long.  Max string size for '%s' is %d.",
    		"TAE-LONGSTR", (uintptr_t) (*v).v_name, 
		  (uintptr_t) (*v).v_size, 0, 0, 0);
    else if (code == V_BADVALID)
    	{
    	if ((*v).v_type == V_STRING)
	    hold_msg ("Specified value is not one of the acceptable strings for '%s'.",
		      "TAE-INVSTR", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    	else
    	    hold_msg ("Value out of the defined range for '%s'.",
		      "TAE-RANGE", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    	}
    else if (code == V_BADFILE)
	hold_msg ("File not found or incorrect file specification for '%s'.",
		  "TAE-BADFILE", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    else if (code == V_BADAMBIG)
	hold_msg ("Specified value is ambiguous for '%s'.", "TAE-AMBIGVAL",
		  (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto fail_exit;


fail_exit:
    free_val (value, count);
    return (FAIL);
    }

/*	
 *	tut_updtab. Set command qualifier or parameter values from tutor run
 *	 command. This function is similar to the updtab function in tm.c.
 *
 *	Returns:
 *		SUCCESS -- all parameters specified are okay
 *		FAIL -- invalid variable specification found
 */

FUNCTION static CODE tut_updtab 
(
    FAST struct SYMTAB	*symtab,	/* in/out: symbol table		*/
    FAST struct SYNBLK  *sb,		/* in/out: syntax block		*/
    struct CONTXT	*pctx		/* in: proc context		*/

 )
    {
    TEXT	*pos;
    CODE	code;
    TEXT	*msg;
    TEXT	key[TOKESIZ+1];		/* keyword			*/
    TEXT	*value[MAXVAL];		/* value pointers		*/
    COUNT	count;			/* number of values for single parm*/
    struct VARIABLE *v;



/* Start out assuming values by position	*/

    for (v=(*symtab).link; FOREVER; v=(*v).v_link)
	{
	pos = (*sb).curchr;		/* save position in cmd stream		*/
        code = getkey(sb, key);		/* try to get a "parm="			*/
	if (code == SUCCESS  ||  code == S_KEYWORD)
	    break;			/* if key present, no more positionals	*/
	if (code == EOS)
	    return (SUCCESS);
	(*sb).curchr = pos;		/* no key; start over for positional	*/
	code = getval(sb, value, MAXVAL, &count);	/* get value		*/
	if (code == S_SYNERR)
	    goto value_syntax;
	if (code == EOS)
	    return (SUCCESS);
        if (v == NULL)
    	    goto too_many;		/* too many positionals		*/
	if (count >= 0)			/* if value is present		*/
	    {
	    code = tut_set_value (pctx, v, 0, count, value);
    	    if (code != SUCCESS)
    		return (FAIL);
	    code = tut_pqlf_upd (sb, v, pctx);  /* update parm quals from cmd line */
	    if (code != SUCCESS)
		return (FAIL);
	    }
	}

/*	Now get parms explicitly named or by flagged keywords	*/

    (*sb).curchr = pos;				/* back to 1st keyword	*/
    while (FOREVER)
	{
	if ((code = getkey(sb, key)) == EOS)	/* get next keyword	*/
	    return (SUCCESS);
	if (code == S_NONE)			/* if specified positionally*/
	    goto position_err;
	if (code != SUCCESS  &&  code != S_KEYWORD)
	    goto parm_syntax;
	if (code == S_KEYWORD)
    	    {
	    code = getval(sb, value, MAXVAL, &count);	/* get keywords	*/
	    if (code == S_SYNERR)
	        goto value_syntax;
	    if (code == EOS)
	        goto key_syntax;
	    if (count < 1)
	        goto key_error;
	    code = key_trans (symtab, value[0], &v);	
	    if (code != SUCCESS)
	        goto bad_keyword;
	    }
	else
	    {
	    code  = lookab(symtab, key, &v);	/* find variable name	*/
	    if (code != SUCCESS)
		goto bad_parmname;
	    code = getval(sb, value, MAXVAL, &count);	/* get the value	*/
	    if (code == S_SYNERR)
		goto value_syntax;
	    if (code == EOS)				/* "key=" at end of line*/
		return(SUCCESS);
	    }
	if (count >= 0)					/* if value present	*/
	    {
	    code = tut_set_value (pctx, v, 0, count, value);
	    if (code != SUCCESS)
    		return (FAIL);
	    code = tut_pqlf_upd (sb, v, pctx);  /* update parm quals from cmd line */
	    if (code != SUCCESS)
		return (FAIL);
	    }
	}

too_many:
    hold_msg ("Too many positional parameters specified.", "TAE-PARS",
	      0, 0, 0, 0, 0);
    return (FAIL);

value_syntax:
    hold_msg ((*sb).errmsg, "TAE-INVPVAL", 0, 0, 0, 0, 0);
    return (FAIL);

key_error:
key_syntax:
    synerr (sb, "Invalid keyword syntax.");
    hold_msg ((*sb).errmsg, "TAE-KEYSYNTAX", 0, 0, 0, 0, 0);
    return (FAIL);

bad_keyword:
    if (code == AMBIG)
        msg = "Ambiguous keyword '%s'.";
    else
        msg = "Undefined keyword '%s'.";
    hold_msg (msg, "TAE-KEYWORD", (uintptr_t) value[0], 0, 0, 0, 0);
    return (FAIL);

bad_parmname:
    if (code == AMBIG)
        hold_msg ("Ambiguous abbreviation '%s'.", "TAE-AMBIGPAR", 
		  (uintptr_t) key, 0, 0, 0, 0);
    else
        hold_msg ("Parameter '%s' does not exist.", "TAE-BADPAR", 
		  (uintptr_t) key, 0, 0, 0, 0);
    return (FAIL);

parm_syntax:
    hold_msg ((*sb).errmsg, "TAE-INVPNAME", 0, 0, 0, 0, 0);
    return (FAIL);

position_err:
    hold_msg ("Positional values may not follow values specified by name.",
	      "TAE-POSERR", 0, 0, 0, 0, 0);
    return (FAIL);

    }

/*
 *	tutaccept - Returns from parameter qualifier tutor space to
 *	normal parameter tutor space, accepting those qualifier values
 *	entered.
 */

FUNCTION CODE tutaccept 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    return (TUT_ACCEPT);
    }

/* tutdisplay - perform NOSCREEN tutor DISPLAY command.
 */

FUNCTION CODE tutdisplay 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct VARIABLE	*v;
    struct SUBCMD	*s;
    TEXT		*varname[MAXVAL];
    COUNT		numvar;
    COUNT		i;
    CODE		code;

    if (tutctx.screen)
	{
	  tutmsg(msg_nscr, key_nscr, (uintptr_t) "DISPLAY", 0, 0, 0, 0);
	return(FAIL);
	}
    code = gettvnam(cmdstr, varname, &numvar);	/* get variable (or subc) name list*/
    if (code != SUCCESS) return(FAIL);
    if (tutctx.subtut)					/* if subcommand tutor		*/
	{
	if (NULLSTR(cmdprm))
	    for (s = (*pctx).subptr; s != NULL; s = (*s).link)
		{
		list_subc(s);			/* displ subc name & value	*/
		h1dsp_subc(pctx, s, hf);	/* displ lev 1 help		*/
		}
	else
	    for (i = 0; i < numvar; i++)
		{
		code = subab((*pctx).subptr, varname[i], &s);
		if (code != SUCCESS)
		    {
		    if (code == AMBIG)
		      tutmsg(msg_ambs, key_ambs, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
		    else
		      tutmsg(msg_unrs, key_unrs, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* unrecognized subcommand		*/
		    continue;
		    }
		list_subc(s);			/* displ subc name & value	*/
		h1dsp_subc(pctx, s, hf);	/* displ lev 1 help		*/
		}
	}
    else				/* parm tutor				*/
	{
	if (NULLSTR(cmdprm))
	    for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)
		{
		list_parm(v);			/* displ parm name & value	*/
		h1dsp_parm(pctx, v, hf);	/* displ lev 1 help		*/
		}
	else
	    for (i = 0; i < numvar; i++)
		{
		code = lookab(&(*pctx).parmst, varname[i], &v);
		if (code != SUCCESS)
		    {
		    if (code == AMBIG)
		      tutmsg(msg_ambi, key_ambi, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
		    else
		      tutmsg(msg_unre, key_unre, (uintptr_t) varname[i],
			     0, 0, 0 ,0);	/* unrecognized parameter		*/
		    continue;
		    }
		list_parm(v);			/* displ parm name & value	*/
		h1dsp_parm(pctx, v, hf);		/* displ lev 1 help		*/
		}
	}
    return(SUCCESS);
    }

/* tutexit - perform tutor mode EXIT command.
 */

FUNCTION CODE tutexit 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    IMPORT struct VARIABLE *skey_gbl;	/* pointer to $SKEY global		*/
    IMPORT CODE		menu_screen;	/* flag to menu if pause before paint	*/

    if (!tutctx.dyntut)				/* if initial tutor		*/
	menu_screen = AUTO_PAINT;		/* do not pause before repainting menu	*/
    ini_status();				/* reset previous sfi, skey	*/
    set_string (skey_gbl, "TAE-EXIT");
    return(TUT_EXIT);
    }

/* tuthlp - perform tutor mode HELP command.
 *
 * NOTE: In case of error, formatted error message and key are returned by
 * help functions in helpblk struct.
 *
 */

FUNCTION CODE tuthlp 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    IMPORT  struct  VARIABLE *char_gbl;		/* system characteristics 	*/

    struct VARIABLE	*v;
    struct SUBCMD	*s;
    struct TUTEXT	*t;
    struct SFILE	tutmhlp;		/* tutor mode help file		*/
    struct HELPBLK	helpblk;		/* help control block		*/
    TEXT		buf[STRINGSIZ+1];
    TEXT		dirctv[STRINGSIZ+1];
    TEXT		name[NAMESIZ+1];
    TEXT		tmpstr[TOKESIZ+1];
    TEXT		type[STRINGSIZ+1];
    TEXT		subcmd[SUBCMDSIZ+1];
    CODE		code;
    struct TXTSTOR	nultitle;		/* null title structure	      */
    struct DIRBLK	db;			/* directive block for d_ pkg */
    TEXT		libe[FLIBRSIZ+1];
    struct CONTXT	*ctx;
    IMPORT	struct  VARIABLE *tutoropt_gbl;  /* tutor options global      */
    BOOL        no_tag, no_name, no_library;

    no_tag = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_TAG");
    no_name = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_NAME");
    no_library = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_LIBRARY");


    code = SUCCESS;
    helpblk.nextcmd[0] = EOS;		/* initialize for prev mode command   */
    helpblk.compiled = FALSE;
    initxt(&nultitle);

    if (NULLSTR(cmdprm))				/* if mode help       */
	{
	if (tutctx.screen)
	    if (tutctx.subtut)
		s_copy("fmtsubtut", buf);
	    else if (tutctx.qualtut)
		(tutctx.dyntut) ? s_copy("fmtdqltut", buf) :
		    		  s_copy("fmtpqltut", buf);
	    else
		(tutctx.dyntut) ? s_copy("fmtdyntut", buf) :
		    		  s_copy("fmtprmtut", buf);
	else
	    if (tutctx.subtut)
		s_copy("nscsubtut", buf);
	    else if (tutctx.qualtut)
		(tutctx.dyntut) ? s_copy("nscdqltut", buf) :
		    		  s_copy("nscpqltut", buf);
	    else
		(tutctx.dyntut) ? s_copy("nscdyntut", buf) :
				  s_copy("nscprmtut", buf);
	code = f_open(&tutmhlp, TUTMHELPLUN, HELPLIB, buf, HLP_TYPE, F_READ);
	if (code != SUCCESS) goto open_err;
	s_copy(buf, helpblk.helpobj);
	proc_help(&tutmhlp,HELPLIB, "", "", "", "", "", &helpblk);
	f_close(&tutmhlp, F_KEEP);
	if (code != SUCCESS) goto help_err;		
	}
    else if (s_lseq("*", cmdprm))	/* help on current proc desired	      */
	{
	if ((*pctx).compiled)
	    helpblk.compiled = TRUE;	/* flag if help on a compiled PDF     */
	if (hf == NULL) 		/* no separate help file avail	      */
	    goto proc_err;
	if (tutctx.hlpexist)
	    {
	    f_setpos(hf, &tutctx.hlppos);
	    f_read(hf, buf);		/* dummy read to pass .HELP directive	*/
    	    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	    s_copy ((*ctx).pdf.libr, libe);
	    d_init(&db, hf, libe,
		(TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);  /* init directive block	*/
	    }
	else if (!tutctx.hlp_searched)		/* never searched .help before	*/
	    {
	    f_setpos(hf, &(*hf).posctx);	/* start at current position	*/
	    tutctx.hlp_searched = TRUE;		/* now searching .help		*/
    	    for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	    s_copy ((*ctx).pdf.libr, libe);
	    d_init(&db, hf, libe,
		(TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);  /* init directive block	*/
	    while ((code = d_dirctv(&db, dirctv, buf)) == SUCCESS)
	        {
	        if (s_equal(dirctv, ".HELP"))	/* .HELP  encountered	*/
		    {
		    f_movpos(&(*hf).posctx, &tutctx.hlppos); /* save .help posctx*/
		    tutctx.hlpexist = TRUE;		/* mark as exists	*/
		    break;
		    }
	    	else if (s_equal(dirctv, ".LEVEL2"))	/* .level2 help found	*/
		    {
		    if (f_read(hf, buf) != SUCCESS) goto read_err;
		    f_movpos(&(*hf).posctx, &tutctx.lasth2pos);
		    f_movpos(&(*hf).posctx, &tutctx.lev2start);
	  	    tutctx.h2exist = TRUE;
		    }
	        else if (s_equal(dirctv, ".END"))	/* End of file reached	*/
		    break;
	    if (code != SUCCESS)  goto read_err;
	    	}
	    }
     	if (tutctx.hlpexist)
	    {
	    if ((*pctx).intrinsic)
		s_copy("command", tmpstr);
	    else
		s_copy("proc", tmpstr);
	    buf[0] = EOS;
	    left_fbld  ( 				/* build left header         */
		   no_library ? "" : (*pctx).pdf.libr,
		   no_name ? "" : (*pctx).pdf.name,
  		   "", tmpstr, buf);	/* build header name */
	    s_copy((*pctx).pdf.name, helpblk.helpobj);
	    code = helper(&db, tmpstr, buf, &tutctx.title, &helpblk);
						/* already positioned	*/
	    if (code != SUCCESS) goto help_err;
	    }
 	else 					/* help searched and not exists	*/
	    goto proc_err;	
	}
    else				/* HELP command had a parameter		*/
	{
	if ((*pctx).compiled)
	    helpblk.compiled = TRUE;	/* flag if help on parm in comp'd PDF	*/
	if (tutctx.subtut || s_lseq("-", cmdprm))  /* if help for subcmd desired		*/
	    {
	    if (s_lseq("-", cmdprm))
		s_shift(cmdprm, 1);	/* delete leading '-' from subcmd	*/
	    if (!tutctx.subtut)
		{
		if (NULLSTR((*pctx).subcmd) || !s_lseq(cmdprm, (*pctx).subcmd))
		    goto entry_err;
		s_copy((*pctx).subcmd, cmdprm);
		}
	    code = subab((*pctx).subptr, cmdprm, &s);
	    if (code != SUCCESS) goto subc_err;
	    t = &(*s).ext;
	    s_copy((*s).name, name);
	    s_copy("subcmd", type);
	    s_copy((*pctx).subcmd, subcmd);
	    while (!(*t).l2hexist)
	    	{
		code = inlevel2(pctx, hf, type, subcmd);
	        if (code == FAIL) goto lev2_err;
		if (code == TUT_NOL2HELP)
		    {
		    if (NULLSTR(subcmd)) goto lev2_err;
		    subcmd[0] = EOS;
		    tutctx.srch2cplt = FALSE;
		    f_movpos(&tutctx.lev2start, &tutctx.lasth2pos);
						/* start over on .LEVEL2 */
		    }
		}
	    }
	else
	    {
	    code = lookab(&(*pctx).parmst, cmdprm, &v);
	    if (code != SUCCESS) goto parm_err;
	    if ((*v).v_tp == NULL)
		if (((*v).v_tp = (struct TUTEXT *) tae_alloc(1,sizeof(struct TUTEXT)))
		    == NULL) goto over_err;
	    t = (*v).v_tp;
	    s_copy((*v).v_name, name);
	    s_copy("parm", type);
	    s_copy((*pctx).subcmd, subcmd);
	    while (!(*t).l2hexist)
	        {
	        code = inlevel2(pctx, hf, type, subcmd);
	    	if (code == FAIL) goto lev2_err;
		if (code == TUT_NOL2HELP) 	/* no l2help found for this parm */
		    {
		    if (NULLSTR(subcmd)) break;
		    subcmd[0] = EOS;		/* reset for search without subcmd */
		    tutctx.srch2cplt = FALSE;
		    f_movpos(&tutctx.lev2start, &tutctx.lasth2pos);
						/* start over on .LEVEL2 */
		    }
		}
	    }
    	if (code == SUCCESS)			/* l2help exists for this parm	*/
	    {
	    f_setpos(hf, &(*t).l2hpos);
	    f_read(hf, buf);			/* dummy read to complete positioning*/
	    }
	for (ctx = pctx; (s_equal((*ctx).pdf.libr,"/LOCAL/")); ctx = (*ctx).backlink);
	s_copy ((*ctx).pdf.libr, libe);
	d_init(&db, hf, (*pctx).pdf.libr,
	    (TEXT **) (*char_gbl).v_cvp, (*char_gbl).v_count);	/* init directive block	*/

	if ((*pctx).intrinsic)
	    s_copy("command", tmpstr);
	else
	    s_copy("proc", tmpstr);
	s_copy(name, helpblk.helpobj);
	if (s_equal(type, "subcmd"))		/* if help on subcommand      */
	    {
	    left_sbld((*pctx).pdf.name, name, tmpstr, buf);	/* build left header string	*/
	    code = helper(&db, tmpstr, buf, &nultitle, &helpblk);
	    }
	else
	    {
	    left_pbld((*pctx).pdf.name, (*pctx).subcmd, name,
		tmpstr, buf);			/* build left header string   */
 	    code = parm_help(v, (*t).l2hexist, &db, buf, &nultitle, &helpblk);
	    }
	if (code != SUCCESS) goto help_err;
	}
    tutctx.dispreq = TRUE;

    if (!NULLSTR(helpblk.nextcmd))
	{
	s_copy(helpblk.nextcmd, tutctx.prev_cmd);
	code = TUT_PREV;
	}
    return(code);

open_err:
    tutmsg("No help available for tutor mode.", "TAE-NOHELP", 0, 0, 0, 0, 0);
    return(FAIL);

proc_err:					/* no .help for the proc	*/
    tutmsg("No help available for '%s'.", "TAE-NOHELP", 
	   (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);
    return(FAIL);

entry_err:
    tutmsg("Invalid subcommand help in tutor mode.", "TAE-TINVSUBHLP",
	   0, 0, 0, 0, 0);
    return(FAIL);

subc_err:
    if (code == AMBIG)
      tutmsg(msg_ambs, key_ambs, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
    else
      tutmsg(msg_unrs, key_unrs, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* unrecognized subcommand		*/
    return(FAIL);

parm_err:
    if (code == AMBIG)
      tutmsg(msg_ambi, key_ambi, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
    else
      tutmsg(msg_unre, key_unre, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* unrecognized parameter		*/
    return(FAIL);

over_err:
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);
    return(FAIL);

lev2_err:
    if (s_equal(type, "subcmd") && code == TUT_NOL2HELP)
	tutmsg("No help available for subcommand '%s'.",
	       "TAE-TNOLEV2", (uintptr_t) name, 0, 0, 0, 0);
    return(FAIL);

read_err:
    tutmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);
    return(FAIL);

help_err:
    tutmsg(helpblk.errmsg, helpblk.errkey, 0, 0, 0, 0, 0);
				/* print error message		*/
    return(FAIL);
    }

/* tuthold - perform tutor mode HOLD command.  This command is used only
 *	     to exit a dynamic tutor session started for an async job
 *	     while saving the parameters for some other time.
 *
 */

FUNCTION CODE tuthold 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    return (TUT_HOLD);			/* hold means get out		*/
    }

/* tutlist - perform NOSCREEN tutor LIST command.
 */

FUNCTION CODE tutlist 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct VARIABLE	*v;
    struct SUBCMD	*s;
    TEXT		*varname[MAXVAL];
    COUNT		numvar;
    COUNT		i;
    CODE		code;

    if (tutctx.screen)
	{
	  tutmsg(msg_nscr, key_nscr, (uintptr_t) "LIST", 0, 0, 0, 0);
	return(FAIL);
	}
    code = gettvnam(cmdstr, varname, &numvar);	/* get variable (or subc) name list*/
    if (code != SUCCESS) return(FAIL);
    if (tutctx.subtut)				/* if subcommand tutor			*/
	{
	if (NULLSTR(cmdprm))
	    for (s = (*pctx).subptr; s != NULL; s = (*s).link)
		list_subc(s);
	else
	    for (i = 0; i < numvar; i++)
		{
		code = subab((*pctx).subptr, varname[i], &s);
		if (code != SUCCESS)
		    {
		    if (code == AMBIG)
		      tutmsg(msg_ambs, key_ambs, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
		    else
		      tutmsg(msg_unrs, key_unrs, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* unrecognized subcommand		*/
		    continue;
		    }
		list_subc(s);
		}
	}
    else				/* parm tutor				*/
	{
	if (NULLSTR(cmdprm))
	    for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)
		list_parm(v);
	else
	    for (i = 0; i < numvar; i++)
		{
		code = lookab(&(*pctx).parmst, varname[i], &v);
		if (code != SUCCESS)
		    {
		    if (code == AMBIG)
		      tutmsg(msg_ambi, key_ambi, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
		    else
		      tutmsg(msg_unre, key_unre, (uintptr_t) varname[i],
			     0, 0, 0, 0);	/* unrecognized parameter		*/
		    continue;
		    }
		list_parm(v);
		}
	}
    return(SUCCESS);
    }

/* tutmsghlp - perform tutor mode HELP on last message key.
 *
 * NOTE: In case of error, formatted error message and key are returned by
 * msg_help() in helpblk struct.
 */

FUNCTION CODE tutmsghlp 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    IMPORT  TEXT	lastkey[];	/* key of last displayed message	*/

    CODE		code;
    struct  HELPBLK	helpblk;	/* help ocntrol block			*/
    struct  SFILE	tutmhlp;	/* tutor mode help file			*/

    helpblk.nextcmd[0] = EOS;				/* initialize		*/
    code = msg_help(&tutmhlp, lastkey, &helpblk);	/* display help data on key	*/
    if (code != SUCCESS)
	{
	  tutmsg(helpblk.errmsg, helpblk.errkey, 0, 0, 0, 0, 0);		/* print error message	*/
	return(FAIL);
	}
    else
	{
	tutctx.dispreq = TRUE;

	if (!NULLSTR(helpblk.nextcmd))
	    {
	    s_copy(helpblk.nextcmd, tutctx.prev_cmd);
	    code = TUT_PREV;				/* previous mode cmd 	*/
	    }
	}
    return(code);
    }

/*
 *	tutnoscreen.  NOSCREEN command.
 */

FUNCTION CODE tutnoscreen 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    tutctx.screen = FALSE;		/* noscreen mode and...		*/
    tutctx.crt_messages = FALSE;	/* scroll error messages	*/
    pos_scroll();			/* position at the bottom 	*/
    return (SUCCESS);
    }

/* tutpage - perform tutor mode PAGE command.
 */

FUNCTION CODE tutpage 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    TAEINT		num;
    COUNT		pagnum;		/* tutor display page number		*/
    CODE		code;

    if (!tutctx.screen)
	{
	  tutmsg("PAGE command not available in NOSCREEN tutor.", "TAE-TPAGNSC",
		 0, 0, 0, 0, 0);
	return(FAIL);
	}
    if (NULLSTR(cmdprm))
	{
	if (tutctx.srch1cplt  &&  tutctx.curpag == tutctx.lastpag)
	    pagnum = 1;
	else
	    pagnum = tutctx.curpag + 1;
	}
    else if (s_s2i(cmdprm, &num) == SUCCESS)
	{
	if (num <= 0)
	    {
	      tutmsg("Page number must be positive.", "TAE-TPAGNUM",
		     0, 0, 0, 0, 0);
	    return(FAIL);
	    }
	else
	    pagnum = num;
	}
    else					/* assume request PAGE to specifi parm of subc*/
	{
	if (tutctx.subtut)
	    code = getspnum(pctx, cmdprm, &pagnum, hf);	/* get pg # of subc (help read as necess)*/
	else
	    code = getpgnum(pctx, cmdprm, &pagnum, hf);	/* get pg # of parm (help read as necess)*/
	if (code != SUCCESS)  return(FAIL);
	}
    tutctx.dispreq = TRUE;
    if (tutctx.srch1cplt  &&  pagnum > tutctx.lastpag)
	tutctx.curpag  = tutctx.lastpag;
    else
	tutctx.curpag  = pagnum;
    if (!tutctx.subtut)
	{
	code = 0;				/* to indicate vcur not set */
	if ((!NULLSTR(cmdprm)) && (s_s2i(cmdprm, &num) != SUCCESS))
	    code = lookab (&(*pctx).parmst, cmdprm, &tutctx.vcur); /* point to variable */
	if (code != SUCCESS)
	    tutctx.vcur = get_parm1 (pctx, tutctx.curpag, hf);
	tutctx.index = 0;
	tutctx.start = TRUE;			/* latch on this vcur	*/
	}
    return(SUCCESS);
    }

/*	tutprmeq - perform tutor mode parm= commands.
 */

FUNCTION CODE tutprmeq 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {	
    struct SYNBLK	sb;
    TEXT		name[NAMESIZ+1];
    COUNT		subscr;		/* subscript.  0 if no subscript in cmd*/
    COUNT		fillcomp;	/* fill component		*/
    struct VARIABLE	*v;
    CODE		code, qualcode, trailcode = 0;
    COUNT		pagnum, expcount;
    struct TUTEXT	*t;
    TEXT		*value[MAXVAL];
    TEXT		*pos;
    COUNT		count;
    TEXT		token[TOKESIZ+1];
    CODE		type;
    COUNT		qualpos;	/* position of qualifier delimiter */
    TEXT		qualstr[STRINGSIZ+1];	/* qualifier part of parm=a|b| */

/*
 *	Get VARIABLE pointer v and subscript subscr
 */

    initok(&sb, cmdstr);
    if (gettpnam(&sb, name, &subscr) != SUCCESS)  /* get name and subscr */
	return(FAIL);					
    if ((code = lookab(&(*pctx).parmst, name, &v)) != SUCCESS)
	{
	if (code == AMBIG)
	  tutmsg(msg_ambi, key_ambi, (uintptr_t) name, 0, 0, 0, 0);	/* ambiguous abbreviation*/
	else
	  tutmsg(msg_unre, key_unre, (uintptr_t) name, 0, 0, 0, 0);	/* unrecognized parameter*/
	return(FAIL);
	}
    type = (*v).v_type;
    if (type == V_NAME || subscr <= (*v).v_count)
        fillcomp = 0;
    else
        fillcomp = ((*v).v_count < 0) ? 1 : (*v).v_count + 1;

/*
 *	contruct a value vector (in string form) for the parameter
 *	and set the parameter's value with tut_set_value.
 */

    if (subscr > 0)				/* if subscript exists	*/
        {
        code = getval (&sb, value, 1, &count);
        if (code == S_SYNERR)
    	    goto syntax_error;
        if (code == EOS)
	    count = -1;				/* no value		*/
	}
    else
        {
	pos = sb.curchr;			/* save position	*/
	code = gettok (&sb, token);
	if (code == S_WHITE)
	    code = gettok (&sb, token);
	if (code == EOS)
	    count = -1;				/* no value		*/
	else
	    {
	    sb.curchr = pos;			/* restore position	*/
	    if (dash_dash(&sb)  &&  chkend(&sb) == SUCCESS)
	        count = 0;			/* null value		*/
	    else
	        {
		if (code != '(')		/* values not in parens	*/
		    {
		    /* place parens around non-qualifier portion of
		       user-supplied parameter values - saves extra typing */
		    qualpos = s_index (sb.curchr, QUAL_SYM);
		    if (qualpos >= 0)
			{
			s_copy (sb.curchr, qualstr);	/* copy entire line */
			s_shift (qualstr, qualpos);	/* shift param out */
			sb.curchr = sb.curchr + qualpos;/* pos to where qual char is */
			*(sb.curchr) = EOS;		/* "delete" qualifier */
			sb.curchr = pos;		/* repos to start of param */
			}
		    else
			qualstr[0] = EOS;
		    parenval(&sb);	/* slap parens on param values string*/
		    s_append (qualstr, sb.curchr);	/* reconstruct line w/parens */
		    }

		expcount = (type == V_NAME) ? 1 : (*v).v_maxc;
		code = getval (&sb, value, expcount, &count);
    		if (code != SUCCESS)
    		    goto syntax_error;
		}
	    }
	}
    code = tut_set_value (pctx, v, subscr, count, value);
    if (code != SUCCESS)
        goto fail_exit;
    qualcode = tut_pqlf_upd (&sb, v, pctx);  /* update parm quals from cmd line */
    if (qualcode == SUCCESS)
	trailcode = chkend(&sb);		/* check for end of line */


/*
 *	Value has been set.  Now adjust screen.  Note that screen is adjusted
 *	even if qualifiers or end of line is in error since value already set.
 */
    t = (*v).v_tp;
    if (t == NULL  ||  (*t).pagnum == 0)
	{
	if (getpgnum(pctx, (*v).v_name, &pagnum, hf) != SUCCESS)
	    goto fail_exit;
        t = (*v).v_tp;			/* new TUTEXT pointer	*/
	}

    /* TBD: getpgnum only fails here if mem overflow. Bug?	*/
    if (tutctx.curpag == (*t).pagnum && NULLSTR(tutctx.prev_cmd))
	{
	if (tutctx.screen)
	    {
	    if (subscr > 0)
		{
		if (subscr >= (*t).pantop  &&  subscr <= (*t).panbot)
		    dispcomp(v, subscr, fillcomp);	/* display value component	*/
		else
		    {
		    if (inipan(v) != SUCCESS) goto over_err;	/* reinit val panel posit array*/
		    adjval(v, subscr);		/* adjust panel top to this component*/
		    dispval(v);			/* display value		*/
		    }
		}
	    else
		{
		if (inipan(v) != SUCCESS) goto over_err;	/* reinit val panel posit array*/
		adjval(v, 1);		/* adjust panel top to 1st component	*/
		dispval(v);		/* display value			*/
		}
	    }
	}
    else
	{
	if (subscr > 0)
	    adjval(v, subscr);		/* adjust panel top & bottom		*/
	else
	    adjval(v, 1);
	tutctx.dispreq = TRUE;
	tutctx.curpag  = (*t).pagnum;
	}

    if (qualcode != SUCCESS)		/* error with param quals ?	*/
	goto fail_exit;			/* yes				*/
    else if (trailcode != SUCCESS)	/* error with end of line ?	*/
	goto trail_error;		/* yes				*/
    tutctx.vcur = v;
    tutctx.index = (subscr == 0) ? 0 : subscr - 1;
    tutctx.start = FALSE;		/* success, so don't latch here	*/
    return(SUCCESS);

syntax_error:				/* no free of value vector req'ed  */
    tutmsg (sb.errmsg, "TAE-INVPVAL", 0, 0, 0, 0, 0);
    goto fail_exit;

trail_error:				/* here, value free of value req'ed*/
    tutmsg("Unexpected characters at end of value.", "TAE-TUNEXCH",
	   0, 0, 0, 0, 0);
    free_val (value, count);
    goto fail_exit;

over_err:				/* note: tut_set_value  did free*/
    tutmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);	
				/* report memory overflow	*/
    goto fail_exit;
/*				
 *	Here when "v" and "subscr" are valid but the value is not
 *	(The editor must be told who the "current" parm is.)	
 */
fail_exit:
    if (subscr <= (*v).v_maxc)
	{
	tutctx.vcur = v;
	tutctx.index = (subscr == 0) ? 0 : subscr - 1;
	tutctx.start =TRUE;			/* latch here because error	*/
	}
    return (FAIL);
    }

/*
 * tutqual - perform tutor QUALIFY command.
 */

FUNCTION CODE tutqual 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in/out:  the command parameter if any*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    struct TUTCTX	save_tutctx;	/* saved parent tutor context		*/
    struct CONTXT	proc_child;	/* child proc context			*/
    BOOL		prev_avail = 0;
    CODE		code;
    COUNT		pagnum;
    BOOL		new_screen_mode;
    struct VARIABLE	*v_parm;

    code = getpgnum (pctx, cmdprm, &pagnum, hf);/* prepare for return...*/
    if (code != SUCCESS)			/* from qual space	*/
	goto getpg_error;
    v_parm = ini_p_child (pctx, cmdprm, &proc_child); /* init child proc context*/
    if (v_parm == NULL)				/* ptr to parm being qualif'd	*/
	goto ini_p_error;
    MOVE_STRUCT (tutctx, save_tutctx);		/* save the parent tutor context*/
    code = ini_t_child (&tutctx, &proc_child,
			(*v_parm).v_name);	/* initialize child tutor context*/
    if (code != SUCCESS)
	goto ini_t_error;

    if (!tutctx.screen)
	listqnam (&proc_child.parmst, (*v_parm).v_name); /* list desired qual names	*/
    while (FOREVER)
	{
	if (tutctx.screen  &&  NULLSTR(tutctx.prev_cmd))
	    {
	    code = dispbld(&proc_child, &tutctx.title, tutctx.helpf);
					/* display a page & do help reads as necessary*/
	    if (code != SUCCESS)
		goto dispb_error;
	    }
	if (!NULLSTR(tutctx.held_msg))
	    {
	    tutctx.msg_held = TRUE;		/* force output of held message */
	    tutmsg(tutctx.held_msg, tutctx.held_key, 0, 0, 0, 0, 0);
	    tutctx.held_msg[0] = EOS;
	    }
	code = q_get_exec (&proc_child);	/* get and execute qual displ cmd*/
	if (code == TUT_ACCEPT)
	    {
	    break;
	    }
	if (code == TUT_PREV)
	    prev_avail = TRUE;
	if (prev_avail)
	    prev_avail = FALSE;
	else
	    tutctx.prev_cmd[0] = EOS;		/* cmd retnd from help gets executed once before display*/
	}

    repair_parent (&proc_child, cmdprm, pctx);
    new_screen_mode = tutctx.screen;	/* carry SCREEN/NOSCREEN flag back to parent*/
    MOVE_STRUCT (save_tutctx, tutctx);	/* restore the parent tutor context	*/
    tutctx.screen  = new_screen_mode;
    tutctx.dispreq = TRUE;
    tutctx.curpag  = pagnum;
    tutctx.vcur    = get_parm1 (pctx, pagnum, hf);
    tutctx.index   = 0;
    tutctx.start   = TRUE;
    return(SUCCESS);

getpg_error:
ini_p_error:
    return(FAIL);

ini_t_error:
    repair_parent (&proc_child, cmdprm, pctx);
    return(FAIL);

dispb_error:
    repair_parent (&proc_child, cmdprm, pctx);
    MOVE_STRUCT (save_tutctx, tutctx);	/* restore the parent tutor context	*/
    return(TUT_EXIT);
    }

/* tutrstr - perform tutor mode RESTORE command.
 * Read parameter file from disk.
 */

FUNCTION CODE tutrstr 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    TEXT		errmsg[STRINGSIZ+1];
    CODE		code;
    struct VARIABLE	*v;

    code = rest_parm(cmdprm, pctx, errmsg);
    if (code == SOME_REJECTED)
	{
	tutctx.msg_held = TRUE;
	s_copy(errmsg, tutctx.held_msg);
	s_copy("TAE-TNOTUSED", tutctx.held_key);
	}
    else if (code != SUCCESS)
    	{
	  tutmsg(errmsg, "TAE-BADPFILE", 0, 0, 0, 0, 0)
;				/* rest_parm builds the msg	*/
        return(FAIL);
    	}
    for (v = (*pctx).parmst.link; v != NULL; v = (*v).v_link)
	{
	inipan(v);			/* init value panel posit array		*/
	adjval(v, 1);			/* adjust panel top to 1st component	*/
	}
    tutctx.dispreq = TRUE;		/* force redisplay of page		*/
    return(SUCCESS);
    }

/*	tutrun - perform tutor mode RUN command.
 */

FUNCTION CODE tutrun 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    TEXT		mislst[STRINGSIZ+1];
    CODE		code;
    COUNT		m;
    struct ITRCMD	*itrcmd;
    TEXT		errmsg[STRINGSIZ+1];
    struct  VARIABLE	*v;
    CODE		runt;


    code = tut_runparms (pctx, cmdstr);		/* set command line values */
    if (code != SUCCESS)			/* no run if anything bad  */
	{
	tutctx.dispreq = TRUE;			/* re-display to show 	   */
	return (FAIL);				/* 'latched' values	   */
	}
    m = misprm(&(*pctx).parmst, mislst);
    if (m > 0)
	{
	tutmsg("Missing parameter%s:  %s", "TAE-TMISPAR",
	       (uintptr_t) (m > 1  ?  "s" : ""), (uintptr_t) mislst, 
	       0, 0, 0);
	return(FAIL);
	}
    if (tutctx.screen)
	pos_scroll();				/* position at the bottom 	*/
    if ((*pctx).intrinsic)		/* if we're "running" a TM intrinsic	*/
	{
	itrcmd = intrin((*pctx).pdf.name);
	itrcmd = itrsub((*pctx).subcmd, itrcmd);
	if (chk_do((*pctx).backlink, itrcmd) != SUCCESS)
	    return(TUT_EXIT);
	code = save_parm (LAST_FILE, pctx, F_OVER, errmsg);
	if (code != SUCCESS)
	  tutmsg(errmsg, "TAE-SAVELAST", 0, 0, 0, 0, 0);
	if ((*itrcmd).flags & Y_CLEAR)
	    ini_status();
	return(TUT_RUNSUB);
	}
    else				/* if we're running a proc		*/
	{
	runt = runtype (&(*pctx).qualst);	/* synch or asynch proc? */
	v = lookex(&(*pctx).qualst, "SAVE");
	if (v != NULL && !(*v).v_default)	/* save file if SAVE qualifier */
	    code = save_parm (SVAL(*v,0), pctx, F_WRITE, errmsg);
	code = save_parm (LAST_FILE, pctx, F_OVER, errmsg);	/* also save last	*/
	if (code != SUCCESS)
	  tutmsg(errmsg, "TAE-SAVELAST", 0, 0, 0, 0, 0);
        if (tutctx.screen)
	    pos_scroll();			/* position at the bottom 	*/
	ini_status();				/* reset sfi, skey	*/
	return(TUT_RUNSUB);
	}
    }

/* tutsav - perform tutor mode SAVE command.
 * Write V-block to disk.
 */

FUNCTION CODE tutsav 
(
    TEXT		cmdstr[],	/* in:  command string		*/
    TEXT		cmdprm[],	/* in:  the command parameter 	*/
    struct CONTXT	*pctx,		/* in:  proc context		*/

    struct SFILE	*UNUSED(hf) /* in/out: help file control block	*/
 )
    {
    TEXT		errmsg[STRINGSIZ+1];
    CODE		code;

    code = save_parm(cmdprm, pctx, F_WRITE,  errmsg);
    if (code != SUCCESS)
    	{
	  tutmsg(errmsg, "TAE-SAVERR", 0, 0, 0, 0, 0);		/* save_parm builds the msg */
        return(FAIL);
    	}
    return(SUCCESS);
    }

/*	tutscreen.  SCREEN command.
 *
 */
FUNCTION CODE tutscreen 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    tutctx.screen = TRUE;
    tutctx.dispreq = TRUE;		/* force re-paint		*/
    tutctx.crt_messages = TRUE;		/* messages on message line	*/
    return (SUCCESS);
    }

/* tutscrol - perform tutor mode command that handles <CR>.
 */

FUNCTION CODE tutscrol 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    if (!tutctx.screen)
	return(SUCCESS);
    if (tutctx.curpag == tutctx.lastpag)
	tutctx.curpag = 1;
    else
	tutctx.curpag++;
    tutctx.dispreq = TRUE;
    if (!tutctx.subtut)				/* if not subcmd tutor...*/
    	{
	tutctx.vcur = get_parm1 (pctx, tutctx.curpag, hf);
	tutctx.index = 0;
	tutctx.start = TRUE;			/* latch here		*/
    	}
    return(SUCCESS);
    }

/*	tutset.   Perform the SET command (like RUN but no RUN).
 *
 */
FUNCTION CODE tutset 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/
	
 )
    {
    CODE code;

    code = tut_runparms (pctx, cmdstr);		/* set values		*/
    tutctx.dispreq = TRUE;			/* re-paint -- no way...*/
    return (code);				/* to know changes	*/
    }


/* tutshow - perform tutor show command.
 * Note:  the command parameter, if subscripted, is delivered sans the ")".
 */

FUNCTION CODE tutshow 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in/out:  the command parameter if any*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    COUNT		parenpos;	/* position of "(" if command parameter	*/
    TAEINT		compnum;	/* component number			*/
    COUNT		pagnum;		/* display page number			*/
    CODE		code;
    struct VARIABLE	*v;

    if (tutctx.edtcmd)			/* command generated by editor?	*/
        {
	v = tutctx.vcur;		/* SHOW this variable		*/
	compnum = tutctx.index+1;	/* SHOW this index		*/
	code = getpgnum (pctx, (*v).v_name, &pagnum, hf);
	if (code != SUCCESS) return (FAIL);
	}
    else
	{
	if (NULLSTR(cmdprm))
	    {
	      tutmsg("The SHOW command needs a variable name.", "TAE-TSHOWPRM",
		     0, 0, 0, 0, 0);
	    return(FAIL);
	    }
	parenpos = s_index(cmdprm, '(');	/* find the "(" 	*/
	if (parenpos >= 0)			/* if subscripted	*/
	    {
	    code = s_s2i(&cmdprm[parenpos+1], &compnum);  /* get index */
	    if (code != SUCCESS) goto subscr_err;
	    cmdprm[parenpos] = EOS;		/* shorten to just the parm name	*/
	    }
	else
	    compnum = 1;
	code = getpgnum(pctx, cmdprm, &pagnum, hf);	/* get page number */
	if (code != SUCCESS) return(FAIL);
	code = lookab(&(*pctx).parmst, cmdprm, &v);
	if (code != SUCCESS) goto name_err;
	tutctx.vcur = v;			/* build editor context	*/
	tutctx.index = compnum - 1;
	tutctx.start = TRUE;			/* latch at this position */
	}
    if (!tutctx.screen)
	list_parm(v);
    else
	{
	if (pagnum == tutctx.curpag)
	    {
	    adjval(v, compnum);			/* adjust panel top to new component #*/
	    dispval(v);				/* display the value		*/
	    }
	else
	    {
	    adjval(v, compnum);			/* adjust panel top & bottom	*/
	    tutctx.dispreq = TRUE;		/* flag that new display reqd	*/
	    tutctx.curpag  = pagnum;		/* change global display pg #	*/
	    }
	}
    return(SUCCESS);

subscr_err:
    tutmsg(msg_sbin, key_sbin, 0, 0, 0, 0, 0);
    return(FAIL);

name_err:
    if (code == AMBIG)
      tutmsg(msg_ambi, key_ambi, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
    else
      tutmsg(msg_unre, key_unre, (uintptr_t) cmdprm, 0, 0, 0, 0);	/* unrecognized parameter		*/
    return(FAIL);
    }

/* tutsubeq - perform tutor subcommand mode "-subcommand" command.
 */

FUNCTION CODE tutsubeq 
(
    TEXT		cmdstr[],	/* in:  command string			*/
    TEXT		cmdprm[],	/* in:  the command parameter if any	*/
    struct CONTXT	*pctx,		/* in:  proc context			*/
    struct SFILE	*hf		/* in/out: help file control block	*/

 )
    {
    TEXT		name[SUBCMDSIZ+1];	/* subcommand name		*/
    CODE		code;
    struct SYNBLK	sb;
    struct SUBCMD	*s;

    initok (&sb, cmdstr);		/* init block for the syntax pkg	*/
    code = gettsnam(&sb, name);		/* get the name of the subc requested	*/
    if (code != SUCCESS) return(FAIL);
    if (NULLSTR(name))
	{
	for (s = (*pctx).subptr; s != NULL; s = (*s).link)
	    {
	    if ((*s).deflt)
		break;
	    }
	if (s == NULL)
	    goto nodef_err;
	}
    else
	{
	code = subab((*pctx).subptr, name, &s);		/* look up subc by abbreviation*/
	if (code != SUCCESS) goto subc_err;
	}
    s_copy((*s).name, (*pctx).subcmd);			/* save subc name in context*/
    return(TUT_TUTPARM);				/* signal done with subcommand phase*/

nodef_err:
    tutmsg("No default subcommand is defined for this command.", "TAE-TNODEFSUB", (uintptr_t) name, 0, 0, 0, 0);
    return(FAIL);

subc_err:
    if (code == AMBIG)
      tutmsg(msg_ambs, key_ambs, (uintptr_t) name, 0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
    else
      tutmsg(msg_unrs, key_unrs, (uintptr_t) name, 0, 0, 0, 0);	/* unrecognized subcommand		*/
    return(FAIL);
    }
