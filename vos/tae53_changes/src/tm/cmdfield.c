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



/* TPAM CHECKOUT FILE_TIME=16-DEC-1984 19:17 DUA1:[TAEV1.TM]CMDFIELD.C;1 */
/* TPAM CHECKOUT FILE_TIME=10-OCT-1983 19:02 DUA0:[TAEV1.TM]CMDFIELD.C;21 */
/*TPAM        CHECKOUT FILE_TIME=22-AUG-1983 18:41 DUA0:[TAEV1.TM]CMDFIELD.C;19 */
/*TPAM        CHECKOUT FILE_TIME=11-JUL-1983 16:11 DUA0:[TAEV1.TM]CMDFIELD.C;18 */
/*
 *	cmdfield.    Substitution and command field management.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	20-aug-83	Nullables: new rules for substitution;
 *			new, permissible syntax for substitution: &"x"
 *	02-sep-83	Detect dash presence in cmd_parse (PR 360)...palm
 *	10-oct-83	Fix unix lint errors...palm
 *	15-feb-84	New cmdmatch call for multi-$DEFCMDs...palm
 *	18-feb-84	Remove defcmd_gbl...palm
 *	04-may-84	VALUE_x to xVAL...ces
 *	26-oct-84	Improve introductory comments in cmd_parse()...peb
 *	27-jul-85	Speed up substitute...palm
 *	24-feb-88	PR1504: Change label dimension in cmd_parse()...ljn
 *
 *
 */
#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"fileinc.inp"	/* file package				*/
#include	"symtab.inc"	/* TM symbol table			*/
#include	"syninc.inc"	/* syntax package			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/
#include "taeintproto.h"

FUNCTION static CODE replace 
(
    TEXT		variab[],	/* in:  variable to be substituted*/
    struct CONTXT	*pctx,		/* in:  proc context		*/
    TEXT		out_str[],	/* out: replacement string	*/
    FUNINT		siz		/* in:  max chars in out_str	*/

 );


#define NOVARIABLE	20		/* replace return codes		*/
#define OVERFLOW	21
#define NOVALUE		22


/* 
 *	find_rep.   Find replacement string for command field defined
 *	by DEFCMD.  Note: Each  defcmd string should have the form:
 *		    command=replacement.
 *		    Otherwise the string would be ignored evenif the
 *		    command portion matches with the input command verb.
 */

FUNCTION TEXT *find_rep
(
    TEXT	verb[]			/* in  command verb		*/

 )
    {
    struct VARIABLE	*dv;
    COUNT		index;			/* index of matching command	*/
    TEXT		*sp;			/* pointer to matching string */
    COUNT		pindex;			/* index to '=' in sp			*/
    CODE		code;

    code = cmdmatch(verb, &dv, &index);
    if (code == SUCCESS)			/* matches with defcmd	*/
	{
    	sp = SVAL(*dv, index);			/* point to matching string */
	pindex = s_index(sp,'=');		/* index to = in matching str */
	if (pindex >= 0)			/* '=' located in string  */
 	    return(&sp[pindex+1]);		/* return string after '=' */
	}    
    return (NULL);				/* no match		*/
    }

/*	get_nowhite.     Gets next non-white token.
 */

FUNCTION CODE get_nowhite 
(
    struct SYNBLK	*sb,			/* in/out: syntax block	*/
    TEXT		token[TOKESIZ+1]	/* out: token		*/

 )
    {
    CODE	code;

    code = gettok(sb, token);
    if (code == S_WHITE)
        code = gettok(sb, token);
    return (code);
    }

/*
 *	cmd_parse.    Obtain label, command, and subcommand for a TCL
 *	command line.   The syntax rules are:
 *
 *		label > filespec- subcommand
 *
 *	(A space above means that whitespace is allowed.)
 *
 *	If return code is FAIL, then sb.errmsg contains the error.
 *
 *	If the subcmd output string is null, then subcmd [1] is
 *	used as a flag to tell whether a subcommand dash was 
 *	present: subcmd[1] = '-'.  This allows tutor and others
 *	to distinguish between a missing subcommand field (no dash)
 *	and a null subcommand field (dash but no subcmd).
 *
 *	cmd_parse, in parsing the filespec, performs string replacement, 
 *	if necessary, for those strings defined
 *	via the TCL DEFCMD command.  For this reason,
 *	the input string "cmdstr" may be modified by cmd_parse.
 */

FUNCTION CODE cmd_parse 
(
    struct SYNBLK	*sb,			/* out: syntax block	*/
    TEXT		cmdstr[CMDLINSIZ+1],	/* in/out: command str	*/
    TEXT		*label,			/* out: command label	*/
    TEXT		cmd[FSPECSIZ+1],	/* out: command		*/
    TEXT		subcmd[SUBCMDSIZ+1]	/* out: subcommand	*/

 )
    {
    TEXT		*save_pos;
    TEXT		token[TOKESIZ+1];
    TEXT		loc_cmdstr[CMDLINSIZ+1];
    TEXT		*replacement;
    COUNT		i;
    CODE		code;
    COUNT		len;
    BOOL		in_bracket;

    initok (sb, cmdstr);
    if (label)
	label[0] = EOS;
    cmd[0] = EOS;
    subcmd[0] = EOS;
    subcmd[1] = 0;
        
    /*	get label  */

    save_pos = (*sb).curchr;		
    code = get_nowhite (sb, token);
    if (code == S_SYNERR)
	goto gettok_error;
    else if (code == S_ALPHA || 
             code == S_QUOTED)			/* candidate for label	*/
        {
	if (label) 
       	    s_bcopy (token, label, LABELSIZ);	/* assume it's a label	*/
	code = get_nowhite (sb, token);
	if (code == S_SYNERR)
	    goto gettok_error;
	else if (code != '>')			/* if not a label so... */
    	    {				
	    if (label)
    	    	label[0] = EOS;			/* backtrack		*/
	    (*sb).curchr = save_pos;
    	    }
	}
    else
        (*sb).curchr = save_pos;		/* not label: re-pos	*/

    /*  here, we are positioned at begin of supposed command field	*/


/*	Get command field -- it may be an unquoted filespec. 	
 *	Note that the replacement string may contain (in addition
 *	to a command) subcommand and paramters.
 */
    save_pos = (*sb).curchr;
    code = get_nowhite(sb, token);
    if (code == S_SYNERR)
	goto gettok_error;
    else if (code == S_ALPHA)			/* candidate for DEFCMD?*/
        {	
        replacement = find_rep(token);		/* ptr to replacement	*/
	if (replacement != NULL)
	    {
	    i = s_copy (replacement, loc_cmdstr);
	    i = s_bcopy ((*sb).curchr, &loc_cmdstr[i], CMDLINSIZ-i);
	    s_copy (loc_cmdstr, cmdstr);	/* re-build cmdstr	*/
	    initok (sb, cmdstr);
	    }
	else
	    (*sb).curchr = save_pos;		/* false alarm: not def	*/
	}
    else
        (*sb).curchr = save_pos;		/* not DEFCMD candidate */

    /*  The following algorithm to gobble a file spec works for VMS, RSX,
     *  UNIX, and others.  A file spec is defined to be a sequence of tokens
     *  not greater than FSPECSIZ and terminated by comma, dash, EOS, or
     *  whitespace.  (For VMS/RSX, a dash or comma inside brackets do not 
     *	terminate the file spec.)
     */

    len = 0;
    in_bracket = FALSE;				/* not in bracket	*/
    save_pos = (*sb).curchr;
    code = get_nowhite(sb, token);
    if (code == S_SYNERR)
        goto gettok_error;
    while (FOREVER)				/* gobble filespec:	*/
        {
	if (code == '[')			/* if inside brackets,  */
	    in_bracket = TRUE;			/* comma/dash do not    */
	if (code == ']')			/* terminate		*/
	    in_bracket = FALSE;
	if (code == EOS     ||
	    code == S_WHITE ||
            (code == '-'   &&  !in_bracket) ||
	    code == '|'     ||
	    (code == S_COMSEP   &&  !in_bracket))
	    break;				/* end of command field	*/
	if (len + s_length(token) > FSPECSIZ)
	    {
	    synerr(sb, "Command too long ");
	    return (FAIL);
	    }
	len = s_append (token, cmd);
	save_pos = (*sb).curchr;
	code = gettok (sb, token);		/* for next time thru	*/
	if (code == S_SYNERR)
            goto gettok_error;
	}

/* 	Get subcommand.
 * 	Note that  whitespace between the command field and the dash
 *	is not permitted (because of confusion with positional negatives).
 */

    if (code == '-')				/* cmd ended with dash	*/
	{	    
	code = get_nowhite (sb, token);
	if (code == S_SYNERR)
            goto gettok_error;
	else 
	    {
	    s_bcopy(token, subcmd, SUBCMDSIZ);	/* copy to caller	*/
	    if (subcmd[0] == EOS) 
		subcmd[1] = '-';		/* flag dash present	*/
	    }
	}
    else
	(*sb).curchr = save_pos;		/* back to white/comma	*/
    
    if (label)
    	strpqu(label);				/* strip quotes		*/
    strpqu(cmd);	
    strpqu(subcmd);
    return (SUCCESS);




/*
 *	The only error being reported here is gettok's S_SYNERR which can
 *	only mean that a token is too long.  We play down this highly
 *	unlikely error with a catch all here.
 */

gettok_error:
    synerr(sb, "Error in command field format");
    return (FAIL);
    }

/*	substitute - do command line substitution in place.
 *	Strategy: build local buffer then copy to caller.
 *
 *	The 'report' flag exists because if substitution fails in
 *	a false part or in search mode, we do not complain.  However
 *	if substitution is successful, the substitution may be 
 *	used to generate an ELSE or a label (which might terminate
 *	the false part or search mode).
 *
 *	CAUTION: the efficiency here is critical to TAE performance.
 *	
 */
FUNCTION CODE substitute 
(
    char*		cmdstr,   /* [siz] */	/* in/out: command string*/
    int		        siz,			/* in:  dim-1 of cmdstr  */
    GENPTR              pctx,			/* in:  proc context	 */
    int 		report			/* in:  true if err msg report	*/
						/*      is desired		*/
 )
    {
    TEXT	chr, locbuf[CMDLINSIZ+1],  variab[TOKESIZ+1];
    COUNT		len;		
    struct SYNBLK	sb;		
    CODE		code;

    if (s_index(cmdstr, '&') < 0)	/* optimize for most probably case */
	return (SUCCESS);		/* (i.e., no substitution)	   */
    initok(&sb, cmdstr);	
    locbuf[0] = EOS;
    len       = 0;
    siz = (siz > CMDLINSIZ) ? CMDLINSIZ : siz;		/* max siz allowed  */
    while ((chr = *(sb.curchr++)) != EOS)		/* walk thru cmdstr */
	{
	if (chr == '&')
	    {
	    if ((code = gettok(&sb, variab)) == S_SYNERR)
		goto syntax_err;
	    if (code == S_ALPHA || code == S_QALPHA || code == S_QUOTED)
		{
		strpqu(variab);				/* strip quotes    */
		code = replace(variab, (struct CONTXT* )pctx, 
			       &locbuf[len], siz-len);
		if (code == NOVARIABLE) goto undefined;
		if (code == NOVALUE)    goto novalue;
		if (code == OVERFLOW)   goto too_long; 
		len = s_length (locbuf);
		}
	    else
		{
		if (len + 1 > siz) goto too_long;
		len = s_append ("&", locbuf);
		if (code != '&')			/* catch &&	*/
		    {
		    if (len + s_length (variab) > siz) goto too_long;
		    len = s_append (variab, locbuf);
		    }
		}
	    }
	else						/* else: no substitn */
	    {
	    if (len + 1 > siz)
	        goto too_long;
	    locbuf[len++] = chr;	
	    locbuf[len] = EOS;
	    }
	}
    s_copy(locbuf, cmdstr);				/* copy to output*/
    return(SUCCESS);

syntax_err:
    if (report)
	{
	sb.errchr = sb.curchr - 1;
	synerr(&sb, "Invalid variable name in substitution");
	tmmsg(PROCFAIL, sb.errmsg, "TAE-INVSUBNAME", 0, 0, 0, 0, 0);
	}
    return(FAIL);

undefined:
    if (report)
        tmmsg(PROCFAIL, "Reference to undefined variable '%s'.",
	      "TAE-UNDEFVAR", (uintptr_t) variab, 0, 0, 0, 0);
    return(FAIL);

too_long:
    if (report)
        tmmsg(PROCFAIL, "Command line too long following substitution.", 
	      "TAE-LONGCMDLINE", 0, 0, 0, 0, 0);
    return(FAIL);

novalue:
    if (report)
	tmmsg (PROCFAIL, "Variable '%s' has no value.", "TAE-NOVALUE", 
	       (uintptr_t) variab, 0, 0, 0, 0);
    return (FAIL);
    }

/*	replace - substitute one variable.
 *
 *	Return codes:
 *		NOVARIABLE  -- variable does not exists
 *		OVERFLOW  --  buffer not large enough
 *		SUCCESS --  ok
 */

FUNCTION static CODE replace 
(
    TEXT		variab[],	/* in:  variable to be substituted*/
    struct CONTXT	*pctx,		/* in:  proc context		*/
    TEXT		out_str[],	/* out: replacement string	*/
    FUNINT		siz		/* in:  max chars in out_str	*/

 )
    {
    struct VARIABLE	*v;
    TEXT		str[STRINGSIZ+1];
    COUNT		i;
    COUNT		out_siz;

    out_str[0] = EOS;
    out_siz = 0;
    v = search(variab, pctx);			/* find variable	*/
    if (v == NULL)
	return(NOVARIABLE);
    if ((*v).v_count == -1)
	return (NOVALUE);
    if ((*v).v_count > 1) {
	if (1 > siz)
	    return (OVERFLOW);
	else
	    out_siz = s_append("(", out_str);
    }
    for (i=0; i < (*v).v_count; i++)		/* note: v_count may be	*/
	{					/* zero and okay by us	*/
	if ((*v).v_type == V_INTEGER)		
	    s_i2s(IVAL(*v, i), str);		/* make character form 	*/
	else if ((*v).v_type == V_REAL)
            s_r2ws(RVAL(*v, i), str);
	else if ((*v).v_type == V_STRING)
	    s_copy(SVAL(*v, i), str);
	if (out_siz + s_length(str) + 1 > siz)
	    return (OVERFLOW);
	s_append (str, out_str);		/* and append to out_str */
	out_siz = s_append (",", out_str);
	}
    if (out_siz > 0)
        out_str[out_siz - 1] = EOS;		/* remove the trailing ","  */
    if ((*v).v_count > 1) {
	if (out_siz + 1 > siz)
	    return (OVERFLOW);
	else
	    s_append(")", out_str);		/* append the closing
						   paren */
    }
    return(SUCCESS);
    }
