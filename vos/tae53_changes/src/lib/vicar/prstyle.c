/*	Dynamic parameter handling for prompt style.
 *	The acronym "ps" is used throughout this source to indicate
 *	prompt style.
 */

/*	Change log:
 *
 *	24-sep-83	Add pssub()...peb
 *	08-dec-83	Add intermed member name for CONTXT.name...peb
 *	08-dec-83	New called fcn names: t_highlight, line_noscreen,...peb
 *	10-dec-83	Change one error msg to avoid conflict w/ TAE...peb
 */

/*
 *	Standard TAE include files.
 */

#include "stdh.inp"		/* system standard  (REQUIRED)		*/
#include "taeconf.inp"		/* TAE configuration (REQUIRED)		*/

#include "tmhost.inp"		/* host dependent defs			*/
#include "symtab.inc"		/* TM symbol table			*/
#include "syninc.inc"		/* syntax package			*/
#include "terminc.inc"		/* terminal package			*/
#include "tminc.inc"		/* TM definitions			*/
#include "parblk.inc"		/* For definition of M_CONTINUE		*/
#include "helpinc.inc"		/* HELP output control block		*/
#include "taeintproto.h"
#include "vicartae.inc"		/* VICAR-specific definitions		*/

IMPORT BOOL procs_allowed;	/* true if external procs allowed */

#define	PS_ESCAPE	SUCCESS+1
#define	PS_PROMPTSIZ	80	/* max size of an automatic prompt line	*/

static BOOL		still_sub;	/* TRUE if still getting subc	*/
static struct VARIABLE	*cur_mandatory;	/* current mandatory parm	*/
static struct SFILE	*saved_pdf;	/* saved pdf			*/
static BOOL		tutor_sub = FALSE;  /* TRUE if we've tutored on subcs*/
static BOOL		prompted_subc = FALSE;	/* TRUE if subc was requested*/

/* Local intrinsic command table.  These commands are for prompt style
 * parameter mode escape mode command space only.
 * They are meant to be accessed by ps_intrin() and ps_itrsub().
 */

    static TEXT		*null_string = "";

    static struct RESIDVAR pt_pshp1[] =	    /* HELP cmd PDF for prompt style */
	{
	/* name    type      k  m maxc   size     dc val      dvp*/

	  {"PROC",    V_STRING, 0, 1, 1,   FSPECSIZ, 1, NULL, (GENPTR)&null_string}
	};

    static struct RESIDVAR pt_pshp2[] =	/* HELP-PARM cmd PDF for prompt style */
	{
	/* name    type      k  m maxc   size     dc val      dvp*/

	  {"PARM",	V_STRING,    0, 1, 1,	NAMESIZ,  1, NULL, (GENPTR)&null_string}
	};

#if INS_USE_EMACS
#if VMS_OS
    IMPORT CODE		emacs_do();
#endif
#endif
FUNCTION CODE ps_nothing_do
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 );
FUNCTION CODE ps_exit_do
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 );
FUNCTION CODE ps_help_do 
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 );
FUNCTION CODE ps_tutor_do 
(
    struct CONTXT	*procctx,
    struct CONTXT	*cmdctx

 );

    static struct ITRCMD ps_cmd[] =		/* escape mode cmd space cmds	*/
	{
{1, "ABORT",	"",	0,		0,   NULL,	ps_exit_do	},
{2, "EXIT",	"",	0,		0,   NULL,	ps_nothing_do	},
#if INS_USE_EMACS
#if VMS_OS
{2, "EMACS",	"",	0,		0,   NULL,	emacs_do	},
#endif
#endif
{1, "HELP",	"",	0, I_NPM(pt_pshp1), pt_pshp1,	ps_help_do	},
{1, "HELP", "PARM",	0, I_NPM(pt_pshp2), pt_pshp2,	ps_help_do	},
{1, "TUTOR",	"",	0, 		0,  NULL,	ps_tutor_do	},

{0, ""}		/* terminator entry:  required at end of table	*/
	};


FUNCTION static CODE ps_prccmd
(
    struct CONTXT	*procctx,	/* in/out: proc context		*/
    TEXT		cmdstr[]	/* in/out: the command string	*/

 );
FUNCTION static VOID put_initial
(
    struct VARIABLE	*v,		/* in:  _PROMPT variable	*/
    TEXT		*last_prompt	/* out: last line of prompt	*/

 );
FUNCTION static CODE ps_updval
(
    struct VARIABLE	*v,		/* in/out: parm to be updated	*/
    TEXT		cmdstr[]	/* in:  the value string	*/

 );



/*	auto_list.  Generate the automatic initial prompt.
 *	This is called if the application did not supply the initial prompt.
 */

FUNCTION static VOID auto_list
(
    struct SYMTAB	*st			/* in:  parm symbol table	*/

 )
    {
    struct VARIABLE	*v;
    TEXT		buf[STRINGSIZ+1];
    COUNT		len;

    len = s_copy("ENTER ", buf);
    for (v = (*st).link; v != NULL; v = (*v).v_link)
	{
	if (s_length((*v).v_name)+len+2 > PS_PROMPTSIZ)
	    {
    	    t_highlight(buf);
	    t_write(buf, T_STDCC);
	    buf[0] = EOS;
	    }
	len = s_append((*v).v_name, buf);	/* append this parm name	*/
	len = s_append(", ", buf);
	}
    buf[len-2] = EOS;				/* strip trailing ", "		*/
    t_highlight(buf);
    t_write(buf, T_STDCC);
    return;
    }

/*	auto_sublist.  Generate the automatic subcommand initial prompt.
 *	This is called if the application did not supply the initial prompt.
 */

FUNCTION static VOID auto_sublist
(
    struct SUBCMD	*subs		/* in:  1st member of SUBCMD chain */

 )
    {
    struct SUBCMD	*s;
    TEXT		buf[STRINGSIZ+1];
    COUNT		len;

    len = s_copy("Choose a subcommand: ", buf);
    for (s = subs; s != NULL; s = (*s).link)
	{
	if (s_length((*s).name)+len+2 > PS_PROMPTSIZ)
	    {
	    t_highlight(buf);
	    t_write(buf, T_STDCC);
	    buf[0] = EOS;
	    }
	len = s_append((*s).name, buf);		/* append this subc name	*/
	len = s_append(", ", buf);
	}
    buf[len-2] = EOS;				/* strip trailing ", "		*/
    t_highlight(buf);
    t_write(buf, T_STDCC);
    s_copy(">", buf);
    t_highlight(buf);
    t_write(buf, T_PROMPT);
    return;
    }

/*	esc_ps.  Escape prompt style input to allow entry of
 *	one of the following commands:
 *
 *		- HELP
 *		- EXIT
 *		- TUTOR
 *
 *	Return codes:
 *
 *		- DO_RUN
 *		- DO_EXIT
 *		- SUCCESS
 */
IMPORT struct VARIABLE *prompt_gbl;

FUNCTION static CODE esc_ps
(
    struct CONTXT	*procctx	/* in/out: dyn parms context	*/

 )
    {
    TEXT		new_prompt[STRINGSIZ+1];
    CODE		code;
    TEXT		cmdstr[CMDLINSIZ+1];

    s_copy((*procctx).pdf.name, new_prompt);	/* build prompt string	*/
    s_append("-ESCAPE", new_prompt);
    s_append(">", new_prompt);
    while (FOREVER)
	{
	cmd_noscreen(A_TUTOR,new_prompt, cmdstr); /* get interactive cmd line */
	if (NULLSTR(cmdstr))
	    continue;
	code = ps_prccmd(procctx, cmdstr);	/* process the command	*/
	if (code == DO_SUCCESS  ||  code == DO_RUN  ||  code == DO_EXIT)
	    return(code);
	}
    }

/*	get_ps.  Get an interactive prompt style command string.
 *	Gets full TCL command line, including continuation lines.
 *	NOTE:  This function is similar to get_psip and should be
 *	maintained in parallel to get_psip.
 *	On return, all continuation lines have been appended to the first
 *	line, with the "+" replaced by a blank separator.
 *	Comments and trailing blanks are stripped.
 *	Prompts the operator before accepting the input.
 *	Return codes:
 *
 *		- SUCCESS
 *		- PS_ESCAPE	if an immediate escape character was entered.
 *		- FAIL		if an error occurred reading script file.
 */

FUNCTION static CODE get_ps
(
    TEXT		pmtbuf[],		/* in:  prompt string	*/
    TEXT		cmdstr[CMDLINSIZ+1]	/* out: command string	*/

 )
    {
    BOOL	cont;			/* TRUE if next line is a continuation*/
    BOOL	start_over;		/* TRUE if start prompting over	*/
    CODE	term;			/* line terminator		*/
    CODE	code;
    TEXT	inbuf[STRINGSIZ+1];	/* single line input buffer	*/
    TEXT	locpmt[STRINGSIZ+1];

    cmdstr[0] = EOS;
    cont = FALSE;			/* start assuming no continuation line*/
    do
	{
	start_over = FALSE;
	s_copy(pmtbuf, locpmt);
	if (cont)			/* if this is a continuation line */
	    s_append("+", locpmt);	/* append a "+"			  */
	code = line_noscreen(
                  A_LASTCMD, locpmt,
                  inbuf, &term);      /* get input line (from oper or script) */
	if (code == F_FILERR)
	    {
	      m_put("Error reading script file.", "TAE-RDERR", 0, 0, 0, 0, 0);
	    cmdstr[0] = EOS;
	    return(FAIL);
	    }
	if (term == T_ESCAPE)
	    {
	    if (NULLSTR(inbuf)  &&  NULLSTR(cmdstr))
		return(PS_ESCAPE);
	    cont = FALSE;
	    start_over = TRUE;
	    cmdstr[0] = EOS;
	    continue;
	    }
	code = bldcmd(inbuf, cmdstr);	/* append cmd to full command string */
        cont = (code == CONTINUE) ;
	}
    while (cont  ||  start_over);	/* loop if continuation line	*/
    history_save(cmdstr);
#ifdef SESSION_LOG2
    sl2write(cmdstr,FALSE);		/* write to session log */
#endif
    return(code);
    }

/*	get_psip.  Get an interactive prompt style command string.
 *	This function is for the prompt style initial prompt only.
 *	The input prompt buffer	is not displayed before the first input line.
 *	The input prompt buffer is displayed for continuation lines.
 *	NOTE:  This function is otherwise identical to get_ps and should be
 *	maintained in parallel to get_ps.
 *	Gets full TCL command line, including continuation lines.
 *	On return, all continuation lines have been appended to the first
 *	line, with the "+" replaced by a blank separator.
 *	Comments and trailing blanks are stripped.
 *	Prompts the operator before accepting the input.
 *	Return codes:
 *
 *		- SUCCESS
 *		- PS_ESCAPE	if an immediate escape character was entered.
 *		- FAIL		if an error occurred reading script file.
 */

FUNCTION static CODE get_psip
(
    TEXT		pmtbuf[],		/* in:  prompt string	*/
    TEXT		cmdstr[CMDLINSIZ+1]	/* out: command string	*/

 )
    {
    BOOL	cont;			/* TRUE if next line is a continuation*/
    BOOL	start_over;		/* TRUE if start prompting over	*/
    CODE	term;			/* line terminator		*/
    CODE	code;
    TEXT	inbuf[STRINGSIZ+1];	/* single line input buffer	*/
    TEXT	locpmt[STRINGSIZ+1];

    locpmt[0] = cmdstr[0] = EOS;
    cont = FALSE;			/* start assuming no continuation line*/
    do
	{
	start_over = FALSE;
	s_copy(pmtbuf, locpmt);
	if (cont)			/* if this is a continuation line */
	    s_append("+", locpmt);	/* append a "+"			  */
	code = line_noscreen(
                   A_LASTCMD, locpmt,
                   inbuf, &term);     /* get input line (from oper or script) */
	if (code == F_FILERR)
	    {
	      m_put("Error reading script file.", "TAE-RDERR", 0, 0, 0, 0, 0);
	    cmdstr[0] = EOS;
	    return(FAIL);
	    }
	if (term == T_ESCAPE)
	    {
	    if (NULLSTR(inbuf)  &&  NULLSTR(cmdstr))
		return(PS_ESCAPE);
	    cont = FALSE;
	    start_over = TRUE;
	    cmdstr[0] = EOS;
	    continue;
	    }
	code = bldcmd(inbuf, cmdstr);	/* append cmd to full command string */
        cont = (code == CONTINUE) ;
	}
    while (cont  ||  start_over);	/* loop if continuation line	*/
    history_save(cmdstr);
#ifdef SESSION_LOG2
    sl2write(cmdstr,FALSE);		/* write to session log */
#endif
    return(code);
    }

/*      ps_exit_do  "do" function for prompt style exit command.   */

FUNCTION CODE ps_exit_do
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 )
   {
   return(DO_EXIT) ;
   }

/*	ps_help_do.  HELP "do" function for prompt style parameter mode.
 */

FUNCTION CODE ps_help_do 
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 )
    {
    IMPORT TEXT		next_cmd[];
    TEXT		*value[1];
    struct VARIABLE	*v;
    CODE		code;
    struct SFILE	fctx;
    TEXT		name[STRINGSIZ+1];
    struct HELPBLK	helpblk;	/* help output control block	*/

    if (s_equal((*cmdctx).subcmd, "PARM"))	/* if this is HELP-PARM	*/
	{
	v = lookex(&(*cmdctx).parmst, "PARM");	/* see if parm name defaulted */
	if (NULLSTR(SVAL(*v, 0)))		/* if parm name was defaulted */
	    {
	    if (cur_mandatory == NULL)
		{
		tmmsg(PROCFAIL, "You must specify a parameter name.",
		      "TAE-NOPRMNM", 0, 0, 0, 0, 0);
		return(DO_RETURN);
		}
	    value[0] = s_save((*cur_mandatory).v_name);
	    if (value[0] == NULL)
		{
		overr();
		return(DO_RETURN);
		}
	    code = set_value(v, (GENPTR) value, 1); /* par name=cur mandatory */
	    s_free(value[0]);
	    if (code != SUCCESS)
		{
		overr();
		return(DO_RETURN);
		}
	    }
	code = help_do(procctx, cmdctx);	/* now do the help on the parm*/
	}
    else					/* HELP on a proc	*/
	{
	v = lookex(&(*cmdctx).parmst, "PROC");
	if (NULLSTR(SVAL(*v, 0)))		/* if no proc name specified, */
	    {					/* give help on prompt style */
	    helpblk.nextcmd[0] = EOS;
	    helpblk.helpobj[0] = EOS;
	    helpblk.compiled = FALSE;
	    s_copy("PRMTMODE", name);
	    s_copy("prompt style dynamic parameters", helpblk.helpobj);
	    code = f_open(&fctx, HELPLUN, HELPLIB, name, HLP_TYPE, F_READ);
	    if (code != SUCCESS)
		goto open_err;
							/* process help file */
	    code = proc_help(&fctx, HELPLIB, "", "", "", "", "", &helpblk);
	    f_close(&fctx, F_KEEP);
	    if (code != SUCCESS)
		goto help_err;
	    s_copy(helpblk.nextcmd, next_cmd);	/* prev mode command from help*/
	    return(DO_CHECK);
	    }
	else
	    code = help_do(procctx, cmdctx);	/* give help on the proc */
	}
    return(code);

open_err:
    tmmsg(PROCFAIL, "No help available on '%s'.", "TAE-NOMODHELP",
	  (uintptr_t) helpblk.helpobj, 0, 0, 0, 0);
    return(DO_CHECK);

help_err:
    tmmsg(PROCFAIL, helpblk.errmsg, helpblk.errkey, 0, 0, 0, 0, 0);
    return(DO_CHECK);
    }

/*
 *	ps_initial.  Perform initial prompt & get initial input.
 */

FUNCTION static CODE ps_initial
(
    struct CONTXT	*ctx		/* in/out: context for dynamic parms */

 )
    {
    IMPORT  struct  VARIABLE	*sfi_gbl;	/* pointer to $sfi	*/
    IMPORT TEXT		next_cmd[];
    IMPORT struct CONTXT primctx;		/* primary level context */
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code;
    struct VARIABLE	*v;
    struct SYNBLK	sb;
    struct SYMTAB	saved_st;	/* copy of initial parm sym tab	*/

    initab(&saved_st);
    code = movest(&(*ctx).parmst, &saved_st);	/* save initial parm sym tab */
    if (code != SUCCESS)
	goto over_err;
    v = lookex(&(*ctx).locst, "_PROMPT");	/* point to user prompt string*/
    if ((*ctx).parmst.link == NULL)		/* if no parms to prompt for */
	tmmsg(SUCCESS,
	"Proc requesting parameters from null parameter list.  Continuing.",
	      "TAE-NULLLIST", 0, 0, 0, 0,0);
    else
	{
	while (FOREVER)				/* initial prompt loop	*/
	    {
	    if (!NULLSTR(next_cmd))
		{					/* yes	*/
		s_copy(next_cmd, cmdstr);
		next_cmd[0] = EOS;
		code = SUCCESS;
		}
	    else
		{
		TEXT str[STRINGSIZ];		/* temp buf for prompt string */

		if (v == NULL  ||  (*v).v_count <= 0) /* if proc didn't supply*/
		    {			  /* _PROMPT, automatic initial prompt*/
		    auto_list(&(*ctx).parmst);
		    s_copy("->", str);
		    }
		else
		    {
		    put_initial(v, str);	/* display user initial prompt*/
		    }
						/* lines and get final line */
				/* get interactive input to initial prompt*/
		code = get_psip(str, cmdstr);
		}
	    if (code == PS_ESCAPE)
		{
		code = esc_ps(ctx);	/* execute in escape mode cmd space */
		if (code == DO_RUN  ||  code == DO_EXIT)
		    break;
		else
		    continue;
		}
					 /* substitute from primary context */
	    code = substitute(cmdstr, CMDLINSIZ, (GENPTR) &primctx, TRUE);
	    if (code != SUCCESS)
		continue;
					/* restart with initial sym tab */
	    code = movest(&saved_st, &(*ctx).parmst);
	    if (code != SUCCESS)
		goto over_err;
	    initok(&sb, cmdstr);	/* init block for syntax package */
					/* update parms from interactive line */
	    code = updtab(&(*ctx).parmst, &sb);
	    if (code == SUCCESS  ||  code == SOME_REJECTED)
		{
		IVAL(*sfi_gbl, 0) = 1;		/* recover if missing parms */
		break;
		}
	    }
	}
ret:
    deltab(&saved_st);
    return(code);

over_err:
    overr();
    code = DO_EXIT;
    goto ret;
    }

/*
 *  ps_intrin.	Find command in prompt style intrinsic table.
 *  The returned pointer
 *  is the pointer to the first occurence of the command. (A command may
 *  occur several times successively, each occurence with a different
 *  subcommand.)  We assume here that the intrinsic table has no
 *  ambiguities w.r.t. abbreviations of commands.
 */

FUNCTION struct ITRCMD * ps_intrin
(
    FAST TEXT		*cmd		/* in:  command	*/

 )
    {
    FAST struct ITRCMD 	*p;
    COUNT		len;

    len = s_length(cmd);			/* length of command	*/
    for (p = &ps_cmd[0]; !NULLSTR((*p).cmd);  p++)
	{
	if ((*p).abbchr == 0)			/* if all chars required */
	    {
	    if (s_equal(cmd, (*p).cmd)) return (p);
	    }
	else if (len >= (*p).abbchr)		/* abbreviations 	*/
	    {
	    if (s_lseq(cmd, (*p).cmd)) return (p);
	    }
	}
    return(NULL);
    }

/*
 *	ps_itrsub.  Locates the ITRCMD structure given a subcommand and the
 *	pointer to the first ITRCMD structure for a prompt style command.
 *
 *	Returns pointer to the correct ITRCMD structure or NULL if the
 *	subcommand is non-existent or ambiguous.
 *
 *	If the subcommand is null, then it must match an ITRCMD entry
 *	which has a null subcommand.  This is how "default" subcommand
 *	is defined.  If you do not want a default, do not put a null
 *	subcommand in the table.
 * 
 */

 FUNCTION struct ITRCMD * ps_itrsub
 (
  TEXT		subcmd[],		/* in: subcommand	*/
  struct ITRCMD    	*p			/* in: first ITRCMD	*/
  )
    {
    COUNT		matches;		/* number of matches	*/
    struct ITRCMD *	p0;			
    struct ITRCMD *	psave = 0;

    matches = 0;
    p0 = p;
    do
	{
	if (NULLSTR(subcmd))			/* special check for null... */
	    {
 	    if (NULLSTR((*p).subcmd))		/* (because null is always */
	        return(p);			/* true for s_lseq)	*/
	    }
	else if (s_lseq(subcmd, (*p).subcmd))
	    {
	    psave = p;
	    matches++;
	    }
	p++;
	}
    while (s_equal((*p0).cmd, (*p).cmd));	/* stay with initial command */

    if (matches == 1) return(psave);
    else 	      return(NULL);
    } 

/*
 *	ps_mandatory.  Perform mandatory prompting and input.
 */

FUNCTION static CODE ps_mandatory
(
    struct CONTXT	*ctx		/* in/out: dynamic parms context */

 )
    {
    IMPORT TEXT		next_cmd[];
    IMPORT struct CONTXT primctx;		/* primary level context */
    IMPORT struct VARIABLE *sfi_gbl;		/* $SFI			 */
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code;
    TEXT		new_prompt[6+NAMESIZ+1];
    struct VARIABLE	*v;

    code = SUCCESS;
    for (v = (*ctx).parmst.link; v != NULL; )
	{
	if ((*v).v_count >= 0)
	    {
	    v = (*v).v_link;
	    continue;				/* skip if not mandatory */
	    }
	cur_mandatory = v;
	s_copy("ENTER ", new_prompt);
	s_append((*v).v_name, new_prompt);
	s_append(">", new_prompt);
    	if (!NULLSTR(next_cmd))			/* saved cmd from HELP?	*/
    	    {					/* yes	*/
    	    s_copy(next_cmd, cmdstr);
    	    next_cmd[0] = EOS;
	    code = SUCCESS;
    	    }
    	else
	    code = get_ps(new_prompt, cmdstr);	/* get interactive input */
	if (code == PS_ESCAPE)
	    {
	    code = esc_ps(ctx);
	    if (code == DO_RUN  ||  code == DO_EXIT)
		break;
	    else
		continue;
	    }
	if (NULLSTR(cmdstr))
	    continue;
					/* substitute from primary context */
	code = substitute(cmdstr, CMDLINSIZ, (GENPTR) &primctx, TRUE);
	if (code != SUCCESS)
	    continue;
	code = ps_updval(v, cmdstr);		/* set the parm value	*/
	if (code != SUCCESS)
	    continue;
	v = (*v).v_link;			/* point to next parm	*/
	}
    return(code);
    }

/*	ps_nothing_do.  Supports new meaning of EXIT command.
 *	Simply returns SUCCESS.
 */

FUNCTION CODE ps_nothing_do
(
    struct CONTXT	*procctx,	/* in:  context of dyn parm request */
    struct CONTXT	*cmdctx	/* in:  context of the HELP cmd line */

 )
   {
   return(SUCCESS) ;
   }

/*	ps_prccmd.  Imitates prccmd for prompt style.
 *	Used only in escape command space.
 */

FUNCTION static CODE ps_prccmd
(
    struct CONTXT	*procctx,	/* in/out: proc context		*/
    TEXT		cmdstr[]	/* in/out: the command string	*/

 )
    {
    IMPORT struct CONTXT primctx;   /* primary level context for substitution */
    IMPORT struct VARIABLE *sfi_gbl;	/* $SFI	*/
    CODE		code;
    struct SYNBLK	sb;		/* syntax block for syntax package */
    TEXT		cmd[FSPECSIZ+1];
    CODE		cmdcod;
    struct CONTXT	cmdctx;		/* context for this ps escape mode cmd*/
    struct ITRCMD	*itrcmd;	/* ps intrinsic command	*/
    
    cmdctx.prclevel = (*procctx).prclevel + 1;
    code = inictx(&cmdctx);		/* init context of the command line */
    if (code != SUCCESS)
	return(DO_RETURN);
    cmdctx.sb = &sb;
					/* substitute from prim ctx */
    code = substitute(cmdstr, CMDLINSIZ, (GENPTR) &primctx, TRUE);
    if (code != SUCCESS)
	goto subst_err;
					/* parse command */
    code = cmd_parse(&sb, cmdstr, cmdctx.label, cmd, cmdctx.subcmd);
    if (code != SUCCESS)
	goto syntax_err;
    if (NULLSTR(cmd))
	cmdcod = DO_SUCCESS;		/* ignore null lines		*/
    else if ((itrcmd = ps_intrin(cmd)) != NULL)	/* if ps intrinsic command */
	{
	cmdctx.intrinsic = TRUE;
	itrcmd = ps_itrsub(cmdctx.subcmd, itrcmd);
	if (itrcmd == NULL)
	    goto subcmd_err;
	s_copy((*itrcmd).subcmd, cmdctx.subcmd);  /* get full subcmd name */
	if (!s_equal("TUTOR", (*itrcmd).cmd))
	    clr_latch(procctx);			/* clear latching if not tutor*/
	if (memtab((*itrcmd).partab,(*itrcmd).numprm,&(cmdctx.parmst))!=SUCCESS)
	    goto close_return;
	if (updtab(&(cmdctx.parmst), &sb) != SUCCESS)
	    goto close_return;
	ini_status();				/* init $SFI, $SKEY	*/
	cmdcod = (*(*itrcmd).cproc)(procctx, &cmdctx);	/* call "do" function*/
	}
    else
	{
	tmmsg(PROCFAIL, "Unrecognized prompt style command.",
	      "TAE-UNRPSCMD", 0, 0, 0, 0, 0);
	cmdcod = DO_RETURN;
	}
    clsctx(&cmdctx);
    if (cmdcod == DO_CHECK  &&  IVAL(*sfi_gbl,0) >= 0)
	cmdcod = DO_SUCCESS;
    return(cmdcod);

subst_err:
    clr_latch(procctx);
    goto close_return;

syntax_err:
    tmmsg(PROCFAIL, sb.errmsg, "TAE-FMTERR", 0, 0, 0, 0, 0);		/* report error	*/
    goto close_return;

subcmd_err:
    tmmsg(PROCFAIL, "Undefined subcommand '%s'.", 
	  "TAE-UNDEFSUB", (uintptr_t) cmdctx.subcmd, 0, 0, 0, 0);
    goto close_return;

close_return:
    clsctx(&cmdctx);
    return(DO_RETURN);
    }

/*	ps_subinitial.  Perform initial subcommand prompt & get input.
 *	Also returns the remainder of the cmd line in case the user has
 *	anticipated and entered the parm values.
 *	TBD:  Use process spec'd prompt string(s)  (ps_subinitial).
 */

FUNCTION static CODE ps_subinitial
(
    struct CONTXT	*ctx,		/* in/out: context for dynamic parms*/
    TEXT		sub_str[]	/* out:  user's cmd after subc	*/

 )
    {
    IMPORT TEXT		next_cmd[];
    IMPORT struct CONTXT primctx;		/* primary level context */
    IMPORT CODE		cmd_mode;		/* for prccmd loop	 */
    IMPORT struct CONTXT *curproc;		/* pointer to current proc */
    TEXT		cmdstr[CMDLINSIZ+1];
    CODE		code;
    struct VARIABLE	*v;
    struct SYNBLK	sb;
    struct SUBCMD	*s;
    TEXT		token[TOKESIZ+1];
    TEXT		subcmd[SUBCMDSIZ+1];	/* user's typed subcommand */
    CODE		saved_mode;
    CODE		saved_level;
    struct CONTXT	*saved_cur;    
    TEXT		pass_cmd[NEXTSIZ+1];	/* for _$PASSCMD */
    CODE		fake_flag;		/* in a _$PASSCMD fake cmd */

    saved_mode = cmd_mode;			/* save old command mode */
    cmd_mode = DYNMODE;
    if ((*ctx).prclevel > 0)		/* Save proc position since prccmd */
	clssav(ctx);			/* doesn't do it at level 0 */
    saved_level = (*ctx).prclevel;		/* save the proc level	*/
    (*ctx).prclevel = 0;			/* make it zero for now	*/
    saved_cur = curproc;			/* save the 'current' proc */
    curproc = ctx;				/* for err msgs		*/
    cur_mandatory = NULL;			/* not yet in mandatory parms */
    sub_str[0] = EOS;
    pass_cmd[0] = EOS;				/* no passthru commands yet */
    v = lookex(&(*ctx).locst, "_PROMPT");	/* point to user's prompt str */
    while (FOREVER)				/* initial prompt loop	*/
	{
	fake_flag = FALSE;
    	if (!NULLSTR(next_cmd))			/* saved cmd from HELP?	*/
    	    {					/* yes	*/
    	    s_copy(next_cmd, cmdstr);
    	    next_cmd[0] = EOS;
	    code = SUCCESS;
    	    }
	else if (!NULLSTR(pass_cmd))		/* saved cmd from passthru? */
	    {
	    s_copy(pass_cmd, cmdstr);		/* yes, use faked cmd line */
	    pass_cmd[0] = EOS;
	    fake_flag = TRUE;			/* avoid loops (unlikely) */
	    code = SUCCESS;
	    }
	else
	    {
	    TEXT str[STRINGSIZ];		/* temp buf for prompt string */

	    if (v == NULL  ||  (*v).v_count <= 0)
		auto_sublist((*ctx).subptr);	/* auto initial subc prompt */
	    else
		put_initial(v, str);
				/* get interactive input to initial prompt */
	    code = get_psip(str, cmdstr);
	    }
	if (code == PS_ESCAPE)
	    {
	    code = esc_ps(ctx);		/* execute in escape mode cmd space */
	    if (code == DO_RUN  ||  code == DO_EXIT)
		break;
	    else
		continue;
	    }
	if (NULLSTR(cmdstr))
	    continue;
					/* substitute from primary context */
	code = substitute(cmdstr, CMDLINSIZ, (GENPTR) &primctx, TRUE);
	if (code != SUCCESS)
	    continue;
	initok(&sb, cmdstr);		/* init block for syntax package */
	code = get_nowhite (&sb, token);
	if (code == S_SYNERR)
	    {
	      tmmsg(PROCFAIL, sb.errmsg, "TAE-SYNERR", 0, 0, 0, 0, 0);
	    continue;
	    }
	s_bcopy(token, subcmd, SUBCMDSIZ);	/* copy to caller	*/
	strpqu(subcmd);
	code = subab((*ctx).subptr, subcmd, &s); /* look up the subc by abbrev*/
	if (code == SUCCESS)
	    {
	    s_copy((*s).name, (*ctx).subcmd);	/* save the subcommand name */
	    (*ctx).subsiz = s_length((*ctx).subcmd);
	    s_copy(sb.curchr, sub_str);	  /* return rest of string to caller */
	    (*ctx).inbody   = FALSE;
	    (*ctx).proctype = Y_UNKNOWN;
	    break;
	    }
	else
	    {
	    if (code == AMBIG)
		tmmsg(PROCFAIL, "Ambiguous subcommand abbreviation.",
		      "TAE-AMBIGSUB", 0, 0, 0, 0, 0);
	    else
		{
		/* If we allow intrinsics and procedures here, process it. */
	        if (procs_allowed || is_passthru(subcmd))
	            {		
	        	prccmd(cmdstr, ctx);

		/* Check if the user wants to be notified of passthru cmds */
		/* by checking for special _$PASSCMD subcommand		   */

			if (!fake_flag &&
				subex((*ctx).subptr, "_$PASSCMD") != NULL)
			    sprintf(pass_cmd,"_$PASSCMD command=\"%s\"",subcmd);

	            }
	        else
		    tmmsg(PROCFAIL, "Unrecognized subcommand '%s'.",
			  "TAE-UNDEFSUB", (uintptr_t) subcmd, 0, 0, 0, 0);
		}
	    continue;
	    }
	}
    cmd_mode = saved_mode;			/* restore old mode */
    (*ctx).prclevel = saved_level;		/* restore the proc level */
    if ((*ctx).prclevel > 0)
	opnsav(ctx);
    curproc = saved_cur;			/* restore the current proc */
    return(code);
    }

/*	ps_tutor_do.  Perform prompt style mode TUTOR command.
 */

FUNCTION CODE ps_tutor_do 
(
    struct CONTXT	*procctx,
    struct CONTXT	*cmdctx

 )
    {
    IMPORT struct SFILE	prcfil;		/* global PDF (used by tutor_do) */
    struct SFILE	loc_pf;		/* local copy of prcfil		*/
    CODE		code;

    if (still_sub)
	{
	tutor_sub = TRUE;		/* flag we entered tutor on subcs */
	code = opn_tutor(saved_pdf, procctx, NOFORCE, NULL, TRUE);
	if (code != SUCCESS)
	    return(DO_EXIT);
	code = sub_tutor(saved_pdf, procctx);
	if (code == DO_EXIT)		/* This is so DO_EXIT will	 */
	    {				/* not abort an inter. process.	 */
	    tutor_sub = FALSE;
	    code = SUCCESS;
	    }
	else if (code == SUCCESS)	/* If code is SUCCESS, DO_RUN	 */
	    code = DO_RUN;		/* forces tutor on chosen subcmd */
	cls_tutor(saved_pdf, procctx);
	}
    else
	{
	tutor_sub = FALSE;
	MOVE_STRUCT(prcfil, loc_pf);		/* save global PDF	*/
	MOVE_STRUCT(*saved_pdf, prcfil);	/* for tutor_do		*/
	code = opn_tutor(&prcfil, procctx, NOFORCE, NULL, TRUE);
	if (code != SUCCESS)
	    return(DO_EXIT);
	code = tutor(&prcfil, procctx);
	if (code == DO_EXIT)
	    code = SUCCESS;
	cls_tutor(&prcfil, procctx);
	MOVE_STRUCT(loc_pf, prcfil);		/* restore global PDF	*/
	}
    return(code);
    }

/*	psdyn.  Perform prompt style dynamic parameters.
 *	This is the main routine for prompt style.
 *
 *	Note:  If we prompt for subcs and there are trailing characters,
 *	these (sub_str) are first used as if they were the parm
 *	specification part of a normal TCL command line.  Note that
 *	sub_str has already had substitution performed in pssub.
 */

FUNCTION CODE psdyn
(
    struct PARBLK	*parblk,	/* in:  parameter block	*/
    struct SFILE	*pdf,
    struct CONTXT	*ctx,	/* in/out: context containing requested params*/
    FUNINT		dash_present	/* in:  TRUE if '-' in proc's cmd */

 )
    {
    IMPORT struct VARIABLE *sfi_gbl;		/* $SFI	*/
    struct VARIABLE	*pst_var, *subc_var;
    CODE		code, tmpcode;
    CODE		mode;
    struct SYNBLK	sb;
    TEXT		saved_sub[SUBCMDSIZ+1];	/* subcmd from process XQDYNP */
    TEXT		held_msg[STRINGSIZ+1], held_key[KEYSIZ+1];
    TEXT		sub_str[CMDLINSIZ+1];

    s_copy((*ctx).subcmd, saved_sub);
    sub_str[0] = EOS;
    code = plcini(ctx);				/* make std locals	*/
    if (code != SUCCESS)
	goto norm_err;
    while (FOREVER)
	{
	code = move_locals(parblk, &(*ctx).locst);
	if (code != SUCCESS)
	    goto over_err;
	sub_str[0] = EOS;
	code = pssub(parblk, pdf, ctx, dash_present, sub_str);
	if (code == DO_EXIT)
	    return(code);
	code = parcmp(&(*parblk).symtab, &(*ctx).parmst, held_msg);
	if (code == SUCCESS)
	    held_msg[0] = EOS;
	s_copy("TAE-NOTUSED", held_key);
	mode = ((*parblk).msgtyp == M_FULLPDF) ? FULLSET : SUBSET;
	code = parmrg(&(*parblk).symtab, &(*ctx).parmst, mode);
	if (code != SUCCESS)
	    goto norm_err;
	if (!NULLSTR(sub_str))		/* if trailing chars after subc	*/
	    {
	    initok(&sb, sub_str);	/* init syntax block		*/
	    code = updtab(&(*ctx).parmst, &sb);	/* update from trailing string*/
	    IVAL(*sfi_gbl, 0) = 1;
	    if (code == SUCCESS  ||  code == SOME_REJECTED)
		break;
	    else
		{
		reini_ctx(ctx);
		f_rewind(pdf);
		(*ctx).subfnd   = FALSE;
		(*ctx).inbody   = FALSE;
		(*ctx).proctype = Y_UNKNOWN;
		(*ctx).pdf_line = 0;
		s_copy(saved_sub, (*ctx).subcmd);	/* restore orig subc */
		(*ctx).subsiz   = s_length(saved_sub);	/* & restore length  */
		}
	    }
	else				/* no trailing chars		*/
	    break;
	}
    code = psparm(pdf, ctx, held_msg, held_key); /*get parms with prompt style*/
    if (!NULLSTR((*ctx).subcmd))	/* if there was a subcommand...	*/
	{
	pst_var = allvar(&(*ctx).parmst);
	if (pst_var == NULL)
	    goto over_err;
	subc_var = lookex(&(*ctx).locst, "_SUBCMD");
	tmpcode = vcopy(subc_var, pst_var);	/* get it into parm st	*/
	if (tmpcode != SUCCESS)
	    goto over_err;
	}
    if (code == SUCCESS)	/* exit from tutor; rewind the pdf to */
	{			/* start over again		      */
	reini_ctx(ctx);
	f_rewind(pdf);
	(*ctx).subfnd   = FALSE;
	(*ctx).inbody   = FALSE;
	(*ctx).proctype = Y_UNKNOWN;
	(*ctx).pdf_line = 0;
	s_copy(saved_sub, (*ctx).subcmd);	/* restore orig subc	*/
	(*ctx).subsiz   = s_length(saved_sub);	/* & restore length	*/
	}
    return(code);

over_err:
    overr();
norm_err:
    return(DO_EXIT);
    }


/* psparm - Get parameters through prompt style.
 *
 * Returns DO_EXIT or DO_RUN 
 */
FUNCTION CODE psparm
(
    struct SFILE	*pdf,		/* in/out: PDF for the proc	*/
    struct CONTXT	*ctx,		/* in/out: parameter context	*/
    TEXT		held_msg[],	/* in: possible previously gen'd EM */
    TEXT		held_key[]	/* in: possible prev gen'd error key */

 )
    {
    IMPORT CODE		cmd_mode;		/* for prccmd loop	*/
    IMPORT  struct  VARIABLE	*sfi_gbl;	/* pointer to $sfi	*/
    IMPORT struct CONTXT *curproc;		/* pointer to current proc */
    CODE		saved_mode;
    CODE		code;
    CODE		saved_level;
    struct CONTXT	*saved_cur;    
    TEXT		buf[STRINGSIZ+1];
    CODE		termin;

    saved_mode = cmd_mode;			/* save old command mode */
    cmd_mode = DYNMODE;
    if ((*ctx).prclevel > 0)		/* Save proc position since prccmd */
	clssav(ctx);			/* doesn't do it at level 0 */
    saved_level = (*ctx).prclevel;		/* save the proc level	*/
    (*ctx).prclevel = 0;			/* make it zero for now	*/
    saved_cur = curproc;			/* save the 'current' proc */
    curproc = ctx;				/* for err msgs		*/
    cur_mandatory = NULL;			/* not yet in mandatory parms */
    saved_pdf = pdf;
    still_sub = FALSE;
    if (!NULLSTR(held_msg))
      m_put(held_msg, held_key, 0, 0, 0, 0, 0);
    if (tutor_sub)			/* if we started tutoring on subcs */
	{
	if (!NULLSTR(held_msg))
	    {
	    t_write("Hit RETURN to continue.", T_STDCC);
	    t_read(buf, &termin);
	    }
	code = ps_prccmd(ctx, "TUTOR");
	}
    else if (prompted_subc)		/* if we prompted for subcommands */
	{				/* mandatory parm prompts only	*/
	code = ps_mandatory(ctx);
	if (code != DO_EXIT)
	    code = DO_RUN;
	}
    else
	{
	code = ps_initial(ctx);			/* initial prompt & input */
	if (code != DO_EXIT  &&  code != DO_RUN)
	    code = ps_mandatory(ctx);		/* mandatory prompts	*/
	if (code != DO_EXIT)
	    code = DO_RUN;
	}
    prompted_subc = FALSE;
    if (code == DO_EXIT)
    	IVAL(*sfi_gbl,0) = -1;		/* force error on 'exit'	*/
    cmd_mode = saved_mode;			/* restore old mode	*/
    (*ctx).prclevel = saved_level;		/* restore the proc level */
    if ((*ctx).prclevel > 0)
	opnsav(ctx);
    curproc = saved_cur;			/* restore the current proc */
    return(code);
    }

/*	pssub.  Request subcommand in Prompt Style.
 */

FUNCTION CODE pssub
(
    struct PARBLK	*parblk,	/* in:  parameter block from process */
    struct SFILE	*pdf,		/* in/out: PDF			*/
    struct CONTXT	*ctx,		/* in/out: context to get subc for */
    FUNINT		dash_present, /* in: TRUE if "-" in process's cmd line*/
    TEXT		sub_str[]	/* out: user's cmd line following subc*/

 )
    {
    IMPORT  struct  VARIABLE	*sfi_gbl;	/* pointer to $sfi */
    TEXT		proc_sub[SUBCMDSIZ+1];
    CODE		ps_code, code;

    prompted_subc = FALSE;
    ps_code 	  = SUCCESS;
    saved_pdf	  = pdf;
    tutor_sub	  = FALSE;
    s_copy((*ctx).subcmd, proc_sub);		/* save subcmd as received */
    if (dash_present  &&  NULLSTR(proc_sub))
	(*ctx).special = SUB_SEARCH;	/* pdftab will just build subcmd chain*/
    else if (!NULLSTR(proc_sub))
	(*ctx).special = NOT_SPECIAL;		/* subcmd already specd	*/
    else
	(*ctx).special = TUTOR_ATTEMPT;		/* we'll try to get parms*/
    code = pdftab(ctx, pdf);
    if (code != SUCCESS)
	goto norm_err;
    if ((NULLSTR(proc_sub) && dash_present)  ||
	    (*ctx).special == SUB_SEARCH)
	{
	prompted_subc = TRUE;
	still_sub     = TRUE;
	ps_code = ps_subinitial(ctx, sub_str);	/* get the subcommand	*/
	if (ps_code != DO_EXIT)
	    {
	    reini_ctx(ctx);			/* reinit sym tabs & refs */
	    (*ctx).special  = NOT_SPECIAL;
	    (*ctx).pdf_line = 0;
	    f_rewind(pdf);
	    code = plcini(ctx);			/* make implicit locals	*/
	    if (code != SUCCESS)
		goto norm_err;
						/* move proc's locals back in */
	    code = move_locals(parblk, &(*ctx).locst);
	    if (code != SUCCESS)
		goto over_err;
	    code = pdftab(ctx, pdf);		/* now build parm symbol tab */
	    if (code != SUCCESS)
		goto norm_err;
	    }
	}

ret:
    if (ps_code == DO_EXIT)
    	IVAL(*sfi_gbl,0) = -1;		/* force error on 'exit'	*/
    return(ps_code);

over_err:
    overr();
norm_err:
    ps_code = DO_EXIT;
    goto ret;
    }

/*	put_initial.  Display user supplied initial prompt lines.  
 *		      The last line is not displayed, but passed back
 *		      for use by get_... prompting routines
 */

FUNCTION static VOID put_initial
(
    struct VARIABLE	*v,		/* in:  _PROMPT variable	*/
    TEXT		*last_prompt	/* out: last line of prompt	*/

 )
    {
    COUNT		i;
    TEXT		buf[STRINGSIZ+1];

    for (i = 0; i < (*v).v_count-1; i++)
	{
	s_copy(SVAL(*v, i), buf);
	t_highlight(buf);
	t_write(buf, T_STDCC);
	}
    s_copy(SVAL(*v, i), last_prompt);	/* last line */
    s_append(">", last_prompt);
    return;
    }

/*	ps_updval.  Update a parameter value from a ps mandatory parm prompt
 *	response.
 */

FUNCTION static CODE ps_updval
(
    struct VARIABLE	*v,		/* in/out: parm to be updated	*/
    TEXT		cmdstr[]	/* in:  the value string	*/

 )
    {
    struct SYNBLK	sb;		/* syntax block			*/
    CODE		code;
    TEXT		*value[MAXVAL];	/* value pointers		*/
    COUNT		count;		/* number of values for single parm */
    char		*pos;
    TEXT		token[TOKESIZ+1];
    COUNT		expcount;

    initok(&sb, cmdstr);		/* init block for syntax package */
    pos = sb.curchr;		       /* save current position for look-ahead*/
    if ((code = gettok(&sb, token)) == S_WHITE)	/* get a token		*/
	code = gettok(&sb, token);	/* if white, get another	*/
    sb.curchr = pos;			/* restore position		*/
    if (code != '(')			/* if value not already in ()	*/
	parenval(&sb);		    /* slap parens around the value components*/
    expcount = ((*v).v_type == V_NAME)  ?  1 : (*v).v_maxc;
    code = getval(&sb, value, expcount, &count);	/* get the value */
    if (code == S_SYNERR) goto syn_err;
    if (code == EOS) goto noval_err;
    if (count >= 0)
	if (repval(v, value, count, code==S_DEREF) != SUCCESS)
	    return(FAIL);
    return(SUCCESS);

syn_err:
    tmmsg(PROCFAIL, sb.errmsg, "TAE-INVPVAL", 0, 0, 0, 0, 0);
    return(FAIL);

noval_err:
    t_write("This parameter is mandatory.", T_STDCC);
    return(FAIL);
    }

/*	move_locals.   Move locals from parblk into symbol table.
 *
 * NOTE: This functions is in dynamic.c, but it is commented out.
 * If the version in dynamic.c ever gets re-enabled, this copy of
 * the routine should be removed.
 */

FUNCTION CODE move_locals
(
    struct PARBLK	*p,		/* in: parblk		*/
    struct SYMTAB	*locst		/* out: symbol table	*/

 )
    {
    struct VARIABLE	*vin, *vout;
    CODE		code;

    for (vin=(*p).symtab.link; vin != NULL; vin=(*vin).v_link)
        {
	if ((*vin).v_class != V_LOCAL)	/* only locals get moved */
	    continue;
	vout = allvar (locst);		/* allocate variable	 */	
        if (vout == NULL)
	    return (FAIL);
	code = vcopy (vin, vout);	/* copy variable	 */
	if (code != SUCCESS)
	    return (FAIL);
	}
    return (SUCCESS);
    }

