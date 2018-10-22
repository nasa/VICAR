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



/* 	help_do
 *
 * This file contains the functions to support command mode help processing.
 *
 * A table of associated intrinsic command structure entries is at the
 * end of the file and associated resident PDFs is near the beginning.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	18-jul-83	Don't allow help cmd from a proc...peb
 *	16-aug-83	Move message help function to helpsubs, 
 *			implement helpblk for interface control...dm
 *	18-aug-83	Implement "?" for help on last message...dm
 *	21-sep-83	Fix RESIVARs for UNIX compatibility...palm
 *	11-oct-83	Fix unix compilation errors...palm
 *	11-oct-83	Fix RESIDVAR for help-parm...palm
 *	31-oct-83	Fix Unix bug for msg_do crash...dm
 *	20-jan-84	Implement "HELP-GLOBAL"...lim 
 *	03-feb-84	Check for help on interrupt mode...nhe
 *	28-feb-84	Fix HELP-GLOBAL errors...lim
 *	04-may-84	VALUE_x to xVAL ...ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Move IMPORT declarations...lim
 *			No more dyncmd mode...nhe
 *	04-jun-84	Implement default intrinsic subcommand...lim
 *	07-jun-84	Allow help from proc (pr #626)...dm
 *	24-aug-84	Allow help to go to stdout (807)...nhe
 *	20-oct-84	TCL 117:  Init helpblk for .compiled...peb
 *	25-oct-84	Add logic to handle subcommand help...lia
 *	07-nov-84	Add logic to handle saved proc and subcommand,
 *			especially for HELP-PARM...lia
 *	08-dec-84	PR 901: set up helpblk.helpobj correctly...lia
 *	16-dec-84	TCL 67: Audit NAMESIZ: replace by F_Q_NAMESIZ?...peb
 *
 ****************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	05-feb-85	Update for getting help on remote proc...nhe
 *	13-mar-85	Renamed get_remote_help() to get_remote_phelp...dm
 *	21-apr-85	Rename get_remote_phelp() to get_rem_inihelp()...dm
 *	22-apr-85	update for remote .include file access...dm
 *	25-apr-85	Fix PR on help-global bug...dm
 *
 *****************************************************************************
 *
 *	11-sep-86	PR 965: Deliver error msg on HELP-COMMAND,
 *			HELP-PROC with no parameter.  Done by creating
 *			a new default subcommand, "", for HELP and causing
 *			HELP-COMMAND, HELP-PROC to each require a parm...peb
 *	19-SEP-86	PR 965: change "" subcommand to GENERAL...peb
 *	08-may-87	PR 1181: change "INTMODE" and "COMMODE" to lower...ljn
 *	22-jul-87	Add get_helpcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	03-may-89	new hardcopy help...palm 
 *	23-may-90	Pull out RCJM...ljn
 *	28-jun-90	Removed get_helpcmd()...ljn
 *	01-aug-91	Braces for static string initializors...ljn
 */

#include	"stdh.inp"		/* standard C definitions		*/
#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"dirinc.inc"		/* directive package			*/
#include	"syninc.inc"		/* syntax package			*/
#include	"helpinc.inc"		/* help output control block		*/
#include	"terminc.inc"		/* terminal package			*/
#include "taeintproto.h"

FUNCTION CODE helpmsg_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context	   */
    struct CONTXT	*npctx		/* in:  proc ctx for HELP command  */

 );
FUNCTION  static  CODE  msg_do
(
    TEXT		msgkey[]		/* message key to help on  */
    
 );
/*	Memory-resident PDF data for commands performed in this file.
 *
 */

/* HELP with no subcommand */

    TEXT  *null_str[] = {""};
    static struct RESIDVAR pthelp0[] =
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"PROC",    V_STRING, 0, 1, 1,      FSPECSIZ, 1, NULL,   (GENPTR)null_str},
	  {"NODE",    V_STRING, 0, 0, 2,   NODENAMESIZ, 0, NULL,   NULL}
	};

/* HELP-PROC */

    static struct RESIDVAR pthelp1[] =
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"PROC",    V_STRING, 0, 1, 1,      FSPECSIZ, 0, NULL,   NULL},
	  {"NODE",    V_STRING, 0, 0, 2,   NODENAMESIZ, 0, NULL,   NULL}
	};


/* HELP-COMMAND */
    static struct RESIDVAR pthelp2[] =
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"COMMAND", V_STRING, 0, 1, 1,      FSPECSIZ, 0, NULL,   NULL},
	  {"NODE",    V_STRING, 0, 0, 2,   NODENAMESIZ, 0, NULL,   NULL}
        };


/* HELP-MESSAGE */
    static struct RESIDVAR pthelp3[] =
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"KEY",     V_STRING, 0, 1, 1,      KEYSIZ,   1, NULL,   (GENPTR)null_str}
	};


/* HELP-PARM */
    static struct RESIDVAR pthelp4[] =		/* normal command mode */
	{
/* name    type      k  m maxc      size     dc val      dvp*/

	  {"PARM",	   V_STRING, 0, 1, 1,	F_Q_NAMESIZ,  -1, NULL,   NULL},
	  {"PROC",    V_STRING, 0, 1, 1,      FSPECSIZ,  1, NULL,   (GENPTR)null_str},
	  {"NODE",    V_STRING, 0, 0, 2,   NODENAMESIZ, 0, NULL,   NULL}
	};


/* HELP-GLOBAL */
    static struct RESIDVAR pthelp5[] =
	{
/* name     type      k  m maxc      size     dc val      dvp*/

	  {"VARIABLE", V_STRING, 0, 1, 1,      NAMESIZ,  -1, NULL,   NULL}
	};

/* HELP-HARDCOPY */
    static struct RESIDVAR pthelp6[] =
       {
/* name     type      k  m maxc      size     dc val      dvp*/

	 {"PROC",    V_STRING, 0, 1, 1,      FSPECSIZ, -1, NULL,    NULL}, 
	 {"OUTPUT",  V_STRING, 0, 1, 1,      FSPECSIZ,  1, NULL,   (GENPTR)null_str}
       };

/*
 *	help_do intrinsic command table	
 *	Note that the help command is not allowed from a proc.
 */

#define Y_HELP Y_GENERAL

    GLOBAL struct ITRCMD helpcmd[] = 	/* help commands  */
	{
    	   /* subcommand */	
{1, "HELP",     "GENERAL",   Y_HELP|Y_DEFSUB,
					I_NPM(pthelp0), pthelp0, help_do },
{1, "HELP",     "PROC",      Y_HELP,	I_NPM(pthelp1), pthelp1, help_do },
{1, "HELP",     "COMMAND",   Y_HELP,	I_NPM(pthelp2), pthelp2, help_do },
{1, "HELP",     "MESSAGE",   Y_HELP,	I_NPM(pthelp3), pthelp3, help_do },
{1, "HELP",     "PARM",      Y_HELP,	I_NPM(pthelp4), pthelp4, help_do },
{1, "HELP",	"GLOBAL",    Y_HELP,	I_NPM(pthelp5), pthelp5, help_do },
{1, "HELP",	"HARDCOPY",  Y_HELP,	I_NPM(pthelp6), pthelp6, help_do },
{1, "?",	"",	     Y_HELP,    0,		NULL,    helpmsg_do},
{0, ""}					/* TERMINATOR ENTRY: REQUIRED AT END */
	};

/*
 * Functions used
 */



/*	help_do - perform HELP command if entered from command (non-interrupt)
 *	 mode.
 *
 *	Assumes that the command was entered from command mode.
 *
 */

FUNCTION CODE help_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context	   */
    struct CONTXT	*npctx		/* in:  proc ctx for HELP command  */

 )
    {
    IMPORT  TEXT	lastkey[];		/* most recent message key 	*/
    IMPORT  TEXT	prevproc[];		/* most recent cmd or proc help */
    IMPORT  TEXT	prevsub[];		/* most recent subcommand help	*/
    IMPORT  TEXT	next_cmd[NEXTSIZ+1];
    IMPORT  CODE	cmd_mode;
    IMPORT  CODE	help_proc;		/* NOPROC or SAVEPROC		*/
    IMPORT  struct SYMTAB glbtab;
    IMPORT  BOOL	help_to_term;	/* TRUE if help output must be terminal	*/
    IMPORT  BOOL	full_scr_help;
    IMPORT  struct SFILE  *pstdo_fil;		/* pointer to std out file context	*/
    IMPORT  CODE	termtype;		/* T_CRT or not			*/

    struct  VARIABLE	*v;		/* variable in symbol table */
    struct  SFILE	fctx;			/* help file context	    */
    FAST    CODE	code;
    FAST    struct DEFPDF *defpdf;		/* global pdf block         */
    TEXT		name[STRINGSIZ+1];
    TEXT		procname[FSPECSIZ+1];
    TEXT		subname[SUBCMDSIZ+1];
    TEXT		parmname[STRINGSIZ+1];
    TEXT		libname[FLIBRSIZ+1],curlibname[FLIBRSIZ+1];
    TEXT		type[STRINGSIZ+1];
    TEXT		msgkey[KEYSIZ+1];
    TEXT		subcmd[SUBCMDSIZ+1];
    BOOL		intrin = FALSE;			/* true if intrinsic		*/
    struct  HELPBLK	helpblk;		/* help output control block	*/
    COUNT		length;


    help_to_term = FALSE;				/* doesn't have to be terminal	*/
    full_scr_help = ((termtype == T_CRT) && (pstdo_fil == NULL)); /* for full-screen      */
    if (s_equal((*npctx).subcmd, "MESSAGE"))		/* help-message */
	{
	help_proc = NOPROC;
	v = lookex(&(*npctx).parmst, "KEY");
	if (v != NULL)
	    s_copy(SVAL(*v, 0), msgkey);		/* get from ctx block */
	if (NULLSTR(msgkey))				/* if a null key  */   
	    s_copy(lastkey, msgkey);			/* get most recent key */
	code = msg_do(msgkey); 
	return(DO_CHECK);
	}			

/*	Comes here for other types of help		*/
    

    helpblk.compiled   = FALSE;
    helpblk.nextcmd[0] = EOS;				/* initialize	*/
    helpblk.helpobj[0] = EOS;
    parmname[0] = EOS;

    s_copy((*npctx).subcmd, subcmd);
    if (s_equal(subcmd, "PARM") || s_equal(subcmd, "GLOBAL"))
	{
	if (s_equal(subcmd, "PARM"))
	    {
	    v  = lookex(&(*npctx).parmst, "PARM");
	    if (v != NULL)
		s_copy(SVAL(*v, 0), parmname);	/* copy parm name */
    	    v  = lookex(&(*npctx).parmst, "PROC");		/* in case "proc" */
	    s_copy(SVAL(*v, 0), name);		/* copy proc name */
	    if (help_proc == SAVEPROC)		/* fill in missing proc & sub */
		{
		s_copy(name, procname);
		if (NULLSTR(name) || s_lseq("-", name))
		    s_copy(prevproc, procname);
		if (NULLSTR(name) && !NULLSTR(prevsub))
		    {
		    s_append("-", procname);
		    s_append(prevsub, procname);
		    }
		else if (s_lseq("-", name))
		    s_append(name, procname);
		s_copy(procname, name);
		}
	    if (NULLSTR(name))
		{
		  tmmsg(PROCFAIL, "Missing parameter:  PROC.", "TAE-MISPAR",0,0,0,0,0);
		return (DO_CHECK);
		}
	    if (s_lseq("-", name)) goto dh_parmerr;
	    if (!subchar(name))
		s_append("-",name);
	    s_copy("proc", type);
	    }
	else
	    {
	    help_proc = NOPROC;
	    v = lookex(&(*npctx).parmst, "VARIABLE");
	    if (v != NULL)
		s_copy(SVAL(*v, 0), parmname);
  	    v = lookex(&glbtab, parmname);
	    if (v != NULL  &&  (*v).v_class == V_GLOBAL)
		{
		defpdf = (*v).v_pdf;
		if (defpdf != NULL) 
		    f_spec(&(*defpdf).pdf, name);	/* fetch pdf file spec */
		else
		    {
		    tmmsg(PROCFAIL, "No help available on '%s'.", "TAE-NOHELP",
		          (uintptr_t) parmname,0,0,0,0);
		    return (DO_CHECK);
		    } 
		}
	    else
		{
		tmmsg(PROCFAIL, "Global variable '%s' does not exist.",
		      "TAE-NOGLB", (uintptr_t)parmname,0,0,0,0);
		return (DO_CHECK);
		}
	    s_copy("global", type);
	    }
    	code = opnhlp(&fctx, name, procname, subname, libname,     
		&intrin, &helpblk);			/* open help file	*/
	if (code != SUCCESS) goto dh_hlperr;
	if (NULLSTR(parmname))
	    s_copy(name, helpblk.helpobj);		/* give proc name */
	else
	    s_copy(parmname, helpblk.helpobj);		/* give parm name */
	code = proc_help(&fctx, libname, "", procname, subname, parmname, 
		type, &helpblk);			/* process help file */
	}
    else				/* mode help or help on cmd, proc*/
	{
	if ((v = lookex(&(*npctx).parmst, "PROC")) != NULL)	/* help  proc */
	    s_copy(SVAL(*v, 0), name);		/* get explicit name */
	else if ((v = lookex(&(*npctx).parmst, "COMMAND")) != NULL)	
	    s_copy(SVAL(*v, 0), name);		/* help  command */

	if (NULLSTR(name))				/* mode  help */
	    {
	    help_proc = NOPROC;
	    s_copy("mode", type);
    	    if (cmd_mode == INTMODE)
    		{
    		s_copy("intmode", name);
		s_copy("interrupt mode", helpblk.helpobj);
    		}
	    else
		{
		s_copy("commode", name);
		s_copy("command mode", helpblk.helpobj);
		}
	    code = f_open(&fctx, HELPLUN, HELPLIB, name, HLP_TYPE, F_READ);
	    if (code != SUCCESS) goto dh_operr;
	    code = proc_help(&fctx, HELPLIB, "", "", "", "",
		    "", &helpblk);		/* process help file	*/
	    }
	else
	    {
	    if (s_lseq("-", name))		/* missing command name */
		{
		if (help_proc != SAVEPROC) goto dh_parmerr;
		s_copy(prevproc, procname);
		s_append(name, procname);
		s_copy(procname, name);
		}
	    code = opnhlp(&fctx, name, procname, subname, libname, 
		    &intrin, &helpblk); 	/* open help file	*/
	    if (code != SUCCESS) goto dh_hlperr;
    	    s_copy (libname, curlibname);	/* libe for opened file */
	    length = (intrin) ? s_copy("command", type) : s_copy("proc", type);
	    if (NULLSTR(subname))
		s_copy(procname, helpblk.helpobj);
	    else
		s_copy(subname, helpblk.helpobj);
	    if (s_equal (subcmd, "HARDCOPY"))
	        {
	        v = lookex (&(*npctx).parmst, "OUTPUT");
	        s_copy (SVAL(*v,0), name);
	        code = hard_help (&fctx, libname, procname, name, &helpblk);
	        }
	    else
   	        code = proc_help(&fctx, curlibname, libname, procname, 
		    subname, "", type, &helpblk);	/* process help file */
	    }
	}
    f_close(&fctx, F_KEEP);
    if (code != SUCCESS) goto dh_hlperr;
    if (s_equal(type, "proc") || s_equal(type, "command"))
	{
	help_proc = SAVEPROC;
	s_copy(procname, prevproc);	/* save command or proc name 	*/
	s_copy(subname, prevsub);	/* save subcommand name 	*/
	}
    s_copy(helpblk.nextcmd, next_cmd);	/* prev mode command from help  */
    goto help_exit;
	
dh_operr:

    tmmsg(PROCFAIL, "No help available on '%s'.", "TAE-NOHELP", 
	  (uintptr_t)helpblk.helpobj,0,0,0,0);
    goto help_exit;

dh_hlperr:

    tmmsg(PROCFAIL, helpblk.errmsg, helpblk.errkey,0,0,0,0,0);
				/* display error message */
    goto help_exit;

dh_parmerr:

    tmmsg(PROCFAIL, "Missing proc name for initial help request.",
          "TAE-MISPROC",0,0,0,0,0);
    goto help_exit;

help_exit:
    return (DO_CHECK);
    }

/*
 * helpmsg_do.  Help on last displayed message.
 */

FUNCTION CODE helpmsg_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context	   */
    struct CONTXT	*npctx		/* in:  proc ctx for HELP command  */

 )
    {
    IMPORT  TEXT	lastkey[];	/* key of last displayed message   */
    IMPORT  BOOL	help_to_term;	/* TRUE if help output must be terminal	*/
    IMPORT  BOOL	full_scr_help;	/* TRUE if we want help with paging */
    IMPORT  struct SFILE  *pstdo_fil;		/* pointer to std out file context	*/
    IMPORT  CODE	termtype;		/* T_CRT or not			*/

    CODE 		code;

    help_to_term = FALSE;		/* not necessarily to terminal	   */
    full_scr_help = ((termtype == T_CRT) && (pstdo_fil == NULL));
    code = msg_do(lastkey);
    return(DO_CHECK);
    }


/* 
 * msg_do.  Get help on error message with given key	.
 */

FUNCTION  static  CODE  msg_do
(
    TEXT		msgkey[]		/* message key to help on  */
    
 )
    {
    IMPORT  TEXT	next_cmd[NEXTSIZ+1];

    CODE		code;
    struct  SFILE	fctx;			/* sfile for help use	*/
    struct  HELPBLK	helpblk;		/* help control block	*/

    if (NULLSTR(msgkey))			/* no key value		*/
  	{
	  tmmsg(PROCFAIL, "No message key specified.", "TAE-NOMSG",0,0,0,0,0);
	return(FAIL);
	}
    helpblk.compiled   = FALSE;
    helpblk.nextcmd[0] = EOS;				/* initialize	*/
    code = msg_help(&fctx, msgkey, &helpblk);	/* display help on key */
    if (code == SUCCESS)
	s_copy(helpblk.nextcmd, next_cmd);	/* previous mode help cmd   */
    else
      tmmsg(PROCFAIL, helpblk.errmsg, helpblk.errkey,0,0,0,0,0);    /* report error */
    return(code);
    }



#ifdef   XXXXXX
/* NOTE: this routine is not currently used.
 *
 * hlppos - return position context of help data.
 * This position context is assumed to have been previously saved.
 * TBD:  help posit for non-intrinsic procs, help on cmds w/ subcommands.
 */

    FUNCTION static struct POSCTX *hlppos (proc, mode)

    TEXT		proc[];		/* in:  name of proc to get help on	*/
    FUNINT		mode;		/* in:  CMDMODE or other mode		*/

    {
    struct ITRCMD 	*intrin();
    struct ITRCMD	*ip;		
    TEXT		locprc[garbage];


    if (mode != CMDMODE || NULLSTR(proc))
	return(NULL);
    s_copy(proc, locprc);		/* make local copy			*/
    repcmd(locprc);			/* replace if defined via DEFCMD	*/
    ip = intrin(locprc);		/* get ITRCMD pointer 			*/
    if (ip == NULL)			/* is cmd intrinsic?			*/
	return(NULL);
    return(&((*ip).hlppos));		/* return help position			*/
    }
#endif


/* prfhlp - perform help for operator attention mode.
 *
 */

FUNCTION VOID prfhlp 
(
    TEXT		proc[],		/* in:  proc name to help on	*/
    CODE		mode,		/* in:  MENUMODE, TUTORMODE, or INTMODE	*/
    TEXT		prev_cmd[]	/* out: previous mode command 	*/

 )
    {
    IMPORT  BOOL	help_to_term;	/* TRUE if help output is terminal	*/
    IMPORT  BOOL	full_scr_help;	/* TRUE if we want full screen help	*/
    IMPORT  struct SFILE  *pstdo_fil;		/* pointer to std out file context	*/
    IMPORT  CODE	termtype;		/* T_CRT or not			*/

    struct SFILE	fctx;		/* help file context		*/
    CODE		code;
    struct  HELPBLK	helpblk;	/* help output control block	*/  

    full_scr_help = ((termtype == T_CRT) && (pstdo_fil == NULL));
    help_to_term = FALSE;		/* don't force output to terminal */
    if (f_open(&fctx, HELPLUN, HELPLIB, "intmode", HLP_TYPE, F_READ) != SUCCESS)	/* open hlp*/
    	goto pr_nherr;
    helpblk.compiled = FALSE;
    s_copy("interrupt mode", helpblk.helpobj);
    code = genhelp(&fctx, HELPLIB, "", "",&helpblk);	/* process the help file	*/
    if (code !=SUCCESS)
	goto pr_nherr;
    f_close(&fctx, F_KEEP);
    s_copy(helpblk.nextcmd, prev_cmd);	/* copy previous mode command */
    return;

pr_nherr:
    m_put ("No help available on interrupt mode", "TAE-NOHELP",0,0,0,0,0);
    return;

    }

#ifdef XXXXXX
/* NOTE: this function is not currently used.  See also hlppos().
 *
 * puthps - store help position for later access.
 * TBD:  non-intrinsic commands, commands with subcommands.
 */

    FUNCTION static VOID puthps (proc, fctx)

    TEXT		*proc;		/* in: proc name			*/
    struct SFILE	*fctx;		/* in: file context			*/
    

    {
    FAST struct ITRCMD *p;
    struct ITRCMD      *intrin();

    p = intrin(proc);			/* point to intrinsic command entry	*/
    if (p != NULL)			/* if command is intrinsic 		*/
	f_movpos(&((*fctx).posctx), &((*p).hlppos));
    else				/* command not intrinsic 			*/
	;				/* TBD					*/
    return;
    }
#endif

/* /\* */
/*  *	help_msg . Output a help related message to the terminal. */
/*  *\/ */

/* FUNCTION   static  VOID  help_msg */
/* ( */
/*     FUNINT	screen,			/\* true if formatted screen *\/ */
/*     TEXT	msg[],			/\* error message *\/ */
/*     TEXT	key[],			/\* error message key *\/ */
/*     uintptr_t	a1,  */
/*     uintptr_t   a2,  */
/*     uintptr_t   a3,  */
/*     uintptr_t   a4,  */
/*     uintptr_t   a5 */
/*  ) */
/*     { */
/*     if (screen) */
/* 	wrterr(msg, key, a1, a2 , a3, a4, a5); */
/*     else */
/*   	tmmsg(PROCFAIL, msg, key, a1, a2, a3, a4, a5); */
/*     return; */
/*     } */
 


