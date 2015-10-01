/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* This file contains the functions for logging (session and terminal output)
 * and script related individual TAE intrinsic commands.
 *
 * NOTE: The ENABLE-SCRIPT command is included here instead of the misccmd
 * source module because of the restriction that all subcommands pertaining
 * to a given command must be defined contiguously.  Similar reasons
 * for DISABLE-HOST, etc.
 *
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	28-jul-83	Fix ENA-SCRIPT RESIDVAR...peb
 *	21-sep-83	Fix RESIDVAR for UNIX...palm
 *	03-oct-83	Add ENABLE/DISABLE-TOL...nhe
 *	10-oct-83	Fix UNIX lint/compilation errors...palm
 *	19-oct-83	PR 552: Close script files correctly on chaining...palm
 *	31-oct-83	PR 566: fix for double log to session log file...dm
 *	01-nov-83	Add operator attention to putmsg...dm
 *	23-nov-83	Fix no terminal echo of msg (pr #593)...dm
 *	07-feb-84	Y_WRITE-->Y_DISPLAY...nhe
 *	08-mar-84	Avoid closing log file if not open...palm	
 *	12-mar-84	Don't log if batch or async...palm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Move IMPORT delcarations...lim
 *	25-may-84	Trap negative count on ENABLE-SCRIPT (PR 556)
 *	14-aug-84	Reformat output from PUTMSG and vary on $MESSAGE...lia
 *	16-aug-84	pr396: Consolidate I/O error keys...peb
 *	08-dec-84	PR 898: Delete usage of $MESSAGE(2) and define new
 *			subcommand for PUTMSG (-TRACE, -NOTRACE)...lia
 *
 ***********************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	04-mar-85	Add ENABLE-PATH and DISABLE-PATH rcjm commands...dm
 *	16-mar-85	Change min_count for NODE to one...dm
 *
 ************************************************************************
 *  MERGRE WITH FOLLOWING TAE-V1.3 CHANGES ...dm (23-may-85)
 *
 *	06-may-85	Fix dimension with SIZ to SIZ+1...lia
 *
 *************************************************************************
 *
 *	25-jul-85	Fix UNIX lint errors...dm
 *	17-feb-86	New DISABLE-HOST, DISABLE-INTERRUPT, ENABLE-INTERRUPT.
 *	16-jul-86	New ENABLE-RECVAR, DISABLE-RECVAR...palm
 *	08-may-87	PR1181: ENABLE-FORCE_LOWER and DISABLE-...ljn
 *	22-jul-87	Add get_logcmd() as part of effort to force TM 
 *			into libraries...ljn
 *	09-aug-87	Make cmd table GLOBAL; see explanation in
 *			intrinsic.c...palm
 *      03-feb-88       Added ENABLE-FACELIFT and DISABLE-FACELIFT...tpl
 *	24-mar-88	Apollo requires braces on static array initializers...ln
 *	24-mar-88	Deleted TAE_RCJM conditionals...ln
 *	26-jan-89	New valids for RESIDVAR...palm
 *      31-oct-89       Changed default of enable-facelift to display...tpl
 * 11-jan-89	Removed ENABLE-PATH and DISABLE-PATH...krw
 * 23-may-90	Remove RCJM stuff according to old TAE_RCJM...ljn
 * 27-jun-90	Remove Facelift code...ljn
 * 28-jun-90	Removed get_logcmd()...ljn
 * 19-apr-91	Because of VMS def of NULL, some rec fields changed to '0'...ljn
 * 01-aug-91	Braces for static string initializers...ljn
 */


#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"terminc.inc"		/* Terminal related defs		*/
#include	"sessinc.inc"		/* session log  defs and struct		*/
#include	"parblk.inc"
#include "taeintproto.h"



#define		MSGLOGSIZ   STRINGSIZ+KEYSIZ+3	/* allow for key string			*/


/*  TM INTERNAL ERROR NUMBERS USED BY THIS MODULE: 200, 201			*/


/* Memory-resident PDF data for commands performed in this file.
 */

#ifdef VMS
    static TEXT *xsvrHost[] = {"DECW$DISPLAY"};
#else
    static TEXT *xsvrHost[] = {"$DISPLAY"};
#endif
    static TEXT *textZero[] = {"0"};
    static TEXT *tcp[] = {"TCP"};

    BEGIN_VALIDS(netValid, 2)
{{"DECNET"}, {"TCP"}}
    END_VALIDS

    static TEXT  *null_string[] = {""};		/* default null string */
    static TEXT	 *def_key[] = {"TAE-NOKEY"};	/* default message key */

    static TAEINT endf1[] = {1};		/* default is 1 time thru script file	*/


    static TEXT *star[] = {"*"};
static struct I_VALID size_valid = {1, {{700, sizeof (struct PARBLK)}}};
    static TAEINT def_size[] = {sizeof(struct PARBLK)/2} ;

    static struct RESIDVAR init[] = 	/* window-init			*/
	{
/* name    type      k m maxc      size    dc val      dvp*/

	  {"HOST",    V_STRING, 0,1,   1,   24,  1, NULL,   (GENPTR)xsvrHost},
	  {"DISPLAY", V_STRING, 0,1,   1,   24,  1, NULL,   (GENPTR)textZero},
	  {"PROTOCOL",V_STRING, 0,1,   1,   24,  1, (GENPTR)&netValid, (GENPTR)tcp}
	};

    static struct RESIDVAR enable_par[] =	/* ENABLE-RECVAR */
	{
/* name    type      k  m    maxc       size        dc  val      dvp      */

	  {"JOB",     V_STRING, 0, 1,      1,  JOBNAMESIZ+1,    1, NULL,    (GENPTR)star},
	  {"SIZE",   V_INTEGER, 0, 1,      1,             0,    1, (GENPTR)&size_valid, (GENPTR)def_size}
	};


    static struct RESIDVAR ptensc[] =	/* ENABLE-SCRIPT PDF			*/
	{
/* name    type      k  m maxc    size     dc val      dvp*/

	  {"FILE",    V_STRING, 0, 1, 1,    FSPECSIZ, 0, NULL,    NULL},
	  {"COUNT",   V_INTEGER,0, 1, 1,    0,        1, NULL,    (GENPTR)endf1}
	};

    static struct RESIDVAR ptentol[] =	/* ENABLE-TOL PDF			*/
	{
/* name    type      k  m maxc    size     dc val      dvp*/

	  {"FILE",    V_STRING, 0, 1, 1,    FSPECSIZ, 0, NULL,    NULL}
	};

    static struct RESIDVAR  ptpmsg[] = 		/* PUTMSG PDF */
	{
/* name    type      k  m maxc    size     dc val      dvp*/

	  {"MESSAGE",  V_STRING, 0, 1, 1,    MAXSTRSIZ, 1, NULL, (GENPTR)null_string},
	  {"KEY",	    V_STRING, 0, 1, 1,    KEYSIZ,    1, NULL, (GENPTR)def_key}
	};

    static struct RESIDVAR ptwrit[] =	 /* WRITE  PDF		*/
	{
/* name    type      k   m maxc    size     dc val      dvp*/

	  {"STRING",  V_STRING, 0,  1, 1,     MAXSTRSIZ, 1, NULL, (GENPTR)null_string}
	};


/*
 * Intrinsic command table for log and script related commands.
 */

CODE disable_do(struct CONTXT* ,struct CONTXT*), enable_do(struct CONTXT* ,struct CONTXT*), putmsg_do(struct CONTXT* ,struct CONTXT*),
	     enable_int_do (struct CONTXT* ,struct CONTXT*), disable_int_do (struct CONTXT* ,struct CONTXT*), disable_host_do (struct CONTXT* ,struct CONTXT*),
	     write_do(struct CONTXT* ,struct CONTXT*), enable_recvar(struct CONTXT* ,struct CONTXT*), disable_recvar(struct CONTXT* ,struct CONTXT*),
             enable_facelift_do(struct CONTXT* ,struct CONTXT*), disable_facelift_do(struct CONTXT* ,struct CONTXT*);

	CODE enable_force_do(struct CONTXT* ,struct CONTXT*), disable_force_do(struct CONTXT* ,struct CONTXT*);


    GLOBAL struct ITRCMD logcmd[] = 	/* message log related commands  */
	{
{4, "DISABLE",  "FORCE_LOWER", Y_GENERAL,       0,      NULL, disable_force_do},

{4, "DISABLE",  "HOST",        Y_GENERAL,	0,	NULL,  disable_host_do},

{4, "DISABLE",  "INTERRUPT",   Y_GENERAL, 	0,	NULL,  disable_int_do},

#ifdef SESSION_LOG
{4, "DISABLE",  "LOG",   Y_GENERAL,		0,	NULL,   disable_do },
#endif

{4, "DISABLE","RECVAR",   Y_GENERAL,  		0,      NULL, disable_recvar},

{4, "DISABLE",  "TOL",   Y_GENERAL,		0,	NULL,   disable_do },

{4, "DISABLE",	"FACELIFT",    Y_GENERAL, 0,  NULL,disable_facelift_do},

{3, "ENABLE",	"FACELIFT",    Y_GENERAL,I_NPM(init),  init,enable_facelift_do},

{3, "ENABLE", "FORCE_LOWER", Y_GENERAL,        0,      NULL,   enable_force_do},

{3, "ENABLE", "INTERRUPT",Y_GENERAL,            0,      NULL, 	enable_int_do},

{3, "ENABLE","RECVAR",   Y_GENERAL,  I_NPM(enable_par),
						    enable_par,  enable_recvar},

{3, "ENABLE",	"SCRIPT",Y_GENINTER, I_NPM(ptensc),    ptensc, 	enable_do },

{3, "ENABLE",	"TOL",   Y_GENERAL, I_NPM(ptentol),    ptentol,	enable_do },

#ifdef SESSION_LOG
{3, "ENABLE",	"LOG",	 Y_GENERAL,		0,	NULL,   enable_do },
#endif

{0, "PUTMSG",	"NOTRACE",Y_DISPLAY|Y_DEFSUB,
				    I_NPM(ptpmsg),    ptpmsg, 	putmsg_do },
{0, "PUTMSG",	"TRACE", Y_DISPLAY, I_NPM(ptpmsg),    ptpmsg, 	putmsg_do },
{0, "WRITE",	"",	 Y_DISPLAY, I_NPM(ptwrit),    ptwrit, 	write_do  },

{0, ""}					/* TERMINATOR ENTRY: REQUIRED AT END */
	};


/*
 * disable_do - disable session log or disable terminal output log
 */

    FUNCTION CODE disable_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context		*/
    struct CONTXT	*npctx;		/* in:  proc ctx for ENABLE command	*/

    {
    IMPORT struct SESSLOG sesslog;	/* session log information		*/
#ifdef SESSION_LOG2
    IMPORT CODE run_type;
    IMPORT struct SFILE sess2file;      /* 2nd session log structures */
#endif

    if (s_equal((*npctx).subcmd, "TOL")) /* terminal output log close	*/
	{
        t_distol();
	return(DO_CHECK);
	}
#ifdef SESSION_LOG
    else if (s_equal((*npctx).subcmd, "LOG") && sesslog.enable)
    	{
	f_close(&sesslog.sfile, F_KEEP);/* close session log file */
#ifdef SESSION_LOG2
        if (run_type == INTER)          /* close 2nd session log file */
            f_close(&sess2file, F_KEEP);
#endif
	sesslog.enable = FALSE;		/* mark logging  disabled */
	return(DO_CHECK);
    	}
#endif
    else
	return (DO_CHECK);			/* dummy case 		*/
    }

/*
 *	 enable_do - perform ENABLE command
 *			(ENABLE SCRIPT, ENABLE LOG, ENABLE TOL).
 */

FUNCTION CODE enable_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context		*/
    struct CONTXT	*npctx		/* in:  proc ctx for ENABLE command	*/

 )
    {
    IMPORT struct SFILE	 *scr_file;	/* ptr to script file context		*/
    IMPORT COUNT	  scpcnt;	/* number of cycles thru script file	*/
    IMPORT struct SESSLOG sesslog;	/* session log information		*/
#ifdef SESSION_LOG2
    IMPORT struct SFILE sess2file;      /* 2nd session log structure */
#endif
    IMPORT CODE		  run_type;	

    FAST struct VARIABLE *v;		/* variable in symbol table		*/
    static  struct  SFILE  script;	/* script file context 			*/
    FAST CODE		code;
    TEXT		*tolmsg;	/* ptr to err msg for term out log	*/

    if (s_equal((*npctx).subcmd, "SCRIPT"))	/* if function is script		*/
	{
	v = lookex(&((*npctx).parmst), "COUNT");
	scpcnt = IVAL(*v, 0);		/* get # times thru script file desired	*/
	if (scpcnt < 0)
	    goto de_cnterr;		/* Reject <0 count (PR 556) */
	v  = lookex(&((*npctx).parmst), "FILE");	
	if (scr_file != NULL)
	    f_close (scr_file, F_KEEP);
	code = f_opnspc(&script, SCPLUN, SVAL(*v,0), "", "",
	                SCR_TYPE, F_READ);
	if (code != SUCCESS)
	    goto de_operr;	
	scr_file = &script;			/* set pointer to script ctx	*/
	return(DO_CHECK);
	}
    else if (s_equal((*npctx).subcmd, "TOL"))	/* if function is TOL	*/
    	{
    	v = lookex (&((*npctx).parmst), "FILE"); /* find FILE parameter	*/
    	code = t_entol(SVAL(*v,0), &tolmsg); /* start logging	*/
    	if (code != SUCCESS)
    	    goto de_tolerr;			/* bad open		*/
	return(DO_CHECK);
    	}
#ifdef    SESSION_LOG
    else if (s_equal((*npctx).subcmd, "LOG"))	/* if function is LOG	*/
	{
        if (sesslog.enable)
	    return (DO_CHECK);			/* already enabled	*/
	if (run_type != INTER)
	    return (DO_CHECK);			/* interactive only 	*/
	code =  (sesslog.exist) ?		/* open in proper mode */
	f_ophf(&sesslog.sfile, LOGLUN, LOGNAME, F_EXTEND) :
	f_ophf(&sesslog.sfile, LOGLUN, LOGNAME, F_WRITE);
	if (code != SUCCESS) goto de_slerr;
#ifdef SESSION_LOG2
        if (run_type == INTER)          /* open 2nd log file for write/append */
            {
            if (sesslog.exist)
                code = f_ophf(&sess2file,STDOUTLUN,LOG2NAME,F_EXTEND);
            else
                code = f_ophf(&sess2file,STDOUTLUN,LOG2NAME,F_WRITE);
            if (code != SUCCESS)
                goto log2_operr;
            }
#endif

	sesslog.exist = TRUE;			/* mark file as existing */
        sesslog.enable = TRUE;			/* mark logging enabled */
	sltime();				/* time stamp		*/
	return(DO_CHECK);
        }
#endif

de_operr:
    tmmsg(PROCFAIL, "Unable to open script file. %s.",
	  "TAE-OPNRD", (uintptr_t)script.errmsg,0,0,0,0);
    return(DO_CHECK);

de_tolerr:
    if (code == FAIL)
      tmmsg(PROCFAIL, "Terminal logging already enabled.", "TAE-TOLENB",0,0,0,0,0);
    else
	tmmsg(PROCFAIL, "Unable to open terminal log. %s.",
	      "TAE-OPNWRT", (uintptr_t)tolmsg,0,0,0,0);
    return(DO_CHECK);

de_cnterr:
    tmmsg(PROCFAIL, "COUNT parameter must be positive or zero.",
	  "TAE-SCRCNT",0,0,0,0,0);
    return(DO_CHECK);

#ifdef SESSION_LOG
de_slerr:
    tmmsg(PROCFAIL, "Unable to open session log file. %s.",
	  "TAE-OPNWRT", (uintptr_t)sesslog.sfile.errmsg,0,0,0,0);
    return(DO_CHECK);
#endif

#ifdef SESSION_LOG2
log2_operr:
    tmmsg(PROCFAIL, "Unable to open 2nd session log file. %s.",
	  "TAE-SESS2OPN",(uintptr_t) sess2file.errmsg,0,0,0,0);
    return(DO_CHECK);
#endif
    }

/*	enable/disable -FORCE_LOWER.
 *
 *	FORCE_LOWER means to translate all filespecs to 
 *	lower case unconditionally.
 */
FUNCTION CODE enable_force_do (struct CONTXT* UNUSED(X1) ,struct CONTXT* UNUSED(X2))
	{
	f_force_lower(TRUE);
	return (DO_CHECK);
	}



	FUNCTION CODE disable_force_do(struct CONTXT* UNUSED(X1) ,struct CONTXT* UNUSED(X2))
	{
	f_force_lower(FALSE);
	return (DO_CHECK);
	}

/*	enable_int_do, disable_int_do, and disable_host_do
*/

    /*	This un-does what DISABLE-INTERRUPT, i.e., back to normal	*/

    FUNCTION CODE enable_int_do (struct CONTXT* UNUSED(X1) ,struct CONTXT* UNUSED(X2))
  
    {
    IMPORT BOOL int_enabled;

    int_enabled = TRUE;
    return (DO_CHECK);
    }



    /* This says: "No interrupt mode; attention sequence means ABORT."  */

    FUNCTION CODE disable_int_do (struct CONTXT* UNUSED(X1) ,struct CONTXT* UNUSED(X2))

    {
    IMPORT BOOL int_enabled;

    int_enabled = FALSE;
    return (DO_CHECK);
    }


    
    /*	This prevents user from getting into host command language	*/

    FUNCTION CODE disable_host_do (struct CONTXT* UNUSED(X1) ,struct CONTXT* UNUSED(X2))

    {
    IMPORT BOOL host_enabled ;
    
    host_enabled = FALSE;
    return (DO_CHECK);
    }


/*
 *	putmsg_do - put message into session log and write to terminal .
 */


FUNCTION CODE putmsg_do
(
    struct CONTXT	*cpctx,		/* in:  containing proc context		*/
    struct CONTXT	*npctx		/* in/out:  proc ctx for ENABLE 	*/

 )
    {
    IMPORT TEXT		lastkey[];	/* key of last displayed message  */
    IMPORT struct VARIABLE *skey_gbl;	/* pointer to $SKEY variable	  */

    struct VARIABLE 	*v1;		/* pointer to MESSAGE in symbol table */
    struct VARIABLE 	*v2;		/* pointer to KEY in symbol table */
    TEXT		msgtext[MSGLOGSIZ+1];	/* local buffer for msg + key	*/
    TEXT		tracetxt[STRINGSIZ+1];	/* local buffer for trace info	*/
    TEXT 		record[2*STRINGSIZ+1];	/* local buffer for standard output */
    COUNT		nchar;			/* length of output string	*/

    v1 = lookex(&(*npctx).parmst, "MESSAGE");	
    v2 = lookex(&(*npctx).parmst, "KEY");
    m_pfmt(SVAL(*v1, 0), SVAL(*v2, 0), msgtext,0,0,0,0,0); /* format message	    */
    s_copy(msgtext, record);
    tracetxt[0] = EOS;
    addproc(cpctx, tracetxt);			/* format proc name/line #     */
    nchar = s_length(record) + s_length(tracetxt);
    if (s_equal((*npctx).subcmd, "TRACE") &&
        nchar <= 2*STRINGSIZ && tracetxt[0] != EOS)
	{
	s_append(";\n\r", record);
	s_append(tracetxt, record);
	}
#ifdef PUTMSG_FIX
    if (s_length(SVAL(*v2,0)) == 0 || s_equal(SVAL(*v2,0), " "))
        put_stdout(SVAL(*v1,0));        /* omit prefix and bell if no key */
    else
        {
        put_outmsg (record);                    /* write to output devices  */
        set_string (skey_gbl, SVAL(*v2, 0));    /* set $skey to key         */
        s_copy(SVAL(*v2, 0), lastkey);          /* save key for help-message */
        }
#else
    put_outmsg (record);			/* write to output devices  */
    set_string (skey_gbl, SVAL(*v2, 0));	/* set $skey to key	    */
    s_copy(SVAL(*v2, 0), lastkey);		/* save key for help-message */
#endif

#ifdef SESSION_LOG
    if (tracetxt[0] == EOS)
	slwrite ("MS: ", msgtext);
    else
	{
	nchar = s_length(msgtext);
	if (nchar > STRINGSIZ-7)
	    s_copy ("...; +", &msgtext[STRINGSIZ-10]);
	else
	    s_append ("; +", msgtext);
	slwrite ("MS: ", msgtext);		/* write to session log     */
    	slwrite ("MS: ", tracetxt);
	}
#endif
    return(DO_SUCCESS);
    }

/*
 *	write_do - write message to standard output.
 */

FUNCTION CODE write_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context		*/
    struct CONTXT	*npctx		/* in:  proc ctx for ENABLE command	*/

 )
    {
    FAST struct VARIABLE *v;		/* variable in symbol table		*/

    v = lookex(&(*npctx).parmst, "STRING");	
    if (v == NULL)
	{
	tmierr(201);			/* TM INTERNAL ERROR: wrong wrt context */
	return(DO_CHECK);
	}
    put_stdout(SVAL(*v,0));
    return(DO_SUCCESS);
    }

FUNCTION CODE enable_facelift_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context    */
    struct CONTXT	*npctx		/* in:  proc ctx for CONT cmd line */

 )
    {
    tmmsg (PROCFAIL, "Facelift is no longer a supported feature of TAE Plus.",
	   "TAE-NOFACE",0,0,0,0,0);
    return (DO_CHECK);
    }

FUNCTION CODE disable_facelift_do 
(
    struct CONTXT	*cpctx,		/* in:  containing proc context    */
    struct CONTXT	*npctx		/* in:  proc ctx for CONT cmd line */

 )
    {
    tmmsg (PROCFAIL, "Facelift is no longer a supported feature of TAE Plus.",
	   "TAE-NOFACE",0,0,0,0,0);
    return (DO_CHECK);
    }
