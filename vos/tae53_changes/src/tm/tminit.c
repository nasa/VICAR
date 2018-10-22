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
 *	TM initialization functions.
 *
 *	CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	21-aug-83	For nullable era and cleanliness, $APLIB init'ed
 *			to zero count; v_nullable set when minc == 0...palm
 *	29-aug-83	New bld_valid call sequence...palm
 *	12-sep-83	New $LASTCMD global...palm
 *	10-oct-83	Fix unix compilation errorrs...palm
 *	28-oct-83	Add new $SYSCHAR...palm
 *	14-jan-84	Set first cmd in $LASTCMD to be "SLOGON"...palm
 *	15-feb-84	Add $MENUS for menu stack...dm
 *	27-feb-84	Remove $DEFCMD; it is now build by defcmd_do...palm
 *	02-mar-84	Make implicit.pdf to taegbl.pdf...palm
 *	27-apr-84	Add new _gbl pointers (dummy goes away)...ces
 *	07-may-84	Move the IMPORT declarations...lim
 *	08-jun-84	New $echo, $aecho, and $becho...palm
 *	25-jun-84	Remove obsolete TBD...palm
 *	24-jul-84	Allow $SYSLIB & $USERLIB to be nullable...lia
 *	01-aug-84	New counts for $APLIB and $MENUS...palm
 *	09-aug-84	New $SR and $IR globals...palm	
 *	14-aug-84	Add a second entry to $MESSAGE for trace...lia
 *	24-aug-84	Add a second entry to $TUTOR for dynamic tutor...lia
 *	17-oct-84	PR 847:  set v_implicit in implicit globals...peb
 *	09-nov-84	PR 885: add TRACE to valid list for echo...palm
 *	08-dec-84	PR 898: change $TUTOR(2) to $DYNTUT and delete 
 *			$MESSAGE(2)...lia
 *	18-dec-84	Initialize defpdf in ini_global for looping tm...dm
 *	16-jul-86	Add $JOB to contain $SESSION if interactive or
 *			current jobname if ASYNC...palm
 *	17-sep-86	Add $PARENT...palm
 *	28-mar-87	New $MENUOPT...palm
 *	08-may-87	PR1181: "TAEGBL" and "SLOGON" to lower...ljn
 *	10-aug-87	New $FILEVER...palm
 *	24-mar-88	Apollo requires braces on static array initializer...ln
 *      07-apr-88       Added $TUTSELE...tpl
 *      02-jun-88       Added $TUTOPT...tpl
 *	08-dec-88	Added NO_SELFTUTOR to $TUTOPT...palm
 *	15-dec-89	Re-alphabetize intrinsic globals...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	22-oct-92	Prototyping tae_alloc is unnecessary and Ultrix 4.3
 *			does not like it...rt
 *
 */


#include	"taeconf.inp"	/* TAE configuration (REQUIRED)		*/
#include	"symtab.inc"	/* TM symbol table			*/
#include 	"terminc.inc"	/* terminal package			*/
#include	"tmhost.inp"	/* TM host-dependent definitions	*/
#include	"tminc.inc"	/* TM definitions			*/
#include "taeintproto.h"


    static struct DEFPDF *defpdf = NULL;


#define DEF_GLOBAL(gblptr, a1, a2, a3, a4, a5, a6, a7, a8, a9)     \
     if ((gblptr = defglobal(a1, a2, a3, a4, a5, a6, a7, a8, a9)) == NULL) \
	goto error



/*
 *	ini_globals.   Create predefine globals.
 *
 *	Those globals which are to be implicitly referenced by all
 *	procs get their v_implicit bit set here.
 */

    FUNCTION CODE ini_globals(void)

    {
    IMPORT  struct  VARIABLE	*aecho_gbl;	/* pointer to $aecho	*/
    IMPORT  struct  VARIABLE	*becho_gbl;	/* pointer to $becho	*/
    IMPORT  struct  VARIABLE	*echo_gbl;	/* pointer to $echo	*/
    IMPORT  struct  VARIABLE    *prompt_gbl;	/* pointer to $prompt	*/
    IMPORT  struct  VARIABLE    *sfi_gbl;	/* pointer to $sfi	*/
    IMPORT  struct  VARIABLE 	*skey_gbl;	/* pointer to $skey	*/
    IMPORT  struct  VARIABLE 	*switch_gbl;	/* pointer to $switch	*/
    IMPORT  struct  VARIABLE	*tutor_gbl;	/* pointer to $tutor	*/
    IMPORT  struct  VARIABLE	*dyntut_gbl;	/* pointer to $dyntut	*/
    IMPORT  struct  VARIABLE    *last_gbl;	/* pointer to $LASTCMD	*/
    IMPORT  struct  VARIABLE	*char_gbl;	/* pointer to $SYSCHAR  */
    IMPORT  struct  VARIABLE	*menu_gbl;	/* pointer to $MENUS	*/
    IMPORT  struct  VARIABLE	*apl_gbl;	/* pointer to $APLIB 	*/
    IMPORT  struct  VARIABLE	*usrl_gbl;	/* pointer to $USERLIB 	*/
    IMPORT  struct  VARIABLE	*sysl_gbl;	/* pointer to $SYSLIB 	*/
    IMPORT  struct  VARIABLE	*run_gbl;	/* pointer to $RUNTYPE  */
    IMPORT  struct  VARIABLE	*log_gbl;	/* pointer to $LOG 	*/
    IMPORT  struct  VARIABLE	*msg_gbl;	/* pointer to $MESSAGE 	*/
    IMPORT  struct  VARIABLE	*sess_gbl;	/* pointer to $SESSION	*/
    IMPORT  struct  VARIABLE	*job_gbl;	/* pointer to $JOB	*/
    IMPORT  struct  VARIABLE	*parent_gbl;	/* pointer to $PARENT	*/
    IMPORT  struct  VARIABLE	*menuopt_gbl;	/* pointer to $MENUOPT	*/
    IMPORT  struct  VARIABLE	*tutoropt_gbl;	/* pointer to $MENUOPT	*/
    IMPORT  struct  VARIABLE	*filever_gbl;	/* pointer to $FILEVER  */
    IMPORT  struct  VARIABLE	*tutsel_gbl;	/* pointer to $FILEVER  */

    /*
     *	String value vectors:
     */
    static TEXT	string[STRINGSIZ+1];		/* gp string		*/
    static TEXT	session[STRINGSIZ+1];		/* session string	*/
    static TEXT	*syslib[] = {SYSLIB};		/* name of TAE syslib	*/
    static TEXT	*null_string[] = {""};		/* null string		*/
    static TEXT	*runtype[] = {"INTERACTIVE"};	/* for $RUNTYPE		*/
    static TEXT	*yes_string[] = {"YES"};	/* for $TUTSELE		*/
    static TEXT	*no_string[] = {"NO"};		/* for $ECHO		*/
    static TEXT	*prompt[] = {"TAE"};		/* for $PROMPT		*/
    static TEXT *log[] = {LOGNAME};		/* session log file	*/
    static TEXT *string_vec[] = {string};	/* general vector	*/
    static TEXT *session_vec[] = {session};	/* session vector	*/
    static TEXT *message[] = {"SILENT"};		/* default mode	*/
    static TEXT	*tutor[] = {"NOSCREEN"};	/* unless supported CRT	*/
    static TEXT	*dyntut[] = {"NOSCREEN"};	/* for $DYNTUT		*/
    static TEXT *syschar[] = SYS_CHAR;		/* system characteristic*/
    struct VARIABLE *dummy;
#define C_SYSCHAR  sizeof(syschar)/sizeof(TEXT *)

    static TEXT *msg_v[] =
		{"SILENT", "BELL", "PAUSE", "ATTN"};
    static TEXT *echo_v[] =
		{"YES", "NO", "FULL", "BODY", "TRACE"} ;
    static TEXT *tutor_v[] =
	        {"NOSCREEN", "SCREEN"};
    static TEXT *slogon[] = {"slogon"};
    static TEXT *filever_v[] = {"YES", "NO"};
    static TEXT *tutsel_v[] = {"YES", "NO"};

    static TEXT *menuopt_v[] = {"NO_TAG", "NO_NAME", "NO_LIBRARY",
				"NO_PRESS_FOR_MENU"};

    static TEXT *tutoropt_v[] = {"NO_TAG", "NO_NAME", "NO_LIBRARY",
				"NO_SELFTUTOR",
				"COMPRESS"};

    TAEINT	zero = 0;
    TAEINT	one =  1;
    defpdf = NULL;			/* initialize	for each run */

/*
 ******************** NOTE ***********************
 *
 *	Intrinsic globals should be defined in alphabetical order
 *	so, as new globals are added, they can be added in order
 *	so that DISPLAY-GLOBALS does not have to do a sort.
 *

name         type      size      m    m     c     value vector      valid
----   	     ----      ----	 i    a     o     ------------      -----
*/

DEF_GLOBAL(aecho_gbl,
"$AECHO",   V_STRING,     4,	 1,   10,   1, (GENPTR)no_string,   echo_v, 5);

DEF_GLOBAL(apl_gbl, 
"$APLIB",  V_STRING, FSPECSIZ,   0, APLIB_COUNT,
					    0,           NULL,       NULL, 0);

DEF_GLOBAL(becho_gbl,
"$BECHO",   V_STRING,     4,     1,   10,   1, (GENPTR)no_string,   echo_v, 5);

DEF_GLOBAL(dyntut_gbl, 
"$DYNTUT",  V_STRING,	  8, 	 1,    1,   1, (GENPTR)dyntut,	    tutor_v, 2);

DEF_GLOBAL(echo_gbl,
"$ECHO",   V_STRING,      4,     1,   10,   1, (GENPTR)no_string,   echo_v, 5);

DEF_GLOBAL(filever_gbl,
"$FILEVER",   V_STRING,   3,     1,   1,   1, (GENPTR)no_string,  filever_v, 2);

DEF_GLOBAL(dummy,	/* general purpose integer register 	*/	
"$IR",    V_INTEGER,      0,     0,REG_COUNT,0,  	 NULL,	     NULL,  0);

/* 	$JOB starts off as the $SESSION string.  For ASYNC jobs, this will
 * 	get changed to the TAE jobname assigned by the parent.
 */

getsession(session);
DEF_GLOBAL(job_gbl, 
"$JOB",    V_STRING, JOBNAMESIZ,  1,      1, 1, (GENPTR)session_vec,  NULL, 0);

DEF_GLOBAL(last_gbl,
"$LASTCMD", V_STRING,MAXSTRSIZ, 0,
		              HISTORY_LOG,
			                   1, (GENPTR)slogon,      NULL, 0);
DEF_GLOBAL(log_gbl, 
"$LOG",    V_STRING, FSPECSIZ,  1,      1, 1, (GENPTR)log,         NULL, 0);

#define MENU_OPTIONS   sizeof (menuopt_v) / sizeof (TEXT *)
DEF_GLOBAL(menuopt_gbl, 
"$MENUOPT",  V_STRING,  VALIDSIZ, 0, MENU_OPTIONS,
					   0,	     NULL,      menuopt_v, 
								  MENU_OPTIONS);
DEF_GLOBAL(menu_gbl, 
"$MENUS",  V_STRING,  FSPECSIZ, 0, MENUS_COUNT,
					   0,	     NULL,         NULL, 0);
 
DEF_GLOBAL(msg_gbl, 
"$MESSAGE", V_STRING,	     6, 1,	1, 1, (GENPTR)message,    msg_v, 4);
 
DEF_GLOBAL(parent_gbl, 
"$PARENT",    V_STRING, JOBNAMESIZ,  0,      1, 0,    NULL,        NULL, 0);

DEF_GLOBAL(prompt_gbl, 
"$PROMPT",  V_STRING,        15, 1,      1, 1, (GENPTR)prompt,    NULL, 0);

DEF_GLOBAL(run_gbl,
"$RUNTYPE", V_STRING,        20, 1,      1, 1, (GENPTR)runtype,   NULL, 0);
(*run_gbl).v_implicit = TRUE;

DEF_GLOBAL(sess_gbl,
"$SESSION", V_STRING, MAXSTRSIZ, 1,      1, 1, (GENPTR)session_vec,   NULL, 0);
(*sess_gbl).v_implicit = TRUE;

DEF_GLOBAL(sfi_gbl, 
"$SFI",    V_INTEGER,         0, 1,      1, 1, (GENPTR)&one,         NULL, 0);
(*sfi_gbl).v_implicit = TRUE;

DEF_GLOBAL(skey_gbl, 
"$SKEY",   V_STRING,  MAXSTRSIZ, 1,      1, 1, (GENPTR)null_string,  NULL, 0);
(*skey_gbl).v_implicit = TRUE;

DEF_GLOBAL(dummy,	/* general purpose string register	*/
"$SR",    V_STRING, MAXSTRSIZ,  0,REG_COUNT, 0, 	NULL,        NULL, 0);

DEF_GLOBAL(switch_gbl,
"$SWITCH", V_INTEGER,         0, 1,      1, 1, (GENPTR)&zero,	     NULL, 0);
(*switch_gbl).v_implicit = TRUE;

DEF_GLOBAL(char_gbl,
"$SYSCHAR", V_STRING, MAXSTRSIZ, 0,  5+C_SYSCHAR,
    				      C_SYSCHAR, (GENPTR)syschar,    NULL, 0);

DEF_GLOBAL(sysl_gbl,
"$SYSLIB", V_STRING,  FSPECSIZ,  0,      1, 1, (GENPTR)syslib,	     NULL, 0);

#define TUTOR_OPTIONS   sizeof (tutoropt_v) / sizeof (TEXT *)
DEF_GLOBAL(tutoropt_gbl, 
"$TUTOPT",  V_STRING,  VALIDSIZ, 0, TUTOR_OPTIONS,
			   0,	     NULL,      tutoropt_v, TUTOR_OPTIONS);

DEF_GLOBAL(tutor_gbl, 
"$TUTOR",  V_STRING,	      8, 1,      1, 1, (GENPTR)tutor,     tutor_v, 2);

DEF_GLOBAL(tutsel_gbl,
"$TUTSELE",   V_STRING,   3,     1,   1,   1, (GENPTR)yes_string,  tutsel_v, 2);

getulib(string);				/* get user library name */
DEF_GLOBAL(usrl_gbl,
"$USERLIB",V_STRING,  FSPECSIZ,  0,      1, 1, (GENPTR)string_vec,   NULL, 0);

return(SUCCESS);


error:			/* DEF_GLOBAL branches here on allocation error */
return(FAIL);
}

/*
 *   defglobal.	   Build a intrinsic global VARIABLE.
 *	
 *	We are not too concerned about memory allocation failures here
 *	because we know we are initializing.
 */

FUNCTION struct VARIABLE  *defglobal 
(
    TEXT	name[],		/* in: name of global		*/
    FUNINT	type,		/* in: type 			*/
    FUNINT	size,		/* in: size if string		*/
    FUNINT	minc,		/* in: minimum count		*/
    FUNINT	maxc,		/* in: max count		*/
    FUNINT	count,		/* in: current count		*/
    GENPTR	vp,		/* in: current value vector	*/
    TEXT	*valid[],	/* in: valid strings		*/
    FUNINT	count_v	/* in: count of valid vector	*/
 )
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table	*/

    struct VARIABLE      *v;
    static struct DEFPDF  implicit = {0, {SYSLIB, "taegbl", PDF_TYPE, ""}};
    static struct SYMTAB dst = {NULL};	/* dummy symbol table	*/    

    if ((v = allvar(&glbtab)) == NULL)
	return(NULL);
    s_copy(name, (*v).v_name);
    (*v).v_type = type;
    (*v).v_class = V_GLOBAL;
    (*v).v_protect = TRUE;	/* all predefined globals: no delete	*/
    (*v).v_refcnt = 0;
    (*v).v_count = 0;		/* so no de-allocation by set_value	*/
    (*v).v_nullable = (minc == 0);
    (*v).v_minc = (minc == 0) ? 1 : minc;
    (*v).v_maxc = maxc;
    (*v).v_size = size;
    (*v).v_intrinsic = TRUE;
    (*v).v_implicit = FALSE;
    if (valid != NULL)
	bld_valid(&dst, v, valid, count_v);	/* build valid struct	*/
    else
	(*v).v_valid = NULL;
    (*v).v_cvp = allval(v);			/* allocate value vector*/
    set_value (v, vp, count);			/* set value 		*/
    if (defpdf == NULL)				/* set up DEFPDF	*/
        {
        defpdf = (struct DEFPDF *) tae_alloc(1, sizeof(struct DEFPDF));
        MOVE_STRUCT (implicit, *defpdf);
        }
    (*v).v_pdf = defpdf;			/* defining "PDF"	*/
    (*defpdf).refcount++;
    return(v);
    }
