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



/* This file contains the functions to perform the TUTOR command.
 * A table of associated intrinsic command structure entries,
 * and the associated resident PDFs are also in this file
 * (the table is at the end of the file).
 * The functions callable by other parts of TM are first in the source file,
 * and are in alphabetical order beginning with cls_tutor and ending with
 * tutor_do.
 * After tutor_do, all functions in this source file are in alphabetical order.
 * In this alphabetization, the underscore character was assumed to precede
 * alphabetic characters, which were, in turn, assumed to precede numeric
 * characters.
 *
 * On December 7, 1984 tutor.c was broken up into:
 *
 *	- tutor.c		TM callable and certain support functions
 *	- tutcmd.c		functions called for specific tutor mode commands
 *	- tutdisp.c		functions supporting tutor displays
 *	- tuthelp.c		functions for processing help files (except tuthlp)
 *
 * In addition, three functions were moved to the already existing tutsubs.c
 *
 * CHANGE LOG:
 *
 *	18-jul-83	Fix PRs 262, 409, 412, 430...peb
 *	22-jul-83	Add value panel scrolling...peb
 *	28-jul-83	Fix bug in dispval...peb
 *	29-jul-83	Fix bug in SHOW, add SHOW to prompt...peb
 *	04-aug-83	Updated reini_ctx() to match with inictx()...dm
 *	08-aug-83	Protect adjval against null tutor extension...peb
 *	10-aug-83	Fix bugs in V_NAME parms...peb
 *	17-aug-83	Implement help on message key, get help output
 *			via helpblk...dm
 *	22-aug-83	Nullables; new DEFAULT for names...palm
 *	02-sep-83	PR 360: dash in VMS filespec...palm
 *	08-sep-83	New name of forpname (m_fpname)...palm
 *			Also, forpval to m_fpval...palm
 *	08-sep-83	Change proc exit message key...palm
 *	08-sep-83	Restructure for tutor primitives...peb
 *	15-sep-83	Some bug fixes -- see marked up listing ...palm	
 *	21-sep-83	Fix RESIDVAR for UNIX...palm
 *	26-sep-83	reini_ctx made external...palm
 *	05-oct-83	Change intcmd() to cmd_screen() etc...dm
 *	06-oct-83	Update for errmsg return from gettitle()...dm
 *	11-oct-83	T for TUTOR; R for RUN; unix compilation errors...palm
 *	12-oct-83	Fix minor recompilation error...dm
 *	18-oct-83	Fix null_str extern declare...palm
 *	29-oct-83	Init scnflg before opnpdf if proc coming from menu to
 *			avoid flashing error messages...dm
 *			Update d_init calling sequence (for .if)...dm
 *	31-oct-83	prim_act usage, various PR fixes...peb
 *	18-jan-84	Micro-editor...palm
 *	03-feb-84	Don't allow RUN for procs if proc int'pt mode...nhe
 *	09-feb-84	Change 'scnflg' to 'menu_screen' etc...dm
 *	12-feb-84	Call d_incclose after help text read...dm
 *	14-feb-84	Friendlier message when no parms...palm
 *	23-feb-84	New micro-editor fixes; new dynamic tutor mode
 *			scheme...palm
 *	27-feb-84	Parameter summary page help...dm
 *	27-feb-84	Fix dispreq in tutrun...palm
 *	01-mar-84	Parameters and quals on command line ...dm
 *	01-mar-84	Delete DYNMODE logic...palm
 *	02-mar-84	Clean-ups...dm
 *	03-mar-84	Fix help related problems, allow for ASYNC...dm
 *	05-mar-84	Do not change v_default bit setting in rsetdef(),
 *			change the function name to rsetini()...dm
 *	09-mar-84	Fix bug in run_async call...dm
 *	11-mar-84	Dynamic tutor (T/F) flag as opn_tutor argument...dm
 *	13-mar-84	Fix tutor crash on latching on intrinsics...dm
 *	15-mar-84	Rename static variable hf to helpf ...dm
 *	15-mar-84	Do dyntut logging...ne
 *	16-mar-84	Reset sfi, skey in case of DO_RUN or DO_EXIT...dm
 *	20-mar-84	Fix no message if HELP subcmd and no help...palm
 *	22-mar-84	Fix TAE-TOOFEW  error message (PR # 467)...dm
 *	30-apr-84	Make value vectors a union in tut_set_value...lim
 *	04-may-84	VALUE_x to xVAL, add tutor_gbl, set_string, and replace
 *			addref using sess_gbl and run_gbl ...ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *			No more dyncmd mode ... nhe
 *			'toprefs'-->'numrefs'...nhe
 *	23-may-84	Fix so all subcommands displayed...nhe
 *	04-jun-84	Implement default intrinsic subcommand...lim
 *	14-jun-84	Support for Y_PDF bit in ITRCMD...palm
 *	15-jun-84	Fix bad error message for ambiguous subcmd...lim
 *	19-jun-84	Allow tutor to be re-entered after RUN...nhe
 *	06-jul-84	Fix value panel fill bug...peb
 *	06-jul-84	Display more string components in value panel...peb
 *	16-jul-84	Display library for hardcopy user...lia
 *	23-jul-84	Use switch 10 for dynamic tutor library request by
 *			hardcopy user...lia
 *	02-aug-84	Fix crash in help * with no help available...lia
 *	03-aug-84	Delete excess f_close for message key help...lia
 *	16-aug-84	pr396:  TAE-HELPRD --> TAE-RDERR...peb
 *	24-aug-84	Implement $TUTOR(2) to direct screen/noscreen mode
 *			for dynamic tutor...lia
 *	31-aug-84	Update for new global help flags full_screen...nhe
 *	13-sep-84	Fix problems with output of error message...lia
 *	17-sep-84	Audit error conditions causing change in micro-editor
 *			context...lia
 *			Add process for error code V_BADAMBIG...lia
 *	18-sep-84	Always repaint tutor screen for PAGE command...lia
 *	12-oct-84	Change t_pos calls to pos_scroll() as for UNIX 1.2...dm
 *	16-oct-84	Updates for internal procs
 *	17-oct-84	PR 847: reini_ctx to use v_implicit...peb
 *	20-oct-84	TCL 117: Handle help in compiled PDFs...peb
 *	23-oct-84	Fix help bug in 20-oct-84 change...peb
 * 	23-oct-84	Add tutor from async...nhe
 *	26-oct-84	Add subcommand help in tutor...lia
 *	29-oct-84	Deleted wrong structure reference bug...lia
 *	30-oct-84	Add HOLD command for async...nhe
 *	01-nov-84	PR 882: test for previous command before display...lia
 *	11-nov-84	Re-order update_ctx to do setqlf earlier...nhe
 *	20-nov-84	Modify for qualified directives...lia
 *	01-dec-84	TCL 67: Move statics to, merge EDTCTX with,TUTCTX...peb
 *	07-dec-84	TCL 67: Breakup tutor, many functions moved...peb
 *	08-dec-84	PR 898: change references of $TUTOR(2) to $DYNTUT...lia
 *	10-dec-84	TCL 67...peb
 *	16-jan-85	Add setqlf call to properly setup qualifier symbol tab
 *			in prep_intrin...lia
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	29-aug-85	PR#988: Fix q_get_exec to avoid infinite loop in parm
 *			qualifier display...joh
 *	09-sep-85	PR 961: Prevent user from entering HOLD command if tutor
 *			in synchronous mode (gttutcmd, tutor-do)..dab
 *	11-sep-85	PR 871: Added an extra argument to get_parms_remote call
 *			for multiple preface lines...lia
 *	13-sep-85	Check for synchr. mode & HOLD command in tutor rather
 *			than gttutcmd so that HOLD from HELP menu rejected...dab
 *	08-mar-86	Add action_setup call for ACTION= PARMSETs
 *	18-jul-86	Check for new ASYNC_PROCESS return from runtype...palm
 *	10-sep-86	PR 915: Reinitialize "internal procs" in context block
 *			(routine reini_ctx) before 2nd pass thru PDF file...dab
 *	31-jul-86	Added code for the TAE_FACELIFT...KRW
 *	11-feb-87	Merged TAE FACELIFT with V1.4 TAE...krw
 *
 ************************ Post TAE 2.1 changes *******************************
 *
 *	19-may-87	Updated for TAE-Plus philosophy. Checked global 
 *			variable DisplayId to determine VT100 or Window 
 *			mode screen operations...dm
 *      13-aug-87       Merged in 2.2 changes...tpl
 *	02-dec-87	Declare DisplayId as GENPTR...dm
 *	26-jan-88	Merge overlooked VMS change involving tutselect...ljn
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl 
 *	08-feb-88	Fix failure to re-open procctx when tutor command
 *			processing fails within a procedure...palm
 *	24-feb-88	PR 1504: Change size of label arg in cmd_parse call...ln
 *	29-jun-88	Do f_read in cls_tutor() only if not compiled PDF; in
 *			opn_tutor() save position from context.prcpos...ljn
 *	28-nov-88	Honor OPTIONS=SELFTUTOR...palm
 *	09-dec-88	Honor COMPRESS_TUTOR ...palm
 *	01-feb-89	VAX C required IMPORT BOOL lookex()...ljn
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	27-jun-90	Removed Facelift code...ljn
 *	28-jun-90	Removed get_tutorcmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 */

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"tmhost.inp"		/* host-dependent defs			*/
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"parblk.inc"		/* param block for subprocess communic	*/
#include	"terminc.inc"
#include	"syninc.inc"		/* syntax package defs & structs	*/
#include	"eventinc.inp"		/* event package defs			*/
#include	"asyncinc.inc"		/* to get ASYNC_PROCESS return	*/
#include "taeintproto.h"


    IMPORT struct TUTCTX tutctx;	/* tutor & micro-editor context	*/

    static TEXT		msg_mmov[] = "Terminal Monitor internal memory overflow.";
    static TEXT		key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/
    static TEXT		msg_unre[] = "Unrecognized parameter name, '%s'.";
    static TEXT		key_unre[] = "TAE-TUNRECPAR";
    static TEXT		msg_unrs[] = "Unrecognized subcommand, '%s'.";
    static TEXT		key_unrs[] = "TAE-UNRECSUB";
    static TEXT		msg_tsub[] = "Unrecognized command for tutor subcommand display.";
    static TEXT		key_tsub[] = "TAE-TSUNTCMD";
    static TEXT		msg_tqual[] = "Unrecognized command for parameter qualifier tutor display.";
    static TEXT		key_tqual[] = "TAE-TQUNTCMD";
    static TEXT		msg_tcmd[] = "Unrecognized tutor command.";
    static TEXT		key_tcmd[] = "TAE-BADTUTCMD";
    static TEXT		msg_sbin[] = "Subscript must be an integer constant.";
    static TEXT		key_sbin[] = "TAE-TNISUBSC";
    static TEXT		msg_synho[] = "HOLD command invalid for synchronous tutor";
    static TEXT		key_synho[] = "TAE-SYNTUTHLD";

FUNCTION static struct TUTCMD *gprevcmd 
(
    TEXT		cmdstr[],	/* out: command string			*/
    TEXT		cmdprm[],	/* out: command parameter (if not parm=)*/
    struct CONTXT	*pctx		/* in:  proc context			*/

 );
FUNCTION static struct TUTCMD *gttutcmd 
(
    TEXT		cmdstr[CMDLINSIZ+1], /* out: command string		*/
    TEXT		cmdprm[],	/* out: command parameter (if not parm=)*/
    struct CONTXT	*pctx		/* in:  proc context			*/

 );
FUNCTION static struct TUTCMD *srchtab 
(
    TEXT		cmdstr[]	/* in:  command string			*/

 );

FUNCTION static VOID initut (void);
FUNCTION  static  CODE  prep_pdf
(
    struct  	CONTXT	*procctx,		/* in: proc context	     */
    struct  	CONTXT	*cmdctx,		/* in: command context	     */
    TEXT		procnm[],		/* in: name of proc to tutor */
    CODE		screen_select		/* in: screen selection	     */

 );
FUNCTION static VOID reini_tut(void);

FUNCTION static CODE tut_itrsub 
(
    struct CONTXT	*ctx,		/* in/out: proc context			*/
    struct ITRCMD	*itrcmd_1st,	/* in:  1st ITRCMD entry for this cmd name*/
    struct ITRCMD	**itrcmd,	/* out: the actual ITRCMD for this cmd-subc*/
    struct SFILE	**hf		/* out: the help file			*/

 );
FUNCTION static CODE tut_subcmd 
(
    struct CONTXT	*ctx,		/* in/out: proc context			*/
    struct SFILE	*pf,		/* in/out: proc file			*/
    struct SFILE	**hf		/* out:  help file			*/

 );
FUNCTION  static CODE  update_ctx
(
    struct    	CONTXT	*procctx,	/* in: proc context		*/
    struct	CONTXT	*cmdctx,	/* in: command context		*/
    TEXT		cmdstr[],	/* in: command line text	*/
    CODE		screen_select  /* in: screen selection		*/

 );



/* Memory-resident PDF data for commands performed in this file.
 * The parameters for each command are defined
 * by a vector of RESIDVAR structures.
 * The RESIDVAR structure is explained in the TMINC include file.
 */


    static TEXT *null_str[] = {""};
    static struct RESIDVAR pttutr[] =	/* TUTOR PDF			*/
	{
/* name    type      k  m maxc      size     dc val      dvp*/

  {"PROC",    V_STRING, 0, 1, 1,     STRINGSIZ, 1, NULL,   (GENPTR)null_str}
	};


FUNCTION CODE tutor_do 
(
    struct CONTXT	*procctx,	/* in:  containing proc context		*/
    struct CONTXT	*cmdctx	/* in:  proc ctx for TUTOR cmd line	*/

 );

/* TCL commands defined in this file.
 */

#define Y_TUTOR	  Y_PRIM|Y_PROC|Y_BODY|Y_CLEAR|Y_CMD|Y_ASYNC|Y_INTER
    GLOBAL struct ITRCMD tutorcmd[] = 	/* tutor commands			*/
	{
{1, "TUTOR", "$TUTOR",	  Y_TUTOR|Y_DEFSUB,
					I_NPM(pttutr),	pttutr, tutor_do },
{1, "TUTOR", "SCREEN",    Y_TUTOR,	I_NPM(pttutr),  pttutr, tutor_do },
{1, "TUTOR", "NOSCREEN",  Y_TUTOR,	I_NPM(pttutr),  pttutr, tutor_do },

{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};


    struct TUTCMD
	{
	TINY		abbchr;		/* min # chars reqd (0=all)		*/
	TEXT		cmdname[NAMESIZ+1];	/* command name			*/
	TINY		numparm;		/* number of parms allowed (-1 means don't enforce)*/
	unsigned	parm_allowed : 1;	/* TRUE if allowed in normal parm tutoring*/
	unsigned	subc_allowed : 1;	/* TRUE if allowed in subcmd tutoring*/
	unsigned	qual_allowed : 1;	/* TRUE if allowed in parm qual tutor space*/
	  CODE		(*tutfunc)(TEXT*, TEXT*, struct CONTXT*, struct SFILE*);		/* command processing function	*/
	};

    static struct TUTCMD	tcmd[] =
	{
	{0, "-",	0, FALSE, TRUE,	FALSE,tutsubeq	},	/* placeholder for all subcmd setting cmds*/
	{0, "parm=",	0, TRUE,  FALSE,TRUE, tutprmeq	},	/* placeholder for general parm setting cmds*/
 	{0, "?",	0, TRUE,  TRUE, TRUE, tutmsghlp	},
	{1, "HELP",	1, TRUE,  TRUE,	TRUE, tuthlp	},
	{1, "PAGE",	1, TRUE,  TRUE, TRUE, tutpage	},
	{0, "",		0, TRUE,  TRUE, TRUE, tutscrol	},
	{1, "RUN",	-1,TRUE,  FALSE,FALSE,tutrun	},
	{1, "EXIT",	0, TRUE,  TRUE,	FALSE,tutexit	},
	{2, "SAVE",	1, TRUE,  FALSE,FALSE,tutsav	},
	{2, "RESTORE",	1, TRUE,  FALSE,FALSE,tutrstr	},
	{1, "LIST",	-1,TRUE,  TRUE, TRUE, tutlist	},
	{1, "DISPLAY",	-1,TRUE,  TRUE, TRUE, tutdisplay},
	{1, "TUTOR",     0,TRUE,  TRUE, TRUE, tutscreen	}, /* to help V1.1 users	*/
	{1, "SCREEN",    0,TRUE,  TRUE, TRUE, tutscreen	}, /* S is SCREEN  */
	{1, "NOSCREEN",  0,TRUE,  TRUE, TRUE,tutnoscreen},
	{2, "SET",      -1,TRUE,  FALSE,FALSE,tutset	},
	{2, "SHOW",	1, TRUE,  FALSE,TRUE, tutshow	},
	{1, "QUALIFY",	1, TRUE,  FALSE,FALSE,tutqual	}, /* for now, no quals on quals*/
	{1, "ACCEPT",	0, FALSE, FALSE,TRUE, tutaccept	},
    	{2, "HOLD",	0, TRUE,  TRUE, FALSE,tuthold	}
	,{3, "SELECT",	-1,TRUE,  FALSE,TRUE, tutselect }
	};

#define	NTCMD	(sizeof(tcmd) / sizeof(struct TUTCMD))

/* cls_tutor - De-activates statics so that tutor can be used again.
 *
 * The pdf is re-positioned to its position when opn_tutor was called.
 * The help file, if separate, is closed.
 */

FUNCTION VOID cls_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile of pdf	*/
    struct CONTXT	*context	/* in/out: proc context	*/

 )
    {
    IMPORT CODE		parmlatch;	/* NOLATCH or RUNLATCH			*/
    TEXT		buf[STRINGSIZ+1];

    if (!tutctx.tut_open)
	return;
    if (tutctx.helpf != NULL && tutctx.helpf != pdf)	/* if open and separate help*/
	{
	f_close(tutctx.helpf, F_KEEP);
	tae_free((GENPTR)tutctx.helpf);	/* deallocate help file block	*/
	}
    if (!(*context).intrinsic)
	{
	f_setpos(pdf, &tutctx.saved_pos); /* restore PDF pos to before tutor*/
	if (!(*context).compiled)
	    f_read(pdf, buf);		/* non-compiled PDF's read past "BODY"*/
	(*context).pdf_line = tutctx.saved_line;
	}
    fretxt(&tutctx.title);
    if (tutctx.screen)
	pos_scroll();
    parmlatch = NOLATCH;
    tutctx.tut_open = FALSE;
    return;
    }

/* opn_tutor - Initializes tutor module so sub_tutor and tutor may be called.
 *
 * If context.intrinsic is set, the the pdf argument is
 * ignored.
 *
 * Between opn_tutor and cls_tutor, the module
 * may not be re-entered. As an integrity check, opn_tutor will hit
 * tmierr if cls_tutor has not been called; this will catch
 * recursion and omitted calls to cls_tutor.
 *
 * opn_tutor saves the current position of the pdf--the
 * position is automatically restored by cls_tutor.
 *
 * If the return is not SUCCESS, opn_tutor has already called
 * tmmsg to send the error message.
 */

FUNCTION CODE opn_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile opened to the pdf  */
    struct CONTXT	*context,	/* in: context as set by opnpdf	    */
    FUNINT		screen_select,	/* in: FORCE_SCREEN, FORCE_NOSCREEN,*/
					/* or NOFORCE			    */
    struct VARIABLE	*vpreface,	/* in: preface strings (or NULL)    */
    FUNINT		dynamic	/* TRUE if dynamic tutor	    */

 )
    {
    IMPORT  struct  VARIABLE	*dyntut_gbl;	/* pointer to $dyntut	*/
    IMPORT  struct  VARIABLE	*tutor_gbl;	/* pointer to $tutor	*/
    IMPORT CODE		  termtype;
    IMPORT BOOL		  full_scr_help; /* TRUE if help is full screen */
    IMPORT BOOL		  help_to_term;  /* TRUE to force help to terminal */

    if (tutctx.tut_open)
	{
	tmierr(700);			/* TM internal error in opn_tutor	*/
	return(FAIL);
	}
    help_to_term = TRUE;		/* output to terminal		*/
    full_scr_help = (termtype == T_CRT);  /* set help mode		*/
    tutctx.vcur = (*context).parmst.link;
    tutctx.index = 0;
    tutctx.start = TRUE;		/* latch on this position	*/
    tutctx.edtcmd = FALSE;
    tutctx.highact = FALSE;		/* no highlighting active	*/
    tutctx.ctx = context;		/* proc context pointer		*/

    tutctx.preface = vpreface;	
    tutctx.dyntut = dynamic;		/* T/F for dynamic/initial tutor	*/
    tutctx.tut_open = TRUE;
    tutctx.prev_hopen = FALSE;
    tutctx.helpf = NULL;		/* no help file found yet		*/
    tutctx.prev_cmd[0] = EOS;
    tutctx.prev_mode = FALSE;
    tutctx.spaceLines = (*context).compress ? 0 : 1;
    tutctx.titleLine = 2 + tutctx.spaceLines ;
    if (!(*context).intrinsic)
	{				/* save PDF position		*/
	if ((*context).compiled)
	    /* we can't guarantee an f_read was performed just prior to
		this so we don't want to save from the SFILE		*/
	    f_movpos(&(*context).prcpos, &tutctx.saved_pos);
	else
	    f_movpos(&(*pdf).posctx, &tutctx.saved_pos);
	tutctx.saved_line = (*context).pdf_line;
	}
    if (screen_select == NOFORCE)		/* use 'natural' mode	*/
        {
	if (termtype != T_CRT)
	    tutctx.screen = FALSE;
	else
	  if ((!tutctx.dyntut && s_equal(SVAL(*tutor_gbl, 0), "SCREEN")) ||
	      (tutctx.dyntut && s_equal(SVAL(*dyntut_gbl, 0), "SCREEN")))
		tutctx.screen = TRUE;
	    else
		tutctx.screen = FALSE;
        }
    else if (screen_select == FORCE_SCREEN)
        tutctx.screen = TRUE;
    else
        tutctx.screen = FALSE;
    initut();				/* init tutor specific variables	*/
    return(SUCCESS);
    }

/* sub_tutor - Perform subcommand display tutoring.
 *
 * The return code is EXIT or SUCCESS.
 *
 * Upon SUCCESS return, the context.subcmd field has been set so that
 * the caller may use pdftab to build a parmst for a tutor call.
 */

FUNCTION CODE sub_tutor 
(
    struct SFILE	*pdf,		/* in/out: sfile of opened pdf	*/
    struct CONTXT	*context	/* in/out: proc context	with...	*/
    					/* subcommand chain built	*/

 )
    {
    CODE		code;

    tutctx.subtut = TRUE;
    tutctx.qualtut = FALSE;
    tutctx.parmname[0] = EOS;			/* only relevant in parm qual display*/
    code = tut_subcmd (context, pdf, &tutctx.helpf);	/* tutor on subcs	*/
    if (code != TUT_TUTPARM)
	return(DO_EXIT);
    tutctx.prev_hopen = TRUE;
    reini_tut();				/* re-init for parm tutoring	*/
    (*context).inbody   = FALSE;
    (*context).proctype = Y_UNKNOWN;
    return(SUCCESS);
    }

/* tutor - perform parameter tutoring.
 *
 * The return codes are EXIT and RUN.   If RUN, it is the caller's
 * responsibility to execute or resume the proc.
 *
 * tutor calls rsetini on the context block!!  (This is a performance
 * feature so that pdftab does not have to set defaults; there
 * is considerable overhead in setting defaults and they are only
 * used by tutor.)
 *
 * The context block, upon entry, contains an initial context.parmst,
 * possibly with some parameters missing.  If the return is RUN,
 * the context.parmst has been updated with the user's values and
 * all parameters have values.
 */

FUNCTION CODE tutor 
(
    struct SFILE	*pdf,		/* in/out: opened pdf		*/
    struct CONTXT	*context	/* in/out: context block	*/

 )
    {
    IMPORT struct VARIABLE *switch_gbl;	/* points to $SWITCH		*/
    IMPORT CODE		run_type;	/* run type of current TM	*/

    struct TUTCMD	*tutcmd;	/* cmd table entry for tutor level cmds*/
    TEXT		header[STRINGSIZ+1];
    TEXT		cmdstr[CMDLINSIZ+1];
    TEXT		cmdprm[STRINGSIZ+1];	/* tutor mode command parameter	*/
    CODE		code;
    BOOL		prev_avail;
    TEXT		fspec[FSPECSIZ+1];
    TEXT		*fsp_pt[1];		/* point to fspec */
    struct VARIABLE	*v;
    IMPORT	struct  VARIABLE *tutoropt_gbl;  /* tutor options global      */
    BOOL        no_tag, no_name, no_library;

    no_tag = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_TAG");
    no_name = search_vector ((TEXT **) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_NAME");
    no_library = search_vector ((TEXT**) (*tutoropt_gbl).v_cvp, 
				    (*tutoropt_gbl).v_count, "NO_LIBRARY");

#ifdef TAE_ASYNC
    if (run_type == ASYNC)			/* get the parms from mother*/
    	{
    	f_spec(&(*context).pdf, fspec);		/* build file spec  */
    	fsp_pt[0] = fspec;				/* point to it	    */
    	v = lookex (&(*context).locst, "_PROC");
    	set_value( v, (GENPTR)fsp_pt, 1);
    	code = get_parms_remote (context, NULL, context, 0);
    	if (code==SUCCESS)
    	    return (DO_RUN);
    	else
    	    return (DO_EXIT);
    	}
#endif
    tutctx.subtut  = FALSE;			/* set up for micro-editor */
    tutctx.qualtut = FALSE;			/* not tutoring on parm quals	*/
    tutctx.parmname[0] = EOS;			/* only relevant in parm qual display*/
    tutctx.vcur = (*context).parmst.link;
    tutctx.start = TRUE;			/* latch at this position  */
    tutctx.ctx = context;

    prev_avail = FALSE;
    if (!tutctx.prev_hopen)
	if (tutohlp(pdf, context, &tutctx.title, &tutctx.helpf) != SUCCESS)
						/* open help file & return title*/
	    return(DO_EXIT);
    tutctx.pageStart = tutctx.titleLine + tutctx.title.numline 
			+ tutctx.spaceLines		/* space after title */
			+ 2 				/* 2 lines of col hdr */
			+ tutctx.spaceLines;		/* optional space     */
    if (tutctx.screen)
	tutctx.crt_messages = TRUE;
    else
	{
	if (tutctx.preface == NULL)		/* preface override?	*/
	    {
	    if (!(*context).intrinsic &&
	        (!tutctx.dyntut || IVAL(*switch_gbl, 0) & SW_DYN_LIB))
		{
	        left_fbld  (   		       /* build left header         */
		   no_library ? "" : (*context).pdf.libr,
		   no_name ? "" : (*context).pdf.name,
		   "",				/* subcommand not used */
		   "proc", 			/* header type */
		   header);
		t_write(header, T_STDCC);	/* display header	*/
		}
	    listpnam(&(*context).parmst);	/* list parm names	*/
	    }
	else
	    list_preface (tutctx.preface);	/* display preface lines    */
	}
    code = rsetini(&(*context).parmst);		/* use currents as initials */
    if (code != SUCCESS)
	return(DO_EXIT);
    while (FOREVER)
	{
	if (tutctx.screen  &&  NULLSTR(tutctx.prev_cmd))
	    if (dispbld(context, &tutctx.title, tutctx.helpf) != SUCCESS)
					/* display a page & do help reads as necessary*/
		return(DO_EXIT);
	if (!NULLSTR(tutctx.held_msg))
	    {
	    tutctx.msg_held = TRUE;		/* force output of held message */
	    tutmsg(tutctx.held_msg, tutctx.held_key, 0, 0, 0, 0, 0);
	    tutctx.held_msg[0] = EOS;
	    }
	if (NULLSTR(tutctx.prev_cmd))
   	    {
	    tutctx.prev_mode = FALSE;
	    tutcmd = gttutcmd(cmdstr, cmdprm, context);		/* get oper's cmd & corresp table entry*/
	    }
	else
	    {
	    tutctx.prev_mode = TRUE;
	    tutcmd = gprevcmd(cmdstr, cmdprm, context);		/* parse cmd returned from help*/
	    }
	if (tutcmd != NULL)
	    {
	    if ( ((*context).asydyntut == FALSE) && 
		 (s_equal ((*tutcmd).cmdname, "HOLD")) )
	        /* can't HOLD if not async job */
		{
		  tutmsg (msg_synho, key_synho, 0, 0, 0, 0, 0);
		}
	    else
		{
	        code = (*(*tutcmd).tutfunc)(cmdstr, cmdprm, context, 
					    tutctx.helpf);
							/* perform command	*/
	        if (code == TUT_EXIT  ||  code == TUT_RUNSUB || 
		    code == TUT_HOLD)
		    {
		    break;
		    }
	        if (code == TUT_PREV)
		    prev_avail = TRUE;
	        }
	    }
	if (prev_avail)
	    prev_avail = FALSE;
	else
	    tutctx.prev_cmd[0] = EOS;		/* cmd retnd from help gets executed once before display*/
	}
    if (code == TUT_RUNSUB)
	{					/* log dynamic parameters  */
#ifdef SESSION_LOG
	if (tutctx.dyntut) slparm ("DP: ", &(*context).parmst);
#endif
	return(DO_RUN);
	}
    else if (code == TUT_HOLD)
    	return (DO_HOLD);    	
    else
	return(DO_EXIT);
    }

/*
 *	tutor_do - perform TUTOR command.
 *	Assume no dynamic tutoring on intrinsics.
 *
 *      In order to force TM-wide help functions to remain hardcopy while in
 * 	hardcopy tutor (even though termtype may be T_CRT), tutor_do
 *	temporarily resets termtype to T_NOTCRT if hardcopy tutor.
 */

FUNCTION CODE tutor_do 
(
    struct CONTXT	*procctx,	/* in:  containing proc context		*/
    struct CONTXT	*cmdctx	/* in:  proc ctx for TUTOR cmd line	*/

 )
    {

    IMPORT CODE		  parmlatch;	/* NOLATCH or RUNLATCH			*/
    IMPORT struct SFILE	  prcfil;	/* proc file context			*/
    IMPORT struct CONTXT  latchctx;	/* parm latching proc context		*/
    IMPORT struct CONTXT  *curproc;	/* PDF containing tutor cmd		*/
    IMPORT CODE		  menu_screen;	/* flag to menu: pause before screen clear?*/
    IMPORT CODE		  prim_act;	/* TUTOR_ACTIVATION or CMD_ACTIVATION	*/
    IMPORT struct ECB	  ecbi;		/* event ctl block for operator attn	*/


    TEXT		cmdstr[CMDLINSIZ+1];
    TEXT		*subcmd;
    CODE		code;
    CODE		screen_select;
    struct ITRCMD	*itrcmd;
    struct SFILE	*pdfptr;
    struct SUBCMD	*s;


    pdfptr = NULL;			/* assume no cleanup required */
    if ((*procctx).prclevel == 0)	/* if called from primary level		*/
	prim_act = TUTOR_ACTIVATION;	/* vs CMD_ACTIVATION of proc		*/
    tutctx.saved_proc = procctx;
    tutctx.held_msg[0] = EOS;		/* reinitialize				*/
    tutctx.held_key[0] = EOS;
    cmdstr[0] = EOS;
    subcmd = (*cmdctx).subcmd;
    if (s_equal(subcmd, "$TUTOR"))
        screen_select = NOFORCE;
    else if (s_equal(subcmd, "SCREEN"))
        screen_select = FORCE_SCREEN;
    else
        screen_select = FORCE_NOSCREEN;
    s_copy((*(*cmdctx).sb).curchr, cmdstr);	/* get command line for tutor		*/
    clsctx(cmdctx);			/* close command context for re-use	*/
    if ((*procctx).prclevel > 0)
	    clssav(procctx);			/* allow use of prcfil */
    if (!NULLSTR(cmdstr))			/* if PROC name  was present		*/
	{
	menu_screen = PROMPT_PAINT;		/* prompt before painting menu 	*/
        clr_latch (procctx);			/* clear latch if necessary	*/
	code = inictx(cmdctx);
	if (code != SUCCESS)
	    goto cleanup; 
	(*cmdctx).prclevel = (*curproc).prclevel + 1;
	(*cmdctx).backlink = curproc;
	code = update_ctx(procctx, cmdctx, cmdstr, screen_select);
						/* update ctx with cmd line values	*/
	}
    else					/* PROC parm not present*/
	{
	if (parmlatch != RUNLATCH)	
	    {
	    tmmsg(PROCFAIL,
		"TUTOR requires a proc name unless parameters are latched.",
		  "TAE-TNOLATCH", 0, 0, 0, 0, 0);
	    goto cleanup;
	    }
	parmlatch = NOLATCH;
	MOVE_STRUCT(latchctx, *cmdctx);
	code = opn_tutor(&prcfil, cmdctx, screen_select, NULL, FALSE);
	}
    if (code != SUCCESS)
        goto cleanup; 
    if (!NULLSTR((*cmdctx).subcmd))
	{
	s = allsub(&(*cmdctx).subptr);
	if (s == NULL)
	    {
	      tmmsg(PROCFAIL, msg_mmov, key_mmov, 0, 0, 0, 0, 0);
	    goto cleanup;
	    }
	s_copy((*cmdctx).subcmd, (*s).name);
	}
    pdfptr = (*cmdctx).intrinsic ? NULL : &prcfil;	/* no pdf if intrinsic	*/
    (*cmdctx).asydyntut = FALSE;			/* not tutor for async job */
    if ((*cmdctx).selftutor && runtype (&(*cmdctx).qualst) == INTER)
	{
	struct VARIABLE *v = lookex (&(*cmdctx).locst, "_TUTOR");
	IVAL(*v, 0) = TRUE;				/* set flag for proc */
	code = DO_RUN;					/* by-pass tutor     */
	}
    else
        code = tutor(pdfptr, cmdctx);			/* normal tutor      */	
    cls_tutor(pdfptr, cmdctx);				/* clean up for recursion*/
    							/* & posit file to body*/
    if ((code == DO_RUN) && ((*cmdctx).intrinsic))
	{
	t_attn(&ecbi);					/* enable operator attention	*/
	itrcmd = intrin((*cmdctx).pdf.name);
	itrcmd = itrsub((*cmdctx).subcmd, itrcmd);
	code = (*(*itrcmd).cproc)(tutctx.saved_proc, cmdctx);
							/* call "do" function	*/
	}
    else if (code == DO_RUN)
    	{
    	code = runtype (&(*cmdctx).qualst);		/* get runtype			*/
    	if (code == BATCH)
    	    code = run_batch (procctx, cmdctx);
	else if (code == INTER)
 	    {
   	    if (set_stdout(procctx, cmdctx) == SUCCESS)	/* set new STDOUT	*/
    	 	{
	        deltab(&(*cmdctx).qualst);		/* delete qualst to avoid nesting */
	        code = run_proc(cmdctx);		/* re-delete by clsctx won't hurt	*/
	        }
	    }
#ifdef TAE_ASYNC
    	else if (code == ASYNC || code == ASYNC_PROCESS)
    	    code = run_async (procctx, cmdctx);
#endif
	}

cleanup:
    if (pdfptr != NULL)				/* if assigned to prcfil */
	f_close(pdfptr, F_KEEP);			
    if ((*procctx).prclevel > 0)
	opnsav(procctx);
    return(DO_CHECK);
    }

/* gettpnam - get parm name and subscript from a tutor mode parm= command.
 */

FUNCTION CODE gettpnam 
(
    struct SYNBLK	*sb,		/* in/out: syntax block			*/
    TEXT		name[NAMESIZ+1],/* out: parm name			*/
    COUNT		*subscr	/* out: subscript on parm		*/

 )
    {
    TEXT		token[TOKESIZ+1];
    TEXT		*value[1];
    COUNT		valcnt;
    CODE		code;
    TAEINT		num;
    char		*pos;

    if ((code = gettok(sb, token)) == S_WHITE)	/* get a token			*/
	 code = gettok(sb, token);
    if (code != S_ALPHA)
	goto alph_err;
    if (s_length(token) > NAMESIZ)
	goto len_err;
    s_copy(token, name);

    pos = (*sb).curchr;				/* save position		*/
    if ((code = gettok(sb, token)) == S_WHITE)	/* get a token			*/
	 code = gettok(sb, token);
    if (code == '(')
	{
	(*sb).curchr = pos;
	if ((code = getval(sb, value, 1, &valcnt)) == S_SYNERR) goto nosub_err;
	if (valcnt <= 0)
	    goto nosub_err;
	if (s_s2i(value[0], &num) != SUCCESS) goto nonint_err;
	if (num <= 0)
	    goto zsub_err;
	*subscr = num;
	s_free(value[0]);
	}
    else
	{
	*subscr = 0;
	(*sb).curchr = pos;
	}
    if ((code = gettok(sb, token)) == S_WHITE)	/* eat the "="			*/
	 code = gettok(sb, token);
    if (code != '=')
	goto noeq_err;
    return(SUCCESS);

alph_err:
len_err:
    tutmsg(msg_unre, key_unre, (uintptr_t) token, 0, 0, 0, 0);		/* unrecognized parameter	*/
    return(FAIL);

zsub_err:
    s_free(value[0]);
nosub_err:
    tutmsg("Subscript must be a positive integer.", "TAE-TNISUBSC", 0, 0, 0, 0, 0);
    return(FAIL);

nonint_err:
    s_free(value[0]);
    tutmsg(msg_sbin, key_sbin, 0, 0, 0, 0, 0);
    return(FAIL);

noeq_err:
    tutmsg("Missing \"=\" in parameter assignement.", "TAE-TMEQASS", 0, 0, 0, 0, 0);
    return(FAIL);
    }

/* gettsnam - get subcommand name from a tutor mode "-subcommand" command
 * (returns null string for name if default requested).
 */

FUNCTION CODE gettsnam 
(
    struct SYNBLK	*sb,			/* in/out: syntax block		*/
    TEXT		name[SUBCMDSIZ+1]	/* out: subcommand name		*/

 )
    {
    TEXT		token[TOKESIZ+1];
    CODE		code;

    if ((code = gettok(sb, token)) == S_WHITE)	/* skip leading "-"		*/
	 code = gettok(sb, token);
    if ((code = gettok(sb, token)) == S_WHITE)	/* get a token			*/
	 code = gettok(sb, token);
    if (code == EOS)
	name[0] = EOS;
    else if (code == S_ALPHA)
	{
	if (s_length(token) > SUBCMDSIZ)
	    goto len_err;
	s_copy(token, name);
	if ((code = gettok(sb, token)) == S_WHITE)	/* get a token		*/
	     code = gettok(sb, token);
	if (code != EOS) goto trail_err;
	}
    else
	goto subc_err;
    return(SUCCESS);

len_err:
    tutmsg(msg_unrs, key_unrs, (uintptr_t) token, 0, 0, 0, 0);
    return(FAIL);

subc_err:
    tutmsg("Incorrect format for subcommand name.", "TAE-SUBCFMT", 0, 0, 0, 0, 0);
    return(FAIL);

trail_err:
    tutmsg("Unexpected characters at end of command line.", "TAE-TUNEXCH", 0, 0, 0, 0, 0);
    return(FAIL);
    }

/* gettvnam - get variable name list for NOSCREEN tutor LIST or DISPLAY command.
 * The list must be enclosed in parentheses if there is more than one member.
 */

FUNCTION CODE gettvnam 
(
    TEXT		cmdstr[],	/* in:  command string incl verb	*/
    TEXT		*varname[MAXVAL],	/* out: variable name list	*/
    COUNT		*numvar	/* out: number of variable names in list*/

 )
    {
    struct SYNBLK	sb;		/* syntax block				*/
    CODE		code;
    TEXT		token[TOKESIZ+1];

    initok(&sb, cmdstr);
    getvrb(&sb, token);
    token[0] = EOS;
    code = getval(&sb, varname, MAXVAL, numvar);
    if (code != SUCCESS || *numvar == 0)	/* if error, or -- syntax */
	{
	tutmsg(sb.errmsg, "TAE-INVPVAL", 0, 0, 0, 0, 0);
 	return(FAIL);
	}
    if (*numvar == -1)				/* if nothing, tell caller */
	*numvar = 0;	
    if ((code = gettok(&sb, token)) == S_WHITE)
	code = gettok(&sb, token);
    if (code != EOS)
	{
	tutmsg("DISPLAY, LIST accept one name or parenthesized list of names.", "TAE-TTOOVAL", 0, 0, 0, 0, 0);
	return(FAIL);
	}
    return(SUCCESS);
    }

/* gprevcmd - get (parse) prev mode command, i.e., command returned from help.
 */

FUNCTION static struct TUTCMD *gprevcmd 
(
    TEXT		cmdstr[],	/* out: command string			*/
    TEXT		cmdprm[],	/* out: command parameter (if not parm=)*/
    struct CONTXT	*pctx		/* in:  proc context			*/

 )
    {
    struct TUTCMD	*cmd;
    COUNT		maxparm;
    CODE		code;
    TEXT		errmsg[STRINGSIZ+1];
    TEXT		errkey[KEYSIZ+1];

    s_copy(tutctx.prev_cmd, cmdstr);
    if ((cmd = srchtab(cmdstr)) == NULL)	/* look up command in table	*/
	{
	if (tutctx.subtut)
	    tutmsg(msg_tsub, key_tsub, 0, 0, 0, 0, 0);		/* Unrec cmd for subc displ	*/
	else if (tutctx.qualtut)
	    tutmsg(msg_tqual, key_tqual, 0, 0, 0, 0, 0);	/* Unrec cmd for parm qual displ*/
	else
	    tutmsg(msg_tcmd, key_tcmd, 0, 0, 0, 0, 0);		/* Unrec cmd			*/
	}
    else
	{
	if (!s_equal((*cmd).cmdname, "parm=")  &&
	    !s_equal((*cmd).cmdname, "-"))
	    {
	    maxparm = (*cmd).numparm;			/* max allowed parms	*/
	    code = intprm(maxparm, (*cmd).cmdname, cmdstr, cmdprm,
			errmsg, errkey);		/* get cmd parameter 	*/
	    if (code != SUCCESS)
		{
		tutmsg(errmsg, errkey, 0, 0, 0, 0, 0);
		cmd = NULL;
		}
	    }
	}
    return(cmd);
    }


/* gttutcmd - get operator's command and corresponding tutor command
 * table entry.
 * Returns the table entry.
 */

FUNCTION static struct TUTCMD *gttutcmd 
(
    TEXT		cmdstr[CMDLINSIZ+1], /* out: command string		*/
    TEXT		cmdprm[],	/* out: command parameter (if not parm=)*/
    struct CONTXT	*pctx		/* in:  proc context			*/

 )
    {
    IMPORT COUNT	   termlines;	/* number lines on terminal screen	*/
    IMPORT struct CONTXT   primctx;	/* primary context for substitution */
    IMPORT struct VARIABLE *prompt_gbl;	/* $PROMPT TAE global variable	    */

    struct TUTCMD	*cmd;
    COUNT		pmtline;	/* line number for prompt ? display */
    COUNT		errline;	/* error message line number */
    TEXT		pmtbuf[STRINGSIZ+1];
    COUNT		maxparm;
    CODE      		terminat;	/* command line terminator */
    CODE		code;
    TEXT		errmsg[STRINGSIZ+1];
    TEXT		errkey[KEYSIZ+1];

    pmtline = termlines - PMTLIN;
    errline = termlines - ERRLIN;
    while (FOREVER)
	{
	if (!tutctx.screen)
	    {
	    s_copy(SVAL(*prompt_gbl, 0), pmtbuf); /* current prompt */
	    s_append ("-", pmtbuf);
	    s_append ((*pctx).pdf.name, pmtbuf);
	    if (tutctx.subtut)
		s_append ("-SUBCOMMAND>", pmtbuf);
	    else
		s_append (">", pmtbuf);
	    cmd_noscreen (A_NONE, pmtbuf, cmdstr);
	    code = SUCCESS;
	    }
	else					/* formatted screen tutor	*/
	  {

/* Check if we are in window mode or VT100 mode. Write ther following 
 * prompt only in VT100 mode.
 */
	  if (tutctx.subtut)
		wrttxt(pmtline-1, 1,
"Enter:  -subcommand, -, HELP, PAGE, or EXIT; RETURN to page.", FALSE);
	  else if (tutctx.qualtut)
		wrttxt(pmtline-1, 1,
"Enter: qual=value, HELP, PAGE, SELECT, SHOW, ACCEPT; RETURN to page.",
		     FALSE);
	  else if ((*pctx).parmst.link != NULL)
		wrttxt(pmtline-1, 1,
"Enter: parm=value,HELP,PAGE,SELECT,SHOW,RUN,EXIT,SAVE,RESTORE; RETURN to page.",
		     FALSE);
	  else
		wrttxt(pmtline-1, 1, "Enter:  HELP, RUN, or EXIT.", FALSE);
	  code = cmd_screen (A_TUTOR, cmdstr, &terminat);	

	  if (terminat == T_ESCAPE)
		continue;			/* ignore if ESCAPE */
	  }
	if (code == SUCCESS)			/* command received */
	    {
	      code = substitute (cmdstr, CMDLINSIZ, (GENPTR) &primctx, FALSE);
	    if (code != SUCCESS)
	 	{
	        tutmsg ("Error using '&' for string substitution.",
			"TAE-TUTSUB", 0, 0, 0, 0, 0);
		continue;
	        }
	    if ((cmd = srchtab(cmdstr)) == NULL)	/* look up command in table	*/
	        {
		if (tutctx.subtut)
		    tutmsg(msg_tsub, key_tsub, 0, 0, 0, 0, 0);		/* Unrec cmd for subc displ	*/
		else if (tutctx.qualtut)
		    tutmsg(msg_tqual, key_tqual, 0, 0, 0, 0, 0);	/* Unrec cmd for parm qual displ*/
		else
		    tutmsg(msg_tcmd, key_tcmd, 0, 0, 0, 0, 0);		/* Unrec cmd			*/
	        continue;
	        }
	    else
	        {
	 	if (s_equal((*cmd).cmdname, "parm="))
		    break;
		if (s_equal((*cmd).cmdname, "-"))
		    break;
	        maxparm = (*cmd).numparm;	/* max allowed parms	*/
 		if (intprm(maxparm, (*cmd).cmdname, cmdstr, cmdprm,
			errmsg, errkey) == SUCCESS)
		    {
		    if (tutctx.screen)
		        t_lclear(errline, 1);
		    break;
		    }
		else
		    tutmsg(errmsg, errkey, 0, 0, 0, 0, 0);	/* write error msg from intprm*/
	        }
	    }
	}
    return(cmd);
    }

/*	hold_msg.   Save message and key in held_msg, held_key.
 */         

FUNCTION VOID hold_msg 
(
    TEXT	msg[],		/* in: control string		*/
    TEXT	key[],		/* in: message key		*/
    uintptr_t	a1,		/* in: edit parameters		*/
    uintptr_t a2,
    uintptr_t a3,
    uintptr_t a4,
    uintptr_t a5

 )
    {
    sprintf (tutctx.held_msg, msg, a1, a2, a3, a4, a5);
    s_copy (key, tutctx.held_key);
    return;
    }

/* initut - initialize tutor specific variables (static C globals).
 */

    FUNCTION static VOID initut (void)

    {
    tutctx.crt_messages  = FALSE;	/* formatted screen messages	*/
    tutctx.curpag   = 1;		/* always start at page 1	*/
    tutctx.lastpag  = 0;
    tutctx.dispreq  = TRUE;		/* display of page 1 required	*/
    tutctx.hlpexist = FALSE;
    tutctx.hlp_searched = FALSE;
    tutctx.h1exist  = FALSE;
    tutctx.h2exist  = FALSE;
    tutctx.h1done   = FALSE;
    tutctx.nohelpf  = FALSE;
    tutctx.srch1cplt = FALSE;
    tutctx.srch2cplt = FALSE;
    tutctx.msg_held  = FALSE;
    initxt(&tutctx.title);		/* initialize title text storage block	*/
    return;
    }

/*
 * 	prep_intrin. Prepare parameter symbol table for an intrinsic.
 */

FUNCTION  static  CODE  prep_intrin
(

    struct	CONTXT	*procctx,		/* in: proc context	     */
    struct  	CONTXT	*cmdctx,		/* in: command context	     */
    struct	ITRCMD	*itrcmd_1st,		/* 1st ITRCMD with this name */
    CODE		screen_select		/* in: screen selection	     */

    )
    {

    BOOL		dash_present;	/* TRUE if "-" present after cmd name	*/
 					/* in tutor cmd line			*/
    struct  	ITRCMD	*itrcmd;
    struct	SFILE	*dummy_pdf;
    struct	SYNBLK	dummy_sb;
    TEXT		blank_str[CMDLINSIZ+1];
    CODE		code;
    CODE		qcode;

    (*cmdctx).intrinsic = TRUE;
    dash_present = !NULLSTR((*cmdctx).subcmd) || (*cmdctx).subcmd[1] == '-';
    dummy_pdf = NULL;
    blank_str[0] = EOS;
    initok(&dummy_sb, blank_str);
    s_copy((*itrcmd_1st).cmd, (*cmdctx).pdf.name);
    opn_tutor(dummy_pdf, cmdctx, screen_select, NULL, FALSE);
    itrcmd  = NULL;
    if (!dash_present  ||  !NULLSTR((*cmdctx).subcmd))
	{
	itrcmd = itrsub((*cmdctx).subcmd, itrcmd_1st);	/* find entry for this subc*/
	if (itrcmd == NULL  &&  !NULLSTR((*cmdctx).subcmd))
	    {
	    tmmsg(PROCFAIL, "Undefined or ambiguous subcommand '%s'.",
		  "TAE-UNDEFSUB", (uintptr_t) (*cmdctx).subcmd,
		  0, 0, 0, 0);
            return (FAIL);
	    }
	}
    if (NULLSTR((*cmdctx).subcmd)  &&  itrcmd == NULL)
	{
	code = tut_itrsub(cmdctx, itrcmd_1st, &itrcmd, &tutctx.helpf); /* tutor on the subc*/
	if (code == DO_EXIT)
	    return(DO_CHECK);
	}
    s_copy((*itrcmd).subcmd, (*cmdctx).subcmd);		/* get full subc name*/
    if ((*itrcmd).flags & Y_PDF)
        {						/* 'PDF intrinsic' */
	cls_tutor (dummy_pdf, cmdctx);
    	qcode = setqlf(FALSE, &dummy_sb, &(*cmdctx).qualst);     /* get qualifiers	*/
	code = prep_pdf (procctx, cmdctx, (*cmdctx).pdf.name, screen_select);
	}
    else
        {
	if ((*itrcmd).flags & Y_PROCSYN)
	    code = memtab((*itrcmd).partab, (*itrcmd).numprm, &(*cmdctx).parmst);
        else
	    {
	    tmmsg(PROCFAIL, "TUTOR not available for this TCL command.",
	        "TAE-TNPRCSYN", 0, 0, 0, 0, 0);
    	    code = FAIL;
	    }
        }
    return (code);
    }

/*
 *	prep_pdf. Prepare the parameter symbol table from pdf (for a proc)
 */

FUNCTION  static  CODE  prep_pdf
(
    struct  	CONTXT	*procctx,		/* in: proc context	     */
    struct  	CONTXT	*cmdctx,		/* in: command context	     */
    TEXT		procnm[],		/* in: name of proc to tutor */
    CODE		screen_select		/* in: screen selection	     */

 )
    {
    IMPORT struct SFILE prcfil;

    BOOL		dash_present;	/* TRUE if "-" present after cmd name	*/
					/* in tutor cmd line			*/
    TEXT		clin_sub[SUBCMDSIZ+1];	/* subcmd from cmd line		*/
    CODE		code;

    (*cmdctx).intrinsic = FALSE;
    dash_present = !NULLSTR((*cmdctx).subcmd) || (*cmdctx).subcmd[1] == '-';
    code = opnpdf(procctx, cmdctx, procnm, &prcfil);	/* open the PDF to tutor on*/
    if (code != SUCCESS)
	goto opnp_err;
    code = plcini(cmdctx);			/* init implicit locals	*/
    if (code != SUCCESS)
	goto locini_err;
    s_copy((*cmdctx).subcmd, clin_sub);		/* save subcmd as typed	*/
    if (dash_present  &&  NULLSTR(clin_sub))
	(*cmdctx).special = SUB_SEARCH;		/* pdftab will just build subcmd chain*/
    else if (!NULLSTR(clin_sub))
	(*cmdctx).special = NOT_SPECIAL;	/* subcmd already specd	*/
    else
	(*cmdctx).special = TUTOR_ATTEMPT;	/* we'll try to get parms*/
    code = pdftab(cmdctx, &prcfil);
    if (code != SUCCESS)
	goto pdf_err;
    code = opn_tutor(&prcfil, cmdctx, screen_select, NULL, FALSE);
    if (code != SUCCESS)
	goto opntut_err;
    if ((NULLSTR(clin_sub) && dash_present)  ||	/* if subc disp reqd	*/
	(*cmdctx).special == SUB_SEARCH)
	{
	code = sub_tutor(&prcfil, cmdctx);	/* tutor subc display	*/
	if (code == DO_EXIT)
	    {
	    code = DO_CHECK;
	    goto cleanup;
	    }
	reini_ctx(cmdctx);			/* reinit sym tabs & refs*/
	(*cmdctx).special  = NOT_SPECIAL;
	(*cmdctx).pdf_line = 0;
	f_rewind(&prcfil);
	code = pdftab(cmdctx, &prcfil);		/* now build parm symbol tab*/
	if (code != SUCCESS)
	    goto pdf_err;
	}
    if (action_setup (cmdctx) != SUCCESS) 	/* honor ACTION= for parmsets */
	goto pdf_err;
    return (SUCCESS);

pdf_err:
locini_err:
opntut_err:
    code = FAIL;

cleanup:
    f_close(&prcfil, F_KEEP);
    if ((*procctx).prclevel > 0)
	opnsav(procctx);
    return (code);		/* fail or DO_EXIT		*/

opnp_err:
    return (FAIL);
    }

/*
 *	q_get_exec - get and execute a parameter qualifier tutor display
 *	command.
 */

FUNCTION CODE q_get_exec 
(
    struct CONTXT	*ctx			/* in/out: child proc context	*/

 )
    {
    TEXT		q_cmdstr[CMDLINSIZ+1];
    TEXT		q_cmdprm[STRINGSIZ+1];
    struct TUTCMD	*tutcmd;
    CODE		code;

    if (NULLSTR(tutctx.prev_cmd))
	{
	tutctx.prev_mode = FALSE;
	tutcmd = gttutcmd (q_cmdstr, q_cmdprm, ctx); /* get oper's cmd & corresp table entry*/
	}
    else
	{
	tutctx.prev_mode = TRUE;
	tutcmd = gprevcmd (q_cmdstr, q_cmdprm, ctx); /* parse cmd returned from help*/
	}
    if (tutcmd != NULL)
	code = (*(*tutcmd).tutfunc)(q_cmdstr, q_cmdprm, ctx, tutctx.helpf);
    							 /* perform command*/
    else
	{
	code = FAIL;
	tutctx.prev_cmd[0] = EOS;		/* clear bad command	*/
	}
    return (code);
    }

/* reini_ctx - reinitialize (actually reset) proc context.
 * Deletes local & parm symbol tables, global refs, and SUBCMD table,
 * then reinstalls the implicit locals & globals.
 * Assumes proc level > 0.
 * Must be coordinated with inictx.
 */

FUNCTION CODE reini_ctx 
(
    struct CONTXT	*ctx		/* in/out: proc context to reset	*/

 )
    {
    IMPORT struct SYMTAB glbtab;	/* global symbol table			*/

    COUNT		i;
    struct VARIABLE	*v;


    deltab(&(*ctx).locst);		/* delete local symbol table		*/
    deltab(&(*ctx).parmst);		/* delete parm symbol table		*/
    for (i = 0; i < (*ctx).numrefs; i++)
	{
	v = (*ctx).refs[i];
	(*v).v_refcnt--;
	(*ctx).refs[i] = NULL;
	}
    (*ctx).numrefs     = 0;
    (*ctx).locst.link  = NULL;
    (*ctx).parmst.link = NULL;
    delsub(&(*ctx).subptr);		/* delete SUBCMD table		    */
    (*ctx).subptr = NULL;
    for (v=glbtab.link, i=0; v != NULL; v=(*v).v_link)
	{
	if ((*v).v_implicit)
	    {
	    (*ctx).refs[i] = v;		/* ref the implicits	*/
	    i++;
	    }
	}
    (*ctx).numrefs = i;
    (*ctx).int_procs = NULL;		/* no internal procs */
    refs_ins(ctx);			/* installation exit for other refs	*/
    (*ctx).ctx_ins = NULL;		/* installation courtesy		*/
    if (plcini(ctx) != SUCCESS)		/* init proc specif implicit locals */
	return(FAIL);
    return(SUCCESS);
    }

/* reini_tut - re-initialize certain tutor-wide variables for the parm phase
 * (after subcommand phase).
 */

    FUNCTION static VOID reini_tut (void)

    {
    tutctx.curpag   	= 1;
    tutctx.lastpag	= 0;
    tutctx.dispreq	= TRUE;
    tutctx.h2exist	= FALSE;
    tutctx.h1done	= FALSE;
    tutctx.srch1cplt	= FALSE;
    tutctx.srch2cplt	= FALSE;
    return;
    }

/*	srchtab - search table of tutor mode commands & return pointer to the
 *	table entry.
 *
 *	Returns NULL if the search fails.
 */

FUNCTION static struct TUTCMD *srchtab 
(
    TEXT		cmdstr[]	/* in:  command string			*/

 )
    {
    struct TUTCMD	*tutcmd;
    struct SYNBLK	sb;
    CODE		code;
    TEXT		verb[TOKESIZ+1], token[TOKESIZ+1];
    COUNT		i;
    TEXT		*pos;

    initok(&sb, cmdstr);
    if ((code=gettok(&sb, token)) == S_WHITE)	/* get first non-white	*/
	code = gettok(&sb, token);
    if ((code=gettok(&sb, token)) == S_WHITE)	/* get second non-white	*/
	code = gettok(&sb, token);
    if (code == '='     ||				/* parm =	*/
	(code == '('  &&  s_index (sb.curchr, '=') >= 0))	/* parm(i) =	*/ {
	if (tutctx.subtut)
	    return(NULL);
	else
	    {
	    tutcmd = &(tcmd[1]);		/* assume "parm = "  	*/
	    return(tutcmd);
	    }
    }
    initok(&sb, cmdstr);			/* re-init syntax block	*/
    if (NULLSTR(cmdstr))
	verb[0] = EOS;
    else
	{
	pos = sb.curchr;
	if (gettok(&sb, verb) == S_WHITE)	/* look ahead for leading "-"	*/
	    gettok(&sb, verb);
	if (s_equal("-", verb))
	    {
	    if (tutctx.subtut)
		{
		tutcmd = &(tcmd[0]);
		return(tutcmd);
		}
	    else
		return(NULL);
	    }
	sb.curchr = pos;			/* restore posit in cmd stream	*/
	code = getvrb(&sb, verb);		/* get the verb			*/
	if (!s_equal(verb, "?") && code == S_SYNERR) 	/* "?" is a valid cmd	*/
	    return(NULL);
	}
    for (i = 0; i < NTCMD; i++)
	{
	tutcmd = &(tcmd[i]);
	if (tutctx.qualtut)		/* if parm qualifier tutor displ space	*/
	    {
	    if (!(*tutcmd).qual_allowed)
		continue;
	    }
	else				/* not parm qualifier tutor displ space	*/
	    {
	    if (tutctx.subtut  &&  !(*tutcmd).subc_allowed)
		continue;
	    else if (!tutctx.subtut  &&  !(*tutcmd).parm_allowed)
		continue;
	    }
	if ((*tutcmd).abbchr == 0)	/* if no abbr allowed			*/
	    {
	    if (s_equal((*tutcmd).cmdname, verb))
		break;
	    }
	else
	    {
	    if (s_length(verb) >= (*tutcmd).abbchr)
		if (s_lseq(verb, (*tutcmd).cmdname))
		    break;
	    }
	}
    if (i == NTCMD)				/* if search failed  */
	return(NULL);
    return(tutcmd);
    }

/* tut_itrsub - tutor on intrinsic subcommands.
 */

FUNCTION static CODE tut_itrsub 
(
    struct CONTXT	*ctx,		/* in/out: proc context			*/
    struct ITRCMD	*itrcmd_1st,	/* in:  1st ITRCMD entry for this cmd name*/
    struct ITRCMD	**itrcmd,	/* out: the actual ITRCMD for this cmd-subc*/
    struct SFILE	**hf		/* out: the help file			*/

 )
    {
    struct ITRCMD	*p;
    struct SUBCMD	*s;
    CODE		code;
    struct SFILE	*dummy;

    p = itrcmd_1st;
    do						/* build SUBCMD table		*/
	{
	s = allsub(&(*ctx).subptr);
	if (s == NULL)
	    {
	    tmmsg(PROCFAIL, msg_mmov, key_mmov, 0, 0, 0, 0, 0);
	    return(FAIL);
	    }
	s_copy((*p).subcmd, (*s).name);
	(*s).deflt = ((*p).flags & Y_DEFSUB)  ?  TRUE : FALSE;
	p++;
	}
    while (s_equal((*itrcmd_1st).cmd, (*p).cmd));
    dummy = NULL;
    code = sub_tutor(dummy, ctx);		/* tutor for the subcommand	*/
    if (code == SUCCESS)
	*itrcmd = itrsub((*ctx).subcmd, itrcmd_1st);
    return(code);
    }

/* tut_subcmd - tutor for subcommand if necessary.
 * Only called if no subcommand specified.
 * Assumes TUTOR command proc name had no explicit subcommand.
 */

FUNCTION static CODE tut_subcmd 
(
    struct CONTXT	*ctx,		/* in/out: proc context			*/
    struct SFILE	*pf,		/* in/out: proc file			*/
    struct SFILE	**hf		/* out:  help file			*/

 )
    {
    IMPORT COUNT	termlines;	/* number lines on terminal screen	*/
    IMPORT CODE		parmlatch;	/* NOLATCH or RUNLATCH			*/

    CODE		code;
    CODE		cplcode;	/* cmd processing function compl code	*/
    TEXT		cmdstr[CMDLINSIZ+1];
    TEXT		cmdprm[STRINGSIZ+1];
    struct TUTCMD	*tutcmd;
    BOOL		prev_avail;	/* TRUE if a prev mode cmd needs to be executed*/

    prev_avail = FALSE;
    (*ctx).subcmd[0] = EOS;	
    code = tutohlp(pf, ctx, &tutctx.title, hf);	/* open help file		*/
    if (code != SUCCESS) goto opnh_error;
    tutctx.pageStart = tutctx.titleLine + tutctx.title.numline 
			+ tutctx.spaceLines		/* space after title */
			+ 2 				/* 2 lines of col hdr */
			+ tutctx.spaceLines;		/* optional space     */
    if ((*ctx).subptr == NULL)		/* if PDF had no SUBCMDs...		*/
	return(TUT_TUTPARM);		/* then no need to tutor on subcommand	*/
    if (!tutctx.screen)
      listsnam((struct SUBCMD *) &(*ctx).subptr);	/* list the available subcommands	*/
    else
	tutctx.crt_messages = TRUE;	/* screen-formatted messages		*/
    while (FOREVER)
	{
	if (tutctx.screen  &&  NULLSTR(tutctx.prev_cmd))
	    {
	    code = dispbld(ctx, &tutctx.title, *hf);	/* displ a pg, doing help reads as necessary*/
	    if (code != SUCCESS)
		return(DO_CHECK);
	    }
	if (tutctx.msg_held)
	    {
	    tutmsg(tutctx.held_msg, tutctx.held_key, 0, 0, 0, 0, 0);
	    tutctx.held_msg[0] = EOS;
	    }
	if (NULLSTR(tutctx.prev_cmd))
	    {
	    tutctx.prev_mode = FALSE;
	    tutcmd = gttutcmd(cmdstr, cmdprm, ctx);	/* get oper's cmd & corresp table entry*/
	    }
	else
	    {
	    tutctx.prev_mode = TRUE;
	    tutcmd = gprevcmd(cmdstr, cmdprm, ctx);	/* parse cmd returned from help*/
	    }
	if (tutcmd != NULL)
	    {
	    cplcode = (*(*tutcmd).tutfunc)(cmdstr, cmdprm, ctx, *hf);	/* perform operator's command*/
	    if (cplcode == TUT_EXIT  ||  cplcode == TUT_TUTPARM)
		break;
	    if (cplcode == TUT_PREV)
		prev_avail = TRUE;
	    }
	if (prev_avail)
	    prev_avail = FALSE;
	else
	    tutctx.prev_cmd[0] = EOS;		/* cmd retnd from help gets executed once before display*/
	}
    tutctx.prev_cmd[0] = EOS;			/* clear prev mode if we leave loop*/
    (*ctx).subsiz = s_length((*ctx).subcmd);
    f_movpos(&tutctx.lev1start, &tutctx.lasth1pos); /* now start over on .LEVEL1	*/
    if (tutctx.screen)
	pos_scroll();				/* position at the bottom 	*/
    tutctx.crt_messages = FALSE;
    return(cplcode);

opnh_error:
    if (tutctx.screen)
	pos_scroll();				/* position at the bottom 	*/
    parmlatch = NOLATCH;
    return(FAIL);
    }

/*
 *	update_ctx. Update context block from command line values.
 */

FUNCTION  static CODE  update_ctx
(
    struct    	CONTXT	*procctx,	/* in: proc context		*/
    struct	CONTXT	*cmdctx,	/* in: command context		*/
    TEXT		cmdstr[],	/* in: command line text	*/
    CODE		screen_select  /* in: screen selection		*/

 )
    {
    IMPORT struct SFILE	prcfil;		/* proc file context			*/

    CODE		code, qcode;
    struct SYNBLK	sb;
    struct ITRCMD	*itrcmd_1st;	/* 1st ITRCMD with this cmd name	*/
    struct ITRCMD	*itrcmd;	/* ITRCMD entry with correct cmd-subcmd	*/
    TEXT		label[LABELSIZ+1];	/* ignored		*/
    TEXT		cmd[FSPECSIZ+1];

    code = cmd_parse(&sb, cmdstr, label, cmd, (*cmdctx).subcmd);
    (*cmdctx).subsiz = s_length((*cmdctx).subcmd);
    if (code != SUCCESS)
	goto gcmd_err;
    itrcmd_1st = intrin (cmd);			/* find intrinsic */
    if (itrcmd_1st == NULL)
    	{
    	qcode = setqlf(FALSE, &sb, &(*cmdctx).qualst);     /* get qualifiers	*/
	code = prep_pdf(procctx, cmdctx, cmd, screen_select);
    	}
    else
        {
	itrcmd = itrsub ((*cmdctx).subcmd, itrcmd_1st);	/* find subcommand */
	if (itrcmd != NULL && ((*itrcmd).flags & Y_PDF))
    	    {
    	    qcode = setqlf(FALSE, &sb, &(*cmdctx).qualst);     /* get qualifiers	*/
	    code = prep_pdf(procctx, cmdctx, cmd, screen_select);
    	    }
	else
    	    {
    	    qcode = setqlf(TRUE, &sb, &(*cmdctx).qualst);     /* get qualifiers	*/
	    code = prep_intrin(procctx, cmdctx, itrcmd_1st, screen_select);
    	    }
	}
    if (code != SUCCESS)
	goto prep_err;	

    if ((*cmdctx).intrinsic)
	{
	if (qcode != S_NONE)		/* qualifiers not allowed for intrinsics	*/
	    {
	    tmmsg(PROCFAIL, "Command qualifiers not allowed for '%s'.",
		  "TAE-NOQUAL", (uintptr_t) cmd, 0, 0, 0, 0);
	    goto intrin_err;	
	    }
    	if (updtab(&(*cmdctx).parmst, &sb) != SUCCESS)
	    goto intrin_err;
	return (SUCCESS);
	}
    else
	{
	if (qcode == FAIL)
	    goto proc_err;			/* bad qualifier specified 	*/
    	if (restore(cmdctx) != SUCCESS)		/* do RESTORE qualifier		*/
	    goto proc_err;
    	if (updtab(&(*cmdctx).parmst, &sb) != SUCCESS)
	    goto proc_err;
	code = runtype (&(*cmdctx).qualst);		/* get run type		*/
	if (code == FAIL)				/* bad run type		*/
	    {
	    tmmsg (PROCFAIL,
		   "Unrecognized RUNTYPE qualifier.",  "TAE-RUNTYPE",
		   0, 0, 0, 0, 0);
	    goto proc_err;
	    }
	return (SUCCESS);
	}
gcmd_err:
prep_err:
intrin_err:
    cls_tutor(&prcfil, cmdctx);
    return(DO_CHECK);

 proc_err:					/* must close pdf if proc */
    cls_tutor(&prcfil, cmdctx);
    f_close(&prcfil, F_KEEP);
    if ((*procctx).prclevel > 0)
	opnsav(procctx);
    return(DO_CHECK);
    }
        
