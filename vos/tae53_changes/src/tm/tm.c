/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/* TAE monitor (TM) source file.
 * After functions tm and local_tm, the functions in this
 * source file are in alphabetical order.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	15-jul-83	Updated for host-code return from c_init()...dm
 *	18-jul-83	Fix latch bug on substitute error...peb
 *	29-jul-83	Updated to use intrinsic parameter pool(ipp) for
 *			intrinsic  parameter processing...dm
 *	05-aug-83	Cosmetic changes...dm
 *	11-aug-83	Deallocate value for NAME variables in repval()...dm
 *	16-aug-83	Add comment relating prccmd to delgbl_do...peb
 *	20-aug-83	Nullables;
 *			Primary CONTXT made global for substitution...palm
 *	25-aug-83	New returns from updtab.
 *	29-aug-83	Keyword parameters...palm
 *	08-sep-83	PR 481, maximum pdf nesting enforced...palm
 *	12-sep-83	Added $LASTCMD to hold most recent command...palm
 *	12-sep-83	Clean-up clr_latch, clsctx, latching...palm
 *	16-sep-83	Fix clsctx so double call for same ctx okay...palm
 *	23-sep-83	Fix PR 444 on TAE-BADCMD...palm
 *	26-sep-83	Implement de-referencing...palm
 *	03-oct-83	Fix lint-identified errors (f_close)...palm
 *	05-oct-83	Change getint() function name to cmd_noscreen...dm
 *	20-oct-83	Fix PR 560, TAE-BADKEY to TAE-KEYWORD...palm
 *	25-oct-83	Add host_init() call...palm
 *	30-oct-83	Set prim_act in tm primary loop...peb
 *	20-jan-84	Fix [TAE-TOOMANY] bad message when "@x" passed
 *			to proc and x is null...palm
 *	23-jan-84	History_save made external...palm
 *	25-jan-84	Save $LASCMD before prccmd (for nesting problem);
 *			New cmd_noscreen call...palm
 *	03-feb-84	Changes to prccmd() for proc interrupt mode...nhe
 *			changes to chk_do to allow all cmds in int mode
 *	09-feb-84	Move bldcmd processing logic to app_cmd() in the
 *			library, and let bldcmd invoke it...dm
 *	14-feb-84	Don't continue if c_init error...palm
 *	14-feb-84	Change runtyp to run_type and CODE...palm
 *	22-feb-84	Remove "cmdstr" arg from call seq to run_batch...peb
 *	22-feb-84	Async changes...nhe
 *	01-mar-84	Fix memtab to set dvp NULL...palm
 *	05-mar-84	Args to main for UNIX...dm
 *	06-mar-84	Update bldcmd, call flush_cmd in case of overflow...dm
 *	11-mar-84	Fix repval crash for V_NAME parameters...dm
 *	14-mar-84	Further async changes...nhe
 *	15-mar-84	Proper check for BATCH in chk_do
 *	19-mar-84	Update REPLY request message...nhe
 *	27-apr-84	Make value vectors a union in repval...lim
 *	04-may-84	VALUE_x to xVAL, set_string added, addref removed due
 *			to use of run_gbl and sess_gbl...ces
 *	07-may-84	Clean-up the IMPORT declarations...lim
 *			No more dyncmd mode; allow async in proc int mode...nhe
 *			'toprefs'-->'numrefs'...nhe
 *	06-jun-84	Fix repval to allow de-referencing on DEFAULT and
 *			INITIAL...palm
 *	08-jun-84	Update updtab() errmsgs to cover qualifier case
 *	11-jun-84	Make _ONFAIL doubly valued for TCL-21...palm
 *	11-jun-84	New echo scheme for 1.3...palm
 *	14-jun-84	Fix double echo for ASYNC; new RUNTYPE values...palm
 *	14-jun-84	Y_PDF ITRCMD entry support...palm
 *	09-jul-84	Fix stdout name bug for batch (as in UNIX)...dm
 *	06-aug-84	New BATCH/ASYNC strategy...palm
 *	24-aug-84	Alter setting of $TUTOR for 1st entry only...lia
 *	13-sep-84	runtype() changed to default to ASYNC if NODE set...nhe
 *	17-sep-84	Add process for error code V_BADAMBIG...lia
 *	08-oct-84	Handle acbptr in CONTXT (for RCJM)...nhe
 *	10-oct-84	New test for interactiveness...nhe
 *	12-oct-84	Conditional compilation of async features as done for
 *			UNIX 1.2...dm
 *	14-oct-84	Changes for internal procs...nhe
 *	17-oct-84	TCL 117: inictx to init ctx.compiled...peb
 *	17-oct-84	PR 847: inictx to ref implicits via v_implicit...peb
 *	24-oct-84	TCL 117: call_do update for compiling PDFs...peb
 *	31-oct-84	TCL 117: handle de-referencing in compilation...peb
 *	02-nov-84	Add 'remote' flag for RCJM remote job processing...dm
 *	05-nov-84	TCL 67: prccmd -- (*ctx).special=GET_PARM_QUAL...peb
 *	09-nov-84	PR 879: remove bogus prccmd call;
 *			PR 885: echo=TRACE...palm
 *	11-nov-84	runtype(): check for null RUNTYPE cmd qual...nhe
 *	21-nov-84	Add processing for NORUN RUNTYPE cmd qual...nhe
 *	25-nov-84	TCL 67: updtab to set parameter qualifiers...peb
 *	29-nov-84	TUTOR 15: clear saved proc & sub name for HELP...lia
 *	04-dec-84	Implement loopback of remote tm for next job...dm
 *	08-dec-84	PR 898: set $TUTOR back to single element...lia
 *	11-dec-84	TCL 97: initialize new flag for executing onfail
 *			cmd in CONTXT block...lia
 *	18-dec-84	TCL 67: in misprm, enforce missing parm qualifs...peb
 *  	27-dec-84	Updated to loop for next job in 'remote' mode...dm
 *	03-jan-85	Do not abort RCJM agent on pdf processing error...dm
 *
 **************************************************************************
 * CHANGES MADE IN THE RCJM TREE:
 *
 *	17-jan-85	Update to getcln() for 'include' command files...dm
 *	02-feb-85	Combine remote tm and remote agent for remote job
 *			processing...dm
 *	16-feb-85	Call c_maxsub() to allow only one subprocess...dm
 *	18-feb-85	Fix bug due to input/output flipping for async...dm
 *	19-feb-85	Break up main tm to smaller modules for readability...dm
 *	22-feb-85	Update remote_tm to recognize JOB_ERR...dm
 *	13-mar-85	Update for new remote job execution protocol...dm
 *	18-mar-85	Do not exit Remote tm if invalid proc/stdout name...dm
 *
 ***************************************************************************
 * MERGE THE FOLLOWING TAE-V1.3 CHANGES...dm (21-may-85)
 *
 *	25-jan-85 	Change TAE-BADDCL to BADDECL to avoid conflict...nhe
 *
 ****************************************************************************
 * 	
 *	14-jun-85	Delete tm_argc, tm_argv args to main routine...dm
 *	01-jul-85	Fix UNIX compilation errors...dm
 *	25-jul-85	Fix UNIX lint errors...dm
 *	27-jul-85	Determine input from run_type rather than termtype
 *			in getcln()...dm
 *	01-aug-85	Fix next_cmd misspelling in local_tm()...dm
 *	01-aug-85	Add some time tracing for remote tm...nhe
 *	13-aug-85	Factor tm sub creation out of loop for remote tm...nhe
 *	21-aug-85	Terminate async job for inaccessible stdout file...joh
 *	12-sep-85	PR 995: Add chk_asy_done to check for async job
 *			completion msgs; called by prc_nextcmd...dab
 *	03-oct-85	PR 946: Added "not compiling" (FALSE) parameter to
 *			calling sequence of 'chk_vector'...dab
 *	29-oct-85	PR 933: Allow param quals when compiling. 'save_p_qual'
 *			now saves quals in proc context compile data area when
 *			compiling a proc...dab
 *	25-feb-86	"Captive" change: init CONTXT.interrupt to FALSE
 *			not TRUE in INICTX...palm
 *	16-jul-86	Honor RUNTYPE=ASYNC-PROCESS...palm
 *	16-jul-86	Honor ACTION= setup for PARMSETs...palm
 *	12-nov-86	Added TAE_FACELIFT code...krw
 ************* RCJM Merge into 1.4 -  3Nov86...nhe **********************
 *	12-feb-86	Perform RSLOGON and save context before remote tm loop,
 *			restore context for remote proc execution (implemented 
 *			for RCJM optimization)...dm
 ************************************************************************
 *	28-mar-87	Allow qualifiers on intrinsics if Y_QUALIF set.
 *			This for the window creation intrinsics...palm
 *	08-may-87	PR1181: changed "SLOGON" and "RSLOGON" to lower...ljn
 *	13-jul-87	Locals with qualifiers.  Here, save_p_qual updated
 *			to get proper symbol table...palm
 *	13-jul-87	Disallow NAMEd parameters as qualifiers.  See note
 *			in save_p_qual below explaining why such can't 
 *			be allowed...palm
 *	15-jul-87	Initialize the d_ package so that MDF/HLP 
 *			MDF/HLP substitution is enabled...palm
 *	23-jul-87	Now call intrin_init() which calls each of the
 *			modules with intrinsic command tables to get them
 *			to return a ptr to their respective table...ljn
 *	12-aug-87	Allow QUALS= to have a real TCL command line
 *			so that defaults can be set for the qualifiers.
 *			Example:  PARM A QUALS="Q |RESTORE=X| Y=2"...palm
 *	20-nov-87	Call CheckXEvent() in prccmd() to repaint exposed 
 *			windows...dm
 *	08-feb-88	Unknown terminal type generates error msg...ljn
 *	24-mar-88	Delete TAE_RCJM conditionals...ljn
 *	02-may-88	Initialize new CONTXT.parfile bit...palm
 *      11-may-88	Fixed error message for de-referenced variable...hsk
 *	11-may-88	New "utility execution mode" where placing a TCL
 *			command on the shell command line causes execution 
 *			in interactive mode, without greeting and slogon...palm
 *	13-may-88	Avoid t_init call unless truly interactive, otherwise
 *			you get screen init for an ASYNC job...palm
 *	28-nov-88	Add _TUTOR as new implicit local...palm
 *	05-dec-88	Init CONTXT.selftutor bit to FALSE in inictx...palm
 *	09-dec-88	Init CONTXT.compress bit to FALSE in inictx...palm
 *	27-jan-89 	memtab: use IVP pool if v_maxc < INT_MAXVAL, else
 *			use normal allocation.  This to allow for very large
 *			MAXVAL and to keep the IVP pool at reasonable size.
 *			Also, Reals in intrinsics now allowed...palm
 *	01-feb-89	VAX C requires IMPORT BOOLS of termlines, termcols...ljn
 *	14-feb-89	fix keywords for intrinsics...palm
 *			make repval extern...palm
 *	01-may-89	remove check on executing process from INTMODE--it's
 *			now in pdfprc:run_proc...palm 
 *	01-may-89	hook to set process name for BATCH and ASYNc...palm
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *	13-dec-89	Exit from utility command with proper status...ljn
 *	23-may-90	Remove RCJM stuff...ljn
 *	27-jun-90	Remove Facelift code...ljn
 *	26-dec-90	Call t_pinit if TM is in "utility mode"...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	27-jan-94	v53 SGI prot:free internal proc blocks differently...dag
 *	16-feb-94	PR2621: we were calling delvar then referencing the
 *			deleted VARIABLE to get its link. This is illegal..krw
 *	10-aug-94	PR2830: added maxc, count and valvec to addint...krw
 *	
 */

#include	"taeconf.inp"	/* TAE configuration definitions	*/
#include	"fileinc.inp"	/* file & file position context structs	*/
#include	"terminc.inc"	/* terminal package definitions		*/
#include	"syninc.inc"	/* syntax package structure and defs	*/
#include	"eventinc.inp"	/* e_ package defs & structs		*/
#include	"tmhost.inp"
#include	"symtab.inc"
#include	"tminc.inc"	/* TM-only host-independent definitions	*/
#ifdef	TAE_ASYNC 		/* Needed for remote and async cases only */
#include	"asyncinc.inc"
#endif
#include "taeintproto.h"

    GLOBAL struct CONTXT primctx;	/* proc context primary level*/
    GLOBAL TEXT next_cmd[NEXTSIZ+1]="";	/* previous mode help command		*/

    static TEXT tutor_msg[] =
"Re-enter the command line or type TUTOR to recover specified values.";

FUNCTION static BOOL call_do
(
    struct CONTXT	*ctx,		/* in: proc context		*/
    struct ITRCMD	*itrcmd	/* in: ITRCMD entry for cmd	*/

 );
FUNCTION CODE save 
(
    struct CONTXT	*ctx	/* in/out: command context	*/

 );
FUNCTION static VOID echo 
(
    struct CONTXT *ctx,		/* input: enclosing proc context	*/
    TEXT	  cmdstr[]	/* input: command string		*/

 );
FUNCTION static VOID tr_echo 
(
    struct CONTXT *ctx,		/* input: enclosing proc context	*/
    TEXT	  proc[]	/* input: name of proc			*/

 );
#if defined(SESSION_LOG2) || defined(VICAR_BATCH_LOG)
    GLOBAL COUNT cmdindex[CMDLINSIZ];   /* where to break up cmd line */
    GLOBAL COUNT cmdind_top = 0;        /* cmdindex pointer */
#endif

FUNCTION CODE runtype 
(
    struct SYMTAB 	*qualst		/* in: qualifier symtab	*/

 );
FUNCTION CODE restore 
(
    struct CONTXT	*ctx	/* in/out: command context	*/

 );
FUNCTION  static  VOID local_tm
(
	FUNINT	argc,
	TEXT	*argv[]

 );
FUNCTION  static CODE  init_localjob
(	
 FUNINT	*run_type		/* out: runtype ASYNC or BATCH   */
);
FUNCTION  static CODE prc_nextcmd
(
    TEXT	next_cmd[],		/* in/out: next TCL  command to execute */
    FUNINT	usermode,		/* in: user mode, COMMAND vs MENU     */
    struct  CONTXT *primctx		/* in: address of primary context blk */

 );
FUNCTION static VOID save_p_qual 
(
    struct CONTXT	*par_ctx,	/* in/out: parent context -- gets quals	*/
    struct CONTXT	*cmd_ctx	/* in/out: command context -- quals in parmst*/

 );
FUNCTION COUNT misprm 
(
    struct SYMTAB	*symtab,	/* in:  symbol table		*/
    FAST TEXT		mislst[STRINGSIZ+1]	/* out: list of missing */

 );



/*	tm - TAE monitor main program.
 */

FUNCTION int main 
(	
	FUNINT	argc,
	TEXT	*argv[]

)
    {
    IMPORT COUNT	 termcols;
    IMPORT COUNT	 termlines;	/* # lines on terminal screen	   */
    IMPORT CODE		 termtype;	/* terminal type		   */



    host_init();			/* host-dependent initialization   */
    d_setsub (substitute);		/* enable MDF/HLP substitution	   */
    d_setctx ((GENPTR) &primctx); /* enable MDF/HLP substitution	   */
    termlines = 0;  termcols = 0;  termtype = T_NOTTERM;
    intrin_init();			/* build intrinsics table	   */
    local_tm(argc, argv);		/* do processing as local tm	   */
    return 0;
    }	

/*	local_tm. Process user commands as the local TAE monitor.
 *	
 *	This module handles:
 *		- user commands as the interactive monitor
 *		- a single batch job as the batch monitor
 *		- a single async job as the async monitor
 *
 *	NOTE: For simplicity, we refer to a 'job' in interactive mode as
 *	equivalent to a TAE session, i.e. it starts with the first user
 *	command and terminates at the executuion of a "LOGOFF" or "EXIT"
 *	command.
 */

FUNCTION  static  VOID local_tm
(
	FUNINT	argc,
	TEXT	*argv[]

 )
    {
    IMPORT CODE		 cmd_mode;	/* NORMCMD or INTMODE			*/
    IMPORT CODE		 usermode;	/* MENUMODE or CMDMODE			*/
    IMPORT CODE		 prim_act;	/* TUTOR_ACTIVATION or CMD_ACTIVATION	*/
    IMPORT TEXT		 vrsion[];	/* program version		*/
    IMPORT struct CONTXT *curproc;
    IMPORT CODE		 run_type;	/* INTER, ASYNC, or BATCH	*/
    IMPORT struct VARIABLE *run_gbl;	/* pointer to $RUNTYPE  	*/
    IMPORT struct VARIABLE *tutor_gbl;	/* pointer to $TUTOR variable	*/
    IMPORT struct VARIABLE *sfi_gbl;	/* $SFI				*/
    IMPORT struct VARIABLE *skey_gbl;	/* $SKEY			*/
    IMPORT CODE		 termtype;	/* terminal type		*/
    IMPORT struct CONTXT primctx;
    IMPORT TEXT  	 next_cmd[];
    IMPORT COUNT	 termcols;
    IMPORT COUNT	 termlines;	/* # lines on terminal screen	   */
    static TEXT		 *screen_mode[2] = {"SCREEN", "NOSCREEN"};
    CODE		 code;
    TEXT		*utilityCommand;
#ifdef USE_DASH_S
    BOOL		force_slogon;	/* True if -s flag given	*/
    TEXT		slogon_cmd[10];

    force_slogon = FALSE;
    utilityCommand = NULL;
    if (argc > 1)
	utilityCommand = argv[1];	/* take first cmd line arg as cmd */ 
    if ((argc > 2) && (s_equal(argv[1], "-s") || s_equal(argv[1], "-S")))
	{
	force_slogon = TRUE;
	utilityCommand = argv[2];
	}
#else
    utilityCommand = argv[1];		/* take first cmd line arg as cmd */
#endif
    curproc = &primctx;
    primctx.prclevel = 0;		/* mark this primary level proc	*/
    primctx.backlink = NULL;		/* this is top level		*/
    usermode  = CMDMODE;		/* always start in command mode	*/
    next_cmd[0] = EOS;
    cmd_mode = NORMCMD;			/* i.e., not inter'pt 		*/
    ini_globals();			/* initialize globals 		*/
    inictx(&primctx);			/* init primary context		*/

    if (inter_proc())                   /* interactive TAE monitor      */
    	{
        if (utilityCommand)
	    t_pinit (&termlines, &termcols, &termtype);
	else
	    t_init (&termlines, &termcols, &termtype);
	run_type = INTER;
        if (utilityCommand)
#ifdef BUG_FIXES
            {
            s_copy (utilityCommand, next_cmd);  /* prime first command       */
            if (NULLSTR(next_cmd))
                s_copy(" ", next_cmd);          /* to avoid prompt */
            }
#else
	    s_copy (utilityCommand, next_cmd);	/* prime first command	     */
#endif
	else
            s_copy("slogon", next_cmd);		/* prime first command       */
	set_string (run_gbl, "INTERACTIVE");
        if (termtype == T_CRT)		/* enable formatted screen tutor */
	    set_value (tutor_gbl, (GENPTR)screen_mode, 1);
 	else if (termtype == T_NOTCRT)
	    {
	      tmmsg (SUCCESS, "Unknown terminal type.", "TAE-UNRECTTY", 0, 0, 
		     0, 0, 0);
	    t_bell ();
	    }
	}
#ifdef BUG_FIXES
#ifdef VMS
    else if (utilityCommand)            /* non-interactive utility command */
        {
        termtype = T_NOTTERM;                   /* force this           */
        run_type = BATCH;
        code = init_utility();                  /* init for utility cmd */
        if (code == FAIL)
            exit(TM_EXIT);
        s_copy (utilityCommand, next_cmd);      /* prime first command       */ 
       if (NULLSTR(next_cmd))
            s_copy(" ", next_cmd);              /* to avoid reading job file */ 
    }
#endif
#endif
   else
	{
       	termtype = T_NOTTERM;			/* force this		*/
	code = init_localjob(&run_type);	/* init for a local job */
	if (code == FAIL)
	    exit (TM_EXIT);
	}
    if (!utilityCommand)
        greet (vrsion);
    code = c_init();			/* task control package init	*/
    if (code != SUCCESS)		/* initialization error		*/
	{
	m_put("Unable to initialize process communication; host code = %d.",
	      "TAE-COMINI", (uintptr_t) code, 0, 0, 0, 0);
#ifdef TAE_ASYNC
	if (run_type == ASYNC)
	    {
	    set_string (skey_gbl, "TAE-COMINI");
	    IVAL (*sfi_gbl, 0) = -1;
	    async_exit();			/* .. or async died 	     */
	    }
#endif
	exit(TM_EXIT);
	}
    init_ins (&primctx);			/* installation exit	    */

#ifdef USE_DASH_S
    if (force_slogon)		/* Run slogon before utility command */
	{
	prim_act = CMD_ACTIVATION;
	s_copy("slogon", slogon_cmd);
	prc_nextcmd(slogon_cmd, usermode, &primctx);
	}
#endif

    while (FOREVER)				/* primary cmd level loop   */
	{
	prim_act = CMD_ACTIVATION;	/* assume procs activated by cmd method	*/
	prc_nextcmd(next_cmd, usermode, &primctx);
	if (utilityCommand)
#ifdef VMS
	    exit (IVAL(*sfi_gbl,0) <= 0 ? 0 : 1);
#else
	    exit (IVAL(*sfi_gbl,0) <= 0 ? 1 : 0);
#endif
					/* only one shot if utility	   */
	}				/* exit from logoff_do/exit_do     */
    }


/*	allprm - TRUE if all parameters in a symbol table have
 *	current values specified.
 *
 *	TBD: In light of misprm, allprm may no longer be needed.
 */

FUNCTION BOOL allprm 
(
    struct SYMTAB	*symtab	/* in: symbol table			*/

 )
    {
    TEXT  			list[STRINGSIZ+1];
    COUNT			m;

    m = misprm (symtab, list);
    return (m==0);
    }

#if defined(VICAR_BATCH_LOG) || defined(SESSION_LOG2)
/* bchlog_cmd - send command string to the batch log.
 * This is a special function which uses a C global array of saved indexes
 * into the command string as a guide in breaking the string up in the log.
 * Thus, if the input command had continuation lines, the line will be
 * broken at the same points in the log (but not necessarily with the same
 * amout of white space).
 */

FUNCTION VOID bchlog_cmd
(
    TEXT	cmdstr[]		/* input.  Command string to log */

 )
    {
    IMPORT COUNT cmdindex[CMDLINSIZ];	/* breakup indexes into cmdstr	*/
    COUNT	reccount;
    COUNT	lowindex;
    TEXT	buf[CMDLINSIZ+1];
    COUNT	length;

    reccount = 0;
    lowindex = 0;
    while (FOREVER)
	{
	length = s_copy(cmdstr, buf);
	if ((length > cmdindex[reccount]) &&  /* if this isn't the last string*/
	    (cmdindex[reccount] > 0))
	    {
	    buf[cmdindex[reccount]] = EOS;	/* cut off the command line */
	    s_append("+", buf);
	    put_stdout(&buf[lowindex]);
	    lowindex = cmdindex[reccount++];	/* move to next partial string*/
	    }

	/* Since cmd line substitutions are done after cmdindex is set	*/
	/* up, the last line may be too big for EDT, the printer, etc.	*/
	/* so write it out in smaller chunks.				*/

	else
	    {
	    while (length > lowindex+PRINTERLINESIZ-1)
		{
		buf[lowindex+PRINTERLINESIZ-1] = '+';
		buf[lowindex+PRINTERLINESIZ] = EOS;
		put_stdout(&buf[lowindex]);
		lowindex += PRINTERLINESIZ-1;
		length = s_copy(cmdstr, buf);
		}
	    put_stdout(&buf[lowindex]);
	    break;
	    }
	}
    return;
    }

#endif
/* bldcmd - work toward building a full TAE command string, given the
 * the accumulated string so far and the next single input line.
 * Strips comments and trailing blanks.
 * "+" as last non-comment character on input line indicates continuation.
 * Function returns :
 *		CONTINUE if another continuation line is expected,
 * 		SUCCESS if this is the last line of the command,
 *		FAIL	if command line is longer than the maximum size.
 *
 * NOTE: This function takes the single input line as well as the accumulated
 *	 command string.
 *
 * NOTE:  This function assumes that a quoted string must be closed on one line
 *	  if the line has a comment at the end.
 */

FUNCTION CODE bldcmd
(
    TEXT	inbuf[],		/* in/out: single input line buffer	*/
    TEXT	cmdstr[]		/* in/out: accumulated cmd string	*/

 )
    {
    BOOL	inquote;		/* TRUE if current character is quoted	*/
    CODE	code;
#if defined(SESSION_LOG2) || defined(VICAR_BATCH_LOG)
    IMPORT COUNT cmdindex[CMDLINSIZ];   /* 2nd session log structures */
    IMPORT COUNT cmdind_top;
#endif

    inquote = FALSE;
    code = app_cmd(inbuf, cmdstr, inquote);	/* append inbuf to cmdstr	*/
    if (code == FAIL)				/* no more room in cmdstr	*/
	{
	tmmsg(PROCFAIL, "Command too long.  Command ignored.",
	      "TAE-LONGCMDLINE", 0, 0, 0, 0, 0);
	cmdstr[0] = EOS;
	}
#if defined(SESSION_LOG2) || defined(VICAR_BATCH_LOG)
    else if (code == CONTINUE)
        cmdindex[cmdind_top++] = s_length(cmdstr);
    else
        {
        cmdindex[cmdind_top] = 0;
        cmdind_top = 0;
    }
#endif
    return(code);
    }

/*
 *	call_do.   Returns true if the intrinsic command should be
 *	executed.
 */

FUNCTION static BOOL call_do
(
    struct CONTXT	*ctx,		/* in: proc context		*/
    struct ITRCMD	*itrcmd	/* in: ITRCMD entry for cmd	*/

 )
    {
    LONG	flags;
    struct SEARCH *srchpt;

    flags = (*itrcmd).flags;
    if ((*ctx).subblk)	{		/* in a subcmd block?		*/
        if ((*ctx).subact  ||  (*ctx).special == COMPILING)
	    return (TRUE);		/* always call if active subcmd	*/
	else
	    return (flags & Y_ENDSUB);	/* call if END-SUB
					   */
    }
    if ((srchpt = (struct SEARCH *)(*ctx).srchblk) != NULL) { /* if search mode...	*/
    	if ((*srchpt).type != SRCH_PROC)
            return (flags & Y_SEARCH);	/* call only if requested	*/
    	else
    	    return (flags & Y_PROCSEARCH);
    } else { 
      if (!(*ctx).inbody)		/* if pre-body and no search...	*/
        return (TRUE);			/* always call 			*/
      else {
	if (true_part(ctx))		/* if true part of if...	*/
	  return (TRUE);			/* always call			*/
	else
	  return (flags & Y_FALSE);	/* false part: call if request
					   */
      }
    }
    }


/*   check_asy_req. Check if a request has arrived from an async job.
 */

    FUNCTION  static  VOID  check_asy_req(void)

    {
#ifdef TAE_ASYNC
    IMPORT BOOL		async_request;	/* TRUE if async request is pending  */
			
    if (async_request)    		
	{
	async_request = FALSE;
	tmmsg (SUCCESS, "Async job requesting parameters.  Use REPLY command.",
	       "TAE-ASYNCREQ", 0, 0, 0, 0, 0);
	}
#endif
    return;
    }

/*   chk_asy_done. Check if any async jobs have completed and require
     that job completion messages be generated.
 */

    FUNCTION  static  VOID  chk_asy_done(void)

    {
#ifdef TAE_ASYNC
    IMPORT struct ACB	*acb_head;	/* Points to ACB list */

    struct ACB		*acb;
    TEXT		job_done_msg[STRINGSIZ+1];

    for (acb = acb_head; acb != NULL; acb = (*acb).link)

	if ( (*acb).waiting  &&  !(*acb).active )
	    {
		(*acb).waiting = FALSE;
		s_copy ("Job \"", job_done_msg);
		s_append ((*acb).name, job_done_msg);
		s_append ("\" complete.", job_done_msg);
		tmmsg (SUCCESS, job_done_msg, "TAE-ASYNCDONE", 0, 0, 0, 0, 0);
	    }

#endif
    return;
    }


/*	chk_do.  Check the environment of an intrinsic command against
 *	bits in the ITRCMD entry.
 */

FUNCTION CODE chk_do
(
    struct CONTXT	*ctx,		/* in: current proc context	*/
    struct ITRCMD	*itrcmd	/* in: ptr to ITRCMD entry	*/

 )
    {
    IMPORT CODE		cmd_mode;	/* NORMCMD or INTMODE		*/
    IMPORT CODE		run_type;	/* INTER, ASYNC, or BATCH		*/

    LONG		flags;
    TEXT		*mode_name;

    flags = (*itrcmd).flags;		
/* first check against current command processing mode			*/
    if (cmd_mode == NORMCMD)
	if (!(flags & Y_CMD)) goto mode_err;

    if (run_type == BATCH && !(flags & Y_BATCH))
	{
	tmmsg(PROCFAIL, "'%s' not available in batch.",
	      "TAE-NOTBATCHCMD", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	return (FAIL);
	}
    if (run_type == ASYNC && !(flags & Y_ASYNC))
	{
	tmmsg(PROCFAIL, "'%s' not available in an asynchronous proc.",
	      "TAE-NOTASYNCMD", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	return (FAIL);
	}
    if ((flags & (Y_PRIM | Y_PROC)) == 0)	/* if both zero, set both	*/
        flags |=  (Y_PRIM | Y_PROC);
    if ((*ctx).prclevel == 0)		/* primary/proc level check	*/
	{
    	if (!(flags & Y_PRIM))
	    {
	    tmmsg(PROCFAIL, "'%s' available only in procedures.",
	          "TAE-PROCONLY", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	    return (FAIL);
	    }
	}
    else
	{
    	if(!(flags & Y_PROC))
	    {
	    tmmsg(PROCFAIL, "'%s' not allowed in proc definition file.",
	          "TAE-NOTINPDF", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	    return (FAIL);
	    }
	}
    if ((*ctx).prclevel == 0)			/* check state of procedure...	*/
	return (SUCCESS);
    else if ((*ctx).proctype == Y_UNKNOWN)	
	{
	if (!(flags & Y_INTRO))
	    {
	    tmmsg(PROCFAIL, "Introductory command (PROCEDURE, PROCESS, ...) missing.",
	          "TAE-MISINTRO", 0, 0, 0, 0, 0);
	    return (FAIL);
	    }
	}
    else if ((*ctx).inbody)
	{
	if (!(flags & Y_BODY))
	    {
	    tmmsg(PROCFAIL, "'%s' not allowed in procedure body.",
	          "TAE-NOTINBODY", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	    return (FAIL);
	    }
	}
    else
	{
	if (!(flags & Y_PREBODY))
	    {
	    tmmsg(PROCFAIL, "'%s' not allowed before procedure body.",
	          "TAE-NOTINPRE", (uintptr_t) (*itrcmd).cmd, 0, 0, 0, 0);
	    return (FAIL);
	    }
	}
    return (SUCCESS);

mode_err:
    if (cmd_mode == NORMCMD) mode_name = "normal command";
    tmmsg(PROCFAIL, "'%s' not available in %s mode.",
	  "TAE-BADCMD", (uintptr_t) (*itrcmd).cmd, (uintptr_t) mode_name, 
	  0, 0, 0);
    return (FAIL);
    }	

/*
 *	clr_latch - clear latching.
 */

FUNCTION VOID clr_latch 
(
    struct CONTXT	*pctx		/* in:  proc context			*/

 )
    {
    IMPORT struct SFILE	 prcfil;	/* proc file context (for f_ functions)	*/
    IMPORT CODE		 parmlatch;	/* parm latch flag		*/
    IMPORT struct CONTXT latchctx;	/* latched parameter proc contxt*/

    if (parmlatch == RUNLATCH)
	{
	parmlatch = NOLATCH;
    	if (!latchctx.intrinsic)
     	    f_close(&prcfil, F_KEEP);	/* close if not intrinsic	*/
	clsctx(&latchctx);
	}
    return;
    }

/*	clsctx - close proc context.
 *	Free any allocated structures.
 *	Decrement reference counts for referenced global variables.
 */

FUNCTION VOID clsctx 
(
    struct CONTXT	*pctx		/* in:  proc context		*/

 )
    {
    COUNT		i;
    struct VARIABLE	*v;
    struct IPBLOCK	*ipbpt;

    deltab(&((*pctx).locst));	/* delete local symbol table		*/
    deltab(&((*pctx).parmst));	/* delete parm symbol table		*/
    for (i = 0; i < (*pctx).numrefs; i++)	/* for each referenced global	*/
	{
	v = (*pctx).refs[i];	/* point to the variable		*/
	if (v == NULL)
	    continue;
	(*v).v_refcnt--;	/* decrement the reference count	*/
	(*pctx).refs[i] = NULL;
	}
    deltab(&(*pctx).qualst);	/* delete qualifier symbol table	*/
    delsub(&(*pctx).subptr);	/* delete SUBCMD chain			*/

    if ((*pctx).acbptr != NULL)	/* if there's an ACB, release it	*/
      free_acb((struct ACB*) (*pctx).acbptr);

    ipbpt = (struct IPBLOCK *)(*pctx).int_procs; /* internal proc blocks */
    if (ipbpt != NULL)
	{
	struct IPBLOCK *nextBlk;
	for (nextBlk = (*ipbpt).flink; ipbpt != NULL; )
	    {
    	    tae_free (ipbpt);
	    ipbpt = nextBlk;
	    if (nextBlk != NULL) nextBlk = (*ipbpt).flink;
	    }
	}

    /* The iteration control blocks must be deleted because we may have
     * a RETURN command in the middle of constructs...
     */
    cleanlp((*pctx).icbptr);	/* delete iteration blocks		*/
    return;
    }

/*
 *	comp_search - search for variable in context of a compiling proc.
 *
 *	Assumes the variable to find is in one of:
 *
 *	- the "before" symbol table (i.e., the declarations before the
 *	  first SUBCMD statement).
 *	- if we are currently in a SUBCMD block, the declarations that
 *	  have already occurred in this block.
 *	- the "after" symbol table (i.e., the declarations after the
 *	  last SUBCMD block).
 */

FUNCTION static struct VARIABLE *comp_search 
(
    TEXT		name[],		/* in: name of variable to find	*/
    struct CONTXT	*pctx		/* in: compilation proc context	*/

 )
    {
    struct COMP_HOUSEKEEP *comp;
    struct VARIABLE	*v;
    struct SUBCMD	*cursub;

    comp = (*pctx).comp;		/* point to compilation housekeeping	*/
    for (v = (*comp).before.link; v != NULL; v = (*v).v_link)  /* each var in sym tab*/
	if (s_equal(name, (*v).v_name))
	    return(v);			/* found; return variable		*/

    if ((*pctx).subblk)			/* if we're in a SUBCMD statement block	*/
	{
	cursub = (*comp).cursub;	/* point to current SUBCMD struct	*/
	for (v = (*cursub).symtab.link; v != NULL; v = (*v).v_link)  /* each var in sym tab*/
	    if (s_equal(name, (*v).v_name))
		return(v);		/* found; return variable		*/
	}

    for (v = (*comp).after.link; v != NULL; v = (*v).v_link)  /* each var in sym tab*/
	if (s_equal(name, (*v).v_name))
	    return(v);			/* found; return variable		*/
    return(NULL);			/* not found				*/
    }

/*	echo.   Echo the command string if appropriate.
 */

FUNCTION static VOID echo 
(
    struct CONTXT *ctx,		/* input: enclosing proc context	*/
    TEXT	  cmdstr[]	/* input: command string		*/

 )
    {
    IMPORT struct VARIABLE  	*echo_gbl, *becho_gbl, *aecho_gbl;
    IMPORT CODE 		run_type;
    struct VARIABLE		*echo_var;
    TEXT			flag;
    COUNT			index;

    if ((*ctx).prclevel == 0)		/* level 0: 			    */
        {
	if (run_type != INTER)		
#ifdef VICAR_BATCH_LOG
            bchlog_cmd (cmdstr);        /* always echo unless interactive   */
#else
	    put_stdout (cmdstr);	/* always echo unless interactive   */
#endif
#ifdef SESSION_LOG2
        else sl2write(cmdstr,TRUE);
#endif
	}
    else
	{
	if (run_type == INTER)		/* get controlling global	    */
	    echo_var = echo_gbl;
	else if (run_type == BATCH)
	    echo_var = becho_gbl;
	else
	    echo_var = aecho_gbl;
	if ((*ctx).prclevel > (*echo_var).v_count)	/* if echo short... */
	    index = (*echo_var).v_count - 1;		/* use last	    */
	else
	    index = (*ctx).prclevel  - 1;		/* use corresponding*/
	flag = *(SVAL(*echo_var, index));		/* first character  */
	if (flag == 'F')
#if defined(VICAR_BATCH_LOG) || defined(SESSION_LOG2)
            bchlog_cmd (cmdstr);
#else
	    put_stdout (cmdstr);				
#endif
	else if ((flag == 'Y' || flag == 'B')  && (*ctx).inbody)
#if defined(VICAR_BATCH_LOG) || defined(SESSION_LOG2)
            bchlog_cmd (cmdstr);
#else
	    put_stdout (cmdstr);
#endif
	}
    return;
    }

/*	tr_echo.   Generate proc trace if appropriate.
 *
 */

FUNCTION static VOID tr_echo 
(
    struct CONTXT *ctx,		/* input: enclosing proc context	*/
    TEXT	  proc[]	/* input: name of proc			*/

 )
    {
    IMPORT struct VARIABLE  	*echo_gbl, *becho_gbl, *aecho_gbl;
    IMPORT CODE 		run_type;
    FAST struct VARIABLE	*echo_var;
    FAST TEXT			flag;
    COUNT			index;

    if (run_type == INTER)			/* get proper global*/
	echo_var = echo_gbl;
    else if (run_type == BATCH)
	echo_var = becho_gbl;
    else
	echo_var = aecho_gbl;

    if ((*ctx).prclevel > (*echo_var).v_count)	/* if echo short... */
	index = (*echo_var).v_count - 1;	/* use last	    */
    else if ((*ctx).prclevel == 0)
        index = 0;
    else
	index = (*ctx).prclevel  - 1;		/* use corresponding*/
    flag = *(SVAL(*echo_var, index));		/* first character  */

    if (flag == 'T')				/* trace proc name  */
         put_stdout (proc);	

    }

/* getcln - get command line.
 *
 * Gets full TCL command line, including continuation lines.
 * On return, all continuation lines have been appended to the first
 * line, with the "+" replaced by a blank separator.
 * Comments and trailing blanks are stripped.
 *
 *	Return codes:
 *
 *		SUCCESS - line read from input successfully
 *		F_EOF - End of fie was encountered on batch input file
 *		F_FILERR - Error reading batch input file of script file
 */

FUNCTION CODE getcln 
(
    FAST TEXT		prompt[],	/* in: prompt (without the '>')	*/
    FAST TEXT 		cmdstr[]	/* out: command string		*/

 )
    {
    IMPORT CODE		 run_type;	/* terminal monitor run type    */
    IMPORT struct SFILE	 job_file;	/* batch/async primary input	*/

    CODE	code;
    COUNT	nrec;			/* record counter, ignored	*/
    TEXT	pmtbuf[STRINGSIZ+1];	/* prompt buffer		*/
    TEXT	inbuf[STRINGSIZ+1];	/* single line input buffer	*/




    cmdstr[0] = EOS;
    if (run_type != INTER)		/* If non-interactive, read from job file	*/
	{
	do
	    {
	    code = f_read (&job_file, inbuf);
	    if (code == F_EOF) return (code);
	    if (code != SUCCESS)
	        {
	        m_put ("Error reading job file. %s",
		       "TAE-BCHRD", (uintptr_t) job_file.errmsg, 0, 0, 0, 0);
	 	return (F_EOF);
		}
	    code = bldcmd(inbuf, cmdstr);	/* append cmd to full command string	*/
	    }
	while (code == CONTINUE);		/* loop if continuation line	*/
	if (code == FAIL)
	    flush_cmd(&job_file, inbuf, &nrec);	/* get past this  cmd		*/
 	return (code);				/* SUCCESS or FAIL		*/
	}
    else
	{
	s_copy(prompt, pmtbuf);			/* get prompt string	*/
	s_append(">", pmtbuf);
	cmd_noscreen (A_LASTCMD, pmtbuf, cmdstr);  /* read command string */
	return (SUCCESS);
	}
    }

/*	history_save.   Save command string in $LASTCMD global.
 *	$LASTCMD(1) has the most recent command, $LASTCMD(2) the next oldest,
 *	and etc.
 */

FUNCTION VOID history_save
(
    TEXT	cmdstr[]		/* in: string at least MAXSTRSIZ */

 )
    {
    IMPORT struct VARIABLE *last_gbl;

    COUNT	i;
    TEXT	save_char;
    TEXT	*saved;

    /* push down each current value, add new value at first component	*/

    if ((*last_gbl).v_count == 0)	/* if null, no history saving;	*/
	return;				/* (for allocation studies)	*/
    i = (*last_gbl).v_size;		/* max size allowed		*/
    save_char = cmdstr[i];
    cmdstr[i] = EOS;			/* clip to largest allowed	*/
    saved = s_save(cmdstr);		/* save in dynamic memory	*/
    cmdstr[i] = save_char;		/* restore character 		*/
    if (saved == NULL)			/* if no memory, do nothing	*/
	return;		
#ifdef HISTORY_DUPS
    if (s_equal(saved, SVAL(*last_gbl, 0)))
        {                              /* If cmd same as last command, */
        s_free(saved);                  /* then don't save in history   */
        return;                         /* (same as DCL)                */
        }
#endif
    if ((*last_gbl).v_count == (*last_gbl).v_maxc)
        s_free (SVAL(*last_gbl, (*last_gbl).v_maxc - 1));
    else
	(*last_gbl).v_count++;
    for (i=(*last_gbl).v_count - 1;   i > 0;   i--)
	SVAL(*last_gbl, i) = SVAL(*last_gbl, i-1);
    SVAL(*last_gbl, 0) = saved;
    return;
    }

/*	inictx - initialize proc context block.
 *	Returns FAIL if a memory overflow occurs.
 *	Must be coordinated with reini_ctx (in tutor.c).
 *
 *	NOTE:  The list of implicitly ref'd globals has been moved
 *		to tminit.c.  inictx now only checks v_implicit
 *		to see if the global is implicit.
 */

FUNCTION CODE inictx 
(
    FAST struct CONTXT	*pctx		/* in/out: proc context block		*/

 )
    {
    IMPORT struct SYMTAB	glbtab;		/* global symbol table	*/
    FAST COUNT			i;
    FAST struct VARIABLE	*p;

    static TEXT *standout[] = {TERMINAL, ""};

    zero_block((GENPTR) (*pctx).refs, MAXREF * sizeof (GENPTR));	/* init global ref pointers	*/
    (*pctx).numrefs    = 0;		/* init to no global refs		*/
    (*pctx).parmst.link = NULL;
    (*pctx).locst.link = NULL;
    (*pctx).qualst.link = NULL;
    if ((*pctx).prclevel > 0)		/* if not on primary level		*/
	{
	for (p=glbtab.link, i=0; p != NULL; p=(*p).v_link)
	    {
	    if ((*p).v_implicit)
		{
		(*pctx).refs[i] = p;	/* ref the implicits	*/
		i++;
		}
	    }
        (*pctx).numrefs = i;
        refs_ins(pctx);			/* installation exit for other refs	*/
        (*pctx).inbody     = FALSE;	/* not in procedure body		*/
        (*pctx).pdf.name[0] = EOS;
    	(*pctx).int_procs = NULL;	/* no internal procs yet */
	}
    else
	{
	(*pctx).inbody = TRUE;		/* fake for environment checking	*/
	s_copy("TERMINAL", (*pctx).pdf.name);
	if (addstr2(&((*pctx).locst), "_STDOUT", 2, 2, standout, V_LOCAL)
	    != SUCCESS)			/* add stdout name			*/
	    {
	    overr();			/* report memory overflow		*/
    	    return(FAIL);
	    }
	}
    (*pctx).prcpos.possav = FALSE;	/* proc position not yet saved		*/
    (*pctx).proctype   = Y_UNKNOWN;	/* init to proc type unknown		*/
    (*pctx).pdf.libr[0] = EOS;
    (*pctx).pdf.type[0] = EOS;
    (*pctx).pdf.attr[0] = EOS;
    (*pctx).subcmd[0]  = EOS;
    (*pctx).subsiz = 0;
    (*pctx).exe_spec[0] = EOS;
    (*pctx).help_spec[0] = EOS;
    (*pctx).subblk     = FALSE;		/* not yet in any SUBCMD block		*/
    (*pctx).subact     = TRUE;		/* not yet in wrong SUBCMD block	*/
    (*pctx).subfnd     = FALSE;		/* correct SUBCMD block not yet found	*/
    (*pctx).subs       = FALSE;		/* no subcmds seen yet			*/
    (*pctx).interrupt  = FALSE;		/* interrupts not allowed	*/
    (*pctx).compiled   = FALSE;		/* not a compiled PDF			*/
    (*pctx).parfile    = FALSE;		/* assume not a PAR file    */
    (*pctx).selftutor  = FALSE;		/* assuem not selftutor     */
    (*pctx).compress = FALSE;		/* assume not compressed tutor */
    (*pctx).onfailcmd  = FALSE;		/* not executing onfail cmd		*/
    stk_in((*pctx).locifs, MAXIF, TRUE);/* init local if stack			*/
    stk_in((*pctx).glbifs, MAXIF, TRUE);/* init global if stack			*/
    stk_in((*pctx).typstk, MAXNEST, TYPE_NONE);	/* init type stack		*/
    (*pctx).pdf_line = 0;		/* current line number			*/
    (*pctx).icbptr = NULL;		/* init chain of iteration blocks	*/
    (*pctx).srchblk = NULL;		/* no search in progress		*/
    (*pctx).subptr = NULL;		/* no subcmd chain			*/
    (*pctx).ctx_ins = NULL;		/* installation courtesy		*/
    (*pctx).special = NOT_SPECIAL;	/* no special processing yet		*/
    (*pctx).comp = NULL;		/* no compilation data yet			*/
    (*pctx).acbptr = NULL;		/* pointer to ACB being constructed	*/
    return(SUCCESS);
    }


/* 	init_localjob. Perform initialization for local job processing
 */

FUNCTION  static CODE  init_localjob
(	
 FUNINT	*run_type		/* out: runtype ASYNC or BATCH   */
)
    {
    IMPORT struct SFILE  stdo_fil;	/* Actual stdout SFILE 		*/
    IMPORT struct SFILE	 *pstdo_fil;	/* Pointer to stdout SFILE 	*/
    IMPORT struct SFILE  job_file;	/* primary input file		*/
    IMPORT struct VARIABLE *run_gbl;	/* ptr to runtype global var    */
    IMPORT struct VARIABLE *sfi_gbl;	/* $SFI				*/
    IMPORT struct VARIABLE *skey_gbl;	/* $SKEY			*/
    IMPORT struct CONTXT   primctx;	/* primary proc context		*/

    struct VARIABLE	*stdoptr;	/* Pointer to _stdout local symbol */
    static TEXT 	*standout[2] = {NULL, "APPEND"};
    TEXT		record[STRINGSIZ+1];
    CODE		code;

    pstdo_fil = &stdo_fil;		
    code = f_ophf (pstdo_fil, STDOUTLUN, PRIMARY_STDOUT, F_WRITE);
    if (code != SUCCESS)
	pstdo_fil = NULL;
    code = f_ophf (&job_file, STDINLUN, PRIMARY_INPUT, F_READ);
    if (code != SUCCESS)
	{
	put_stdout ("Unable to access job file.");
	return (FAIL);
	}
    if (pstdo_fil != NULL)
	{
	stdoptr = lookex (&primctx.locst, "_STDOUT");	
	standout[0] = (*pstdo_fil).full_spec;	    	/* value vector     */
	set_value (stdoptr, (GENPTR) standout, 2);	/* set _STDOUT      */
	}
    f_read (&job_file, record);			/* read first data record...*/
    if (s_lseq ("BATCH", record))		/* tells us BATCH vs ASYNC..*/
	{
	f_read (&job_file, record);		/* read new process name    */
	set_process_name (BATCH, record);
	*run_type = BATCH;
	set_string (run_gbl, "BATCH");
	}
#ifdef TAE_ASYNC
    else if (s_lseq ("ASYNC", record))
	{
	f_read (&job_file, record);		/* read new process name    */
	set_process_name (ASYNC, record);
	*run_type = ASYNC;		/* local job execution	*/
	set_string (run_gbl, "ASYNC");
	get_asyctx();			/* read init message	*/
	if (pstdo_fil == NULL)		/* if no stdout file	*/
	    {				/* set error status	*/
	    set_string (skey_gbl, "TAE-STDOUTOPEN");
	    IVAL (*sfi_gbl, 0) = -1;
	    emit_stat();		/* send status code to parent TM */
	    async_exit();		/* terminate job	*/
	    return (FAIL);
	    }
	}
#endif
    else
	{
	put_stdout ("Job file incorrectly formatted.");
#ifdef TAE_ASYNC
	if (*run_type == ASYNC)
	    {
	    set_string (skey_gbl, "TAE-BADJOBFILE");
	    IVAL (*sfi_gbl, 0) = -1;
	    async_exit();
	    }
#endif
	return (FAIL);
	}
    return (SUCCESS);
    }
#ifdef BUG_FIXES
#ifdef VMS
/* 	init_utility. Perform initialization for a non-interactive utility
 *	command.  Simulates batch so the log files will work properly, but
 *	without a job file.  Since there is no job file, getcln() must not
 *	be called, but this is ensured by local_tm().
 *	This routine is needed under VMS because t_write can't write to
 *	sys$output when it's a file!  Sys$output is used explicitly below
 *	instead of PRIMARY_STDOUT because TAE_JOB_LOG (the value of the macro
 *	PRIMARY_STDOUT) may not be defined in a utility run from batch.
 */

    FUNCTION  static CODE  init_utility(void)
    {
    IMPORT struct SFILE  stdo_fil;	/* Actual stdout SFILE 		*/
    IMPORT struct SFILE	 *pstdo_fil;	/* Pointer to stdout SFILE 	*/
    IMPORT struct VARIABLE *run_gbl;	/* ptr to runtype global var    */
    IMPORT struct CONTXT   primctx;	/* primary proc context		*/

    struct VARIABLE	*stdoptr;	/* Pointer to _stdout local symbol */
    static TEXT 	*standout[2] = {NULL, "APPEND"};
    CODE		code;

    pstdo_fil = &stdo_fil;		
    /* Sys$output used instead of PRIMARY_STDOUT since it may not be defined */
    code = f_ophf (pstdo_fil, STDOUTLUN, "SYS$OUTPUT", F_WRITE);
    if (code != SUCCESS)
	pstdo_fil = NULL;
    if (pstdo_fil != NULL)
	{
	stdoptr = lookex (&primctx.locst, "_STDOUT");	
	standout[0] = (*pstdo_fil).full_spec;	    	/* value vector     */
	set_value (stdoptr, (GENPTR) standout, 2);	/* set _STDOUT      */
	}
    set_string (run_gbl, "BATCH");

    return (SUCCESS);
    }
#endif
#endif

/*	memtab - build a symbol table from a table of resident variables.
 *
 * NOTE: For speed, memtab gets VARIABLE blocks from the intrinsic parameter
 * pool (ipp) rather than allocating them in dynamic memory. This block
 * already has the v_cvp set so that the value vector is not allocated any
 * more. memtab points the v_valid field to the RESIDVAR, which is
 * typically static.
 */

FUNCTION CODE memtab 
(
    struct RESIDVAR 	*resvar,	/* table of resident variables		*/
    FUNINT		numres,		/* number of resident variables		*/
    struct SYMTAB	*s		/* out: symbol table header		*/

 )
    {

    struct RESIDVAR 	*p;		/* table of resident variables		*/
    FAST struct VARIABLE *v;		/* dynamic in-memory variable		*/
    COUNT		i;
    COUNT		j;
    TEXT		**bidsv;	/* blt-in dflt val ptr to vec of ptrs	*/
    COUNT		bytes;

    initab(s);				/* initialize symbol table		*/
    p = resvar;
    for (j = 0; j < numres; j++, p++)	/* step thru resident parm table */
	{
	/*   If possible, we allocate from the special IVP VARIABLE pool
	because intrinsic processing is so important to TCL performance.
	In the pool, the largest v_maxc is INT_MAXVAL and REALs are not 
	allowed--this covers most intrinsic parms and keeps the pool at
	a reasonable size.  Note that DEFAULT, VALID, and INITIAL in 
	the PARM and LOCAL statements demand the full MAXVAL and these 
	cannot use the fast IVP pool.
	*/
	if ((*p).maxc <= INT_MAXVAL && (*p).type != V_REAL)	
	    {
	    v = get_ivp(s);		 /* get an ivp at end of chain	*/
	    (*v).v_valid = (*p).valid;
	    }
	else				/* use slow (but large) pool	*/
	    {
	    if ((*p).valid)		/* for simplicity: no valids here */
		tmierr (503);	
	    v = allvar (s);		/* allocate normally (but slow) */
	    (*v).v_valid = NULL;
	    (*v).v_maxc = (*p).maxc;	/* must set these fields for allval  */
	    (*v).v_type = (*p).type;
	    (*v).v_cvp = allval (v);	/* allocate value vector	*/
	    }
	s_copy((*p).name, (*v).v_name);	/* move parm name		*/
	(*v).v_class = V_PARM;
	/* TBD: allow NAME variables in memtab; following assumes 'valued' */
	(*v).v_type	= (*p).type;
	(*v).v_keyword  = (*p).kiv;
	(*v).v_count    = -1;		/* assume no value for now	*/
        (*v).v_nullable = ((*p).minc == 0);
	(*v).v_minc	= ((*p).minc == 0) ? 1 : (*p).minc;
	(*v).v_maxc	= (*p).maxc;
	(*v).v_dcount = (*p).dcount;
/*
 *	If the RESIDVAR specifies a .dcount of zero, yet the .minc does not
 *	allow nullables, we assume that there is no default.
 */
	(*v).v_default = (*p).dcount > 0 ||
	  ((*v).v_nullable && (*p).dcount == 0) ;
	(*v).v_size = (*p).size;
	(*v).v_dvp = NULL;
	if ((*v).v_default)			/* default value present?*/
	    {
	    (*v).v_count = (*p).dcount;
	    if ((*v).v_type == V_INTEGER)	/* copy values if integer */
		{
		bytes = (*v).v_count * valsize((*v).v_type);
		bytmov((*p).dvp, (*v).v_cvp, bytes);	/* set current	*/
		}
	    else if ((*v).v_type == V_STRING)
		{
		bidsv = (TEXT **) (*p).dvp;		/* intrin deflt */
		for (i = 0; i < (*v).v_dcount; i++)
		    if (( SVAL(*v, i)=s_save(bidsv[i])) == NULL)
			goto mt_overr;
		}
	    }
	}
    return(SUCCESS);

mt_overr:
    overr();				/* report memory overflow		*/
    deltab(s);				/* delete corrupted symbol table	*/
    return(FAIL);
    }

/*	misprm - return list of missing parameters, i.e., parameters
 *	which do not have a current value specified.
 *	Parms in returned list are separated by commas.
 *	Parameter qualifiers are also checked and, if missing, added to the
 *	list as "parm.qual".
 *
 *	Returns number of missing parms (that fit in mislst).
 */

FUNCTION COUNT misprm 
(
    struct SYMTAB	*symtab,	/* in:  symbol table		*/
    FAST TEXT		mislst[STRINGSIZ+1]	/* out: list of missing */

 )
    {
    FAST struct VARIABLE *v;		/* variable in symbol table	*/
    struct VARIABLE	*vq;
    COUNT		listlen;	/* length of missing parms 	*/
    BOOL		missing;	
    COUNT		count;

    count = 0;
    mislst[0] = EOS;			/* clear the list		*/
    listlen = 0;
    for (v = (*symtab).link; v != NULL; v = (*v).v_link)
        {
        missing = ((*v).v_type == V_NAME && (*v).v_ref == NULL) ||
                  ((*v).v_type != V_NAME && (*v).v_count == -1);
	if (missing)
	    {
	    count++;
	    if ((s_length((*v).v_name) + listlen + 2) > STRINGSIZ)
		{
		mislst[listlen] = EOS;		/* term string leaving comma*/
		return(count);
		}
	    s_append((*v).v_name, mislst);	/* append name to list	*/
	    listlen = s_append(", ", mislst);
	    }
	if ((*v).v_pv12)		/* quals possible?	*/
	    for (vq = (*v).v_qualst.link; vq != NULL; vq = (*vq).v_link)
		{				/* for each qualif of this parm	*/
		missing = ((*vq).v_type == V_NAME && (*vq).v_ref == NULL) ||
			  ((*vq).v_type != V_NAME && (*vq).v_count == -1);
		if (missing)
		    {
		    count++;
		    if ((s_length((*v).v_name) +
			 s_length((*vq).v_name) + listlen + 3) > STRINGSIZ)
			{
			mislst[listlen] = EOS;		/* term string leaving comma*/
			return(count);
			}
		    s_append ((*v).v_name, mislst);
		    s_append (".", mislst);
		    s_append ((*vq).v_name, mislst);	/* appends parm.qual to list*/
		    listlen = s_append (", ", mislst);
		    }
		}
        }
    if (listlen > 1)
	mislst[listlen-2] = EOS;		/* delete last comma	*/
    return(count);
    }

/*
 *	plcini - proc local symbol table initialization.
 *
 *	NOTE: When plcini is called (for procs only) the pdf must be
 *	open so that CONTXT.pdf and CONTXT.subcmd are set up.
 */

FUNCTION CODE plcini 
(
    struct CONTXT	*pctx		/* in/out:  proc context	*/

 )
    {
    TEXT	*vptr[1];		/* string value vector		*/
    TAEINT intval[1];			/* _TUTOR value vector		*/
    static TEXT *standout[] = {TERMINAL, ""};
    static TEXT *retern[] = {"RETURN"};

    if (addstr2(&((*pctx).locst), "_STDOUT", 2, 2, standout, V_LOCAL) != SUCCESS)
	goto pl_overr;				/* add stdout name		*/
    if (addstr2(&((*pctx).locst), "_ONFAIL", 2, 1, retern, V_LOCAL) != SUCCESS)
	goto pl_overr;
    vptr[0] = (*pctx).pdf.name;
    if (addstr2(&((*pctx).locst), "_PROC", 1, 1, vptr, V_LOCAL) != SUCCESS)
	goto pl_overr;
    vptr[0] = (*pctx).subcmd;
    if (addstr2(&((*pctx).locst), "_SUBCMD", 1, 1, vptr, V_LOCAL) != SUCCESS)
	goto pl_overr;
    intval[0] = 0;
    if (addint(&((*pctx).locst), "_TUTOR", 1, 1, intval, V_LOCAL) != SUCCESS)
	goto pl_overr;
    return(SUCCESS);

pl_overr:
    overr();				/* report memory overflow	*/
    return(FAIL);
    }

/*
 *	prc_nextcmd. Process the next TCL command.
 *
 *	The next command is received from the argument next_cmd.
 *	if next_cmd is empty, then the command is received from the
 *	operator (for interactive case) or from the job file (for batch
 *	and async case).
 */

FUNCTION  static CODE prc_nextcmd
(
    TEXT	next_cmd[],		/* in/out: next TCL  command to execute */
    FUNINT	usermode,		/* in: user mode, COMMAND vs MENU     */
    struct  CONTXT *primctx		/* in: address of primary context blk */

 )
    {
    IMPORT struct VARIABLE *prompt_gbl;	/* prompt string		      */
    TEXT	cmdstr[CMDLINSIZ+1];
    CODE	code;

    if (usermode == CMDMODE)
	{
	if (!NULLSTR(next_cmd)) 	/* HELP user gave next cmd  */
	    {			
	    s_copy (next_cmd, cmdstr);
	    next_cmd[0] = EOS;		/* delete it for next run   */
	    }
	else
	    {
	    check_asy_req();		/* check for an async request       */
	    chk_asy_done();		/* check for async jobs done and
					   waiting for completion msg	    */
	    code = getcln (SVAL(*prompt_gbl,0), cmdstr);
	    				/* get cmd from operator/job file   */
	    if (code == F_EOF) 		/* end of job file   		    */
	 	{
		logoff_do(primctx, primctx);  /* End of batch/async err */
		return (F_EOF);
		}
    	    }
	}
    else					/* assume menu mode	*/
	menmod(cmdstr);				/* display menu 	*/
    if (NULLSTR(cmdstr))			/* if null input...	*/
	return (SUCCESS);			/* get command again	*/
    history_save(cmdstr);
    prccmd(cmdstr, primctx);	    		/* process command	*/
    return (SUCCESS);
    }


/*	prccmd - process TAE command mode command string.
 *	Executes proc activation and intrinsic commands.
 *	WARNING:  delgbl_do imitates this function.  When making any changes
 *		  here, analyze delgbl_do to see if analogous changes are
 *		  required there.
 *
 *	Return codes:
 *		DO_*** codes as for intrinsic commands.
 */

FUNCTION CODE prccmd 
(
    TEXT   cmdstr[CMDLINSIZ+1],		/* in/out:  command string		*/
    FAST struct CONTXT	*procctx	/* in:  current proc context		*/

 )
    {
    IMPORT CODE		 cmd_mode;	/* NORMCMD or INTMODE			*/
    IMPORT struct ECB	 ecbi;		/* event block for operator interrupts	*/
    IMPORT struct SFILE	 prcfil;	/* proc file context (for f_ functions)	*/
    IMPORT CODE		 parmlatch;	/* parm latch flag			*/
    IMPORT struct CONTXT latchctx;	/* latched context			*/
    IMPORT CODE		 run_type;	/* BATCH, INTER, or ASYNC		*/
    IMPORT CODE		 help_proc;	/* SAVEPROC or NOPROC			*/
#ifdef TAE_ASYNC
    IMPORT struct SYMTAB asy_parmst;	/* params from parent monitor, async	*/
#endif
    IMPORT CODE 	 usermode;
    IMPORT struct SFILE	 *pstdo_fil;
    IMPORT struct VARIABLE *skey_gbl;	/* pointer to $SKEY variable		*/
#ifdef SYNTAX_CHECK
    IMPORT struct VARIABLE *switch_gbl;
#endif

    CODE		runt;		/* run type requested			*/
    struct SYNBLK	sb;		/* syntax block (for syntax package)	*/
    TEXT		cmd[FSPECSIZ+1];/* command line command			*/
    struct ITRCMD	*itrcmd;	/* intrinsic command			*/
    struct CONTXT	cmdctx;		/* command context			*/
    CODE		cmdcod;		/* command return code			*/
    CODE		qual_code;	/* qualifier return code	*/
    CODE		code;
    BOOL		active;		/* true if we are executing commands	*/
    BOOL		we_care;	/* do we care about subsitute errors	*/
    BOOL		get_parm_qual;	/* quick flag for...		*/

    get_parm_qual =  (*procctx).special == GET_PARM_QUAL;
    cmdctx.prclevel = (*procctx).prclevel + 1;		/* mark new proc as next level	*/
    if (cmdctx.prclevel > MAX_PROC_NEST)
	{
	tmmsg(PROCFAIL, "Proc nesting exceeds %d levels.",
	      "TAE-MAXPNEST", (uintptr_t) MAX_PROC_NEST, 0, 0, 0, 0);
	return (DO_RETURN);
	}
    if (inictx(&cmdctx) != SUCCESS)
	return (DO_RETURN);			/* init context of next proc */
    cmdctx.backlink = procctx;			/* link to previous	     */
    cmdctx.sb = &sb;				/* let point to syntax block */
    if ((*procctx).prclevel == 0)		/* if interactive level...   */
	t_attn(&ecbi);				/* enable operator attention */


    active = (*procctx).srchblk == NULL
             &&  true_part(procctx);
    we_care = active;
    code = substitute (cmdstr, CMDLINSIZ, (GENPTR) procctx, we_care); /* perform substitution		*/
    if (code != SUCCESS  &&  we_care)			/* if substitute error		*/
	goto subst_err;
    code = cmd_parse (&sb, cmdstr, (*procctx).label, cmd, cmdctx.subcmd);
    if (code != SUCCESS)
	goto syntax_err;
    cmdctx.subsiz = s_length (cmdctx.subcmd);
    chk_label(procctx);					/* resolve GOTO search		*/
    active = (*procctx).srchblk == NULL			/* re-calc for found label	*/
             &&  true_part(procctx);
    cmdcod = DO_SUCCESS;				/* in case !active		*/
    if (NULLSTR(cmd))
        cmdcod = DO_SUCCESS;				/* ignore null lines		*/
    else if ((code = itr_lookup(cmd, cmdctx.subcmd, &itrcmd)) == SUCCESS)
	{						/* intrinsic:		*/
	if (get_parm_qual)		/* no intrin parms as qualifiers */
	    goto itr_p_qual_err;
	cmdctx.intrinsic = TRUE;			/* if latched, tutor will know  */
	s_copy ((*itrcmd).cmd, cmdctx.pdf.name);	/* save cmd for tutor, etc	*/
	s_copy((*itrcmd).subcmd, cmdctx.subcmd);	/* get full subcmd name		*/
	if (!s_equal("TUTOR", (*itrcmd).cmd))
	    clr_latch (procctx);			/* clr latching if not tutor 	*/
	if (!s_equal("HELP", (*itrcmd).cmd))
	    help_proc = NOPROC;				/* clr previous help proc name */
        if (call_do(procctx, itrcmd))			/* if "do" should be called	*/
    	    {
	    echo (procctx, cmdstr);			/* echo if appropriate	*/
	    if (chk_do(procctx, itrcmd) != SUCCESS)	/* check command environment	*/
		goto close_return;
	    qual_code = setqlf(cmdctx.intrinsic, &sb, &cmdctx.qualst) ;

	    /* an intrinsic may have qualifiers (if Y_QUALIF) but must 
	       do its own processing of the qualifiers */

	    if (qual_code != S_NONE   &&
	       !((*itrcmd).flags & Y_QUALIF))
		    goto qual_err;			/* not allowed for intrinsics	*/
	    if ((*itrcmd).flags & Y_PROCSYN)		/* proc invocation syntax	*/
		{					/* so do syntax scan		*/
		if (memtab((*itrcmd).partab, (*itrcmd).numprm, &(cmdctx.parmst)) != SUCCESS)
    		    goto close_return;
		if (updtab(&cmdctx.parmst, &sb) != SUCCESS)
    		    goto intrinsic_cmderr;
		if (list_missing (&cmdctx.parmst) != SUCCESS)
		    goto intrinsic_cmderr;
		}
#ifdef SYNTAX_CHECK
            if ((IVAL(*switch_gbl,0) & SW_SYNTAX) &&
                        run_type == INTER &&
                        ((*itrcmd).flags & Y_SYNTAX))
                {
                cmdcod = DO_SUCCESS;    /* skip if syntax check */
                }
            else
#endif
                {
	    if ((*itrcmd).flags & Y_CLEAR)		/* init $SFI/$SKEY ?		*/
		ini_status();
	    cmdcod = (*(*itrcmd).cproc)(procctx, &cmdctx);	/* call "do" function 	*/
                }
    	    }
	}
    else if (code == ITR_BADSUB)			/* bad intrinsic subcmd	*/
	goto subcmd_err;

    /* Here, the command is a proc (or an intrinsic with Y_PDF set)  */
    else if (!(*procctx).inbody && active && !get_parm_qual)
	{				/* pre-body proc activation: */
	tmmsg(PROCFAIL, "Unrecognized declaration command '%s'.",
	      "TAE-BADDECL", (uintptr_t) cmd, 0, 0, 0, 0);
	cmdcod = DO_RETURN;				/* no _ONFAIL pre-body		*/
	}
    else if (active)		 			/* activate proc if 'active'	*/
	{
	if (code == ITR_PDF)			/* get full spellings:    */
	    {
	    s_copy ((*itrcmd).cmd, cmd);
	    s_copy((*itrcmd).subcmd, cmdctx.subcmd);	
	    }
	clr_latch(procctx);				/* clear latching if necessary	*/
	cmdctx.intrinsic = FALSE;			/* not an intrinsic		*/
	if (setqlf(cmdctx.intrinsic, &sb, &cmdctx.qualst) == FAIL)	/* set qualifers 		*/
	    goto close_return;
        if (get_parm_qual)
    	    runt = NORUN;				/* force NORUN   */
        else
            runt = runtype (&cmdctx.qualst);		/* get |RUNTYPE| */
	echo (procctx, cmdstr);				/* echo if appropriate	*/
	if ((*procctx).prclevel > 0)			/* if not from primary level	*/
	    clssav(procctx);				/* close calling proc & save pos*/
	ini_status();					/* initialize $SFI and $SKEY	*/
	if (opnpdf(procctx, &cmdctx, cmd, &prcfil) != SUCCESS)
    	    goto re_open;
	tr_echo (procctx, prcfil.full_spec);		/* trace if selected */
	if (plcini(&cmdctx) != SUCCESS)			/* init implicit locals		*/
    	    goto pdf_error;
	if (pdftab(&cmdctx, &prcfil) != SUCCESS)	
    	    goto pdf_error;
	f_movpos(&prcfil.posctx, &cmdctx.prcpos);	/* save posit for latching	*/
	if (restore (&cmdctx) != SUCCESS)		/* do RESTORE qualifier		*/
	    goto pdf_error;
	if (action_setup (&cmdctx) != SUCCESS)
	    goto pdf_error;
	if (!get_parm_qual && procctx == &primctx && run_type == ASYNC)   
	    {				/* use params from parent monitor */
#ifdef TAE_ASYNC
	      code = parmrg ((struct SYMTAB *) &asy_parmst.link, 
			     &(cmdctx.parmst), FULLSET);
	    deltab (&asy_parmst);			/* NOTE: we assume here that NAME */
#endif
	    }					/* are not in asy_parmst 	*/
	else
	    {
	    if (updtab(&cmdctx.parmst, &sb) != SUCCESS)
		goto pdf_cmderr;	
	    if (!get_parm_qual && list_missing (&cmdctx.parmst) != SUCCESS)
		goto pdf_cmderr;
	    }
	if (save (&cmdctx) != SUCCESS)		/* do SAVE qualifier		*/
	    goto pdf_error;
	if (runt == BATCH || runt == ASYNC)	/* but ok for ASYNC-PROCESS */
	    if (s_equal(cmdctx.pdf.libr, "/LOCAL/"))
		goto runt_error;
	if (runt == BATCH)
	    cmdcod = run_batch (procctx, &cmdctx);
#ifdef TAE_ASYNC
	else if (runt == ASYNC  ||  runt == ASYNC_PROCESS)
	    cmdcod = run_async (procctx, &cmdctx);
#endif
	else if (runt == INTER)
	    {
	    if (set_stdout(procctx, &cmdctx) != SUCCESS) /* set new STDOUT	*/
		goto pdf_error;
	    deltab(&cmdctx.qualst);		/* delete qualst to avoid nesting */
	    cmdcod = run_proc(&cmdctx);	/* re-delete by clsctx won't hurt	*/
	    }
        else if (get_parm_qual)
           save_p_qual(procctx, &cmdctx);	     /* attach param quals    */
	else if (runt == NORUN)	
	    set_string (skey_gbl, prcfil.full_spec); /* $SKEY = PDF file spec */
	else
	    {
	    tmmsg (PROCFAIL,
		   "Unrecognized RUNTYPE qualifier.", "TAE-RUNTYPE", 0, 0, 
		   0, 0, 0);
	    cmdcod = DO_CHECK;
	    }
	f_close(&prcfil, F_KEEP);			/* close PDF			*/
	if ((*procctx).prclevel > 0)			/* if not from primary level	*/
	    if (opnsav(procctx) != SUCCESS)
    		return(DO_RETURN);
	}
    clsctx(&cmdctx);					/* close proc context		*/
    return(cmdcod);

subst_err:
    clr_latch(procctx);
    goto close_return;

syntax_err:
    tmmsg(PROCFAIL, sb.errmsg, "TAE-FMTERR", 0, 0, 0, 0, 0); /* report error			*/
    goto close_return;

subcmd_err:
    if (NULLSTR(cmdctx.subcmd))
	tmmsg(PROCFAIL, "Subcommand is required for '%s'.",
	      "TAE-SUBREQ", (uintptr_t) cmd, 0, 0, 0, 0);
    else
        tmmsg(PROCFAIL, "Undefined or ambiguous subcommand '%s'.",
	      "TAE-UNDEFSUB", (uintptr_t) cmdctx.subcmd, 0, 0, 0, 0);
    goto close_return;

qual_err:
    tmmsg(PROCFAIL, "Command qualifiers not allowed for '%s'.",
	  "TAE-NOQUAL", (uintptr_t) cmd, 0, 0, 0, 0);
    goto close_return;

itr_p_qual_err:
    tmmsg(PROCFAIL,
"Intrinsic command not allowed as parameter qualifier proc.",
	  "TAE-ITRPQUAL", 0, 0, 0, 0, 0);
    goto close_return;

close_return:
    clsctx(&cmdctx);
    return(DO_RETURN);

intrinsic_cmderr:
    if ((*procctx).prclevel == 0 && usermode != MENUMODE)
        {
	parmlatch = RUNLATCH;
        MOVE_STRUCT(cmdctx, latchctx);		/* save context for tutor	*/
	put_stdout(tutor_msg);
	}
    else
        clsctx(&cmdctx);
    return (DO_CHECK);

pdf_error:
    f_close(&prcfil, F_KEEP);
    goto re_open;

re_open:
    clsctx(&cmdctx);
    if ((*procctx).prclevel > 0)
	opnsav(procctx);			/* reopen calling proc & reposition	*/
    return(DO_CHECK);

pdf_cmderr:
    if ((*procctx).prclevel == 0  &&  usermode != MENUMODE)
        {
	parmlatch = RUNLATCH;
        MOVE_STRUCT(cmdctx, latchctx);		/* save context for tutor	*/
	put_stdout(tutor_msg);
    	}
    else
    	{
        clsctx(&cmdctx);
    	f_close (&prcfil, F_KEEP);
        }
    if ((*procctx).prclevel > 0)
        opnsav(procctx);
    return (DO_CHECK);

runt_error:
    tmmsg (PROCFAIL, "Attempt to run an internal proc in async or batch.",
	   "TAE-RUNINT", 0, 0, 0, 0, 0);
    f_close (&prcfil, F_KEEP);
    goto re_open;
    }

/*	repval - replace current value for a variable.
 *	Assumes that current value array has already been allocated (but not
 *	necessarily any strings).
 *
 *	NOTE: This function also deallocates the input value strings if they
 *	are no longer needed.
 */

FUNCTION CODE repval 
(
    FAST struct VARIABLE *v,		/* in/out: symbol table entry		*/
    FAST TEXT		*value[],	/* in:  val str ptrs (vals to repl with)*/
    FUNINT		count,		/* in:  count of values for this parm	*/
    FUNINT		by_name	/* in: TRUE if value[0] is the name of	*/
					/* a referenced variable		*/
					
 )
    {
    IMPORT struct CONTXT *curproc;	/* current proc context		*/

    union				/* union of value vectors	*/
	{
	TAEINT		ival[MAXVAL];	/* temp integer array		*/
	TAEFLOAT	rval[MAXVAL];	/* temp real array		*/
	} vect;
    GENPTR		valvec = 0; /* pointer to value vector	*/
    COUNT		i;
    COUNT		val_count;	/* value count (of ref var)	*/
    struct VARIABLE	*ref = 0; /* reference			*/
    FAST CODE		code, val_type;
    BOOL		compiling;	/* TRUE if we're compiling a proc*/


    compiling = ((*curproc).special == COMPILING);
    if ((*v).v_type == V_NAME  ||  by_name)
        {
        if (count != 1)
    	    goto count_error;
	if (compiling)
	    ref = comp_search(value[0], curproc);
	else
	    ref = search (value[0], curproc);
        if (ref == NULL)
    	    {
    	    tmmsg(PROCFAIL, "Reference to undefined variable '%s'.",
		  "TAE-UNDEFVAR", (uintptr_t) value[0], 0, 0, 0, 0);
    	    goto err_exit;
	    }
	if (compiling && (*ref).v_class == V_GLOBAL &&  /* no de-ref globals...*/
	    !((*v).v_iparm  && s_equal("DEFAULT",(*v).v_name)))  /* except parm dflts*/
	    goto deref_error;
        }

    /*****

    The following "if" is a kludge to handle de-referencing for DEFAULT
    or INITIAL.   Since we don't know the type of the variable being
    defined, we can't properly check the reference.  The approach here
    is to change the type of the DEFAULT or INITIAL variable to be
    V_NAME so the reference can be remembered for parm_do and local_do.

    Normally, when one changes the v_type of a variable from
    "directly valued" to "named", there is a value vector to
    de-allocate.  However, for v_iparm VARIABLE structures, the value
    vector is self-contained and need not be explicitly de-allocated.

    BEWARE: this code makes DEFAULT and INITIAL special parameter
    names for intrinsic commands.

    ******/

    if (by_name && (*v).v_iparm &&
	(s_equal ("DEFAULT", (*v).v_name) || s_equal ("INITIAL", (*v).v_name)))
	(*v).v_type = V_NAME;

    if ((*v).v_type == V_NAME)
      	{
        (*v).v_ref = ref;
        (*v).v_default = FALSE;
	s_free (value[0]);		/* free string with variable name */
	return (SUCCESS);
	}

    if (by_name)			/* for de-referenced variable	*/
	{
	valvec = (*ref).v_cvp;
	val_count = (*ref).v_count;
	val_type = (*ref).v_type;
	}
    else
	{
	val_count = count;
	val_type = (*v).v_type;
        if (val_type == V_INTEGER)
    	    {
	    for (i = 0; i < val_count; i++)
		{
		if (NULLSTR(value[i]))
		    vect.ival[i] = 0;
		else
		    if (s_s2i(value[i], &(vect.ival[i])) != SUCCESS)	
			goto int_error;
		}
	    valvec = (GENPTR) vect.ival;
	    }
	else if (val_type == V_REAL)
	    {
	    for (i=0; i < val_count; i++)
		{
		if (NULLSTR(value[i]))
		    vect.rval[i] = 0.0;
		else					/* convert to binary	*/
		    if (s_s2r(value[i], &(vect.rval[i])) != SUCCESS)
			goto real_error;
		}
	    valvec = (GENPTR) vect.rval;
	    }
	else if (val_type == V_STRING)
	    valvec = (GENPTR) value;
	}
    code = chk_vector (v, val_type, valvec, val_count, FALSE);
    if (code != SUCCESS)
	goto value_error;
    if (val_type == V_STRING  &&  !by_name) /* do not free input value strings */
	trs_value (v, valvec, val_count);   /* transfer input string pointers */
    else
	{
	code = set_value(v, valvec, val_count);	
	if (code != SUCCESS)
	    goto no_memory;
	free_val(value, count);		/* free values' space from dyn memory	*/
	}
    (*v).v_default = FALSE;		/* value no longer defaulted	*/
    return(SUCCESS);

value_error:
    if (code == V_BADCOUNT)
	goto val_count_error;
    if (code == V_BADSIZE)
	goto size_error;
    if (code == V_BADTYPE)
	goto type_error;
    if (code == V_BADVALID)
	goto valid_error;
    if (code == V_BADAMBIG)
	goto ambig_error;
    goto file_error;			/* assume file error otherwise	*/

deref_error:
    tmmsg(PROCFAIL, "De-referencing of globals not allowed in compiled procs.",
	  "TAE-COMPDRFG", 0, 0, 0, 0, 0);
    goto err_exit;

val_count_error:
    if (val_count == 0)
	tmmsg (PROCFAIL, "'%s' does not accept a null value.",
	       "TAE-NOTNULL", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    else if (val_count < (*v).v_minc)
        tmmsg(PROCFAIL, "Too few values. '%s' must have at least %d values.",
	"TAE-TOOFEW", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    else
	tmmsg(PROCFAIL, "Too many values. '%s' must have at most %d values.",
	"TAE-TOOMANY", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    goto err_exit;

count_error:
    if (count == 0)
	tmmsg (PROCFAIL, "'%s' does not accept a null value.",
	"TAE-NOTNULL", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    else if (count < (*v).v_minc)
        tmmsg(PROCFAIL, "Too few values. '%s' must have at least %d values.",
	"TAE-TOOFEW", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    else
	tmmsg(PROCFAIL, "Too many values. '%s' must have at most %d values.",
	"TAE-TOOMANY", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_minc,
	       0, 0, 0);
    goto err_exit;

int_error:
    tmmsg(PROCFAIL, "Invalid integer value for '%s'.",
	  "TAE-INVINT", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto err_exit;

real_error:
    tmmsg(PROCFAIL, "Invalid real value for '%s'.",
	  "TAE-INVREAL", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto err_exit;

size_error:
    tmmsg(PROCFAIL, "String too long.  Max string size for '%s' is %d.",
	  "TAE-LONGSTR", (uintptr_t) (*v).v_name, (uintptr_t) (*v).v_size,
	  0, 0, 0);
    goto err_exit;

type_error:
    tmmsg(PROCFAIL, "Type mismatch assigning '%s' to '%s'.",
	  "TAE-BADTYPE", (uintptr_t) (*ref).v_name, (uintptr_t) (*v).v_name,
	  0, 0, 0);
    goto err_exit;

file_error:
    tmmsg(PROCFAIL, "File not found or incorrect file specification for '%s'.",
	  "TAE-BADFILE", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto err_exit;

no_memory:
    overr();				/* report memory overflow	*/
    goto err_exit;

valid_error:
    if ((*v).v_type == V_STRING)
        tmmsg(PROCFAIL,
		"Value not one of the acceptable strings defined for '%s'.",
	      "TAE-INVSTR", (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    else
        tmmsg(PROCFAIL, "Value out of the defined range for '%s'.", "TAE-RANGE",
              (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto err_exit;

ambig_error:
    tmmsg(PROCFAIL, "Value ambiguous for '%s'.", "TAE-AMBIGVAL",
	  (uintptr_t) (*v).v_name, 0, 0, 0, 0);
    goto err_exit;

err_exit:
    free_val(value, count);		/* free values' space from dyn memory	*/
    return (FAIL);
    }

/*
 *	restore.  Perform RESTORE qualifier operation;
 */

FUNCTION CODE restore 
(
    struct CONTXT	*ctx	/* in/out: command context	*/

 )
    {

    struct VARIABLE		*v;
    TEXT			errmsg[STRINGSIZ+1];
    CODE			code;

    v = lookex(&(*ctx).qualst, "RESTORE");
    code = SUCCESS;
    if (!(*v).v_default)
        {
	code = rest_parm (SVAL(*v,0), ctx, errmsg);
	if (code == SOME_REJECTED)
	    {
	      tmmsg(SUCCESS, errmsg, "TAE-NOTUSED", 0, 0, 0, 0, 0);  /* information only	*/
	    code = SUCCESS;
	    }	
	else if (code != SUCCESS)
	  tmmsg (PROCFAIL, errmsg, "TAE-BADPFILE", 0, 0, 0, 0, 0);
	}
    return (code);
    }

/*
 *	runtype.  Classify the runtype qualifier.
 *
 *	Returns BATCH, INTER, ASYNC, NORUN, or FAIL
 */

FUNCTION CODE runtype 
(
    struct SYMTAB 	*qualst		/* in: qualifier symtab	*/

 )
    {
    struct VARIABLE		*v;
    TEXT			*value;

    v = lookex (qualst, "RUNTYPE");
    if (v == NULL)
	return (FAIL);
    if ((*v).v_default)
        return (INTER);			/* no, default to INTER */
    value = SVAL(*v,0);
    if (s_lseq (value, "INTERACTIVE") || s_lseq (value, "SYNCHRONOUS"))
        return (INTER);
    if (s_lseq(value, "BATCH"))
        return (BATCH);
#ifdef TAE_ASYNC
    if (s_lseq(value, "ASYNCHRONOUS"))
        return (ASYNC);
    else if (s_lseq (value, "ASYNC-PROCESS"))
	return (ASYNC_PROCESS);
#endif
    if (s_lseq(value, "NORUN"))
    	return (NORUN);
    return (FAIL);
    }

/*
 *	save.   Perform SAVE qualifier operation.
 */

FUNCTION CODE save 
(
    struct CONTXT	*ctx	/* in/out: command context	*/

 )
    {
    struct VARIABLE		*v;
    TEXT			errmsg[STRINGSIZ+1];
    CODE			code;

    v = lookex(&(*ctx).qualst, "SAVE");
    code = SUCCESS;
    if (!(*v).v_default)
        {
	code = save_parm (SVAL(*v,0), ctx, F_WRITE, errmsg);
	if (code != SUCCESS)
	  tmmsg(PROCFAIL, errmsg, "TAE-SAVERR", 0, 0, 0, 0, 0);
	}
    return (code);
    }

/*
 *	save_p_qual - Save parameter qualifiers into parent proc's
 *	latest variable's qualifier symbol table.
 *	Assumes that the qualifiers have been built as a parameter
 *	symbol table in the current command's context.
 *
 *	This function is useful because we assume the last parameter
 *	in the parent's symbol table is the one we're currently getting
 *	qualifiers for.
 */

FUNCTION static VOID save_p_qual 
(
    struct CONTXT	*par_ctx,	/* in/out: parent context -- gets quals	*/
    struct CONTXT	*cmd_ctx	/* in/out: command context -- quals in parmst*/

 )
    {
    struct VARIABLE	*v;
    struct SYMTAB	*destst;	/* where to save qualifiers */
    struct SYMTAB 	*qualtab;	/* qualifier symbol table   */
    struct SUBCMD	*s;		/* subcmd compilation block */

    if ( ((*par_ctx).comp) == NULL)	/* are we not compiling ?	*/
	destst= ((*par_ctx).parmqual) ? &(*par_ctx).parmst : &(*par_ctx).locst; 
    else
	{					/* compilaton mode: 	*/
	if ((*par_ctx).subblk)			/* if in a SUBCMD block	*/
	    {
	    s = (*(*par_ctx).comp).cursub;		/* current SUBCMD table entry*/
	    destst = &(*s).symtab;			/* sym tab for this SUBCMD blk*/
	    }
	else
	    {
	    if ((*par_ctx).subs)			/* if any SUBCMDs found yet*/
		destst = &(*(*par_ctx).comp).after; /* after subs symbol table*/
	    else
		destst = &(*(*par_ctx).comp).before; /* before subs symbol table*/
	    }
	}

    for (v = (*destst).link; v != NULL; v = (*v).v_link)
	{				/* find last parm in parent's sym tab	*/
	if ((*v).v_link == NULL)	/* if we've found last parm		*/
	    break;
	}
    if (v != NULL)			/* in case no parms -- shouldn't happen	*/
	{
        qualtab = &(*v).v_qualst;
	(*qualtab).link = (*cmd_ctx).parmst.link;	/* move parm sym tab...	*/
					/* to this parm's qualifier sym tab...	*/
					/* i.e., the parms become parameter qualifiers*/
	(*cmd_ctx).parmst.link = NULL;	/* prevent early de-allocation by prccmd*/

	/********

	    A variable is not allowed to have a NAMEd qualifier because
	    there are too many problems, e.g., the default value of the NAME
	    might be a local--local to the qualifier PDF--and not exist any
	    more.   Another example: it might point to a GLOBAL, resulting
	    in the proc with the qualified variable having an undocumented 
    	    reference to a GLOBAL.

	    Here, we delete NAME parameters from the qualifier list 
    	    (before they cause any more chaos).

	*********/

	for (v = (*qualtab).link;  v != NULL;)
	    {
	    struct VARIABLE	*v_link = (*v).v_link;	/*PR2621 */
	    if ((*v).v_type == V_NAME)
		delvar (qualtab, v);
	    v = v_link;
	    }
	}
    return;
    }

/*	updtab - update a symbol table from a command mode command line.
 *	Assumes proc invocation syntax.
 *	Note: The syntax block (sb) is modified by this routine.
 *
 *	Returns:
 *		SUCCESS -- ok
 *		FAIL -- invalid variable specification found
 *		SOME_REJECTED -- some mandatory parm missing
 */

FUNCTION CODE updtab 
(
    FAST struct SYMTAB	*symtab,	/* in/out: symbol table			*/
    FAST struct SYNBLK	*sb		/* in/out:  syntax block		*/

 )
    {
    GENPTR	pos;
    CODE	code;
    TEXT	*msg;
    TEXT	key[TOKESIZ+1];		/* keyword				*/
    TEXT	*value[MAXVAL];		/* value pointers			*/
    COUNT	count;			/* number of values for single parm	*/
    COUNT	i;
    struct VARIABLE *v;

/* Start out assuming values by position	*/

    for (v=(*symtab).link; FOREVER; v=(*v).v_link)
	{
	pos = (*sb).curchr;		/* save position in cmd stream		*/
        code = getkey(sb, key);		/* try to get a "parm="			*/
	if (code == SUCCESS  ||  code == S_KEYWORD)
	    break;			/* if key present, no more positionals	*/
	if (code == EOS)
	    goto ud_cdone;
	(*sb).curchr = pos;		/* no key; start over for positional	*/
	code = getval(sb, value, MAXVAL, &count);	/* get value		*/
	if (code == S_SYNERR)
	    goto ud_gverr;
	if (code == EOS)
	    goto ud_cdone;		/* done if EOS				*/
        if (v == NULL)
    	    goto ud_toomany;		/* too many positionals			*/
	if (count >= 0)			/* if value is present			*/
	    {
    	    if (repval(v, value, count, code==S_DEREF) != SUCCESS)
		return (FAIL);
	    code = upd_p_qlf(sb, v);	/* update parm quals from cmd line	*/
	    if (code != SUCCESS)
		return (FAIL);
	    }
	}


/* Remaining values must be by parameter name or flagged keyword	*/

    (*sb).curchr = pos;			/* restore posit to get 1st keyword	*/
    while (FOREVER)
	{
	if ((code = getkey(sb, key)) == EOS)	/* get next keyword		*/
	    goto ud_cdone;		/* done if EOS				*/
	if (code == S_NONE)		/* if parm specified positionally	*/
	    goto ud_poserr;
	if (code != SUCCESS  &&  code != S_KEYWORD)
	    goto ud_gkerr;
	if (code == S_KEYWORD)
    	    {
	    code = getval(sb, value, MAXVAL, &count);	/* get keywords		*/
	    if (code == S_SYNERR)
	        goto ud_gverr;
	    if (code == EOS)
	        goto key_syntax;
	    if (count < 1)
	        goto key_error;
	    code = key_trans (symtab, value[0], &v);	/* get associated parm 	*/
	    if (code != SUCCESS)
	        goto bad_keyword;
	    }
	else
	    {
	    code  = lookab(symtab, key, &v);		/* find variable name	*/
	    if (code != SUCCESS)
		goto ud_noparm;
	    code = getval(sb, value, MAXVAL, &count);	/* get the value	*/
	    if (code == S_SYNERR)
		goto ud_gverr;
	    if (code == EOS)				/* "key=" at end of line*/
		goto ud_cdone;
	    }
	if (count >= 0)					/* if value present	*/
	    {
	    if (repval(v, value, count, code==S_DEREF) != SUCCESS)
    		return (FAIL);
	    code = upd_p_qlf(sb, v);	/* update parm quals from cmd line	*/
	    if (code != SUCCESS)
		return (FAIL);
	    }
	}

ud_cdone:
    return(SUCCESS);

ud_toomany:
    tmmsg(PROCFAIL,
"Too many parameter values for this command or too many qualifiers.",
	  "TAE-PARS", 0, 0, 0, 0, 0);
    for (i=0; i < count; i++)
	s_free(value[i]);
    return (FAIL);

ud_gverr:
    tmmsg(PROCFAIL, (*sb).errmsg, "TAE-INVPVAL", 0, 0, 0, 0, 0);
    return (FAIL);

key_syntax:
key_error:
/* Disabled 11/26/01 RGD: synerr crashes... I think errchr is not set.
    synerr(sb, "Invalid keyword syntax");
    tmmsg(PROCFAIL, (*sb).errmsg, "TAE-KEYSYNTAX");
*/
    tmmsg(PROCFAIL, "Invalid keyword syntax", "TAE-KEYSYNTAX", 0, 0, 0, 0, 0);
    return (FAIL);

bad_keyword:
    if (code == AMBIG)
	msg = "Ambiguous keyword '%s'.";
    else
	msg = "Undefined keyword '%s'.";
    tmmsg (PROCFAIL, msg, "TAE-KEYWORD", (uintptr_t) value[0], 0, 0, 0, 0);
    return (FAIL);

ud_gkerr:
    tmmsg(PROCFAIL, (*sb).errmsg, "TAE-INVPNAME", 0, 0, 0, 0, 0);
    return (FAIL);

ud_poserr:
    tmmsg(PROCFAIL,
	    "Positional values may not follow values specified by name.",
	  "TAE-POSERR", 0, 0, 0, 0, 0);
    return (FAIL);

ud_noparm:
    if (code == AMBIG)
	tmmsg(PROCFAIL, "Ambiguous abbreviation '%s'.", "TAE-AMBIGPAR",
	      (uintptr_t) key, 0, 0, 0, 0);
    else
	tmmsg(PROCFAIL,
"'%s' is an undefined parameter or unknown qualifier.",
	      "TAE-BADPAR", (uintptr_t) key, (uintptr_t) key, 0, 0, 0);
    return (FAIL);
    }

