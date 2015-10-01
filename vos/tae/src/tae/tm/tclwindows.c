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
 *	TCL WINDOW command.
 * 
 *	TBD: the name "window" for this command is probably wrong
 *	TBD: need -PICTURE to load xgedit file into window
 *
 ***************************************************************************
 *      BEWARE! This file is used by X Windows TAE and non-X Windows
 * 	TAE alike. Any new _do routines and any code which
 *	relies on X Windows header files MUST be conditionally coded using 
 *	#ifdef XWINDOWS.
 ***************************************************************************
 *
 * CHANGE LOG:
 *
 *	20-may-87	fixed auto origining in -TEXT...palm
 *      20-may-87	fixed default origin for -TEXT...palm	
 *	20-may-87	fixed crash on window-list...palm
 *	28-may-87	Add window-colormap...palm
 *	31-may-87	Add window-info...palm
 *	02-jun-87	Add parenting to -list; add list output
 *			variable to -list...palm
 *	09-jun-87	Initialize for Xr calls here ()...dm
 *	16-jun-87	Fix window-list for no window named "root";
 *			fix existence check for when a parent is
 *			is specified on -create...palm
 *	18-aug-87	Remove "parent" parameter from -create;
 *			honor "root" in -info...palm			
 *      18-aug-87       Add get_windowcmd()...tpl
 *	18-aug-87	Fix parenting bug...palm
 *	18-nov-87	Add WPT commands...palm
 *	30-nov-87	Allow multiple WINDOW-INITs...palm
 *	21-jan-88	Fix duplicate panel crash...palm
 *	25-jan-88	Upon Wpt-Event, only copy viewed parameters
 *			into the target variable...palm
 *	25-jan-88	Add new PANEL name parameter to WPT-PANEL...palm
 *	18-feb-88	Fix error report in WPT-ERASE;  new WPT-REJECT...palm
 *			Add "all" option to WPT-ERASE...palm
 *	21-feb-88	Fix (once again) the WPT-ERASE error message...palm
 *	24-feb-88	Add WPT-BEGINWAIT and WPT-ENDWAIT...palm
 *	26-feb-88	Upon WPT-EVENT set flag that there is a procedure doing
 *			an event loop so that CheckXEvent gets disabled. 
 *			Without this, WPT-EVENT looses events to 
 *			CheckXEvent...palm 
 *	11-mar-88	Add more info to the "can't init" message...palm
 *	04-apr-88	Convert _SELECTed index to TCL type...pm
 *	18-apr-88	Add WPT-PARMUPDATE and WPT-VIEWUPDATE subcommands...dm
 *	20-apr-88	Add conditionals #ifdef XWINDOWS...ljn
 *	10-may-88	Setenv DISPLAY to proper string so applications receive
 *			benefit of WPT-INIT...palm
 *	11-may-88	Fix DISPLAY to do above correctly...palm
 *      01-jun-88       If default fail try $DISPLAY...tpl
 *      07-jun-88       Implemented ParmUpdate...tpl
 ******
 *	29-jun-88	Converted to X11...tpl
 *	08-jul-88	Add TIMEOUT to WPT-EVENT...krw
 *	25-jul-88	Change TIMEOUT period to real number in seconds...dm 
 *	27-jul-88 	Check for XrInput() fail status 0 (X11 change)...dm 
 * 	06-aug-88	Fix bug in XQueryTree() calling sequence...dm
 *	15-aug-88	Fix bug in check for XrXRAY type events...dm
 *	09-sep-88	Allow special host name of $DISPLAY in wpt-init to
 *			mean "use the shell variable DISPLAY to determin
 *			the connection"...palm
 *	05-oct-88	Set _panel.name from Wpt-Panel comamnd line so
 *			that window name becomes same as PANEL=name...palm
 *	02-jan-89	Fix call to XCreateSimpleWindow...palm
 *	26-jan-89	MAXVAL -> INT_MAXVAL in RESIDVAR...palm
 *      01-feb-89	Declare search() in updateVariable()...ljn
 *
 *************************************************************************
 *
 *	07-apr-89	Remove Xr dependency, use WPT/Xt architecture...dm
 *	14-jul-89	Request for interrupt from Xt at Wpt_Init()...dm
 *	19-jul-89	Ignore WPT_WINDOW_EVENTs in Wpt-Event loop...dm
 *	22-jul-89	Removed header file, sys/time.h for Apollo sake...ljn
 *	23-jul-89	Declare EventProcCtx in non-XWINDOWS case...ljn
 *	10-aug-89	Changed Vm_Find to Vm_FindVar if there is a
 *                      chance of failure...krw/tl/dm
 *	13-sep-89	VMS port (moved bunch of func decl's to top)...ljn
 *	30-sep-89	Set "ecbi" after a WPT Interrupt  event...dm
 *      17-oct-89       Call XStoreName in panel_do so convertname will
 *                      work...tpl
 *      18-oct-89       Remove call to wpt_convertname in convertname...tpl
 *      31-oct-89       use $DISPLAY as default for windowinit...tpl
 *	03-nov-89	Added compiling flag to chk_vector...krw
 *	09-jul-90	Removed EventProcCtx, #ifdef'd out setWait() and
 *			eraseonePanel(); moved this file into $TTM...ljn
 *	08-sep-90	new wptscript commands...palm
 *			and collapse "not implemented" reporting...palm
 *	12-nov-90	Fixed previous fix...ljn
 *	12-nov-90	WPT_PANEL modified to allow creation flags; 
 *			added WPT_SETPANELSTATE for generated code support...ljn
 *	13-nov-90	WPT_FAST is now WPT_FASTICONIC;
 *			WPT-PANEL needs a default creation state...ljn
 *	14-nov-90	Add TCL func $PANEL to check for panel existence...ljn
 *	16-nov-90	Added WPT-REHEARSE for DDO rehearsal...ljn
 *	18-Nov-90	Add code to put _subcmd in wptscript message...gc
 *      01-dec-90	Changed WPT-REHEARSE order valids from "min-> max" to
 *			"min-max", etc.  Also changed cycle to cycles...cew
 *	19-apr-91	VMS port; also, c_read changed to c_getmsg...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	21-aug-91	NEC (R3000/SYSV.4) port: wpt-closedisplay...ljn
 *      16-sep-91	use c_read to read ack in wptscript_do; VMS 
 *			uses c_getmsg (a bug?) because VMS has no c_read...palm
 *	12-may-92	PR1373: added kludge to allow a checkbox in a menubar
 *			to return "yes" or "no" in second value. Also improved
 *			error reporting from chk_vector...krw
 *			PR1411: copy all target qualifier values to the
 *			variable (not just _SELECT)...krw
 * 21-sep-92	PR1516: do not go through valid validation of item values
 *		because the items do that themselves...krw
 ***************************************************************************/

#include	"taeconf.inp"		/* TAE configuration definitions	*/
#include	"comminc.inp"
#include	"symtab.inc"		/* symbol table				*/
#include	"tminc.inc"		/* TM-only host-independent definitions	*/
#include	"expinc.inc"
#include        "chartype.inc"
#ifdef XWINDOWS
#include	"wptinc.inp"
#endif
#include	"eventinc.inp"


GLOBAL int		Vtclwin14 = 0;		/* source version number	*/

IMPORT Id Vm_New ();
IMPORT Id Co_New();
IMPORT Id Co_Find();
IMPORT struct VARIABLE *lookex();
IMPORT struct VARIABLE *search();
static COUNT translateFunc();
static CODE setOutputValue();
static notInitialized ();
static extractParent();
#ifdef XWINDOWS
static Window convertName();
GLOBAL Display *DisplayId = NULL;	/* NULL -> not X initialized */ 
GLOBAL int CurrentScreen;
#else
GLOBAL TAEINT *DisplayId = NULL;
#endif
static VOID notImplemented ();

#   define  ROOT_WINDOW  RootWindow(DisplayId, CurrentScreen)

#define CheckInit   if (DisplayId == NULL) {notInitialized(); return(DO_CHECK);}
#define Parm(name)  (lookex(&((*npctx).parmst), name))
#define StringParm(name)  SVAL(*Parm(name), 0)
#define IntegerParm(name) IVAL(*Parm(name), 0)
#define IntegerParm0(name) IVAL(*Parm(name), 0)
#define IntegerParm1(name) IVAL(*Parm(name), 1)
#define NOT_REQUESTED  "not-requested"
#define ONELINE  24		/* one tutor value line for tutor */ 
#define VALUE_MAXVAL	200	/* because some scripts actually use such */


    static TEXT	*null_string[] = {""};	
    static TEXT *white[] = {"white"};
    static TEXT *black[] = {"black"};
    static TEXT *fixed[] = {"fixed"};
    static TEXT *arrow[] = {"arrow"};
    static TEXT *root[] = {"Root"};
    static TEXT *no_request[] = {NOT_REQUESTED};
    static TAEINT five[] = {5};
    static TAEINT one[] = {1};
    static TEXT *copy[] = {"copy"};
    static TEXT *set[] = {"set"};
    static TEXT *absolute[] = {"absolute"};
    static TAEINT zero[] = {0, 0};
    static TAEINT all[] = {-1};
#ifdef VMS
    static TEXT *xsvrHost[] = {"DECW$DISPLAY"};
#else
    static TEXT *xsvrHost[] = {"$DISPLAY"};
#endif
    static TEXT *textZero[] = {"0"};
    static TAEINT minus1[] = {-1, -1};
    static TEXT *tcp[] = {"TCP"};
    static TEXT *visible[] = {"visible"};
    static TEXT *repeated[] = {"repeated"};
    static TEXT *minMaxMin[] = {"Min-Max-Min"};

    BEGIN_VALIDS(validNewStates, 6)
	"preferred", "visible", "invisible",
	"iconic", "fast iconic", "modal"   
    END_VALIDS

    BEGIN_VALIDS(validSetStates, 5)
	"preferred", "visible", "invisible",
	"iconic", "fast iconic"   
    END_VALIDS

    BEGIN_VALIDS(validCycles, 2)
	"repeated", "once"
    END_VALIDS

    BEGIN_VALIDS(validOrders, 4)
	"Min-Max", "Max-Min", "Max-Min-Max",
	"Min-Max-Min"
    END_VALIDS

    BEGIN_VALIDS(validFuncs, 16)
	"clear", "and", "andReverse", "copy",
        "andInverted", "noop", "xor", "or",
	"nor", "equiv", "invert", "orReverse",
	"copyInverted", "orInverted", "nand", "set"   
    END_VALIDS

    BEGIN_VALIDS(netValid, 2)
	"DECNET", "TCP" 
    END_VALIDS


    static struct RESIDVAR sleepParms[] = /* sleep 			*/
	{
/* name    type        k m maxc      size    dc val      dvp */

"SECONDS", V_INTEGER,  0,1,   1,        0,    1, NULL,   (GENPTR)one
	};

    static struct RESIDVAR init[] = 	/* window-init			*/
	{
/* name    type      k m maxc      size    dc val      dvp*/

"HOST",    V_STRING, 0,1,   1,   ONELINE,  1, NULL,   (GENPTR)xsvrHost,
"DISPLAY", V_STRING, 0,1,   1,   ONELINE,  1, NULL,   (GENPTR)textZero,
"PROTOCOL",V_STRING, 0,1,   1,   ONELINE,  1, (GENPTR)&netValid,
						      (GENPTR)tcp
	};

    static struct RESIDVAR info[] = 	/* window-info			*/
	{
/* name    type      k m maxc      size    dc  val      dvp */

"NAME",    V_STRING, 0,1,   1, 2*ONELINE,   1,  NULL,   (GENPTR)root, 
"ORIGIN",  V_STRING, 0,1,   1,   ONELINE,   1,  NULL,   (GENPTR)no_request,
"SIZE",    V_STRING, 0,1,   1,   ONELINE,   1,  NULL,   (GENPTR)no_request
	};

    static struct RESIDVAR list[] = 	/* window-list			*/
	{
"NAME",  V_STRING, 0,1,     1, 2*ONELINE,   1,  NULL,   (GENPTR)root,
"LIST",  V_STRING, 0,1,     1,   ONELINE,   1,  NULL,   (GENPTR)no_request
	};

    static struct RESIDVAR create[] =	/* window-create		*/
	{
/* name    type      k m maxc      size     dc val      dvp*/

"NAME",   V_STRING,  0,1,   1,  2*ONELINE,  -1, NULL,   NULL,
"TITLE",  V_STRING,  0,0,   1,    ONELINE,   0, NULL,   NULL,
"ORIGIN", V_INTEGER, 0,2,   2,  	0,   2, NULL,   (GENPTR)zero,
"SIZE",   V_INTEGER, 0,2,   2,          0,   2, NULL,   (GENPTR)zero,
"BG",     V_STRING,  0,1,   1,    ONELINE,   1, NULL,   (GENPTR)white,
"FG",     V_STRING,  0,1,   1,    ONELINE,   1, NULL,   (GENPTR)black, 
"BORDER", V_INTEGER, 0,1,   1,  	0,   1, NULL,   (GENPTR)five, 
"FONT",   V_STRING,  0,1,   1,    ONELINE,   1, NULL,   (GENPTR)fixed,
"CURSOR", V_STRING,  0,1,   1,    ONELINE,   1, NULL,   (GENPTR)arrow
	};

    static struct RESIDVAR name[] =	/* for single parm commands  */
	{
/* name    type      k m maxc      size     dc val      dvp*/

"NAME",   V_STRING,  0,1,   1, 2*ONELINE,    -1, NULL,   NULL
	};

    static struct RESIDVAR modify[] =	/* window-modify             */
	{
/* name    type      k m maxc      size     dc val      dvp*/

"NAME",   V_STRING,  0,1,   1,  2*ONELINE,  -1, NULL,   NULL,
"ORIGIN", V_INTEGER, 0,0,   2,          0,   0, NULL,   NULL,
"WIDTH",  V_INTEGER, 0,0,   1,          0,   0, NULL,   NULL, 
"HEIGHT", V_INTEGER, 0,0,   1,          0,   0, NULL,   NULL
	};

    static struct RESIDVAR draw[] = 	/* window-draw			*/
	{
/* name    type      k m maxc      size     dc  val     dvp */

"NAME",  V_STRING,   0,1,   1,  2*ONELINE,  -1, NULL,    NULL,
"X",     V_INTEGER,  0,1,INT_MAXVAL,    0,  -1, NULL,    NULL,
"Y",     V_INTEGER,  0,1,INT_MAXVAL,    0,  -1, NULL,    NULL,
"FLAGS", V_INTEGER,  0,1,INT_MAXVAL,    0,   1, NULL,    (GENPTR)zero,      
"WIDTH", V_INTEGER,  0,1,     1,        0,   1, NULL,    (GENPTR)one,
"HEIGHT",V_INTEGER,  0,1,     1,        0,   1, NULL,    (GENPTR)one,
"COLOR", V_STRING,   0,1,     1,  ONELINE,   1, NULL,    (GENPTR)black,
"FUNC",  V_STRING,   0,1,     1,  ONELINE,   1, (GENPTR)&validFuncs,    
						         (GENPTR)copy,
"PLANES",V_INTEGER,  0,1,     1,        0,   1, NULL,    (GENPTR)all 
	};

    static struct RESIDVAR textParms[] = 	/* window-text	*/
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"NAME",  V_STRING,   0,1,    1, 2*ONELINE,   -1, NULL,   NULL, 
"FONT",  V_STRING,   0,1,    1,   ONELINE,    1, NULL,   (GENPTR)fixed,
"ORIGIN", V_INTEGER, 0,1,    2,         0,    2, NULL,   (GENPTR)minus1,
"STRING", V_STRING,  0,1,    1, STRINGSIZ,   -1, NULL,   NULL
	};

/*	Parameters for Wpt_ Commands...		*/


static struct RESIDVAR PWpt_Panel [] = 	/* WPT-PANEL */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"VIEW"  ,V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL ,
"TARGET",V_STRING,   0,1,    1,     ONELINE,    1, NULL,   (GENPTR)null_string,
"PANEL", V_STRING,   0,1,    1,     ONELINE,    1, NULL,   (GENPTR)null_string, 
"STATE", V_STRING,   0,1,    1,     ONELINE,    1, (GENPTR)&validNewStates,   
							(GENPTR)visible
	};

static struct RESIDVAR PWpt_SetPanelState[] = 	/* WPT-SETPANELSTATE  */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"STATE", V_STRING,   0,1,    1,     ONELINE,    1, (GENPTR)&validSetStates,   
							(GENPTR)null_string
	};

static struct RESIDVAR PWpt_Event [] = 	/* WPT-EVENT	*/
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL,
"PANEL", V_STRING,   0,1,    1,     ONELINE,    1, NULL,   (GENPTR)no_request,
"TIMEOUT", V_INTEGER,   0,1, 1,           0,    1, NULL,   (GENPTR)zero
	};

static struct RESIDVAR PWpt_panelName[] = 	/* WPT-ERASE, ...	*/
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    INT_MAXVAL,  ONELINE,   -1, NULL,   NULL
	};

static struct RESIDVAR PWpt_Message [] = 	/* WPT-MESSAGE	*/
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"MESSAGE",V_STRING,  0,1,    100, STRINGSIZ,   -1, NULL,   NULL 
	};

static struct RESIDVAR PWpt_Rehearse[] = 	/* WPT-REHEARSE  */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"INTERVAL", V_INTEGER,  0,1, 1,       0,      1, NULL,   (GENPTR)one, 
"ORDER",  V_STRING,  0,1,    1,    ONELINE, 1, (GENPTR)&validOrders, 
						    	(GENPTR)minMaxMin,
"CYCLES",   V_STRING,  0,1,    1,    ONELINE, 1, (GENPTR)&validCycles,
							(GENPTR)repeated 
	};

static struct RESIDVAR PWpt_Reject[] = 	/* WPT-REJECT  */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL , 
"MESSAGE",V_STRING,  0,1,    20,  STRINGSIZ,   -1, NULL,   NULL 
	};

static struct RESIDVAR PWpt_ParmUp[] = 	/* WPT-PARMUPDATE  */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL
	};

static struct RESIDVAR PWpt_ViewUp[] = 	/* WPT-VIEWUPDATE */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL , 
"VIEW",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL
	};

static struct RESIDVAR PWpt_CloseDisplay[] = 	/* WPT-CLOSEDISPLAY  */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"DISPLAY", V_INTEGER,  0,1, 1,       0,      1, NULL,   (GENPTR)one
	};


/* 	wptscript RESIDVARs  	*/

static struct RESIDVAR ScriptIntg[] = 		/* WPTSCRIPT-INTG */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL , 
"VALUE", V_INTEGER,  0,0,  VALUE_MAXVAL,  0,    0, NULL,   NULL 
	};

static struct RESIDVAR ScriptReal[] = 		/* WPTSCRIPT-REAL */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL , 
"VALUE", V_REAL,     0,0,  VALUE_MAXVAL,  0,    0, NULL,   NULL
	};

static struct RESIDVAR ScriptString[] = 	/* WPTSCRIPT-STRING */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL , 
"VALUE", V_STRING,   0,0, VALUE_MAXVAL,MAXSTRSIZ,0, NULL,   NULL 
	};

static struct RESIDVAR ScriptSelect[] = 	/* WPTSCRIPT-SELECT */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,0,    1,     ONELINE,    0, NULL,   NULL 
	};

static struct RESIDVAR ScriptMove[] = 	/* WPTSCRIPT-MOVE */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,0,    1,     ONELINE,    0, NULL,   NULL , 
"ORIGIN",V_INTEGER,  0,2,    2,           0,   -1, NULL,   NULL
	};

static struct RESIDVAR ScriptResize[] = 	/* WPTSCRIPT-RESIZE */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"PANEL", V_STRING,   0,1,    1,     ONELINE,   -1, NULL,   NULL, 
"PARM",  V_STRING,   0,0,    1,     ONELINE,    0, NULL,   NULL , 
"ORIGIN",V_INTEGER,  0,2,    2,           0,   -1, NULL,   NULL, 
"SIZE",  V_INTEGER,  0,2,    2,           0,   -1, NULL,   NULL
	};

static struct RESIDVAR ScriptSetJob[] = 	/* WPTSCRIPT-SETJOB */
	{
/* name    type      k m  maxc      size     dc  val      dvp */
 
"JOB", V_STRING,     0,1,    1,     ONELINE, -1, NULL,   NULL 
	};


/*	static data for Wpt_ Commands		*/

static Id    PanelCollection = NULL;	/* collection of panels		    */

struct PANEL 
    {
    Id		panelId;		/* Wpt_Panel id of panel     */
    TEXT	name[STRINGSIZ+1];	/* user-assigned panel name */
    Id		target;			/* target Vm object	    */
    Id		view;			/* view Vm object	    */
    TEXT	tarname[STRINGSIZ+1];	/* name of target variable  */
    };


CODE	sleep_do();
CODE	window_init_do (), window_create_do(), window_clear_do (),
	window_clear_do (), window_delete_do (), window_modify_do (),
	window_raise_do (), window_lower_do (), window_draw_do(), 
	window_list_do(), window_text_do(), window_colormap_do(),
	window_info_do(); 

CODE	Wpt_Panel_do (), Wpt_Event_do (), Wpt_CloseDisplay_do ();
CODE	Wpt_Combined_do (), Wpt_Message_do ();
CODE    Wpt_Reject_do (), Wpt_ParmUpdate_do (), Wpt_SetPanelState_do ();
CODE	Wpt_ViewUpdate_do (), Wpt_Rehearse_do ();
CODE 	Wpt_Script_do (), Wpt_SetJob_do ();

#define Y_WINDOW  (Y_GENERAL | Y_QUALIF)

    GLOBAL struct ITRCMD windowcmd[] = 		/* window commands  */
	{
{5, "SLEEP",    "",        Y_GENERAL,   I_NPM(sleepParms), sleepParms,sleep_do},
{4, "WINDOW",	"INIT",    Y_GENERAL,	I_NPM(init),  init,    window_init_do},
{4, "WINDOW",	"CREATE",   Y_WINDOW,	I_NPM(create),create, window_create_do},
{4, "WINDOW",	"CLEAR",   Y_GENERAL,	I_NPM(name),  name,    window_clear_do},
{4, "WINDOW","COLORMAP",   Y_GENERAL,      0,      NULL, window_colormap_do},
{4, "WINDOW",	"DELETE",  Y_GENERAL,	I_NPM(name),  name,   window_delete_do},
{4, "WINDOW",   "DRAW",    Y_GENERAL,   I_NPM(draw),  draw,   window_draw_do},
{4, "WINDOW",   "TEXT",    Y_GENERAL,   I_NPM(textParms), 
						    textParms, window_text_do}, 
{4, "WINDOW",   "INFO",    Y_GENERAL,   I_NPM(info),  info,   window_info_do},
{4, "WINDOW",   "LIST",    Y_GENERAL,   I_NPM(list),  list,   window_list_do},
{4, "WINDOW",	"MODIFY",   Y_WINDOW,	I_NPM(modify),modify, window_modify_do},
{4, "WINDOW",	"RAISE",   Y_GENERAL,	I_NPM(name),  name,    window_raise_do},
{4, "WINDOW",	"LOWER",   Y_GENERAL,	I_NPM(name),  name,    window_lower_do},
{3, "WPT",	"BEGINWAIT",Y_GENERAL,  I_NPM(PWpt_panelName), 
						PWpt_panelName, Wpt_Combined_do},
{3, "WPT",	"CLOSEDISPLAY",    Y_GENERAL,  I_NPM(PWpt_CloseDisplay),
					PWpt_CloseDisplay, Wpt_CloseDisplay_do},
{3, "WPT",	"ENDWAIT",   Y_GENERAL,I_NPM(PWpt_panelName), 
						PWpt_panelName, Wpt_Combined_do},
{3, "WPT",	"ERASE",   Y_GENERAL,	I_NPM(PWpt_panelName), 
					PWpt_panelName, Wpt_Combined_do},
{3, "WPT",	"EVENT",   Y_GENERAL,	I_NPM(PWpt_Event), 
						PWpt_Event, Wpt_Event_do},
{3, "WPT",	"INIT",    Y_GENERAL,	I_NPM(init),  init,    window_init_do},
{3, "WPT",	"MESSAGE", Y_GENERAL,	I_NPM(PWpt_Message), PWpt_Message, 
							 Wpt_Message_do},
{3, "WPT",	"PANEL",   Y_GENERAL,	I_NPM(PWpt_Panel), 
						PWpt_Panel, Wpt_Panel_do},
{3, "WPT",	"REJECT",   Y_GENERAL,	I_NPM(PWpt_Reject), 
						PWpt_Reject,    Wpt_Reject_do},
{3, "WPT",	"REHEARSE",   Y_GENERAL, I_NPM(PWpt_Rehearse), 
						PWpt_Rehearse, Wpt_Rehearse_do},
{4, "WPT",	"PARMUPDATE",   Y_GENERAL,	I_NPM(PWpt_ParmUp), 
					     PWpt_ParmUp,    Wpt_ParmUpdate_do},
{9, "WPT",	"SETPANELSTATE",   Y_GENERAL,	I_NPM(PWpt_SetPanelState), 
				PWpt_SetPanelState,  Wpt_SetPanelState_do},
{4, "WPT",	"VIEWUPDATE",   Y_GENERAL,	I_NPM(PWpt_ViewUp), 
					     PWpt_ViewUp,    Wpt_ViewUpdate_do},
{4, "WPTSCRIPT","INTG",   Y_GENERAL,	I_NPM(ScriptIntg), 
					     ScriptIntg,    Wpt_Script_do},
{4, "WPTSCRIPT","REAL",   Y_GENERAL,	I_NPM(ScriptReal),
					     ScriptReal,    Wpt_Script_do},
{4, "WPTSCRIPT","STRING",   Y_DEFSUB|Y_GENERAL,	
					I_NPM(ScriptString), 
					     ScriptString,    Wpt_Script_do},
{4, "WPTSCRIPT","SELECT",   Y_GENERAL,	I_NPM(ScriptSelect), 
					     ScriptSelect,    Wpt_Script_do},
{4, "WPTSCRIPT","MOVE",   Y_GENERAL,	I_NPM(ScriptMove), 
					     ScriptMove,      Wpt_Script_do},
{4, "WPTSCRIPT","RESIZE",   Y_GENERAL,	I_NPM(ScriptResize), 
					     ScriptResize,    Wpt_Script_do},
{4, "WPTSCRIPT","SETJOB",   Y_GENERAL,	I_NPM(ScriptSetJob), 
					     ScriptSetJob,    Wpt_SetJob_do},
{0, ""}	/* TERMINATOR ENTRY: REQUIRED AT END */
	};


/*	sleep_do.
 *
 */

    FUNCTION CODE sleep_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
#ifdef VAX_VMS
    tmmsg (PROCFAIL, "Command not yet implemented", "TAE-NOCOMMAND");
#endif
#ifdef UNIX
    sleep (IntegerParm("seconds"));
#endif
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *
 */
    FUNCTION VOID Wpt_CloseDisplay_do(cpctx, npctx)
	
    struct CONTXT *cpctx;
    struct CONTXT *npctx;

    {
#ifdef XWINDOWS
    CheckInit;
    /*
     * TBD Implement this right by using sent in DisplayId.
     */
    XCloseDisplay(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
     return (DO_CHECK);
    }

/*
 *
 */

    FUNCTION CODE window_init_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    TEXT env_string[STRINGSIZ+1];
    TEXT string[STRINGSIZ+1];
    TEXT *separator;
    TEXT *s_save ();
    TEXT *getenv();
#ifdef VMS
    TEXT *envptr = getenv ("DECW$DISPLAY");
#else
    TEXT *envptr = getenv ("DISPLAY");
#endif
    struct VARIABLE *ptr;
    BOOL defflag=TRUE;
    void ReqErrorMessage();

    ReqErrorMessage();
    if (DisplayId != NULL)			/* already init'd?	*/
	return (DO_CHECK);
    
    ptr = Parm("host");
    defflag = defflag && (BOOL)(*ptr).v_default;
    s_copy (StringParm("host"), string);

    ptr = Parm("protocol");
    defflag = defflag && (BOOL)(*ptr).v_default;
    separator = (s_equal(StringParm("protocol") , "TCP")) ? ":" : "::"; 
    s_append (separator, string);

    ptr = Parm("display");
    defflag = defflag && (BOOL)(*ptr).v_default;
    s_append (StringParm("display"), string);
#ifdef VMS
    if (s_equal(StringParm("host"), "DECW$DISPLAY"))	/* special host name: */
#else
    if (s_equal(StringParm("host"), "$DISPLAY"))	/* special host name: */
#endif
	{
	if (envptr)
	    s_copy (envptr, string);			/* "use environment"  */
	}
    DisplayId = Wpt_Init (string);
    if (DisplayId == NULL)
	{
	tmmsg (PROCFAIL, "Cannot initialize display screen (%s).", 
			"TAE-OPENDISPLAY", string);
        return (DO_CHECK);
	}
    /* break from Wpt_NextEvent upon operator interrupt */
    Wpt_EnableInterrupt(TRUE);				/* Honor interrupts */
    CurrentScreen = DefaultScreen(DisplayId);
#ifndef VMS
    sprintf (env_string, "DISPLAY=%s", string);		/* help app process */
    putenv (s_save(env_string));
#endif
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *	window_create_do.
 *
 *	TBD: handle qualifiers: NORUN and RESTORE and SAVE
 *		 (whoops, must be done by TM before updtab!!)
 *	TBD: did not do TITLE and FOREGROUND and CURSOR
 *	TBD: allow spec of border pixmap as parm?
 *	TBD: user-defined pixmaps for bg for other than white/black
 *
 */

    FUNCTION CODE window_create_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window XCreateSimpleWindow (), window, parentWindow;
    TEXT *bgName, *windowName;
    TEXT parentName[STRINGSIZ+1];
    TEXT simpName[STRINGSIZ+1];			/* simple window name */ 
    XColor background, border, hardwarecolor;
    struct VARIABLE *vsize;
    XWindowAttributes info;
    COUNT  width, height;
    Status status;

    CheckInit;
    windowName = StringParm("name");
    extractParent (windowName, parentName, simpName);      /* crack name */
    if (NULLSTR(parentName))
	parentWindow = ROOT_WINDOW;
    else
        parentWindow = Wpt_ConvertName (parentName);
    if (parentWindow == NULL)
        {
	tmmsg (PROCFAIL, "Proposed parent window '%s' does not exist.", 
		"TAE-BADPARENT", parentName);
	return (DO_CHECK);
	}    
    window = Wpt_ConvertNameP (simpName, parentWindow);
    if (window != NULL)
	{
	tmmsg (PROCFAIL, "Window '%s' already exists.", "TAE-WINDEXISTS",
		windowName);
	return (DO_CHECK);
	}
    status = XGetWindowAttributes(DisplayId, parentWindow, &info);		/* pg 14 */
    bgName = StringParm("bg");
    XAllocNamedColor ( DisplayId, info.colormap, bgName, &background,
                       &hardwarecolor );
    if ( s_equal ( "white", bgName ) )
        {
        XAllocNamedColor ( DisplayId, info.colormap, "black", &border,
                       &hardwarecolor );
        }
    else
        {
        XAllocNamedColor ( DisplayId, info.colormap, "white", &border,
                       &hardwarecolor );
        }
    vsize = Parm("size");
    if (IVAL(*vsize,0) <= 0)
	width = info.width/2;			/* make resonable default */
    else
	width = IVAL(*vsize,0);
    if (IVAL(*vsize,1) <= 0)
	height = info.height/2;			/* make reasonable default */
    else
	height = IVAL(*vsize,1);

    window = XCreateSimpleWindow (
		DisplayId,
		parentWindow,			/* pg 6 */
		IntegerParm0("origin"), IntegerParm1("origin"),
  		width, height,		
		IntegerParm("border"), border.pixel,
    		background.pixel);						
    if (window == NULL)
        {
        tmmsg (PROCFAIL, "TAE-WINDOWCREATE", "Cannot create window '%s'.",
    	       windowName);
        return (DO_CHECK);    		
        }
    XMapRaised (DisplayId, window);		/* display the window pg 11 */
    XStoreName (DisplayId, window, simpName);	/* pg 16 */
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *
 */

    FUNCTION CODE window_clear_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window window;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
        return (DO_CHECK);		/* convertName does the tmmsg */
    XClearWindow (DisplayId, window);
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*	window_colormap_do.  display color map.
 *
 *	TBD: avoid hardcode to 256 colors.  Do some headers.
 */

     FUNCTION CODE window_colormap_do ()

     {
#ifdef XWINDOWS
     Status status;
     COUNT  pixel;
     XColor color;
     TEXT string[STRINGSIZ+1];
     XWindowAttributes wininfo;

     CheckInit;
     XGetWindowAttributes ( DisplayId, ROOT_WINDOW, &wininfo );
     for (pixel = 0;  pixel < 256; pixel++)
	{
	color.pixel = pixel;
	status = XQueryColor (DisplayId, wininfo.colormap, &color);
	sprintf (string, "%d (10) %x (16):  %d %d %d", pixel, pixel, 
		color.red, color.green, color.blue);
	put_stdout (string);
	}
#else
    notImplemented ();
#endif /* XWINDOWS */
     return (DO_CHECK);
     }

/*
 *
 */

    FUNCTION CODE window_delete_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window window;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
        return (DO_CHECK);		/* convertName does the tmmsg */
    XDestroyWindow (DisplayId, window);
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*	window_draw_do.
 *
 *
 */

    FUNCTION CODE window_draw_do (cpctx, npctx)

	struct CONTXT *cpctx;
	struct CONTXT *npctx;

    {
#ifdef XWINDOWS
    XPoint vertices[INT_MAXVAL];
    COUNT i, n;
    struct VARIABLE *vx, *vy, *vflags;
    XColor hardwareColor, dummyColor;
    Status status;
    Window window;
    XWindowAttributes wininfo;

    CheckInit;
    window = convertName(StringParm("name"));
    if (window == NULL)
	return (DO_CHECK);
    XGetWindowAttributes ( DisplayId, ROOT_WINDOW, &wininfo );
    vx= Parm("x");
    vy = Parm("y");
    vflags = Parm("flags");
    vertices[0].x = IVAL(*vx,0);
    vertices[0].y = IVAL(*vy,0);
/*
    vertices[0].flags = IVAL(*vflags,0); 
*/
    n = (*vx).v_count;
    for (i=1; i < n; i++)
	{
	vertices[i].x = IVAL(*vx,i);
	vertices[i].y = (i < (*vy).v_count) ? IVAL(*vy,i) : 0;
/*
	vertices[i].flags = (i < (*vflags).v_count) ? 
				IVAL(*vflags,i) : vertices[i-1].flags;
*/
        }
    status = XAllocNamedColor ( DisplayId, wininfo.colormap, 
                      StringParm("color"), &hardwareColor, &dummyColor); 
    if (status == 0)
	{
	tmmsg (PROCFAIL, "Unknown color '%s'.", "TAE-NOCOLOR", 
			  StringParm("color"));
 	return (DO_CHECK);
	}
  
/*    XDraw (window, vertices, n, IntegerParm("width"), IntegerParm("height"),
		hardwareColor.pixel, 
		translateFunc(StringParm("func")),
		IntegerParm("planes")); 
*/
    XDrawLines ( DisplayId, window, DefaultGC(DisplayId,CurrentScreen), vertices, n,
                 CoordModeOrigin );
    XFlush(DisplayId);
#else
    notImplemented ();
#endif
    return (DO_CHECK);
    }



    FUNCTION static CODE translateFunc (func)
	TEXT	func[];		/* in: name of bitblt function */

    {
 
    COUNT i;

    for (i=0; i < 16;  i++)
        {
	if (s_equal (func, validFuncs.slist[i].string))
	    return (i);
	}
    return (0);
    }

/*	window_info_do.
 *
 *	TBD: deliver parent name?
 *
 */

    FUNCTION CODE window_info_do (procctx, npctx)

    	struct CONTXT	*procctx;	/* proc context */
        struct CONTXT	*npctx;		/* cmd  context */

    {
#ifdef XWINDOWS
    XWindowAttributes	info;
    Window 	window;
    struct PANEL 	*panel;
    Status	status;
    TAEINT	origin_vv[2], size_vv[2];
    TEXT	local_name[STRINGSIZ+1];    
    TEXT        *windowName;
    CheckInit;
    
    windowName = StringParm("name");
    panel = (struct PANEL *)Co_Find ( PanelCollection, windowName );    
    if (panel == NULL)
	return (DO_CHECK);
    window = Wpt_PanelWindow ( (*panel).panelId );
    if (window == NULL)
	return (DO_CHECK);
    status = XGetWindowAttributes(DisplayId, window, &info);
    if (status == 0)
        {
        tmmsg (PROCFAIL, "Cannot query window '%s'.", 
    				"TAE-BADQUERY", StringParm("name"));
        return (DO_CHECK);
        }
    origin_vv[0] = info.x;  
    origin_vv[1] = info.y;
    size_vv[0] = info.width; 
    size_vv[1] = info.height;
    setOutputValue (procctx, StringParm("ORIGIN"), origin_vv, 2, V_INTEGER);
    setOutputValue (procctx, StringParm("SIZE"), size_vv, 2, V_INTEGER);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }


    FUNCTION static CODE setOutputValue (procctx, name, vv, count, type)

    	struct CONTXT	*procctx;	/* in: proc context 	*/
        TEXT		name[];		/* in: parm name	*/
        GENPTR		vv;		/* in: value vector	*/            	
    	FUNINT		count;		/* in: value count	*/
    	FUNINT		type;		/* in: value type	*/

    {
    struct VARIABLE *v;
    CODE	code;

    if (NULLSTR(name) || s_equal (name, NOT_REQUESTED))
        return (SUCCESS);	
    v = search (name, procctx);		/* find target variable	*/
    if (v == NULL)
        {
        tmmsg (PROCFAIL, "Variable '%s' does not exist.", "TAE-BADNAME",
    		name);
        return (FAIL);
        }	
#ifdef CHECK_VALUE
    code = chk_vector (v, type, vv, count, FALSE);/* check value against target */
    if (code != SUCCESS)
    	{
	if (code == V_BADTYPE)
	    tmmsg (PROCFAIL, "Variable '%s' has improper type.", "TAE-BADTYPE", 
			 name);
        else if (code == V_BADCOUNT)
	    tmmsg (PROCFAIL, "Variable '%s' has improper count.", 
		   "TAE-BADCOUNT", name);
        else if (code == V_BADSIZE)
	    tmmsg (PROCFAIL, "A value for variable '%s' is too large.", 
		   "TAE-BADSIZE", name);
        else if (code == V_BADVALID)
	    {
	    tmmsg (PROCFAIL, "Invalid value for variable '%s'.", 
		   "TAE-BADVALID", name);
	    }
        else if (code == V_BADFILE)
	   tmmsg (PROCFAIL, "File mode verification failure for variable '%s'.",
		  "TAE-BADFILE", name);
        else if (code == V_BADAMBIG)
	    tmmsg (PROCFAIL, "Ambiguous value for variable '%s'.", 
		  "TAE-BADAMBIG", name);
        else
	    tmmsg (PROCFAIL, "Bad value for variable '%s'.", 
		  "TAE-FAIL", name);
	return (FAIL);
	}
#endif
    set_value (v, vv, count);			/* set the value	*/
    return (SUCCESS);            
    }        

/*	window_list_do.
 *
 *
 */
    FUNCTION CODE window_list_do (cpctx, npctx)
	struct CONTXT *cpctx;
	struct CONTXT *npctx;

    {
#ifdef XWINDOWS
    IMPORT  TEXT	*s_save();
    Window  *windowList;
    Window  window;
    Window  root, parent;
    int n,i;
    TEXT *windowName;
    BOOL   displayRequested;
    TEXT *list_vv[INT_MAXVAL];

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
	return (DO_CHECK);
    displayRequested = s_equal (StringParm("list"), NOT_REQUESTED);
    XQueryTree (DisplayId, window, 
                &root, &parent, &windowList, &n);
    n = (n < INT_MAXVAL) ? n : INT_MAXVAL;
    for (i=0; i < n; i++)
	{
	XFetchName (DisplayId, windowList[i], &windowName);
	if (windowName == NULL)
	    list_vv[i] = s_save ("NO NAME");
	else
	    list_vv[i] = windowName;
	} 
    setOutputValue (cpctx, StringParm("list"), list_vv, n, V_STRING); 
    free (windowList);
    for (i=0; i < n; i++)
	{
	if (displayRequested)
	    put_stdout (list_vv[i]);
	free (list_vv[i]);
	}
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *
 */

    FUNCTION CODE window_modify_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window window;
    COUNT   x, y, width, height;
    struct VARIABLE *v;
    XWindowAttributes info;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
        return (DO_CHECK);
    XGetWindowAttributes(DisplayId, window, &info);		/* get window info pg 14*/
    v = Parm("origin");
    if ((*v).v_count  == 2)		/* is move desired?	pg 12	*/
	{
	x = IVAL(*v, 0);
	y = IVAL(*v, 1);
	}
    else				/* use existing location	*/
	{
	x = info.x;
	y = info.x;
        }
    v = Parm("width");
    if ((*v).v_count >= 1)		/* was width specified?		*/
	width = IVAL(*v, 0);
    else
	width = info.width;
    v = Parm("height");			
    if ((*v).v_count >= 1)		/* was height specified?	*/
	height = IVAL(*v, 0);
    else
	height = info.height;
    XMoveResizeWindow (DisplayId, window, x, y, width, height);	/* pg 13	*/
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *
 */

    FUNCTION CODE window_raise_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window window;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
        return (DO_CHECK);		/* convertName does the tmmsg */
    XRaiseWindow (DisplayId, window);
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*	window_text_do.
 *
 *	TBD: major bug: fonts get copied in the X server and the server
 *		will run out of memory !!!
 *	TBD: do multi-valued string as multiple lines
 *	TBD: allow specification by user of fg and bg color
 */

    FUNCTION CODE window_text_do (cpctx, npctx)

	struct CONTXT *cpctx;
	struct CONTXT *npctx;

    {
#ifdef XWINDOWS
    Window window;
    Font font;
    TEXT *string;
    struct VARIABLE *vorigin;
    XWindowAttributes info;
    COUNT   x, y;
    Status status;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
	return (DO_CHECK); 
    font = XLoadFont (DisplayId, StringParm("font"));
    if (font == NULL)
	{
	tmmsg (PROCFAIL, "Unknown font '%s'.", "TAE-NOFONT", 
			StringParm("font"));
	return (DO_CHECK);
	}
    vorigin = Parm("origin");
    status = XGetWindowAttributes(DisplayId, window, &info);
    if (IVAL(*vorigin,0) < 0)
	x = info.width/10;			/* default: start at  left */
    else
	x = IVAL(*vorigin,0);
    if (IVAL(*vorigin,1) < 0)
        y = info.height/2;			/* default: middle	    */	
    else
	y = IVAL(*vorigin,1);
    string = StringParm("string");	
/*
    XText (window, x, y,  
	   string, s_length(string), font, BlackPixel, WhitePixel);
*/
    XDrawImageString ( DisplayId, window, DefaultGC(DisplayId,CurrentScreen), x,y,
	   string, s_length(string) );
      
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*
 *
 */

    FUNCTION CODE window_lower_do (cpctx, npctx)

    struct CONTXT	*cpctx;		/* in:  containing proc context    */
    struct CONTXT	*npctx;		/* in:  proc ctx for CONT cmd line */

    {
#ifdef XWINDOWS
    Window window;

    CheckInit;
    window = convertName (StringParm("name"));
    if (window == NULL)
        return (DO_CHECK);		/* convertName does the tmmsg */
    XLowerWindow (DisplayId, window);
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }


/*	Wpt_Panel_do.   Create panel on screen.	
 *
 */

    FUNCTION CODE Wpt_Panel_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL 	*panel;
    struct VARIABLE 	*viewv, *targetv, *v;
    struct VARIABLE	*Vm_Find();
    Id			Wpt_NewPanel ();
    TEXT		*panelName;
    struct VARIABLE	*panelNameV;
    TEXT		*string;
    TAEINT		state;

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) tae_alloc (1, sizeof (struct PANEL));
    (*panel).target = Vm_New (P_CONT);
    (*panel).view = Vm_New (P_CONT);

    /*	Make a Vm object to be the target parmset	*/

    s_copy (StringParm("target"), (*panel).tarname);
    if (!NULLSTR((*panel).tarname))
        {
        targetv = search (StringParm("target"), cpctx);
        if (targetv == NULL)
            {
	    tmmsg (PROCFAIL, "Variable '%s' not found.", "TAE-NOTARGET", 
    			 StringParm("target"));
	    goto cleanup;
            }
        for (v=(*targetv).v_qualst.link; v != NULL;  v=(*v).v_link)
	    Vm_CopyVarToVm ((*panel).target, v);	
	}

    /*	Make a Vm object to be the view parmset	*/

    viewv = search (StringParm("view"), cpctx);
    if (viewv == NULL)
        {
	tmmsg (PROCFAIL, "Variable '%s' not found.", "TAE-NOVIEW",
    			 StringParm("view"));
	goto cleanup;
        }
    for (v=(*viewv).v_qualst.link; v != NULL;  v=(*v).v_link)
	Vm_CopyVarToVm ((*panel).view, v);	
    panelNameV = Vm_Find ((*panel).view, "_panel.name");		
    panelName = StringParm("panel");
    if (NULLSTR(panelName))			/* if no name on command  */
	{
        if (panelNameV == NULL || (*panelNameV).v_count == 0)
	    (*panel).name[0] = EOS;		/* panel has no name !!   */
        else
	    s_copy (SVAL(*panelNameV,0), (*panel).name); /* use _panel.name  */
	}
    else
	{
        s_copy (panelName, (*panel).name);	/* use name from WPT-PANEL */
	if (panelNameV)
	    set_string (panelNameV, panelName); /* let _panel.name = ... */
	}
    if (Co_Find (PanelCollection, (*panel).name) != NULL)
	{
	tmmsg (PROCFAIL, "Duplicate panel name '%s'.", "TAE-DUPPANEL", 
    			 (*panel).name);
	goto cleanup;
	}

    /* Get creation state information. */
    string = StringParm("state");
    if (s_equal(string, "preferred"))
	state = WPT_PREFERRED;
    else if (s_equal(string, "visible"))
	state = WPT_VISIBLE;
    else if (s_equal(string, "invisible"))
	state = WPT_INVISIBLE;
    else if (s_equal(string, "iconic"))
	state = WPT_ICONIC;
    else if (s_equal(string, "fast iconic"))
	state = WPT_FASTICONIC;
    else if (s_equal(string, "modal"))
	state = WPT_MODAL;

    (*panel).panelId = Wpt_NewPanel ("", (*panel).target, 
			 (*panel).view, NULL, panel, state);
    if ((*panel).panelId == NULL)
	{
	tmmsg (PROCFAIL, "Wpt-Panel failure.", "TAE-WPTPANEL");
	goto cleanup;
	}
    Co_Add (PanelCollection, panel, (*panel).name, NULL);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);


#ifdef XWINDOWS
cleanup:
    Vm_Free ((*panel).target);
    Vm_Free ((*panel).view);
    tae_free (panel);
    return (DO_CHECK);	    
#endif
    }

/*	Wpt_Event_do.    Wait until next Wpt_ event.
 *
 */

static struct PANEL *CurPanel = NULL;

    FUNCTION CODE Wpt_Event_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    IMPORT WptEvent wpt_Event;	
    IMPORT BOOL  int_enabled;		/* if TRUE no interrupt: force abort */
    XEvent		xevent;
    struct PANEL	*panel;
    TEXT		*svv[1];
    IMPORT		struct ECB ecbi;	/* TAE operator attn block */
    CODE		updateVariable ();
    unsigned  long	timeoutVal;
    WptEvent 		aWptEvent;	
    BOOL		aTimeout;		/* True if timeout specified */

    CheckInit;

    timeoutVal = IntegerParm("timeout");	   /* resolution in seconds */ 
    timeoutVal = timeoutVal * 1000;  		/* Wpt needs in millisecs   */

    aTimeout = (timeoutVal != 0);

    while (FOREVER)
	{
	if (e_occur (&ecbi))		/* control/c ?	*/
	    {
	    if (!int_enabled)		/* proc interrupt disabled	*/
		return (DO_ABORT);	/* abort the proc		*/
	    else
		{
	        tmmsg (PROCFAIL, "Attention during WPT-EVENT.", "TAE-CONTROLC");
	        return (DO_CHECK);
		}
	    }

        if (aTimeout)
	    Wpt_SetTimeOut(timeoutVal);
	Wpt_NextEvent(&aWptEvent);
    	if (aWptEvent.eventType  == WPT_INTERRUPT_EVENT)
	    {
	    e_set(&ecbi);		/* for check in next cycle  */	
	    continue;
	    }
	else if (aWptEvent.eventType == WPT_WINDOW_EVENT)
	    continue;			/* ignore extraneous window events */
	else
	    break;
	}
    
    
    if (aWptEvent.eventType  == WPT_PARM_EVENT)
	{
    	panel = (struct PANEL *) aWptEvent.p_userContext;
    	svv[0] = (*panel).name;
    	setOutputValue (cpctx, StringParm("panel"), svv, 1, V_STRING);
    	svv[0] = aWptEvent.parmName;
    	setOutputValue (cpctx, StringParm("parm"), svv, 1, V_STRING);
	 
    /*  Move the current parm values from the target Vm object 
	to the qualified variable identified for the panel.
    	Note that the target variable must be re-certified here
        because it may have been deleted since the WPT-PANEL.
    */

    	CurPanel = panel;	/* kludge communication with updateVariable */
    	Vm_ForEach ((*panel).view, updateVariable, cpctx);
	}
    
    else if (aWptEvent.eventType  == WPT_TIMEOUT_EVENT)     /* had a TIMEOUT */
	{
    	svv[0] = "WPT_EVENT";
    	setOutputValue (cpctx, StringParm("panel"), svv, 1, V_STRING);
    	svv[0] = "TIMEOUT";
    	setOutputValue (cpctx, StringParm("parm"), svv, 1, V_STRING);
	}
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

/*	updateVariable.   Vm_ForEach callback.
 *
 *	vvar is a variable from the view block.  If a 
 *	corresponding variable exists in the target
 *	block, then move that variable's value
 *	into the TCL variable designated as 'target'
 *	on the WPT-PANEL command.
 *
 *	Note that we only move variables that are viewed
 *	by the panel.  This allows several panels to view
 *	the same target collection without wiping each 
 *	other out (unless the same variable is viewed twice).
 */
FUNCTION CODE updateVariable (vvar, cpctx)

	struct VARIABLE	*vvar;		/* view variable	*/
	struct CONTXT	*cpctx;		/* proc context		*/

    {
    IMPORT struct VARIABLE *Vm_FindVar();
    IMPORT struct VARIABLE *allvar();
    TEXT qualTarget[STRINGSIZ+1];  /* T.x where T is WPT-PANEL target */
    struct VARIABLE *targetSel;	   /* destination of data 	      */	
    struct VARIABLE *targetv;	   /* destination of data 	      */	
    struct VARIABLE *datav;	   /* source of data 		      */
    struct VARIABLE *qualv;	   /* next qualifier variable         */

    datav = Vm_FindVar ((*CurPanel).target, (*vvar).v_name);
    if (datav == NULL)
	return (NULL);			        /* no related target	  */
    s_copy ((*CurPanel).tarname, qualTarget);	/* start with parent name */
    s_append (".", qualTarget);
    s_append ((*datav).v_name, qualTarget);
    targetv = search (qualTarget, cpctx);
    setOutputValue (cpctx, qualTarget, (*datav).v_cvp, (*datav).v_count,
				       (*datav).v_type);
    if (targetv == NULL)			/* no corresponding var	  */
	return (NULL);

    /*   Copy all qualifiers (if any) */

    for (qualv = (*datav).v_qualst.link; qualv; qualv = (*qualv).v_link)
	{
	TEXT qualTargetName[STRINGSIZ+1]; 
	COUNT i;

        targetSel = lookex (&(*targetv).v_qualst, (*qualv).v_name); 
        if (targetSel == NULL)			/* add in if necessary    */
	    {
	    if ((*qualv).v_type == V_INTEGER)
	        addint (&(*targetv).v_qualst, (*qualv).v_name, 0, 
			(*targetv).v_class);
	    else if ((*qualv).v_type == V_STRING)
    		addstr(&(*targetv).v_qualst, (*qualv).v_name, 
			(*qualv).v_maxc, (*qualv).v_count, 
			(*qualv).v_cvp,   (*targetv).v_class);
	    else if ((*qualv).v_type == V_REAL)
		{				/* no convenience routine */
		struct VARIABLE     *v;	   /* based on addint in tmutil.c */
		TAEFLOAT	    *civ;       /* real value in st       */
  						/* allocate new variable  */
		v = allvar(&(*targetv).v_qualst);
    		s_copy((*qualv).v_name, (*v).v_name);
    		(*v).v_class = (*targetv).v_class;
    		(*v).v_type    = V_REAL;
    		(*v).v_count = (*v).v_minc = (*v).v_maxc = 1;
    		(*v).v_dcount  = 0;
    		(*v).v_default = FALSE;
    		(*v).v_size    = 0;
    		(*v).v_valid = (*v).v_cvp = (*v).v_dvp = NULL;
    		(*v).v_cvp = (GENPTR) allval(v);
    		civ  = (TAEFLOAT *) (*v).v_cvp;
    		*civ = 0.0;
		}
	    else
		continue;
	    }
	if ((*qualv).v_type == V_INTEGER && s_equal((*qualv).v_name, "_SELECT"))
            IVAL(*qualv, 0)++;	/* Special case, Convert to TCL type index */
	i = s_copy(qualTarget, qualTargetName);
	i += s_copy(".", &qualTargetName[i]);
	s_copy ((*qualv).v_name, &qualTargetName[i]);
        setOutputValue (cpctx, qualTargetName, (*qualv).v_cvp, 
		(*qualv).v_count, (*qualv).v_type);
	}
    return (NULL);
    }

/*	updateTarget.   Vm_ForEach callback.
 *
 *	vvar is a variable from the view block.  If a 
 *	corresponding variable exists in the target
 *	block, then copy the qualfier value of the TCL 
 *      variable designated as 'target'	on the WPT-PANEL command.
 *
 */
FUNCTION CODE updateTarget (vvar, cpctx)

	struct VARIABLE	*vvar;		/* view variable	*/
	struct CONTXT	*cpctx;		/* proc context		*/

    {
#ifdef XWINDOWS
    IMPORT struct VARIABLE *Vm_FindVar();
    TEXT qualTarget[STRINGSIZ+1];  /* T.x where T is WPT-PANEL target */
    struct VARIABLE *targetSel;	   /* destination of data 	      */	
    struct VARIABLE *targetv;	   /* destination of data 	      */	
    struct VARIABLE *datavSel;	   /* source of data 		      */
    struct VARIABLE *datav;	   /* source of data 		      */

    datav = Vm_FindVar ((*CurPanel).target, (*vvar).v_name);
    if (datav == NULL)
	return (NULL);			        /* no related target	  */
    s_copy ((*CurPanel).tarname, qualTarget);	/* start with parent name */
    s_append (".", qualTarget);
    s_append ((*datav).v_name, qualTarget);
    targetv = search (qualTarget, cpctx);
    if (targetv == NULL)			/* no corresponding var	  */
	return (NULL);

    if ((*targetv).v_type == V_STRING)		/* a string value */
       Vm_SetString((*CurPanel).target, (*targetv).v_name, (*targetv).v_count, 
	    (TEXT *)(*targetv).v_cvp, P_UPDATE);

    else if ((*targetv).v_type == V_INTEGER)		/* integer value   */
	{
	Vm_SetIntg((*CurPanel).target, (*targetv).v_name, (*targetv).v_count, 
	    (TAEINT *)(*targetv).v_cvp, P_UPDATE);
	}
   else if ((*targetv).v_type == V_REAL)		/* real value   */
	{
	Vm_SetReal((*CurPanel).target, (*targetv).v_name, (*targetv).v_count, 
	    (TAEFLOAT *)(*targetv).v_cvp, P_UPDATE);
	} 

#endif /* XWINDOWS */
    return (NULL);
    }

/*	Wpt_Combined _do.	Erase, beginwait, and endwait.
 * 
 *	Commands that accept only a panel name are combined here
 *	to make "all" easy, etc.
 */

    FUNCTION CODE Wpt_Combined_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    IMPORT VOID		Wpt_PanelErase ();
    struct VARIABLE	*pv;
    COUNT		i;
    static CODE eraseOnePanel (), setWait ();
    TEXT	*subcmd;
    
    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    pv = lookex (&(*npctx).parmst, "panel");
    subcmd = (*npctx).subcmd;
    if ((*pv).v_count == 1 && s_equal (SVAL(*pv,0), "all"))
	if (s_equal (subcmd, "erase"))
            Co_ForEach (PanelCollection, eraseOnePanel, NULL);
	else 
	    Co_ForEach (PanelCollection, setWait, subcmd);
    else
        for (i=0; i < (*pv).v_count; i++)		
	    {
            panel = (struct PANEL *) Co_Find (PanelCollection, SVAL(*pv, i)); 
            if (panel == NULL)
	        tmmsg (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", 
				  SVAL(*pv, i)); 
	    else
		if (s_equal(subcmd, "erase"))
	            eraseOnePanel (panel); 
		else
		    setWait (panel, subcmd);
            }
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }


#ifdef XWINDOWS
FUNCTION static CODE eraseOnePanel (panel)

    struct PANEL *panel;

    {

        Wpt_PanelErase ((*panel).panelId);
        Co_Remove (PanelCollection, (*panel).name);	
        Vm_Free ((*panel).view);
        Vm_Free ((*panel).target);
        tae_free (panel);
        return (NULL);		/* keep the Co_ForEach going */
    }

FUNCTION static CODE setWait (panel, subcmd)	/* begin or end wait mode */

    struct PANEL *panel;
    TEXT	subcmd[];
 
    {
    if (subcmd[0] == 'B')
	Wpt_BeginWait ((*panel).panelId);
    else
	Wpt_EndWait ((*panel).panelId);
    return (NULL);
    }
#endif

/*	Wpt_Message_do.  Write a message wrt a panel.
 *
 */

    FUNCTION CODE Wpt_Message_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    IMPORT VOID		Wpt_PanelMessage ();

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) Co_Find (PanelCollection, StringParm("panel"));
    if (panel == NULL)
	{
	tmmsg (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", 
    			 StringParm("panel"));
	return (DO_CHECK);
	}
    Wpt_PanelMessage ((*panel).panelId, StringParm("message"));
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }


/*	Wpt_Reject_do.  Reject a new value.
 *
 *	TBD: what happens if the PARM parameter is unknown on the panel?
 */

    FUNCTION CODE Wpt_Reject_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    IMPORT VOID		Wpt_ParmReject();

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) Co_Find (PanelCollection, StringParm("panel"));
    if (panel == NULL)
	{
	tmmsg (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", 
    			 StringParm("panel"));
	return (DO_CHECK);
	}
    Wpt_ParmReject ((*panel).panelId, StringParm("parm"), StringParm("message"));
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }



/*	Wpt_Rehearse_do. 	Rehearse DDO's. (See newcalls.cc.)
 */

    FUNCTION CODE Wpt_Rehearse_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    CheckInit;
    Wpt_Rehearse 
	(IntegerParm("interval"), StringParm("order"), StringParm("cycles")); 
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }



/*	Wpt_SetPanelState_do.  Set a panel to the specified state.
 *
 */

    FUNCTION CODE Wpt_SetPanelState_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    TEXT		*string;
    TAEINT		state;			/* panel creation state */

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) Co_Find (PanelCollection, StringParm("panel"));
    if (panel == NULL)
	{
	tmmsg 
	  (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", StringParm("panel"));
	return (DO_CHECK);
	}

    /* Get creation state information. */
    string = StringParm("state");
    if (s_equal(string, "preferred"))
	state = WPT_PREFERRED;
    else if (s_equal(string, "visible"))
	state = WPT_VISIBLE;
    else if (s_equal(string, "invisible"))
	state = WPT_INVISIBLE;
    else if (s_equal(string, "iconic"))
	state = WPT_ICONIC;
    else if (s_equal(string, "fast iconic"))
	state = WPT_FASTICONIC;

    Wpt_SetPanelState ((*panel).panelId, state);
    XFlush(DisplayId);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);

    }

/*	Wpt_ParmUpdate_do.  Update a parm display with new selection 
 *      and/or  valid list.
 *
 *      The new valid list/selection is already set in the collection. 
 *
 *	TBD: what happens if the PARM parameter is unknown on the panel?
 */

    FUNCTION CODE Wpt_ParmUpdate_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    IMPORT VOID		Wpt_ParmUpdate();

    Id 			target;			/* target Vm object */
    Id			view;			/* view Vm object   */
    struct VARIABLE 	*parmv, *targetv, *v;
    struct VARIABLE	*Vm_FindVar();
    TEXT		*panelName;
    CODE		updateTarget ();

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) Co_Find (PanelCollection, StringParm("panel"));
    if (panel == NULL)
	{
	tmmsg (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", 
    			 StringParm("panel"));
	return (DO_CHECK);
	}

    /*	Check Vm object in target parmset for parm specified 	*/


    parmv = Vm_FindVar ( (*panel).target, StringParm("parm") );
    if ( parmv == NULL )
        {
	tmmsg (PROCFAIL, "Variable '%s' not found.", "TAE-NOTARGET",
    			 StringParm("parm"));
	return ( DO_CHECK );
        }



    /*  Move the current parm values from the
	qualified variable identified for the panel
        to the target vm object.
    */

    CurPanel = panel;		/* kludge communication with updateVariable */
    Vm_ForEach ((*panel).view, updateTarget, cpctx);

    Wpt_ParmUpdate ((*panel).panelId, StringParm("parm"));
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);

    }

/*	Wpt_ViewUpdate_do. Change the view of a parameter to a new one. 
 *	The new view is that of the parameter VIEW. 
 *
 *	TBD: what happens if the PARM parameter is unknown on the panel?
 */

    FUNCTION CODE Wpt_ViewUpdate_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PANEL	*panel;
    struct VARIABLE 	*viewv, *v;
    Id			newView;
    TEXT		*name;
    TEXT		var_name[NAMESIZ+1];
    TEXT		parent_name[STRINGSIZ+1];
    IMPORT VOID		Wpt_ViewUpodate();

    CheckInit;
    if (PanelCollection == NULL) PanelCollection = Co_New ();
    panel = (struct PANEL *) Co_Find (PanelCollection, StringParm("panel"));
    if (panel == NULL)
	{
	tmmsg (PROCFAIL, "Unknown panel '%s'.", "TAE-NOPANEL", 
    			 StringParm("panel"));
	return (DO_CHECK);
	}

    /*	Make a Vm object to be the view parmset	*/

    viewv = search (StringParm("view"), cpctx);
    if (viewv == NULL)
        {
        tmmsg (PROCFAIL, "Variable '%s' does not exist.", "TAE-BADNAME",
    			 StringParm("view"));
	return (DO_CHECK);
        }

    newView = Vm_New (P_CONT);
    Vm_CopyVarToVm (newView, viewv);	
    name = StringParm("view");
    p_get_leaf(name, var_name, parent_name);
    if (NULLSTR(var_name))
	s_copy(name, var_name);
    Wpt_ViewUpdate ((*panel).panelId, StringParm("parm"), 
 		newView, var_name); 
    
    Vm_Free(newView);
#else
    notImplemented ();
#endif /* XWINDOWS */
    return (DO_CHECK);
    }

#ifdef XWINDOWS
/*	convertName.  Convert string name to window ID.
 *
 */

    FUNCTION static Window convertName (name)

    	TEXT	*name;

    {
    Window window;

    if (s_equal(name, "root"))
	return (RootWindow(DisplayId, CurrentScreen) );
    window = Wpt_ConvertName (name);
    if (window == NULL)
        tmmsg (PROCFAIL, "Window '%s' does not exist.", "TAE-NOWINDOW", name);
    return (window);
    }
#endif /* XWINDOWS */



/*	notInitialized.  
 *
 */

    FUNCTION static VOID notInitialized ()

    {
    tmmsg (PROCFAIL, "Window system not initialized. Type WPT-INIT.",
           "TAE-NOXINIT");
    return ;
    }

/*	extractParent.    Crack a qualified name to get parent name.
 *
 */

    FUNCTION static VOID extractParent (name, parent, simple) 

	TEXT	name[];			/* in: full qualified name 	*/
	TEXT	parent[STRINGSIZ+1];	/* out:	parent name		*/
	TEXT	simple[STRINGSIZ+1];	/* out: simple right hand name  */
   
    {
    FAST TEXT	*s;			/* general text pointer		*/

    for (s = s_length(name) + name - 1;  s >= name;  s--)
        if (*s == '.')					
    	    break;			/* find last period		*/
    s_bcopy (s+1, simple, STRINGSIZ);	
    while (name < s)			/* copy rest till last period	*/
	*parent++ = *name++;		
    *parent = EOS;
    }

static TEXT ScriptJob[JOBNAMESIZ+1] = "WorkBench" ;

/*	Wpt_SetJob_do. 
 *
 *	Sets job name for subsequent WPTSCRIPT commands.
 */
     FUNCTION CODE Wpt_SetJob_do (cpctx, npctx)

		struct CONTXT *cpctx;
		struct CONTXT *npctx;
    {
    s_bcopy (StringParm("JOB"),  ScriptJob, sizeof (ScriptJob) - 1); 
    return (DO_SUCCESS);
    }


/*	Wpt_Script_do. 
 *
 *	Package and send a parblk to some process under test.
 */

     FUNCTION CODE Wpt_Script_do (cpctx, npctx)
	
	struct CONTXT *cpctx;
	struct CONTXT *npctx;
    {
#ifdef XWINDOWS
    struct PATH		path;		/* communication path context */
    struct PARBLK	parblk;		/* message block to send to task     */
    CODE		code;
    int			msgSize;	/* bytes in parblk */
    int			ack;
    TEXT		*vptr[1];

    zero_block ((GENPTR) &parblk, (GENPTR) parblk.pool - (GENPTR) &parblk);
    parblk.last = FALSE;
    parblk.msgtyp = M_INIPAR;			/* initial parameter block */

    /*  add some local symbols to the ipc message */
    vptr[0] = (*npctx).subcmd;
    addstr(&((*npctx).locst), "_SUBCMD", 1, 1, vptr, V_LOCAL);
    vptr[0] = (*cpctx).pdf.name;
    addstr(&((*npctx).locst), "_PROC",   1, 1, vptr,   V_LOCAL);
    addint(&((*npctx).locst), "_LINE", (*cpctx).pdf_line+1, V_LOCAL);

    package(npctx, &parblk, sizeof parblk.pool, 0); 
    code = c_conpath (&path, ScriptJob); 	/* connect to UUT process */
    if (code != SUCCESS) goto commError;
    msgSize = parblk.blksiz;
    code = c_putmsg (&path, &msgSize, sizeof (msgSize));	/* send count */
    if (code != SUCCESS) goto commError;
    code = c_putmsg (&path, &parblk, msgSize);			  /* send msg */
    if (code != SUCCESS) goto commError;
#ifdef VMS
    c_getmsg (&path, &ack, 1);	     		/* handshake in VMS */
#else
    c_read (&path, path.chnl, &ack, 1);		/* handshake in UNIX */
#endif
    c_discpath (&path);
    return (DO_CHECK);

commError:
    tmmsg (PROCFAIL, "Cannot send script message. %s.", 
        "TAE-COMMERR", path.errmsg);
    return (DO_CHECK);

#else
    notImplemented ();
    return (DO_CHECK);
#endif /* XWINDOWS */
    }



/*
 *      panel_func - return 1 if panel is displayed, 0 otherwise
 *
 *      returns SUCCESS or FAIL
 */
    FUNCTION CODE panel_func(nameval, context, numval, errmsg)

    struct VALUE        *nameval;       /* in: value containing the name*/
    struct CONTXT       *context;       /* in: TM context               */
    struct VALUE        *numval;        /* out: value containing the number*/
    struct ERRMSG       *(*errmsg);     /* out: error message           */

    {
    IMPORT struct SYMTAB glbtab;
    TEXT                *name;
    struct VARIABLE     *varptr, *lookex();

    inival(numval);                             /* set some defaults    */
    (*numval).type = V_INTEGER;
    name = (*nameval).uval.strpt;               /* name of panel     */
    if (PanelCollection == NULL)
        (*numval).uval.intval = 0;
#ifdef XWINDOWS
    else
        (*numval).uval.intval = (Co_Find(PanelCollection, name) == NULL) ? 0:1;
#endif
    return(SUCCESS);
    }



static FUNCTION VOID notImplemented ()
{
tmmsg (PROCFAIL, "TAE has not been installed with X Windows support.",
		"TAE-NOXLIB");
}
