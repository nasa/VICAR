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



/* menu_do SOURCE FILE.
 *
 * CHANGE LOG:
 *
 *	11-jul-83	Purged change log, deleted checkout records,
 *			and audited global definitions...jtm
 *	13-jul-83	Added error checkings for long title and zero
 *			menu entry selection...dm
 *	16-aug-83	Implemented "?" feature to help on message,
 *			added helpblk as help output control block...dm
 *	21-sep-83	Fix RESIDVAR inits for UNIX...palm
 *	05-oct-83	Change intcmd() to cmd_screen(), intlin() to
 *			line_noscreen()...dm			
 *	06-oct-83	Update for error message from gettitle()...dm
 *	11-oct-83	Fix unix compilation errors; allow "m" for menu...palm
 *	20-oct-83	Fix "maximum menu nesting " crash error...dm
 *	26-oct-83	Removed re-dundant "MENU" on header (PR 573)...palm
 *	29-oct-83	New d_init calling sequence (for .if)...dm
 *			Insert blank line after the title...dm
 *	31-oct-83	No prompt if invoked from proc the first time...dm
 *	14-jan-84	New line_noscreen calling sequence...palm
 *	25-jan-84	New line_noscreen calling sequence...palm
 *	03-feb-84	Implement .command directives...dm
 *	07-feb-84	Declare second argument of s_s2i() as TAEINT...dm
 *	14-feb-84	Fix UNIX hang on hardcopy prompt...dm
 *	15-FEB-84	Implement $MENUS global as menu stack, and
 *			hierarchy search for mdf...dm
 *	02-mar-84	Add REPLY for async...palm
 *	14-mar-84	Fix double f_close at menu exit...dm
 *	30-apr-84	New line_noscreen calling sequence...dm
 *	04-may-84	VALUE_x to xVAL ... ces
 *	06-may-84	Conform to no .defalt in RESIDVAR...peb
 *	07-may-84	Move IMPORT declarations...lim
 *	17-may-84	New hierarchy and f_crack calls...palm
 *	25-may-84	Trap absence of descriptor text in menu entries (PR 698)
 *	06-jun-84	Catch f_crack errors;
 *			"no description available" if no entry text...palm
 *	13-jul-84	Display menu library for hard-copy user...lia
 *	02-aug-84	Consolidate EMs (PR 396)...peb
 *	09-aug-84	Allow "let $menus=--" from .comand...nhe
 *	25-aug-84	Use new help output globals...nhe
 *	27-aug-84	Prompt "reply" when reply waiting from cmd mode...nhe
 *	12-oct-84	New pos_scroll() to position to bottom of screen as
 *			implemented for UNIX 1.2...dm
 *	29-oct-84	Fix call to opnhlp and proc_help...lia
 *	25-jul-85	Fix UNIX lint compilation errors...dm
 *	13-sep-85	PR 995: gtmencmd revised to determine if async REPLY
 *			needed via async_request as opposed to find_oldest which
 *			also "finds" async jobs waiting on completion msgs...dab
 *	20-sep-85	PR 836: gtmencmd revised to determine if async REPLY
 *			needed via search of ACBs as opposed to async_request
 *			test which failed when going from cmd to menu mode...dab
 *	17-feb-86	New MENU-CLOSED "captive" feature.
 *	05-aug-86	Added windowing capability for the SUN...ljn
 *	19-jan-87	PR1181: make "ROOT" and "MENUMOD" lowercase...palm
 *	10-feb-87	Merged TAE V1.4 with TAE FACELIFT...krw
 *      20-May-87       Added check for DisplayId(xwindow)...tpl
 *      13-aug-87       Merged in 2.2 changes...tpl
 *      15-oct-87       Added call to WinMenuMsg...tpl
 *      12-nov-87       Added call to HideCurMenu if tutor...tpl
 *      19-nov-87       Checked for display id in msghlp...tpl 
 *      30-nov-87       Checked for error returned from MenuWinPaint...tpl
 *	02-dec-87	Declare DisplayId as GENPTR...dm
 *	02-feb-88	PR1425: Paint flags -- See NOTE in menmod()...ljn
 *      03-feb-88       Changed DisplayId to XFACELIFT...tpl
 *	17-mar-88	PR 1499: Added isTerm() in opnmdf()...ljn
 *	14-apr-88	Extend PR1425 fix to Facelift...ljn for krw
 *	05-may-88	isTerm() -> f_isterm()...ljn
 *	08-sep-88	Initialize helpblk.remctx to NULL (PR fix)...dm
 *	12-jun-89	Removed TAE_FACELIFT...ljn
 *      15-nov-89       wrttxt does not return code...tpl
 *	23-may-90	Remove RCJM stuff...ljn
 *	27-jun-90	Remove Facelift code...ljn
 *	28-jun-90	Removed get_menucmd()...ljn
 *	01-aug-91	Braces for static string initializers...ljn
 *	18-mar-93	PR1867: A bug in menu_do allowed the user to get out of
 *			MENU Captive mode...krw
 */

#include	"taeconf.inp"
#include	"tmhost.inp"
#include	"tminc.inc"
#include	"dirinc.inc"
#include	"syninc.inc"
#include	"terminc.inc"
#include	"helpinc.inc"
#include	"menuinc.inc"
#ifdef TAE_ASYNC
#include	"asyncinc.inc"
#endif
#include "taeintproto.h"

FUNCTION  static  CODE  opnmdf
(
    TEXT		mdfname[],		/* IN: mdf name 	     */
    TEXT		menu_spec[]		/* OUT: menu file spec 	     */

 );
FUNCTION  static  CODE  curdsp
(
    CODE		disp_type,		/* IN: display refresh type  */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block*/

 );
FUNCTION  static  VOID  close_menu
(
    struct   CURMEN	*curmenu		/* current menu data block  */

 );
FUNCTION  CODE reposition 
(
    struct	CURMEN	*curmenu,		/* current menu block   */
    FUNINT		entrynum		/* entry number 0=start*/

 );
FUNCTION  static  VOID  rels_cur
(
    struct   CURMEN	*curmenu		/* current menu data block  */

 );
FUNCTION  static  CODE mencmd
(
    struct MCMDTAB	*ptr,		/* IN: pointer to command table entry */
    TEXT		param[],	/* IN: parameter string (if any) */
    TEXT		tmcmd[]	/* OUT: command string for TM */

 );
FUNCTION  static VOID prev_err
(
    struct  CURMEN	*curmenu, 		/* current menu control blk	*/
    TEXT		msg[],
    TEXT		key[],
    uintptr_t		a1, 
    uintptr_t		a2, 
    uintptr_t		a3, 
    uintptr_t		a4, 
    uintptr_t		a5
 );
FUNCTION static BOOL  menus_change(void);
FUNCTION  static  CODE  mpaint
(
    struct	CURMEN	*curmenu		/* IN/OUT: current menu block */

 );
FUNCTION static CODE set_prev(void);
FUNCTION VOID 	men_err 
(
TEXT		*msg,			/* IN: control string		*/
TEXT		*key,			/* IN: message key		*/
uintptr_t a1,			/* IN: integers or string ptrs	*/
uintptr_t a2,
uintptr_t a3,
uintptr_t a4,
uintptr_t a5
 );
FUNCTION   static   struct MCMDTAB  *  gtmencmd
(
    struct  CURMEN	*curmenu,		/* IN: current menu block */
    TEXT		param[]		/* OUT: command parameter */

 );

/* TM INTERNAL ERRORS USED BY THIS MODULE:  300		  	*/

/*
 * Define the resident PDF structure for menu_do parameters.
 * Note:  NAME is the implied name of the parameter.
 */

    static TEXT	*null_string[] = {""};		/* default is null string */
    static  struct  RESIDVAR   ptmenu[]=
      {{ "NAME", V_STRING, 0, 1, 1, FSPECSIZ, 1, NULL, (GENPTR)null_string }};


    GLOBAL struct CURMEN  *Curmenu = NULL; /* current menu block for editor */


/*
 * Define the intrinsic command table for the menu related TM  commands.
 */
FUNCTION CODE menu_do 
(
    struct CONTXT	*cpctx,			/* IN/OUT: ctx for proc      */
    struct CONTXT	*npctx			/* IN: ctx for MENU command  */

 );
    GLOBAL struct  ITRCMD  menucmd[] =
    {
    { 1, "MENU", "OPEN", Y_DEFSUB|Y_GENINTER, I_NPM(ptmenu), ptmenu, menu_do },
    { 1, "MENU", "CLOSED", Y_GENINTER,        I_NPM(ptmenu), ptmenu, menu_do },
    { 0, ""}				/* TERMINATOR ENTRY */
    };



/* previous menu stack; used to find out if menu stacke changed		*/

    static COUNT	prev_menus_count = 0;
    static TEXT		*prev_menus[MENUS_COUNT] = {NULL};

/*****************      file context  	*********************/
    GLOBAL struct  SFILE    mn_fctx;		/* SFILE to open mdf	     */
    GLOBAL struct  FSBLOCK  mn_fsblk;		/* FSBLOCK for opened mdf    */
    static BOOL	     tcl_menu; 			/* TRUE if a TCL menu	     */

    static  TEXT    prev_cmd[NEXTSIZ+1] = {0};	/* previous mode cmd buffer  */
    static  BOOL    prev_mode;			/* true if previous mode cmd */

    static  TEXT    nesting_msg[] =
    	"Maximum number of successive menu references (%d) exceeded.";
    static  TEXT    nesting_key[] = "TAE-MAXMEN";
    static  TEXT    over_msg[] = "Memory overflow; cannot display new menu.";
    static  TEXT    over_key[] = "TAE-MEMOVR";
    static  TEXT    open_msg[] = "Unable to access menu '%s'.";
    static  TEXT    open_key[] = "TAE-MDFOPEN";
    static  TEXT    atroot_msg[] = "Already at root menu.";
    static  TEXT    atroot_key[] = "TAE-ATROOT";
    static  TEXT    lomen_msg[] = "Entry number must be a positive integer.";
    static  TEXT    lomen_key[] = "TAE-LOMENNUM";
    static  TEXT    himen_msg[] =
    		"Selected entry number too large for this menu.";
    static  TEXT    himen_key[] = "TAE-HIMENNUM";
    static  BOOL    menu_closed = FALSE;	/* TRUE if MENU-CLOSED 	*/



/*
 * The 'menu_do' routine is called by the terminal monitor in response to a
 * TCL 'MENU' command from the operator, from a PROCEDURE or from a .command
 * mdf entry.
 * It opens the specified menu and sets the mode to MENUMODE. It also sets
 * the tcl_menu flag to TRUE to show menu command origin.
 * (The opened menu becomes the current menu and is later displayed
 * by the routine 'menmod'.
 */


FUNCTION CODE menu_do 
(
    struct CONTXT	*cpctx,			/* IN/OUT: ctx for proc      */
    struct CONTXT	*npctx			/* IN: ctx for MENU command  */

 )
    {
    IMPORT	CODE	menu_screen;		/* screen status indicator   */
    IMPORT 	CODE	usermode;		/* user mode:COMMAND or MENU */

    struct  SYMTAB	*p;			/* ptr to parameter block    */
    struct  VARIABLE	*v;			/* ptr to specific parameter */
    CODE 		code;			/* status return code        */
    TEXT	menu_spec[FSPECSIZ+1];		/* mdf spec (no type)	     */

    menu_closed |= s_equal ((*npctx).subcmd, "CLOSED");
    p = &((*npctx).parmst);			/* parameter symbol table    */
    v = lookex(p, "NAME");			/* check for parameter=FILE  */
    if  (v == NULL) 				/* wrong ctx block 	     */
	{
    	tmierr(300);				/* wrong ctx block in menu   */
	return(DO_CHECK);
	}

    code = opnmdf(SVAL(*v,0), menu_spec);	/* open specified mdf 	     */
    if (code == SUCCESS)
	{
	usermode = MENUMODE;			/* indicate menu mode        */
	code = DO_STOP;				/* return to primary level   */
	menu_screen = ((*cpctx).prclevel > 0 && menu_screen != UNDEFINE) ?
	    PROMPT_PAINT : AUTO_PAINT;		/* prompt if from proc 	     */
	tcl_menu = TRUE;			/* executing TCL menu command*/
	}
    else
	{
	if (code == M_NESTERR)			/* menu nesting error        */
	  tmmsg(PROCFAIL, nesting_msg, nesting_key, (uintptr_t)MENUS_COUNT,0,0,0,0); 
	else if (code == M_ALLERR)		/* stack allocation error    */
	    overr();
	else if (code == M_OPNERR)		/* mdf open error 	     */
	  tmmsg(PROCFAIL, open_msg, open_key, (uintptr_t)menu_spec,0,0,0,0 );
	code = DO_CHECK;
	}
    return (code);
    }


/*
 * 	opnmdf. Open menu definition file.
 *	Extract file name and library name from menu name. If no name
 *	specified use current menu name. If no current menu, assume root menu.
 *	If file name given but no library name, do hierarchy search to get
 *	the library .
 *
 */

FUNCTION  static  CODE  opnmdf
(
    TEXT		mdfname[],		/* IN: mdf name 	     */
    TEXT		menu_spec[]		/* OUT: menu file spec 	     */

 )
    {
    IMPORT  struct VARIABLE *menu_gbl;		/* ptr to variable $MENUS    */

    BOOL 		new_menu;		/* true if a new menu	     */
    TEXT		*save_str;		/* ptr to dynamic-saved str  */
    CODE		code;
    TEXT		errstr[STRINGSIZ+1];

    menu_spec[0] = EOS;				/* initialize		     */
    if  ( NESTING(menu_gbl) < 1)		/* no nesting:no current menu*/
	{					/* default to root menu      */
	new_menu = TRUE;			/* new menu display flag     */
	code = f_crack(mdfname, "", "root", MDF_TYPE, &mn_fsblk, errstr);
	if (code != SUCCESS)
	    {
	    s_bcopy (mdfname, menu_spec, FSPECSIZ);
	    return (M_OPNERR);			/* bad file spec	     */
	    }
	f_spec(&mn_fsblk, menu_spec);		/* file spec of menu to open */
	}
    else				
        {
	new_menu = !NULLSTR(mdfname);
	if  (new_menu)				/*  menu name given          */
	    {
	    if (NESTING(menu_gbl) >= MENUS_COUNT)	/* if stack full	     */
		return (M_NESTERR);		/* error return		     */
	    s_copy(mdfname, menu_spec);
	    }
	else
	    s_copy(LAST_ENTRY(menu_gbl), menu_spec);	/* get current menu  */
	}
    code = hierarchy(&mn_fctx, menu_spec, MDF_TYPE, MENULUN, &mn_fsblk, errstr);
						/* search menu hierarchy     */
    if (code != SUCCESS)
	return (M_OPNERR);
    if (f_isterm (&mn_fctx)) return (M_OPNERR);
    if (new_menu)
	{
	f_spec(&mn_fsblk, menu_spec);		/* get full file spec	     */
	save_str = s_save(menu_spec);		/* save in dynamic memory    */
	if (save_str == NULL)			/* failure		     */
	    return(M_ALLERR);
	NESTING(menu_gbl)++;			/* increment nesting level   */
	LAST_ENTRY(menu_gbl) = save_str;  	/* add menu name to stack    */
    	}
    return(SUCCESS);
    }


/*
 * 	menmod. (Re)enter menu mode.
 *
 * This routine is called by the terminal monitor to receive the next
 * command while in menu mode. Menmod displays the current menu on
 * the screen. Then it prompts the oparator for the next
 * command, performs the desired function if a menu related command,
 * or returns to the calling routine if a non-menu related command.
 * Note that getting the operator command and processing the command is
 * performed in a loop until the oparator specifies a command or selects an
 * entry that should be returned to the terminal monitor.
 *
 * NOTE: refreshing a menu screen and re-initializing the CURMENU block
 * go hand in hand in TAE (see mpaint()). Therefore, only cautiously use
 * the NO_PAINT flag. You may be stuck with an un-initialized CURMENU block.
 */

FUNCTION  VOID  menmod 
(
    TEXT 	tmcmd[]		/* OUT: command string returned to TM */

 )
    {
    IMPORT	CODE	menu_screen; 	/* menu screen status indicator */
    IMPORT	CODE	termtype;	/* terminal type 		*/
    IMPORT 	CODE	usermode;	/* user mode:COMMAND or MENU 	*/

    static struct CURMEN  curmenu = 
	{TRUE, FALSE, 0};		/* current menu block		*/
    TEXT	param[CMDLINSIZ+1];	/* parameter in command 	*/
    CODE 	disp_type;		/* Indicator for new menu screen ;
				 	 * NO_PAINT: do not paint screen ;
				 	 * PROMPT_PAINT: paint after prompt ;
				 	 * AUTO_PAINT: automatic paint   */
    CODE 	code;
    BOOL	tmret;			/* flag to return to TM 	*/
    struct 	MCMDTAB   *ptr;		/* ptr to entry in command table*/
    static	LONG	last_tupdate;   /* update count at leaving menu */
    LONG	new_tupdate;		/* update count at re-entering  */
    IMPORT struct VARIABLE *menu_gbl;


    Curmenu = &curmenu;			/* so editor can access it	*/
    zero_block ((GENPTR)&curmenu, sizeof (curmenu));
    curmenu.open = TRUE;
    curmenu.index = -1;			/* set "no current entry"	*/
    if (menu_screen == NO_PAINT)	/* returning after .command     */
	{
	new_tupdate = t_count();	/* screen update cnt after cmd  */
	if (new_tupdate != last_tupdate) /* display has changed		*/
	    menu_screen = PROMPT_PAINT;	/* prompt and display last menu	*/
	else if (prev_mode)		/* cmd issued from help screen  */
	    menu_screen = AUTO_PAINT;	/* display last menu     */
	else if (menus_change())	/* look for $MENUS change */
	    menu_screen = PROMPT_PAINT;
	else				/* PR1425 "fix"	*/
	    menu_screen = AUTO_PAINT;
	}
    if (menu_screen != NO_PAINT)		    /* if going to paint     */
	rels_cur(&curmenu);			    /* release old menu info.*/
    disp_type = menu_screen;			    /* copy locally	     */
    curmenu.open = tcl_menu;			    /* mdf open if TCL 'menu'*/
    for(tmret = FALSE; !tmret;)
	{
	if (disp_type != NO_PAINT)		    /* redisplay desired     */
	    {
	    code = curdsp(disp_type, &curmenu);	    /* display current menu  */
	    if (code != SUCCESS) goto menerr;	    /* error return to tm    */
	    }
	ptr = gtmencmd(&curmenu, param);	    /* get cmd in menu mode  */
	if (ptr == NULL) goto menerr;
	disp_type = (*(*ptr).cproc)(param, &curmenu);  /* process the command*/
	if ((*ptr).cmdgrp == CMDTM)		    /* if to be sent to TM   */
	    {
	    code = mencmd(ptr, param, tmcmd);	    /* build command string  */
	    menu_screen = code;		    	    /* default upon return   */
	    tmret = TRUE;			    /* flag 'return to tm'   */
            if ((*ptr).cmdmode)			    /* new mode = cmdmode    */
                usermode = CMDMODE;			    /* change mode 	     */
            }
	}
    if (termtype == T_CRT) pos_scroll();	    /* position at bottom    */
    last_tupdate = t_count();			    /* term update counter   */
    tcl_menu = FALSE;				    /* reset flag	     */
/*****************************************************************************
 *       The following  non-command mode restriction is only required in
 *       x window facelift version.  If we don't restrict it to
 *       CMDMODE only, tm will crash if one attempt to enter menu mode
 *       more than once; also will crash if one use menu in classic
 *       mode and than do window-init and attempt to use facelift menu.
 *       I am not sure whether this restriction causes any strange problems
 *       in menu mode  So far it seems to be harmless...tpl 10 sep, 1987
 ****************************************************************************/ 
    if ( usermode != CMDMODE )
        {
        if ( set_prev() != SUCCESS)
        	overr();
        }
       
    Curmenu = NULL;
    return;

menerr:						    /* menu mode error	     */
    tmcmd[0] = EOS;				    /* no command for tm     */
    usermode = CMDMODE;				    /* change mode 	     */
    if (termtype == T_CRT) pos_scroll();	    /* position at bottom    */
    Curmenu = NULL;
    return;
    }

/*
 *  curdsp. Display current menu on screen.	
 *
 * Note: if the prompt flag so indicates, it prompts the opearator for
 * carriage return before displaying the current menu on the screen.
 */

FUNCTION  static  CODE  curdsp
(
    CODE		disp_type,		/* IN: display refresh type  */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block*/

 )
    {
    IMPORT struct VARIABLE *menuopt_gbl;	/* $MENUOTP TCL global */
    TEXT		cmdnm[TOKESIZ+1];	/* command name */
    TEXT		cmdstr[CMDLINSIZ+1];	/* command string */
    struct SYNBLK	sb;			/* syntax block */
    struct MCMDTAB	*ptr;			/* pointer to table entry */
    TEXT                param[CMDLINSIZ+1];	/* command parameter */
    TEXT		errmsg[STRINGSIZ+1];	/* error message on bad */
						/*  parameter		*/
    TEXT		errkey[KEYSIZ+1];	/* error key on bad parameter */
    CODE 		code;			/* status code */
    BOOL		query_allowed;	
/*
 * This table exists for recognition of help requests when the operator
 * is prompted to: Press return key for menu.
 */
FUNCTION  CODE  menhlp
(
    TEXT		param[],		/* IN: parameter if any      */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 );
FUNCTION  CODE  msghlp
(
    TEXT		param[],		/* IN:parameter if any */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 );
    static struct MCMDTAB	mhlpmen[] =
	{
/*      cmdnm	 abbchar cmdgroup   numparm   cmdmode   rstrctd  function   */
/*                                                                         */
	{"HELP",   1,	CMDMENU,	1,    FALSE,	FALSE,   menhlp  },
	{"?",	   1,	CMDMENU,	0,    FALSE,    FALSE,   msghlp  },
	{"",0,0,0,FALSE,FALSE,0},
	};

    query_allowed = 
      ! search_vector ((TEXT**)(*menuopt_gbl).v_cvp, (*menuopt_gbl).v_count,
    		       "NO_PRESS_FOR_MENU") ;
    if (disp_type == PROMPT_PAINT && query_allowed)  /* display after prompt */
  	{
        cmd_noscreen (A_NONE, "Press RETURN key for menu", cmdstr);	
 	initok(&sb,cmdstr);            	
 	getvrb(&sb,cmdnm);			/* get command verb */
 	for (ptr = mhlpmen; !NULLSTR((*ptr).cmdnm); ptr++)
	    {					/* find entry in table */
	    if ((*ptr).abbchar == 0)
      		{
 		if (s_equal(cmdnm, (*ptr).cmdnm))  /* exact match found */
		    break;
 		}
	    else if (s_length(cmdnm) >= (*ptr).abbchar)
 		{
 		if (s_lseq(cmdnm, (*ptr).cmdnm))  /* strings match */
		    break;
 		}
	    }
 	if (!NULLSTR((*ptr).cmdnm))		/* match found, valid command */
	    {					/* get param, if any */
	    prev_mode = TRUE;
	    code = intprm((*ptr).numparm, (*ptr).cmdnm, cmdstr, param,
 			  errmsg, errkey);
	    if (code == SUCCESS)
		disp_type = (*(*ptr).cproc)(param, curmenu);
	    else
	      prev_err(curmenu, errmsg, errkey,0,0,0,0,0);
	    prev_mode = FALSE;
	    if (disp_type != AUTO_PAINT)
		return(SUCCESS);
	    }
 	} 	    					
    code = mpaint(curmenu);			 	/* paint the menu    */
    return(code);
    }

/*
 * dsptop. Go to the top of menu tree and display the menu.
 */

FUNCTION CODE dsptop 
(
    TEXT		param[],		/* IN: parameter (null string)*/
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    IMPORT  struct VARIABLE  *menu_gbl;		/* ptr to menu stack	     */

    CODE		code;			/* status from mpaint,ignored*/

    if (NESTING(menu_gbl) <= 1)			/* at root menu 	     */
      prev_err(curmenu, atroot_msg, atroot_key,0,0,0,0,0);  /* error  */
    else
	{
	close_menu(curmenu);			/* close current menu 	     */
	while (NESTING(menu_gbl) > 1 )		/* not root menu	     */
 	    {
	    s_free (LAST_ENTRY(menu_gbl));	/* free last menu on stack   */
	    NESTING(menu_gbl)--;		/* decrement nesting level   */
	    }
	code = mpaint(curmenu);			/* paint the root menu    */
	}
    return(NO_PAINT);				/* no new display upon return*/
    }

/*
 * dspbck. Back up one level in the menu tree and display the menu.
 */

FUNCTION CODE dspbck 
(
    TEXT  		param[],		/* IN: parameter (null string) */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    IMPORT  struct VARIABLE  *menu_gbl;		/* ptr to menu stack	     */

    CODE		code;			/* status from mpaint,ignored*/

    if (NESTING(menu_gbl) <= 1)			/* at root menu 	     */
      prev_err(curmenu, atroot_msg, atroot_key,0,0,0,0,0);  /* error  */
    else
	{
	close_menu(curmenu);			/* close current menu 	     */
	if (NESTING(menu_gbl) > 1)		/* don't do if null	     */
	    s_free (LAST_ENTRY(menu_gbl));	/* free last menu on stack   */
	NESTING(menu_gbl)--;			/* decrement nesting level   */
        code = mpaint(curmenu);			/* paint the root menu       */
	}
    return(NO_PAINT);				/* do not change upon return */
    }

/*
 * dspmen. Display user specified menu.
 * Note: if no menu name is specified, redisplay current menu.
 */

FUNCTION  CODE   dspmen
(
    TEXT 		mdfname[],		/* IN: menu name 	     */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block*/

 )
    {
    CODE		code;
    TEXT		menu_spec[FSPECSIZ+1];

    code = SUCCESS;
    strpqu(mdfname);				/* strip quotes, if any	     */
    if (!NULLSTR(mdfname))			/* if menu name given        */
	{
 	f_close(&mn_fctx, F_KEEP);		/* close current mdf 	     */
	(*curmenu).open = FALSE;
   	code = opnmdf(mdfname, menu_spec);	/* open new mdf  	     */
        if (code == SUCCESS)
	    {
 	    rels_cur(curmenu);			    /* release old info      */
     	    (*curmenu).open = TRUE;		    /* reinit for new mdf    */
	    }
	else
    	    {
   	    if (code == M_NESTERR)		    /* menu nesting error    */
	      prev_err(curmenu, nesting_msg, nesting_key, MENUS_COUNT,0,0,0,0);
	    else if (code == M_ALLERR)		    /* stack allocation error*/
	      prev_err(curmenu, over_msg, over_key,0,0,0,0,0);
	    else if (code == M_OPNERR)		    /* mdf open error 	     */
	      prev_err(curmenu, open_msg, open_key, (uintptr_t)menu_spec,0,0,0,0);
 	    if (!(*curmenu).open)	        /* if not opened in prev_err */
		{
    	        opnmdf("", menu_spec);		    /* reopen current menu   */
    	        (*curmenu).open = TRUE;		    /* mark as open	     */
		}
	    }
        }
    if (code == SUCCESS)
        code = mpaint(curmenu);			/* (re)paint the menu    */
    return(NO_PAINT);				/* do not change upon return */
    }

/*
 * menhlp. Display menu/proc/command help information on screen.
 */

FUNCTION  CODE  menhlp
(
    TEXT		param[],		/* IN: parameter if any      */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    IMPORT BOOL		full_scr_help;		/* TRUE if we want full screen */
    IMPORT BOOL		help_to_term;		/* TRUE if output to terminal is to be assumed */
    IMPORT CODE		termtype;

    struct SFILE	fctx;			/* a file context block	     */
    TEXT		proc[STRINGSIZ+1];
    TEXT		name[FSPECSIZ+1];
    TEXT		subname[SUBCMDSIZ+1];
    TEXT		libname[FLIBRSIZ+1];
    TEXT		lefthead[STRINGSIZ+1];
    TEXT		record[STRINGSIZ+1];
    TEXT		type[8];
    TAEINT		p;
    CODE		help_type=0;		/* type of help needed	     */
    CODE		code=0;
    BOOL		intrin;
    struct  FSBLOCK	fsblock;
    struct  HELPBLK	helpblk;		/* help output control block */
    TEXT		errstr[STRINGSIZ+1];

#define  MENUMODE_HELP	1
#define	 CURMENU_HELP	2
#define	 MENU_HELP	3
#define	 PROC_HELP	4
#define	 ENTRY_HELP	5

    full_scr_help = (termtype == T_CRT);		/* full screen help if CRT  */
    help_to_term = TRUE;			/* output to terminal	    */

    name[0] = proc[0] = EOS;
    helpblk.nextcmd[0] = EOS;
    helpblk.helpobj[0] = EOS;
    helpblk.compiled   = FALSE;			/* assume not on compiled PDF*/

    if (NULLSTR(param))
	help_type = MENUMODE_HELP;		/* help on menu mode	     */
    else if (s_equal(param, "*"))	
	help_type = CURMENU_HELP;		/* help on current menu      */
    else if (s_s2i(param, &p)==SUCCESS)		/* parameter a numeric ?     */
    	{
	if (p < 1)
	    goto lownum;
	else if (p > (*curmenu).nent)	      		/* not in range      */
	    goto highnum;
    	else
    	    {
	    if ((*curmenu).type[p-1] == MN_MENU)	
		help_type = MENU_HELP;			/* help on a menu    */
	    else if ((*curmenu).type[p-1] == MN_PROC)
		{
		help_type = PROC_HELP;			/* help on a proc    */
	        s_copy((*curmenu).nameblk.tp[p-1], proc);   /* get proc name */
		}
	    else if ((*curmenu).type[p-1] == MN_COMMAND)
		help_type = ENTRY_HELP;			/* help on .command  */
	    }
	}
    else
	{
	help_type = PROC_HELP;				/* default case	     */
	s_copy(param, proc);				/* get proc name     */
	}

    if (help_type == MENUMODE_HELP)		/* get help on menu-mode     */
	{					
	s_copy("menu mode", helpblk.helpobj);
    	if (f_open(&fctx, HELPLUN, HELPLIB, "menumode", HLP_TYPE, F_READ)
			 != SUCCESS)   goto opnerr;
 	code = genhelp(&fctx, HELPLIB, "gen", "", &helpblk);	/* position and disp */
    	f_close(&fctx, F_KEEP);
	}

    else if (help_type == CURMENU_HELP)		/* get help on current menu  */
	{
    	code = reposition (curmenu, 0);			/* position to start */
	if (code != SUCCESS) return(NO_PAINT);
 	left_fbld(mn_fsblk.libr, mn_fsblk.name, "",
		 "menu", lefthead);  			/* build disp header */
	s_copy(mn_fsblk.name, helpblk.helpobj);
   	code = genhelp(&mn_fctx, mn_fsblk.libr, "menu", lefthead,
			&helpblk);			/* position and disp */
   	}

    else if (help_type == MENU_HELP)		/* get help on a menu entry  */
	{
	s_copy((*curmenu).nameblk.tp[p-1], name);  	/* get entry name    */
	f_crack(name, "", "", MDF_TYPE, &fsblock, errstr);
	s_copy(fsblock.name, helpblk.helpobj);
        code = hierarchy(&fctx, name, MDF_TYPE, HELPLUN, &fsblock, errstr);
	if (code != SUCCESS) goto opnerr;

	left_fbld(fsblock.libr, fsblock.name, "", "menu", lefthead);
	code = genhelp(&fctx, fsblock.libr, "menu", lefthead,
		&helpblk);				/* position and disp */
	f_close(&fctx, F_KEEP);
	}
    else if (help_type == PROC_HELP)		/* get help on given proc    */
	{
    	if ((code = opnhlp(&fctx, proc, name, subname, libname,
	     &intrin, &helpblk)) == SUCCESS)	/* open the file			*/
    	    {
	    s_copy("proc", type);			
	    if (intrin) s_copy("command", type);	/* intrinsic command */
	    s_copy(subname, helpblk.helpobj);
    	    code = proc_help(&fctx, libname, libname, name, subname, "",
		   type, &helpblk);		/* get help on it    */
	    f_close(&fctx, F_KEEP);
	   }
	else
	    goto chkprev;
	}

    else if (help_type == ENTRY_HELP)		/* get help on .command entry*/
	{
	code = reposition(curmenu, p);			/* position to entry */
	if (code != SUCCESS) return(NO_PAINT);
	s_copy("Entry ", helpblk.helpobj);
	s_append(param, helpblk.helpobj);		/* give entry number */
	f_read(&mn_fctx, record);			/* position past dirctv  */
	code = entr_help(&mn_fctx, mn_fsblk.libr,
	    	mn_fsblk.name, &helpblk);		/* display help info */
	}

    if (code == SUCCESS && NULLSTR(helpblk.nextcmd))	/* no cmd returned   */
	return(AUTO_PAINT);		/* refresh screen    */
    else
	goto chkprev;

lownum:
    prev_err(curmenu, lomen_msg, lomen_key,0,0,0,0,0);
    return(NO_PAINT);

highnum:
    prev_err(curmenu, himen_msg, himen_key,0,0,0,0,0);
    return(NO_PAINT);

opnerr:
    prev_err(curmenu, "No help available on '%s'.", "TAE-NOHELP",
	     (uintptr_t)helpblk.helpobj,0,0,0,0);
    return(NO_PAINT);

chkprev:
    if (code != SUCCESS)
      prev_err(curmenu, helpblk.errmsg, helpblk.errkey,0,0,0,0,0);
    s_copy(helpblk.nextcmd, prev_cmd);		/*previous mode command	*/
    return(NO_PAINT);
    }

/*
 * menus_change - return TRUE if previous $MENUS menu list is not the same
 *		 as the one in current $MENUS.  Else FALSE
 */
    FUNCTION static BOOL  menus_change(void)

    {
    IMPORT  struct VARIABLE  *menu_gbl;		/* ptr to menu stack	     */
    COUNT		i;

    if (NESTING(menu_gbl) != prev_menus_count)  /* prev_etc is a menu.c static */
    	return(TRUE);
    for (i=0; i<NESTING(menu_gbl); i++)
    	if (!s_equal(SVAL(*menu_gbl,i), prev_menus[i]))
    	    return(TRUE);
    return(FALSE);				/* no change		    */
    }

/*
 * msghlp. Help on the last displayed  TAE message.
 */

FUNCTION  CODE  msghlp
(
    TEXT		param[],		/* IN:parameter if any */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    IMPORT  TEXT	lastkey[];		/* last message key  	     */
    IMPORT BOOL		full_scr_help;		/* TRUE if we want full screen */
    IMPORT BOOL		help_to_term;		/* TRUE if output to terminal is to be assumed */

    IMPORT CODE		termtype;
    CODE		code;
    CODE		disp_type;		/* display refresh type      */
    struct  SFILE	fctx;			/* help file context block   */
    struct  HELPBLK	helpblk;		/* help output control block */

    full_scr_help = (termtype == T_CRT);		/* full screen help if CRT  */
    help_to_term = TRUE;			/* output to terminal	    */

    helpblk.nextcmd[0] = EOS;
    helpblk.compiled   = FALSE;			/* not help on compiled PDF  */
    code = msg_help(&fctx, lastkey, &helpblk);		/* display help data */
    if (code == SUCCESS && NULLSTR(helpblk.nextcmd))	/* no prev. mode cmd */
  	disp_type = AUTO_PAINT;				/* refresh screen    */
    else
	{
	if (code != SUCCESS)
	  prev_err(curmenu, helpblk.errmsg, helpblk.errkey,0,0,0,0,0);
        s_copy(helpblk.nextcmd, prev_cmd);
	disp_type = NO_PAINT;			/* no automatic menu display */
	}
    f_close(&fctx, F_KEEP);
    return(disp_type);			
    }

/*
 * reposition. reposition the mdf as specified.
 * Note: all mdf opens for menu display must use the fsblock 'mn_fsblk'
 *	 to get the current menu library and name for display header.
 */

FUNCTION  CODE reposition 
(
    struct	CURMEN	*curmenu,		/* current menu block   */
    FUNINT		entrynum		/* entry number 0=start*/

 )
    {
    IMPORT  struct  VARIABLE  *menu_gbl;	/* ptr to menu stack	    */

    CODE		code;
    TEXT		errstr[STRINGSIZ+1];
    TEXT		menu_spec[FSPECSIZ+1];

    if (!(*curmenu).open)				/* mdf not open	    */
	{
    	if (NESTING(menu_gbl)==0) 		/* somebody NULLed us */
    	    {
    	    code = opnmdf("",menu_spec);
	    if (code == M_NESTERR)			/* menu nesting error        */
	      tmmsg(PROCFAIL, nesting_msg, "TAE-MAXMEN", MENUS_COUNT,0,0,0,0);
	    else if (code == M_ALLERR)		/* stack allocation error    */
		overr();
	    else if (code == M_OPNERR)		/* mdf open error 	     */
	      tmmsg(PROCFAIL, open_msg, "TAE-MDFOPEN", (uintptr_t)menu_spec,0,0,0,0);
    	    return(code);
    	    }
	f_crack(LAST_ENTRY(menu_gbl), "", "", "", &mn_fsblk, errstr);
	code = f_opnblk(&mn_fctx, MENULUN, &mn_fsblk, F_READ);
	if (code != SUCCESS)
	    {
	      men_err(open_msg, open_key, (uintptr_t)LAST_ENTRY(menu_gbl),0,0,0,0);
	    return(code);
	    }
	(*curmenu).open = TRUE;				/* mark as open     */
	}
    if (entrynum == 0)					/* position at start */
	f_rewind(&mn_fctx);			
    else
	f_setpos(&mn_fctx, &(*curmenu).pos[entrynum-1]);	/* position to entry */
    return (SUCCESS);
    }

/* set_prev.  Set the prev_menus array to the current value of $MENUS
 *	      Return FAIL on allocation error
 */
FUNCTION static CODE set_prev(void)

    {
    IMPORT	struct VARIABLE *menu_gbl;

    COUNT		i;

    for (i=0; i<prev_menus_count; i++)
    	s_free(prev_menus[i]);		/* deallocate the old ones	*/
    prev_menus_count = NESTING(menu_gbl);
    for (i=0; i<NESTING(menu_gbl); i++)
    	{
    	prev_menus[i] = s_save(SVAL(*menu_gbl, i));
    	if (prev_menus[i] == NULL)
    	    return(FAIL);		/* allocation error		*/
    	}
    return(SUCCESS);
    }

	
/*
 * dsppth. Trace path of menu tree on the screen.
 */

FUNCTION  CODE  dsppth
(
    TEXT		param[],		/* IN:parameter if any */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    CODE		disp_type;		/* display refresh type  */

    disp_type = AUTO_PAINT;			/* automatic menu display with prompt*/
    return(disp_type);			/* dummy routine */
    }

/*
 * dspcpy. Generate a hardcopy of the current menu
 */

FUNCTION  CODE  dspcpy
(
    TEXT		param[],		/* IN:parameter if any */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    CODE		disp_type;		/* display refresh type  */

    disp_type = NO_PAINT;			/* no automatic redisplay */
    return(disp_type);			/* dummy routine */
    }

/*
 * exitmen. Exit from menu display.
 */

FUNCTION  CODE  exitmen
(
    TEXT		param[],		/* IN: parameter (NULL STRING) */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    close_menu(curmenu);			/* close current mdf 	     */
    return(NO_PAINT);				/* no new display	     */
    }

/*
 * retmen. Return from menu display (to execute a .command).
 */

FUNCTION  CODE  retmen
(
    TEXT		param[],		/* IN: parameter (NULL STRING) */
    struct  CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {

    f_close(&mn_fctx, F_KEEP);			/* close mdf 		     */
    (*curmenu).open = FALSE;			/* mark mdf closed.	     */
    return(NO_PAINT);				/* no new display	     */
    }

/*
 * close_menu. Close the current menu (close mdf, delete curmenu block).
 */

FUNCTION  static  VOID  close_menu
(
    struct   CURMEN	*curmenu		/* current menu data block  */

 )
    {
    if ((*curmenu).open)
	{
	f_close(&mn_fctx, F_KEEP);		/* close mdf 		    */
	(*curmenu).open = FALSE;		/* mark mdf closed.	    */
	}
    rels_cur(curmenu);				/* release info in curmenu  */
    return;
    }

/*
 * rels_cur. Release information in current menu block.
 */

FUNCTION  static  VOID  rels_cur
(
    struct   CURMEN	*curmenu		/* current menu data block  */

 )
    {
    COUNT i;
    fretxt(&((*curmenu).ttlblk));		/* free title strings 	    */
    fretxt(&((*curmenu).nameblk));		/* free entry name strings  */
    (*curmenu).finit = FALSE;			/* mark curmenu unused      */

    for (i=0; i < (*curmenu).nent; i++)		/* free strings		    */
        {
        s_free ((*curmenu).entryline[i]);
        (*curmenu).entryline[i] = NULL;
        }
    (*curmenu).index = -1;			/* no active index	    */
    return;
    }

/*
 * gtmencmd. Get next menu mode command.
 *
 * This function receives a command  in menu mode and determines the
 * command type by matching against a table. It also extracts the associated
 * parameters , if any.
 *
 */

FUNCTION   static   struct MCMDTAB  *  gtmencmd
(
    struct  CURMEN	*curmenu,		/* IN: current menu block */
    TEXT		param[]		/* OUT: command parameter */

 )
    {
    IMPORT	COUNT	termlines;		/* number of lines on terminal */
    IMPORT	CODE	termtype;		/* terminal type 		*/

    TEXT 		cmdnm[TOKESIZ+1];	/* command name */
    TEXT		cmdstr[CMDLINSIZ+1];	/* command string */
    TEXT		*options;		/* ptr to options prompt  */
    BOOL		good_command;
    struct SYNBLK	sb;			/* syntax block */
    struct MCMDTAB	*ptr;			/* pointer to table entry */
    TAEINT		entnum;			/* entry number */
    CODE		terminat;		/* command line terminator */
    CODE 		code;			/* status code */
    TEXT  		errmsg[STRINGSIZ+1];
    TEXT		errkey[KEYSIZ+1], prompt[STRINGSIZ+1];
    TEXT		tmpstr[30];
#ifdef TAE_ASYNC
    IMPORT struct ACB	*acb_head;	/* Beginning of ACBs */
    struct ACB		*acb;		/* To find if async job reply required */
#endif

/*
 * This table defines all the menu mode commands specified by a name.
 * (one entry per command).  The "restricted" flag is set TRUE for
 * commands not allowed in MENU-CLOSED mode.
 */

					/* explicitely named commands */
    static struct MCMDTAB	mcmdmen[] =
	{
/*	cmdnm	 abbchar  cmdgroup   numparm   cmdmode	rstrctd  function   */
/*									   */
	{"TOP",	   1,    CMDMENU,    0,	      FALSE, 	FALSE, dsptop   },
	{"BACK",   1,    CMDMENU,    0,       FALSE,	FALSE, dspbck   },
	{"MENU",   1,    CMDMENU,    1,	      FALSE,	TRUE,  dspmen	},
	{"HELP",   1,    CMDMENU,    1,	      FALSE,    FALSE, menhlp	},
	{"?",	   1,    CMDMENU,    0,	      FALSE,	FALSE, msghlp   },
	{"REPLY",  1,    CMDTM,	     0,	      FALSE,	FALSE, exitmen  },
	{"COMMAND",1,    CMDTM,	     0,	      TRUE,	TRUE,  exitmen	},
	{"EXIT",   1,    CMDTM,	     0,	      TRUE,	TRUE,  exitmen	},
	{"LOGOFF", 1,    CMDTM,	     0,	      TRUE,	FALSE, exitmen	},
	{"",0,0,0,FALSE,FALSE,0},
	};					

				/* proc selection, assume command = "PROC" */
    static struct MCMDTAB	mproctab =
	{"PROC",  4,	CMDTM,      1, 	      FALSE,	FALSE, exitmen  };
				/* TCL command, assume command = "TCLCMD"*/
    static struct MCMDTAB	mtcltab =
	{"TCLCMD",6,	CMDTM,	    1,	      FALSE,    FALSE, retmen  };


    while (FOREVER)
	{
	prev_mode = !NULLSTR(prev_cmd);		/* true if not null     */
	if (prev_mode)				/* previous mode cmd from help */
	    {
	    s_copy(prev_cmd, cmdstr);		/* copy to command buffer*/
	    prev_cmd[0] = EOS;			/* mark prev cmd buffer as empty	*/
	    }
	else
	    {
	    s_copy ("Enter: ", prompt);
#ifdef TAE_ASYNC
						/* look for async request    */
	    for (acb = acb_head; acb != NULL; acb = (*acb).link)
	    if ((*acb).waiting == TRUE && (*acb).active == TRUE)
     	        {
		s_copy ("REPLY,", tmpstr);
		t_highlight (tmpstr);
		s_append (tmpstr, prompt);
		break;
		}
#endif /* TAE_ASYNC */
	    if (menu_closed)
		options = " selection number, HELP, BACK, TOP, or LOGOFF.";
	    else
	        options = " selection number, HELP, BACK, TOP, MENU, COMMAND, or LOGOFF.";
	    s_append (options, prompt);
	    wrttxt((termlines - PMTLIN - 1), 1, prompt,	 FALSE);
	    if (termtype == T_CRT)
	        {
		code = cmd_screen (A_MENU, cmdstr, &terminat);	
		if (code != SUCCESS)
		    return(NULL);
		}
	    else
	        cmd_noscreen (A_NONE, "? ", cmdstr);
	    }
	if (NULLSTR(cmdstr))			/* no command 		   */
	    continue;
	initok(&sb, cmdstr);			/* initialise for syntax scanning */
  	getvrb(&sb, cmdnm);			/* retrieve command name   */
	for (ptr = mcmdmen; !NULLSTR((*ptr).cmdnm); ptr++)	/* find entry in table */
	    {
	    if ((*ptr).abbchar == 0)
	        {
	    	if (s_equal(cmdnm, (*ptr).cmdnm))  break;	/* exact match found */
	    	}
	     else if (s_length(cmdnm) >= (*ptr).abbchar)
	    	{
	    	if (s_lseq(cmdnm, (*ptr).cmdnm))  break;	/* strings match */
	    	}
	    }
	
	if (NULLSTR((*ptr).cmdnm))
	    good_command = FALSE;
	else
	    good_command = !menu_closed || (menu_closed && !(*ptr).restricted);

   	if (good_command)		/* match found, valid command*/
	    {						/* get param, if any */
	    code = intprm((*ptr).numparm, (*ptr).cmdnm, cmdstr, param,
			errmsg, errkey);
	    if (code == SUCCESS)
		{
		if (termtype == T_CRT)
		    t_lclear(termlines - ERRLIN, 1); 	/* blank error msg line */
 	        break;				
	    	}
	    else
	      prev_err(curmenu, errmsg, errkey,0,0,0,0,0);
	    }	
	else
	    {						/* no match found in table */
	    ptr = NULL;
	    code = s_s2i(cmdnm, &entnum);		/* try to convert to a number */
	    if (code == SUCCESS)
	        {
		if (entnum < 1)
		  prev_err(curmenu, lomen_msg, lomen_key,0,0,0,0,0);
	      	else if (entnum > (*curmenu).nent)	 /* not in range     */
		  prev_err(curmenu, himen_msg, himen_key,0,0,0,0,0);
	    	else
		    {
		    if ((*curmenu).type[entnum-1] == MN_PROC)
		        ptr = &mproctab;		/* point to PROC table */
		    else if ((*curmenu).type[entnum-1] == MN_COMMAND)
		        ptr = &mtcltab;		/* point to table for TCL cmd's*/
		    else
		        {
		        for (ptr = mcmdmen; !NULLSTR((*ptr).cmdnm); ptr++)
			if (s_equal((*ptr).cmdnm, "MENU")) break;  /* point to MENU */
		    	}
		    code = intprm((*ptr).numparm-1, "select number", cmdstr,
				param, errmsg, errkey);	/* check syntax */
		    if (code == SUCCESS)		/* no syntax error */
			{
		    	s_copy((*curmenu).nameblk.tp[entnum-1], param);
							/* get proc/mdf name */
		   	 break;
			}
		    else
		      prev_err(curmenu, errmsg, errkey,0,0,0,0,0);	/* report error */	
		    }
		}
	    else
		prev_err(curmenu, "Unrecognized menu command '%s'.",
			 "TAE-BADMENCMD", (uintptr_t)cmdnm,0,0,0,0);
	    }
	}
    return(ptr);
    }

/*
 *	mencmd. Build a command string to be returned to TM.
 *	It also decides if the screen should be automatically
 *	repainted when we come back from executing that command.
 *	NO_PAINT implies that if there is no change to the screen
 *	when we come back to menu mode, do not repaint.
 */

FUNCTION  static  CODE mencmd
(
    struct MCMDTAB	*ptr,		/* IN: pointer to command table entry */
    TEXT		param[],	/* IN: parameter string (if any) */
    TEXT		tmcmd[]	/* OUT: command string for TM */

 )
    {

    CODE		code;

    code = AUTO_PAINT;				/* default		*/
    if (s_equal((*ptr).cmdnm, "COMMAND") || s_equal((*ptr).cmdnm, "EXIT"))
	s_copy("COMMAND", tmcmd);		/* command = COMMAND	*/
    else if (s_equal((*ptr).cmdnm, "LOGOFF"))
	s_copy("LOGOFF", tmcmd);		/* command = LOGOFF	*/
    else if (s_equal ((*ptr).cmdnm, "REPLY"))
	{	
	s_copy ("REPLY", tmcmd);		/* command = REPLY    	*/
	code = NO_PAINT;			/* no automatic repaint */
	}
    else if (s_equal((*ptr).cmdnm, "TCLCMD"))   /* a TCL command        */
	{
	s_copy(param, tmcmd);			/* copy the command     */
	code = NO_PAINT;			/* no automatic repaint */
	}
    else					/* assume proc select */
    	{
	s_copy("TUTOR ",tmcmd);			/* command = TUTOR    */
	s_append(param, tmcmd);			/* give  proc name    */
        }
    return (code);					
    }

/*
 * addent. Add entry number to text record.
 */


FUNCTION  VOID  addent
(
    TEXT		record[],		/* IN: text record */
    FUNINT		entnum,			/* IN: entry number, <= 99 */
    FUNINT		firstlin,		/* IN: true if first line of entry */
    TEXT		textrec[]		/* OUT: record with entry num*/
 )
    {
    IMPORT	COUNT	termcols;		/* number of columns per line 	*/

    TEXT		numstr[12];		/* entry number in text */

    if (s_length(record) > (termcols-4))
	record[termcols-4]=EOS;			/* truncate if necessary */
    if (firstlin)
	{
	s_i2s(entnum, numstr);			/* convert to string */
	s_copy(numstr, textrec);
	s_append(")  ", textrec);
	}
    else
	s_copy("    ", textrec);		/* blank first four columns */
    s_copy(record, &textrec[4]);		/* add after 4th column */
    return;
    }

/*
 *	prev_err. Check if previous mode command and display error message.
 */

FUNCTION  static VOID prev_err
(
    struct  CURMEN	*curmenu, 		/* current menu control blk	*/
    TEXT		msg[],
    TEXT		key[],
    uintptr_t		a1, 
    uintptr_t		a2, 
    uintptr_t		a3, 
    uintptr_t		a4, 
    uintptr_t		a5
 )
    {
    if (prev_mode)				/* if a previous mode command	*/
        mpaint(curmenu);			 	/* paint the menu    */

    men_err (msg, key, a1, a2, a3, a4, a5);	/* write error message		*/
    return;
    }


/*
 * mpaint 
 *	Paint the screen with current menu.
 */

FUNCTION  static  CODE  mpaint
(
    struct	CURMEN	*curmenu		/* IN/OUT: current menu block */

 )
    {
    IMPORT	struct  VARIABLE *char_gbl;     /* system characteristics   */
    IMPORT	struct  VARIABLE *menuopt_gbl;  /* menu options global      */
    IMPORT	COUNT	termlines;		/* number of lines on terminal */
    IMPORT	CODE	termtype;		/* terminal type 	    */

    COUNT		line;			/* line number on crt       */
    COUNT		startlin;		/* text display start line  */
    COUNT		stopline;		/* text display end line    */
    COUNT		maxtitle;		/* max allowed title length */
    CODE		code;
    TEXT		record[CMDLINSIZ+1];
    TEXT		dirctv[STRINGSIZ+1];
    TEXT		textrec[STRINGSIZ+1];
    TEXT		lefthead[STRINGSIZ+1];
    COUNT		entnum;			/* menu entry number 	    */
    BOOL		firstlin;		/* first text line of entry */
    struct   DIRBLK	dirblk;			/* directive block 	    */
    TEXT		ttlmsg[STRINGSIZ+1];	/* error msg from gettitle  */
    TEXT		ttlkey[KEYSIZ+1];	/* error key from gettitle  */
    BOOL      no_tag, no_name, no_library;

    code = reposition(curmenu, 0);		/* position mdf to beginning */
    if (code != SUCCESS)
	return(code);
    startlin = TITLELIN;		        /* menu display start line   */
    stopline = termlines-ERRLIN-2;		/* end line for text display */
    maxtitle = stopline-TITLELIN-1;		/* max title length 	     */

    no_tag = search_vector ((TEXT**)(*menuopt_gbl).v_cvp, 
				    (*menuopt_gbl).v_count, "NO_TAG");
    no_name = search_vector ((TEXT**)(*menuopt_gbl).v_cvp, 
				    (*menuopt_gbl).v_count, "NO_NAME");
    no_library = search_vector ((TEXT**)(*menuopt_gbl).v_cvp, 
				    (*menuopt_gbl).v_count, "NO_LIBRARY");
    if (termtype == T_CRT)
	{
	code = t_clear();			/* clear screen if CRT	     */
	left_fbld  ( 				/* build left header         */
		   no_library ? "" : mn_fsblk.libr, 
		   no_name ? "" : mn_fsblk.name, 
		   "",				/* subcommand not used */
		   " ", 			/* type not used       */
		   lefthead);
	dsphdr (no_tag ? "" : "Menu", 		/* display header      */
		lefthead, 0, TRUE);   
	}
    else
	{
	left_fbld(mn_fsblk.libr, mn_fsblk.name, "",
		  "menu", lefthead);		/* build left header */
	code = t_write(" ", T_STDCC);		/* for hardcopy, skip a line */
	code = t_write(lefthead, T_STDCC);	/* display menu library */
	code = t_write(" ", T_STDCC);
	}
    if (!(*curmenu).finit)
	{
	initxt(&(*curmenu).ttlblk);		/* init title bolck 	     */
	initxt(&(*curmenu).nameblk);		/* init for entry names      */
	}
    d_init(&dirblk, &mn_fctx, "", (TEXT **)(*char_gbl).v_cvp,
	 (*char_gbl).v_count);    		/* init d_ package       */
    code = d_cdirctv(&dirblk, dirctv, record);	/* get first directive record*/
    if (s_equal(dirctv, ".TITLE"))
	{
	if (!(*curmenu).finit)
	    {
	    code = gettitle(&dirblk, &((*curmenu).ttlblk), ttlmsg, ttlkey);
  	    if (code !=SUCCESS) 		/* error reading title      */
	   	{
		  men_err(ttlmsg, ttlkey,0,0,0,0,0);
	    	return(code);
	        }
	    }
	if ((*curmenu).ttlblk.numline > 0)	/* title text present 	     */
	    {
	    if ((*curmenu).ttlblk.numline > maxtitle)
	  	{
		  men_err ("BAD title block in MDF.", "TAE-BADTITLE",0,0,0,0,0);
		}
	    else
		{
	        disptxt(TITLELIN, 1, &((*curmenu).ttlblk));	/* display title */
	        startlin = TITLELIN+(*curmenu).ttlblk.numline+1; /* give a blank line */
		}
	    }
	code = d_cdirctv(&dirblk, dirctv, record);	/* read next directive */	
 	}
    if (s_equal(dirctv, ".HELP"))			/* if help information */
	code = d_cdirctv(&dirblk, dirctv, record);	/* get next directive*/


    line = startlin;					/* start of text disp*/
    entnum = 0;					
    while(entnum < MAXMENUENT )
	{
	if ( termtype == T_CRT && line > stopline) break;
	if (s_equal(dirctv, ".END") || s_equal(dirctv, ".HELP"))
	    break;					/* no more menu entry*/
	else if (s_equal(dirctv, ".TITLE"))
	    ;						/* ignore .TITLE within text */
	else if (s_equal(dirctv, ".EHELP"))
	    ;						/* skip entry helps  */
	else if (s_equal(dirctv, ".MENU") || s_equal(dirctv, ".PROC")
		|| s_equal(dirctv, ".COMMAND"))
	    {
	    if (NULLSTR(record))
		{
		men_err ("Malformed menu definition file; entry number %3d.",
			 "TAE-MALMDF", entnum+1,0,0,0,0); 
		break;
		}
	    entnum++;				
	    if (!(*curmenu).finit)				/* not initialized*/
		{
		strpqu(record);					/* strip quotes, if any */
		code = addtxt(&((*curmenu).nameblk), record);	/* save name dynamically */
		if (s_equal(dirctv, ".COMMAND"))
		    (*curmenu).type[entnum-1] = MN_COMMAND;
		else
		    (*curmenu).type[entnum-1] = (s_equal(dirctv, ".MENU"))?
				MN_MENU : MN_PROC;      	/* save entry type */
	        f_movpos(&mn_fctx.posctx, &((*curmenu).pos[entnum-1]));
	 	}
	
	     firstlin = TRUE;			/* first line of text*/
	     for (code = SUCCESS; code == SUCCESS;)
		{
		if (termtype == T_CRT && line > stopline) break;
		code = d_text(&dirblk, record);		/* read text record */
		if (code != SUCCESS) break;		/* no more text records */
		addent(record, entnum, firstlin, textrec); /* add entry number*/
		wrttxt(line, 1, textrec, FALSE);	/* write text */
		if (firstlin)
		    {
		    (*curmenu).linenr[entnum-1] = line;
		    (*curmenu).entryline[entnum-1] = s_save (textrec);
		    }
		line++;					
		firstlin = FALSE;			/* not first line */
		}
	    if (firstlin == TRUE)			/* empty menu entry */
		{
		addent ("No description available.", entnum, firstlin, textrec);
		wrttxt (line, 1, textrec, FALSE);
		(*curmenu).linenr[entnum-1] = line;
		(*curmenu).entryline[entnum-1] = s_save (textrec);
		line++;
		}
	    }
	else
	    {
	    men_err ("Unrecognized directive '%s' in menu definition.",
		     "TAE-BADMENDIR", (uintptr_t)dirctv,0,0,0,0);
	    break;
	    }
	code = d_cdirctv(&dirblk, dirctv, record);	/* get next directive*/
        }
    (*curmenu).finit = TRUE;
    (*curmenu).nent = entnum;
    return(SUCCESS);
    }


/*
 *  men_err
 *	All menu errors are printed here.
 */

FUNCTION VOID 	men_err 
(
TEXT		*msg,			/* IN: control string		*/
TEXT		*key,			/* IN: message key		*/
uintptr_t a1,			/* IN: integers or string ptrs	*/
uintptr_t a2,
uintptr_t a3,
uintptr_t a4,
uintptr_t a5
 )
{
wrterr (msg, key, a1, a2, a3, a4, a5);
return;
}
