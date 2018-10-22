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



#define SRCVER	V01.03		/* source file version number		*/
#define	SRCNAM	TUTSELECT	/* source file name			*/

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
* 
* ABSTRACT:
*	This file contains the functions called for the tutor/select 
*	function of TM's TUTOR mode.
*	Tutor/select mode is entered from a TUTOR screen by entering
*	the command "SELECT parm", where "parm" is a PDF parm of type
*	STRING with a list of VALIDs specified.
*	Tutor/select mode brings up a new screen which displays the
*	VALIDs and assists the user in selecting the VALIDs to use as
*	the value for the parm.
*
* USE:	
*
* NOTES:
*
*   1)	TBD 
*
* AUTHOR:
*
*	G. C. Chatters		29-Apr-1987
*
*	Century Computing, Inc
*	1100 West St.
*	Laurel, Maryland	20707
*	(301)-953-3330
*
* CHANGE LOG:
*
* V01.00 29-Apr-87	(GCC)	Initial Release
* V01.01  5-Aug-87	(GCC)	tselaccept: Put quotes around string.
* V01.02  6-Aug-87	(GCC)	GOLD/ARROW function - advance by pages.
*	10-aug-87	New d_remote call...palm
* V01.03 27-Sep-87	(GCC)	Fix display, message and command parse bugs.
*	20-n0v-87	Add t_read_event  and t_input_event() calls...dm
*       03-feb-88       Changed DisplayId to XFACELIFT...tpl 
*	17-oct-88	New interface protocol for getscr...nhe
*	28-jan-89	In prep for new POINTER_VALIDS, remove assumptions
*			that S_RANGE contains a string (as opposed to
*			the new pointer to string)...palm
*	01-feb-89	set starting col of valid string to 1...palm
*       23-may-90	Remove RCJM stuff...ljn
*	27-jun-90	Remove Facelift code...ljn
*	30-apr-91	pr995 fix search in select...tpl
*	05-oct-92	PR1669 support valid size up to 132...tpl
*
--*/


/*----------------------------------------------------------------------*/
/*	Include Files							*/
/*----------------------------------------------------------------------*/

#include	"taeconf.inp"
#include	"terminc.inc"
#include	"tminc.inc"
#include	"helpinc.inc"
#include	"eventinc.inp"
#include	"tmhost.inp"
#include	"dirinc.inc"
#include "taeintproto.h"

/*----------------------------------------------------------------------*/
/*  Global Declarations							*/
/*----------------------------------------------------------------------*/
 

IMPORT	struct  TUTCTX  tutctx;

IMPORT	COUNT	termlines;		/* Number of lines on terminal.	*/
IMPORT	COUNT	termcols;		/* Number of columns.		*/


/*----------------------------------------------------------------------*/
/*  Local Macro Definitions						*/
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*  Local data type definitions.					*/
/*  Define structures needed by select mode.				*/
/*  (Note: Some contents are redundant.)				*/
/*----------------------------------------------------------------------*/

/*	SVRCTX - Select Mode Variable Context structrue.		*/
/*	This context block holds information on the variable being	*/
/*	selected on.							*/

typedef	struct	SVRCTX {
	struct	CONTXT		*p_ctx;  /* Kludge */
	struct	SFILE		*hf;
	struct	VARIABLE	*v;
	struct	S_RANGE		*p_firstvalid;
	struct	S_RANGE		*p_lastvalid;
	struct	S_RANGE		*p_prmsetvalid;
	COUNT			nvalids;
	unsigned		intrinsic : 1;
	unsigned		l1hexist   : 1;
	TEXT			*pdfname;
	TEXT			*subcmd;
	TEXT			*parmname;
	TEXT			parmspec[STRINGSIZ+1];
	struct	TXTSTOR		l1help;
	COUNT			varinx;
	}  T_SVRCTX;

/*	VDRECT - Video display rectangle structure.			*/
/*	Difines size and position of a region on the VDT.		*/

typedef	struct	VDRECT {
	COUNT	x;
	COUNT	y;
	COUNT	dx;
	COUNT	dy;
	} T_VDRECT;

/*	SDPCTX - Select Mode Display Context structure.			*/
/*	This context block specifies changes to the display and		*/
/*	maintains the current status of the display.			*/

typedef	struct	SDPCTX {
		unsigned		dispreq : 1;
		struct	TITLE  {
			unsigned		dispreq : 1;
			unsigned		intrinsic : 1;
			TEXT			*pdfname;
			TEXT			*subcmd;
			TEXT			*parmspec;
			struct	VDRECT		rect;
			struct	TITLE_VAR {
				COUNT		pagnum;
				unsigned	lastflag : 1;
			} cur, upd;
		} title;

		struct	HELP1 {
			unsigned		dispreq : 1;
			unsigned		l1hexist : 1;
			struct	VDRECT		rect;
			struct	TXTSTOR		l1help;
		} help1;

		struct	VALIDS {
			unsigned		dispreq : 1;
			struct	VDRECT		rect;
			struct	S_RANGE		*p_firstvalid;
			struct	S_RANGE		*p_lastvalid;
			struct	VALIDS_VAR {
				struct	S_RANGE	*p_topvalid;
				struct	S_RANGE	*p_actvalid;
			} cur, upd;
		} valids;

		struct	CMDREQ {
			unsigned		dispreq : 1;
			struct	VDRECT		rect;
			TEXT			*cmdreqtext;
		} cmdreq;

	}  T_SDPCTX;


/*	SELCTX - Select Mode Context structure.				*/
/*	Tutor select mode keeps one copy of this structure to maintain	*/
/*	its status.  In general, SELCTX retains values that must be	*/
/*	remembered for more than once iteration of the main loop.	*/
/*	Other parameters may be local data values.			*/

typedef	struct	SELCTX {
	unsigned		active : 1;	/* IF select active.	*/
	TEXT			prev_cmd[CMDLINSIZ+1];
	TEXT			next_cmd[CMDLINSIZ+1];
	struct	SVRCTX		svrctx;	/* Context of variable.		*/
	struct	SDPCTX		sdpctx;	/* Contest of select display	*/
	}  T_SELCTX;

typedef	struct	SELCMD {	/* Structure for SELECT command table.	*/
	TINY	abbchr;		/* Min abbreviation, 0 => all.		*/
	TEXT	cmdname[NAMESIZ+1];	/* Command name.		*/
	TINY	numparm;	/* Num parameters, -1 => any		*/
        CODE	(*selfunc)(struct SELCTX *, TEXT *, TEXT *);	/* Command function.			*/
	} T_SELCMD;

/*----------------------------------------------------------------------*/
/*  Local Data Definitions						*/
/*----------------------------------------------------------------------*/

/*  Command request line.						*/

static	TEXT	cmdreqdata[] = 
    "Enter: ?, up-arrow, down-arrow, SEARCH, HELP, ACCEPT, EXIT";

/*  Error/information messages and keys.				*/

static	TEXT	key_tnotfound[]  = "TAE-TNOTFOUND";
static	TEXT	msg_tnotfound[]  = "The string '%s' could not be found.";
static	TEXT	key_tselnotscr[] = "TAE-TSELNOTSCR";
static	TEXT	msg_tselnotscr[] = 
                        "SELECT display is not available in NOSCREEN tutor.";
static	TEXT	key_tselnavail[] = "TAE-TSELNAVAIL";
static	TEXT	msg_tselnavail[] = 
			"SELECT display not available for parameter '%s'";
static	TEXT	key_suberr[] = "TAE-TUTSUB";
static	TEXT	msg_suberr[] = "Error using '&' for string substitution.";
static	TEXT	key_tscmd[] = "TAE-TSLUNTCMD";
static	TEXT	msg_tscmd[] = "Unrecognized command for select mode.";
static	TEXT	msg_ambi[] = "Ambiguous parameter abbreviation, '%s'.";
static	TEXT	key_ambi[] = "TAE-AMBIGPAR";
static	TEXT	msg_unre[] = "Unrecognized parameter name, '%s'.";
static	TEXT	key_unre[] = "TAE-TUNRECPAR";
static	TEXT	msg_mmov[] = "Terminal Monitor internal memory overflow.";
static	TEXT	key_mmov[] = "TAE-MEMOVR";	/* dyn mem overflow in tutor*/
static	TEXT	msg_hrde[] = "Error reading help file. %s";
static	TEXT	key_hrde[] = "TAE-RDERR";	/* help file read error in tutor*/
static	TEXT	msg_ambs[] = "Ambiguous subcommand abbreviation, '%s'.";
static	TEXT	key_ambs[] = "TAE-AMBIGSUB";
static	TEXT	msg_unrs[] = "Unrecognized subcommand, '%s'.";
static	TEXT	key_unrs[] = "TAE-UNRECSUB";

/*  Data tables for select mode commands.				*/

FUNCTION CODE gen_line_editor
(
COUNT	line_num,	/* in: line number on screen		*/
			/*     (-1 if line number not known)	*/
TEXT	inistr[],	/* In: Initial input string.		*/
TEXT 	prompt[],	/* in: prompt string			*/
TEXT	line[]		/* out: user-typed line			*/
 );
FUNCTION static CODE tselexit
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 );
FUNCTION static CODE tselmsghlp
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 );
FUNCTION struct	S_RANGE	*findvalid
(TEXT		*string,
struct	S_RANGE	*p_valids,
COUNT		nvalids
 );
FUNCTION static CODE tselhlp
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 );
FUNCTION CODE sdispupd
(struct	SDPCTX	*p_sdpctx
 );
FUNCTION  CODE  gen_line_screen
(
COUNT	line_num,		/* IN: input line number	     */
TEXT 	prompt[],		/* IN: prompt string		     */
TEXT	inistr[],		/* IN: Initial input string.	     */
TEXT  	line[],			/* OUT: line from interactive source */
CODE	*term			/* OUT: line terminator 	     */
 );
FUNCTION CODE sdispupd_cmdreq
(struct	CMDREQ	*p_cmdreq
 );
FUNCTION CODE gen_cmd_editor
(
COUNT	line_num,	/* in: line number on screen		*/
			/*     (-1 if line number not known)	*/
TEXT 	prompt[],	/* in: prompt string			*/
TEXT	inistr[],	/* In: Initial input string.		*/
TEXT	line[]		/* out: user-typed line			*/
 );
FUNCTION CODE sdispupd_valids
(struct	VALIDS	*p_valids
 );
FUNCTION CODE sdispupd_help1
(struct	HELP1	*p_help1
 );
FUNCTION CODE sdispupd_title
(struct	TITLE	*p_title
 );
FUNCTION static struct SELCMD *srchseltab 
(TEXT		cmdstr[]	/* in:  command string			*/
 );
FUNCTION struct SELCMD *gtprvselcmd
(TEXT	*prvcmdstr,		/* IN: Previous mode command string.	*/
TEXT	*selcmdstr,		/* OUT: Command string.			*/
TEXT	*selcmdprm		/* OUT: First command parameter.	*/
 );
FUNCTION struct SELCMD *gtselcmd
(TEXT	*inicmdstr,		/* IN: Initial command string.		*/
TEXT	*selcmdstr,		/* OUT: Command string.			*/
TEXT	*selcmdprm		/* OUT: First command parameter.	*/
 );
FUNCTION struct SELCMD *gtselcmd
(TEXT	*inicmdstr,		/* IN: Initial command string.		*/
TEXT	*selcmdstr,		/* OUT: Command string.			*/
TEXT	*selcmdprm		/* OUT: First command parameter.	*/
 );
FUNCTION CODE gen_cmd_screen
(TEXT		inicmdstr[],	/* In:  Initial command string.		*/
TEXT		cmdstr[],	/* Out: Command string.			*/
CODE		*terminat	/* Out: Command terminator.		*/
 );
FUNCTION static CODE tselincact
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 );
FUNCTION static CODE tseldecact
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 );
FUNCTION static CODE tselaccept
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 );
FUNCTION static CODE tselsearch
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 );
FUNCTION VOID selmsg 
(
TEXT		msg[],
TEXT		key[],
uintptr_t a1,
uintptr_t a2,
uintptr_t a3,
uintptr_t a4,
uintptr_t a5
 );
FUNCTION CODE tutselect_main
(struct SELCTX	*p_selctx	/* in:  SELECT mode context.		*/
 );
FUNCTION CODE tutselect_init
(
struct	SELCTX		*p_selctx,	/* IN: SELECT context.		*/
struct	CONTXT		*pctx,	/* in:  proc context			*/
struct	VARIABLE	*v,	/* in: VARIABLE being selected on.	*/
COUNT			subscr,	/* In: Subscript of variable selected.	*/
struct	SFILE		*hf	/* In: Help file.			*/
 );

static	struct	SELCMD	scmd[] = {
	{ 0, "?",      0,  tselmsghlp },
	{ 1, "HELP",   -1, tselhlp    },
	{ 6, "INCACT", -1, tselincact },
	{ 6, "DECACT", -1, tseldecact },
	{ 1, "EXIT",   0,  tselexit   },
	{ 1, "ACCEPT", 0,  tselaccept },
	{ 1, "SEARCH", 1,  tselsearch }
	};

/*----------------------------------------------------------------------*/
/*  Local Symbol Definitions						*/
/*----------------------------------------------------------------------*/

#define	NSCMD	( sizeof( scmd ) / sizeof( T_SELCMD ) )


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tutselect
*
* DESCRIPTION:
*	This function is the initial function called for TUTOR/SELECT.
*	It is called by tutor mode.
*	It checks for the validity of the request.  
*
* USE:	tutselect is called when the SELECT option is requested from 
*	tutor mode.  A specific parameter must be specified.
*
* PARAMETERS:
*
* RETURN VALUE:
*	tutselect returns a status of type CODE.
*	Values are:
*		SUCCESS - Executed successfully.
*		FAIL    - There was an error in SELECT request.
*
* CONTROL:
*	Check for SCREEN mode.
*	Extract parameter name and subscript from command string.
*	Get pointer to parm being SELECTed on.
*	   Use command parameter and look up.
*	   If no command parameter, use current tutor edit parameter.
*	Check for "parm"'s type is STRING.
*	Check for "parm" having VALID list.
*	If all checks pass, call tutselect_init to initialize tutor/select
*	Prepare to restore tutor/parameter or
*	 tutor/qualify display.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/

FUNCTION CODE tutselect
(
TEXT		cmdstr[],	/* in:  command string			*/
TEXT		cmdprm[],	/* in:  the command parameter if any	*/
struct CONTXT	*pctx,		/* in:  proc context			*/
struct SFILE	*hf		/* in/out: help file control block	*/
 )
{
    struct	SELCTX		selctx;

    CODE  	code;
    COUNT 	pagnum;
    struct	VARIABLE	*v;
    struct	SYNBLK		sb;
    TEXT			token[TOKESIZ+1];
    TEXT			name[TOKESIZ+1];
    COUNT			subscr, maxc;
/*----------------------------------------------------------------------*/
/*  Check for valid request for SELECT screen.				*/
/*----------------------------------------------------------------------*/

/*  Check for TUTOR running in SCREEN mode.				*/

    if( !tutctx.screen )
    {
      tutmsg( msg_tselnotscr, key_tselnotscr, 0, 0, 0, 0, 0 );
        return( FAIL );
    }

/*  Extract parameter name and subscript from command string.		*/

/*  Initialize name and subscript.					*/

    name[0] = EOS;

    subscr = 1;

/*  Initialize syntax package, position past command verb (i.e."SELECT").*/

    initok( &sb, cmdstr );

    code = getvrb( &sb, token );

/*  Extract name if it exists.						*/

    if( *sb.curchr != EOS )
    {
        do code = gettok( &sb, token ); while ( code == S_WHITE );

        if( code == S_ALPHA )
            s_copy( token, name );
        else
        {
	  selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
            return( FAIL );
        }
    }

/*  Now extract subscript if it exists.				*/

    if( *sb.curchr != EOS )
    {
        do code = gettok( &sb, token ); while ( code == S_WHITE );
        if( code == EOS )
            ;
        else if( code == '(' )
        {
            do code = gettok( &sb, token ); while ( code == S_WHITE );
            if( code == S_ALPHA )
            {
                if( ( code = s_s2i( token, &subscr )) != SUCCESS )
                {
		  selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
                    return( FAIL );
                }
            }
            else
            {
	      selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
                return( FAIL );
            }
        }
        else
        {
	  selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
            return( FAIL );
        }
    }

/*  Get pointer to variable for parm being selected.			*/
/*	If name is null, use current tutor editor variable.		*/
/*      Else, Look up name in parameter list.				*/

    if( NULLSTR( name ) )
    {
        subscr = tutctx.index + 1;
        v = tutctx.vcur;
        if( v == NULL )
        {
	  selmsg(msg_unre, key_unre, (uintptr_t) name, 0, 0, 0, 0 );
            return( FAIL );
        }
    }
    else
    {
        code = lookab( &(*pctx).parmst, name, &v );

        if (code == AMBIG)
        {
	  selmsg(msg_ambi, key_ambi, (uintptr_t) name, 0, 0, 0, 0);
            return( FAIL );
        }
        else if( code == FAIL )
        {
	    selmsg(msg_unre, key_unre, (uintptr_t) name, 0, 0, 0, 0);
            return( FAIL );
        }
    }

/*  Check that parm is of type STRING.					*/

    if( (*v).v_type != V_STRING )
    {
      tutmsg( msg_tselnavail, key_tselnavail, (uintptr_t) (*v).v_name, 0, 0, 0, 0 );
        return( FAIL );
    }

/*  Check that parm has VALID list specified.				*/

    if( (*v).v_valid == NULL )
    {
      tutmsg( msg_tselnavail, key_tselnavail, (uintptr_t) (*v).v_name, 0, 0, 0, 0 );
        return( FAIL );
    }

/*  Get page number of selected parameter.  This gets help.		*/

    if( getpgnum( pctx, (*v).v_name, &pagnum, hf ) != SUCCESS )
        return( FAIL );
/*----------------------------------------------------------------------*/
/*  Request is valid.  Call tutselect_init to initiate select mode.	*/
/*----------------------------------------------------------------------*/

    code = tutselect_init( &selctx, pctx, v, subscr, hf );

/*----------------------------------------------------------------------*/
/*  Tutor/Select display is now done, prepare to restore		*/
/*  tutor/parameter  or tutor/qualify display.				*/
/*----------------------------------------------------------------------*/
    tutctx.dispreq = TRUE;
    tutctx.start = TRUE;
    tutctx.curpag = pagnum;
    tutctx.vcur   = v;

    maxc = (*v).v_maxc;
    tutctx.index  = min( subscr, maxc - 1 );

    if( !NULLSTR( selctx.next_cmd ) )
    {
        s_copy( selctx.next_cmd, tutctx.prev_cmd );
        return( TUT_PREV );
    }
    else
        return ( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tutselect_init
*
* DESCRIPTION:
*	This function initializes Tutor/Select mode.
*
* USE:	This function is called by tutselect after it has been determined
*	that the reqeust for select mode is valid.
*
* PARAMETERS:
*
* RETURN VALUE:
*	tutselect returns a status of type CODE.
*	Values are:
*		SUCCESS - Executed successfully.
*		FAIL    - There was an error in SELECT request.
*
* CONTROL:
*	Zero entire Select Context structure.
*	Set constant values for VARIABLE context.
*	Set initial variable values for VARIABLE context.
*	Set constant values for display context.
*	Set initial variable values for display context.
*	Call tutselect_main to run the display.
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/

FUNCTION CODE tutselect_init
(
struct	SELCTX		*p_selctx,	/* IN: SELECT context.		*/
struct	CONTXT		*pctx,	/* in:  proc context			*/
struct	VARIABLE	*v,	/* in: VARIABLE being selected on.	*/
COUNT			subscr,	/* In: Subscript of variable selected.	*/
struct	SFILE		*hf	/* In: Help file.			*/
 )
{
    struct	SVRCTX		*p_svrctx;
    struct	SDPCTX		*p_sdpctx;
    struct	TITLE		*p_title;
    struct	HELP1		*p_help1;
    struct	VALIDS		*p_valids;
    struct	CMDREQ		*p_cmdreq;

    COUNT	count;
    COUNT	maxc;
    TEXT	**valvec;

/*----------------------------------------------------------------------*/
/*  Zero entire Select Context structure.				*/
/*----------------------------------------------------------------------*/

    zero_block( (GENPTR) p_selctx, sizeof( struct SELCTX ) );

/*----------------------------------------------------------------------*/
/*  Indicate that select mode is active.				*/
/*----------------------------------------------------------------------*/

    (*p_selctx).active = TRUE;

/*----------------------------------------------------------------------*/
/*  Clear previous mode command string.					*/
/*----------------------------------------------------------------------*/

    (*p_selctx).prev_cmd[0] = EOS;

/*----------------------------------------------------------------------*/
/*  Set constant values for VARIABLE context.				*/
/*----------------------------------------------------------------------*/

    p_svrctx = &(*p_selctx).svrctx;

    (*p_svrctx).p_ctx   =   pctx;   /* Kludge */
    (*p_svrctx).v = v;
    (*p_svrctx).p_firstvalid = (*(struct S_VALID *)(*v).v_valid).slist;
    (*p_svrctx).nvalids      = (*(struct S_VALID *)(*v).v_valid).count;
    (*p_svrctx).p_lastvalid  = 
     (*p_svrctx).p_firstvalid + (*p_svrctx).nvalids - 1;

    (*p_svrctx).intrinsic = (*pctx).intrinsic;
    (*p_svrctx).pdfname   = (*pctx).pdf.name;
    (*p_svrctx).subcmd    = (*pctx).subcmd;
    (*p_svrctx).parmname  = (*v).v_name;
    (*p_svrctx).varinx    = subscr;
    maxc  = (*v).v_maxc;
    if( maxc > 1 )
        sprintf( (*p_svrctx).parmspec, "%s( %d )", (*v).v_name, subscr );
    else
        sprintf( (*p_svrctx).parmspec, "%s", (*v).v_name );

    (*p_svrctx).l1hexist   = (*(*v).v_tp).l1hexist;
    (*p_svrctx).l1help    = (*(*v).v_tp).l1help;
    (*p_svrctx).hf        = hf;

/*----------------------------------------------------------------------*/
/*  Set initial variable values for VARIABLE context.			*/
/*----------------------------------------------------------------------*/

    count = (*v).v_count;
    maxc  = (*v).v_maxc;
    valvec = (TEXT **)(*v).v_cvp;

    if( subscr > count )
    {
        (*p_svrctx).p_prmsetvalid = NULL;
    }
    else
    {
        (*p_svrctx).p_prmsetvalid = findvalid( valvec[subscr-1], 
                                               (*p_svrctx).p_firstvalid, 
                                               (*p_svrctx).nvalids );
    }
/*----------------------------------------------------------------------*/
/*  Set constant values for display context.				*/
/*----------------------------------------------------------------------*/

    p_sdpctx = &(*p_selctx).sdpctx;

/*  Set location and size of each display item.				*/

    (*p_sdpctx).title.rect.x  =  1;
    (*p_sdpctx).title.rect.y  =  1;
    (*p_sdpctx).title.rect.dx = 80;
    (*p_sdpctx).title.rect.dy =  1;

    (*p_sdpctx).help1.rect.x  = 10;
    (*p_sdpctx).help1.rect.y  =  3;
    (*p_sdpctx).help1.rect.dx = 80;
    (*p_sdpctx).help1.rect.dy =  1;

    (*p_sdpctx).valids.rect.x  =  1; 
    (*p_sdpctx).valids.rect.y  =  5;
    (*p_sdpctx).valids.rect.dx = 24;
    (*p_sdpctx).valids.rect.dy = 17;

    (*p_sdpctx).cmdreq.rect.x  = 1;
    (*p_sdpctx).cmdreq.rect.y  = termlines - PMTLIN - 1;
    (*p_sdpctx).cmdreq.rect.dx = 80;
    (*p_sdpctx).cmdreq.rect.dy = 1;

/*  Set constant items in title item.					*/

    p_title = &(*p_sdpctx).title;

    (*p_title).intrinsic = (*p_svrctx).intrinsic;
    (*p_title).pdfname   = (*p_svrctx).pdfname;
    (*p_title).subcmd    = (*p_svrctx).subcmd;
    (*p_title).parmspec  = (*p_svrctx).parmspec;

    (*p_title).upd.pagnum = -1;
    (*p_title).upd.lastflag = TRUE;

/*  Set constant items in help item.					*/

    p_help1 = &(*p_sdpctx).help1;

    (*p_help1).l1hexist  =  (*p_svrctx).l1hexist;
    (*p_help1).l1help   =  (*p_svrctx).l1help;

/*  Set constant items in valid data display item.			*/

    p_valids = &(*p_sdpctx).valids;

    (*p_valids).p_firstvalid = (*p_svrctx).p_firstvalid;
    (*p_valids).p_lastvalid  = (*p_svrctx).p_lastvalid;

/*  Set constant items in command request line item.			*/

    p_cmdreq = &(*p_sdpctx).cmdreq;

    (*p_cmdreq).cmdreqtext = cmdreqdata;

/*----------------------------------------------------------------------*/
/*  Set initial variable values for display context.			*/
/*----------------------------------------------------------------------*/
    (*p_sdpctx).dispreq = TRUE;

/*  Set variable items for title.					*/
/*  ---There are no variable items in title item---			*/

/*  Set variable items in help item.					*/
/*  ---There are no variable items in help item---			*/

/*  Set variable items for VALIDs data display.				*/

    (*p_valids).upd.p_actvalid = (*p_svrctx).p_prmsetvalid;

    if( (*p_valids).upd.p_actvalid == NULL )
        (*p_valids).upd.p_topvalid = (*p_svrctx).p_firstvalid;
    else
    {
        (*p_valids).upd.p_topvalid = max(
            (*p_valids).p_firstvalid,
            (*p_valids).upd.p_actvalid - ((*p_valids).rect.dy - 2 - 1)
            );
    }

/*----------------------------------------------------------------------*/
/*  Call tutselect_main to run SELECT mode.				*/
/*----------------------------------------------------------------------*/

    return( tutselect_main( p_selctx ) );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tutselect_main
*
* DESCRIPTION:
*	This function contains the main control loop for the
*	tutor select mode of TM.  The loop is executed once for
*	each valid command generated at the keyboard.  
*	At this level a command consists of a character string 
*	terminated by a carraige return.  If the command verb is
*	invalid, it will be rejected before it gets back to this level.
*
* USE:	tutselect_main is called after tutselect has determined
*	that the SELECT request is valid.
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	LOOP until select mode goes inactive.
*	    Do physical update of display.
*	    Get next command from user.
*	    Execute command.
*	END-LOOP
*
* NOTES:
*
*   1)	Physical update of the screen occurs once each time through
*	the loop.  Command functions may request changes to the
*	display by changing values in the display context structures
*	but all actual changes are done by the sdispupd function.
*
* SYNOPSIS:
--*/
FUNCTION CODE tutselect_main
(struct SELCTX	*p_selctx	/* in:  SELECT mode context.		*/
 )
{
  struct	SELCMD	*selcmd;
    CODE	code;
    TEXT	selcmdstr[CMDLINSIZ+1];
    TEXT	selcmdprm[CMDLINSIZ+1];

    static	TEXT	inicmdstr[STRINGSIZ+1] = "";
/*----------------------------------------------------------------------*/
/*  Run command input and execution loop for SELECT mode.		*/
/*----------------------------------------------------------------------*/
    while( (*p_selctx).active )
    {
/*      Update display before executing next command.			*/

        sdispupd( &(*p_selctx).sdpctx );

/*      Get next valid command.						*/
/*      Either previous mode command from HELP or user command.		*/

        if( NULLSTR( (*p_selctx).prev_cmd ) )
        {
            selcmd = gtselcmd( inicmdstr, selcmdstr, selcmdprm );
        }
        else
        {
            selcmd = gtprvselcmd( (*p_selctx).prev_cmd, selcmdstr, selcmdprm );
            (*p_selctx).prev_cmd[0] = EOS;
        }

/*      Execute command.						*/

        if( selcmd != NULL )
            code = 
             (*(*selcmd).selfunc)( p_selctx, selcmdstr, selcmdprm );
    }

    return ( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gtselcmd
*
* DESCRIPTION:
*	This function gets the next command from the user for SELECT
*	mode.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	LOOP until valid command obtained.
*	    Get full command line from user.
*	    Do "&" substitution.
*	    Look up command in table.
*	END-LOOP
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION struct SELCMD *gtselcmd
(TEXT	*inicmdstr,		/* IN: Initial command string.		*/
TEXT	*selcmdstr,		/* OUT: Command string.			*/
TEXT	*selcmdprm		/* OUT: First command parameter.	*/
 )
{
    IMPORT	struct	CONTXT	primctx;
    CODE	terminat;

    CODE	code;
    struct	SELCMD	*cmd;
    TEXT	errmsg[STRINGSIZ+1];
    TEXT	errkey[KEYSIZ+1];
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*  LOOP until valid command obtained.					*/
/*----------------------------------------------------------------------*/

    while( FOREVER )
    {

/*      Get full command line from user.				*/

        code = gen_cmd_screen( inicmdstr, selcmdstr, &terminat );

        if( terminat == T_ESCAPE ) continue;  /* For TEST */
        if( code != SUCCESS ) continue;

/*      Do "&" substitution.						*/

        code = substitute( selcmdstr, CMDLINSIZ, (GENPTR) &primctx, FALSE );

        if( code != SUCCESS )
        {
	  selmsg( msg_suberr, key_suberr, 0, 0, 0, 0, 0 );
            continue;
        }

/*      Look up command in table.					*/

        cmd = srchseltab( selcmdstr );

        if( cmd == NULL )
        {
            selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
            continue;
        }

/*      Check number of parameters, extract first.			*/

        if( (code = intprm( (*cmd).numparm, (*cmd).cmdname,
         selcmdstr, selcmdprm, errmsg, errkey )) != SUCCESS )
        {
            selmsg( errmsg, errkey, 0, 0, 0, 0, 0 );
            continue;
        }

        break;	/* If we got this far, we have a valid command.		*/
    }

    return( cmd );
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gtprvselcmd
*
* DESCRIPTION:
*	This function gets the next command for SELECT mode from a command
*	entered as previous mode command (usually from HELP display).
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	Get command line (a parameter to this function).
*	Do "&" substitution.
*	Look up command in table.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION struct SELCMD *gtprvselcmd
(TEXT	*prvcmdstr,		/* IN: Previous mode command string.	*/
TEXT	*selcmdstr,		/* OUT: Command string.			*/
TEXT	*selcmdprm		/* OUT: First command parameter.	*/
 )
{
    IMPORT	struct	CONTXT	primctx;

    CODE	code;
    struct	SELCMD	*cmd;
    TEXT	errmsg[STRINGSIZ+1];
    TEXT	errkey[KEYSIZ+1];
/*----------------------------------------------------------------------*/

/*  Copy command line from previoud mode buffer.			*/

    s_copy( prvcmdstr, selcmdstr );

/*  Do "&" substitution.						*/

    code = substitute( selcmdstr, CMDLINSIZ, (GENPTR) &primctx, FALSE );

    if( code != SUCCESS )
    {
        selmsg( msg_suberr, key_suberr, 0, 0, 0, 0, 0 );
        return( NULL );
    }

/*  Look up command in table.					*/

    cmd = srchseltab( selcmdstr );

    if( cmd == NULL )
    {
        selmsg( msg_tscmd, key_tscmd, 0, 0, 0, 0, 0 );
        return( NULL );
    }

/*  Check number of parameters, extract first.			*/

    if( (code = intprm( (*cmd).numparm, (*cmd).cmdname,
    selcmdstr, selcmdprm, errmsg, errkey )) != SUCCESS )
    {
        selmsg( errmsg, errkey, 0, 0, 0, 0, 0 );
        return( NULL );
    }

    return( cmd );
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselexit
*
* DESCRIPTION:
*	This function is called when the user wants to exit from
*	SELECT mode.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselexit
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 )
{
    (*p_selctx).active = FALSE;

    return SUCCESS;
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselmsghlp
*
* DESCRIPTION:
*	This function is one of the SELECT mode command functions.
*	It displays help on the last error message.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselmsghlp
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 )
{
    IMPORT  TEXT	lastkey[];	/* key of last displayed message	*/

    struct	SDPCTX	*p_sdpctx = &(*p_selctx).sdpctx;

    CODE		code;
    struct  HELPBLK	helpblk;	/* help control block			*/
    struct  SFILE	fctx;		/* File context block for msg_help */
/*----------------------------------------------------------------------*/
    helpblk.nextcmd[0] = EOS;				/* initialize	*/

    code = msg_help( &fctx, lastkey, &helpblk );/* display help data on key */

    if (code != SUCCESS)
    {
	selmsg(helpblk.errmsg, helpblk.errkey, 0, 0, 0, 0, 0);		/* print error message	*/
	return(FAIL);
    }

    (*p_sdpctx).dispreq = TRUE;

    if (!NULLSTR(helpblk.nextcmd))
    {
        s_copy(helpblk.nextcmd, (*p_selctx).prev_cmd);
	code = TUT_PREV;			/* previous mode cmd 	*/
    }

    return(code);
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselincact
*
* DESCRIPTION:
*	This function is one of the SELECT mode command functions.
*	This function increments the current active valid.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF active (highlighted) valid is NULL THEN
*	    Set active valid to be first valid.
*	ELSE IF command parameter is "PAGE" THEN
*	    Increment active valid by number on page.
*	    IF it goes off end of valids THEN
*	        Set active valid to last valid.
*	    Set top valid to that active is now at bottom of display.
*	ELSE
*	    Increment active valid by one.
*	    If it goes off end of page THEN
*	        Increment top valid by one.
*	    If it goes off end of valids THEN
*	        Wrap around to first.
*	        Set topvalid to firstvalid.
*	Set parm set valid to active valid.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselincact
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 )
{
    struct	SVRCTX	*p_svrctx = &(*p_selctx).svrctx;
    struct	SDPCTX	*p_sdpctx = &(*p_selctx).sdpctx;

    struct	VALIDS	*p_valids = &(*p_sdpctx).valids;
/*----------------------------------------------------------------------*/

/*  IF active (highlighted) valid is NULL THEN 				*/
/*      Set active to first.						*/

    if( (*p_valids).upd.p_actvalid == NULL )
        (*p_valids).upd.p_actvalid = (*p_valids).p_firstvalid;

/*  ELSE IF command parameter is "PAGE" THEN				*/

    else if( s_equal( selcmdprm, "PAGE" ) )
    {
/*      Increment active valid by number on page.			*/

        (*p_valids).upd.p_actvalid += (*p_valids).rect.dy - 2;

/*      IF active valid goes off end of valids THEN			*/
/*          Set active valid to last valid.				*/

        if( (*p_valids).upd.p_actvalid > (*p_valids).p_lastvalid )
            (*p_valids).upd.p_actvalid = (*p_valids).p_lastvalid;

/*      Set top valid to that active is now at bottom of display.	*/

        (*p_valids).upd.p_topvalid = 
         (*p_valids).upd.p_actvalid - ((*p_valids).rect.dy - 2 - 1);

        if( (*p_valids).upd.p_topvalid < (*p_valids).p_firstvalid )
            (*p_valids).upd.p_topvalid = (*p_valids).p_firstvalid;
    }

/*  ELSE 								*/
/*      Increment active valid by one.					*/

    else
    {
        (*p_valids).upd.p_actvalid += 1;

/*      If it goes off end of page THEN					*/
/*          Increment top valid by one.					*/

        if( (*p_valids).upd.p_actvalid > 
            (*p_valids).upd.p_topvalid + (*p_valids).rect.dy - 2 - 1 )
        {
            (*p_valids).upd.p_topvalid += 1;
        }

/*      If active (highlighted) valid goes off end THEN			*/
/*          Wrap around to first.					*/
/*          Set topvalid to firstvalid.					*/

        if( (*p_valids).upd.p_actvalid > (*p_valids).p_lastvalid )
        {
            (*p_valids).upd.p_actvalid = (*p_valids).p_firstvalid;
            (*p_valids).upd.p_topvalid = (*p_valids).p_firstvalid;
        }
    }

/*      Set parm set valid to active valid.				*/

    (*p_svrctx).p_prmsetvalid = (*p_valids).upd.p_actvalid;

    return( SUCCESS );
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tseldecact
*
* DESCRIPTION:
*	This function is one of the SELECT mode command functions.
*	This function decrements the current active valid.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF active (highlighted) valid is NULL THEN
*	    Set active valid to be first valid.
*	ELSE IF command parameter is "PAGE" THEN
*	    Decrement active valid by number on page.
*	    IF it goes off top of valids THEN
*	        Set active valid to first valid.
*	    Set topvalid to active valid.
*	ELSE
*	    Decrement active valid by one.
*	    If it goes off top of page THEN
*	        Set top valid to active valid.
*	    If it goes off top of valid list THEN
*	        Set active valid to last valid.
*	        Set top valid to last valid - # on page or first valid.
*	Set parm set valid to active valid.
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tseldecact
(struct	SELCTX	*p_selctx,
TEXT	*selcmdstr,
TEXT	*selcmdprm
 )
{
    struct	SVRCTX	*p_svrctx = &(*p_selctx).svrctx;
    struct	SDPCTX	*p_sdpctx = &(*p_selctx).sdpctx;

    struct	VALIDS	*p_valids = &(*p_sdpctx).valids;

/*----------------------------------------------------------------------*/
    p_sdpctx = &(*p_selctx).sdpctx;
    p_valids = &(*p_sdpctx).valids;

/*  IF active (highlighted) valid is NULL THEN				*/
/*      Set active valid to be first valid.				*/

    if( (*p_valids).upd.p_actvalid == NULL )
        (*p_valids).upd.p_actvalid = (*p_valids).p_firstvalid;

/*  ELSE IF command parameter is "PAGE" THEN				*/

    else if( s_equal( selcmdprm, "PAGE" ) )
    {
/*      Decrement active valid by number on page.			*/

        (*p_valids).upd.p_actvalid -= (*p_valids).rect.dy - 2;

/*      IF active valid goes off top of valids THEN			*/
/*          Set active valid to first valid.				*/

        if( (*p_valids).upd.p_actvalid < (*p_valids).p_firstvalid )
            (*p_valids).upd.p_actvalid = (*p_valids).p_firstvalid;

/*      Set topvalid to active valid.					*/

        (*p_valids).upd.p_topvalid = (*p_valids).upd.p_actvalid;
    }

/*  ELSE								*/

    else
    {
/*      Decrement active valid by one.					*/

        (*p_valids).upd.p_actvalid -= 1;

/*      If it goes off top of page THEN					*/
/*          Set top valid to active valid.				*/

        if( (*p_valids).upd.p_actvalid < (*p_valids).upd.p_topvalid )
        {
            (*p_valids).upd.p_topvalid = (*p_valids).upd.p_actvalid;
        }

/*      If it goes off top of valid list THEN				*/
/*          Set active valid to last valid.				*/
/*          Set top valid to last valid - # on page or first valid.	*/

        if( (*p_valids).upd.p_actvalid < (*p_valids).p_firstvalid )
        {
            (*p_valids).upd.p_actvalid = (*p_valids).p_lastvalid;

            (*p_valids).upd.p_topvalid = 
            (*p_valids).p_lastvalid - ((*p_valids).rect.dy - 2 - 1);
            if( (*p_valids).upd.p_topvalid < (*p_valids).p_firstvalid )
                (*p_valids).upd.p_topvalid = (*p_valids).p_firstvalid;
        }
    }

/*  Set parm set valid to active valid.					*/

    (*p_svrctx).p_prmsetvalid = (*p_valids).upd.p_actvalid;

    return( SUCCESS );
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselhlp
*
* DESCRIPTION:
*	This function displays help information for the user
*	from select mode.
*	This is derived from the tuthlp function.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF no parameter THEN
*	   Display general help for select mode.
*	ELSE IF parameter is "*" THEN
*	   Display help on the proc
*	ELSE IF parameter is a string that is the name of a parameter THEN
*	   Display help for the parameter.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselhlp
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 )
{

/*
 * NOTE: In case of error, formatted error message and key are returned by
 * help functions in helpblk struct.
 *
 */


    struct CONTXT	*pctx = (*p_selctx).svrctx.p_ctx;
    struct SFILE	*hf = (*p_selctx).svrctx.hf;

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


/*----------------------------------------------------------------------*/

    code = SUCCESS;
    helpblk.nextcmd[0] = EOS;		/* initialize for prev mode command   */
    helpblk.compiled = FALSE;
    initxt(&nultitle);
/*----------------------------------------------------------------------*/
/*  IF HELP command had no parameter THEN display select mode HELP.	*/
/*----------------------------------------------------------------------*/

    if (NULLSTR(selcmdprm))
    {
        s_copy( "fmtvalsel", buf );
	code = f_open(&tutmhlp, TUTMHELPLUN, HELPLIB, buf, HLP_TYPE, F_READ);
	if (code != SUCCESS) goto open_err;
	s_copy(buf, helpblk.helpobj);
	proc_help(&tutmhlp,HELPLIB, "", "", "", "", "", &helpblk);
	f_close(&tutmhlp, F_KEEP);
	if (code != SUCCESS) goto help_err;		
    }
/*----------------------------------------------------------------------*/
/*  IF HELP command had "*" as parameter THEN display proc help.	*/
/*----------------------------------------------------------------------*/
    else if (s_lseq("*", selcmdprm))	/* help on current proc desired	      */
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
/*----------------------------------------------------------------------*/
/*  OTHERWISE HELP parameter is parameter name, display its help.	*/
/*----------------------------------------------------------------------*/
    else				/* HELP command had a parameter		*/
	{
	if ((*pctx).compiled)
	    helpblk.compiled = TRUE;	/* flag if help on parm in comp'd PDF	*/
	if (tutctx.subtut || s_lseq("-", selcmdprm))  /* if help for subcmd desired		*/
	    {
	    if (s_lseq("-", selcmdprm))
		s_shift(selcmdprm, 1);	/* delete leading '-' from subcmd	*/
	    if (!tutctx.subtut)
		{
		if (NULLSTR((*pctx).subcmd) || !s_lseq(selcmdprm, (*pctx).subcmd))
		    goto entry_err;
		s_copy((*pctx).subcmd, selcmdprm);
		}
	    code = subab((*pctx).subptr, selcmdprm, &s);
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
	    code = lookab(&(*pctx).parmst, selcmdprm, &v);
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
    if (!NULLSTR(helpblk.nextcmd))
	{
	s_copy(helpblk.nextcmd, (*p_selctx).prev_cmd);
	}
    (*p_selctx).sdpctx.dispreq = TRUE;
    return(code);

open_err:
    selmsg("No help available for tutor mode.", "TAE-NOHELP", 0, 0, 0, 0, 0);
    return(FAIL);

proc_err:					/* no .help for the proc	*/
    selmsg("No help available for '%s'.", "TAE-NOHELP", 
	   (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);
    return(FAIL);

entry_err:
    selmsg("Invalid subcommand help in tutor mode.", "TAE-TINVSUBHLP", 0, 0, 0, 0, 0);
    return(FAIL);

subc_err:
    if (code == AMBIG)
	selmsg(msg_ambs, key_ambs, (uintptr_t) selcmdprm, 0, 0, 0, 0);	/* ambiguous subcommand abbreviation	*/
    else
	selmsg(msg_unrs, key_unrs, (uintptr_t) selcmdprm, 0, 0, 0, 0);	/* unrecognized subcommand		*/
    return(FAIL);

parm_err:
    if (code == AMBIG)
      selmsg(msg_ambi, key_ambi, (uintptr_t) selcmdprm, 0, 0, 0, 0);	/* ambiguous parameter abbreviation	*/
    else
	selmsg(msg_unre, key_unre, (uintptr_t) selcmdprm, 0, 0, 0, 0);	/* unrecognized parameter		*/
    return(FAIL);

over_err:
    selmsg(msg_mmov, key_mmov, 0, 0, 0, 0, 0);
    return(FAIL);

lev2_err:
    if (s_equal(type, "subcmd") && code == TUT_NOL2HELP)
	selmsg("No help available for subcommand '%s'.",
		"TAE-TNOLEV2", (uintptr_t) name, 0, 0, 0, 0);
    return(FAIL);

read_err:
    selmsg(msg_hrde, key_hrde, (uintptr_t) (*pctx).pdf.name, 0, 0, 0, 0);
    return(FAIL);

help_err:
    selmsg(helpblk.errmsg, helpblk.errkey, 0, 0, 0, 0, 0);	/* print error message		*/
    return(FAIL);
    }


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselaccept
*
* DESCRIPTION:
*	This function is one of the SELECT mode command functions.
*	This function accepts the highlighted value as the value
*	to set the parameter to.  It then returns to the tutor screen.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	Set parameter to current active value.
*	Terminate tutor select mode.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselaccept
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 )
{
    struct	SVRCTX	*p_svrctx = &(*p_selctx).svrctx;
    TEXT	prmset[CMDLINSIZ+1];
/*----------------------------------------------------------------------*/

/*  Terminate tutor select mode.					*/

    (*p_selctx).active = FALSE;

    if( (*p_svrctx).p_prmsetvalid == NULL )
        return( SUCCESS );

/*  Format parm="value" string to use for setting value.		*/
/*  Pass this back to tutor parameter mode as a previous mode command.	*/

    sprintf(prmset, "%s = \"%s\"", 
            (*p_svrctx).parmspec, (*p_svrctx).p_prmsetvalid -> string);

    s_copy( prmset, (*p_selctx).next_cmd );

    return( SUCCESS );
}


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	tselsearch
*
* DESCRIPTION:
*	This function is one of the SELECT mode command functions.
*	This function searchs for a VALID that contains the given
*	string as a substring.  Case is not significant.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	===Set reference point VALID===
*	IF active valid is NULL THEN
*	    Set reference to one less than first valid.
*	ELSE if active valid is last valid THEN
*	    Set reference valid to one less than first valid.
*	ELSE
*	    Set reference valid to active valid.
*	===Search from one past reference valid to last valid===
*	===Search from first valid to reference valid.===
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static CODE tselsearch
(struct	SELCTX	*p_selctx,
TEXT		*selcmdstr,
TEXT		*selcmdprm
 )
{
    struct	SVRCTX	*p_svrctx = &(*p_selctx).svrctx;
    struct	SDPCTX	*p_sdpctx = &(*p_selctx).sdpctx;

    struct	VALIDS	*p_valids = &(*p_sdpctx).valids;

    struct	S_RANGE	*p_ref, *p_test, *p_newtop;
    BOOL		match = FALSE;
/*----------------------------------------------------------------------*/
/*  Set reference point VALID.						*/
/*----------------------------------------------------------------------*/
/*  IF active valid is NULL THEN					*/
/*      Set reference to one less than first valid.			*/

    if( (*p_valids).upd.p_actvalid == NULL )
        p_ref = (*p_valids).p_firstvalid - 1;
    
/*  ELSE if active valid is last valid THEN				*/
/*      Set reference valid to one less than first valid.		*/

    else if( (*p_valids).upd.p_actvalid == (*p_valids).p_lastvalid )
        p_ref = (*p_valids).p_firstvalid - 1;

/*  ELSE								*/
/*      Set reference valid to active valid.				*/

    else
        p_ref = (*p_valids).upd.p_actvalid;

/*----------------------------------------------------------------------*/
/*  Search from one past reference valid to last valid.			*/
/*----------------------------------------------------------------------*/

    for( p_test = p_ref + 1; 
         p_test <= (*p_valids).p_lastvalid; 
         p_test++ )
    {
	if ( (*p_test).string[0] != EOS )
            match = s_lseq( selcmdprm, (*p_test).string);
        if( match ) 
		break;
    }

/*----------------------------------------------------------------------*/
/*  IF not yet found THEN Search from first valid to reference valid.	*/
/*----------------------------------------------------------------------*/

    if( !match )
    {
        for( p_test = (*p_valids).p_firstvalid; 
             p_test <= p_ref; 
             p_test++ )
        {
	    if ( (*p_test).string[0] != EOS )
                match = s_lseq( selcmdprm, (*p_test).string );
            if( match ) 
		break;
        }
    }

/*----------------------------------------------------------------------*/
/*  IF string found in a VALID string THEN set active to that VALID.	*/
/*  Also set new top valid so that new active is displayed.		*/
/*----------------------------------------------------------------------*/

    if( match )
    {
        (*p_valids).upd.p_actvalid = p_test;
        (*p_svrctx).p_prmsetvalid  = p_test;
        p_newtop = p_test - ((*p_valids).rect.dy - 2)/2;
        if( p_newtop > (*p_valids).p_lastvalid - ((*p_valids).rect.dy - 2)+1 )
            p_newtop = (*p_valids).p_lastvalid - ((*p_valids).rect.dy - 2)+1;
        if( p_newtop < (*p_valids).p_firstvalid )
            p_newtop = (*p_valids).p_firstvalid;
        (*p_valids).upd.p_topvalid = p_newtop;
    }
    else
    {
        selmsg( msg_tnotfound, key_tnotfound, (uintptr_t) selcmdprm, 0, 0, 0, 0 );
    }

    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	sdispupd
*
* DESCRIPTION:
*	This function updates the physical display.
*	All command execution functions request changes to the
*	display by making changes to the update copy of the
*	structures that define the display.  This function takes
*	care of actually changing the display.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	Compute derived values.
*	If display request flag for whole display set then
*	   erase screen
*	   set display request flags for all items in display.
*	   Reset whole display request flag.
*	Call update funtion for each item in display.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE sdispupd
(struct	SDPCTX	*p_sdpctx
 )
{
/*----------------------------------------------------------------------*/

/*  Compute derived items.						*/

    (*p_sdpctx).title.upd.pagnum = -1;

    (*p_sdpctx).title.upd.lastflag = FALSE;

/*  If display request flag set for whole display, then			*/
/*  set display request flags for each item in display.			*/

    if( (*p_sdpctx).dispreq )
    {
        t_clear();

        (*p_sdpctx).title.dispreq  = TRUE;
        (*p_sdpctx).help1.dispreq  = TRUE;
        (*p_sdpctx).valids.dispreq = TRUE;
        (*p_sdpctx).cmdreq.dispreq = TRUE;

        (*p_sdpctx).dispreq = FALSE;
    }

/*  Call function to update each item on screen.			*/

    sdispupd_title( &(*p_sdpctx).title );

    sdispupd_help1( &(*p_sdpctx).help1 );

    sdispupd_valids( &(*p_sdpctx).valids );

    sdispupd_cmdreq( &(*p_sdpctx).cmdreq );

    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	sdispupd_title
*
* DESCRIPTION:
*	This function updates the title item on the physical display.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF the display request flag is set OR
*	update request page is different from current page OR
*	lastpage flag is different from current flag THEN
*	    Plot title line.
*	    Set current plot descriptor structure to indicate update.
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE sdispupd_title
(struct	TITLE	*p_title
 )
{
    TEXT	lefthead[STRINGSIZ+1];
/*----------------------------------------------------------------------*/

/*  IF the display request flag is set OR				*/
/*  update request page is different from current page OR		*/
/*  update request lastpage flag is different from current flag THEN	*/

    if( (*p_title).dispreq
    ||  (*p_title).upd.pagnum   != (*p_title).cur.pagnum
    ||  (*p_title).upd.lastflag != (*p_title).cur.lastflag )
    {
/*      Plot title line.						*/

        left_pbld( (*p_title).pdfname,
                   (*p_title).subcmd,
                   (*p_title).parmspec,
                   (*p_title).intrinsic ? "command" : "proc",
                   lefthead );

        dsphdr( "Tutor/Select", 
                lefthead, 
                (*p_title).upd.pagnum, 
                (*p_title).upd.lastflag );

/*      Set current plot descriptor structure to indicate update.	*/

        (*p_title).cur= (*p_title).upd;

        (*p_title).dispreq = FALSE;
    }
    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	sdispupd_help1
*
* DESCRIPTION:
*	This function updates the help item on the physical display.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF display request flag set THEN
*	    Plot level 1 help.
*	    Clear display request flag.
* NOTES:
*
*   1)	The help1 display item has no variable parts.
*
* SYNOPSIS:
--*/
FUNCTION CODE sdispupd_help1
(struct	HELP1	*p_help1
 )
{
static	TEXT	nodescr_notice[] = "Description not available";
/*----------------------------------------------------------------------*/

/*  If display request flag is set, THEN plot level 1 help.		*/

    if( (*p_help1).dispreq )
    {
        if( (*p_help1).l1hexist )
            disptxt( (*p_help1).rect.y,
                     (*p_help1).rect.x,
                     &(*p_help1).l1help );
        else
            t_output( (*p_help1).rect.y,
                      (*p_help1).rect.x,
                      nodescr_notice );

        (*p_help1).dispreq = FALSE;
    }
    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	sdispupd_valids
*
* DESCRIPTION:
*	This function updates the VALIDs item on the physical display.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF display request flag is set THEN
*	    Replot entire VALID display region.
*	ELSE IF topvalid has changed THEN
*	    IF lines to scroll > number of display lines THEN
*	        Replot VALID display region.
*	    ELSE IF lines to scroll < 0 THEN
*	        Do backwards scroll:
*	        Set scroll region.
*               Position cursor to top line
*               LOOP nscroll times
*                   Reverse scroll a line.
*	            Add a line.
*	    ELSE IF lines to scroll >0 THEN
*	        Do forward scroll (similar to reverse scroll).
*	ELSE IF active item has changed THEN
*	    IF current active item on display THEN
*	        Turn off highlighting for current active item.
*	    IF update request item on display THEN
*	        Turn on highlighting for it.
*           Update request item becomes current active item.
*	
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE sdispupd_valids
(struct	VALIDS	*p_valids
 )
{
    struct	S_RANGE	*p_outvalid;
    COUNT	line;
    TEXT	valstr[STRINGSIZ+1];
    TEXT	ctlstr[STRINGSIZ+1];
    CODE	code;

    COUNT	topline      = (*p_valids).rect.y;
    COUNT	nlines       = (*p_valids).rect.dy;
    COUNT	topdataline  = topline + 2;
    COUNT	ndatalines   = nlines - 2;
    COUNT	botdataline  = topdataline + ndatalines - 1;
    COUNT	nscroll      = 
                (*p_valids).upd.p_topvalid - (*p_valids).cur.p_topvalid;

    COUNT	leftcolumn   = (*p_valids).rect.x;
/*----------------------------------------------------------------------*/
/*  IF display request flag is set THEN					*/
/*      Replot entire VALID display region.				*/
/*----------------------------------------------------------------------*/
    if( (*p_valids).dispreq )
    {
        t_lclear( topline, leftcolumn );
        t_output( topline, leftcolumn, "Selection List" );

        t_lclear( topline + 1, leftcolumn );
        t_output( topline + 1, leftcolumn, "------------------------" );

        for( line = 0; line < ndatalines; line++ )
        {
            t_lclear( topdataline + line, leftcolumn );

            p_outvalid = (*p_valids).upd.p_topvalid + line;

            if( p_outvalid >= (*p_valids).p_firstvalid
             && p_outvalid <= (*p_valids).p_lastvalid )
		{
		if (s_length((*p_outvalid).string ) > 79 )
		    {
		    TEXT tempstring[STRINGSIZ+1];

		    s_bcopy ( (*p_outvalid).string, tempstring, termcols-4 );
		    tempstring[termcols-4]=EOS;
		    s_append ("...", tempstring );
                    t_output( topdataline + line, leftcolumn, tempstring);
		    }
		else
                    t_output( topdataline + line, leftcolumn, 
				(*p_outvalid).string);
		}
        }

/*      Set current display descriptors to indicate current display.	*/

        (*p_valids).cur = (*p_valids).upd;
        (*p_valids).cur.p_actvalid = NULL;
        (*p_valids).dispreq = FALSE;

        code = sdispupd_valids( p_valids );
    }

/*----------------------------------------------------------------------*/
/*  ELSE IF topvalid has changed THEN					*/
/*----------------------------------------------------------------------*/

    if( (*p_valids).upd.p_topvalid != (*p_valids).cur.p_topvalid )
    {
/*      IF lines to scroll > number of display lines THEN		*/
/*          Replot VALID display region.				*/

        if( abs( nscroll ) > ndatalines )
        {
            (*p_valids).dispreq = TRUE;
            code = sdispupd_valids( p_valids );
        }

/*	ELSE IF lines to scroll < 0 THEN				*/
/*	    Do backwards scroll						*/

        else if( nscroll < 0 )
        {
            sprintf( ctlstr, "\033[%d;%dr", topdataline, botdataline );
            t_write( ctlstr, T_NULL );

            sprintf( ctlstr, "\033[%d;1H", topdataline );
            t_write( ctlstr, T_NULL );

            for( p_outvalid = (*p_valids).cur.p_topvalid - 1;
                 p_outvalid >= (*p_valids).upd.p_topvalid;
                 p_outvalid-- )
            {
                if( p_outvalid < (*p_valids).p_firstvalid )
                    sprintf( valstr, "\033M\033[%d;%dH", 
                             topdataline, leftcolumn );
                else
                    sprintf( valstr, "\033M\033[%d;%dH%s", 
                             topdataline, leftcolumn, (char *) p_outvalid );
                t_write( valstr, T_NULL );
            }
        t_write( "\033[r", T_NULL );
        (*p_valids).cur.p_topvalid = (*p_valids).upd.p_topvalid;
        code = sdispupd_valids( p_valids );
        }

/*	ELSE IF lines to scroll >0 THEN					*/
/*	    Do forward scroll						*/

        else if( nscroll > 0 )
        {
            sprintf( ctlstr, "\033[%d;%dr", topdataline, botdataline );
            t_write( ctlstr, T_NULL );

            sprintf( ctlstr, "\033[%d;1H", botdataline );
            t_write( ctlstr, T_NULL );

            for( p_outvalid = (*p_valids).cur.p_topvalid + ndatalines;
                 p_outvalid <= (*p_valids).upd.p_topvalid + ndatalines - 1;
                 p_outvalid++ )
            {     
                if( p_outvalid > (*p_valids).p_lastvalid )
                    sprintf( valstr, "\033D\033[%d;%dH", 
                             botdataline, leftcolumn );
                else
                    sprintf( valstr, "\033D\033[%d;%dH%s", 
                             botdataline, leftcolumn, (*p_outvalid).string);
                t_write( valstr, T_NULL );
            }
        t_write( "\033[r", T_NULL );
        (*p_valids).cur.p_topvalid = (*p_valids).upd.p_topvalid;
        code = sdispupd_valids( p_valids );
        }

        (*p_valids).cur = (*p_valids).upd;
    }

/*----------------------------------------------------------------------*/
/*  ELSE IF active item has changed THEN				*/
/*----------------------------------------------------------------------*/
    else if( (*p_valids).upd.p_actvalid != (*p_valids).cur.p_actvalid )
    {

/*      IF current active item on display THEN				*/
/*          Turn off highlighting for current active item.		*/

        if( (*p_valids).cur.p_actvalid >= (*p_valids).cur.p_topvalid
         && (*p_valids).cur.p_actvalid <= 
            (*p_valids).cur.p_topvalid + (*p_valids).rect.dy - 2 - 1 )
        {
            line = (*p_valids).cur.p_actvalid - (*p_valids).cur.p_topvalid;
            if (s_length((*p_valids).cur.p_actvalid -> string ) > termcols )
                    {
                    TEXT tempstring[STRINGSIZ+1];
                    s_bcopy ( (*p_valids).cur.p_actvalid -> string, tempstring, termcols-4 );
		    tempstring[termcols-4]=EOS;
		    s_append ( "...",tempstring);
                    t_output( topdataline + line, leftcolumn, tempstring);
                    }
                else
            	    t_output( topdataline + line, leftcolumn, 
                      (*p_valids).cur.p_actvalid -> string );
        }

/*      IF update request active item on display THEN			*/
/*          Turn on highlighting for it.				*/

        if( (*p_valids).upd.p_actvalid >= (*p_valids).upd.p_topvalid
         && (*p_valids).upd.p_actvalid <= 
            (*p_valids).upd.p_topvalid + (*p_valids).rect.dy - 2 - 1 )
        {
	    if (s_length( (*p_valids).upd.p_actvalid -> string ) > termcols )
		{
		s_bcopy ( (*p_valids).upd.p_actvalid -> string, valstr, termcols-4 );
                valstr[termcols-4]=EOS;
		s_append ("...",valstr);
		}
	    else
                s_copy( (*p_valids).upd.p_actvalid -> string, valstr );

            t_highlight( valstr );
            line = (*p_valids).upd.p_actvalid - (*p_valids).upd.p_topvalid;
            t_output( topdataline + line, leftcolumn, valstr );
        }

/*      Update request item becomes current active item.		*/

        (*p_valids).cur = (*p_valids).upd;
    }

    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	sdispupd_cmdreq
*
* DESCRIPTION:
*	This function updates the command request line item on 
*	the physical display.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	IF the display request flag is set THEN
*	    Plot command request line.
*	    Clear display request flag.
*	
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE sdispupd_cmdreq
(struct	CMDREQ	*p_cmdreq
 )
{
/*----------------------------------------------------------------------*/

/*  If display request flag is set, THEN plot command request line.	*/

    if( (*p_cmdreq).dispreq )
    {
        t_output( (*p_cmdreq).rect.y,
                  (*p_cmdreq).rect.x,
                  (*p_cmdreq).cmdreqtext );

        (*p_cmdreq).dispreq = FALSE;
    }

    return( SUCCESS );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	selmsg
*
* DESCRIPTION:
*	This function displays an error message.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION VOID selmsg 
(
TEXT		msg[],
TEXT		key[],
uintptr_t	a1,
uintptr_t	a2,
uintptr_t	a3,
uintptr_t	a4,
uintptr_t	a5
 )
{
    wrterr(msg, key, a1, a2, a3, a4, a5);
    return;
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	srchseltab
*
* DESCRIPTION:
*	This function searches a command table for the command specified
*	by the string.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*	Get command verb from command string.
*	Search command table for command verb.
*	Return pointer to command record.
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION static struct SELCMD *srchseltab 
(TEXT		cmdstr[]	/* in:  command string			*/
 )
{
    struct SELCMD	*p_selcmd;
    struct SYNBLK	sb;
    CODE		code;
    TEXT		verb[TOKESIZ+1];
    COUNT		i;
/*----------------------------------------------------------------------*/
/*  Initialize command parsing.					*/

    initok(&sb, cmdstr);

/*  Extract command verb from command string.				*/

    if (NULLSTR(cmdstr))
	verb[0] = EOS;
    else
    {
	code = getvrb(&sb, verb);

	if (!s_equal(verb, "?") && code == S_SYNERR) 	/* "?" is a valid cmd	*/
	    return(NULL);
    }

/*  Search command table for entry corresponding to command verb.	*/

    for( i = 0, p_selcmd = scmd; i < NSCMD; i++, p_selcmd++ )
    {
	if( s_equal(verb, (*p_selcmd).cmdname) )
            break;

        if( (*p_selcmd).abbchr > 0
         && s_length( verb ) >= (*p_selcmd).abbchr
         && s_lseq( verb, (*p_selcmd).cmdname ) )
            break;
    }

    return( i == NSCMD ? NULL : p_selcmd );
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gen_cmd_screen
*
* DESCRIPTION:
*	This function reads the next command.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	This is a general purpose version of cmd_screen.  It is derived
*	from cmd_screen.
*
* SYNOPSIS:
--*/
FUNCTION CODE gen_cmd_screen
(TEXT		inicmdstr[],	/* In:  Initial command string.		*/
TEXT		cmdstr[],	/* Out: Command string.			*/
CODE		*terminat	/* Out: Command terminator.		*/
 )
{
    IMPORT struct  SFILE	*scr_file;	/* SFILE for the script file 	*/
    IMPORT struct  ECB		ecbi;		/* operator-attn ecb		*/
    IMPORT COUNT		termlines;	/* number of lines on terminal */
    TEXT		inbuf[STRINGSIZ+1];	/* single line input buffer	*/
    TEXT		inistr[STRINGSIZ+1];	/* Initial command string. */
    TEXT		prompt[STRINGSIZ+1];
    COUNT		pmtline;		/* display line for "?"	*/
    COUNT		col;
    COUNT		nrec;
    CODE		code;
    CODE		cmdcode;
/*----------------------------------------------------------------------*/

    pmtline = termlines - PMTLIN;
    cmdstr[0] = EOS;
    cmdcode = SUCCESS;

    do
    {
        col = (cmdcode == CONTINUE) ?
            inistr[0] = EOS : s_copy( inicmdstr, inistr );
	col = (cmdcode == CONTINUE) ? 
	    s_copy("?+ ", prompt ) : s_copy("? ", prompt );
        code = gen_line_screen( pmtline, prompt, inistr, inbuf, terminat );
	t_attn(&ecbi);				/* clear operator-attn ecb */
	if (code != SUCCESS  ||  *terminat == T_ESCAPE)
	    break;
	cmdcode = bldcmd(inbuf, cmdstr);	/* app to full cmd str (& strip comments)*/
	if (code == FAIL && scr_file != NULL)	/* if from script	*/
	    flush_cmd(scr_file, inbuf, &nrec);	 /* ignore rest of the cmd	*/
    } while (cmdcode == CONTINUE);

    t_lclear(termlines-ERRLIN, 1);

    if (code == F_FILERR)
	wrterr("Error reading script file.", "TAE-RDERR", 0, 0, 0, 0, 0);
    if (*terminat == T_ESCAPE)
	cmdstr[0] = EOS;		/* ignore the input command */
    return(code);
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gen_line_screen
*
* DESCRIPTION:
*	This function is a general purpose input line read function.
*	It is similar to line_screen.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION  CODE  gen_line_screen
(
COUNT	line_num,		/* IN: input line number	     */
TEXT 	prompt[],		/* IN: prompt string		     */
TEXT	inistr[],		/* IN: Initial input string.	     */
TEXT  	line[],			/* OUT: line from interactive source */
CODE	*term			/* OUT: line terminator 	     */
 )
{
    IMPORT struct SFILE		*scr_file;	/* pointer to script file ctx*/
    IMPORT COUNT termcols;			/* terminal columns	     */
    IMPORT COUNT termlines;			/* number of lines on terminal */
    CODE	code;	
    TEXT	loc_prompt[STRINGSIZ+1];

/*----------------------------------------------------------------------*/
    if (scr_file != NULL)
	{
        s_copy (prompt, loc_prompt);
        t_highlight (loc_prompt);
	while (FOREVER)
	    {
	    t_lclear(termlines - PMTLIN, 1);		/* clear prompt line */
            t_output((termlines - PMTLIN), 1, loc_prompt);  /* write prompt  */
	    code = getscr(line, term, TRUE);		/* get from script   */
	    if (code == F_EOF) 			/* on EOF go back to opera'r */
		break;
	    if (code != F_KEEP)			/* KEEP => delay or pause    */	
	        return(code);
	    }
	}

    if (scr_file == NULL )
    {

	t_lclear(termlines - PMTLIN, 1);		/* clear prompt line */

	code = gen_cmd_editor( line_num, prompt, inistr, line );

	*term = (code == SUCCESS) ? T_CR : T_ESCAPE;
	code = SUCCESS;				/* terminal: always success */
    }
    return(code);
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gen_cmd_editor
*
* DESCRIPTION:
*	This function is a general purpose command line editor.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE gen_cmd_editor
(
COUNT	line_num,	/* in: line number on screen		*/
			/*     (-1 if line number not known)	*/
TEXT 	prompt[],	/* in: prompt string			*/
TEXT	inistr[],	/* In: Initial input string.		*/
TEXT	line[]		/* out: user-typed line			*/
 )
{
/*----------------------------------------------------------------------*/
    return( gen_line_editor( line_num, prompt, inistr, line ));
}

/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	gen_line_editor
*
* DESCRIPTION:
*	This function is a general purpose command line editor.
*
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION CODE gen_line_editor
(
COUNT	line_num,	/* in: line number on screen		*/
			/*     (-1 if line number not known)	*/
TEXT	inistr[],	/* In: Initial input string.		*/
TEXT 	prompt[],	/* in: prompt string			*/
TEXT	line[]		/* out: user-typed line			*/
 )
{
    IMPORT	struct	VARIABLE  *last_gbl;
    IMPORT COUNT 	termlines, termcols;
    IMPORT CODE 	termtype;

    CODE		terminator;
    COUNT		index, lm, c, maxl, i, j, l;
    COUNT		count, column;
    TEXT		tmpline[STRINGSIZ+1];
    FAST BOOL		gold_active, gold;
    FAST CODE		keytoke;	
int	screen = TRUE;
/*----------------------------------------------------------------------*/
#ifdef VAX_VMS
    IMPORT BOOL tt_editing;
    if (tt_editing)		/* is "SET TERM/LINE_EDITING" active	*/
	goto edit_with_vms;
#endif
    lm = s_copy (prompt, tmpline) + 1;	/* copy prompt, set left margin */
    t_highlight (tmpline);		/* highlight prompt		*/
    if (screen)
	{
	t_output (line_num, 1, tmpline);	/* prompt for screen	*/
	t_input (line_num, lm, line, &terminator);
	}
    else
	{
        t_write (tmpline, T_PROMPT);		/* scrolling prompt	*/
        t_read (line, &terminator);		/* read line 		*/
	}
    if (terminator == T_CR)
	return (SUCCESS);	/* line read efficiently with no editing*/
    if (termtype != T_CRT)
        return (T_2ESC);	/* not a CRT, so kill the command	*/
    gold_active = FALSE;
    count = (*last_gbl).v_count;
    maxl = termcols - lm ;	/* maximum chars that can be read	*/
    line [maxl] = EOS;		/* clip line if too long		*/
    index = -1;			/* current $LASTCMD index		*/
    l = s_length(line);		/* current number of chars in line	*/
    keytoke = t_1gettok();	/* get first token (escape already came)*/
    t_getpos (&line_num, &column);	/* get line nr (unless UNIX)	*/
    if (line_num < 0)			/* if line nr still not known	*/
	{
	line_num = termlines;		/* re-write to last line	*/
	i = s_copy (prompt, tmpline);	/* (and do the editing there)	*/
	i += t_highlight (tmpline);	/* add highlighting		*/
	s_copy (line, &tmpline[i]); 	/* and the line typed so far	*/
	t_output (line_num, 1, tmpline);
	}
    i = l;				/* current index in line buffer	*/
    for (; keytoke != TERM_RETCHAR; keytoke=t_gettok())
        {
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
        if (keytoke == '\177')		/* DELETE key		*/
	    {
	    if (i == 0)			/* already at left?	*/
		{
		/* t_bell(); */
		continue;
		}
	    if (gold)
		c = i;				/* number of chars to delet */
	    else
		c = 1;		
	    for (j=i-c; j+c <= l; j++)		/* delete c chars to left */
		line[j] = line[j+c];		
	    s_copy (line, tmpline);		/* temp copy of line	*/
	    for (j=l-c; j < l; j++)		/* add spaces for...	*/
		tmpline[j] = ' ';		/* each char deleted	*/
	    tmpline[l] = EOS;
	    l = l - c;				/* new line length	*/
	    i = i - c;				/* new current positon	*/
	    t_output (line_num, lm+i, &tmpline[i]);	/* re-paint line*/
	    t_pos (line_num, lm + i);		/* position to current	*/
    	    }
	else if (keytoke == '\025' || keytoke == '\030')
	    {					/* CONTROL/U or		*/
	    i = 0;				/* CONTROL/X		*/
	    l = 0;
	    line[0] = EOS;
	    t_lclear (line_num, lm);	
	    }
	else if (keytoke < T_SPECIAL)		/* normal key?		*/
	    {
	    if (keytoke < ' ' && keytoke != '\t')
		continue;			/* ignore control chars */
    	    if (keytoke == '\t')
    		keytoke = ' ';			/* tab: crude approach  */
	    if (l >= maxl)			
	        {
	        t_bell(); 			/* too many chars 	*/
	    	continue;
	    	}
	    for (j=l; j > i; j--)		/* move chars over 	*/
	        line[j] = line[j-1];
	    line[i] = keytoke;			/* insert new char	*/
	    line[++l] = EOS;			/* new length and EOS	*/
	    t_write (&line[i], T_NULL);		/* re-paint right side	*/
	    i++;				/* move to next char	*/
	    t_pos (line_num, lm + i);		/* position to next	*/
	    }	
	else if (keytoke == T_LEFT)
	    {
	    if (i == 0)
	        ; /* t_bell(); */		/* too far left		*/
	    else if (gold)			/* GOLD/LEFT		*/
	        {
		t_pos (line_num, lm);		/* position to first	*/
		i = 0;
		}
	    else				/* normal LEFT		*/
		{
		i--;				/* move left one char	*/
		t_pos(line_num, lm + i);
		}
	    }
        else if (keytoke == T_RIGHT)
	    {
	    if (i >= l)
	        ; /* t_bell(); */		/* too far right	*/
	    else if (gold)			/* GOLD/RIGHT		*/
	        {
		i = l;
		t_pos (line_num, lm + i);
		}
	    else				/* RIGHT		*/
		{
		i++;
		t_pos (line_num, lm + i);
		}
	    }
	else if (keytoke == T_UP)
	    {
	    if (gold)				/* GOLD/UP		*/
                {
                s_copy( "DECACT PAGE", line );
                }
	    else
                {
                s_copy( "DECACT", line );
                }
            return( SUCCESS );
	    }
	else if (keytoke == T_DOWN)
	    {
	    if (gold)				/* GOLD/UP		*/
                {
                s_copy( "INCACT PAGE", line );
                }
	    else
                {
                s_copy( "INCACT", line );
                }
            return( SUCCESS );
	    }
	else if (keytoke == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (keytoke == T_2ESC)		/* double escape?	*/
	    {
	    line[0]=0;				/* formality		*/
	    return (FAIL);			/* indicate 'kill cmd'	*/
	    }
        else
	    ; /* t_bell(); */			/* unknown token	*/
        }
    tmpline[0] = TERM_RETCHAR;
    tmpline[1] = '\0';
    t_write (tmpline, T_NULL);			/* echo CR		*/
    return (SUCCESS);

/*	This code make use of the VMS line editor and is executed
 *	when "SET TERM/LINE_EDIT" is active.
 */
#ifdef VAX_VMS
edit_with_vms:					/* cmd_editor:		*/
    line[0] = EOS;				/* start edit with null	*/
    s_copy( inistr, line );
    lm = s_length (prompt) + 1;			/* left margin		*/
    if ((code = t_edit(prompt, line, screen)) == T_CR)
	return (SUCCESS);
    t_getpos (&line_num, &column);		/* get line nr 		*/
    maxl = termcols - lm ;			/* max chars to read	*/
    gold_active = FALSE;
    count = (*last_gbl).v_count;		/* nr of $LASTCMDs	*/
    index = -1;					/* $LASTCMD index	*/
    do
        {
        t_lclear (line_num, 1);		/* clear line if CRT		*/
	line[0] = EOS;			/* start new line		*/
	gold = gold_active;		/* copy for this iteration	*/
	gold_active = FALSE;		/* so we can clear this once	*/
	if (code == T_2ESC)
	    return (FAIL);
	else if (code == T_GOLD)		/* GOLD			*/
	    gold_active = TRUE;
	else if (code == T_UP)
	    {
	    if (gold)				/* GOLD/UP		*/
                {
                s_copy( "DECACT PAGE", line );
                }
	    else
                {
                s_copy( "DECACT", line );
                }
            return( SUCCESS );
	    }
	else if (code == T_DOWN)
	    {
	    if (gold)				/* GOLD/UP		*/
                {
                s_copy( "INCACT PAGE", line );
                }
	    else
                {
                s_copy( "INCACT", line );
                }
            return( SUCCESS );
	    }
        else if( code == T_F2 )
            {
            }
        else if( code == T_F3 )
            {
            }

	}
        while ((code = t_edit1 (prompt, line, screen)) != T_CR);
    return(SUCCESS);

#endif
    }


/*++
*
* PROJECT:	TAE
*
* FACILITY:	TM
*
* FILE:		TUTSELECT.C
*
* FUNCTION:	findvalid
*
* DESCRIPTION:
*	Given a string, this function returns a pointer to an item
*	in a VALID list that matches the string.
* USE:
*
* PARAMETERS:
*
* RETURN VALUE:
*
* CONTROL:
*
* NOTES:
*
*   1)	
*
* SYNOPSIS:
--*/
FUNCTION struct	S_RANGE	*findvalid
(TEXT		*string,
struct	S_RANGE	*p_valids,
COUNT		nvalids
 )
{
    COUNT	i;
/*----------------------------------------------------------------------*/

    for( i = 0; i < nvalids; i++, p_valids++)
        if( s_equal( string, (*p_valids).string) )
            return( p_valids );

    return( NULL );
}

