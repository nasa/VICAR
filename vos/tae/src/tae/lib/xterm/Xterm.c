/*
 * Change Log:
 *	xx-xxx-91 Initial...Margi Klemp and Eric Hillis
 *	27-feb-92 Changed path to TAE Plus header files...kbs
 *	28-feb-92 Changed cursor from XC_xterm to XC_left_ptr
 *	          to enable easier resize in WorkBench...kbs
 *	          -- ABOVE CHANGE DOES NOT HAVE ANY EFFECT...kbs
 *	03-mar-92 Applied Eric's change for Wpt_SetNoValue: XtermReset...kbs
 *	19-jun-92 Added code in VtDestroy to remove the GCs and Fonts...krw
 *	04-feb-93 Fixed Multi-Free bug for logfile...ewh
 *	22-feb-93 Added XtermGetLoggerSelectedLineNum, XtermGetLoggerLine,
 *		  XtermSetLoggerLine for v5.1a VMS...ewh
 */

/*  This file is simply a partial extraction from the 'charproc.c' file of the
    original XtermWidget software, but since I didn't like the name
    'charproc.c', I decided to name it Xterm.c since this seems to fit the
    X Toolkit Widget file naming conventions.  I will admit, this stuff is
    pretty bad, and maybe the naming convention is the only real convention
    that is followed in this whole implementation, but whatever...

    6/6/91 Convert(ing) to Motif...

   The following routines have been modified from 'charproc.c'
*    VTParse() ELIMINATED CODE these are OUTPUT only widgets for OASIS.
*    in_put() ELIMINATED CODE ...
*    unparseXXX() ELIMINATED.....
     VTExpose() declared 'term' as par. w.
     VTGraphicsOrNoExpose(...[term])
     VTResize() declared 'term' as par. w.
     VTInit([term])
     VTallocbuf([term])
*    VTInitialize() 1) register window callbacks with 'new' as calldata.
		    2) don't create scrollbar unless specified.
		    3) initialize 'parent' and 'pOldColors' fields.
     VTDestroy() deletes color stuff.
     VTRealize() declared 'term' ...
     ShowCursor([screen])
     HideCursor([screen])
*    VTReset(...[term]) eliminated call to 'longjmp' DANGEROUS.
     LoadNewFont(...[-fontnum],[+term])
     malloc() to XtMalloc()
   routines called:
     HideCursor([screen])
     VTallocbuf([term])
     VTGraphicsOrNoExpose(...[(XtermWidget)closure])
     recolor_cursor([term]...)
     xevents([term])
     Bell([term])
     Redraw([term])
*/

#define ALLOWLOGFILEONOFF	/* added this one */

/*
 * $XConsortium: charproc.c,v 1.123 90/03/12 10:30:21 jim Exp $
 */


#ifndef VMS
#include <X11/copyright.h>
#endif
#include <X11/Xlib.h>

/*
 * Copyright 1988 Massachusetts Institute of Technology
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *			   All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */


/* charproc.c */

#ifdef att
#ifndef STREAMSCONN
#define STREAMSCONN
#endif
#endif

#include <stdio.h>
#include <X11/Xos.h>
#if !defined(CRAY) && !defined(VMS)
#include <sgtty.h>
#endif
#include <ctype.h>
#include <errno.h>
#if defined(macII) || defined(CRAY)
#undef FIOCLEX					/* redefined from sgtty.h */
#undef FIONCLEX					/* redefined from sgtty.h */
#endif
#include "ptyx.h"
#include "VTparse.h"
#include <X11/Xutil.h>
#include "error.h"
#include "main.h"
#include <X11/cursorfont.h>
#ifdef __hpux
#include <X11/Xlibint.h>
#endif
#include <Xm/Xm.h>

#include <wptxmdef.h>				/* TAE specific */

#if !defined(EWOULDBLOCK) && defined(EAGAIN)
#define EWOULDBLOCK EAGAIN
#endif

#ifndef __hpux
extern void exit(), bcopy();
#endif
static void VTallocbuf();

#define INSIDEXTERM 1
#include "XtermP.h"

#define	doinput(bptr,len)	((len)-- > 0 ? *(bptr)++ : -1)

#ifndef lint
static char rcs_id[] = "$XConsortium: Xterm.c,v 1.123 91/06/06 13:30:21 ewh Exp $";
#endif	/* lint */

static unsigned long ctotal;
static unsigned long ntotal;

/* Following defined in VTPrsTbl.c */
#ifndef VMS
extern int groundtable[];
extern int csitable[];
extern int dectable[];
extern int eigtable[];
extern int esctable[];
extern int iestable[];
extern int igntable[];
extern int scrtable[];
extern int scstable[];
#else
#include "VTPrsTbl.c"
#endif

/* event handlers */
extern void HandleKeyPressed(), HandleEightBitKeyPressed();
extern void HandleStringEvent();
extern void HandleEnterWindow();
extern void HandleLeaveWindow();
extern void HandleFocusChange();
static void HandleKeymapChange();
extern void HandleInsertSelection();
extern void HandleSelectStart(), HandleKeyboardSelectStart();
extern void HandleSelectExtend(), HandleSelectSet();
extern void HandleSelectEnd(), HandleKeyboardSelectEnd();
extern void HandleStartExtend(), HandleKeyboardStartExtend();
static void HandleBell();
static void HandleIgnore();
extern void HandleScrollForward();
extern void HandleScrollBack();

/*
 * NOTE: VTInitialize zeros out the entire ".screen" component of the
 * XtermWidget, so make sure to add an assignment statement in VTInitialize()
 * for each new ".screen" field added to this resource list.
 */

/* Defaults */
static	Boolean	defaultFALSE	   = FALSE;
static	Boolean	defaultTRUE	   = TRUE;
static	int	defaultIntBorder   = DEFBORDER;
static	int	defaultSaveLines   = SAVELINES;
static	int	defaultScrollLines = SCROLLLINES;
static	int	defaultNMarginBell = N_MARGINBELL;
static	int	defaultMultiClickTime = MULTICLICKTIME;

/*
 * Warning, the following must be kept under 1024 bytes or else some
 * compilers (particularly AT&T 6386 SVR3.2) will barf).  Workaround is to
 * declare a static buffer and copy in at run time (the Athena text widget
 * does).  Yuck.
 */
static char defaultTranslations[] =
"\
  Shift <KeyPress> Prior:	scroll-back(1,halfpage) \n\
  Shift <KeyPress> Next:	scroll-forw(1,halfpage) \n\
  Shift <KeyPress> Select:	select-cursor-start() select-cursor-end(PRIMARY, CUT_BUFFER0) \n\
  Shift <KeyPress> Insert:	insert-selection(PRIMARY, CUT_BUFFER0) \n\
       ~Meta<KeyPress>:		insert-seven-bit()	\n\
	Meta<KeyPress>:		insert-eight-bit()	\n\
      ~Meta <Btn1Down>:		select-start()	\n\
      ~Meta <Btn1Motion>:	select-extend() \n\
~Ctrl ~Meta <Btn2Down>:		ignore()	\n\
~Ctrl ~Meta <Btn2Up>:		insert-selection(PRIMARY, CUT_BUFFER0) \n\
~Ctrl ~Meta <Btn3Down>:		start-extend()	\n\
      ~Meta <Btn3Motion>:	select-extend()	\n\
~Ctrl ~Meta <BtnUp>:		select-end(PRIMARY, CUT_BUFFER0) \n\
	    <BtnDown>:		bell(0)		\
";

static XtActionsRec actionsList[] = {
    { "bell",		  HandleBell },
    { "ignore",		  HandleIgnore },
    { "insert",		  HandleKeyPressed },  /* alias for insert-seven-bit */
    { "insert-seven-bit", HandleKeyPressed },
    { "insert-eight-bit", HandleEightBitKeyPressed },
    { "insert-selection", HandleInsertSelection },
    { "keymap",		  HandleKeymapChange },
    { "select-start",	  HandleSelectStart },
    { "select-extend",	  HandleSelectExtend },
    { "select-end",	  HandleSelectEnd },
    { "select-set",	  HandleSelectSet },
    { "select-cursor-start",	  HandleKeyboardSelectStart },
    { "select-cursor-end",	  HandleKeyboardSelectEnd },
    { "start-extend",	  HandleStartExtend },
    { "start-cursor-extend",	  HandleKeyboardStartExtend },
    { "scroll-forw",	  HandleScrollForward },
    { "scroll-back",	  HandleScrollBack },

#ifdef JUNK
    /* added accelerator...??? actions */
    { "set-visual-bell",	HandleVisualBell },
    { "set-logging",		HandleLogging },
    { "redraw",			HandleRedraw },

    { "set-scrollbar",		HandleScrollbar },
    { "set-jumpscroll",		HandleJumpscroll },
    { "set-bltscroll",		HandleBltscroll },
    { "set-reverse-video",	HandleReverseVideo },
    { "set-autowrap",		HandleAutoWrap },
    { "set-reversewrap",	HandleReverseWrap },
    { "set-autolinefeed",	HandleAutoLineFeed },
    { "set-appcursor",		HandleAppCursor },

    { "set-scroll-on-key",	HandleScrollKey },
    { "set-scroll-on-tty-output",	HandleScrollTtyOutput },
    { "set-allow132",		HandleAllow132 },
    { "set-cursesemul",		HandleCursesEmul },
    { "set-marginbell",		HandleMarginBell },
    { "set-altscreen",		HandleAltScreen },
    { "soft-reset",		HandleSoftReset },
    { "hard-reset",		HandleHardReset },
    { "set-terminal-type",	HandleSetTerminalType },
    { "set-visibility",		HandleVisibility },
#endif JUNK
};

static XtResource resources[] = {
{XmNfont, XmCFont, XmRString, sizeof(char *),
	XtOffset(XtermWidget, misc.f_n), XmRString,
	DEFFONT},
{XmNboldFont, XmCFont, XmRString, sizeof(char *),
	XtOffset(XtermWidget, misc.f_b), XmRString,
	DEFBOLDFONT},
{XmNc132, XmCC132, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.c132),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNcharClass, XmCCharClass, XmRString, sizeof(char *),
	XtOffset(XtermWidget, screen.charClass),
	XmRString, (caddr_t) NULL},
{XmNcurses, XmCCurses, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.curses),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNcutNewline, XmCCutNewline, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.cutNewline),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNcutToBeginningOfLine, XmCCutToBeginningOfLine, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.cutToBeginningOfLine),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNbackground, XmCBackground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, core.background_pixel),
	XmRString, "XtDefaultBackground"},
{XmNcursorColor, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.cursorcolor),
	XmRString, "XtDefaultForeground"},
{XmNcolor0, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_0]),
	XmRString, "XtDefaultForeground"},
{XmNcolor1, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_1]),
	XmRString, "XtDefaultForeground"},
{XmNcolor2, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_2]),
	XmRString, "XtDefaultForeground"},
{XmNcolor3, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_3]),
	XmRString, "XtDefaultForeground"},
{XmNcolor4, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_4]),
	XmRString, "XtDefaultForeground"},
{XmNcolor5, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_5]),
	XmRString, "XtDefaultForeground"},
{XmNcolor6, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_6]),
	XmRString, "XtDefaultForeground"},
{XmNcolor7, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.colors[COLOR_7]),
	XmRString, "XtDefaultForeground"},
{XmNeightBitInput, XmCEightBitInput, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.input_eight_bits),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNeightBitOutput, XmCEightBitOutput, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.output_eight_bits),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNgeometry,XmCGeometry, XmRString, sizeof(char *),
	XtOffset(XtermWidget, misc.geo_metry),
	XmRString, (caddr_t) NULL},
{XmNalwaysHighlight,XmCAlwaysHighlight,XmRBoolean,
	sizeof(Boolean),XtOffset(XtermWidget, screen.always_highlight),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNinternalBorder,XmCBorderWidth,XmRInt, sizeof(int),
	XtOffset(XtermWidget, screen.border),
	XmRInt, (caddr_t) &defaultIntBorder},
{XmNjumpScroll, XmCJumpScroll, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.jumpscroll),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNbltScroll, XmCBltScroll, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.bltscroll),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNlistCallback,  XmCCallback, XmRCallback, sizeof (XtCallbackList),
	XtOffset (XtermWidget, screen.selectcallback),
        XmRImmediate, (caddr_t) NULL},
{XmNlogFile, XmCLogfile, XmRString, sizeof(char *),
	XtOffset(XtermWidget, screen.logfile),
	XmRString, (caddr_t) NULL},
{XmNlogging, XmCLogging, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.log_on),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNlogInhibit, XmCLogInhibit, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.logInhibit),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNmarginBell, XmCMarginBell, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.marginbell),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNpointerColor, XmCForeground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.mousecolor),
	XmRString, "XtDefaultForeground"},
{XmNpointerColorBackground, XmCBackground, XmRPixel, sizeof(Pixel),
	XtOffset(XtermWidget, screen.mousecolorback),
	XmRString, "XtDefaultBackground"},
{XmNpointerShape,XmCCursor, XmRCursor, sizeof(Cursor),
	XtOffset(XtermWidget, screen.pointer_cursor),
	XmRString, (caddr_t) "xterm"},
{XmNmultiClickTime,XmCMultiClickTime, XmRInt, sizeof(int),
	XtOffset(XtermWidget, screen.multiClickTime),
	XmRInt, (caddr_t) &defaultMultiClickTime},
{XmNmultiScroll,XmCMultiScroll, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.multiscroll),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNnMarginBell,XmCColumn, XmRInt, sizeof(int),
	XtOffset(XtermWidget, screen.nmarginbell),
	XmRInt, (caddr_t) &defaultNMarginBell},
{XmNreverseVideo,XmCReverseVideo,XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.re_verse),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNreverseWrap,XmCReverseWrap, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.reverseWrap),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNautoWrap,XmCAutoWrap, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.autoWrap),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNsaveLines, XmCSaveLines, XmRInt, sizeof(int),
	XtOffset(XtermWidget, screen.savelines),
	XmRInt, (caddr_t) &defaultSaveLines},
{XmNscrollBar, XmCScrollBar, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.scrollbar),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNscrollTtyOutput,XmCScrollCond, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.scrollttyoutput),
	XmRBoolean, (caddr_t) &defaultTRUE},
{XmNscrollKey, XmCScrollCond, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.scrollkey),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNscrollLines, XmCScrollLines, XmRInt, sizeof(int),
	XtOffset(XtermWidget, screen.scrolllines),
	XmRInt, (caddr_t) &defaultScrollLines},
{XmNselectedLine,XmCSelectedLine,XmRString, sizeof(char *),
	XtOffset(XtermWidget, screen.selectedline),
	XmRString, (caddr_t) NULL},
{XmNsignalInhibit,XmCSignalInhibit,XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.signalInhibit),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNtiteInhibit, XmCTiteInhibit, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.titeInhibit),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNvisualBell, XmCVisualBell, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.visualbell),
	XmRBoolean, (caddr_t) &defaultFALSE},
{XmNallowSendEvents, XmCAllowSendEvents, XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, screen.allowSendEvents),
	XmRBoolean, (caddr_t) &defaultFALSE},
{"dynamicColors", "DynamicColors", XmRBoolean, sizeof(Boolean),
	XtOffset(XtermWidget, misc.dynamicColors),
	XmRBoolean, (caddr_t) &defaultTRUE}
};


static void VTInitialize(), VTRealize(), VTExpose(), VTResize();
static void VTDestroy();
static Boolean VTSetValues();

XtermClassRec xtermClassRec = {
  {
/* core_class fields */	
    /* superclass	 */	(WidgetClass) &wptxmBoxClassRec,
    /* class_name	 */	"VT100",
    /* widget_size	 */	sizeof(XtermWidgetRec),
    /* class_initialize	 */	NULL,
    /* class_part_initialize */ NULL,
    /* class_inited	 */	FALSE,
    /* initialize	 */	VTInitialize,
    /* initialize_hook	 */	NULL,				
    /* realize		 */	VTRealize,
    /* actions		 */	actionsList,
    /* num_actions	 */	XtNumber(actionsList),
    /* resources	 */	resources,
    /* num_resources	 */	XtNumber(resources),
    /* xrm_class	 */	NULLQUARK,
    /* compress_motion	 */	TRUE,
    /* compress_exposure */	FALSE,
    /* compress_enterleave */	TRUE,
    /* visible_interest	 */	FALSE,
    /* destroy		 */	VTDestroy,
    /* resize		 */	VTResize,
    /* expose		 */	VTExpose,
    /* set_values	 */	VTSetValues, /* added 6/20/90 ewh */
    /* set_values_hook	 */	NULL,
    /* set_values_almost */	NULL,
    /* get_values_hook	 */	NULL,
    /* accept_focus	 */	NULL,
    /* version		 */	XtVersion,
    /* callback_offsets	 */	NULL,
    /* tm_table		 */	defaultTranslations,
    /* query_geometry	 */	XtInheritQueryGeometry,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension	 */	NULL
  }, 
    {	/* composite class record */    

	/* childrens geo mgr proc   */  XtInheritGeometryManager, 
	/* set changed proc	    */  VTResize, 
	/* add a child		    */	XtInheritInsertChild,
	/* remove a child	    */	XtInheritDeleteChild, 
	/* extension		    */	NULL, 
    }, 

    {	/* constraint class record */

	/* no additional resources  */	NULL, 
	/* num additional resources */	0, 
	/* size of constraint rec   */	0, 
	/* constraint_initialize    */	NULL, 
	/* constraint_destroy	    */	NULL, 
	/* constraint_setvalue	    */	NULL, 
	/* extension		    */	NULL, 
    }, 

    {	/* manager class record */
      XtInheritTranslations, 		/* default translations   */
      NULL, 				/* get resources      	  */
      0, 				/* num get_resources 	  */
      NULL, 				/* get_cont_resources     */
      0, 				/* num_get_cont_resources */
      XmInheritParentProcess,           /* parent_process         */
      NULL, 				/* extension		  */
    }, 
    {	/* box class record */     
	NULL,			/* extension */
    }, 	
};

WidgetClass xtermWidgetClass = (WidgetClass)&xtermClassRec;

VTparse()
{
  /* CODE ELIMINATED BY ewh 6/20/90 */;
}

finput(bptr,len)
     char **bptr;
     int *len;
{
	return(doinput(*bptr,*len));
}

in_put()
{
  ; /* CODE ELIMINATED BY ewh 6/20/90 */
}

/*
 * process a string of characters according to the character set indicated
 * by charset.	worry about end of line conditions (wraparound if selected).
 */
dotext(screen, flags, fg, bg, charset, buf, ptr)
     register TScreen  *screen;
     unsigned  flags;
     unsigned  fg, bg;
     char    charset;
     char  *buf;
     char  *ptr;
{
  register char	 *s;
  register int	len;
  register int	n;
  register int	next_col;

  switch (charset) {
  case 'A':  /* United Kingdom set	*/
    for (s=buf; s<ptr; ++s)
      if (*s == '#')
	*s = '\036';  /* UK pound sign*/
    break;

  case 'B':  /* ASCII set	 */
    break;

  case '0':  /* special graphics (line drawing)	 */
    for (s=buf; s<ptr; ++s)
      if (*s>=0x5f && *s<=0x7e)
	*s = *s == 0x5f ? 0x7f : *s - 0x5f;
    break;

  default:  /* any character sets we don't recognize*/
    return;
  }

  len = ptr - buf;
  ptr = buf;
  while (len > 0) {
    n = screen->max_col-screen->cur_col+1;
    if (n <= 1) {
      if (screen->do_wrap && (flags&WRAPAROUND)) {
	Index(screen, 1);
	screen->cur_col = 0;
	screen->do_wrap = 0;
	n = screen->max_col+1;
      } else
	n = 1;
    }
    if (len < n)
      n = len;
    next_col = screen->cur_col + n;
    WriteText(screen, ptr, n, flags, fg, bg);
    /*
     * the call to WriteText updates screen->cur_col.
     * If screen->cur_col != next_col, we must have
     * hit the right margin, so set the do_wrap flag.
 */
    screen->do_wrap = (screen->cur_col < next_col);
    len -= n;
    ptr += n;
  }
}

XtermOutText(term,bptr,len)
     XtermWidget term;
     char *bptr;
     int len;
{
  register TScreen *screen = &term->screen;
  register int *parsestate;
  register unsigned int c;
  register unsigned char *cp;
  register int row, col, top, bot, scstype;
  extern int bitset(), bitclr(), finput(), TrackMouse();
  int was_logging;

  parsestate = groundtable;
  if (screen->logging) 
    screen->logstart = bptr;	/* added 6/10/91, modified */
				/* 6/20/91 */
  while( len > 0 ) {
    switch (parsestate[c = doinput(bptr,len)]) {
    case CASE_PRINT:
      /* printable characters */
      top = len > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : len;
      cp = (unsigned char *)bptr;
      *--bptr = c;
      while(top > 0 && isprint(*cp & 0x7f)) {
	top--;
	len--;
	cp++;
      }
      if(screen->curss) {
	dotext(screen, term->flags,
	       term->cur_foreground,
	       term->cur_background,
	       screen->gsets[screen->curss], bptr, bptr + 1);
	screen->curss = 0;
	bptr++;
      }
      if(bptr < (char *)cp)
	dotext(screen, term->flags,
	       term->cur_foreground,
	       term->cur_background,
	       screen->gsets[screen->curgl], bptr, (char *)cp);
      bptr = (char *)cp;
      break;

    case CASE_GROUND_STATE:
      /* exit ignore mode */
      parsestate = groundtable;
      break;

    case CASE_IGNORE_STATE:
      /* Ies: ignore anything else */
      parsestate = igntable;
      break;

    case CASE_IGNORE_ESC:
      /* Ign: escape */
      parsestate = iestable;
      break;

    case CASE_IGNORE:
      /* Ignore character */
      break;

    case CASE_BELL:
      /* bell */
      Bell(term);
      break;

    case CASE_BS:
      /* backspace */
      CursorBack(screen, 1);
      break;

    case CASE_CR:
      /* carriage return */
      CarriageReturn(screen);
      break;

    case CASE_ESC:
      /* escape */
      parsestate = esctable;
      break;

    case CASE_VMOT:
      /*
       * form feed, line feed, vertical tab
 */
      Index(screen, 1);
      if (term->flags & LINEFEED)
	CarriageReturn(screen);
#if XlibSpecificationRelease >= 5
      if (QLength(screen->display) > 0)
	xevents(term);
#else
      if (screen->display->qlen > 0)
	xevents(term);
#endif
      break;

    case CASE_TAB:
      /* tab */
      screen->cur_col = TabNext(term);
      if (screen->cur_col > screen->max_col)
	screen->cur_col = screen->max_col;
      break;

    case CASE_SI:
      screen->curgl = 0;
      break;

    case CASE_SO:
      screen->curgl = 1;
      break;

    case CASE_SCR_STATE:
      /* enter scr state */
      parsestate = scrtable;
      break;

    case CASE_SCS0_STATE:
      /* enter scs state 0 */
      scstype = 0;
      parsestate = scstable;
      break;

    case CASE_SCS1_STATE:
      /* enter scs state 1 */
      scstype = 1;
      parsestate = scstable;
      break;

    case CASE_SCS2_STATE:
      /* enter scs state 2 */
      scstype = 2;
      parsestate = scstable;
      break;

    case CASE_SCS3_STATE:
      /* enter scs state 3 */
      scstype = 3;
      parsestate = scstable;
      break;

    case CASE_ESC_IGNORE:
      /* unknown escape sequence */
      parsestate = eigtable;
      break;

    case CASE_ESC_DIGIT:
      /* digit in csi or dec mode */
      if((row = screen->param[screen->nparam - 1]) == DEFAULT)
	row = 0;
      screen->param[screen->nparam - 1] = 10 * row + (c - '0');
      break;

    case CASE_ESC_SEMI:
      /* semicolon in csi or dec mode */
      screen->param[screen->nparam++] = DEFAULT;
      break;

    case CASE_DEC_STATE:
      /* enter dec mode */
      parsestate = dectable;
      break;

    case CASE_ICH:
      /* ICH */
      if((row = screen->param[0]) < 1)
	row = 1;
      InsertChar(screen, row);
      parsestate = groundtable;
      break;

    case CASE_CUU:
      /* CUU */
      if((row = screen->param[0]) < 1)
	row = 1;
      CursorUp(screen, row);
      parsestate = groundtable;
      break;

    case CASE_CUD:
      /* CUD */
      if((row = screen->param[0]) < 1)
	row = 1;
      CursorDown(screen, row);
      parsestate = groundtable;
      break;

    case CASE_CUF:
      /* CUF */
      if((row = screen->param[0]) < 1)
	row = 1;
      CursorForward(screen, row);
      parsestate = groundtable;
      break;

    case CASE_CUB:
      /* CUB */
      if((row = screen->param[0]) < 1)
	row = 1;
      CursorBack(screen, row);
      parsestate = groundtable;
      break;

    case CASE_CUP:
      /* CUP | HVP */
      if((row = screen->param[0]) < 1)
	row = 1;
      if(screen->nparam < 2 || (col = screen->param[1]) < 1)
	col = 1;
      CursorSet(screen, row-1, col-1, term->flags);
      parsestate = groundtable;
      break;

    case CASE_ED:
      /* ED */
      switch (screen->param[0]) {
      case DEFAULT:
      case 0:
	ClearBelow(screen);
	break;
	
      case 1:
	ClearAbove(screen);
	break;
	
      case 2:
	ClearScreen(screen);
	break;
      }
      parsestate = groundtable;
      break;

    case CASE_EL:
      /* EL */
      switch (screen->param[0]) {
      case DEFAULT:
      case 0:
	ClearRight(screen);
	break;
      case 1:
	ClearLeft(screen);
	break;
      case 2:
	ClearLine(screen);
	break;
      }
      parsestate = groundtable;
      break;

    case CASE_IL:
      /* IL */
      if((row = screen->param[0]) < 1)
	row = 1;
      InsertLine(screen, row);
      parsestate = groundtable;
      break;

    case CASE_DL:
      /* DL */
      if((row = screen->param[0]) < 1)
	row = 1;
      DeleteLine(screen, row);
      parsestate = groundtable;
      break;

    case CASE_DCH:
      /* DCH */
      if((row = screen->param[0]) < 1)
	row = 1;
      DeleteChar(screen, row);
      parsestate = groundtable;
      break;

    case CASE_TRACK_MOUSE:
      /* Track mouse as long as in window and between
	 specified rows */
      TrackMouse(term, screen->param[0], screen->param[2]-1,
		 screen->param[1]-1, screen->param[3]-1, screen->param[4]-2);
      break;

    case CASE_DECID:
      screen->param[0] = -1;	/* Default ID parameter */
      /* Fall through into ... */

    case CASE_TBC:
      /* TBC */
      if ((row = screen->param[0]) <= 0) /* less than means default */
	TabClear(term->tabs, screen->cur_col);
      else if (row == 3)
	TabZonk(term->tabs);
      parsestate = groundtable;
      break;

    case CASE_SET:
      /* SET */
      modes(term, bitset);
      parsestate = groundtable;
      break;

    case CASE_RST:
      /* RST */
      modes(term, bitclr);
      parsestate = groundtable;
      break;

    case CASE_SGR:
      /* SGR */
      for (row=0; row<screen->nparam; ++row) {
	switch (screen->param[row]) {
	case DEFAULT:
	case 0:
	  term->flags &=
	    ~(INVERSE|BOLD|UNDERLINE|FG_COLOR|
	      BG_COLOR|DIM);
	  break;
	case 1:
	case 5:	 /* Blink, really. */
	  term->flags |= BOLD;
	  break;
	case 2:
	  term->flags |= DIM;
	  break;
	case 4:	 /* Underscore	 */
	  term->flags |= UNDERLINE;
	  break;
	case 7:
	  term->flags |= INVERSE;
	  break;
	case 30:
	case 31:
	case 32:
	case 33:
	case 34:
	case 35:
	case 36:
	case 37:
	  term->flags |= FG_COLOR;
	  term->cur_foreground = screen->param[row] - 30;
	  break;
	case 40:
	case 41:
	case 42:
	case 43:
	case 44:
	case 45:
	case 46:
	case 47:
	  term->flags |= BG_COLOR;
	  term->cur_background = screen->param[row] - 40;
	  break;
	}
      }
      parsestate = groundtable;
      break;

    case CASE_CPR:
      /* CPR */
      /* NOT IMPLEMENTED */
      Bell(term);
      parsestate = groundtable;
      break;

    case CASE_DECSTBM:
      /* DECSTBM */
      if((top = screen->param[0]) < 1)
	top = 1;
      if(screen->nparam < 2 || (bot = screen->param[1]) == DEFAULT
	 || bot > screen->max_row + 1
	 || bot == 0)
	bot = screen->max_row+1;
      if (bot > top) {
	if(screen->scroll_amt)
	  FlushScroll(screen);
	screen->top_marg = top-1;
	screen->bot_marg = bot-1;
	CursorSet(screen, 0, 0, term->flags);
      }
      parsestate = groundtable;
      break;

    case CASE_DECREQTPARM:
      /* DECREQTPARM */
      /* NOT IMPLEMENTED ... */
      Bell(term);
      parsestate = groundtable;
      break;

    case CASE_DECSET:
      /* DECSET */
      was_logging = screen->logging;
      dpmodes(term, bitset);
      parsestate = groundtable;
      if ((was_logging != screen->logging) &&
	  (screen->logging))
	screen->logstart = bptr;
      break;

    case CASE_DECRST:
      /* DECRST */
      was_logging = screen->logging;
      dpmodes(term, bitclr);
      parsestate = groundtable;
      if ((was_logging != screen->logging) &&
	  (!screen->logging)) {
	screen->logging = 1;	/* temporary (cluge) */
	FlushLog(screen,bptr);
	CloseLog(screen);
	screen->logstart = NULL;
      }
      break;

    case CASE_DECALN:
      /* DECALN */
      if(screen->cursor_state)
	HideCursor(screen);
      for(row = screen->max_row ; row >= 0 ; row--) {
	bzero((char *)screen->buf[4 * row + 1],
	      col = screen->max_col + 1);
	for(cp = (unsigned char *)screen->buf[4 * row] ; col > 0 ; col--)
	  *cp++ = (unsigned char) 'E';
      }
      ScrnRefresh(screen, 0, 0, screen->max_row + 1,
		  screen->max_col + 1, False);
      parsestate = groundtable;
      break;

    case CASE_GSETS:
      screen->gsets[scstype] = c;
      parsestate = groundtable;
      break;

    case CASE_DECSC:
      /* DECSC */
      CursorSave(term, &screen->sc);
      parsestate = groundtable;
      break;

    case CASE_DECRC:
      /* DECRC */
      CursorRestore(term, &screen->sc);
      parsestate = groundtable;
      break;

    case CASE_DECKPAM:
      /* DECKPAM */
      term->keyboard.flags |= KYPD_APL;
      parsestate = groundtable;
      break;

    case CASE_DECKPNM:
      /* DECKPNM */
      term->keyboard.flags &= ~KYPD_APL;
      parsestate = groundtable;
      break;

    case CASE_IND:
      /* IND */
      Index(screen, 1);
#if XlibSpecificationRelease >= 5
      if (QLength(screen->display) > 0)
        xevents(term);
#else
      if (screen->display->qlen > 0)
        xevents(term);
#endif
      parsestate = groundtable;
      break;

    case CASE_NEL:
      /* NEL */
      Index(screen, 1);
      CarriageReturn(screen);

#if XlibSpecificationRelease >= 5
      if (QLength(screen->display) > 0)
        xevents(term);
#else
      if (screen->display->qlen > 0)
        xevents(term);
#endif
      parsestate = groundtable;
      break;

    case CASE_HTS:
      /* HTS */
      TabSet(term->tabs, screen->cur_col);
      parsestate = groundtable;
      break;

    case CASE_RI:
      /* RI */
      RevIndex(screen, 1);
      parsestate = groundtable;
      break;

    case CASE_SS2:
      /* SS2 */
      screen->curss = 2;
      parsestate = groundtable;
      break;

    case CASE_SS3:
      /* SS3 */
      screen->curss = 3;
      parsestate = groundtable;
      break;

    case CASE_CSI_STATE:
      /* enter csi state */
      screen->nparam = 1;
      screen->param[0] = DEFAULT;
      parsestate = csitable;
      break;

    case CASE_OSC:
      /* do osc escapes */
      do_osc(term,finput,&bptr,&len);
      parsestate = groundtable;
      break;

    case CASE_RIS:
      /* RIS */
      VTReset(TRUE,term);
      parsestate = groundtable;
      break;

    case CASE_LS2:
      /* LS2 */
      screen->curgl = 2;
      parsestate = groundtable;
      break;

    case CASE_LS3:
      /* LS3 */
      screen->curgl = 3;
      parsestate = groundtable;
      break;

    case CASE_LS3R:
      /* LS3R */
      screen->curgr = 3;
      parsestate = groundtable;
      break;

    case CASE_LS2R:
      /* LS2R */
      screen->curgr = 2;
      parsestate = groundtable;
      break;

    case CASE_LS1R:
      /* LS1R */
      screen->curgr = 1;
      parsestate = groundtable;
      break;

    case CASE_XTERM_SAVE:
      savemodes(term);
      parsestate = groundtable;
      break;

    case CASE_XTERM_RESTORE:
      restoremodes(term);
      parsestate = groundtable;
      break;
    }
  }
  if (screen->logging) 
    FlushLog(screen,bptr);	/* added 6/10/91 */
}

XtermDoNewLine(term) 
     XtermWidget term;
{
  /* This routine added 6/14/91 to aid in the implementation of the */
  /* TAE presentation type... */
  static char *nl = "\n";
  Index(&term->screen,1);
  term->screen.cur_col = 0;
  term->screen.logstart = nl;
  FlushLog(&term->screen,&nl[1]); /* in case we are logging... */
}

XtermClearScreen(term)
     XtermWidget term;
{
  /* Added 9/9/91 for use by OASIS C interface... */
  ClearScreen(&term->screen);
}

XtermReset(term)
     XtermWidget term;
{
  /* Added 02/20/92 ewh for purposes of Wpt_SetNoValue() */
  term->screen.savedlines = 0;
  term->screen.topline = 0;
  term->screen.cur_col = 0;
  term->screen.cur_row = 0;
  ClearScreen(&term->screen);
}

XtermFlushScroll(term)
     XtermWidget term;
{
  FlushScroll(&term->screen);
}

XtermScrollTextBy(term,rows)
     XtermWidget term;
{
  int newTopLine = term->screen.topline + rows;
  WindowScroll(&term->screen, newTopLine);
}

/*
 * write a string str of length len onto the screen at
 * the current cursor position.	 update cursor position.
 */
WriteText(screen, str, len, flags, fg, bg)
     register TScreen  *screen;
     register char  *str;
     register int  len;
     unsigned  flags;
     unsigned  fg, bg;
{
  register int cx, cy;
  register unsigned fgs = flags;
  register Pixel fg_pix, bg_pix;
  GC  currentGC;
  XtermWidget term = (XtermWidget)screen->parent;

  fg_pix = (fgs&FG_COLOR) ? screen->colors[fg] : screen->foreground;
  bg_pix = (fgs&BG_COLOR) ? screen->colors[bg] : term->core.background_pixel;

  /* Following lines added 9/18/91 by ewh to implement scrollttyoutput */
  if(screen->scrollWidget && screen->scrollttyoutput &&
     screen->topline < 0)
    /* Scroll to bottom */
    WindowScroll(screen, 0);

   if(screen->cur_row - screen->topline <= screen->max_row) {
    /*
      if(screen->cur_row == screen->cursor_row && screen->cur_col <=
      screen->cursor_col && screen->cursor_col <= screen->cur_col + len - 1)
      screen->cursor_state = OFF;
 */
    if(screen->cursor_state)
      HideCursor(screen);

    /*
     *	make sure that the correct GC is current
 */

    if (fgs & INVERSE) {
      if (fgs & BOLD)
	currentGC = screen->reverseboldGC;
      else  currentGC = screen->reverseGC;

      XSetForeground(screen->display, currentGC, bg_pix);
      XSetBackground(screen->display, currentGC, fg_pix);

    } else {  /* not inverse */
      if (fgs & BOLD)
	currentGC = screen->normalboldGC;
      else  currentGC = screen->normalGC;

      XSetForeground(screen->display, currentGC, fg_pix);
      XSetBackground(screen->display, currentGC, bg_pix);

    }

    if (fgs & INSERT)
      InsertChar(screen, len);
    if (!(AddToRefresh(screen))) {
      if(screen->scroll_amt)
	FlushScroll(screen);
      cx = CursorX(screen, screen->cur_col);
      cy = CursorY(screen, screen->cur_row)+screen->fnt_norm->ascent;


      XDrawImageString(screen->display, TextWindow(screen), currentGC,
		       cx, cy, str, len);

      if((fgs & BOLD) && screen->enbolden)
	if (currentGC == screen->normalGC || screen->reverseGC)
	  XDrawString(screen->display, TextWindow(screen),
		      currentGC,cx + 1, cy, str, len);

      if(fgs & UNDERLINE)
	XDrawLine(screen->display, TextWindow(screen), currentGC,
		  cx, cy+1,
		  cx + len * FontWidth(screen), cy+1);
      /*
       * the following statements compile data to compute the average
       * number of characters written on each call to XText.  The data
       * may be examined via the use of a "hidden" escape sequence.
 */
      ctotal += len;
      ++ntotal;
    }
  }
  ScreenWrite(screen, str, flags, fg, bg, len);
  CursorForward(screen, len);
}

/*
 * process ANSI modes set, reset
 */
modes(term, func)
     XtermWidget  term;
     int    (*func)();
{
  register int	i;

  for (i=0; i<term->screen.nparam; ++i) {
    switch (term->screen.param[i]) {
    case 4:	 /* IRM	 */
      (*func)(&term->flags, INSERT);
      break;

    case 20:	/* LNM	 */
      (*func)(&term->flags, LINEFEED);
      break;
    }
  }
}

/*
 * process DEC private modes set, reset
 */
dpmodes(term, func)
     XtermWidget  term;
     int    (*func)();
{
  register TScreen  *screen  = &term->screen;
  register int	i, j;
  extern int bitset();

  for (i=0; i<screen->nparam; ++i) {
    switch (screen->param[i]) {
    case 1:	 /* DECCKM	*/
      (*func)(&term->keyboard.flags, CURSOR_APL);
      break;
    case 2:	 /* ANSI/VT52 mode */
      if (func == bitset) {
	screen->gsets[0] =
	  screen->gsets[1] =
	    screen->gsets[2] =
	      screen->gsets[3] = 'B';
	screen->curgl = 0;
	screen->curgr = 2;
      }
      break;
    case 3:	 /* DECCOLM	 */
      if(screen->c132) {
	ClearScreen(screen);
	CursorSet(screen, 0, 0, term->flags);
	if((j = func == bitset ? 132 : 80) !=
	   ((term->flags & IN132COLUMNS) ? 132 : 80) ||
	   j != screen->max_col + 1) {
	  Dimension replyWidth, replyHeight;
	  XtGeometryResult status;
	
	  status = XtMakeResizeRequest (
					(Widget) term,
					(Dimension) FontWidth(screen) * j
					+ 2*(screen->border + SHAD)
					+ screen->scrollbar,
					(Dimension) FontHeight(screen)
					* (screen->max_row + 1)
					+ 2 *(screen->border + SHAD),
					&replyWidth, &replyHeight);
	
	  if (status == XtGeometryYes ||
	      status == XtGeometryDone) {
	    ScreenResize (&term->screen,
			  (int)replyWidth,
			  (int)replyHeight,
			  &term->flags);
	  }
	}
	(*func)(&term->flags, IN132COLUMNS);
      }
      break;
    case 4:	 /* DECSCLM (slow scroll) */
      if (func == bitset) {
	screen->jumpscroll = 0;
	if (screen->scroll_amt)
	  FlushScroll(screen);
      } else
	screen->jumpscroll = 1;
      (*func)(&term->flags, SMOOTHSCROLL);
      break;
    case 5:	 /* DECSCNM	 */
      j = term->flags;
      (*func)(&term->flags, REVERSE_VIDEO);
      if ((term->flags ^ j) & REVERSE_VIDEO)
	ReverseVideo(term);
      /* update_reversevideo done in RevVid */
      break;

    case 6:	 /* DECOM */
      (*func)(&term->flags, ORIGIN);
      CursorSet(screen, 0, 0, term->flags);
      break;

    case 7:	 /* DECAWM	*/
      (*func)(&term->flags, WRAPAROUND);
      break;
    case 8:	 /* DECARM	*/
      /* ignore autorepeat */
      break;
    case 9:	 /* MIT bogus sequence	 */
      if(func == bitset)
	screen->send_mouse_pos = 1;
      else
	screen->send_mouse_pos = 0;
      break;
    case 40:	/* 132 column mode */
      screen->c132 = (func == bitset);
      break;
    case 41:	/* curses hack	 */
      screen->curses = (func == bitset);
      break;
    case 44:	/* margin bell	 */
      screen->marginbell = (func == bitset);
      if(!screen->marginbell)
	screen->bellarmed = -1;
      break;
    case 45:	/* reverse wraparound */
      (*func)(&term->flags, REVERSEWRAP);
      break;
    case 46:	/* logging */
#ifdef ALLOWLOGFILEONOFF
      /*
       * if this feature is enabled, logging may be
       * enabled and disabled via escape sequences.
       */
      if(func == bitset)
	StartLog(screen);
      else
	screen->logging = 0;
#else
      Bell(term);
      Bell(term);
#endif
      break;
    case 47:	/* alternate buffer */
      if(func == bitset)
	ToAlternate(screen);
      else
	FromAlternate(screen);
      break;
    case 1000:	  /* xtem bogus sequence */
      if(func == bitset)
	screen->send_mouse_pos = 2;
      else
	screen->send_mouse_pos = 0;
      break;
    case 1001:	  /* xtem sequence w/hilite tracking */
      if(func == bitset)
	screen->send_mouse_pos = 3;
      else
	screen->send_mouse_pos = 0;
      break;
    }
  }
}

/*
 * process xterm private modes save
 */
savemodes(term)
     XtermWidget term;
{
  register TScreen  *screen  = &term->screen;
  register int i;

  for (i = 0; i < screen->nparam; i++) {
    switch (screen->param[i]) {
    case 1:	 /* DECCKM	*/
      screen->save_modes[0] = term->keyboard.flags &
	CURSOR_APL;
      break;
    case 3:	 /* DECCOLM	 */
      if(screen->c132)
	screen->save_modes[1] = term->flags &
	  IN132COLUMNS;
      break;
    case 4:	 /* DECSCLM (slow scroll) */
      screen->save_modes[2] = term->flags & SMOOTHSCROLL;
      break;
    case 5:	 /* DECSCNM	 */
      screen->save_modes[3] = term->flags & REVERSE_VIDEO;
      break;
    case 6:	 /* DECOM */
      screen->save_modes[4] = term->flags & ORIGIN;
      break;

    case 7:	 /* DECAWM	*/
      screen->save_modes[5] = term->flags & WRAPAROUND;
      break;
    case 8:	 /* DECARM	*/
      /* ignore autorepeat */
      break;
    case 9:	 /* mouse bogus sequence */
      screen->save_modes[7] = screen->send_mouse_pos;
      break;
    case 40:	/* 132 column mode */
      screen->save_modes[8] = screen->c132;
      break;
    case 41:	/* curses hack	 */
      screen->save_modes[9] = screen->curses;
      break;
    case 44:	/* margin bell	 */
      screen->save_modes[12] = screen->marginbell;
      break;
    case 45:	/* reverse wraparound */
      screen->save_modes[13] = term->flags & REVERSEWRAP;
      break;
    case 46:	/* logging */
      screen->save_modes[14] = screen->logging;
      break;
    case 47:	/* alternate buffer */
      screen->save_modes[15] = screen->alternate;
      break;
    case 1000:	/* mouse bogus sequence */
    case 1001:
      screen->save_modes[7] = screen->send_mouse_pos;
      break;
    }
  }
}

/*
 * process xterm private modes restore
 */
restoremodes(term)
     XtermWidget term;
{
  register TScreen  *screen  = &term->screen;
  register int i, j;

  for (i = 0; i < screen->nparam; i++) {
    switch (screen->param[i]) {
    case 1:	 /* DECCKM	*/
      term->keyboard.flags &= ~CURSOR_APL;
      term->keyboard.flags |= screen->save_modes[0] &
	CURSOR_APL;
      break;
    case 3:	 /* DECCOLM	 */
      if(screen->c132) {
	ClearScreen(screen);
	CursorSet(screen, 0, 0, term->flags);
	if((j = (screen->save_modes[1] & IN132COLUMNS)
	    ? 132 : 80) != ((term->flags & IN132COLUMNS)
			    ? 132 : 80) || j != screen->max_col + 1) {
	  Dimension replyWidth, replyHeight;
	  XtGeometryResult status;
	  status = XtMakeResizeRequest (
					(Widget) term,
					(Dimension) FontWidth(screen) * j
					+ 2*(screen->border + SHAD)
					+ screen->scrollbar,
					(Dimension) FontHeight(screen)
					* (screen->max_row + 1)
					+ 2*(screen->border + SHAD),
					&replyWidth, &replyHeight);
	
	  if (status == XtGeometryYes ||
	      status == XtGeometryDone) {
	    ScreenResize (&term->screen,
			  (int)replyWidth,
			  (int)replyHeight,
			  &term->flags);
	  }
	}
	term->flags &= ~IN132COLUMNS;
	term->flags |= screen->save_modes[1] &
	  IN132COLUMNS;
      }
      break;
    case 4:	 /* DECSCLM (slow scroll) */
      if (screen->save_modes[2] & SMOOTHSCROLL) {
	screen->jumpscroll = 0;
	if (screen->scroll_amt)
	  FlushScroll(screen);
      } else
	screen->jumpscroll = 1;
      term->flags &= ~SMOOTHSCROLL;
      term->flags |= screen->save_modes[2] & SMOOTHSCROLL;
      break;
    case 5:	 /* DECSCNM	 */
      if((screen->save_modes[3] ^ term->flags) &
	 REVERSE_VIDEO) {
	term->flags &= ~REVERSE_VIDEO;
	term->flags |= screen->save_modes[3] &
	  REVERSE_VIDEO;
	ReverseVideo(term);
	/* update_reversevideo done in RevVid */
      }
      break;
    case 6:	 /* DECOM */
      term->flags &= ~ORIGIN;
      term->flags |= screen->save_modes[4] & ORIGIN;
      CursorSet(screen, 0, 0, term->flags);
      break;

    case 7:	 /* DECAWM	*/
      term->flags &= ~WRAPAROUND;
      term->flags |= screen->save_modes[5] & WRAPAROUND;
      break;
    case 8:	 /* DECARM	*/
      /* ignore autorepeat */
      break;
    case 9:	 /* MIT bogus sequence	 */
      screen->send_mouse_pos = screen->save_modes[7];
      break;
    case 40:	/* 132 column mode */
      screen->c132 = screen->save_modes[8];
      break;
    case 41:	/* curses hack	 */
      screen->curses = screen->save_modes[9];
      break;
    case 44:	/* margin bell	 */
      if(!(screen->marginbell = screen->save_modes[12]))
	screen->bellarmed = -1;
      break;
    case 45:	/* reverse wraparound */
      term->flags &= ~REVERSEWRAP;
      term->flags |= screen->save_modes[13] & REVERSEWRAP;
      break;
    case 46:	/* logging */
#ifdef ALLOWLOGFILEONOFF
      if(screen->save_modes[14])
	StartLog(screen);
      else
	CloseLog(screen);
#endif /* ALLOWLOGFILEONOFF */
      /* update_logging done by StartLog and CloseLog */
      break;
    case 47:	/* alternate buffer */
      if(screen->save_modes[15])
	ToAlternate(screen);
      else
	FromAlternate(screen);
      /* update_altscreen done by ToAlt and FromAlt */
      break;
    case 1000:	  /* mouse bogus sequence */
    case 1001:
      screen->send_mouse_pos = screen->save_modes[7];
      break;
    }
  }
}

/*
 * set a bit in a word given a pointer to the word and a mask.
 */
bitset(p, mask)
     int  *p;
{
  *p |= mask;
}

/*
 * clear a bit in a word given a pointer to the word and a mask.
 */
bitclr(p, mask)
     int  *p;
{
  *p &= ~mask;
}

ToAlternate(screen)
     register TScreen *screen;
{
  extern ScrnBuf Allocate();

  if(screen->alternate)
    return;
  if(!screen->altbuf)
    screen->altbuf = Allocate(screen->max_row + 1, screen->max_col
			      + 1, (Char **)&screen->abuf_address);
  SwitchBufs(screen);
  screen->alternate = TRUE;
}

FromAlternate(screen)
     register TScreen *screen;
{
  if(!screen->alternate)
    return;
  screen->alternate = FALSE;
  SwitchBufs(screen);
}

SwitchBufs(screen)
     register TScreen *screen;
{
  register int rows, top;
  char *save [4 * MAX_ROWS];

  if(screen->cursor_state)
    HideCursor(screen);
  rows = screen->max_row + 1;
  bcopy((char *)screen->buf, (char *)save, 4 * sizeof(char *) * rows);
  bcopy((char *)screen->altbuf, (char *)screen->buf, 4 * sizeof(char *) *
	rows);
  bcopy((char *)save, (char *)screen->altbuf, 4 * sizeof(char *) * rows);

  if((top = -screen->topline) <= screen->max_row) {
    if(screen->scroll_amt)
      FlushScroll(screen);
    if(top == 0)
      XClearWindow(screen->display, TextWindow(screen));
    else
      XClearArea(
		 screen->display,
		 TextWindow(screen),
		 (int) screen->border + SHAD + screen->scrollbar,
		 (int) top * FontHeight(screen) + screen->border + SHAD,
		 (unsigned) Width(screen),
		 (unsigned) (screen->max_row - top + 1)
		 * FontHeight(screen),
		 FALSE);
  }
  ScrnRefresh(screen, 0, 0, rows, screen->max_col + 1, False);
}


/*ARGSUSED*/
static void VTExpose(w, event, region)
     Widget w;
     XEvent *event;
     Region region;
{
  XtermWidget term=(XtermWidget)w;
  register TScreen *screen = &term->screen;

  if (event->type == Expose)
    HandleExposure (screen, (XExposeEvent *)event);
}

static void VTGraphicsOrNoExpose (event,term)
     XEvent *event;
     XtermWidget term;
{
  register TScreen *screen = &term->screen;
  if (screen->incopy <= 0) {
    screen->incopy = 1;
    if (screen->scrolls > 0)
      screen->scrolls--;
  }
  if (event->type == GraphicsExpose)
    if (HandleExposure (screen, (XExposeEvent *)event))
      screen->cursor_state = OFF;
  if ((event->type == NoExpose) || ((XGraphicsExposeEvent *)event)->count == 0) {
    if (screen->incopy <= 0 && screen->scrolls > 0)
      screen->scrolls--;
    if (screen->scrolls)
      screen->incopy = -1;
    else
      screen->incopy = 0;
  }
}

/*ARGSUSED*/
static void VTNonMaskableEvent (w, closure, event)
     Widget w;
     caddr_t closure;
     XEvent *event;
{
  switch (event->type) {
  case MappingNotify:
    XRefreshKeyboardMapping (&event->xmapping);
    break;
  case GraphicsExpose:
  case NoExpose:
    VTGraphicsOrNoExpose (event,(XtermWidget)closure);
    break;
  }
}




static void VTResize(w)
     Widget w;
{
  XtermWidget term=(XtermWidget)w;
  if (XtIsRealized(w))
    ScreenResize (&term->screen, (int)XtWidth(w), (int)XtHeight(w),
		  &term->flags);
}

int VTInit (term)
     XtermWidget term;
{
  register TScreen *screen = &term->screen;

  XtRealizeWidget (term->core.parent);
  if (screen->allbuf == NULL) VTallocbuf (term);
  return (1);
}

static void VTallocbuf (term)
     XtermWidget term;
{
  register TScreen *screen = &term->screen;
  int nrows = screen->max_row + 1;
  extern ScrnBuf Allocate();

  /* allocate screen buffer now, if necessary. */
  if (screen->scrollWidget)
    nrows += screen->savelines;
  screen->allbuf = Allocate (nrows, screen->max_col + 1,
			     (Char **)&screen->sbuf_address);
  if (screen->scrollWidget)
    screen->buf = &screen->allbuf[4 * screen->savelines];
  else
    screen->buf = screen->allbuf;
  return;
}

static void VTfreebufs (term)
     XtermWidget term;
{
  TScreen *screen = &term->screen;
  extern void Deallocate();

  if (screen->allbuf) Deallocate((char *)screen->allbuf);
  if (screen->sbuf_address) Deallocate(screen->sbuf_address);
  if (screen->altbuf) Deallocate((char *)screen->altbuf);
  if (screen->abuf_address) Deallocate(screen->abuf_address);

  screen->allbuf = NULL;
  screen->sbuf_address = NULL;
  screen->altbuf = NULL;
  screen->abuf_address = NULL;
  screen->buf = NULL;

}

static void VTInitialize (request, new)
     XtermWidget request, new;
{
  int i;
  char *news;			/* added 02/04/93 for fix to memory
				   bash ewh...  */
  /* Zero out the entire "screen" component of "new" widget,
     then do field-by-field assigment of "screen" fields
     that are named in the resource list. */

  bzero ((char *) &new->screen, sizeof(new->screen));

  /* added the following 6/4/91 to simplify code...*/
  new->screen.parent = (XtermWidget)new;

  new->screen.c132 = request->screen.c132;
  new->screen.curses = request->screen.curses;
  new->screen.foreground = request->manager.foreground;
  new->screen.cursorcolor = request->screen.cursorcolor;
  new->screen.border = request->screen.border;
  new->screen.jumpscroll = request->screen.jumpscroll;
  new->screen.bltscroll = request->screen.bltscroll;
  news = new->screen.logfile;
  if (news && (!news))
    news = new->screen.logfile = NULL;
  if (news) {			/* allocate internal copy to be deleted */
    new->screen.logfile = XtMalloc(strlen(news)+1);
    strcpy(new->screen.logfile,news);
  }
  new->screen.marginbell = request->screen.marginbell;
  new->screen.mousecolor = request->screen.mousecolor;
  new->screen.mousecolorback = request->screen.mousecolorback;
  new->screen.multiscroll = request->screen.multiscroll;
  new->screen.nmarginbell = request->screen.nmarginbell;
  new->screen.savelines = request->screen.savelines;
  new->screen.scrolllines = request->screen.scrolllines;
  new->screen.scrollttyoutput = request->screen.scrollttyoutput;
  new->screen.scrollkey = request->screen.scrollkey;
  new->screen.selectedline = request->screen.selectedline;
  new->screen.selectcallback = request->screen.selectcallback;
  new->screen.visualbell = request->screen.visualbell;
  new->misc.re_verse = request->misc.re_verse;
  new->screen.multiClickTime = request->screen.multiClickTime;
  new->screen.charClass = request->screen.charClass;
  new->screen.cutNewline = request->screen.cutNewline;
  new->screen.cutToBeginningOfLine = request->screen.cutToBeginningOfLine;
  new->screen.always_highlight = request->screen.always_highlight;
  new->screen.pointer_cursor = request->screen.pointer_cursor;
  new->screen.input_eight_bits = request->screen.input_eight_bits;
  new->screen.output_eight_bits = request->screen.output_eight_bits;
  new->screen.allowSendEvents = request->screen.allowSendEvents;
  new->misc.titeInhibit = request->misc.titeInhibit;
  new->flags = 0;

  if (!new->screen.jumpscroll) {
    new->flags |= SMOOTHSCROLL;
  }

  new->misc.reverseWrap = request->misc.reverseWrap;
  if (new->misc.reverseWrap) {
    new->flags |= REVERSEWRAP;
  }

  new->misc.autoWrap = request->misc.autoWrap;
  if (new->misc.autoWrap) {
    new->flags |= WRAPAROUND;
  }

  if (new->misc.re_verse) {
    new->flags |= REVERSE_VIDEO;
  }

  new->screen.inhibit = 0;
  if (new->misc.logInhibit = request->misc.logInhibit)
    new->screen.inhibit |= I_LOG;
  if (new->misc.signalInhibit = request->misc.signalInhibit)
    new->screen.inhibit |= I_SIGNAL;

  new->initflags = new->flags;

  for (i = 0; i < MAXCOLORS; i++) {
    new->screen.colors[i] = request->screen.colors[i];
  }
  /* set default in realize proc */

  /*
   * The definition of -rv now is that it changes the definition of
   * XtDefaultForeground and XtDefaultBackground.  So, we no longer
   * need to do anything special.
 */
  new->keyboard.flags = 0;
  new->screen.display = new->core.screen->display;
  /* new->core.height = new->core.width = 1; */
  /* dummy values so that we don't try to Realize the parent shell
     with height or width of 0, which is illegal in X.	The real
     size is computed in the xtermWidget's Realize proc,
     but the shell's Realize proc is called first, and must see
     a valid size. */

  /* look for focus related events on the shell, because we need
   * to care about the shell's border being part of our focus.
   */
  XtAddEventHandler((Widget)new, EnterWindowMask, FALSE,
		    HandleEnterWindow, (Opaque)NULL);
  XtAddEventHandler((Widget)new, LeaveWindowMask, FALSE,
		    HandleLeaveWindow, (Opaque)NULL);
  XtAddEventHandler((Widget)new, FocusChangeMask, FALSE,
		    HandleFocusChange, (Opaque)NULL);
  XtAddEventHandler((Widget)new, 0L, TRUE,
		    VTNonMaskableEvent, (Opaque)new);

  set_character_class (new->screen.charClass);

  if (request->misc.scrollbar) {
    /* create it, but don't realize it */
    ScrollBarOn (new, TRUE, FALSE);
  }
  new->screen.pOldColors = NULL;

  if (request->misc.log_on)
    StartLog(&new->screen);

  return;
}

static Boolean VTSetValues(current, request, new)
     XtermWidget current;
     XtermWidget request;
     XtermWidget new;
{
  Boolean retval;
  TScreen *screen = &new->screen;
  TScreen *reqscr = &request->screen;
  int i;
  char *news,*olds;

  retval = False;

  screen->parent = new;		/* set 'parent' field properly... */

  /* Start at the top of the resources, and check each one to see if a */
  /* change has been made... */
  if ((strcmp(current->misc.f_n,request->misc.f_n)) ||
      (strcmp(current->misc.f_b,request->misc.f_b))){
    /* Change the font.. */
    
    screen->fnt_norm = screen->fnt_bold = NULL;
    if (!LoadNewFont(screen, request->misc.f_n, 
		     request->misc.f_b, False, new)) {
      fprintf (stderr,
	       "Xterm:  unable to open font \"%s\", trying \"fixed\"....\n",
	       request->misc.f_n);
      (void) LoadNewFont (screen, "fixed", NULL, False, new);
    }

    /* really screwed if we couldn't open default font */
    if (!screen->fnt_norm) {
      fprintf (stderr, "Xterm:  unable to locate a suitable font\n");
      exit (1);
    }
    retval = True;
  }

  screen->c132 = reqscr->c132;	/* simple flag... */
  screen->charClass = reqscr->charClass;
  screen->curses = reqscr->curses;

  if (screen->border != reqscr->border) {
    screen->border = reqscr->border;
    retval = True;
  }

  screen->jumpscroll = reqscr->jumpscroll;
  screen->bltscroll = reqscr->bltscroll;
  news = screen->logfile;
  olds = current->screen.logfile;
  { 
    if ((news) && (!(*news))) news = NULL;
    if (((news != NULL) ^ (olds != NULL)) ||
        (news && (strcmp(news,olds))))
      {
	/* Different log file name.... */
        if (olds) {
	  XtFree(olds);
	  olds = 0;
	}
        if (news) {
	  screen->logfile = (char *)XtMalloc(strlen(news)+1);
	  strcpy(screen->logfile,news);
	  news = screen->logfile;
	}
      }
    else			/* there the same */
      {				/* bug fix (12/02/92 ewh) */
	news = olds;		/* keep that way... */
      }
  }
  screen->logfile = news;
  current->screen.logfile = olds;

  screen->marginbell = reqscr->marginbell;
  screen->multiscroll = reqscr->multiscroll;
  screen->nmarginbell = reqscr->nmarginbell;

  screen->scrolllines = reqscr->scrolllines;
  screen->scrollttyoutput = reqscr->scrollttyoutput;
  screen->scrollkey = reqscr->scrollkey;
  screen->selectcallback = reqscr->selectcallback;
  screen->visualbell = reqscr->visualbell;
  screen->multiClickTime = reqscr->multiClickTime;
  screen->cutNewline = reqscr->cutNewline;
  screen->cutToBeginningOfLine = reqscr->cutToBeginningOfLine;
  screen->always_highlight = reqscr->always_highlight;

  screen->input_eight_bits = reqscr->input_eight_bits;
  screen->output_eight_bits = reqscr->output_eight_bits;
  screen->allowSendEvents = reqscr->allowSendEvents;
  new->misc.titeInhibit = request->misc.titeInhibit;

  for (i = 0; i < MAXCOLORS; i++) {
    if (screen->colors[i] != reqscr->colors[i]) {
      screen->colors[i] = reqscr->colors[i];
      retval = True;
    }
  }

  if (new->misc.re_verse != request->misc.re_verse) {
    new->misc.re_verse = request->misc.re_verse;
    retval = True;
  }

  new->flags = request->flags;

  if (!new->screen.jumpscroll) {
    new->flags |= SMOOTHSCROLL;
  } else
    new->flags &= ~SMOOTHSCROLL;

  new->misc.reverseWrap = request->misc.reverseWrap;
  if (new->misc.reverseWrap) {
    new->flags |= REVERSEWRAP;
  } else
    new->flags &= ~REVERSEWRAP;

  new->misc.autoWrap = request->misc.autoWrap;
  if (new->misc.autoWrap) {
    new->flags |= WRAPAROUND;
  } else
    new->flags &= ~WRAPAROUND;

  if (new->misc.re_verse) {
    new->flags |= REVERSE_VIDEO;
  }
    new->flags &= ~REVERSE_VIDEO;

  new->screen.inhibit = request->screen.inhibit;
  if (new->misc.logInhibit = request->misc.logInhibit)
    screen->inhibit |= I_LOG;
  else
    screen->inhibit &= ~I_LOG;
  if (new->misc.signalInhibit = request->misc.signalInhibit)
    screen->inhibit |= I_SIGNAL;
  else
    screen->inhibit &= ~I_SIGNAL;

  new->initflags = new->flags;

  if (screen->mousecolor != reqscr->mousecolor) {
    screen->mousecolor = reqscr->mousecolor;
    retval = True;
  }

  if (screen->pointer_cursor != reqscr->pointer_cursor) {
    screen->pointer_cursor = reqscr->pointer_cursor;
    retval = True;
  }

  if (screen->mousecolorback != reqscr->mousecolorback) {
    screen->mousecolorback = reqscr->mousecolorback;
    retval = True;
  }

  if (screen->foreground != request->manager.foreground) {
    screen->foreground = request->manager.foreground;
    retval = True;
  }

  if (screen->cursorcolor != reqscr->cursorcolor) {
    screen->cursorcolor = reqscr->cursorcolor;
    retval = True;
  }

  if (request->misc.scrollbar) {
    if ((!current->misc.scrollbar) &&
	(!screen->scrollWidget) &&
	(!XtIsRealized(current))) {
      ScrollBarOn(new, TRUE, FALSE);
    }
    if ((screen->scrollWidget) &&
	(new->misc.scrollbar != request->misc.scrollbar)) {
      new->misc.scrollbar = request->misc.scrollbar;
      retval = True;
    }
  } else if (screen->scrollbar) {
    screen->scrollbar = 0;
    retval = True;
  }
  if (screen->scrollWidget) {
    /* Only allow changes to 'savelines' if their is a scrollWidget */
    if (screen->savelines != reqscr->savelines) {
      Reallocate(&screen->allbuf, (Char **)&screen->sbuf_address,
		 screen->max_row + 1 + reqscr->savelines,
		 screen->max_col + 1, screen->max_row + 1 +
		 screen->savelines, screen->max_col + 1, 0);
      screen->savelines = reqscr->savelines;
      screen->buf = &screen->allbuf[4 * screen->savelines];
    }
  }

  if (((current->misc.geo_metry) &&
       (!request->misc.geo_metry)) ||
      ((request->misc.geo_metry) &&
       (!current->misc.geo_metry)) ||
      ((current->misc.geo_metry) &&
       (request->misc.geo_metry) &&
       (strcmp(current->misc.geo_metry,request->misc.geo_metry))))
    retval = True;
  new->misc.geo_metry = request->misc.geo_metry;

  if (reqscr->logging != screen->logging) {
    if (reqscr->logging)
      StartLog(screen);
    else
      CloseLog(screen);
  }
  return retval;
}

static void VTDestroy (w)
     Widget w;
{
  XtermWidget term = (XtermWidget) w;
  TScreen *screen = &term->screen;
  ScrnColors  *pOldColors = screen->pOldColors;
  int i;
  XtFree(screen->selection);
  if (pOldColors) {
    for (i=0;i<NCOLORS;i++)
      if (pOldColors->names[i]) {
	XtFree(pOldColors->names[i]);
      }
    XtFree((char *)pOldColors);
  }
  VTfreebufs(term);
  if (screen->box) XtFree((char *)screen->box);
  if (screen->logfile) XtFree(screen->logfile);
  /* COMMENTED out the following due to errors...!!!  
    if (screen->arrow) XFreeCursor(screen->display,screen->arrow);
  if (screen->pointer_cursor) 
    XFreeCursor(screen->display,screen->pointer_cursor);
    */
  if (screen->selection_atoms) XtFree((char *)screen->selection_atoms);
  /* Required the followin due to problems after destroying a child */
  XtRemoveEventHandler(w, EnterWindowMask, FALSE, HandleEnterWindow, NULL);
  XtRemoveEventHandler(w, LeaveWindowMask, FALSE, HandleLeaveWindow, NULL);
  XtRemoveEventHandler(w, FocusChangeMask, FALSE, HandleFocusChange, NULL);
  XtRemoveEventHandler(w, 0L, TRUE, VTNonMaskableEvent, (char *)w);


/* Free the GC's and Fonts...Memory Leak...krw */
  if (screen->normalGC)
    {
    XFreeGC (screen->display, screen->normalGC);
    if (screen->normalGC != screen->normalboldGC)
        XFreeGC (screen->display, screen->normalboldGC);
    }
  if (screen->reverseGC)
    {
    XFreeGC (screen->display, screen->reverseGC);
    if (screen->reverseGC != screen->reverseboldGC)
        XFreeGC (screen->display, screen->reverseboldGC);
    }
  if (screen->fnt_norm) 
    {
    XFreeFont (screen->display, screen->fnt_norm);
    if (screen->fnt_norm != screen->fnt_bold) 
	XFreeFont (screen->display, screen->fnt_bold);
    }
}

void GetTextDimensions(w,width,height,xstart,ystart)
     XtermWidget w;
     unsigned int *width;
     unsigned int *height;
     unsigned int *xstart;
     unsigned int *ystart;
{
  /* use window dimensions, and return a width, height in character */
  /* cells, and an xstart and ystart in pixels as offset from the */
  /* start of the window */
  int wid,hei,xs,ys,j;
  int scrollbar_width,bbST;
  TScreen *screen = &w->screen;
  bbST = screen->parent->manager.shadow_thickness;
  scrollbar_width = (w->misc.scrollbar ?
		     screen->scrollWidget->core.width +
		     screen->scrollWidget->core.border_width : 0);
  j = 2 * (screen->border + bbST);
  xs = j + scrollbar_width;
  ys = j;
  wid = w->core.width - xs;
  hei = w->core.height - ys;
  *width  = wid / FontWidth(screen);
  *height = hei / FontHeight(screen);
  if (xstart) *xstart = xs;
  if (ystart) *ystart = ys;
}

/*ARGSUSED*/
static void VTRealize (w, valuemask, values)
     Widget w;
     XtValueMask *valuemask;
     XSetWindowAttributes *values;
{
  unsigned int width, height;
  XtermWidget term=(XtermWidget)w;
  register TScreen *screen = &term->screen;
  int xpos, ypos, pr;
  extern char *XtMalloc();
  XmManagerClassRec *classRec;

  TabReset (term->tabs);

  screen->fnt_norm = screen->fnt_bold = NULL;
  if (!LoadNewFont(screen, term->misc.f_n, term->misc.f_b, False, term)) {
    fprintf (stderr,
	     "Xterm:  unable to open font \"%s\", trying \"fixed\"....\n",
	     term->misc.f_n);
    (void) LoadNewFont (screen, "fixed", NULL, False, term);
  }

  /* really screwed if we couldn't open default font */
  if (!screen->fnt_norm) {
    fprintf (stderr, "Xterm:  unable to locate a suitable font\n");
    exit (1);
  }

  /* making cursor (was originally using XC_xterm) */
  if (!screen->pointer_cursor)
    screen->pointer_cursor = make_colored_cursor
      (XC_left_ptr, screen->mousecolor, screen->mousecolorback,term);
  else
    recolor_cursor (term, screen->pointer_cursor,
		    screen->mousecolor, screen->mousecolorback);

  /* set defaults */
  xpos = term->core.x; ypos = term->core.y; 
  GetTextDimensions(term,&width,&height,NULL,NULL);
  pr = XParseGeometry (term->misc.geo_metry, &xpos, &ypos,
		       &width, &height);
  if ((pr & XValue) && (XNegative&pr)) 
    xpos += term->core.parent->core.width - width;
  if ((pr & YValue) && (YNegative&pr))
    ypos += term->core.parent->core.height - height;
  screen->max_col = (width - 1);  /* units in character cells */
  screen->max_row = (height - 1);  /* units in character cells */
  update_font_info (&term->screen, False);

  width = screen->fullVwin.fullwidth;
  height = screen->fullVwin.fullheight;

  values->bit_gravity = NorthWestGravity;
  /* use the Manager class realize proc..., This replaces the call to */
  /* XCreateWindow... */
  
  classRec = (XmManagerClassRec *)&xmManagerClassRec;
  (*classRec->core_class.realize) (w, valuemask, values);

  term->screen.fullVwin.window = term->core.window;

  set_cursor_gcs (screen);

  /* Reset variables used by ANSI emulation. */

  screen->gsets[0] = 'B';	/* ASCII_G */
  screen->gsets[1] = 'B';
  screen->gsets[2] = 'B';	/* DEC supplemental. */
  screen->gsets[3] = 'B';
  screen->curgl = 0;		/* G0 => GL.	 */
  screen->curgr = 2;		/* G2 => GR.	 */
  screen->curss = 0;		/* 'No single shift. */

  XDefineCursor(screen->display, TextWindow(screen), screen->pointer_cursor);

  screen->cur_col = screen->cur_row = 0;
  screen->max_col = Width(screen)/screen->fullVwin.f_width - 1;
  screen->top_marg = 0;
  screen->bot_marg = screen->max_row = Height(screen) /
    screen->fullVwin.f_height - 1;

  screen->sc.row = screen->sc.col = screen->sc.flags = NULL;

  /* Mark screen buffer as unallocated.	 We wait until the run loop so
     that the child process does not fork and exec with all the dynamic
     memory it will never use.	If we were to do it here, the
     swap space for new process would be huge for huge savelines. */
  screen->buf = screen->allbuf = NULL;

  screen->do_wrap = 0;
  screen->scrolls = screen->incopy = 0;
  set_vt_box (screen);

  screen->savedlines = 0;

  if (term->misc.scrollbar) {
    screen->scrollbar = 0;
    ScrollBarOn (term, FALSE, TRUE);
  }

  /* Following line required due to bug in widget */
  if (!screen->allbuf) VTallocbuf (term);

  CursorSave (term, &screen->sc);
  return;
}

/*
 * Shows cursor at new cursor position in screen.
 */
ShowCursor(screen)
     register TScreen *screen; /*  = &term->screen; */
{
  register int x, y, flags;
  Char c;
  GC  currentGC;
  Boolean  in_selection;

  if (screen->eventMode != NORMAL) return;

  if (screen->cur_row - screen->topline > screen->max_row)
    return;
  c = screen->buf[y = 4 * (screen->cursor_row = screen->cur_row)]
    [x = screen->cursor_col = screen->cur_col];
  flags = screen->buf[y + 1][x];
  if (c == 0)
    c = ' ';

  if (screen->cur_row > screen->endHRow ||
      (screen->cur_row == screen->endHRow &&
       screen->cur_col >= screen->endHCol) ||
      screen->cur_row < screen->startHRow ||
      (screen->cur_row == screen->startHRow &&
       screen->cur_col < screen->startHCol))
    in_selection = False;
  else
    in_selection = True;

  if(screen->select || screen->always_highlight) {
    if (( (flags & INVERSE) && !in_selection) ||
	(!(flags & INVERSE) &&	in_selection)){
      /* text is reverse video */
      if (screen->cursorGC) {
	currentGC = screen->cursorGC;
      } else {
	if (flags & BOLD) {
	  currentGC = screen->normalboldGC;
	} else {
	  currentGC = screen->normalGC;
	}
      }
    } else { /* normal video */
      if (screen->reversecursorGC) {
	currentGC = screen->reversecursorGC;
      } else {
	if (flags & BOLD) {
	  currentGC = screen->reverseboldGC;
	} else {
	  currentGC = screen->reverseGC;
	}
      }
    }
  } else { /* not selected */
    if (( (flags & INVERSE) && !in_selection) ||
	(!(flags & INVERSE) &&	in_selection)) {
      /* text is reverse video */
      currentGC = screen->reverseGC;
    } else { /* normal video */
      currentGC = screen->normalGC;
    }

  }

  x = CursorX (screen, screen->cur_col);
  y = CursorY(screen, screen->cur_row) +
    screen->fnt_norm->ascent;
  XDrawImageString(screen->display, TextWindow(screen), currentGC,
		   x, y, (char *) &c, 1);

  if((flags & BOLD) && screen->enbolden) /* no bold font */
    XDrawString(screen->display, TextWindow(screen), currentGC,
		x + 1, y, (char *) &c, 1);
  if(flags & UNDERLINE)
    XDrawLine(screen->display, TextWindow(screen), currentGC,
	      x, y+1, x + FontWidth(screen), y+1);
  if (!screen->select && !screen->always_highlight) {
    screen->box->x = x;
    screen->box->y = y - screen->fnt_norm->ascent;
    XDrawLines (screen->display, TextWindow(screen),
		screen->cursoroutlineGC ? screen->cursoroutlineGC
		: currentGC,
		screen->box, NBOX, CoordModePrevious);
  }
  screen->cursor_state = ON;
}

/*
 * hide cursor at previous cursor position in screen.
 */
HideCursor(screen)
     register TScreen *screen; /*  = &term->screen; */
{
  GC  currentGC;
  register int x, y, flags;
  char c;
  Boolean  in_selection;

  if(screen->cursor_row - screen->topline > screen->max_row)
    return;
  c = screen->buf[y = 4 * screen->cursor_row][x = screen->cursor_col];
  flags = screen->buf[y + 1][x];

  if (screen->cursor_row > screen->endHRow ||
      (screen->cursor_row == screen->endHRow &&
       screen->cursor_col >= screen->endHCol) ||
      screen->cursor_row < screen->startHRow ||
      (screen->cursor_row == screen->startHRow &&
       screen->cursor_col < screen->startHCol))
    in_selection = False;
  else
    in_selection = True;

  if (( (flags & INVERSE) && !in_selection) ||
      (!(flags & INVERSE) &&  in_selection)) {
    if(flags & BOLD) {
      currentGC = screen->reverseboldGC;
    } else {
      currentGC = screen->reverseGC;
    }
  } else {
    if(flags & BOLD) {
      currentGC = screen->normalboldGC;
    } else {
      currentGC = screen->normalGC;
    }
  }

  if (c == 0)
    c = ' ';
  x = CursorX (screen, screen->cursor_col);
  y = (((screen->cursor_row - screen->topline) * FontHeight(screen))) +
    screen->border + SHAD;
  y = y+screen->fnt_norm->ascent;
  XDrawImageString(screen->display, TextWindow(screen), currentGC,
		   x, y, &c, 1);
  if((flags & BOLD) && screen->enbolden)
    XDrawString(screen->display, TextWindow(screen), currentGC,
		x + 1, y, &c, 1);
  if(flags & UNDERLINE)
    XDrawLine(screen->display, TextWindow(screen), currentGC,
	      x, y+1, x + FontWidth(screen), y+1);
  screen->cursor_state = OFF;
}

VTReset(full,term)
     int full;
     XtermWidget term;
{
  register TScreen *screen = &term->screen;

  /* reset scrolling region */
  screen->top_marg = 0;
  screen->bot_marg = screen->max_row;
  term->flags &= ~ORIGIN;
  if(full) {
    TabReset (term->tabs);
    term->keyboard.flags = NULL;
    screen->gsets[0] = 'B';
    screen->gsets[1] = 'B';
    screen->gsets[2] = 'B';
    screen->gsets[3] = 'B';
    screen->curgl = 0;
    screen->curgr = 2;
    screen->curss = 0;
    ClearScreen(screen);
    screen->cursor_state = OFF;
    if (term->flags & REVERSE_VIDEO)
      ReverseVideo(term);

    term->flags = term->initflags;
    if(screen->c132 && (term->flags & IN132COLUMNS)) {
      Dimension junk;
      XtMakeResizeRequest(
			  (Widget) term,
			  (Dimension) 80*FontWidth(screen)
			  + 2 * screen->border + SHAD + screen->scrollbar,
			  (Dimension) FontHeight(screen)
			  * (screen->max_row + 1) + 2 * screen->border + SHAD,
			  &junk, &junk);
      XSync(screen->display, FALSE);  /* synchronize */
      if(QLength(screen->display) > 0)
	xevents(term);
    }
    CursorSet(screen, 0, 0, term->flags);
  }
  /* force ground state in parser */
  /* longjmp(vtjmpbuf, 1);  DANGEROUS statement!!*/
}



/*
 * set_character_class - takes a string of the form
 *
 *		   low[-high]:val[,low[-high]:val[...]]
 *
 * and sets the indicated ranges to the indicated values.
 */

int set_character_class (s)
     register char *s;
{
  register int i;		/* iterator, index into s */
  int len;			/* length of s */
  int acc;			/* accumulator */
  int low, high;		/* bounds of range [0..127] */
  int base;			/* 8, 10, 16 (octal, decimal, hex) */
  int numbers;			/* count of numbers per range */
  int digits;			/* count of digits in a number */
  static char *errfmt = "Xterm:  %s in range string \"%s\" (position %d)\n";

  if (!s || !s[0]) return -1;

  base = 10;			/* in case we ever add octal, hex */
  low = high = -1;		/* out of range */

  for (i = 0, len = strlen (s), acc = 0, numbers = digits = 0;
       i < len; i++) {
    char c = s[i];

    if (isspace(c)) {
      continue;
    } else if (isdigit(c)) {
      acc = acc * base + (c - '0');
      digits++;
      continue;
    } else if (c == '-') {
      low = acc;
      acc = 0;
      if (digits == 0) {
	fprintf (stderr, errfmt, "missing number", s, i);
	return (-1);
      }
      digits = 0;
      numbers++;
      continue;
    } else if (c == ':') {
      if (numbers == 0)
	low = acc;
      else if (numbers == 1)
	high = acc;
      else {
	fprintf (stderr, errfmt, "too many numbers",
		 s, i);
	return (-1);
      }
      digits = 0;
      numbers++;
      acc = 0;
      continue;
    } else if (c == ',') {
      /*
       * now, process it
 */

      if (high < 0) {
	high = low;
	numbers++;
      }
      if (numbers != 2) {
	fprintf (stderr, errfmt, "bad value number",
		 s, i);
      } else if (SetCharacterClassRange (low, high, acc) != 0) {
	fprintf (stderr, errfmt, "bad range", s, i);
      }

      low = high = -1;
      acc = 0;
      digits = 0;
      numbers = 0;
      continue;
    } else {
      fprintf (stderr, errfmt, "bad character", s, i);
      return (-1);
    }	     /* end if else if ... else */

  }

  if (low < 0 && high < 0) return (0);

  /*
   * now, process it
 */

  if (high < 0) high = low;
  if (numbers < 1 || numbers > 2) {
    fprintf (stderr, errfmt, "bad value number", s, i);
  } else if (SetCharacterClassRange (low, high, acc) != 0) {
    fprintf (stderr, errfmt, "bad range", s, i);
  }

  return (0);
}

/* ARGSUSED */
static void HandleKeymapChange(w, event, params, param_count)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *param_count;
{
  static XtTranslations keymap, original;
  static XtResource resources[] = {
    { XmNtranslations, XmCTranslations, XmRTranslationTable,
	sizeof(XtTranslations), 0, XmRTranslationTable, (caddr_t)NULL}
  };
  char mapName[1000];
  char mapClass[1000];

  if (*param_count != 1) return;

  if (original == NULL) original = w->core.tm.translations;

  if (strcmp(params[0], "None") == 0) {
    XtOverrideTranslations(w, original);
    return;
  }
  (void) sprintf( mapName, "%sKeymap", params[0] );
  (void) strcpy( mapClass, mapName );
  if (islower(mapClass[0])) mapClass[0] = toupper(mapClass[0]);
  XtGetSubresources( w, (XtPointer)&keymap, mapName, mapClass,
		    resources, (Cardinal)1, NULL, (Cardinal)0 );
  if (keymap != NULL)
    XtOverrideTranslations(w, keymap);
}


/* ARGSUSED */
static void HandleBell(w, event, params, param_count)
     Widget w;
     XEvent *event;		/* unused */
     String *params;		/* [0] = volume */
     Cardinal *param_count;	/* 0 or 1 */
{
  int percent = (*param_count) ? atoi(params[0]) : 0;

  XBell( XtDisplay(w), percent );
}


/* ARGSUSED */
static void HandleIgnore(w, event, params, param_count)
     Widget w;
     XEvent *event;		/* unused */
     String *params;		/* unused */
     Cardinal *param_count;	/* unused */
{
  /* do nothing, but check for funny escape sequences */
  (void) SendMousePosition(w, event);
}

int LoadNewFont (screen, nfontname, bfontname, doresize, term)
     TScreen *screen;
     char *nfontname, *bfontname;
     Bool doresize;
     XtermWidget term;
{
  XFontStruct *nfs = NULL, *bfs = NULL;
  XGCValues xgcv;
  unsigned long mask;
  GC new_normalGC = NULL, new_normalboldGC = NULL;
  GC new_reverseGC = NULL, new_reverseboldGC = NULL;
  char *tmpname = NULL;

  if (!nfontname) return 0;

  if (!(nfs = XLoadQueryFont (screen->display, nfontname))) goto bad;

  if (!(bfontname &&
	(bfs = XLoadQueryFont (screen->display, bfontname))))
    bfs = nfs;

  mask = (GCFont | GCForeground | GCBackground | GCGraphicsExposures |
	  GCFunction);

  xgcv.font = nfs->fid;
  xgcv.foreground = screen->foreground;
  xgcv.background = term->core.background_pixel;
  xgcv.graphics_exposures = TRUE;  /* default */
  xgcv.function = GXcopy;

  new_normalGC = XCreateGC(screen->display,
			   DefaultRootWindow(screen->display), mask, &xgcv);
  if (!new_normalGC) goto bad;

  if (nfs == bfs) {	 /* there is no bold font */
    new_normalboldGC = new_normalGC;
  } else {
    xgcv.font = bfs->fid;
    new_normalboldGC = XCreateGC(screen->display,
				 DefaultRootWindow(screen->display),
				 mask, &xgcv);
    if (!new_normalboldGC) goto bad;
  }

  xgcv.font = nfs->fid;
  xgcv.foreground = term->core.background_pixel;
  xgcv.background = screen->foreground;
  new_reverseGC = XCreateGC(screen->display,
			    DefaultRootWindow(screen->display), mask, &xgcv);
  if (!new_reverseGC) goto bad;

  if (nfs == bfs) {	 /* there is no bold font */
    new_reverseboldGC = new_reverseGC;
  } else {
    xgcv.font = bfs->fid;
    new_reverseboldGC = XCreateGC(screen->display,
				  DefaultRootWindow(screen->display),
				  mask, &xgcv);
    if (!new_reverseboldGC) goto bad;
  }

  if (screen->normalGC)
    XFreeGC (screen->display, screen->normalGC);
  if (screen->normalGC != screen->normalboldGC)
    XFreeGC (screen->display, screen->normalboldGC);
  if (screen->reverseGC)
    XFreeGC (screen->display, screen->reverseGC);
  if (screen->reverseGC != screen->reverseboldGC)
    XFreeGC (screen->display, screen->reverseboldGC);
  screen->normalGC = new_normalGC;
  screen->normalboldGC = new_normalboldGC;
  screen->reverseGC = new_reverseGC;
  screen->reverseboldGC = new_reverseboldGC;
  screen->fnt_norm = nfs;
  screen->fnt_bold = bfs;			/* why these two lines?.krw */
/*  screen->fnt_bold = screen->fnt_norm; */ 	/* commented out, mem leak krw*/
  screen->enbolden = (nfs == bfs);
  set_cursor_gcs (screen);
  update_font_info (screen, doresize);
  return 1;

 bad:
  if (tmpname) XtFree (tmpname);
  if (new_normalGC)
    XFreeGC (screen->display, screen->normalGC);
  if (new_normalGC && new_normalGC != new_normalboldGC)
    XFreeGC (screen->display, new_normalboldGC);
  if (new_reverseGC)
    XFreeGC (screen->display, new_reverseGC);
  if (new_reverseGC && new_reverseGC != new_reverseboldGC)
    XFreeGC (screen->display, new_reverseboldGC);
  if (nfs) XFreeFont (screen->display, nfs);
  if (nfs && nfs != bfs) XFreeFont (screen->display, bfs);
  return 0;
}


update_font_info (screen, doresize)
     TScreen *screen;
     Bool doresize;
{
  XtermWidget term = screen->parent;
  int i, j, width, height, scrollbar_width, tmp, bbST;

  bbST = screen->parent->manager.shadow_thickness;

  screen->fullVwin.f_width = screen->fnt_norm->max_bounds.width;
  screen->fullVwin.f_height = (screen->fnt_norm->ascent +
			       screen->fnt_norm->descent);
  scrollbar_width = (term->misc.scrollbar ?
		     screen->scrollWidget->core.width +
		     screen->scrollWidget->core.border_width : 0);
  tmp = 2 * (screen->border + bbST);
  i = tmp + scrollbar_width;
  j = tmp;
  width = (screen->max_col + 1) * screen->fullVwin.f_width + i;
  height = (screen->max_row + 1) * screen->fullVwin.f_height + j;
  screen->fullVwin.fullwidth = width;
  screen->fullVwin.fullheight = height;
  screen->fullVwin.width = width - i;
  screen->fullVwin.height = height - j;

  if (doresize) {
    if (VWindow(screen)) {
      XClearWindow (screen->display, VWindow(screen));
    }
    DoResizeScreen (term);    /* set to the new natural size */
    if (screen->scrollWidget)
      ResizeScrollBar (screen->scrollWidget, bbST, bbST,
		       (unsigned)(Height(screen) + (screen->border + SHAD) * 2));
    Redraw (term);
  }
  set_vt_box (screen);
}

set_vt_box (screen)
     TScreen *screen;
{
  XPoint  *VTbox;
  XPoint  *vp;

  VTbox = (XPoint *)XtMalloc(NBOX * sizeof(XPoint));
  vp = &VTbox[1];
  (vp++)->x = FontWidth(screen) - 1;
  (vp++)->y = FontHeight(screen) - 1;
  (vp++)->x = -(FontWidth(screen) - 1);
  vp->y = -(FontHeight(screen) - 1);
  if (screen->box) XtFree((char *)screen->box);
  screen->box = VTbox;
}


set_cursor_gcs (screen)
     TScreen *screen;
{
  XtermWidget term = screen->parent;
  XGCValues xgcv;
  unsigned long mask;
  unsigned long cc = screen->cursorcolor;
  unsigned long fg = screen->foreground;
  unsigned long bg = term->core.background_pixel;
  GC new_cursorGC = NULL, new_reversecursorGC = NULL;
  GC new_cursoroutlineGC = NULL;

  /*
   * Let's see, there are three things that have "color":
   *
   *	 background
   *	 text
   *	 cursorblock
   *
   * And, there are four situation when drawing a cursor, if we decide
   * that we like have a solid block of cursor color with the letter
   * that it is highlighting shown in the background color to make it
   * stand out:
   *
   *	 selected window, normal video - background on cursor
   *	 selected window, reverse video - foreground on cursor
   *	 unselected window, normal video - foreground on background
   *	 unselected window, reverse video - background on foreground
   *
   * Since the last two are really just normalGC and reverseGC, we only
   * need two new GC's.	 Under monochrome, we get the same effect as
   * above by setting cursor color to foreground.
 */

  xgcv.font = screen->fnt_norm->fid;
  mask = (GCForeground | GCBackground | GCFont);
  if (cc != fg && cc != bg) {
    /* we have a colored cursor */
    xgcv.foreground = fg;
    xgcv.background = cc;
    new_cursorGC = XtGetGC ((Widget) term, mask, &xgcv);

    if (screen->always_highlight) {
      new_reversecursorGC = (GC) 0;
      new_cursoroutlineGC = (GC) 0;
    } else {
      xgcv.foreground = bg;
      xgcv.background = cc;
      new_reversecursorGC = XtGetGC ((Widget) term, mask, &xgcv);
      xgcv.foreground = cc;
      xgcv.background = bg;
      new_cursoroutlineGC = XtGetGC ((Widget) term, mask, &xgcv);
    }
  } else {
    new_cursorGC = (GC) 0;
    new_reversecursorGC = (GC) 0;
    new_cursoroutlineGC = (GC) 0;
  }
  if (screen->cursorGC) XtReleaseGC ((Widget)term, screen->cursorGC);
  if (screen->reversecursorGC)
    XtReleaseGC ((Widget)term, screen->reversecursorGC);
  if (screen->cursoroutlineGC)
    XtReleaseGC ((Widget)term, screen->cursoroutlineGC);
  screen->cursorGC = new_cursorGC;
  screen->reversecursorGC = new_reversecursorGC;
  screen->cursoroutlineGC = new_cursoroutlineGC;
}

/* Added the following 02/22/93 for being able to update a specific line ... */

/* XtermGetLoggerSelectedLineNum() Returns the line number of the user
   selected line as a positive value (0 being the line number of the oldest
   saved line) and a negative value (-1 being the most recently logged line, -2
   being the second most recent line, etc. going backward through the list of
   lines logged).

      This routine may also be used to get the total count of lines written
   by using:

	XtermGetLoggerSelectedLineNum(widget , &pos, &neg);
	count = pos - neg;

	*/
void XtermGetLoggerSelectedLineNum(wid, pos, neg)
     XtermWidget wid;
     int *pos, *neg;
{
  /* See the comment in 'scrollbar.c' in ScrollCallback() about savedlines etc.
     for how this numbering works... */
  TScreen *screen = &wid->screen;
  int mr;
  if (pos) *pos = screen->selectedRow;
  if (neg) {
    if (screen->topline) mr = screen->max_row;
    else  mr = screen->cur_row;
    *neg = screen->selectedRow - (screen->savedlines + mr);
  }
}

/* XtermGetLoggerLine() Updates the string passed in as 'ret_string'  (caller
   provides enough space to write a single line of characters) with the line
   identified as 'line_number' (may be positive or negative according to rules
   specified in previous routine) */
int XtermGetLoggerLine(wid, lineNumber, retString)
     XtermWidget wid;
     int lineNumber;
     char *retString;
{
  TScreen *screen = &wid->screen;
  int s_t_row;			/* SaveText() row. */
  int mr;
  char *SaveText();

  /* If topline is zero, then no lines appear below the bottom of the screen,
     so we need not add 'max_row' to get the total... */
  if (screen->topline) mr = screen->max_row;
  else mr = screen->cur_row;

  if (lineNumber < 0) {
    /* convert lineNumber to a positive number....(see previous routine) */
    lineNumber = lineNumber + screen->savedlines + mr;
  }

  /* At this point, we have a positive line number, and we want to get the text
     at that location .... */
  /* Check to see that it is not too big (or small) */
  if ((lineNumber < 0) || (lineNumber > (screen->savedlines + mr))) {
    return 0;			/* failure... */
  }

  /* First, we must calculate a row in the terms that the routine SaveText()
     expects to see things in... */
  s_t_row = lineNumber - (screen->savedlines + screen->topline);

  retString = SaveText(screen, s_t_row, 0, screen->max_col, retString, &mr);
  *retString = 0;		/* null terminate */

  return 1;			/* success */

}

/* XtermSetLoggerLine() writes out 'string' beginning at the line identified as
   'lineNumber' (may be positive or negative as specified above) */
int XtermSetLoggerLine(wid, lineNumber, string, flag)
     XtermWidget wid;
     int   lineNumber;
     char *string;
     int   flag;		/* future use ONLY */
{
  TScreen *screen = &wid->screen;
  int s_c_row;			/* saved cur_row */
  int s_c_col;			/* saved cur_col */
  int s_tty;			/* saved scrollttyoutput (prevent scrolling) */
  int mr;

  /* If topline is zero, then no lines appear below the bottom of the screen,
     so we need not add 'max_row' to get the total... */
  if (screen->topline) mr = screen->max_row;
  else mr = screen->cur_row;

  if (lineNumber < 0) {
    /* convert lineNumber to a positive number....(see previous routine) */
    lineNumber = lineNumber + screen->savedlines + mr;
  }

  /* At this point, we have a positive line number, and we want to set the text
     at that location .... */
  /* Check to see that it is not too big (or small) */
  if ((lineNumber < 0) || (lineNumber > (screen->savedlines + mr))) {
    return 0;			/* failure... */
  }

  /* Here, we want to set things such that the cursor is at the line marked out
     by position 'lineNumber'.   But, before we do this, we want to save
     everything as is --- to be able to come back to this location */

  s_c_row = screen->cur_row;
  s_c_col = screen->cur_col;
  s_tty = screen->scrollttyoutput;

  /* Calculate the new 'cur_row', so that we will be able to output text FROM
     this row.... */
  screen->cur_row = lineNumber - screen->savedlines;
  screen->cur_col = 0;
  screen->scrollttyoutput = 0;	/* DON'T scroll text */

  /* Output text (with escape sequences or whatever) as if from the row
     desired... */
  XtermOutText(wid, string, strlen(string));

  /* Set things back (if going back would not be going UP) */
  screen->cur_row = (mr = (screen->cur_row >= s_c_row)) ? 
    screen->cur_row : s_c_row;
  /* If we passed the formerly "current" row, then keep the column the same. */
  screen->cur_col = mr ? screen->cur_col : 0;
  screen->scrollttyoutput = s_tty; /* restore value... */

  return 1;

}
/***********************************************************************/
/* End of inserts dated 02/22/93 for updating single line in scroller  */
