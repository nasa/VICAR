/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/
static char wptcalls_ccVersion[] = "%W% %G% %U%";
//
// C++ functions, implementing WPT interface with an application
//
// Change Log:
//
// 06-apr-89	Initial Release...dm
// 10-apr-89	Implement proper Wpt_CloseItem()...dm
// 27-apr-89	Convert to XtR2 (X11R3) version, new Display argument to
//		WptPanel() call...dm
// 11-may-89	Check for opened display in Wpt_PrintMessage()...dm
// 15-may-89	Create default Wait cursor in agentRt.c...dm
// 22-may-89	Maintain a list of application panels...dm
// 16-jun-89	Initialize InterViews...krw
// 19-jun-89	Added cast for InputCallbackProc(); sprinkled in 'const'..ljn
// 20-jun-89	Add creation of scrolled subpanels in Wpt_NewPanel...dm
// 11-jul-89	Check for Nullabless in Wpt_MissingVal()...dm
// 13-jul-89	Bring Wpt_PanelTopWindow() here ...dm
// 16-jul-89 	Don't invoke Vm_ functions in Wpt_Setxx() calls...dm	
// 09-aug-89	Check for elapsed time for time out events, to avoid
//		timing out on a queued timer event (from output objects)...dm
// 28-aug-89	Include <stdio.h>...ljn
// 30-aug-89	Change Wpt_SetReal to  using TAEFLOAT rather than float...krw
//
/***************************************************************************/
//
// 06-sep-89	Conditional XtAppWaitForEvent for VMS implementation...dm
// 10-sep-89	VMS port...ljn
// 26-set-89	New Wpt_NextEvent architecture using Client message...dm
// 30-sep-89	Set WmReparent = TRUE for VMS
// 03-oct-89    Added modal window support...tpl
// 05-oct-89    Check the validity of panel id in ParmUpdate & ViewUpdate...tpl
// 16-oct-89	Audited each function with a panelid parm. Put valid check in...
//		krw
// 19-oct-89	Fixed bug in Wpt_SetNoValue, if statement wrong...krw
// 30-oct-89	Add Extra checks for possible Wpt Event in Wpt_Pending()...dm
// 08-nov-89    Check for null target and view and dummy in Wpt_NewPanel...tpl
// 10-nov-89	Check WindowEvents in Wpt_NextEvent() after XtAppNextEvent(), 
//		change CheckModelEvent() name to AllowedPanelEvent()...dm
// 17-nov-89    reparent wm=true for VMS only...tpl
// 15-dec-89    removed compilation warnings...tpl
// 19-dec-89	removed 'static' decl on GetPanelParmNoMsg()...ljn
// 25-jan-90	g++ 1.36 port...ljn
// 27-mar-90    Added VMS kludge for the deletion of a pulldown.
//              Added Wpt_XErrorHandler...krw, kbs
// 04-may-90  X11R4 conversion;
//            XtAppWaitForEvent() changed to WptAppWaitForEvent()...ljn
// 05-apr-90    merged in MOTIF changes...tpl
// 14-jun-90	Removed junk after #endif...ljn
// 17-jun-90	Provide caste in XtAppCreateShell() call...ljn
// 22-oct-90	Support for "fast" application panels...ljn
// 24-oct-90	Support various panel creation states...ljn
// 02-nov-90	Raise & Normalize MODAL panel on button, key press...cew
// 02-nov-90    Added Wpt_SetItemSensitivity, Wpt_SetConstraintSensitivity...tpl
// 07-nov-90    Add Wpt_SetPanelState(), Wpt_GetPanelState(); removed
//	 	#ifdef MOTIF's; modified cew's fix on 2-nov...ljn
// 09-nov-90	Changed WPT_ panel creation constants...ljn
// 11-nov-90	Wpt_PanelParmNoMsg() should finish panel if "fast" panel...ljn
// 12-nov-90	Wpt_GetPanelState() detects modal panels...ljn
// 26-nov-90	Wpt_SetPanelState() should do an XSync()...ljn
// 30-nov-90	PR599: Reset WM hints for initial state at map time...ljn
// 04-dec-90    pR627: Added WPT_CHILD in wpt_newpanel...tpl
// 05-dec-90    pr475: Replaced the macro VALID_PANEL with the function 
//	  	       CheckValidPanel...tpl
// 06-dec-90    minor fix to loop for above fix...krw
// 05-dec-90    Always do a GetWMHints() before SetWMHints()!...ljn
// 06-dec-90	PR505: changes for origin/size problem...krw
//		Also removed XwAppRegisterConverter refs...ln/krw
// 11-jan-90	Panel state for WPT_CHILD panels was incorrectly determined.krw
//		Also fixed problem with offset calcs for these panels...krw
// 14-jan-91    pr549 handle default button properly in nextevent and
//              appwaitforevent...tpl
// 24-jan-91    Allow iconic and child in wptnewpanel...tpl
// 01-feb-91    added special case for childpanel in get/set panelstate...tpl
// 08-feb-91	Minor Memory Leak in Wpt_NextEvent...krw
// 25-feb-91	HP port for 5.0: minor compilation warnings...ljn (also 18-mar)
// 06-mar-91    Use an slist for wpt events...tpl
// 12-mar-91    A slightly more intelligent Wpt_Init regarding 
//		reparenting wm...tpl
// 12-mar-91    PR474: Changed Wpt_ItemWindow so it uses GetPanelParmNoMsg...rt
// 12-mar-91    PR828: Added ConfigureNotify...tpl
// 15-mar-91    Use GetPanelParmNoMsg instead of GetPanelParm...tpl
// 18-mar-91	_wpt_entry() is no wpt_entry()...ljn
// 19-mar-91    pr659 check for visible and child...tpl
// 28-mar-91    copy event into wpt_event...tpl/kw
// 30-mar-91	Header files for VMS...ljn
// 19-apr-91    PR990 fixed timeout with widget timeout...tpl
// 19-apr-91    replace XScreenNumberofScreen with actual macro for VMS...tpl
// 14-may-91	For VMS, look for DECW$DISPLAY...ljn
// 06-jun-91    PR1022 In sensitivity calls, return and set with the correct
//		values...tpl
// 25-jun-91	Apollo: compiler complaint about unused var...ljn
// 03-jul-91    PR 1104 allow modal in Wpt_SetPanelState...tpl
// 18-jul-91	Add call to id_entry()...ljn
// 21-aug-91	Add Wpt_CloseDisplay()...ljn
// 23-aug-91    Add WaitPanel so that Wpt_BeginWait blocks events...tpl
// 27-sep-91	Remove #ifdef hpux for signals...ljn
// 11-nov-91	PR1223, PR1224 Removed call to id_entry()..put in to 
//		InterViews_Init()
//		Also Removed call to InterViews_Init().done by ddos.
//		Also Added ReparentNotify code to make FrameOffset more
//		efficient...krw
// 15-nov-91	PR1254 Change to FrameOffset call to correctly handle frameless 
//		panels..krw
// 06-jan-92	PR1257: Changed #include filenames to use <>...ljn
// 19-feb-92	Removed scrollpanel.h (Facelift)...kbs
// 06-mar-92	Changed static toplevel to GLOBAL _wptInitialWidget..dar
// 18-mar-92	Added non-blocking message boxes, Wpt_MessageNoBlock,
//		Wpt_RejectNoBlock...cew
// 30-mar-92	Multiple Display support. Make modal panel and _wptMessageList
//		per display.  Added FindWptDisplay(), Wpt_OpenDisplay(),
//		Wpt_PanelDisplayId.  Changed dummy argument to Display * for
//		Wpt_NewPanel.  Change Wpt_Pending(), Wpt_CloseDisplay().
//		Added screen arg to Wpt_PrintMessage().  Changed
//		AllowedPanelEvent() to WptAllowedPanelEvent to allow access
//   	        from the WorkBench; also changed it to always allow all events
//		that are not Button or Key related...cew
// 03-apr-92    Added Wpt_CCInit and use const TEXT as needed...tpl	
// 10-apr-92	Delete Painter, State, and WorldRep in Wpt_CloseDisplay...cew
// 14-apr-92	Comment classname interdependency with agentRt:DoICCCM...kbs
// 19-may-92	PR1427: Purify detected a problem in FindWptDisplay...krw
// 22-jul-92	PR1519: Label appropriate functions as UNSUPPORTED if not
//		intended for use by applications; added FUNCTION tag word
//		for Wpt_Finish, Wpt_CCInit, Wpt_OpenDisplay, Wpt_ItemWindow,
//		and Wpt_NewPanel...kbs
// 04-aug-92	PR1472: Call Wpt_EndWait when panel is erased to make sure
//		it has been removed from the wait list...krw
// 19-aug-92	PR435: New processing in support of WPT_UPDATE_READMASK flag,
//		new UpdInputCallbackProc...crb
// 01-oct-92	Entry points changed because of automatic generation of
//		static constructors...ljn/rt
// 14-oct-92	filio.h is nonportable.  Better to use ioctl.h.  ioctl
//		was not prototyped...rt
// 15-oct-92	PR1682: Wpt_CloseDisplay would cause a core dump when the 
//		painter	was being deleted. We removed the wptDisplay too
//		soon...krw
// 19-oct-92	ioctl() conflicts with g++ 1.x header...rt
// 11-mar-93	PR-1860: Remove call to XtCloseDisplay() for HP...swd
// 23-aug-93    PR2191 if panels were resized, pulldown menus, menubars and
//              option menus displayed their popups in the wrong location...krw
/***************************************************************************/
#include	<stdio.h>
#include	<stdlib.h>
#ifdef VMS
#include	<signal.h>
#include 	<Xatom.h>
#else
#   ifdef     UNIX_NEXT
#   include   <signal.h>
#   endif
#include      <X11/Xatom.h>
#endif

#include      <sys/ioctl.h>

#include      <wpteventP.h>
#include      <wptinc.inp>
#include      <widgetagent.h>
#include      <msgpanel.h>
#include      <slist.h>

#ifdef VMS
#define  HANDLER_TYPE  SIG_PF		// from InterViews 
#else
#define  HANDLER_TYPE  void
#endif
 
    GLOBAL  Display 	   *_defaultDisplay;
    GLOBAL  XtAppContext   _wptAppContext;	// single threaded
    GLOBAL  slist	   _appPanelList;	// List of application panels
    GLOBAL  slist	   _wptGenEventList;	// wpt event queue
    GLOBAL  slist          _waitingWidgetList;  // List of BeginWaited panels
    GLOBAL  int  	   WmReparent;		// Panels reparented by win_mgr?
    GLOBAL  int  	   Usingmwm;		// using mwm?
    GLOBAL  Widget	   _wptInitialWidget;	// used to be static toplevel, now needs to
						//be accessible to InitWbGlobal
    extern BOOL IsWorkBench; 
    GLOBAL  slist          _wptDisplayList;	// list of open displays
    GLOBAL  WptDisplay 	   *_defaultWptDisplay;

    extern "C"
    {
    void    zero_block( char *, TAEINT);
    void    wptentry();
#ifndef UNIX_SIGNAL
#ifdef UNIX
    int signal(...);
#endif
#endif
#ifdef VMS
    int  sys$clref(...);
#endif
    }

    extern  WptEventClass  *_wptGenEvent;
    extern  WptEvent wpt_Event;
    static unsigned long   timeOut = nil;	// timeout interval
    static  int		   fileSources = 0;	// number of file event source 
    static  Boolean 	   ignoreInterrupt;	// ignore interrupts - default 
    static  TEXT  *interruptName = "_interrupt";
    static  TEXT  *timeoutName = "_timeout";
    static  Window	   ClientMessageWindow;	// window where to send events 
    static  XtIntervalId   timeoutId=nil;
    static  Atom   TIMEOUT_MESSAGE, FILE_MESSAGE,
		   OPINT_MESSAGE;		// msg type of ClientMessages

    static WptItem  * GetPanelParm(Id thePanel, const TEXT *parmName);
    BOOL     CheckValidPanel ( WptPanel * panelId);
    CODE     GetPanelParmNoMsg(Id thePanel, const TEXT *parmName, 
		WptItem **itemId);

    static   void  InputCallbackProc(void *client_data, 
		int *source, XtInputId *eventId);	// WPT level callback
    static   void  UpdInputCallbackProc(void *client_data, 
		int *source, XtInputId *eventId);	// WPT level callback
    static   void  EnableInterrupt(Boolean flag); 
    // callback model for different types of WPT events
    static   void  TimeoutCallbackProc( void *clientData, XtIntervalId *id);
    static   void OpIntHandler();
    static   void  SendClientMessage(Atom, int, int);
    static   unsigned long * GetWMState(Display *dpy, Window window);


extern BOOL DDOREHEARSING;		// True if Wpt_Rehearse(ing)
extern void Rehearse_AddPanel(WptPanel *);
extern void Rehearse_RemovePanel(WptPanel *);
extern "C" 
{
void FrameOffset(Display *, Window, int *, int *);
Window XmuClientWindow(Display *, Window);
#ifndef sgi
#ifdef __GNUG__
int ioctl(int, int, void *);
#else
int ioctl(int, int, long *);
#endif
#endif
}


// Look for display in known list of open displays
// If it can't be found, NULL is returned
WptDisplay *FindWptDisplay (Display *display)
    {
    WptDisplay *wptdpy;
    WptDisplay *rtnWptDpy = NULL;
    slist_iterator dpy_iter(_wptDisplayList);
    while (wptdpy = (WptDisplay *) dpy_iter())
	{
	if (display == wptdpy->dpy)
	    {
	    rtnWptDpy = wptdpy; 
	    break;
	    }
	}
    return (rtnWptDpy);
    }


/**********************************************************************
 *
 *     Wpt_Init.  Initialize for WPT calls from an C application.
 *
 *      Wpt_Init initializes for communication with the X server.
 *      It also initializes the XToolkit with standard defaults.
 *
 *	Note that Wpt_Init() exits if it cannot open the display
 *
 ***********************************************************************/

    extern "C" FUNCTION Display  * Wpt_Init (const TEXT *displayName)
    {
    wptentry();                              // init Wpt static ctors
    return Wpt_CCInit ( displayName );
    }

/**********************************************************************
 *
 *     Wpt_CCInit.  Initialize for WPT calls from an C++ application.
 *
 *      Wpt_CCInit initializes for communication with the X server.
 *      It also initializes the XToolkit with standard defaults.
 *
 *      Note that Wpt_CCInit() exits if it cannot open the display
 *
 ***********************************************************************/
    extern "C" FUNCTION Display  * Wpt_CCInit (const TEXT *displayName)
    {
    Display  *display;
    TEXT     *argv[1];
    TEXT     *appClass;

    // TBD: This is left over from the old days.  What we really
    // should be doing is passing in argv and argc from the command
    // line. The thought is we could have a Wpt call that could be
    // called before Wpt_Init that sets argc, argv, and the class
    // name.
    int argc = 1; 
    argv[0] = "_Wpt";				// a dummy string    

    XtToolkitInitialize(); 
    _waitingWidgetList.clear();

    _wptAppContext = XtCreateApplicationContext();

    if ( IsWorkBench )
        appClass = "Taewb" ;
    else
	//      CAUTION: If you change this, change agentRt.cc:DoICCCM also!
        appClass = "TAEPlus" ;          // KLUDGE for now

    display = XtOpenDisplay(_wptAppContext, displayName, "main", appClass, 
		NULL, (Cardinal)0, (Cardinal *)&argc, argv);
    if (display == nil) 
  	{
#ifdef VMS
 	const TEXT *dispPtr = displayName ? displayName : getenv("DECW$DISPLAY");
#else
 	const TEXT *dispPtr = displayName ? displayName : getenv("DISPLAY");	
#endif
 	fprintf(stderr,	
	    "Cannot communicate with X server on display '%s'. Exiting.\n", 
			dispPtr);
	XtDestroyApplicationContext(_wptAppContext);
	exit(1);
	}


    _wptInitialWidget = XtAppCreateShell("Wpt", appClass, applicationShellWidgetClass,
			display, (ArgList) argv, argc);

    /* Create a (1,1) size unmapped window which is to be used as the
     * reference window for sending client message events later.
     */
    ClientMessageWindow = XCreateSimpleWindow(XtDisplay(_wptInitialWidget),
				DefaultRootWindow(XtDisplay(_wptInitialWidget)), 
				0, 0, 1, 1, 0, 1, 1);


//  Initialize global/static variables needed by WPT.
    _defaultDisplay = XtDisplay(_wptInitialWidget);
    _wptGenEvent = nil;
// This is done by ctor.
    _wptGenEventList.clear();    // initializes wpt event queue
//    _appPanelList.clear();
    ignoreInterrupt = TRUE;

    // initialize list of displays.
    _wptDisplayList.clear();
    WptDisplay *wptdpy = (WptDisplay *) tae_alloc (1, sizeof (WptDisplay));
    wptdpy->dpy = _defaultDisplay;
    wptdpy->modalWidgetId = nil;
    wptdpy->defWaitCursor = nil;
    wptdpy->wptMessageList.clear();
    _wptDisplayList.append(wptdpy);
    _defaultWptDisplay = wptdpy;

//
//  Is mwm running?
//
    if (  XmIsMotifWMRunning(_wptInitialWidget) )
	{
	Usingmwm = True;
	WmReparent = True;
	}
    else
	{
//
//	no mwm. Try to find out whether a reparenting manager is running
//	The scheme is to map the client window and try to find out 
//	whether this window has the property WM_STATE.
//      If there is no window manager running, WM_STATE will not be
//      available for that window.  User will see a tiny window on
// 	the upper left corner for a few seconds.
//
	int i;
	XSizeHints hints;

	Usingmwm = False;
        WmReparent = False;

        hints.x = 1;
        hints.y = 1;
        hints.flags = USPosition;
        XSetWMNormalHints ( display, ClientMessageWindow, &hints );
        XMapWindow(display, ClientMessageWindow);

//
//	the loop is one big kludge.  If we don't try many times, 
//	we will run into a timing problem.  For a window may not
//      be reparented yet when we ask for the property.  So we will
//	try a 100 time.  If there is still no property assume there
//	is no reparenting window manager.
//
        for ( i=0; i<100; i++ )
		{
		if ( GetWMState ( display, ClientMessageWindow) )
			{
		        WmReparent = True;
			break;
			}
		}
//
//	unmap to clean up the upper left corner
//
        XUnmapWindow(display, ClientMessageWindow);
	}
    return (_defaultDisplay);
    }



/**********************************************************************
 *
 *     Wpt_Finish.  Close all displays known by Wpt
 *
 ***********************************************************************/

    extern "C" FUNCTION CODE  Wpt_Finish ()

    {
    CODE code = SUCCESS;
    WptDisplay *wptdpy;
    slist_iterator *dpy_iter = new slist_iterator (_wptDisplayList);
    while (wptdpy = (WptDisplay *) (*dpy_iter)())
	{
	if (Wpt_CloseDisplay (wptdpy->dpy) == FAIL)
	    code = FAIL;
	}
    delete dpy_iter;
    return (code);
    }



/**********************************************************************
 *
 *     Wpt_OpenDisplay.  Open a display.
 *
 *      Wpt_OpenDisplay initializes for communication with an X server.
 *
 ***********************************************************************/

    extern "C" FUNCTION Display  * Wpt_OpenDisplay (TEXT *displayName)

    {
    Display  *display;
    TEXT     *argv[1];
    static   TEXT  *appClass;
    
    if ( IsWorkBench )
        appClass = "Taewb" ;
    else
	//      CAUTION: If you change this, change agentRt.cc:DoICCCM also!
        appClass = "TAEPlus" ;          // KLUDGE for now

    // TBD: change this.  See comments in Wpt_Init.
    int argc = 1; 
    argv[0] = "_Wpt";				// a dummy string    

    display = XtOpenDisplay(_wptAppContext, displayName, "main",
			    appClass, NULL, (Cardinal)0, 
			    (Cardinal *)&argc, argv);
    if (display == nil) 
  	{
#ifdef VMS
 	TEXT *dispPtr = displayName ? displayName : getenv("DECW$DISPLAY");
#else
 	TEXT *dispPtr = displayName ? displayName : getenv("DISPLAY");	
#endif
 	fprintf(stderr,	
	    "Cannot communicate with X server on display '%s'.\n", 
			dispPtr);
	}
    else
	{
	WptDisplay *wptdpy = (WptDisplay *) tae_alloc (1, sizeof(WptDisplay));
	wptdpy->dpy = display;
	wptdpy->modalWidgetId = nil;
	wptdpy->defWaitCursor = nil;
	wptdpy->wptMessageList.clear();
	_wptDisplayList.append(wptdpy);
	}

    return (display);
    }



/***********************************************************************/
// Create a new WPT panel

extern "C" FUNCTION Id Wpt_NewPanel(Display *display, Id targetVm, Id viewVm,
	Window parentWindow, void * userContext, int stateFlag)
    {
    int		 wptFlags = stateFlag;		// local copy 
    TEXT	 *name;				// panel name
    SymbolTable  *targetSt;
    SymbolTable  *viewSt;
    WptPanel     *panelId;
    Display	 *dpy = NULL;
    WptDisplay	 *wptdpy;

    // If display is "" or unknown, use the default display
    if ( (wptdpy = FindWptDisplay(display)) == NULL )
	{
	if (_defaultWptDisplay == NULL)
	    // the default display has been closed
	    return (nil);
	else
	    wptdpy = _defaultWptDisplay;
	}
    dpy = wptdpy->dpy;

    if (!(targetSt = (SymbolTable *) targetVm))
 	return(nil);
    if (!(viewSt = (SymbolTable *) viewVm))
 	return(nil);

    if (wptFlags)  
        // If wptFlags is non-null, use it in preference to .res file info.
	// But check for some bad conditions.
	{
	if (wptFlags != WPT_VISIBLE &&
	    wptFlags != WPT_SUBPANEL &&
	    wptFlags != WPT_CHILD &&
	    wptFlags != WPT_MODAL &&
	    wptFlags != WPT_INVISIBLE &&
	    wptFlags != WPT_ICONIC &&
	    wptFlags != WPT_FASTICONIC &&
	    wptFlags != (WPT_VISIBLE | WPT_CHILD) &&
	    wptFlags != (WPT_INVISIBLE | WPT_CHILD) )
	    {
	    printf 
	   ("Could not create panel. Invalid panel creation flag specified.\n");
            return(nil);
	    }
        else if ((wptFlags == WPT_MODAL) && wptdpy->modalWidgetId)
            // Only one modal panel is allowed. Return panelId set to nil.
	    {
	    printf ("Could not create modal panel on this display\n");
	    printf ("because another modal panel already exists.\n");
            return(nil);
	    }
	}
    else  	// use .res file info for panel creation state 
	{
	Symbol &state = (*viewSt)["_panel.state"];
	if (&state)		// only TAE 5.0 or later
	    {
	    if (s_equal (state.String(), "Visible"))
	        wptFlags = WPT_VISIBLE;
	    else if (s_equal (state.String(), "Invisible"))
	        wptFlags = WPT_INVISIBLE;
	    else if (s_equal (state.String(), "Visible & Modal"))
	    	{
     	    	if (!wptdpy->modalWidgetId)
	    	    wptFlags = WPT_MODAL;
	   	else
		   {
                   // Only one modal panel allowed. Return panelId set to nil.
	           printf("Couldn't create modal panel on this display\n");
		   printf ("because another modal panel already exists.\n");
        	   return(nil);
		   }
	        }
	    else if (s_equal (state.String(), "Iconic"))
	        wptFlags = WPT_ICONIC;
	    else if (s_equal (state.String(), "Fast Iconic"))
	        wptFlags = WPT_FASTICONIC;
	    else 
		{
 	        printf("Design Error: Unexpected value for symbol STATE: %s.\n",
						state.String()); 
                return(nil);
		}
	    }
	}
    
    name = "";
    Symbol *v = viewSt->GetSymbol("_panel.name");
    if (v && v->Count() )
	name = v->String();

    panelId = new WptPanel(name, (SymbolTable *) targetVm, 
		(SymbolTable *) viewVm, parentWindow, userContext, 
		wptFlags, dpy);

    if (panelId)			// panel successfully created
        {
	_appPanelList.insert(panelId);	// save in list
        if ( wptFlags == WPT_MODAL )
                wptdpy->modalWidgetId = panelId->WidgetHandle();
        }
    if (DDOREHEARSING)
	Rehearse_AddPanel(panelId); 
    return ( (Id) panelId);
    }


/***********************************************************************/

// Create an item on an existing wpt panel.
// Note: Currently to be used by WorkBench only  //

extern "C" UNSUPPORTED Id Wpt_NewItem(Id thePanel, const TEXT *name, 
                                      Id targetVm, Id viewVm)

    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))			// invalid panel Id
	return (nil);

    SymbolTable  *targetSt;
    SymbolTable  *viewSt;
    
    targetSt =  (SymbolTable *) targetVm; 
    viewSt =  (SymbolTable *) viewVm; 

    Symbol *target = targetSt->GetSymbol(name);	
    Symbol *view = viewSt->GetSymbol(name);
    if (!view)					// may not be in target
	return (nil);
    WptItem *itemId = ((WptPanel *)panelId)->AddNewItem(name,
		target, view);
    return ((Id) itemId);
    }

/*******************************************************************/

// Delete an item from a wpt panel.
// Note: Currently to be used by WorkBench only  //

extern "C" UNSUPPORTED int Wpt_ItemErase(Id thePanel, const TEXT *itemName)

    {

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, itemName, &itemId);

    if (code != SUCCESS)
        return (FAIL);


    WptPanel *panelId = (WptPanel *) thePanel;
    panelId->EraseItem(itemId);
    return (SUCCESS);
    }


/****************************************************************************
 *
 *	Wpt_SetTimeOut.  Set/Cancel timeout  for gathering Wpt events.
 *
 *      Note:  timeInterval is in milliseconds.
 *
 ***************************************************************************/

extern "C" FUNCTION  void Wpt_SetTimeOut (int timeInterval)

    {
    if (timeInterval != nil)
	timeOut = (unsigned long) timeInterval;		// add timeout
    else
	timeOut = nil;					// cancel timeout
    }

/****************************************************************************
 *
 *	Wpt_AddEvent.  Add other sources for input/output/exception.
 *
 *
 ***************************************************************************/

extern "C" FUNCTION  Id Wpt_AddEvent(int eventSource, int eventMask)

    {
    XtInputId   eventId;
    Opaque 	condition;
#ifdef VMS
    condition = NULL;			/* Force NULL iosb */
    eventId = XtAppAddInput(_wptAppContext, eventSource, condition,
        (XtInputCallbackProc) InputCallbackProc, (Opaque) eventMask);
#else
    condition = (Opaque) eventMask;
    if (condition == (Opaque) WPT_UPDATE_READMASK)
	{
        //
        // WPT_UPDATE_READMASK only generates a WPT_FILE event if *New*
        // input is written to the eventSource.  However, since this is handled
        // in UpdInputCallbackProc; we only need pass in WPT_READMASK here.
        //
	eventId = XtAppAddInput(_wptAppContext, eventSource,
	    (Opaque) WPT_READMASK, (XtInputCallbackProc) UpdInputCallbackProc,
	    (Opaque) eventMask);
	}
    else
	{
        eventId = XtAppAddInput(_wptAppContext, eventSource, condition,
	    (XtInputCallbackProc) InputCallbackProc, (Opaque) eventMask);
	}
#endif
    if (eventId)  fileSources++;
    return ((Id) eventId);
    }

/*************/

extern "C" FUNCTION  CODE Wpt_RemoveEvent(Id eventId)

    {
    if (eventId == NULL)
	return (FAIL);

    XtRemoveInput((XtInputId) eventId);
    fileSources--;
    return (SUCCESS);
    }

/*************/
    static FUNCTION  void  InputCallbackProc(void *client_data, 
		int *source, XtInputId *eventId)

    {
    int inputSource = *source;
    int inputMask = (int) client_data;
    eventId = eventId;

#ifdef VMS	

   /* Under VMS, here we must clear the input flag to avoid XtNextEvent()
    * to go into an infinite loop on the input source event. 
    */

/* Note: the CC compiler cannot handle defines with embedded '$' character */
#define   SYS_WASCLR 	1
#define   SYS_WASSET    9

    int stat = sys$clref(inputSource);
    if (stat != SYS_WASCLR && stat != SYS_WASSET)
	{
	printf("Could not clear Input event flag %d, status = %d\n",
	    inputSource, stat);
	}
#endif

    FILE_MESSAGE = XInternAtom (XtDisplay (_wptInitialWidget), "_file", 0);
    SendClientMessage(FILE_MESSAGE, inputSource, inputMask);  
    return;
    }

/*************/
    //
    // Called only under UNIX; only if WPT_UPDATE_READMASK
    //     
    static FUNCTION  void  UpdInputCallbackProc(void *client_data, 
		int *source, XtInputId *eventId)

    {
    int inputSource = *source;
    int inputMask = (int) client_data;
    eventId = eventId;

   /* Under UNIX with WPT_UPDATE_READMASK, we only generate a file event 
    * if there is something *New* to read.  Otherwise, the file will always
    * be READY for reading and needless events are generated.
    */

    long nbytes = 0;
    if (ioctl (inputSource, FIONREAD, &nbytes) != -1 && nbytes != 0)
        {
        FILE_MESSAGE = XInternAtom (XtDisplay (_wptInitialWidget), "_file", 0);
        SendClientMessage(FILE_MESSAGE, inputSource, inputMask);  
	}
    return;
    }
/***********************************************************************/

//  Enable/Disable interrupt event from user, during Wpt_NextEvent.
//  Currently it is used from TCL only.  Applications always ignore
//  interrupt.

    extern "C" UNSUPPORTED  void Wpt_EnableInterrupt(Boolean flag)

    {
    EnableInterrupt(flag);		// deligate to a static function
    }


static  FUNCTION  void EnableInterrupt(Boolean flag)
    {

// Compile the following code conditionally for UNIX. Resolve confict
// with InterView's signal.h.


#ifdef UNIX_SIGNAL 
    CODE		code; 
    struct	sigvec  isv; 
    struct	sigvec  osv;


    isv.sv_mask = 0;			/* no signal masking	   */
    isv.sv_onstack = 0;			/* no special stack	   */
    isv.sv_handler = flag ? OpIntHandler : SIG_DFL;

    CODE sigvec(...);
    code = sigvec(SIGINT, &isv, &osv);	  /* assign handler to signum */
#else
#ifdef UNIX
#define SIGINT  2
#define SIG_DFL (void (*) ())0
#endif
    if (flag)
#ifdef VMS
        signal(SIGINT, (SIG_PF)OpIntHandler);   // why??
#else
        signal(SIGINT, OpIntHandler);
#endif
    else
        signal(SIGINT, SIG_DFL);
#endif

    ignoreInterrupt = !flag;
    return;
#ifdef UNIX
#undef SIGINT
#undef SIG_DFL
#endif
    }

/****************************************************************************
 *
 *	Wpt_NextEvent.  Get the next event from any Wpt panel.
 *
 *	This function blocks until the next WPT event is available.
 *
 *	A Wpt Event implies either an X related event from a parm,
 *	or a file I/O event from a file descriptor registered via 
 *	XtAppAddEvent().
 *
 *	If a Wpt Event is already available (created by widget agents etc.
 *	due to some user interaction or timer), that event is returned 
 *	immediately. 
 *	Else, the function blocks until an X event is available
 *	that results in the firing of a widget's Dispatch function,
 *	(via XtDispatchEvent() call), which creates a Wpt event.
 *
 *	If the application has already provided a non-zero timer via 
 *	Wpt_SetTimeOut() function call,  Wpt_NextEvent()
 *      returns at the expiration of the timer, if prior to a parm event.
 *
 *      The time interval is in milliseconds.  
 *      Note: we copy timeOut interval to a local as WptAppWaitForEvent() 
 *	resets it to Zero in case of a real timeout.
 *
 ***************************************************************************/ 
 
    extern "C" FUNCTION  CODE Wpt_NextEvent(WptEvent *appEvent) 
    {

    XEvent	xEvent;
    int		eventType;
    unsigned long interval;
    unsigned long * intervalPtr;
    int		eventSource, eventMask;
    WptPanel 	*panelId;

    zero_block((char *)appEvent, sizeof(WptEvent)); 	// initialize
    interval = timeOut;				// If there is a timeout,..
    intervalPtr = timeOut ? &interval : NULL;

    while ( !_wptGenEventList.count() )			// No Wpt event waiting
	{
    	if (timeOut != nil || fileSources > 0 || !ignoreInterrupt) 
	    {
	    // Timeout or File i/o specified. Wait for next event or timeout.
	    eventType =  WptAppWaitForEvent(_wptAppContext, &xEvent, 
			intervalPtr, ignoreInterrupt, 
			&eventSource, &eventMask);	

	    if (eventType == -1 && !ignoreInterrupt)   // An operator interrupt
		{
		appEvent->eventType = WPT_INTERRUPT_EVENT;
		appEvent->parmName = interruptName;
		return(appEvent->eventType);
		}
	    /* Check for a real timeout.  We may get extraneous ones
	     * if a widget has a timer that has elapsed.
	     */
	    else if (eventType == 0 && interval == 0)	//no real event, timeout
		{
		appEvent->eventType = WPT_TIMEOUT_EVENT;
		appEvent->parmName = timeoutName;
		return(appEvent->eventType);
		}
	    else if (eventType == 1)		// event from X - hard coded ??
		{
		// Check if it is a vanilla X window, not a widget
		if (!XtWindowToWidget(xEvent.xany.display, xEvent.xany.window))
		     {
		     WindowEventClass *itemEvent;
		     itemEvent = new WindowEventClass(&xEvent); 
		     //      add event to _wptGenEventList
        	     _wptGenEventList.append( (ent) itemEvent );
		     }
                else 
		    { // intercept some specific events, 
		      // but pass on to widgets too.
                    if (xEvent.type == MapNotify)
                        {
		    // TBD Troubles with `extern "C"': Declared pid_iter a PTR.
                        slist_iterator *pid_iter = 
			    new slist_iterator (_appPanelList);
			while (panelId = (WptPanel *) (*pid_iter)())
                            {
			    Window shellw = panelId->ShellWindow();
			    if (xEvent.xany.window == shellw)
                                {
			        // Found one of our application panels.
				// If it's a "fast" panel, complete it.
				if (!(panelId->GetWidgetId()))    // fast panel
				    panelId->Finish();
				// Reset WM hints to Normal if necessary.
				Display * dpy = 
				    XtDisplay (panelId->WidgetHandle());
				XWMHints *wmHints = XGetWMHints (dpy, shellw);
				if (wmHints->initial_state != NormalState)
        			    {
				    wmHints->initial_state = NormalState;
				    wmHints->flags |= StateHint;
				    XSetWMHints (dpy, shellw, wmHints);
				    }
				XFree ((char *)wmHints);
				break;
				}
			    }
			delete pid_iter;
			}
		    else if (xEvent.type == ReparentNotify)
                        {
		    // TBD Troubles with `extern "C"': Declared pid_iter a PTR.
			slist_iterator *pid_iter = 
			    new slist_iterator (_appPanelList);
			while (panelId = (WptPanel *) (*pid_iter)())
                            {
			    Window shellw = panelId->ShellWindow();
			    if (xEvent.xany.window == shellw)
                                {
				int xOff, yOff;
				FrameOffset (xEvent.xany.display, shellw,
					     &xOff, &yOff);
				panelId->SetOffset(xOff, yOff);
				break;
				}
			    }
			delete pid_iter;
			}
		    // Pass the events on to the widgets
                    if (WptAllowedPanelEvent (xEvent))
                        XtDispatchEvent(&xEvent);  // WptEvent via
                    }                              // widget callbacks
		}
	    else if (eventType == 2)		// from an external source
		     {
		     FileEventClass *itemEvent;
       	    	     itemEvent = new FileEventClass(eventSource, eventMask); 
		     //      add event to _wptGenEventList
        	     _wptGenEventList.append( (ent) itemEvent );
		     }
	    } 
	else					// no timeout or file I/O
	    {
	    // Blocked wait for next X event.
	    // Generate WindowEvent if it is from a vanilla X window.

            if ( XtAppPeekEvent(_wptAppContext,&xEvent) )
            {
	    XtAppNextEvent(_wptAppContext, &xEvent);	
	    if (!XtWindowToWidget(xEvent.xany.display, xEvent.xany.window))
		     {
                     WindowEventClass *itemEvent;
                     itemEvent = new WindowEventClass(&xEvent);
                     //      add event to _wptGenEventList
                     _wptGenEventList.append( (ent) itemEvent );
                     }
            else 
		{
		if (xEvent.type == MapNotify)
                    {
		    slist_iterator *pid_iter = 
			new slist_iterator (_appPanelList);
		    while (panelId = (WptPanel *) (*pid_iter)())
                        {
			Window shellw = panelId->ShellWindow();
			if (xEvent.xany.window == shellw)
                            {
			    // Found one of our application panels.
			    // If it's a "fast" panel, complete it.
			    if (!(panelId->GetWidgetId()))      // fast panel
				panelId->Finish();
			    // Reset WM hints to normal if necessary.
			    Display * dpy = 
				XtDisplay (panelId->WidgetHandle());
			    XWMHints *wmHints = XGetWMHints (dpy, shellw);
			    if (wmHints->initial_state != NormalState)
        		        {
				wmHints->initial_state = NormalState;
				wmHints->flags |= StateHint;
				XSetWMHints (dpy, shellw, wmHints);
				}
			    XFree ((char *)wmHints);
			    break;
			    }
			}
		    delete pid_iter;
		    }
		else if (xEvent.type == ReparentNotify)
                    {
		    slist_iterator *pid_iter = 
			new slist_iterator (_appPanelList);
		    while (panelId = (WptPanel *) (*pid_iter)())
                        {
			Window shellw = panelId->ShellWindow();
			if (xEvent.xany.window == shellw)
                            {
			    int xOff, yOff;
			    FrameOffset (xEvent.xany.display, shellw,
					 &xOff, &yOff);
			    panelId->SetOffset(xOff, yOff);
			    break;
			    }
			}
		    delete pid_iter;
		    }
		// Pass the events on to the widgets
                if (WptAllowedPanelEvent (xEvent))
                    XtDispatchEvent(&xEvent);  	// let widget process it
		}
	    }
            else
                /* timer or alternateInput */
                XtAppProcessEvent (_wptAppContext,XtIMTimer|XtIMAlternateInput);
            }

	}

//  A Wpt Event available. copy it to caller
    _wptGenEvent = (WptEventClass *)_wptGenEventList.get();
    _wptGenEvent->CopyEvent(appEvent); 		// copy to caller (application)
    _wptGenEvent->CopyEvent(&wpt_Event); 	// copy to caller (application)
    delete _wptGenEvent;			// free allocated data
    _wptGenEvent = nil;

    return (appEvent->eventType);
    }

/* Check if it is okay to process the given panel event.
 * If there is a modal panel present, events (except exposure) are
 * not allowed from other panels.
 */
BOOL WptAllowedPanelEvent (XEvent xEvent)
    {
    Display *display;
    WptDisplay *wptdpy;

    display = xEvent.xany.display;
    wptdpy = FindWptDisplay(display);

    if (wptdpy->wptMessageList.count() == 0 &&
	wptdpy->modalWidgetId == nil &&
	_waitingWidgetList.count() == 0 ) return TRUE;
    //
    // Check whether this event belongs to the modal panel.
    // If not, just beep if the event is a button press or keypress
    // and return false.
    // Expose events will always be allowed.  In fact, all events
    // except for ButtonPress, ButtonRelease, KeyPress, KeyRelease,
    // will always be allowed.
    //
    if ( !(xEvent.type == ButtonPress || xEvent.type == ButtonRelease ||
	   xEvent.type == KeyPress || xEvent.type == KeyRelease) )
            return TRUE;
    else if ( wptdpy->wptMessageList.count() != 0 )
	{
	// there are non-blocking message boxes waiting to be dismissed
	Widget pwidget;
	Window eventWindow = xEvent.xany.window;
	slist_iterator pid_iter (wptdpy->wptMessageList);
	while ( pwidget = (Widget) pid_iter())
	    {
	    Widget widget = XtWindowToWidget(display, eventWindow);
	    while(widget)
                {
                if (widget == pwidget )
		    // the event occurred in one of the non-blocking
		    // message boxes
                    return TRUE;
                widget = XtParent(widget);
                }
	    }
	}
    else if ( wptdpy->modalWidgetId != nil )
            {
            Window eventWindow = xEvent.xany.window;
            Widget widget = XtWindowToWidget(display, eventWindow);
            while(widget)
                {
                if (widget == wptdpy->modalWidgetId )
                    return TRUE;
                widget = XtParent(widget);
                }
            }
    else if ( _waitingWidgetList.count() != 0 )
            {
            Widget pwidget;
            Window eventWindow = xEvent.xany.window;
       	    slist_iterator pid_iter (_waitingWidgetList );
            while ( pwidget = (Widget) pid_iter())
              {
              Widget widget = XtWindowToWidget(display, eventWindow);
              while(widget)
                {
                if (widget == pwidget )
                    return FALSE;
                widget = XtParent(widget);
                }
	      }
            return TRUE;
            }


//
//  Does not belong to the modal panel or any nonblocking wpt message panel.
//  Beep and make sure panel is visible.
//
    if (xEvent.type == ButtonPress || xEvent.type == KeyPress)
	{
        XBell(display,0);
	if (wptdpy->wptMessageList.count() != 0)
	    {
	    // nonblocking wpt message panel
	    Widget pwidget;
	    slist_iterator pid_iter (wptdpy->wptMessageList);
	    while ( pwidget = (Widget) pid_iter())
		{
		Xt_RaiseWidget(pwidget);
		}
	    }
	else
	    {
	    // modal panel
	    slist_iterator pid_iter (_appPanelList);
	    WptPanel *panelId;
	    while (panelId = (WptPanel *) pid_iter())	
		{
		if ( wptdpy->modalWidgetId == panelId->WidgetHandle())
		    {
		    Wpt_SetPanelState ((Id)panelId, WPT_VISIBLE);
		    break;
		    }
		}
	    }
	}

    return FALSE;
    }
/**************************************************************************/

/* We implement timeout the following way: 
 * At every Wpt_NextEvent call (with timer), we request a
 * timer proc. If and when the timer expires, the proc is executed. In
 * the proc, we send an event to the application itself 
 * (with XClieneMessage event type). 
 * This creates an XEvent so that we come out of the XtNextEvent wait.
 * Upon examining the XEvent, WptAppWaitForEvent decides what event code 
 * to return to caller.
 *
 * The timeout proc is deleted from the callback list if a real 
 * WPT event is generated before the timeout.
 */



    extern "C" CODE WptAppWaitForEvent(XtAppContext appContext, 
		XEvent *event, unsigned long *intervalPtr,
		Boolean noInterrupt, int *eventSource, 
		int *eventMask)

    {

    if (intervalPtr) 			/* timeout specified	*/
	{
	/* Request control after elapsed time  			*/
	if ( timeoutId == nil )
	    timeoutId = XtAppAddTimeOut(appContext, *intervalPtr, 
			(XtTimerCallbackProc)TimeoutCallbackProc, 
			(Opaque) intervalPtr); 
	}
    if ( XtAppPeekEvent(appContext,event) )
    {

    XtAppNextEvent(appContext, event);		/* wait for an event	*/ 

    if (timeoutId)
	    {
	    XtRemoveTimeOut(timeoutId);		/* cancel timeout	*/ 
	    timeoutId = nil;
	    }
    if (event->type == ClientMessage && 
		(event->xany.window == ClientMessageWindow) )
	{
	/* Read the message subtype to determine the type of event
	 * (createed by the callbacks). 
	 */
	if (event->xclient.message_type == TIMEOUT_MESSAGE)
	    {
	    *intervalPtr = 0;			/* time remaining for timeout */
	    return 0; 
	    }
	else if (event->xclient.message_type == FILE_MESSAGE)
	    {
	    *eventSource = (int) event->xclient.data.l[0];
	    *eventMask = (int) event->xclient.data.l[1]; 
	    return 2; 
	    }
	else if (!noInterrupt && event->xclient.message_type == OPINT_MESSAGE)
	    {
	    return -1; 
	    }
	}		
    else
	return 1;			/* a real X event from the server */
    }
   else
        {
        if ( !_wptGenEventList.count())
        	XtAppProcessEvent (appContext,XtIMTimer|XtIMAlternateInput);

        if (_wptGenEventList.count() != 0)
        {
        WptEvent appEvent;
        zero_block((char *)&appEvent, sizeof(WptEvent));        // initialize
//      A Wpt Event available. copy it to caller
	_wptGenEvent = ( WptEventClass *)_wptGenEventList.head();
        _wptGenEvent->CopyEvent(&appEvent);                     //copy to caller
        _wptGenEvent->CopyEvent(&wpt_Event); 	// copy to caller (application)

    	if (timeoutId)
	    {
	    XtRemoveTimeOut(timeoutId);		/* cancel timeout	*/ 
	    timeoutId = nil;
	    }

        if ( appEvent.eventType == WPT_PARM_EVENT )
                {
                if (&(appEvent.p_xEvent) != nil )
                        {

//                      MOVE_STRUCT ((appEvent.p_xEvent), *event );
                        event->xany.window = appEvent.p_windowId;
                        if ( appEvent.p_panelWidgetId )
                            event->xany.display =
                                XtDisplay(appEvent.p_panelWidgetId);
                        else
                            event->xany.display = _defaultDisplay;

                        event->xany.type = NULL;
                        return 1;
                        }
                }

        }

        return 3;
        }
    }


/*************************************************************************/
/* 
 * The timer has elapsed. Send a message to the application itself 
 * via the X server, using the ClientMessage Event.
 */

    static void TimeoutCallbackProc(void * client_data, XtIntervalId *id)

    {
     if (*id != timeoutId)
    	return;
    
    client_data = client_data;
    timeoutId = nil; 
    TIMEOUT_MESSAGE = XInternAtom(XtDisplay(_wptInitialWidget), "_timeOut", 0);
    SendClientMessage(TIMEOUT_MESSAGE, NULL, NULL);  
    return;
    }


/*************************************************************************/
/* 
 * Operator interrupt has occured. Send a message to the application itself 
 * via the X server, using the ClientMessage Event.
 */

    static FUNCTION void OpIntHandler()

    {

    if (ignoreInterrupt)
	return;

    OPINT_MESSAGE = XInternAtom(XtDisplay(_wptInitialWidget), "_interrupt", 0);
    SendClientMessage(OPINT_MESSAGE, NULL, NULL);  


    EnableInterrupt(TRUE);		// request for next time 
    return;
    }
/************************************************************************/

    static void SendClientMessage(Atom msg_type, int data1, int data2)

    {
    XClientMessageEvent  sendEvent;

    sendEvent.type = ClientMessage;
    sendEvent.display = XtDisplay(_wptInitialWidget);
    sendEvent.window = ClientMessageWindow; 
    sendEvent.message_type = msg_type;
    sendEvent.format = 32;
    sendEvent.data.l[0] = data1;
    sendEvent.data.l[1] = data2;

    Status status;
    status = 1;			/**** TBD: *********/
    XSendEvent(XtDisplay(_wptInitialWidget), ClientMessageWindow, 
		False, (unsigned long) NoEventMask, (XEvent *) &sendEvent);
    if (status)
	XFlush(XtDisplay(_wptInitialWidget));	// server must get it now
    else
	{
        WptError(XtScreen(_wptInitialWidget), WPT_FATALERROR,
	"Could not send ClientMessage to the server. Exiting.",
	"WPT-BADXSEND");
	}
    return;
    }

/**********************************************************************
 *
 * Wpt_CheckEvent.  Check if a Wpt event exists.
 * 
 * Note: There may be atmost one Wpt event existing at a time.
 *
 ************************************************************************/

    extern "C" UNSUPPORTED BOOL Wpt_CheckEvent()
    
    {
    return (_wptGenEventList.count() != 0);
    }


/***********************************************************************
 *
 * Wpt_Pending. Check if a WptEvent is pending, from X, Parm or a file.
 *
 * TBD:  Wpt_Pending ignores WPT_TIMEOUT_EVENTS.
 *
 ************************************************************************/

    extern "C" FUNCTION BOOL Wpt_Pending()

    {
    XEvent xEvent;
    if (_wptGenEventList.count() != 0)
	return (TRUE);

    XtInputMask mask;
    while (mask = XtAppPending(_wptAppContext))
	{
	if (mask & XtIMAlternateInput)		// Event from a file
	    return (TRUE);

	XtAppPeekEvent (_wptAppContext, &xEvent);

	if (mask & XtIMTimer)
	    {
	    // needed to handle Motif double-clicks
	    XtAppProcessEvent (_wptAppContext, XtIMTimer);
	    }
	else
	    {
	    if (!XtWindowToWidget(xEvent.xany.display, xEvent.xany.window))
		return (TRUE);			// A window event is waiting
	    else
		{
		// Event from a widget window. Process it and check if it
		// generates a WptEvent.
		
		XtAppNextEvent(_wptAppContext, &xEvent);	
		if (WptAllowedPanelEvent (xEvent))
		    {
		    XtDispatchEvent(&xEvent);  	// let widget proc. it
		    if (_wptGenEventList.count() != 0)
			// resulted in a WPT event
			return (TRUE);
		    }
		}
	    }
	}
    return (FALSE);	
    }

/************************************************************************
 *
 *  Wpt_PanelErase.  Delete the specified panel.  
 *  This also involves deassociating the saved data and freeing
 *  up the dynamic memory allocated for that session.
 *
 *  Note: When a user panel is erased, all Help panels associated with 
 *  that panel are also automatically deleted. (TBD)
 *
 ************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_PanelErase(Id thePanel)

    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

//
//   if modal reset modal id to null
//
    WptDisplay *wptdpy = FindWptDisplay (XtDisplay(panelId->WidgetHandle()));
    if ( panelId->WidgetHandle()  == wptdpy->modalWidgetId )
        wptdpy->modalWidgetId = nil;
   // TBD: delete all outstanding help panels for this panel
    
//
//   Remove the panel from the wait list
//
    Wpt_EndWait(thePanel);

//
//   Remove from the DDO rehearsal list
//
    if (DDOREHEARSING)
	Rehearse_RemovePanel(panelId); 
    delete panelId;				// invoke the dtor 
    _appPanelList.remove(panelId);		// remove from panel list
    return (SUCCESS);
    }

 
/***************************************************************************
 *
 *  Wpt_PanelReset. Reset the given panel with original parameter values.
 *
 *  	This routine redisplays all the parameters on the panel with
 *	their initial values and selected indices supplied with the 
 *	Wpt_NewPanel() call.
 *
 ***************************************************************************/


    extern "C" FUNCTION CODE  Wpt_PanelReset (Id thePanel)
    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    panelId->Reset();
    return (SUCCESS);
    }


/***************************************************************************
 *
 *  Wpt_BeginWait. Begin the presentation of Wait feedback for the user.
 *
 ***************************************************************************/


    extern "C" FUNCTION CODE  Wpt_BeginWait (Id thePanel)
    {

    WptPanel *panelId = (WptPanel *) thePanel;
    Widget pwidget;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);

/*
 *  check whether already in list 
 */
    slist_iterator *pid_iter = new slist_iterator (_waitingWidgetList );
    while ( pwidget = (Widget) (*pid_iter)() )
	{
        if ( pwidget == (Widget) panelId->WidgetHandle() )
		{
    		delete pid_iter;
                return (SUCCESS);
		}
	}
    delete pid_iter;

    _waitingWidgetList.insert ( (ent)panelId->WidgetHandle() );
    panelId->BeginWait();
    return (SUCCESS);
    }

/****************************************************************************
 *
 *      Wpt_EndWait.  Turns off the blocking waiting feedback.
 *
 ***************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_EndWait(Id thePanel)
    {

    Widget pwidget;
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);

    slist_iterator *pid_iter = new slist_iterator (_waitingWidgetList );
    while ( pwidget = (Widget) (*pid_iter)() )
	{
	if ( pwidget == (Widget)panelId->WidgetHandle() )
		{
    		panelId->EndWait();
    		_waitingWidgetList.remove ( (ent)panelId->WidgetHandle() );
    		delete pid_iter;
    		return (SUCCESS);
		}
	}
    delete pid_iter;

    return (SUCCESS);
    }

/***************************************************************************
 *
 *  Wpt_PanelMessage.  Print a message pertaining to a
 *      Wpt panel session.
 *
 **********************************************************************/

    extern "C" FUNCTION  CODE  Wpt_PanelMessage(Id thePanel, const TEXT *message)
    {

    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);
    
    panelId->Message(message);
    return (SUCCESS);
    }


/***************************************************************************
 *
 *  Wpt_MessageNoBlock.  Print a message pertaining to a
 *      Wpt panel session.  Don't block.
 *
 *  This routine is currently unsupported.  The name or the argument
 *  list may change in the future.
 *
 **********************************************************************/

    extern "C" UNSUPPORTED  CODE  Wpt_MessageNoBlock(Id thePanel,
						  const TEXT *message,
						  void *userContext,
						  BOOL returnToApp)
    {

    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);
    
    // create a nonblocking message box that will return an event back
    // to the application
    panelId->MessageNoBlock(message, userContext, returnToApp);
    return (SUCCESS);
    }


/***************************************************************************
 *
 *  Wpt_ParmReject. Generate a rejection message  for a given parameter. 
 *
 *  	This routine generates a rejection message for a given parameter.
 *	The bad parameter value remains present on the screen until
 *	the user exits the message box, at which time the value reverts
 *	back to the most recent value that was acceptable.
 * 
 ***************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_ParmReject(Id thePanel,const TEXT *parmName, 
			 		      TEXT *message)
    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);
    message = message;

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);

    Wpt_PanelMessage(thePanel, message);	// Display the message
    code = itemId->ResetLast();		// reset to last accepted value 
    return (code);
    }

/***************************************************************************
 *
 *  Wpt_RejectNoBlock. Generate a rejection message  for a given parameter. 
 *
 *  This is identical to Wpt_ParmReject, except it does not busy wait.
 * 
 *  This routine is currently unsupported.  The name or the argument
 *  list may change in the future.
 *
 ***************************************************************************/

    extern "C" UNSUPPORTED  CODE  Wpt_RejectNoBlock(Id thePanel, 
					const TEXT *parmName,  TEXT *message)
    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);
    message = message;

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);

    Wpt_MessageNoBlock(thePanel, message, NULL, FALSE);	// Display the message
    code = itemId->ResetLast();		// reset to last accepted value 
    return (code);
    }


/*************************************************************************
 *
 *   Wpt_PanelWindow. Return the X Window Id of a Wpt Panel
 *
 *   Note: This window Id corresponds to that of the TAE Box widget, rather
 *   the windowId of the top level shell widget.
 *
 *************************************************************************/

    extern "C" FUNCTION Window Wpt_PanelWindow (Id thePanel)

    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (nil);

    return (panelId->WindowId());		// Window Id of box widget 
    }


/***********************************************************************
 *
 * Wpt_PanelTopWindow. Return the top level window Id of a Wpt Panel.
 *
 * This window should be used to move/hide/show the panel via X calls.
 *
 ***********************************************************************/

    extern "C" FUNCTION  Window Wpt_PanelTopWindow (Id thePanel)

    {
    WptPanel *panelId = (WptPanel *) thePanel;


    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (nil);
	
    return (panelId->ShellWindow());
    }


/*************************************************************************
 *
 *   Wpt_PanelWidgetId. Return the Widget Id of a Wpt Panel
 *
 *************************************************************************/

    extern "C" FUNCTION Widget Wpt_PanelWidgetId (Id thePanel)

    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (nil);

    return (panelId->WidgetHandle());		// Widget Id of top level 
    }


/*************************************************************************
 *
 *   Wpt_PanelDisplayId. Return the Display Id and Screen number of a Wpt Panel
 *
 *************************************************************************/

    extern "C" FUNCTION CODE Wpt_PanelDisplayId (Id thePanel, 
						   Display **displayId,
						   COUNT *screen_number)

    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    *displayId = XtDisplay(panelId->WidgetHandle());
    *screen_number = XScreenNumberOfScreen
	(XtScreen(panelId->WidgetHandle()));
    return (SUCCESS);
    }


/**********************************************************************
 *
 *  Wpt_ItemWindow. Return the X window Id for a panel item.  
 *
 *  Note: Useful for workspace items.
 *
 **********************************************************************/

    extern "C" FUNCTION Window Wpt_ItemWindow (Id thePanel,
                                               const TEXT *parmName)

    {
    WptPanel *panelId = (WptPanel *) thePanel;
    WptItem *itemId;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);
    else
    if (itemId)				               // a valid item name
	return (itemId->WindowId());

    return (nil);				       // item not on this panel
    }

/**********************************************************************
 *
 *	Wpt_ParmUpdate - Update a parameter value on a displayed panel.
 *
 * 	This function updates the value associated with a parameter 
 *	displayed on a WPT panel. The new value and selected entry are
 *	retrieved from the parameter block (dataVm) associated with the
 *	panel. The caller must set the new value, that is the new set of
 *	valids, or the new default, for the named parameter in the block
 *	before invoking this function.
 *
 *********************************************************************/

    extern "C" FUNCTION  CODE  Wpt_ParmUpdate(Id thePanel, 
                                              const TEXT *parmName)

    {
    CODE 	code;
    WptItem   *itemId;			// Id of Wpt object for the parm

    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);

    code = GetPanelParmNoMsg (thePanel, parmName, &itemId); 
    if (code == FAIL)
	return (FAIL);			// note: intentionally no error msg 
 
    Symbol *v = panelId->TargetSt()->GetSymbol(parmName);
    if (v == nil)                       // parm no longer there
        return (FAIL);                  // no message ??

    code = itemId->UpdateItem(v);    // assume target already updated by caller
    if (code != SUCCESS)
	goto update_error;
    
    return (SUCCESS);

update_error:
    WptPanelError(thePanel, WPT_WARNING, 
	"Parameter '%s' could not be updated with given values.", 
	"WPT-BADPVAL", parmName);
    return (FAIL);
    }


/**********************************************************************
 *
 *      Wpt_ViewUpdate - Update a parameter's  view on a displayed panel.
 *
 *      This function changes the view associated with a parameter
 *      displayed on a WPT panel. The new view information is retrieved
 *      from the view block under the named parameter (viewParm).
 *      Note that if the specified view block could be a different
 *      one from the original one used to draw the new panel, or
 *      it may be the same view block with certain  attributes updated.
 *
 *********************************************************************/

    extern "C" FUNCTION  CODE  Wpt_ViewUpdate(Id thePanel, 
                                              const TEXT *parmName,
                                              Id viewblk, const TEXT *viewParm)

    {
    CODE        code;
    WptItem     *itemId;                // Id of Wpt object for the parm

    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);

    code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);


    Symbol *newView = ((SymbolTable *)viewblk)->GetSymbol(viewParm);

    if (newView == nil)
        goto bad_view;

    code = itemId->UpdateView(newView);
    if (code != SUCCESS)
        goto redraw_error;

    return (SUCCESS);

bad_view:
    WptPanelError(thePanel, WPT_WARNING,
        "Parameter '%s' could not be found in specified view.",
        "WPT-BADVIEWPARM", viewParm);
    return (FAIL);

redraw_error:
    WptPanelError(thePanel, WPT_WARNING,
        "Parameter '%s' could not be updated with given view.",
        "WPT-BADPVIEW", parmName);
    return (FAIL);
    }



/**********************************************************************
 *
 * GetPanelParm.  Get the Id of item corresponding to the given parm in
 * the specified panel.
 *
 * If the parm is not found, an error message is printed by this function.
 *
 **********************************************************************/


    static WptItem * GetPanelParm(Id thePanel, const TEXT *parmName) 

    {
    WptItem 	*itemId; 

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId); 

    if (code != SUCCESS)
	{
        WptPanelError(thePanel, WPT_WARNING,
        "Parameter '%s' is not displayed in the given panel.",
        "WPT-BADPNAME", parmName);
	return (nil);
	}
    return (itemId);
    }

/**********************************************************************
 *
 * GetPanelParmNoMsg.  Get the Id of item corresponding to the given parm in
 * the specified panel.
 *
 * If the parm is not found, this function does not generate any msg.
 *
 **********************************************************************/

    CODE  GetPanelParmNoMsg(Id thePanel, const TEXT *parmName, 
		WptItem **itemId) 

    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    // If this panel is not completely built (it's a "fast" panel), as a
    // courtesy, go ahead and complete its construction.
    if (!panelId->GetWidgetId())    	
	panelId->Finish();

    TEXT tempname[STRINGSIZ+1];
    s_copy ( parmName, tempname );
    *itemId = panelId->GetItemId(tempname);
    return (*itemId ? SUCCESS : FAIL);
    }

/***********************************************************************
 *
 *      Wpt_MissingVal. Check if a parameter in the Vm data block has 
 *	value missing.
 *
 *	This routine return list of missing parameters, i.e., parameters
 *	which do not have a current value specified.
 *	Parms in returned list are separated by commas.
 *
 *	Returns TRUE if any parm with missing value (that fit in missingList).
 *
 *	Note that message should be long enough to hold the name of all
 *      missing parameters.
 *
 **********************************************************************/

    extern "C" FUNCTION  BOOL  Wpt_MissingVal(Id dataVm, TEXT *message)


    {
    COUNT		listlen;	// length of missing msg 
    BOOL		missing;	
    SymbolTable 	*vmBlock;

    COUNT  count = 0;
    message[0] = EOS;			// clear the list 
    if (dataVm == nil)
	return (FALSE);

    listlen = 0;
    vmBlock = (SymbolTable *) dataVm; 

    ForEachSymbol(v, *vmBlock)	  
	// NOTE: for type=VOID v_count=0; for Nullables  v_count=-1 is OK
        missing = ( (!v->Nullable()) && (v->Count() == -1) );
	if (missing)
	    {
	    count++;
	    s_append(v->Name(), message);	    // append name to list  
	    listlen = s_append(", ", message);
	    }
    EndForEach(v);
	
    if (listlen > 1)
	message[listlen-2] = EOS;		// delete last comma
    return (count > 0);			// missing if count > 0 
    }


/*************************************************************************
 *
 *  Wpt_PrintMessage.  Print a message on the screen.
 *
 *  This is similar to Wpt_PanelMessage, but prints the message on
 *  with respect to the root window. Intended to be called from
 *  c functions. (Currently WptError needed it.)
 *
 ************************************************************************/

    extern "C" UNSUPPORTED  void Wpt_PrintMessage(Screen *screen, 
                                               const TEXT *message)

    {
    if (screen)		// Display has been opened
	{
	// Create a non-blocking message box.  When the user clicks on
	// the "noted" button, NO wpt event will be sent back to the
	// application.
        MessageBox *msgbox = new MessageBox(message, screen, FALSE);
	}
    else if (_defaultDisplay)
	{
	// Wpt_PrintMessage should never be called without a screen,
	// so this should never happen.
        MessageBox *msgbox = 
	    new MessageBox(message, 
			   DefaultScreenOfDisplay(_defaultDisplay), 
			   FALSE);
	}
    else
	{
	printf(message);		// no display: print to stdout
	printf ("\n");
	}
    return;
    }
    

/**********************************************************************
 *
 *	Wpt_QueryItem. Query the size of an item.
 *
 *********************************************************************/

    extern "C" UNSUPPORTED  CODE  Wpt_QueryItem(Id thePanel, 
                                                const TEXT *parmName, 
		                       int *x, int *y, int *width, int *height)
    {
    Coord	ix0, iy0; 
    Coord	iwidth, iheight;

    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);

    itemId->GetOrigin(ix0, iy0);		// get item's corners 
    itemId->GetSize(iwidth, iheight);		// get item's dimension
    *x = (int) ix0;
    *y = (int) iy0; 
    *width = (int) iwidth;
    *height =  (int) iheight;
    return (SUCCESS);
    }

// For Output Objects, specifically, but can be used in general

/**********************************************************************
 *
 *	The following Wpt functions Wpt_SetReal, Wpt_SetIntg, 
 *	Wpt_SetString, and Wpt_SetNoValue each update a target
 *	parameter and call Wpt_ParmUpdate. The functions are combination 
 * 	of Vm_Set functions and Wpt_ParmUpdate function.
 *
 *	Wpt_SetReal - Set real value into object context and
 *		      update the value on a displayed panel.
 *	Wpt_SetIntg - Set integer value into object context and
 *		      update the value on a displayed panel.
 *	Wpt_SetString - Set string value into object context and
 *		        update the value on a displayed panel.
 *	Wpt_SetNoValue - Set to no value. This function is called 
 *			 for realtime graphs. Real time graphs are
 *			 updated automatically by Wpt according to
 *			 the polling interval defined for the graph.
 *
 *	For setting a value, the new value and selected entry are
 *	retrieved from the parameter block (dataVm) associated with the
 *	panel. The caller must set the new value, that is the new set of
 *	valids, or the new default, for the named parameter in the block
 *	before invoking this function.
 *
 *********************************************************************/

    extern "C" FUNCTION  CODE  Wpt_SetReal (Id thePanel, const TEXT *parmName, 
			TAEFLOAT real)  
    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);


    itemId->TargetSym()->SetElement(real, 0); 
    Wpt_ParmUpdate (thePanel, parmName);
    
    return (SUCCESS);
    }

 /*********************************************************************/

    extern "C" FUNCTION  CODE  Wpt_SetIntg (Id thePanel, const TEXT *parmName, 
			TAEINT intg)  	

    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);

    itemId->TargetSym()->SetElement(intg, 0); 
    Wpt_ParmUpdate (thePanel, parmName);
    
    return (SUCCESS);
    }

 /*********************************************************************/
   
 
    extern "C" FUNCTION CODE  Wpt_SetString (Id thePanel,const TEXT *parmName, 
				   const TEXT *string)  

    {
    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);


    itemId->TargetSym()->SetElement(string, 0); 
    Wpt_ParmUpdate (thePanel, parmName);

    return (SUCCESS);
    }

 /*********************************************************************/

    extern "C" FUNCTION CODE Wpt_SetNoValue (Id thePanel,const TEXT *parmName)

    {
#define NO_VALUE 0

    WptPanel *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (FAIL);

    itemId->TargetSym()->SetNullValue();	// make count = 0

    Wpt_ParmUpdate (thePanel, parmName);
    return (SUCCESS);
    }


/***************************************************************************
 *
 *  Wpt_CloseDisplay. Close connection to X server.
 *
 ***************************************************************************/
    extern "C" FUNCTION  CODE  Wpt_CloseDisplay(Display *theDisplay)

    {
    if (theDisplay)
	{
	// look for display in known list of displays
	WptDisplay	*wptdpy = FindWptDisplay(theDisplay);
	if (wptdpy)
	    {
	    if (wptdpy->dpy == _defaultDisplay)
		{
		// close the default display
		_defaultWptDisplay = NULL;
		_defaultDisplay = NULL;
		}

	    if (wptdpy->aPainter) delete wptdpy->aPainter;
	    if (wptdpy->aState) delete wptdpy->aState;
	    if (wptdpy->world) delete wptdpy->world;

	    // remove wptdpy from _wptDisplayList
	    _wptDisplayList.remove(wptdpy);

	    tae_free (wptdpy);

	    // close the display
/* XtCloseDisplay() not called for HP, PR-1860 temporary fix.	*/
#if !defined(hpux) || !defined(TAEX11R4)
	    XtCloseDisplay(theDisplay);
#endif
	    return SUCCESS;
	    }
	}
    return FAIL;
    }

/***************************************************************************
 *
 *  Wpt_CloseItems. Close (Terminate) any outstanding editors in the panel
 *
 *      This routine is primarily used to terminate any data entry
 *      items on the panel left unfinished by the user. Typically, 
 *      PageEdit items may be in this state.
 *
 ***************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_CloseItems(Id thePanel)

    {
    WptPanel *panelId = (WptPanel *) thePanel; 

    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    panelId->CloseItems();
    return  SUCCESS;
    }

/***************************************************************************
 *
 *  Wpt_SetPanelState. 	Set the state (iconic, invisible, etc.) of a panel.
 *
 *  Acceptable states:
 *			WPT_PREFERRED (use state info in panel's view)
 *			WPT_VISIBLE
 *			WPT_INVISIBLE
 *			WPT_ICONIC
 *			WPT_FASTICONIC (interpreted as WPT_ICONIC)
 *
 ***************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_SetPanelState (Id thePanel, int state)
    {
    TAEINT curState;
    TAEINT nextState = state;			// local copy

    WptPanel *panelId = (WptPanel *) thePanel; 
    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

//
    Boolean childpanel = panelId->ChildPanel();
    Widget toplevel = panelId->WidgetHandle();
    Window shellw; 
    Display *dpy = XtDisplay (toplevel);
    WptDisplay *wptdpy = FindWptDisplay(dpy);
#ifdef VMS
    Screen *scr = XtScreen(toplevel);
    Screen *dpyscr = dpy->screens;
    int i;
    int screen = -1;

    for (i = 0; i < dpy->nscreens; i++, dpyscr++) {
              if (scr == dpyscr)
                {
                screen = i;
                break;
                }
    }
#else
    int screen = XScreenNumberOfScreen (XtScreen(toplevel));
#endif

    if ( childpanel )
        shellw = XtWindow(panelId->WidgetHandle()) ;
    else
	shellw = panelId->ShellWindow();

    // WPT_PREFERRED comes from generated code and means: Use panel preferred
    // state information in panel's view.
    if (nextState == WPT_PREFERRED)
	{
	nextState = panelId->GetPreferredState();
	if (toplevel == wptdpy->modalWidgetId && nextState == WPT_MODAL )
		nextState = WPT_VISIBLE;
	}

    // Modal panels can only be made visible.
    if (toplevel == wptdpy->modalWidgetId && nextState != WPT_VISIBLE ) 
	return (P_BADSTATE);

    Wpt_GetPanelState ((Id)panelId, &curState);

    // These state transitions are ICCCM compliant.
    if (nextState == WPT_VISIBLE)		// set to visible
	{
	if (curState == WPT_VISIBLE || curState == WPT_MODAL)
	    {
	    // just raise it
	    XRaiseWindow (dpy, shellw);
	    }
 	else
	    {
	    // wm hints are ignored if current state is iconic
	    // or panel is a Child panel
	    if ( !childpanel )
	      {
	      if (curState == WPT_INVISIBLE)
	        {	
                XWMHints *xwmh = XGetWMHints (dpy, shellw);
            	xwmh->flags = StateHint;
            	xwmh->initial_state = NormalState;
            	XSetWMHints (dpy, shellw, xwmh);
	        XFree((char *)xwmh);
	    	}
	      }
	    XMapWindow (dpy, shellw);
	    }
	}
    else if (nextState == WPT_INVISIBLE)	// set to invisible
	{
	if (curState != WPT_INVISIBLE)		// skip if already done
	    if ( childpanel )			// for childpanel just unmap
	    	XUnmapWindow (dpy, shellw);
	    else
	    	XWithdrawWindow (dpy, shellw, screen);
	}
    else if (nextState == WPT_ICONIC || nextState == WPT_FASTICONIC)
	{	// set to iconic
	if ( !childpanel )	// iconic does not apply to child panel
	  {
          if (curState == WPT_VISIBLE)
	    XIconifyWindow (dpy, shellw, screen);
	  else if (curState == WPT_INVISIBLE)
	    {
            XWMHints *xwmh = XGetWMHints (dpy, shellw);
            xwmh->flags = StateHint;
            xwmh->initial_state = IconicState;
            XSetWMHints (dpy, shellw, xwmh);
            XMapWindow(dpy, shellw);
	    XFree((char *)xwmh);
	    }
	  }
	}
    else
	{
	// Note that WPT_MODAL is rejected.
	return (P_BADSTATE);
	}

    XSync (dpy, FALSE);		// Get server to act on it immediately.
    return (SUCCESS);
    }


/***************************************************************************
 *
 *  Wpt_GetPanelState. 	Get the state of a panel.
 *
 *  Possible states are:
 *				WPT_VISIBLE
 *				WPT_INVISIBLE
 *				WPT_MODAL
 *				WPT_ICONIC
 *
 *  Note that WPT_ICONIC includes WPT_FASTICONIC panels.
 *
 ***************************************************************************/

    extern "C" FUNCTION  CODE  Wpt_GetPanelState (Id thePanel, TAEINT *state)
    {

    WptPanel *panelId = (WptPanel *) thePanel; 
    if (!CheckValidPanel(panelId))                      // invalid panel Id
	return (FAIL);

    Widget toplevel = panelId->WidgetHandle();
    Window shellw = panelId->ShellWindow();
    Boolean childpanel = panelId->ChildPanel();
    Display *dpy = XtDisplay(toplevel);
    WptDisplay *wptdpy = FindWptDisplay(dpy);

    if (toplevel == wptdpy->modalWidgetId)
	{
	*state = WPT_MODAL;
   	return (SUCCESS); 		// modal panel
	}

    // TBD See GetWMState() caveat in function definition below.

    // Child panels are unknown to the WM, so just get its WindowAttributes
    if ( childpanel )
	{				// child panel
	XWindowAttributes   winInfo;
	shellw = XtWindow(panelId->WidgetHandle());
        XGetWindowAttributes(dpy, shellw, &winInfo);
	if (winInfo.map_state == IsUnmapped)
	    *state = WPT_INVISIBLE;
	else
	    *state = WPT_VISIBLE;
	return (SUCCESS);
	}

    // Normal Top level panel
    unsigned long *wmState = GetWMState (dpy, shellw); 
    if (!wmState)
	{
	// Assume it was never mapped, thus WM doesn't know about it.
	*state = WPT_INVISIBLE;	
	return (SUCCESS);
	}

    if (*wmState == IconicState)
        *state = WPT_ICONIC;
    else if (*wmState == NormalState)
        *state = WPT_VISIBLE;
    else if (*wmState == WithdrawnState)
        *state = WPT_INVISIBLE;
    else
	{
	printf ("Wpt_GetPanelState Design Error: No matching window state.\n"); 
        XFree ((char *)wmState);
        return (FAIL);
	}

    XFree ((char *)wmState);
    return (SUCCESS);
    }

/***************************************************************************
 *
 *  GetWMState. 	Get the window manager state.
 *
 *  
 *  TBD: This function will eventually be superceded when WM_STATE support
 *  is added to the official X release.
 * 
 ****************************************************************************/

#define WM_STATE_ELEMENTS 1

unsigned long *GetWMState (Display *dpy, Window window)
{
    int ret_val;
    unsigned long *property = NULL;
    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long leftover;


    Atom xa_WM_STATE = XInternAtom (dpy, "WM_STATE", False);

    ret_val = XGetWindowProperty (dpy, window, xa_WM_STATE, 
		  0L, WM_STATE_ELEMENTS,
		  False, xa_WM_STATE, 
		  &actual_type, &actual_format, 
		  &nitems, &leftover, (unsigned char **)&property);

    if (!((ret_val == Success) && (actual_type == xa_WM_STATE) &&
         (nitems == WM_STATE_ELEMENTS)))
        { // The property could not be retrieved or is not correctly set up. 
        if (property)
            {
	    XFree ((char *)property);
	    property = NULL;
            }
        }

    return (property);
} 


/******************************************************************/

//
// Set the input sensitivity state of an item
//
// The argument 'state' indicates TRUE of FALSE.


    extern "C" FUNCTION  CODE Wpt_SetItemSensitivity (Id thePanel,
                                              const TEXT *parmName, BOOL state)
    {
    int newState = WPT_INACTIVE;
    WptPanel  *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);                  // invalid panel Id

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (P_BADNAME);

    if ( state )
	newState = WPT_ACTIVE;

    itemId->SetState(newState);

    return (SUCCESS);
    }
//
// Get the input sensitivity state of an item
//
// The argument 'state' returns TRUE of FALSE.


    extern "C" FUNCTION  CODE Wpt_GetItemSensitivity (Id thePanel,
                                             const TEXT *parmName, BOOL *state)
    {
    int newState;
    WptPanel  *panelId = (WptPanel *) thePanel;

    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return (FAIL);                  // invalid panel Id

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (P_BADNAME);

    itemId->GetState(&newState);

    if ( newState == WPT_ACTIVE )
	*state = TRUE;
    else
	*state = FALSE;

    return (SUCCESS);
    }


//
// Set the input sensitivity state of the constraints of an item
//
// The argument 'state' indicates TRUE of FALSE.


    extern "C" FUNCTION  CODE Wpt_SetConstraintSensitivity (Id thePanel,
                                  const TEXT *parmName, FUNINT num, 
                                  const TEXT **stringvec, BOOL *state)
    {
    WptPanel  *panelId = (WptPanel *) thePanel;
    int i;


    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return ( P_NONE );                  // invalid panel Id

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (P_BADNAME);

    Symbol *s = itemId->TargetSym();
    int vcount = s->Valids();
    BOOL find ;
    int vindex;
    CODE status;


    for ( i=0; i<num; i++ )
        {
        find = FALSE;

        for ( vindex = 0; vindex < vcount; vindex++ )
                {
                if ( s_equal ( stringvec[i], s->Valid (vindex) ) )
                        {
                        status = itemId->SetState(vindex, state[i] );
                        if ( status != SUCCESS )
                                return ( P_BADTYPE );
                        find = TRUE;
			break;
                        }
                }
        if ( !find )
                return ( FAIL );
        }
    return(SUCCESS);
    }

//
// Get the input sensitivity state of the constraints of an item
//


    extern "C" FUNCTION  CODE Wpt_GetConstraintSensitivity (Id thePanel,
                                  const TEXT *parmName, FUNINT num, 
                                  TEXT **stringvec, BOOL *state)
    {
    WptPanel  *panelId = (WptPanel *) thePanel;
    int i;


    if (!CheckValidPanel(panelId))                      // invalid panel Id
        return ( P_NONE );                  // invalid panel Id

    WptItem *itemId;

    CODE code = GetPanelParmNoMsg(thePanel, parmName, &itemId);

    if (code != SUCCESS)
        return (P_BADNAME);

    Symbol *s = itemId->TargetSym();
    int vcount = s->Valids();
    BOOL find ;
    int vindex;
    CODE status;


    for ( i=0; i<num; i++ )
        {
        find = FALSE;

        for ( vindex = 0; vindex < vcount; vindex++ )
                {
                if ( s_equal ( stringvec[i], s->Valid (vindex) ) )
                        {
			int newstate;
                        status = itemId->GetState(vindex, &newstate );
                        if ( status != SUCCESS )
                                return ( P_BADTYPE );
			state[i] = newstate;

                        find = TRUE;
                        break;
                        }
                }
        if ( !find )
                return ( FAIL );
        }
    return(SUCCESS);
    }

BOOL CheckValidPanel ( WptPanel * panelId )
    {
    WptPanel *listPanel;
    slist_iterator pid_iter (_appPanelList);
    
    if ( panelId == nil )
	return FALSE;

    while ((listPanel = (WptPanel *) pid_iter()))
        if (panelId == listPanel) return TRUE;

    return FALSE;
    }



