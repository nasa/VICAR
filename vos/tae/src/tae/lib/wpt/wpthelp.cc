/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/************************************************************************
 * 
 *  Window Programming package HELP related functions. 
 *
 *  This module contains the functions required to construct, display,
 *  and process the help panels associated with a WPT panel.
 *
 **********************************************************************
 *
 *  CHANGE LOG:
 *
 * 21-jun-89	Original C++ version, under Xt architecture...dm
 * 14-jul-89	Delete panel on CloseHelp icon release...dm
 * 19-jul-89	Make help panel visible if falls outside screen...dm
 * 10-sep-89	VMS port...ljn
 * 25-jan-90	g++ 1.36 port...ljn
 * 05-apr-90    merge in motif changes...tpl
 * 17-jun-90	XtAddCallback() casted properly...ljn
 * 04-dec-90	Changed GetBox to GetFullBox...krw
 *		Resfile now in TAERES
 * 18-dec-90    Added activatecallback...tpl
 * 21-feb-91	HP port for 5.0...ljn
 * 30-mar-91	Header files for VMS...ljn
 * 15-apr-91    Corrected declaration of wpthelpcreate...tpl */
// 30-apr-91    corrected vms help res file location...ljn
// 06-jan-92	PR1257: Changed #include filenames to use <>...ljn
// 22-feb-92	Don't reverse video during help for items with components...ljn
// 30-mar-92  	Multiple Display support.  Change WptError to WptPanelError.
//            	Change create cursor arg from display to screen.
//            	Remove reference to DefaultRootWindow...cew
// 01-apr-92	Allow capability to define your own help style...ljn
// 07-apr-92	PR1314: Check when helpSpec is NULL in WptItem::Helper...rt
// 25-apr-92	helpStyle should be declared with size FSPECSIZ+1...ljn
// 01-oct-92	helpStyle should be declared and defined here...ljn/rt
// 05-oct-92	Fix C++ anachronisms...rt
// 14-oct-92	SGI does not like GLOBAL TEXT for helpStyle...rt
/**************************************************************************/

#include         <widgetagent.h>
#include         <helppanel.h>
#include	 <tminc.inc>			// for TXTSTOR structure

extern "C"
{
XCursor  X_CreateCursor (Screen *, int, int, char [], char [], int, int, int); 
struct  TXTSTOR *WptHelpCreate(TEXT *, BOOL, TEXT *);
void    WptFreeHelp(struct TXTSTOR *);
}

static void ServiceCloseIcon(Widget widgetId,
                void * closure,  void * callData);
static XCursor CreateHelpCursor(Screen *display);

#ifdef VMS
GLOBAL TEXT helpStyle[FSPECSIZ+1] = "$TAERES:wpthelp.res";
#else
#if defined(sgi) || (defined(sun) && OSMajorVersion >= 5)
GLOBAL char helpStyle[FSPECSIZ+1] = "$TAERES/wpthelp.res";
#else
#ifdef sun
GLOBAL char helpStyle[FSPECSIZ+1] = "$TAERES/wpthelp.res";
#else
GLOBAL TEXT helpStyle[FSPECSIZ+1] = "$TAERES/wpthelp.res";
#endif
#endif
#endif

/**********************************************************************/

WptHelpPanel::WptHelpPanel(WptPanel * theReqPanel, 
	WidgetAgent *theHelpObj, Boolean panelHelp, TEXT *helpSpec)

    {
    helpPanelId = nil;
    helpTarget = helpView = nil;

    if (NULLSTR(helpSpec)) 		// no help file name given
	{	
        WptPanelError((Id)theReqPanel,
                    WPT_WARNING, "No help available for %s.",
                    "WPT-NOHELPTEXT", theHelpObj->Name());
	goto error_exit;
	}

// Read the HelpPanel target and view symbol tables
// Update the table with proper FG/BG, Font and position information.

    SymbolTable *targetSt, *viewSt;
    int status;
    WptItem *closeIconId; 

    status = BuildSymbolTables(theReqPanel, theHelpObj, &targetSt, &viewSt);
    if (status != SUCCESS)
        goto error_exit;

    requestPanel = theReqPanel;
    helpObject = theHelpObj;
    helpTarget = targetSt;
    helpView = viewSt;

// Read help text from Help file spec.
    status = ReadHelpInfo(helpObject->Name(), viewSt, helpSpec, panelHelp); 
    if (status != SUCCESS)
        goto error_exit;

// Create and display the help panel.
    helpPanelId = new WptPanel("Help", helpTarget, helpView,     
        	(Window)nil, nil, (int)nil,
		XtDisplay(requestPanel->WidgetHandle()) );

    if (helpPanelId == nil)
	{ 
        WptPanelError((Id)theReqPanel, 
		     WPT_WARNING, "Could not display help panel.", 
                     "Wpt-BADPANEL");
        goto error_exit;
	}

    closeIconId = helpPanelId->GetItemId("CloseHelp");  // close btn 
    XtAddCallback(closeIconId->WidgetHandle(), XmNactivateCallback,
                (XtCallbackProc) ServiceCloseIcon, (XtPointer) this);
    return;

error_exit:
    delete this;
    }



/**********************************************************************/


// Delete the displayed panel. 
// Note: Here we remove the help panel Id from the request 
// Panel's helppanel list as the creator of help panel
// (Wpt) does not know when the help panel is being deleted.

// However: We don't delete the help panel view and symbol tables as 
// that is used repeatedly by WPT for helppanel displays.

WptHelpPanel::~WptHelpPanel()
    {
    if (helpPanelId) 
	{
        delete helpPanelId;				// Erase help panel 
        helpObject->helpPanelList.remove(this);		// delete from list
    	} 
    if (helpPanelTables)
	{
	helpPanelTables->Free();			// deallocate memory
	delete helpPanelTables;				
	}
    }


/**********************************************************************/

// Callback for the Close Icon (at release event) on the help panel.

static void ServiceCloseIcon(Widget widgetId,
                void * closure,  void * callData)
    {
    widgetId = widgetId;
    callData = callData;

    WptHelpPanel *helpPanel = (WptHelpPanel *) closure;	// from XtAddCallback() 
    delete helpPanel;				// remove the help instance 
    return;
    }



/****************************************************************************/

int  WptHelpPanel::BuildSymbolTables(WptPanel *reqPanel, 
	WidgetAgent *helpObj, SymbolTable **targetSt, 
	SymbolTable **viewSt)

    {
    SymbolTable *theHelpView; 
    SymbolTable *theHelpTarget;
    
    helpPanelTables = new SymbolTableList (helpStyle); 
    if ((*helpPanelTables).Error())
        {
        WptPanelError((Id)reqPanel, WPT_WARNING,
                      (*helpPanelTables).Error(), "WPT-OPENFILE");
	return (FAIL);
	}
    else
	{
        theHelpView = (*helpPanelTables).Find ("WptHelp_v");
        theHelpTarget = (*helpPanelTables).Find ("WptHelp_t");
        if (!theHelpView  || !theHelpTarget)
	    {
            WptPanelError((Id)reqPanel, WPT_WARNING,
                "Invalid Wpt Help resource File '%s'.", 
	 	"WPT-BADHELPRES", helpStyle); 
    	    return (FAIL); 
	    }	
	}

// Check validity of the target and view data checking for the Close button.
    if ((theHelpTarget->GetSymbol("CloseHelp") == nil) ||
       		(theHelpView->GetSymbol("CloseHelp") == nil))
	{
        WptPanelError((Id)reqPanel, WPT_WARNING,
                "Invalid Wpt Help resource File '%s.'",
                "WPT-BADHELPRES", helpStyle);
    	return (FAIL); 
 	}	

// Set the panel FG/BG/FONT/ORIGIN.
    Symbol *fgColor = theHelpView->GetSymbol("_PANEL.FG");
    if (fgColor && s_equal(fgColor->String(), "parent")) 
   	{
    	Symbol *fg = reqPanel->viewSt->GetSymbol("_PANEL.FG");
        if (fg && fg->Count() > 0 && (!NULLSTR (fg->String())))
            (*theHelpView)["_PANEL.FG"].Set(fg->String());
	}
	
    Symbol *bgColor = theHelpView->GetSymbol("_PANEL.BG");
    if (bgColor && s_equal (bgColor->String(), "parent"))
   	{
    	Symbol *bg = reqPanel->viewSt->GetSymbol("_PANEL.BG");
        if (bg && bg->Count() > 0 && (!NULLSTR (bg->String())))
            (*theHelpView)["_PANEL.BG"].Set(bg->String());
	}
	
    Symbol *fontName = theHelpView->GetSymbol("_PANEL.FONT");
    if (fontName && s_equal (fontName->String(), "parent"))
   	{
    	Symbol *font = reqPanel->viewSt->GetSymbol("_PANEL.FONT");
        if (font && font->Count() > 0 && (!NULLSTR (font->String())))
            (*theHelpView)["_PANEL.FONT"].Set(font->String());
	}
	
    Symbol *titleName = theHelpView->GetSymbol("_PANEL");
    if (titleName && NULLSTR(titleName->String()))
	{
    	Symbol *title = reqPanel->viewSt->GetSymbol("_PANEL");
        if (title && title->Count() > 0 && (!NULLSTR(title->String())))
	    {
	    TEXT helpTitle[STRINGSIZ+6];
	    s_copy (title->String(), helpTitle);
	    s_append (" Help", helpTitle);
            (*theHelpView)["_PANEL"].Set(helpTitle);
	    }
        else
            (*theHelpView)["_PANEL"].Set("Help");
	}

    int origin[2];
    Symbol *helpOrigin = theHelpView->GetSymbol("_PANEL.ORIGIN");
    origin[0] = helpOrigin->IntgVec()[0];
    origin[1] = helpOrigin->IntgVec()[1];
    if (origin[0] < 0 || origin[1] < 0)
	{
	// Don't use these x,y coords. Do relative positioning of help panel.
  	// Set it to the right of help object plus some offset. 
    	int origin[2];
    	Coord x1, x2, y1, y2;
    	Window child;
    	XWindowAttributes winInfo;
    	int root_x, root_y;

    	Display *dpy = XtDisplay(reqPanel->toplevel); 
    	Window reqWin = XtWindow(reqPanel->toplevel);
	Screen *scrn = XtScreen(reqPanel->toplevel);
    	XTranslateCoordinates(dpy, reqWin, RootWindowOfScreen(scrn),
	    	0, 0, &root_x, &root_y, &child);  

    	if ((WidgetAgent *)reqPanel == helpObj)		// help on panel	
	    {
	    // Ask server for size, it could have changed via the window mgr.
            XGetWindowAttributes(dpy, reqWin, &winInfo); // ask server for size
            origin[0] = root_x + winInfo.width + 10;
            origin[1] = root_y;
	    }
    	else						// help on parm 
	    {
	    // We know the item size (it can only change programmatically).
            helpObj->GetFullBox(x1, y1, x2, y2);	// we know the size
            origin[0] = root_x + x2 + 10; 
            origin[1] = root_y + y1;
	    }

	// We should assume there is no window manager running and ensure
	// that the entire panel is visible. So, based on help panel size
	// and the root window size, recalculate the origin if necessary.
	// NOTE: This code will be obsolete once we allow designers to easily
	// customize their help (including position info) on a per-panel basis.

    	XGetWindowAttributes(dpy, RootWindowOfScreen(scrn), &winInfo); 

    	int size[2];
    	Symbol *helpSize = theHelpView->GetSymbol("_PANEL.SIZE");
    	size[0] = helpSize->IntgVec()[0];
    	size[1] = helpSize->IntgVec()[1];

    	Symbol *border = theHelpView->GetSymbol("_PANEL.BORDER");
    	int borderWidth =  (border && border->Count() > 0) ? border->Intg() : 0;

    	if (winInfo.width < (origin[0] + size[0] + 2*borderWidth))
	    origin[0] = winInfo.width - (size[0] + 2*borderWidth +2);
    	if (winInfo.height < (origin[1] + size[1] + 2*borderWidth))
	    origin[1] = winInfo.height - (size[1] + 2*borderWidth +2);

    	// Make sure that the help panel origin is not negative.
    	if (origin[0] <= 0) origin[0] = 1;
    	if (origin[1] <= 0) origin[1] = 1;

        (*theHelpView)["_PANEL.ORIGIN"].Set(2, origin);
 	}

    *viewSt = theHelpView;
    *targetSt = theHelpTarget;
    return(SUCCESS);
    }


/************************************************************************/
int WptHelpPanel::ReadHelpInfo(TEXT *name, SymbolTable *viewSt, 
		TEXT *helpSpec, Boolean panelHelp)
    {

    struct  TXTSTOR *helpText;
    int	    numlines;

    // Get the help text to be displayed for the item. 
    helpText = WptHelpCreate(helpSpec, !panelHelp, name);
    if (helpText == (struct TXTSTOR *) (-1) )
	{
        WptPanelError ((Id)requestPanel, WPT_WARNING,
                       "Could not open help file %s",
                       "WPT-OPENFILE", helpSpec );
	goto errExit;
	}
    else if  (helpText == NULL || ((*helpText).numline == 0)) 	// no text 
	{
        WptPanelError ((Id)requestPanel, WPT_WARNING,
                       "No help available for %s",
                       "WPT-NOHELPTEXT", name );
	if (helpText != NULL)
            WptFreeHelp(helpText);		// free allocated memory  
	goto errExit;
	}

    //  Cut off at MAXVAL number of lines as v_maxc cannot go beyond that.
    // ******  TBD: Remove restriction on number of help lines.
     
    numlines = ((*helpText).numline <= MAXVAL) ? 
		(*helpText).numline : MAXVAL;

    viewSt->Set("HelpText.textstrs", numlines, (const TEXT**)(*helpText).tp);
    WptFreeHelp(helpText);		// Already saved in viewSt 
    return (SUCCESS);

errExit:
    return (FAIL);
    }
/*******************************************************************/


// The following functions should be in a separate module, but are
// kept here for convenience.

/**************************************************************************/

// Provide help for a Wpt panel.

void WptPanel::Helper()
    {
    Boolean panelHelp = TRUE;
    Symbol *helpSpec = viewSt->GetSymbol("_helpvu.helpfile");

    if (NULLSTR(helpSpec->String())) 		// no help file name given
	{
        WptPanelError((Id)this, WPT_WARNING,
              "No help available for this panel.", "WPT-NOHELPTEXT"); 
        EndHelpMode();		// Change back from Help mode
	return;
	}
	
    DisplayHelp(panelHelp, helpSpec->String(), this, this);
    return;
    }

/**************************************************************************/

// Provide help for a Wpt Item.

void WptItem::Helper()
    {
    Symbol *helpSpec = parentPanel->ViewSt()->GetSymbol("_helpvu.helpfile");
                                                      // no help file name given
    if ((helpSpec == NULL) || NULLSTR(helpSpec->String()))
	{	
        WptPanelError((Id)parentPanel, WPT_WARNING,
              "No help available for %s.", "WPT-NOHELPTEXT", Name()); 
        parentPanel->EndHelpMode();	// Change back from Help mode
	return;
	}

    DisplayHelp(FALSE, helpSpec->String(), this, parentPanel);
    return;
    }
    
/************************************************************************/

void WidgetAgent::DisplayHelp(Boolean panelHelp, TEXT *helpSpec, 
			WidgetAgent *helpObject, WptPanel *reqPanel)

    {
    WptHelpPanel   *helpPanel;
    helpPanel = new WptHelpPanel(reqPanel, helpObject, panelHelp, helpSpec);
    if (helpPanel != nil && helpPanel->HelpPanelId() != nil)
	helpPanelList.insert(helpPanel);

    reqPanel->EndHelpMode();		// Change back from Help mode
    return;
    }


/************************************************************************/
 
// Show that the panel is in  help mode: which implies 
// (a) highlight the help button, 
// (b) Change cursor to help cursor, and
// (c) change panel mode to HELP.	

void WptPanel::BeginHelpMode(WptItem *helpBtn)
    {
    if (GetMode() == HELP)		// already in help mode
	return;
	
    if (!helpBtn->HasComponents())
        helpBtn->ReverseColor();		// Highlight the button/icon
    XCursor helpCursor = 
	CreateHelpCursor(XtScreen(widgetId));   // new cursor in help mode
    ChangePanelCursor(helpCursor);		// use help cursor for panel
    SetMode(HELP);
    helpItemId = helpBtn;		// Id of selected help button 
    return;
    }


/**************************************************************************/

// User cancelled help, or help panel display completed. 
// Disable button select events from the panel background.

void WptPanel::EndHelpMode()
    {
    if (GetMode() != HELP)		// not in help mode
	return;
	
    if (!helpItemId->HasComponents())
        helpItemId->ReverseColor();	// Unhighlight the button/icon
    ChangePanelCursor((XCursor)nil);	// reset to default cursor 
    SetMode(OPERATE);
    helpItemId = nil;			// Id of selected help button 
    return;
    }


void PanelSelectCallback(Widget widgetId,
                void * panelData,  void * callData)
    {
    widgetId = widgetId;
    callData = callData;

    WptPanel * panelId = (WptPanel *) panelData;
    if (panelId->GetMode() == HELP)
        panelId->Helper();
    return;
    }


/************************************************************/ 
// Callback for an item designated to provide help

void HelpItemCallback(Widget widgetId, 
  		void * itemData,  void * callData) 
    {
    widgetId = widgetId;
    callData = callData;

    WptItem  *helpItemId = (WptItem *) itemData; 
    WptPanel  *reqPanel = helpItemId->ParentPanel();

    if (reqPanel->GetMode() == OPERATE)		// not already in help mode
        reqPanel->BeginHelpMode(helpItemId);    // indicate HELP mode to user 

    else if (reqPanel->GetMode() == HELP)	// already in help mode
        reqPanel->EndHelpMode();    		// Set to Operate mode

    return;
    }

/**************************************************************************/

static FUNCTION  XCursor  CreateHelpCursor(Screen *screen)

    {
#define	CURSOR_WIDTH	16
#define	CURSOR_HEIGHT	16

static unsigned char HelpCursorImage[] = 
   {
   0xfc, 0x3f, 0x02, 0x40, 0xe1, 0x87, 0x71, 0x8e, 
   0x71, 0x9c, 0x21, 0x9c, 0x01, 0x8e, 0x01, 0x87,
   0x81, 0x81, 0x81, 0x80, 0x01, 0x81, 0x01, 0x80,
   0x81, 0x81, 0x81, 0x81, 0x02, 0x40, 0xfc, 0x3f,
   0x00,
   };

static unsigned char HelpCursorMask [] =
   {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0x00,
   };
    XCursor	helpCursor;

    helpCursor = X_CreateCursor(screen, CURSOR_WIDTH, CURSOR_HEIGHT,
                (char *)HelpCursorImage, (char *)HelpCursorMask, 8, 8, // Hot spot (x, y)
                GXcopy);
    return (helpCursor);
    }
