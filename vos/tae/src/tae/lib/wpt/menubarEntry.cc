/******************************************************************************
 *	Copyright (c) 1990, 1991, 1992,
 *	National Aeronautics and Space Administration
 *	ALL RIGHTS RESERVED
 *
 *	The software (programs, data bases and/or documentation) on or in
 *	any media can not be reproduced, disclosed or used except under
 *	terms of the license between COSMIC and your organization.
 *****************************************************************************/

/***********************************************************************
 *
 * menubarEntry.cc  This module contains all member functions for class
 * MenuEntry, that are referenced by the menubarRt.cc file.
 * This module defines all entries in a menubar.
 * 
 * Change Log:
 *
 * 19-apr-92	Initial Creation...krw
 * 19-apr-92	If change FG, must update border and shadow colors too...krw
 * 11-may-92	Proper casting of warning handler Pr1410...tpl
 * 31-jul-92	PR1466: If we are running the WorkBench, we must disable 
 *		MenuIcon caching. Otherwise, the designer may not see his 
 *		changes to the bitmap file...krw
 * 05-oct-92	Fix C++ anachronisms...rt
 * 19-oct-92	PR1695: Some functions needed to be declared void to remove some
 *		compiler warnings...krw
 * 19-mar-93	nonexistent passive grab message for X11R5...krw/rt
 * 19-mar-93	HP compiler is picky about returning a value...rt
/*************************************************************************/

#include <menubarentry.h>
#include <resconvert.h>

//**************************
// The following define is for an appearent problem with MOTIF. There
// is a bug when some of the entries in a cascade menu have justification
// set to something other than "left". They will always appear left justified
// whether you like it or not. To get around it, we have to call XtSetValues
// on the choice widgets right after creating them...seems absurd.
// This is so far true up through Motif v1.1.3
//

#define MOTIF_JUSTIFY_PROBLEM 0

//**************************

	// from menubarRt.cc	 for inheritance
extern XtArgVal mb_item_fg;             // color of menubar item foreground
extern XtArgVal mb_item_bg;             // color of menubar item background
extern XtArgVal mb_item_font;           // font of menubar item
extern XtArgVal mb_panel_fg;            // color of menubar panel foreground
extern XtArgVal mb_panel_bg;            // color of menubar panel background
extern XtArgVal mb_panel_font;          // font of menubar panel
extern Screen	*mb_screen;		// Screen of item.

extern "C" CODE  ReadBitmapFile(TEXT *, int *,int *,short **,int *,int *);
extern BOOL IsWorkBench;		// TRUE if this is the WorkBench

// Common ENTRY Resources
static VmResource mnemonic[1] = {"mnemonic", 0};
static VmResource accelerator[1]   = {"accelerator", 0};
static VmResource state[1]  = {"state", 0};
static VmResource justification[1] = {"justification", 0};
static VmResource foreground[1]  = {"fg", 0};
static VmResource background[1]  = {"bg", 0}; 
static VmResource font[1] = {"font", 0}; 
static VmResource none[1] = {"", -1};		// no qualifier needed 

static VmToWptArg commonEntryArgs[] = {

/*   ConverFunc    XtName        vmResource  vmResCount  defaultVal */ 
    {XmStringConvert,XmNlabelString,	none,	0, (void *) " "},
    {KeySymConvert,XmNmnemonic,    mnemonic,	1, (void *) 0},
    {XmStringQualConvert,XmNacceleratorText,    accelerator, 1, (void *) ""},
    {AccelConvert,XmNaccelerator, accelerator,	1, (void *) ""},
    {AlignConvert,XmNalignment, justification,	1, (void *) "left"},
    {ColorConvert, XmNforeground,  foreground,	1,   (void *) WPT_DEF_FG },
    {ColorConvert, XmNbackground,  background,	1,   (void *) WPT_DEF_BG },
    {FontListConvert, XmNfontList,	font,	1,     (void *) WPT_DEF_FONT},
    {ColorConvert, XmNborderColor,  foreground,	1,   (void *) WPT_DEF_FG },
    {IntgConvert, XmNtraversalOn,	none,	0,  (void *) True},
    {IntgConvert, XmNhighlightThickness, none,	0, (void *) 2},
    {IntgConvert, XmNsensitive,		state,	1, (void *) TRUE},
};

#define FGARG 5
#define BGARG 6
#define FONTARG 7
#define BDRARG 8
static COUNT NUM_COMMON_MENUENTRIES = sizeof(commonEntryArgs)/sizeof(VmToWptArg);

#ifndef NO_MOTIF_PASSIVE_GRAB_BUG
// WarningHandler is set to true when we have installed the Xt Warning
// handler the first time. We don't want to keep doing that.
// This is a kludge for MOTIF Passive Grab warnings.
static BOOL WarningHandler = FALSE;
#endif

MenuEntry::MenuEntry(Widget parent, CascadeEntry *cascade,
			Symbol* viewSym, Symbol* targetSym)
{
parentCascade = cascade;
name = s_save(viewSym->Name());
BldCommonEntryArgs(viewSym, targetSym, parent);
}

MenuEntry::~MenuEntry()
{
if (mainWidget) XtDestroyWidget(mainWidget);
if (name) tae_free(name);
FreeCommonArgList();
}


// Somewhat analogus to BldWptArgList for WptItems.
// These are the common args for all MenuEntry objects.
int MenuEntry::BldCommonEntryArgs (Symbol *viewSym, Symbol *targetSym,
                Widget parentWidget)
{
   TEXT *fg, *bg, *font;
   argCnt = NUM_COMMON_MENUENTRIES;
   WptEntryArgs = (WptArg **)tae_alloc( argCnt + 1, sizeof(WptArg *) );
					// Remember NULL arg
   for (int i = 0; i < NUM_COMMON_MENUENTRIES; i++)
	{
        WptEntryArgs[i] = commonEntryArgs[i].converter(&commonEntryArgs[i], 
			viewSym, targetSym, parentWidget);
        if (WptEntryArgs[i] == NULL)	// invalid target or view
	    {
    	    WptError(XtScreen(parentWidget), WPT_WARNING, 
		"Could not create Menubar item %s, bad attribute %s.\n",
 	       "WPT-BADVIEW", viewSym->Name(), commonEntryArgs[i].SymbolName());
    	    FreeCommonArgList();
    	    return 0;
	    }
	}

		// Color and Font inheritance
// FG
    fg = (*viewSym)["fg"].String();
    if (STR_EQUAL(fg, "parent") || STR_EQUAL(fg, "parent panel"))
	{
	WptEntryArgs[FGARG]->SetValue(mb_panel_fg);
	WptEntryArgs[BDRARG]->SetValue(mb_panel_fg);
	}
    else if (STR_EQUAL(fg, "item foreground"))
	{
	WptEntryArgs[FGARG]->SetValue(mb_item_fg);
	WptEntryArgs[BDRARG]->SetValue(mb_item_fg);
	}
    else if (STR_EQUAL(fg, "parent cascade"))
	{
	XtArgVal foreground = (parentCascade) ? parentCascade->fg : mb_item_fg;
	WptEntryArgs[FGARG]->SetValue(foreground);
	WptEntryArgs[BDRARG]->SetValue(foreground);
	}

// BG
    bg = (*viewSym)["bg"].String();
    if (STR_EQUAL(bg, "parent") || STR_EQUAL(bg, "parent panel"))
	{
	WptEntryArgs[BGARG]->SetValue(mb_panel_bg);
	}
    else if (STR_EQUAL(bg, "item background"))
	{
	WptEntryArgs[BGARG]->SetValue(mb_item_bg);
	}
    else if (STR_EQUAL(bg, "parent cascade"))
	{
	WptEntryArgs[BGARG]->SetValue(
		(parentCascade) ? parentCascade->bg : mb_item_bg);
	}

// FONT
    font = (*viewSym)["font"].String();
    if (STR_EQUAL(font, "parent") || STR_EQUAL(font, "parent panel"))
	{
	WptEntryArgs[FONTARG]->SetValue(mb_panel_font);
	}
    else if (STR_EQUAL(font, "item font"))
	{
	WptEntryArgs[FONTARG]->SetValue(mb_item_font);
	}
    else if (STR_EQUAL(font, "parent cascade"))
	{
	WptEntryArgs[FONTARG]->SetValue(
		(parentCascade) ? parentCascade->font : mb_item_font);
	}

    WptEntryArgs[i] = NULL;		// null terminated list
    return 1;
}


void MenuEntry::FreeCommonArgList()
{
    if (WptEntryArgs == nil)
        return;

    for (FAST i=0; WptEntryArgs[i] != NULL; ++i)
	delete WptEntryArgs[i]; 	// free each individual member

    tae_free(WptEntryArgs);
    WptEntryArgs = nil;
}


/***********************************************************************/
/*
 * Build the widget arglist by combining :
 *      1 - widget resources derived from the target/view info,
 *      2 - widget's constraint resources (common to all items)
 *      3 - Any default resources determined by the item
 *
 * NOTE: this routine should mimic very closely the WptItem::BldXtkArgList
 *	routine. Any changes to that routine should be reflected here.
 *	We couldn't use that routine here because MenuEntry objects are
 *	not WptItem objects.
 */

int MenuEntry::BldXtkArgList(Symbol *viewSym, Symbol *targetSym,
		Widget parentWidget, XtkArg ** xtkArgs)

{
    XtkArg*  argList;
    FAST  i;

// Special handling for monochrome displays to guarantee that fg and
// bg are never the same for any item or panel.
    short int    IsMonochrome;
    XtArgVal     XTK_BLACK_PIXEL;
    XtArgVal     XTK_WHITE_PIXEL;

    if (DefaultDepthOfScreen(mb_screen) <= 2 )
        {
        IsMonochrome = TRUE ;
        XTK_BLACK_PIXEL = BlackPixelOfScreen (mb_screen);
        XTK_WHITE_PIXEL = WhitePixelOfScreen (mb_screen);
        }
    else
        IsMonochrome = FALSE ;

    argList = (XtkArg *) tae_alloc(argCnt+SpecificArgCnt(), sizeof(XtkArg));
    if ( IsMonochrome && 
	(WptEntryArgs[FGARG]->Value() == WptEntryArgs[BGARG]->Value()))
	{
	WptEntryArgs[BGARG]->SetValue( 
			(WptEntryArgs[BGARG]->Value() == XTK_WHITE_PIXEL) ?
			XTK_BLACK_PIXEL : XTK_WHITE_PIXEL);
	}

    for (i = 0; i < argCnt && WptEntryArgs[i]; i++)
        {
        argList[i].name = WptEntryArgs[i]->Name();
        argList[i].value = WptEntryArgs[i]->Value();
        }

// Add any extra XtkArgs for the entry.
    BldSpecificXtkArgs (viewSym, targetSym, parentWidget, argList);

    *xtkArgs = argList;
    return (argCnt+SpecificArgCnt());
}

void MenuEntry::LoadTargetValue(Symbol *target)
{
target->Set(name);
Load_Select(target);
}

void MenuEntry::Load_Select(Symbol *target)
{
TEXT **valids;
COUNT validCnt;

//Update "_SELECT" based on the index of name
// in the target's valid list
// Note: the is only one "_select" qualifier for the menubar. Its value
// reflects the index in the valid list, of the name of the last 
// selected leaf entry.
target->GetValid(&validCnt, &valids);
for (TAEINT i=0; i < validCnt; ++i)
    {
    if (STR_EQUAL(name, valids[i]))
	{
	target->Set("_SELECT", i);
	return;
	}
    }
target->Set("_SELECT", -1);		// name not found...should not happen
printf ("Design Error: MenubarEntry::Load_Select, entry name not found in valids (%s)\n",
    name);
}

// Called from UpdateView...note name and entrytype cannot change
void MenuEntry::UpdateEntryView(Widget parent, Symbol* viewSym, 
				Symbol* targetSym)
{
BldCommonEntryArgs(viewSym, targetSym, parent);
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

XtSetValues (mainWidget, xtkArgs, xtkArgCnt); //Change Look of entry

Finish(targetSym, viewSym);
FreeCommonArgList();
tae_free (xtkArgs);
}

// C A S C A D E
// The following event handler is required by the WorkBench. Normally
// Cascades ignore motion notify events. However, the WorkBench needs
// them for item dragging. This event handle lets them through.
extern "C" void CascadeMotionEventHandler(Widget widget, 
		XtPointer clientData, XEvent *event, 
		Boolean *continue_to_dispatch)
{
widget = widget;
clientData = clientData;
event = event;
*continue_to_dispatch = False;				// do nothing.
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
// return NULL;    /* I commented this line out.  -steve h. */
#endif
}

#ifndef NO_MOTIF_PASSIVE_GRAB_BUG
// This following Kludge is used to suppress the Motif bug with "passive grab"
// warnings. We get the warging here due to the addition of the
// EventHandler for ButtonMotion.
extern "C" void CascadeXtWarningHandler(String message) 
{
#ifdef TAEX11R4
if (!s_substring("non-existant passive grab", message)) // skip passive grab
#else
if (!s_substring("nonexistent passive grab", message)) // skip passive grab
#endif
    printf ("%s\n", message);			// All other messages
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
// return NULL;    /* I commented this line out.  -steve h. */
#endif
}
#endif

CascadeEntry::CascadeEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
pulldownWidget = XmCreatePulldownMenu(parent, viewSym->Name(),
	xtkArgs, xtkArgCnt);

TEXT cascadeName[2*NAMESIZ+1];

s_copy(viewSym->Name(), cascadeName);
s_append("_CB", cascadeName);

mainWidget = XmCreateCascadeButton (parent, 
		cascadeName, xtkArgs, xtkArgCnt);

XtSetArg (xtkArgs[0], XmNsubMenuId, pulldownWidget);
XtSetValues (mainWidget, xtkArgs, 1);	// Attach the submenu to the cascade button

XtManageChild (mainWidget);

// Save fg, bg and font for subEntry inheritance
fg	= WptEntryArgs[FGARG]->Value();
bg	= WptEntryArgs[BGARG]->Value();
font	= WptEntryArgs[FONTARG]->Value();

FreeCommonArgList();
tae_free (xtkArgs);

// Enable Motion notify events for cascade widgets...the workbench needs this.
XtAddEventHandler(mainWidget, ButtonMotionMask, FALSE,
        (XtEventHandler)CascadeMotionEventHandler, (XtPointer) this);

#ifndef NO_MOTIF_PASSIVE_GRAB_BUG
if (!WarningHandler)			// Kludge for Xt passive grap warnings
    {
    XtSetWarningHandler ((XtErrorHandler)CascadeXtWarningHandler);
    WarningHandler = TRUE;
    }
#endif
}

CascadeEntry::~CascadeEntry()
{
// The following three lines of code are from a suggestion on Motif-talk
// about some cleanup which is not done when the cascade button is
// deleted. Setting the subMenuId to NULL is supposed to perform this cleanup.
XtkArg xtkArgs[1];
XtSetArg (xtkArgs[0], XmNsubMenuId, NULL);
XtSetValues (mainWidget, xtkArgs, 1);	// Detach submenu from the cascade

if (pulldownWidget) XtDestroyWidget(pulldownWidget);
}

// Called from UpdateView...note name and entrytype cannot change
// Slightly revised version  of the MenuEntry method of the same name.
// We need to update the Pulldown widget too.
void CascadeEntry::UpdateEntryView(Widget parent, Symbol* viewSym, 
				   Symbol* targetSym)
{
BldCommonEntryArgs(viewSym, targetSym, parent);
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

XtSetValues (pulldownWidget, xtkArgs, xtkArgCnt);
XtSetValues (mainWidget, xtkArgs, xtkArgCnt);	//Change Look of entry

// Save fg, bg and font for subEntry inheritance
fg	= WptEntryArgs[FGARG]->Value();
bg	= WptEntryArgs[BGARG]->Value();
font	= WptEntryArgs[FONTARG]->Value();

FreeCommonArgList();
tae_free (xtkArgs);

// Handle shadow colors

Pixel topshadow,bottomshadow,selectcolor,foreground;
COUNT j = 0;
Colormap cmap;
Arg temparg[1];
XtkArg shadowArgs[4];

XtSetArg ( temparg[0], XmNcolormap, &cmap );
XtGetValues ( mainWidget, temparg, 1 );

XmGetColors ( XtScreen(mainWidget), cmap,
              bg, &foreground,
              &topshadow,&bottomshadow, &selectcolor );

XtSetArg(shadowArgs[j],XmNtopShadowColor, (XtArgVal)topshadow); j++;
XtSetArg(shadowArgs[j],XmNbottomShadowColor, (XtArgVal)bottomshadow); j++;
XtSetArg(shadowArgs[j],XmNselectColor, (XtArgVal)selectcolor); j++;
XtSetArg(shadowArgs[j],XmNarmColor, (XtArgVal)selectcolor); j++;

XtSetValues (pulldownWidget, shadowArgs, j);
XtSetValues (mainWidget, shadowArgs, j);	//Change Look of entry
    
//

#ifndef NO_MOTIF_CASCADE_ARROW_BUG
// A kludge to get around a bug in MOTIF... fixed in v1.2???
// This should properly update the color of the cascade arrow button
XtkArg kludgeArgs[2];
XtSetArg (kludgeArgs[0], XmNsubMenuId, NULL);
XtSetArg (kludgeArgs[1], XmNcascadePixmap, XmUNSPECIFIED_PIXMAP);
XtSetValues (mainWidget, kludgeArgs, 2);// Detach submenu from the cascade
XtSetArg (kludgeArgs[0], XmNsubMenuId, pulldownWidget);
XtSetValues (mainWidget, kludgeArgs, 1);	// Now reattach it
#endif 
}


// H E L P
HelpEntry::HelpEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym) 
	: CascadeEntry(parent, cascade, viewSym, targetSym)
{
XtkArg helpArg;

//	Define this menu as a Help menu
XtSetArg (helpArg, XmNmenuHelpWidget, mainWidget);
XtSetValues (parent, &helpArg, 1);
}

// L A B E L
LabelEntry::LabelEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
mainWidget = XmCreateLabel (parent, viewSym->Name(), xtkArgs, xtkArgCnt);

XtManageChild (mainWidget);

#ifdef MOTIF_JUSTIFY_PROBLEM
XtSetValues (mainWidget, xtkArgs, xtkArgCnt);
#endif

FreeCommonArgList();
tae_free (xtkArgs);
}


// I C O N
IconEntry::IconEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

pixmap = 0;
COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
mainWidget = XmCreatePushButton (parent, viewSym->Name(), xtkArgs, xtkArgCnt);

XtManageChild (mainWidget);

#ifdef MOTIF_JUSTIFY_PROBLEM
XtSetValues (mainWidget, xtkArgs, xtkArgCnt);
#endif

FreeCommonArgList();
tae_free (xtkArgs);
}


// Add icon specific XtkArgs
void IconEntry::BldSpecificXtkArgs (Symbol *viewSym, Symbol *targetSym,
		Widget parent, XtkArg * xtkArgs)
{
FSBLOCK fsblock;
TEXT full_iconFilename[STRINGSIZ+1], errstr[STRINGSIZ+1];
CODE expandOk;

if (pixmap) FreePixmap();		// we may be changing the pixmap

// The following code will properly handle environment parameters (UNIX)
// or logical names (VMS) in file names.
expandOk = f_crack ((*viewSym)["filename"].String(), "", "", "", 
		&fsblock, errstr);
if (expandOk)
    {
    f_spec (&fsblock, full_iconFilename);
    if (IsWorkBench)
	{			// WorkBench must not have pixmap caching
	short	*data = nil;
	int	width, height, xhot, yhot;
	CODE	code;
	code = ReadBitmapFile (full_iconFilename, &width, &height,
		&data, &xhot, &yhot);
	if (code == 1)
	    {
	    pixmap = XCreatePixmapFromBitmapData (DisplayOfScreen (mb_screen),
		XRootWindowOfScreen(mb_screen), (char *)data, 
		(unsigned int)width, (unsigned int)height, 
		(unsigned long)xtkArgs[FGARG].value, 
		(unsigned long)xtkArgs[BGARG].value, 
		DefaultDepthOfScreen(mb_screen));
	    tae_free (data);
	    }
	else
    	    pixmap = XmUNSPECIFIED_PIXMAP;	// pixmap was bad
	}
    else
	{			// Normal, Motif pixmap caching
        pixmap = XmGetPixmap (mb_screen, full_iconFilename,
		xtkArgs[FGARG].value, xtkArgs[BGARG].value);
	}
    }
else
    {
    pixmap = XmUNSPECIFIED_PIXMAP;	// pixmap not found
    printf ("Menubar Icon file (%s) could not be found, error:\n",
		(*viewSym)["filename"].String());
    printf ("%s\n", errstr);
    }

int i = argCnt;
XtSetArg (xtkArgs[i], XmNlabelType, XmPIXMAP); i++;
XtSetArg (xtkArgs[i], XmNlabelPixmap, pixmap);
}


IconEntry::~IconEntry()
{
FreePixmap();
}


void IconEntry::FreePixmap()
{
if (pixmap && pixmap != XmUNSPECIFIED_PIXMAP)
    {
    if (IsWorkBench)
	XFreePixmap (DisplayOfScreen (mb_screen), pixmap);
    else
	XmDestroyPixmap (mb_screen, pixmap);
    }
pixmap = 0;
}

    

// P U S H B U T T O N
PushButtonEntry::PushButtonEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
mainWidget = XmCreatePushButton (parent, viewSym->Name(), xtkArgs, xtkArgCnt);

XtManageChild (mainWidget);

#ifdef MOTIF_JUSTIFY_PROBLEM
XtSetValues (mainWidget, xtkArgs, xtkArgCnt);
#endif

FreeCommonArgList();
tae_free (xtkArgs);
}


// S E P A R A T O R
SeparatorEntry::SeparatorEntry(Widget parent, CascadeEntry *cascade,
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
mainWidget = XmCreateSeparator (parent, viewSym->Name(), 
		xtkArgs, xtkArgCnt);

XtManageChild (mainWidget);

FreeCommonArgList();
tae_free (xtkArgs);
}


// Add icon specific XtkArgs
void SeparatorEntry::BldSpecificXtkArgs (Symbol *viewSym, Symbol *targetSym,
		Widget parent, XtkArg * xtkArgs)
{
unsigned char sepType;
TEXT *separator = viewSym->String();

sepType = (STR_EQUAL(separator, "Single Line")) ? XmSINGLE_LINE :
          (STR_EQUAL(separator, "Double Line")) ? XmDOUBLE_LINE :
          (STR_EQUAL(separator, "Single Dashed Line")) ? XmSINGLE_DASHED_LINE :
	  (STR_EQUAL(separator, "Double Dashed Line")) ? XmDOUBLE_DASHED_LINE :
          (STR_EQUAL(separator, "Shadow Etched In")) ? XmSHADOW_ETCHED_IN :
          (STR_EQUAL(separator, "Shadow Etched Out")) ? XmSHADOW_ETCHED_OUT :
           XmNO_LINE;			// No Line is the default

int i = argCnt;
XtSetArg (xtkArgs[i], XmNseparatorType, sepType); i++;
XtSetArg (xtkArgs[i], XmNshadowThickness, (*viewSym)["septhickness"].Intg());
}

// C H E C K B O X
CheckboxEntry::CheckboxEntry(Widget parent, CascadeEntry *cascade, 
	Symbol* viewSym, Symbol* targetSym)
	: MenuEntry(parent, cascade, viewSym, targetSym)
{
XtkArg *xtkArgs;

COUNT xtkArgCnt = BldXtkArgList (viewSym, targetSym, parent, &xtkArgs);

// First create the pulldown menu widget
mainWidget = XmCreateToggleButton (parent, viewSym->Name(), xtkArgs, xtkArgCnt);

Finish(targetSym, viewSym);
XtManageChild (mainWidget);

#ifdef MOTIF_JUSTIFY_PROBLEM
XtSetValues (mainWidget, xtkArgs, xtkArgCnt);
#endif

FreeCommonArgList();
tae_free (xtkArgs);
}


void CheckboxEntry::Finish(Symbol *targetSym, Symbol *viewSym)
{ 		// null or no value is the same as "no"
Symbol *cbVal = &(*targetSym)[(*viewSym).Name()];
TEXT *tbState = (cbVal && cbVal->Count() >0) ? cbVal->String() : "no";

XmToggleButtonSetState (mainWidget, ((s_equal(tbState, "yes")) ? TRUE :
	FALSE), FALSE);
}

// Checkbox sets value(0) to the entry name...like all other entries
// But also sets value(1) to the checkbox value..."yes" or "no"
// It must also update the associated qualifier in the target so the
// target can remember the value.
// Note: values returned are internal pointers. 
//	They should NOT be freed by the caller.
void CheckboxEntry::LoadTargetValue(Symbol *target)
{
TEXT *newValues[2];

newValues[0] = name;
newValues[1] = (XmToggleButtonGetState(mainWidget)) ? "yes" : "no";
if (target->MaxCount() < 2) target->SetMaxCount(2); // make sure room
target->Set(2, (const TEXT**)newValues);
			// Also set qualifier!
(*target)[name].Set(newValues[1]);
Load_Select(target);	// Set _Select too.
}
