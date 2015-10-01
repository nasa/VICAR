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
 * scaleRt.c.  This module contains all member functions for class
 * ScaleA (subclass of WptItem), that are referenced by an application
 * at run time in OPERATE mode.  All MANIPULATE type functions (if any), that
 * override the corresponding virtual functions in WptItem should be
 * in a separate module (scaleWb.c). 
 *
 * 
 * CHANGE LOG:
 *
 * 20-apr-90	created for MOTIF based on checkboxRt.cc ...kbs
 *		CAUTION: if MOTIF is undef, all bets are off (TBD: VMS,HP)...kbs
 * 24-apr-90	re-write DispatchCallbacks, BldWptArgsList...kbs
 * 30-apr-90	added UpdateView for widget-specific resources...kbs
 * 30-apr-90	added DisplayNewValue ...kbs
 * 22-may-90	fixes to BldWptArgsList, DisplayNewValue...kbs
 * 23-may-90	reworking of UpdateView, no wptArgs needed...kbs
 * 13-aug-90	deleted all Help functions and SetState...kbs
 * 13-aug-90	added shadowThickness, scaleMultiple, scaleWidth &
 *		scaleHeight...kbs
 * 18-sep-90	Motif 1.1 public: do scaleWidth & scaleHeight as
 *		sliderThickness...kbs
 * 21-sep-90	added SetState for dimming BUT DOESN'T WORK MOTIF BUG 2512...kbs
 * 25-sep-90	SetState now calls XtSetSensitive; workaround for
 *		 MOTIF BUG P2101...kbs
 * 04-oct-90	fix BldWptArgList to set initial value for Set Defaults
 *		 Mode...kbs,krw
 * 08-oct-90	changed min, max, and multiple to reals...kbs
 * 01-nov-90	changed VM resource "shadowThickness" to "shadow"...kbs
 * 07-nov-90	force XmNdecimalPoints to zero for integer scales...kbs,krw
 * 21-feb-91	HP port for 5.0...ljn
 * 18-mar-91	min, max names changed to not conflict with InterViews...ljn
 * 10-may-91    part of PR926...tpl
 * 08-aug-91    Apollo port...tpl */
// 16-aug-91	fix #ifdef EXTRA_DEBUG (not col 1), remove extra cb...cew,ljn
// 06-jan-92	PR1257: Changed #include filenames to use <>...ljn
// 10-jan-92    Added call to BldTraversalArgs...tpl
// 07-feb-92	Fixed UpdateView to change scrollbar troughColor...kbs
// 18-feb-92	SetState method moved to WptItem w/SetStatePropogate...cew
// 30-mar-92	Multiple Display support.  Pass panelId to WptItem ctor...cew
// 08-apr-92	Memory Leak in UpdateView. Need to free the title string
//		value after setting the widget...krw
// 25-apr-92    Check for zero width/heigh PR1095...tpl
// 05-oct-92	Fix C++ anachronisms and fix up comment lines more than 80...rt
// 19-mar-93	HP compiler does not like some kinds of initialization...rt
/*************************************************************************/

#include <scaleitem.h>
#include <resconvert.h>

#include <Xm/ScrollBar.h>  // for XmIsScrollBar

extern   XtAppContext   _wptAppContext;
extern "C"
{
void  WptXmCallParentProcess ( Widget w , XEvent * event );
}


//	TABLE of Motif-specific SCALE WIDGET RESOURCES
//	----------------------------------------------
// !  Motif RESOURCE		WHERE HANDLED
// !
// !  XmScale-specific resources
// decimalPoints	Scale Presentation Panel or DefaultArgs()
// dragCallback		DispatchCallbacks()
// fontList		Item Specification Panel
// highlightOnEnter	global Motif issue or DefaultArgs()
// highlightThickness	global Motif issue or DefaultArgs()
// maximum		Scale Presentation 
// minimum		Scale Presentation 
// orientation		Scale Presentation 
// processingDirection	Scale Presentation 
// scaleHeight		Scale Presentation
// scaleWidth		Scale Presentation
// scaleMultiple	Scale Presentation
// showValue		Scale Presentation 
// titleString		Item Specification 
// traversalOn		global Motif issue ??
// value		Scale Presentation (initial); DisplayNewValue() updates
// valueChangedCallback	DispatchCallbacks()
// !
// ! from XmManager
// bottomShadowColor	global Motif issue or DefaultArgs()
// bottomShadowPixmap	global Motif issue or DefaultArgs()
// foreground		Item Specification 
// helpCallback		global Motif issue
// highlightColor	global Motif issue or DefaultArgs()
// highlightPixmap	global Motif issue or DefaultArgs()
// shadowThickness	Item Specification
// topShadowColor	global Motif issue or DefaultArgs()
// topShadowPixmap	global Motif issue or DefaultArgs()
// unitType		global Motif issue
// userData		global Motif issue
// !
// ! from Composite
// insertPosition	global Motif issue
// !
// ! from Core
// sensitive		Scale Presentation or GenerateEvents in Item Spec
// accelerators		global Motif issue
// ancestorSensitive	global Motif issue
// background		Item Specification 
// backgroundPixmap	global Motif issue
// borderColor		global Motif issue or DefaultArgs()
// borderPixmap		global Motif issue
// borderWidth		Item Specification
// colormap		global Motif issue
// depth		global Motif issue
// destroyCallback	global Motif issue
// height		direct manipulation in WB
// mappedWhenManaged	global Motif issue
// screen		global Motif issue
// translations		global Motif issue
// width		direct manipulation in WB
// x			direct manipulation in WB
// y			direct manipulation in WB


static ScaleP  TheScalePres;		// only one instance

static void ScaleSelectCallback(Widget widgetId, void * closure, 
		void * callData);

static void ScaleReleaseCallback(Widget widgetId, void * closure, 
		void * callData);



	// INCLUDE ALL RESOURCES WHICH WE ALLOW USER TO CHANGE (eg, thru WB)
	// 0 or 1 is index into vector

	// CAUTION: If you add/delete from this resource list,
	//          please also modify the UpdateView() func.


// GENERAL (CommonViewArgs) 

static VmResource xOffset[1] 		= {"origin", 0};
static VmResource yOffset[1] 		= {"origin", 1};
static VmResource width[1]   		= {"size", 0};
static VmResource height[1]  		= {"size", 1};

static VmResource borderWidth[1] 	= {"border", 0};
static VmResource shadow[1] 		= {"shadow", 0};

static VmResource foreground[1]  	= {"fg", 0};
static VmResource background[1]  	= {"bg", 0}; 
static VmResource font[1] 		= {"font", 0}; 

// SCALE SPECIFIC

// NOTE: sliderThickness is TAE-ism, for scaleWidth/scaleHeight

static VmResource sliderThickness[1] 	= {"sliderThickness", 0}; // gripper dim.
// static VmResource scaleWidth[1] 	= {"scaleWidth", 0}; // gripper dim.
// static VmResource scaleHeight[1] 	= {"scaleHeight", 0}; // gripper dim.

static VmResource scaleMultiple[1] 	= {"scaleMultiple", 0}; // increment
static VmResource showValue[1] 		= {"showValue", 0}; // display numeric val
static VmResource sensitive[1] 		= {"sensitive", 0}; 
static VmResource direction[1] 		= {"direction", 0}; // normal/reverse
static VmResource orientation[1] 	= {"orientation", 0};  // horiz/vertical
static VmResource decimalPts[1] 	= {"decimalPts", 0};   // non-0 is float
	// If we handle min and max by Constraints, we don't need these two:
static VmResource minimum[1] 		= {"minimum", 0}; 
static VmResource maximum[1] 		= {"maximum", 0}; 
	// static VmResource value[1] 	// handled by target
static VmResource none[1] 		= {"", -1};    // no qualifier needed 

static void checkdefault ( Widget w, XEvent *event );

static XtActionsRec newaction[] =
                        {
                        {"CheckDefault", (XtActionProc) checkdefault},
                        };
static char *newtb = {
      "<Key>osfActivate:  CheckDefault()\n\
       <Key>Return:       CheckDefault()"
};
static XtTranslations ScaleTB = NULL;  // binary version of newtb


static void checkdefault( Widget w,XEvent *event)
{

 if(    XtIsSensitive( w )  )
                {
                WptXmCallParentProcess ( w, event );
                }

}



//****************************************************************************

WidgetAgent *ScaleP::MakeWidgetAgent(TEXT *name, WptPanel *panelId, 
		Symbol *target, Symbol *view)
{
    ScaleA *aScale;
    aScale = new ScaleA(name, panelId, target, view);
    return ((WidgetAgent *) aScale);
}


ScaleA::ScaleA (TEXT * name, WptPanel *panelId, 
		Symbol* targetSym, Symbol* viewSym) : WptItem(panelId)

{
   static    Boolean NOTADDED = True;
   XtkArg    *argList;
   int	     numArgs;
  
   sentinel = SCALE_SENTINEL; 
   wptArgs = nil;  argList = nil;

   if ( NOTADDED )
        {
        XtAppAddActions ( _wptAppContext, newaction,  XtNumber(newaction) );
        NOTADDED = False;
        }

   // Build arg list info from WPT target and view data
   int status = BldWptArgList(targetSym, viewSym, panelId->GetWidgetId());
   if (! status)
	{
	delete this;
	return;
	}

   // Prepare for widget creation
   BldTraversalArgs ( viewSym, panelId, DefaultTraversalState() );
   numArgs = BldXtkArgList(panelId->GetWidgetId(), &argList);  // Arglist for widget 

#ifdef EXTRA_DEBUG
printf ("DEBUG: ScaleA  BldXtkArgList returned numArgs = %d\n", numArgs);
for ( int j = 0 ; j < numArgs ; j++ )
{
printf ("DEBUG: argList[%2d] = %-20s %1s %d\n", j, argList[j].name, ":", argList[j].value );
}
#endif


    // Create widget with resources resulting from BldWptArgList/BldXtkArgList

   widgetId = XtCreateManagedWidget(name, xmScaleWidgetClass,
                panelId->GetWidgetId(), argList, numArgs);

   FreeArgList(argList);
   FreeWptArgList();
   if (widgetId == NULL)
	{
        delete this;
        return;
        }

   // Some presentation types need to call Xt_SetWidgetSize here

   DoSetUp(name, targetSym, viewSym); 

   selectCallbackPtr = NULL;		// Init for application callback
   releaseCallbackPtr = NULL;         // Init for application callback

   XtAddCallback(widgetId, XmNvalueChangedCallback, 
		(XtCallbackProc) ScaleReleaseCallback, (XtPointer) this);

	// Does user want continuous events to be reported?
	// Default is "only on buttonpress" (click).
   Symbol &continEvents = (*viewSym)["continued_event"] ;
   if ( &continEvents )
   	{
	if (s_equal( continEvents.String(), "Continuously") )
   	    XtAddCallback(widgetId, XmNdragCallback, 
		(XtCallbackProc) ScaleReleaseCallback, (XtPointer) this);
	}
   else
	{
	printf ("DESIGN ERROR: cannot get value of continued_event resource\n");
	}

   if(!ScaleTB) ScaleTB = XtParseTranslationTable(newtb);
   XtOverrideTranslations ( widgetId, ScaleTB );


   active = TRUE ;	// default; MAY BE PROBLEM - KBS; TBD
} // ScaleA constructor


/*************************************************************************/

ScaleA::~ScaleA ()

{
sentinel = 0;
}

/*************************************************************************/

// Set application callback function
void ScaleA::SetAppCallback(TEXT* callbackType, 
		 XtCallbackProc appCallbackProc,  void *  appCallbackData)

	// FOR FUTURE USE

{
if (s_equal (callbackType, XmNarmCallback))
    {
    selectCallbackPtr = appCallbackProc;
    selectCallbackData = appCallbackData;
    }
else if (s_equal (callbackType, XmNvalueChangedCallback))
    {
    releaseCallbackPtr = appCallbackProc;
    releaseCallbackData = appCallbackData;
    }
else
    // Ignore other callbacks.
    printf ("SetAppCallback: Callback for '%s' not implemented. \n", callbackType);
} // SetAppCallback



/********************************************************************/

void ScaleA::DispatchCallback(Widget widgetId, 
		TEXT * callbackType,  void * callData) 
    {
		// TBD: yet to define callbacks for non-MOTIF
#ifdef EXTRA_DEBUG
printf ("DEBUG: DispatchCallback got callback: %s\n", callbackType);
#endif

	// for future
    if (selectCallbackPtr != NULL)	// application specified callback
	{
	printf("DESIGN ERROR: selectCallbackPtr not null\n");
	(*selectCallbackPtr) (widgetId, (Opaque) selectCallbackData, 
			(Opaque) callData);
	}
    else if (releaseCallbackPtr != NULL)	// application specified callback
	{
	printf("DESIGN ERROR: releaseCallbackPtr not null\n");
	(*releaseCallbackPtr) (widgetId, (Opaque) releaseCallbackData, 
			(Opaque) callData);
	}

    else	// NORMAL CASE
	{
	// application specified no callback proc.  So create a WPT event
	// and put it in the queue.  Also update the target
	
	/********************************************************************/
	
static  TAEINT    intVal[1];
static  TAEFLOAT  realVal[1];
void    * valuePtr  = nil ;
Symbol  *s = TargetSym();
XtkArg  newArg[2];
TAEINT	value ;
short	dec_pts ;
	    
        if (s_equal (callbackType, XmNdragCallback))
	{
printf ("DESIGN ERROR: got XmNdragCallback!!!!!!!!!!! TBD what to do here.\n");
	}
        else if (s_equal (callbackType, XmNvalueChangedCallback))
	{
	    switch ( s->Type() )
	    {
		case V_INTEGER:
    			XtSetArg(newArg[0], XmNvalue, &value );
    			XtGetValues(widgetId, newArg, 1);
#ifdef EXTRA_DEBUG
printf ("DEBUG: DispatchCallback: value = %d\n", value );
#endif
			intVal[0] = value ;
			valuePtr = (void *) &intVal[0];
			break ;
	
		case V_REAL:
		// NOTE that Motif widget supports DISPLAYING value as if it was
		// a float, but scale value is always an integer.  So TAE must
		// convert the value to a float to store in TAE target.
    			XtSetArg(newArg[0], XmNvalue, &value );
    			XtSetArg(newArg[1], XmNdecimalPoints, &dec_pts );
    			XtGetValues(widgetId, newArg, 2);

			realVal[0] = (TAEFLOAT) value ;
	{
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
			short i;
			for ( i = dec_pts ; i > 0 ; i-- )
#else
			for ( short i = dec_pts ; i > 0 ; i-- )
#endif
				realVal[0] /= 10.0 ;
	}
			valuePtr = (void *) &realVal[0];
#ifdef EXTRA_DEBUG
printf ("DEBUG: DispatchCallback: value = %d dec_pts = %d, ", value, dec_pts );
printf ("converted to REAL = %f\n", realVal[0] );
#endif
			break ;
	
		default:	// UNREACHABLE
			printf ("DESIGN ERROR: DispatchCallback: Unexpected Data Type: %d\n", 
			s->Type() );
	    } // switch
	} // end if release

	else // UNREACHABLE
	    printf ("DESIGN ERROR: UNEXPECTED callback\n");

	// for select or release callbacks


	// Update TAE target variable and return event to application
	if ( valuePtr )
	    SetTargetValue ( valuePtr, 0 );  

	if (GenerateEvent())
	    CreateWptEvent();		// inform caller 

	} // end application specified no callback proc
} // DispatchCallback


/************************************************************/ 

int ScaleA::BldWptArgList(Symbol *targetSym, Symbol *viewSym, 
		Widget parentWidget)
{


// static  VmResource  scaleRes[] 	// see top of file

static VmToWptArg scaleArgs[] = {

     // ConverFunc    XtName        vmResource  vmResCount  defaultVal
     {IntgConvert,  XmNx,           xOffset,      1,   (void *) 0 },
     {IntgConvert,  XmNy,           yOffset,      1,   (void *) 0 },
     {IntgConvert,  XmNwidth,       width,        1,   (void *) 100 },
     {IntgConvert,  XmNheight,      height,       1,   (void *) 100 },

     {IntgConvert,  XmNborderWidth, borderWidth,  1,   (void *) 0 },
     {IntgConvert,  XmNshadowThickness, shadow,  1,   (void *) 2 },

     {ColorConvert, XmNbackground,  background,   1,   (void *) WPT_DEF_BG },
     {ColorConvert, XmNforeground,  foreground,   1,   (void *) WPT_DEF_FG },
     {ColorConvert, XmNborderColor, foreground,   1,   (void *) WPT_DEF_FG },
     {FontListConvert,  XmNfontList, font,        1,   (void *) WPT_DEF_FONT },

// SCALE SPECIFIC RESOURCES - all defaults are current OSF/MOTIF defaults
/******** KBS: replace XmNscaleWidth/XmNscaleHeight with sliderThickness
     {IntgConvert,  XmNscaleWidth, scaleWidth,	  1,   (void *) 30 },
     {IntgConvert,  XmNscaleHeight, scaleHeight,	  1,   (void *) 30 },
**********/

	// XmNshowValue & XmNsensitive are Boolean
     {IntgConvert,  XmNshowValue, showValue,	  1,   (void *) 1 },
     {IntgConvert,  XmNsensitive, sensitive,	  1,   (void *) 1 },
 
	// NOTE: MOTIF Scale uses XmNtitleString, not XmNlabelString
     {XmStringConvert, XmNtitleString, none,      1,     (void *) "    "      }
};

// NOTE:  These resources are handled directly, NOT by converters:
//
// {IntgConvert,  XmNminimum,       minimum,	  1,   (void *) 0 },
// {IntgConvert,  XmNmaximum,       maximum,	  1,   (void *) 100 },
// {IntgConvert,  XmNscaleMultiple, scaleMultiple,	  1,   (void *) 10 },
// {StringConvert,  XmNprocessingDirection, direction, 1, (void *) "Normal" },
// {StringConvert,  XmNorientation,  orientation, 1,   (void *) "Vertical" },
// {IntgConvert,  XmNdecimalPoints,  decimalPts, 1,   (void *) 0 },


 const  NUM_SCALE_ENTRIES = sizeof(scaleArgs)/sizeof(VmToWptArg);
 const  SCALE_WIDTH_INDEX = 2;
 const  SCALE_HEIGHT_INDEX = 3;

// The "+ 10" is for NULL + orientation + processing direction 
// + sliderThickness + scaleWidth OR scaleHeight + XmNvalue
// + minimum, maximum, scaleMultiple, decimalPoints
 wptArgs = (WptArg **) tae_alloc(NUM_SCALE_ENTRIES + 10, sizeof(WptArg *) );

    for (int i = 0; i < NUM_SCALE_ENTRIES; i++)
 	{
        wptArgs[i] = scaleArgs[i].converter(&scaleArgs[i], 
		viewSym, targetSym, parentWidget);
        if (wptArgs[i] == NULL)		// invalid target or view
	    {
	    FreeWptArgList();
	    return (0);
	   }
	}


// Convert our TAE string values to integer for XT; don't use above converters
    int orientation = s_equal( (*viewSym)["orientation"].String(), "Horizontal") 
		? XmHORIZONTAL : XmVERTICAL  ;
    wptArgs[i] = new WptArg ( XmNorientation, (XtArgVal) orientation, FALSE );
    i++ ;

    int direction = s_equal( (*viewSym)["direction"].String(), 
				"Ascending") ? 1 : 0 ;
    if ( orientation == XmVERTICAL ) 
    {
	if ( direction == 0 )   // Reverse processingDirection
		direction = XmMAX_ON_BOTTOM ;
	else
		direction = XmMAX_ON_TOP ;
    }
    else  // XmHORIZONTAL
    {
	if ( direction == 0 )   // Reverse processingDirection
		direction = XmMAX_ON_LEFT ;
	else
		direction = XmMAX_ON_RIGHT ;
    }
    wptArgs[i] = new WptArg ( XmNprocessingDirection, (XtArgVal) direction, FALSE );
    i++ ;

    // Update either scaleWidth or scaleHeight based on orientation
    // and our Details panel item called slider thickness.
    Dimension minor_size ;
    Symbol &sliderThickness = (*viewSym)["sliderThickness"] ;

    if ( &sliderThickness ) // already saved
	minor_size = sliderThickness.Intg() ;
    else
	printf ("DESIGN ERROR: cannot get value of sliderThickness.\n");

    if ( orientation == XmVERTICAL )
    {
#ifdef EXTRA_DEBUG
printf ("\tDEBUG: XmVERTICAL, Height = %d, minor_size = %d\n", (*viewSym)["size"].Intg(0), minor_size );
#endif
    	wptArgs[i] = new WptArg ( XmNscaleWidth, (XtArgVal) minor_size, FALSE );
    	i++ ;
    }
    else // XmHORIZONTAL
    {
#ifdef EXTRA_DEBUG
printf ("\tDEBUG: XmHORIZONTAL, Width = %d, minor_size = %d\n", (*viewSym)["size"].Intg(1), minor_size );
#endif
    	wptArgs[i] = new WptArg ( XmNscaleHeight, (XtArgVal) minor_size, FALSE );
    	i++ ;
    } // end orientation


    // Convert Vm real min, max, and multiple to int for widget
    int imin, imax, imult;
    switch ( targetSym->Type() )
    {
    case V_INTEGER:
    	imin      = (TAEINT) ((*viewSym)["minimum"].Real());
    	imax      = (TAEINT) ((*viewSym)["maximum"].Real());
    	imult     = (TAEINT) ((*viewSym)["scaleMultiple"].Real());

	// Force decimalPoints to zero for integer scales.
    	wptArgs[i] = new WptArg ( XmNdecimalPoints, (XtArgVal) 0, FALSE );
    	i++ ;
    	break;

    case V_REAL:
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
TAEFLOAT fmin, fmax, fmult;
fmin  = (*viewSym)["minimum"].Real();
fmax  = (*viewSym)["maximum"].Real();
fmult = (*viewSym)["scaleMultiple"].Real();
#else
      {
    	TAEFLOAT fmin  = (*viewSym)["minimum"].Real();
    	TAEFLOAT fmax  = (*viewSym)["maximum"].Real();
    	TAEFLOAT fmult = (*viewSym)["scaleMultiple"].Real();
#endif

#ifdef EXTRA_DEBUG
printf ("+++ BEFORE CONVERT, fmin = %f, fmax = %f, fmult = %f\n", 
fmin, fmax, fmult);
#endif

#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
	int dec_pts;
	dec_pts    = (*viewSym)["decimalPts"].Intg();
	short j;
	for ( j = dec_pts ; j > 0; j-- )
#else
      {
	int dec_pts    = (*viewSym)["decimalPts"].Intg();
    	for ( short j = dec_pts ; j > 0; j-- )
#endif
	{
    		fmin  *= 10 ;
    		fmax  *= 10 ;
    		fmult *= 10 ;
	}

#ifdef EXTRA_DEBUG
printf ("+++ AFTER CONVERT, fmin = %f, fmax = %f, fmult = %f, dec_pts = %d\n", 
fmin, fmax, fmult, dec_pts);
#endif
    	imin      = (TAEINT) fmin ;
    	imax      = (TAEINT) fmax ;
    	imult     = (TAEINT) fmult ;

	// Set decimalPoints to user-requested value for real scales.
    	wptArgs[i] = new WptArg ( XmNdecimalPoints, (XtArgVal) dec_pts, FALSE );
    	i++ ;
    	break;
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
#else
      }
      }
#endif

    default:
    	printf ("DESIGN ERROR: BldWptArgList: Unexpected Data Type: %d\n",
                            targetSym->Type() );
    }  // switch

#ifdef EXTRA_DEBUG
printf ("+++ setting imin = %d, imax = %d, imult = %d\n",imin,imax,imult);
#endif

    wptArgs[i] = new WptArg ( XmNminimum, (XtArgVal) imin, FALSE );
    i++ ;
    wptArgs[i] = new WptArg ( XmNmaximum, (XtArgVal) imax, FALSE );
    i++ ;
    wptArgs[i] = new WptArg ( XmNscaleMultiple, (XtArgVal) imult, FALSE );
    i++ ;


    // In case user has used Set Defaults Mode to set initial widget value,
    // set XmNvalue so widget will display this value upon XtCreateWidget.
    // Unlike all other scale resources, this is target info.
    if ( targetSym->Count() > 0 )
    {
        int initial_value = 0;
        switch ( targetSym->Type() )
        {
        case V_INTEGER:
    	    	    	initial_value = targetSym->Intg();
    			break;
        case V_REAL:
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
			int dec_pts;
			dec_pts = (*viewSym)["decimalPts"].Intg();
			TAEFLOAT real_val;
			real_val = targetSym->Real();
			short i;
			for ( i = dec_pts ; i > 0; i-- )
#else
	{
    			int dec_pts = (*viewSym)["decimalPts"].Intg();
    	    	    	TAEFLOAT real_val = targetSym->Real();
    			for ( short i = dec_pts ; i > 0; i-- )
#endif
    				real_val *= 10 ;
    			initial_value = (TAEINT) real_val ;
    			break;
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
#else
	}
#endif
    
        default:
    			printf ("DESIGN ERROR: BldWptArgList: Unexpected Data Type: %d\n", 
    				targetSym->Type() );
        }  // switch
    
        // Set XmNvalue resource to initial_value (must be integer)
#ifdef EXTRA_DEBUG
    printf ("======== setting initial_value to %d\n", initial_value );
#endif
    
        wptArgs[i] = new WptArg ( XmNvalue, (XtArgVal) initial_value, FALSE );
        i++ ;
    } // end if default value set

    wptArgs[i] = NULL;			// null terminated list

//
//  check initial sizes, for the Motif scale is not smart enough to do it
//
    if ( wptArgs[SCALE_WIDTH_INDEX]->Value() <= 0 &&
	 wptArgs[SCALE_HEIGHT_INDEX]->Value() <= 0 )
	{
	wptArgs[SCALE_WIDTH_INDEX]->SetValue(100);
	wptArgs[SCALE_HEIGHT_INDEX]->SetValue(100);
	}

    return (1);
} // BldWptArgList


/************************************************************/ 

// UpdateView is called by Wpt_ViewUpdate
//
int ScaleA::UpdateView(Symbol *newView)

{    
 
  XtkArg   argList[30];
  int      numArgs, titleArgIndex;

   if (newView == nil)
	return (FAIL);


// Get Common view arguments (color, font, etc.)
// File agentRt.cc contains def of CommonViewArgs which
// handles only the general widget resources.
// $TINC/wptinc/X{m,w}/widgetagent.h declares this function
// so that 3rd arg (label, title, etc) can be omitted in the call.

// Note that CommonViewArgs updates argList but does not call XtSetValues

    numArgs = CommonViewArgs(newView, argList, XmNtitleString);
    titleArgIndex = numArgs - 1;		// title is always last
    int i = numArgs ;

    // Fix scrollbar troughColor (must be after CommonViewArgs since
    // that determines XmNselectColor using XmGetColors)
    Pixel selectColor;
    XtArgVal xtArgVal; // data type incompatible with int or Pixel
    if ( Xt_GetResource ( argList, numArgs, XmNselectColor, &xtArgVal ) )
	{
	WidgetList *kids;
	int nkids;
	Arg tmpargs[2];
        int s;		// use local indices

	selectColor = (Pixel) xtArgVal;
	XtSetArg ( argList[i], XmNtroughColor, selectColor ); i++;

	// Unfortunately, scale does not have a direct way
	// to get its scrollbar widget, so use Composite resources
	s = 0;
        XtSetArg (tmpargs[s], XmNnumChildren, &nkids ); s++ ;
        XtSetArg (tmpargs[s], XmNchildren, &kids ); s++ ;
        XtGetValues ( widgetId, tmpargs, s );
	for ( int t = 0; t < nkids; t++ )
            {

            if ( XmIsScrollBar ( (Widget) kids[t]) ) // from ScrollBar.h
                {
		// Use full argList (CommonView + trough)
		XtSetValues ( (Widget) kids[t], argList, i );
                }
            }
	}
    else 
	printf ("DESIGN ERROR: couldn't get XmNselectColor; trough will be wrong color.\n");

    // Convert our TAE string values to integer for XT.
    // orientation & direction are stored as string only because
    // radio button (in Scale Presentation Panel) can't be type integer.
    // showValue controls whether numeric is displayed in widget

    // Note: XmVERTICAL = 1 and XmHORIZONTAL = 2
    //       XmMAX_ON_TOP and XmMAX_ON_LEFT are both 0.
    //       XmMAX_ON_BOTTOM and XmMAX_ON_RIGHT are both 1.
    //       Motif defaults: XmVERTICAL and XmMAX_ON_TOP.
    //       processingDirection controls whether values are ascending/decending

    int orientation = s_equal( (*newView)["orientation"].String(), 
				"Horizontal") ? XmHORIZONTAL : XmVERTICAL  ;
    XtSetArg (argList[i], XmNorientation, orientation );   i++ ;

    int direction = s_equal( (*newView)["direction"].String(), 
				"Ascending") ? 1 : 0 ;
    if ( orientation == XmVERTICAL ) 
    {
	if ( direction == 0 )   // Reverse processingDirection
		direction = XmMAX_ON_BOTTOM ;
	else
		direction = XmMAX_ON_TOP ;
    }
    else  // XmHORIZONTAL
    {
	if ( direction == 0 )   // Reverse processingDirection
		direction = XmMAX_ON_LEFT ;
	else
		direction = XmMAX_ON_RIGHT ;
    }
    XtSetArg (argList[i], XmNprocessingDirection, direction );   i++ ;

/************* scaleWidth/scaleHeight are indirect; see sliderThickness
    XtSetArg (argList[i], XmNscaleWidth, (*newView)["scaleWidth"].Intg() );  i++ ;
    XtSetArg (argList[i], XmNscaleHeight, (*newView)["scaleHeight"].Intg() );  i++ ;
**************/

    // Convert min, max, and mult fromn real to int for widget
    int imin, imax, imult;

    switch ( targetSym->Type() )
    {
    case V_INTEGER:
    	imin      = (TAEINT) ((*newView)["minimum"].Real());
    	imax      = (TAEINT) ((*newView)["maximum"].Real());
    	imult     = (TAEINT) ((*newView)["scaleMultiple"].Real());

	// Force decimalPoints to zero for integer scales.
        XtSetArg (argList[i], XmNdecimalPoints, 0 );  i++ ;
    	break;

    case V_REAL:
	{
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
	TAEFLOAT fmin, fmax, fmult;
	fmin  = (*newView)["minimum"].Real();
	fmax  = (*newView)["maximum"].Real();
	fmult = (*newView)["scaleMultiple"].Real();
#else
    	TAEFLOAT fmin  = (*newView)["minimum"].Real();
    	TAEFLOAT fmax  = (*newView)["maximum"].Real();
    	TAEFLOAT fmult = (*newView)["scaleMultiple"].Real();
#endif

        XtSetArg (argList[i], XmNdecimalPoints, 
		(*newView)["decimalPts"].Intg() );  i++ ;

#ifdef EXTRA_DEBUG
printf ("+++ UpdateView, BEFORE fmin = %f, fmax = %f, fmult = %f\n", 
fmin, fmax, fmult);
#endif

	{
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
	int dec_pts;
	dec_pts    = (*newView)["decimalPts"].Intg();
	short i;
	for ( i = dec_pts ; i > 0; i-- )
#else
	int dec_pts    = (*newView)["decimalPts"].Intg();
    	for ( short i = dec_pts ; i > 0; i-- )
#endif
	{
    		fmin  *= 10 ;
    		fmax  *= 10 ;
    		fmult *= 10 ;
	}
	}

#ifdef EXTRA_DEBUG
printf ("+++ UpdateView, AFTER fmin = %f, fmax = %f, fmult = %f\n", 
fmin, fmax, fmult);
#endif

    	imin      = (TAEINT) fmin ;
    	imax      = (TAEINT) fmax ;
    	imult     = (TAEINT) fmult ;
    	break;
	}

    default:
    	printf ("DESIGN ERROR: UpdateView: Unexpected Data Type: %d\n",
                            targetSym->Type() );
    }  // switch

#ifdef EXTRA_DEBUG
printf ("+++ UpdateView, setting imin = %d, imax = %d, imult = %d\n",
imin,imax,imult);
#endif

    XtSetArg (argList[i], XmNminimum,       imin ); i++ ;
    XtSetArg (argList[i], XmNmaximum,       imax ); i++ ;
    XtSetArg (argList[i], XmNscaleMultiple, imult ); i++ ;


    // The rest of the resources are all integer so they require
    // no special handling.

    XtSetArg (argList[i], XmNshowValue, (*newView)["showValue"].Intg() );  i++ ;


// Perform the widget update for both common & scale-specific resources.
    XtSetValues(widgetId, argList, i);    		
//
//  free compound string bad programming but necessary (from CommonViewArgs)
//
    XmStringFree((XmString) argList[titleArgIndex].value);

// The following is not usually necessary, but DisplayNewValue() is called
// here to force the widget to show its (possibly new) value.
    int new_value ;
    XtSetArg (argList[0], XmNvalue, &new_value );
    XtGetValues(widgetId, argList, 1);    		
    DisplayNewValue ( new_value );

    return (SUCCESS);

    } // UpdateView


/************************************************************/ 
//	Update the item's selected value displayed on the panel.

//	This is for Wpt_ParmUpdate, Wpt_SetNoValue and Wpt_PanelReset

int ScaleA::DisplayNewValue(int newTarget)
{
	// TBD: non-MOTIF DisplayNewValue

    XtkArg  newArg[1];
    TEXT    nullString[1]; 

    TAEINT 	int_val ;
    short 	dec_pts ;
    TAEFLOAT 	real_val ;
    TEXT  	*newString;

    int dummy = newTarget ;	// avoid compiler warning for virtual function
    nullString[0] = 0;

	// NOTE: CurrentTargetStrings() obtains target value(s)
    TEXT  **strings = CurrentTargetStrings();  	// already set in target
    newString =  (strings == nil) ? &nullString[0] : strings[0];

    Symbol *s = TargetSym();
	
    if ( strings == nil )
    {
	// This branch is for Wpt_SetNoValue and Wpt_PanelReset

    	XtSetArg(newArg[0], XmNvalue, 0 );
    	XtSetValues(widgetId, newArg, 1);  // set the new value
        return 1;
    }
    else
    {
        switch ( s->Type() )
        {
            case V_STRING:
			// UNREACHTABLE
			printf ("ERROR: scaleRt::DisplayNewValue: String should not be possible.\n");
                        break ;

            case V_INTEGER:
			s_s2i ( newString, &int_val ); // convert to int
#ifdef EXTRA_DEBUG
printf ("KBS DEBUG: in DisplayNewValue, updating value to ------> %d\n", int_val);
#endif
    			XtSetArg(newArg[0], XmNvalue, int_val );
    			XtSetValues(widgetId, newArg, 1);  // set the new value
                        break ;

            case V_REAL:
			// NOTE: Motif scale looks like it's displaying a float
			//       but widget can only hold an int.
			// Determine how many decimal places are needed.
    			XtSetArg(newArg[0], XmNdecimalPoints, &dec_pts );
    			XtGetValues(widgetId, newArg, 1);         
#ifdef EXTRA_DEBUG
printf ("KBS DEBUG: DisplayNewValue: decimalPts = %d\n", dec_pts );
#endif

			s_s2r ( newString, &real_val ); // convert to real
#ifdef EXTRA_DEBUG
printf ("KBS DEBUG: in DisplayNewValue, updating value to ------> %f\n", real_val);
#endif

	{
#if defined(hpux) || (defined(sun) && OSMajorVersion >= 5)
			short i;
			for ( i = dec_pts ; i > 0 ; i-- )
#else
			for ( short i = dec_pts ; i > 0 ; i-- )
#endif
				real_val *= 10 ;
			int_val = (TAEINT) real_val ;
	}
			
#ifdef EXTRA_DEBUG
printf ("KBS DEBUG: converted value to INT = %d\n", int_val );
#endif
    			XtSetArg(newArg[0], XmNvalue, int_val );
    			XtSetValues(widgetId, newArg, 1); // set the new value
                        break ;

            default:    // UNREACHABLE
                        printf ("DESIGN ERROR: DisplayNewValue: Unexpected Data Type: %d\n", s->Type() )
;
        } // switch
      }

    if (strings != nil)
	{
	for (int i = 0; i < s->Count(); i++)
	    tae_free(strings[i]);
	tae_free(strings);
	}
return 1;
} // DisplayNewValue

/********************************************************************/

// SELECT CALLBACKS

static void ScaleSelectCallback(Widget widgetId, 
  		void * itemData,  void * callData) 
    {
    widgetId = widgetId;
    ScaleA * itemId = (ScaleA *) itemData; 
    itemId->ScaleSelect(callData);
    return;
    }

void ScaleA::ScaleSelect(void *callData)
    {
    if (parentPanel->GetMode() == HELP)		// in help mode
    	{
	DisplayNewValue(FALSE);		// user might have clicked
	Helper();
    	}
    else
        DispatchCallback( widgetId, XmNarmCallback, callData);
    return;
    }


// RELEASE CALLBACKS

static void ScaleReleaseCallback(Widget widgetId, 
  		void * itemData,  void * callData) 
    {
    widgetId = widgetId;
    ScaleA * itemId = (ScaleA *) itemData; 
    itemId->ScaleRelease(callData);
    return;
    }


void ScaleA::ScaleRelease(void *callData)
    {
    if (parentPanel->GetMode() == HELP)		// in help mode
    	{
	DisplayNewValue(FALSE);		// user might have clicked
	Helper();
	}
    else
        DispatchCallback( widgetId, XmNvalueChangedCallback, callData);
    return;
    }

