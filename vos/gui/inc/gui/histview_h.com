$!****************************************************************************
$!
$! Build proc for MIPL module histview_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:30
$!
$! Execute by entering:		$ @histview_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module histview_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to histview_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("histview_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @histview_h.bld "STD"
$   else
$      @histview_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histview_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histview_h.com -mixed -
	-s HistAxisView.h HistBox.h HistBoxWindow.h HistDefs.h HistGraphView.h -
	   HistHorAxisView.h HistVerAxisView.h HistView.h StatView.h -
	   HistLogVerAxis.h HistBtnInterface.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create HistAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// HistAxisView.h: A component class to show histogram axis
/////////////////////////////////////////////////////////////
#ifndef HISTAXISVIEW_H
#define HISTAXISVIEW_H
#include "HistDefs.h"
#include "HistView.h"

class HistAxisView : public HistView {

  private:

	static XtResource _resources[];

  protected:

	Widget _ruler;

    	GC _gc;
	XFontStruct *_fontStruct;
	char *_fontname;
	char *_drawColor;

	static String _defaults[];

	// Geometry
	Dimension _drawOffset, _twoTicks, _fourTicks, _eightTicks;
	Dimension _longTickLength, _shortTickLength;

	static void displayCallback ( Widget, XtPointer, XtPointer);

    	virtual void display(){ }

  public:

    	HistAxisView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, 
			OrientType );
    	virtual ~HistAxisView ();

	virtual void update ();

    	virtual const char *const className() { return "HistAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// HistBox.h: Container class for histogram view components.
////////////////////////////////////////////////////////////////
#ifndef HISTBOX_H
#define HISTBOX_H
#include "UIComponent.h"
#include "HistDefs.h"
#include "Histogram.h"

class HistAxisView;
class HistGraphView;
class StatView;

class HistBox : public UIComponent {

  private:

	static int _histBoxInit;           // flag for class initialization

	Boolean _shell_resize;

	static XtResource _resources[];

	static Boolean CvtStringToPopupDirectionType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToMethodType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToOrientType(Display *,
                   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );
	static Boolean CvtStringToVerAxisDirType(Display *,
		   XrmValue *, Cardinal *,
                   XrmValue *, XrmValue *, XtPointer * );

  protected:

	Histogram 		*_histR;
	Histogram 		*_histG;
	Histogram 		*_histB;

	HistBox 		*_histBoxR;
	HistBox 		*_histBoxG;
	HistBox 		*_histBoxB;

	Widget 			_sepRG, _sepGB;

	HistAxisView 		*_histHorAxisView;
	HistAxisView 		*_histVerAxisView;
	HistGraphView 		*_histGraphView;
	StatView 		*_statView;

	PopupDirectionType 	_popDirection;  // Meaningfull only for "Pop-up
	MethodType 		_method;
	OrientType 		_histOrient;
	VerAxisDirType		_verAxisDir;

	static String 		_defaults[];

        Boolean 		_showAxis;
        Boolean 		_showHist;
        Boolean 		_showStat;

        void layComponents();   // Shows only specified components
        void showComponents();  // Manages components
        void hideComponents();  // Unmanages components

  public:

	HistBox(Widget, const char *,
			Histogram *, Histogram * =NULL, Histogram* =NULL);
	~HistBox();

        void setPopupDirectionType ( PopupDirectionType );
        void setMethodType ( MethodType );
        void setOrientType ( OrientType );
	void setVerAxisDirType ( VerAxisDirType );
	void setSpike (int spike);

	PopupDirectionType getPopupDirectionType() { return _popDirection; }
	MethodType getMethodType() { return _method; }
	OrientType getOrientType() { return _histOrient; }
	VerAxisDirType getVerAxisDirType() { return _verAxisDir; }
        int getSpike();

	void setLogScale(Boolean log);

	void showAxis (Boolean show);
	void showHist (Boolean show);
	void showStat (Boolean show);

	Boolean AxisIsDisplayed () { return _showAxis; }
	Boolean HistIsDisplayed () { return _showHist; }
	Boolean StatIsDisplayed () { return _showStat; }
	Boolean logScaleIsSet ();

	Histogram *getHistR () { return _histR; }
	Histogram *getHistG () { return _histG; }
	Histogram *getHistB () { return _histB; }

	virtual const char *const className() { return "HistBox"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistBoxWindow.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// HistBoxWindow.h:
////////////////////////////////////////////////////////////////
#ifndef HISTBOXWINDOW_H
#define HISTBOXWINDOW_H
#include "MainWindow.h"

class HistBox;

class HistBoxWindow : public MainWindow {

private:

   HistBox *_histBox;

protected:

   virtual Widget createWorkArea(Widget);

public:

   HistBoxWindow(char *);
   ~HistBoxWindow();
};

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistDefs.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// HistDefs.h:
///////////////////////////////////////////////////////////////////
#include "GuiDefs.h"

#ifndef HISTDEFS_H
#define HISTDEFS_H

enum MethodType { STACKED, BLEND, POPUP };
enum PopupDirectionType { ROW, COLUMN };
enum VerAxisDirType { ASC, DESC };

// Define X resource names

#define XvicNmethod		"method"
#define XvicCMethod		"Method"
#define XvicRMethodType		"MethodType"

#define XvicNorientation	"histOrientation"
#define XvicCOrientation	"HistOrientation"
#define XvicROrientType         "OrientType"

#define XvicNpopupDirection	"popDirection"
#define XvicCPopupDirection	"PopDirection"
#define XvicRPopupDirectionType "PopupDirectionType,"

#define XvicNshowAxis           "showAxis"
#define XvicCShowAxis           "ShowAxis"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowStat           "showStat"
#define XvicCShowStat           "ShowStat"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowHist           "showHist"
#define XvicCShowHist           "ShowHist"
#define XvicRShowHistType       "Boolean"

#define XvicNspike              "spike"
#define XvicCSpike              "Spike"
#define XvicRSpikeType          "Int"

#define XvicNverAxisDir		"verAxisDir"
#define XvicCVerAxisDir		"VerAxisDir"
#define XvicRVerAxisDirType	"VerAxisDirType"

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistGraphView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// HistGraphView.h:  This is a view component that draws histogram.
// BW histogram gets drawn in the appropriate color -- red, green, or blue.
// Color histogram is drawn with colors blended (displayBlend method) or
// stacked on top of each other (displayStacked method).
//////////////////////////////////////////////////////////////////////////
#ifndef HISTGRAPHVIEW_H
#define HISTGRAPHVIEW_H
#include "HistDefs.h"
#include "HistView.h"

class HistGraphView : public HistView {

    private:

	static XtResource _resources[];

	static void displayCallback ( Widget widget, 
				      XtPointer client_data, 
				      XtPointer call_data );

    protected:

	int _spike;
	Boolean _log;

	Boolean _drawInsideTicks;
	int _insideTickLength, _insideMinorTickLength;
	int _insideTickInterval, _insideMinorTickInterval;

	char *_red, *_green, *_blue;
	char *_yellow, *_cyan, *_magenta;
	char *_white, *_bg;
	char *_insideTickColor;

	GC _gc;
	Dimension _width, _height;

	virtual void displayBW ( Boolean R, Boolean G, Boolean B );
	virtual void displayStacked();
	virtual void displayBlend();

	virtual void drawInsideTicks(double maxCountValue, 
				double ppb, int nbins);

    public:

	HistGraphView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, 
			MethodType, OrientType, VerAxisDirType );

	~HistGraphView ();

	void setSpike (int spike) { _spike = spike; }
	int getSpike() { return _spike; }

	void setLogScale(Boolean log) { _log = log; }
	Boolean logScaleIsSet() { return _log; }

	virtual void update ();

	virtual const char *const className() { return "HistGraphView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistHorAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// HistHorAxisView.h:
/////////////////////////////////////////////////////////////
#ifndef HISTHORAXISVIEW_H
#define HISTHORAXISVIEW_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistHorAxisView : public HistAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistHorAxisView ( Widget, const char *,
			Histogram *, Histogram *, Histogram *, OrientType );


    	virtual const char *const className() { return "HistHorAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistVerAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// HistVerAxisView.h:
/////////////////////////////////////////////////////////////
#ifndef HISTVERAXISVIEW_H
#define HISTVERAXISVIEW_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistVerAxisView : public HistAxisView {

  protected:

	XmString  xmstr;

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistVerAxisView ( Widget, const char *,
				Histogram *, Histogram *, Histogram *, 
				OrientType, VerAxisDirType );

    	virtual const char *const className() { return "HistVerAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// HistView.h:
///////////////////////////////////////////////////////////////////
#ifndef HISTVIEW_H
#define HISTVIEW_H
#include "UIComponent.h"
#include "HistDefs.h"
#include <iostream>

class Histogram;

class HistView : public UIComponent {

  protected:

	OrientType 	_hor;
	MethodType 	_method;
	VerAxisDirType 	_verAxisDir;

	Histogram *_hist;
	Histogram *_hist1;
	Histogram *_hist2;

  public:

	HistView ( const char *name ) : UIComponent (name) { }
	virtual ~HistView() { }

	void setOrientType (OrientType hor) { _hor = hor; }
	void setMethodType (MethodType method) { _method = method; }
	void setVerAxisDir (VerAxisDirType verAxisDir) 
					{ _verAxisDir = verAxisDir; }

	virtual void update()=0;

	virtual const char *const className() { return "HistView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StatView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StatView.h: A component class to show histogram statistics
/////////////////////////////////////////////////////////////

#ifndef STATVIEW_H
#define STATVIEW_H
#include "HistView.h"

class StatView : public HistView {

  private:

    Widget _frameMean, _frameStDev;   
    Widget _frameMean1, _frameStDev1;
    Widget _frameMean2, _frameStDev2;

    Widget _mean, _stDev;		// Input area
    Widget _mean1, _stDev1;
    Widget _mean2, _stDev2;

    Widget _labelMean, _labelStDev;	// The label
    Widget _labelMean1, _labelStDev1;
    Widget _labelMean2, _labelStDev2;

    char * _red, *_green, *_blue;

  public:

    StatView ( Widget, const char *, Histogram *, Histogram *, Histogram * );
    ~StatView ();

    void setStat ( double, double );
    void setStatColor ( double mR, double mG, double mB, 
			double sdR, double sdG, double sdB );

    virtual void update ();

    virtual const char *const className() { return "StatView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistLogVerAxis.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////
// HistVerAxisView.h:  This module is designed for use on MPF
// display.  It displays an vertical axis without tick marks, but
// with labels indicating the DN count for a logarithmically scaled
// histogram.  Labels are 1e1, 1e2, 1e3, etc.
//////////////////////////////////////////////////////////////////
#ifndef HISTLOGVERAXIS_H
#define HISTLOGVERAXIS_H
#include "HistAxisView.h"
#include "HistDefs.h"

class HistLogVerAxis : public HistAxisView {

  protected:

	Dimension _width, _height;

    	virtual void display();

  public:

    	HistLogVerAxis ( Widget, const char *, 
			 Histogram *, Histogram *, Histogram *, 
			 OrientType );
	virtual ~HistLogVerAxis() { }

    	virtual const char *const className() { return "HistLogVerAxis"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistBtnInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// HistBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef HISTBTNINTERFACE_H
#define HISTBTNINTERFACE_H
#include "SgDrawAreaInterface.h"

class Histogram;

class HistBtnInterface : public SgDrawAreaInterface {

  public:
    
    HistBtnInterface ( Widget, Cmd*, Histogram*, Histogram*, Histogram* );
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
