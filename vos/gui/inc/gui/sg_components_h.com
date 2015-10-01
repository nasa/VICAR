$!****************************************************************************
$!
$! Build proc for MIPL module sg_components_h
$! VPACK Version 1.9, Tuesday, January 13, 1998, 11:38:06
$!
$! Execute by entering:		$ @sg_components_h
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
$ write sys$output "*** module sg_components_h ***"
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
$ write sys$output "Invalid argument given to sg_components_h.com file -- ", primary
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
$   if F$SEARCH("sg_components_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @sg_components_h.bld "STD"
$   else
$      @sg_components_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sg_components_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sg_components_h.com -mixed -
	-s SgDrawAreaInterface.h SgAxisView.h SgGraphView.h -
	   SgIntKeyinInterface.h SgButtonPanel.h SgColorChooserInterface.h -
	   SgResourceConverter.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SgDrawAreaInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgDrawAreaInterface.h: A "push button" interface to a Cmd object.
// The surface of a push-button is a drawing area widget.
// This class is intended to have subclasses.  The subclass
// must provide the drawing area widget.
///////////////////////////////////////////////////////////////
#ifndef SGDRAWAREAINTERFACE_H
#define SGDRAWAREAINTERFACE_H
#include "CmdInterface.h"

class SgDrawAreaInterface : public CmdInterface {

  protected:

    Widget _drawingAreaWidget;

    SgDrawAreaInterface ( Widget, Cmd * );
    virtual ~SgDrawAreaInterface ( );

    void setDrawingAreaWidget ( Widget );

  public:
    
    virtual void executeCmd(XtPointer);

    int operator== ( const SgDrawAreaInterface & );
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgAxisView.h: A component class to show a plot axis.
/////////////////////////////////////////////////////////////
#ifndef SGAXISVIEW_H
#define SGAXISVIEW_H
#include "UIComponent.h"

class SgAxisView : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    static String _defaults[];

    // Attribures that can change at run-time
    float _min;
    float _max;
    Boolean _intRange;

    GC _gc;
    XFontStruct *_fontStruct;
    char *_fontname;
    char *_drawColor;

    // Geometry
    Dimension _height, _width;
    Boolean _vertical;
    Boolean _ascending;
    Boolean _ticksOnly;
    Dimension _drawOffset, _fistTickMargin, _lastTickMargin;
    Dimension _longTickLength, _shortTickLength;
    Dimension _twoTicks, _fourTicks, _eightTicks;
    Dimension _tickThickness;
    Dimension _strOffset;

    static void displayCallback ( Widget, XtPointer, XtPointer);
    virtual void display();

    virtual void drawHorizontal ( Dimension width );
    virtual void drawVertical ( Dimension height, Dimension width );
    virtual int getNumTicks ( Dimension );

  public:

    SgAxisView ( Widget, const char *, Boolean vertical = TRUE );
    virtual ~SgAxisView();

    void setLimits ( int min, int max );
    void setLimits ( float min, float max );

    void setIntRange ( Boolean intRange );

    void setVertical ( Boolean vertical );
    void setAscending ( Boolean ascending );

    virtual const char *const className() { return "SgAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgGraphView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////
// SgGraphView.h:  This is a view component that draws graphs.
//////////////////////////////////////////////////////////////////////////
#ifndef SGGRAPHVIEW_H
#define SGGRAPHVIEW_H
#include "UIComponent.h"

class SgGraphView : public UIComponent {

  private:

    static XtResource _resources[];

  protected:

    float *_dataSet[3];
    int _numElements;

    Boolean _singleGraph;
    Boolean _blended;
    Boolean _horizontal;
    Boolean _ascending;

    float _maxUserValue;
    float _maxValue;
    int _spike;
    Boolean _log;
    Boolean _filled;

    Boolean _drawInsideTicks;
    int _insideTickLength, _insideMinorTickLength;
    int _insideTickInterval, _insideMinorTickInterval;

    Dimension _topMargin;
    Dimension _width;
    Dimension _height;

    char *_red, *_green, *_blue;
    char *_yellow, *_cyan, *_magenta;
    char *_white, *_bg;
    char *_insideTickColor;

    GC _gc;
    XColor _colorR, _colorG, _colorB;
    XColor _colorM, _colorC, _colorY, _colorW;

    static String _defaults[];

    static void displayCallback ( Widget, XtPointer, XtPointer);
    virtual void display();

    virtual void displayOneFilled();
    virtual void displayOneEmpty();
    virtual void displayThreeStacked();
    virtual void displayThreeBlended();
    virtual void displayThreeEmpty();

    void allocateColors();
    void setDrawingColor ( XColor color );
    static float spike ( float *ds, int size, int spikeCount );
    static float midOfThree ( float, float, float );
    float arrayMax ( float *, int );
    float maxSpikeValue();

    virtual void drawInsideTicks(double maxCountValue, 
				double ppb, int nbins);

  public:

    SgGraphView ( Widget, const char *, float *, float *, float *, int );
    SgGraphView ( Widget, const char *, float *, int );
    SgGraphView ( Widget, const char * );
    virtual ~SgGraphView();

    void setDataSet ( float *, int numElements );
    void setDataSets ( float *, float *, float *, int numElements );

    void setHorizontal ( Boolean h ) { _horizontal = h; display(); }
    Boolean getHorizontal() { return _horizontal; }

    void setAscending ( Boolean a ) { _ascending = a; display(); }
    Boolean getAscending() { return _ascending; }

    void setBlended ( Boolean b ) { _blended = b; display(); }
    Boolean getBlended() { return _blended; }
    
    void setSpike (int spike);
    int getSpike() { return _spike; }

    void setLogScale ( Boolean log ) { _log = log; display(); }
    Boolean logScaleIsSet() { return _log; }

    virtual const char *const className() { return "SgGraphView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgIntKeyinInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SgIntKeyinInterface.h: An integer keyin interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef SGINTKEINTERFACE
#define SGINTKEINTERFACE
#include "CmdInterface.h"

class KeyinView;

class SgIntKeyinInterface : public CmdInterface {

  protected:

    KeyinView *_keyin;
    
    virtual void executeCmd(XtPointer);
    virtual void setValue(CmdValue);

  public:

    SgIntKeyinInterface ( Widget, Cmd * );
    virtual ~SgIntKeyinInterface();
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgButtonPanel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////////////////
// SgButtonPanel.h: This is a row-column container that user can stack with 
// commands, each represented by a command interface.
//////////////////////////////////////////////////////////////////////////////
#ifndef SGBUTTONPANEL_H
#define SGBUTTONPANEL_H
#include "UIComponent.h"

class Cmd;
class CmdList;

class SgButtonPanel : public UIComponent {

  public:

    SgButtonPanel(Widget parent, const char *name);
    virtual ~SgButtonPanel() { }

    void addCommands(Cmd *);
    void addOptionMenu(CmdList *);

    virtual const char * const className() { return ("SgButtonPanel"); }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgColorChooserInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////////////
// SgColorChooserInterface.h:  A command interface that allows user to select 
// a color by either typing the value or using color chooser.  It then 
// executes command with value passed as a string (either standard X color 
// name, like 'red', or hex value in the form #RRGGBB).
//////////////////////////////////////////////////////////////////////////////
#ifndef SGCOLORCHOOSERINTERFACE_H
#define SGCOLORCHOOSERINTERFACE_H

#include "CmdInterface.h"

class ColorChooser;

class SgColorChooserInterface : public CmdInterface {

  protected:

    Widget _text, _button;
    ColorChooser *_colorChooser;

    char *_oldValue;		// Avoid repeat execs if string doesn't change

    virtual void copyOldValue(char *string);
    virtual void executeCmd(XtPointer = NULL);

    static void colorSelectedCallback ( int, int, int, void * );
    static void pickColorCallback ( Widget, XtPointer, XtPointer );

  public:

    SgColorChooserInterface(Widget, Cmd *);

    virtual void setValue(CmdValue);

    virtual void setValueAndRun(CmdValue);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SgResourceConverter.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <Xm/Xm.h>

class SgResourceConverter {

  protected:

    static Boolean _firstTime;

    static Boolean stringsAreEqual(const char *in_str, const char *test_str);
    
    static void cvtStringToPixmap(XrmValue *args, Cardinal *numArgs,
				  XrmValue *fromVal, XrmValue *toVal);

  public:

    static void registerStringToPixmapConverter();

};
$ VOKAGLEVE
$ Return
$!#############################################################################
