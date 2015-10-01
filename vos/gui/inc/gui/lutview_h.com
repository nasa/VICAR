$!****************************************************************************
$!
$! Build proc for MIPL module lutview_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:33
$!
$! Execute by entering:		$ @lutview_h
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
$ write sys$output "*** module lutview_h ***"
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
$ write sys$output "Invalid argument given to lutview_h.com file -- ", primary
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
$   if F$SEARCH("lutview_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @lutview_h.bld "STD"
$   else
$      @lutview_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lutview_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lutview_h.com -mixed -
	-s LutAxisView.h LutHorAxisView.h LutBox.h LutBtnInterface.h LutView.h -
	   LutVerAxisView.h LutGraphView.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create LutAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// LutAxisView.h: A component class to show axis
/////////////////////////////////////////////////////////////
#ifndef LUTAXISVIEW_H
#define LUTAXISVIEW_H
#include "LutView.h"

class LutAxisView : public LutView {

  private:

        static XtResource _resources[];

  protected:

	XmString        xmstr;

	Widget _ruler;

    	GC _gc;
	XFontStruct *_fontStruct;
	char *_fontname;

	static String _defaults[];

        // Geometry
        Dimension _drawOffset, _twoTicks, _fourTicks, _eightTicks;
        Dimension _longTickLength, _shortTickLength;

	static void displayCallback ( Widget, XtPointer, XtPointer);

  public:

    	LutAxisView ( Widget, const char *, Lut *, Lut*, Lut* );
	LutAxisView ( Widget, const char *, Lut *);
    	virtual ~LutAxisView ();

	virtual void update ()=0;

    	virtual const char *const className() { return "LutAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutHorAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutVerAxisView.C: This class implements update function
// that draws axis oriented vertically.
////////////////////////////////////////////////////////
#ifndef LUTHORAXISVIEW_H
#define LUTHORAXISVIEW_H
#include "LutAxisView.h"
#include "Lut.h"

class LutHorAxisView : public LutAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void update();

  public:

    	LutHorAxisView ( Widget parent, const char *name, 
			Lut *lut, Lut *lut1, Lut *lut2 )
		: LutAxisView (parent, name, lut, lut1, lut2) 
			{ }

	LutHorAxisView ( Widget parent, const char *name, Lut *lut)
		: LutAxisView (parent, name, lut)
			{ }

	virtual ~LutHorAxisView() { } 

    	virtual const char *const className() { return "LutHorAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutBox.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// LutBox.h
////////////////////////////////////////////////////////////////
#ifndef LUTBOX_H
#define LUTBOX_H
#include "UIComponent.h"
#include "Lut.h"

class Lut;

class LutBox : public UIComponent {

  public:

	LutBox(Widget, const char *, Lut *, Lut *, Lut *);

	virtual const char *const className() { return "LutBox"; }

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutBtnInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// LutBtnInterface.h: A "push button" interface to a Cmd object
///////////////////////////////////////////////////////////////
#ifndef LUTBTNINTERFACE
#define LUTBTNINTERFACE
#include "SgDrawAreaInterface.h"

class Lut;

class LutBtnInterface : public SgDrawAreaInterface {

  public:
    
    LutBtnInterface ( Widget, Cmd*, Lut*, Lut*, Lut* );
    virtual ~LutBtnInterface ( ) { }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// LutView.h: Abstact base class for LUT views
///////////////////////////////////////////////////////////////////
#ifndef LUTVIEW_H
#define LUTVIEW_H
#include "UIComponent.h"
#include "Lut.h"
#include <iostream>

class LutView : public UIComponent {

  protected:

	Lut *_lut;	// red
	Lut *_lut1;	// green
	Lut *_lut2;	// blue

  public:

        LutView ( const char *name, Lut *r, Lut *g, Lut *b ) 
	    	: UIComponent (name) 
	    { _lut = r; _lut1 = g; _lut2 = b; }

	LutView ( const char *name, Lut *r )
                : UIComponent (name)
            { _lut = r; _lut1 = NULL; _lut2 = NULL; }

	virtual ~LutView() 
	    { if (_lut) _lut->detachView(this);
	      if (_lut) _lut1->detachView(this);
	      if (_lut) _lut2->detachView(this);
	    }

        virtual void update()=0;

        virtual const char *const className() { return "LutView"; }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutVerAxisView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// LutHorAxisView.C: This class implements update function
// that draws axis oriented horizontally.
////////////////////////////////////////////////////////
#ifndef LUTVERAXISVIEW_H
#define LUTVERAXISVIEW_H
#include "LutAxisView.h"

class Lut;

class LutVerAxisView : public LutAxisView {

  protected:

	XmString        xmstr;

	Dimension _width, _height;

    	virtual void update();

  public:

    	LutVerAxisView ( Widget parent, const char *name, 
			Lut *lut, Lut *lut1, Lut *lut2 )
		: LutAxisView (parent, name, lut, lut1, lut2)
			{ if (_lut) _lut->attachView(this); }

	LutVerAxisView ( Widget parent, const char *name, Lut *lut )
                : LutAxisView (parent, name, lut)
			{ if (_lut) _lut->attachView(this); }

	virtual ~LutVerAxisView() { }

    	virtual const char *const className() { return "LutVerAxisView"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutGraphView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// LutGraphView.h: A component class to show lut graph 
///////////////////////////////////////////////////////////////////
#ifndef LUTGRAPHVIEW_H
#define LUTGRAPHVIEW_H
#include "LutView.h"

class Lut;

class LutGraphView : public LutView {

  private:

	static XtResource _resources[];

	static void displayCallback ( Widget widget, 
				      XtPointer client_data, 
				      XtPointer call_data );

  protected:

	XColor  colorBg, colorFg, colorR, colorG, colorB;

	char * _red, *_green, *_blue;

	GC _gc;
	Dimension _width, _height;

  public:

	LutGraphView ( Widget, const char *, Lut *, Lut *, Lut * );
	LutGraphView ( Widget, const char *, Lut * );
	virtual ~LutGraphView ();

	virtual void update ();

	virtual const char *const className() { return "LutGraphView"; }
};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
