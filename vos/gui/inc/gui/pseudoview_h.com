$!****************************************************************************
$!
$! Build proc for MIPL module pseudoview_h
$! VPACK Version 1.8, Monday, July 07, 1997, 17:35:59
$!
$! Execute by entering:		$ @pseudoview_h
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
$ write sys$output "*** module pseudoview_h ***"
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
$ write sys$output "Invalid argument given to pseudoview_h.com file -- ", primary
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
$   if F$SEARCH("pseudoview_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pseudoview_h.bld "STD"
$   else
$      @pseudoview_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudoview_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudoview_h.com -mixed -
	-s PseudoCmdInterface.h BasicWedgeOverlay.h PseudoColorView.h -
	   WedgeOverlayView.h InterpolationChooser.h DnValueView.h -
	   MarksToValueGlue.h ValueToMarksGlue.h MarksToColorGlue.h -
	   ImageToPseudoGlue.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create PseudoCmdInterface.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// PseudoCmdInterface.h: Fills pseudocolor LUT 
//////////////////////////////////////////////////////////////
#ifndef PSEUDOCMDINTERFACE
#define PSEDOCMDINTERFACE
#include "CmdInterface.h"

class PseudoValue;
class PseudoMarks;
class BasicWedgeOverlay;

class PseudoCmdInterface : public CmdInterface {

  private: 

	static void imageWidgetChangeCallback(Widget, XtPointer, XtPointer);

  protected: 

	PseudoValue *_pseudoValue;

	BasicWedgeOverlay *_wedge;      // bw wedge
	BasicWedgeOverlay *_ps;		// pseudocolored wedge

	void imageWidgetChange(XtPointer);
        void copyDisplayModeResources();
	Widget _iw;

  public:

	PseudoCmdInterface ( Widget, Cmd *, PseudoValue *, PseudoMarks *, Widget iw );

	void loadTable ( PseudoValue * );
	void executeCmd(XtPointer);

	virtual void setValue(CmdValue);

};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create BasicWedgeOverlay.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// BasicWedgeOverlay.h: 
////////////////////////////////////////////////////////////////

#ifndef BASICWEDGEOVERLAY_H
#define BASICWEDGEOVERLAY_H
#include "UIComponent.h"

class PseudoMarks;
class PseudoValue;

class BasicWedgeOverlay : public UIComponent {

  public:

	BasicWedgeOverlay(const char *name) 
				: UIComponent (name) { }

	virtual Widget getWidget() { return NULL; }

	virtual void update ( PseudoMarks * ) = 0;
	virtual void update ( PseudoValue * ) = 0;

	virtual const char *const className() { return ("BasicWedgeOverlay"); }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoColorView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// PseudoColorView.h : Applies pseudocolor table to the wedge
////////////////////////////////////////////////////////////////
#ifndef PSEUDOCOLORVIEW_H
#define PSEUDOCOLORVIEW_H
#include "WedgeOverlayView.h"
#include "PseudoMarks.h"

class PseudoValue;
class ColorModel;

class PseudoColorView : public WedgeOverlayView {

  public:

	PseudoColorView(Widget parent, const char * name,
					PseudoValue *pseudoValue);
	~PseudoColorView() { }


	virtual void update ( PseudoValue * );
	virtual void update ( PseudoMarks * m ) { WedgeOverlayView::update(m); }
//	virtual void update ( ColorModel * );

	virtual const char *const className() { return ("PseudoColorView"); }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create WedgeOverlayView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// WedgeOverlayView.h: Provides overlay graphics support for wedge class
// 		       The class allows to put set marks that are used
//		       by pseudocolor tool
////////////////////////////////////////////////////////////////
#ifndef WEDGEOVERLAYVIEW_H
#define WEDGEOVERLAYVIEW_H
#include "BasicWedgeOverlay.h"
#include "XvicImage.h"

class PseudoMarks;
class PseudoValue;

class WedgeOverlayView : public BasicWedgeOverlay {

  private:

        static XtResource _resources[];

  protected:

	PseudoValue 	*_pseudoValue;		// Model for pseudocolor value

	Widget _iw;

	XvicGC    _lineGC;
        XvicColor _lineColor, _curLineColor;

        String    _markColor, _selectedMarkColor;    // From the resource
	Dimension _markLength, _markThickness, _barThickness;

  public:

	WedgeOverlayView(Widget, const char *);

	Widget getWidget() { return _iw; };

	virtual void update ( PseudoMarks * );
	virtual void update ( PseudoValue * ) { };

	virtual const char *const className() { return ("WedgeOverlayView"); }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create InterpolationChooser.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// InterpolationChooser.cc: A component class to choose the interpolation
// on the interval.  The choices are No Interpolation,
// Flat, Linear, Cubic Spline.  Only one option can be chosen
///////////////////////////////////////////////////////
#ifndef INTERPOLATIONCHOOSER_H
#define INTERPOLATIONCHOOSER_H
#include "UIComponent.h"
#include "PseudoDefs.h"

class PseudoValue;
class PseudoMarks;
class PseudoCmdInterface;

class InterpolationChooser : public UIComponent {

 private:

   static void valueChangedCallback(Widget, XtPointer, XtPointer);

 protected:

   Widget _none, _flat, _linear, _cubs;

   PseudoValue *_pseudoValue;
   PseudoMarks *_pseudoMarks;
   InterpolationType _type;
   PseudoCmdInterface *_pseudoCmdInterface;

 public:

   InterpolationChooser(Widget, const char *, InterpolationType, PseudoValue *,
		PseudoMarks *, PseudoCmdInterface *);
   virtual ~InterpolationChooser() { }

   virtual void valueChanged();
   virtual void setValue(InterpolationType);

   virtual void update (PseudoMarks *);
   virtual const char *const className() { return "InterpolationChooser"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DnValueView.h
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// DnValueView.h: A display for a current mark numerical value
///////////////////////////////////////////////////////////////
#ifndef DNVALUEVIEW
#define DNVALUEVIEW
#include "ColorView.h"

class KeyinView;
class ColorModel;

class DnValueView : public ColorView {

  protected:

    KeyinView *_position;

  public:
    
    DnValueView ( Widget, const char * );
    virtual void update ( ColorModel * );

    virtual const char *const className() { return ("DnValueView"); }
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MarksToValueGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// MarksToValueGlue: class that serves as a "glue" class between an
// PseudoMarks object and a PseudoValue object.  The class is a
// View to PseudoMarks, so whenever it receives an update() from PseudoMarks,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef MARKSTOVALUEGLUE_H
#define MARKSTOVALUEGLUE_H
#include "BasicWedgeOverlay.h"

class PseudoMarks;
class PseudoValue;
class PseudoCmdInterface;

class MarksToValueGlue : public BasicWedgeOverlay {

 protected:

   PseudoValue *_value;
   PseudoCmdInterface *_pseudoCmdInterface;

   void *_collectionActive;

 public:

   MarksToValueGlue (Widget, const char*, PseudoMarks *, PseudoValue *, PseudoCmdInterface *);

   virtual void update(PseudoMarks *);	// the whole reason for the class existing
   virtual void update(PseudoValue *);

   virtual const char *const className() { return  "MarksToValueGlue"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ValueToMarksGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ValueToMarksGlue: Class that serves as a "glue" class between an
// Pseudovalue object and a PseudoMarks object.  The class is a
// View to PseudoValue, so whenever it receives an update() from PseudoValue,
// it recollects the LUT (which in turn cause it to update its
// own view(s)).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef VALUETOMARKSGLUE_H
#define VALUETOMARKSGLUE_H
#include "BasicWedgeOverlay.h"

class PseudoMarks;
class PseudoValue;
class PseudoCmdInterface;

class ValueToMarksGlue : public BasicWedgeOverlay {

 protected:

   PseudoMarks *_marks;
   PseudoCmdInterface *_pseudoCmdInterface;

   void *_collectionActive;

 public:

   ValueToMarksGlue (Widget, const char*, PseudoMarks *, PseudoValue *, PseudoCmdInterface *);

   virtual void update(PseudoValue *);	// the whole reason for the class existing
   virtual void update(PseudoMarks *);  

   virtual const char *const className() { return  "ValueToMarksGlue"; }

};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MarksToColorGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
#ifndef MARKSTOCOLORGLUE_H
#define MARKSTOCOLORGLUE_H
#include "InterpolationChooser.h"

class ColorModel;

class MarksToColorGlue : public InterpolationChooser {

 protected:

   ColorModel *_bwModel, *_colorModel;

 public:

   MarksToColorGlue(Widget parent, const char *name, InterpolationType type, 
		PseudoValue *value, PseudoMarks *marks, PseudoCmdInterface *pci,
		ColorModel *bwModel, ColorModel *colorModel) 
	: InterpolationChooser(parent, name, type, value, marks, pci) 
	  { _bwModel = bwModel; _colorModel = colorModel; }

   virtual void update (PseudoMarks *);
   virtual const char *const className() { return "MarksToColorGlue"; }
};
#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToPseudoGlue.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToPseudoGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Pseudocolor objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it disables the pseudocolor tool if the image is color and enables it
// if the image is b&w.  This class, even though it's a UIComponent, 
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOPSEUDOGLUE_H
#define IMAGETOPSEUDOGLUE_H
#include "BasicImageView.h"

class ImageData;
class Cmd;
class MenuDialog;

class ImageToPseudoGlue : public BasicImageView {

 protected:

   Cmd        *_pseudoModeCmd;
   MenuDialog *_pseudoDialog;
   Cmd	      *_pseudoCmd;
   Cmd        *_postLutCmd;
   Cmd        *_postPseudoCmd;

 public:

   ImageToPseudoGlue(ImageData *model, Cmd *modeCmd, MenuDialog *pseudoDialog, 
                     Cmd *pseudoCmd, Cmd *postLutCmd, Cmd *postPseudoCmd);

   virtual void update();	// the whole reason for the class existing

   virtual const char *const className() { return  "ImageToPseudoGlue"; }

};
#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
