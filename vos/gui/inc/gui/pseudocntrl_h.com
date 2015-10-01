$!****************************************************************************
$!
$! Build proc for MIPL module pseudocntrl_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:55:35
$!
$! Execute by entering:		$ @pseudocntrl_h
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
$ write sys$output "*** module pseudocntrl_h ***"
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
$ write sys$output "Invalid argument given to pseudocntrl_h.com file -- ", primary
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
$   if F$SEARCH("pseudocntrl_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @pseudocntrl_h.bld "STD"
$   else
$      @pseudocntrl_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudocntrl_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudocntrl_h.com -mixed -
	-s CursorColorController.h PseudoRGBController.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorColorController.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorColorController.h
//
///////////////////////////////////////////////////////////////
#ifndef CURSORCOLORCONTROLLER_H
#define CURSORCOLORCONTROLLER_H
#include <Xm/Xm.h>
#include "XvicImage.h"
#include "PseudoDefs.h"		// Only for InterpolationType

class ColorModel;
class BasicWedgeOverlay;
class PseudoMarks;
class PseudoValue;

class CursorColorController {

  private:

	Boolean _firstTime;

  protected:

	Widget		 _colorImage, _bwImage;
	ColorModel 	 *_colorModel, *_bwModel;
	BasicWedgeOverlay *_wedgeView;
	PseudoMarks 	 *_pseudoMarks;
	PseudoValue 	 *_pseudoValue;

	int _current;

	int _red, _grn, _blu, _dn;
	int _red1, _grn1, _blu1, _dn1;	// for button-up state
	InterpolationType _itype;

	static void inputCallback( Widget,
			XtPointer clientData,
			XtPointer callData);

	static void cursorCallback( Widget,
			XtPointer clientData,
			XtPointer callData);

	virtual void startTracking();

	virtual void input(XtPointer);
	virtual void cursor(XtPointer);

	virtual void getColorValues( int, int&, int&, int& );  // get RGB values
	virtual int getBWValue( int ); 

  public:

	CursorColorController ( Widget, Widget,
		ColorModel *, ColorModel *,
		BasicWedgeOverlay *, 
		PseudoMarks *, PseudoValue * );
  virtual ~CursorColorController() {}
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoRGBController.h
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// PseudoRGBController.h: This class derives from RGBController
//                        The only difference is that it changes
//                        not only ColorModel, but also
//                        PseudoValue for the current mark as 
//                        user drags the slider
/////////////////////////////////////////////////////////////
#ifndef PSEUDORGBCONTROLLER_H
#define PSEUDORGBCONTROLLER_H
#include "RGBController.h"

class PseudoMarks;

class PseudoRGBController : public RGBController {

  protected:
    
    PseudoMarks *_pseudoMarks;

    virtual void  redChanged ( int );
    virtual void  greenChanged ( int );
    virtual void  blueChanged ( int );
    
  public:
    
    PseudoRGBController ( Widget , ColorModel *, const char *, PseudoMarks * );

    const char *const className() { return "PseudoRGBController"; }
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
