$!****************************************************************************
$!
$! Build proc for MIPL module pseudocntrl
$! VPACK Version 1.8, Tuesday, September 19, 1995, 15:03:58
$!
$! Execute by entering:		$ @pseudocntrl
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
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
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
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
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module pseudocntrl ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to pseudocntrl.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("pseudocntrl.imake") .nes. ""
$   then
$      vimake pseudocntrl
$      purge pseudocntrl.bld
$   else
$      if F$SEARCH("pseudocntrl.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake pseudocntrl
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @pseudocntrl.bld "STD"
$   else
$      @pseudocntrl.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create pseudocntrl.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack pseudocntrl.com -mixed -
	-s CursorColorController.cc PseudoRGBController.cc -
	-i pseudocntrl.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create CursorColorController.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CursorColorController.cc: This class controlls the color of
//		the bw Swatch (see Swatch in MotifApp library)
///////////////////////////////////////////////////////////////
#include "CursorColorController.h"
#include "CursorModel.h"
#include "BasicWedgeOverlay.h"
#include "ColorModel.h"
#include "PseudoMarks.h"
#include "PseudoValue.h"
#include <Xm/Xm.h>
#include <stdio.h>
#include <stdlib.h>

CursorColorController::CursorColorController (
	Widget colorImage, Widget bwImage,
	ColorModel *colorModel, ColorModel *bwModel,
	BasicWedgeOverlay *wedgeView, 
	PseudoMarks *pseudoMarks, PseudoValue *pseudoValue )
{
	_colorImage 	= colorImage;
	_bwImage	= bwImage;
	_bwModel 	= bwModel;
	_colorModel 	= colorModel;
	_wedgeView 	= wedgeView;
	_pseudoMarks 	= pseudoMarks;
	_pseudoValue 	= pseudoValue;
	_firstTime	= True;

        // Set all callbacks for tracking the cursor
        startTracking();
}

void CursorColorController::startTracking()
{
	// Add input callback to mark and move the points
	XtAddCallback ( _colorImage,
		XvicNinputCallback,
                &CursorColorController::inputCallback, 
		(XtPointer) this);

	XtAddCallback ( _bwImage,
		XvicNinputCallback,
		&CursorColorController::inputCallback,
		(XtPointer) this);

	// Add cursor callback to track the current dn value
	XtAddCallback ( _colorImage,
                XvicNcursorCallback,
                &CursorColorController::cursorCallback,
                (XtPointer) this);

        XtAddCallback ( _bwImage,
                XvicNcursorCallback,
                &CursorColorController::cursorCallback,
                (XtPointer) this);
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user moves the cursor
////////////////////////////////////////////////////////////////////////
void CursorColorController::cursorCallback(Widget, XtPointer clientData,
                                    XtPointer callData)
{
   CursorColorController *obj = (CursorColorController *)clientData;
   obj->cursor(callData);
}

void CursorColorController::cursor(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_CURSOR)
      return;                           // oops

   if ( (cb->x >= 0) && (cb->x <= 255) ) {
	getColorValues( cb->x, _red1, _grn1, _blu1 );
	_colorModel->setRgb ( _red1, _grn1, _blu1 );
	_dn1 = getBWValue( cb->x );
	_bwModel->setRgb ( _dn1, _dn1, _dn1 );
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from our widget when the user takes an action
////////////////////////////////////////////////////////////////////////
void CursorColorController::inputCallback(Widget, XtPointer clientData,
                                    XtPointer callData)
{
   CursorColorController *obj = (CursorColorController *)clientData;
   obj->input(callData);
}

void CursorColorController::input(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_INPUT)
      return;                           // oops

   if (cb->input_num_params != 2)
      return;                           // oops

   if (strcmp(cb->input_params[0], "mark_point") == 0) {
      if (strcmp(cb->input_params[1], "start") == 0) {
	getColorValues( cb->x, _red, _grn, _blu );
	if ( (cb->x >= 0) && (cb->x <= 255) ) {
	   if (_pseudoMarks->addMark(cb->x, 4, _red, _grn, _blu) == 1) {
		_red = _pseudoMarks->getRed();
		_grn = _pseudoMarks->getGrn();
		_blu = _pseudoMarks->getBlu();
	   }
	   _colorModel->setRgb ( _red, _grn, _blu );
	   _dn = getBWValue( cb->x );
	   _bwModel->setRgb ( _dn, _dn, _dn );
	}
      }
      else if (strcmp(cb->input_params[1], "drag") == 0) {
	if ( (cb->x >= 0) && (cb->x < 255) ) {
	   _pseudoMarks->moveMark(cb->x);
	}
      }
      else if (strcmp(cb->input_params[1], "release") == 0) {
	if ( (cb->x >= 0) && (cb->x <= 255) ) {
	   _itype = _pseudoMarks->getInterpolation();
	   _pseudoMarks->addMark(cb->x, 0, _red, _grn, _blu, _itype);
           _pseudoValue->setRedDn(cb->x, _red);
           _pseudoValue->setGrnDn(cb->x, _grn);
           _pseudoValue->setBluDn(cb->x, _blu);
           _colorModel->setRgb ( _red, _grn, _blu );
	   _dn = getBWValue( cb->x );
           _bwModel->setRgb ( _dn, _dn, _dn );
	}
	else 
	   _pseudoMarks->deleteMark();
	   _red = _pseudoMarks->getRed();
	   _grn = _pseudoMarks->getGrn();
	   _blu = _pseudoMarks->getBlu();
	   _colorModel->setRgb ( _red, _grn, _blu );
	   int newdn = _pseudoMarks->getDn(_pseudoMarks->getCurrent());
	   _dn = getBWValue( newdn );
	   _bwModel->setRgb ( _dn, _dn, _dn );
      }
   }

   if (strcmp(cb->input_params[0], "mark_interval") == 0) {
        if (strcmp(cb->input_params[1], "start") == 0) {
          _pseudoMarks->markInterval(cb->x);
        }
        else if (strcmp(cb->input_params[1], "drag") == 0) {
          _pseudoMarks->resizeInterval(cb->x);
        }
   }

}

void CursorColorController::getColorValues(int pos, int &red, int &grn, int &blu)
{
	int r[256], g[256], b[256];

	XvicImageGetColorLUT ( _colorImage, r, g, b );

        red = r[pos];
	grn = g[pos];
	blu = b[pos];
}

int CursorColorController::getBWValue(int x)
{
	return x;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PseudoRGBController.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// PseudoRGBController.cc: This class derives from RGBController
//			   The only difference is that it changes 
//			   not only ColorModel, but also
//			   PseudoMarks value for the 
//			   current mark as user drags the slider
/////////////////////////////////////////////////////////////
#include "PseudoRGBController.h"
#include "PseudoMarks.h"
#include "ColorModel.h"

PseudoRGBController::PseudoRGBController ( Widget parent, 
			 ColorModel *model, const char *name,
			 PseudoMarks *marks )
			  : RGBController ( parent, model, name )
{
    _pseudoMarks = marks;
}

void PseudoRGBController::redChanged ( int value )
{
    _model->setRed ( value );
    _pseudoMarks->setRed ( value );
}

void PseudoRGBController::greenChanged ( int value )
{
    _model->setGreen ( value );
    _pseudoMarks->setGrn ( value );
}

void PseudoRGBController::blueChanged ( int value )
{
    _model->setBlue ( value );
    _pseudoMarks->setBlu ( value );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create pseudocntrl.imake
#define SUBROUTINE pseudocntrl
#define MODULE_LIST PseudoRGBController.cc \
 CursorColorController.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_P2SUB
$ Return
$!#############################################################################
