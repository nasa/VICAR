$!****************************************************************************
$!
$! Build proc for MIPL module xvdprefs
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:46
$!
$! Execute by entering:		$ @xvdprefs
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
$ write sys$output "*** module xvdprefs ***"
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
$ write sys$output "Invalid argument given to xvdprefs.com file -- ", primary
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
$   if F$SEARCH("xvdprefs.imake") .nes. ""
$   then
$      vimake xvdprefs
$      purge xvdprefs.bld
$   else
$      if F$SEARCH("xvdprefs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xvdprefs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xvdprefs.bld "STD"
$   else
$      @xvdprefs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xvdprefs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xvdprefs.com -mixed -
	-s BorderCmd.cc ColorMapCmd.cc ColorMapModes.cc DitherCmd.cc -
	   LookupTableCmd.cc PrefDialog.cc MenuBarCmd.cc -
	   FullScreenDisplayCmd.cc -
	-i xvdprefs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create BorderCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// BorderCmd.cc: Turn the Motif border on and off for the shell
// ancestor of any BasicComponent subclass.
///////////////////////////////////////////////////////////
#include "BorderCmd.h"
#include "BasicComponent.h"
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>

BorderCmd::BorderCmd(const char *name, int active, BasicComponent *component)
		: Cmd(name, active)
{
   // Find the shell widget for this component.

   _shell = component->baseWidget();
   if (_shell && !XtIsShell(_shell)) {
      do {
         _shell = XtParent(_shell);
      } while (_shell && !XtIsShell(_shell));
   }

   if (_shell == NULL)
      return;				// huh?

   // Now check and save the current state of the borders.  If anything is
   // on, we assume that's the set the user wants when borders are enabled.
   // If nothing is on, we assume that borders are off and set up to display
   // all when turned back on.

   XtVaGetValues(_shell, XmNmwmDecorations, &_decorations, NULL);

   if (_decorations == 0) {
      _value = (CmdValue) False;
      _decorations = (int)MWM_DECOR_ALL;
   }
   else {
      _value = (CmdValue) True;
   }
   _lastValue = _value;
   newValue();
}

void BorderCmd::doit()
{
   int decor;

   // Save previous state for undo

   XtVaGetValues(_shell, XmNmwmDecorations, &decor, NULL);
   if (decor == 0)
      _lastValue = (CmdValue) False;
   else
      _lastValue = (CmdValue) True;

   if (_shell == NULL)
      return;			// huh?

   // We should be able to simply set XmNmwmDecorations and be done with it.
   // Alas, this is not enough, because mwm is too stupid to notice
   // _MOTIF_WM_HINTS property changes.  So, we must unmap and remap
   // the window instead, since it will notice when it receives a
   // MappingNotify event.  We must check the map state first so we
   // don't e.g. uniconify a window just by mapping it!
   // Also, if the window is iconified, mwm is too stupid to set the
   // state correctly when it is uniconified.  Holy cow.  So, we simply
   // don't allow the state to be changed in that case.  A little weird
   // but what else are you supposed to do about Motif bugs?

   if (XtIsRealized(_shell)) {

      XWindowAttributes attr;
      XGetWindowAttributes(XtDisplay(_shell), XtWindow(_shell), &attr);
      if (attr.map_state == IsViewable) {

         if (_value)				// Turn the border on
            XtVaSetValues(_shell, XmNmwmDecorations, _decorations, NULL);
         else					// Turn the border off
            XtVaSetValues(_shell, XmNmwmDecorations, 0, NULL);

         XtUnmapWidget(_shell);
         XtMapWidget(_shell);
      }
      else {
         _value = _lastValue;		// It's iconic, don't allow the change
         newValue();
      }
   }
}

void BorderCmd::undoit()
{
   _value = _lastValue;
   newValue();
   doit();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ColorMapCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// ColorMapCmd.cc: Radio Command Class to set the
//      XvicNcolormapPolicy  resource to the
//      value stored in _colorMapMode.  The dither mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "ColorMapCmd.h"
#include "ColorMapModes.h"
#include "XvicBasicImage.h"
#include <iostream>
using namespace std;
#include <stdio.h>

ColorMapCmd::ColorMapCmd ( const char *name, int active,
   unsigned char colorMapValue,
   Widget widget, ColorMapModes *colorMapModes, CmdList *list=NULL ) 
  : RadioCmd ( name, active, list )
{
  unsigned char policy;

    _imageWidget = widget;
    _colorMapValue = colorMapValue;
    _colorMapModes = colorMapModes;

    XtVaGetValues( _imageWidget, XvicNcolormapPolicy, &policy, NULL);
 
    switch(policy) {
      case XvicFULL: 
      case XvicFULL_COLOR: if ( _colorMapValue==XvicFULL || 
			       _colorMapValue==XvicFULL_COLOR) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      case XvicHALF:       if ( _colorMapValue==XvicHALF) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      case XvicDITHER: 
      case XvicALLOC:      if ( _colorMapValue==XvicDITHER ||
			       _colorMapValue==XvicALLOC ) setValue(TRUE);
			   else setValue(FALSE);
			   break;
      default:             printf("Unknown ColorMapPolicy %d\n",policy);
    }

}

void ColorMapCmd::doit()
{

//  unsigned char value;


  if (_value) {
    _colorMapModes->SetColorMapModes();
    _colorMapModes->SetColorMapButtons();
  }
}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ColorMapModes.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// ColorMapModes.cc:  Class to set the ColorMap Modes for
//      the Image Display
//////////////////////////////////////////////////////////
#include "DitherCmd.h"
#include "ColorMapCmd.h"
#include "ColorMapModes.h"
#include "LookupTableCmd.h"
#include "XvicBasicImage.h"
#include "Cmd.h"
#include "CmdList.h"
#include "Application.h"
#include "ImageWindow.h"
#include <iostream>
using namespace std;

/*********************************************************************/
/*  ColorMapModes contstructor which saves the color levels defined  */
/*  by user resources in ImageWindow, the image Widget defined in    */
/*  ImageWindow and get the current values of the color map color    */
/*  levels defined by the image view.  Also, register a callback     */
/*  with the widget so we can be notified of display mode changes,   */
/*  so the option menus can be kept consistent (a bw<->color<->pseudo*/
/*  mode change can now change display modes).                       */
/*********************************************************************/

ColorMapModes::ColorMapModes ( Widget widget, int userRed, int userGreen,
   int userBlue, int userGray )
{
    _imageWidget = widget;

    _allocRedLevels = userRed;
    _allocGreenLevels = userGreen;
    _allocBlueLevels = userBlue;
    _allocGrayLevels = userGray;

   XtVaGetValues( _imageWidget, XvicNredLevels, &_redLevels,
                                XvicNgreenLevels, &_greenLevels,
                                XvicNblueLevels, &_blueLevels,
                                XvicNgrayLevels, &_grayLevels, NULL);

   XtAddCallback(_imageWidget, XvicNvisibleAreaCallback,
		&ColorMapModes::modeChangedCallback, (XtPointer) this);
}

/*********************************************************************/
/*  SetColorMapModes is a routine to determines the modes for setting*/
/*  the display colormaps based on the selected options from the     */
/*  Preferences Dialog for Dither Mode and the ColorMap Policy.      */
/*  SetColorMapButtons should be called after this routine to ensure */
/*  that the requested modes and policies were allowed and if not    */
/*  to set the option menu buttons to those that it reset them to    */
/*  if the requested modes and policies were not allowed.            */
/*********************************************************************/
void ColorMapModes::SetColorMapModes()
{
  unsigned char imageMode;
  unsigned char colorMapPolicy = XvicALLOC;
  unsigned char ditherMode;
  int redLevels, greenLevels, blueLevels, grayLevels;
  enum leveltype { defLevels, halfLevels, allocLevels, BWgrayLevels };
  enum leveltype level;


    ditherMode = XvicNONE;   // DEFAULT SETTINGS.
    level = defLevels;

//  FIND CURRENT STATE OF RadioButtons FOR THE  DitherMode AND ColorMap Policy

   int ditherSize = _ditherRadList->size();
   int colorMapSize = _colorMapRadList->size();

   int i;
   DitherCmd * tempDitherPtr;
   ColorMapCmd * tempCMapPtr;

   for ( i = 0; i < ditherSize && ! _ditherCmds[i]->getValue(); i++);

//   printf ("Dither Index = %d\n", i);
   if (i < ditherSize ) {
     tempDitherPtr = (DitherCmd *)_ditherCmds[i];
     _ditherMode = tempDitherPtr->getDitherMode();
   }

   for ( i = 0; i < colorMapSize && ! _colorMapCmds[i]->getValue(); i++);

//   printf ("CMap Index = %d\n", i);
   if (i < colorMapSize) {
     tempCMapPtr = (ColorMapCmd *)_colorMapCmds[i];
     _colorMapPolicy = tempCMapPtr->getColorMapMode();
   }

   XtVaGetValues( _imageWidget, XvicNimageMode, &imageMode, NULL);


   if (_colorMapPolicy == XvicFULL ) {		/* Full */
      colorMapPolicy = XvicFULL;
      if (_ditherMode == XvicNONE)			/* None */
         ditherMode = XvicNONE;
      else if (_ditherMode == XvicORDERED)   		/* Ordered */
         ditherMode = XvicORDERED;
      else {						/* Kagels */
         colorMapPolicy = XvicDITHER;
         ditherMode = XvicKAGELS;
      }
   }
   else if (_colorMapPolicy == XvicHALF) {	/* Half */
      colorMapPolicy = XvicHALF;
      if (_ditherMode == XvicNONE)			/* None */
         ditherMode = XvicNONE;
      else if (_ditherMode == XvicORDERED)			/* Ordered */
         ditherMode = XvicORDERED;
      else {						/* Kagels */
         colorMapPolicy = XvicDITHER;
         ditherMode = XvicKAGELS;
         level = halfLevels;
      }
   }
   else {					/* Alloc */
      colorMapPolicy = XvicALLOC;
      ditherMode = XvicKAGELS;
      if ( imageMode == XvicCOLOR ) {		/* Color */
	 level = allocLevels;
      }
      else {				/* BW */
	 level = BWgrayLevels;
         if (_ditherMode == XvicNONE) {
            ditherMode = XvicNONE;
         } else if (_ditherMode == XvicORDERED)
            ditherMode = XvicORDERED;
         else {
            ditherMode = XvicKAGELS;
	    level = allocLevels;
         }
      }
   }
//
//  DETERMINE COLOR LEVELS BASED ON IF IT IS THE DEFAULT VALUES DEFINED 
//  WITHIN THE PROGRAM, THE HALF LEVELS OR THE GRAY LEVELS OR THE ALLOCATED
//  COLOR LEVELS (CAN BE USER DEFINED WITH RESOURCES)
//   
  switch (level) {
  case defLevels:    redLevels = 16;
		     greenLevels = 16;
		     blueLevels = 13;
		     grayLevels = 16;
		     break;
  case halfLevels:   redLevels = 12;
		     greenLevels = 12;
		     blueLevels = 9;
		     grayLevels = 12;
		     break;
  case allocLevels:  redLevels = _allocRedLevels;
		     greenLevels = _allocGreenLevels;
		     blueLevels = _allocBlueLevels;
		     grayLevels = _allocGrayLevels;
		     break;
  case BWgrayLevels: redLevels = 0;
		     greenLevels = 0;
		     blueLevels = 0;
		     grayLevels = 16;
		     break;
  }  
  XtVaSetValues(_imageWidget,
	XvicNcolormapPolicy, colorMapPolicy,
	XvicNditherMode, ditherMode,
	XvicNredLevels, redLevels,
	XvicNgreenLevels, greenLevels,
	XvicNblueLevels, blueLevels,
	XvicNgrayLevels, grayLevels,
	NULL);
}
/****************************************************************************/
/*  SetColorMapButtons is a routine to set the Preferences option menues    */
/*  buttons to their current state.                                         */
/****************************************************************************/
void ColorMapModes::SetColorMapButtons()
{
  unsigned char imageMode, colorMapPolicy, ditherMode, stretchPolicy;
  int redLevel, greenLevel, blueLevel, grayLevel;


  XtVaGetValues( _imageWidget,                   //  Get current Settings
	XvicNimageMode, &imageMode,
	XvicNcolormapPolicy, &colorMapPolicy,
	XvicNditherMode, &ditherMode,
	XvicNredLevels, &redLevel,
	XvicNgreenLevels, &greenLevel,
	XvicNblueLevels, &blueLevel,
	XvicNgrayLevels, &grayLevel,
        XvicNstretchPolicy, &stretchPolicy,
	NULL);
//  printf("             PrevSet WidgetSet\n");
//  printf("Image Mode =   %d          \n",imageMode);
//  printf("ClrMap Pol =   %d        %d\n",_colorMapPolicy, colorMapPolicy);
//  printf("ditherMode =   %d        %d\n",_ditherMode, ditherMode);
//  printf("redLevels  =   %d        %d\n",_redLevels, redLevel);
//  printf("greenLevel =   %d        %d\n",_greenLevels, greenLevel);
//  printf("blueLevel  =   %d        %d\n",_blueLevels, blueLevel);
//  printf("grayLevel  =   %d        %d\n",_grayLevels, grayLevel);
//  printf("stretchPol =   %d        %d\n",_stretchPolicy, stretchPolicy);

  ColorMapCmd *tempCMapPtr;

  tempCMapPtr = (ColorMapCmd *)_colorMapCmds[0];   // Set Command Interfaces
  tempCMapPtr->setValue(FALSE);                    // for ColorMap Option Menu
  tempCMapPtr = (ColorMapCmd *)_colorMapCmds[1];
  tempCMapPtr->setValue(FALSE);
  tempCMapPtr = (ColorMapCmd *)_colorMapCmds[2];
  tempCMapPtr->setValue(FALSE);

  if ( colorMapPolicy != _colorMapPolicy ) {      // Set ColorMap Buttons
    switch (colorMapPolicy) {
      case XvicFULL:
      case XvicFULL_COLOR: tempCMapPtr = (ColorMapCmd *)_colorMapCmds[0]; 
		           tempCMapPtr->setValue(TRUE);
			   colorMapPolicy = XvicFULL;
		           break;
      case XvicHALF:       tempCMapPtr = (ColorMapCmd *)_colorMapCmds[1];
		           tempCMapPtr->setValue(TRUE);
		           break;
      case XvicALLOC:      tempCMapPtr = (ColorMapCmd *)_colorMapCmds[2];
		           tempCMapPtr->setValue(TRUE);
		           break;
      case XvicDITHER:     int limit;
			   limit = grayLevel + greenLevel + (redLevel*blueLevel);
	                   if ( limit > 132 ) {
	                     tempCMapPtr = (ColorMapCmd *)_colorMapCmds[0];
			     colorMapPolicy = XvicFULL;
			   } else {
		             tempCMapPtr = (ColorMapCmd *)_colorMapCmds[1];
		             colorMapPolicy = XvicHALF;
		           }
			   tempCMapPtr->setValue(TRUE);
			   break;
    }
    _colorMapPolicy = colorMapPolicy;
  }


  DitherCmd *tempDitherPtr;

  tempDitherPtr = (DitherCmd *)_ditherCmds[0];  // Set the command interfaces.
  tempDitherPtr->setValue(FALSE);               // for DitherMode option menu
  tempDitherPtr = (DitherCmd *)_ditherCmds[1];
  tempDitherPtr->setValue(FALSE);
  tempDitherPtr = (DitherCmd *)_ditherCmds[2];
  tempDitherPtr->setValue(FALSE);

  if ( ditherMode != _ditherMode) {
      switch (ditherMode) {
        case XvicNONE:    tempDitherPtr = (DitherCmd *)_ditherCmds[0];
		          tempDitherPtr->setValue(TRUE);
		          break;
        case XvicORDERED: tempDitherPtr = (DitherCmd *)_ditherCmds[1];
		          tempDitherPtr->setValue(TRUE);
			  break;
        case XvicKAGELS:  tempDitherPtr = (DitherCmd *)_ditherCmds[2];
		          tempDitherPtr->setValue(TRUE);
			  break;
      }
      _ditherMode = ditherMode;
  }

// Set the command interfaces for the StretchPolicy option menu

  LookupTableCmd *tempStretchPtr;

  tempStretchPtr = (LookupTableCmd *)_stretchPolicyCmds[0];
  tempStretchPtr->setValue(FALSE);
  tempStretchPtr = (LookupTableCmd *)_stretchPolicyCmds[1];
  tempStretchPtr->setValue(FALSE);

  if ( stretchPolicy != _stretchPolicy) {
      switch ( stretchPolicy ) {
        case XvicUSE_HW:  tempStretchPtr = (LookupTableCmd *)_stretchPolicyCmds[0];
		          tempStretchPtr->setValue(TRUE);
		          break;
        case XvicUSE_SW:  tempStretchPtr = (LookupTableCmd *)_stretchPolicyCmds[1];
		          tempStretchPtr->setValue(TRUE);
		          break;
      }
      _stretchPolicy = stretchPolicy;
  }

  _redLevels = redLevel;
  _greenLevels = greenLevel;
  _blueLevels = blueLevel;
  _grayLevels = grayLevel;
}

/*********************************************************************/
/*  addRadLists is a routine to add the Radio Buttons lists for      */
/*  the Dither Mode, the ColorMap policy and the Stretch policy      */
/*  created in the calling routine to be added after the ColorMapModes*/
/*  constructor has already been called.			     */
/*  Also the pointers to the Commands is set			     */
/*********************************************************************/
void ColorMapModes::addRadLists( CmdList *ditherList, CmdList *colorMapList,
				CmdList *stretchPolicyList )
{

  _ditherRadList = ditherList;
  _colorMapRadList = colorMapList;
  _stretchPolicyRadList = stretchPolicyList;
  _ditherCmds = _ditherRadList->contents(); 
  _colorMapCmds = _colorMapRadList->contents();
  _stretchPolicyCmds = _stretchPolicyRadList->contents();
}

/************************************************************************/
/* Callback to let us know something in the widget changed.		*/
/************************************************************************/

void ColorMapModes::modeChangedCallback(Widget, XtPointer clientData,
						XtPointer callData)
{
   ColorMapModes *obj = (ColorMapModes *)clientData;

   obj->modeChanged(callData);
}

/************************************************************************/
/* Check to see if the display modes changed and if so, update the	*/
/* option menus.							*/
/************************************************************************/

void ColorMapModes::modeChanged(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)callData;

   if (cb->reason != XvicCR_VISIBLE_AREA)
      return;				// Shouldn't happen

   if ((cb->flags & XvicDITHER_CHANGED) || (cb->flags & XvicMODE_CHANGED)) {
      SetColorMapButtons();
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DitherCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// DitherCmd.cc: Radio Command Class to set the
//      XvicNditherMode resource to the
//      value stored in _ditherMode.  The dither mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "DitherCmd.h"
#include "XvicBasicImage.h"
#include "ColorMapModes.h"
#include <iostream>
using namespace std;

DitherCmd::DitherCmd ( const char *name, int active, unsigned char ditherMode, 
  Widget widget, ColorMapModes *colorMapModes, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
  unsigned char value;

    _imageWidget = widget;
    _ditherMode = ditherMode;
    _colorMapModes = colorMapModes;

    XtVaGetValues( _imageWidget, XvicNditherMode, &value, NULL);

    if ( value == _ditherMode) setValue(TRUE);
    else setValue(FALSE);

}

void DitherCmd::doit()
{

  if (_value) {
    _colorMapModes->SetColorMapModes();
    _colorMapModes->SetColorMapButtons();
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LookupTableCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// LookupTableCmd.cc: Radio Command Class to set the
//      XvicNstretchPolicy resource to the
//      value stored in _LUTMode.  The LUT mode is
//      is found from the option menu which is under the 
//      Preferences Menu
//////////////////////////////////////////////////////////
#include "ColorMapModes.h"
#include "LookupTableCmd.h"
#include "XvicBasicImage.h"
#include <iostream>
using namespace std;

LookupTableCmd::LookupTableCmd ( const char *name, int active,
  unsigned char LUTMode, 
  Widget widget, ColorMapModes * colorMap, CmdList *list=NULL ) 
  : RadioCmd ( name, active, list )
{
  unsigned char value;

    _imageWidget = widget;
    _LUTMode = LUTMode;
    _colorMapModes = colorMap;

    XtVaGetValues( _imageWidget, XvicNstretchPolicy, &value, NULL);
    if ( value == _LUTMode) {
      _value = ( CmdValue ) TRUE;
      newValue();
    } else {
      _value = ( CmdValue ) FALSE;
      newValue();
    }
}

void LookupTableCmd::doit()
{
  unsigned char policy;

  if (_value) {
    XtVaGetValues( _imageWidget, XvicNstretchPolicy, &policy, NULL);
    if ( policy != _LUTMode ) {
       XtVaSetValues( _imageWidget, XvicNstretchPolicy, _LUTMode, NULL );
       _colorMapModes->SetStretchPolicy( _LUTMode );
       _colorMapModes->SetColorMapButtons();
     }
  }
}      


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create PrefDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//PrefDialog.cc: Source file for creating Preferences Dialog Box
////////////////////////////////////////////////////////////////////////
#include "PrefDialog.h"
#include "XvicBasicImage.h"
#include "CheckBoxInterface.h"
#include "SeparatorInterface.h"
#include "SeparatorCmd.h"
#include "Cmd.h"
#include "CmdList.h"
#include "OptionCmdMenu.h"
#include "DitherCmd.h"
#include "ColorMapCmd.h"
#include "ColorMapModes.h"
#include "LookupTableCmd.h"
#include <Xm/RowColumn.h>
#include "xvmaininc.h"		// for UNUSED


PrefDialog::PrefDialog(const char *name, Cmd *sideBar , Cmd *border,
	Cmd * menuBar, 
	Cmd * fullScreen, Widget imageWidget, BasicImageView *imageView,
	int redLevel, int greenLevel, int blueLevel, int grayLevel)
	: CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{

   _showSideBarCmd = sideBar;
   _showBorderCmd = border;
   _showMenuBarCmd = menuBar;
   _showFullScreenCmd = fullScreen;
   _imageViewWidget = imageWidget;
   _imageView = imageView;
   _CMMRedLevels = redLevel;
   _CMMGreenLevels = greenLevel;
   _CMMBlueLevels = blueLevel;
   _CMMGrayLevels = grayLevel;
}

Widget PrefDialog::createWorkArea(Widget parent)
{
   Widget rc = XtVaCreateWidget("PrefDialog", xmRowColumnWidgetClass, parent,
				XmNorientation, XmVERTICAL, NULL);

//  MAKE COMMANDS FOR THE Preference Dialog
//  Get a pointer to the ColorMapModes class to be used by the Option Menu
//  commands

    ColorMapModes *colorMapModes = new ColorMapModes( _imageViewWidget, 
       _CMMRedLevels, _CMMGreenLevels, _CMMBlueLevels, _CMMGrayLevels);

// MAKE THE COMMAND LIST FOR THE OPTION MENU Dither Mode

    CmdList *ditherList = new CmdList();

    Cmd *UNUSED(ditherNoneCmd) = new DitherCmd( "None", TRUE, XvicNONE, 
			   _imageViewWidget, colorMapModes, ditherList );
    Cmd *UNUSED(ditherOrderedCmd) = new DitherCmd( "Ordered", TRUE, XvicORDERED,
			   _imageViewWidget, colorMapModes, ditherList );
    Cmd *UNUSED(ditherKagelsCmd) = new DitherCmd( "Kagels", TRUE, XvicKAGELS,
			   _imageViewWidget, colorMapModes, ditherList );

// MAKE THE COMMAND LIST FOR THE OPTION MENU ColorMap Policy

    CmdList *colorMapList = new CmdList();

    Cmd *UNUSED(colorMapAllCmd) = new ColorMapCmd( "Full", TRUE, XvicFULL, 
			       _imageViewWidget, colorMapModes, colorMapList );
    Cmd *UNUSED(colorMapHalfCmd) = new ColorMapCmd( "Half", TRUE, XvicHALF,
			       _imageViewWidget, colorMapModes, colorMapList );
    Cmd *UNUSED(colorMapAllocateCmd) = new ColorMapCmd( "Allocate", TRUE, XvicALLOC,
			       _imageViewWidget, colorMapModes, colorMapList );

// MAKE THE COMMAND LIST FOR THE OPTION MENU LOOKUP Table/Stretch Policy

    CmdList *LUTList = new CmdList();

    Cmd *UNUSED(hardwareLUTCmd) = new LookupTableCmd( "H/W Lookup Table", TRUE,
					      XvicUSE_HW, _imageViewWidget,
					      colorMapModes, LUTList);

    Cmd *UNUSED(softwareLUTCmd) = new LookupTableCmd( "S/W Lookup Table", TRUE,
					      XvicUSE_SW, _imageViewWidget,
					      colorMapModes, LUTList);

    colorMapModes->addRadLists( ditherList, colorMapList, LUTList );
    colorMapModes->SetColorMapButtons();



// Create CheckBoxes and Option Menues for dialog

   CmdInterface *ci1 = new CheckBoxInterface(rc, _showFullScreenCmd);
   CmdInterface *ci2 = new CheckBoxInterface(rc, _showBorderCmd);
   CmdInterface *ci3 = new CheckBoxInterface(rc, _showMenuBarCmd);
   CmdInterface *ci4 = new CheckBoxInterface(rc, _showSideBarCmd);
   OptionCmdMenu *ci5 = new OptionCmdMenu(rc, "Dither Mode", ditherList, 
					  _applyCmdList);
   OptionCmdMenu *ci6 = new OptionCmdMenu(rc, "ColorMap Mode", colorMapList, 
					  _applyCmdList);
   OptionCmdMenu *ci7 = new OptionCmdMenu(rc, "LUT Mode", LUTList, 
					  _applyCmdList);

   //  Manage Commands

   ci1->manage();
   ci2->manage();
   ci3->manage();
   ci4->manage();
   ci5->manage();
   ci6->manage();
   ci7->manage();

   //  Set commands to defer command execution until "Apply" button is pressed

   ci1->setDeferredExec(_applyCmdList);
   ci2->setDeferredExec(_applyCmdList);
   ci3->setDeferredExec(_applyCmdList);
   ci4->setDeferredExec(_applyCmdList);

   return rc;
}






$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create MenuBarCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// MenuBarCmd.cc: Creates the MenuBar Command for turning a
//                MenuWindow's menu bar on and off
///////////////////////////////////////////////////////////
#include "MenuBarCmd.h"
#include "MenuWindow.h"
#include <stdint.h>

MenuBarCmd::MenuBarCmd(const char *name, int active, MenuWindow *obj,
			Boolean initial) // = True
	: Cmd(name, active)
{
   // Save the menu window and set the initial state.  We can't just ask the
   // window for its state because it might not be managed yet when this
   // command is created!

   _menuWindow = obj;
   int value = (int)initial;
   _value = (CmdValue) (uintptr_t) value;
   newValue();

}

void MenuBarCmd::doit()
{
   // Save the old value for the undoit command and change to
   //  the new value;

   _oldValue = _menuWindow->isMenuBarVisible();
   if (_value)
      _menuWindow->showMenuBar();
   else
      _menuWindow->hideMenuBar();
}      

void MenuBarCmd::undoit()
{

   // Undo the command to the last state

   if (_oldValue)
      _menuWindow->showMenuBar();
   else
      _menuWindow->hideMenuBar();
   _value = (CmdValue) (uintptr_t) _oldValue;
   newValue();

}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FullScreenDisplayCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// FullScreenDisplayCmd.cc: Turn off all decorations and make the
// image cover the entire screen.
///////////////////////////////////////////////////////////
#include "FullScreenDisplayCmd.h"
#include "ImageDisplayView.h"

FullScreenDisplayCmd::FullScreenDisplayCmd(const char *name, int active,
		Cmd *borderCmd, Cmd *sideBarCmd, Cmd *menuBarCmd,
		ImageDisplayView *imageView)
	: Cmd(name, active)
{
   // Save the Command's for use later

   _borderCmd = borderCmd;
   _sideBarCmd = sideBarCmd;
   _menuBarCmd = menuBarCmd;
   _imageView = imageView;

   _shell = _imageView->baseWidget();
   if (_shell && !XtIsShell(_shell)) {
      do {
         _shell = XtParent(_shell);
      } while (_shell && !XtIsShell(_shell));
   }

   _borderCmdStoreValue = _borderCmd->getValue();
   _sideBarCmdStoreValue = _sideBarCmd->getValue();
   _menuBarCmdStoreValue = _menuBarCmd->getValue();
   _imageView->getViewSize(_storeViewWidth, _storeViewHeight);
   if (_shell)
      XtVaGetValues(_shell, XmNx, &_storeX, XmNy, &_storeY, NULL);
   _storedValues = False;
}

void FullScreenDisplayCmd::doit()
{
   // Save the current state so we can restore it for Undo

   _borderCmdSaveValue = _borderCmd->getValue();
   _sideBarCmdSaveValue = _sideBarCmd->getValue();
   _menuBarCmdSaveValue = _menuBarCmd->getValue();
   _imageView->getViewSize(_saveViewWidth, _saveViewHeight);
   if (_shell)
      XtVaGetValues(_shell, XmNx, &_saveX, XmNy, &_saveY, NULL);
   _saveStoredValues = _storedValues;

   // Execute the three Commands to turn on or off all decorations

   if (_value) {

      // Save the current state for when we turn this mode off (note: this
      // is different from undo!)

      if (!_storedValues) {
         _borderCmdStoreValue = _borderCmd->getValue();
         _sideBarCmdStoreValue = _sideBarCmd->getValue();
         _menuBarCmdStoreValue = _menuBarCmd->getValue();
         _imageView->getViewSize(_storeViewWidth, _storeViewHeight);
         if (_shell)
            XtVaGetValues(_shell, XmNx, &_storeX, XmNy, &_storeY, NULL);
         _storedValues = True;
      }

      // Turn everything off

      _borderCmd->execute((CmdValue) False);
      _sideBarCmd->execute((CmdValue) False);
      _menuBarCmd->execute((CmdValue) False);

      // Now set the display's view size and shell position

      _imageView->setViewSize(
		XDisplayWidth(XtDisplay(_imageView->getWidget()),
			    XDefaultScreen(XtDisplay(_imageView->getWidget()))),
		XDisplayHeight(XtDisplay(_imageView->getWidget()),
			    XDefaultScreen(XtDisplay(_imageView->getWidget()))),
		False);				// No scrollbars
      if (_shell)
         XtVaSetValues(_shell, XmNx, 0, XmNy, 0, NULL);
   }

   else {
      _borderCmd->execute(_borderCmdStoreValue);
      _sideBarCmd->execute(_sideBarCmdStoreValue);
      _menuBarCmd->execute(_menuBarCmdStoreValue);

      // Set scrollbars if needed
      _imageView->setViewSize(_storeViewWidth, _storeViewHeight, True);
      if (_shell)
         XtVaSetValues(_shell, XmNx, _storeX, XmNy, _storeY, NULL);

      _storedValues = False;
   }
}      

void FullScreenDisplayCmd::undoit()
{

   // Undo the command to the last state

   _borderCmd->execute(_borderCmdSaveValue);
   _sideBarCmd->execute(_sideBarCmdSaveValue);
   _menuBarCmd->execute(_menuBarCmdSaveValue);

   // Set scrollbars if needed
   _imageView->setViewSize(_saveViewWidth, _saveViewHeight, True);
   if (_shell)
      XtVaSetValues(_shell, XmNx, _saveX, XmNy, _saveY, NULL);

   _storedValues = _saveStoredValues;

   if (!_borderCmdSaveValue && !_sideBarCmdSaveValue && !_menuBarCmdSaveValue)
      _value = (CmdValue) True;
   else
      _value = (CmdValue) False;
   newValue();

}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xvdprefs.imake
#define SUBROUTINE xvdprefs
#define MODULE_LIST BorderCmd.cc ColorMapCmd.cc ColorMapModes.cc DitherCmd.cc \
   LookupTableCmd.cc PrefDialog.cc MenuBarCmd.cc FullScreenDisplayCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

/*#define DEBUG*/

$ Return
$!#############################################################################
