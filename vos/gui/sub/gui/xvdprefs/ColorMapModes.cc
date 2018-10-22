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

