$!****************************************************************************
$!
$! Build proc for MIPL module datarange
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:23
$!
$! Execute by entering:		$ @datarange
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
$ write sys$output "*** module datarange ***"
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
$ write sys$output "Invalid argument given to datarange.com file -- ", primary
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
$   if F$SEARCH("datarange.imake") .nes. ""
$   then
$      vimake datarange
$      purge datarange.bld
$   else
$      if F$SEARCH("datarange.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake datarange
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @datarange.bld "STD"
$   else
$      @datarange.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create datarange.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack datarange.com -mixed -
	-s DataRangeDialog.cc DataRangeAutoCmd.cc DataRangeValueCmd.cc -
	   ImageToRangeDialogGlue.cc -
	-i datarange.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create DataRangeDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// DataRangeDialog.cc:  Create the Data Range dialog.
////////////////////////////////////////////////////////////////////////
#include "DataRangeDialog.h"
#include "CheckBoxInterface.h"
#include "Cmd.h"
#include "StringKeyinInterface.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <stdio.h>

DataRangeDialog::DataRangeDialog(const char *name, Cmd *minAutoCmd,
		Cmd *maxAutoCmd, Cmd *minValueCmd, Cmd *maxValueCmd)
	: CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
   _minAutoCmd = minAutoCmd;
   _maxAutoCmd = maxAutoCmd;
   _minValueCmd = minValueCmd;
   _maxValueCmd = maxValueCmd;
   _minInterface = NULL;
   _maxInterface = NULL;
   _saveMin = 0.0;
   _saveMax = 0.0;
}

Widget DataRangeDialog::createWorkArea(Widget parent)
{
   Widget form = XtVaCreateWidget("DataRangeDialog", xmFormWidgetClass,
		parent, NULL);

   Widget label = XtVaCreateManagedWidget("Set Data Range", xmLabelWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget label2= XtVaCreateManagedWidget("Auto Range", xmLabelWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, label,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget minForm = XtVaCreateManagedWidget("minForm", xmFormWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, label2,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   Widget maxForm = XtVaCreateManagedWidget("maxForm", xmFormWidgetClass,
		form,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, minForm,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   CmdInterface *ci1 = new CheckBoxInterface(minForm, _minAutoCmd);
   XtVaSetValues(ci1->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _minInterface = new StringKeyinInterface(minForm, _minValueCmd);
   XtVaSetValues(_minInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, ci1->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   CmdInterface *ci2 = new CheckBoxInterface(maxForm, _maxAutoCmd);
   XtVaSetValues(ci2->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   _maxInterface = new StringKeyinInterface(maxForm, _maxValueCmd);
   XtVaSetValues(_maxInterface->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, ci2->baseWidget(),
		XmNrightAttachment, XmATTACH_FORM,
		NULL);

   // Manage Interfaces

   ci1->manage();
   ci2->manage();
   _minInterface->manage();
   _maxInterface->manage();

   // Set commands to defer command execution until the apply button is pressed
   // Policy decision: don't defer this dialog box.  It feels better that way.

//   ci1->setDeferredExec(_applyCmdList);
//   ci2->setDeferredExec(_applyCmdList);
//   _minInterface->setDeferredExec(_applyCmdList);
//   _maxInterface->setDeferredExec(_applyCmdList);

   setDataRange(_saveMin, _saveMax);

   return form;
}

void DataRangeDialog::setDataRange(double min, double max)
{
   char buf[30];

   sprintf(buf, "%g", min);
   if (_minInterface)
      _minInterface->setValue((CmdValue)strdup(buf));
   else
      _saveMin = min;
   sprintf(buf, "%g", max);
   if (_maxInterface)
      _maxInterface->setValue((CmdValue)strdup(buf));
   else
      _saveMax = max;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataRangeAutoCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// DataRangeAutoCmd.h: Set or clear automatic determination of
// data range in the image model.
///////////////////////////////////////////////////////////
#include "DataRangeAutoCmd.h"
#include "ImageData.h"
#include <stdint.h>

DataRangeAutoCmd::DataRangeAutoCmd(const char *name,int active,ImageData *image,
					Boolean isMax)
		: Cmd(name, active)
{
   _image = image;
   _isMax = isMax;

   // Check current auto state.

   if (isMax)
      _value = (CmdValue) ((uintptr_t) _image->isMaxAuto());
   else
      _value = (CmdValue) ((uintptr_t) _image->isMinAuto());

   _lastValue = _value;
   newValue();
}

void DataRangeAutoCmd::doit()
{
   if (_isMax) {
      _lastValue = (CmdValue) ((uintptr_t) _image->isMaxAuto());	// save for undo
      _image->setMaxAuto((_value != 0));
   }
   else {
      _lastValue = (CmdValue) ((uintptr_t) _image->isMinAuto());	// save for undo
      _image->setMinAuto((_value != 0));
   }
}

void DataRangeAutoCmd::undoit()
{
   _value = _lastValue;
   newValue();
   doit();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DataRangeValueCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////
// DataRangeValueCmd.cc: Sets the data range to the value specified
// in the string CmdValue.  Also, turns off auto mode.
///////////////////////////////////////////////////////////
#include "DataRangeValueCmd.h"
#include "ImageData.h"
#include "UIComponent.h"		// only for strdup()
#include <stdlib.h>
#include <stdio.h>

DataRangeValueCmd::DataRangeValueCmd(const char *name, int active,
				ImageData *image, Cmd *autoCmd, Boolean isMax)
		: Cmd(name, active)
{
   char buf[30];
   double val;

   _image = image;
   _autoCmd = autoCmd;
   _isMax = isMax;

   // Get current value

   if (isMax)
      val = _image->getMaxDataRange();
   else
      val = _image->getMinDataRange();

   _lastValue = val;
   sprintf(buf, "%g", val);
   _value = (CmdValue)strdup(buf);
   newValue();
}

void DataRangeValueCmd::doit()
{
   double val;

   if (_isMax)					// save for undo
      _lastValue = _image->getMaxDataRange();
   else
      _lastValue = _image->getMinDataRange();
   _lastAuto = _autoCmd->getValue();

   if (_value)
      val = atof((char *)_value);
   else
      val = 0.0;

   if (_isMax)
      _image->setDataMax(val);
   else
      _image->setDataMin(val);
   _autoCmd->execute((CmdValue) False);		// turn off auto
}

void DataRangeValueCmd::undoit()
{
   char buf[30];

   _autoCmd->execute(_lastAuto);

   sprintf(buf, "%g", _lastValue);
   _value = (CmdValue)strdup(buf);
   newValue();
   doit();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToRangeDialogGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToRangeDialogGlue: class that serves as a "glue" class between an
// ImageData object and the Data Range dialog.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData,
// it tells the RangeDialog to update its min/max value displays.
// This class, even though it's a UIComponent, creates no widget,
// therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToRangeDialogGlue.h"
#include "DataRangeDialog.h"
#include "ImageData.h"

ImageToRangeDialogGlue::ImageToRangeDialogGlue(ImageData *model,
					DataRangeDialog *dialog)
	: BasicImageView("range glue", model)
{
   _dialog = dialog;
   _model->attachView(this);
}

ImageToRangeDialogGlue::~ImageToRangeDialogGlue()
{
   _model->detachView(this);
}

void ImageToRangeDialogGlue::update()
{
   updatePart(IMAGE_DATA_UPDATE_RANGE);		// Set hist limits
}

void ImageToRangeDialogGlue::updatePart(int flags)
{
   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      double min, max;
      _model->getDataRange(min, max);
      _dialog->setDataRange(min, max);
   }
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create datarange.imake
#define SUBROUTINE datarange
#define MODULE_LIST DataRangeDialog.cc DataRangeAutoCmd.cc \
	DataRangeValueCmd.cc ImageToRangeDialogGlue.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_RTL
#define LIB_TAE
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
