$!****************************************************************************
$!
$! Build proc for MIPL module stretch
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:41
$!
$! Execute by entering:		$ @stretch
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
$ write sys$output "*** module stretch ***"
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
$ write sys$output "Invalid argument given to stretch.com file -- ", primary
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
$   if F$SEARCH("stretch.imake") .nes. ""
$   then
$      vimake stretch
$      purge stretch.bld
$   else
$      if F$SEARCH("stretch.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake stretch
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @stretch.bld "STD"
$   else
$      @stretch.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create stretch.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack stretch.com -mixed -
	-s StretchCmd.cc StretchDialog.cc StretchCmdInterface.cc -
	   StretchValue.cc StretchFun.cc StretchBandChooser.cc -
	   StretchListCmd.cc ListControl.cc StretchParmListDialog.cc -
	   StretchListInterface.cc StretchParmList.cc CurListValue.cc -
	   Function.cc StretchCheckBox.cc StretchRadioBtn.cc -
	   StretchParmInpView.cc StretchPercValuesDialog.cc -
	   StretchPercValuesIf.cc LoadLutFileCmd.cc SaveLutFileCmd.cc -
	-i stretch.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create StretchCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchCmd.cc: This command applies LUT(s) to the image
//////////////////////////////////////////////////////////////
#include "StretchCmd.h"
#include "Lut.h"
#include "SiHistogram.h"
#include "StretchValue.h"
#include "StretchFun.h"
#include <iostream>
using namespace std;

StretchCmd::StretchCmd ( const char *name, int active, 
			 Lut *lutR, SiHistogram *histR, 
			 Lut *lutG, SiHistogram *histG, 
			 Lut *lutB, SiHistogram *histB ) 
    : Cmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
    
    _histR = histR;
    _histG = histG;
    _histB = histB;
    
    _undoValue = NULL;

    _redValue = new StretchValue(STR_RED);
    _grnValue = new StretchValue(STR_GREEN);
    _bluValue = new StretchValue(STR_BLUE);
    _allValue = new StretchValue(STR_ALL);

    _currentValue = _allValue;

    _value = (CmdValue) (new StretchValue(*_currentValue));
}

StretchCmd::StretchCmd ( const char *name, int active,
			 Lut *lutR, SiHistogram *histR) 
    : Cmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _histR = histR;
    
    _lutG = _lutB = NULL; 
    _histG = _histB = NULL;
    
    _undoValue = NULL;

    _redValue = new StretchValue(STR_RED);
    _grnValue = new StretchValue(STR_GREEN);
    _bluValue = new StretchValue(STR_BLUE);
    _allValue = new StretchValue(STR_ALL);

    _currentValue = _allValue;

    _value = (CmdValue) (new StretchValue(*_currentValue));
}

StretchCmd::~StretchCmd()
{
    if (_undoValue)
	delete _undoValue;
    if (_currentValue)
	delete _currentValue;
    if (_redValue)
	delete _redValue;
    if (_grnValue)
        delete _grnValue;
    if (_bluValue)
        delete _bluValue;
    if (_allValue)
        delete _allValue;
}

void StretchCmd::doit()
{
    // If a null value is passed in, assume a linear ramp.

    if (_value == NULL) {
	StretchValue *stretchValue = new StretchValue(*_allValue);
	_value = (CmdValue)stretchValue;
	newValue();
    }
    
    // Because there is no model to ask, we save the current CmdValue, and
    // move the old current as the CmdValue to use for Undo.  This requires
    // saving two CmdValue's, and breaks if there is more than one StretchCmd
    // in existence!!!!

    if (!(*((StretchValue *)_value) == *_currentValue)) {
        if (_undoValue != NULL)
            delete _undoValue;
        _undoValue = new StretchValue(*_currentValue);
    }

    _currentValue = new StretchValue(*(StretchValue *)_value);

    if (_currentValue->changeOnlyBand == True) {

	switch (_currentValue->band) {
	case STR_ALL:
	    delete _currentValue;
	    _currentValue = _allValue;
	    break;
	case STR_RED:
	    delete _currentValue;
	    _currentValue = _redValue;
            break;
	case STR_GREEN:
	    delete _currentValue;
            _currentValue = _grnValue;
            break;
	case STR_BLUE:
	    delete _currentValue;
            _currentValue = _bluValue;
            break;
	default:
	    cerr << "StretchCmd::doit(): Memory error\n";
	}
    }
    else {
	switch ( _currentValue->band ) {
	case STR_ALL:
	    delete _allValue;
	    _allValue = _currentValue;
	    break;
        case STR_RED:
	    delete _redValue;
            _redValue = _currentValue;
            break;
        case STR_GREEN:
	    delete _grnValue;
            _grnValue = _currentValue;
            break;
        case STR_BLUE:
	    delete _bluValue;
            _bluValue = _currentValue;
            break;
        default:
            cerr << "StretchCmd::doit(): Memory error\n";
        }
    }

    switch ( _currentValue->band ) {
    case STR_ALL:

	// Stretch all three planes

	stretchOneBand(_lutR, _currentValue, _histR, 
		       _currentValue->lPercValueRed, 
		       _currentValue->hPercValueRed);
	stretchOneBand(_lutG, _currentValue, _histG, 
		       _currentValue->lPercValueGrn,
                       _currentValue->hPercValueGrn);
	stretchOneBand(_lutB, _currentValue, _histB, 
		       _currentValue->lPercValueBlu,
                       _currentValue->hPercValueBlu);
	
	// Do post-stretches
	if ( _currentValue->table ) {
	    stretch_table(_lutR, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	    stretch_table(_lutG, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	    stretch_table(_lutB, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	}
	if ( _currentValue->alarm ) {
	    stretch_alarm(_lutR, _currentValue->alarmValues, 
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	    stretch_alarm(_lutG, _currentValue->alarmValues,
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	    stretch_alarm(_lutB, _currentValue->alarmValues,
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	}				
	if ( _currentValue->comple ) {
	    stretch_comp(_lutR);
	    stretch_comp(_lutG);
	    stretch_comp(_lutB);
	}
	if ( _currentValue->off ) {
	    stretch_off(_lutR);
	    stretch_off(_lutG);
	    stretch_off(_lutB);
	}

	// Adjust red, green, and blue planes to match all.

	*_redValue = *_allValue;
	*_grnValue = *_allValue;
	*_bluValue = *_allValue;
	_redValue->band = STR_RED;
	_grnValue->band = STR_GREEN;
	_bluValue->band = STR_BLUE;
	
	break;
	
    case STR_RED:
	stretchOneBand(_lutR, _currentValue, _histR, 
		       _currentValue->lPercValueRed,
		       _currentValue->hPercValueRed);
	
	if ( _currentValue->table ) 
	    stretch_table(_lutR, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	if ( _currentValue->alarm )
	    stretch_alarm(_lutR, _currentValue->alarmValues,
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	if ( _currentValue->comple )
	    stretch_comp ( _lutR );
	if ( _currentValue->off)
	    stretch_off ( _lutR );
	break;
	
    case STR_GREEN:
	stretchOneBand(_lutG, _currentValue, _histG, 
		       _currentValue->lPercValueGrn,
                       _currentValue->hPercValueGrn);
	
	if ( _currentValue->table )
	    stretch_table(_lutG, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	if ( _currentValue->alarm )
	    stretch_alarm(_lutG, _currentValue->alarmValues,
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	if ( _currentValue->comple )
	    stretch_comp(_lutG);
	if ( _currentValue->off)
            stretch_off(_lutG);
	break;
	
    case STR_BLUE:
	stretchOneBand(_lutB, _currentValue, _histB, 
		       _currentValue->lPercValueBlu,
                       _currentValue->hPercValueBlu);
	
	if ( _currentValue->table )
	    stretch_table(_lutB, _currentValue->inTable,
			  _currentValue->outTable, _currentValue->tableNoVals);
	if ( _currentValue->alarm )
	    stretch_alarm(_lutB, _currentValue->alarmValues,
			  _currentValue->alarmNoVals, _currentValue->dnValue );
	if ( _currentValue->comple )
	    stretch_comp(_lutB );
	if ( _currentValue->off)
            stretch_off(_lutB);
	break;
    }

    freeValue(_value);
    _value = (CmdValue) (new StretchValue(*_currentValue));
}

void StretchCmd::stretchOneBand(Lut *lut, StretchValue *stretchValue, 
	   SiHistogram *hist, double &lPercValue, double &hPercValue)
{
    if (!lut) return;
    
    switch (stretchValue->stretchName ) {
    case ASTRETCH:
	if (!hist) return;
	stretch_percent(lut, hist, 
			stretchValue->lPerc, stretchValue->hPerc, 
			(int)stretchValue->dnmin, (int)stretchValue->dnmax, 
			lPercValue, hPercValue);
	break;
	
    case CLIP:
	stretch_clip(lut, stretchValue->nbits );
	break;
	
    case CONTOUR:
	stretch_contour(lut, (int)stretchValue->dnmin, 
			(int)stretchValue->dnmax, 
			stretchValue->interval, stretchValue->maxnum,
			stretchValue->dnValue, False );
	break;
	
    case ELLIPSE:
	if (!hist) return;
	stretch_ellipse(lut, hist,
			(int)stretchValue->dnmin, (int)stretchValue->dnmax );
	break;
	
    case FUNCTION:
	stretch_function(lut, stretchValue->func );
	break;
	
    case GAUSS:
	if (!hist) return;
	stretch_gauss(lut, hist,
		      stretchValue->gsigma, stretchValue->gmean,
		      (int)stretchValue->dnmin, (int)stretchValue->dnmax );
	break;
	
    case ITABLE:
	stretch_itable(lut, stretchValue->inITable, stretchValue->outITable,
		       stretchValue->itableNoVals, stretchValue->backgnd);
	break;
	
    case LINEAR:
	stretch_linear(lut, stretchValue->low, stretchValue->high );
	break;
	
    case LOGARITHM:
	stretch_log(lut, stretchValue->curve, stretchValue->dnmin, 
		    stretchValue->dnmax );
	break;
	
    case MEAN:
	//!!! Not implemented
	break;
	
    case PEAK:
	//!! Not implemented
	break;
	
    case PSTRETCH:
	stretch_period(lut, stretchValue->pmean, stretchValue->ampl, 
		       stretchValue->freq, stretchValue->phi );
	break;
	
    case POWER:
	if (!hist) return;
	stretch_power(lut, hist,
		      (int)stretchValue->dnmin, (int)stretchValue->dnmax );
	break;

    case RAW:
	stretch_linear(lut, lut->getLowerLimit(), lut->getUpperLimit());
	break;

    case SMOOTH:
	if (!hist) return;
	stretch_smooth(lut, hist,
		       (int)stretchValue->dnmin, (int)stretchValue->dnmax );
	break;
	
    default:
	cerr << "StretchCmd: Unidentified type of stretch was requested!\n";
    }
}      

void StretchCmd::undoit()
{
    if (_undoValue) {
	_value = new StretchValue(*_undoValue);
	doit();
	newValue();
    }
}

void StretchCmd::freeValue(CmdValue value)
{
    if (value)
	delete (StretchValue *)value;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// StretchDialog.cc: This class creates a work area for stretch dialog.
//////////////////////////////////////////////////////////////////////
#include "StretchDialog.h"
#include "StretchCmdInterface.h"
#include "StretchBandChooser.h"
#include "PostSingleFileDialogCmd.h"
#include "LoadLutFileCmd.h"
#include "SaveLutFileCmd.h"
#include "StretchPercValuesDialog.h"
#include "PostDialogCmd.h"
#include "UndoCmd.h"
#include "HelpOnContextCmd.h"
#include "HelpSelfCmd.h"
#include "Lut.h"
#include "Cmd.h"
#include "MenuCmdList.h"
#include "MenuBar.h"
#include <Xm/RowColumn.h>

StretchDialog::StretchDialog(const char *name, Cmd *cmd, 
			     Lut *lutR, Lut *lutG, Lut *lutB)
    : MenuDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    _cmd = cmd;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

Widget StretchDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("StretchDialogRC", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 NULL);
    CmdInterface *ci = new StretchCmdInterface(rc, _cmd);
    CmdInterface *bandChooser = new StretchBandChooser(rc, _cmd);

    ci->manage();
    bandChooser->manage();

    return rc;
}

void StretchDialog::createMenuPanes()
{
   MenuCmdList *cmdList;
 
   ////////
   // CREATE FILE PULLDOWN
   ////////
 
   cmdList = new MenuCmdList("File");
 
   Cmd *loadFileCmd = new LoadLutFileCmd("loadFile", True, _cmd);
   Cmd *loadWinFileCmd = new PostSingleFileDialogCmd("load", True, 
						     loadFileCmd);
   cmdList->add(loadWinFileCmd);

   Cmd *saveFileCmd = new SaveLutFileCmd ("saveFile", True, 
					  _lutR, _lutG, _lutB);
   Cmd *saveWinFileCmd = new PostSingleFileDialogCmd ("save", True, 
						      saveFileCmd);
   cmdList->add(saveWinFileCmd);
 
   _menuBar->addCommands(cmdList);
   delete cmdList;
 
   ////////
   // CREATE OPTIONS PULLDOWN
   ////////

   cmdList = new MenuCmdList("Options");

   cmdList->addButton(theUndoCmd);

   // Show percent stretch limits
   
   CustomDialog *percValuesDialog;
   percValuesDialog = new StretchPercValuesDialog("percValuesDialog", _cmd);
   Cmd *postPercValuesCmd;
   postPercValuesCmd = new PostDialogCmd("postPercValuesDialog",
					 True, percValuesDialog );
   cmdList->add(postPercValuesCmd);

   _menuBar->addCommands(cmdList);
   delete cmdList;

   ////////
   // Create Help menu
   ////////
 
        Cmd *helpOnContextCmd = new HelpOnContextCmd("On Context", True, 
						     _menuBar->baseWidget());
        Cmd *helpOnHelpCmd = new HelpSelfCmd("On Help", True,
                        _menuBar->baseWidget(), "*Help*On Help");
        Cmd *helpOnWindowCmd = new HelpSelfCmd("On Window", True,
                        _menuBar->baseWidget(), "*Help*On Window");
        Cmd *helpOnKeysCmd = new HelpSelfCmd("On Keys", True,
                        _menuBar->baseWidget(), "*Help*On Keys");
        Cmd *helpOnVersionCmd = new HelpSelfCmd("On Version", True,
                        _menuBar->baseWidget(), "*Help*On Version");
 
        cmdList = new MenuCmdList("Help");
 
        cmdList->addButton(helpOnContextCmd);
        cmdList->addButton(helpOnHelpCmd);
        cmdList->addButton(helpOnWindowCmd);
        cmdList->addButton(helpOnKeysCmd);
        cmdList->addButton(helpOnVersionCmd);
 
        _menuBar->addCommands(cmdList, True);
	delete cmdList;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchCmdInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchCmdInterface.cc: Command interface subclass that fills
// StretchValue structure end calls execution on the command with
// filled-in value.
//////////////////////////////////////////////////////////////
#include "StretchCmdInterface.h"
#include "StretchRadioBtn.h"
#include "StretchCheckBox.h"
#include "StretchParmInpView.h"
#include "PostDialogCmd.h"
#include "StretchParmListDialog.h"
#include "RadioCmdBox.h"
#include "ButtonInterface.h"
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Separator.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <iostream>
using namespace std;

XtResource StretchCmdInterface::_resources[] = {
 {
   (char *)"alarmValue", 
   (char *)"AlarmValue",
   XmRInt,
   sizeof ( int ),
   XtOffsetOf ( StretchCmdInterface, _alarmValue ),
   XmRImmediate,
   ( XtPointer ) 255,
 },
 {
   (char *)"unselectedParmColor", 
   (char *)"UnselectedParmColor", 
   XmRString,
   sizeof ( String ),
   XtOffset ( StretchCmdInterface *, _unselectedParmColor ),
   XmRString,
   ( XtPointer ) "black",
 },
 {
   (char *)"selectedParmColor", 
   (char *)"SelectedParmColor", 
   XmRString,
   sizeof ( String ),
   XtOffset ( StretchCmdInterface *, _selectedParmColor ),
   XmRString,
   ( XtPointer ) "blue",
 },
};

//////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////
StretchCmdInterface::StretchCmdInterface ( Widget parent, Cmd *cmd )
    : CmdInterface ( cmd )
{
    int i;

    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler();

    getResources ( _resources, XtNumber ( _resources ) );

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.

    if ( _active )
	activate();
    else
	deactivate();      

    Widget stretchForm = XtVaCreateManagedWidget("stretchForm",
                xmFormWidgetClass, _w,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
                NULL );

    _numRadios = 0;
    _numChecks = 0;
    _numInpViews = 0;
    
    // Get current value

    _stretchValue = new StretchValue (*((StretchValue *)(_cmd->getValue())));

    /////////////////////////////////////////////////////////////////
    // Stretch type names are listed in a radio bank

    Widget stretchNameRC = XtVaCreateManagedWidget("stretchNameRC",
                xmRowColumnWidgetClass, stretchForm,
                XmNorientation, XmVERTICAL,
		XmNradioBehavior, True,
		XmNradioAlwaysOne, True,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
                NULL);

    // If you add to this list make sure you adjust Stretch_MAX_RADIOS!!

    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"clip", CLIP, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"contour", CONTOUR, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"ellipse", ELLIPSE, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"func", FUNCTION, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"gauss", GAUSS, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"itable", ITABLE, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"linear", LINEAR, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"log", LOGARITHM, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"percent", ASTRETCH, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"period", PSTRETCH, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"power", POWER, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
                "raw", RAW, this);
    _radioBtnList[_numRadios++] = new StretchRadioBtn(stretchNameRC,
		"smooth", SMOOTH, this);

    for (i = 0; i < _numRadios; i++ )
	_radioBtnList[i]->manage();


    /////////////////////////////////////////////////////////////////////

    Widget stretchParms1 = XtVaCreateManagedWidget("stretchParms1",
                xmFormWidgetClass, stretchForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, stretchNameRC,
                NULL );

    // If you add to this list make sure you adjust Stretch_MAX_INP_VIEWS!!
    // The indexes in the table are significant.  If you change anything which
    // changes the table indexing, you must also change the activation lists
    // in setValue().
    
    // Keyin single-value parameters, first column

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 0
		"low", _stretchValue,
		XtOffsetOf(StretchValue, low), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 1
		"high", _stretchValue,
		XtOffsetOf(StretchValue, high), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
		XmNtopWidget,	    _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 2
		"dnmin", _stretchValue,
		XtOffsetOf(StretchValue, dnmin), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 3
		"dnmax", _stretchValue,
		XtOffsetOf(StretchValue, dnmax), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 4
		"dnValue", _stretchValue,
		XtOffsetOf(StretchValue, dnValue), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 5
		"gmean", _stretchValue,
		XtOffsetOf(StretchValue, gmean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 6
		"gsigma", _stretchValue,
		XtOffsetOf(StretchValue, gsigma), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 7
		"nbits", _stretchValue,
		XtOffsetOf(StretchValue, nbits), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 8
                "curve", _stretchValue,
                XtOffsetOf(StretchValue, curve), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms1,// 9
                "interval", _stretchValue,
                XtOffsetOf(StretchValue, interval), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    // Second column of parameters

    Widget stretchParms2 = XtVaCreateManagedWidget("stretchParms2",
                xmFormWidgetClass, stretchForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, stretchParms1,
		XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 10
                "lPerc", _stretchValue,
                XtOffsetOf(StretchValue, lPerc), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 11
                "hPerc", _stretchValue,
                XtOffsetOf(StretchValue, hPerc), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 12
		"maxnum", _stretchValue,
		XtOffsetOf(StretchValue, maxnum), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 13
		"backgnd", _stretchValue,
		XtOffsetOf(StretchValue, backgnd), StretchInt, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 14
		"mean", _stretchValue,
		XtOffsetOf(StretchValue, mean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 15
		"pmean", _stretchValue,
		XtOffsetOf(StretchValue, pmean), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 16
		"ampl", _stretchValue,
		XtOffsetOf(StretchValue, ampl), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 17
		"freq", _stretchValue,
		XtOffsetOf(StretchValue, freq), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchParms2, // 18
		"phi", _stretchValue,
		XtOffsetOf(StretchValue, phi), StretchDouble, this);
    XtVaSetValues ( _inpViewList[_numInpViews-1]->baseWidget(),
                XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-2]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );

    ///////////////////////////////////////////////////////////////////////
    // ITable Parameter 

    _itableListDialog = new StretchParmListDialog("itableListDialog", 
						  _cmd, ITABLE);
    Cmd *postITableListCmd;
    postITableListCmd = new PostDialogCmd("PostITableListDialog", 
					  True, _itableListDialog );
    _itableValueList = new ButtonInterface(stretchParms2, postITableListCmd);
    XtVaSetValues ( _itableValueList->baseWidget(),
		XmNtopAttachment,   XmATTACH_WIDGET,
                XmNtopWidget,       _inpViewList[_numInpViews-1]->baseWidget(),
                XmNrightAttachment, XmATTACH_FORM,
                NULL );
    _itableValueList->manage();

    /////////////////////////////////////////////////////////////
    // Function parameter

    _inpViewList[_numInpViews++] = new StretchParmInpView(stretchForm, // 19
                "func", _stretchValue,
                XtOffsetOf(StretchValue, func), StretchString, this);
    XtVaSetValues(_inpViewList[_numInpViews-1]->baseWidget(),
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, stretchParms2,
		XmNleftAttachment, XmATTACH_NONE,
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		NULL);

    /////////////////////////////////////////////////////////////
    // Manage all keyin parameters

    for (i = 0; i < _numInpViews; i++ )
	_inpViewList[i]->manage();

    /////////////////////////////////////////////////////////////
    // Post Stretches

    Widget postFrame =  XtVaCreateManagedWidget("postFrame",
		xmFrameWidgetClass, _w,
		XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, stretchForm,
                XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL );
    XtVaCreateManagedWidget ("postLabel",
		xmLabelGadgetClass, postFrame,
		XmNchildType, XmFRAME_TITLE_CHILD,
		XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
		NULL );

    Widget postForm =  XtVaCreateManagedWidget("postForm",
                xmFormWidgetClass, postFrame,
                NULL );

    Widget rcPostList = XtVaCreateManagedWidget("rcPostList",
                xmRowColumnWidgetClass, postForm,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
                NULL);

    // Post stretches are implemented as checkboxes

    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "table", 
						      TABLE, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "alarm", 
						      ALARM, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "comp", 
						      COMP, this);
    _checkBoxList[_numChecks++] = new StretchCheckBox(rcPostList, "off",
                                                      OFF, this);

    // Manage all the checkboxes

    for (i = 0; i < _numChecks; i++ )
	_checkBoxList[i]->manage();


    // Table Stretch Values

    _tableListDialog = new StretchParmListDialog ("tableListDialog", 
						  _cmd, TABLE );
    Cmd *postTableListCmd = new PostDialogCmd("PostTableListDialog", 
					      True, _tableListDialog);
    CmdInterface *tableValueList = new ButtonInterface(postForm, 
						       postTableListCmd);
    XtVaSetValues ( tableValueList->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, rcPostList,
		NULL );
    tableValueList->manage();

    // Alarm Stretch Values

    _alarmListDialog = new StretchParmListDialog ("alarmListDialog", 
						  _cmd, ALARM, _alarmValue );
    Cmd *postAlarmListCmd;
    postAlarmListCmd = new PostDialogCmd("PostAlarmListDialog", True, 
					 _alarmListDialog);
    CmdInterface *alarmValueList;
    alarmValueList = new ButtonInterface(postForm, postAlarmListCmd);
    XtVaSetValues ( alarmValueList->baseWidget(),
                XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, tableValueList->baseWidget(),
                XmNleftAttachment, XmATTACH_WIDGET,
                XmNleftWidget, rcPostList,
                NULL );
    alarmValueList->manage();

    setValue(_cmd->getValue());
}

//////////////////////////////////////////////////////////////
// Actually run the command by creating a dynamically allocated
// copy of the stretchValue (because that's what Cmd likes).
//////////////////////////////////////////////////////////////
void StretchCmdInterface::stretchIt(StretchValue *stretchValue)
{
    _stretchValue = stretchValue;
    runCmd((CmdValue *) new StretchValue(*_stretchValue));
}

void StretchCmdInterface::stretchIt()
{
    runCmd((CmdValue *) new StretchValue(*_stretchValue));
}

//////////////////////////////////////////////////////////////
// Update the interface to match a given value
//////////////////////////////////////////////////////////////
void StretchCmdInterface::setValue(CmdValue value)
{
    int i;

    CmdInterface::setValue(value);	// Removes cmd from deferred list

    if (value != NULL)		// NULL means use saved default
	*_stretchValue = *((StretchValue *)value);
    else 
	cerr << "StretchCmdInterface::setValue: No Value error\n";

    for (i = 0; i < _numInpViews; i++)
	_inpViewList[i]->setValue(_stretchValue);

    //_tableListDialog->setValue(_stretchValue);
    //_alarmListDialog->setValue(_stretchValue);

    for (i = 0; i < _numChecks; i++) {
	if (_checkBoxList[i]->getType() == TABLE)
	    _checkBoxList[i]->setValue(_stretchValue->table);
	else if (_checkBoxList[i]->getType() == ALARM)
	    _checkBoxList[i]->setValue(_stretchValue->alarm);
	else if (_checkBoxList[i]->getType() == COMP)
	    _checkBoxList[i]->setValue(_stretchValue->comple);
	else if (_checkBoxList[i]->getType() == OFF)
            _checkBoxList[i]->setValue(_stretchValue->off);
    }

    for (i = 0; i < _numRadios; i++)
	_radioBtnList[i]->setValue(_stretchValue->stretchName);

    setSensitivity(_stretchValue->stretchName);
}

//////////////////////////////////////////////////////////////
// We set sensitivity for relevant parameters by first setting them all
// to false, then setting the relevant ones to true, based on
// stretch type.
//////////////////////////////////////////////////////////////
void StretchCmdInterface::setSensitivity(StretchType type)
{
    Boolean sens_flag[Stretch_MAX_INP_VIEWS];
    
    // Check for post-stretches
    
    if ( (type == COMP) || (type == ALARM) || (type == TABLE) )
	return;
    
    int i;
    for (i = 0; i < _numInpViews; i++)
	sens_flag[i] = False;
    
    XtSetSensitive(_itableValueList->baseWidget(), False);
    XtVaSetValues(_itableValueList->baseWidget(),
		  XtVaTypedArg,
		      XmNforeground, XmRString, 
		      _unselectedParmColor, (strlen(_unselectedParmColor) + 1),
		  NULL);

    switch (type) {
	
    case LINEAR:
	sens_flag[0] = True;					// low
	sens_flag[1] = True;					// high
	break;
	
    case RAW:
	break;

    case OFF:
	break;

    case CLIP:
	sens_flag[7] = True;					// nbits
	break;
	
    case CONTOUR:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[4] = True;					// dnValue
	sens_flag[9] = True;					// interval
	sens_flag[12] = True;					// maxnum
	//!!!! stretched??
	break;
	
    case ITABLE:
	sens_flag[13] = True;					// backgnd
	XtSetSensitive(_itableValueList->baseWidget(), True);  // inTable, outTable
	XtVaSetValues(_itableValueList->baseWidget(),
		      XtVaTypedArg,
                          XmNforeground, XmRString, 
		          _selectedParmColor, (strlen(_selectedParmColor) + 1),
		      NULL);

	break;
	
    case PSTRETCH:
	sens_flag[15] = True;					// pmean
	sens_flag[16] = True;					// ampl
	sens_flag[17] = True;					// freq
	sens_flag[18] = True;					// phi
	break;
	
    case FUNCTION:
	sens_flag[19] = True;					// func
	break;
	
    case LOGARITHM:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[8] = True;					// curve
	break;
	
    case ASTRETCH:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[10] = True;					// lPerc
	sens_flag[11] = True;					// hPerc
	break;
	
    case GAUSS:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	sens_flag[5] = True;					// gmean
	sens_flag[6] = True;					// gsigma
	break;
	
    case SMOOTH:
    case ELLIPSE:
    case POWER:
	sens_flag[2] = True;					// dnmin
	sens_flag[3] = True;					// dnmax
	break;
    default:
	break;			// shouldn't happen, but fairly benign
    }

    for (i = 0; i < _numInpViews; i++) {
	XtSetSensitive(_inpViewList[i]->baseWidget(), sens_flag[i]);
	sens_flag[i] ? _inpViewList[i]->setForeground(_selectedParmColor) : 
	               _inpViewList[i]->setForeground(_unselectedParmColor);
    }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchValue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// StretchValue.h: Contains type of stretch and all parameters needed
// by the function to execute the stretch.  The object of this class
// is passed to the command as a value.
///////////////////////////////////////////////////////////////////
#include "StretchValue.h"
#include "Lut.h"
#include "SiHistogram.h"
#include <iostream>
using namespace std;
#include <stdio.h>

///////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////
StretchValue::StretchValue(StretchBand b)
{
    band = b;
    changeOnlyBand = False;

    stretchName = LINEAR;
    low = 0.0;
    high = 255.0;
    dnmin = 0.0;
    dnmax = 255.0;
    alarmValues = inTable = outTable = inITable = outITable = NULL;
    tableNoVals = itableNoVals = alarmNoVals = 0;
    dnValue = (int)dnmax;
    nbits = 0;
    curve = 1.0;
    interval = 16;
    lPerc = 2.0;		// percent stretch
    hPerc = 2.0;		// percent stretch
    maxnum = 255;
    stretched = False;
    backgnd = FALSE;
    func = NULL;
    mean = dnmax - dnmin;
    pmean = (dnmax+dnmin)/2; // mean for periodic stretch
    ampl = 255.0;
    freq = 1.0;
    phi = 0.0;
    factor = 12.0;
    percent = factor;
    range = (int)percent;
    lpercent = percent / 2;
    hpercent = percent / 2;
    gsigma = 3.0;
    gmean = (dnmax+dnmin)/2;
    
    table = alarm = comple = off = False;

    lPercValueRed = lPercValueGrn = lPercValueBlu = 0.0;
    hPercValueRed = hPercValueGrn = hPercValueBlu = 255.0;
}

///////////////////////////////////////////////////////////////////
// Destructor
///////////////////////////////////////////////////////////////////
StretchValue::~StretchValue()
{
    if (func) delete []func;
    if (alarmValues) delete []alarmValues;
    if (inTable) delete []inTable;
    if (outTable) delete []outTable;
    if (inITable) delete []inITable;
    if (outITable) delete []outITable;
}

///////////////////////////////////////////////////////////////////
// Copy constructor
///////////////////////////////////////////////////////////////////
StretchValue::StretchValue(StretchValue &val)
{
    //memcpy((void *)this, (void *)&val, sizeof(StretchValue));
    stretchName = val.stretchName;
    band = val.band;
    changeOnlyBand = val.changeOnlyBand;
    low = val.low;
    high = val.high;
    dnmin = val.dnmin;
    dnmax = val.dnmax;
    dnValue = val.dnValue;
    nbits = val.nbits;
    curve = val.curve;
    interval = val.interval;
    maxnum = val.maxnum;
    lPerc = val.lPerc;
    hPerc = val.hPerc;
    stretched = val.stretched;
    tableNoVals = val.tableNoVals;
    itableNoVals = val.itableNoVals;
    alarmNoVals = val.alarmNoVals;
    backgnd = val.backgnd;
    mean = val.mean;
    pmean = val.pmean;
    ampl = val.ampl;
    freq = val.freq;
    phi = val.phi;
    range = val.range;
    factor = val.factor;
    percent = val.percent;
    lpercent = val.lpercent;
    hpercent = val.hpercent;
    gsigma = val.gsigma;
    gmean = val.gmean;
    table = val.table;
    alarm = val.alarm;
    comple = val.comple;
    off = val.off;

    func = NULL;
    alarmValues = inTable = outTable = inITable = outITable = NULL;

    if (val.func) {
	func = new char[strlen(val.func)+1];
	strcpy(func, val.func);
    }
    int i;
    if (val.alarmValues) {
	alarmValues = new int[alarmNoVals];
	for (i = 0; i < alarmNoVals; i++)
	    alarmValues[i] = val.alarmValues[i];
    }
    if (val.inTable) {
	inTable = new int[tableNoVals];
	for (i = 0; i < tableNoVals; i++)
	    inTable[i] = val.inTable[i];
    }
    if (val.outTable) {
	outTable = new int[tableNoVals];
	for (i = 0; i < tableNoVals; i++)
	    outTable[i] = val.outTable[i];
    }
    if (val.inITable) {
	inITable = new int[itableNoVals];
	for (i = 0; i < itableNoVals; i++)
	    inITable[i] = val.inITable[i];
    }
    if (val.outITable) {
	outITable = new int[itableNoVals];
	for (i = 0; i < itableNoVals; i++)
	    outITable[i] = val.outITable[i];
    }

    lPercValueRed = val.lPercValueRed;
    hPercValueRed = val.hPercValueRed;
    lPercValueGrn = val.lPercValueGrn;
    hPercValueGrn = val.hPercValueGrn;
    lPercValueBlu = val.lPercValueBlu;
    hPercValueBlu = val.hPercValueBlu;
}

///////////////////////////////////////////////////////////////////
// Assignment operator
///////////////////////////////////////////////////////////////////
StretchValue &StretchValue::operator=(StretchValue &val)
{
    if (this == &val)
	return *this;		// assignment to self
    
    if (func)
	delete []func;
    if (inTable)
	delete []inTable;
    if (outTable)
	delete []outTable;
    if (inITable)
	delete []inITable;
    if (outITable)
	delete []outITable;
    if (alarmValues)
	delete []alarmValues;
    
    func = NULL;
    alarmValues = inTable = outTable = inITable = outITable = NULL;

    // memcpy((void *)this, (void *)&val, sizeof(StretchValue));
    stretchName = val.stretchName;
    band = val.band;
    changeOnlyBand = val.changeOnlyBand;
    low = val.low;
    high = val.high;
    dnmin = val.dnmin;
    dnmax = val.dnmax;
    dnValue = val.dnValue;
    nbits = val.nbits;
    curve = val.curve;
    interval = val.interval;
    maxnum = val.maxnum;
    lPerc = val.lPerc;
    hPerc = val.hPerc;
    stretched = val.stretched;
    tableNoVals = val.tableNoVals;
    itableNoVals = val.itableNoVals;
    alarmNoVals = val.alarmNoVals;
    backgnd = val.backgnd;
    mean = val.mean;
    pmean = val.pmean;
    ampl = val.ampl;
    freq = val.freq;
    phi = val.phi;
    range = val.range;
    factor = val.factor;
    percent = val.percent;
    lpercent = val.lpercent;
    hpercent = val.hpercent;
    gsigma = val.gsigma;
    gmean = val.gmean;
    table = val.table;
    alarm = val.alarm;
    comple = val.comple;
    off = val.off;

    if (val.func) {
	func = new char[strlen(val.func)+1];
	strcpy(func, val.func);
    }
    int i;
    if (val.alarmValues) {
	alarmValues = new int[alarmNoVals];
	for (i = 0; i < alarmNoVals; i++)
	    alarmValues[i] = val.alarmValues[i];
    }
    if (val.inTable) {
	inTable = new int[tableNoVals];
	for (i = 0; i < tableNoVals; i++)
	    inTable[i] = val.inTable[i];
    }
    if (val.outTable) {
	outTable = new int[tableNoVals];
	for (i = 0; i < tableNoVals; i++)
	    outTable[i] = val.outTable[i];
    }
    if (val.inITable) {
	inITable = new int[itableNoVals];
	for (i = 0; i < itableNoVals; i++)
	    inITable[i] = val.inITable[i];
    }
    if (val.outITable) {
        outITable = new int[itableNoVals];
        for (i = 0; i < itableNoVals; i++)
            outITable[i] = val.outITable[i];
    }

    lPercValueRed = val.lPercValueRed;
    hPercValueRed = val.hPercValueRed;
    lPercValueGrn = val.lPercValueGrn;
    hPercValueGrn = val.hPercValueGrn;
    lPercValueBlu = val.lPercValueBlu;
    hPercValueBlu = val.hPercValueBlu;
    
    return *this;
}

///////////////////////////////////////////////////////////////////
// Equality operator
///////////////////////////////////////////////////////////////////
Boolean StretchValue::operator==(StretchValue &val)
{
    if (this == &val)
        return True;
 
    if ((stretchName != val.stretchName) ||
	(band != val.band) ||
	(changeOnlyBand != val.changeOnlyBand) ||
	(low != val.low) ||
	(high != val.high) ||
	(dnmin != val.dnmin) ||
	(dnmax != val.dnmax) ||
	(dnValue != val.dnValue) ||
	(nbits != val.nbits) ||
	(curve != val.curve) ||
	(interval != val.interval) ||
	(maxnum != val.maxnum) ||
	(lPerc != val.lPerc) ||
	(hPerc != val.hPerc) ||
	(stretched != val.stretched) ||
	(tableNoVals != val.tableNoVals) ||
	(itableNoVals != val.itableNoVals) ||
	(alarmNoVals != val.alarmNoVals) ||
	(backgnd != val.backgnd) ||
	(mean != val.mean) ||
	(pmean != val.pmean) ||
	(ampl != val.ampl) ||
	(freq != val.freq) ||
	(phi != val.phi) ||
	(range != val.range) ||
	(factor != val.factor) ||
	(percent != val.percent) ||
	(lpercent != val.lpercent) ||
	(hpercent != val.hpercent) ||
	(gsigma != val.gsigma) ||
	(gmean != val.gmean) ||
	(table != val.table) ||
	(alarm != val.alarm) ||
	(comple != val.comple) ||
	(off != val.off) ||
	(alarmNoVals != val.alarmNoVals) ||
	(tableNoVals != val.tableNoVals) ||
	(itableNoVals != val.itableNoVals))	
	return False;
    else {
	int i;
	for (i = 0; i < alarmNoVals; i++)
	    if (alarmValues[i] != val.alarmValues[i])
		return False;
	for (i = 0; i < tableNoVals; i++)
	    if (inTable[i] != val.inTable[i])
		return False;
	for (i = 0; i < tableNoVals; i++)
	    if (outTable[i] != val.outTable[i])
		return False;
	for (i = 0; i < itableNoVals; i++)
	    if (inITable[i] != val.inITable[i])
		return False;
	for (i = 0; i < itableNoVals; i++) 
	    if (outITable[i] != val.outITable[i])
		return False;
	if (func && val.func) {
	    if (strlen(func) != strlen(val.func))
		return False;
	    else if ((func && !val.func) || (!func && val.func))
		return False;
	    else if (!strcmp(func, val.func))
		return False;
	}
	return True;
    }

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchFun.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////////////
// StretchFun.cc: This is a collection of functions that do all types
// of stretches supported by the stretch tool.  Basically this functions
// take LUT and perform some mathematical operations on it.  
// These functions change point by point intensity of an image
// by generating a transfer function on the domain of intensity values.
///////////////////////////////////////////////////////////////////
#include "StretchFun.h"
#include "SiHistogram.h"
#include "Lut.h"
#include "Function.h"
#include <math.h>
#include <stdlib.h>
#include <iostream>
using namespace std;

#ifndef MIN
#define MIN(x,y)        (((x) < (y)) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y)        (((x) > (y)) ? (x) : (y))
#endif

///////////////////////////////////////////////////////////////////
// Linear stretch function does a linear interpolation between 
// the min and max values
///////////////////////////////////////////////////////////////////
void stretch_linear ( Lut *lut, double dnmin, double dnmax )
{
    int i;

    int *l = lut->getAsArray();
    if (!l) { 
	cerr << "Memory allocation error in stretch_linear\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit());
    double range = dnmax - dnmin;
    if (range == 0.0)		// avoid divide by 0
	range = 0.00001;		// (arbitrary number)
    for ( i = 0; i <= size; i++ ) {
	l[i] = int (MAX(lut->getLowerLimit(),
			MIN(lut->getUpperLimit(),
			    lut->getLowerLimit() +
			    (double)(i - dnmin) * (double)size / range)));
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_alarm sets a series of values in the lookup table
// to a user specified value.
///////////////////////////////////////////////////////////////////
void stretch_alarm ( Lut *lut, int *alarmValues, int nVals, int dnValue )
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_alarm\n"; 
	return; 
    }

    for ( int j = 0; j < nVals; j++ )
	l[alarmValues[j]] = dnValue;
    
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_comp inverts the named lookup tables.
///////////////////////////////////////////////////////////////////
void stretch_comp ( Lut *lut )
{
        int *l = lut->getAsArray();
	if (l == NULL) { 
	    cerr << "Memory allocation error in stretch_comp\n"; 
	    return; 
	}

	int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
        for ( int j = 0; j < size; j++ )
                l[j] = lut->getUpperLimit() - l[j];

	lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_off turns off one plane by setting every member of 
// lut to minimum value.
///////////////////////////////////////////////////////////////////
void stretch_off ( Lut *lut )
{
    stretch_contour(lut, (int)lut->getLowerLimit(),
		    (int)lut->getUpperLimit(),
		    1, (int)lut->getUpperLimit(),
		    (int)lut->getLowerLimit(), False );
}

///////////////////////////////////////////////////////////////////
// stretch_clip performes bit-clipping on LUT. If n > 0, the binary 
// representation of the intensity level is shifted n bits to the left 
// and the n most significant bits are truncated.  If n < 0, the binary 
// representation of the intensity level is shifted n bits to the right 
// and the n least significant bits are truncated.
///////////////////////////////////////////////////////////////////
void stretch_clip ( Lut *lut, int nbits )
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_clip\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);

    int j;
    if (nbits > 0)
	for (j = 0; j < size; j++)
	    l[j] = (j << nbits) % size;
    else
	for (j = 0; j < 256; j++)
	    l[j] = (j >> abs(nbits)) % size;
    
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_log performs logarithmic transformation on LUT
///////////////////////////////////////////////////////////////////
void stretch_log ( Lut *lut, double curve, double dnmin, double dnmax )
{
    if (curve <= 0) {
	cerr << "stretch_log: CURVE must be greater than zero, ";
	cerr << "setting to 1e-30 \n";
	curve = 1e-30;
    }

    if (dnmin == dnmax) {
	cerr << "stretch_log: HIGH and LOW parameters ";
	cerr << "may not be equal.\n";
	return;
    }
    if ((dnmin + curve) <= 0.0) {
	cerr << "stretch_log: LOW + CURVE must be greater than zero.\n";
	return;
    }
    if ((dnmax + curve) <= 0.0) {
	cerr << "stretch_log: HIGH + CURVE must be greater than zero.\n";
	return;
    }

    double a;
    a = lut->getUpperLimit() / (log(dnmax + curve) - log(dnmin + curve));
    
    double b = -a * log(dnmin + curve) + 0.5;

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_log: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);

    for ( int j = 0; j < size; j++ ) {
	l[j] = (int) (a * log( (double)j + curve ) + b);
	if (l[j] < lut->getLowerLimit()) 
	    l[j] = lut->getLowerLimit();
	if (l[j] >lut->getUpperLimit()) 
	    l[j] = lut->getUpperLimit();
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_contour provides DN contours on the given intervals.  Input 
// intensities which are a multiple of n are set to the value specified 
// by the dnValue parameter.
///////////////////////////////////////////////////////////////////
void stretch_contour ( Lut *lut, int start, int end, 
		       int interval, int maxnum, 
		       int dnValue, Boolean stretched )
{
    int j, k;
    stretch_linear (lut, lut->getLowerLimit(), lut->getUpperLimit());

    int *l = lut->getAsArray();
    if (l == NULL) { cerr << "Fail\n"; return; }

    int current = stretched ? l[0] : 0;
    if (interval == 0) return;
    if (((current - start) % interval) == 0) {
	l[0] = dnValue;
	k = 1;
    }
    int last = (current - start) / interval;
    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (j = 1; j < size; j++) {
	if (k > maxnum) break;
	current = stretched ? l[j] : j;
	if (current < start) continue;
	if (current > end) continue;
	current = (current - start) / interval;
	if (current != last) {
	    l[j] = dnValue;
	    k++;
	}
	last = current;
    }
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_table: will set the input DN values in1,in2,... to the DN values
// out1,out2,... and will linearly interpolate between these points to 
// compute the intervening DN values.  All DN values outside the range in1 
// to inN will remain unchanged unless the parameter BACKGND is also 
// specified.  In that case, all DN values outside the range in1 to inN will 
// be set to the BACKGND value. The maximum number of pairs which may be 
// specified is 100.  The values in1,in2,...,inN must be specified in 
// increasing order.  
///////////////////////////////////////////////////////////////////
void stretch_table ( Lut *lut, int *inTable, int *outTable,
		     int nVals)
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_table\n"; 
	return; 
    }

    if (nVals > 0)
	l[inTable[0]] = outTable[0];

    double m, b;            // slope, offset
    int j;
    for (j = 1; j < nVals; j++) {
	m = (double) (outTable[j] - outTable[j - 1]) /
	    (double) (inTable[j] - inTable[j - 1]);
	b = (double)outTable[j] - (m * (double)inTable[j]) + 0.5;
	int k;
	for (k = inTable[j - 1]; k <= inTable[j]; k++)
	    l[k] = (int) (m * (double)k + b);
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_itable: will set the input DN values in1,in2,... to the DN values
// out1,out2,... but will not do any interpolation.  All DN values not 
// explicitly specified, including those outside the range in1 to inN will 
// remain unchanged unless the BACKGND parameter is also specified.  In that 
// case, all DN values not explicitly specified, including those outside the
// range in1 to inN, will be set to the BACKGND value.  The maximum number of 
// pairs which may be specified is 100.
///////////////////////////////////////////////////////////////////
void stretch_itable ( Lut *lut, int *inTable, int *outTable,
		      int nVals, int background )	// background = False
{
    stretch_linear (lut, lut->getLowerLimit(), lut->getUpperLimit());

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_itable: Memory allocation error\n"; 
	return; 
    }

    int i, j;
    for (j = 0; j < nVals; j++) {
	l[inTable[j]] = outTable[j];
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    Boolean contain;
    if (background) {
	for (i = 0; i < size; i++) {
	    contain = False;
	    for (j = 0; j < nVals; j++)
		if ( i == inTable[j] )
		    contain = True;
	    if (!contain)
		l[i] = background;
	}
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_func applies a user-specified mathematical function to
// the lookup tables.
///////////////////////////////////////////////////////////////////
void stretch_function ( Lut *lut, char* v)
{
    FunctionDef theFunc;

    int status = ParseFunction(v, &theFunc);
    if (status == 0) return; 

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_function: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (int j = 0; j < size; j++) {
	l[j] = (int) (ApplyFunction(&theFunc, (float)j) + 0.5);
	if (l[j] < int(lut->getLowerLimit())) 
	    l[j] = int(lut->getLowerLimit());
	if (l[j] > int(lut->getUpperLimit())) 
	    l[j] = int(lut->getUpperLimit());
    }

    lut->setAsArray(l);
}


///////////////////////////////////////////////////////////////////
// stretch_period specifies that a periodic stretch is to be performed.  
// The transfer function is: 
//   DN(out)=AMPL/2*SIN[2*PI*FREQ*DN(in)/(DNMAX-DNMIN)+PHI]+DC
//   where PI = 3.14159
//   and AMPL,FREQ,PHI,DC are parameters which may be specified separately
//   by the user and which have the following defaults: AMPL= DNMAX-DNMIN
//   FREQ= 1.0 
//   PHI = 0.0
//   DC  = (DNMAX+DNMIN)/2
//   Note that AMPL is 2 times what would be called the amplitude in academia.
///////////////////////////////////////////////////////////////////
#define PI 3.1415927
void stretch_period ( Lut *lut, double mean, double ampl, 
				double freq, double phi )
{
    freq = 2.0 * freq * PI / lut->getUpperLimit();
    ampl = ampl / 2.0;
    mean = mean + 0.5;
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_period: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (int j = 0; j < size; j++) {
	l[j] = (int) (ampl * sin(freq * j + phi) + mean);
	if (l[j] < int(lut->getLowerLimit())) 
	    l[j] = int(lut->getLowerLimit());
	if (l[j] > int(lut->getUpperLimit())) 
	    l[j] = int(lut->getUpperLimit());
    }

    lut->setAsArray(l);
}

//////////////////////////////////////////////////////////////////////////
//	HISTOGRAM STRETCHES
//////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////
// stretch_percent performs an automatic linear stretch.  It specifies the 
// total percentage of the input image to be saturated.  The result is 
// equivalent to specifying the parameters LPERCENT and HPERCENT where
// LPERCENT = PERCENT/2 and HPERCENT = PERCENT/2.  This percentage applies 
// to the input image DN distribution after exclusions have been performed.
// PERCENT must be in the range 0.0 to 100.0 (Default is PERCENT=12.0)
///////////////////////////////////////////////////////////////////
void stretch_percent ( Lut *lut, SiHistogram *hist, 
		       double lPerc, double hPerc,
		       int lExclude, int hExclude, 
		       double &low, double &high )
{
    int j, k;

    if (hist == NULL) { 
	cerr << "stretch_percent: Histogram cannot be NULL!\n"; 
	return; 
    }

    if (lExclude < 0)
	lExclude = 0;
    if (hExclude > hist->numBins())
	hExclude = hist->numBins();
    if (lExclude > hExclude)
	hExclude = lExclude;

    lPerc /= 100.0;
    hPerc /= 100.0;
    
    SiHistogram     *locHist;
    locHist = new SiHistogram();
    *locHist = *hist;

    double total = 0.0;
    for (j = lExclude; j <= hExclude; j++)
	total += (double) (*locHist)[j];
    
    low = lExclude;
    high = hExclude;

    if (total != 0.0) {
	double lTotal, hTotal;
	lTotal = hTotal = 0.0;
	
	for (j = lExclude, k = hExclude; j <= hExclude; j++, k--) {
	    lTotal += (double) (*locHist)[j];
	    hTotal += (double) (*locHist)[k];
	    if ((lTotal / total) <= lPerc) low  = (double)j;
	    if ((hTotal / total) <= hPerc) high = (double)k;
	}
    }
    if (high == low) {
	cerr << "stretch_percent: Invalid stretch was calculated, ";
	cerr << "none applied.\n";
    }
    else
      stretch_linear (lut, low, high );

    delete locHist;
}

///////////////////////////////////////////////////////////////////
// HistToCurve will create a lookup table which gives a good match
// of the histogram given by hist to the curve described by the array
// curve.
///////////////////////////////////////////////////////////////////
static void HistToCurve(Lut *lut, SiHistogram *hist, 
			double *curve, 
			int low, int high)
{
    const int HSIZE = hist->numBins();
    const int LUTSIZE = lut->getUpperLimit()-lut->getLowerLimit()+1;

    int           i,cdfPos;                /* increment variables          */
    double        cumSum,cumSumNext,scale; /* Cumulative sum, scaling factor*/
    SiHistogram	  *locHist;		   /* local copy of histogram      */
    double *cdf = new double [HSIZE];      /* cumulative distribution func.*/
    int		  lutPos, lastLutPos;

    if (hist==NULL) { 
	cerr << "HistToCurve: Histogram cannot be NULL!\n"; 
	delete []cdf;
	return; 
    }
    locHist = new SiHistogram();
    *locHist = *hist;
    if (locHist == NULL) cerr << "Fail\n";
    for (i=0; i< HSIZE; i++)
	cdf[i] = curve[i];

    for (i = 0; i < low; i++) 		/* Zero out values  */
	locHist->setBin (i,0);		/* to be excluded   */

    for (i = high+1; i < HSIZE; i++)	/* Zero out values  */
	locHist->setBin(i,0);		/* to be excluded   */

    int *l = lut->getAsArray();
    if (l == NULL) 
	cerr << "HistToCurve: Memory allocation error\n";

/* Now make the cdf table contain an actual cumulative distribution
 * function (cdf); ie, each value is the sum of the values below it.
 * scale is modified to hold a scaling factor to adjust the gaussian
 * to the actual number of pixels we have, and each value in cdf
 * is scaled and then added to the point below it, to give
 * an actual cumulative distribution function.
 */
    cumSum = 0.0;                 /* Total area under the curve to be fit */
    for (i = 0; i < HSIZE; i++)
	cumSum += cdf[i];
    scale = 0;
    for (i = low; i <= high; i++)         /* scale is the total number of */
	scale += (double) (*locHist)[i];    /* pixels in our histogram      */
    scale = scale / cumSum;               /* divided by the total in gauss*/
    cdf[0] *= scale;
    for (i = 1; i < HSIZE; i++)
	cdf[i] = cdf[i] * scale + cdf[i - 1];
    
/* Now generate the lookup table based on the shape of the
 * cumulative distribution function.  Each output DN is that DN (location)
 * in the cdf[] array that has the accumulation (value) that is
 * closest to the accumulation (sum of values) of the input DN in
 * our histogram.
 * If the LUT and histogram bins aren't the same size, compensate by
 * overwriting the LUT value multiple times (if the LUT is smaller), or
 * by taking the current value and filling in the gap.
 */
    cumSum=0.0;
    i = cdfPos = 0;
    cumSumNext = (*locHist)[0];
    lutPos = lastLutPos = 0;
    while ((i < HSIZE) && (cdfPos < HSIZE)) {
	while (fabs(cdf[cdfPos] - cumSum) >= fabs(cdf[cdfPos] - cumSumNext)) {

            // cumulative sum not yet big enough, so store the cdf position
	    // and try the next spot in the histogram.

	    lutPos = (int)((((double)i) / HSIZE) * LUTSIZE + lut->getLowerLimit());
	    l[lutPos] = (int)((((double)cdfPos) / HSIZE) * LUTSIZE +
			      lut->getLowerLimit());
	    i++;
	    if (lutPos - lastLutPos > 1) {	/* Fill in the gap, if any */
		for (int j=lastLutPos+1; j<lutPos; j++) l[j] = l[lutPos];
	    }
	    lastLutPos = lutPos;
	    if (i >= HSIZE) break;
	    cumSum = cumSumNext;
	    cumSumNext = cumSum + (*locHist)[i];
	}
	cdfPos++;                   /* match next spot in cdf       */
    }
    for (lutPos++; lutPos <= lut->getUpperLimit(); lutPos++)
	l[lutPos] = lut->getUpperLimit();   /* saturate out rest of lut     */

    lut->setAsArray(l);

    delete cdf;
    delete locHist;
}


/************************************************************************/
/* GaussLut will force the given histogram into roughly a gaussian
 * curve with the given mean and sigma by tweaking the given lookup
 * table.  The histogram is ignored below "low" and above "high".
 * "This arrangement is by D. Stanfill, from an original composition
 * by J. Addington entitled 'RDISPLAY' (in D minor)".
 */
static void GaussLut ( Lut *lut, SiHistogram *hist,
		       double gsigma, double mean,
		       int  low, int high )
{
    const int HSIZE = hist->numBins();
    const double MID = hist->numBins() / 2.0;

    int           i;                      /* increment variable           */
    double        sig2;                   /* sigma squared                */
    double *cdf = new double[HSIZE];

    if (gsigma == 0.0) 
	cerr << "GaussLut: gsigma cannot be 0\n";
    sig2 = MID / gsigma;                          /* sigma                */
    sig2 = 2.0 * sig2 * sig2;                     /* 2 * sigma^2          */

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a gaussian histogram.  Each value
 * in cdf[] contains the gaussian formula evaluated at that point.
 * ( y = e^(-(x-mean)^2 / (2*sigma^2))  )
 */
    for (i = 0; i < HSIZE; i++)
	cdf[i] = exp ( -(i - mean) * (i - mean) / sig2 );

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}

/************************************************************************/
/* EllipseLut will force the given histogram into roughly an ellipse
 */
static void EllipseLut ( Lut *lut, SiHistogram *hist,
			 int  low, int high )
{
    const int HSIZE = hist->numBins();
    
    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];
    double        hilowDN = high - low;

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * ( y = 1 - abs(x)^power on interval (-1, 1)  )
 */

    for (i = 0; i < HSIZE; i++)
	cdf[i] = sqrt(1 - (2.0 * (double)i / hilowDN - 1) 
		      * (2.0 * (double)i / hilowDN - 1) );

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}

/************************************************************************/
/* PowerLut
 */
static void PowerLut ( Lut *lut, SiHistogram *hist,
		       int  low, int high )
{
    const int HSIZE = hist->numBins();
    
    int           i;                     /* increment variable           */
    double *cdf = new double [HSIZE];
    double	power = 2.0;
    double	hilowDN = high - low;

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * ( y = 1 - abs(x)^power on interval (-1, 1)  )
 */

    for (i = 0; i < HSIZE; i++)
	cdf[i] = 1 - pow( fabs(2.0 * (double)i / hilowDN - 1), power );

    HistToCurve(lut, hist, cdf, low, high);

    delete cdf;
}

#if 0		/* doesn't work; see comment later */
/************************************************************************/
/* PeakLut 
 */
static void PeakLut ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    const int HSIZE = hist->numBins();

    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];


/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * y = 
 */
    for (i = 0; i < HSIZE; i++);
	//  cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
	//!!!! Not implemented !!

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}
#endif

/************************************************************************/
/* MeanLut 
 */
static void MeanLut ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    const int HSIZE = hist->numBins();

    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];


/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * y =
 */
    //for (i = 0; i < HSIZE; i++)
    //  cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
    //!!!! Not implemented !!

    //  HistToCurve(lut, hist, cdf, low, high);


    // values assigned to 0 to elim. compiler warnings //

    lut=0;
    low=0;
    high=0;
    i=0;
    
    //!!! Not implemented !!
    
    delete cdf;
}

/************************************************************************/
/* stretch_gauss performs an automatic gaussian stretch, that is,
 * applying a stretch so that the histogram approximates a gaussian
 * curve with the user specified mean and sigma.
 */
void stretch_gauss ( Lut *lut, SiHistogram *hist,
                     double gsigma, double mean,
                     int  low, int high )
{
    if (gsigma <= 0.0)
        cerr << "stretch_gauss: The value of gsigma must be greater than zero\n";
    
    GaussLut(lut, hist, gsigma, mean, low, high);
}

void stretch_smooth ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    double gsigma = 0.0001;
    double mean = 127.5;
    GaussLut(lut, hist, gsigma, mean, low, high);
}

void stretch_ellipse ( Lut *lut, SiHistogram *hist,
		       int  low, int high )
{
    EllipseLut(lut, hist, low, high);
}

void stretch_power( Lut *lut, SiHistogram *hist,
		    int  low, int high )
{
    PowerLut(lut, hist, low, high);
}

#if 0			// doesn't work
/********************/
void stretch_peak ( Lut *lut, SiHistogram *hist,
                     int  dnmin, int dnmax )
{
        int npkfq=0;
      	double npkdn=0;
	//    DON'T USE DN=DNMIN OR DN=DNMAX
      	i1=dnmin+1;
      	i2=dnmax-1;
      	for (i=i1; i<i2; i++)
	{
          if(npkfq < (*hist)[i])
	  {
            npkfq=(*hist)[i];
            npkdn=i;
	  }
	}
        if (nrange < 2) nrange = 2;
	{
          nrange /= 2;
          idelta=nrange;
	}
	if (xfactr > nlev/2) || (xfactr < 1) xfactr=2.0;
	nrange = (int) ( (double)nlev / xfactr );
	nrange /= 2;
	idelta = nrange;

	lp = npts - (double)npts*0.01*perc;
	icnt = hist[npkdn];
	ilow = icnt;
	ndnmin = MIN((nlev-npkdn), (npkdn-1));
	for (i=1; i<ndnmin; i++)
	{
          idelta = i;
          j = npkdn + i;
          k = npkdn - i;
          icnt += (*hist)[i] + (*hist)[k];
          if (lp > icnt)
             ilow = icnt;
          else
             if( (lp-ilow) > (icnt-lp) ) idelta = i++;
	}

	nmin = npkdn - idelta - 1;
	nmax = npkdn + idelta - 1;
	a = (double)(dnmax-dnmin)/(double)(nmax-nmin);
	b = -a*nmin+dnmin;
	for (i = dnmin; i < dnmax; i++)
	{
          if(i < nmin)
            n = dnmin;
          else if (i > nmax)
            n = dnmax;
          else
	  {
            n = (int)( a*i + b );
            if(n < dnmin) n=dnmin;
            if(n < dnmax) n=dnmax;
	  }
          lut(i) = n;
	}

//!!!! The following line was commented out, but was uncommented simply to
// shut up the compiler.  stretch_peak and PeakLut don't work and aren't
// implemented, and should not be called until they are fixed.
   PeakLut(lut, hist, low, high);
}
/*******************/
#endif

void stretch_mean ( Lut *lut, SiHistogram *hist,
		    int  low, int high )
{
    MeanLut(lut, hist, low, high);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchBandChooser.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StretchBandChooser.cc: A component class to choose which band will be
// stretched.  The choices are Red, Green, Blue, All.  Only one option
// can be chosen.  The other should be stored.  If the user decides to 
// stretch another band, then the stored verstion becomes active.
///////////////////////////////////////////////////////
#include "StretchBandChooser.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <iostream>
using namespace std;

StretchBandChooser::StretchBandChooser(Widget parent, Cmd *cmd )
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmHORIZONTAL,
                XmNradioBehavior, True,
                XmNradioAlwaysOne, True,
		NULL );
    installDestroyHandler();
    
    _all = XtVaCreateManagedWidget("all", xmToggleButtonWidgetClass, _w,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, True,
		NULL);
    _red = XtVaCreateManagedWidget("red", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _grn = XtVaCreateManagedWidget("green", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);
    _blu = XtVaCreateManagedWidget("blue", xmToggleButtonWidgetClass, _w,
                XmNindicatorType, XmONE_OF_MANY,
                XmNset, False,
                NULL);

    XtAddCallback(_all, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_red, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_grn, XmNvalueChangedCallback,
		  &StretchBandChooser::valueChangedCallback, (XtPointer)this);
    XtAddCallback(_blu, XmNvalueChangedCallback,
		 &StretchBandChooser::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
    StretchBandChooser *obj = (StretchBandChooser *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::valueChanged()
{
    StretchValue *stretchValue = new StretchValue;
    stretchValue->changeOnlyBand = True;

    if (XmToggleButtonGetState(_all)) {
	stretchValue->band = STR_ALL;
    }
    
    if (XmToggleButtonGetState(_red)) {
        stretchValue->band = STR_RED;
    }
    
    if (XmToggleButtonGetState(_grn)) {
        stretchValue->band = STR_GREEN;
    }
    
    if (XmToggleButtonGetState(_blu)) {
        stretchValue->band = STR_BLUE;
    }

    runCmd(stretchValue);
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchBandChooser::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)value;
    StretchBand band = stretchValue->band;

    if (band == STR_ALL)			// We're it
	XmToggleButtonSetState(_all, True, False);
    else					// We're not it
	XmToggleButtonSetState(_all, False, False);

    if (band == STR_RED)                     // We're it
	XmToggleButtonSetState(_red, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_red, False, False);
    
    if (band == STR_GREEN)                     // We're it
	XmToggleButtonSetState(_grn, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_grn, False, False);
    
    if (band == STR_BLUE)                     // We're it
	XmToggleButtonSetState(_blu, True, False);
    else                                 // We're not it
	XmToggleButtonSetState(_blu, False, False);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchListCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// StretchListCmd.cc: Command class to set stretch value for 
// TABLE, ITABLE, or ALARM stretches.
//////////////////////////////////////////////////////////
#include "StretchListCmd.h"
#include "StretchCmdInterface.h"
#include "TableValue.h"
#include <iostream>
using namespace std;
#include <stdio.h>

StretchListCmd::StretchListCmd ( const char *name, int active, 
	StretchCmdInterface *stretchCmdInterface, 
	StretchType stretchType ) 
    : NoUndoCmd ( name, active )
{
    _stretchCmdInterface = stretchCmdInterface;
    _stretchType = stretchType;
}

void StretchListCmd::doit()
{
    // Change table's "in" and "out" values

    TableValue *tableValue = (TableValue*)_value;
    switch (_stretchType) {
      case ITABLE:
	_stretchCmdInterface->setITable(tableValue->inTable,
					tableValue->outTable, 
					tableValue->count);
	break;

      case TABLE: 
	_stretchCmdInterface->setTable(tableValue->inTable,
                                        tableValue->outTable,
                                        tableValue->count);
	break;

      case ALARM:
	_stretchCmdInterface->setAlarmTable(tableValue->inTable,
					    tableValue->count);
	break;

      default:
	cerr << "StretchListCmd::doit(): Memory error or wrong type is set\n";
    }

   _stretchCmdInterface->stretchIt();
}

void StretchListCmd::freeValue(CmdValue value)
{
    if (value)
	delete (TableValue *)value;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ListControl.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// ListControl.cc: Contains "Add" and "Delete" buttons for the list
// representation of table values
///////////////////////////////////////////////////////////////
#include "ListControl.h"
#include "CurListValue.h"
#include "StretchParmList.h"
#include "StretchListInterface.h"
#include "TableValue.h"
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <stdlib.h>

ListControl::ListControl ( Widget parent, const char *name, 
			   CurListValue *curListInValue,
			   CurListValue *curListOutValue,
			   StretchParmList *list,
			   StretchListInterface *interface ) 
    : UIComponent ( name )
{
    // Save the current values and the tables

    _curListInValue = curListInValue;
    _curListOutValue = curListOutValue;
    _list = list;
    _interface = interface;
    _tableValue = new TableValue;
    
    _w = XtVaCreateWidget ( _name, 
			    xmFormWidgetClass,
			    parent,
			    NULL);
    installDestroyHandler();
    
    Widget addButton = XtVaCreateManagedWidget ( "add",
			 xmPushButtonWidgetClass, _w,
			 XmNshowAsDefault, True,
			 XmNdefaultButtonShadowThickness, 1,
			 XmNalignment, XmALIGNMENT_CENTER,
			 XmNleftAttachment, XmATTACH_FORM,
                         NULL );
    Widget deleteButton = XtVaCreateManagedWidget ( "delete",
                         xmPushButtonWidgetClass, _w,
			 XmNshowAsDefault, False,
			 XmNdefaultButtonShadowThickness, 1,
			 XmNalignment, XmALIGNMENT_CENTER,
			 XmNrightAttachment, XmATTACH_FORM,
                         NULL );

    XtAddCallback ( addButton,  
		   XmNactivateCallback, 
		   &ListControl::addToListCallback,
		   (XtPointer) this );

    XtAddCallback ( deleteButton,
                   XmNactivateCallback,
                   &ListControl::deleteFromListCallback,
                   (XtPointer) this );
}

void ListControl::addToListCallback ( Widget, 
				      XtPointer clientData,
				      XtPointer callData)
{
    ListControl *obj = (ListControl *) clientData;
    obj->addToList(callData);
}

void ListControl::addToList ( XtPointer )
{
    // Make sure the values are correct
    if ((_curListInValue->getValue() < 0) || 
	(_curListInValue->getValue() > 255))
	_curListInValue->setValue(0);

    if ((_curListOutValue->getValue() < 0) || 
	(_curListOutValue->getValue() > 255))
	_curListOutValue->setValue(0);

    // Make room for one more element

    int count = _tableValue->count+1;

    int i;

    // Copy 'in' array
    
    int *in = new int[count];
    for (i = 0; i < count-1; i++)
	in[i] = _tableValue->inTable[i];
    
    // Copy 'out' array

    int *out = new int[count];
    for (i = 0; i < count-1; i++)
	out[i] = _tableValue->outTable[i];
    
    i = 0;
    while ( (i < count-1) && (in[i] != _curListInValue->getValue()) )
	i++;
    
    if ( i < count-1 ) {
	out [i] = _curListOutValue->getValue();
	count --;	// no need for another element
    }
    else {
	in [count-1] = _curListInValue->getValue();
	out [count-1] = _curListOutValue->getValue();
    }
    _list->update( in, out, count );
    
    // Sort the "in" values
    for (int m = 0; m < count-1; m++) {
	for (int n = 0; n < count-1; n++) {
	    if (in[n] > in[n+1]) {
		int tempIn = in[n];
		int tempOut = out[n];
		in[n] = in[n+1];
		out[n] = out[n+1];
		in[n+1] = tempIn;
		out[n+1] = tempOut;
	    }
	}
    }
    
    delete _tableValue->inTable;
    delete _tableValue->outTable;
    
    _tableValue->inTable = new int[count];
    _tableValue->outTable = new int[count];
    _tableValue->count = count;
    for ( i = 0; i < count; i ++ ) {
	_tableValue->inTable[i] = in[i];
	_tableValue->outTable[i] = out[i];
    }
    
    if (in) delete [] in;
    if (out) delete [] out;
    
    _interface->runIt();
}

void ListControl::deleteFromListCallback ( Widget,
					   XtPointer clientData,
					   XtPointer callData)
{
    ListControl *obj = (ListControl *) clientData;
    obj->deleteFromList(callData);
}

void ListControl::deleteFromList ( XtPointer )
{
    int count = _tableValue->count;
    
    int i;

        // Copy 'in' array

    int *in = new int[count];
    for (i = 0; i < count; i++)
	in[i] = _tableValue->inTable[i];
    
    // Copy 'out' array

    int *out = new int[count];
    for (i = 0; i < count; i++)
	out[i] = _tableValue->outTable[i];
    
    i = 0;
    int n;
    
    while ( ( in[i] != _curListInValue->getValue() ) && ( i < count) ) {
	i++;
    }
    if ( i < count ) {
	for ( n = i; n < count; n++ ) {
	    in[n] = in[n+1];
	    out[n] = out[n+1];
	}
	count--;
	
	_list->update( in, out, count );
    }

    delete _tableValue->inTable;
    delete _tableValue->outTable;
    
    _tableValue->inTable = new int[count];
    _tableValue->outTable = new int[count];
    _tableValue->count = count;
    for ( i = 0; i < count; i ++ ) {
	_tableValue->inTable[i] = in[i];
	_tableValue->outTable[i] = out[i];
    }
    _interface->runIt();
}

void ListControl::setTableValue ( TableValue *tv )
{
    delete _tableValue;
    _tableValue = tv;
    _list->update( tv->inTable, tv->outTable, tv->count );	
    
    TableValue *temp = new TableValue;
    *temp = *_tableValue;
}

int *ListControl::getInAsArray()
{
    return _tableValue->inTable;
}

int *ListControl::getOutAsArray()
{
    return _tableValue->outTable;
}

int ListControl::getCount()
{
    return _tableValue->count;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmListDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// StretchParmListDialog.cc: Custom dialog for selecting 
// values for table, itable and alarm stretches.
/////////////////////////////////////////////////////////////////
#include "StretchParmListDialog.h"
#include "StretchListInterface.h"
#include "StretchCmd.h"
#include <iostream>
using namespace std;

StretchParmListDialog::StretchParmListDialog(const char *name, 
		Cmd *cmd, StretchType stretchType, int defValue)
                                                          // = 0
    : CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    if ((stretchType != TABLE) &&
	(stretchType != ITABLE) && 
	(stretchType != ALARM))
	cerr << "Wrong values passed to the constructor or memory error\n";
    else
	_stretchType = stretchType;

    _defValue = defValue;
    _cmd = cmd;
}

Widget StretchParmListDialog::createWorkArea(Widget parent)
{
    CmdInterface *ci = new StretchListInterface(parent, _cmd, 
						_stretchType, _defValue);
    
    return ci->baseWidget();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchListInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchListCmdInterface.cc: Cmd interface to StretchCmd.  Fills in 
// StretchValue and executes the command.  Intended to be used for 
// table stretches.
//////////////////////////////////////////////////////////////
#include "StretchListInterface.h"
#include "StretchParmList.h"
#include "StretchValue.h"
#include "CurListValue.h"
#include "ListControl.h"
#include "TableValue.h"
#include "Cmd.h"
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdio.h>
			
StretchListInterface::StretchListInterface(Widget parent, Cmd *cmd, 
				       StretchType stretchType, int defValue) 
                                                                // = 0
    : CmdInterface(cmd)
{
    _stretchType = stretchType;
    _defValue = defValue;

    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler();
    
    StretchParmList *list = NULL;
    CurListValue *curListInValue = NULL;
    CurListValue *curListOutValue = NULL;
    
    curListInValue = new CurListValue(_w, "curListInValue");
    curListOutValue = new CurListValue (_w, "curListOutValue", defValue);
    list = new StretchParmList(_w, "list", curListInValue, curListOutValue );
    _control = new ListControl(_w, "listControl", curListInValue, 
			       curListOutValue, list, this);
    
    XtVaSetValues(XtParent(list->baseWidget()),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     curListOutValue->baseWidget(),
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNrightAttachment,  XmATTACH_FORM,
		  XmNtopAttachment,    XmATTACH_FORM,
		  NULL );
    
    XtVaSetValues(curListInValue->baseWidget(),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     _control->baseWidget(),
		  XmNleftAttachment,   XmATTACH_FORM,
		  NULL );
    
    XtVaSetValues(curListOutValue->baseWidget(),
		  XmNbottomAttachment, XmATTACH_WIDGET,
		  XmNbottomWidget,     _control->baseWidget(),
		  // right attachment is set in the resource file
		  // to hide OUT value if necessary
		  NULL);
    
    XtVaSetValues(_control->baseWidget(),
		  XmNbottomAttachment, XmATTACH_FORM,
		  XmNleftAttachment,   XmATTACH_FORM,
		  XmNrightAttachment,  XmATTACH_FORM,
		  NULL );
    
    list->manage();
    curListInValue->manage();
    curListOutValue->manage();
    _control->manage();

    setValue(_cmd->getValue());
}

void StretchListInterface::runIt()
{
    TableValue *t = new TableValue;
    
    t->count = _control->getCount();
    t->inTable = new int[t->count];
    t->outTable = new int[t->count];
    for (int i = 0; i < t->count; i++) {
        t->inTable[i] = _control->getInAsArray()[i];
        t->outTable[i] = _control->getOutAsArray()[i];
    }
    
    StretchValue *stretchValue;
    stretchValue = new StretchValue(*((StretchValue *)_cmd->getValue()));
    switch(_stretchType) {
    case (ITABLE):
	stretchValue->inITable = t->inTable;
	stretchValue->outITable = t->outTable;
	stretchValue->itableNoVals = t->count;
	break;
    case (TABLE):
        stretchValue->inTable = t->inTable;
        stretchValue->outTable = t->outTable;
        stretchValue->tableNoVals = t->count;
	break;
    case (ALARM):
        stretchValue->alarmValues = t->inTable;
        stretchValue->alarmNoVals = t->count;
	break;
    default:
	cerr << "runIt(): Wrong type!\n";
    }

    runCmd(stretchValue);
}

////////////////////////////////////////////////////////
// Update the interface to match a given value
////////////////////////////////////////////////////////
void StretchListInterface::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)value;
    int i;
    TableValue *tableValue = new TableValue;
    switch(_stretchType) {
    case (ITABLE):
	tableValue->count = stretchValue->itableNoVals;
	tableValue->inTable = new int [tableValue->count];
	tableValue->outTable = new int [tableValue->count];
	for (i = 0; i < tableValue->count; i++) {
	    tableValue->inTable[i] = stretchValue->inITable[i];
	    tableValue->outTable[i] = stretchValue->outITable[i];
	}
	break;
    case (TABLE):
	tableValue->count = stretchValue->tableNoVals;
        tableValue->inTable = new int [tableValue->count];
        tableValue->outTable = new int [tableValue->count];
        for (i = 0; i < tableValue->count; i++) {
            tableValue->inTable[i] = stretchValue->inTable[i];
            tableValue->outTable[i] = stretchValue->outTable[i];
        }
	break;
    case (ALARM):
	tableValue->count = stretchValue->alarmNoVals;
	tableValue->inTable = new int [tableValue->count];
	tableValue->outTable = new int [tableValue->count];
	for (i = 0; i < tableValue->count; i++) {
	    tableValue->inTable[i] = stretchValue->alarmValues[i];
	    tableValue->outTable[i] = _defValue;
	}
	break;
    default:
	cerr << "setValue(): Wrong type!\n";
    }

    // Do not delete tableValue after set call!

    _control->setTableValue(tableValue);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmList.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StretchParmList.cc:
///////////////////////////////////////////////////////
#include "StretchParmList.h"
#include "CurListValue.h"
#include <Xm/ScrolledW.h>
#include <Xm/List.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
using namespace std;

StretchParmList::StretchParmList ( Widget parent, const char *name, 
		CurListValue *inSingleValue, CurListValue *outSingleValue ) 
    : UIComponent (name)
{
    _w = XmCreateScrolledList ( parent, _name, NULL, 0 );
    
    installDestroyHandler ();
    
    XtVaSetValues( _w, XmNselectionPolicy, XmSINGLE_SELECT, NULL );
    
    _count = 0;
    _in = NULL;
    _out = NULL;

    _inSingleValue = inSingleValue;
    _outSingleValue = outSingleValue;
    
    XtAddCallback ( _w, XmNsingleSelectionCallback,
		    &StretchParmList::selectCallback,
		    ( XtPointer ) this );
}

void StretchParmList::update ( int *in, int *out, int count )
{
    int i;

    // Save the "in" and "out" values

    if (_in) delete [] _in;
    if (_out) delete [] _out;

    _count = count;
    _in = new int [count];
    _out = new int [count];

    for ( i = 0; i < _count; i++ ) {
	_in[i] = in[i];
	_out[i] = out[i];
    }
    
    // Sort the "in" values

    for (int m = 0; m < _count-1; m++) {
	for (int n = 0; n < _count-1; n++) {
	    if (_in[n] > _in[n+1]) {
		int tempIn = _in[n];
		int tempOut = _out[n];
		_in[n] = _in[n+1];
		_out[n] = _out[n+1];
		_in[n+1] = tempIn;
		_out[n+1] = tempOut;
	    }
	}
    }
    
    XmStringTable strList;
    strList = (XmStringTable) XtMalloc (count * sizeof (XmString));
    
    for (i = 0; i < count; i++) {
	char *inOut = new char[256];
	sprintf ( inOut, "%6d%6d", _in[i], _out[i] );
	strList[i] = XmStringCreateLocalized ( inOut );
	delete [] inOut;
    }
    
    XtVaSetValues ( _w,
		    XmNitems, strList,
		    XmNitemCount, count,
		    NULL );
    
    for(i = 0; i < count; i++)
	XmStringFree(strList[i]);
    XtFree((char *)strList);
}

void StretchParmList::selectCallback ( Widget,
				       XtPointer clientData,
				       XtPointer callData)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
    char *choice;
    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
    XtFree (choice);
    
    StretchParmList *obj;
    obj = ( StretchParmList * ) clientData;
    if (obj != NULL)
	obj->select(cbs->item_position);
}

void StretchParmList::select ( int position )
{
    _inSingleValue->setValue(_in[position-1]);
    _outSingleValue->setValue(_out[position-1]);	
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CurListValue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// CurListValue.cc: Current Selection on the List
////////////////////////////////////////////////////////
#include "CurListValue.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
using namespace std;

CurListValue::CurListValue (Widget parent, const char *name, int defValue)
		: KeyinView (parent, name)
{
	if (defValue)
		setValue(defValue);
	installCallback();
}

void CurListValue::setValue ( int newValue )
{
	_value = newValue;
	char buf[5];
	sprintf(buf, "%d", newValue);
	setFieldValue(buf);
}

int CurListValue::getValue ()
{
	return _value;
}

void CurListValue::update ( XtPointer )
{
	if (!strcmp("", getFieldValue()))
		_value = _defValue;
	else
		_value = atoi(getFieldValue());
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create Function.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Function.cc -- contains code for compiling and executing user-specified
 * functions.
 */
#include "Function.h"
#include <string.h>

void BlockFill(int val, char *dst, int len)
{
   memset(dst, val, len);
}

/************************************************************************/
/* ParseFunction verifies and compiles a user function string to make it
 * useable for ApplyFunction().  The string must be in upper case.
 */

#define STRINGSIZ 	100

int ParseFunction(char *string, FunctionDef *func)
{
  char	locStr[101];		/* local copy of string		*/
  int  i, j;

  i = 0; j = 0;
  while (string && i < (int)strlen(string))	/* first, replace all	*/
  {						/* occurrences of DN	*/
    if (strncmp(&string[i], "DN", 2) == 0)	/* with IN1 for knuth	*/
    {
      strncpy(&locStr[j], "IN1", 3);
      j += 3; i += 2;
    }
    else
    {
      locStr[j] = string[i];
      j++; i++;
    }
  }
  locStr[j] = '$';		/* knuth() requires str to end with $	*/

  BlockFill(0, (char *)func, sizeof(FunctionDef));
  i = zknuth(locStr, (int *)func);		/* parse the string		*/
  if (i != 0) return 0;
  return 1;
}


/************************************************************************/
/* ApplyFunction will apply the compiled function func to the argument
 * contained in the compiled expression.
 * Returns the result of the evaluated function.
 */
float ApplyFunction(FunctionDef *func, float arg)
/*  FunctionDef *func		in: compiled function		*/
/*  float arg			in: variable to evaluate	*/
{
  float result;

  func->args[0] = arg;
  zxknuth((float *)func, &result);
  return result;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchCheckBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StretchCheckBox.cc: On stretch interface, checkboxes are 
// used for turning post-stretch functions on/off.
///////////////////////////////////////////////////////
#include "StretchCheckBox.h"
#include "StretchCmdInterface.h"
#include <Xm/ToggleB.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchCheckBox::StretchCheckBox(Widget parent, const char *name,
				 StretchType type, 
				 StretchCmdInterface *stretchCmdInterface)
    : UIComponent(name)
{ 
    _type = type;
    _stretchCmdInterface = stretchCmdInterface;
    
    _w = XtVaCreateWidget(_name, xmToggleButtonWidgetClass, parent,
			  XmNset, False,
			  NULL);
    installDestroyHandler();
    
    XtAddCallback(_w, XmNvalueChangedCallback,
		  &StretchCheckBox::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::valueChangedCallback(Widget,
		    XtPointer clientData, XtPointer)
{
    StretchCheckBox *obj = (StretchCheckBox *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::valueChanged()
{
    int value = XmToggleButtonGetState(_w);

    if (value) {
	if (_type == ALARM) _stretchCmdInterface->setAlarmOn(True);
	if (_type == COMP) _stretchCmdInterface->setComplOn(True);
	if (_type == TABLE) _stretchCmdInterface->setTableOn(True);
	if (_type == OFF) _stretchCmdInterface->setOffOn(True);
   }
   else {
	if (_type == ALARM) _stretchCmdInterface->setAlarmOn(False);
	if (_type == COMP) _stretchCmdInterface->setComplOn(False);
	if (_type == TABLE) _stretchCmdInterface->setTableOn(False);
	if (_type == OFF) _stretchCmdInterface->setOffOn(False);
   }
   _stretchCmdInterface->setSensitivity(_type);
   _stretchCmdInterface->stretchIt();
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchCheckBox::setValue(Boolean on)
{
   if (on)
      XmToggleButtonSetState(_w, True, False);
   else
      XmToggleButtonSetState(_w, False, False);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchRadioBtn.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StretchRadioBtn.cc: A component class to show a Stretch type radio button
///////////////////////////////////////////////////////
#include "StretchRadioBtn.h"
#include "StretchCmdInterface.h"
#include <Xm/ToggleB.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchRadioBtn::StretchRadioBtn(Widget parent, const char *name,
		StretchType type,
		StretchCmdInterface * stretchCmdInterface)
	: UIComponent(name)
{ 
    _type = type;
    _stretchCmdInterface = stretchCmdInterface;

    _w = XtVaCreateWidget(_name, xmToggleButtonWidgetClass, parent,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, False,
		NULL);
    installDestroyHandler();

    XtAddCallback(_w, XmNvalueChangedCallback,
		  &StretchRadioBtn::valueChangedCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback function
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::valueChangedCallback(Widget,
				XtPointer clientData, XtPointer)
{
    StretchRadioBtn *obj = (StretchRadioBtn *)clientData;
    
    obj->valueChanged();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::valueChanged()
{
    int value;

    value = XmToggleButtonGetState(_w);

    if (value) {
	_stretchCmdInterface->setType(_type);
	_stretchCmdInterface->setSensitivity(_type);
	_stretchCmdInterface->stretchIt();
    }
}

////////////////////////////////////////////////////////////////////////
// Update the radio bank to reflect an externally-set value
// We don't set the sensitivity because the caller does
////////////////////////////////////////////////////////////////////////
void StretchRadioBtn::setValue(StretchType type)
{
    if (type == _type)			// We're it
	XmToggleButtonSetState(_w, True, False);
    else				// We're not it
	XmToggleButtonSetState(_w, False, False);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchParmInpView.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////
// StretchParmInpView.cc:  Implements keyin view for one stretch parameter
///////////////////////////////////////////////////////
#include "StretchParmInpView.h"
#include "StretchCmdInterface.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

StretchValue StretchParmInpView::_defaultStretchValue;	// init static data

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchParmInpView::StretchParmInpView ( Widget parent, const char *name,
		StretchValue *stretchValue, int offset, 
		StretchParmType type, StretchCmdInterface *ci )
    : KeyinView (parent, name)
{
    _stretchValue = stretchValue;
    _offset = offset;
    _type = type;
    _stretchCmdInterface = ci;
    
    installCallback();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchParmInpView::update ( XtPointer )
{
    Boolean useDefault = False;
    char *string = getFieldValue();
    
    if (string == NULL || strlen(string) == 0)
	useDefault = True;
    
    switch (_type) {
	
      case StretchInt:
	if (useDefault)
            *(int *)(((char *)_stretchValue) + _offset) =
		*(int *)(((char *)&_defaultStretchValue) + _offset);
	else
            *(int *)(((char *)_stretchValue) + _offset) = atoi(string);
	break;
	
      case StretchDouble:
	if (useDefault)
            *(double *)(((char *)_stretchValue) + _offset) =
		*(double *)(((char *)&_defaultStretchValue) + _offset);
	else
            *(double *)(((char *)_stretchValue) + _offset) = atof(string);
	break;
	
      case StretchString:
	if (useDefault)		// No non-NULL defaults for strings!
            *(char **)(((char *)_stretchValue) + _offset) = NULL;
	else {
            *(char **)(((char *)_stretchValue) + _offset) =
		new char[strlen(string)+1];
            strcpy(*(char **)(((char *)_stretchValue) + _offset), string);
	}
	break;
	
      default:
	assert(0);	// Shouldn't happen!
	break;
    }

    XtFree(string);
    
    _stretchCmdInterface->stretchIt(_stretchValue);
}

////////////////////////////////////////////////////////////////////////
// Update the text field with the value from the struct.  Don't execute 
// the cmd.
////////////////////////////////////////////////////////////////////////
void StretchParmInpView::setValue(StretchValue *value)
{
    _stretchValue = value;

    char string[1024];

    switch (_type) {
      case StretchInt:
	sprintf(string, "%d", *(int *)(((char *)_stretchValue) + _offset));
	break;

      case StretchDouble:
	sprintf(string, "%f", *(double *)(((char *)_stretchValue) + _offset));
	break;

      case StretchString:
	if (*(char **)(((char *)_stretchValue) + _offset)) {
            strncpy(string, *(char **)(((char *)_stretchValue) + _offset),
		    sizeof(string));
            string[sizeof(string)-1] = '\0';
	}
	else			// null string
            strcpy(string, "");
	break;

      default:
	assert(0);	// Shouldn't happen!
	break;
    }

    setFieldValue(string);
}

void StretchParmInpView::setForeground(char *color)
{
    XtVaSetValues(_field, 
		  XtVaTypedArg, 
		      XmNforeground, XmRString, color, (strlen(color) + 1), 
		  NULL);

    XtVaSetValues(_label, 
                  XtVaTypedArg,
                      XmNforeground, XmRString, color, (strlen(color) + 1),
                  NULL);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchPercValuesDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////////
// StretchPercValuesDialog.cc: Custom dialog for displaying  
// values for percent stretch.
/////////////////////////////////////////////////////////////////
#include "StretchPercValuesDialog.h"
#include "StretchPercValuesIf.h"
#include "Cmd.h"
#include <iostream>
using namespace std;

StretchPercValuesDialog::StretchPercValuesDialog(const char *name, Cmd *cmd)
    : CustomDialog(name, Default, Invisible, Invisible, Invisible, Visible)
{
    _cmd = cmd;
}

Widget StretchPercValuesDialog::createWorkArea(Widget parent)
{
    CmdInterface *ci = new StretchPercValuesIf(parent, _cmd);
    return ci->baseWidget();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StretchPercValuesIf.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// StretchPercValuesIf.cc: Shows current percent stretch limits.
//////////////////////////////////////////////////////////////
#include "StretchPercValuesIf.h"
#include "StretchValue.h"
#include "KeyinView.h"
#include "Cmd.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include "xvmaininc.h"		// for UNUSED

StretchPercValuesIf::StretchPercValuesIf(Widget parent, Cmd *cmd)
    : CmdInterface(cmd)
{
    _w = XtVaCreateWidget(_name,
			  xmFormWidgetClass, parent,
			  NULL );
    installDestroyHandler(); 

    Widget mainLabel = XtVaCreateManagedWidget("Percent Stretch Values",
			  xmLabelWidgetClass, _w, 
			  XmNtopAttachment, XmATTACH_FORM,
			  XmNleftAttachment, XmATTACH_FORM,
			  XmNrightAttachment, XmATTACH_FORM,
			  NULL);

    Widget subForm = XtVaCreateManagedWidget("Sub Form",
			 xmFormWidgetClass, _w,
			 XmNtopAttachment, XmATTACH_WIDGET,
			 XmNtopWidget, mainLabel,
			 XmNbottomAttachment, XmATTACH_FORM,
			 NULL);

    Widget lowRowCol = XtVaCreateManagedWidget("Low Row Col",
				        xmRowColumnWidgetClass, subForm ,
					XmNtopAttachment, XmATTACH_FORM,
				 	XmNpacking, XmPACK_COLUMN, 
					XmNnumColumns, 4,
					XmNorientation, XmHORIZONTAL,
					XmNisAligned, True,
					XmNentryAlignment, XmALIGNMENT_END,
					NULL);

    Widget highRowCol = XtVaCreateManagedWidget("High Row Col",
					 xmRowColumnWidgetClass, subForm ,
					 XmNleftAttachment, XmATTACH_WIDGET,
					 XmNleftWidget, lowRowCol,
					 XmNtopAttachment, XmATTACH_FORM  ,
					 XmNpacking, XmPACK_COLUMN, 
					 XmNnumColumns, 4,
					 XmNorientation, XmHORIZONTAL,
					 XmNisAligned, True,
					 XmNentryAlignment, XmALIGNMENT_END,
					 NULL);

    Widget UNUSED(lowLabel) = XtVaCreateManagedWidget("Low Label",
					      xmLabelWidgetClass, lowRowCol,
					      NULL);

    Widget UNUSED(highLabel) = XtVaCreateManagedWidget("High Label",
					       xmLabelWidgetClass, highRowCol,
					       NULL);
					
    _lPercValueRed = new KeyinView(lowRowCol, "lPercValueRed");
    _lPercValueGrn = new KeyinView(lowRowCol, "lPercValueGrn");
    _lPercValueBlu = new KeyinView(lowRowCol, "lPercValueBlu");

    _hPercValueRed = new KeyinView(highRowCol, "hPercValueRed");
    _hPercValueGrn = new KeyinView(highRowCol, "hPercValueGrn");
    _hPercValueBlu = new KeyinView(highRowCol, "hPercValueBlu");

    _lPercValueRed->manage();
    _lPercValueGrn->manage();
    _lPercValueBlu->manage();
    _hPercValueRed->manage();
    _hPercValueGrn->manage();
    _hPercValueBlu->manage();

    setValue(_cmd->getValue());

}

////////////////////////////////////////////////////////
// Update the interface to match a given value
////////////////////////////////////////////////////////
void StretchPercValuesIf::setValue(CmdValue value)
{
    StretchValue *stretchValue = (StretchValue *)(_cmd->getValue());

    char buf[16];

    sprintf(buf, "%f", stretchValue->lPercValueRed);
    _lPercValueRed->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->lPercValueGrn);
    _lPercValueGrn->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->lPercValueBlu);
    _lPercValueBlu->setFieldValue(buf);

    sprintf(buf, "%f", stretchValue->hPercValueRed);
    _hPercValueRed->setFieldValue(buf);
 
    sprintf(buf, "%f", stretchValue->hPercValueGrn);
    _hPercValueGrn->setFieldValue(buf);
 
    sprintf(buf, "%f", stretchValue->hPercValueBlu);
    _hPercValueBlu->setFieldValue(buf);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadLutFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// LoadPseudoFileCmd: A Command class that loads a IBIS file.  The Command
// value is a dynamically allocated single string, suitable for passing in
// to a PseudoValue subclass.
/////////////////////////////////////////////////////////
#include "LoadLutFileCmd.h"
#include "StretchValue.h"
#include "ErrorManager.h"
#include "UIComponent.h"              // only for strdup()
#include "ibisfile.h"
#include "zvproto.h"
#include "file_no_path.h"
#include <stdio.h>
#include <assert.h>

LoadLutFileCmd::LoadLutFileCmd(const char *name, int active, Cmd *stretchCmd)
    : NoUndoCmd(name, active)
{
    _stretchCmd = stretchCmd;
}

void LoadLutFileCmd::doit()
{
    const int lut_size = 256; //!!! Make this more general by passing lut

    char *filename = (char *)_value;

    int unit, ibis, status, record, i;
    int lut[lut_size][3];
    
    if (!filename) return;
    
    // open IBIS file for reading

    status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);
    
    status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileOpen", 
				 "Error loading file");
	return;
    }

    char *stripFilename = strdup(filename);
    file_no_path(stripFilename);
    
    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							stripFilename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) {
	theErrorManager->process(Error, "IBISRecordOpen", 
				 "Error loading file");
	IBISFileClose(ibis, 0);
	return;
    }
    
    for (i = 1; i <= lut_size; i++) {
	status = IBISRecordRead(record, (char*)lut[i-1], i);
	if (status != 1) {
	    theErrorManager->process(Error, "IBISRecordRead", 
				     "Error loading file");
	    IBISFileClose(ibis, 0);
	    return;
	}
    }
    
    IBISFileClose(ibis, 0);

    delete [] stripFilename;

    StretchValue *stretchValue;

    // Stretch all planes to red plane's value 

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_ALL;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
	delete [] stretchValue->inITable;
    if (stretchValue->outITable)
	delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
    
    for (i = 0; i < stretchValue->itableNoVals; i++) {
	stretchValue->inITable[i] = i;
	stretchValue->outITable[i] = lut[i][0];
    }

    _stretchCmd->execute(stretchValue);

    // Stretch green plane only

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_GREEN;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
        delete [] stretchValue->inITable;
    if (stretchValue->outITable)
        delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
 
    for (i = 0; i < stretchValue->itableNoVals; i++) {
        stretchValue->inITable[i] = i;
        stretchValue->outITable[i] = lut[i][1];
    }
 
    _stretchCmd->execute(stretchValue);

    // Stretch blue plane only

    stretchValue = new StretchValue(*((StretchValue*)_stretchCmd->getValue()));
    stretchValue->band = STR_BLUE;
    stretchValue->stretchName = ITABLE;
    stretchValue->itableNoVals = lut_size;
    if (stretchValue->inITable)
        delete [] stretchValue->inITable;
    if (stretchValue->outITable)
        delete [] stretchValue->outITable;
    stretchValue->inITable = new int [lut_size];
    stretchValue->outITable = new int [lut_size];
 
    for (i = 0; i < stretchValue->itableNoVals; i++) {
        stretchValue->inITable[i] = i;
        stretchValue->outITable[i] = lut[i][2];
    }
 
    _stretchCmd->execute(stretchValue);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SaveLutFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////
// SaveLutFileCmd: A Command class that saves pseudocolor tables in  an
// IBIS file format.  The Command value is a dynamically allocated single
// string.
/////////////////////////////////////////////////////////
#include "SaveLutFileCmd.h"
#include "Lut.h"
#include "ErrorManager.h"
#include "UIComponent.h"                // only for strdup()
#include "ibisfile.h"
#include "zvproto.h"
#include "file_no_path.h"
#include <stdio.h>
#include <assert.h>

SaveLutFileCmd::SaveLutFileCmd(const char *name, int active, 
			       Lut *lutRed, Lut *lutGrn, Lut *lutBlu)
    : NoUndoCmd(name, active)
{
    _lutRed = lutRed;
    _lutGrn = lutGrn;
    _lutBlu = lutBlu;
}

void SaveLutFileCmd::doit()
{
    const int lut_size = 256; //!!! Make this more general by passing lut

    char *filename = (char *)_value;

    int unit, i, ibis, status, record;
    int lutcols[3];
    int lut[lut_size][3];
 
    if (!filename) return;
    
    for (i = 0; i < 3; i++) 
	lutcols[i] = i+1;
 
    status = zvunit(&unit, (char *)"out_file",  1, "u_name", filename, NULL);
    if (status != 1) {
	theErrorManager->process(Error, "zvunit", "Error saving file", 
				 filename);
	return;
    }

    status = IBISFileOpen(unit, &ibis, (char *)IMODE_WRITE, 3,
			  lut_size, 0, (char *)IORG_COLUMN);
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileOpen", 
				 "Error saving file", filename);
	return;
    }
    
    status = IBISFileSet(ibis, (char *)IFILE_TYPE, (char *)"LOOKUP_TABLE", 0);
    if (status != 1) {
        theErrorManager->process(Error, "IBISFileSet",
                                 "Error saving file", filename);
	return;
    }

    status = ICLNewRGB(ibis, 1, 2, 3, 0);
    if (status < 0) {
	theErrorManager->process(Error, "ICLNewRGB",
                                 "Error saving file", filename);
	return;
    }

    char *stripFilename = strdup(filename);
    file_no_path(stripFilename);

    status = ICLNewLOOKUP_TABLE(ibis, lutcols, 3, 0, 0, 
				(char *)"PSEUDOCOLOR", stripFilename);
    if (status < 0) {
        theErrorManager->process(Error, "ICLNewLOOKUP_TABLE",
                                 "Error saving file", filename);
	return;
    }

    ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
							filename);
    ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
							(char *)"$MyLut");
    
    status = IBISRecordOpen(ibis, &record, (char *)"$MyRED | $MyGRN | $MyBLU",
			    0, 0, (char *)IFMT_FULL);
    if (status != 1) {
	theErrorManager->process(Error, "IBISRecordOpen",
                                 "Error saving file", filename);
	return;
    }
    
    for (i = 0; i < lut_size; i++) {
	lut[i][0] = (*_lutRed)[i];
	lut[i][1] = (*_lutGrn)[i];
	lut[i][2] = (*_lutBlu)[i];
    }
    for (i = 0; i < lut_size; i++) {
	status = IBISRecordWrite(record, (char*)lut[i], i+1);
	if (status != 1) {
	    theErrorManager->process(Error, "IBISRecordWrite",
				     "Error saving file", filename);
	    return;
	}
    }
    
    // close up shop

    status = IBISFileClose( ibis, 0 );
    if (status != 1) {
	theErrorManager->process(Error, "IBISFileClose",
				 "Error saving file", filename);
    }

    delete [] stripFilename;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create stretch.imake
#define SUBROUTINE stretch
#define MODULE_LIST \
	StretchCmd.cc StretchDialog.cc \
	StretchCmdInterface.cc StretchValue.cc \
	StretchFun.cc StretchBandChooser.cc
#define MODULE_LIST2 \
	StretchListCmd.cc ListControl.cc \
	StretchParmListDialog.cc StretchListInterface.cc \
        StretchParmList.cc CurListValue.cc
#define MODULE_LIST3 \
	Function.cc StretchCheckBox.cc \
	StretchRadioBtn.cc StretchParmInpView.cc \
	StretchPercValuesDialog.cc StretchPercValuesIf.cc
#define MODULE_LIST4 \
	LoadLutFileCmd.cc SaveLutFileCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
#define LIB_P2SUB

$ Return
$!#############################################################################
