$!****************************************************************************
$!
$! Build proc for MIPL module histspike
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:25
$!
$! Execute by entering:		$ @histspike
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
$ write sys$output "*** module histspike ***"
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
$ write sys$output "Invalid argument given to histspike.com file -- ", primary
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
$   if F$SEARCH("histspike.imake") .nes. ""
$   then
$      vimake histspike
$      purge histspike.bld
$   else
$      if F$SEARCH("histspike.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histspike
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histspike.bld "STD"
$   else
$      @histspike.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histspike.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histspike.com -mixed -
	-s SpikeDialog.cc KeyInSpikeInterface.cc SpikeButtonInterface.cc -
	   SpikeCmd.cc -
	-i histspike.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SpikeDialog.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
//SpikeDialog.cc: Source file for creating Spike Dialog Box
////////////////////////////////////////////////////////////////////////
#include "HistBox.h"
#include "Histogram.h"
#include "SpikeDialog.h"
#include "KeyInSpikeInterface.h"
#include "SpikeButtonInterface.h"
#include "SpikeCmd.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>

SpikeDialog::SpikeDialog ( HistBox *obj, const char *name )
	: CustomDialog(name, Invisible, Invisible, Invisible, Visible, Visible)
{
   _histBox = obj;
}

Widget SpikeDialog::createWorkArea( Widget parent)
{
   
   int spike;
   spike = _histBox->getSpike();

   Histogram *histogram;
   histogram = _histBox->getHistR();

   Widget rc = XtVaCreateWidget("workArea", xmRowColumnWidgetClass, parent,
				XmNorientation, XmHORIZONTAL,
				XmNpacking,     XmPACK_TIGHT,
				NULL);

   _SpikeCmd = new SpikeCmd ("SpikeCmd", TRUE, _histBox);

   KeyInSpikeInterface  *keyIn; 
   keyIn = new KeyInSpikeInterface ( rc, _SpikeCmd);


   CmdInterface *incButton;
   incButton = new SpikeButtonInterface ( rc, 1, histogram,  _SpikeCmd );

   CmdInterface *decButton;
   decButton = new SpikeButtonInterface ( rc, -1, histogram, _SpikeCmd );

   keyIn->manage();
   incButton->manage();
   decButton->manage();

   return rc;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create KeyInSpikeInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// KeyInSpikeInterface.C: An KeyIn interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "KeyInSpikeInterface.h"
#include "Cmd.h"
#include "KeyinView.h"
#include "stdlib.h"
#include "stdio.h"
#include <iostream>
#include <stdint.h>
using namespace std;
#include <Xm/RowColumn.h>

KeyInSpikeInterface::KeyInSpikeInterface ( Widget parent,
				  Cmd *cmd ) : CmdInterface ( cmd )
{
  _w = XtCreateManagedWidget(_name, xmRowColumnWidgetClass, parent, 
			     NULL, 0 );
  installDestroyHandler();

  _spike = addOneSubView(_w, "SpikeValue:");
  setValue(_cmd->getValue());
}

KeyInSpikeInterface::~KeyInSpikeInterface ()
{
delete _spike;
}

KeyinView *KeyInSpikeInterface::addOneSubView (Widget parent, const char *name)
{
  KeyinView *view = new KeyinView(parent, name);
  view->manage();
  view->installCallback(&CmdInterface::executeCmdCallback, (XtPointer)this);
  return view;
}

void KeyInSpikeInterface::executeCmd(XtPointer)
{
  char *_strValue;
  int  _intValue;
  
  _strValue = _spike->getFieldValue();
  _intValue = atoi(_strValue);
  XtFree(_strValue);

  runCmd((CmdValue) (uintptr_t) _intValue);
}

void KeyInSpikeInterface::setValue(CmdValue value)
{
  char _strValue[20];
  int  _intValue = (int) (uintptr_t)value;
  
  sprintf(_strValue, "%d", _intValue);

  _spike->setFieldValue(_strValue);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SpikeButtonInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////
// SpikeButtonInterface.C: An SpikeButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "SpikeButtonInterface.h"
#include "Cmd.h"
#include "Histogram.h"
#include <stdint.h>
#define  MINVALUE   1

SpikeButtonInterface::SpikeButtonInterface ( Widget parent, 
					    int step,
					    Histogram *obj,
					    Cmd *cmd ) : ArrowButtonInterface ( parent, cmd )
{
  _step = step;
  _histogram = obj;

  if (_step < 0){
    XtVaSetValues (_w, XmNarrowDirection,     XmARROW_DOWN,
		   NULL);
    }
  setValue(_cmd->getValue());

}

void SpikeButtonInterface::executeCmd(XtPointer)
{
  int value = (int) (uintptr_t)_cmd->getValue();
  value += _step;
  runCmd ((CmdValue)(uintptr_t)value);
}

void SpikeButtonInterface::setValue(CmdValue value)
{
  int MAXVALUE = _histogram->numBins();
//  cout <<"The current value of MAXVALUE is:"<< MAXVALUE<<".\n"<<flush;
  
  //Deactivate Increment Spike Button if spike value is 
  //greater than or equal to 256 (MAXVALUE) 
  int x = (int) (uintptr_t)value;

  if ( (_step > 0) && ( x >= MAXVALUE)){
    deactivate();
  }
  else if ( (_step < 0) && ( x <= MINVALUE)){
    deactivate();
  }
  else activate();

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SpikeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SpikeCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SpikeCmd.h"
#include "HistBox.h"
#include "Application.h"
#include <iostream>
#include <stdint.h>

SpikeCmd::SpikeCmd ( const char *name, int active, HistBox *obj ) : Cmd ( name, active)
{
    _histBox = obj;
    _value = (CmdValue)(uintptr_t)  _histBox->getSpike();
}

void SpikeCmd::doit()
{

    _prevValue = _histBox->getSpike();
    _histBox->setSpike((int) (uintptr_t)_value );

}      

void SpikeCmd::undoit()
{  
    _value = (CmdValue) (uintptr_t) _prevValue;
    _histBox->setSpike((int) (uintptr_t)_value );
    newValue();
}       


$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histspike.imake
#define SUBROUTINE histspike
#define MODULE_LIST SpikeDialog.cc KeyInSpikeInterface.cc SpikeButtonInterface.cc SpikeCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP

$ Return
$!#############################################################################
