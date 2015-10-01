$!****************************************************************************
$!
$! Build proc for MIPL module histcommands
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:24
$!
$! Execute by entering:		$ @histcommands
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
$ write sys$output "*** module histcommands ***"
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
$ write sys$output "Invalid argument given to histcommands.com file -- ", primary
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
$   if F$SEARCH("histcommands.imake") .nes. ""
$   then
$      vimake histcommands
$      purge histcommands.bld
$   else
$      if F$SEARCH("histcommands.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histcommands
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histcommands.bld "STD"
$   else
$      @histcommands.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histcommands.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histcommands.com -mixed -
	-s AxisCmd.cc HistCmd.cc SetAscAxisCmd.cc SetDescAxisCmd.cc -
	   SetHorHistGraphCmd.cc SetPopUpDirColCmd.cc SetPopUpDirRowCmd.cc -
	   SetStackBlendCmd.cc SetStackNoBlendCmd.cc SetVertHistGraphCmd.cc -
	   StatsCmd.cc HistLogScaleCmd.cc -
	-i histcommands.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create AxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// AxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "AxisCmd.h"
#include "HistBox.h"
#include <iostream>
#include <stdint.h>
using namespace std;

AxisCmd::AxisCmd ( const char *name, int active, HistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    int value = (int) _box->AxisIsDisplayed ( );
    _value = (CmdValue) (uintptr_t) value; 
    newValue ( );
}

void AxisCmd::doit ( )
{
    _oldValue = _box->AxisIsDisplayed ( );
    _box->showAxis( (_value != 0) );
}      

void AxisCmd::undoit ( )
{
    _box->showAxis( _oldValue );
    _value = (CmdValue) ((uintptr_t) _oldValue);
    newValue ( );
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// HistCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include "HistCmd.h"
#include <iostream>
using namespace std;
#include <stdint.h>

HistCmd::HistCmd ( char *name, int active, HistBox *obj ) : Cmd ( name, active )
{

    _menuView = obj;
    int value = (int) _menuView->HistIsDisplayed();
    _value = (CmdValue) ((uintptr_t) value); 
    newValue ();
}

void HistCmd::doit()
{

    _oldValue = _menuView->HistIsDisplayed();
    _menuView->showHist( (_value != 0));

}      

void HistCmd::undoit()
{
    // Just print a message that allows us to trace the execution
    _menuView->showHist( _oldValue);
    _value = (CmdValue) ((uintptr_t) _oldValue);
    newValue();
}       

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetAscAxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetAscAxisCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetAscAxisCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetAscAxisCmd::SetAscAxisCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getVerAxisDirType() == ASC) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetAscAxisCmd::doit()
{

  if (_value) {
      _menuView->setVerAxisDirType( ASC );
  }

}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetDescAxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetDescAxisCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetDescAxisCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetDescAxisCmd::SetDescAxisCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( _menuView->getVerAxisDirType() == DESC) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetDescAxisCmd::doit()
{

    if (_value ) {
      _menuView->setVerAxisDirType( DESC );
    }

}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetHorHistGraphCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetHorHistGraphCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetHorHistGraphCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetHorHistGraphCmd::SetHorHistGraphCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getOrientType() == VERTICAL) {
       _value = (CmdValue) TRUE;
       newValue ();
     }
}

void SetHorHistGraphCmd::doit()
{

    if (_value) {
      _menuView->setOrientType( VERTICAL );
    }

}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetPopUpDirColCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetPopUpDirColCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetPopUpDirColCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>
using namespace std;

SetPopUpDirColCmd::SetPopUpDirColCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( (_menuView->getPopupDirectionType() == COLUMN && _menuView->getMethodType() == POPUP) || _menuView->getHistB() == NULL ) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
  }

void SetPopUpDirColCmd::doit()
{

    if (_value) {
       _menuView->setPopupDirectionType( COLUMN );
     }
}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetPopUpDirRowCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetPopUpDirRowCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetPopUpDirRowCmd.h"
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetPopUpDirRowCmd::SetPopUpDirRowCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( (_menuView->getPopupDirectionType() == ROW && _menuView->getMethodType() == POPUP) || _menuView->getHistB() == NULL) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetPopUpDirRowCmd::doit()
{
    if (_value) {
        _menuView->setPopupDirectionType( ROW );
     }
}      


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetStackBlendCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetStackBlendCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetStackBlendCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetStackBlendCmd::SetStackBlendCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if ( _menuView->getMethodType()== BLEND || _menuView->getHistB() == NULL ) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }      
}

void SetStackBlendCmd::doit()
{
    if (_value) {
       _menuView->setMethodType( BLEND );
    }
}      


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetStackNoBlendCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetStackNoBlendCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetStackNoBlendCmd.h"
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetStackNoBlendCmd::SetStackNoBlendCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getMethodType() == STACKED  || _menuView->getHistB() == NULL ) {
      _value = ( CmdValue ) TRUE;
      newValue();
    }
}

void SetStackNoBlendCmd::doit()
{
    if (_value) {
       _menuView->setMethodType ( STACKED );
     }
}      



$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SetVertHistGraphCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SetVertHistGraphCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "SetVertHistGraphCmd.h"
#include "Histogram.h"
#include "HistBox.h"
#include <iostream>

SetVertHistGraphCmd::SetVertHistGraphCmd ( const char *name, int active, HistBox *obj, CmdList *list=NULL ) : RadioCmd ( name, active, list )
{
    _menuView = obj;
    if (_menuView->getOrientType() == HORIZONTAL) {
       _value = (CmdValue) TRUE;
       newValue ();
     }
}

void SetVertHistGraphCmd::doit()
{

    if (_value) {
      _menuView->setOrientType( HORIZONTAL );
    }

}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create StatsCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// StatsCmd.C: Example, dummy command class
//////////////////////////////////////////////////////////
#include "Application.h"
#include "Histogram.h"
#include "HistBox.h"
#include "StatsCmd.h"
#include <iostream>
#include <stdint.h>

StatsCmd::StatsCmd ( const char *name, int active, HistBox *obj ) : Cmd ( name, active )
{

    _menuView = obj;
    int value = (int) _menuView->StatIsDisplayed();
    _value = (CmdValue) (uintptr_t) value; 
    newValue ();
}

void StatsCmd::doit()
{

    _oldValue = _menuView->StatIsDisplayed();
    _menuView->showStat(  (_value != 0));

}      

void StatsCmd::undoit()
{
    // Just print a message that allows us to trace the execution
    _menuView->showStat( _oldValue);
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}       














$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create HistLogScaleCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// HistLogScaleCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "HistLogScaleCmd.h"
#include "HistBox.h"
#include <iostream>
#include <stdint.h>

HistLogScaleCmd::HistLogScaleCmd ( const char *name, int active, HistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    int value = (int) _box->logScaleIsSet ( );
    _value = (CmdValue) ((uintptr_t) value); 
    newValue ( );
}

void HistLogScaleCmd::doit ( )
{
    _oldValue = _box->logScaleIsSet ( );
    _box->setLogScale( (_value != 0) );
}

void HistLogScaleCmd::undoit ( )
{
    _box->setLogScale( _oldValue );
    _value = (CmdValue)((uintptr_t)  _oldValue);
    newValue ( );
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histcommands.imake
#define SUBROUTINE histcommands
#define MODULE_LIST AxisCmd.cc HistCmd.cc SetAscAxisCmd.cc SetDescAxisCmd.cc \
   SetHorHistGraphCmd.cc SetPopUpDirColCmd.cc SetPopUpDirRowCmd.cc

#define MODULE_LIST2 \
   SetStackBlendCmd.cc SetStackNoBlendCmd.cc \
   SetVertHistGraphCmd.cc StatsCmd.cc HistLogScaleCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP


$ Return
$!#############################################################################
