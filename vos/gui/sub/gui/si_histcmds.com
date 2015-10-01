$!****************************************************************************
$!
$! Build proc for MIPL module si_histcmds
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:37
$!
$! Execute by entering:		$ @si_histcmds
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
$ write sys$output "*** module si_histcmds ***"
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
$ write sys$output "Invalid argument given to si_histcmds.com file -- ", primary
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
$   if F$SEARCH("si_histcmds.imake") .nes. ""
$   then
$      vimake si_histcmds
$      purge si_histcmds.bld
$   else
$      if F$SEARCH("si_histcmds.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake si_histcmds
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @si_histcmds.bld "STD"
$   else
$      @si_histcmds.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create si_histcmds.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack si_histcmds.com -mixed -
	-s SiHistShowAxisCmd.cc SiHistShowStatCmd.cc SiHistSetAscAxisCmd.cc -
	   SiHistSetDescAxisCmd.cc SiHistSetHorGraphCmd.cc -
	   SiHistSetVerGraphCmd.cc SiHistSetColCmd.cc SiHistSetRowCmd.cc -
	   SiHistSetBlendCmd.cc SiHistSetStackCmd.cc SiHistSpikeCmd.cc -
	   SiHistSetLogScaleCmd.cc -
	-i si_histcmds.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SiHistShowAxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistShowAxisCmd.h: Displays or hides histogram axis.
/////////////////////////////////////////////////////////////
#include "SiHistShowAxisCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistShowAxisCmd::SiHistShowAxisCmd ( const char *name, int active,
							SiHistBox *box )
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->axisIsDisplayed(); 
    newValue();
}

void SiHistShowAxisCmd::doit()
{
    _oldValue = _box->axisIsDisplayed();
    _box->showAxis ( (_value != 0) );
}

void SiHistShowAxisCmd::undoit()
{
    _box->showAxis ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistShowStatCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistShowStatsCmd.C:  
//////////////////////////////////////////////////////////
#include "SiHistShowStatCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistShowStatCmd::SiHistShowStatCmd ( const char *name, int active,
							SiHistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->statIsDisplayed();; 
    newValue ();
}

void SiHistShowStatCmd::doit()
{
    _oldValue = _box->statIsDisplayed();
    _box->showStat( (_value != 0) );
}

void SiHistShowStatCmd::undoit()
{
    _box->showStat ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetAscAxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSetAscAxisCmd.C:  Display histogram with ascending/discending
// axis.
//////////////////////////////////////////////////////////
#include "SiHistSetAscAxisCmd.h"
#include "SiHistBox.h"

SiHistSetAscAxisCmd::SiHistSetAscAxisCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getVerAxisDirType() == ASC) {
	_value = (CmdValue)TRUE;
	newValue();
    }
}

void SiHistSetAscAxisCmd::doit()
{
    if (_value)
	_box->setVerAxisDirType ( ASC );
    else
	_box->setVerAxisDirType ( DESC );
}

void SiHistSetAscAxisCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value )
	_box->setVerAxisDirType ( DESC );
    else
	_box->setVerAxisDirType ( ASC );

   _value = (CmdValue)!value;
    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetDescAxisCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSetDescAxisCmd.C:  Set descending orientation on histogram 
// axis.
//////////////////////////////////////////////////////////
#include "SiHistSetDescAxisCmd.h"
#include "SiHistBox.h"

SiHistSetDescAxisCmd::SiHistSetDescAxisCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if ( _box->getVerAxisDirType() == DESC)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetDescAxisCmd::doit()
{
    if (_value)
	_box->setVerAxisDirType ( DESC );
    else 
	_box->setVerAxisDirType ( ASC );
}

void SiHistSetDescAxisCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value )
	_box->setVerAxisDirType ( ASC );
    else 
	_box->setVerAxisDirType ( DESC );

    _value = (CmdValue)!value;
    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetHorGraphCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetHorGraphCmd.h:  Set horizontal orientation on hstogram
// plot.
/////////////////////////////////////////////////////////////
#include "SiHistSetHorGraphCmd.h"
#include "SiHistBox.h"

SiHistSetHorGraphCmd::SiHistSetHorGraphCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if (_box->getOrientType() == VERTICAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetHorGraphCmd::doit()
{
    if (_value )
	_box->setOrientType ( VERTICAL );
}

void SiHistSetHorGraphCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value ) 
        _box->setOrientType ( HORIZONTAL );
    else
        _box->setOrientType ( VERTICAL );

    _value = (CmdValue)!value;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetVerGraphCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSetVerGraphCmd.C:  
//////////////////////////////////////////////////////////
#include "SiHistSetVerGraphCmd.h"
#include "SiHistBox.h"

SiHistSetVerGraphCmd::SiHistSetVerGraphCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getOrientType() == HORIZONTAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue ();
}

void SiHistSetVerGraphCmd::doit()
{
    _oldValue = _box->getOrientType();

    if (_value)
	_box->setOrientType ( HORIZONTAL );
}

void SiHistSetVerGraphCmd::undoit()
{
    _box->setOrientType ( _oldValue );

    if ( _oldValue == HORIZONTAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue ();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetColCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetColCmd.cc:  Arrange r,g,b histograms in column order.
/////////////////////////////////////////////////////////////
#include "SiHistSetColCmd.h"
#include "SiHistBox.h"

SiHistSetColCmd::SiHistSetColCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if ( ( _box->getPopupDirectionType() == COLUMN &&
	   _box->getMethodType() == POPUP ) ||
	 _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

	newValue();
}

void SiHistSetColCmd::doit()
{
    if (_value)
       _box->setPopupDirectionType ( COLUMN );
}

void SiHistSetColCmd::undoit()
{
//    Boolean value = _box->getPopupDirectionType();
//
  //  if ( value ) 
    //   _box->setPopupDirectionType ( ROW );
//    else
  //      _box->setPopupDirectionType ( COLUMN );
//
  //  _value = (CmdValue)!value;
    //newValue();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetRowCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetRowCmd.cc:  Arrange r,g,b histograms in row order.
/////////////////////////////////////////////////////////////
#include "SiHistSetRowCmd.h"
#include "SiHistBox.h"

SiHistSetRowCmd::SiHistSetRowCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if ( ( _box->getPopupDirectionType() == ROW &&
	   _box->getMethodType() == POPUP ) ||
	_box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

	newValue();
}

void SiHistSetRowCmd::doit()
{
    if (_value)
	_box->setPopupDirectionType ( ROW );
}

void SiHistSetRowCmd::undoit()
{
//    Boolean value = _box->getPopupDirectionType();
//
  //  if ( value ) 
//	_box->setPopupDirectionType ( COLUMN );
  //  else
//	_box->setPopupDirectionType ( ROW );
//
   // _value = (CmdValue)!value;
  //  newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetBlendCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSetBlendCmd.C:  Display histogram in blend mode.
//////////////////////////////////////////////////////////
#include "SiHistSetBlendCmd.h"
#include "SiHistBox.h"

SiHistSetBlendCmd::SiHistSetBlendCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if ( _box->getMethodType() == BLEND || _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetBlendCmd::doit()
{
    _oldValue = _box->getMethodType();

    if (_value) {
	_box->setMethodType ( BLEND );
    }
}      

void SiHistSetBlendCmd::undoit()
{
    _box->setMethodType ( _oldValue );

    if ( _oldValue == BLEND || _box->getHistB() == NULL ) 
	_value = (CmdValue)TRUE;
    else
	_value = (CmdValue)FALSE;

    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetStackCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSetStackCmd.C: 
//////////////////////////////////////////////////////////
#include "SiHistSetStackCmd.h"
#include "SiHistBox.h"

SiHistSetStackCmd::SiHistSetStackCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getMethodType() == STACKED  || _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetStackCmd::doit()
{
    _oldValue = _box->getMethodType();
    if (_value) {
       _box->setMethodType ( STACKED );
    }
}      

void SiHistSetStackCmd::undoit()
{
    _box->setMethodType ( _oldValue );
    if (_box->getMethodType() == STACKED  || _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSpikeCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
///////////////////////////////////////////////////////////
// SiHistSpikeCmd.C:  Set histogram spike value.
//////////////////////////////////////////////////////////
#include "SiHistSpikeCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistSpikeCmd::SiHistSpikeCmd ( const char *name, int active, SiHistBox *box ) 
	: Cmd ( name, active)
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->getSpike();
    newValue();
}

void SiHistSpikeCmd::doit()
{
    _oldValue = _box->getSpike();
    _box->setSpike( (int) (uintptr_t)_value );
}

void SiHistSpikeCmd::undoit()
{
    _value = (CmdValue) (uintptr_t) _oldValue;
    _box->setSpike( (int) (uintptr_t) _value );
    newValue();
}       
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create SiHistSetLogScaleCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/////////////////////////////////////////////////////////////
// SiHistSetLogScaleCmd.h: Displays histogram in a logarithmic scale.
/////////////////////////////////////////////////////////////
#include "SiHistSetLogScaleCmd.h"
#include "SiHistBox.h"
#include <stdint.h>

SiHistSetLogScaleCmd::SiHistSetLogScaleCmd ( const char *name, int active, SiHistBox *box ) 
	: Cmd ( name, active )
{
    _box = box;
    _value = (CmdValue) (uintptr_t) _box->logScaleIsSet(); 
    newValue();
}

void SiHistSetLogScaleCmd::doit()
{
    _oldValue = _box->logScaleIsSet();
    _box->setLogScale ( (_value != 0) );
}

void SiHistSetLogScaleCmd::undoit()
{
    _box->setLogScale ( _oldValue );
    _value = (CmdValue) (uintptr_t) _oldValue;
    newValue();
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create si_histcmds.imake
#define SUBROUTINE si_histcmds
#define MODULE_LIST \
   SiHistShowAxisCmd.cc \
   SiHistSetAscAxisCmd.cc \
   SiHistSetDescAxisCmd.cc \
   SiHistSetHorGraphCmd.cc

#define MODULE_LIST2 \
   SiHistSetColCmd.cc \
   SiHistSetRowCmd.cc \
   SiHistSetBlendCmd.cc \
   SiHistSetStackCmd.cc \
   SiHistSetVerGraphCmd.cc \
   SiHistShowStatCmd.cc \
   SiHistSetLogScaleCmd.cc \
   SiHistSpikeCmd.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
