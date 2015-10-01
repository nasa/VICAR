$!****************************************************************************
$!
$! Build proc for MIPL module lutmodel
$! VPACK Version 1.8, Wednesday, October 16, 1996, 16:18:23
$!
$! Execute by entering:		$ @lutmodel
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
$ write sys$output "*** module lutmodel ***"
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
$ write sys$output "Invalid argument given to lutmodel.com file -- ", primary
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
$   if F$SEARCH("lutmodel.imake") .nes. ""
$   then
$      vimake lutmodel
$      purge lutmodel.bld
$   else
$      if F$SEARCH("lutmodel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lutmodel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lutmodel.bld "STD"
$   else
$      @lutmodel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lutmodel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lutmodel.com -mixed -
	-s Lut.cc -
	-i lutmodel.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create Lut.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////
// Lut.cc: model class for LUT
///////////////////////////////////////////////////
#include "Lut.h"
#include "ViewMacros.h"
#include "LutView.h"
#include <iostream>
using namespace std;

Lut::Lut ( )
{
        _numViews = 0;
        _views = NULL;
        _lut = NULL;

	_lowerLimit = 0;
	_upperLimit = 255;
	ramp ( );
}

Lut::~Lut ( )
{
	if (_views) delete _views;
        delete [] _lut;
}

void Lut::attachView (LutView *view)
{
        AttachViewMacro(LutView, _views, _numViews, view);
        view->update ( );
}

void Lut::detachView (LutView *view)
{
        DetachViewMacro(LutView, _views, _numViews, view);
}

void Lut::updateViews( )
{
      int i;
      for (i=0; i<_numViews; i++)
              _views[i]->update ( );
}

int Lut::operator [] ( int i )
{
        return _lut[i];
}

void Lut::setAsArray (int *array)
{
	if (!_lut) _lut = new int [256];
	if (!_lut) cerr << "Memory allocation error\n";
	for (int i = 0; i < 256; i++)
		_lut[i] = array[i];

	updateViews();
}

void Lut::ramp ( )
{
	if (!_lut) _lut = new int [256];
	if (!_lut) cerr << "Allocation error\n";
	for (int i = 0; i < 256; i++)
		_lut[i] = i;

	updateViews();
}

void Lut::setDN ( int index, int newDN )
{
	_lut[index] = newDN;

	updateViews();
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lutmodel.imake
#define SUBROUTINE lutmodel
#define MODULE_LIST Lut.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIFAPP
#define LIB_MOTIF

$ Return
$!#############################################################################
