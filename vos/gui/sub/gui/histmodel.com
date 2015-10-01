$!****************************************************************************
$!
$! Build proc for MIPL module histmodel
$! VPACK Version 1.8, Wednesday, October 16, 1996, 16:18:20
$!
$! Execute by entering:		$ @histmodel
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
$ write sys$output "*** module histmodel ***"
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
$ write sys$output "Invalid argument given to histmodel.com file -- ", primary
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
$   if F$SEARCH("histmodel.imake") .nes. ""
$   then
$      vimake histmodel
$      purge histmodel.bld
$   else
$      if F$SEARCH("histmodel.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histmodel
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histmodel.bld "STD"
$   else
$      @histmodel.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histmodel.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histmodel.com -mixed -
	-s Histogram.cc -
	-i histmodel.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create Histogram.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// Histogram.cc
////////////////////////////////////////////////////////////////
#include "Histogram.h"
#include "HistView.h"
#include "ViewMacros.h"
#include <assert.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include <math.h>

////////////////////////////////////////////////////////////////
// Constructors/Destructors
////////////////////////////////////////////////////////////////
Histogram::Histogram(double ll, double ul, double bs)
{
	_isIntRange = False;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul);
}

Histogram::Histogram(int ll, int ul, double bs)
{
	_isIntRange = True;
	_useBinSize = True;
	_binSize = bs;
	ctor(ll, ul + 1);
}

Histogram::Histogram()
{
	_isIntRange = True;
	_useBinSize = False;
	_numBins = 256;
	ctor(0, 256);
}

void Histogram::ctor(double ll, double ul)
{
	_lowerLimit = ll;
	_upperLimit = ul;

	if ( _upperLimit < _lowerLimit ) {
		cerr << "numBins: Upper limit can't be less than lower limit!";
		_lowerLimit = 0; _upperLimit = 256;
	}

	_numViews = 0;

	_views = NULL;
	_bin = NULL;

	allocate ();

	clear();
}

Histogram::~Histogram()
{
	if (_views) delete _views;
	delete [] _bin;
}

////////////////////////////////////////////////////////////////
// Views
////////////////////////////////////////////////////////////////
void Histogram::attachView (HistView *view)
{
	AttachViewMacro(HistView, _views, _numViews, view);
	view->update ( );
}

void Histogram::detachView (HistView *view)
{
	DetachViewMacro(HistView, _views, _numViews, view);
}

void Histogram::updateViews()
{
	int i;
	for (i=0; i<_numViews; i++)
		_views[i]->update ( );
}

////////////////////////////////////////////////////////////////
// Misc. manipulation
////////////////////////////////////////////////////////////////

void Histogram::allocate()
{
	if (_useBinSize)	// Calc numBins based on bin size
		_numBins = int((_upperLimit-_lowerLimit) / _binSize + 0.5);
	else			// Calc _binSize based on number of bins
		_binSize = (_upperLimit - _lowerLimit) / _numBins;

	if (_bin)
		delete [] _bin;
	_bin = new int[_numBins];
	if (!_bin) 
		cerr << "Allocation error\n";
}

void Histogram::clear()
{
	clear_noupdate();
	updateViews();
}

void Histogram::clear_noupdate()
{
        for  ( int ix=0; ix<_numBins; ix++ )
                _bin[ix]=0;
}

////////////////////////////////////////////////////////////////
// Bin value operations
////////////////////////////////////////////////////////////////

int Histogram::operator [] ( int b )
{
	if (b < 0)
		b = 0;
	if (b >= _numBins)
		b = _numBins-1;
	return _bin[b];
}

////////////////////////////////////////////////////////////////
// Functions that manipulate the histogram range (min/max/#bins/bin size)
////////////////////////////////////////////////////////////////
void Histogram::setBinSize (double bs)
{
        _binSize = bs;
	_useBinSize = True;
	allocate();
}

void Histogram::setNumBins(int num)
{
	_numBins = num;
	_useBinSize = False;
	allocate();
}

void Histogram::setLowerLimit (double limit)
{
	_lowerLimit = limit;
	allocate();
}

void Histogram::setUpperLimit (double limit)
{
        _upperLimit = limit;
	_isIntRange = False;
	allocate();
}

void Histogram::setUpperLimit (int limit)
{
        _upperLimit = limit + 1;
	_isIntRange = True;
	allocate();
}

void Histogram::setLimits (double minval, double maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	allocate();
}

void Histogram::setLimits (double minval, double maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void Histogram::setLimits (double minval, double maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval;
	_isIntRange = False;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

void Histogram::setLimits (int minval, int maxval)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	allocate();
}

void Histogram::setLimits (int minval, int maxval, double binSize)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_binSize = binSize;
	_useBinSize = True;
	allocate();
}

void Histogram::setLimits (int minval, int maxval, int numBins)
{
        _lowerLimit = minval;
        _upperLimit = maxval + 1;
	_isIntRange = True;
	_numBins = numBins;
	_useBinSize = False;
	allocate();
}

////////////////////////////////////////////////////////////////
// Functions on statistics calculation
////////////////////////////////////////////////////////////////

double Histogram::getMean()
{
	double rsum0, rsum1;
	rsum0 = 0;
	rsum1 = 0;
	double mean;

	int i;
	for (i=0; i<_numBins; i++)
	{
		rsum0 += (double)_bin[i];
		rsum1 += (double)_bin[i] * i;
	}
	if (rsum0 == 0.0)
		mean = 0.0;
	else
	{
		mean = rsum1 / rsum0;
	}

	return mean;
}

double Histogram::getStDev()
{
	double rsum0, rsum2;
	rsum0 = 0;
	rsum2 = 0;
	double stdev;
	double variance;

	for (int i=0; i<_numBins; i++)
	{
		rsum0 += (double)_bin[i];
		rsum2 += (double)_bin[i] * i * i;
	}
	if (rsum0 == 0.0)
		stdev = 0.0;
	else 
	{
		variance = (rsum2 / rsum0) - (getMean() * getMean());
		if (variance < 0.0)
			variance = 0.0;
		stdev = sqrt(variance);
	}
	return stdev;
}

int Histogram::getMaxValue()
{
	int maxv = 0;
	for (int i=0; i<_numBins; i++)
		maxv = max ( maxv, _bin[i]);

	return maxv;
}

////////////////////////////////////////////////////////////////
// Spike
////////////////////////////////////////////////////////////////
int Histogram::spike ( int n )
{
	int oldmax = -1;
	int maxval;
	int i, j;

	for (i=0; i<n; i++)               // de-spike the histogram
	{
		maxval = -1;
		for (j=0; j<_numBins; j++)
		{
			if ((_bin[j] > maxval) && (oldmax == -1 || _bin[j] < oldmax))
			maxval = _bin[j];
		}
		if (maxval >= 0)
			oldmax = maxval;
	}

	return oldmax;
}

////////////////////////////////////////////////////////////////
// Assignment
////////////////////////////////////////////////////////////////
Histogram &Histogram::operator=(Histogram &hist)
{
	if (this == &hist)
		return *this;		// assignment to self

	_binSize = hist._binSize;
	_lowerLimit = hist._lowerLimit;
	_upperLimit = hist._upperLimit;
	_numBins = hist._numBins;
	_useBinSize = hist._useBinSize;
	_isIntRange = hist._isIntRange;

	allocate();

	for (int i=0; i<_numBins; i++)
		_bin[i] = hist._bin[i];

	_numViews = 0;			// Don't transfer views
	_views = NULL;

	return *this;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histmodel.imake
#define SUBROUTINE histmodel
#define MODULE_LIST Histogram.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIFAPP
#define LIB_MOTIF

$ Return
$!#############################################################################
