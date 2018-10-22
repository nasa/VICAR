///////////////////////////////////////////////////////
// PseudoValue.cc: Contains three pseudocolor tables
////////////////////////////////////////////////////////
#include "PseudoValue.h"
#include "PseudoDefs.h"
#include "BasicWedgeOverlay.h"
#include "PseudoMarks.h"
#include "ViewMacros.h"
#include "ibisfile.h"
#include "file_no_path.h"
#include "zvproto.h"
#include <iostream>
using namespace std;
#include <stdio.h>
#include <ctype.h>

PseudoValue::PseudoValue ( char *filename)
{
        _numViews = 0;
        _views = NULL;
	_pseudoR = _defR = new int [256];
	_pseudoG = _defG = new int [256];
	_pseudoB = _defB = new int [256];

	if (!filename) return;

	if (!loadFile(filename)) {
		for (int i = 0; i < 256; i++) {
			_pseudoR[i] = _defR[i] = i;
			_pseudoG[i] = _defG[i] = i;
			_pseudoB[i] = _defB[i] = i;
		}
	}
}

PseudoValue::~PseudoValue()
{
	delete [] _pseudoR;
	delete [] _pseudoG;
	delete [] _pseudoB;
	delete [] _defR;
	delete [] _defG;
	delete [] _defB;
}

void PseudoValue::attachView (BasicWedgeOverlay *view)
{
        AttachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
        view->update ( this );
}

void PseudoValue::detachView (BasicWedgeOverlay *view)
{
        DetachViewMacro(BasicWedgeOverlay, _views, _numViews, view);
}

void PseudoValue::updateViews( )
{
	for (int i=0; i<_numViews; i++)
	    _views[i]->update ( this );
}

PseudoValue::PseudoValue(PseudoValue &val)	// Copy constructor
{
	memcpy((void *)this, (void *)&val, sizeof(PseudoValue));
	   
	if (val._pseudoR) {
	   _pseudoR = new int[256];
	   for (int i=0; i<256; i++)
		_pseudoR[i] = val._pseudoR[i];
	}
	if (val._pseudoG) {
           _pseudoG = new int[256];
           for (int i=0; i<256; i++)
                _pseudoG[i] = val._pseudoG[i];
        }
	if (val._pseudoB) {
           _pseudoB = new int[256];
           for (int i=0; i<256; i++)
                _pseudoB[i] = val._pseudoB[i];
        }

        if (val._defR) {
           _defR = new int[256];
           for (int i=0; i<256; i++)
                _defR[i] = val._defR[i];
        }
        if (val._defG) {
           _defG = new int[256];
           for (int i=0; i<256; i++)
                _defG[i] = val._defG[i];
        }
        if (val._defB) {
           _defB = new int[256];
           for (int i=0; i<256; i++)
                _defB[i] = val._defB[i];
        }
}

PseudoValue &PseudoValue::operator=(PseudoValue &val)
{
	if (this == &val)
	   return *this;		// assignment to self

	if (_pseudoR)
	   delete []_pseudoR;
	if (_pseudoG)
           delete []_pseudoG;
	if (_pseudoB)
           delete []_pseudoB;

	memcpy((void *)this, (void *)&val, sizeof(PseudoValue));

        if (val._pseudoR) {
           _pseudoR = new int[256]; 
           for (int i=0; i<256; i++)
                _pseudoR[i] = val._pseudoR[i];
        }  
        if (val._pseudoG) { 
           _pseudoG = new int[256];
           for (int i=0; i<256; i++)
                _pseudoG[i] = val._pseudoG[i];
        }  
        if (val._pseudoB) { 
           _pseudoB = new int[256];
           for (int i=0; i<256; i++)
                _pseudoB[i] = val._pseudoB[i];
        }  

        if (val._defR) {
           _defR = new int[256];
           for (int i=0; i<256; i++)
                _defR[i] = val._defR[i];
        }
        if (val._defG) {
           _defG = new int[256];
           for (int i=0; i<256; i++)
                _defG[i] = val._defG[i];
        }
        if (val._defB) {
           _defB = new int[256];
           for (int i=0; i<256; i++)
                _defB[i] = val._defB[i];
        }

	return *this;
}

void PseudoValue::setRedAsArray ( int *array )
{
	int i;
	for (i=0; i<256; i++)
		_pseudoR[i] = array[i];
	updateViews();
}

void PseudoValue::setGrnAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _pseudoG[i] = array[i];
        updateViews();
}

void PseudoValue::setBluAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _pseudoB[i] = array[i];
        updateViews();
}

void PseudoValue::setDefRedAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defR[i] = array[i];
        updateViews();
}

void PseudoValue::setDefGrnAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defG[i] = array[i];
        updateViews();
}

void PseudoValue::setDefBluAsArray ( int *array )
{
        int i;
        for (i=0; i<256; i++)
                _defB[i] = array[i];
        updateViews();
}

void PseudoValue::setRedDn ( int index, int newDn )
{
        _pseudoR[index] = newDn;
        updateViews();
}

void PseudoValue::setGrnDn ( int index, int newDn )
{
        _pseudoG[index] = newDn;
        updateViews();
}

void PseudoValue::setBluDn ( int index, int newDn )
{
        _pseudoB[index] = newDn;
        updateViews();
}

void PseudoValue::setRGBDn ( int index, int red, int grn, int blu )
{
	_pseudoR[index] = red;
	_pseudoG[index] = grn;
	_pseudoB[index] = blu;
	updateViews();
}

int PseudoValue::getRGBDn ( int index, int *red, int *grn, int *blu)
{
	if ( (index < 0) || (index > 255) ) {
		*red = *grn = *blu = 0;
		return 0;
	}
	else {
		*red = _pseudoR[index];
		*grn = _pseudoG[index];
		*blu = _pseudoB[index];
		return 1;
	}
}

void PseudoValue::regenerate ( PseudoMarks* marks )
{
	// In order to regenerate values, we need at least two marks at 
	// 0 and 255 dn.
	if ( (marks->getNumMarks() < 2) || (marks->find(0) == 0) || 
		(marks->find(255) == 0) ) return;

	// The goal is to go from mark to mark (left to right) and set the 
	// pseudocolor values according to the mark's color value and interpolation
	int dn = 0;		// starting point
	int markNo;
	while (dn != 255) {	// ending point
		markNo = marks->find(dn);
		int nextDn = marks->findNextDn(dn);

		if ( marks->getInterpolation(markNo) == NONE) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
			_pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
			_pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			restoreDefault (marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == FLAT ) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
                        _pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
                        _pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			flat ( marks->getRed(markNo), marks->getGrn(markNo), 
				marks->getBlu(markNo), marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == LINEAR ) {
			_pseudoR[marks->getDn(markNo)] = marks->getRed(markNo);
                        _pseudoG[marks->getDn(markNo)] = marks->getGrn(markNo);
                        _pseudoB[marks->getDn(markNo)] = marks->getBlu(markNo);
			int nextMarkNo = marks->find(nextDn);
			linear (marks->getRed(markNo), marks->getRed(nextMarkNo),
				marks->getGrn(markNo), marks->getGrn(nextMarkNo), 
				marks->getBlu(markNo), marks->getBlu(nextMarkNo),
				marks->getDn(markNo), nextDn );
		}

		if ( marks->getInterpolation(markNo) == CUBS ) {
		// Not implemented yet
		}
		dn = nextDn;
	}
	// Process the last mark at 255
	markNo = marks->find(255);
	_pseudoR[255] = marks->getRed(markNo);
	_pseudoG[255] = marks->getGrn(markNo);
	_pseudoB[255] = marks->getBlu(markNo);

	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Restore default values between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::restoreDefault ( int start, int end )
{
	for (int i = start+1; i < end; i++) {
		_pseudoR[i] = _defR[i];
		_pseudoG[i] = _defG[i];
		_pseudoB[i] = _defB[i];
	}
	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Do flat interpolation between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::flat ( int valueR, int valueG, int valueB, int startDn, int endDn )
{
	for (int i = startDn+1; i < endDn; i++) {
		_pseudoR[i] = valueR;
		_pseudoG[i] = valueG;
		_pseudoB[i] = valueB;
	}
	updateViews();
}

///////////////////////////////////////////////////////////////////////////
// Do linear interpolation between two points
///////////////////////////////////////////////////////////////////////////
void PseudoValue::linear ( int startRedValue, int endRedValue, int startGrnValue, 
	int endGrnValue, int startBluValue, int endBluValue, int startDn, int endDn )
{
	for (int i = startDn+1; i < endDn; i++) {
              _pseudoR[i] = ( (i-startDn)*(endRedValue-startRedValue) +
                        startRedValue*(endDn-startDn) ) / (endDn-startDn);
              _pseudoG[i] = ( (i-startDn)*(endGrnValue-startGrnValue) +
                        startGrnValue*(endDn-startDn) ) / (endDn-startDn);
              _pseudoB[i] = ( (i-startDn)*(endBluValue-startBluValue) +
                        startBluValue*(endDn-startDn) ) / (endDn-startDn);
	}
	updateViews();
}

/////////////////////////////////////////////////////////////////////////////
// Load an IBIS file.  Return 1 on success.
////////////////////////////////////////////////////////////////////////////
int PseudoValue::loadFile(char *filename)
{
        const int SUCCESS = 1;
        const int FAILURE = 0;
	int unit, ibis, status, record, i;
	int lut[256][3];

	if (!filename || !strlen(filename)) return FAILURE;

	// open IBIS file for reading

	status = zvunit(&unit, (char *)"in_file",  1, "u_name", filename, NULL);

	status = IBISFileOpen(unit, &ibis, (char *)IMODE_READ, 0, 0, 0, 0);
	if(status!=1) return FAILURE;

	// Strip directory name from the filename
 
        char *stripFilename = strdup(filename);
        file_no_path(stripFilename);

	ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0, (char *)"PSEUDOCOLOR",
					stripFilename);
	ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
						(char *)"$MyLut");

	status = IBISRecordOpen(ibis, &record,
		(char *)"$MyRED | $MyGRN | $MyBLU", 0, 0, (char *)IFMT_FULL);
	if (status != 1) return FAILURE;

	for (i=1; i<=256; i++) {
		status = IBISRecordRead(record, (char*)lut[i-1], i);
		if (status != 1) return FAILURE;
	}

	IBISFileClose(ibis, 0);

	for (i=0; i<256; i++) {
		_pseudoR[i] = _defR[i] = lut[i][0];
		_pseudoG[i] = _defG[i] = lut[i][1];
		_pseudoB[i] = _defB[i] = lut[i][2];
	}

	updateViews();

	delete [] stripFilename;

        return SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
// Save pseudocolor tables in IBIS-formatted file
//////////////////////////////////////////////////////////////////////////
int PseudoValue::saveFile(char *filename)
{
	const int SUCCESS = 1;
        const int FAILURE = 0;
        int unit, i, ibis, status, record;
        int lutcols[3];
	int lut[256][3];

	if (!filename || !strlen(filename)) return FAILURE;

        for (i=0; i<3; i++) lutcols[i]=i+1;

	// Create ibis file

        status = zvunit(&unit, (char *)"out_file",  1, "u_name", filename,NULL);
        status = IBISFileOpen(unit, &ibis, (char *)IMODE_WRITE, 3,
                                        256, 0, (char *)IORG_COLUMN);
        if (status!=1) return FAILURE;

	// Strip directory name from the filename

	char *stripFilename = strdup(filename);
	file_no_path(stripFilename);

        status = IBISFileSet(ibis, (char *)IFILE_TYPE,
					(char *)"LOOKUP_TABLE", 0);
        if (status!=1) return FAILURE;

        status = ICLNewRGB(ibis, 1, 2, 3, 0);
        if (status < 0) return FAILURE;

        status = ICLNewLOOKUP_TABLE(ibis, lutcols, 3, 0, 0, 
				    (char *)"PSEUDOCOLOR", stripFilename);
        if (status < 0) return FAILURE;

	ICLGetLOOKUP_TABLE(ibis, (char *)"$MyLut", 0,
					(char *)"PSEUDOCOLOR", stripFilename);
        ICLGetRGB(ibis, (char *)"$MyRED", (char *)"$MyGRN", (char *)"$MyBLU",
						(char *)"$MyLut");

        status = IBISRecordOpen(ibis, &record,
		(char *)"$MyRED | $MyGRN | $MyBLU", 0, 0, (char *)IFMT_FULL);
        if (status != 1) return FAILURE;

	for (i=0; i<256; i++) {
		lut[i][0]=_pseudoR[i];
		lut[i][1]=_pseudoG[i];
		lut[i][2]=_pseudoB[i];
        }

        for (i=1; i<=256; i++) {
                status = IBISRecordWrite(record, (char*)lut[i-1], i);
                if (status != 1) return FAILURE;
        }

        // close up shop
        status = IBISFileClose( ibis, 0 );
        if (status != 1) return FAILURE;

	delete [] stripFilename;

	return SUCCESS;
}
