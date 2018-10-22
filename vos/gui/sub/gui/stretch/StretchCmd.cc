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

