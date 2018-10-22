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

