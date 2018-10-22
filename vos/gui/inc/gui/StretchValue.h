///////////////////////////////////////////////////////////////////
// StretchValue.h: Contains type of stretch and all parameters needed
// by the function to execute the stretch.  The object of this class
// is passed to the command as a value.
///////////////////////////////////////////////////////////////////
#ifndef STRETCHVALUE_H
#define STRETCHVALUE_H
#include <Xm/Xm.h>

enum StretchType { RAW, LINEAR, CLIP, 
		   CONTOUR, ITABLE, 
		   PSTRETCH, FUNCTION, LOGARITHM, 
		   SMOOTH, GAUSS, ELLIPSE, POWER, 
		   PEAK, MEAN, ASTRETCH, 
		   TABLE, ALARM, COMP, OFF };

enum StretchBand { STR_ALL, STR_RED, STR_GREEN, STR_BLUE };

#define HSIZE	256

class SiHistogram;

struct StretchValue {

    StretchType stretchName;

    StretchBand band;
    Boolean     changeOnlyBand;
    
    double	low, high;
    double 	dnmin, dnmax;
    int 	dnValue;
    int 	nbits;
    double 	curve;
    int 	interval, maxnum; 
    double      lPerc, hPerc;
    Boolean     stretched;
    
    int 	*inTable, *outTable, tableNoVals;
    int	        *inITable, *outITable, itableNoVals;
    int         *alarmValues, alarmNoVals;
    
    int 	backgnd;
    char 	*func;
    double 	mean, pmean, ampl, freq, phi;
    
    int         range;		// used in 
    double      factor, percent; // mean & peak
    
    double      lpercent, hpercent; // used in astretch
    
    double      gsigma, gmean;
    
    // Post stretches
    Boolean table, alarm, comple, off;
    
    StretchValue(StretchBand = STR_ALL);
    StretchValue(StretchValue &);		// copy ctor
    ~StretchValue ();
    
    StretchValue &operator=(StretchValue &val);
    Boolean operator==(StretchValue &val);

    double lPercValueRed, hPercValueRed;
    double lPercValueGrn, hPercValueGrn;
    double lPercValueBlu, hPercValueBlu;

};
#endif
