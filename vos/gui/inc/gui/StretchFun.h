///////////////////////////////////////////////////////////////////
// StretchFun.h: This is a collection of functions that do all types
// of stretches supported by the stretch tool.
///////////////////////////////////////////////////////////////////
#ifndef STRETCHFUN_H
#define STRETCHFUN_H
#include <Xm/Xm.h>		// only for Boolean types

class Lut;
class SiHistogram;

// Non-histogram Stretches

void stretch_linear ( Lut *lut, double dnmin, double dnmax );
void stretch_alarm ( Lut *lut, int *alarmValues, int nVals, int dnValue );
void stretch_comp ( Lut *lut );
void stretch_off ( Lut *lut );
void stretch_clip ( Lut *lut, int nbits );
void stretch_log ( Lut *lut, double curve, double dnmin, double dnmax );
void stretch_contour ( Lut *lut, int start, int end, int interval, int maxnum, 
		       int dnValue, Boolean stretched=False );
void stretch_table ( Lut *lut, int *inTable, int *outTable,
		     int nVals);
void stretch_itable ( Lut *lut, int *inTable, int *outTable,
		      int nVals, int background = False );
void stretch_function ( Lut *lut, char* v);
void stretch_period ( Lut *lut, double mean, double ampl, 
		      double freq, double phi );

// Histogram Stretches

void stretch_percent ( Lut *lut, SiHistogram *hist,
		       double lPerc, double hPerc,
		       int lExclude, int hExclude,
		       double &low, double &high );
void stretch_gauss ( Lut *lut, SiHistogram *hist,
                     double gsigma, double mean,
                     int low, int high );
void stretch_smooth ( Lut *lut, SiHistogram *hist,
		      int low, int high );
void stretch_ellipse ( Lut *lut, SiHistogram *hist,
		       int low, int high );
void stretch_power( Lut *lut, SiHistogram *hist,
		    int low, int high );
void stretch_peak ( Lut *lut, SiHistogram *hist,
		    int dnmin, int dnmax );
void stretch_mean ( Lut *lut, SiHistogram *hist,
		    int low, int high );

#endif
