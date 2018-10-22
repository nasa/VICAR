///////////////////////////////////////////////////////////////////
// StretchFun.cc: This is a collection of functions that do all types
// of stretches supported by the stretch tool.  Basically this functions
// take LUT and perform some mathematical operations on it.  
// These functions change point by point intensity of an image
// by generating a transfer function on the domain of intensity values.
///////////////////////////////////////////////////////////////////
#include "StretchFun.h"
#include "SiHistogram.h"
#include "Lut.h"
#include "Function.h"
#include <math.h>
#include <stdlib.h>
#include <iostream>
using namespace std;

#ifndef MIN
#define MIN(x,y)        (((x) < (y)) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y)        (((x) > (y)) ? (x) : (y))
#endif

///////////////////////////////////////////////////////////////////
// Linear stretch function does a linear interpolation between 
// the min and max values
///////////////////////////////////////////////////////////////////
void stretch_linear ( Lut *lut, double dnmin, double dnmax )
{
    int i;

    int *l = lut->getAsArray();
    if (!l) { 
	cerr << "Memory allocation error in stretch_linear\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit());
    double range = dnmax - dnmin;
    if (range == 0.0)		// avoid divide by 0
	range = 0.00001;		// (arbitrary number)
    for ( i = 0; i <= size; i++ ) {
	l[i] = int (MAX(lut->getLowerLimit(),
			MIN(lut->getUpperLimit(),
			    lut->getLowerLimit() +
			    (double)(i - dnmin) * (double)size / range)));
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_alarm sets a series of values in the lookup table
// to a user specified value.
///////////////////////////////////////////////////////////////////
void stretch_alarm ( Lut *lut, int *alarmValues, int nVals, int dnValue )
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_alarm\n"; 
	return; 
    }

    for ( int j = 0; j < nVals; j++ )
	l[alarmValues[j]] = dnValue;
    
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_comp inverts the named lookup tables.
///////////////////////////////////////////////////////////////////
void stretch_comp ( Lut *lut )
{
        int *l = lut->getAsArray();
	if (l == NULL) { 
	    cerr << "Memory allocation error in stretch_comp\n"; 
	    return; 
	}

	int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
        for ( int j = 0; j < size; j++ )
                l[j] = lut->getUpperLimit() - l[j];

	lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_off turns off one plane by setting every member of 
// lut to minimum value.
///////////////////////////////////////////////////////////////////
void stretch_off ( Lut *lut )
{
    stretch_contour(lut, (int)lut->getLowerLimit(),
		    (int)lut->getUpperLimit(),
		    1, (int)lut->getUpperLimit(),
		    (int)lut->getLowerLimit(), False );
}

///////////////////////////////////////////////////////////////////
// stretch_clip performes bit-clipping on LUT. If n > 0, the binary 
// representation of the intensity level is shifted n bits to the left 
// and the n most significant bits are truncated.  If n < 0, the binary 
// representation of the intensity level is shifted n bits to the right 
// and the n least significant bits are truncated.
///////////////////////////////////////////////////////////////////
void stretch_clip ( Lut *lut, int nbits )
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_clip\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);

    int j;
    if (nbits > 0)
	for (j = 0; j < size; j++)
	    l[j] = (j << nbits) % size;
    else
	for (j = 0; j < 256; j++)
	    l[j] = (j >> abs(nbits)) % size;
    
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_log performs logarithmic transformation on LUT
///////////////////////////////////////////////////////////////////
void stretch_log ( Lut *lut, double curve, double dnmin, double dnmax )
{
    if (curve <= 0) {
	cerr << "stretch_log: CURVE must be greater than zero, ";
	cerr << "setting to 1e-30 \n";
	curve = 1e-30;
    }

    if (dnmin == dnmax) {
	cerr << "stretch_log: HIGH and LOW parameters ";
	cerr << "may not be equal.\n";
	return;
    }
    if ((dnmin + curve) <= 0.0) {
	cerr << "stretch_log: LOW + CURVE must be greater than zero.\n";
	return;
    }
    if ((dnmax + curve) <= 0.0) {
	cerr << "stretch_log: HIGH + CURVE must be greater than zero.\n";
	return;
    }

    double a;
    a = lut->getUpperLimit() / (log(dnmax + curve) - log(dnmin + curve));
    
    double b = -a * log(dnmin + curve) + 0.5;

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_log: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);

    for ( int j = 0; j < size; j++ ) {
	l[j] = (int) (a * log( (double)j + curve ) + b);
	if (l[j] < lut->getLowerLimit()) 
	    l[j] = lut->getLowerLimit();
	if (l[j] >lut->getUpperLimit()) 
	    l[j] = lut->getUpperLimit();
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_contour provides DN contours on the given intervals.  Input 
// intensities which are a multiple of n are set to the value specified 
// by the dnValue parameter.
///////////////////////////////////////////////////////////////////
void stretch_contour ( Lut *lut, int start, int end, 
		       int interval, int maxnum, 
		       int dnValue, Boolean stretched )
{
    int j, k;
    stretch_linear (lut, lut->getLowerLimit(), lut->getUpperLimit());

    int *l = lut->getAsArray();
    if (l == NULL) { cerr << "Fail\n"; return; }

    int current = stretched ? l[0] : 0;
    if (interval == 0) return;
    if (((current - start) % interval) == 0) {
	l[0] = dnValue;
	k = 1;
    }
    int last = (current - start) / interval;
    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (j = 1; j < size; j++) {
	if (k > maxnum) break;
	current = stretched ? l[j] : j;
	if (current < start) continue;
	if (current > end) continue;
	current = (current - start) / interval;
	if (current != last) {
	    l[j] = dnValue;
	    k++;
	}
	last = current;
    }
    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_table: will set the input DN values in1,in2,... to the DN values
// out1,out2,... and will linearly interpolate between these points to 
// compute the intervening DN values.  All DN values outside the range in1 
// to inN will remain unchanged unless the parameter BACKGND is also 
// specified.  In that case, all DN values outside the range in1 to inN will 
// be set to the BACKGND value. The maximum number of pairs which may be 
// specified is 100.  The values in1,in2,...,inN must be specified in 
// increasing order.  
///////////////////////////////////////////////////////////////////
void stretch_table ( Lut *lut, int *inTable, int *outTable,
		     int nVals)
{
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "Memory allocation error in stretch_table\n"; 
	return; 
    }

    if (nVals > 0)
	l[inTable[0]] = outTable[0];

    double m, b;            // slope, offset
    int j;
    for (j = 1; j < nVals; j++) {
	m = (double) (outTable[j] - outTable[j - 1]) /
	    (double) (inTable[j] - inTable[j - 1]);
	b = (double)outTable[j] - (m * (double)inTable[j]) + 0.5;
	int k;
	for (k = inTable[j - 1]; k <= inTable[j]; k++)
	    l[k] = (int) (m * (double)k + b);
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_itable: will set the input DN values in1,in2,... to the DN values
// out1,out2,... but will not do any interpolation.  All DN values not 
// explicitly specified, including those outside the range in1 to inN will 
// remain unchanged unless the BACKGND parameter is also specified.  In that 
// case, all DN values not explicitly specified, including those outside the
// range in1 to inN, will be set to the BACKGND value.  The maximum number of 
// pairs which may be specified is 100.
///////////////////////////////////////////////////////////////////
void stretch_itable ( Lut *lut, int *inTable, int *outTable,
		      int nVals, int background )	// background = False
{
    stretch_linear (lut, lut->getLowerLimit(), lut->getUpperLimit());

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_itable: Memory allocation error\n"; 
	return; 
    }

    int i, j;
    for (j = 0; j < nVals; j++) {
	l[inTable[j]] = outTable[j];
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    Boolean contain;
    if (background) {
	for (i = 0; i < size; i++) {
	    contain = False;
	    for (j = 0; j < nVals; j++)
		if ( i == inTable[j] )
		    contain = True;
	    if (!contain)
		l[i] = background;
	}
    }

    lut->setAsArray(l);
}

///////////////////////////////////////////////////////////////////
// stretch_func applies a user-specified mathematical function to
// the lookup tables.
///////////////////////////////////////////////////////////////////
void stretch_function ( Lut *lut, char* v)
{
    FunctionDef theFunc;

    int status = ParseFunction(v, &theFunc);
    if (status == 0) return; 

    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_function: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (int j = 0; j < size; j++) {
	l[j] = (int) (ApplyFunction(&theFunc, (float)j) + 0.5);
	if (l[j] < int(lut->getLowerLimit())) 
	    l[j] = int(lut->getLowerLimit());
	if (l[j] > int(lut->getUpperLimit())) 
	    l[j] = int(lut->getUpperLimit());
    }

    lut->setAsArray(l);
}


///////////////////////////////////////////////////////////////////
// stretch_period specifies that a periodic stretch is to be performed.  
// The transfer function is: 
//   DN(out)=AMPL/2*SIN[2*PI*FREQ*DN(in)/(DNMAX-DNMIN)+PHI]+DC
//   where PI = 3.14159
//   and AMPL,FREQ,PHI,DC are parameters which may be specified separately
//   by the user and which have the following defaults: AMPL= DNMAX-DNMIN
//   FREQ= 1.0 
//   PHI = 0.0
//   DC  = (DNMAX+DNMIN)/2
//   Note that AMPL is 2 times what would be called the amplitude in academia.
///////////////////////////////////////////////////////////////////
#define PI 3.1415927
void stretch_period ( Lut *lut, double mean, double ampl, 
				double freq, double phi )
{
    freq = 2.0 * freq * PI / lut->getUpperLimit();
    ampl = ampl / 2.0;
    mean = mean + 0.5;
    int *l = lut->getAsArray();
    if (l == NULL) { 
	cerr << "stretch_period: Memory allocation error\n"; 
	return; 
    }

    int size = int (lut->getUpperLimit() - lut->getLowerLimit() + 1);
    for (int j = 0; j < size; j++) {
	l[j] = (int) (ampl * sin(freq * j + phi) + mean);
	if (l[j] < int(lut->getLowerLimit())) 
	    l[j] = int(lut->getLowerLimit());
	if (l[j] > int(lut->getUpperLimit())) 
	    l[j] = int(lut->getUpperLimit());
    }

    lut->setAsArray(l);
}

//////////////////////////////////////////////////////////////////////////
//	HISTOGRAM STRETCHES
//////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////
// stretch_percent performs an automatic linear stretch.  It specifies the 
// total percentage of the input image to be saturated.  The result is 
// equivalent to specifying the parameters LPERCENT and HPERCENT where
// LPERCENT = PERCENT/2 and HPERCENT = PERCENT/2.  This percentage applies 
// to the input image DN distribution after exclusions have been performed.
// PERCENT must be in the range 0.0 to 100.0 (Default is PERCENT=12.0)
///////////////////////////////////////////////////////////////////
void stretch_percent ( Lut *lut, SiHistogram *hist, 
		       double lPerc, double hPerc,
		       int lExclude, int hExclude, 
		       double &low, double &high )
{
    int j, k;

    if (hist == NULL) { 
	cerr << "stretch_percent: Histogram cannot be NULL!\n"; 
	return; 
    }

    if (lExclude < 0)
	lExclude = 0;
    if (hExclude > hist->numBins())
	hExclude = hist->numBins();
    if (lExclude > hExclude)
	hExclude = lExclude;

    lPerc /= 100.0;
    hPerc /= 100.0;
    
    SiHistogram     *locHist;
    locHist = new SiHistogram();
    *locHist = *hist;

    double total = 0.0;
    for (j = lExclude; j <= hExclude; j++)
	total += (double) (*locHist)[j];
    
    low = lExclude;
    high = hExclude;

    if (total != 0.0) {
	double lTotal, hTotal;
	lTotal = hTotal = 0.0;
	
	for (j = lExclude, k = hExclude; j <= hExclude; j++, k--) {
	    lTotal += (double) (*locHist)[j];
	    hTotal += (double) (*locHist)[k];
	    if ((lTotal / total) <= lPerc) low  = (double)j;
	    if ((hTotal / total) <= hPerc) high = (double)k;
	}
    }
    if (high == low) {
	cerr << "stretch_percent: Invalid stretch was calculated, ";
	cerr << "none applied.\n";
    }
    else
      stretch_linear (lut, low, high );

    delete locHist;
}

///////////////////////////////////////////////////////////////////
// HistToCurve will create a lookup table which gives a good match
// of the histogram given by hist to the curve described by the array
// curve.
///////////////////////////////////////////////////////////////////
static void HistToCurve(Lut *lut, SiHistogram *hist, 
			double *curve, 
			int low, int high)
{
    const int HSIZE = hist->numBins();
    const int LUTSIZE = lut->getUpperLimit()-lut->getLowerLimit()+1;

    int           i,cdfPos;                /* increment variables          */
    double        cumSum,cumSumNext,scale; /* Cumulative sum, scaling factor*/
    SiHistogram	  *locHist;		   /* local copy of histogram      */
    double *cdf = new double [HSIZE];      /* cumulative distribution func.*/
    int		  lutPos, lastLutPos;

    if (hist==NULL) { 
	cerr << "HistToCurve: Histogram cannot be NULL!\n"; 
	delete []cdf;
	return; 
    }
    locHist = new SiHistogram();
    *locHist = *hist;
    if (locHist == NULL) cerr << "Fail\n";
    for (i=0; i< HSIZE; i++)
	cdf[i] = curve[i];

    for (i = 0; i < low; i++) 		/* Zero out values  */
	locHist->setBin (i,0);		/* to be excluded   */

    for (i = high+1; i < HSIZE; i++)	/* Zero out values  */
	locHist->setBin(i,0);		/* to be excluded   */

    int *l = lut->getAsArray();
    if (l == NULL) 
	cerr << "HistToCurve: Memory allocation error\n";

/* Now make the cdf table contain an actual cumulative distribution
 * function (cdf); ie, each value is the sum of the values below it.
 * scale is modified to hold a scaling factor to adjust the gaussian
 * to the actual number of pixels we have, and each value in cdf
 * is scaled and then added to the point below it, to give
 * an actual cumulative distribution function.
 */
    cumSum = 0.0;                 /* Total area under the curve to be fit */
    for (i = 0; i < HSIZE; i++)
	cumSum += cdf[i];
    scale = 0;
    for (i = low; i <= high; i++)         /* scale is the total number of */
	scale += (double) (*locHist)[i];    /* pixels in our histogram      */
    scale = scale / cumSum;               /* divided by the total in gauss*/
    cdf[0] *= scale;
    for (i = 1; i < HSIZE; i++)
	cdf[i] = cdf[i] * scale + cdf[i - 1];
    
/* Now generate the lookup table based on the shape of the
 * cumulative distribution function.  Each output DN is that DN (location)
 * in the cdf[] array that has the accumulation (value) that is
 * closest to the accumulation (sum of values) of the input DN in
 * our histogram.
 * If the LUT and histogram bins aren't the same size, compensate by
 * overwriting the LUT value multiple times (if the LUT is smaller), or
 * by taking the current value and filling in the gap.
 */
    cumSum=0.0;
    i = cdfPos = 0;
    cumSumNext = (*locHist)[0];
    lutPos = lastLutPos = 0;
    while ((i < HSIZE) && (cdfPos < HSIZE)) {
	while (fabs(cdf[cdfPos] - cumSum) >= fabs(cdf[cdfPos] - cumSumNext)) {

            // cumulative sum not yet big enough, so store the cdf position
	    // and try the next spot in the histogram.

	    lutPos = (int)((((double)i) / HSIZE) * LUTSIZE + lut->getLowerLimit());
	    l[lutPos] = (int)((((double)cdfPos) / HSIZE) * LUTSIZE +
			      lut->getLowerLimit());
	    i++;
	    if (lutPos - lastLutPos > 1) {	/* Fill in the gap, if any */
		for (int j=lastLutPos+1; j<lutPos; j++) l[j] = l[lutPos];
	    }
	    lastLutPos = lutPos;
	    if (i >= HSIZE) break;
	    cumSum = cumSumNext;
	    cumSumNext = cumSum + (*locHist)[i];
	}
	cdfPos++;                   /* match next spot in cdf       */
    }
    for (lutPos++; lutPos <= lut->getUpperLimit(); lutPos++)
	l[lutPos] = lut->getUpperLimit();   /* saturate out rest of lut     */

    lut->setAsArray(l);

    delete cdf;
    delete locHist;
}


/************************************************************************/
/* GaussLut will force the given histogram into roughly a gaussian
 * curve with the given mean and sigma by tweaking the given lookup
 * table.  The histogram is ignored below "low" and above "high".
 * "This arrangement is by D. Stanfill, from an original composition
 * by J. Addington entitled 'RDISPLAY' (in D minor)".
 */
static void GaussLut ( Lut *lut, SiHistogram *hist,
		       double gsigma, double mean,
		       int  low, int high )
{
    const int HSIZE = hist->numBins();
    const double MID = hist->numBins() / 2.0;

    int           i;                      /* increment variable           */
    double        sig2;                   /* sigma squared                */
    double *cdf = new double[HSIZE];

    if (gsigma == 0.0) 
	cerr << "GaussLut: gsigma cannot be 0\n";
    sig2 = MID / gsigma;                          /* sigma                */
    sig2 = 2.0 * sig2 * sig2;                     /* 2 * sigma^2          */

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a gaussian histogram.  Each value
 * in cdf[] contains the gaussian formula evaluated at that point.
 * ( y = e^(-(x-mean)^2 / (2*sigma^2))  )
 */
    for (i = 0; i < HSIZE; i++)
	cdf[i] = exp ( -(i - mean) * (i - mean) / sig2 );

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}

/************************************************************************/
/* EllipseLut will force the given histogram into roughly an ellipse
 */
static void EllipseLut ( Lut *lut, SiHistogram *hist,
			 int  low, int high )
{
    const int HSIZE = hist->numBins();
    
    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];
    double        hilowDN = high - low;

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * ( y = 1 - abs(x)^power on interval (-1, 1)  )
 */

    for (i = 0; i < HSIZE; i++)
	cdf[i] = sqrt(1 - (2.0 * (double)i / hilowDN - 1) 
		      * (2.0 * (double)i / hilowDN - 1) );

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}

/************************************************************************/
/* PowerLut
 */
static void PowerLut ( Lut *lut, SiHistogram *hist,
		       int  low, int high )
{
    const int HSIZE = hist->numBins();
    
    int           i;                     /* increment variable           */
    double *cdf = new double [HSIZE];
    double	power = 2.0;
    double	hilowDN = high - low;

/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * ( y = 1 - abs(x)^power on interval (-1, 1)  )
 */

    for (i = 0; i < HSIZE; i++)
	cdf[i] = 1 - pow( fabs(2.0 * (double)i / hilowDN - 1), power );

    HistToCurve(lut, hist, cdf, low, high);

    delete cdf;
}

#if 0		/* doesn't work; see comment later */
/************************************************************************/
/* PeakLut 
 */
static void PeakLut ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    const int HSIZE = hist->numBins();

    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];


/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * y = 
 */
    for (i = 0; i < HSIZE; i++);
	//  cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
	//!!!! Not implemented !!

    HistToCurve(lut, hist, cdf, low, high);
    
    delete cdf;
}
#endif

/************************************************************************/
/* MeanLut 
 */
static void MeanLut ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    const int HSIZE = hist->numBins();

    int           i;                      /* increment variable           */
    double *cdf = new double [HSIZE];


/* Now fill up the cumulative distribution function (cdf) table with the
 * proper values so that we have a peak histogram.  Each value
 * in cdf[] contains the peak formula evaluated at that point.
 * y =
 */
    //for (i = 0; i < HSIZE; i++)
    //  cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
    //!!!! Not implemented !!

    //  HistToCurve(lut, hist, cdf, low, high);


    // values assigned to 0 to elim. compiler warnings //

    lut=0;
    low=0;
    high=0;
    i=0;
    
    //!!! Not implemented !!
    
    delete cdf;
}

/************************************************************************/
/* stretch_gauss performs an automatic gaussian stretch, that is,
 * applying a stretch so that the histogram approximates a gaussian
 * curve with the user specified mean and sigma.
 */
void stretch_gauss ( Lut *lut, SiHistogram *hist,
                     double gsigma, double mean,
                     int  low, int high )
{
    if (gsigma <= 0.0)
        cerr << "stretch_gauss: The value of gsigma must be greater than zero\n";
    
    GaussLut(lut, hist, gsigma, mean, low, high);
}

void stretch_smooth ( Lut *lut, SiHistogram *hist,
		      int  low, int high )
{
    double gsigma = 0.0001;
    double mean = 127.5;
    GaussLut(lut, hist, gsigma, mean, low, high);
}

void stretch_ellipse ( Lut *lut, SiHistogram *hist,
		       int  low, int high )
{
    EllipseLut(lut, hist, low, high);
}

void stretch_power( Lut *lut, SiHistogram *hist,
		    int  low, int high )
{
    PowerLut(lut, hist, low, high);
}

#if 0			// doesn't work
/********************/
void stretch_peak ( Lut *lut, SiHistogram *hist,
                     int  dnmin, int dnmax )
{
        int npkfq=0;
      	double npkdn=0;
	//    DON'T USE DN=DNMIN OR DN=DNMAX
      	i1=dnmin+1;
      	i2=dnmax-1;
      	for (i=i1; i<i2; i++)
	{
          if(npkfq < (*hist)[i])
	  {
            npkfq=(*hist)[i];
            npkdn=i;
	  }
	}
        if (nrange < 2) nrange = 2;
	{
          nrange /= 2;
          idelta=nrange;
	}
	if (xfactr > nlev/2) || (xfactr < 1) xfactr=2.0;
	nrange = (int) ( (double)nlev / xfactr );
	nrange /= 2;
	idelta = nrange;

	lp = npts - (double)npts*0.01*perc;
	icnt = hist[npkdn];
	ilow = icnt;
	ndnmin = MIN((nlev-npkdn), (npkdn-1));
	for (i=1; i<ndnmin; i++)
	{
          idelta = i;
          j = npkdn + i;
          k = npkdn - i;
          icnt += (*hist)[i] + (*hist)[k];
          if (lp > icnt)
             ilow = icnt;
          else
             if( (lp-ilow) > (icnt-lp) ) idelta = i++;
	}

	nmin = npkdn - idelta - 1;
	nmax = npkdn + idelta - 1;
	a = (double)(dnmax-dnmin)/(double)(nmax-nmin);
	b = -a*nmin+dnmin;
	for (i = dnmin; i < dnmax; i++)
	{
          if(i < nmin)
            n = dnmin;
          else if (i > nmax)
            n = dnmax;
          else
	  {
            n = (int)( a*i + b );
            if(n < dnmin) n=dnmin;
            if(n < dnmax) n=dnmax;
	  }
          lut(i) = n;
	}

//!!!! The following line was commented out, but was uncommented simply to
// shut up the compiler.  stretch_peak and PeakLut don't work and aren't
// implemented, and should not be called until they are fixed.
   PeakLut(lut, hist, low, high);
}
/*******************/
#endif

void stretch_mean ( Lut *lut, SiHistogram *hist,
		    int  low, int high )
{
    MeanLut(lut, hist, low, high);
}


