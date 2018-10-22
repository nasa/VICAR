/*--------------------------------------------------------------------------*/
/*   c-language bridge to Timsfilt, which calls the cwave subroutine        */
/*   from the file timssubs.f.  Timsfilt is a simple interface to centwave  */
/*   that returns the filter and wavelen arrays explicitly instead of via   */
/*   fortran common blocks                                                  */


#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ftnname.h"


int ztimsfilt(date, wave, filter, wavelen)
  int date;
  float wave[6], filter[6][158], wavelen[158];
{
  FTN_NAME(timsfilt)(&date, wave, filter, wavelen);

  return 1;
}
