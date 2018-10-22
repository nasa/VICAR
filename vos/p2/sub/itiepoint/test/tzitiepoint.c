#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzitiepoint)(tie1, npts, tie2) 
int   *npts;
float *tie1, *tie2;
{
      int unit2, nah2, npts2, i, k;
      char pbuf[81];
/*  ==================================================================  */

/*  TEST 2: non-gridded set of tiepoints  */

      zvunit(&unit2,"OUT",2, 0);

      ziwrite_tiepoints(unit2, 0,0, *npts, tie1, 4);  /*  write to a file  */

      zvunit(&unit2,"OUT",2, 0);

      npts2 = 0;
      ziread_tiepoints(unit2, &nah2,&npts2,100,tie2, 4);  /* read from a file.*/

/*  Now check that no data was lost in the move.  */

      if (nah2 != 0)
         zmabend("Error in nah2 value");

      if (npts2 != (*npts))
         zmabend("Error in npts2 value");

      k = 0;  /*  error count  */
      for (i=0; i < npts2; ++i)  {
          if (tie1[k] != tie2[k])  {
             ++k;
             sprintf( pbuf, "Error on tiepoint %d  %f  %f", i,tie2[i],tie1[i]);
             zvmessage(pbuf, "");
           }
       }

       if (k == 0)
          zvmessage("Success on test 2", "");
       else
          zvmessage("Failure on test 2", "");

}
