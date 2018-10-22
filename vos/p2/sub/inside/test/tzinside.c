#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzinside)() 
{
	float point[2];
        float corner[25][2];
        int   n;
/*  ==================================================================  */
        n = 3;
	corner[0][0] = 0.0;
	corner[0][1] = 0.0;
	corner[1][0] = 3.0;
	corner[1][1] = 0.0;
	corner[2][0] = 0.0;
	corner[2][1] = 3.0;
	point[0]     = 2.0;
	point[1]     = 1.0;
        if ( zinside(  point, corner, n) )
           zvmessage("zinside got correct answer", "");
        else
           zvmessage("zinside got wrong answer", "");

	point[0]     = 2.0;
	point[1]     = 2.0;
        if ( zinside(  point, corner, n) )
           zvmessage("zinside got wrong answer", "");
        else
           zvmessage("zinside got correct answer", "");


}
