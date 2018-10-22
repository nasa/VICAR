/*
 *  tibisgr_c.c
 *
 *  A C subroutine which increments the Y coordinate of
 *   a VICAR/IBIS graphics file by one. Used to test the C interface
 *   to the IBISGR graphics library.
 */

#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(update_file)()
{
	float x, y, extra_coords[8];
	int zero, eof;
	int status,count,unit;
	int num_dimen;
	
 	status = zvp ("DIMEN", &num_dimen, &count);
	status = zvunit(&unit,"INP",1,0);
  	status = zupdategr (unit, 1, num_dimen);
	if (status != 1) zsignalgr(1,status,1);

/*     Update the coordinate sets */
	eof = 0;
	while (!eof)
	{
	    status = znextgr (1, &eof, &x, &y, extra_coords);
	    if (status != 1) zsignalgr(1,status,1);
	    zero = 0; /* false */
	    while (!zero && !eof) 
	    {
		status = zputgr (1, x, y+1.0,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
		status = zgetgr (1, &zero, &eof, &x, &y,extra_coords);
	 	if (status != 1) zsignalgr(1,status,1);
	    }
	    if (zero) status = zendgr (1);
	    if (status != 1) zsignalgr(1,status,1);
	}

	status = zclgr (1);
	if (status != 1) zsignalgr(1,status,1);
	
	return;
}


