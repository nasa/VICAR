//
//#####################################################################
//						Grid2D.h
//#####################################################################
//
// Igor Yanovsky (C) JPL
// Version: March 7, 2009
//
//#####################################################################
//

#ifndef __Grid2D__
#define __Grid2D__


class Grid2D
{
public :

	double xMin;	// Computational Region is [xMin,xMax]x[yMin,yMax]
	double xMax;
	double yMin;
	double yMax;
	long   m;       // Number of Points in the x direction
	long   n;       // Number of Points in the y direction

	long   bxi;		// Number of points the image is extended at the time of input.
	long   byi;		// These points will be cropped before output.
					// These are usually set to 0 (no extension or cropping).
	long   bxo;		// Number of points the image is cropped at the time of output.
	long   byo;		// These points will be cropped before output.
					// These are usually set to 0 (no extension or cropping).

	long side1xcrop;
	long side2xcrop;
	long side1ycrop;
	long side2ycrop;

	long   w;		// Number of Points used for the width of the boundary of the domain
	long   gamma;	// the Width of the boundary of the tube (for Local Level Set)
	
	double dx;      // mesh width in x direction
	double dy;      // mesh widht in y direction

	double deltaWidth;

	Grid2D() {	bxi = 0; byi = 0;
				bxo = 0; byo = 0;	}
};

#endif
