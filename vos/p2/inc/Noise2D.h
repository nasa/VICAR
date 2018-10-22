//
//#####################################################################
// Igor Yanovsky (C) JPL                               
// version:  08/22/2017
//#####################################################################
//

#ifndef __Noise2D__
#define __Noise2D__

#include "DoubleArray1D.h"
#include "DoubleArray2D.h"
#include "Grid2D.h"

//#####################################################################

void addNoise2D( DoubleArray2D& A, double noise_number, const Grid2D& grid );

void addNoise2D_sc( DoubleArray2D& A, double noise_number );

void addGaussNoise2D( DoubleArray2D& A, double variance, const Grid2D& grid );
void addGaussNoise2D( DoubleArray2D& A, DoubleArray1D& noise1D, const long& image_N, double variance, const Grid2D& grid );

void initializeGaussNoise( DoubleArray1D& noise1D );

void addNoiseSaltPepper( DoubleArray2D& A, double d );

void addNoiseSaltPepper( DoubleArray2D& A, double d, double pepper_value, double salt_value );

void readNoise2D( DoubleArray2D& A, const Grid2D& grid, string filename );

#endif
