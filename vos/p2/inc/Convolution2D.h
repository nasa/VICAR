//
//#####################################################################
//					 Convolution2D.h
//#####################################################################
// Igor Yanovsky (C) UCLA, JPL                                
// version:  08/29/2016
//
// Igor Yanovsky, as original author, is contributing this code to JPL 
// with no restrictions.
//
//#####################################################################
//

#ifndef __convolution2D__
#define __convolution2D__

#include "DoubleArray2D.h"
#include "Grid2D.h"


void fft2( const DoubleArray2D& A,
           DoubleArray2D& fftA_r,
           DoubleArray2D& fftA_i );
DoubleArray2D ifft2( const DoubleArray2D& A_r,
                     const DoubleArray2D& A_i );

DoubleArray2D fftshift2D( const DoubleArray2D& A );
DoubleArray2D circshift2D( const DoubleArray2D& A,
                           const long& p1,
                           const long& p2 );

void psf2otf( const DoubleArray2D& A,
              DoubleArray2D& fftA_r,
              DoubleArray2D& fftA_i,
              const long& xx, const long& yy,
              bool output );

DoubleArray2D OutOfFocus2D( const Grid2D& grid,
                            const double& radius);
DoubleArray2D OutOfFocus2D_directional( const Grid2D& grid,
                                        const double& rx,
                                        const double& ry );
DoubleArray2D RectBlur( const Grid2D& grid,
                        const double& width,
                        const double& height );
DoubleArray2D ComplexShapeBlur( const Grid2D& grid,
                                const double& w1,
                                const double& h1,
                                const double& w2,
                                const double& h2 );
DoubleArray2D Gaussian2D( const Grid2D& grid,
                          const double& std);
DoubleArray2D Gaussian2D( const long& m,
                          const long& n,
                          const double& std );
DoubleArray2D Gaussian2D( const long& m,
                          const double& std);

DoubleArray2D SMAP_c( const long& number );

DoubleArray2D SMAP_PSF( const long& m, const long& n );

DoubleArray2D GeoSTAR_c( );
DoubleArray2D GeoSTAR_c_noisy( const long& k );
DoubleArray2D GeoSTAR_c( const long& size );
DoubleArray2D afk_real_c( long choice );
DoubleArray2D GeoSTAR( const DoubleArray2D& Kc,
                       const Grid2D& grid );

DoubleArray2D GeoSTAR( const Grid2D& grid );
DoubleArray2D MER_MI_kernel( const Grid2D& grid,
                             const char kernel_choice );
DoubleArray2D IdentityBlur( const Grid2D& grid );
DoubleArray2D StraightMotionBlur( const Grid2D& grid );
DoubleArray2D StraightMotionBlur( const long& m, const long& n, const long& dist );
DoubleArray2D HurricanePSF( const Grid2D& grid );

DoubleArray2D dGdy( const long I,
                    const double std);

void convolution2D( DoubleArray2D& F,
                    const DoubleArray2D& K );

void convolution_MTF_2D( DoubleArray2D& F,
                         const DoubleArray2D& MTF_r,
                         const DoubleArray2D& MTF_i );

void deconvFidelity2D( DoubleArray2D& result,
                       const DoubleArray2D& F,
                       const DoubleArray2D& K,
                       const DoubleArray2D& Krot,
                       const DoubleArray2D& U );

void convGaussian2D( DoubleArray2D& F,
                     const Grid2D& grid,
                     const double& std );

#endif
