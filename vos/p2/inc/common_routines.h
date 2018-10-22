//
//#####################################################################
//					      common_routines.h
//#####################################################################
// Igor Yanovsky (C) JPL
// version: 07/25/2016
//#####################################################################
//

#ifndef __common_routines__
#define __common_routines__

#include <vector>
#include "DoubleArray1D.h"
#include "DoubleArray2D.h"
#include "DoubleArray3D.h"
#include "VectorImage.h"

#include "Grid2D.h"


long round_to_int( double a );

void showSize( const DoubleArray2D& A, const string& str );

bool isequal( const DoubleArray2D& A, const DoubleArray2D& B );

void display( const DoubleArray2D& A, const string& str, const long& i, const long& j );
void display( const DoubleArray3D& A, const string& str, const long& i, const long& j, const long& k );

double findMax(  const DoubleArray2D& A );
void findMaxMin( const DoubleArray2D& A );
void findMaxMin( const DoubleArray3D& A );

double find_position_of_max( const DoubleArray2D& A, double& x, double& y );
double find_MIN_and_position( const DoubleArray2D& A, double& x, double& y );
DoubleArray2D find_multiple_MIN_and_positions( const DoubleArray2D& A, const long& N );

DoubleArray2D getNormGrad( const DoubleArray2D& A );

DoubleArray2D getLap( const DoubleArray2D& A );

double calculate_sum( const DoubleArray2D& A );

void calculate_mean( const DoubleArray2D& A, double& mean, long w );
void calculate_Statistics( const DoubleArray2D& A, double& mean, double& std_dev );

double calculate_StD( const DoubleArray2D& A );

DoubleArray2D getVar( const DoubleArray2D& A, const double& r );

DoubleArray2D rotate180( const DoubleArray2D& A );	// rotates structure A by 180 degrees around its center

DoubleArray2D padarray( const DoubleArray2D& A, const long& xx, const long& yy );

DoubleArray2D padarray_post( const DoubleArray2D& A, const long& xx, const long& yy );

DoubleArray2D cumsum( const DoubleArray2D& A, const long& dim );

DoubleArray2D BilinInterp( const DoubleArray2D& T, 
						   const DoubleArray2D& X, const DoubleArray2D& Y,
						   const Grid2D& grid );

DoubleArray2D BilinInterp( const DoubleArray2D& T, 
						   const DoubleArray2D& X, const DoubleArray2D& Y,
						   const double& dx );

DoubleArray2D BilinInterp2( const DoubleArray2D& T, 
						    const DoubleArray2D& u, const DoubleArray2D& v,
						    const Grid2D& grid );

double calculateRMSE( const DoubleArray2D& A, const DoubleArray2D& B );
double calculateRMSE( const VectorImage&   A, const VectorImage&   B );
double calculateRMSE( const DoubleArray3D& A, const DoubleArray3D& B );
double calculateSNR(  const DoubleArray2D& original, const DoubleArray2D& u );
double calculateSNR(  const VectorImage&   original, const VectorImage&   u );
double calculatePSNR( const DoubleArray2D& original, const DoubleArray2D& u );

void LowerEffectiveResolution( DoubleArray2D& u );

void generateTestImage( DoubleArray2D& u );

DoubleArray2D shrink( const DoubleArray2D& y, const double tau );

void assign_Vector_values_to_Matrix( DoubleArray2D& W,
                                     const DoubleArray2D& P,
                                     const DoubleArray1D& b );

void assign_Matrix_values_to_Vector( const DoubleArray2D& W,
                                     const DoubleArray2D& P,
                                     DoubleArray1D& b );

void construct_Pick( DoubleArray2D& P, const long& k );

DoubleArray2D permute( const DoubleArray2D& A );
DoubleArray3D permute( const DoubleArray3D& A, const long order );

//#####################################################################

#endif
