#ifndef __BC_2D__
#define __BC_2D__

#include "DoubleArray2D.h"

//
//#####################################################################
//						BC_2D.h
//#####################################################################
//
// Igor Yanovsky (C) UCLA
// Version: Dec. 15, 2006
//
// Igor Yanovsky, as original author, is contributing this code to JPL 
// with no restrictions.
//
//#####################################################################
//
// Routines below apply different boundary conditions (BC)
//

void applyBC( DoubleArray2D& A, const long width, const long type );

void DirichletBC( DoubleArray2D& A, const long width );

void NeumannBC( DoubleArray2D& A, const long width );

void NeumannBC_linear( DoubleArray2D& A, const long width );

void NeumannBC_flipping( DoubleArray2D& A, const long width );

void PeriodicBC( DoubleArray2D& A, const long width );

void Periodic_X_Extrapolation_Y( DoubleArray2D& A, const long width );


#endif
