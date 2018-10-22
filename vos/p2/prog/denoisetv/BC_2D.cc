#include <iostream>
using namespace std;

#include <stdlib.h>

//
//#####################################################################
//						BC_2D.cpp
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


#include "BC_2D.h"

//#####################################################################

//
// Routines below apply different boundary conditions (BC)
//

//#####################################################################

void applyBC( DoubleArray2D& A, const long width, const long type )
{
	switch( type )
	{
	case 0:		// None
		break;

	case 1:		// Dirichlet
		DirichletBC( A,width );
		break;

	case 5:		// Constant extrapolation
		NeumannBC( A, width );
		break;

	case 6:		// Linear extrapolation
		NeumannBC_linear( A, width );
		break;

	case 9:		// mirroring/flipping
		NeumannBC_flipping( A, width );
		break;

	case 10:	// Periodic
		PeriodicBC( A, width );
		break;

	case 11:	// Periodic in X, linear extrapolation in Y
		Periodic_X_Extrapolation_Y( A, width );
		break;
	
	default: cout << "Incorrect Specification of Boundary Conditions " << endl;
		exit(1);
		break;
	}
}

//#####################################################################

void DirichletBC( DoubleArray2D& A, const long width )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;
	
	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{	A(i,j) = 0;	}

		for( i = m-width; i < m; i++ )
		{	A(i,j) = 0;	}
	}

	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{	A(i,j) = 0;	}

		for( j = n-width; j < n; j++ )
		{	A(i,j) = 0;	}
	}
}

//#####################################################################

/*
void NeumannBC( DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;

	for(j=0; j < n; j++)
	{
		A(0,j)   = A(1,j);
		A(m-1,j) = A(m-2,j);
	}

	for(i=0; i < m; i++)
	{
		A(i,0)   = A(i,1);
		A(i,n-1) = A(i,n-2);
	}

	A(0,0) = A(1,1);
	A(m-1,0) = A(m-2,1);
	A(0,n-1) = A(1,n-2);
	A(m-1,n-1) = A(m-2,n-2);
}
*/

//#####################################################################

void NeumannBC( DoubleArray2D& A, const long width )
{
	//
	// constant extension:
	//
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;
	
	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{	A(i,j) = A(width,j);		}

		for( i = m-width; i < m; i++ )
		{	A(i,j) = A((m-1)-width,j);	}
	}

	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{	A(i,j) = A(i,width);		}

		for( j = n-width; j < n; j++ )
		{	A(i,j) = A(i,(n-1)-width);	}
	}

	for( i=0; i < width; i++ )
	{
		A(  i  ,  i  ) = A( width     ,   width    );
		A(m-1-i,  i  ) = A((m-1)-width,   width    );
		A(  i  ,n-1-i) = A( width     , (n-1)-width);
		A(m-1-i,n-1-i) = A((m-1)-width, (n-1)-width);
	}
}

//#####################################################################

void NeumannBC_linear( DoubleArray2D& A, const long width )
{
	//
	// linear extrapolation:
	//
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;

	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{	A(i,j) = 2*A(width,j) - A(2*width-i,j);			}

		for( i = m-width; i < m; i++ )
		{	A(i,j) = 2*A((m-1)-width,j) - A( 2*((m-1)-width)-i, j );	}
	}

	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{	A(i,j) = 2*A(i,width) - A(i,2*width-j);			}

		for( j = n-width; j < n; j++ )
		{	A(i,j) = 2*A(i,(n-1)-width) - A( i, 2*((n-1)-width)-j );	}
	}
}

//#####################################################################

void NeumannBC_flipping( DoubleArray2D& A, const long width )
{
	//
	// mirroring/flipping the values:
	//
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;
	
	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{	A(i,j) = A(2*width-i,j);			}

		for( i = m-width; i < m; i++ )
		{	A(i,j) = A( 2*((m-1)-width)-i, j );	}
	}

	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{	A(i,j) = A(i,2*width-j);			}

		for( j = n-width; j < n; j++ )
		{	A(i,j) = A( i, 2*((n-1)-width)-j );	}
	}
}

//#####################################################################

void PeriodicBC( DoubleArray2D& A, const long width )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;

	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{
			A(i,j) = A( m-1-2*width+i , j );
		}
		for( i = m-width-1; i < m; i++ )
		{
			A(i,j) = A( i+2*width-m+1 , j );
		}
	}

	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{
			A(i,j) = A( i , n-1-2*width+j );
		}
		for( j = n-width-1; j < n; j++ )
		{
			A(i,j) = A( i , j+2*width-n+1 );
		}
	}
}

//#####################################################################

void Periodic_X_Extrapolation_Y( DoubleArray2D& A, const long width )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long i;  long j;

	// Periodic in X direction:
	for(j=0; j < n; j++)
	{
		for( i = 0; i < width; i++ )
		{
			A(i,j) = A( m-1-2*width+i , j );
		}
		for( i = m-width-1; i < m; i++ )
		{
			A(i,j) = A( i+2*width-m+1 , j );
		}
	}

	// Linear extrapolation in Y direction:
	for(i=0; i < m; i++)
	{
		for( j = 0; j < width; j++ )
		{	A(i,j) = 2*A(i,width) - A(i,2*width-j);			}

		for( j = n-width; j < n; j++ )
		{	A(i,j) = 2*A(i,(n-1)-width) - A( i, 2*((n-1)-width)-j );	}
	}
}

//#####################################################################
