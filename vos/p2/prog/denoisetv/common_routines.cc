//
//#####################################################################
//					      common_routines.cpp
//#####################################################################
// Igor Yanovsky (C) JPL
// version: 07/25/2016
//#####################################################################
//

#include <algorithm>
#include <functional> 
#include <iostream>
using namespace std;

#include <math.h>

#include "common_routines.h"
#include "BC_2D.h"


//#####################################################################

long round_to_int( double a )
{
	int int_a = (int)floor(a + 0.5);
	return int_a;
}

//#####################################################################

void showSize( const DoubleArray2D& A, const string& str )
{
	cout << "size(" << str << ") = " << "(" << A.getIndex1Size() << "," << A.getIndex2Size() << ")" << endl;
}

//#####################################################################

void display( const DoubleArray2D& A, const string& str, const long& i, const long& j )
{
	cout << str << "( " << i << "," << j << ") =" << A(i,j) << endl;
}

//#####################################################################

void display( const DoubleArray3D& A, const string& str, const long& i, const long& j, const long& k )
{
	cout << str << "( " << i << "," << j << "," << k << ") =" << A(i,j,k) << endl;
}

//#####################################################################

bool isequal( const DoubleArray2D& A, const DoubleArray2D& B )
{
	bool equality = true;
	
	long mA = A.getIndex1Size();
	long nA = A.getIndex2Size();
	
	long mB = B.getIndex1Size();
	long nB = B.getIndex2Size();
	
	if( mA != mB || nA != nB)
	{
		showSize( A, "A");
		showSize( B, "B");
		cout << endl << endl << "Dimensions are different" << endl << endl;
		exit(1);
	}

	long i, j;
	for ( i = 0; i < mA; i++ )
	{
		for ( j = 0; j < nA; j++ )
		{
			if( A(i,j) != B(i,j) )
			{	equality = false;	}
		}
	}
	return equality;
}

//#####################################################################

double findMax( const DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
    
	double Amax = A(1,1);
    
	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			Amax = (Amax > A(i,j)) ? Amax : A(i,j);
		}
	}
	cout << "max = " << Amax << endl;
    
    return Amax;
}

//#####################################################################

void findMaxMin( const DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double AMAX = fabs(A(1,1));
	double AMIN = fabs(A(1,1));
	double Amax = A(1,1);
	double Amin = A(1,1);

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			AMAX = (AMAX > fabs(A(i,j))) ? AMAX : fabs(A(i,j));
			AMIN = (AMIN < fabs(A(i,j))) ? AMIN : fabs(A(i,j));

			Amax = (Amax > A(i,j)) ? Amax : A(i,j);
			Amin = (Amin < A(i,j)) ? Amin : A(i,j);
		}
	}
	cout << "||max = " << AMAX << ", ||min = " << AMIN << ", " << "max = " << Amax << ", min = " << Amin << endl;
}

//#####################################################################

void findMaxMin( const DoubleArray3D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	long p = A.getIndex3Size();

	double AMAX = fabs(A(1,1,1));
	double AMIN = fabs(A(1,1,1));
	double Amax = A(1,1,1);
	double Amin = A(1,1,1);

	long i, j, k;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			for ( k = 0; k < p; k++ )
			{
				AMAX = (AMAX > fabs(A(i,j,k))) ? AMAX : fabs(A(i,j,k));
				AMIN = (AMIN < fabs(A(i,j,k))) ? AMIN : fabs(A(i,j,k));

				Amax = (Amax > A(i,j,k)) ? Amax : A(i,j,k);
				Amin = (Amin < A(i,j,k)) ? Amin : A(i,j,k);
			}
		}
	}
	cout << "MAX = " << AMAX << ", MIN = " << AMIN << ", " << "max = " << Amax << ", min = " << Amin << endl;
}

//#####################################################################
// used to be called find_position_of_max(...)
double find_MAX_and_position( const DoubleArray2D& A, long& x, long& y )
{
	// Find largest element of A as well as its index
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double AMAX = fabs(A(1,1)) - 1.0;

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			if( fabs(A(i,j)) > AMAX )
			{
				AMAX = fabs(A(i,j));
				x = i;
				y = j;
			}
		}
	}
	cout << "MAX = " << AMAX << ", x = " << x << ", y = " << y << endl;
		
	return AMAX;
}

//#####################################################################

double find_MIN_and_position( const DoubleArray2D& A, long& x, long& y )
{
	// Find largest element of A as well as its index
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double AMIN = fabs(A(1,1)) + 1.0;

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			if( fabs(A(i,j)) < AMIN )
			{
				AMIN = fabs(A(i,j));
				x = i;
				y = j;
			}
		}
	}
	cout << "MIN = " << AMIN << ", x = " << x << ", y = " << y << endl;
		
	return AMIN;
}

//#####################################################################

DoubleArray2D find_multiple_MIN_and_positions( const DoubleArray2D& A, const long& N )
{
	// Return N smallest minima of A and their coordinates.
	//
	// There are two approaches:
	// (1) Call find_MIN_and_position(...) N number of times,
	//     record the value and the coordinates after each call,
	//     set the value of the minimum to 0 after each call.
	//
	// (2) Declare an array with N elements being the first pixels of A.
	//     Find where the max is located within this array.
	//     Compare this max value to next pixel in A.  If the pixel has a smaller value,
	//     replace the value and coordinates.
	//
	// In this routine, the first approach is implemented.
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	long i, j;
	double Mp1 = find_MAX_and_position( A, i, j ) + 1.0;
	
	DoubleArray2D B = A;
	
	// N coordinates and values. Each entry has the form (x,y,min).
	DoubleArray2D C(N,3);

	long k;
	for( k = 0; k < N; k++ )
	{
		C(k,2) = find_MIN_and_position( B, i, j );
		C(k,0) = i;
		C(k,1) = j;
		B(i,j) = Mp1;
	}
	
	return C;
}

//#####################################################################

DoubleArray2D getNormGrad( const DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	double dx = 1.0;
	double dy = 1.0;

	double dx2 = dx*2.0;
	double dy2 = dy*2.0;
		
	DoubleArray2D Grad(m,n);
	
	double dAdx, dAdy;

	long i, j;
	for( i = 1; i < m-1; i++ )
	{
		for( j = 1; j < n-1; j++ )
		{
			dAdx = (A(i+1,j) - A(i-1,j)) / dx2;
			dAdy = (A(i,j+1) - A(i,j-1)) / dy2;
			
			Grad(i,j) = sqrt( dAdx*dAdx + dAdy*dAdy );
		}
	}
	
	return Grad;
}

//#####################################################################

DoubleArray2D getLap( const DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	double dx = 1.0;
	double dy = 1.0;

	double dx_sqrd = dx*dx;
	double dy_sqrd = dy*dy;
	
	DoubleArray2D Lap(m,n);
	
	double d2Adx2, d2Ady2;

	long i, j;
	for( i = 1; i < m-1; i++ )
	{
		for( j = 1; j < n-1; j++ )
		{
			d2Adx2 = (A(i+1,j) - 2.0*A(i,j) + A(i-1,j)) / dx_sqrd;
			d2Ady2 = (A(i,j+1) - 2.0*A(i,j) + A(i,j-1)) / dy_sqrd;
			
			Lap(i,j) = d2Adx2 + d2Ady2;
		}
	}
	
	return Lap;
}

//#####################################################################

double calculate_sum( const DoubleArray2D& A )
{
	// Calculates the sum of all elements in A
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double sum = 0.0;

	long i, j;
	for ( i = 0; i < m; i++ )
	{	for ( j = 0; j < n; j++ )
		{	sum = sum + A(i,j);	}	}
	
	return sum;
}

//#####################################################################

void calculate_mean( const DoubleArray2D& A, double& mean, long w )
{
	// Calculates the mean and standard deviation of elements in A
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
    
	mean = 0.0;
    
	long i, j;
	for ( i = w; i < m-w; i++ )
	{	for ( j = w; j < n-w; j++ )
    {	mean = mean + A(i,j);	}	}
    
	mean = mean /(m-2*w)/(n-2*w);
    
    cout << "mean = " << mean << endl;
}

//#####################################################################

void calculate_Statistics( const DoubleArray2D& A, double& mean, double& std_dev )
{
	// Calculates the mean and standard deviation of elements in A
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	mean = 0.0;
	std_dev = 0.0;

	long i, j;
	for ( i = 0; i < m; i++ )
	{	for ( j = 0; j < n; j++ )
		{	mean = mean + A(i,j);	}	}

	mean = mean / m/n;
	
	for ( i = 0; i < m; i++ )
	{	for ( j = 0; j < n; j++ )
		{	std_dev = std_dev + (A(i,j)-mean)*(A(i,j)-mean);	}	}
		
	std_dev = sqrt( std_dev / double(m*n-1) );
}

//#####################################################################

double calculate_StD( const DoubleArray2D& A )
{
	// Calculates the standard deviation of all elements in A
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double mean = 0.0;
	double std_dev = 0.0;

	long i, j;
	for ( i = 0; i < m; i++ )
	{	for ( j = 0; j < n; j++ )
		{	mean = mean + A(i,j);	}	}

	mean = mean / m/n;
	
	for ( i = 0; i < m; i++ )
	{	for ( j = 0; j < n; j++ )
		{	std_dev = std_dev + (A(i,j)-mean)*(A(i,j)-mean);	}	}
		
	std_dev = sqrt( std_dev / double(m*n-1) );
	
	return std_dev;
}

//#####################################################################

DoubleArray2D getVar( const DoubleArray2D& A, const double& r )
{
	//
	// Calculate the variance of intensities inside a disk with radius r around each point (i,j)
	//
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	// Calculate the mean of intensities inside a disk with radius r around each point (i,j):
	DoubleArray2D mean(m,n);
	long i, j;
	long ii, jj, count = 0;
	for( i = r; i < m-r; i++ )	// consider points on [r, m-r-1]x[r, n-r-1] from image grid [0,m-1]x[0,n-1]
	{
		for( j = r; j < n-r; j++ )
		{
			count = 0;
			for( ii = i-r; ii <= i+r; ii++ )	// consider 2r+1 x 2r+1 square around each point (i,j)
			{
				for( jj = j-r; jj <= j+r; jj++ )
				{	
					if( sqrt( (ii-i)*(ii-i) + (jj-j)*(jj-j) ) <= r )	// consider a disk of radius r around each point (i,j)
					{
						mean(i,j) = mean(i,j) + A(ii,jj);
						count++;
					}
				}
			}
			mean(i,j) = mean(i,j) / count;
		}
	}
	cout << "count = " << count << endl;
		
	// Given the mean, calculate the variance of intensities inside a disk with radius r around each point (i,j):
	DoubleArray2D var(m,n);
	for( i = r; i < m-r; i++ )	// consider points on [r, m-r-1]x[r, n-r-1] from image grid [0,m-1]x[0,n-1]
	{
		for( j = r; j < n-r; j++ )
		{
			for( ii = i-r; ii <= i+r; ii++ )	// consider 2r+1 x 2r+1 square around each point (i,j)
			{
				for( jj = j-r; jj <= j+r; jj++ )
				{	
					if( sqrt( (ii-i)*(ii-i) + (jj-j)*(jj-j) ) <= r )	// consider a disk of radius r around each point (i,j)
					{
						var(i,j) = var(i,j) + (A(ii,jj)-mean(i,j))*(A(ii,jj)-mean(i,j));
					}
				}
			}
		}
	}
	
	return var;
}

//#####################################################################

DoubleArray2D rotate180( const DoubleArray2D& A )
{
	// rotates structure A by 180 degrees around its center
	
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	DoubleArray2D A_rotated(m,n);

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			A_rotated((m-1)-i,(n-1)-j) = A(i,j);
		}
	}
	return A_rotated;
}

//#####################################################################

DoubleArray2D padarray( const DoubleArray2D& A, const long& xx, const long& yy )
{
	//Pads before the first array element and after the last array element along each dimension.

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	DoubleArray2D B( m+2*xx, n+2*yy );

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			B(i+xx,j+yy) = A(i,j);
		}
	}
	return B;
}

//#####################################################################

DoubleArray2D padarray_post( const DoubleArray2D& A, const long& xx, const long& yy )
{
	// Pads after the last array element along each dimension.

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	DoubleArray2D B( m+xx, n+yy );

	long i, j;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
			B(i,j) = A(i,j);
		}
	}
	return B;
}

//#####################################################################

DoubleArray2D cumsum( const DoubleArray2D& A, const long& dim )
{
	// returns the cumulative sum along different dimensions of an array A

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	DoubleArray2D B = A;
	
	long i, j;
	switch(dim)
	{
	case 1:
		for ( i = 1; i < m; i++ )
		{
			for ( j = 0; j < n; j++ )
			{	
				B(i,j) = B(i-1,j) + A(i,j);
			}
		}
		break;

	case 2:
		for ( i = 0; i < m; i++ )
		{
			for ( j = 1; j < n; j++ )
			{	
				B(i,j) = B(i,j-1) + A(i,j);
			}
		}		
		break;

	default: cout << "Invalid dimension entered!" << endl;
		exit(1);
		break;
	}

	return B;
}


//#####################################################################

DoubleArray2D BilinInterp( const DoubleArray2D& T, 
						   const DoubleArray2D& X, const DoubleArray2D& Y,
						   const Grid2D& grid )
{
        // Bilinear Interpolation

	long   m    = grid.m;
	long   n    = grid.n;
	long   w    = grid.w;

	double dx   = grid.dx;
	double dy   = grid.dy;

	double xMin = grid.xMin;
	double xMax = grid.xMax;
	double yMin = grid.yMin;
	double yMax = grid.yMax;

	DoubleArray2D Tx      = T;		// interpolation in x-direction
	DoubleArray2D interpT = T;

	long i, j;

	long newX, newY;
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newX = long(floor(X(i,j)));
			if( newX < 0 )
			{	Tx(i,j) = T(0,j);	}
			else if( newX+1 >= m )
			{	Tx(i,j) = T(m-1,j);	}
			else
			{	Tx(i,j)      = T(newX,j) + ((T(newX+1,j)-T(newX,j))/dx) * (X(i,j)-newX);	}	// T(x-u1)
		}
	}
	
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newY = long(floor(Y(i,j)));
			if( newY < 0 )
			{	interpT(i,j) = T(i,0);	}
			else if( newY+1 >= n )
			{	interpT(i,j) = T(i,n-1); }
			else
			{	interpT(i,j) = Tx(i,newY) + ((Tx(i,newY+1)-Tx(i,newY))/dy) * (Y(i,j)-newY);	}		// T(y-u2)
		}
	}

	applyBC( interpT, w, 5 );

	return interpT;
}

//#####################################################################

DoubleArray2D BilinInterp( const DoubleArray2D& T, 
						   const DoubleArray2D& X1, const DoubleArray2D& Y1,
						   const double& dx )
{
        // Bilinear Interpolation

	long  m = T.getIndex1Size();
	long  n = T.getIndex2Size();

	double dy = dx;

	DoubleArray2D Tx      = T;		// interpolation in x-direction
	DoubleArray2D interpT = T;


	DoubleArray2D X = X1;	X = X/dx;
	DoubleArray2D Y = Y1;	Y = Y/dx;

	long i, j;

	long newX, newY;
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newX = long(floor(X(i,j)));
			if( newX < 0 )
			{	Tx(i,j) = T(0,j);	}
			else if( newX+1 >= m )
			{	Tx(i,j) = T(m-1,j);	}
			else
			{	
				//Tx(i,j)      = T(newX,j) + ((T(newX+1,j)-T(newX,j))/dx) * (X(i,j)-newX);	
				Tx(i,j)      = T(newX,j) + (T(newX+1,j)-T(newX,j)) * (X(i,j)-newX);
			}	// T(x-u1)
		}
	}
	
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newY = long(floor(Y(i,j)));
			if( newY < 0 )
			{	interpT(i,j) = T(i,0);	}
			else if( newY+1 >= n )
			{	interpT(i,j) = T(i,n-1); }
			else
			{	
				//interpT(i,j) = Tx(i,newY) + ((Tx(i,newY+1)-Tx(i,newY))/dy) * (Y(i,j)-newY);	
				interpT(i,j) = Tx(i,newY) + (Tx(i,newY+1)-Tx(i,newY)) * (Y(i,j)-newY);
			}		// T(y-u2)
		}
	}

	applyBC( interpT, 1, 5 );

	return interpT;
}

//#####################################################################

DoubleArray2D BilinInterp2( const DoubleArray2D& T, 
						    const DoubleArray2D& u, const DoubleArray2D& v,
						    const Grid2D& grid )
{
        // Bilinear Interpolation

	long   m    = grid.m;
	long   n    = grid.n;
	long   w    = grid.w;

	double dx   = grid.dx;
	double dy   = grid.dy;

	double xMin = grid.xMin;
	double xMax = grid.xMax;
	double yMin = grid.yMin;
	double yMax = grid.yMax;

	DoubleArray2D Tx      = T;		// interpolation in x-direction
	DoubleArray2D interpT = T;

	DoubleArray2D X(m,n), Y(m,n);
	long i, j;
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			X(i,j) = i + u(i,j);		// X = x-u1
			Y(i,j) = j + v(i,j);		// Y = y-u2
		}
	}


	long newX, newY;
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newX = long(floor(X(i,j)));
			if( newX < 0 )
			{	Tx(i,j) = T(0,j);	}
			else if( newX+1 >= m )
			{	Tx(i,j) = T(m-1,j);	}
			else
			{	Tx(i,j)      = T(newX,j) + ((T(newX+1,j)-T(newX,j))/dx) * (X(i,j)-newX);	}	// T(x-u1)
		}
	}
	
	for( j = 1; j < n-1; j++ )
	{
		for( i = 1; i < m-1; i++ )
		{
			newY = long(floor(Y(i,j)));
			if( newY < 0 )
			{	interpT(i,j) = T(i,0);	}
			else if( newY+1 >= n )
			{	interpT(i,j) = T(i,n-1); }
			else
			{	interpT(i,j) = Tx(i,newY) + ((Tx(i,newY+1)-Tx(i,newY))/dy) * (Y(i,j)-newY);	}		// T(y-u2)
		}
	}

	applyBC( interpT, w, 5 );

	return interpT;
}

//#####################################################################

// Calculate root-mean-square error (RMSE)

double calculateRMSE( const DoubleArray2D& A, const DoubleArray2D& B )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	double RMSE = 0.0;

	long i, j;
	for(j = 0; j < n; j++)
	{
		for(i = 0; i < m; i++)
		{
			RMSE = RMSE + pow( A(i,j) - B(i,j), 2.0 );
		}
	}
	// RMSE = sqrt(RMSE) /m/n;
	RMSE = sqrt(RMSE/(m*n));   // Changed on 06/15/2013

	cout << "rmse = " << RMSE << endl;

	return RMSE;
}

//#####################################################################

// Calculate root-mean-square error (RMSE)

double calculateRMSE( const VectorImage& A, const VectorImage& B )
{
	long m = A.ch[0].getIndex1Size();
	long n = A.ch[0].getIndex2Size();
	
	double RMSE = 0.0;
	
	long i, j;
	long kk;
	
	for (kk=0; kk < numCh; kk++)
	{
		for(j = 0; j < n; j++)
		{
			for(i = 0; i < m; i++)
			{
				RMSE = RMSE + pow( A.ch[kk](i,j) - B.ch[kk](i,j), 2.0 );
			}
		}
	}
	// RMSE = sqrt(RMSE) /m/n /numCh;
    // RMSE = sqrt(RMSE/m/n) /numCh;    // Changed on 06/15/2013
    RMSE = sqrt(RMSE/m/n/numCh);    // Changed on 07/09/2013
	
	cout << "rmse = " << RMSE << endl;
	
	return RMSE;
}

//#####################################################################

// Calculate root-mean-square error (RMSE)

double calculateRMSE( const DoubleArray3D& A, const DoubleArray3D& B )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
    long p = A.getIndex3Size();
    
	double RMSE = 0.0;
    
	long i, j, k;
    for(k = 0; k < p; k++)
    {
        for(j = 0; j < n; j++)
        {
            for(i = 0; i < m; i++)
            {
                RMSE = RMSE + pow( A(i,j,k) - B(i,j,k), 2.0 );
            }
        }
	}
	RMSE = sqrt(RMSE/(m*n*p));
    
	cout << "rmse = " << RMSE << endl;
    
	return RMSE;
}

//#####################################################################

// Calculate signal-to-noise ratio (SNR)

double calculateSNR( const DoubleArray2D& original, const DoubleArray2D& u )
{
	long m = original.getIndex1Size();
	long n = original.getIndex2Size();
	
	long size = m*n;
	double sum = 0.0;

	long i, j;

	// Average intensity of original image:
	for(j = 0; j < n; j++)
	{	for(i = 0; i < m; i++)
		{	sum = sum + original(i,j);	}	}
	double mean = sum / double(size);

	// Variance of the original image (variance = standard_deviation^2):
	double temp = 0.0;
	for(j = 0; j < n; j++)
	{	for(i = 0; i < m; i++)
		{	temp = temp + (original(i,j) - mean)*(original(i,j) - mean);	}	}
	double var = temp / double(size);

	// Mean Square Error:
	double temp2 = 0.0;
	for(j = 0; j < n; j++)
	{	for(i = 0; i < m; i++)
		{	temp2 = temp2 + (original(i,j) - u(i,j))*(original(i,j) - u(i,j));	}	}
	double MSE = temp2 / double(size);

	// signal-to-noise ratio:
	double SNR = 10*log10( var/MSE );
	
	cout << "  snr = " << SNR << endl;

	return SNR;
}

//#####################################################################

// Calculate signal-to-noise ratio (SNR)

double calculateSNR( const VectorImage& original, const VectorImage& u )
{
	long m = original.ch[0].getIndex1Size();
	long n = original.ch[0].getIndex2Size();
	
	long size = m*n;
	double sum = 0.0;
	
	long i, j;
	long kk;
	
	// Average intensity of original image:
	for (kk=0; kk < numCh; kk++)
	{
		for(j = 0; j < n; j++)
		{	for(i = 0; i < m; i++)
			{	sum = sum + original.ch[kk](i,j);	}	}
	}
	double mean = sum / double(size);
	
	// Variance of the original image (variance = standard_deviation^2):
	double temp = 0.0;
	for (kk=0; kk < numCh; kk++)
	{
		for(j = 0; j < n; j++)
		{	for(i = 0; i < m; i++)
			{	temp = temp + (original.ch[kk](i,j) - mean)*(original.ch[kk](i,j) - mean);	}	}
	}
	double var = temp / double(size);
	
	// Mean Square Error:
	double temp2 = 0.0;
	for (kk=0; kk < numCh; kk++)
	{
		for(j = 0; j < n; j++)
		{	for(i = 0; i < m; i++)
			{	temp2 = temp2 + (original.ch[kk](i,j) - u.ch[kk](i,j))*(original.ch[kk](i,j) - u.ch[kk](i,j));	}	}
	}
	double MSE = temp2 / double(size);
	
	// signal-to-noise ratio:
	double SNR = 10*log10( var/MSE );
	
	cout << "  snr = " << SNR << endl;
	
	return SNR;
}

//#####################################################################

// Calculate peak signal-to-noise ratio (PSNR)

double calculatePSNR( const DoubleArray2D& original, const DoubleArray2D& u )
{
	long m = original.getIndex1Size();
	long n = original.getIndex2Size();
	
	long size = m*n;
    
	long i, j;
    
    double max_u0 = findMax(original);
    
	// Mean Square Error:
	double temp2 = 0.0;
	for(j = 0; j < n; j++)
	{	for(i = 0; i < m; i++)
    {	temp2 = temp2 + (original(i,j) - u(i,j))*(original(i,j) - u(i,j));	}	}
	double MSE = temp2 / double(size);
    
	// signal-to-noise ratio:
	double PSNR = 10*log10( max_u0 * max_u0 /MSE );
	
	cout << "  psnr = " << PSNR << endl;
    
	return PSNR;
}

//#####################################################################

void LowerEffectiveResolution( DoubleArray2D& u )
{
    // For image u, fix intensity values for even rows and columns.
    // Interpolate (average) intensity values when either (or both)
    // rows and columns are odd.
	long m = u.getIndex1Size();
	long n = u.getIndex2Size();
    
	long i, j;
    for(j = 0; j < n-1; j++)
    {
        for(i = 0; i < m-1; i++)
        {
            // For i%2 == 0 and j%2 == 0 => Intensity values at these points are fixed.
            if( i%2 == 0 && j%2 == 1 )
            {
                u(i,j) = (u(i,j-1) + u(i,j+1)) / 2.0;
            }
            else if( i%2 == 1 && j%2 == 0 )
            {
                u(i,j) = (u(i-1,j) + u(i+1,j)) / 2.0;
            }
            else if( i%2 == 1 && j%2 == 1 )
            {
                u(i,j) = (u(i-1,j-1) + u(i+1,j-1) + u(i-1,j+1) + u(i+1,j+1)) / 4.0;
            }
        }
    }
}

//#####################################################################

void generateTestImage( DoubleArray2D& u )
{
    long m = u.getIndex1Size();
	long n = u.getIndex2Size();
    
    double xcent = (m-1) / 2.0;
	double ycent = (n-1) / 2.0;
	double r = (min(m,n))/3.0;
    
    double xx; double yy;
    
    long i, j;
    for( j = 0; j < n; j++ )
    {
        for( i = 0; i < m; i++ )
        {
            xx = double(i);
            yy = double(j);
            if( sqrt( (xx-xcent)*(xx-xcent) + (yy-ycent)*(yy-ycent) ) < m/5.5 )
                u(i,j) = 255.0;
            else
                u(i,j) = 0.0;
        }
    }
}

//#####################################################################

void assign_Vector_values_to_Matrix( DoubleArray2D& W,
                                     const DoubleArray2D& P,
                                     const DoubleArray1D& b )
{

    long m = W.getIndex1Size();
	long n = W.getIndex2Size();
 
    long I = 0;
    long i, j;
    for( i = 0; i < m; i++ )
    {
        for( j = 0; j < n; j++ )
        {
            if( P(i,j) > 0.5 )  // i.e. pij = 1
            {
                W(i,j) = b(I);
                I = I + 1;
            }
        }
    }
}

//#####################################################################

void assign_Matrix_values_to_Vector( const DoubleArray2D& W,
                                     const DoubleArray2D& P,
                                     DoubleArray1D& b )
{
    
    long m = W.getIndex1Size();
	long n = W.getIndex2Size();
    
    long I = 0;
    long i, j;
    for( i = 0; i < m; i++ )
    {
        for( j = 0; j < n; j++ )
        {
            if( P(i,j) > 0.5 )  // i.e. pij = 1
            {
                b(I) = W(i,j);
                I = I + 1;
            }
        }
    }
}

//#####################################################################

void construct_Pick( DoubleArray2D& P, const long& k )
{
    long m = P.getIndex1Size();
	long n = P.getIndex2Size();
    
    long i, j;
    for( j = 0; j < n; j++ )
    {
        for( i = 0; i < m; i++ )
        {
            if( i%k == 0 && j%k == 0 )
            {
                P(i,j) = 1.0;
            }
        }
    }
}

//#####################################################################

DoubleArray2D permute( const DoubleArray2D& A )
{
    long n = A.getIndex1Size();     // dimensions are switched
    long m = A.getIndex2Size();
    
    DoubleArray2D B(m,n);
    
	long i, j, k;
	for ( i = 0; i < m; i++ )
	{
		for ( j = 0; j < n; j++ )
		{
            B(i,j) = A(j,i);
		}
	}
	return B;
}

//#####################################################################

DoubleArray3D permute( const DoubleArray3D& A, const long order )
{
    long m, n, p;
    long i, j, k;
    
    DoubleArray3D B;
    
    if ( order == 132 )
    {
        m = A.getIndex1Size();
        p = A.getIndex2Size();     // 2nd and 3rd dimensions are switched
        n = A.getIndex3Size();
        
        B.initialize(m,n,p);
 
        for ( i = 0; i < m; i++ )
        {
            for ( j = 0; j < n; j++ )
            {
                for( k = 0; k < p; k++ )
                {
                    B(i,j,k) = A(i,k,j);
                }
            }
        }
    }
    if ( order == 321 )
    {
        p = A.getIndex1Size();
        n = A.getIndex2Size();
        m = A.getIndex3Size();
        
        B.initialize(m,n,p);
        
        for ( i = 0; i < m; i++ )
        {
            for ( j = 0; j < n; j++ )
            {
                for( k = 0; k < p; k++ )
                {
                    B(i,j,k) = A(k,j,i);
                }
            }
        }
    }
    else
    {
        cout << "Not a valid order for permute(). Exiting..." << endl << endl;
    }
    
	return B;
}
