#ifndef __DoubleArray3D__
#define __DoubleArray3D__

//#define _DEBUG

#include <iostream>
#include <sstream>
#include <iomanip>
using namespace std;

#include <fstream>

#include <stdlib.h>

//
//####################################################################
//                    DoubleArray3D.h  
//####################################################################
/**
   Provides a "light weight" three dimensional array structure 
   with initialization capabilities, algebraic operations, 
   and optional bounds checking. 

   <pre>
   The beginning index default is 0                    : (C convention)
   Data for the array is assumed to be stored by ROWS  : (C convention)
   Access using (*,*,*), e.g. A(i,j,k) for (i,j,k)th element.: (NOT C convention) 
   </pre>

   The copy constructor creates a duplicate instance. Deleting the copy 
   will not delete the data of the original. <p>
   
   Based on the DoubleArray2D class by CRA 01/21/03 <p>
 
@author Igor Yanovsky (C) UCLA 
@version  April 14, 2005
*/
//#####################################################################
// Igor Yanovsky (C) UCLA                               April 14, 2005
//
// Igor Yanovsky, as original author, is contributing this code to JPL 
// with no restrictions.
//
//#####################################################################
//
class DoubleArray3D 
{

public :
//
//###################################################################
//                 Constructors/Initialization
//###################################################################
//
    DoubleArray3D()
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
	index3Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
	index3Begin   = 0;
    index1End     = 0;
    index2End     = 0;
	index3End     = 0;
    };

    DoubleArray3D(long m, long n, long p)
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
	index3Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
	index3Begin   = 0;
    index1End     = 0;
    index2End     = 0;
	index3End     = 0;
    initialize(m,n,p);
    };

    DoubleArray3D(double* d, long m, long n, long p)
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
	index3Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
	index3Begin   = 0;
    index1End     = 0;
    index2End     = 0;
	index3End     = 0;
    initialize(d,m,n,p);
    };

    DoubleArray3D(const DoubleArray3D& D)
    {    
    index1Size    = D.index1Size;
    index2Size    = D.index2Size;
	index3Size    = D.index3Size;
    index1Begin   = D.index1Begin;
    index2Begin   = D.index2Begin;
	index3Begin   = D.index3Begin;
    index1End     = D.index1End;
    index2End     = D.index2End;
	index3End     = D.index3End;

	dataPtr       = new double[index1Size*index2Size*index3Size];
	long i;
	for(i = 0; i < index1Size*index2Size*index3Size; i++) 
	{dataPtr[i] = D.dataPtr[i];}
    };
                                                                ///<p>
    virtual ~DoubleArray3D()
    {
		if(dataPtr !=  0) delete [] dataPtr;
    }


    void initialize(long m, long n, long p)
    {

		if((index1Size != m)||(index2Size != n)||(index3Size != p))
		{
			delete [] dataPtr;
			dataPtr = new double[m*n*p];
		}
		index1Size    = m;
		index2Size    = n;
		index3Size    = p;
		index1Begin   = 0;
		index2Begin   = 0;
		index3Begin   = 0;
		index1End     = index1Begin + (index1Size - 1);
		index2End     = index2Begin + (index2Size - 1);
		index3End     = index3Begin + (index3Size - 1);

		long i;
		for(i = 0; i < index1Size*index2Size*index3Size; i++) 
		{dataPtr[i] = 0.0;}
    };
                                                         

    void initialize(double* d, long m, long n, long p)
    {
		initialize(m,n,p);
		long i;
		for(i = 0; i < index1Size*index2Size*index3Size; i++) 
		{dataPtr[i] = d[i];}
    };
//
//###################################################################
//                  Element Access 
//###################################################################
//
	//
	//  Original indexing: changed to reduce the op count
	//
	//   return *(dataPtr + (i3 - index3Begin) 
	//                    + (i2 - index2Begin)*index3Size 
	//                    + (i1 - index1Begin)*index3Size*index2Size);
	//

#ifdef _DEBUG 
    double&  operator()(long i1, long i2, long i3)
    {
		boundsCheck(i1, index1Begin, index1End,1);
		boundsCheck(i2, index2Begin, index2End,2);
		boundsCheck(i3, index3Begin, index3End,3);
		return *(dataPtr + (i3 - index3Begin) + index3Size*((i2 - index2Begin) + (i1 - index1Begin)*index2Size));	// 3D
		// return *(dataPtr +  (i2 - index2Begin) + (i1 - index1Begin)*index2Size);   // 2D
    };

    const double&  operator()(long i1, long i2, long i3) const
    {
		boundsCheck(i1, index1Begin, index1End,1);
		boundsCheck(i2, index2Begin, index2End,2);
		boundsCheck(i3, index3Begin, index3End,3);
		return *(dataPtr + (i3 - index3Begin) + index3Size*((i2 - index2Begin) + (i1 - index1Begin)*index2Size));
    };
#else
    inline double&  operator()(long i1, long i2, long i3)
    {
		return *(dataPtr + (i3 - index3Begin) + index3Size*((i2 - index2Begin) + (i1 - index1Begin)*index2Size));
    };
                                                                     ///<p>
    inline const double&  operator()(long i1, long i2, long i3) const
    {
		return *(dataPtr + (i3 - index3Begin) + index3Size*((i2 - index2Begin) + (i1 - index1Begin)*index2Size));
    };

#endif

//
//###################################################################
//                Array Structure Access Functions
//###################################################################
//
                                                               ///<p>
    double* getDataPointer(){return dataPtr;};

    void setIndex1Begin(long i) 
    {index1Begin = i; index1End   = index1Begin + (index1Size - 1);};

    void setIndex2Begin(long i)
    {index2Begin = i; index2End   = index2Begin + (index2Size - 1);};

	void setIndex3Begin(long i)
	{index3Begin = i; index3End   = index3Begin + (index3Size - 1);};

    long getIndex1Begin() const {return index1Begin;}
    long getIndex2Begin() const {return index2Begin;}
	long getIndex3Begin() const {return index3Begin;}

    long getIndex1End() const {return index1End;}
    long getIndex2End() const {return index2End;}
	long getIndex3End() const {return index3End;}

    long getIndex1Size()  const {return index1Size;}
    long getIndex2Size()  const {return index2Size;}
	long getIndex3Size()  const {return index3Size;}

//
//###################################################################
//                     Array Operators
//###################################################################
//

DoubleArray3D operator+(const DoubleArray3D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	#endif
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] += D.dataPtr[i];
    }
    return R;
}

DoubleArray3D operator+(double a)				// Added by Igor
{
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] += a;
    }
    return R;
}

DoubleArray3D operator-(double a)				// Added by Igor
{
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] -= a;
    }
    return R;
}

DoubleArray3D operator-(const DoubleArray3D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	#endif
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] -= D.dataPtr[i];
    }
    return R;
}

DoubleArray3D operator*(double alpha)
{
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}

friend DoubleArray3D operator*(double alpha, const DoubleArray3D& D)
{
    DoubleArray3D R(D);
    long i;
    for(i = 0; i < D.index1Size*D.index2Size*D.index3Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}
                                                               ///<p>
DoubleArray3D operator/(double alpha)
{
    DoubleArray3D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {
    R.dataPtr[i] /= alpha;
    }
    return R;
}

void operator=(const DoubleArray3D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
		sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	}
	#endif

	if(index1Size*index2Size*index3Size == 0)
	{
    initialize(D.index1Size,D.index2Size,D.index3Size);
    
    index1Size    = D.index1Size;
    index2Size    = D.index2Size;
	index3Size    = D.index3Size;
    index1Begin   = D.index1Begin;
    index2Begin   = D.index2Begin;
	index3Begin   = D.index3Begin;
    index1End     = D.index1End;
    index2End     = D.index2End;
	index3End     = D.index3End;
    }

    long i;
    for(i = 0; i < D.index1Size*D.index2Size*D.index3Size; i++)
    {dataPtr[i] = D.dataPtr[i];}
}

void operator*=(double alpha)
{
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {dataPtr[i] *= alpha;}
}

void operator+=(const DoubleArray3D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	}
	#endif
	if(index1Size*index2Size*index3Size == 0)
	{initialize(D.index1Size,D.index2Size,D.index3Size);}

    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {dataPtr[i] += D.dataPtr[i];}
}
                                                               ///<p>
void operator-=(const DoubleArray3D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	}
	#endif
	if(index1Size*index2Size*index3Size == 0)
	{initialize(D.index1Size,D.index2Size,D.index3Size);}

    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {dataPtr[i] -= D.dataPtr[i];}
}

void setToValue(double d)
{
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {dataPtr[i] = d;}
}

double dot(const DoubleArray3D& D) const
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size, this->index3Size,D.index3Size);
	#endif

    double R;
    R  = 0;
    long i;
    for(i = 0; i < index1Size*index2Size*index3Size; i++)
    {R += dataPtr[i]*D.dataPtr[i];}
    return R;
}

	//###################################################################
	//
	//	The output 'friend' routines below were added by Igor.
	//
	//###################################################################

	//  Input/Output
	//
	//  Prints out values as as if they were in the first Cartesian 
	//  quadrant --- not in matrix indexing. 
	//
	//

	friend ostream&  operator <<(ostream& outStream, const DoubleArray3D& A)
	{
		long i,j,k;
		for( k = A.index3Begin; k <= A.index3End; k++ )
		{
			for( j = A.index2End; j >=  A.index2Begin; j-- )
			{
				for( i = A.index1Begin; i <=  A.index1End; i++ )
				{
					outStream <<  setw(5) << A(i,j,k) << " ";
				}
				outStream << endl;
			}
			outStream << endl;
		}
		return outStream;
	}

//###################################################################
	friend void readImage_noheading( DoubleArray3D& A, string filename )
	{
		ifstream infile( filename.c_str() );
		if(!infile)
		{
			cout << "Error reading " << filename<< "!!!" << endl;
			exit(-1);
		}

		// long m; long n; long p;

		// infile >> m >> n >> p;		// Read the dimension of the DAT image

		long i;  long j;  long k;
		for(k = A.index3Begin; k <= A.index3End; k++)
		{
			for(j = A.index2Begin; j <= A.index2End; j++)
			{
				for(i = A.index1Begin; i <= A.index1End; i++)
				{
					infile >> A(i,j,k);
				}
			}
		}
		infile.close();
	}

//###################################################################

	friend void writeDoubleImage_noheading( const DoubleArray3D& A, string filename )
	{
		ofstream outfile( filename.c_str() );
		if(!outfile)
		{
			cout << "Error opening  " << filename << "!!!" << endl;
			exit(-1);
		}

		// outfile << A.index1Size << " " << A.index2Size << " " << A.index3Size << endl;

		long i; long j; long k;
		for(k = A.index3Begin; k <= A.index3End; k++)
		{
			for(j = A.index2Begin; j <= A.index2End; j++)
			{
				for(i = A.index1Begin; i <= A.index1End; i++)
				{
					outfile <<  setw(5) << A(i,j,k) << " ";
				}
				outfile << endl;
			}
		}
		outfile.close();
	}

//###################################################################

	friend void writeLongImage_noheading( const DoubleArray3D& A, string filename )
	{
		ofstream outfile( filename.c_str() );
		if(!outfile)
		{
			cout << "Error opening  " << filename << "!!!" << endl;
			exit(-1);
		}

		// outfile << A.index1Size << " " << A.index2Size << " " << A.index3Size << endl;

		long i; long j; long k;
		for(k = A.index3Begin; k <= A.index3End; k++)
		{
			for(j = A.index2Begin; j <= A.index2End; j++)
			{
				for(i = A.index1Begin; i <= A.index1End; i++)
				{
					outfile <<  setw(5) << (long)A(i,j,k) << " ";
				}
				outfile << endl;
			}
		}
		outfile.close();
	}

//
//###################################################################
//                      Class Data Members
//###################################################################
//
    protected :

    double*      dataPtr;     // data pointer
    long     index1Begin;     // coordinate 1 starting index
    long     index2Begin;     // coordinate 2 starting index
	long     index3Begin;
    long       index1End;     // coordinate 1 ending index
    long       index2End;     // coordinate 2 ending index
	long       index3End;
    long      index1Size;     // coordinate 1 size
    long      index2Size;     // coordinate 2 size
	long      index3Size;

//
//###################################################################
//                      Bounds Checking
//###################################################################
//

#ifdef _DEBUG 
    static void boundsCheck(long i, long begin, long end, int coordinate)
    {
    if((i < begin)||(i  > end))
    {
    printf("Array index %d out of bounds \n",coordinate);
    printf("Offending index value %d : Acceptable Range [%d, %d] \n",i, begin, end);
    }}
#else
static void boundsCheck(long, long, long, int){}
#endif


#ifdef _DEBUG 
    static void sizeCheck(long Msize1, long Msize2, long Nsize1, long Nsize2, long Psize1, long Psize2)
    {
    if(Msize1 != Msize2)
    {
		printf("1st Dimension Sizes Are Incompatable  %d != %d \n" , Msize1, Msize2);
    }
	if(Nsize1 != Nsize2)
    {
		printf("2nd Dimension Sizes Are Incompatable  %d != %d \n" , Nsize1, Nsize2);
    }
	if(Psize1 != Psize2)
    {
		printf("3nd Dimension Sizes Are Incompatable  %d != %d \n" , Psize1, Psize2);
    }
    }
#else
static void sizeCheck(long, long, long, long, long, long){}
#endif
};

#endif


