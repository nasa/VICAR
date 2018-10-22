#ifndef __DoubleArray2D__
#define __DoubleArray2D__

#define _DEBUG

#include <iostream>
#include <iomanip>
using namespace std;

#include <stdio.h>

//
//####################################################################
//                    DoubleArray2D.h  
//####################################################################
/**
   Provides a "light weight" two dimensional array structure 
   with initialization capabilities, algebraic operations, 
   and optional bounds checking. 

   <pre>
   The beginning index default is 0                    : (C convention)
   Data for the array is assumed to be stored by ROWS  : (C convention)
   Access using (*,*), e.g. A(i,j) for (i,j)th element.: (NOT C convention) 
   </pre>

   The copy constructor creates a duplicate instance. Deleting the copy 
   will not delete the data of the original. <p>
   Created for use in Math 270E and Math 270C<p>

      ***Fixes***
   Fixed assignment operator so that indexing information is not
   overwritten. Made dot const correct. CRA 01/21/03 <p>
 
   Added iostream support. CRA 01/21/03 <p>

<i>Source</i>: 
<A HREF="../DoubleArray2D.h">DoubleArray2D.h</A><p>

@author Chris Anderson (C) UCLA 
@version  May 25, 2000
*/
//#####################################################################
// Chris Anderson (C) UCLA                                April 2, 2000
//#####################################################################
//
/*
#############################################################################
#
# Copyright 2015 Chris Anderson
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# For a copy of the GNU General Public License see
# <http://www.gnu.org/licenses/>.
#
#############################################################################
*/
class DoubleArray2D 
{

public :
//
//###################################################################
//                 Constructors/Initialization
//###################################################################
//
    DoubleArray2D()
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
    index1End     = 0;
    index2End     = 0;
    };


    DoubleArray2D(long m, long n)
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
    index1End     = 0;
    index2End     = 0;
    initialize(m,n);
    };

    DoubleArray2D(double* d, long m, long n)
    {
    dataPtr       = 0;
    index1Size    = 0;
    index2Size    = 0;
    index1Begin   = 0;
    index2Begin   = 0;
    index1End     = 0;
    index2End     = 0;
    initialize(d,m,n);
    };
    
    DoubleArray2D(float* d, long m, long n)            // Brian Rheingans
    {
        dataPtr       = 0;
        index1Size    = 0;
        index2Size    = 0;
        index1Begin   = 0;
        index2Begin   = 0;
        index1End     = 0;
        index2End     = 0;
        initialize(d,m,n);
    };

    DoubleArray2D(const DoubleArray2D& D)
    {    
    index1Size    = D.index1Size;
    index2Size    = D.index2Size;
    index1Begin   = D.index1Begin;
    index2Begin   = D.index2Begin;
    index1End     = D.index1End;
    index2End     = D.index2End;

	dataPtr       = new double[index1Size*index2Size];
	long i;
	for(i = 0; i < index1Size*index2Size; i++) 
	{dataPtr[i] = D.dataPtr[i];}
    };
                                                                ///<p>
    virtual ~DoubleArray2D()
    {
    if(dataPtr !=  0) delete [] dataPtr;
    }

    void initialize(long m, long n)
    {

    if((index1Size != m)||(index2Size != n))
    {
        delete [] dataPtr;
        dataPtr = new double[m*n];
    }
    index1Size    = m;
    index2Size    = n;
    index1Begin   = 0;
    index2Begin   = 0;
    index1End     = index1Begin + (index1Size - 1);
    index2End     = index2Begin + (index2Size - 1);

	long i;
	for(i = 0; i < index1Size*index2Size; i++) 
	{dataPtr[i] = 0.0;}
    };
                                                                ///<p>
    void initialize(double* d, long m, long n)
    {
    initialize(m,n);
	long i;
    for(i = 0; i < index1Size*index2Size; i++) 
	{dataPtr[i] = d[i];}
    };
    
    void initialize(float* d, long m, long n)               // Brian Rheingans
    {
        initialize(m,n);
        long i;
        for(i = 0; i < index1Size*index2Size; i++)
        {dataPtr[i] = d[i];}
    };
    
//
//###################################################################
//                  Element Access 
//###################################################################
//
#ifdef _DEBUG 
    double&  operator()(long i1, long i2)
    {
    boundsCheck(i1, index1Begin, index1End,1);
    boundsCheck(i2, index2Begin, index2End,2);
    return *(dataPtr +  (i2 - index2Begin) + (i1 - index1Begin)*index2Size);
    };

    const double&  operator()(long i1, long i2) const
    {
    boundsCheck(i1, index1Begin, index1End,1);
    boundsCheck(i2, index2Begin, index2End,2);
    return *(dataPtr +  (i2 - index2Begin) + (i1 - index1Begin)*index2Size);
    };
#else
    inline double&  operator()(long i1, long i2)
    {
    return *(dataPtr +  (i2 - index2Begin) + (i1 - index1Begin)*index2Size);
    };
                                                                     ///<p>
    inline const double&  operator()(long i1, long i2) const
    {
    return *(dataPtr +  (i2 - index2Begin) + (i1 - index1Begin)*index2Size);
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

    long getIndex1Begin() const {return index1Begin;}
    long getIndex2Begin() const {return index2Begin;}

    long getIndex1End() const {return index1End;}
    long getIndex2End() const {return index2End;}

    long getIndex1Size()  const {return index1Size;}
                                                               ///<p>
    long getIndex2Size()  const {return index2Size;}

//
//###################################################################
//                     Array Operators
//###################################################################
//

DoubleArray2D operator+(const DoubleArray2D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	#endif
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] += D.dataPtr[i];
    }
    return R;
}

DoubleArray2D operator+(double a)				// Added by Igor
{
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] += a;
    }
    return R;
}

DoubleArray2D operator-(double a)				// Added by Igor
{
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] -= a;
    }
    return R;
}

DoubleArray2D operator-(const DoubleArray2D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	#endif
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] -= D.dataPtr[i];
    }
    return R;
}

DoubleArray2D operator*(double alpha)
{
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}

friend DoubleArray2D operator*(double alpha, const DoubleArray2D& D)
{
    DoubleArray2D R(D);
    long i;
    for(i = 0; i < D.index1Size*D.index2Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}
   
// IGOR
friend DoubleArray2D ComponentwiseProduct(const DoubleArray2D& D1, const DoubleArray2D& D2)
{
    DoubleArray2D R(D1);
	
    long i;
    for(i = 0; i < D1.index1Size*D1.index2Size; i++)
    {
    R.dataPtr[i] *= D2.dataPtr[i];
    }
    return R;
}

// IGOR
friend void ComplexProduct(const DoubleArray2D& rD1, const DoubleArray2D& iD1,
						   const DoubleArray2D& rD2, const DoubleArray2D& iD2,
						   DoubleArray2D& rR, DoubleArray2D& iR)
{
	long i;
    for(i = 0; i < rD1.index1Size*rD1.index2Size; i++)
    {
    rR.dataPtr[i] = rD1.dataPtr[i] * rD2.dataPtr[i] - iD1.dataPtr[i] * iD2.dataPtr[i];
	iR.dataPtr[i] = rD1.dataPtr[i] * iD2.dataPtr[i] + iD1.dataPtr[i] * rD2.dataPtr[i];
    }
}
																														///<p>
DoubleArray2D operator/(double alpha)
{
    DoubleArray2D R(*this);
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {
    R.dataPtr[i] /= alpha;
    }
    return R;
}

void operator=(const DoubleArray2D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	}
	#endif


	if(index1Size*index2Size == 0)
	{
    initialize(D.index1Size,D.index2Size);
    
    index1Size    = D.index1Size;
    index2Size    = D.index2Size;
    index1Begin   = D.index1Begin;
    index2Begin   = D.index2Begin;
    index1End     = D.index1End;
    index2End     = D.index2End;
    }



    long i;
    for(i = 0; i < D.index1Size*D.index2Size; i++)
    {dataPtr[i] = D.dataPtr[i];}
}

void operator*=(double alpha)
{
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {dataPtr[i] *= alpha;}
}

void operator+=(const DoubleArray2D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	}
	#endif
	if(index1Size*index2Size == 0)
	{initialize(D.index1Size,D.index2Size);}

    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {dataPtr[i] += D.dataPtr[i];}
}
                                                               ///<p>
void operator-=(const DoubleArray2D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	}
	#endif
	if(index1Size*index2Size == 0)
	{initialize(D.index1Size,D.index2Size);}

    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {dataPtr[i] -= D.dataPtr[i];}
}

void setToValue(double d)
{
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {dataPtr[i] = d;}
}

double dot(const DoubleArray2D& D) const
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size, this->index2Size,D.index2Size);
	#endif

    double R;
    R  = 0;
    long i;
    for(i = 0; i < index1Size*index2Size; i++)
    {R += dataPtr[i]*D.dataPtr[i];}
    return R;
}

//  Input/Output
//
//  Prints out values as as if they were in the first Cartesian 
//  quadrant --- not in matrix indexing. 
//
//
friend ostream&  operator <<(ostream& outStream, const DoubleArray2D& A)
{
    long i; long j;
    for(j = A.index2End; j >=  A.index2Begin; j--)
    {
    for(i = A.index1Begin; i <=  A.index1End; i++)
    {
      outStream <<  setw(5) << A(i,j) << " ";
    }
      outStream << endl;
    }
    return outStream;


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
    long       index1End;     // coordinate 1 ending index
    long       index2End;     // coordinate 2 ending index
    long      index1Size;     // coordinate 1 size
    long      index2Size;     // coordinate 2 size

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
    static void sizeCheck(long Msize1, long Msize2, long Nsize1, long Nsize2)
    {
    if(Msize1 != Msize2)
    {
    printf("1st Dimension Sizes Are Incompatable  %d != %d \n" , Msize1, Msize2);
    }
	if(Nsize1 != Nsize2)
    {
    printf("2nd Dimension Sizes Are Incompatable  %d != %d \n" , Nsize1, Nsize2);
    }
    }
#else
static void sizeCheck(long, long, long, long){}
#endif
};

#endif


