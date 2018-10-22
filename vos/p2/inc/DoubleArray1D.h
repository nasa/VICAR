#ifndef __DoubleArray1D__
#define __DoubleArray1D__

#include <iostream>
#include <iomanip>
using namespace std;

#ifdef  _DEBUG 
#include <stdio.h>
#endif
//
//####################################################################
//                    DoubleArray1D.h  
//####################################################################
/**
   
   Provides a "light weight" one dimensional array structure 
   with initialization capabilities, array operations, and 
   optional bounds checking.

   <pre>
  
   The beginning index default is 0                : (C convention)
   Access using (*), e.g. A(i) for ith element.    : (Not C convention)
   </pre>

   The copy constructor creates a duplicate instance. Deleting the copy 
   will not delete the data of the original.<p> 

   Created for use in Math 270E and 270C<p>

   ***Fixes***
   Fixed assignment operator so that indexing information is not
   overwritten. Made dot const correct. CRA 01/21/03 <p>
   
   Added ostream support. CRA 01/21/03 <p>

<i>Source</i>: 
<A HREF="../DoubleArray1D.h">DoubleArray1D.h</A><p>

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
class DoubleArray1D 
{
public :
//
//###################################################################
//                 Constructors/Initialization
//###################################################################
//
    DoubleArray1D()
    {
    dataPtr       = 0;
    index1Size    = 0;
    index1Begin   = 0;
    index1End     = 0;
    }

    DoubleArray1D(long m)
    {
     dataPtr       = 0;
    index1Size    = 0;
    index1Begin   = 0;
    index1End     = 0;
	initialize(m);
	};                                                       

    DoubleArray1D(double* d, long m)
    {
    dataPtr       = 0;
    index1Size    = 0;
    index1Begin   = 0;
    index1End     = 0;
	initialize(d,m);
	};

    DoubleArray1D(const DoubleArray1D& D)
    {
    index1Size    = D.index1Size;
    index1Begin   = D.index1Begin;
    index1End     = D.index1End;
    dataPtr       = new double[index1Size];
	long i;
	for(i = 0; i < index1Size; i++){dataPtr[i] = D.dataPtr[i];}
    };                                                          ///<p>

    virtual ~DoubleArray1D()
    {
    if( dataPtr != 0) delete [] dataPtr;
    }

    void initialize(long m)
    {

    if(index1Size != m)
    {
       delete [] dataPtr;
       dataPtr = new double[m];
    }
    index1Size    = m;
    index1Begin   = 0;
    index1End     = index1Begin + (index1Size - 1); 

	long i;
	for(i = 0; i < index1Size; i++)
    {dataPtr[i] = 0.0;}                                     
    };                                                      ///<p>

    void initialize(double* d, long m)
    {

    initialize(m);

	long i;
	for(i = 0; i < index1Size; i++)
	{dataPtr[i] = d[i];}
    };
//
//###################################################################
//                  Element Access 
//###################################################################
//

#ifdef _DEBUG 
    double&  operator()(long i1)
    {
    boundsCheck(i1, index1Begin, index1End,1);
    return *(dataPtr +  (i1 - index1Begin));
    };

    const double&  operator()(long i1) const
    {
    boundsCheck(i1, index1Begin, index1End,1);
    return *(dataPtr +  (i1 - index1Begin));
    };                                                     
#else

    inline double&  operator()(long i1)
    {
    return *(dataPtr + (i1 - index1Begin));
    };                                                       ///<p>

    inline const double&  operator()(long i1) const
    {
    return *(dataPtr +   (i1 - index1Begin));
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

    long getIndex1Begin() const {return index1Begin;}
    long getIndex1End()   const {return index1End;}
    long getIndex1Size()  const {return index1Size;}

//
//  Get/Set specifically for one dimensional arrays
//
    void setIndexBegin(long i) 
    {index1Begin = i; index1End   = index1Begin + (index1Size - 1);};

    long getIndexBegin() const {return index1Begin;}
    long getIndexEnd()   const {return index1End;}
    long getSize()       const {return index1Size;}              ///<p>

	//
    // Resizes array to exactly newSize
    //
    void resize(long newSize)
	{
	long i;
    double*  newDataPtr = new double[newSize];
	double*  tmpDataPtr;

	if(newSize > index1Size) 
	{
		for(i = 0; i < index1Size; i++) newDataPtr[i] = dataPtr[i];
	}
	else
	{
		for(i = 0; i < newSize; i++) newDataPtr[i] = dataPtr[i];
	}

	index1Size = newSize;
	tmpDataPtr = dataPtr;
	dataPtr    = newDataPtr;

	if(tmpDataPtr != 0) delete [] tmpDataPtr;
	index1End   = index1Begin + (index1Size - 1);
	}



//
//###################################################################
//                     Array Operators
//###################################################################
//
DoubleArray1D operator+(const DoubleArray1D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size);
	#endif
    DoubleArray1D R(*this);
    long i;
    for(i = 0; i < index1Size; i++)
    {
    R.dataPtr[i] += D.dataPtr[i];
    }
    return R;
}

DoubleArray1D operator-(const DoubleArray1D& D)
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,D.index1Size);
	#endif
    DoubleArray1D R(*this);
    long i;
    for(i = 0; i < index1Size; i++)
    {
    R.dataPtr[i] -= D.dataPtr[i];
    }
    return R;
}

DoubleArray1D operator*(double alpha)
{
    DoubleArray1D R(*this);
    long i;
    for(i = 0; i < index1Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}

friend DoubleArray1D operator*(double alpha, const DoubleArray1D& D)
{
    DoubleArray1D R(D);
    long i;
    for(i = 0; i < D.index1Size; i++)
    {
    R.dataPtr[i] *= alpha;
    }
    return R;
}
                                                                ///<p>
DoubleArray1D operator/(double alpha)
{
    DoubleArray1D R(*this);
    long i;
    for(i = 0; i < index1Size; i++)
    {
    R.dataPtr[i] /= alpha;
    }
    return R;
}
//
//###################################################################
//          
//###################################################################
//
void operator=(const DoubleArray1D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size);
	}
	#endif
//
//  If null instance, then initialize and acquire indexing
//  from right hand side.
//
    if(index1Size == 0)
    {
    initialize(D.index1Size);
    index1Size    = D.index1Size;
    index1Begin   = D.index1Begin;
    index1End     = D.index1End;
    }
//
//  copy over the data
//
    long i;
    for(i = 0; i < D.index1Size; i++)
    {dataPtr[i] = D.dataPtr[i];}
}

void operator*=(double alpha)
{
    long i;
    for(i = 0; i < index1Size; i++)
    {dataPtr[i] *= alpha;}
}

void operator+=(const DoubleArray1D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size);
	}
	#endif

    long i;
    for(i = 0; i < D.index1Size; i++)
    {dataPtr[i] += D.dataPtr[i];}
}                                                            ///<p>

void operator-=(const DoubleArray1D& D)
{
	#ifdef _DEBUG 
	if(index1Size != 0)
	{
    sizeCheck(this->index1Size,D.index1Size);
	}
	#endif

    long i;
    for(i = 0; i < D.index1Size; i++)
    {dataPtr[i] -= D.dataPtr[i];}
}

void setToValue(double d)
{
    long i;
    for(i = 0; i < index1Size; i++)
    {
     dataPtr[i] = d;
    }
}

double dot(const DoubleArray1D& B) const
{
	#ifdef _DEBUG 
    sizeCheck(this->index1Size,B.index1Size);
	#endif

    double R;
    R  = 0;
    long i;
    for(i = 0; i < index1Size; i++)
    {R += dataPtr[i]*B.dataPtr[i];}
    return R;
}

//
//  Output
//
friend ostream& operator<<(ostream& outStream, const DoubleArray1D& V)
{

    long i; 
    for(i = 0; i <  V.index1Size; i++)
    { 
      outStream <<  setw(5) << V.dataPtr[i] << " ";
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
    long    index1Begin;      // coordinate 1 starting index
    long    index1End;        // coordinate 1 ending index
    long    index1Size;       // coordinate 1 size

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
    }
    }
#else
static void boundsCheck(long, long, long, int){}
#endif

#ifdef _DEBUG 
    static void sizeCheck(long size1, long size2)
    {
    if(size1 != size2)
    {
    printf("Array Sizes Are Incompatable %d != %d \n", size1, size2);
    }
    }
#else
static void sizeCheck(long, long){}
#endif
};

#endif


