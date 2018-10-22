//
//#####################################################################
// Igor Yanovsky (C) JPL
// version: 06/13/2013
//#####################################################################


#ifndef __VectorImage__
#define __VectorImage__


#include "DoubleArray2D.h"
#include "DefineNumChannels.h"


class VectorImage
{
public :

	// Contains the channels of a vector-valued image:
	DoubleArray2D ch[numCh];

	void initialize( long m, long n )
	{
		long k;
		for( k = 0; k < numCh; k ++)
		{
			ch[k].initialize(m,n);
		}
	}
	
	//#####################################################################
	
	//virtual void getImageInfo( long& m, long& n ) = 0; // no implementation
	
	//#####################################################################
	
	VectorImage operator+(const VectorImage& D)
	{
		VectorImage R(*this);
		
		long kk;
		for (kk=0; kk < numCh; kk++)
		{
			R.ch[kk] = R.ch[kk] + D.ch[kk];
		}
		return R;
	}
	
	//#####################################################################
	
	VectorImage operator-(const VectorImage& D)
	{
		VectorImage R(*this);
		
		long kk;
		for (kk=0; kk < numCh; kk++)
		{
			R.ch[kk] = R.ch[kk] - D.ch[kk];
		}
		return R;
	}
	
	//#####################################################################
	
	VectorImage operator+(double a)
	{
		VectorImage R(*this);
		
		long kk;
		for (kk=0; kk < numCh; kk++)
		{
			R.ch[kk] = R.ch[kk] + a;
		}
		return R;
	}
	
	//#####################################################################
	
	VectorImage operator-(double a)				// Added by Igor
	{
		VectorImage R(*this);
		
		long kk;
		for (kk=0; kk < numCh; kk++)
		{
			R.ch[kk] = R.ch[kk] - a;
		}
		return R;
    }
    
    //#####################################################################

    VectorImage operator*(double alpha)
    {
        VectorImage R(*this);
        
        long kk;
        for (kk=0; kk < numCh; kk++)
		{
			R.ch[kk] = R.ch[kk] * alpha;
		}
        return R;
    }
    
    //#####################################################################
    
   friend ostream&  operator <<(ostream& outStream, const VectorImage& A)
    {
        long i; long j;
        long kk;
		for (kk=0; kk < numCh; kk++)
        {
            for(j = A.ch[0].getIndex2End(); j >= A.ch[0].getIndex2Begin(); j--)
            {
                for(i = A.ch[0].getIndex1Begin(); i <= A.ch[0].getIndex1End(); i++)
                {
                    outStream <<  setw(5) << A.ch[kk](i,j) << " ";
                }
                outStream << endl;
            }
            outStream << endl;
        }
        return outStream;
    }

};

#endif
