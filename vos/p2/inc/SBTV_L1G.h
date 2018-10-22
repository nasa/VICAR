//
//#####################################################################
// Igor Yanovsky (C) JPL
// original: 09/19/2011
// updated:  08/14/2017
//#####################################################################
//
// Split Bregrmam Method for Total Variation Denoising
// with L1-norm fidelity.
//
// Grayscale image additive noise problem:
//               f = u + n
// where n is inpulse noise.
// Alternating Minimization method solves
//             min_u TV(u) + mu ||u - f||_1.
// where 
//             TV(u) = \sum_i ||D_i u||.
// 
// Minimization problem:
//
// min_{u,d,z}  sum_i {||d_i|| + lambda/2 ||d_i - D_i u - b_i||^2} 
//			    + mu ( ||z||_1 + gamma/2 ||z - (u - f) - w||_2^2 )
//
// Refer to Igor Yanovsky, Split-Bregman TV-L1 Denoising, Report, 2017.
//
//#####################################################################
//

#ifndef __SBTV_L1G__
#define __SBTV_L1G__

#include <vector>

#include "DoubleArray2D.h"
#include "Grid2D.h"

typedef vector<  double   > vector1Ddouble;

enum {L0,L1,L2};


class SBTV_L1G
{
private:
			   
	void get_d( DoubleArray2D& d1, DoubleArray2D& d2,
				const DoubleArray2D& u,
			    const DoubleArray2D& b1, const DoubleArray2D& b2,
			    const Grid2D& grid );
	
	void get_z( DoubleArray2D& z,
				const DoubleArray2D& u,
				const DoubleArray2D& f,
                const DoubleArray2D& w,
			    const Grid2D& grid );
    
	void get_b1( DoubleArray2D& b1,
				const DoubleArray2D& u,
				const DoubleArray2D& d1,
				const Grid2D& grid );
	
	void get_b2( DoubleArray2D& b2,
				const DoubleArray2D& u,
				const DoubleArray2D& d2,
				const Grid2D& grid );
    
    void get_w( DoubleArray2D& w,
                const DoubleArray2D& u,
                const DoubleArray2D& f,
                const DoubleArray2D& z,
                const Grid2D& grid );
	
	vector1Ddouble energy;
	vector1Ddouble egrad;
	vector1Ddouble efid;

				
public:

	double mu;			// weight of fidelity
	double lambda;		// penalty parameter
	double gamma;		
	
	long nIter;			// number of iterations
	long outputCount;	// output every outputCount iterations
	
	double noise_number;	
	double epsilon;
	
	vector1Ddouble rmse;
	vector1Ddouble snr;
	
	void TV_L1G( DoubleArray2D& u, DoubleArray2D& f,
				 DoubleArray2D& img,
				 const Grid2D& grid );
				
	void calculateEnergy( const DoubleArray2D& u,
				const DoubleArray2D& f,
				const Grid2D& grid );

	void outputParameters( const Grid2D& grid );
};

#endif
