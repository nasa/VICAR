//
//#####################################################################
// Igor Yanovsky (C) JPL
// version:  10/11/2017
//#####################################################################
//
// Split Bregrmam Method for Total Variation Denoising
// with L1-norm fidelity.
//
// Grayscale image additive noise problem:
//               f = u + n
// where n is inpulse noise.
// Alternating Minimization method solves
//             min_u TV(u) + mu ||u - f||_1,
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
#include "zvproto.h"
#include <math.h>
#include <iostream>
#include <sstream>
using namespace std;

#include <string.h>

#include "SBTV_L1G.h"
#include "BC_2D.h"
#include "Convolution2D.h"
#include "common_routines.h"
#include "FileIO_2D.h"

#define PI 4.0*atan2(1.0,1.0)

//#####################################################################
//				  TV-L1 Denoising via Normal Equations
//#####################################################################

void SBTV_L1G::TV_L1G( DoubleArray2D& u, DoubleArray2D& f,
					   DoubleArray2D& img,
					   const Grid2D& grid )
{
	FileIO_2D IO;
	
	long m = grid.m;
	long n = grid.n;
	
	DoubleArray2D d1(m,n);
	DoubleArray2D d2(m,n);
	
	DoubleArray2D b1(m,n);
	DoubleArray2D b2(m,n);
    
	DoubleArray2D z(m,n);
    
    DoubleArray2D w(m,n);
			
	// fft2(f)
	DoubleArray2D fftF_r(m,n), fftF_i(m,n);		
	fft2( f, fftF_r, fftF_i );
			
	DoubleArray2D fftDtD(m,n);
									
	// fft2(z)
	DoubleArray2D fftZ_r(m,n), fftZ_i(m,n);		
			
    // fft2(w)
	DoubleArray2D fftW_r(m,n), fftW_i(m,n);
    
	long i,j;
						
	// Finite difference matrices: D1, D2
	DoubleArray2D D1(1,2);
	DoubleArray2D D2(2,1);
	
	D1(0,0) = -1.0;
	D1(0,1) =  1.0;
	D2(0,0) = -1.0;
	D2(1,0) =  1.0;
	
	// fft(D1), fft(D2)
	DoubleArray2D fftD1_r, fftD1_i;		// fft2(D1)
	DoubleArray2D fftD2_r, fftD2_i;		// fft2(D2)
	
	psf2otf(D1, fftD1_r, fftD1_i, m, n, false);		// [m,n] is a size of f
	psf2otf(D2, fftD2_r, fftD2_i, m, n, false);
	
	// fftDtD = |fft(D1)|.^2 + |fft(D2)|.^2
	double term1, term2;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			term1 = fftD1_r(i,j)*fftD1_r(i,j) + fftD1_i(i,j)*fftD1_i(i,j);
			term2 = fftD2_r(i,j)*fftD2_r(i,j) + fftD2_i(i,j)*fftD2_i(i,j);
			 
			fftDtD(i,j) = term1 + term2;
		}
	}
	
	double lam_gamma;	// lambda/gamma
	
	DoubleArray2D Denom(m,n);
	
	DoubleArray2D d1x(m,n), d2y(m,n);
	DoubleArray2D d1x_d2y(m,n);
	
	DoubleArray2D b1x(m,n), b2y(m,n);
	DoubleArray2D b1x_b2y(m,n);
	
	DoubleArray2D fftu_r(m,n), fftu_i(m,n);
	DoubleArray2D fft_d1x_d2y_r(m,n), fft_d1x_d2y_i(m,n);
	DoubleArray2D fft_b1x_b2y_r(m,n), fft_b1x_b2y_i(m,n);
	
	long k;
	long kkk;
	for( kkk=1; kkk <= 1; kkk++ )		// while( lambda < LAMBDA_MAX )
	{
		lam_gamma = lambda / gamma;
	
		Denom = lam_gamma*fftDtD + 1.0;  // since fft(Identity) = 1.
	
		for(k=1; k <= nIter; k++)		// Alternating Minimization iterations
		{
			// Get d:
			get_d( d1, d2, u, b1, b2, grid );
			
			// Get b1, b2:
			//if( k%10 == 0 )
			//{
				cout << endl << "lambda_" << kkk << " = " << lambda << ", iter = " << k << endl << endl;
				get_b1( b1, u, d1, grid );
				get_b2( b2, u, d2, grid );
			//}
							
			// Periodic BC:
			for(j=0; j < n; j++)
			{
				for(i=1; i < m; i++)
				{
					d1x(i,j) = d1(i,j) - d1(i-1,j);
					b1x(i,j) = b1(i,j) - b1(i-1,j);
				}
				i = 0;
				d1x(i,j) =  d1(i,j) - d1(m-1,j);
				b1x(i,j) =  b1(i,j) - b1(m-1,j);
			}
			for(i=0; i < m; i++)
			{
				for(j=1; j < n; j++)
				{
					d2y(i,j) = d2(i,j) - d2(i,j-1);
					b2y(i,j) = b2(i,j) - b2(i,j-1);
				}
				j = 0;
				d2y(i,j) = d2(i,j) - d2(i,n-1);
				b2y(i,j) = b2(i,j) - b2(i,n-1);
			}
		
			d1x_d2y = d1x + d2y;
			fft2( d1x_d2y, fft_d1x_d2y_r, fft_d1x_d2y_i );
			
			b1x_b2y = b1x + b2y;
			fft2( b1x_b2y, fft_b1x_b2y_r, fft_b1x_b2y_i );
			
			
			// Get z:
			get_z( z, u, f, w, grid );
						
			// fft2(z)
			fft2( z, fftZ_r, fftZ_i );
            
            // Get w:
//            if( k%10 == 0 )
//			{
                get_w( w, u, f, z, grid );
//			}
            
            // fft2(w)
			fft2( w, fftW_r, fftW_i );
			
			for(j=0; j < n; j++)
			{
				for(i=0; i < m; i++)
				{
					fftu_r(i,j) = (fftF_r(i,j) + fftZ_r(i,j) - fftW_r(i,j) - lam_gamma*(fft_d1x_d2y_r(i,j) - fft_b1x_b2y_r(i,j))  ) /Denom(i,j);
					fftu_i(i,j) = (fftF_i(i,j) + fftZ_i(i,j) - fftW_i(i,j) - lam_gamma*(fft_d1x_d2y_i(i,j) - fft_b1x_b2y_i(i,j))  ) /Denom(i,j);
				}
			}
		
			u = ifft2(fftu_r, fftu_i);
									
			calculateEnergy( u, f, grid );
			rmse.push_back( calculateRMSE( img, u) );
			snr.push_back( calculateSNR( img, u) );
		} // end for
		
		//IO.writePGM( u, "u", kkk );
		//IO.writePGM( f-u+127, "v", kkk );
		//IO.writePGM( img-u+127, "e", kkk );
        
        //IO.writePGM( b1, "b1_", kkk );
        //IO.writePGM( b2, "b2_", kkk );
        //IO.writePGM( w,  "w", kkk );
		
		//IO.write_ascii( energy, "energy"   );
		//IO.write_ascii( egrad,  "grad"     );
		//IO.write_ascii( efid,   "fidelity" );
		//IO.write_ascii( rmse,   "rmse"     );
		//IO.write_ascii( snr,    "snr"      );
		
		//cout << endl << endl << "*** VERY TEMP - no lambda and gamma increase *** " << endl << endl;
		//cout << "lambda = " << lambda << endl;
		//cout << "gamma = " << gamma << endl;
		lambda = lambda * pow(2.0, 2.0/3.0);
		gamma = gamma * 2.0;
		
		
	}	// end while
	
}

// 
//#####################################################################
// 	       Calculate z using one-dimensional shrinkage formula
//#####################################################################
//


void SBTV_L1G::get_z( DoubleArray2D& z,
					  const DoubleArray2D& u,
					  const DoubleArray2D& f,
                      const DoubleArray2D& w,
					  const Grid2D& grid )
{
	// z = max( |u-f+w| - tau/lambda , 0 ) * sign(u-f+w)

	//muOVERgamma = mu/gamma;
	//ufw = u - f + w;
	//Z = sign(ufw).*max(0, abs(ufw) - muOVERgamma);
	
	long m = grid.m;
	long n = grid.n;
	
	double mu_gamma = mu / gamma;

	DoubleArray2D u_temp = u;

	DoubleArray2D ufw = u_temp - f + w;
	
	double sgn;
	long i,j;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			sgn = ufw(i,j) / (fabs(ufw(i,j)) + 1e-8);
			z(i,j) = sgn * max(0.0, fabs(ufw(i,j))-mu_gamma);
		}
	}
	
}

// 
//#####################################################################
// 	       Calculate d using generalized shrinkage formula
//#####################################################################
//

void SBTV_L1G::get_d( DoubleArray2D& d1, DoubleArray2D& d2,
					  const DoubleArray2D& u,
					  const DoubleArray2D& b1, const DoubleArray2D& b2,
					  const Grid2D& grid )
{	
	// For deconvolution, use periodic BC
		
	long m = grid.m;
	long n = grid.n;
	
	double a, b, s, s2;
	double C = 1/(lambda);
	double C2 = C*C;

	long i, j;
	for(j=0; j < n-1; j++)
	{
		for(i=0; i < m-1; i++)
		{
			a = u(i+1,j) - u(i,j) + b1(i,j);
			b = u(i,j+1) - u(i,j) + b2(i,j);
			
			s2 = a*a + b*b;
			
			if( s2 < C2 )
			{
				d1(i,j) = 0.0;
				d2(i,j) = 0.0;
			}
			else
			{
				s = sqrt(s2);
				s = (s-C)/s;
				d1(i,j) = a*s;
				d2(i,j) = b*s;
			}
		}
	}
	
	// Periodic BC:	u(m,j) = u(0,j)
	//				u(i,n) = u(i,0)
	j = n-1;
	for(i=0; i < m-1; i++)
	{
		a = u(i+1,j) - u(i,j) + b1(i,j);
		b = u(i,0)   - u(i,j) + b2(i,j);
		
		s2 = a*a + b*b;
		
		if( s2 < C2 )
		{
			d1(i,j) = 0.0;
			d2(i,j) = 0.0;
		}
		else
		{
			s = sqrt(s2);
			s = (s-C)/s;
			d1(i,j) = a*s;
			d2(i,j) = b*s;
		}
	}
	
	i = m-1;
	for(j=0; j < n-1; j++)
	{
		a = u(0,j)   - u(i,j) + b1(i,j);
		b = u(i,j+1) - u(i,j) + b2(i,j);
			
		s2 = a*a + b*b;
			
		if( s2 < C2 )
		{
			d1(i,j) = 0.0;
			d2(i,j) = 0.0;
		}
		else
		{
			s = sqrt(s2);
			s = (s-C)/s;
			d1(i,j) = a*s;
			d2(i,j) = b*s;
		}
	}
	
	i = m-1; j = n-1;	// Corner point
	a = u(0,j)   - u(i,j) + b1(i,j);
	b = u(i,0)   - u(i,j) + b2(i,j);
			
	s2 = a*a + b*b;
			
	if( s2 < C2 )
	{
		d1(i,j) = 0.0;
		d2(i,j) = 0.0;
	}
	else
	{
		s = sqrt(s2);
		s = (s-C)/s;
		d1(i,j) = a*s;
		d2(i,j) = b*s;
	}
	
}

//#####################################################################

void SBTV_L1G::get_b1( DoubleArray2D& b1,
						  const DoubleArray2D& u,
						  const DoubleArray2D& d1,
						  const Grid2D& grid )
{
	long m = grid.m;
	long n = grid.n;
	
	long i, j;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m-1; i++)
		{
			b1(i,j) = b1(i,j) + u(i+1,j) - u(i,j) - d1(i,j);
		}
	}
	
}

//#####################################################################

void SBTV_L1G::get_b2( DoubleArray2D& b2,
						  const DoubleArray2D& u,
						  const DoubleArray2D& d2,
						  const Grid2D& grid )
{
	long m = grid.m;
	long n = grid.n;
	
	long i, j;
	for(j=0; j < n-1; j++)
	{
		for(i=0; i < m; i++)
		{
			b2(i,j) = b2(i,j) + u(i,j+1) - u(i,j) - d2(i,j);
		}
	}
	
}

//#####################################################################

void SBTV_L1G::get_w( DoubleArray2D& w,
                      const DoubleArray2D& u,
                      const DoubleArray2D& f,
                      const DoubleArray2D& z,
                      const Grid2D& grid )
{
	long m = grid.m;
	long n = grid.n;
	
	long i, j;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			w(i,j) = w(i,j) + u(i,j) - f(i,j) - z(i,j);
		}
	}
	
}

//
//#####################################################################
//

void SBTV_L1G::calculateEnergy( const DoubleArray2D& u,
							 const DoubleArray2D& f,
							 const Grid2D& grid )
{
        char msg[256];

	long   m  = grid.m;
	long   n  = grid.n;
	double dx = grid.dx;
	double dy = grid.dy;

	double du_x0, du_y0;
	
	//DoubleArray2D K_u = u;
	
	//convolution2D( K_u, Ks );	// K_u = K*u

	double grad = 0.0, fid_L1 = 0.0;
	
	long i, j;
	
	for(j=1; j < n-1; j++)
	{
		for(i=1; i < m-1; i++)
		{
			du_x0 = (u(i+1,j) - u(i-1,j))/2.0;
			du_y0 = (u(i,j+1) - u(i,j-1))/2.0;
			
			grad = grad + sqrt(du_x0*du_x0 + du_y0*du_y0);
			
			fid_L1 = fid_L1 + fabs( u(i,j) - f(i,j) );
		}
	}

	grad   = grad   /(m-2)/(n-2);
	fid_L1 = fid_L1 /(m-2)/(n-2);
	
	egrad.push_back( grad );
	efid.push_back( fid_L1 );
	energy.push_back( grad + mu*fid_L1 );

        sprintf(msg, "grad = %f   fid = %f   E = %f   ", grad, mu*fid_L1, grad + mu*fid_L1);
        zvmessage(msg, "");
	//cout << "grad = " << grad << "   fid = " << mu*fid_L1 << "  E = " << grad + mu*fid_L1 << "  ";
}

//
//#####################################################################
//

void SBTV_L1G::outputParameters( const Grid2D& grid )
{
    ostringstream outs;
    char fileName[256];
    outs.str("");

	outs << "parameters_SBTV_L1G.dat";

	strcpy(fileName,(outs.str()).c_str());
    //strcpy_s( fileName, 256, (outs.str()).c_str() );

	FILE* dataFile;

	if( (dataFile = fopen(fileName, "w+" )) == NULL )
    //if( fopen_s( &dataFile, fileName, "w+" ) != 0 )
    {
		printf( "The file %s could not be  opened\n",fileName);
		return;
    }

	fprintf(dataFile, "%-10.5e \n", double(grid.m) );
	fprintf(dataFile, "%-10.5e \n", double(grid.n) );

	fprintf(dataFile, "%-10.5e \n", noise_number );
	
	fprintf(dataFile, "%-10.5e \n", mu );
	fprintf(dataFile, "%-10.5e \n", lambda );
	fprintf(dataFile, "%-10.5e \n", gamma );
	
	fprintf(dataFile, "%-10.5e \n", double(nIter) );
	fprintf(dataFile, "%-10.5e \n", double(outputCount) );

	fclose(dataFile);
}
