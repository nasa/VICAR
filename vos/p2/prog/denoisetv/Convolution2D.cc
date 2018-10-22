//
//#####################################################################
//					 Convolution2D.cpp
//#####################################################################
// Igor Yanovsky (C) UCLA, JPL                                
// version:  08/29/2016
//
// Igor Yanovsky, as original author, is contributing this code to JPL 
// with no restrictions.
//
//#####################################################################
//

#include <iostream>
using namespace std;

#include <math.h>
#include "DoubleArray2D.h"
#include "Grid2D.h"
#include "FileIO_2D.h"
#include "common_routines.h"

#include <fftw3.h>

#define PI 4.0*atan2(1.0,1.0)

//#####################################################################

// Shift zero-frequency component to center of spectrum

DoubleArray2D fftshift2D( const DoubleArray2D& A )
{
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	DoubleArray2D B(m,n);

	long i, j;
	for( i = 0; i < m; i++ )
	{
		for( j = 0; j < n; j++ )
		{
			B( (i+m/2)%m, (j+n/2)%n ) = A(i,j);	
			// if m-odd, then m/2 is integer division;
		}
	}

	return B;
}

//#####################################################################

DoubleArray2D circshift2D( const DoubleArray2D& A, const long& p1, const long& p2 )
{
	// Shift array circularly.
	//
	// Similar to Matlab's circshift function.
	// Circularly shifts the values in the array A by shiftsize [p1,p2] elements. 
	// [p1,p2] is a vector of integer scalars specifying the shift amount for 
	// each dimension of array A. If an element in [p1,p2] is positive, the values 
	// of A are shifted down (or to the right). If it is negative, the values of A
	//  are shifted up (or to the left).

	// Calculate the indices that will convert the input matrix to the desired output

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	DoubleArray1D indX(m);
	DoubleArray1D indY(n);
	
	long i,j;
	
	// Calculate shifted indices:
	for( i=0; i<m; i++ )
	{
		indX(i) = (i-p1)%m; // + 1;
	}
	
	// Calculate shifted indices:
	for( j=0; j<n; j++ )
	{
		indY(j) = (j-p2)%n; // + 1;
	}
	
	// Perform the actual conversion by indexing into the input matrix

	DoubleArray2D B(m,n);

	for( i = 0; i < m; i++ )
	{
		for( j = 0; j < n; j++ )
		{
			B(i,j) = A(indX(i),indY(j));
		}
	}

	return B;
}

//#####################################################################

void fft2( const DoubleArray2D& A, DoubleArray2D& fftA_r, DoubleArray2D& fftA_i )
{	
	// Two-dimensional discrete Fourier Transform.
		
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	fftw_complex *inA, *outA;
	fftw_plan p1;

	inA  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outA = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	long i, j;
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	inA[i*n + j][0] = A(i,j);
			inA[i*n + j][1] = 0.0;
		}
	}

	p1 = fftw_plan_dft_2d( m, n, inA, outA, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p1);
	
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	fftA_r(i,j) = outA[i*n + j][0];
			fftA_i(i,j) = outA[i*n + j][1];
		}
	}

	fftw_destroy_plan(p1);
	fftw_free(inA); fftw_free(outA);
}

//#####################################################################

DoubleArray2D ifft2( const DoubleArray2D& A_r, const DoubleArray2D& A_i )
{
	long m = A_r.getIndex1Size();
	long n = A_i.getIndex2Size();
	
	//cout << "m = " << m << ", n = " << n << endl;
	DoubleArray2D ifftA_r(m,n);

	fftw_complex *inA, *outA;
	fftw_plan p1;

	inA  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outA = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	long i, j;
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	inA[i*n + j][0] = A_r(i,j);
			inA[i*n + j][1] = A_i(i,j);
		}
	}

	p1 = fftw_plan_dft_2d( m, n, inA, outA, FFTW_BACKWARD, FFTW_ESTIMATE );
	fftw_execute(p1);

	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	ifftA_r(i,j) = outA[i*n + j][0]/m/n;	// real(ifft(A))
		}
	}

	fftw_destroy_plan(p1);
	fftw_free(inA); fftw_free(outA);
	
	return ifftA_r;
}

//#####################################################################

void psf2otf( const DoubleArray2D& D, 
			  DoubleArray2D& fftD_r, DoubleArray2D& fftD_i, 
			  const long& xx, const long& yy, bool output )
{	
	// Convert point-spread function to optical transfer function.
	
	// Similar to Matlab's psf2otf function
	// Computes the FFT of the PSF array and creates the OTF array 
	// that is not influenced by the PSF off-centering. 
	// Converts the PSF array D of size [m,n] 
	//    into an OTF array DD of size [xx,yy].
	// [xx,yy] should not be smaller than the PSF array size in any dimension.
	
	// To ensure that the OTF is not altered because of PSF off-centering, 
	// psf2otf postpads the PSF array (down or to the right) with 0's to match 
	// dimensions specified in OUTSIZE, then circularly shifts the values of 
	// the PSF array up (or to the left) until the central pixel reaches (0,0) position.
	
	FileIO_2D IO;
	
	long m = D.getIndex1Size();
	long n = D.getIndex2Size();
	
	long padX = xx - m;
	long padY = yy - n;
	
	// Pad the PSF to [xx,yy] 
	DoubleArray2D DD = padarray_post( D, padX, padY );
	
	/*if(output)
	{	IO.writePGMsc( D, "K");
		IO.writePGMsc( DD, "K_padarray_post");
	}*/
	
	fftD_r.initialize(xx,yy);
	fftD_i.initialize(xx,yy);
		
	// Circularly shift otf so that the "center" of the PSF is at the
	// (1,1) element of the array.

	DD = circshift2D( DD, -floor(m/2), -floor(n/2) );
	
	// Compute FFT2(DD):
	fft2( DD, fftD_r, fftD_i );

	/*if(output)
	{	IO.writePGMsc( DD, "K_circshift");
		IO.writePGMsc( fftD_r, "fftK_r");
		IO.writePGMsc( fftD_i, "fftK_i");
	}*/
}

//#####################################################################

void convolution2D( DoubleArray2D& F, const DoubleArray2D& K )
{
	// Input: F is the clean image, K is the PSF
	// Output: F is the convolution (F = K*F)

	long m = K.getIndex1Size();
	long n = K.getIndex2Size();

	fftw_complex *inK, *outK;
	fftw_complex *inF, *outF;
	fftw_complex *fftK_fftF;
	fftw_complex *ifft_fftK_fftF;
	fftw_plan p1, p2, p3;

	inK  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outK = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	inF  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	fftK_fftF      = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	ifft_fftK_fftF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	long i, j;
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	inK[i*n + j][0] = K(i,j);
			inK[i*n + j][1] = 0.0;
			inF[i*n + j][0] = F(i,j);
			inF[i*n + j][1] = 0.0;
		}
	}

	p1 = fftw_plan_dft_2d( m, n, inK, outK, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p1);

	p2 = fftw_plan_dft_2d( m, n, inF, outF, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p2);
	
	
	//   -------
	
	// testing only
	/*DoubleArray2D fftKr(m,n), fftKi(m,n), fftFr(m,n), fftFi(m,n);
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	fftFr(i,j) = outF[i*n + j][0];
			fftFi(i,j) = outF[i*n + j][1];
			fftKr(i,j) = outK[i*n + j][0];
			fftKi(i,j) = outK[i*n + j][1];
		}
	}
	cout << "fftFr = " << endl << fftFr << endl;
	cout << "fftFi = " << endl << fftFi << endl;
	cout << "fftKr = " << endl << fftKr << endl;
	cout << "fftKi = " << endl << fftKi << endl;
	*/
	//   -------
	
    //   -------
	
	// testing only
	/*DoubleArray2D fftKr(m,n), fftKi(m,n), fftFr(m,n), fftFi(m,n);
     for(i = 0; i < m; i++)
     {	for( j = 0; j < n; j++ )
        {	fftFr(i,j) = outF[i*n + j][0];
            fftFi(i,j) = outF[i*n + j][1];
            fftKr(i,j) = outK[i*n + j][0];
            fftKi(i,j) = outK[i*n + j][1];
        }
     }*/
    
    //FileIO_2D IO;
    
    //IO.writePGMsc( fftFr, "fftIMGr" );
    //IO.writePGMsc( fftFi, "fftIMGi" );
    //IO.writePGMsc( fftKr, "fftPSFr" );
    //IO.writePGMsc( fftKi, "fftPSFi" );
    
    //IO.writePGMsc_to_range( fftFr, 43, 53, "fftIMGr_scRange" );
    //IO.writePGMsc_to_range( fftFi, 123, 133, "fftIMGi_scRange" );
    //IO.writePGMsc_to_range( fftKr, "fftPSFr" );
    //IO.writePGMsc_to_range( fftKi, "fftPSFi" );
	//   -------

	
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )	// complex multiplication:  fft(K)*fft(F)
		{	fftK_fftF[i*n + j][0] = outK[i*n + j][0] * outF[i*n + j][0] - outK[i*n + j][1] * outF[i*n + j][1];
			fftK_fftF[i*n + j][1] = outK[i*n + j][0] * outF[i*n + j][1] + outK[i*n + j][1] * outF[i*n + j][0];
		}
	}

	p3 = fftw_plan_dft_2d( m, n, fftK_fftF, ifft_fftK_fftF, FFTW_BACKWARD, FFTW_ESTIMATE );
	fftw_execute(p3);

	// ifft( fft(K)*fft(F) ):
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	F(i,j) = ifft_fftK_fftF[i*n + j][0]/m/n;	// F is returned back
			//ifft_fftK_fftF[i*n + j][1] = ifft_fftK_fftF[i*n + j][1]/m/n;
		}
	}
	//cout << "F = " << endl << F << endl;

	fftw_destroy_plan(p1); fftw_destroy_plan(p2); fftw_destroy_plan(p3);
	fftw_free(inK); fftw_free(outK);
	fftw_free(inF); fftw_free(outF);
	fftw_free(fftK_fftF);
	fftw_free(ifft_fftK_fftF);
}

//#####################################################################

void convolution_MTF_2D( DoubleArray2D& F, const DoubleArray2D& MTF_r, const DoubleArray2D& MTF_i )
{
	// Input: F is the clean image, MTF is the MTF = fft(K)
	// Output: F is the convolution
    
	long m = MTF_r.getIndex1Size();
	long n = MTF_r.getIndex2Size();
    
	fftw_complex *inF, *outF;
    fftw_complex *MTF;
	fftw_complex *MTF_fftF;
	fftw_complex *ifft_MTF_fftF;
	fftw_plan p2, p3;
    
	MTF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
    
	inF  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
    
	MTF_fftF      = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	ifft_MTF_fftF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
    
	long i, j;
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
        {   inF[i*n + j][0] = F(i,j);
            inF[i*n + j][1] = 0.0;
        }
	}
    
	p2 = fftw_plan_dft_2d( m, n, inF, outF, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p2);
	
    for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
        {
            MTF[i*n + j][0] = MTF_r(i,j);
            MTF[i*n + j][1] = MTF_i(i,j);
        }
	}
	
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )	// complex multiplication:  fft(K)*fft(F)
        {	MTF_fftF[i*n + j][0] = MTF[i*n + j][0] * outF[i*n + j][0] - MTF[i*n + j][1] * outF[i*n + j][1];
            MTF_fftF[i*n + j][1] = MTF[i*n + j][0] * outF[i*n + j][1] + MTF[i*n + j][1] * outF[i*n + j][0];
        }
	}
    
	p3 = fftw_plan_dft_2d( m, n, MTF_fftF, ifft_MTF_fftF, FFTW_BACKWARD, FFTW_ESTIMATE );
	fftw_execute(p3);
    
	// ifft( MTF*fft(F) ):
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
        {	F(i,j) = ifft_MTF_fftF[i*n + j][0]/m/n;	// F is returned back
        }
	}
	//cout << "F = " << endl << F << endl;
    
	fftw_destroy_plan(p2); fftw_destroy_plan(p3);
	fftw_free(MTF);
	fftw_free(inF); fftw_free(outF);
	fftw_free(MTF_fftF);
	fftw_free(ifft_MTF_fftF);
}

//#####################################################################

void deconvFidelity2D( DoubleArray2D& result, const DoubleArray2D& F, const DoubleArray2D& K, const DoubleArray2D& Krot, const DoubleArray2D& U )
{
	long m = K.getIndex1Size();
	long n = K.getIndex2Size();

	fftw_complex *inK,    *outK;
	fftw_complex *inKrot, *outKrot;
	fftw_complex *inF,    *outF;
	fftw_complex *inU,    *outU;

	fftw_complex *fftKrot_fftF;						// fft(Kr)*fft(F)
	fftw_complex *fftKrot_fftK;						// fft(Kr)*fft(K)
	fftw_complex *fftKrot_fftK_fftU;				// fft(Kr)*fft(K)*fft(u)
	fftw_complex *fftKrot_fftK_fftU__fftKrot_fftF;	// fft(Kr)*fft(K)*fft(u) - fft(Kr)*fft(F)

	fftw_complex *ifft_result;
	fftw_plan p1, p2, p3, p4, p5;

	inK     = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outK    = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	inKrot  = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outKrot = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	inF     = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outF    = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	inU     = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	outU    = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	fftKrot_fftF                    = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	fftKrot_fftK                    = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	fftKrot_fftK_fftU               = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	fftKrot_fftK_fftU__fftKrot_fftF = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);
	ifft_result                     = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * m*n);

	long i, j;
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	inKrot[i*n + j][0] = Krot(i,j);
			inKrot[i*n + j][1] = 0.0;
			inK[i*n + j][0] = K(i,j);
			inK[i*n + j][1] = 0.0;

			inF[i*n + j][0] = F(i,j);
			inF[i*n + j][1] = 0.0;
			inU[i*n + j][0] = U(i,j);
			inU[i*n + j][1] = 0.0;
		}
	}

	p1 = fftw_plan_dft_2d( m, n, inK, outK, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p1);

	p2 = fftw_plan_dft_2d( m, n, inF, outF, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p2);

	p3 = fftw_plan_dft_2d( m, n, inU, outU, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p3);

	p4 = fftw_plan_dft_2d( m, n, inKrot, outKrot, FFTW_FORWARD, FFTW_ESTIMATE );
	fftw_execute(p4);
	
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	// complex multiplication:  fft(Kr)*fft(F)     NOT fft(K)*fft(F)
			fftKrot_fftF[i*n + j][0] = outKrot[i*n + j][0] * outF[i*n + j][0] - outKrot[i*n + j][1] * outF[i*n + j][1];
			fftKrot_fftF[i*n + j][1] = outKrot[i*n + j][0] * outF[i*n + j][1] + outKrot[i*n + j][1] * outF[i*n + j][0];
			
			// fft(Kr)*fft(K)
			fftKrot_fftK[i*n + j][0] = outKrot[i*n + j][0] * outK[i*n + j][0] - outKrot[i*n + j][1] * outK[i*n + j][1];
			fftKrot_fftK[i*n + j][1] = outKrot[i*n + j][0] * outK[i*n + j][1] + outKrot[i*n + j][1] * outK[i*n + j][0];
			
			// fft(Kr)*fft(K)*fft(u)
			fftKrot_fftK_fftU[i*n + j][0] = fftKrot_fftK[i*n + j][0] * outU[i*n + j][0] - fftKrot_fftK[i*n + j][1] * outU[i*n + j][1];
			fftKrot_fftK_fftU[i*n + j][1] = fftKrot_fftK[i*n + j][0] * outU[i*n + j][1] + fftKrot_fftK[i*n + j][1] * outU[i*n + j][0];

			// fft(Kr)*fft(K)*fft(u) - fft(Kr)*fft(F)
			fftKrot_fftK_fftU__fftKrot_fftF[i*n + j][0] = fftKrot_fftK_fftU[i*n + j][0] - fftKrot_fftF[i*n + j][0];
			fftKrot_fftK_fftU__fftKrot_fftF[i*n + j][1] = fftKrot_fftK_fftU[i*n + j][1] - fftKrot_fftF[i*n + j][1];
		}
	}

	p5 = fftw_plan_dft_2d( m, n, fftKrot_fftK_fftU__fftKrot_fftF, ifft_result, FFTW_BACKWARD, FFTW_ESTIMATE );
	fftw_execute(p5);

	// ifft( fft(Kr)*fft(K)*fft(u) - fft(Kr)*fft(F) ):
	for(i = 0; i < m; i++)
	{	for( j = 0; j < n; j++ )
		{	result(i,j) = ifft_result[i*n + j][0]/m/n;	// F is returned back
			//ifft_result[i*n + j][1] = ifft_result[i*n + j][1]/m/n;
		}
	}

	fftw_destroy_plan(p1); fftw_destroy_plan(p2); fftw_destroy_plan(p3); fftw_destroy_plan(p4); fftw_destroy_plan(p5);
	fftw_free(inK); fftw_free(outK);
	fftw_free(inF); fftw_free(outF);
	fftw_free(inU); fftw_free(outU);
	fftw_free(inKrot); fftw_free(outKrot);

	fftw_free(fftKrot_fftF);
	fftw_free(fftKrot_fftK);
	fftw_free(fftKrot_fftK_fftU);
	fftw_free(fftKrot_fftK_fftU__fftKrot_fftF);

	fftw_free(ifft_result);
}

//#####################################################################

