//
//#####################################################################
// Igor Yanovsky (C) UCLA, JPL
// version:  07/25/2016
// update: 08/22/2017
// added void addNoiseSaltPepper( DoubleArray2D& A, double d, double pepper_value, double salt_value )
//#####################################################################
//

#include <iostream>
using namespace std;

#include <fstream>
#include <math.h>
#include <stdlib.h>

#include "DoubleArray1D.h"
#include "DoubleArray2D.h"
#include "Grid2D.h"
#include "BC_2D.h"
#include "Noise2D.h"

//#####################################################################

void addNoise2D( DoubleArray2D& A, double noise_number, const Grid2D& grid )
{
	cout << "Adding noise: The image will NOT be scaled to 0-255 range" << endl;
	long m = grid.m;
	long n = grid.n;
	long w = grid.w;

	long NumPixels = m*n;
	double sumNoise = 0.0;

	DoubleArray2D noise(m,n);

	//
	// Gives a matrix of random *double* values between -noise_number and noise_number:
	//
	// RAND_MAX := 32767
	// rand() :=  gives a random integer between 0 and RAND_MAX
	// 
	long i;  long j;

	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{		// random *double* between  -noise_number  and  noise_number:
			noise(i,j) = -noise_number + 2*noise_number * (rand()/double(RAND_MAX));
			sumNoise = sumNoise + noise(i,j);
		}
	}

	double avgNoise = sumNoise / NumPixels;
	cout << "1) Mean of noise: " << avgNoise << endl;

	//
	//	Gives a noise matrix of average 0:
	//

	sumNoise = 0.0;

	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			noise(i,j) = noise(i,j) - avgNoise;
			sumNoise = sumNoise + noise(i,j);
		}
	}

	avgNoise = sumNoise / NumPixels;
	cout << "2) Mean of noise: " << avgNoise << endl;

	// Make sure the pixel values of the noise matrix are in -noise_number to noise_number range:
	double maximum_entry_noise = 0.0;
	double minimum_entry_noise = 100.0;

	for(j = 0; j < n; j++)
	{
		for(i = 0; i < m; i++)
		{
			maximum_entry_noise = (maximum_entry_noise > fabs(noise(i,j))) ? maximum_entry_noise : fabs(noise(i,j));
			minimum_entry_noise = (minimum_entry_noise < fabs(noise(i,j))) ? minimum_entry_noise : fabs(noise(i,j));
		}
	}
	cout << "Max: " << maximum_entry_noise << "  Min: " << minimum_entry_noise << endl;

	// cout << noise << endl;

	A = A + noise;

	NeumannBC( A, w );
}
//
//#####################################################################
//
void addNoise2D_sc( DoubleArray2D& A, double noise_number )
{
	cout << "Adding noise: The image WILL BE scaled to 0-255 range" << endl;
	long m = A.getIndex1Size();
	long n = A.getIndex2Size();

	long NumPixels = m*n;
	double sumNoise = 0.0;

	DoubleArray2D noise(m,n);
	DoubleArray2D noisy_img(m,n);

	//
	// Gives a matrix of random integer values between -noise_number and noise_number:
	//
	long i;  long j;

	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			noise(i,j) = long(-noise_number + 2*noise_number * (rand()/double(RAND_MAX)));
			sumNoise = sumNoise + noise(i,j);
		}
	}

	cout << "1) Mean of noise: " << double(sumNoise) / NumPixels << endl;

	noisy_img = A + noise;

	//
	// Regularize the values of the pixels to fall in 0-255 range:
	//
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			if( noisy_img(i,j) > 255 )
				noisy_img(i,j) = 255;
			else if( noisy_img(i,j) < 0 )
				noisy_img(i,j) = 0;
		}
	}

	// This code below should be functional.  However, when run last time, it went into infinite loop, and is
	// therefore commented.  This should be investigated.

/*
	//
	// Real Average Noise (after ensuring that the pixel values are in the correct range):
	//
	double total_noise = 0.0;

	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			total_noise = total_noise + ( noisy_img(i,j) - A(i,j) );
		}
	}
	cout << "2) Mean of noise: " << double(total_noise) / NumPixels << endl;
	cout << "Total Noise: " << total_noise << endl;

	long row, column; 

	while( abs(total_noise) > 0 )
	{
		// choose random row and column of the noisy image (1 to m  x  1 to n):
		row = long((m-1)*(rand()/double(RAND_MAX)));
		column = long((n-1)*(rand()/double(RAND_MAX)));
		
		if(total_noise > 0)			// if the noise added is positive
		{
			if(total_noise >= noise_number)
			{
				if(noisy_img(row,column) >= noise_number)
				{
					// and if the value at the random place is positive:
						// set random place's value to 0 and subract the value from the total noise
					noisy_img(row,column) = noisy_img(row,column) - noise_number;
					total_noise = total_noise - noise_number;
				}
			}
			else if(total_noise < noise_number)
			{
				// if noise added (which is positive) is less than the noise number
				if(noisy_img(row,column) >= total_noise)
				{
					// noisy_image is greater than the noise added
						// subtract the entire noise added from that pixel.
					noisy_img(row,column) = noisy_img(row,column) - total_noise;
					total_noise = 0;
				}
			}
		}
		else if(total_noise < 0)		// if the noise added is negative
		{
			if(abs(total_noise) >= noise_number)
			{
				if(noisy_img(row,column) <= (255-noise_number))
				{
					noisy_img(row,column) = noisy_img(row,column) + noise_number;
					total_noise = total_noise + noise_number;
				}
				else if(abs(total_noise) < noise_number)
					// if noise added (which is negative) is less (in abs value) than the noise number
				{
					if(noisy_img(row,column) <= (255 - abs(total_noise)))
					{
						noisy_img(row,column) = noisy_img(row,column) + abs(total_noise);
						total_noise = 0.0;
					}
				}
			}
		}
		//cout << "Total Noise: " << total_noise << endl;
	}

	noise = A - noisy_img;

	// Make sure the pixel values of the Noisy Image are in 0-255 range:
	// Check that the entries of the noisy image fall in the range 0-255:
	double maximum_entry_noisy_image = 0.0;
	double minimum_entry_noisy_image = 0.0;

	for(j = 0; j < n; j++)
	{
		for(i = 0; i < m; i++)
		{
			maximum_entry_noisy_image = (maximum_entry_noisy_image > fabs(noisy_img(i,j))) ? maximum_entry_noisy_image : fabs(noisy_img(i,j));
			minimum_entry_noisy_image = (minimum_entry_noisy_image < fabs(noisy_img(i,j))) ? minimum_entry_noisy_image : fabs(noisy_img(i,j));
		}
	}
	cout << "Max: " << maximum_entry_noisy_image << "  Min: " << minimum_entry_noisy_image << endl;
	
	// Check that the average noise is 0:
	total_noise = 0.0;

	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			total_noise = total_noise + ( noisy_img(i,j) - A(i,j) );
		}
	}
	cout << "3) Mean of noise: " << double(total_noise) / NumPixels << endl;
	*/
	
	A = noisy_img;	
}

//#####################################################################

void addGaussNoise2D( DoubleArray2D& A, double variance, const Grid2D& grid )
{
	long m = grid.m;
	long n = grid.n;
	long w = grid.w;
	
	double std = sqrt(variance);
	long NumPixels = m*n;
	double sumNoise = 0.0;
	
	DoubleArray2D noise(m,n);
	
	if( variance > 1e-15 )
	{
		if( NumPixels <= 15625000 )
		{
			readNoise2D( noise, grid, "/Users/yanovsky/Igor/Images/Noise/GaussianNoise15625000.txt" );
		}
		else
		{
			cout << "Generate GaussianNoiseXXX.txt file containing " << NumPixels << " entries using GenerateGaussianNoise.m" << endl;
			exit(1);
		}
		
		long i; long j;
		for(j = 0; j < n; j++)
		{
			for(i = 0; i < m; i++)
			{	noise(i,j) = std*noise(i,j);	}
		}
		
		// Now, avg(noise) is almost 0.  If we subtract this value from noise:
		//				noise - avg(noise),
		// we would obtain:
		//			avg( noise - avg(noise) ) = 0.
		
		/////////////////////////////////////////////////////////////////////////////
		//
		// Calculate the average of the noise:
		//
		
		for(j = 0; j < n; j++)
		{
			for(i = 0; i < m; i++)
			{	sumNoise = sumNoise + noise(i,j);	}
		}
		double avgNoise = sumNoise / NumPixels;
		cout << "1) Mean of noise: " << avgNoise << endl;
		
		//
		//	Gives a noise matrix of average 0:
		//
		
		sumNoise = 0.0;
		for(j=0; j < n; j++)
		{	for(i=0; i < m; i++)
			{	noise(i,j) = noise(i,j) - avgNoise;
				sumNoise = sumNoise + noise(i,j);	}
		}
		avgNoise = sumNoise / NumPixels;
		cout << "2) Mean of noise: " << avgNoise << endl;
		
		// Make sure the pixel values of the noise matrix are in -noise_number to noise_number range:
		double maximum_entry_noise = 0.0;
		double minimum_entry_noise = 0.0;
		
		for(j=0; j < n; j++)
		{
			for(i=0; i < m; i++)
			{
				maximum_entry_noise = (maximum_entry_noise > fabs(noise(i,j))) ? maximum_entry_noise : fabs(noise(i,j));
				minimum_entry_noise = (minimum_entry_noise < fabs(noise(i,j))) ? minimum_entry_noise : fabs(noise(i,j));
			}
		}
		cout << "Max: " << maximum_entry_noise << "  Min: " << minimum_entry_noise << endl;
		
		A = A + noise;
	}
	else
	{
		cout << "NOT ADDING NOISE!" << endl << endl;
	}
}

//#####################################################################

void initializeGaussNoise( DoubleArray1D& noise1D )
{
	long m = 15625000;
	noise1D.initialize(m);
	
	string filename = "/Users/yanovsky/Igor/Images/Noise/GaussianNoise15625000.txt";
	
	ifstream infile( filename.c_str() );
	if(!infile)
	{
		cout << "Error reading " << filename<< "!!!" << endl;
		exit(-1);
	}
	
	long i;
	cout << "Reading the noise..." << endl;
	for(i = 0; i < m; i++)
	{
		infile >> noise1D(i);
	}
	
	infile.close();
}

//#####################################################################

void addGaussNoise2D( DoubleArray2D& A, DoubleArray1D& noise1D, const long& image_N, double variance, const Grid2D& grid )
{
	long m = grid.m;
	long n = grid.n;
	long w = grid.w;
	
	double std = sqrt(variance);
	long NumPixels = m*n;
	double sumNoise = 0.0;
	
	DoubleArray2D noise(m,n);
	
	if( variance > 1e-15 )
	{
		if( NumPixels * (image_N+1) > 15625000 )
		{
			cout << "Generate GaussianNoiseXXX.txt file containing " << NumPixels << " entries using GenerateGaussianNoise.m" << endl;
			exit(1);
		}
		
		long i; long j;
		for(j = 0; j < n; j++)
		{
			for(i = 0; i < m; i++)
			{	noise(i,j) = std*noise1D( (i+j*m) * (image_N+1) );	}
		}
		
		// Now, avg(noise) is almost 0.  If we subtract this value from noise:
		//				noise - avg(noise),
		// we would obtain:
		//			avg( noise - avg(noise) ) = 0.
		
		/////////////////////////////////////////////////////////////////////////////
		//
		// Calculate the average of the noise:
		//
		
		for(j = 0; j < n; j++)
		{
			for(i = 0; i < m; i++)
			{	sumNoise = sumNoise + noise(i,j);	}
		}
		double avgNoise = sumNoise / NumPixels;
		cout << "1) Mean of noise: " << avgNoise << endl;
		
		//
		//	Gives a noise matrix of average 0:
		//
		
		sumNoise = 0.0;
		for(j=0; j < n; j++)
		{	for(i=0; i < m; i++)
		{	noise(i,j) = noise(i,j) - avgNoise;
			sumNoise = sumNoise + noise(i,j);	}
		}
		avgNoise = sumNoise / NumPixels;
		cout << "2) Mean of noise: " << avgNoise << endl;
		
		// Make sure the pixel values of the noise matrix are in -noise_number to noise_number range:
		double maximum_entry_noise = 0.0;
		double minimum_entry_noise = 0.0;
		
		for(j=0; j < n; j++)
		{
			for(i=0; i < m; i++)
			{
				maximum_entry_noise = (maximum_entry_noise > fabs(noise(i,j))) ? maximum_entry_noise : fabs(noise(i,j));
				minimum_entry_noise = (minimum_entry_noise < fabs(noise(i,j))) ? minimum_entry_noise : fabs(noise(i,j));
			}
		}
		cout << "Max: " << maximum_entry_noise << "  Min: " << minimum_entry_noise << endl;
		
		A = A + noise;
	}
	else
	{
		cout << "NOT ADDING NOISE!" << endl << endl;
	}
}

//#####################################################################

void addNoiseSaltPepper( DoubleArray2D& A, double d )
{
	// d - noise density

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	DoubleArray2D random(m,n);
	
	long i, j;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			random(i,j) = (rand()/double(RAND_MAX));
			if( random(i,j) < d/2.0 )
			{	A(i,j) = 0.0;	}
			else if( random(i,j) >= d/2.0 && random(i,j) < d )
			{	A(i,j) = 255.0;	}
		}
	}
}

//#####################################################################

void addNoiseSaltPepper( DoubleArray2D& A, double d, double pepper_value, double salt_value )
{
	// d - noise density

	long m = A.getIndex1Size();
	long n = A.getIndex2Size();
	
	DoubleArray2D random(m,n);
	
	long i, j;
	for(j=0; j < n; j++)
	{
		for(i=0; i < m; i++)
		{
			random(i,j) = (rand()/double(RAND_MAX));
			if( random(i,j) < d/2.0 )
			{	A(i,j) = pepper_value;	}
			else if( random(i,j) >= d/2.0 && random(i,j) < d )
			{	A(i,j) = salt_value;	}
		}
	}
}

//#####################################################################

void readNoise2D( DoubleArray2D& A, const Grid2D& grid, string filename )
{
	long m = grid.m;
	long n = grid.n;
	long w = grid.w;
	
	ifstream infile( filename.c_str() );
	if(!infile)
	{
		cout << "Error reading " << filename<< "!!!" << endl;
		exit(-1);
	}
	
	long i; long j;
	
	/*cout << "Read the noise in forward direction: " << endl;
	 for(k = w; k < p-w; k++)
	 {
	 for(j = w; j < n-w; j++)
	 {
	 for(i = w; i < m-w; i++)
	 {
	 infile >> A(i,j,k);
	 }
	 }
	 }*/
	
	cout << "Read the noise in backward direction: " << endl;
    cout << "*** Change in readNoise2D() was made on 07/25/2016 ***";
	for(j = w; j < n-w; j++)
	{
		for(i = w; i < m-w; i++)
		{
			// infile >> A(n-i,m-j);
            infile >> A(m-1-i,n-1-j);   // Change was made on 07/25/2016
		}
	}
	
	infile.close();
	
	NeumannBC( A, w );
}
