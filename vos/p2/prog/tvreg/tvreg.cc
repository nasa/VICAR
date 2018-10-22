#include <iostream>
#include <iomanip>
#include <math.h>
#include "zvproto.h"
#include "vicmain_c"

//
// Total Variation Regularization
// algorithm from Rudin et al., 1992
// coded by J. Maki, 5 Feb. 1998
//

int status,unit,ns,nl,nb,recsize,outUnit; // label values, I/O stuff
int ZMAX,XDIM,YDIM,count,def;
char org[3],format[4];	// label values, I/O stuff
int image[512*512] = {0};// array for input image
int x[512] = {0};	// array used to read in lines of input image
float xx[512] = {0};	// array used to write out lines of output image
extern float fabs();

void main44()

{
        int i, j;

	void deltaX(int direction,float inputImage[],float outputImage[]);
	void deltaY(int direction,float inputImage[],float outputImage[]);
	void minMod(float aImage[], float bImage[],float outputImage[]);
	
        float alpha;
	
	// float sgnA=0;
	// float sgnB=0;
	
        // Parameters
        status=zvparm((char *)"ITER",&ZMAX,&count,&def,1,0);
        status=zvparm((char *)"ALPHA",&alpha,&count,&def,1,0);

	// input image
	status = zvunit(&unit,(char *)"INP",1,NULL);
	status = zvopen(unit,"U_FORMAT","FULL",NULL); 
	status = zvget(unit,"NS",&ns,"NL",&nl,"NB",&nb,"RECSIZE",&recsize,"ORG",
		&org,NULL); // get header info
        if((ns > 512) || (nl > 512)){
          zvmessage((char *)"Input image too large, limits 512 by 512",NULL);
          zabend();
        }

        // Storage
        XDIM=ns;
        YDIM=nl;
	float *xDeltaMinus =  new float [XDIM*YDIM];
	float *xDeltaPlus =  new float [XDIM*YDIM];
	float *yDeltaMinus =  new float [XDIM*YDIM];
	float *yDeltaPlus =  new float [XDIM*YDIM];
	
	float *xDeltaPlus0 =  new float [XDIM*YDIM];
	float *yDeltaPlus0 =  new float [XDIM*YDIM];
	float *xDeltaMinus0 =  new float [XDIM*YDIM];
	float *yDeltaMinus0 =  new float [XDIM*YDIM];
	
	float *minModX = new float [XDIM*YDIM];
	float *minModY = new float [XDIM*YDIM];
	
	float *u0 = new float [XDIM*YDIM];
	float *un = new float [XDIM*YDIM];
	float *un1 = new float [XDIM*YDIM];

	float *p1 = new float [XDIM*YDIM];
	float *p2 = new float [XDIM*YDIM];
	float *p3 = new float [XDIM*YDIM];
	float lambda = 0;
	
	float *part1 = new float [XDIM*YDIM];
	float *part2 = new float [XDIM*YDIM];
	float *dnm = new float [XDIM*YDIM];
	
	// output image
	status = zvunit(&outUnit,(char *)"OUT",1,NULL);
	status = zvopen(outUnit,"U_FORMAT","REAL","OP","WRITE",
	"O_FORMAT","REAL","U_NL",nl,"U_NS",ns,"U_ORG",org,NULL);

	// read the image data
	for (j = 0; j < nl; j++) {
		status = zvread(unit,&x,NULL);
		for (i = 0; i < ns; i++)
			image[i+j*ns] = x[i];
	}
        
	status = zvclose(unit,NULL);    // close the input file



//--------------- calculations

	for (i=0; i<XDIM*YDIM; i++) {	// copy image into u0,un,un1
		u0[i]=image[i];
		un[i]=image[i];
		un1[i]=image[i];
	}

		deltaX(1,u0,xDeltaPlus0);	// derivatives for image
		deltaX(-1,u0,xDeltaMinus0);
		deltaY(1,u0,yDeltaPlus0);
		deltaY(-1,u0,yDeltaMinus0);

	for (int z=0; z<ZMAX; z++){

	        int i;
		for (i=0; i<XDIM*YDIM; i++) {	// copy image into un
			un[i]= un1[i];
		}
		
		std::cout << "interation: " <<z <<  std::endl;
		//std::cout << un[XDIM*186 + 112] << std::endl;

		deltaX(1,un,xDeltaPlus);	// derivatives for un
		deltaX(-1,un,xDeltaMinus);
		deltaY(1,un,yDeltaPlus);
		deltaY(-1,un,yDeltaMinus);

		minMod(xDeltaPlus,xDeltaMinus,minModX);	// minmod for un
		minMod(yDeltaPlus,yDeltaMinus,minModY);
	
		for (i=0; i<XDIM*YDIM; i++) {
	
			// part 1 of eq. 2.8a
			if(xDeltaPlus[i] != 0)	
				p1[i] = xDeltaPlus[i]/sqrt((xDeltaPlus[i]
				*xDeltaPlus[i] + minModY[i]*minModY[i]));
			else  
				p1[i] = 0;
		
			// part 2 of eq. 2.8a
			if(yDeltaPlus[i] != 0)
 				p2[i] = yDeltaPlus[i]/sqrt((yDeltaPlus[i]
				*yDeltaPlus[i] + minModX[i]*minModX[i]));
			else 
				p2[i] = 0;
		
	
			// denominator for p3 of eq. 2.8a
	
			if(xDeltaPlus[i] != 0 || yDeltaPlus[i] != 0 ){

				dnm[i] = sqrt(xDeltaPlus[i]*xDeltaPlus[i] 
					+ yDeltaPlus[i]*yDeltaPlus[i]);
	
				lambda = lambda - (dnm[i]
					- (xDeltaPlus0[i]*xDeltaPlus[i]
				    + yDeltaPlus0[i]*yDeltaPlus[i])/dnm[i]);
			}


		}


			//std::cout << "alpha = " << lambda << std::endl;

			//if ((fabs(lambda) > .30) && (fabs(lambda) < .44))
				 //z = ZMAX;	

			for (i=0; i<XDIM*YDIM; i++) { 
				// part 3 of eq. 2.8a
				p3[i] = .25*(un[i] - u0[i]);
			}	

			deltaX(-1,p1,part1);	// derivative of p1
			deltaY(-1,p2,part2);	// derivative of p2
		
			for (i=0; i<XDIM*YDIM; i++) 
	 			un1[i] = un[i] + (part1[i] + part2[i]) + p3[i];
		
			lambda = 0;	// reset lambda;
	}

	// write out the image

	for(j=0; j< YDIM; j++) {
		for(i=0; i<XDIM; i++) {
	 		xx[i] = un1[i+j*XDIM];
		}
		// write a line to output file
		status = zvwrit(outUnit,xx,NULL);
	}

        status = zvclose(outUnit,NULL); // close the output file

return ;

}

void deltaX(int direction,float inputImage[], float outputImage[])

{
	//std::cout << "Computing deltaX..." << std::endl;
	
	//std::cout << "Direction is " << direction << "." << std::endl;

	for(int j=1; j<YDIM-1; j++) {
		for(int i=1; i<XDIM-1; i++){
			outputImage[i+j*XDIM] =
			direction*(inputImage[(i+direction)+j*XDIM] -
			inputImage[i+j*XDIM]);
		}
	}

}

void deltaY(int direction,float inputImage[], float outputImage[])

{
	//std::cout << "Computing deltaY..." << std::endl;
	//std::cout << "Direction is " << direction << "." << std::endl;

	for(int j=1; j<YDIM-1; j++) {
		for(int i=1; i<XDIM-1; i++) {
			outputImage[i+j*XDIM] = 
			direction*(inputImage[i+(j+direction)*XDIM] - 
			inputImage[i+j*XDIM]);
		}
	}

}

	
void minMod(float aImage[], float bImage[],float outputImage[])

{
	int sgnA,sgnB;
	//std::cout << "computing minMod..." << std::endl;

	for(int i=0; i<XDIM*YDIM; i++) {
		if (aImage[i] < 0) 
			sgnA = -1;
		else
			sgnA = 1;
	
	   	if (bImage[i] < 0)
			sgnB = -1;
	   	else
			sgnB = 1;

		if (fabs(aImage[i]) < fabs(bImage[i]))
			outputImage[i] = ((sgnA + sgnB)/2)*fabs(aImage[i]);
	   	else
			outputImage[i] = ((sgnA + sgnB)/2)*fabs(bImage[i]);
	   	if (aImage[i] == 0 | bImage[i] == 0)
			outputImage[i] = 0;
	}
	//std::cout << "exiting minMod..." << std::endl;
}
