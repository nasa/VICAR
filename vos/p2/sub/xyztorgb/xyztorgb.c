/*This function returns the transformation from tristimulus values to
red,green,blue for the designated device.  The calling program then performs
the transformation as follows:
   	rgb = ttr * xyz + offset, 
where ttr and offset are a matrix and a vector output by this function, and 
xyz and rgb are vectors of tristimulus values and r,g,b, outputs,
respectively.  The function returns -1 if the device does not exist, 1
otherwise.  The field dtag.output[][] contains the outputs of the device, in
arbitrary units, for inputs of (255,0,0), (0,255,0), (0,0,255), and (0,0,0),
respectively.

  The vector, rgb, represents intensity for a color monitor, and dn can be 
obtained by
  	DN = I ** 1/2.6
(see HELP GIACONDA).

  Unfortunately, a film recorder cannot be represented by equations as simple
as the ones above.  So, although at this time, this function returns no useful
information for a film recorder, a data structure (DEVICE) does exist to
contain relevant information on such devices for a future solution, and there
is an entry for the MDA.*/ 

#include "xvmaininc.h"   
#include "colors.h"

struct dtag {						/*output devices*/
  char *name;
  int light;						/*illuminant*/
  double output[4][50];					/*intensity or  */
  							/*  reflectivity at 255*/
  							/*  1st index = r,g,b,*/
  							/*  black	      */
  int rtt_valid;					/*rtt and black valid*/
  double rtt[3][3];					/*rgb to tristim*/
  							/*  ([xyz][rgb])*/
  double black[3];					/*tristim of black*/
} DEVICE[] = {
  {"MDA",FLR,
    {						/*not up to date*/
      {20,20,20,20,20,20,20,20,20,20,20,50,52,46,43,43,43,44,45,47,48,47,45,
       42,42,42,47,59,
       83,125,181,245,308,364,418,470,510,547,580,603,620,633,643,652,661,665,
       673,677,682
      },
      {20,20,20,20,20,20,20,20,20,20,20,42,50,52,54,58,64,73,87,106,135,167,
       194,204,195,
       172,142,112,86,65,50,41,35,31,28,31,27,28,29,31,35,40,48,61,78,103,140,
       167,224
      },
      {20,20,20,20,20,20,20,20,20,20,20,62,97,127,164,212,243,248,224,183,143,
       107,81,62,50,
       43,40,38,35,33,30,27,24,22,22,24,22,22,23,25,27,31,37,47,63,84,128,154,200
      },
      {20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
       20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
       20,20,20,20
      }
    }
  },
  {"TV",0,{0},1,					/*conrac 7211,lp    */
    {.3127 * .62, .3291 * .21,  .3582 * .15,		/* 2nd no. = cc     */
     .3127 * .33, .3291 * .675, .3582 * .06,		/* 1st no. chosen | */
     .3127 * .05, .3291 * .115, .3582 * .79}		/* r=g=b => D65     */
  },
  {0}							/*end of list*/
};

double minvert(),mdet(),mcof(),fabs();



xyztorgb(device,ttr,offset)
char   device[];
double ttr[3][3],offset[3];
{
  struct dtag *pdevice;
  int i,rgb,lamda;
  double determinant;

  for (pdevice = DEVICE;				/*find device matrix*/
       strcmp(device,pdevice->name) &&
       pdevice->name[0];
       pdevice++);
  if (!pdevice->name[0]) return -1;			/*no such device*/

  if (!pdevice->rtt_valid) {				/*if rtt not valid*/
    for (i = 0; i < 3; i++) {				/*for x,y,z*/
      for (rgb = 0; rgb < 3; rgb++) {			/*for r,g,b*/
	pdevice->rtt[i][rgb] = 0;
	for (lamda = 0; lamda < SSIZE; lamda++) {	/*rtt=(output-black) */
	  pdevice->rtt[i][rgb] += 			/*  * ill. * cmf     */
	    (pdevice->output[rgb][lamda] -
	    pdevice->output[BLACK][lamda]) *
	    ILLUMINANT[pdevice->light][lamda] *
	    MATCH[i][lamda];
	}
        pdevice->rtt[i][rgb] /= 255;			/*units = dn*/
      }
      pdevice->black[i] = 0;
      for (lamda = 0; lamda < SSIZE; lamda++) {		/*black = black_out * */
	pdevice->black[i] +=				/*  ill. * cmf        */
	  pdevice->output[BLACK][lamda] *
	  ILLUMINANT[pdevice->light][lamda] *
	  MATCH[i][lamda];
      }
    }
    pdevice->rtt_valid = 1;				/*mark valid*/
  }

  determinant = minvert(pdevice->rtt,ttr,3);		/*invert matrix*/

  for (rgb = 0; rgb < 3; rgb++) {			/*get offset vector*/
    offset[rgb] = 0;
    for (i = 0; i < 3; i++) {
      offset[rgb] +=
        pdevice->black[i] *
        mcof(pdevice->rtt,i,rgb,3);
    }
    offset[rgb] /= - determinant;
  }

  return 1;
}



double minvert(matrix,inverse,size)
/*This function calculates the inverse of a matrix of dimensions, size x size.
mm(i,j), mi(i,j), and mc(i,j) are the (i,j)th elements of matrix, inverse, and
comatrix, respectively.  The function returns the determinant of the original
matrix.*/ 

double matrix[],inverse[];
int size;
{
  int i,j;
  double mm_det,comatrix[MFILTERS * MFILTERS];

  mm_det = mdet(matrix,size);				/*get determinant*/
  if (fabs(mm_det) > EPSILON) {				/*if unique solution*/
    for (i = 0; i < size; i++) {			/*for each element of mi*/
      for (j = 0; j < size; j++) {
	mi(i,j) = mcof(matrix,j,i,size) / mm_det;	/*get value*/
      }
    }
  }
  return mm_det;
}



double mdet(matrix,size)
/*This co-recursive function returns the determinant of a matrix of dimensions,
size x size.*/ 

double matrix[];
int size;
{
  double determinant,comatrix[MFILTERS * MFILTERS];
  int i,j;

  if (size == 1) {					/*terminal case*/
    return matrix[0];
  }

  determinant = 0;					/*initialize sum*/
  for (i = 0; i < size; i++) {				/*for first column*/
    determinant += mm(i,0) * mcof(matrix,i,0,size);
  }
  return determinant;
}



double mcof(matrix,row,column,size)
/*This co-recursive function returns the cofactor of a given matrix of
dimensions, size x size.*/ 

double matrix[];
int row,column,size;
{
  int k,l;
  double comatrix[MFILTERS * MFILTERS];
  int parity;

  if (size == 1) {					/*terminal case*/
    return 1;
  }

  for (k = 0; k < size - 1; k++) {			/*get comatrix*/
    for (l = 0; l < size - 1; l++) {
      mc((k + row) % (size - 1),
  	 (l + column) % (size - 1)) =
      mm((row + k + 1) % size,
         (column + l + 1) % size);
    }
  }
  parity = 1 - (row + column) % 2 * 2;			/* -1 ** (i+j) */
  return parity * mdet(comatrix,size - 1);
}
