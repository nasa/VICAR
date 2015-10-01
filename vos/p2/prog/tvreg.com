$!****************************************************************************
$!
$! Build proc for MIPL module tvreg
$! VPACK Version 1.9, Tuesday, March 28, 2000, 15:50:42
$!
$! Execute by entering:		$ @tvreg
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module tvreg ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to tvreg.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("tvreg.imake") .nes. ""
$   then
$      vimake tvreg
$      purge tvreg.bld
$   else
$      if F$SEARCH("tvreg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tvreg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tvreg.bld "STD"
$   else
$      @tvreg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tvreg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tvreg.com -
	-s tvreg.cc -
	-i tvreg.imake -
	-p tvreg.pdf -
	-t tsttvreg.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tvreg.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tvreg.imake
#define PROGRAM tvreg
#define MODULE_LIST tvreg.cc
#define LIB_TAE
#define USES_C_PLUS_PLUS
#define LIB_P2SUB
#define LIB_RTL
#define MAIN_LANG_C_PLUS_PLUS
$ Return
$!#############################################################################
$PDF_File:
$ create tvreg.pdf
process help=*
PARM INP TYPE=STRING COUNT=1
PARM OUT TYPE=STRING COUNT=1
PARM ITER TYPE=INTEGER COUNT=(0:1) VALID=(1:100) DEFAULT=17
PARM ALPHA TYPE=REAL COUNT=(0:1) VALID=(0.0:1.0) DEFAULT=.00009
END-PROC

.TITLE
VICAR program TVREG

.HELP
PURPOSE:
To reduce noise in images.

EXECUTION:
tvreg inp=in.img out=out.img iter=5 alpha=.001

.PAGE
METHOD:
Rudin et.al. "Nonlinear total variation based noise removal algorithms"
Physica D 60 (1992) 259-268.

Restrictions:
1. Image cannot exceed 512 by 512


HISTORY:
9-1-98  J Maki. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI INP
Input image.

.VARI OUT
Output image

.VARI ITER
Number of iterations.

.VARI ALPHA
Balancing parameter.
(not implemented)

.LEVEL2

.VARI INP
Input image.

.VARI OUT
Output image is always real format.

.VARI ITER
Number of iterations. Defaults to 17.

.VARI ALPHA
Balancing parameter. Defaults to .00009 .
(not implemented)
$ Return
$!#############################################################################
$Test_File:
$ create tsttvreg.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
gausnois out=a.img nl=50 ns=50
boxflt2 inp=a.img out=b.img nlw=3 nsw=3
tvreg inp=b.img out=c.img
xvd c.img
!
end-proc
$ Return
$!#############################################################################
