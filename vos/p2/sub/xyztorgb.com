$!****************************************************************************
$!
$! Build proc for MIPL module xyztorgb
$! VPACK Version 1.7, Tuesday, November 23, 1993, 13:38:42
$!
$! Execute by entering:		$ @xyztorgb
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module xyztorgb ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to xyztorgb.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("xyztorgb.imake") .nes. ""
$   then
$      vimake xyztorgb
$      purge xyztorgb.bld
$   else
$      if F$SEARCH("xyztorgb.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake xyztorgb
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @xyztorgb.bld "STD"
$   else
$      @xyztorgb.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create xyztorgb.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack xyztorgb.com -
	-s xyztorgb.c -
	-i xyztorgb.imake -
	-t txyztorgb.c txyztorgb.imake txyztorgb.pdf tstxyztorgb.pdf -
	-o xyztorgb.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create xyztorgb.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create xyztorgb.imake
/* Imake file for VICAR subroutine xyztorgb   */

#define SUBROUTINE  xyztorgb

#define MODULE_LIST  xyztorgb.c  

#define P2_SUBLIB

#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create txyztorgb.c
/*This program tests function XYZTORGB*/
#include   "vicmain_c"
main44()
{
  int device,i;
  double rgb[3],out[3][3],offset[3];
  							/*chromaticity coords.*/
  double white[3] = {.3324,.3475,.3201};		  /*D55*/

  double red[3] = {.62,.33,.05};			  /*Conrac 7211 LP*/
  double green[3] = {.21,.675,.115};
  double blue[3] = {.15,.06,.79};

  xyztorgb("TV",out,offset);
  printf("RESULTS FOR \"TV\":\n");
  printf("matrix = %6.6f %6.6f %6.6f\n",out[0][0],out[0][1],out[0][2]);
  printf("         %6.6f %6.6f %6.6f\n",out[1][0],out[1][1],out[1][2]);
  printf("         %6.6f %6.6f %6.6f\n",out[2][0],out[2][1],out[2][2]);
  printf("offset=  %6.6f %6.6f %6.6f\n",offset[0],offset[1],offset[2]);
  printf("\n");
  
  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * white[0] +
  	     out[i][1] * white[1] +
  	     out[i][2] * white[2] +
  	     offset[i];
  }


  printf("When returned matrix is multiplied by the chromaticity \n");
  printf("coordinates of sunlight (D55) and of the three phosphors, \n");
  printf("the results are the following phosphor intensities:\n");
  printf("\n");
  printf("reproduced       red      green     blue\n");
  printf("  color       intensity intensity intensity\n");


  printf("white       %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * red[0] + out[i][1] * red[1] + out[i][2] * red[2];
  }
  printf("red         %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * green[0] + out[i][1] * green[1] + out[i][2] * green[2];
  }
  printf("green       %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  for (i = 0; i < 3; i++) {
    rgb[i] = out[i][0] * blue[0] + out[i][1] * blue[1] + out[i][2] * blue[2];
  }
  printf("blue        %10.6f %10.6f %10.6f\n",rgb[0],rgb[1],rgb[2]);

  printf("\n");
  printf("The values for white should be approximately equal; those for \n");
  printf("red, green, and blue should be non-zero only in the red, green, \n");
  printf("and blue, respectively\n");
}

$!-----------------------------------------------------------------------------
$ create txyztorgb.imake
/* IMAKE file for Test of VICAR subroutine  xyztorgb   */

#define PROGRAM txyztorgb

#define MODULE_LIST txyztorgb.c

#define MAIN_LANG_C
#define TEST

#define USES_C

#define   LIB_RTL
#define   LIB_TAE
#define   LIB_LOCAL       /*  Disable during delivery   */
/* #define LIB_P2SUB             Enable during delivery  */
$!-----------------------------------------------------------------------------
$ create txyztorgb.pdf
Process
End-Proc
$!-----------------------------------------------------------------------------
$ create tstxyztorgb.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!  
txyztorgb
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create xyztorgb.hlp
1 XYZTORGB

    This program provides information necessary to convert from
    tristimulus values to image device inputs.  It currently works only
    for the Conrac 7211 LP color monitor, although it contains code which
    may be used later for film recorders. 

  Calling Sequence:

  	xyztorgb(device,ttr,offset),

  where

    device (input) is the address of a zero-terminated character string
	    denoting the output device (currently only "TV" is valid) 
    ttr (output) is the address of a 9-element double (float) array
    offset (output) is the address of a 3-element double (float) array

2 History

  Original Programmer: 		R. Brill, 20 June 1986 
  Current Cognizant Programmer: R. Brill, 20 June 1986 
  Source Language: C

2 Operation

      The table of device information is searched for the appropriate
    tristimulus values.  If they are not found, then they are calculated
    and placed in the table.  The matrix of such values (and offsets) are
    then inverted and returned to the caller.  (This last step is
    inappropriate for a film recorder; another computation will be done
    when that device is implemented).  The subroutine is not simply a
    table of values; it has been designed so that the tables that do exist
    contain the data that can be measured, and the program calculates the
    data the is required. 

      The file also contains subroutines (used by XYZTORGB) to obtain the
    inverse, the determinant, and the cofactors of an arbitrary-sized
    array. 

    Example (in C): 

    get_dn(tristim,dn)
    double tristim[3];                          /*tristimulus values*/
    double dn[3];                               /*dn values for monitor*/
    {
      double rgb,xyz,ttr,offset;
  
      status = xyztorgb("TV",ttr,offset);       /*get ttr,offset for TV*/
      if (status = -1) {
        return;                                 /*no such device*/
      }
      for (rgb = 0; rgb < 3; rgb++) {           /*for each primary*/
        intensity = ttr[rgb][0] * tristim[0] +  /*get intensity*/
		    ttr[rgb][1] * tristim[1] +
		    ttr[rgb][2] * tristim[2] +
		    offset[rgb];
        dn[rgb] = pow(intensity,1/2.6);         /*get dn*/
      }
    }

2 Arguments

      The calling program performs the transformation for a monitor as
    follows: 

	    rgb = ttr * xyz + offset, 

      where xyz and rgb are vectors of tristimulus values and red, green,
    and blue   intensities, respectively.  The input dn for any phosphor
    is given by 

	    DN = I ** 1/2.6 (see HELP GIACONDA). 

      The function returns -1 if the device does not exist, 1 otherwise. 

$ Return
$!#############################################################################
