$!****************************************************************************
$!
$! Build proc for MIPL module phottest
$! VPACK Version 1.9, Monday, December 07, 2009, 16:51:48
$!
$! Execute by entering:		$ @phottest
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
$ write sys$output "*** module phottest ***"
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
$ write sys$output "Invalid argument given to phottest.com file -- ", primary
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
$   if F$SEARCH("phottest.imake") .nes. ""
$   then
$      vimake phottest
$      purge phottest.bld
$   else
$      if F$SEARCH("phottest.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake phottest
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @phottest.bld "STD"
$   else
$      @phottest.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create phottest.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack phottest.com -mixed -
	-s phottest.c pho_label.c -
	-p phottest.pdf phottestm.pdf phottestm.mdf phottestm_general.pdf -
	-i phottest.imake -
	-t tstphottest.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create phottest.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include "vicmain_c"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "pho.h"
#include <string.h>
#include <stdlib.h>

/* Program PHOTTEST  */

/* Zufallszahlengeneratoren : */
void zget_ran( long *seed1, float *randout);
double zgasdev(long *idum);

/* routines to write a photometric catalog :*/

/* phocat1 = old style photometric catalog as comming from PHOTOM : */
int phocatOpen(int VicarUnit, int nr, int *IbisUnit);
int phocatWrite(int IbisUnit, int row, int class_id, double IncAng, double EmAng, double PhasAng, double phoFuncVal, double eps);
int phocatClose(int VicarUnit, int IbisUnit);

#define SQRT2PI sqrt(2.0 * PI)
#define phoCAT_TYPE_1 "PHOCAT1"
#define phoCAT_TYPE "phocat"
#define phoCAT_NAME_LENGTH 7
#define PHOCAT_NC 			19
#define PHOCAT_COL_NAME_LENGTH 		15
#define PHOCAT_COL_UNIT_LENGTH		15
#
void main44()
{
  int cnt, def, i, j, l, l1, l2, ival, ival1, ival2, num, illMode, status;
  int class_id;
  int nr,row;
  float temp, *tempf;
  double eps, sigma, p, p_norm=0.0, mean_err=0.0, std_dev=0.0;
  double dtemp, dtemp1, dtemp2, dval, dval1, angLim;
  double *IncAng, *EmAng, *PhasAng, start[3], delta[3]; 
  double *phoFuncVal;
  long seed;
  char filenam[MAX_FILE_NAME_SIZE];
  char subcmd[9], cval[133], cval1[133], msg[133], msg1[133],
  keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char *pkeylist;
  PHO pho_obj;
  PHO_ILLUM Millum;

  char collName[PHOCAT_NC][PHOCAT_COL_NAME_LENGTH]={	
					"LAT",
					"LONG",
					"line",
					"samp",
					"ObjectLine",
					"ObjectSamp",
					"BoxLines",
					"BoxSamp",
					"LuminanceLat",
					"LuminanceLong",
					"IncidenceAngle",
					"EmissionAngle",
					"PhaseAngle",
					"DN_BoxMean",
					"Radiance",
					"I/F",
					"StandDev",
					"",
					"CLASS_ID"
					};

  char collUnits[PHOCAT_NC][PHOCAT_COL_NAME_LENGTH]={	
 					"degrees",
					"degrees",
					"pixels",
					"pixels",
					"pixels",
					"pixels",
					"pixels",
					"pixels",
					"degrees",
					"degrees",
					"degrees",
					"degrees",
					"degrees",
					"DN",
					"W/cm**2/str/nm",
					"",
					"",
					"",
					""
					};	


  char format_buf[PHOCAT_NC][IFMT_SIZE]={
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_REAL,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB,
					IFMT_DOUB ,
					IFMT_DOUB,
					IFMT_FULL
					};


  int VicarUnit, IbisUnit;
  float *IbisBuff=(float *)NULL;

  int iset=0;
  double zgasdev1;

/*  zvmessage(" program PHOTTEST", ""); */
  zvmessage( " ", "");

  zveaction("","");

  angLim = 89.5;

  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** main44: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("main44 abend");
  }

  zvp("CLASS_ID", &class_id, &cnt);
  strcpy( msg, " CLASS_ID = " );
  sprintf( cval1, " %3i", class_id);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoInit( &pho_obj);

/* get the photometric function and there input parameters from the PDF     */
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj);
  if(status != phoSUCCESS) zmabend("phoGetParms failed in main44");	

/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
  if(status != phoSUCCESS) zmabend("phoGetKeys failed in main44");	
/*  strcpy( msg, " Parameter number = " );
  sprintf( cval1, " %d", num);
  strcat( msg, cval1);
  zvmessage( msg, "");
*/
/* get the list of parameter keywords for the current photometric function : */

  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
	if (pkeylist == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(pkeylist);
	  zmabend("phottest abend");
	}
  pkeylist = (char *)keylist;

  status = phoGetKeys( pho_obj, pkeylist, &num);
  if(status != phoSUCCESS) zmabend("phoGetKeys failed in main44");	

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  if(status != phoSUCCESS) zmabend("phoGetFunc failed in main44");	
  strcpy( msg, " Function = " );
  strcat( msg, cval1);
  zvmessage( msg, "");
/*  zvmessage( " ", ""); */

  for (i = 0; i < num; i++) 
  {

    status = phoGetVal( pho_obj, keylist[i], &dval1);
  if(status != phoSUCCESS) zmabend("phoGetVal failed in main44");	
    strcpy( msg, "    ");
    strcat( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", dval1);
    strcat( msg, cval1);
    zvmessage( msg, "");
  }


/* additional experimental error ? */

  zvp("SIGMA", &temp, &cnt);
  if ( temp!=0)
  {
    sigma = (double )temp;
    zvp("SEED", &ival, &cnt);
    seed = (long )ival;

  }
  else
  {
    sigma = (double )0;
  }


/* reads in the function arguments from PDF and fill the illumination union: */

  zvpcnt("INC_ANG", &cnt);
  if ( cnt!=0)
  {
    j=cnt;
    nr=j;
    zvpcnt("EM_ANG", &cnt);
    if ( j!=cnt)
    {
      zvmessage( " Count of INC_ANC, EM_ANG and PHAS_ANG have to be equal", "");
      zmabend("phottest abend");
    }
    j=cnt;
    zvpcnt("PHAS_ANG", &cnt);
    if ( j!=cnt)
    {
      zvmessage( " Count of INC_ANC, EM_ANG and PHAS_ANG have to be equal", "");
      zmabend("phottest abend");
    }

    tempf = (float *)malloc(sizeof(float) * j);
	if (tempf == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(tempf);
	  zmabend("phottest abend");
	}
    IncAng = (double *)malloc(sizeof(double) * j);
	if (IncAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(IncAng);
	  zmabend("phottest abend");
	}
    EmAng = (double *)malloc(sizeof(double) * j);
	if (EmAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(EmAng);
	  zmabend("phottest abend");
	}
    PhasAng = (double *)malloc(sizeof(double) * j);
	if (PhasAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(PhasAng);
	  zmabend("phottest abend");
	}

    zvp("INC_ANG", tempf, &cnt);
    for (l=0; l<=j-1; l++) *(IncAng+l) = (double )(*(tempf+l));

    zvp("EM_ANG", tempf, &cnt);
    for (l=0; l<=j-1; l++) *(EmAng+l) = (double )(*(tempf+l));

    zvp("PHAS_ANG", tempf, &cnt);
    for (l=0; l<=j-1; l++) *(PhasAng+l) = (double )(*(tempf+l));

  }
  else
  {

    tempf = (float *)malloc(sizeof(float) * 3);
	if (tempf == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(tempf);
	  zmabend("phottest abend");
	}
    zvpcnt("START", &cnt);
    if ( cnt!=3 )
    {
      zvmessage( " missing angles", "");
      zmabend("phottest abend");
    }
    zvp("START", tempf, &cnt);
    for (l=0; l<=cnt-1; l++) start[l] = (double )(*(tempf+l));

    zvpcnt("DELTA", &cnt);
    if ( cnt!=3 )
    {
      zvmessage( " missing angles", "");
      zmabend("phottest abend");
    }
    zvp("DELTA", tempf, &cnt);
    for (l=0; l<=cnt-1; l++) delta[l] = (double )(*(tempf+l));

    ival = (angLim - start[0])/delta[0] + 1;
    ival1 = (angLim - start[1])/delta[1] + 1;
    ival2 = (180.0 - start[2])/delta[2] + 1;
    j = ival * ival1 * ival2;
    nr=j;

    IncAng = (double *)malloc(sizeof(double) * j);
	if (IncAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(IncAng);
	  zmabend("phottest abend");
	}
    EmAng = (double *)malloc(sizeof(double) * j);
	if (EmAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(EmAng);
	  zmabend("phottest abend");
	}
    PhasAng = (double *)malloc(sizeof(double) * j);
	if (PhasAng == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phottest error  error***","");
	  zvmessage("*** memory allocation failed ***","");
	  free(PhasAng);
	  zmabend("phottest abend");
	}

    i = -1;
    for (l=0; l<=ival-1; l++)
    {
      dtemp = start[0] + l * delta[0];
      for (l1=0; l1<=ival1-1; l1++)
      {
        dtemp1 = start[1] + l1 * delta[1];
        for (l2=0; l2<=ival2-1; l2++)
        {
	  i=i+1;
          dtemp2 = start[2] + l2 * delta[2];
	  *(IncAng+i) = dtemp;
	  *(EmAng+i) =  dtemp1;
	  *(PhasAng+i) =  dtemp2;
	  dtemp2 = COSPHAS((*(IncAng+i)),(*(EmAng+i)),(*(PhasAng+i)));
	  if (dtemp2 > 1 ) dtemp2 = 1.0;
/*	  if (dtemp2 < -1 + ANG_EPS ) dtemp2 = -1.0 + ANG_EPS; */
	  if (dtemp2 < -1 ) dtemp2 = -1.0; 
	  *(PhasAng+i) = RETURN_DEGREES(acos(dtemp2));

        }

      }

    }



  } /* end of else */

  zvmessage( " ", "");
  strcpy( msg, " Number of points =" );
  sprintf( cval1, " %10i", nr);
  strcat( msg, cval1);
  zvmessage( msg, "");


/* Screen output ? */

  if ( zvptst( "PRINT" ) )
  {
    zvmessage( " ", "");
  zvmessage( "   PT	 RAD	   INC_ANG    EM_ANG  PHAS_ANG	  EPS","");
    zvmessage( " ", "");
  }	



/* open the phocat file */

  zvp("OUT", filenam, &cnt);
  if (cnt <=0 ) zmabend("no outfile specified in the pdf");

  status = hwibis( &VicarUnit, &IbisUnit, filenam, nr, 1, 13, (collName+4), (format_buf+4), (collUnits+4), phoCAT_TYPE, 2, (collName+17), (format_buf+17), (collUnits+17), PHOCAT_COL_NAME_LENGTH);
      if (status <= 0)
      {
	  zvmessage(" *** error using hwibis ***", "");
	  status = IBISFileClose( IbisUnit, NULL );
	     if (status <= 0) IBISSignal (IbisUnit, status, 1 );
	  zmabend("phottest abend");
      }
  status = phoLabelWrite( VicarUnit, pho_obj);


/* fill the illumination union for the meassured illumination conditions :   */

  Millum.mode = illEllCos;
  Millum.type.sunshadow = illNoShadow;
  Millum.type.viewshadow = illNoShadow;

  for (l=0; l<=j-1; l++)
  {

    row = l+1;
    Millum.cos.inc = cos(RETURN_RADIANS((*(IncAng+l))));
    Millum.cos.em = cos(RETURN_RADIANS((*(EmAng+l))));
    Millum.cos.phas = cos(RETURN_RADIANS((*(PhasAng+l))));

    /* get the Bidirectional Reflectance Value : */

     status = phoBidiRef( pho_obj, &Millum, phoFuncVal );

     if ( sigma >= 1.0e-200 )
     {

	do
	{
	  eps = sigma * zgasdev( &seed );
	}while ( -eps >= *phoFuncVal);
	 *phoFuncVal = *phoFuncVal + eps; 

/*	sprintf( cval1, " %11.8e", eps); 
	strcat( msg, cval1);
*/
	mean_err =  eps + mean_err; 
	std_dev = std_dev + eps * eps ;
      }
      else eps=0;

     status = phocatWrite( IbisUnit, row, class_id, *(IncAng+l), *(EmAng+l), *(PhasAng+l), *phoFuncVal, eps); 
      if (status <= 0)
      {
	  zvmessage(" *** error using phocatWrite ***", "");
	  status = IBISFileClose( IbisUnit, NULL );
	     if (status <= 0) IBISSignal (IbisUnit, status, 1 );
	  zmabend("phottest abend");
      }



    /* Print to screen : */

    if ( zvptst( "PRINT" ) )
    {
      strcpy( msg, " " );
      sprintf( cval1, " %3i", l+1);
      strcat( msg, cval1);
      sprintf( cval1, " %10.3e", *phoFuncVal);
      strcat( msg, cval1);
      sprintf( cval1, " %9.4f", *(IncAng+l));
      strcat( msg, cval1);
      sprintf( cval1, " %9.4f", *(EmAng+l));
      strcat( msg, cval1);
      sprintf( cval1, " %9.4f", *(PhasAng+l)); 
      strcat( msg, cval1);
      if ( sigma >= 1.0e-200)
      {
        sprintf( cval1, " %10.3e", eps); 
        strcat( msg, cval1);
      }

      zvmessage( msg, "");
    }

  }

  if ( sigma >= 1.0e-200)
  {
    mean_err = mean_err / ((double )l);
    std_dev = sqrt( std_dev /((double )l) );

    zvmessage( " ", "");

    strcpy( msg, " sigma = " );
    sprintf( cval1, " %11.6e", sigma);
    strcat( msg, cval1);
    zvmessage( msg, "");
 
    if ( zvptst( "PRINT" ) )
    {
    	strcpy( msg, " mean error = " );
    	sprintf( cval1, " %11.6e", mean_err);
    	strcat( msg, cval1);
    	zvmessage( msg, "");
 
    	strcpy( msg, " standard deviation = " );
    	sprintf( cval1, " %11.6e", std_dev);
    	strcat( msg, cval1);
   	 zvmessage( msg, "");
     }

  }


  zvmessage( " ", "");

  status = phocatClose( VicarUnit, IbisUnit); 

  status = phoFree( pho_obj);
  status = phoFree( tempf);
  status = phoFree( IncAng);
  status = phoFree( EmAng);
  status = phoFree( PhasAng);

  return;
}



/************************************************************************
*									*
*		zget_ran						*
*									*
************************************************************************/

/* generates uniform deviation */


void zget_ran( 
	long *seed1, 
	float *randout)

{
  int i;
  srand((unsigned) *seed1);  
  i = ((unsigned int )rand());
  if (i >= 32767) *randout = (float) (i%32768)/32767.0;
  else *randout = (float) (i/32767.0);
  *seed1 = ((*seed1)*1103515245+12345);
  *seed1 = (unsigned int) (*seed1/65536)%32768;
  return;
}


/************************************************************************
*									*
*		zgasdev							*
*									*
************************************************************************/

/* generates normal deviation */

double zgasdev(
	long *idum)

{
  static int iset = 0;
  static double gset;
  double v1, v2, rsq, fac;
  float rand1, rand2;

    /* additional experimental normal deviated error with unit standard 
       deviation : 
       ( BOX-Muller method, ref.: W.H.Press, B.P.Flannery, S.A.Teukolsky, 
         W.T. Vetterling, Numerical Recipes, The Art of Scientific Computing, 
         FORTRAN Version, Cambridge University Pess, 1989, pp. 202-3 )       */


      if (iset==0)
      {
	do
        {
	  zrangen( idum, &rand1);
	  zrangen( idum, &rand2);

/*	  zget_ran( idum, &rand1);
	  zget_ran( idum, &rand2);
*/
          v1 = ( 2.0 * (double )rand1 - 1.0 );
          v2 = ( 2.0 * (double )rand2 - 1.0 );
	  rsq = v1 * v1  + v2 * v2;
	}while (rsq >= 1.0 || rsq == 0.0 );

	fac = sqrt( -2.0 * ( log( rsq ) )/ rsq );
	gset =  v1 * fac;
	iset = 1;
	return v2*fac;
      }
      else
      {
	iset = 0;
	return gset;
      }

}


/************************************************************************
*									*
*		hwibis						*
*									*
************************************************************************/

/* opens a photometric catalog file for writing as comming from PHOTOM */

/*******************/
/*** H W I B I S ***/
/*******************/

#define MAXCOL 1000 /* Defines maximum No. of columns expected */
#define MAXSTR 100  /* Maximum No. of characters in temp. string */

/* The necessary UNIX IO and VICAR routines required */
#include "ibisfile.h"
#include "ibiserrs.h"

hwibis(
                      /*            P-A-R-A-M-E-T-E-R-S                      */
                      /*            ===================                      */
int *vunit,           /* VICAR Unit No. - for IBIS error messages            */
int *iunit,           /* IBIS unit No. - for IBIS error messages etc         */
char *filename,       /* File name                                           */
int nr,               /* No. of rows                                         */
int n_imgrps,         /* No. of image groups                                 */
int n_ipqlf,          /* No. of qualifiers per "image" group                 */
char *ipqlf_na,       /* "Image" group qualifiers - names                    */
char *ipqlf_fo,       /* "Image" group qualifiers - formats                  */
char *ipqlf_un,       /* "Image" group qualifiers - units                    */
char *optype,         /* "Object point" type                                 */
int n_gqlf,           /* No. of qualifiers per "general" group               */
char *gqlf_na,        /* "General" group qualifier - names                   */
char *gqlf_fo,        /* "General" group qualifier - formats                 */
char *gqlf_un,        /* "General" group qualifier - units                   */
int strln            /* Maximum string length on input                       */
)

/********************************************************************

   Subroutine Description
   ======================
   The purpose of this routine is to open an IBIS2 file for a
   variety of point types (to be used by the Mars 96 project).
   It opens a column orientated IBIS2 file and sets up group
   names for each column and column sets. The general structure of
   this file is as follows (its effective DB fields)...

   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_1
   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_2
      "   "   "      "    ...
      "   "   "      "    ...
   line samp ipqlf1 ipqlf2... ipqlf`n' for IMAGE_`n'

   optype                              for OBJECT_TYPES

   gqlf1 gqlf2            ...  gqlf`n' for GENERAL_QLF


   where there are:
   n_imgrps                    `n' image groups
   n_ipqlf                     `n' image point qualifiers
   n_gqlf                      `n' general qualifiers for the points

   Groups "IMAGE_1..IMAGE_`N'",refer to common points between
          ------------------   images, and associated information
                               pertaining to "each" image e.g.
                               line, sample, quality of match, DN
                               value etc. So these are unique
                               characteristics to each image.

   Group "OBJECT_TYPE", refers to the type of point data held
         --------------        in the IBIS file, and is set
                               internally in the routine according
                               to the following rules:
   if optype="xyz" & no image groups     IBIS file type set to: "xyz"     
   if optype="latlon" & no image groups  IBIS file type set to: "latlon"   
   if optype="no_op" & image groups      IBIS file type set to: "tiepoint"
   if optype="latlon" & image groups     IBIS file type set to: "phocat"   
   for all other circumstances           IBIS file type set to: "no_type"  

   Group "GENERAL_QLF", pertains to common features between the
         -------------         the points e.g. a common point
                               between images will have the
                               same lon-lat-albedo on the
                               surface of the planet. So its
                               really a "general characteristic".

   N.B. In addition, by default, the unit types for line and samp,
   are "pixels", for X,Y,Z are "m", and Lon.Lat are "degrees".
   All other qualifiers can have their units entered via unit
   qualifier parameters - should you so desire.


   Example use:
   ============
   For example use, see the test program "thwibis.c", which sets
   up some group names, units, formats etc, and then closes the
   file. You can check out the contents of an IBIS label by
   doing a "label-list filename" on the IBIS file.


   History:
   ========
   Subroutine specified  15th July 1994 by:

   Dr B.Giese, DLR Berlin-Adlershof,
   Institute for Planetary Exploration, 12489 Berlin, Germany.


   Subroutine  written      Aug 1994    by ACC
              Modified 25th Aug 1994    by ACC
              Modified 18th May 1995    by ACC  - Added extra comments
	      Modified 13th Oct 1995    by FO   - Types into parameter list 
						- Main groups past subgroups
						- Accepts phocat as valid type

 
   Dr A.C.Cook, F. Oschuetz, DLR Berlin-Adlershof,
   Institute of Planetary Exploration, 12489 Berlin, Germany.  
**********************************************************************/




{
int nc;                             /* No. of columns in the file */
int i;                              /* Index variable */
int j;                              /* Index variable */
int count;                          /* A counter of the No. of columns set */
int col[MAXCOL];                    /* Contains column numbers for each group */
int status;                         /* Vicar status */
int vicar_unit;                     /* Vicar unit - internal use */
int ibis_unit;                      /* IBIS unit - internal use */
int n_opc;                          /* No. of object type columns */

char format_buf[MAXCOL][IFMT_SIZE]; /* Currently assume MAXCOL columns max */
char *fmt_ptr=(char *)0;            /* Pointer to this array */
char type[MAXSTR];                  /* Temporary string variable */
char name[MAXSTR];                  /* Temporary string variable */
char number[MAXSTR];                /* Temporary string variable */
char msgbuf[80];                    /* Message buffer */




                       /************************/
                       /* Initialise variables */
                       /************************/
n_opc=0;
if (strcmp(optype,"no_op")==0) n_opc=0;
if (strcmp(optype,"latlon")==0) n_opc=2;
if (strcmp(optype,"xyz")==0) n_opc=3;
if (strcmp(optype,"phocat")==0) n_opc=2;

nc=(2+n_ipqlf)*n_imgrps+n_opc+n_gqlf;        /* No. of columns */
status=0;                                    /* Status is O.K. */

if (nc>=MAXCOL)
  {
  zvmessage(msgbuf, "[hwibis] Exceeded maximum allowed number of columns");
  zabend();
  }





                    /******************************/
                    /* The formats of the columns */
                    /******************************/
/* IMAGE_1.. */     for (i=0;i<n_imgrps;i++)
                    {
			strcpy(format_buf[i*(2+n_ipqlf)],"REAL");
                        strcpy(format_buf[1+i*(2+n_ipqlf)],"REAL");
                        for(j=0;j<n_ipqlf;j++)
			{
			   strcpy(format_buf[j+2+i*(2+n_ipqlf)],
                           (ipqlf_fo+j*IFMT_SIZE));
			}
		    }

/* OBJECT_COORDS */ for(i=0;i<n_opc;i++)
                    {
			strcpy(format_buf[i+n_imgrps*(2+n_ipqlf)],"DOUB");
		    }

/* GENERAL_QLF */   for(i=0;i<n_gqlf;i++)
                    {
			strcpy(format_buf[i+n_opc+n_imgrps*(2+n_ipqlf)],
                      (gqlf_fo+i*IFMT_SIZE));
		    }





                       /*************************/
                       /* Opening the IBIS file */
                       /*************************/
/* VICAR unit */    zvunit(&vicar_unit,"none",1,"U_NAME",filename, NULL);

/* Format pntr */   fmt_ptr=format_buf[0];

/* IBIS unit */     status=IBISFileOpen(vicar_unit,&ibis_unit,IMODE_WRITE,nc,
                    nr,fmt_ptr,IORG_COLUMN);

/* Abort if req. */ if (status!=0) IBISSignal(ibis_unit,status,0);





                      /*****************************/
                      /* Define the IBIS file type */
                      /*****************************/
     strcpy(type,"no_type");  /* Default type */

     if ((strcmp(optype,"xyz")==0) && (n_imgrps==0)) strcpy(type,"xyz");

     if (strcmp(optype,"latlon")==0 && n_imgrps==0) strcpy(type,"latlon");

     if (n_imgrps!=0 && strcmp(optype,"no_op")==0) strcpy(type,"tiepoint");

     if (n_imgrps!=0 && strcmp(optype,"latlon")==0) strcpy(type,"phocat");

     if (n_imgrps!=0 && strcmp(optype,"phocat")==0) strcpy(type,"phocat");

     status=IBISFileSet(ibis_unit,IFILE_TYPE,type,0);
     if (status < 0) IBISSignalU(vicar_unit,status,0);






                   /******************************/
                    /* Define the sub-group names */
                    /******************************/
/* IMAGE_N: line */    if (n_imgrps > 0)
                         {for (i=0;i<n_imgrps;i++)
                           {col[i]=1+i*(2+n_ipqlf);}
                          count=IBISGroupNew(ibis_unit,"group",
                          "line",col,n_imgrps,0);}

/* IMAGE_N: samp */    if (n_imgrps > 0)
                         {for (i=0;i<n_imgrps;i++)
                           {col[i]=2+i*(2+n_ipqlf);}
                           count=IBISGroupNew(ibis_unit,"group",
                            "samp",col,n_imgrps,0);}

/* IMAGE_N: qualif*/   if (n_ipqlf > 0 && n_imgrps > 0)
                         {for (i=0;i<n_ipqlf;i++)
                           {
                           for (j=0;j<n_imgrps;j++)
                             {col[j]=3+i+j*(2+n_ipqlf);}

                           count=IBISGroupNew(ibis_unit,"group",
                            (ipqlf_na+i*strln),col,n_imgrps,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                             {status=IBISGroupModify(ibis_unit,"group",
                               (ipqlf_na+i*strln),"append",col,n_imgrps);
                             if (status < 0) IBISSignal(ibis_unit,status,0);}
                           }
                         }


/* "xyz": Obj. typ  */ if (strcmp(optype,"xyz")==0)
                           {
/*               X */      col[0]=1+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group","x",col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                             "X","append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
/*               Y */      col[0]=2+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group","y",col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                              "Y","append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
/*               Z */      col[0]=3+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group","z",col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                              "Z","append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
                           }


/* "latlon": Obj. typ */ if (strcmp(optype,"latlon")==0 || strcmp(optype,"phocat")==0)
                           {
/*               LAT */    col[0]=1+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group","lat",col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                              "LAT","append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
/*               LON */    col[0]=2+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group","lon",col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                              "LONG","append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
                           }



/* GENERAL_QLF: qualif*/ for (i=0;i<n_gqlf;i++)
                           {
                           col[0]=i+1+n_opc+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"group",
                            (gqlf_na+i*strln),col,1,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"group",
                              (gqlf_na+i*strln),"append",col,1);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
                           }





                    /******************************/
                    /* Define the sub-group units */
                    /******************************/

/* IMAGE_N: line */   if (n_imgrps > 0)
/*        & samp */     {for (i=0;i<n_imgrps;i++)
                          {col[2*i]=1+i*(2+n_ipqlf);
                          col[2*i+1]=2+i*(2+n_ipqlf);}
                          count=IBISGroupNew(ibis_unit,"unit",
                           "pixels",col,2*n_imgrps,0);
                          if (count==IBIS_GROUP_ALREADY_EXISTS)
                            {status=IBISGroupModify(ibis_unit,"unit",
                            "pixels","append",col,2*n_imgrps);
                            if (status < 0) IBISSignal(ibis_unit,status,0);}
                        }


/* IMAGE_N: qualif*/     if (n_ipqlf > 0 && n_imgrps > 0)
                           {for (i=0;i<n_ipqlf;i++)
                             {for (j=0;j<n_imgrps;j++)
                               {col[j]=3+i+j*(2+n_ipqlf);}
                               count=IBISGroupNew(ibis_unit,"unit",
                                     (ipqlf_un+i*strln),col,n_imgrps,0);
                               if (count==IBIS_GROUP_ALREADY_EXISTS)
                               {status=IBISGroupModify(ibis_unit,"unit",
                                 ipqlf_un+i*strln,"append",col,n_imgrps);
                               if (status < 0) IBISSignal(ibis_unit,status,0);}
                             }
                           }

/* "xyz: Obj. typ  */     if (strcmp(optype,"xyz")==0)
                            {
                            col[0]=1+n_imgrps*(2+n_ipqlf);
                            col[1]=2+n_imgrps*(2+n_ipqlf);
                            col[2]=3+n_imgrps*(2+n_ipqlf);
                            count=IBISGroupNew(ibis_unit,"unit","m",col,3,0);
                            if (count==IBIS_GROUP_ALREADY_EXISTS)
                              {status=IBISGroupModify(ibis_unit,"unit",
                              "m","append",col,3);
                              if (status < 0) IBISSignal(ibis_unit,status,0);}
                            }

/* "latlon: Obj. typ */  if(strcmp(optype,"latlon")==0 || 
/* "phocat: Obj. typ */	 strcmp(optype,"phocat")==0)
			 {
                           col[0]=1+n_imgrps*(2+n_ipqlf);
                           col[1]=2+n_imgrps*(2+n_ipqlf);
                           count=IBISGroupNew(ibis_unit,"unit",
                            "degrees",col,2,0);
                           if (count==IBIS_GROUP_ALREADY_EXISTS)
                             {status=IBISGroupModify(ibis_unit,"unit",
                             "degrees","append",col,2);
                           if (status < 0) IBISSignal(ibis_unit,status,0);}
                          }



/* GENERAL_QLF: qualifiers */ for (i=0;i<n_gqlf;i++)
                                {
                                col[0]=i+1+n_opc+n_imgrps*(2+n_ipqlf);
                                count=IBISGroupNew(ibis_unit,"unit",
                                 (gqlf_un+i*strln),col,1,0);
                                if (count==IBIS_GROUP_ALREADY_EXISTS)
                                   {status=IBISGroupModify(ibis_unit,"unit",
                                   (gqlf_un+i*strln),"append",col,1);
                                  if (status < 0) IBISSignal(ibis_unit,
                                      status,0);}
                                }










                       /**************************/
                       /* Define the main groups */
                       /**************************/
/* IMAGE_N */      for (i=0;i<n_imgrps;i++)
                     {
                     col[0]=1+i*(2+n_ipqlf);
                     col[1]=2+i*(2+n_ipqlf);
                     for (j=0;j<n_ipqlf;j++)
                       {col[j+2]=j+3+i*(2+n_ipqlf);}
                     strcpy(name,"IMAGE_");
                     j=i+1;
                     sprintf(number,"%d",j);
                     strcat(name,number);
                     count=IBISGroupNew(ibis_unit,"group",name,col,
                      (2+n_ipqlf),0);
                     }

/* OBJECT_COORDS */ if (n_opc > 0)
                     {for (i=0;i<n_opc;i++)
                       {
                       col[i]=n_imgrps*(2+n_ipqlf)+1+i;
                       }
                      count=IBISGroupNew(ibis_unit,"group",
                       "OBJECT_COORDINATES",col,n_opc,0);
                     }

/* GENERAL_QLF */      if (n_gqlf > 0) 
                         {for (i=0;i<n_gqlf;i++)
                           {col[i]=n_imgrps*(2+n_ipqlf)+n_opc+1+i;}
                            count=IBISGroupNew(ibis_unit,"group",
                             "GENERAL_QLF",col,n_gqlf,0);
                         }





 
/* Now return the Vicar unit */  *vunit=vicar_unit;
/* Then return the IBIS unit */  *iunit=ibis_unit;



return status;

}



/************************************************************************
*									*
*		phocatWrite						*
*									*
************************************************************************/

/* writes contens of the buffer into a photometric catalog file as comming */
/* from PHOTTEST (IBIS2 phocat file) and PHOTOM (IBIS1 file) */

int phocatWrite(
	int IbisUnit, 
	int row,
	int class_id,
	double IncAng,
	double EmAng, 
	double PhasAng, 
	double phoFuncVal,
	double eps)
{
  int status, count, col ,rowOld;
  char type[201]=phoCAT_TYPE, ctmp1[133];
  double ftemp[1];
  int itemp[1];
  double dtemp;
  int nrows=10;	/* number of rows which will be appended if necessary */
  status = 1;



/* check to see if there is the valid subtype - if so then O.K. */

  count = IBISFileGet( IbisUnit, IFILE_TYPE, type,  1, 1, phoCAT_NAME_LENGTH);
	if (count <= 0) IBISSignal (IbisUnit, count, 1 );

  if ( strcmp(type, phoCAT_TYPE) == 0 && strcmp(type, "tiepoint" ) == 0 )
  {
    zvmessage( " ", "");
    zvmessage(" *** phocatWrite error ***", "");
    zvmessage(" file isn't a PHOCAT file --> can't read as such", "");
    zvmessage( " ", "");
    status = IBISFileClose( IbisUnit, NULL );
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );
    zmabend("phottest abend");
  }
 


/* Appending new rows onto the end of the file if necessary */

  count = IBISFileGet( IbisUnit, IFILE_NR, &rowOld, 1, 1, 0 );
	if (count <= 0) IBISSignal (IbisUnit, count, 1 );
  if (row > rowOld)
  {
	status = phocatNewRow(IbisUnit, nrows);

  }



/* Write the data into the file (in a bad style) */

  strcpy( ctmp1, "IncidenceAngle" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  ftemp[0] = IncAng;
  status = IBISColumnWrite( IbisUnit, (char *)ftemp, col, row, 1);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );

  strcpy( ctmp1, "EmissionAngle" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  ftemp[0] = EmAng;
  status = IBISColumnWrite( IbisUnit, (char *)ftemp, col, row, 1);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );

  strcpy( ctmp1, "PhaseAngle" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  ftemp[0] = PhasAng;
  status = IBISColumnWrite( IbisUnit, (char *)ftemp, col, row, 1);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );

  strcpy( ctmp1, "I/F" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  ftemp[0] = phoFuncVal;
  status = IBISColumnWrite( IbisUnit, (char *)ftemp, col, row, 1);
	if (count <= 0) IBISSignal (IbisUnit, count, 1 );

  strcpy( ctmp1, "StandDev" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  ftemp[0] = eps;
  status = IBISColumnWrite( IbisUnit, (char *)ftemp, col, row, 1);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );

  strcpy( ctmp1, "CLASS_ID" );
  count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );
  itemp[0] = class_id;
  status = IBISColumnWrite( IbisUnit, (char *)itemp, col, row, 1);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );



  return status;
}


/************************************************************************
*									*
*		phocatNewRow						*
*									*
************************************************************************/

/* Append new rows onto the end of the file */

int phocatNewRow(
	int IbisUnit, 
	int nrows)

/********************************************************************
	* IbisUnit	IBIS Unit No.
	* nrows		numer of new rows to append
*********************************************************************/
{
  int status;


  status = IBISRowNew ( IbisUnit, 0, nrows);
	if (status <= 0) IBISSignal (IbisUnit, status, 1 );


  return status;
}



/************************************************************************
*									*
*		phocatClose						*
*									*
************************************************************************/

/* closes a photometric catalog file as comming from PHOTOM */

int phocatClose(
	int VicarUnit, 
	int IbisUnit)

/********************************************************************
	* IbisUnit	IBIS Unit No.
	* VicarUnit	VICAR Unit No.
*********************************************************************/
{
  int status;

/* Now we try to close-down the IBIS file */
 
 status = IBISFileClose(IbisUnit, ICLOSE_UDELETE);
    if (status != 1) IBISSignalU(VicarUnit, status, 0);

  return status;

}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create pho_label.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "pho.h"
#include "defines.h"

/*****************************************************************************
This routine is supposed to write information specific to a certain photometric
funtion into a property label called "PHOT". 
The application program has to include the follwing file:
pho.h

calling from C : status=phoLabelWrite(inp_unit, pho_obj);

******************************************************************************/

int phoLabelWrite(
	int inpunit, 
	PHO pho_obj)

{
  int status, e[phoMAX_PARAM_PER_FUNC + 1], i, j, num;
  float fphoPar;
  double phoPar[phoMAX_PARAM_PER_FUNC];
  char phoFuncName[phoMAX_FUNC_NAME_LENGTH+1];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 



/* get the photometric function name : */

  status = phoGetFunc( pho_obj, phoFuncName);
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetFunc failed ***","");
	    		zmabend("phoLabelWrite abend");
  }

  status=zladd(inpunit,"property","PHO_FUNC",
  phoFuncName,"format","string",
  "property","PHOT","ERR_ACT","SU","ERR_MESS",
  "property=PHOT keyword 'PHO_FUNC' was not added", NULL);

  e[0]=status;


/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
  if(status!=phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  }


/* get the parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, keylist, &num);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  }

  for ( j=0; j<num; j++ )  /* for all function parameters */
  {

    status = phoGetVal( pho_obj, keylist[j], (phoPar+j) );
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("phoLabelWrite abend");
    }

    fphoPar = (float )phoPar[j];

    status=zladd( inpunit, "property", &keylist[j],&fphoPar,
    "format","real", "property","PHOT", "ERR_ACT","SU",
    "ERR_MESS","property=PHOT parameter was not added", NULL);

    e[j+1]=status;
  }



  for (j=0; j<(num + 1); j++)
  {
    if (e[j]<1) 
    {
	status=e[j];
	zvmessage(" *** phoLabelWrite error: pho-label does not written " ,"");
    }
}

return status;
}




/*****************************************************************************
This routine is supposed to read information specific from a certain photometric
function from a property label called "PHOT". 
The application program has to include the following file:
pho.h

calling from C : status=phoLabelRead(inp_unit, pho_obj);

******************************************************************************/


int phoLabelRead(
	int inpunit, 
	PHO pho_obj)

{
  int status, e[phoMAX_PARAM_PER_FUNC + 1], i, j, k, x, y, nprop, nret, num;
  float fphoPar;
  double phoPar[phoMAX_PARAM_PER_FUNC];
  char property_names[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
  char phoFuncName[phoMAX_FUNC_NAME_LENGTH+1];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 

  nprop=MAX_PROPS;

/* 'zlpinfo' gets the information about all existing propertys */

  status=zlpinfo(inpunit,property_names,&nprop,"nret",&nret,
  "ulen",MAX_LABEL_KEY_SIZE+1, NULL);

  y=(-1);

  for (i=0; i<nprop; i++)
  {
     x=strcmp("PHOT",&property_names[i][0]);

/*  'zlget' returns all values of defined items of a 'PHOT' property, */
/*  if it exists */
 
    if (x==0) 
    { 

/* get the name of the photometric function */

	status=zlget(inpunit,"property","PHO_FUNC",
	phoFuncName,
	"format","string","property","PHOT","ERR_ACT","SU","ERR_MESS",
	"keyword 'PHO_FUNC' was not read", NULL);
	e[0]=status;


  	status = phoSetFunc( pho_obj, phoFuncName);
  	if ( status != phoSUCCESS ) 
  	{
	   zvmessage(" ","");
	   zvmessage("*** ERROR in phoLabelRead : phoSetFunc failed ***","");
	   zvmessage(" ","");
	   return status;
  	}

/* get the parameters names of the current photometric function : */

  	status = phoGetKeys( pho_obj, 0, &num); 
  	if(status!=phoSUCCESS)
 	{
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  	}

  	status = phoGetKeys( pho_obj, keylist, &num);
  	if(status!=1)
  	{
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("phoLabelWrite abend");
  	}


/* get the parameters names of the current photometric function : */

 	for (j=0; j<num; j++) /* for all function parameters */
  	{
	    status=zlget( inpunit, "property",  keylist[j],&fphoPar,
	   "format","double",  "property","PHOT",  "ERR_ACT","SU",
	   "ERR_MESS","property=PHOT parameter  was not read", NULL);
	   e[j+1]=status;
	   phoPar[j] = (double )fphoPar;
    	   status = phoSetVal( pho_obj, keylist[j], phoPar[j]);
    	   if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
    	   {
			zvmessage(" ","");
	    		zvmessage("***phoLabelWrite error***","");
	    		zvmessage("*** phoSetVal  9 failed ***","");
	    		zmabend("phoLabelWrite abend");
    	   }


  	}

	for (k=0;k<phoMAX_PARAM_PER_FUNC + 1;k++)
	{
		if (e[k]<1) status=e[k];
	}
	y=0;
	return status;
    } /* if (x==0) */

  } /* for (i=0; i<nprop; i++) */


  if (y==(-1))
  {
    zvmessage("there is no property label 'PHOT'","");
    status=(-2);
    return status;
  }

}
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create phottest.pdf
  process help=*


	parm OUT	type=(string,32) count=1	default=photcat.dat


	parm PHO_FUNC type=(string,32) count=1 		+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default=MINNAERT

	parm CLASS_ID	int	count=1			default=1	


        parm INC_ANG	real count=0:100 		default=--	+
			    				valid=(0.0:90)
        parm EM_ANG 	real count=0:100 		default=--	+
			    				valid=(0.0:90)
        parm PHAS_ANG 	real count=0:100 		default=--	+
							valid=(0.0:180)
	parm START	real count=0:3			default=(10,10,20)
	parm DELTA	real count=0:3			default=(30,30,70)

	parm SIGMA	real count=1			default=0.000001
	parm SEED	int  count=1			default=-9999999

	parm PRINT 	keyword count=0:1 valid=PRINT 	default=-- 


	parm ALBEDO 	real count=0:1 			default=--
	parm EXPONENT 	real count=0:1 valid=(0:1)	default=--
	parm A_VEVERKA 	real count=0:1 			default=--
	parm B_VEVERKA 	real count=0:1 			default=--
	parm C_VEVERKA 	real count=0:1 			default=--
	parm D_VEVERKA 	real count=0:1 			default=--
	parm MO_EXP1 	real count=0:1 			default=--
	parm MO_EXP2 	real count=0:1 			default=--
	parm E_BURATTI 	real count=0:1 			default=--
	parm DEN_SOIL 	real count=0:1 			default=--
	parm W_SOIL 	real count=0:1 valid=(0:1)	default=--
	parm HG1_SOIL 	real count=0:1 			default=--
	parm HG2_SOIL 	real count=0:1 			default=--
	parm HG_ASY_SOIL real count=0:1 		default=--
	parm LE1_SOIL 	real count=0:1 			default=--
	parm LE2_SOIL 	real count=0:1 			default=--
	parm H_SHOE 	real count=0:1 			default=--
	parm B_SHOE 	real count=0:1 			default=--
	parm H_CBOE 	real count=0:1 			default=--
	parm B_CBOE 	real count=0:1 			default=--
	parm THETA 	real count=0:1 			default=--
	parm COOK 	real count=0:1 			default=--
	parm TAU_ATM 	real count=0:1 			default=--
	parm W_ATM 	real count=0:1 valid=(0:1)	default=--
	parm HG1_ATM 	real count=0:1 			default=--
	parm IRV_EXP1 	real count=0:1 			default=--
	parm IRV_EXP2 	real count=0:1 			default=--




  end-proc

.title
VICAR program PHOTTEST generates synthetic photometric data and 
stores them in an IBIS2 photometric catalog (phocat).

.help
PURPOSE:

PHOTTEST  is a VICAR program which generates synthetic photometric data and 
stores them in an IBIS2 photometric catalog (phocat), for use in testing 
photometric functions in program PHOTFIT2.
	

FUNCTION:

PHOTTEST computes reflectances for a set of points and writes them into the 
catalog (phocat).  The points can either be specified directly by parameters 
INC_ANG, EMI_ANG, and PHAS_ANG, or can be computed in a grid using parameters 
START and DELTA. Shifts can be added to the data with gaussian distribution to 
simulate observational errors which will be an absolute error. 
The parameters used to generate the data are written into the property label.


PHOCAT FILE:

The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. The program PHOTTEST used only one IMAGE_* group. but tiepoint 
files using some IMAGE_* groups containing informations relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g.  CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 

abstract groups	      primitive groups    units	      formats	used in PHOTTEST

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used





EXECUTION:



In the SHELL-VICAR :

	 PHOTTEST 'OUT=PHOTCAT.DAT PARAMS'
  (no default values for the photometric parameters!)



In the command modus :

	TAE> PHOTTEST OUT=PHOTCAT.DAT PARAMS
   (no default values for the photometric parameters!)
 or
	TAE> PHOTTESTM OUT=PHOCAT.DAT PARAMS  
  (it helds for every photometric function its own parameter defaults)



In the tutor modus  --> menu-driven parameter input :

	TAE> tutor PHOTTESTM   
  (it helds for every photometric function its own parameter defaults)


tutor PHOTTESTM

There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  In this program, the menu points have the following meanings:

1. Select the first menu point to input the general parameters 
   such as the names of output catalog, the illumination conditions, and 
   so on.
2. Containing the parameters of the selected photometric function.
   The photometric function pertaining to this menu point and the name of this 
   menu point are changing depending on your input of the parameter PHO_FUNC
   in the first menu point.
3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'the user-specified name or  
					phottestm.par' 

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.



HELPS :

- You will get the common help contained in the ".mdf" file (phottestm.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (phottest.pdf   
   or phottestm.pdf) by processing of "help-help" applied to the program 
  (should be verry similary to the help of phottestm.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.




REQUIREMENTS and DEPENDENCIES:

LIBRARIES TO RUN PROGRAM:	LIB_TAE, LIB_RTL, LIB_P2SIB, LIB_P1SIB

INCLUDE-FILES:			vicmain_c, math.h, defines.h, 
				ibisfile.h, ibiserrs.h, 
				pho.h, pho_global.pdf

SUBROUTINES:			pho_routines.com
				PHOPDF.COM

	
GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

Name		Type			Description

PHO_FUNC_type 	string			It containes the names of the 
					valid photometric functions (to 
					pass into the menu).

pho_PROC_NAME 	string			Name of the main program 



INTERFACES:


INPUT:	


VICAR PDF-PARAMETERS:

Name	Type	count	Default	Description


1.) phottestm_general.pdf 	(menu point (1) of PHOTTEST)

OUT	char*32	 1	--	File name for the photometric catalog. 
							
PHO_FUNC char*32 1  HAPKE_86_LE2 Photometric function.

CLASS_ID int	1	1	class_id 

INC_ANG	real 	0:100	--	Incidence angles in degree.

EM_ANG	real 	0:100	--	Emission angles in degree.

PHAS_ANG real 	0:100	--	Phase angles in degree.

START		real 0:3	Starting point for grid in degrees.
				(INC_ANG,EM_ANG,PHAS_ANG)

DELTA		real 0:3	Increment for grid in degrees.
				(INC_ANG,EM_ANG,PHAS_ANG)

SIGMA		real		Simulated standard deviation.

SEED		int		Arbitrary seed for SIGMA.

PRINT		keyword		Screen output  of data generated.



2.) pho_"&PHO_FUNC".pdf	(menupoint (2) of PHOTTESTM) 
(photometry menu; the subPDFs pertaining to photometry are deliverd to the 
VICAR system by phopdf.com)

actuell  second menu	Name		default		Comment
(photom.function)	(funct.params)

LAMBERT			ALBEDO		1.0

MINNAERT		ALBEDO		1.0
			EXPONENT	0.6

IRVINE			EXPONENT	0.9
			IRV_EXP1	0.118
			IRV_EXP2	0.0039

VEVERKA			A_VEVERKA	0.997
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14

BURATTI1		ALBEDO		0.5
			B_VEVERKA	0.6
			E_BURATTI	0.14

BURATTI2		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
 			E_BURATTI	0.14

BURATTI3		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			E_BURATTI	0.14

MOSHER			A_VEVERKA	0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			MO_EXP1		0.5
			MO_EXP2		0.1

LUMME_BOWEL_HG1	W_SOIL	0.3
			H_SHOE		0.06
			DEN_SOIL	0.8
			THETA		20
			HG1_SOIL	-0.26

HAPKE_81_LE2		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_81_COOK		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3
			COOK		0.9

HAPKE_86_HG1		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		15.0
			HG1_SOIL	-0.26

HAPKE_86_HG2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.29
			HG2_SOIL	0.39
			HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.012
			THETA		20.0
			LE1_SOIL	0.29
			LE2_SOIL	0.39

HAPKE_HG1_DOM		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		1.0
			THETA		20.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			B_CBOE		1.0

REGNER_HAPKE_HG1	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35

ATMO_CORR_REGNER	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35





3.) common_save_par.pdf		(menupoint (3) of PHOTTESTM)
(see common_subpdf.com)

SAVE_PAR	string		Name for the TEA-parameter file


4.) common_proc_done.pdf	(menupoint (4) of PHOTTESTM)
(run the main program PHOTTEST; see common_subpdf.com)


	
OUTPUT:

FILES:				IBIS2 photometric catalog file 
				(see parameter OUT)

SCREEN OUTPUT:			Class identification, photometric function and 
				its parameters, # points, 
				mean error and  standard deviation depending of 
				parameter SIGMA,
				generated data (optional accordingly parameter 
				PRINT)



BACKGROUND AND REFERENCES :	see pho_routines.com and phopdf.com



SOFTWARE PLATFORM :		VICAR, TAE 5.2
				


HARDWARE PLATFORM :		VMS/UNIX(AXP/SOLARIS,SGI)
				No particular hardware required;
				tested on AXP/SOLARIS/SGI


PROGRAMMING LANGUAGE :		TCL , C	


HISTORY:			20-1-87 L.W.Kamp: 	original 
				Nov. '95 F.Oschuetz:	new written to be 
							portable to run on 
							AXP/SOLARIS/SGI 
							based on
							pho_routines.com and
							phopdf.com 


COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



.LEVEL1

.VARI OUT
Photometric catalog

.VARI PHO_FUNC
Photometric function type

.VARI CLASS_ID
class_id

.VARI INC_ANG
Incidence angles

.VARI EM_ANG
Emission angles

.VARI PHAS_ANG
Phase angles

.VARI START
Starting point for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI DELTA
Increment for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI SIGMA
Standard deviation

.VARI SEED
Arbitrary seed for SIGMA

.VARI PRINT
Screen output of data generated.

.VARI ALBEDO
albedo

VARI EXPONENT
Minnaert's konstant

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI DEN_SOIL
Hapke parameter

.VARI W_SOIL
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI H_CBOE
Hapke-Dominique parameter

.VARI B_CBOE
Hapke-Dominique parameter

.VARI THETA
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI TAU_ATM
Regner parameter

.VARI W_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.VARI IRV_EXP1
Irvine parameter

.VARI IRV_EXP2
Irvine parameter

.VARI SAVE_PAR
file name for par-file


.LEVEL2

.VARI OUT
Filename of the photometric catalog file (IBIS2) of type "phocat".
The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. 
The program PHOTTEST used only one IMAGE_* group IMAGE_1 containing 
informations relates to the image. GENERAL_QLF containes informations relates 
to the object point (e.g. CLASS_IDentifier). 
There are 19 columns in this file. All are empty exept folowing columns:
	IMAGE_1 & IncidenceAngle
	IMAGE_1 & EmissionAngle	 
	IMAGE_1 & PhaseAngle
	IMAGE_1 & I/F 		= reflectance values as computed from the 
				  photometric function.
	IMAGE_1 & StandDev
	GENERAL_QLF & CLASS_ID  = Class identification


.VARI PHO_FUNC
Photometric function :

	valid values :	LAMBERT, 
			MINNAERT, 
			IRVINE, 
			VEVERKA, 
			BURATTI1, 
			BURATTI2, 
			BURATTI3, 
			MOSHER, 
			LUMME_BOWEL_HG1, 
			HAPKE_81_LE2, 
			HAPKE_81_COOK, 
			HAPKE_86_HG1, 
			HAPKE_86_HG2, 
			HAPKE_86_LE2, 
			HAPKE_HG1_DOM, 
			REGNER_HAPKE_HG1, 
			ATMO_CORR_REGNER
.page
NOTE: When returning to the highest level of the menu (i.e. the MDF-file) you 
will see that the second selection point has been changed according to your 
input of PHO_FUNC.
For more see pho_routines.com and PHOPDF.COM 

.VARI CLASS_ID
The class_id nummerates the photometric functions. For using different fotometric functions or parameter sets.


.VARI INC_ANG
Incidence angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI EM_ANG
Emission angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI PHAS_ANG
Phase angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI START
Starting point for grid in degrees.
This parameter is only used if INC_ANG, IMI_ANG, and PHAS_ANG are not 
specified. It is used, togeter with parameter DELTA, to compute a grid of 
angles. This parameter specifies the starting values for the grid of: (Incidence, Emission, Azimuth) in that order. The Phase angle is computed for 
each point from the three given angles. 
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI DELTA
Increment for grid in degrees.
This parameter is only used if INC_ANG, EMIS_ANG, and PHAS_ANG are not 
specified. It is used, together with parameter START, to compute a grid of 
angles. This parameter specifies the increment values for the grid of: 
(Incidence, Emission, Azimuth) in that order.
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI SIGMA
Simulated standard deviation.
If this quantity is non_zero, then the program will simulate experimental error 
by adding an absolute "error" to each computed reflectance. These errors will 
be random with a gaussian probability distribution with standard deviation = 
SIGMA. Since reflectances cannot exceeded 1.0, this is also assumed to be a 
maximum on SIGMA. To check that the simulated errors are reasonable, the 
program prints out the computed mean error and standard deviation for the 
errors added. The "STD.DEV" should be about equal to SIGMA, and the "MN.ERR" 
should be close to 0 (much smaller than SIGMA).

.VARI SEED
Arbitrary seed for SIGMA.
This is an arbitrary number used to start the random number generator for 
SIGMA. It is only used if SIGMA is not zero. The default can be used unless the 
user wants to ensure that the random numbers are different from a previous 
run. The value of this parameter should be a large, odd, negative integer.

.VARI PRINT
Screen output of the data generated.
If this keyword is set to 'PRINT', a table will be printed to the screen. 

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI DEN_SOIL
Specific volume density of the soil.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI IRV_EXP1
Parameter of the Irvine photometric function.

.VARI IRV_EXP2
Parameter of the Irvine photometric function.

.VARI SAVE_PAR
This is the name for the TAE-parameter file containing all parameters 
needed to running the program. The default name is PHOTTEST.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

.end
$!-----------------------------------------------------------------------------
$ create phottestm.pdf
  procedure option=selftutor help=*

  !*******************************************************************
  ! If OPTIONS=SELFTUTOR (see the first line of this procedure), the
  ! TAE Terminal Monitor (TM) does not do the tutoring, i.e. when a
  ! user requests t(utor) for a SELFTUTOR procedure, the procedure
  ! is executed immediately to perform its own version of tutor.
  !*******************************************************************


	!*************************************************************
	! The process of the main program part is named
	! PHOTTEST and has to be declared here.
	!*************************************************************

!	process name=PHOTTEST
!	end-proc

	!*************************************************************
	! The following definitions/defaults will be used in 
	! the batch modus :
	!*************************************************************
	


  ! general input parameters :


	parm OUT	type=(string,32) count=1	default=photcat.dat


	! photometric functions :

	parm PHO_FUNC type=(string,32) count=1 		+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default=HAPKE_86_LE2

	parm CLASS_ID	int	count=1			default=1	


	! illumination conditions :

        parm INC_ANG	real count=0:100 		default=--	+
			    				valid=(0.0:90)
        parm EM_ANG 	real count=0:100 		default=--	+
			    				valid=(0.0:90)
        parm PHAS_ANG 	real count=0:100 		default=--	+
							valid=(0.0:180)
	parm START	real count=0:3			default=(10,10,20)
	parm DELTA	real count=0:3			default=(30,30,70)

	! observation errors :

	parm SIGMA	real count=1			default=0.0
	parm SEED	int  count=1			default=-9999999

	parm PRINT 	keyword count=0:1 valid=PRINT 	default=-- 


	! parameters of photometric functions

	parm ALBEDO 	real count=0:1 			default=--
	parm EXPONENT 	real count=0:1 valid=(0:1)	default=--
	parm A_VEVERKA 	real count=0:1 			default=--
	parm B_VEVERKA 	real count=0:1 			default=--
	parm C_VEVERKA 	real count=0:1 			default=--
	parm D_VEVERKA 	real count=0:1 			default=--
	parm MO_EXP1 	real count=0:1 			default=--
	parm MO_EXP2 	real count=0:1 			default=--
	parm E_BURATTI 	real count=0:1 			default=--
	parm DEN_SOIL 	real count=0:1 			default=--
	parm W_SOIL 	real count=0:1 valid=(0:1)	default=--
	parm HG1_SOIL 	real count=0:1 			default=--
	parm HG2_SOIL 	real count=0:1 			default=--
	parm HG_ASY_SOIL real count=0:1 		default=--
	parm LE1_SOIL 	real count=0:1 			default=--
	parm LE2_SOIL 	real count=0:1 			default=--
	parm H_SHOE 	real count=0:1 			default=--
	parm B_SHOE 	real count=0:1 			default=--
	parm H_CBOE 	real count=0:1 			default=--
	parm B_CBOE 	real count=0:1 			default=--
	parm THETA 	real count=0:1 			default=--
	parm COOK 	real count=0:1 			default=--
	parm TAU_ATM 	real count=0:1 			default=--
	parm W_ATM 	real count=0:1 valid=(0:1)	default=--
	parm HG1_ATM 	real count=0:1 			default=--
	parm IRV_EXP1 	real count=0:1 			default=--
	parm IRV_EXP2 	real count=0:1 			default=--


      !*******************************************************************
      ! local variable for the photometric parameters to enable different
      !	default values of the same parameter in different photometric 
      ! functions :
      !*******************************************************************

	local ALBEDO_count 	int 
	local EXPONENT_count 	int 
	local A_VEVERKA_count 	int
	local B_VEVERKA_count 	int
	local C_VEVERKA_count	int
	local D_VEVERKA_count 	int 
	local MO_EXP1_count 	int 
	local MO_EXP2_count 	int
	local E_BURATTI_count 	int
	local DEN_SOIL_count 	int
	local W_SOIL_count 	int
	local HG1_SOIL_count 	int
	local HG2_SOIL_count 	int
	local HG_ASY_SOIL_cou 	int
	local LE1_SOIL_count 	int
	local LE2_SOIL_count 	int
	local H_SHOE_count 	int
	local B_SHOE_count 	int
	local H_CBOE_count 	int
	local B_CBOE_count 	int
	local THETA_count 	int
	local COOK_count 	int
	local TAU_ATM_count 	int
	local W_ATM_count 	int
	local HG1_ATM_count 	int
	local IRV_EXP1_count	int
	local IRV_EXP2_count  	int




  ! the name of the parameter file :

	parm SAVE_PAR	 type=(string,40) count=1       default=last.par
	parm MAIN_PROC_NAME	type=string count=1	default=PHOTTESTM



  ! for running the main program: 

	parm readparam	keyword		 count=0:1	valid=read def=--

	!*******************************************************************
	! The globals "$MENUOPT" and "SYSCHAR" are used in this proc,
	! so we have to declare they here with the command REFGBL
	!*******************************************************************

	refgbl $menuopt
	refgbl $syschar

  body


  !*************************************************************************
  ! "pho_global.pdf" is being executed in the following line. 
  ! In this PDF the global PHO_FUNC_type is defined. 
  !*************************************************************************

  pho_global PHO_FUNC_type = "&PHO_FUNC" pho_PROC_NAME="&_PROC"



  ! for different photometric funtions :

  !*******************************************************************
  ! set the value oft local variable parameter-count to the beginning 
  ! parameter_count:
  !*******************************************************************

  let ALBEDO_count 	=$count(ALBEDO)
  let EXPONENT_count 	=$count(EXPONENT)
  let A_VEVERKA_count 	=$count(A_VEVERKA)
  let B_VEVERKA_count 	=$count(B_VEVERKA)
  let C_VEVERKA_count	=$count(C_VEVERKA)
  let D_VEVERKA_count 	=$count(D_VEVERKA)
  let MO_EXP1_count 	=$count(MO_EXP1)
  let MO_EXP2_count 	=$count(MO_EXP2)
  let E_BURATTI_count 	=$count(E_BURATTI)
  let DEN_SOIL_count 	=$count(DEN_SOIL)
  let W_SOIL_count 	=$count(W_SOIL)
  let HG1_SOIL_count 	=$count(HG1_SOIL)
  let HG2_SOIL_count 	=$count(HG2_SOIL)
  let HG_ASY_SOIL_cou 	=$count(HG_ASY_SOIL)
  let LE1_SOIL_count 	=$count(LE1_SOIL)
  let LE2_SOIL_count 	=$count(LE2_SOIL)
  let H_SHOE_count 	=$count(H_SHOE)
  let B_SHOE_count 	=$count(B_SHOE)
  let H_CBOE_count 	=$count(H_CBOE)
  let B_CBOE_count 	=$count(B_CBOE)
  let THETA_count 	=$count(THETA)
  let COOK_count 	=$count(COOK)
  let TAU_ATM_count 	=$count(TAU_ATM)
  let W_ATM_count 	=$count(W_ATM)
  let HG1_ATM_count 	=$count(HG1_ATM)
  let IRV_EXP1_count	=$count(IRV_EXP1)
  let IRV_EXP2_count  	=$count(IRV_EXP2)




    !*************************************************************************
    ! _TUTOR (type=integer) is an implicit local variable in a procedure
    ! with OPTIONS=SELFTUTOR (see first line of this proc). When TAE
    ! receives an initial tutor request for a procedure declared as selftutor,
    ! _TUTOR is set to one, otherwise it is set to zero.
    !*************************************************************************

    if (_tutor=1)

  	!*********************************************************************
  	! dummy files in case the procs aren't called :
  	! The save-variable (sub-)commmand is used to save the specified
  	! variables into the save file.
 	!
 	! Command:
 	! SAVE-VARIABLE FILE = save_file_name, VARIABLE = variable_list
  	!**********************************************************************

  	!**********************************************************************
  	! Saving the parameter-file with the photometric function parameters
  	! either the inputed parameters or if did not parameter input - the  
  	! default parameter for the actual photometric function :
  	!********************************************************************** 
	!**********************************************************************
	! At this position you can input function-specific default values 
	! for the function parameter for the tutor modus : 
	!**********************************************************************


  ! LAMBERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0

          save-var pho_lambert.par, ALBEDO

	if (ALBEDO_count=0)	let ALBEDO=--


  ! MINNAERT:

	if (ALBEDO_count=0)				let ALBEDO=1.0
	if (EXPONENT_count=0)				let EXPONENT=0.6

          save-var pho_minnaert.par, (		+
				ALBEDO,		+
				EXPONENT	+
				)


	if (ALBEDO_count=0)	let ALBEDO=--
	if (EXPONENT_count=0)	let EXPONENT=--

  ! IRVINE :

	if (EXPONENT_count=0)				let EXPONENT=0.9
	if (IRV_EXP1_count=0)				let IRV_EXP1=0.118
	if (IRV_EXP2_count=0)				let IRV_EXP2=0.0039

          save-var pho_irvine.par,  (		+
				EXPONENT,	+
				IRV_EXP1,	+
				IRV_EXP2	+
				)

	if (EXPONENT_count=0)	let EXPONENT=--
	if (IRV_EXP1_count=0)	let IRV_EXP1=--
	if (IRV_EXP2_count=0)	let IRV_EXP2=--

  ! VEVERKA :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.997
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14

          save-var pho_veverka.par, (		+
				A_VEVERKA,	+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA	+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--

  ! BURATTI1 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14

          save-var pho_buratti1.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! BURATTI2 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (E_BURATTI_count=0) 			let E_BURATTI=0.14

          save-var pho_buratti2.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				C_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! BURATTI3 :

	if (ALBEDO_count=0) 				let ALBEDO=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=-0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (E_BURATTI_count=0) 				let E_BURATTI=0.14

          save-var pho_buratti3.par, (		+
				ALBEDO,		+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA,	+
				E_BURATTI	+
				)

	if (ALBEDO_count=0) 	let ALBEDO=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (E_BURATTI_count=0) 	let E_BURATTI=--

  ! MOSHER :

	if (A_VEVERKA_count=0) 				let A_VEVERKA=0.5
	if (B_VEVERKA_count=0) 				let B_VEVERKA=0.6
	if (C_VEVERKA_count=0) 				let C_VEVERKA=0.003
	if (D_VEVERKA_count=0) 				let D_VEVERKA=0.14
	if (MO_EXP1_count=0) 				let MO_EXP1=0.5
	if (MO_EXP2_count=0) 				let MO_EXP2=0.1

          save-var pho_mosher.par, (		+
				A_VEVERKA,	+
				B_VEVERKA,	+
				C_VEVERKA,	+
				D_VEVERKA,	+
				MO_EXP1,	+
				MO_EXP2		+
				)

	if (A_VEVERKA_count=0) 	let A_VEVERKA=--
	if (B_VEVERKA_count=0) 	let B_VEVERKA=--
	if (C_VEVERKA_count=0) 	let C_VEVERKA=--
	if (D_VEVERKA_count=0) 	let D_VEVERKA=--
	if (MO_EXP1_count=0) 	let MO_EXP1=--
	if (MO_EXP2_count=0) 	let MO_EXP2=--

  ! LUMME_BOWEL_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (DEN_SOIL_count=0) 				let DEN_SOIL=0.8
	if (THETA_count=0) 				let THETA=20
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26

          save-var pho_lumme_bowel_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				DEN_SOIL,	+
				THETA,		+
				HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (DEN_SOIL_count=0) 	let DEN_SOIL=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--

  ! HAPKE_81_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3

          save-var pho_hapke_81_le2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				LE1_SOIL,	+
				LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--

  ! HAPKE_81_COOK :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.3
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.3
	if (COOK_count=0) 				let COOK=0.9

          save-var pho_hapke_81_cook.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				LE1_SOIL,	+
				LE2_SOIL,	+
				COOK		+
				)


	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--
	if (COOK_count=0) 	let COOK=--

  ! HAPKE_86_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=15.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.26

          save-var pho_hapke_86_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--

  ! HAPKE_86_HG2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0) 				let HG1_SOIL=-0.29
	if (HG2_SOIL_count=0) 				let HG2_SOIL=0.39
	if (HG_ASY_SOIL_cou=0) 				let HG_ASY_SOIL=1

          save-var pho_hapke_86_hg2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				HG2_SOIL,	+
				HG_ASY_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0) 	let HG1_SOIL=--
	if (HG2_SOIL_count=0) 	let HG2_SOIL=--
	if (HG_ASY_SOIL_cou=0) let HG_ASY_SOIL=--

  ! HAPKE_86_LE2 :

	if (W_SOIL_count=0) 				let W_SOIL=0.21
	if (H_SHOE_count=0) 				let H_SHOE=0.07
	if (B_SHOE_count=0) 				let B_SHOE=2.012
	if (THETA_count=0) 				let THETA=20.0
	if (LE1_SOIL_count=0) 				let LE1_SOIL=0.29
	if (LE2_SOIL_count=0) 				let LE2_SOIL=0.39

          save-var pho_hapke_86_le2.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				LE1_SOIL,	+
				LE2_SOIL	+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (LE1_SOIL_count=0) 	let LE1_SOIL=--
	if (LE2_SOIL_count=0) 	let LE2_SOIL=--

  ! HAPKE_HG1_DOM :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (H_CBOE_count=0) 				let H_CBOE=0.06
	if (B_CBOE_count=0) 				let B_CBOE=1.0

          save-var pho_hapke_hg1_dom.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				H_CBOE,		+
				B_CBOE		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (H_CBOE_count=0) 	let H_CBOE=--
	if (B_CBOE_count=0) 	let B_CBOE=--

  ! REGNER_HAPKE_HG1 :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (W_ATM_count=0)				let W_ATM=0.78
	if (TAU_ATM_count=0)				let TAU_ATM=0.05
	if (HG1_ATM_count=0)				let HG1_ATM=0.35

          save-var pho_regner_hapke_hg1.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				W_ATM,		+
				TAU_ATM,	+
				HG1_ATM		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (W_ATM_count=0)	let W_ATM=--
	if (TAU_ATM_count=0)	let TAU_ATM=--
	if (HG1_ATM_count=0)	let HG1_ATM=--

  ! ATMO_CORR_REGNER :

	if (W_SOIL_count=0) 				let W_SOIL=0.3
	if (H_SHOE_count=0) 				let H_SHOE=0.06
	if (B_SHOE_count=0) 				let B_SHOE=2.0
	if (THETA_count=0) 				let THETA=20.0
	if (HG1_SOIL_count=0)				let HG1_SOIL=-0.26
	if (W_ATM_count=0)				let W_ATM=0.78
	if (TAU_ATM_count=0)				let TAU_ATM=0.05
	if (HG1_ATM_count=0)				let HG1_ATM=0.35

          save-var pho_atmo_corr_regner.par, (	+
				W_SOIL,		+
				H_SHOE,		+
				B_SHOE,		+
				THETA,		+
				HG1_SOIL,	+
				W_ATM,		+
				TAU_ATM,	+
				HG1_ATM		+
				)

	if (W_SOIL_count=0) 	let W_SOIL=--
	if (H_SHOE_count=0) 	let H_SHOE=--
	if (B_SHOE_count=0) 	let B_SHOE=--
	if (THETA_count=0) 	let THETA=--
	if (HG1_SOIL_count=0)	let HG1_SOIL=--
	if (W_ATM_count=0)	let W_ATM=--
	if (TAU_ATM_count=0)	let TAU_ATM=--
	if (HG1_ATM_count=0)	let HG1_ATM=--





	!*******************************************************************
	! other dummy files in case the procs aren't called :
	!*******************************************************************


          save-var &"_PROC"_general.par, (	+
				OUT, 		+
				PHO_FUNC,	+
				INC_ANG,	+
				EM_ANG,		+
				PHAS_ANG,	+
				CLASS_ID,	+
				START,		+
				DELTA,		+
				SIGMA,		+
				SEED,		+
				PRINT		+
				)


	  save-var common_save_par.par, SAVE_PAR	


	  save-var common_proc_name.par, MAIN_PROC_NAME





	  !****************************************************************
	  ! The string global variable $MENUOPT allows suppression of the
	  ! "Press RETURN key for menu" message. Hence, when typing
	  ! "VICAR> tutor PHOTTESTM" the menu can be entered directly 
	  ! without the need to press the RETURN key first.
	  !****************************************************************

	    let $menuopt="NO_PRESS_FOR_MENU"

	  !****************************************************************
	  ! "menu" puts VICAR into the Menu Mode, i.e. here we enter the
	  !  menu of 'PHOTTESTM.MDF'
	  !****************************************************************

	    menu &"_PROC".mdf

   end-if

    write "returned into main-procedure &_PROC"

! passing the parameters from tutor mode and then running the process :

	!***************************************************************
	! $COUNT returns the current number of elements for "readparam":
	! If the keyword parameter "readparam" has been set to 'read',
	! $count(readparam)=1, else $count(readparam)<>1 (see below).
	!***************************************************************

	if ($count(readparam)=1) 


		!****************************************************
		! The restore-parms (sub-)command is used to restore
		! only the parameters in the save file.
		!
		! Command:
		! RESTORE-PARMS FILE = save_file_name
		!****************************************************

		restore-parm common_save_par.par

		restore-parm &"_PROC"_general.par

		if ("&PHO_FUNC" = "LAMBERT" )			+ 
					restore-parm pho_lambert.par
		if ("&PHO_FUNC" = "MINNAERT")			+  
					restore-parm pho_minnaert.par
		if ("&PHO_FUNC" = "IRVINE")			+  
					restore-parm pho_irvine.par
		if ("&PHO_FUNC" = "VEVERKA")			+ 
					restore-parm pho_veverka.par
		if ("&PHO_FUNC" = "BURATTI1")			+ 
					restore-parm pho_buratti1.par
		if ("&PHO_FUNC" = "BURATTI2")			+ 
					restore-parm pho_buratti2.par
		if ("&PHO_FUNC" = "BURATTI3")			+  
					restore-parm pho_buratti3.par
		if ("&PHO_FUNC" = "MOSHER") 			+
					restore-parm pho_mosher.par
		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1") 		+
					restore-parm pho_lumme_bowel_hg1.par
		if ("&PHO_FUNC" = "HAPKE_81_LE2")		+
  					restore-parm pho_hapke_81_le2.par
		if ("&PHO_FUNC" = "HAPKE_81_COOK")  		+
 					restore-parm pho_hapke_81_cook.par
		if ("&PHO_FUNC" = "HAPKE_86_HG1")		+
 					restore-parm pho_hapke_86_hg1.par
		if ("&PHO_FUNC" = "HAPKE_86_HG2")		+
 					restore-parm pho_hapke_86_hg2.par
		if ("&PHO_FUNC" = "HAPKE_86_LE2") 		+
 					restore-parm pho_hapke_86_le2.par
		if ("&PHO_FUNC" = "HAPKE_HG1_DOM") 		+
					restore-parm pho_hapke_hg1_dom.par
		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")  		+
					restore-parm pho_regner_hapke_hg1.par
		if ("&PHO_FUNC" = "ATMO_CORR_REGNER")		+
 					restore-parm pho_atmo_corr_regner.par
                                                                             



		save-var "&SAVE_PAR", (		+
			OUT, 			+
			PHO_FUNC,		+
			CLASS_ID,		+
			INC_ANG,		+
			EM_ANG,			+
			PHAS_ANG,		+
			START,			+
			DELTA,			+
			SIGMA,			+
			SEED,			+
			PRINT,			+
			ALBEDO,			+ 
			EXPONENT,		+
      			A_VEVERKA, 		+
       			B_VEVERKA, 		+
       			C_VEVERKA, 		+
       			D_VEVERKA,		+ 
       			MO_EXP1, 		+
       			MO_EXP2, 		+
       			E_BURATTI, 		+
       			DEN_SOIL, 		+
       			W_SOIL, 		+
       			HG1_SOIL, 		+
       			HG2_SOIL, 		+
       			HG_ASY_SOIL,		+
       			LE1_SOIL, 		+
       			LE2_SOIL, 		+
       			H_SHOE, 		+
       			B_SHOE, 		+
       			H_CBOE, 		+
       			B_CBOE, 		+
       			THETA ,			+
       			COOK,			+
       			TAU_ATM,		+
       			W_ATM, 			+
       			HG1_ATM,		+
       			IRV_EXP1, 		+
       			IRV_EXP2, 		+  
			SAVE_PAR		+
 				)

		save-var "pho_temp_par", (	+
			OUT, 			+
			PHO_FUNC,		+
			CLASS_ID,		+
			INC_ANG,		+
			EM_ANG,			+
			PHAS_ANG,		+
			START,			+
			DELTA,			+
			SIGMA,			+
			SEED,			+
			PRINT,			+
			ALBEDO,			+ 
			EXPONENT,		+
      			A_VEVERKA, 		+
       			B_VEVERKA, 		+
       			C_VEVERKA, 		+
       			D_VEVERKA,		+ 
       			MO_EXP1, 		+
       			MO_EXP2, 		+
       			E_BURATTI, 		+
       			DEN_SOIL, 		+
       			W_SOIL, 		+
       			HG1_SOIL, 		+
       			HG2_SOIL, 		+
       			HG_ASY_SOIL,		+
       			LE1_SOIL, 		+
       			LE2_SOIL, 		+
       			H_SHOE, 		+
       			B_SHOE, 		+
       			H_CBOE, 		+
       			B_CBOE, 		+
       			THETA ,			+
       			COOK,			+
       			TAU_ATM,		+
       			W_ATM, 			+
       			HG1_ATM,		+
       			IRV_EXP1, 		+
       			IRV_EXP2 		+  
 				)



		!*********************************************
		! The parameter and their values can bee 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*************************************************
		! ... and the main program PHOTTEST is run
		!*************************************************

		PHOTTEST |pho_temp_par|

	end-if



! running the process in the batch modus :

	if ($count(readparam) <> 1) 

		!***********************************************************
		! At this position you can input function-specific default 
		! values for the function parameter for the batch modus : 
		!***********************************************************



		if ( "&PHO_FUNC" = "LAMBERT" )
			if (ALBEDO_count=0)		let ALBEDO=1.0
		end-if

		if ("&PHO_FUNC" = "MINNAERT")
			if (ALBEDO_count=0)		let ALBEDO=1.0
			if (EXPONENT_count=0)		let EXPONENT=0.6
		end-if

		if ("&PHO_FUNC" = "IRVINE")
			if (EXPONENT_count=0)		let EXPONENT=0.9
			if (IRV_EXP1_count=0)		let IRV_EXP1=0.118
			if (IRV_EXP2_count=0)		let IRV_EXP2=0.0039
		end-if

		if ("&PHO_FUNC" = "VEVERKA")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.997
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI1")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI2")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "BURATTI3")
			if (ALBEDO_count=0) 		let ALBEDO=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (E_BURATTI_count=0) 		let E_BURATTI=0.14
		end-if

		if ("&PHO_FUNC" = "MOSHER")
			if (A_VEVERKA_count=0) 		let A_VEVERKA=0.5
			if (B_VEVERKA_count=0) 		let B_VEVERKA=0.6
			if (C_VEVERKA_count=0) 		let C_VEVERKA=0.003
			if (D_VEVERKA_count=0) 		let D_VEVERKA=0.14
			if (MO_EXP1_count=0) 		let MO_EXP1=0.5
			if (MO_EXP2_count=0) 		let MO_EXP2=0.1
		end-if

		if ("&PHO_FUNC" = "LUMME_BOWEL_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (DEN_SOIL_count=0) 		let DEN_SOIL=0.8
			if (THETA_count=0) 		let THETA=20
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
		end-if

		if ("&PHO_FUNC" = "HAPKE_81_COOK")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.3
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.3
			if (COOK_count=0) 		let COOK=0.9
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=15.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.26
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_HG2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0) 		let HG1_SOIL=-0.29
			if (HG2_SOIL_count=0) 		let HG2_SOIL=0.39
			if (HG_ASY_SOIL_cou=0) 		let HG_ASY_SOIL=1
		end-if

		if ("&PHO_FUNC" = "HAPKE_86_LE2")
			if (W_SOIL_count=0) 		let W_SOIL=0.21
			if (H_SHOE_count=0) 		let H_SHOE=0.07
			if (B_SHOE_count=0) 		let B_SHOE=2.012
			if (THETA_count=0) 		let THETA=20.0
			if (LE1_SOIL_count=0) 		let LE1_SOIL=0.29
			if (LE2_SOIL_count=0) 		let LE2_SOIL=0.39
		end-if

		if ("&PHO_FUNC" = "HAPKE_HG1_DOM")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=1.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (H_CBOE_count=0) 		let H_CBOE=0.06
			if (B_CBOE_count=0) 		let B_CBOE=1.0
		end-if

		if ("&PHO_FUNC" = "REGNER_HAPKE_HG1")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (W_ATM_count=0)		let W_ATM=0.78
			if (TAU_ATM_count=0)		let TAU_ATM=0.05
			if (HG1_ATM_count=0)		let HG1_ATM=0.35
		end-if

		if ("&PHO_FUNC" = "ATMO_CORR_REGNER")
			if (W_SOIL_count=0) 		let W_SOIL=0.3
			if (H_SHOE_count=0) 		let H_SHOE=0.06
			if (B_SHOE_count=0) 		let B_SHOE=2.0
			if (THETA_count=0) 		let THETA=20.0
			if (HG1_SOIL_count=0)		let HG1_SOIL=-0.26
			if (W_ATM_count=0)		let W_ATM=0.78
			if (TAU_ATM_count=0)		let TAU_ATM=0.05
			if (HG1_ATM_count=0)		let HG1_ATM=0.35
		end-if


		save-var "pho_temp_par", (	+
			OUT, 			+
			PHO_FUNC,		+
			CLASS_ID,		+
			INC_ANG,		+
			EM_ANG,			+
			PHAS_ANG,		+
			START,			+
			DELTA,			+
			SIGMA,			+
			SEED,			+
			PRINT,			+
			ALBEDO,			+ 
			EXPONENT,		+
      			A_VEVERKA, 		+
       			B_VEVERKA, 		+
       			C_VEVERKA, 		+
       			D_VEVERKA,		+ 
       			MO_EXP1, 		+
       			MO_EXP2, 		+
       			E_BURATTI, 		+
       			DEN_SOIL, 		+
       			W_SOIL, 		+
       			HG1_SOIL, 		+
       			HG2_SOIL, 		+
       			HG_ASY_SOIL,		+
       			LE1_SOIL, 		+
       			LE2_SOIL, 		+
       			H_SHOE, 		+
       			B_SHOE, 		+
       			H_CBOE, 		+
       			B_CBOE, 		+
       			THETA ,			+
       			COOK,			+
       			TAU_ATM,		+
       			W_ATM, 			+
       			HG1_ATM,		+
       			IRV_EXP1, 		+
       			IRV_EXP2 		+  
 				)




		!*********************************************
		! The parameter and their values can bee 
		! displayed on the terminal ...
		!*********************************************

!		display-parms

		!*********************************************
		! ... and the main program TPHO_ROUTINES_C is run
		!*********************************************

		PHOTTEST |pho_temp_par|

	end-if



! delete the temporary .par files only for the tutor modud :

	if ($count(readparam) = 1)  

	    if ($syschar(1) = "UNIX")

	       ush /bin/rm -f phottestm_general.par;+
		   /bin/rm -f pho_temp_par.par;

	       ush /bin/rm -f pho_lambert.par;+
		   /bin/rm -f pho_minnaert.par;+
		   /bin/rm -f pho_irvine.par;+
		   /bin/rm -f pho_veverka.par;+
		   /bin/rm -f pho_buratti1.par;+
		   /bin/rm -f pho_buratti2.par;+
		   /bin/rm -f pho_buratti3.par;+
		   /bin/rm -f pho_mosher.par;+
		   /bin/rm -f pho_lumme_bowel_hg1.par;+
		   /bin/rm -f pho_hapke_81_le2.par;+
		   /bin/rm -f pho_hapke_81_cook.par;+
		   /bin/rm -f pho_hapke_86_hg1.par;+
		   /bin/rm -f pho_hapke_86_hg2.par;+
		   /bin/rm -f pho_hapke_86_le2.par;+
		   /bin/rm -f pho_hapke_hg1_dom.par;+
		   /bin/rm -f pho_regner_hapke_hg1.par;+
		   /bin/rm -f pho_atmo_corr_regner.par;

	       ush /bin/rm -f common_proc_name.par;+
		   /bin/rm -f common_save_par.par


	    else

              dcl if f$search ("phottest_general.par;*") .nes. "" 	+
			then delete phottest_general.par;* 
              dcl if f$search ("pho_temp_par.par;*") .nes. "" 		+
			then delete pho_temp_par.par;* 

	      dcl if f$search ("pho_lambert.par;*") .nes. "" 		+
		 	then delete pho_lambert.par;*
	      dcl if f$search ("pho_minnaert.par;*") .nes. "" 		+
		 	then delete pho_minnaert.par;*
	      dcl if f$search ("pho_irvine.par;*") .nes. "" 		+
		 	then delete pho_irvine.par;*
	      dcl if f$search ("pho_veverka.par;*") .nes. "" 		+
		 	then delete pho_veverka.par;*
	      dcl if f$search ("pho_buratti1.par;*") .nes. "" 		+
		 	then delete pho_buratti1.par;*
	      dcl if f$search ("pho_buratti2.par;*") .nes. "" 		+
		 	then delete pho_buratti2.par;*
	      dcl if f$search ("pho_buratti3.par;*") .nes. "" 		+
		 	then delete pho_buratti3.par;*
	      dcl if f$search ("pho_mosher.par;*") .nes. "" 		+
		 	then delete pho_mosher.par;*
	      dcl if f$search ("pho_lumme_bowel_hg1.par;*") .nes. "" 	+
		 	then delete pho_lumme_bowel_hg1.par;*
	      dcl if f$search ("pho_hapke_81_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_le2.par;*
	      dcl if f$search ("pho_hapke_81_cook.par;*") .nes. "" 	+
		 	then delete pho_hapke_81_cook.par;*
	      dcl if f$search ("pho_hapke_86_hg1.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg1.par;*
	      dcl if f$search ("pho_hapke_86_hg2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_hg2.par;*
	      dcl if f$search ("pho_hapke_86_le2.par;*") .nes. "" 	+
		 	then delete pho_hapke_86_le2.par;*
	      dcl if f$search ("pho_hapke_hg1_dom.par;*") .nes. "" 	+
		 	then delete pho_hapke_hg1_dom.par;*
	      dcl if f$search ("pho_regner_hapke_hg1.par;*") .nes. "" 	+
		 	then delete pho_regner_hapke_hg1.par;*
	      dcl if f$search ("pho_atmo_corr_regner.par;*") .nes. "" 	+
		 	then delete pho_atmo_corr_regner.par;*

              dcl if f$search ("common_proc_name.par;*") .nes. "" 	+
		 	then delete common_proc_name.par;*
	      dcl if f$search ("common_save_par.par;*") .nes. "" 	+
		 	then delete common_save_par.par;*


	    end-if
	end-if

	! delete all photometrical globals:

!	delete-global pho_global 

  end-proc

.title
VICAR program PHOTTEST generates synthetic photometric data and 
stores them in an IBIS2 photometric catalog (phocat).

.help
PURPOSE:

PHOTTEST  is a VICAR program which generates synthetic photometric data and 
stores them in an IBIS2 photometric catalog (phocat), for use in testing 
photometric functions in program PHOTFIT2.
	

FUNCTION:

PHOTTEST computes reflectances for a set of points and writes them into the 
catalog (phocat).  The points can either be specified directly by parameters 
INC_ANG, EMI_ANG, and PHAS_ANG, or can be computed in a grid using parameters 
START and DELTA. Shifts can be added to the data with gaussian distribution to 
simulate observational errors which will be an absolute error. 
The parameters used to generate the data are written into the property label.


PHOCAT FILE:

The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. The program PHOTTEST used only one IMAGE_* group. but tiepoint 
files using some IMAGE_* groups containing informations relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g.  CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 

abstract groups	      primitive groups    units	      formats	used in PHOTTEST

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used




EXECUTION:



In the SHELL-VICAR :

	 PHOTTEST 'OUT=PHOTCAT.DAT PARAMS'
  (no default values for the photometric parameters!)



In the command modus :

	TAE> PHOTTEST OUT=PHOTCAT.DAT PARAMS
   (no default values for the photometric parameters!)
 or
	TAE> PHOTTESTM OUT=PHOCAT.DAT PARAMS  
  (it helds for every photometric function its own parameter defaults)




In the tutor modus  --> menu-driven parameter input :

	TAE> tutor PHOTTESTM   
  (it helds for every photometric function its own parameter defaults)


tutor PHOTTEST

There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  In this program, the menu points have the following meanings:

1. Select the first menu point to input the general parameters 
   such as the names of output catalog, the illumination conditions, and 
   so on.
2. Containing the parameters of the selected photometric function.
   The photometric function pertaining to this menu point and the name of this 
   menu point are changing depending on your input of the parameter PHO_FUNC
   in the first menu point.
3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'the user-specified name or  
					phottestm.par' 

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.



HELPS :

- You will get the common help contained in the ".mdf" file (phottestm.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (phottest.pdf   
   or phottestm.pdf) by processing of "help-help" applied to the program 
  (should be verry similary to the help of phottestm.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.




REQUIREMENTS and DEPENDENCIES:

LIBRARIES TO RUN PROGRAM:	LIB_TAE, LIB_RTL, LIB_P2SIB, LIB_P1SIB

INCLUDE-FILES:			vicmain_c, math.h, defines.h, 
				ibisfile.h, ibiserrs.h, 
				pho.h, pho_global.pdf

SUBROUTINES:			pho_routines.com
				PHOPDF.COM

	
GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

Name		Type			Description

PHO_FUNC_type 	string			It containes the names of the 
					valid photometric functions (to 
					pass into the menu).

pho_PROC_NAME 	string			Name of the main program 



INTERFACES:


INPUT:	


VICAR PDF-PARAMETERS:

Name	Type	count	Default	Description


1.) phottestm_general.pdf 	(menu point (1) of PHOTTEST)

OUT	char*32	 1	--	File name for the photometric catalog. 
							
PHO_FUNC char*32 1  HAPKE_86_LE2 Photometric function.

CLASS_ID int	1	1	class_id 

INC_ANG	real 	0:100	--	Incidence angles in degree.

EM_ANG	real 	0:100	--	Emission angles in degree.

PHAS_ANG real 	0:100	--	Phase angles in degree.

START		real 0:3	Starting point for grid in degrees.
				(INC_ANG,EM_ANG,PHAS_ANG)

DELTA		real 0:3	Increment for grid in degrees.
				(INC_ANG,EM_ANG,PHAS_ANG)

SIGMA		real		Simulated standard deviation.

SEED		int		Arbitrary seed for SIGMA.

PRINT		keyword		Screen output  of data generated.



2.) pho_"&PHO_FUNC".pdf	(menupoint (2) of PHOTTESTM) 
(photometry menu; the subPDFs pertaining to photometry are deliverd to the 
VICAR system by phopdf.com)

actuell  second menu	Name		default		Comment
(photom.function)	(funct.params)

LAMBERT			ALBEDO		1.0

MINNAERT		ALBEDO		1.0
			EXPONENT	0.6

IRVINE			EXPONENT	0.9
			IRV_EXP1	0.118
			IRV_EXP2	0.0039

VEVERKA			A_VEVERKA	0.997
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14

BURATTI1		ALBEDO		0.5
			B_VEVERKA	0.6
			E_BURATTI	0.14

BURATTI2		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
 			E_BURATTI	0.14

BURATTI3		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			E_BURATTI	0.14

MOSHER			A_VEVERKA	0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			MO_EXP1		0.5
			MO_EXP2		0.1

LUMME_BOWEL_HG1	W_SOIL	0.3
			H_SHOE		0.06
			DEN_SOIL	0.8
			THETA		20
			HG1_SOIL	-0.26

HAPKE_81_LE2		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_81_COOK		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3
			COOK		0.9

HAPKE_86_HG1		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		15.0
			HG1_SOIL	-0.26

HAPKE_86_HG2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.29
			HG2_SOIL	0.39
			HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.012
			THETA		20.0
			LE1_SOIL	0.29
			LE2_SOIL	0.39

HAPKE_HG1_DOM		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		1.0
			THETA		20.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			B_CBOE		1.0

REGNER_HAPKE_HG1	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35

ATMO_CORR_REGNER	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35





3.) common_save_par.pdf		(menupoint (3) of PHOTTESTM)
(see common_subpdf.com)

SAVE_PAR	string		Name for the TEA-parameter file


4.) common_proc_done.pdf	(menupoint (4) of PHOTTESTM)
(run the main program PHOTTEST; see common_subpdf.com)


	
OUTPUT:

FILES:				IBIS2 photometric catalog file 
				(see parameter OUT)

SCREEN OUTPUT:			Class identification, photometric function and 
				its parameters, # points, 
				mean error and  standard deviation depending of 
				parameter SIGMA,
				generated data (optional accordingly parameter 
				PRINT)



BACKGROUND AND REFERENCES :	see pho_routines.com and phopdf.com



SOFTWARE PLATFORM :		VICAR, TAE 5.2
				


HARDWARE PLATFORM :		VMS/UNIX(AXP/SOLARIS,SGI)
				No particular hardware required;
				tested on AXP/SOLARIS/SGI


PROGRAMMING LANGUAGE :		TCL , C	


HISTORY:			20-1-87 L.W.Kamp: 	original 
				Nov. '95 F.Oschuetz:	new written to be 
							portable to run on 
							AXP/SOLARIS/SGI 
							based on
							pho_routines.com and
							phopdf.com 


COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)



.LEVEL1

.VARI OUT
Photometric catalog

.VARI PHO_FUNC
Photometric function type

.VARI CLASS_ID
class_id

.VARI INC_ANG
Incidence angles

.VARI EM_ANG
Emission angles

.VARI PHAS_ANG
Phase angles

.VARI START
Starting point for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI DELTA
Increment for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI SIGMA
Standard deviation

.VARI SEED
Arbitrary seed for SIGMA

.VARI PRINT
Screen output of data generated.

.VARI ALBEDO
albedo

VARI EXPONENT
Minnaert's konstant

.VARI A_VEVERKA 
Veverka parameter

.VARI B_VEVERKA
Veverka parameter

.VARI C_VEVERKA
Veverka parameter

.VARI D_VEVERKA
Veverka parameter

.VARI MO_EXP2
Mosher's exponent

.VARI MO_EXP1
Mosher's exponent

.VARI E_BURATTI
Buratti's parameter

.VARI DEN_SOIL
Hapke parameter

.VARI W_SOIL
Hapke parameter

.VARI HG1_SOIL
Hapke Parameter

.VARI HG2_SOIL
Hapke parameter

.VARI HG_ASY_SOIL
Hapke parameter

.VARI LE1_SOIL
Hapke parameter

.VARI LE2_SOIL
Hapke parameter

.VARI H_SHOE
Hapke parameter

.VARI B_SHOE
Hapke parameter

.VARI H_CBOE
Hapke-Dominique parameter

.VARI B_CBOE
Hapke-Dominique parameter

.VARI THETA
Hapke parameter

.VARI COOK
Hapke-Cook parameter

.VARI TAU_ATM
Regner parameter

.VARI W_ATM
Regner parameter

.VARI HG1_ATM
Regner parameter

.VARI IRV_EXP1
Irvine parameter

.VARI IRV_EXP2
Irvine parameter

.VARI SAVE_PAR
file name for par-file


.LEVEL2

.VARI OUT
Filename of the photometric catalog file (IBIS2) of type "phocat".
The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. 
The program PHOTTEST used only one IMAGE_* group IMAGE_1 containing 
informations relates to the image. GENERAL_QLF containes informations relates 
to the object point (e.g. CLASS_IDentifier). 
There are 19 columns in this file. All are empty exept folowing columns:
	IMAGE_1 & IncidenceAngle
	IMAGE_1 & EmissionAngle	 
	IMAGE_1 & PhaseAngle
	IMAGE_1 & I/F 		= reflectance values as computed from the 
				  photometric function.
	IMAGE_1 & StandDev
	GENERAL_QLF & CLASS_ID  = Class identification


.VARI PHO_FUNC
Photometric function :

	valid values :	LAMBERT, 
			MINNAERT, 
			IRVINE, 
			VEVERKA, 
			BURATTI1, 
			BURATTI2, 
			BURATTI3, 
			MOSHER, 
			LUMME_BOWEL_HG1, 
			HAPKE_81_LE2, 
			HAPKE_81_COOK, 
			HAPKE_86_HG1, 
			HAPKE_86_HG2, 
			HAPKE_86_LE2, 
			HAPKE_HG1_DOM, 
			REGNER_HAPKE_HG1, 
			ATMO_CORR_REGNER
.page
NOTE: When returning to the highest level of the menu (i.e. the MDF-file) you 
will see that the second selection point has been changed according to your 
input of PHO_FUNC.
For more see pho_routines.com and PHOPDF.COM 

.VARI CLASS_ID
The class_id nummerates the photometric functions. For using different fotometric functions or parameter sets.


.VARI INC_ANG
Incidence angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI EM_ANG
Emission angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI PHAS_ANG
Phase angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI START
Starting point for grid in degrees.
This parameter is only used if INC_ANG, IMI_ANG, and PHAS_ANG are not 
specified. It is used, togeter with parameter DELTA, to compute a grid of 
angles. This parameter specifies the starting values for the grid of: (Incidence, Emission, Azimuth) in that order. The Phase angle is computed for 
each point from the three given angles. 
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI DELTA
Increment for grid in degrees.
This parameter is only used if INC_ANG, EMIS_ANG, and PHAS_ANG are not 
specified. It is used, together with parameter START, to compute a grid of 
angles. This parameter specifies the increment values for the grid of: 
(Incidence, Emission, Azimuth) in that order.
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI SIGMA
Simulated standard deviation.
If this quantity is non_zero, then the program will simulate experimental error 
by adding an absolute "error" to each computed reflectance. These errors will 
be random with a gaussian probability distribution with standard deviation = 
SIGMA. Since reflectances cannot exceeded 1.0, this is also assumed to be a 
maximum on SIGMA. To check that the simulated errors are reasonable, the 
program prints out the computed mean error and standard deviation for the 
errors added. The "STD.DEV" should be about equal to SIGMA, and the "MN.ERR" 
should be close to 0 (much smaller than SIGMA).

.VARI SEED
Arbitrary seed for SIGMA.
This is an arbitrary number used to start the random number generator for 
SIGMA. It is only used if SIGMA is not zero. The default can be used unless the 
user wants to ensure that the random numbers are different from a previous 
run. The value of this parameter should be a large, odd, negative integer.

.VARI PRINT
Screen output of the data generated.
If this keyword is set to 'PRINT', a table will be printed to the screen. 

.VARI ALBEDO
Albedo -  valid for the Lambert and Minnaert photometric functions.

.VARI EXPONENT
Exponent - the geometrical constant k of the Minnaert photometric function.

.VARI A_VEVERKA 
Parameter of the Veverka, Squyres-Veverka and Mosher photometric functions.

.VARI B_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI C_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI D_VEVERKA
Parameter of the Veverka, Mosher, Squyres-Veverka and Buratti 
photometric functions.

.VARI E_BURATTI
Buratti's parameter for modification of the Veverka photometric function.

.VARI MO_EXP1
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP2).

.VARI MO_EXP2
Modification of the coefficient k in the Minnaert part 
of Mosher's photometric function (goes along with MO_EXP1).

.VARI DEN_SOIL
Specific volume density of the soil.

.VARI W_SOIL
Single-scattering albedo of the soil particles. It characterizes the 
efficiency of an average particle to scatter and absorb light. 
One of the classical Hapke parameter.

.VARI HG1_SOIL
Parameter of the first term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG2_SOIL
Parameter of the second term of the Henyey-Greenstein soil particle 
phase function.

.VARI HG_ASY_SOIL
Asymmetry parameter (weight of the two terms 
in the Henyey-Greenstein soil phase function).

.VARI LE1_SOIL
Parameter of the first term of the Legendre-Polynomial soil particle 
phase function.

.VARI LE2_SOIL
Parameter of the second term of the Legendre-Polynomial soil particle 
phase function.

.VARI H_SHOE
One of the classical Hapke parameter.
Parameter which characterizes the soil structure in the terms of porosity, 
particle-size distribution, and rate of compaction with depth (angular width 
of opposition surge due to shadowing). 

.VARI B_SHOE
One of the classical Hapke parameter. 
Opposition magnitude coefficient. The total amplitude of the opposition surge 
due to shadowing. It is the ratio of the light scattered from near the 
illuminated surface of the particle to the total amount of light scattered at 
zero phase : 
B_SHOE=S(0)/(W_SOIL*p(0))
with p(0) - soil phase function
S(0) - opposition surge amplitude term which characterizes the contribution of 
light scattered from near the front surface of individual particles at zero 
phase.
.page
For a true, shadow-hiding opposition effect, 0<=B_SHOE<=1.
However, there are several other phenomena that may also cause a surge in 
brightness at small phase angles. These including the following:
1) The coherent backscatter or weak photon localisation due to multiply 
   scattered light.
2) An single-particle opposition effect caused by complex porous agglomerates 
   ( soil phase function )
3) Glory caused by sperical particles ( soil phase function )
4) Internal reflections of transparent particles ( soil phase function )
   These various phenomena may be large enough to increase the opposition surge 
   by more than a factor of 2. This possibility may be taken into account by 
   allowing B_SHOE to be greater than 1.
 
.VARI H_CBOE
Parameter of the coherent backscattering ( angular width of the opposition 
surge due to multiply scattered light).
H_CBOE=lambda/(2*pi*L)
lambda - wavelength
L - the free path of the phonon in the medium

.VARI B_CBOE
Opposition magnitude coefficient of the coherent backscattering 
(height of opposition surge due to multiply scattered light). 

.VARI THETA
Average topographic slope angle of surface roughness at subresolution scale.
One of the classical Hapke parameter. 

.VARI COOK
 Parameter of the Cook's modification of the old Hapke function.

.VARI TAU_ATM
Optical depth of the atmosphere.

.VARI W_ATM
Single scattering albedo of the atmospheric aerosols.

.VARI HG1_ATM
Parameter of the first term of the Henyey-Greenstein atmospheric phase function.

.VARI IRV_EXP1
Parameter of the Irvine photometric function.

.VARI IRV_EXP2
Parameter of the Irvine photometric function.

.VARI SAVE_PAR
This is the name for the TAE-parameter file containing all parameters 
needed to running the program. The default name is PHOTTEST.PAR.
A user-specified name can be given to that file. This is similar to the
SAVE command in the Tutor Mode.

.end
$!-----------------------------------------------------------------------------
$ create phottestm.mdf
.TITLE
VICAR program  '&"pho_PROC_NAME"'

.proc &"pho_PROC_NAME"_general
Enter general input parameters and select function of the second menu-point
(type RUN when done)

.proc pho_&PHO_FUNC_type
Enter parameters for the "&PHO_FUNC_type" photometric function 
(type RUN when done)

.proc common_save_par
Enter the name for the par-file where you want to save your parameters
(type RUN when done)

.proc common_proc_done
Run main photometry application program '&"pho_PROC_NAME"'



.help
Name of Program:	PHOTTESTM
	

PURPOSE:

PHOTTEST  is a VICAR program which generates synthetic photometric data and 
stores them in an IBIS2 photometric catalog (phocat), for use in testing 
photometric functions in program PHOTFIT2.
	

FUNCTION:

PHOTTEST computes reflectances for a set of points and writes them into the 
catalog (phocat).  The points can either be specified directly by parameters 
INC_ANG, EMI_ANG, and PHAS_ANG, or can be computed in a grid using parameters 
START and DELTA. Shifts can be added to the data with gaussian distribution to 
simulate observational errors which will be an absolute error. 
The parameters used to generate the data are written into the property label.


PHOCAT FILE:

The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. The program PHOTTEST used only one IMAGE_* group. but tiepoint 
files using some IMAGE_* groups containing informations relates to the image.
GENERAL_QLF containes informations relates to the object point (e.g.  CLASS_IDentifier). OBJECT_COORDINATES containes only coordinates of the object 
point (e.g. LATitude, LONGitude or the X,Y,Z-coordinates in planetocentric 
coordinate system).

The structure of the photometric catalog file is given by: 

abstract groups	      primitive groups    units	      formats	used in PHOTTEST

IMAGE_1 		line 		  pixels	REAL	 used
			samp		  pixels	REAL	 used
			ObjectLine	  pixels	REAL	  --
			ObjectSamp	  pixels	REAL	  --
			BoxLines	  pixels	REAL	  --
			LuminanceLat	  degrees	DOUB	  --
			LuminanceLong	  degrees	DOUB	  --
			IncidenceAngle	  degrees	DOUB	 used
			EmissionAngle	  degrees	DOUB	 used
			PhaseAngle	  degrees	DOUB	 used
			DN_BoxMean	  DN		DOUB	  --
			Radiance	W/cm**2/str/nm	DOUB	  --
			I/F		  --		DOUB	 used
			StandDev	  --		DOUB	 used

OBJECT_COORDINATES  	LAT		  degrees	REAL	  --
			LONG		  degrees	REAL	  --

GENERAL_QLF		--		  --		DOUB	  --
			CLASS_ID	  --		FULL	 used



EXECUTION:



In the SHELL-VICAR :

	 PHOTTEST 'OUT=PHOTCAT.DAT PARAMS'
  (no default values for the photometric parameters!)



In the command modus :

	TAE> PHOTTEST OUT=PHOTCAT.DAT PARAMS
   (no default values for the photometric parameters!)
 or
	TAE> PHOTTESTM OUT=PHOCAT.DAT PARAMS  
  (it helds for every photometric function its own parameter defaults)



In the tutor modus  --> menu-driven parameter input :

	TAE> tutor PHOTTESTM   
  (it helds for every photometric function its own parameter defaults)


tutor PHOTTESTM

There are separate PDFs for each selection point seen in the main menu.  On 
selection of a particular menu point you will enter the normal tutor mode of 
this PDF.  In this program, the menu points have the following meanings:

1. Select the first menu point to input the general parameters 
   such as the names of output catalog, the illumination conditions, and 
   so on.
2. Containing the parameters of the selected photometric function.
   The photometric function pertaining to this menu point and the name of this 
   menu point are changing depending on your input of the parameter PHO_FUNC
   in the first menu point.
3. Select this menu point to specify the name of the parameter file which is 
   generated by the program (the default name in VICAR programs: LAST.PAR).
   This is useful because in a Menu there is no 'save'-command to save a 
   parameter file with a user-specified name (e.g. "save proc_name.par").

   EXECUTION :

   USER ACTION				RESULT

   don't call this menu point		last.par

   exit this menu point with 'exit'	last.par

   exit this menu point with 'run'	the user-specified name or  
					phottestm.par' 

4. This menu point is to be entered to execute the main program.

You can repeat all steps and reenter all menu items except the step that leads 
to the execution of the program.

If you request help for the selection points in the Menu, you will get the help 
text contained in the respective sub PDFs.



HELPS :

- You will get the common help contained in the ".mdf" file (phottestm.mdf) by 
  typing "help *" in the menu,
- but you will get the help text contained in programs main-PDF (phottest.pdf   
   or phottestm.pdf) by processing of "help-help" applied to the program 
  (should be verry similary to the help of phottestm.mdf).
- If you request help for the selection points in the Menu, you will get the 
  help text contained in the respective sub PDFs.




REQUIREMENTS and DEPENDENCIES:

LIBRARIES TO RUN PROGRAM:	LIB_TAE, LIB_RTL, LIB_P2SIB, LIB_P1SIB

INCLUDE-FILES:			vicmain_c, math.h, defines.h, 
				ibisfile.h, ibiserrs.h, 
				pho.h, pho_global.pdf

SUBROUTINES:			pho_routines.com
				PHOPDF.COM

	
GLOBAL VARIABLE:
The following global variables defined by the pho_global.pdf must be referenced:

Name		Type			Description

PHO_FUNC_type 	string			It containes the names of the 
					valid photometric functions (to 
					pass into the menu).

pho_PROC_NAME 	string			Name of the main program 



INTERFACES:

INPUT:	

VICAR PDF-PARAMETERS:

Name	Type	count	Default	Description

1.) phottestm_general.pdf 	(menu point (1) of PHOTTESTM)

OUT	char*32	1	--	Filename of the photometric catalog file (IBIS2)
				of type "phocat".
				The structure of the phocat file is desined in 
				such a way that tiepoint files can be extended 
				and containing all collumns of the old IBIS1 
				photometric catalog files. The program PHOTTEST 
				used only one IMAGE_* group IMAGE_1 containing 
				informations relates to the image. 
				GENERAL_QLF containes informations relates to 
				the object point (e.g.  CLASS_IDentifier). 
				There are 19 columns in this file. All are 
				empty exept folowing columns:
				IMAGE_1 & IncidenceAngle
				IMAGE_1 & EmissionAngle	 
				IMAGE_1 & PhaseAngle
				IMAGE_1 & I/F = reflectance values as 
					        computed from the photometric 
					        function.
				IMAGE_1 & StandDev
				GENERAL_QLF & CLASS_ID = Class identification

				
PHO_FUNC char*32 1  HAPKE_86_LE2 Photometric function.
				NOTE: When returning to the highest level of  
				the menu (i.e. the MDF-file) you will see that 
 				the second selection point has been changed 
				according to your input of PHO_FUNC.
				For more see pho_routines.com and PHOPDF.COM.
				valid values :	LAMBERT, 
						MINNAERT, 
						IRVINE, 
						VEVERKA, 
						BURATTI1, 
						BURATTI2, 
						BURATTI3, 
						MOSHER, 
						LUMME_BOWEL_HG1, 
						HAPKE_81_LE2, 
						HAPKE_81_COOK, 
						HAPKE_86_HG1, 
						HAPKE_86_HG2, 
						HAPKE_86_LE2, 
						HAPKE_HG1_DOM, 
						REGNER_HAPKE_HG1, 
						ATMO_CORR_REGNER

CLASS_ID int	1	1	class identification 

INC_ANG	real 	0:100	--	Incidence angles in degree.
				This parameter specifies the incidence angles 
				for up to 100 points. If any of INC_ANG, 
				EMIS_ANG, and PHAS_ANG are specified, then all 
				three must be specified, and all with the same 
				number of values.

EM_ANG	real 	0:100	--	Emission angles in degree.
				This parameter specifies the emition angles 
				for up to 100 points. If any of INC_ANG, 
				EMIS_ANG, and PHAS_ANG are specified, then all 
				three must be specified, and all with the same 
				number of values.

PHAS_ANG real 	0:100	--	Phase angles in degree. You have to be carefully
				for the maximal angle depending of the value of
				related INC_ANG and EM_ANG.
				This parameter specifies the phase angles 
				for up to 100 points. If any of INC_ANG, 
				EMIS_ANG, and PHAS_ANG are specified, then all 
				three must be specified, and all with the same 
				number of values.

START		real 0:3	Starting point for grid in degrees.
				This parameter is only used if INC_ANG, 
				IMI_ANG, and PHAS_ANG are not specified. It is 
				used, togeter with parameter DELTA, to compute 
				a grid of angles. This parameters specify the 
				starting values for the grid of: (Incidence, 
				Emission, Azimuth) in that order. The Phase 
				angle is computed for each point from the three 
				given angles. The grid range for these angles 
				is: 
				 Incidence & Emission angles : 0 -  90 degrees. 
				 Azimuth: 		     : 0 - 180 degrees.

DELTA		real 0:3	Increment for grid in degrees.
				This parameter is only used if INC_ANG, 
				EMIS_ANG, and PHAS_ANG are not specified. It is 
				used, together with parameter START, to compute 
				a grid of angles. This parameter specifies the 
				increment values for the grid of: 
				(Incidence, Emission, Azimuth) in that order.
				The grid range for these angles is: 
				 Incidence & Emission angles : 0 -  90 degrees. 
				 Azimuth: 		     : 0 - 180 degrees.

SIGMA		real		Simulated standard deviation.
				If this quantity is non_zero, then the program 
				will simulate experimental error by adding an 
				"error" to each computed reflectance. These 
				errors will be random with a gaussian 
				probability distribution with standard 
				deviation = SIGMA. Since reflectances cannot 
				exceeded 1.0, this is also assumed to be a 
				maximum on SIGMA. To check that the simulated 
				errors are reasonable, the program prints out 
				the computed mean error and standard deviation 
				for the errors added. The "STD.DEV" should be 
				about equal to SIGMA, and the "MN.ERR" should 
				be close to 0 (much smaller than SIGMA).

SEED		int		Arbitrary seed for SIGMA.
				This is an arbitrary number used to start the 
				random number generator for SIGMA. It is only 
				used if SIGMA is not zero. The default can be 
				used unless the user wants to ensure that the 
				random numbers are different from a previous 
				run. The value of this parameter should be a 
				large, odd, negative integer.

PRINT		keyword		Screen output of the data generated.
				If this keyword is set to 'PRINT', a table will 
				be printed to the screen. 




2.) pho_"&PHO_FUNC".pdf	(menupoint (2) of PHOTTESTM) 
(photometry menu; the subPDFs pertaining to photometry are deliverd to the 
VICAR system by phopdf.com)

actuell  second menu	Name		default		Comment
(photom.function)	(funct.params)

LAMBERT			ALBEDO		1.0

MINNAERT		ALBEDO		1.0
			EXPONENT	0.6

IRVINE			EXPONENT	0.9
			IRV_EXP1	0.118
			IRV_EXP2	0.0039

VEVERKA			A_VEVERKA	0.997
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14

BURATTI1		ALBEDO		0.5
			B_VEVERKA	0.6
			E_BURATTI	0.14

BURATTI2		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
 			E_BURATTI	0.14

BURATTI3		ALBEDO		0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			E_BURATTI	0.14

MOSHER			A_VEVERKA	0.5
			B_VEVERKA	0.6
			C_VEVERKA	0.003
			D_VEVERKA	0.14
			MO_EXP1		0.5
			MO_EXP2		0.1

LUMME_BOWEL_HG1	W_SOIL	0.3
			H_SHOE		0.06
			DEN_SOIL	0.8
			THETA		20
			HG1_SOIL	-0.26

HAPKE_81_LE2		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3

HAPKE_81_COOK		W_SOIL		0.3
			H_SHOE		0.06
			LE1_SOIL	0.3
			LE2_SOIL	0.3
			COOK		0.9

HAPKE_86_HG1		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		15.0
			HG1_SOIL	-0.26

HAPKE_86_HG2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.29
			HG2_SOIL	0.39
			HG_ASY_SOIL	1.0

HAPKE_86_LE2		W_SOIL		0.21
			H_SHOE		0.07
			B_SHOE		2.012
			THETA		20.0
			LE1_SOIL	0.29
			LE2_SOIL	0.39

HAPKE_HG1_DOM		W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		1.0
			THETA		20.0
			HG1_SOIL	-0.26
 			H_CBOE		0.06
			B_CBOE		1.0

REGNER_HAPKE_HG1	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35

ATMO_CORR_REGNER	W_SOIL		0.3
			H_SHOE		0.06
			B_SHOE		2.0
			THETA		20.0
			HG1_SOIL	-0.26
			W_ATM		0.78
			TAU_ATM		0.05
			HG1_ATM		0.35




3.) common_save_par.pdf		(menupoint (3) of PHOTTESTM)
(see common_subpdf.com)

SAVE_PAR	string		This is the name for the TEA-parameter file
				containing all parameters needed to running 
				the program. 
				The default name is LAST.PAR or PHOTTESTM.PAR 
				depending of calling the third menupoind or do 
				not calling it. 
				This is similar to the SAVE command in the Tutor
				Mode.
				A user-specified name can be given to that file. 


4.) common_proc_done.pdf	(menupoint (4) of PHOTTESTM)
(run the main program PHOTTEST; see common_subpdf.com)





OUTPUT:

FILES:				IBIS2 photometric catalog file 
				(see parameter OUT and common program help)

SCREEN OUTPUT:			Class identification, photometric function and 
				its parameters, # points, 
				mean error and  standard deviation depending of 
				parameter SIGMA,
				generated data (optional accordingly parameter 
				PRINT)


BACKGROUND AND REFERENCES :	see pho_routines.com and phopdf.com



SOFTWARE PLATFORM :		VICAR, TAE 5.2
				


HARDWARE PLATFORM :		VMS/UNIX(AXP/SOLARIS,SGI)
				No particular hardware required;
				tested on AXP/SOLARIS/SGI


PROGRAMMING LANGUAGE :		TCL , C	


HISTORY:			20-1-87 L.W.Kamp: 	original 
				Nov '95 F.Oschuetz:	new written to be 
							portable to run on 
							AXP/SOLARIS/SGI 
							based on
							pho_routines.com and
							phopdf.com 


COGNIZANT PROGRAMMER:		Friedel Oschuetz
				Institute of Planetary Exploration
				DLR
				12484 Berlin (FRG)

									
.end
$!-----------------------------------------------------------------------------
$ create phottestm_general.pdf
procedure option=selftutor help=*

!-----------------------------------------------------------------------------
! PHOTTESTM_GENERAL.PDF
!
! This is the PDF for the first menu point of the MDF file.
! In this PDF file general parameters are defined like the names of 
! input/output files.
!
!-----------------------------------------------------------------------------

	!**********************************************************************
	! The global variables PHO_FUNC_type (and $MENUS) will be used in this
	! procedure, so they have to be declared here.
	! The global PHO_FUNC_type (the desired photometric function) will be
	! used to change the third menu point of the MDF file (see above)
	! according to the input to PHO_FUNC in this PDF.
	! The global $MENUS keeps the active stack of Menu Definition File
	! names. $MENUS(1) is the root menu name, $MENUS(2) the menu selected
	! from the root menu, and so on. The current menu stack can be
	! displayed with "DISPLAY $MENUS". Since there is only one MDF in
	! this demo, it doesn't make too much sense to have that global
	! included here. It is just referenced for completeness.
	!**********************************************************************

	refgbl PHO_FUNC_type 
	refgbl $menus

	parm MAIN_PROC_NAME string

	procedure name=phottestm_general_sub help=*

		parm OUT	type=(string,32) count=0:1 default=--

		! photometric functions :

		parm PHO_FUNC type=(string,32) count=1 	+
			valid = (			+
				LAMBERT,		+
				MINNAERT,		+
				IRVINE,			+
				VEVERKA,		+
				BURATTI1,		+
				BURATTI2,		+
				BURATTI3,		+
				MOSHER,			+
				LUMME_BOWEL_HG1,	+
				HAPKE_81_LE2,		+
				HAPKE_81_COOK,		+
				HAPKE_86_HG1,		+
				HAPKE_86_HG2,		+
				HAPKE_86_LE2,		+
				HAPKE_HG1_DOM,		+
				REGNER_HAPKE_HG1, 	+
				ATMO_CORR_REGNER	+
				) 	default="&PHO_FUNC_type"

		parm CLASS_ID	int  count=1		default=1

		! illumination conditions :

    		parm INC_ANG	real count=0:100 	default=--	+
				valid=(0.0:90)
    		parm EM_ANG 	real count=0:100 	default=--	+
				valid=(0.0:90)
    		parm PHAS_ANG 	real count=0:100 	default=--	+
				valid=(0.0:180)
		parm START	real count=0:3		default=(10,10,20)
		parm DELTA	real count=0:3		default=(30,30,70)

		! observation errors :

		parm SIGMA	real count=1		default=0.0
		parm SEED	int  count=1		default=-9999999

		parm PRINT 	keyword count=0:1  	default=--         +
				valid=PRINT



	body

	!*******************************************************************
	! "pho_global.pdf" is being executed in the following line. 
	! In this PDF, two globals are defined (PHO_FUNC_type, pho_PROC_NAME)
	!*******************************************************************
 
	pho_global PHO_FUNC_type="&PHO_FUNC"

	end-proc
body

	if (_tutor=1)

	   restore-parm common_proc_name.par

 	   tutor phottestm_general_sub 				+
			|restore=&"MAIN_PROC_NAME"_general.par 	+
		 	 save=&"MAIN_PROC_NAME"_general.par|
	else
	   write " ************************************************"
	   write " "
	   write " This program works only when run from tutor mode"
           write " of other programs."
	   write " "
	   write " ************************************************"
	end-if

end-proc

.TITLE
VICAR sub-menu &"MAIN_PROC_NAME"_GENERAL

.HELP
PURPOSE:
This menu point is dedicated to input general parameters for the program 
PHOTTEST such as the names of output catalog, the illumination conditions, 
and so on.

NOTE : When returning to the highest level of the menu 
(i.e. the PHOTTESTM.MDF-file) you will see that the second selection 
point has been changed according to your input of PHO_FUNC in this tutor.


.PAGE
Programmer:

Friedel Oschuetz
Institute of Planetary Exploration
DLR
12484 Berlin (FRG)


.LEVEL1

.VARI OUT
Photometric catalog

.VARI PHO_FUNC
Photometric function type

.VARI CLASS_ID
class_id

.VARI INC_ANG
Incidence angles

.VARI EM_ANG
Emission angles

.VARI PHAS_ANG
Phase angles

.VARI START
Starting point for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI DELTA
Increment for grid
(INC_ANG,EM_ANG,AZIM_ANG)

.VARI SIGMA
Standard deviation

.VARI SEED
Arbitrary seed for SIGMA

.VARI PRINT
Screen output of data generated.


.LEVEL2

.VARI OUT
Filename of the photometric catalog file (IBIS2) of type "phocat".
The structure of the phocat file is desined in such a way that tiepoint files 
can be extended and containing all collumns of the old IBIS1 photometric 
catalog files. 
The program PHOTTEST used only one IMAGE_* group IMAGE_1 containing 
informations relates to the image. GENERAL_QLF containes informations relates 
to the object point (e.g. CLASS_IDentifier). 
There are 19 columns in this file. All are empty exept folowing columns:
	IMAGE_1 & IncidenceAngle
	IMAGE_1 & EmissionAngle	 
	IMAGE_1 & PhaseAngle
	IMAGE_1 & I/F 		= reflectance values as computed from the 
				  photometric function.
	IMAGE_1 & StandDev
	GENERAL_QLF & CLASS_ID  = Class identification


.VARI PHO_FUNC
Photometric function :

	valid values :	LAMBERT, 
			MINNAERT, 
			IRVINE, 
			VEVERKA, 
			BURATTI1, 
			BURATTI2, 
			BURATTI3, 
			MOSHER, 
			LUMME_BOWEL_HG1, 
			HAPKE_81_LE2, 
			HAPKE_81_COOK, 
			HAPKE_86_HG1, 
			HAPKE_86_HG2, 
			HAPKE_86_LE2, 
			HAPKE_HG1_DOM, 
			REGNER_HAPKE_HG1, 
			ATMO_CORR_REGNER
.page
NOTE: When returning to the highest level of the menu (i.e. the MDF-file) you 
will see that the second selection point has been changed according to your 
input of PHO_FUNC.
For more see pho_routines.com and PHOPDF.COM 

.VARI CLASS_ID
The class_id numerates the photometric functions. For using different fotometric functions or parameter sets.

.VARI INC_ANG
Incidence angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI EM_ANG
Emission angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI PHAS_ANG
Phase angle in degree.
This parameter specifies the incidence angles for up to 100 points. If any of 
INC_ANG, EMIS_ANG, and PHAS_ANG are specified, then all three must be 
specified, and all with the same number of values.

.VARI START
Starting point for grid in degrees.
This parameter is only used if INC_ANG, IMI_ANG, and PHAS_ANG are not 
specified. It is used, togeter with parameter DELTA, to compute a grid of 
angles. This parameter specifies the starting values for the grid of: (Incidence, Emission, Azimuth) in that order. The Phase angle is computed for 
each point from the three given angles. 
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI DELTA
Increment for grid in degrees.
This parameter is only used if INC_ANG, EMIS_ANG, and PHAS_ANG are not 
specified. It is used, together with parameter START, to compute a grid of 
angles. This parameter specifies the increment values for the grid of: 
(Incidence, Emission, Azimuth) in that order.
The grid range for these angles is: 
	Incidence & Emission angles  : 0 -  90 degrees. 
	Azimuth 		     : 0 - 180 degrees.

.VARI SIGMA
Simulated standard deviation.
If this quantity is non_zero, then the program will simulate experimental error 
by adding an absolute "error" to each computed reflectance. These errors will 
be random with a gaussian probability distribution with standard deviation = 
SIGMA. Since reflectances cannot exceeded 1.0, this is also assumed to be a 
maximum on SIGMA. To check that the simulated errors are reasonable, the 
program prints out the computed mean error and standard deviation for the 
errors added. The "STD.DEV" should be about equal to SIGMA, and the "MN.ERR" 
should be close to 0 (much smaller than SIGMA).

.VARI SEED
Arbitrary seed for SIGMA.
This is an arbitrary number used to start the random number generator for 
SIGMA. It is only used if SIGMA is not zero. The default can be used unless the 
user wants to ensure that the random numbers are different from a previous 
run. The value of this parameter should be a large, odd, negative integer.

.VARI PRINT
Screen output of the data generated.
If this keyword is set to 'PRINT', a table will be printed to the screen. 


.end
$ Return
$!#############################################################################
$Imake_File:
$ create phottest.imake
#define PROGRAM  phottest

#define MODULE_LIST phottest.c pho_label.c

#define MAIN_LANG_C

#define USES_ANSI_C

#define P2LIB

/********************************************
LOCAL LIBRARY and DEBUGGER for development 

#define TEST
#define LIB_LOCAL
#define DEBUG
#define LIB_P1SUB_DEBUG
#define LIB_P2SUB_DEBUG

*******************************************/

#define LIB_P2SUB
#define LIB_P1SUB
#define LIB_RTL
#define LIB_TAE

$ Return
$!#############################################################################
$Test_File:
$ create tstphottest.pdf
procedure
body

 phottest out=phottest_m.ibis2 start=(10,10,180) delta=(30,30,70) +
 'PRINT SIGMA=0.000001 +
 pho_func=MINNAERT ALBEDO=0.7 EXPONENT=0.6 

 ibis-list phottest_m.ibis2 csize=16 units=units groups=groups formats=formats 

 label-list phottest_m.ibis2

end-proc
$ Return
$!#############################################################################
