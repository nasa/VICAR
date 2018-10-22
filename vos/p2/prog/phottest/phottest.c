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
