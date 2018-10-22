#include <math.h>
#include <time.h> 
#include "vicmain_c"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "pho.h"
#include "zmabend.h"
#include <string.h>
#include <stdlib.h>

/* Program PHOTFIT2  */

#define D_EPS 1.0e-10
#define TEMP_RANGE 0.5
#define phoCAT_TYPE_1 "PHOCAT1"
#define phoCAT_TYPE "phocat"
#define phoCAT_NAME_LENGTH 6
#ifndef phoFILENAME_LENGTH
#define phoFILENAME_LENGTH 72
#endif
#ifndef phoINP_FILE_NUM
#define phoINP_FILE_NUM 20
#endif

#define phoCAT_NAME_LENGTH 6
#ifndef phoINVALID_CLASS_ID
#define phoINVALID_CLASS_ID -1013
#endif
#ifndef phoROW_OUT_OF_RANGE
#define phoROW_OUT_OF_RANGE -1012
#endif
#ifndef phoNO_CLASS_MATCH
#define phoNO_CLASS_MATCH -1014
#endif
#ifndef phoBAD_POINT
#define phoBAD_POINT -1015
#endif

/* from pho_private.h */
typedef struct  {
	char func_name[phoMAX_FUNC_NAME_LENGTH+1];
	double func_params[phoMAX_PARAM_PER_FUNC];
	char flag_set[phoMAX_PARAM_PER_FUNC];
	char flag_chg[phoMAX_PARAM_PER_FUNC];
	char flag_func_set[1];
	char flag_func_chg[1];
	char flag_mode[phoMax_MODE_NAME_LENGTH+1];
	}
PHO_STRUCT;
/* from pho_routines.c */
extern int phoInit(PHO *pho_obj);
extern int phoGetParms(PHO pho_obj);
extern int phoGetFunc(PHO_STRUCT *pho, char func_name[phoMAX_FUNC_NAME_LENGTH+1]);
extern int phoGetKeys(PHO_STRUCT *pho, char keywds[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1], int *num_kwd);
extern int phoGetVal(PHO_STRUCT *pho, char key[phoMAX_KEYWD_LENGTH+1], double *val);
extern int phoBidiRef(PHO_STRUCT *pho_obj, PHO_ILLUM *illum, double *phoFuncVal);
extern int phoFree(PHO pho_obj);
extern int phoSetVal(PHO_STRUCT *pho, char key[phoMAX_KEYWD_LENGTH+1], double val);

/* from rangen.c */
extern void zrangen(long*idum,float*rand_num);

int phocat1Open(int VicarUnit, int *IbisUnit, int *nrow, int *ncol);
int phocatRead(int IbisUnit, int *row, int *class_id, PHO_ILLUM *illum, double *phoFuncVal, double *eps);
int phocat1Close(int VicarUnit, int IbisUnit);
int phoCost(PHO pho_obj, double *x[2],PHO_ILLUM *illum_array, double limits[2][phoMAX_PARAM_PER_FUNC],
	    int npts, double tolerance, double minData,	double *error);
int phoMetropolis(PHO pho_obj,double limits[2][phoMAX_PARAM_PER_FUNC],double range[phoMAX_PARAM_PER_FUNC],
		  double *x[2],PHO_ILLUM *illum_array,int npts,double tolerance,double minData,int numIter,
		  int numTen,int numNorm,int numPrint,double *xcost);

void main44()
{
  int cnt, l, i, j, num, status, catReadStatus, count, npts, tnpts=0, ipts=0, n=0, least;
  int row, nrow, allrow=0, ncol, inpCount;
  int class_id, pdfClass_id, classCount;
  int rerun, maxIter, numTen, numNorm, numPrint;
  float temp;
  double dval, dval1, *phoFuncVal, eps;
  double phoParMin[phoMAX_PARAM_PER_FUNC], phoParMax[phoMAX_PARAM_PER_FUNC];
  double limits[2][phoMAX_PARAM_PER_FUNC];
  double phoParT[phoMAX_PARAM_PER_FUNC], percent, toleranc; 
  double  *x[2]; 
  double xcost;
  double mean[phoMAX_PARAM_PER_FUNC], std_dev[phoMAX_PARAM_PER_FUNC];
  double *res[phoMAX_PARAM_PER_FUNC];
  double reflErr, sumerr=0;
  char cval1[133], msg[133];
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1]; 
  char keyListMin[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+4]; 
  char keyListMax[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+4]; 
  char keyListT[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1+2];
  char fileName[phoINP_FILE_NUM][phoFILENAME_LENGTH+1];
  PHO pho_obj;
  PHO_ILLUM illum;
  int *VicarUnit, *IbisUnit;

  PHO_ILLUM *illum_array; 

  zvmessage(" program PHOTFIT2", "");
  zvmessage( " ", "");

  zveaction("sau","");

  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** main44: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("main44 abend");
  }

  status = phoInit( &pho_obj );
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoInit failed ***","");
	    		zmabend("photfit abend");
  }

/* get the photometric function and there first parameter guess from the PDF*/
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj );
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetParms failed ***","");
	    		zmabend("photfit abend");
  }

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  if(status != phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetFunc failed ***","");
	    		zmabend("photfit abend");
  }
  strcpy( msg, " Function =" );
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the number of parameters of the current photometric function : */

  status = phoGetKeys( pho_obj, 0, &num); 
  if(status!=phoSUCCESS)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  strcpy( msg, " parameter number = " );
  sprintf( cval1, " %d", num);
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

/* allocate memory for the parameters, limits,temperaturs */

  status = phoGetKeys( pho_obj, keylist, &num);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  if ( zvptst( "PRINT" ) )
  {

    zvmessage( " ", "");
    zvmessage( " First guess and there limits: ", "");
    zvmessage( " ----------------------------- ", "");
    zvmessage( " ", "");
  }

/* get the first guess, there limits and temperaturs */

  for (i=0; i<num; i++) /* for all function parameters */
  {
    /* first parameter guess */
    status = phoGetVal( pho_obj, keylist[i], &dval1);
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
    }
    strcpy( keyListMin[i], "MIN_");
    strcat( keyListMin[i], keylist[i]); 

    /* parameter minimum */
    status = zvp( keyListMin[i], &temp, &count);
    status = zvpcnt( keyListMin[i], &count);
    if ( count != 0 )
    { 
	phoParMin[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Minimum limit was not spezified ***","");
	zmabend("photfit abend");
    }
	
    /* parameter maximum */
    strcpy( keyListMax[i], "MAX_");
    strcat( keyListMax[i], keylist[i]); 

    status = zvp( keyListMax[i], &temp, &count);
    status = zvpcnt( keyListMax[i], &count);
    if ( count != 0 )
    { 
	phoParMax[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Maximum limit was not spezified ***","");
	zmabend("photfit abend");
    }
	
    /* parameter temperatur */
    strcpy( keyListT[i], "T_");
    strcat( keyListT[i], keylist[i]); 

    status = zvp( keyListT[i], &temp, &count);
    status = zvpcnt( keyListT[i], &count);
    if ( count != 0 )
    { 
	phoParT[i] = (double)temp;
    } 
    if ( count == 0 ) 
    {
	zvmessage(" ","");
	zvmessage("***photfit2 error***","");
	zvmessage("*** Temperatur was not spezified ***","");
	zmabend("photfit abend");
    }

  /* Check first guess limits and temperaturs, and corrections if possible: */

    if ( phoParMin[i] > dval1 ) phoParMin[i] = dval1;
    if ( phoParMax[i] < dval1 ) phoParMax[i] = dval1;
    if ( phoParMin[i] > phoParMax[i] )
    {
	dval = phoParMin[i];
	phoParMin[i] = phoParMax[i];
	phoParMax[i] = dval;
    }
    if ( phoParMax[i] - phoParMin[i] > D_EPS )
    {
	if( phoParT[i] > TEMP_RANGE * ( phoParMax[i] - phoParMin[i] ) ) \
	   phoParT[i] = TEMP_RANGE * ( phoParMax[i] - phoParMin[i] );
    }
    else phoParT[i] = (double)0.0;
 
    /* screen output */

    if ( zvptst( "PRINT" ) )
    {

      strcpy( msg, "     " );
      strcat( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListMin[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParMin[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListMax[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParMax[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      strcpy( msg, "     " );
      strcat( msg, keyListT[i] );
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", phoParT[i]);
      strcat( msg, cval1);
      zvmessage( msg, "");

      zvmessage( " ", "");
    }

  } /* if for all function parameters */

/* get the other Parameters from the PDF : */

  status = zvpcnt( "CLASS_ID", &classCount);
  if (classCount == 1 )   status = zvp( "CLASS_ID", &pdfClass_id, &count);
  else pdfClass_id = -1;
  status = zvp( "RERUN", &rerun, &count);
  if ( count==0 || rerun<=0 ) rerun=1;
  status = zvp( "MAXITER", &maxIter, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for MAXITER " );
  status = zvp( "NUMTEN", &numTen, &count);
  if ( count==0 || numTen<=0 ) numTen = (int )floor(maxIter/4.0 + 0.5); 
  status = zvp( "NORM", &numNorm, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for NORM " );
  status = zvp( "METROP", &numPrint, &count);
  if ( count==0 || numPrint<=0 ) numPrint=0;
  status = zvp( "PERCENT", &temp, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for PERCENT " );
  percent = (double)temp;
  status = zvp( "TOLERANC", &temp, &count);
  if ( count==0) zmabend(" photfit2 abends: value was not given for TOLERANC" );
  toleranc = (double)temp;

/* start IBIS */

  status = zvpcnt( "INP", &inpCount);
  if ( inpCount<= 0 ) zmabend( " photfit2 : INP file wasn't given ");
  VicarUnit = (int *)malloc( inpCount * sizeof(int) );  
		    if ( VicarUnit == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 5 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  IbisUnit = (int *)malloc( inpCount * sizeof(int) );  
		    if ( IbisUnit == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 6 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  status = zvparm( "INP", fileName, &count, &cnt, phoINP_FILE_NUM, (phoFILENAME_LENGTH+1) );


/* loop over all phocat files */

  for (j=1; j<=inpCount; j++)
  {
     status = zvunit( VicarUnit+j-1, "inp", j, NULL);
       if ( status != 1 ) zvsignal ( *(VicarUnit+j-1), status, 1 );

  /* open the photcat1 file : */

     status = phocat1Open( *(VicarUnit+j-1), IbisUnit+j-1, &nrow, &ncol );

  /* Screen output ? */

     if ( zvptst( "PRINT" ) )
     {
      	zvmessage( " ", "");
      	strcpy( msg, "  " );
      	sprintf( cval1, " %d", j);
      	strcat( msg, cval1);
      	strcat( msg, ".File: ");
      	strcat( msg, &fileName[j-1][0]);
      	zvmessage( msg, "");

      	if (classCount == 1 )  /* if CLASS_ID is given */
      	{
            zvmessage(\
        "   ROW   CLASS_ID     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS",\
        "");
      	} /* if CLASS_ID is given */
      	else /* else if CLASS_ID isn't given */
      	{
            zvmessage(\
        "   ROW     RAD      INC_ANG    EM_ANG  PHAS_ANG     EPS",\
        "");
      	}  /* else if CLASS_ID isn't given */

    } /*if ( zvptst( "PRINT" ) ) */

    /* read the phocat IBIS-file : */

    row = 1;	
    npts = 0;
    do		/* loop throw all rows */
    {
        class_id = pdfClass_id;
        catReadStatus = phocatRead( *(IbisUnit+j-1), &row, &class_id, &illum, \
                                   phoFuncVal, &eps);
        if ( catReadStatus == phoROW_OUT_OF_RANGE || \
	     catReadStatus == phoNO_CLASS_MATCH )
        {
             zvmessage("no more points found in this file","");
             break;
        }
        if (  catReadStatus == phoBAD_POINT ) 
    	{
             row = row + 1;
             continue;
    	}

    	/* Screen output ? */

        if (zvptst("PRINT")) 
    	{
      	    strcpy( msg, " " );
     	    sprintf( cval1, " %4i", row);
      	    strcat( msg, cval1);
	    if (classCount == 1 )
	    {
     	  	sprintf( cval1, " %10i", class_id);
      	  	strcat( msg, cval1);
	    } /* if (classCount == 1 ) */
      	    sprintf( cval1, " %10.3e", *phoFuncVal);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos(illum.cos.inc));
      	    sprintf( cval1, " %9.4f", dval1);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos( illum.cos.em ));
      	    sprintf( cval1, " %9.4f", dval1);
      	    strcat( msg, cval1);
      	    dval1 = RETURN_DEGREES(acos( illum.cos.phas ));
     	    sprintf( cval1, " %9.4f", dval1); 
     	    strcat( msg, cval1);
            sprintf( cval1, " %10.3e", eps); 
            strcat( msg, cval1);

      	   zvmessage( msg, "");

    	} /*  if (zvptst("PRINT")) */

    	row = row + 1;
   	npts = npts + 1; 	 /* number of good points in this IBIS file */
    	tnpts = tnpts + 1; /* number of gool points in all all IBIS files */

    } while (row <= nrow ); /* loop throw all rows */

    if (zvptst("PRINT")) 
    {
      strcpy( msg, "  " );
      sprintf( cval1, " %d",j);
      strcat (msg, cval1);
      strcat( msg,".IBIS file contains");
      sprintf( cval1, "%5i",nrow);
      strcat( msg, cval1);
      strcat( msg, " points");
      zvmessage( msg, "");
      strcpy( msg, "   Number of points left from this file =" );
      sprintf( cval1, " %5i", npts);
      strcat( msg, cval1);
      zvmessage( msg, "");
    } /*  if (zvptst("PRINT")) */

    allrow = allrow + nrow;

    status = phocat1Close( *(VicarUnit+j-1), *(IbisUnit+j-1));

  } /* for (j=1; j<=inpCount; j++)  loop over all phocat files  */

  zvmessage( " ", "");
  zvmessage( " ", "");
  strcpy( msg, "  Total number of input points =" );
  sprintf( cval1, " %6i", allrow);
  strcat( msg, cval1);
  zvmessage( msg, "");
  strcpy( msg, "  Total number of points left =" );
  sprintf( cval1, " %6i", tnpts);
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

/* Prepering for parameter fitting */

/* allocate memory for the buffers for metropolis */
/*  for(i=0; i<5; i++ ) */
  for(i=0; i<2; i++ ) 
  {
    x[i] = (double *)malloc( tnpts * sizeof(double));
		    if ( x[i] == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 7 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  }

  illum_array = (PHO_ILLUM *)malloc(tnpts * sizeof(PHO_ILLUM));
		    if ( illum_array == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 8 failed ***","");
	    		free(illum_array);
	    		zmabend("photfit2 abend");
		    }

/* loop over all phocat files and fill the buffers for metropolis */
  ipts = 0;
  for (j=1; j<=inpCount; j++)
  {
    status = zvunit( VicarUnit+j-1, "inp", j, NULL);
      if ( status != 1 ) zvsignal ( *(VicarUnit+j-1), status, 1 );

  /* open the photcat1 file : */

    status = phocat1Open( *(VicarUnit+j-1), IbisUnit+j-1, &nrow, &ncol );

    /* read the phocat IBIS-file : */
    row = 1;	
    npts = 0;
    do		/* loop throw all rows */
    {
       class_id = pdfClass_id;
       catReadStatus = phocatRead( *(IbisUnit+j-1), &row, &class_id, &illum, \
                                   phoFuncVal, &eps);
       if ( catReadStatus == phoROW_OUT_OF_RANGE || \
	     catReadStatus == phoNO_CLASS_MATCH ) break;

       if (  catReadStatus == phoBAD_POINT ) 
       {
	  row = row + 1;
	  continue;
       }

       /* fill the buffers */
      
      illum_array[ipts] = illum; 
       *(x[0] + ipts) = *phoFuncVal;
       *(x[1] + ipts) = eps;

       ipts = ipts + 1;
       row = row + 1;
       npts = npts + 1;

   } while (row <= nrow ); /* loop throw all rows */

    status = phocat1Close( *(VicarUnit+j-1), *(IbisUnit+j-1));
  
  } /* for (j=1; j<=inpCount; j++)  lopp over all phocat files  */

  for (i=0; i<num; i++) /* for all function parameters */
  {
    limits[0][i] = phoParMin[i];
    limits[1][i] = phoParMax[i];
  } /* for all function parameters */

  if( rerun<1 ) rerun = 1;

  for (i=0; i<num; i++)  /* for all function parameters */
  {
    std_dev[i] = 0.0;
    mean[i] = 0.0;
    res[i] = (double *)malloc( rerun * sizeof(double));
		    if ( res[i] == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***photfit2 error***","");
	    		zvmessage("*** memory allocation 9 failed ***","");
	    		/*free(pKeyListT);*/
	    		zmabend("photfit2 abend");
		    }
  } /* for all function parameters */

  for (l=1; l<=rerun; l++) /* for statistics */
  { 

    status = phoMetropolis( pho_obj, limits, phoParT, x, illum_array, tnpts, toleranc,
			    percent, maxIter, numTen, numNorm, numPrint, &xcost); 

    if ( zvptst( "PRINT" ) )
    {
      zvmessage( " ", "");
      sprintf( cval1, " %10i", l);
      strcpy( msg, cval1);
      strcat( msg, ". Metropolis-result  :" );
      zvmessage( msg, "");
    }

    /* solution statistics : */

    for (i=0; i<num; i++) /* for all function parameters */
    {
      /* first parameter guess */
      status = phoGetVal( pho_obj, keylist[i], &dval1);
      if(status!=phoSUCCESS)
      {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
      }

      mean[i] = mean[i] + dval1;
      *(res[i] + l-1) = dval1;

      if ( zvptst( "PRINT" ) )
      {
        strcpy( msg, " " );
        strcat( msg, keylist[i]);
        strcat( msg, " = ");
        sprintf( cval1, " %10.3e", dval1);
        strcat( msg, cval1);
        zvmessage( msg, "");
      } /*end PRINT */

    } /* for all function parameters */

    if ( zvptst( "PRINT" ) ) zvmessage( " ", "");

  } /* for statistics */

  for ( i=0; i<tnpts; i++)
  {
    status = phoBidiRef(pho_obj,&illum_array[i], phoFuncVal);
    reflErr= *(x[0] + i) - *phoFuncVal;
    sumerr = sumerr + fabs(reflErr);
    if(fabs(reflErr)< toleranc) n = n+1;
  }
  sumerr = sumerr/((double )tnpts);
  least = (int )floor(0.5 + percent * tnpts / 100. );

   
  zvmessage(" ", "");
  zvmessage("          Statistical results:", "");
  zvmessage("          --------------------", "");
  zvmessage(" ", "");

  zvmessage("   INC_ANG    EM_ANG  PHAS_ANG      I/F     I/F-FitVal ", "");

  for ( i=0; i<tnpts; i++)
  {
    illum = illum_array[i];
    status = phoBidiRef(pho_obj,&illum_array[i], phoFuncVal);
    reflErr= *(x[0] + i) - *phoFuncVal;
    dval1 = RETURN_DEGREES(acos( illum.cos.inc ));
    sprintf( cval1, " %9.4f", dval1);
    strcpy( msg, cval1);
    dval1 = RETURN_DEGREES(acos( illum.cos.em ));
    sprintf( cval1, " %9.4f", dval1);
    strcat( msg, cval1);
    dval1 = RETURN_DEGREES(acos( illum.cos.phas ));
    sprintf( cval1, " %9.4f", dval1);
    strcat( msg, cval1);
    sprintf( cval1, " %10.7f", *phoFuncVal);
    strcat( msg, cval1);
    sprintf( cval1, " %12.8f", reflErr);
    strcat( msg, cval1);
    if( n >= least)
    {
	if( fabs(reflErr)< toleranc)
	{
    	   strcat( msg, "   used ");
	  
	}
	else
	{
    	   strcat( msg, " unused ");
	}

    }
    else
    {
	strcat( msg, "   used ");
    }
    zvmessage(msg, "");
  }

  strcpy( msg, "        Mean I/F error per fitted point =");
  sprintf( cval1, " %12.8f", sumerr);
  strcat( msg, cval1);
  zvmessage(msg, "");

  zvmessage( " ", "");
  zvmessage( " MEAN VALUES (to thinking about the stability of fitting) :", "");

  for (i=0; i<num; i++) /* for all function parameters */
  {
    if(rerun>1) mean[i] = mean[i] / ( (double)rerun );
    else mean[i] = mean[i] / ((double)rerun);

    for (l=1; l<=rerun; l++)
    { 
      std_dev[i] = std_dev[i]						   \
                 + (mean[i] - *(res[i] + l-1)) * (mean[i] - *(res[i] + l-1));
    }

    if(rerun>1) std_dev[i] = sqrt(std_dev[i]) / ( (double)rerun - (double)1.0);
    else std_dev[i] = sqrt(std_dev[i]) / ( (double)rerun);

    strcpy( msg, " " );
    strcat( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", mean[i]);
    strcat( msg, cval1);
    strcat( msg, " +/-");
    sprintf( cval1, " %10.3e", std_dev[i]);
    strcat( msg, cval1);
    zvmessage( msg, "");
    
  }
  zvmessage( " ", "");
  zvmessage(" ", "");

 
  zvmessage("*****************************************************************************",
	    "");
  zvmessage("* ***************************************************************************",
	    "");
  zvmessage("* *", "");
  zvmessage("* *  RESULTING PARAMETERS OF BEST-FIT: ", "");
  zvmessage("* *", "");

  for (i=0; i<num; i++) /* for all function parameters */
  {
      status = phoGetVal( pho_obj, keylist[i], &dval1);
      if(status!=phoSUCCESS)
      {
			zvmessage(" ","");
	    		zvmessage("***main44 error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
      }

      strcpy( msg, "* *      " );
      strcat( msg, keylist[i]);
      strcat( msg, " = ");
      sprintf( cval1, " %10.3e", dval1);
      strcat( msg, cval1);
      zvmessage( msg, "");

  } /* for all function parameters */
  zvmessage("* *", "");
  zvmessage("* ***************************************************************************",
	    "");
  zvmessage("*****************************************************************************",
	    "");
  zvmessage(" ", "");

  status = phoFree( pho_obj);
  free(x[0]);
  free(x[1]);
  free(illum_array);
  free(VicarUnit);
  free(IbisUnit);
  free(phoFuncVal);
  for (i=0; i<num; i++)  /* for all function parameters */
  {
    free(res[i]);
  } /* for all function parameters */
  return;
}



/************************************************************************
*									*
*		phocat1Open						*
*									*
************************************************************************/

/* opens a photometric catalog file for read as comming from PHOTTEST2 */

int phocat1Open(
	int VicarUnit, 
	int *IbisUnit,
	int *nrow,
	int *ncol)

/********************************************************************
	* VicarUnit	IBIS Unit No.
	* IbisUnit	VICAR Unit No.
	* IbisBuff	IBIS buffer
*********************************************************************/
{

  int status, count;
  char *mode;
  char org[10]=IORG_COLUMN;
  char type[phoCAT_NAME_LENGTH+1];
  char version[10];
  status=1; 
  mode = IMODE_READ;


/* Open the file : */

  status = IBISFileOpen(VicarUnit, IbisUnit, mode, 0, 0, NULL, org ); 
  	if (status != 1) IBISSignalU( VicarUnit, status, 1 );

 
/* get the number of rows and columns */

  count = IBISFileGet( *IbisUnit, IFILE_NR, nrow, 1, 1, 0 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );

  count = IBISFileGet( *IbisUnit, IFILE_NC, ncol, 1, 1, 0 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );

  if ( *nrow < 1 )
   {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** phocat file is empty --> can't read ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
  } /* if ( *nrow < 1 ) */

  if ( *ncol < 17 )
   {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** phocat file hasn't enough coloumns ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
 } /* if ( *ncol < 17 ) */





/* check which IBIS-Version : */
  
  count = IBISFileGet( *IbisUnit, IFILE_VERSION, version, 1, 1, 9 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );



  if (strcmp(version,IVERSION_1)!=0) /*  for IBIS-2 */
  {

  /* Now check to see which subtype : */

    count = IBISFileGet( *IbisUnit, IFILE_TYPE, type, 1, 1, phoCAT_NAME_LENGTH+1 );
  	if (count != 1) IBISSignal( *IbisUnit, count, 1 );
    if(strcmp(type, phoCAT_TYPE)!=0 || strcmp(type,"tiepoint")!=0) /*if phocat*/
    {} /* if phocat */
    else /* unknown photometric catalog file */
    {
	zvmessage(" ", "");
	zvmessage("***phocat1Open error***","");
	zvmessage("*** file isn't a phocat --> can't read as such ***","");
	status = IBISFileClose( *IbisUnit, NULL );
   	if (status != 1) IBISSignalU( VicarUnit, status, 1 );
    } /* else unknown photometric catalog file */
  } /* if (strcmp(version,IVERSION_1)!=0) */


  return status; 

}



/************************************************************************
*									*
*		phocatRead						*
*									*
************************************************************************/

/* reads contens into the illu-object and from a photometric catalog file as comming */
/* from PHOTTEST2 */

int phocatRead(
	int IbisUnit, 
	int *row,
	int *class_id, 
	PHO_ILLUM *illum, 
	double *phoFuncVal,
	double *eps)
{
  int status, rstatus, count, col, ncol, nrow, i, tclass_id;
  float ftemp;
  double temp, dtemp, caz;
  char type[phoCAT_NAME_LENGTH+1];
  char ctmp1[133];
  char version[10];

  status = 1;
  rstatus = 1;


 /* Check up the number of rows and columns */

  count = IBISFileGet( IbisUnit, IFILE_NR, &nrow, 1, 1, 0 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );

  count = IBISFileGet( IbisUnit, IFILE_NC, &ncol, 1, 1, 0 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );

 


 if ( nrow < 1 )
  {
	zvmessage(" ", "");
	zvmessage("***phocatRead error***","");
	zvmessage("*** phocat file is empty --> can't read ***","");
	status = IBISFileClose( IbisUnit, NULL );
   	if (status != 1) IBISSignal( IbisUnit, status, 1 );
  } /* if ( nrow < 1 ) */





/* check which IBIS-Version : */
  
  count = IBISFileGet( IbisUnit, IFILE_VERSION, version, 1, 1, 9 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );







  if (strcmp(version,IVERSION_1))  /* if IBIS-2 */
  {

  /* Check to see which subtype : */

    count = IBISFileGet( IbisUnit, IFILE_TYPE, type, 1, 1, phoCAT_NAME_LENGTH+1 );
  	if (count != 1) IBISSignal( IbisUnit, count, 1 );




    if ( strcmp(type, phoCAT_TYPE) == 0 )   /* if phocat */
    {
  	illum->type.sunshadow = illNoShadow;
  	illum->type.viewshadow = illNoShadow;
  	illum->type.mode = illEllCos;

	strcpy( ctmp1, "CLASS_ID" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );



	if (*class_id >= 0 )  /* search for class-id */
	{
	  if ( *row <= 0) *row = 1;
	  if (*row > nrow ) return rstatus=phoROW_OUT_OF_RANGE;
	  for(i = *row;  i <= nrow;  i++ )
	  {
            status = IBISColumnRead(  IbisUnit, (char *)&tclass_id, col, i, 1);

   	       if (status != 1) IBISSignal( IbisUnit, status, 1 );
	    if( tclass_id == *class_id )
	    {
		*row = i;
		break;
	    } /* if( tclass_id == *class_id ) */
	  } /* for(i=*row; i <= nrow; i++ ) */
	  if( tclass_id != *class_id )
	  {
	     *row = nrow;
	     return rstatus=phoNO_CLASS_MATCH;
	  } /* if( tclass_id == *class_id ) */

	} /* if (*class_id >= 0 ) */



	else /* read for given row */
	{
          status = IBISColumnRead(  IbisUnit, (char *)class_id, col, *row, 1);

   	     if (status != 1) IBISSignal( IbisUnit, status, 1 );
	} /* else read for given row */




	strcpy( ctmp1, "IncidenceAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 90.0 || temp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )temp));
	illum->cos.inc = dtemp;

	strcpy( ctmp1, "EmissionAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 90.0 || temp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )temp));
	illum->cos.em = dtemp;

	strcpy( ctmp1, "PhaseAngle" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)&temp, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( temp >= 1800.0 || temp < 00.0 ) return phoBAD_POINT;
	illum->cos.phas = cos(RETURN_RADIANS((double )temp));

	   if (illum->cos.inc < 0 || illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em < 0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 ) return phoBAD_POINT;

	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - illum->cos.inc * illum->cos.inc) <= 0.0 || fabs(1.0 - 
illum->cos.em * illum->cos.em) <= 0.0 )
		caz = 1.0;
	   else  
		caz = ( illum->cos.phas - illum->cos.em * illum->cos.inc ) / sqrt( fabs(1. - illum->cos.inc * illum->cos.inc) * fabs(1. - illum->cos.em * illum->cos.em) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) return phoBAD_POINT;


	strcpy( ctmp1, "I/F" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)phoFuncVal, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );

	/* Check if point never was computed (off picture...).		*/
	/* PHOTOM tries to keep the 1:1 order between IBIS files	*/
	/* computed in BATCH mode. Points which cannot be computed	*/
	/* are set to zero.						*/

	if ( illum->cos.inc >= 1.0  - ANG_EPS && 	\
	     illum->cos.em >= 1.0  - ANG_EPS && 	\
	     illum->cos.phas >= 1.0  - ANG_EPS && 	\
	     *phoFuncVal == 0.0) return phoBAD_POINT;


	strcpy( ctmp1, "StandDev" );
	count = IBISColumnFind( IbisUnit, ITYPE_GROUP, ctmp1, &col, 1, 1);
  	  if (count != 1) IBISSignal( IbisUnit, count, 1 );
        status = IBISColumnRead(  IbisUnit, (char *)eps, col, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );



    } /* if phocat */
    else
    {
	zvmessage(" ", "");
	zvmessage("***phocatRead error***","");
	zvmessage("*** file isn't a phocat --> can't read as such ***","");
	status = IBISFileClose( IbisUnit, NULL );
   	if (status != 1) IBISSignal( IbisUnit, status, 1 );
    } /* else no phocat*/

  } /* if IBIS-2 */




  else		/* for IBIS-1 */
  {
     	if ( ncol < 17 )
   	{
	   zvmessage(" ", "");
	   zvmessage("***phocatRead error***","");
	   zvmessage("*** IBIS_1 phocat file hasn't enough coloumns ***","");
	   status = IBISFileClose( IbisUnit, NULL );
   	   if (status != 1) IBISSignal( IbisUnit, status, 1 );
  	} /* if ( ncol < 17 ) */


  	illum->type.sunshadow = illNoShadow;
  	illum->type.mode = illEllCos;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 11, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 ); 
	if ( ftemp >= 90.0 || ftemp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )ftemp));
	illum->cos.inc = dtemp;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 12, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( ftemp >= 90.0 || ftemp < 0.0 ) return phoBAD_POINT;
	dtemp = cos(RETURN_RADIANS((double )ftemp));
	illum->cos.em = dtemp;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 13, *row, 1);

  	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	if ( ftemp >= 180.0 || ftemp < 0.0 ) return phoBAD_POINT;
	illum->cos.phas = cos(RETURN_RADIANS((double )ftemp));


	   if (illum->cos.inc < 0 || illum->cos.inc-ANG_EPS >= 1.0 || illum->cos.em < 0 || illum->cos.em-ANG_EPS >= 1.0 || illum->cos.phas < -1.0 || illum->cos.phas > 1.0 ) return phoBAD_POINT;

	   if ( fabs(illum->cos.inc) < ANG_EPS || fabs(illum->cos.em) < ANG_EPS ) 
		caz = 1.0;
           if ( fabs(1.0 - illum->cos.inc * illum->cos.inc) <= 0.0 || fabs(1.0 - 
illum->cos.em * illum->cos.em) <= 0.0 )
		caz = 1.0;
	   else  
		caz = ( illum->cos.phas - illum->cos.em * illum->cos.inc ) / sqrt( fabs(1. - illum->cos.inc * illum->cos.inc) * fabs(1. - illum->cos.em * illum->cos.em) );
	   if (caz+ANG_EPS < -1.0 || caz-ANG_EPS > 1.0 ) return phoBAD_POINT;


        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 16, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	*phoFuncVal = (double)ftemp;

	/* Check if point never was computed (off picture...).		*/
	/* PHOTOM tries to keep the 1:1 order between IBIS files	*/
	/* computed in BATCH mode. Points which cannot be computed	*/
	/* are set to zero.						*/

	if ( illum->cos.inc >= 1.0  - ANG_EPS && 	\
	     illum->cos.em >= 1.0  - ANG_EPS && 	\
	     illum->cos.phas >= 1.0  - ANG_EPS && 	\
	     *phoFuncVal == 0.0) return phoBAD_POINT;

        status = IBISColumnRead( IbisUnit, (char *)&ftemp, 18, *row, 1);

   	  if (status != 1) IBISSignal( IbisUnit, status, 1 );
	*eps = (double)ftemp;

	*class_id = -1;

  } /* else IBIS-1 */

  return rstatus;
}






/************************************************************************
*									*
*		phocat1Close						*
*									*
************************************************************************/

/* closes a photometric catalog file as comming from PHOTOM */

int phocat1Close(
	int VicarUnit, 
	int IbisUnit)

/********************************************************************
	* VicarUnit	IBIS Unit No.
	* IbisUnit	VICAR Unit No.
*********************************************************************/
{
  int status;

/* Now we try to close-down the IBIS file */
 
 status = IBISFileClose(IbisUnit, ICLOSE_UDELETE);

/* ... and if something is wrong we issue an IBIS warning and
	finish the subroutine as normal */

  if (status != 1) IBISSignalU(VicarUnit, status, 0);

  return status;

}




/************************************************************************
*									*
*		phoMetropolis						*
*									*
************************************************************************/

/*  The technique was conceived of by Edward Teller and Nocholas
 *  Metropolis in 1953 and in my humble opinion is the most
 *  powerful numerical minimization method in existence.
 *  I have taken the liberty to modify it as i saw fit by incorporating
 *  ideas from several authors including a few of my own.
 *
 *  HISTORY
 *   Original Programmer:               J. J. Lorre
 *   Current Cognizant Programmer:      F.Oschuetz
 *   Documentation Author:              J. J. Lorre
 *   Revision:                          Claus Groebner 8.3.95 C-Version
 *					F.Oschuetz 7/95 photometrie things
 *   
 *   REFERENCES:     	Szu H. H., SPIE 698 Real Time Signal Processing
 *                  	#9 (1986) 59 "Non-Convex Optimization"
 *		    	J.J.Lorre, Function Minimisation Whith Partially 
 *			Correct Data,
 *			J. Soc.  Ind.Appl. Math. 43, 1990, 123-127
 *
 *   ATTENTION !! ALL ARRAY ARE INITIATED BY 1 TO MAXPAR-1
 *               for the compatibility to the Marquart-Levenberg functions.
 *
 *  PARAMETER:
 *
 *  PHO pho_obj			photometric object,:
 *			input	first guess;
 *			output	solution.
 *  double limits[2][]	input	Array of upper/lowest limit of parameters.
 *  double range[]	input	Bounds the jumps of solution (temperatur).
 *  double x[2][]	input 	Array of data : I/F, [sigma]:
 *  PHO_ILLUM *illum_array input Array of data illumination objects
 *  int npts		input	The number of data points in x.
 *  double tolerance	input	Tolerance value for residuals(data,phoFuncVal).
 *  double minData	input	Minimum % of data points to be used.
 *  int numIter		input	The total number of iterations permitted.
 *  int numTen		input	Number of iterations to reduce the error by 10.
 *  int numNorm		input	The number of iterations between normalizations.
 *  int numPrint	input	The number of iterations between printouts.
 *  double *xcost	output	Returns minimal cost.
 *
 *  Function value:	Status indicator. 
 */


int phoMetropolis(
	PHO pho_obj,
	double limits[2][phoMAX_PARAM_PER_FUNC],
	double range[phoMAX_PARAM_PER_FUNC],
	double *x[2],
	PHO_ILLUM *illum_array,
	int npts,
        double tolerance, 
	double minData,
	int numIter,
	int numTen,
	int numNorm,
	int numPrint,
	double *xcost)
{
  int status, rstatus;
  int i,ii, kk, fail, loop, loop1, loop2, loop3, numreset, ind;
  int narg;
  float randout;
  double temp[phoMAX_PARAM_PER_FUNC], aa[phoMAX_PARAM_PER_FUNC];
  double mincost, costsum, tmp, prob, boltzmann = 0.0, energy;
  double answer[phoMAX_PARAM_PER_FUNC], minx[phoMAX_PARAM_PER_FUNC];
  double scale, c1, c2, c3, maxCostDif;
  double  xrd2, pi2;
  double dval;
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char msg[133], cval1[133];
  time_t tim;
  long *seed1 = NULL;

  if (seed1 != NULL )  free(seed1);
	seed1 = (long *)malloc(sizeof(long));
	if (seed1 == NULL )
	{
	  zvmessage(" ", "");
	  zvmessage("***phoMetropolis error***","");
	  zvmessage("*** memory allocation 10 failed ***","");
	  free(seed1);
	  zmabend("phottest abend");
	}

  pi2   = (double)0.5 * M_PI;
  xrd2  = (double)1.0 /((double)1.0);
  maxCostDif = (double)50.0;
/*  maxCostDif = 50.0 * 0.0001 *tolerance*/;

/* get the first guess for the parameters */

  status = phoGetKeys( pho_obj, 0, &narg);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  status = phoGetKeys( pho_obj, keylist, &narg);
  if(status!=1)
  {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
  }

  for (i=0; i<narg; i++) /* for all function parameters */
  {
    status = phoGetVal( pho_obj, keylist[i], answer+i);
    if(status!=phoSUCCESS)
    {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
    }
  } /* for all function parameters */ 


/*  Compute a random number seed based on the time of day.  */

/*  if(!(time(&tim) % 2))  tim--; */
  tim = time(NULL);
  *seed1 = (long )tim;
  while( *seed1 > 714025) *seed1 = *seed1 - 714025;
  if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  zrangen( seed1, &randout ); 
/*  zget_ran( &tim, &randout );*/

/*  Compute the cost at position answer and assign to variable c1.  */

  if (!(ind=phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c1))) 
  {
    zvmessage( " phoMetropolis: Failure in phoCost at initial guess","");
    free(seed1);
    return rstatus=ind;
  }

/*  Save the start guess position in case it was chosen wisely.  */
/*  And set initial temperatures to the range estimates.  	 */

  mincost = c1;
  for (ii=0; ii<narg; ii++)  /* for all function parameters */
  {
    minx[ii] = answer[ii];
    temp[ii] = range[ii];
  } /* for all function parameters */

  fail = 0;	/* costneu-costalt > 50/0.0001/tolerance */
  loop1 = 0;	/* Number of lower costneu */
  loop2 = 0;	/* Accepted higher costneu */
  loop3 = 0;	/* Number of to big jumps */
  numreset = numTen/10;
  if ( numreset<= (int)0 ) numreset=1;
  costsum = numTen; 
  costsum = 1.0/costsum;
  scale = pow(0.1, costsum);
/*  minscale = pow (10.0, ((double )numIter)/((double )numTen) ); 
  if ( scale < minscale) scale = minscale;
*/
  if (numPrint!=0) 
  {
    zvmessage(" ", "");   
    zvmessage(" 		phoMetropolis :", "");   
    zvmessage(" ", "");   
    strcpy( msg, " scaling factor reduces temperatur = " );
    sprintf( cval1, " %11.6e", scale);
    strcat( msg, cval1);
    zvmessage(msg, "");   
    zvmessage(" ", "");   
/*    zvmessage(\
" Boltzmann     Temperatur", ""); */
    zvmessage(\
"  loopNumber        Cost  #downhill    #uphill  #rejected #out_of_bounds","");

    sprintf( cval1, " %12s", keylist[0]);
    strcpy( msg, cval1 );

    for (i=1; i<narg; i++) /* for all function parameters */
    {
	  sprintf( cval1, " %12s", keylist[i]);
	  strcat( msg, cval1);
    } /* for all function parameters */

    zvmessage( msg, "");

  }  /* for numPrint!=0 */

/*  MAIN LOOP: loop on number of successful changes in solution space. */

  for (loop = 1; loop <= numIter; loop++) /* main loop */
  {
  /*  Compute the delta_cost/temperature ratio for normalization of
   *  probabilities. Note that this is the Boltzman constant for this 'system'.
   */

    if ((loop % numNorm) == 1)	/* renormalisation */
    {
      costsum = tmp = (double)0.0;
      kk = 0;
      for (ii=0; ii<narg; ii++)  aa[ii] = answer[ii];

      for (ii=0; ii<narg; ii++) /* for all parameters */
      {
        tmp += temp[ii];

        aa[ii] = answer[ii] - temp[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if( !(status == phoSUCCESS || status == phoKEYWD_CHANGED) )
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 1 failed ***","");
	    		zmabend("photfit abend");
        }

	ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);

        if( ind == 1 ) /* if parameters valid */
        {
          kk++;
          costsum += fabs(c1 - c2);
/*          tmp += temp[ii] * temp[ii]; */
/*          tmp += temp[ii]; */

        } /* if parameters valid */

        aa[ii] = answer[ii] + temp[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 2 failed ***","");
	    		zmabend("photfit abend");
        }

	ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);

        if (ind == 1 ) /* if parameters valid */
        {
          kk++;
          costsum += fabs(c1 - c2);
/*          tmp += temp[ii] * temp[ii]; */ 
/*          tmp += temp[ii]; */
        } /* if parameters valid */

        aa[ii] = answer[ii]; /*reset the parameters as befor normalisation */

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 3 failed ***","");
	    		zmabend("photfit abend");
        }

      } /* for all parameters */

      if (kk == 0) /* if parameters valid */
      {
        zvmessage(" METROPOLIS: Failure in normalization procedure: ", "");
	zvmessage(" solution + - range out of bounds", "");
        free(seed1);
        return rstatus=ind;
      } /* if parameters valid */

      tmp = tmp / (double )narg; /* mean temperatur */
      costsum = costsum /(double )kk;  

      if (tmp < 1.0e-30)    /* prevent divison by 0.0 */
        boltzmann = 1.0e-30;
      else
        boltzmann = 5.0 * costsum/tmp; 

/*      boltzmann = 5.0 * costsum /(kk * temp[1]);  */

      if (numPrint!=0)
      { 
	strcpy (msg, "  Boltzmann = ");
	sprintf( cval1, " %10.6e", boltzmann);
	strcat( msg, cval1);
	strcat( msg, "  Temperatur = ");
 	sprintf( cval1, " %10.6e", tmp);
	strcat( msg, cval1);
/*	sprintf( cval1, " %10d", kk);
	strcat( msg, cval1);
 	sprintf( cval1, " %10.6e", 5.0*costsum);
	strcat( msg, cval1); */
    	zvmessage( msg, "");

      } /* if (numPrint!=0) */

    } /* END if ((loop % numNorm) == 1) renormalisation */ 

/*  Decrement the temperature according to the multiplicative cooling schedule.
    for (ii=0; ii<narg; ii++)  
    {
	temp[ii] *= scale;
  	if ( temp[ii] < range[ii] * minscale) temp[ii] = range[ii] * minscale; 
    }

    energy = boltzmann * temp[1];
*/
    tmp = (double)0.0;
    for (ii=0; ii<narg; ii++)
    {
      temp[ii] *= scale;
/*      if ( temp[ii] < range[ii]*minscale) temp[ii] = range[ii] * minscale; */

/*      tmp += temp[ii]*temp[ii]; */
      tmp += temp[ii]; 
    }
    energy = boltzmann * tmp / ((double )narg);

/*  Compute a solution space guess using a Cauchy-Lorentzian random
 *  probability distribution function.
 */

    do  /* while( ! ind ) */
    {
      for (ii=0; ii<narg; ii++) /* new function parameters */
      {
  	if( *seed1 > 714025) *seed1 = *seed1 - 714025;
  	if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  	zrangen( seed1, &randout );
	dval = randout;
        aa[ii] = temp[ii] * tan( dval * M_PI + pi2) + answer[ii];

	status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 4 failed ***","");
	    		zmabend("photfit abend");
        }
      } /* new function parameters */

      ind = phoCost(pho_obj,x,illum_array,limits,npts,tolerance,minData, &c2);
 
      if (ind!=1) /* wrong function parameters */
      {
	for (ii=0; ii<narg; ii++) /* for all function parameters */
	{
	  aa[ii] = answer[ii];
	  status = phoSetVal( pho_obj, keylist[ii], aa[ii]);
          if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
          {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 5 failed ***","");
	    		zmabend("photfit abend");
          }
	} /* for all function parameters */
	loop3++;
      } /* wrong function parameters */
      else /* valid function parameters */
      {
        if(c2 < c1) 
	{
	/*  Accept lower cost position.
 	*  We always accept a downhill cost route if offered.
 	*/
          c1 = c2;
          for (ii=0; ii<narg; ii++)  /* for all function parameters */
	  {
	    answer[ii] = aa[ii];
	    status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
            if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
            {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 6 failed ***","");
	    		zmabend("photfit abend");
            }
	  } /* for all function parameters */
          loop1++;
        }/* Accept lower cost position. */
        else 
	{
/*  Compute probability of accepting higher cost position.
 *  This comes from the Boltzman probability of our system 
 *  transitioning from energy state c1 to energy state c2.
 */
          c3 = (c2 - c1)/energy;

          if (c3 > maxCostDif) { fail++; ind = 1; }
          else 
	  {
/*            prob = 1.0/(1.0 + exp(c3));  */
	    if (c3< 1.0e-15) prob=1.0;
            else prob = 1.0/exp(c3);

/*  Evaluate the probability by comparing it against chance.  */

	    if( *seed1 > 714025) *seed1 = *seed1 - 714025;
	    if(!(*seed1 % 2))  *seed1 = *seed1 - (long )1;
  	    zrangen( seed1, &randout );
	    dval = randout;
            if (prob > xrd2*dval ) /*  Accept higher cost position.  */
            {	
              c1 = c2;
              for (ii=0; ii<narg; ii++)
	      {
		answer[ii] = aa[ii];
		status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
                if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
                {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 7 failed ***","");
	    		zmabend("photfit abend");
                }
	      } 
              loop2++;
            }
            else { fail++; ind = 1; }	/*  Reject higher cost position.  */
          }
        }
      } /* END loop3++ */

    } while(! ind );

/*  Save the minimum cost and associated solution as we go.  */

    if (c1 < mincost) 
    {
      mincost = c1;
      for (ii=0; ii<narg; ii++) minx[ii] = answer[ii];
    }

/*  Reset the solution pointer to the minimum cost location every numreset
 *  successful iterations.
 */
    if (!(loop % numreset)) 
    {
/*  	strcpy( msg,"reset of parameters to ones of mincost = " );
	sprintf( cval1, " %10.6e", mincost);
	strcat( msg, cval1);
	zvmessage(msg,""); 
*/
      c1 = mincost;
      for (ii=0; ii<narg; ii++)
      {
	answer[ii] = minx[ii];
	status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
        if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
        {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal 8 failed ***","");
	    		zmabend("photfit abend");
        }

      }
/*        sprintf( cval1, " %10.6e", answer[0]);
  	strcpy( msg, cval1 );
	for (ii=1; ii<narg; ii++)
	{
 	  sprintf( cval1, " %10.6e", answer[ii]);
	  strcat( msg, cval1);
	}
    	zvmessage( msg, "");
*/
/*      loop++; */
    }

/*  Print out a status every PRNT iterations. */

    if (numPrint!=0) 
    {
      if (!(loop % numPrint)) 
      {

	sprintf( cval1, " %10i", loop);
  	strcpy( msg, cval1 );
	sprintf( cval1, " %10.6e", c1);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop1);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop2);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", fail);
	strcat( msg, cval1);
	sprintf( cval1, " %10i", loop3);
	strcat( msg, cval1);
    	zvmessage( msg, "");

	sprintf( cval1, " %10.6e", answer[0]);
  	strcpy( msg, cval1 );
	for (ii=1; ii<narg; ii++)
	{
 	  sprintf( cval1, " %10.6e", answer[ii]);
	  strcat( msg, cval1);
	}
    	zvmessage( msg, "");

        fail = loop1 = loop2 = loop3 = 0;
      }
    }
  } /*  END of MAIN LOOP  */

/*  Put minimum solution into "answer" & it's cost into xcost. */

  for (ii=0; ii<narg; ii++) 
  {
    answer[ii] = minx[ii];
    status = phoSetVal( pho_obj, keylist[ii], answer[ii]);
    if(!(status == phoSUCCESS || status == phoKEYWD_CHANGED))
    {
			zvmessage(" ","");
	    		zvmessage("***phoMetropolis error***","");
	    		zvmessage("*** phoSetVal  9 failed ***","");
	    		zmabend("photfit abend");
    }
/*    range[ii] = temp[ii]; */
  }

  *xcost = mincost;
/*  free(pkeylist);*/
 
  free(seed1);

  return rstatus=ind;
}




/************************************************************************
*									*
*		phoCost							*
*									*
************************************************************************/

/* The cost function has two levels in order to reject bad data point.
 * For each observable we define a residual er=| I/F(ii) - phofuncval |
 * We the count the number of points N for which er<tolerance where tolerance 
 * is a precision thresold on the residual er.
 * Then we define the cost as the sum of residuals weighted by the standard 
 * deviation of the data :
 *
 *			   { sum[er(ii)/sigma(ii)]/N,   ii[0,N]   for N>=minNum 
 * cost(all I/F,pho_obj) = {sum[er(ii)/sigma(ii)]/npts, ii[0,npts] for N<minNum
 * 
 * where minNum is the minimum number of points acceptable. The minimum of 
 * standard deviation will be be set to 0.0001*tolerance.
 */

int phoCost(
	PHO pho_obj,	 /* input photometric object */
	double *x[2],	 /* input Array of data :  I/F, [sigma] */
	PHO_ILLUM *illum_array, /* Array of data illumination objects */
	double limits[2][phoMAX_PARAM_PER_FUNC], /* parameters minima&maxima */
	int npts, 	 /* input Number of data points in x  */
        double tolerance, /* input Maximum of acceptable residual */
	double minData,	 /* input Percent of minimal acceptable data points */
	double *error) 
{
  int status, rstatus=1; /* TRUE */
  int narg;
  int i,ii, n, minNum;
  double er, sum, sigSum, sig, sigma, sig2, ref, minSigma, *phoFuncVal;
  double a[phoMAX_PARAM_PER_FUNC];
/*  char *pkeylist;*/
  char keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  PHO_ILLUM illum;

  phoFuncVal = (double *)malloc((4 + phoMAX_PARAM_PER_FUNC) * sizeof(double));
  if( phoFuncVal == NULL ) 
  {
    zvmessage("*** phoCost: memory allocation failed***","");
    free(phoFuncVal);
    zmabend("phoCost abend");
  }

/* get the photometric parameters */

  status = phoGetKeys( pho_obj, 0, &narg);
  if(status!=1) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
		    }


/*  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
		    if ( pkeylist == NULL) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** memory allocation 11 failed ***","");
	    		free(pkeylist);
	    		zmabend("photfit abend");
		    }

  pkeylist = (char *)keylist;
*/
  status = phoGetKeys( pho_obj, keylist, &narg);
  if(status!=1) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetKeys failed ***","");
	    		zmabend("photfit abend");
		    }
  for (i=0; i<narg; i++) /* for all function parameters */
  {
    status = phoGetVal( pho_obj, keylist[i], a+i);
  if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoGetVal failed ***","");
	    		zmabend("photfit abend");
		    }


/* strcpy( msg," min: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", limits[0][i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
  	  strcpy( msg," max: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", limits[1][i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
  	  strcpy( msg," par: ");
for (i=0; i<narg; i++)
{
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
}
    	  zvmessage( msg, "");
*/

    if(a[i] < limits[0][i] || a[i] > limits[1][i]) 
    {
/*      free(pkeylist); */

/*	  strcpy(  msg, "abort : ");
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
	  strcat( msg, "Min_" );
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", limits[0][i]);
	  strcat( msg, cval1);
	  strcat( msg, " Max_" );
	  strcat( msg, keylist[i] );
	  sprintf( cval1, " %10.6e", limits[1][i]);
	  strcat( msg, cval1);
    	  zvmessage( msg, "");
*/
      free(phoFuncVal);
      return rstatus=0;
    }

/*	  strcpy( msg, keylist[i] );
	  strcat( msg, " = ");
	  sprintf( cval1, " %10.6e", a[i]);
	  strcat( msg, cval1);
    	  zvmessage( msg, "");
*/
  } 
/*    	  zvmessage( " ", "");*/

  n = 0;
  *error = sum = 0.0;
  sig = sigSum = 0.0;
/*
  ref = 0.01 * limits[0][phoMAX_PARAM_PER_FUNC] * npts;
  tolerance = limits[1][phoMAX_PARAM_PER_FUNC];
*/
  minSigma = 0.0001 * tolerance;
  ref = npts * minData;
  if (ref < 0.0) minNum = ref - 0.5;
  else           minNum = ref + 0.5;

/*  illum->type.sunshadow = illNoShadow;
  illum->type.mode = illEllCos;
*/

  for (ii=0; ii<npts; ii++) 
  {
   illum = illum_array[ii];

    status = phoBidiRef( pho_obj, &illum, phoFuncVal );
    if(status!=phoSUCCESS) 
		    {
			zvmessage(" ","");
	    		zvmessage("***phoCost error***","");
	    		zvmessage("*** phoFunc failed ***","");
	    		zmabend("photfit abend");
		    }

    er = (*(x[0]+ii) - *phoFuncVal) * (*(x[0]+ii) - *phoFuncVal);

    if ( fabs(*(x[1]+ii) ) < minSigma ) sigma = minSigma;
    else                                sigma = fabs(*(x[1]+ii));
    sig2 = sigma * sigma;

    *error = *error + er/sig2;
    sig = sig + 1.0 / sig2;
    if ( fabs(*(x[0]+ii) - *phoFuncVal) < tolerance ) 
    {
      sum = sum + er / sig2;
      sigSum = sigSum + 1.0 / sig2;
      n++;
    }

/*
    er = fabs( *(x[0]+ii) - *phoFuncVal) ;
    *error = *error + er;

    if ( er < tolerance ) 
    {
      sum = sum + er;
      n++;
    }
*/
  }


  if (n > minNum) *error = sqrt(sum / sigSum) ;
  else            *error = sqrt(*error / sig) ;

/* 	strcpy( msg, " *error = " );
	sprintf( cval1, " %10.6e", *error);
	strcat( msg, cval1);
    	zvmessage( msg, "");
*/

/*  if (n > minNum) *error = sum/(1.0 * n); 
  else            *error = *error / (1.0 * npts); 
 
 	strcpy( msg, " *error = " );
	sprintf( cval1, " %10.6e", *error);
	strcat( msg, cval1);
    	zvmessage( msg, "");
*/
/*  free(pkeylist); */
  free(phoFuncVal);
  return rstatus=1;
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

