#include <math.h>
#include "vicmain_c"
#include "pho.h"

/* Program TPHOPDF  */

void main44()
{
  int cnt, def, i, ival, ival1, num, illMode, status;
  float temp;
  double dval, dval1, IncAng, EmAng, PhasAng, phoFuncVal;
  char subcmd[9], cval[133], cval1[133], msg[133],
  keylist[phoMAX_PARAM_PER_FUNC][phoMAX_KEYWD_LENGTH+1];
  char *pkeylist;
  PHO pho_obj;
  PHO_ILLUM Millum, Tillum;

  zvmessage(" program TPHOPDF", "");
  zvmessage( " ", "");

  zveaction("","");

  status = phoInit( &pho_obj);

/* get the photometric function and there input parameters from the PDF     */
/* and set these in the photometric object :				    */

  status = phoGetParms( pho_obj);
  if(status != phoSUCCESS) return;	
  
/* get the number of parameters of the current photometric function : 	    */

  status = phoGetKeys( pho_obj, 0, &num); 
  strcpy( msg, " parameter number = " );
  sprintf( cval1, " %i", num);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the list of parameter keywords for the current photometric function : */

  pkeylist = (char *)malloc( phoMAX_PARAM_PER_FUNC * ( phoMAX_KEYWD_LENGTH+1 ) * sizeof(char));
  pkeylist = (char *)keylist;

  status = phoGetKeys( pho_obj, pkeylist, &num);

/* get the photometric function name : */

  status = phoGetFunc( pho_obj, cval1);
  strcpy( msg, " Function =" );
  strcat( msg, cval1);
  zvmessage( msg, "");
  zvmessage( " ", "");

  for (i=0; i<num; i++) {

    status = phoGetVal( pho_obj, keylist[i], &dval1);
    strcpy( msg, keylist[i]);
    strcat( msg, " = ");
    sprintf( cval1, " %10.3e", dval1);
    strcat( msg, cval1);
    zvmessage( msg, "");
  }

/* reads in the function arguments from PDF and fill the illumination union: */

/* fill the illumination union for the meassured illumination conditions :   */

  zvmessage( " ", "");

  zvp("INC_ANG", &temp, &cnt);
  strcpy( msg, "Incidence Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.inc = cos(RETURN_RADIANS((double )temp));

  zvp("EM_ANG", &temp, &cnt);
  strcpy( msg, "Emission Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.em = cos(RETURN_RADIANS((double )temp));

  zvp("PHAS_ANG", &temp, &cnt);
  strcpy( msg, "Phase Angle =  " );
  sprintf( cval1, " %10.3e", temp);
  strcat( msg, cval1);
  zvmessage( msg, "");

  Millum.cos.phas = cos(RETURN_RADIANS((double )temp));

  zvmessage( " ", "");

  Millum.mode = illEllCos;
  Millum.type.sunshadow = illNoShadow;
  Millum.type.viewshadow = illNoShadow;

/* fill the illumination union for the target illumination conditions : */

  Tillum.mode = illEllCos;
  Tillum.type.sunshadow = illNoShadow;
  Tillum.type.viewshadow = illNoShadow;

  Tillum.cos.inc  = 1.0;
  Tillum.cos.em = 1.0;
  Tillum.cos.phas = 1.0;



/* get the Bidirectional Reflectance Value : */

  status = phoBidiRef( pho_obj, &Millum, &phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (meassured illumin.) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoBidiRef( pho_obj, &Tillum, &phoFuncVal );

  strcpy( msg, "Bidirectional Reflectance Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the photometric function value : */

  status = phoFunc( pho_obj, &Millum, &phoFuncVal );

  strcpy( msg, "Photometric Function Value (meassuered illumin.) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  status = phoFunc( pho_obj, &Tillum, &phoFuncVal );

  strcpy( msg, "Photometric Function Value (target illumin.=nadir/nadir) =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

/* get the correction value to nadir viewing and illumination conditions : */

  status = phoCorrect( pho_obj, &Millum, &Tillum, &phoFuncVal );

  strcpy( msg, " to-nadir Correction Value =");
  sprintf( cval1, " %10.3e", phoFuncVal);
  strcat( msg, cval1);
  zvmessage( msg, "");

  zvmessage( " ", "");


  status = phoFree( pho_obj);
  return;
}
