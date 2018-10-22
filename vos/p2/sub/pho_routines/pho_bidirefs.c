#include "pho_private.h"	/* definitions of the pho_object and ...    */

#define HFNCT1(a,b)       ((1.0 + 2.0*a)/(1.0 + 2.0*a*b))
#define EPS_HX_DIE       0.00001
#define EPS_HX_DA        0.00001
#define EPS_THETA	 1.7e-3  /* THETA > 0.1 degree  		*/
#define MAX_THETA	 1.4	 /* THETA < 80 degree   		*/
#define MAX_AZIMUTH	 3.10	 /* AZIMUTH < 177.6 degree 		*/


/******************************************************************************
	*****************************************************************
	*								*
	*	Subroutines needed for phoFunction			*
	*								*
	*****************************************************************


 N O T I C E, that the subroutines doesn't contain any check if the angles are
 greater than 90 degrees (if the cos(angle) > 0.0). You have to do it before.
 Only a possible division by zero is prevented.

 N O T I C E the angle unit is rad.

 The arguments are equal for all functions and are not changed.

 Parameter
   x    Input of the observation geometrie
 	N O T I C E the angle unit is rad.
   a    Input of the photometric parameters
   res  result array for the function value and the derivations.
	The dimension has to be: Dim(res) = 4 + Dim(a)
	res[0]   = function value
	res[1]   = d res[0]/ d cos(i)
	res[2]   = d res[0]/ d cos(e)
	res[3]   = d res[0]/ d phas
  	res[4+i] = d res[0]/ d a[ i ]   ( i = 0, 1, ..)

 
*******************************************************************************/

/******************************************************************************

			LAMBERT Function

 ******************************************************************************/

void LAMBERT (
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/**********************************************************
 * 	Dim(arguments)=1 	arguments[0]= ci = cos(i)
 *	Dim(param)=1		params[0]= ALBEDO 
 **********************************************************/

{


  res[0] = a[0] * x[0];		/*  res[0] = albedo * ci           	*/
  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1] = a[0];		/*  res[1] = d res[0]/ d cos(i)   	*/
    res[2] = 0.0;		/*  independent				*/
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = x[0];		/*  res[4] = d res[0]/ d albedo 	*/
    return;
  }

  return;
}

/******************************************************************************

			Minnaert Funktion

 ******************************************************************************/


void MINNAERT ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/****************************************************
 * Dim(arguments)=2  	x[0] = ci = cos(i)
 *			x[1] = ce = cos(e)
 * Dim(params)=2	a[0] = B0 = Albedo
 * 			a[1] = k  = Exponent
 ****************************************************/

{
  double ce, cice, power;
 
  ce = MAX(x[1], ANG_EPS);
  cice = x[0] * ce;
  power = pow(cice, a[1]) / ce;


  res[0]  = a[0] * power; 	/* res = B0*pow(ci*ce,k)/ce; 		*/
  if( EQUAL(phoVALUE,phoMode) ) return;
 
  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1]  = a[1] * res[0]/MAX(x[0], ANG_EPS); 
    res[2]  = (a[1] - 1.0) * res[0]/ ce;
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = power; 		/* res[4] = d res[0]/ d B0		*/
    res[5] = res[0] * log(cice);   /* res[5] = d res[0]/ d k		*/
    return;
  }

  return;
}

/******************************************************************************

			IRVINE Function

 ******************************************************************************/

void IRVINE ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 2    x[0]    = ci    = cos(i)
 *			x[1]    = ce    = cos(e)
 * Dim(params)= 3	a[0..2] = photometric function parameters
 * 			a[0]    = k	= EXPONENT
 * 			a[1]    = B	= IRV_EXP1
 * 			a[2]    = C	= IRV_EXP2
*************************************************************************/

{
  double ce, cice, bz, bn, exn, exz, power;
 
  ce = MAX(x[1], ANG_EPS);
  cice = x[0] * ce;
  exn = exp(-x[1]*a[2]);
  bz = 1.0 - exn;
  bn = MAX(bz, ANG_EPS);
  exz = exp(-x[0]*a[1]);
  bz = 1.0 - exz;
  power = pow(cice, a[0]);

/*   *res = pow( ci*ce, k )/ce * (1-exp(-ci*B))/(1-exp(-ce*C));  */

  res[0] =  power/ce * bz/bn;

  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
    res[1] = power / ce / bn * ( 1.0 - (1.0 - a[1]) * exz);
    res[2] = res[0] * ( (a[0]-2.0)/x[1] - a[2] * exn / bn );  
  }

  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = 0.0;		/*  independent				*/
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = res[0] * log(cice);	/*  res[4] = d res[0]/ d k 	*/
    res[5] = power/ce * a[1] * exz / bn;
    res[6] = - res[0] * a[2] * exn / bn;
    return;
  }

  return;
}

/******************************************************************************

			Veverka Funktion

 ******************************************************************************/

void VEVERKA ( 
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/***********************************************************************
 * Dim(arguments)= 3    x[0] = ci = cos(i)
 *			x[1] = ce = cos(e)
 *			x[2] = cg = cos(phase angle)
 * Dim(params)= 4	a[0..3] = photometric function parameters
 *			  a[0] = A = A-VEVERKA
 *			  a[1] = B = B_VEVERKA
 *			  a[2] = C = C_VEVERKA
 * 			  a[3] = D = D_VEVERKA
 ************************************************************************/

{
   double phase, sum, s2, xe, xf, lom;

  phase = RETURN_DEGREES(acos(x[2]));
  xe = exp(-a[3] * phase);
  xf = a[0] + a[1] * phase + a[2] * xe;

  sum = x[0] + x[1];
  if(sum < ANG_EPS) sum = ANG_EPS;
  s2 = sum * sum;
  lom = x[0]/sum;

  res[0] = lom * xf;
  if( EQUAL(phoVALUE,phoMode) ) return;

  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
  res[1] = xf*x[1]/s2;	/*  res[1] = d res[0]/ d cos(i)   	*/
  res[2] = -res[0]/sum;	/*  res[2] = d res[0]/ d cos(e)   	*/
  }


  if( EQUAL(phoDIR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[3] = lom * ( a[1] - a[2] * a[3] * xe);	/*  res[3] = d res[0]/ d phas */
  }

 if( EQUAL(phoPAR_DEV,phoMode) ||  EQUAL(phoALL,phoMode) ) 
  {
    res[4] = x[0]/sum;			        /*  res[4] = d res[0]/ d A */
    res[5] = res[4] * phase;		        /*  res[5] = d res[0]/ d B */
    res[6] = res[4] * xe;		        /*  res[6] = d res[0]/ d C */
    res[7] = -res[6] * a[2] * phase;	        /*  res[7] = d res[0]/ d D */
    return;
  }

  return;
}

/******************************************************************************

			HAPKE_86_LE2 Funktion

 ******************************************************************************/

void HAPKE_86_LE2 (   
	double *x,
	double *a,
	double *res,
	char phoMode[phoMax_MODE_NAME_LENGTH+1])

/*****************************************************************
 * Dim(arguments)= 3    arguments[0] = ci = cos(i)
 *			arguments[1] = ce = cos(e)
 *			arguments[2] = cg = cos(phase angle)
 * Dim(params)= 6	photometric function parameters
 *			  params[0] = w		= W_SOIL
 *			  params[1] = h		= H_SHOE
 *			  params[2] = B0	= B-SHOE
 *			  params[3] = THETA	= THETA(in grad)
 *			  params[4] = b		= LE1_SOIL
 *			  params[5] = c		= LE2_SOIL
 *****************************************************************/

{
int flag;
double dpg;





  if( EQUAL(phoVALUE,phoMode) ) /* res[0] */
  {
    flag = 0;

   /*	cos(phase angle) = x[2].
	Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)  */

    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);

    return;
  }






  if( EQUAL(phoGRAD,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
                res[2] = d res[0] / d cose  */
    flag = 2;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */
    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);

    return;
  }






  if( EQUAL(phoDIR_DEV,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
		res[2] = d res[0] / d cose,
		res[3] = d res[0] / d Tetha  not yet implemented */

    zmabend(" ****phoDIR_DEV not yet implemented**** ");

    flag = 4;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */
    res[0] = 1.0 + a[4] * x[2] + a[5] * (1.5 * x[2] * x[2] - 0.5);

    pho_HAPKE86(x, a, res, flag);


    return;
  }


  if( !EQUAL(phoPAR_DEV,phoMode) ) 
  {
  /* res[0] and the parameter derivations
	res[4] = d res[0] / d w,
	res[5] = d res[0] / d h,
	res[6] = d res[0] / d Bo,
	res[7] = d res[0] / d Theta  not yet implemented ,
	res[8] = d res[0] / d b,
	res[9] = d res[0] / d c  */

    flag = 1;

    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    dpg = 1.5 * x[2] * x[2] - 0.5;
    res[0] = 1.0 + a[4] * x[2] + a[5] * dpg;


    pho_HAPKE86(x, a, res, flag);


    /*	Derivation dHapke/dc = dHapke/da5 = res[9] */

    res[9] = res[8] * dpg;


    /*	Derivation dHapke/db = dHapke/da4 = res[8] */

    res[8] *= x[2];

  return;


  }


 if( EQUAL(phoALL,phoMode) ) 
  {
  /* res[0] and res[1] = d res[0] / d cosi,
		res[2] = d res[0] / d cose,
		res[3] = d res[0] / d Tetha and the parameter derivations 
		res[4] = d res[0] / d w,
		res[5] = d res[0] / d h,
		res[6] = d res[0] / d Bo,
		res[7] = d res[0] / d Theta,  not yet implemented
		res[8] = d res[0] / d b,
		res[9] = d res[0] / d c  */

    flag = 3;


    /* Legendre Polynom : 1 + b * cos(a) + c * (3*cos(a)*cos(a) - 1)/2)    */

    dpg = 1.5 * x[2] * x[2] - 0.5;
    res[0] = 1.0 + a[4] * x[2] + a[5] * dpg;


    pho_HAPKE86(x, a, res, flag);


    /*	Derivation dHapke/dc = dHapke/da5 = res[8] */

    res[9] = res[8] * dpg;


    /*	Derivation dHapke/db = dHapke/da4 = res[7] */

    res[8] *= x[2];

		
    return;
  }

  return;
}





/******************************************************************************

		common HAPKE 86

  The INPUT parameter flag controls the result calculation:
  flag = 0  res[0] only
  flag = 1  res[0] and the parameter derivations d res[0]/ds[i]
  flag = 2  res[0], res[1] and res[2]
  flag = 3  res[0], res[1], res[2] and the parameter derivations
  flag = 4  res[0], res[1], res[2], res[3] 

  res[0] has to contain the value of the single scattering function. 

 ******************************************************************************/

void pho_HAPKE86 (
	double *x,
	double *a, 
	double *res,
	int flag)
{
  double xx, tg, sqw, pg, Theta;
  double cosi, cose, cosg, sfnct, di[3], de[3], dg[3], da[3], bg, dbgdh, dbgdb;
  double hi, dih, dhida, he, deh, dheda, cof, dac, dic, dec, rbr, dir, der;

  cosi = x[0];
  cose = x[1];
  cosg = x[2];
  pg = res[0];
  Theta = RETURN_RADIANS(a[3]);

  if(a[0] > 1.0) a[0] = 1.0;		/* a[0] = w  */
  sqw  = sqrt(1.0 - a[0]);

  if(flag & 3) sfnct = pho_dg_rough(&cosi, &cose, cosg, Theta, di, de, dg, da, flag);
  else         sfnct = pho_hp_rough(&cosi, &cose, cosg, Theta);

/*	Determine the backscattering function */
  if(a[1] == 0.0)  bg = dbgdh = dbgdb = 0.0;
  else {
/*	General is tan(x/2) = sqrt[(1-cos(x))/(1+cos(x))] for 0 <= x < Pi. */
    xx = MAX(cosg, -0.99999999);
    tg = sqrt((1.0-xx)/(1.0+xx)); 
    if(flag & 1) {
      xx = a[1] + tg;
      dbgdh = a[2] * tg / (xx * xx);
      dbgdb = 1.0/(1.0 + tg/a[1]);
      bg = a[2] * dbgdb;
    }
    else {
      tg /= a[1];
      bg = a[2]/(1.0 + tg);
    }
  }

  xx = cosi + cose;
  if(xx < ANG_EPS) xx = ANG_EPS;

/*	COEF_FAC = 1.0/(4 * M_PI) */

  tg = COEF_FAC / xx;
  dac = cosi * tg;
  cof = a[0] * dac;

  if(flag & 3) {
    tg *= a[0] / xx;
    dic = cose * tg;
    dec = -cosi * tg;

    hi = pho_d_h_fct (cosi, sqw, &dih, &dhida);
    he = pho_d_h_fct (cose, sqw, &deh, &dheda);

    rbr = (1.0 + bg) * pg + hi * he - 1.0;
    dir = (dic * rbr + cof * dih * he) * sfnct;
    der = (dec * rbr + cof * hi * deh) * sfnct;
  }  
  else
    rbr = (1.0 + bg) * pg + HFNCT1(cosi, sqw) * HFNCT1(cose, sqw) - 1.0;

/*	The function value */
  res[0] = cof * rbr * sfnct * M_PI;

/*	Derivation dHapke/dcosi and dHapke/dcose */
  if(flag & 2) {
    res[1] = (dir * di[1] + der * di[2] + cof * rbr * di[0]) * M_PI;
    res[2] = (dir * de[1] + der * de[2] + cof * rbr * de[0]) * M_PI;
  }

  if(flag & 1) {
/*	Derivation dHapke/dw = dHapke/da0 = res[4] */
    res[4] = (dac * rbr + cof * (dhida * he + hi * dheda)) * sfnct * M_PI;

/*	Derivation dHapke/dh = dHapke/da1 = res[5] */
    xx = cof * sfnct * pg;
    res[5] = xx * dbgdh * M_PI;

/*	Derivation dHapke/dBo = dHapke/da2 = res[6] */
    res[6] = xx * dbgdb * M_PI;

/*	Derivation dHapke/dtheta = dHapke/da3 = res[7] */
    res[7] = (dir * da[1] + der * da[2] + cof * rbr * da[0]) * M_PI;

/*	Single scattering parameter derivation factor =  = res[8]. */
    res[8] = cof * (1.0 + bg) * sfnct * M_PI;
  }

  return;
}



/******************************************************************************

			pho_hp_rough Subroutine

	This subroutine is called by the HAPKE functions to consider 
	the macroscopic roughness

  *cosi   is a pointer for cos(i)
  *cose   is a pointer for cos(e)
   cosg   is the cos(phase angle)
   tbar   is THETA in rad
 ******************************************************************************/

double pho_hp_rough ( 
	double *cosi,
	double *cose, 
	double cosg, 
	double tbar)
{
  double sini, sine, csphi, psi, s2psi, f, y;
  double ci, ce, c0i, c0e, ei1, ei2, ee1, ee2;
  double tant, beta, cof, sfnct;

  if(*cosi > 1.0) *cosi = 1.0;
  if(*cose > 1.0) *cose = 1.0;

  if(tbar < 0.0) tbar *= -1.0;
  if(tbar > EPS_THETA) {
/*   tbar > 0.1 degree, so rough-surface changes cosi and cose   */

    sini = sqrt(1.0 - *cosi * *cosi);
    sine = sqrt(1.0 - *cose * *cose);

    if(sini != 0.0 && sine != 0.0)
      csphi = (cosg - *cosi * *cose)/(sini * sine);
    else
      csphi = 1.0;

    if(csphi < -1.0) csphi = -1.0;
    if(csphi >  1.0) csphi =  1.0;

    s2psi = 0.5 * (1.0 - csphi);   /*  = sin(0.5*phi)*sin(0.5*phi);  */
    psi = acos(csphi);             /*  0 <= psi <= Pi                */

    if(psi < MAX_AZIMUTH)  f = exp(-2.0 * tan(0.5 * psi));
    else            	    f = 0.0;

    if(tbar > MAX_THETA) tbar = MAX_THETA;

    tant = tan(tbar);
    beta = sqrt(1.0 + M_PI * tant * tant);

    y = sini * tant;
    if(y > 1.0e-2) {
      ci = *cosi/y;
      ei1 = exp(-2.0 * ci/M_PI);
      ei2 = exp(-ci * ci/M_PI);
    }
    else  ei1 = ei2 = 0.0;

    y = sine * tant;
    if(y > 1.0e-2) {
      ce = *cose/y;
      ee1 = exp(-2.0 * ce/M_PI);
      ee2 = exp(-ce * ce/M_PI);
    }
    else  ee1 = ee2 = 0.0;

    ci = *cosi;
    ce = *cose;
    c0i = ci + sini * tant * ei2/(2.0 - ei1);
    c0i /= beta;
    c0e = ce + sine * tant * ee2/(2.0 - ee1);
    c0e /= beta;

    if(*cosi >= *cose) {                       /*   case    i <= e  */
      y = 2.0 - ee1 - (psi/M_PI) * ei1;
      *cosi += sini * tant * (csphi * ee2 + s2psi * ei2)/y;
      *cosi /= beta;
      *cose += sine * tant * (ee2 - s2psi * ei2)/y;
      *cose /= beta;
      y = ci/c0i;
    }
    else {                                     /*   case    i > e   */
      y = 2.0 - ei1 - (psi/M_PI) * ee1;
      *cosi += sini * tant * (ei2 - s2psi * ee2)/y;
      *cosi /= beta;
      *cose += sine * tant * (csphi * ei2 + s2psi * ee2)/y;
      *cose /= beta;
      y = ce/c0e;
    }

    cof = beta * (1.0 - f + f * y/beta);
    sfnct  = (*cose/c0e) * (ci/c0i)/cof;

  }
  else sfnct = 1.0;

  return(sfnct);
}


/******************************************************************************

		derivations of the Roughness Function

  *ci   is a pointer for cos(i)
  *ce   is a pointer for cos(e)
   cg   is the cos(phase angle)
   tbar is THETA in rad
  di[0] expects the derivation d sfnct / d cos(i)
  di[1] expects the derivation d cos(i`) / d cos(i)
  di[2] expects the derivation d cos(e`) / d cos(i)
  de[0] expects the derivation d sfnct / d cos(e)
  de[1] expects the derivation d cos(i`) / d cos(e)
  de[2] expects the derivation d cos(e`) / d cos(e)
  da[0] expects the derivation d sfnct / d tbar
  da[1] expects the derivation d cos(i`) / d tbar
  da[2] expects the derivation d cos(e`) / d tbar
  dg[0] expects the derivation d sfnct /d phase		not yet implemented
  dg[1] expects the derivation d cos(i`) / d phase	not yet implemented
  dg[2] expects the derivation d cos(e`) / d phase	not yet implemented
  flag = 1  di and de are calculated
  flag = 2  da is calculated
  flag = 3  di, de and da are calculated
  flag = 4  dg not yet implemented
 ******************************************************************************/

double pho_dg_rough (
	double *ci,
	double *ce, 
	double cg, 
	double tbar, 
	double *di, 
	double *de, 
	double *dg,
	double *da,
	int flag)
{
  double sfnct, h1ci, h1ce, h2ci, h2ce, h3ci, h3ce, hx;

  h1ci = h2ci = h3ci = *ci;
  h1ce = h2ce = h3ce = *ce;
  sfnct = pho_hp_rough (ci, ce, cg, tbar);

  if(flag & 2) {
    hx = EPS_HX_DIE;
    if(h3ci < hx) hx *= -1;
    h1ci -= hx;
    di[0] = (sfnct - pho_hp_rough(&h1ci, &h1ce, cg, tbar))/hx;
    di[1] = (*ci - h1ci)/hx;
    di[2] = (*ce - h1ce)/hx;

    hx = EPS_HX_DIE;
    if(h3ce < hx) hx *= -1;
    h2ce -= hx;
    de[0] = (sfnct - pho_hp_rough (&h2ci, &h2ce, cg, tbar))/hx;
    de[1] = (*ci - h2ci)/hx;
    de[2] = (*ce - h2ce)/hx;
  }

  if(flag & 1) {
    hx = EPS_HX_DA;
    tbar += hx;
    da[0] = (pho_hp_rough (&h3ci, &h3ce, cg, tbar) - sfnct)/hx;
    da[1] = (h3ci - *ci)/hx;
    da[2] = (h3ce - *ce)/hx;
  }

  return(sfnct);
}

/******************************************************************************

    	pho_d_h_fct	derivation of the h function

  a       cos of the considered angle
  sqw     sqrt(1 - w)
  *da     expects the derivation d h_fct/da
  *dw     expects the derivation d w_fct/dw
 ******************************************************************************/

double pho_d_h_fct (
	double a, 
	double sqw, 
	double *da, 
	double *dw)
{
  double xx, tg, hh;

  xx = 1.0 + 2.0 * a * sqw;
  tg = sqw * xx;
  hh = (1 + 2.0 * a) / xx;
  dw[0] = a * hh / tg;
  da[0] = 2.0 * (1.0 - sqw)/(xx * xx);

  return(hh);
}

