/*******************************************************************************

  Title:     eos_coords
  Author:    Mike Burl 
  Date:      2006/12/14
  Function:  This function finds rotation matrix from True-of-Date coordinate system (which
               includes both precession and nutation) to (a) the J2000 coordinate system
               and (b) the ECEF coordinate system.

  History:   2007/05/24 (MCB) - Changed API so we now take:
              (1) an elapsed TT  (in seconds) since the instant 2000-01-01T12:00:00 TT and 
              (2) an elpased UT1 (in seconds) since the instant 2000-01-01T12:00:00 UT1.


             2007/03/09 (MCB) - Left nutation and precession calculations in TDB pending
               clarification from contractor. Fixed comments regarding input.

             2007/03/05 (MCB) - Modified to incorporate higher-order nutation model
               from 3rd revision of "Attitude Control Coordinate Frames".

             2007/02/26 (MCB) - Modified to take barycentric dynamical time and ut1 time,
               both expressed in seconds since the J2000.0 epoch, as arguments rather than
               a UTC_string and Delta_UT1 value. This means that the caller of eos_coords()
               may need to first call utc_to_tdb_and_ut1() or 
               utc_iso_time_string_to_tdb_and_ut1(), which have been added to
               time_conversion.c.

             2006/12/27 (MCB) - Revised to incorporate note 1a below, plus fixed two errors
               found in the 2006/07/03 memo (units on nutation coefficients and units on 
               Theta_GMST).

             2006/12/14 (MCB) - Based on eos_coords.m

  INPUT:
  -----
  TT          - elapsed time (in seconds) since 2000-01-01T12:00:00 TT (the J2000.0 epoch).
  UT1         - elpased Universal time (in seconds) since 2000-01-01T12:00:00 UT1.

  OUTPUT: 
  ------
  J2000_R_TOD - (3 X 3) rotation matrix to transform position vectors from True-of-date (TOD) 
                   coordinate frame to J2000 coordinate frame.
  ECEF_R_TOD  - (3 X 3) rotation matrix to transform position vectors from True-of-date (TOD)
                  coordinate frame to ECEF.


  NOTES:
  -----
  1a. Current version is based on rev 3 of a memo (see also notes 1b and 1c) that is
      titled "Attitude Control Coordinate Frames".
  1b. Based on rev 2 of a memo titled "Coordinate Systems for Earth Observing Spacecraft", 
        2006/07/03, which in turn is based on D. A. Valladio, "Fundamentals of Astrodynamics 
        and Applications", El Segundo, CA, Mircocosm Press, (2001).
  1c. An earlier version of this function was based on rev1 of the memo in Note 1b,
        dated 2005/01/14.

  2.  See also time_conversion.c

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "qmalloc.h"
#include "time_conversion.h"
#include "eos_coords.h"
#include "mat33.h"

#define N_NUTATION_TERMS 106

/***********************/
/* EOS_COORDS          */
/***********************/
int eos_coords(double TT, double UT1, double *J2000_R_TOD, double *ECEF_R_TOD)
{
  double               T_UT1, T_UT1_POWERS[5];
  double               T_TT, T_TT_POWERS[5];
  int                  i, j;

  double               zeta_arcsec, zeta_rad;
  double               Theta_arcsec, Theta_rad;
  double               z_arcsec, z_rad;

  double               F1_deg, F2_deg, F3_deg, F4_deg, F5_deg;
  double               msvec[5];

  double               M1[9], M2[9], M3[9], M4[9], MOD_R_J2000[9];
  double               G1[9], G2[9], G3[9], G4[9], TOD_R_MOD[9], TOD_R_J2000[9];
  double               A_deg[N_NUTATION_TERMS], A_rad[N_NUTATION_TERMS];
  double               alpha;
  double               Delta_psi_uarcsec, Delta_psi_arcsec, Delta_psi_rad;
  double               Delta_epsilon_uarcsec, Delta_epsilon_arcsec;
  double               epsilon_prime_deg, epsilon_prime_rad;
  double               epsilon_deg, epsilon_rad;

  double               alpha_GMST_sec;
  double               EQ_sec, EQ_arcsec;
  double               alpha_GAST_deg;
  double               alpha_GAST_rad;
  double               alpha_GAST_deg_standard;

  /* Coefficients for various calculations */
  static double        C_zeta_arcsec[5]  = { D_ZERO, 2306.2181, 0.30188, 0.017998, D_ZERO }; /* MCB: fixed 2 errors in memo-rev3 */
  static double        C_Theta_arcsec[5] = { D_ZERO, 2004.3109, -0.42665, -0.041833, D_ZERO };
  static double        C_z_arcsec[5]     = {D_ZERO, 2306.2181, 1.09468, 0.018203, D_ZERO }; /* MCB: Fixed 2 errors in memo-rev3 */

  static double        C_F1[5] = { 134.9629814, 477198.8674, 0.008697, 1.7778e-05, D_ZERO };
  static double        C_F2[5] = { 357.5277233, 35999.05034, -0.000160, -3.3e-06, D_ZERO };
  static double        C_F3[5] = {  93.27191028, 483202.0175, -0.0036825, 3.0556e-06, D_ZERO };
  static double        C_F4[5] = { 297.8503631, 445267.1115, -0.001914, 5.2778e-06, D_ZERO };
  static double        C_F5[5] = { 125.0445222, -1934.136261, 0.002071, 2.22e-06, D_ZERO };

  static double        C_epsilon_deg[5] = { 23.43929111, -0.0130042, -1.6389e-07, 5.0361e-07, D_ZERO };
  static double        C_alpha_GMST[5] = { 67310.54841, (876600.0 * 3600.0 + 8640184.812866), 0.093104, -6.2e-06, D_ZERO };

  static double        NUT[N_NUTATION_TERMS][9] = {
                         { 0,  0,  0,  0,  1, -171996, -174.2, 92025,  8.9}, /* 1-10 */
                         { 0,  0,  0,  0,  2,    2062,    0.2,  -895,  0.5},
			 {-2,  0,  2,  0,  1,      46,    0.0,   -24,  0.0},
			 { 2,  0, -2,  0,  0,      11,    0.0,     0,  0.0},
			 {-2,  0,  2,  0,  2,      -3,    0.0,     1,  0.0},
			 { 1, -1,  0, -1,  0,      -3,    0.0,     0,  0.0},
			 { 0, -2,  2, -2,  1,      -2,    0.0,     1,  0.0},
			 { 2,  0, -2,  0,  1,       1,    0.0,     0,  0.0},
                         { 0,  0,  2, -2,  2,  -13187,   -1.6,  5736, -3.1},
                         { 0,  1,  0,  0,  0,    1426,   -3.4,    54, -0.1},

			 { 0,  1,  2, -2,  2,    -517,    1.2,   224, -0.6}, /* 11-20 */
			 { 0, -1,  2, -2,  2,     217,   -0.5,   -95,  0.3},
			 { 0,  0,  2, -2,  1,     129,    0.1,   -70,  0.0},
			 { 2,  0,  0, -2,  0,      48,    0.0,     1,  0.0},
			 { 0,  0,  2, -2,  0,     -22,    0.0,     0,  0.0},
			 { 0,  2,  0,  0,  0,      17,   -0.1,     0,  0.0},
			 { 0,  1,  0,  0,  1,     -15,    0.0,     9,  0.0},
			 { 0,  2,  2, -2,  2,     -16,    0.1,     7,  0.0},
			 { 0, -1,  0,  0,  1,     -12,    0.0,     6,  0.0},
			 {-2,  0,  0,  2,  1,      -6,    0.0,     3,  0.0},

			 { 0, -1,  2, -2,  1,      -5,    0.0,     3,  0.0}, /* 21-30 */
			 { 2,  0,  0, -2,  1,       4,    0.0,    -2,  0.0},
			 { 0,  1,  2, -2,  1,       4,    0.0,    -2,  0.0},
			 { 1,  0,  0, -1,  0,      -4,    0.0,     0,  0.0},
			 { 2,  1,  0, -2,  0,       1,    0.0,     0,  0.0},
			 { 0,  0, -2,  2,  1,       1,    0.0,     0,  0.0},
			 { 0,  1, -2,  2,  0,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  0,  2,       1,    0.0,     0,  0.0},
			 {-1,  0,  0,  1,  1,       1,    0.0,     0,  0.0},
			 { 0,  1,  2, -2,  0,      -1,    0.0,     0,  0.0},

                         { 0,  0,  2,  0,  2,   -2274,   -0.2,   977, -0.5}, /* 31-40 */
                         { 1,  0,  0,  0,  0,     712,    0.1,    -7,  0.0},
			 { 0,  0,  2,  0,  1,    -386,   -0.4,   200,  0.0},
			 { 1,  0,  2,  0,  2,    -301,    0.0,   129, -0.1},
			 { 1,  0,  0, -2,  0,    -158,    0.0,    -1,  0.0},
			 {-1,  0,  2,  0,  2,     123,    0.0,   -53,  0.0},
			 { 0,  0,  0,  2,  0,      63,    0.0,    -2,  0.0},
			 { 1,  0,  0,  0,  1,      63,    0.1,   -33,  0.0},
			 {-1,  0,  0,  0,  1,     -58,   -0.1,    32,  0.0},
			 {-1,  0,  2,  2,  2,     -59,    0.0,    26,  0.0},

			 { 1,  0,  2,  0,  1,     -51,    0.0,    27,  0.0}, /* 41-50 */
			 { 0,  0,  2,  2,  2,     -38,    0.0,    16,  0.0},
			 { 2,  0,  0,  0,  0,      29,    0.0,    -1,  0.0},
			 { 1,  0,  2, -2,  2,      29,    0.0,   -12,  0.0},
			 { 2,  0,  2,  0,  2,     -31,    0.0,    13,  0.0},
			 { 0,  0,  2,  0,  0,      26,    0.0,    -1,  0.0},
			 {-1,  0,  2,  0,  1,      21,    0.0,   -10,  0.0},
			 {-1,  0,  0,  2,  1,      16,    0.0,    -8,  0.0},
			 { 1,  0,  0, -2,  1,     -13,    0.0,     7,  0.0},
			 {-1,  0,  2,  2,  1,     -10,    0.0,     5,  0.0},

			 { 1,  1,  0, -2,  0,      -7,    0.0,     0,  0.0}, /* 51-60 */
			 { 0,  1,  2,  0,  2,       7,    0.0,    -3,  0.0},
			 { 0, -1,  2,  0,  2,      -7,    0.0,     3,  0.0},
			 { 1,  0,  2,  2,  2,      -8,    0.0,     3,  0.0},
			 { 1,  0,  0,  2,  0,       6,    0.0,     0,  0.0},
			 { 2,  0,  2, -2,  2,       6,    0.0,    -3,  0.0},
			 { 0,  0,  0,  2,  1,      -6,    0.0,     3,  0.0},
			 { 0,  0,  2,  2,  1,      -7,    0.0,     3,  0.0},
			 { 1,  0,  2, -2,  1,       6,    0.0,    -3,  0.0},
			 { 0,  0,  0, -2,  1,      -5,    0.0,     3,  0.0},

			 { 1, -1,  0,  0,  0,       5,    0.0,     0,  0.0}, /* 61-70 */
			 { 2,  0,  2,  0,  1,      -5,    0.0,     3,  0.0},
			 { 0,  1,  0, -2,  0,      -4,    0.0,     0,  0.0},
			 { 1,  0, -2,  0,  0,       4,    0.0,     0,  0.0},
			 { 0,  0,  0,  1,  0,      -4,    0.0,     0,  0.0},
			 { 1,  1,  0,  0,  0,      -3,    0.0,     0,  0.0},
			 { 1,  0,  2,  0,  0,       3,    0.0,     0,  0.0},
			 { 1, -1,  2,  0,  2,      -3,    0.0,     1,  0.0},
			 {-1, -1,  2,  2,  2,      -3,    0.0,     1,  0.0},
			 {-2,  0,  0,  0,  1,      -2,    0.0,     1,  0.0},

			 { 3,  0,  2,  0,  2,      -3,    0.0,     1,  0.0}, /* 71-80 */
			 { 0, -1,  2,  2,  2,      -3,    0.0,     1,  0.0},
			 { 1,  1,  2,  0,  2,       2,    0.0,    -1,  0.0},
			 {-1,  0,  2, -2,  1,      -2,    0.0,     1,  0.0},
			 { 2,  0,  0,  0,  1,       2,    0.0,    -1,  0.0},
			 { 1,  0,  0,  0,  2,      -2,    0.0,     1,  0.0},
			 { 3,  0,  0,  0,  0,       2,    0.0,     0,  0.0},
			 { 0,  0,  2,  1,  2,       2,    0.0,    -1,  0.0},
			 {-1,  0,  0,  0,  2,       1,    0.0,    -1,  0.0},
			 { 1,  0,  0, -4,  0,      -1,    0.0,     0,  0.0},

			 {-2,  0,  2,  2,  2,       1,    0.0,    -1,  0.0}, /* 81-90 */
			 {-1,  0,  2,  4,  2,      -2,    0.0,     1,  0.0},
			 { 2,  0,  0, -4,  0,      -1,    0.0,     0,  0.0},
			 { 1,  1,  2, -2,  2,       1,    0.0,    -1,  0.0},
			 { 1,  0,  2,  2,  1,      -1,    0.0,     1,  0.0},
			 {-2,  0,  2,  4,  2,      -1,    0.0,     1,  0.0},
			 {-1,  0,  4,  0,  2,       1,    0.0,     0,  0.0},
			 { 1, -1,  0, -2,  0,       1,    0.0,     0,  0.0},
			 { 2,  0,  2, -2,  1,       1,    0.0,    -1,  0.0},
			 { 2,  0,  2,  2,  2,      -1,    0.0,     0,  0.0},

			 { 1,  0,  0,  2,  1,      -1,    0.0,     0,  0.0}, /* 91-100 */
			 { 0,  0,  4, -2,  2,       1,    0.0,     0,  0.0},
			 { 3,  0,  2, -2,  2,       1,    0.0,     0,  0.0},
			 { 1,  0,  2, -2,  0,      -1,    0.0,     0,  0.0},
			 { 0,  1,  2,  0,  1,       1,    0.0,     0,  0.0},
			 {-1, -1,  0,  2,  1,       1,    0.0,     0,  0.0},
			 { 0,  0, -2,  0,  1,      -1,    0.0,     0,  0.0},
			 { 0,  0,  2, -1,  2,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  2,  0,      -1,    0.0,     0,  0.0},
			 { 1,  0, -2, -2,  0,      -1,    0.0,     0,  0.0},

			 { 0, -1,  2,  0,  1,      -1,    0.0,     0,  0.0},/* 101-106 */
			 { 1,  1,  0, -2,  1,      -1,    0.0,     0,  0.0},
			 { 1,  0, -2,  2,  0,      -1,    0.0,     0,  0.0},
			 { 2,  0,  0,  2,  0,       1,    0.0,     0,  0.0},
			 { 0,  0,  2,  4,  2,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  1,  0,       1,    0.0,     0,  0.0}}; /* MCB: Fixed probable error in memo-rev3 */

  /* char                 infunc[] = "eos_coords"; */

  /*=====================*/
  /* Get powers of T_TT  */
  /*=====================*/
  T_TT= TT/ (86400.0 * 36525.0); /* T_TT is TT expressed in Julian centuries */
  T_TT_POWERS[0] = D_ONE;
  for (i = 1; i <= 4; i++) {
    T_TT_POWERS[i] = T_TT * T_TT_POWERS[i-1];
  }

  /*======================*/
  /* Get powers of T_UT1  */
  /*======================*/
  T_UT1 = UT1/(86400.0*36525.0); /* T_UT1 is UT1 expressed in Julian centuries */
  T_UT1_POWERS[0] = D_ONE;
  for (i = 1; i <= 4; i++) {
    T_UT1_POWERS[i] = T_UT1 * T_UT1_POWERS[i-1];
  }
  /*===========================*/
  /* Precession Transformation */
  /*===========================*/
  zeta_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    zeta_arcsec += C_zeta_arcsec[i] * T_TT_POWERS[i];
  }
  zeta_rad = zeta_arcsec * ARCSEC2RAD;

  Theta_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    Theta_arcsec += C_Theta_arcsec[i] * T_TT_POWERS[i];
  }
  Theta_rad = Theta_arcsec * ARCSEC2RAD;

  z_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    z_arcsec += C_z_arcsec[i] * T_TT_POWERS[i];
  }
  z_rad = z_arcsec * ARCSEC2RAD;

  mat33_assign(M1, cos(z_rad), -sin(z_rad), D_ZERO, sin(z_rad), cos(z_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);    
  mat33_assign(M2, cos(Theta_rad), D_ZERO, -sin(Theta_rad), D_ZERO, D_ONE, D_ZERO, sin(Theta_rad), D_ZERO, cos(Theta_rad));
  mat33_assign(M3, cos(zeta_rad), -sin(zeta_rad), D_ZERO, sin(zeta_rad), cos(zeta_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);    
  mat33_mat33_mult(M2, M3, M4);
  mat33_mat33_mult(M1, M4, MOD_R_J2000);


  /*========================*/
  /* Sun and Moon Variables */
  /*========================*/
  /* F1 = l = Mean Anomaly of the Moon */
  F1_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F1_deg += C_F1[i] * T_TT_POWERS[i];
  }

  /* F2 = l' = Mean anomaly of the Sun */
  F2_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F2_deg += C_F2[i] * T_TT_POWERS[i];
  }
	
  /* F3 = F = Mean longitude of the Moon minus mean longitude of moons node */
  F3_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F3_deg += C_F3[i] * T_TT_POWERS[i];
  }

  /* F4 = D = Mean elongation of the Moon from the Sun */
  F4_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F4_deg += C_F4[i] * T_TT_POWERS[i];
  }

  /* F5 = Omega = Longitutde of the mean ascending node of the Moon */
  F5_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F5_deg += C_F5[i] * T_TT_POWERS[i];
  }
	     
  msvec[0] = F1_deg;
  msvec[1] = F2_deg;
  msvec[2] = F3_deg;
  msvec[3] = F4_deg;
  msvec[4] = F5_deg;

  /*=========================*/
  /* Nutation Transformation */
  /*=========================*/

  /* Calculate the mean obliquity of date */
  epsilon_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    epsilon_deg += C_epsilon_deg[i] * T_TT_POWERS[i];
  }
  epsilon_rad = epsilon_deg * DEG2RAD;

  /* Calculate Ai terms */
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    A_deg[i] = D_ZERO;
    for (j = 0; j < 5; j++) {
      A_deg[i] += NUT[i][j] * msvec[j];
    }
    A_rad[i] = A_deg[i] * DEG2RAD;
  }

  /* Calculate Delta_psi */
  /*   Coefficients in nutation table are in tenths of milliarcseconds, */
  /*   so need to multiply by 100 to get uarcsec. */
  Delta_psi_uarcsec = D_ZERO;
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    alpha = (NUT[i][5] + NUT[i][6]*T_TT) * sin(A_rad[i])*100;
    Delta_psi_uarcsec += alpha;
  }
  Delta_psi_arcsec = Delta_psi_uarcsec * 1e-06;
  Delta_psi_rad = Delta_psi_arcsec/((double) 3600.0) * DEG2RAD;

  /* Calculate Delta_epsilon */
  /*   Coefficients in nutation table are in tenths of milliarcseconds, */
  /*   so need to multiply by 100 to get uarcsec. */
  Delta_epsilon_uarcsec = D_ZERO;
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    alpha = (NUT[i][7] + NUT[i][8]*T_TT) * cos(A_rad[i])*100;
    Delta_epsilon_uarcsec += alpha;
  }
  Delta_epsilon_arcsec = Delta_epsilon_uarcsec * 1e-06;


  /* Calculate epsilon_prime */
  epsilon_prime_deg = epsilon_deg + Delta_epsilon_arcsec/((double) 3600.0);
  epsilon_prime_rad = epsilon_prime_deg * DEG2RAD;

  /* Compose the basic rotations */
  /* The memo dated 2007/04/02 has a bug in the description of nutation calculation */
  mat33_assign(G1, D_ONE, D_ZERO, D_ZERO, D_ZERO, cos(epsilon_prime_rad), -sin(epsilon_prime_rad), D_ZERO, sin(epsilon_prime_rad), cos(epsilon_prime_rad));
  mat33_assign(G2, cos(Delta_psi_rad), -sin(Delta_psi_rad), D_ZERO, sin(Delta_psi_rad), cos(Delta_psi_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);
  mat33_assign(G3, D_ONE, D_ZERO, D_ZERO, D_ZERO, cos(epsilon_rad), sin(epsilon_rad), D_ZERO, -sin(epsilon_rad), cos(epsilon_rad));
  mat33_mat33_mult(G2, G3, G4);
  mat33_mat33_mult(G1, G4, TOD_R_MOD);

  /* Transforms between TOD and J2000 */
  mat33_mat33_mult(TOD_R_MOD, MOD_R_J2000, TOD_R_J2000);
  mat33_inverse(TOD_R_J2000, J2000_R_TOD);

  /*=====================*/
  /* Spin Transformation */
  /*=====================*/
  /* NOTE: alpha_GMST_sec is in time seconds not arcseconds! */
  alpha_GMST_sec = D_ZERO;
  for (i = 0; i < 5; i++) {
    alpha_GMST_sec += C_alpha_GMST[i] * T_UT1_POWERS[i];
  }

  /* Commented part of equation includes the moon correction terms, which are apparently omitted */
  /*   in partner's calculations. */
  EQ_arcsec = Delta_psi_arcsec * cos(epsilon_prime_rad); /* + 0.00264*sin(F1_deg * DEG2RAD) + 0.000063*sin(2 * F1_deg * DEG2RAD); */
  EQ_sec = EQ_arcsec/((double) 15.0); /* 15 degrees per 3600 seconds is 15 arcsec per second */

  alpha_GAST_deg = (alpha_GMST_sec + EQ_sec) * ((double) 15.0)/((double) 3600.0); /* 15 degrees per 3600 time seconds */
  alpha_GAST_rad = alpha_GAST_deg * DEG2RAD;

  /* for debugging only */
  alpha_GAST_deg_standard = alpha_GAST_deg - 360 * floor(alpha_GAST_deg / ((double) 360.0));
  /* printf("alpha_GAST_deg_standard = %.15f\n",
     alpha_GAST_deg_standard); */

  /* Transformations between ECEF and TOD */
  mat33_assign(ECEF_R_TOD, cos(alpha_GAST_rad), sin(alpha_GAST_rad), D_ZERO, -sin(alpha_GAST_rad), cos(alpha_GAST_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);
  /* mat33_inverse(ECEF_R_TOD, TOD_R_ECEF); */

  return(OK);
}
