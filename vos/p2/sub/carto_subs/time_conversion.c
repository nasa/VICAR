/*******************************************************************************

  Title:     time_conversion
  Author:    Mike Burl 
  Date:      2006/12/27
  Function:  This file contains a number of functions for converting between 
               various time systems such as UTC, UT1, TT, and TDB. It is important
               to distinguish between "absolute times" and elpased time since
               some event.

  History:   2008/07/09 (MCB) - Added functionality to use the JD field in the
               USNO leap second file to offset a leap second so that it does
               not occur exactly at midnight (UTC).

             2008/06/08 (MCB) - Added utc_iso_time_string_to_dut1tbl_time_string()
               function written by Kyle/Walt.

             2008/06/02 (MCB) - Added a function to populate the leap second table
               by reading in a file in the U.S. Naval Observatory format. See:

               http://maia.usno.navy.mil/ser7/tai-utc.dat

               The table is read into a set of global variables. Also, modified 
               interface to functions that return leap second count so that the leap
               second count is returned as a double.

             2007/09/17 (MCB) - Realized ACS_time is *apparent elapsed UTC seconds* 
                NOT the true elapsed SI seconds since the 2000-01-01T12:00:00UTC epoch.
                Changed old routines to be si_to_tt_and_ut1() and si_to_utc_iso_time_string()
                and introduced new routines to deal with new understanding of ACS_time.
             2007/05/25 (MCB) - Went through functions again. Added the following
               routines: utc_iso_time_string_to_tt_and ut1()
                         acs_to_tt_and ut1().
               Note that the resulting tt is elapsed time in SI seconds since
               2000-01-01T12:00:00 TT (i.e., the J2000.0 epoch), while the
               resulting ut1 time is elapsed universal time since 
               2000-01-01T12:00:00 UT1.
             2007/03/09 (MCB) - Carefully went through and checked out and fixed
               several functions.
             2007/03/06 (MCB) - moved calculation of leap second count into
               separate functions. Fixed very minor bug in leap_second_table,
               which would have only affected things if you put in a UTC time
               string such as 1972-06-30T23:59:60, i.e., right at a leap second
               instant. Moved some repeated code dealing with standardizing
               time components into the proper ranges over to time_utils.c.
             2007/02/26 (MCB) - reorganized the functions to be more logical;
               added two function to convert from UTC or UTC_string to 
               TDB and UT1.             

             Based on various .m files of same names.

  NOTES:
  -----
  1.  On/around 2007/09/18, made changes to reflect fact that ACS_time is apparent
        elapsed UTC seconds since 2000-01-01T12:00:00UTC. 
  2a. Updated implementation on/around 2007/05/25 to incorporate concept of ACS_time
        and fact that elpased UT1 time should be measured from 2000-01-01T12:00:00 UT1
        rather than from J2000.0.
  2b. Updated implementation on/around 2007/03/09 and 2007/03/06 to take better 
        understanding into account.
  2c. Updated implementation based on a revision to above memo dated 2006/07/03.
  2d. Based in part on a memo titled "Coordinate Systems for Earth Observing Spacecraft", 
       2005/01/4, which in turn is based on D. A. Valladio, "Fundamentals of Astrodynamics 
       and Applications", El Segundo, CA, Mircocosm Press, (2001).

  3. See http://en.wikipedia.org/wiki/Coordinated_Universal_Time for more information on UTC.

  4. Delta_AT = 33 as of 2006/01/01
  5. On 2000-01-01, Delta_UT1 = +0.3554752 sec.
     On 2005-12-29, Delta_UT1 = -0.6610984 sec.
     On 2006-06-29, Delta_UT1 = +0.1958360 sec.

  6. The behavior for some of these routines is undefined if a time is given that
       falls *WITHIN* or *NEAR THE BOUNDARY OF* a leap second, e.g., if you call 
       utc_iso_string_to_utc with UTC_string = 2005-12-31T23:59:60.5, the result 
       should not be trusted as it is probably wrong.

*******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "burl.h"
#include "strsel.h"
#include "time_utils.h"
#include "count_lines.h"
#include "tokenize.h"
#include "qmalloc.h"
#include "fgetl.h"
#include "time_conversion.h"
#include "sprintf_alloc.h"

#define MAX_N_LEAPS 1024

/***********************/
/* GLOBAL DECLARATIONS */
/***********************/
int    N_LEAPS = 0;
int    LEAP_TABLE_Y[MAX_N_LEAPS];   /* year */
int    LEAP_TABLE_T[MAX_N_LEAPS];   /* month */
int    LEAP_TABLE_D[MAX_N_LEAPS];   /* day */
int    LEAP_TABLE_H[MAX_N_LEAPS];   /* hour */
int    LEAP_TABLE_M[MAX_N_LEAPS];   /* minute */
double LEAP_TABLE_S[MAX_N_LEAPS];   /* second */
double LEAP_TABLE_ADJ[MAX_N_LEAPS]; /* adjustment */

/************************************/
/* General Notes on Time References */
/************************************/
/* UT0 - Universal Time based upon observations of stars (tied to Earth's angular rotation rate) */
/* UT1 - UT0 with corrections for polar motion */
/* UTC - Coordinated Universal Time derived from Atomic Time with updates to */
/*         maintain it within +/- 0.9 sec from UT1 */
/* TAI - International atomic time standard */
/* TT  - Terrestrial time */
/* TDB - Barycentric Dynamical Time (referenced to solar system barycenter) */ 
/*                                                                          */
/* Some useful relationships regarding absolute time: */
/*   TAI = UTC + Delta_AT;  where Delta_AT is number of leap seconds */
/*   TT  = TAI + 32.184;    */
/*   UT1 = UTC + Delta_UT1; */
/*                                                                             */
/* Delta_UT1 is available from http://tf.nist.gov/pubs/bulletin/leapsecond.htm */
/*   On Jan 01, 2000, Delta_UT1 = +0.3554732.     */
/*   On Dec 29, 2005, Delta_UT1 = -0.6611092 sec. */
/*   On Jun 29, 2006, Delta_UT1 = +0.1961948 sec. */
/*                                                                               */
/* Delta_AT is available from http://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat */
/* NOTE: that the value published on the hpiers.obspm.fr web page is the negative of Delta_AT */
/*   As of Jan 1, 2006, Delta_AT = +33 sec. This is the total number of leapseconds */
/*   added since 1972 (plus original 10 second offset). */
/*                                                                                  */
/* When time is given in seconds, it is important to know the start reference.      */
/*   For elpased TT time, the reference is 2000-01-01T12:00:00 TT (the J2000 epoch).*/
/*   For elapsed UT1 time, the reference is 2000-01-01T12:00:00 UT1.                */
/*   For ACS time, the reference is 2000-01-01T12:00:00 UTC.                        */

/**************************************/
/* UTC_ISO_TIME_STRING_TO_SI          */
/**************************************/
/* Given an absolute UTC ISO time string, determine the number of elapsed SI        */
/*   seconds since 2000-01-01T12:00:00 UTC. Note that this will be different than   */
/*   the number of *APPARENT* UTC seconds (obtained naively by converting end time  */
/*   and start time to Julian dates and subtracting) due to leap  seconds.          */

int utc_iso_time_string_to_si(char *UTC_string, double *SI_adr)
{
  int                  y1, t1, d1, h1, m1;
  double               s1;
  double               jd0 = 0.0;
  double               jd1;
  char                 *TT_string;
  char                 infunc[] = "utc_iso_time_string_to_si";

  utc_iso_time_string_to_tt_iso_time_string(UTC_string, &TT_string);
  parse_iso_time_string(TT_string, &y1, &t1, &d1, &h1, &m1, &s1);
  if (TT_string != NULL) {
    free((void *) TT_string);
  }

  if (y1 < 2000) {
    fprintf(stderr, "ERROR (%s): only valid for years 2000 and up\n", infunc);
    return(ERR);
  }
  /* Convert current TT time to a Julian date */
  julian_date(y1, t1, d1, h1, m1, s1, &jd1);

  /* Convert absolute TT time at 2000-01-01T12:00:00 UTC to a Julian date */
  julian_date(2000, 1, 1, 12, 1, ((double) 4.184), &jd1); 

  *SI_adr = (jd1 - jd0) * 86400; /* Elpased SI seconds since 2000-01-01T12:00:00 UTC */
  return(OK);
}

/**************************************/
/* UTC_ISO_TIME_STRING_TO_TT_AND_UT1 */
/**************************************/
/* Given an absolute UTC ISO time string, determine the elpased TT time and elapsed   */
/*   UT1 time. The elpased TT time is measured from 2000-01-01T12:00:00 TT, while the */
/*   elapsed UT1 time is measured from 2000-01-01T12:00:00 UT1.                       */

int utc_iso_time_string_to_tt_and_ut1(char *UTC_string, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  char                 *TT_string;
  double               TT;
  /*  char                 infunc[] = "utc_iso_time_string_to_tdb_and_ut1";*/

  /* UTC_string -> TT_string -> TT -> TDB */
  utc_iso_time_string_to_tt_iso_time_string(UTC_string, &TT_string);
  tt_iso_time_string_to_tt(TT_string, &TT);
  /*  printf("UTC_string = %s, TT_string = %s, TT = %.15f\n", UTC_string, TT_string, TT); */
  if (TT_string != NULL) {
    free((void *) TT_string);
  }

  /* TT -> UT1 */
  tt_to_ut1(TT, Delta_UT1, UT1_adr);
  *TT_adr = TT;

  return(OK);
}


/***********************************************/
/* UTC_ISO_TIME_STRING_TO_TT_ISO_TIME_STRING   */
/***********************************************/
/* Convert an absolute UTC time specified as an ISO (T) time string into an      */
/*   absolute TT time (also specified as an ISO time string).                    */
/*                                                                               */
/* INPUT:  UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                 */
/* OUTPUT: TT_string in ISO format, e.g., 1972-01-01T00:00:42.184                */
/*                                                                               */
/* NOTE:   This function is only valid for times after 1972-01-01T00:00:00 UTC.  */

int utc_iso_time_string_to_tt_iso_time_string(char *UTC_string, char **TT_string_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  int        y2, t2, d2, h2, m2;
  double     s2;
  double     leapsec_count;
  double     delta;
  /*  char       infunc[] = "utc_iso_time_string_to_tt_iso_time_string"; */

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  utc_time_components_to_leapsec_count(y0, t0, d0, h0, m0, s0, &leapsec_count);
  delta = leapsec_count+32.184;
  /* printf("leap adjustment  = %.3f\n", leapsec_count); */
  /* printf("TAI adjustment   = %.3f\n", 32.184); */
  /* printf("Total adjustment = %.3f\n", delta); */
  standardize_time_components(y0, t0, d0, h0, m0, s0+delta, &y2, &t2, &d2, &h2, &m2, &s2);
  compose_iso_time_string(y2, t2, d2, h2, m2, s2, TT_string_adr);

  return(OK);
}

/**********************************************/
/* UTC_ISO_TIME_STRING_TO_UT1_ISO_TIME_STRING */
/**********************************************/
/* Convert an absolute UTC time specified as an ISO (T) time string into an absolute UT1 */
/*   ISO time string                                                                     */
/*                                                                                       */
/* INPUT:                                                                                */
/* ------                                                                                */
/*   UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                               */
/*   Delta_UT1:   scalar value indicating UT1-UTC difference that is correct             */
/*                   for the date represented by UTC_string.                             */
/*                                                                                       */
/* OUTPUT:                                                                               */
/* ------                                                                                */
/* UT1_string - UT1 time in ISO format, e.g., 1972-01-01T00:00:42.184                    */
/*                                                                                       */
/* NOTES:                                                                                */
/* -----                                                                                 */
/* See also lookup_delta_ut1().                                                          */

int utc_iso_time_string_to_ut1_iso_time_string(char *UTC_string, double Delta_UT1, char **UT1_string_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  int        y2, t2, d2, h2, m2;
  double     s2;
  /*  char  infunc[] = "utc_iso_time_string_to_ut1_iso_time_string"; */

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  standardize_time_components(y0, t0, d0, h0, m0, s0+Delta_UT1, &y2, &t2, &d2, &h2, &m2, &s2);

  compose_iso_time_string(y2, t2, d2, h2, m2, s2, UT1_string_adr);

  return(OK);
}

/******************************************/
/* UTC_ISO_TIME_STRING_TO_LEAPSEC_COUNT   */
/******************************************/
/* Given an absolute UTC time specified as an ISO (T) time string, determine number of */
/*   leap seconds that have occurred prior to this time.                               */
/*                                                                                     */
/* INPUT:  UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                       */  
/* OUTPUT: integer number of leap seconds.                                             */
/* NOTE:   This function is only valid for UTC times after 1972-01-01T00:00:00 UTC.    */

int utc_iso_time_string_to_leapsec_count(char *UTC_string, double *leapsec_count_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  char       infunc[] = "utc_iso_time_string_to_leapsec_count";

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  if (y0 < 1972) {
    fprintf(stderr, "ERROR (%s): function only valid for UTC times after 1972-01-01T00:00:00, not %s\n", infunc, UTC_string);
    return(ERR);
  }
  utc_time_components_to_leapsec_count(y0, t0, d0, h0, m0, s0, leapsec_count_adr);

  return(OK);
}

/*****************************************************************/
/* UTC_ISO_TIME_STRING_TO_DUT1TBL_TIME_STRING                    */
/*****************************************************************/
/* Takes UTC iso-format time string and converts to DUT1 Table format (still UTC
content) */
/* NOTE: This function truncates input seconds to whole seconds. */

int utc_iso_time_string_to_dut1tbl_time_string(char *UTC_string, char **DUT1_table_string_adr)
{
  int y1, t1, d1, h1, m1;
  double s1;

  parse_iso_time_string(UTC_string, &y1, &t1, &d1, &h1, &m1, &s1);
  compose_dut1_table_time_string(y1, t1, d1, h1, m1, (int) s1, DUT1_table_string_adr);

  return(OK);
}

/*=========================================================================================*/

/******************************************/
/* UTC_TIME_COMPONENTS_TO_LEAPSEC_COUNT   */
/******************************************/
/* Given a UTC time split into time components (year, month, day, hour, minute, second) */
/*   determine number of leap seconds that have occured prior to this time.             */
/*                                                                                      */
/* INPUT:  UTC time components, e.g., 1972, 1, 1, 0, 0, 0.00                            */
/* OUTPUT: number of leap seconds (an integer, but returned as a double).               */
/* NOTE:   This function is only valid for years >= 1972.                               */

int utc_time_components_to_leapsec_count(int y0, int t0, int d0, int h0, int m0, double s0, double *leapsec_count_adr)
{
  int        k;
  char       infunc[] = "utc_time_components_to_leapsec_count";

  if (utc_time_components_to_leap_table_index(y0, t0, d0, h0, m0, s0, &k) == ERR) {
    fprintf(stderr, "ERROR (%s): unable to determine leap table index\n", infunc);
    return(ERR);
  }

  *leapsec_count_adr = LEAP_TABLE_ADJ[k];

  return(OK);
}

/*******************************************/
/* UTC_TIME_COMPONENTS_TO_LEAP_TABLE_INDEX */
/*******************************************/
/* Given a UTC time split into time components (year, month, day, hour, minute, second) */
/*   determine number of leap seconds that have occurred prior to this time.            */
/*                                                                                      */
/* INPUT:  UTC time components, e.g., 1972, 1, 1, 0, 0, 0.00                            */
/* OUTPUT: number of leap seconds (an integer, but returned as a double).               */
/* NOTE:   This function is only valid for years >= 1972.                               */

int utc_time_components_to_leap_table_index(int y0, int t0, int d0, int h0, int m0, double s0, int *k_adr)
{
  int        yi, ti, di, hi, mi;
  double     si;
  int        i, k;
  char       infunc[] = "utc_time_components_to_leap_table_index";

  if (N_LEAPS == 0) {
    fprintf(stderr, "ERROR (%s): leap second table not properly initialized\n", infunc);
    return(ERR);
  }
  k = N_LEAPS-1;
  for (i = 0; i < N_LEAPS; i++) {
    yi = LEAP_TABLE_Y[i];
    ti = LEAP_TABLE_T[i];
    di = LEAP_TABLE_D[i];
    hi = LEAP_TABLE_H[i];
    mi = LEAP_TABLE_M[i];
    si = LEAP_TABLE_S[i];
    if (y0 < yi) {
      k = i-1;
      break;
    }
    else if (y0 == yi) {
      if (t0 < ti) {
        k = i-1;
        break;
      }
      else if (t0 == ti) {
        if (h0 < hi) {
          k = i-1;
           break;
        }
        else if (h0 == hi) {
          if (m0 < mi) {
            k = i-1;
            break;
          }
          else if (m0 == mi) {
            if (s0 < si) {
              k = i-1;
              break;
            }
          }
	}
      }
    }
  }

  if (k == -1) {
    fprintf(stderr, "ERROR (%s): cannot handle UTC times before 1972-01-01T00:00:00\n", infunc);
    return(ERR);
  }

  *k_adr = k;

  return(OK);
}

/*=========================================================================================*/

/**********************/
/* ACS_TO_TT_AND_UT1   */
/**********************/
/* Given the number of apparent UTC seconds that have elapsed since 2000-01-01T12:00:00 UTC, */
/*   determine the number of TT seconds since 2000-01-01T12:00:00 TT and the number of       */
/*   UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1.                            */

int acs_to_tt_and_ut1(double ACS_time, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  /*  char      infunc[] = "acs_to_tt_and_ut1"; */

  acs_to_tt(ACS_time, TT_adr);
  acs_to_ut1(ACS_time, Delta_UT1, UT1_adr);

  return(OK);
}

/**************/
/* ACS_TO_TT   */
/**************/
/* INPUT:  ACS_time is the number of apparent elapsed UTC seconds since 2000-01-01T12:00:00 UTC. */
/*                                                                                          */
/* OUTPUT: TT is the number of elapsed TT seconds since 2000-01-01T12:00:00 TT.             */
/*                                                                                          */
/* NOTE:   The duration of a TT second is the same as an SI second.                         */

int acs_to_tt(double ACS_time, double *TT_adr)
{
  char     *UTC_string;
  double   leapsec_count;
  /*  char     infunc[] = "acs_to_tt"; */

  acs_to_utc_iso_time_string(ACS_time, &UTC_string);
  utc_iso_time_string_to_leapsec_count(UTC_string, &leapsec_count);
  free((void *) UTC_string);
  *TT_adr = ACS_time + 64.184 + (leapsec_count - 32); /* 32 is leapsec count at 2000-01-01T12:00:00UTC */
                                                 /* 64.184 is time elapsed from TT epoch to SC epoch */

  return(OK);
}

/**************/
/* ACS_TO_UT1  */
/**************/
/* Convert an ACS time (apparent elapsed UTC seconds since 2000-01-01T12:00:00 UTC)  */
/*   into the number of UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1. */
/*   UT1 is closely related to UTC by design. The value of Delta_UT1 is published in */
/*   tables. See the lookup_delta_ut1() function further below.                      */

int acs_to_ut1(double ACS_time, double Delta_UT1, double *UT1_adr)
{
  /*  char     infunc[] = "acs_to_ut1"; */

  *UT1_adr = ACS_time + Delta_UT1;

  return(OK);
}

/******************************/
/* ACS_TO_UTC_ISO_TIME_STRING  */
/******************************/
/* Take apparent elpased UTC seconds since 2000-01-01T12:00:00 UTC and convert into  */
/* a UTC iso-format time string.                                           */

int acs_to_utc_iso_time_string(double ACS_time, char **UTC_string_adr)
{
  int      y1, t1, d1, h1, m1;
  double   s1;
  /* char     infunc[] = "acs_to_utc_iso_time_string"; */

  standardize_time_components(2000, 1, 1, 12, 0, ACS_time, &y1, &t1, &d1, &h1, &m1, &s1);
  compose_iso_time_string(y1, t1, d1, h1, m1, s1, UTC_string_adr);

  return(OK);
}


/*=========================================================================================*/

/**********************/
/* SI_TO_TT_AND_UT1   */
/**********************/
/* Given the number of SI seconds that have elapsed since 2000-01-01T12:00:00 UTC,     */
/*   determine the number of TT seconds since 2000-01-01T12:00:00 TT and the number of */
/*   UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1.                      */

int si_to_tt_and_ut1(double SI, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  /*  char      infunc[] = "si_to_tt_and_ut1"; */

  si_to_tt(SI, TT_adr);
  si_to_ut1(SI, Delta_UT1, UT1_adr);

  return(OK);
}

/**************/
/* SI_TO_TT   */
/**************/
/* INPUT:  SI is the number of elapsed SI seconds since 2000-01-01T12:00:00 UTC. */
/*                                                                                */
/* OUTPUT: TT is the number of elapsed TT seconds since 2000-01-01T12:00:00 TT.   */
/*                                                                                */
/* NOTE:   The duration of a TT second is the same as an SI second.               */

int si_to_tt(double SI, double *TT_adr)
{
  char     infunc[] = "si_to_tt";

  if (SI < (-64.184 - 1e-07)) { /* Add an epsilon for numerical precision errors */
    fprintf(stderr, "ERROR (%s): SI time must be >= -64.184\n", infunc);
    return(ERR);
  }

  *TT_adr = SI + 64.184;

  return(OK);
}

/**************/
/* SI_TO_UT1  */
/**************/
/* Convert an SI time (elapsed SI seconds since 2000-01-01T12:00:00 UTC)            */
/*   into the number of UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1. */
/*   UT1 is clsoely related to UTC by design. The value of Delta_UT1 is published in */
/*   tables. See the lookup_delta_ut1() function further below.                      */

int si_to_ut1(double SI, double Delta_UT1, double *UT1_adr)
{
  char     *UTC_string;
  double   leapsec_count;
  /*  char     infunc[] = "si_to_ut1"; */

  si_to_utc_iso_time_string(SI, &UTC_string);
  utc_iso_time_string_to_leapsec_count(UTC_string, &leapsec_count);
  if (UTC_string != NULL) {
    free((void *) UTC_string);
  }
  *UT1_adr = SI + Delta_UT1 - (leapsec_count - 32); /* 32 is leapsec_count at 2000-01-01T12:00:00 UTC */

  return(OK);
}

/******************************/
/* SI_TO_UTC_ISO_TIME_STRING  */
/******************************/
/* Take elpased SI seconds since 2000-01-01T12:00:00 UTC and convert into  */
/* a UTC iso-format time string.                                           */

int si_to_utc_iso_time_string(double SI, char **UTC_string_adr)
{
  double   TT;
  /* char     infunc[] = "si_to_utc_iso_time_string"; */

  si_to_tt(SI, &TT);
  tt_to_utc_iso_time_string(TT, UTC_string_adr);

  return(OK);
}

/*=========================================================================================*/

/*****************************/
/* TT_ISO_TIME_STRING_TO_TT  */
/*****************************/
/* Convert an absolute TT time (Terrestrial Time) specified as an ISO (T) */
/*   time string into TT time in seconds since the J2000.0 epoch          */
/*   (2000-01-01:12:00:00 TT).                                            */
/*                                                                        */
/* NOTE: See also the NAIF CSPICE library function STR2ET.c               */

int tt_iso_time_string_to_tt(char *TT_string, double *TT_adr)
{
  int                      year, month, day, hour, minute;
  double                   JD;
  double                   second;
  /*  char                 infunc[] = "tt_iso_time_string_to_tt"; */

  parse_iso_time_string(TT_string, &year, &month, &day, &hour, &minute, &second);
  julian_date(year, month, day, hour, minute, second, &JD);
  *TT_adr = (JD - 2451545.0) * 86400.0; /* TT seconds since J2000.0 epoch */

  return(OK);
}

/*****************************/
/* TT_ISO_TIME_STRING_TO_TDB */
/*****************************/
/* Convert a TT time (Terrestrial Time) specified as an ISO (T) time string into */
/*   TDB time (Barycentric Dynamical Time) in seconds since the J2000.0 epoch,   */
/*   which is defined as 2000-01-01:12:00:00 TT.                                 */
/*                                                                               */
/* NOTE 1: See also the NAIF CSPICE library function STR2ET.c                    */
/* NOTE 2: tt_to_tdb evaluated at J2000.0 (TT=0) returns a TDB value ~0.0007.    */
/*           Unsure whether this small TDB value should be subtracted to get     */
/*           an official elpased TDB or whether this is just ignored. For now,   */
/*           it is being ignored.                                                */

int tt_iso_time_string_to_tdb(char *TT_string, double *TDB_adr)
{
  double                   TT;
  /*  char                 infunc[] = "tt_iso_time_string_to_tdb"; */

  tt_iso_time_string_to_tt(TT_string, &TT);
  tt_to_tdb(TT, TDB_adr);

  return(OK);
}

/*=========================================================================================*/


/******************************/
/* TT_TO_UTC_ISO_TIME_STRING */
/******************************/
/* Take a TT elapsed time given as seconds since the J2000.0 epoch         */
/* (2000-01-01T12:00:00 TT) and convert into a UTC iso-format time string. */
/*                                                                         */
/*  NOTE: if TT put us inside of a leap second, we report an error.        */

int tt_to_utc_iso_time_string(double TT, char **UTC_string_adr)
{
  int      y0, t0, d0, h0, m0;
  double   s0;
  int      y1, t1, d1, h1, m1;
  double   s1;
  double   leapfix;
  int      inside_leap;
  double   tt_pre_leap_instant, tt_post_leap_instant;
  char     *UTC_tmp;
  double   utc_jd0, utc_jd;
  double   this_leap, apparent_seconds_since_j2000, total_leap_adjustment;
  int      i, k;
  char     infunc[] = "tt_to_utc_iso_time_string";

  /* Check that input is valid */
  if (TT < 0) {
    fprintf(stderr, "ERROR (%s): TT must be non-negative\n", infunc);
    return(ERR);
  }

  /* Calculate Julian date at J2000 instant using UTC time scale */
  julian_date(2000, 1, 1, 11, 58, (double) 55.816, &utc_jd0);

  /* Determine row in leap table for J2000 */ 
  utc_time_components_to_leap_table_index(2000, 1, 1, 11, 58, (double) 55.816, &k);

  inside_leap = 0;
  leapfix = LEAP_TABLE_ADJ[N_LEAPS-1] - LEAP_TABLE_ADJ[k]; /* If TT is past end of leap table */

  /* See where TT falls relative to the leap seconds that have occurred since J2000 */
  for (i = k+1; i < N_LEAPS; i++) {
    julian_date(LEAP_TABLE_Y[i], LEAP_TABLE_T[i], LEAP_TABLE_D[i], LEAP_TABLE_H[i], LEAP_TABLE_M[i], LEAP_TABLE_S[i], &utc_jd);
    apparent_seconds_since_j2000 = (utc_jd - utc_jd0) * 86400.0;
    this_leap =  LEAP_TABLE_ADJ[i] - LEAP_TABLE_ADJ[i-1];
    total_leap_adjustment =  LEAP_TABLE_ADJ[i] - LEAP_TABLE_ADJ[k];
    tt_post_leap_instant = apparent_seconds_since_j2000 + total_leap_adjustment;
    tt_pre_leap_instant = tt_post_leap_instant - this_leap;

    /* printf("tt_pre_leap_instant = %.15f, tt_post_leap_instant = %.15f\n", tt_pre_leap_instant, tt_post_leap_instant); */
    if (TT < tt_pre_leap_instant) {
      leapfix = total_leap_adjustment - this_leap;
      break;
    }
    else if ((TT >= tt_pre_leap_instant) && (TT < tt_post_leap_instant)) {
      /* TT puts us inside leap second interval. Break out. */
      inside_leap = 1;
      break;
    }
    else {
      /* TT is past the current tt_post_leap_instant - do next iteration or drop out of loop */
    }

  }
  /*  printf("after loop, inside_leap = %d, leapfix = %.15f\n", inside_leap, leapfix);*/
  if (inside_leap == 0) {
    /* Normal case - not inside leap second */
    y0 = 2000;
    t0 = 1;
    d0 = 1;
    h0 = 11;
    m0 = 58;
    s0 = 55.816 + TT - leapfix;
    standardize_time_components(y0, t0, d0, h0, m0, s0, &y1, &t1, &d1, &h1, &m1, &s1);
    compose_iso_time_string(y1, t1, d1, h1, m1, s1, UTC_string_adr);
  }
  else {
    compose_iso_time_string(LEAP_TABLE_Y[i], LEAP_TABLE_T[i], LEAP_TABLE_D[i], LEAP_TABLE_H[i], LEAP_TABLE_M[i], LEAP_TABLE_S[i], &UTC_tmp);
    fprintf(stderr, "ERROR (%s): TT falls within a leap second (%s UTC)\n", infunc, UTC_tmp);
    free((void *) UTC_tmp);
    return(ERR);
  }

  return(OK);
}

/**************/
/* TT_TO_SI   */
/**************/
/* Given a TT time as elpased SI seconds since 2000-01-01T12:00:00 TT, determine   */
/*   the elapsed time in SI seconds since 2000-01-01T12:00:00 UTC.                 */
/* NOTE:   The duration of a TT second is the same as an SI second.                */

int tt_to_si(double TT, double *SI_adr)
{
  char     infunc[] = "tt_to_si";

  if (TT < 0) { 
    fprintf(stderr, "ERROR (%s): TT time must be >= 0\n", infunc);
    return(ERR);
  }

  *SI_adr = TT - 64.184;

  return(OK);
}


/**************/
/* TT_TO_UT1  */
/**************/
/* TT is given as elapsed seconds since 2000-01-01T12:00:00 TT.       */
/* UT1 is given as elapsed UT1 seconds since 2000-01-01T12:00:00 UT1. */ 

int tt_to_ut1(double TT, double Delta_UT1, double *UT1_adr)
{
  double   SI;
  /*  char     infunc[] = "tt_to_ut1"; */

  tt_to_si(TT, &SI);
  si_to_ut1(SI, Delta_UT1, UT1_adr);

  return(OK);
}


/**************/
/* TT_TO_TDB */
/**************/
/* Convert a TT time (Terrestrial Time) in seconds since the J2000.0 epoch,   */
/*   which is defined as 2000-01-01:12:00:00 TT, into a TDB time (Barycentric */
/*   Dynamical Time) in seconds since the J2000.0 epoch.                      */
/*                                                                            */
/* NOTE 1: See also the NAIF CSPICE library function STR2ET.c                 */
/* NOTE 2: At the J2000.0 epoch, where TT = 0, this function returns a non-   */
/*           zero value for TBD (something like 0.00007).                     */

int tt_to_tdb(double TT, double *TDB_adr)
{
  double                   T_TT;
  double                   M_Earth_deg;
  double                   M_Earth_rad;
  /*  char                     infunc[] = "tt_to_tdb"; */

  T_TT = TT/(86400.0 * 36525.0);  /* T_TT is TT expressed in Julian centuries */
  M_Earth_deg = ((double) 357.5277233) + ((double) 35999.05034) * T_TT;
  M_Earth_rad = M_Earth_deg * DEG2RAD;

  /* The barycentric dynamical time is approximately given by: [formula from */
  /*   memo]. The NAIF CSPICE STR2ET function uses a slightly more complex */
  /*   formula. */
  *TDB_adr = TT + 0.001658*sin(M_Earth_rad) + 0.00001385 * sin(2*M_Earth_rad);

  return(OK);
}

/*=========================================================================================*/

/********************/
/* LOOKUP_DELTA_UT1 */
/********************/
/* Given a date in yyyymmdd form, determine through table lookup, the proper */
/*   value of Delta_UT1, the difference between UTC and UT1 for the date. */
/*   The table is stored in a file named fname. Each line has a yyyymmdd date */
/*   followed by a real value for Delta_UT1. Lines in file are sorted by increasing */
/*   date. */

int lookup_delta_ut1(char *fname, int yyyymmdd, double *Delta_UT1_adr)
{
  FILE    *fp;
  int     i;
  int     n_lines;
  int     n_tokens;
  int     *b, *e;
  char    tok0[64];
  char    tok1[64];
  int     L;
  int     yyyymmdd_i;
  int     found;
  double  Delta_UT1;
  char    line[MAXSTRING];
  char    infunc[] = "lookup_delta_ut1";

  count_lines(fname, 2, &n_lines, &fp);

  found = 0;
  for (i = 0; i < n_lines; i++) {
    fgetl(line, MAXSTRING, fp);
    tokenize(line, " ", &n_tokens, &b, &e);
    if (n_tokens != 2) {
      fprintf(stderr, "ERROR (%s): expected 2 tokens on line |%s|, found %d\n", infunc, line, n_tokens);
      return(ERR);
    }
    L = get_max_token_length(n_tokens, b, e);
    if (L > 63) {
      fprintf(stderr, "ERROR (%s): token too long on line |%s|\n", infunc, line);
      return(ERR);
    }
    extract_token(tok0, line, b, e, 0);
    extract_token(tok1, line, b, e, 1);
    free((void *) b);
    free((void *) e);

    yyyymmdd_i  = (int) atoi(tok0);
    if (yyyymmdd == yyyymmdd_i) {
      found = 1;
      Delta_UT1 = (double) atof(tok1);
      break;
    }
  }
  fclose(fp);

  if (found == 0) {
    fprintf(stderr, "ERROR (%s): lookup failed for date %d in file %s\n", infunc, yyyymmdd, fname); 
    return(ERR);
  }
  *Delta_UT1_adr = Delta_UT1;

  return(OK);
}

/********************************/
/* INITIALIZE_LEAP_SECOND_TABLE */
/********************************/
int initialize_leap_second_table(char *filename)
{
  FILE    *fp;
  char    tok0[64], tok1[64], tok2[64], tok3[64], tok4[64], tok5[64], tok6[64];
  int     n, nn, n_lines, n_tokens;
  int     y, t, d;
  double  L;
  int     *b, *e;
  int     yf, tf, df, hf, mf;
  double  sf;
  double  jd0, jd1, offset;
  char    line[MAXSTRING];
  char    infunc[] = "initialize_leap_second_table";

  if (count_lines(filename, 2, &n_lines, &fp) == ERR) {
    fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
    return(ERR);
  }
  if (n_lines > MAX_N_LEAPS) {
    fprintf(stderr, "ERROR (%s): number of lines in %s exceeds MAX_N_LEAPS = %d\n", infunc, filename, (int) MAX_N_LEAPS);
    return(ERR);
  }

  nn = 0;
  for (n = 0; n < n_lines; n++) {
    fgetl(line, MAXSTRING, fp);
    tokenize(line, " ", &n_tokens, &b, &e);
   
    /*    for (i = 0; i < n_tokens; i++) { */
    /*      extract_token(tok0, line, b, e, i);*/
    /*      printf("token %d = |%s|\n", i, tok0);*/
    /*    }*/

    if (n_tokens < 7) {
      fprintf(stderr, "ERROR (%s): expected at least 7 tokens on line |%s|, found %d\n", infunc, line, n_tokens);
      return(ERR);
    }
    /* For safety check that no tokens longer than 63; */
    /*   really only need to check that tokens 0,1,2, and 6 meet this requirement */
    L = get_max_token_length(n_tokens, b, e);
    if (L > 63) {
      fprintf(stderr, "ERROR (%s): token too long on line |%s|\n", infunc, line);
      return(ERR);
    }

    extract_token(tok0, line, b, e, 0); /* year */
    extract_token(tok1, line, b, e, 1); /* month string */
    extract_token(tok2, line, b, e, 2); /* day */
    extract_token(tok3, line, b, e, 3); /* =JD */
    extract_token(tok4, line, b, e, 4); /* juian date */
    extract_token(tok5, line, b, e, 5); /* TAI-UTC= */
    extract_token(tok6, line, b, e, 6); /* leap second adjustment */

   /* Verify that token 3 is "=JD" */
    if (strcasecmp(tok3, "=JD") != 0) {
      fprintf(stderr, "ERROR (%s): invalid tok3 = |%s| on line |%s|\n", infunc, tok3, line);
      return(ERR);
    }

    /* Verify that token 5 is "TAI-UTC=" */
    if (strcasecmp(tok5, "TAI-UTC=") != 0) {
      fprintf(stderr, "ERROR (%s): invalid tok5 = |%s| on line |%s|\n", infunc, tok5, line);
      return(ERR);
    }

    y = (int) atoi(tok0);
    if (y >= 1972) {
      /* Only process for years >= 1972 */
      if (month_string_to_month(tok1, &t) == ERR) {
        fprintf(stderr, "ERROR (%s): bad month string |%s| on line |%s|\n", infunc, tok1, line);
        return(ERR);
      }
      L = (double) atof(tok6);
      d = (int) atoi(tok2);
      julian_date(y, t, d, 0, 0, 0, &jd0); /* nominal julian date at midnight given year, month, and day */
      jd1 = (double) atof(tok4); /* supplied value of julian date in token 4 */
      offset = (jd1 - jd0) * 86400; /* Number of seconds by which to adjust leap second instant based on JD */
      standardize_time_components(y, t, d, 0, 0, offset, &yf, &tf, &df, &hf, &mf, &sf); /* negative offsets are ok */
      LEAP_TABLE_Y[nn] = yf;
      LEAP_TABLE_T[nn] = tf;
      LEAP_TABLE_D[nn] = df;
      LEAP_TABLE_H[nn] = hf;
      LEAP_TABLE_M[nn] = mf;
      LEAP_TABLE_S[nn] = sf;
      LEAP_TABLE_ADJ[nn] = L;
      /* printf("nn = %d, yf = %d, tf = %d, df = %d, hf = %d, mf = %d, sf = %.15f\n", nn, yf, tf, df, hf, mf, sf);*/
      nn++;
    }
    N_LEAPS = nn;

    free((void *) b);
    free((void *) e);
  }
  fclose(fp);
  
  return(OK);
}


