/*******************************************************************************

  Title:     time_utils
  Author:    Mike Burl 
  Date:      2006/12/27
  Function:  This file contains a number of functions for handling
               generic time and date formats/conversions. 

  History:   2008/07/09 (MCB) - Modified standardize_time_components to allow
               a negative value for seconds. Also, modified calendar_increment
               to allow a negative value for n_days. Found a "theoretical" bug
               in standardize_time_components where if input value of month or day
               was out-of-range it would not get standardized. 

             2008/06/08 (MCB) - Added compose_dut1_table_time_string() function 
               written by Kyle/Walt.

             2007/02/20 (MCB) - removed floor function from parse_yyyymmdd as it
               is unecessary and was causing a bug under solaris. (unsafe due to 
               epsilon error in division?).

             Based on various .m files of same names.
 
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "burl.h"
#include "time_utils.h"
#include "strsel.h"
#include "sprintf_alloc.h"

/***********************/
/* GLOBAL DECLARATIONS */
/***********************/


/***********************/
/* JULIAN_DATE         */
/***********************/
int julian_date(int year, int month, int day, int hour, int minute, double second, double *jd_adr)
{
  int                  a, y, m;
  int                  jdi;
  double               jd;
  /* char                 infunc[] = "julian_date"; */

  a = (14-month)/12; /* integer division is intentional */
  y = year + 4800-a;
  m = month + 12*a-3;
  jdi = day + ((153*m+2)/5) + y/4 - (y/100) + (y/400) - 32045; /* again integer division is intentional */
  jd = jdi + 365.0 * y + ((double) (hour-12.0))/((double) 24.0) + ((double) minute)/((double) 1440.0) + second/((double) 86400.0);

  *jd_adr = jd;

  return(OK);
}


/*************************/
/* PARSE_ISO_TIME_STRING */
/*************************/
int parse_iso_time_string(char *T, int *year_adr, int *month_adr, int *day_adr, int *hour_adr, int *minute_adr, double *second_adr)
{
  int                  L;
  char                 tmp[128];
  char                 infunc[] = "parse_iso_time_string";

  L = strlen(T);
  if (L < 18) {
    fprintf(stderr, "ERROR (%s): input time string = [%s] has bad format\n", infunc, T);
    return(ERR);
  }
  if ((T[4] != '-') || (T[7] != '-') || (T[10] != 'T') || (T[13] != ':') || (T[16] != ':')) {
    fprintf(stderr, "ERROR (%s): input time string = [%s] has bad format\n", infunc, T);
    return(ERR);
  }

  strsel(tmp, T, 0, 3);
  *year_adr = (int) atoi(tmp);
  strsel(tmp, T, 5, 6);
  *month_adr = (int) atoi(tmp);
  strsel(tmp, T, 8, 9);
  *day_adr = (int) atoi(tmp);
  strsel(tmp, T, 11, 12);
  *hour_adr = (int) atoi(tmp);
  strsel(tmp, T, 14, 15);
  *minute_adr = (int) atoi(tmp);
  strsel(tmp, T, 17, L-1);
  *second_adr = (double) atof(tmp);

  return(OK);
}

/***************************/
/* COMPOSE_ISO_TIME_STRING */
/***************************/
/* OUTPUT: T - Time string is ISO (T) format: 2006-12-19T11:48:12.3457 */

int compose_iso_time_string(int year, int month, int day, int hour, int minute, double second, char **T_adr)
{
  int    wholesec;
  /*  char                 infunc[] = "compose_iso_time_string"; */

  wholesec = (int) second;
  if (wholesec >= 10) {
    sprintf_alloc(T_adr, "%04d-%02d-%02dT%02d:%02d:%.12f", year, month, day, hour,  minute, second);
  }
  else {
    sprintf_alloc(T_adr, "%04d-%02d-%02dT%02d:%02d:%d%.12f", year, month, day, hour,  minute, 0, second);
  }
  return(OK);
}

/**************************************************/
/* COMPOSE_DUT1_TABLE_TIME_STRING                 */
/**************************************************/
/* OUTPUT: T - Time string is DUT1 Table format: 12-19-2006 11:48:12 */
/*   NOTE: the input "second" is specified as an int! */

int compose_dut1_table_time_string(int year, int month, int day, int hour, int minute, int second, char **T_adr)
{

  sprintf_alloc(T_adr, "%02d-%02d-%04d %02d:%02d:%02d", month, day, year, hour, minute,
second);

  return(OK);
}

/****************/
/* IS_LEAP_YEAR */
/****************/
/* Given a year in the Gregorian (standard) calendar, determine whether it is */
/*   a leap year. */

int is_leap_year(int year)
{
  int                  leap_year_flag;
  /*  char                 infunc[] = "is_leap_year"; */

  if ((year % 400) == 0) {
    /* Divisible by 400 */
    leap_year_flag = 1;
  }
  else if ((year % 100) == 0) {
    /* Not divisible by 400, but divisible by 100 */
    leap_year_flag = 0;
  }
  else if ((year % 4) == 0) {
    /* Not divisible by 400, not divisible by 100, but divisible by 4 */
    leap_year_flag = 1;
  }
  else {
    /* Not divisible by 400, 100, or 4 */
    leap_year_flag = 0;
  }

  return(leap_year_flag);
}


/******************/
/* PARSE_YYYYMMDD */
/******************/
/* Take an input calendar date as an integer in the form yyyymmdd */
/*   and split into year, month, and day. */

int parse_yyyymmdd(int yyyymmdd, int *year_adr, int *month_adr, int *day_adr)
{
  *year_adr  = yyyymmdd/10000; /* integer-division is intentional */
  *month_adr = (yyyymmdd - 10000*(*year_adr))/100; /* interger division is intentional */
  *day_adr   = yyyymmdd - 10000*(*year_adr) - 100*(*month_adr);

  return(OK);
}

/********************/
/* COMPOSE_YYYYMMDD */
/********************/
/* Take an input calendar date split into year, month, and day and */
/*   compose a single integer representing the date in yyyymmdd form */

int compose_yyyymmdd(int year, int month, int day, int *yyyymmdd_adr)
{

  *yyyymmdd_adr = 10000*year+100*month+day;

  return(OK);
}


/******************************************/
/* STANDARDIZE_TIME_COMPONENTS            */
/******************************************/
/* Given a time split into time components (year, month, day, hour, minute, second) */
/*   standardize the components, so that:                                           */
/*                                                                                  */
/*   0 <= second < 60;                                                              */
/*   0 <= minute < 60                                                               */
/*   0 <= hour < 24                                                                 */
/*   day within range of days for month and year                                    */
/*   1 <= month <= 12                                                               */
/*                                                                                  */
/* NOTE: We assume all incoming parameter values are nonnegative except possibly    */
/*   for the value of seconds.                                                      */
/*   This func does not deal with leap seconds, e.g., if you had split a UTC        */ 
/*   time string into 2005, 12, 31, 23, 59, 65 and then called this function, you   */
/*   would get back 2006, 1, 1, 0, 0, 5, even though UTC added a leap second at     */
/*   2005-12-31T23:59:59.                                                           */
/*                                                                                  */
/* 2008/07/09 (MCB) - modified to allow s0 to be negative. Other input params must  */
/*   still be non-negative.                                                         */

int standardize_time_components(int y0, int t0, int d0, int h0, int m0, double s0, 
      int *y1_adr, int *t1_adr, int *d1_adr, int *h1_adr, int *m1_adr, double *s1_adr)
{
  static int  days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int         leap_days[12];
  int         yyyymmdd;
  int         yyyymmdd_new;
  int         i;
  char        infunc[] = "standardize_time_components";

  if ((y0 < 0) || (t0 < 0) || (d0 < 0) || (h0 < 0) || (m0 < 0)) {
    fprintf(stderr, "ERROR (%s): input values except for s0 must be non-negative\n", infunc);
    return(ERR);
  }

  /* In case month or day is out of range */
  for (i = 0; i < 12; i++) {
    leap_days[i] = 0;
  }
  leap_days[2-1] = is_leap_year(y0);
  while (t0 > 12) {
    t0 = t0-12;
    y0 = y0+1;
    leap_days[2-1] = is_leap_year(y0);
  }
  while (d0 > (days_in_month[t0-1] + leap_days[t0-1])) {
    d0 = d0 - (days_in_month[t0-1] + leap_days[t0-1]);
    t0 = t0+1;
  }  
  while (t0 > 12) {
    t0 = t0-12;
    y0 = y0+1;
    leap_days[2-1] = is_leap_year(y0);
  }
  compose_yyyymmdd(y0, t0, d0, &yyyymmdd); /* Should have a valid yyyymmdd now */


  if (s0 >= 0) {
    while (s0 >= 60) {
      s0 = s0-60;
      m0 = m0+1;
    }

    while (m0 >= 60) {
      m0 = m0-60;
      h0 = h0+1;
    }

    while (h0 >= 24) {
      h0 = h0-24;
      calendar_increment(yyyymmdd, 1, &yyyymmdd_new);
      yyyymmdd = yyyymmdd_new;
    }
  }
  else {
    while (s0 < 0) {
      s0 = s0+60;
      m0 = m0-1;
    }
    while (m0 < 0) {
      m0 = m0+60;
      h0 = h0-1;
    }
    while (h0 < 0) {
      h0 = h0+24;
      calendar_increment(yyyymmdd, -1, &yyyymmdd_new);
      yyyymmdd = yyyymmdd_new;
    }
  }

  parse_yyyymmdd(yyyymmdd, y1_adr, t1_adr, d1_adr);
  *h1_adr = h0;
  *m1_adr = m0;
  *s1_adr = s0;

  return(OK);
}

/**********************/
/* CALENDAR_INCREMENT */
/**********************/
/* Take an input calendar date as an integer in the form yyyymmdd    */
/*   and add n_days to that to get a new calendar date in the same   */
/*   yyyymmdd format.                                                */
/*                                                                   */
/* 2008/07/09 (MCB) - modified to also allow decrements (n_days < 0) */

int calendar_increment(int yyyymmdd_old, int n_days, int *yyyymmdd_new_adr)
{
  static int  days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int         year, month, day;
  int         leap_days[12];
  int         i;
  /*  char        infunc[] = "calendar_increment";*/

  parse_yyyymmdd(yyyymmdd_old, &year, &month, &day);
  for (i = 0; i < 12; i++) {
    leap_days[i] = 0;
  }
  leap_days[2-1] = is_leap_year(year);

  if (n_days >= 0) {
    /* increment case */
    day = day + n_days;
    while (day > (days_in_month[month-1] + leap_days[month-1]) ) {
      day = day - (days_in_month[month-1]+leap_days[month-1]);
      month++;
      while (month > 12) {
        month -= 12;
        year++;
        leap_days[2-1] = is_leap_year(year);
      }
    }
    compose_yyyymmdd(year, month, day, yyyymmdd_new_adr);
  }
  else {
    /* decrement case */
    day = day + n_days;
    while (day < 1) {
      if (month == 1) {
        day = day + (days_in_month[12-1]+leap_days[12-1]);
      }
      else {
        day = day + (days_in_month[month-2]+leap_days[month-2]);
      }
      month--;
      while (month < 1) {
        month += 12;
        year--;
        leap_days[2-1] = is_leap_year(year);
      }
    }
    compose_yyyymmdd(year, month, day, yyyymmdd_new_adr);
  }

  return(OK);
}

/*************************/
/* MONTH_STRING_TO_MONTH */
/*************************/
/* Convert a month string to month. Only looks at first three characters, so "Janus",  */
/*   "Jan", "jan", etc. will all match to "January". The month string must be at least */
/*   three characters even though some months are unambiguous at 1. */

int month_string_to_month(char *month_str, int *month_adr)
{
  int   L;
  int   month;
  char  infunc[] = "month_string_to_month";

  L = strlen(month_str);
  if (L < 3) {
    fprintf(stderr, "ERROR (%s): month string %s does not contain at least 3 chars\n", infunc, month_str);
    *month_adr = 0;
    return(ERR);
  } 

  if (strncasecmp(month_str, "jan", 3) == 0) {
    month = 1;
  }
  else if (strncasecmp(month_str, "feb", 3) == 0) {
    month = 2;
  }
  else if (strncasecmp(month_str, "mar", 3) == 0) {
    month = 3;
  }
  else if (strncasecmp(month_str, "apr", 3) == 0) {
    month = 4;
  }
  else if (strncasecmp(month_str, "may", 3) == 0) {
    month = 5;
  }
  else if (strncasecmp(month_str, "jun", 3) == 0) {
    month = 6;
  }
  else if (strncasecmp(month_str, "jul", 3) == 0) {
    month = 7;
  }
  else if (strncasecmp(month_str, "aug", 3) == 0) {
    month = 8;
  }
  else if (strncasecmp(month_str, "sep", 3) == 0) {
    month = 9;
  }
  else if (strncasecmp(month_str, "oct", 3) == 0) {
    month = 10;
  }
  else if (strncasecmp(month_str, "nov", 3) == 0) {
    month = 11;
  }
  else if (strncasecmp(month_str, "dec", 3) == 0) {
    month = 12;
  }
  else {
    fprintf(stderr, "ERROR (%s): could not find match for month string %s\n", infunc, month_str);
    *month_adr = 0;
    return(ERR);
  }

  *month_adr = month;

  return(OK);
}
