/*************************************************************************
* time_value.cc
* Subroutines for sclk2scet  & scet2sclk programs.
* History:
*    May 22, 2001 (Hyun H. Lee)
*       - Added range checks for doy, hr, min, sec in get_components method.
*************************************************************************/
#include <iostream>
#include <stdlib.h>
#include <iomanip>
#include "time_value.h"


/********************************* TimeValue *********************************/
void TimeValue::set_time(int fract, int sec, int min, int hour,
              int day, int month, int year, int tz, int dst)
{
   struct tm my_time_tm;

   t_fract = fract;
   my_time_tm.tm_sec = sec;
   my_time_tm.tm_min = min;
   my_time_tm.tm_hour = hour;
   my_time_tm.tm_mday = day;
   my_time_tm.tm_mon = month-1;
   my_time_tm.tm_year = year-1900;

   if(dst == 99) dst = -1;
   my_time_tm.tm_isdst = dst;
   my_time_tm.tm_isdst = -1;
   
   if(tz == 99)
   {
      my_time_t = mktime(&my_time_tm);   // assume local time
   }
   else
   {
      // Retrieve local time zone and set local_TZ to be used in restoring it
      static char local_TZ[13], ltz[10], *p;
      strcpy(local_TZ,"TZ=");
      p = getenv("TZ");
      strcat(ltz,p);
      strcat(local_TZ,ltz);

      // Set input_TZ to "XXXzXXX", where z = time zone tz (input parameter)
      static char input_TZ[13], ctz[5];
      sprintf(ctz,"%d",-tz);
      strcpy(input_TZ,"TZ=XXX");
      strcat(input_TZ,ctz);
      strcat(input_TZ,"XXX");

      putenv(input_TZ);  // set TZ environment variable (time zone) to tz
      my_time_t = mktime(&my_time_tm);     // get calendar time for tz
      putenv(local_TZ);  // restore TZ to local time zone 
   }
  
   // mktime converts local time to calendar time (time_t)
}

/*......................................................................*/
void TimeValue::set_local_time(int fract, int sec, int min, int hour,
              int day_of_year, int year)
{
   int i, leap, day, month;
   static char daytab[2][13] = {
      {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
      {31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
   };

   leap = year%4 == 0 && year%100 !=0 || year%400 == 0;
   for (i=0; day_of_year > daytab[leap][i]; i++)
      day_of_year = day_of_year - daytab[leap][i];

   month = i;
   day = day_of_year;

   struct tm my_time_tm;

   t_fract = fract;
   my_time_tm.tm_sec = sec;
   my_time_tm.tm_min = min;
   my_time_tm.tm_hour = hour;
   my_time_tm.tm_mday = day;
   my_time_tm.tm_mon = month;
   my_time_tm.tm_year = year-1900;
   my_time_tm.tm_isdst = -1;

   my_time_t = mktime(&my_time_tm);   // assume local time
   set_dst(my_time_tm.tm_isdst);      // set daylight saving flag to compare later

   // mktime converts local time to calendar time (time_t)
}

/*......................................................................*/
void TimeValue::set_time(time_t seconds, int fract)
{
   t_fract = fract;
   my_time_t = seconds;
}

/*......................................................................*/
void TimeValue::set_dst(int dstflag)
{
   my_dst = dstflag;
}

/*......................................................................*/
void TimeValue::get_dst(int *dstflag)
{
   *dstflag = my_dst;
}

/*......................................................................*/
void TimeValue::get_calendar_time(time_t *seconds, int *fract)
{
   *fract = t_fract;
   *seconds = my_time_t;
}

/*......................................................................*/
void TimeValue::get_gm_time(int *fract, int *sec, int *min, int *hour,
              int *day, int *month, int *year, int *tz, int *dst)
{
   struct tm *my_time_tm;
   my_time_tm = gmtime(&my_time_t);

   *fract = t_fract;
   *sec = my_time_tm->tm_sec;
   *min = my_time_tm->tm_min;
   *hour = my_time_tm->tm_hour;
   *day = my_time_tm->tm_mday;
   *month = my_time_tm->tm_mon + 1;
   *year = my_time_tm->tm_year + 1900;
   *tz = 0;
   *dst = my_time_tm->tm_isdst;
}

/*......................................................................*/
void TimeValue::get_gm_time(char *timestamp)
{
   USHORT ff, yy;
   struct tm *my_time_tm;
   my_time_tm = gmtime(&my_time_t);

   yy = my_time_tm->tm_year + 1900;
   memcpy(timestamp,&yy,2);
   timestamp[2] = my_time_tm->tm_mon + 1;
   timestamp[3] = my_time_tm->tm_mday;
   timestamp[4] = my_time_tm->tm_hour;
   timestamp[5] = my_time_tm->tm_min;
   timestamp[6] = my_time_tm->tm_sec;
   timestamp[7] = 0xff;
   ff = (unsigned short) (t_fract);
   memcpy(&timestamp[8],&ff,2);
   timestamp[10] = 0;
   timestamp[11] = my_time_tm->tm_isdst;
   timestamp[12] = 0x00;
   timestamp[13] = 0x00;
   timestamp[14] = 0x00;
   timestamp[15] = 0x00;
}

/*......................................................................*/
void TimeValue::get_local_time(int *fract, int *sec, int *min, int *hour,
              int *day, int *month, int *year, int *tz, int *dst)
{
   struct tm *my_time_tm;
   my_time_tm = localtime(&my_time_t);

   *fract = t_fract;
   *sec = my_time_tm->tm_sec;
   *min = my_time_tm->tm_min;
   *hour = my_time_tm->tm_hour;
   *day = my_time_tm->tm_mday;
   *month = my_time_tm->tm_mon + 1;
   *year = my_time_tm->tm_year + 1900;
   *dst = my_time_tm->tm_isdst;

   my_time_tm = gmtime(&my_time_t);
   *tz = *hour - my_time_tm->tm_hour - *dst;
}

/*......................................................................*/
void TimeValue::get_local_time(int *fract, int *sec, int *min, int *hour,
              int *day_of_year, int *year, int *tz, int *dst)
{
   struct tm *my_time_tm;
   int day, month, i, leap;    // day, month, index, leap flag
   int old_dst;                // old daylight saving flag

   my_time_tm = localtime(&my_time_t);

   *fract = t_fract;
   *sec = my_time_tm->tm_sec;
   *min = my_time_tm->tm_min;
   *hour = my_time_tm->tm_hour;
   *day_of_year = my_time_tm->tm_yday+1; // starts at 0 for Jan. 1
   *year = my_time_tm->tm_year + 1900;
   *dst = my_time_tm->tm_isdst;

   get_dst(&old_dst);        // get old daylight saving flag

   // if daylight saving flag is changed, 1 hour need to be added/subtracted
   if (*dst != old_dst) {
      if (*dst > old_dst)
         *hour -= 1;
      else 
         *hour += 1;
   }

   my_time_tm = gmtime(&my_time_t);
}

/*......................................................................*/
void TimeValue::get_local_time(char *timestamp)
{
   USHORT ff, yy;
   struct tm *my_time_tm;
   my_time_tm = localtime(&my_time_t);

   yy = my_time_tm->tm_year + 1900;
   memcpy(timestamp,&yy,2);
   timestamp[2] = my_time_tm->tm_mon + 1;
   timestamp[3] = my_time_tm->tm_mday;
   timestamp[4] = my_time_tm->tm_hour;
   int local_hour = my_time_tm->tm_hour;

   timestamp[5] = my_time_tm->tm_min;
   timestamp[6] = my_time_tm->tm_sec;
   timestamp[7] = 0xff;
   ff = (unsigned short) (t_fract);
   memcpy(&timestamp[8],&ff,2);
   timestamp[11] = my_time_tm->tm_isdst;
   int local_isdst = my_time_tm->tm_isdst;

   timestamp[12] = 0x00;
   timestamp[13] = 0x00;
   timestamp[14] = 0x00;
   timestamp[15] = 0x00;

   my_time_tm = gmtime(&my_time_t);
   timestamp[10] = local_hour - my_time_tm->tm_hour - local_isdst;
}

/*......................................................................*/
void TimeValue::what_time_is_it_here(int *fract, int *sec, int *min, 
      int *hour, int *day, int *month, int *year, int *tz, int *dst)
{
   timeval tv;
   gettimeofday(&tv, 0);
   t_fract = (int)(float)(tv.tv_usec/100.);

   struct tm *my_time_tm;
   my_time_tm = localtime(&tv.tv_sec);

   *fract = t_fract;
   *sec = my_time_tm->tm_sec;
   *min = my_time_tm->tm_min;
   *hour = my_time_tm->tm_hour;
   *day = my_time_tm->tm_mday;
   *month = my_time_tm->tm_mon + 1;
   *year = my_time_tm->tm_year + 1900;
   *dst = my_time_tm->tm_isdst;

   my_time_tm = gmtime(&tv.tv_sec);
   *tz = *hour - my_time_tm->tm_hour - *dst;
}

/*......................................................................*/
void TimeValue::what_time_is_it_here(char *timestamp)
{
   timeval tv;
   gettimeofday(&tv, 0);
   t_fract = (int)(float)(tv.tv_usec/100.);

   USHORT ff, yy;
   struct tm *my_time_tm;
   my_time_tm = localtime(&tv.tv_sec);

   yy = my_time_tm->tm_year + 1900;
   memcpy(timestamp,&yy,2);
   timestamp[2] = my_time_tm->tm_mon + 1;
   timestamp[3] = my_time_tm->tm_mday;
   timestamp[4] = my_time_tm->tm_hour;
   int local_hour = my_time_tm->tm_hour;

   timestamp[5] = my_time_tm->tm_min;
   timestamp[6] = my_time_tm->tm_sec;
   timestamp[7] = 0xff;
   ff = (unsigned short) (t_fract);
   memcpy(&timestamp[8],&ff,2);
   timestamp[11] = my_time_tm->tm_isdst;
   int local_isdst = my_time_tm->tm_isdst;

   timestamp[12] = 0x00;
   timestamp[13] = 0x00;
   timestamp[14] = 0x00;
   timestamp[15] = 0x00;

   my_time_tm = gmtime(&tv.tv_sec);
   timestamp[10] = local_hour - my_time_tm->tm_hour - local_isdst;
}

/*......................................................................*/
int TimeValue::time_cmp(TimeValue *t)
// Compares TimeValue of object to that of input time *t.  
// Returns -1 if TimeValue of object < that of *t; 
// Returns  0 if TimeValue of object = that of *t; 
// Returns +1 if TimeValue of object > that of *t; 
{
   double diff;
   int icmp;
   diff = my_time_t + (double)t_fract/10000. 
          - t->my_time_t - (double)t->t_fract/10000.;
   if(diff == 0) icmp= 0;
   if(diff > 0) icmp= 1;
   if(diff < 0) icmp= -1;
   return icmp;
}

/*......................................................................*/
double TimeValue::elapsed_time(TimeValue *t)
{
   double diff = 0;
   diff = my_time_t + (double)t_fract/10000. 
          - t->my_time_t - (double)t->t_fract/10000.;
   return diff;
}   

/******* End of TimeValue *********************************************/

/******* Start of SC_EVENT_TIME *************************/

void ScEventTime::set_scet_time(char *scet) 
{
	int year, doy, hr, min, sec, msec;
	get_components(scet, &year, &doy, &hr, &min, &sec, &msec);
	msec *= 10;
	set_local_time(msec, sec, min, hr, doy, year);
}

void ScEventTime::set_scet_time(int year, int doy, int hr, int min, int sec,
                                int msec)
{
	msec *= 10;
	set_local_time(msec, sec, min, hr, doy, year);
}

char *ScEventTime::get_scet_time()
{
	static char scet[25];
	int fract, sec, min, hr, doy, year, tz, dst;
	get_local_time(&fract, &sec, &min, &hr, &doy, &year, &tz, &dst);

	memset(scet, '\0', sizeof(scet));
        int msec = (int)(fract/10.0+0.5);
        if (msec >= 1000) {
           sec += 1;
           msec %= 1000;
        }

	sprintf(scet, "%04d/%03d-%02d:%02d:%02d.%03d", year, doy, hr, min, 
						sec, msec);
	return scet;
}

/*
char *ScEventTime::get_scet_time(TimeUnits::units tu)
{
	static char scet[25];

	int fract, sec, min, hr, doy, year, tz, dst;
	get_local_time(&fract, &sec, &min, &hr, &doy, &year, &tz, &dst);

	memset(scet, '\0', sizeof(scet));
	int msec = (int)(fract/10);

	switch(tu) {
	   case TimeUnits::YEAR:
	      sprintf(scet, "%04d", year);
	      break;
	   case TimeUnits::DOY:
	      sprintf(scet, "%04d-%03d", year, doy);
	      break;
	   case TimeUnits::HR:
	      sprintf(scet, "%04d-%03dT%02d", year, doy, hr);
	      break;
   	   case TimeUnits::MIN:
	      sprintf(scet, "%04d-%03dT%02d:%02d", year, doy, hr, min);
	      break;
 	   case TimeUnits::SEC:
	      sprintf(scet, "%04d-%03dT%02d:%02d:%02d", year, doy, hr, min, 
					sec);
	      break;
	   case TimeUnits::MSEC:
	      sprintf(scet, "%04d-%03dT%02d:%02d:%02d.%02d", year, doy, hr, 
					min, sec, msec);
	      break;
	}

	return scet;
}
*/

void ScEventTime::get_scet_time(int *year, int *doy, int *hr, int *min, 
						int *sec, int *msec)
{
	int tz, dst;
	get_local_time(msec, sec, min, hr, doy, year, &tz, &dst);
	*msec /= 10;
}

double ScEventTime::get_time_in_sec()
{
	time_t sec; 
	int fract;
	get_calendar_time(&sec, &fract);
        
	char tmp[30];
	sprintf(tmp, "%ld.%.4d", sec, fract);
	return atof(tmp);
} 

void ScEventTime::set_time_in_sec(double sec)
{
	time_t s = (time_t)sec;
	int fract = (int)((sec-s) * 10000);
	set_time(s, fract);
}

double ScEventTime::get_time_in_days()
{
	double sec = get_time_in_sec();
	return sec / 86400.0;
} 

void ScEventTime::set_time_in_days(double days)
{
	set_time_in_sec(days * 86400.0);
}

void ScEventTime::scet_to_time(char *scet, int *year, int *doy, int *hr, 
				int *min, int *sec, int *msec)
{
	get_components(scet, year, doy, hr, min, sec, msec);

	int tz, dst;
	int fract = *msec*10;
	set_local_time(fract, *sec, *min, *hr, *doy, *year);
	get_local_time(&fract, sec, min, hr, doy, year, &tz, &dst);
	*msec = (int)(fract / 10);
}

double ScEventTime::get_year()
{
	int y, d, h, mn, s, ms;
	get_scet_time(&y, &d, &h, &mn, &s, &ms);
	double fract = d / 365.0;
	return y+fract;
}

double ScEventTime::get_doy()
{
	int y, d, h, mn, s, ms;
	get_scet_time(&y, &d, &h, &mn, &s, &ms);
	double fract = h / 24.0;
	return d+fract;
}

double ScEventTime::get_hour()
{
	int y, d, h, mn, s, ms;
	get_scet_time(&y, &d, &h, &mn, &s, &ms);
	double fract = mn / 60.0;
	return h+fract;
}

double ScEventTime::get_min()
{
	int y, d, h, mn, s, ms;
	get_scet_time(&y, &d, &h, &mn, &s, &ms);
	double fract = s / 60.0;
	return mn+fract;
}

double ScEventTime::get_sec()
{
	int y, d, h, mn, s, ms;
	get_scet_time(&y, &d, &h, &mn, &s, &ms);
	double fract = ms / 1000.0;
	return s+fract;
}

void ScEventTime::year_to_year_doy(double year_in, int *year_out, int *doy)
{
	*year_out = (int)year_in;
	double fract = year_in - *year_out;
	*doy = (int)(fract * 364);	 // 0 to 364
}

void ScEventTime::doy_to_doy_hr(double doy_in, int *doy_out, int *hr)
{
	*doy_out = (int)doy_in;
	double fract = doy_in - *doy_out;
	*hr = (int)(fract * 23);	 // 0 to 23
} 

void ScEventTime::hour_to_hour_min(double hour_in, int *hour_out, int *min)
{
	*hour_out = (int)hour_in;
	double fract = hour_in - *hour_out;
	*min = (int)(fract * 59);	 // 0 to 59
}

void ScEventTime::min_to_min_sec(double min_in, int *min_out, int *sec)
{
	*min_out = (int)min_in;
	double fract = min_in - *min_out;
	*sec = (int)(fract * 59);	// 0 to 59
}

void ScEventTime::sec_to_sec_msec(double sec_in, int *sec_out, int *msec)
{
	*sec_out = (int)sec_in;
	double fract = sec_in - *sec_out;
	*msec = (int)(fract * 1000);
}


void ScEventTime::get_components(char *s, int *year, int *doy, int *hr, 
				int *min, int *sec, int *msec)
{
	*year = 0; *doy = 0; *hr = 0; *min = 0; *sec = 0; *msec = 0;

        char *p = s;
        int ct = 0;
        char tmp[6];
        while (1) {
           if (*p == '-' || *p == 'T' || *p == ':' || *p == '.' || *p == '\0') {
              switch(ct) {
                case 0:
                   strncpy(tmp, p-4, 4);
                   tmp[4] = '\0';
                   *year = atoi(tmp);
                   ct++;
                   break;
                case 1:
                   strncpy(tmp, p-3, 3);
                   tmp[3] = '\0';
                   *doy = atoi(tmp);

                   /* Check if leapyear, doy can be 0-366 */
                   if (((*year % 4) == 0) && ((*year % 100) != 0) || ((*year % 400) == 0)) {
                      if (*doy > 366) {
                         std::cerr << "ERROR: DOY cannot be larger than 366!!!\n";
                         exit(0);
                      }
                   }
                   else { /* if year is not leapyear, doy can be 0-365 */
                      if (*doy > 365) {
                         std::cerr << "ERROR: DOY cannot be larger than 365!!!\n";
                         exit(0);
                      }
                   }
                   ct++;
                   break;
                case 2:
                   strncpy(tmp, p-2, 2);
                   tmp[2] = '\0';
                   *hr = atoi(tmp);
                   if (*hr > 24) {
                      std::cerr << "ERROR: HR cannot be larger than 24!!!\n";
                      exit(0);
                   }
                   else
                      ct++;
                   break;
                case 3:
                   strncpy(tmp, p-2, 2);
                   tmp[2] = '\0';
                   *min = atoi(tmp);
                   if (*min > 60) {
                      std::cerr << "ERROR: MIN cannot be larger than 60!!!\n";
                      exit(0);
                   }
                   else
                      ct++;
                   break;
                case 4:
                   strncpy(tmp, p-2, 2);
                   tmp[2] = '\0';
		   *sec = atoi(tmp);
                   if (*sec > 60) {
                      std::cerr << "ERROR: SEC cannot be larger than 60!!!\n";
                      exit(0);
                   }
                   else
		      ct++;
		   break;
		case 5:
                   strncpy(tmp, p-3, 3);
                   tmp[3] = '\0';
                   *msec = atoi(tmp);
                   break;
              }
           }
	   if (*p == '\0') break;
           p++;
        }
}
