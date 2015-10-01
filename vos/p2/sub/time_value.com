$!****************************************************************************
$!
$! Build proc for MIPL module time_value
$! VPACK Version 1.9, Monday, December 07, 2009, 16:38:34
$!
$! Execute by entering:		$ @time_value
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
$ write sys$output "*** module time_value ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to time_value.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$   if F$SEARCH("time_value.imake") .nes. ""
$   then
$      vimake time_value
$      purge time_value.bld
$   else
$      if F$SEARCH("time_value.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake time_value
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @time_value.bld "STD"
$   else
$      @time_value.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create time_value.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack time_value.com -mixed -
	-s time_value.cc ScetCoeff.cc -
	-i time_value.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create time_value.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ScetCoeff.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/***********************************************************************
*  ScetCoeff.cc  
*
*  Subroutines for sclk2scet & scet2sclk programs.
*
*  History:
*      March 29, 2001 (Hyun H. Lee)
*         Separated ScetCoeff implementation from sclk2scet.
*         Added get_values_with_scet method.
*         Renamed get_values to get_values_with_sclk.
***********************************************************************/
#include "time_value.h"
#include "ScetCoeff.h"
#include <iomanip>
#include <math.h>

void ScetCoeff::reset()
{
        delete[] sclk0; delete[] sclkrate;
        for (int i = 0; i < num; i++) delete scet0[i];
        delete[] scet0;
        num = 0;
}

int ScetCoeff::load_coeff(char *filename)
{
   char buffer[100], *p;

   std::ifstream infile(filename);
   if (!infile ) {
     std::cout << "Unable to load file " << filename << std::endl;
      return -1;
   }

   int get_rec = 0;
   int ast = 0;
   int ct = 0;
   std::streampos pos;
   while (infile.getline(p = buffer, sizeof(buffer))) {
      if (*p == '*') {
         ast = 1;
         pos = infile.tellg();
      }
      else if (ast) {
         get_rec = 1;
         ast = 0;
      }

      if (get_rec && *p != 'C') ct++;
   }
   if (!ct) {
     std::cout << "No Records found in Coefficient File" << std::endl;
      return -1;
   }
   reset();

   infile.close();
   infile.open(filename);
   infile.seekg(pos);
   sclk0 = new double[ct];
   sclkrate = new double[ct];
   scet0 = new char*[ct];
   num = ct;
   for (int i = 0; i < num; i++) {
      scet0[i] = new char[22];
      infile.getline(p = buffer, sizeof(buffer));
      int ct = 0;
      char *str;
      while ((str = strtok(p, " "))) {
         switch(ct) {
            case 0:
                     sclk0[i] = atof(str);
                     break;
            case 1:

                     strcpy(scet0[i], str);
                     break;
            case 3:
                     sclkrate[i] = atof(str);
                     break;
            default:
                     break;
                     // do nothing
         } // end switch
         ct++;
         p = 0;
      } // end while
   } // end for

   return 0;
}

int ScetCoeff::get_values_with_sclk(double insclk0, double &sclk0_out,
                                           double &rate_out, char *scet0_out)
{
   int ret = 0;
   int i;
   for (i = 0; i < num; i++) {
      if (sclk0[i] > insclk0) break;
   }

   if (i == 0) ret = -1;
   else {
      int inx = i-1;
      int d = (int)sclk0[inx];  // get whole number
      double f = ((sclk0[inx] - d) * 1000);     // get fraction
      /*  You can't use an int here.  The int is truncated and you loose precision.   */
      /*  int f = (int)((sclk0[inx] - d) * 1000);       // get fraction */
      double df = f / 256.0;
      sclk0_out = d + df;
      rate_out = sclkrate[inx];
      strcpy(scet0_out, scet0[inx]);
   }
   return ret;
}

int ScetCoeff::get_values_with_scet(char *scet0_in, double &sclk0_out,
                                           double &rate_out, char *scet0_out)
{

   int ret = 0;
   ScEventTime stime;
   stime.set_scet_time(scet0_in);
   double in_sec = stime.get_time_in_sec();

   int i;
   for (i=0; i<num; i++) {
      stime.set_scet_time(scet0[i]);
      double out_sec = stime.get_time_in_sec();

      if (out_sec > in_sec) break;
   }

   if (i == 0) ret = -1;
   else {
      int inx = i-1;
      int d = (int)sclk0[inx];  // get whole number
      double f = ((sclk0[inx] - d) * 1000);     // get fraction
      /*  You can't use an int here.  The int is truncated and you loose precision.   */
      /*  int f = (int)((sclk0[inx] - d) * 1000);       // get fraction */
      double df = f / 256.0;
      sclk0_out = d + df;
      rate_out = sclkrate[inx];
      strcpy(scet0_out, scet0[inx]);
   }
   return ret;
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create time_value.imake
#define SUBROUTINE time_value
#define MODULE_LIST time_value.cc ScetCoeff.cc

/* #define DEVELOPMENT */

#define P2_SUBLIB

#define MAIN_LANG_C_PLUS_PLUS
#define USES_C_PLUS_PLUS

#define LIB_P2SUB

#ifdef DEVELOPMENT

#define LIB_LOCAL
#define DEBUG

#endif


$ Return
$!#############################################################################
