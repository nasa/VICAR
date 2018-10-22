/**********************************************************************
* time_value.h  2/21/2001
* 
* Include file for subroutine time_value.com 
*
**********************************************************************/
#ifndef _Time_Value_H_
#define _Time_Value_H_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

typedef unsigned short int USHORT;

class TimeValue {

    protected:

        time_t my_time_t;
        int t_fract;  // units of 100 microseconds
        int my_dst;   // daylight saving flag

    public:

    /*  TimeValue::set_time - sets the calendar time (my_time_t) and fraction of 
    a second (t_fract) equal to the time specified by the input parameters.
    The input parameter dst, when specified, is ignored.  The correct value 
    of dst is determined and used instead.  If the default value (99) of tz 
    is used, the local time zone is assumed.  Note that tz is negative in 
    the Western Hemisphere.  */

    void set_time(int fract, int sec, int min, int hour, int day,
                  int month, int year, int tz=99, int dst=99);
 
    void set_time(time_t seconds, int fract);

    void set_local_time(int fract, int sec, int min, int hour, 
                        int day_of_year, int year);

    void set_dst (int dstflag);
    void get_dst (int *dstflag);
 
    void get_calendar_time(time_t *seconds, int *fract);

    void get_gm_time(int *fract, int *sec, int *min, int *hour,
         	     int *day, int *month, int *year, int *tz, int *dst);

    void get_gm_time(char *timestamp);

    void get_local_time(int *fract, int *sec, int *min, int *hour,
     		        int *day, int *month, int *year, int *tz, int *dst);

    void get_local_time(int *fract, int *sec, int *min, int *hour,
   		        int *day_of_year, int *year, int *tz, int *dst);

    void get_local_time(char *timestamp);

    void what_time_is_it_here(int *fract, int *sec, int *min, int *hour,
                              int *day, int *month, int *year, int *tz, int *dst);

    void what_time_is_it_here(char *timestamp);

    double elapsed_time(TimeValue *t);

    int time_cmp(TimeValue *t);

    // Constructors
    TimeValue(){}

    // Destructor
    ~TimeValue(){}
};

/****Class SC_EVENT_TIME ** used for keeping tract of spacecraft time ***/

class ScEventTime : public TimeValue {
   private:
      void get_components(char *s, int *year, int *doy, int *hr, int *min,
				     int *sec, int *msec);
   public:

      void set_scet_time(char *scet);
      void set_scet_time(int year, int doy, int hr, int min, int sec,
        	    	 int msec);
      
      // these next 2 return a pointer on the stack, be sure to copy the
      // values if necessary
      char *get_scet_time();   	// get complete SCET 

      void get_scet_time(int *year, int *doy, int *hr, int *min, int *sec,
  		         int *msec);

      // gets the current scet time in seconds
      double get_time_in_sec();
      void set_time_in_sec(double sec);

      // get the current scet time in days
      double get_time_in_days();
      void set_time_in_days(double days);

      void scet_to_time(char *scet, int *year, int *doy, int *hr, int *min, 
  		        int *sec, int *msec);

      double get_year();
      double get_doy();
      double get_hour();
      double get_min();
      double get_sec();

      static void year_to_year_doy(double year_in, int *year_out, int *doy); 
      static void doy_to_doy_hr(double doy_in, int *doy_out, int *hr); 
      static void hour_to_hour_min(double hour_in, int *hour_out, int *min); 
      static void min_to_min_sec(double min_in, int *min_out, int *sec); 
      static void sec_to_sec_msec(double sec_in, int *sec_out, int *msec); 
};

#endif
