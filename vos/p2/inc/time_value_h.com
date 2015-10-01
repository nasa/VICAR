$!****************************************************************************
$!
$! Build proc for MIPL module time_value_h
$! VPACK Version 1.9, Monday, December 07, 2009, 15:59:02
$!
$! Execute by entering:		$ @time_value_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module time_value_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to time_value_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("time_value_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @time_value_h.bld "STD"
$   else
$      @time_value_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create time_value_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack time_value_h.com -mixed -
	-s time_value.h ScetCoeff.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create time_value.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ScetCoeff.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/***********************************************************************
* ScetCoeff.h     
* Include file for use with sclk2scet & scet2sclk programs.
* History:
*    March 29, 2001 (Hyun H. Lee)
*       Added get_values_with_scet(...) method. 
*       Renamed get_values(...) to get_values_with_sclk(...).  
***********************************************************************/

#ifndef _SCETCOEFF_
#define _SCETCOEFF_
#include <iostream>
#include <iomanip>
#include <strings.h>
#include <fstream>
#include <errno.h>

class ScetCoeff {
private:
        double *sclk0;
        double *sclkrate;
        char **scet0;
        int num;
public:
        // constructors
        ScetCoeff() : sclk0(0), sclkrate(0), scet0(0), num(0) {}
        ScetCoeff(char *filename) { load_coeff(filename); }

        // function to load coefficient file 
        int load_coeff(char *filename);

        // function to get the values with sclk as input 
        // input is non-converted sclk0; return 0 on success -1 on failure
        int get_values_with_sclk(double insclk0, double &sclk0, double &rate, 
			  	 char *scet0);
  
        // function to get the values with scet as input
        // input is scet_in; return 0 on success -1 on failure
        int get_values_with_scet(char *scet_in, double &sclk0, double &rate,
                                 char *scet0);
	void reset();

        // destructor
        ~ScetCoeff() { reset(); }
};

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
