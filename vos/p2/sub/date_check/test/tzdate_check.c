#include "vicmain_c.h"
#include "date_check.h"

void main44 ()
{
   int scet_date[6], std_date[7], count;

   zvp ("SCET",scet_date, &count);
   if (count == 6)
   {
      if (zchk_scet_date (scet_date)) 
         zvmessage ("zchk_scet_date: Valid date", "");
      else
         zvmessage ("zchk_scet_date: Invalid date","");

      if (zvptst("DEBUG"))
      {
         zvmessage ("--- Check All Fields ---","");
         if (zchk_year(scet_date[0]))
            zvmessage ("zchk_year: Valid year","");
         else
            zvmessage ("zchk_year: Invalid year","");

         if (zchk_julian(scet_date[0],scet_date[1]))
            zvmessage ("zchk_julian: Valid julian day","");
         else
            zvmessage ("zchk_julian: Invalid julian day","");

         if (zchk_hour(scet_date[2]))
            zvmessage ("zchk_hour: Valid hour number", "");
         else
            zvmessage ("zchk_hour: Invalid hour number", "");

         if (zchk_minute (scet_date[3]))
            zvmessage ("zchk_minute: Valid minute number", "");
         else
            zvmessage ("zchk_minute: Invalid minute number", "");
 
         if (zchk_second (scet_date[4]))
            zvmessage ("zchk_second: Valid second number", "");
         else
            zvmessage ("zchk_second: Invalid second number", "");
 
         if (zchk_msec (scet_date[5]))
            zvmessage ("zchk_msec: Valid millisecond  number", "");
         else
            zvmessage ("zchk_msec: Invalid millisecond number", "");
      } 

   }
   zvp ("STD",std_date, &count);
   if (count == 7)
   {
      if (zchk_std_date (std_date))
         zvmessage ("zchk_std_date: Valid date", "");
      else
         zvmessage ("zchk_std_date: Invalid date", "");

      if (zvptst("DEBUG"))
      {
         zvmessage ("--- Check All Fields ---","");
         if (zchk_year(std_date[0]))
            zvmessage ("zchk_year: Valid year","");
         else
            zvmessage ("zchk_year: Invalid year","");
 
         if (zchk_month(std_date[1]))
            zvmessage ("zchk_month: Valid month","");
         else
            zvmessage ("zchk_month: Invalid month","");

         if (zchk_day(std_date[0], std_date[1], std_date[2]))
            zvmessage ("zchk_day: Valid day","");
         else
            zvmessage ("zchk_day: Invalid day","");
 
         if (zchk_hour(std_date[3]))
            zvmessage ("zchk_hour: Valid hour number", "");
         else
            zvmessage ("zchk_hour: Invalid hour number", "");
 
         if (zchk_minute (std_date[4]))
            zvmessage ("zchk_minute: Valid minute number", "");
         else
            zvmessage ("zchk_minute: Invalid minute number", "");
 
         if (zchk_second (std_date[5]))
            zvmessage ("zchk_second: Valid second number", "");
         else
            zvmessage ("zchk_second: Invalid second number", "");
 
         if (zchk_msec (std_date[6]))
            zvmessage ("zchk_msec: Valid millisecond  number", "");
         else
            zvmessage ("zchk_msec: Invalid millisecond number", "");
      }

   }
}

