#include <stdio.h>
#include <string.h>


int read_info1 (label)
   char  *label;
{
   int length, day, month, year, count, date;

   if ((length = strlen(label)) >= 20) {
      if (label[5] == 'D')
         count = sscanf(&label[14],"%d/%d/%d", &month, &day, &year);
      else
         count = sscanf(&label[17],"%d/%d/%d", &month, &day, &year);

      if (count == 3)
         date = 10000 * year + 100 * month + day;
      else {
         l_message (" read_info1 error:");
         l_message ("  Unable to read date from the label string.");
         date = 0;
       }
   }
   else {
      l_message (" read_info1 error:");
      l_message ("  Label string too short to have date in INFO1 format");
      l_message ("  date (mm/dd/yy) must start on or past char 15 or 18");
      date = 0;
   }

   return date;
}

