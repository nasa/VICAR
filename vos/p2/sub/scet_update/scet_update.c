/***********************************************************************/
/* SCET_UPDATE subroutine for detect/correct invalide SCET input.      */
/* Seet scet_update.hlp for detail.                                    */
/*                                                                     */
/* History:                                                            */
/*     Aug. 25, 1998  .T.Huang.   Initial release.                     */
/*                                                                     */
/***********************************************************************/

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "date_check.h"
#include <string.h>
#include <stdio.h>

int is_null ();
int time_checker ();

/* define FORTRAN interface */
void FTN_NAME2_(scet_update, SCET_UPDATE) (char *project, void *data, int *ind,
								ZFORSTR_PARAM)
{
   ZFORSTR_BLOCK
   char proj[6];
   int length=5;

   zsfor2c(proj, length, project, &project, 3, 1, 1, ind);
   *ind = zscet_update (proj, data);
}


/* implementation of scet_update */
int zscet_update (project, data)
   char *project;
   int  data[80];
{
   int  ind;

   /* initialize temp time buffer */
   int temp[6];

/**
   zvmessage ("************* SCET_UPDATE Begin *************","");
   zvmessage ("scet_update::Checking SCET entry....","");
**/
   temp[0] = data[7];
   temp[1] = data[8];
   temp[2] = data[9];
   temp[3] = data[10];
   temp[4] = data[11];
   temp[5] = data[12];

   if (!is_null(temp))
   {
      /* checking and/update scet */
      if (time_checker (project,temp))
      {
         /* update input buffer scet entry */
         data[7] = temp[0];
         data[12] = temp[5];
      }
      else
         zabend ();
   }
   else
      zmabend ("scet_update::>>> Error, SCET cannot be NULL.");
/**  
   zvmessage ("",""); 
   zvmessage ("scet_update::Checking ERT entry.","");
**/
   temp[0] = data[65];
   temp[1] = data[66];
   temp[2] = data[67];
   temp[3] = data[68];
   temp[4] = data[69];
   temp[5] = -999;  /* NULL flag. ERT has no msec */

   /* allow ERT to be null */
   if (!is_null(temp))
   {
      temp[5] = 0;  /* set to zero, so date_check routine will ignor it */
      /* checking and/update scet, and allow for NULL date input */
      if (time_checker (project,temp))
         /* update input buffer ert entry */
         data[65] = temp[0];
      else
         zabend ();
   }
/**
   zvmessage ("************** SCET_UPDATE End **************","");
**/
   return 1;
}



/* subroutine to check for default date buffer 
   Return values:
      1  true
      0  false
*/
int is_null (tbuf)
   int tbuf[6];
{
   return (tbuf[0]==tbuf[1] &&
           tbuf[1]==tbuf[2] &&
           tbuf[2]==tbuf[3] &&
           tbuf[3]==tbuf[4] &&
           tbuf[4]==tbuf[5] &&
           tbuf[5]==-999);
}



/* subroutine to perform date checking and update when necessary. 
   Return values:
       1  Valid date
       0  Invalid date
*/
int time_checker (project,tbuf)
   char *project;
   int  tbuf[6];
{
   char msg[80];

   /* invoke date_check subroutine */
   if (!zchk_scet_date (tbuf))
      /* correct Voyager date */
      if (!strcmp(project,"VGR-1") || !strcmp(project,"VGR-2"))
      {
         sprintf (msg,"time_checker::Correcting %s year input...", project);
/**
         zvmessage (msg,"");
**/
         /* check for year between 2000 -- 2074 */
         if (tbuf[0] < 75 && tbuf[0] >= 0)
         {
            tbuf[0] = tbuf[0] + 2000;
            tbuf[5] = 0;
            /* performs final check on the corrected date */
            if (!zchk_scet_date (tbuf)) return 0;
         }

         else if (tbuf[0] > 75 && tbuf[0] < 100)
         {
            /* check for year between 1975 -- 1999 */
            tbuf[0] = tbuf[0] + 1900;
            tbuf[5] = 0;
            /* performs final check on the corrected date */
            if (!zchk_scet_date (tbuf)) return 0;
         }

         else
         {
            sprintf (msg, 
               "time_checker::>>> Invalid %s date format!!!", project);
/**
            zvmessage (msg,"");
**/
            return 0;
         }
      }

      else
      {
         sprintf (msg, "time_checker::>>> Invalid %s date format!!!", project);
         zvmessage (msg,"");
         return 0;
      }
   return 1;
}

