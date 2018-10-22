#include "vicmain_c"

#include  <stdio.h>
#include <string.h>

/**********************************************************************/
/* PROGRAM: comment                                                   */
/**********************************************************************/
/*                                                                    */
/*   Program "comment " is a VICAR applications program which types   */
/*   current local date and either types the input comment into the   */
/*   log for the VICAR session or prompts the operator to enter a     */
/*   comment to be included in the session log.                       */
/**********************************************************************/
/*                                                                    */
/* HISTORY:                                                           */
/*                                                                    */
/*   Converted to 'C' and ported to UNIX  by CRI          Jan   95    */
/*   Prepared in Fortran-77 for MIPL by Steve Pohorsky    Sept  84    */
/*                                                                    */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*                                                                    */
/*   Input args : message                                             */
/*                                                                    */
/*   Output args: None                                                */
/*                                                                    */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/* MAIN PROGRAM                                                       */
/*                                                                    */
/**********************************************************************/


/*comment(message)
char message[80]; */
void main44()
{
    long  time(), t;
    int	  i,status,num_values,defaults_used;
    char  *date_time, *ctime();
    char  aday[3], amonth[4], ayear[5], adate[8];
    char  line[80];
    char  bar[25];
    char  message[80];
    char  c;

    zifmessage("COMMENT version 13-NOV-98");
    status = zvparm ("MESSAGE",message,&num_values,&defaults_used,1,80);
                                        /* Get the message to display   */
    strcpy(bar,"--------------------");
    zvmessage(bar,"");
    strcpy(bar,"\n--------------------");
    /*               Get current time                                 */

    t = time(0);

    /*               Convert to Local in ASCII String                 */

    date_time = ctime(&t); 

    /*               Gather date and time pieces                      */

    strncpy(ayear, (date_time + 20), 4);
    strncpy(amonth, (date_time + 4), 3);
    strncpy(aday, (date_time + 8), 2);
    strncpy(adate, (date_time +11), 8);

    sprintf(line, "\nDate: %.2s-%.3s-%.4s    Time: %.8s\0\n",
            aday,amonth,ayear,adate);

    zvmessage(line, "");
    if (defaults_used == 1)
      {
        printf("comment: ");           /*    Request a one line comment  */
        gets(message);   
       }
     else
       {
         if (message[0] == ' ')
            for ( i = 0; i < 80 ; i++)
               message[i]= message[i+1];       
         if (message[0] !='\0')
               zvmessage(message,""); 
       }
     zvmessage(bar,"");
} 
