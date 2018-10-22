
/* Revision History */
/* 3-97 SXP Initial release. Simple, but still for the glory of God.*/
/*          Corrected by Thomas Huang.  Added call to zvfilename with
            enlarged full_name per Bob Deen.*/

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    250            /* buffer length for file name.     */
#define MAXR2    2*MAXR1        /* buffer length for full file name */   

/*    also used for zvmessage max message length.  */
   int   count;                  /* used by zvp to get parameters    */
   int   len, status;
   FILE  *textfile;              /* file pointer to text file        */
   char  buffer[MAXR1];          /* buffer for text.                 */
   char  file_name[MAXR1];       /* name of file to be TYPEd.        */
   char full_name[MAXR2+1];	 /* full file path name		    */
/*  ==================================================================  */

/* Retrieve the user specified filename.*/

   zvp("FILE", file_name, &count);
   zvfilename(file_name,full_name,MAXR2);

/* Open the text file for read-only. */

   textfile = fopen(full_name, "r");
   if (textfile == NULL) {
       sprintf(buffer, "ERROR OPENING text FILE '%s'", file_name);
       zmabend(buffer);
   }
   while (fgets(buffer, sizeof(buffer), textfile) != NULL) {
      len = strlen(buffer);
      if ( len>0 && buffer[len-1] == '\n')
         buffer[len-1]  =  '\0';
      zvmessage(buffer,"");
   }
   status= fclose(textfile);
   return;
}
