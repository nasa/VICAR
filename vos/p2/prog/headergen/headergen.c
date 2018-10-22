/* Headergen takes an ascii file of records and combines them into a
   single record in outfile. */
/* Revision History */
/* 7-97 RRD Combined modules headergen and headers and ported to UNIX */

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    250              /* buffer length for file name      */
#define MAXR2    2*MAXR1          /* buffer length for full file name */   

   int   count;                   /* used by zvp to get parameters    */
   int   len, status;
   FILE  *infile, *outfile;       /* pointers to input/output files   */
   char  inbuf[132];              /* buffer for text from infile      */
   char  outbuf[32767];           /* buffer for text to outfile       */
   char  iname[MAXR1], oname[MAXR1];         /* file names            */
   char full_in[MAXR2+1];         /* full file path name for infile   */
/*  ================================================================= */

/* Retrieve the user specified filenames */

   zvp("INFILE", iname, &count);
   zvfilename(iname,full_in,MAXR2);
   zvp("OUTFILE", oname, &count);


/* Open files */

   infile = fopen(full_in, "r");
   if (infile == NULL) {
       sprintf(outbuf, "ERROR OPENING text FILE '%s'", iname);
       zmabend(outbuf);
   }
   outfile = fopen(oname, "w");

/* Read infile into outbuf */

   outbuf[0] = '\0';
   while (fgets(inbuf, sizeof(inbuf), infile) != NULL) {
      len = strlen(inbuf);
      if ( len>0 && inbuf[len-1] == '\n')
         inbuf[len-1]  =  '\0';
      strcat(outbuf,inbuf);
   }

/* Write outbuf to outfile */

   fprintf(outfile,"%s",outbuf);

   status= fclose(infile);
   status= fclose(outfile);
   return;
}
