/* Tabulate takes up to 3 ascii files and changes all space runs including
   tabs to a single tab and appends the files together. */
/* Note: tabulate ignores leading spaces and tabs.                    */ 
/* Revision History */
/* 7-97 RRD Combined modules tabulate, tabulator, and fileappend      */

#include <stdio.h>
#include <string.h>
#include "vicmain_c"

void main44()
{
#define MAXR1    255              /* buffer length for file name      */
#define MAXR2    2*MAXR1          /* buffer length for full file name */
#define BLEN     32767            /* buffer length for text           */
#define NUMFILES 3                /* maximum number of files          */

   int   count;                   /* used by zvparm to get parameters */
   int   len, status, def;
   FILE  *infile, *outfile;       /* in/out files                     */
   char  inbuf[BLEN], outbuf[BLEN];  /* buffer for text               */
   char  oname[MAXR1];            /* output file name                 */
   char  full_out[MAXR2+1];       /* full file path name for outfile  */
   char  full_in[MAXR2+1];        /* full file path name for infile   */
   int   i, j, k;                 /* indeces                          */
   char  space_or_tab;            /* boolean variable                 */
   char  inames[NUMFILES][MAXR1]; /* array of input file names        */
/*  ================================================================= */

/* Retrieve the user specified filenames */

   zvp("OUT", oname, &count);
   zvfilename(oname,full_out,MAXR2);

   zvparm("IN", inames, &count, &def, NUMFILES, MAXR1);

/* Open and read files */

   outfile = fopen(full_out, "w");
   if (outfile == NULL)
     file_error(oname, "OPENING");

   for (k=0;k<count;++k) {
     zvfilename(inames[k],full_in,MAXR2);
     infile = fopen(full_in, "r");
     if (infile == NULL)
       file_error(inames[k], "OPENING");

     /* Read infile into outbuf and write to outfile */

     while (fgets(inbuf, sizeof(inbuf), infile) != NULL) {
       j=0;
       space_or_tab=1;
       len = strlen(inbuf);
       for (i=0;i<len;++i)
         if (inbuf[i]!=' ' && inbuf[i]!='\t') {
       	   outbuf[j] = inbuf[i];
           ++j;
           space_or_tab=0;
         }
         else
           if (!space_or_tab) {
             outbuf[j]='\t';
             ++j;
             space_or_tab=1;
           }
       outbuf[j]='\0';
       status = fprintf(outfile,"%s",outbuf);
       if (status==0)
         file_error(oname, "WRITING");
     }
     status = fclose(infile);
     if (status!=0) 
       file_error(inames[k], "CLOSING");
   }

   status= fclose(outfile);
   if (status!=0)
     file_error(oname, "CLOSING");
   return;
}


/* handles file errors */
file_error(fname, msg)
char fname[], msg[];
{
  char buf[MAXR1+25];

  sprintf(buf, "ERROR %s TEXT FILE '%s'", msg, fname);
  zmabend(buf);
  return;
}
