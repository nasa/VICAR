/* tzdarray is a program to test the C Callable Subroutine for DARRAY */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     int mode, i, j, m, n, is, id, k, a, b;
     double s1[5][5],d1[10][10];
     char ms1[400];

sprintf(ms1,"Test the C interface");
zvmessage(ms1," ");

/* Test Mode 1 */

     id=10;
     is=5;
     k=0;

     for (i=0; i<is; i++)
     {
          for (j=0; j<is; j++)
          {
               s1[j][i]=(double)(j+i+2.0);
          }
     }

     for (n=0; n<id; n++)
     {
          for (m=0; m<id; m++)
          {
               d1[m][n]=(double)(0.0);
          }
     }

     mode=1;
     j=is;
     i=is;
     n=id;
     m=id;

     zdarray(mode,i,j,n,m,&s1[0][0],&d1[0][0]);
     sprintf(ms1," mode 1: ");
     zvmessage(ms1," ");

     for (a=0; a<is; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",s1[0][a],s1[1][a],s1[2][a],s1[3][a],s1[4][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"d array: ");
     zvmessage(ms1," ");
     for (a=0; a<id; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[0][a],d1[1][a],d1[2][a],d1[3][a],d1[4][a]);
         zvmessage(ms1," ");      
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[5][a],d1[6][a],d1[7][a],d1[8][a],d1[9][a]);
         zvmessage(ms1," ");      
     }


/* Test Mode 2 */

     id=10;
     is=5;
     k=0;

     for (i=0; i<is; i++)
     {
          for (j=0; j<is; j++)
          {
               s1[j][i]=(double)(0.0);
          }
     }

     for (n=0; n<id; n++)
     {
          for (m=0; m<id; m++)
          {
               d1[m][n]=(double)(m+n+2.0);
          }
     }

     mode=2;
     j=is;
     i=is;
     n=id;
     m=id;

     zdarray(mode,i,j,n,m,&s1[0][0],&d1[0][0]);
     sprintf(ms1," mode 2: ");
     zvmessage(ms1," ");

     for (a=0; a<is; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",s1[0][a],s1[1][a],s1[2][a],s1[3][a],s1[4][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"d array: ");
     zvmessage(ms1," ");
     for (a=0; a<id; a++) 
     {
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[0][a],d1[1][a],d1[2][a],d1[3][a],d1[4][a]);
         zvmessage(ms1," ");      
         sprintf(ms1,"%05.4E %05.4E %05.4E %05.4E %05.4E ",d1[5][a],d1[6][a],d1[7][a],d1[8][a],d1[9][a]);
         zvmessage(ms1," ");      
     }

     sprintf(ms1,"Test the FORTRAN interface");
     zvmessage(ms1," ");

     FTN_NAME(tdarray)();

}
