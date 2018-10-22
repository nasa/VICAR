#include "vicmain_c"
#include "ftnbridge.h"
#include <string.h>
/************************************************************************/
/*                                                               	*/
/************************************************************************/


char  string[132];

int   f2rbuf[12];            /* Full to Real translation buffer  */
int   i2offs,i4offs, r4offs1, r4offs2, workoffs1, workoffs2;

short i2buf [100];
int   i4buf [100];
float rtemp, r4buf1[100], r4buf2 [100];
void  zdisplay_i2buf(void);
void  zdisplay_i4buf (void);
void  zdisplay_i4buf5 (void);
void  zdisplay_r4buf1 (void);
void  zdisplay_r4buf2 (void);


void main44()
{
int i, type;
    memset (i2buf,0,sizeof(i2buf));
    memset (i4buf,0,sizeof(i4buf));
    memset (i2buf,0,sizeof(i2buf));
    memset (r4buf1,0,sizeof(r4buf1));
    memset (r4buf2,0,sizeof(r4buf2));
      
    /* Initialize offsets into AP memory array */
    i2offs = 1000;              /* I*2 Working area */
    i4offs = 2000;              /* I*4 Working area */
    r4offs1 = 3000;             /* R*4 Working area #1 */
    r4offs2 = 4000;             /* R*4 Working area #2 */
    workoffs1 = 5000;           /* Out put work offset #1 */
    workoffs2 = 6000;           /* Out put work offset #2 */

    zifmessage ("TZFPSE version 2-Jan-95");

    /* Call FPSE subroutine APINIT */
    zvmessage ("Call APINIT","");
    zapinit ();

    /* Call FPSE subroutine APWD */
    zvmessage ("Call APWD","");
    zapwd ();

    /* Call FPSE subroutine APWR */
    zvmessage ("Call APWR",""); 
    zapwr ();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Clear segmentS of memory to be used for test */
   zvmessage ("VCLR  - Clear segments of AP memory","");
   zvclr (i2offs,1,100);        /* Clear each word of INTEGER*2 AREA */
   zvclr (i4offs,1,100);        /* Clear each word of INTEGER*4 AREA */
   zvclr (r4offs1,1,100);       /* Clear each word of REAL*4 AREA */
   zvclr (r4offs2,1,100);       /* Clear each word of REAL*4 AREA */
   zvclr (workoffs1,1,100);     /* Clear each word of WORK AREA */
   zvclr (workoffs2,1,100);     /* Clear each word of WORK AREA */


   /* Get and display cleared AP memory segments */
   zvmessage ("APGET - Get and display cleared AP memory segments","");
     

   /* Get 100 entries from AP memory in I*2 format into I2BUF  */
   type = 1;                         /* Return I*2 format */
   zapget (i2buf, i2offs, 100, type);
   zdisplay_i2buf();

   /* Get 100 entries from AP memory in I*4 format into I4BUF  */
   type = 0;                       /* Return I*4 format */
   zapget (i4buf, i4offs, 100, type);
   zdisplay_i4buf();

   /* Get 100 entries from AP memory in R*4 format into R4BUF1 */
   type = 2;                       /* Return R*4 format */
   zapget (r4buf1, r4offs1, 100, type);
   zdisplay_r4buf1();                   

   /* Initialize SEGMENTS of local memory */
   for (i = 0; i < 100; i++) {
      i2buf[i] = i+1;  
      i4buf[i] = i+1;  
      r4buf1[i] = (float)(i+1) * 0.1;
   }

   /* Put local memory to AP memory
      Reformatting will be performed by FPSE as specified by TYPE:
 	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
 	       1: in = I*4, out = I*2 ( then use VFIX)
 	       2: in/out = R*4
 	       3: in/out = R*4 (ignore "IBM format")
   */

   zvmessage ("APPUT - Put initialized memory segments into AP memory","");
   zapput (i2buf, i2offs,100,1);
   zapput (i4buf, i4offs,100,0);
   zapput (r4buf1,r4offs1,100,2);

   /* GET segments of AP memory to check for initialized values */

   /* But first clear local memory segments to zero */
   for (i = 0; i < 100; i++) {
     i2buf[i] = 0;
     i4buf[i] = 0;
     r4buf1[i] = 0.0;
   }

   /* Get 100 entries from AP memory in I*2 format into I2BUF */
   zvmessage ("APGET - Get and display initialized AP memory segments","");
   type = 1;                        /* Return I*2 format */
   zapget (i2buf, i2offs, 100, type);
   zdisplay_i2buf();

   /* Get 100 entries from AP memory in I*4 format into I4BUF */
   type = 0;                        /* Return I*4 format */
   zapget (i4buf, i4offs, 100, type);
   zdisplay_i4buf();

   /* Get 100 entries from AP memory in R*4 format into R4BUF1  */
   type = 2;                        /* Return R*4 format */
   zapget (r4buf1, r4offs1, 100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Correlate or convolute arrays A and B to obtain C */

   /* Initialize SEGMENTS of local memory */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.25 + 50.0;
     r4buf2[i] = 50.0 - (float)(i+1) * 0.25;
   }

   /* Put local memory to AP memory
   Reformatting will be performed by FPSE as specified by TYPE:
	TYPE = 0: in/out = I*4 (then use VPK8, VFIX32)
	       1: in = I*4, out = I*2 ( then use VFIX)
  	       2: in/out = R*4
 	       3: in/out = R*4 (ignore "IBM format")
   */
   zvmessage ("APPUT - Put initialized memory segments into AP memory","");
   zapput (r4buf1,r4offs1,100,2);
   zapput (r4buf2,r4offs2,100,2);

   zvmessage ("CONV  - Correlate arrays","");
   zconv (r4offs1,1,r4offs2,1,workoffs1,1,90,10);

   /* Get100 entries from AP memory in R*4 format into R4BUF1 */
   zvmessage ("APGET - Display Correlated array","");
   type = 2;                          /* Return R*4 format */
   zapget (r4buf1, workoffs1, 100, type);
   zdisplay_r4buf1();

   /* Clear AP memory working segment */
   zvclr (workoffs1,1,100);           /* Clear each word of WORK AREA */

   /* Convulate arrays */
   zvmessage ("CONV  - Convolute arrays","");
   zconv (r4offs1,1,r4offs2,-1,workoffs1,1,90,10);

   /* Get100 entries from AP memory in R*4 format into R4BUF1  */
   zvmessage ("APGET - Display convoluted array","");
   type = 2;                         /* Return R*4 format */
   zapget (r4buf1, workoffs1, 100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Generate histogram of an array starting at A, increment I
   with limits AMAX, AMIN, and stored in array C */

   zvmessage ("HIST  - Create histogram","");
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);           /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 2.5 + 100.0;
     r4buf2[i] = (float)(i+1) * 2.5 + 100.0;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Create histogram */
   zhist (r4offs1,1,workoffs1,100,70,r4offs2+70,r4offs2+20);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display histogram array","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                       /* Return R*4 format */
   zapget (r4buf1, workoffs1,100,type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* MMUL32 - Matrix multiply arrays A and B to obtain C */
 
   zvmessage ("MMUL32- Matrix multiply arrays A and B","");
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);          /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.1 + 100.0;
     r4buf2[i] = 100.0 - (float)(i+1) * 0.1;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Perform Matrix Multioplication */
   zmmul32 (r4offs1,1,r4offs2,1,workoffs1,1,100,10,10);

   /* Get 100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display MMUL32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                          /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* Add arrays A and B to obtain C */
   zvmessage ("VADDEM  - Add arrays A and B","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);          /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (100.0 + (float)(i+1) * 0.1) * 100.0;
     r4buf2[i] = 100.0 + (float)(i+1) * 0.1;
   }
   
   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Add arrays A and B to obtain C */
   zvaddem (r4offs1,1,r4offs2,1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1  */
   zvmessage ("APGET - Get and display VADDEM results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                            /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VCLIP - Move Array A to D, clipping it to range (B to C) */
   zvmessage ("VCLIP - Move array A to D & clip","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */
   zvclr (r4offs2  ,1,10);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 0.25 + 100.0;
   }
   r4buf2[0] = 105.5;
   r4buf2[1] = 120.3;
   
   /* Put segments into AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,2,2);

   /* Move array and clip */
   zvclip (r4offs1,1,r4offs2,r4offs2+1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display VCLIP results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                             /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();


/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFIX - Convert Elements from floating-point to Integer */
   zvmessage ("VFIX  - Convert Elements from floating-point to Integer","");

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */

   /* Initialize SEGMENTS of local memory to known real values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.25;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Convert float to integer */
   zvfix (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory into I4BUF */
   zvmessage ("APGET - Get and display VFIX results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                            /* Return R*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf();
 
 
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
 
   /* VFIX32 - Convert elements from floating point to integer */

   zvmessage ("VFIX32  - Convert from floating point to integer","");
     

   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);            /* Clear work area */

   /* Initialize SEGMENTS of local memory to known real values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.25;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Convert float to integer */
   zvfix32 (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory into I4BUF */
   zvmessage ("APGET - Get and display VFIX32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                              /* Return I*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFLT - Convert elements from integer to floating point */
   zvmessage ("VFLT  - Convert elements from integer to floating point","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = (i+1) * 100;
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Perform Integer to Floating point conversion */
   zvflt (i4offs,1,workoffs1+20,1,20);

   /* Get 100 entries from AP memory (in R*4 format) into R4BUF1 */
   zvmessage ("APGET - Get and display VFLT results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();
  
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VFLT32 - Convert elements from integer to floating point */
   zvmessage ("VFLT32  - Convert from integer to floating point","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = (i+1) * 100;
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Perform Integer to Floating point conversion */
   zvflt32 (i4offs,1,workoffs1+20,1,20);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1 */ 
   zvmessage ("APGET - Get and display VFLT32 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();
 
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
 
   /* VMOV - Move array A to Array C */
   zvmessage ("VMOV  - Move array A to C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 123.5;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);

   /* Move data */
   zvmov (r4offs1,1,workoffs1,1,100);

   /* Get100 entries from AP memory (in R*4 format) into R4BUF1  */
   zvmessage ("APGET - Get and display VMOV results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VPK8 - Packs LSB from 4 words of A into a single word C */
   zvmessage ("VPK8  - Pack low bytes of 4 words of A into 1 word of C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = 16909060.0;             /* '01020304'X */
   }

   /* Put segments in AP memory in I*4 format */
   zapput (r4buf1, r4offs1,100,0);

   /* Pack data */
   zvpk8 (r4offs1,1,workoffs1,1,100);

   /* Get packed entries from AP memory */
   zvmessage ("APGET - Get and display VPK8 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 0;                            /* Return I*4 format */
   zapget (i4buf, workoffs1,100, type);
   zdisplay_i4buf5();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VSADD - Add array A to Scaler B to obtain C */
 
   zvmessage ("VSADD - Add array A to Scaler B","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);             /* Clear work area */

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     r4buf1[i] = (float)(i+1) * 1.1 + 100.0;
     r4buf2[i] = 100.0 - (float)(i+1) * 1.1;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Move data */
   zvsadd (r4offs1,1,r4offs2+50,workoffs1,1,100);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VSADD results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                           /* Return R*4 format */
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VSMUL - Multiply array A and Scaler B to obtain C */

   zvmessage ("VSMUL - Multiply array A by Scaler B","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);              /* Clear work area */

   for (i = 0; i < 100; i++) {
     r4buf2[i] = 0.0;
   }
   zapput (r4buf2, r4offs1-50,100,2);
   zapput (r4buf2, r4offs1+50,100,2);
   zapput (r4buf2, r4offs2-50,100,2);
   zapput (r4buf2, r4offs2+50,100,2);
   zapput (r4buf2, workoffs1-50,100,2);
   zapput (r4buf2, workoffs1+50,100,2);

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     rtemp = (float) i;
     r4buf1[i] = (rtemp + 1.0 + 100.0) * 1.1;
     r4buf2[i] = 50.2;
   }

   /* Put segments in AP memory */
   zapput (r4buf1, r4offs1,100,2);
   zapput (r4buf2, r4offs2,100,2);

   /* Multiply data  */
   zvsmul (r4offs1,1,r4offs2,workoffs1,1,100);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VSMUL results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;                              /* Return R*4 format */
   zapget (r4buf2, workoffs1,100, type);
   zdisplay_r4buf2();

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

   /* VUP8 - Unpacks 4 bytes, from four bytes of word A, into 4 words of C */
 
    zvmessage ("VUP8 - Unpack four bytes of A into four words of C","");
     
   /* Clear AP memory working area */
   zvclr (workoffs1,1,100);

   /* Initialize SEGMENTS of local memory to known values */
   for (i = 0; i < 100; i++) {
     i4buf[i] = 0X01020304;  
   }

   /* Put segments in AP memory */
   zapput (i4buf, i4offs,100,0);

   /* Unpack data */
   zvup8 (i4offs,1,workoffs1,1,25);

   /* Get 100 entries from AP memory */
   zvmessage ("APGET - Get and display VUP8 results","");

   /* Call APGET to retrieve working array from AP memory */
   type = 2;
   zapget (r4buf1, workoffs1,100, type);
   zdisplay_r4buf1();

   /* Convert floating point to fixed point format */
   zvfix (workoffs1,1,workoffs2,1,100);
   zapget (i4buf,workoffs2,100,0);
   zdisplay_i4buf();

   return;
}
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
                                                                              
void zdisplay_i2buf (void)
{
int j;

   zvmessage ("I2BUF","");
   for (j = 0; j < 100; j+=10) {
      sprintf (string,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d",
       i2buf[j+0], i2buf[j+1], i2buf[j+2], i2buf[j+3], i2buf[j+4],
       i2buf[j+5], i2buf[j+6], i2buf[j+7], i2buf[j+8], i2buf[j+9]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

void zdisplay_i4buf (void)
{
int j;

   zvmessage ("I4BUF","");
   for (j = 0; j < 100; j+=10) {
      sprintf (string,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d",
           i4buf[j+0], i4buf[j+1], i4buf[j+2], i4buf[j+3], i4buf[j+4],
           i4buf[j+5], i4buf[j+6], i4buf[j+7], i4buf[j+8], i4buf[j+9]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

void zdisplay_i4buf5 (void)
{
int j;

   zvmessage ("I4BUF5","");
   for (j = 0; j < 25; j+=5) {
      sprintf (string,"%16d%16d%16d%16d%16d",
          i4buf[j+0], i4buf[j+1], i4buf[j+2], i4buf[j+3], i4buf[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

void zdisplay_r4buf1 (void)
{
int j;

   zvmessage ("R4BUF1","");
   for (j = 0; j < 100; j+=5) {
      sprintf (string,"%16.4f%16.4f%16.4f%16.4f%16.4f",
           r4buf1[j+0], r4buf1[j+1], r4buf1[j+2], r4buf1[j+3], r4buf1[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

void zdisplay_r4buf2 (void)
{
int j;

   zvmessage ("R4BUF2","");
   for (j = 0; j < 100; j+=5) {
      sprintf (string,"%16.4f%16.4f%16.4f%16.4f%16.4f",
           r4buf2[j+0], r4buf2[j+1], r4buf2[j+2], r4buf2[j+3], r4buf2[j+4]);
      zvmessage (string,"");
   }
   return;
}

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
