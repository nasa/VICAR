#include <math.h>
#include <stdio.h>
#include <stdlib.h>         //64-bit edit for NULL
#include <string.h>

#include "ms_defines.h" 
#include "applic.h" 
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "vicmain_c.h"
#include "zifmessage.h"
#include "zmabend.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MXDDWID 2000

/* prototypes  added for 64-bit */

unsigned char ct1 (unsigned char *s);
short int ct2(short int *s) ;
int ct4(int *s);
float ct7(float *s);
double ct8(double *s);

void st1(unsigned char v,unsigned char *s);
void st2(short int v,short int *s);
void st4(int v,int *s);
void st7(float v,float *s);
void st8(double v,double *s);
void lflush(FILE *infile);

/************************************************************************/
/* program acopin                                                       */
/************************************************************************/
/* 00-05 ...alz... initial version                                      */
/* see pdf for history continuation                                     */
/************************************************************************/

int bufsizdefv[6] = {240000,2000000,8000000,20000000,60000000,99000000};

void lflush(infile)
   FILE *infile;
{
   int i; char ch;
   for (i=0;i<10000;i++)
      {
      fscanf(infile,"%c",&ch);
      if (ch=='\n'||ch=='\r') break;
      }
   return;
}

/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1 (unsigned char *s) {
  return(*s);
}

short int ct2(short int *s) {
  return(*s);
}

int ct4(int *s) {
  return(*s);
}

float ct7(float *s) {
  return(*s);
}

double ct8(double *s) {
  return(*s);
}

void st1(unsigned char v,unsigned char *s) {
  *s = v;
  return;
}

void st2(short int v,short int *s) {
  *s = v;
  return;
}

void st4(int v,int *s) {
  *s = v;
  return;
}

void st7(float v,float *s) {
  *s = v;
  return;
}

void st8(double v,double *s) {
  *s = v;
  return;
}

void main44(void)
{
   int      nincol,rt,lead_rm;
   char     infilename[2][255];             //64-bit
   
   double *numoutcol;

   unsigned char *buf=NULL,*nbuf;
   
   int ncol,tablen=0,totrec=0,i,j,k;
   int datcols[100],typ[100],wid[100],totwid[101],ibig;
   int krt,klen,kcp,bufsiz,ototwid[101],ototrec;
   int coldef,unit,ibis,status,parmct,parmdf,irec,rstlen;
   int ind;
   double tmp8;
   char *p,fmtstring[10],rst[MXDDWID];
   char msg[150];
   FILE *infile;

   nincol = 101;
   zifmessage("acopin Fri Dec  5 2014 - wlb");
   
   /* open tae, fetch params */
   
   zvparm("inp",infilename,&parmct,&parmdf,2,254);
   if ((infile = fopen(infilename[0],"r")) == NULL) {
        sprintf (msg,"??E - Cannot find file = %s\n",infilename[0]);
        zmabend(msg);
    }
   for (i=0;i<100;i++) datcols[i] = i+1;
   zvparm("cols",datcols,&nincol,&coldef,100,0);            //20,99);
   rt = zvptst("rtjust");
   zvp("lead_rm",&lead_rm,&parmct);

   for (i=0;i<lead_rm;i++) lflush(infile);
   /* open table */
   status = zvunit(&unit,"inp",2,NULL);                     //64-bit edit
   status = IBISFileOpen(unit,&ibis,IMODE_UPDATE,0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (coldef) nincol = ncol;
    
   /* process widths of fields */
   sprintf (msg,"Number of input cols = %d",nincol);
	zvmessage(msg," ");
   totwid[0] = 0;
   for (i=0;i<nincol;i++) {
        status = IBISColumnGet(ibis,"FORMAT",fmtstring,datcols[i]);
        if (status!=1) IBISSignal(ibis,status,1);
        if (fmtstring[0]=='A') {
            wid[i] = ms_num(&fmtstring[1])+1;
            typ[i] = 0;
	        sprintf (msg,"    Col %d is ASCII width = %d",i+1,wid[i]);
	        zvmessage (msg," ");
        } else {
        	/* If not ascii, force DOUB format */
            status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datcols[i]);
            if (status!=1) IBISSignal(ibis,status,1);
            wid[i] = 8;
            typ[i] = 8;
        }
        totwid[i+1] = totwid[i]+wid[i];
    } //for (i=0;i<nincol;i++)
   /* read all input into contiguous columns */

   ototrec = 0;
   for (ibig=0;ibig<6;ibig++) {
        bufsiz = bufsizdefv[ibig];
        mz_alloc1((unsigned char **)&nbuf,bufsiz,1);
        if (ibig==0) {
	        totrec = bufsiz/totwid[nincol];
	        totrec = (totrec/8)*8;
	        for (i=0;i<=nincol;i++) totwid[i] *= totrec;
	    } else {
	        ototrec = totrec;
	        for (i=0;i<=nincol;i++) {
	            ototwid[i] = totwid[i];
	            totwid[i] /= totrec;
	        }
	        totrec = bufsiz/totwid[i];      /* changed from totrec = bufsiz/totwid[nincol]; */
                                            /* to prevent  "warning: array subscript is below array bounds" */
	        totrec = (totrec/8)*8;
	        for (i=0;i<=nincol;i++) totwid[i] *= totrec;
	        for (j=nincol-1;j>=0;j--) {
	    /*bcopy(&buf[ototwid[j]],&nbuf[totwid[j]],wid[j]*ototrec);*/
	            zmve(1,wid[j]*ototrec,&buf[ototwid[j]],&nbuf[totwid[j]],1,1);
	        }
	    free (buf);
	    }
        buf = nbuf;

        for (i=ototrec;i<totrec;i++)
	        for (j=0;j<nincol;j++) {
	            switch(typ[j])
	            {
	            case 0: {					/* ASCII case */
//                    printf ("case 0\n");
	                fscanf(infile,"%s",rst);
	                if (feof(infile)&&j==0) { tablen = i-1; goto endread; }
	                if (feof(infile)&&j<(nincol-1)) {
			            sprintf (msg,"Case 0 (ASCII): record %d col %d",i,j);
			            zvmessage (msg," ");
			            zmabend("??E Partial record at end");
		            }
	                klen = (int)strlen(rst); krt = 0;
	                if (rt) krt = wid[j]-klen-1;
	                if (krt<0) krt = 0;
	                kcp = wid[j]-1; if (klen<kcp) kcp = klen;
	                for (k=0;k<wid[j]-1;k++)
		                buf[totwid[j]+i*wid[j]+k] = ' ';
	                    buf[totwid[j]+(i+1)*wid[j]-1] = (char)0;
	                    for (k=0;k<kcp;k++)
		                    buf[totwid[j]+i*wid[j]+k+krt] = (unsigned char)rst[k];
	                    if (feof(infile)) { tablen = i; goto endread; }
	                break;
	            }
	            case 8:	{				/* DOUB case */
//                    printf ("case 8 --->\n");
	                ind = fscanf(infile,"%s",rst);
                    if (ind == 1 || ind == -1) {
                    } else {
                        sprintf (msg,"ind on fscanf of %s = %d\n",infilename[0],ind);
                        zvmessage (msg," ");
                        zmabend("??E Input file error");
                    }
	                if (feof(infile)&&j==0) { 
                        tablen = i-1; goto endread; 
                    }
	                if (feof(infile)&&j<(nincol-1)) {
			            sprintf (msg,"Case 8 (DOUBLE): record %d col %d",i,j);
			            zvmessage (msg," ");
			            zmabend("??E Partial record at end");
		            }
	                rstlen = (int)strlen(rst);
	                for (p=rst,k=0;k<rstlen;p++,k++)
	                {
	                    if (*p>='0'&&*p<='9') continue;
	                    if (*p=='.'||*p=='+'||*p=='-'||*p=='e') continue;
	                    if (*p=='E'||*p=='d'||*p=='D') continue;
/*			sprintf (msg,"typ[%d] = %d",j,typ[j]);
			zvmessage (msg," ");
*/
			            sprintf (msg,"     ??E character = %s in numeric col %d  record %d",p,j,i);
			            zvmessage (msg," ");
			            zvmessage ("     ??E Are there blanks in ASCII field?"," ");
	                    zmabend("??E Case 8 (DOUBLE): Non-numeric data in a numeric field");
	                }   
	                sscanf(rst,"%lf",&tmp8);
	                st8(tmp8,(double*)&buf[totwid[j]+i*wid[j]]);             //64-bit edit
	                if (feof(infile)) { tablen = i; goto endread; }
	                break;
                }
                default: {
                    zvmessage ("     ??E Unexpected Case for first switch(typ[j])"," ");
                    zmabend("??E Case Default");
                }
	        } //end switch
	     } // for (j=0;j<nincol;j++)
      } // for (ibig=0;ibig<6;ibig++)   
      endread: tablen++;

   /* write out data */

    sprintf (msg,"Output length is %d records\n\n",tablen);
    zvmessage (msg," ");
//   mz_alloc1((unsigned char **)&numoutcol,tablen,8);      //warning: dereferencing type-punned pointer will break strict-aliasing rules
   mz_alloc1((unsigned char **)&numoutcol,tablen,8);
   status = IBISFileSet(ibis,"nr",(char*)((long)tablen),0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   for (i=1;i<=ncol;i++) {
      k = -1;
      for (j=0;j<nincol;j++) if (datcols[j]==i) k=j;
      if (k>=0) {
	    switch (typ[k])
	        {
	            case 0: {
	                status = IBISColumnWrite(ibis,(char*)&buf[totwid[k]],i,1,tablen);       //64-bit
                    if (status!=1) IBISSignal(ibis,status,1);
                    break;
                }
                case 8: {
	                for (irec=0;irec<tablen;irec++)
	                    numoutcol[irec] = ct8((double*)&buf[totwid[k]+irec*wid[k]]);        //64-bit
	                status = IBISColumnWrite(ibis,(char*)numoutcol,i,1,tablen);
                    if (status!=1) IBISSignal(ibis,status,1);
                    break;
                }
                default: {
                    zvmessage ("     ??E Unexpected Case for second switch(typ[j])"," ");
                    zmabend("??E Case Default");
                }
            }
        } else {
	        status=7;
	    }
     }
   
   /* IBISFileClose required here because nr is changed */
   
   status = IBISFileClose(ibis,NULL);                                   //64-bit
   if (status!=1) IBISSignalU(unit,status,1);
   return;

}
