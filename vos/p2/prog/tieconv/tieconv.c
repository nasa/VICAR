#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoMatUtils.h"
#include "cartoSortUtils.h"
#include "cartoLsqUtils.h"
#include "cartoMemUtils.h"
#include "cartoVicarProtos.h"
#include "cartoGridUtils.h"*/

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
#define MAXLSQ          400
#define MAXLSQD         MAXLSQ+1
#define MAXLSQ2         2*MAXLSQD
#define MAXLSQ10        10*MAXLSQD
#define MAXTIE          10000000

/************************************************************************/
/* program tieconv                                                      */
/************************************************************************/
/*  83-10 ...alz... initial version, new algorithm for thiessen         */
/*                  triangulation, converted to c,                      */
/************************************************************************/

short int *vgroup;
int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
      cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
      ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
double gridtol;
typedef unsigned char  byte;

/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1(s) unsigned char *s; { return(*s); }
short int ct2(s) short int *s; { return(*s); }
int ct4(s) int *s; { return(*s); }
float ct7(s) float *s; { return(*s); }
double ct8(s) double *s; { return(*s); }
void st1(v,s) unsigned char v,*s; { *s = v; return; }
void st2(v,s) short int v,*s; { *s = v; return; }
void st4(v,s) int v,*s; { *s = v; return; }
void st7(v,s) float v,*s; { *s = v; return; }
void st8(v,s) double v,*s; { *s = v; return; }

void tgridinf(double* ptx,
	   double* pty,
	   double dist,
	   int ptr1,
	   int ptr2,
	   int ptr3)
{
   double x1,x2,y1,y2,dx,dy,ds,vx,vy;

   x1 = ptx[ptr1];
   x2 = ptx[ptr2];
   y1 = pty[ptr1];
   y2 = pty[ptr2];

   dx = x2-x1;
   dy = y2-y1;
   ds = sqrt(dx*dx+dy*dy);
   vx = (y1-y2)/ds;
   vy = (x2-x1)/ds;
   ptx[ptr3] = vx*dist+0.5*(x1+x2);
   pty[ptr3] = vy*dist+0.5*(y1+y2);
   return;
}

void tgridchk(int npoints,
	   double* ptx,
	   double* pty,
	   int** tcon1,
	   int** tcon2,
	   int** tcon3,
	   int gridnah,
           int ptr)
{
   int i,nah,nav,nahp,navp,n;
   double xtst,ytst;
   
   n = npoints-4;
   nah = gridnah;
   nahp = nah+1;
   navp = n/nahp;
   nav = navp-1;

   for (i=1;i<nah-1;i++)
      {
      xtst = ptx[nah+i+1];
      ytst = pty[nah+i+1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, top row");
            }

      xtst = ptx[n-2*nah+i-2];
      ytst = pty[n-2*nah+i-2];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, bottom row");
            }
      }

   for (i=1;i<nav-1;i++)
      {
      xtst = ptx[i*nahp+1];
      ytst = pty[i*nahp+1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, left column");
            }
      xtst = ptx[i*nahp+nah-1];
      ytst = pty[i*nahp+nah-1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, right column");
            }
      }
   return;
}

/*  subroutine tgrid:
triangulates a grid, and the four extrapolated points in the last four
positions as generated by polygeov main */

void tgridxxx(int npoints,
	   int* nlinret,
	   double* ptx,
	   double* pty,
	   int* ntriang,
	   int** tcon1,
	   int** tcon2,
	   int** tcon3,
	   int gridnah,
	   double* csol,
	   double* optx,
	   double* opty,
	   int zgeom)
{
   int i,j,nah,nav,nahp,navp,ptr,n,opoint;
   double bigdist,x,y,fac;
   
   n = npoints-4;
   nah = gridnah;
   nahp = nah+1;
   navp = n/nahp;
   nav = navp-1;
   if (nahp*navp!=n) zmabend("grid has wrong number of points");
   *ntriang = nah*nav*2+(nah+nav)*2+4;
   *nlinret = (*ntriang*3)/2+2;
           
   mz_alloc1((unsigned char **)tcon1,*ntriang,4);
   mz_alloc1((unsigned char **)tcon2,*ntriang,4);
   mz_alloc1((unsigned char **)tcon3,*ntriang,4);
   
   /* 4 far points may be unsuitable, substitute these */
  
   bigdist = 0.6*(abs(ptx[n-1]-ptx[0])+abs(pty[n-1]-pty[0])+
      abs(ptx[n-nah-1]-ptx[nah])+abs(pty[n-nah-1]-pty[nah]));
   tgridinf(ptx,pty,bigdist,0,nah,n);

   /*printf("ptx[0] %f %f\n",ptx[0],pty[0]);
   printf("ptx[nah] %f %f\n",ptx[nah],pty[nah]);
   printf("ptx[n] %f %f\n\n",ptx[n],pty[n]);*/

   tgridinf(ptx,pty,bigdist,nah,n-1,n+1);

   /*printf("ptx[nah] %f %f\n",ptx[nah],pty[nah]);
   printf("ptx[n-1] %f %f\n",ptx[n-1],pty[n-1]);
   printf("ptx[n+1] %f %f\n\n",ptx[n+1],pty[n+1]);*/

   tgridinf(ptx,pty,bigdist,n-1,n-nah-1,n+3);

   /*printf("ptx[n-1] %f %f\n",ptx[n-1],pty[n-1]);
   printf("ptx[n-nah-1] %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
   printf("ptx[n+3] %f %f\n\n",ptx[n+3],pty[n+3]);*/

   tgridinf(ptx,pty,bigdist,n-nah-1,0,n+2);

   /*printf("ptx[n-nah-1] %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
   printf("ptx[0] %f %f\n",ptx[0],pty[0]);
   printf("ptx[n+2] %f %f\n\n",ptx[n+2],pty[n+2]);*/

   /* resolve the corner points */

   fac = 0.5;         /* 0 for linear, 1.0 for cubic, 0.5 for 1/2 each */
   for (i=0;i<4;i++)
      {
      x = ptx[n+i]; y = pty[n+i];
      optx[n+i] = csol[0]*x+csol[1]*y+csol[2]+fac*(csol[3]*x*y+
         csol[4]*x*x+csol[5]*y*y+csol[6]*x*x*x+csol[7]*x*x*y+
         csol[8]*x*y*y+csol[9]*y*y*y);
      if (!zgeom) opty[n+i] = csol[10]*x+csol[11]*y+csol[12]+fac*(csol[13]*x*y+
         csol[14]*x*x+csol[15]*y*y+csol[16]*x*x*x+csol[17]*x*x*y+
         csol[18]*x*y*y+csol[19]*y*y*y);
      }
   
   /* edge triangles to top outside point, note pointers are +1 */
    
   ptr = 0;
   opoint = n+1;
   for (i=0;i<nah;i++)
      {
      (*tcon1)[ptr] = opoint;
      (*tcon3)[ptr] = i+2;
      (*tcon2)[ptr++] = i+1;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with top side\n");*/
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = opoint+1;
   (*tcon2)[ptr++] = nahp;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with top corner 1\n");*/
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = 1;
   (*tcon2)[ptr++] = opoint+2;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with top corner 2\n");*/

            /*printf("ptx[0] %f %f\n",ptx[0],pty[0]);
            printf("ptx[nah+1] %f %f\n",ptx[nah+1],pty[nah+1]);
            printf("ptx[nah+2] %f %f\n\n",ptx[nah+2],pty[nah+2]);

            printf("ptx[nah] %f %f\n",ptx[nah+2],pty[nah]);
            printf("ptx[2*nah+1]  %f %f\n",ptx[2*nah+1],pty[2*nah+1]);
            printf("ptx[2*nah]  %f %f\n\n",ptx[2*nah],pty[2*nah]);

            printf("ptx[n-nah-1]  %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
            printf("ptx[n-2*nah-2]  %f %f\n",ptx[n-2*nah-2],pty[n-2*nah-2]);
            printf("ptx[n-2*nah-1]  %f %f\n\n",ptx[n-2*nah-1],pty[n-2*nah-1]);

            printf("ptx[n-1]  %f %f\n",ptx[n-1],pty[n-1]);
            printf("ptx[n-2]  %f %f\n",ptx[n-2],pty[n-2]);
            printf("ptx[n-nah-3]  %f %f\n\n",ptx[n-nah-3],pty[n-nah-3]);*/


   /* grid triangles, contents of arrays are fortran referenced (+1) */
   
   for (i=0;i<nav;i++)
      {
      for (j=0;j<nah;j++)
         {
         /* upper left triangle */
         (*tcon1)[ptr] = i*nahp+j+1;
         (*tcon3)[ptr] = i*nahp+j+2;
         (*tcon2)[ptr++] = (i+1)*nahp+j+1;
         /* lower right triangle */
         (*tcon1)[ptr] = i*nahp+j+2;
         (*tcon3)[ptr] = (i+1)*nahp+j+2;
         (*tcon2)[ptr++] = (i+1)*nahp+j+1;
         }
      /* triangles to outside side points */
      (*tcon1)[ptr] = (i+1)*nahp;
      (*tcon3)[ptr] = opoint+1;
      (*tcon2)[ptr++] = (i+2)*nahp;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      (*tcon1)[ptr] = i*nahp+1;
      (*tcon3)[ptr] = (i+1)*nahp+1;
      (*tcon2)[ptr++] = opoint+2;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with sides\n");*/

   /* edge triangles to bottom outside point */
   
   for (i=0;i<nah;i++)
      {
      (*tcon1)[ptr] = nav*nahp+i+1;
      (*tcon3)[ptr] = nav*nahp+i+2;
      (*tcon2)[ptr++] = opoint+3;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with bottom row\n");*/
   (*tcon1)[ptr] = opoint+3;
   (*tcon3)[ptr] = opoint+2;
   (*tcon2)[ptr++] = n-nahp+1;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with bottom left\n");*/
   
   (*tcon1)[ptr] = opoint+3;
   (*tcon3)[ptr] = n;
   (*tcon2)[ptr++] = opoint+1;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with bottom right\n");*/
   
      /*printf("triB3 %f %f\n %f %f\n%f %f \n",      useful code
     ptx[(*tcon1)[ptr-1]-1]+3388.0,pty[(*tcon1)[ptr-1]-1]+3722.0,
     ptx[(*tcon2)[ptr-1]-1]+3388.0,pty[(*tcon2)[ptr-1]-1]+3722.0,
     ptx[(*tcon3)[ptr-1]-1]+3388.0,pty[(*tcon3)[ptr-1]-1]+3722.0);*/
   
   return;
}





void main44(void)
{
   double *rpar;
   int *con1,*con2,*con3;
   
   double tmaxx,tmaxy,tminx,tminy,x,y,xx,yy=0,*ptx,*pty;
   double clsq[MAXLSQ10], clsqxy[MAXLSQ2], elsqxy[MAXLSQ2];
   double csol[20],*optx,*opty;
   double tab[4],work[16];
   double **coeff,dx,dy,**vout;
   double minx,miny,maxx,maxy,normlz[4];
   float **pout,*rout,fac;
   int cols[4];
   int zgeom,mgeom,abendl,abendg,lgeom,geomv,keystone,linear; /*booleans*/
   int quad,cubic,polynom; /*booleans*/
   int plot,printit;
   int found,null9;
   int nah = 30,nav = 30,npoint = 4,nrank = 6,nerr = 0;
   int geoma = 1,lgeomlike = 0;
   char outnam[73],polystring[10];
   
   int i,j,k,n,ier,inpcnt,status,unit,colcount,ibis,clen,ptr,record;
   int irow,ntiepp,icnt,idef,nvt,nklsq,nlret;
   int ntri,nah1,nav1,gridnah,gridnav=0,pcount,pdef;
   int ttri,ttrj1,minldf,maxldf,minsdf,maxsdf,coldef,tiepdef,nht,tiept;
   int ix,itri,tri,isign,p,ibisOut,parmOut,ptr2,nout,outdf;
   double eps,skinny,reject;
        
   zifmessage("tieconv version Tue Mar 01 2011");
   
   printit = zvptst("PRINT");
   
   zvparm("poly",polystring,&pcount,&pdef,4,0);
   keystone = strncmp(polystring,"KEYSTONE",8)==0;
   linear = strncmp(polystring,"LINEAR",6)==0;
   quad = strncmp(polystring,"QUAD",4)==0;
   cubic = strncmp(polystring,"CUBIC",5)==0;
   polynom = keystone||linear||quad||cubic;
   zvparmd("gridtol",&gridtol,&pcount,&pdef,1,0);
   
   /*	if inp file specified then read in tiepoints from
	  the ibis interface file */

   status = zvpcnt("inp",&inpcnt);
   if (inpcnt>0)
      {
      status = zvunit(&unit,"inp",1, NULL);
     
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1,0);
      mz_alloc1((unsigned char **)&rpar,colcount*clen,8);
      ptr = 0;
      status = IBISRecordOpen(ibis,&record,0,cols,colcount,IFMT_DOUB);
      if (status!=1) IBISSignal(ibis,status,1);
      for (irow=1;irow<=clen;irow++)
	 {
         status = IBISRecordRead(record,(char*)(&rpar[ptr]),irow);
         if (status!=1) IBISSignal(ibis,status,1);
	 ptr += colcount;
	 }
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      ntiepp = ptr;
      }
   else
      {
      mz_alloc1((unsigned char **)&rpar,1625,8);
      zvparm("tiepoint",&rpar,&ntiepp,&tiepdef,1625,0);
      }
   zvparm("nah",&nht,&icnt,&idef,1,0);
   zvparm("nav",&nvt,&icnt,&idef,1,0);
   zvparm("gridnah",&gridnah,&icnt,&idef,1,0);

   abendl = zvptst("abend");
   abendg = zvptst("abendg");
   reject = 0.01;
   zvparm("reject",&reject,&icnt,&idef,1,0);
   reject = reject*reject;
   geoma = zvptst("geoma");
   geomv = zvptst("geomv");
   mgeom = zvptst("mgeom");
   lgeom = zvptst("lgeom");
   lgeomlike = mgeom||lgeom||geomv;
   zgeom = zvptst("geomz");
   plot = zvptst("plot");
   if (plot&&polynom)
      zmabend("can't plot with polynomial fit options");
   
   if (zgeom) npoint = 3;
   if (zgeom) nrank = 3;
   
   n = ntiepp/npoint;
   if (n<3) zmabend("need 3 tiepoints");
   if (n<4&&keystone) zmabend("need 4 tiepoints for keystone option");
   if (n<6&&quad) zmabend("need 6 tiepoints for quadratic option");
   if (n<10&&cubic) zmabend("need 10 tiepoints for cubic option");
   if (n>MAXTIE) zmabend("maximum input tiepoints exceeded");
   if (lgeomlike)
      {
      for (i=0;i<ntiepp;i+=4)
         {
         dx = rpar[i];
         rpar[i] = rpar[i+2];
         rpar[i+2] = dx;
         dx = rpar[i+1];
         rpar[i+1] = rpar[i+3];
         rpar[i+3] = dx;
         }
      }
 
   tmaxx = 0.0;
   tmaxy = 0.0;
   tminx = 1.e20;
   tminy = 1.e20;
   if (n>MAXLSQ) nklsq = MAXLSQ; else nklsq = n;
   
   mz_alloc1((unsigned char **)&ptx,n+4,8);
   mz_alloc1((unsigned char **)&pty,n+4,8);
   mz_alloc1((unsigned char **)&optx,n+4,8);
   if (!zgeom) mz_alloc1((unsigned char **)&opty,n+4,8);
   
   /* transfer to vector format for thiessen; the slight random 
   perterbation was an experiment, can help with debugging the 
   rectangular grid input case */
   
   /*srand((unsigned)1);*/
   if (zgeom) ptr = 0; else ptr = 2;
   for (i=0;i<4;i++) normlz[i] = 0.0;
   for (i=0;i<n;i++)
      {
      /*  experimental code, also need srand() call above
      randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
      ptx[i] = rpar[ptr]*randout;
      randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
      pty[i] = rpar[ptr+1]*randout;*/
      
      ptx[i] = rpar[ptr];
      pty[i] = rpar[ptr+1];
      
      if (!zgeom)
         {
         optx[i] = rpar[ptr-2];
         opty[i] = rpar[ptr-1];
         }
      else optx[i] = rpar[ptr+2];
      
      normlz[0] += ptx[i];
      normlz[1] += pty[i];
      normlz[2] += optx[i];
      if (!zgeom) normlz[3] += opty[i];
      
      if (ptx[i]>tmaxx) tmaxx = ptx[i];
      if (ptx[i]<tminx) tminx = ptx[i];
      if (pty[i]>tmaxy) tmaxy = pty[i];
      if (pty[i]<tminy) tminy = pty[i];
      ptr += npoint;
      }
   free(rpar);
   for (i=0;i<4;i++) normlz[i] = (double)((int)(normlz[i]/(double)n+0.5));
   for (i=0;i<n;i++)
      {
      ptx[i] -= normlz[0];
      pty[i] -= normlz[1];
      optx[i] -= normlz[2];
      if (!zgeom) opty[i] -= normlz[3];
      }
   tmaxx -= normlz[0];
   tminx -= normlz[0];
   tmaxy -= normlz[1];
   tminy -= normlz[1];
   
   /* detect grid here and set gridnah, not necessary for polynom cases */
   /* some grids have null areas (-999,-999) for example off earth.  these */
   /* are required to be a grid, and the getlinef is applied to fill the */
   /* -999 areas with an approximate grid */
   
   if (!polynom)
      {
      cartogetline(ptx,pty,0,1,n,n,&gridnah,&null9);
      if (null9==1) goto nullarea;
      if (gridnah==0) goto endgrid;
      if (n%(gridnah+1)!=0) { gridnah = 0; goto endgrid; }
      gridnav = n/(gridnah+1)-1;
      if (n%(gridnav+1)!=0) { gridnah = 0; goto endgrid; }
      if ((double)gridnah<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      if ((double)gridnav<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      for (i=1;i<=gridnav;i++)
         {
         cartogetline(ptx,pty,i*(gridnah+1),1,gridnah+1,n,&k,&null9);
         if (null9==1) goto nullarea2;
         if (k!=gridnah) { gridnah = 0; goto endgrid; }
         }
      for (i=0;i<=gridnah;i++)
         {
         cartogetline(ptx,pty,i,gridnah+1,gridnav+1,n,&k,&null9);
         if (null9==1) goto nullarea2;
         if (k!=gridnav) { gridnah = 0; goto endgrid; }
         }
      }
   goto endgrid;
   nullarea:   /* try five more ways to find grid with null areas */
      cartogetline(ptx,pty,n-1,-1,n,n,&gridnah,&null9);
      if (null9==0)
         {
         if (n%(gridnah+1)!=0) zmabend("(nullarea) failure to find grid");
         gridnav = n/(gridnah+1)-1;
         if (n%(gridnav+1)!=0) zmabend("(nullarea) failure to find grid");
         goto nullarea2;
         }
      getline2(ptx,pty,0,1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline2(ptx,pty,n-1,-1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline3(ptx,pty,0,1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline3(ptx,pty,n-1,-1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      else zmabend("(nullarea) failure to find grid");
   nullarea2: 
      for (i=0;i<=gridnav;i++) gridfill(ptx,pty,i*(gridnah+1),1,gridnah);
      for (i=0;i<=gridnav;i++) gridfill(ptx,pty,(i+1)*(gridnah+1)-1,-1,gridnah);
      for (i=0;i<=gridnah;i++) gridfill(ptx,pty,i,gridnah+1,gridnav);
      for (i=0;i<=gridnah;i++) gridfill(ptx,pty,n-1-i,-gridnah-1,gridnav);
   endgrid:
      
   /* for the large case, the random formula scatters the points to be
   fitted across the area extended; will get duplicates but that is not
   a problem with a sample of 400; the sequence repeats for the the j 
   loop */
   
   for (j=0;j<npoint-2;j++)
      {
      k = 0;
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = (k*379+i*i)%n; else k = i;
         clsq[i] = ptx[k];
         clsq[i+nklsq] = pty[k];
         clsq[i+nklsq*2] = 1.0;
         clsq[i+nklsq*3] = ptx[k]*pty[k];
         clsq[i+nklsq*4] = ptx[k]*ptx[k];
         clsq[i+nklsq*5] = pty[k]*pty[k];
         clsq[i+nklsq*6] = ptx[k]*ptx[k]*ptx[k];
         clsq[i+nklsq*7] = ptx[k]*ptx[k]*pty[k];
         clsq[i+nklsq*8] = ptx[k]*pty[k]*pty[k];
         clsq[i+nklsq*9] = pty[k]*pty[k]*pty[k];
         clsqxy[i] = optx[k];
         if (!zgeom) clsqxy[i+nklsq] = opty[k];
         elsqxy[i] = optx[k];
         if (!zgeom) elsqxy[i+nklsq] = opty[k];
         }
      eps = 1.e-7;
      for (i=0;i<10;i++) csol[i+j*10] = 0.0;
      if (linear)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
      else if (keystone)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,4,&csol[j*10],eps,&ier);
      else if (quad)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,6,&csol[j*10],eps,&ier);
      else if (cubic)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,10,&csol[j*10],eps,&ier);
      else
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
      }
   if (ier!=0)
      {
      printf("j,npoint %d %d\n",j,npoint);
      zmabend("least squares fit failure");
      }
   zvparmd("mins",&miny,&icnt,&minsdf,1,0);
   zvparmd("maxs",&maxy,&icnt,&maxsdf,1,0);
   zvparmd("minl",&minx,&icnt,&minldf,1,0);
   zvparmd("maxl",&maxx,&icnt,&maxldf,1,0);
   minx -= normlz[0];
   maxx -= normlz[0];
   miny -= normlz[1];
   maxy -= normlz[1];
   if (minsdf==1) miny = tminy;
   if (maxsdf==1) maxy = tmaxy;
   if (minldf==1) minx = tminx;
   if (maxldf==1) maxx = tmaxx;
   if (lgeom) nah = 10; else if (geomv) nah = 50; else nah = 30;
   if (lgeom) nav = 10; else if (geomv) nav = 50; else nav = 30;
   if (nht!=0) nah = nht;
   if (nvt!=0) nav = nvt;
   dx = (tmaxx-tminx+tmaxy-tminy)*5.0;
   if (polynom) goto polyout;
   
   if (printit)
      {
      zprnt(8,3,csol,"lsq fit x'=ax+by+c.");
      if (!zgeom) zprnt(8,3,&csol[10],"lsq fit y'=dx+ey+f.");
      zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
      }
      
   for (i=0;i<nklsq;i++)
      {
      if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
      elsqxy[i] = elsqxy[i]-ptx[k]*csol[0]-pty[k]*csol[1]-csol[2];
      if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]-
             ptx[k]*csol[10]-pty[k]*csol[11]-csol[12];
      }
   if (printit) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
     
   ptr = n*npoint;
   if (!zgeom) ptr = ptr+2;
   ptx[n] = tminx-dx;
   pty[n] = (tminy+tmaxy)*0.5;
   ptx[n+1] = (tminx+tmaxx)*0.5;
   pty[n+1] = tmaxy+dx;
   ptx[n+2] = (tminx+tmaxx)*0.5;
   pty[n+2] = tminy-dx;
   ptx[n+3] = tmaxx+dx;
   pty[n+3] = (tminy+tmaxy)*0.5;
  
   fac = 0.5;         /* 0 for linear, 1.0 for cubic, 0.5 for 1/2 each */
   for (i=0;i<4;i++)
      {
      x = ptx[n+i];
      y = pty[n+i];
      optx[n+i] = csol[0]*x+csol[1]*y+csol[2]+fac*(csol[3]*x*y+
         csol[4]*x*x+csol[5]*y*y+csol[6]*x*x*x+csol[7]*x*x*y+
         csol[8]*x*y*y+csol[9]*y*y*y);
      if (!zgeom) opty[n+i] = csol[10]*x+csol[11]*y+csol[12]+fac*(csol[13]*x*y+
         csol[14]*x*x+csol[15]*y*y+csol[16]*x*x*x+csol[17]*x*x*y+
         csol[18]*x*y*y+csol[19]*y*y*y);
      }
   n += 4;

   /* ready for the big triangulation routine, con1,con2,con3 are
      mallocked in the subroutine (type is **) */
      
   if (gridnah==0)
      {
      printf("no grid detected\n");
      if (abendg&&n>1500) zmabend("user abend on no grid found");
      skinny = 0.0;
      thiessen(n,&nlret,reject,skinny,abendl,ptx,pty,&ntri,
			  &con1,&con2,&con3);
      }
   else
      {
      printf("grid automatically detected\n");
      /* this local routine substituted for library routine alz 11/23/10 */
      tgridxxx(n,&nlret,ptx,pty,&ntri,&con1,&con2,&con3,gridnah,csol,
         optx,opty,zgeom);
      }
   
   if (printit)
      {
      zprnt(4,1,&n,"nodes.");
      zprnt(4,1,&nlret,"edges.");
      zprnt(4,1,&ntri,"triangles.");
      }
   
   if (!plot)
      {
      /* solve triangles */
         
      mz_alloc2((unsigned char ***)&coeff,nrank,ntri,8);
      for (ix=0;ix<2;ix++)
         {
         if (zgeom&&ix>0) break;
         for (itri=0;itri<ntri;itri++)
	    {
	    work[0] = ptx[con1[itri]-1];
	    work[1] = ptx[con2[itri]-1];
	    work[2] = ptx[con3[itri]-1];
	    work[3] = pty[con1[itri]-1];
	    work[4] = pty[con2[itri]-1];
	    work[5] = pty[con3[itri]-1];
	    work[6] = 1.;
	    work[7] = 1.;
	    work[8] = 1.;
	    if (ix==0)
	       {
	       tab[0] = optx[con1[itri]-1];
	       tab[1] = optx[con2[itri]-1];
	       tab[2] = optx[con3[itri]-1];
	       }
	    else
	       {
	       tab[0] = opty[con1[itri]-1];
	       tab[1] = opty[con2[itri]-1];
	       tab[2] = opty[con3[itri]-1];
	       }
	    dgauss(work,tab,3,1.e-14,&ier);
	    for (j=0;j<3;j++) coeff[j+ix*3][itri] = tab[j];
	    if (ier!=0) coeff[0][itri] = 1.0E35;
	    }
	 }
         
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* start the geom of the grid, geomv goes to ibis file */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      tri = 0;
      ttrj1 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            isign = -1;
            found = 0;
            ttri = tri+ntri;
               
            for (k=0;k<ntri;k++)
               {
               tri = (ttri+((k+1)/2)*isign)%ntri;
               isign = -isign;
               if (coeff[0][tri]>1.0E34) continue;
               if (insidetri(x,y,
                       ptx[con1[tri]-1],pty[con1[tri]-1],
                       ptx[con2[tri]-1],pty[con2[tri]-1],
                       ptx[con3[tri]-1],pty[con3[tri]-1]))
                  {
                  found = 1;
                  break;
                  }
               }
            if (!found)
               {
               tri = ttri-ntri;
               if (j==1) tri = ttrj1;
               if (printit)
                 printf("grid point %f,%f not in triangle\n",x,y);
               }
            if (j==1) ttrj1 = tri;
            
            xx = coeff[0][tri]*x+coeff[1][tri]*y+coeff[2][tri];
            if (!zgeom) yy = coeff[3][tri]*x+coeff[4][tri]*y+coeff[5][tri];
            
            if (geomv)
               {
               vout[0][ptr] = x+normlz[0];
               vout[1][ptr] = y+normlz[1];
               vout[2][ptr] = xx+normlz[2];
               vout[3][ptr++] = yy+normlz[3];
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x+normlz[0];
               rout[ptr2+1] = y+normlz[1];
               rout[ptr2+2] = xx+normlz[2];
               rout[ptr2+3] = yy+normlz[3];
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x+normlz[0];
                  rout[ptr2+1] = y+normlz[1];
                  rout[ptr2+2] = xx+normlz[2];
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx+normlz[2];
                  rout[ptr2+1] = yy+normlz[3];
                  rout[ptr2+2] = x+normlz[0];
                  rout[ptr2+3] = y+normlz[1];
                  ptr2 += 4;
                  }
               }
            }
         }
   
   /* the next section is for output of polynomial fit cases */
   
   polyout:
   if (polynom)
      {
      if (printit)
         {
         printf("x' lsq fit, coeff of x = %15.8f\n",csol[0]);
         printf("x' lsq fit, coeff of y = %15.8f\n",csol[1]);
         printf("x' lsq fit, coeff of 1 = %15.8f\n",csol[2]);
         printf("x' lsq fit, coeff of xy = %15.8f\n",csol[3]);
         printf("x' lsq fit, coeff of xx = %15.8f\n",csol[4]);
         printf("x' lsq fit, coeff of yy = %15.8f\n",csol[5]);
         printf("x' lsq fit, coeff of xxx = %15.8f\n",csol[6]);
         printf("x' lsq fit, coeff of xxy = %15.8f\n",csol[7]);
         printf("x' lsq fit, coeff of xyy = %15.8f\n",csol[8]);
         printf("x' lsq fit, coeff of yyy = %15.8f\n",csol[9]);
         
         if (!zgeom)
            {
            printf("y' lsq fit, coeff of x = %15.8f\n",csol[10]);
            printf("y' lsq fit, coeff of y = %15.8f\n",csol[11]);
            printf("y' lsq fit, coeff of 1 = %15.8f\n",csol[12]);
            printf("y' lsq fit, coeff of xy = %15.8f\n",csol[13]);
            printf("y' lsq fit, coeff of xx = %15.8f\n",csol[14]);
            printf("y' lsq fit, coeff of yy = %15.8f\n",csol[15]);
            printf("y' lsq fit, coeff of xxx = %15.8f\n",csol[16]);
            printf("y' lsq fit, coeff of xxy = %15.8f\n",csol[17]);
            printf("y' lsq fit, coeff of xyy = %15.8f\n",csol[18]);
            printf("y' lsq fit, coeff of yyy = %15.8f\n",csol[19]);
            }
	 printf("normlz[0] = %15.8f\n", normlz[0]);
	 printf("normlz[1] = %15.8f\n", normlz[1]);
	 printf("normlz[2] = %15.8f\n", normlz[2]);
	 printf("normlz[3] = %15.8f\n", normlz[3]);
         zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
         }
      
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
         elsqxy[i] = elsqxy[i]
            -csol[0]*ptx[k]
            -csol[1]*pty[k]
            -csol[2]
            -csol[3]*ptx[k]*pty[k]
            -csol[4]*ptx[k]*ptx[k]
            -csol[5]*pty[k]*pty[k]
            -csol[6]*ptx[k]*ptx[k]*ptx[k]
            -csol[7]*ptx[k]*ptx[k]*pty[k]
            -csol[8]*ptx[k]*pty[k]*pty[k]
            -csol[9]*pty[k]*pty[k]*pty[k];
         if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]
            -csol[10]*ptx[k]
            -csol[11]*pty[k]
            -csol[12]
            -csol[13]*ptx[k]*pty[k]
            -csol[14]*ptx[k]*ptx[k]
            -csol[15]*pty[k]*pty[k]
            -csol[16]*ptx[k]*ptx[k]*ptx[k]
            -csol[17]*ptx[k]*ptx[k]*pty[k]
            -csol[18]*ptx[k]*pty[k]*pty[k]
            -csol[19]*pty[k]*pty[k]*pty[k];
         }
      if (printit) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
      
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* like above except use surface fit for all points */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            xx = csol[0]*x
                +csol[1]*y
                +csol[2]
                +csol[3]*x*y
                +csol[4]*x*x
                +csol[5]*y*y
                +csol[6]*x*x*x
                +csol[7]*x*x*y
                +csol[8]*x*y*y
                +csol[9]*y*y*y;
            if (!zgeom) yy = csol[10]*x
                +csol[11]*y
                +csol[12]
                +csol[13]*x*y
                +csol[14]*x*x
                +csol[15]*y*y
                +csol[16]*x*x*x
                +csol[17]*x*x*y
                +csol[18]*x*y*y
                +csol[19]*y*y*y;
                
            if (geomv)
               {
               vout[0][ptr] = x+normlz[0];
               vout[1][ptr] = y+normlz[1];
               vout[2][ptr] = xx+normlz[2];
               vout[3][ptr++] = yy+normlz[3];
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x+normlz[0];
               rout[ptr2+1] = y+normlz[1];
               rout[ptr2+2] = xx+normlz[2];
               rout[ptr2+3] = yy+normlz[3];
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x+normlz[0];
                  rout[ptr2+1] = y+normlz[1];
                  rout[ptr2+2] = xx+normlz[2];
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx+normlz[2];
                  rout[ptr2+1] = yy+normlz[3];
                  rout[ptr2+2] = x+normlz[0];
                  rout[ptr2+3] = y+normlz[1];
                  ptr2 += 4;
                  }
               }
            }
         }
      }
      
      /* Output array to IBIS file in col_ordr, or tiepoints parm file */
      
      if (geomv)
         {
         clen = (nah+1)*(nav+1);
         status = zvunit(&ibisOut,"out",1, NULL);
         status = IBISFileUnit(ibisOut,&ibis,"write",4,clen,0,"column");
         status = IBISFileSet(ibis,"fmt_default","doub",0);
         status = IBISFileUnitOpen(ibis);
         /*status = IBISFileOpen(ibisOut,&ibis,"write",4,clen,0,0);*/
        
         for (i=0;i<npoint;i++)
	    {
	    status = IBISColumnWrite(ibis,(char*)(vout[i]),i+1,1,clen);
	    if (status!=1) IBISSignal(ibis,status,0);
	    }
	 status = IBISFileClose(ibis,0);
         return;
         }
      else
         {
         status = zvunit(&parmOut,"out",1, NULL);
         status = zvparm("out",outnam,&nout,&outdf,1,0);
         status = zvpopen(outnam,"SA",&tiept);
         status = zvpout("NAH",&nah,"INT",1,0);
         status = zvpout("NAV",&nav,"INT",1,0);
         status = zvpout("TIEPOINT",rout,"REAL",ptr2,0);
         status = zvpclose();
         return;
         }
      } 
   else    /* new plot option */
      {
      mz_alloc2((unsigned char ***)&pout,2,ntri*5,4);
      
      p = 0;
      for (i=0;i<ntri;i++)
         {
         for (j=0;j<5;j++)
            {
            for (k=0;k<2;k++)
               {
               switch (j+k*5)
                  {
                  case 0: pout[k][p] = ptx[con1[i]-1]+normlz[0]; break;
                  case 1: pout[k][p] = ptx[con2[i]-1]+normlz[0]; break;
                  case 2: pout[k][p] = ptx[con3[i]-1]+normlz[0]; break;
                  case 3: pout[k][p] = ptx[con1[i]-1]+normlz[0]; break;
                  
                  case 5: pout[k][p] = pty[con1[i]-1]+normlz[1]; break;
                  case 6: pout[k][p] = pty[con2[i]-1]+normlz[1]; break;
                  case 7: pout[k][p] = pty[con3[i]-1]+normlz[1]; break;
                  case 8: pout[k][p] = pty[con1[i]-1]+normlz[1]; break;
                  
                  default: pout[k][p] = 0.0;
                  }
               }
            p++;
            }
         }
      clen = ntri*5;
      
      status = zvunit(&ibisOut,"out",1, NULL);
      status = IBISFileOpen(ibisOut,&ibis,"write",2,clen,0,0);
      for (i=0;i<2;i++)
	 {
	 status = IBISColumnWrite(ibis,(char*)(pout[i]),i+1,1,clen);
	 if (status!=1) IBISSignal(ibis,status,0);
	 }
      status = IBISFileClose(ibis,0);
      return;
      }
}
