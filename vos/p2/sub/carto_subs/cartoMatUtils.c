#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "zmabend.h"
#include "zvproto.h"

#include "cartoMatUtils.h"
#include "cartoMemUtils.h"
#include "cartoSortUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
static int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
static double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
  cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
  ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
static short int *vgroup;

int dceiling( double x )
{
   /* not a true ceiling routine, need numbers near
   floor to go to floor*/
   
   if (x>0) return((int)(x+0.999999));
   else     return((int)(x-0.000001));
}

double xzprod( double x1, double y1, double x2, double y2 )
{
   return(x2*y1-x1*y2);
}

/* signed triangular area, for polygon use xp,yp as fixed point */
/* in (E,N) coordinates,        clockwise is positive area */
/* in (N,E) coordinates, counterclockwise is positive area */

double triarea( double xp, double yp, double x1, double y1, double x2, double y2 )
{
   return(0.5*(xp*y1-x1*yp+x1*y2-x2*y1+x2*yp-xp*y2));
}

void lin2( double * a, double * b, double * x, double eps )
{
   /* for case where the first equation is not null */
   /* provides a solution for the singular case and causes x[1] =-1 */
   /* stored by column in a */

   if (fabs(a[0])>=fabs(a[1]))
      {
      if (fabs(a[0])<eps)
         {
         x[0] = 0.5;
         x[1] = 0.5;
         return;
         }
      a[2] /= a[0]; b[0] /= a[0];
      a[3] -= a[2]*a[1]; b[1] -= b[0]*a[1];
      if (fabs(a[3])>eps)
	 {
	 x[1] = b[1]/a[3];
	 x[0] = b[0]-x[1]*a[2];
	 }
      else { x[1] = -1.; x[0] = b[0]+a[2]; }
      }
   else
      {
      a[3] /= a[1]; b[1] /= a[1];
      a[2] -= a[3]*a[0]; b[0] -= b[1]*a[0];
      if (fabs(a[2])>eps)
	 {
	 x[1] = b[0]/a[2];
	 x[0] = b[1]-x[1]*a[3];
	 }
      else { x[1] = -1.; x[0] = b[1]+a[3]; }
      }
   return;
}

void segxseg( double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double * w )
{
   /* The first segment is x1,y1 to x2,y2; the second segment is   */
   /* x3,y3 to x4,y4; the return w[0] is the index of the crossing */
   /* of the first segment; 0. is at x1,y1; 1. is at x2,y2 other   */
   /* values are linearly interpolated or extrapolated; w[1] is    */
   /* the same for the second segment                              */

   double aa[4],bb[2];

   aa[0] = x2-x1; aa[1] = y2-y1;
   aa[2] = x3-x4; aa[3] = y3-y4;
   bb[0] = x3-x1; bb[1] = y3-y1;
   lin2(aa,bb,w,1.e-8);
   return;
}


void insert_seg( int jj, int * ccount, double * p4max, double xbig, double ybig, double xjbig, double yjbig, int wchcall )
{
   int ccdel[VORMAXPOLY];
   int i,k,tcross,tdelete,srong,niter,iter,kmin=0,kc,kcmin=0,whch,irep;
   int ptr,z12,lcross,sccx[10],srl[10];
   double xm,ym,xbisv,ybisv,xbis,ybis,tw[2],xzprod();
   double xcr,ycr,ccrad,pmax,z2min=0,rat;
   double z1,z2=0,sx[10],sy[10],dmin,px,py,dot,hyp,dist,thet,dx,dy;
   
   /*if(bgg)printf("enter insert_seg jj %d\n",jj);*/
   xm = (xbig+xjbig)*.5;
   ym = (ybig+yjbig)*.5;
   xbisv = ym-yjbig;
   ybisv = xjbig-xm;
   xbis = xbisv+xm; ybis = ybisv+ym;
   /*if(bgg)printf("xm,ym %f %f\n",xm,ym);
   if(bgg)printf("xbis,ybis %f %f  ",xbis,ybis);
   if(bgg)printf("xbisv,ybisv %f %f\n",xbisv,ybisv);*/
   tcross = 0; tdelete = 0;
   for (k=0;k<*ccount;k++)
      {
      segxseg(xm,ym,xbis,ybis,cc1x[k],cc1y[k],cc2x[k],cc2y[k],tw);
      /*if(bgg)printf("tw %f %f\n",tw[0],tw[1]);*/
      if (tw[1]>=0. && tw[1]<=1.) lcross = 1; else lcross = 0.;
      xcr = xbisv*tw[0]+xm;
      ycr = ybisv*tw[0]+ym;
      /*if(bgg)printf("xcr,ycr %f %f\n",xcr,ycr);*/
      z1 = xzprod(cc1x[k]-xm,cc1y[k]-ym,xbisv,ybisv);
      z2 = xzprod(cc2x[k]-xm,cc2y[k]-ym,xbisv,ybisv);
      /*if(bgg)printf("z1,z2 %f %f\n",z1,z2);*/
      srong = 0; z12 = 1;
      if (z1<0.) srong++;
      if (z2<0.) { srong++; z12 = 2; }
      /*if(bgg)printf("a-srong,lcross %d %d\n",srong,lcross);*/
      if (lcross==0 && srong==1) srong = 0;
      if (lcross==1 && srong!=1)
	 {
	 if (fabs(z1)<fabs(z2)) z12 = 1+srong/2; else z12 = 2-srong/2;
	 srong = 1;
	 /*if(bgg)printf("end point enforced\n");*/
	 }
      if (lcross==1)
	 {
	 sx[tcross] = xcr;
	 sy[tcross] = ycr;
	 sccx[tcross] = k;
	 srl[tcross] = z12; tcross++;
	 }
      ccdel[k] = srong;
      if (srong==2) tdelete++;
      /*if(bgg)printf("b-srong,lcross %d %d\n",srong,lcross);*/
      }
   /*if(bgg)printf("tcross,tdelete %d %d\n",tcross,tdelete);*/
   if (tcross<2 && tdelete==0) return;
   if (tcross<2)
      {
      /*if(bgg)printf("add polygon cross\n");*/
      niter = 2-tcross;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (k=0;k<*ccount;k++)
	    {
	    if (ccdel[k]==1) continue;
	    px = cc1x[k]-xbis;
	    py = cc1y[k]-ybis;
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       z2 = xzprod(cc2x[k]-px,cc2y[k]-py,xbisv,ybisv);
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kmin = k; z2min = z2;
	    }
	 ccdel[kmin] = 1;
	 sx[tcross] = cc1x[kmin];
	 sy[tcross] = cc1y[kmin];
	 sccx[tcross] = kmin;
	 if (z2min<0) srl[tcross] = 2; else srl[tcross] = 1;
	 tcross++;
	 }
      }
   if (tcross>2)
      {
      /*if(bgg)printf("elim polygon cross\n");*/
      niter = tcross-2;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (kc=0;kc<tcross;kc++)
	    {
	    k = sccx[kc];
	    whch = srl[kc];
	    if (whch==1)
	       { px = cc1x[k]-xbis; py = cc1y[k]-ybis; }
	    else
	       { px = cc2x[k]-xbis; py = cc2y[k]-ybis; }
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kcmin = kc; kmin = k;
	    }
	 ccdel[kmin] = 0;
	 for (kc=kcmin;kc<tcross;kc++)
	    {
	    sx[kc] = sx[kc+1];
	    sy[kc] = sy[kc+1];
	    sccx[kc] = sccx[kc+1];
	    srl[kc] = srl[kc+1];
	    }
	 tcross--;
	 }
      }
   if (tcross!=2) zmabend("tcross.ne.2");
   if (sx[0]==sx[1] && sy[0]==sy[1]) return;
   for (irep=0;irep<2;irep++)
      {
      whch = srl[irep];
      ptr = sccx[irep];
      if (whch==1)
	 {
	 cc1x[ptr] = sx[irep];
	 cc1y[ptr] = sy[irep];
	 }
      else
	 {
	 cc2x[ptr] = sx[irep];
	 cc2y[ptr] = sy[irep];
	 }
      }
   ptr = 0;
   for (k=0;k<*ccount;k++)
      {
      cc1x[ptr] = cc1x[k]; cc1y[ptr] = cc1y[k];
      cc2x[ptr] = cc2x[k]; cc2y[ptr] = cc2y[k];
      ccdel[ptr] = ccdel[k];
      switch(wchcall)
	 {
	 case 3: ccrx[ptr] = ccrx[k]; ccry[ptr] = ccry[k];
		 ccgrp[ptr] = ccgrp[k];
	 case 2: ccjj[ptr] = ccjj[k]; break;
	 /*case 1: break;*/
	 }
      if (ccdel[k]<2) ptr++;
      }
   z1 = xzprod(sx[0]-xbig,sy[0]-ybig,sx[1]-xbig,sy[1]-ybig);
   if (z1<0) whch = 0; else whch = 1;
   cc1x[ptr] = sx[whch]; cc1y[ptr] = sy[whch];
   cc2x[ptr] = sx[1-whch]; cc2y[ptr] = sy[1-whch];
   switch(wchcall)
      {
      case 3: ccrx[ptr] = xjbig; ccry[ptr] = yjbig; ccgrp[ptr] = vgroup[jj];
      case 2: ccjj[ptr] = jj; break;
      /*case 1: break;*/
      }
   *ccount = ptr+1;
   pmax = 0.;
   for (i=0;i<*ccount;i++)
      {
      dx = cc1x[i]-xbig; dy = cc1y[i]-ybig;
      ccrad = dx*dx+dy*dy;
      if (ccrad>pmax) pmax = ccrad;
      }
   *p4max = 4.*pmax;
   return;
}

void thiessen( int npoints, int * nlinret, double reject, double skinny, int abendi, double * ptx, double * pty, int * ntriang, int ** tcon1, int ** tcon2, int ** tcon3 )
{

   int *bptr,**hash1,**hash2,**tc,bjj[VORMAXPOLY],blink[VORMAXPOLY];
   double *bufio,*buf;

   int np,i,j,npts,linct,ip1,hlen1,hlen2;
   int topstop,botstop,dirj,jj,k,ptr,jsave,jdup,trict,ibig;
   int t0,t1,t2,t3,tt3,tt,h12,h123,l,triptr,ttptr,ccount,temp;
   double dx,dy,dx2,dist2,xlink,ylink,low1,low2,upp1,upp2,tstarea,diam;
   double tw[2],p4max,xbig,ybig,xjbig,yjbig;
   double triarea();

   bgg = 0; bggset = -1;
   
   
   /* read the data */

   np = npoints*2;
   mz_alloc1((unsigned char **)&bufio,np,8);
   mz_alloc1((unsigned char **)&buf,np,8);
   mz_alloc1((unsigned char **)&bptr,npoints,4);
   for (i=0;i<10000;i++)
      {
      hlen1 = (npoints*4+1000)+i;
      if (hlen1%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen1%j==0) break;
	 if (j==37) goto rnd1;
	 }
      }
   rnd1: /*if(bgg)printf("hash length = %d\n",hlen1);*/
   mz_alloc2((unsigned char ***)&hash1,4,hlen1,4);
   for (i=0;i<hlen1;i++) hash1[0][i] = 0;

   low1 = 1.e20; low2 = 1.e20; upp1 = -1.e20; upp2 = -1.e20;
   for (i=0;i<npoints;i++)
      {
      bufio[2*i] = ptx[i];
      bufio[2*i+1] = pty[i];
      low1 = MIN(low1,ptx[i]);
      low2 = MIN(low2,pty[i]);
      upp1 = MAX(upp1,ptx[i]);
      upp2 = MAX(upp2,pty[i]);
      }
   diam = 5*((upp1-low1)+(upp2-low2));
   low1 = low1-diam;
   low2 = low2-diam;
   upp1 = upp1+diam;
   upp2 = upp2+diam;

   /* apply the voronoi routine */

   npts = np/2; ccount = 0; linct = 0; trict = 0;
   for (i=0;i<np;i++) buf[i] = bufio[i];
   for (i=0;i<npts;i++) bptr[i] = i+1;
   
   sort88(buf,bptr,npts);
   sortrec88(bufio,bptr,npts);
   
   ptr = 0;
   for (i=0;i<npts;i++)
      {
      if (bufio[i*2]<low1 || bufio[i*2]>upp1 ||
	 bufio[i*2+1]<low2 || bufio[i*2+1]>upp2)
	 { printf("outside point removed\n"); continue; }
      if (i!=0) if (bufio[i*2]==buf[ptr*2-2] &&
	 bufio[i*2+1]==buf[ptr*2-1])
	    {
	    if (abendi) zmabend("duplicate point abend");
	    printf("duplicate point rejected\n");
	    continue;
	    }
       for (j=1;ptr-j>=0;j++)
	 {
	 if (fabs(bufio[i*2]-buf[(ptr-j)*2])>reject) break;
	 if ((fabs(bufio[i*2]-buf[(ptr-j)*2])+
	    fabs(bufio[i*2+1]-buf[(ptr-j)*2+1]))<reject)
	    {
	    printf("i,j,ptr %d %d %d\n",i,j,ptr);
	    printf("bufio[i*2],buf[(ptr-j)*2] %12.3f %12.3f\n",
	                 bufio[i*2],buf[(ptr-j)*2]);
	    printf("bufio[i*2+1],buf[(ptr-j)*2+1] %12.3f %12.3f\n",
	                 bufio[i*2+1],buf[(ptr-j)*2+1]);
	                 
	    if (abendi) zmabend("close point abend");
	    printf("close point rejected\n");
	    goto clpt;
	    }
	 }
      buf[ptr*2] = bufio[i*2];
      buf[ptr*2+1] = bufio[i*2+1]; bptr[ptr++] = bptr[i];
      clpt: continue;
      }
   free(bufio);
   if (ptr==0) return; npts = ptr;
   for (ibig=0;ibig<npts;ibig++)
      {
      if (ibig%10000==9999) printf("%d pts processed\n",ibig);
      xbig = buf[ibig*2];
      ybig = buf[ibig*2+1];
      /*if (bggset>=0) printf("xbig,ybig %f %f\n",xbig,ybig);*/
      if (ibig==bggset) bgg = 1; else bgg = 0;
      cc1x[0] = upp1; cc1y[0] = upp2; cc2x[0] = low1; cc2y[0] = upp2;
      cc1x[1] = upp1; cc1y[1] = low2; cc2x[1] = upp1; cc2y[1] = upp2;
      cc1x[2] = low1; cc1y[2] = low2; cc2x[2] = upp1; cc2y[2] = low2;
      cc1x[3] = low1; cc1y[3] = upp2; cc2x[3] = low1; cc2y[3] = low2;
      ccjj[0] = ibig; ccjj[1] = ibig;
      ccjj[2] = ibig; ccjj[3] = ibig;
      p4max = upp1+upp2-low1-low2; p4max = p4max*p4max; ccount = 4;
      topstop = 0; botstop = 0;
      for (j=0;j<2*npts;j++)
	 {
	 dirj = j%2;
	 jj = ibig+(dirj*2-1)*((j+2)/2);
	 /*if(bgg)printf("a-ibig,j,jj %d %d %d\n",ibig,j,jj);
	 if(bgg)printf("dirj,tops,bots %d %d %d\n",dirj,topstop,botstop);*/
	 if (jj<0 || jj>=npts) continue;
	 if (dirj==0 && topstop) continue;
	 if (dirj==1 && botstop) continue;
	 xjbig = buf[jj*2];
	 yjbig = buf[jj*2+1];
	 dx = xjbig-xbig; dy = yjbig-ybig; dx2 = dx*dx;
	 dist2 = dx2+dy*dy;
	 /*if(bgg)printf("xjbig,yjbig,dist2 %f %f %f\n",xjbig,yjbig,dist2);*/
	 if (dist2<p4max)
	    insert_seg(jj,&ccount,&p4max,xbig,ybig,xjbig,yjbig,2);
	 /*if(bgg)printf("set-stop %f %f %f\n",xbig,xjbig,p4max);*/
	 if (dx2>p4max)
	    { if (dirj==0) topstop = 1; else botstop = 1; }
	 if (topstop&&botstop) break;
	 }

/* output the polygon in chain order, zero length edges are kept
   but can be thinned by user proc */

      for (i=0;i<ccount;i++) blink[i] = 0;
      ptr = 0;
      xlink = cc2x[0]; ylink = cc2y[0]; jsave = 0;
      for (i=0;i<ccount;i++)
	 {
	 jdup = 0;
	 for (j=0;j<ccount;j++)
	    {
	    if (blink[j]) continue;
	    if (cc1x[j]!=xlink || cc1y[j]!=ylink) continue;
	    if (j==jsave) continue;
	    jdup++;
	    if (jdup>1&&cc1x[jsave]==cc2x[jsave]&&
			cc1y[jsave]==cc2y[jsave]) break;
	    jsave = j;
	    }
	 if (ccjj[jsave]!=ibig) bjj[ptr++] = ccjj[jsave];
	 xlink = cc2x[jsave]; ylink = cc2y[jsave];
	 blink[jsave] = 1;
	 }
      for (i=0;i<ptr;i++)
	 {
	 if (i==(ptr-1)) ip1 = 0; else ip1 = i+1;
	 if (bjj[i]!=ibig && bjj[ip1]!=ibig)
	    {
	    if (bjj[i]==bjj[ip1]) continue;
	    tstarea = triarea((double)(buf[ibig*2]),(double)(buf[ibig*2+1]),
		       (double)(buf[bjj[i]*2]),(double)(buf[bjj[i]*2+1]),
		       (double)(buf[bjj[ip1]*2]),(double)(buf[bjj[ip1]*2+1]));
            if (tstarea<skinny) continue;
	    /*if (bgg) printf("***************saved\n");*/
	    t1 = bptr[ibig];
	    t2 = bptr[bjj[i]];
	    t3 = bptr[bjj[ip1]];
	    if (t1>t2) { tt=t1; t1=t2; t2=tt; }
	    if (t1>t3) { tt=t1; t1=t3; t3=tt; }
	    if (t2>t3) { tt=t2; t2=t3; t3=tt; }
	    h123 = (t1*7+t2*330+t3*4199)%hlen1-1;
	    for (j=0;j<hlen1;j++)
	       {
	       h123++; if (h123>=hlen1) h123 = 0;
	       if (hash1[0][h123]==0) goto stor1;
	       if (hash1[1][h123]!=t1) continue;
	       if (hash1[2][h123]!=t2) continue;
	       if (hash1[3][h123]!=t3) continue;
	       hash1[0][h123]++; goto done1;
	       }
	    printf("debug:trict %d\n",trict);
	    /*mifcb2 = mi_fopen("hash1dbg","w");
	    lab[0] = '\0';
	    mi_labwr(mifcb2,lab,0,0,"graphics","polytr");
	    mg_put(mifcb2,1,4,hlen1,hash1[0]);
	    mg_put(mifcb2,2,4,hlen1,hash1[1]);
	    mg_put(mifcb2,3,4,hlen1,hash1[2]);
	    mg_put(mifcb2,4,4,hlen1,hash1[3]);
	    fclose(mifcb2);
	    zmabend("hash error 1");*/
	    stor1: hash1[1][h123] = t1;
		   hash1[2][h123] = t2;
		   hash1[3][h123] = t3;
		   hash1[0][h123] = 1;
		   /*if (bgg)
		      {
		      printf("###########saving h123 %d\n",h123);
		      printf("ibig,ptr,i %d %d %d\n",ibig,ptr,i);
		      for (k=0;k<ptr;k++)
			 printf("k,bjj[k],bptr[bjj[k]] %d %d %d\n",
				   k,bjj[k],bptr[bjj[k]]);
		      }*/
		   trict++;
		   continue;
	    done1: continue;
	    }
	 }
      }

   /* condense the triangles, then remove intersecting triangles that
       result from perfect grid squares, prefer higher count duplicated
       triangles; uses a hash table of pairs of points from triangles */

   free(buf); free(bptr);
   mz_alloc2((unsigned char ***)&tc,4,trict,4);
   triptr = 0;

   for (i=3;i>0;i--)
      {
      for (j=0;j<hlen1;j++)
	 {
	 if (hash1[0][j]<i) continue;
	 tc[0][triptr] = hash1[0][j];
	 tc[1][triptr] = hash1[1][j];
	 tc[2][triptr] = hash1[2][j];
	 tc[3][triptr++] = hash1[3][j];
	 hash1[0][j] = 0;
	 }
      }
   mz_free2((unsigned char **)hash1,4);
   if (triptr!=trict) zmabend("hash error 2");
   for (i=0;i<10000;i++)
      {
      hlen2 = (trict*3+1000)+i;
      if (hlen2%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen2%j==0) break;
	 if (j==37) goto rnd2;
	 }
      }
   rnd2: /*if(bgg)printf("hash length = %d\n",hlen2);*/
   mz_alloc2((unsigned char ***)&hash2,3,hlen2,4);
   for (i=0;i<hlen2;i++) hash2[0][i] = -1;
   for (i=0;i<trict;i++)
      {
      if (i%10000==9999) printf("%d triangles processed\n",i);
      t0 = tc[0][i];
      for(j=0;j<3;j++)
	 {
	 t1 = tc[j/2+1][i];
	 t2 = tc[(j+8)/3][i];
	 t3 = tc[3-j][i];
	 h12 = (t1*7+t2*330)%hlen2-1;
	 for (k=0;k<hlen2;k++)
	    {
	    h12++; if (h12>=hlen2) h12 = 0;
	    if (hash2[0][h12]==(-1)) goto stor2;
	    if (hash2[1][h12]!=t1) continue;
	    if (hash2[2][h12]!=t2) continue;
	    /* may enter more than once */
	    ttptr = hash2[0][h12];
	    if (tc[0][ttptr]==(-99)) continue;
	    for (l=1;l<4;l++)
	       {
	       tt3 = tc[l][ttptr];
	       if (tt3!=t1&&tt3!=t2) break;
	       }
	    segxseg(ptx[t1-1],pty[t1-1],ptx[t3-1],pty[t3-1],
		    ptx[t2-1],pty[t2-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    segxseg(ptx[t2-1],pty[t2-1],ptx[t3-1],pty[t3-1],
		    ptx[t1-1],pty[t1-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    }
	 zmabend("hash error 3");
	 stor2: hash2[0][h12] = i;
		hash2[1][h12] = t1;
		hash2[2][h12] = t2;
		continue;
	 done2: tc[0][i] = -99;
		break;
	 }
      }

   mz_free2((unsigned char **)hash2,3);
   mz_alloc1((unsigned char **)tcon1,triptr,4);
   mz_alloc1((unsigned char **)tcon2,triptr,4);
   mz_alloc1((unsigned char **)tcon3,triptr,4);
   trict = 0;
   for (i=0;i<triptr;i++)
      {
      if (tc[0][i]==(-99)) continue;
      (*tcon1)[trict] = tc[1][i];
      (*tcon2)[trict] = tc[2][i];
      (*tcon3)[trict++] = tc[3][i];
      }

   /* now sort the triangles in vertical order, as required by tiegrid */

   mz_free2((unsigned char **)tc,4);
   mz_alloc1((unsigned char **)&buf,trict,8);
   mz_alloc1((unsigned char **)&bptr,trict,4);
   for (i=0;i<trict;i++)
      {
      buf[i] = ptx[(*tcon1)[i]-1]+ptx[(*tcon2)[i]-1]+ptx[(*tcon3)[i]-1];
      bptr[i] = i+1;
      }
   sort8(buf,bptr,trict);
   sortrec4(*tcon1,bptr,trict);
   sortrec4(*tcon2,bptr,trict);
   sortrec4(*tcon3,bptr,trict);

   /* triangles also have to be clockwise */

   for (i=0;i<trict;i++)
      {
      tstarea = triarea((double)(ptx[(*tcon1)[i]-1]),(double)(pty[(*tcon1)[i]-1]),
		 (double)(ptx[(*tcon2)[i]-1]),(double)(pty[(*tcon2)[i]-1]),
		 (double)(ptx[(*tcon3)[i]-1]),(double)(pty[(*tcon3)[i]-1]));
      if (tstarea>0.0) continue;
      temp = (*tcon3)[i]; (*tcon3)[i] = (*tcon1)[i]; (*tcon1)[i] = temp;
      }

   *ntriang = trict;             /* can check this with Euler 2P-2-CVXHULL*/
   *nlinret = npoints+trict-1;   /* using Euler */
   printf("%d points %d lines %d triangles\n",npoints,*nlinret,trict);
   free(buf); free(bptr);
   return;
}

int insidetri( double x, double y, double x1, double y1, double x2, double y2, double x3, double y3 )
{
   if ((x-x1)*(y2-y1)-(y-y1)*(x2-x1)>0.) return(0);
   if ((x-x2)*(y3-y2)-(y-y2)*(x3-x2)>0.) return(0);
   if ((x-x3)*(y1-y3)-(y-y3)*(x1-x3)>0.) return(0);
   return(1);
}

 
