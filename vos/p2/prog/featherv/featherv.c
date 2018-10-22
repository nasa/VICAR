#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"
/*#include "cartoVicarProtos.h"*/

#define NUMINFILES 400

int *pstack,*istack,*vstack,dbg;
float fhnl,fhns;
float pi2 = 1.5707963;
float pi2inv = 0.6366197;

void propagate(ival,moore_bix,previx,i,redfeather,redtns,currix)
   int ival,previx,i,redfeather,redtns,currix;
   short int **moore_bix;
{
   int ptr,tp,ti,tv,upix;

   ptr = 1;
   pstack[0] = previx;
   istack[0] = i;
   vstack[0] = ival;
   
   /* only 3 directions needed for this Moore algorithm */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      moore_bix[tp][ti] = (short int)tv;
      if (ti+1<redtns)
         {
         if ((int)moore_bix[tp][ti+1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti+1;
            vstack[ptr++] = tv+1;
            }
         }
      if (ti>0)
         {
         if ((int)moore_bix[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      upix = (tp-1+redfeather)%redfeather;
      if (upix!=currix)
         {
         if ((int)moore_bix[upix][ti]>tv+1)
            {
            pstack[ptr] = upix;
            istack[ptr] = ti;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   return;
}

/************************************************************************/
/*									*/
/*		open_files()						*/
/*	This routine opens the input and output image files		*/
/*									*/
/************************************************************************/

int i_unit,o_unit;			/* input unit, output unit */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
int inpline, inpsamp;			/* size of primary input */
char fmt_str[10];                       /* format of primary input */
 
void open_files()
{
  int status,tsize[4],sizepcnt,sizedef;
  
  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, NULL);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str, NULL);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  /***************************/
  /* open up the output file */
  /***************************/
  /*zvsize( &sline, &ssamp, &nline, &nsamp, &inpline, &inpsamp);*/
  /*zvsize no good for negative parameters: sline,ssamp*/
  zvget(i_unit,"NL",&inpline,"NS",&inpsamp, NULL);
  zvparm("SIZE",tsize,&sizepcnt,&sizedef,4,0);
  if (!sizedef)
     {
     sline = tsize[0];
     if (sizepcnt>=2) ssamp = tsize[1];
     if (sizepcnt>=3) nline = tsize[2];
     if (sizepcnt>=4) nsamp = tsize[3];
     }
  else
     {
     zvparm("SL",&sline,&sizepcnt,&sizedef,1,0);
     zvparm("SS",&ssamp,&sizepcnt,&sizedef,1,0);
     zvparm("NL",&nline,&sizepcnt,&sizedef,1,0);
     zvparm("NS",&nsamp,&sizepcnt,&sizedef,1,0);
     }
  if (nline==0) nline = inpline;
  if (nsamp==0) nsamp = inpsamp;
  
  status=zvunit( &o_unit, "OUT", 1, NULL);
  /* note that zvopen is intelligent enough to default to the same */
  /* format as the input file.  */
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp, "U_FORMAT","HALF",
		"OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",
                "O_FORMAT",fmt_str,"TYPE","IMAGE", NULL);
  return;
}

float rampcalc(tns,tnl,dz,countz,j,q1,q2,zbarj)
   int j,q1,q2,**countz;
   float tns,tnl,**dz,zbarj;
{
   /* faster rectilinear interpolation, see commented code below for angular
   interpolation */
   float x,y,xpy,val0,val1;
   
   x = tns*fhns;
   y = tnl*fhnl;
   xpy = 1.0/(x+y+0.005);
   val0 = -y*dz[q1][j]; /*alz australia blue validation of - sign*/
   val1 = x*dz[q2][j];
   /*if (dbg) printf("B:x,y,val0,val1,zbarj,q1,q2,j %f %f %f %f %f %d %d %d\n",
                      x,y,val0,val1,zbarj,q1,q2,j);*/
   if (countz[q1][j]==0&&countz[q2][j]==0) return -zbarj;
   else if (countz[q1][j]==0) return val1-zbarj;
   else if (countz[q2][j]==0) return val0-zbarj;
   else return y*xpy*val0+x*xpy*val1-zbarj; /*alz australia blue validation of -zbar*/
}
 
/*float rampcalc(tns,tnl,dz,countz,j,q1,q2,zbarj)
   int j,q1,q2;
   float tnl,tns,**dz,zbarj;
{
   ** angular interpolation save this code **
   float x,y,xpy,val0,val1,alpha;
   x = tns*fhns;
   y = tnl*fhnl;
   if (x>0.01&&y>0.01) alpha = atan(y/x);
   else if (x>0.01) alpha = 0.0;
   else alpha = pi2;
   val0 = y*dz[q1][j];
   val1 = x*dz[q2][j];
   return alpha*pi2inv*val0+(pi2-alpha)*pi2inv*val1-zbarj;, etc.
}*/


void main44(void)
{
   int i,j,k,kk,status,colcount,coldef,finfo,ibis,nrec,adjust;
   int filcount,notfound,dfeather,dummy,iread,currix,maxmfns;
   int previx,tempnl,tempns,datafound,rightbrk=0,leftbrk=0,midns,iout;
   int listix=0,ix2,ix3,tns,fildef,numcross,maxcross,*bufused;
   int *mfsl,*mfss,*mfnl,*mfns,**nbrj,**countz,*ibisrev,**ibisnbrj;
   int *fileix,*ibisix,*openstat,*vunit,*rotix,*lineread,*bufix;
   int edge,thresh,nibble,lnibble,rnibble,nthresh,lthresh,rthresh;
   int ixl,ixr,ibj,nseq,nincr,cloudout,*firstread,bix,ibix,nbrseq;
   int cols[7],inlist[50],sampoff[50],progress,thrcount,tval;
   int ramp,rdkthr,rdiffthr,rmoore,rcols[10],kq,jq,bixq,ix2q,ix3q;
   int zcnt,iix,loff,soff,rcolcount,rcoldef,jnbr,fmt_byte,dimax;
   int kmatch,redfeather,redmfns,moorefac,ired,redtns,curline,mooremax;
   int ix3red,ix3qred,ibjq,*rereadmemory,rereadline,moorectr,mooreend;
   int geotiff,labnl,labns,len,tolerct,tolerdf,img,rot1,rot2;
   int sctype1,sctype2,*ilcorner,*iscorner,*inl,*ins,isav,moorenbl;
   int igncase;
   short int ***footprnt,*bufout,*bufhit,pself,pnbr,i2dkthr=0;
   short int **inbuf,***moore,mooretst=0,mnbr;
   char *fnames,infil[NUMINFILES][99],*name1,*name2;
   char *p,fmt_str2[10];
   char *labelstr1,*labelstr2,scalestr[50],transstr[133];
   double t[6],tinv[6],r[6],rinv[6],corner[4],rcorner[4];
   double tout[6],toutinv[6],scale1,scale2,voff,bcor,dcor,dpow;
   double scale11,scale12,scale21,scale22,roundval1,roundval2;
   double toler,xcorner,ycorner,lcorner,scorner;
   
   double *mffac1,**sumz,ztot;
   float vi,usum,ovi=0,fcloudout,di,odi,lsum,**dz,*zbar,gorefac,rpow;
   float zramp=0,vself,vnbr,hnl,hns,locnl,locns,**ibisdz,fbigthresh=0;
   float vdiff,vimax=0;
   
   /* initialize, fetch params */

   zifmessage("featherv version Thu Oct 09 2008");
   
   if (zvptst("factor")) adjust = 1; else adjust = 0;
   if (zvptst("add")) adjust = adjust*100+2;
   if (zvptst("addzmask")) adjust = adjust*100+3;
   if (adjust>100) zmabend("Cannot use two adjust keywords");
   
   if (zvptst("geotiff")) geotiff = 1; else geotiff = 0;
   
   zvp("dfeather",&dfeather,&dummy);
   zvp("moorefac",&moorefac,&dummy);
   if (moorefac%2!=1) zmabend("moorefac parm must be an odd integer");
   redfeather = dfeather/moorefac;
   if (redfeather<2) zmabend("dfeather must be at least twice moorefac");
   if (redfeather>65535) zmabend("dfeather must be less than 65535 times moorefac");
   mooreend = moorefac-1;
   moorectr = mooreend/2;
   dfeather = redfeather*moorefac;
   zvp("moorenbl",&moorenbl,&dummy);
   igncase = zvptst("igncase");
   
   status = zvp("moorepow",&rpow,&dummy);
   if (rpow>1.001) dpow = (double)rpow; else dpow = 1.0;
   mooremax = zvptst("mooremax");
   if (mooremax&&dpow>1.001) zmabend("can't use both mooremax and moorepow"); 
   
   edge = zvptst("edge");
   progress = zvptst("progress");
   status = zvp("thresh",&thresh,&dummy);
   status = zvp("nibble",&nibble,&dummy);
   status = zvp("lnibble",&lnibble,&dummy);
   status = zvp("rnibble",&rnibble,&dummy);
   status = zvp("nthresh",&nthresh,&dummy);
   status = zvp("lthresh",&lthresh,&dummy);
   status = zvp("rthresh",&rthresh,&dummy);
   status = zvp("nseq",&nseq,&dummy);
   status = zvp("nincr",&nincr,&dummy);
   if (lnibble==0&&rnibble==0) { lnibble = nibble; rnibble = nibble; }
   if (lthresh==0&&rthresh==0) { lthresh = nthresh; rthresh = nthresh; }
   
   status = zvpcnt("inp",&filcount);
   filcount -= geotiff;
   zvparm("cols",cols,&colcount,&coldef,7,0);
   status = zvunit(&finfo,"inp",filcount, NULL);
   status = IBISFileOpen(finfo,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(finfo,status,1);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"A99",cols[0]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[1]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[2]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[3]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",cols[4]);
   IBISFileGet(ibis,"nr",&nrec,1,1,0);
   mz_alloc1((unsigned char **)&fnames,100*nrec,1);
   mz_alloc1((unsigned char **)&mfsl,nrec,4);
   mz_alloc1((unsigned char **)&mfss,nrec,4);
   mz_alloc1((unsigned char **)&mfnl,nrec,4);
   mz_alloc1((unsigned char **)&mfns,nrec,4);
   mz_alloc1((unsigned char **)&ibisrev,nrec,4);
   status = IBISColumnRead(ibis,fnames,cols[0],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   if (igncase)
      for (i=0;i<100*nrec;i++) fnames[i] = toupper(fnames[i]);
   status = IBISColumnRead(ibis,(char*)mfsl,cols[1],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*)mfss,cols[2],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*)mfnl,cols[3],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*)mfns,cols[4],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   if (adjust>0)
      {
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[5]);
      mz_alloc1((unsigned char **)&mffac1,nrec,8);
      status = IBISColumnRead(ibis,(char*)mffac1,cols[5],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }
   /*if (keep this code for variance case)
      {
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[6]);
      mz_alloc1((unsigned char **)&mffac2,nrec,8);
      status = IBISColumnRead(ibis,mffac2,cols[6],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }*/
   
   /* open the files for size parm, and open output */
   
   open_files();
   zvclose(i_unit,"CLOS_ACT","FREE", NULL);
   if (strcmp(fmt_str,"BYTE")==0) fmt_byte = 1; else fmt_byte = 0;
   
   /* find any missing coverage.  this will be a file in IBIS table
   that has (sl,ss,nl,ns) indicating overlap with the output but
   is not in the INP param. only filename (not path) is checked */

   zvparm("INP",infil,&filcount,&fildef,NUMINFILES,99);
   filcount -= geotiff;
   if (igncase)
      for (j=1;j<filcount;j++)
         for (i=0;i<99;i++) infil[j-1][i] = toupper(infil[j-1][i]);
   mz_alloc1((unsigned char **)&fileix,filcount,4);
   mz_alloc1((unsigned char **)&ibisix,filcount,4);
   mz_alloc1((unsigned char **)&openstat,filcount,4);
   mz_alloc1((unsigned char **)&vunit,filcount,4);
   mz_alloc1((unsigned char **)&rotix,filcount,4);
   mz_alloc1((unsigned char **)&firstread,filcount,4);
   mz_alloc1((unsigned char **)&lineread,filcount,4);
   mz_alloc1((unsigned char **)&bufix,filcount,4);
   mz_alloc1((unsigned char **)&rereadmemory,filcount,4);
   
   /* geotiff section: reads the last file as a geotiff reference file,
   the (sl,ss,nl,ns) will be in the coordinates of this file, the offsets
   will be calculated for each input image and compared to mfsl and mfss,
   the tolerance parameter is read and all offsets must be within
   <tolerance> of a whole pixel */
   
   if (geotiff)
      {
      zvparmd("toler",&toler,&tolerct,&tolerdf,1,0);
      status = gtgetlab("inp",filcount+1,&labelstr1,&labnl,&labns);
      if (status!=1) zmabend("problem with GeoTIFF file");
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      mz_alloc1((unsigned char **)&ilcorner,filcount,4);
      mz_alloc1((unsigned char **)&iscorner,filcount,4);
      mz_alloc1((unsigned char **)&inl,filcount,4);
      mz_alloc1((unsigned char **)&ins,filcount,4);
      
      for (img=1;img<filcount;img++)
         {
         status = gtgetlab("inp",img,&labelstr2,&labnl,&labns);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, i-th input");
         inl[img] = labnl;
         ins[img] = labns;
         len = strlen(labelstr2);
         for (i=0;i<len;i++) labelstr2[i] = toupper(labelstr2[i]);
         if (img==1) status = gtrect(labelstr1,(double)1.0e-12);
         if (status!=1)
            zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
         status = gtrect(labelstr2,(double)1.0e-12);
         if (status!=1)
            zmabend("GTMSS is restricted to rectangular mappings when GeoTIFF");
         status = geofix(labelstr2,r,rinv,labnl,labns,rcorner);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, i-th input");
         status = gtmapcom(labelstr1,labelstr2);
         if (status!=1) zmabend("Mappings not compatible");
         rot1 = gtgetrot(labelstr1);
         rot2 = gtgetrot(labelstr2);
         if (rot1!=rot2)
            zmabend("Different rotations for two inputs, use GTROTATE");
         gtgetscl(labelstr1,&sctype1,&scale11,&scale12);
         gtgetscl(labelstr2,&sctype2,&scale21,&scale22);
         if (sctype1!=sctype2) /* this is redundant, see rotation */
            zmabend("Different rotations for two inputs, use GTROTATE");
         if (fabs(scale11-scale21)>toler*fabs(scale11))
            zmabend("Different scales for two inputs, use GTSIZE");
         if (fabs(scale12-scale22)>toler*fabs(scale12))
            zmabend("Different scales for two inputs, use GTSIZE");
         xcorner = r[0]+r[1]+r[2];
         ycorner = r[3]+r[4]+r[5];
         lcorner = tinv[0]*xcorner+tinv[1]*ycorner+tinv[2]-1;
         scorner = tinv[3]*xcorner+tinv[4]*ycorner+tinv[5]-1;
         roundval1 = lcorner; roundval2 = scorner;
         if (roundval1>=0.0) roundval1 += 0.5; else roundval1 -= 0.5;
         if (roundval2>=0.0) roundval2 += 0.5; else roundval2 -= 0.5;
         printf("calculated (sl,ss) for image %d is (%18.10f,%18.10f)\n",
            img,lcorner,scorner);
         ilcorner[img] = (int)(roundval1);
         iscorner[img] = (int)(roundval2);
         if (fabs(lcorner-(double)ilcorner[img])>toler)
            zmabend("Non-integral offset calculated from GeoTIFF");
         if (fabs(scorner-(double)iscorner[img])>toler)
            zmabend("Non-integral offset calculated from GeoTIFF");
         }
   }
   
   /* end of geotiff section.  */
   
   maxmfns = 0;
   for (i=1;i<filcount;i++) { fileix[i] = 0; openstat[i] = -99; }
   for (i=0;i<nrec;i++)
      {
      name1 = nameget(&fnames[i*100]);
      notfound = 1;
      isav = i;
      for (j=1;j<filcount;j++) /* (0th) index not used, except for infil */
         {
         name2 = nameget(infil[j-1]);
         if (strcmp(name1,name2)==0)
            {
            notfound = 0;
            fileix[j] = j;
            ibisix[j] = i;
            ibisrev[i] = j; /* sparse OK */
            rotix[j] = -1;
            firstread[j] = 0;
            lineread[j] = 1;
            bufix[j] = -999;
            rereadmemory[j] = -1;
            /* geotiff operations */
            if (geotiff)
               {
               if (mfnl[i]==0)
                  {
                  mfnl[i] = inl[j];
                  mfns[i] = ins[j];
                  mfsl[i] = ilcorner[j];
                  mfss[i] = iscorner[j];
                  maxmfns = MAX(maxmfns,mfns[i]);
                  }
               if (mfnl[i]!=inl[j])
                   zmabend("nl in IBIS file disagrees with GeoTIFF");
               if (mfns[i]!=ins[j])
                   zmabend("ns in IBIS file disagrees with GeoTIFF");
               if (mfsl[i]!=ilcorner[j])
                   zmabend("sl in IBIS file disagrees with GeoTIFF");
               if (mfss[i]!=iscorner[j])
                   zmabend("ss in IBIS file disagrees with GeoTIFF");
               }
            maxmfns = MAX(maxmfns,mfns[i]);
            /*check if overlap*/
            if ((mfsl[i]+mfnl[i])<sline) openstat[j] = -1;
            else if ((mfss[i]+mfns[i])<ssamp) openstat[j] = -1;
            else if (mfsl[i]>=(sline+nline)) openstat[j] = -1;
            else if (mfss[i]>=(ssamp+nsamp)) openstat[j] = -1;
            else openstat[j] = 0;
            break;
            }
         }
      if (notfound)
         {
         /*check if overlap, this feature allows checking of coverage via ibis
         table, missing a file that touches range will cause abort*/
         if (((mfsl[isav]+mfnl[isav])>=sline)&&
               ((mfss[isav]+mfns[isav])>=ssamp)&&
               (mfsl[isav]<(sline+nline))&&(mfss[isav]<(ssamp+nsamp)))
            {
            printf("\nfile %s\n",name1);
            zmabend("is in IBIS table, but not input");
            }
         }
      }
   redmfns = 100+(maxmfns+moorefac-1)/moorefac;
   /* error on +0, +10         alz need correction */
   for (j=1;j<filcount;j++) if (fileix[j]==0||openstat[j]==-99)
      {
      name2 = nameget(infil[j-1]);
      printf("\nfile %s\n",name2);
      zmabend("is not in IBIS table");
      }
   
   /* set up for ramp calculations */
   
   if (zvptst("noramp")) ramp = 3;
   else if (zvptst("readramp")) ramp = 2;
   else ramp = 1;
   mz_alloc2((unsigned char ***)&nbrj,4,filcount,4);
   mz_alloc2((unsigned char ***)&countz,4,filcount,4);
   mz_alloc2((unsigned char ***)&dz,4,filcount,4);
   mz_alloc1((unsigned char **)&zbar,filcount,4);
   for (i=0;i<4;i++)
      for (j=1;j<filcount;j++)
         nbrj[i][j] = -99;
   if (ramp==1||ramp==2)
      {
      mz_alloc2((unsigned char ***)&ibisnbrj,5,nrec,4);
      mz_alloc2((unsigned char ***)&sumz,4,filcount,8);
      
      status = zvp("rdkthr",&rdkthr,&dummy);
      status = zvp("rdiffthr",&rdiffthr,&dummy);
      if (rdkthr<thresh) zmabend("rdkthr must be equal to or greather than thresh");
      i2dkthr = (short int)rdkthr;
      fbigthresh = (float)rdiffthr;
      status = zvp("rmoore",&rmoore,&dummy);
      status = zvp("gorefac",&gorefac,&dummy);
      zvparm("rcols",rcols,&rcolcount,&rcoldef,10,0);
      mooretst = (short int)rmoore;
      for (k=0;k<5;k++)
         {
         status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"FULL",rcols[k]);
         status = IBISColumnRead(ibis,(char*)ibisnbrj[k],rcols[k],1,nrec);
         if (status!=1) IBISSignal(ibis,status,0);
         }
      for (j=1;j<filcount;j++)
         {
         ibix = ibisix[j];
         for (k=0;k<4;k++)
            {
            sumz[k][j] = 0.0;
            countz[k][j] = 0;
            nbrseq = ibisnbrj[k+1][ibix];
            for (kk=0;kk<nrec;kk++)
               if (ibisnbrj[0][kk]==nbrseq) nbrj[k][j] = ibisrev[kk];
            }
         }
      }
   if (ramp==2)
      {
      mz_alloc2((unsigned char ***)&ibisdz,5,nrec,4);
      for (k=5;k<10;k++)
         {
         status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"REAL",rcols[k]);
         status = IBISColumnRead(ibis,(char*)ibisdz[k-5],rcols[k],1,nrec);
         if (status!=1) IBISSignal(ibis,status,0);
         }
      for (j=1;j<filcount;j++)
         {
         i = ibisix[j];
         zbar[j] = ibisdz[0][i];
         for (k=0;k<4;k++)
            {
            dz[k][j] = ibisdz[k+1][i];
            if (fabs(dz[k][j]+999.0)>0.01) countz[k][j] = 1; /* fake value for 'READRAMP case */
            }
         }
      mz_free2((unsigned char **)ibisdz,5);
      }
   if (ramp==3)
      {
      for (j=1;j<filcount;j++)
         {
         zbar[j] = 0.0;
         for (k=0;k<4;k++)
            {
            dz[k][j] = -999.0;
            nbrj[k][j] = 0;
            countz[k][j] = 0;
            }
         }
      
      }
   
   /* A quick scan over the data sets to find the max number of files open
   simultaneously.  Needs to account for distance DFEATHER */
   
   free(fnames);
   numcross = 0;
   maxcross = 0;
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               openstat[j] = 3;
               numcross--;
               continue;
               }
            }
         if (openstat[j]==0) /* check for top line */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               numcross++;
               maxcross = MAX(maxcross,numcross);
               }
            }
         if (openstat[j]==1) /* consider this line */
            {
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               /* file closed but buffer still needed */
               openstat[j] = 2;
               continue;
               }
            }
         } /* j=1..filecount loop */
      } /* the big simultaneous file count loop */
   
   /* Now set up the big buffers for image footprints and Moore distances */
   /* also set up inbuf for actual reading; bufused for tracking usage */
   
   printf("\nMaximum files open simultaneously is %d\n",maxcross);
   printf("Virtual memory needed for Footprints is %d\n\n",
        3*maxcross*redmfns*redfeather);
   mz_alloc3((unsigned char ****)&footprnt,maxcross,redfeather,redmfns,2);
   mz_alloc3((unsigned char ****)&moore,maxcross,redfeather,redmfns,2);
   mz_alloc2((unsigned char ***)&inbuf,maxcross,maxmfns,2);
   mz_alloc1((unsigned char **)&bufused,maxcross,4);
   for (i=0;i<maxcross;i++) bufused[i] = 0;
   for (i=1;i<filcount;i++) if (openstat[i]>0) openstat[i] = 0;
   
   /* prep for main loop, the first file was opened for type */
   
   status = zvp("cloudout",&cloudout,&dummy);
   fcloudout = (float)cloudout;
   mz_alloc1((unsigned char **)&bufout,nsamp,2);
   mz_alloc1((unsigned char **)&bufhit,nsamp,2);
   for (i=0;i<nsamp;i++) bufhit[i] = 0;
   mz_alloc1((unsigned char **)&pstack,redmfns*redfeather,4);
   mz_alloc1((unsigned char **)&istack,redmfns*redfeather,4);
   mz_alloc1((unsigned char **)&vstack,redmfns*redfeather,4);
   
   /* this skips the pass 1 for the 'READRAMP case and the 'NORAMP case */
   
   if (ramp!=1) goto pass2entry;
   
   /* OUTPUT LOOP I: GATHER THE DZ AT MOORE DISTANCE rmoore, NOTE THAT
   THE VICAR FILE CLOSE RELEASES THE UNIT; ignore lines not involved
   because moorefac skips them */
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      if (progress&&iout%500==499)
         printf("%d lines completed, pass 1\n",iout+1);
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         bix = bufix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)==mooreend)
               {
               currix = (rotix[j]+1)%redfeather;
               rotix[j] = currix;
               }
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               zvclose(vunit[j],"CLOS_ACT","FREE", NULL);
               openstat[j] = 3;
               k = bufix[j];
               bufused[k] = 0;
               bufix[j] = -998;
               continue;
               }
            }
           
         if (openstat[j]==0) /* check for top line and open */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               lineread[j] = sline+iread-mfsl[ibj]+1+moorectr;
               status = zvunit( &vunit[j], "INP", fileix[j], NULL);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF", NULL);
               zvget(vunit[j],"NL",&tempnl, NULL);
               zvget(vunit[j],"NS",&tempns, NULL);
               if (tempnl!=mfnl[ibj]||tempns!=mfns[ibj])
                  {
                  printf("file %s has\n",infil[j-1]);
                  zmabend("(nl,ns) disagreement, label vs. IBIS file");
                  }
               zvget(vunit[j],"FORMAT",fmt_str2, NULL);
               if (strcmp(fmt_str,fmt_str2)!=0)
                  zmabend("Mixed byte and halfword files");
               for (k=0;k<maxcross;k++) if (bufused[k]==0)
                  {
                  bufused[k] = 1;
                  bufix[j] = k;
                  bix = k;
                  break;
                  }
               }
            }
         if (openstat[j]==1) /* read the line and apply Moore */
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)!=mooreend) continue;
            if (firstread[j]==0) firstread[j] = 1;
            previx = rotix[j];
            currix = (rotix[j]+1)%redfeather;
            rotix[j] = currix;
            tns = mfns[ibj];
            redtns = (tns+moorefac-1)/moorefac;
            
            if (moorefac==1)
               {
               status = zvread(vunit[j],footprnt[bix][currix],
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns, NULL);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, NULL);
               lineread[j] += moorefac;
               if (lineread[j]>mfnl[ibj]) lineread[j] = mfnl[ibj];
               for (i=moorectr,ired=0;i<tns;i+=moorefac,ired++)
                  footprnt[bix][currix][ired] = inbuf[bix][i];
               if (tns%moorefac!=0)
                  footprnt[bix][currix][redtns-1] = inbuf[bix][tns-1];
               }
            
            /*for (i=1;i<redtns;i++)
               printf(" %d",footprnt[bix][currix][i]);
            printf(" A\n");*/
            
            /* nibble operations */
            if (edge)
               {
               for (i=0;i<lnibble;i++) footprnt[bix][currix][i] = 0;
               for (i=1;i<=rnibble;i++) footprnt[bix][currix][redtns-i] = 0;
               if (lthresh>0)
                  {
                  thrcount = 0;
                  for (i=lnibble;i<redtns;i+=nincr)
                     {
                     if (footprnt[bix][currix][i]>=lthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=lnibble;k<=i;k++)
                              footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq) 
                     {
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                     goto edgedone1;
                     }
                  }
               if (rthresh>0)
                  {
                  thrcount = 0;
                  for (i=redtns-1-rnibble;i>=0;i-=nincr)
                     {
                     if (footprnt[bix][currix][i]>=rthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=redtns-1-rnibble;k>=i;k--)
                               footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq)
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                  }
               }
            edgedone1:
            
            if (firstread[j]==1) /* first line Moore */
               {
               firstread[j] = 2;
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     moore[bix][currix][i] = (short int)0;
                  else moore[bix][currix][i] = (short int)1;
                  }
               continue;
               }
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     {
                     moore[bix][currix][i] = (short int)0;
                     if (previx>=0&&((int)(moore[bix][previx][i])>1))
                     propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  else
                     {
                     moore[bix][currix][i] = (short int)1;
                     if (previx>=0&&((int)(moore[bix][previx][i])>2))
                     propagate(2,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  }
               openstat[j] = 2;
               continue;
               }
            
            datafound = 0;
            for (i=0;i<redtns;i++)
               {
               leftbrk = i;
               if (footprnt[bix][currix][i]>=thresh) { datafound = 1; break; }
               moore[bix][currix][i] = (short int)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            if (!datafound) continue;
            for (i=redtns-1;i>=0;i--)
               {
               rightbrk = i;
               if (footprnt[bix][currix][i]>=thresh) break;
               moore[bix][currix][i] = (short int)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            midns = (leftbrk+rightbrk)/2;
            if (leftbrk==0) moore[bix][currix][leftbrk++] = (short int)1;
            if (rightbrk==(redtns-1)) moore[bix][currix][rightbrk--] = (short int)1;
            for (i=leftbrk;i<midns;i++)
               {
               moore[bix][currix][i] = (short int)MIN((int)moore[bix][currix][i-1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            for (i=rightbrk;i>=midns;i--)
               {
               moore[bix][currix][i] = (short int)MIN((int)moore[bix][currix][i+1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            } /* openstat[j]==1 cases */
         } /* j=1..filecount loop */
      
      /* get tiepoints from the oldest line in footprint; a grid is imposed on the */
      /* mosaic space for the tiepoints so as not to collect too many; this grid will */
      /* center with the grids in the footprints, but they don't register with each */
      /* other anyway; note that the tiepoints are collected at high resolution */
      
      if (iout>=0&&((iout%moorefac)==moorectr))
         {
         /* initialize the start and stop points of the output line in bufhit */
         
         bufhit[0] = 1;
         for (j=1;j<filcount;j++)
            {
            if (openstat[j]>0&&openstat[j]<3)
               {
               ibj = ibisix[j];
               if (iout>=mfsl[ibj]-sline)
                  {
                  ixl = mfss[ibj]-ssamp;
                  ixr = mfss[ibj]+mfns[ibj]-ssamp;
                  if (ixl>=0&&ixl<nsamp) bufhit[ixl] = 1;
                  if (ixr>=0&&ixr<nsamp) bufhit[ixr] = 1;
                  }
               }
            }
         for (i=0;i<nsamp;i++)
            {
            if (bufhit[i]==1)
               {
               listix = 0;
               for (j=1;j<filcount;j++)
                  {
                  ibj = ibisix[j];
                  if ((openstat[j]>0)&&(openstat[j]<3)&&
                     (i>=(mfss[ibj]-ssamp))&&
                     (i<(mfss[ibj]+mfns[ibj]-ssamp))&&
                     (iread>=(mfsl[ibj]-sline+dfeather-1)))
                     {
                     inlist[listix] = j;
                     sampoff[listix++] = mfss[ibj]-ssamp;
                     }
                  }
               bufhit[i] = 0;
               }
            if ((i%moorefac)!=moorectr) continue;
            if (listix>1)                           /* must be overlap */
               {
               /*if (i%250==0&&iout%250==0)
                  {
                  printf("%d %d:",iout,i);
                  for (kq=0;kq<listix;kq++)
                     printf(" %d",ibisnbrj[0][ibisix[inlist[kq]]]);
                  printf("\n");
                  }*/
               for (k=0;k<listix;k++)
                  {
                  j = inlist[k];
                  bix = bufix[j];
                  ix2 = (rotix[j]+1)%redfeather;
                  ix3 = i-sampoff[k];
                  ix3red = ix3/moorefac;
                  if (moore[bix][ix2][ix3red]!=mooretst) continue;
                  if (moorefac==1)
                     {
                     pself = footprnt[bix][ix2][ix3red];
                     }
                  else
                     {
                     ibj = ibisix[j];
                     rereadline = iout-mfsl[ibj]+sline+1+moorectr;
                     if (rereadmemory[j]!=rereadline)
                        {
                        status = zvread(vunit[j],inbuf[bix],
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj], NULL);
                        rereadmemory[j] = rereadline;
                        }
                     pself = inbuf[bix][ix3];
                     }
                  if (pself<i2dkthr) continue;
                  /* accumulate dz for all nbrs, but have to match to 4 nbrs */
                  for (kq=0;kq<listix;kq++)
                     {
                     if (kq==k) continue;
                     jq = inlist[kq];
                     for (kmatch=0;kmatch<4;kmatch++)
                        {
                        if (jq==nbrj[kmatch][j])
                           {
                           bixq = bufix[jq];
                           ix2q = (rotix[jq]+1)%redfeather;
                           ix3q = i-sampoff[kq];
                           ix3qred = ix3q/moorefac;
                           mnbr = moore[bixq][ix2q][ix3qred];
                           if (mnbr==(short int)0) continue;
                           if (moorefac==1)
                              {
                              pnbr = footprnt[bixq][ix2q][ix3qred];
                              }
                           else
                              {
                              ibjq = ibisix[jq];
                              rereadline = iout-mfsl[ibjq]+sline+1+moorectr;
                              if (rereadmemory[jq]!=rereadline)
                                 {
                                 status = zvread(vunit[jq],inbuf[bixq],
                                    "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibjq], NULL);
                                 rereadmemory[jq] = rereadline;
                                 }
                              pnbr = inbuf[bixq][ix3q];
                              }
                           if (pnbr<i2dkthr) break;
                           vself = (float)pself;
                           switch (adjust)
                              {
                              case 1: vself = vself*mffac1[ibisix[j]]; break;
                              case 2: /* same as case 3 */
                              case 3: vself = vself+mffac1[ibisix[j]]; break;
                              }
                           vnbr = (float)pnbr;
                           switch (adjust)
                              {
                              case 1: vnbr = vnbr*mffac1[ibisix[jq]]; break;
                              case 2: /* same as case 3 */
                              case 3: vnbr = vnbr+mffac1[ibisix[jq]]; break;
                              }
                           /* reverse self-nbr */
                           vdiff = vnbr-vself;
                           if (fabs(vdiff)>fbigthresh) break;
                           sumz[kmatch][j] += (double)vdiff;
                           countz[kmatch][j]++;
                           break;
                           }
                        }
                     }
                  }
               } /* listix>1 case */
            } /* i=0..nsamp */
         } /* iout>=0 output of oldest line in bufffer */
      } /* the big input/output loop */
   
   /* reset for loop II, also process the dz, leave zero dz alone */
   
   for (i=0;i<maxcross;i++) bufused[i] = 0;
   for (j=1;j<filcount;j++)
      {
      if (openstat[j]==1||openstat[j]==2) zvclose(vunit[j],"CLOS_ACT","FREE", NULL);
      if (openstat[j]>0) openstat[j] = 0;
      rotix[j] = -1;
      firstread[j] = 0;
      lineread[j] = 1;
      bufix[j] = -999;
      rereadmemory[j] = -1;
      ztot = 0.0;
      zcnt = 0;
      for (k=0;k<4;k++)
         {
         if (countz[k][j]==0) continue;
         sumz[k][j] /= (double)countz[k][j];
         ztot += sumz[k][j];
         zcnt++;
         }
      if (zcnt==0) zbar[j] = 0; else zbar[j] = ztot/(double)zcnt;
      /* now apply the "half-to-neighbor" factor to zbar with no "boost" */
      zbar[j] *= 0.5;
      /* deduct the self zbar */
      for (k=0;k<4;k++)
         {
         if (countz[k][j]==0) dz[k][j] = 0.0;
         else dz[k][j] = sumz[k][j]-zbar[j];
         }
      }
   for (j=1;j<filcount;j++)
      {
      /* add the neighbor zbar, prev loop has calculated all the zbar's */
      for (k=0;k<4;k++)
         {
         if (countz[k][j]>0)
            {
            jnbr = nbrj[k][j];
            if (jnbr>0) dz[k][j] = dz[k][j]+zbar[jnbr];
            }
         }
      /* now apply the "gorefac" factor */
      for (k=0;k<4;k++) dz[k][j] *= gorefac;
      }
      
   /* OUTPUT LOOP II: USE THE DZ TO RAMP THE NEIGHBORS CLOSER, NOTE THAT
   THE VICAR FILE CLOSE RELEASES THE UNIT */
   
   /*zprnt(8,filcount,sumz[0],"sumz[0].");
   zprnt(8,filcount,sumz[1],"sumz[1].");
   zprnt(8,filcount,sumz[2],"sumz[2].");
   zprnt(8,filcount,sumz[3],"sumz[3].");*/
   
   pass2entry:
   
   /*zprnt(4,filcount,countz[0],"countz[0].");
   zprnt(4,filcount,countz[1],"countz[1].");
   zprnt(4,filcount,countz[2],"countz[2].");
   zprnt(4,filcount,countz[3],"countz[3].");
   zprnt(7,filcount,dz[0],"dz[0].");
   zprnt(7,filcount,dz[1],"dz[1].");
   zprnt(7,filcount,dz[2],"dz[2].");
   zprnt(7,filcount,dz[3],"dz[3].");
   zprnt(7,filcount,zbar,"zbar.");*/
   
   for (iread=1-dfeather,iout=2-2*dfeather;iout<nline;iread++,iout++)
      {
      if (progress&&iout%500==499)
         printf("%d lines completed, pass 2\n",iout+1);
      for (j=1;j<filcount;j++)
         {
         ibj = ibisix[j];
         bix = bufix[j];
         if (openstat[j]==3) continue;
         if (openstat[j]==2)
            {
            
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)==mooreend)
               {
               currix = (rotix[j]+1)%redfeather;
               rotix[j] = currix;
               }
            /* terminate footprnt if past Moore distance */
            if ((sline+iread-dfeather+2)>(mfsl[ibj]+mfnl[ibj]))
               {
               zvclose(vunit[j],"CLOS_ACT","FREE", NULL);
               openstat[j] = 3;
               k = bufix[j];
               bufused[k] = 0;
               bufix[j] = -998;
               continue;
               }
            }
           
         if (openstat[j]==0) /* check for top line and open */
            {
            if (mfsl[ibj]<=(sline+iread))
               {
               openstat[j] = 1;
               lineread[j] = sline+iread-mfsl[ibj]+1+moorectr;
               status = zvunit( &vunit[j], "INP", fileix[j], NULL);
               status = zvopen( vunit[j], "OPEN_ACT", "SA",
                 "IO_ACT", "SA", "U_FORMAT","HALF", NULL);
               zvget(vunit[j],"NL",&tempnl, NULL);
               zvget(vunit[j],"NS",&tempns, NULL);
               if (tempnl!=mfnl[ibj]||tempns!=mfns[ibj])
                  {
                  printf("file %s has\n",infil[j-1]);
                  zmabend("(nl,ns) disagreement, label vs. IBIS file");
                  }
               for (k=0;k<maxcross;k++) if (bufused[k]==0)
                  {
                  bufused[k] = 1;
                  bufix[j] = k;
                  bix = k;
                  break;
                  }
               }
            }
         if (openstat[j]==1) /* read the line and apply Moore */
            {
            curline = sline+iread-mfsl[ibj];
            if ((curline%moorefac)!=mooreend) continue;
            if (firstread[j]==0) firstread[j] = 1;
            previx = rotix[j];
            currix = (rotix[j]+1)%redfeather;
            rotix[j] = currix;
            tns = mfns[ibj];
            redtns = (tns+moorefac-1)/moorefac;
            
            if (moorefac==1)
               {
               status = zvread(vunit[j],footprnt[bix][currix],
                   "LINE",lineread[j]++,"SAMP",1,"NSAMPS",tns, NULL);
               }
            else
               {
               status = zvread(vunit[j],inbuf[bix],
                   "LINE", lineread[j], "SAMP", 1, "NSAMPS", tns, NULL);
               lineread[j] += moorefac;
               if (lineread[j]>mfnl[ibj]) lineread[j] = mfnl[ibj];
               for (i=moorectr,ired=0;i<tns;i+=moorefac,ired++)
                  footprnt[bix][currix][ired] = inbuf[bix][i];
               if (tns%moorefac!=0)
                  footprnt[bix][currix][redtns-1] = inbuf[bix][tns-1];
               }
            
            /* nibble operations */
            if (edge)
               {
               for (i=0;i<lnibble;i++) footprnt[bix][currix][i] = 0;
               for (i=1;i<=rnibble;i++) footprnt[bix][currix][redtns-i] = 0;
               if (lthresh>0)
                  {
                  thrcount = 0;
                  for (i=lnibble;i<redtns;i+=nincr)
                     {
                     if (footprnt[bix][currix][i]>=lthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=lnibble;k<=i;k++)
                              footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq) 
                     {
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                     goto edgedone2;
                     }
                  }
               if (rthresh>0)
                  {
                  thrcount = 0;
                  for (i=redtns-1-rnibble;i>=0;i-=nincr)
                     {
                     if (footprnt[bix][currix][i]>=rthresh)
                        {
                        thrcount++;
                        if (thrcount>=nseq)
                           {
                           for (k=redtns-1-rnibble;k>=i;k--)
                               footprnt[bix][currix][k] = 0;
                           break;
                           }
                        }
                     else thrcount = 0;
                     }
                  if (thrcount<nseq)
                     for (k=lnibble;k<redtns;k++) footprnt[bix][currix][k] = 0;
                  }
               }
            edgedone2:
            
            if (firstread[j]==1) /* first line Moore */
               {
               firstread[j] = 2;
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     moore[bix][currix][i] = (short int)0;
                  else moore[bix][currix][i] = (short int)1;
                  }
               continue;
               }
            if ((mfsl[ibj]+mfnl[ibj])<=(sline+iread+1)) /* last line Moore */
               {
               for (i=0;i<redtns;i++)
                  {
                  if (footprnt[bix][currix][i]<thresh)
                     {
                     moore[bix][currix][i] = (short int)0;
                     if (previx>=0&&((int)(moore[bix][previx][i])>1))
                     propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  else
                     {
                     moore[bix][currix][i] = (short int)1;
                     if (previx>=0&&((int)(moore[bix][previx][i])>2))
                     propagate(2,moore[bix],previx,i,redfeather,redtns,currix);
                     }
                  }
               openstat[j] = 2;
               continue;
               }
            
            datafound = 0;
            for (i=0;i<redtns;i++)
               {
               leftbrk = i;
               if (footprnt[bix][currix][i]>=thresh) { datafound = 1; break; }
               moore[bix][currix][i] = (short int)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            if (!datafound) continue;
            for (i=redtns-1;i>=0;i--)
               {
               rightbrk = i;
               if (footprnt[bix][currix][i]>=thresh) break;
               moore[bix][currix][i] = (short int)0;
               if (previx>=0&&((int)(moore[bix][previx][i])>1))
                  propagate(1,moore[bix],previx,i,redfeather,redtns,currix);
               }
            midns = (leftbrk+rightbrk)/2;
            if (leftbrk==0) moore[bix][currix][leftbrk++] = (short int)1;
            if (rightbrk==(redtns-1)) moore[bix][currix][rightbrk--] = (short int)1;
            for (i=leftbrk;i<midns;i++)
               {
               moore[bix][currix][i] = (short int)MIN((int)moore[bix][currix][i-1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            for (i=rightbrk;i>=midns;i--)
               {
               moore[bix][currix][i] = (short int)MIN((int)moore[bix][currix][i+1]+1,
                  MIN((int)moore[bix][previx][i]+1,redfeather));
               }
            } /* openstat[j]==1 cases */
         } /* j=1..filecount loop */
     
      if (iout>=0) /* write output of oldest line in buffer */
         {
         /* initialize the start and stop points of the output line in bufhit */
         
         bufhit[0] = 1;
         for (j=1;j<filcount;j++)
            {
            if (openstat[j]>0&&openstat[j]<3)
               {
               ibj = ibisix[j];
               if (iout>=mfsl[ibj]-sline)
                  {
                  ixl = mfss[ibj]-ssamp;
                  ixr = mfss[ibj]+mfns[ibj]-ssamp;
                  if (ixl>=0&&ixl<nsamp) bufhit[ixl] = 1;
                  if (ixr>=0&&ixr<nsamp) bufhit[ixr] = 1;
                  }
               }
            }
         for (i=0;i<nsamp;i++)
            {
            /*if (iout==0&&i==4) dbg = 1; else dbg = 0;*/
            if (bufhit[i]==1)
               {
               listix = 0;
               for (j=1;j<filcount;j++)
                  {
                  ibj = ibisix[j];
                  if ((openstat[j]>0)&&(openstat[j]<3)&&
                     (i>=(mfss[ibj]-ssamp))&&
                     (i<(mfss[ibj]+mfns[ibj]-ssamp))&&
                     (iread>=(mfsl[ibj]-sline+dfeather-1)))
                     {
                     inlist[listix] = j;
                     sampoff[listix++] = mfss[ibj]-ssamp;
                     }
                  }
               bufhit[i] = 0;
               }
            if (listix==1)
               {
               j = inlist[0];
               bix = bufix[j];
               ix2 = (rotix[j]+1)%redfeather;
               ix3 = i-sampoff[0];
               ix3red = ix3/moorefac;
               if (moore[bix][ix2][ix3red]<=moorenbl)
                  { bufout[i] = 0; continue; }  /* moore is lo-res */
               if (moorefac==1)                 /* tval is hi-res */
                  {
                  tval = footprnt[bix][ix2][ix3];
                  }
               else
                  {
                  ibj = ibisix[j];
                  rereadline = iout-mfsl[ibj]+sline+1;  /* no adjust to moorectr */
                  if (rereadmemory[j]!=rereadline)
                     {
                     status = zvread(vunit[j],inbuf[bix],
                        "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj], NULL);
                     rereadmemory[j] = rereadline;
                     }
                  tval = inbuf[bix][ix3];
                  }           
               
               if (ramp<3&&tval>=thresh)                       /* ramp is hi-res */
                  {
                  iix = ibisix[j];
                  loff = mfsl[iix]-sline;
                  soff = mfss[iix]-ssamp;
                  hnl = (float)(mfnl[iix]+1)*0.5;
                  hns = (float)(mfns[iix]+1)*0.5;
                  fhnl = 1.0/hnl;
                  fhns = 1.0/hns;
                  locnl = (float)(iout-loff+1);
                  locns = (float)(i-soff+1);
                  /*if (dbg) printf("A:locnl,locns,hnl,hns %f %f %f %f\n",
                                     locnl,locns,hnl,hns);*/
                  if (locnl<hnl)
                     {
                     if (locns<hns)
                        zramp = rampcalc(hns-locns,hnl-locnl,dz,countz,j,3,2,zbar[j]);
                     else
                        zramp = rampcalc(locns-hns,hnl-locnl,dz,countz,j,3,0,zbar[j]);
                     }
                  else
                     {
                     if (locns<hns)
                        zramp = rampcalc(hns-locns,locnl-hnl,dz,countz,j,1,2,zbar[j]);
                     else
                        zramp = rampcalc(locns-hns,locnl-hnl,dz,countz,j,1,0,zbar[j]);
                     }
                  }
               else zramp = 0.0;
               /*if (dbg) printf("B:tval,adjust,zbar[j],zramp %d %d %f %f\n",
                                      tval,adjust,zbar[j],zramp);*/
               switch (adjust)
                  {
                  case 0: bufout[i] = tval; break;
                  case 1: bufout[i] = MAX((short int)0,(short int)
                          ((float)tval*mffac1[ibisix[j]]-zramp+0.5)); break;
                  case 2: bufout[i] = MAX((short int)0,(short int)
                          ((float)tval+mffac1[ibisix[j]]-zramp+0.5)); break;
                  case 3: if (tval>0) bufout[i] = MAX((short int)1,(short int)
                          ((float)tval+mffac1[ibisix[j]]-zramp+0.5));
                          else bufout[i] = 0; break;
                   /*alz australia blue validation of - sign on zramp*/
                   }
               }
            else if (listix==0) bufout[i] = 0;
            else                                 /* listix>1 overlap case */
               {
               usum = 0.0; lsum = 0.0; dimax = -1; kk = 0;
               for (k=0;k<listix;k++)
                  {
                  j = inlist[k];
                  bix = bufix[j];
                  ix2 = (rotix[j]+1)%redfeather;
                  ix3 = i-sampoff[k];
                  ix3red = ix3/moorefac;
                  di = (float)(MAX((int)
                     (moore[bix][ix2][ix3red]-moorenbl),0));  /* moore is lo-res */
                  if (dpow>1.001) di = pow(di/dpow,dpow);
                  if (moorefac==1)                           /* vi is hi-res */
                     {
                     tval = (int)footprnt[bix][ix2][ix3];
                     }
                  else
                     {
                     ibj = ibisix[j];
                     rereadline = iout-mfsl[ibj]+sline+1;  /* no adjust to moorectr */
                     if (rereadmemory[j]!=rereadline)
                        {
                        status = zvread(vunit[j],inbuf[bix],
                           "LINE",rereadline,"SAMP",1,"NSAMPS",mfns[ibj], NULL);
                        rereadmemory[j] = rereadline;
                        }
                     tval = (int)inbuf[bix][ix3];
                     }
                  if (tval<thresh) continue;
                  vi = (float)tval;
                  
                  if (ramp<3)
                     {
                     iix = ibisix[j];
                     loff = mfsl[iix]-sline;
                     soff = mfss[iix]-ssamp;
                     hnl = (float)(mfnl[iix]+1)*0.5;
                     hns = (float)(mfns[iix]+1)*0.5;
                     fhnl = 1.0/hnl;
                     fhns = 1.0/hns;
                     locnl = (float)(iout-loff+1);
                     locns = (float)(i-soff+1);
                     if (locnl<hnl)
                        {
                        if (locns<hns)
                           zramp = rampcalc(hns-locns,hnl-locnl,dz,countz,j,3,2,zbar[j]);
                        else
                           zramp = rampcalc(locns-hns,hnl-locnl,dz,countz,j,3,0,zbar[j]);
                        }
                     else
                        {
                        if (locns<hns)
                           zramp = rampcalc(hns-locns,locnl-hnl,dz,countz,j,1,2,zbar[j]);
                        else
                           zramp = rampcalc(locns-hns,locnl-hnl,dz,countz,j,1,0,zbar[j]);
                        }
                     }
                  /*if (dbg) printf("C:tval,adjust,zbar[j],zramp %d %d %f %f\n",
                                tval,adjust,zbar[j],zramp);*/
                  switch (adjust)
                     {
                     case 1: vi = vi*mffac1[ibisix[j]]-zramp; break;
                     case 2: /* same as case 3 */
                     case 3: vi = vi+mffac1[ibisix[j]]-zramp; break;
                     }
                  if (cloudout>0)
                     {
                     if (kk>0)
                        {
                        if (vi>=(ovi+fcloudout)) continue;
                        if (ovi>=(vi+fcloudout))
                           {
                           usum = 0;
                           lsum = 0;
                           ovi = vi;
                           odi = di;
                           }
                        }
                     else { ovi = vi; odi = di; }
                     }
                  usum += di*vi;
                  lsum += di;
                  kk++;
                  if (mooremax&&di>dimax)
                     {
                     dimax = di;
                     vimax = vi;
                     }
                  /*if (dbg)
                     {
                     printf("D:di,vi,lsum,usum %f %f %f %f\n",
                                di,vi,lsum,usum);
                     }*/
                  } /* end k loop */
               if (mooremax)
                     {
                     usum = dimax*vimax;
                     lsum = dimax;
                     }
               if (lsum<0.01) bufout[i] = (short int)0;
               else if (adjust!=3) 
                  bufout[i] = MAX((short int)0,(short int)((usum/lsum)+0.5));
               else 
                  bufout[i] = MAX((short int)1,(short int)((usum/lsum)+0.5));
               } /* listix>1 case */
            if (fmt_byte&&(bufout[i]>255)) bufout[i] = 255;
            } /* i=0..nsamp */
         zvwrit(o_unit,bufout,"LINE",iout+1,"SAMP",1,"NSAMPS", nsamp, NULL);
         } /* iout>=0 output of oldest line in bufffer */
      } /* the big input/output loop */
   
   /* write the output ibis columns, old values read and preserved with
   new values as update, but has to be the 'RAMP case */
   
   if (ramp==1)
      {
      mz_alloc2((unsigned char ***)&ibisdz,5,nrec,4);
      
      for (i=0;i<5;i++)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","REAL",rcols[5+i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)ibisdz[i],rcols[5+i],1,nrec);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      
      for (j=1;j<filcount;j++)
         {
         i = ibisix[j];
         ibisdz[0][i] = zbar[j];
         for (k=0;k<4;k++) if (countz[k][j]==0) ibisdz[k+1][i] = -999.0;
                else ibisdz[k+1][i] = dz[k][j];
         }
   
      for (i=0;i<5;i++)
         {
         status = IBISColumnWrite(ibis,(char*)ibisdz[i],rcols[5+i],1,nrec);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      }
   
   /* update the geotiff label, gtreplab reopens for update */
   /* the tout[] solutions are in GeoTIFF coordinates, see size */
   
   zvclose(o_unit, NULL);
   if (geotiff)
      {
      bcor = t[0]+t[1]+t[2];
      dcor = t[3]+t[4]+t[5];
      p = ms_find(labelstr1,"GTRASTERTYPEGEOKEY=2");
      if (p!=0) voff = 1.0; else voff = 0.5;
      
      toutinv[0] = tinv[3];
      toutinv[1] = tinv[4];
      toutinv[2] = 1.0-voff-toutinv[0]*bcor-toutinv[1]*dcor-ssamp;
      toutinv[3] = tinv[0];
      toutinv[4] = tinv[1];
      toutinv[5] = 1.0-voff-toutinv[3]*bcor-toutinv[4]*dcor-sline;
      
      scale2 = -t[3];
      scale1 = t[1];
      
      invertmap(toutinv,tout);
      scalefmt(scalestr,scale1,scale2);
      trnsfmt(transstr,tout);
      gtreplab("OUT",1,labelstr1,0,1,toutinv,scalestr,transstr);
      }
 
   /* close and exit */
   
   return;

/* some useful debugging code
   
   ibj = ibisix[j];
   tns = mfns[ibj];
   redtns = (tns+moorefac-1)/moorefac;
   for (qq=1;qq<=redfeather;qq++) {for (q=0;q<redtns;q++)
    printf("%4d",footprnt[bix][(currix+qq)%redfeather][q]);printf("\n");}
   printf(" rotix %d\n",ix2);
   for (qq=1;qq<=redfeather;qq++)             
   {for (q=0;q<tns;q++)
    printf("%4d",moore[bix][(currix+qq)%redfeather][q]);printf("\n");}
   printf("\n");
                  
*/  
   
}
