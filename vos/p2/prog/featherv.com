$!****************************************************************************
$!
$! Build proc for MIPL module featherv
$! VPACK Version 1.9, Thursday, January 26, 2012, 13:58:36
$!
$! Execute by entering:		$ @featherv
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module featherv ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to featherv.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("featherv.imake") .nes. ""
$   then
$      vimake featherv
$      purge featherv.bld
$   else
$      if F$SEARCH("featherv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake featherv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @featherv.bld "STD"
$   else
$      @featherv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create featherv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack featherv.com -mixed -
	-s featherv.c -
	-i featherv.imake -
	-p featherv.pdf -
	-t tstfeatherv.pdf devfeatherv.pdf tstfeatherv.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create featherv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create featherv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM featherv

   To Create the build file give the command:

		$ vimake featherv			(VMS)
   or
		% vimake featherv			(Unix)


************************************************************************/


#define PROGRAM	featherv

#define MODULE_LIST featherv.c

#define MAIN_LANG_C
#define R2LIB
#define USES_ANSI_C

#define LIB_CARTO
#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create featherv.pdf
process help=*
!  FILE NAMES      
!
PARM INP     TYPE=STRING   COUNT=(2:400)
PARM OUT     TYPE=STRING   COUNT=1
PARM COLS    TYPE=INTEGER  COUNT=(5:7)   DEFAULT=(1,2,3,4,5,6,7)
!
PARM SIZE    TYPE=INTEGER  COUNT=4       DEFAULT=(1,1,0,0)
PARM SL      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM SS      TYPE=INTEGER  COUNT=1       DEFAULT=1
PARM NL      TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NS      TYPE=INTEGER  COUNT=1       DEFAULT=0
!
PARM ADJUST   TYPE=KEYWORD  COUNT=1   DEFAULT=NOADJ +
      VALID=(NOADJ,FACTOR,ADD,ADDZMASK)
PARM DFEATHER TYPE=INTEGER  COUNT=1   DEFAULT=10     VALID=(2:50000)
PARM MOOREFAC TYPE=INTEGER  COUNT=1   DEFAULT=5      VALID=(1:199)
PARM MOOREMAX TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--     VALID=MOOREMAX
PARM MOOREPOW TYPE=REAL     COUNT=1   DEFAULT=1.0    VALID=(1.0:8.0)
PARM CLOUDOUT TYPE=INTEGER  COUNT=1   DEFAULT=0      VALID=(0:32767)
!
PARM RAMP     TYPE=KEYWORD  COUNT=1   DEFAULT=RAMP +
      VALID=(NORAMP,RAMP,READRAMP)
PARM RDKTHR   TYPE=INTEGER  COUNT=1   DEFAULT=10    VALID=(-32768:32767)
PARM RDIFFTHR TYPE=INTEGER  COUNT=1   DEFAULT=35    VALID=(1:32767)
PARM RMOORE   TYPE=INTEGER  COUNT=1   DEFAULT=5     VALID=(1:10000)
PARM GOREFAC  TYPE=REAL     COUNT=1   DEFAULT=0.6   VALID=(0.0:1.0)
PARM RCOLS    TYPE=INTEGER  COUNT=10  DEFAULT=(8,9,10,11,12,13,14,15,16,17)
PARM MOORENBL TYPE=INTEGER  COUNT=1   DEFAULT=0     VALID=(0:10000)
!
PARM THRESH  TYPE=INTEGER  COUNT=1       DEFAULT=1        VALID=(-32768:32767)
PARM PROGRESS TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--       VALID=PROGRESS
PARM EDGE    TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--       VALID=EDGE
PARM NTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM LTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM RTHRESH TYPE=INTEGER  COUNT=1       DEFAULT=0
PARM NSEQ    TYPE=INTEGER  COUNT=1       DEFAULT=8        VALID=(1:32767)
PARM NIBBLE  TYPE=INTEGER  COUNT=1       DEFAULT=4        VALID=(0:32767)
PARM LNIBBLE TYPE=INTEGER  COUNT=1       DEFAULT=0        VALID=(0:32767)
PARM RNIBBLE TYPE=INTEGER  COUNT=1       DEFAULT=0        VALID=(0:32767)
PARM NINCR   TYPE=INTEGER  COUNT=1       DEFAULT=1        VALID=(1:32767)
!
PARM GEOTIFF TYPE=KEYWORD COUNT=(0:1)   DEFAULT=--    VALID=GEOTIFF
PARM TOLER   TYPE=REAL    COUNT=1   DEFAULT=0.00001 VALID=(0.0:1.0)
PARM IGNCASE TYPE=KEYWORD  COUNT=(0:1)   DEFAULT=--     VALID=IGNCASE
END-PROC
.TITLE
FEATHERV - Program for mosaicking images with Moore distance feathering, ramp version
.HELP
 PURPOSE:
Program FEATHERV takes input images and mosaics them to form an output image
with a gradual feathering in all directions of all image overlaps.
FEATHERV performs mosaicking by reading all of the input image files and 
combining them to form an output image according to the parameters for
edging, and an IBIS file for image placement.  In a typical application, the
output image is a composite picture made from two or more input images each
of which contains a portion of the overall picture.  There are special 
features for cloud (or glint) removal, for brightness correction, for
colossal mosaics via tiling, and for analyst error checking.

A special feature of this program is that tiepoints between frames that are
declared to be neighbors in the IBIS file are gathered.  The brightness difference
statistics are averaged and result in two additional corrections:

1.  The average difference (halved) to all neighbors is added to each frame as
    a constant.  The constant is reported in a column of the IBIS file.

2.  The remaining differences (halved) to each declared neighbor is added to
    each frame as an angled ramp or half-plane.  The half plane is "hinged",
    has a zero value, down or across the middle of the image.  At the edge
    of the image, it has a ramp height that is reported in the IBIS file.
    
The four ramps are merged in each quadrant of the image by an approximation to
angular interpolation.  For example, the 45 degree diagonal to the upper left
corner of the image would get half of its value from the left=facing ramp and
half from the top-facing ramp.  The approximation is to use

         y/(x+y) to approximate atan(y/x)/90
 and        
         x/(x+y) to approximate (90-atan(y/x))/90

for example, on the diagonal, x=y and all four formulas give .5

The constant and ramp corrections are applied after the ADJUST keyword corrections,
since it is best to apply them as a residual operation.  The feathering and
cloud removal are applied after the constant and ramp corrections.
.PAGE
 EXECUTION:
 
Note that the image files are first in the parameter sequence, and the last
input is an IBIS file.  The IBIS file contains master mosaicking information
on all of the images used in the INP parameter (at the least) and is
recommended to contain information on all images in the application, even if
this numbers in the tens of thousands.  The program limits at present (9/99)
has a limit of 400 inputs, and it opens and closes them as a narrow mosaicking
band passes from top to bottom of the output.  The number of files open over
the band cannot exceed the VICAR limit for number of files open at once.
The IBIS file contains the following information in columns that can be
parameter specified or defaulted.  The default column numbers are given here:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input (see GeoTIFF options below)
3.  The offset SS of the image input (see GeoTIFF options below)
4.  The NL of the image input (cannot be less as in FASTMOS) (see GeoTIFF options below)
5.  The NS of the image input (cannot be less as in FASTMOS) (see GeoTIFF options below)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

GeoTIFF Options:  If the images all have GeoTIFF labels and the 'GEOTIFF
keyword is given, then there are two possibilities:

The first is: if columns 2-5 contain non-zero values (only column 4 is
checked for 0), then the values are compared with values calculated from
the GeoTIFF labels or the nl,ns from the VICAR label.  Since the values
might calculate with a slight inaccuracy, the TOLER parameter is provided
with a default value of 1.e-7.  If a calculated offset exceeds the TOLER
parameter, this indicates that the files have an inaccurate mapping for
the purposes of mosaicking and do not have an integral offset.  All mappings
are checked for consistency (map projection type, zones, meridians,
etc.).

The second is: if columns 2-5 contain zero values (only column 4 is
checked for 0), then the values are calculated from the GeoTIFF labels
or the nl,ns from the VICAR label.  The only check is that the offsets
are integral values (within the parameter TOLER) and that the mappings
are consistent.

When in the GeoTIFF case, a geotiff label is calculated and written
for the output mosaicked image.

The IBIS file is used for two reasons.  First, the input of offsets is 
simplified.  Second, coverage can be checked.  The program will abort with
an error message if a mosaic is performed and an input file cannot be found
in the IBIS file.  The program will also abort with an error message if a
mosaic is performed and an file in the IBIS file touches the output area
and is not included in the INP parameter (this is why NL,NS is in the IBIS
file).  If a file is opened and its (NL,NS) does not agree with the IBIS
file, the program aborts.  In all of these cases, the file is named.

The size of the output image is determined by the number of lines and number 
of samples in the SIZE field if the SIZE field is entered by the user.  If the
SIZE field is not entered, the output file is the same size as the first image
file (see note above, this is the second input). The input images are not
required all to have the same size.  The data type of the input images may
either be byte or halfword data, but all of the input images must have the
same data type.  The data type is obtained from the VICAR label of the first 
input image.

For mosaicking, the program needs to know the locations in the output image 
of each of the input images.  This is done by giving the location in
the output image of each input image.  The locations are given in terms
of line number and pixel number within a line.  This is no longer given
by parameters, but must be placed in the columns of an IBIS file as discussed
above.  The offset value must be in the IBIS file unless GeoTIFF labels are
used.  An offset of (1,1) means that the upper left pixel of the offset image
would line up with the upper left pixel of the output image (assuming that
it also had a (sl,s) of (1,1) (the normal case).

An input image is not required to lie entirely within the boundaries of the
output image.  If the upper left hand corner of an input image is not within 
the boundaries of the output image, the location of the input image is given
by extending the numbering of lines and pixels beyond the boundaries of the
output image.  Thus negative numbers or zero would be used for the locations
of input images beginning to the left or above the boundaries of the output
image.  Input images are allowed to overlap, in fact, that is the motivation
for this program.

FEATHERV only has a default mode of overlap, namely, averaging.  The value at
a pixel will generally be the average of all values above the THRESH value
from images that cover the pixel.  But the averaging is modified by feathering
the edges of data as follows.  For each input, a MOORE DISTANCE ALGORITHM is
applied to data inside of the image so that the image is mirrored by a edge
distance image that gives distance from the edge of data above the THRESH 
value or the absolute edge of the input image (if the data goes fully to the
edge of the input image).  The MOORE DISTANCE ALGORITHM has to be applied
outside of the mosaic output image area, because an image edge some distance
from the mosaic output image area can contribute to a MOORE DISTANCE NUMBER
inside of the mosaic output image area.

for example, the image data

 16  16  16  16  16  16  16 
 16  16  16  16  16  16  16
 16  16  16  16  16  16  16
 16  16  16  16  16  16  16 
 16  16   0   0   0  16  16  
 
has the MOORE DISTANCE NUMBERS

 01  01  01  01  01  01  01 
 01  02  02  02  02  02  01
 01  02  02  02  02  02  01
 01  02  01  01  01  02  01 
 01  01   0   0   0  01  01  

The inputs are then weighted by the MOORE DISTANCE NUMBERS

   output = sum((Ith input pixel value)*(Ith MOORE DISTANCE NUMBER))
                  divided by sum(Ith MOORE DISTANCE NUMBER)
                  
The user inputs a parameter DFEATHER to set the distance that the MOORE
DISTANCE ALGORITHM will work.  Then all remaining inside numbers are set
to that value as a maximum.  If MOOREFAC is greater than one, the
value used is DFEATHER/MOOREFAC in tiles of size MOOREFAC x MOOREFAC, 
and also the DFEATHER will be adjusted slightly to a multiple of
MOOREFAC because of the tiling.  The net effect of MOOREFAC>1 is to
still feather to depth DFEATHER, but in coarser steps.

Increasing DFEATHER has two consequences.  First, the program will run
more slowly, probably sublinear to the size of this parameter.  Second,
the program will consume more memory space.  The virtual memory space in
bytes needed for image buffers and MOORE DISTANCE buffers is:

  6 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

FEATHERV has two parameters to adjust the mathematics of the feathering
calculation.  MOOREMAX and MOOREPOW.  The choice here is affected by the
quality of image geometric registration.  If the images are not in perfect
registration, consider using MOOREMAX.  If MOOREMAX causes an undesirable
brightness edge, consider using MOOREPOW with a high number, lowering it
until the brightness edge disappears.

FEATHERV is written entirely in the C programming language and uses dynamic
memory allocation to avoid imposing any restrictions on the size of the
images. 

FEATHERV HAS BOTH THE OLD TYPES OF NIBBLING (SEE NEXT PARAGRAPH) AND
A NEW EDGING CAPABILITY BASED ON THE MOORE ALGORITHM.  The new edging
nibbles in a perpendicular direction to the edge of data in all
directions (the old nibble worked from the righ or left only).  Use 
the parameter MOORENBL to invoke this.  The default of 0 is no nibble,
a value of n nibbles all values where the MOORE distance from the edge
is n, etc.  To give a smooth transition, instead of starting at n+1,
the whole MOORE distance input is reduced by n so the MOORE distance
that is used in feathering still starts at 1.  Keep in mind that
MOOREFAC=3 means that a MOORENBL=1 will nibble 3 pixels from all
edges.

FEATHERV has an option for edging the input images prior to applying the
mosaicking mode.  Edging effectively removes the 'edges' of the input images
so that pixels in the 'edges' are not considered to be contained in their
images as far as the mosaicking process is concerned.  Several parameters
determine the precise effect of edging in any situation, but basically if
edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than or
equal to a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling-in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.  NOTE THAT EDGING IS INCREASED
BY USE OF MOOREFAC.  A NIBBLE OF TWO AND A MOOREFAC OF THREE WILL RESULT
IN SIX PIXELS BEING REMOVED.

FEATHERV has a parameter named ADJUST that can take a column(s) in the
IBIS file as an adjustment to brightness on a per image basis.  See the
parameter description for more details.

RAMP CALCULATIONS

A special feature of this program is that tiepoints between frames that are
declared to be neighbors in the IBIS file are gathered.  The brightness difference
statistics are averaged and result in two additional corrections:

1.  The average difference (halved) to all neighbors is added to each frame as
    a constant.  The constant is reported in a column of the IBIS file.

2.  The remaining differences (halved) to each declared neighbor is added to
    each frame as an angled ramp or half-plane.  The half plane is "hinged",
    has a zero value, down or across the middle of the image.  At the edge
    of the image, it has a ramp height that is reported in the IBIS file.
    
The four ramps are merged in each quadrant of the image by an approximation to
angular interpolation.  For example, the 45 degree diagonal to the upper left
corner of the image would get half of its value from the left=facing ramp and
half from the top-facing ramp.  The approximation is to use

         y/(x+y) to approximate atan(y/x)/90
 and        
         x/(x+y) to approximate (90-atan(y/x))/90

for example, on the diagonal, x=y and all four formulas give .5

The constant and ramp corrections are applied after the ADJUST keyword corrections,
since it is best to apply them as a residual operation.  The feathering and
cloud removal are applied after the constant and ramp corrections.


The output image has the same data format  (byte or halfword) as the input 
images.  
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      feather INP=(a...,ibis) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      feather INP=(a...,ibis) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      feather (a...,ibis) b (sl,ss,nl,ns) optional parameters
      feather (a...,ibis) b optional parameters

      Here '(a...,ibis)' represents a list of one to 399 input image file
      names followed by an ibis file, and 'b' represents the output image file
      name.
The GeoTIFF options are:
      The following command line formats show the major allowable forms:
      feather INP=(a...,ibis,gtref) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      feather INP=(a...,ibis,gtref) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      feather (a...,ibis,gtref) b (sl,ss,nl,ns) optional parameters
      feather (a...,ibis,gtref) b optional parameters

      Here '(a...,ibis,gtref)' represents a list of one to 398 input image file
      names followed by an ibis file, and then by a GeoTIFF reference image
      that defines the overall pixel space, and 'b' represents the output image file
      name.
.PAGE
EXAMPLES

See examples under program FASTMOS to learn more about the mosaicking process.
The major difference to note here is that SL,SS can be given for the mosaic
image space.
.PAGE
RESTRICTIONS
1. The input and output images must be byte or halfword data.
2. Plan carefully using the virtual memory limit formula

  6 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

 OPERATION:

FEATHERV performs mosaicking on a line by line basis.  The offsetting of
input images is done at READ time.  However, the program has to read ahead
of the output line by distance DFEATHER to run the MOORE DISTANCE ALGORITHM.
This readahead buffer is called the "footprint" of the input.  The footprint
has to go above, to the right, to the left, and below the image output area
to get the MOORE DISTANCE NUMBERS.  Because the footprint buffers are large,
a rolling index scheme is used on them and on the MOORE DISTANCE BUFFERS.

Data in halfword format may include negative data numbers.  Negative data
numbers that do not meet the threshold criteria are ignored.
.PAGE

 TIMING: 

Will get back with this when some big cases are run.  Expect 3x slower
than FASTMOS.  Expect good behavior in the virtual memory (not a lot of
swapping).  

 ORIGINAL PROGRAMMER:    A. Zobrist          27 Oct 1999
 COGNIZANT PROGRAMMER:   Barbara McGuffie    27 Oct 1999
 
 REVISION HISTORY
  99-10-27    AZ   Initial version, named feather
  99-11-05    AZ   two pass version, named feather2
  99-11-18    AZ   ramps added to feather2
  99-11-18    AZ   halfword MOORE algorithm, named featherh
  99-11-28    AZ   all options rolled into featherh
  99-11-30    AZ   reduced resolution MOORE algorithm, named featherv
  00-04-16    AZ   GeoTIFF label use
  04-02-04    AZ   igncase added
Thu Jan 10 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  
  
.LEVEL1
.VARIABLE INP
Input image file names
followed by the controlling
IBIS file, and an optional
GeoTIFF reference image
.VARIABLE OUT
Output image file name
.VARIABLE COLS
Columns of the IBIS file that
contain the mosaic information
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
.VARIABLE SL
Starting line number
(can window the output)
.VARIABLE SS
Starting sample number
(can window the output)
.VARIABLE NL
Number of lines output
.VARIABLE NS
Number of samples output
.VARIABLE THRESH
Threshold used for mosaicking.
.VARIABLE PROGRESS
Enter for progress reporting.
.VARIABLE EDGE
Enter for edging.
.VARIABLE NTHRESH
Threshold for edging on both
left and right.
.VARIABLE LTHRESH
Threshold for edging on left.
.VARIABLE RTHRESH
Threshold for edging on right.
.VARIABLE NSEQ
Number of sequential pixels
which must satisfying edging 
threshold criteria at edge of
scene data.
.VARIABLE NIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on both left and right.
.VARIABLE LNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on left.
.VARIABLE RNIBBLE
Number of pixels to remove
beyond edge of scene data for
edging on right.
.VARIABLE NINCR
If NINCR=n, then scanning for
edge of scene data will check
every nth pixel.
.VARIABLE ADJUST
Set this to use IBIS columns
as a brightness adjustment
.VARIABLE DFEATHER
Feather width in pixels (div 2)
.VARIABLE MOOREFAC
Factor to reduce resolution of
MOORE algorithm, must be odd
.VARIABLE MOOREMAX
If selected, does not blend the
images, but takes 100% of image
with largest Moore number.
.VARIABLE MOOREPOW
Alternative to linear interp;
larger than 1.0 concentrates 
the feathering near the 50/50
Moore split
.VARIABLE CLOUDOUT
Brightness difference to
identify clouds or glint
for erasure
.VARIABLE RAMP
'RAMP - apply ramping procedure
'NORAMP - don't apply ramping
'READRAMP - apply previous ramp
      values from file
.VARIABLE RDKTHR
Discard tiepoints with either
image raw value below this
.VARIABLE RDIFFTHR
Discard tiepoints with diff
in adjusted values above this
.VARIABLE RMOORE
Moore distance to use for
gathering tiepoints
.VARIABLE GOREFAC
Means of adjusting ramp for
gores
.VARIABLE RCOLS
The IBIS columns to input 
neighbors for ramping and
to output the ramp parameters
.VARIABLE MOORENBL
Nibble using MOORE distance
up to this value
.VARIABLE GEOTIFF
Use GeoTIFF labels from all
image inputs, including a
master reference as last input
.VARIABLE TOLER
Amount that GeoTIFF calculated
offsets can vary from integral
values
.VARIABLE IGNCASE
If selected, the character case
of the landsat files vs the
filenames in the ibis file is
ignored
.LEVEL2
.VARIABLE INP
The last required file is an IBIS file giving information on the input
files.  The columns of the file are usually:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input
3.  The offset SS of the image input
4.  The NL of the image input (must be exact, cannot be less as in FASTMOS)
5.  The NS of the image input (must be exact, cannot be less as in FASTMOS)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

the numbering of the columns can be changed by the parameter COLS.  See
parameter ADJUST for an explanation of column six.

The program limits at present (9/99):
1. 400 inputs
2. The number allowed by the system for simultaneous open (somewhere 
   between 67 and 100 according to one expert).
   The files are opened and closed as a narrow mosaicking band passes
   from top to bottom of the output.  The width of this band is DFEATHER.
   The number of files open over the band cannot exceed the VICAR
   or system limit for number of files open at once.  It is possible that
   a UNIX system call can enlarge this.

The optional GeoTIFF reference image, which is placed after the IBIS file,
is used in the following way:
1.  Its pixel referencing is a master for the mosaic.
2.  All other images will have offsets relative to the master.
3.  The SIZE or SL,SS,NL,NS parameters will be relative to the master.
4.  The offsets can be placed in the IBIS file, in which case they are
    checked against the GeoTIFF mappings.
5.  The offsets can be zero'd in the IBIS file, in which case they are
    calculated from the GeoTIFF mappings.  NL,NS columns must be zero'd
    for this to occur and they are also calculated.
6.  For this case, the 'GEOTIFF keyword must be given and all input images
    are required to have GeoTIFF labels.
.VARIABLE OUT
A major difference with program FASTMOS is that (SL,SS) can be used to 
window down into the mosaic.  See HELP level 2 under SL.
.VARIABLE COLS
The last file is an IBIS file giving information on the input files.  The
columns of the file are usually:

1.  The name of the image file in any order, with or without pathname.
    Only the part of the pathname/filename after the last \ or / is read
    for checking
2.  The offset SL of the image input
3.  The offset SS of the image input
4.  The NL of the image input (must be exact, cannot be less as in FASTMOS)
5.  The NS of the image input (must be exact, cannot be less as in FASTMOS)
6.  Input factor for brightness equalization formulas (only one as of 9/99)
7.  Reserved for more complex brightness equalization formulas

By using this parameter, the columns can be renumbered as the user desires.
.VARIABLE SIZE
If the SIZE field is not entered, the output image has the same size as the
first input image.  If the SIZE field is entered, the number of lines and
number of samples refer to the size of the output image.
.VARIABLE SL
The default is 1.  Setting it larger moves the "window" of the mosaic upwards
in line number and is the same as lowering all of the individual input SL's
by an equivalent amount.  This means that the mosaic can conveniently be done
in sections by setting (SL,SS,NL,NS) in a checkerboard fashion and then by
butting together the checkerboard pieces with the VICAR programs APPEND and
MSS.
.VARIABLE SS
The default is 1.  Setting it larger moves the "window" of the mosaic upwards
in sample number and is the same as lowering all of the individual input SS's
by an equivalent amount.  This means that the mosaic can conveniently be done
in sections by setting (SL,SS,NL,NS) in a checkerboard fashion and then by
butting together the checkerboard pieces with the VICAR programs APPEND and
MSS.
.VARIABLE MMODE
The mosaicking mode specifies how the output data number values are determined 
from the input data numbers.   The following rules apply for each of the 
modes.  If none of the input images have a data number value for a 
given pixel that is greater than or equal to the THRESH value, the 
output data number is the data number from the first input image if 
the pixel is contained in the first input image, and the output 
data number is L0 if the pixel is not contained in the first input image, where
L0 is 0 if THRESH is greater than 0, L0 is 0 if THRESH=0 and the data format 
is byte, and L0 is equal to THRESH-1 otherwise.  If
exactly one of the input images has a data number value for a given pixel that
is greater than or equal to the THRESH value, the output data number is the
data number from the one input image.  If more than one of the input images
have a data number value for a given pixel that is greater than or equal to the
THRESH value, the output data number is determined by the mosaicking mode. 

There are currently five modes to choose from.  They are listed by name below.
For each mode a description is given of how the output data number is 
determined when there is more than one input image having a data number value 
that is greater than or equal to the THRESH value for a given pixel.  The
default mode is OVERLAY.

OVERLAY  - The input images are checked against the THRESH value in the order
           in which they are entered by the user.  The first data number value
           found which meets the threshold criteria is used for the output
           image.  This means that the order in which the input files are 
           entered gives a priority to the data in the files.

AVERAGE  - The average of the values meeting the threshold criteria is used.
           The average is found by integer division with no rounding.

MOD      - When there are two values meeting the threshold criteria, the 
           average of the values is used.  When there are more than two 
           values meeting the threshold criteria, the value closest to the
           average is used.  This mode may be particularly useful when
           combining many images with high bit-error rates.

MAX      - The maximum of the values meeting the threshold criteria is used.

MIN      - The minimum of the values meeting the threshold criteria is used.
.VARIABLE THRESH
Only values greater than or equal to the THRESH threshold parameter are used
by the mosaicking mode in determining the data numbers for the output image.
The THRESH value is usually greater than 0 for mosaicking.  THRESH can be
set to 0 for cases such as averaging images.  The default value is 1.
(See Example 5 in the main help for details about the case of THRESH=0
for byte data.  Users may need to convert images to halfword to use THRESH=0
for mosaicking.  Other VICAR programs, such as INSECT may be an alternative.)

For halfword images for which negative DNs are considered valid, a negative
THRESH value may be used.  In this case, 0 is an inappropriate value for
representing the absence of image data.  When THRESH is less than 0, FEATHERV
uses an output DN of THRESH-1 to represent the absence of image data.
(If THRESH = -32768, -32768 is used to represent the absence of image data.)
This value is used as an output DN where the output pixel does not lie in one
of the input images.  (See the MMODE parameter.)
.VARIABLE PROGRESS
If the PROGRESS parameter is specified, FEATHERV prints a comment every 500th
line.  The default is to not print the progress.
.VARIABLE EDGE
If the EDGE parameter is specified, all input images are edged prior to
applying the mosaicking mode. No edging is the default.  'EDGE M U S T
be specified to invoke the edging algorithm.

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.

If edging is selected, then each line of each input image is edged first on the
left and then on the right.  Edging means scanning through a line from one
end or another to the point at which the data numbers are greater than or
equal to a threshold value and then removing the pixels from the end of
the line up to a certain number of pixels beyond the point.  This is 
referred to as finding the edge of the scene data and nibbling in a 
certain number of pixels beyond the edge.  Because of the line by 
line processing performed by the program, edging is only available
in the horizontal direction.  (Edging the top and bottom of images would
normally require an intermediate data set.)  Edging is typically used
to remove distortion around the edges of pictures that was caused by
interpolation, filtering, or other things.

Several parameters are used to control the way that edging is done.  The 
parameters NTHRESH, LTHRESH, RTHRESH, NSEQ, and NINCR determine the location
of the edge of the scene data for lines of the input images.  The parameters
NIBBLE, LNIBBLE, and RNIBBLE determine how many pixels beyond the edge of 
the scene data are removed.

The edge of the scene data for a line is determined as follows.  The program
scans through the pixels of a line comparing the data numbers against the
edging threshold.  (Separate edging thresholds can be specified for scanning
from the left and scanning from the right using the LTHRESH and RTHRESH
parameters.  The NTHRESH parameter can be used to specify the same threshold
for scanning from the left and scanning from the right.)  The scanning begins
at one end of the line, and it checks successive pixels unless the NINCR
parameter is entered.  If NINCR is entered, the scanning checks only every
NINCRth pixel.  The program scans until it finds a group of NSEQ consecutive
(in terms of NINCR) pixels all of which have a data number greater than or
equal to the edging threshold.  The edge of the scene data is defined as the
first pixel (according to the direction of the scan) of that group.

The nibbling number is the number of pixels, starting with the edge of the
scene data, which are to be removed along with any pixels from the end of
the line to the edge of the scene data.  (If the nibbling number is zero, 
then just the pixels from the end of the line to the edge of the scene data
are removed.)  Separate nibbling numbers can be specified for scanning from 
the left and scanning from the right using the LNIBBLE and RNIBBLE parameters.
The NIBBLE parameter can be used to specify the same nibbling number for 
scanning from the left and scanning from the right.  

If no edge of the scene data is found when scanning, the entire line is 
removed.
.VARIABLE NTHRESH
The default for NTHRESH is THRESH.  (See also under EDGE.)
.VARIABLE LTHRESH
The default for LTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE RTHRESH
The default for RTHRESH is NTHRESH.  (See also under EDGE.)
.VARIABLE NSEQ
The default for NSEQ is 8.  (See also under EDGE.)
.VARIABLE NIBBLE
The default for NIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE LNIBBLE
The default for LNIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE RNIBBLE
The default for RNIBBLE is 4.  (See also under EDGE.)

ALSO NOTE THAT EDGING IS INCREASED BY THE USE OF MOOREFAC.  A NIBBLE OF
TWO AND A MOOREFAC OF THREE WILL RESULT IN SIX PIXELS BEING REMOVED.
.VARIABLE NINCR
The default for NINCR is 1.  (See also under EDGE.)
.VARIABLE ADJUST
This keyword is defaulted to 'NOADJ which does no brightness adjustment.
Three kinds of adjustment can be performed using column 6 of the IBIS
file:

'FACTOR - All input images will be multiplied by the number in column 6
of the IBIS file as a brightness adjustment.  Note that there is a
mathematical risk of a one turing into a zero if the factor is less
than .5.  This would be significant if zero means "no data" for
mosaicking.

'ADD - All input images will be added to by the number in column 6
of the IBIS file as a brightness adjustment.

'ADDZMASK - All input images will be added to by the number in column 6
of the IBIS file as a brightness adjustment except that zeros/nonzeros
are preserved (pixels that were greater than 0 prior to the add are
compared to a minimum of 1, zeros remain at zero).  This is useful for
the case where zero means "no data" for mosaicking.

The adjustment is performed just prior to the output or the averaging for
output, so it does not affect the edging operations.  The cloud erasing
operation is done after this adjustment operation.
.VARIABLE DFEATHER
Width of feathering is the max distance of the MOORE DISTANCE ALGORITHM
measured in pixels.  The effective feathering will be twice this distance
because a large overlap will feather one image to its full MOORE DISTANCE
yielding a 50/50 averaging of the two images.  Then a reverse feather of 
the other image will take place.

A larger number will feather the inputs to a greater degree, but the program 
will use more time and memory.  Plan carefully using the virtual memory limit
formula

  6 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

If MOOREFAC is greater than one, the value used is DFEATHER/MOOREFAC
in tiles of size MOOREFAC x MOOREFAC, and also the DFEATHER will be
adjusted slightly to a multiple of MOOREFAC because of the tiling.
The net effect of MOOREFAC>1 is to still feather to depth DFEATHER,
but in coarser steps.
.VARIABLE MOOREFAC
In the formula for storage:

  6 x (max number of inputs open) x (max input NS) x DFEATHER / MOOREFAC**2

DFEATHER increases storage linearly, and also increases compute time.  The
MOOREFAC reduces the resolution of the MOORE algorithm and decreases storage
quadratically, and also saves compute time.  Further, the ratio of
DFEATHER to MOOREFAC must be less than 65536.  

A good suggested value is a ratio of 50 to 100.  This will result in
100 to 200 steps of brightness in feathering from one image to the other.
If ramping is used, the need for feathering is reduced and a smaller
ratio can be used.

Keep in mind that increasing MOOREFAC does not decrease the feathering
distance in the images, it simply coarsens the number of steps in the
MOORE distance function.  The human eye probably cannot see more than
100 steps in the typical mosaicking situation.
.VARIABLE CLOUDOUT
If a pixel comes from multiple inputs and one of the inputs is brighter than
one of the others (which of the others is order dependent for three or more)
by this amount, then it is deleted from the averaging process.  This
correction is applied after the brightness adjustment (see keyword ADJUST).
.VARIABLE RAMP
'RAMP - apply ramping procedure;
   The first pass over the files calculates the ramp values to apply
   to each scene.  The second pass constructs ramps to smooth together
   the scenes.  The ramp values are written to the IBIS file.
'NORAMP - don't apply ramping
   A single pass smoothes together the scenes using the Moore distance
   interpolation only.  No ramp values are calculated. No ramp values
   are written to the IBIS file.
'READRAMP - apply previous ramp values from file
   This option requires an IBIS file with ramp values calculated from
   a previous run with the 'RAMP option.  The previous run can be on
   a reduced image... the ramp values are size-independent.  A single
   pass is performed like the second pass of the 'RAMP option, and
   no values are written to the IBIS file.
.VARIABLE RDKTHR
This is for throwing out unreliable water or shadow pixels in Landsat, or space,
shadow, etc. for planets.
.VARIABLE RDIFFTHR
This allows glimmer, seasonal variation, wetness, etc. to be discarded from
tiepoints.  It is applied after the adjust parameter correction.
.VARIABLE RMOORE
Tiepoints in overlap areas for an image are taken at a particular distance
inside of the neighbor image.  The Moore distance function is used to determine
when a pixel is that distance inside.  The unit of distance is the pixel.
By setting this parameter to 7 (for example) all pixels that are a distance
of 7 from the edge of non-0 data in the neighbor overlap will be used as tiepoints.

The tiepoints from different neighbor frames are kept separate in the tally,
so that brightness ramps can be constructed to each neighbor.  Neighbors are
specified in the IBIS file input (see parameter rcols).
.VARIABLE GOREFAC
The ramps are four half-images hinged horizontally and vertically in the middle 
of an image.  The difference with a neighbor (divided by 2) is the height of the 
ramp at the edge of the image.  But the data may actually start inside the
image with a zero gore of data at the edge.  This factor multiplies the height of
the half plane so that it applies better to the data.
.VARIABLE RCOLS
The ten columns in order are (using their default column numbers, which can be changed)

8.   An index of the images (any unique integer index will do)
9.   Right neighbor to ramp to, or 0 if no neighbor to the right (using the index
     of column 8)
10.  Bottom neighbor to ramp to, or 0 if no neighbor to the bottom (using the index
     of column 8)
11.  Left neighbor to ramp to, or 0 if no neighbor to the left (using the index
     of column 8)
12.  Top neighbor to ramp to, or 0 if no neighbor to the top (using the index
     of column 8)
13.  Constant calculated by the ramping procedure, add to this image
14.  Ramp constant calculated by the ramping procedure for right-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
15.  Ramp constant calculated by the ramping procedure for bottom-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
16.  Ramp constant calculated by the ramping procedure for left-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)
17.  Ramp constant calculated by the ramping procedure for top-facing ramp (a
     value of -999.0 means that there is no ramp in this direction##)

## A ramp of height zero is different than no ramp.  The ramp of height zero
would be interpolated with neighboring ramps, causing them to curve towards
zero.  No ramp (-999.0) would not be interpolated with neighboring ramps.
.VARIABLE GEOTIFF
GeoTIFF Options:  If the images all have GeoTIFF labels and the 'GEOTIFF
keyword is given, then there are two possibilities:

The first is: if columns 2-5 contain non-zero values (only column 4 is
checked for 0), then the values are compared with values calculated from
the GeoTIFF labels or the nl,ns from the VICAR label.  Since the values
might calculate with a slight inaccuracy, the TOLER parameter is provided
with a default value of 1.e-7.  If a calculated offset exceeds the TOLER
parameter, this indicates that the files have an inaccurate mapping for
the purposes of mosaicking and do not have an integral offset.  All mappings
are checked for consistency (map projection type, zones, meridians,
etc.).

The second is: if columns 2-5 contain zero values (only column 4 is
checked for 0), then the values are calculated from the GeoTIFF labels
or the nl,ns from the VICAR label.  The only check is that the offsets
are integral values (within the parameter TOLER) and that the mappings
are consistent.
.VARIABLE TOLER
The calculated value, if within TOLER of an integral value, will be converted 
to the integral value for purposes of mosaicking.
.VARIABLE MOORENBL
FEATHERV HAS BOTH THE OLD TYPES OF NIBBLING (SEE PARAM EDGE) AND
A NEW EDGING CAPABILITY BASED ON THE MOORE ALGORITHM.  The new edging
nibbles in a perpendicular direction to the edge of data in all
directions (the old nibble worked from the righ or left only).  Use 
the parameter MOORENBL to invoke this.  The default of 0 is no nibble,
a value of n nibbles all values where the MOORE distance from the edge
is n, etc.  To give a smooth transition, instead of starting at n+1,
the whole MOORE distance input is reduced by n so the MOORE distance
that is used in feathering still starts at 1.  Keep in mind that
MOOREFAC=3 means that a MOORENBL=1 will nibble 3 pixels from all
edges.
.VARIABLE MOOREMAX
This will help if the images do not register perfectly in the geometric (x,y)
sense.  A road that appears doubled in the overlap area will turn into a 
disjoint line.  The seam will be along the polyline where the Moore numbers
are at equality.  Do not use this parameter along with MOOREPOW.

FEATHERV has two parameters to adjust the mathematics of the feathering
calculation.  MOOREMAX and MOOREPOW.  The choice here is affected by the
quality of image geometric registration.  If the images are not in perfect
registration, consider using MOOREMAX.  If MOOREMAX causes an undesirable
brightness edge, consider using MOOREPOW with a high number, lowering it
until the brightness edge disappears.
.VARIABLE MOOREPOW
If set to a number greater thatn the default 1.0, the Moore distance values
will be taken to this power.  So the linear ratio of Moore numbers, say, 80
to 20 would be converted into 80**pow to 20**pow (or a pow
split

FEATHERV has two parameters to adjust the mathematics of the feathering
calculation.  MOOREMAX and MOOREPOW.  The choice here is affected by the
quality of image geometric registration.  If the images are not in perfect
registration, consider using MOOREMAX.  If MOOREMAX causes an undesirable
brightness edge, consider using MOOREPOW with a high number, lowering it
until the brightness edge disappears.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstfeatherv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="no"


!   TEST SCRIPT FOR featherv, really simple small case to see offsets
!   also compare with fastmos to see offsets the same

gen xim2 12 10 SINC=0 LINC=0 ival=10
gen xim3 10 12 SINC=0 LINC=0 ival=110

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(6,3,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,7,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=1 ss=1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes

fastmos (xim2,xim3) ximmos2 nl=17 ns=15 off1=(6,3) off2=(4,7)

list ximmos2 'zeroes


! case to test two pass ramp corrections, use small overlap, foursquare

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,11,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(12,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,10,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes

ibis-list xxa 'format

! now test the 'READRAMP feature

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'readramp moorefac=1

difpic (ximmos,ximmos2)

! now test the 'NORAMP feature

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos3 sl=3 ss=2 nl=20 ns=18 dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'noramp moorefac=1

list ximmos3 'zeroes

! now test the size parm feature, also case insensitive filenames

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos4 size=(3,2,20,18) dfeather=4 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'noramp moorefac=1

difpic (ximmos3,ximmos4)


!   TEST SCRIPT FOR featherv, simple small case


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,3,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes

!   TEST SCRIPT FOR featherv, simple small case, top edge


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-2,4,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-1,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=3 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes

!   TEST SCRIPT FOR featherv, simple small case, right edge

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,9,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,8,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, top edge


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-2,4,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-1,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=3 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, simple small case, left edge

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,-2,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,-4,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, cloudout

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,-2,10,12,0.88) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(1,-4,12,10,1.5) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress cloudout=60 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, simple small case, nibble, nibble left-right 

gen xim1 12 10 SINC=1 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=2 LINC=0 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,1,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(7,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge lnibble=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge rnibble=2 'noramp moorefac=1

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, thresh, thresh left-right 
!   also the nincr param test

gen xim1 12 10 SINC=1 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=2 LINC=0 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(3,1,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(7,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 nthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 lthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes

featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 rthresh=6 nseq=2 'noramp moorefac=1

list ximmos 'zeroes
featherv (xim2,xim3,xxc) ximmos sl=-1 ss=-1 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'edge nibble=2 rthresh=6 nseq=2 nincr=2 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, large case

gen xim1 1200 1000 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,500.5,1.,500.5,        1.,1000.,1.,1000.,+
            1200.,1.,11.9,1.,  1200.,500.5,1200.04,500.5,   1200.,1000.,1100.6,1000.)
gen xim1 1000 1200 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,1200.,0.,1300.,+
            1000.,1.,1100.,0., 1000.,1200.,1100.,1300.)

list xim2 'zeroes linc=100 sinc=100
list xim3 'zeroes linc=100 sinc=100

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,3,1000,1200,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,1200,1000,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc) ximmos sl=3 ss=2 nl=1700 ns=1500 dfeather=40 +
   'factor 'progress  'noramp moorefac=1

list ximmos 'zeroes  linc=100 sinc=100

! case to test storage regeneration, maxcross=2

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,1.0) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


! test case for 'ADD keyword


gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,-1) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,-2) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,-3) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,-4) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format



featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1

list ximmos 'zeroes


! test case for 'ADDZMASK keyword


gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,-1) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,-2) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,-3) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,-4) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format



featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'addz 'progress 'noramp moorefac=1

list ximmos 'zeroes



!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, MOOREFAC=3, vertical
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,16) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes

! case to test two pass ramp corrections with MOOREFAC=3

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,8,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(9,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(8,7,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=3

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes ns=16
list ximmos2 'zeroes ns=16

f2 inp=(ximmos,ximmos2) out=ximmos3 func="abs(in1-in2)"
list ximmos3 'zeroes ns=16

ibis-list xxa 'format

!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total
!   now using unique input lines to see if proper lines in footprint
!   have to use standard debug statement in code to see this

gen xim1 13 13 SINC=1 LINC=10 ival=10
gen xim2 13 13 SINC=1 LINC=10 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, simple small case, leave out one input
!   but input doesn't touch output area, see devfeatherv.pdf for case
!   where it does touch output area and causes error condition


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,33,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,7,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


!   GeoTIFF case, no offsets in ibis file, standard vicar rotation

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.5,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.5,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.5,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.5,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.5,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.5,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.3,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes
gtlist ximmos

!   GeoTIFF case, no offsets in ibis file, now use area
!   type file different rotation

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.5,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes
gtlist ximmos

! base case for moorenbl, see second output with nibble

gen xim1 12 10 SINC=2 LINC=0 ival=1
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=2 ival=2
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,3,10,12,0) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(-4,5,12,10,0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(25,3,12,10,0) datacols=(2,3,4,5,6) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(27,5,10,12,0) datacols=(2,3,4,5,6) +
      string=("./xim5") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-list xxb 'format

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1
list ximmos 'zeroes sl=20

featherv (xim2,xim3,xim4,xim5,xxb) ximmos sl=3 ss=2 nl=30 ns=15 dfeather=4 +
   'add 'progress 'noramp moorefac=1 moorenbl=1
list ximmos 'zeroes sl=20

end-proc
$!-----------------------------------------------------------------------------
$ create devfeatherv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"



!   GeoTIFF case, no offsets in ibis file, now use area
!   type file different rotation, negative sl,ss and check results
!   with gtcomp for data offset within mosaic. VERY IMPORTANT TEST.

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.4,.4,0.0)", +
          "ModelTiePointTag=(0,10,0,.6,.4,0.0)", +
          "ModelTiePointTag=(10,0,0,.4,.6,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,10.,0.,11.,+
            12.,1.,13.,0., 12.,10.,13.,11.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.5,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=1(RasterPixelIsArea)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(0,0,0,0,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=-1 ss=-6 nl=12 ns=14 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes

gtcopy ximmos ximsubmos size=(5,3,5,5)

list ximsubmos 'zeroes
gtcomp ximsubmos ximmos
gtcomp ximsubmos xim2


!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


!   TEST SCRIPT FOR featherv, MOOREFAC=3, vertical
!   not a pretty case since overlap is almost total


gen xim1 13 13 SINC=0 LINC=0 ival=10
gen xim2 13 13 SINC=0 LINC=0 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(4,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,16) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes

! case to test two pass ramp corrections with MOOREFAC=3

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

copy xim2 xim4
copy xim3 xim5

list xim4 'zeroes
list xim5 'zeroes

copy xim3 xim6    ! extra file for out of strip test

list xim6 'zeroes

ibis-gen xxa nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(4,1,10,12,.8,1509,1563,1510,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(1,8,12,10,4.0,1563,0,1564,1509,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim2") strcols=(1)

ibis-gen xxc nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(9,2,12,10,6.0,1510,1564,0,0,1509) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=(".\xim4") strcols=(1)

ibis-gen xxd nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(8,7,10,12,1.2,1564,0,0,1510,1563) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim5") strcols=(1)

ibis-gen xxf nr=1 nc=16 format=("A10","FULL","FULL","FULL","FULL","DOUB", +
      "FULL","FULL","FULL","FULL","FULL","REAL","REAL","REAL","REAL","REAL") +
      data=(11,31,10,12,1.2,1565,0,0,0,0) +
      datacols=(2,3,4,5,6,7,8,9,10,11) +
      string=("./xim6") strcols=(1)

ibis-cat (xxa,xxb) xxe
ibis-cat (xxe,xxc) xxa
ibis-cat (xxa,xxd) xxb
ibis-cat (xxb,xxf) xxa
ibis-list xxa 'format

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=3

featherv (xim2,xim3,xim4,xim5,xim6,xxa) ximmos2 sl=3 ss=2 nl=20 ns=18 dfeather=6 +
   'factor 'progress  rcols=(7,8,9,10,11,12,13,14,15,16) rmoore=1 +
   gorefac=0.5 rdkthr=1 rdiffthr=140 'ramp moorefac=1

list ximmos 'zeroes ns=16
list ximmos2 'zeroes ns=16

f2 inp=(ximmos,ximmos2) out=ximmos3 func="abs(in1-in2)"
list ximmos3 'zeroes ns=16

ibis-list xxa 'format


! DO NOT DISCARD THE NEXT CASE FROM DEVFEATHERV.PDF SINCCE IT REQUIRES
! DEBUG STATEMENTS

!   TEST SCRIPT FOR featherv, MOOREFAC=3, horizontal
!   not a pretty case since overlap is almost total
!   now using unique input lines to see if proper lines in footprint
!   have to use standard debug statement in code to see this

gen xim1 13 13 SINC=1 LINC=10 ival=10
gen xim2 13 13 SINC=1 LINC=10 ival=110

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.0) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.0) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes




!   TEST THE 255 MAX FOR BYTE CASE, NO ROLLOVER


gen xim1 13 13 SINC=1 LINC=1 ival=243
gen xim2 13 13 SINC=1 LINC=1 ival=243

list xim1 'zeroes
list xim2 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,2,13,13,1.02) datacols=(2,3,4,5,6) +
      string=(".\xim1") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(2,4,13,13,1.02) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
ibis-list xxc 'format

featherv (xim1,xim2,xxc) ximmos size=(1,1,18,18) dfeather=12 +
   'factor 'progress 'noramp moorefac=3 

list ximmos 'zeroes


! CASES THAT CHECK ERROR CONDITIONS FOLLOW


!   TEST SCRIPT FOR featherv, simple small case, leave out one input
!   but input DOES touch output area and causes error condition


gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim2 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim3 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

list xim2 'zeroes
list xim3 'zeroes

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(11,7,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(9,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xxc) ximmos sl=3 ss=2 nl=17 ns=15 dfeather=4 +
   'factor 'progress 'noramp moorefac=1

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file, toler error

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.30001,.30001,0.0)", +
          "ModelTiePointTag=(3,-11,0,.50001,.30001,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.30001,.50001,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(12,8,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(10,3,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


!   GeoTIFF case, offsets and nl,ns are in ibis file, mismatch err

gen xim1 nl=1 ns=1
gtgen inp=xim1 out=ximmaster 'tiecnvrt +
   geotiff=("ModelTiePointTag=(0,0,0,.3,.3,0.0)", +
          "ModelTiePointTag=(10,0,0,.5,.3,0.0)", +
          "ModelTiePointTag=(0,10,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gen xim1 12 10 SINC=0 LINC=0 ival=10
geomv xim1 xim4 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,5.5,1.,5.5,        1.,10.,1.,10.,+
            12.,1.,11.9,1.,  12.,5.5,12.04,5.5,   12.,10.,11.6,10.)
gen xim1 10 12 SINC=0 LINC=0 ival=110
geomv xim1 xim5 + 
   TIEPOINT=(1.,1.,0.,0.,   1.,12.,0.,13.,+
            10.,1.,11.,0., 10.,12.,11.,13.)

gtgen inp=xim4 out=xim2 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-2,-9,0,.3,.3,0.0)", +
          "ModelTiePointTag=(8,-9,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-2,1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

gtgen inp=xim5 out=xim3 'tiecnvrt +
   geotiff=("ModelTiePointTag=(-7,-11,0,.3,.3,0.0)", +
          "ModelTiePointTag=(3,-11,0,.5,.3,0.0)", +
          "ModelTiePointTag=(-7,-1,0,.3,.5,0.0)", +
          "ProjectionGeoKey=20(CT_MillerCylindrical)", +
          "GTRasterTypeGeoKey=2(RasterPixelIsPoint)", +
          "GeogEllipsoidGeoKey=7030(Ellipse_WGS84)")

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim2) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

ibis-gen xxa nr=1 nc=6 datacols=(1,2) data=(1,1) +
      format=("DOUB","DOUB","DOUB","DOUB")
pixmap (xxa,xim3) mapcols=(3,4) pixcols=(1,2) 'pixtomap
pixmap (xxa,ximmaster) mapcols=(3,4) pixcols=(5,6) 'maptopix
ibis-list xxa

list xim2 'zeroes
list xim3 'zeroes

gtlist ximmaster
gtlist xim2
gtlist xim3

ibis-gen xxa nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(12,8,10,12,0.77) datacols=(2,3,4,5,6) +
      string=(".\xim3") strcols=(1)

ibis-gen xxb nr=1 nc=6 format=("A10","FULL","FULL","FULL","FULL","DOUB") +
      data=(10,2,12,10,1.6) datacols=(2,3,4,5,6) +
      string=("./xim2") strcols=(1)

ibis-cat (xxa,xxb) xxc
!!ibis-list xxa 'format
!!ibis-list xxb 'format
ibis-list xxc 'format

featherv (xim2,xim3,xxc,ximmaster) ximmos sl=3 ss=2 nl=17 ns=15 +
    dfeather=4 +
   'factor 'progress 'noramp moorefac=1 'geotiff toler=0.0000001

list ximmos 'zeroes


theend>
end-proc
$!-----------------------------------------------------------------------------
$ create tstfeatherv.log_solos
tstfeatherv
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:34 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10  10  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:34 2012
     Samp     1       3       5       7       9      11
   Line
      1     110 110 110 110 110 110 110 110 110 110 110 110
      2     110 110 110 110 110 110 110 110 110 110 110 110
      3     110 110 110 110 110 110 110 110 110 110 110 110
      4     110 110 110 110 110 110 110 110 110 110 110 110
      5     110 110 110 110 110 110 110 110 110 110 110 110
      6     110 110 110 110 110 110 110 110 110 110 110 110
      7     110 110 110 110 110 110 110 110 110 110 110 110
      8     110 110 110 110 110 110 110 110 110 110 110 110
      9     110 110 110 110 110 110 110 110 110 110 110 110
     10     110 110 110 110 110 110 110 110 110 110 110 110
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           6           3          10          12        0.77
      ./xim2           4           7          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:34 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:35 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0  16  16  16  16  16  16  16  16  16
      5       0   0   0   0   0   0  16  16  16  16  16  16  16  16  16
      6       0   0  85  85  85  85  50  39  33  33  33  33  33  33  16
      7       0   0  85  85  85  85  62  50  43  39  39  39  39  33  16
      8       0   0  85  85  85  85  68  57  50  45  45  45  39  33  16
      9       0   0  85  85  85  85  71  62  55  50  50  45  39  33  16
     10       0   0  85  85  85  85  71  62  55  50  50  45  39  33  16
     11       0   0  85  85  85  85  71  62  55  50  50  45  39  33  16
     12       0   0  85  85  85  85  71  62  55  50  50  45  39  33  16
     13       0   0  85  85  85  85  68  57  50  50  50  50  43  33  16
     14       0   0  85  85  85  85  62  50  50  50  50  50  50  39  16
     15       0   0  85  85  85  85  50  50  50  50  50  50  50  50  16
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task fastmos
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:34 2012
 Task:FASTMOS   User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0 110 110 110 110 110 110 110 110 110
      5       0   0   0   0   0   0 110 110 110 110 110 110 110 110 110
      6       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
      7       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
      8       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
      9       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
     10       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
     11       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
     12       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
     13       0   0  10  10  10  10  10  10  10  10  10  10 110 110 110
     14       0   0  10  10  10  10  10  10  10  10  10  10   0   0   0
     15       0   0  10  10  10  10  10  10  10  10  10  10   0   0   0
     16       0   0  10  10  10  10  10  10  10  10  10  10   0   0   0
     17       0   0  10  10  10  10  10  10  10  10  10  10   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:37 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:38 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:38 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 16      
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           4           1          10          12        0.80
      ./xim2           1          11          12          10        4.00
      .\xim4          12           2          12          10        6.00
      ./xim5          11          10          10          12        1.20
      ./xim6          11          31          10          12        1.20
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
        FULL        FULL        FULL        FULL        FULL        REAL
+-----------+-----------+-----------+-----------+-----------+-----------
        1509        1563        1510           0           0        0.00
        1563           0        1564        1509           0        0.00
        1510        1564           0           0        1509        0.00
        1564           0           0        1510        1563        0.00
        1565           0           0           0           0        0.00
 
Rows: 1:5
+-----------+-----------+-----------+-----------
        C:13        C:14        C:15        C:16
        REAL        REAL        REAL        REAL
+-----------+-----------+-----------+-----------
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:40 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0   0   0   0   0   0  77  77  76  76  75  75  75  75  75
      2       0   0   0   0   0   0   0   0   0  77  77  76  76  75  75  75  75  75
      3      69  69  69  69  69  69  68  68  67  72  77  76  76  75  75  75  75  75
      4      69  69  69  69  69  69  68  68  67  72  77  76  76  75  75  75  75  75
      5      69  69  69  69  69  69  68  68  67  72  77  76  76  75  76  76  76  76
      6      69  69  69  69  69  69  68  68  67  72  77  77  76  76  77  77  77  77
      7      70  70  70  70  70  69  69  68  68  72  77  77  77  78  78  78  78  78
      8      71  71  71  71  71  71  70  69  69  73  78  78  78  79  79  79  79  79
      9      73  73  73  73  73  72  71  70  70  74  79  79  79  80  81  81  81  81
     10      76  76  76  76  76  76  76  76  76  80  84  84  84  87  84  84  84  84
     11      79  79  79  79  79  80  81  81  81  86  90  90  89  88  87  87  87  87
     12      81  81  81  81  81  81  82  82  82  87  91  91  90  89  89  89  89  89
     13      82  82  82  82  82  82  83  83  83  88  92  92  91  91  90  90  90  90
     14      83  83  83  83  83  84  84  83  83  88  93  92  92  91  91  91  91  91
     15      84  84  84  84  84  85  84  84  83  88  93  92  92  91  91  91  91  91
     16      85  85  85  85  85  85  84  84  83  88  93  92  92  91  91  91  91  91
     17      85  85  85  85  85  85  84  84  83  88  93  92  92  91  91  91  91  91
     18      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0   0   0
     19      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0   0   0
     20      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 16      
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           4           1          10          12        0.80
      ./xim2           1          11          12          10        4.00
      .\xim4          12           2          12          10        6.00
      ./xim5          11          10          10          12        1.20
      ./xim6          11          31          10          12        1.20
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
        FULL        FULL        FULL        FULL        FULL        REAL
+-----------+-----------+-----------+-----------+-----------+-----------
        1509        1563        1510           0           0      -19.00
        1563           0        1564        1509           0       35.00
        1510        1564           0           0        1509       25.00
        1564           0           0        1510        1563      -41.00
        1565           0           0           0           0        0.00
 
Rows: 1:5
+-----------+-----------+-----------+-----------
        C:13        C:14        C:15        C:16
        REAL        REAL        REAL        REAL
+-----------+-----------+-----------+-----------
        3.00        8.00     -999.00     -999.00
     -999.00        8.00       -3.00     -999.00
        3.00     -999.00     -999.00       -8.00
     -999.00     -999.00       -3.00       -8.00
     -999.00     -999.00     -999.00     -999.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:36 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:40 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0   0   0   0   0   0  40  40  40  40  40  40  40  40  40
      2       0   0   0   0   0   0   0   0   0  40  40  40  40  40  40  40  40  40
      3      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      4      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      5      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      6      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      7      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      8      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
      9      88  88  88  88  88  88  88  88  88  64  40  40  40  40  40  40  40  40
     10      74  74  74  74  74  74  74  74  74  80  86  86  86 132  86  86  86  86
     11      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     12      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     13      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     14      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     15      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     16      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     17      60  60  60  60  60  60  60  60  60  96 132 132 132 132 132 132 132 132
     18      60  60  60  60  60  60  60  60  60  60   0   0   0   0   0   0   0   0
     19      60  60  60  60  60  60  60  60  60  60   0   0   0   0   0   0   0   0
     20      60  60  60  60  60  60  60  60  60  60   0   0   0   0   0   0   0   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:41 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:41 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:41 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:41 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          11           3          10          12        0.77
      ./xim2           9           2          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:41 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      8      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      9      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     10      16  16  33  30  30  30  30  33  39  50  85  85   0   0   0
     11      16  16  33  39  39  39  39  43  50  62  85  85   0   0   0
     12      16  16  33  39  45  45  45  50  57  68  85  85   0   0   0
     13      16  16  33  39  45  50  50  55  62  68  85  85   0   0   0
     14      16  16  33  39  45  50  50  55  62  68  85  85   0   0   0
     15      16  16  33  39  50  45  45  50  57  68  85  85   0   0   0
     16      16  16  33  43  50  43  43  43  50  62  85  85   0   0   0
     17      16  16  39  39  50  39  39  39  39  50  85  85   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          -2           4          10          12        0.77
      ./xim2          -1           2          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:42 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0  16  16  16  50  50  50  50  50  50  50  85  85
      2       0   0   0  16  16  16  39  50  50  50  50  50  62  85  85
      3       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      4       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      5       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      6       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      7       0   0   0  16  16  16  33  43  43  43  43  50  62  85  85
      8       0   0   0  16  16  16  33  33  33  33  33  39  50  85  85
      9       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     10       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     11       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     12       0   0   0  16  16  16  16   0  16  16  16  16  16   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           3           9          10          12        0.88
      ./xim2           1           8          12          10        1.50
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:43 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0  15  15  15  15  15  15
      4       0   0   0   0   0   0   0   0   0  15  15  15  15  15  15
      5       0   0   0   0   0   0   0   0   0  15  15  15  15  15  15
      6       0   0   0   0   0   0   0   0   0  15  15  35  31  31  31
      7       0   0   0   0   0   0   0   0   0  15  15  35  42  42  42
      8       0   0   0   0   0   0   0   0   0  15  15  35  42  50  50
      9       0   0   0   0   0   0   0   0   0  15  15  35  42  50  56
     10       0   0   0   0   0   0   0   0   0  15  15  35  42  50  56
     11       0   0   0   0   0   0   0   0   0  15  15  35  42  56  50
     12       0   0   0   0   0   0   0   0   0  15  15  35  48  56  48
     13       0   0   0   0   0   0   0   0   0  15  15  42  42  56  42
     14       0   0   0   0   0   0   0   0   0  15  15  15  15   0  15
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          -2           4          10          12        0.77
      ./xim2          -1           2          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:44 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0  16  16  16  50  50  50  50  50  50  50  85  85
      2       0   0   0  16  16  16  39  50  50  50  50  50  62  85  85
      3       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      4       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      5       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      6       0   0   0  16  16  16  33  43  50  50  50  57  68  85  85
      7       0   0   0  16  16  16  33  43  43  43  43  50  62  85  85
      8       0   0   0  16  16  16  33  33  33  33  33  39  50  85  85
      9       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     10       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     11       0   0   0  16  16  16  16  16  16  16  16  16  16   0   0
     12       0   0   0  16  16  16  16   0  16  16  16  16  16   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           3          -2          10          12        0.88
      ./xim2           1          -4          12          10        1.50
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:45 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      4      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      5      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      6      31  31  31  31  35  42  56  97  97  97   0   0   0   0   0
      7      31  42  42  42  48  56  70  97  97  97   0   0   0   0   0
      8      31  42  50  50  56  64  76  97  97  97   0   0   0   0   0
      9      31  42  50  56  62  70  80  97  97  97   0   0   0   0   0
     10      31  42  50  56  62  70  80  97  97  97   0   0   0   0   0
     11      31  48  50  50  56  64  76  97  97  97   0   0   0   0   0
     12      35  56  48  48  48  56  70  97  97  97   0   0   0   0   0
     13      42  56  42  42  42  42  56  97  97  97   0   0   0   0   0
     14      15   0  15  15  15  15  15   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           3          -2          10          12        0.88
      ./xim2           1          -4          12          10        1.50
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:46 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      4      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      5      15  15  15  15  15  15  15   0   0   0   0   0   0   0   0
      6      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
      7      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
      8      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
      9      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
     10      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
     11      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
     12      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
     13      15  15  15  15  15  15  15  97  97  97   0   0   0   0   0
     14      15   0  15  15  15  15  15   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       1   2   3   4   5   6   7   8   9  10
      3       1   2   3   4   5   6   7   8   9  10
      4       1   2   3   4   5   6   7   8   9  10
      5       1   2   3   4   5   6   7   8   9  10
      6       1   2   3   4   5   6   7   8   9  10
      7       1   2   3   4   5   6   7   8   9  10
      8       1   2   3   4   5   6   7   8   9  10
      9       1   2   3   4   5   6   7   8   9  10
     10       1   2   3   4   5   6   7   8   9  10
     11       1   2   3   4   5   6   7   8   9  10
     12       1   2   3   4   0   6   7   8   9  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   5   7   9  12  14  17  19  21  24   0
      3       0   2   5   7   9  12  14  17  19  21  24   0
      4       0   2   5   7   9  12  14  17  19  21  24   0
      5       0   2   5   7   9  12  14  17  19  21  24   0
      6       0   2   5   7   9  12  14  17  19  21  24   0
      7       0   2   5   7   9  12  14  17  19  21  24   0
      8       0   2   5   7   9  12  14  17  19  21  24   0
      9       0   2   5   7   9  12  14  17  19  21  24   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           3           1          10          12        1.00
      ./xim2           7           3          12          10        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:48 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   5   7   9  12  14  17  19  21   0   0   0
      7       0   0   0   0   5   7   9  12  14  17  19  21   0   0   0
      8       0   0   0   0   5   7   9  12  14  17  19  21   0   0   0
      9       0   0   0   0   5   7   8  10  12  14  15  15   0   0   0
     10       0   0   0   0   5   7   8   9  11  13  13  15   0   0   0
     11       0   0   0   0   5   7   8   9  10  12  13  15   0   0   0
     12       0   0   0   0   5   7   7   8   8  10  13  15   0   0   0
     13       0   0   0   0   5   7   6   7   7   9  11  15   0   0   0
     14       0   0   0   0   0   0   3   4   5   6   7   8   0   0   0
     15       0   0   0   0   0   0   3   4   5   6   7   8   0   0   0
     16       0   0   0   0   0   0   3   4   5   6   7   8   0   0   0
     17       0   0   0   0   0   0   3   4   5   6   7   8   0   0   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:48 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   5   7   9  12  14  17  19  21  24   0   0
      7       0   0   0   0   5   7   9  12  14  17  19  21  24   0   0
      8       0   0   0   0   5   7   9  12  14  17  19  21  24   0   0
      9       0   0   0   0   5   7   8  10  12  15  16  17  17  10   0
     10       0   0   0   0   5   7   8   9  11  13  14  15  14  10   0
     11       0   0   0   0   5   7   8   9  10  12  13  13  14  10   0
     12       0   0   0   0   5   7   7   8   9  10  11  13  14  10   0
     13       0   0   0   0   5   7   6   7   7   8   9  11  14  10   0
     14       0   0   0   0   0   0   3   4   5   6   7   8   9  10   0
     15       0   0   0   0   0   0   3   4   5   6   7   8   9  10   0
     16       0   0   0   0   0   0   3   4   5   6   7   8   9  10   0
     17       0   0   0   0   0   0   3   4   5   6   7   8   9  10   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:47 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:48 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   2   5   7   9  12  14  17  19  21   0   0   0
      7       0   0   0   2   5   7   9  12  14  17  19  21   0   0   0
      8       0   0   0   2   5   7   9  12  14  17  19  21   0   0   0
      9       0   0   0   2   4   6   8  10  12  14  15  15   0   0   0
     10       0   0   0   2   4   5   7   9  11  13  13  15   0   0   0
     11       0   0   0   2   4   5   6   8  10  12  13  15   0   0   0
     12       0   0   0   2   4   5   5   7   8  10  13  15   0   0   0
     13       0   0   0   2   3   4   5   6   7   9  11  15   0   0   0
     14       0   0   0   0   1   2   3   4   5   6   7   8   0   0   0
     15       0   0   0   0   1   2   3   4   5   6   7   8   0   0   0
     16       0   0   0   0   1   2   3   4   5   6   7   8   0   0   0
     17       0   0   0   0   1   2   3   4   5   6   7   8   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
     Samp     1       3       5       7       9
   Line
      1       1   2   3   4   5   6   7   8   9  10
      2       1   2   3   4   5   6   7   8   9  10
      3       1   2   3   4   5   6   7   8   9  10
      4       1   2   3   4   5   6   7   8   9  10
      5       1   2   3   4   5   6   7   8   9  10
      6       1   2   3   4   5   6   7   8   9  10
      7       1   2   3   4   5   6   7   8   9  10
      8       1   2   3   4   5   6   7   8   9  10
      9       1   2   3   4   5   6   7   8   9  10
     10       1   2   3   4   5   6   7   8   9  10
     11       1   2   3   4   5   6   7   8   9  10
     12       1   2   3   4   0   6   7   8   9  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   5   7   9  12  14  17  19  21  24   0
      3       0   2   5   7   9  12  14  17  19  21  24   0
      4       0   2   5   7   9  12  14  17  19  21  24   0
      5       0   2   5   7   9  12  14  17  19  21  24   0
      6       0   2   5   7   9  12  14  17  19  21  24   0
      7       0   2   5   7   9  12  14  17  19  21  24   0
      8       0   2   5   7   9  12  14  17  19  21  24   0
      9       0   2   5   7   9  12  14  17  19  21  24   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           3           1          10          12        1.00
      ./xim2           7           3          12          10        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
      7       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
      8       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
      9       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
     10       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
     11       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
     12       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
     13       0   0   0   0   0   0   0  12  14  17   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0  12  14  17  19  21   0   0   0
      7       0   0   0   0   0   0   0  12  14  17  19  21   0   0   0
      8       0   0   0   0   0   0   0  12  14  17  19  21   0   0   0
      9       0   0   0   0   0   0   0  12  14  17  19  15   0   0   0
     10       0   0   0   0   0   0   0  12  14  17  19  15   0   0   0
     11       0   0   0   0   0   0   0  12  14  17  19  15   0   0   0
     12       0   0   0   0   0   0   0  12  14  17  19  15   0   0   0
     13       0   0   0   0   0   0   0  12  14  17  19  15   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   8   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   8   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   8   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   8   0   0   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   5   7   9  12  14  17   0   0   0   0   0
      7       0   0   0   0   5   7   9  12  14  17   0   0   0   0   0
      8       0   0   0   0   5   7   9  12  14  17   0   0   0   0   0
      9       0   0   0   0   5   7   8  10  11  12   0   0   0   0   0
     10       0   0   0   0   5   7   8   9  10  12   0   0   0   0   0
     11       0   0   0   0   5   7   8   8  10  12   0   0   0   0   0
     12       0   0   0   0   5   7   7   7  10  12   0   0   0   0   0
     13       0   0   0   0   5   7   6   6   8  12   0   0   0   0   0
     14       0   0   0   0   0   0   3   4   5   6   0   0   0   0   0
     15       0   0   0   0   0   0   3   4   5   6   0   0   0   0   0
     16       0   0   0   0   0   0   3   4   5   6   0   0   0   0   0
     17       0   0   0   0   0   0   3   4   5   6   0   0   0   0   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:49 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   5   7   9  12  14   0   0   0   0   0   0
      7       0   0   0   0   5   7   9  12  14   0   0   0   0   0   0
      8       0   0   0   0   5   7   9  12  14   0   0   0   0   0   0
      9       0   0   0   0   5   7   8   9  10   0   0   0   0   0   0
     10       0   0   0   0   5   7   8   8  10   0   0   0   0   0   0
     11       0   0   0   0   5   7   8   8  10   0   0   0   0   0   0
     12       0   0   0   0   5   7   7   8  10   0   0   0   0   0   0
     13       0   0   0   0   5   7   6   7  10   0   0   0   0   0   0
     14       0   0   0   0   0   0   3   4   5   0   0   0   0   0   0
     15       0   0   0   0   0   0   3   4   5   0   0   0   0   0   0
     16       0   0   0   0   0   0   3   4   5   0   0   0   0   0   0
     17       0   0   0   0   0   0   3   4   5   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
     Samp     1     201     401     601     801
   Line
      1      10  10  10  10  10  10  10  10  10  10
    101      10  10  10  10  10  10  10  10  10  10
    201      10  10  10  10  10  10  10  10  10  10
    301      10  10  10  10  10  10  10  10  10  10
    401      10  10  10  10  10  10  10  10  10  10
    501      10  10  10  10  10  10  10  10  10  10
    601      10  10  10  10  10  10  10  10  10  10
    701      10  10  10  10  10  10  10  10  10  10
    801      10  10  10  10  10  10  10  10  10  10
    901      10  10  10  10  10  10  10  10  10  10
   1001      10  10  10  10  10  10  10  10  10  10
   1101      10  10  10  10  10  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:51 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:51 2012
     Samp     1     201     401     601     801    1001
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
    101       0 110 110 110 110 110 110 110 110 110 110 110
    201       0 110 110 110 110 110 110 110 110 110 110 110
    301       0 110 110 110 110 110 110 110 110 110 110 110
    401       0 110 110 110 110 110 110 110 110 110 110 110
    501       0 110 110 110 110 110 110 110 110 110 110 110
    601       0 110 110 110 110 110 110 110 110 110 110 110
    701       0 110 110 110 110 110 110 110 110 110 110 110
    801       0 110 110 110 110 110 110 110 110 110 110 110
    901       0 110 110 110 110 110 110 110 110 110 110 110
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          11           3        1000        1200        0.77
      ./xim2           9           2        1200        1000        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:50 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:52 2012
     Samp     1     201     401     601     801    1001    1201    1401
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
    101      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    201      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    301      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    401      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    501      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    601      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    701      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    801      16  50  50  50  50  50  50  50  50  50  85  85   0   0   0
    901      16  36  36  36  36  36  36  36  36  36  85  85   0   0   0
   1001      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
   1101      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
   1201      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
   1301       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   1401       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   1501       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
   1601       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:53 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:53 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:54 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:54 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:53 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:54 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:54 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:55 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:4  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           2           3          10          12        1.00
      ./xim2          -4           5          12          10        1.00
      .\xim4          25           3          12          10        1.00
      ./xim5          27           5          10          12        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:53 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0 110  60  43  35  30  30  30  30  35  43  10   0   0
      2       0   0 110  77  60  50  43  50  43  43  50  43  10   0   0
      3       0   0 110  77  70  60  60  70  60  60  50  43  10   0   0
      4       0   0 110  77  70  77  77  90  77  70  60  43  10   0   0
      5       0   0 110  77  85  90  90 110  90  85  77  60  10   0   0
      6       0   0 110 110 110 110 110 110 110 110 110 110   0   0   0
      7       0   0 110 110 110 110 110 110 110 110 110 110   0   0   0
      8       0   0 110 110 110 110 110 110 110 110 110 110   0   0   0
      9       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     11       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     12       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     21       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     22       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     23       0  10  10  10  10  10  10  10  10  10  10   0   0   0   0
     24       0  10  10  10  10  10  10  10  10  10  10   0   0   0   0
     25       0  10  10  10  10  10  10  10  10  10  10   0   0   0   0
     26       0  10  10  10  30  30  30  30  35  43  60 110 110 110   0
     27       0  10  10  10  30  43  43  43  50  60  77 110 110 110   0
     28       0  10  10  10  30  43  53  53  60  70  85 110 110 110   0
     29       0  10  10  10  30  43  53  60  67  77  90 110 110 110   0
     30       0  10  10  10  30  43  53  60  67  77  90 110 110 110   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:57 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:31:58 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:4  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           2           3          10          12       -1.00
      ./xim2          -4           5          12          10       -2.00
      .\xim4          25           3          12          10       -3.00
      ./xim5          27           5          10          12       -4.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:56 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   1   0   1   3   4   6   7   9  10  10  17   0   0
      2       0   0   4   2   3   3   5   6   7   9   9  11  17   0   0
      3       0   0   6   4   4   5   6   6   8   9  10  12  17   0   0
      4       0   0   9   6   6   7   8   9   9  10  11  13  17   0   0
      5       0   0  11   7   9   9  10  11  11  11  12  13  17   0   0
      6       0   0  14  14  14  14  14  14  14  14  14  14   0   0   0
      7       0   0  16  16  16  16  16  16  16  16  16  16   0   0   0
      8       0   0  19  19  19  19  19  19  19  19  19  19   0   0   0
      9       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     11       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     12       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     21       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     22       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     23       0   0   0   2   4   6   8  10  12  14  16   0   0   0   0
     24       0   0   0   2   4   6   8  10  12  14  16   0   0   0   0
     25       0   0   0   2   4   6   8  10  12  14  16   0   0   0   0
     26       0   0   0   2   3   4   6   8   9   9   7   0   0   0   0
     27       0   0   0   2   3   4   6   7   8   8   6   1   1   1   0
     28       0   0   0   2   4   5   6   7   8   7   6   3   3   3   0
     29       0   0   0   2   4   6   7   8   9   9   8   6   6   6   0
     30       0   0   0   2   5   7   8   9  10  10  10   8   8   8   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:00 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:01 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:4  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           2           3          10          12       -1.00
      ./xim2          -4           5          12          10       -2.00
      .\xim4          25           3          12          10       -3.00
      ./xim5          27           5          10          12       -4.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:31:59 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:02 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   1   1   1   3   4   6   7   9  10  10  17   0   0
      2       0   0   4   2   3   3   5   6   7   9   9  11  17   0   0
      3       0   0   6   4   4   5   6   6   8   9  10  12  17   0   0
      4       0   0   9   6   6   7   8   9   9  10  11  13  17   0   0
      5       0   0  11   7   9   9  10  11  11  11  12  13  17   0   0
      6       0   0  14  14  14  14  14  14  14  14  14  14   0   0   0
      7       0   0  16  16  16  16  16  16  16  16  16  16   0   0   0
      8       0   0  19  19  19  19  19  19  19  19  19  19   0   0   0
      9       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     10       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     11       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     12       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     13       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     14       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     21       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     22       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     23       0   1   1   2   4   6   8  10  12  14  16   0   0   0   0
     24       0   1   1   2   4   6   8  10  12  14  16   0   0   0   0
     25       0   1   1   2   4   6   8  10  12  14  16   0   0   0   0
     26       0   1   1   2   3   4   6   8   9   9   7   1   1   1   0
     27       0   1   1   2   3   4   6   7   8   8   6   1   1   1   0
     28       0   1   1   2   4   5   6   7   8   7   6   3   3   3   0
     29       0   1   1   2   4   6   7   8   9   9   8   6   6   6   0
     30       0   1   1   2   5   7   8   9  10  10  10   8   8   8   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:02 2012
     Samp     1       3       5       7       9      11      13
   Line
      1      10  10  10  10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10  10  10  10  10  10  10  10  10  10
     13      10  10  10  10  10  10  10  10  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:02 2012
     Samp     1       3       5       7       9      11      13
   Line
      1     110 110 110 110 110 110 110 110 110 110 110 110 110
      2     110 110 110 110 110 110 110 110 110 110 110 110 110
      3     110 110 110 110 110 110 110 110 110 110 110 110 110
      4     110 110 110 110 110 110 110 110 110 110 110 110 110
      5     110 110 110 110 110 110 110 110 110 110 110 110 110
      6     110 110 110 110 110 110 110 110 110 110 110 110 110
      7     110 110 110 110 110 110 110 110 110 110 110 110 110
      8     110 110 110 110 110 110 110 110 110 110 110 110 110
      9     110 110 110 110 110 110 110 110 110 110 110 110 110
     10     110 110 110 110 110 110 110 110 110 110 110 110 110
     11     110 110 110 110 110 110 110 110 110 110 110 110 110
     12     110 110 110 110 110 110 110 110 110 110 110 110 110
     13     110 110 110 110 110 110 110 110 110 110 110 110 110
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim1           2           2          13          13        1.00
      ./xim2           2           4          13          13        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:02 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:03 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0  10  10  60  60  60  60  60  60  60  60  60  60  60 110 110   0   0
      3       0  10  10  60  60  60  60  60  60  60  60  60  60  60 110 110   0   0
      4       0  10  10  60  60  60  60  60  60  60  60  60  60  60 110 110   0   0
      5       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
      6       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
      7       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
      8       0  10  10  60  43  43  60  50  50  60  70  70  60  77 110 110   0   0
      9       0  10  10  60  43  43  60  50  50  60  70  70  60  77 110 110   0   0
     10       0  10  10  60  43  43  60  50  50  60  70  70  60  77 110 110   0   0
     11       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
     12       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
     13       0  10  10  60  43  43  60  60  60  60  60  60  60  77 110 110   0   0
     14       0  10  10  60  60  60  60  60  60  60  60  60  60  60 110 110   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:03 2012
     Samp     1       3       5       7       9      11      13
   Line
      1      10  10  10  10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10  10  10  10  10  10  10  10  10  10
     13      10  10  10  10  10  10  10  10  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:03 2012
     Samp     1       3       5       7       9      11      13
   Line
      1     110 110 110 110 110 110 110 110 110 110 110 110 110
      2     110 110 110 110 110 110 110 110 110 110 110 110 110
      3     110 110 110 110 110 110 110 110 110 110 110 110 110
      4     110 110 110 110 110 110 110 110 110 110 110 110 110
      5     110 110 110 110 110 110 110 110 110 110 110 110 110
      6     110 110 110 110 110 110 110 110 110 110 110 110 110
      7     110 110 110 110 110 110 110 110 110 110 110 110 110
      8     110 110 110 110 110 110 110 110 110 110 110 110 110
      9     110 110 110 110 110 110 110 110 110 110 110 110 110
     10     110 110 110 110 110 110 110 110 110 110 110 110 110
     11     110 110 110 110 110 110 110 110 110 110 110 110 110
     12     110 110 110 110 110 110 110 110 110 110 110 110 110
     13     110 110 110 110 110 110 110 110 110 110 110 110 110
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim1           2           2          13          13        1.00
      ./xim2           4           2          13          13        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:03 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:03 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0  10  10  10  10  10  10  10  10  10  10  10  10  10   0   0
      3       0  10  10  10  10  10  10  10  10  10  10  10  10  10   0   0
      4       0  60  60  60  60  60  60  60  60  60  60  60  60  60   0   0
      5       0  60  60  60  43  43  43  43  43  43  43  43  43  60   0   0
      6       0  60  60  60  43  43  43  43  43  43  43  43  43  60   0   0
      7       0  60  60  60  60  60  60  60  60  60  60  60  60  60   0   0
      8       0  60  60  60  60  60  60  50  50  50  60  60  60  60   0   0
      9       0  60  60  60  60  60  60  50  50  50  60  60  60  60   0   0
     10       0  60  60  60  60  60  60  60  60  60  60  60  60  60   0   0
     11       0  60  60  60  60  60  60  70  70  70  60  60  60  60   0   0
     12       0  60  60  60  60  60  60  70  70  70  60  60  60  60   0   0
     13       0  60  60  60  60  60  60  60  60  60  60  60  60  60   0   0
     14       0  60  60  60  77  77  77  77  77  77  77  77  77  60   0   0
     15       0 110 110 110 110 110 110 110 110 110 110 110 110 110   0   0
     16       0 110 110 110 110 110 110 110 110 110 110 110 110 110   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:05 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:06 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 16      
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           4           1          10          12        0.80
      ./xim2           1           8          12          10        4.00
      .\xim4           9           2          12          10        6.00
      ./xim5           8           7          10          12        1.20
      ./xim6          11          31          10          12        1.20
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
        FULL        FULL        FULL        FULL        FULL        REAL
+-----------+-----------+-----------+-----------+-----------+-----------
        1509        1563        1510           0           0        0.00
        1563           0        1564        1509           0        0.00
        1510        1564           0           0        1509        0.00
        1564           0           0        1510        1563        0.00
        1565           0           0           0           0        0.00
 
Rows: 1:5
+-----------+-----------+-----------+-----------
        C:13        C:14        C:15        C:16
        REAL        REAL        REAL        REAL
+-----------+-----------+-----------+-----------
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
        0.00        0.00        0.00        0.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:07 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0  77  77  76  76  75  75  75  75  75  75
      2       0   0   0   0   0   0  77  77  76  76  75  75  75  75  75  75
      3      69  69  69  69  69  69  73  72  72  73  75  75  75  75  75  75
      4      69  69  69  69  69  69  73  72  72  73  75  75  75  75  75  75
      5      69  69  69  69  69  69  71  71  72  73  75  76  76  76  76  76
      6      69  69  69  69  69  69  71  71  72  73  76  77  77  77  77  77
      7      74  74  73  73  73  72  77  77  78  78  81  81  81  81  81  82
      8      75  75  75  75  75  75  80  80  79  79  83  83  83  83  83  83
      9      77  77  77  77  77  77  81  81  82  82  86  86  86  86  85  85
     10      78  78  78  79  79  79  82  82  83  83  91  87  87  87  86  86
     11      83  83  83  83  83  84  87  87  88  89  91  91  91  91  91  91
     12      84  84  84  84  84  85  87  87  86  87  91  91  91  91  91  91
     13      85  85  85  85  85  85  87  87  86  87  91  91  91  91  91  91
     14      85  85  85  85  85  85  87  87  86  87  91  91  91  91  91  91
     15      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     16      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     17      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     18      85  85  85  85   0  85  84  84  83  83   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:07 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0  77  77  76  76  75  75  75  75  75  75
      2       0   0   0   0   0   0  77  77  76  76  75  75  75  75  75  75
      3      69  69  69  69  69  69  73  74  74  74  75  75  75  75  75  75
      4      69  69  69  69  69  69  71  72  73  74  75  75  75  75  75  75
      5      69  69  69  69  69  69  71  71  73  74  75  76  76  76  76  76
      6      69  69  69  69  69  69  70  71  73  74  76  77  77  77  77  77
      7      74  73  72  71  71  71  75  75  76  78  80  80  80  80  81  82
      8      75  75  74  74  74  74  77  78  79  80  83  83  83  83  83  83
      9      77  77  77  77  77  78  80  81  82  83  87  86  86  86  85  85
     10      78  79  80  80  80  81  82  84  85  85  91  89  89  88  87  86
     11      83  83  83  83  83  84  86  87  89  90  91  91  91  91  91  91
     12      84  84  84  84  84  85  86  87  89  89  91  91  91  91  91  91
     13      85  85  85  85  85  85  86  87  88  89  91  91  91  91  91  91
     14      85  85  85  85  85  85  86  86  86  87  91  91  91  91  91  91
     15      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     16      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     17      85  85  85  85  85  85  84  84  83  83   0   0   0   0   0   0
     18      85  85  85  85   0  85  84  84  83  83   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task f2
F2 version 26-Jul-11
F2 using hash table lookup
FUNCTION EVALUATED 49 TIMES
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:04 2012
 Task:F2        User:lwk       Date_Time:Thu Jan 26 13:32:08 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   2   2   1   0   0   0   0   0   0
      4       0   0   0   0   0   0   2   0   1   1   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   1   1   0   0   0   0   0   0
      6       0   0   0   0   0   0   1   0   1   1   0   0   0   0   0   0
      7       0   1   1   2   2   1   2   2   2   0   1   1   1   1   0   0
      8       0   0   1   1   1   1   3   2   0   1   0   0   0   0   0   0
      9       0   0   0   0   0   1   1   0   0   1   1   0   0   0   0   0
     10       0   1   2   1   1   2   0   2   2   2   0   2   2   1   1   0
     11       0   0   0   0   0   0   1   0   1   1   0   0   0   0   0   0
     12       0   0   0   0   0   0   1   0   3   2   0   0   0   0   0   0
     13       0   0   0   0   0   0   1   0   2   2   0   0   0   0   0   0
     14       0   0   0   0   0   0   1   1   0   0   0   0   0   0   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
 
Number of Rows:5  Number of Columns: 16      
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           4           1          10          12        0.80
      ./xim2           1           8          12          10        4.00
      .\xim4           9           2          12          10        6.00
      ./xim5           8           7          10          12        1.20
      ./xim6          11          31          10          12        1.20
 
Rows: 1:5
+-----------+-----------+-----------+-----------+-----------+-----------
         C:7         C:8         C:9        C:10        C:11        C:12
        FULL        FULL        FULL        FULL        FULL        REAL
+-----------+-----------+-----------+-----------+-----------+-----------
        1509        1563        1510           0           0      -19.00
        1563           0        1564        1509           0       35.00
        1510        1564           0           0        1509       25.00
        1564           0           0        1510        1563      -41.00
        1565           0           0           0           0        0.00
 
Rows: 1:5
+-----------+-----------+-----------+-----------
        C:13        C:14        C:15        C:16
        REAL        REAL        REAL        REAL
+-----------+-----------+-----------+-----------
        3.00        8.00     -999.00     -999.00
     -999.00        8.00       -3.00     -999.00
        3.00     -999.00     -999.00       -8.00
     -999.00     -999.00       -3.00       -8.00
     -999.00     -999.00     -999.00     -999.00
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:08 2012
     Samp     1       3       5       7       9      11      13
   Line
      1      10  11  12  13  14  15  16  17  18  19  20  21  22
      2      20  21  22  23  24  25  26  27  28  29  30  31  32
      3      30  31  32  33  34  35  36  37  38  39  40  41  42
      4      40  41  42  43  44  45  46  47  48  49  50  51  52
      5      50  51  52  53  54  55  56  57  58  59  60  61  62
      6      60  61  62  63  64  65  66  67  68  69  70  71  72
      7      70  71  72  73  74  75  76  77  78  79  80  81  82
      8      80  81  82  83  84  85  86  87  88  89  90  91  92
      9      90  91  92  93  94  95  96  97  98  99 100 101 102
     10     100 101 102 103 104 105 106 107 108 109 110 111 112
     11     110 111 112 113 114 115 116 117 118 119 120 121 122
     12     120 121 122 123 124 125 126 127 128 129 130 131 132
     13     130 131 132 133 134 135 136 137 138 139 140 141 142
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:08 2012
     Samp     1       3       5       7       9      11      13
   Line
      1     110 111 112 113 114 115 116 117 118 119 120 121 122
      2     120 121 122 123 124 125 126 127 128 129 130 131 132
      3     130 131 132 133 134 135 136 137 138 139 140 141 142
      4     140 141 142 143 144 145 146 147 148 149 150 151 152
      5     150 151 152 153 154 155 156 157 158 159 160 161 162
      6     160 161 162 163 164 165 166 167 168 169 170 171 172
      7     170 171 172 173 174 175 176 177 178 179 180 181 182
      8     180 181 182 183 184 185 186 187 188 189 190 191 192
      9     190 191 192 193 194 195 196 197 198 199 200 201 202
     10     200 201 202 203 204 205 206 207 208 209 210 211 212
     11     210 211 212 213 214 215 216 217 218 219 220 221 222
     12     220 221 222 223 224 225 226 227 228 229 230 231 232
     13     230 231 232 233 234 235 236 237 238 239 240 241 242
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim1           2           2          13          13        1.00
      ./xim2           2           4          13          13        1.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:08 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
     Samp     1       3       5       7       9      11      13      15      17
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0  10  11  61  62  63  64  65  66  67  68  69  70  71 121 122   0   0
      3       0  20  21  71  72  73  74  75  76  77  78  79  80  81 131 132   0   0
      4       0  30  31  81  82  83  84  85  86  87  88  89  90  91 141 142   0   0
      5       0  40  41  91  76  77  94  95  96  97  98  99 100 117 151 152   0   0
      6       0  50  51 101  86  87 104 105 106 107 108 109 110 127 161 162   0   0
      7       0  60  61 111  96  97 114 115 116 117 118 119 120 137 171 172   0   0
      8       0  70  71 121 106 107 124 115 116 127 138 139 130 147 181 182   0   0
      9       0  80  81 131 116 117 134 125 126 137 148 149 140 157 191 192   0   0
     10       0  90  91 141 126 127 144 135 136 147 158 159 150 167 201 202   0   0
     11       0 100 101 151 136 137 154 155 156 157 158 159 160 177 211 212   0   0
     12       0 110 111 161 146 147 164 165 166 167 168 169 170 187 221 222   0   0
     13       0 120 121 171 156 157 174 175 176 177 178 179 180 197 231 232   0   0
     14       0 130 131 181 182 183 184 185 186 187 188 189 190 191 241 242   0   0
     15       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     16       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     17       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          11          33          10          12        0.77
      ./xim2           9           2          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:09 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:10 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      8      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      9      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     10      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     11      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     12      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     13      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     14      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     15      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     16      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     17      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.34        0.48       10.00        3.00
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.44        0.52       12.00        8.00
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:10 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:10 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:10 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:11 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3          11           7          10          12        0.77
      ./xim2           9           2          12          10        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:10 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:12 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      8      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      9      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     10      16  16  16  16  16  16  30  33  39  50  85  85  85  85  85
     11      16  16  16  16  16  16  30  43  50  62  85  85  85  85  85
     12      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     13      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     14      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     15      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     16      16  16  16  16  16  16  33  43  50  62  85  85  85  85  85
     17      16  16  16  16  16  16  39  39  39  50  85  85  85  85  85
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.34        0.32       10.00        3.00
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.44        0.28       12.00        8.00
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:12 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:13 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:13 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:13 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           0           0           0           0        0.77
      ./xim2           0           0           0           0        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:12 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:14 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      8      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      9      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     10      16  16  16  16  16  16  30  33  39  50  85  85  85  85  85
     11      16  16  16  16  16  16  30  43  50  62  85  85  85  85  85
     12      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     13      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     14      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     15      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     16      16  16  16  16  16  16  33  43  50  62  85  85  85  85  85
     17      16  16  16  16  16  16  39  39  39  50  85  85  85  85  85
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task gtgen
gtgen version Fri Jan 11 2008
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.49        0.35       10.00        3.00
Beginning VICAR task ibis
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task pixmap
pixmap version Thu Jan  3 2008
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
+-----------+-----------+-----------+-----------+-----------+-----------
        1.00        1.00        0.53        0.45       12.00        8.00
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:15 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:15 2012
     Samp     1       3       5       7       9
   Line
      1      10  10  10  10  10  10  10  10  10  10
      2      10  10  10  10  10  10  10  10  10  10
      3      10  10  10  10  10  10  10  10  10  10
      4      10  10  10  10  10  10  10  10  10  10
      5      10  10  10  10  10  10  10  10  10  10
      6      10  10  10  10  10  10  10  10  10  10
      7      10  10  10  10  10  10  10  10  10  10
      8      10  10  10  10  10  10  10  10  10  10
      9      10  10  10  10  10  10  10  10  10  10
     10      10  10  10  10  10  10  10  10  10  10
     11      10  10  10  10  10  10  10  10  10  10
     12      10  10  10  10   0  10  10  10  10  10
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:15 2012
 Task:GTGEN     User:lwk       Date_Time:Thu Jan 26 13:32:15 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0 110 110 110 110 110 110 110 110 110 110   0
      3       0 110 110 110 110 110 110 110 110 110 110   0
      4       0 110 110 110 110 110 110 110 110 110 110   0
      5       0 110 110 110 110 110 110 110 110 110 110   0
      6       0 110 110 110 110 110 110 110 110 110 110   0
      7       0 110 110 110 110 110 110 110 110 110 110   0
      8       0 110 110 110 110 110 110 110 110 110 110   0
      9       0 110 110 110 110 110 110 110 110 110 110   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:2  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:2
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           0           0           0           0        0.77
      ./xim2           0           0           0           0        1.60
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:15 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:16 2012
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      2       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      3       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      4       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
      7      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      8      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
      9      16  16  16  16  16  16  16  16  16  16   0   0   0   0   0
     10      16  16  16  16  16  16  30  33  39  50  85  85  85  85  85
     11      16  16  16  16  16  16  30  43  50  62  85  85  85  85  85
     12      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     13      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     14      16  16  16  16  16  16  30  43  57  71  85  85  85  85  85
     15      16  16  16  16  16  16  30  43  57  68  85  85  85  85  85
     16      16  16  16  16  16  16  33  43  50  62  85  85  85  85  85
     17      16  16  16  16  16  16  39  39  39  50  85  85  85  85  85
Beginning VICAR task gtlist
gtlist version Wed Jan  2 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task gen
GEN Version 6
GEN task completed
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:GEOMV     User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task copy
 COPY VERSION 12-JUL-1993
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:18 2012
     Samp     1       3       5       7       9
   Line
      1       1   3   5   7   9  11  13  15  17  19
      2       1   3   5   7   9  11  13  15  17  19
      3       1   3   5   7   9  11  13  15  17  19
      4       1   3   5   7   9  11  13  15  17  19
      5       1   3   5   7   9  11  13  15  17  19
      6       1   3   5   7   9  11  13  15  17  19
      7       1   3   5   7   9  11  13  15  17  19
      8       1   3   5   7   9  11  13  15  17  19
      9       1   3   5   7   9  11  13  15  17  19
     10       1   3   5   7   9  11  13  15  17  19
     11       1   3   5   7   9  11  13  15  17  19
     12       1   3   5   7   0  11  13  15  17  19
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:COPY      User:lwk       Date_Time:Thu Jan 26 13:32:18 2012
     Samp     1       3       5       7       9      11
   Line
      1       0   0   0   0   0   0   0   0   0   0   0   0
      2       0   2   2   2   2   2   2   2   2   2   2   0
      3       0   5   5   5   5   5   5   5   5   5   5   0
      4       0   7   7   7   7   7   7   7   7   7   7   0
      5       0  10  10  10  10  10  10  10  10  10  10   0
      6       0  12  12  12  12  12  12  12  12  12  12   0
      7       0  15  15  15  15  15  15  15  15  15  15   0
      8       0  17  17  17  17  17  17  17  17  17  17   0
      9       0  20  20  20  20  20  20  20  20  20  20   0
     10       0   0   0   0   0   0   0   0   0   0   0   0
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
Beginning VICAR task ibis
 
Number of Rows:4  Number of Columns: 6       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+-----------+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5         C:6
         A10        FULL        FULL        FULL        FULL        DOUB
+-----------+-----------+-----------+-----------+-----------+-----------
      .\xim3           2           3          10          12        0.00
      ./xim2          -4           5          12          10        0.00
      .\xim4          25           3          12          10        0.00
      ./xim5          27           5          10          12        0.00
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list
 ** Requested area exceeds size of input picture.
 ** Number of lines printed reduced.

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:19 2012
     Samp     1       3       5       7       9      11      13      15
   Line
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     21       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     22       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     23       0   1   3   5   7   9  11  13  15  17  19   0   0   0   0
     24       0   1   3   5   7   9  11  13  15  17  19   0   0   0   0
     25       0   1   3   5   7   9  11  13  15  17  19   0   0   0   0
     26       0   1   3   5   6   8   9  11  12  12  11   2   2   2   0
     27       0   1   3   5   7   8   9  10  11  11  10   5   5   5   0
     28       0   1   3   5   7   8   9  10  11  11  10   7   7   7   0
     29       0   1   3   5   8   9  11  12  12  12  12  10  10  10   0
     30       0   1   3   5   8  10  11  13  13  14  13  12  12  12   0
Beginning VICAR task featherv
featherv version Thu Oct 09 2008
Beginning VICAR task list
 ** Requested area exceeds size of input picture.
 ** Number of lines printed reduced.

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Thu Jan 26 13:32:17 2012
 Task:FEATHERV  User:lwk       Date_Time:Thu Jan 26 13:32:19 2012
     Samp     1       3       5       7       9      11      13      15
   Line
     20       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     21       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     22       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     23       0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
     24       0   0   3   5   7   9  11  13  15  17   0   0   0   0   0
     25       0   0   3   5   7   9  11  13  15  17   0   0   0   0   0
     26       0   0   3   5   7   9  11  13  15  17   0   0   0   0   0
     27       0   0   3   5   7   8  10  11  12  11   5   5   5   0   0
     28       0   0   3   5   7   9   9  11  11  10   7   7   7   0   0
     29       0   0   3   5   7   9  11  12  12  12  10  10  10   0   0
     30       0   0   3   5   7  10  11  13  13  13  12  12  12   0   0
exit
slogoff
$ Return
$!#############################################################################
