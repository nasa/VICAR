$!****************************************************************************
$!
$! Build proc for MIPL module geomv
$! VPACK Version 1.9, Wednesday, December 21, 2011, 16:14:51
$!
$! Execute by entering:		$ @geomv
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
$ write sys$output "*** module geomv ***"
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
$ write sys$output "Invalid argument given to geomv.com file -- ", primary
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
$   if F$SEARCH("geomv.imake") .nes. ""
$   then
$      vimake geomv
$      purge geomv.bld
$   else
$      if F$SEARCH("geomv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake geomv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @geomv.bld "STD"
$   else
$      @geomv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create geomv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack geomv.com -mixed -
	-s geomv.c -
	-i geomv.imake -
	-p geomv.pdf -
	-t tstgeomv.pdf devgeomv.pdf tstgeomv.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create geomv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoVicarProtos.h"*/
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
/*#include "cartoLsqUtils.h"*/
#include "cartoGtUtils.h"

/*  image geom routine   A. Zobrist    7/14/99   */

/************************************************************************/
/*									*/
/*		open_files()						*/
/*	This routine opens the input and output image files		*/
/*									*/
/************************************************************************/

int i_unit,o_unit;			/* input unit, output unit */
int sline, ssamp, nline, nsamp;		/* User specified size of output */
int inpline, inpsamp;			/* size of primary input */


void open_files(filetype)
   int *filetype;
{
  int status,tsize[4],sizepcnt,sizedef;
  char fmt_str[10];

  /***********************/
  /* open the input file */
  /***********************/
  status = zvunit( &i_unit, "INP", 1, NULL);
  status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA", NULL);
  /* should have checked status to make sure file opened properly. */
  /* Make sure we have either BYTE or HALF */
  zvget(i_unit,"FORMAT",fmt_str, NULL);
  if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF")) {
    zvmessage("Invalid input data format.  Use BYTE or HALF.","");
    zabend();
  }
  if (strcmp(fmt_str,"BYTE")==0) *filetype = 0; else *filetype = 1;
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
  status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp,
		"OP", "WRITE",
		"OPEN_ACT", "SA",
		"IO_ACT", "SA", NULL);
  return;
}

void get_extrema(newsl,newss,newnl,newns,nahm1,navm1,grids,ldel,sdel,
      outl,outs,inl,ins,jtop,lmin,lmax,smin,smax)
   int newsl,newss,newnl,newns,nahm1,navm1,grids,jtop;
   double *outl,*outs,*inl,*ins,ldel,sdel;
   double *lmin,*lmax,*smin,*smax;
{
   int iline,jgrp,bx,nloop,ipos,jpos,icell,jcell;
   double fipos,fjpos,ficell,fjcell,fnloop1;
   double fri,frj,fri1,frj1,pi1,pi2,pj1,pj2,pipos,pjpos;
   double fiposq,fjposq,ficellq,fjcellq,piposq,pjposq;
   
   *lmin = 1.e20; *smin = 1.e20;
   *lmax = -1.e20; *smax = -1.e20;
   for (iline=0;iline<newnl;iline++)
      {
      ipos = newsl+iline;
      fipos = (double)ipos+.5;
      ficell = (fipos-outl[0])/ldel;
      icell = MAX(MIN((int)ficell,navm1),0);
      jpos = newss;
      for (jgrp=0;;jgrp++)
	 {
	 if (jpos>=jtop) break;
	 fjpos = (double)jpos+.5;
	 fjcell = (fjpos-outs[0])/sdel;
	 jcell = MAX(MIN((int)fjcell,nahm1),0);
	 fri = ficell-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcell-(double)jcell;
	 frj1 = 1.-frj;
	 bx = icell*grids+jcell;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 pipos = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjpos = pj1*frj1+pj2*frj;
	 nloop = MIN((int)(fabs(frj1)*sdel+1.),jtop-jpos);
	 fnloop1 = (double)(nloop-1);
	 
	 fiposq = fipos;
	 fjposq = fjpos+fnloop1;
	 ficellq = (fiposq-outl[0])/ldel;
	 fjcellq = (fjposq-outs[0])/sdel;
	 fri = ficellq-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcellq-(double)jcell;
	 frj1 = 1.-frj;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 piposq = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjposq = pj1*frj1+pj2*frj;
	 jpos += nloop;
	 *lmin = MIN(*lmin,pipos); *lmin = MIN(*lmin,piposq);
	 *smin = MIN(*smin,pjpos); *smin = MIN(*smin,pjposq);
	 *lmax = MAX(*lmax,pipos); *lmax = MAX(*lmax,piposq);
	 *smax = MAX(*smax,pjpos); *smax = MAX(*smax,pjposq);
	 }
      }
   return;
}

void process_block(sl,ss,nl,ns,nlp6,nsp6,newsl,newss,
      newnl,newns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
      znoin,ldel,sdel,istrip,fileptr,newnswrite,filetype)
   int sl,ss,nl,ns,nlp6,nsp6,newsl,newss,newnl,newns;
   int jtop,nahm1,navm1,grids,interp,znoin,istrip,newnswrite,filetype;
   double *outl,*outs,*inl,*ins,ldel,sdel;
   FILE *fileptr;
{
   int i,j,iline,bx,cxl,cxs,nloop,tin,xl,xs,outcount,ipos,jpos;
   int status,icell,jcell,bufpos,jgrp,cubtype;
   unsigned char **bbuf,*boutbuf;
   short int **buf,*outbuf;
   double fslbd,flubd,fssbd,fsubd,fxl,fxs,fxl1,fxs1,fipos,fjpos;
   double ficell,fjcell,rl,ru,fnloop1;
   double fri,frj,fri1,frj1,pi1,pi2,pj1,pj2,pipos,pjpos;
   double fiposq,fjposq,ficellq,fjcellq,piposq,pjposq;
   double fidel,fjdel;
   double t1,t2,t3,c1,c2,c3,c4,r1,r2,r3,r4,div6=0;
   
   if (interp==2||interp==3)
      {
      cubtype = 1;
      div6 = 1.0/6.0;
      }
   else cubtype = 0;
   
   outcount = 1;
   
   if (filetype)
      {
      mz_alloc2((unsigned char ***)&buf,nlp6,nsp6,2);
      mz_alloc1((unsigned char **)&outbuf,newnswrite,2);
      for (i=1;i<=nl;i++)
         {
         status = zvread(i_unit,&buf[i+1][2],"LINE", i+sl,
  		   "SAMP", ss+1, "NSAMPS", ns, NULL);
         buf[i+1][0] = 0;
         buf[i+1][1] = 0;
         buf[i+1][ns+2] = 0;
         buf[i+1][ns+3] = 0;
         }
      for (j=0;j<nsp6;j++)
         {
         buf[0][j] = 0;
         buf[1][j] = 0;
         buf[nl+2][j] = 0;
         buf[nl+3][j] = 0;
         }
      }
   else
      {
      mz_alloc2((unsigned char ***)&bbuf,nlp6,nsp6,1);
      mz_alloc1((unsigned char **)&boutbuf,newnswrite,1);
      for (i=1;i<=nl;i++)
         {
         status = zvread(i_unit,&bbuf[i+1][2],"LINE", i+sl,
  		   "SAMP", ss+1, "NSAMPS", ns, NULL);
         bbuf[i+1][0] = (unsigned char)0;
         bbuf[i+1][1] = (unsigned char)0;
         bbuf[i+1][ns+2] = (unsigned char)0;
         bbuf[i+1][ns+3] = (unsigned char)0;
         }
      for (j=0;j<nsp6;j++)
         {
         bbuf[0][j] = (unsigned char)0;
         bbuf[1][j] = (unsigned char)0;
         bbuf[nl+2][j] = (unsigned char)0;
         bbuf[nl+3][j] = (unsigned char)0;
         }
      }
      
   /* calculate one output line at a time */

   flubd = (double)(sl+nl)-0.499; fsubd = (double)(ss+ns)-0.499;
   fslbd = (double)sl+0.499; fssbd = (double)ss+0.499;
   fxl = 0.; fxs = 0.; fxl1 = 1.; fxs1 = 1.;
   for (iline=0;iline<newnl;iline++)
      {
      ipos = newsl+iline;
      fipos = (double)ipos+.5;
      ficell = (fipos-outl[0])/ldel;
      icell = MAX(MIN((int)ficell,navm1),0);
      jpos = newss;
      bufpos = 0;
      for (jgrp=0;;jgrp++)
	 {
	 if (jpos>=jtop) break;
	 fjpos = (double)jpos+.5;
	 fjcell = (fjpos-outs[0])/sdel;
	 jcell = MAX(MIN((int)fjcell,nahm1),0);
	 fri = ficell-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcell-(double)jcell;
	 frj1 = 1.-frj;
	 bx = icell*grids+jcell;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 pipos = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjpos = pj1*frj1+pj2*frj;
	 nloop = MIN((int)(fabs(frj1)*sdel+1.),jtop-jpos);
	 fnloop1 = (double)(nloop-1);
	 fiposq = fipos;
	 fjposq = fjpos+fnloop1;
	 ficellq = (fiposq-outl[0])/ldel;
	 fjcellq = (fjposq-outs[0])/sdel;
	 fri = ficellq-(double)icell;
	 fri1 = 1.-fri;
	 frj = fjcellq-(double)jcell;
	 frj1 = 1.-frj;
	 pi1 = inl[bx]*fri1+inl[bx+grids]*fri;
	 pi2 = inl[bx+1]*fri1+inl[bx+grids+1]*fri;
	 piposq = pi1*frj1+pi2*frj;
	 pj1 = ins[bx]*fri1+ins[bx+grids]*fri;
	 pj2 = ins[bx+1]*fri1+ins[bx+grids+1]*fri;
	 pjposq = pj1*frj1+pj2*frj;
	 if (nloop>1)
	    {
	    fidel = (piposq-pipos)/fnloop1;
	    fjdel = (pjposq-pjpos)/fnloop1;
	    }
	 else
	    {
	    fidel = 0.0;
	    fjdel = 0.0;
	    }
	 for (j=0;j<nloop;j++)
	    {
	    tin = interp;
	    xl = (int)(pipos+9.5)-10;
	    xs = (int)(pjpos+9.5)-10;
	    if (pipos<fslbd||pjpos<fssbd||pipos>flubd||pjpos>fsubd)
	       {
	          if (filetype) outbuf[bufpos+j] = 0;
	          else boutbuf[bufpos+j] = (unsigned char)0;
	          goto skip;
	       }
	    fxl = pipos-(double)xl-.5;
	    fxs = pjpos-(double)xs-.5;
	    fxl1 = 1.0-fxl;
	    fxs1 = 1.0-fxs;
	    cxl = xl-sl+2;
	    cxs = xs-ss+2;
	    
	    if (filetype)
               {	    
	       if (znoin)
	          {
	          if (cubtype)
	             {
	             if (buf[cxl-1][cxs-1]==0||buf[cxl+2][cxs-1]==0||
	                 buf[cxl-1][cxs+2]==0||buf[cxl+2][cxs+2]==0)
	                {
	                /* the zero(s) have to participate */
	                if (((buf[cxl-1][cxs-1]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl+2][cxs-1]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl-1][cxs+2]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    ((buf[cxl+2][cxs+2]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          else
	             {
	             if (buf[cxl][cxs]==0||buf[cxl+1][cxs]==0||
	                 buf[cxl][cxs+1]==0||buf[cxl+1][cxs+1]==0)
	                {
	                /* the zero(s) have to participate */
	                if (((buf[cxl][cxs]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl+1][cxs]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    ((buf[cxl][cxs+1]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    ((buf[cxl+1][cxs+1]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          }
               switch (tin)
	          {
case 1:           rl = fxl1*buf[cxl][cxs]+fxl*buf[cxl+1][cxs];
	          ru = fxl1*buf[cxl][cxs+1]+fxl*buf[cxl+1][cxs+1];
	          outbuf[bufpos+j] = (short int)(fxs1*rl+fxs*ru+.5);
                  break;

case 2:           /* cubic convolution */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  r1 = c1*buf[cxl-1][cxs-1]+c2*buf[cxl-1][cxs]+
                       c3*buf[cxl-1][cxs+1]+c4*buf[cxl-1][cxs+2];
                  r2 = c1*buf[cxl][cxs-1]+c2*buf[cxl][cxs]+
                       c3*buf[cxl][cxs+1]+c4*buf[cxl][cxs+2];
                  r3 = c1*buf[cxl+1][cxs-1]+c2*buf[cxl+1][cxs]+
                       c3*buf[cxl+1][cxs+1]+c4*buf[cxl+1][cxs+2];
                  r4 = c1*buf[cxl+2][cxs-1]+c2*buf[cxl+2][cxs]+
                       c3*buf[cxl+2][cxs+1]+c4*buf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  outbuf[bufpos+j] = (short int)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;


case 3:           /* cubic spline */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  r1 = c1*buf[cxl-1][cxs-1]+c2*buf[cxl-1][cxs]+
                       c3*buf[cxl-1][cxs+1]+c4*buf[cxl-1][cxs+2];
                  r2 = c1*buf[cxl][cxs-1]+c2*buf[cxl][cxs]+
                       c3*buf[cxl][cxs+1]+c4*buf[cxl][cxs+2];
                  r3 = c1*buf[cxl+1][cxs-1]+c2*buf[cxl+1][cxs]+
                       c3*buf[cxl+1][cxs+1]+c4*buf[cxl+1][cxs+2];
                  r4 = c1*buf[cxl+2][cxs-1]+c2*buf[cxl+2][cxs]+
                       c3*buf[cxl+2][cxs+1]+c4*buf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  outbuf[bufpos+j] = (short int)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;

case 9:           if (fxl>=.5) cxl += 1;
	          if (fxs>=.5) cxs += 1;
	          outbuf[bufpos+j] = buf[cxl][cxs];
	          break;
	          }
               }
            else
               {
               if (znoin)
	          {
	          if (cubtype)
	             {
	             if ((int)bbuf[cxl-1][cxs-1]==0||(int)bbuf[cxl+2][cxs-1]==0||
	                 (int)bbuf[cxl-1][cxs+2]==0||(int)bbuf[cxl+2][cxs+2]==0)
	                {
	                /* the zero(s) have to participate */
	                if ((((int)bbuf[cxl-1][cxs-1]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl+2][cxs-1]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl-1][cxs+2]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    (((int)bbuf[cxl+2][cxs+2]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          else
	             {
	             if ((int)bbuf[cxl][cxs]==0||(int)bbuf[cxl+1][cxs]==0||
	                 (int)bbuf[cxl][cxs+1]==0||(int)bbuf[cxl+1][cxs+1]==0)
	                {
	                /* the zero(s) have to participate */
	                if ((((int)bbuf[cxl][cxs]==0)&&(fxl1>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl+1][cxs]==0)&&(fxl>0.01)&&(fxs1>0.01))||
	                    (((int)bbuf[cxl][cxs+1]==0)&&(fxl1>0.01)&&(fxs>0.01))||
	                    (((int)bbuf[cxl+1][cxs+1]==0)&&(fxl>0.01)&&(fxs>0.01)))
	                   tin = 9;
	                }
	             }
	          }
               switch (tin)
	          {
case 1:           rl = fxl1*(float)bbuf[cxl][cxs]+fxl*(float)bbuf[cxl+1][cxs];
	          ru = fxl1*(float)bbuf[cxl][cxs+1]+fxl*(float)bbuf[cxl+1][cxs+1];
	          boutbuf[bufpos+j] = (unsigned char)(fxs1*rl+fxs*ru+.5);
                  break;

case 2:           /* cubic convolution */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  r1 = c1*(float)bbuf[cxl-1][cxs-1]+c2*(float)bbuf[cxl-1][cxs]+
                       c3*(float)bbuf[cxl-1][cxs+1]+c4*(float)bbuf[cxl-1][cxs+2];
                  r2 = c1*(float)bbuf[cxl][cxs-1]+c2*(float)bbuf[cxl][cxs]+
                       c3*(float)bbuf[cxl][cxs+1]+c4*(float)bbuf[cxl][cxs+2];
                  r3 = c1*(float)bbuf[cxl+1][cxs-1]+c2*(float)bbuf[cxl+1][cxs]+
                       c3*(float)bbuf[cxl+1][cxs+1]+c4*(float)bbuf[cxl+1][cxs+2];
                  r4 = c1*(float)bbuf[cxl+2][cxs-1]+c2*(float)bbuf[cxl+2][cxs]+
                       c3*(float)bbuf[cxl+2][cxs+1]+c4*(float)bbuf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = -0.5*t3+t2-0.5*t1;
                  c2 = 1.5*t3-2.5*t2+1.0;
                  c3 = -1.5*t3+2.0*t2+0.5*t1;
                  c4 = 0.5*t3-0.5*t2;
                  
                  boutbuf[bufpos+j] = (unsigned char)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;


case 3:           /* cubic spline */
                  t1 = fxs; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  r1 = c1*(float)bbuf[cxl-1][cxs-1]+c2*(float)bbuf[cxl-1][cxs]+
                       c3*(float)bbuf[cxl-1][cxs+1]+c4*(float)bbuf[cxl-1][cxs+2];
                  r2 = c1*(float)bbuf[cxl][cxs-1]+c2*(float)bbuf[cxl][cxs]+
                       c3*(float)bbuf[cxl][cxs+1]+c4*(float)bbuf[cxl][cxs+2];
                  r3 = c1*(float)bbuf[cxl+1][cxs-1]+c2*(float)bbuf[cxl+1][cxs]+
                       c3*(float)bbuf[cxl+1][cxs+1]+c4*(float)bbuf[cxl+1][cxs+2];
                  r4 = c1*(float)bbuf[cxl+2][cxs-1]+c2*(float)bbuf[cxl+2][cxs]+
                       c3*(float)bbuf[cxl+2][cxs+1]+c4*(float)bbuf[cxl+2][cxs+2];
                  
                  t1 = fxl; t2 = t1*t1; t3 = t2*t1;
                  c1 = (-t3+3.0*t2-3.0*t1+1.0)*div6;
                  c2 = (3.0*t3-6.0*t2+4.0)*div6;
                  c3 = (-3.0*t3+3.0*t2+3.0*t1+1.0)*div6;
                  c4 = t3*div6;
                  
                  boutbuf[bufpos+j] = (unsigned char)(c1*r1+c2*r2+c3*r3+c4*r4+.5);
                  break;

case 9:           if (fxl>=.5) cxl += 1;
	          if (fxs>=.5) cxs += 1;
	          boutbuf[bufpos+j] = bbuf[cxl][cxs];
	          break;
	          }
               }
            
skip:	    pipos += fidel;
	    pjpos += fjdel;
	    }
	 jpos += nloop;
	 bufpos += nloop;
	 }
      if (istrip<0)
         {
         if (filetype) zvwrit(o_unit,outbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, NULL);
         else zvwrit(o_unit,boutbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, NULL);
         outcount++;
         }
      else
         {
         if (filetype) fwrite(outbuf,2,newnswrite,fileptr);
         else fwrite(boutbuf,1,newnswrite,fileptr);
         }
      }
   
   if (filetype)
      {
      mz_free2((unsigned char **)buf,nlp6);
      free(outbuf);
      }
   else
      {
      mz_free2((unsigned char **)bbuf,nlp6);
      free(boutbuf);
      }
   
   return;
}
   
void main44(void)
{
   int    newsl,newss,newnl,newns,interp,filetype;
   
   double *outl,*outs,*inl,*ins;
   int i,j,lnl,lns,itype,nrec;
   int gridl,grids,iline,vmemsize,dummy,filect,filedf;
   int sl,ss,nl,ns,nlp6,nsp6,jtop,stripn,writepix;
   int bufix;
   int labnl,labns,gtfirst,len,istrip,jstrip,outcount;
   int snewsl,snewss,snewnl,snewns,linc,sinc,snewnswrite;
   unsigned char *outbuf;
   long int lbufsiz;
   char *p,*labelstr,scalestr[50],transstr[133],tmpfile[100];
   char tmpfilex[150],tstr[2];
   char *ms_find();
   double gridck,ldel,sdel;
   double lmin,smin,lmax,smax;
   double t[6],tinv[6],tout[6],toutinv[6],corner[4];
   double b,d,bcor,dcor,voff,scale1,scale2;
   FILE *tf0=0,*tf1=0,*tf2=0,*tf3=0,*tf4=0,*tf5=0,*tf6=0,*tf7=0,*tf8=0,*tf9=0,*tf10=0,
    *tf11=0,*tf12=0,*tf13=0,*tf14=0,*tf15=0,*tf16=0,*tf17=0,*tf18=0,*tf19=0,*tf20=0;
   FILE *fileptr=0;
   
   int status,parmcnt,gridIn,ibis;
   int ntiepp,tiepdef,znoin,colcount,coldef,nahm1,navm1;
   int cols[4];
   double *rpar;
   
   /* initialize, fetch params */

   zifmessage("geomv version Tue Nov 11 2008");
   
   open_files(&filetype);
   writepix = filetype+1;
   
   status = zvpcnt("inp",&parmcnt);
   zvparm("tmpfile",tmpfile,&filect,&filedf,1,99);
   zvp("vmemsize",&vmemsize,&dummy);
   if (parmcnt>1)
      {
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = zvunit(&gridIn,"inp",2, NULL);
      status = IBISFileOpen(gridIn,&ibis,IMODE_READ,0,0,0,0);
      if (status!=1) IBISSignalU(gridIn,status,1);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[0]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[1]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[2]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[3]);
      IBISFileGet(ibis,"nr",&nrec,1,1,0);
      mz_alloc1((unsigned char **)&outl,nrec,8);
      mz_alloc1((unsigned char **)&outs,nrec,8);
      mz_alloc1((unsigned char **)&inl,nrec,8);
      mz_alloc1((unsigned char **)&ins,nrec,8);
      status = IBISColumnRead(ibis,(char*)outl,cols[0],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*)outs,cols[1],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*)inl,cols[2],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*)ins,cols[3],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      }
   else
      {
      mz_alloc1((unsigned char **)&rpar,1625,8);
      zvparmd("tiepoint",rpar,&ntiepp,&tiepdef,1625,0);
      if (ntiepp==0) zmabend("No tiepoint input");
      if (ntiepp>=1625)
        zmabend("Too many grid tiepoints for parm dataset, use IBIS file");
      /* put rpar into the column format */
      nrec = ntiepp/4;
      mz_alloc1((unsigned char **)&outl,nrec,8);
      mz_alloc1((unsigned char **)&outs,nrec,8);
      mz_alloc1((unsigned char **)&inl,nrec,8);
      mz_alloc1((unsigned char **)&ins,nrec,8);
      for (i=0;i<nrec;i++)
         {
         outl[i] = rpar[i*4];
         outs[i] = rpar[i*4+1];
         inl[i] = rpar[i*4+2];
         ins[i] = rpar[i*4+3];
         }
      free(rpar);
      }
   if (parmcnt>2) /* reference image */
      {
      status = gtgetlab("inp",3,&labelstr,&labnl,&labns);
      gtfirst = status==1;
      if (gtfirst)
         {
         len = strlen(labelstr);
         for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
         status = geofix(labelstr,t,tinv,labnl,labns,corner);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, first input");
         }
      }
   
   znoin = zvptst("znoin");
   interp = 1;
   if (zvptst("bilin")) interp = 1;
   if (zvptst("noin")) interp = 9;
   if (zvptst("cubconv")) interp = 2;
   if (zvptst("cubsplin")) interp = 3;
   
   /* adjust grid 0.5 for algorithm */

   for (i=0;i<nrec;i++)
      {
      outl[i] -= 0.5;
      outs[i] -= 0.5;
      inl[i] -= 0.5;
      ins[i] -= 0.5;
      }
   newsl = sline - 1;
   newss = ssamp - 1;
   newnl = nline;
   newns = nsamp;
   lnl = inpline;
   lns = inpsamp;
   itype = 2;
   
   /* determine the grid geometry */
   /* the large loop finds min and max extremes in input */
   /* can't just do cell corners because output might be small 
      subset of large cell */

   gridck = outl[0];
   for (i=0;;i++)
      {
      grids = i;
      if (outl[i]!=gridck) break;
      }
   gridl = nrec/grids;
   if (gridl*grids!=nrec||gridl<2||grids<2)
      zmabend("warp grid not rectangular");
   for (i=1;i<gridl;i++)
      for (j=0;j<grids;j++)
         {
         if (outs[i*grids+j]==outs[j]) continue;
         zmabend("warp grid not rectangular");
         }
   for (i=0;i<gridl;i++)
      for (j=1;j<grids;j++)
         {
         if (outl[i*grids+j]==outl[i*grids]) continue;
         zmabend("warp grid not rectangular");
         }
   if ((double)newsl<(outl[0]-1)||(double)(newsl+newnl)>(outl[nrec-1]+1)||
      (double)newss<(outs[0]-1)||(double)(newss+newns)>(outs[nrec-1]+1))
	 zmabend("warp grid does not cover output");
   ldel = (outl[nrec-1]-outl[0])/(double)(gridl-1);
   sdel = (outs[nrec-1]-outs[0])/(double)(grids-1);
   for (i=1;i<gridl;i++)
      if (fabs(outl[i*grids]-outl[(i-1)*grids]-ldel)>0.002)
         zmabend("warp grid not evenly spaced vertically");
   for (i=1;i<grids;i++)
      {
      if (fabs(outs[i]-outs[i-1]-sdel)>0.002)
         zmabend("warp grid not evenly spaced horizontally");
      }
   printf("Warp grid OK: nah = %d nav = %d\n",grids-1,gridl-1);
   nahm1 = grids-2;
   navm1 = gridl-2;
   
   jtop = newss+newns;
   get_extrema(newsl,newss,newnl,newns,nahm1,navm1,grids,ldel,sdel,
      outl,outs,inl,ins,jtop,&lmin,&lmax,&smin,&smax);
   
   /* read into buf, allow extra space for cubic spline */

   sl = MAX((int)lmin-3,0);
   ss = MAX((int)smin-3,0);
   nl = MIN((int)lmax+3,lnl)-sl;
   ns = MIN((int)smax+3,lns)-ss;
   nlp6 = nl+6; nsp6 = ns+6;
     
   mz_alloc1((unsigned char **)&outbuf,2*newns+200,1); /*extra needed for strips*/
   if (nl<=0||ns<=0)
      {
      printf("input range: (sl,ss,nl,ns)=(%d,%d,%d,%d)\n",sl,ss,nl,ns);
      printf("writing all 0 image\n");
      for (i=0;i<2*newns;i++) outbuf[i] = (unsigned char)0;
      for (i=0;i<newnl;i++)
         {
         zvwrit(o_unit,outbuf,"LINE",i+1,
               "SAMP",1,"NSAMPS", newns, NULL);
         }
      goto fin;
      }
   lbufsiz = (long int)nlp6*(long int)nsp6*(long int)writepix;
   if (lbufsiz<(long int)1) zmabend("anomalous value for input area");
   if (lbufsiz>(long int)vmemsize)
      {
      stripn = MIN((int)(1.5*sqrt((double)lbufsiz/(double)vmemsize)+1.0),21);
      printf("requested memory %ld, processing %d strips\n",lbufsiz,stripn);
      strcpy(tstr,"x");
      sinc = newns/stripn+1;
      snewnswrite = sinc;
      outcount = 1;
      for (istrip=0;istrip<stripn;istrip++)
         {
	   printf("strip %d/%d\n", istrip, stripn);
         strcpy(tmpfilex,tmpfile);
         tstr[0] = (char)(istrip+97);
         strcat(tmpfilex,tstr);
         switch (istrip)
            {
            case 0: tf0 = fopen(tmpfilex,"w"); fileptr = tf0; break;
            case 1: tf1 = fopen(tmpfilex,"w"); fileptr = tf1; break;
            case 2: tf2 = fopen(tmpfilex,"w"); fileptr = tf2; break;
            case 3: tf3 = fopen(tmpfilex,"w"); fileptr = tf3; break;
            case 4: tf4 = fopen(tmpfilex,"w"); fileptr = tf4; break;
            case 5: tf5 = fopen(tmpfilex,"w"); fileptr = tf5; break;
            case 6: tf6 = fopen(tmpfilex,"w"); fileptr = tf6; break;
            case 7: tf7 = fopen(tmpfilex,"w"); fileptr = tf7; break;
            case 8: tf8 = fopen(tmpfilex,"w"); fileptr = tf8; break;
            case 9: tf9 = fopen(tmpfilex,"w"); fileptr = tf9; break;
            case 10: tf10 = fopen(tmpfilex,"w"); fileptr = tf10; break;
            case 11: tf11 = fopen(tmpfilex,"w"); fileptr = tf11; break;
            case 12: tf12 = fopen(tmpfilex,"w"); fileptr = tf12; break;
            case 13: tf13 = fopen(tmpfilex,"w"); fileptr = tf13; break;
            case 14: tf14 = fopen(tmpfilex,"w"); fileptr = tf14; break;
            case 15: tf15 = fopen(tmpfilex,"w"); fileptr = tf15; break;
            case 16: tf16 = fopen(tmpfilex,"w"); fileptr = tf16; break;
            case 17: tf17 = fopen(tmpfilex,"w"); fileptr = tf17; break;
            case 18: tf18 = fopen(tmpfilex,"w"); fileptr = tf18; break;
            case 19: tf19 = fopen(tmpfilex,"w"); fileptr = tf19; break;
            case 20: tf20 = fopen(tmpfilex,"w"); fileptr = tf20; break;
            }
         snewss = newss+istrip*sinc;
         snewns = sinc;
         if ((snewss+snewns)>(newss+newns)) snewns = newss+newns-snewss;
         
         linc = newnl/stripn+1;
         for (jstrip=0;jstrip<stripn;jstrip++)
            {
            snewsl = newsl+jstrip*linc;
            snewnl = linc;
            if ((snewsl+snewnl)>(newsl+newnl)) snewnl = newsl+newnl-snewsl;

            jtop = snewss+snewns;
            get_extrema(snewsl,snewss,snewnl,snewns,nahm1,navm1,grids,ldel,sdel,
               outl,outs,inl,ins,jtop,&lmin,&lmax,&smin,&smax);
            sl = MAX((int)lmin-3,0);
            ss = MAX((int)smin-3,0);
            nl = MIN((int)lmax+3,lnl)-sl;
            ns = MIN((int)smax+3,lns)-ss;
            nlp6 = nl+6; nsp6 = ns+6;
            
            if (nl<=0 || ns<=0)
               {
               /* this little segment all 0 -- uncover write for dev case*/
               /*printf("input range: (sl,ss,nl,ns)=(%d,%d,%d,%d)\n",sl,ss,nl,ns);
               printf("writing all 0 piece\n");*/
               for (j=0;j<2*snewnswrite;j++) outbuf[j] = (unsigned char)0;
               for (j=0;j<snewnl;j++)
                  fwrite(outbuf,writepix,snewnswrite,fileptr);
               }
            else process_block(sl,ss,nl,ns,nlp6,nsp6,snewsl,snewss,
               snewnl,snewns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
               znoin,ldel,sdel,istrip,fileptr,snewnswrite,filetype);
            }
         rewind(fileptr);
         fclose(fileptr);
         }
      /* write strips to vicar file */
      for (istrip=0;istrip<stripn;istrip++)
         {
         strcpy(tmpfilex,tmpfile);
         tstr[0] = (char)(istrip+97);
         strcat(tmpfilex,tstr);
         switch (istrip)
            {
            case 0: tf0 = fopen(tmpfilex,"r"); break;
            case 1: tf1 = fopen(tmpfilex,"r"); break;
            case 2: tf2 = fopen(tmpfilex,"r"); break;
            case 3: tf3 = fopen(tmpfilex,"r"); break;
            case 4: tf4 = fopen(tmpfilex,"r"); break;
            case 5: tf5 = fopen(tmpfilex,"r"); break;
            case 6: tf6 = fopen(tmpfilex,"r"); break;
            case 7: tf7 = fopen(tmpfilex,"r"); break;
            case 8: tf8 = fopen(tmpfilex,"r"); break;
            case 9: tf9 = fopen(tmpfilex,"r"); break;
            case 10: tf10 = fopen(tmpfilex,"r"); break;
            case 11: tf11 = fopen(tmpfilex,"r"); break;
            case 12: tf12 = fopen(tmpfilex,"r"); break;
            case 13: tf13 = fopen(tmpfilex,"r"); break;
            case 14: tf14 = fopen(tmpfilex,"r"); break;
            case 15: tf15 = fopen(tmpfilex,"r"); break;
            case 16: tf16 = fopen(tmpfilex,"r"); break;
            case 17: tf17 = fopen(tmpfilex,"r"); break;
            case 18: tf18 = fopen(tmpfilex,"r"); break;
            case 19: tf19 = fopen(tmpfilex,"r"); break;
            case 20: tf20 = fopen(tmpfilex,"r"); break;
            }
         }
      for (iline=0;iline<newnl;iline++)
         {
         bufix = snewnswrite*writepix;
         switch (stripn-1)    /* no breaks in switch cases */
            {
            case 20: fread(&outbuf[20*bufix],writepix,snewnswrite,tf20);
            case 19: fread(&outbuf[19*bufix],writepix,snewnswrite,tf19);
            case 18: fread(&outbuf[18*bufix],writepix,snewnswrite,tf18);
            case 17: fread(&outbuf[17*bufix],writepix,snewnswrite,tf17);
            case 16: fread(&outbuf[16*bufix],writepix,snewnswrite,tf16);
            case 15: fread(&outbuf[15*bufix],writepix,snewnswrite,tf15);
            case 14: fread(&outbuf[14*bufix],writepix,snewnswrite,tf14);
            case 13: fread(&outbuf[13*bufix],writepix,snewnswrite,tf13);
            case 12: fread(&outbuf[12*bufix],writepix,snewnswrite,tf12);
            case 11: fread(&outbuf[11*bufix],writepix,snewnswrite,tf11);
            case 10: fread(&outbuf[10*bufix],writepix,snewnswrite,tf10);
            case 9: fread(&outbuf[9*bufix],writepix,snewnswrite,tf9);
            case 8: fread(&outbuf[8*bufix],writepix,snewnswrite,tf8);
            case 7: fread(&outbuf[7*bufix],writepix,snewnswrite,tf7);
            case 6: fread(&outbuf[6*bufix],writepix,snewnswrite,tf6);
            case 5: fread(&outbuf[5*bufix],writepix,snewnswrite,tf5);
            case 4: fread(&outbuf[4*bufix],writepix,snewnswrite,tf4);
            case 3: fread(&outbuf[3*bufix],writepix,snewnswrite,tf3);
            case 2: fread(&outbuf[2*bufix],writepix,snewnswrite,tf2);
            case 1: fread(&outbuf[bufix],writepix,snewnswrite,tf1);
            case 0: fread(outbuf,writepix,snewnswrite,tf0);
            }
         zvwrit(o_unit,outbuf,"LINE",outcount,
                  "SAMP",1,"NSAMPS", newns, NULL);
         outcount++;
         }
      }
   else
      {
      printf("requested memory %d\n",(int)lbufsiz);
      process_block(sl,ss,nl,ns,nlp6,nsp6,newsl,newss,
         newnl,newns,outl,outs,inl,ins,jtop,nahm1,navm1,grids,interp,
         znoin,ldel,sdel,-1,0,newns,filetype);
      }
   
   /* update the geotiff label, gtreplab reopens for update */
   /* the tout[] solutions are in GeoTIFF coordinates, similar
   to gtsize.  Here only deal with a shift. */
   
fin:
   zvclose(o_unit, NULL);
   if (parmcnt>2)
      {
      b = (double)sline;
      d = (double)ssamp;
      
      bcor = t[0]*b+t[1]*d+t[2];
      dcor = t[3]*b+t[4]*d+t[5];
      p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
      if (p!=0) voff = 1.0; else voff = 0.5;
   
      toutinv[0] = tinv[3];
      toutinv[1] = tinv[4];
      toutinv[2] = 1.0-voff-toutinv[0]*bcor-toutinv[1]*dcor;
      toutinv[3] = tinv[0];
      toutinv[4] = tinv[1];
      toutinv[5] = 1.0-voff-toutinv[3]*bcor-toutinv[4]*dcor;
      
      scale2 = -t[3];
      scale1 = t[1];
      
      invertmap(toutinv,tout);
      scalefmt(scalestr,scale1,scale2);
      trnsfmt(transstr,tout);
      gtreplab("OUT",1,labelstr,0,1,toutinv,scalestr,transstr);
      zvclose(o_unit, NULL);
      }
   
   zvclose(i_unit, NULL);
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create geomv.imake
/***********************************************************************

                     IMAKE FILE FOR PROGRAM geomv

   To Create the build file give the command:

		$ vimake geomv			(VMS)
   or
		% vimake geomv			(Unix)


************************************************************************/


#define PROGRAM	geomv

#define MODULE_LIST geomv.c 

#define MAIN_LANG_C
#define R2LIB
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/*#define LIB_LOCAL	/* remove on delivery */
/*#define DEBUG		/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$PDF_File:
$ create geomv.pdf
process help=*
 !
 PARM INP     TYPE=STRING COUNT=1:3
 PARM OUT     TYPE=STRING COUNT=1
 PARM COLS    TYPE=INTEGER COUNT=4 DEFAULT=(1,2,3,4)
 PARM SIZE    TYPE=INTEGER COUNT=0:4 DEFAULT=(1,1,0,0)
 PARM SL      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM SS      TYPE=INTEGER COUNT=0:1 DEFAULT=1
 PARM NL      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM NS      TYPE=INTEGER COUNT=0:1 DEFAULT=0
 PARM ZNOIN   KEYWORD COUNT=0:1 VALID=ZNOIN DEFAULT="ZNOIN"
 PARM INTERP  TYPE=KEYWORD COUNT=0:1 +
      VALID=(NOIN,BILIN,CUBCONV,CUBSPLIN) DEFAULT=BILIN
 
 PARM NAH     TYPE=INTEGER,VALID=(1:5000) default=1
 PARM NAV     TYPE=INTEGER,VALID=(1:5000) default=1
 PARM TIEPOINT TYPE=REAL,COUNT=0:600 default=--
 PARM PARMS   TYPE=STRING  COUNT=0:1  DEFAULT=--
 PARM VMEMSIZE TYPE=INTEGER COUNT=0:1 DEFAULT=300000000
 PARM TMPFILE TYPE=STRING COUNT=0:1 DEFAULT="xxxgeomvtmp87"
 !
 END-PROC
!
! HELP TEXT FOR GEOMV
.TITLE
GEOMV - Program for high resolution geometric transformations on images.
.HELP
PURPOSE
     GEOMV is a VICAR applications program which makes geometric
     changes in pictures.  It can be used for many purposes including
     correcting geometric distortion, increasing picture size, reducing 
     picture size, and rotating a picture.  The motive for creating
     GEOMV was to use approximately 1000 x 1000 geometric transformation
     grids to allow modelling terrain, etc.  The following are the
     principal differences with respect to the programs LGEOM and
     GEOMV:

	1. No limits on size of input/output pictures in GEOMV.
	   For really large images the program automatically goes
           into a strip processing mode, see parameters VMEMSIZE
           and TMPFILE.
	2. No limits on size of interpolation grid in GEOMV
	   except due to virtual memory limits.  There is about
	   a thousandfold increase in capacity vs the older geom
	   programs.  The large grids can only be transferred by
	   means of IBIS files.  TIECONV has been designed as a
	   companion program to handle large numbers of irregular
	   tiepoints as input and large grids as output.  A 4000
	   by 4000 image was transformed by a 500 x 500 grid
	   in 99 seconds on a SPARCstation 20.
	3. GEOMV uses double precision throughout.  There are 
	   frequent differences in LGEOM and GEOMV results for
	   cells larger than 150 pixels.  It appears that GEOMV is
	   much more accurate.  Making LGEOM more accurate would
	   probably make it slower than GEOMV.
	4. Preparation of "sharp edges" for mosaicking is
           available using ZNOIN (GEOMV) and NOIZ (LGEOM).  The
           two programs may do it differently, if so there should 
           probably be a standardization of this operation.  In
           GEOMV, any 0 input to the bilinear interpolation (the
           four neighbors) causes nearest neighbor interpolation.
	5. GEOMV is a little slower than LGEOM (249 seconds for an 
	   8000 x 8000 image vs 221 seconds for LGEOM).
	6. GEOMV has no bad cases.  The old restriction of less
	   than 85 degree rotation in MGEOM is gone.  Reading of
	   data for grid cells one at a time is gone.
	7. GEOMV is written entirely in C and uses dynamic allocation
	   for all data dependent arrays.
	8. The parameters NAH and NAV are ignored but are kept for
	   compatibility with the profusion of data sets.  The actual
	   values are calculated from the grid while the grid is
	   checked for rectangularity.
	9. GEOMV precalculates from the grid what part of the input
	   is needed.  For example, if you GEOMV a tiny portion of
	   Australia, then the program will only read in the tiny
	   portion of huge input that is needed.  This precalculation
	   is not affected by the fact that the grid might cover a
	   much larger output.  The result is not only speedup of
	   smaller cases but it also allows truly huge GEOM's to
	   be calculated in the strip mode.
       10. Grid spacing requirements differ from LGEOM.  The output
           points do not have to be integral, but the spacing has
           to be uniform in each direction.  This is easy to meet
           for mathematically generated grids, or for grids that
           come from routine TIECONV.  Incidentally, LGEOM does not
           presently check for non-integral output tiepoints, but
           seems to calculate an erroneous output.
       11. Automatic GeoTIFF labelling is supported.
	   
CALL
     geomv (INPUT,GRID) OUTPUT SIZE '(QUALIFIERS) TIEPOINT-GRID
  WHERE:
     INPUT          is the input data set.
     GRID           is an IBIS file containing a warp grid.
     OUTPUT         is the output data set.
     SIZE           is a VICAR size field for the output file
     QUALIFIERS     consist of any of the following keywords:
          NOIN          no interpolation is to be done.
          ZNOIN         no interpolation for points with 0 DN.
     TIEPOINT-GRID      is an alternative form for the warp grid.

     The two forms of the warp grid will now be described.

     The IBIS-file form of the grid contains four columns of
single or double precision tiepoint records.  The columns in
order are (newline,newsamp,oldline,oldsamp); or the column order
can be user specified.  NAH and NAV will be calculated from the
grid and the grid must be rectangular.

     The TIEPOINT-GRID is a set of points describing the relation
of the output file to that of the input file using the keyword
TIEPOINT and optionally NAH and NAV which are ignored.
   NAH=nah  NAV=nav
     tiepoint=(nl1,ns1,ol1,os1,nl2,ns2,ol2,os2,...
                   ...nlk,nsk,olk,osk)

where the grid is rectangular in the output image space, nah is
the number of grid cells horizontally (across the top in the sample
direction), nav is the number of grid cells vertically in the output
image space, the point pairs (nli,nsi,oli,osi) are line-sample
coordinates in the output (new) and input (old) spaces respectively.
the number of pairs k must equal (nah+1)*(nav+1).  The grid must be
perfectly rectangular in the output image space (the rows and
columns must be perfectly horizontal and vertical respectively).
Each direction must be evenly spaced as well, but all values can
br fractional.  The keywords NAH and NAV are ignored and the true
values are calculated from the grid.

The input image may either be byte or halfword data.  The data format is taken
from the VICAR label of the input file.  The output image has the same data 
format (byte or halfword) as the input image.  

Truly large cases (I envision 100 GB) are done by computing a strip size
that depends on the parameter VMEMSIZE.  VMEMSIZE is presently defaulted to
300 MB under the assumption that a 2 GB memory can handle that pretty easily
without too much paging.  But it can be reset larger or smaller as needed and
should be defaulted larger in the future (say when 10 GB memories are common).
Then, if the warp area of the input image is larger than VMEMSIZE, the program
shifts into a "strip processing" mode using the filename given by parameter
TMPFILE followed by a sequence of digits to create one file for each strip.
Then all of the strips are concatenated to give the final output.  Each piece
of each strip is designed to fit within the user provided or defaulted virtual
memory size.  Do not use a link for tmpfile.  Instead, you can use a path/filename
for the TMPFILE.  This is because multiple files will be created with TMPFILE as a
root.  You must delete the TMPFILE files after the run.
  
OPERATION

GEOMV calculates what part of the input is needed for the output, and
reads that entire amount into (virtual) memory.  Images up to 8000
square can be handled easily by the current (1999) generation of
workstations.  Larger images can be handled by sectioning of the output
into a set of tiles, warping them, and then mosaicking the tiles.
The tiles are guaranteed to butt together perfectly.

Unlike MGEOM, GEOMV does not tile the input for the warp grid cells.
It holds all of the cells for a cell-row of output in memory and holds
all of the input image in memory.  Thus there is little penalty for
having a vast number of grid cells.

PERFORMANCE

A 4000 by 4000 image was transformed by a 500 x 500 grid
in 99 seconds on a SPARCstation 20.  Reducing the grid to 30 x 30
cut the time to 39 seconds.  This shows that the use of a large 
grid doesn't penalize the time too much.

.PAGE
Restrictions
------------

The output grid must cover the output image.  The program gives an
error message and stops if it doesn't.  The output grid can be larger.
There are no restrictions on the input grid. 

THE OUTPUT GRID MUST ALSO BE UNIFORMLY SPACED IN EACH DIRECTION
(MORE RESTRICTIVE THAN LGEOM).  THE SPACING VERTICALLY DOES
NOT HAVE TO EQUAL THE SPACING HORIZONTALLY THOUGH.
ON THE OTHER HAND, THE OUTPUT GRID VALUES CAN BE FRACTIONAL (LGEOM
REQUIRES WHOLE NUMBERS).  THESE REQUIREMENTS ARE EASY TO MEET IF THE
GRID IS GENERATED MATHEMATICALLY OR IF A PROGRAM SUCH AS TIECONV IS
USED.

.PAGE
Original Programmer: A. L. Zobrist, 17 Jul. 1999
Current Cognizant Programmer: A. L. Zobrist
25 April 2007 Change to 2 dimensional arrays: A. L. Zobrist
25 April 2007 Do strips if memory too small, handles huge cases: A. L. Zobrist
25 April 2007 Add cubic convolution and cubic spline: A. L. Zobrist
Fri Jan 11 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  
Tue Nov 11 2008 wlb added printf to keep browser from timing out

.LEVEL1
.VARI INP
Input file name, second file
Optional grid (IBIS format)
,third file GeoTIFF reference
.VARI OUT
Output file name
.VARI SIZE
Standard VICAR Size Field
.VARI SL
Starting line for output
.VARI SS
Starting sample for output
.VARI NL
Number of lines for output
* See restrictions
.VARI NS
Number of samples for output
* See restrictions
.VARI INTERP
interpolation options
Valid: NOIN,ZNOIN,BILIN,CUBCONV,CUBSPLIN
.VARI FORMAT
FORMAT is ignored.
.VARI NAH
ignored, will calculate
from grid
.VARI NAV
ignored, will calculate
from grid
.VARIABLE COLS
Columns to use from
optional IBIS file.
.VARI TIEPOINT
grid corner tiepoints in
rows NL1,NS1,OL1,OS1,...
.VARI PARMS
previously saved parameter
dataset
.VARI VMEMSIZE
max allocation for input
image, larger case goes
to "strip process" mode
.VARI TMPFILE
temp filename root for
"strip process" mode
.LEVEL2
.VARI INP
Input file name.  This parameter is input as:
     INP=innam
where "innam" is the input file name.

The second file, if given, is an IBIS file containing a trans-
formation grid in four columns specified by the COLS parameter.
This allows large grids, say 500 x 500 or 1000 x 1000.

The third file, if given, is a VICAR image containing a GeoTIFF
label.  In this case, the output image is assumed to have the
same coordinates as the GeoTIFF labelled image except for the
translation produced by the (sl,ss) coordinates.  This translation
is put into the GeoTIFF coordinate information and the GeoTIFF
label is added to the output.  For an illustration of Automatic
GeoTIFF labelling, see the two PDF's included in the geomv.com
file named gtwarp.pdf and tstgtwarp.pdf.

.VARI OUT
Output and intermediate file names. This parameter is input as:
     OUT=outnam
where:
"outnam" is the output file name, and

.VARI SIZE
The size field is specified with four arguments,
     SIZE=(a,b,c,d)
where:
a is the starting line number of the output picture.
b is the starting sample of the output picture.
c is the number of lines, and
d is the number of samples
For example, SIZE=(1,1,40,50)
would create an output picture of size 40 lines by 50 bytes.
The size field can be thought of as a window relative to the output
grid.  The first two values offset the window down and to the right
causing the features in the image to move up and to the left.
.VARI SL
SL can be used to specify the starting line of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SL - 1.)  The default for SL is 1.
.VARI SS
SS can be used to specify the starting sample of the output picture.
This is actually a coordinate relative to the output grid, therefore,
it offsets the output picture by (SS - 1.)  The default for SS is 1.
.VARI NL
NL can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of lines for output.
* See restrictions for more information
.VARI NS
NS can be used in conjunction with NS in place of the SIZE
parameter to specify the size of the output picture.  It simply
represents the number of bytes for output.
* See restrictions for more information
.VARI INTERP
This parameter has four valid keyword values: NOIN, BILIN,
CUBCONV, and CUBSPLIN

NOIN means no interpolation.   The default method (used when neither
keyword is specified) for computing the
DN values of the output picture is to use a bi-linear interpolation
on the four nearest neighbors in the input picture.  With NOIN, the
value of the nearest point is simply used.
For example, say a point in the output picture was determined
to have come from point (R,P) in the input picture.  Since R and P
are real values, we must somehow calculate a DN value for that
point.  Take IR and IP as the truncated values.  We then have
          VAL1                                 VAL2
           *                                    *
         (IR,IP)                              (IR,IP+1)
                     POINT
                       *
                     (R,P)
          VAL3                                 VAL4
           *                                    *
         (IR+1,IP)                           (IR+1,IP+1)
Here, POINT is the result of a bilinear interpolation using
VAL1, VAL2, VAL3, and VAL4.
If NOIN is specified, then POINT would be VAL1, the nearest
neighbor.

CUBCONV:  (reference Goshtasby on the web)
CUBSPLIN: (reference Goshtasby on the web)

ZNOIN specifies that an interpolation is done except
when one or more of the points used has a value equal to zero. 
In that case the nearest neighbor method is used.
This allows preparation of sharp edges (no interpolation rolloff)
for mosaicking.

.VARI FORMAT
The format is obtained from the input image label. 
.VARI NAH
the nah is number of grid cells horizontally, the number of tiepoints 
across is one larger (nah+1).
.VARI NAV
the nav is number of grid cells vertically, the number of tiepoints
vertically is one larger (nav+1).
.VARIABLE COLS
    COLS=(C1,C2,C3,C4)   Columns in the IBIS tabular file that
			 contain the tiepoints.  C1 has new line,
			 C2 has new sample, C3 has old line, and
			 C4 has old sample.  This parameter is used
			 only if the IBIS file input is given.  The
			 defaults are (1,2,3,4) and they are automat-
			 ically given if a TIECONV type program is 
			 used.

.VARI TIEPOINT
There are four real numbers for each tiepoint , the first two are the
line-sample coordinate in the output, the second two are the line-sample
coordinate in the input which is mapped to the point in the output.  There
must be (nah+1)*(nav+1) tiepoints (quadruple)s aligned in a perfectly
horizontal and vertical grid.  THE OUTPUT GRID MUST ALSO BE UNIFORMLY
SPACED IN EACH DIRECTION.  THE SPACING VERTICALLY DOES NOT HAVE TO
EQUAL THE SPACING HORIZONTALLY THOUGH (MORE RESTRICTIVE THAN LGEOM).
ON THE OTHER HAND, THE OUTPUT GRID VALUES CAN BE FRACTIONAL (LGEOM
REQUIRES WHOLE NUMBERS).  THESE REQUIREMENTS ARE EASY TO MEET IF THE
GRID IS GENERATED MATHEMATICALLY OR IF A PROGRAM SUCH AS TIECONV IS USED.


.VARI PARMS
A parameter data set containing the geom parameters.  This file should
have been written by a program which uses the XVP routines for writing
parameter data sets.  This is the most common means by which the parameters
NAH, NAV, and TIEPOINT are passed.
.VARI TMPFILE
If the warp area of the input image is larger than VMEMSIZE, the program
shifts into a "strip processing" mode using the filename given by parameter
TMPFILE followed by a sequence of digits to create one file for each strip.
Then all of the strips are concatenated to give the final output.  Each piece
of each strip is designed to fit within the user provided or defaulted virtual
memory size.  Do not use a link for TMPFILE.  Instead, you can use a path/filename
for the TMPFILE.  This is because multiple files will be created with TMPFILE as a
root.  You must delete the TMPFILE files after the run.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstgeomv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"


!   TEST SCRIPT FOR GEOMV


! Now generate BYTE input data set
 
gen mgtest 10 10 SINC=40 LINC=40
 
! Verify existence of input file
list mgtest

!  Try some copies.
!  Check case of grid bigger than image.
geomv mgtest mgtest1 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (mgtest1 mgtest)

 
! Perform simple enlargement to 2X size
geomv mgtest mgenlarg + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)

! Print it out
list mgenlarg

 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Print it out
list mgrotat


!   SUBSET OF ABOVE CASE, THIS WAS AN ERROR CASE UNTIL FIXED 05/00

gen xxxim1 10 10 SINC=40 LINC=40

! Perform 45 degree rotation clockwise with 1.4 times enlargement

GEOMV xxxim1 xxxim3 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
 
list xxxim3 'zer





! Perform test of size field handling 
geomv mgtest mgrotat1 + 
   SIZE=(1,1,20,10)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
             20.,1.,15.,5.,20.,20.,5.,15.)

geomv mgtest mgrotat2 + 
   SIZE=(1,11,20,1)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

geomv mgtest mgrotat3 + 
   SIZE=(1,12,20,9)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Concatenate the three images.  
mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA 
difpic (mgrotatA,mgrotat) 


! Perform the same operation, but without interpolation
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
            20.,1.,15.,5.,20.,20.,5.,15.)+
   INTERP=NOIN

! Print it out
list mgrotat


! Perform the same operation, but without interpolation
gen mgtest 10 10 SINC=64 LINC=64
list mgtest
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)+
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)+
   'ZNOIN

! Print it out
list mgrotat
 
! Now generate BYTE input data set
 
gen mgtest2 1000 1000 SINC=1 LINC=1
 
! Verify existence of input file
list mgtest2 SIZE=(1,1,15,15)
 
! DO LONG THIN CASE WITH 45 DEG ROTATION.

geomv mgtest2 mgenthin + 
   SIZE=(1,1,2,1000)+
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
            2.,1.,1001.,2.,2.,1000.,2.,1001.)

! Print it out
list mgenthin 'NOEJECT
 

! Now do simple tests for half
! Now generate HALF input data set
 
gen mgtest 10 10 SINC=40 LINC=40 'HALF
 
! Verify existence of input file
list mgtest
 

!  Try some copies.
!  Check case of grid bigger than image.
geomv mgtest mgtest1 + 
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
            20.,1.,20.,1., 20.,20.,20.,20.)
difpic (mgtest1 mgtest)


! Perform simple enlargement to 2X size
geomv mgtest mgenlarg + 
   SIZE=(1,1,20,20)   +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
list mgenlarg
 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
geomv mgtest mgrotat + 
   SIZE=(1,1,20,20)  +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

! Print it out
list mgrotat


! big case with offsets
gen mgtest2 1000 1000 SINC=3 LINC=7

geomv mgtest2 mgenthin + 
   SIZE=(1,1,1000,1000)+
   TIEPOINT=(1.,1.,7.,7.,1.,1000.,1.,1022.,+
            1000.,1.,970.,22.,1000.,1000.,1050.,1060.)

list mgenthin linc=199 sinc=199

! ibis file grid input


! small grid small image testing ibis file pass to geomv

gen mgtest2 400 400 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=400.,MAXS=400. +
    'GEOMV

geomv INP=(mgtest2,b) OUT=mgenlarg + 
   SIZE=(1,1,400,400)

list mgenlarg linc=39 sinc=39

! parms file use by GEOMV... have to use the MGEOM version to
! get a parms file since GEOMV keyword to tieconv produces an IBIS
! file in the output, the LGEOM version produces an unevenly spaced
! grid

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")

mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=6,MINL=1.,MINS=1.,MAXL=10.,MAXS=10. +
    'MGEOM

geomv INP=mgtest OUT=mgenlarg PARMS=b + 
   SIZE=(1,1,10,10)
list mgenlarg


! ibis file grid input large grid and large image

gen mgtest2 1000 1000 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*10000000","c2=mod(c2,0.0001)*10000000")
mf a func=("c3=c1*1.1","c4=c2*1.1")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=200,NAV=200,MINL=1.,MINS=1.,MAXL=1000.,MAXS=1000. +
    'GEOMV

geomv INP=(mgtest2,b) OUT=mgenlarg + 
   SIZE=(1,1,1000,1000)

list mgenlarg linc=199 sinc=199

theend>
end-proc
$!-----------------------------------------------------------------------------
$ create devgeomv.pdf
procedure
refgbl $echo
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $echo="yes"


!!!!!!!!!!!! THESE ARE DEVELOPER TESTS, COMPARING WITH LGEOM, FOR 
!!!!!!!!!!!! INSTANCE

goto curr


!   TEST SCRIPT FOR GEOMV
!
!
! Now generate BYTE input data set
! 
!gen mgtest 10 10 SINC=40 LINC=40
! 
! Verify existence of input file
!list mgtest
!
!  Try some copies.
!  Check case of grid bigger than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
!            20.,1.,20.,1., 20.,20.,20.,20.)
!difpic (mgtest1 mgtest)

!  Check case of grid smaller than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
!             2.,1.,2.,1.,   2.,2.,2.,2.)
!difpic (mgtest1 mgtest)

! 
! Perform simple enlargement to 2X size
!geomv mgtest mgenlarg NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
!mgeom mgtest mgenlarg2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
!list mgenlarg ss=17 nl=3
!list mgenlarg2 ss=17 nl=3
!list mgenlarg2 nl=3
! also diff it
!difpic (mgenlarg mgenlarg2) 
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
!geomv mgtest mgrotat + 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3

! also diff it
!difpic (mgrotat mgrotat2)

! Perform test of size field handling 
!geomv mgtest mgrotat1 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,10)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

!geomv mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,11,20,1)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

!geomv mgtest mgrotat3 NAH=1 NAV=1+ 
!   SIZE=(1,12,20,9)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)

! Concatenate the three images.  
!mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA 
!difpic (mgrotatA,mgrotat) 

!
! Perform the same operation, but without interpolation
! (NOTE BY ALZ: ZNOIN WAS IN THE GEOMA CASE, NOT THE SAME AS NOIN)
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTERP=NOIN
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTERP=NOIN
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3
!difpic (mgrotat,mgrotat2)
!
! Perform the same operation, but without interpolation
!gen mgtest 10 10 SINC=64 LINC=64
!list mgtest
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   'ZNOIN
!lgeom mgtest mgrotat2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)+
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)+
!   INTRPZ=NOIZ
! Print it out
!list mgrotat
!list mgrotat2
!f2 inp=(mgrotat,mgrotat2) out=mgrotat3 func="abs(in1-in2)"
!list mgrotat3
!difpic (mgrotat,mgrotat2)
! 
! Now generate BYTE input data set
! 
!gen mgtest2 1000 1000 SINC=1 LINC=1
! 
! Verify existence of input file
!list mgtest2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000)+
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000)+
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!            2.,1.,1001.,2.,2.,1000.,2.,1001.)
!datetime
! Print it out
!list mgenthin 'NOEJECT nl=2 ns=2
!list mgenthin 'NOEJECT nl=2 ns=2 ss=999
!list mgenthin2 'NOEJECT nl=2 ns=2
!list mgenthin2 'NOEJECT nl=2 ns=2 ss=999
!difpic (mgenthin,mgenthin2)
!f2 inp=(mgenthin,mgenthin2) out=mgenthin3 func="abs(in1-in2)"
!list mgenthin3 'NOEJECT
! 
!gen a 1000 1000
!  Test bilinear interpolation for FR 87169
!geomv							+
!inp=a					+
!out=b					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!70,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!list b (50,50,850,850) linc=100 sinc=100
!geoma							+
!inp=a					+
!out=b2					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!70,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!difpic (b,b2)
!
! Now do simple tests for half
! Now generate HALF input data set
! 
!gen mgtest 10 10 SINC=40 LINC=40 'HALF
! 
! Verify existence of input file
!list mgtest
! 
!
!  Try some copies.
!  Check case of grid bigger than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20.,+
!            20.,1.,20.,1., 20.,20.,20.,20.)
!difpic (mgtest1 mgtest)

!  Check case of grid smaller than image.
!geomv mgtest mgtest1 NAH=1 NAV=1+ 
!   TIEPOINT=(1.,1.,1.,1.,   1.,2.,1.,2.,+
!             2.,1.,2.,1.,   2.,2.,2.,2.)
!difpic (mgtest1 mgtest)

! Perform simple enlargement to 2X size
!geomv mgtest mgenlarg NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)   +
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
!lgeom mgtest mgenlarg2 NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)   +
!   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10.,+
!                20.,1.,10.,1.,20.,20.,10.,10.)
 
! Print it out
!list mgenlarg
!difpic (mgenlarg,mgenlarg2)
! 
! Perform 45 degree rotation clockwise with 1.4 times enlargement
!geomv mgtest mgrotat NAH=1 NAV=1+ 
!   SIZE=(1,1,20,20)  +
!   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
!                20.,1.,15.,5.,20.,20.,5.,15.)
!
! Print it out
!list mgrotat
! 
! Now generate HALF input data set
! 
!gen mgtest2 1000 1000 SINC=1 LINC=1 'HALF
! 
! Verify existence of input file
!list mgtest2 SIZE=(1,1,15,15)
! 
! DO LONG THIN CASE WITH 45 DEG ROTATION.
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000) +
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!           2.,1.,1001.,2.,2.,1000.,2.,1001.)
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,2,1000) +
!   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000.,+
!           2.,1.,1001.,2.,2.,1000.,2.,1001.)
!difpic (mgenthin,mgenthin2)           
!
! Print it out
!list mgenthin 'NOEJECT
!
!gen a 1000 1000 'half
!  Test bilinear interpolation for FR 87169
!geomv							+
!inp=a					+
!out=b					+
!nl=900 ns=900						+
!nav=9							+
!nah=9							+
!tiepoint=(						+
!001,001,900,900,001,100,800,800,001,200,300,300,001,300,300,300,+
!001,400,400,400,001,500,500,500,001,600,600,600,001,700,700,700,+
!001,800,001,800,001,900,001,900,100,001,100,001,+
!100,100,100,100,100,200,600,200,100,300,700,300,100,400,100,400,+
!100,500,500,500,100,600,600,600,100,700,700,700,100,800,800,800,+
!100,900,100,900,		+
!200,001,200,001,200,100,200,100,200,200,200,200,200,300,200,300,+
!200,400,400,400,200,500,500,500,200,600,600,600,200,700,700,700,+
!200,800,200,800,200,900,200,900,300,001,300,001,+
!300,100,300,100,300,200,300,200,300,300,300,300,300,400,300,400,+
!300,500,300,500,300,600,300,600,300,700,300,700,300,800,300,800,+
!300,900,300,900,		+
!400,001,400,001,400,100,400,100,400,200,400,200,400,300,400,300,+
!400,400,400,400,400,500,500,500,400,600,400,600,400,700,400,700,+
!400,800,400,800,400,900,400,900,500,001,500,001,+
!500,100,500,100,500,200,500,200,500,300,500,300,500,400,500,400,+
!500,500,500,500,500,600,500,600,500,700,500,700,500,800,500,800,+
!500,900,500,900,		+
!600,001,600,001,600,100,600,100,600,200,600,200,600,300,600,300,+
!600,400,600,400,600,500,600,500,600,600,600,600,600,700,600,700,+
!600,800,600,800,600,900,600,900,700,001,700,001,+
!700,100,700,100,700,200,700,200,700,300,700,300,700,400,700,400,+
!700,500,700,500,700,600,700,600,700,700,700,700,700,800,700,800,+
!700,900,700,900,		+
!800,001,800,001,800,100,100,100,800,200,200,200,800,300,300,300,+
!800,400,800,400,800,500,800,500,800,600,800,600,800,700,800,700,+
!800,800,800,800,800,900,800,900,900,001,900,001,+
!900,100,900,100,900,200,900,200,900,300,900,300,900,400,400,400,+
!900,500,900,500,900,600,900,600,900,700,900,700,900,800,800,800,+
!900,900,900,900)
!list b (50,50,850,850) linc=100 sinc=100


! long case with offsets SHOWS INACCURACY OF LGEOM

!gen mgtest2 10 4000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,10,4000)+
!   TIEPOINT=(1.,1.,1.,1.,1.,4000.,1.,3900.,+
!            4000.,1.,4000.,1.,4000.,4000.,4000.,3900.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,10,4000)+
!   TIEPOINT=(1.,1.,1.,1.,1.,4000.,1.,3900.,+
!            4000.,1.,4000.,1.,4000.,4000.,4000.,3900.)
!datetime
!difpic (mgenthin,mgenthin2)
!list mgenthin nl=1 ss=3288 ns=3    !GEOMV IS CORRECT
!list mgenthin2 nl=1 ss=3288 ns=3   !LGEOM IS INCORRECT

! big case with offsets / timing case

!gen mgtest2 4000 4000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+ 
!   SIZE=(1,1,4000,4000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,4000.,1.,4022.,+
!            4000.,1.,3970.,22.,4000.,4000.,4050.,4060.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,4000,4000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,4000.,1.,4022.,+
!            4000.,1.,3970.,22.,4000.,4000.,4050.,4060.)
!datetime
!difpic (mgenthin,mgenthin2)

! ibis file grid input

! really big case with offsets / timing case

!datetime
!gen mgtest2 8000 8000 SINC=3 LINC=7
!datetime
!geomv mgtest2 mgenthin NAH=1 NAV=1+
!   SIZE=(1,1,8000,8000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,8000.,1.,8022.,+
!            8000.,1.,7970.,22.,8000.,8000.,8050.,8060.)
!datetime
!lgeom mgtest2 mgenthin2 NAH=1 NAV=1+ 
!   SIZE=(1,1,8000,8000)+
!   TIEPOINT=(1.,1.,7.,7.,1.,8000.,1.,8022.,+
!            8000.,1.,7970.,22.,8000.,8000.,8050.,8060.)
!datetime
!difpic (mgenthin,mgenthin2)
!datetime

! small grid small image testing ibis file pass to geomv

!gen mgtest2 400 400 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=444
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!!ibis-list a cols=(1,2,3,4) csize=12 'format
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=1,NAV=1,MINL=1.,MINS=1.,MAXL=400.,MAXS=400. +
!    'GEOMV 'NOPR 
!ibis-list b cols=(1,2,3,4) csize=12  'format

!geomv INP=(mgtest2,b) OUT=mgenlarg NAH=3 NAV=3+ 
!   SIZE=(1,1,400,400)

!list mgenlarg linc=37 sinc=37

! parms file use

!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12
! INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'LGEOM 'NOPR

!datetime
!geomv INP=mgtest OUT=mgenlarg PARMS=b + 
!   SIZE=(1,1,20,20)
!list mgenlarg ns=12 nl=20


! ibis file grid input large grid and large image

!gen mgtest2 4000 4000 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=444
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*40000000","c2=mod(c2,0.0001)*40000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!!ibis-list a cols=(1,2,3,4) csize=12 'format
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=4000.,MAXS=4000. +
!    'GEOMV 'NOPR 
!ibis-list b cols=(1,2,3,4) csize=12 nr=10 'format

!datetime
!geomv INP=(mgtest2,b) OUT=mgenlarg NAH=3 NAV=3+ 
!   SIZE=(1,1,4000,4000)
!datetime

!list mgenlarg linc=97 sinc=97

! LARGE CASE COMPARISON WITH LGEOM, BE CAREFUL, LGEOM
! REQUIRES INTEGRAL VALUES IN OUTPUT GRID, CHECK IT OUT

gen mgtest2 3000 3000 SINC=3 LINC=7
ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=444
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
mf a func=("c1=mod(c1,0.0001)*30000000","c2=mod(c2,0.0001)*30000000")
mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
mf a func=("c3=c1*1.1","c4=c2*1.1")
ibis-list a cols=(1,2,3,4) csize=12 NR=10 'format
tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=3001.,MAXS=3001. +
    'GEOMV 'NOPR 
ibis-list b cols=(1,2,3,4) csize=12 nr=40 'format

datetime
geomv INP=(mgtest2,b) OUT=mgenlarg SIZE=(1,1,3000,3000)
datetime

tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=3000.,MAXS=3000. +
    'LGEOM 'NOPR 

datetime
lgeom INP=mgtest2 OUT=mgenlarg2 SIZE=(1,1,3000,3000) PARMS=b2
datetime

difpic (mgenlarg,mgenlarg2)

! small grid small image testing ibis file pass to geomv

!gen mgtest2 400 400 SINC=3 LINC=7
!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
!mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
!mf a func=("c3=c1*1.1","c4=c2*1.1")

!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=421.,MAXS=421. +
!    'GEOMV 'NOPR 
!ibis-list b nr=40 'format

!geomv INP=(mgtest2,b) OUT=mgenlarg + 
!   SIZE=(1,1,400,400)

!list mgenlarg linc=39 sinc=39

! ibis file input single precision case, also test columns
! can't use tieconv since it is always doublew precision

!ibis-gen b version=&version org=&org  datacol=(1,2,3,4,5) +
!    nc=5 nr=961
!mf b func=("c1=int((index-1)/31)*14+1","c2=mod(index+30,31)*14+1")
!mf b func=("c3=c1*1.1","c5=c2*1.1")

!ibis-list b nr=961 'format

!geomv INP=(mgtest2,b) OUT=mgenlarg2 cols=(1,2,3,5)+ 
!   SIZE=(1,1,400,400)

!list mgenlarg2 linc=39 sinc=39

!difpic (mgenlarg,mgenlarg2)



!   THIS CASE SHOWS GEOMV VS LGEOM ACCURACY, THE VALUE AT 18,4
! SHOULD CLEARLY BE 37, BUT LGEOM GETS 36.  IN FACT THE FLOATING
! VALUE CALCULATES TO 37.4, SO IT CAN'T TRUNCATE OR ROUND TO 36.

! FOR THIS CASE, THE GRID IS RECTANGULAR, ALL WHOLE NUMBERS, AND
! EVENLY SPACED

! YOU CAN ALSO USE THIS CASE FOR THE VARIOUS BAD GRID ERROR CHECKS.

! Now generate BYTE input data set
 
!gen mgtest 10 10 SINC=40 LINC=40
!list mgtest

 
! Perform bent enlargement to 2X size
!geomv mgtest mgenlarg + 
!   SIZE=(1,1,21,21)+
!   TIEPOINT=(1.,1.,1.,1.,     1.,11.,1.,2.,      1.,21.,1.,10.,+
!             11.,1.,3.,1.,     11.,11.,4.,2.,      11.,21.,3.,10.,+
!             21.,1.,10.,1.,   21.,11.,10.,2.,    21.,21.,10.,10.)

!lgeom mgtest mgenlarg2 nah=2 nav=2 + 
!   SIZE=(1,1,21,21)+
!   TIEPOINT=(1.,1.,1.,1.,     1.,11.,1.,2.,      1.,21.,1.,10.,+
!             11.,1.,3.,1.,     11.,11.,4.,2.,      11.,21.,3.,10.,+
!             21.,1.,10.,1.,   21.,11.,10.,2.,    21.,21.,10.,10.)

!list mgenlarg ss=1 ns=15

!list mgenlarg2 ss=1 ns=15

gen xxxim1 10 10 SINC=40 LINC=40

! Perform 45 degree rotation clockwise with 1.4 times enlargement

GEOMV xxxim1 xxxim2 sl=7 ss=1 +
         nl=6 ns=6 interp=noin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)


GEOMV xxxim1 xxxim5 sl=7 ss=1 +
         nl=6 ns=6 interp=noin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim3 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
GEOMV xxxim1 xxxim6 sl=7 ss=1 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)
GEOMV xxxim1 xxxim4 sl=7 ss=1 +
         nl=6 ns=6 +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim7 sl=7 ss=1 +
         nl=6 ns=6 +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

difpic (xxxim2,xxxim3)
difpic (xxxim3,xxxim4)
difpic (xxxim2,xxxim5)
difpic (xxxim3,xxxim6)
difpic (xxxim4,xxxim7)

! this case tests the zero piece write, remove write in program to see in action

ibis-copy xxqqgrid2 devgeomvgrid

mf3 devgeomvgrid f="c7=(c7-33284.24)*450+33284.24$c8=(c8-15462.29)*450+15462.29"

geomv +
 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/lcalnorm56.img, +
 devgeomvgrid,tiny)        out=tiny2 sl=1 ss=1   +
 nl=100 ns=100 interp=BILIN cols=(1,2,7,8) tiepoint=

geomv +
 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/lcalnorm56.img, +
 devgeomvgrid,tiny)        out=tiny3 sl=1 ss=1  vmemsize=40000000 +
 nl=100 ns=100 interp=BILIN cols=(1,2,7,8) tiepoint=

difpic (tiny2,tiny3)
xvd tiny3

!curr>

! this is the first huge case

!label-create xxxgeomvtmp87b xxxa nl=10451 ns=10620
!xvd xxxa
!goto theend

!gtwarp /home/alz/astapp/tomcrete/ca-aster-quads/mos1.img +
!   /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb.img +
!   ref=/home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master +
!   'coverinp

geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubspl.img +
 size=(-130710,-196887,6000,6000)  cols=(1,2,7,8) +
 interp=cubsplin vmemsize=800000000

!size=(-150710,-206887,31355,31857)  cols=(1,2,7,8) +
 
!xvd /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb.img

f2 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubbil.img,+
  /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubcnv.img) +
  out=xxxa func="abs(in1-in2+128)"

hist xxxa

f2 inp=(/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubbil.img,+
  /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_cubspl.img) +
  out=xxxb func="abs(in1-in2+128)"

hist xxxb


goto theend

!curr>

! now cubic convolution and cubic spline

gen xxxim1 20 20 SINC=40 LINC=40

GEOMV xxxim1 xxxim2 sl=10 ss=4 +
         nl=6 ns=6 interp=bilin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim3 sl=10 ss=4 +
         nl=6 ns=6 interp=cubsplin +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

GEOMV xxxim1 xxxim4 sl=10 ss=4 +
         nl=6 ns=6 interp=cubconv +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5.,+
                20.,1.,15.,5.,20.,20.,5.,15.)

difpic (xxxim2,xxxim3)
difpic (xxxim2,xxxim4)
difpic (xxxim3,xxxim4)
list xxxim2
list xxxim3
xvd xxxim2
xvd xxxim3


! TIMIMG TEST

!curr>

ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=700000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=600000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=500000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=400000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=300000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=200000000
ush date
geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_time.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=cubconv vmemsize=500000000
ush date

! comparison of data with different strip layout

curr>

ush date
/home/alz/ikapp/geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 /home/alz/ikapp/xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_300.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=300000000
ush date
/home/alz/ikapp/geomv +
 INP=(/home/alz/astapp/tomcrete/ca-aster-quads/mosall.img, +
 /home/alz/ikapp/xxqqgrid2, +
 /home/alz/astapp/tomcrete/ca-aster-quads/ca_lamb_master) +
 OUT=/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_200.img +
 size=(-150710,-226761,63423,62836)  cols=(1,2,7,8) +
 interp=bilin vmemsize=200000000
ush date

difpic (/home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_300.img, +
        /home/alz/astapp/tomcrete/ca-aster-quads/mos1_lamb_200.img)

theend>
end-proc
$!-----------------------------------------------------------------------------
$ create tstgeomv.log_solos
tstgeomv
gen mgtest 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:45 2011
     Samp     1       3       5       7       9
   Line
      1       0  40  80 120 160 200 240  24  64 104
      2      40  80 120 160 200 240  24  64 104 144
      3      80 120 160 200 240  24  64 104 144 184
      4     120 160 200 240  24  64 104 144 184 224
      5     160 200 240  24  64 104 144 184 224   8
      6     200 240  24  64 104 144 184 224   8  48
      7     240  24  64 104 144 184 224   8  48  88
      8      24  64 104 144 184 224   8  48  88 128
      9      64 104 144 184 224   8  48  88 128 168
     10     104 144 184 224   8  48  88 128 168 208
geomv mgtest mgtest1  +
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20., +
            20.,1.,20.,1., 20.,20.,20.,20.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
difpic (mgtest1 mgtest)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
geomv mgtest mgenlarg  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10., +
                20.,1.,10.,1.,20.,20.,10.,10.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenlarg
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:45 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:47 2011
     Samp     1       3       5       7       9      11      13      15      17      19
   Line
      1       0   0  40  57  76  95 114 133 152 171 189 208 227 206 104  28  47  66  85 104
      2       0   0  40  76  95 114 133 152 171 189 208 202 163 123  78  47  66  85 104 123
      3      40  40  80  95 114 133 152 171 189 208 227 195  99  40  52  66  85 104 123 142
      4      57  76  95 114 133 152 171 189 208 199 167 126  75  47  66  85 104 123 142 161
      5      76  95 114 133 152 171 189 208 227 186  96  49  56  66  85 104 123 142 161 180
      6      95 114 133 152 171 189 208 198 172 128  71  47  66  85 104 123 142 161 180 199
      7     114 133 152 171 189 208 227 178  95  58  58  66  85 104 123 142 161 180 199 218
      8     133 152 171 189 208 198 178 129  65  47  66  85 104 123 142 161 180 194 175 156
      9     152 171 189 208 227 172  95  65  59  66  85 104 123 142 161 180 199 207 130  53
     10     171 189 208 199 186 128  58  47  66  85 104 123 142 161 180 192 179 159  89  19
     11     189 208 227 167  96  71  58  66  85 104 123 142 161 180 199 198 127  63  50  37
     12     208 202 195 126  49  47  66  85 104 123 142 161 180 190 184 161  85  19  37  56
     13     227 163  99  75  56  66  85 104 123 142 161 180 199 190 126  72  53  37  56  75
     14     206 123  40  47  66  85 104 123 142 161 180 190 190 162  79  19  37  56  75  94
     15     104  78  52  66  85 104 123 142 161 180 199 184 126  79  53  37  56  75  94 113
     16      28  47  66  85 104 123 142 161 180 192 198 161  72  19  37  56  75  94 113 132
     17      47  66  85 104 123 142 161 180 199 179 127  85  53  37  56  75  94 113 132 151
     18      66  85 104 123 142 161 180 194 207 159  63  19  37  56  75  94 113 132 151 170
     19      85 104 123 142 161 180 199 175 130  89  50  37  56  75  94 113 132 151 170 189
     20     104 123 142 161 180 199 218 156  53  19  37  56  75  94 113 132 151 170 189 208
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:45 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:47 2011
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  40  40   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0  88  88  88  88   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 131 131 131 131 131 131   0   0   0   0   0   0   0
      7       0   0   0   0   0   0 173 173 173 173 173 173 173 173   0   0   0   0   0   0
      8       0   0   0   0   0 210 215 215 215 215 215 215 215 215 210   0   0   0   0   0
      9       0   0   0   0 158 132 154 136 149 142 142 149 136 154 132 158   0   0   0   0
     10       0   0   0  43  58  43  54  43  49  43  43  49  43  54  43  58  43   0   0   0
     11       0   0  85  85  85  85  85  85  85  85  85  85  85  85  85  85  85  85   0   0
     12       0   0 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127 127   0   0
     13       0   0   0 169 169 169 169 169 169 169 169 169 169 169 169 169 169   0   0   0
     14       0   0   0   0 211 186 211 190 203 196 196 203 190 211 186 211   0   0   0   0
     15       0   0   0   0   0  65  93  74  88  82  82  88  74  93  65   0   0   0   0   0
     16       0   0   0   0   0   0  40  40  40  40  40  40  40  40   0   0   0   0   0   0
     17       0   0   0   0   0   0   0  82  82  82  82  82  82   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0 124 124 124 124   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0 166 166   0   0   0   0   0   0   0   0   0
gen xxxim1 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
GEOMV xxxim1 xxxim3 sl=7 ss=1  +
         nl=6 ns=6 interp=bilin  +
         TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task GEOMV
geomv version Tue Nov 11 2008
list xxxim3 'zer
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:47 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:47 2011
     Samp     1       3       5
   Line
      1       0   0   0   0   0   0
      2       0   0   0   0   0 210
      3       0   0   0   0 158 132
      4       0   0   0  43  58  43
      5       0   0  85  85  85  85
      6       0   0 127 127 127 127
geomv mgtest mgrotat1  +
   SIZE=(1,1,20,10) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
             20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
geomv mgtest mgrotat2  +
   SIZE=(1,11,20,1) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
geomv mgtest mgrotat3  +
   SIZE=(1,12,20,9) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
mss (mgrotat1,mgrotat2,mgrotat3) mgrotatA
Beginning VICAR task mss
* OUTPUT CONTAINS   3INTERLEAVED DATA SETS **
* ACTUAL OUTPUT RECORD LENGTH     20SAMPLES **
difpic (mgrotatA,mgrotat)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   0
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
            20.,1.,15.,5.,20.,20.,5.,15.) +
   INTERP=NOIN
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:45 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:48 2011
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  40  40   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0  80  80  80  80   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 120 120 120 120 120 120   0   0   0   0   0   0   0
      7       0   0   0   0   0   0 160 160 160 160 160 160 160 160   0   0   0   0   0   0
      8       0   0   0   0   0 200 240 200 200 200 200 200 200 240 200   0   0   0   0   0
      9       0   0   0   0 240  24 240  24 240 240 240 240  24 240  24 240   0   0   0   0
     10       0   0   0  24  64  24  64  24  64  24  24  64  24  64  24  64  24   0   0   0
     11       0   0  64 104  64 104  64 104  64 104 104  64 104  64 104  64 104  64   0   0
     12       0   0 144 104 144 104 144 104 144 144 144 144 104 144 104 144 104 144   0   0
     13       0   0   0 184 144 184 144 184 184 184 184 184 184 144 184 144 184   0   0   0
     14       0   0   0   0 224 184 224 224 224 224 224 224 224 224 184 224   0   0   0   0
     15       0   0   0   0   0   8   8   8   8   8   8   8   8   8   8   0   0   0   0   0
     16       0   0   0   0   0   0  48  48  48  48  48  48  48  48   0   0   0   0   0   0
     17       0   0   0   0   0   0   0  88  88  88  88  88  88   0   0   0   0   0   0   0
     18       0   0   0   0   0   0   0   0 128 128 128 128   0   0   0   0   0   0   0   0
     19       0   0   0   0   0   0   0   0   0 168 168   0   0   0   0   0   0   0   0   0
gen mgtest 10 10 SINC=64 LINC=64
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:48 2011
     Samp     1       3       5       7       9
   Line
      1       0  64 128 192   0  64 128 192   0  64
      2      64 128 192   0  64 128 192   0  64 128
      3     128 192   0  64 128 192   0  64 128 192
      4     192   0  64 128 192   0  64 128 192   0
      5       0  64 128 192   0  64 128 192   0  64
      6      64 128 192   0  64 128 192   0  64 128
      7     128 192   0  64 128 192   0  64 128 192
      8     192   0  64 128 192   0  64 128 192   0
      9       0  64 128 192   0  64 128 192   0  64
     10      64 128 192   0  64 128 192   0  64 128
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20) +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.) +
   'ZNOIN
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgrotat
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:48 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp     1       3       5       7       9      11      13      15      17      19
   Line

      4       0   0   0   0   0   0   0   0   0  64  64   0   0   0   0   0   0   0   0   0
      5       0   0   0   0   0   0   0   0 141 141 141 141   0   0   0   0   0   0   0   0
      6       0   0   0   0   0   0   0 192 192 192 192 192 192   0   0   0   0   0   0   0

      8       0   0   0   0   0  88 128  88  64  64  64  64  88 128  88   0   0   0   0   0
      9       0   0   0   0 128 155 128 155 155 155 155 155 155 128 155 128   0   0   0   0
     10       0   0   0 192   0 192   0 192   0 192 192   0 192   0 192   0 192   0   0   0
     11       0   0   0  64   0  64   0  64   0  64  64   0  64   0  64   0  64   0   0   0
     12       0   0 128 101 128 101 128 101 101 101 101 101 101 128 101 128 101 128   0   0
     13       0   0   0 168 128 168 128 168 192 192 192 192 168 128 168 128 168   0   0   0
     14       0   0   0   0   0 192   0   0   0   0   0   0   0   0 192   0   0   0   0   0
     15       0   0   0   0   0  64  64  64  64  64  64  64  64  64  64   0   0   0   0   0
     16       0   0   0   0   0   0 115 115 115 115 115 115 115 115   0   0   0   0   0   0
     17       0   0   0   0   0   0   0 192 192 192 192 192 192   0   0   0   0   0   0   0

     19       0   0   0   0   0   0   0   0   0  64  64   0   0   0   0   0   0   0   0   0
gen mgtest2 1000 1000 SINC=1 LINC=1
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest2 SIZE=(1,1,15,15)
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp     1       3       5       7       9      11      13      15
   Line
      1       0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
      2       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
      3       2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
      4       3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
      5       4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
      6       5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
      7       6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
      8       7   8   9  10  11  12  13  14  15  16  17  18  19  20  21
      9       8   9  10  11  12  13  14  15  16  17  18  19  20  21  22
     10       9  10  11  12  13  14  15  16  17  18  19  20  21  22  23
     11      10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
     12      11  12  13  14  15  16  17  18  19  20  21  22  23  24  25
     13      12  13  14  15  16  17  18  19  20  21  22  23  24  25  26
     14      13  14  15  16  17  18  19  20  21  22  23  24  25  26  27
     15      14  15  16  17  18  19  20  21  22  23  24  25  26  27  28
geomv mgtest2 mgenthin  +
   SIZE=(1,1,2,1000) +
   TIEPOINT=(1.,1.,1000.,1.,1.,1000.,1.,1000., +
            2.,1.,1001.,2.,2.,1000.,2.,1001.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenthin 'NOEJECT
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp     1       3       5       7       9      11      13      15      17      19      21      23      25      27      29
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2       0 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp    31      33      35      37      39      41      43      45      47      49      51      53      55      57      59
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp    61      63      65      67      69      71      73      75      77      79      81      83      85      87      89
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp    91      93      95      97      99     101     103     105     107     109     111     113     115     117     119
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   121     123     125     127     129     131     133     135     137     139     141     143     145     147     149
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   151     153     155     157     159     161     163     165     167     169     171     173     175     177     179
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   181     183     185     187     189     191     193     195     197     199     201     203     205     207     209
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   211     213     215     217     219     221     223     225     227     229     231     233     235     237     239
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   241     243     245     247     249     251     253     255     257     259     261     263     265     267     269
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   271     273     275     277     279     281     283     285     287     289     291     293     295     297     299
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   301     303     305     307     309     311     313     315     317     319     321     323     325     327     329
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   331     333     335     337     339     341     343     345     347     349     351     353     355     357     359
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   361     363     365     367     369     371     373     375     377     379     381     383     385     387     389
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   391     393     395     397     399     401     403     405     407     409     411     413     415     417     419
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   421     423     425     427     429     431     433     435     437     439     441     443     445     447     449
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   451     453     455     457     459     461     463     465     467     469     471     473     475     477     479
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   481     483     485     487     489     491     493     495     497     499     501     503     505     507     509
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   511     513     515     517     519     521     523     525     527     529     531     533     535     537     539
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   541     543     545     547     549     551     553     555     557     559     561     563     565     567     569
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   571     573     575     577     579     581     583     585     587     589     591     593     595     597     599
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   601     603     605     607     609     611     613     615     617     619     621     623     625     627     629
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   631     633     635     637     639     641     643     645     647     649     651     653     655     657     659
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   661     663     665     667     669     671     673     675     677     679     681     683     685     687     689
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   691     693     695     697     699     701     703     705     707     709     711     713     715     717     719
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   721     723     725     727     729     731     733     735     737     739     741     743     745     747     749
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   751     753     755     757     759     761     763     765     767     769     771     773     775     777     779
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   781     783     785     787     789     791     793     795     797     799     801     803     805     807     809
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   811     813     815     817     819     821     823     825     827     829     831     833     835     837     839
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   841     843     845     847     849     851     853     855     857     859     861     863     865     867     869
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   871     873     875     877     879     881     883     885     887     889     891     893     895     897     899
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   901     903     905     907     909     911     913     915     917     919     921     923     925     927     929
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   931     933     935     937     939     941     943     945     947     949     951     953     955     957     959
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   961     963     965     967     969     971     973     975     977     979     981     983     985     987     989
   Line
      1     231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233 233

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp   991     993     995     997     999
   Line
      1     231 231 231 231 231 231 231 231 231 231
      2     233 233 233 233 233 233 233 233 233   0
gen mgtest 10 10 SINC=40 LINC=40 'HALF
Beginning VICAR task gen
GEN Version 6
GEN task completed
list mgtest
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0    40    80   120   160   200   240   280   320   360
      2        40    80   120   160   200   240   280   320   360   400
      3        80   120   160   200   240   280   320   360   400   440
      4       120   160   200   240   280   320   360   400   440   480
      5       160   200   240   280   320   360   400   440   480   520
      6       200   240   280   320   360   400   440   480   520   560
      7       240   280   320   360   400   440   480   520   560   600
      8       280   320   360   400   440   480   520   560   600   640
      9       320   360   400   440   480   520   560   600   640   680
     10       360   400   440   480   520   560   600   640   680   720
geomv mgtest mgtest1  +
   TIEPOINT=(1.,1.,1.,1.,   1.,20.,1.,20., +
            20.,1.,20.,1., 20.,20.,20.,20.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
difpic (mgtest1 mgtest)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENCES =   0
geomv mgtest mgenlarg  +
   SIZE=(1,1,20,20)    +
   TIEPOINT=(1.,1.,1.,1.,1.,20.,1.,10., +
                20.,1.,10.,1.,20.,20.,10.,10.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenlarg
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line
      1         0     0    40    57    76    95   114   133   152   171   189   208   227   246   265
      2         0     0    40    76    95   114   133   152   171   189   208   227   246   265   284
      3        40    40    80    95   114   133   152   171   189   208   227   246   265   284   303
      4        57    76    95   114   133   152   171   189   208   227   246   265   284   303   322
      5        76    95   114   133   152   171   189   208   227   246   265   284   303   322   341
      6        95   114   133   152   171   189   208   227   246   265   284   303   322   341   360
      7       114   133   152   171   189   208   227   246   265   284   303   322   341   360   379
      8       133   152   171   189   208   227   246   265   284   303   322   341   360   379   398
      9       152   171   189   208   227   246   265   284   303   322   341   360   379   398   417
     10       171   189   208   227   246   265   284   303   322   341   360   379   398   417   436
     11       189   208   227   246   265   284   303   322   341   360   379   398   417   436   455
     12       208   227   246   265   284   303   322   341   360   379   398   417   436   455   474
     13       227   246   265   284   303   322   341   360   379   398   417   436   455   474   493
     14       246   265   284   303   322   341   360   379   398   417   436   455   474   493   512
     15       265   284   303   322   341   360   379   398   417   436   455   474   493   512   531
     16       284   303   322   341   360   379   398   417   436   455   474   493   512   531   549
     17       303   322   341   360   379   398   417   436   455   474   493   512   531   549   568
     18       322   341   360   379   398   417   436   455   474   493   512   531   549   568   587
     19       341   360   379   398   417   436   455   474   493   512   531   549   568   587   606
     20       360   379   398   417   436   455   474   493   512   531   549   568   587   606   625

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
     Samp      16    17    18    19    20
   Line
      1       284   303   322   341   360
      2       303   322   341   360   379
      3       322   341   360   379   398
      4       341   360   379   398   417
      5       360   379   398   417   436
      6       379   398   417   436   455
      7       398   417   436   455   474
      8       417   436   455   474   493
      9       436   455   474   493   512
     10       455   474   493   512   531
     11       474   493   512   531   549
     12       493   512   531   549   568
     13       512   531   549   568   587
     14       531   549   568   587   606
     15       549   568   587   606   625
     16       568   587   606   625   644
     17       587   606   625   644   663
     18       606   625   644   663   682
     19       625   644   663   682   701
     20       644   663   682   701   720
geomv mgtest mgrotat  +
   SIZE=(1,1,20,20)   +
   TIEPOINT=(1.,1.,5.,-5.,1.,20.,-5.,5., +
                20.,1.,15.,5.,20.,20.,5.,15.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgrotat
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
     Samp       1     2     3     4     5     6     7     8     9    10    11    12    13    14    15
   Line

      4         0     0     0     0     0     0     0     0     0    40    40     0     0     0     0
      5         0     0     0     0     0     0     0     0    88    88    88    88     0     0     0
      6         0     0     0     0     0     0     0   131   131   131   131   131   131     0     0
      7         0     0     0     0     0     0   173   173   173   173   173   173   173   173     0
      8         0     0     0     0     0   215   215   215   215   215   215   215   215   215   215
      9         0     0     0     0   257   257   257   257   257   257   257   257   257   257   257
     10         0     0     0   299   299   299   299   299   299   299   299   299   299   299   299
     11         0     0   341   341   341   341   341   341   341   341   341   341   341   341   341
     12         0     0   383   383   383   383   383   383   383   383   383   383   383   383   383
     13         0     0     0   425   425   425   425   425   425   425   425   425   425   425   425
     14         0     0     0     0   467   467   467   467   467   467   467   467   467   467   467
     15         0     0     0     0     0   509   509   509   509   509   509   509   509   509   509
     16         0     0     0     0     0     0   552   552   552   552   552   552   552   552     0
     17         0     0     0     0     0     0     0   594   594   594   594   594   594     0     0
     18         0     0     0     0     0     0     0     0   636   636   636   636     0     0     0
     19         0     0     0     0     0     0     0     0     0   678   678     0     0     0     0

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
     Samp      16    17    18    19    20
   Line

      9       257     0     0     0     0
     10       299   299     0     0     0
     11       341   341   341     0     0
     12       383   383   383     0     0
     13       425   425     0     0     0
     14       467     0     0     0     0
gen mgtest2 1000 1000 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
geomv mgtest2 mgenthin  +
   SIZE=(1,1,1000,1000) +
   TIEPOINT=(1.,1.,7.,7.,1.,1000.,1.,1022., +
            1000.,1.,970.,22.,1000.,1000.,1050.,1060.)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenthin linc=199 sinc=199
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:50 2011
     Samp     1     399     797
   Line
      1      60 146 232  63 149   0
    200     132 245 101 214  71   0
    399     204  87 226 110 248   0
    598      19 185  95  31 172   0
    797      91  28 220 157  94   0
    996     163 126  89   0   0   0
gen mgtest2 400 400 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=444
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*4000000","c2=mod(c2,0.0001)*4000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=30,NAV=30,MINL=1.,MINS=1.,MAXL=400.,MAXS=400.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
geomv INP=(mgtest2,b) OUT=mgenlarg  +
   SIZE=(1,1,400,400)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenlarg linc=39 sinc=39
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:51 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:52 2011
     Samp     1      79     157     235     313     391
   Line
      1       0 130  25 131   4 132   5 134   7 135   0
     40      45 174  47 175  48 177  50 178  51 180   0
     79      90 218  91 220  92 221  94 223  95 224   0
    118     134   7 135  10 137   9 138  11 140  12   0
    157     178  51 180  52 181  54 182  55 184  57   0
    196     223  95 224  97 225  98 227  99 228 101   0
    235      11 140  12 141  14 142  15 144  16 145   0
    274      55 184  57 185  58 187  59 188  61 189   0
    313      99 228 101 230 102 231 104 232 105 234   0
    352     144  16 145  18 147  19 148  21 149  22   0
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=44
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=7,NAV=6,MINL=1.,MINS=1.,MAXL=10.,MAXS=10.  +
    'MGEOM
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
geomv INP=mgtest OUT=mgenlarg PARMS=b  +
   SIZE=(1,1,10,10)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenlarg
Beginning VICAR task list

   HALF     samples are interpreted as HALFWORD data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:49 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:52 2011
     Samp       1     2     3     4     5     6     7     8     9    10
   Line
      1         0    52    96   140   184   228   272   316   360     0
      2        52    96   140   184   228   272   316   360   404     0
      3        96   140   184   228   272   316   360   404   448     0
      4       140   184   228   272   316   360   404   448   492     0
      5       184   228   272   316   360   404   448   492   536     0
      6       228   272   316   360   404   448   492   536   580     0
      7       272   316   360   404   448   492   536   580   624     0
      8       316   360   404   448   492   536   580   624   668     0
      9       360   404   448   492   536   580   624   668   712     0
gen mgtest2 1000 1000 SINC=3 LINC=7
Beginning VICAR task gen
GEN Version 6
GEN task completed
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=444
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+3743)","c2=sqrt(index*7+4431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*10000000","c2=mod(c2,0.0001)*10000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1","c4=c2*1.1")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=200,NAV=200,MINL=1.,MINS=1.,MAXL=1000.,MAXS=1000.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
geomv INP=(mgtest2,b) OUT=mgenlarg  +
   SIZE=(1,1,1000,1000)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgenlarg linc=199 sinc=199
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:31:52 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:31:53 2011
     Samp     1     399     797
   Line
      1       0 146  34 179  68   0
    200     253 142  31 175  64   0
    399     250 138  27 172  60   0
    598     246 135  23 168  57   0
    797     242 131  20 164  53   0
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
