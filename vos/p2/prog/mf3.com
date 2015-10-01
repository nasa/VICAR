$!****************************************************************************
$!
$! Build proc for MIPL module mf3
$! VPACK Version 1.9, Wednesday, December 21, 2011, 14:18:46
$!
$! Execute by entering:		$ @mf3
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
$ write sys$output "*** module mf3 ***"
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
$ write sys$output "Invalid argument given to mf3.com file -- ", primary
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
$   if F$SEARCH("mf3.imake") .nes. ""
$   then
$      vimake mf3
$      purge mf3.bld
$   else
$      if F$SEARCH("mf3.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mf3
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mf3.bld "STD"
$   else
$      @mf3.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mf3.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mf3.com -mixed -
	-s mf3.c -
	-i mf3.imake -
	-p mf3.pdf -
	-t tstmf3.pdf devmf3.pdf tstmf3.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mf3.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <string.h>
/*#include <strings.h>*/
/*#include <ctype.h>*/
#include <stdlib.h>	/* this is in the MIPL mf3.c */

#include "vicmain_c.h"
/*#include "ms_defines.h"*/
#include "applic.h"
#include "ftnbridge.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
/*#include "cartoMemUtils.h"*/
/*#include "cartoVicarProtos.h"*/

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define MAXTEXT         26
#define ARITHBUF        2062
#define OPBUF           3000
#define STRINGBUF       120000
#define NUMCOLS         9    /* problem with s=9 in knoth() */
#define NUMCOLS1        NUMCOLS+1
#define NUMCOLS2        NUMCOLS*2

int fp,sbop2,cp,nbpo,idebug,debugrec1,functionsize;

/************************************************************************/
/* program mf3                                                      */
/************************************************************************/
/*  99-11 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

char cvec[64] = {'0','1','2','3','4','5','6','7','8','9',
     '.','a','l','o',
     'g','i','n','t','s','q','r','x','d','m','b','c','e','p',
     'f','h','j','k','u','v','w','y','z','_','A','B',
     'C','D','E','F','G','H','I','J','K','L','M','N',
     'O','P','Q','R','S','T','U','V','W','X','Y','Z'};

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


int mtchfield(q,fld,nincol)
   char *q,fld[20][MAXTEXT];
   int nincol;
{
   int len1,len2,j,js;
   char *r;

   r = strpbrk(q,",)");
   len1 = r-q;
   js = -1;
   for (j=0;j<nincol;j++)
      {
      len2 = strlen(fld[j]);
      if (len1!=len2) continue;
      if (strncmp(q,fld[j],len1)==0) js = j;
      }
   if (js<0) zmabend("field name not found");
   return(js);
}
double ffetchcd(k,typ,c_data)
   int k,typ;
   unsigned char c_data[];
{
   unsigned char ct1();
   short int ct2(); int ct4(); float ct7(); double ct8();
   switch(typ)
      {
      case 1: return((double)ct1(&c_data[k]));
      case 2: return((double)ct2(&c_data[k]));
      case 4: return((double)ct4(&c_data[k]));
      case 7: return((double)ct7(&c_data[k]));
      case 8: return(ct8(&c_data[k]));
      }
   return(0.);
}
void fstorecd(k,typ,val,c_data)
   int k,typ; double val;
   unsigned char c_data[];
{
    short int x2; int x4; float x7; unsigned char x1;
    switch(typ)
       {
       case 1: x1 = (unsigned char) val;
	       st1(x1,&c_data[k]); return;
       case 2: x2 = (short int) val;
	       st2(x2,&c_data[k]); return;
       case 4: x4 = (int) val;
	       st4(x4,&c_data[k]); return;
       case 7: x7 = (float)val;
	       st7(x7,&c_data[k]); return;
       case 8: st8(val,&c_data[k]); return;
       }
   return;
}

/* c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines */
   
void stsget(s,fstrng,dbuf,cnum,sbuf,sptr)
   int *s,*cnum,*sptr;
   char *fstrng,*sbuf;
   double *dbuf;

{
   double rnum,rfac=0;
   int first,atop;
   char c,cl,minus,aop[19],intg[66];
   int fcv[148] = {1661,1662,1663,1664,1665,1666,1667,1668,1669,16610,
      16611,16612,16613,16614,16615,16616,16617,16618,16619,16620,
      16621,16622,16623,16624,16625,16626,16627,16628,16629,16630,
      16631,16632,16633,16634,16635,16636,16637,16638,16639,16640,
      16641,16642,16643,16644,16645,16646,16647,16648,16649,16650,
      16651,16652,16653,16654,16655,16656,16657,16658,16659,16660,
      16661,16662,16663,16664,16665,16666,16667,16668,16669,16670,
      16671,16672,16673,16674,16675,16676,16677,16678,16679,16680,
      16681,16682,16683,16684,16685,16686,16687,16688,16689,16690,
      16691,16692,16693,16694,16695,16696,16697,16698,16699,166100,
      21282,168481,
      13686,19357,168481,12677,1234410,12344,20117,1966,2648,1826,
      134311,134661,13452,1358,1677,2431,2466,2452,128262,12966,
      13648,12826,24546,24004,2627,262741,29990,25990,13474796,20474796,
      19173,346306,146306,22883376,1991476,2848,199279,1992144,1992827,
      153397,233397,1943,153990,283990,2449990,2449943};
   int kcv[148] = {1,2,3,4,5,6,7,8,9,10,  11,12,13,14,15,16,17,18,19,20,
      21,22,23,24,25,26,27,28,29,30,  31,32,33,34,35,36,37,38,39,40,
      41,42,43,44,45,46,47,48,49,50,  51,52,53,54,55,56,57,58,59,60,
      61,62,63,64,65,66,67,68,69,70,  71,72,73,74,75,76,77,78,79,80,
      81,82,83,84,85,86,87,88,89,90,  91,92,93,94,95,96,97,98,99,100,
      102,101,
      101,18,20, 8, 6, 7,16,17,18,19,  20,21,22,23, 8,20,21,22,25,26,
      27,28,39,40,41,42,43,44,45,46,  47,48,49,50,51,52,53,54,55,56,
      57,58,59,60,61,62};
   int cop[20] = {1,2,3,4,9,10,0,14,24,11, 29,30,31,32,33,34,35,36,37,38};
   int prior[63] = {0, 4,4,5,5,6,7,7,7,0,0, 0,0,0,1,0,7,7,7,7,1,
                       1,1,7,7,1,7,7,7,3,3, 3,3,3,3,2,2,2,7,1,1,
                       1,1,1,1,1,1,1,7,7,1, 7,1,1,1,1,1,1,7,7,1,7,7};
   int bop2[63] = {0,  1,1,1,1,1,0,0,0,0,1, 1,0,0,0,0,0,0,0,0,1,
                       1,1,0,0,1,0,0,0,1,1, 1,1,1,1,1,1,1,0,1,1,
                       1,1,1,1,1,1,1,0,0,1, 0,1,1,1,1,1,1,0,0,1,0,0};
   int isavtr[6] = {11,12,13,16,17,14};
   int type,fpx,num,snum,i,isav,isl,qtype,ipow,qret=0,isv;
   
   minus = '-';
   strcpy(aop,"+-*/() ,;$<=!|&>^@");
   strcpy(intg,"0123456789.alogintsqrxdmbcepfhjkuvwyz_ABC");
   strcat(intg,"DEFGHIJKLMNOPQRSTUVWXYZ'");
   
   /* temporarily, column names and functions are case insensitive, later
   the column names will become case sensitive, and this routine and
   the main program have to be modified. alz 1/26/00 */
     
      atop = 0;
      first = 1;
      nbpo = 1;
      num = 0;
      snum = 0;
      rnum = 0.0;
      type = 0;
      qtype = 0;
l100: c = fstrng[fp+1];
      if (c==aop[6]) goto l8;
      fpx = fp+1;
      for (i=0;i<17;i++)
         {
         isav = i;
         if (c==aop[i]) goto l700;
         }
      if (c==aop[17]) goto l704;
      goto l39;
l700: if (isav<10||isav>15) goto l702;
      c = fstrng[fp+2];
      for (i=10;i<16;i++)
         {
         if (c==aop[i]) goto l703;
         }
l702: if (isav==16) isav = 18;
      if (isav==12) isav = 19;
      if (isav==14) isav = 17;
      fpx = fp+1;
      goto l10;
l703: isav = isavtr[isav-10];
      fpx = fp+2;
      goto l10;
l704: atop = 1;
      fp = fp+1;
      goto l100;
 l39: first = 0;
      for (i=0;i<65;i++)
         {
         isl = i;
         cl = tolower(c);
         if (cl==intg[i]) goto l4;
         }
      printf("illegal symbol = %c\n",c);
      zmabend("program terminating");
      return;
 l4:  if (isl<10) goto l7;
      if (isl>10) goto l5;
      type = 1;
      rnum = (double)num;
      rfac = 1.0;
      goto l8;
 l5:  if (isl==26) goto l55;
      if (isl==64) goto l65;
      type = -1;
      goto l7;
 l55: if (type!=1) goto l7;
      ipow = 0;
      for (i=0;i<10;i++)
         {
         if (fstrng[fp+3]==intg[i]) ipow = ipow+i*10;
         if (fstrng[fp+4]==intg[i]) ipow = ipow+i;
         }
      if (fstrng[fp+2]==aop[0]) rnum = rnum*pow(10.0,(double)ipow);
      if (fstrng[fp+2]==aop[1]) rnum = rnum*pow(0.10,(double)ipow);
      fp = fp+4;
      goto l100;
 l65: type = 0;
      qret = *sptr;
      qtype = 1;
      for (i=0;i<55;i++)
         {
         if (fstrng[fp+2]==intg[64]) break;
         sbuf[*sptr] = fstrng[fp+2];
         *sptr = *sptr+1;
         fp = fp+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      fp = fp+2;
      goto l100;
 l7:  num = num*10+isl;
      snum = snum*39;
      if (snum>100000000) snum = snum/31;
      snum = snum+isl;
      rfac = .1*rfac;
      rnum = rnum+rfac*(double)(isl);
 l8:  fp = fp+1;
      goto l100;
 l10: if (first) goto l20;
      sbop2 = 0;
      if (type<0) goto l11;
      if (type==0) goto l12;
      if (type>0) goto l13;
 l11: if (!atop) goto l801;
      for (i=0;i<148;i++)
         {
         isv = i;
         if (num==fcv[i]) goto l15;
         }
      zmabend("operator not found");
      return;
l801: for (i=0;i<20;i++)
         {
         if (snum!=cnum[i]) continue;
         *s = i;
         return;
         }
      return;
 l15: *s = kcv[isv];
      if (isv<=104) return;
      sbop2 = bop2[*s];
      if (prior[*s]==1) nbpo = 0;
      return;
 l12: cp = cp+1;
      dbuf[cp] = (double)(num);
      if (qtype>0) dbuf[cp] = (double)qret;
      *s = cp;
      return;
 l13: cp = cp+1;
      dbuf[cp] = rnum;
      *s = cp;
      return;
 l20: fp = fpx;
      *s = cop[isav];
      if (*s==14) nbpo = 0;
      if (c==aop[8]) fstrng[fp] = minus;
      if (c!=aop[2]) goto l21;
      c = fstrng[fp+1];
      if (c!=aop[2]) goto l21;
      *s = 5;
      fp = fp+1;
 l21: sbop2 = bop2[*s];
      return;
}
    
/*
   c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines

c  modified 1/17/90 for string functions
c  modified 3/16/87 a. zobrist for mosx system
c  kludged again 6/18/87 a. zobrist
c  this routine is really getting encrusted from about
c  five major changes */

void sp_knuth(char* fstrng,int* ibuf,double* dbuf,char* sbuf,int* cnum,int* sptr)
     /*   char *fstrng,*sbuf;
   int cnum,*sptr,*ibuf;
   double *dbuf; */
{

   int firvar;
   int stack[50],bpostk[10];
   int prior[63] = {0, 4,4,5,5,6,7,7,7,0,0, 0,0,0,1,0,7,7,7,7,1,
                       1,1,7,7,1,7,7,7,3,3, 3,3,3,3,2,2,2,7,1,1,
                       1,1,1,1,1,1,1,7,7,1, 7,1,1,1,1,1,1,7,7,1,7,7};
   int bop3[63] =  {0, 1,1,1,1,1,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,1,
                       1,1,0,0,1,0,0,0,1,1, 1,1,1,1,1,1,1,0,1,1,
                       1,1,1,1,1,1,1,0,0,1, 0,1,1,1,1,1,1,0,0,1,0,0};
   char schar[10];
   
   int ix,itemp,bp,op,sp,s,loc,iop,ptr,s2;

   strcpy(schar,"(,-+$; xx"); 
   
   /* have to pull out unary + and unary -, the algorithm will put back
   later */
   
   schar[7] = schar[0];
   for (ix=0;ix<functionsize;ix++)
      {
      schar[8] = fstrng[ix];
      if (schar[8]==schar[4]) break;
      if (schar[8]==schar[6]) continue;
      if (schar[7]!=schar[0]&&schar[7]!=schar[1])
         {
         schar[7] = schar[8];
         continue;
         }
      if (schar[8]==schar[2]) fstrng[ix] = schar[5];
      if (schar[8]==schar[3]) fstrng[ix] = schar[6];
      schar[7] = schar[8];
      }
      
      if (idebug) printf("\n");
      bp = -1;
      op = 0;
      fp = -1;
      cp = 102;
      sp = -1;
      s = 12; /* data value */
      itemp = 1062; /* stack ptr */
      firvar = 1;
      ibuf[1] = 15*65536;
      goto l4;
 l2:  if (sbop2) goto l3;
 l4:  sp = sp+1;
      stack[sp] = s;
 l5:  stsget(&s,fstrng,dbuf,cnum,sbuf,sptr);
      if (idebug) printf("input symbol: %d\n",s);
      if (firvar&&(s!=9)) ibuf[0] = 13*65536+s;
      if (s!=9) firvar = 0;
      if (nbpo) goto l2;
      if (s==14) goto l21;
      bp = bp+1;
      bpostk[bp] = s;
      goto l5;
 l21: s = bpostk[bp];
      bp = bp-1;
      sbop2 = 1;
      goto l2;
 l3:  s2 = stack[sp-1];
      if (prior[s]>prior[s2]) goto l4;
      if (s2==12) goto l24;
      if (s2==9) goto l7;
      if (bop3[s2]) goto l10;
      goto l9;
 l7:  stack[sp-1] = stack[sp];
      sp = sp-1;
      goto l5;
 l9:  loc = stack[sp];
      ibuf[op] = stack[sp-1]*65536+loc;
      ibuf[op+1] = 14*65536+itemp;
      op = op+2;
      sp = sp-1;
      goto l11;
 l10: loc = stack[sp-2];
      iop = 14*65536+loc;
      ibuf[op] = iop-65536;
      /*if (ibuf[op-1]!=iop&&loc>=61) ibuf[2*loc] = 1;optimizer*/
      if (ibuf[op-1]==iop) op = op-1;
      loc = stack[sp];
      ibuf[op+1] = stack[sp-1]*65536+loc;
      ibuf[op+2] = 14*65536+itemp;
      op = op+3;
      sp = sp-2;
 l11: stack[sp] = itemp;
      /*if (loc>=61) ibuf[loc-1] = 1;used in optimizer*/
      /*dbuf[itemp] = 0.0;should never need to clear a temp*/
      itemp++;
      ibuf[op] = 15*65536;
      goto l3;
 l24: ptr = 61;
      /*for (itemp=1061;itemp<250;itemp++) don't use optimizer for now
         {                   indexes screwed
         ibuf[ptr-1] = ibuf[itemp-1];
         m = ibuf[itemp-1]/65536;
         if (m==15) return;
         n = ibuf[itemp-1]-m*65536;
         if (m!=14||ibuf[n-1]!=0) ptr = ptr+2;
         }*/
      return;
}

  
void insq(buf,indx,ptr)
   char *buf;
   int indx,ptr;
{
      int ichar,i,iu=0,ict,ir=0;
      
      if (indx==0) return;
      for (i=ptr;i<131;i++)
         {
         iu = i;
         ichar = (int)buf[i];
         if (ichar==0) break;
         }
      ict = iu-ptr+1;
      for (i=1;i<=ict;i++)
         {
         ir = iu-i+1;
         buf[ir+1] = buf[ir];
         }
      buf[ir] = '\?';
      return;
}

void delq(buf,ptr)
   char *buf;
   int ptr;
{
      int ichar,i,iq,iu=0,iu2;
   
      if (ptr<0) return;
   
      for (i=ptr;i<131;i++)
         {
         iu = i;
         ichar = (int)buf[i];
         if (ichar==0) break;
         }
      iq = 0;
      
      for (i=ptr;i<131;i++)
         {
         ichar = (int)buf[i];
         if (ichar!=63) break;
         iq = iq+1;
         }
 
      iu2 = iu-iq;
      
      for (i=ptr;i<iu2;i++)
         {
         buf[i] = buf[i+iq];
         }
      return;
}


/*    modified 3/16/87 by a. zobrist for mosx system */
/*    kludged from fortran to c 1/24/00 by a. zobrist 
      ... no attempt to use c constructs, just a straight conversion
      of the fortran lines*/

void sp_xknuth(ibuf,dbuf,sbuf,sptr,result)
   int *ibuf,*sptr;
   double *result,*dbuf;
   char *sbuf;
{
 double reg=0,div,t,num,sig,minut,secnd,frac,sig2;
 char patbuf[131];                  /* left at fortran indexing */
 char tchar;
 int starp[4],isu[4],blnct[3];      /* left at fortran indexing */
       
 int ptr,op,opnd,ibit,jbit,kbit,ireg,jreg,i,j,tmtch,mtch,len,imtch=0;
 int osptr,slen,pmtch,stp,cptr,isu1,is1,isu2,is2,isu3,is3,break2;
 int lrsw,ltr,btr,str,knum,itop=0,kdig,itop2,deccnt,ichar,ichxx;
 char *p,*q,mtchbuf[1000];
 
 for (ptr=0;ptr<OPBUF;ptr++)
    {
    op = ibuf[ptr]>>16;
    opnd = ibuf[ptr]&65535;
    if (idebug&&debugrec1) printf("xknuth:op,opnd %d %d\n",op,opnd);
    
    switch (op)
    {      
    case 9: case 10: case 11: case 12: case 39: case 40: 
      zmabend("arith execution error");
      break;
    case 1:
      reg = reg+dbuf[opnd];
      break;
    case 2:
      reg = reg-dbuf[opnd];
      break;
    case 3:
      reg = reg*dbuf[opnd];
      break;
    case 4:
      div = dbuf[opnd];
      if (fabs(div)>=1.0e-20)
         {
         reg = reg/div;
         break;
         }
      if (div>=0) div = div+1.0e-20;
      if (div<0) div = div-1.0e-20;
      reg = reg/div;
      break;
    case 37:    /* temporarily using ^ for exponentiation */
      reg = pow(MAX(reg,1.0e-6),dbuf[opnd]);
      break;
    case 6:
      reg = log10(MAX(dbuf[opnd],1.0e-6));
      break;
    case 7:
      reg = log(MAX(dbuf[opnd],1.0e-6));
      break;
    case 8:
      reg = (int)(dbuf[opnd]);
      break;
    case 13:
      reg = dbuf[opnd];
      break;
    case 14:
      dbuf[opnd] = reg;
      break;
    case 15:
      *result = reg;
      return;
    case 16:
      reg = sqrt(fabs(dbuf[opnd]));
      break;
    case 17:
      reg = sin(dbuf[opnd]);
      break;
    case 18:
      reg = cos(dbuf[opnd]);
      break;
    case 19:
      reg = tan(dbuf[opnd]);
      break;
    case 20:
      reg = MAX(reg,dbuf[opnd]);
      break;
    case 21:
      reg = MIN(reg,dbuf[opnd]);
      break;
    case 22:
      div = dbuf[opnd];
      if (fabs(div)>=1.0e-20)
         {
         reg = fmod(reg,div);
         break;
         }
      if (div>=0) div = div+1.0e-20;
      if (div<0) div = div-1.0e-20;
      reg = fmod(reg,div);
      break;
    case 23:
      reg = fabs(dbuf[opnd]);
      break;
    case 24:
      reg = -dbuf[opnd];
      break;
    case 25:
      if ((reg==0.0)&&(dbuf[opnd]==0.0))
         {
         reg = 0.0;
         break;
         }
      reg = atan2(reg,dbuf[opnd]);
      break;
    case 26:
      reg = asin(dbuf[opnd]);
      break;
    case 27:
      reg = acos(dbuf[opnd]);
      break;
    case 28:
      reg = atan(dbuf[opnd]);
      break;
    case 29:
      t = 0.0;
      if (reg<dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 30:
      t = 0.0;
      if (reg<=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 31:
      t = 0.0;
      if (reg==dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 32:
      t = 0.0;
      if (reg!=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 33:
      t = 0.0;
      if (reg>=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 34:
      t = 0.0;
      if (reg>dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 35:
      ibit = reg;
      jbit = dbuf[opnd];
      kbit = ibit|jbit;
      reg = (double)kbit;
      break;
    case 36:
      ibit = reg;
      jbit = dbuf[opnd];
      kbit = ibit&jbit;
      reg = (double)kbit;
      break;
    /*case 37: the ^ symbol appropriated for expon
      ibit = reg;
      jbit = dbuf[opnd];
      kbit = ibit^jbit;
      reg = (double)kbit;
      break;*/
    case 38:
      reg = 1.0-dbuf[opnd];
      break;
                               /* cat */
    case 41:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         if (ichar==0) break;
         }
      *sptr = *sptr-1;
      for (j=0;j<100;j++)
         {
         ichar = (int)sbuf[jreg+j];
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         if (ichar==0) break;
         }
      break;
                                /* break */
    case 42:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         for (j=0;j<100;j++)
            {
            ichxx = (int)sbuf[jreg+j];
            if (ichxx==0) break;
            if (ichxx==ichar) break;
            }
         if (ichxx==ichar) break;
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* fstr */
    case 43:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      for (i=0;i<jreg;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* bstr */
    case 44:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      
      for (i=0;i<60;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         if (i>=jreg) sbuf[*sptr] = (char)ichar;
         if (i>=jreg) *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* adelete */
    case 45:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         for (j=0;j<100;j++)
            {
            ichxx = (int)sbuf[jreg+j];
            if (ichxx==0) break;
            if (ichxx==ichar) break;
            }
         if (ichxx==ichar) continue;
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* sdelete */
    case 46:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      tmtch = 0;
      
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[jreg+j];
         if (ichxx==0) break;
         tmtch = tmtch+1;
         }
      mtch = 0;
      i = 0;
      while (i<100)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         ichxx = (int)sbuf[jreg+mtch];
         if (ichxx==ichar) mtch = mtch+1;
         else if (mtch>0)
            {
            i = i-mtch+1;
            *sptr = *sptr-mtch+1;
            mtch = 0;
            continue;
            }
         if (mtch>=tmtch)
            {
            *sptr = *sptr-tmtch+1;
            mtch = 0;
            }
         else
            {
            sbuf[*sptr] = (char)ichar;
            *sptr = *sptr+1;
            }
         i++;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* trim */
    case 47:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      len = 0;
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         len = len+1;
         }
      if (len>=0) for (i=0;i<len;i++)
         {
         ichar = (int)sbuf[*sptr-1];
         osptr = *sptr;
         
         for (j=0;j<55;j++)
            {
            ichxx = (int)sbuf[jreg+j];
            if (ichxx==0) break;
            if (ichxx==ichar) *sptr = *sptr-1;
            }
         if (*sptr==osptr) break;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* ucase */
    case 48:
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[jreg+i];
         if (ichar==0) break;
         sbuf[*sptr] = toupper((char)ichar);
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* lcase */
    case 49:
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[jreg+i];
         if (ichar==0) break;
         sbuf[*sptr] = tolower((char)ichar);
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* replace */
    case 50:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      tmtch = 0;
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[jreg+j];
         if (ichxx==61) break;
         if (ichxx==0) zmabend("no equals sign in replacement string");
         tmtch = tmtch+1;
         }
      mtch = 0;
      i = 0;
      while (i<100)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         ichxx = (int)sbuf[jreg+mtch];
         if (ichxx==ichar) mtch = mtch+1;
         else if (mtch>0)
            {
            i = i-mtch+1;
            *sptr = *sptr-mtch+1;
            mtch = 0;
            continue;
            }
         if (mtch<tmtch)
            {
            sbuf[*sptr] = (char)ichar;
            *sptr = *sptr+1;
            i++;
            continue;
            }
         *sptr = *sptr-tmtch+1;
         mtch = 0;
      
         for (j=0;j<100;j++)
            {
            ichxx = (int)sbuf[jreg+tmtch+j+1];
            if (ichxx==0) break;
            sbuf[*sptr] = (char)ichxx;
            *sptr = *sptr+1;
            }
         i++;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* strlen */
    case 51:
      jreg = (int)(dbuf[opnd]+.001);
      len = 0;
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[jreg+i];
         if (ichar==0) break;
         len = len+1;
         }
      reg = (double)len;
      break;
                                /* streq */
    case 53:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = 0.0;
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         ichxx = (int)sbuf[jreg+i];
         if (ichxx!=ichar) goto done53;
         if (ichar==0||ichxx==0) break;
         }
      if (ichar==0&&ichxx==0) reg = 1.0;
      done53:
      break;
                                /* strsub */
    case 54:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = 0.0;
      tmtch = 0;
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[jreg+j];
         if (ichxx==0) break;
         tmtch = tmtch+1;
         }
      mtch = 0;
      i = 0;
      while (i<100)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar==0) break;
         ichxx = (int)sbuf[jreg+mtch];
         if (ichxx==ichar) mtch = mtch+1;
         else if (mtch>0)
            {
            i = i-mtch+1;
            *sptr = *sptr-mtch+1;
            mtch = 0;
            continue;
            }
         if (mtch>=tmtch)
            {
            reg = 1.0;
            break;
            }
         i++;
         }
      break;
                               /*  52=pos 55=strpat */
    case 52: case 55:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = 0.0;
      slen = 0;
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[ireg+j];
         if (ichxx==0) break;
         slen = slen+1;
         }
      pmtch = 0;
      stp = 0;
      isu[1] = 1;
      isu[2] = 1;
      isu[3] = 1;
      starp[1] = -999;
      starp[2] = -999;
      starp[3] = -999;
      cptr = 0;
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[jreg+j];
         patbuf[cptr] = (char)ichxx;
         cptr = cptr+1;
         if (ichxx==42)
            {
            cptr = cptr-1;
            pmtch = pmtch-1;
            stp = stp+1;
            starp[stp] = cptr;
            isu[stp] = slen;
            }
         if (ichxx==0) break;
         pmtch = pmtch+1;
         }
      
      isu1 = MIN(isu[1],slen-pmtch+3);
      for (is1=0;is1<isu1;is1++)
         {
         insq(patbuf,is1,starp[1]);
         isu2 = MIN(isu[2],slen-pmtch+3);
         for (is2=0;is2<isu2;is2++)
            {
            insq(patbuf,is2,starp[2]+is1);
            isu3 = MIN(isu[3],slen-pmtch+3);
            for (is3=0;is3<isu3;is3++)
               {
               insq(patbuf,is3,starp[3]+is2+is1);
               tmtch = pmtch+is1+is2+is3;
               if (tmtch>slen+2) break;
               
               for (i=0;i<1000;i++)
                  {
                  mtch = 0;
                  imtch = 0;
                  break2 = 0;
                  for (j=0;j<1000;j++)
                     {
                     ichar = (int)sbuf[ireg+i+j];
                     ichxx = (int)patbuf[mtch];
                     if (ichar!=0||ichxx!=37)
                        {
                        if (ichar==0) { break2 = 1; break; }
                        ichxx = (int)patbuf[mtch];
                        if (ichxx==94)
                           {
                           if (i!=0) { break2 = 1; break; }
                           mtch = mtch+1;
                           ichxx = (int)patbuf[mtch];
                           }
                        if (ichxx!=ichar&&ichxx!=63) break;
                        mtchbuf[imtch++] = (char)ichar;
                        }
                     mtch = mtch+1;
                     if (mtch<tmtch) continue;
                     reg = i+1;
                     if (op==55) reg = 0.0;
                     goto done55;
                     } /* j loop */
                  if (break2) break;
                  } /* i loop */
               } /* is3 */
            delq(patbuf,starp[3]+is2+is1-2);
            } /* is2 */
         delq(patbuf,starp[2]+is1-1);
         } /* is1 */
      done55:
      if (op==55)
         {
         reg = (double)(*sptr);
         for (i=0;i<imtch;i++) sbuf[(*sptr)++] = mtchbuf[i];
         sbuf[(*sptr)++] = (char)0;
         }
      break;
                                /* ljust */
    case 56:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      len = 0;
      blnct[1] = 0;
      blnct[2] = 0;
      lrsw = 1;
      ichxx = (int)' ';
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar!=ichxx) lrsw = 2;
         if (ichar!=ichxx) blnct[2] = 0;
         if (ichar==ichxx) blnct[lrsw] = blnct[lrsw]+1;
         if (ichar==0) break;
         len = len+1;
         }
 
      ltr = MIN(jreg,len-blnct[1]-blnct[2]);
      btr = jreg-ltr;
      str = blnct[1]+1;
      for (i=0;i<ltr;i++)
         {
         sbuf[*sptr] = sbuf[ireg+str+i-1];
         *sptr = *sptr+1;
         }
      for (i=0;i<btr;i++)
         {
         sbuf[*sptr] = ' ';
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                /* rjust */
    case 57:
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      len = 0;
      blnct[1] = 0;
      blnct[2] = 0;
      lrsw = 1;
      ichxx = (int)' ';
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[ireg+i];
         if (ichar!=ichxx) lrsw = 2;
         if (ichar!=ichxx) blnct[2] = 0;
         if (ichar==ichxx) blnct[lrsw] = blnct[lrsw]+1;
         if (ichar==0) break;
         len = len+1;
         }
 
      ltr = MIN(jreg,len-blnct[1]-blnct[2]);
      btr = jreg-ltr;
      str = blnct[1]+1;
      
      for (i=0;i<btr;i++)
         {
         sbuf[*sptr] = ' ';
         *sptr = *sptr+1;
         }
      for (i=0;i<ltr;i++)
         {
         sbuf[*sptr] = sbuf[ireg+str+i-1];
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)ichar;
      *sptr = *sptr+1;
      break;
                                /* num */
    case 58:
      jreg = (int)(dbuf[opnd]+.001);
      p = &sbuf[jreg];
      reg = ms_dnum(&p);
      break;
                                /* i2str */
    case 59:
      ireg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      if (ireg<0)
         {
         sbuf[*sptr] = '-';
         *sptr = *sptr+1;
         ireg = (int)(-dbuf[opnd]+.001);
         }
      for (i=0;i<12;i++)
         {
         knum = ireg/10;
         if (ireg==0&&i!=1) { itop = i; break; }
         kdig = ireg-knum*10;
         ichar = kdig+48;
         sbuf[*sptr+i] = (char)ichar;
         ireg = knum;
         }
      itop2 = itop/2;
      
      for (i=0;i<itop2;i++)
         {
         tchar = sbuf[*sptr+i];
         sbuf[*sptr+i] = sbuf[*sptr+itop-1-i];
         sbuf[*sptr+itop-1-i] = tchar;
         }
      *sptr = *sptr+itop+1;
      sbuf[*sptr-1] = (char)0;
      break;
                                /* f2str */
    case 60:
      num = reg;
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      if (num<0.0)
         {
         sbuf[*sptr] = '-';
         *sptr = *sptr+1;
         num = -num;
         }
      num = num+0.5*pow(0.1,(double)jreg);
      ireg = (int)num;
      deccnt = 0;
      for (i=0;i<50;i++)
         {
         if (num<10.0) break;
         deccnt = deccnt+1;
         num = 0.1*num;
         }
      itop = deccnt+jreg+1;
      for (i=0;i<itop;i++)
         {
         kdig = (int)num;
         num = fmod(num,1.0);
         num = num*10.0;
         ichar = kdig+48;
         sbuf[*sptr] = (char)ichar;
         *sptr = *sptr+1;
         deccnt = deccnt-1;
         if (deccnt!=(-1)) continue;
         if (jreg==0) break;
         sbuf[*sptr] = '.';
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                   /* dmsstr */
    case 61:
      jreg = (int)(dbuf[opnd]+.001);
      len = strlen(&sbuf[jreg]);
      if (strchr("WSwsENen",sbuf[jreg+len-1])!=0)
         {
         if (strchr("WSws",sbuf[jreg+len-1])!=0) sig2 = -1.0;
         else sig2 = 1.0;
         len--;
         sbuf[jreg+len] = (char)0;
         }
      else sig2 = 1.0;
      if (strchr("WSwsENen",sbuf[jreg])!=0)
         {
         if (strchr("WSws",sbuf[jreg])!=0) sig2 = -1.0;
         else sig2 = 1.0;
         jreg++;
         }
      for (p=&sbuf[jreg],q=mtchbuf;;p++)
         {
         if (*p==(char)0) { *q = (char)0; break; }
         if (strchr("0123456789.+-eE",*p)!=0) *(q++) = *p;
         if ((*p=='d'||*p=='D')&&strchr("+-",*(p+1))!=0) *(q++) = *p;
         }
      p = mtchbuf;
      num = ms_dnum(&p);
      if (num<0.0) { sig = -1.0; num = -num;} else sig = 1.0;
      frac = 100.0*((int)(num/100.0));
      secnd = num-frac;
      num -= secnd;
      frac = 10000.0*((int)(num/10000.0));
      minut = num-frac;
      num -= minut;
      reg = sig2*sig*(num/10000.0+minut/6000.0+secnd/3600.0);
      break;
                                    /* dmsnum */
    case 62:
      num = dbuf[opnd];
      if (num<0.0) { sig = -1.0; num = -num;} else sig = 1.0;
      frac = 100.0*((int)(num/100.0));
      secnd = num-frac;
      num -= secnd;
      frac = 10000.0*((int)(num/10000.0));
      minut = num-frac;
      num -= minut;
      reg = sig*(num/10000.0+minut/6000.0+secnd/3600.0);
      break;
 }
 }
 return;
}

/* There is a fairly obscure error that occurs with gcc 4.4.4. I'm
   pretty sure this is an optimization error. The code looked like:

   rcol = savvec[savptr++];

   I printed out savptr (which was 0), "savvec[0]" (which was a
   reasonable value), and "savvec[savptr]" which was a garbage value.
   rcol gets assigned this garbage value, and then later this triggers a
   program failure.

   I'm fairly convinced that this was an optimization bug with gcc 4.4.4.
   The code is very complex, with some fairly large loops in it, so it
   seem to have triggered some problem with the compiler.  The code works
   perfectly fine in debug mode, this isn't a bug Al put into the code or
   anything like that.
  
   As a work around, turn optimization down for this function.
*/

#pragma GCC optimize "O1"

void main44(void)
{
   char c_field[NUMCOLS][MAXTEXT];
   char *function,funcparm[40][251];

   char *c_data,*sbuf;
   int ibuf[OPBUF];
   
   int i,j,k,ibig,ncol,nincol,tablen,rcol,lres,seed;
   int datcols[NUMCOLS],typ[NUMCOLS],wid[NUMCOLS],totwid[NUMCOLS1];
   int sptr,svsptr,mxddwid,savvec[NUMCOLS2];
   int strl,snum,alphc,cptr,lptr,cnum[NUMCOLS],savptr;
   int js,jt,n,ii,iii,l,k1,k2,ist,icount,ksv,isv,wmx,ires;
   char *p,*q=NULL,*r,c,*index();
   char fmtstring[10];
   int j1,j2,j3,j4,j5,j6=0,j7;
   int npar,idef,unit,ibis,status;
   double drand48();
   double phi1,tht1,p1,p2,p3;
   double phi2,tht2,q1,q2,q3,q4;
   double phi3,tht3,n1,n2,n3;
   double pxn1,pxn2,pxn3,pxq1,pxq2,pxq3;
   double ndq,pdn,pdq,pxndq,phi,pxnpxq,raddeg,mpr,rdist=0;
   double degrad = 57.295779512;
   char c_tmp[8],blanks[500],zeros[2];
   double res,dbuf[ARITHBUF];
   double sum,vout,mean,ssq,val0,val1=0,val2=0,cmp0,cmp1,cmp2,val=0,pval=0;
   double vmin=0,vmax=0,dcmp,ldiff,ndiff,diff;
   
   zifmessage("mf3 version Wed Oct 07 2008");
   
   /* get the function parameter and concatenate it */
   
   functionsize = 0;
   /*mz_alloc2((unsigned char ***)&funcparm,40,251,1);why doesn't this work
   I suspect that the VICAR routine made an adjacency assumption for the malloc*/
   zvparm("function",funcparm,&npar,&idef,40,251);
   for (i=0;i<npar;i++) functionsize += strlen(funcparm[i]);
   mz_alloc1((unsigned char **)&function,functionsize+1,1);
   strcpy(function,funcparm[0]);
   for (i=1;i<npar;i++) strcat(function,funcparm[i]);
   /*mz_free2((unsigned char **)funcparm,40);*/
   printf("function string = %s\n",function);
   
   zvp("seed",&seed,&npar);
   zvp("debug",&idebug,&npar);
   
   /* open the data set */
   
   status = zvunit(&unit,"inp",1,NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&tablen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (tablen==0) return;
   
   mxddwid = 50;
   mz_alloc1((unsigned char **)&sbuf,STRINGBUF,1);
   totwid[0] = 0;
   for (i=0;i<500;i++) blanks[i] = ' ';
   for (i=0;i<2;i++) zeros[i] = (char)0;

   /* get all of the unique field names in sequence and save the
      character names in c_field */

   snum = 0; cptr = 0; lptr = 0; savptr = 0; alphc = 0;
   for (i=0;i<NUMCOLS;i++) cnum[i] = -1;
   strl = strlen(function);
   for (i=0;i<=strl;i++)
      {
      c = function[i];
      if (c=='@')
	 do c = function[++i]; while (isalnum(c));
      if (c=='\'')
	 do c = function[++i]; while (c!='\'');
      c = tolower(c);
      for (j=0;j<64;j++) if (c==cvec[j])
	 {
	 c_field[cptr][lptr++] = c;
	 snum *= 39;
	 if (snum>100000000) snum /= 31;
	 snum += j;
	 if (j>10 && lptr==1) alphc = 1;
	 goto nexti;
	 }
      if (snum*alphc!=0)
	 {
	 alphc = 0;
	 ksv = -1;
	 for (k=0;k<cptr;k++)
	    if (cnum[k]==snum) ksv = k;
	 if (ksv==(-1))
	    {
	    ksv = cptr;
	    cnum[cptr] = snum;
            if (cptr==NUMCOLS) zmabend("too many columns");
	    c_field[cptr++][lptr] = (char)0;
	    }
	 if (c=='=' && function[i+1]!='=')
	    {
	    savvec[savptr++] = ksv;
	    }
	 }
      snum = 0;
      lptr = 0;
      nexti: continue;
      }
   
   /* read in the columns NEED LOGIC FOR STRINGS*/

   nincol = cptr;
   for (i=0;i<nincol;i++)
      {
      datcols[i] = atoi(&c_field[i][1]);
      
      status = IBISColumnGet(ibis,"FORMAT",fmtstring,datcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      if (fmtstring[0]=='A')
         {
         wid[i] = ms_num(&fmtstring[1])+1;
         typ[i] = 0;
         }
      else
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datcols[i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnGet(ibis,"U_SIZE",&wid[i],datcols[i]);
         if (status!=1) IBISSignal(ibis,status,1);
         typ[i] = 8;
         }
      wmx = wid[i]*tablen;
      totwid[i+1] = totwid[i]+((wmx+7)/8)*8;
      }
   
   mz_alloc1((unsigned char **)&c_data,totwid[nincol],1);
   for (i=1;i<=ncol;i++)
      for (j=0;j<nincol;j++)
	 if (datcols[j]==i)
	    {
	    status = IBISColumnRead(ibis,&c_data[totwid[j]],i,1,tablen);
            if (status!=1) IBISSignal(ibis,status,1);
            }
   
   /* iterate over functions separated by $, call
      knuth to parse and compile the function,
      and call xknuth to execute the function */

   srand48((long int)seed); savptr = 0;
   i = strlen(function);
   function[i] = '$';
   function[i+1] = (char)0;
   r = &function[0];
   for (ibig=0;;ibig++)
      {
      p = r;
      if (*p=='@')    /* this section traps column ops */
	{ /***/
	 if (strncasecmp(p+1,"shift",5)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    n = ms_num(r);
	    for (i=0;i<tablen;i++)
	       {
	       ii = i; if (n>=0) ii = tablen-i-1;
	       iii = ii-n;
	       if (iii>=tablen) iii=tablen-1;
	       if (iii<0) iii=0;
	       k1 = totwid[js]+ii*wid[js];
	       k2 = totwid[js]+iii*wid[js];
	       for (l=0;l<wid[js];l++) c_data[k1+l] = c_data[k2+l];
	       }
	    }
	 if (strncasecmp(p+1,"rot",3)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    n = ms_num(r);
	    icount = 1;
	    for (ist=0;ist<tablen;ist++)
	       {
	       ii = ist;
	       k1 = totwid[js]+ii*wid[js];
	       for (l=0;l<wid[js];l++) c_tmp[l] = c_data[k1+l];
	       for (i=0;i<tablen;i++)
		  {
		  iii = (ii-n+tablen)%tablen;
		  k1 = totwid[js]+ii*wid[js];
		  k2 = totwid[js]+iii*wid[js];
		  if (iii==ist) break;
		  for (l=0;l<wid[js];l++) c_data[k1+l] = c_data[k2+l];
		  ii = iii; icount++;
		  }
	       for (l=0;l<wid[js];l++) c_data[k1+l] = c_tmp[l];
	       icount++; if (icount>=tablen) break;
	       }
	    }
	 if (strncasecmp(p+1,"cdiff",5)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    ldiff = 0.; pval = 0.;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],c_data);
	       if (val!=pval) ldiff = 0.;
	       ndiff = ffetchcd(totwid[js]+i*wid[js],typ[js],c_data);
	       diff = ndiff-ldiff;
	       fstorecd(totwid[js]+i*wid[js],typ[js],diff,c_data);
	       pval = val; ldiff = ndiff;
	       }
	    }
	 if (strncasecmp(p+1,"crsum",5)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    sum = 0.; pval = 0.;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],c_data);
	       if (val!=pval) sum = 0.;
	       sum += ffetchcd(totwid[js]+i*wid[js],typ[js],c_data);
	       fstorecd(totwid[js]+i*wid[js],typ[js],sum,c_data);
	       pval = val;
	       }
	    }
	 if (strncasecmp(p+1,"csum",4)==0 || strncasecmp(p+1,"cvmin",5)==0 ||
	     strncasecmp(p+1,"cvmax",5)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    isv = 0; sum = 0.;
	    if (tablen>0)
	       {
	       pval = ffetchcd(totwid[jt],typ[jt],c_data);
	       vmin = ffetchcd(totwid[js],typ[js],c_data);
	       vmax = vmin;
	       }
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],c_data);
	       val0 = ffetchcd(totwid[js]+i*wid[js],typ[js],c_data);
	       if (val==pval)
		  {
		  sum += val0;
		  if (val0>vmax) vmax = val0;
		  if (val0<vmin) vmin = val0;
		  if (i<tablen-1) continue;
		  }
	       if (strncasecmp(p+1,"cvmin",5)==0 ) sum = vmin;
	       if (strncasecmp(p+1,"cvmax",5)==0 ) sum = vmax;
	       if (val==pval&&i==tablen-1) i++;
	       for (j=isv;j<i;j++)
		  fstorecd(totwid[js]+j*wid[js],typ[js],sum,c_data);
	       if (val!=pval&&i==tablen-1)
		  fstorecd(totwid[js]+i*wid[js],typ[js],val0,c_data);
	       isv = i; sum = val0; vmin = val0; vmax = val0; pval = val;
	       }
	    }
	 if (strncasecmp(p+1,"interp",6)==0 || strncasecmp(p+1,"fill",4)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    val0 = ffetchcd(totwid[js],typ[js],c_data);
	    for (ist=0;ist<tablen;ist++)
	       {
	       for (ii=ist+1;ii<tablen;ii++)
		  {
		  val2 = ffetchcd(totwid[js]+ii*wid[js],typ[js],c_data);
		  if (val2!=0.) break;
		  }
	       cmp0 = ffetchcd(totwid[jt]+ist*wid[jt],typ[jt],c_data);
	       cmp2 = ffetchcd(totwid[jt]+ii*wid[jt],typ[jt],c_data);
	       dcmp = cmp2-cmp0;
	       if (dcmp<1.e-6 && dcmp>=0.) dcmp = 1.e-6;
	       if (dcmp>(-1.e-6) && dcmp<=0.) dcmp = -1.e-6;
	       dcmp = 1./dcmp;
	       for (i=ist+1;i<ii;i++)
		  {
		  cmp1 = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],c_data);
		  if (strncasecmp(p+1,"fill",4)==0)
		     val1 = val0;
		  if (strncasecmp(p+1,"interp",6)==0)
		     val1 = (cmp1-cmp0)*(val2-val0)*dcmp+val0;
		  fstorecd(totwid[js]+i*wid[js],typ[js],val1,c_data);
		  }
	       val0 = val2;
	       ist = ii-1; if (ist==tablen-2) break;
	       }
	    }
	 if (strncasecmp(p+1,"sum",3)==0  || strncasecmp(p+1,"av",2)==0
	    || strncasecmp(p+1,"sig",3)==0
	    || strncasecmp(p+1,"vmin",4)==0  || strncasecmp(p+1,"vmax",4)==0
	    || strncasecmp(p+1,"rsum",4)==0  || strncasecmp(p+1,"diff",4)==0)
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    sum = 0.; pval = 0.; vmin = 1.0e30; vmax = -1.0e30;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[js]+i*wid[js],typ[js],c_data);
	       if (val>vmax) vmax = val;
	       if (val<vmin) vmin = val;
	       sum += val;
	       if (strncasecmp(p+1,"rsum",4)==0)
		  fstorecd(totwid[js]+i*wid[js],typ[js],sum,c_data);
	       if (strncasecmp(p+1,"diff",4)==0)
		  fstorecd(totwid[js]+i*wid[js],typ[js],val-pval,c_data);
	       pval = val;
	       }
	    vout = sum;
	    if (strncasecmp(p+1,"av",2)==0 && tablen!=0) vout = sum/tablen;
	    if (strncasecmp(p+1,"sig",3)==0 && tablen!=0)
	       {
	       mean = sum/tablen; ssq = 0;
	       for (i=0;i<tablen;i++)
		  {
		  val = ffetchcd(totwid[js]+i*wid[js],typ[js],c_data)-mean;
		  ssq += val*val;
		  }
	       vout = sqrt(ssq/(tablen-1));
	       }
	    if (strncasecmp(p+1,"vmin",4)==0 ) vout = vmin;
	    if (strncasecmp(p+1,"vmax",4)==0 ) vout = vmax;
	    if (strncasecmp(p+1,"sum",3)==0  || strncasecmp(p+1,"av",2)==0
		  || strncasecmp(p+1,"sig",3)==0
		  || strncasecmp(p+1,"vmin",4)==0  || strncasecmp(p+1,"vmax",4)==0)
	       for (i=0;i<tablen;i++)
		  fstorecd(totwid[js]+i*wid[js],typ[js],vout,c_data);
	    }
	 if (strncasecmp(p+1,"dist",4)==0)
	    {
	    q = index(p,'(')+1;
	    j1 = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    j2 = mtchfield(r,c_field,nincol);
	    q = index(r,',')+1;
	    j3 = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    j4 = mtchfield(r,c_field,nincol);
	    q = index(r,',')+1;
	    j5 = mtchfield(q,c_field,nincol);
	    raddeg = 1./degrad;  mpr = 1.1132e5*degrad;
	    val1 = 0.; val2 = 0.;
	    for (i=0;i<tablen;i++)
	       {
	       tht1 = ffetchcd(totwid[j1]+i*wid[j1],typ[j1],c_data)*raddeg;
	       phi1 = ffetchcd(totwid[j2]+i*wid[j2],typ[j2],c_data)*raddeg;
	       tht2 = ffetchcd(totwid[j3]+i*wid[j3],typ[j3],c_data)*raddeg;
	       phi2 = ffetchcd(totwid[j4]+i*wid[j4],typ[j4],c_data)*raddeg;
	       rdist = (fabs(tht1-tht2)+fabs(phi1-phi2))*10.*degrad;
	       if (rdist>9.5)
		  {
		  p1 = sin(phi1)*sin(phi2);
		  q1 = cos(phi1)*cos(phi2)*cos(tht1-tht2);
		  val1 = acos(MAX(MIN((p1+q1),1.0),-1.0))*degrad*60.*1851.984;
		  }
	       if (rdist<10.5) /*adjust for utm at 500k,0 */
		  {
		  p1 = (phi2-phi1)*mpr/1.007146960651;
		  q1 = (tht2-tht1)*mpr*cos((phi1+phi2)*.5)/1.000404734947;
		  val2 = sqrt(p1*p1+q1*q1);
		  }
	       val = (rdist-9.5)*val1+(10.5-rdist)*val2;
	       if (rdist<9.5) val = val2;
	       if (rdist>10.5) val = val1;
	       fstorecd(totwid[j5]+i*wid[j5],typ[j5],val,c_data);
	       }
	    }
	 if (strncasecmp(p+1,"head",4)==0 || strncasecmp(p+1,"bear",4)==0)
	    {
	    q = index(p,'(')+1;
	    j1 = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    j2 = mtchfield(r,c_field,nincol);
	    q = index(r,',')+1;
	    j3 = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    j4 = mtchfield(r,c_field,nincol);
	    q = index(r,',')+1;
	    j5 = mtchfield(q,c_field,nincol);  j7 = j5;
	    if (strncasecmp(p+1,"bear",4)==0)
	       {
	       r = index(q,',')+1;
	       j6 = mtchfield(r,c_field,nincol);
	       q = index(r,',')+1;
	       j7 = mtchfield(q,c_field,nincol);
	       }
	    raddeg = 1./degrad; val1 = 0; val2 = 0;
	    for (i=0;i<tablen;i++)
	       {
	       tht1 = ffetchcd(totwid[j1]+i*wid[j1],typ[j1],c_data)*raddeg;
	       phi1 = ffetchcd(totwid[j2]+i*wid[j2],typ[j2],c_data)*raddeg;
	       tht2 = ffetchcd(totwid[j3]+i*wid[j3],typ[j3],c_data)*raddeg;
	       phi2 = ffetchcd(totwid[j4]+i*wid[j4],typ[j4],c_data)*raddeg;
	       if (strncasecmp(p+1,"bear",4)==0)
		  {
		  tht3 = ffetchcd(totwid[j5]+i*wid[j5],typ[j5],c_data)*raddeg;
		  phi3 = ffetchcd(totwid[j6]+i*wid[j6],typ[j6],c_data)*raddeg;
		  }
	       else { tht3 = tht1; phi3 = 90.*raddeg; }
	       if (phi1==phi2 && tht1==tht2) {val = 0.; goto storval;}
	       rdist = (fabs(tht1-tht2)+fabs(phi1-phi2))*10.*degrad;
    if (rdist>9.5)
       {
	       p1=cos(phi1)*cos(tht1);p2=cos(phi1)*sin(tht1);p3=sin(phi1);
	       q1=cos(phi2)*cos(tht2);q2=cos(phi2)*sin(tht2);q3=sin(phi2);
	       n1=cos(phi3)*cos(tht3);n2=cos(phi3)*sin(tht3);n3=sin(phi3);
	       if (n3>=.99999)
		  {
		  if (p3>=.99999) { val = 180.; goto storval;}
		  if (p3<=-.99999) { val = 0.; goto storval;}
		  }
	       if (p1==-q1 && p2==-q2 && p3==-q3) { val = 90.; goto storval;}
	       ndq = n1*q1+n2*q2+n3*q3;
	       pdn = p1*n1+p2*n2+p3*n3;
	       pdq = p1*q1+p2*q2+p3*q3;
	       phi = acos(MAX(MIN(((ndq-pdn*pdq)/
		     sqrt(MAX((1.-pdn*pdn)*(1.-pdq*pdq),0.0))),1.0),-1.0));
	       pxn1 = p2*n3-p3*n2;
	       pxn2 = p3*n1-p1*n3;
	       pxn3 = p1*n2-p2*n1;
	       pxq1 = p2*q3-p3*q2;
	       pxq2 = p3*q1-p1*q3;
	       pxq3 = p1*q2-p2*q1;
	       pxndq = pxn1*q1+pxn2*q2+pxn3*q3;
	       if (pxndq<0.) val = phi*degrad;
	       if (pxndq>0.) val = 360.-phi*degrad;
	       if (pxndq==0.)
		  {
		  pxnpxq = pxn1*pxq1+pxn2*pxq2+pxn3*pxq3;
		  if (pxnpxq>0.) val = 0.; else val = 180.;
		  }
	       storval: val1 = val;
       }
    if (rdist<10.5)
       {
	       q1 = phi1-phi2;
	       q2 = (tht1-tht2)*cos((phi1+phi2)*.5);
	       q3 = phi1-phi3;
	       q4 = (tht1-tht3)*cos((phi1+phi3)*.5);
	       if (q1!=0.||q2!=0.) p1 = atan2(q1,q2); else p1 = 0.;
	       if (q3!=0.||q4!=0.) p2 = atan2(q3,q4); else p2 = 0.;
	       val2 = (p2-p1)*degrad;
	       if (val2<0.) val2 += 360.;
       }
	       val = (rdist-9.5)*val1+(10.5-rdist)*val2;
	       if (rdist<9.5) val = val2;
	       if (rdist>10.5) val = val1;
	       fstorecd(totwid[j7]+i*wid[j7],typ[j7],val,c_data);
	       }
	    }
	 r = index(q,'$');
	 do r++; while (*r=='$');
	 if (strlen(r)==0) break;
	 continue;
	}

      q = index(p,'=')+1;
      r = index(q,'$');
      do r++; while (*r=='$');
      rcol = savvec[savptr++];
      sptr = 0;
      sp_knuth(q,ibuf,dbuf,sbuf,cnum,&sptr);
      if (idebug) printf("\n");
 
      svsptr = sptr;

      /* rand() changed to drand48() */

      for (i=0;i<tablen;i++)
	 {
	 sptr = svsptr;
	 for (j=0;j<nincol;j++)
	    {
	    if (typ[j]!=0)
	       dbuf[j] = ffetchcd(totwid[j]+i*wid[j],typ[j],c_data);
	    else
	       {
	       dbuf[j] = (double)sptr;
	       /*bcopy(&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],wid[j]);*/
	       zmve(1,wid[j],&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],1,1);
	       sptr += wid[j]+1;
	       sbuf[sptr-1] = (char)0;
	       }
	    }
	 dbuf[102] = drand48();
	 dbuf[101] = (double)(i+1);
	 debugrec1 = i==0;
	 sp_xknuth(ibuf,dbuf,sbuf,&sptr,&res);
	 if (typ[rcol]!=0)
	    fstorecd(totwid[rcol]+i*wid[rcol],typ[rcol],res,c_data);
	 else
	    {
	    ires = (int)(res+.001);
	    lres = MIN(strlen(&sbuf[ires]),wid[rcol]);
	    /*bcopy(&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],lres);*/
	    zmve(1,lres,&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],1,1);
  /*bcopy(blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],wid[rcol]-lres);*/
  zmve(1,wid[rcol]-lres-1,blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],1,1);
  zmve(1,1,zeros,&c_data[totwid[rcol]+(i+1)*wid[rcol]-1],1,1);
	    }
	 }
      if (strlen(r)==0) break;
      }

   /* write the result to the file */

   for (i=1;i<=ncol;i++)
      for (j=0;j<nincol;j++)
	 if (datcols[j]==i&&tablen>0)
	    {
	    status = IBISColumnWrite(ibis,&c_data[totwid[j]],i,1,tablen);
            if (status!=1) IBISSignal(ibis,status,1);
            }

   /* print in/out file lengths */
   
   printf("%d records in\n",tablen);
   
   /* close files */
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   return;
       
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mf3.imake
#define  PROGRAM   mf3

#define MODULE_LIST mf3.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL
#define LIB_FORTRAN

/*#define LIB_LOCAL	/* Comment this out before delivery.  */
/*#define DEBUG		/* Comment this out before delivery.  */
$ Return
$!#############################################################################
$PDF_File:
$ create mf3.pdf
PROCESS        HELP=*
! MF3 PDF - VICAR/IBIS SOFTWARE
PARM INP TYPE=STRING
PARM FUNCTION TYPE=(STRING,250),COUNT=(1:40)
PARM SEED  TYPE=INTEGER DEF=0
PARM DEBUG TYPE=INTEGER DEF=0
END-PROC
.TITLE
VICAR/IBIS Program MF3
.HELP
PURPOSE

MF3   allows   the  user  to  create C -like 
expressions to perform general mathematical operations on 
one  or more IBIS/graphics file columns.   The  expressions 
are  written as a parameter string.   The parameter  is 
interpreted  to determine the input and output  columns 
and   operations  to  be  performed.    Applies a user 
specified arithmetic expression to columns of a cagis table.
All results are computed in double precision (15 decimal
places) even if the input columns are single precision or
integer.

The functions available are: @sqrt, @alog, @alog10,
@aint, @sin, @cos, @tan, @asin, @acos, @atan,
@atan2, @abs, @min, @max, @mod, along with
standard binary operations +,- *, / and ^ (pow), and
logic operations <, >, <=, >=, ==, !=, && (and), || (or),
and ! (not).  The main difference with C is the use
of ^ for power of two integers or reals is implemented;
and ^^ (xor) is not implemented (use (a||b)&&(!(a&&b))).

The old FORTRAN constructs .EQ. etc. and also **
are no longer allowed.

String functions are also available.  The arguments
can be column names (must contain strings) or string
constants enclosed in single quotes, except for some arguments
which are numeric (e.g., see fstr below).  Examples:
	      @cat(a,b)  or @cat(a,'xxx')

The string functions are:

@cat(a,b)                concatenates a to b

@break(a,b)              outputs a up to first occurrence of a
			 character in b   (e.g., @break(a,'.,;:?'))

@fstr(a,m)               outputs the first m characters of a

@bstr(a,m)               outputs from the m'th character to the end of a

@adelete(a,b)            deletes any of b's characters from a
			 (e.g., @adelete(a,'.,;:?'))

@sdelete(a,b)            deletes occurrences of the whole string b from a
			 (e.g., @sdelete(a,'dog')

@trim(a,b)               trims from the low order end of a, all characters
			 in b, but stops trimming at the first non-b char

@ucase(a)                outputs a in upper case

@lcase(a)                outputs a in lower case

@ljust(a,n)              left justifies a in an n-character field.  if too
			 long, keeps high order part of a

@rjust(a,n)              right justifies a in an n-character field.  if too
			 long, keeps high order part of a

@replace(a,'dog=cat')    replaces all occurrences of the string before the
			 = with the string after the =

@strlen(a)               outputs the length of the string a

@pos(a,b)                finds the pattern b in a and returns its starting
			 position.  ^ is left anchor % is right anchor
			 ? matches any single character * matches a run
			 (e.g., @pos(a,'^a??.*z*%'))

@streq(a,b)              returns TRUE or 1 if a equals b else FALSE or 0

@strsub(a,b)             returns TRUE or 1 if a contains b else FALSE or 0

@strpat(a,b)             returns TRUE or 1 if a contains the pattern b
			 else FALSE or 0.  see the syntax for @pos(a,b)

@num(a)                  returns the numeric value of string a, which must
			 contain an integer or floating number, can have exponent
			 such as 2.73e-06 (use e, E, d, or D).

@i2str(n)                converts the integer n to a string; zero goes to 0

@f2str(f,n)              converts the float or integer f to a floating
			 string with n digits of precision to the right of
			 the decimal; n=0 omits the decimal; rounding is
			 performed

@dmsstr(a)               converts the string degree-min-second into a
                         degree number.   Acceptable formats include
                         1332727.666, 1332727.666W, 1.332727E+06W,
                         133d27m27.666 where the - can be any non-numeric
                         separator other than .+-Ee.  The EWNS can be
                         lower case and can be at the front e1332727.666.
                         A minor point, exponent e or E can be followed
                         by a number or a sign, but d or D must be
                         followed by a sign.

@dmsnum(f)               converts the number degree-min-second into a
                         degree number.   Acceptable formats include
                         1332727.666, -1332727 (real or integer).

All operations work as in the c language with
except for the column operations described below.
The special variable @index may be used to insert
the record number into an expression.  The special
variable @rand may be used to put a random number
between 0 and 1 in the column.  If @rand is used, the
parameter seed can be used to vary the random sequence
Multiple formulas may be given by separating them with the
$ character.

Column operations are added features that perform
specialized functions to the table.  Two restrictions
must be observed:

1. Column operations cannot be used in a formula.
2. The arguments must be column names, not constants
   or expressions.

They perform an operation on columns placing
results in a column.  The operations @fill and
@interp require a column of values separated by
zeros.

The column operations are:

@average(col)           calculates the average in
			the column

@sigma(col)             calculates the standard deviation in
			the column

@sum(col)               sum the values in the column

@rsum(col)              running sum of values in the
			column

@csum(col1,col2)        controlled sum; sum the values
			in col1 using col2 as a control
			column, restarts the sum for a
			change in the value in col2

@crsum(col1,col2)       controlled running sum; running
			sum of values in col1 but restarts
			the sum for a change in the value
			in col2

@vmax(col)              calculates the maximum in the column

@vmin(col)              calculates the minimum in the column

@cvmax(col1,col2)       controlled maximum; calculates the
			maximum in col1 using col2 as a control
			column, restarts the max for a change
			in the value in col2

@cvmin(col1,col2)       controlled minimum; calculates the
			minimum in col1 using col2 as a control
			column, restarts the min for a change
			in the value in col2

@diff(col)              subtracts the value in the previous
			record from the value in the current
			record

@cdiff(col1,col2)       subtracts the value in the previous
			record from the value in the current
			record; restarts the operation for a
			change in the value in col2

@shift(col,n)           shifts downward n records,
			negative n for upward shift;
			downward shift replicates first
			value in column while upward
			shift replicates last value

@rotate(col,n)          same as shift except values that
			are rotated off the end of the
			column are wrapped around to the
			other end

@interp(col1,col2)      replace zero values between non-zero
			values in col1 by interpolating
			between the non-zero values in col1
			to corresponding values in col2;
			col2 may contain @index in which case
			interpolation is linear or it may
			contain some other function
			(i.e. logarithmic or exponential)

@fill(col)              fill the zeros in the column with the
			previous non-zero value in the column

@dist(lon1,lat1,lon2,lat2,dist)     calculate the distance in meters
				    between the two geographic points
				    on the Earth.  A spherical formula
				    is used above 1.05 degrees and a
				    plane formula is used below .95
				    degrees of central arc.  Between
				    these values, both formulas are used
				    and the result is a linear
				    interpolation of both formulas.
				    This is done to give a continuous
				    result.  Results near the poles
				    are not guaranteed accurate.

@head(lon1,lat1,lon2,lat2,head)     calculate the heading of the line
				    from the first to the second point
				    in degrees clockwise from north.
				    The interpolation technique used
				    in @dist is applied here.

@bear(lon1,lat1,lon2,lat2,lon3,lat3,bear) calculate the bearing of the
					  line from the first to the
				    second point clockwise in degrees
				    from the line from the first to the
				    third point.  The interpolation
				    technique used in @dist is
				    applied here.


A full example of an fstring to calculate a
time increment dt from a column t is
fstring="dt=t$shift(dt,-1)$dt=t-dt"

TAE COMMAND LINE FORMAT

     MF3 INP=int PARAMS

     where

     int                 is a random access file.  Since it
                         is used for both input and  output, 
                         no output file is specified.

     PARAMS              is   a  standard  VICAR   parameter 
                         field.
.PAGE
METHOD

     MF3 performs arithmetic operations on an interface file.  
     The  program  uses  two  library  routines SP_KNUTH  and 
     SP_XKNUTH,   to   compile  and  interpret  C-like 
     expressions  entered by the parameters in an expression 
     such as:

                     C135 = (100*C34)/C4

     In this expression,  C34 and C4 are the input  columns.  
     SP_KNUTH    compiles   the   expression   into  pseudo-machine 
     instructions.   The  expression is applied to the input 
     column in SP_XKNUTH to produce the output column, C135.


RESTRICTIONS

1.     Maximum number of columns in one execution is 100. (oops, 9 until bug is fixed)
2.     The number of columns in the IBIS file is not limited here.
3.     Maximum input string length is 10,000 (40 x 250).
4.     Maximum number of operations is 3000.
5.     Maximum number of temp locations is 938.
6.     Maximum number of constants from the expression is 960.

notes:

1.  Column numbers greater than 100 are mapped sequentially 1,2,3...
    so there is no limit on the number of columns in the IBIS file.
3.  The input parameter is a string array (40) each with 250 chars.
    The array is concatenated by the program into a single array.
4.  These can be counted by setting debug to one and counting the
    lines that begin with "xknuth:op,opnd".  The count is not
    easily determined by looking at a long input.
5.  These can be counted by setting debug to one and counting the
    lines that begin with "xknuth:op,opnd" and having an opnd
    value above 1061.  The count is not easily determined by
    looking at a long input.
6.  These can be counted in the input, or by setting debug to one
    and counting the lines that begin with "xknuth:op,opnd" and
    having an opnd value between 103 and 1061, inclusive.
.PAGE
EXAMPLE

     MF3 INP=FILE.INT FUNCTION=("C5 = C2/C3+100+@SQRT(C2)")

     In this example,  C2 is divided by C3 and added to  100 
     plus the square root of C2.   The results are placed in 
     C5.  Further examples of allowable functions follow:

                FUNCTION=("C5 = !(C3  || C2)")

     logical   operations  are  performed  bitwise  on   the 
     operands. The  logical values T and F are converted to 1.  and 0. 
     for storage in column C5

                FUNCTION=("X5 = X3<=INDEX")

     Column 5 is 1.0 if column 3 has a value < its row value (INDEX).
     
                FUNCTION=("@average(C3)")

     In this example, the mean of column 3 is calculated and 
     that  value is placed in every row entry in  column  3.  
     This  operation  is different than the  arithmetic  and 
     logic operations given earlier because it operates on a 
     vertical  column instead of horizontally across a  row.  
     These  operations  cannot  be  used  in  an  arithmetic 
     expression  such as C5 = @average(C3)*10.   See the FUNCTION
     help for more examples.

Original Programmer:  A. L. Zobrist, 15 December 1976

Cognizant Programmer:  N. D. Ritter

Revision:
  12 December 1999 Double precision and strings, etc. A. L. Zobrist
  06 February 2000 Enlarge all Function restrictions  A. L. Zobrist
  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup


.LEVEL1
.VARIABLE INP
Input IBIS interface file
.VARIABLE FUNCTION
Specifies function and columns,
case insensitive
.VARIABLE SEED
Use to vary the random sequence
.VARIABLE DEBUG
Set 1 to see symbol fetch and
compiled expression
.LEVEL2
.VARIABLE INP
                        Specifies IBIS interface file. There
                        is no output file. Results of MF3 are
                        written in INP.
.VARIABLE FUNCTION
     FUNCTION            
	 
MF3   allows   the  user  to  create  FORTRAN or C -like 
expressions to perform general mathematical operations on 
one  or more IBIS/graphics file columns.   The  expressions 
are  written as a parameter string.   The parameter  is 
interpreted  to determine the input and output  columns 
and   operations  to  be  performed.    Applies a user 
specified arithmetic expression to columns of a cagis table.
All results are computed in double precision (15 decimal
places) even if the input columns are single precision or
integer.

The functions available are: @sqrt, @alog, @alog10,
@aint, @sin, @cos, @tan, @asin, @acos, @atan,
@atan2, @abs, @min, @max, @mod, along with
standard binary operations +,- *, / and **, and
logic operations <, >, <=, >=, ==, !=, &&, ||,
^, and !.

String functions are also available.  The arguments
can be column names (must contain strings) or string
constants enclosed in single quotes, except for some arguments
which are numeric (e.g., see fstr below).  Examples:
	      @cat(a,b)  or @cat(a,'xxx')

The string functions are:

@cat(a,b)                concatenates a to b

@break(a,b)              outputs a up to first occurrence of a
			 character in b   (e.g., @break(a,'.,;:?'))

@fstr(a,m)               outputs the first m characters of a

@bstr(a,m)               outputs from the m'th character to the end of a

@adelete(a,b)            deletes any of b's characters from a
			 (e.g., @adelete(a,'.,;:?'))

@sdelete(a,b)            deletes occurrences of the whole string b from a
			 (e.g., @sdelete(a,'dog')

@trim(a,b)               trims from the low order end of a, all characters
			 in b, but stops trimming at the first non-b char

@ucase(a)                outputs a in upper case

@lcase(a)                outputs a in lower case

@ljust(a,n)              left justifies a in an n-character field.  if too
			 long, keeps high order part of a

@rjust(a,n)              right justifies a in an n-character field.  if too
			 long, keeps high order part of a

@replace(a,'dog=cat')    replaces all occurrences of the string before the
			 = with the string after the =

@strlen(a)               outputs the length of the string a

@pos(a,b)                finds the pattern b in a and returns its starting
			 position.  ^ is left anchor % is right anchor
			 ? matches any single character * matches a run
			 (e.g., @pos(a,'^a??.*z*%')).  There is a limit of 
			 three * in a pattern.  Also, patterns with a *
			 will find the shortest match, not the first match.

@streq(a,b)              returns TRUE or 1 if a equals b else FALSE or 0

@strsub(a,b)             returns TRUE or 1 if a contains b else FALSE or 0

@strpat(a,b)             returns TRUE or 1 if a contains the pattern b
			 else FALSE or 0.  see the syntax for @pos(a,b)

@num(a)                  returns the numeric value of string a, which must
			 contain an integer or floating number, can have exponent
			 such as 2.73e-06 (use e, E, d, or D).

@i2str(n)                converts the integer n to a string; zero goes to 0

@f2str(f,n)              converts the float or integer f to a floating
			 string with n digits of precision to the right of
			 the decimal; n=0 omits the decimal; rounding is
			 performed

@dmsstr(a)               converts the string degree-min-second into a
                         degree number.   Acceptable formats include
                         1332727.666, 1332727.666W, 1.332727E+06W,
                         133d27m27.666 where the - can be any non-numeric
                         separator other than .+-Ee.  The EWNS can be
                         lower case and can be at the front e1332727.666.
                         A minor point, exponent e or E can be followed
                         by a number or a sign, but d or D must be
                         followed by a sign.

@dmsnum(f)               converts the number degree-min-second into a
                         degree number.   Acceptable formats include
                         1332727.666, -1332727 (real or integer).

See the test pdf for more examples of the use of strings,
excerpted here:

!ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
!      data=(0.00001) datacols=(3) +
!      string=("aabbccddee") strcols=(1)

! use any one line of the next bunch

!mf3 xx1 f="c2='b'"
!mf3 xx1 f="c2=@trim(c1,'e')"
!mf3 xx1 f="c2=@break(c1,'db')"
!mf3 xx1 f="c2=@fstr(c1,7)"
!mf3 xx1 f="c2=@bstr(c1,7)"
!mf3 xx1 f="c2=@adelete(c1,'bd')"
!mf3 xx1 f="c2=@sdelete(c1,'bb')"
!mf3 xx1 f="c2=@sdelete(c1,'bc')"   
!mf3 xx1 f="c2=@replace(c1,'bb=qqq')"
!mf3 xx1 f="c2=@replace(c1,'bc=qqq')"
!mf3 xx1 f="c3=@strlen(c1)"
!mf3 xx1 f="c3=@strlen('abc')"
!mf3 xx1 f="c3=@streq(c1,'aabbccddee')"
!mf3 xx1 f="c3=@streq(c1,'aabbccddeef')"
!mf3 xx1 f="c3=@strsub(c1,'bb')"
!mf3 xx1 f="c3=@strsub(c1,'bc')"
!mf3 xx1 f="c2=@ljust(c1,12)"
!mf3 xx1 f="c2=@rjust(c1,12)"
!mf3 xx1 f="c3=@num('23456')"
!mf3 xx1 f="c3=@num('23456.7890123')"
!mf3 xx1 f="c2=@i2str(1234567890)"
!mf3 xx1 f="c2=@i2str(-75)"
!mf3 xx1 f="c2=@f2str(47.55555,2)"
!mf3 xx1 f="c3=@pos(c1,'bb')"
!mf3 xx1 f="c3=@pos(c1,'bc')"
!mf3 xx1 f="c3=@pos(c1,'^a')"
!mf3 xx1 f="c3=@pos(c1,'b*d')"
!mf3 xx1 f="c3=@pos(c1,'e%')"
!mf3 xx1 f="c3=@pos(c1,'b?c')"
!mf3 xx1 f="c2=@strpat(c1,'bbc')"
!mf3 xx1 f="c2=@strpat(c1,'^a')"
!mf3 xx1 f="c2=@strpat(c1,'b*d')"
!mf3 xx1 f="c2=@strpat(c1,'e%')"
!mf3 xx1 f="c2=@strpat(c1,'b?c')"

!ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"


All operations work as in the c language with
except for the column operations described below
and the ^ operator noted above.
The special variable @index may be used to insert
the record number into an expression.  The special
variable @rand may be used to put a random number
between 0 and 1 in the column.  If @rand is used, the
parameter seed can be used to vary the random sequence
Multiple formulas may be given by separating them with the
$ character.

Column operations are added features that perform
specialized functions to the table.  Two restrictions
must be observed:

1. Column operations cannot be used in a formula.
2. The arguments must be column names, not constants
   or expressions.

They perform an operation on columns placing
results in a column.  The operations @fill and
@interp require a column of values separated by
zeros.

The column operations are:

@average(col)           calculates the average in
			the column

@sigma(col)             calculates the standard deviation in
			the column

@sum(col)               sum the values in the column

@rsum(col)              running sum of values in the
			column

@csum(col1,col2)        controlled sum; sum the values
			in col1 using col2 as a control
			column, restarts the sum for a
			change in the value in col2

@crsum(col1,col2)       controlled running sum; running
			sum of values in col1 but restarts
			the sum for a change in the value
			in col2

@vmax(col)              calculates the maximum in the column

@vmin(col)              calculates the minimum in the column

@cvmax(col1,col2)       controlled maximum; calculates the
			maximum in col1 using col2 as a control
			column, restarts the max for a change
			in the value in col2

@cvmin(col1,col2)       controlled minimum; calculates the
			minimum in col1 using col2 as a control
			column, restarts the min for a change
			in the value in col2

@diff(col)              subtracts the value in the previous
			record from the value in the current
			record

@cdiff(col1,col2)       subtracts the value in the previous
			record from the value in the current
			record; restarts the operation for a
			change in the value in col2

@shift(col,n)           shifts downward n records,
			negative n for upward shift;
			downward shift replicates first
			value in column while upward
			shift replicates last value

@rotate(col,n)          same as shift except values that
			are rotated off the end of the
			column are wrapped around to the
			other end

@interp(col1,col2)      replace zero values between non-zero
			values in col1 by interpolating
			between the non-zero values in col1
			to corresponding values in col2;
			col2 may contain @index in which case
			interpolation is linear or it may
			contain some other function
			(i.e. logarithmic or exponential)

@fill(col)              fill the zeros in the column with the
			previous non-zero value in the column

@dist(lon1,lat1,lon2,lat2,dist)     calculate the distance in meters
				    between the two geographic points
				    on the Earth.  A spherical formula
				    is used above 1.05 degrees and a
				    plane formula is used below .95
				    degrees of central arc.  Between
				    these values, both formulas are used
				    and the result is a linear
				    interpolation of both formulas.
				    This is done to give a continuous
				    result.  Results near the poles
				    are not guaranteed accurate.

@head(lon1,lat1,lon2,lat2,head)     calculate the heading of the line
				    from the first to the second point
				    in degrees clockwise from north.
				    The interpolation technique used
				    in @dist is applied here.

@bear(lon1,lat1,lon2,lat2,lon3,lat3,bear) calculate the bearing of the
					  line from the first to the
				    second point clockwise in degrees
				    from the line from the first to the
				    third point.  The interpolation
				    technique used in @dist is
				    applied here.


A full example of an fstring to calculate a
time increment dt from a column t is
fstring="dt=t$shift(dt,-1)$dt=t-dt"

RESTRICTIONS

1.     Maximum number of columns in one execution is 100.
2.     The number of columns in the IBIS file is not limited here.
3.     Maximum input string length is 10,000 (40 x 250).
4.     Maximum number of operations is 3000.
5.     Maximum number of temp locations is 938.
6.     Maximum number of constants from the expression is 960.

notes:

1.  Column numbers greater than 100 are mapped sequentially 1,2,3...
3.  The input parameter is a string array (40) each with 250 char
    The array is concatenated by the program, be careful to 
    distinguish the TAE-TCL continuation + from function + by using
    quotes around each member of the array.
4.  These can be counted by setting debug to one and counting the
    lines that begin with "xknuth:op,opnd".  The count is not
    easily determined by looking at a long input.
5.  These can be counted by setting debug to one and counting the
    lines that begin with "xknuth:op,opnd" and having an opnd
    value above 1061.  The count is not easily determined by
    looking at a long input.
6.  These can be counted in the input, or by setting debug to one
    and counting the lines that begin with "xknuth:op,opnd" and
    having an opnd value between 103 and 1061, inclusive.
.VARIABLE SEED

Suppose that mf3 is used to set two columns to random values, or
a function involving a random values, in two executions of mf3.
Since the same random sequence would be used, the columns
would correlate.  To avoid this, use different values of seed
in the two executions of mf3 to get different random sequences
(actually two subsequences of a very long equirandom sequence).
For more on the math, see the SUN documentation on srand48.
.VARIABLE DEBUG
The symbols are read in a left to right parse.  Look at the 
code in sp_knuth and its subroutines to interpret.  The operations
are listed in sp_xknuth.  Operands are:

0-100     columns (mapped 0,1,2,3... regardless of actual columns)
101       random number
102       row index
103-1061  constants from the expression, or a string ref
1062+     temp locations, or temp string refs

The old optimizer from 1975 has been turned off because of algorithm
changes (see code) so there will be some inefficiencies in the
load and store from temp locations.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tstmf3.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"



! basic double precision case

ibis-gen xx1 NC=3 NR=4 deffmt=DOUB

mf3 xx1 f="c1=@index"
mf3 xx1 f="c2=c1+2"
mf3 xx1 f="c3=@sqrt(c2)"

ibis-list xx1 csiz=(16,16,16) cfor="%16.14f %16.14f %16.14f"

! random case,

ibis-gen xx1 NC=2 NR=100 deffmt=DOUB
mf3 xx1 f="c1=@rand$c2=c1$@rsum(c2)"
ibis-list xx1 csiz=(16,16) cfor="%16.14f%16.9f"

! random case, test seed

ibis-gen xx1 NC=2 NR=100 deffmt=DOUB
mf3 xx1 f="c1=@rand$c2=c1$@rsum(c2)" seed=1
ibis-list xx1 csiz=(16,16) cfor="%16.14f%16.9f"

! test distance function

ibis-gen xx1 NC=5 NR=1

mf3 xx1 f="c1=35.0$c2=-121.0$c3=35.0$c4=-122.0"
mf3 xx1 f="@dist(c1,c2,c3,c4,c5)"

ibis-list xx1 


! basic string case, also double precision cosine

ibis-gen xx1 nr=1 nc=2 format=("A10","A12","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aaaaabbbbb") strcols=(1)

mf3 xx1 +
 f="c1='bc'$c2=@cat(@trim(c1,' '),'xxxxxx')$c3=@cos(c3)$c1=@ucase(c1)"


ibis-list xx1 csiz=(16,16,16) cfor="%16s %16s %16.12f"


!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=c3+@sQrt(70)$c4=c3"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="C3=70$c4=c3+@index+@rand+100$c3=@sQrt(c4)"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=5||3"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=5&&3"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=5^3"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=2.1^15.0"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=2.1^15.01"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c4=(c3>=0)*@sqrt(c3)" ! toms case

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2='b'"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=35.0$c4=-122.0$c4=@max(c3,c4)"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@trim(c1,'e')"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 +
 f="c1='bc'$c2=@cat(@trim(c1,' '),'xxxxxx')$c3=@cos(c3)$c1=@ucase(c1)"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@break(c1,'db')"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@fstr(c1,7)"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@bstr(c1,7)"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@adelete(c1,'bd')"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@sdelete(c1,'bb')"

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@sdelete(c1,'bc')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@replace(c1,'bb=qqq')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@replace(c1,'bc=qqq')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@strlen(c1)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@strlen('abc')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@streq(c1,'aabbccddee')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@streq(c1,'aabbccddeef')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@strsub(c1,'bb')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@strsub(c1,'bc')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@rjust(c1,12)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@ljust(c1,12)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@num('23456')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@num('23456.7890123')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@i2str(1234567890)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@i2str(-75)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@f2str(47.55555,2)" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'bb')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'bc')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'^a')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'b*d')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'e%')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=@pos(c1,'b?c')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@strpat(c1,'bbc')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@strpat(c1,'^a')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@strpat(c1,'b*d')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@strpat(c1,'e%')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c2=@strpat(c1,'b?c')" 

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

!  standard case, see function inside mf3 parm

ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f="c3=(1+2)*(3+4)+(5*6)" debug=1

ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

END-PROC
$!-----------------------------------------------------------------------------
$ create devmf3.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
!let _onfail="continue"
let $echo="yes"


! basic string case, also double precision cosine

ibis-gen xx1 nr=4 nc=3 format=("A10","A12","DOUB","DOUB") +
      data=(0.00001) datacols=(3) +
      string=("aabbccddee") strcols=(1)

mf3 xx1 f=("C3=1+2 +
         $c4=  3+4") debug=1 
           
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"



! error cases, or not tested



!mf3 xx1 f="c3=5^^3" debug=1 not implemented


! OK cases, first group is simple numeric

!mf3 xx1 f="c3=c3+@sQrt(70)$c4=c3"
!mf3 xx1 f="C3=70$c4=c3+@index+@rand+100$c3=@sQrt(c4)"
!mf3 xx1 f="c3=5||3"
!mf3 xx1 f="c3=5&&3"
!mf3 xx1 f="c3=5^3"
!mf3 xx1 f="c3=2.1^15.0"
!mf3 xx1 f="c3=2.1^15.01"
!mf3 xx1 f="c4=(c3>=0)*@sqrt(c3)" ! toms case


!ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB") +
!      data=(0.00001) datacols=(3) +
!      string=("aabbccddee") strcols=(1)

!mf3 xx1 f="c2='b'"
!mf3 xx1 f="c1=35.0$c2=-121.0$c3=35.0$c4=-122.0"
!mf3 xx1 f="c4=@max(c3,c4)"
!mf3 xx1 f="c2=@trim(c1,'e')"
!mf3 xx1 +
! f="c1='bc'$c2=@cat(@trim(c1,' '),'xxxxxx')$c3=@cos(c3)$c1=@ucase(c1)"
!mf3 xx1 f="c2=@break(c1,'db')"
!mf3 xx1 f="c2=@fstr(c1,7)"
!mf3 xx1 f="c2=@bstr(c1,7)"
!mf3 xx1 f="c2=@adelete(c1,'bd')"
!mf3 xx1 f="c2=@sdelete(c1,'bb')"
!mf3 xx1 f="c2=@sdelete(c1,'bc')"    ! corrects an old error at RAND
!mf3 xx1 f="c2=@replace(c1,'bb=qqq')"
!mf3 xx1 f="c2=@replace(c1,'bc=qqq')"    ! corrects an old error at RAND
!mf3 xx1 f="c3=@strlen(c1)"
!mf3 xx1 f="c3=@strlen('abc')"
!mf3 xx1 f="c3=@streq(c1,'aabbccddee')"
!mf3 xx1 f="c3=@streq(c1,'aabbccddeef')"
!mf3 xx1 f="c3=@strsub(c1,'bb')"
!mf3 xx1 f="c3=@strsub(c1,'bc')"   ! corrects an old error at RAND
!mf3 xx1 f="c2=@ljust(c1,12)"
!mf3 xx1 f="c2=@rjust(c1,12)"
!mf3 xx1 f="c3=@num('23456')"
!mf3 xx1 f="c3=@num('23456.7890123')"
!mf3 xx1 f="c2=@i2str(1234567890)"
!mf3 xx1 f="c2=@i2str(-75)"
!mf3 xx1 f="c2=@f2str(47.55555,2)"
!mf3 xx1 f="c3=@pos(c1,'bb')"
!mf3 xx1 f="c3=@pos(c1,'bc')"
!mf3 xx1 f="c3=@pos(c1,'^a')"
!mf3 xx1 f="c3=@pos(c1,'b*d')"
!mf3 xx1 f="c3=@pos(c1,'e%')"
!mf3 xx1 f="c3=@pos(c1,'b?c')"
!mf3 xx1 f="c2=@strpat(c1,'bbc')"
!mf3 xx1 f="c2=@strpat(c1,'^a')"
!mf3 xx1 f="c2=@strpat(c1,'b*d')"
!mf3 xx1 f="c2=@strpat(c1,'e%')"
!mf3 xx1 f="c2=@strpat(c1,'b?c')"
!mf3 xx1 f="c3=(1+2)*(3+4)+(5*6)" debug=1
!mf3 xx1 f=("C3=1+2+" +
!           "3+4") debug=1
!mf3 xx1 f="C3=1+2+ +
!           3+4" debug=1
!mf3 xx1 f=("C3=1+2+ +
!           3+4") debug=1

!mf3 xx1 f="c3=5^^3" debug=1 not implemented
!mf3 xx1 f=("C3=1+                                       +
!                                                     2+ +
!                                                     3+ +
!                                                     4+ +
!                                 5") debug=1 

!ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"

END-PROC
$!-----------------------------------------------------------------------------
$ create tstmf3.log_solos
tstmf3
ibis-gen xx1 NC=3 NR=4 deffmt=DOUB
Beginning VICAR task ibis
mf3 xx1 f="c1=@index"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
mf3 xx1 f="c2=c1+2"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
mf3 xx1 f="c3=@sqrt(c2)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16) cfor="%16.14f %16.14f %16.14f"
Beginning VICAR task ibis
 
Number of Rows:4  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:4
+----------------+----------------+---------------
              C:1              C:2             C:3
+----------------+----------------+---------------
1.00000000000000 3.00000000000000 1.73205080756888
2.00000000000000 4.00000000000000 2.00000000000000
3.00000000000000 5.00000000000000 2.23606797749979
4.00000000000000 6.00000000000000 2.44948974278318
ibis-gen xx1 NC=2 NR=100 deffmt=DOUB
Beginning VICAR task ibis
mf3 xx1 f="c1=@rand$c2=c1$@rsum(c2)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16) cfor="%16.14f%16.9f"
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 2       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.17082803610629     0.170828036
0.74990198048496     0.920730017
0.09637165562357     1.017101672
0.87046522702708     1.887566899
0.57730350679511     2.464870406
0.78579925883967     3.250669665
0.69219415345864     3.942863818
0.36876626992042     4.311630088
0.87390407686181     5.185534165
0.74509509845007     5.930629264
0.44604590909311     6.376675173
0.35372820309338     6.730403376
0.73251963200254     7.462923008
0.26022200108288     7.723145009
0.39429377492388     8.117438784
0.77678995122562     8.894228735
0.84503513758029     9.739263873
0.57578820048278    10.315052073
0.71553859516866    11.030590668
0.08300424607388    11.113594914
0.45582512865976    11.569420043
0.10994681418065    11.679366857
0.54522802381657    12.224594881
0.39068657064868    12.615281452
0.56858542321445    13.183866875
0.95906644948838    14.142933324
0.86771909645914    15.010652421
0.16318951025236    15.173841931
0.27550892686832    15.449350858
0.26036109487207    15.709711953
 
Rows: 31:60
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.92409474186917    16.633806695
0.43592263710268    17.069729332
0.78946086555209    17.859190197
0.12761701681174    17.986807214
0.08220568604044    18.069012900
0.94064201644785    19.009654917
0.02557492625302    19.035229843
0.15421093132782    19.189440774
0.38218242527816    19.571623199
0.15473669966669    19.726359899
0.52933341811698    20.255693317
0.87684849108325    21.132541808
0.43061143833840    21.563153247
0.26390622634208    21.827059473
0.31359449902321    22.140653972
0.77009168585472    22.910745658
0.10739088305411    23.018136541
0.77104225519565    23.789178796
0.70519555889445    24.494374355
0.21863965870776    24.713014014
0.76179399285600    25.474808007
0.41171304557896    25.886521052
0.64882682292911    26.535347875
0.92995625490737    27.465304130
0.50241856559867    27.967722696
0.68744067942887    28.655163375
0.43609094814733    29.091254323
0.60830090794974    29.699555231
0.57655863363537    30.276113865
0.63262171071407    30.908735575
 
Rows: 61:90
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.46342564762874    31.372161223
0.63224378961009    32.004405013
0.13829493264933    32.142699945
0.96076141792519    33.103461363
0.14437500108784    33.247836364
0.44668307106778    33.694519435
0.32458445984538    34.019103895
0.95258407759247    34.971687973
0.35818391048118    35.329871883
0.39820822249224    35.728080106
0.10128154223238    35.829361648
0.95508573565288    36.784447384
0.98461694767107    37.769064331
0.57597008145149    38.345034413
0.86591373538526    39.210948148
0.14987589123277    39.360824039
0.90915043028166    40.269974470
0.65125250864004    40.921226978
0.06386085476492    40.985087833
0.95499938656246    41.940087220
0.96626276427566    42.906349984
0.78554338521397    43.691893369
0.80516481789500    44.497058187
0.57125367979575    45.068311867
0.28258571995424    45.350897587
0.96255105196796    46.313448639
0.57947705894631    46.892925698
0.43685862697057    47.329784325
0.37543617453727    47.705220499
0.92340760919302    48.628628108
 
Rows: 91:100
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.02869938006311    48.657327489
0.76885997771550    49.426187466
0.72310677171759    50.149294238
0.59096934981297    50.740263588
0.42588853857984    51.166152126
0.64214305908302    51.808295185
0.74676272910063    52.555057915
0.06901854981410    52.624076464
0.05329494862236    52.677371413
0.77431364557135    53.451685059
ibis-gen xx1 NC=2 NR=100 deffmt=DOUB
Beginning VICAR task ibis
mf3 xx1 f="c1=@rand$c2=c1$@rsum(c2)" seed=1
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16) cfor="%16.14f%16.9f"
Beginning VICAR task ibis
 
Number of Rows:100  Number of Columns: 2       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.04163034477188     0.041630345
0.45449244472863     0.496122790
0.83481721816691     1.330940008
0.33598603014520     1.666926038
0.56548940356614     2.232415441
0.00176691239174     2.234182354
0.18758951699996     2.421771871
0.99043407993766     3.412205951
0.75049713322952     4.162703084
0.36627363815273     4.528976722
0.35120909779088     4.880185820
0.57334510445569     5.453530924
0.13255423031022     5.586085155
0.06416647540188     5.650251630
0.95085373365191     6.601105364
0.15356010510015     6.754665469
0.58464936653110     7.339314835
0.21658811985893     7.555902955
0.80650171783641     8.362404673
0.14047297726302     8.502877650
0.62205875694953     9.124936407
0.21089277700644     9.335829184
0.00657788481444     9.342407069
0.57329859512915     9.915705664
0.93266420128950    10.848369865
0.34032653370951    11.188696399
0.89108063522881    12.079777034
0.59387255333926    12.673649588
0.39289646680827    13.066546055
0.89932326521405    13.965869320
 
Rows: 31:60
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.69499415059891    14.660863470
0.22860269223855    14.889466163
0.96245934907768    15.851925512
0.01205379763141    15.863979309
0.11095725048541    15.974936560
0.88409649461671    16.859033054
0.11675589085890    16.975788945
0.75092259508993    17.726711540
0.29693356870572    18.023645109
0.64669171943316    18.670336829
0.42657987132955    19.096916700
0.50094101246202    19.597857712
0.31457721859844    19.912434931
0.43678429451538    20.349219225
0.66003561078343    21.009254836
0.70254571146042    21.711800548
0.77480919277434    22.486609740
0.81326658811872    23.299876329
0.31981518206561    23.619691511
0.98232674243823    24.602018253
0.67963271276228    25.281650966
0.15097478104247    25.432625747
0.87602709192972    26.308652839
0.69675860892464    27.005411448
0.37736592096033    27.382777369
0.56483522921032    27.947612598
0.47499220570885    28.422604804
0.27249091928155    28.695095723
0.93932800383548    29.634423727
0.25906050374184    29.893484230
 
Rows: 61:90
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.54018394967845    30.433668180
0.64777955237646    31.081447733
0.56977536388007    31.651223096
0.50956528406760    32.160788380
0.20329844975663    32.364086830
0.08857618433260    32.452663015
0.28086813092831    32.733531145
0.70466452207183    33.438195668
0.75992906523637    34.198124733
0.67062010373978    34.868744837
0.60709943356377    35.475844270
0.37562035408458    35.851464624
0.51224656457422    36.363711189
0.55219304568800    36.915904234
0.65851075100640    37.574414985
0.93788417528933    38.512299161
0.19533377937136    38.707632940
0.59323186312032    39.300864803
0.08184164755744    39.382706451
0.56972517026258    39.952431621
0.26739977507538    40.219831396
0.95309678979291    41.172928186
0.22934156790116    41.402269754
0.80176047641840    42.204030230
0.33813620510513    42.542166435
0.58494139507711    43.127107830
0.94525045481432    44.072358285
0.64374410659402    44.716102392
0.90323538155596    45.619337773
0.36842564944214    45.987763423
 
Rows: 91:100
+---------------+---------------
             C:1             C:2
+---------------+---------------
0.24200405361749    46.229767476
0.48949583538141    46.719263312
0.91394851287901    47.633211825
0.32918237321663    47.962394198
0.82723420251665    48.789628400
0.31334652358549    49.102974924
0.93403133944583    50.037006263
0.59334129264320    50.630347556
0.98693448732957    51.617282043
0.38885687887097    52.006138922
ibis-gen xx1 NC=5 NR=1
Beginning VICAR task ibis
mf3 xx1 f="c1=35.0$c2=-121.0$c3=35.0$c4=-122.0"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
mf3 xx1 f="@dist(c1,c2,c3,c4,c5)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 5       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+-----------+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4         C:5
+-----------+-----------+-----------+-----------+-----------
       35.00     -121.00       35.00     -122.00   110824.55
ibis-gen xx1 nr=1 nc=2 format=("A10","A12","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aaaaabbbbb") strcols=(1)
Beginning VICAR task ibis
mf3 xx1  +
 f="c1='bc'$c2=@cat(@trim(c1,' '),'xxxxxx')$c3=@cos(c3)$c1=@ucase(c1)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16) cfor="%16s %16s %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 3       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+---------------
              C:1              C:2             C:3
+----------------+----------------+---------------
      BC             bcxxxxxx       0.999999999950
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=c3+@sQrt(70)$c4=c3"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    8.366610265341   8.366610265341
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="C3=70$c4=c3+@index+@rand+100$c3=@sQrt(c4)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                   13.105338682403 171.749901980485
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=5||3"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    7.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=5&3"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    1.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=5^3"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                  125.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=2.1^15.0"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                  68122.31858295173  0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=2.1^15.01"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                  68629.62311837915  0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c4=(c3>=0)*@sqrt(c3)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    0.000010000000   0.003162277620
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2='b'"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     b              0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=35.0$c4=-122.0$c4=@max(c3,c4)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                   35.000000000000  35.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@trim(c1,'e')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aabbccdd       0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1  +
 f="c1='bc'$c2=@cat(@trim(c1,' '),'xxxxxx')$c3=@cos(c3)$c1=@ucase(c1)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      BC             bcxxxxxx       0.999999999950   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@break(c1,'db')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aa             0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@fstr(c1,7)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aabbccd        0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@bstr(c1,7)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     dee            0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@adelete(c1,'bd')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aaccee         0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@sdelete(c1,'bb')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aaccddee       0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@sdelete(c1,'bc')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aabcddee       0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@replace(c1,'bb=qqq')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aaqqqccddee    0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@replace(c1,'bc=qqq')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aabqqqcddee    0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@strlen(c1)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                   10.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@strlen('abc')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    3.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@streq(c1,'aabbccddee')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    1.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@streq(c1,'aabbccddeef')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    0.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@strsub(c1,'bb')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    1.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@strsub(c1,'bc')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    1.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@rjust(c1,12)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee       aabbccddee   0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@ljust(c1,12)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     aabbccddee     0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@num('23456')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                  23456.00000000000  0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@num('23456.7890123')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                  23456.78901230000  0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@i2str(1234567890)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     1234567890     0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@i2str(-75)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     -75            0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@f2str(47.55555,2)"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     47.56          0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'bb')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    3.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'bc')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    4.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'^a')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    1.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'b*d')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    4.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'e%')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                   10.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=@pos(c1,'b?c')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                    3.000000000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@strpat(c1,'bbc')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     bbc            0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@strpat(c1,'^a')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     a              0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@strpat(c1,'b*d')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     bccd           0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@strpat(c1,'e%')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     e              0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c2=@strpat(c1,'b?c')"
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee     bbc            0.000010000000   0.000000000000
ibis-gen xx1 nr=1 nc=3 format=("A10","A12","DOUB","DOUB")  +
      data=(0.00001) datacols=(3)  +
      string=("aabbccddee") strcols=(1)
Beginning VICAR task ibis
mf3 xx1 f="c3=(1+2)*(3+4)+(5*6)" debug=1
Beginning VICAR task mf3
mf3 version Wed Oct 07 2008
ibis-list xx1 csiz=(16,16,16,16) cfor="%16s %16s %16.12f %16.12f"
Beginning VICAR task ibis
 
Number of Rows:1  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:1
+----------------+----------------+----------------+---------------
              C:1              C:2              C:3             C:4
+----------------+----------------+----------------+---------------
      aabbccddee                   51.000000000000   0.000000000000
END-PROC
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
