#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdlib.h>             //64-bit def of NULL

#include "ibisfile.h"
#include "ibiserrs.h"
#include "cartoMemUtils.h"      //for mz_alloc1
#include "cartoStrUtils.h"      //for ms_num

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

/* prototypes */
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

int mtchfield(char *q,char fld[NUMCOLS][MAXTEXT],int nincol);
int mtchfield2(char *q,char fld[NUMCOLS][MAXTEXT],int nincol,double value);

double ffetchcd(int k,int typ,unsigned char c_data[]);
void fstorecd(int k,int typ,double val,unsigned char c_data[]);
void stsget(int *s,char *fstrng,double *dbuf,int *cnum,char *sbuf,
    int *sptr);
void sp_knuth(char *fstrng,int *ibuf,double *dbuf,char *sbuf,int *cnum,int *sptr);
double ms_dnum (char **num_ptr);
void insq(char *buf,int indx,int ptr);
void delq(char *buf,int ptr);
void sp_xknuth(int *ibuf,double *dbuf,char *sbuf,int *sptr,double *result,
    int code);

int fp,sbop2,cp,nbpo,idebug,functionsize;

/************************************************************************/
/* program mf4                                                      */
/************************************************************************/
/*  99-11 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/*  RJB - add code dump like f2                                         */
/*  Mar 21, 2008 rjb - removed  ms_dnum,  ms_num, mystrnicmp,           */
/*                      mz_alloc1, mz_alloc2, and mz_free2 code         */
/*                      mystrnicmp replaced by strncasecmp              */
/*                      debug no longer dumps code                      */
/*  Jul 26, 2008 rjb - merged with svn rev 50 mf3 changes pkim          */
/*  Jul 27, 2008 rjb - merged with solaris version with all fixes       */
/*                     removed routines assoc with libcarto             */
/*                      replace solaris mystrnicmp with strncasecmp     */
/*                      in main44                                       */
/*  Jan 29, 2010 rjb - Made compatible with 64-bit afids Build 793      */
/*                          Linux, MacOSX (both Intel/PowerPC)          */
/*                                                                      */
/************************************************************************/

char cvec[64] = {'0','1','2','3','4','5','6','7','8','9',
     '.','a','l','o',
     'g','i','n','t','s','q','r','x','d','m','b','c','e','p',
     'f','h','j','k','u','v','w','y','z','_','A','B',
     'C','D','E','F','G','H','I','J','K','L','M','N',
     'O','P','Q','R','S','T','U','V','W','X','Y','Z'};

/*=========================================================*/
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

void st1(unsigned char v,unsigned char *s) { *s = v; return; }
void st2(short int v,short int *s) { *s = v; return; }
void st4(int v,int *s) { *s = v; return; }
void st7(float v,float *s) { *s = v; return; }
void st8(double v,double *s) { *s = v; return; }

/*================================================================*/
/* mtchfield - from column number assign it an order  js        */
/*      q is remainder of function string after ( or ,          */
/*      fld  contains input column numbers in c3, etc. format   */
/*      nincol is the number of columns contained in function   */
/*      j is the column found and returned to caller            */
/*      a -1 return indicates that a column number wasnt found  */
int mtchfield(q,fld,nincol)
   char *q,fld[NUMCOLS][MAXTEXT];
   int nincol;
{
   int len1,len2,j,js;
   char *r;
/*    char *strpbrk(const char *, const char *);  */

//    printf ("fld[0][0]fld[0][1] = %c%c %c%c <\n",fld[0][0],fld[0][1],fld[0][2],fld[0][3]);
   r = strpbrk(q,",)");                 //find comma or close quote
   len1 = (int)(r-q);                   /* cast - May 06, 2011 */
//    printf ("r = %s  q = %s len1 = %d  nincol = %d\n",r,q,len1,nincol); 
   js = -1;
   for (j=0;j<nincol;j++)
      {
      len2 = (int)strlen(fld[j]);           /* cast - May 06, 2011 */
      if (len1!=len2) continue;
//    printf ("j = %d  js = %d\n",j,js);
      if (strncmp(q,fld[j],(size_t)len1)==0) js = j;        /* cast - May 06, 2011 */
      }
   if (js<0)    {
        zmabend("??E mtchfield: column number not found - Forget c?");
    }
//    printf ("js = %d\n",js);
   return(js);
}
/*================================================================*/
/* mtchfield2 - from column number assign it an order  js        */
/*  in most functions a column number nust be found             */
/*  However, in GEOPHYSICAL Column Operations, not all fields   */
/*  require column numbers. This function is called for them    */

/*      q is remainder of function string after ( or ,          */
/*      fld  contains input column numbers in c3, etc. format   */
/*      nincol is the number of columns contained in function   */
/*      j is the column found and returned to caller            */
/*      a -1 return indicates that a column number wasnt found  */
/*      in that case a value was found, which is returned in    */
/*      value                                                   */
int mtchfield2(q,fld,nincol,value)
   char *q,fld[NUMCOLS][MAXTEXT];
     int nincol;
    double value;
{
    int len1,len2,j,js;
    char *r;
/*  *strpbrk(const char *, const char *); */
    char numval[MAXTEXT];

//    printf ("fld[0][0] = %c%c %c%c\n",fld[0][0],fld[0][1],fld[0][2],fld[0][3]);
    r = strpbrk(q,",)");            //find comma or close quote
    len1 = (int)(r-q);
//    printf ("r = %s  q = %s len1 = %d  nincol = %d\n",r,q,len1,nincol); 
    js = -1;
    for (j=0;j<nincol;j++)
        {
            len2 = (int)strlen(fld[j]);         /* cast - May 06, 2011 */
            if (len1!=len2) continue;
//    printf ("j = %d  js = %d\n",j,js);
            if (strncmp(q,fld[j],(size_t)len1)==0) js = j;          /* cast - May 06, 2011 */
        }
/* if no columm found, then string is a value, pass it back in value     */
    if (js<0)    {
        strncpy(numval,q,(long unsigned int)len1);          /* cast - May 06, 2011 */
        numval[len1]='\0';
        value = atof(numval);
    }
//    printf ("js = %d,  value=%f\n",js,value);
   return(js);
}
/*================================================================*/
/*  ffetchcd - get 8-byte value from a column   */
/*             as unsigned characters           */
/* returns value as a double                    */
double ffetchcd(k,typ,c_data)
   int k,typ;
   unsigned char c_data[];
{
   switch(typ)
      {
      case 1: return((double)ct1((unsigned char *)&c_data[k]));
      case 2: return((double)ct2((short int *)&c_data[k]));
      case 4: return((double)ct4((int *)&c_data[k]));
      case 7: return((double)ct7((float *)&c_data[k]));
      case 8: return(ct8((double *)&c_data[k]));
      }
   return(0.);
}
/*================================================================*/
/* fstorecd - store a value (double) in a column    */
/*              as a series of unsigned characters  */
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
	       st2(x2,(short int*)&c_data[k]); return;
       case 4: x4 = (int) val;
	       st4(x4,(int *)&c_data[k]); return;
       case 7: x7 = (float)val;
	       st7(x7,(float *)&c_data[k]); return;
       case 8: st8(val,(double *)&c_data[k]); return;
       }
   return;
}
/*================================================================*/

/* c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines 
        inputs fstrng (function string - whatever is after =  sign)
        returns s (symbol value)

        outputs: sbuf - string buffer for string functions
                 dbuf - numeric values for evaluation
        temporarily modifies fstrng for + and - values
        modifies: sptr,fp,sp,sbop2,nbpo
        fp =  number of useful chars in fstrng after removal of "(" or ")"
*/
   
void stsget(s,fstrng,dbuf,cnum,sbuf,sptr)
   int *s,*cnum,*sptr;
   char *fstrng,*sbuf;
   double *dbuf;

{
   double rnum,rfac=0;
   int first,atop;
   char c,cl,minus,aop[19],intg[66];
   char outmsg[132];
/* fcv - operator */
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
     
/* printf ("stsget: fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo); */
      atop = 0;
      first = 1;
      nbpo = 1;
      num = 0;
      snum = 0;
      rnum = 0.0;
      type = 0;
      qtype = 0;
/* parse function */
l100: c = fstrng[fp+1];
      if (c==aop[6]) goto l8;		/* blank */
      fpx = fp+1;
      for (i=0;i<17;i++)
         {
         isav = i;
         if (c==aop[i]) goto l700;	/* if legal operator */
         }
      if (c==aop[17]) goto l704;	/* if @ */
      goto l39;
/* legal operator eval */
l700: if (isav<10||isav>15) goto l702;
      c = fstrng[fp+2];
      for (i=10;i<16;i++)
         {
         if (c==aop[i]) goto l703;	/* if logical operator <,=,!,|,&,>,^ */
         }
l702: if (isav==16) isav = 18;          /* ^ to aop[18] */
      if (isav==12) isav = 19;		/* ! to aop[19] */
      if (isav==14) isav = 17;		/* & to aop[17] - overwrite @ */
      fpx = fp+1;
      goto l10;
/* logical eval */
l703: isav = isavtr[isav-10];
      fpx = fp+2;
      goto l10;
/* @ eval */
l704: atop = 1;		/* flag @ operator */
      fp = fp+1;
      goto l100;	/* next char */
/* continuei, check for legal characters in intg */
 l39: first = 0;
      for (i=0;i<65;i++)
         {
         isl = i;
         cl = (char)tolower(c);	/* make lowercase - char added 20-Jun-2011 */
         if (cl==intg[i]) goto l4;
         }
      sprintf(outmsg,"??E stsget: illegal symbol = %c\n",c);
      zvmessage(outmsg," ");
      zmabend("??E program terminating");
      return;

/* legal char eval */
 l4:  if (isl<10) goto l7;	/* branch if numeric */
      if (isl>10) goto l5;	/* branch if alphabetic */
/* char is a period */
      type = 1;                /* float */
      rnum = (double)num;
      rfac = 1.0;
      goto l8;
/* alphabetic eval */
 l5:  if (isl==26) goto l55;	/* branch if e */
      if (isl==64) goto l65;	/* branch if ' */
      type = -1;		/* alphabetic */
      goto l7;
/* e eval */
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
      goto l100;	/* next char */
/* ' eval */
 l65: type = 0;		/* quoted */
      qret = *sptr;
      qtype = 1;	/* single quote */
      for (i=0;i<30;i++)
         {
         if (fstrng[fp+2]==intg[64]) break; 	/* closing ' */
         sbuf[*sptr] = fstrng[fp+2];
         *sptr = *sptr+1;
         fp = fp+1;
         }
      sbuf[*sptr] = (char)0;		/* append a nul */
      *sptr = *sptr+1;
      fp = fp+2;
      goto l100;        /* next char */
/* compute real value */
 l7:  num = num*10+isl;		/* isl is in intg */
      snum = snum*39;
      if (snum>100000000) snum = snum/31;
      snum = snum+isl;
      rfac = .1*rfac;
      rnum = rnum+rfac*(double)(isl);
/* blank eval */
 l8:  fp = fp+1;
      goto l100;        /* next char */
/* resume logical eval */
 l10: if (first) goto l20;
      sbop2 = 0;
      if (type<0) goto l11;
      if (type==0) goto l12;	/* branch if quoted */
      if (type>0) goto l13;	/* branch if float */
 l11: if (!atop) goto l801;
/* @ eval */
      for (i=0;i<148;i++)
         {
         isv = i;
         if (num==fcv[i]) goto l15;
         }
      zmabend("??E stsget: operator not found");
      return;

l801: for (i=0;i<20;i++)
         {
         if (snum!=cnum[i]) continue;
         *s = i;
         return;
         }
      return;
/* get operator in s */
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
/* float to double */
 l13: cp = cp+1;
      dbuf[cp] = rnum;
      *s = cp;
      return;

 l20: fp = fpx;
      *s = cop[isav];
      if (*s==14) nbpo = 0;
      if (c==aop[8]) fstrng[fp] = minus; 	/* ; */
      if (c!=aop[2]) goto l21;			/* branch if not 1 */
      c = fstrng[fp+1];
      if (c!=aop[2]) goto l21;			/* branch if not 1 */
      *s = 5;					/* set OPCODE to 5 (EXP) */
      fp = fp+1;
 l21: sbop2 = bop2[*s];
      return;
}
    
/*================================================================*/
/*
   c version 1/23/00 al zobrist ... no attempt to use c constructs,
   just a straight conversion of the fortran lines

c  modified 1/17/90 for string functions
c  modified 3/16/87 a. zobrist for mosx system
c  kludged again 6/18/87 a. zobrist
c  this routine is really getting encrusted from about
c  five major changes 

   input:  fstrng (whatever is on the right side of = sign)
   modifies: sbop2,fp,cp
   output: ibuf, (sbuf,dbuf,sptr from stsget)
*/
/*================================================================*/

void sp_knuth(char* fstrng, int* ibuf, double* dbuf, char* sbuf, int* cnum, int* sptr)
/*
   char *fstrng,*sbuf;
   int *cnum,*sptr,*ibuf;
   double *dbuf;
*/
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
   char outmsg[501]; 
   int ix,itemp,bp,op,sp,s,loc,iop,s2;

/*	        sprintf (outmsg,"sp_knuth: debug = %d   debugrec1 = %d\n",idebug,debugrec1);  
		zvmessage(outmsg," ");
*/

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
      
/*      if (idebug) {
	  sprintf(outmsg,"\n");
	  zvmessage(outmsg," ");
      }
*/ 
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
/*        printf ("before - fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo); */
/*  void stsget(int *s, char *fstrng, double *dbuf, int *cnum, char *sbuf, int *sptr); */

 l5:  stsget(&s,fstrng,dbuf,cnum,sbuf,sptr);
      if (idebug) { 
	        sprintf(outmsg,"sp_knuth: input symbol: %d:   fstrng: %s ",s,fstrng);
	        zvmessage(outmsg," ");
      } 
/*       printf ("after  - fp,cp,sbop2,nbpo = %d %d %d %d\n",fp,cp,sbop2,nbpo); */
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
 l24: 
      /*  ptr = 61;  */
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

 /*================================================================*/
/* look into char patbuf[131] starting at ptr */ 
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
/*================================================================*/
/* delete in char patbuf[131] starting at ptr */
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
         if (ichar!=63) break;   /* ? */
         iq = iq+1;
         }
 
      iu2 = iu-iq;
      
      for (i=ptr;i<iu2;i++)
         {
         buf[i] = buf[i+iq];
         }
      return;
}
/*================================================================*/
/*    modified 3/16/87 by a. zobrist for mosx system */
/*    kludged from fortran to c 1/24/00 by a. zobrist 
      ... no attempt to use c constructs, just a straight conversion
      of the fortran lines
        inputs:  ibuf,dbuf,sbuf
        modifies: sbuf,sptr
    outputs: result (only after RETN)
*/
/*================================================================*/

void sp_xknuth(ibuf,dbuf,sbuf,sptr,result,code)
   int *ibuf,*sptr;
   int code;
   double *result,*dbuf;
   char *sbuf;
{
/* operator *opptr, oopp;  */
 double reg=0,div,t,num,sig,minut,secnd,frac,sig2;
 char patbuf[131];                  /* left at fortran indexing */
 char outmsg[132]; 
 char tchar;
 int starp[4],isu[4],blnct[3];      /* left at fortran indexing */
       
 int ptr,op,opnd,ibit,jbit,kbit,ireg,jreg,i,j,tmtch,mtch,len,imtch=0;
 int osptr,slen,pmtch,stp,cptr,isu1,is1,isu2,is2,isu3,is3,break2;
 int lrsw,ltr,btr,str,knum,itop=0,kdig,itop2,deccnt,ichar,ichxx;
 char *p,*q,mtchbuf[1000];

  char opcode_name[63][7] = {
	"x","ADD   ","SUB   ","MUL   ","DIV   ","x","LOG10 ","LOG   ","INT   ","x","x",
	"x","x","LOAD  ","STOR  ","RETN  ","SQRT  ","SIN   ","COS   ","TAN   ","MAX   ",
	"MIN   ","MOD  ","ABS   ","LCMP  ","ATAN2 ","ASIN  ","ACOS  ","ATAN  ","LT    ","LE    ",
	"EQ    ","NE    ","GE    ","GT    ","OR    ","AND   ","POW   ","NOT   ","x","x",
	"LSHF  ","RSHF  ","FSTR  ","BSTR  ","ADEL  ","SDEL  ","TRIM  ","UCASE ","LCASE ","REPL  ",
	"STRLEN","POS   ","STREQ ","STRSUB","STRPAT","LJUST ","RJUST ","NUM   ","I2STR ","F2STR ",
	"DMSSTR","DMSNUM"};  

/*	sprintf (outmsg,"sp_xknuth: debug = %d   debugrec1 = %d\n",idebug,debugrec1);  
	zvmessage(outmsg," ");	
*/
 for (ptr=0;ptr<OPBUF;ptr++)
    {
    op = ibuf[ptr]>>16;				/* OPCODE */
    opnd = ibuf[ptr]&65535;			/* OPERAND */
/*
    printf ("op = %d opnd = %d\n",op,opnd);
    opptr = &oopp;
    oopp.operand = (short) opnd;
    oopp.opcode = (short) op;

	printf ("oopp.opcode = %d oopp.operand = %d\n", oopp.opcode,oopp.operand);
	printf ("opptr->opcode = %d\n",opptr->opcode);
*/
    switch (op)
    {      
    case 9: case 10: case 11: case 12: case 39: case 40: 
      zmabend("??E sp_xknuth: arithmetic execution error");
      break;
    case 1:					/* +  (ADD) */
      reg = reg+dbuf[opnd];
      break;
    case 2:
      reg = reg-dbuf[opnd];			/* - (SUB) */
      break;
    case 3:
      reg = reg*dbuf[opnd];			/* * (MUL) */
      break;
    case 4:					/* / (DIV) */
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
    case 6:					/* LOG10 */
      reg = log10(MAX(dbuf[opnd],1.0e-6));
      break;
    case 7:					/* LN  (LOG) */
      reg = log(MAX(dbuf[opnd],1.0e-6));
      break;
    case 8:
      reg = (int)(dbuf[opnd]);			/* INT */ 
      break;
    case 13:					/* LOAD */
      reg = dbuf[opnd];
      break;
    case 14:					/* STOR */
      dbuf[opnd] = reg;
      break;
    case 15:					/* RTN */
      *result = reg;
        if (idebug || code) {
            sprintf (outmsg,"%s %5d    reg = %f",&opcode_name[op][0],opnd,reg);
            zvmessage(outmsg," ");
        }

      return;
    case 16:					/* SQRT */
      reg = sqrt(fabs(dbuf[opnd]));
      break;
    case 17:					/* SIN */
      reg = sin(dbuf[opnd]);
      break;
    case 18:					/* COS */
      reg = cos(dbuf[opnd]);
      break;
    case 19:					/* TAN */
      reg = tan(dbuf[opnd]);
      break;
    case 20:					/* max (AMAX) */
      reg = MAX(reg,dbuf[opnd]);
      break;
    case 21:					/* MIN (AMIN) */
      reg = MIN(reg,dbuf[opnd]);
      break;
    case 22:					/* FMOD (MOD) */
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
    case 23:					/* ABS */
      reg = fabs(dbuf[opnd]);
      break;
    case 24:					/* LCMP */
      reg = -dbuf[opnd];
      break;
    case 25:					/* ATAN2 */
      if ((reg==0.0)&&(dbuf[opnd]==0.0))
         {
         reg = 0.0;
         break;
         }
      reg = atan2(reg,dbuf[opnd]);
      break;
    case 26:					/* ASIN */
      reg = asin(dbuf[opnd]);
      break;
    case 27:					/* ACOS */
      reg = acos(dbuf[opnd]);
      break;
    case 28:
      reg = atan(dbuf[opnd]);			/* ATAN */
      break;
    case 29:
      t = 0.0;
      if (reg<dbuf[opnd]) t = 1.0;		/* < (.LT.) */
      reg = t;
      break;
    case 30:					/* <= (.LE.) */
      t = 0.0;
      if (reg<=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 31:					/* == (.EQ.) */
      t = 0.0;
      if (reg==dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 32:					/* != (.NE.) */
      t = 0.0;
      if (reg!=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 33:					/* >= (.GE.) */
      t = 0.0;
      if (reg>=dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 34:					/* > (.GT.) */
      t = 0.0;
      if (reg>dbuf[opnd]) t = 1.0;
      reg = t;
      break;
    case 35:					/* || (.OR.) */
      ibit = (int)reg;              /* cast - May 06, 2011 */
      jbit = (int)dbuf[opnd];            /* cast - May 06, 2011 */
      kbit = ibit|jbit;
      reg = (double)kbit;
      break;
    case 36:					/* && (.AND. */
      ibit = (int)reg;              /* cast - May 06, 2011 */
      jbit = (int)dbuf[opnd];       /* cast - May 06, 2011 */
      kbit = ibit&jbit;
      reg = (double)kbit;
      break;
    /*case 37: the ^ symbol appropriated for expon
      ibit = reg;
      jbit = dbuf[opnd];
      kbit = ibit^jbit;
      reg = (double)kbit;
      break;*/
    case 38:					/* ! (.NOT.) */
      reg = 1.0-dbuf[opnd];
      break;
                               /* cat */
    case 41:					/* <<    LSHF */
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
                                
    case 42:			/* break */	/* >>    RSHF */
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
                               
    case 43:			/* FSTR */
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
                               
    case 44:			 /* BSTR */
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
                                
    case 45:			/* ADELETE */
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
                                
    case 46:			/* SDELETE */
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
                                
    case 47:			/* TRIM */
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
         
         for (j=0;j<30;j++)
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
                                
    case 48:			/* UCASE */
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[jreg+i];
         if (ichar==0) break;
         sbuf[*sptr] = (char)toupper((char)ichar);
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                
    case 49:			/* LCASE */
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      
      for (i=0;i<100;i++)
         {
         ichar = (int)sbuf[jreg+i];
         if (ichar==0) break;
         sbuf[*sptr] = (char)tolower((char)ichar);
         *sptr = *sptr+1;
         }
      sbuf[*sptr] = (char)0;
      *sptr = *sptr+1;
      break;
                                
    case 50:			/* REPLACE */
      ireg = (int)(reg+.001);
      jreg = (int)(dbuf[opnd]+.001);
      reg = (double)(*sptr);
      tmtch = 0;
      for (j=0;j<100;j++)
         {
         ichxx = (int)sbuf[jreg+j];
         if (ichxx==61) break;
         if (ichxx==0) zmabend("??E sp_xknuth: no equals sign in replacement string");
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
                               
    case 51:			/* STRLEN */
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
                                
    case 53:			/* STREQ */
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
                                
    case 54:			/* STRSUB */
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
                               
    case 52: case 55:		/*  52=POS 55=STRPAT */
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
                                
    case 57:			/* RJUST */
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
                               
    case 58:			/* NUM */
      jreg = (int)(dbuf[opnd]+.001);
      p = &sbuf[jreg];
      reg = ms_dnum(&p);
      break;
                               
    case 59:			/* I2STR */
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

    case 61:				/* dmsstr */
      jreg = (int)(dbuf[opnd]+.001);
      len = (int)strlen(&sbuf[jreg]);               /* cast - May 06, 2011 */
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
                                   
    case 62:				/* DMSNUM */
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
        if (idebug || code) { 
            sprintf (outmsg,"%s %5d    reg = %f",&opcode_name[op][0],opnd,reg);
            zvmessage(outmsg," ");
/*        zknuth_dump(opnd,op); */
        } 
 } 
 return;
}
/*================================================================*/
/* main program */
void main44()
{
   char c_field[NUMCOLS][MAXTEXT];
   char *function,funcparm[40][251];

   char *c_data,*sbuf;
   char *p,*q=0,*qlp,*r,c;
   char fmtstring[10];
   char outmsg[251],outmsg2[501];
   char c_tmp[8],blanks[500],zeros[2];
   int ibuf[OPBUF];
   
   int i,j,k,m,ibig,ncol,nincol,tablen,rcol,lres,seed;
   int datcols[NUMCOLS],typ[NUMCOLS],wid[NUMCOLS],totwid[NUMCOLS1];
   int sptr,svsptr,savvec[NUMCOLS2];
   int strl,snum,alphc,cptr,lptr,cnum[NUMCOLS],savptr;
   int js,jt=0,n,ii,iii,l,k1,k2,ist,icount,ksv,isv,wmx,ires;
   int rctr,rstart,rstop;
   int ifop,lparfound;
   int j1,j2,j3,j4,j5,j6=0,j7;
   int npar,idef,unit,ibis,status;
   int idebug,code;
   double phi1,tht1,p1,p2,p3;
   double phi2,tht2,q1,q2,q3,q4;
   double phi3,tht3,n1,n2,n3;
   double pxn1,pxn2,pxn3,pxq1,pxq2,pxq3;
   double ndq,pdn,pdq,pxndq,phi,pxnpxq,raddeg,mpr,rdist=0.0;
   double degrad = 57.295779512;
   double res,dbuf[ARITHBUF];
   double sum,vout,mean,ssq,val0,val1=0.0,val2=0.0,cmp0,cmp1,cmp2,val=0.0,pval=0.0;
   double vmin=0.0,vmax=0.0,dcmp,ldiff,ndiff,diff;
    double value1=0.0,value2=0.0,value3=0.0,value4=0.0,value5=0.0,value6=0.0;

   char fop[11] = {'+','-','*','/','<','>','=','!','^','&','|'}; /*,"+-/()* ,;$<=!|&>^@"); */
   char paren[2] = {'(',')'};

   zifmessage("mf4 version Jun 18, 2010 (64-bit)- RJB");
   
   /* get the function parameter and concatenate it */
   
   functionsize = 0;
   zvparm("function",funcparm,&npar,&idef,40,251);
   for (i=0;i<npar;i++) functionsize += (int)strlen(funcparm[i]);       /* cast - May 06, 2011 */
   mz_alloc1((unsigned char **)&function,functionsize+1,1);
   strcpy(function,funcparm[0]);
   for (i=1;i<npar;i++) strcat(function,funcparm[i]);
   sprintf(outmsg2,"function string = %s\n",function);
   zvmessage(outmsg2," ");
   zvp("seed",&seed,&npar);
   idebug=0;
   zvp("debug",&idebug,&npar);
   code=0;
   zvp("code",&code,&npar);
/*
	sprintf (outmsg,"debug = %d\n",idebug);   
	zvmessage(outmsg," ");
*/
   /* open the data set */
   
   status = zvunit(&unit,"inp",1,NULL);                     //64-bit
   status = IBISFileOpen(unit,&ibis,IMODE_UPDATE,0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&tablen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);			
  
/* the following prevents the message:              */
/*  *** glibc detected *** free(): invalid next size (fast): 0x0000000000640400 ***   */
/*  in the IBISColumnRead(ibis,&c_data[totwid[j]],i,1,tablen);  statement           */ 
  
    if (tablen == 0) {
        zmabend("??E Input file has 0 rows");
    }
    if (ncol == 0) {
        zmabend("??E Input file has 0 columns");
    }
 
    /* mxddwid = 50;    */
   mz_alloc1((unsigned char **)&sbuf,STRINGBUF,1);
   totwid[0] = 0;
   for (i=0;i<500;i++) blanks[i] = ' ';
   for (i=0;i<2;i++) zeros[i] = (char)0;

   /* get all of the unique field names in sequence and save the
      column names in c_field */
   snum = 0; cptr = 0; lptr = 0; savptr = 0; alphc = 0;
   for (i=0;i<NUMCOLS;i++) cnum[i] = -1;
/*   strl = strlen(funcparm); */
    strl = functionsize+1;
   for (i=0;i<=strl;i++)
      {
      c = function[i];
/*   sprintf (outmsg2,"c = %c\n",c);
    zvmessage(outmsg2," ");
*/
      if (c=='@')						/* @ */
	        do c = function[++i]; while (isalnum(c));
      if (c=='\'')						/* ' */
	        do c = function[++i]; while (c!='\'');
      c = (char)tolower(c);
/*	sprintf (outmsg,"c = %c\n",&c);			
	zvmessage(outmsg," ");
*/

      for (j=0;j<64;j++) if (c==cvec[j])		/* cvec[] contains legal characters */
	 {
	 c_field[cptr][lptr++] = c;			/* c_field will contain control column id, eg. (C12) */
	 snum *= 39;
	 if (snum>100000000) snum /= 31;
	 snum += j;
	 if (j>10 && lptr==1) alphc = 1;
	 goto nexti;
	 } /* for (j=0;j<64;j++) */
      if (snum*alphc!=0)
	 {
	 alphc = 0;
	 ksv = -1;
	if (idebug) {
	    sprintf (outmsg,"cptr = %d\n",cptr);
            zvmessage(outmsg," ");
        }

	 for (k=0;k<cptr;k++)
	    if (cnum[k]==snum) ksv = k;
	 if (ksv==(-1))
	    {
	    ksv = cptr;
	    cnum[cptr] = snum;
            if (cptr==NUMCOLS) zmabend("??E too many columns");
	    c_field[cptr++][lptr] = (char)0;
	    }
	 if (c=='=' && function[i+1]!='=')		/* check for = (not ==) */
	    {
	    savvec[savptr++] = ksv;
	    }
	 }
      snum = 0;
      lptr = 0;
/*	if (idebug) {
	    sprintf (outmsg,"cfield = %s",&c_field[0,0]);
            zvmessage(outmsg," ");
        }
*/
nexti: continue;
      }  /* for (i=0;i<=strl;i++) */
   
   /* read in the columns NEED LOGIC FOR STRINGS*/
	if (idebug) {
	    sprintf (outmsg,"Num of columns: cptr = %d",cptr);
            zvmessage(outmsg," ");
        }
   nincol = cptr;
   for (i=0;i<nincol;i++)				/* nincol is number of columns called */
      {
      datcols[i] = atoi(&c_field[i][1]);		/* datcols[i] is each column called */
 
	if (idebug) {
	    sprintf (outmsg,"cfield[%d][1] = %s  datcols[%d] = %d",i,&c_field[i][1],i,datcols[i]);    
            zvmessage(outmsg," ");
        }
      status = IBISColumnGet(ibis,"FORMAT",fmtstring,datcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
	if (idebug) {
	    sprintf (outmsg,"   fmtstring = %s",fmtstring);
            zvmessage(outmsg," ");
        }
      if (fmtstring[0]=='A')
         {
         wid[i] = ms_num(&fmtstring[1])+1;			/* ms_num is number of ASCII chars */
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
      } /* for (i=0;i<nincol;i++) */
  
   mz_alloc1((unsigned char **)&c_data,totwid[nincol],1);
   for (i=1;i<=ncol;i++) 					/* ncol is total columns in table */
      for (j=0;j<nincol;j++) 
	 if (datcols[j]==i)
	    {
	    status = IBISColumnRead(ibis,&c_data[totwid[j]],i,1,tablen);	/* c_data[] is slurpped in data from file */
            if (status!=1) IBISSignal(ibis,status,1);
            } /* terminates 2 for loops */
/*  This input buffer is written out as dbuf (below) in debug mode */ 
   /* iterate over functions separated by $, call
      knuth to parse and compile the function,
      and call xknuth to execute the function */

   srand48((long int)seed); savptr = 0;
   i = (int)strlen(function);               /* cast - May 06, 2011 */
   function[i] = '$';						/* append a $ */
   function[i+1] = (char)0;					/* append a nul */
   r = &function[0];

   for (ibig=0;;ibig++)
      {
        if (idebug) {
            sprintf (outmsg,"loop %d ---------",ibig);
	        zvmessage(outmsg," ");
        }
      p = r;
/************************************/
/* COLUMN OPERATIONS  @function(cx) */
/************************************/
/* @shift, @rot, @cdiff, @crsum, @csum, @cvmin, @cvmax, @cav, @csig, @count
*/  
      if (*p=='@')    /* this section traps column ops */		/* @ */
	 {
	 if (strncasecmp(p+1,"shift",5)==0)				/* if (*p=='@')      shift */
	    {
	    q = index(p,'(')+1;
/* int mtchfield(char *q, char fld[], int nincol);  */
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
	 if (strncasecmp(p+1,"rot",3)==0)				/* rot */
	    {
	    q = index(p,'(')+1;						/* ( */
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
	 if (strncasecmp(p+1,"cdiff",5)==0)				/* cdiff (col1,col2) */
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    ldiff = 0.; pval = 0.;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt], (unsigned char *)c_data);              // , &c_data);
	       if (val!=pval) ldiff = 0.;
	       ndiff = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data);
	       diff = ndiff-ldiff;
	       fstorecd(totwid[js]+i*wid[js],typ[js],diff,(unsigned char *)c_data);
	       pval = val; ldiff = ndiff;
	       }
	    }
	 if (strncasecmp(p+1,"crsum",5)==0)				/* crsum (col1,col2) */
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    sum = 0.; pval = 0.;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],(unsigned char *)c_data);		/* col typ=8 is real typ=0 is ASCII] */
	       if (val!=pval) sum = 0.;
	       sum += ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data);
	       fstorecd(totwid[js]+i*wid[js],typ[js],sum,(unsigned char *)c_data);
	       pval = val;
	       }
	    }
	 if (strncasecmp(p+1,"csum",4)==0 || strncasecmp(p+1,"cvmin",5)==0 ||  /* csum || cvmin || cvmax */
	     strncasecmp(p+1,"cvmax",5)==0)					/* (col1,col2) */
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    isv = 0; sum = 0.;
	    if (tablen>0)
	       {
	       pval = ffetchcd(totwid[jt],typ[jt],(unsigned char *)c_data);			/* col typ=8 is real typ=0 is ASCII] */
	       vmin = ffetchcd(totwid[js],typ[js],(unsigned char *)c_data);
	       vmax = vmin;
	       }
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],(unsigned char *)c_data);
	       val0 = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data);
	       if (val==pval)
		  {
		  sum += val0;
		  if (val0>vmax) vmax = val0;
		  if (val0<vmin) vmin = val0;
		  if (i<tablen-1) continue;
		  }
	       if (strncasecmp(p+1,"cvmin",5)==0 ) sum = vmin;			/* cvmin */
	       if (strncasecmp(p+1,"cvmax",5)==0 ) sum = vmax;			/* cvmax */
	       if (val==pval&&i==tablen-1) i++;
	       for (j=isv;j<i;j++)
		  fstorecd(totwid[js]+j*wid[js],typ[js],sum,(unsigned char *)c_data);
	       if (val!=pval&&i==tablen-1)
		  fstorecd(totwid[js]+i*wid[js],typ[js],val0,(unsigned char *)c_data);
	       isv = i; sum = val0; vmin = val0; vmax = val0; pval = val;
	       }
	    }
	 if (strncasecmp(p+1,"cav",3) ==0 || strncasecmp(p+1,"csig",4) ==0 ||
		strncasecmp(p+1,"count",5) ==0 )					/* cavg || csigma || count */
	    {
		if (idebug) {
		    sprintf (outmsg,"csig||cav||count\n");
	        zvmessage(outmsg," ");
        }
            q = index(p,'(')+1;
            js = mtchfield(q,c_field,nincol);					/* column to be modified */
            r = index(q,',')+1;
            jt = mtchfield(r,c_field,nincol);					/* control column */
            isv = 0; 
            pval = 0.; vmin = 1.0e30; vmax = -1.0e30;
            rstart=0;                                                           /* init starting row  of each pval */
	    rstop=0;								/* end row of each pval */

cavcsig:
	    rctr=0;
	    sum = 0.;								/* keep track of row count in each pval of control col */
            if (tablen>0)
               {								/* col typ=8 is real typ=0 is ASCII] */
/*		sprintf (outmsg,"rstart = %d, totwid[jt]+rstart*wid[jt] = %d\n",rstart,totwid[jt]+rstop*wid[jt]);  
		zvmessage(outmsg," ");
*/
               pval = ffetchcd(totwid[jt]+rstart*wid[jt],typ[jt],(unsigned char *)c_data);		/* pval is the value in control col to key on */
               vmin = ffetchcd(totwid[js]+rstart*wid[js],typ[js],(unsigned char *)c_data);		/* rstop is end record of each pval */
               vmax = vmin;
		if (idebug) {
		    sprintf (outmsg,"rstart = %d, pval = %7.1f\n",rstart,pval);
	        zvmessage(outmsg," ");
       	}
               }
		if (idebug) {
		    sprintf (outmsg,"tablen = %d, rstart = %d, js = %d, jt = %d, typ[js] = %d, typ[jt] = %d, pval = %7.1f, vmin=vmax = %5.1f\n",
			tablen,rstart,js,jt,typ[js],typ[jt],pval,vmin);
		    zvmessage(outmsg," ");
        }

            for (i=rstart;i<tablen;i++)						/* on first path get row count and sum */
               {
		val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],(unsigned char *)c_data);		/* get cntrol col val */
               val0 = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data);		/* get src col val */
		if (idebug) {
		    sprintf (outmsg,"cavg: i = %d, pval = %7.1f, val = %7.1f, val0 = %7.1f\n",i,pval,val,val0);
            zvmessage(outmsg," ");
	    }
               if (val==pval)							/* do following if same as control val */
                  {
                  sum += val0;
                  if (val0>vmax) vmax = val0;					/* set new max */
                  if (val0<vmin) vmin = val0;					/* set new min */
		  rctr++;							/* count rows */
		  rstop=i; 							/* mark as last row */
                  } else {
		   break;
                  }
		}  /* for (i=0;i<tablen;i++) */
                mean = sum/rctr; 						/* needed for csigma also */

             if (strncasecmp(p+1,"count",5) ==0)
		{
		if (idebug) {
		    sprintf (outmsg,"count: rstart = %d, rstop = %d, rctr = %d\n",rstart,rstop,rctr);
            zvmessage(outmsg," ");
	    }
                for (i=rstart;i<rstop+1;i++)             /* go all the way to rstop in case vals are not contiguous */
                   {
                       fstorecd(totwid[js]+i*wid[js],typ[js],(float)rctr,(unsigned char *)c_data);             /* store count in src col */
                   }
                rstart=rstop+1;
                if (rstop<tablen-1) goto cavcsig;          /* dont like goto's but keeps things in style of other routines */
                }

	     if (strncasecmp(p+1,"cav",3) ==0)
		{
             if (idebug) {
		sprintf (outmsg,"cavg: rstart = %d, rstop = %d, rctr = %d, mean = %9.1f\n",rstart,rstop,rctr,mean);
	        zvmessage(outmsg," ");
             }

                for (i=rstart;i<rstop+1;i++)                      /* go all the way to rstop in case vals are not contiguous */
 		   {
			if (idebug) {
			    sprintf (outmsg,"cavg: write: i = %d, pval = %6.1f, mean = %6.1f\n",i,pval,mean);
		            zvmessage(outmsg," ");
        		}
	                fstorecd(totwid[js]+i*wid[js],typ[js],mean,(unsigned char *)c_data);             /* store mean in src col */

		   }
		   rstart=rstop+1;
		   if (rstop<tablen-1) goto cavcsig;			/* dont like goto's but keeps things in style of other routines */
		}

	    if (strncasecmp(p+1,"csig",4) ==0 ) 
		{
                ssq = 0;
	    	for (i=rstart;i<rstop+1;i++)			/* go all the way to rstop in case vals are not contiguous */
		   {
		   	val = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],(unsigned char *)c_data);          /* get cntrol col val */
                  	val0 = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data) - mean;	/* subtract mean from input data */
               if (idebug) {
		    sprintf (outmsg,"csigma: i = %d, pval = %7.1f, val = %7.1f, val0 = %7.1f\n",i,pval,val,val0);
	            zvmessage(outmsg," ");
        	}
               	   if (val==pval)                                   		/* do following if same as control val */
		      {		  
                      ssq += val0*val0;

                      } else {
		       break;
		      }
                   }
		if (rctr < 2) {
		   vout=0.0;							/* in case 1 value or less */
		} else {
	           vout = sqrt(ssq/(rctr-1));						/* std dev goes into src col */
		}
		if (idebug) {
		    sprintf (outmsg,"ssq = %7.2f, rctr = %d, vout = %7.2f\n",ssq,rctr,vout);
		    zvmessage(outmsg," ");
		}
	        for (i=rstart;i<rstop+1;i++)                          /* go all the way to rstop in case vals are not contiguous */
		   {
	             if (idebug) {
			sprintf (outmsg,"csigma: write: i = %d, pval = %6.1f, vout = %6.1f\n",i,pval,vout);
	                zvmessage(outmsg," ");
       		     }		
		       fstorecd(totwid[js]+i*wid[js],typ[js],vout,(unsigned char *)c_data);	/* store std dev (sigma) in scr col */
	   	   }
		    rstart=rstop+1;
		   if (rstop<tablen-1) goto cavcsig;
		}
	  /*  }  for i=0;i<tablen; */

      } /* if (*p=='@') */  
/* END COLUMN OPS */
        if (strncasecmp(p+1,"fill",4)==0)                /* fill  */
        {
/* printf ("fill processing\n"); */
        q = index(p,'(')+1;
        js = mtchfield(q,c_field,nincol);
         val0 = ffetchcd(totwid[js],typ[js],(unsigned char *)c_data);
        for (ist=0;ist<tablen;ist++)
           {
           for (ii=ist+1;ii<tablen;ii++)
              {
              val2 = ffetchcd(totwid[js]+ii*wid[js],typ[js],(unsigned char *)c_data);
              if (val2!=0.) break;
              }  
           for (i=ist+1;i<ii;i++)
              {
              val1 = val0;
              fstorecd(totwid[js]+i*wid[js],typ[js],val1,(unsigned char *)c_data);
              }
           val0 = val2;
           ist = ii-1; if (ist==tablen-2) break;

          }
        }  /* if (mystrnicmp(p+1,"fill",4)==0) */
/* Before Jun 14, 2008 fill was processed in interp loop */

	 if (strncasecmp(p+1,"interp",6)==0)	/* interp */
	    {
	    q = index(p,'(')+1;
	    js = mtchfield(q,c_field,nincol);
	    r = index(q,',')+1;
	    jt = mtchfield(r,c_field,nincol);
	    val0 = ffetchcd(totwid[js],typ[js],(unsigned char *)c_data);
	    for (ist=0;ist<tablen;ist++)
	       {
	       for (ii=ist+1;ii<tablen;ii++)
		      {
		      val2 = ffetchcd(totwid[js]+ii*wid[js],typ[js],(unsigned char *)c_data);
		      if (val2!=0.) break;
		      }
	       cmp0 = ffetchcd(totwid[jt]+ist*wid[jt],typ[jt],(unsigned char *)c_data);
	       cmp2 = ffetchcd(totwid[jt]+ii*wid[jt],typ[jt],(unsigned char *)c_data);
	       dcmp = cmp2-cmp0;
	       if (dcmp<1.e-6 && dcmp>=0.) dcmp = 1.e-6;
	       if (dcmp>(-1.e-6) && dcmp<=0.) dcmp = -1.e-6;
	       dcmp = 1./dcmp;
	       for (i=ist+1;i<ii;i++)
		      {
		      cmp1 = ffetchcd(totwid[jt]+i*wid[jt],typ[jt],(unsigned char *)c_data);
		      val1 = (cmp1-cmp0)*(val2-val0)*dcmp+val0;
		      fstorecd(totwid[js]+i*wid[js],typ[js],val1,(unsigned char *)c_data);
		      }
	       val0 = val2;
	       ist = ii-1; if (ist==tablen-2) break;
	       }
	    } /* if (mystrnicmp(p+1,"interp",6)==0 || mystrnicmp(p+1,"fill",4)==0) */

	 if (strncasecmp(p+1,"sum",3)==0  || strncasecmp(p+1,"av",2)==0		/* sum || av  */
	    || strncasecmp(p+1,"sig",3)==0					/*  || sig */
	    || strncasecmp(p+1,"vmin",4)==0  || strncasecmp(p+1,"vmax",4)==0	/*  || vmin || vmax */
	    || strncasecmp(p+1,"rsum",4)==0  || strncasecmp(p+1,"diff",4)==0)	/*  || rsum || diff */
	    {
	if (idebug) {
	    sprintf (outmsg,"sum||av||sig||vmin||vmax||rsum||diff\n");
            zvmessage(outmsg," ");
        }
	    q = index(p,'(')+1;                 //point to loc after '(' - the next chars are cN where N=col num
//        printf ("rsum - c_field[0][0]  = %c%c %c%c <\n",c_field[0][0],c_field[0][1],c_field[0][2],c_field[0][3]);
        
        js = mtchfield(q,c_field,nincol);
	    sum = 0.; pval = 0.; vmin = 1.0e30; vmax = -1.0e30;
	    for (i=0;i<tablen;i++)
	       {
	       val = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data);
	       if (val>vmax) vmax = val;
	       if (val<vmin) vmin = val;
	       sum += val;
	       if (strncasecmp(p+1,"rsum",4)==0)					/* rsum */
		  fstorecd(totwid[js]+i*wid[js],typ[js],sum,(unsigned char *)c_data);
	       if (strncasecmp(p+1,"diff",4)==0)					/* diff */
		  fstorecd(totwid[js]+i*wid[js],typ[js],val-pval,(unsigned char *)c_data);
	       pval = val;
	       }
	    vout = sum;
		if (idebug) {
		    sprintf (outmsg,"tablen = %d, js = %d, jt = %d, pval = %f, vmin=vmax = %f\n",
			tablen,js,jt,pval,vmin);
	            zvmessage(outmsg," ");
        }
	    if (strncasecmp(p+1,"av",2)==0 && tablen!=0) vout = sum/tablen;	/* av */
	    if (strncasecmp(p+1,"sig",3)==0 && tablen!=0)			/* sig */
	       {
	       mean = sum/tablen; ssq = 0;
	       for (i=0;i<tablen;i++)
		  {
		  val = ffetchcd(totwid[js]+i*wid[js],typ[js],(unsigned char *)c_data) - mean;
		  ssq += val*val;
		  }
	       vout = sqrt(ssq/(tablen-1));
	       }
	    if (strncasecmp(p+1,"vmin",4)==0 ) vout = vmin;				/* vmin */
	    if (strncasecmp(p+1,"vmax",4)==0 ) vout = vmax;				/* vmax */
	    if (strncasecmp(p+1,"sum",3)==0  || strncasecmp(p+1,"av",2)==0		/* sum  || av */
		  || strncasecmp(p+1,"sig",3)==0						/* || sig */	
		  || strncasecmp(p+1,"vmin",4)==0  || strncasecmp(p+1,"vmax",4)==0)	/* || vmin || vmax  */
	       for (i=0;i<tablen;i++)
		  fstorecd(totwid[js]+i*wid[js],typ[js],vout,(unsigned char *)c_data);
	    } /*if (mystrnicmp(p+1,"sum",3)==0  || mystrnicmp(p+1,"av",2)==0 */
/* GEOPHYSICAL COLUMN OPS  only last field requires a column number */
	 if (strncasecmp(p+1,"dist",4)==0)						/* dist -   @dist(lon1,lat1,lon2,lat2,dist) */
	    {                                                   /* @dist(-1.130000000000e+02,4.100000000000e+01,c3,c4,c11) */

	    q = index(p,'(')+1;
	    j1 = mtchfield2(q,c_field,nincol,value1);
	    r = index(q,',')+1;
	    j2 = mtchfield2(r,c_field,nincol,value2);
	    q = index(r,',')+1;
	    j3 = mtchfield2(q,c_field,nincol,value3);
	    r = index(q,',')+1;
	    j4 = mtchfield2(r,c_field,nincol,value4);
	    q = index(r,',')+1;
	    j5 = mtchfield(q,c_field,nincol);
	    raddeg = 1./degrad;  mpr = 1.1132e5*degrad;
	    val1 = 0.; val2 = 0.;
	    for (i=0;i<tablen;i++)
	       {
            if (j1<0) {
                tht1 = value1;
            } else {    
	            tht1 = ffetchcd(totwid[j1]+i*wid[j1],typ[j1],(unsigned char *)c_data) * raddeg;
            }
            if (j2<0) {
                phi1 = value2;
            } else {
	            phi1 = ffetchcd(totwid[j2]+i*wid[j2],typ[j2],(unsigned char *)c_data) * raddeg;
            }
            if (j3<0) {
                tht2 = value3;
            } else {
	            tht2 = ffetchcd(totwid[j3]+i*wid[j3],typ[j3],(unsigned char *)c_data) * raddeg;
            }
            if (j4<0) {
                phi2 = value4;
            } else {
	            phi2 = ffetchcd(totwid[j4]+i*wid[j4],typ[j4],(unsigned char *)c_data) * raddeg;
            }
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
	       fstorecd(totwid[j5]+i*wid[j5],typ[j5],val,(unsigned char *)c_data);
	       }
	    }

	 if (strncasecmp(p+1,"head",4)==0 || strncasecmp(p+1,"bear",4)==0)		/* head || bear */
	    {
	    q = index(p,'(')+1;
	    j1 = mtchfield2(q,c_field,nincol,value1);
	    r = index(q,',')+1;
	    j2 = mtchfield2(r,c_field,nincol,value2);
	    q = index(r,',')+1;
	    j3 = mtchfield2(q,c_field,nincol,value3);
	    r = index(q,',')+1;
	    j4 = mtchfield2(r,c_field,nincol,value4);
	    q = index(r,',')+1;
	    j5 = mtchfield2(q,c_field,nincol,value5);  j7 = j5;
	    if (strncasecmp(p+1,"bear",4)==0)						/* bear */
	       {
	       r = index(q,',')+1;
	       j6 = mtchfield2(r,c_field,nincol,value6);
	       q = index(r,',')+1;
	       j7 = mtchfield(q,c_field,nincol);
	       }
	    raddeg = 1./degrad; val1 = 0; val2 = 0;
	    for (i=0;i<tablen;i++)
	       {
            if (j1 <0) {
                tht1 = value1;
            } else {
	            tht1 = ffetchcd(totwid[j1]+i*wid[j1],typ[j1],(unsigned char *)c_data) * raddeg;
            }
            if (j2<0) {
                phi1 = value2;
            } else {
	            phi1 = ffetchcd(totwid[j2]+i*wid[j2],typ[j2],(unsigned char *)c_data) * raddeg;
	        }
            if (j3<0) {
                tht2 = value3;
            } else {
                tht2 = ffetchcd(totwid[j3]+i*wid[j3],typ[j3],(unsigned char *)c_data) * raddeg;
            }
            if (j4<0) {
                phi2 = value4;
            } else {
	            phi2 = ffetchcd(totwid[j4]+i*wid[j4],typ[j4],(unsigned char *)c_data) * raddeg;
            }
	       if (strncasecmp(p+1,"bear",4)==0)						/* bear */
		  {
            if (j5<0) {
                tht3 = value5;
            } else {
		        tht3 = ffetchcd(totwid[j5]+i*wid[j5],typ[j5],(unsigned char *)c_data) * raddeg;
            }
            if (j6<0) {
                phi3 = value6;
            } else {
		        phi3 = ffetchcd(totwid[j6]+i*wid[j6],typ[j6],(unsigned char *)c_data) * raddeg;
            }
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
	       fstorecd(totwid[j7]+i*wid[j7],typ[j7],val,(unsigned char *)c_data);
	       }
	    } /* if (mystrnicmp(p+1,"head",4)==0 || mystrnicmp(p+1,"bear",4)==0)  */
/* END GEOPHYSICAL COLUMN OPS */
/* r = function string  with $ appended */
    r = index(q,'$');
	do r++; while (*r=='$');
	if (strlen(r)==0) break;
	continue;
	}  /* for (ibig=0;;ibig++) */

/* NOW, parse the remainder of function after = sign */
    q = index(p,'=')+1;		/* find =  and point to remainder of expression */
	qlp = index(p,'$');
    if (idebug) {
	    sprintf (outmsg2,">%s",&q[0]);
	    zvmessage(outmsg2," ");
    }
    lparfound = 0;
    diff = (double)(qlp -q);
    ifop=0;
/* simple ( and ops checks (needs to be smarter) */	
    for (m=0;m<diff;m++) {
/*	sprintf (outmsg2,"m = %d &q = %c",m,q[m]);
	zvmessage(outmsg2, " ");
*/
	    if (q[m] == paren[0]) {		/* check for ( */
             lparfound = 1;
        }
	    for (j=0;j<11;j++) {		/* check for op? */
	        if (q[m] == fop[j]) {
	        ifop++;
            }
        }
	    if (q[m] == 'e') {
		    --ifop;
        }
    } /* for (m=0;m<diff;m++) { */
    if (ifop > 1 && lparfound == 0) {
        zvmessage("parentheses required for function"," ");
	    zmabend ("??E bad statement");
    } 
/* need yet to ensure that two ops are not adjacent 		*/
/* to do this need to strip out embedded blanks 		    */
/* check for ( after @signs 					            */
/* have to be careful about ! and !=, > and >= and >>, 		*/
/* < and <= and <<						                    */
/* need to issue warnings about >-,>+,<-,<+,>=-,>=+,<=-,<=+	*/
/* ==+,==-,!=+,!=-,+-,++,-+,--,||-,||+,&&+,&&-,*-,*+,/+,/-  */
/* ^+,^-							                        */
/* combos (need parenthesis separators)				        */
/* Not sure about c-like +=,-= combos				        */
    r = index(q,'$');			/* find terminating $       */
    do r++; while (*r=='$');
    rcol = savvec[savptr++];
    sptr = 0;
    sp_knuth(q,ibuf,dbuf,sbuf,cnum,&sptr);
    if (idebug) {
	    sprintf(outmsg,"\n");
        zvmessage(outmsg," ");
    }
    svsptr = sptr;

      /* rand() changed to drand48() */
    for (i=0;i<tablen;i++)
    { 
	    sptr = svsptr;
	/* assemble input buffer into double format */
	    for (j=0;j<nincol;j++)
	    {
	    if (typ[j]!=0) {
	       dbuf[j] = ffetchcd(totwid[j]+i*wid[j],typ[j],(unsigned char *)c_data);
/* row in following print is in ibis convention - not c */
		    if (idebug || code) { 
            	     sprintf (outmsg,"<<   original value in row %d col %d = %f",i+1,datcols[j],dbuf[j]);
		        zvmessage(outmsg," ");
		    } 
        }
	    else
	        {
	        dbuf[j] = (double)sptr;
	        /*bcopy(&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],wid[j]);*/
	        zmve(1,wid[j],&c_data[totwid[j]+i*wid[j]],&sbuf[sptr],1,1);
	        sptr += wid[j]+1;
	        sbuf[sptr-1] = (char)0;
	        }
	    } /* for (j=0;j<nincol;j++) */
	dbuf[102] = drand48();
	dbuf[101] = (double)(i+1);
	/*debugrec1 = i;    */			/* debugrec1 = i==0;  */
/*	 if (idebug) {
	     sprintf (outmsg,">>> debug = %d   debugrec1 = %d",idebug,debugrec1);
             zvmessage(outmsg," ");
         }
*/
	sp_xknuth(ibuf,dbuf,sbuf,&sptr,&res,code);

    if (idebug) {
        sprintf (outmsg,"   result = %f datcols[%d] = %d",res,rcol,datcols[rcol]);
	    zvmessage(outmsg," ");
	}

	if (typ[rcol]!=0) {

	    fstorecd(totwid[rcol]+i*wid[rcol],typ[rcol],res,(unsigned char *)c_data);
	    }
	 else
	    {
	    ires = (int)(res+.001);
	    lres = MIN((int)strlen(&sbuf[ires]),wid[rcol]);         /* cast - may 06, 2011 */
	    	/*bcopy(&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],lres);*/
	    zmve(1,lres,&sbuf[ires],&c_data[totwid[rcol]+i*wid[rcol]],1,1);
  		/*bcopy(blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],wid[rcol]-lres);*/
  	    zmve(1,wid[rcol]-lres-1,blanks,&c_data[totwid[rcol]+i*wid[rcol]+lres],1,1);
  	    zmve(1,1,zeros,&c_data[totwid[rcol]+(i+1)*wid[rcol]-1],1,1);
	    }
        if (idebug || code) {
             sprintf (outmsg,">>   output value in row %d col %d = %f",i+1,datcols[rcol],res);
             zvmessage(outmsg," ");
	    }
	 } /* for (i=0;i<tablen;i++) */
      if (strlen(r)==0) break;
}
   /* write the result to the file */

    for (i=1;i<=ncol;i++) { 
        for (j=0;j<nincol;j++) { 
	        if (datcols[j]==i&&tablen>0)
	        {
	        status = IBISColumnWrite(ibis,&c_data[totwid[j]],i,1,tablen);
            if (status!=1) IBISSignal(ibis,status,1);
            }
    /* print in/out file lengths */
        }
    } 
    sprintf(outmsg,"%d records in\n",tablen);
    zvmessage(outmsg," ");

    /* close files */
   
    status = IBISFileClose(ibis,0);
    if (status!=1) IBISSignal(ibis,status,1);
    return;
}
