/* pixstat.c - removed defines and prototypes to  pixstat.h */

#include "pixstat.h"
#include "vicmain_c.h"  /* changed from vicmain_c - 5/10/2011 */
/* globals */
int iunit,sl,ss,nl,ns;
int ounit,slo,sso,nlo,nso,elo;
int nlw,nsw;		/* filter window size is nlw x nsw */
int nlwh,nswh;
double scale,mindn,maxdn;

void main44()
{
  float *ibuf;    /* nlw+1 consecutive image lines = ibuf[nlw+1][ns] -- formerly short */
  float *mean;    /* mean of image line = mean[nso] */
  double *moment; /* 2nd moment of image line = moment[nso] */
  float *vs;        /* vs[i] = sum of DN in column i  -- formerly int */
  double *vs2;    /* vs2[i] = sum of DN**2 in column i */

  float rscale;
  int sli,ssi,nli,nsi,el,es;
  int icode,ocode;
  int cnt,status;              /* status; */
  int opcode;			/* 1=mean, 2=moment, 3=variance, 4=sdev */
  char format[5],oformat[5];	/* input and output data formats */
  char msg[80];
  char fmt[4][5]={"BYTE","HALF","FULL","REAL"};

  zvmessage("pixstat - Sep 17, 2013 (64-bit) - RJB"," ");

  status = zvunit(&iunit,"INP",1,NULL);
  zvopen(iunit,"OPEN_ACT","SA","IO_ACT","SA",NULL);
  status = zvget(iunit,"FORMAT",format,NULL);
        if (status != 1) {
            zmabend ("??E - zvget fail");
        }
  zvclose(iunit,NULL);

  icode = 0;
  if (!strcmp(format,"BYTE")) icode=1;
  if (!strcmp(format,"HALF")) icode=2;
  if (!strcmp(format,"FULL")) icode=4;
  if (!strcmp(format,"REAL")) icode=7;
  if (icode == 0) goto WRONG_FORMAT;
  //reopen
  zvopen(iunit,"U_FORMAT","REAL","I_FORMAT",fmt[icode],"OPEN_ACT","SA","IO_ACT","SA",NULL);

  zvsize(&sli,&ssi,&nlo,&nso,&nli,&nsi);
  sli--;	/* pixstatel coordinates start at (0,0) rather than (1,1) */
  ssi--;

  if (nlo>nli-sli) nlo=nli-sli;		/* Check size of output image */
  if (nso>nsi-ssi) nso=nsi-ssi;

  ocode=0;
  zvp("FORMAT",oformat,&cnt);   /* if cnt = 1 then format entered, if cnt = 0 then "--" */
  if (cnt == 0) strcpy(oformat,format);
  if (!strcmp(oformat,"BYTE")) ocode=1;
  if (!strcmp(oformat,"HALF")) ocode=2;
  if (!strcmp(oformat,"FULL")) ocode=4;
  if (!strcmp(oformat,"REAL")) ocode=7;
  status = zvunit(&ounit,"OUT",1,NULL);
  zvopen(ounit,"OP","WRITE","U_FORMAT","REAL","O_FORMAT",oformat,
        "U_NL",nlo,"U_NS",nso,"OPEN_ACT","SA","IO_ACT","SA",NULL);

  maxdn = 0.;
  if (ocode==1) {
     mindn = 0;
     maxdn = 255;
  }
  if (ocode==2) {
     mindn = -32768.;
     maxdn = 32767.;
  }
  if (ocode==4) {
     mindn = -32768.*65536.;
     maxdn = 32768.*65536.-1.;
  }

	/* Get size of filter window */
  zvp("NLW",&nlw,&cnt);
  zvp("NSW",&nsw,&cnt);
  nlwh = nlw/2;
  nswh = nsw/2;
  nlw = 2*nlwh + 1;		/* Force window to have odd value dimensions */
  nsw = 2*nswh + 1;
  sprintf(msg,"NLW=%d NSW=%d",nlw,nsw);
  zvmessage(msg," ");
  
  opcode=0;
  if (zvptst("MEAN")) opcode=1;
  if (zvptst("MOMENT")) opcode=2;
  if (zvptst("VARIANCE")) opcode=3;
  if (zvptst("SDEV")) opcode=4;

  zvp("SCALE",&rscale,&cnt);
  scale = rscale;

	/* Compute size of input image needed to create output image */
  sl = sli - nlwh;
  ss = ssi - nswh;
  el = sli + nlo + nlwh - 1;
  es = ssi + nso + nswh - 1;
  if (sl < 0) sl=0;
  if (ss < 0) ss=0;
  if (el >= nli) el=nli-1;
  if (es >= nsi) es=nsi-1;
  nl = el - sl + 1;
  ns = es - ss + 1;	/* Final input size is (sl,ss,nl,ns) */

	/* Compute where the output image starts and ends */
  slo = sli - sl;
  sso = ssi - ss;
  elo = slo + nlo - 1;

    /*  ibuf = (short *) malloc((nlw+1)*ns*sizeof(short)); */
  ibuf = (float *) malloc(((long unsigned int)nlw+1)*(long unsigned int)ns*sizeof(float));
    /*   vs = (int *) malloc(ns*sizeof(int));  */
  vs = (float *) malloc((long unsigned int)ns*sizeof(float));
  mean = (float *) malloc((long unsigned int)ns*sizeof(float));

  if (opcode == 1) compute_mean(ibuf,mean,vs);
  else {
     vs2 = (double *) malloc((long unsigned int)ns*sizeof(double));
     moment = (double *) malloc((long unsigned int)ns*sizeof(double));
     compute_sigma(opcode,ibuf,mean,moment,vs,vs2);
     free(moment);
     free(vs2);
  }
  zvclose(iunit,NULL);
  free(vs);
  free(mean);
  free(ibuf);
  return;

WRONG_FORMAT:
  zvmessage("??E - Input image must be in byte format"," ");
//FATAL_ERROR:
//  zvmessage("***pixstat task cancelled",0);
//  zabend();
}
/*****************************************************************************/
/* Compute mean image.							     */
/*****************************************************************************/
void compute_mean(
	float *ibuf,	/* nlw+1 consecutive image lines = ibuf[nlw+1][ns] ----  formerly short*/
	float *mean,	/* output image line = mean[nso] */
	float *vs)	/* vs[i] = sum of DN in column i ---    formerly int */
{
  int l,s,n0;
  float *top,*bot,*max;             /*formerly short */

  scale = scale/(nlw*nsw);
  max = &ibuf[nlw*ns];
  n0 = nlwh + 1;	/* vertical pixstatel distance to middle of window */

	/* Initialize column sums with pixstatels from top margin of image */
  zvread(iunit,ibuf,"LINE",sl+1,"SAMP",ss+1,"NSAMPS",ns,NULL);
  for (s=0; s<ns; s++) vs[s]=ibuf[s];
  bot = ibuf + ns;
  for (l=1; l<=nlwh; l++) {
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) vs[s]+=2*bot[s];
     bot += ns;
  }

	/* Filter top margin */
  top = bot - ns;
  for (l=0; l<nlwh; l++) {
     if (l >= slo) {
        unit_filter(vs,mean,ns,nsw,scale);
        output(ounit,&mean[sso],nso,mindn,maxdn);
     }
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     top -= ns;
     bot += ns;
  }

	/* Filter internal lines */
  for (l=nlwh; l<nl-n0; l++) {
     unit_filter(vs,mean,ns,nsw,scale);
     output(ounit,&mean[sso],nso,mindn,maxdn);
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     bot = top;
     top += ns;
     if (top > max) top=ibuf;
  }

	/* Point bottom to next to last line in image to start reflection */
/*  l = (nl-2) - (nlw+1)*((nl-2)/(nlw+1));
  bot = &ibuf[l*ns]; */
  bot -= ns;
  if (bot < ibuf) bot=max;
  bot -= ns;
  if (bot < ibuf) bot=max;
  
	/* Filter bottom margin */
  for (l=nl-n0; l<nl; l++) {
     unit_filter(vs,mean,ns,nsw,scale);
     output(ounit,&mean[sso],nso,mindn,maxdn);
     if (l >= elo) break;
     for (s=0; s<ns; s++) vs[s]=vs[s]-top[s]+bot[s];
     top += ns;
     bot -= ns;
     if (bot < ibuf) bot=max;
     if (top > max) top=ibuf;
  }
}
/*****************************************************************************/
/* Compute moment, variance, or sigma image.				     */
/*****************************************************************************/
void compute_sigma(
	int opcode,	/* 2=moment, 3=variance, 4=sigma */
	float *ibuf,    /* nlw+1 consecutive image lines = ibuf[nlw+1][ns]   ---- formerly short */
	float *mean,    /* mean of image line = mean[nso] */
	double *moment, /* second moment of image line = moment[nso] */
	float *vs,        /* vs[i] = sum of DN in column i  ---- formerly int */
	double *vs2)    /* vs2[i] = sum of DN**2 in column i */
{
  int l,s,n0,dn,dn0;
  float *top,*bot,*max;         /* formerly short */

  n0 = nlwh + 1;	/* vertical pixstatel distance to middle of window */
  max = &ibuf[nlw*ns];

	/* Initialize column sums with pixstatels from top margin of image */
  zvread(iunit,ibuf,"LINE",sl+1,"SAMP",ss+1,"NSAMPS",ns,NULL);
  for (s=0; s<ns; s++) {
     dn = (int)ibuf[s];
     vs[s] = (float)dn;
     vs2[s] = (float)(dn*dn);
  }

  bot = ibuf + ns;
  for (l=1; l<=nlwh; l++) {
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) {
        dn = (int)bot[s];
        vs[s] += (float)(2*dn);
        vs2[s] += (float)(2*dn*dn);
     }
     bot += ns;
  }

	/* Filter top margin */
  top = bot - ns;
  for (l=0; l<nlwh; l++) {
     if (l >= slo) {
        unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
        output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     }
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) {
         dn0 = (int)top[s];
         dn  = (int)bot[s];
         vs[s] = vs[s] - (float)dn0 + (float)dn;
         vs2[s] = vs2[s] - dn0*dn0 + dn*dn;
     }
     top -= ns;
     bot += ns;
  }

	/* Filter internal lines */
  for (l=nlwh; l<nl-n0; l++) {
     unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
     output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     zvread(iunit,bot,"SAMP",ss+1,"NSAMPS",ns,NULL);
     for (s=0; s<ns; s++) {
        dn0 = (int)top[s];
        dn  = (int)bot[s];
        vs[s] = vs[s] - (float)dn0 + (float)dn;
        vs2[s] = vs2[s] - (float)(dn0*dn0) + (float)(dn*dn);
     }
     bot = top;
     top += ns;
     if (top > max) top=ibuf;
  }

	/* Point bottom to next to last line in image to start reflection */
  bot -= ns;
  if (bot < ibuf) bot=max;
  bot -= ns;
  if (bot < ibuf) bot=max;
  
	/* Filter bottom margin */
  for (l=nl-n0; l<nl; l++) {
     unit_filter2(opcode,vs,vs2,mean,moment,ns,nlw,nsw);
     output2(ounit,mean,&moment[sso],nso,mindn,maxdn,scale);
     if (l >= elo) break;
     for (s=0; s<ns; s++) {
        dn0 = (int)top[s];
        dn  = (int)bot[s];
        vs[s] = vs[s] - (float)(dn0 + dn);
        vs2[s] = vs2[s] - (float)(dn0*dn0) + (float)(dn*dn);
     }
     top += ns;
     bot -= ns;
     if (bot < ibuf) bot=max;
     if (top > max) top=ibuf;
  }
}
