$!****************************************************************************
$!
$! Build proc for MIPL module picscale
$! VPACK Version 1.9, Monday, December 07, 2009, 16:31:46
$!
$! Execute by entering:		$ @picscale
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module picscale ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to picscale.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("picscale.imake") .nes. ""
$   then
$      vimake picscale
$      purge picscale.bld
$   else
$      if F$SEARCH("picscale.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake picscale
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @picscale.bld "STD"
$   else
$      @picscale.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create picscale.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack picscale.com -mixed -
	-s picscale.c lighting.f zlighting.c spice2convev.f zspice2convev.c -
	   rpicscale.c -
	-i picscale.imake -
	-t tzpicscale.c txpicscale.f tzpicscale.imake tzpicscale.pdf -
	   tstpicscale.pdf tzlighting.c tzlighting.pdf tzlighting.imake -
	   picscale.data -
	-o picscale.hlp rpicscale.hlp lighting.hlp spice2convev.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create picscale.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdlib.h>
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "mp_routines.h" 

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

int   picscal1(float *,double,double,double,double,double *);
void  picscal2(double,double,double,double,double,double,double *); 
void  picscal3(double,double,double,double,double,double,double,double *);
int   psll2xy(float *,double,double,double *,double *);
int   psxy2ll(float *,double,double,double *,double *,float *);
int   xy2ll(MP,double,double,double *,double *);
/*
int  *matrixmult(double *,double *,double *);
*/
double zrpd(void);
double zdpr(void);

/*****************************************************************************
 ***   FORTRAN BRIDGE: "picscale"                                             
 *****************************************************************************/
void FTN_NAME2 (picscale, PICSCALE) (sbuf,data,mp,mptype,sclat,sclon,nl,ns,
							pbuf,line,samp)
double *sbuf;
float  *data;
MP      mp; 
double *sclat,*sclon;
int    *mptype,*nl,*ns;
float  *pbuf;
double *line,*samp;
{
    zpicscale(sbuf,data,mp,*mptype,*sclat,*sclon,*nl,*ns,pbuf,line,samp);
}
/******************************************************************************/
/***   "zpicscale"  subroutine                                                */
/******************************************************************************/
int zpicscale(sbuf,data,mp,mptype,sclat,sclon,nl,ns,pbuf,line,samp)
double *sbuf;		/* input SPICE buffer */
float  *data;           /* input 40-word geometry buffer for PPROJ call */
MP      mp;		/* Pointer to map projection data */
int     mptype;         /* Map projection type */
double  sclat,sclon;	/* spacecraft latitude/longitude */
int     nl,ns;		/* size of input image */
float  *pbuf;		/* output buffer */
double *line,*samp;	/* output coordinates at which scale applies */
{

   double  fl,oal,oas,scale;
   float  srange;
   double scrange,sunrange;	/* spacecraft and sun range from targ center */
   double vsc[3],vsun[3];	/* spacecraft and solar vectors */
   double om[3][3];		/* tranformation from target to camera coords */
   double ra,rb,rc;	/* radii for triaxial ellipse */
   double re,rp;	/* equatorial and polar radius */
   double pcl,pcs;	/* line,rsamp of planet center */
   double rnl,rns;	/* number of lines and samples in image */
   double rsamp,rline;
   double rlat,rlon,rlat2,rlon2;
   double p[3],na,noraz,sunaz,scaz,sunlat,sunlon;
   double hscale,vscale,hscale_pc,vscale_pc;
   double lora;		/* longitude of semi-major axis (unknown) */
   double gcr;		/* geocentric radius */
   double x,y,z,d;
   int    inc,ind,ind1,ind2,ind3,ind4;
   int    ibeg,iend,i,use_pc;
   char   projection[mpMAX_KEYWD_LENGTH];
   char   direction[mpMAX_KEYWD_LENGTH];
	   /*  INITIALIZE VARIABLES */
   for (i=0; i<6; i++) *(pbuf+i) = -999.0; 
   hscale = 1.0e+20;		/* initialize as invalid scale */
   vscale = 1.0e+20;
   rnl = (double) nl; 
   rns = (double) ns; 

   if (mptype!=7 && mptype!=8) {
      ind = mpGetValues(mp,"MAP_PROJECTION_TYPE",projection,"");   
      if (strncmp(projection,"POINT_PERSPECTIVE",17)==0) {
         mptype = 16;
         mpMpo2Buf(mp,data);
         sclat = data[30];
         sclon = data[31];
      }
   }
	/* If image is map projected, compute scale at center of image */
   if (mptype != 7 && mptype != 8 && mptype != 16) {
      ind = mpGetValues(mp,"A_AXIS_RADIUS",&ra,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"B_AXIS_RADIUS",&rb,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"C_AXIS_RADIUS",&rc,"");   
      if (ind != mpSUCCESS) goto ERROR1;
      ind = mpGetValues(mp,"POSITIVE_LONGITUDE_DIRECTION",direction,"");
      *line = (double) nl/2.0;		/* line,samp at center of image */
      *samp = (double) ns/2.0;
      rline = (double) *line; 
      rsamp = (double) *samp;
      ind = mpxy2ll(mp,rline,rsamp,&rlat,&rlon,1);
      if (ind != mpSUCCESS) goto ERROR2;
      if (strncmp(direction,"WEST",4)==0) rlon=360.-rlon;
      ind = mpxy2ll(mp,rline,rsamp+1.0,&rlat2,&rlon2,1);
      if (ind == mpSUCCESS) {
         if (strncmp(direction,"WEST",4)==0) rlon2=360.-rlon2;
         picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,&hscale);
         pbuf[0] = (float) hscale;
      }
      ind = mpxy2ll(mp,rline+1.0,rsamp,&rlat2,&rlon2,1);
      if (ind == mpSUCCESS) {
         if (strncmp(direction,"WEST",4)==0) rlon2=360.-rlon2;
         picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,&vscale);
         pbuf[1]=(float) vscale;
      }
      return ind;
   }

/* Here if mptype=7, 8, or 16 (image or object space).  Geometric camera
   distortions will be ignored (image is treated as object-space frame). */

   zmve(8,9,(sbuf+58),om,1,1);
   zmve(8,3,(sbuf+21),vsc,1,1);
   ra = *(sbuf+12);
   rb = *(sbuf+13);
   rc = *(sbuf+14);
   fl = data[26];
   oal = data[27];
   oas = data[28];
   scale = data[29];
   re = (double) data[25];		/* equitorial radius (km) */
   rp = (double) data[24];		/* polar radius */
	/* See if all four corners are on the target body... */
   ind1 = psxy2ll(data,1.,1.,&rlat,&rlon,&srange);  /* upper left corner */
   ind2 = psxy2ll(data,1.,rns,&rlat,&rlon,&srange); /* upper right corner */
   ind3 = psxy2ll(data,rnl,1.,&rlat,&rlon,&srange); /* lower left corner */
   ind4 = psxy2ll(data,rnl,rns,&rlat,&rlon,&srange);/* lower right corner */
   use_pc = 0; 
	/* If all 4 corners are on planet, image is high-resolution */
	/* compute scale at center of image */
   if (ind1==1 && ind2==1 && ind3==1 && ind4==1) { 
      *line = (double) nl/2.0;		/* line,samp at center of image */
      *samp = (double) ns/2.0;
      rline = (double) *line; 
      rsamp = (double) *samp;
      ind1 = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
      ind2 = psxy2ll(data,rline,rsamp+1.,&rlat2,&rlon2,&srange);
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&hscale); 
      ind3 = psxy2ll(data,rline+1.,rsamp,&rlat2,&rlon2,&srange);
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&vscale); 
      goto COMPUTE_AZIMUTHS;
   }
	/* Here if 4 corners not on planet (low-resolution image */
	/* Calculate scale at planet center */
   ind = psll2xy(data,sclat,sclon,&pcl,&pcs);
   if (ind != 1) return ind;
   ind = psxy2ll(data,pcl,pcs+0.1,&rlat2,&rlon2,&srange);
   if (ind != 1) return ind;
   picscal2(sclat,sclon,rlat2,rlon2,re,rp,&hscale_pc);	/* horizontal scale */
   hscale_pc*=10.0; /* To accomodate for pcl+0.1 */
   ind = psxy2ll(data,pcl+0.1,pcs,&rlat2,&rlon2,&srange);
   if (ind != 1) return ind;
   picscal2(sclat,sclon,rlat2,rlon2,re,rp,&vscale_pc); 	/* vertical scale */
   vscale_pc*=10.0; /* To accomodate for pcl+0.1 */
   use_pc = 1;
	/* If PC is in the image, use scale at PC */
   if (pcl>0 && pcs>0 && pcl<=rnl && pcs<rns) goto COMPUTE_AZIMUTHS;
	/* If the target is smaller than the width of image, use scale at PC */
   if (1000.*re/hscale > rnl) goto COMPUTE_AZIMUTHS;

	/* Search margin of image for point on planet with the
	   largest scale */
   inc = 20;			/* margin search increment is 20 pixels */
   if (pcl < pcs) {		/* top or right margin */
      if (pcl < rns-pcs) {	/* top margin */
         *line = 1; 
         if (pcs < 1.0) {	/* top-left corner */
            ibeg = 1; 
            iend = 1; 
         } 
         else if (pcs > rns) {  /* top-right corner */
            ibeg = ns; 
            iend = ns; 
         } 
         else {			/* entire top margin */
            ibeg = 1; 
            iend = ns; 
         } 
             /* Search for smallest scale along margin */
         rline = 1.0;
         for (i=ibeg; i<=iend; i+=inc) { 
             rsamp = (double) i; 
             ind = picscal1(data,rline,rsamp,re,rp,&hscale);
             if (ind == 1) *samp = i;
         } 
      }

      else {			/* Right margin */
         *samp = ns;
         if (pcl < 1.0) {	/* top-right corner */
            ibeg = 1; 
            iend = 1; 
         } 
         else if (pcl > rnl) {	/* bottom-right corner */
            ibeg = nl; 
            iend = nl; 
         } 
         else { 		/* entire right margin */
            ibeg = 1; 
            iend = nl; 
         } 

            /* Search for smallest scale along margin */
         rsamp = rns;
         for (i=ibeg; i<=iend; i+=inc) { 
            rline = (double) i; 
            ind = picscal1(data,rline,rsamp,re,rp,&hscale);
            if (ind == 1) *line = i;
         } 
      } 
   }

   else {                     /* left or bottom margins */
       if (pcs < rnl-pcl) {   /* left margin */
          *samp = 1; 
          if (pcl < 1.0) {        /* top-left corner */
             ibeg = 1; 
             iend = 1; 
          }
          else if (pcl > rnl) {    /* bottom-left corner */
             ibeg = nl; 
             iend = nl; 
          }
          else { 
             ibeg = 1; 
             iend = nl; 
          } 
                    /* search for smallest scale along margin */
          rsamp = 1.0;
          for (i=ibeg; i<=iend; i+=inc) { 
             rline = (double) i; 
             ind = picscal1(data,rline,rsamp,re,rp,&hscale);
             if (ind == 1) *line = i; 
          } 
       }
       else { 
           *line = nl; 
           if (pcs < 1.0) {         /* bottom-left corner */
              ibeg = 1; 
              iend = 1; 
           } 
           else if (pcs > rns) {  /* bottom-right corner */
               ibeg = ns; 
               iend = ns; 
           } 
           else { 
               ibeg = 1; 
               iend = ns; 
           } 

                   /* search for smallest scale along margin */
           rline = rnl;
           for (i=ibeg; i<=iend; i+=inc) { 
              rsamp =(double) i; 
              ind = picscal1(data,rline,rsamp,re,rp,&hscale);
              if(ind == 1) *samp = i;
           } 
        } 
     } 

   if (hscale < 1.0e+20) {
      rline = (double) *line; 
      rsamp = (double) *samp; 
      ind = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
      if (ind != 1) return ind;
      ind = psxy2ll(data,rline+1.,rsamp,&rlat2,&rlon2,&srange); 
      if (ind != 1) return ind;
      picscal2(rlat,rlon,rlat2,rlon2,re,rp,&vscale); 
      use_pc = 0;
   }

COMPUTE_AZIMUTHS:
   if (use_pc==1) {
      *line = pcl; 
      *samp = pcs; 
      hscale = hscale_pc;
      vscale = vscale_pc;
      zvmessage("Picture scale calculated at target center","");
   }
   *(pbuf+0) = (float) hscale;	/* horizontal scale in meters/pixel */
   *(pbuf+1) = (float) vscale;  /* vertical scale in meters/pixel */

	/* Compute north angle */
   na = atan2(om[2][1],om[2][0])*zdpr();
   if (na < 0.) na=na+360.;
   *(pbuf+2) =(float) na;

	/* Compute vector to surface point */
   rline = *line;
   rsamp = *samp;
   if (use_pc==1) {
      rline = pcl; 
      rsamp = pcs; 
   }
   ind = zellipse_inv(om,vsc,fl,oal,oas,scale,ra,rb,rc,rline,rsamp,p);

	/* Compute solar vector */
   sunrange = *(sbuf+24);		/* sun distance to target center */
   sunlat = *(sbuf+27)*zrpd();		/* Subsolar geocentric latitude */
   sunlon = (360.0 - *(sbuf+28))*zrpd(); /* Subsolar east longitude */
   vsun[0] = sunrange*cos(sunlat)*cos(sunlon);
   vsun[1] = sunrange*cos(sunlat)*sin(sunlon);
   vsun[2] = sunrange*sin(sunlat);

   zazimuth(om,p,vsc,vsun,ra,rb,rc,use_pc,&noraz,&scaz,&sunaz);
   *(pbuf+3) = (float) sunaz;
   *(pbuf+4) = (float) scaz;
   *(pbuf+5) = srange;
   *(pbuf+6) =(float) noraz;

	/* Compute lat,lon at surface point */
   x = p[0];
   y = p[1];
   z = p[2];
   d = sqrt(x*x + y*y);
   rlat = atan2(z,d)*zdpr();
   rlon = - atan2(y,x)*zdpr();
   if (rlon < 0.) rlon=rlon+360.;
   *(pbuf+7) = (float) rlat;
   *(pbuf+8) = (float) rlon;
   return ind;

ERROR1:
   zvmessage("***Error calling mpGetValues","PICSCALE");
   return ind;   

ERROR2:
   zvmessage("***Error calling mpxy2ll","PICSCALE");
   return ind;
}

/******************************************************************************/
/*  Returns ind=1 if scale is replaced by a smaller value.		      */
/******************************************************************************/
int picscal1(data,rline,rsamp,re,rp,scale)
float   *data;
double  rline,rsamp,re,rp;
double  *scale;
{
   int ind;
   double tscale,rlat,rlon,rlat2,rlon2;
   float srange;

   ind = psxy2ll(data,rline,rsamp,&rlat,&rlon,&srange);
   if (ind != 1) return(0);
   ind = psxy2ll(data,rline,rsamp+1.,&rlat2,&rlon2,&srange);
   if (ind != 1) return(0);
   picscal2(rlat,rlon,rlat2,rlon2,re,rp,&tscale);
   if (tscale < *scale) {
      *scale = tscale;
      return(1);
   }
   return(0);
}

/****************************************************************************** 
    Compute the picture scale
*******************************************************************************/
void picscal2(rlat,rlon,rlat2,rlon2,re,rp,scale) 
double rlat,rlon,rlat2,rlon2,re,rp,*scale;
{
   double  alat,dlat,dlon,gcr,dm,dz,d;

   alat = zrpd()*(rlat+rlat2)/2.0;   /* average latitude */
   dlat = zrpd()*fabs(rlat2-rlat);   /* delta latitude */
   dlon = zrpd()*fabs(rlon2-rlon);   /* delta longitude */
	   /* Compute geocentric radius */
   gcr = re*rp/sqrt(rp*cos(alat)*rp*cos(alat) + re*sin(alat)*re*sin(alat));

   dm  = gcr*dlat;                 /* meridional displacement(km) */
   dz  = gcr*cos(alat)*dlon;       /* zonal displacement(km) */
   d   = sqrt(dm*dm + dz*dz);      /* resultant displacement(km) */

   *scale = 1000.0 * d;           /* output scale in meters/pixel */
   if (*scale < 1.0) *scale = 0.0;
} 

/****************************************************************************
  This routine needs to be corrected PORT/PICSCALE/OLD for triaxial 
  ellipsoid Compute the picture scale.
*****************************************************************************/
void picscal3(rlat,rlon,rlat2,rlon2,ra,rb,rc,scale)
double rlat,rlon,rlat2,rlon2,ra,rb,rc,*scale;
{
  double d1,d2,d3,x,y,z,r1,r2,x2,y2,z2,d;

  rlon = zrpd()*rlon;
  rlon2 = zrpd()*rlon2;
  rlat = zrpd()*rlat;
  rlat2 = zrpd()*rlat2;
  d1 =(rb*rc*cos(rlat)*cos(rlon))*(rb*rc*cos(rlat)*cos(rlon));
  d2 =(ra*rc*cos(rlat)*sin(rlon))*(ra*rc*cos(rlat)*sin(rlon));
  d3 =(ra*rb*sin(rlat))*(ra*rb*sin(rlat));
  r1 =(ra*rb*rc)/sqrt(d1+d2+d3);
   x = r1*cos(rlat)*cos(rlon);
   y = r1*cos(rlat)*sin(rlon);
   z = r1*sin(rlat);
  d1 =(rb*rc*cos(rlat2)*cos(rlon2))*(rb*rc*cos(rlat2)*cos(rlon2));
  d2 =(ra*rc*cos(rlat2)*sin(rlon2))*(ra*rc*cos(rlat2)*sin(rlon2));
  d3 =(ra*rb*sin(rlat2))*(ra*rb*sin(rlat2));
  r2 =(ra*rb*rc) / sqrt(d1 + d2 + d3);
  x2 = r2*cos(rlat2)*cos(rlon2);
  y2 = r2*cos(rlat2)*sin(rlon2);
  z2 = r2*sin(rlat2);
   d = sqrt(((x2-x)*(x2-x)) + ((y2-y)*(y2-y)) + ((z2-z)*(z2-z)));
  *scale = 1000.0 * d;
  if (*scale < 1.0) *scale = 0.0;
}

/******************************************************************************/
/*  This routine multiplies a 3x3 double precision matrix with a 3-element    */
/*  double precision vector and returns a 3-element double precision vector.  */
/******************************************************************************/
/*** txh::Subroutine not being called by any programs and generates compile
 ***      warnings.
int *matrixmult(OMmatrix,vin,vout)
double *OMmatrix,*vin,*vout;
{
   int i,j,k,l;

   for (i=0; i<3; i++) {
      j = i + 0;
      k = i + 3;
      l = i + 6;
      *(vout+i) =(*(OMmatrix+j) * *(vin+0))    
		+(*(OMmatrix+k) * *(vin+1))    
		+(*(OMmatrix+l) * *(vin+2));
   }
}
***/
/******************************************************************************/
/*  This function returns the number of degrees per radian in double precision*/
/*  format by calculating 180/pi.  The value of pi is determined by the acos()*/
/*  function--that is:  zdpr = 180.0 / acos(-1.0).  The first time this func-  */
/*  tion is called,it calculates the value to return.  It saves this value   */
/*  and does not re-calculate it for subsequent calls.                        */
/******************************************************************************/
/* Convert from line,sample to lat,lon */
int psxy2ll(data,rline,rsamp,rlat,rlon,srange)
float  *data,*srange;
double rline,rsamp,*rlat,*rlon;
{
   float zline,zsamp,zlat,zlon,zradius;
   int   ind;

   zline = (float) rline;
   zsamp = (float) rsamp;
   zpproj(data,&zline,&zsamp,&zlat,&zlon,2,0,&zradius,srange,&ind);
   if (ind == 1) {
      *rlat = (double) zlat;
      *rlon = (double) zlon;
   }
   return(ind);
}

/******************************************************************************/
/* Convert from lat,lon to line sample */
int psll2xy(data,rlat,rlon,rline,rsamp)
float  *data;
double rlat,rlon,*rline,*rsamp;
{
   float zline,zsamp,zlat,zlon,zradius,zrange;
   int   ind;

   zlat = (float) rlat;
   zlon = (float) rlon;
   zpproj(data,&zline,&zsamp,&zlat,&zlon,1,0,&zradius,&zrange,&ind);
   if (ind == 1) {
      *rline = (double) zline;
      *rsamp = (double) zsamp;
   }
   else {
      *rline = -999.0;
      *rsamp = -999.0;
   }
   return(ind);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create lighting.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Given (lat,lon) of a point on target and GETSPICE95 navigation data,
C caluclate phase, incidence, and emission angles.
C
      SUBROUTINE LIGHTING(BUF,LAT,LON,phase,incidence,emission)
      IMPLICIT NONE
      REAL*8 BUF(100)		!GETSPICE95 buffer (input)
      REAL*8 LAT,LON		!Point where angles are calculated
      REAL*8 PHASE,INCIDENCE,EMISSION	!Output lighting angles

      REAL*8 RA,RC		!Target radii (km)

      REAL*8 SUNRANGE		!Distance from Sun to target center (km)
      REAL*8 SUNLAT,SUNLON	!Sun lat,lon (radians)

      REAL*8 SCRANGE		!Distance from S/C to target center (km)
      REAL*8 SCLAT,SCLON	!Spacecraft lat,lon (radians)

      REAL*8 VSC(3),VSUN(3),V(3),N(3),S(3),C(3),MAGNITUDE,GCR,D
      REAL*8 RLAT,RLON

      REAL*8 PI,RTD,DTR

      PI = 3.141592653589D0
      RTD = 180.D0/PI
      DTR = PI/180.D0

      RA = BUF(13)
      RC = BUF(15)
      SUNRANGE = BUF(25)
      SUNLAT = BUF(28)*DTR
      SUNLON = (360.D0 - BUF(29))*DTR!Convert to East longitude
      CALL LATREC(SUNRANGE,SUNLON,SUNLAT,vsun)!Vector from center to Sun
C     ....Spacecraft position vector
      SCRANGE = BUF(27)
      SCLAT = BUF(30)*DTR
      SCLON = (360.D0 - BUF(31))*DTR!Convert to East longitude
C     ....Vector from center to S/C
      CALL LATREC(SCRANGE,SCLON,SCLAT,vsc)
C     ....Compute geocentric radius
      RLAT = LAT*DTR
      RLON = (360.D0 - LON)*DTR
      GCR = RA*RC/DSQRT((RC*DCOS(RLAT))**2 + (RA*DSIN(RLAT))**2)
C     ....Vector from center of target to surface pt
      CALL LATREC(GCR,RLON,RLAT,v)
C     ....Compute unit normal N at surface point
      N(1) = V(1)*(RC/RA)**2
      N(2) = V(2)*(RC/RA)**2
      N(3) = V(3)
      CALL UNORM(N,n,magnitude)      
C     ....Compute unit vector from surface point to Sun
      S(1) = VSUN(1) - V(1)
      S(2) = VSUN(2) - V(2)
      S(3) = VSUN(3) - V(3)
      CALL UNORM(S,s,magnitude)
C     ....Compute unit vector from surface point to spacecraft
      C(1) = VSC(1) - V(1)
      C(2) = VSC(2) - V(2)
      C(3) = VSC(3) - V(3)
      CALL UNORM(C,c,magnitude)
C     ....phase angle = ARCCOS(S o C)
      D = S(1)*C(1) + S(2)*C(2) + S(3)*C(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      PHASE = DACOS(D)*RTD
C     ....incidence angle = ARCCOS(N o S)
      D = N(1)*S(1)+N(2)*S(2)+N(3)*S(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      INCIDENCE = DACOS(D)*RTD
C     ....emission angle = ARCCOS(N o C)
      D = N(1)*C(1)+N(2)*C(2)+N(3)*C(3)
      IF (D.GT.1.D0) D=1.D0
      IF (D.LT.-1.D0) D=-1.D0
      EMISSION = DACOS(D)*RTD
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zlighting.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/*  C-Callable Version zlightning                                       */
/************************************************************************/
#include  "xvmaininc.h"  
#include  "ftnbridge.h"

void  zlighting (buf,lat,lon,phase,incidence,emission)  
double buf[];         /* spice buffer  */
double lat;           /* latitude  */
double lon;           /* longitude */
double *phase;        /* returned angle */
double *incidence;    /* returned angle */
double *emission;     /* returned angle */ 
{
FTN_NAME2(lighting, LIGHTING) (buf,&lat,&lon,phase,incidence,emission) ;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create spice2convev.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Copy SPICE data into CONVEV data buffer.
C
      SUBROUTINE SPICE2CONVEV(RBUF,FL,OAL,OAS,SCALE,MPTYPE,data)
      REAL*8 RBUF(100)
      REAL*4 DATA(40)
      REAL*4 FL,OAL,OAS,SCALE
      INTEGER*4 MPTYPE
      CALL MVE(1,72,RBUF(59),data,1,1)!OM-matrix
      CALL MVE(1,27,RBUF(22),data(19),1,1)!RS-vector
      DATA(25) = RBUF(15)!Polar radius
      DATA(26) = RBUF(13)!Equatorial radius
      DATA(27) = FL
      DATA(28) = OAL
      DATA(29) = OAS
      DATA(30) = SCALE
      DATA(38) = RBUF(27)!Target range
      CALL MVE(4,1,MPTYPE,data(39),1,1)!Projection type
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zspice2convev.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include  "xvmaininc.h"  
#include  "ftnbridge.h"
/************************************************************************/
/*  C-Callable Version SPICE2CONVEV                                     */
/************************************************************************/

void  zspice2convev(rbuf,fl,oal,oas,scale,mptype,data)
double  rbuf[];       /* spice buffer */  
double  fl;           /* focal length in mm. */
double  oal;          /* optical axis line object space  */
double  oas;          /* optical axis sample object space */
double  scale;        /* object space scale */
int     mptype;       /* projection type */
float   data[];       /* returned buffer */
/* Parameters fl,oal,oas and scale are declared as double to            */
/* receive the correct values due to the C parameter promotion feature. */
{float focal_length,opt_axis_line,opt_axis_samp,obj_space_scal;
       focal_length  = (float)fl;
       opt_axis_line = (float)oal;
       opt_axis_samp = (float)oas;
       obj_space_scal= (float)scale;
FTN_NAME(spice2convev) (rbuf,&focal_length,&opt_axis_line,&opt_axis_samp,
         &obj_space_scal,&mptype,data);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rpicscale.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdlib.h>
#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "mp_routines.h" 

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

double zrpd(void);
double zdpr(void);

/*****************************************************************************
 ***   FORTRAN BRIDGE: "rpicscale"                                             
 *****************************************************************************/
void FTN_NAME2 (rpicscale,RPICSCALE) (sbuf,data,mp,mptype,sclat,sclon,r,
						nl,ns,pbuf,line,samp)
double *sbuf;
float  *data;
MP      mp; 
double *sclat,*sclon;
double *r;
int    *mptype,*nl,*ns;
float  *pbuf;
double *line,*samp;
{
    zrpicscale(sbuf,data,mp,*mptype,*sclat,*sclon,*r,*nl,*ns,pbuf,line,samp);
}
/******************************************************************************/
/***   "rzpicscale"  subroutine                                                */
/******************************************************************************/
int zrpicscale(sbuf,data,mp,mptype,sclat,sclon,r,nl,ns,pbuf,line,samp)
double *sbuf;		/* input SPICE buffer */
float  *data;           /* input 40-word geometry buffer for PPROJ call */
MP      mp;		/* Pointer to map projection data */
int     mptype;         /* Map projection type */
double  sclat,sclon;	/* spacecraft latitude/longitude */
double  r;              /* ring radius at which scale is calculated */
int     nl,ns;		/* size of input image */
float  *pbuf;		/* output buffer */
double *line,*samp;	/* output coordinates at which scale applies */
{

   float  fl,oal,oas,scale;
   double srange;
   double scrange,sunrange;	/* spacecraft and sun range from targ center */
   double vsc[3],vsun[3];	/* spacecraft and solar vectors */
   double om[3][3];		/* tranformation from target to camera coords */
   double rlon;			/* ring longitude */
   double p[3],na,noraz,sunaz,scaz,sunlat,sunlon;
   double rscale,ascale;  /* radial and azimuthal ring scale in km/pixel */
   int    i,ind;
	   /*  INITIALIZE VARIABLES */
   for (i=0; i<30; i++) *(pbuf+i) = -999.0; 
   rscale = 1.0e+20;		/* initialize as invalid scale */
   ascale = 1.0e+20;
   zmve(8,9,(sbuf+58),om,1,1);
   zmve(8,3,(sbuf+21),vsc,1,1);
   fl = data[26];
   oal = data[27];
   oas = data[28];
   scale = data[29];

        /* Compute north angle */
   na = atan2(om[2][1],om[2][0])*zdpr();
   if (na < 0.) na=na+360.;
   *(pbuf+2) =(float) na;

   ind = zring_scale(om,vsc,&fl,&oal,&oas,&scale,nl,ns,r,
		&rlon,&srange,line,samp,&rscale,&ascale);
   if (ind == 0) return ind;
   *(pbuf+0) = 1000.*rscale;
   *(pbuf+1) = 1000.*ascale;
   
   p[0] = r*cos(rlon*zrpd());
   p[1] = -r*sin(rlon*zrpd());
   p[2] = 0.0;
	/* Compute solar vector */
   sunrange = *(sbuf+24);		/* sun distance to target center */
   sunlat = *(sbuf+27)*zrpd();		/* Subsolar geocentric latitude */
   sunlon = (360.0 - *(sbuf+28))*zrpd(); /* Subsolar east longitude */
   vsun[0] = sunrange*cos(sunlat)*cos(sunlon);
   vsun[1] = sunrange*cos(sunlat)*sin(sunlon);
   vsun[2] = sunrange*sin(sunlat);

   ind = zrazimuth(om,p,vsc,vsun,&noraz,&scaz,&sunaz);

   *(pbuf+3) = (float) sunaz;
   *(pbuf+4) = (float) scaz;
   *(pbuf+5) = (float) srange;
   *(pbuf+6) =(float) noraz;
   *(pbuf+7) = (float) r;		/* radius-longitude coordinates */
   *(pbuf+8) = (float) rlon;		/* where calculations are done */
   return ind;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create picscale.imake
#define SUBROUTINE picscale

#define MODULE_LIST picscale.c rpicscale.c lighting.f zlighting.c spice2convev.f zspice2convev.c 

#define USES_ANSI_C
#define MAIN_LANGUAGE_C
#define USES_FORTRAN
#define FTN_STRING 
#define P2_SUBLIB 		/* Enable during delivery */
$ Return
$!#############################################################################
$Test_File:
$ create tzpicscale.c
/* TZPICSCALE -- Test program for subroutine PICSCALE */
#include <stdio.h>
#include <string.h>
#include "vicmain_c"
#include "ftnbridge.h"
#include "mp_routines.h"

#define THIS_ROUTINE "tzpicscale"
#define DBUG 0
#define LOCAL  0
#define REMOTE 1
static  char project [6]  = "     ";
static  char source  [5]  = "    ";
static  char planet  [13] = "            ";
void t_mpGetValues();

void main44()
{
    float  hscale,vscale;           /* scale in meters/pixel */
    float  noraz,sunaz,scaz;	    /* north, solar, and s/c azimuth angles */
    double line,samp;               /* Coordinates at which scale applies */
    int    nl,ns;
    int    input,frame_id,mptype,status;
    double sbuf[200]; 	         /* buffer returned by GETSPICE */
    float  pbuf[30];	         /* buffer returned by PICSCALE */
    int camera;
    float  focal_length,oal,oas,pxlscale;
    double sclat,sclon;		/* spacecraft lat-lon */
    float srange;        	/* slant range */
    char msg[132];

    int index;

/*   VARIABLES USED FOR CALLING NEW MP_ROUTINES */
    MP     mp;
    char   MapType[mpMAX_KEYWD_LENGTH];
    char   keywords[mpNUMBER_OF_KEYWORDS][mpMAX_KEYWD_LENGTH+1] ;
    float  data  [40];  
    int    type  [mpNUMBER_OF_KEYWORDS];
    int    class [mpNUMBER_OF_KEYWORDS];
    int    number_of_keywords;

    zifmessage ("TZPICSCALE version March 21, 1998"); 

    memset (project,'\0',sizeof(project));
    memset (source, '\0',sizeof(source));
    memset (planet, '\0',sizeof(planet));

    for (index=0; index<200; index++)
       sbuf[index] = 0.0;
    for (index=0; index<30; index++)
       pbuf[index] = 0.0;
    for (index=0; index<40; index++)
       data[index] = 0.0;

    status = zvunit(&input,"INP",1, 0);
    status = zvopen(input, "OPEN_ACT","SA","IO_ACT","SA", 0);
    status = zvget(input,"NL",&nl,"NS",&ns, 0); 

    zgetproj(input,project,&camera,&frame_id,&status); 
    zgetcamcon(project,camera,&focal_length,&oal,&oas,&pxlscale,&status);
    status = zgetspice2(input,1,sbuf);

    status = mpInit(&mp);
    if (status == -1) {
       sprintf(msg,"***mpInit error status: %d.\n",status);
       zvmessage(msg,"");
       zabend();
    };   
    status = mpLabelRead(mp,input );
    if (status != mpSUCCESS) {
       mptype = 7;           /* if not map projected, assume image-space */
       mp = NULL;
       zmve (8,9,&sbuf[58],&data[0],1,1); /* OM Matrix */
       zmve (8,3,&sbuf[21],&data[18],1,1);/* RS Vector */
       data[24] = (float)sbuf[14];     /* polar radius */
       data[25] = (float)sbuf[12];     /* equitorial radius */
       data[26] = focal_length;        /* camera focal length */
       data[27] = oal;                 /* optical axis line */
       data[28] = oas;                 /* optical axis sample */
       data[29] = pxlscale;            /* picture scale in pixels/mm */
       data[37] = (float)sbuf[26];     /* space craft range */
       sclat    = sbuf[29];  	       /* space craft latitude */
       sclon    = sbuf[30];	       /* space craft longitude */		
    } else {
       mptype = -7;
       status = mpGetValues(mp,mpMAP_PROJECTION_TYPE,MapType,0);
       if (status != mpSUCCESS) {
	 sprintf (msg,
         "***mp error, unrecognized projection, status: %d.",status);
 	 zvmessage(msg, "");
	 mpFree( mp );
	 zabend();
       }
       status=mpGetKeywords(mp,keywords,&number_of_keywords,&type[0],&class[0]);
       if (status != mpSUCCESS) {
	 sprintf (msg,"***mpGetKeywords error, status: %d.",status);
 	 zvmessage(msg, "");
	 mpFree( mp );
	 zabend();
       }
    }

    zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
    hscale = pbuf[0];
    vscale = pbuf[1];
    sunaz  = pbuf[3];
    scaz   = pbuf[4];
    srange = pbuf[5];
    noraz  = pbuf[6];

    sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
    zvmessage(msg,"");
    sprintf(msg,"noraz=%e  sunaz=%e  scaz=%e  ",noraz,sunaz,scaz);
    zvmessage(msg,"");
    sprintf(msg,"slant range=%e",srange);
    zvmessage(msg,"");
	   /* If map projection type other than 7, 8, 16 terminate test */
   if (mptype != 7 && mptype != 8 && mptype != 16) return;

/*  Move optical-axis around to test other parts of the code */
/*  then Call zpicscale again */

      oas = -300;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];

      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

      oal = 800.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

      oal = 0.0;
      oas = 400.0;
      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

      oal = 1100.0;
      oas =  400.0;
  
      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************* */

      oal =  100.0;
      oas = -300.0;

      data[27] = oal;
      data[28] = oas;

      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************* */

      oal = 100.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;
  
      zpicscale (&sbuf[0],&data[0],mp,mptype,sclat,sclon,nl,ns,
	&pbuf[0],&line,&samp);
      hscale = pbuf[0];
      vscale = pbuf[1];
      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

/* ************************************************************************ */

/*    Quick Test  of Fortran to C bridge */
/*    zpicscale (sbuf,data,mp,mptype,sclat,sclon,nl,ns,pbuf,&line,&samp); */

      oal = 100.0;
      oas = 800.0;

      data[27] = oal;
      data[28] = oas;

      FTN_NAME(txpicscale) (&sbuf[0], &data[0], &mp, &mptype, &sclat, &sclon, 
                            &nl, &ns, &pbuf[0], &line, &samp);
      hscale = pbuf[0];
      vscale = pbuf[1];

      sprintf(msg,"hscale=%e  vscale=%e  line=%f  samp=%f",
	hscale,vscale,line,samp);
      zvmessage(msg, "");

      status = mpFree( mp );
}
/************************************************************************************/
/***  GET (INT) VALUE FROM MP OBJECT                                 **/
/************************************************************************************/
void t_mpGetValues(mp,  keyword,  value)
MP mp;
char *keyword;
int  *value;
{
int status, results;
static char msg[132];

      status = mpGetValues( mp,  keyword,  &results, "" );

      *value = results;
      if (status != mpSUCCESS) { 
	sprintf
          (msg, "Call to mp_routines mpGetValues returned error status: %d.\n",
           status);
	zvmessage(msg, "");
	mpFree( mp );
	zabend();
      }
}
$!-----------------------------------------------------------------------------
$ create txpicscale.f
C
      SUBROUTINE txpicscale( sbuf, data, mp, mptype, sclat, sclon, 
     &               nl, ns, pbuf, line, samp)
      REAL*8    sbuf(200)         !input SPICE buffer
      REAL*4    data(40)          !input projection information
      INTEGER*4 mp                !input projection information
      INTEGER*4 mptype            !input projection information
      REAL*8    sclat
      REAL*8    sclon
      INTEGER*4 nl
      INTEGER*4 ns                !size of input image
      REAL*4    pbuf(30)          !output buffer
      REAL*8    line, samp        !coordinates at which scale applies

      REAL*8    isbuf(200)        !input SPICE buffer
      REAL*4    idata(40)         !input projection information
      INTEGER*4 imp               !input projection information
      INTEGER*4 imptype           !input projection information
      REAL*8    isclat
      REAL*8    isclon
      INTEGER*4 inl
      INTEGER*4 ins               !size of input image
      REAL*4    ipbuf(30)         !output buffer
      INTEGER   I

C    CALL PICSCALE 
      ! Move data to loal declarations
      DO I = 1,200
        isbuf(I) = sbuf(I)         !input SPICE buffer
      END DO

      DO I = 1,40
        idata(I) = data(I)         !input projection information
      END DO

      isclat = sclat
      isclon = sclon
      inl = nl 
      ins = ns                     !size of input image
      
      DO I = 1,30
        ipbuf(I) = pbuf(I)         !input projection information
      END DO

      imptype = mptype             !input projection information
      imp = mp                     !input projection information
      CALL picscale(isbuf,idata,imp,imptype,isclat,isclon,
     &              inl,ins,ipbuf,line,samp) 

C   RETURN
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzpicscale.imake
/* Imake file for Test of VICAR subroutine picscale */

#define PROGRAM tzpicscale

#define MODULE_LIST tzpicscale.c  txpicscale.f

#define MAIN_LANG_FORTRAN
#define TEST
#define USES_ANSI_C
#define USES_FORTRAN
#define FTN_STRING

#define LIB_RTL 
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
$!-----------------------------------------------------------------------------
$ create tzpicscale.pdf
PROCESS HELP=*
PARM INP  TYPE=STRING  COUNT=1 	                       DEFAULT="MIPL:[MIPL.GLL]JUPPROJ1.IMG"
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
END-PROC
$!-----------------------------------------------------------------------------
$ create tstpicscale.pdf
procedure
refgbl $echo
refgbl $autousage
refgbl $syschar
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
local path  type=string init="/project/test_work/testdata/mipl/gll/"
local PATH1 type=string init="/project/gll/ssi/udr/"
local usr   type=string init="mipsdev"
local db    type=string init="devCat"
local pw    type=string init="1$beale"
local srv   type=string init="miplTest"
local pw1   type=string init="1\$beale"

if ($syschar(1) <> "UNIX")
   let path="WMS_TEST_WORK:[TESTDATA.MIPL.GLL]"
   let PATH1="WMS_GLL:[SSI.UDR]"
   let PW1="1$beale"
end-if
write "PICSCALE TEST PDF VERSION 04-05-98"
run_isql user=&USR pass=&PW1 sy_filename=picscale.data
! Test of subroutine PICSCALE
fit &"path"venus2.img venus.byt perc=0.1 'byte
!	Test of image-space label
tzpicscale venus.byt
!	Test of PERSLAB label
perslab venus.byt venus.lab
tzpicscale venus.lab
!	Test of SIZE label
size venus.lab venus.siz zoom=-2
tzpicscale venus.siz
!	Test of map projection label
map3 venus.byt venus.map plat=-2.97 plon=159. line=400. samp=400. +
 scale=15. 'stereo 'remote
tzpicscale venus.map
! Test of LIGHTING and SPICE2CONVEV subroutines.
!gspice &"PATH1"s0350029745.mos1 'remote
gspice venus.byt 'remote
! Test ZLIGHTING and ZSPICE2CONVEV (C-bridges)
!tzlighting &"PATH1"s0350029745.mos1 
tzlighting venus.byt
Write "Testing of very very small image."
catlabel &"path"s0394449168.m2 target="ADRASTEA" +
out="temp.img" catu=&usr catpw=&pw catsrv=&srv catdb=&db

if ($syschar(1) = "VAX_VMS")
   dcl delete venus.byt;*
   dcl delete venus.lab;*
   dcl delete venus.siz;*
   dcl delete venus.map;*
else
   ush rm venus.byt
   ush rm venus.lab
   ush rm venus.siz
   ush rm venus.map
end-if
end-proc
$!-----------------------------------------------------------------------------
$ create tzlighting.c
#include <math.h>
#include <stdio.h>
#include "vicmain_c"
#include "mp_routines.h"
#ifndef  TRUE
#define TRUE  1
#define FALSE 0
#endif
void main44(){
double	spicebuf[100];		
MP      mpo;
int	i,j,camera,fsd,ptype,inunit,
	nl,ns,istat,mode,ilat;	
float	data[40],pbuf[30],focal_length,	
	oal,oas,pxlscale,radius,srange,
        rline,rsamp,rlat,rlon;         	
double  dlat,dlon,sclat,sclon,line,samp;
char    msg[132],project[6];

   istat=zvunit(&inunit,"INP",1,0);   
   istat=zvopen(inunit,"OP","READ","OPEN_ACT","SA","IO_ACT","SA",0);
   zgetproj(inunit,project,&camera,&fsd,&istat);
   if(istat != 0) zprnt(4,1,&istat,"zgetproj fatal indicator=.");
   if(!zgetspice2(inunit,1, spicebuf)) exit(1);
   istat=zvget(inunit,"NL",&nl,"NS",&ns,0);	
   /* Get Camera constants			*/
   zgetcamcon(project,camera,&focal_length,&oal,&oas,&pxlscale,&istat);
   /* set up call to PICSCALE */
   ptype=8;
   zspice2convev(spicebuf,focal_length,oal,oas,pxlscale,ptype,data);
   sclat=spicebuf[29];
   sclon=spicebuf[30];
   zvmessage("Items calculated by PICSCALE using zspice2convev buffer"," ");
   zpicscale(spicebuf,data,mpo,ptype,sclat,sclon,nl,ns,pbuf,&line,&samp);
   sprintf(msg,"line=%f samp=%f",line,samp);
   zvmessage(msg," ");
   sprintf(msg,"HSCL = %-15.5f   VSCL   = %-15.5f",pbuf[0],pbuf[1]);  
   zvmessage(msg," ");
   sprintf(msg,"NORAZ= %-15.5f   SUNAZ  = %-15.5f",pbuf[2],pbuf[3]);	
   zvmessage(msg," ");       
   sprintf(msg,"SCAZ = %-15.5f   SLRANGE= %-15.5f",pbuf[4],pbuf[5]);	
   zvmessage(msg," ");  
   mode = 2;  /* 2=convert from line-samp to lat-lon*/
   ilat = 0;  /* 0=planetocentric latitudes */
   rline = line;
   rsamp = samp;
   zpproj(data,&rline,&rsamp,&rlat,&rlon,mode,ilat,&radius,&srange,&istat);
   dlat = (double) rlat;
   dlon = (double) rlon;
   zvmessage("Items calculated by LIGHTING ********************"," ");
   zlighting(spicebuf,dlat,dlon,&spicebuf[80],&spicebuf[78],&spicebuf[79]);
   sprintf(msg,"INA = %-15.5f   EMA = %-15.5f",spicebuf[78],spicebuf[79]);
   zvmessage(msg," ");
   sprintf(msg,"PHA = %-15.5f",spicebuf[80]);	
   zvmessage(msg," ");
   istat=zvclose(inunit,0);	     /* Close input file		*/
}
$!-----------------------------------------------------------------------------
$ create tzlighting.pdf
PROCESS  HELP=*
PARM INP  TYPE=STRING  COUNT=1 	  
PARM TARGET     TYPE=(STRING,12) COUNT=0:1			DEFAULT=--
PARM SPICEMODE  TYPE=KEYWORD     COUNT=0:1 VALID=(LOCAL,REMOTE) DEFAULT=--
PARM CKNAME     TYPE=(STRING,4)  COUNT=1			DEFAULT=DAVI
PARM CKID       TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM USERID     TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM GROUPID    TYPE=(STRING,3)  COUNT=0:1			DEFAULT=--
PARM INSTITUTE  TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM CDATE      TYPE=(STRING,12) COUNT=1		DEFAULT=000000000000
PARM REQNUM     TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PURPOSE    TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
PARM PROGRAM    TYPE=(STRING,6)  COUNT=1			DEFAULT=*NONE*
PARM SPKID      TYPE=(STRING,4)  COUNT=1			DEFAULT=NONE
END-PROC
$!-----------------------------------------------------------------------------
$ create tzlighting.imake
/* Imake file for Test of VICAR subroutines lighting and 
   spice2conv */

#define PROGRAM tzlighting

#define MODULE_LIST tzlighting.c

#define MAIN_LANG_FORTRAN
#define TEST
#define USES_ANSI_C
#define USES_FORTRAN
#define FTN_STRING

#define LIB_RTL 
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB
#define LIB_SPICE
#define LIB_NETWORK
$!-----------------------------------------------------------------------------
$ create picscale.data
delete from ssiraw where sclkstrtcnt=394449168
go
delete from ssioverview where sclkstrtcnt=394449168
go
insert into ssiraw(ert,tml,tpl,gaps,note,maxcml,rawnum,ccdtemp,coneang,fileIdx,
maxssnr,minssnr,ertmilli,exposdur,filename,filepath,filtname,framedur,mergenum,
modified,piccount,averagedn,expostype,telemmode,dsnstation,filestatus,
gainmodeid,obstructid,telemfmtid,bestversion,maxhufratio,maxictratio,
minhufratio,minictratio,producttype,rearopttemp,sclkstrtcnt,blemprotflag,
encodingtype,meanhufratio,meanictratio,stopgapline1,stopgapline2,stopgapline3,
invclckstflag,litefldstflag,meantruncbits,meantruncsamp,rotorposition,
sclkpartition,startgapline1,startgapline2,
startgapline3)
  values("May 21 1997  8:46:00:000PM",8,NULL,1,
"GLLTELEMPROC v2.0.0",8,1,119,94.1158599853516,25618055,0,0,393,
395.833312988281,"s0394449168.1","/project/gll/ssi/udr/g08/","CLEAR",4,NULL,
"May 21 1997  3:05:26:103PM",111,36,"EXT","PB","43",1,"40K","N","HIS",0,0,
46.718807220459,0,5.64282512664795,NULL,51,394449168,"OFF","ICT",0,
23.6806907653809,120,0,0,"NOR","ON",0,0,282.337799072266,1,113,0,0)
go
insert into ssiraw(ert,tml,tpl,gaps,note,maxcml,rawnum,ccdtemp,coneang,fileIdx,
maxssnr,minssnr,ertmilli,exposdur,filename,filepath,filtname,framedur,mergenum,
modified,piccount,averagedn,expostype,telemmode,dsnstation,filestatus,
gainmodeid,obstructid,telemfmtid,bestversion,maxhufratio,maxictratio,
minhufratio,minictratio,producttype,rearopttemp,sclkstrtcnt,blemprotflag,
encodingtype,meanhufratio,meanictratio,stopgapline1,stopgapline2,stopgapline3,
invclckstflag,litefldstflag,meantruncbits,meantruncsamp,rotorposition,
sclkpartition,startgapline1,startgapline2,
startgapline3)
  values("May 21 1997  8:46:00:000PM",8,NULL,1,"SSIMERGE2",8,2,
119,94.1158599853516,26732679,NULL,NULL,393,395.833312988281,"s0394449168.m2",
"/project/gll/ssi/udr/merge/g08/","CLEAR",4,1,"Apr  3 1998 11:44:01:086AM",111,
36,"EXT","PB","43",1,"40K","N","HIS",1,0,46.718807220459,0,5.64282512664795,"M",
51,394449168,"OFF","ICT",0,23.6806907653809,120,0,0,"NOR","ON",0,0,
282.337799072266,1,113,0,0)
go
insert into ssioverview(ra,note,scaz,smaz,noraz,obsid,sunaz,twist,maxlat,maxlon,
minlat,minlon,phsang,edrdone,imageid,soldist,sysdone,cspiceid,emissang,espiceid,
incidang,ispiceid,modified,pspiceid,redrdone,smearmag,sspiceid,subsclat,
subsclon,targname,centerlat,centerlon,edrcddone,ictseqnum,imagetime,mergedone,
opnavflag,slantdist,subscline,subsollat,subsollon,ctrbdydist,galsosdone,
orbnumchar,redrcddone,resolution,ringradius,subscrange,timeclsapr,angsemidiam,
declination,edrfilldone,readoutmode,sclkstopcnt,sclkstrtcnt,subscsample,
targctrdist,edrspikedone,localhourang,onchipmosaic,predexposdur,predfiltname,
predframedur,redrblemdone,redrfilldone,redrspikedone,sattimeclsapr,
sclkpartition,imagetimemilli,predgainmodeid,predtelemfmtid,
prtsclkstrtcnt)
  values(221.753799438477,NULL,175.347702026367,NULL,
88.6500778198242,"G8SSADRAST01",179.054748535156,73.2079925537109,NULL,NULL,
NULL,NULL,80.5533981323242,0,"G8S0001",761976000,0,"N094",0.084196001291275,
"E032",80.5528717041016,NULL,"Apr 28 1998 11:26:50:443AM","N126",0,
0.100000001490116,"N126",-0.586521983146667,136.935455322266,"SMALL_SAT",
-0.586521983146667,136.935455322266,0,0,"May  7 1997  1:16:13:000AM",1,0,
1435774.125,-54.4647560119629,-0.458534985780716,56.3834075927734,1507172.75,0,
"G8",0,29.0562782287598,NULL,1435774.25,-1102550,2.71862888336182,
-17.4079170227051,0,NULL,394449190,394449168,3326.98779296875,1507265.625,0,
99.4479522705078,0,395.833312988281,"CLEAR",4,0,0,0,-1102550,1,424,"40K","HIS",0)
go
$ Return
$!#############################################################################
$Other_File:
$ create picscale.hlp
1 PICSCALE

   Compute picture scale, north angle, azimuth angles, and slant range for
   a selected point in an image.

   FORTRAN Calling Sequence:

         PICSCALE(SBUF,DATA,MP,MPTYPE,SCLAT,SCLON,NL,NS,PBUF,LINE,SAMP)

   where:

       	 REAL*8    SBUF(200)    Buffer returned by subroutine GETSPICE
         REAL*4    DATA(40)     Array containing image geometry data (see below)
         INTEGER   MP           Address of Structure created by mpInit(MP)
         INTEGER   MPTYPE       map projection type
       	 REAL*8    SCLAT        Subspacecraft planetocentric latitude (degrees)
       	 REAL*8    SCLON        Subspacecraft planetocentric west-longitude deg
         INTEGER*4 NL           Number of lines in image
         INTEGER*4 NS           Number of samples in image
       	 REAL*4 PBUF(30)        Output array of computed terms (see below)
         REAL*8    LINE,SAMP    (Output) Coordinates where computation is done

  C Calling Sequence:

        #include "mp_routines.h"
        zpicscale(sbuf,data,mp,mptype,sclat,sclon,nl,ns,pbuf,&line,&sample);
  where:
        double *SBUF;           Buffer returned by subroutine GETSPICE
        float  *data;           Image geometry data (see below)
       	MP     *mp              Address of Structure created by mpInit(MP)
        int     mptype          map projection type
      	double  sclat;          Subspacecraft planetocentric latitude in degrees
      	double  sclon;          Subspacecraft planetocentric west-longitude in degrees
       	int     nl;             Number of lines in image
        int     ns;             Number of samples in image
        float  *pbuf;           Output array of computed terms (see below)
        double *line,*samp;     (Output) Coordinates were computation is done

  The image geometry DATA is only used if the MP array is not valid.  This
  would occur if the input image label did not contain MP or PERSLAB labels.
  The contents of DATA should be as follows:

       DATA(1-18)  = OM matrix              
       DATA(19-24) = RS vector
       DATA(25) = polar radius (km)            
       DATA(26) = equatorial radius (km) 
       DATA(27) = focal length (mm)
       DATA(28) = optical axis line        
       DATA(29) = optical axis sample      
       DATA(30) = scale in pixels/mm.      
       DATA(31)-DATA(37) = ignored
       DATA(38) = range to target body (km)
       DATA(39)-DATA(40) = ignored

  The contents of the output buffer are:
    PBUF(1) = horizontal picture scale in meters/pixel
    PBUF(2) = vertical picture scale in meters/pixel
    PBUF(3) = north angle
    PBUF(4) = sun azimuth
    PBUF(5) = spacecraft azimuth
    PBUF(6) = slant range
    PBUF(7) = north azimuth
    PBUF(8) = latitude of surface point where computations are performed
    PBUF(9) = longitude of surface point where computations are performed

  If an argument cannot be computed, its value is returned as -999.

  NOTE: if MPTYPE is not 7 or 8, or if the projection type is not POINT
  PERSPECTIVE, then the azimuth angles and slant range cannot be calculated
  and therefore are not returned ( PBUF(3) - PBUF(6)).

  MPTYPE = 7 for image space (DATA buffer is used)
         = 8 for object space (DATA buffer is used)
         = other (MP buffer is used)

2 Operation

  The picture scale, north azimuth, sun azimuth, and spacecraft azimuth
  are computed and returned along with the line-sample location at which
  the computations are performed.

  The horizontal and vertical picture scales are determined by computing the
  distance (in meters) between two adjacent pixels.

  If the image has been map-projected, the center of the image is chosen
  for the computations.

  If the image has not been projected, it is assumed to be in object-space.
  The point in the image at which the above values are computed
  is determined as follows:
 
  (1) If all four corners of the image lie on the target-surface, the center
      of the image is used.
  (2) Otherwise, if the planet-center is visible, it is used.
  (3) Otherwise, the margins of the image are scanned at 20-pixel
      intervals for a point at highest resolution. 

2 History

  Original Programmer: Gary Yagi, April 10, 1991
  Current Cognizant Programmer: Gary Yagi
  Source Language: C
  Revision history:
      4 July  91  GMY  Add slant range
      1 June  91  GMY  Add vertical scale and azimuth angles
                  93   PDB  Converted from Fortran to 'C' {but not delivered}
      6 June  94  GAC  Completed porting.  
      2 Jan   95  JCT  (CRI) Made portable for UNIX
      5 Feb   96  GMY  Beef up algorithm so that it always returns a scale
                       (FR 89104)
      5 May   96  GMY  Get it to work on PERSLAB and MAP3 labels
     30 May   96  GMY  Delete RPD and DPR functions and call zrpd and zdpr
                       instead (DFR)
     31 July  96  GMY  Replaced GETSPICE call with GETSPICE2 in test program
                       (FR 89172).  Corrected SCAZ calculation.
     16 Dec   96  OAM  Included subroutines spice2convev and lighting in
                       picscale.com.
      1 Nov   97  GMY  Correct computation of azimuth angles (FR 90511)
                       Add north angle, lat,lon
     21 Mar   98  GMY  Fixed call to zellipse_inv (AR 9325)
                       Fixed NORAZ in tzpicscale (AR 9411)
     04 May   98  RRP  In zpicscale updated the lat and lon calculations 
                       through psxy2ll to calculate at pcs+0.1 and pcl+0.1.
                       and the return value of horizontal and vertical scales
                       are multiplied by 10 to level out the 1/10 step factor.
                       And updated psll2xy to default rline and
                       rsamp to -999.0 if call to zpproj is not successful.
     25 June 1998 TXH  Removed subroutine MATRIXMULT, because it causes 
                       compilation warnings on VMS.  Since this subroutine 
                       is not being called by any existing programs, this 
                       change should have little impact.
     30 Sept 99   gmy  Check that magnitude of vector dot product is not greater
                       than 1 before taking arc cos.
$!-----------------------------------------------------------------------------
$ create rpicscale.hlp
1 RPICSCALE

   Compute picture scale, north angle, azimuth angles, and slant range for
   a selected point in an image of the ring plane.


   FORTRAN Calling Sequence:

         RPICSCALE(SBUF,DATA,MP,MPTYPE,SCLAT,SCLON,R,NL,NS,PBUF,LINE,SAMP)
   where:
       	 REAL*8    SBUF(200)    Buffer returned by subroutine GETSPICE
         REAL*4    DATA(40)     Array containing image geometry data (see below)
         INTEGER   MP           Address of Structure created by mpInit(MP)
         INTEGER   MPTYPE       map projection type
       	 REAL*8    SCLAT        Subspacecraft planetocentric latitude (degrees)
       	 REAL*8    SCLON        Subspacecraft planetocentric west-longitude deg
         REAL*8    R            Ring radius
         INTEGER*4 NL           Number of lines in image
         INTEGER*4 NS           Number of samples in image
       	 REAL*4    PBUF(30)     Output array of computed terms (see below)
         REAL*8    LINE,SAMP    (Output) Coordinates where computation is done


  C Calling Sequence:

        #include "mp_routines.h"
        zrpicscale(sbuf,data,mp,mptype,sclat,sclon,r,nl,ns,pbuf,&line,&sample);
  where:
        double *SBUF;           Buffer returned by subroutine GETSPICE
        float  *data;           Image geometry data (see below)
       	MP     *mp              Address of Structure created by mpInit(MP)
        int     mptype          map projection type
      	double  sclat;          Subspacecraft planetocentric latitude in degrees
      	double  sclon;          Subspacecraft planetocentric west-longitude in degrees
        double  r;              Ring radius at which computation is performed
       	int     nl;             Number of lines in image
        int     ns;             Number of samples in image
        float  *pbuf;           Output array of computed terms (see below)
        double *line,*samp;     (Output) Coordinates were computation is done

  The image geometry DATA is only used if the MP array is not valid.  This
  would occur if the input image label did not contain MP or PERSLAB labels.
  The contents of DATA should be as follows:

       DATA(1-18)  = OM matrix              
       DATA(19-24) = RS vector
       DATA(25) = polar radius (km)            
       DATA(26) = equatorial radius (km) 
       DATA(27) = focal length (mm)
       DATA(28) = optical axis line        
       DATA(29) = optical axis sample      
       DATA(30) = scale in pixels/mm.      
       DATA(31)-DATA(37) = ignored
       DATA(38) = range to target body (km)
       DATA(39)-DATA(40) = ignored

  The contents of the output buffer are:
    PBUF(1) = radial picture scale in meters/pixel
    PBUF(2) = longitudinal picture scale in meters/pixel
    PBUF(3) = north angle
    PBUF(4) = sun azimuth
    PBUF(5) = spacecraft azimuth
    PBUF(6) = slant range
    PBUF(7) = north azimuth
    PBUF(8) = latitude of surface point where computations are performed
    PBUF(9) = longitude of surface point where computations are performed

  If an argument cannot be computed, its value is returned as -999.

  NOTE: if MPTYPE is not 7 or 8, or if the projection type is not POINT
  PERSPECTIVE, then the azimuth angles and slant range cannot be calculated
  and therefore are not returned ( PBUF(3) - PBUF(6)).

  NOTE: RPICSCALE currently is not implemented for map projected images.

  MPTYPE = 7 for image space (DATA buffer is used)
         = 8 for object space (DATA buffer is used)
         = other (MP buffer is used)

2 Operation

  If the image has not been projected, it is assumed to be in object-space.

  The picture scale, north azimuth, sun azimuth, and spacecraft azimuth
  are computed and returned along with the line-sample location at which
  the computations are performed.

  All computations are performed at the point on the visible ring at the given
  radius closest to the camera.

  For a detailed description of the algorithms used see the documentation for
  subroutines RAZIMUTH and RING_SCALE.


2 History

  Original Programmer: Gary Yagi, November 1, 1997
  Current Cognizant Programmer: Gary Yagi
  Source Language: C
  Revision history: New

  25 Jun 1998 TXH   Modified rpicscale's call to zring_scale to have its
                    float parameters to be passed-by-reference.   The problem
                    was found under SGI, where its float values are treated
                    as doubles.

$!-----------------------------------------------------------------------------
$ create lighting.hlp
1 LIGHTING

Calculate the phase, incidence, and emission angles for a specific latitude,
longitude.

FORTRAN calling sequence:

      CALL LIGHTING(SBUF,LAT,LON,phase,incidence,emission)

where:

      REAL*8 SBUF(200)  !Buffer returned by subroutine GETSPICE95
      REAL*8 LAT        !Planetocentric latitude of surface pt (deg)
      REAL*8 LON        !West-longitude of surface pt (deg)
      REAL*8 PHASE,INCIDENCE,EMISSION	!Output lighting angels (deg)

C calling sequence:

      zlighting(sbuf,lat,lon,phase,incidence,emission);

where:

      double sbuf[100];  /* Buffer returned by subroutine GETSPICE95 */
      double lat;        /* Planetocentric latitude of surface pt (deg) */
      double lon;        /* West-longitude of surface pt (deg) */
      double *phase,*incidence,*emission;  Output lighting angels (deg) */

2 Operation

Lighting uses an oblate spheroid model of the target body.  The equatorial
and polar radii are extracted from SBUF.

The latitude-longitude of the subsolar and subspacecraft points, and the
distances from the target center to the sun and spacecraft are extracted from
SBUF.  These values are sufficient to compute the following at the surface
point defined by (LAT,LON):

   S = unit vector from surface point to Sun
   C = unit vector from surface point to spacecraft
   N = unit vector normal to surface

Then:

   PHASE = ARCCOS(S o C)
   INCIDENCE = ARCCOS(N o S)
   EMISSION = ARCCOS(N o C)

where o is the dot product.  All computations are performed in double precision.

2 History

  Original Programmer: Gary Yagi, November 1, 1996
  Current Cognizant Programmer: Gary Yagi
  Source Language: F
  Revision history: New
$!-----------------------------------------------------------------------------
$ create spice2convev.hlp
1 SPICE2CONVEV

Create a data buffer suitable for input to CONVEV from input image geometry data
and camera constants.

FORTRAN calling sequence:

      CALL SPICE2CONVEV(SBUF,FL,OAL,OAS,SCALE,MPTYPE,data)

where:

      REAL*8 SBUF(200)  !Buffer returned by subroutine GETSPICE95
      REAL*4 FL		!Camera focal length (mm)
      REAL*4 OAL,OAS	!Optical axis intercept line,sample
      REAL*4 SCALE	!Picture scale (pixels/mm)
      INTEGER*4 MP	!7=image space, 8=object space
      REAL*4 DATA(40)	!Output CONVEV data buffer

C calling sequence:

      zspice2convev(sbuf,fl,oal,oas,scale,mptype,data);

where:

      double sbuf[100];  /* Buffer returned by subroutine GETSPICE95 */
      float  fl;	 /* Camera focal length mm */
      float  oal,oas;	 /* Optical axis intercept line,sample */
      float  scale;	 /* Picture scale (pixels/mm) */
      int mptype;	 /* 7=image space, 8=object space */
      float data[40];	 /* Output CONVEV data buffer */

2 Operation

SPICE2CONVEV copies the OM matrix, RS vector, polar and equatorial radii,
and target range from SBUF into DATA.  The input arguments FL, OAL, OAS,
SCALE, and MPTYPE are also copied to DATA.

2 History

  Original Programmer: Gary Yagi, November 1, 1996
  Current Cognizant Programmer: Gary Yagi
  Source Language: F
  Revision history: New
$ Return
$!#############################################################################
