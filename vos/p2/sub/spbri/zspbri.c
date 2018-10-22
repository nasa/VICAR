#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/*

C Language Bridges for GENERAL NAIF TOOLKIT ROUTINES:

        BODEUL  DAFCLS  DPR     M2Q     SCENCD
        BODFND  DAFENA  ERRACT  MXM     SCE2T
        BODVAR  DAFFNA  ERRPRT  MXMT    SCS2E
	BODN2C  		MXV	SCT2E	
	CKGP			MTXV	SPKAPP
        CKGPAV  DAFGS   ET2UTC  PI	SPKLEF
        CKLPF   DAFHSF  FAILED  RECLAT  SPKSSB
        CLPOOL  DAFOPR  HALFPI          TWOPI
        DAFADA  DAFOPW  IRFROT  ROTATE  UTC2ET
        DAFBFS  DAFPS   LDPOOL  ROTMAT  VMINUS
        DAFBNA  DAFUS   M2EUL   RTPOOL  XPOSE
	======================================
	CKBSS	CKSNS 	CKPFS	CKUPF
	DAFA2B	DAFB2A	EUL2M	SCTIKS
	SPCA2B	SPCB2A	SPKUEF	SURFPT
	VADD	VNORM	VSEP	VSUB
	TXTOPR  SPCT2B	TXTCLS	SCE2S
	Written By: Sam Le, 4/27/95
	======================================
        LATREC
        Done by: Helen Mortensen
*
* -----------   --------------- ------------------------------------------
*  7-Oct-1999	Payam Zamani	Fixed bug in ET2UTC call interface.  Format
*				parameter was passed as char rather than
*				pointer to array of charcters.
* 30-Sep-1999	Payam Zamani	Added C bridges for:
*					STR2ET
*					SPKEZ
*					VPACK
*					RESET
*/

/*
1st bridge for BODEUL, called from C 
*/
void zbodeul( body, et, ra, dec, w, lambda )
int    body;         	/* input */
double et;          	/* input */
double *ra;         	/* output */
double *dec;         	/* output */
double *w;         	/* output */
double *lambda;         /* output */

{
   FTN_NAME(bodeul) ( &body, &et, ra, dec, w, lambda );
}

/*
 1st bridge for BODFND, called from C
*/
int zbodfnd(body, item)

int body;
char *item;
{
   int i, status;
   i=strlen(item);

   status = FTN_NAME(xbodfnd) (&body, item, &i );
   return status;
}

/*
 1st bridge for BODVAR, called from C 
*/
void zbodvar(body, item, dim, values)

int body;
char *item;
int *dim;
double *values;
{
   int i;
   i=strlen(item);

   FTN_NAME(xbodvar) (&body, item, &i, dim, values);
}


/*
 1st bridge for BODN2C, called from C 
*/
void zbodn2c(body_name, body_id, status)

char *body_name;
int *body_id;
int *status;
{
   int i;

   i = strlen(body_name);

   FTN_NAME(xbodn2c) (body_name, &i, body_id, status);
}


/*
 1st bridge for CKGP, called from C 
*/
void zckgp(inst, sclkdp, tol, ref, cmat, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xckgp) (&inst,&sclkdp,&tol,ref,&i,cmat,clkout,status);
}

/*
 1st bridge for CKGPAV, called from C 
*/
void zckgpav(inst, sclkdp, tol, ref, cmat, av, clkout, status)

int inst;          	/*input*/
double sclkdp;          /*input*/
double tol;          	/*input*/
char *ref;          	/*input*/
void *cmat;   		/*output*/
void *av;   		/*output*/
double *clkout;   	/*output*/
int *status;   		/*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xckgpav) (&inst,&sclkdp,&tol,ref,&i,cmat,av,clkout,status);
 }

/*
 1st bridge for CKLPF, called from C 
*/
void zcklpf(x, handl)

char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME(xcklpf) (x, &i, handl);
}

/*
1st bridge for CLPOOL, called from C 
*/
void zclpool()
{
   FTN_NAME(clpool) ();
}

/*
Bridge for DAFADA, called from C 
*/
void zdafada( buf, n)
int n;       /*input*/
void *buf;      /*input*/

{
   FTN_NAME(dafada) ( buf, &n);
}

/*
1st bridge for DAFBFS, called from C 
*/
void zdafbfs( handle )
int    handle;      /*input*/

{
   FTN_NAME(dafbfs) ( &handle );
}

/*
 1st-stage bridge for DAFBNA, called from C 
*/
void zdafbna( handle, sum, name)

char *name; /*input*/
int handle; /*input*/
void *sum; /*input*/
{
   int i;
   i=strlen(name);

   FTN_NAME(xdafbna) (&handle, sum, name, &i);
}

/*
 1st bridge for DAFCLS, called from C 
*/
void zdafcls(handle)

int handle;      /*input*/
{
   FTN_NAME(dafcls) (&handle);
}

/*
Bridge for DAFENA, called from C 
*/
void zdafena()

{
  FTN_NAME(dafena) ();
  return;
}


/*
 1st bridge for DAFFNA, called from C 
*/
void zdaffna(status)
int *status;     /*outpu*/
{
   FTN_NAME(xdaffna) (status);
}

/*
1st bridge for DAFGS, called from C 
*/
void zdafgs( sum )
void *sum;      /*output*/

{
   FTN_NAME(dafgs) ( sum );
}

/*
Bridge for DAFHSF, called from C 
*/
void zdafhsf( handle , nd, ni )
int    handle;      /*input*/
int    *nd;         /*output*/
int    *ni;         /*output*/

{
   FTN_NAME(dafhsf) ( &handle, nd, ni );
}

/*
 1st bridge for DAFOPR, called from C 
*/
void zdafopr(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME(xdafopr) (fname, &i, handle);
}


/*
 1st-stage bridge for DAFOPW, called from C 
*/
void zdafopw(fname, handle)

char *fname;
int *handle;
{
   int i;
   i=strlen(fname);

   FTN_NAME(xdafopw) (fname, &i, handle);
}

/*
Bridge for DAFPS, called from C 
*/
void zdafps( nd, ni, dc, ic, sum )
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*input*/
void  *ic;        /*input*/
void *sum;      /*output*/

{
   FTN_NAME(dafps) ( &nd, &ni, dc, ic, sum );
}

/*
1st bridge for DAFUS, called from C 
*/
void zdafus( sum, nd, ni, dc, ic )
void *sum;      /*input*/
int    nd;        /*input*/
int    ni;        /*input*/
void *dc;        /*output*/
void  *ic;        /*output*/

{
   FTN_NAME(dafus) ( sum, &nd, &ni, dc, ic );
}

/*
1st bridge for DPR, called from C 
*/
double zdpr()
{
  double dpr();
  double FTN_NAME(dpr) ();

  return FTN_NAME(dpr) ();
}

/*
1st bridge for RPD, called from C 
*/
double zrpd()
{
  double rpd();
  double FTN_NAME(rpd) ();

  return FTN_NAME(rpd) ();
}

/*
 1st bridge for ERRACT, called from C 
*/
void zerract(x,y)
char *x,*y;
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xerract) (x,&i,y,&j);
}

/*
 1st bridge for ERRPRT, called from C 
*/
void zerrprt(x,y)
char *x;            /*input*/
char *y;          /*i/o*/
{
   int i,j;
   i=strlen(x);
   j=strlen(y);
   FTN_NAME(xerrprt) (x,&i,y,&j);
}

/*

NAIF SPICE C-language Bridge for ET2UTC

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

 History:

 February 25, 1994   	DEVELOPER NOTE

 The UTC string variable UTC_temp is of fixed length 80 to provide an
 appropriate interface with FORTRAN SPICE routine ET2UTC. UTC_temp is pruned 
 to remove superfluous blanks and returned to the user in the string UTC.

 02mar95 -lwk- prune the UTC string *before* moving it to the user buffer,
		to reduce chance of overwriting memory (can't be totally
		avoided since there is no way to emulate the character*(*)
		argument to ET2UTC from C)
*/

void zet2utc(ephemeris_time,format,precision,UTC)
double ephemeris_time;
char *format;
int precision;
char *UTC;
{
int  i;
char UTC_temp[80];

    i = strlen( format);
    FTN_NAME(xet2utc) (&ephemeris_time, format, &i, &precision, UTC_temp);

/* Process FORTRAN character string */

/* Remove blanks from the end of this string */
i = 79;
while(UTC_temp[i] == ' ' && i>0)
	i--;

strncpy(UTC, UTC_temp, ++i);

/* Add NULL terminator */
UTC[i] = '\0';
}

/*
 1st bridge for FAILED, called from C 
*/
int zfailed()

{
 int status;

   status = FTN_NAME(failed) ();
   return status;
}

/*
1st bridge for PI, called from C 
*/
double zpi()
{
  double pi();
  double FTN_NAME(pi) ();

  return FTN_NAME(pi) ();
}

/*
1st bridge for HALFPI, called from C 
*/
double zhalfpi()
{
  double halfpi();
  double FTN_NAME(halfpi) ();

  return FTN_NAME(halfpi) ();
}

/*
1st bridge for TWOPI, called from C 
*/
double ztwopi()
{
  double twopi();
  double FTN_NAME(twopi) ();

  return FTN_NAME(twopi) ();
}

/*
1st bridge for IRFROT, called from C 
*/
void zirfrot( refa, refb, rotab )
int    refa;  /*input*/
int    refb;  /*input*/
void *rotab;  /*output*/

{
 FTN_NAME(irfrot) ( &refa, &refb, rotab );
}

/*
 1st bridge for LDPOOL, called from C 
*/
void zldpool(x)
char *x;
{
   int i;
   i=strlen(x);
   FTN_NAME(xldpool) (x,&i);
}

/*
1st bridge for M2EUL, called from C 
*/
void zm2eul( r, axis3, axis2, axis1, angle3, angle2, angle1 )
void *r;        /* input */
int    axis3;        /* input */
int    axis2;        /* input */
int    axis1;        /* input */
double *angle3;        /* output */
double *angle2;        /* output */
double *angle1;        /* output */

{
   FTN_NAME(m2eul) ( r, &axis3, &axis2, &axis1, angle3, angle2, angle1 );
}

/*
Bridge for M2Q, called from C 
*/
void zm2q( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/


{
   FTN_NAME(m2q) ( m1, mout );
}

/*
1st bridge for MXM, called from C 
*/
void zmxm( m1, m2, mout )
void *m1;   /*input*/
void *m2;   /*input*/
void *mout;   /*output*/


{
 FTN_NAME(mxm) ( m1, m2, mout );
}

/*
1st bridge for MXMT, called from C 
*/
void zmxmt( m1, m2, mout )
void *m1;		/*input*/
void *m2;		/*input*/
void *mout;		/*output*/


{
 FTN_NAME(mxmt) ( m1, m2, mout );
}

/*
1st bridge for MXV, called from C 
*/
void zmxv( matrix, vin, vout )
void  *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 FTN_NAME(mxv) ( matrix, vin, vout);
}

/*
1st bridge for MTXV, called from C 
*/
void zmtxv( matrix, vin, vout )
void *matrix;    	/*input*/
void  *vin;          	/*input*/
void  *vout;         	/*output*/

{
 FTN_NAME(mtxv) ( matrix, vin, vout);
}

/*
1st bridge for RECLAT, called from C 
*/
void zreclat(rectan, radius, longi, lat)

void *rectan;  /*input*/
double *radius;
double *longi;
double *lat;
{
   FTN_NAME(reclat) (rectan, radius, longi, lat);
}


/*
1st bridge for ROTATE, called from C 
*/
void zrotate( angle, iaxis, mout )
double angle;        	/* input */
int    iaxis;         	/* input */
void *mout;   		/* output */

{
   FTN_NAME(rotate) ( &angle, &iaxis, mout );
}

/*
1st bridge for ROTMAT, called from C 
*/
void zrotmat( m1, angle, iaxis, mout )
void  *m1;      /*input*/
double angle;      /*input*/
int    iaxis;      /*input*/
void  *mout;      /*output*/

{
   FTN_NAME(rotmat) ( m1, &angle, &iaxis, mout );
}

/*
 1st bridge for RTPOOL, called from C 
*/
void zrtpool(x,dim,values,flag)
char 	*x;
int 	*dim;
double 	*values;
int 	*flag;
{
   int i;
   i=strlen(x);

   FTN_NAME(xrtpool) (x,&i,dim,values,flag);
}

/*
1st bridge for SCENCD, called from C 
*/
void zscencd( sc, text, sclkdp)
int    sc;           /*input*/
char *text;         /*input*/
double *sclkdp;       /*output*/

{
  int i;
  i=strlen(text);
  FTN_NAME(xscencd) (  &sc, text, &i, sclkdp);
}

/*
1st bridge for SCE2T, called from C 
*/
void zsce2t( sc, et, sclkdp)
int    sc;           /*input*/
double et;         /*input*/
double *sclkdp;       /*output*/

{
   FTN_NAME(sce2t) (  &sc, &et, sclkdp);
}

/*

NAIF SPICE C-language Bridge for SCS2E

Bridge to be called from C language applications. 

 Written by:		Justin McNeill
 Date:			February 14, 1994
 Cognizant Engineer:	Justin McNeill (JFM059)

*/
void zscs2e(spacecraft_code,SCLK,ephemeris_time)
int spacecraft_code;
char *SCLK;
double *ephemeris_time;
{
int i;

i=strlen(SCLK);		/* Get string length of SCLK */
FTN_NAME(xscs2e) (&spacecraft_code, SCLK, &i, ephemeris_time);
}

/*
1st bridge for SCT2E, called from C 
*/
void zsct2e( sc, sclkdp, et )
int    sc;           /*input*/
double sclkdp;      /*input*/
double *et;          /*output*/

{
   FTN_NAME(sct2e) ( &sc, &sclkdp, et );
}

/*
 1st bridge for SPKAPP, called from C 
*/
void zspkapp( targ, et, ref, sobs, abcorr, starg, lt )

int targ;          /*input*/
double et;        /*input*/
char *ref;        /*input*/
void *sobs;        /*input*/
char *abcorr;        /*input*/
void *starg;        /*output*/
double *lt;       /*output*/
{
   int i,j;
   i=strlen(ref);
   j=strlen(abcorr);

   FTN_NAME(xspkapp) (&targ,&et,ref,&i,sobs,abcorr,&j,starg,lt);
}

/*
 *===========================================================================
 * 1st bridge for FAILED, called from C 
 *===========================================================================
 */
int zreset()

{
	int		status;

    status = FTN_NAME(reset)();
    return status;
}
/*
 *===========================================================================
 * 1st bridge for SPKEZ, called from C
 *===========================================================================
 */
void	zspkez( 
int		target,			/* target id			*/
double		et,			/* Ephemeris time		*/
char		*ref,
char		*abcorr,
int		obs,
void		*strag,
double		*lt)
{
	int		i;
	int		j;

    i = strlen( ref);
    j = strlen( abcorr);
    FTN_NAME( xspkez)( &target,	&et, ref, &i, abcorr, &j, &obs, strag, lt);
}


/*
 1st bridge for SPKLEF, called from C 
*/
void zspklef(x, handl)
char *x;
int *handl;
{
   int i;
   i=strlen(x);

   FTN_NAME(xspklef) (x, &i, handl);
}

/*
 1st bridge for SPKSSB, called from C 
*/
void zspkssb(targ, et, ref, starg)

int targ;           /*input*/
double et;           /*input*/
char *ref;           /*input*/
void *starg;      /*output*/
{
   int i;
   i=strlen(ref);

   FTN_NAME(xspkssb) (&targ, &et, ref, &i, starg);
}

/*
 *===========================================================================
 * 1st bridge for STR2ET, called from C
 *===========================================================================
 */
void	zstr2et(
char		*str,
double		*et)
{
	int		i;

    i = strlen( str);
    FTN_NAME( xstr2et) ( str, &i, et);
}

/*
 1st bridge for UTC2ET, called from C 
*/
void zutc2et(utc, et)

char *utc;
double *et;

{
   int i;
   i=strlen(utc);

   FTN_NAME(xutc2et) (utc, &i, et);

}

/*
1st bridge for VMINUS, called from C 
*/
void zvminus( v1, vout )
void *v1;       /*input*/
void *vout;      /*output*/


{
   FTN_NAME(vminus) ( v1, vout );
}

/*
 *============================================================================
 * 1st bridge for VPACK, called from C
 *============================================================================
 */
void	zvpack(
double		x,
double		y,
double		z,
void		*v)
{
    FTN_NAME( vpack) ( &x, &y, &z, v);
}

/*
Bridge for XPOSE, called from C 
*/
void zxpose( m1, mout )
void *m1;   /*input*/
void *mout;   /*output*/
{
 FTN_NAME(xpose) ( m1, mout );
}
/*=====================================================
1st bridge for CKBSS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Search through loaded file to find data segment with the
same s/c instrument and sclk value (or within sclk_tol)
=======================================================*/
void zckbss(inst, sclk, tol, needav)
 int	inst;				/* input: instrument id	*/
 double	sclk;				/* input: sclk value	*/
 double	tol;				/* input: error_tolrnce */
 int	needav;				/* input: need av ?	*/
{
 FTN_NAME(ckbss) (&inst, &sclk, &tol, &needav);
}
/*=====================================================
1st bridge for CKSNS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
CKBSS specifies the search criteria and CKSNS searches
through the loaded files for data segment(s) that match
the criteria; segment id and descriptor are also returned
=======================================================*/
void zcksns(int *handle, double *descr, char *segid, int *found, ZFORSTR_PARAM)
#if 0
 int		*handle;		/* output	*/
 double		*descr;			/* output	*/
 char 		*segid;			/* output	*/
 int		*found;			/* output	*/
#endif
{
 ZFORSTR_BLOCK
 char	temp[41];

 FTN_NAME(xcksns) (handle, descr, temp, found);
 zsfor2c(segid, 40, temp, &handle, 4, 3, 1, found);
}
/*==============================================
1st bridge for CKPFS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Evaluate pointing data from a given segment for
a given time.
================================================*/
void zckpfs(handle, descr, sclkin, tol, needav, 
			cmat, av, clkout, found)
 int		handle;			/*   input	*/
 double		*descr;			/*   input	*/
 double		sclkin;			/*   input	*/
 double		tol;			/*   input	*/
 int		needav;			/*   input	*/
 double		*cmat;			/*   output	*/
 double		*av;			/*   output	*/
 double		*clkout;		/*   output	*/
 int		*found;			/*   output	*/
{
 int		i, j;
 double		*dptr, tcmat[3][3];

 FTN_NAME(xckpfs) (&handle, descr, &sclkin, &tol,
		&needav, tcmat, av, clkout, found);
 dptr = cmat;
 for(i = 0; i < 3; i++)
    for(j = 0; j < 3; j++) {
       *dptr = tcmat[j][i];
       dptr++;
       }
}
/*=====================================================
1st bridge for CKUPF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload a CK pointing file so that it will no longer be
searched by the readers.
=======================================================*/
void zckupf(handle)
 int handle;
{
 FTN_NAME(ckupf) (&handle);
}
/*=====================================================
1st bridge for DAFA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFA2B
converts the ascii (text) file into its daf file (binary).
=======================================================*/
void zdafa2b(ascii_name, daf_name, resv)
 char	*ascii_name;
 char	*daf_name;
 int	resv;
{
 int	asc_len,
	daf_len;

 asc_len = strlen(ascii_name);
 daf_len = strlen(daf_name);

 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFA2B::File Name Is Too Long\n");
 else
    FTN_NAME(xdafa2b) (ascii_name, &asc_len,
		daf_name, &daf_len, &resv);
}
/*=====================================================
1st bridge for DAFB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Given to file names "daf_file" & "ascii_file", DAFB2A
converts the daf (binary) file into its ascii file (text).
=======================================================*/
void zdafb2a(dname, aname)
 char   *dname;
 char   *aname;
{
 int    daf_len,
        asc_len;

 asc_len = strlen(aname);
 daf_len = strlen(dname);
 if ((asc_len > 80) || (daf_len > 80))
    printf("ZDAFB2A::File Name Is Too Long\n");
 else
    FTN_NAME(xdafb2a) (dname, &daf_len, aname, &asc_len);
}
/*=====================================================
1st bridge for EUL2M, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
EUL2M constructs a rotation matrix from a set of Euler
angles and their rotation axes

last modified: Tue Oct  8 18:17:04 PDT 1996
	remove matrix inversion requested by LWK.
=======================================================*/
void zeul2m(angle3, angle2, angle1, axis3, axis2, axis1, matrix)
 double                 angle3;                 /* input        */
 double                 angle2;                 /* input        */
 double                 angle1;                 /* input        */
 int                    axis3;                  /* input        */
 int                    axis2;                  /* input        */
 int                    axis1;                  /* input        */
 double                 *matrix;                /* output       */
{
 FTN_NAME(eul2m) (&angle3, &angle2, &angle1,
                &axis3, &axis2, &axis1, matrix);
}
/*=====================================================
1st bridge for SPCA2B, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCA2B convert the given ascii CK/SPK file into its
binary format, including the comment area.
=======================================================*/
void zspca2b(ascfile, binfile)
 char *ascfile; char *binfile;
{
 int    asclen, binlen;

 asclen = strlen(ascfile);
 binlen = strlen(binfile);
 if ((asclen > 80) || (binlen > 80))
    printf("ZSPCA2B: File name is too long\n");
 else
    FTN_NAME(xspca2b) (ascfile, &asclen, binfile, &binlen);
}
/*=====================================================
1st bridge for SPCB2A, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSPCB2A convert the given binary CK/SPK file into its
ascii format, including the comment area.
=======================================================*/
void zspcb2a(binfile, ascfile)
 char *binfile;
 char *ascfile;
{
 int binlen, asclen;

 binlen = strlen(binfile);
 asclen = strlen(ascfile);
 if ((binlen > 80) || (asclen > 80))
    printf("ZSPCB2A: File name is too long\n");
 else
    FTN_NAME(xspcb2a) (binfile, &binlen, ascfile, &asclen);
}
/*=====================================================
1st bridge for SCTIKS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
ZSCTIKS receives a s/c_id and a clock string then
convert the clock string into ticks for that specific
spacecraft
=======================================================*/
void zsctiks(sc, clkstr, ticks)
 int            sc;
 char           *clkstr;
 double         *ticks;
{
 int    i;
 char	for_str[15];

 i = strlen(clkstr);
 FTN_NAME(xsctiks) (&sc,clkstr, &i, ticks);
 }
/*=====================================================
1st bridge for SPKUEF, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Unload an ephemeris file so that it will no longer be
searched by the readers
=======================================================*/
void zspkuef(handle)
 int handle;
{
 FTN_NAME(spkuef) (&handle);
}
/*=====================================================
1st bridge for VADD, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
add two vector v1[3] and v2[3] and return the results
back via vout[3]
=======================================================*/
void zvadd_mat(v1, v2, vout)
 double	*v1;
 double *v2;
 double *vout;
{
 FTN_NAME(vadd) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFPT, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Determine the intersection of a line-of-sight vector
with the surface of an ellipsoid.
=======================================================*/
void zsurfpt(pos, u, a, b, c, pts, fnd)
 double *pos;			/* input	*/
 double *u;			/* input	*/
 double a;			/* input	*/
 double b;			/* input	*/
 double c;			/* input	*/
 double *pts;			/* output	*/
 int	*fnd;			/* output	*/
{
 FTN_NAME(xsurfpt) (pos, u, &a, &b, &c, pts, fnd);
}
/*=====================================================
1st bridge for VNORM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the magnitude of a double precision,
3-dimensional vector.
=======================================================*/
double zvnorm(v)
 double *v;
{
 double value;
 double FTN_NAME(vnorm) (double *);
 return (FTN_NAME(vnorm) (v));
}
/*=====================================================
1st bridge for VSUB, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Compute the different between two 3-dimensional, double
precision vectors
=======================================================*/
void zvsub(v1, v2, vout)
 double	*v1;			/* input	*/
 double *v2;			/* input	*/
 double *vout;			/* output	*/
{
 FTN_NAME(vsub) (v1, v2, vout);
}
/*=====================================================
1st bridge for SURFNM, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Computes the outward-pointing, unit normal vector
from a point on the surface of an ellipsoid.
=======================================================*/
void zsurfnm(a, b, c, pt, out)
 double	a;
 double b;
 double c;
 double *pt;
 double *out;
{
 FTN_NAME(surfnm) (&a, &b, &c, pt, out);
}
/*=====================================================
1st bridge for VSEP, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Find the separation angle in radians between two double
precision, 3-dimensional vectors. This angle is defined as
zero if either vector is zero.
=======================================================*/
double zvsep(v1, v2)
 double	*v1;
 double *v2;
{
 double FTN_NAME(vsep) (double *, double *);
 return (FTN_NAME(vsep) (v1, v2));
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Open a text (ASCII) file for reading, return the unit number
========================================================*/
void ztxtopr(fname, unit)
 char *fname; int *unit;
{
 int flen;

 flen = strlen(fname);
 if ((flen > 80) || (flen <= 0)) {
    printf("ZTXTOPR: File name is too long/short\n");
    exit (0);
    }
 FTN_NAME(xtxtopr) (fname, &flen, unit);
}
/*======================================================
1st bridge for TXTOPR, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Convert a given text file into its corresponding binary
format.
========================================================*/
void zspct2b(unit, fname)
 int	unit;
 char	*fname;
{
 int	len;
 len = strlen(fname);
 if (len >= 80) 
    printf("Error File Name Is Too Long\n");
 else
    FTN_NAME(xspct2b) (&unit, fname, len);
}
/*======================================================
1st bridge for TXTCLS, called from C
Written By: Sam Le
Date      : 04/27/1995

Development Note:
Close the text file associated with the given unit
========================================================*/
void ztxtcls(unit)
 int	unit;
{
 FTN_NAME(xtxtcls) (&unit);
}
/*=======================================================
1st bridge for SCE2S, called from C
Written By: Sam Le
Date	  : 06/6/1995

Development Note:
Convert a given ETC (double precision) to SCLK string
=========================================================*/
void zsce2s(sc, etc, sclk)
 int	sc;
 double	etc;
 char	*sclk;
{
 int	i;
 char	SCLK_temp[80];

 FTN_NAME(xsce2s) (&sc, &etc, SCLK_temp);

 i = 79;
 while (SCLK_temp[i] == ' ') i--;
 i++; SCLK_temp[i] = '\0';
 strcpy(sclk, SCLK_temp);
}
/*=======================================================
1st bridge for DAFGH, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFGH returns the handle of the file being searched
currenty. MIPS SPICE routines used this with GAFHFN to
retrieve the name and id of the file being searched.
=========================================================*/
void zdafgh(handle)
 int *handle;
{
 FTN_NAME(dafgh) (handle);
}
/*=========================================================
1st bridge for DAFHFN, called from C
Written by: Sam Le
Date	  : 09/14/1995

Development Note:
DAFHFN return the name of the file currently being searched
given its file handle. This subroutine does not check for
the string length of the fname. It assumes that the calling
program allocate enough memory space to store the file name.
===========================================================*/
void zdafhfn(int *handle, char *fname, ZFORSTR_PARAM)
{
 ZFORSTR_BLOCK
 char	temp_fname[256];

 FTN_NAME(xdafhfn) (handle, temp_fname);
 zsfor2c(fname, 256, temp_fname, &handle, 2, 2, 1, fname);
}
/*=========================================================
1st bridge for LATREC, called from C
Written by: Helen Mortensen
Date	  : 01/04/1998

===========================================================*/
void zlatrec(range, lon, lat, rec_coordinates)
 double range, lon, lat, *rec_coordinates;
{
 FTN_NAME(latrec) (&range, &lon, &lat, rec_coordinates);
}
