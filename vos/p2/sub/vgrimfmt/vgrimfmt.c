/*===========================================================================*
 | VGRIMFMT   -- Fortran bridge function to return image format information  |
 |		 for each Voyager telemetry format. (UNIX version)           |
 |									     |
 |	vgrimfmt(imcode,fmt,&scan_rate,spix,npix,&ind)			     |
 |			where ind=1 for normal return			     |
 |			         =0 for invalid image format		     |
 |									     |
 |	Input:     imcode						     |
 |									     |
 |	Outputs:   fmt,scan_rate,spix,npix			             |
 *============================================================================
 *	Module history:
 *
 *	  DATE		 FR	    DESCRIPTION
 *     -----------     ------- ----------------------------------------------
 *	27-Oct-94		REL	Fortran bridge to C routine zvgrimfmt	
 *					for the Unix platform
 *				Original release.
 *=============================================================================
 */
#include  "xvmaininc.h"   
#include  "ftnbridge.h" 
#include  "vgrimcod.h"   
#include <string.h>

/*============================================================================
 *
 *	Fortran-Callable Version
 *
 *==========================================================================*/

void FTN_NAME2(vgrimfmt, VGRIMFMT) (int *imcode, char format[], int *scan_rate,
	short int spix[], short int npix[], int *ind, ZFORSTR_PARAM)
#if 0
int *imcode;	/* input image format code */
char format[];	/* ASCII image format, e.g. "IM2s" */
int *scan_rate;	/* number of minor frames expected (== scan rate) */
short int spix[],npix[];  /* starting pixel, number of pixels for each seg */
int  *ind;
#endif
{
	ZFORSTR_BLOCK
	char	c_format[6];  

	*ind=zvgrimfmt( *imcode, c_format, scan_rate, spix, npix );

	zsc2for( c_format, 5, format, &imcode, 5, 2, 1, ind);

}

/*============================================================================
 *
 *	C-Callable Version
 *
*==========================================================================*/
int
zvgrimfmt(imcode,format,scan_rate,spix,npix)
int imcode;	/* input image format code */
char *format;	/* ASCII image format, e.g. "IM2s" */
int *scan_rate;	/* number of minor frames expected (== scan rate) */
short int spix[],npix[];  /* starting pixel, number of pixels for each seg */

{
static struct imd {
	char imcode;
	char format[5];
	int scan_rate;
	short ss;		/* Starting sample of first segment of line */
	short ns;		/* Number of samples per telemetry frame    */
   } imdata[33] = {
/*	 0,"I3-G4", 1,   1, 800,	   decomitted in favor of IM2WP */
	 0,"IM-2W", 1,   1, 800,	/* IM2WP, PWS part of IM2W	*/
	 1,"     ", 0,   0,   0,	/* IM16 */
         2,"IM-S ", 5,  61, 338,
	 3,"IM-20",10,  29, 744,	/* 742 PIXELS FOR ODD LINES */
	 4,"IM-O ", 5,   1, 800,
	 5,"IM-21",10,  30, 372,	/* 370 PIXELS FOR ODD LINES */
	 6,"IM-Q ", 5,   1, 800,
	 7,"IM-22",10,   1, 400,	/* ODD LINES ONLY, ALT. PIXELS */
	 8,"IM-K ",10,   1, 800,
/*	 9,"IM-7 ", 3,   1, 272,   ------ commented out Oct. 10, 1989 ----*/
         9,"GS-4B", 1,   1, 800,    
	10,"IM-2X", 2,   1, 800,
/*	11,"IM-9 ", 3, 161, 160,   -----  commented out Oct. 10, 1989 ----*/
        11,"GS-4C", 1,   1, 800,
	12,"IM-2C", 5,   1, 800,
	13,"IM-23",10,  17, 384,	/* ODDS 17-783, EVES 18-780	*/
	14,"IM-24", 2, 101, 600,
	15,"GS-4 ", 1,   1, 800,
	16,"IM-2W", 1,   1, 800,
	17,"GS-2 ", 1,   1, 800,
	18,"     ", 0,   0,   0,		/* IM14? */
	19,"OC-3 ",10,365,   44,	/* ODDS 333-419. EVENS 334-420	*/
	20,"IM-12", 5, 201,  80,
	21,"IM-11", 5,   1, 160,
	22,"IM-10", 5, 136, 106,	/* Real-time imaging from PB-10 */
	23,"IM-26", 5,   2, 800,
	24,"IM-15", 1, 265, 272,
	25,"IM-25", 5, 101, 600,
/*	26,"IM-6 ", 1, 181, 440,	/* Decommited in favor of IM2D	*/
	26,"IM-2D", 5,  77, 324,	/* ODDS 77-723, EVENS 78-724	*/
	27,"IM-5 ", 2,   1, 440,
	28,"IM-4 ", 1,  97, 608,
	29,"IM-3 ", 1,   1, 800,
	30,"IM-2 ", 1,   1, 800,
	31,"IM-13",10,	 1,  80};

	int	imc;
	int	n;
	int	seg;
	int	sr;
	short	ss;
	short	ns;

    imc = imcode;
    if ( (imc < 0 ) || (imc > 31) )
	return (0);
    sr = imdata[imc].scan_rate;
    if (sr == 0)
	return(0);		/* skip if format error */
    strcpy( format, imdata[imc].format);
    *scan_rate = sr;
    ss = imdata[imc].ss;
    ns = imdata[imc].ns;
    for (seg=0; seg<sr; seg++)
    {	spix[seg] = ss;
	npix[seg] = ns;
	/*
	 *--------------------------------------------------------------------
	 * IM20,21 AND 22 ARE SPECIAL SINCE THERE IS REALY ONE SEGMENT OF
	 * IMAGING LINE EVEN THOUGH THE RATE IS 10:1 THEREFORE THE STARTING
	 * SAMPLE FOR ALL SEGMENTS IS THE SAME
	 *--------------------------------------------------------------------
	 */
	if ((imc == IM20) || (imc == IM21) || (imc == IM22))
	    continue;
	else
	    ss += ns;
    }
    if (imc == IM5)
	npix[1] = 360;
/* 						   */
/* ---- IM7 does NOT exist anymore   Oct. 10, 1989 */
/*   else			*/
/*	if (imc == IM7)         */
/*	    npix[2] = 256;      */
    return(1);
}
