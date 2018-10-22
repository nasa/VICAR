
/*************************************************************

 VERSION 5 16 95

 GMASK - Primitive routines to create a mask.  Current features include :

		1) Straight line vectors of any orientation
		2) Character strings in block or Hershey fonts (TXTSUBS)
		3) Gray scale boxes
                4) Continuous shading and discrete step gray scale 
                5) PDS or VICAR format image data
                6) Tick marks
                7) Computation of histograms
	        8) Display of histograms
		9) Color or black/white mode
	       10) Zooms or reductions of image data
	       11) Accomodation of halfword image data
	       12) Buffer data 
	       13) Filled and hollow ellipses
	       14) Automatic mask centering or border production
	       15) Rays without arrows

 Author :			Al Wong
 System :			Vax 8600 VMS 5.2 OS
 Date Written :			01/21/86

 Last revised :			05/16/95
 Cognizant Programmer : 	Florance Moss
 Revisions :

 22-JUL-1986 	AXW, MAG	General mask building subroutines.
 17-OCT-1986 	AXW		Upgraded.
  4-DEC-1986 	MAG		Added continuous fonts.
 10-MAR-1987 	MAG		Corrected a routine to return the 
				correct status value.
 30-APR-1987 	MAG 		Added the selectable interface (FORTRAN or C).
  1-JUL-1987 	MAG		Modified mask product ID and time fields.
 14-DEC-1987 	MAG 		SORBIT changed to ORBIT FR#30448; use MAXNS 
				instead of NS FR#30447; remove straight-orbit 
				error FR#30428.
 23-MAY-1988 	KKA		Implemented new fonts.
 23-MAY-1988 	LIB		Changed identification to say Subroutine, 
				changed Realtime lib to say Run Time Lib, 
				fixed cleanup section.
  3-AUG-1988 	KKA		Added help documentation to file, FR #33226;
				readjusted fonts.
  3-NOV-1988 	FFM		Make changes to give better support to 
				adaptive stretch.
  7-APR-1989 	FFM		Added new routine XASTRINGLENGTH, added the 
				capability to smear Hershey font, modified 
				help and test file.
 12-JUL-1989 	FFM		Increase the maximum number of lines and 
				samples for the output mask from 9000 to 20000.
 25-JUL-1989 	FFM		Implemented new routines XADEFINESCALE,
				XACLEARSCALE, XASCALE.
 28-AUG-1989 	FFM		Added annotation for scale routine.
 13-SEP-1989 	FFM		Use unit nummber instead of instance number 
				in subroutines XASTOREIMAGE, XASTOREHISTOGRAM.
 22-SEP-1989 	FFM		Fix sample offset by one.
 29-SEP-1989 	FFM		Modify annotation routine to work w/ block font.
 27-NOV-1989 	FFM		Change default character height of block font 
				to be 5, enlarge limits of MAXBOX and 
				MAXSTRING size, plus other changes.
 28-NOV-1989 	FFM		Fix minor problem with gen #20.
  5-DEC-1989 	FFM		Added mirror image option to routine XACOPYMASK.
  6-DEC-1989 	FFM		Modified test file to include the test case of 
				mirror image; removed LOG file from COM file, 
				fixed NOSTRETCH option of XASTOREIMAGE.
 23-FEB-1990 	KKA		Deleted definition of MAX and MIN functions to 
				fix linking problems FR 52783. 
 28-FEB-1990 	JFM		Color mode and zoom feature available, modulus
				and justification provided for scale annotation,
				static arrays now linked lists.
  5-MAR-1990 	JFM		New color option, zoom capability, character 
				spacing adjustment, for strings, modulus and 
				justification for defining scales, array bound 
				made larger plus other changes.
 12-MAR-1990 	JFM		Correction to string printing of vertically 
				oriented text (+/- 90 degrees).
 13-APR-1990 	JFM		This version correctd the vertical display of 
				XASTOREGRAY, enables odd replication zooms and 
				corrects the limits of the zoom and store 
				image displays.
 19-APR-1990 	JFM		Font placement for XADEFINESCALE, in the 
				horizontal-bottom case is no longer dependent 
				on annotation justification.
  1-MAY-1990 	JFM		Addition of XACALCULATEHIST, a new XAZOOMIMAGE,
				a new XASTOREHISTROGRAM, halfword data 
				capabilities (see users).
  2-MAY-1990 	JFM		Adaptive stretch revised; problem encountered 
				with XACALCULATEHIST closing input files after 
				XXAOPENINPUTFILES called.
 15-MAY-1990 	RGD		Fixed for VMS 5.2 compile.
 18-MAY-1990 	JFM		A driver program can now produce more than one 
				output mask by calling XACOPYMASK more than 
				once; VMS 5.2 upgrade revisions.
  4-JUN-1990 	JFM		XACALCULATEHIST revised to be compatible with 
				HISCALE, unit test updated.
 19-JUL-1990 	JFM		XACALCULATEHIST corrected for excluded DN 
				values; automatic mask sizing based on stored 
				primitives corrected; XASTOREHISTOGRAM revised 
				for bin filling in stretched histogram case.
 24-JUL-1990 	JFM		XASTOREGRAY vertical case corrected; previously
				width had been improperly defined.
  9-AUG-1990 	JFM		modulus calculation now performed on 
				START_VALUE of annotation struct when modulus 
				is specified, FR 61189.
 26-AUG-1990 	JFM		XACALCULATEHIST revised to include negative 
				halfword values in its calculation of halfword 
				image histograms.
  8-OCT-1990 	JFM		Modified internal routines to exclude 
				user-defined values from stretch calculations; 
				linked list items dropped & freed to reduce 
				memory usage; test revised; corrected string 
				length calculations in XXACALCMAXLINESSAMPLES, 
				FRs 63882, 64513.
 31-OCT-1990 	JFM		XASTOREGRAY revised to generate 16-step gray 
				wedge; test file, user and design documents 
				revised, FR 39521.
 19-NOV-1990 	JFM		XASTORELLIPSE routine added to draw ellipses 
				and circles; xastoreimage, xazoomimage to keep 
				inputs open if already open, 
				FRs 64578, 64645, 64541.
  8-DEC-1990 	JFM		XAOUTSIZE routine added to provide file 
				centering through border production, FR 64697. 
 22-JAN-1991 	JFM		Inverse linear stretch capability added to 
				XACALCULATEHIST, XASTOREIMAGE and XAZOOMIMAGE; 
				XAOUTSIZE & XAINITIALIZE revised, FR 64662.
 29-JAN-1991 	JFM		Removed static array AsciiSet in 
				XXACOPYHERSHEYSTRING; replaced with dynamic 
				allocation using Deen's XXAMALLOC, FR 64699.
 21-FEB-1991 	JFM 		Global variable *AsciiSet(type char) redeclared 
				as globaldef variable, FR 64692.
 18-MAR-1991 	JFM		Added new feature to XASTOREVECTOR to draw 
				vectors at various slope, XASTOREGRAY, FR 66515.
  3-MAY-1991 	JFM		Hershey font support expanded to produce any 
				size characters, XASTOREHISTOGRAM bin interp 
				made optional, FR 64693.
  6-MAY-1991 	JFM		Test file revised, vectoray program call 
				removed, FR 64695.
 15-JUL-1991 	JFM		Added conditional test for character values<=0 
				in string sent to XASTORESTRING, FR 68871.
 19-JUL-1991 	JFM		Additional condition added to error checking 
				in XASTORESTRING for null strings; test PDF 
				revised accordingly, FR 68871.
 05-JUL-1992	JFM		Hershey font handling revised to speed execution
				by making memory allocations for strings in 
				Hershey fonts on a per-object basis instead of
				a per-object-per-line-written basis.  
 				Also, check added in XASTORESTRING to prevent
				negative starting line value in StringType
				structure when Hershey font characters are 
				smeared for added thickness, FR 70936.
				PCA added as a test option for program TESTBW.C.
 11-NOV-1992	JFM		XASTORERAY and XASTOREVECTOR revised to draw
				at any angle with square ends. FR 76813.
 25-MAR-1993	FFM		Modified routines XXADRAWANNOTATION_VR('T','V',
                                & 'C','V'), XXADRAWANNOTATION_VL('T','V', & 
                                'C', 'V', & 'C', 'L'), XXADRAWANNOTATION_HT
                                ('T','L', & 'B', 'R', & 'C','V', & 'C','L')
                                XXADRAWANNOTATION_HB('T','L', & 'B', 'R')so
                                that annotation will not attach to the tick 
                                marks and also will be centered correctly-
                                FR 81759, the test file is in dev:[ffm059.mo]
                                testgmask.com.
                                Modified routine XXACALCWIDTH to add '_', ',',
                                & ';' as tail characters, also created a new
                                font(font style 15) to interprete underscore
                                as underscore instead of <-. Bob will put the
                                new font in VRDI$FONTS:, Paul will modify the
                                VRDI documentation. - FR 81760, the test file
                                is in dev:[ffm059.mo]testgmaskstring.com
7-APR-1993      FFM             Port to Unix :
                                1. Removed FILL.MAR, use MEMSET in routine 
                                   FILLBYTE.
                                2. Removed xxaMalloc and xxaFree, use BigMalloc
                                   and BigFree in copyfont.c.
                                3. Changed names of XA routines to ZA.
                                4. Changed XV calls to ZV calls and added null 
                                   terminaters, also pass value instead of ref
                                   for input..
                                5. Removed FORTRAN interface in all za routines,
                                   removed include file gmaskif.c
                                6. Rename maskfont.c to maskfont.h
                                7. Made necessary changes to include files.
                                8. Replaced ZIA with MEMSET.
                                9. Replaced CCASE with ZCCASE.
                          ***  10. Convert optional arguments in routines 
                                   ZACOPYMASK, ZASTOREIMAGE, ZAZOOMIMAGE to
                                   required arguments due to NARGS is not
                                   portable.
                               11. Revised TSTGMASK.PDF, added TESTRAY in the
                                   TSTGMASK.PDF, corrected error in testray.pdf.

 The following changes made for UNIX compiler errors :

                                1. Changed GLOBALDEF INT to INT, because 
                                   GLOBALDEF is not portable.
                                2. Put all ENUM declarations at the beginning,
                                   corrected the case mismatch error.
                                3. Put CAST operators throught the code, 
                                   especially in routine ZADEFINESCALE, and 
                                   in front of MALLOC (e.g., 
                                   (struct Unittype *)malloc...).
                                4. Modify STRCMP parameter passing, E.G., pass 
                                   "HERSHEY" instead of &"HERSHEY").
                                5. Use &zero instead of &0.
                                6. Use MEMSET instead of int white = {255,...}.
                          ***   7. Pass address instead of values for background
                                   DNs of BW images, removed "if Dimension != 1
                                   " statements, modify test programs 
                                   accordingly.
                                8. Call ZTXT routines instead of TXT routines, 
                                   also pass vaule instead of ref for input
                                   in COPYFONT.C. 
                                9. Add error checks for all MALLOC calls.
                               10. Removed include file STDIO.H from all.
                               11. Call ZABEND, ZMABEND instead of ABEND & MABEND.
                               12. Call ZHISTAT2 & ZHISCALE instead of HISTAT2, 
                                   HISCALE in testbw.c, and testrgb.c.
                               13. ZACALCULATEHIST modified such that low 
				   saturation (low stretch limit) and high
				   saturation (high stretch limit) values are
				   returned without modification.
                               14. Added stringlength check for VHmb in routine
                                   ZASTOREHISTOGRAM to eliminate fill & inter-
                                   polate when it is not required.
                               15. Rewrite part of ZADEFINESCALE to eliminate 
                                   the use of increment of pointers.
                               16. Delete all VOID declarations of firstvector
                                   through firstunit. Added them in the global 
                                   declarations of boxptr to uptr.  
                               17. Put cast operator in front of temp2 in 
                                   routine  ZACALCULATEHIST.
                               18. Add 0.5 to temphist, temphist1, temphist2 
                                   to eliminate the round up discrepancy betwn
                                   2 machines in TESTRGB.C, because of this 
                                   change, there will be differences on 
                                   gmaskrgbc.dat on old gmask & ported gmask on
                                   VAX.
                               19. Changed i from integer to unsigned char for
                                   ztxttext routine. Added printf for all ZTXT
                                   routines status checking.(In copyfont.c).
                               20. Changed xa to za/xa in gmask.c & .rno. Re-
                                   moved XASETLANGIF in source & help. Modified
                                   .rno for ZACOPYMASK,ZASTOREIMAGE, & ZAZOOMIMAGE.
   
MAY 21 1993  FFM               changed char string[80] from global to local
                               and place it in ZASTORESTRING to avoid conflict
                               with RTL (so testdif would run on UNIX)
JUL  9 1993  FFM               Added FORTRAN bridges to all routines.
                               Changed XASTORESTRING's warning message when 
                               stline = 1, due to the insufficient memory 
                               allocation in SPRINTF which caused GLLMASK to 
                               fail.(FR 75789)

                               Fixed error in TESTRGBF.F
                               (index for do loop from 1,256 instead of 0 ,
                                255)
30-AUG-1993	JFM	      1. Made arithmetic of unsigned char values
				 consistent with ANSI C conventions on type
				 conversions. 
			      2. Added new functions of ZASTOREPDSIMAGE,
			         ZAZOOMPDSIMAGE, ZACALCULATEPDSHIST,
				 ZANAMEMASKFILE; expanded test programs to
				 include TZPDS.C, TPDS.C, TVICAR.C, TZVICAR.C.

16-May-1995	FFM	      1. Removed Free in the fortran bridges.
                                 (FR 87247)	
06-Jun-1997	AJR		Fixed error in zastoregray that compared the
				lines against the max samples instead of the
				max lines
10-Aug-1998     RRP           Made it ANSI C compitable so it can be ported
                              on hp.
*****************************************************************************
	INCLUDE FILES
*****************************************************************************/

#include <stdio.h>

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "pdsdef.h"
#include "pdsglob.h"
#include "label.h"
#include "gpds.h"
#include "zvproto.h"

#include <ctype.h>   
#include <math.h>
#include <stdlib.h>			
#include <string.h>

void zadeterminelimit();
void zastringlength();
void xxaIncrementAnnotation();
void xxatransferstring();
void xxatransferstring90();
void xxaapplystretch();
void xxacloseinputfiles();
void xxadrawvector();
void xxacopyzoom();
void xxacopyPDSzoom();
void xxacopybuffer();
void xxacalccolorbands();
void xxacalcmaxlinessamples();
void fillbyte();
void jsort();
void swap();

  		
/*****************************************************************************
	GLOBAL DEFINES
*****************************************************************************/
  	
#define OPEN 0x0001		/* File open? */
	
				/* Macro declaration	     */
#define Max(a,b) (a>b) ? a:b	/* Return maximum value      */
#define Min(a,b) (a<b) ? a:b    /* Return minimum value      */
  	
#define MAXIMAGENUMBER	30	/* Maximum # of lines */
#define MAXMASKSIZE	20000	/* Maximum # of lines */
#define MAXLINESIZE	20000	/* Maximum # of lines */
  	
#define HISTOGRAMTICKINCREMENT	16	/* Used for tick marks in histogram   */
#define HMAX		10 	/* Maximum number of histograms in multidisp. */
#define MAXDNVALUE	255	/* Maximum Dn value 			      */
#define MAXDNEXCLUDED   50	/* Maximum number of DN's to be excluded      */
  	
#define	TRUE		1	/* For "UserDefined" */
#define FALSE		0
  	
#define DEFAULT 	0	/* For font type */
#define HERSHEY 	1
#define DEFFONT		3
#define NOOVERRIDE	(-1)	/* CharSpacing default */
  	
#define ASCIISPACE	32	/* Ascii space value - zero character offset */
#define ASCIIVALUES	126-31	/* We do not want control characters 	     */
#define MAXSTRINGSIZE	1000	/* Maximum # of characters/string = 132+null */

#define XACHARWIDTH	5		/* Character font width 	*/
#define XACHARHEIGHT	5		/* Character font height 	*/
#define XACHARSPACING	1		/* Character spacing 		*/
#define ACTUAL_BLOCK_CHAR_HEIGHT 10	/* Character font height	*/

#define MAXFILENAME	200	/* Maximum length of PDS image file name     */
#define MAXFILES	250	/* Maximum number of open files		     */
  		
/*****************************************************************************
	GLOBAL VARIABLE DECLARATION
******************************************************************************/

int XALODNSTRETCH = (-1) ; 	/* This is needed by the MIDR mask */
int XAHIDNSTRETCH = (-1) ; 	/* for adaptive stretches 	   */

int DETERMINELIMIT = 0 ; 	/* Flag to tell whether routine      */
				/* XADETERMINELIMIT is called or not */
  	
int XAFONTTYPE   = DEFAULT ;	/* To tell whether to use the regular */
				/* default font or the Hershey fonts  */
int XAFONTNUMBER = DEFFONT ;	/* For Hershey fonts 		      */
int XAFONTANGLE  = 0 ;		/* Angle of fonts		      */
  	
static unsigned char	**Mask;		/* Output mask line buffer 	*/

					/* VICAR IMAGE HANDLING VARS    */
static int 	unitnumber[100];	/* Array of VICAR image unit #s */
static int 	unitcount;		/* Counter of VICAR image files */

					/* PDS IMAGE FILE HANDLING VARS */
static int 	PDSimagecount;		/* Counter of PDS image files	*/		
struct PDSfiles	{			/* Array of structures for      */
					/* pertinent file information   */
	char 	file[MAXFILENAME+1];	/* File specification name	*/
	FILE 	*fp;			/* File pointer			*/
	int  	file_open_flag;		/* File open status		*/
	int	bytes;			/* Number of bytes per pixel	*/
	int	lines;			/* Number of lines in object	*/
	int	samples;		/* Number of samples in object  */
	int	bands;			/* Number of input bands	*/
	int	record_bytes;		/* Number of bytes per record   */
	int	start_record;		/* Starting record of object    */

}	pds_image[MAXFILES];
					/* REDIRECTION OF OUTPUT VARS   */
static char	mask_file_name[MAXFILENAME]; /* File name of output     */
					/* mask set by ZANAMEMASKFILE   */
static int	redirect_mask_output;	/* Flag to indicate if VICAR    */
					/* pdf "OUT" parameter is to be */
					/* overridden with a user       */
					/* file name by ZANAMEMASKFILE  */

static int 	OutputInstance = 0;	/* Instance constant for output unit */
static int	Dimension = 1;		/* Dimension of output   	*/
static int	d;			/* Current band number   	*/

static int	UserMaxLines = 0 ;	/* User defined maximums 	*/
static int	UserMaxSamples;         
static int	MaskSizeLines = 0;	
static int	MaskSizeSamples = 0;	
static int	UserDefined ;

static int	BackgroundDn[] = {0, 0, 0} ;	/* Background Dn value 	*/
static int	BorderDn[] = {0, 0, 0} ;	/* Border Dn value 	*/
static int	CharSpacing = NOOVERRIDE;	/* Default CharSpacing 	*/
static char	Filename_Extension[50][20];	/* Default CharSpacing 	*/

static int	minboxline;	/* Minimum line values for various 	*/
static int	minbufline;	/* primitives in the linked lists  	*/
static int	minellipseline;	
static int	minhistline;
static int	minimageline;
static int	minPDSimageline;
static int	minscaleline;
static int	minstringline;
static int	mintickline;
static int	minvectorline;
static int	minzoomline;
static int	minPDSzoomline;

static float	SCALE32K = (255.0/32767.0);	/* Scaling of unstretched   */
static float	SCALE64K = (255.0/65535.0);	/* and stretched halfword   */
						/* data to byte data        */

  				/* BLOCK Character fonts      		    */
static int Font[ ASCIIVALUES ][ ACTUAL_BLOCK_CHAR_HEIGHT ][ XACHARWIDTH ] =
{
#include "maskfonts.h"		/* ascii fonts are stored here		    */
} ;
  		
/*******************************************************************************
	STRUCTURE DECLARATION			       
********************************************************************************/
							/* C ENUMERATIONS 	*/
enum StretchType { Fixed, Adaptive, NoStretch };	/* Image stretch type	*/
enum DispType { Minimum, Blend, None }; 	/* Histogram multidisplay type 	*/
enum DirType { Hori, Vert, Klinein };			/* Direction type	*/
enum ZoomType { Interpolation, Replication, NoZoom };
	 	
/*****************************/
/* Box Definition 	     */
/*****************************/
struct	BoxType			/* Box description vector */
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int Width ;
 	   int Length ;
 	   int Dn[3] ;
 	   struct BoxType *next;
 	   struct BoxType *previous;
 	};
 	
/*********************/
/* Buffer definition */
/*********************/
struct BufType
 	{
 	   unsigned char *buffer;	/* Input Buffer 		*/
	   int StartLine ;		/* Starting line in buffer 	*/
 	   int EndLine ;		/* Ending line 			*/
 	   int StartSample ;		/* Starting sample 		*/
 	   int EndSample ;		/* Ending sample		*/
 	   int OutStartLine ;		/* Starting line in mask	*/
 	   int OutStartSample ;		/* Starting sample in mask	*/
 	   int MagFactor ;     		/* Magnification/reduction factor     */

	   int Band;			/* Number of samples in a band 	*/
	   int NumLines;		/* Number of lines in buffer 	*/
	   int NumSamp;			/* Number of samples per line   */
	   int OutSamp;			/* OutStartSample - 1	  	*/
	   int Samp;			/* StartSample - 1	  	*/
 	   float MulFactor ; 		/* Multiplicative Mag. factor  	      */
	   char Display[10] ;		/* Indicator of color or BW display   */
 	   unsigned char **Input;	/* Buffer for zooming calculation     */

 	   struct BufType *next;
 	   struct BufType *previous;
 	};

 	
/*****************************/
/* Ellipse Definition 	     */
/*****************************/
struct	EllipseType			/* Circle description vector */
 	{
 	   int CenterLine ;
 	   int CenterSample ;
 	   int Radius1 ;
 	   int Radius2 ;
 	   int Width ;
	   int **Edge;
	   float **Smooth;
 	   int Dn[3] ;
 	   struct EllipseType *next;
 	   struct EllipseType *previous;
 	};
 	
/************************/
/* Histogram definition */
/************************/
struct HistType
 	{
 	   int HistStLine ;		/* Histogram starting line            */
 	   int HistStSample ;		/* Histogram starting sample          */
 	   int HistEnLine ;		/* Histogram ending line              */
 	   int HistEnSample ;		/* Histogram ending sample            */
	   float SampleFraction ; 	/* Fraction percentage to be sampled  */
	   float SaturationPercentage ; /* Saturation percentage for adapt. s */
 	   int *EXC;			/* Excluded DN for adaptive stretch   */
 	   int NUM ;			/* Number of elements to be excluded  */
	   float HistScale;		/* Scale factor for DN dimension      */
	   float PixScale;		/* Scale factor for Freq. dimension   */
 	   int Dn[3] ;			/* Dn value or Dn array for color     */
	   int ColorBands ; 		/* Number of color bands 	      */
	   enum StretchType Stretch ;	/* Type of stretch used		      */
	   enum DirType Direction ;	/* Vertical or Horizontal histogram?  */
	   enum DispType Display ; 	/* Minimum or blending histograms     */
	   unsigned int Histogram[256];	/* Pointer to histogram counter array */
	   unsigned int MaxPixelValue ;	/* Maximum value in histogram array   */
	   struct HistType *HistSet[HMAX];/* Array of pointers to histograms  */
	   unsigned char ***Blend;	/* Resultant color of three histogram */
	   struct HistType *Ptr[HMAX][MAXDNVALUE+1]; 
 	   struct HistType *next;
 	   struct HistType *previous;
 	};
 	
/*************************/
/* Image data definition */
/*************************/
struct ImageType
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int EndSample ;
 	   int OutStartLine ;
 	   int OutStartSample ;
 	   int InputUnitNumber ;
 	
 	   int InputLines ;    /* These fields are derived during file I/O */
 	   int InputSamples ;
	   int ColorBands ;    
	   int InputBands ;    /* Number of bands in input image      	*/
 	   int OutBand ;       /* Band of output image                	*/
	   int NumDNs;         /* Number of Dn values: 256 or 32768   	*/
 	
 	   enum StretchType Stretch ;	/* For the stretching the image */
 	   float SampleFraction ;
 	   float SaturationPercentage ;
 	   int *LUT;		/* Lookup table for stretch 		*/
 	   int *EXC;		/* Excluded DN for adaptive stretch 	*/
 	   int NUM ;		/* Number of elements to be excluded 	*/
 	   struct ImageType *next;
 	   struct ImageType *previous;
 	};
	
/*****************************/
/* PDS Image data definition */
/*****************************/
struct PDSImageType
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int EndSample ;
 	   int OutStartLine ;
 	   int OutStartSample ;
 	   char InputFileName[MAXFILENAME+1] ;
 	   FILE *fp;
 	
 	   int InputLines ;    /* These fields are derived during file I/O */
 	   int InputSamples ;
	   int ColorBands ;    
	   int InputBands ;    /* Number of bands in input image      	*/
 	   int OutBand ;       /* Band of output image                	*/
	   int NumDNs;         /* Number of Dn values: 256 or 32768   	*/
 
	   int Bytes;			/* Number of bytes per pixel	*/
	   int RecordBytes;		/* Number of bytes per record	*/
	   int ImageStartRecord;	/* Image object starting record */

 	   enum StretchType Stretch ;	/* For the stretching the image */
 	   float SampleFraction ;
 	   float SaturationPercentage ;
 	   int *LUT;		/* Lookup table for stretch 		*/
 	   int *EXC;		/* Excluded DN for adaptive stretch 	*/
 	   int NUM ;		/* Number of elements to be excluded 	*/
 	   struct PDSImageType *next;
 	   struct PDSImageType *previous;
 	};
 	
/************************/
/* Scale  definition    */
/************************/
struct ScaleType
 	{
 	   unsigned char Type ;
 	   int End_Cap_Length ;
 	   int Tick_Length[50] ;
 	   int End_Cap_Width ;
 	   int Tick_Width[50] ;
 	   int Center_Line_Width ;
 	   double Tick_Increment[50][2] ;
 	   int Array_Length ;

 	   unsigned char Annotation_Position[50] ;
 	   double Annotation_Start_Value[50] ;
 	   double Annotation_Increment[50] ;
 	   double Annotation_Modulus[50] ;
 	   int Annotation_Significant_Digits[50] ;
 	   int Annotation_Size[50] ;
 	   unsigned char Annotation_Orientation[50] ;
 	   unsigned char Annotation_Justification[50] ;
	} static ScaleVector[3];

/*****************************/
/* Annotation  definition    */
/*****************************/
struct AnnotationType
 	{
 	   unsigned char Annotation_Position ;
 	   double Annotation_Start_Value ;
 	   double Annotation_Increment ;
 	   double Annotation_Modulus ;
 	   int Annotation_Significant_Digits ;
 	   int Annotation_Size ;
 	   unsigned char Annotation_Orientation ;
 	   unsigned char Annotation_Justification ;
	} static Annotation[50];
  	
/****************************/
/* String Vector Definition */
/****************************/
struct	StringType		/* String definition */
  	{
  	   int StartLine ;
  	   int StartSample ;
  	   int Size ;
  	   int Dn[3] ;
  	   char CharString[ MAXSTRINGSIZE ] ;
  	   int CharHeight ;
  	   int CharSpac;
  	   int CharWidth ;
  	
  	   int FontType ;
  	   int FontThickness ;
  	   int FontNum ;
  	   int Angle ;
	   int LineScale;
	   int LastLine;

	   int *usedcharindex;		/* Hershey font specific 	*/
	   char *AsciiSet;		/* Hershey font specific	*/
  	   int bufheight;		/* Hershey font specific	*/
  	   int bufwidth;		/* Hershey font specific	*/
  	   int bufarea;			/* Hershey font specific	*/

  	   struct StringType *next;
  	   struct StringType *previous;
  	};

/************************/
/* Tick mark definition */
/************************/
struct TickType
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int EndSample ;
 	   int Width ;
 	   int Length ;
 	   int Spacing ;
 	   int Dn[3] ;
 	   int Vertical_Tick_End_Line ;
	   int LastLine; 
	   struct TickType *next;
 	   struct TickType *previous;
 	};

/***************************/
/* Unit Number Structure   */
/***************************/
struct UnitType		
  	{
  	   int UnitNumber ;		/* Unit number of input file 	  */
  	   int BandNumber ;		/* Associated output band number  */
  	   struct UnitType *next;
	};

/***********************************/
/* Straight Line Vector Definition */
/***********************************/
/*
		Straight line orientations :
  	
  			1) Horizontal	( StartLine = EndLine      )
  			2) Vertical	( StartSample = EndSample  )
			3) Klinein      ( Nonzero and finite slope )
*/
struct VectorType		/* Straight line vector definition */
  	{
  	   enum DirType Direction ;
  	   int StartLine ;
  	   int StartSample ;
  	   int EndLine ;
  	   int EndSample ;
  	   int Length ;
  	   int Width ;
  	   int Dn[3] ;
	   int **Edge ;
	   float **Smooth ;
  	   struct VectorType *next;
  	   struct VectorType *previous;
  	};

/******************************/
/* Zoom Image data definition */
/******************************/
struct ZImageType
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int EndSample ;
 	   int OutStartLine ;
 	   int OutStartSample ;
 	   int InputUnitNumber ;
 	
 	   int InputLines ;    /* These fields are derived during file I/O */
 	   int InputSamples ;
	   int ColorBands ;    
	   int InputBands ;    /* Number of bands in input image      */
 	   int OutBand ;       /* Band of output image                */
	   int NumDNs;         /* Number of Dn values: 256 or 32768   */
 	
 	   int MagFactor ;     		/* Magnification or reduction factor  */
 	   float MulFactor ; 		/* Multiplicative Mag. factor  	      */
 	   enum ZoomType Ztype ;	/* Type of zoom to be used            */

 	   enum StretchType Stretch ;	/* For the stretching the image */
 	   float SampleFraction ;
 	   float SaturationPercentage ;
 	   int *LUT;	/* Lookup table for stretch */
 	   int *EXC;	/* Excluded DN for adaptive stretch */
 	   int NUM ;	/* Number of elements to be excluded */
 	
 	   unsigned char **Input;
 	   struct ZImageType *next;
 	   struct ZImageType *previous;
 	};

/**********************************/
/* Zoom PDS Image data definition */
/**********************************/
struct PDSZImageType
 	{
 	   int StartLine ;
 	   int EndLine ;
 	   int StartSample ;
 	   int EndSample ;
 	   int OutStartLine ;
 	   int OutStartSample ;
 	   char InputFileName[MAXFILENAME+1] ;
 	   FILE *fp;
 	
 	   int InputLines ;    /* These fields are derived during file I/O */
 	   int InputSamples ;
	   int ColorBands ;    
	   int InputBands ;    /* Number of bands in input image      */
 	   int OutBand ;       /* Band of output image                */
	   int NumDNs;         /* Number of Dn values: 256 or 32768   */
 	
 	   int MagFactor ;     		/* Magnification or reduction factor  */
 	   float MulFactor ; 		/* Multiplicative Mag. factor  	      */
 	   enum ZoomType Ztype ;	/* Type of zoom to be used            */

	   int Bytes;			/* Number of bytes per pixel	*/
	   int RecordBytes;		/* Number of bytes per record	*/
	   int ImageStartRecord;	/* Image object starting record */

 	   enum StretchType Stretch ;	/* For the stretching the image */
 	   float SampleFraction ;
 	   float SaturationPercentage ;
 	   int *LUT;	/* Lookup table for stretch */
 	   int *EXC;	/* Excluded DN for adaptive stretch */
 	   int NUM ;	/* Number of elements to be excluded */
 	
 	   unsigned char **Input;
 	   struct PDSZImageType *next;
 	   struct PDSZImageType *previous;
 	};

/****************************************************/
/* Pointers to structures during allocation         */
/****************************************************/

struct BoxType 		*firstbox,     *boxptr;
struct BufType 		*firstbuf,     *bufptr;
struct EllipseType 	*firstellipse, *ellipseptr;
struct HistType 	*firsthist,    *histptr;
struct ImageType	*firstimage,   *imageptr;
struct ZImageType 	*firstzoom,    *zoomptr;
struct PDSImageType	*firstPDSimage,*PDSimageptr;
struct PDSZImageType 	*firstPDSzoom, *PDSzoomptr;
struct StringType 	*firststring,  *stringptr;
struct TickType 	*firsttick,    *tickptr;
struct VectorType 	*firstvector,  *vectorptr;
struct UnitType 	*firstunit,    *uptr;


#include "copyfont.c"			/* Hershey font routines 	*/
/******************************************************************************

GMASK routines currently include:

       	ZA/XABAND		(v. 02.22.90)
	ZA/XACALCULATEHIST	(v. 04.30.90)
	ZA/XACLEARSCALE		
	ZA/XACOPYMASK
	ZA/XADEFINESCALE
	ZA/XADETERMINELIMIT
	ZA/XAINITIALIZE
	ZA/XANAMEMASKFILE	(v. 08.30.93)
	ZA/XAOUTSIZE		(v. 11.28.90)
	ZA/XASCALE
	ZA/XASETCHARSPACING	(v. 02.22.90)
	ZA/XASETDIM		(v. 02.22.90)
	ZA/XASETFONT
	ZA/XASETFONTANGLE
	ZA/XASTOREBOX
	ZA/XASTOREBUFFER	(v. 04.30.90)
	ZA/XASTORELLIPSE	(v. 11.19.90)
	ZA/XASTOREGRAY
	ZA/XASTOREHISTOGRAM	(v. 04.30.90)
	ZA/XASTOREIMAGE
	ZA/XASTOREPDSIMAGE	(v. 08.30.93)
	ZA/XASTORERAY		(v. 03.15.91)
	ZA/XASTORESTRING
	ZA/XASTORETICK
	ZA/XASTOREVECTOR
	ZA/XASTRINGLENGTH
	ZA/XAZOOMIMAGE		(v. 04.30.90)
	ZA/XAZOOMPDSIMAGE	(v. 08.30,93)
	
******************************************************************************/
  		
/****************************************************************************/
/*         Fortran-Callable Version					    */
/****************************************************************************/
        void FTN_NAME2(xaband, XABAND) (unit, band)
        int *unit, *band;
        {
        zaband( *unit, *band);
        }
/****************************************************************************/
/*         C-Callable Version						    */
/****************************************************************************/

 	/***********************************/
 	/* Set output band for unit number */
 	/***********************************/
 	zaband( unit, band )
 	int unit, band;
	{
           struct UnitType *newblock;
 	   struct UnitType *ptr;
	   int UnitFound;

	   if( band <= 0 )
		{
		zvmessage("Negative band value; za/xaband not performed."," ");
		return(-1);
		}

	   ptr = firstunit;	/* Loop through all unit numbers to ensure */

           UnitFound = FALSE;	/* unit number has not already been used.  */
           while ( ptr != NULL && !UnitFound )
		{
		if ( ptr->UnitNumber == unit )
			{
			ptr->BandNumber = band;		
			UnitFound = TRUE;
			}
		ptr = ptr->next;
		}

	   if( ptr == NULL )
		{
 	  	newblock = (struct UnitType *)malloc(sizeof (struct UnitType));
 	  	if( !newblock )
 			{
 			zvmessage( "Allocation error for vectors"," ");
 			zabend();
 			}
 	   	if( !uptr )
 			firstunit = newblock;
 	   	else
 			uptr->next = newblock;
 	   	newblock->next = NULL;
 	  	uptr = newblock;

		uptr->UnitNumber = unit;
		uptr->BandNumber = band;
		}
	  return( 0 );
	}	
   		
/****************************************************************************/
/*         Fortran-Callable Version					    */
/****************************************************************************/
	void FTN_NAME2(xacalculatehist, XACALCULATEHIST) ( int *Unit,
		int *SL, int *SS, int *EL,
		int *ES, int *LINC, int *SINC, 
		unsigned int histbuf[], unsigned int *maxfreq,
		int *numpoints, char *SFlag, float *LS, float *HS,
		int ExClude[], int *NUMEXC, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char *c_string;
        int length;

        zsfor2len(length, SFlag, &Unit,15, 11, 1, NUMEXC);
        c_string = (char *)calloc(1,(length+1));	
        zsfor2c(c_string, length, SFlag, &Unit, 15, 11, 1, NUMEXC); 
	zacalculatehist( *Unit, *SL, *SS, *EL, *ES, *LINC, *SINC, 
                         histbuf, maxfreq,numpoints, c_string[0],LS,
			 HS, ExClude, *NUMEXC );
        free (c_string);
        }
/****************************************************************************/
/*         C-Callable Version						    */
/****************************************************************************/

	/*********************/
	/* Compute histogram */ 
 	/*********************/
	/*
	
	Purpose:	A general purpose histogram computation routine.

	*/
	int zacalculatehist( Unit, SL, SS, EL, ES, LINC, SINC, histbuf, maxfreq,
			 numpoints, SFlag, LS, HS, ExClude, NUMEXC )
	char 	SFlag;			/* Stretch flag 		  */
	int	Unit,			/* Input image unit number 	  */
		SL,			/* Starting line in image  	  */
		EL,			/* Ending line in image      	  */
		SS,			/* Starting sample in image  	  */
		ES,			/* Ending sample in image         */
		LINC,			/* Line incrementation 		  */
		SINC,			/* Sample incrementation 	  */
		ExClude[],		/* DN  values to exclude 	  */
		NUMEXC,			/* Number of DN values excluded   */
		*numpoints;		/* Number of points in hist.      */
	float	*LS,			/* Lower sat. perc. or LO dn	  */
		*HS;			/* Upper sat. perc. or HI dn 	  */
	unsigned int	histbuf[];	/* Histogram buffer		  */
	unsigned int	*maxfreq;	/* Maximum Frequency of histogram */
	{
	float	Lo, Hi, temp1, temp2;
	float	scale,			/* Histogram bin scaling factor */
		scale2;			/* Histogram freq. scaling fac. */
	int	bytes,			/* Number of bytes per pixel    */
		flags,
		halfoffset,		/* Offset of index, half strtch */
		i,			/* Loop control variable 	*/
		index,			/* Bin number of stretched hist */
		IOStatus,		/* VICAR routine success status */
		line,			/* Loop control variable	*/
		numsamples,		/* Number of samples per line   */
		numlines,		/* Number of lines in image     */
		sample,			/* Loop control variable	*/
		size,			/* Number of elements in hist	*/
		tally,			/* Number of points to be excl. */
		x;			/* Loop control variable	*/
	unsigned int *TempHist;		/* Histogram buffer		*/
	short int *HalfInput;		/* Pointer to image line array  */
	unsigned char *ByteInput;	/* Pointer to image line array  */
        int dif;
   
	IOStatus = zvget( Unit,"FLAGS", &flags, NULL);
	if ( ( flags & OPEN ) == 0 )
		IOStatus = zvopen( Unit,"OPEN_ACT", "SA", "IO_ACT","SA", NULL);
	IOStatus = zvget( Unit, "NS", &numsamples, "NL", &numlines,
	                  "PIX_SIZE", &bytes, NULL);
	IOStatus= zvclose( Unit,NULL);

	/* If a negative value is passed as a parameter, return -1 as error   */
 	if ( Unit < 0 ||
	     SL < 0 || SL > numlines ||
 	     SS < 0 || SS > numsamples ||
 	     EL < 0 || 
 	     ES < 0 || ES > UserMaxSamples ||
	     LINC <= 0 || SINC <= 0 || 
	     LINC > (EL - SL + 1) || SINC > (ES - SS + 1) ||
	     ( bytes != 1 && bytes != 2 ) )
 		return(-1);

	*numpoints = 0;		/* Initialize variables */
	*maxfreq = 0;

	if( bytes == 1 )	/* BYTE data condition	*/
		{
		IOStatus = zvopen( Unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			           "U_FORMAT", "BYTE", NULL );
		size = 256;
		halfoffset = 0;
		temp1 = size - 1;
		
		/* Clear and allocate memory */
		memset(histbuf,0, 4*size);

		ByteInput = (unsigned char *)calloc
			    (numsamples,sizeof(unsigned char));
		if ( ByteInput == NULL )
			{
			zvmessage("MEMORY ALLOCATION ERROR"," ");
			zvmessage(" - ZA/XACALCULATEHIST aborted"," ");
			return( -2 );
			}
		for( line = SL; line <= EL; line += LINC )
			{
			IOStatus = zvread( Unit, ByteInput, "LINE", line, NULL );	
			for(sample=(SS-1);sample<ES;sample+=SINC,(*numpoints)++)
				histbuf[(size-1) & ByteInput[sample]]++;
			}
		free (ByteInput);	/* Free allocated memory 	*/
		}
	else				/* HALFWORD data condition 	*/
		{
		IOStatus = zvopen( Unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			           "U_FORMAT", "HALF", NULL );
		size = 65536;
		halfoffset = 32768;
		temp1 = halfoffset - 1;

		/* Clear histogram buffer */
		memset(histbuf,0, 4*size);

		HalfInput = (short int *)calloc(numsamples, sizeof(short int));
		if ( HalfInput == NULL )
			{
			zvmessage("MEMORY ALLOCATION ERROR"," ");
			zvmessage(" - ZA/XACALCULATEHIST aborted"," ");
			return( -2 );
			}
		for( line = SL; line <= EL; line += LINC )
			{
			IOStatus = zvread( Unit, HalfInput, "LINE", line, NULL );	
			for(sample=(SS-1);sample<ES;sample+=SINC,(*numpoints)++)
				histbuf[HalfInput[sample]+halfoffset]++;
			}
		free (HalfInput);	/* Free allocated memory */
		}

	for( x=0; x<NUMEXC; x++ )
		{
		*numpoints -= histbuf[ExClude[x]+halfoffset];
		histbuf[ExClude[x]+halfoffset] = 0;
		}
	if( (SFlag != 'n') && (SFlag != 'N'))	/* Stretch parameters? */
		{
	     	if( *LS < 0.0 || *LS > size-1 || *HS < 0.0 || *HS > size-1 )
			return(-1);

		if ( (SFlag == 'a') || (SFlag == 'A')) /* Determine DN limits */
			{
			tally = (*numpoints) * *LS;

			Lo = 0;
			for( i=0; i<size; i++ )
				if(( tally -= histbuf[ i ])<=0 )
					{
					Lo = i;
					break;
					}

			tally = (*numpoints) * *HS;
				
			Hi = size-1;
			for( i=size-1; i>=0; i-- )
				if(( tally -= histbuf[ i ])<=0 )
					{
					Hi = i;
					break;
					}
			}
		else	/* Fixed stretch condition */
			{
 	         	Lo = *LS + halfoffset  ;
 	         	Hi = *HS + halfoffset ;
			}

                dif = (int) (Hi - Lo);
		temp2 = (float)abs(dif) ;
		if ( temp2 == 0.0 )
			temp2 = 1.0;
		scale = temp1 / temp2 ; 

		/* Allocate memory for histogram */
		TempHist = (unsigned int *)calloc(size,sizeof(unsigned int));
		if ( TempHist == NULL )
			{
			zvmessage("MEMORY ALLOCATION ERROR"," ");
			zvmessage(" - ZA/XACALCULATEHIST aborted"," ");
			return( -2 );
			}

		if( Hi > Lo )
			{
			for( i=0; i<Lo; i++ )           /* Stretch histogram */
				TempHist[halfoffset] += histbuf[i];
			for( i=Lo; i<=Hi; i++ )	
				{
				index = (i-Lo)*scale + halfoffset + 0.5;
				TempHist[index] += histbuf[i] ;
				}
			for( i=Hi+1; i<size; i++ )
				TempHist[size-1] += histbuf[i];
			}
		else		/* Calculate inverse stretch histogram   */
			{
			for( i=0; i<Hi; i++ )
				TempHist[size-1] += histbuf[i];
			for( i=Hi; i<=Lo; i++ )	
				{
				index = (Lo - i)*scale + halfoffset + 0.5;
				TempHist[index] += histbuf[i] ;
				}
			for( i=Lo+1; i<size; i++ )  
				TempHist[halfoffset] += histbuf[i];
			}

		for( i=0; i<size; i++ )
			histbuf[i] = TempHist[i] ;

		free( TempHist );	/* Free allocated memory */
		}

	for( x=0; x<size; x++ )
		*maxfreq = Max( histbuf[x], *maxfreq );

	IOStatus = zvclose( Unit, NULL );	/* Close image file    */

	return( 0 );
	}
   		
/****************************************************************************/
/*         Fortran-Callable Version					    */
/****************************************************************************/
	void FTN_NAME2(xacalculatePDShist, XACALCULATEPDSHIST) ( char *File,
		int *SL, int *SS, int *EL, int *ES, int *LINC, int *SINC, 
		unsigned int histbuf[], unsigned int *maxfreq, int *numpoints,
		char *SFlag, float *LS, float *HS,
		int ExClude[], int *NUMEXC, ZFORSTR_PARAM)
	{
	ZFORSTR_BLOCK
        char *c1_string;
        char *c2_string;
        int length;

        zsfor2len(length, File, &File,15, 1, 1, NUMEXC);
        c1_string = (char *)calloc(1,(length+1));	
        zsfor2c(c1_string, length, File, &File, 15, 1, 1, NUMEXC); 

        zsfor2len(length, SFlag, &File,15, 11, 2, NUMEXC);
        c2_string = (char *)calloc(1,(length+1));	
        zsfor2c(c2_string, length, SFlag, &File, 15, 11, 2, NUMEXC); 

	zacalculatePDShist( c1_string, *SL, *SS, *EL, *ES, *LINC, *SINC, 
                         histbuf, maxfreq,numpoints, c2_string[0],LS,
			 HS, ExClude, *NUMEXC );
        free (c1_string);
        free (c2_string);
        }
/***************************************************************************
         C-Callable Version
****************************************************************************/

	/*********************/
	/* Compute histogram */ 
 	/*********************/
	/*
	
	Purpose:	A general purpose histogram computation routine.

	*/
	int zacalculatePDShist( File, SL, SS, EL, ES, LINC, SINC, histbuf, maxfreq,
			 numpoints, SFlag, LS, HS, ExClude, NUMEXC )
	char 	*File;			/* File specification name 	  */
	char 	SFlag;			/* Stretch flag 		  */
	int	SL,			/* Starting line in image  	  */
		EL,			/* Ending line in image      	  */
		SS,			/* Starting sample in image  	  */
		ES,			/* Ending sample in image         */
		LINC,			/* Line incrementation 		  */
		SINC,			/* Sample incrementation 	  */
		ExClude[],		/* DN  values to exclude 	  */
		NUMEXC,			/* Number of DN values excluded   */
		*numpoints;		/* Number of points in hist.      */
	float	*LS,			/* Lower sat. perc. or LO dn	  */
		*HS;			/* Upper sat. perc. or HI dn 	  */
	unsigned int	histbuf[];	/* Histogram buffer		  */
	unsigned int	*maxfreq;	/* Maximum Frequency of histogram */
	{

	char 	string[MAXFILENAME+1];

	float	Lo, Hi, temp1, temp2;
	float	scale,			/* Histogram bin scaling factor */
		scale2;			/* Histogram freq. scaling fac. */

	int	bands,			/* Number of bands in PDS image */
		bytes,			/* Number of bytes per pixel    */
	        dif,
		file_index,		/* PDS image file index		*/
		file_open,		/* PDS image open flag		*/
		flags,
		halfoffset,		/* Offset of index, half strtch */
		i,			/* Loop control variable 	*/
		image_start_record,	/* Starting record of image obj */
		index,			/* Bin number of stretched hist */
		IOStatus,		/* VICAR routine success status */
		line,			/* Loop control variable	*/
		numsamples,		/* Number of samples per line   */
		numlines,		/* Number of lines in image     */
		record_bytes,		/* Number of bytes per record	*/
		sample,			/* Loop control variable	*/
		size,			/* Number of elements in hist	*/
		status,			/* File IO status flag		*/
		tally,			/* Number of points to be excl. */
		x;			/* Loop control variable	*/
	unsigned int 	*TempHist;	/* Histogram buffer		*/
	short int 	*InputLine;	/* Pointer to image line array  */
   
	strcpy(string,File);

	/* Get PDS image statistics */

	IOStatus = get_pds_image_stats( string, &numlines, &numsamples, &bands,
			&bytes, &record_bytes, &image_start_record );
	if ( IOStatus < 0 )
		return -1;
	
	/* If a negative value is passed as a parameter, return -1 as error   */

	if ( strlen( File ) > MAXFILENAME ||
	     strlen( File ) <= 0 ||
 	     SL < 0 || SL > numlines ||
 	     SS < 0 || SS > numsamples ||
 	     EL < 0 || 
 	     ES < 0 || ES > numsamples ||
	     record_bytes < 0 || image_start_record < 0 ||
	     LINC <= 0 || SINC <= 0 || 
	     LINC > (EL - SL + 1) || SINC > (ES - SS + 1) ||
	     ( bytes != 1 && bytes != 2 ) )
 		return(-1);

	*numpoints = 0;		/* Initialize variables */
	*maxfreq = 0;

	/* Check to see if PDS file is already open; if not, open the file */

	file_index = get_pds_image_index(string);
	if ( file_index < 0 )
		{
		file_index = PDSimagecount;
		strcpy(pds_image[file_index].file,string);
		pds_image[file_index].file_open_flag = FALSE;
		PDSimagecount++;
		}

	if ( pds_image[file_index].file_open_flag == FALSE )
		{
		pds_image[file_index].fp = fopen( string, "r" );	
		if ( pds_image[file_index].fp == NULL )
			return -1;
		pds_image[file_index].file_open_flag = TRUE;
		}

	pds_image[file_index].bytes = bytes;
	pds_image[file_index].record_bytes = record_bytes;
	pds_image[file_index].start_record = image_start_record;
	pds_image[file_index].lines = numlines;
	pds_image[file_index].samples = numsamples;
		
	InputLine = (short int *)calloc(numsamples,sizeof(short int));
	if ( InputLine == NULL )
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" 	- za/xacalculatePDShist aborted"," ");
		return -2;
		}

	if( bytes == 1 )	/* BYTE data condition	*/
		{
		size = 256;
		halfoffset = 0;
		temp1 = size - 1;

		/* Clear histogram buffer */
		memset(histbuf,0, 4*size);

		for( line = SL; line <= EL; line += LINC )
			{			/* Read PDS image line	*/
			IOStatus = read_PDS_image_line( string, InputLine, 
					line, SS, numsamples, 1 );	
			if ( IOStatus < 0 )
				return -1;

			for(sample=0;sample<(ES-SS+1);sample+=SINC,(*numpoints)++)
				histbuf[(size-1) & InputLine[sample]]++;
			}
		}
	else				/* HALFWORD data condition 	*/
		{
		size = 65536;
		halfoffset = 32768;
		temp1 = halfoffset - 1;

		/* Clear histogram buffer */
		memset(histbuf,0, 4*size);

		for( line = SL; line <= EL; line += LINC )
			{			/* Read PDS image line 	*/
			IOStatus = read_PDS_image_line( string, InputLine, line,
					line, SS, numsamples, 1 );	
			if ( IOStatus < 0 )
				return -1;

			for(sample=0;sample<(ES-SS+1);sample+=SINC,(*numpoints)++)
				histbuf[InputLine[sample]+halfoffset]++;
			}
		}

	free (InputLine);	/* Free allocated memory 	*/

	for( x=0; x<NUMEXC; x++ )
		{
		*numpoints -= histbuf[ExClude[x]+halfoffset];
		histbuf[ExClude[x]+halfoffset] = 0;
		}

	if( (SFlag != 'n') && (SFlag != 'N'))	/* Stretch parameters? */
		{
	     	if( *LS < 0.0 || *LS > size-1 || *HS < 0.0 || *HS > size-1 )
			return(-1);

		if ( (SFlag == 'a') || (SFlag == 'A')) /* Determine DN limits */
			{
			tally = (*numpoints) * *LS;

			Lo = 0;
			for( i=0; i<size; i++ )
				if(( tally -= histbuf[ i ])<=0 )
					{
					Lo = i;
					break;
					}

			tally = (*numpoints) * *HS;
				
			Hi = size-1;
			for( i=size-1; i>=0; i-- )
				if(( tally -= histbuf[ i ])<=0 )
					{
					Hi = i;
					break;
					}
			}
		else	/* Fixed stretch condition */
			{
 	         	Lo = *LS + halfoffset  ;
 	         	Hi = *HS + halfoffset ;
			}

                dif = (int) (Hi - Lo);
		temp2 = (float)abs(dif) ;
		if ( temp2 == 0.0 )
			temp2 = 1.0;
		scale = temp1 / temp2 ; 

		/* Allocate memory for histogram */
		TempHist = (unsigned int *)calloc(size,sizeof(unsigned int));
		if ( TempHist == NULL )
			{
			zvmessage("MEMORY ALLOCATION ERROR"," ");
			zvmessage(" - za/xacalculatePDShist aborted"," ");
			return( -2 );
			}

		if( Hi > Lo )
			{
			for( i=0; i<Lo; i++ )           /* Stretch histogram */
				TempHist[halfoffset] += histbuf[i];
			for( i=Lo; i<=Hi; i++ )	
				{
				index = (i-Lo)*scale + halfoffset + 0.5;
				TempHist[index] += histbuf[i] ;
				}
			for( i=Hi+1; i<size; i++ )
				TempHist[size-1] += histbuf[i];
			}
		else		/* Calculate inverse stretch histogram   */
			{
			for( i=0; i<Hi; i++ )
				TempHist[size-1] += histbuf[i];
			for( i=Hi; i<=Lo; i++ )	
				{
				index = (Lo - i)*scale + halfoffset + 0.5;
				TempHist[index] += histbuf[i] ;
				}
			for( i=Lo+1; i<size; i++ )  
				TempHist[halfoffset] += histbuf[i];
			}

		for( i=0; i<size; i++ )
			histbuf[i] = TempHist[i] ;

		free( TempHist );	/* Free allocated memory */
		}

	for( x=0; x<size; x++ )
		*maxfreq = Max( histbuf[x], *maxfreq );

	IOStatus = fclose(pds_image[file_index].fp);	/* Close PDS image file */	
	if ( IOStatus != 0 )
		return -1;
	else
		pds_image[file_index].file_open_flag = FALSE;

	return( 0 );
	}
  		
 /****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xaclearscale, XACLEARSCALE) ()
        {
	zaclearscale();
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/****************************/
 	/* Clear all current scales */
 	/****************************/
 	int zaclearscale () 
 	{
 	   int i, j ; 
 	
 	   if (( ScaleVector[ 0 ].Type != 'C') && 
 	       ( ScaleVector[ 1 ].Type != 'L') && 
 	       ( ScaleVector[ 2 ].Type != 'R')) 
 	         return( 0 ) ;  /* Nothing has been defined */
 	   else
 	   {
 	      ScaleVector[ 0 ].Type = 0 ;
 	      ScaleVector[ 0 ].End_Cap_Length = 0 ;
 	      ScaleVector[ 0 ].End_Cap_Width  = 0 ;
 	      ScaleVector[ 0 ].Center_Line_Width  = 0 ;
 	
 	      for ( i = 1 ; i <= 2 ; i ++ )
 	      {
 	         for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	         {
 	            ScaleVector[ i ].Tick_Length[j] = 0 ;
 	            ScaleVector[ i ].Tick_Width[j]  = 0 ;
 	            ScaleVector[ i ].Tick_Increment[j][0] = 0 ;
 	            ScaleVector[ i ].Tick_Increment[j][1] = 0 ;
 	            ScaleVector[ i ].Annotation_Modulus[j]  = 0 ;
 	            ScaleVector[ i ].Annotation_Justification[j]  = 'L';
 	            if (ScaleVector[ i ].Annotation_Position[j] != 0 )
 	              ScaleVector[ i ].Annotation_Position[j] = 0 ;
 	         }
 	         ScaleVector[ i ].Type = 0 ;
 	         ScaleVector[ i ].Array_Length = 0 ;
 	      }
 	   }
 	   return( 0 ) ;
 	}
   		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xacopymask, XACOPYMASK) ( char *ImFlag, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char *c_string;
        int length;

        zsfor2len(length, ImFlag, &ImFlag,1, 1, 1, ImFlag);
        c_string = (char *)calloc(1,(length+1));	
        zsfor2c(c_string, length, ImFlag, &ImFlag, 1, 1, 1, ImFlag); 
	zacopymask( c_string[0]);
        free (c_string);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/**********************/
 	/* Copy Mask to Image */
 	/**********************/
 	
 	/*
 	
 	Output image sizing comes in three flavors :
 	
 	1) If the user defines the maximum number of lines and samples. 
	   we use it.
 	2) If the user has not defined (1), we derive from the mask data
 	   the number of lines and samples that are needed.
 	3) If the input image is larger than (2), 
	   we expand the sizing to the input.
 	
 	Algorithm :

	   Open input files
	   Calculate stretch for each image
	   Calculate histogram for each image
	   Determine the maximum lines and samples for output image
	   Calculate the number of color bands to be output
	   Open output file
	   Determine if the mirror image flag is present

	   Loop for the number of bands in output 
	     Begin 		
  	       Loop for the number of lines in output
                 Begin
                    	Fill mask array with background DN or color

 			If ( line # greater than minimum box line) Then
				Loop through linked list of box structures
			           If ( line # less than last box line ) Then
             				Copy box segment to mask array
           			   Else
             			        Drop box structure out of the list
	
 			If ( line # greater than minimum image line) Then
				Loop through linked list of image structures
			           If ( line # less than last image line ) Then
             				Copy image to mask array
           			   Else
             			        Drop image structure out of the list
	
 			If ( line # greater than minimum zoomed image line) Then
				Loop through list of zoomed image structures
			           If ( line less than last zm image line ) Then
             				Copy zoomed image to mask array
           			   Else
             			        Drop zm image structure out of the list
	
 			If ( line # greater than minimum buffer line) Then
				Loop through linked list of buffer structures
			           If ( line # less than last buffer line ) Then
             				Copy buffer segment to mask array
           			   Else
             			        Drop buffer structure out of the list
	
 			If ( line # greater than minimum tick line) Then
				Loop through linked list of tick structures
			           If ( line # less than last tick line ) Then
             				Copy tick segment to mask array
           			   Else 
             			        Drop tick structure out of the list
	
 			If ( line # greater than minimum string line) Then
				Loop through linked list of string structures
			           If ( line # less than last string line ) Then
             				Copy string segment to mask array
           			   Else
             			        Drop string structure out of the list
		
		       	If ( mirror image flag is set )   
        			Write mirrored mask array to output
       			Else
         			Write mask array to output
     		  End
   		End
	
	   Close input files
	   Close output file

 	*/
  	int zacopymask(ImFlag)
 	unsigned char  	ImFlag;
 	{
 	int input, output, IOStatus;	/* I/O unit #s, Status flag 	*/
  	int OutLines, OutSamples;	/* Output lines and samples 	*/
 	int 	i, j, k;		/* Index counters 		*/
	int	line,outline;	 	/* Line number variables	*/
 	int  	sample_index ;
	int	band, Samp, OutSamp, NumSamp;
	int 	HistIndex, temp, itemp[HMAX];
	int 	sign,			/* Ellipse variables		*/
		end,g,
		arcwidth,		/* Width of arc line		*/
		width;			/* Start sample of arc 		*/
	float   denominator,multiplier;	/* Smoothing variables  	*/
	int 	XOffset,YOffset;	/* Mask offset within border 	*/
	
	char	str[100];		/* Message printing string	*/

        unsigned int 	Level;
 	unsigned char	**mirror_buffer;          
 	unsigned char	**border;          
	short int 	*InputLine;
 
 	struct HistType    	*hptr;	/* Declare pointers to structures */
 	struct ImageType   	*iptr;
 	struct PDSImageType 	*PDSiptr;
 	struct TickType    	*tptr;
 	struct VectorType 	*vptr;
 	struct BoxType     	*bptr;
 	struct EllipseType 	*eptr;
 	struct StringType  	*sptr;
	void 		  *temps;
	void 		  *tempv;
	void 		  *tempb;
	void 		  *tempe;
	void 		  *tempi;
	void 		  *tempt;
	void 		  *temph;

 	xxaopeninputfiles() ;		/* Output all input files	*/
 	
 	xxaapplystretch() ;		/* For the images 		*/
 	
	/* Determine max. lines & samples */
 	xxacalcmaxlinessamples() ; 	

	/* Calculate the number of color bands to be output */
	xxacalccolorbands() ;


	/* Set mask output file dimensions */
 	
	OutLines   	= UserMaxLines;
	OutSamples 	= UserMaxSamples;

	if( MaskSizeLines == 0 || MaskSizeSamples == 0 )
		{
	 	MaskSizeLines   = UserMaxLines ;
 		MaskSizeSamples = UserMaxSamples ;
		}

	/* Set mask border (matte) offsets */

	YOffset	   = (MaskSizeLines-OutLines)/2;;
	XOffset	   = (MaskSizeSamples-OutSamples)/2;

 	/* Get an output unit number */

	if ( redirect_mask_output )
		IOStatus = zvunit( &output, "NA", 1, "U_NAME", mask_file_name, NULL );
	else
	 	IOStatus = zvunit( &output, "OUT", OutputInstance, NULL ) ;

	if ( IOStatus != 1 )
		{
		zvmessage("Failure in returning output unit number"," ");
		zvmessage(" - za/xacopymask aborted."," ");
		return -1;
		}

 	/* Open output file */

 	IOStatus = zvopen( output,"OPEN_ACT", "SA", "IO_ACT", "SA",
 	                   "OP", "WRITE", "U_NL", MaskSizeLines,
		           "U_NS", MaskSizeSamples,"U_NB",Dimension,
                           "U_ORG","BSQ","U_FORMAT", "BYTE","O_FORMAT", "BYTE",
                           NULL ) ;
        if (IOStatus != 1) 
          	{
           	sprintf(str,"IOStatus of %d for output file", IOStatus);
	   	zvmessage(str," ");
           	zvmessage("Fail to open output", " ");
		zvmessage(" - za/xacopymask aborted."," ");
	   	return -1;
           	}
		   
	/* Perform set up if mask mirroring is desired	*/
 	
 	zccase( &ImFlag, 1, 1 );
 	if ( ImFlag == 'M' )
 	       {
 	       mirror_buffer = (unsigned char **)calloc
			       (Dimension, sizeof(unsigned char *));
 	       if(mirror_buffer != NULL)
			for(i=0; i<Dimension; i++)
				mirror_buffer[i] = (unsigned char *)calloc
				   (MaskSizeSamples, sizeof( unsigned char ));
	       else
			{
			zvmessage("Allocation error for image mirroring."," ");
			zvmessage(" - za/xacopymask aborted."," ");
		   	return -1;
			}
  	       }
  		
	/* Allocate memory for Mask buffer */

	Mask = (unsigned char **)calloc(Dimension,sizeof(unsigned char *));

	if( Mask != NULL )
		for( d=0; d<Dimension; d++ )
			{
			Mask[d] = (unsigned char *)calloc
				  (MaskSizeSamples,sizeof(unsigned char));	
			if( Mask[d] == NULL )
				{
				zvmessage("Memory allocation error"," ");
				zvmessage(" - za/xacopymask aborted."," ");
				return -1;
				}
			}	
	else
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" - za/xacopymask aborted."," ");
		return -1;
		}

	if( XOffset > 0 )
		{
		border = (unsigned char **)
			 calloc(Dimension,sizeof(unsigned char *));
	   	if( border != NULL )
		   for( d=0; d<Dimension; d++ )
			{
			border[d] = (unsigned char *)calloc
				  (XOffset+1,sizeof(unsigned char));	
			if( border[d] == NULL )
				{
				zvmessage("Memory allocation error"," ");
				zvmessage(" - za/xacopymask aborted."," ");
				return( -1 );
				}
	 		fillbyte( border[d], BorderDn[d], (XOffset+1) ); 
   	        	fillbyte( Mask[d], BorderDn[d], MaskSizeSamples ); 
			}	
	   	else
		   {
		   zvmessage("Memory allocation error"," ");
		   zvmessage(" - za/xacopymask aborted."," ");
		   return( -1 );
		   }
		}

	/* If there is a border atop mask, write it to the output file */

	for ( line = 1; line <= YOffset; line++ )
		{
		/* Write output line finally */
 		if ( ImFlag == 'M' )
 			{
 			sample_index = MaskSizeSamples - 1 ;
 			for ( i = 0; i < MaskSizeSamples ; i ++ )
 				{
				for( d=0; d<Dimension; d++ )
 				  mirror_buffer[d][i] = Mask[d][sample_index] ;
 				sample_index -- ;   
 				}
			for( d=1; d<=Dimension; d++ )
 			  IOStatus = zvwrit( output,&mirror_buffer[d-1],
				             "SAMP",1,"LINE",line,"BAND",d, NULL);
 			}
 		else
			for( d=1; d<=Dimension; d++ )
 			  IOStatus = zvwrit( output,Mask[d-1],
				             "SAMP",1,"BAND",d,"LINE",line, NULL);
		}

	   /* Write mask output file line by line */

	   for ( line = 1; line <= OutLines; line++ )
 	   	{
		for( d=0; d<Dimension; d++ )
		   fillbyte( Mask[d], BackgroundDn[d], OutSamples ); 

 	      	/*************************/
 	      	/* Copy gray scale boxes */
 	      	/*************************/
		if( line >= minboxline )
		  {
 		  bptr = firstbox;	/* Start linked list search of boxes */
 		  while(bptr!=NULL)
		    { 
		    tempb = bptr->next;	/* Store pointer to next box */
		    if ( line >= bptr->StartLine )
			{
			if ( line <= bptr->EndLine )
 	        		{
 				j = bptr->StartSample ;
				for( d=0; d<Dimension; d++ )
 				   fillbyte(&Mask[d][j],bptr->Dn[d],
					bptr->Width) ;
 	        		}
			else
				{	/* Drop structure from linked list */
				if( bptr->previous==NULL )
				  {
				  if ( bptr->next==NULL )
					firstbox = tempb = NULL;
			          else			
					{
					(bptr->next)->previous = NULL;
					firstbox = bptr->next;
					}
				  }
				else
				  {
				  if( bptr->next != NULL )
					(bptr->next)->previous = bptr->previous;
				  (bptr->previous)->next = bptr->next;
				  }
				free( bptr );	/* Free memory used by struct */
				}
			}
		    bptr = tempb;
		    }
 		  }
 	
 	      	/*******************/
 	      	/* Copy image data */
 	      	/*******************/
	 	/*
 		Algorithm :
 	
 		Loop with line number
 	   		If ( line intersects with any image array records )
 	      			Read a line from input file
 	      			Store image subset in line
 	   		EndIf
 		EndLoop
 		*/
		if( line >= minimageline )
		  {
	 	  iptr = firstimage;	
 		  while(iptr != NULL)   /* Loop through all images	*/
		    {
		    tempi = iptr->next;
		    if( line >= iptr->OutStartLine )
	    	      {
		      if( line < iptr->OutStartLine + iptr->EndLine
				 - iptr->StartLine + 1 )
			{
			OutSamp = iptr->OutStartSample - 1;
			for(d=0; d<Dimension; d++)
			  {
			  if( iptr->OutBand==(d+1) || !iptr->OutBand )
			    {
			    band = d+1;
			    if( iptr->InputBands==1 )
			 	band = 1;
 		
			    /* Get input line from appropriate input file */
			    k = iptr->StartLine + (line - iptr->OutStartLine);
 		
			    /* Allocate memory for image line zvread      */
			    InputLine = (short int *)calloc
					(iptr->InputSamples,sizeof(short int));	
			    if( InputLine == NULL )
				{
				zvmessage("Memory allocation error"," ");
				zvmessage(" - image copy aborted."," ");
				break;
				}

			    IOStatus = zvread(iptr->InputUnitNumber,
					InputLine,
				      	"LINE",k,
					"SAMP",iptr->StartSample,
					"NSAMPS",iptr->InputSamples,
					"BAND",band,NULL);
	 			
			    if( iptr->NumDNs == 256 )
			      for(j=0;j<iptr->InputSamples;j++)	
				Mask[d][OutSamp+j] = iptr->LUT[(iptr->NumDNs-1) &
							InputLine[j]];
			    else
			      {
			      if( iptr->Stretch == NoStretch )
			      	for(j=0; j<iptr->InputSamples; j++)	
					{
					if( InputLine[j]<0 )
					  	InputLine[j] = 0;
					Mask[d][OutSamp+j] = 
						SCALE32K * iptr->LUT[32767 & 
						InputLine[j]];
					}
			      else
			      	for(j=0; j<iptr->InputSamples; j++)	
					Mask[d][OutSamp+j] = 
						SCALE64K * iptr->LUT[65535 & 
						(InputLine[j]+32768)];
			      }
			    free(InputLine);   /* Free memory */
			    }
		    	  else
			    if( iptr->ColorBands==1 && iptr->OutBand!=(d+1))
				for(j=0; j<iptr->InputSamples; j++)
					Mask[d][OutSamp+j] = 0;
		    	  }
			}
		      else
			{	/* Drop structure from linked list */
			if( iptr->previous == NULL )
				  {
				  if ( iptr->next == NULL )
					firstimage = tempi = NULL;
			          else			
					{
					(iptr->next)->previous = NULL;
					firstimage = iptr->next;
					}
				  }
			else
				  {
				  if( iptr->next != NULL )
					(iptr->next)->previous = iptr->previous;
				  (iptr->previous)->next = iptr->next;
				  }
			free(iptr);	/* Free memory used by struct */
			}
		      }
 		    iptr = tempi;
 		    }
		  }
 	
 	      	/***********************/
 	      	/* Copy PDS image data */
 	      	/***********************/
	 	/*
 		Algorithm :
 	
 		Loop with line number
 	   		If ( line intersects with any image array records )
 	      			Read a line from input file
 	      			Store image subset in line
 		EndLoop
 		*/
		if( line >= minPDSimageline )
		  {
	 	  PDSiptr = firstPDSimage;	
 		  while(PDSiptr != NULL)   /* Loop through all images	*/
		    {
		    tempi = PDSiptr->next;
		    if( line >= PDSiptr->OutStartLine )
	    	      {
		      if( line < PDSiptr->OutStartLine + PDSiptr->EndLine
				 - PDSiptr->StartLine + 1 )
			{
			OutSamp = PDSiptr->OutStartSample - 1;

			for(d=0; d<Dimension; d++)
			  {
			  if( PDSiptr->OutBand==(d+1) || !PDSiptr->OutBand )
			    {
			    band = d+1;
			    if( PDSiptr->InputBands==1 )
			 	band = 1;
 		
			    /* Get input line from appropriate input file */

			    k = PDSiptr->StartLine + (line - PDSiptr->OutStartLine);
 		
			    /* Allocate memory for image line read      */

			    InputLine = (short int *)calloc
				(PDSiptr->InputSamples,sizeof(short int));	
			    if( InputLine == NULL )
				{
				zvmessage("Memory allocation error"," ");
				zvmessage(" - PDS image copy aborted."," ");
				break;
				}
				
			    /* Read image line from PDS file		*/
			    IOStatus = read_PDS_image_line(PDSiptr->InputFileName,
					InputLine,k,PDSiptr->StartSample,
					PDSiptr->InputSamples,band);
	 		    if ( IOStatus < 0 )
				return -1;

			    if( PDSiptr->NumDNs == 256 )
			      for(j=0;j<PDSiptr->InputSamples;j++)	
				Mask[d][OutSamp+j]
				= PDSiptr->LUT[(PDSiptr->NumDNs-1)&InputLine[j]];
			    else
			      {
			      if( PDSiptr->Stretch == NoStretch )
			      	for(j=0; j<PDSiptr->InputSamples; j++)	
					{
					if( InputLine[j]<0 )
					  	InputLine[j] = 0;
					Mask[d][OutSamp+j] = 
						SCALE32K *
						PDSiptr->LUT[32767 & 
						InputLine[j]];
					}
			      else
			      	for(j=0; j<PDSiptr->InputSamples; j++)	
					Mask[d][OutSamp+j] = 
						SCALE64K *
						PDSiptr->LUT[65535 & 
						(InputLine[j]+32768)];
			      }
			    free( InputLine );   /* Free memory */
			    }
		    	  else
			    if( PDSiptr->ColorBands == 1 && PDSiptr->OutBand != (d+1))
				for(j=0; j<PDSiptr->InputSamples; j++)	
					Mask[d][OutSamp+j] = 0;
		    	  }
			}
		      else
			{	/* Drop structure from linked list */
			if( PDSiptr->previous==NULL )
				  {
				  if ( PDSiptr->next==NULL )
					firstPDSimage = tempi = NULL;
			          else			
					{
					(PDSiptr->next)->previous = NULL;
					firstPDSimage = PDSiptr->next;
					}
				  }
			else
				  {
				  if( PDSiptr->next != NULL )
					(PDSiptr->next)->previous = PDSiptr->previous;
				  (PDSiptr->previous)->next = PDSiptr->next;
				  }
			free( PDSiptr );	/* Free memory used by struct */
			}
		      }
 		    PDSiptr = tempi;
 		    }
		  }
 		
  	      	/*******************/
 	      	/* Copy zoom image */
 	      	/*******************/
  		xxacopyzoom( line ) ;
 		
  	      	/***********************/
 	      	/* Copy zoom PDS image */
 	      	/***********************/
  		xxacopyPDSzoom( line ) ;
 	
  	      	/*******************/
 	      	/* Copy buffer	   */
 	      	/*******************/

                 xxacopybuffer( line ) ;
 			
 	      	/*************************/
 	      	/* Copy ellipse		 */
 	      	/*************************/
		if( line >= minellipseline )
		  {
 		  eptr = firstellipse;	/* Start link list search of ellipses */
 		  while(eptr!=NULL)
		    { 
		    tempe = eptr->next;	/* Store pointer to next ellipse */
		    if ( line >= eptr->CenterLine - eptr->Radius2 )
		       {
		       if ( line <= eptr->CenterLine + eptr->Radius2 )
			  {
			  i = abs( eptr->CenterLine-eptr->Radius2-line );
			  for( j=0; j<4; j++ )
			     {
			     sign = -1;
			     if ( j==0 || j==2 )
			    	sign = 1;
			     if ( eptr->Smooth[j][i] > 1 )
				   {
				   denominator = eptr->Smooth[j][i] + 1;
				   for ( g=eptr->Smooth[j][i],k=1; 
					 k<=eptr->Smooth[j][i]; k++, g-- )
				       {
				       multiplier 
					= pow((double)k/denominator,2.0);
				       for ( d=0; d<Dimension; d++ )
					  Mask[d][eptr->Edge[j][i]-g*sign-1]
					  =  eptr->Dn[d]*multiplier
					     + (1.0-multiplier)*
					     Mask[d][eptr->Edge[j][i]-g*sign-1];
				        }
			           }
			     else
				   for ( d=0; d<Dimension; d++ )
				       Mask[d][eptr->Edge[j][i]-sign-1]
					   = (eptr->Dn[d]*eptr->Smooth[j][i])
					   + ((1.0-eptr->Smooth[j][i]) *
					   Mask[d][eptr->Edge[j][i]-sign-1]);
                            } 		    

			  if( eptr->Edge[1][i] == 0 )
				{
				width = eptr->Edge[3][i] 
					- eptr->Edge[0][i] + 1;
				for( d=0; d<Dimension; d++ )
					fillbyte(&Mask[d][eptr->Edge[0][i]-1],
				  	eptr->Dn[d],width);
				}
			  else
				{
				width = eptr->Edge[1][i] 
						- eptr->Edge[0][i] + 1;
				for( d=0; d<Dimension; d++ )
					fillbyte(&Mask[d][eptr->Edge[0][i]-1],
					  	eptr->Dn[d],width);
				width = eptr->Edge[3][i]
						- eptr->Edge[2][i] + 1;
				for( d=0; d<Dimension; d++ )
			        	fillbyte(&Mask[d][eptr->Edge[2][i]-1],
						eptr->Dn[d],width);
				}
			  }
		   	else
				{	/* Drop structure from linked list */
				if( eptr->previous==NULL )
					{
					if ( eptr->next==NULL )
						firstellipse = tempe = NULL;
					else			
						{
						(eptr->next)->previous = NULL;
						firstellipse = eptr->next;
						}
					}
				else
					{
					if( eptr->next != NULL )
					 	(eptr->next)->previous 
							= eptr->previous;
					(eptr->previous)->next = eptr->next;
					}
				free( eptr );	/* Free memory used by struct */
				}
			}
		    eptr = tempe;
		    }
 		  }
 	
 		/*******************/
 	      	/* Copy tick marks */
 	      	/*******************/
 	 	if( line >= mintickline )
		  {
		  tptr = firsttick;
 	   	  while( tptr != NULL )
 		    {
		    tempt = tptr->next;
 	            if ( line >= tptr->StartLine )
 	              {
		      if ( line <= tptr->LastLine )
			{
			if ( tptr->StartLine == tptr->EndLine )
 	              /* If (line is within range) Then
 	                  For (each tick within range of start sample 
				to end sample)
 	                     For (the width of one tick)
 	                        Store a Dn
 	              */
 			     {
 	               	     for(j = tptr->StartSample; j <= tptr->EndSample;
 	                     	 j += tptr->Spacing + tptr->Width)
 	
 	                  	for ( k = 0 ; k < tptr->Width ; k++ )
 	                     		if ( j+k <= tptr->EndSample )
 			                   for(d=0; d<Dimension; d++)
						Mask[d][j+k] = tptr->Dn[d];
 	                     		else
 	                        		break ;
 	         	     }
			else
			    {
 	            	/* If (line is within range) Then
 	                  For (each tick within range of start line 
			     and end line)
 	                     If (line is within range of tick) Then
 	                        Store tick on line	*/
 	               	    for ( j = tptr->StartLine; j <= tptr->EndLine;
 	                	  j += tptr->Spacing + tptr->Width)
 	                  	if ( line >= j && line < j + tptr->Width &&
 	                       	   ( j + tptr->Width - 1 ) <= tptr->EndLine )
 	                  		{
					for( d=0; d<Dimension; d++ )
 					  fillbyte(&Mask[d][tptr->StartSample],
 	                                	   tptr->Dn[d],tptr->Length ) ;
 	                     		break ;
 	                  		}
			    }
			}
		      else
			{	/* Drop structure from linked list */
			if( tptr->previous==NULL )
				  {
				  if ( tptr->next==NULL )
					firsttick = tempt = NULL;
			          else			
					{
					(tptr->next)->previous = NULL;
					firsttick = tptr->next;
					}
				  }
			else
				  {
				  if( tptr->next != NULL )
					(tptr->next)->previous = tptr->previous;
				  (tptr->previous)->next = tptr->next;
				  }
			free( tptr );	/* Free memory used by struct */
			}
		      }
		    tptr = tptr->next;
 	      	    }
 	          }
 	
 		/*******************/
 	      	/* Copy histograms */
 	      	/*******************/
		if( line >= minhistline )
		  { 	
 	   	  hptr = firsthist;
 	   	  while ( hptr != NULL )
 		    {
		    temph = hptr->next;
		    if ( line >= hptr->HistStLine && hptr->ColorBands != -2)
		      {
		      if( line <= hptr->HistEnLine )
 	      		{
 	         	if ( hptr->Direction == Vert )	/* Vertical case */
 	         	   {
 	                  /* Determine scaled down histogram index 
						from current line
 	               	   For (each histogram index (0-255) within bounds 
			   		of the scaled down histogram index)
 	                  	Scale down histogram element length
 	                  	Store the scaled down histogram element length
 	               	   EndFor 					*/
 	
 	            	   HistIndex = line - hptr->HistStLine ;
			   itemp[0] = 0;

			   switch(hptr->Display){
				 
			   case Blend:

 	            	   for ( k = 0 ; k <= MAXDNVALUE ; k++ )
 	            		{
 	               		temp = k * hptr->HistScale;

				/* Is k within bounds? */
 	               		if ( HistIndex == temp )
 	               		   {
				   for(band=0;band<hptr->ColorBands;band++)
					itemp[band+1] = 
					hptr->Ptr[band][k]->Histogram[k] 
					* hptr->PixScale;

				   for(band=0;band<hptr->ColorBands;band++)
	         		      for(j=itemp[band];j<itemp[band+1];j++)
				         for(d=0;d<Dimension;d++)
 					    Mask[d][j+hptr->HistStSample] =
 	                         		hptr->Blend[band][k][d];
				   }
 	               		else
 	               		   if ( HistIndex < temp )
 	                  		break ;
 	            		}
			   break;
	
			   case Minimum: case None:

 	            	   for ( k = 0 ; k <= MAXDNVALUE ; k++ )
 	            		{
 	               		temp = k * hptr->HistScale;

				/* Is k within bounds? */
 	               		if ( HistIndex == temp )
 	               		   {
				   for(band=0;band<hptr->ColorBands;band++)
					itemp[band+1] = 
					hptr->Ptr[band][k]->Histogram[k] 
					* hptr->PixScale;
				   for(band=0;band<hptr->ColorBands;band++)
	         		      for(j=itemp[band];j<itemp[band+1];j++)
				         for(d=0;d<Dimension;d++)
 					    Mask[d][j+hptr->HistStSample] =
					    hptr->Ptr[band][k]->Dn[d];
				   }
 	               		else
 	               		   if ( HistIndex < temp )
 	                  		break ;
 	            		}
			   break;	}	/* End switch statement */
 	         	   }
 	         	else				/* Horizontal case */
			   {
 	            	   /* Scale down current line # to defined level
 	               		   For (each histogram element)
 	                  		If (element >= Scaled down current line)
 	                     			Scale down histogram index 
						(0-maxDN) to defined level
 	                     			Store a pixel to scaled down 
						histogram index
 	                  		EndIf
 	               		   EndFor  */
 	
 	            	   Level = ((float)((hptr->HistEnLine-hptr->HistStLine 
			           +1) - (line-hptr->HistStLine))) / 
				   hptr->PixScale ;

			   switch( hptr->Display ) {

			   case Blend:	

 	           	   for ( j = 0 ; j <= MAXDNVALUE; j++ )
				{
				band = -1;
 				/* Compare to current line level */
 				for( k=hptr->ColorBands-1; k>=0; k--)
				   if( hptr->Ptr[k][j]->Histogram[j] >= Level )
					band = k;

        	       		if( band >= 0 )
					{
 				        /* Scale down histogram index */
 	                  		temp = j * hptr->HistScale ;
					for( d=0; d<Dimension; d++ )
					      Mask[d][temp+hptr->HistStSample]
					      = hptr->Blend[band][j][d];
					}
				}
			   break;
	
			   case Minimum:

 	           	   for ( j = 0 ; j <= MAXDNVALUE; j++ )
				{
				temp = j * hptr->HistScale;
 				/* Compare to current line level */
 				for( band=(hptr->ColorBands)-1;band>=0;band--)
				   if( hptr->Ptr[band][j]->Histogram[j] 
				       >= Level )
				      for(d=0;d<Dimension;d++)
					Mask[d][temp+hptr->HistStSample]
					= hptr->Ptr[band][j]->Dn[d];
				}
			   break;
			   
                           case None:

			   for( j=0; j <= MAXDNVALUE; j++ )
				if(hptr->Ptr[0][j]->Histogram[j] >= Level )
					{
 				        /* Scale down histogram index */
 	                  		temp = j * hptr->HistScale ;
					for( d=0; d<Dimension; d++ )
 					      Mask[d][temp+hptr->HistStSample]
					      = hptr->Dn[d] ;
					}
			   break;} /* End switch statement */
 	         	   }
			}
		      else
			{	/* Drop structure from linked list */
			if( hptr->previous==NULL )
				  {
				  if ( hptr->next==NULL )
					firsthist = temph = NULL;
			          else			
					{
					(hptr->next)->previous = NULL;
					firsthist = hptr->next;
					}
				  }
			else
				  {
				  if( hptr->next != NULL )
					(hptr->next)->previous = hptr->previous;
				  (hptr->previous)->next = hptr->next;
				  }
			free( hptr );	/* Free memory used by struct */
			}
		       }
 		    hptr = hptr->next;
 		    }
 	          }

 	      	/*********************/
 	      	/* Copy line vectors */
 	      	/*********************/
 		if( line >= minvectorline )	
		  {
 		  vptr = firstvector;
 		  while( vptr != NULL )
 		    {
		    tempv = vptr->next;
		    if( line >= vptr->StartLine )
	              {
		      if( line <= vptr->EndLine )
			xxadrawvector(line,vptr);
		      else
			{
			if( vptr->previous==NULL )
				  {
				  if ( vptr->next==NULL )
					firstvector = tempv = NULL;
			          else			
					{
					(vptr->next)->previous = NULL;
					firstvector = vptr->next;
					}
				  }
			else
				  {
				  if( vptr->next != NULL )
					(vptr->next)->previous = vptr->previous;
				  (vptr->previous)->next = vptr->next;
				  }
			free( vptr );
			}
		      }
 		    vptr = tempv;
 		    }
		  }
		
 	
 	      	/**********************/
 	      	/* Copy ascii strings */
 	      	/**********************/
		if( line >= minstringline )
		  {
 		  sptr = firststring;
 		  while(sptr!=NULL)
 			{
			temps = sptr->next;
			if( line >= sptr->StartLine )
			    {
			    if( line < sptr->LastLine )
 	            		{
 	               		if ( sptr->FontType == DEFAULT )
				     {
				     if( sptr->Angle == 0 )
 	                		     xxatransferstring( sptr, line );
				     else
 	                		     xxatransferstring90( sptr, line );
				     }
 	        		else
				     xxacopyhersheystring(sptr,line) ;
 	        		}
			    else
				{
				if( sptr->previous==NULL )
				  {
				  if ( sptr->next==NULL )
					firststring = temps = NULL;
			          else			
					{
					(sptr->next)->previous = NULL;
					firststring = sptr->next;
					}
				  }
				else
				  {
				  if( sptr->next != NULL )
					(sptr->next)->previous = sptr->previous;
				  (sptr->previous)->next = sptr->next;
				  }
				free( sptr );
				}
			   }
 	         	sptr = temps;
 			}
		  }

		outline = line + YOffset; 	

 		/* Write output line finally */
 		if ( ImFlag == 'M' )
 			{
 			sample_index = OutSamples - 1 ;
 			for ( i = 0; i < OutSamples ; i ++ )
 				{
				for( d=0; d<Dimension; d++ )
 				  mirror_buffer[d][i] = Mask[d][sample_index] ;
 				sample_index -- ;   
 				}
			for( d=1; d<=Dimension; d++ )
			  {
			  if( XOffset > 0 )
 			     IOStatus = zvwrit( output,border[d-1],"SAMP",1,
				                "NSAMPS",XOffset,"LINE",outline,
                                                "BAND",d, NULL );
			  sample_index = XOffset + 1;
 			  IOStatus = zvwrit( output,&mirror_buffer[d-1],
				             "SAMP",sample_index,"NSAMPS",
                                             OutSamples,"LINE",outline,
                                             "BAND",d, NULL);
				  
			  sample_index += OutSamples;
			  if( XOffset > 0 )
			      {
			      j = MaskSizeSamples - XOffset - OutSamples;
 			      IOStatus = zvwrit(output,border[d-1],
			                        "SAMP",sample_index,"NSAMPS",j,
				                "LINE",outline,"BAND",d, NULL);
			      }
			  }
 			}
 		else
			for( d=1; d<=Dimension; d++ )
			  {
			  if( XOffset > 0 )
 			      IOStatus = zvwrit( output,border[d-1],"SAMP",1,
				                 "NSAMPS",XOffset,"BAND",d,
                                                 "LINE",outline,NULL);
			  sample_index = XOffset + 1;
 			  IOStatus = zvwrit( output,Mask[d-1], "SAMP",
				             sample_index,"NSAMPS",OutSamples,
				             "BAND",d,"LINE",outline, NULL);
			  sample_index += OutSamples;
			  if( XOffset > 0 )
			      {
			      j = MaskSizeSamples - XOffset - OutSamples;
 			      IOStatus = zvwrit( output,border[d-1],"SAMP",
				                 sample_index,"NSAMPS",j,
				                 "BAND",d,"LINE",outline,NULL);
			      }
			  }
 		}
	   if( XOffset > 0 )	/* If there is a side border, free array */
		free( border );

	   if( YOffset > 0 )
 	     for( d=0; d<Dimension; d++ )
   	        fillbyte( Mask[d], BorderDn[d], MaskSizeSamples ); 

	   for ( line = YOffset+OutLines+1; line <= MaskSizeLines; line++ )
		{
		/* Write output line finally */
 		if ( ImFlag == 'M' )
 			{
 			sample_index = MaskSizeSamples - 1 ;
 			for ( i = 0; i < MaskSizeSamples ; i ++ )
 				{
				for( d=0; d<Dimension; d++ )
 				  mirror_buffer[d][i] = Mask[d][sample_index] ;
 				sample_index -- ;   
 				}
			for( d=1; d<=Dimension; d++ )
 			  IOStatus = zvwrit( output,&mirror_buffer[d-1],
				             "SAMP",1,"LINE",line,"BAND",d, NULL);
 			}
 		else
			for( d=1; d<=Dimension; d++ )
 			  IOStatus = zvwrit( output,Mask[d-1],"SAMP",
				             1,"BAND",d,"LINE",line,NULL);
		}

	   free( Mask );

 	   xxacloseinputfiles() ;		/* Close all input files */
 	   IOStatus = zvclose( output, NULL ) ;	/* Close mask output     */
           zvmessage ("Finished copy mask", " ");
	   return(0);
   	}
   	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xadefinescale, XADEFINESCALE) (char *Scale_Type,
		int *Elength, int *Ethick, int *Cthick, int *Length_Of_Array,
		char *Annotation_Position,
		double *Annotation_Start_Value, double *Annotation_Increment,
		double *Annotation_Modulus, int *Annotation_Significant_Digits,
                int *Annotation_Size, char *Annotation_Orientation,
		char *Annotation_Justification, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK

        struct 
 	   {
 	   unsigned char Annotation_Position ;
 	   double Annotation_Start_Value ;
 	   double Annotation_Increment ;
 	   double Annotation_Modulus ;
 	   int Annotation_Significant_Digits ;
 	   int Annotation_Size ;
 	   unsigned char Annotation_Orientation ;
 	   unsigned char Annotation_Justification ;
	   } FAnnotation;

        char c_string[2], c_string1[2], c_string2[2], c_string3[2];
        int length;

        length = 1; 
        zsfor2c(c_string, length, Scale_Type, &Scale_Type, 13, 1, 1,
						Annotation_Justification); 

        zsfor2c(c_string1, length, Annotation_Position, &Scale_Type, 13, 6, 2,
						Annotation_Justification); 
        FAnnotation.Annotation_Position = c_string1[0];

        zsfor2c(c_string2, length, Annotation_Orientation, &Scale_Type, 13, 12,
               3, Annotation_Justification); 
        FAnnotation.Annotation_Orientation = c_string2[0];

        zsfor2c(c_string3, length, Annotation_Justification, &Scale_Type, 13, 
               13, 4, Annotation_Justification); 
        FAnnotation.Annotation_Justification = c_string3[0];

        FAnnotation.Annotation_Start_Value = *Annotation_Start_Value;
        FAnnotation.Annotation_Increment = *Annotation_Increment;
        FAnnotation.Annotation_Modulus = *Annotation_Modulus;
        FAnnotation.Annotation_Significant_Digits= 
                                                *Annotation_Significant_Digits;
        FAnnotation.Annotation_Size = *Annotation_Size;

 	zadefinescale(c_string[0],Elength,Ethick,Cthick,*Length_Of_Array,
                      &FAnnotation);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	/*********************/
 	/* Store a scale     */
 	/*********************/
 	/*
 	Parameters :
 	
 	   Scale type
 	   End cap length or Tickmark length
 	   End cap thickness or Tickmark thickness
 	   Center line thickness or Tickmark frequency
 	   Array length
 	   Address Of Annotation Structure
 	*/
 	
 	int zadefinescale(Scale_Type,Elength,Ethick,Cthick,Length_Of_Array,
                      Address_Of_Annotation)
 	int *Elength, *Ethick, *Cthick, Length_Of_Array; 
        struct AnnotationType  *Address_Of_Annotation;
        unsigned char Scale_Type;
  	{
  	   int i , j ;
 	   double *Period ;

  	   zccase(&Scale_Type,1,1);
 	
 	   if (( Scale_Type != 'L') && ( Scale_Type != 'R') && 
						( Scale_Type != 'C'))
 	      {
 	         zmabend ("Check Scale Type") ;
 	         return( -1 ) ;
 	      }
 	   else
 	      if ( Scale_Type == 'C') 
 	      {
 	            i = 0 ;
 	            ScaleVector[ i ].Type = Scale_Type ;
 	            ScaleVector[ i ].End_Cap_Length = *Elength ;
 	            ScaleVector[ i ].End_Cap_Width  = *Ethick ;
 	            ScaleVector[ i ].Center_Line_Width  = *Cthick ;
 	            return( 0 ) ;
 	      }
 	      else
 	      {
 	            if ( Scale_Type == 'L') 
 	                  i = 1 ;
 	            else
 	                  i = 2 ;                 /* Scale_Type = 'R' */
                    Period = (double *)Cthick ;

 	            ScaleVector[ i ].Type = Scale_Type ;
  	            ScaleVector[ i ].Array_Length = Length_Of_Array ;

 	            for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	                  {
 	                  ScaleVector[ i ].Tick_Length[j] = *Elength ;
 	                  ScaleVector[ i ].Tick_Width[j] =  *Ethick ;
 	                  ScaleVector[ i ].Tick_Increment[j][0] = *Period ;
 	                  Period  ++ ;
 	                  ScaleVector[ i ].Tick_Increment[j][1] = *Period ;
 	                  Elength ++ ;
 	                  Ethick  ++ ;
 	                  Period  ++ ;
 	                  ScaleVector[ i ].Annotation_Position[j] = 
                               Address_Of_Annotation[j].Annotation_Position;
 	                  zccase(&ScaleVector[ i ].Annotation_Position[j],1,1);

                          
 	                 /* if  Annotation required at the current tickmark */
 	                  if (( ScaleVector[ i ].Annotation_Position[j] == 'T') 
				|| 
 	                      ( ScaleVector[ i ].Annotation_Position[j] == 'C') 
				||
 	                      ( ScaleVector[ i ].Annotation_Position[j] == 'B'))
			      {
 	                      ScaleVector[i].Annotation_Start_Value[j] = 
                                Address_Of_Annotation[j].Annotation_Start_Value;
 	                      ScaleVector[ i ].Annotation_Increment[j] = 
                                Address_Of_Annotation[j].Annotation_Increment;
 	                      ScaleVector[ i ].Annotation_Modulus[j] = 
                                Address_Of_Annotation[j].Annotation_Modulus;
 	                      ScaleVector[ i ].Annotation_Significant_Digits[j]=
                              Address_Of_Annotation[j].Annotation_Significant_Digits;
 	                      ScaleVector[ i ].Annotation_Size[j] =
                                Address_Of_Annotation[j].Annotation_Size;
 	                      ScaleVector[i].Annotation_Orientation[j] = 
                                Address_Of_Annotation[j].Annotation_Orientation;
 	                      ScaleVector[i].Annotation_Justification[j]= 
                                Address_Of_Annotation[j].Annotation_Justification;
 	                      }
                          }
 	            return( 0 ) ;
 	      }
 	}
  	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xadeterminelimit, XADETERMINELIMIT) (lowdn, highdn)
 	int lowdn[MAXIMAGENUMBER], highdn[MAXIMAGENUMBER] ;
        {
 	zadeterminelimit ( lowdn, highdn );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/

 	/************************/
 	/* Return stretch limit */
 	/************************/
 	/*
 		Parameters :
 	
 			Low  DN threshold
 			High DN threshold
 	*/
 	void zadeterminelimit ( lowdn, highdn ) 
 	int lowdn[MAXIMAGENUMBER], highdn[MAXIMAGENUMBER] ;
 	{
 	
 	   DETERMINELIMIT = 1 ;
 	   xxaopeninputfiles() ;
 	   xxaapplystretch(lowdn,highdn) ;
 	   xxacloseinputfiles() ;
 	   return ;
 	}
   		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xainitialize,XAINITIALIZE) ( UMaxLines, UMaxSamples, BG )
 	int *UMaxLines, *UMaxSamples, *BG ;
        {
 	zainitialize( *UMaxLines, *UMaxSamples, BG );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/**************************/
 	/* Initialize all vectors */
 	/**************************/
 	int zainitialize( UMaxLines, UMaxSamples, BG )
 	int UMaxLines, UMaxSamples, *BG ;
 	{
 	   int i, j;
 	

  	   DETERMINELIMIT = 0 ; 	/* Flag to tell whether routine      */
					/* XADETERMINELIMIT is called or not */
  	   XALODNSTRETCH = (-1) ; 	/* Needed for application program to */
	   XAHIDNSTRETCH = (-1) ; 	/* have access to upper and lower DN */
					/* limits computed by the automatic  */
					/* linear stretch function in GMASK. */

	   XAFONTTYPE   = DEFAULT ;	/* To tell whether to use the regular */
					/* default font or the Hershey fonts  */
	   XAFONTNUMBER = DEFFONT ;	/* For Hershey fonts 		      */
	   XAFONTANGLE  = 0 ;		/* Angle of fonts		      */
   
	   unitcount = 0;		/* Counter of unique unit numbers */
	   PDSimagecount = 0;		/* Counter of unique PDS images   */

	   redirect_mask_output = FALSE; /* Flag to indicate if VICAR pdf */
					/* "OUT" parameter is to be over- */
					/* riden with a file name speci-  */
					/* via XA/ZANAMEMASKFILE.	  */

	   CharSpacing = NOOVERRIDE;	/* Set default character spacing  */

	   OutputInstance++ ;		/* Increment output instance      */

	   firstvector = NULL;		/* Set first structure pointers   */
	   firststring = NULL;		/* to NULL		          */
	   firstbox = NULL;
	   firstbuf = NULL;
	   firstellipse = NULL;
	   firsttick = NULL;
	   firsthist = NULL;
	   firstimage = NULL;
	   firstzoom = NULL;
	   firstPDSimage = NULL;
	   firstPDSzoom = NULL;
	   firstunit = NULL;
	
	   boxptr = NULL;
	   ellipseptr = NULL;
	   bufptr = NULL;
	   tickptr = NULL;
	   histptr = NULL;
	   imageptr = NULL;
	   zoomptr = NULL;
	   PDSimageptr = NULL;
	   PDSzoomptr = NULL;
	   stringptr = NULL;
	   vectorptr = NULL;
	   uptr = NULL;

	   minimageline = MAXLINESIZE;	/* Initialize minimums */
	   minzoomline = MAXLINESIZE;
	   minPDSimageline = MAXLINESIZE;
	   minPDSzoomline = MAXLINESIZE;
	   minboxline = MAXLINESIZE;
	   minbufline = MAXLINESIZE;
	   minellipseline = MAXLINESIZE;
	   minhistline = MAXLINESIZE;
	   minscaleline = MAXLINESIZE;
	   mintickline = MAXLINESIZE;
	   minstringline = MAXLINESIZE;
	   minvectorline = MAXLINESIZE;

	   for(i=0;i<Dimension;i++,BG++)
		{
 		BackgroundDn[i] = *BG;
 		if ( BackgroundDn[i] < 0 )
 	        	BackgroundDn[i] = 0 ;
 	   	if ( BackgroundDn[i] > MAXDNVALUE )
 	      		BackgroundDn[i] = MAXDNVALUE ;
 		}
  	
 	   MaskSizeLines = MaskSizeSamples = 0;
 	
 	   UserDefined = FALSE ;
 	   if ( UMaxLines >= 0 && UMaxLines <= MAXLINESIZE &&
 	        UMaxSamples >= 0 && UMaxSamples <= MAXMASKSIZE)
 	   {
 	      if ( UMaxLines > 0 && UMaxSamples > 0 )
		{
 	     	UserMaxLines = UMaxLines ;	/* Store user defined values */
 	        UserMaxSamples = UMaxSamples ;	/* if any */ 	
 	        UserDefined = TRUE ;
		}
	      else
		{
 	     	UserMaxLines = MAXMASKSIZE;	/* If UMaxLines and 	     */
 	        UserMaxSamples = MAXMASKSIZE;	/* UMaxSamples are 0, set    */
		}				/* UserMaxs to MAXLINESIZE   */
 	   }					/* until xxacalcmax. is exec.*/
 	   else
	      {
	      zvmessage("MAX LINES AND/OR SAMPLES OUT OF BOUNDS"," ");
	      zvmessage(" - GMASK JOB ABORTED"," ");
	      zabend();
	      }

	   for(i=0;i<3;i++)
	   	for(j=0;j<50;j++)
			{
			ScaleVector[ i ].Annotation_Modulus[j] = 0;
			ScaleVector[ i ].Annotation_Justification[j] = 'L';
			}
 	
 	   return( 0 ) ;
 	}
  		
/****************************************************************************
         Fortran-Callable Version
****************************************************************************/

        void FTN_NAME2(xanamemaskfile,XANAMEMASKFILE)(char *file, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
	char 	*c_string;
	int	length;

	zsfor2len(length, file, &file, 1, 1, 1, file);
	c_string = (char *)calloc(1,(length+1));
	zsfor2c(c_string, length, file, &file, 1, 1, 1, file);
	
 	zanamemaskfile( c_string );

	free ( c_string );
        }

/****************************************************************************
         C-Callable Version
****************************************************************************/

 	/*******************************************/
 	/* Redirect the mask output to a file name */ 
	/* that is specified in ZA/XANAMEMASKFILE. */
 	/*******************************************/

 	int zanamemaskfile( file )
 	char *file;
	{

	if( strlen(file) > 0 && strlen(file) < MAXFILENAME )
		{
		strcpy( mask_file_name, file );
		redirect_mask_output = TRUE;
		}
	else
		return -1;		/* Return failure status	*/

	return 0;			/* Return normal/success status	*/
	}
   		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xaoutsize, XAOUTSIZE) ( UMaxLines, UMaxSamples, BG )
  	int *UMaxLines, *UMaxSamples, *BG ;
        {
 	zaoutsize( *UMaxLines, *UMaxSamples, BG );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/***************************************/
 	/* Set Mask Size and Border Background */
 	/***************************************/
 	int zaoutsize( UMaxLines, UMaxSamples, BG )
 	int UMaxLines, UMaxSamples, *BG ;
 	{
 	   int i, j;
 
 	   MaskSizeSamples = MaskSizeLines = 0;	/* Initialize variables	*/

           for(i=0;i<Dimension;i++,BG++)
		{
 		BorderDn[i] = *BG;
 		if ( BorderDn[i] < 0 )
 			BorderDn[i] = 0 ;
 		if ( BorderDn[i] > MAXDNVALUE )
 			BorderDn[i] = MAXDNVALUE ;
 		}
 	
 	   if ( UMaxLines >= 0 && UMaxLines <= MAXLINESIZE &&
 	        UMaxSamples >= 0 && UMaxSamples <= MAXMASKSIZE )
 	   {
 	      if ( UMaxLines > 0 && UMaxSamples > 0 )
		{
 	     	MaskSizeLines = UMaxLines ;	/* Store user defined values */
 	        MaskSizeSamples = UMaxSamples ;	/* if any */
		}
	      else
		{
 	     	MaskSizeLines = MAXMASKSIZE;	/* If UMaxLines and 	     */
 	        MaskSizeSamples = MAXMASKSIZE;	/* UMaxSamples are 0, set    */
		}				/* UserMaxs to MAXLINESIZE   */
	       
	      if ( UserMaxLines > MaskSizeLines )
		UserMaxLines = MaskSizeLines;

	      if ( UserMaxSamples > MaskSizeSamples )
		UserMaxSamples = MaskSizeSamples;
 	   }					/* until xxacalcmax. is exec.*/
 	   else
	      return( -1 );			/* Return error code	     */

 	   return( 0 ) ;
 	}
  	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xasetcharspacing, XASETCHARSPACING) ( Spacing )
 	int *Spacing;
         {
 	 zasetcharspacing( *Spacing );
         }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/

 	/**************************/
 	/* Set Character Spacing  */
 	/**************************/
 	int zasetcharspacing( Spacing )
 	int Spacing;
 	{  
              /* can't use CharSpacing as the formal parameter in this routine,
                 because CharSpacing is a global variable, its value needs to
                 be retained outside this routine                           */

           CharSpacing = Spacing;
  	   if(CharSpacing < -1)
 		{
 		zvmessage("Error in user specified character spacing - "," ");
 		zvmessage("	ZA/XASETCHARSPACING ignored."," ");
 		CharSpacing = NOOVERRIDE;
		return ( -1 );
 		}
	   return ( 0 );
 	}
  		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xasetdim, XASETDIM) ( Dim)
 	int *Dim;
        {
 	zasetdim( *Dim);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/***************************/
 	/* Set Dimension of Output */
 	/***************************/
 	int zasetdim( Dim)
 	int Dim;
 	{
 	   int i;
         
 	   Dimension = Dim;
  	   if(Dimension!=1 && Dimension!=3)
 		{
 		zvmessage("Error in dimension declared, dimension set to 1"," ");
 		Dimension = 1;
 		}
	   return( 0 );
 	}
  		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xasetfont, XASETFONT) (char FontType[20], int *FontNum,
								ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char c_string[20];
        int length;

        length = 20;
        zsfor2c(c_string, length, FontType, &FontType, 2, 1, 1, FontNum); 
 	zasetfont( c_string, *FontNum );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	/*****************/
 	/* Set Font Type */
 	/*****************/
 	/*
 	Parameters are :
 	
 		Font type (DEFAULT,HERSHEY)
 		Font number (Hershey font number)
 	*/
 	int zasetfont( FontType, FontNum )
        char FontType[ 20 ] ;
 	int FontNum ;
 	{
  	   if ( strcmp( FontType, "HERSHEY" ) == 0 )
 	   {
 	      XAFONTTYPE = HERSHEY ;
 	      XAFONTNUMBER = FontNum ;
 	   }
 	   else
 	   if ( strcmp( FontType, "DEFAULT" ) == 0 )
 	   {
 	      XAFONTTYPE = DEFAULT ;
 	      XAFONTNUMBER = -1 ;
 	   }
 	   else
 	      return ( -1 ) ;
 	}
  		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xasetfontangle, XASETFONTANGLE) ( Angle )
 	int *Angle ;
        {
 	zasetfontangle( *Angle );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	/******************/
 	/* Set Font Angle */
 	/******************/
 	/*
 	   Parameters are :
 	
 	      Font Angle (-90,0,90)
 	*/
 	int zasetfontangle( Angle )
 	int Angle ;
 	{
 	   int OldAngle ;
 	
 	   OldAngle = XAFONTANGLE ;
 	
 	   if ( Angle == 0 || Angle == 90 || Angle == -90 )
 	      XAFONTANGLE = Angle ;
 	   else
 	      return( -1 ) ;
 	
 	   return( OldAngle) ;
 	}
  		
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xastorebox,XASTOREBOX)( StLine, StSample, Wid, Len, DnV )
	int *StLine, *StSample, *Wid, *Len, *DnV;
         {
         zastorebox( *StLine, *StSample, *Wid, *Len, DnV );
         }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	/**************************/
 	/* Store Gray Scale Boxes */
 	/**************************/
 	/*
 	Parameters are :
 	
 		Start line
 		Start sample
 		Width
 		Length
 		Dn value or Dn array for color
 	*/
 	int zastorebox( StLine, StSample, Wid, Len, DnV )
 	int StLine, StSample, Wid, Len, *DnV;
 	{
  	   struct BoxType *newblock;
 	   int dm, DnVal[3] ;
           int DN ; 

           for(dm=0;dm<Dimension;dm++,DnV++)
		{
		DnVal[dm] = *DnV ;
	
		/* If we get negative values Then error */
		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
 		}
 	
 	   if ( StLine < 0 ) 
 		StLine = 0 ;
 	 
 	   if ( StSample < 0 ) 
 	      	StSample =  0 ;
 	   else  
 		if ( StSample > UserMaxSamples) 
 	      		StSample = UserMaxSamples ;
 	
	   /* If we get negative values Then error */
 	   if ( Wid <= 0 || Len <= 0)
 	  	return( -1 ) ;    
 	
 	   /* If we run out of space on the mask Then error */
 	
 	   if ( StSample+Wid >= UserMaxSamples )
 	      	return( -2 ) ;
 	
 	   newblock = (struct BoxType *)malloc( sizeof (struct BoxType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for boxes"," ");
 		zabend();
 		}
 	   if( !boxptr )
		{
 		firstbox = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		boxptr->next = newblock;
		newblock->previous = boxptr;
		}
 	   newblock->next = NULL;
 	   boxptr = newblock;
 	   boxptr->StartLine = StLine ;	/* Store "good" data */
 	   boxptr->StartSample = StSample ;
 	   boxptr->Width = Wid ;
 	   boxptr->Length = Len ;
 	   for(dm=0;dm<Dimension;dm++)
 		 boxptr->Dn[dm] = DnVal[dm] ;
 	   boxptr->EndLine = StLine + Len - 1 ;

	   /* Recompute minimum and maximum start lines 		  */
	   minboxline = Min( boxptr->StartLine, minboxline );

 	   return( 0 ) ;
 	}
  		
 	/**************************/
 	/* Store Ellipses 	  */
 	/**************************/
 	/*
 	Parameters are :
 	
 		Center line
 		Center sample
 		Major axis radius
 		Minor axis radius
		Width of line
 		Dn value or Dn array for color
 	*/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
	void FTN_NAME2(xastorellipse, XASTORELLIPSE) ( CtLine, CtSample,
							R1, R2, Wid, DnV )
 	int *CtLine, *CtSample, *R1, *R2, *Wid, *DnV;
        {
	zastorellipse( *CtLine, *CtSample, *R1, *R2, *Wid, DnV );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
	int zastorellipse( CtLine, CtSample, R1, R2, Wid, DnV )
 	int CtLine, CtSample, R1, R2, Wid, *DnV;
 	{
  	   struct EllipseType *newblock;
 	   int 	dm, I1, I2, DnVal[3] ;

	   int 	count[4],**delta,denominator,line,oldwidth,width,FILL; 	
	   float **smooth;
	   int	p,v,w,x,xc,y,yc,z;
	   int  ones[4],onesets;
	   int  zeroes[4],zerosets;
	
           for(dm=0;dm<Dimension;dm++,DnV++)
		{
	    	DnVal[dm] = *DnV ;
			
		/* If we get negative values Then error */
		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
 		}
 	
 	   if ( CtLine-R2 < 0 ) 
 		CtLine = R2 + 1 ;
 	 
 	   if ( CtSample-R1 < 0 ) 
 	      	CtSample =  R1 + 1 ;
 	   else  
 		if ( CtSample > UserMaxSamples) 
 	      		CtSample = UserMaxSamples - R1 - 1 ;
 	
	   /* If we get negative values Then error */
 	   if ( Wid <= 0 || R1 <= 0 || R2 <= 0 )
 	  	return( -1 ) ;    
 	
 	   /* If we run out of space on the mask Then error */
 	   if ( CtSample+R1 >= UserMaxSamples )
 	      	return( -2 ) ;
	
	   /* If width of arc is greater than R1 or R2, then FILL */
	   if ( Wid >= R2 || Wid >= R1 )
		FILL = TRUE;
	   else
		{
		FILL = FALSE;
		I1 = R1 - Wid + 1 ;
		I2 = R2 - Wid + 1 ;
		}
 	
 	   newblock = (struct EllipseType *)malloc( sizeof (struct EllipseType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for ellipses"," ");
 		zabend();
 		}
 	   if( !ellipseptr )
		{
 		firstellipse = newblock;
		newblock->previous = NULL;
		minellipseline = CtLine - R2 - 1;
		}
 	   else
		{
 		ellipseptr->next = newblock;
		newblock->previous = ellipseptr;
		}
 	   newblock->next = NULL;
 	   ellipseptr = newblock;
 	   ellipseptr->CenterLine = CtLine ;	/* Store "good" data */
 	   ellipseptr->CenterSample = CtSample ;
 	   ellipseptr->Radius1 = R1 ;
 	   ellipseptr->Radius2 = R2 ;
 	   ellipseptr->Width = Wid ;
 	   for(dm=0;dm<Dimension;dm++)
 		 ellipseptr->Dn[dm] = DnVal[dm] ;
	
	   ellipseptr->Edge 	= (int **)calloc(4,sizeof(int *));
	   if( ellipseptr->Edge == NULL )
		{
		zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
		zvmessage(" - Ellipse not stored"," ");
		return( -2 );
		}
	   ellipseptr->Smooth 	= (float **)calloc(4,sizeof(float *));
	   if( ellipseptr->Smooth == NULL )
		{
		zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
		zvmessage(" - Ellipse not stored"," ");
		return( -2 );
		}
	   smooth = (float **)calloc(4,sizeof(float *));
	   if( smooth == NULL )
		{
		zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
		zvmessage(" - Ellipse not stored"," ");
		return( -2 );
		}
	   delta = (int **)calloc(4,sizeof(int *));
	   if( delta == NULL )
		{
		zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
		zvmessage(" - Ellipse not stored"," ");
		return( -2 );
		}
	   for( z=0; z<4; z++ )
		{
		ellipseptr->Edge[z]   	= (int *)calloc(R2*2+1,sizeof(int));
	        if( ellipseptr->Edge[z] == NULL )
			{
			zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
			zvmessage(" - Ellipse not stored"," ");
			return( -2 );
			}
	   	ellipseptr->Smooth[z] 	= (float *)calloc(R2*2+1,sizeof(float));
	        if( ellipseptr->Smooth[z] == NULL )
			{
			zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
			zvmessage(" - Ellipse not stored"," ");
			return( -2 );
			}
	   	smooth[z] 		= (float *)calloc(R2*2+1,sizeof(float));
	        if( smooth[z] == NULL )
			{
			zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
			zvmessage(" - Ellipse not stored"," ");
			return( -2 );
			}
	   	delta[z] 		= (int *)calloc(R2*2+1,sizeof(int));
	        if( delta[z] == NULL )
			{
			zvmessage(" ZA/XASTORELLIPSE memory allocation failure"," ");
			zvmessage(" - Ellipse not stored"," ");
			return( -2 );
			}
		}

	   if( R2 == R1 )
		{
		x = line = 0;
		y = R2;
		p = 3 - 2 * R2;
	   	while(  x < y  )
			{
			if( p < 0 )
				{
				p += 4 * x + 6;
				ellipseptr->Edge[0][line] = CtSample - x;
				ellipseptr->Edge[3][line] = CtSample + x;
				ellipseptr->Edge[0][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[3][R2*2 - line] = CtSample + x;
				}
			else
				{
				p += 10 + 4 * ( x - y );
				y -= 1;
				line++;
				ellipseptr->Edge[0][line] = CtSample - x;
				ellipseptr->Edge[3][line] = CtSample + x;
				ellipseptr->Edge[0][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[3][R2*2 - line] = CtSample + x;
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				}
			x += 1;
			}
 		line++;
		p = 2*R2*R2 - 2*(y-1)*(y-1) - x*x - (x+1)*(x+1);
		while ( x <= R2 && line<=R2+1 )
			{
			if( p < 0 )
				{
				p += 4*y - 6;
				ellipseptr->Edge[0][line] = CtSample - x;
				ellipseptr->Edge[3][line] = CtSample + x;
				ellipseptr->Edge[0][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[3][R2*2 - line] = CtSample + x;
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				}
			else
				{
				p += 4 * (y - x) - 10;
				ellipseptr->Edge[0][line] = CtSample - x;
				ellipseptr->Edge[3][line] = CtSample + x;
				ellipseptr->Edge[0][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[3][R2*2 - line] = CtSample + x;
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				x += 1;
				}
			line++;
			y -= 1;
			}

		if( FILL )
		    for( y=0; y<=R2; y++ )
			{
			ellipseptr->Edge[1][y] = 0;
			ellipseptr->Edge[2][y] = 0;
			ellipseptr->Edge[1][R2*2 - y] = 0;
			ellipseptr->Edge[2][R2*2 - y] = 0;
			}
		else
		    if( Wid < R1 && Wid < R2 && Wid!=1 )
			{
	   		x = 0;
			line = Wid;
			for( y=0; y<line; y++ )
				{
				ellipseptr->Edge[1][y] = 0;
				ellipseptr->Edge[2][y] = 0;
				ellipseptr->Edge[1][R2*2 - y] = 0;
				ellipseptr->Edge[2][R2*2 - y] = 0;
				}
	   		y = I2;
	   		p = 3 - 2 * I2;
			while( x < y )
				{
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				if( p < 0 )
				   {
				   p += 4 * x + 6;
				   ellipseptr->Edge[1][line] = CtSample - x;
				   ellipseptr->Edge[2][line] = CtSample + x;
				   ellipseptr->Edge[1][R2*2 - line] 
					= CtSample - x;
				   ellipseptr->Edge[2][R2*2 - line] 
					= CtSample + x;
				   }
				else
				   {
				   p += 10 + 4 * ( x - y );
				   line++;
				   ellipseptr->Edge[1][line] = CtSample - x;
				   ellipseptr->Edge[2][line] = CtSample + x;
				   ellipseptr->Edge[1][R2*2 - line] 
					= CtSample - x;
				   ellipseptr->Edge[2][R2*2 - line] 
					= CtSample + x;
				   y -= 1;
				   }
				x += 1;
				}
 			line++;
			p = 2*I2*I2 - 2*(y-1)*(y-1) - x*x - (x+1)*(x+1);
			while ( x <= I2 && line <= R2+1 )
			   {
			   if( p < 0 )
				{
				p += 4 * y - 6;
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				}
			   else
				{
				p += 4 * (y - x) - 10;
				ellipseptr->Edge[1][line] = CtSample - x;
				ellipseptr->Edge[2][line] = CtSample + x;
				ellipseptr->Edge[1][R2*2 - line] = CtSample - x;
				ellipseptr->Edge[2][R2*2 - line] = CtSample + x;
				x += 1;
				}
			   line++;
			   y -= 1;
			   }
		  	}
		line = R2*2 + 1;
		}
	   else 
		{
		for( y=R2-1,line=0; y>=0; y--,line++ )
			{
			if( line != 0 )
				oldwidth = (float)(R1*sqrt(1.0
					-pow((double)(y+1.05)/R2,2.0)))+0.5;
			width = 
			(float)(R1*sqrt(1.0-pow((double)(y+0.05)/R2,2.0)))+0.5;
			ellipseptr->Edge[0][line] = CtSample - width; 
			ellipseptr->Edge[3][line] = CtSample + width; 
			ellipseptr->Edge[0][2*R2-line] = CtSample - width; 
			ellipseptr->Edge[3][2*R2-line] = CtSample + width; 
			if( line > 0 )
			  if( !FILL )
			    {
			    if( Wid == 1 )
			      {
			      if( oldwidth != width )
				{
				ellipseptr->Edge[1][line] 
					= CtSample - oldwidth - 1; 
				ellipseptr->Edge[2][line] 
					= CtSample + oldwidth + 1;
				ellipseptr->Edge[1][2*R2-line] 
					= CtSample - oldwidth - 1; 
				ellipseptr->Edge[2][2*R2-line] 
					= CtSample + oldwidth + 1;
				}
			      else
				{
				ellipseptr->Edge[1][line] = CtSample - width; 
				ellipseptr->Edge[2][line] = CtSample + width;
				ellipseptr->Edge[1][2*R2-line] 
					= CtSample - width; 
				ellipseptr->Edge[2][2*R2-line] 
					= CtSample + width;
				}
			      }
			    else
				{
				width = (float)(I1*sqrt(1.0
					-pow((double)(y+0.05)/I2,2.0))) + 0.5;
				ellipseptr->Edge[1][line] = CtSample - width; 
				ellipseptr->Edge[2][line] = CtSample + width;
				ellipseptr->Edge[1][2*R2-line] 
					= CtSample - width; 
				ellipseptr->Edge[2][2*R2-line] 
					= CtSample + width;
				}
			    }
			}
		ellipseptr->Edge[0][R2] = CtSample - R1; 
		ellipseptr->Edge[3][R2] = CtSample + R1; 
		ellipseptr->Edge[1][R2] = CtSample - I1; 
		ellipseptr->Edge[2][R2] = CtSample + I1; 
		line = 2*R2 + 1;
		}

	   for( z=0; z<4; z++ )		/* Set delta at equator */	
		count[z] = 1;

	   for( v=0; v<line/2; v++ )	/* Calculate SAMPLE_DELTA */
	     for( z=0; z<4; z++ )
		{
		delta[z][v]
			= abs(ellipseptr->Edge[z][v+1]-ellipseptr->Edge[z][v]);

		if( delta[z][v] > 1 )
			{
			ellipseptr->Smooth[z][v] = delta[z][v];
			if( count[z] > 1 )
				{
				ellipseptr->Smooth[z][v] = count[z];	
				count[z] = 1;
				}
			}
		else
			{
			if( delta[z][v] == 1 || ( v == line/2 - 1 && ( z==1 || z==2 ) ) )
				{
				if( count[z] > 1 )
					{
					denominator = count[z] + 1;
				        if( z==0 || z==3 )
					  for( w=0; w<count[z]; w++ )
					   {
					   ellipseptr->Smooth[z][v-w] 
					   = (float)(count[z]-w)/denominator;	
					   ellipseptr->Smooth[z][v-w] =
					   pow(ellipseptr->Smooth[z][v-w],2.0);
					   }
					else
					  for( w=0; w<count[z]; w++ )
					   {
					   ellipseptr->Smooth[z][v-w-1] 
					   = (float)(w+1)/denominator;	
					   ellipseptr->Smooth[z][v-w-1] =
					   pow(ellipseptr->Smooth[z][v-w-1],
					   2.0);
					   }
					count[z] = 1;
					}
				else
					ellipseptr->Smooth[z][v] = 0.5;
				}
			else
				{
				ellipseptr->Smooth[z][v] = 0;
				count[z] += 1;
				}
			}
		if( ellipseptr->Edge[z][v]==0 )
			ellipseptr->Smooth[z][v] = 0;
		}

	   /* Shift inner smoothing values down */

	   for(v=0;v<line/2;v++)	
	      for(z=1;z<3;z++)
		smooth[z][v+1] = ellipseptr->Smooth[z][v]; 
	  
           /* Smooth top & bottom of inner circle	*/
	   if( Wid != 1 && R1!=R2 )
	      for(z=1;z<3;z++)
	  	smooth[z][Wid-1] = 
		abs(ellipseptr->Edge[2][Wid-1]-ellipseptr->Edge[1][Wid-1])/2-1;	
	   else
	      for(z=1;z<3;z++)
	 	smooth[z][Wid] = 
		abs(ellipseptr->Edge[2][Wid]-ellipseptr->Edge[1][Wid])/2 - 1;	

	   for(v=0;v<line/2;v++)	
	      for(z=1;z<3;z++)
		if( ellipseptr->Edge[z][v]==0 )
			ellipseptr->Smooth[z][v] = 0;
		else
			ellipseptr->Smooth[z][v] = smooth[z][v]; 

	   /* Copy top smoothing values to the bottom		*/
	   for(v=0;v<line/2;v++)
	      for(z=0;z<4;z++)
		ellipseptr->Smooth[z][line-1-v] = ellipseptr->Smooth[z][v];

	   /* Recompute minimum and maximum start lines 		  */
	   minellipseline = Min( CtLine - R2, minellipseline );

 	   return( 0 ) ;
 	}

   		
	/****************/
 	/* Store buffer */
 	/****************/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastorebuffer, XASTOREBUFFER) (unsigned char Buf[],
		int *StLine, int *StSample, int *EnLine, int *EnSample, 
                int *OutStLine, int *OutStSample, int *MagFactor,
		char BWRGB[10], ZFORSTR_PARAM)
#if 0
        char 	BWRGB[10];		/* Color or BW indicator     */
#endif
        {
	ZFORSTR_BLOCK
        char c_string[10];
        int length;

        length = 10; 
        zsfor2c(c_string, length, BWRGB, Buf, 9, 9, 1, BWRGB);

	zastorebuffer(Buf, *StLine, *StSample, *EnLine, *EnSample, *OutStLine, 
                      *OutStSample, *MagFactor, c_string);

        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	
 	int zastorebuffer(Buf, StLine, StSample, EnLine, EnSample, OutStLine, 
                      OutStSample, MagFactor, BWRGB )
 	int StLine, StSample, EnLine, EnSample, OutStLine, OutStSample, 
            MagFactor;
        char 	BWRGB[10];		/* Color or BW indicator     */
 	unsigned char Buf[];
 	{
  	   struct BufType *newblock;

		
           int          OutEndLine;
 	   int 		j, k;			/* Loop control variable     */
 	
  	   /* If we get negative values Then error */
 	   if ( StLine   < 0 ||
 	        StSample < 0 || StSample > UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        OutStLine < 0 ||
 	        OutStSample < 0 || OutStSample > UserMaxSamples ||
		(BWRGB[0] != 'b' && BWRGB[0] != 'B' && 
		 BWRGB[0] != 'c' && BWRGB[0] != 'C' ))
		return( -1 );

 	   newblock = (struct BufType *)malloc(sizeof (struct BufType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for zoomed image"," ");
 		zabend();
 		}
 	   if( !bufptr )
		{
 		firstbuf = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		bufptr->next = newblock;
		newblock->previous = bufptr;
		}
 	   newblock->next = NULL;
 	   bufptr = newblock;
 	
 	   /* Store with Start values < End values */
 	   bufptr->StartLine = Min( StLine, EnLine )  ;
 	   bufptr->StartSample = Min( StSample, EnSample ) ;
 	   bufptr->EndLine = Max( StLine, EnLine ) ;
 	   bufptr->EndSample = Max( StSample, EnSample ) ;
 	   bufptr->OutStartLine = OutStLine ;
 	   bufptr->OutStartSample = OutStSample ;
 	   bufptr->MagFactor = MagFactor;	
	   strcpy( bufptr->Display, BWRGB );

	   bufptr->NumSamp = bufptr->EndSample - bufptr->StartSample + 1;
	   bufptr->NumLines = bufptr->EndLine - bufptr->StartLine + 1;
	   bufptr->OutSamp = bufptr->OutStartSample - 1 ;
	   bufptr->Samp = bufptr->StartSample - 1 ;
	   bufptr->Band = 0;		/* Set Band to zero for BW default */
	 
	
	   if( bufptr->Display[0] != 'B' && bufptr->Display[0] != 'b' )
		{
	    	bufptr->Band = bufptr->NumLines * bufptr->NumSamp;
           	bufptr->buffer = (unsigned char *)calloc
			    (3*bufptr->Band,sizeof(unsigned char));
     	        if( bufptr->buffer == NULL )
			{
			zvmessage("Memory allocation error"," ");
			zvmessage(" - za/xastorebuffer aborted."," ");
			bufptr->previous->next = NULL;
			return( -1 );
			}
	   	for( j=0; j<(3*bufptr->Band); j++ )
			bufptr->buffer[j] = Buf[j];
		}
	   else
		{
		bufptr->buffer = (unsigned char *)calloc
		     (bufptr->NumLines*bufptr->NumSamp,sizeof(unsigned char));
     	        if( bufptr->buffer == NULL )
			{
			zvmessage("Memory allocation error"," ");
			zvmessage(" - za/xastorebuffer aborted."," ");
			bufptr->previous->next = NULL;
			return( -1 );
			}
		for( j=0; j<bufptr->NumLines*bufptr->NumSamp; j++ )
			bufptr->buffer[j] = Buf[j];
		}	

	   /* Compute multiplicative magnification factor 	*/
 	   bufptr->MulFactor = bufptr->MagFactor;
 	   if(bufptr->MagFactor < 0)
 		bufptr->MulFactor = (-1 * (1.0/bufptr->MagFactor));
	   if( bufptr->MagFactor == 1 || bufptr->MagFactor == -1 )
		bufptr->MagFactor = 1;

	   /* Recompute minimum start lines 		  	*/
	   minbufline = Min( bufptr->OutStartLine, minbufline );

 	   if(MagFactor>0)
 		{	
 	   	bufptr->Input = (unsigned char **)calloc
 				(Dimension,sizeof(unsigned char *));
     	        if( bufptr->Input == NULL )
			{
			zvmessage("Memory allocation error"," ");
			zvmessage(" - za/xastorebuffer aborted."," ");
			bufptr->previous->next = NULL;
		 	return( -1 );
			}

 	   	for(k=0; k<Dimension; k++)
 			{
 			bufptr->Input[k] = (unsigned char *)calloc
			  (bufptr->NumSamp*MagFactor,sizeof(unsigned char));
	     	        if( bufptr->Input[k] == NULL )
				{
				zvmessage("Memory allocation error"," ");
				zvmessage(" - za/xastorebuffer aborted."," ");
				bufptr->previous->next = NULL;
				return( -1 );
				}
 			}
 		}

 	   return ( 0 );
 	}
 	
  		
 	/****************************************/
 	/* Store Continuous Gray Scale Shadings */
 	/****************************************/
 	
 	/*
 	Parameters are :
 	
 		Start line
 		Start sample
 		Width
		Length
 		Starting Dn
 		Ending Dn
 		Wedge Type
 	
 	Algorithm :
 	

	   If (negative or out of bounds values in parameters) Then
	      Return out of bounds error
   
	   If ( horizontal gray scale ) Then
	   Begin
	      If ( continuous scale type ) Then
		If ( there is a large sample range and small dn range ) Then
       		  Distribution = sample difference / dn difference
      	        Else
	        If ( there is a small sample range and a large dn range ) Then
        	  Distribution = dn difference / sample difference
      	      Else
	      	Draw 16 separate gray scale boxes with separators
   	   End
	   Else
 	   If ( vertical gray scale ) Then
   	   Begin
	      If ( continuous scale type ) Then
		If ( there is a large line range and small dn range ) Then
 	      	  Distribution = line difference / dn difference
	        Else
   	        If ( there is a small line range and a large dn range ) Then
      		   Distribution = dn difference / line difference
 	   Else
	      Draw 16 separate gray scale boxes with separators
	   End

 	*/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastoregray, XASTOREGRAY) ( int *StLine, int *StSample,
		int *Width, int *Length, int *StDn, int *EnDn,
		char *ScaleType, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char *c_string;
        int length;

        zsfor2len(length, ScaleType, &StLine, 7, 7, 1, ScaleType);
        c_string = (char *)calloc(1,(length+1));	     
        zsfor2c(c_string, length, ScaleType, &StLine, 7, 7, 1, ScaleType); 
 	zastoregray( *StLine, *StSample, *Width, *Length, *StDn, *EnDn, 
                     c_string[0]);
        free(c_string);
        }
 	
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastoregray( StLine, StSample, Width, Length, StDn, EnDn, ScaleType )
 	int StLine, StSample, Width, Length, StDn, EnDn ;
        unsigned char ScaleType;
 	{
	   int BoxWidth ;	/* Width of Step wedge boxes	*/

	   unsigned int white[3] ;
           int black[3] ;
 	
 	   int d, i ;		/* Loop control variables	*/
 	   int SampDiff ;	/* Sample difference 		*/
 	   int LineDiff ;	/* Line # difference 		*/
 	   int DnDiff ;		/* Dn difference 		*/
 	   int Sign ;		/* +-1 				*/
 	   float Offset ;	/* Sample difference / Dn difference 	*/
 	   int CurrentDn[3] ;   /* Current color for parameter passing 	*/
 	   int CurrDn ;         /* Current Dn for parameter passing 	*/
 	   float f ;		/* For conversion from int to float 	*/
 	
 	   /* If we get negative values Then error 	*/
 	
 	   if ( StLine   < 0 || StLine > UserMaxLines     ||
 	        StSample < 0 || StSample > UserMaxSamples ||
 	        Width   <= 0 || Length <= 0  		  ||
 	        StDn     < 0 || StDn > MAXDNVALUE 	  ||
 	        EnDn     < 0 || EnDn > MAXDNVALUE )
 	      	return( -1 ) ;
	
 	   if (  ScaleType != 'c' && ScaleType != 'C' &&
                 ScaleType != 's' && ScaleType != 'S'  )	 
		return( -1 );

           white[0] = 255;
           white[1] = 255;
           white[2] = 255;

	   memset(black,   0, 4*3) ;

 	   if ( Width > Length )		/* Horizontal case */
	     	{
	     	if( ScaleType == 'c' || ScaleType == 'C' )
 	      	      {
	 	      DnDiff = EnDn - StDn ;

 		      if ( DnDiff == 0 )	/* Case Start Dn = End Dn */
 		      	Sign = 1 ;
 		      else
 		        Sign = DnDiff / abs( DnDiff ) ;
 	
 		      f = abs( DnDiff ) + 1 ;	/* Make sure Offset is float */
 		      Offset = f / Width ;
 	
 		      for ( i = 0 ; i < Width ; i++ )
 		        {
			  for(d = 0; d < Dimension; d++)
			      CurrentDn[d] = StDn + (Sign*i*Offset); 
			      zastorebox(StLine,StSample+i,1,Length,CurrentDn);
 		        }
		      }
		else
		      {
		      BoxWidth = (Width-30)/16;	/* Determine the width / box  */

		      DnDiff = (EnDn-StDn)/15;	/* Change in grey scale / box */
                      
                      for (i=0; i<15; i++)
                          {  
		          for( d=0; d<Dimension; d++ )	/* Calculate grey level       */
			       CurrentDn[d] = StDn + (i*DnDiff);
		          zastorebox(StLine,StSample,BoxWidth,
			      BoxWidth,CurrentDn );	
		          StSample += BoxWidth; /* Increment sample 	     */
		          if( CurrentDn[0] < 120 )
			      zastorebox( StLine,StSample,2,BoxWidth,white );
			  else
			      zastorebox( StLine,StSample,2,BoxWidth,black );
			  StSample += 2;	
		          }	

		      for( d=0; d<Dimension; d++ )	/* Calculate grey level       */
		  	  CurrentDn[d] = StDn + (i*DnDiff);
	 	      zastorebox( StLine,StSample,BoxWidth,
		    	            BoxWidth,CurrentDn );
                      }
		}
 	   else
	   	if( ScaleType == 'c' || ScaleType == 'C' ) /* Vertical case  */
		      { 
	 	      DnDiff = EnDn - StDn ;

	 	      if ( DnDiff == 0 )	/* Case Start Dn = End Dn */
 		         Sign = 1 ;
 		      else
 		         Sign = DnDiff / abs( DnDiff ) ;
 		
 		      f = abs( DnDiff ) + 1 ;
 		      Offset = f / Length ;	/* Make sure Offset is float */
 		
 		      for ( i = 0 ; i < Length ; i++ )
 		         {
 		       	     for(d = 0; d < Dimension; d++)
 			 	CurrentDn[d] = StDn + ( Sign * i * Offset ); 
 			     zastorebox(StLine+i,StSample,Width,1,CurrentDn) ;
 	   	    	 }
       		      }
		   else
		      {
		      BoxWidth = (Length-30)/16;
	
		      DnDiff = (EnDn-StDn)/15;	/* Change in grey scale / box */
 		
		      for( i=0; i<15; i++)	/* GENERATE GREY SCALE WEDGE  */
			   {
			   for( d=0; d<Dimension; d++ )	/* Calculate grey level       */
				CurrentDn[d] = StDn + (i*DnDiff);
			   zastorebox(StLine,StSample,BoxWidth,
				      BoxWidth,CurrentDn );	
			   StLine += BoxWidth; 	/* Increment line	     */
			   if( CurrentDn[0] < 120 )
			      zastorebox( StLine,StSample,BoxWidth,2,white );
			   else
			      zastorebox( StLine,StSample,BoxWidth,2,black );
		     	   StLine += 2;		/* Increment line    	     */
			   }	

			for( d=0; d<Dimension; d++ )	/* Calculate grey level       */
				CurrentDn[d] = StDn + (i*DnDiff);
		 	zastorebox(StLine,StSample,BoxWidth,BoxWidth,CurrentDn);
 	      	      }
	    return(0);
 	}
  		
 	/*********************/
 	/* Store a histogram */
 	/*********************/
 	/*
 	Parameters :
 	
	   Histogram to be displayed 	
 	   Histogram start line
 	   Histogram start sample
 	   Histogram end line
 	   Histogram end sample
	   Scale code for desired type of scaling of histogram (IOF/Radiance/DN)
 	   Vertical or Horizontal histogram and Minimum or Blend flag 
	   Number of bins of histogram to be interpolated for smoothness
 	   Dn value or Dn array for color
 	*/
 	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastorehistogram,XASTOREHISTOGRAM)(unsigned int Hist[256],
		int *HiStLine, int *HiStSample, int *HiEnLine, int *HiEnSample,
		unsigned int *MAXDN, char VHmb[4], int *DnV, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char c_string[4];
        int length;

        length = 4;
        zsfor2c(c_string, length, VHmb, Hist, 8, 8, 1, DnV);
 	zastorehistogram( Hist, *HiStLine, *HiStSample, *HiEnLine, *HiEnSample,
                          *MAXDN, c_string, DnV );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastorehistogram( Hist, HiStLine, HiStSample, HiEnLine, HiEnSample,
                          MAXDN, VHmb, DnV )
  	int HiStLine, HiStSample, HiEnLine, HiEnSample, *DnV ;
 	unsigned int Hist[256], MAXDN;
	char	VHmb[4];
 	{
 	   struct HistType *newblock;
 	
 	   char 	StretBand;
	   float 	fMaxDnValue = MAXDNVALUE ;
 	   float 	ftemp, Frac, Satur, temp1, temp2 ;
 	   int 		DnVal[3], bytes;
	   int  	count, difference, dm,  number_bins, 
			IOStatus, j, point, *rgb, temp,	zerocount, len;

	   /* Check for color or black/white mode 		*/
 	   for(dm=0;dm<Dimension;dm++,DnV++)
 		{
		DnVal[dm] = *DnV ;
 		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
 		}

 	   /* If we get negative values or illegal VorH values Then error */
 	   if ( HiStLine   < 0 ||
 	        HiStSample < 0 || HiStSample >= UserMaxSamples ||
 	        HiEnLine   < 0 ||
 	        HiEnSample < 0 || HiEnSample > UserMaxSamples ||
 	   	( VHmb[0]!='V' && VHmb[0]!='v' && VHmb[0]!='H' && VHmb[0]!='h' )
		|| ( VHmb[1] != 'M' && VHmb[1] != 'm'
		&& VHmb[1] != 'B' && VHmb[1] != 'b' && VHmb[1] != '\0' ) )
 	      return( -1 ) ;
 	
	   /* Check for scaling factor; if zero, retrieve appropriate scale from
	      the flight label of the respective image input file */

 	   newblock = (struct HistType *)malloc(sizeof (struct HistType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for histogram"," ");
 		zabend();
 		}
 	   if( !histptr )
		{
 		firsthist = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		histptr->next = newblock;
		newblock->previous = histptr;
		}
 	   newblock->next = NULL;
 	   histptr = newblock;

           /* Bins are interpolated and filled. 	 	*/	

           len = strlen(VHmb);
           if (len > 2)
              {
              if( VHmb[2] == 'i' || VHmb[2] == 'I' )
	        {
	        number_bins = atoi( &VHmb[3] );
	        for(zerocount = -1, count = 0; count <= MAXDNVALUE; count++)
		   {
	   	   if( Hist[count] == 0 )
		      {
		      if( zerocount != -1 )
		   	  zerocount++;
		      }
		   else
		        {
			if( zerocount > 0 && zerocount <= number_bins )
			   {
			   difference = Hist[count] - Hist[count-zerocount-1];	
			   difference /= (zerocount+1);
			   for( point=1; point<=zerocount; point++ )
			      Hist[count-point] = Hist[count]-difference*point;
			   }
			zerocount = 0;
			}	
		   }
                }
              }

	   for(count=0;count<=MAXDNVALUE;count++)
		histptr->Histogram[count] = Hist[count];

	   histptr->HistStLine = Min( HiStLine, HiEnLine ) ;
 	   histptr->HistStSample = Min( HiStSample, HiEnSample ) ;
 	   histptr->HistEnLine = Max( HiStLine, HiEnLine ) ;
 	   histptr->HistEnSample = Max( HiStSample, HiEnSample ) ;
	   histptr->MaxPixelValue = MAXDN;
	   histptr->ColorBands = 0;
	   histptr->Stretch = NoStretch;
	
	   /* Recompute minimum and maximum start lines 		  */
	   minhistline = Min( histptr->HistStLine, minhistline );
	   
 	   for(dm=0;dm<Dimension;dm++)
 		histptr->Dn[dm] = DnVal[dm] ;

	   /* Determine the type of multihistogram display */
   	   histptr->Display = None;
	   if ( Dimension != 1 )
		{
           	if ( VHmb[1] == 'm' || VHmb[1] == 'M' )
			histptr->Display = Minimum;
	   	else
			if ( VHmb[1] == 'B' || VHmb[1] == 'b' )
				histptr->Display = Blend;
			else
				histptr->Display = None;
		}

 	   if ( VHmb[0] == 'V' || VHmb[0] == 'v' )	/* Vertical histogram */
 	   	{
 	        histptr->Direction = Vert ;

		/* Determine scaling factors for histogram display */
	        ftemp = MAXDNVALUE + 1 ;
 	        histptr->HistScale = 
			(histptr->HistEnLine-histptr->HistStLine+1)/ftemp;
 	
 	        ftemp = histptr->MaxPixelValue ;
		if( ftemp == 0 )
			ftemp = 1;
 	        histptr->PixScale = 
			(histptr->HistEnSample-histptr->HistStSample+1)/ftemp;
 		}
 	   else					/* Horizontal histogram      */
 	      	{
 	        histptr->Direction = Hori ;
 	
		/* Determine scaling factors for histogram display */
	   	ftemp = MAXDNVALUE + 1 ;
	   	histptr->HistScale = 
			(histptr->HistEnSample-histptr->HistStSample+1)/ftemp;

		ftemp = histptr->MaxPixelValue; 	
		if( ftemp == 0 )
			ftemp = 1;
 	        histptr->PixScale = 
			(histptr->HistEnLine-histptr->HistStLine+1)/ftemp;
 	      	}

 	   return( 0 ) ;				/* Normal return */
 	}
   		
	/********************/
 	/* Store image data */
 	/********************/
 	/*
 	Parameters :
 	
 		Input Unit number
 		Start line
 		Start sample
 		End line
 		End sample
 	
 		Output start line
 		Output start sample
 	
 	        Stretch flag	F = Fixed stretch
 				A = Adaptive stretch
 			 	N = No stretch
 		The next two float parameters are determined by :
 	
 		If ( Stretch flag = Fixed )
 		   Low Dn value for stretch
 		   High Dn value for stretch
 		Else
 		If ( Stretch flag = Adaptive )
 	           Fraction of image to be sampled
 	           Percentage of saturation to 0 and 255
 	        Else
 		If ( Stretch flag = No stretch )
 	           0.0
 	           0.0
 	*/
 	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastoreimage, XASTOREIMAGE) ( int *Unit, int *StLine,
		int *StSample,
		int *EnLine, int *EnSample, int *OutStLine, int *OutStSample,
		char *StretBand, float *Frac, float *Satur,
                int ExClude[], int *Number_Of_Element, ZFORSTR_PARAM )
        {
	ZFORSTR_BLOCK
        char *c_string;
        int length;

        zsfor2len(length, StretBand, &Unit, 12, 8, 1, Number_Of_Element);
        c_string = (char *)calloc(1,(length+1));
        zsfor2c(c_string, length, StretBand, &Unit, 12, 8, 1,Number_Of_Element);
 	zastoreimage( *Unit, *StLine, *StSample, *EnLine, *EnSample,
 	              *OutStLine, *OutStSample, c_string[0], Frac, Satur,
                      ExClude, *Number_Of_Element );
        free(c_string);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastoreimage( Unit, StLine, StSample, EnLine, EnSample,
 	              OutStLine, OutStSample, StretBand, Frac, Satur,
                      ExClude, Number_Of_Element )
 	int Unit, StLine, StSample, EnLine, EnSample, OutStLine, OutStSample,
            Number_Of_Element ;
 	int ExClude[] ;
        unsigned char StretBand ;
	float *Frac, *Satur ;
  	{
 	   struct ImageType *newblock;
 	   struct UnitType *ptr;
 
  	   int  j, UnitAlreadyUsed, UnitFound;
	   int temp1, temp2, IOStatus, bytes, flags, OutEndLine;
	   int InputLines, InputSamples, InputBands;
 	   char Band, Organization[5] ;
 	   float fMaxDnValue, fMinDnValue;

	   UnitAlreadyUsed = FALSE;
 	
 	   /* If we get negative values Then error */
 	
 	   if ( Unit     < 0 ||
 	        StLine   < 0 ||
 	        StSample < 0 || StSample >= UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        OutStLine < 0 ||
 	        OutStSample < 0 || OutStSample > UserMaxSamples )
		return( -1 );

	   /* Determine pixel size of image data */
	   IOStatus = zvget( Unit,"FLAGS", &flags, NULL );
	   if ( ( flags & OPEN ) == 0 )
		IOStatus = zvopen( Unit, "OPEN_ACT", "SA", NULL );
	   IOStatus = zvget( Unit, "NL", &InputLines, "NS", &InputSamples,
		             "NB", &InputBands, "PIX_SIZE", &bytes, NULL ) ;
	   if ( ( flags & OPEN ) == 0 )
	  	   IOStatus = zvclose( Unit, NULL );

	   if( bytes != 1 && bytes != 2 )
		{
		zvmessage("Image data format not halfword nor byte"," ");
		zvmessage("	- xa/zastoreimage aborted"," ");
		return( -1 );
		}
	   if( bytes == 1 )
		{
		fMinDnValue = 0.0;
		fMaxDnValue = 255;
		}
	   else
		{
		fMinDnValue = -32768.0;
	   	fMaxDnValue = 32767;
		}

	   if ( StretBand != 'n' && StretBand != 'N' &&
		( *Frac < fMinDnValue || *Frac > fMaxDnValue ||
 	        *Satur < fMinDnValue || *Satur > fMaxDnValue ) )
		{
		zvmessage("Image stretch parameters are not valid"," ");
		zvmessage("	- xa/zastoreimage aborted"," ");
		return( -1 );
		}
		
	   j=0;
	   while(!UnitAlreadyUsed&&j<unitcount)	/* Check to see if this unit  */
		if(unitnumber[j++]==Unit)	/* number has already been   */
			UnitAlreadyUsed = TRUE;	/* used. If not, add it to   */
	   if(!UnitAlreadyUsed)			/* list of unit numbers.     */
		unitnumber[unitcount++] = Unit;

 	   newblock = (struct ImageType *)malloc(sizeof (struct ImageType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for image"," ");
 		zabend();
 		}
 	   if( !imageptr )
		{
 		firstimage = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		imageptr->next = newblock;
		newblock->previous = imageptr;
		}
 	   newblock->next = NULL;
 	   imageptr = newblock;
 	
 	      /* Store with Start values < End values */
 	
 	   imageptr->InputUnitNumber = Unit ;		/* Store "good" data */
 	   imageptr->StartLine = Min( StLine, EnLine )  ;
 	   imageptr->StartSample = Min( StSample, EnSample ) ;
 	   imageptr->EndLine = Max( StLine, EnLine ) ;
 	   imageptr->EndSample = Max( StSample, EnSample ) ;
 	   imageptr->OutStartLine = OutStLine ;
 	   imageptr->OutStartSample = OutStSample ;

	   imageptr->OutBand = 0;
	   imageptr->ColorBands = -1;
	   imageptr->NumDNs = (int) pow(2.0,bytes*8.0);
 	   imageptr->Stretch = NoStretch ;

	   imageptr->InputLines = InputLines;
	   imageptr->InputBands = InputBands;

	   /* Check for boundary conditions */
 	   imageptr->StartLine = 
		Min( imageptr->StartLine,imageptr->InputLines ) ;
 	   imageptr->EndLine = Min( imageptr->EndLine, imageptr->InputLines ) ;
 	   imageptr->StartSample = 
		Min( imageptr->StartSample,InputSamples );
	   imageptr->EndSample = 
		Min( imageptr->EndSample,InputSamples ); 
	   imageptr->NUM = -1;

	   /* Recompute minimum and maximum start lines 		  */
	   minimageline = Min( imageptr->OutStartLine, minimageline );
	   
	   imageptr->InputSamples = 
		imageptr->EndSample - imageptr->StartSample + 1;

	   /* Check to see if unit number is associated with a color band */
	   ptr = firstunit;
	   UnitFound = FALSE;
	   while ( ptr != NULL && !UnitFound)
		{
		if( ptr->UnitNumber == imageptr->InputUnitNumber )
			{
			imageptr->OutBand = ptr->BandNumber;
			imageptr->ColorBands = 0;
			UnitFound = TRUE;
			}
		ptr = ptr->next;
		}

	   /* Check for stretch parameter value */
 	   if ( StretBand == 'F' || StretBand == 'f' )
 	      	{
 	        imageptr->Stretch = Fixed ;
 	        temp1 = *Frac ;
 	        temp2 = *Satur ;
		if( bytes == 2 )
			{
			temp1 += 32768;
			temp2 += 32768;
			}
 	        imageptr->SampleFraction = temp1 ;
 	        imageptr->SaturationPercentage = temp2 ;
 	      	}
 	   else
 	      	if ( StretBand == 'A' || StretBand == 'a' )
 	      		{
 	         	imageptr->Stretch = Adaptive ;
 	         	if ( *Frac > 1.0 || *Satur > 1.0 )
 	            		return( -1 ) ;
 	         	else
 	         		{
 	            		if (Number_Of_Element != 0 )
 	            			{	
 	               			imageptr->NUM = Number_Of_Element ;
					imageptr->EXC = (int *)calloc
						(Number_Of_Element,sizeof(int));
					if ( imageptr->EXC != NULL )
	 	               			for(j=0;j<Number_Of_Element;j++)
 		                     		  imageptr->EXC[j] = ExClude[j];
					else
	 				   {
					   zvmessage("Memory allocation error"," ");
					   zvmessage(" - DNs excluded is NULL"," ");
					   imageptr->NUM = -1;
					   };
 	            			}
 	 			imageptr->SampleFraction = *Frac ;
 	            		imageptr->SaturationPercentage = *Satur ;
 	         		}
 	      		}
	   imageptr->LUT = (int *)calloc(imageptr->NumDNs, sizeof( int ));
	   if( imageptr->LUT != NULL )
	 	for ( j = 0 ; j < imageptr->NumDNs ; j++ )	
 		           imageptr->LUT[ j ] = j;	/* Lookup table j=j */
 	   else
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" - za/xastoreimage aborted"," ");
		imageptr->previous->next = NULL;
		return( -1 );
		}
 	   return( 0 ) ;				/* Normal return */
 	} 
  		
	/************************/
 	/* Store PDS image data */
 	/************************/
 	/*
 	Parameters :
 	
 		Input file name
 		Start line
 		Start sample
 		End line
 		End sample
 	
 		Output start line
 		Output start sample
 	
 	        Stretch flag	F = Fixed stretch
 				A = Adaptive stretch
 			 	N = No stretch
 		The next two float parameters are determined by :
 	
 		If ( Stretch flag = Fixed )
 		   Low Dn value for stretch
 		   High Dn value for stretch
 		Else
 		If ( Stretch flag = Adaptive )
 	           Fraction of image to be sampled
 	           Percentage of saturation to 0 and 255
 	        Else
 		If ( Stretch flag = No stretch )
 	           0.0
 	           0.0
 	*/
 	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastorePDSimage, XASTOREPDSIMAGE) ( char *File,
		int *StLine, int *StSample,
		int *EnLine, int *EnSample, int *OutStLine, int *OutStSample,
		char *StretBand, float *Frac, float *Satur,
                int ExClude[], int *Number_Of_Element, ZFORSTR_PARAM )
        {
	ZFORSTR_BLOCK
        char *c1_string;
        char *c2_string;
        int length;

        zsfor2len(length, File, &File, 12, 1, 1, Number_Of_Element);
        c1_string = (char *)calloc(1,(length+1));
        zsfor2c(c1_string, length, File, &File, 12, 1, 1, Number_Of_Element);

        zsfor2len(length, StretBand, &File, 12, 8, 2, Number_Of_Element);
        c2_string = (char *)calloc(1,(length+1));
        zsfor2c(c2_string, length, StretBand, &File, 12, 8,2,Number_Of_Element);

 	zastorePDSimage( c1_string, *StLine, *StSample, *EnLine, *EnSample,
 	              *OutStLine, *OutStSample, c2_string[0], Frac, Satur,
                      ExClude, *Number_Of_Element );
        free(c1_string);
        free(c2_string);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastorePDSimage( File, StLine, StSample, EnLine, EnSample,
 	              OutStLine, OutStSample, StretBand, Frac, Satur,
                      ExClude, Number_Of_Element )
	char *File;
 	int StLine, StSample, EnLine, EnSample, OutStLine, OutStSample,
            Number_Of_Element ;
 	int ExClude[] ;
        unsigned char StretBand ;
	float *Frac, *Satur ;
  	{
 	   struct PDSImageType *newblock;
	   /* 	   struct UnitType *ptr;*/
 
  	   int i, j, PDSFileAlreadyUsed, PDSImageFound;
	   int temp1, temp2, IOStatus, bytes, flags, OutEndLine;
	   int InputLines, InputSamples, InputBands;
	   int record_bytes, image_start_record;
	   int index;

	   char string[MAXFILENAME+1];
 	   char Band;
 	   float fMaxDnValue, fMinDnValue;

 	   /* If we get negative values Then error */
 	
	   if ( strlen( File ) > MAXFILENAME ||
		strlen( File ) <= 0 ||
		StLine   < 0 ||
 	        StSample < 0 || StSample >= UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        OutStLine < 0 ||
 	        OutStSample < 0 || OutStSample > UserMaxSamples )
		return -1;

	   strcpy(string,File);

	   /* Determine pixel size of image data */
	   IOStatus = get_pds_image_stats( string, &InputLines, &InputSamples,
		     &InputBands, &bytes, &record_bytes, &image_start_record );
	   if ( IOStatus < 0 || InputSamples != (record_bytes/bytes) )
		return -1;

	   if( bytes != 1 && bytes != 2 )
		{
		zvmessage("Image data format not halfword nor byte"," ");
		zvmessage("	- xa/zastorePDSimage aborted"," ");
		return( -1 );
		}
	   if( bytes == 1 )
		{
		fMinDnValue = 0.0;
		fMaxDnValue = 255;
		}
	   else
		{
		fMinDnValue = -32768.0;
	   	fMaxDnValue = 32767;
		}

	   if ( StretBand != 'n' && StretBand != 'N' &&
		( *Frac < fMinDnValue || *Frac > fMaxDnValue ||
 	        *Satur < fMinDnValue || *Satur > fMaxDnValue ) )
		{
		zvmessage("Image stretch parameters are not valid"," ");
		zvmessage("	- xa/zastorePDSimage aborted"," ");
		return( -1 );
		}

	   /* Allocate memory for next PDS image object */
 	   newblock = (struct PDSImageType *)malloc
				(sizeof (struct PDSImageType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for PDS image"," ");
 		zabend();
 		}
 	   if( !PDSimageptr )
		{
 		firstPDSimage = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		PDSimageptr->next = newblock;
		newblock->previous = PDSimageptr;
		}
 	   newblock->next = NULL;
 	   PDSimageptr = newblock;
 	
 	      /* Store with Start values < End values */
 	
 	   strcpy(PDSimageptr->InputFileName,string);	/* Store "good" data */
	   PDSimageptr->fp = NULL;
	
 	   PDSimageptr->StartLine = Min( StLine, EnLine )  ;
 	   PDSimageptr->StartSample = Min( StSample, EnSample ) ;
 	   PDSimageptr->EndLine = Max( StLine, EnLine ) ;
 	   PDSimageptr->EndSample = Max( StSample, EnSample ) ;
 	   PDSimageptr->OutStartLine = OutStLine ;
 	   PDSimageptr->OutStartSample = OutStSample ;

	   PDSimageptr->OutBand = 0;
	   PDSimageptr->ColorBands = -1;
	   PDSimageptr->NumDNs = (int) pow(2.0,bytes*8.0);
 	   PDSimageptr->Stretch = NoStretch ;

	   PDSimageptr->InputLines = InputLines;
	   PDSimageptr->InputBands = InputBands;

	   PDSimageptr->Bytes = bytes;
	   PDSimageptr->RecordBytes = record_bytes;
	   PDSimageptr->ImageStartRecord = image_start_record;

	   /* Check for boundary conditions */
 	   PDSimageptr->StartLine = 
		Min( PDSimageptr->StartLine,PDSimageptr->InputLines );
 	   PDSimageptr->EndLine = 
		Min( PDSimageptr->EndLine,PDSimageptr->InputLines );
 	   PDSimageptr->StartSample = 
		Min( PDSimageptr->StartSample,InputSamples );
	   PDSimageptr->EndSample = 
		Min( PDSimageptr->EndSample,InputSamples ); 
	   PDSimageptr->NUM = -1;

	   /* Recompute minimum and maximum start lines 		  */
	   minPDSimageline = Min( PDSimageptr->OutStartLine,minPDSimageline );

	   PDSimageptr->InputSamples = 
		PDSimageptr->EndSample - PDSimageptr->StartSample + 1;
	   
	   /* Register the image file name in the array of structures for */
	   /* tracking status of all PDS image files.			  */

	   index = get_pds_image_index(File);
	   if ( index < 0 )
		{
		index = PDSimagecount;	
		strcpy(pds_image[index].file,
			PDSimageptr->InputFileName);
		pds_image[index].file_open_flag = FALSE;
		PDSimagecount++;
		}

	   pds_image[index].lines = InputLines;
	   pds_image[index].samples = InputSamples;
	   pds_image[index].bands = InputBands;
	   pds_image[index].bytes = bytes;
	   pds_image[index].record_bytes = record_bytes;
	   pds_image[index].start_record = image_start_record;
		
	   /* Check for stretch parameter value */
 	   if ( StretBand == 'F' || StretBand == 'f' )
 	      	{
 	        PDSimageptr->Stretch = Fixed ;
 	        temp1 = *Frac ;
 	        temp2 = *Satur ;
		if( bytes == 2 )
			{
			temp1 += 32768;
			temp2 += 32768;
			}
 	        PDSimageptr->SampleFraction = temp1 ;
 	        PDSimageptr->SaturationPercentage = temp2 ;
 	      	}
 	   else
 	      	if ( StretBand == 'A' || StretBand == 'a' )
 	      		{
 	         	PDSimageptr->Stretch = Adaptive ;
 	         	if ( *Frac > 1.0 || *Satur > 1.0 )
 	            		return( -1 ) ;
 	         	else
 	         		{
 	            		if (Number_Of_Element != 0 )
 	            			{	
 	               			PDSimageptr->NUM = Number_Of_Element ;
					PDSimageptr->EXC = (int *)calloc
						(Number_Of_Element,sizeof(int));
					if ( PDSimageptr->EXC != NULL )
	 	               			for(j=0;j<Number_Of_Element;j++)
 		                     		  PDSimageptr->EXC[j] = ExClude[j];
					else
	 				   {
					   zvmessage("Memory allocation error"," ");
					   zvmessage(" - DNs excluded is NULL"," ");
					   PDSimageptr->NUM = -1;
					   };
 	            			}
 	 			PDSimageptr->SampleFraction = *Frac ;
 	            		PDSimageptr->SaturationPercentage = *Satur ;
 	         		}
 	      		}
	   PDSimageptr->LUT = (int *)calloc(PDSimageptr->NumDNs, sizeof( int ));
	   if( PDSimageptr->LUT != NULL )
	 	for ( j = 0 ; j < PDSimageptr->NumDNs ; j++ )	
 		           PDSimageptr->LUT[ j ] = j;	/* Lookup table j=j */
 	   else
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" - za/xastorePDSimage aborted"," ");
		PDSimageptr->previous->next = NULL;
		return( -1 );
		}
 	   return( 0 ) ;				/* Normal return */
 	}
  		
	/***************************/
	/* Store ray at any angle  */
	/***************************/
	/* 
	Parameters are :
	
		Start line
		Start sample
		Angle
		Length
		Width
		DN value or DN array for color
		Dummy string variable for future expansion
		End Line   (returned)
		End Sample (returned)

	*/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastoreray, XASTORERAY) (int *StLine, int *StSample,
		float *Ang, int *Length, int *Width, int *DnV, 
                char *DMY, int *EnL, int *EnS, ZFORSTR_PARAM )
        {
	ZFORSTR_BLOCK
        char *c_string;
        int length;

        zsfor2len(length, DMY, &StLine, 9, 7, 1, EnS);
        c_string = (char *)calloc(1,(length+1));
        zsfor2c(c_string, length, DMY, &StLine,  9, 7, 1, EnS);
	zastoreray( *StLine, *StSample, Ang, *Length, *Width, DnV, c_string[0], 
                    EnL, EnS );
        free(c_string);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastoreray( StLine, StSample, Ang, Length, Width, DnV, DMY, 
                    EnL, EnS )
 	int StLine, StSample, Length, Width, *DnV, *EnL, *EnS ;
	float *Ang;
	char  *DMY ;
 	{
 	   struct VectorType *newblock;
 	   int dx, dy, dx_sign, dy_sign;
	   int DnVal[3], dm ;
	   float  OriginalAngle, Angle;

	   OriginalAngle = *Ang;
	   Angle = *Ang;

	   for(dm=0;dm<Dimension;dm++,DnV++)
		{
		DnVal[dm] = *DnV ;
		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
		return(-1);
		}

	   /* If we get negative values Then error */
 	   if ( StLine   <= 0 || StLine > UserMaxLines     ||
		StSample <= 0 || StSample > UserMaxSamples ||
 	        Angle    <  0 || 
		Length   <= 0 || Length > UserMaxSamples   ||
		Width    <= 0 || Width  > UserMaxSamples   )
 		return(-1);
 	
 	   newblock = (struct VectorType *)malloc(sizeof (struct VectorType));
 	   if( !newblock )
 		{
 		zvmessage( "Allocation error for vector"," ");
		zvmessage( " - GMASK ABEND"," " );
 		zabend();
 		}
 	   if( !vectorptr )
		{
 		firstvector = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		vectorptr->next = newblock;
		newblock->previous = vectorptr;
		}

 	   newblock->next = NULL;

 	   vectorptr = newblock;

	   vectorptr->StartLine = StLine;
	   vectorptr->StartSample = StSample;
	   vectorptr->Length = Length;
	   vectorptr->Width = Width;

	   for( dm=0; dm<Dimension; dm++ )
		vectorptr->Dn[dm] = DnVal[dm];

	   /* Angle processing and vector orientation determination */

	   Angle = fmod( (double)Angle, 360.0 );
	
	   if( fmod((double)Angle,180.0) == 0.0 )
		{ /* Vertical vector cases */

		vectorptr->Direction = Vert;
		vectorptr->EndSample = vectorptr->StartSample + Width/2;	
		vectorptr->StartSample -= Width/2;
		
		*EnS = StSample;
		if( Angle == 180.0 )
			{ /* Vector pointer due SOUTH */
			vectorptr->EndLine 
				= vectorptr->StartLine + vectorptr->Length;
			*EnL = vectorptr->EndLine;
			}
		else
			{ /* Vector pointer due NORTH */
			vectorptr->EndLine = vectorptr->StartLine;
			vectorptr->StartLine 
				= vectorptr->EndLine - vectorptr->Length;
			*EnL = vectorptr->StartLine;
			}
		
		if ( vectorptr->StartLine   <= 0 ||   /* Check mask boundary */
		     vectorptr->StartSample <= 0 ||    
		     vectorptr->EndSample > UserMaxSamples ||
		     vectorptr->EndLine   > UserMaxLines    )
		 	return(-1);
		}
 	   else	
		if( fmod((double)(Angle+90.0),180.0) == 0.0 )
			{ /* Horizontal vector cases */

			vectorptr->Direction = Hori;
			vectorptr->EndLine = vectorptr->StartLine + Width/2;
			vectorptr->StartLine -= Width/2;
			
			*EnL = StLine;
			if( (Angle+90.0) == 180.0 )
				{ /* Vector due EAST */
				vectorptr->EndSample 
				= vectorptr->StartSample + vectorptr->Length;
				*EnS = vectorptr->EndSample;
				}
			else
				{ /* Vector due WEST */
				vectorptr->EndSample = vectorptr->StartSample;
				vectorptr->StartSample 
				= vectorptr->EndSample - vectorptr->Length;
				*EnS = vectorptr->StartSample;
				}
			if ( vectorptr->StartLine   <= 0 ||   /* Check mask 
			     vectorptr->StartSample <= 0 ||      boundary */
			     vectorptr->EndSample > UserMaxSamples ||
			     vectorptr->EndLine   > UserMaxLines    )
			 	return(-1);
			}
		else	
			{ /* Non-zero and defined slopes */

			vectorptr->Direction   = Klinein ;

		        Angle = fmod( (double)OriginalAngle, 360.0 );
		
			/* Calculate slanted line */
			xxacalculateklinein(vectorptr,Angle,EnL,EnS);
	   		}

	   /* Recompute minimum and maximum start lines */
	   minvectorline = Min( vectorptr->StartLine, minvectorline );

	   return ( 0 );
	}
  		
 	/***************************/
 	/* Store character strings */
	/***************************/
 /****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastorestring, XASTORESTRING)( int *StLine,
		int *StSample, int *Sizval,
		int *DnV, char ChString[MAXSTRINGSIZE],
                int *Thickness, ZFORSTR_PARAM)
        {
	ZFORSTR_BLOCK
        char c_string[MAXSTRINGSIZE];

        zsfor2c(c_string, MAXSTRINGSIZE, ChString, &StLine, 6, 5, 1, Thickness);
 	zastorestring( *StLine, *StSample, *Sizval, DnV, c_string, *Thickness );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	/*
 	Parameters are :
 	
 		Start line
 		Start sample
 		Size of characters
 		Dn value or Dn array for color
 		Character string
 	*/

 	int zastorestring( StLine, StSample, Sizval, DnV, ChString, Thickness )
 	int StLine, StSample, Sizval, *DnV , Thickness ;
        char ChString[ MAXSTRINGSIZE ] ;
  	{
 	   struct StringType *newblock;
 	   int DnVal[3];
	   int length;
 	   char string[80];
 	   char Ch ;
 	   int dm, j, k, l;


	   /* Get DN value(s) of characters 		*/
	   for(dm=0;dm<Dimension;dm++,DnV++)
 		{
		DnVal[dm] = *DnV ;
 		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
		}

	   /* If we get negative values Then error 	*/
 	   if ( StLine <= 0 || StSample <= 0 || StSample >= UserMaxSamples || 
		Sizval <= 0 || ChString[0] <= 0 )
 	      	return( -1 ) ;
 	
	   /* Determine string length 			*/
	   zastringlength( ChString, Sizval, Thickness, &length );

 	   if ( XAFONTANGLE == -90 )     		/* If -90 angle Then */
 	   {
 	        k = strlen( ChString ) - 1 ;
 	        for ( j = 0 ; j <= k / 2 ; j++ )	/* Reverse string */
 	        {
 	              Ch = ChString[ j ] ;
 	              ChString[ j ] = ChString[ k-j ] ;
 	              ChString[ k-j ] = Ch ;
 	        }
 	   }
 	
 	   if ( XAFONTTYPE == DEFAULT )	 Thickness = 1 ;
 	
 	   for ( l = 0 ; l < Thickness ; l++ )	   /* For each thickness */
 	      	{
 		/* Allocate space for current string for each thickness plane */
 	   	newblock = (struct StringType *)malloc(sizeof(struct StringType));
 	  	if( !newblock )
 			{
			zvmessage("***"," ");
			zvmessage("*** GMASK ZA/XASTORESTRING ERROR"," ");
			zvmessage("***"," ");
 			zvmessage("*** Allocation error for strings"," ");
			zvmessage(" "," ");
 			zabend();
 			}
 	   	if( !stringptr )
			{
 			firststring = newblock;
			newblock->previous = NULL;
			}
 	   	else
			{
 			stringptr->next = newblock;
			newblock->previous = stringptr;
			}
 	   	newblock->next = NULL;
 	   	stringptr = newblock;
 	
 	        stringptr->StartLine = StLine ; /* Store "good" data */
 	        StLine-- ;
/***/
/* change mos059: added '&& Thickness > 1' below, and rearranged the */
/* calls to zvmessage and sprintf.                                   */
/***/
		if ( StLine == 0 && Thickness > 1)
			{
			zvmessage("***"," ");
			zvmessage("*** GLLMASKSTORESTRING WARNING"," ");
			zvmessage("***"," ");
			l++;
			sprintf(string,
                   "*** Thickness of string was limited to thickness of %d",l);
			zvmessage(string," ");
			zvmessage("*** because the top of the mask has been reached."," ");
	                zvmessage("The string involved will be printed below:"," ");
			zvmessage(stringptr->CharString," ");
			l = Thickness;
			}

 	        stringptr->StartSample = StSample ;
 		for(dm=0;dm<Dimension;dm++)
 			stringptr->Dn[dm] = DnVal[dm] ;
 	        strcpy( stringptr->CharString, ChString ) ;
 	        stringptr->FontType = XAFONTTYPE ;
 	        stringptr->Angle = XAFONTANGLE ;
 	        stringptr->FontThickness = Thickness ;
 	        stringptr->CharHeight = Sizval * XACHARHEIGHT ;
 	        stringptr->Size = Sizval ;

		if ( stringptr->FontType == HERSHEY )
			{
			stringptr->CharSpac  = Thickness - 1;
			stringptr->FontNum   = XAFONTNUMBER;
 	        	stringptr->LineScale = XACHARHEIGHT * 1.7;
	  	 	stringptr->CharWidth = stringptr->CharHeight * 0.9; 
			stringptr->usedcharindex 
				= (int *)calloc(96,sizeof(int));
     	                if( stringptr->usedcharindex == NULL )
		          {
 		           zvmessage( "Allocation error for strings"," ");
		           zvmessage( " - GMASK ABEND"," " );
 	                   zabend();
 		          }
			stringptr->bufheight = stringptr->CharHeight * 1.5;
			stringptr->bufwidth  = stringptr->CharHeight;
			stringptr->bufarea   = stringptr->bufwidth *
						stringptr->bufheight;
			}
 	        else
 	        	{
			stringptr->CharSpac = stringptr->Size;
 	        	if ( (stringptr->CharHeight % 2) == 0 )
 	        		stringptr->CharWidth = stringptr->CharHeight/2 ;
 	             	else
 	               		stringptr->CharWidth = 
				stringptr->CharHeight/2 + 1 ;
 	        	if ( stringptr->Angle == 0 )
 	        		stringptr->LineScale = XACHARHEIGHT ;
 	        	else
 	        		stringptr->LineScale 
					= XACHARWIDTH + XACHARSPACING ;
 	        	}

 		if (CharSpacing != NOOVERRIDE )
 			stringptr->CharSpac = CharSpacing;
 	 
		if( stringptr->Angle == 0 )
			stringptr->LastLine = stringptr->StartLine + 
			stringptr->LineScale * stringptr->Size;
		else
			stringptr->LastLine = stringptr->StartLine + length;

		/* Recompute minimum and maximum start lines 		  */
	  	minstringline = Min( stringptr->StartLine, minstringline );
 	      	}

 	   return( 0 ) ;				/* Normal return */
 	}
  		
 	/************************/
 	/* Store Tick Mark Line */
 	/************************/
 	
 	/*
 	Parameters are :
 	
 		Start Line
 		End Line
 		Start Sample
 		End Sample
 		Width of each tick
 		Length of each tick
 		Spacing between tick marks
 		Dn value or Dn array for color
 	*/
 	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastoretick, XASTORETICK) ( StLine, StSample, EnLine,
								EnSample, Wid, 
                      Len, Space, DnV )
 	int *StLine, *StSample, *EnLine, *EnSample, *Wid, *Len, *Space, *DnV ;
         {
 	 zastoretick( *StLine, *StSample, *EnLine, *EnSample, *Wid, *Len, 
                      *Space, DnV );
         }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastoretick( StLine, StSample, EnLine, EnSample, Wid, Len, Space, DnV )
 	int StLine, StSample, EnLine, EnSample, Wid, Len, Space, *DnV ;
 	{
 	   struct TickType *newblock;
 	   int dm, DnVal[3];
 	
	   for(dm=0;dm<Dimension;dm++,DnV++)
 		{
		DnVal[dm] = *DnV ;
		
		/* If we get negative values, then error */
 		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
 		}

	   /* If we get negative values, then error */
 	   if ( StLine   < 0 ||
 	        StSample < 0 || StSample >= UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        Wid     <= 0 ||
 	        Len     <= 0 ||
 	        Space   <= 0 ||
		(EnLine != StLine && EnSample != StSample))
 	      return( -1 ) ;
 	
 	   newblock = (struct TickType *)malloc( sizeof (struct TickType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for ticks"," ");
 		zabend();
 		}
 	   if( !tickptr )
		{
 		firsttick = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		tickptr->next = newblock;
		newblock->previous = tickptr;
		}
 	   newblock->next = NULL;
 	   tickptr = newblock;
 	
	   /* Store with Start values < End values */
  	   tickptr->StartLine = Min( StLine, EnLine ) ;
 	   tickptr->StartSample = Min( StSample, EnSample ) ;
 	   tickptr->EndLine = Max( StLine, EnLine ) ;
 	   tickptr->EndSample = Max( StSample, EnSample ) ;
 	   tickptr->Width = Wid ;
 	   tickptr->Length = Len ;
 	   tickptr->Spacing = Space ;

	   
 	   for(dm=0;dm<Dimension;dm++)
 		tickptr->Dn[dm] = DnVal[dm] ;

	   /* Recompute minimum and maximum start lines 		  */
	   mintickline = Min( tickptr->StartLine, mintickline );

           /* Determine ending line of tick marks			  */
 	   if ( tickptr->StartLine == tickptr->EndLine )
		{ 
 		tickptr->Vertical_Tick_End_Line = tickptr->StartLine 
		+ tickptr->Length - 1 ;
		tickptr->LastLine = tickptr->Vertical_Tick_End_Line;
		}
	   else
		tickptr->LastLine = tickptr->EndLine;

 	   return( 0 ) ;				/* Normal return */
 	}

  		
 	/*******************************/
 	/* Store Straight Line Vectors */
 	/*******************************/
 	/*
 	   Parameters are :
 	
 		Start line
 		Start sample
 		End line
 		End sample
 		Width
 		Dn value or Dn array for color
 	*/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastorevector, XASTOREVECTOR) ( StLine, StSample,
					EnLine, EnSample, Width, DnV )
 	int *StLine, *StSample, *EnLine, *EnSample, *Width, *DnV ;
        {
 	zastorevector( *StLine, *StSample, *EnLine, *EnSample, *Width, DnV );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zastorevector( StLine, StSample, EnLine, EnSample, Width, DnV )
 	int StLine, StSample, EnLine, EnSample, Width, *DnV ;
 	{
 	   struct VectorType *newblock;
 	   int dm, DnVal[3] ;
 	   int i, j, dx, dy;
	   float Angle, fdx, fdy;
	   double rad_to_deg;

 	   for(dm=0;dm<Dimension;dm++,DnV++)
 		{
		DnVal[dm] = *DnV ;
 		if(DnVal[dm] < 0 || DnVal[dm] > MAXDNVALUE)
 		return(-1);
 		}
 				/* If we get negative values Then error */
 	   if ( StLine   < 0  || StLine   > UserMaxLines   || 
		StSample < 0  || StSample > UserMaxSamples ||
 	        EnLine   < 0  || EnLine   > UserMaxLines   ||
		EnSample < 0  || EnSample > UserMaxSamples ||
 	        Width    <= 0 || Width    > UserMaxSamples )
 		return(-1);
 	
 	   newblock = (struct VectorType *)malloc(sizeof (struct VectorType));
 	   if( !newblock )
 		{
 		zvmessage( "Allocation error for vector"," ");
		zvmessage( " - GMASK ABEND"," ");
 		zabend();
 		}
 	   if( !vectorptr )
		{
 		firstvector = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		vectorptr->next = newblock;
		newblock->previous = vectorptr;
		}
 	   newblock->next = NULL;
 	   vectorptr = newblock;
 
	   /* Reorder starting line and sample to be true to image 
	      writing order						*/
	   vectorptr->StartLine   = Min( StLine, EnLine ) ; 
 	   vectorptr->StartSample = Min( StSample, EnSample ) ;
 	   vectorptr->EndLine     = Max( StLine, EnLine ) ;
 	   vectorptr->EndSample   = Max( StSample, EnSample ) ;
  	
 	   vectorptr->Width = Width ;

 	   for(dm=0;dm<Dimension;dm++)
 	   	vectorptr->Dn[dm] = DnVal[dm] ;
 	
	   /* Recompute minimum and maximum start lines 		  */
	   minvectorline = Min( vectorptr->StartLine, minvectorline );
	   
 	   /* Vector orientation determination */
 	
 	   if ( vectorptr->StartLine == vectorptr->EndLine )
 	      	{ /* Horizontal vector case */

	      	vectorptr->Direction = Hori ;
 	      	vectorptr->EndLine = vectorptr->StartLine + Width/2 ;
		vectorptr->StartLine -= Width/2 ;
 	      	vectorptr->Length  = vectorptr->EndSample 
				     - vectorptr->StartSample + 1 ;
		
		/* Check mask boundary	*/
		if ( vectorptr->StartLine   <= 0 ||
		     vectorptr->StartSample <= 0 ||
		     vectorptr->EndSample > UserMaxSamples ||
		     vectorptr->EndLine   > UserMaxLines )
			return(-1);
 	      	}
 	   else
		if( vectorptr->StartSample == vectorptr->EndSample )
		    { /* Vertical vector case */

 	            vectorptr->Direction = Vert ;
		    vectorptr->EndSample = vectorptr->StartSample + Width/2 ;
		    vectorptr->StartSample -= Width/2 ;

		    /* Run out of mask space? */
 		    if ( StSample+Width >= UserMaxSamples ) 
 	        	{
			/* If so, reset element */
 	            	vectorptr->StartLine = -1 ;	
 	            	return( -2 ) ;
 	         	}

		    /* Check mask boundary	*/
		    if ( vectorptr->StartLine   <= 0 ||
		         vectorptr->StartSample <= 0 ||
		         vectorptr->EndSample > UserMaxSamples ||
		         vectorptr->EndLine   > UserMaxLines )
			return(-1);
 	     	    }
		else
                    {
		    vectorptr->Direction = Klinein ;    /* KLINEIN: Greek 
 							   for slant 	 */
		    dx =  EnSample - StSample ;
		    dy =  EnLine - StLine ;

 		    dm = dx*dx + dy*dy;

		    /* Check for dx or dy of zero and round up */
		    fdx = (float)abs(dx);
		    fdy = (float)abs(dy);

		    if ( fdx == 0.0 )
			fdx = 1.0;
		    if ( fdy == 0.0 )
			fdy = 1.0;

		    rad_to_deg = 360.0 / (3.14159265 * 2.0);

		    /* Note: Angles (angle) measured clockwise from North */

		    /* First Quadrant: 0 - 90 deg. (clockwise)	*/
		    if ( dx > 0 && dy < 0 )	
			Angle = rad_to_deg*atan((double)(fdx/fdy));

		    /* Second Quadrant: 270 - 360 degrees	*/ 
		    if ( dx < 0 && dy < 0 )
			Angle = 360.0 - rad_to_deg*atan((double)(fdx/fdy));

		    /* Third Quadrant: 180 - 270 degrees 	*/
		    if ( dx < 0 && dy > 0 )
			Angle = 180.0 + rad_to_deg*atan((double)(fdx/fdy));

		    /* Fourth Quadrant: 90 - 180 degrees 	*/
		    if ( dx > 0 && dy > 0 )	
			Angle = 180.0 - rad_to_deg*atan((double)(fdx/fdy));

		    vectorptr->Length      = sqrt ( (double)dm );
		    vectorptr->StartLine   = StLine;
		    vectorptr->StartSample = StSample;

		    /* Calculate slanted line */
		    xxacalculateklinein(vectorptr,Angle,&i,&j);
		    }	

 	   return( 0 ) ;				/* Normal return */
 	}
  		
 	/***************************/
 	/* Calculate String Length */
 	/***************************/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xastringlength, XASTRINGLENGTH) ( char ChString[MAXSTRINGSIZE],
		int *StringSize, int *Thickness, int *Length, ZFORSTR_PARAM )
        {
	ZFORSTR_BLOCK
        char c_string[MAXSTRINGSIZE];

        zsfor2c(c_string, MAXSTRINGSIZE, ChString, &ChString, 4, 1, 1, Length);
 	zastringlength ( c_string, *StringSize, *Thickness, Length );
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	void zastringlength ( ChString, StringSize, Thickness, Length )
 	int StringSize, Thickness, *Length ;
        char ChString[ MAXSTRINGSIZE ] ;
  	{
  	   int ch ;
 	   int i ;
 	   int  CharHeight ;
 	   int  width ;
 	
  	
 	   ch = 0 ;
 	   *Length = 0 ;
 	   CharHeight = StringSize * XACHARHEIGHT ;
 	
 	   if ( XAFONTTYPE == DEFAULT )	/* Determine font type */
 	   {
 	      if ( (CharHeight % 2) == 0 )
 	        width = CharHeight/2  ;
 	      else
 	        width = CharHeight/2 + 1 ;
 	
 	      if( CharSpacing != NOOVERRIDE )
		*Length = strlen(ChString ) * width + 
 	                CharSpacing * (strlen(ChString)- 1) ;
	      else
 	      	*Length = strlen(ChString ) * width + 
 	                (strlen(ChString)- 1) * StringSize ;
 	   }
 	   else
 	   {
 	     for ( i = 0 ; i < strlen( ChString ) ; i++ )
 	     {
 	        ch = ChString[ i ] ;	/* Current Char */
 	        if ( ch == 'm' )	/* Make room for widest character, m */
 	            width = CharHeight ;
 	        else		/* Make room for almost widest char */
 	        if ( ch == '%' || ch == '@' || ch == '+' ||
 	             ch == 'M' ||
 	             ch == 'W' || ch == 'w' || ch == 'U' )
 	            width = CharHeight * 0.9 ;
 	        else		/* Make sure character is not 'broken' */
 	        if ( ch == 'B' || ch == 'D' || ch == 'H' ||
 	             ch == 'L' || ch == 'K' || ch == 'O' ||
 	             ch == 'P' || ch == 'Q' || ch == 'R' ||
 	             ch == 'T' ||
 	             ch == 'b' || ch == 'h' || ch == 'k' ||
 	             ch == 'p' || ch == 'n' )
 	            width = CharHeight * 0.7 ;
 	        else		/* Close up gaps for thin characters */
 	        if ( ch == 'i' || ch == 'I' ||
 	             ch == 'j' || ch == 'J' ||
 	             ch == 'L' || ch == 'l' ||
 	             ch == 'f' ||
 	             ch == ':' || ch == '.' || ch == ',' )
 	            width = CharHeight * WIDTH2HEIGHTRATIO * 0.8 ;
 	        else		/* Regular characters */
 	            width = CharHeight * WIDTH2HEIGHTRATIO ;
 	  
 	        *Length = *Length + width + Thickness - 1 ;
 	     }
 	   }
 	}
 	
   		
	/************************/
 	/* Store zoomed image   */
 	/************************/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xazoomimage, xazoomimage) (int *Unit,
		int *StLine, int *StSample,
		int *EnLine, int *EnSample, int *OutStLine, int *OutStSample,
		int *MagFactor, char Ztype[20], char *StretBand,
		float *Frac, float *Satur, int ExClude[],
		int *Number_Of_Element, ZFORSTR_PARAM)
#if 0
        char          Ztype[20];		/* Type of zoom performed    */
#endif
        {
	ZFORSTR_BLOCK
        char c_string1[20],*c_string2;
        int length;

        length = 20;
        zsfor2c(c_string1, length, Ztype, &Unit, 14, 9, 1, Number_Of_Element);

        zsfor2len(length, StretBand, &Unit, 14, 10, 2, Number_Of_Element); 
        c_string2 = (char *)calloc(1,(length+1));	
        zsfor2c(c_string2, length, StretBand, &Unit, 14,10,2,Number_Of_Element);

 	zazoomimage(*Unit, *StLine, *StSample, *EnLine, *EnSample, *OutStLine, 
                    *OutStSample, *MagFactor, c_string1, c_string2[0], Frac, Satur, 
		    *ExClude, *Number_Of_Element );
        free (c_string2);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zazoomimage(Unit, StLine, StSample, EnLine, EnSample, OutStLine, 
                    OutStSample, MagFactor, Ztype, StretBand, Frac, Satur, 
		    ExClude, Number_Of_Element )

        int           Unit, StLine, StSample, EnLine, EnSample, OutStLine,
                      OutStSample, MagFactor;
        char          Ztype[20];		/* Type of zoom performed    */
        unsigned char StretBand;
        float	      *Frac, *Satur;
  	int           Number_Of_Element, ExClude[];
 	{
  	   struct ZImageType *newblock;
 	   
 	   int 	 i, j, k;			/* Loop control variable     */
  	   float fMaxDnValue, fMinDnValue;
 	
 	   struct UnitType *ptr;
 	   enum ZoomType Type;
 	
 	   int  OutEndLine, OutBand, UnitAlreadyUsed,UnitFound, temp1, temp2,
		IOStatus, bytes, InputLines, InputSamples, InputBands;
	   int  flags;
 	
  	
 	   Type = 2;
 	   if(Ztype[0] == 'r' || Ztype[0] == 'R')
 	   	Type = 1;
 	   else
 	   	if(Ztype[0] == 'I' || Ztype[0] == 'i')
 	   		Type = 0;
 	
 	   /* If we get negative values Then error */
 	   if ( Unit     < 0 ||
 	        StLine   < 0 ||
 	        StSample < 0 || StSample > UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        OutStLine < 0 ||
 	        OutStSample < 0 || OutStSample > UserMaxSamples )
		return( -1 );

	   /* Determine pixel size of image data */
	   IOStatus = zvget( Unit, "FLAGS", &flags, NULL );
	   if ( ( flags & OPEN ) == 0 )
		IOStatus = zvopen( Unit, "OPEN_ACT", "SA", NULL );
	   IOStatus = zvget( Unit, "NL", &InputLines, "NS", &InputSamples,
		             "NB", &InputBands, "PIX_SIZE", &bytes, NULL ) ;
	   if ( ( flags & OPEN ) == 0 )
	  	   IOStatus = zvclose( Unit, NULL );

	   if( bytes != 1 && bytes != 2 )
		{
		zvmessage("Image data format not halfword nor byte"," ");
		zvmessage("	- xa/zazoomimage aborted"," ");
		return( -1 );
		}

	   if( bytes == 1 )
		{
		fMinDnValue = 0.0;
		fMaxDnValue = 255;
		}
	   else
		{
		fMinDnValue = -32768.0;
	   	fMaxDnValue = 32767;
		}

	   if ( StretBand != 'n' && StretBand != 'N' &&
		( *Frac < fMinDnValue || *Frac > fMaxDnValue ||
 	        *Satur < fMinDnValue || *Satur > fMaxDnValue ) )
		{
		zvmessage("Image stretch parameters are not valid"," ");
		zvmessage("	- xa/zazoomimage aborted"," ");
		return( -1 );
		}

	   /* If Magnification factor is 1, do an xastoreimage */
	   if( MagFactor == 1 || MagFactor == -1 )
		{
 		zastoreimage(Unit, StLine, StSample, EnLine, EnSample, 
                             OutStLine, OutStSample, StretBand, Frac,
			     Satur, ExClude, Number_Of_Element );
		return( 0 );
		}

 	   /* Test input parameters and print appropriate messages to user */
 	   if( MagFactor % 2 != 0 && Type == 0 )
 	      {
 	      zvmessage("ODD ZOOM FACTORS ARE NOT ALLOWED WITH INTERPOLATION"," ");
	      zvmessage("	- xa/zazoomimage aborted."," ");
 	      return(-1);
 	      }

 	   if(Type!=0 && Type!=1)
 		{
 		zvmessage("UNKNOWN ZOOMING METHOD"," ");
		zvmessage("	- xa/zazoomimage aborted."," ");
 		return(-2);
 		}

 	   if( MagFactor > 8 || MagFactor < -8 )
 		zvmessage("High zoom factor may mean longer execution time."," ");

	   UnitAlreadyUsed = FALSE;		
	   j=0;
	   while(!UnitAlreadyUsed&&j<unitcount)	/* Check to see if this unit  */
		if(unitnumber[j++]==Unit)	/* number has already been   */
			UnitAlreadyUsed = TRUE;	/* used. If not, add it to   */
	   if(!UnitAlreadyUsed)			/* list of unit numbers.     */
		unitnumber[unitcount++] = Unit;
 	
 	   newblock = (struct ZImageType *)malloc(sizeof (struct ZImageType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for zoomed image"," ");
 		zabend();
 		}
 	   if( !zoomptr )
		{
 		firstzoom = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		zoomptr->next = newblock;
		newblock->previous = zoomptr;
		}
 	   newblock->next = NULL;
 	   zoomptr = newblock;
 	
 	   /* Store with Start values < End values */
 	   zoomptr->InputUnitNumber = Unit ;		/* Store "good" data */
 	   zoomptr->StartLine = Min( StLine, EnLine )  ;
 	   zoomptr->StartSample = Min( StSample, EnSample ) ;
 	   zoomptr->EndLine = Max( StLine, EnLine ) ;
 	   zoomptr->EndSample = Max( StSample, EnSample ) ;
 	   zoomptr->OutStartLine = OutStLine ;
 	   zoomptr->OutStartSample = OutStSample ;
 	   zoomptr->OutBand = 0;
 	   zoomptr->ColorBands = -1;
 	   zoomptr->Ztype = Type ;
 	   zoomptr->MagFactor = MagFactor;
	   zoomptr->NumDNs = (int) pow(2.0,bytes*8.0);
	   zoomptr->Stretch = NoStretch;
	   zoomptr->NUM = -1;

	   zoomptr->InputLines = InputLines;
	   zoomptr->InputBands = InputBands;

	   /* Check for boundary conditions */
 	   zoomptr->StartLine = Min( zoomptr->StartLine, zoomptr->InputLines ) ;
 	   zoomptr->EndLine = Min( zoomptr->EndLine, zoomptr->InputLines ) ;
 	   zoomptr->StartSample = 
		Min( zoomptr->StartSample,InputSamples );
	   zoomptr->EndSample = 
		Min( zoomptr->EndSample,InputSamples ); 

	   /* Compute multiplicative magnification factor 	*/
 	   zoomptr->MulFactor = zoomptr->MagFactor;
 	   if(zoomptr->MagFactor < 0)
 		zoomptr->MulFactor = (-1 * (1.0/zoomptr->MagFactor));

	   /* Recompute minimum and maximum start lines 		  */
	   minzoomline = Min( zoomptr->OutStartLine, minzoomline );

	   zoomptr->InputSamples = zoomptr->EndSample - zoomptr->StartSample + 1;

	   /* Check to see if unit number is associated with a color band */
	   ptr = firstunit;
	   UnitFound = FALSE;
	   while ( ptr != NULL && !UnitFound)
		{
		if( ptr->UnitNumber == zoomptr->InputUnitNumber )
			{
			zoomptr->OutBand = ptr->BandNumber;
			zoomptr->ColorBands = 0;
			UnitFound = TRUE;
			}
		ptr = ptr->next;
		}
 	
 	   if(MagFactor>0)
 		{	
 	   	zoomptr->Input = (unsigned char **)calloc
 				(MagFactor+1,sizeof(unsigned char *));
     	        if( zoomptr->Input == NULL )
			{
			zvmessage("Memory allocation error; zoom aborted."," ");
			zoomptr->Ztype = NoZoom;
			}
 	   	for(k=0; k<=MagFactor; k++)
 			{
 			zoomptr->Input[k] = (unsigned char *)calloc
				(zoomptr->InputSamples*MagFactor,sizeof(unsigned char));
 			if(zoomptr->Input[k] == NULL)
			  {
 			  zvmessage("Memory allocation failed; zoom aborted"," ");
 			  zoomptr->Ztype = NoZoom;
 			  }
 			}
 		}

	   /* Check for stretch parameter value */
 	   if ( StretBand == 'F' || StretBand == 'f' )
 	      	{
 	        zoomptr->Stretch = Fixed ;
 	        temp1 = *Frac ;
 	        temp2 = *Satur ;
		if( bytes == 2 )
			{
			temp1 += 32768;
			temp2 += 32768;
			}
 	        zoomptr->SampleFraction = temp1 ;
 	        zoomptr->SaturationPercentage = temp2 ;
 	      	}
 	   else
 	      	if ( StretBand == 'A' || StretBand == 'a' )
 	      		{
 	         	zoomptr->Stretch = Adaptive ;
 	         	if ( *Frac > 1.0 || *Satur > 1.0 )
 	            		return( -1 ) ;
 	         	else
 	         		{
 	            		if (Number_Of_Element != 0 )
 	            			{	
 	               			zoomptr->NUM = Number_Of_Element ;
					zoomptr->EXC = (int *)calloc
						(Number_Of_Element,sizeof(int));
					if( zoomptr->EXC != NULL )
	 	               			for(j=0;j<Number_Of_Element;j++)
 		                     		   zoomptr->EXC[j] = ExClude[j];
					else
					  {
					  zvmessage("Memory allocation error"," ");
					  zvmessage(" - DNs excluded is NULL"," ");
					  zoomptr->NUM = -1;
					  }
 	            			}
 	 			zoomptr->SampleFraction = *Frac ;
 	            		zoomptr->SaturationPercentage = *Satur ;
 	         		}
 	      		}

	   /* Initialize lookup table for zoom image structure */
	   zoomptr->LUT = (int *)calloc(zoomptr->NumDNs,sizeof( int ));
           if( zoomptr->LUT != NULL )
		for ( j = 0 ; j < zoomptr->NumDNs ; j++ )	
 	           zoomptr->LUT[ j ] = j;		/* Lookup table j=j */
	   else
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" - za/xazoomimage aborted."," ");
		zoomptr->previous->next = NULL;
		return( -1 );
		}

 	   return ( 0 );
 	}
 	
  		
	/***************************/
	/* ZAZOOMPDSIMAGE	   */
	/***************************/
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME(xazoomPDSimage)(char *File, int *StLine, int *StSample,
		int *EnLine, int *EnSample, int *OutStLine, int *OutStSample,
		int *MagFactor, char Ztype[20], char *StretBand,
		float *Frac, float *Satur, int ExClude[],
		int *Number_Of_Element, ZFORSTR_PARAM)
#if 0
        char          Ztype[20];		/* Type of zoom performed    */
#endif
        {
	ZFORSTR_BLOCK
        char *c1_string;
	char *c2_string;
	char *c3_string;
        int length;

	zsfor2len(length, File, &File, 14, 1, 1, Number_Of_Element);
	c1_string = (char *)calloc(1,(length+1));
	zsfor2c(c1_string, length, File, &File, 14, 1, 1, Number_Of_Element);
	
        length = 20;
	c2_string = (char *)calloc(1,(length+1));
        zsfor2c(c2_string, length, Ztype, &File, 14, 9, 2, Number_Of_Element);

        zsfor2len(length, StretBand, &File, 14, 10, 3, Number_Of_Element); 
        c3_string = (char *)calloc(1,(length+1));	
        zsfor2c(c3_string, length, StretBand, &File, 14, 10,3,Number_Of_Element); 

 	zazoomPDSimage( c1_string, *StLine, *StSample, *EnLine, *EnSample, *OutStLine, 
                    *OutStSample, *MagFactor, c2_string, c3_string[0], Frac, Satur, 
		    *ExClude, *Number_Of_Element );

        free (c1_string);
        free (c2_string);
        free (c3_string);
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zazoomPDSimage(File, StLine, StSample, EnLine, EnSample, OutStLine, 
                    OutStSample, MagFactor, Ztype, StretBand, Frac, Satur, 
		    ExClude, Number_Of_Element )

	char	      *File;
        int           StLine, StSample, EnLine, EnSample, OutStLine,
                      OutStSample, MagFactor;
        char          Ztype[20];		/* Type of zoom performed    */
        unsigned char StretBand;
        float	      *Frac, *Satur;
  	int           Number_Of_Element, ExClude[];
 	{
  	   struct PDSZImageType *newblock;

  	   float fMaxDnValue, fMinDnValue;
 	   enum ZoomType Type;
	   char	string[MAXFILENAME]; 	   
 	   int 	i, j, k;				/* Loop control variable     */
 	   int  OutEndLine, OutBand, UnitAlreadyUsed,UnitFound, temp1, temp2,
		IOStatus, bytes, InputLines, InputSamples, InputBands;
	   int  flags;
	   int  record_bytes;
	   int	image_start_record; 	
	   int  index;
  	
 	   Type = 2;
 	   if(Ztype[0] == 'r' || Ztype[0] == 'R')
 	   	Type = 1;
 	   else
 	   	if(Ztype[0] == 'I' || Ztype[0] == 'i')
 	   		Type = 0;
 	
 	   /* If we get negative values Then error */
	   if ( strlen( File ) > MAXFILENAME ||
		strlen( File ) <= 0 ||
 	   	StLine   < 0 ||
 	        StSample < 0 || StSample > UserMaxSamples ||
 	        EnLine   < 0 ||
 	        EnSample < 0 || EnSample > UserMaxSamples ||
 	        OutStLine < 0 ||
 	        OutStSample < 0 || OutStSample > UserMaxSamples )
		return( -1 );

	   strcpy(string,File);
	
	   /* Determine pixel size of image data */
	   IOStatus = get_pds_image_stats( string, &InputLines, &InputSamples,
			&InputBands, &bytes, &record_bytes, &image_start_record );
	   if ( IOStatus < 0 || InputSamples != (record_bytes/bytes) )
		return -1;

	   if( bytes != 1 && bytes != 2 )
		{
		zvmessage("Image data format not halfword nor byte"," ");
		zvmessage("	- xa/zazoomPDSimage aborted"," ");
		return( -1 );
		}
	   if( bytes == 1 )
		{
		fMinDnValue = 0.0;
		fMaxDnValue = 255;
		}
	   else
		{
		fMinDnValue = -32768.0;
	   	fMaxDnValue = 32767;
		}

	   if ( StretBand != 'n' && StretBand != 'N' &&
		( *Frac < fMinDnValue || *Frac > fMaxDnValue ||
 	        *Satur < fMinDnValue || *Satur > fMaxDnValue ) )
		{
		zvmessage("Image stretch parameters are not valid"," ");
		zvmessage("	- xa/zazoomPDSimage aborted"," ");
		return( -1 );
		}

	   /* If Magnification factor is 1, do an xastoreimage */
	   if( MagFactor == 1 || MagFactor == -1 )
		{
 		zastorePDSimage(File, StLine, StSample, EnLine, EnSample, 
                             OutStLine, OutStSample, StretBand, Frac,
			     Satur, ExClude, Number_Of_Element );
		return( 0 );
		}

 	   /* Test input parameters and print appropriate messages to user */
 	   if( MagFactor % 2 != 0 && Type == 0 )
 	      	{
 	      	zvmessage("ODD ZOOM FACTORS ARE NOT ALLOWED WITH INTERPOLATION"," ");
	      	zvmessage("	- zoom aborted."," ");
 	      	return(-1);
 	      	}
 	   if(Type!=0 && Type!=1)
 		{
 		zvmessage("UNKNOWN ZOOMING METHOD"," ");
		zvmessage(" 	- zoom aborted."," ");
 		return(-2);
 		}
 	   if( MagFactor > 8 || MagFactor < -8 )
 		zvmessage("High zoom factor may mean longer execution time."," ");

 	   newblock = (struct PDSZImageType *)malloc(sizeof (struct PDSZImageType));
 	   if( !newblock )
 		{
 		zvmessage("Allocation error for zoomed image"," ");
 		zabend();
 		}
 	   if( !PDSzoomptr )
		{
 		firstPDSzoom = newblock;
		newblock->previous = NULL;
		}
 	   else
		{
 		PDSzoomptr->next = newblock;
		newblock->previous = PDSzoomptr;
		}
 	   newblock->next = NULL;
 	   PDSzoomptr = newblock;
 	
 	   /* Store with Start values < End values */
 	   strcpy(PDSzoomptr->InputFileName,string) ;		/* Store "good" data */
	   PDSzoomptr->fp = NULL;
	
 	   PDSzoomptr->StartLine = Min( StLine, EnLine )  ;
 	   PDSzoomptr->StartSample = Min( StSample, EnSample ) ;
 	   PDSzoomptr->EndLine = Max( StLine, EnLine ) ;
 	   PDSzoomptr->EndSample = Max( StSample, EnSample ) ;
 	   PDSzoomptr->OutStartLine = OutStLine ;
 	   PDSzoomptr->OutStartSample = OutStSample ;

 	   PDSzoomptr->OutBand = 0;
 	   PDSzoomptr->ColorBands = -1;
 	   PDSzoomptr->Ztype = Type ;
 	   PDSzoomptr->MagFactor = MagFactor;
	   PDSzoomptr->NumDNs = (int) pow(2.0,bytes*8.0);
	   PDSzoomptr->Stretch = NoStretch;
	   PDSzoomptr->NUM = -1;

	   PDSzoomptr->InputLines = InputLines;
	   PDSzoomptr->InputBands = InputBands;

	   PDSzoomptr->Bytes = bytes;
	   PDSzoomptr->RecordBytes = record_bytes;
	   PDSzoomptr->ImageStartRecord = image_start_record;

	   /* Check for boundary conditions */
 	   PDSzoomptr->StartLine   = Min( PDSzoomptr->StartLine, PDSzoomptr->InputLines );
 	   PDSzoomptr->EndLine     = Min( PDSzoomptr->EndLine, PDSzoomptr->InputLines );
 	   PDSzoomptr->StartSample = Min( PDSzoomptr->StartSample,InputSamples );
	   PDSzoomptr->EndSample   = Min( PDSzoomptr->EndSample,InputSamples ); 

	   /* Compute multiplicative magnification factor 	*/
 	   PDSzoomptr->MulFactor = PDSzoomptr->MagFactor;
 	   if(PDSzoomptr->MagFactor < 0)
 		PDSzoomptr->MulFactor = (-1 * (1.0/PDSzoomptr->MagFactor));

	   /* Recompute minimum and maximum start lines 		  */
	   minPDSzoomline = Min( PDSzoomptr->OutStartLine, minPDSzoomline );

	   PDSzoomptr->InputSamples = PDSzoomptr->EndSample - PDSzoomptr->StartSample + 1;;

	   
	   /* Register the image file name in the array of structures for	*/
	   /* tracking status of all PDS image files.				*/

	   index = get_pds_image_index(File);
	   if ( index < 0 )
		{
		index = PDSimagecount;	
		strcpy(pds_image[index].file,
			PDSzoomptr->InputFileName);
		pds_image[index].file_open_flag = FALSE;
		PDSimagecount++;
		}

	   pds_image[index].lines = InputLines;
	   pds_image[index].samples = InputSamples;
	   pds_image[index].bands = InputBands;
	   pds_image[index].bytes = bytes;
	   pds_image[index].record_bytes = record_bytes;
	   pds_image[index].start_record = image_start_record;
		
 	   if(MagFactor>0)
 		{	
 	   	PDSzoomptr->Input = (unsigned char **)calloc
 				(MagFactor+1,sizeof(unsigned char *));
     	        if( PDSzoomptr->Input == NULL )
			{
			zvmessage("Memory allocation error"," ");
			zvmessage("	- zoom aborted."," ");
			PDSzoomptr->Ztype = NoZoom;
			}
 	   	for(k=0; k<=MagFactor; k++)
 			{
 			PDSzoomptr->Input[k] = (unsigned char *)calloc
				(PDSzoomptr->InputSamples*MagFactor,sizeof(unsigned char));
 			if(PDSzoomptr->Input[k] == NULL)
			  	{
 			  	zvmessage("Memory allocation failed"," ");
			  	zvmessage(" 	-zoom aborted"," ");
	 			PDSzoomptr->Ztype = NoZoom;
 			  	}
 			}
 		}

	   /* Check for stretch parameter value */
 	   if ( StretBand == 'F' || StretBand == 'f' )
 	      	{
 	        PDSzoomptr->Stretch = Fixed ;
 	        temp1 = *Frac ;
 	        temp2 = *Satur ;
		if( bytes == 2 )
			{
			temp1 += 32768;
			temp2 += 32768;
			}
 	        PDSzoomptr->SampleFraction = temp1 ;
 	        PDSzoomptr->SaturationPercentage = temp2 ;
 	      	}
 	   else
 	      	if ( StretBand == 'A' || StretBand == 'a' )
 	      		{
 	         	PDSzoomptr->Stretch = Adaptive ;
 	         	if ( *Frac > 1.0 || *Satur > 1.0 )
 	            		return( -1 ) ;
 	         	else
 	         		{
 	            		if (Number_Of_Element != 0 )
 	            			{	
 	               			PDSzoomptr->NUM = Number_Of_Element ;
					PDSzoomptr->EXC = (int *)calloc
						(Number_Of_Element,sizeof(int));
					if( PDSzoomptr->EXC != NULL )
	 	               			for(j=0;j<Number_Of_Element;j++)
 		                     		   PDSzoomptr->EXC[j] = ExClude[j];
					else
					  {
					  zvmessage("Memory allocation error"," ");
					  zvmessage(" - DNs excluded is NULL"," ");
					  PDSzoomptr->NUM = -1;
					  }
 	            			}
 	 			PDSzoomptr->SampleFraction = *Frac ;
 	            		PDSzoomptr->SaturationPercentage = *Satur ;
 	         		}
 	      		}

	   /* Initialize lookup table for zoom image structure */
	   PDSzoomptr->LUT = (int *)calloc(PDSzoomptr->NumDNs,sizeof( int ));
           if( PDSzoomptr->LUT != NULL )
		for ( j = 0 ; j < PDSzoomptr->NumDNs ; j++ )	
 	           PDSzoomptr->LUT[ j ] = j;		/* Lookup table j=j */
	   else
		{
		zvmessage("Memory allocation error"," ");
		zvmessage(" - za/xazoomPDSimage aborted."," ");
		PDSzoomptr->previous->next = NULL;
		return( -1 );
		}

 	   return ( 0 );
 	}
 	
  		
 	/*********************/
 	/* Draw a scale      */
 	/*********************/
 	/*
 	Parameters :
 	
 	   Starting line
 	   Starting sample
 	   Ening line
 	   Ening sample
 	   DN value or DN array for color
 	*/
 	
/****************************************************************************/
/*         Fortran-Callable Version */
/****************************************************************************/
 	void FTN_NAME2(xascale, XASCALE) ( StartLine, StartSample,
						EnLine, EnSample, DnV )
 	int *StartLine, *StartSample, *EnLine, *EnSample, *DnV ;
        {
 	zascale ( *StartLine, *StartSample, *EnLine, *EnSample, DnV ) ;
        }
/****************************************************************************/
/*         C-Callable Version */
/****************************************************************************/
 	int zascale ( StartLine, StartSample, EnLine, EnSample, DnV )
 	int StartLine, StartSample, EnLine, EnSample, *DnV ;
 	{
 	   int DN_Value[3] ;
 	   int i,dm;
 
	   for(dm=0;dm<Dimension;dm++,DnV++)
		DN_Value[dm] = *DnV;
 	
 	   if (( StartSample != EnSample ) && (StartLine != EnLine)) 
 	   {
 	      zvmessage("The scale is neither horizontal nor vertical"," ") ;
 	      return( -1 ) ;
 	   }
 	   else 
 		{
 		for(dm=0;dm<Dimension;dm++)
 			{
 			if (( DN_Value[dm] < 0 ) || (DN_Value[dm] > 255 ))
 	   			{	
 	      			zvmessage("DN outside the range 0-255"," ") ;
 	      			return( -1 ) ;
 	   			}
 			}
 	     	if (ScaleVector[ 0 ].Type != 'C')
 	   		{
 	   		zvmessage("The center scale has not been defined"," ") ;
 	   		return( -1 ) ;
 	   		}
 	   	else 
 			if((ScaleVector[1].Type!='L') &&
			 (ScaleVector[2].Type!='R'))
 	   		 {
                         zvmessage(" Neither left nor right scale is defined."," ");
  	      		 return( -1 ) ;
 	   		 }
 	   		else    
			 if ( StartLine   < 0 ||
 	             	    StartSample < 0 || 
			    StartSample > UserMaxSamples ||
 	             	    EnLine   < 0    ||
 	             	    EnSample < 0    || EnSample > UserMaxSamples )
 	   		    {
		            zvmessage(" The scale falls outside of the image area."," ");
  	      		    return( -1 ) ;
 	   		    }
 		}
 	
	   /* Recompute minimum and maximum line numbers */
	   minscaleline = Min( StartLine, minscaleline );

 	   if (( StartSample == EnSample ) 
		&& (StartLine != EnLine))		/*Vertical Scale*/ 
 	     {
 	     if ( ScaleVector[ 0 ].Type == 'C')
 	        xxadrawcenterscale_V(StartLine,StartSample,EnLine, 	
			EnSample, DN_Value) ;
 	     if ( ScaleVector[ 1 ].Type == 'L') 
 	        xxadrawscale_V(StartLine, StartSample, EnLine, 
			EnSample, DN_Value, 1) ;
  	     if ( ScaleVector[ 2 ].Type == 'R') 
 	        xxadrawscale_V(StartLine, StartSample, EnLine, 
			EnSample, DN_Value, 2) ;
 	   }
 	   else 
		if (( StartSample != EnSample ) && 
			(StartLine == EnLine)) 		/* Horizontal Scale */
 	   		{
 	    	 	if ( ScaleVector[ 0 ].Type == 'C')
 	        		xxadrawcenterscale_H(StartLine,StartSample,
					EnLine, EnSample, DN_Value) ;
	 	     	if ( ScaleVector[ 1 ].Type == 'L') 
 	        		xxadrawscale_H(StartLine, StartSample, EnLine, 
					EnSample, DN_Value, 1) ;
			if ( ScaleVector[ 2 ].Type == 'R') 
 	         		xxadrawscale_H(StartLine, StartSample, EnLine, 
					EnSample, DN_Value, 2) ;
 	   		}
 	   return( 0 ) ;
	}
 	
  		
 	/********************************/
 	/* Draw Center Scale (Vertical) */
 	/********************************/
 	int xxadrawcenterscale_V (StartLine,StartSample,EnLine,EnSample,DN_Value) 
 	int StartLine, StartSample, EnLine, EnSample, DN_Value[] ;
 	{
	       /* Need to draw End Cap */
 	       if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	           ( ScaleVector[ 0 ].End_Cap_Length != 0))  
 	           {
			/* Top part of end cap */
 	           zastorebox( StartLine,
 	                     StartSample - ScaleVector[0].End_Cap_Length/2 , 
 	                     ScaleVector[0].End_Cap_Length,
 	                     ScaleVector[0].End_Cap_Width,
 	                     DN_Value ) ;

			/* Bottom part of end cap */
 	           zastorebox( EnLine - ScaleVector[0].End_Cap_Width + 1,
 	                     StartSample - ScaleVector[0].End_Cap_Length/2, 
 	                     ScaleVector[0].End_Cap_Length,
 	                     ScaleVector[0].End_Cap_Width,  
 	                     DN_Value ) ;
 		   }	
 	       
 	       if (ScaleVector[0].Center_Line_Width  != 0 ) /* Center line */
  		   zastorebox( StartLine + ScaleVector[0].End_Cap_Width,
 	                     StartSample - ScaleVector[0].Center_Line_Width/2 , 
 	                     ScaleVector[0].Center_Line_Width,
 	                     EnLine - StartLine - 
			     ScaleVector[0].End_Cap_Width * 2 + 1,
 	                     DN_Value ) ;
 	       return( 0 ) ;
 	}
 	
  		
 	/**********************************/
 	/* Draw Center Scale (Horizontal) */
 	/**********************************/
 	int xxadrawcenterscale_H(StartLine,StartSample,EnLine,EnSample,DN_Value) 
 	int StartLine, StartSample, EnLine, EnSample, DN_Value[] ;
 	{
		/* Need to draw End Cap */
 	       if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	           ( ScaleVector[ 0 ].End_Cap_Length != 0))  
		   { 	                  
 	             zastorebox( StartLine - ScaleVector[0].End_Cap_Length/2,
 	                     StartSample ,
 	                     ScaleVector[0].End_Cap_Width,
 	                     ScaleVector[0].End_Cap_Length,
 	                     DN_Value ) ;   /* Left side of the end cap */
 	             zastorebox( StartLine - ScaleVector[0].End_Cap_Length/2,
 	                     EnSample  - ScaleVector[0].End_Cap_Width +1 , 
 	                     ScaleVector[0].End_Cap_Width,
 	                     ScaleVector[0].End_Cap_Length,
 	                     DN_Value ) ;   /* Right side of the end cap */
 	      	   }
 			
 	       if (ScaleVector[0].Center_Line_Width  != 0 ) /* Center line */
 			zastorebox(StartLine - 
				ScaleVector[0].Center_Line_Width/2 ,
 	                     	StartSample + ScaleVector[0].End_Cap_Width , 
 	                     	EnSample - StartSample - 
				ScaleVector[0].End_Cap_Width * 2+1,
				ScaleVector[0].Center_Line_Width,
 	                     	DN_Value ) ;  /* Center part */
 			
 	       return( 0 ) ;
 	}
 
  		
 	/*****************************************************/
 	/* Draw Left Scale ( i = 1 ), Right Scacle ( i = 2 ) */
 	/*****************************************************/
 	int xxadrawscale_V ( StartLine, StartSample, EnLine, EnSample, DN_Value, i) 
 	int StartLine, StartSample, EnLine, EnSample, DN_Value[], i ;
 	{
 	   int Adjusted_StartLine ;
 	   int Tick_StartLine[50], Tick_EndLine[50], Tick_StartSample[50] ;
 	   double Tick_StartLineF[50] ;
 	   int j, line ;
 	   int Last_Line_To_Draw_Tick ;
 	
 	   /*  Adjust the start line to be in the middle of the top end cap */
		/* Need to draw End Cap */
 	   if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	       ( ScaleVector[ 0 ].End_Cap_Length != 0))  
 	      Adjusted_StartLine = StartLine + ScaleVector[0].End_Cap_Width/2 ;
 	
 	   for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	   {
			/*End Cap is not needed */
 	         if ((ScaleVector[ 0 ].End_Cap_Width == 0) || 
 	            ( ScaleVector[ 0 ].End_Cap_Length == 0))
 	         {
			/* no offset */
 	          if (ScaleVector[ i ].Tick_Increment[j][1] == 0.0 )
 	          {
 	              Tick_StartLineF[j]  =  StartLine ;
 	              Tick_StartLine[j]   =  Tick_StartLineF[j] ;
 	          }
 	          else
 	          {
 	              Tick_StartLineF[j]  =  StartLine +        /* offset */
 	                	ScaleVector[ i ].Tick_Increment[j][1] ;
 	              Tick_StartLine[j]   =  Tick_StartLineF[j] ;
			/* adjust if negative offset*/
 	              while(Tick_StartLine[j]<StartLine ) 
 	              {
 	                Tick_StartLineF[j] = Tick_StartLineF[j] + 
 	                 	ScaleVector[ i ].Tick_Increment[j][0];
 	                Tick_StartLine[j]  = Tick_StartLineF[j] ;
 	              }
 	          }
 	         }
 	         else         /* End Cap is used */
 	         {             
			/* no offset */
 	           if (ScaleVector[ i ].Tick_Increment[j][1] == 0.0 )
 	           {
 	             Tick_StartLineF[j]  =  Adjusted_StartLine +
 	                	ScaleVector[ i ].Tick_Increment[j][0] - 
 	                        ScaleVector[ i ].Tick_Width[j]/2 ;
 	             Tick_StartLine[j]   =  Tick_StartLineF[j] ;
 	           }
 	           else
 	           {
 	             Tick_StartLineF[j]  =  Adjusted_StartLine +  /* offset */
 	                        ScaleVector[ i ].Tick_Increment[j][1] +
 	                        ScaleVector[ i ].Tick_Increment[j][0] - 
 	                        ScaleVector[ i ].Tick_Width[j]/2 ;
 	             Tick_StartLine[j]   =  Tick_StartLineF[j] ;
 	           }
 	           /* The first tick mark has to be at least one pixel distance
 	              to the end cap, if not, increment to the next one */
 	           while (Tick_StartLine[j] <=
			(StartLine+ScaleVector[0].End_Cap_Width))
 	           	{
 	               	Tick_StartLineF[j] = Tick_StartLineF[j] + 
 	                 	ScaleVector[ i ].Tick_Increment[j][0];
 	               	Tick_StartLine[j]  = Tick_StartLineF[j] ;
 	           	}
 	         }
 	         Tick_EndLine[j] = Tick_StartLine[j] + 
 	                           ScaleVector[ i ].Tick_Width[j] - 1 ; 
 	
 	         if ( i == 2 )   /* no adjustment needed for right scale */
 	              Tick_StartSample[j] = StartSample ;  
 	         else if ( i == 1 )
 	              Tick_StartSample[j] = StartSample -
 	            		ScaleVector[ i ].Tick_Length[j] + 1 ;
 	   }
 	
 	   if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	      ( ScaleVector[ 0 ].End_Cap_Length != 0))  
/* Need to draw End Cap */
/* The first tick mark has to be at least one pixel away to top end cap */
/* The last tick mark has to be at least one pixel away to bottom end cap*/
 	   {
 	     line = StartLine + ScaleVector[0].End_Cap_Width +1 ; 
 	     Last_Line_To_Draw_Tick = EnLine - ScaleVector[0].End_Cap_Width -1;
 	   }
 	   else
 	   {
 	     line = StartLine ; 
 	     Last_Line_To_Draw_Tick = EnLine ;
 	   }
 	
 	   while (line <= Last_Line_To_Draw_Tick) /* Process line by line */
 	   {
 	     for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	     {
 	      if (( line == Tick_StartLine[j] )  &&
 	        ((line + ScaleVector[ i ].Tick_Width[j]-1) <= 
		Last_Line_To_Draw_Tick))
 	        {
 	          if (( j == 0 ) || ( Tick_EndLine[j]<(Tick_StartLine[j-1] -1)))
 	        /* The first set has highest priority, starting from 
 	        the second set, has to make sure it doesn't interfere with 
 	        the previous set, for example, if the second set tick occupies 
 	        lines 20,21, the first set tick occupies lines 22,23,24, don't
 	        draw the second set, leave the space for the first set */
 	          {
            	     zastorebox( line,
 	                         Tick_StartSample[j],
 	                         ScaleVector[ i ].Tick_Length[j],
 	                         ScaleVector[ i ].Tick_Width[j],
 	                         DN_Value ) ;
 			/* add Annotation*/
 	             if ( ScaleVector[ i ].Annotation_Position[j] != 0) 	
               		xxadrawAnnotation_V ( line, Tick_StartSample[j], 
				i, j, DN_Value) ;
 	             Tick_StartLineF[j] = Tick_StartLineF[j] + 
 	                        ScaleVector[ i ].Tick_Increment[j][0] ;
 	             Tick_StartLine[j] =  Tick_StartLineF[j] ;
 	             Tick_EndLine[j] = Tick_StartLine[j] +   /* Update */
 	                               ScaleVector[ i ].Tick_Width[j] - 1 ; 
 	             line = line + ScaleVector[ i ].Tick_Width[j] ;
 	             break ;
 	          }
 	      }
 	     }
 	     line ++ ; 
 	     for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	     {
 	       if (line > Tick_StartLine[j] ) 
		/* Make sure the starting line of each set
 	        keeps up with the current line, also 
 	        increment the digit even the tick is
 	        not drawn */
 	       {
 	           Tick_StartLineF[j] = Tick_StartLineF[j] +
 	                                ScaleVector[ i ].Tick_Increment[j][0] ; 
 	           Tick_StartLine[j]  = Tick_StartLineF[j] ;
 	           Tick_EndLine[j] = Tick_StartLine[j] + 
 	                             ScaleVector[ i ].Tick_Width[j] - 1 ; 
			/*need Annotation*/
 	           if(ScaleVector[ i ].Annotation_Position[j] != 0)
			xxaIncrementAnnotation(i,j);
 	       }
 	     }
 	   }
 	   return( 0 ) ;
 	}

 	/*****************************************************/
 	/* Increment Annotation of Scale, check Modulus	     */
 	/*****************************************************/
	void xxaIncrementAnnotation(i,j)
	int i,j;
	{

	ScaleVector[ i ].Annotation_Start_Value[j] +=
		ScaleVector[ i ].Annotation_Increment[j];
	if(ScaleVector[ i ].Annotation_Modulus[j] > 0)
		ScaleVector[ i ].Annotation_Start_Value[j] -= 
			((int)(ScaleVector[ i ].Annotation_Start_Value[j]/
			ScaleVector[ i ].Annotation_Modulus[j]) *
			ScaleVector[ i ].Annotation_Modulus[j]);
	} 	
  		
 	/*****************************************************/
 	/* Draw Top Scale ( i = 2 ), Bottom Scacle ( i = 1 ) */
 	/*****************************************************/
 	int xxadrawscale_H ( StartLine, StartSample, EnLine, EnSample, DN_Value, i) 
 	int StartLine, StartSample, EnLine, EnSample, DN_Value[], i ;
 	{
 	   int Adjusted_StartSample ;
 	   int Tick_StartLine[50], Tick_EndSample[50], Tick_StartSample[50] ;
 	   double Tick_StartSampleF[50] ;
 	   int j, sample ;
 	   int Last_Sample_To_Draw_Tick ;
 	
 	   /*  Adjust the start line to be in the middle of the top end cap */
		/* Need to draw End Cap */
 	   if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	       ( ScaleVector[ 0 ].End_Cap_Length != 0))  
 	      Adjusted_StartSample = StartSample + 
			ScaleVector[0].End_Cap_Width/2 ;
 	
 	   for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	   {
			/*End Cap is not needed */
 	         if ((ScaleVector[ 0 ].End_Cap_Width == 0) || 
 	            ( ScaleVector[ 0 ].End_Cap_Length == 0))
 	         {
			/* no offset */
 	          if (ScaleVector[ i ].Tick_Increment[j][1] == 0.0 )
 	          {
 	              Tick_StartSampleF[j]  =  StartSample ;
 	              Tick_StartSample[j]   =  Tick_StartSampleF[j] ;
 	          }
 	          else
 	          {
 	              Tick_StartSampleF[j]  =  StartSample +        /* offset */
 	                 	ScaleVector[ i ].Tick_Increment[j][1] ;
 	              Tick_StartSample[j]   =  Tick_StartSampleF[j] ;
			/* adjust if negative offset*/
 	              while(Tick_StartSample[j]<StartSample ) 
 	              {
 	                Tick_StartSampleF[j] = Tick_StartSampleF[j] + 
 	                	ScaleVector[ i ].Tick_Increment[j][0];
 	                Tick_StartSample[j]  = Tick_StartSampleF[j] ;
 	              }
 	          }
 	         }
 	         else         /* End Cap is used */
 	         {             
			/* no offset */
 	           if (ScaleVector[ i ].Tick_Increment[j][1] == 0.0 )
 	           {
 	             Tick_StartSampleF[j]  =  Adjusted_StartSample +
 	                	ScaleVector[ i ].Tick_Increment[j][0] - 
 	                        ScaleVector[ i ].Tick_Width[j]/2 ;
 	             Tick_StartSample[j]   =  Tick_StartSampleF[j] ;
 	           }
 	           else
 	           {
			/* offset */
 	             Tick_StartSampleF[j]  =  Adjusted_StartSample +  
 	                	ScaleVector[ i ].Tick_Increment[j][1] +
 	                        ScaleVector[ i ].Tick_Increment[j][0] - 
 	                        ScaleVector[ i ].Tick_Width[j]/2 ;
 	             Tick_StartSample[j]   =  Tick_StartSampleF[j] ;
 	           }
 	           /* The first tick mark has to be at least one pixel distance
 	              to the end cap, if not, increment to the next one */
 	           while (Tick_StartSample[j] <=
				(StartSample+ScaleVector[0].End_Cap_Width))
 	           {
 	               Tick_StartSampleF[j] = Tick_StartSampleF[j] + 
 	                        ScaleVector[ i ].Tick_Increment[j][0];
 	               Tick_StartSample[j]  = Tick_StartSampleF[j] ;
 	           }
 	         }
 	         Tick_EndSample[j] = Tick_StartSample[j] + 
 	                           ScaleVector[ i ].Tick_Width[j] - 1 ; 
 	
 	         if ( i == 1 )   /* no adjustment needed for bottom scale */
 	              Tick_StartLine[j] = StartLine ;  
 	         else if ( i == 2 )
 	              Tick_StartLine[j] = StartLine -
 	                                  ScaleVector[ i ].Tick_Length[j] + 1 ;
 	   }
 	
 	   if (( ScaleVector[ 0 ].End_Cap_Width != 0) &&
 	      ( ScaleVector[ 0 ].End_Cap_Length != 0))  
		/* Need to draw End Cap */
/* The first tick mark has to be at least one pixel away to left end cap */
/* The last tick mark has to be at least one pixel away to right end cap*/
 	   {
 	     sample = StartSample + ScaleVector[0].End_Cap_Width +1 ; 
 	     Last_Sample_To_Draw_Tick = EnSample - 
			ScaleVector[0].End_Cap_Width -1;
 	   }
 	   else
 	   {
 	     sample = StartSample ; 
 	     Last_Sample_To_Draw_Tick = EnSample ;
 	   }
 	
 	   while (sample <= Last_Sample_To_Draw_Tick) /* Process samplewise */
 	   {
 	     for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	     {
 	      if (( sample == Tick_StartSample[j] )  &&
 	        ((sample +ScaleVector[ i ].Tick_Width[j]-1) <= 
		Last_Sample_To_Draw_Tick))
 	      {
 	          if (( j == 0 ) || ( Tick_EndSample[j] < 
			(Tick_StartSample[j-1] -1)))
 	      /* The first set has highest priority, starting from 
 	      the second set, has to make sure it doesn't interfere with 
 	      the previous set, for example, if the second set tick occupies 
 	      samples 20,21, the first set tick occupies samples 22,23,24,
 	      don't draw the second set, leave the space for the first set */
 	          {
 	             zastorebox( Tick_StartLine[j],
 	                         sample,
 	                         ScaleVector[ i ].Tick_Width[j],
 	                         ScaleVector[ i ].Tick_Length[j],
 	                         DN_Value ) ;
 	
			/* add Annotation*/
 	             if ( ScaleVector[ i ].Annotation_Position[j] != 0)
 	               xxadrawAnnotation_H (Tick_StartLine[j], sample, i, j, 
				DN_Value) ;
 	             Tick_StartSampleF[j] = Tick_StartSampleF[j] + 
 	                        ScaleVector[ i ].Tick_Increment[j][0] ;
 	             Tick_StartSample[j] =  Tick_StartSampleF[j] ;
 	             Tick_EndSample[j] = Tick_StartSample[j] +   /* Update */
 	                                 ScaleVector[ i ].Tick_Width[j] - 1 ; 
 	             sample = sample + ScaleVector[ i ].Tick_Width[j] ;
 	             break ;
 	          }
 	      }
 	     }
 	     sample ++ ; 
 	     for ( j = 0 ; j < ScaleVector[i].Array_Length ; j ++ )
 	     {
 	       if (sample > Tick_StartSample[j] ) 
		/* Make sure the starting sample of
 	        each set keeps up with the current 
 	        line, also increment the digit even
 	        the tick is not drawn */
 	       {
 	           Tick_StartSampleF[j] = Tick_StartSampleF[j] +
 	                                ScaleVector[ i ].Tick_Increment[j][0] ; 
 	           Tick_StartSample[j]  = Tick_StartSampleF[j] ;
 	           Tick_EndSample[j] = Tick_StartSample[j] + 
 	                             ScaleVector[ i ].Tick_Width[j] - 1 ; 
			/*need Annotation*/
 	           if(ScaleVector[ i ].Annotation_Position[j] != 0)
	 	              xxaIncrementAnnotation(i,j);
 	       }
 	     }
 	   }
 	   return( 0 ) ;
 	}
 	
  		
 	/**********************************/
 	/* Draw Annotation, Vertical Scale */
 	/**********************************/
 	/*
 	   Starting line
 	   Starting sample
 	   j = number of tick set 
 	   DN value or DN array for color
 	*/
 	int xxadrawAnnotation_V(Annotation_StartLine,
		Annotation_StartSample,i,j,DN_Value)
 	int Annotation_StartLine, Annotation_StartSample, i, j, DN_Value[] ;
 	{
 	   int FontThickness, width, count ; 
 	   char ss[30] ;
 	   int Value, CharHeight, StringLen, VerticalOffset, HorizontalOffset, 
 	       OldAngle, DigitHeight;

	DigitHeight = 0; 	/* Initialize to zero */


	/* Perform modulus operation */
	if(ScaleVector[ i ].Annotation_Modulus[j] > 0)
	 	ScaleVector[ i ].Annotation_Start_Value[j] -= 
		((int)(ScaleVector[ i ].Annotation_Start_Value[j]/
		      ScaleVector[ i ].Annotation_Modulus[j]) *
  		      ScaleVector[ i ].Annotation_Modulus[j]);

 	/* convert double to string */
 	   if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 0)
 	   {                                      
 	     Value = ScaleVector[ i ].Annotation_Start_Value[j] ;
 	     sprintf(ss,"%d",(int)(Value)); 
 	   }
 	   else if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 1)
 	     sprintf(ss,"%.1f",ScaleVector[ i ].Annotation_Start_Value[j]);
 	   else if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 2)
 	     sprintf(ss,"%.2f",ScaleVector[ i ].Annotation_Start_Value[j]);

           CharHeight = XACHARHEIGHT * ScaleVector[ i ].Annotation_Size[j] ;
           for( count=1; count<=ScaleVector[i].Annotation_Size[j]; count++)
		if(count%2 == 0)
			DigitHeight += 4;
		else
			DigitHeight += 3;

 	   if ( XAFONTTYPE == HERSHEY )
 	       {
 	        FontThickness = 2 ;
 	        width = CharHeight * 0.6 ;
 	       }
 	   else
 	       {
 	        FontThickness = 1 ;
 	        if ( (CharHeight % 2) == 0 )
 	            width = CharHeight/2  ;
 	        else
 	            width = CharHeight/2 + 1 ;
 	       }
 
	   /* Determine string length */	
 	   zastringlength ( ss, ScaleVector[ i ].Annotation_Size[j], 
 	                    FontThickness, &StringLen ) ; 

 	   VerticalOffset = 1 + ScaleVector[i].Annotation_Size[j]/2 ;
 	   HorizontalOffset = 1 + ScaleVector[i].Annotation_Size[j]/2 ;
 	
 	   if (ScaleVector[ i ].Annotation_Orientation[j] == 'L')
		/* rotate -90 degrees for left orientated text */
 	     	OldAngle = zasetfontangle (-90) ; 
 	   else if (ScaleVector[ i ].Annotation_Orientation[j] == 'R')
		/* rotate 90 degrees for right orientated text */
 	    	OldAngle = zasetfontangle (90) ; 
 	   else
		/* no rotation needed for vertical orientated text */
		OldAngle = zasetfontangle (0); 
 	
 	   if ( i == 1)     
		/* Draw annotation for the left scale */
 	     	xxadrawAnnotation_VL(Annotation_StartLine, 
			Annotation_StartSample, j,DN_Value, FontThickness, 
			VerticalOffset, HorizontalOffset, ss, CharHeight, 
			StringLen, width, DigitHeight ) ;
 	                          
 	   else   
		/* Draw annotation for the right scale */
 	     	xxadrawAnnotation_VR (Annotation_StartLine, 
			Annotation_StartSample, j, DN_Value, FontThickness, 
			VerticalOffset, HorizontalOffset, ss, CharHeight, 
			StringLen, width, DigitHeight ) ;
 	
           xxaIncrementAnnotation(i,j);

 	   zasetfontangle (OldAngle);
 	   return( 0 ) ;
 	}
 
  		
 	/*****************************************/
 	/* Draw Annotation, Vertical Scale, Right */
 	/*****************************************/
 	int xxadrawAnnotation_VR (Annotation_StartLine, Annotation_StartSample, j, 
 		DN_Value, FontThickness, VerticalOffset, HorizontalOffset, ss, 
		CharHeight, StringLen, width, DigitHeight ) 
 	int Annotation_StartLine, Annotation_StartSample, j, 
		DigitHeight, DN_Value[], FontThickness, 
 		CharHeight, StringLen, VerticalOffset, HorizontalOffset, width ;
 	char ss[30] ;
 	{
 	   int sl, sam;
	   
	   /* Test for annotation placed above tick marks */
 	   if ( ScaleVector[ 2 ].Annotation_Position[j] == 'T') 
 	   {
             /*  0 degrees  */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
 	     	{
                /*  Add the following if statement, so annotation will not 
                    attach to the left/right tick 3/9/93   ... FFM   */

                 if ( XAFONTTYPE == HERSHEY )
     	       	      sl = Annotation_StartLine - VerticalOffset - CharHeight -  
			FontThickness ;
                 else
     	       	      sl = Annotation_StartLine - VerticalOffset - DigitHeight -  
			FontThickness ;
	
		/* Check Annotation Justification */
		 if(ScaleVector[2].Annotation_Justification[j] == 'R')
			sam = Annotation_StartSample + 
				(0.75*ScaleVector[ 2 ].Tick_Length[j]);
	   	 else
			sam = Annotation_StartSample + 
				ScaleVector[2].Tick_Length[j] - StringLen - 1;
 	     	}
		  /* Check Annotation Orientation/rotation: +/- 90 degrees */
 	     else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R' || 
 	               ScaleVector[ 2 ].Annotation_Orientation[j] == 'L')  
 	     	{
 	       	sl = Annotation_StartLine - VerticalOffset - StringLen - 1  ;
 	       	sam = Annotation_StartSample + 
			ScaleVector[ 2 ].Tick_Length[j] - DigitHeight - 2 ;
 	     	}
 	                   
 	     zastorestring( sl ,
 	                    sam,
 	                    ScaleVector[ 2 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
	   /* Test for annotation placed below tick marks */
 	   else if (ScaleVector[ 2 ].Annotation_Position[j]=='B')
 	   {
	     /*  0 degrees  */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
		
		/* Check Annotation justification */
	   	if(ScaleVector[2].Annotation_Justification[j] == 'R')
			sam = Annotation_StartSample + 
				(0.75*ScaleVector[ 2 ].Tick_Length[j]);
	   	else
			sam = Annotation_StartSample + 
				ScaleVector[2].Tick_Length[j] - StringLen - 1;

		  /* Check Annotation orientation/rotation: +/- 90 degrees */
 	     else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R' ||  
 	               ScaleVector[ 2 ].Annotation_Orientation[j] == 'L')   
 	       sam = Annotation_StartSample + 
			ScaleVector[ 2 ].Tick_Length[j] - DigitHeight - 2  ;
 	  
 	     sl = Annotation_StartLine + ScaleVector[ 2 ].Tick_Width[j] +
 	          VerticalOffset + FontThickness ; 
 	
 	     zastorestring( sl ,
 	                    sam ,
 	                    ScaleVector[ 2 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
	   /* Test for annotation placed parrallel to tick marks */
 	   else if (ScaleVector[ 2 ].Annotation_Position[j]=='C')
 	   {
             /*  0 degrees  */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
 	     {
              /*  Add the following if statement, so annotation will be 
                  centered  when parallel to the left/right tick 3/9/93
                  ...FFM  */ 
             if ( XAFONTTYPE == HERSHEY )
         	sl=Annotation_StartLine-CharHeight/2;
             else
         	sl=Annotation_StartLine-(DigitHeight+FontThickness)/2+2; 
 	       sam = Annotation_StartSample + ScaleVector[ 2 ].Tick_Length[j] 
			+ HorizontalOffset ;
/* 	       if ( ss[0] == '-' )
 	        sam += width+XACHARSPACING*ScaleVector[ 2 ].Annotation_Size[j] ;
 */	     }
		  /* Check Annotation orientation/rotation: +/- 90 degrees */
 	     else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R' || 
 	               ScaleVector[ 2 ].Annotation_Orientation[j] == 'L')  
 	     {
 	       sl = Annotation_StartLine - StringLen/2 ;
 	       sam = Annotation_StartSample + ScaleVector[ 2 ].Tick_Length[j] + 
 	             HorizontalOffset ;
 	     }
 	
	     zastorestring(sl,
 	                   sam,
 	                   ScaleVector[ 2 ].Annotation_Size[j],
 	                   DN_Value,
 	                   ss,
 	                   FontThickness ) ;
 	   }
 	   return (0) ;
 	}
 	
  		
 	/****************************************/
 	/* Draw Annotation, Vertical Scale, Left */
 	/****************************************/
 	int xxadrawAnnotation_VL (Annotation_StartLine, Annotation_StartSample, j, 
 	   	DN_Value, FontThickness, VerticalOffset, HorizontalOffset, ss,
		CharHeight, StringLen, width, DigitHeight ) 
 	int Annotation_StartLine, Annotation_StartSample, j, DN_Value[], 
		FontThickness,CharHeight, StringLen, VerticalOffset, 
		HorizontalOffset, width, DigitHeight ;
 	char ss[30] ;
 	{
 	   int sl, sam, l1 ;

           sam = Annotation_StartSample;

           /* Test for annotation placed above tick marks */
 	   if ( ScaleVector[ 1 ].Annotation_Position[j] == 'T') 
 	   {
		/*  0 degrees rotation  */
 	     if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V')
 	     {
              /*  Add the following if statement, so annotation will not 
                  attach to the left/right tick 3/9/93   ... FFM   */

               if ( XAFONTTYPE == HERSHEY )
 	          sl = Annotation_StartLine - VerticalOffset - CharHeight -  
		       FontThickness ;
               else
 	          sl = Annotation_StartLine - VerticalOffset - DigitHeight -  
		       FontThickness ;
	       /* Check Annotation Justification */
	       if( ScaleVector[ 1 ].Annotation_Justification[j] == 'R')
		  sam += (ScaleVector[1].Tick_Length[j]*0.25) - StringLen - 1;

 	       if(ss[0]=='-')
		sam -= width-XACHARSPACING*ScaleVector[1].Annotation_Size[j]+1;
 	     }
		  /* +/- 90 degrees rotation */	
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'R' || 
 	               ScaleVector[ 1 ].Annotation_Orientation[j] == 'L') 
 	       	sl = Annotation_StartLine - VerticalOffset - StringLen - 1  ;

 	     zastorestring( sl ,
 	                    sam,
 	                    ScaleVector[ 1 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
	   /* Test for annotation placed below tick marks */
 	   else if (ScaleVector[ 1 ].Annotation_Position[j]=='B')
 	   {
 	      sl = Annotation_StartLine + ScaleVector[ 1 ].Tick_Width[j] +
 	                    VerticalOffset + FontThickness ; 

	      /* 0 degrees rotation */
 	      if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V' ) 
		{
		if( ScaleVector[1].Annotation_Justification[j] == 'R')
		      	sam += (ScaleVector[1].Tick_Length[j]*0.25) 
				- StringLen - 1;
 	        if ( ss[0] == '-' )
 	          sam -=width-XACHARSPACING*ScaleVector[1].Annotation_Size[j]+1;
 		}

       	        zastorestring( sl ,
 	                    sam,
 	                    ScaleVector[ 1 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	   }
	   /* Test for annotation placed parallel to tick marks */
 	   else if (ScaleVector[ 1 ].Annotation_Position[j]=='C')
 	   {
		/*  0 degrees rotation */
 	     if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V')
 	     {
              /*  Add the following if statement, so annotation will be 
                  centered  when parallel to the left/right tick 3/9/93
                  ...FFM  */ 

             if ( XAFONTTYPE == HERSHEY )
         	sl=Annotation_StartLine-CharHeight/2;
             else
         	sl=Annotation_StartLine-(DigitHeight+FontThickness)/2+2; 

 	       sam -= HorizontalOffset + FontThickness + StringLen ;
 	     }
		/* +/- 90 degrees rotation */
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'R' || 
 	               ScaleVector[ 1 ].Annotation_Orientation[j] == 'L') 
 	     {
 	       sl = Annotation_StartLine - StringLen/2 + 1 ;
              /*  Add the following if statement, so annotation will be 
                  closer to the left tick mark  with -90 rotation 3/9/93
                  ...FFM  */ 
               if ( XAFONTTYPE != HERSHEY  &&
 	            ScaleVector[ 1 ].Annotation_Orientation[j] == 'L')
 	          sam -= HorizontalOffset + DigitHeight + FontThickness + 1 ;
               else
 	          sam -= HorizontalOffset + CharHeight + FontThickness + 1 ;
 	     }
 	       zastorestring(sl,
                           sam,				
 	                   ScaleVector[ 1 ].Annotation_Size[j],
 	                   DN_Value,
 	                   ss,
 	                   FontThickness ) ;
 	
 	   }
 	
 	   return (0) ;
 	}
 
  		
 	/*************************************/
 	/* Draw Annotation, Horizontal Scale */
 	/*************************************/
 	/*
 	   Starting line
 	   Starting sample
 	   j = number of tick set 
 	   DN value or DN array for color
 	*/
 	int xxadrawAnnotation_H(Annotation_StartLine,
		Annotation_StartSample,i,j,DN_Value)
 	int Annotation_StartLine, Annotation_StartSample, i, j, DN_Value[] ;
 	{
 	   int FontThickness, width ; 
 	   char ss[30] ;
 	   int Value, CharHeight, StringLen, VerticalOffset, 
 	       count, HorizontalOffset, OldAngle, DigitHeight;
	
	   DigitHeight = 0;
	 	

	   /* Perform modulus operation */
	   if(ScaleVector[ i ].Annotation_Modulus[j] > 0)
	      ScaleVector[ i ].Annotation_Start_Value[j] -= 
	      ((int)(ScaleVector[ i ].Annotation_Start_Value[j]/
		 ScaleVector[ i ].Annotation_Modulus[j]) *
		 ScaleVector[ i ].Annotation_Modulus[j]);

 	   /* convert double to string */
 	   if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 0)
 	   {                                      
 	     Value = ScaleVector[ i ].Annotation_Start_Value[j] ;
 	     sprintf(ss,"%d",(int)(Value)); 
 	   }
 	   else if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 1)
 	     sprintf(ss,"%.1f",ScaleVector[ i ].Annotation_Start_Value[j]);
 	   else if (ScaleVector[ i ].Annotation_Significant_Digits[j] == 2)
 	     sprintf(ss,"%.2f",ScaleVector[ i ].Annotation_Start_Value[j]);

           CharHeight = XACHARHEIGHT * ScaleVector[ i ].Annotation_Size[j] ;
           for( count=1; count<=ScaleVector[i].Annotation_Size[j]; count++)
		if(count%2 == 0)
			DigitHeight += 4;
		else
			DigitHeight += 3;
 	
 	   if ( XAFONTTYPE == HERSHEY )
 	       {
 	        FontThickness = 2 ;
 	        width = CharHeight * 0.6 ;
 	       }
 	   else
 	       {
 	        FontThickness = 1 ;
 	        if ( (CharHeight % 2) == 0 )
 	            width = CharHeight/2  ;
 	        else
 	            width = CharHeight/2 + 1 ;
 	       }
 	
	   /* Determine string length */
 	   zastringlength ( ss, ScaleVector[ i ].Annotation_Size[j], 
 	                    FontThickness, &StringLen ) ; 
 	
	   VerticalOffset = 1 + ScaleVector[i].Annotation_Size[j]/2 ;
 	   HorizontalOffset = 1 + ScaleVector[i].Annotation_Size[j]/2 ;
 	
 	   if (ScaleVector[ i ].Annotation_Orientation[j] == 'L')
	  	/* rotate -90 degrees for left orientated text */
 	     	OldAngle = zasetfontangle (-90) ; 
 	   else if (ScaleVector[ i ].Annotation_Orientation[j] == 'R')
		/* rotate 90 degrees for right orientated text */
 	     	OldAngle = zasetfontangle (90) ; 
 	   else
		/* no rotation needed for vertical orientated text */
 	     	OldAngle = zasetfontangle (0); 
 	
 	   if ( i == 1) 
		/* Draw annotation for bottom scale */
 	     	xxadrawAnnotation_HB(Annotation_StartLine, 
			Annotation_StartSample, j, DN_Value, FontThickness, 
			VerticalOffset, HorizontalOffset, ss, CharHeight, 
			StringLen, width, DigitHeight ) ;
 	   else   	
		/* Draw annotation for top scale */
 	     	xxadrawAnnotation_HT(Annotation_StartLine, 
			Annotation_StartSample, j, DN_Value, FontThickness, 
			VerticalOffset, HorizontalOffset, ss, CharHeight, 
			StringLen, width, DigitHeight) ;
 	
	   xxaIncrementAnnotation(i,j);
 	
 	   zasetfontangle( OldAngle );
 	   return( 0 ) ;
 	}
 	
  		
 	/******************************************/
 	/* Draw Annotation, Horizontal Scale, Top */
 	/******************************************/
 	int xxadrawAnnotation_HT (Annotation_StartLine, Annotation_StartSample, j, 
 	   	DN_Value, FontThickness, VerticalOffset, HorizontalOffset, ss,
		CharHeight, StringLen, width, DigitHeight) 
 	int Annotation_StartLine, Annotation_StartSample, j, DN_Value[], 
		FontThickness, CharHeight, StringLen, VerticalOffset, 
		HorizontalOffset, width, DigitHeight ;
 	char ss[30] ;
 	{
 	   int sl, sam ;

	   /* Test for annotation placed to the left of tick marks */
 	   if ( ScaleVector[ 2 ].Annotation_Position[j] == 'T') 
 	   {
 	     sl = Annotation_StartLine ;

	     /*  0 degrees rotation */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
 	       sam = Annotation_StartSample - HorizontalOffset - StringLen - 2 ;
 	     else
		/* +/- 90 degrees rotation */ 
                /*  Add the following if statement, so annotation will be 
                    closer to the left tick mark  with -90 rotation 3/9/93
                     ...FFM  */ 
                {  
                if ( XAFONTTYPE != HERSHEY  &&
 	             ScaleVector[ 2 ].Annotation_Orientation[j] == 'L')
  	             sam = Annotation_StartSample - HorizontalOffset - 
		      	   DigitHeight - FontThickness - 2 ;
                else
  	             sam = Annotation_StartSample - HorizontalOffset - 
			   CharHeight - FontThickness - 2 ;

	        if( ScaleVector[ 2 ].Annotation_Justification[j] == 'R' )
		    sl += ScaleVector[ 2 ].Tick_Length[j]*(0.25)-StringLen;
                }
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R' &&
		  ScaleVector[ 2 ].Annotation_Justification[j] == 'L' )  
 	       if ( ss[0] == '-' )
 	         sl -= XACHARSPACING*ScaleVector[2].Annotation_Size[j] + width ;
 	                   
	         zastorestring( sl ,
 	                    sam,
 	                    ScaleVector[ 2 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
	   /* Test for annotation placed to the right of tick mark */
 	   else if (ScaleVector[ 2 ].Annotation_Position[j]=='B')
 	   {
 	     sl = Annotation_StartLine ;

	     /*  0 degrees rotation */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
 	       sam = Annotation_StartSample + ScaleVector[ 2 ].Tick_Width[j] +
 	             HorizontalOffset ;
	     /* +/- 90 degrees rotation */
 	     else 
		{
		if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'L' )
		sam = Annotation_StartSample + ScaleVector[ 2 ].Tick_Width[j] 
			+ HorizontalOffset + FontThickness ;
		else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R')
                     {
              /*  Add the following if statement, so annotation will not
                  attach to the tick mark when rotate 90 along the top tick,
                  3-9-93, FFM  */ 
                     if ( XAFONTTYPE == HERSHEY )
			sam = Annotation_StartSample + HorizontalOffset 
                              + (CharHeight-DigitHeight) ;
                     else
			sam = Annotation_StartSample + HorizontalOffset ;
                     }
 		if( ScaleVector[ 2 ].Annotation_Justification[j] == 'R' )
			sl += ScaleVector[ 2 ].Tick_Length[j]*(0.25)-StringLen;
 		}
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R' &&  
		  ScaleVector[ 2 ].Annotation_Justification[j] == 'L' )  
 	       if ( ss[0] == '-' )
 	         sl -= XACHARSPACING*ScaleVector[2].Annotation_Size[j] + width ;
 	
 	       zastorestring( sl ,
 	                    sam ,
 	                    ScaleVector[ 2 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
           /* Test for annotation placed on top of tick mark */
 	   else if (ScaleVector[ 2 ].Annotation_Position[j]=='C')
 	   {
	     /* 0 degrees rotation */
 	     if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'V')
 	     {
             /*  Use CharHeight instead of DigitHeight,so annotation won't 
                 attach to the top tick mark,  3-9-93,  FFM   */

               if ( XAFONTTYPE == HERSHEY )
                  sl = Annotation_StartLine - VerticalOffset - CharHeight -  
		       FontThickness ;
               else
                  sl = Annotation_StartLine - VerticalOffset - DigitHeight -  
		       FontThickness ;

 	       sam = Annotation_StartSample - StringLen/2 ; 
 	     }
	     /* +/- 90 degrees rotation */
 	     else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'L' ) 
 	     {
               sl = Annotation_StartLine - VerticalOffset - StringLen - 1  ;

              /*  Add the following if statement, so annotation will be 
                  centered  when rotate -90 along the top tick, 3-9-93, FFM  */ 
           
               if ( XAFONTTYPE == HERSHEY )
  	            sam = Annotation_StartSample - CharHeight/2;
               else
  	            sam = Annotation_StartSample - DigitHeight/2 + 1 ; 
 	     }
	     else if ( ScaleVector[ 2 ].Annotation_Orientation[j] == 'R')
	     {
 	       sl = Annotation_StartLine - VerticalOffset 
		    - StringLen - 1  ;
	       sam = Annotation_StartSample - DigitHeight*(0.75) ;
	     }
 	
             zastorestring(sl,
 	                   sam,
 	                   ScaleVector[ 2 ].Annotation_Size[j],
 	                   DN_Value,
 	                   ss,
 	                   FontThickness ) ;
           }
 	   return (0) ;
 	}
 	
  		
 	/********************************************/
 	/* Draw Annotation, Horizontal Scale, Bottom */
 	/********************************************/
 	int xxadrawAnnotation_HB (Annotation_StartLine, Annotation_StartSample, j, 
 		DN_Value, FontThickness, VerticalOffset, HorizontalOffset, ss, 
		CharHeight, StringLen, width, DigitHeight) 
 	int Annotation_StartLine, Annotation_StartSample, j, DN_Value[], 
		FontThickness, CharHeight, StringLen, VerticalOffset, 
		HorizontalOffset, width, DigitHeight ;
 	char ss[30] ;
 	{
 	   int sl, sam ;
 	
 	   sl = Annotation_StartLine + ScaleVector[1].Tick_Length[j];
 	
           /* Test for annotation to the left of tick mark */
 	   if ( ScaleVector[ 1 ].Annotation_Position[j] == 'T') 
 	   {
	     /* 0 degrees rotation */
 	     if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V')
 	      	{
		sl -= DigitHeight;
 		sam = Annotation_StartSample-HorizontalOffset-StringLen-2;
		}
	     /* +/- 90 degrees rotation */
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'R' || 
 	               ScaleVector[ 1 ].Annotation_Orientation[j] == 'L')  
 	     {
                /*  Add the following if statement, so annotation will be 
                    closer to the left tick mark  with -90 rotation 3/9/93
                     ...FFM  */ 
                   if ( XAFONTTYPE != HERSHEY  &&
 	               ScaleVector[ 1 ].Annotation_Orientation[j] == 'L')
     	               sam = Annotation_StartSample - HorizontalOffset - DigitHeight - 
 	               FontThickness - 2 ;
                   else
     	               sam = Annotation_StartSample - HorizontalOffset - CharHeight - 
 	               FontThickness - 2 ;

	           if( ScaleVector[ 1 ].Annotation_Justification[j] == 'R' )
		       sl -= ScaleVector[1].Tick_Length[j]*(0.25);
 	           else
			sl -= StringLen ;

/* 	       if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'L') 
 	         if (ss[0] == '-')
 	          sl += width+XACHARSPACING*ScaleVector[ 1 ].Annotation_Size[j];
 */	     }
  		zastorestring( sl ,
	                    sam,
 	                    ScaleVector[ 1 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
  	
 	   }
           /* Test for annotation placed to the right of tick mark */
 	   else if (ScaleVector[ 1 ].Annotation_Position[j]=='B')
 	   {
	     /* 0 degrees rotation */
 	     if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V')
 		{
		sl -= DigitHeight;
 	       	sam = Annotation_StartSample + ScaleVector[ 1 ].Tick_Width[j] +
 	             HorizontalOffset ;
 		}
	     /* +/- 90 degrees rotation */
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'L') 
 	     {
 	       sam = Annotation_StartSample + ScaleVector[ 1 ].Tick_Width[j] +
 	             HorizontalOffset + FontThickness ;

	       if( ScaleVector[ 1 ].Annotation_Justification[j] == 'R' )
			sl -= ScaleVector[1].Tick_Length[j]*(0.25);
 	       else
			sl -= StringLen ;

	     }
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'R')
		     {
              /*  Add the following if statement, so annotation will not
                  attach to the tick mark when rotate 90 along the top tick,
                  3-9-93, FFM  */ 
                     if ( XAFONTTYPE == HERSHEY)
		        sam = Annotation_StartSample + HorizontalOffset
                              +(CharHeight-DigitHeight) ;
                     else
		        sam = Annotation_StartSample + HorizontalOffset ;

		     if( ScaleVector[1].Annotation_Justification[j] == 'R' )
			sl -= ScaleVector[1].Tick_Length[j]*(0.25);
 	       	     else
		        sl -= StringLen ;
 	     	     }
 	
 		zastorestring( sl ,
 	                    sam ,
 	                    ScaleVector[ 1 ].Annotation_Size[j],
 	                    DN_Value,
 	                    ss,
 	                    FontThickness ) ;
 	
 	   }
	   /* Test for annotation placed below or parallel to tick mark */
 	   else if (ScaleVector[ 1 ].Annotation_Position[j]=='C')
 	   {
	     /* 0 degrees rotation */
	     sl += VerticalOffset + 2 ;
 	     if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'V')
 	       sam = Annotation_StartSample - StringLen/2 ;
	     /* +/- 90 degrees rotation */ 
 	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'L') 


               if ( XAFONTTYPE == HERSHEY )
  	            sam = Annotation_StartSample - CharHeight/2;
               else
  	            sam = Annotation_StartSample - DigitHeight/2 + 1 ; 

	     else if ( ScaleVector[ 1 ].Annotation_Orientation[j] == 'R')
		    sam = Annotation_StartSample - DigitHeight*(0.75) ;
 	
             /* Check for right justification of annotation 	
 	     if (ScaleVector[ 1 ].Annotation_Orientation[j] == 'R')  
 	     {
 	       sam = sam - 1 ;
 	       if (ss[0] == '-')
 	         sl += XACHARSPACING * ScaleVector[ 1 ].Annotation_Size[j] + 
			width;
 	     }*/
 	     zastorestring(sl,
 	                   sam,
 	                   ScaleVector[ 1 ].Annotation_Size[j],
 	                   DN_Value,
 	                   ss,
 	                   FontThickness ) ;
 	   }
 	   return (0) ;
 	}
 	
  		
 	/***************************/
 	/* Transfer String to Mask */
 	/***************************/
 	/*
 	
 	This is an internal routine for the module.
 	
 	Parameters are :
 	
 		String Vector Index
 		Line number
 	
 	Algorithm :
 	
 	Determine vertical font line # wrt size
 	For each character from string
 	For each bit of font line do size times
 	If we have not filled up the mask Then
 	   Copy bit to mask
 	*/
 	
 	void xxatransferstring( SVIndex, Line )
 	int Line ;
 	struct StringType *SVIndex;
 	{
 	   int FOffset ;	/* Ascii character offset for fonts */
 	   int FontLine ;	/* Font line # to transfer */
 	   int FontSample ;	/* Font sample # to transfer */
 	   int MIndex ;		/* Mask index */
 	   int i, k ;
 	   int SOffset ;        /* Calculate sample number in the font */
 	
 	   /* Determine vertical font line # */
 	
 	   FontLine =  2 * ( Line - SVIndex->StartLine ) / SVIndex->Size ;
 	   MIndex = SVIndex->StartSample ;
 	
 	   for ( k = 0 ; k < strlen( SVIndex->CharString ) ; k++ )
 	   	{
 	      	/* Grab a character */
 	
 	      	FOffset = SVIndex->CharString[ k ] - ASCIISPACE ;
 	      	SOffset = SVIndex->StartSample ; 
 	
 		/* For each bit of font line do size times copy bit to mask */
 	
 	      	for ( i = 0 ; i < SVIndex->CharWidth ; i++ )
 	      		{
 	           	FontSample =  2 * ( SOffset - SVIndex->StartSample ) /
 	                	SVIndex->Size ;
 	           	if(MIndex<UserMaxSamples && 
 			   Font[FOffset][FontLine][FontSample]!=0)
			   for( d=0; d<Dimension; d++ )
 				Mask[d][MIndex] = SVIndex->Dn[d] ;
 	            	++MIndex ;
 	            	++SOffset ;
 	       		}
 	
 	      	/* Spacing between letters */
 	
 	      	MIndex += SVIndex->CharSpac;
 	      	}
 	}
 	
 	
  		
 	/**************************************************/
 	/* Transfer String to Mask Using 90 Degree Angles */
 	/**************************************************/
 	/*
 	
 	This is an internal routine for the module.
 	
 	Parameters are :
 	
 		String Vector Index
 		Line number
 	
 	Algorithm :
 	
 	Determine vertical font line # wrt size
 	For each character from string
 	For each bit of font line do size times
 	If we have not filled up the mask Then
 	   Copy bit to mask
 	*/
 	
 	void xxatransferstring90( SVIndex, Line )
 	int Line ;
 	struct StringType *SVIndex;
 	{
 	   int FOffset ;    /* Ascii character offset for fonts */
 	   int FontLine ;   /* Font line # to transfer */
 	   int FontSample ; /* Font sample # to transfer */
 	   int MIndex ;	    /* Mask index */
 	   int CharIndex ;  /* Character index into StringVector.CharString */
 	   int i, j, k;
 	   int Adjustment ;    
 	   int Adjusted_line_number ;
 	   int CharWidth ;      /* Character width + spacing */
 	   int LOffset ;        /* offset to the starting line */
 	
 	
 	   CharWidth =  SVIndex->CharWidth + SVIndex->CharSpac ;
 	   LOffset   =  Line - SVIndex->StartLine ;
 	
 	/* Calc character index in CharString */
 	   CharIndex =  LOffset / CharWidth ;
 	
 	   Adjustment = LOffset/CharWidth ;
 	   Adjusted_line_number = Line - Adjustment * CharWidth ;
 	
 	/* Determine vertical font line # */
 	
 	   if ( SVIndex->Angle == 90 )
 	   	{
 	     	FontLine = 2 * (Adjusted_line_number - SVIndex->StartLine) / 
 			SVIndex->Size ;
 	     	if ( FontLine > (XACHARWIDTH-1))
 	        	return ;	/* Don't need to write the spacings */
 	   	}
 	   else
 	   	{
 	     	FontLine = 4 - 2 * (Adjusted_line_number - SVIndex->StartLine)/
 	                SVIndex->Size ;
 	     	if ( FontLine < 0 )
 	      		return ;	/* Don't need to write the spacings */
 	   	}
 	
 	   MIndex = SVIndex->StartSample ;
 	
 	/* Grab the current character */
 	   FOffset = SVIndex->CharString[ CharIndex ] - ASCIISPACE ;
 	   if (FOffset < 0) 
 		return ;
 	
 	/* For each bit of font line do size times 
 	   copy bit to mask */
 	
 	   if ( SVIndex->Angle == 90 )
 	   	{
 	      	j = 0 ;
 	      	for ( i = SVIndex->CharHeight ; i > 0 ; i-- )
 	      		{
 	         	FontSample =  10 - ( 2 * j / SVIndex->Size ) ;
 	                       
 	         	if ( FontSample > 0 )
 	         		{
 	           		if ( MIndex < UserMaxSamples &&
 	                	Font[FOffset][FontSample-1][FontLine] != 0 )
				   for(d=0; d<Dimension; d++)
 					Mask[d][MIndex] = SVIndex->Dn[d] ;
 	           		++MIndex ;
 	           		++j ;
 	         		}
 	      		}
 	   	}
 	   else
 	      	for ( i = 0 ; i < SVIndex->CharHeight ; i++ )
 	      		{
 	           	FontSample =  2 * i / SVIndex->Size ;
 	           	if ( MIndex < UserMaxSamples &&
 	                Font[ FOffset ][ FontSample][FontLine] != 0 )
                           for( d=0; d<Dimension; d++ )
 				Mask[d][MIndex] = SVIndex->Dn[d] ;
 	            	++MIndex ;
 	      		}
 	}
 
 	
  		
 	/*****************/
 	/* Fixed stretch */
 	/*****************/
 	/*
		This internal routine calculates the lookup table (OutLut)
		for a fixed stretch.  The stretch can be a complement stretch
		as found in STRECH.COM.  In the comstretch case, the
		Low Dn (Lo) is the first value designated by the user in 
		XASTOREIMAGE or XAZOOMIMAGE, and is actually the largest 
		numeric value but corresponds to the DN to be driven to 0.
		Likewise, the High Dn (Hi) given in the complement mode is
		driven to 255, even though is numerically smaller than Lo.

 		Parameters :
 	
 			Low Dn
 			High Dn
 			Output look up table
			Maximum Dn
 	*/
 	
 	void xxafixedstretch( Lo, Hi, OutLut, MAXDN )
 	int Lo, Hi, MAXDN;
 	int OutLut[] ;		/* Lookup table */
 	{
 	   int i ;
 	   float scale ;	/* Scale factor */
 	   float temp ;		/* Used to convert from integer to float */
 	   float temp2 ;	/* Ditto */

 	   temp = MAXDN;
 	   temp2 = abs( Hi - Lo ) ;

 	   if ( temp2 == 0.0 )
 	      temp2 = 1.0 ;
 	   scale = temp / temp2 ;
 	
	   if( Lo > Hi )	/* Determine if COMPLEMENT stretch is desired */
	     {
	     if( Hi != 0 )
 	      	fillbyte(OutLut, MAXDN, Hi*4);   
	     }
           else
	     {
             if( Lo != 0 )	
  	     	fillbyte(OutLut, 0, Lo*4);   
	     }
 	
 	   if ( Lo == Hi )		/* Special case if all the same DN */
 	      OutLut[ Lo ] = Lo ;
 	   else
		{
		if( Lo > Hi )
			{
			for ( i = Hi ; i <= Lo ; i++ )
 		      		OutLut[ i ] = ( Lo - i ) * scale + 0.5 ;
		        for( i = Lo+1; i <= MAXDN; i++) 
	      			OutLut[ i ] = 0;
			}
		else
	 		{
			for ( i = Lo ; i <= Hi ; i++ )
 		      		OutLut[ i ] = ( i - Lo ) * scale + 0.5 ;
		        for( i = Hi+1; i <= MAXDN; i++) 
	      			OutLut[ i ] = MAXDN;
			}
		}
 	}
 	
   		
	
 	/********************/
 	/* Adaptive stretch */
 	/********************/
 	/*
 		Parameters :
 	
 			Input histogram [0..MaxDn]
 			Fraction of image to be sampled [0.0..1.0]
 			Percentage of low and high saturation [0.0..1.0]
 			Output look up table [0..MaxDn]
			Maximum Dn
 	*/
 	void xxaadaptivestretch( InpHist, FracSamp, SaturPerc, OutLut, MaxDn )
 	float FracSamp, SaturPerc ;
 	int InpHist[], OutLut[], MaxDn ;
 	{
 	   int i ;
 	   unsigned int count ;
 	   int tally ;
 	   int Hi, Lo ;
 	
 	   count = 0 ;
 	   for ( i = 0 ; i <= MaxDn ; i++ ) /* Count up all the samples */
 	      count += InpHist[ i ] ;
 	
 	   tally = count * SaturPerc ;    /* Derive number of samples for */
 					  /* saturation to 0 and 255      */
 	   Lo = 0 ;				/* Get Low index */
 	   for ( i = 0 ; i <= MaxDn ; i++ )
 	      if ( ( tally -= InpHist[ i ] ) <= 0 )
 	      {
 	         Lo = i ;
 	         break ;
 	      }
 	
 	   tally = count * SaturPerc ;    /* Derive number of samples for */
 					  /* saturation to 0 and 255      */
 	   Hi = MaxDn ;				/* Get High index */
 	   for ( i = MaxDn ; i >= 0 ; i-- )
 	      if ( ( tally -= InpHist[ i ] ) <= 0 )
 	      {
 	         Hi = i ;
 	         break ;
 	      }

 	    	   XALODNSTRETCH = Lo ;
 	    	   XAHIDNSTRETCH = Hi ;
	
 	   xxafixedstretch( Lo, Hi, OutLut, MaxDn ) ;	/* Do the stretch */
 	}
   		
	/*********************************/
 	/* Apply a stretch to all images */
 	/*********************************/
 	
 	void xxaapplystretch(lowdn,highdn)
 	int lowdn[MAXIMAGENUMBER], highdn[MAXIMAGENUMBER] ;
 	{
	   float scale, scale2;	/* Histogram scaling factors for stretches */
 	   int IOStatus ;	/* I/O status flag 			*/
 	   int line ;		/* Line counter 			*/
 	   int lineincr, sampincr ;
 	   int samp ;		/* Sample counter 			*/
 	   int i, j ;
 	   int Hi, Lo ;
 	   int temp1, temp2, dummy1, dummy2, d[10];
           int zero ;
	   unsigned int 	count;
 	   unsigned int 	*TempHist;
 	   struct HistType 	*hptr;
 	   struct ImageType 	*ptr;
 	   struct ZImageType 	*zptr;
 	   struct PDSImageType 	*PDSptr;
 	   struct PDSZImageType *PDSzptr;
 	
 	   if ( XALODNSTRETCH != -1 ) return;

           zero = 0; 

	   i = 0; 	
	   j = 0;

 	   ptr = firstimage;   
	   while(ptr!=NULL)
 		{
 	      	if ( ptr->Stretch == Fixed )
 	      		{
 	         	temp1 = ptr->SampleFraction ;
 	         	temp2 = ptr->SaturationPercentage ;
 	
 	         	Lo = temp1 ;
 	         	Hi = temp2 ;

 	         	XALODNSTRETCH = Lo ;
 	         	XAHIDNSTRETCH = Hi ;

 	         	xxafixedstretch( Lo, Hi, ptr->LUT, ptr->NumDNs-1 ) ;
 	      		}
 	      	else
 	      		if ( ptr->Stretch == Adaptive )
 	      			{
 	         		/* Sample fraction of image for lookup table */
 				lineincr = 1.0 / ptr->SampleFraction ;
				sampincr = 1;

				TempHist = (unsigned int *)calloc
					   (ptr->NumDNs,sizeof(unsigned int));
		     	        if( TempHist == NULL )
					{
					zvmessage("Memory allocation error"," ");
					zvmessage("- xxaapplystretch aborted."," ");
					continue;
					}

				zacalculatehist( ptr->InputUnitNumber, 
						 ptr->StartLine, 
						 ptr->StartSample,ptr->EndLine, 
						 ptr->EndSample, lineincr,
						 1, TempHist, &dummy1, &dummy2,
						 'n', &zero, &zero, 
					         ptr->EXC, ptr->NUM);

 				xxaadaptivestretch( TempHist,
					ptr->SampleFraction,
 	                          	ptr->SaturationPercentage,
					ptr->LUT,
					ptr->NumDNs-1 ) ;

 	         		if ( DETERMINELIMIT == 1 )
 	         			{
 	              			lowdn[j] = XALODNSTRETCH ; 
 	              			highdn[j] = XAHIDNSTRETCH ;
					j++;
 	       	 			}
				free( TempHist );
 	      			}
		i++;
 		ptr = ptr->next;
 		}

	   i = 0;
 	   zptr = firstzoom;		/* Apply stretch to zoomed images    */
 	   while(zptr!=NULL)
 		{
 	      	if ( zptr->Stretch == Fixed )
 	      		{
 	         	temp1 = zptr->SampleFraction ;
 	         	temp2 = zptr->SaturationPercentage ;
 	
 	         	Lo = temp1 ;
 	         	Hi = temp2 ;

 	         	XALODNSTRETCH = Lo ;
 	         	XAHIDNSTRETCH = Hi ;

 	         	xxafixedstretch( Lo, Hi, zptr->LUT, zptr->NumDNs-1 ) ;
 	      		}
 	      	else
 	      		if ( zptr->Stretch == Adaptive )
 	      			{
 	         		/* Sample fraction of image for lookup table */
 				lineincr = 1.0 / zptr->SampleFraction ;
				sampincr = 1;

				TempHist = (unsigned int *)calloc
					   (zptr->NumDNs,sizeof(unsigned int));
		     	        if( TempHist == NULL )
					{
					zvmessage("Memory allocation error"," ");
					zvmessage("- xxaapplystretch aborted."," ");
					continue;
					}


				zacalculatehist( zptr->InputUnitNumber, 
						 zptr->StartLine, 
						 zptr->StartSample,
						 zptr->EndLine, 
						 zptr->EndSample, lineincr,
						 sampincr, TempHist, &dummy1,
						 &dummy2, 'n', &zero, &zero, 
					         zptr->EXC, zptr->NUM );
 	
 				xxaadaptivestretch( TempHist,
					zptr->SampleFraction,
 	                          	zptr->SaturationPercentage,
					zptr->LUT,
					zptr->NumDNs-1 ) ;

 	         		if ( DETERMINELIMIT == 1 )
 	         			{
 	              			lowdn[j] = XALODNSTRETCH ; 
 	              			highdn[j] = XAHIDNSTRETCH ;
					j++;
 	       	 			}

				free( TempHist );
 	      			}
 		i++;
 		zptr = zptr->next;
 		}

	   i = 0; 	
 	   PDSptr = firstPDSimage;		/* Apply stretch to PDS image data	*/
 	   while(PDSptr!=NULL)
 		{
 	      	if ( PDSptr->Stretch == Fixed )
 	      		{
 	         	temp1 = PDSptr->SampleFraction ;
 	         	temp2 = PDSptr->SaturationPercentage ;
 	
 	         	Lo = temp1 ;
 	         	Hi = temp2 ;

 	         	XALODNSTRETCH = Lo ;
 	         	XAHIDNSTRETCH = Hi ;

 	         	xxafixedstretch( Lo, Hi, PDSptr->LUT, PDSptr->NumDNs-1 ) ;
 	      		}
 	      	else
 	      		if ( PDSptr->Stretch == Adaptive )
 	      			{
 	         		/* Sample fraction of image for lookup table */
 				lineincr = 1.0 / PDSptr->SampleFraction ;
				sampincr = 1;

				TempHist = (unsigned int *)calloc
					   (PDSptr->NumDNs,sizeof(unsigned int));
		     	        if( TempHist == NULL )
					{
					zvmessage("Memory allocation error"," ");
					zvmessage("- xxaapplystretch aborted."," ");
					continue;
					}

				zacalculatePDShist( PDSptr->InputFileName, 
						 PDSptr->StartLine, 
						 PDSptr->StartSample,PDSptr->EndLine, 
						 PDSptr->EndSample, lineincr,
						 1, TempHist, &dummy1, &dummy2,
						 'n', &zero, &zero, 
					         PDSptr->EXC, PDSptr->NUM);

 				xxaadaptivestretch( TempHist,
					PDSptr->SampleFraction,
 	                          	PDSptr->SaturationPercentage,
					PDSptr->LUT,
					PDSptr->NumDNs-1 ) ;

 	         		if ( DETERMINELIMIT == 1 )
 	         			{
 	              			lowdn[j] = XALODNSTRETCH ; 
 	              			highdn[j] = XAHIDNSTRETCH ;
					j++;
 	       	 			}
				free( TempHist );
 	      			}
		i++;
 		PDSptr = PDSptr->next;
 		}

	   i = 0;
 	   PDSzptr = firstPDSzoom;		/* Apply stretch to zoomed PDS images    */
 	   while(PDSzptr!=NULL)
 		{
 	      	if ( PDSzptr->Stretch == Fixed )
 	      		{
 	         	temp1 = PDSzptr->SampleFraction ;
 	         	temp2 = PDSzptr->SaturationPercentage ;
 	
 	         	Lo = temp1 ;
 	         	Hi = temp2 ;

 	         	XALODNSTRETCH = Lo ;
 	         	XAHIDNSTRETCH = Hi ;

 	         	xxafixedstretch( Lo, Hi, PDSzptr->LUT, PDSzptr->NumDNs-1 ) ;
 	      		}
 	      	else
 	      		if ( PDSzptr->Stretch == Adaptive )
 	      			{
 	         		/* Sample fraction of image for lookup table */
 				lineincr = 1.0 / PDSzptr->SampleFraction ;
				sampincr = 1;

				TempHist = (unsigned int *)calloc
					   (PDSzptr->NumDNs,sizeof(unsigned int));
		     	        if( TempHist == NULL )
					{
					zvmessage("Memory allocation error"," ");
					zvmessage("- xxaapplystretch aborted."," ");
					continue;
					}

				zacalculatePDShist( PDSzptr->InputFileName, 
						 PDSzptr->StartLine, 
						 PDSzptr->StartSample,
						 PDSzptr->EndLine, 
						 PDSzptr->EndSample, lineincr,
						 sampincr, TempHist, &dummy1,
						 &dummy2, 'n', &zero, &zero, 
					         PDSzptr->EXC, PDSzptr->NUM );
 	
 				xxaadaptivestretch( TempHist,
					PDSzptr->SampleFraction,
 	                          	PDSzptr->SaturationPercentage,
					PDSzptr->LUT,
					PDSzptr->NumDNs-1 ) ;

 	         		if ( DETERMINELIMIT == 1 )
 	         			{
 	              			lowdn[j] = XALODNSTRETCH ; 
 	              			highdn[j] = XAHIDNSTRETCH ;
					j++;
 	       	 			}

				free( TempHist );
 	      			}
 		i++;
 		PDSzptr = PDSzptr->next;
 		}


        xxaopeninputfiles();    /* Reopen input files after being closed by */
			 	/* XACALCULATEHIST during adaptive stretch  */
 	}
  		
	/************************/
 	/* Open all input files */
 	/************************/
 	
 	int xxaopeninputfiles()
 	{
 	   int i, j, bytes ;
 	   int input, IOStatus ;		/* I/O unit #s, Status flag */
 	   int flags ;
	   char string[MAXFILENAME];

	   FILE *fp; 	
	   struct PDSImageType *PDSiptr;
	   struct PDSZImageType *PDSzptr;
	   void *temp;

	   /* Loop through all VICAR files for opening 	*/

	   for(j=0;j<unitcount;j++)
 		{
		IOStatus = zvget( unitnumber[j], "FLAGS", &flags, NULL );
 	        if ( ( flags & OPEN ) == 0 )
			IOStatus = zvopen( unitnumber[j], "OPEN_ACT", "SA", NULL );
 	        IOStatus = zvget( unitnumber[j], "PIX_SIZE", &bytes, NULL ) ;
		IOStatus = zvclose( unitnumber[j], NULL );

		if( bytes == 1 )
 	            	IOStatus = zvopen( unitnumber[j], "OPEN_ACT", "SA", 
			                   "IO_ACT", "SA","U_FORMAT", "HALF",
                                           "O_FORMAT", "BYTE", NULL) ;
 		else
 	            	IOStatus = zvopen( unitnumber[j], "OPEN_ACT", "SA", 
			"IO_ACT", "SA","U_FORMAT","HALF","O_FORMAT", "HALF", NULL) ;
		}


	   /* Loop through all PDS files for opening */

	   for( i=0; i<PDSimagecount; i++ )
		if ( pds_image[i].file_open_flag == FALSE )
			{
			pds_image[i].fp = fopen( pds_image[i].file, "r" );
			if ( pds_image[i].fp != NULL )
				{
				pds_image[i].file_open_flag = TRUE;    

				/* Set file pointer values in PDS image object structures */
				/* of XA/ZASTOREPDSIMAGE and XA/ZAZOOMPDSIMAGE            */

				PDSiptr = firstPDSimage;
				while( PDSiptr != NULL )
					{
					temp = PDSiptr->next;
					if ( strcmp(pds_image[i].file,PDSiptr->InputFileName) == 0 )
						PDSiptr->fp = pds_image[i].fp;
		 		    	PDSiptr = temp;
 					}
				PDSzptr = firstPDSzoom;
				while( PDSzptr != NULL )
					{
					temp = PDSzptr->next;
					if ( strcmp(pds_image[i].file,PDSzptr->InputFileName) == 0 )
						PDSzptr->fp = pds_image[i].fp;
		 		    	PDSzptr = temp;
 					}
				}
			else
				{
				sprintf(string,"Error in opening file %s",pds_image[i].file);
				zvmessage(string," ");
				zvmessage("	- xxaopeninputfiles aborted."," ");				
				return -1;
				}
			}
	    return(0);
 	}
   		
	/*************************/
 	/* Close all input files */
 	/*************************/
 	void xxacloseinputfiles()
 	{
 	   int IOStatus, count ;
 	
	   for( count=0; count<unitcount; count++ )
		IOStatus = zvclose(unitnumber[count], NULL);

	   for( count=0; count<PDSimagecount; count++ )
		if( pds_image[count].file_open_flag == TRUE )
			{
			fclose(pds_image[count].fp);
			pds_image[count].file_open_flag = FALSE;
			}
 	}
   		
	/**************************/
	/* Draw vector		  */
	/**************************/
	void xxadrawvector(line,ptr)
	int line;
	struct VectorType *ptr;
	{
	int 	width, d, j, x1, x2;

	line -= ptr->StartLine;

	for(d=0; d<Dimension; d++)
 	   switch( ptr->Direction )
		{
		case Hori:

			j = ptr->StartSample - 1;
		     	fillbyte(&Mask[d][j],ptr->Dn[d],ptr->Length);
		     	break;
	
		case Vert:
		     
			j = ptr->StartSample - 1;
			fillbyte(&Mask[d][j],ptr->Dn[d],ptr->Width);
			break;
		
		case Klinein:	
		default:
		
			j = ptr->Edge[line][0];
			if ( ptr->Edge[line][0] <= ptr->Edge[line][1] )
			   {
			   width = ptr->Edge[line][1] - ptr->Edge[line][0] + 1;
			   fillbyte(&Mask[d][j],ptr->Dn[d],width);
			   }
			break;	
		} 
	}
   		
	/**************************/
 	/* Copy zoomed image data */
 	/**************************/
 	void xxacopyzoom( line )  /* Copy all relevant parts of all images to */
 	int line ;           /* output line LINE */
 	{
 	int 	j,
 	    	OutSamp; 		/* Output image sample index 	*/
 	struct ZImageType *ptr;

	if( line >= minzoomline)
	   {
 	   ptr = firstzoom;	
 	   while(ptr != NULL)		/* Loop through all images	*/
 		{
 		if( line >= ptr->OutStartLine )
		  {
		  if( line < ptr->OutStartLine + ptr->MulFactor * 
		      (ptr->EndLine - ptr->StartLine + 1) )
			{
	   		OutSamp = ptr->OutStartSample - 1 ;

			for( d=1; d<=Dimension; d++ )
			  if( ptr->OutBand == d || !ptr->OutBand )
				xxadozoom(ptr,line,OutSamp);
		    	  else
				if( ptr->ColorBands == 1 && ptr->OutBand != d )
					{
			 		if( ptr->Ztype=='I' || ptr->Ztype=='i' )
						for(j=0; j<(ptr->MulFactor - 1)
						    *(ptr->InputSamples-1)
						    +ptr->InputSamples; 
						    j++)	
						   Mask[d-1][OutSamp+j] = 0;
					else	
						for(j=0;j<ptr->MulFactor*
						    ptr->InputSamples;j++)
						  Mask[d-1][OutSamp+j] = 0;
					}
			}
		  else
		        {
			if( ptr->previous == NULL )
				firstzoom = ptr->next;
			else
				( ptr->previous )->next = ptr->next;
			}
		  }
 		ptr = ptr->next;
 		}
           }
 	}
   		
	/******************************/
 	/* Perform zoom on image data */
 	/******************************/
 	int xxadozoom(ptr,line,OutSamp)
 	struct ZImageType *ptr;
 	int	line,
 		OutSamp;
 	{
 	   int 	j, k;			/* Loop control variables    	*/
 	   int input, IOStatus ;	/* I/O unit #s, Status flag 	*/
 	   int band, numlines;
 	   int x, index, cycle, skip;
 	   static int calcfinished;
 	   short int *InputLine;	/* Input image line buffer   	*/
 	   int m_factor;		/* Magnification factor      */
 	   int twotothe;
 	
 	   numlines = ptr->EndLine - ptr->StartLine + 1;
 	   m_factor = ptr->MagFactor;
 	
 	   /* Allocate space for zoom calculations */
 	   InputLine = (short int *)calloc(ptr->InputSamples,sizeof(short int));
	   if( InputLine == NULL )
		{
		zvmessage("Memory allocation error"," ");
		zvmessage("- xxadozoom aborted."," ");
		return( -1 );
		}
 	 
	   band = d;
	   if( ptr->InputBands == 1 )
		band = 1;

	   /* If first line of zoom, read input line and do interpolation. */
 	   if( line==ptr->OutStartLine && ptr->Ztype==0 && ptr->MagFactor>0) 
 		{    
		IOStatus = zvread(ptr->InputUnitNumber,InputLine,
				"LINE",ptr->StartLine,
				"SAMP",ptr->StartSample,
				"NSAMPS",ptr->InputSamples,
				"BAND",band, NULL);
		if ( IOStatus < 0 )
			return -1;

		if( ptr->NumDNs == 256 )
			for(k=0,j=0; j<ptr->InputSamples; j++,k+=m_factor)
 			   ptr->Input[0][k] = ptr->LUT[255&InputLine[j]];
		else
			{
			if( ptr->Stretch == NoStretch )
				for(k=0,j=0; j<ptr->InputSamples; j++,k+=m_factor)
				{
				if(InputLine[j]<0)
					InputLine[j] = 0;
 				ptr->Input[0][k] = SCALE32K*
					ptr->LUT[32767 & InputLine[j]];
				}
			else
				for(k=0,j=0; j<ptr->InputSamples; j++,k+=m_factor)
 					ptr->Input[0][k] = SCALE64K*
						ptr->LUT[65535 & 
						(InputLine[j]+32768)];
			}		
 		index = (ptr->InputSamples-1)*m_factor; /* Interpolate between samples */
 		for(k=0;k<(ptr->InputSamples-1);k++)
 			{
 			cycle = 0;
 			skip = m_factor;
 			while(skip!=1)
 				{  
 				twotothe = (int) pow(2,cycle);
 				for(j=0;j<twotothe;j++)
 				      	ptr->Input[0][index-(j*skip)-(skip/2)]
 				      	= (unsigned char)
					(((int)ptr->Input[0][index-(j*skip)] +
 				      	(int)ptr->Input[0][index-((j+1)*skip)])
					/2);
 				cycle++;
 				skip/=2;
 				}
 			index -= m_factor;
 			}
 		}
 	
 	   if(m_factor < 0)
 		{ /* Sample the image by every | MagFactor | sample    */
 		k = ptr->StartLine + (line-ptr->OutStartLine)*( -1 * m_factor );
		IOStatus = zvread(ptr->InputUnitNumber,InputLine,
				"LINE",k,
				"SAMP",ptr->StartSample,
				"NSAMPS",ptr->InputSamples,
				"BAND",band, NULL);
		if ( IOStatus < 0 )
			return -1;

		if(ptr->NumDNs == 256)
			for( k=0,j=0; k<ptr->InputSamples; k+=(-1*m_factor), j++)
				Mask[d-1][OutSamp+j] = ptr->LUT[(ptr->NumDNs-1)&
					  InputLine[k]];
		else
			{
			if( ptr->Stretch == NoStretch )
				for( k=0,j=0; k<ptr->InputSamples; k+=(-1*m_factor), j++)
					{
					if( InputLine[k] < 0 )
						InputLine[k] = 0;
 					Mask[d-1][OutSamp+j] = SCALE32K *
					ptr->LUT[32767&InputLine[k]];
					}
			else
				for( k=0,j=0; k<ptr->InputSamples; k+=(-1*m_factor), j++)
 					Mask[d-1][OutSamp+j] = SCALE64K *
					ptr->LUT[65535&
					(InputLine[k]+32768)];
			}			
 		}
 	   else		/* Is this zooming instead of shrinking */
 		if(ptr->Ztype == 0 )	/* Zoom by means of interpolation */
 			if((line - ptr->OutStartLine) % m_factor == 0)
 				{
 				if(line != ptr->OutStartLine)
 					for(k=0;k<(m_factor*ptr->InputSamples);k++)
 						ptr->Input[0][k] = 
 							ptr->Input[m_factor][k];
 				k = ptr->StartLine + 1 +
 					(line - ptr->OutStartLine)/m_factor;

				IOStatus = zvread(ptr->InputUnitNumber,InputLine,
						"LINE",k,
						"SAMP",ptr->StartSample,
						"NSAMPS",ptr->InputSamples,
						"BAND",band, NULL);
				if ( IOStatus < 0 )
					return -1;
 
				if(ptr->NumDNs == 256)
 					for(k=0,j=0;k<ptr->InputSamples;k++,j+=m_factor)
					  ptr->Input[m_factor][j] 
						= ptr->LUT[(ptr->NumDNs-1)&
						InputLine[k]];
				else
					{
					if(ptr->Stretch == NoStretch )
 					  for(k=0,j=0;k<ptr->InputSamples;k++,j+=m_factor)
						{
						if( InputLine[k] < 0 )
						   InputLine[k] = 0;
 						ptr->Input[m_factor][j] 
						= SCALE32K *
						ptr->LUT[32767&
						InputLine[k]];
						}
					else
 					  for(k=0,j=0;k<ptr->InputSamples;k++,j+=m_factor)
 						ptr->Input[m_factor][j] 
						= SCALE64K *
						ptr->LUT[65535 & 
						(InputLine[k]+32768)];
					}
 				index = (ptr->InputSamples-1)*m_factor;
 				for(k=0;k<(ptr->InputSamples-1);k++)
 					{
 					cycle = 0;
 					skip = m_factor;
 					while(skip!=1)
 			        		{ 
 						twotothe = (int) pow(2,cycle); 
 			        		for(j=0;j<twotothe;j++)
 						    {
 						    ptr->Input[m_factor]
 						    [index-(j*skip)-(skip/2)]
 						    = (unsigned char)
						    (((int)ptr->Input[m_factor]
 						    [index-(j*skip)] +
 						    (int)ptr->Input[m_factor]
 						    [index-((j+1)*skip)])/2);
 						    }
 						cycle++;
 						skip/=2;
 						}
 					index -= m_factor;
 					}
				for(k=0;k<(m_factor-1)*(ptr->InputSamples-1)+ptr->InputSamples;k++)
					Mask[d-1][OutSamp+k] = ptr->Input[0][k];
 				calcfinished = FALSE;
 				}
 			else
 		        	{
 				if(calcfinished == FALSE)
 					{ 
 	 				cycle = 0;
 					skip = m_factor;
 					while(skip != 1)
 					 	{
 	 					twotothe = (int) pow(2,cycle);
 					 	for(k=0;
						 k<(m_factor*ptr->InputSamples);k++)
 			  		  	 for(j=0;j<twotothe;j++)
 						    ptr->Input[m_factor-
 						    (j*skip)-(skip/2)][k] = 
						    (unsigned char)
					   	    (((int) ptr->Input[m_factor-
						    (j*skip)][k] +
 						    (int)ptr->Input[m_factor-
 						    ((j+1)*skip)][k])/2);
 					        cycle++;
 					    	skip/=2;
 					    	}
 		  			calcfinished = TRUE;
 					}
 				index = ( line - ptr->OutStartLine ) % m_factor;
				for(k=0;k<(m_factor-1)*(ptr->InputSamples-1)+ptr->InputSamples;k++)
				   Mask[d-1][OutSamp+k] = ptr->Input[index][k];
 				}
 		else			/* Zoom by replication of pixels     */
 			if ( ( line - ptr->OutStartLine ) % m_factor == 0 )
 				{  
 				k = ptr->StartLine+(line - ptr->OutStartLine)
					/ m_factor;

				IOStatus = zvread(ptr->InputUnitNumber,InputLine,
						"LINE",k,
						"SAMP",ptr->StartSample,
						"NSAMPS",ptr->InputSamples,
						"BAND",band, NULL);
				if ( IOStatus < 0 )
					return -1;
	
				if(ptr->NumDNs == 256)
					for(k=0;k<ptr->InputSamples;k++)
						for(j=0;j<m_factor;j++)
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							ptr->LUT[
							(ptr->NumDNs-1)&
							InputLine[k]];
				else
					{
					if( ptr->Stretch == NoStretch )
					  for(k=0;k<ptr->InputSamples;k++)
						for(j=0;j<m_factor;j++)
						   {
						   if( InputLine[k] < 0 )
						      InputLine[k] = 0;
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							SCALE32K *
							ptr->LUT[32767 & 
							InputLine[k]];
						   }
					else
					  for(k=0;k<ptr->InputSamples;k++)
						for(j=0;j<m_factor;j++)
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							SCALE64K *
							ptr->LUT[65535 & 
							(InputLine[k]
							+ 32768)];
					}
 				for(j=0;j<(m_factor*ptr->InputSamples);j++)
 					ptr->Input[0][j] 
					= Mask[d-1][OutSamp+j];
 				}
 			else
				for(j=0;j<(m_factor*ptr->InputSamples);j++)
					Mask[d-1][OutSamp+j] = ptr->Input[0][j];
 	   free(InputLine);
	   return(0);
 	}
   		
	/******************************/
 	/* Copy zoomed PDS image data */
 	/******************************/
 	void xxacopyPDSzoom( line )  /* Copy all relevant parts of all images to */
 	int line ;           	/* output line LINE */
 	{
 	int 	j,
		Samp, 			/* Index of image sample     	*/
 	    	OutSamp, 		/* Output image sample index 	*/
 		NumSamp;		/* Number of samples	     	*/
 	struct PDSZImageType *ptr;

	if( line >= minPDSzoomline)
	   {
 	   ptr = firstPDSzoom;	
 	   while(ptr != NULL)		/* Loop through all images	*/
 		{
 		if( line >= ptr->OutStartLine )
		  {
		  if( line < ptr->OutStartLine + ptr->MulFactor * 
		      (ptr->EndLine - ptr->StartLine + 1) )
			{
	   		OutSamp = ptr->OutStartSample - 1 ;
	   		Samp = ptr->StartSample - 1 ;

			for( d=1; d<=Dimension; d++ )
			  if( ptr->OutBand == d || !ptr->OutBand )
				xxadoPDSzoom(ptr,line,OutSamp,Samp,ptr->InputSamples);
		    	  else
				if( ptr->ColorBands == 1 && ptr->OutBand != d )
					{
			 		if( ptr->Ztype=='I' || ptr->Ztype=='i' )
						for(j=0; j<(ptr->MulFactor - 1)
						    * (ptr->InputSamples-1)
						    + ptr->InputSamples; 
						    j++)	
						   Mask[d-1][OutSamp+j] = 0;
					else	
						for(j=0;j<ptr->MulFactor*
						    ptr->InputSamples;j++)
						  Mask[d-1][OutSamp+j] = 0;
					}
			}
		  else
		        {
			if( ptr->previous == NULL )
				firstPDSzoom = ptr->next;
			else
				( ptr->previous )->next = ptr->next;
			}
		  }
 		ptr = ptr->next;
 		}
           }
 	}
   		
	/**********************************/
 	/* Perform zoom on PDS image data */
 	/**********************************/
 	int xxadoPDSzoom(ptr,line,OutSamp,Samp,NumSamp)
 	struct PDSZImageType *ptr;
 	int	line,
 		OutSamp,
 		Samp,
 		NumSamp;
 	{
 	   int 	j, k;			/* Loop control variables    	*/
 	   int input, IOStatus ;	/* I/O unit #s, Status flag 	*/
 	   int band, numlines;
 	   int x, index, cycle, skip;
 	   static int calcfinished;
 	   short int *InputLine;	/* Input image line buffer   	*/
 	   int m_factor;		/* Magnification factor      */
 	   int twotothe;
 	
 	   numlines = ptr->EndLine - ptr->StartLine + 1;
 	   m_factor = ptr->MagFactor;
 	
 	   /* Allocate space for zoom calculations */
 	   InputLine = (short int *)calloc(ptr->InputSamples,sizeof(short int));
	   if( InputLine == NULL )
		{
		zvmessage("Memory allocation error"," ");
		zvmessage("- xxadoPDSzoom aborted."," ");
		return( -1 );
		}
 	 
	   band = d;
	   if( ptr->InputBands == 1 )
		band = 1;

	   /* If first line of zoom, read input line and do interpolation. */
 	   if( line==ptr->OutStartLine && ptr->Ztype==0 && ptr->MagFactor>0) 
 		{    
		IOStatus = read_PDS_image_line(ptr->InputFileName,InputLine,
				line,ptr->StartSample,ptr->InputSamples,band);
		if ( IOStatus < 0 )
			return -1;
	
		if( ptr->NumDNs == 256 )
			for(k=0,j=0; j<NumSamp; j++,k+=m_factor)
 			   ptr->Input[0][k] = ptr->LUT[255&InputLine[j]];
		else
			{
			if( ptr->Stretch == NoStretch )
				for(k=0,j=0; j<NumSamp; j++,k+=m_factor)
				{
				if(InputLine[j]<0)
					InputLine[j] = 0;
 				ptr->Input[0][k] = SCALE32K*
					ptr->LUT[32767 & InputLine[j]];
				}
			else
				for(k=0,j=0; j<NumSamp; j++,k+=m_factor)
 					ptr->Input[0][k] = SCALE64K*
						ptr->LUT[65535 & 
						(InputLine[j]+32768)];
			}		
 		index = (NumSamp-1)*m_factor; /* Interpolate between samples */
 		for(k=0;k<(NumSamp-1);k++)
 			{
 			cycle = 0;
 			skip = m_factor;
 			while(skip!=1)
 				{  
 				twotothe = (int) pow(2,cycle);
 				for(j=0;j<twotothe;j++)
 				      	ptr->Input[0][index-(j*skip)-(skip/2)]
 				      	= ((unsigned char)
					(ptr->Input[0][index-(j*skip)] +
 				      	ptr->Input[0][index-((j+1)*skip)]))/2;
 				cycle++;
 				skip/=2;
 				}
 			index -= m_factor;
 			}
 		}
 	
 	   if(m_factor < 0)
 		{ /* Sample the image by every | MagFactor | sample    */
 		k = ptr->StartLine + (line-ptr->OutStartLine)*( -1 * m_factor );

		IOStatus = read_PDS_image_line(ptr->InputFileName,InputLine,
				k,ptr->StartSample,ptr->InputSamples,band);
		if ( IOStatus < 0 )
			return -1;

		if(ptr->NumDNs == 256)
			for( k=0,j=0; k<NumSamp; k+=(-1*m_factor), j++)
				Mask[d-1][OutSamp+j] = ptr->LUT[(ptr->NumDNs-1)&
					  InputLine[k]];
		else
			{
			if( ptr->Stretch == NoStretch )
				for( k=0,j=0; k<NumSamp; k+=(-1*m_factor), j++)
					{
					if( InputLine[k] < 0 )
						InputLine[k] = 0;
 					Mask[d-1][OutSamp+j] = SCALE32K *
					ptr->LUT[32767&InputLine[k]];
					}
			else
				for( k=0,j=0; k<NumSamp; k+=(-1*m_factor), j++)
 					Mask[d-1][OutSamp+j] = SCALE64K *
					ptr->LUT[65535&(InputLine[k]+32768)];
			}			
 		}
 	   else		/* Is this zooming instead of shrinking */
 		if(ptr->Ztype == 0 )	/* Zoom by means of interpolation */
 			if((line - ptr->OutStartLine) % m_factor == 0)
 				{
 				if(line != ptr->OutStartLine)
 					for(k=0;k<(m_factor*NumSamp);k++)
 						ptr->Input[0][k] = 
 							ptr->Input[m_factor][k];
 				k = ptr->StartLine + 1 +
 					(line - ptr->OutStartLine)/m_factor;

				IOStatus = read_PDS_image_line(ptr->InputFileName,
						InputLine,k,ptr->StartSample,
						ptr->InputSamples,band);
				if ( IOStatus < 0 )
					return -1;

				if(ptr->NumDNs == 256)
 					for(k=0,j=0;k<NumSamp;k++,j+=m_factor)
					  ptr->Input[m_factor][j] 
						= ptr->LUT[(ptr->NumDNs-1)&
						InputLine[k]];
				else
					{
					if(ptr->Stretch == NoStretch )
 					  for(k=0,j=0;k<NumSamp;k++,j+=m_factor)
						{
						if( InputLine[k] < 0 )
						   InputLine[k] = 0;
 						ptr->Input[m_factor][j] 
						= SCALE32K *
						ptr->LUT[32767&InputLine[k]];
						}
					else
 					  for(k=0,j=0;k<NumSamp;k++,j+=m_factor)
 						ptr->Input[m_factor][j] 
						= SCALE64K * ptr->LUT[65535 & 
						(InputLine[k]+32768)];
					}
 				index = (NumSamp-1)*m_factor;
 				for(k=0;k<(NumSamp-1);k++)
 					{
 					cycle = 0;
 					skip = m_factor;
 					while(skip!=1)
 			        		{ 
 						twotothe = (int) pow(2,cycle); 
 			        		for(j=0;j<twotothe;j++)
 						      {
 						      ptr->Input[m_factor]
 						      [index-(j*skip)-(skip/2)]
 						      = ((unsigned char)
							(ptr->Input[m_factor]
 							[index-(j*skip)] +
 						      	ptr->Input[m_factor]
 							[index-((j+1)*skip)]))/2;
 						      }
 						cycle++;
 						skip/=2;
 						}
 					index -= m_factor;
 					}
				for(k=0;k<(m_factor-1)*(NumSamp-1)+NumSamp;k++)
					Mask[d-1][OutSamp+k] = ptr->Input[0][k];
 				calcfinished = FALSE;
 				}
 			else
 		        	{
 				if(calcfinished == FALSE)
 					{ 
 	 				cycle = 0;
 					skip = m_factor;
 					while(skip != 1)
 					 	{
 	 					twotothe = (int) pow(2,cycle);
 					 	for(k=0;
						 k<(m_factor*NumSamp);k++)
 			  		  	 for(j=0;j<twotothe;j++)
 						  ptr->Input[m_factor-
 							(j*skip)-(skip/2)][k]= 
							((unsigned char)
 							(ptr->Input[m_factor-
							(j*skip)][k] +
 							ptr->Input[m_factor-
 							((j+1)*skip)][k]))/2;
 					        cycle++;
 					    	skip/=2;
 					    	}
 		  			calcfinished = TRUE;
 					}
 				index = ( line - ptr->OutStartLine ) % m_factor;
				for(k=0;k<(m_factor-1)*(NumSamp-1)+NumSamp;k++)
				   Mask[d-1][OutSamp+k] = ptr->Input[index][k];
 				}
 		else			/* Zoom by replication of pixels     */
 			if ( ( line - ptr->OutStartLine ) % m_factor == 0 )
 				{  
 				k = ptr->StartLine+(line - ptr->OutStartLine)
					/ m_factor;

				IOStatus = read_PDS_image_line(ptr->InputFileName,
						InputLine,k,ptr->StartSample,
						ptr->InputSamples,band);
				if ( IOStatus < 0 )
					return -1;

				if(ptr->NumDNs == 256)
					for(k=0;k<NumSamp;k++)
						for(j=0;j<m_factor;j++)
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							ptr->LUT[(ptr->NumDNs-1)&
							InputLine[k]];
				else
					{
					if( ptr->Stretch == NoStretch )
					  for(k=0;k<NumSamp;k++)
						for(j=0;j<m_factor;j++)
						   {
						   if( InputLine[k] < 0 )
						      InputLine[k] = 0;
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							SCALE32K*ptr->LUT[32767 & 
							InputLine[k]];
						   }
					else
					  for(k=0;k<NumSamp;k++)
						for(j=0;j<m_factor;j++)
						   Mask[d-1][OutSamp + j +
							(k*m_factor)] = 
							SCALE64K *
							ptr->LUT[65535 & 
							(InputLine[k]+32768)];
					}
 				for(j=0;j<(m_factor*NumSamp);j++)
 					ptr->Input[0][j] 
					= Mask[d-1][OutSamp+j];
 				}
 			else
				for(j=0;j<(m_factor*NumSamp);j++)
					Mask[d-1][OutSamp+j] = ptr->Input[0][j];
 	   free(InputLine);
	   return(0);
 	}

   		
	/*********************/
 	/* Copy buffer data  */
 	/*********************/
 	void xxacopybuffer( line ) 
 	int line ;             /* output line LINE */
 	{
 	struct BufType *ptr;
 	int 	j, k, m, n,
		Line;			/* Buffer line index	 	*/

	if( line >= minbufline)
	   {
 	   ptr = firstbuf;	
 	   while(ptr != NULL)		/* Loop through all buffers	*/
 	      {
 	      if( line >= ptr->OutStartLine )
		  {
		  if( line < ptr->OutStartLine+ptr->MulFactor*ptr->NumLines )
			{
			Line = (ptr->StartLine-1) + (line-ptr->OutStartLine);

			/* Case of no zoom */
			if( ptr->MagFactor == 1 || ptr->MagFactor == -1 )
			   for( j=0, k=ptr->Samp; k<ptr->Samp+ptr->NumSamp; 
				j++, k++ )
			          for( d=0; d<Dimension; d++ )
					Mask[d][ptr->OutSamp+j] =
					ptr->buffer[d*ptr->Band + 
						    Line*ptr->NumSamp+k];
			else 
			   {			   
			   Line = (ptr->StartLine-1) + 
			       (line-ptr->OutStartLine)*(-1*ptr->MagFactor);

			   if( ptr->MagFactor < 0 )	/* Shrinking */
				for( j=0, k=0; k<ptr->NumSamp; 
 				     j++, k+=(-1*ptr->MagFactor) )
				   for( d=0; d<Dimension; d++ )
				      Mask[d][ptr->OutSamp+j] =
				      ptr->buffer[d*ptr->Band+Line*ptr->NumSamp
						  +ptr->Samp+k];
			   else				/* Zooming */
			      {
			      if((line-ptr->OutStartLine)%ptr->MagFactor == 0)
 			         {  
 			         Line = (ptr->StartLine-1) + 
				     (line - ptr->OutStartLine)/ptr->MagFactor;
			         for( k=0; k<ptr->NumSamp; k++)
				    for( j=0; j<ptr->MagFactor; j++) 
				       for( d=0; d<Dimension; d++ )
				          Mask[d][ptr->OutSamp+j+
					       (k*ptr->MagFactor)] = 
				       	       ptr->buffer[d*ptr->Band + 
					       Line*ptr->NumSamp +
					       ptr->Samp + k];
 			         for( j=0; j<(ptr->MagFactor*ptr->NumSamp); 
				      j++ )
				    for( d=0; d<Dimension; d++ )
				 	ptr->Input[d][j] 
					= Mask[d][ptr->OutSamp+j];
 			         }
 			      else
			         for( j=0; j<(ptr->MagFactor*ptr->NumSamp); 
				      j++ )
				    for( d=0; d<Dimension; d++ )
					Mask[d][ptr->OutSamp+j] 
					= ptr->Input[d][j];
			      }
 			   }
			}
		  }
	      ptr = ptr->next;
 	      }
	   }
	}

   		
	/*************************************************/
	/* Determine Single Band Displays	         */
 	/*************************************************/
 	void xxacalccolorbands()
	{
	struct ImageType *iptr, *iiptr;
	struct ZImageType *zptr, *zzptr;
	struct HistType *hptr, *hhptr;
	unsigned int hval[HMAX];
	struct HistType *index[HMAX];
 	int x, y, z, count;

	iptr = firstimage;
	while ( iptr != NULL )
		{
		iiptr = firstimage;
		while ( iiptr != NULL )
			{
			if ( iiptr->OutStartLine == iptr->OutStartLine &&
			     iiptr->OutStartSample == iptr->OutStartSample )
				iptr->ColorBands++;
			iiptr = iiptr->next;
			}
		iptr = iptr->next;
		}	

	hptr = firsthist;	/* Determine multiple histogram groupings */
	while ( hptr != NULL )
		{
		hhptr = hptr;
		while ( hhptr != NULL && hptr->ColorBands != -2)
			{
			if ( hhptr->HistStLine == hptr->HistStLine &&
			     hhptr->HistStSample == hptr->HistStSample 
			     && hhptr->ColorBands != -2 )
				{
				hptr->HistSet[hptr->ColorBands] = hhptr;
				hptr->ColorBands++;
				if( hptr->ColorBands > 1 )
					hhptr->ColorBands = -2;
				}
			hhptr = hhptr->next;
			}
		hptr = hptr->next;
		}

	hptr = firsthist;	/* Sort histograms' bin values and generate */
	while ( hptr != NULL )	/* colors of blends of histograms	    */
	    {
	    if( hptr->ColorBands != -2 )
		{
		for(x=0;x<=MAXDNVALUE;x++)
	     		{
			for(z=0;z<hptr->ColorBands;z++)
				{
				index[z] = hptr->HistSet[z];
				hval[z] =  hptr->HistSet[z]->Histogram[x];
				}
	     		jsort(hval,index,0,hptr->ColorBands-1);
			for(z=0;z<hptr->ColorBands;z++)
				hptr->Ptr[z][x] = index[z];
			}

		if(hptr->Display == Blend)	/* Allocate memory for colors */
		  {				/* produced by blending hists */
		  hptr->Blend = (unsigned char ***)calloc
				(hptr->ColorBands,sizeof(unsigned char **));
		  if(hptr->Blend != NULL)
			for(x=0;x<hptr->ColorBands;x++)
				{
				hptr->Blend[x] = (unsigned char **)calloc
					(MAXDNVALUE+1,sizeof(unsigned char *));
				for(y=0;y<=MAXDNVALUE;y++)
					hptr->Blend[x][y] = (unsigned char *)
					calloc(Dimension,sizeof(unsigned char));
				}
		  else
			{
			zvmessage("Memory allocation error"," ");
			zvmessage("- histogram color blend aborted."," ");
			continue;
			}

		  for(x=0;x<=MAXDNVALUE;x++) /* Generate colors of blends */
		     for(y=0;y<hptr->ColorBands;y++)
		       for(z=y;z<hptr->ColorBands;z++)
			   if(hptr->Ptr[z][x]->Histogram[x] > 0)
				for(d=0;d<Dimension;d++)
					hptr->Blend[y][x][d]
					+= hptr->Ptr[z][x]->Dn[d];
		  }
		}	
	    hptr = hptr->next;
	    }

	zptr = firstzoom;
	while ( zptr != NULL )
		{
		zzptr = firstzoom;
		while ( zzptr != NULL )
			{
			if ( zzptr->OutStartLine == zptr->OutStartLine &&
			     zzptr->OutStartSample == zptr->OutStartSample )
				zptr->ColorBands++;
			zzptr = zzptr->next;
			}
		zptr = zptr->next;
		}
	}
  		
	/*************************************************/
 	/* Calculate Maximum Number of Lines and Samples */
 	/*************************************************/
 	void xxacalcmaxlinessamples()
 	{
	int    stringlength;		/* String length of strings	      */

 	struct BoxType *bptr;		/* All graphic structures of GMASK.C  */
 	struct EllipseType *eptr;
 	struct BufType *bfptr;
 	struct HistType *hptr;
 	struct ImageType *iptr;
 	struct ZImageType *zptr;
 	struct PDSImageType *PDSiptr;
 	struct PDSZImageType *PDSzptr;
 	struct StringType *sptr;
 	struct TickType *tptr;
 	struct VectorType *ptr;
 	
 	if ( !UserDefined )		/* If user do not define MASK dimens. */
 	      	{
		UserMaxSamples = 0;	/* Reset User Max Lines and Samples   */
		UserMaxLines = 0;	/* to 0 to determine MASK dimensions  */

 		ptr = firstvector;
 		while( ptr != NULL )
 			{
 			if(ptr->Direction == Hori)
 	         		{
 	            		UserMaxSamples = 
					Max( UserMaxSamples,ptr->EndSample );
 			        UserMaxLines = Max( UserMaxLines,ptr->StartLine 
					+ ptr->Width ) ;
 	         		}
 	         	else
 	         		if ( ptr->Direction == Vert )
 	         			{
 	            			UserMaxSamples = Max( UserMaxSamples,
 	                                    ptr->StartLine + ptr->Width);
 			 	        UserMaxLines = 
					    Max(UserMaxLines,ptr->EndLine);
 	         			}
 			ptr = ptr->next;
 			}
 	
 		zptr = firstzoom;
 		while( zptr != NULL )
 			{
			UserMaxSamples = 
				Max( UserMaxSamples,(zptr->InputSamples-1) * 
				zptr->MulFactor + zptr->OutStartSample );
	 	        UserMaxLines = 
				Max( UserMaxLines,
				(zptr->EndLine - zptr->StartLine) * 
				zptr->MulFactor + zptr->OutStartLine );
 			zptr = zptr->next;
 			}
 	
		PDSzptr = firstPDSzoom;
 		while( PDSzptr != NULL )
 			{
			UserMaxSamples = 
				Max( UserMaxSamples,(PDSzptr->InputSamples-1) * 
				PDSzptr->MulFactor + PDSzptr->OutStartSample );
	 	        UserMaxLines = 
				Max( UserMaxLines,
				(PDSzptr->EndLine - PDSzptr->StartLine) * 
				PDSzptr->MulFactor + PDSzptr->OutStartLine );
 			PDSzptr = PDSzptr->next;
 			}
 	
 		bfptr = firstbuf;
 		while( bfptr != NULL )
 			{
			UserMaxSamples = Max(UserMaxSamples,bfptr->StartLine);
			UserMaxLines = Max(UserMaxLines,bfptr->EndLine);
 			bfptr = bfptr->next;
 			}
 	
 	        sptr = firststring;
 		while( sptr != NULL )
 			{
			/* Calculate string length	*/

			CharSpacing = sptr->CharSpac;
			zastringlength(sptr->CharString,sptr->Size,
					sptr->FontThickness,&stringlength);
			CharSpacing = NOOVERRIDE;

 	/*@@@ Need to determine how vertical character strings affect this */
 	            	if ( sptr->FontType == HERSHEY )
 	              		if ( sptr->Angle == 0 )     
 	              			{
 	                    		UserMaxSamples = Max( UserMaxSamples,
 	                                     	sptr->StartSample+stringlength);
 	                    		UserMaxLines = Max( UserMaxLines,
						sptr->LastLine + 5 );
 	              			}
 	              		else    /* +- 90 degrees 	*/
 	              			{
 	                    		UserMaxLines  = Max( UserMaxLines,
 	                                    	sptr->LastLine + 5 );
 	                    		UserMaxSamples = Max( UserMaxSamples,
 	                                     	sptr->StartSample + sptr->Size *
						( XACHARHEIGHT + 1 )) ;
 	              			}
 	            	else            /* block font   	*/
 	              		if ( sptr->Angle == 0 )     
 	              			{
 	                    		UserMaxSamples = Max(UserMaxSamples,
 	                                     	sptr->StartSample+stringlength);
 	                    		UserMaxLines = Max( UserMaxLines,
 	                                   	sptr->LastLine + 5 );
	 	              		}
 	              		else    /* +- 90 degrees 	*/
 	              			{
 	                   	 	UserMaxSamples = Max( UserMaxSamples,
 	                                     	sptr->StartSample + sptr->Size
						* ( XACHARHEIGHT + 1 ));
 	                    		UserMaxLines   = Max( UserMaxLines,
 	                                     	sptr->LastLine + 5 );
 	              			}
 	   		sptr = sptr->next;
 		     	}
 	
 		bptr = firstbox;
 	        while( bptr != NULL )
 			{
 	            	UserMaxLines = 
				Max( UserMaxLines,bptr->StartLine+bptr->Length);
 	            	UserMaxSamples = Max( UserMaxSamples,bptr->StartSample +
 	                   	bptr->Width ) ;
 			bptr = bptr->next;
 			}
 	 	
 		eptr = firstellipse;
 	        while( eptr != NULL )
 		     {
 	             UserMaxLines = 
			Max( UserMaxLines,eptr->CenterLine+eptr->Radius2 );
 	             UserMaxSamples = 
		 	Max( UserMaxSamples,eptr->CenterSample+eptr->Radius1 );
 		     eptr = eptr->next;
 		     }
 	
 		iptr = firstimage;
 		while( iptr != NULL )
 			{
 	            	UserMaxLines = Max( UserMaxLines,iptr->OutStartLine +
 	                   	iptr->EndLine - iptr->StartLine ) ;
 	            	UserMaxSamples = 
				Max( UserMaxSamples,iptr->OutStartSample +
 	                        iptr->InputSamples - 1) ;
 	         	iptr = iptr->next;
 			}
 	
 		PDSiptr = firstPDSimage;
 		while( PDSiptr != NULL )
 			{
 	            	UserMaxLines = Max( UserMaxLines,PDSiptr->OutStartLine +
 	                   	PDSiptr->EndLine - PDSiptr->StartLine ) ;
 	            	UserMaxSamples = 
				Max( UserMaxSamples,PDSiptr->OutStartSample +
 	                        PDSiptr->InputSamples - 1 ) ;
 	         	PDSiptr = PDSiptr->next;
 			}
 	
 		tptr = firsttick;
 		while( tptr != NULL )
 			{					/* Vertical */
 			if ( tptr->StartSample == tptr->EndSample )
 	            		{
 	               		UserMaxLines = 
					Max( UserMaxLines,tptr->EndLine ) ;
 		               	UserMaxSamples = Max( UserMaxSamples,
 					tptr->StartSample + tptr->Length ) ;
 	            		}
 	            	else				/* Horizontal */
 	           	 	if ( tptr->StartLine == tptr->EndLine )
 	            			{
 	               			UserMaxLines = Max( UserMaxLines,
 						tptr->StartLine + tptr->Length);
 	               			UserMaxSamples = Max( UserMaxSamples,
 	                                     	tptr->EndSample ) ;
 	            			}
 	         	tptr = tptr->next;
 			}
 	
 		hptr = firsthist;
 		while ( hptr != NULL )
 			{
 			UserMaxSamples = 
				Max( UserMaxSamples,hptr->HistEnSample ) ;
 			UserMaxLines = Max( UserMaxLines,hptr->HistEnLine ) ;
 			hptr = hptr->next;
 			}
 		}
 	}
  		
	/***********************************************/
	/*  store nonzero and finite slope vector      */
	/***********************************************/
	int xxacalculateklinein(ptr,angle,EndLine,EndSample)
	struct VectorType *ptr;
 	int *EndLine,   /* Ending line and sample of center of ray or   */
	    *EndSample; /* vector.  Currently used only in XASTORERAY   */
	float angle;	/* Angle of polygon rotation measured clockwise	*/
			/* from North.					*/
	{
	int i,j,k,n;	/* Loop control variables	*/
	int count, p, c1, c2, dx, dy;
	float DX, DY, x, y, x_end, y_end;

	float C1X, C1Y;	/* C1, C2, C3 and C4 are the four vertices of a	*/
	float C2X, C2Y;	/* polygon (ray or vector) to be drawn.		*/
	float C3X, C3Y;	/* 	C1 is NW corner, C2 is NE corner, 	*/
	float C4X, C4Y;	/* 	C3 is SW corner, C4 is SE corner.	*/
	
	float width, length;	/* Width and length of polygon		*/

	float sl, ss;		/* Starting line and sample of polygon  */
				/* which is center of SOUTH line segm.  */

	float B[4],MLT[4];	/* Array of Y intercepts and inverse    */
				/* slopes of lines C1C2,C1C3,C2C4,C3C4. */

	float POINTS[4][3];	/* Coordinates of C1,C2,C3,C4 w/ point  */
				/* identifier or point number.		*/
				/* POINTS[0][0] = Cn x coordinate value	*/
				/* POINTS[0][1] = Cn y coordinate value	*/
				/* POINTS[0][2] = n or point number	*/
	
	float dummy[3];		/* Dummy array used in sort routine	*/

	double cosB, sinB;	/* Rotation matrix values		*/

	double deg_to_rad;	/* Conversion factor to radians		*/
	
	int currentline;	/* Line value for output mask image	*/
	float cline;

	/* Load local variables from polygon structure description 	*/
	sl 	= ptr->StartLine;
	ss 	= ptr->StartSample;
	width	= ptr->Width;
	length	= ptr->Length;

	deg_to_rad = (2.0 * 3.14159265) / 360.0;

	/* Determine values in rotation matrix		*/	

	cosB	= cos( (double)( deg_to_rad * angle ) );
	sinB	= sin( (double)( deg_to_rad * angle ) );

	/* Determine vertices of rotated polygon	*/
	
	C1X	= ss + ( cosB*( -1*(width/2.0) ) + sinB*( length ) );
	C1Y	= sl - ( cosB*( length ) - sinB*( -1*(width/2.0) ) );

	C2X	= ss + ( cosB*( width/2.0 ) + sinB*( length ) );
	C2Y	= sl - ( cosB*( length ) - sinB*( width/2.0 ) );

	C3X	= ss + ( cosB*( -1*(width/2.0) ) );
	C3Y	= sl - ( -1*sinB*( -1*(width/2.0) ) );

	C4X	= ss + ( cosB*( width/2.0 ) );
	C4Y	= sl - ( -1*sinB*( width/2.0 ) );

	/* Determine Y-intercepts ( B[4] ) of lines C1C2,C1C3,C2C4,C3C4 */

	B[0]	= C2Y - (( C2Y - C1Y )/( C2X - C1X ))*C2X;
	B[1]	= C3Y - (( C3Y - C1Y )/( C3X - C1X ))*C3X;
	B[2]	= C4Y - (( C4Y - C2Y )/( C4X - C2X ))*C4X;
	B[3]	= C4Y - (( C4Y - C3Y )/( C4X - C3X ))*C4X;

	/* Determine inverse slope multipliers ( MLT[4] ) of lines	*/

	MLT[0] 	= (( C2X - C1X )/( C2Y - C1Y ));
	MLT[1] 	= (( C3X - C1X )/( C3Y - C1Y ));
	MLT[2] 	= (( C4X - C2X )/( C4Y - C2Y ));
	MLT[3] 	= (( C4X - C3X )/( C4Y - C3Y ));

	/* Load C1,C2,C3,C4 into POINTS array with identifiers		*/
	
	POINTS[0][0] 	= C1X;
	POINTS[0][1] 	= C1Y;
	POINTS[0][2] 	= 1;

	POINTS[1][0] 	= C2X;
	POINTS[1][1] 	= C2Y;
	POINTS[1][2] 	= 2;

	POINTS[2][0] 	= C3X;
	POINTS[2][1] 	= C3Y;
	POINTS[2][2] 	= 3;

	POINTS[3][0] 	= C4X;
	POINTS[3][1] 	= C4Y;
	POINTS[3][2] 	= 4;

	/* The midpoint of line C1C2 is the ending point of the vector or
	   ray that is to be returned to the calling routine.		*/

	*EndLine   = ( POINTS[0][1] + POINTS[1][1] ) / 2.0 ; 
	*EndSample = ( POINTS[0][0] + POINTS[1][0] ) / 2.0 ;

	/* Sort vertices C1,C2,C3,C4 of polygon in POINTS array 	*/

	for ( k=0; k<3; k++ )
	   for ( j=0; j<3; j++ )
		
		if ( POINTS[j][1] > POINTS[j+1][1] )
		   {
	 	   for ( i=0; i<3; i++ )
			dummy[i] = POINTS[j][i];
	 	   for ( i=0; i<3; i++ )
			POINTS[j][i] = POINTS[j+1][i];
	 	   for ( i=0; i<3; i++ )
			POINTS[j+1][i] = dummy[i];
		   }

	/* Determine starting and ending line and sample	*/
	ptr->StartLine   = POINTS[0][1];
	ptr->StartSample = POINTS[0][0];
	for ( k=1; k<4; k++ )
		if ( ptr->StartSample > POINTS[k][0] )
			ptr->StartSample = POINTS[k][0];

	ptr->EndLine   = POINTS[3][1];	
	ptr->EndSample = POINTS[0][0];
	for ( k=1; k<4; k++ )
		if ( ptr->EndSample < POINTS[k][0] )
			ptr->EndSample = POINTS[k][0];

	/* Check mask boundary 					*/
	if ( ptr->StartLine   <= 0 ||
	     ptr->StartSample <= 0 ||
	     ptr->EndSample > UserMaxSamples ||
	     ptr->EndLine   > UserMaxLines 	)
		return(-1);

	/* Edge lines (left-to-right) for polygons with various
	   starting (top) vertice points C1,C2,C3,C4:

			
		       C2  <-  TOP VERTEX in this example
			*
                      *   *
		    *   1   *
               C1 *  -  -  -  * 
                    *     2     *
                      * -  -  -   * C4
                        *   3   * 
                          *   *  
                            * 
                           C3


			Top Vertex	Edge Lines	POINTS[x][2] Indices 
		
	Section 1:	C1		C1C3 & C1C2		1, 0
		2:			C1C3 & C2C4		1, 2
		3:			C3C4 & C2C4		3, 2	

	Section 1:	C2		C1C2 & C2C4		0, 2
		2:			C1C3 & C2C4		1, 2
		3:			C1C3 & C3C4		1, 3	

	Section 1:	C3		C3C4 & C1C3		3, 1
		2:			C2C4 & C1C3		2, 1
		3:			C2C4 & C1C2		2, 0	

	Section 1:	C4		C2C4 & C2C4		2, 3
		2:			C2C4 & C1C2		2, 1
		3:			C1C2 & C1C2		0, 1	 
									*/

	dy = POINTS[3][1] - POINTS[0][1] + 2;

	/* Allocate and dimension memory for Edge array in polygon data */
	/* structure.							*/

	ptr->Edge = (int **)calloc(dy,sizeof(int *));
	if ( ptr->Edge != NULL )
           {
	   for ( i=0; i<dy; i++ )
		{
		ptr->Edge[i] = (int *)calloc(2,sizeof(int));
		if ( ptr->Edge[i] == NULL )
		   {
		   zvmessage("XXACALCULATEKLINEIN memory allocation error."," ");
		   zvmessage(" - Polygon for vector or ray not stored."," ");
		   return(-2);
		   }
	  	}
           }
        else
           {
	   zvmessage("XXACALCULATEKLINEIN memory allocation error."," ");
	   return(-2);
           }     

	i = (int)POINTS[0][2];	/* Switch on starting vertex		*/

	switch (i) {

	case 1:		
			for ( cline=POINTS[0][1], n=0;
			      cline<=POINTS[1][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[1]*(cline-B[1]) + .5;
			      ptr->Edge[n][1] = MLT[0]*(cline-B[0]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline; 
			      cline<=POINTS[2][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[1]*(cline-B[1]) + .5;
			      ptr->Edge[n][1] = MLT[2]*(cline-B[2]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline;
			      cline<=POINTS[3][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[3]*(cline-B[3]) + .5;
			      ptr->Edge[n][1] = MLT[2]*(cline-B[2]) + .5;
			      cline += 1.0;
			      }
			break;

	case 2:	
			for ( cline=POINTS[0][1], n=0;
			      cline<=POINTS[1][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[0]*(cline-B[0]) + .5;
			      ptr->Edge[n][1] = MLT[2]*(cline-B[2]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline; 
			      cline<=POINTS[2][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[1]*(cline-B[1]) + .5;
			      ptr->Edge[n][1] = MLT[2]*(cline-B[2]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline;
			      cline<=POINTS[3][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[1]*(cline-B[1]) + .5;
			      ptr->Edge[n][1] = MLT[3]*(cline-B[3]) + .5;
			      cline += 1.0;
			      }
			break;

	case 3:	
			for ( cline=POINTS[0][1], n=0;
			      cline<=POINTS[1][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[3]*(cline-B[3]) + .5;
			      ptr->Edge[n][1] = MLT[1]*(cline-B[1]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline; 
			      cline<=POINTS[2][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[2]*(cline-B[2]) + .5;
			      ptr->Edge[n][1] = MLT[1]*(cline-B[1]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline;
			      cline<=POINTS[3][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[2]*(cline-B[2]) + .5;
			      ptr->Edge[n][1] = MLT[0]*(cline-B[0]) + .5;
			      cline += 1.0;
			      }

			break;

	case 4:	
			for ( cline=POINTS[0][1], n=0;
			      cline<=POINTS[1][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[2]*(cline-B[2]) + .5;
			      ptr->Edge[n][1] = MLT[3]*(cline-B[3]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline; 
			      cline<=POINTS[2][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[2]*(cline-B[2]) + .5;
			      ptr->Edge[n][1] = MLT[1]*(cline-B[1]) + .5;
			      cline += 1.0;
			      }

			for ( cline=cline;
			      cline<=POINTS[3][1]; n++ )
			      {
			      ptr->Edge[n][0] = MLT[0]*(cline-B[0]) + .5;
			      ptr->Edge[n][1] = MLT[1]*(cline-B[1]) + .5;
			      cline += 1.0;
			      }
			break;

	default: 	break;		}


	return (0);
	}
  		
	/************************/
 	/*  initialize buffer   */
 	/************************/
 	void fillbyte(address_of_buffer,value_to_fill,no_of_bytes)
 	int   address_of_buffer, value_to_fill, no_of_bytes ;
 	{
 	    int   i, nbytes ;
 	    int   maxbytes = 65535;
 	
 	    nbytes = no_of_bytes ;

/*    use memset instead of fill because fill.mar is not portable  FFM 4-7-93 */

 	    while ( nbytes >= maxbytes )
 	    	{
 	       	memset( (void* )address_of_buffer, value_to_fill, maxbytes ) ;
 	       	address_of_buffer += maxbytes ;
 	       	nbytes -= maxbytes ;
 	    	}

 	    memset ( (void *) address_of_buffer, value_to_fill, nbytes ) ;
 	}
  		
	/***************************/
 	/*  Sort histogram buffers */
 	/***************************/
	
	void jsort(hval,pointers,left,right)
	unsigned int 	hval[],
			pointers[];
	int 	left,right;
	{
	int	i, last;
		
	if( left>=right )
	 	return;
	
	swap( hval, left, (left+right)/2 );	
	swap( pointers, left, (left+right)/2 );	

	last = left;
 
	for( i=left+1; i<=right; i++ )
		if( hval[i] < hval[left] )
			{
			swap( hval, ++last, i );	
			swap( pointers, last, i );
			}	
	swap( hval, left, last );	
	swap( pointers, left, last );	

        jsort( hval, pointers, left, last-1 );
        jsort( hval, pointers, last+1, right );
	}
  		
	/***************************/
 	/*  Swap values in array   */
 	/***************************/
	
	void swap( value, i, j )
	unsigned int 	value[];
	int 	i, j;
	{
	unsigned int temp;
	
	temp = value[i];
	value[i] = value[j];
	value[j] = temp;
	}
  		
	/*************************************************************/
 	/* Get image dimensions and data format for PDS image file   */
 	/*************************************************************/
	
	int get_pds_image_stats( file, nl, ns, nb, bytes, record_bytes,
		image_start_record )
	char 	*file;
	int	*nl, *ns, *nb, *bytes, *record_bytes, *image_start_record;
	{
	AGGREGATE 	label_ptr;
	char 		*value;
	int		stat;
	char		string[MAXFILENAME];

	/* Open PDS labeled image file	*/

	strcpy(string,file);

	label_ptr = gpds_open_file( string, &stat );
	if ( stat < 0 )
		return -1;	
	
	/* Get the number of lines, samples, bands and bytes in image file */
	/* This requires that the image have the following keywords in its */
	/* EMBEDDED PDS label: LINES, LINES_SAMPLES, BANDS, SAMPLE_BITS.   */

	value = gpds_get_label_value( label_ptr, "IMAGE", "LINES", 1, &stat );
	if ( stat < 0 )
		{
		gpds_close_file( label_ptr );
		return -1;
		}
	*nl = atoi(value);

	value = gpds_get_label_value( label_ptr, "IMAGE", "LINES_SAMPLES", 
			1, &stat );
	if ( stat < 0 )
		{
		gpds_close_file( label_ptr );
		return -1;
		}
	*ns = atoi(value);

	value = gpds_get_label_value( label_ptr, "IMAGE", "SAMPLE_BITS", 
			1, &stat );
	if ( stat < 0 )
		{
		gpds_close_file( label_ptr );
		return -1;
		}
	*bytes = atoi(value) / 8;

	value = gpds_get_label_value( label_ptr, "IMAGE", "BANDS", 1, &stat );
	if ( stat < 0 )
		*nb = 1;
	else	
		*nb = atoi(value);	

	/* Get the starting record of the image object and the number of */
	/* bytes per record.						 */

	value = gpds_get_label_value( label_ptr, "ROOT", "RECORD_BYTES", 
			1, &stat );
	if ( stat < 0 )
		{
		gpds_close_file( label_ptr );
		return -1;
		}
	*record_bytes = atoi(value);

	value = gpds_get_label_value( label_ptr, "ROOT", "IMAGE", 1, &stat );
	if ( stat < 0 )
		{
		gpds_close_file( label_ptr );
		return -1;
		}
	*image_start_record = atoi(value);

	gpds_close_file( label_ptr );

	return 0;			/* Normal return	*/
	}
  		
	/***************************************/
	/* Return PDS image file index         */
	/***************************************/
	int	get_pds_image_index(file)
	char *file;
	{
	int	i;
	int	file_found;

	file_found = FALSE;

	for ( i=0; i<PDSimagecount && file_found==FALSE; i++ )
		if ( strcmp( pds_image[i].file,file ) == 0 )
			file_found = TRUE; 
	
	if ( file_found==FALSE )
		return -1;
	else
		return --i;
	}
  		
	/*************************/
 	/* Read PDS image line   */
	/*************************/

	int read_PDS_image_line(file,line,line_number,start_sample,
		samples,band_number)
	char *file;
	short int *line;
	int 	band_number;
	int	line_number;
	int	samples;
	int	start_sample;
	{
	int		i, j, k;
	unsigned char 	*temp;
	long int	byte_offset;
	int		index;
	int		status;

	index = get_pds_image_index(file);
	if ( index < 0 )
		return -1;

	/* Seek until appropriate record is found in image file */
	/* by determining byte offset for reading		*/

	byte_offset = pds_image[index].record_bytes * 
			((pds_image[index].start_record - 1) 
			+ (line_number - 1))
			+ ((start_sample - 1) * pds_image[index].bytes);

	status = fseek( pds_image[index].fp, byte_offset, 0 );
	if ( status != 0 )
		return -1;

	if ( pds_image[index].bytes == 1 )
		{
		temp = (unsigned char *)calloc(samples,sizeof(unsigned char));
		if ( temp==NULL )
			return -1;
		fread( temp,sizeof(unsigned char),samples,pds_image[index].fp );
		status = ferror(pds_image[index].fp);
		if ( status != 0 )
			return -1;

		for ( i=0; i<samples; i++ )
			*(line+i) = *(temp+i);
		free ( temp );
		}
	else
		if ( pds_image[index].bytes == 2  )
			{
			fread( line,sizeof(short int),samples,
				pds_image[index].fp );
			status = ferror(pds_image[index].fp);
			if ( status != 0 )
				return -1;
			}
		else
			return -1;

	return 0;			/* Normal return	*/
	}
  	
