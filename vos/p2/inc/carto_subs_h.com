$!****************************************************************************
$!
$! Build proc for MIPL module carto_subs_h
$! VPACK Version 2.1, Thursday, December 10, 2015, 13:29:54
$!
$! Execute by entering:		$ @carto_subs_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
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
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module carto_subs_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to carto_subs_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("carto_subs_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @carto_subs_h.bld "STD"
$   else
$      @carto_subs_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create carto_subs_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack carto_subs_h.com -mixed -
	-s cartoGridUtils.h cartoGtUtils.h cartoLsqUtils.h cartoMatUtils.h -
	   cartoMemUtils.h cartoSortUtils.h cartoStrUtils.h cartoTaeUtils.h -
	   astroreference_camera.h ephreference_camera.h eos_coords.h mat33.h -
	   qmalloc.h quaternion.h rodrigues.h time_conversion.h safe_sqrt.h -
	   time_utils.h strsel.h count_lines.h tokenize.h fgetl.h -
	   sprintf_alloc.h burl.h georeference_camera.h utils_return_values.h -
	   cartoTieUtils.h ibisControlMapper.h ibishelper.h cartoLinkedList.h -
	   ImageUtils.h cloud_masks.h cartoLoggerUtils.h hdfIncludes.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cartoGridUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOGRIDUTILS_H
#define CARTOGRIDUTILS_H

extern double gridtol;
void cartogetline(double *x,double*y,
	     int istart, int inc, int nmax, int bign, int* nfound, int* null9);

void getline2(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound);

void getline3(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound);

void gridfill(double *x,double *y, int istart, int inc, int nmax);

void tgrid(int npoints,
	   int* nlinret,
	   double* ptx,
	   double* pty,
	   int* ntriang,
	   int** tcon1,
	   int** tcon2,
	   int** tcon3,
	   int gridnah,
	   double* csol,
	   double* optx,
	   double* opty,
	   int zgeom);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoGtUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOGTUTILS_H
#define CARTOGTUTILS_H

/*================================================================

int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*/

int gtgetlab( char * inp, int instance, char ** labelstr, int * nl, int * ns);

/*================================================================

int invertmap

invertmap calculates the inverse of a six-vector double precision
map.

function return:
     ier from the dgauss call (see dgauss)

arguments:
      1. map: double[6] map;
	 (input) coefficients to convert pixel to coord OR
	 COORD TO PIXEL
      2. invmap: double[6] invmap;
	 (output) coefficients to convert the other way
*/

int invertmap( double * t, double * tinv);

/*================================================================

int geofix

geofix translates label coordinates into a linear transformation
vectors that can be used for VICAR pixel-to-map or map-to-pixel
conversions.  If the file is a VICAR image then the mapping of 
the corner points is also returned (the (1,1) and (nline,nsamp)
centers of the corner pixels).

The convention for the transforms is (line,samp) -> (East,North)
for the map and (East,North) -> (line,samp) for the invmap
for convenience in working with VICAR.

Note that VICAR pixel referencing is different than GeoTIFF
pixel referencing (both "area" and "point"/"post" types).

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. map: double[6] map;
	 (output) coefficients to convert pixel to coord
      3. invmap: double[6] invmap;
	 (output) coefficients to convert coord to pixel
      4. nl: int nl;
	 (input) number of lines in vicar image to calc corner
      5. ns: int ns;
	 (input) number of samples in vicar image to calc corner
      6. corner: double[4] corner;
	 (output) the mapping of the corners (.5,.5,nl+.5,ns+.5)
	 if the file is a VICAR image, else zero
*/

int geofix( char * labelstr, double * map, double * invmap, int nl, int ns, double * corner);

/*================================================================

int gtrect

gtrect tests whether the mapping is "rectangular".  This means 
that the GeoTIFF label has the keyword MODELPIXELSCALETAG or that
it has the keyword MODELTRANSFORMATIONTAG and the upper left part
of the transformation has two 0.0 in diagonal formation.  To allow
for a slight inaccuracy in a calculated transformation, a 
parameter eps is provided for values very close to 0. It actually
ratios with the main terms, see code below.

function return:
     int, 1 if mapping is rectangular, else 0

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. eps: double eps;
	 (input) tolerance about zero for the off-diagonals
	 to still consider the mapping rectangular. It is a
	 ratio to the largest term.  suggest 1.0e-12.
*/

int gtrect( char * labelstr, double eps );

int gtcompval( char * p1, char * p2 );

/*================================================================

int gtmapcom

gtmapcom tests whether the two mappings are compatible.  This is
defined as having the same values for a list of attributes that
pertain to mappings.  The list is taken from the GeoTIFF spec
rev 1.0.  If the "value" contains parenthetical material, it is
not required to be the same.  Vector values are required to be
the same.  There is a tolerance of 1 part in 1e-12 on all numeric
values.

If one of the listed attributes is present in one label it must
also be present in the other label.  This prevents default values
from making two labels incompatible, or for alternate keywords
to do the same.

function return:
     int, 1 if mappings are compatible, else 0

arguments:
      1. labelstr1: char *labelstr;
	 (input) string containing the first GeoTIFF label
      1. labelstr2: char *labelstr2;
	 (input) string containing the second GeoTIFF label
*/

int gtmapcom( char * labelstr1, char * labelstr2 );

/*================================================================

int gtgetrot

gtgetrot gets the rotation number of the GeoTIFF label.  A standard
VICAR rotation is 1.  See the help PDF for VICAR routine GTLIST for
information on the other rotations.

function return:
     int, a number from 0 to 8 designating the rotation relative to
     an (East,North) coordinate system.

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
*/

int gtgetrot( char * labelstr );

/*================================================================

gtreplab

gtreplab writes a GeoTIFF label into the property part of VICAR label

function return : void

arguments:
      1. fileparm: input, char *fileparm;
         name of the file to put the label, usually "INP"
      2. nfile: input, int nfile;
         which file of fileparm, set to 1 if only one file
      3. labstr: input, char *labstr;
         string containing new GeoTIFF label, comma separated,
         see GTGEN document for format details, and TSTGTGEN.PDF
         for examples
      4. add: input, int add;
         0 - then the old label is deleted and labstr
             becomes the new label
         1 - then the old label is kept and added to,
      5. replflag: input, int replflag;
         0 - no processing of coord-pixel mapping
         1 - coord-pixel mapping replaced w/ next three params
         2 - coord-pixel mapping replaced w/ next three params, but
             MODELPIXELSCALETAG and MODELTRANSFORMATIONTAG type labels
             are swapped due to rotation
      6. tinv: input, double tinv[6];
         will recalculate every MODELTIEPOINTTAG using this transf
      7. scalestr: input, char *scalestr;
         will replace an occurrence of MODELPIXELSCALETAG with this
      8. transstr: input, char *transstr;
         will replace an occurrence of MODELTRANSFORMATIONTAG with this
      
*/

void gtreplab( char * fileparm, int nfile, char * labstr, int add, int replflag, double * tinv, char * scalestr, char * transstr );

/*================================================================

int gtgetscl

gtgetscl gets the scale factors of the GeoTIFF label.  There are two
types of scale that can be returned:

The first is for the four rotations that can be represented by the
MODELTIEPOINTTAG-MODELPIXELSCALETAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips".  The
odd thing about this scale is that the increasing "lines" coordinate
to the South is denoted by a positive scale factor.

The second is for the four rotations that have to be represented
by the MODELTRANSFORMATIONTAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips" of a
single ninety degree rotate.

function return:
     void

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. sctype: int *sctype;
	 (output) type of scale factors (see above)
	 1 - from MODELPIXELSCALETAG
	 2 - from MODELTRANSFORMATIONTAG
      3. scale1: double *scale1;
	 (output) scale factor 1
      4. scale2: double *scale2;
	 (output) scale factor 2
*/

void gtgetscl( char * labelstr, int * sctype, double * scale1, double * scale2 );

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLsqUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOLSQUTILS_H
#define CARTOLSQUTILS_H

/*=====================================================================
dgauss

dgauss solves a set of linear equations via gaussian elimination

arguments:

     1. a: input and output, double *a;
	m by m coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector; output solution.
     3. m: input, int m;
	number of linear equations.
     4. eps: input, double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     5. ierror: output int *ierror;
	result 0=OK, 1=pivot is zero, K=loss of significance warning
	pivot less than eps times max element of a

The matrix a is stored by column order

*/
void dgauss( double * a, double * r, int m, double eps, int * ierror );

/*=====================================================================
lsqfit

lsqfit solves for the minimum least squares fit (for ax=r, the minimum
over all x of L2 norm of r-ax)

The matrix a is stored by column order

arguments:

     1. a: input and output, double *a;
	m by n coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector.
     3. m: input, int m;
	number of linear equations.
     4. n: input, int n;
	number of independent coords; dimension of x.
     5. x: output, double *x;
	solution vector.
     6. eps: input double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     7. ierror: output int *ierror;
	result 0=OK; K=singular at kth column
	-1=zero matrix a; -2=m<n

*/

void lsqfit( double * a, double * r, int m, int n, double * x, double eps, int * ierror );

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoMatUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOMATUTILS_H
#define CARTOMATUTILS_H

int dceiling( double x );

double xzprod( double x1, double y1, double x2, double y2 );

/* signed triangular area, for polygon use xp,yp as fixed point */
/* in (E,N) coordinates,        clockwise is positive area */
/* in (N,E) coordinates, counterclockwise is positive area */

double triarea( double xp, double yp, double x1, double y1, double x2, double y2 );

void lin2( double * a, double * b, double * x, double eps );

void segxseg( double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double * w );

void insert_seg( int jj, int * ccount, double * p4max, double xbig, double ybig, double xjbig, double yjbig, int wchcall );

void thiessen( int npoints, int * nlinret, double reject, double skinny, int abendi, double * ptx, double * pty, int * ntriang, int ** tcon1, int ** tcon2, int ** tcon3 );

int insidetri( double x, double y, double x1, double y1, double x2, double y2, double x3, double y3 );

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoMemUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOMEMUTILS_H
#define CARTOMEMUTILS_H

/*=========================================================

zvparmalloc

Allocate enough memory for named parameter, assumed to be a single
string. Retrieve parameter, storing in allocated buffer. Return
buffer.

arguments:
     1. name: parameter name

Returned pointer should be freed when no longer needed.

*/
char* zvparmalloc( char* name );

/*=========================================================

mz_alloc1

allocate a one dimensional array of any type

arguments:
     1. buf: output, unsigned char **buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	dimension of the array
     3. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space may be
released with the free(buf) statement.
*/
void mz_alloc1( unsigned char ** buf, int d1, int w );

/*=========================================================

mz_alloc2

allocate a two dimensional array of any type

arguments:
     1. buf: output, unsigned char ***buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free2(buf,d1) so all of the parts can be freed
in reverse order.
*/

void mz_alloc2( unsigned char *** buf, int d1, int d2, int w );

/*=========================================================

mz_free2

free a two dimensional array created by mz_alloc2

arguments:
     1. buf: output, unsigned char **buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array

The subparts are freed first and then the top part.  Use
the first dimension from the mz_alloc2 call.

*/
void mz_free2( unsigned char ** buf, int d1 );

/*=========================================================

mz_alloc3

allocate a three dimensional array of any type

arguments:
     1. buf: output, unsigned char ****buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. d3: input, int d3;
	third dimension of the array
     5. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free3(buf,d1,d2) so all of the parts can be freed
in reverse order.
*/
void mz_alloc3( unsigned char **** buf, int d1, int d2, int d3, int w );

/*=========================================================

mz_free3

free a three dimensional array created by mz_alloc3

arguments:
     1. buf: output, unsigned char ***buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array

The subparts are freed first and then the top part.  Use
the first two dimensions from the mz_alloc3 call.

*/
void mz_free3( unsigned char *** buf, int d1, int d2);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoSortUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOSORTUTILS_H
#define CARTOSORTUTILS_H

#define CART_CHAR   1
#define CART_SHORT  2
#define CART_INT    3
#define CART_FLOAT  4
#define CART_DOUBLE 5
#define CART_LONG   6
#define CART_UCHAR  7
#define CART_USHORT 8
#define CART_UINT   9
#define CART_ULONG  10
#define CANNOT_COMPARE -2

void getSelectionSortIndices(void *unsorted, int *indices, int n, int type);

void sort8( double * buf, int * ptr, int n );

void sort88( double * buf, int * ptr, int n );

void sortrec4( int * key, int * ptr, int len );

void sortrec88( double * key, int * ptr, int len );

void sortrec8( double *key, int* ptr,int len );

void sortretn8( double *key, int* ptr, int len );

void sort4(int *buf, int *ptr, int n);

void sort7( float *buf, int *ptr, int n );

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoStrUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*#ifndef CARTOSTRUTILS_H
#define CARTOSTRUTILS_H*/

void rztrim( char * buf );

void nicelen( char * hdr, double val, char * buf);

void scalefmt( char * outbuf, double scale1, double scale2 );

void trnsfmt( char * outbuf, double * t );

int grab( char * p, char c, char * buf );

/*================================================================

ms_dnum

ms_dnum converts a string to a double and moves the pointer, also
allows for positive and negative exponent with e or E or D or d, for
example 123.45E-002

function return : double

argument :
      1. num_ptr: input, char **num_ptr;

*/

double ms_dnum ( char ** num_ptr );

/*  parse ascii file to fill a TAE TCL variable   A. Zobrist  5/26/00   */


/*================================================================
ms_num

ms_num converts a string to an integer.

function return : integer

argument :
      1. num_ptr: input, char *num_ptr;

*/

int ms_num (char *num_ptr);

/*===================================================================

ms_find

ms_find searches string str1 for the substring str2 and returns
a pointer to the first location in string after the substring.

function return : character pointer

arguments :

      1. str1: input, char *str1;

      2. str2: input, char *str2;

Null pointer is returned if substring is not found.

*/

char *ms_find( char * str1, char * str2 );

char *nameget(char* s);

/*#endif*/
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoTaeUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOTAEUTILS_H
#define CARTOTAEUTILS_H

/*======================================================

mq_out_int

output integer value to parameter block

arguments :

      1. pname: input, char *pname;
         Parameter name.

      2. val: output, int val;
         Value output to parameter block.

mq_out_int returns integer variables to the parameter block.
*/

void mq_out_int (char *pname, int val);

/*======================================================

mq_out_real

output real value to parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, double val;
	 Value output to parameter block.

mq_out_real returns integer variables to the parameter block.  The name
and value are output to the print log.
*/

void mq_out_real (char *pname, double val);

/*======================================================

mq_out_string

returns character string to the parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, char *val;
	 String returned to parameter block.

      3. maxlen: input, int maxlen;
	 max length to put to output parameter block

mq_out_string returns character strings to the
parameter block.
*/

void mq_out_string (char *pname, char *val, int maxlen);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create astroreference_camera.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __ASTROREFERENCE_CAMERA_H
#define __ASTROREFERENCE_CAMERA_H

#ifndef ASTROREFERENCE_ECEF 
#define ASTROREFERENCE_ECEF 0
#endif

#ifndef ASTROREFERENCE_TOD 
#define ASTROREFERENCE_TOD 1
#endif

int astroreference_camera(double *urange, double *vrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);
int astroreference_camera_sv_c(double *urange, double *vrange, double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ephreference_camera.h
$ DECK/DOLLARS="$ VOKAGLEVE"
int ephreference_camera_sv_c(double *urange, double *vrange, double *hrange,
			     double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, 
			     double fu, double fv, double q, double u0, double v0, double *kappa, 
			     double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr,
			     int clen, int calc_mode,int *calc_case);
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create eos_coords.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __EOS_COORDS_H
#define __EOS_COORDS_H


#ifdef __cplusplus
extern "C" {
#endif

  int eos_coords(double TDB, double UT1, double *J2000_R_TOD, double *ECEF_R_TOD);

#ifdef __cplusplus
}
#endif

#endif


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mat33.h
$ DECK/DOLLARS="$ VOKAGLEVE"
int mat33_mat33_mult(double *A, double *B, double *C);
double mat33_det(double *A);
int mat33_add(double *A, double *B, double *C);
int mat33_inverse(double *A, double *B);
int mat33_assign(double *A, double a0, double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8);
int mat33_print(double *A);
int mat33_copy(double *A, double *B);
int mat33_transpose(double *A, double *B);
double mat33_xtax(double *X, double *A);
double mat33_xtay(double *X, double *A, double *Y);
int mat33_vec31_mult(double *A, double *V, double *B);
int vec31_add(double *A, double *B, double *C);
int vec31_assign(double *V, double v0, double v1, double v2);
int vec31_copy(double *A, double *B);
int vec31_subtract(double *A, double *B, double *C);
int mat33_scale(double *A, double s, double *B);
int vec31_print(double *A);
int vec31_scale(double *A, double s, double *B);
int vec31_axpy(double a, double *X, double *Y, double *Z);
double vec31_vec31_dot(double *A, double *B);
int vec31_vec31_cross(double *A, double *B, double *C);
double vec31_norm(double *A);
int vec31_unit(double *A);
int vec31_vec31_basis(double *A, double *B, double *C);
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create qmalloc.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __QMALLOC_H
#define __QMALLOC_H

#include <stdlib.h>

#define QMALLOC_TRUE  1
#define QMALLOC_FALSE 0

#ifdef __cplusplus
extern "C" {
#endif

void *qmalloc(size_t nelem, size_t elsize, int reset, char *infunc,
	      char *vname);

#ifdef __cplusplus
}
#endif
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create quaternion.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __QUATERNION_H
#define __QUATERNION_H

int rodrigues_to_quaternion(double *v, double *q);
int quaternion_to_rodrigues(double *q, double *v);
int quaternion_to_mat(double *q, double *R);
int mat_to_quaternion(double *R, double *q);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rodrigues.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __RODRIGUES_H
#define __RODRIGUES_H

int rodrigues_vec2mat(double *wrot, double *R);
int rodrigues_mat2vec(double *R, double *wrot);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create time_conversion.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __TIME_CONVERSION_H
#define __TIME_CONVERSION_H

/* From UTC_ISO_TIME_STRING */
int utc_iso_time_string_to_si(char *UTC_string, double *SI_adr);
int utc_iso_time_string_to_tt_and_ut1(char *UTC_string, double Delta_UT1, double *TT_adr, double *UT1_adr);
int utc_iso_time_string_to_tt_iso_time_string(char *UTC_string, char **TT_string_adr);
int utc_iso_time_string_to_ut1_iso_time_string(char *UTC_string, double Delta_UT1, char **UT1_string_adr);
int utc_iso_time_string_to_leapsec_count(char *UTC_string, double *leapsec_count_adr);
int utc_iso_time_string_to_dut1tbl_time_string(char *UTC_string, char **DUT1_table_string_adr);

/* From UTC time components */
int utc_time_components_to_leapsec_count(int y0, int t0, int d0, int h0, int m0, double s0, double *leapsec_count_adr);
int utc_time_components_to_leap_table_index(int y0, int t0, int d0, int h0, int m0, double s0, int *k_adr);

/* From ACS */
int acs_to_tt_and_ut1(double ACS, double Delta_UT1, double *TT_adr, double *UT1_adr);
int acs_to_tt(double ACS_time, double *TT_adr);
int acs_to_ut1(double ACS_time, double Delta_UT1, double *UT1_adr);
int acs_to_utc_iso_time_string(double ACS, char **UTC_string_adr);

/* From SI */
int si_to_tt_and_ut1(double SI, double Delta_UT1, double *TT_adr, double *UT1_adr);
int si_to_tt(double SI, double *TT_adr);
int si_to_ut1(double SI, double Delta_UT1, double *UT1_adr);
int si_to_utc_iso_time_string(double SI, char **UTC_string_adr);

/* From TT_ISO_TIME_STRING */
int tt_iso_time_string_to_tt(char *TT_string, double *TT_adr);
int tt_iso_time_string_to_tdb(char *TT_string, double *TDB_adr);

/* From TT */
int tt_to_utc_iso_time_string(double UTC, char **UTC_string_adr);
int tt_to_si(double TT, double *SI_adr);
int tt_to_ut1(double TT, double Delta_UT1, double *UT1_adr);
int tt_to_tdb(double TT, double *TDB_adr);

/* Helper functions */
int lookup_delta_ut1(char *fname, int yyyymmdd, double *Delta_UT1_adr);
int initialize_leap_second_table(char *filename);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create safe_sqrt.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __SAFE_SQRT_H
#define __SAFE_SQRT_H

double safe_sqrt(double a);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create time_utils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __TIME_UTILS_H
#define __TIME_UTILS_H

int julian_date(int year, int month, int day, int hour, int minute, double second, double *jd_adr);
int parse_iso_time_string(char *T, int *year_adr, int *month_adr, int *day_adr, int *hour_adr, int *minute_adr, double *second_adr);
int compose_iso_time_string(int year, int month, int day, int hour, int minute, double second, char **T_adr);
int compose_dut1_table_time_string(int year, int month, int day, int hour, int minute, int second, char **T_adr);
int is_leap_year(int year);
int parse_yyyymmdd(int yyyymmdd, int *year_adr, int *month_adr, int *day_adr);
int compose_yyyymmdd(int year, int month, int day, int *yyyymmdd_adr);
int standardize_time_components(int y0, int t0, int d0, int h0, int m0, double s0, int *y1_adr, int *t1_adr, int *d1_adr, int *h1_adr, int *m1_adr, double *s1_adr);
int calendar_increment(int yyyymmdd_old, int n_days, int *yyyymmdd_new_adr);
int month_string_to_month(char *month_str, int *month_adr);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create strsel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef STRSEL_H
#define STRSEL_H

#ifdef __cplusplus
extern "C" {
#endif

int strsel(char *dest, char *src, int i0, int i1);

#ifdef __cplusplus
}
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create count_lines.h
$ DECK/DOLLARS="$ VOKAGLEVE"
int count_lines(char *filename, int usage, ...);
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tokenize.h
$ DECK/DOLLARS="$ VOKAGLEVE"
int tokenize(char *line, char *sep_char, int *n_adr, int **b_adr, int **e_adr);
int get_max_token_length(int n_tokens, int *bt, int *et);
int extract_token(char *dest, char *src, int *b, int *e, int w);
int get_token_list(char *src, int n_tokens, int *b, int *e, char ***token_list_adr);
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fgetl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __FGETL_H
#define __FGETL_H

char *fgetl(char *s, int n, FILE *fp);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sprintf_alloc.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __SPRINTF_ALLOC
#define __SPRINTF_ALLOC

int sprintf_alloc(char **S_adr, char *fmt, ...);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create burl.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __BURL_H
#define __BURL_H

#include <math.h>

#ifndef MAX
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif

#ifndef ABS
#define ABS(A)    ((A) > 0.0 ? (A) : (-(A)))
#endif

#ifndef ROUND
#define ROUND(A)  (floor((A) + 0.5))
#endif

#ifndef CLIP
#define CLIP(V, A, B) ( MIN((MAX((V),(A))), (B)) )
#endif

#ifndef MAXABS
#define MAXABS(A, B) (MAX(ABS(A),ABS(B)))
#endif

#ifndef SIGNUM
#define SIGNUM(A) (((A) == 0) ? 0 : (((A) > 0) ? 1 : -1))
#endif

#ifndef TWOPI
#define TWOPI (((double) 2.0) * M_PI)
#endif

#ifndef DEG2RAD
#define  DEG2RAD ((M_PI)/((double) 180.0))
#endif

#ifndef ARCSEC2RAD
#define  ARCSEC2RAD ((M_PI)/((double) 180.0 * 3600.0))
#endif

#ifndef RAD2DEG
#define  RAD2DEG   (((double) 180.0)/(M_PI))
#endif

#ifndef D_ZERO
#define D_ZERO ((double) 0.0)
#endif

#ifndef D_ONE
#define D_ONE ((double) 1.0)
#endif

#ifndef D_TWO
#define D_TWO ((double) 2.0)
#endif

#ifndef D_THREE
#define D_THREE ((double) 3.0)
#endif

#ifndef D_FOUR
#define D_FOUR ((double) 4.0)
#endif

#ifndef D_TEN
#define D_TEN ((double) 10.0)
#endif

#ifndef ERR
#define ERR -1
#endif

#ifndef OK
#define OK 0
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef MILLION
#define MILLION 1000000
#endif

#ifndef BILLION
#define BILLION 1000000000
#endif

#ifndef MAXSTRING
#define MAXSTRING 4096
#endif

#ifndef MAXLINE
#define MAXLINE 4096
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create georeference_camera.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __GEOREFERENCE_CAMERA_H
#define __GEOREFERENCE_CAMERA_H

#ifndef GEOREFERENCE_ECEF 
#define GEOREFERENCE_ECEF 0
#endif

#ifndef GEOREFERENCE_TOD 
#define GEOREFERENCE_TOD 1
#endif

int georeference_camera(double *urange, double *vrange, double *hrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);
int georeference_camera_sv_c(double *urange, double *vrange, double *hrange, double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create utils_return_values.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __UTILS_RETURN_VALUES_H
#define __UTILS_RETURN_VALUES_H

#ifndef OK
#define OK 0
#endif

#ifndef ERR
#define ERR -1
#endif

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoTieUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __CARTOTIEUTILS_H
#define __CARTOTIEUTILS_H

void rot90(double* tie, int nrot);
void swp90(double* tie, int nrot);
void flip(double* tie);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisControlMapper.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef IBISCONTROLMAPPER
#define IBISCONTROLMAPPER

#include "ibishelper.h"

/***********************************/
typedef struct{
   int controlID;
   int length;
   int *toIbisIndices;
}IBIS_CONTROL_MAP;

/***********************************/
typedef struct{
   IBIS_CONTROL_MAP **maps;
   int nControls;
}IBIS_CONTROL_MAPPER;

/***********************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getMapper(IBISStruct *ibis, int controlCol);

/***********************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getSingleMapper(IBISStruct *ibis);

/***********************************/
IBIS_CONTROL_MAP* getIbisControlMap(IBISStruct *ibis, int controlCol, double control);

/******************************************************/
IBIS_CONTROL_MAP* IBISCONTROL_getMap(IBISStruct *ibis, int controlCol, double control, int count);

/******************************************************/
void IBISCONTROL_deleteMapper(IBIS_CONTROL_MAPPER** mapper);

/******************************************************/
void IBISCONTROL_deleteMap(IBIS_CONTROL_MAP** map);

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibishelper.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef IBISHELPER
#define IBISHELPER

#include <stdio.h>

#include "cartoLinkedList.h"

#ifndef MAXCOLS
#define MAXCOLS 500
#endif

/* IBIS formatting and printing was copied over from ibis_list.c */
#define IBISPRINT( ptr, fmtchar, fmtStr ) \
	switch (tolower(fmtchar)) { \
		case 'b': printf( fmtStr,    *(char *)  (ptr));  break; \
		case 'h': printf( fmtStr,    *(short *) (ptr));  break; \
		case 'f': printf( fmtStr,    *(long *)  (ptr));  break; \
		case 'r': printf( fmtStr,    *(float *)  (ptr));  break; \
		case 'd': printf( fmtStr,    *(double *)(ptr));  break; \
		case 'c': printf( fmtStr,    ((float*)ptr)[0],((float*)ptr)[1] );break; \
		case 'a': printf( fmtStr,    ptr );break; \
		default:  printf( fmtStr,   "BAD FORMAT!" );break; \
	}

#define IBISFORMAT( str, fmtchar, colsize ) \
	switch (tolower(fmtchar)) { \
		case 'b': \
		case 'h': \
		case 'f': sprintf( str, "%%%dd",  (colsize));  break; \
		case 'r': sprintf( str, "%%%d.2f",  (colsize));  break; \
		case 'd': sprintf( str, "%%%d.2lf", (colsize));  break; \
		case 'c': sprintf( str, " (%%-%d.2f,%%%d.2f)", \
			((colsize)-4)/2, colsize - (4 + ((colsize)-4)/2) );\
			 break; \
		case 'a': sprintf( str, "%%%ds", (colsize));break; \
		default:  sprintf( str, "%%%ds", (colsize));break; \
	}

///////////////////////////////////////////////////
typedef struct{
   int unit, handle;
   int nr, nc;
   char mode[8], org[7];
   char formats[MAXCOLS][6];
   int colLens[MAXCOLS];
   int totRecSize;   // size of each record in bytes
   int totDataSize;  // size of row * col in bytes
   char **data;

} IBISStruct;

///////////////////////////////////////////////////
typedef struct{
   int unit, handle;
   int nr, nc;
   char mode[8], org[7];
   LINKEDLIST *formats;
   LINKEDLIST *colLens;
} IBISPrep;

/* errors when an unknown format is specified    */
void IBISHELPER_wrongFormatError(IBISStruct *ibis, int col);

/* opens the ibis file for read or update        */
IBISStruct* IBISHELPER_openIBIS(char *name, int instance, char *mode);

/* DEPRECATED - USE IBISHELPER_openIBIS_out2     */
/* creates an output ibis file based on the out  */
/* parameter and returns an ibis struct          */
/* creates a new file but does not IBISFileOpen  */
/* until the IBISHELPER_closeIBIS is called      */
/* Data is written out at the end of program     */
IBISStruct* IBISHELPER_openIBIS_out(char **format, int inst, int nr, int nc);

/* writes out data if mode is read or update,    */
/* closes the file and deletes the ibis struct   */
void IBISHELPER_closeIBIS(IBISStruct **ibis);

/* returns the greatest column size              */
int IBISHELPER_getMaxColLen(IBISStruct *ibis);

/* casts the IBIS data into an integer data type */
/* and returns the integer                       */
/*                                               */
/* col and index start offset at 0 not 1         */
int IBISHELPER_getInt(IBISStruct *ibis, int col, int index);

/* casts the IBIS data into a float data type    */
/* and return the float                          */
/*                                               */
/* col and index start offset at 0 not 1         */
float IBISHELPER_getFloat(IBISStruct *ibis, int col, int index);

/* casts the IBIS data into a float data type    */
/* and return the double                         */
/*                                               */
/* col and index start offset at 0 not 1         */
double IBISHELPER_getDouble(IBISStruct *ibis, int col, int index);

/* copies a strint into the parameter variable   */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_getString(IBISStruct *ibis, char *buf, int col, int index);

/* returns an actual pointer to the data in ibis */
/*                                               */
/* col and index start offset at 0 not 1         */
char* IBISHELPER_getBufPtr(IBISStruct *ibis, int col, int index);

/* gets the formats                              */
void IBISHELPER_getFormats(IBISStruct *ibis, char formats[MAXCOLS][30]);

/* sets the IBIS file buffer at col, index with  */
/* data                                          */
/* data gets written out when                    */
/* IBISHELPER_closeIBIS is called                */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_setDouble(IBISStruct *ibis, int col, int index, double data);

/* sets the IBIS file buffer at col, index with  */
/* data                                          */
/* data gets written out when                    */
/* IBISHELPER_closeIBIS is called                */
/*                                               */
/* col and index start offset at 0 not 1         */
void IBISHELPER_setString(IBISStruct *ibis, int col, int index, char *str);

/* creates an output ibis file based on the out  */
/* parameter and returns an ibis struct          */
/* creates a new file but does not IBISFileOpen  */
/* until the IBISHELPER_closeIBIS is called      */
/* Data is written out at the end of program     */
IBISPrep* IBISHELPER_openIBIS_out2(char *name, int inst, int nr);

/* Since IBISPrep does not require all the       */
/* columns to be declared at start we need to    */
/* set it up using this call.                    */
/* The column number will be in order of the     */
/* call to this function.                        */
void IBISHELPER_addColumn(IBISPrep *ibis2, char *format);

/* Returns an IBISStruct* so that the data can   */
/* be written out                                */
IBISStruct* IBISHELPER_getIBISStruct(IBISPrep **prep);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLinkedList.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOLINKEDLIST
#define CARTOLINKEDLIST

///////////////////////////////////////////
struct node{
   void *data;
   struct node *next;
   struct node *prev;
   int rank;
};

///////////////////////////////////////////
typedef struct{
   struct node *head;
   struct node *tail;
   int size;
} LINKEDLIST;

/*********************************************/
LINKEDLIST* LinkedList_getLinkedList();

/*********************************************/
void LinkedList_setNodeArray(LINKEDLIST *list, struct node** array);

/*********************************************/
void LinkedList_addWithRank(LINKEDLIST *list, void *data, int rank);

/*********************************************/
void LinkedList_add(LINKEDLIST *list, void *data);

/*********************************************/
void* LinkedList_remove(LINKEDLIST *list, int index);

/*********************************************/
void LinkedList_free(LINKEDLIST **list);

/*********************************************/
LINKEDLIST* LinkedList_sortAscending(LINKEDLIST **list);

/*********************************************/
LINKEDLIST* LinkedList_bigMemSortAscending(LINKEDLIST **list);

/*********************************************/
struct node* LinkedList_getMinNode(LINKEDLIST *list);

/*********************************************/
//struct node* LinkedList_getNode(LINKEDLIST *list, int index);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef IMAGE_UTILS
#define IMAGE_UTILS

#define IU_MAX_FNAME_LEN 250
#define IU_MAX_PARM       20
#define IU_SKIP_LINES      0
#define IU_BILINEAR_INTERP 1
#define IU_BICUBIC_INTERP  2
#define IU_NEAR_NEIGHBOR   3

/******************************************************************************/
typedef struct
{
   int unit;
   int nl, ns, pixsize;
   char format[8];
   char fname[IU_MAX_FNAME_LEN];

   double *buffer;
   unsigned char **valid;
   long long int valid_cnt;
   int curr_line_in_buff;
}VICAR_IMAGE;

/******************************************************************************/
typedef struct
{
   VICAR_IMAGE *vi;

   double **buffer;
   double **tile;
   int buffer_size;
   int last_line_requested;
   int tile_nl, tile_ns;
   int startPos;
}VICAR_TILE_IMAGE;

/******************************************************************************/
typedef struct
{
   VICAR_IMAGE *from;
   VICAR_IMAGE *to;

   /* buffer is assumed to be 4 sequential lines */
   double **buffer;
   int curr_lines_in_buff[4];
   int resample_mode;
}VICAR_RESAMPLE_IMAGE;

/******************************************************************************/
//void getValidMask(VICAR_IMAGE **vi);

/******************************************************************************/
// getVI_inp: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp(int inst);

/******************************************************************************/
// getVI_inp_by_fname: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + fname
//    - filename
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_fname(char *fname, int inst);

/******************************************************************************/
// getVI_inp_by_parmName: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + parmName
//    - parameter name in the pdf file
// + inst
//    - the instance of vicar input file opened
//
// output:
// =======
// + NULL
//    - if inst is less than the number of parameter given
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_parmName(char *parmName, int inst);

/******************************************************************************/
// getVI_out: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + format
//    - format of the output file
// + inst
//    - the instance of vicar input file opened
// 
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out(char *format, int inst, int nl, int ns);

/******************************************************************************/
// getVI_out_by_fname: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + fname
//    - filename
// + type
//    - group name of inst
// + format
//    - format of the output file
// + inst
//    - instance of vicar output file opened
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out_by_fname(char *fname, char *type, char *format, int inst, int nl, int ns);

/******************************************************************************/
// getVI_out_by_parmName: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + parmName
//    - parameter name in the calling pdf file
// + format
//    - format of the output file
// + inst
//    - instance of vicar output file opened
//    - !!! every new parm should begin w/ 1 !!! - corresponds to nth parm
//      in a multidimensional parm
//
// output:
// =======
// + NULL
//    - if inst is less than the number of parameters given
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getVI_out_by_parmName(char *parmName, char *format, int inst, int nl, int ns);

/******************************************************************************/
// (depricated - use getVI function) 
// getImage: returns an initialized VICAR_IMAGE struct
//
// input:
// ======
// + unit
//    - an opened VICAR file handle
//
// output:
// =======
// + vi
//    - initialized VICAR_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_IMAGE* getImage(int unit);

/******************************************************************************/
// deleteImage: deletes a VICAR_IMAGE struct
//
// input:
// ======
// + *vi
//    - pointer to VICAR_IMAGE to delete
/******************************************************************************/
void deleteImage(VICAR_IMAGE **vi);

/******************************************************************************/
// getVRI: return an initialized VICAR_RESAMPLE_IMAGE struct pointer
//
// input:
// ======
// + from
//    - image to resample from
// + to
//    - image to resample to
// + resample_mode
//    - method used to resample
//
// output:
// =======
// + vri
//    - initialized VICAR_RESAMPLE_IMAGE
/******************************************************************************/
VICAR_RESAMPLE_IMAGE* getVRI(VICAR_IMAGE *from, VICAR_IMAGE *to, int resample_mode);

/******************************************************************************/
// getVTI: returns an initialized VICAR_TILE_IMAGE struct
//
// input:
// ======
// + vi
//    - an initialzed VICAR_IMAGE
//    - call getImage to initialize vi before calling the function
//
// output:
// =======
// + vti
//    - initialized VICAR_TILE_IMAGE struct pointer
//    - memory will be allocated inside the function
/******************************************************************************/
VICAR_TILE_IMAGE* getVTI(VICAR_IMAGE *vi, int tile_nl, int tile_ns);

/******************************************************************************/
// readVicarTileImage: reads the tile for given line samp
//
// input:
// ======
// + vti
//    - an initialzed VICAR_TILE_IMAGE
//    - call getVTI before calling this function
//
// output:
// =======
// + vti
//    - buffer inside vti will hold image tile
/******************************************************************************/
void readVicarTileImage(VICAR_TILE_IMAGE *vti, int line, int samp);

/******************************************************************************/
// deleteVTI: deletes the VICAR_RESAMPLE_IMAGE structure
//            (!! DOES NOT DELETE THE VICAR_IMAGE STRUCT !!)
//
// input:
// ======
// + vti
//    - VICAR_TILE_IMAGE struct
/******************************************************************************/
void deleteVTI(VICAR_TILE_IMAGE **vti);

/******************************************************************************/
// getSkippedLine: returns a downsampled line by skipping
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getSkippedLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int to_line);

/******************************************************************************/
// getInterpolatedLine: returns an interpolated line *** NOT YET IMPLEMENTED ***
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getBilinInterpLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int line);

/******************************************************************************/
// getSkippedLine: returns a downsampled line by skipping
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
// + to_line
//    - line at offset 0 specifying the output line
//
// output:
// =======
// + buf
//    - output buffer to put the downsampled line into
/******************************************************************************/
void getDownSampledLine(VICAR_RESAMPLE_IMAGE *vri, double *ds_buf, int line);

/******************************************************************************/
// writeVicarImageLine: writes a VICAR line onto disk
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct (writes the data stored in vi->buffer onto disk)
// + line
//    - line at offset 0 specifying the line to read (offset = 0)
//
/******************************************************************************/
void writeVicarImageLine(VICAR_IMAGE *vi, int line);

/******************************************************************************/
// readVicarResampleImageLine: reads a line from "from" image in vri and stores it
//                             inside the vri buffer
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLED_IMAGE with from and to set
// + buf_index
//    - specifies which of the 4 buffers in the 4 X NS vri buffer to read into
// + line
//    - specifies the line number in the "from" file to read (offset = 0)
//
// output:
// =======
// + vri->buffer[buf_index] stores the line read from "from" image
/******************************************************************************/
void readVicarResampleImageLine(VICAR_RESAMPLE_IMAGE *vri, int buf_index, int line);

/******************************************************************************/
// createDownSampledImage: creates a downsampled image
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLED_IMAGE with from and to set
//
// output:
// =======
// + vri->to image
/******************************************************************************/
void createDownSampledImage(VICAR_RESAMPLE_IMAGE *vri);

/******************************************************************************/
// readVicarImageLine: read a VICAR line and stores it in vi->buffer
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
// + line
//    - line at offset 0 specifying the line to read
//
// output:
// =======
// + vi->buffer
//    - output buffer
/******************************************************************************/
void readVicarImageLine(VICAR_IMAGE *vi, int line);

/******************************************************************************/
// deleteAndCloseImage: closes the VICAR_IMAGE and frees memory
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
/******************************************************************************/
void deleteAndCloseImage(VICAR_IMAGE **vi);

/******************************************************************************/
// deleteVRI: deletes the VICAR_RESAMPLE_IMAGE structure
//            (!! DOES NOT DELETE THE TO AND FROM FILES !!)
//
// input:
// ======
// + vri
//    - VICAR_RESAMPLE_IMAGE struct
/******************************************************************************/
void deleteVRI(VICAR_RESAMPLE_IMAGE **vri);

/******************************************************************************/
// getValidMask: gets valid pixels of images by looking at non-zero pixels
//               (*UNTESTED FUNCTION*)
//
// input:
// ======
// + vi
//    - VICAR_IMAGE struct
//
// output:
// =======
// + vi valid buffer
/******************************************************************************/
void getValidMask(VICAR_IMAGE **vi);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cloud_masks.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CLOUD_MASK
#define CLOUD_MASK

#define CM_CLOUD      1
#define CM_AMBIG      1
#define CM_SNOW       1
#define CM_DESERT     1
#define CM_WARM_CLOUD 1
#define CM_COLD_CLOUD 1
#define CM_ICE        1
#define CM_CIRRUS     1
#define CM_VALID      1

#define CM_NONCLOUD       0
#define CM_NONAMBIG       0
#define CM_NONSNOW        0
#define CM_NONDESERT      0
#define CM_NONWARM_CLOUD  0
#define CM_NONCOLD_CLOUD  0
#define CM_NONICE         0
#define CM_NONCIRRUS      0
#define CM_NONVALID       0

#define CM_GREEN_REF (cm->band_files->green_band->buffer)
#define CM_RED_REF   (cm->band_files->red_band->buffer)
#define CM_NIR_REF   (cm->band_files->nir_band->buffer)
#define CM_SWIR1_REF (cm->band_files->swir1_band->buffer)
#define CM_TIR1_REF  (cm->band_files->tir_band1->buffer)

#define CM_CLOUDMASK     0
#define CM_SNOWMASK      1
#define CM_DESERTMASK    2
#define CM_WARMCLOUDMASK 3
#define CM_COLDCLOUDMASK 4
#define CM_ICEMASK       5
#define CM_CIRRUSMASK    6
#define CM_AMBIGMASK     7
#define CM_VALIDMASK     8

#define CM_FILTER1       10
#define CM_FILTER2       11
#define CM_FILTER3       12
#define CM_FILTER4       13
#define CM_FILTER5       14
#define CM_FILTER6       15
#define CM_FILTER7       16
#define CM_FILTER8       17
#define CM_FILTER9       18
#define CM_FILTER10      19

#define NEIGHBORSIZE 5

#include "ImageUtils.h"

typedef struct
{
   long long int b3_noncloud;
   long long int b6_noncloud;
   long long int snowCnt;
   long long int ndsi_noncloud;
   long long int pass2_revisit;
   long long int b5_noncloud;
   long long int b42ratio_tally;
   long long int b45ratio_tally;
   long long int warmcloud;
   long long int coldcloud;
   long long int scenePixels;
   long long int enterdesert;
   long long int exitdesert;
   long long int snowpresent;
   long long int cloudCnt;

   double filter4Thresh;
   double filter8Thresh;

   int nl, ns;
}ALGORITHM_VARS;

typedef struct
{
   double *ndsi;
   double *bTempComp;
   double *gv;
   double *sv;
   double *rs;
}CM_WORKSPACE;

typedef struct
{
   VICAR_IMAGE *green_band;
   VICAR_IMAGE *red_band;
   VICAR_IMAGE *nir_band;
   VICAR_IMAGE *swir1_band;
   VICAR_IMAGE *tir_band1;
}BAND_FILES;

typedef struct
{
   VICAR_IMAGE *ndsi_file;
   VICAR_IMAGE *bTempComp_file;
   VICAR_IMAGE *gv_file;
   VICAR_IMAGE *sv_file;
   VICAR_IMAGE *rs_file;
}WORKSPACE_FILES;

typedef struct
{
   ALGORITHM_VARS *vars;
   CM_WORKSPACE *ws;
   WORKSPACE_FILES *ws_files;
   BAND_FILES *band_files;

   unsigned char **CMcloud;
   unsigned char **CMsnow;
   unsigned char **CMdesert;
   unsigned char **CMcloud_warm;
   unsigned char **CMcloud_cold;
   unsigned char **ice;
   unsigned char **filter_cirrus;
   unsigned char **ambig;
   unsigned char **valid;

   unsigned char **filter1;
   unsigned char **filter2;
   unsigned char **filter3;
   unsigned char **filter4;
   unsigned char **filter5;
   unsigned char **filter6;
   unsigned char **filter7;
   unsigned char **filter8;
   unsigned char **filter9;
   unsigned char **filter10;

   unsigned char **tambig_warm_mask;
   unsigned char **tambig_cold_mask;

}CLOUD_MASKS;

/******************************************************************************/
// get_Cloud_Masks: creates a CLOUD_MASKS and initializes the buffers inside them
//
// input:
// ======
// + nl
//    - number of lines
// + ns
//    - number of samples
//
// output:
// =======
// + masks
//    - CLOUD_MASKS pointer
//
/******************************************************************************/
CLOUD_MASKS* get_CLOUD_MASKS(int nl, int ns);

/******************************************************************************/
// delete_Cloud_Masks: deletes a CLOUD_MASKS
// 
// input:
// ======
// + masks
//    - CLOUD_MASKS
/******************************************************************************/
void delete_CLOUD_MASKS(CLOUD_MASKS **masks);

/******************************************************************************/
// init_CM_WORKSPACE: creates a CLOUD_MASK workspace
//
// input:
// ======
// + nl
//    - number of lines
// + ns
//    - number of samples
//
// output:
// =======
// + work
//    - CM_WORKSPACE pointer
//
/******************************************************************************/
void init_CM_WORKSPACE(CLOUD_MASKS **masks);

/******************************************************************************/
// delete_CM_WORKSPACE: deletes a CLOUD_MASKS
// 
// input:
// ======
// + work
//    - CM_WORKSPACE
/******************************************************************************/
void delete_CM_WORKSPACE(CLOUD_MASKS **masks);

/******************************************************************************/
// filter1: brightness threshold
//
// inputs:
// =======
// + red_ref (0.6-0.7 micron) reflectance
//    - ETM+ (BAND 3)
//    - ASTER (BAND 2)
// + ns
//    - number of samples
// + valid
//    - valid pixels of the scene
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambig mask
/******************************************************************************/
void filter1(double *red_ref, unsigned char *finalmask, unsigned char* ambig,
             unsigned char *valid, int ns);

/******************************************************************************/
// filter2: snow threshold
//
// inputs:
// =======
// + ndsi
//    - normalized difference snow index
//    - see getNDSI
// + near_ref (0.7-0.9 micron) reflectance
//    - ETM+ (BAND 4)
//    - ASTER (BAND 3)
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + snow
//    - flag to represent if snow is in the image
/******************************************************************************/
void filter2(double* ndsi, double *near_ref, unsigned char *finalmask,
             unsigned char *snow, int ns);

/******************************************************************************/
// filter3: temperature threshold
//
// inputs:
// =======
// + b_temp
//    - brightness temperature (! NOT AT SATELLITE TEMPERATURE !)
//    - ETM+ (BAND 6)
//    - Aster (BAND 13)
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
/******************************************************************************/
void filter3(double *b_temp, unsigned char *finalmask, int ns);

/******************************************************************************/
// filter4: snow and tundra threshold
//
// inputs:
// =======
// + bTempComp
//    - see getBTempComp
// + shortwave_ref (0.7-0.9 micron) reflectance
//    - ETM+ (BAND 5)
//    - ASTER (BAND 4)
// + b_temp
//    - brightness temperature (! NOT AT SATELLITE TEMPERATURE !)
//    - see getBTemp
// + ns
//    - number of samples
// + thresh
//    - threshold for brightness comp filter
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
// + snow
//    - flag to represent if snow is in the image
// + ice
//    - ice mask
/******************************************************************************/
void filter4(double *bTempComp, double *shortwave_ref, double *b_temp, unsigned char *finalmask,
             unsigned char *ambig, unsigned char *ice, int ns, double thresh);

/******************************************************************************/
// filter5: growing vegetation
//
// inputs:
// =======
// + gv
//    - see getGV
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
/******************************************************************************/
void filter5(double *gv, unsigned char *finalmask, unsigned char *ambig, int ns);

/******************************************************************************/
// filter6: senescing vegetation
//
// inputs:
// =======
// + sv
//    - see getSV
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + ambig
//    - ambiguous mask
// + enterdesert
//    - count of pixels entering "desert"
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter6(double *sv, unsigned char *finalmask, unsigned char *ambig,
             long long int *enterdesert, int ns);

/******************************************************************************/
// filter7: reflective rocks and soil
//
// inputs:
// =======
// + rs
//    - see getRS
// + ns
//    - number of samples
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + CMdesert
//    - desert mask
// + exitdesert
//    - count of desert pixels exiting "desert"
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter7(double *rs, unsigned char *finalmask, unsigned char *CMdesert,
             unsigned char *ambig, long long int *exitdesert, int ns);

/******************************************************************************/
// filter8: identifying warm and cold clouds
//
// inputs:
// =======
// + bTempComp
//    - see getBTempComp
// + ns
//    - number of samples
// + thresh
//    - threshold for this filter
//
// outputs:
// ========
// + finalmask
//    - cloud mask
// + CMcloud_cold
//    - cold cloud mask
// + CMcloud_warm
//    - warm cloud mask
// + enterdesert
//    - count of desert pixels
//    - cumulative count - does not start at 0 but at the value passed in
/******************************************************************************/
void filter8(double *bTempComp, unsigned char *finalmask, unsigned char *CMcloud_cold,
             unsigned char *CMcloud_warm, int ns, double thresh,
             long long int *coldcloud, long long int *warmcloud);

/******************************************************************************/
// getNDSI: gets the normalized difference snow index
//
// inputs:
// =======
// + green_ref reflectance
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + ndsi
//    - buffer containing ndsi data
/******************************************************************************/
void getNDSI(double *green_ref, double *shortwave_ref, double *ndsi, int ns);

/******************************************************************************/
// getBTempComp: gets the brightness temperature
//
// inputs:
// =======
// + b_temp
//    - brightness temperature
//    - ETM+ (BAND 6)
//    - Aster (BAND 13)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + bTempComp
//    - buffer containing bTempComp
/******************************************************************************/
void getBTempComp(double *b_temp, double *shortwave_ref, double *bTempComp, int ns);

/******************************************************************************/
// getGV: gets the growing vegetation indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + red_ref reflectance
//    - ETM+ (BAND 3)
//    - Aster (BAND 2)
// + ns
//    - number of samples
//
// outputs:
// ========
// + gv
//    - buffer containing growing vegetation data
/******************************************************************************/
void getGV(double *near_ref, double *red_ref, double *gv, int ns);

/******************************************************************************/
// getSV: gets the senescing vegetation indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + green_ref reflectance
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + ns
//    - number of samples
//
// outputs:
// ========
// + sv
//    - buffer containing senescing vegetation data
/******************************************************************************/
void getSV(double *near_ref, double *green_ref, double *sv, int ns);

/******************************************************************************/
// getRS: gets the reflective soil indicator
//
// inputs:
// =======
// + near_ref reflectance
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + shortwave_ref reflectance
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + ns
//    - number of samples
//
// outputs:
// ========
// + rs
//    - buffer containing reflective soil data
/******************************************************************************/
void getRS(double *near_ref, double *shortwave_ref, double *rs, int ns);

/******************************************************************************/
// setBandImages: assign images to corresponding bands
//
// inputs:
// =======
// + cm
//    - CLOUD_MASKS struct to assign
// + green
//    - ETM+ (BAND 2)
//    - Aster (BAND 1)
// + red
//    - ETM+ (BAND 3)
//    - Aster (BAND 2)
// + nir
//    - ETM+ (BAND 4)
//    - Aster (BAND 3)
// + swir1
//    - ETM+ (BAND 5)
//    - Aster (BAND 4)
// + tir1
//    - ETM+ (BAND 6)
//    - Aster (BAND 5)
//
// outputs:
// ========
// + cm
//    - BAND_FILES struct in cm is initialized
/******************************************************************************/
void setBandImages(CLOUD_MASKS **cm, VICAR_IMAGE *green, VICAR_IMAGE *red, VICAR_IMAGE *nir,
                   VICAR_IMAGE *swir1, VICAR_IMAGE *tir1);

/******************************************************************************/
// setWorkspaceImages: assign images to corresponding workspace
//
// inputs:
// =======
// + cm
//    - CLOUD_MASKS struct to assign
// + ndsi
//    - Normalized Difference Snow Index image
// + bTempComp
//    - bTempComp image
// + gv
//    - growing vegetation image
// + sv
//    - scenesing vegetation image
// + rs
//    - reflective soil image
//
// outputs:
// ========
// + cm
//    - WORKSPACE_FILES struct in cm is initialized
/******************************************************************************/
void setWorkspaceImages(CLOUD_MASKS **cm, VICAR_IMAGE *ndsi, VICAR_IMAGE *bTempComp,
                        VICAR_IMAGE *gv, VICAR_IMAGE *sv, VICAR_IMAGE *rs);

/******************************************************************************/
// doPass1: perform all the filters of pass1 (setBandImages and setWorkspaceImages
//                                            should be called before performing
//                                            this function)
//
// inputs:
// =======
// + masks
//    - CLOUD_MASKS stuct to perform the pass1 on
//
// outputs:
// ========
// + masks
//    - filter buffers will be set
/******************************************************************************/
void doPass1(CLOUD_MASKS *masks);

/******************************************************************************/
// doPass2: perform all the filters of pass2 (pass1 should be called before performing
//                                            this function)
//
// inputs:
// =======
// + masks
//    - CLOUD_MASKS stuct to perform the pass1 on
//
// outputs:
// ========
// + masks
//    - CMcloud buffers will be set
/******************************************************************************/
void doPass2(CLOUD_MASKS *cm);

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLoggerUtils.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _loggerutils_h_
#define _loggerutils_h_

#include <stdlib.h>
#include "ibisfile.h"
#include "zvproto.h"

#define _loggerutils_version_ 22

void checkLoggerUtilsVersion (int minimum);

/* Version Changes:
   22 Enhanced labelRaw16bitImage to pad short images
   21 Changed labelRaw16bitImage to complain and return when fread fails
   20 Added logMetaDoubleProperty
   19 Switched IBIS organization from row to column
   18 Added noQuotes parm to logMetaStringToProperty
   17 Allowed metadata to log to arbitrary label
   16 Added raw image labeler
   15 Enhanced usingLatLonSubArea to allow specification of only one lat/lon limit
   14 Added shortSwapBytes to convert shorts from 386 architecture
   13 Added echoMeta flag to logMeta* to echo to stdout via zifmessage
   12 Added more HDF declarations and azimuth and zenith columns to IBIS file
      Added checkedMalloc function
   11 Added HDF declarations
   10 Added daysInMonth (from sunup.c)
      Added dateToDayOfYear (from sunup.c)
      Added dayOfYearToDate (from sunup.c)
    9 Added null path option to logMeta* to inhibit logging to file
    8 Added VICAR image labeling with meta data interface; replaced getTimesData with mallocAndRead
    7 Added zvread to missing VICAR declarations
    6 Removed metaFileName from getTimesData args; replaced logDataSetTime with getDataSetTime
    5 Added geocentricToGeodetic
    4 Added logDayTimeFlag, getTimesData, logDataSetTime (factored out of avhrrlog and avhrrllog)
    3 Added forceSubAreaSanity
    2 Added meta data interface and lat/long sub-area parameters
      Added lat/lon specified sub-area
    1 Initial version Mon Dec  3 2001
  */

short shortSwapBytes (short s); /* to convert shorts from 386 architecture */

/* calls stdlib malloc, calls abend on null returned pointer; description used for error message */
void * checkedMalloc (size_t size, char * description);

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

#define BOUNDED(val, min, max) (MIN ((max), MAX ((min), (val))))

char * mallocAndRead (char * path);
double geocentricToGeodetic (double latitude);
void getDataSetTime (char * dataSetName, char * times, int * utcTimeInMinutes, char * date, char * time);

/* Ensures sub area make sense for image shape.
   sl, ss are forced to be within the image
   if nl or ns are < 1 > possible, they are forced to include all image available
 */
void forceSubAreaSanity (int * sl, int * ss, int * nl, int * ns, int lines, int samples);

/* Allows lat/lon specified sub-area
   Assumes that sl, ss, nl, ns default to -999
   Assumes that minLat, maxLat, minLon, maxLon default to -999.0
   Assumes that exactly one of the following is true:
      1 All line/sample values and no lat/lon values are specified; returns 0
      2 All lat/lon values and no line/sample values are specified; returns 1
      3 No line/sample values or lat/lon values are specified; returns 0
 */
int usingLatLonSubArea (int imageLines, int imageSamples,
		       double * minLat, double * maxLat, double * minLon, double * maxLon,
		       int * sl, int * ss, int * nl, int * ns, int allow);

/* Meta data interface
   initMetaData must be called before meta data is logged. The path names the
   meta data file. unitCount indicates the number of vunits for the data to be 
   added to the vicar label as Property "loggerMetaData", Key name, Value value.
   MetaToLabel stuffs the whole contents of the named meta file into a label named
   ALL_META_DATA.
*/

void initMetaData (char * path);
void logMetaString (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits);
void logMetaStringToProperty (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits, char * property, int noQuotes);
void logMetaInt (int echoMeta, char * path, char * name, int value, int unitCount, int * vunits);
void logMetaDouble (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits);
void logMetaDoubleToProperty (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits, char * property);

void metaToLabel (char * metaName, int vunit);
void addGTKey (int vunit, char * key, char * value);

/* IBIS navigation data support */
int logNavDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
		      int numRows,
		      double * lineColumn, double * sampleColumn,
		      double * latColumn, double * lonColumn);
int logNavAndAnglesDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
			       int numRows,
			       double * lineColumn, double * sampleColumn,
			       double * latColumn, double * lonColumn,
			       double * zenithColumn, double * azimuthColumn);

/* returns vunit on open image; must be zvclosed */
int labelRaw16bitImage (char * inpath, char * outpath, int nl, int ns);

/* Time functions */
int daysInMonth (int year, int month);

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
int dateToDayOfYear (int year, int month, int day);
void dayOfYearToDate (int dayOfYear, int year, int * month, int * day);

/* Misc macros */
#ifndef MIN
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))
#endif

#define BETWEEN(left, middle, right) ((left) <= (middle) && (middle) <= (right))

#endif

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create hdfIncludes.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef _hdfIncludes_h_
#define _hdfIncludes_h_

/* HDF and HDF-EOS includes */
#include "hdf.h"
/* conflicts with taeconf.inp */
#undef FAIL

#include "hdfi.h"
#include "mfhdf.h"
#include "hntdefs.h"
#include "HdfEosDef.h"

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
