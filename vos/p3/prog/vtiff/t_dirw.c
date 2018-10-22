#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_dir.c,v 1.110 92/03/06 11:59:49 sam Exp $";
#endif

/*
 * Copyright (c) 1988, 1989, 1990, 1991, 1992 Sam Leffler
 * Copyright (c) 1991, 1992 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * TIFF Library.
 *
 * Directory Tag Get & Set Routines.
 * (and also some miscellaneous stuff)
 *
 * NB: Beware of the varargs declarations for routines in
 *     this file.  The names and types of variables has been
 *     carefully chosen to make things work with compilers that
 *     are busted in one way or another (e.g. SGI/MIPS).
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <string.h>

#if USE_PROTOTYPES
extern	int TIFFSetCompressionScheme(TIFF *, int);
extern	int TIFFDefaultDirectory(TIFF*);
extern  int TIFFFreeDirectory(TIFF*);
#else
extern	int TIFFSetCompressionScheme();
extern	int TIFFDefaultDirectory();
extern	int TIFFFreeDirectory();
#endif

/** from dir_writ **/

#if HAVE_IEEEFP
#define	TIFFCvtNativeToIEEEFloat(tif, n, fp)
#endif

#if USE_PROTOTYPES
static	TIFFWriteNormalTag(TIFF*, TIFFDirEntry*, TIFFFieldInfo*);
static	TIFFSetupShortLong(TIFF *, u_short, TIFFDirEntry *, u_long);
static	TIFFSetupShortPair(TIFF *, u_short, TIFFDirEntry *);
static	TIFFWriteRational(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, float);
static	TIFFWritePerSampleShorts(TIFF *, u_short, TIFFDirEntry *);
static	TIFFWriteShortTable(TIFF *, u_short, TIFFDirEntry *, int, u_short **);
static	TIFFWriteShortArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, u_short *);
static	TIFFWriteLongArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, u_long *);
static	TIFFWriteRationalArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, float *);
static	TIFFWriteFloatArray(TIFF *,
	    TIFFDataType, u_short, TIFFDirEntry *, int, float *);
static	TIFFWriteString(TIFF *, u_short, TIFFDirEntry *, char *);
static	TIFFFloatToRational(float,u_long *);
static	CARTLinkDirectory(register TIFF *);
static	TIFFgetfraction(dblparam_t, u_long *, u_long *, dblparam_t,int);
extern  int TIFFFlushData1(TIFF*);
extern  int CARTWriteDirectory(TIFF*);
extern  int	TIFFFreeDirectory(TIFF*);
extern  int	TIFFDefaultDirectory(TIFF*);
#ifdef JPEG_SUPPORT
static	TIFFWriteJPEGQTables(TIFF *, TIFFDirEntry *);
static	TIFFWriteJPEGCTables(TIFF *, u_short, TIFFDirEntry *, u_char **);
#endif
#ifdef COLORIMETRY_SUPPORT
static	TIFFWriteTransferFunction(TIFF*, TIFFDirEntry*);
#endif
static	TIFFWriteData(TIFF *, TIFFDirEntry *, char *);
static	TIFFLinkDirectory(TIFF *);
static  TIFFWriteTIFFDir(int fd, TIFFDirEntry* dir, int n);
#else
static	TIFFWriteNormalTag();
static	TIFFSetupShortLong();
static	TIFFSetupShortPair();
static	TIFFWriteRational();
static	TIFFWritePerSampleShorts();
static	TIFFWriteShortTable();
static	TIFFWriteShortArray();
static	TIFFWriteLongArray();
static	TIFFWriteRationalArray();
static	TIFFWriteFloatArray();
static	TIFFWriteString();
static	TIFFFloatToRational();
static	CARTLinkDirectory();
static	TIFFgetfraction();
extern  int TIFFFlushData1();
extern  int CARTWriteDirectory();
extern  int	TIFFFreeDirectory();
extern  int	TIFFDefaultDirectory();
#ifdef JPEG_SUPPORT
static	TIFFWriteJPEGQTables();
static	TIFFWriteJPEGCTables();
#endif
static	TIFFWriteData();
static	TIFFLinkDirectory();
static  TIFFWriteTIFFDir();
#endif

#define	WriteRationalPair(type, tag1, v1, tag2, v2) {		\
	if (!TIFFWriteRational(tif, type, tag1, dir, v1))	\
		goto bad;					\
	if (!TIFFWriteRational(tif, type, tag2, dir+1, v2))	\
		goto bad;					\
	dir++;							\
}

/** end dir_writ **/


static
DECLARE2(setString, char**, cpp, char*, cp)
{
	if (*cpp)
		free(*cpp), *cpp = 0;
	if (cp) {
		int len = strlen(cp)+1;
		if (*cpp = malloc(len))
			mybcopy(cp, *cpp, len);
	}
}

static
DECLARE3(setShortArray, u_short**, wpp, u_short*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (u_short);
	if (wp && (*wpp = (u_short *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

static
DECLARE3(setLongArray, u_long**, wpp, u_long*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (u_long);
	if (wp && (*wpp = (u_long *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

static
DECLARE3(setFloatArray, float**, wpp, float*, wp, long, n)
{
	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	n *= sizeof (float);
	if (wp && (*wpp = (float *)malloc(n)))
		mybcopy(wp, *wpp, n);
}

#ifdef JPEG_SUPPORT
/*
 * Install a JPEG Quantization table.
 * Note that we reorder the elements
 * of the array in the zig-zag order
 * that is expected by the compression code
 * and that is to be stored in the file.
 */
static
DECLARE3(setJPEGQTable, u_char***, wpp, u_char**, wp, int, nc)
{
	static u_char zigzag[64] = {
	    0,  1,  5,  6, 14, 15, 27, 28,
	    2,  4,  7, 13, 16, 26, 29, 42,
	    3,  8, 12, 17, 25, 30, 41, 43,
	    9, 11, 18, 24, 31, 40, 44, 53,
	   10, 19, 23, 32, 39, 45, 52, 54,
	   20, 22, 33, 38, 46, 51, 55, 60,
	   21, 34, 37, 47, 50, 56, 59, 61,
	   35, 36, 48, 49, 57, 58, 62, 63
	};
	char *tab;
	int i, j;

	if (*wpp)
		free((char *)*wpp), *wpp = 0;
	*wpp = (u_char **)
	    malloc(nc * (sizeof (u_char *) + 64*sizeof (u_char)));
	tab = (((char *)*wpp) + nc*sizeof (u_short *));
	for (i = 0; i < nc; i++) {
		(*wpp)[i] = (u_char *)tab;
		for (j = 0; j < 64; j++)
			tab[zigzag[j]] = wp[i][j];
		tab += 64*sizeof (u_char);
	}
}

/*
 * Install a JPEG Coefficient table.
 */
static
DECLARE3(setJPEGCTable, u_char***, cpp, u_char**, cp, int, nc)
{
	u_char *tab;
	int i, j, nw;

	if (*cpp)
		free(*cpp), *cpp = 0;
	/*
	 * Calculate the size of the table by counting
	 * the number of codes specified in the bits array.
	 */
	nw = 0;
	for (i = 0; i < nc; i++) {
		nw += 16;		/* 16 bytes for bits array */
		for (j = 0; j < 16; j++)/* sum up count of codes */
			nw += cp[i][j];
	}
	*cpp = (u_char **)malloc(nc*sizeof (u_char *) + nw);
	tab = ((u_char *)*cpp) + nc*sizeof (u_char *);
	/*
	 * Setup internal array and copy user data.
	 */
	for (i = 0; i < nc; i++) {
		(*cpp)[i] = tab;
		for (nw = 16, j = 0; j < 16; j++)
			nw += cp[i][j];
		mybcopy(cp[i], tab, nw);
		tab += nw;
	}
}
#endif

#ifdef CARTOGRAPHIC_SUPPORT
static
CARTSetField(tif, tag, ap)
TIFF *tif;
int tag;
va_list ap;
{
	TIFFDirectory *td = &tif->tif_dir;

	switch (tag) {
		case TIFFTAG_CARTO_IFD_OFFSET:
			td->td_carto_ifd_offset = va_arg(ap, u_long);
			break;
		case CARTTAG_PROJECTIONTYPE:
			td->td_projectiontype = va_arg(ap, int);
			break;
		case CARTTAG_PROJ_XPOS:
			td->td_proj_xpos = va_arg(ap, u_long);
			break;
		case CARTTAG_PROJ_YPOS:
			td->td_proj_ypos = va_arg(ap, u_long);
			break;
		case CARTTAG_LATITUDE:
			td->td_latitude = va_arg(ap, u_long);
			break;
		case CARTTAG_LONGITUDE:
			td->td_longitude = va_arg(ap, u_long);
			break;
		case CARTTAG_XPIXPERANGLE:
			td->td_xpixperangle = va_arg(ap, dblparam_t);
			break;
		case CARTTAG_YPIXPERANGLE:
			td->td_ypixperangle = va_arg(ap, dblparam_t);
			break;
		default:
			return(0);
	}

	tif->tif_flags |= TIFF_DIRTYCART;
	return (1);
}
#endif /* CARTO */

static
TIFFSetField1(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	TIFFDirectory *td = &tif->tif_dir;
	int i, status = 1;
	long v;

	switch (tag) {
	case TIFFTAG_SUBFILETYPE:
		td->td_subfiletype = va_arg(ap, u_long);
		break;
	case TIFFTAG_IMAGEWIDTH:
		td->td_imagewidth = va_arg(ap, u_long);
		break;
	case TIFFTAG_IMAGELENGTH:
		td->td_imagelength = va_arg(ap, u_long);
		break;
	case TIFFTAG_BITSPERSAMPLE:
		td->td_bitspersample = va_arg(ap, int);
		break;
	case TIFFTAG_COMPRESSION:
		v = va_arg(ap, int) & 0xffff;
		/*
		 * If we're changing the compression scheme,
		 * the notify the previous module so that it
		 * can cleanup any state it's setup.
		 */
		if (TIFFFieldSet(tif, FIELD_COMPRESSION)) {
			if (td->td_compression == v)
				break;
			if (tif->tif_cleanup)
				(*tif->tif_cleanup)(tif);
		}
		/*
		 * Setup new compression routine state.
		 */
		if (status = TIFFSetCompressionScheme(tif, v))
			td->td_compression = v;
		break;
	case TIFFTAG_PHOTOMETRIC:
		td->td_photometric = va_arg(ap, int);
		break;
	case TIFFTAG_THRESHHOLDING:
		td->td_threshholding = va_arg(ap, int);
		break;
	case TIFFTAG_FILLORDER:
		v = va_arg(ap, int);
		if (v != FILLORDER_LSB2MSB && v != FILLORDER_MSB2LSB)
			goto badvalue;
		td->td_fillorder = v;
		break;
	case TIFFTAG_DOCUMENTNAME:
		setString(&td->td_documentname, va_arg(ap, char *));
		break;
	case TIFFTAG_ARTIST:
		setString(&td->td_artist, va_arg(ap, char *));
		break;
	case TIFFTAG_DATETIME:
		setString(&td->td_datetime, va_arg(ap, char *));
		break;
	case TIFFTAG_HOSTCOMPUTER:
		setString(&td->td_hostcomputer, va_arg(ap, char *));
		break;
	case TIFFTAG_IMAGEDESCRIPTION:
		setString(&td->td_imagedescription, va_arg(ap, char *));
		break;
	case TIFFTAG_MAKE:
		setString(&td->td_make, va_arg(ap, char *));
		break;
	case TIFFTAG_MODEL:
		setString(&td->td_model, va_arg(ap, char *));
		break;
	case TIFFTAG_SOFTWARE:
		setString(&td->td_software, va_arg(ap, char *));
		break;
	case TIFFTAG_ORIENTATION:
		v = va_arg(ap, int);
		if (v < ORIENTATION_TOPLEFT || ORIENTATION_LEFTBOT < v) {
			TIFFWarning(tif->tif_name,
			    "Bad value %ld for \"%s\" tag ignored",
			    v, TIFFFieldWithTag(tag)->field_name);
		} else
			td->td_orientation = v;
		break;
	case TIFFTAG_SAMPLESPERPIXEL:
		/* XXX should cross check -- e.g. if pallette, then 1 */
		v = va_arg(ap, int);
		if (v == 0)
			goto badvalue;
		if (v > 4) {
			TIFFError(tif->tif_name,
			    "Cannot handle %ld-channel data", v); 
			status = 0;
		} else
			td->td_samplesperpixel = v;
		break;
	case TIFFTAG_ROWSPERSTRIP:
		v = va_arg(ap, u_long);
		if (v == 0)
			goto badvalue;
		td->td_rowsperstrip = v;
		if (!TIFFFieldSet(tif, FIELD_TILEDIMENSIONS)) {
			td->td_tilelength = v;
			td->td_tilewidth = td->td_imagewidth;
		}
		break;
	case TIFFTAG_MINSAMPLEVALUE:
		td->td_minsamplevalue = va_arg(ap, int) & 0xffff;
		break;
	case TIFFTAG_MAXSAMPLEVALUE:
		td->td_maxsamplevalue = va_arg(ap, int) & 0xffff;
		break;
	case TIFFTAG_XRESOLUTION:
		td->td_xresolution = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_YRESOLUTION:
		td->td_yresolution = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_PLANARCONFIG:
		v = va_arg(ap, int);
		if (v != PLANARCONFIG_CONTIG && v != PLANARCONFIG_SEPARATE)
			goto badvalue;
		td->td_planarconfig = v;
		break;
	case TIFFTAG_PAGENAME:
		setString(&td->td_pagename, va_arg(ap, char *));
		break;
	case TIFFTAG_XPOSITION:
		td->td_xposition = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_YPOSITION:
		td->td_yposition = va_arg(ap, dblparam_t);
		break;
	case TIFFTAG_GROUP3OPTIONS:
		td->td_group3options = va_arg(ap, u_long);
		break;
	case TIFFTAG_GROUP4OPTIONS:
		td->td_group4options = va_arg(ap, u_long);
		break;
	case TIFFTAG_RESOLUTIONUNIT:
		v = va_arg(ap, int);
		if (v < RESUNIT_NONE || RESUNIT_CENTIMETER < v)
			goto badvalue;
		td->td_resolutionunit = v;
		break;
	case TIFFTAG_PAGENUMBER:
		td->td_pagenumber[0] = va_arg(ap, int);
		td->td_pagenumber[1] = va_arg(ap, int);
		break;
	case TIFFTAG_HALFTONEHINTS:
		td->td_halftonehints[0] = va_arg(ap, int);
		td->td_halftonehints[1] = va_arg(ap, int);
		break;
	case TIFFTAG_COLORMAP:
		v = 1L<<td->td_bitspersample;
		setShortArray(&td->td_colormap[0], va_arg(ap, u_short *), v);
		setShortArray(&td->td_colormap[1], va_arg(ap, u_short *), v);
		setShortArray(&td->td_colormap[2], va_arg(ap, u_short *), v);
		break;
	case TIFFTAG_PREDICTOR:
		td->td_predictor = va_arg(ap, int);
		break;
	case TIFFTAG_EXTRASAMPLES:
		v = va_arg(ap, int);
		if (v > td->td_samplesperpixel)
			goto badvalue;
		if (v != 1)			/* XXX */
			goto badvalue;		/* XXX */
		v = va_arg(ap, int);
		if (v != EXTRASAMPLE_ASSOCALPHA)/* XXX */
			goto badvalue;		/* XXX */
		td->td_matteing = 1;
		break;
	case TIFFTAG_MATTEING:
		td->td_matteing = va_arg(ap, int);
		break;
	case TIFFTAG_BADFAXLINES:
		td->td_badfaxlines = va_arg(ap, u_long);
		break;
	case TIFFTAG_CLEANFAXDATA:
		td->td_cleanfaxdata = va_arg(ap, int);
		break;
	case TIFFTAG_CONSECUTIVEBADFAXLINES:
		td->td_badfaxrun = va_arg(ap, u_long);
		break;
	case TIFFTAG_TILEWIDTH:
		v = va_arg(ap, u_long);
		if (v % 8)
			goto badvalue;
		td->td_tilewidth = v;
		tif->tif_flags |= TIFF_ISTILED;
		break;
	case TIFFTAG_TILELENGTH:
		v = va_arg(ap, u_long);
		if (v % 8)
			goto badvalue;
		td->td_tilelength = v;
		tif->tif_flags |= TIFF_ISTILED;
		break;
	case TIFFTAG_TILEDEPTH:
		v = va_arg(ap, u_long);
		if (v == 0)
			goto badvalue;
		td->td_tiledepth = v;
		break;
	case TIFFTAG_DATATYPE:
	case TIFFTAG_SAMPLEFORMAT:
		v = va_arg(ap, int);
		if (tag == TIFFTAG_DATATYPE && v == 0)
			v = SAMPLEFORMAT_VOID;
		if (v < SAMPLEFORMAT_INT || SAMPLEFORMAT_VOID < v)
			goto badvalue;
		td->td_sampleformat = v;
		break;
	case TIFFTAG_IMAGEDEPTH:
		td->td_imagedepth = va_arg(ap, u_long);
		break;
#ifdef YCBCR_SUPPORT
	case TIFFTAG_YCBCRCOEFFICIENTS:
		setFloatArray(&td->td_ycbcrcoeffs, va_arg(ap, float *), 3);
		break;
	case TIFFTAG_YCBCRPOSITIONING:
		td->td_ycbcrpositioning = va_arg(ap, int);
		break;
	case TIFFTAG_YCBCRSUBSAMPLING:
		td->td_ycbcrsubsampling[0] = va_arg(ap, int);
		td->td_ycbcrsubsampling[1] = va_arg(ap, int);
		break;
#endif
#ifdef JPEG_SUPPORT
	case TIFFTAG_JPEGPROC:
		td->td_jpegproc = va_arg(ap, int);
		break;
	case TIFFTAG_JPEGRESTARTINTERVAL:
		td->td_jpegrestartinterval = va_arg(ap, int);
		break;
	case TIFFTAG_JPEGQTABLES:
		setJPEGQTable(&td->td_qtab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
	case TIFFTAG_JPEGDCTABLES:
		setJPEGCTable(&td->td_dctab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
	case TIFFTAG_JPEGACTABLES:
		setJPEGCTable(&td->td_actab, va_arg(ap, u_char **),
		    td->td_samplesperpixel);
		break;
#endif
#ifdef COLORIMETRY_SUPPORT
	case TIFFTAG_WHITEPOINT:
		setFloatArray(&td->td_whitepoint, va_arg(ap, float *), 2);
		break;
	case TIFFTAG_PRIMARYCHROMATICITIES:
		setFloatArray(&td->td_primarychromas, va_arg(ap, float *), 6);
		break;
	case TIFFTAG_TRANSFERFUNCTION:
		for (i = 0; i < td->td_samplesperpixel; i++)
		    setShortArray(&td->td_transferfunction[i],
			va_arg(ap, u_short *), 1L<<td->td_bitspersample);
		break;
	case TIFFTAG_REFERENCEBLACKWHITE:
		/* XXX should check for null range */
		v = 2 * td->td_samplesperpixel;
		setFloatArray(&td->td_refblackwhite, va_arg(ap, float *), v);
		break;
#endif
#ifdef CMYK_SUPPORT
	case TIFFTAG_INKSET:
		td->td_inkset = va_arg(ap, int);
		break;
	case TIFFTAG_DOTRANGE:
		/* XXX should check for null range */
		td->td_dotrange[0] = va_arg(ap, int);
		td->td_dotrange[1] = va_arg(ap, int);
		break;
	case TIFFTAG_INKNAMES:
		setString(&td->td_inknames, va_arg(ap, char *));
		break;
	case TIFFTAG_TARGETPRINTER:
		setString(&td->td_targetprinter, va_arg(ap, char *));
		break;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	case TIFFTAG_CARTO_IFD_OFFSET:
	case CARTTAG_PROJECTIONTYPE:
	case CARTTAG_PROJ_XPOS:
	case CARTTAG_PROJ_YPOS:
	case CARTTAG_LATITUDE:
	case CARTTAG_LONGITUDE:
	case CARTTAG_XPIXPERANGLE:
	case CARTTAG_YPIXPERANGLE:
		CARTSetField(tif,tag,ap);
		break;
#endif /* CARTO */
	default:
		TIFFError(tif->tif_name,
		    "Internal error, tag value botch, tag \"%s\"",
		    TIFFFieldWithTag(tag)->field_name);
		status = 0;
		break;
	}
	if (status) {
		TIFFSetFieldBit(tif, TIFFFieldWithTag(tag)->field_bit);
		tif->tif_flags |= TIFF_DIRTYDIRECT;
	}
	va_end(ap);
	return (status);
badvalue:
	TIFFError(tif->tif_name, "%ld: Bad value for \"%s\"", v,
	    TIFFFieldWithTag(tag)->field_name);
	va_end(ap);
	return (0);
}

/*
 * Return 1/0 according to whether or not
 * it is permissible to set the tag's value.
 * Note that we allow ImageLength to be changed
 * so that we can append and extend to images.
 * Any other tag may not be altered once writing
 * has commenced, unless its value has no effect
 * on the format of the data that is written.
 */
static
OkToChangeTag(tif, tag)
	TIFF *tif;
	int tag;
{
	if (tag != TIFFTAG_IMAGELENGTH &&
	    (tif->tif_flags & TIFF_BEENWRITING)) {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		/*
		 * Consult info table to see if tag can be changed
		 * after we've started writing.  We only allow changes
		 * to those tags that don't/shouldn't affect the
		 * compression and/or format of the data.
		 */
		if (fip && !fip->field_oktochange)
			return (0);
	}
	return (1);
}

/*
 * Record the value of a field in the
 * internal directory structure.  The
 * field will be written to the file
 * when/if the directory structure is
 * updated.
 */
/*VARARGS2*/
DECLARE2V(TIFFSetField, TIFF*, tif, int, tag)
{
	int status = 0;

	if (OkToChangeTag(tif, tag)) {
		va_list ap;

		VA_START(ap, tag);
		status = TIFFSetField1(tif, tag, ap);
		va_end(ap);
	} else {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		if (fip)
			TIFFError("TIFFSetField",
			    "%s: Cannot modify tag \"%s\" while writing",
			    tif->tif_name, fip->field_name);
	}
	return (status);
}

/*
 * Like TIFFSetField, but taking a varargs
 * parameter list.  This routine is useful
 * for building higher-level interfaces on
 * top of the library.
 */
TIFFVSetField(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	int status = 0;

	if (!OkToChangeTag(tif, tag)) {
		TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);
		if (fip)
			TIFFError("TIFFVSetField",
			    "%s: Cannot modify tag \"%s\" while writing",
			    tif->tif_name, fip->field_name);
	} else
		status = TIFFSetField1(tif, tag, ap);
	return (status);
}

static
TIFFGetField1(td, tag, ap)
	TIFFDirectory *td;
	int tag;
	va_list ap;
{

	switch (tag) {
	case TIFFTAG_SUBFILETYPE:
		*va_arg(ap, u_long *) = td->td_subfiletype;
		break;
	case TIFFTAG_IMAGEWIDTH:
		*va_arg(ap, u_long *) = td->td_imagewidth;
		break;
	case TIFFTAG_IMAGELENGTH:
		*va_arg(ap, u_long *) = td->td_imagelength;
		break;
	case TIFFTAG_BITSPERSAMPLE:
		*va_arg(ap, u_short *) = td->td_bitspersample;
		break;
	case TIFFTAG_COMPRESSION:
		*va_arg(ap, u_short *) = td->td_compression;
		break;
	case TIFFTAG_PHOTOMETRIC:
		*va_arg(ap, u_short *) = td->td_photometric;
		break;
	case TIFFTAG_THRESHHOLDING:
		*va_arg(ap, u_short *) = td->td_threshholding;
		break;
	case TIFFTAG_FILLORDER:
		*va_arg(ap, u_short *) = td->td_fillorder;
		break;
	case TIFFTAG_DOCUMENTNAME:
		*va_arg(ap, char **) = td->td_documentname;
		break;
	case TIFFTAG_ARTIST:
		*va_arg(ap, char **) = td->td_artist;
		break;
	case TIFFTAG_DATETIME:
		*va_arg(ap, char **) = td->td_datetime;
		break;
	case TIFFTAG_HOSTCOMPUTER:
		*va_arg(ap, char **) = td->td_hostcomputer;
		break;
	case TIFFTAG_IMAGEDESCRIPTION:
		*va_arg(ap, char **) = td->td_imagedescription;
		break;
	case TIFFTAG_MAKE:
		*va_arg(ap, char **) = td->td_make;
		break;
	case TIFFTAG_MODEL:
		*va_arg(ap, char **) = td->td_model;
		break;
	case TIFFTAG_SOFTWARE:
		*va_arg(ap, char **) = td->td_software;
		break;
	case TIFFTAG_ORIENTATION:
		*va_arg(ap, u_short *) = td->td_orientation;
		break;
	case TIFFTAG_SAMPLESPERPIXEL:
		*va_arg(ap, u_short *) = td->td_samplesperpixel;
		break;
	case TIFFTAG_ROWSPERSTRIP:
		*va_arg(ap, u_long *) = td->td_rowsperstrip;
		break;
	case TIFFTAG_MINSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_minsamplevalue;
		break;
	case TIFFTAG_MAXSAMPLEVALUE:
		*va_arg(ap, u_short *) = td->td_maxsamplevalue;
		break;
	case TIFFTAG_XRESOLUTION:
		*va_arg(ap, float *) = td->td_xresolution;
		break;
	case TIFFTAG_YRESOLUTION:
		*va_arg(ap, float *) = td->td_yresolution;
		break;
	case TIFFTAG_PLANARCONFIG:
		*va_arg(ap, u_short *) = td->td_planarconfig;
		break;
	case TIFFTAG_XPOSITION:
		*va_arg(ap, float *) = td->td_xposition;
		break;
	case TIFFTAG_YPOSITION:
		*va_arg(ap, float *) = td->td_yposition;
		break;
	case TIFFTAG_PAGENAME:
		*va_arg(ap, char **) = td->td_pagename;
		break;
	case TIFFTAG_GROUP3OPTIONS:
		*va_arg(ap, u_long *) = td->td_group3options;
		break;
	case TIFFTAG_GROUP4OPTIONS:
		*va_arg(ap, u_long *) = td->td_group4options;
		break;
	case TIFFTAG_RESOLUTIONUNIT:
		*va_arg(ap, u_short *) = td->td_resolutionunit;
		break;
	case TIFFTAG_PAGENUMBER:
		*va_arg(ap, u_short *) = td->td_pagenumber[0];
		*va_arg(ap, u_short *) = td->td_pagenumber[1];
		break;
	case TIFFTAG_HALFTONEHINTS:
		*va_arg(ap, u_short *) = td->td_halftonehints[0];
		*va_arg(ap, u_short *) = td->td_halftonehints[1];
		break;
	case TIFFTAG_COLORMAP:
		*va_arg(ap, u_short **) = td->td_colormap[0];
		*va_arg(ap, u_short **) = td->td_colormap[1];
		*va_arg(ap, u_short **) = td->td_colormap[2];
		break;
	case TIFFTAG_PREDICTOR:
		*va_arg(ap, u_short *) = td->td_predictor;
		break;
	case TIFFTAG_STRIPOFFSETS:
	case TIFFTAG_TILEOFFSETS:
		*va_arg(ap, u_long **) = td->td_stripoffset;
		break;
	case TIFFTAG_STRIPBYTECOUNTS:
	case TIFFTAG_TILEBYTECOUNTS:
		*va_arg(ap, u_long **) = td->td_stripbytecount;
		break;
	case TIFFTAG_MATTEING:
		*va_arg(ap, u_short *) = td->td_matteing;
		break;
	case TIFFTAG_EXTRASAMPLES:
		*va_arg(ap, u_short *) = td->td_matteing;
		*va_arg(ap, u_short **) = &td->td_matteing;
		break;
	case TIFFTAG_BADFAXLINES:
		*va_arg(ap, u_long *) = td->td_badfaxlines;
		break;
	case TIFFTAG_CLEANFAXDATA:
		*va_arg(ap, u_short *) = td->td_cleanfaxdata;
		break;
	case TIFFTAG_CONSECUTIVEBADFAXLINES:
		*va_arg(ap, u_long *) = td->td_badfaxrun;
		break;
	case TIFFTAG_TILEWIDTH:
		*va_arg(ap, u_long *) = td->td_tilewidth;
		break;
	case TIFFTAG_TILELENGTH:
		*va_arg(ap, u_long *) = td->td_tilelength;
		break;
	case TIFFTAG_TILEDEPTH:
		*va_arg(ap, u_long *) = td->td_tiledepth;
		break;
	case TIFFTAG_DATATYPE:
		*va_arg(ap, u_short *) =
		    (td->td_sampleformat == SAMPLEFORMAT_VOID ?
			0 : td->td_sampleformat);
		break;
	case TIFFTAG_SAMPLEFORMAT:
		*va_arg(ap, u_short *) = td->td_sampleformat;
		break;
	case TIFFTAG_IMAGEDEPTH:
		*va_arg(ap, u_long *) = td->td_imagedepth;
		break;
#ifdef YCBCR_SUPPORT
	case TIFFTAG_YCBCRCOEFFICIENTS:
		*va_arg(ap, float **) = td->td_ycbcrcoeffs;
		break;
	case TIFFTAG_YCBCRPOSITIONING:
		*va_arg(ap, u_short *) = td->td_ycbcrpositioning;
		break;
	case TIFFTAG_YCBCRSUBSAMPLING:
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[0];
		*va_arg(ap, u_short *) = td->td_ycbcrsubsampling[1];
		break;
#endif
#ifdef JPEG_SUPPORT
	case TIFFTAG_JPEGPROC:
		*va_arg(ap, u_short *) = td->td_jpegproc;
		break;
	case TIFFTAG_JPEGRESTARTINTERVAL:
		*va_arg(ap, u_short *) = td->td_jpegrestartinterval;
		break;
	case TIFFTAG_JPEGQTABLES:
		*va_arg(ap, u_char ***) = td->td_qtab;
		break;
	case TIFFTAG_JPEGDCTABLES:
		*va_arg(ap, u_char ***) = td->td_dctab;
		break;
	case TIFFTAG_JPEGACTABLES:
		*va_arg(ap, u_char ***) = td->td_actab;
		break;
#endif
#ifdef COLORIMETRY_SUPPORT
	case TIFFTAG_WHITEPOINT:
		*va_arg(ap, float **) = td->td_whitepoint;
		break;
	case TIFFTAG_PRIMARYCHROMATICITIES:
		*va_arg(ap, float **) = td->td_primarychromas;
		break;
	case TIFFTAG_TRANSFERFUNCTION: {
		int i;
		for (i = 0; i < td->td_samplesperpixel; i++)
			*va_arg(ap, u_short **) = td->td_transferfunction[i];
		break;
	}
	case TIFFTAG_REFERENCEBLACKWHITE:
		*va_arg(ap, float **) = td->td_refblackwhite;
		break;
#endif
#ifdef CMYK_SUPPORT
	case TIFFTAG_INKSET:
		*va_arg(ap, u_short *) = td->td_inkset;
		break;
	case TIFFTAG_DOTRANGE:
		*va_arg(ap, u_short *) = td->td_dotrange[0];
		*va_arg(ap, u_short *) = td->td_dotrange[1];
		break;
	case TIFFTAG_INKNAMES:
		*va_arg(ap, char **) = td->td_inknames;
		break;
	case TIFFTAG_TARGETPRINTER:
		*va_arg(ap, char **) = td->td_targetprinter;
		break;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	case TIFFTAG_CARTO_IFD_OFFSET:
		*va_arg(ap, u_long *) = td->td_carto_ifd_offset;
		break;
	case CARTTAG_PROJECTIONTYPE:
		*va_arg(ap, u_short *) = td->td_projectiontype ;
		break;
	case CARTTAG_PROJ_XPOS:
		*va_arg(ap, u_long *) = td->td_proj_xpos ;
		break;
	case CARTTAG_PROJ_YPOS:
		*va_arg(ap, u_long *) = td->td_proj_ypos ;
		break;
	case CARTTAG_LATITUDE:
		*va_arg(ap, u_long *) = td->td_latitude ;
		break;
	case CARTTAG_LONGITUDE:
		*va_arg(ap, u_long *) = td->td_longitude ;
		break;
	case CARTTAG_XPIXPERANGLE:
		*va_arg(ap, float *) = td->td_xpixperangle ;
		break;
	case CARTTAG_YPIXPERANGLE:
		*va_arg(ap, float *) = td->td_ypixperangle ;
		break;
#endif
	default:
		TIFFError("TIFFGetField1",
		    "Internal error, no value returned for tag \"%s\"",
		    TIFFFieldWithTag(tag)->field_name);
		break;
	}
	va_end(ap);
}

/*
 * Return the value of a field in the
 * internal directory structure.
 */
/*VARARGS2*/
DECLARE2V(TIFFGetField, TIFF*, tif, int, tag)
{
	TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);

	if (fip) {
		u_short bit = fip->field_bit;
		if (bit != FIELD_IGNORE && TIFFFieldSet(tif, bit)) {
			va_list ap;
			VA_START(ap, tag);
			(void) TIFFGetField1(&tif->tif_dir, tag, ap);
			va_end(ap);
			return (1);
		}
	} else
		TIFFError("TIFFGetField", "Unknown field, tag 0x%x", tag);
	return (0);
}

/*
 * Like TIFFGetField, but taking a varargs
 * parameter list.  This routine is useful
 * for building higher-level interfaces on
 * top of the library.
 */
TIFFVGetField(tif, tag, ap)
	TIFF *tif;
	int tag;
	va_list ap;
{
	TIFFFieldInfo const *fip = TIFFFindFieldInfo(tag, TIFF_ANY);

	if (fip) {
		u_short bit = fip->field_bit;
		if (bit != FIELD_IGNORE && TIFFFieldSet(tif, bit)) {
			(void) TIFFGetField1(&tif->tif_dir, tag, ap);
			return (1);
		}
	} else
		TIFFError("TIFFGetField", "Unknown field, tag 0x%x", tag);
	return (0);
}

/*
 * Internal interface to TIFFGetField...
 */
int
/*VARARGS2*/
DECLARE2V(_TIFFgetfield, TIFFDirectory*, td, int, tag)
{
	va_list ap;

	VA_START(ap, tag);
	(void) TIFFGetField1(td, tag, ap);
	va_end(ap);
	return (1);
}

#define	CleanupField(member) {		\
    if (td->member) {			\
	free((char *)td->member);	\
	td->member = 0;			\
    }					\
}

/*
 * Release storage associated with a directory.
 */
TIFFFreeDirectory(tif)
	TIFF *tif;
{
	register TIFFDirectory *td = &tif->tif_dir;

	CleanupField(td_colormap[0]);
	CleanupField(td_colormap[1]);
	CleanupField(td_colormap[2]);
	CleanupField(td_documentname);
	CleanupField(td_artist);
	CleanupField(td_datetime);
	CleanupField(td_hostcomputer);
	CleanupField(td_imagedescription);
	CleanupField(td_make);
	CleanupField(td_model);
	CleanupField(td_software);
	CleanupField(td_pagename);
#ifdef YCBCR_SUPPORT
	CleanupField(td_ycbcrcoeffs);
#endif
#ifdef JPEG_SUPPORT
	CleanupField(td_qtab);
	CleanupField(td_dctab);
	CleanupField(td_actab);
#endif
#ifdef CMYK_SUPPORT
	CleanupField(td_inknames);
	CleanupField(td_targetprinter);
#endif
#ifdef COLORIMETRY_SUPPORT
	CleanupField(td_whitepoint);
	CleanupField(td_primarychromas);
	CleanupField(td_refblackwhite);
	CleanupField(td_transferfunction[0]);
	CleanupField(td_transferfunction[1]);
	CleanupField(td_transferfunction[2]);
	CleanupField(td_transferfunction[3]);
#endif
	CleanupField(td_stripoffset);
	CleanupField(td_stripbytecount);
}
#undef CleanupField

/*
 * Setup a default directory structure.
 */
TIFFDefaultDirectory(tif)
	TIFF *tif;
{
	register TIFFDirectory *td = &tif->tif_dir;

	mybzero((char *)td, sizeof (*td));
	td->td_fillorder = FILLORDER_MSB2LSB;
	td->td_bitspersample = 1;
	td->td_threshholding = THRESHHOLD_BILEVEL;
	td->td_orientation = ORIENTATION_TOPLEFT;
	td->td_samplesperpixel = 1;
	td->td_predictor = 1;
	td->td_rowsperstrip = 0xffffffff;
	td->td_tilewidth = 0xffffffff;
	td->td_tilelength = 0xffffffff;
	td->td_tiledepth = 1;
	td->td_resolutionunit = RESUNIT_INCH;
	td->td_sampleformat = SAMPLEFORMAT_VOID;
	td->td_imagedepth = 1;
#ifdef YCBCR_SUPPORT
	td->td_ycbcrsubsampling[0] = 2;
	td->td_ycbcrsubsampling[1] = 2;
	td->td_ycbcrpositioning = YCBCRPOSITION_CENTERED;
#endif
#ifdef CMYK_SUPPORT
	td->td_inkset = INKSET_CMYK;
#endif
	(void) TIFFSetField(tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
	/*
	 * NB: The directory is marked dirty as a result of setting
	 * up the default compression scheme.  However, this really
	 * isn't correct -- we want TIFF_DIRTYDIRECT to be set only
	 * if the user does something.  We could just do the setup
	 * by hand, but it seems better to use the normal mechanism
	 * (i.e. TIFFSetField).
	 */
	tif->tif_flags &= ~(TIFF_DIRTYDIRECT);
	return (1);
}

/*
 * Set the n-th directory as the current directory.
 * NB: Directories are numbered starting at 0.
 */
TIFFSetDirectory(tif, dirn)
	register TIFF *tif;
	int dirn;
{
	static char module[] = "TIFFSetDirectory";
	u_short dircount;
	long nextdir;
	int n;

	nextdir = tif->tif_header.tiff_diroff;
	for (n = dirn; n > 0 && nextdir != 0; n--) {
		if (!SeekOK(tif->tif_fd, nextdir) ||
		    !TIFFReadShort(tif->tif_fd, (short *)&dircount)) {
			TIFFError(module, "%s: Error fetching directory count",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		lseek(tif->tif_fd, dircount*TIFFDirSize, L_INCR);
		if (!ReadOK(tif->tif_fd, &nextdir, TIFFLongSize)) {
			TIFFError(module, "%s: Error fetching directory link",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabLong((u_long *)&nextdir);
	}
	tif->tif_nextdiroff = nextdir;
	/*
	 * Set curdir to the actual directory index.  The
	 * -1 is because TIFFReadDirectory will increment
	 * tif_curdir after successfully reading the directory.
	 */
	tif->tif_curdir = (dirn - n) - 1;
	return (TIFFReadDirectory(tif));
}



/* start dir_writ */

static	long dataoff;
/*
 * Write the contents of the current directory
 * to the specified file.  This routine doesn't
 * handle overwriting a directory with auxiliary
 * storage that's been changed.
 */
TIFFWriteDirectory(tif)
	TIFF *tif;
{
	short dircount, tag;
	int nfields, dirsize;
	char *data;
	TIFFFieldInfo *fip;
	TIFFDirEntry *dir;
	TIFFDirectory *td;
	u_long b, fields[sizeof (td->td_fieldsset) / sizeof (u_long)];

	if (tif->tif_mode == O_RDONLY)
		return (1);
	/*
	 * Clear write state so that subsequent images with
	 * different characteristics get the right buffers
	 * setup for them.
	 */
	if (tif->tif_flags & TIFF_POSTENCODE) {
		tif->tif_flags &= ~TIFF_POSTENCODE;
		if (tif->tif_postencode && !(*tif->tif_postencode)(tif)) {
			TIFFError(tif->tif_name,
			    "Error post-encoding before directory write");
			return (0);
		}
	}
	if (tif->tif_close)
		(*tif->tif_close)(tif);
	if (tif->tif_cleanup)
		(*tif->tif_cleanup)(tif);
	/*
	 * Flush any data that might have been written
	 * by the compression close+cleanup routines.
	 */
	if (tif->tif_rawcc > 0 && !TIFFFlushData1(tif)) {
		TIFFError(tif->tif_name,
		    "Error flushing data before directory write");
		return (0);
	}
	if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata) {
		free(tif->tif_rawdata);
		tif->tif_rawdata = NULL;
		tif->tif_rawcc = 0;
	}
	tif->tif_flags &= ~(TIFF_BEENWRITING|TIFF_BUFFERSETUP|TIFF_ISTILED);
#ifdef CARTOGRAPHIC_SUPPORT
	/*
	 * Write out the private Carto Directory, if values have been set.
	 */
	 if (tif->tif_flags & TIFF_DIRTYCART)
	 	CARTWriteDirectory(tif);
	tif->tif_flags &= ~TIFF_HASCARTTAGS;
#endif

	td = &tif->tif_dir;
	/*
	 * Size the directory so that we can calculate
	 * offsets for the data items that aren't kept
	 * in-place in each field.
	 */
	nfields = 0;
	for (b = 0; b <= FIELD_LAST; b++)
		if (TIFFFieldSet(tif, b))
			nfields += (b < FIELD_SUBFILETYPE ? 2 : 1);
	dirsize = nfields * sizeof (TIFFDirEntry);
	data = malloc(dirsize);
	if (data == NULL) {
		TIFFError(tif->tif_name,
		    "Cannot write directory, out of space");
		return (0);
	}
	/*
	 * Directory hasn't been placed yet, put
	 * it at the end of the file and link it
	 * into the existing directory structure.
	 */
	if (tif->tif_diroff == 0 && !TIFFLinkDirectory(tif))
		return (0);
	dataoff = tif->tif_diroff + 
		TIFFShortSize + TIFFDirSize*nfields + TIFFLongSize;
	if (dataoff & 1)
		dataoff++;
	(void) lseek(tif->tif_fd, dataoff, L_SET);
	tif->tif_curdir++;
	dir = (TIFFDirEntry *)data;
	/*
	 * Setup external form of directory
	 * entries and write data items.
	 */
	mybcopy(td->td_fieldsset, fields, sizeof (fields));
/*BEGIN XXX*/
	/*
	 * Write out ExtraSamples tag only if Matteing would
	 * be set to 1 (i.e. Associated Alpha data is present).
	 */
	if (FieldSet(fields, FIELD_MATTEING) && !td->td_matteing) {	/*XXX*/
		ResetFieldBit(fields, FIELD_MATTEING);			/*XXX*/
		nfields--;						/*XXX*/
		dirsize -= sizeof (TIFFDirEntry);			/*XXX*/
	}								/*XXX*/
/*END XXX*/
	for (fip = (TIFFFieldInfo *)tiffFieldInfo; fip->field_tag; fip++) {
		if (fip->field_bit == FIELD_IGNORE ||
		    !FieldSet(fields, fip->field_bit))
			continue;
		switch (fip->field_bit) {
		case FIELD_STRIPOFFSETS:
			/*
			 * We use one field bit for both strip and tile
			 * offsets, and so must be careful in selecting
			 * the appropriate field descriptor (so that tags
			 * are written in sorted order).
			 */
			tag = isTiled(tif) ?
			    TIFFTAG_TILEOFFSETS : TIFFTAG_STRIPOFFSETS;
			if (tag != fip->field_tag)
				continue;
			if (!TIFFWriteLongArray(tif, TIFF_LONG, tag, dir,
			    (int) td->td_nstrips, td->td_stripoffset))
				goto bad;
			break;
		case FIELD_STRIPBYTECOUNTS:
			/*
			 * We use one field bit for both strip and tile
			 * byte counts, and so must be careful in selecting
			 * the appropriate field descriptor (so that tags
			 * are written in sorted order).
			 */
			tag = isTiled(tif) ?
			    TIFFTAG_TILEBYTECOUNTS : TIFFTAG_STRIPBYTECOUNTS;
			if (tag != fip->field_tag)
				continue;
			if (!TIFFWriteLongArray(tif, TIFF_LONG, tag, dir,
			    (int) td->td_nstrips, td->td_stripbytecount))
				goto bad;
			break;
		case FIELD_COLORMAP:
			if (!TIFFWriteShortTable(tif, TIFFTAG_COLORMAP, dir,
			    3, td->td_colormap))
				goto bad;
			break;
		case FIELD_IMAGEDIMENSIONS:
			TIFFSetupShortLong(tif, TIFFTAG_IMAGEWIDTH,
			    dir++, td->td_imagewidth);
			TIFFSetupShortLong(tif, TIFFTAG_IMAGELENGTH,
			    dir, td->td_imagelength);
			break;
		case FIELD_TILEDIMENSIONS:
			TIFFSetupShortLong(tif, TIFFTAG_TILEWIDTH,
			    dir++, td->td_tilewidth);
			TIFFSetupShortLong(tif, TIFFTAG_TILELENGTH,
			    dir, td->td_tilelength);
			break;
		case FIELD_POSITION:
			WriteRationalPair(TIFF_RATIONAL,
			    TIFFTAG_XPOSITION, td->td_xposition,
			    TIFFTAG_YPOSITION, td->td_yposition);
			break;
		case FIELD_RESOLUTION:
			WriteRationalPair(TIFF_RATIONAL,
			    TIFFTAG_XRESOLUTION, td->td_xresolution,
			    TIFFTAG_YRESOLUTION, td->td_yresolution);
			break;
		case FIELD_BITSPERSAMPLE:
		case FIELD_MINSAMPLEVALUE:
		case FIELD_MAXSAMPLEVALUE:
		case FIELD_SAMPLEFORMAT:
			if (!TIFFWritePerSampleShorts(tif, fip->field_tag, dir))
				goto bad;
			break;
		case FIELD_PAGENUMBER:
		case FIELD_HALFTONEHINTS:
#ifdef YCBCR_SUPPORT
		case FIELD_YCBCRSUBSAMPLING:
#endif
#ifdef CMYK_SUPPORT
		case FIELD_DOTRANGE:
#endif
			TIFFSetupShortPair(tif, fip->field_tag, dir);
			break;
#ifdef JPEG_SUPPORT
		case FIELD_JPEGQTABLES:
			if (!TIFFWriteJPEGQTables(tif, dir))
				goto bad;
			break;
		case FIELD_JPEGDCTABLES:
			if (!TIFFWriteJPEGCTables(tif,
			    TIFFTAG_JPEGDCTABLES, dir, td->td_dctab))
				goto bad;
			break;
		case FIELD_JPEGACTABLES:
			if (!TIFFWriteJPEGCTables(tif,
			    TIFFTAG_JPEGACTABLES, dir, td->td_actab))
				goto bad;
			break;
#endif
#ifdef COLORIMETRY_SUPPORT
		case FIELD_REFBLACKWHITE:
			if (!TIFFWriteRationalArray(tif, TIFF_RATIONAL,
			    TIFFTAG_REFERENCEBLACKWHITE, dir,
			    2*td->td_samplesperpixel, td->td_refblackwhite))
				goto bad;
			break;
		case FIELD_TRANSFERFUNCTION:
			if (!TIFFWriteTransferFunction(tif, dir))
				goto bad;
			break;
#endif
		default:
			if (!TIFFWriteNormalTag(tif, dir, fip))
				goto bad;
			break;
		}
		dir++;
		ResetFieldBit(fields, fip->field_bit);
	}
	/*
	 * Write directory.
	 */
	(void) lseek(tif->tif_fd, tif->tif_diroff, L_SET);
	dircount = nfields;
	if (!TIFFWriteShort(tif->tif_fd, &dircount)) {
		TIFFError(tif->tif_name, "Error writing directory count");
		goto bad;
	}
	if (!TIFFWriteTIFFDir(tif->tif_fd, (TIFFDirEntry*)data, dircount)) {
		TIFFError(tif->tif_name, "Error writing directory contents");
		goto bad;
	}
	if (!WriteOK(tif->tif_fd, &tif->tif_nextdiroff, TIFFLongSize)) {
		TIFFError(tif->tif_name, "Error writing directory link");
		goto bad;
	}
	TIFFFreeDirectory(tif);
	free(data);
	tif->tif_flags &= ~TIFF_DIRTYDIRECT;

	/*
	 * Reset directory-related state for subsequent
	 * directories.
	 */
	TIFFDefaultDirectory(tif);
	tif->tif_diroff = 0;
	tif->tif_curoff = 0;
	tif->tif_row = -1;
	tif->tif_curstrip = -1;
	return (1);
bad:
	free(data);
	return (0);
}
#undef WriteRationalPair

static
DECLARE3(TIFFWriteTIFFDir,
    int, fd, TIFFDirEntry*, dir, int, n)
{
	int i;
	
	for (i=0; i< n; i++)
	{
		if(!TIFFWriteShort(fd,(short *)&dir->tdir_tag)) return (0);
		if(!TIFFWriteShort(fd,(short *)&dir->tdir_type)) return (0);
		if(!WriteOK(fd,&dir->tdir_count,TIFFLongSize)) return (0);
		if(!WriteOK(fd,&dir->tdir_offset,TIFFLongSize)) return (0);
		dir++;
	}
	return (1);
}


/*
 * Process tags that are not special cased.
 */
static
DECLARE3(TIFFWriteNormalTag,
    TIFF*, tif, TIFFDirEntry*, dir, TIFFFieldInfo*, fip)
{
	TIFFDirectory* td = &tif->tif_dir;
	u_short wc = (u_short) fip->field_writecount;

	dir->tdir_tag = fip->field_tag;
	dir->tdir_type = (u_short)fip->field_type;
	dir->tdir_count = wc;
#define	WRITE(x,y)	x(tif, fip->field_type, fip->field_tag, dir, wc, y)
	switch (fip->field_type) {
	case TIFF_SHORT:
	case TIFF_SSHORT:
		if (wc > 1) {
			u_short *wp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &wp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &wp);
			if (!WRITE(TIFFWriteShortArray, wp))
				return (0);
		} else {
			u_short sv;
			_TIFFgetfield(td, fip->field_tag, &sv);
			dir->tdir_offset =
			    TIFFInsertData(tif, dir->tdir_type, sv);
		}
		break;
	case TIFF_LONG:
	case TIFF_SLONG:
		if (wc > 1) {
			u_long *lp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &lp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &lp);
			if (!WRITE(TIFFWriteLongArray, lp))
				return (0);
		} else {
			/* XXX handle LONG->SHORT conversion */
			_TIFFgetfield(td, fip->field_tag, &dir->tdir_offset);
		}
		break;
	case TIFF_RATIONAL:
	case TIFF_SRATIONAL:
		if (wc > 1) {
			float *fp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &fp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &fp);
			if (!WRITE(TIFFWriteRationalArray, fp))
				return (0);
		} else {
			float fv;
			_TIFFgetfield(td, fip->field_tag, &fv);
			if (!TIFFWriteRational(tif, fip->field_type, fip->field_tag, dir, fv))
				return (0);
		}
		break;
	case TIFF_FLOAT:
		if (wc > 1) {
			float *fp;
			if (wc == (u_short) TIFF_VARIABLE) {
				_TIFFgetfield(td, fip->field_tag, &wc, &fp);
				dir->tdir_count = wc;
			} else
				_TIFFgetfield(td, fip->field_tag, &fp);
			if (!WRITE(TIFFWriteFloatArray, fp))
				return (0);
		} else {
			float fv;
			_TIFFgetfield(td, fip->field_tag, &fv);
			TIFFCvtNativeToIEEEFloat(tif, 1, &fv);
			/* XXX assumes sizeof (long) == sizeof (float) */
			dir->tdir_offset = *(u_long *)&fv;	/* XXX */
		}
		break;
	case TIFF_ASCII: {
		char *cp;
		_TIFFgetfield(td, fip->field_tag, &cp);
		if (!TIFFWriteString(tif, fip->field_tag, dir, cp))
			return (0);
		break;
	}
	}
	return (1);
}
#undef WRITE

/*
 * Setup a directory entry with either a SHORT
 * or LONG type according to the value.
 */
static
DECLARE4(TIFFSetupShortLong,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, u_long, v)
{
	dir->tdir_tag = tag;
	dir->tdir_count = 1;
	if (v > 0xffffL) {
		dir->tdir_type = (short)TIFF_LONG;
		dir->tdir_offset = v;
	} else {
		dir->tdir_type = (short)TIFF_SHORT;
		dir->tdir_offset = TIFFInsertData(tif, (int)TIFF_SHORT, v);
	}
}
#undef MakeShortDirent

/*
 * Setup a RATIONAL directory entry and
 * write the associated indirect value.
 */
static
DECLARE5(TIFFWriteRational,
    TIFF*, tif, TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, float, v)
{
	u_long t[2];
 
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = 1;
	if (type == TIFF_RATIONAL && (v < 0 || v > (u_long)ULONG_MAX))
		TIFFWarning(tif->tif_name,
	"\"%s\": Information lost writing value (%g) as (unsigned) RATIONAL",
		    TIFFFieldWithTag(tag)->field_name, v);
    TIFFFloatToRational(v,(u_long *)t);
	return (TIFFWriteData(tif, dir, (char *)t));
}

/*
 * Setup a directory entry that references a
 * samples/pixel array of SHORT values and
 * (potentially) write the associated indirect
 * values.
 */
static
DECLARE3(TIFFWritePerSampleShorts,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir)
{
	u_short w[4], v;
	int i, samplesperpixel = tif->tif_dir.td_samplesperpixel;

	_TIFFgetfield(&tif->tif_dir, tag, &v);
	for (i = 0; i < samplesperpixel; i++)
		w[i] = v;
	return (TIFFWriteShortArray(
	    tif, TIFF_SHORT, tag, dir, samplesperpixel, w));
}

/*
 * Setup a pair of shorts that are returned by
 * value, rather than as a reference to an array.
 */
static
DECLARE3(TIFFSetupShortPair,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir)
{
	u_short v[2];

	_TIFFgetfield(&tif->tif_dir, tag, &v[0], &v[1]);
	return (TIFFWriteShortArray(tif, TIFF_SHORT, tag, dir, 2, v));
}

/*
 * Setup a directory entry for an NxM table of shorts,
 * where M is known to be 2**bitspersample, and write
 * the associated indirect data.
 */
static
DECLARE5(TIFFWriteShortTable,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, int, n, u_short**, table)
{
	u_long off;
	int i;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)TIFF_SHORT;
	/* XXX -- yech, fool TIFFWriteData */
	dir->tdir_count = 1L<<tif->tif_dir.td_bitspersample;
	off = dataoff;
	for (i = 0; i < n; i++)
	{
		TIFFPackShort( (short *)table[i], dir->tdir_count);
		if (!TIFFWriteData(tif, dir, (char *)table[i]))
			return (0);
		TIFFUnPackShort( (short *)table[i], dir->tdir_count);
	}
	dir->tdir_count *= n;
	dir->tdir_offset = off;
	return (1);
}

/*
 * Setup a directory entry of an ASCII string
 * and write any associated indirect value.
 */
static
DECLARE4(TIFFWriteString,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, char*, cp)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)TIFF_ASCII;
	dir->tdir_count = strlen(cp) + 1;	/* includes \0 byte */
	if (dir->tdir_count > 4) {
		if (!TIFFWriteData(tif, dir, cp))
			return (0);
	} else
		mybcopy(cp, &dir->tdir_offset, dir->tdir_count);
	return (1);
}

/*
 * Setup a directory entry of an array of SHORT
 * or SSHORT and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteShortArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, u_short*, v)
{
	int ok;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	if (n <= 2) {
		if (tif->tif_header.tiff_magic == TIFF_BIGENDIAN) {
			dir->tdir_offset = (long)v[0] << 16;
			if (n == 2)
				dir->tdir_offset |= v[1] & 0xffff;
		} else {
			dir->tdir_offset = v[0] & 0xffff;
			if (n == 2)
				dir->tdir_offset |= (long)v[1] << 16;
		}
		return (1);
	} else
	{
		TIFFPackShort((short *)v, n);
		ok = TIFFWriteData(tif, dir, (char *)v);
		TIFFUnPackShort( (short *)v, n);
		return (ok);
	}
}

/*
 * Setup a directory entry of an array of LONG
 * or SLONG and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteLongArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, u_long*, v)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	if (n == 1) {
		dir->tdir_offset = v[0];
		return (1);
	} else
		return (TIFFWriteData(tif, dir, (char *)v));
}

/*
 * Setup a directory entry of an array of RATIONAL
 * or SRATIONAL and write the associated indirect values.
 */
static
DECLARE6(TIFFWriteRationalArray, TIFF*, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry*, dir, int, n, float*, v)
{
	int i, status;
	u_long *t;

	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	t = (u_long *)malloc(2*n * sizeof (long));
	for (i = 0; i < n; i++) {
	    TIFFFloatToRational(v[i],t+2*i);
	}
	status = TIFFWriteData(tif, dir, (char *)t);
	free((char *)t);
	return (status);
}

static
DECLARE6(TIFFWriteFloatArray, TIFF *, tif,
    TIFFDataType, type, u_short, tag, TIFFDirEntry *, dir, int, n, float *, v)
{
	dir->tdir_tag = tag;
	dir->tdir_type = (short)type;
	dir->tdir_count = n;
	TIFFCvtNativeToIEEEFloat(tif, n, v);
	if (n == 1) {
		dir->tdir_offset = *(u_long *)&v[0];
		return (1);
	} else
		return (TIFFWriteData(tif, dir, (char *)v));
}

#ifdef JPEG_SUPPORT
/*
 * Setup a directory entry for JPEG Quantization
 * tables and write the associated indirect values.
 */
static
DECLARE2(TIFFWriteJPEGQTables, TIFF*, tif, TIFFDirEntry*, dir)
{
	TIFFDirectory *td = &tif->tif_dir;
	TIFFDirEntry tdir;
	u_long off[4];
	int i;

	tdir.tdir_tag = TIFFTAG_JPEGQTABLES;	/* for diagnostics */
	tdir.tdir_type = (short)TIFF_BYTE;
	tdir.tdir_count = 64;
	for (i = 0; i < td->td_samplesperpixel; i++) {
		if (!TIFFWriteData(tif, &tdir, (char *)td->td_qtab[i]))
			return (0);
		off[i] = tdir.tdir_offset;
	}
	return (TIFFWriteLongArray(tif, TIFF_LONG,
	    TIFFTAG_JPEGQTABLES, dir, td->td_samplesperpixel, off));
}

/*
 * Setup a directory entry for JPEG Coefficient
 * tables and write the associated indirect values.
 */
static
DECLARE4(TIFFWriteJPEGCTables,
    TIFF*, tif, u_short, tag, TIFFDirEntry*, dir, u_char **, tab)
{
	TIFFDirectory *td = &tif->tif_dir;
	TIFFDirEntry tdir;
	u_long off[4];
	int i, j, ncodes;

	tdir.tdir_tag = tag;		/* for diagnostics */
	tdir.tdir_type = (short)TIFF_BYTE;
	for (i = 0; i < td->td_samplesperpixel; i++) {
		for (ncodes = 0, j = 0; j < 16; j++)
			ncodes += tab[i][j];
		tdir.tdir_count = 16+ncodes;
		if (!TIFFWriteData(tif, &tdir, (char *)tab[i]))
			return (0);
		off[i] = tdir.tdir_offset;
	}
	return (TIFFWriteLongArray(tif,
	    TIFF_LONG, tag, dir, td->td_samplesperpixel, off));
}
#endif

#ifdef COLORIMETRY_SUPPORT
static
DECLARE2(TIFFWriteTransferFunction, TIFF*, tif, TIFFDirEntry*, dir)
{
	TIFFDirectory *td = &tif->tif_dir;
	int j, ncols;
	u_long n;
	u_short **tf = td->td_transferfunction;

	/*
	 * Check if the table can be written as a single column.
	 */
	n = (1L<<td->td_bitspersample) * sizeof (u_short);
	ncols = 1;		/* assume only one column is needed */
	for (j = 1; j < td->td_samplesperpixel; j++)
		if (mybcmp(tf[0], tf[j], n)) {
			ncols = td->td_samplesperpixel;
			break;
		}
	return (TIFFWriteShortTable(tif,
	    TIFFTAG_TRANSFERFUNCTION, dir, ncols, tf));
}
#endif

/*
 * Write a contiguous directory item.
 */
static
TIFFWriteData(tif, dir, cp)
	TIFF *tif;
	TIFFDirEntry *dir;
	char *cp;
{
	int cc;

	dir->tdir_offset = dataoff;
	cc = dir->tdir_count * tiffDataWidth[dir->tdir_type];
	if (SeekOK(tif->tif_fd, dir->tdir_offset) &&
	    WriteOK(tif->tif_fd, cp, cc)) {
		dataoff += (cc + 1) & ~1;
		return (1);
	}
	TIFFError(tif->tif_name, "Error writing data for field \"%s\"",
	    TIFFFieldWithTag(dir->tdir_tag)->field_name);
	return (0);
}

/*
 * Link the current directory into the
 * directory chain for the file.
 */
static
TIFFLinkDirectory(tif)
	register TIFF *tif;
{
	static char module[] = "TIFFLinkDirectory";
	u_short dircount;
	long nextdir;

	tif->tif_diroff = (lseek(tif->tif_fd, 0L, L_XTND)+1) &~ 1L;
	if (tif->tif_header.tiff_diroff == 0) {
		/*
		 * First directory, overwrite header.
		 */
		tif->tif_header.tiff_diroff = tif->tif_diroff;
		(void) lseek(tif->tif_fd, 0L, L_SET);
		if (!TIFFWriteTIFFHdr(tif->tif_fd, &tif->tif_header)) {
			TIFFError(tif->tif_name, "Error writing TIFF header");
			return (0);
		}
		return (1);
	}
	/*
	 * Not the first directory, search to the last and append.
	 */
	nextdir = tif->tif_header.tiff_diroff;
	do {
		if (!SeekOK(tif->tif_fd, nextdir) ||
		    !TIFFReadShort(tif->tif_fd, (short *)&dircount)) {
			TIFFError(module, "Error fetching directory count");
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabShort(&dircount);
		lseek(tif->tif_fd, dircount * TIFFDirSize, L_INCR);
		if (!ReadOK(tif->tif_fd, &nextdir, TIFFLongSize)) {
			TIFFError(module, "Error fetching directory link");
			return (0);
		}
		if (tif->tif_flags & TIFF_SWAB)
			TIFFSwabLong((u_long *)&nextdir);
	} while (nextdir != 0);
	(void) lseek(tif->tif_fd, -TIFFLongSize, L_INCR);
	if (!WriteOK(tif->tif_fd, &tif->tif_diroff, TIFFLongSize)) {
		TIFFError(module, "Error writing directory link");
		return (0);
	}
	return (1);
}

#ifdef CARTOGRAPHIC_SUPPORT
/*
 * Write the contents of the cartographic directory
 * to the specified file.
 */
CARTWriteDirectory(tif)
	TIFF *tif;
{
	u_short dircount;
	int nfields, dirsize;
	char *data;
	TIFFFieldInfo *fip;
	TIFFDirEntry *dir;
	TIFFDirectory *td;
	long nextoff=0L;

	if (tif->tif_mode == O_RDONLY)
		return (1);

	td = &tif->tif_dir;
	/*
	 * Size the directory so that we can calculate
	 * offsets for the data items that aren't kept
	 * in-place in each field. All cart fields counted.
	 */
	nfields = 0;
	for (fip = (TIFFFieldInfo *)cartFieldInfo; fip->field_tag; fip++)
		nfields++;
	dirsize = nfields * sizeof (TIFFDirEntry);
	data = malloc(dirsize);
	if (data == NULL) {
		TIFFError(tif->tif_name,
		    "Cannot write CART directory, out of space");
		return (0);
	}
	/*
	 * If Directory hasn't been placed yet, put
	 * it at the end of the file and link it
	 * into the existing directory structure.
	 */
	if (td->td_carto_ifd_offset == 0 && !CARTLinkDirectory(tif))
		return (0);
	/* here's the offset to data written outside of tag */
	dataoff = td->td_carto_ifd_offset + 
		TIFFShortSize + TIFFDirSize*nfields + TIFFLongSize;
	if (dataoff & 1)
		dataoff++;
	(void) lseek(tif->tif_fd, dataoff, L_SET);
	dir = (TIFFDirEntry *)data;

	/*
	 * Setup external form of carto directory
	 * entries and write data items.
	 * Field-bit stuff not yet needed; All fields
	 * should be written out.
	 */
	for (fip = (TIFFFieldInfo *)cartFieldInfo; fip->field_tag; fip++) {
		if (!TIFFWriteNormalTag(tif, dir, fip))
			goto bad;
		dir++;
	}
	/*
	 * Write private directory.
	 */
	(void) lseek(tif->tif_fd, td->td_carto_ifd_offset, L_SET);
	dircount = nfields;
	if (!TIFFWriteShort(tif->tif_fd, (short *)&dircount)) {
		TIFFError(tif->tif_name, "Error writing CART directory count");
		goto bad;
	}
	if (!TIFFWriteTIFFDir(tif->tif_fd, (TIFFDirEntry*)data, dircount)) {
		TIFFError(tif->tif_name, "Error writing CART directory contents");
		goto bad;
	}
	if (!WriteOK(tif->tif_fd, &nextoff, TIFFLongSize)) {
		TIFFError(tif->tif_name, "Error writing CART directory link");
		goto bad;
	}

	free(data);
	tif->tif_flags &= ~TIFF_DIRTYCART;

	return (1);
bad:
	free(data);
	return (0);
}

/*
 * Link the current CART directory into the
 * current directory's carto_ifd offset value..
 */
static
CARTLinkDirectory(tif)
	register TIFF *tif;
{
	u_long offset;
	
	if (!(offset = (lseek(tif->tif_fd, 0L, L_XTND)+1)))
		return(0);
	offset = offset & ~1L;   /* must be on word boundary */
	TIFFSetField(tif, TIFFTAG_CARTO_IFD_OFFSET, offset);
	return (1);
}
#endif /* CARTO */

/*
 * Convert a floating point value to rational pair
 * using the Continued Fraction Expansion of the value.
 * This gives a fraction accurate to seven significant digits.
 */
static
DECLARE2(TIFFFloatToRational,float,in,u_long *,outr)
{
	dblparam_t threshold;

	threshold=(u_long)ULONG_MAX;
	threshold=1.1/threshold;

	if (in>=(1/threshold)) in= (0.999/threshold);
	if (in < 1.0)
		TIFFgetfraction((dblparam_t)in,outr,outr+1,threshold,0);
	else
		TIFFgetfraction((dblparam_t)1/in,outr+1,outr,threshold,0);
}

/* Recursive Computation of Continued Fraction (x <= 1) */
#define RECURSION_LIMIT 15
static
TIFFgetfraction(x, num, den, thresh,level)
dblparam_t x;
u_long *num;
u_long *den;
dblparam_t thresh;
int level;
{
	u_long k,num1,den1;
	dblparam_t rem;

	if (x <= thresh || ++level > RECURSION_LIMIT ) /* We're done */
	{
		*num=0;
		*den=1;
	}
	else /* get fraction of remainder */
	{
		k=1/x;
		rem=(1/x)-k;
		TIFFgetfraction(rem, &num1, &den1, thresh*(k+1),level);
		*num = den1;
		*den = k*den1 + num1;
	}
}

/* end dir_writ */
