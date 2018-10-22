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
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <stdio.h>
#include <string.h>

static void
DECLARE3(defaultPHandler,FILE*, fd, char*, fmt, va_list, ap)
{
	vfprintf(fd, fmt, ap);
}

static TIFFPrintHandler _printHandler = defaultPHandler;

TIFFPrintHandler
DECLARE1(TIFFSetPrintHandler, TIFFPrintHandler, handler)
{
	TIFFPrintHandler prev = _printHandler;
	_printHandler = handler;
	return (prev);
}

void
#if USE_PROTOTYPES
TIFFPrint(FILE* fd,char *fmt, ...)
#else
/*VARARGS2*/
TIFFPrint(fd, fmt, va_alist)
	FILE *fd;
	char *fmt;
	va_dcl
#endif
{
	if (_printHandler) {
		va_list ap;
		VA_START(ap, fmt);
		(*_printHandler)(fd, fmt, ap);
		va_end(ap);
	}
}


#ifdef JPEG_SUPPORT
static
JPEGPrintQTable(fd, tab)
	FILE *fd;
	u_char tab[64];
{
	int i, j;
	char *sep;

	fputc('\n', fd);
	for (i = 0; i < 8; i++) {
		sep = "    ";
		for (j = 0; j < 8; j++) {
			TIFFPrint(fd, "%s%2u", sep, tab[8*i+j]);
			sep = ", ";
		}
		fputc('\n', fd);
	}
}

static
JPEGPrintCTable(fd, tab)
	FILE *fd;
	u_char *tab;
{
	int i, n, count;
	char *sep;

	TIFFPrint(fd, "\n    Bits:");
	count = 0;
	for (i = 0; i < 16; i++) {
		TIFFPrint(fd, " %u", tab[i]);
		count += tab[i];
	}
	n = 0;
	for (; count > 0; count--) {
		if ((n % 8) == 0) {
			fputc('\n', fd);
			sep = "    ";
		}
		TIFFPrint(fd, "%s0x%02x", sep, tab[i++]);
		sep = ", ";
		n++;

	}
	if (n % 8)
		fputc('\n', fd);
}
#endif

static const char *photoNames[] = {
    "min-is-white",				/* PHOTOMETRIC_MINISWHITE */
    "min-is-black",				/* PHOTOMETRIC_MINISBLACK */
    "RGB color",				/* PHOTOMETRIC_RGB */
    "palette color (RGB from colormap)",	/* PHOTOMETRIC_PALETTE */
    "transparency mask",			/* PHOTOMETRIC_MASK */
    "separated",				/* PHOTOMETRIC_SEPARATED */
    "YCbCr",					/* PHOTOMETRIC_YCBCR */
    "7 (0x7)",
    "CIE L*a*b*",				/* PHOTOMETRIC_CIELAB */
};
#define	NPHOTONAMES	(sizeof (photoNames) / sizeof (photoNames[0]))

static const char *orientNames[] = {
    "0 (0x0)",
    "row 0 top, col 0 lhs",			/* ORIENTATION_TOPLEFT */
    "row 0 top, col 0 rhs",			/* ORIENTATION_TOPRIGHT */
    "row 0 bottom, col 0 rhs",			/* ORIENTATION_BOTRIGHT */
    "row 0 bottom, col 0 lhs",			/* ORIENTATION_BOTLEFT */
    "row 0 lhs, col 0 top",			/* ORIENTATION_LEFTTOP */
    "row 0 rhs, col 0 top",			/* ORIENTATION_RIGHTTOP */
    "row 0 rhs, col 0 bottom",			/* ORIENTATION_RIGHTBOT */
    "row 0 lhs, col 0 bottom",			/* ORIENTATION_LEFTBOT */
};
#define	NORIENTNAMES	(sizeof (orientNames) / sizeof (orientNames[0]))

/*
 * Print the contents of the current directory
 * to the specified stdio file stream.
 */
void
TIFFPrintDirectory(tif, fd, flags)
	TIFF *tif;
	FILE *fd;
	long flags;
{
	register TIFFDirectory *td;
	char *sep;
	int i, j;
	long n;

	TIFFPrint(fd, "TIFF Directory at offset 0x%x\n", tif->tif_diroff);
	td = &tif->tif_dir;
	if (TIFFFieldSet(tif,FIELD_SUBFILETYPE)) {
		TIFFPrint(fd, "  Subfile Type:");
		sep = " ";
		if (td->td_subfiletype & FILETYPE_REDUCEDIMAGE) {
			TIFFPrint(fd, "%sreduced-resolution image", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_PAGE) {
			TIFFPrint(fd, "%smulti-page document", sep);
			sep = "/";
		}
		if (td->td_subfiletype & FILETYPE_MASK)
			TIFFPrint(fd, "%stransparency mask", sep);
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_subfiletype, td->td_subfiletype);
	}
	if (TIFFFieldSet(tif,FIELD_IMAGEDIMENSIONS)) {
		TIFFPrint(fd, "  Image Width: %lu Image Length: %lu",
		    td->td_imagewidth, td->td_imagelength);
		if (TIFFFieldSet(tif,FIELD_IMAGEDEPTH))
			TIFFPrint(fd, " Image Depth: %lu", td->td_imagedepth);
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_TILEDIMENSIONS)) {
		TIFFPrint(fd, "  Tile Width: %lu Tile Length: %lu",
		    td->td_tilewidth, td->td_tilelength);
		if (TIFFFieldSet(tif,FIELD_TILEDEPTH))
			TIFFPrint(fd, " Tile Depth: %lu", td->td_tiledepth);
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_RESOLUTION)) {
		TIFFPrint(fd, "  Resolution: %g, %g",
		    td->td_xresolution, td->td_yresolution);
		if (TIFFFieldSet(tif,FIELD_RESOLUTIONUNIT)) {
			switch (td->td_resolutionunit) {
			case RESUNIT_NONE:
				TIFFPrint(fd, " (unitless)");
				break;
			case RESUNIT_INCH:
				TIFFPrint(fd, " pixels/inch");
				break;
			case RESUNIT_CENTIMETER:
				TIFFPrint(fd, " pixels/cm");
				break;
			default:
				TIFFPrint(fd, " (unit %u = 0x%x)",
				    td->td_resolutionunit,
				    td->td_resolutionunit);
				break;
			}
		}
		TIFFPrint(fd, "\n");
	}
	if (TIFFFieldSet(tif,FIELD_POSITION))
		TIFFPrint(fd, "  Position: %g, %g\n",
		    td->td_xposition, td->td_yposition);
	if (TIFFFieldSet(tif,FIELD_BITSPERSAMPLE))
		TIFFPrint(fd, "  Bits/Sample: %u\n", td->td_bitspersample);
	if (TIFFFieldSet(tif,FIELD_SAMPLEFORMAT)) {
		TIFFPrint(fd, "  Sample Format: ");
		switch (td->td_sampleformat) {
		case SAMPLEFORMAT_VOID:
			TIFFPrint(fd, "void\n");
			break;
		case SAMPLEFORMAT_INT:
			TIFFPrint(fd, "signed integer\n");
			break;
		case SAMPLEFORMAT_UINT:
			TIFFPrint(fd, "unsigned integer\n");
			break;
		case SAMPLEFORMAT_IEEEFP:
			TIFFPrint(fd, "IEEE floating point\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_sampleformat, td->td_sampleformat);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_COMPRESSION)) {
		TIFFPrint(fd, "  Compression Scheme: ");
		switch (td->td_compression) {
		case COMPRESSION_NONE:
			TIFFPrint(fd, "none\n");
			break;
		case COMPRESSION_CCITTRLE:
			TIFFPrint(fd, "CCITT modified Huffman encoding\n");
			break;
		case COMPRESSION_CCITTFAX3:
			TIFFPrint(fd, "CCITT Group 3 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTFAX4:
			TIFFPrint(fd, "CCITT Group 4 facsimile encoding\n");
			break;
		case COMPRESSION_CCITTRLEW:
			TIFFPrint(fd, "CCITT modified Huffman encoding %s\n",
			    "w/ word alignment");
			break;
		case COMPRESSION_PACKBITS:
			TIFFPrint(fd, "Macintosh PackBits encoding\n");
			break;
		case COMPRESSION_THUNDERSCAN:
			TIFFPrint(fd, "ThunderScan 4-bit encoding\n");
			break;
		case COMPRESSION_LZW:
			TIFFPrint(fd, "Lempel-Ziv & Welch encoding\n");
			break;
		case COMPRESSION_NEXT:
			TIFFPrint(fd, "NeXT 2-bit encoding\n");
			break;
		case COMPRESSION_JPEG:
			TIFFPrint(fd, "JPEG encoding\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_compression, td->td_compression);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PHOTOMETRIC)) {
		TIFFPrint(fd, "  Photometric Interpretation: ");
		if (td->td_photometric < NPHOTONAMES)
			TIFFPrint(fd, "%s\n", photoNames[td->td_photometric]);
		else
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_photometric, td->td_photometric);
	}
	if (TIFFFieldSet(tif,FIELD_MATTEING))
		TIFFPrint(fd, "  Matteing: %s\n", td->td_matteing ?
		    "pre-multiplied with alpha channel" : "none");
#ifdef CMYK_SUPPORT
	if (TIFFFieldSet(tif,FIELD_INKSET)) {
		TIFFPrint(fd, "  Ink Set: ");
		switch (td->td_inkset) {
		case INKSET_CMYK:
			TIFFPrint(fd, "CMYK\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_inkset, td->td_inkset);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_INKNAMES)) {
		char *cp;
		TIFFPrint(fd, "  Ink Names: ");
		i = td->td_samplesperpixel;
		sep = "";
		for (cp = td->td_inknames; i > 0; cp = strchr(cp, '\0')) {
			TIFFPrint(fd, "%s%s", sep, cp);
			sep = ", ";
		}
	}
	if (TIFFFieldSet(tif,FIELD_DOTRANGE))
		TIFFPrint(fd, "  Dot Range: %u-%u\n",
		    td->td_dotrange[0], td->td_dotrange[1]);
	if (TIFFFieldSet(tif,FIELD_TARGETPRINTER))
		TIFFPrint(fd, "  Target Printer: %s\n", td->td_targetprinter);
#endif
	if (TIFFFieldSet(tif,FIELD_THRESHHOLDING)) {
		TIFFPrint(fd, "  Thresholding: ");
		switch (td->td_threshholding) {
		case THRESHHOLD_BILEVEL:
			TIFFPrint(fd, "bilevel art scan\n");
			break;
		case THRESHHOLD_HALFTONE:
			TIFFPrint(fd, "halftone or dithered scan\n");
			break;
		case THRESHHOLD_ERRORDIFFUSE:
			TIFFPrint(fd, "error diffused\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_threshholding, td->td_threshholding);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_FILLORDER)) {
		TIFFPrint(fd, "  FillOrder: ");
		switch (td->td_fillorder) {
		case FILLORDER_MSB2LSB:
			TIFFPrint(fd, "msb-to-lsb\n");
			break;
		case FILLORDER_LSB2MSB:
			TIFFPrint(fd, "lsb-to-msb\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_fillorder, td->td_fillorder);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PREDICTOR)) {
		TIFFPrint(fd, "  Predictor: ");
		switch (td->td_predictor) {
		case 1:
			TIFFPrint(fd, "none\n");
			break;
		case 2:
			TIFFPrint(fd, "horizontal differencing\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_predictor, td->td_predictor);
			break;
		}
	}
#ifdef YCBCR_SUPPORT
	if (TIFFFieldSet(tif,FIELD_YCBCRSUBSAMPLING))
		TIFFPrint(fd, "  YCbCr Subsampling: %u, %u\n",
		    td->td_ycbcrsubsampling[0], td->td_ycbcrsubsampling[1]);
	if (TIFFFieldSet(tif,FIELD_YCBCRPOSITIONING)) {
		TIFFPrint(fd, "  YCbCr Positioning: ");
		switch (td->td_ycbcrpositioning) {
		case YCBCRPOSITION_CENTERED:
			TIFFPrint(fd, "centered\n");
			break;
		case YCBCRPOSITION_COSITED:
			TIFFPrint(fd, "cosited\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_ycbcrpositioning, td->td_ycbcrpositioning);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_YCBCRCOEFFICIENTS))
		TIFFPrint(fd, "  YCbCr Coefficients: %g, %g, %g\n",
		    td->td_ycbcrcoeffs[0],
		    td->td_ycbcrcoeffs[1],
		    td->td_ycbcrcoeffs[2]);
#endif
#ifdef JPEG_SUPPORT
	if (TIFFFieldSet(tif,FIELD_JPEGPROC)) {
		TIFFPrint(fd, "  JPEG Processing Mode: ");
		switch (td->td_jpegproc) {
		case JPEGPROC_BASELINE:
			TIFFPrint(fd, "baseline sequential algorithm\n");
			break;
		case JPEGPROC_LOSSLESS:
			TIFFPrint(fd, "lossless algorithm with Huffman coding\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_jpegproc, td->td_jpegproc);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_JPEGRESTARTINTERVAL)) {
		TIFFPrint(fd, "  JPEG Restart Interval: ");
		if (td->td_jpegrestartinterval)
			TIFFPrint(fd, "%u\n", td->td_jpegrestartinterval);
		else
			TIFFPrint(fd, "(no restart markers)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGQTABLES)) {
		TIFFPrint(fd, "  JPEG Quantization Tables: ");
		if (flags & TIFFPRINT_JPEGQTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintQTable(fd, td->td_qtab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGDCTABLES)) {
		TIFFPrint(fd, "  JPEG DC Tables: ");
		if (flags & TIFFPRINT_JPEGDCTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintCTable(fd, td->td_dctab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
	if (TIFFFieldSet(tif,FIELD_JPEGACTABLES)) {
		TIFFPrint(fd, "  JPEG AC Tables: ");
		if (flags & TIFFPRINT_JPEGACTABLES) {
			for (i = 0; i < td->td_samplesperpixel; i++)
				JPEGPrintCTable(fd, td->td_actab[i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
#endif
	if (TIFFFieldSet(tif,FIELD_HALFTONEHINTS))
		TIFFPrint(fd, "  Halftone Hints: light %u dark %u\n",
		    td->td_halftonehints[0], td->td_halftonehints[1]);
	if (TIFFFieldSet(tif,FIELD_ARTIST))
		TIFFPrint(fd, "  Artist: \"%s\"\n", td->td_artist);
	if (TIFFFieldSet(tif,FIELD_DATETIME))
		TIFFPrint(fd, "  Date & Time: \"%s\"\n", td->td_datetime);
	if (TIFFFieldSet(tif,FIELD_HOSTCOMPUTER))
		TIFFPrint(fd, "  Host Computer: \"%s\"\n", td->td_hostcomputer);
	if (TIFFFieldSet(tif,FIELD_SOFTWARE))
		TIFFPrint(fd, "  Software: \"%s\"\n", td->td_software);
	if (TIFFFieldSet(tif,FIELD_DOCUMENTNAME))
		TIFFPrint(fd, "  Document Name: \"%s\"\n", td->td_documentname);
	if (TIFFFieldSet(tif,FIELD_IMAGEDESCRIPTION))
		TIFFPrint(fd, "  Image Description: \"%s\"\n",
		    td->td_imagedescription);
	if (TIFFFieldSet(tif,FIELD_MAKE))
		TIFFPrint(fd, "  Make: \"%s\"\n", td->td_make);
	if (TIFFFieldSet(tif,FIELD_MODEL))
		TIFFPrint(fd, "  Model: \"%s\"\n", td->td_model);
	if (TIFFFieldSet(tif,FIELD_ORIENTATION)) {
		TIFFPrint(fd, "  Orientation: ");
		if (td->td_orientation < NORIENTNAMES)
			TIFFPrint(fd, "%s\n", orientNames[td->td_orientation]);
		else
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_orientation, td->td_orientation);
	}
	if (TIFFFieldSet(tif,FIELD_SAMPLESPERPIXEL))
		TIFFPrint(fd, "  Samples/Pixel: %u\n", td->td_samplesperpixel);
	if (TIFFFieldSet(tif,FIELD_ROWSPERSTRIP)) {
		TIFFPrint(fd, "  Rows/Strip: ");
		if (td->td_rowsperstrip == 0xffffffffL)
			TIFFPrint(fd, "(infinite)\n");
		else
			TIFFPrint(fd, "%u\n", td->td_rowsperstrip);
	}
	if (TIFFFieldSet(tif,FIELD_MINSAMPLEVALUE))
		TIFFPrint(fd, "  Min Sample Value: %u\n", td->td_minsamplevalue);
	if (TIFFFieldSet(tif,FIELD_MAXSAMPLEVALUE))
		TIFFPrint(fd, "  Max Sample Value: %u\n", td->td_maxsamplevalue);
	if (TIFFFieldSet(tif,FIELD_PLANARCONFIG)) {
		TIFFPrint(fd, "  Planar Configuration: ");
		switch (td->td_planarconfig) {
		case PLANARCONFIG_CONTIG:
			TIFFPrint(fd, "single image plane\n");
			break;
		case PLANARCONFIG_SEPARATE:
			TIFFPrint(fd, "separate image planes\n");
			break;
		default:
			TIFFPrint(fd, "%u (0x%x)\n",
			    td->td_planarconfig, td->td_planarconfig);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_PAGENAME))
		TIFFPrint(fd, "  Page Name: \"%s\"\n", td->td_pagename);
	if (TIFFFieldSet(tif,FIELD_GROUP3OPTIONS)) {
		TIFFPrint(fd, "  Group 3 Options:");
		sep = " ";
		if (td->td_group3options & GROUP3OPT_2DENCODING)
			TIFFPrint(fd, "%s2-d encoding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_FILLBITS)
			TIFFPrint(fd, "%sEOL padding", sep), sep = "+";
		if (td->td_group3options & GROUP3OPT_UNCOMPRESSED)
			TIFFPrint(fd, "%suncompressed data", sep);
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_group3options, td->td_group3options);
	}
	if (TIFFFieldSet(tif,FIELD_CLEANFAXDATA)) {
		TIFFPrint(fd, "  Fax Data: ");
		switch (td->td_cleanfaxdata) {
		case CLEANFAXDATA_CLEAN:
			TIFFPrint(fd, "clean\n");
			break;
		case CLEANFAXDATA_REGENERATED:
			TIFFPrint(fd, "receiver regenerated\n");
			break;
		case CLEANFAXDATA_UNCLEAN:
			TIFFPrint(fd, "uncorrected errors\n");
			break;
		default:
			TIFFPrint(fd, "(%u = 0x%x)\n",
			    td->td_cleanfaxdata, td->td_cleanfaxdata);
			break;
		}
	}
	if (TIFFFieldSet(tif,FIELD_BADFAXLINES))
		TIFFPrint(fd, "  Bad Fax Lines: %u\n", td->td_badfaxlines);
	if (TIFFFieldSet(tif,FIELD_BADFAXRUN))
		TIFFPrint(fd, "  Consecutive Bad Fax Lines: %u\n",
		    td->td_badfaxrun);
	if (TIFFFieldSet(tif,FIELD_GROUP4OPTIONS)) {
		TIFFPrint(fd, "  Group 4 Options:");
		if (td->td_group4options & GROUP4OPT_UNCOMPRESSED)
			TIFFPrint(fd, "uncompressed data");
		TIFFPrint(fd, " (%u = 0x%x)\n",
		    td->td_group4options, td->td_group4options);
	}
	if (TIFFFieldSet(tif,FIELD_PAGENUMBER))
		TIFFPrint(fd, "  Page Number: %u-%u\n",
		    td->td_pagenumber[0], td->td_pagenumber[1]);
	if (TIFFFieldSet(tif,FIELD_COLORMAP)) {
		TIFFPrint(fd, "  Color Map: ");
		if (flags & TIFFPRINT_COLORMAP) {
			TIFFPrint(fd, "\n");
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++)
				TIFFPrint(fd, "   %5d: %5u %5u %5u\n",
				    i,
				    td->td_colormap[0][i],
				    td->td_colormap[1][i],
				    td->td_colormap[2][i]);
		} else
			TIFFPrint(fd, "(present)\n");
	}
#ifdef COLORIMETRY_SUPPORT
	if (TIFFFieldSet(tif,FIELD_WHITEPOINT))
		TIFFPrint(fd, "  White Point: %g-%g\n",
		    td->td_whitepoint[0], td->td_whitepoint[1]);
	if (TIFFFieldSet(tif,FIELD_PRIMARYCHROMAS))
		TIFFPrint(fd, "  Primary Chromaticities: %g,%g %g,%g %g,%g\n",
		    td->td_primarychromas[0], td->td_primarychromas[1],
		    td->td_primarychromas[2], td->td_primarychromas[3],
		    td->td_primarychromas[4], td->td_primarychromas[5]);
	if (TIFFFieldSet(tif,FIELD_REFBLACKWHITE)) {
		TIFFPrint(fd, "  Reference Black/White:\n");
		for (i = 0; i < (int)td->td_samplesperpixel; i++)
			TIFFPrint(fd, "    %2d: %5g %5g\n",
			    i,
			    td->td_refblackwhite[2*i+0],
			    td->td_refblackwhite[2*i+1]);
	}
	if (TIFFFieldSet(tif,FIELD_TRANSFERFUNCTION)) {
		TIFFPrint(fd, "  Transfer Function: ");
		if (flags & TIFFPRINT_CURVES) {
			TIFFPrint(fd, "\n");
			n = 1L<<td->td_bitspersample;
			for (i = 0; i < n; i++) {
				TIFFPrint(fd, "    %2d: %5u",
				    i, td->td_transferfunction[0][i]);
				for (j = 1; j < (int)td->td_samplesperpixel; j++)
					TIFFPrint(fd, " %5u",
					    td->td_transferfunction[j][i]);
				putc('\n', fd);
			}
		} else
			TIFFPrint(fd, "(present)\n");
	}
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	{
		long deg,min;
		float sec;

#define GET_DEG_MIN_SEC(s1000) \
			deg=(s1000)/36e5; \
			min=(s1000)/6e4 - deg*60; \
			sec=(float)(s1000)/1000 - deg*3600-min*60;
		
		if (TIFFhasCartoTags(tif))
		{
			/* Print the values in the private IFD */
			if (TIFFFieldSet(tif,FIELD_CARTO_IFD_OFFSET))
				TIFFPrint(fd, "  Cartographic Directory Offset at  0x%x:\n",
					td->td_carto_ifd_offset);
			else
				TIFFPrint(fd, "  Cartographic Data Derived from Old Method:\n");
			TIFFPrint(fd, "  \tProjection Type: %s\n",
				(td->td_projectiontype==0)? "UTM":"???");
			TIFFPrint(fd, "  \tReference X Position: %lu pixels from left\n",
				td->td_proj_xpos);
			TIFFPrint(fd, "  \tReference Y Position: %lu pixels from top\n",
				td->td_proj_ypos);
			GET_DEG_MIN_SEC(td->td_latitude)
			TIFFPrint(fd, "  \tReference Latitude  : (%d Deg, %d Min, %g Sec) from S. Pole\n",
				deg,min,sec);
			GET_DEG_MIN_SEC(td->td_longitude)
			TIFFPrint(fd, "  \tReference Longitude : (%d Deg, %d Min, %g Sec) from -180.\n",
				deg,min,sec);
			GET_DEG_MIN_SEC((1/td->td_xpixperangle))
			TIFFPrint(fd, "  \tX Map Resolution:     (%d Deg, %d Min, %g Sec) per Pixel\n",
				deg,min,sec);
			GET_DEG_MIN_SEC((1/td->td_ypixperangle))
			TIFFPrint(fd, "  \tY Map Resolution:     (%d Deg, %d Min, %g Sec) per Pixel\n",
				deg,min,sec);
		}
	}
#endif
	if ((flags & TIFFPRINT_STRIPS) &&
	    TIFFFieldSet(tif,FIELD_STRIPOFFSETS)) {
		TIFFPrint(fd, "  %u %s:\n",
		    td->td_nstrips,
		    isTiled(tif) ? "Tiles" : "Strips");
		for (i = 0; i < td->td_nstrips; i++)
			TIFFPrint(fd, "    %3d: [%8u, %8u]\n",
			    i, td->td_stripoffset[i], td->td_stripbytecount[i]);
	}
}
