/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiffiop.h,v 1.28 92/03/06 11:59:52 sam Exp $ */

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

#ifndef _TIFFIOP_
#define	_TIFFIOP_
/*
 * ``Library-private'' definitions.
 */
#include "tiffcompat.h"
#include "tiff.h"


/*
 * Internal format of a TIFF directory entry.
 */
typedef	struct {
	u_long	td_fieldsset[3];	/* bit vector of fields that are set */

	u_long	td_imagewidth, td_imagelength, td_imagedepth;
	u_long	td_tilewidth, td_tilelength, td_tiledepth;
	u_short	td_subfiletype;
	u_short	td_bitspersample;
	u_short	td_sampleformat;
	u_short	td_compression;
	u_short	td_photometric;
	u_short	td_threshholding;
	u_short	td_fillorder;
	u_short	td_orientation;
	u_short	td_samplesperpixel;
	u_short	td_predictor;
	u_long	td_rowsperstrip;
	u_long	td_minsamplevalue, td_maxsamplevalue;	/* XXX */
	float	td_xresolution, td_yresolution;
	u_short	td_resolutionunit;
	u_short	td_planarconfig;
	float	td_xposition, td_yposition;
	u_long	td_group3options;
	u_long	td_group4options;
	u_short	td_pagenumber[2];
	u_short	td_matteing;
	u_short	td_cleanfaxdata;
	u_short	td_badfaxrun;
	u_long	td_badfaxlines;
	u_short	*td_colormap[3];
	u_short	td_halftonehints[2];
	char	*td_documentname;
	char	*td_artist;
	char	*td_datetime;
	char	*td_hostcomputer;
	char	*td_imagedescription;
	char	*td_make;
	char	*td_model;
	char	*td_software;
	char	*td_pagename;
	u_long	td_stripsperimage;
	u_long	td_nstrips;		/* size of offset & bytecount arrays */
	u_long	*td_stripoffset;
	u_long	*td_stripbytecount;
#ifdef YCBCR_SUPPORT
	float	*td_ycbcrcoeffs;
	u_short	td_ycbcrsubsampling[2];
	u_short	td_ycbcrpositioning;
#endif
#ifdef JPEG_SUPPORT
	u_short	td_jpegproc;
	u_short	td_jpegrestartinterval;
	u_char	**td_qtab;
	u_char	**td_dctab;
	u_char	**td_actab;
#endif
#ifdef COLORIMETRY_SUPPORT
	float	*td_whitepoint;
	float	*td_primarychromas;
	float	*td_refblackwhite;
	u_short	*td_transferfunction[4];
#endif
#ifdef CMYK_SUPPORT
	u_short	td_inkset;
	u_short	td_dotrange[2];
	char	*td_inknames;
	char	*td_targetprinter;
#endif
#ifdef CARTOGRAPHIC_SUPPORT
	u_long	td_carto_ifd_offset; /* the only public tag */
	u_short	td_projectiontype;
	u_long	td_proj_xpos;
	u_long	td_proj_ypos;
	u_long	td_latitude;
	u_long	td_longitude;
	float	td_xpixperangle;
	float	td_ypixperangle;
#endif
} TIFFDirectory;

/*
 * Field flags used to indicate fields that have
 * been set in a directory, and to reference fields
 * when manipulating a directory.
 */
/* multi-entry fields */
#define	FIELD_IMAGEDIMENSIONS		0
#define FIELD_TILEDIMENSIONS		1
#define	FIELD_CELLDIMENSIONS		2		/* XXX */
#define	FIELD_RESOLUTION		3
#define	FIELD_POSITION			4
/* single-entry fields */
#define	FIELD_SUBFILETYPE		5
#define	FIELD_BITSPERSAMPLE		6
#define	FIELD_COMPRESSION		7
#define	FIELD_PHOTOMETRIC		8
#define	FIELD_THRESHHOLDING		9
#define	FIELD_FILLORDER			10
#define	FIELD_DOCUMENTNAME		11
#define	FIELD_IMAGEDESCRIPTION		12
#define	FIELD_MAKE			13
#define	FIELD_MODEL			14
#define	FIELD_ORIENTATION		15
#define	FIELD_SAMPLESPERPIXEL		16
#define	FIELD_ROWSPERSTRIP		17
#define	FIELD_MINSAMPLEVALUE		18
#define	FIELD_MAXSAMPLEVALUE		19
#define	FIELD_PLANARCONFIG		20
#define	FIELD_PAGENAME			21
#define	FIELD_GROUP3OPTIONS		22
#define	FIELD_GROUP4OPTIONS		23
#define	FIELD_RESOLUTIONUNIT		24
#define	FIELD_PAGENUMBER		25
#define	FIELD_STRIPBYTECOUNTS		26
#define	FIELD_STRIPOFFSETS		27
#define	FIELD_COLORMAP			28
#define FIELD_PREDICTOR			29
#define FIELD_ARTIST			30
#define FIELD_DATETIME			31
#define FIELD_HOSTCOMPUTER		32
#define FIELD_SOFTWARE			33
#define	FIELD_MATTEING			34
#define	FIELD_BADFAXLINES		35
#define	FIELD_CLEANFAXDATA		36
#define	FIELD_BADFAXRUN			37
#define FIELD_SAMPLEFORMAT		38
#define	FIELD_SMINSAMPLEVALUE		39
#define	FIELD_SMAXSAMPLEVALUE		40
#define FIELD_IMAGEDEPTH		41
#define FIELD_TILEDEPTH			42
#define	FIELD_HALFTONEHINTS		43
#ifdef YCBCR_SUPPORT
#define FIELD_YCBCRCOEFFICIENTS		44
#define FIELD_YCBCRSUBSAMPLING		45
#define FIELD_YCBCRPOSITIONING		46
#endif
#ifdef JPEG_SUPPORT
#define FIELD_JPEGPROC			47
#define FIELD_JPEGRESTARTINTERVAL	48
#define FIELD_JPEGQTABLES		49
#define FIELD_JPEGDCTABLES		50
#define FIELD_JPEGACTABLES		51
#endif
#ifdef COLORIMETRY_SUPPORT
#define	FIELD_REFBLACKWHITE		52
#define	FIELD_WHITEPOINT		53
#define	FIELD_PRIMARYCHROMAS		54
#define	FIELD_TRANSFERFUNCTION		55
#endif
#ifdef CMYK_SUPPORT
#define	FIELD_INKSET			56
#define	FIELD_INKNAMES			57
#define	FIELD_DOTRANGE			58
#define	FIELD_TARGETPRINTER		59
#endif
#ifdef CARTOGRAPHIC_SUPPORT
#define	FIELD_CARTO_IFD_OFFSET	60   /* The only cart field that is public */
#endif
#define	FIELD_LAST				61

/* Private Cartographic fields */
#ifdef CARTOGRAPHIC_SUPPORT
#define CARTFIELD_FIRST FIELD_LAST+1
#define	FIELD_PROJECTIONTYPE	CARTFIELD_FIRST
#define	FIELD_PROJ_XPOS			CARTFIELD_FIRST+1	
#define	FIELD_PROJ_YPOS			CARTFIELD_FIRST+2	
#define	FIELD_LATITUDE			CARTFIELD_FIRST+3	
#define	FIELD_LONGITUDE			CARTFIELD_FIRST+4	
#define	FIELD_XPIXPERANGLE		CARTFIELD_FIRST+5	
#define	FIELD_YPIXPERANGLE		CARTFIELD_FIRST+6
#define CARTFIELD_LAST			CARTFIELD_FIRST+7
#endif /* CARTO */

#define	TIFFExtractData(tif, type, v) \
    ((tif)->tif_header.tiff_magic == TIFF_BIGENDIAN ? \
        ((v) >> (tif)->tif_typeshift[type]) & (tif)->tif_typemask[type] : \
	(v) & (tif)->tif_typemask[type])
#define	TIFFInsertData(tif, type, v) \
    ((tif)->tif_header.tiff_magic == TIFF_BIGENDIAN ? \
        ((v) & (tif)->tif_typemask[type]) << (tif)->tif_typeshift[type] : \
	(v) & (tif)->tif_typemask[type])

typedef	struct {
	u_short	field_tag;		/* field's tag */
	short	field_readcount;	/* read count (-1 for unknown) */
	short	field_writecount;	/* write count (-1 for unknown) */
	TIFFDataType field_type;	/* type of associated data */
	u_short	field_bit;		/* bit in fieldsset bit vector */
	u_short	field_oktochange;	/* if true, can change while writing */
	char	*field_name;		/* ASCII name */
} TIFFFieldInfo;

#define	FIELD_IGNORE	((u_short)-1)	/* tags processed but ignored */

#define	TIFF_ANY	TIFF_NOTYPE	/* for field descriptor searching */
#define	TIFF_VARIABLE	-1		/* marker for variable length tags */
#define	TIFF_SPP	-2		/* marker for SamplesPerPixel tags */

extern	const TIFFFieldInfo tiffFieldInfo[];/* table of field descriptors */
extern	const int tiffDataWidth[];	/* table of tag datatype widths */
#ifdef CARTOGRAPHIC_SUPPORT
extern	const TIFFFieldInfo cartFieldInfo[];/* table of field descriptors */
#endif

#define BITn(n)				(((unsigned long)1L)<<((n)&0x1f)) 
#define BITFIELDn(tif, n)		((tif)->tif_dir.td_fieldsset[(n)/32]) 
#define TIFFFieldSet(tif, field)	(BITFIELDn(tif, field) & BITn(field)) 
#define TIFFSetFieldBit(tif, field)	(BITFIELDn(tif, field) |= BITn(field))
#define TIFFClrFieldBit(tif, field)	(BITFIELDn(tif, field) &= ~BITn(field))

#define	FieldSet(fields, f)		(fields[(f)/32] & BITn(f))
#define	ResetFieldBit(fields, f)	(fields[(f)/32] &= ~BITn(f))

struct tiff {
	char	*tif_name;		/* name of open file */
	short	tif_fd;			/* open file descriptor */
	short	tif_mode;		/* open mode (O_*) */
	char	tif_fillorder;		/* natural bit fill order for machine */
	char	tif_options;		/* compression-specific options */
	short	tif_flags;
#define	TIFF_DIRTYHEADER	0x1	/* header must be written on close */
#define	TIFF_DIRTYDIRECT	0x2	/* current directory must be written */
#define	TIFF_BUFFERSETUP	0x4	/* data buffers setup */
#define	TIFF_BEENWRITING	0x8	/* written 1+ scanlines to file */
#define	TIFF_SWAB		0x10	/* byte swap file information */
#define	TIFF_NOBITREV		0x20	/* inhibit bit reversal logic */
#define	TIFF_MYBUFFER		0x40	/* my raw data buffer; free on close */
#define	TIFF_ISTILED		0x80	/* file is tile, not strip- based */
#define	TIFF_MAPPED		0x100	/* file is mapped into memory */
#define	TIFF_POSTENCODE		0x200	/* need call to postencode routine */
#ifdef CARTOGRAPHIC_SUPPORT
#define	TIFF_DIRTYCART		0x400	/* CART Directory must be written */
#define	TIFF_HASCARTTAGS	0x800	/* CART Information Present */
#endif
	long	tif_diroff;		/* file offset of current directory */
	long	tif_nextdiroff;		/* file offset of following directory */
	TIFFDirectory tif_dir;		/* internal rep of current directory */
	TIFFHeader tif_header;		/* file's header block */
	int const *tif_typeshift;	/* data type shift counts */
	long const *tif_typemask;	/* data type masks */
	long	tif_row;		/* current scanline */
	int	tif_curdir;		/* current directory (index) */
	int	tif_curstrip;		/* current strip for read/write */
	long	tif_curoff;		/* current offset for read/write */
/* tiling support */
	long 	tif_col;		/* current column (offset by row too) */
	int 	tif_curtile;		/* current tile for read/write */
	long 	tif_tilesize;		/* # of bytes in a tile */
/* compression scheme hooks */
#if USE_PROTOTYPES
	int	(*tif_predecode)(struct tiff *tif);						/* pre row/strip/tile decoding */
	int	(*tif_preencode)(struct tiff *tif);						/* pre row/strip/tile encoding */
	int	(*tif_postencode)(struct tiff *tif);					/* post row/strip/tile encoding */
	int	(*tif_decoderow)(struct tiff *, u_char*, u_long, u_int);	/* scanline decoding routine */
	int	(*tif_encoderow)(struct tiff *, u_char*, u_long, u_int);	/* scanline encoding routine */
	int	(*tif_decodestrip)(struct tiff *, u_char*, u_long, u_int);	/* strip decoding routine */
	int	(*tif_encodestrip)(struct tiff *, u_char*, u_long, u_int);	/* strip encoding routine */
	int	(*tif_decodetile)(struct tiff *, u_char*, u_long, u_int);	/* tile decoding routine */
	int	(*tif_encodetile)(struct tiff *, u_char*, u_long, u_int);	/* tile encoding routine */
	int	(*tif_close)(struct tiff *tif);							/* cleanup-on-close routine */
	int	(*tif_seek)(struct tiff *tif, int);						/* position within a strip routine */
	int	(*tif_cleanup)(struct tiff *tif);						/* routine called to cleanup state */
#else
	int	(*tif_predecode)();	/* pre row/strip/tile decoding */
	int	(*tif_preencode)();	/* pre row/strip/tile encoding */
	int	(*tif_postencode)();	/* post row/strip/tile encoding */
	int	(*tif_decoderow)();	/* scanline decoding routine */
	int	(*tif_encoderow)();	/* scanline encoding routine */
	int	(*tif_decodestrip)();	/* strip decoding routine */
	int	(*tif_encodestrip)();	/* strip encoding routine */
	int	(*tif_decodetile)();	/* tile decoding routine */
	int	(*tif_encodetile)();	/* tile encoding routine */
	int	(*tif_close)();		/* cleanup-on-close routine */
	int	(*tif_seek)();		/* position within a strip routine */
	int	(*tif_cleanup)();	/* routine called to cleanup state */
#endif /* PROTOTYPES */
	char	*tif_data;		/* compression scheme private data */
/* input/output buffering */
	u_long	tif_scanlinesize;	/* # of bytes in a scanline */
	int	tif_scanlineskew;	/* scanline skew for reading strips */
	char	*tif_rawdata;		/* raw data buffer */
	long	tif_rawdatasize;	/* # of bytes in raw data buffer */
	char	*tif_rawcp;		/* current spot in raw buffer */
	long	tif_rawcc;		/* bytes unread from raw buffer */
#ifdef MMAP_SUPPORT
	char*	tif_base;		/* base of mapped file */
	long	tif_size;		/* size of mapped file region (bytes) */
#endif
};

#define	isTiled(tif)	(((tif)->tif_flags & TIFF_ISTILED) != 0)
#ifdef MMAP_SUPPORT
#define	isMapped(tif)	(((tif)->tif_flags & TIFF_MAPPED) != 0)
#else
#define	isMapped(tif)	0		/* force dead code */
#endif

/* generic option bit names */
#define	TIFF_OPT0	0x1
#define	TIFF_OPT1	0x2
#define	TIFF_OPT2	0x4
#define	TIFF_OPT3	0x8
#define	TIFF_OPT4	0x10
#define	TIFF_OPT5	0x20
#define	TIFF_OPT6	0x40
#define	TIFF_OPT7	0x80

#include "tiffio.h"

/* NB: the u_long casts are to silence certain ANSI-C compilers */
#ifdef howmany
#undef howmany
#endif
#define	howmany(x, y)	((((u_long)(x))+(((u_long)(y))-1))/((u_long)(y)))
#ifdef roundup
#undef roundup
#endif
#define	roundup(x, y)	(howmany(x,y)*((u_long)(y)))

#if defined(c_plusplus) || defined(__cplusplus) || defined(__STDC__) || USE_PROTOTYPES
#if defined(__cplusplus)
extern "C" {
#endif
extern  int TIFFWriteTIFFHdr( int fd, TIFFHeader *hdr );
extern	TIFFFieldInfo const *TIFFFindFieldInfo(u_short, TIFFDataType);
extern	TIFFFieldInfo const *TIFFFieldWithTag(u_short);
extern	int TIFFFlushData1(TIFF*);
extern	int TIFFSetCompressionScheme(TIFF*, int);
extern	int _TIFFgetfield(TIFFDirectory*, int, ...);
extern	int TIFFNoRowEncode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoStripEncode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoTileEncode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoRowDecode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoStripDecode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoTileDecode(TIFF*, u_char*, u_long, u_int);
extern	int TIFFNoDecode(TIFF *, char *);
#if defined(__cplusplus)
}
#endif
#else
extern  int TIFFWriteTIFFHdr();
extern	TIFFFieldInfo const *TIFFFindFieldInfo();
extern	TIFFFieldInfo const *TIFFFieldWithTag();
extern	int TIFFFlushData1();
extern	int TIFFSetCompressionScheme();
extern	int _TIFFgetfield();
extern	int TIFFNoRowEncode();
extern	int TIFFNoStripEncode();
extern	int TIFFNoTileEncode();
extern	int TIFFNoRowDecode();
extern	int TIFFNoStripDecode();
extern	int TIFFNoTileDecode();
extern	int TIFFNoDecode();
#endif
#endif /* _TIFFIOP_ */
