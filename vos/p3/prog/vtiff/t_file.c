#ifndef lint
static char rcsid[] = "$Header: /usr/people/sam/tiff/libtiff/RCS/tif_write.c,v 1.41 92/02/10 19:06:47 sam Exp $";
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
 * Scanline-oriented Write Support
 */
#include "tiffiop.h"
#include "prototypes.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>

#define	STRIPINCR	20		/* expansion factor on strip array */

#if USE_PROTOTYPES
static	TIFFWriteCheck(TIFF *, int, char []);
static	TIFFBufferSetup(TIFF *, char []);
static	TIFFGrowStrips(TIFF *, int, char []);
static	TIFFAppendToStrip(TIFF *, u_int, u_char *, u_long);
extern	int TIFFFlushData1(register TIFF *);
#else
static	TIFFWriteCheck();
static	TIFFBufferSetup();
static	TIFFGrowStrips();
static	TIFFAppendToStrip();
extern	int TIFFFlushData1();
#endif

/** tif_flush **/
#if USE_PROTOTYPES
extern int TIFFFlushData1(TIFF*);
#else
extern int TIFFFlushData1();
#endif

/** end tif_flush **/


/**  tif_read **/


#if USE_PROTOTYPES
static	TIFFSeek(TIFF *, u_int, u_int);
static	long TIFFReadRawStrip1(TIFF *, u_int, u_char *, u_long, char []);
static	long TIFFReadRawTile1(TIFF *, u_int, u_char *, u_long, char []);
static	TIFFFillStrip(TIFF *, u_int);
static	TIFFFillTile(TIFF *, u_int);
static	TIFFStartStrip(TIFF *, u_int);
static	TIFFStartTile(TIFF *, u_int);
static	TIFFCheckRead(TIFF *, int);
#else
static	TIFFSeek();
static	long TIFFReadRawStrip1();
static	long TIFFReadRawTile1();
static	TIFFFillStrip();
static	TIFFFillTile();
static	TIFFStartStrip();
static	TIFFStartTile();
static	TIFFCheckRead();
#endif

/** end tif_read **/

/*VARARGS3*/
TIFFWriteScanline(tif, buf, row, sample)
	register TIFF *tif;
	u_char *buf;
	u_int row, sample;
{
	static char module[] = "TIFFWriteScanline";
	register TIFFDirectory *td;
	int strip, status, imagegrew = 0;

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized more intelligently (using
	 * directory information).
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	td = &tif->tif_dir;
	/*
	 * Extend image length if needed
	 * (but only for PlanarConfig=1).
	 */
	if (row >= td->td_imagelength) {	/* extend image */
		if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
			TIFFError(tif->tif_name,
		"Can not change \"ImageLength\" when using separate planes");
			return (-1);
		}
		td->td_imagelength = row+1;
		imagegrew = 1;
	}
	/*
	 * Calculate strip and check for crossings.
	 */
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
		if (sample >= td->td_samplesperpixel) {
			TIFFError(tif->tif_name,
			    "%d: Sample out of range, max %d",
			    sample, td->td_samplesperpixel);
			return (-1);
		}
		strip = sample*td->td_stripsperimage + row/td->td_rowsperstrip;
	} else
		strip = row / td->td_rowsperstrip;
	if (strip != tif->tif_curstrip) {
		/*
		 * Changing strips -- flush any data present.
		 */
		if (tif->tif_rawcc > 0 && !TIFFFlushData(tif))
			return (-1);
		tif->tif_curstrip = strip;
		/*
		 * Watch out for a growing image.  The value of
		 * strips/image will initially be 1 (since it
		 * can't be deduced until the imagelength is known).
		 */
		if (strip >= td->td_stripsperimage && imagegrew)
			td->td_stripsperimage =
			    howmany(td->td_imagelength, td->td_rowsperstrip);
		tif->tif_row =
		    (strip % td->td_stripsperimage) * td->td_rowsperstrip;
		if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
			return (-1);
		tif->tif_flags |= TIFF_POSTENCODE;
	}
	/*
	 * Check strip array to make sure there's space.
	 * We don't support dynamically growing files that
	 * have data organized in separate bitplanes because
	 * it's too painful.  In that case we require that
	 * the imagelength be set properly before the first
	 * write (so that the strips array will be fully
	 * allocated above).
	 */
	if (strip >= td->td_nstrips && !TIFFGrowStrips(tif, 1, module))
		return (-1);
	/*
	 * Ensure the write is either sequential or at the
	 * beginning of a strip (or that we can randomly
	 * access the data -- i.e. no encoding).
	 */
	if (row != tif->tif_row) {
		if (tif->tif_seek) {
			if (row < tif->tif_row) {
				/*
				 * Moving backwards within the same strip:
				 * backup to the start and then decode
				 * forward (below).
				 */
				tif->tif_row = (strip % td->td_stripsperimage) *
				    td->td_rowsperstrip;
				tif->tif_rawcp = tif->tif_rawdata;
			}
			/*
			 * Seek forward to the desired row.
			 */
			if (!(*tif->tif_seek)(tif, row - tif->tif_row))
				return (-1);
			tif->tif_row = row;
		} else {
			TIFFError(tif->tif_name,
		    "Compression algorithm does not support random access");
			return (-1);
		}
	}
	status = (*tif->tif_encoderow)(tif, buf, tif->tif_scanlinesize, sample);
	tif->tif_row++;
	return (status);
}

/*
 * Encode the supplied data and write it to the
 * specified strip.  There must be space for the
 * data; we don't check if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteEncodedStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	u_long cc;
{
	static char module[] = "TIFFWriteEncodedStrip";
	TIFFDirectory *td = &tif->tif_dir;

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	if (strip >= td->td_nstrips) {
		TIFFError(module, "%s: Strip %d out of range, max %d",
		    tif->tif_name, strip, td->td_nstrips);
		return (-1);
	}
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized according to the directory
	 * info.
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	tif->tif_curstrip = strip;
	tif->tif_flags &= ~TIFF_POSTENCODE;
	if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
		return (-1);
	if (!(*tif->tif_encodestrip)(tif,
	    data, cc, strip / td->td_stripsperimage))
		return (0);
	if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
		return (-1);
	if (td->td_fillorder != tif->tif_fillorder &&
	    (tif->tif_flags & TIFF_NOBITREV) == 0)
		TIFFReverseBits((u_char *)tif->tif_rawdata, tif->tif_rawcc);
	if (tif->tif_rawcc > 0 &&
	    !TIFFAppendToStrip(tif, strip, (u_char *)tif->tif_rawdata, tif->tif_rawcc))
		return (-1);
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (cc);
}

/*
 * Write the supplied data to the specified strip.
 * There must be space for the data; we don't check
 * if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteRawStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	u_long cc;
{
	static char module[] = "TIFFWriteRawStrip";

	if (!TIFFWriteCheck(tif, 0, module))
		return (-1);
	if (strip >= tif->tif_dir.td_nstrips) {
		TIFFError(module, "%s: Strip %d out of range, max %d",
		    tif->tif_name, strip, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFAppendToStrip(tif, strip, data, cc) ? cc : -1);
}

/*
 * Write and compress a tile of data.  The
 * tile is selected by the (x,y,z,s) coordinates.
 */
TIFFWriteTile(tif, buf, x, y, z, s)
	TIFF *tif;
	u_char *buf;
	u_long x, y, z;
	u_int s;
{
	if (!TIFFCheckTile(tif, x, y, z, s))
		return (-1);
	/*
	 * NB: A tile size of -1 is used instead of tif_tilesize knowing
	 *     that TIFFWriteEncodedTile will clamp this to the tile size.
	 *     This is done because the tile size may not be defined until
	 *     after the output buffer is setup in TIFFBufferSetup.
	 */
	return (TIFFWriteEncodedTile(tif,
	    TIFFComputeTile(tif, x, y, z, s), buf, (u_long)-1));
}

/*
 * Encode the supplied data and write it to the
 * specified tile.  There must be space for the
 * data.  The function clamps individual writes
 * to a tile to the tile size, but does not (and
 * can not) check that multiple writes to the same
 * tile do not write more than tile size data.
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteEncodedTile(tif, tile, data, cc)
	TIFF *tif;
	u_int tile;
	u_char *data;
	u_long cc;
{
	static char module[] = "TIFFWriteEncodedTile";
	TIFFDirectory *td;

	if (!TIFFWriteCheck(tif, 1, module))
		return (-1);
	td = &tif->tif_dir;
	if (tile >= td->td_nstrips) {
		TIFFError(module, "%s: Tile %d out of range, max %d",
		    tif->tif_name, tile, td->td_nstrips);
		return (-1);
	}
	/*
	 * Handle delayed allocation of data buffer.  This
	 * permits it to be sized more intelligently (using
	 * directory information).
	 */
	if ((tif->tif_flags & TIFF_BUFFERSETUP) == 0) {
		if (!TIFFBufferSetup(tif, module))
			return (-1);
		tif->tif_flags |= TIFF_BUFFERSETUP;
	}
	tif->tif_curtile = tile;
	/* 
	 * Compute tiles per row & per column to compute
	 * current row and column
	 */
    /* ndr -- fixed mathematical error in row computation - cf tif_read */
	tif->tif_row = (tile / howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilelength;
	tif->tif_col = (tile % howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilewidth;

	tif->tif_flags &= ~TIFF_POSTENCODE;
	if (tif->tif_preencode && !(*tif->tif_preencode)(tif))
		return (-1);
	/*
	 * Clamp write amount to the tile size.  This is mostly
	 * done so that callers can pass in some large number
	 * (e.g. -1) and have the tile size used instead.
	 */
	if (cc > tif->tif_tilesize)
		cc = tif->tif_tilesize;
	if (!(*tif->tif_encodetile)(tif, data, cc, tile/td->td_stripsperimage))
		return (0);
	if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
		return (-1);
	if (td->td_fillorder != tif->tif_fillorder &&
	    (tif->tif_flags & TIFF_NOBITREV) == 0)
		TIFFReverseBits((u_char *)tif->tif_rawdata, tif->tif_rawcc);
	if (tif->tif_rawcc > 0 && !TIFFAppendToStrip(tif, tile,
	    (u_char *)tif->tif_rawdata, tif->tif_rawcc))
		return (-1);
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (cc);
}

/*
 * Write the supplied data to the specified strip.
 * There must be space for the data; we don't check
 * if strips overlap!
 *
 * NB: Image length must be setup before writing; this
 *     interface does not support automatically growing
 *     the image on each write (as TIFFWriteScanline does).
 */
long
TIFFWriteRawTile(tif, tile, data, cc)
	TIFF *tif;
	u_int tile;
	u_char *data;
	u_long cc;
{
	static char module[] = "TIFFWriteRawTile";

	if (!TIFFWriteCheck(tif, 1, module))
		return (-1);
	if (tile >= tif->tif_dir.td_nstrips) {
		TIFFError(module, "%s: Tile %d out of range, max %d",
		    tif->tif_name, tile, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFAppendToStrip(tif, tile, data, cc) ? cc : -1);
}

static
TIFFSetupStrips(tif)
	TIFF *tif;
{
#define	isUnspecified(td, v) \
    (td->v == 0xffffffff || (td)->td_imagelength == 0)
	register TIFFDirectory *td = &tif->tif_dir;

	if (!isTiled(tif))
		td->td_stripsperimage = isUnspecified(td, td_rowsperstrip) ?
		    1 : howmany(td->td_imagelength, td->td_rowsperstrip);
	else
		td->td_stripsperimage = isUnspecified(td, td_tilelength) ?
		    1 : TIFFNumberOfTiles(tif);
	td->td_nstrips = td->td_stripsperimage;
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE)
		td->td_nstrips *= td->td_samplesperpixel;
	td->td_stripoffset = (u_long *)
	    malloc(td->td_nstrips * sizeof (u_long));
	td->td_stripbytecount = (u_long *)
	    malloc(td->td_nstrips * sizeof (u_long));
	if (td->td_stripoffset == NULL || td->td_stripbytecount == NULL)
		return (0);
	/*
	 * Place data at the end-of-file
	 * (by setting offsets to zero).
	 */
	mybzero((char *)td->td_stripoffset, td->td_nstrips * sizeof (u_long));
	mybzero((char *)td->td_stripbytecount, td->td_nstrips * sizeof (u_long));
	TIFFSetFieldBit(tif, FIELD_STRIPOFFSETS);
	TIFFSetFieldBit(tif, FIELD_STRIPBYTECOUNTS);
	return (1);
#undef isUnspecified
}

/*
 * Verify file is writable and that the directory
 * information is setup properly.  In doing the latter
 * we also "freeze" the state of the directory so
 * that important information is not changed.
 */
static
TIFFWriteCheck(tif, tiles, module)
	register TIFF *tif;
	int tiles;
	char module[];
{
	if (tif->tif_mode == O_RDONLY) {
		TIFFError(module, "%s: File not open for writing",
		    tif->tif_name);
		return (0);
	}
	if (tiles ^ isTiled(tif)) {
		TIFFError(tif->tif_name, tiles ?
		    "Can not write tiles to a stripped image" :
		    "Can not write scanlines to a tiled image");
		return (0);
	}
	/*
	 * On the first write verify all the required information
	 * has been setup and initialize any data structures that
	 * had to wait until directory information was set.
	 * Note that a lot of our work is assumed to remain valid
	 * because we disallow any of the important parameters
	 * from changing after we start writing (i.e. once
	 * TIFF_BEENWRITING is set, TIFFSetField will only allow
	 * the image's length to be changed).
	 */
	if ((tif->tif_flags & TIFF_BEENWRITING) == 0) {
		if (!TIFFFieldSet(tif, FIELD_IMAGEDIMENSIONS)) {
			TIFFError(module,
			    "%s: Must set \"ImageWidth\" before writing data",
			    tif->tif_name);
			return (0);
		}
		if (!TIFFFieldSet(tif, FIELD_PLANARCONFIG)) {
			TIFFError(module,
		    "%s: Must set \"PlanarConfiguration\" before writing data",
			    tif->tif_name);
			return (0);
		}
		if (tif->tif_dir.td_stripoffset == NULL &&
		    !TIFFSetupStrips(tif)) {
			tif->tif_dir.td_nstrips = 0;
			TIFFError(module, "%s: No space for %s arrays",
			    tif->tif_name, isTiled(tif) ? "tile" : "strip");
			return (0);
		}
		tif->tif_flags |= TIFF_BEENWRITING;
	}
	return (1);
}

/*
 * Setup the raw data buffer used for encoding.
 */
static
TIFFBufferSetup(tif, module)
	register TIFF *tif;
	char module[];
{
	long size;

	if (isTiled(tif))
		tif->tif_tilesize = size = TIFFTileSize(tif);
	else
		tif->tif_scanlinesize = size = TIFFScanlineSize(tif);
	/*
	 * Make raw data buffer at least 8K
	 */
	if (size < 8*1024)
		size = 8*1024;
	tif->tif_rawdata = malloc(size);
	if (tif->tif_rawdata == NULL) {
		TIFFError(module, "%s: No space for output buffer",
		    tif->tif_name);
		return (0);
	}
	tif->tif_rawdatasize = size;
	tif->tif_rawcc = 0;
	tif->tif_rawcp = tif->tif_rawdata;
	return (1);
}

/*
 * Grow the strip data structures by delta strips.
 */
static
TIFFGrowStrips(tif, delta, module)
	TIFF *tif;
	int delta;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	assert(td->td_planarconfig == PLANARCONFIG_CONTIG);
	td->td_stripoffset = (u_long *)realloc(td->td_stripoffset,
	    (td->td_nstrips + delta) * sizeof (u_long));
	td->td_stripbytecount = (u_long *)realloc(td->td_stripbytecount,
	    (td->td_nstrips + delta) * sizeof (u_long));
	if (td->td_stripoffset == NULL || td->td_stripbytecount == NULL) {
		td->td_nstrips = 0;
		TIFFError(module, "%s: No space to expand strip arrays",
		    tif->tif_name);
		return (0);
	}
	mybzero(td->td_stripoffset+td->td_nstrips, delta*sizeof (u_long));
	mybzero(td->td_stripbytecount+td->td_nstrips, delta*sizeof (u_long));
	td->td_nstrips += delta;
	return (1);
}

/*
 * Append the data to the specified strip.
 *
 * NB: We don't check that there's space in the
 *     file (i.e. that strips do not overlap).
 */
static
TIFFAppendToStrip(tif, strip, data, cc)
	TIFF *tif;
	u_int strip;
	u_char *data;
	u_long cc;
{
	TIFFDirectory *td = &tif->tif_dir;
	static char module[] = "TIFFAppendToStrip";

	if (td->td_stripoffset[strip] == 0 || tif->tif_curoff == 0) {
		/*
		 * No current offset, set the current strip.
		 */
		if (td->td_stripoffset[strip] != 0) {
			if (!SeekOK(tif->tif_fd, td->td_stripoffset[strip])) {
				TIFFError(module,
				    "%s: Seek error at scanline %d",
				    tif->tif_name, tif->tif_row);
				return (0);
			}
		} else
			td->td_stripoffset[strip] =
			    lseek(tif->tif_fd, 0L, L_XTND);
		tif->tif_curoff = td->td_stripoffset[strip];
	}
	if (!WriteOK(tif->tif_fd, data, cc)) {
		TIFFError(module, "%s: Write error at scanline %d",
		    tif->tif_name, tif->tif_row);
		return (0);
	}
	tif->tif_curoff += cc;
	td->td_stripbytecount[strip] += cc;
	return (1);
}

/*
 * Internal version of TIFFFlushData that can be
 * called by ``encodestrip routines'' w/o concern
 * for infinite recursion.
 */
TIFFFlushData1(tif)
	register TIFF *tif;
{
	if (tif->tif_rawcc > 0) {
		if (tif->tif_dir.td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata,
			    tif->tif_rawcc);
		if (!TIFFAppendToStrip(tif,
		    isTiled(tif) ? tif->tif_curtile : tif->tif_curstrip,
		    (u_char *)tif->tif_rawdata, tif->tif_rawcc))
			return (0);
		tif->tif_rawcc = 0;
		tif->tif_rawcp = tif->tif_rawdata;
	}
	return (1);
}

/* tif_read */


/*VARARGS3*/
TIFFReadScanline(tif, buf, row, sample)
	register TIFF *tif;
	u_char *buf;
	u_int row, sample;
{
	int e;

	if (!TIFFCheckRead(tif, 0))
		return (-1);
	if (e = TIFFSeek(tif, row, sample)) {
		/*
		 * Decompress desired row into user buffer.
		 */
		e = (*tif->tif_decoderow)(tif, buf, tif->tif_scanlinesize, sample);
		tif->tif_row++;
	}
	return (e ? 1 : -1);
}

/*
 * Seek to a random row+sample in a file.
 */
static
/*VARARGS2*/
TIFFSeek(tif, row, sample)
	register TIFF *tif;
	u_int row, sample;
{
	register TIFFDirectory *td = &tif->tif_dir;
	int strip;

	if (row >= td->td_imagelength) {	/* out of range */
		TIFFError(tif->tif_name, "%d: Row out of range, max %d",
		    row, td->td_imagelength);
		return (0);
	}
	if (td->td_planarconfig == PLANARCONFIG_SEPARATE) {
		if (sample >= td->td_samplesperpixel) {
			TIFFError(tif->tif_name,
			    "%d: Sample out of range, max %d",
			    sample, td->td_samplesperpixel);
			return (0);
		}
		strip = sample*td->td_stripsperimage + row/td->td_rowsperstrip;
	} else
		strip = row / td->td_rowsperstrip;
	if (strip != tif->tif_curstrip) { 	/* different strip, refill */
		if (!TIFFFillStrip(tif, strip))
			return (0);
	} else if (row < tif->tif_row) {
		/*
		 * Moving backwards within the same strip: backup
		 * to the start and then decode forward (below).
		 *
		 * NB: If you're planning on lots of random access within a
		 * strip, it's better to just read and decode the entire
		 * strip, and then access the decoded data in a random fashion.
		 */
		if (!TIFFStartStrip(tif, strip))
			return (0);
	}
	if (row != tif->tif_row) {
		if (tif->tif_seek) {
			/*
			 * Seek forward to the desired row.
			 */
			if (!(*tif->tif_seek)(tif, row - tif->tif_row))
				return (0);
			tif->tif_row = row;
		} else {
			TIFFError(tif->tif_name,
		    "Compression algorithm does not support random access");
			return (0);
		}
	}
	return (1);
}

/*
 * Read a strip of data and decompress the specified
 * amount into the user-supplied buffer.
 */
long
TIFFReadEncodedStrip(tif, strip, buf, size)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	u_long size;
{
	TIFFDirectory *td = &tif->tif_dir;
	u_long stripsize = TIFFStripSize(tif);

	if (!TIFFCheckRead(tif, 0))
		return ((u_long)-1);
	if (strip >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Strip out of range, max %d",
		    strip, td->td_nstrips);
		return (-1);
	}
	/*
	 * Calculate the strip size according to the number of
	 * rows in the strip (check for truncated last strip).
	 */
	if (size == (u_long)-1)
		size = stripsize;
	else if (size > stripsize)
		size = stripsize;
	return (TIFFFillStrip(tif, strip) && 
    (*tif->tif_decodestrip)(tif, buf, size, strip / td->td_stripsperimage) ?
	    size : -1);
}

/*
 * Read a strip of data from the file.
 */
long
TIFFReadRawStrip(tif, strip, buf, size)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	u_long size;
{
	static char module[] = "TIFFReadRawStrip";
	TIFFDirectory *td = &tif->tif_dir;
	u_long bytecount;

	if (!TIFFCheckRead(tif, 0))
		return (-1);
	if (strip >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Strip out of range, max %d",
		    strip, td->td_nstrips);
		return (-1);
	}
	bytecount = td->td_stripbytecount[strip];
	if (size != (u_int)-1 && size < bytecount)
		bytecount = size;
	return (TIFFReadRawStrip1(tif, strip, buf, bytecount, module));
}

static long
TIFFReadRawStrip1(tif, strip, buf, size, module)
	TIFF *tif;
	u_int strip;
	u_char *buf;
	u_long size;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, td->td_stripoffset[strip])) {
			TIFFError(module,
			    "%s: Seek error at scanline %d, strip %d",
			    tif->tif_name, tif->tif_row, strip);
			return (-1);
		}
		if (!ReadOK(tif->tif_fd, buf, size)) {
			TIFFError(module, "%s: Read error at scanline %d",
			    tif->tif_name, tif->tif_row);
			return (-1);
		}
#ifdef MMAP_SUPPORT
	} else {
		if (td->td_stripoffset[strip] + size > tif->tif_size) {
			TIFFError(module,
			    "%s: Seek error at scanline %d, strip %d",
			    tif->tif_name, tif->tif_row, strip);
			return (-1);
		}
		mybcopy(tif->tif_base + td->td_stripoffset[strip], buf, size);
#endif
	}
	return (size);
}

/*
 * Read the specified strip and setup for decoding. 
 * The data buffer is expanded, as necessary, to
 * hold the strip's data.
 */
static
TIFFFillStrip(tif, strip)
	TIFF *tif;
	u_int strip;
{
	static char module[] = "TIFFFillStrip";
	TIFFDirectory *td = &tif->tif_dir;
	u_long bytecount;

	bytecount = td->td_stripbytecount[strip];
#ifdef MMAP_SUPPORT
	if (isMapped(tif) &&
	    (td->td_fillorder == tif->tif_fillorder || (tif->tif_flags & TIFF_NOBITREV))) {
		/*
		 * The image is mapped into memory and we either don't
		 * need to flip bits or the compression routine is going
		 * to handle this operation itself.  In this case, avoid
		 * copying the raw data and instead just reference the
		 * data from the memory mapped file image.  This assumes
		 * that the decompression routines do not modify the
		 * contents of the raw data buffer (if they try to,
		 * the application will get a fault since the file is
		 * mapped read-only).
		 */
		if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata)
			free(tif->tif_rawdata);
		tif->tif_flags &= ~TIFF_MYBUFFER;
		if (td->td_stripoffset[strip] + bytecount > tif->tif_size) {
			/*
			 * This error message might seem strange, but it's
			 * what would happen if a read were done instead.
			 */
			TIFFError(module, "%s: Read error on strip %d",
			    tif->tif_name, strip);
			tif->tif_curstrip = -1;		/* unknown state */
			return (0);
		}
		tif->tif_rawdatasize = bytecount;
		tif->tif_rawdata = tif->tif_base + td->td_stripoffset[strip];
	} else {
#endif
		/*
		 * Expand raw data buffer, if needed, to
		 * hold data strip coming from file
		 * (perhaps should set upper bound on
		 *  the size of a buffer we'll use?).
		 */
		if (bytecount > tif->tif_rawdatasize) {
			tif->tif_curstrip = -1;		/* unknown state */
			if ((tif->tif_flags & TIFF_MYBUFFER) == 0) {
				TIFFError(module,
				"%s: Data buffer too small to hold strip %d",
				    tif->tif_name, strip);
				return (0);
			}
			if (!TIFFReadBufferSetup(tif, 0,
			    roundup(bytecount, 1024)))
				return (0);
		}
		if (TIFFReadRawStrip1(tif, strip, (u_char *)tif->tif_rawdata,
		    bytecount, module) != bytecount)
			return (0);
		if (td->td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata, bytecount);
#ifdef MMAP_SUPPORT
	}
#endif
	return (TIFFStartStrip(tif, strip));
}

/*
 * Tile-oriented Read Support
 * Contributed by Nancy Cam (Silicon Graphics).
 */

/*
 * Read and decompress a tile of data.  The
 * tile is selected by the (x,y,z,s) coordinates.
 */
TIFFReadTile(tif, buf, x, y, z, s)
	TIFF *tif;
	u_char *buf;
	u_long x, y, z;
	u_int s;
{
	u_int tile;

	if (!TIFFCheckRead(tif, 1) || !TIFFCheckTile(tif, x, y, z, s))
		return (-1);
	tile = TIFFComputeTile(tif, x, y, z, s);
	if (tile >= tif->tif_dir.td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, tif->tif_dir.td_nstrips);
		return (-1);
	}
	return (TIFFFillTile(tif, tile) &&
	    (*tif->tif_decodetile)(tif, buf, tif->tif_tilesize, s) ?
	    tif->tif_tilesize : -1);
}

/*
 * Read a tile of data and decompress the specified
 * amount into the user-supplied buffer.
 */
long
TIFFReadEncodedTile(tif, tile, buf, size)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	u_long size;
{
	TIFFDirectory *td = &tif->tif_dir;
	u_long tilesize = tif->tif_tilesize;

	if (!TIFFCheckRead(tif, 1))
		return (-1);
	if (tile >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, td->td_nstrips);
		return (-1);
	}
	if (size == (u_long)-1)
		size = tilesize;
	else if (size > tilesize )
		size = tilesize;
	return (TIFFFillTile(tif, tile) && 
	    (*tif->tif_decodetile)(tif, buf, size, tile/td->td_stripsperimage) ?
	    size : -1);
}

/*
 * Read a tile of data from the file.
 */
long
TIFFReadRawTile(tif, tile, buf, size)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	u_long size;
{
	static char module[] = "TIFFReadRawTile";
	TIFFDirectory *td = &tif->tif_dir;
	u_long bytecount;

	if (!TIFFCheckRead(tif, 1))
		return (-1);
	if (tile >= td->td_nstrips) {
		TIFFError(tif->tif_name, "%d: Tile out of range, max %d",
		    tile, td->td_nstrips);
		return (-1);
	}
	bytecount = td->td_stripbytecount[tile];
	if (size != (u_long)-1 && size < bytecount)
		bytecount = size;
	return (TIFFReadRawTile1(tif, tile, buf, bytecount, module));
}

static long
TIFFReadRawTile1(tif, tile, buf, size, module)
	TIFF *tif;
	u_int tile;
	u_char *buf;
	u_long size;
	char module[];
{
	TIFFDirectory *td = &tif->tif_dir;

	if (!isMapped(tif)) {
		if (!SeekOK(tif->tif_fd, td->td_stripoffset[tile])) {
			TIFFError(module,
			    "%s: Seek error at row %d, col %d, tile %d",
			    tif->tif_name, tif->tif_row, tif->tif_col, tile);
			return (-1);
		}
		if (!ReadOK(tif->tif_fd, buf, size)) {
			TIFFError(module, "%s: Read error at row %d, col %d",
			    tif->tif_name, tif->tif_row, tif->tif_col);
			return (-1);
		}
#ifdef MMAP_SUPPORT
	} else {
		if (td->td_stripoffset[tile] + size > tif->tif_size) {
			TIFFError(module,
			    "%s: Seek error at row %d, col %d, tile %d",
			    tif->tif_name, tif->tif_row, tif->tif_col, tile);
			return (-1);
		}
		mybcopy(tif->tif_base + td->td_stripoffset[tile], buf, size);
#endif
	}
	return (size);
}

/*
 * Read the specified tile and setup for decoding. 
 * The data buffer is expanded, as necessary, to
 * hold the tile's data.
 */
static
TIFFFillTile(tif, tile)
	TIFF *tif;
	u_int tile;
{
	static char module[] = "TIFFFillTile";
	TIFFDirectory *td = &tif->tif_dir;
	u_long bytecount;

	bytecount = td->td_stripbytecount[tile];
#ifdef MMAP_SUPPORT
	if (isMapped(tif) &&
	    (td->td_fillorder == tif->tif_fillorder || (tif->tif_flags & TIFF_NOBITREV))) {
		/*
		 * The image is mapped into memory and we either don't
		 * need to flip bits or the compression routine is going
		 * to handle this operation itself.  In this case, avoid
		 * copying the raw data and instead just reference the
		 * data from the memory mapped file image.  This assumes
		 * that the decompression routines do not modify the
		 * contents of the raw data buffer (if they try to,
		 * the application will get a fault since the file is
		 * mapped read-only).
		 */
		if ((tif->tif_flags & TIFF_MYBUFFER) && tif->tif_rawdata)
			free(tif->tif_rawdata);
		tif->tif_flags &= ~TIFF_MYBUFFER;
		if (td->td_stripoffset[tile] + bytecount > tif->tif_size) {
			tif->tif_curtile = -1;		/* unknown state */
			return (0);
		}
		tif->tif_rawdatasize = bytecount;
		tif->tif_rawdata = tif->tif_base + td->td_stripoffset[tile];
	} else {
#endif
		/*
		 * Expand raw data buffer, if needed, to
		 * hold data tile coming from file
		 * (perhaps should set upper bound on
		 *  the size of a buffer we'll use?).
		 */
		if (bytecount > tif->tif_rawdatasize) {
			tif->tif_curtile = -1;		/* unknown state */
			if ((tif->tif_flags & TIFF_MYBUFFER) == 0) {
				TIFFError(module,
				"%s: Data buffer too small to hold tile %d",
				    tif->tif_name, tile);
				return (0);
			}
			if (!TIFFReadBufferSetup(tif, 0,
			    roundup(bytecount, 1024)))
				return (0);
		}
		if (TIFFReadRawTile1(tif, tile, (u_char *)tif->tif_rawdata,
		    bytecount, module) != bytecount)
			return (0);
		if (td->td_fillorder != tif->tif_fillorder &&
		    (tif->tif_flags & TIFF_NOBITREV) == 0)
			TIFFReverseBits((u_char *)tif->tif_rawdata, bytecount);
#ifdef MMAP_SUPPORT
	}
#endif
	return (TIFFStartTile(tif, tile));
}

/*
 * Setup the raw data buffer in preparation for
 * reading a strip of raw data.  If the buffer
 * is specified as zero, then a buffer of appropriate
 * size is allocated by the library.  Otherwise,
 * the client must guarantee that the buffer is
 * large enough to hold any individual strip of
 * raw data.
 */
int
TIFFReadBufferSetup(tif, bp, size)
	TIFF *tif;
	char *bp;
	u_long size;
{
	static char module[] = "TIFFReadBufferSetup";

	if (tif->tif_rawdata) {
		if (tif->tif_flags & TIFF_MYBUFFER)
			free(tif->tif_rawdata);
		tif->tif_rawdata = NULL;
	}
	if (bp) {
		tif->tif_rawdatasize = size;
		tif->tif_rawdata = bp;
		tif->tif_flags &= ~TIFF_MYBUFFER;
	} else {
		tif->tif_rawdatasize = roundup(size, 1024);
		tif->tif_rawdata = malloc(tif->tif_rawdatasize);
		tif->tif_flags |= TIFF_MYBUFFER;
	}
	if (tif->tif_rawdata == NULL) {
		TIFFError(module,
		    "%s: No space for data buffer at scanline %d",
		    tif->tif_name, tif->tif_row);
		tif->tif_rawdatasize = 0;
		return (0);
	}
	return (1);
}

/*
 * Set state to appear as if a
 * strip has just been read in.
 */
static
TIFFStartStrip(tif, strip)
	register TIFF *tif;
	u_int strip;
{
	TIFFDirectory *td = &tif->tif_dir;

	tif->tif_curstrip = strip;
	tif->tif_row = (strip % td->td_stripsperimage) * td->td_rowsperstrip;
	tif->tif_rawcp = tif->tif_rawdata;
	tif->tif_rawcc = td->td_stripbytecount[strip];
	return (tif->tif_predecode == NULL || (*tif->tif_predecode)(tif));
}

/*
 * Set state to appear as if a
 * tile has just been read in.
 */
static
TIFFStartTile(tif, tile)
	register TIFF *tif;
	u_int tile;
{
	TIFFDirectory *td = &tif->tif_dir;

	tif->tif_curtile = tile;
    /* ndr -- fixed mathematical error in row computation -cf tif_write */
	tif->tif_row = (tile / howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilelength;
	tif->tif_col = (tile % howmany(td->td_imagewidth, td->td_tilewidth))
		* td->td_tilewidth;
	tif->tif_rawcp = tif->tif_rawdata;
	tif->tif_rawcc = td->td_stripbytecount[tile];
	return (tif->tif_predecode == NULL || (*tif->tif_predecode)(tif));
}

static
TIFFCheckRead(tif, tiles)
	TIFF *tif;
	int tiles;
{
	if (tif->tif_mode == O_WRONLY) {
		TIFFError(tif->tif_name, "File not open for reading");
		return (0);
	}
	if (tiles ^ isTiled(tif)) {
		TIFFError(tif->tif_name, tiles ?
		    "Can not read tiles from a stripped image" :
		    "Can not read scanlines from a tiled image");
		return (0);
	}
	return (1);
}

/* end tif_read */

/* tif_flush */
TIFFFlush(tif)
	TIFF *tif;
{

	if (tif->tif_mode != O_RDONLY) {
		if (!TIFFFlushData(tif))
			return (0);
		if ((tif->tif_flags & TIFF_DIRTYDIRECT) &&
		    !TIFFWriteDirectory(tif))
			return (0);
	}
	return (1);
}

/*
 * Flush buffered data to the file.
 */
TIFFFlushData(tif)
	TIFF *tif;
{
	if ((tif->tif_flags & TIFF_BEENWRITING) == 0)
		return (0);
	if (tif->tif_flags & TIFF_POSTENCODE) {
		tif->tif_flags &= ~TIFF_POSTENCODE;
		if (tif->tif_postencode && !(*tif->tif_postencode)(tif))
			return (0);
	}
	return (TIFFFlushData1(tif));
}
/* end tif_flush */


#if USE_PROTOTYPES
extern	int TIFFDefaultDirectory(TIFF*);
static int TIFFReadTIFFHdr( int fd, TIFFHeader *hdr );
#else
extern	int TIFFDefaultDirectory();
static int TIFFReadTIFFHdr();
#endif

static const long typemask[13] = {
	0,		/* TIFF_NOTYPE */
	0x000000ff,	/* TIFF_BYTE */
	0xffffffff,	/* TIFF_ASCII */
	0x0000ffff,	/* TIFF_SHORT */
	0xffffffff,	/* TIFF_LONG */
	0xffffffff,	/* TIFF_RATIONAL */
	0x000000ff,	/* TIFF_SBYTE */
	0x000000ff,	/* TIFF_UNDEFINED */
	0x0000ffff,	/* TIFF_SSHORT */
	0xffffffff,	/* TIFF_SLONG */
	0xffffffff,	/* TIFF_SRATIONAL */
	0xffffffff,	/* TIFF_FLOAT */
	0xffffffff,	/* TIFF_DOUBLE */
};
static const int bigTypeshift[13] = {
	0,		/* TIFF_NOTYPE */
	24,		/* TIFF_BYTE */
	0,		/* TIFF_ASCII */
	16,		/* TIFF_SHORT */
	0,		/* TIFF_LONG */
	0,		/* TIFF_RATIONAL */
	16,		/* TIFF_SBYTE */
	16,		/* TIFF_UNDEFINED */
	24,		/* TIFF_SSHORT */
	0,		/* TIFF_SLONG */
	0,		/* TIFF_SRATIONAL */
	0,		/* TIFF_FLOAT */
	0,		/* TIFF_DOUBLE */
};
static const int litTypeshift[13] = {
	0,		/* TIFF_NOTYPE */
	0,		/* TIFF_BYTE */
	0,		/* TIFF_ASCII */
	0,		/* TIFF_SHORT */
	0,		/* TIFF_LONG */
	0,		/* TIFF_RATIONAL */
	0,		/* TIFF_SBYTE */
	0,		/* TIFF_UNDEFINED */
	0,		/* TIFF_SSHORT */
	0,		/* TIFF_SLONG */
	0,		/* TIFF_SRATIONAL */
	0,		/* TIFF_FLOAT */
	0,		/* TIFF_DOUBLE */
};


/*
 * Initialize the bit fill order, the
 * shift & mask tables, and the byte
 * swapping state according to the file
 * contents and the machine architecture.
 */
static
DECLARE3(TIFFInitOrder, register TIFF*, tif, int, magic, int, bigendian)
{
	/* XXX how can we deduce this dynamically? */
	tif->tif_fillorder = FILLORDER_MSB2LSB;

	tif->tif_typemask = typemask;
	if (magic == TIFF_BIGENDIAN) {
		tif->tif_typeshift = bigTypeshift;
		if (!bigendian)
			tif->tif_flags |= TIFF_SWAB;
	} else {
		tif->tif_typeshift = litTypeshift;
		if (bigendian)
			tif->tif_flags |= TIFF_SWAB;
	}
}

static int
DECLARE2(getMode, char*, mode, char*, module)
{
	int m = -1;

	switch (mode[0]) {
	case 'r':
		m = O_RDONLY;
		if (mode[1] == '+')
			m = O_RDWR;
		break;
	case 'w':
	case 'a':
		m = O_RDWR|O_CREAT;
		if (mode[0] == 'w')
			m |= O_TRUNC;
		break;
	default:
		TIFFError(module, "\"%s\": Bad mode", mode);
		break;
	}
	return (m);
}

/*
 * Open a TIFF file for read/writing.
 */
TIFF *
TIFFOpen(name, mode)
	char *name, *mode;
{
	static char module[] = "TIFFOpen";
	int m, fd;

	m = getMode(mode, module);
	if (m == -1)
		return ((TIFF *)0);
	fd = TIFFOpenFile(name, m, 0666);
	if (fd < 0) {
		TIFFError(module, "%s: Cannot open", name);
		return ((TIFF *)0);
	}
	return (TIFFFdOpen(fd, name, mode));
}

/*
 * Open a TIFF file descriptor for read/writing.
 */
TIFF *
TIFFFdOpen(fd, name, mode)
	int fd;
	char *name, *mode;
{
	static char module[] = "TIFFFdOpen";
	TIFF *tif;
	int m, bigendian;

	m = getMode(mode, module);
	if (m == -1)
		goto bad2;
	tif = (TIFF *)malloc(sizeof (TIFF) + strlen(name) + 1);
	if (tif == NULL) {
		TIFFError(module, "%s: Out of memory (TIFF structure)", name);
		goto bad2;
	}
	mybzero((char *)tif, sizeof (*tif));
	tif->tif_name = (char *)tif + sizeof (TIFF);
	strcpy(tif->tif_name, name);
	tif->tif_fd = fd;
	tif->tif_mode = m &~ (O_CREAT|O_TRUNC);
	tif->tif_curdir = -1;		/* non-existent directory */
	tif->tif_curoff = 0;
	tif->tif_curstrip = -1;		/* invalid strip */
	tif->tif_row = -1;		/* read/write pre-increment */
	
	/*
	 *  Make sure buffer flags are set for both read and write:
	 */
	
	tif->tif_flags |= TIFF_MYBUFFER;
	tif->tif_rawcp = tif->tif_rawdata = 0;
	tif->tif_rawdatasize = 0;
	
	{ int one = 1; bigendian = (*(char *)&one == 0); }
	/*
	 * Read in TIFF header.
	 */
	if (!TIFFReadTIFFHdr(fd, &tif->tif_header)) {
	
		/*
		 *  ******* W R I T E   M O D E ***********
		 */
		
		if (tif->tif_mode == O_RDONLY) {
			TIFFError(name, "Cannot read TIFF header");
			goto bad;
		}
		/*
		 * Setup header and write.
		 */
		tif->tif_header.tiff_magic =  bigendian ?
		    TIFF_BIGENDIAN : TIFF_LITTLEENDIAN;
		tif->tif_header.tiff_version = TIFF_VERSION;
		tif->tif_header.tiff_diroff = 0;	/* filled in later */
		if (!TIFFWriteTIFFHdr(fd, &tif->tif_header)) {
			TIFFError(name, "Error writing TIFF header");
			goto bad;
		}
		/*
		 * Setup the byte order handling.
		 */
		TIFFInitOrder(tif, tif->tif_header.tiff_magic, bigendian);
		/*
		 * Setup default directory.
		 */
		if (!TIFFDefaultDirectory(tif))
			goto bad;
		tif->tif_diroff = 0;
		return (tif);
	}

	/*
	 *  ******* R E A D  /  A P P E N D    M O D E ***********
	 */


	/*
	 * Setup the byte order handling.
	 */
	if (tif->tif_header.tiff_magic != TIFF_BIGENDIAN &&
	    tif->tif_header.tiff_magic != TIFF_LITTLEENDIAN) {
		TIFFError(name,  "Not a TIFF file, bad magic number %d (0x%x)",
		    tif->tif_header.tiff_magic,
		    tif->tif_header.tiff_magic);
		goto bad;
	}
	TIFFInitOrder(tif, tif->tif_header.tiff_magic, bigendian);
	/*
	 * Swap header if required.
	 */
	if (tif->tif_flags & TIFF_SWAB) {
		TIFFSwabShort(&tif->tif_header.tiff_version);
		TIFFSwabLong(&tif->tif_header.tiff_diroff);
	}
	/*
	 * Now check version (if needed, it's been byte-swapped).
	 * Note that this isn't actually a version number, it's a
	 * magic number that doesn't change (stupid).
	 */
	if (tif->tif_header.tiff_version != TIFF_VERSION) {
		TIFFError(name,
		    "Not a TIFF file, bad version number %d (0x%x)",
		    tif->tif_header.tiff_version,
		    tif->tif_header.tiff_version); 
		goto bad;
	}
	/*
	 * Setup initial directory.
	 */
	switch (mode[0]) {
	case 'r':
		tif->tif_nextdiroff = tif->tif_header.tiff_diroff;
#ifdef MMAP_SUPPORT
		if (TIFFMapFileContents(fd, &tif->tif_base, &tif->tif_size))
			tif->tif_flags |= TIFF_MAPPED;
#endif
		if (TIFFReadDirectory(tif)) {
			tif->tif_rawcc = -1;
			tif->tif_flags |= TIFF_BUFFERSETUP;
			return (tif);
		}
		break;
	case 'a':
		/*
		 * Don't append to file that has information
		 * byte swapped -- we will write data that is
		 * in the opposite order.
		 */
		if (tif->tif_flags & TIFF_SWAB) {
			TIFFError(name,
		"Cannot append to file that has opposite byte ordering");
			goto bad;
		}
		/*
		 * New directories are automatically append
		 * to the end of the directory chain when they
		 * are written out (see TIFFWriteDirectory).
		 */
		if (!TIFFDefaultDirectory(tif))
			goto bad;
		return (tif);
	}
bad:
	tif->tif_mode = O_RDONLY;	/* XXX avoid flush */
	TIFFClose(tif);
	return ((TIFF *)0);
bad2:
	(void) close(fd);
	return ((TIFF *)0);
}

u_long
TIFFScanlineSize(tif)
	TIFF *tif;
{
	TIFFDirectory *td = &tif->tif_dir;
	long scanline;
	
	scanline = td->td_bitspersample * td->td_imagewidth;
	if (td->td_planarconfig == PLANARCONFIG_CONTIG)
		scanline *= td->td_samplesperpixel;
	return ((u_long)howmany(scanline, 8));
}

static
int TIFFReadTIFFHdr( fd, hdr )
int fd;
TIFFHeader *hdr;
{
	if (!TIFFReadShort( fd, (short *)&hdr->tiff_magic ))
		return (0);
	if (!TIFFReadShort( fd, (short *)&hdr->tiff_version ))
		return (0);
	if (!ReadOK(fd, &hdr->tiff_diroff, TIFFLongSize))
		return (0);
	return (1);
}

int TIFFWriteTIFFHdr( fd, hdr )
int fd;
TIFFHeader *hdr;
{
	if (!TIFFWriteShort( fd, (short *)&hdr->tiff_magic ))
		return (0);
	if (!TIFFWriteShort( fd, (short *)&hdr->tiff_version ))
		return (0);
	if (!WriteOK(fd, &hdr->tiff_diroff, TIFFLongSize))
		return (0);
	return (1);
}

/*
 * Query functions to access private data.
 */

/*
 * Return open file's name.
 */
char *
TIFFFileName(tif)
	TIFF *tif;
{
	return (tif->tif_name);
}

/*
 * Return open file's I/O descriptor.
 */
int
TIFFFileno(tif)
	TIFF *tif;
{
	return (tif->tif_fd);
}

/*
 * Return read/write mode.
 */
int
TIFFGetMode(tif)
	TIFF *tif;
{
	return (tif->tif_mode);
}

/*
 * Return nonzero if file is organized in
 * tiles; zero if organized as strips.
 */
int
TIFFIsTiled(tif)
	TIFF *tif;
{
	return (isTiled(tif));
}

/*
 * Return nonzero if file has 
 * Cartographic Tag Information.
 */
int
TIFFhasCartoTags(tif)
	TIFF *tif;
{
	return (tif->tif_flags & TIFF_HASCARTTAGS);
}

/*
 * Return current row being read/written.
 */
long
TIFFCurrentRow(tif)
	TIFF *tif;
{
	return (tif->tif_row);
}

/*
 * Return index of the current directory.
 */
int
TIFFCurrentDirectory(tif)
	TIFF *tif;
{
	return (tif->tif_curdir);
}

/*
 * Return current strip.
 */
int
TIFFCurrentStrip(tif)
	TIFF *tif;
{
	return (tif->tif_curstrip);
}

/*
 * Return current tile.
 */
int
TIFFCurrentTile(tif)
	TIFF *tif;
{
	return (tif->tif_curtile);
}


#if USE_PROTOTYPES
extern	int TIFFFreeDirectory(TIFF*);
#else
extern	int TIFFFreeDirectory();
#endif

void
TIFFClose(tif)
	TIFF *tif;
{
	if (tif->tif_mode != O_RDONLY)
		/*
		 * Flush buffered data and directory (if dirty).
		 */
		TIFFFlush(tif);
	if (tif->tif_cleanup)
		(*tif->tif_cleanup)(tif);
	TIFFFreeDirectory(tif);
	if (tif->tif_rawdata && (tif->tif_flags&TIFF_MYBUFFER))
		free(tif->tif_rawdata);
#ifdef MMAP_SUPPORT
	if (isMapped(tif))
		TIFFUnmapFileContents(tif->tif_base, tif->tif_size);
#endif
	(void) close(tif->tif_fd);
	free((char *)tif);
}
