/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package	jpl.mipl.jade.jadis.agile2d;

import java.awt.*;
import java.awt.image.*;

/**
 * Performs	operations on Java AWT BufferedImage data. <p>
 * 
 * In particular, provides methods to convert from Java	component 
 * ordering	(A,R,G,B) to OpenGL	component ordering (R, G, B, A)	- necessary	
 * because for some	unknowable reason Java and OpenGL's	large set of component 
 * orderings manage	to not overlap!
 */
public class ImageUtils {
	/**
	 * Returns the power of	2 immediately larger or	equal to the specified size.
	 * @param size the size
	 * @return the power of	2 immediately larger or	equal to the specified size
	 */
	public static int nextPowerOf2(int size) {
		if (size ==	0)
			return 0;
		for	(int i = 1;	i <= 1024; i *=	2) { 
		  //	no more	than 1024
			if (i >= size)
				return i;
		}
		return 10;
	}
	/**
	 * Returns true	if the image is	known to have no alpha channel,	and
	 * false if	it may or may not have an alpha	channel
	 */
	static boolean isOpaque(Image image) {
		boolean	opaque = false;
		if (image instanceof BufferedImage) {
			BufferedImage bim =	(BufferedImage)	image;
			if (bim.getColorModel().getTransparency()
				== Transparency.OPAQUE) {
				opaque = true;
			}
		}
		return opaque;
	}

	/**
	 * Clears an area of an	image to zero.
	 */
	static void	clearArea(BufferedImage	image, int width, int height) {
		int	scanSize = image.getWidth();
		if (width >	image.getWidth())
			width =	image.getWidth();
		if (height > image.getHeight())
			height = image.getHeight();
		int	data[] =
			((DataBufferInt) (image
			.getWritableTile(0,	0)
			.getDataBuffer()))
			.getData();
		int	bwidth = (width	& ~7);
		for	(int y = 0;	y <	height;	y++) {
			int	base = y * scanSize;
			int	x =	0;
			for	(; x < bwidth; x +=	8) {
				int	pos	= base + x;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
				data[pos++]	= 0;
			}
			for	(; x < width; x++) {
				data[base +	x] = 0;
			}
		}
	}
	
	public static int ARGBtoRGBA(int srcPix) {
		return 
			((srcPix & 0xff00ff00))	|
			((srcPix & 0x00ff0000) >> 16) |
			((srcPix & 0x000000ff) << 16);
	}
	
	public static int RGBAtoARGB(int srcPix) {
		return 
			((srcPix & 0xff00ff00))	|
			((srcPix & 0x00ff0000) >> 16) |
			((srcPix & 0x000000ff) << 16);
	}

	/**
	 * Converts	a Java ARGB	Image into a GL	RGBA Image.	Takes the source data from
	 * srcPixels and stores	the	result in dstPixels. (They can point to	the	same pixel
	 * array to	perform	the	conversion inline).	
	 */
	public static void convertARGBtoRGBA(int srcPixels[], int srcScanSize,
					int sx1, int sy1, int width, int height, int dstPixels[], int dstScanSize) {
		int	srcPos;
		int	dstPos;
		int	bwidth = (width	& ~7);

		for	(int y = sy1; y	< (sy1 + height); y++) {
			int	srcBase	= y	* srcScanSize +	sy1;
			int	dstBase	= y	* dstScanSize +	sy1;

			srcPos = srcBase + sx1;
			dstPos = dstBase + sx1;
			int	x;
			for	(x = 0;	x <	bwidth;	x += 8) {
				// UNROLL 1
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 2
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 3
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 4
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 5
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 6
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 7
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
				// UNROLL 8
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
			}

			for	(; x < width; x++) {
				dstPixels[dstPos++]	= ARGBtoRGBA(srcPixels[srcPos++]);
			}
		}
	}

	/**
	 * Converts	a Java ARGB	Image into a GL	RGBA Image.	In addition, this flips	
	 * the Y orientation of	the	image.
	 */
	public static void convertAndFlipARGBtoRGBA(int srcPixels[], int imageWidth, int imageHeight, 
									int width, int height) {
		int	srcPos,	dstPos,	srcPix,	dstPix;

		if (width >	imageWidth)
			width =	imageWidth;
		if (height > imageHeight)
			height = imageHeight;
		int	bwidth = (width	& ~7);

		for	(int y = 0;	y <	(height	+ 1) / 2; y++) {
			int	srcBase	= y	* imageWidth;
			int	dstBase	= (height -	y -	1) * imageWidth;

			srcPos = srcBase;
			dstPos = dstBase;
			
			int	x;
			for	(x = 0;	x <	bwidth;	x += 8) {
				// UNROLL 1
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 2
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 3
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 4
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 5
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 6
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);
				// UNROLL 7
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);

				// UNROLL 8
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);
			}

			for	(; x < width; x++) {
				srcPix = srcPixels[srcPos];
				dstPix = srcPixels[dstPos];
				srcPixels[srcPos++]	= ARGBtoRGBA(dstPix);
				srcPixels[dstPos++]	= ARGBtoRGBA(srcPix);
			}
		}
	}
}
