/*
 * @(#)PDSImageDecoder.java	
 * @version 1.01 6-3-2002 for JAI codec
 * @author Steve Levoe NASA/JPL
 */

package jpl.mipl.io.codec;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;

import javax.media.jai.DataBufferFloat;
import javax.media.jai.ComponentSampleModelJAI;


import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;

import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.IOException;

import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.ImageDecoder;
import com.sun.media.jai.codec.ImageDecoderImpl;
import com.sun.media.jai.codec.ImageDecodeParam;
import com.sun.media.jai.codec.SeekableStream;

import jpl.mipl.io.vicar.*;

import jpl.mipl.io.streams.*;

/**
 * An <code>ImageDecoder</code> for the PDS family of file formats.
 * <br>
 * "SAMPLE_INTERLEAVED", "BAND_SEQUENTIAL" and "LINE_INTERLEAVED"
 * grey scale images, and  color images.  The decoder understands
 * Images are created in their native data format. <BR>
 * Byte swapping from the native file format will be performed if needed <BR>
 * For display some images should be converted to BYTE format after they have 
 * been read in by this codec.<br>
 * 
 * This codec currently can decode BYTE, USHORT, FULL, REAL data<br>
 * DOUBLE is untested.<br>
 * This decoder employs the <code>VicarIO libraries</code>.
 * There is no ImageDecodeParam class defined for this decoder. 
 * The decoder defaults to the simplest common cases. <br>
 * A single banded image is decoded as grayscale. <br>
 * A 3 banded image is decoded as an RGB color image <br>
 * The decoder will decode images with any number of bands.
 * It is up to the display program to extract the bands to display.
 * The JAI "BandSelect" operator is useful for this.
 */
public class PDSImageDecoder extends ImageDecoderImpl {

    /**
    * Creates an instance of the decoder. <br>
    * There is no vicar specific ImageDecodeParam subclass.<br>
    * Someday there will be one to control decoding of non-standard images.
    */
    public PDSImageDecoder(SeekableStream input,
                                 ImageDecodeParam param) {
        super(input, param);
        
    }

    /**
    * This method is called to begin the decoding of an image.
    *
    * @param page Only works if page == 0
    * @return A RenderedImage is returned.
    */
    public RenderedImage decodeAsRenderedImage(int page) throws IOException {
        PDSImage image = null ;
        if (page != 0) {
            throw new IOException(
                             "Illegal page requested from a PDS image.");
        }
        try {
            image = new PDSImage(input, param);
        }
        catch (Exception e) {
            System.err.println("ERROR: "+e);
        }
        
        return image;
    }
    
    
    
}

