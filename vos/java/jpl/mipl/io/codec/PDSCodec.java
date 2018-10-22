/*
 * @(#)PDSCodec.java	
 * @version 1.01 6-3-2002
 *
 * @author Steve Levoe NASA/JPL
 * JAI codec
 
 */

package jpl.mipl.io.codec;

import jpl.mipl.io.vicar.*;

import java.awt.image.DataBuffer;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.sun.media.jai.codec.ForwardSeekableStream;
import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.ImageDecoder;
import com.sun.media.jai.codec.ImageDecodeParam;
import com.sun.media.jai.codec.ImageEncoder;
import com.sun.media.jai.codec.ImageEncodeParam;

// create this class someday
// import javax.media.jai.codec.VicarEncodeParam;
import com.sun.media.jai.codec.SeekableStream;



/**
 * A subclass of JAI <code>ImageCodec</code> that handles the
 * PDS image. <p>
 * Currently the codec defaults to the simplest usual cases, which are: <br>
 * 1 band is a grayscale image<br>
 * 3 bands are an RGB image.
 * <br>
 * Future enhancements will includePDS specific extentions of  
 * com.sun.media.jai.codec.ImageDecodeParam and 
 * com.sun.media.jai.codec.ImageEncodeParam <br>
 * The encode and decode params will allow the user to tell the codec 
 * exactly how an image should be decoded. 
 * 
 */
public final class PDSCodec extends ImageCodec {

    /** Constructs an instance of <code>VicarCodec</code>. */
    public PDSCodec() {}

    /** 
    *
    * @return the name of the format handled by this codec. 
    */
    public String getFormatName() {
        return "pds";
    }


    /** 
    *
    * @return <code>null</code> since no encoderParam class exists. 
    */
    public Class getEncodeParamClass() {
        return null;
    }

    /**
     * @return <code>Object.class</code> since no DecodeParam
     * object is required for decoding.
     */
    public Class getDecodeParamClass() {
        return Object.class;
        // return VicarPdsIsisImageDecodeParam;
        // return ISISDecodeParam ;
    }


    /** 
    *
    * @return true if the image is encodable by this codec. 
    */
    public boolean canEncodeImage(RenderedImage im,
                                  ImageEncodeParam param) {
        SampleModel sampleModel = im.getSampleModel();

        boolean weCanEncode = false;
        
        int dataType = sampleModel.getTransferType();
        // if ((dataType == DataBuffer.TYPE_FLOAT) || (dataType == DataBuffer.TYPE_DOUBLE)) {
        if (dataType == DataBuffer.TYPE_DOUBLE) {
            // weCanEncode =  false;
            weCanEncode =  true;
            System.out.println("PDSCodec.canEncodeImage "+weCanEncode );
            // return  weCanEncode;
        }

        int numBands = sampleModel.getNumBands();
        
        // add more cases later depending on the ImageEncodeParam
        // encode ANY number of bands, it will be up to the display program
        // to select the bands to display
        // the JAI operator BandSelect is useful
        // if (numBands == 1 || numBands == 3) {
            weCanEncode =  true;
        // }

        
        // System.out.println("PDSCodec.canEncodeImage bands="+numBands+"  weCanEncode="+weCanEncode );
        return  weCanEncode;
    }


    /**
     * Instantiates a <code>VicarImageEncoder</code> to write to the
     * given <code>OutputStream</code>.
     *
     * @param dst the <code>OutputStream</code> to write to.
     * @param param an instance of <code>ImageEncodeParam</code> used to
     *        control the encoding process, or <code>null</code>.  A
     *        <code>ClassCastException</code> will be thrown if
     *        <code>param</code> is non-null but not an instance of
     *        <code>VicarEncodeParam</code>.
     *
     * @return the ImageEncoder
     */
    protected ImageEncoder createImageEncoder(OutputStream dst,
                                              ImageEncodeParam param) {
        ImageEncodeParam p = null;
        if (param != null) {
            p = (ImageEncodeParam)param; // May throw a ClassCast exception
        }

        /*******
        if (param instanceof  VicarPdsIsisImageDecodeParam) {
            return new PDSImageEncoder(dst, param);
        }
        else {
            return new PDSImageEncoder(dst, null);
        }
        *********/
        return new PDSImageEncoder(dst, null);
    }

    /**
     * Instantiates a <code>PDSImageDecoder</code> to read from the
     * given <code>InputStream</code>.
     *
     * <p> By overriding this method, <code>PDSCodec</code> is able to
     * ensure that a <code>ForwardSeekableStream</code> is used to
     * wrap the source <code>InputStream</code> instead of the a
     * general (and more expensive) subclass of
     * <code>SeekableStream</code>.  Since the Vicar decoder does not
     * require the ability to seek backwards in its input, this allows
     * for greater efficiency.
     *
     * @param src the <code>InputStream</code> to read from.
     * @param param an instance of <code>ImageDecodeParam</code> used to
     *        control the decoding process, or <code>null</code>.
     *        This parameter is ignored by this class.
     *
     * @return the ImageDecoder
     */
    protected ImageDecoder createImageDecoder(InputStream src,
                                              ImageDecodeParam param) {
        // Add buffering for efficiency
        if (!(src instanceof BufferedInputStream)) {
            src = new BufferedInputStream(src);
        }
        
        /***
        if (param instanceof  VicarPdsIsisImageDecodeParam) {
        return new PDSImageDecoder(new ForwardSeekableStream(src), param);
        }
        else {
        return new PDSImageDecoder(new ForwardSeekableStream(src), null);
        } ***/
        return new PDSImageDecoder(new ForwardSeekableStream(src), null);
    }

    /**
     * Instantiates a <code>PDSImageDecoder</code> to read from the
     * given <code>SeekableStream</code>.
     *
     * @param src the <code>SeekableStream</code> to read from.
     * @param param an instance of <code>ImageDecodeParam</code> used to
     *        control the decoding process, or <code>null</code>.
     *        This parameter is ignored by this class.
     *
     * @return the ImageEDecoder
     */
    protected ImageDecoder createImageDecoder(SeekableStream src,
                                              ImageDecodeParam param) {
        
        /***
        if (param instanceof  VicarPdsIsisImageDecodeParam) {
        return new PDSImageDecoder(src, param);
        }
        else {
        return new PDSImageDecoder(src, null);
        } ***/
        return new PDSImageDecoder(src, null);
    }

    /**
     * @return the number of bytes from the beginning of the data required
     * to recognize it as being in PDS format.
     */
    public int getNumHeaderBytes() {
         return 14;
    }

    /**
     * Used to test if this codec can decode this file. This method is called 
     * automatically on all registered codecs when a image file open is requested.
     *
     * @param header an array of bytes containing the initial bytes of the
     *        input data.     
     * we will be given getNumHeaderBytes() of the header to test 
     * @return <code>true</code> if the header bytes indicate PDS format.
     */
    public boolean isFormatRecognized(byte[] header) {
        
        String s = new String(header);
        // return (s.equals("LBLSIZE="));
        return (((header[0]  == 'P') || (header[0]  == 'O')) &&
                (header[1]  == 'D') &&
               ((header[2]  == 'S') || (header[2]  == 'L')) &&
                (header[3]  == '_') &&
                (header[4]  == 'V') &&
                (header[5]  == 'E') &&
                (header[6]  == 'R') &&
                (header[7]  == 'S') &&
                (header[8]  == 'I') &&
                (header[9]  == 'O') &&
                (header[10] == 'N') &&
                (header[11] == '_') &&
                (header[12] == 'I') &&
                (header[13] == 'D'));
    }
} 
