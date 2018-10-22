/**
 * @(#)PDSImage.java	
 * @version 1.11 6-3-2002
 *
 * @author Steve Levoe NASA/JPL
 
 * subclass of VicarImage
 * uses a PDSImageFile which is a subclass of VicarImageFile
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

import java.util.Hashtable;
import java.util.Properties;

import jpl.mipl.io.vicar.*;

import jpl.mipl.io.streams.*;



/**
* PDSImage holds the PDS image headers and data.
* If this image is a "Dualie" which means it also contains an
* embedded Vicar image label then this embedded Vicar label will
* be read and a VicarLabel Object will be created.<br>
* The VicarLabel Object will be placed into a property 
* "vicar_label".
*
* @see VicarImage
* @see jpl.mipl.io.vicar.PDSInputFile
*/
public class PDSImage extends VicarImage {
    
    // VicarPdsIsisImageDecodeParam  decodeParam = null;
    // ImageDecodeParam decodeParam  = null;
    /**
    * @param input the SeekableStream conatining the image data.
    */
    public PDSImage(SeekableStream input) throws Exception {

        super(input, new PDSInputFile()); 
        
        // get the PDSInputFile we sent into the constructor above
        // since we can't do ANYTHING before the super, we must get vif now
        PDSInputFile pif = (PDSInputFile) this.vif;
        //pif.setToProperties(properties);
        
    }
    
    /**
    * @param input the SeekableStream containing the image data.
    */
    public PDSImage(SeekableStream input, ImageDecodeParam param) throws Exception {

        // super(input, new PDSInputFile((VicarPdsIsisImageDecodeParam) param )); 
        super(input, new PDSInputFile()); 
        // get the PDSInputFile we sent into the constructor above
        // since we can't do ANYTHING before the super, we must get vif now
        PDSInputFile pif = (PDSInputFile) this.vif;
        // pif.setToProperties(properties); 
    }
    
    /**
    * @param input the SeekableStream containing the image data.
    * @param tileSize sets the tileHeight and tileWidth to this value
    */
    public PDSImage(SeekableStream input, int tileSize) throws Exception {

        super(input, new PDSInputFile()); 
        this.tileHeight = tileSize;
        this.tileWidth = tileSize;
        
        // get the PDSInputFile swe sent into the constructor above
        // since we can't do ANYTHING before the super, we must get vif now
        PDSInputFile pif = (PDSInputFile) this.vif;
        // pif.setToProperties(properties);
        
        
    }
    
}
