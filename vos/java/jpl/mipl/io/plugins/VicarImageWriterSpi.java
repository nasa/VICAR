
/*
VicarImageWriterSpi.java

Steve Levoe JPL/NASA 1-2001
*/


package jpl.mipl.io.plugins;

import java.io.IOException;
import java.util.Locale;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.spi.ImageWriterSpi;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.ImageOutputStream;
import javax.imageio.IIOException;
import javax.imageio.ImageWriter;

import java.awt.image.SampleModel;

import java.io.FileOutputStream;

import java.awt.image.RenderedImage;
// import java.awt.image.*;

public class VicarImageWriterSpi extends ImageWriterSpi {
    static final String vendorName = "Jet Propulsion Laboratory/MIPL";
    static final String version = "1.0";
    // static final String writerClassName ="com.mycompany.imageio.MyFormatImageWriter";
    static final String writerClassName ="jpl.mipl.io.plugins.vicar.VicarImageWriter";
    static final String[] names = { "vicar", "vic" };
    static final String[] suffixes =  {"vic", "vicar", "img"};
    static final String[] MIMETypes =  {"image/x-vic",
                                         "image/x-vicar",
                                         "image/x-img"};
    // static final String[] readerSpiNames = {"com.mycompany.imageio.MyFormatImageReaderSpi" };
    static final String[] readerSpiNames = {"jpl.mipl.io.plugins.vicar.VicarImageReaderSpi" };
    
    //  put all the OutputStreams the VicarImageWriter can successfully handle
    static final Class[] outputTypes = { ImageInputStream.class, ImageOutputStream.class, FileOutputStream.class };

// Metadata formats, more information below
    static final String nativeStreamMetadataFormatName = null;
    static final String[] streamMetadataFormatNames = null;
    // static final String nativeImageMetadataFormatName ="jpl.mipl.io.plugins.vicar.VicarImageWriterMetadata_1.0";
    public static final String nativeImageMetadataFormatName = "VICAR_LABEL";
    static final String[] imageMetadataFormatNames = { nativeImageMetadataFormatName };
    
    boolean debug = false;
    
    
    public VicarImageWriterSpi() {
          
          super(vendorName,
              version,
              names,
              suffixes,
              MIMETypes,
              writerClassName,
              outputTypes,  // STANDARD_OUTPUT_TYPE,
              readerSpiNames,
              false, // supportsStandardImageMetadataImageFormat
              // new add Metadata stuff
              
             VicarMetadata.nativeStreamMetadataFormatName,
             VicarMetadata.nativeStreamMetadataFormatClassName,
              null,
              null,
               false, // supportsStandardStreamMetadataImageFormat
              VicarMetadata.nativeImageMetadataFormatName,
              VicarMetadata.nativeImageMetadataFormatClassName,
              null, 
              null
              );
}

    // this is an Abstract method
    // public  ImageWriter createWriterInstance(Object extension) {
        
    public  ImageWriter createWriterInstance() {
                                          // throws IIOException {
                                          // IllegalArgumentException 
                                          
        // System.out.println(" createWriterInstance extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        ImageWriter iw = new VicarImageWriter(this);
        return (ImageWriter) iw ;
    }                                      
    
    public  ImageWriter createWriterInstance(Object extension) {
    
                                          // throws IIOException {
                                          // IllegalArgumentException 
                                          
        // System.out.println(" createWriterInstance extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        ImageWriter iw = new VicarImageWriter(this);
        return (ImageWriter) iw ;
    }          
    
    public boolean canEncodeImage(ImageTypeSpecifier imageType) {
		return true; // we can handle most anything??
		// JAI.create aned ImageIO.read MAY call this
		/***
		if (debug) {
			System.out.println("****** VicarImageWriterSpi.canEncodeImage imageType");
    		// if (imageType == null) return false;
    	
    		System.out.println("VicarImageWriterSpi.canEncodeImage");
			}
    	SampleModel sm = imageType.getSampleModel();
    	if (sm == null){
			if (debug) System.out.println("SampleModel is NULL");
    	}
    	else {
    	
			System.out.println("SampleModel "+sm);
			int bands = sm.getNumBands();
			System.out.println("SampleModel bands "+bands);
        	// int bands = imageType.getNumBands();
        	// return bands == 1 || bands == 3;
    	}
    	return true; // we can handle most anything??
    	**/
    }
    
    public boolean canEncodeImage(RenderedImage ri) {
        // should do some checking
		if (debug) System.out.println("****** VicarImageWriterSpi.canEncodeImage ri");
        return true;
        
    }
    
    public String getDescription(Locale locale) {
    // Localize as appropriate
    return "Vicar Image writer";
    }
}
