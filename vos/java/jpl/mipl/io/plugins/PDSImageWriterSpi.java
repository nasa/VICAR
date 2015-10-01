
/*
PDSImageWriterSpi.java

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

import java.io.FileOutputStream;

import java.awt.image.RenderedImage;
// import java.awt.image.*;

public class PDSImageWriterSpi extends ImageWriterSpi {
    static final String vendorName = "Jet Propulsion Laboratory/MIPL";
    static final String version = "1.0";
    // static final String writerClassName ="com.mycompany.imageio.MyFormatImageWriter";
    static final String writerClassName ="jpl.mipl.io.plugins.vicar.PDSImageWriter";
    static final String[] names = { "pds" };
    static final String[] suffixes =  {"pds", "img"};
    static final String[] MIMETypes =  {"image/x-img","image/x-pds","image/pds","image/img"};
    // static final String[] readerSpiNames = {"com.mycompany.imageio.MyFormatImageReaderSpi" };
    static final String[] readerSpiNames = {"jpl.mipl.io.plugins.PDSImageReaderSpi" };
    
    //  put all the OutputStreams the VicarImageWriter can successfully handle
    static final Class[] outputTypes = { ImageInputStream.class, ImageOutputStream.class, FileOutputStream.class };

// Metadata formats, more information below
    static final String nativeStreamMetadataFormatName = null;
    static final String[] streamMetadataFormatNames = null;
    // static final String nativeImageMetadataFormatName ="jpl.mipl.io.plugins.vicar.VicarImageWriterMetadata_1.0";
    public static final String nativeMetadataFormatName = "jpl.mipl.io.plugins.pdsimage_1.0";
    // public static final String nativeImageMetadataFormatName = "VICAR_LABEL";
    static final String[] imageMetadataFormatNames = { "jpl.mipl.io.plugins.pdsimage_1.0" };
    
    
    public PDSImageWriterSpi() {
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
              
              PDSMetadata.nativeStreamMetadataFormatName,
              PDSMetadata.nativeStreamMetadataFormatClassName,
              null,
              null,
               false, // supportsStandardStreamMetadataImageFormat
              PDSMetadata.nativeImageMetadataFormatName,
              PDSMetadata.nativeImageMetadataFormatClassName,
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
        
        ImageWriter iw = new PDSImageWriter(this);
        return (ImageWriter) iw ;
    }                                      
    
    public  ImageWriter createWriterInstance(Object extension) {
    
                                          // throws IIOException {
                                          // IllegalArgumentException 
                                          
        // System.out.println(" createWriterInstance extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        ImageWriter iw = new PDSImageWriter(this);
        return (ImageWriter) iw ;
    }          
    
    public boolean canEncodeImage(ImageTypeSpecifier imageType) {
		return true;
		/** we can handle ANY number of bands
    	int bands = imageType.getNumBands();
    	return bands == 1 || bands == 3;
    	*****/
    }
    
    public boolean canEncodeImage(RenderedImage ri) {
        // should do some checking
        return true;
    }
    
    public String getDescription(Locale locale) {
    // Localize as appropriate
    return "PDS Image writer";
    }
}
