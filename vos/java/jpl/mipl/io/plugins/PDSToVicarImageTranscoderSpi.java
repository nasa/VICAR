/*
 * @(#)PDSToVicarImageTranscoderSpi.java	1.9 00/05/23
 *
 
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 10-2003 
 *
 */


package jpl.mipl.io.plugins;

import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import javax.imageio.ImageTranscoder;
import javax.imageio.spi.ImageTranscoderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * 
 */
public class PDSToVicarImageTranscoderSpi extends ImageTranscoderSpi {

    private static final boolean debug = false;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "1.0";
    
    // none of these are used by Transcoders
    
	// private static final String[] suffixes = {"vic", "vicar", "img"};
                                             
		// no idea what we should use for mime types
	private static final String[] MIMEtypes = {"image/x-vic",
												   "image/x-vicar", "image/x-img",
												   "image/img","image/vic","image/vicar"};

    // private static final String[] names = {"pds"}; 
	private static final String[] names = {"vicar", "vic", "img"}; 
    
    private static final String[] suffixes = {"pds", "img"};
    // vicar is also .img  This may be a confict/problem  
                                        
    // no idea what we should use for mime types
    // private static final String[] MIMEtypes = {"image/x-pds", "image/x-img"};
                                               
                                               

    private static final String TranscoderClassName =
        "jpl.mipl.io.plugins.PDSToVicarImageTranscoder";

    // Class names are given as strings to avoid loading classes
    // until they are needed.
    private static final 
    String  readerSpiName = "jpl.mipl.io.plugins.PDSImageReaderSpi";
    
    private static final 
    String writerSpiName = "jpl.mipl.io.plugins.VicarImageWriterSpi" ;
    /**
     * No-argument constructor required by Service.
     */
    public PDSToVicarImageTranscoderSpi() {
       // public ImageTranscoderSpi(String vendorName, String version)
        super(vendorName, version);                  
        /****
        super(vendorName,
              version,
              names,
              suffixes,
              MIMEtypes,
              TranscoderClassName,
              ImageTranscoderSpi.STANDARD_INPUT_TYPE,
              writerSpiName,
              // new add Metadata stuff
              VicarMetadata.metadataFormatNames,
              VicarMetadata.metadataFormatNames,
              VicarMetadata.nativeMetadataFormatName
              );
           ******/   
          if (debug)    System.out.println("VicarToPDSImageTranscoderSpi 1.4 constructor");
    }


    public ImageTranscoder createTranscoderInstance() {
     
        if (debug) System.out.println("createTranscoderInstance PDSToVicarImageTranscoderSpi");
        return new PDSToVicarImageTranscoder();
    }
    
    public String getReaderServiceProviderName() {
        return readerSpiName;
    }
    
    public String getWriterServiceProviderName() {
        return writerSpiName;
    }
    
    
    public String getVendorName() {
        return vendorName;
    }
    
    public String getVersion() {
        return version;
    }
    
    public void onRegistration() {
        if (debug) {
            System.out.println("PDSToVicar Transcoder jdk1.4 spi: on registration");
        }
    }

    public void onDeregistration() {
        if (debug) {
            System.out.println("PDSToVicar Transcoder jdk1.4 spi: on De-registration");
        }
    }
    // No localization
    public String getDescription(Locale locale) {
        return "PDS to Vicar Image Transcoder";
    }

    //----------------------------
    
    
}
