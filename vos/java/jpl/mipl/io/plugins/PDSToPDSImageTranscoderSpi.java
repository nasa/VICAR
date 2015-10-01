/*
 * @(#)PDSToPDSImageTranscoderSpi.java	1.9 2011/04/28
 *
 
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 4-2011 ImageIO 
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
public class PDSToPDSImageTranscoderSpi extends ImageTranscoderSpi {

    private static final boolean debug = false;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "0.1";

    private static final String[] names = {"pds"}; 
    
    private static final String[] suffixes = {"pds",
                                              "img"};
    // vicar is also .img  This may be a confict/problem                                      
    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-pds",
                                               "image/x-img"};
                                               
                                               

    private static final String TranscoderClassName =
        "jpl.mipl.io.plugins.PDSToPDSImageTranscoder";

    // Class names are given as strings to avoid loading classes
    // until they are needed.
    private static final 
    String  writerSpiName = "jpl.mipl.io.plugins.PDSImageWriterSpi";
    
    private static final 
    String readerSpiName = "jpl.mipl.io.plugins.PDSImageReaderSpi" ;
    /**
     * No-argument constructor required by Service.
     */
    public PDSToPDSImageTranscoderSpi() {
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
              PDSMetadata.metadataFormatNames,
              PDSMetadata.nativeMetadataFormatName,
              PDSMetadata.metadataFormatNames,
              PDSMetadata.nativeMetadataFormatName
              );
           ******/   
          if (debug)    System.out.println("PDSToPDSImageTranscoderSpi 1.5+ constructor");
    }


    public ImageTranscoder createTranscoderInstance() {
     
        if (debug) System.out.println("createTranscoderInstance PDSToPDSImageTranscoderSpi");
        return new PDSToPDSImageTranscoder();
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
            System.out.println("PDSToPDS Transcoder jdk1.5 spi: on registration");
        }
    }

    public void onDeregistration() {
        if (debug) {
            System.out.println("PDSToPDS Transcoderjdk1.5 spi: on De-registration");
        }
    }
    // No localization
    public String getDescription(Locale locale) {
        return "PDS To PDS Image Transcoder";
    }

    //----------------------------
    
    
}
