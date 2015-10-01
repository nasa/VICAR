/*
 * @(#)ISISImageReaderSpi.java	1.9 00/05/23
 *
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 2-2001 ImageIO EA2 version
 *
 */


package jpl.mipl.io.plugins;

import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * @version 1.0
 */
public class ISISImageReaderSpi extends ImageReaderSpi {

    private boolean debug = false;
    // private boolean debug = true;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "1.0";

    private static final String[] names = {"isis"}; 
    
    private static final String[] suffixes = {"cub","bil","bip"};
                                              // "img"};
    // vicar is also .img  This may be a confict/problem                                      
    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-isis",
                                               "image/x-img","image/isis", "image/cub", "image/img"};
    
    // use in the super constructor for nemonic value                                           
    private static boolean _supportsStandardStreamMetadataFormat    =  false;                                     
	private static boolean _supportsStandardImageMetadataFormat      =  false;   
	// this class should support these, FIX IT !!!!      

    private static final String readerClassName =
        "jpl.mipl.io.plugins.ISISImageReader";
        
     // private static String[] extraStreamMetadataFormatNames[] = (String[][]) "";
    //  private static String[] extraStreamMetadataFormatClassNames[] = (String[][]) "";
    // Class names are given as strings to avoid loading classes
    // until they are needed.
    private static final String [] writerSpiNames = {
        "jpl.mipl.io.plugins.ISISImageWriterSpi"};
        
    //  private static String[] extraImageMetadataFormatNames[] = (String[][]) "";
     // private static String[] extraImageMetadataFormatClassNames[] = (String[][]) "";

    /**
     * No-argument constructor required by Service.
     */
    public ISISImageReaderSpi() {
        super(vendorName,
              version,
              names,
              suffixes,
              MIMEtypes,
              readerClassName,
              STANDARD_INPUT_TYPE,
              writerSpiNames,
              _supportsStandardStreamMetadataFormat, // new in 1.4
              // new add Metadata stuff
              ISISMetadata.nativeStreamMetadataFormatName,
              ISISMetadata.nativeStreamMetadataFormatClassName,
              
             null, //  extraStreamMetadataFormatNames,  // new in 1.4
			 null, // extraStreamMetadataFormatClassNames,  // new in 1.4
              
              _supportsStandardImageMetadataFormat, // new in 1.4
              ISISMetadata.nativeImageMetadataFormatName,
              ISISMetadata.nativeImageMetadataFormatClassName,
              
              null, // extraImageMetadataFormatNames,  // new in 1.4
			  null // extraImageMetadataFormatClassNames  // new in 1.4
              );
              
            if (debug)  System.out.println("ISISImageReaderSpi 1.4 constructor");
    }

    public void onRegistration() {
        if (debug) {
            System.out.println("ISIS reader spi: on registration");
        }
    }

    // No localization
    public String getDescription(Locale locale) {
        return "ISIS Image Reader";
    }

    public boolean canDecodeInput(Object input)
        throws IOException {
        if (debug) {
            System.out.println("In ISIS canDecodeInput");
        }

        // might this be something else ???
        
        if (!(input instanceof ImageInputStream)) {
            return false;
        }
        // else convert/wrap the stream to something we can use

        ImageInputStream stream = (ImageInputStream)input;

        if (debug) {
            // System.out.println("stream ok");
        }

        // get the first 80 bytes of the stream into byte array
        byte[] header = new byte[80];
        stream.mark();  // Calling routine does this
        stream.readFully(header);
        stream.reset();
        
        String s = new String(header);
        if (debug) System.out.println("ISISImageReaderSpi.canDecodeInput "+s);
        int i = s.indexOf("SFDU_LABEL");
        if (i == -1) {      	
        	return false;
        }
        else {
        	// the substring is contained in the header beginnning
        	return true;
        }
        
        
        
    }

	public void setDebug(boolean d) {
		debug = d;
	}
	
    public ImageReader createReaderInstance() {
     
        System.out.println(" createReaderInstance ");
        return new ISISImageReader(this);
    }
    
    public ImageReader createReaderInstance(Object extension) {
        // we'll ignore this till we know what it does
        if (debug) System.out.println(" createReaderInstance extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        return new ISISImageReader(this);
    }
    
    /* unimplemented interfaces ....
    public Class[] getInputTypes() {
        // return an array of all the inputs we can handle
        // should include things we can "wrap" to look correct
        ImageInputStream  // we wrap this to look like SeekableStream
        SeekableStream  
        // if we learn how to use the new ones IIO added list them too
        //
    }
    */
    public boolean isOwnReader(ImageReader reader) {
        if (reader instanceof jpl.mipl.io.plugins.ISISImageReader) {
            return true;
        } else {
            return false;
        }
    }
    
}
