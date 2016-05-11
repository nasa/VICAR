/*
 * @(#)PDSImageReaderSpi.java	1.9 00/05/23
 *
 * Copyright 2000 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 2-2001 ImageIO EA2 version
 *
 */

// this is where all the sun plugins currently live
// com.sun.imageio.plugins
// jpl plugins
package jpl.mipl.io.plugins;

import java.io.IOException;
// import java.util.Iterator;
import java.util.Locale;
import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.FileImageInputStream;
import java.io.File;

/**
 * @version 0.5
 */
public class PDSImageReaderSpi extends ImageReaderSpi {

    private boolean debug = false;
    // private boolean debug = true;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "1.0";

    private static final String[] names = {"pds"}; 
    
    private static final String[] suffixes = {"pds",
                                              "img", "lbl", "odl"};
    // vicar is also .img  This may be a confict/problem                                      
    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-pds",
                                               "image/x-img","image/pds", "image/img",
                                               "image/lbl", "image/odl", "image/x-lbl", "image/x-odl"};
                                               
    //private static final Class[] inputTypes = {
    // }
    
    // use in the super constructor for nemonic value                                           
    private static boolean _supportsStandardStreamMetadataFormat    =  false;                                     
	private static boolean _supportsStandardImageMetadataFormat      =  false;   
	// this class should support these, FIX IT !!!!      

    private static final String readerClassName =
        "jpl.mipl.io.plugins.PDSImageReader";
        
     // private static String[] extraStreamMetadataFormatNames[] = (String[][]) "";
    //  private static String[] extraStreamMetadataFormatClassNames[] = (String[][]) "";
    // Class names are given as strings to avoid loading classes
    // until they are needed.
    private static final String [] writerSpiNames = {
        "jpl.mipl.io.plugins.PDSImageWriterSpi"};
        
    //  private static String[] extraImageMetadataFormatNames[] = (String[][]) "";
     // private static String[] extraImageMetadataFormatClassNames[] = (String[][]) "";

    /**
     * No-argument constructor required by Service.
     */
    public PDSImageReaderSpi() {
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
              PDSMetadata.nativeStreamMetadataFormatName,
              PDSMetadata.nativeStreamMetadataFormatClassName,
              
              // NEW TO SUPPORT vicar metadata for a dualie 12-14
              PDSMetadata.extraMetadataFormatNames,
              PDSMetadata.extraMetadataFormatClassNames,
             // null, //  extraStreamMetadataFormatNames,  // new in 1.4
			 // null, // extraStreamMetadataFormatClassNames,  // new in 1.4
              
              _supportsStandardImageMetadataFormat, // new in 1.4
              PDSMetadata.nativeImageMetadataFormatName,
              PDSMetadata.nativeImageMetadataFormatClassName,
              
           // NEW TO SUPPORT vicar metadata for a dualie 12-14
              PDSMetadata.extraMetadataFormatNames,
              PDSMetadata.extraMetadataFormatClassNames
              // null, // extraImageMetadataFormatNames,  // new in 1.4
			  // null // extraImageMetadataFormatClassNames  // new in 1.4
              );
              
            if (debug)  System.out.println("PDSImageReaderSpi 1.4 constructor");
    }

    public void onRegistration() {
        if (debug) {
            System.out.println("PDS reader spi: on registration");
        }
    }

    // No localization
    public String getDescription(Locale locale) {
        return "PDS Image Reader";
    }

	public Class[] getInputTypes() {
		// Class[] inputTypes = {String.class, ImageInputStream.class };
		Class[] inputTypes = {String.class, File.class, ImageInputStream.class };
		return inputTypes;
	}
	
	
    public boolean canDecodeInput(Object input)
        throws IOException {
        if (debug) {
            System.out.println("In pds canDecodeInput");
        }

		ImageInputStream stream = null;
		FileImageInputStream fileStream = null;
        // might this be something else ???
        
        if ((input instanceof ImageInputStream)) {
			stream = (ImageInputStream)input;
        }
        else if((input instanceof File)) {
        	fileStream = new FileImageInputStream((File) input);
        	// do I need to close this stream ???
        	stream = fileStream;
        }
        else if ((input instanceof String)) {
			fileStream = new FileImageInputStream(new File((String)input));
						// do I need to close this stream ???
			stream = fileStream;
		}
        else {
        
            return false;
        }
        // else convert/wrap the stream to something we can use

         

        if (debug) {
            // System.out.println("stream ok");
        }

        // get the first 8 bytes of the stream into byte array
        byte[] header = new byte[14];
        stream.mark();  // Calling routine does this
        stream.readFully(header);
        stream.reset();
        
        String s = new String(header);
        // return (s.equals("PDS_VERSION_ID"));
        boolean ret = ((header[0]  == 'P') &&
                (header[1]  == 'D') &&
                (header[2]  == 'S') &&
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
        
        if (ret) {
        	return ret;
        } else {
        	ret = ((header[0]  == 'O') &&
                    (header[1]  == 'D') &&
                    (header[2]  == 'L') &&
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
        
        // if (fileStream != null) fileStream.close;
        return ret;
        
    }

	public void setDebug(boolean d) {
		debug = d;
	}
	
    public ImageReader createReaderInstance() {
     
        if (debug) System.out.println(" createReaderInstance PDSImageReader");
        return new PDSImageReader(this);
    }
    
    public ImageReader createReaderInstance(Object extension) {
        // we'll ignore this till we know what it does
        if (debug) System.out.println(" createReaderInstance extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        return new PDSImageReader(this);
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
        if (reader instanceof jpl.mipl.io.plugins.PDSImageReader) {
            return true;
        } else {
            return false;
        }
    }
    
}
