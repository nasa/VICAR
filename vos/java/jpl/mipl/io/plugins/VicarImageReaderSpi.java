/*
 * @(#)VicarImageReaderSpi.java	1.9 00/05/23
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
 * 12-2000 ImageIO EA2 version
 *
 */

// this is where all the sun plugins curently live
// com.sun.imageio.plugins
// jpl plugins
package jpl.mipl.io.plugins;

import java.io.IOException;
import java.util.Iterator;
import java.util.Locale;
import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * @version 0.5
 */
public class VicarImageReaderSpi extends ImageReaderSpi {

    private boolean debug = false;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "1.0";

    private static final String[] names = {"vicar"}; 
    
    private static final String[] suffixes = {"vic",
                                              "vicar",
                                              "img"};
                                             
    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-vic",
                                               "image/x-vicar", "image/x-img",
                                               "image/img","image/vic","image/vicar"};

    private static final String readerClassName =
        "jpl.mipl.io.plugins.VicarImageReader";

    // Class names are given as strings to avoid loading classes
    // until they are needed.
    private static final String [] writerSpiNames = {
        "jpl.mipl.io.plugins.VicarWriterSpi"};

    /**
     * No-argument constructor required by Service.
     */
    public VicarImageReaderSpi() {
       
         
super(vendorName,
              version,
              names,
              suffixes,
              MIMEtypes,
              readerClassName,
              STANDARD_INPUT_TYPE,
              writerSpiNames,
              false, // supportsStandardStreamMetadataFormat, // new in 1.4
              // new add Metadata stuff
              VicarMetadata.nativeStreamMetadataFormatName,
              VicarMetadata.nativeStreamMetadataFormatClassName,
              
             null, //  extraStreamMetadataFormatNames,  // new in 1.4
			 null, // extraStreamMetadataFormatClassNames,  // new in 1.4
              
              false, // supportsStandardImageMetadataFormat, // new in 1.4
             VicarMetadata.nativeImageMetadataFormatName,
             VicarMetadata.nativeImageMetadataFormatClassName,
              
              null, // extraImageMetadataFormatNames,  // new in 1.4
			  null // extraImageMetadataFormatClassNames  // new in 1.4
              );    
             if (debug) System.out.println("VicarImageReaderSpi 1.4 constructor");
    }

    public void onRegistration() {
        if (debug) {
            System.out.println("Vicar 1.4 spi: on registration");
        }
    }

	public void debug(boolean d) {
		debug = d;
	}
	
    // No localization
    public String getDescription(Locale locale) {
        return "Vicar Image Reader";
    }

    public boolean canDecodeInput(Object input)
        throws IOException {
        if (debug) {
            System.out.println("In vicar canDecodeInput");
        }

        // might this be something else ???
        
        if (!(input instanceof ImageInputStream)) {
            return false;
        }
        // else convert/wrap the stream to something we can use

        ImageInputStream stream = (ImageInputStream)input;

        if (debug) {
            System.out.println("stream ok");
        }

        // get the first 8 bytes of the stream into byte array
        byte[] header = new byte[8];
        stream.mark();  // Calling routine does this
        stream.readFully(header);
        stream.reset();
        
        String s = new String(header);
        // return (s.equals("LBLSIZE="));
        return ((header[0] == 'L') &&
                (header[1] == 'B') &&
                (header[2] == 'L') &&
                (header[3] == 'S') &&
                (header[4] == 'I') &&
                (header[5] == 'Z') &&
                (header[6] == 'E') &&
                (header[7] == '='));
        
        
    }

    public ImageReader createReaderInstance() {
     
        if (debug) System.out.println(" createReaderInstance VicarImageReader");
        return new VicarImageReader(this);
    }
    
    public ImageReader createReaderInstance(Object extension) {
        // we'll ignore this till we know what it does
        if (debug) System.out.println(" createReaderInstance VicarImageReader extension="+extension);
        // throw new IllegalArgumentException ("Vicar does not support extensions");
        // find out what an extention might be
        
        return new VicarImageReader(this);
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
        if (reader instanceof jpl.mipl.io.plugins.VicarImageReader) {
            return true;
        } else {
            return false;
        }
    }
    
}
