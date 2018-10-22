/**
 * Service Provider Interface for Magellan F-BIDR format.
 *
 * Based on PdsImageReaderSpi.java
 *
 * @author Bob Deen, JPL
 *
 */

package jpl.mipl.io.plugins;

import java.io.*;
import java.util.Locale;
import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.*;

/**
 * @version 0.1
 */
public class MgnFbidrImageReaderSpi extends ImageReaderSpi {

    private boolean debug = false;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "0.1";

    private static final String[] names = {"mgn-fbidr"}; 
    
    private static final String[] suffixes = {""};

    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-mgn-fbidr" };
                                               
    // use in the super constructor for nemonic value                                           
    private static boolean _supportsStandardStreamMetadataFormat    =  false;
    private static boolean _supportsStandardImageMetadataFormat      =  false;

    // this class should support these, FIX IT !!!!

    private static final String readerClassName =
        "jpl.mipl.io.plugins.MgnFbidrImageReader";
        
    /**
     * No-argument constructor required by Service.
     */
    public MgnFbidrImageReaderSpi() {
        super(vendorName,
              version,
              names,
              suffixes,
              MIMEtypes,
              readerClassName,
              STANDARD_INPUT_TYPE,
              null,		// no writer for this format
              _supportsStandardStreamMetadataFormat,
              MgnFbidrMetadata.nativeStreamMetadataFormatName,
              MgnFbidrMetadata.nativeStreamMetadataFormatClassName,
              null,
	      null,
              _supportsStandardImageMetadataFormat,
              MgnFbidrMetadata.nativeImageMetadataFormatName,
              MgnFbidrMetadata.nativeImageMetadataFormatClassName,
              null,
	      null
              );
        if (debug)  System.out.println("MgnFbidrImageReaderSpi " + version +
					" constructor");
    }

    public void onRegistration() {
        if (debug) {
            System.out.println("MgnFbidr reader spi: on registration");
        }
    }

    // No localization
    public String getDescription(Locale locale) {
        return "MgnFbidr Image Reader";
    }

    public boolean canDecodeInput(Object input) throws IOException {
        if (debug) {
            System.out.println("In MgnFbidr canDecodeInput");
        }

	ImageInputStream stream = null;
	FileImageInputStream fileStream = null;

	// Make sure it's a type we can use, and convert to stream
        if (input instanceof ImageInputStream) {
	    stream = (ImageInputStream)input;
        }
        else if (input instanceof File) {
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

        if (debug) {
            System.out.println("stream ok");
        }

        // get the first 12 bytes of the stream into byte array
        byte[] header = new byte[12];
        stream.mark();  // Calling routine does this
        stream.readFully(header);
        stream.reset();

        String s = new String(header);
        return (s.equals("NJPL1I000104") ||
		s.equals("NJPL1I000105") ||
		s.equals("NJPL1I000106") ||
		s.equals("NJPL1I000107") ||
		s.equals("NJPL1I000108"));
    }

    public void setDebug(boolean d) {
	debug = d;
    }

    public ImageReader createReaderInstance() {

        if (debug)
	    System.out.println(" createReaderInstance MgnFbidrImageReader");
        return new MgnFbidrImageReader(this);
    }

    public ImageReader createReaderInstance(Object extension) {
        // we'll ignore this till we know what it does
        if (debug) System.out.println(" createReaderInstance extension="+extension);
        // find out what an extension might be

        return new MgnFbidrImageReader(this);
    }

/**
 * Return an array of all the inputs we can handle.  Should include
 * things we can "wrap" to look correct.
 */
    public Class[] getInputTypes() {
	Class[] classes = {
            ImageInputStream.class,
	    File.class,
	    String.class
	};
	return classes;
    }

    public boolean isOwnReader(ImageReader reader) {
        if (reader instanceof jpl.mipl.io.plugins.MgnFbidrImageReader) {
            return true;
        } else {
            return false;
        }
    }

}

