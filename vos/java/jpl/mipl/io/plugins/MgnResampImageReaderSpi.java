/**
 * Service Provider Interface for Magellan Resamp format.
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
import jpl.mipl.io.vicar.VicarDataFormat;

/**
 * @version 0.1
 */
public class MgnResampImageReaderSpi extends ImageReaderSpi {

    private boolean debug = false;

    private static final String vendorName = "Jet Propulsion Laboratory/MIPL";

    private static final String version = "0.1";

    private static final String[] names = {"mgn-resamp"};
    
    private static final String[] suffixes = {""};

    // no idea what we should use for mime types
    private static final String[] MIMEtypes = {"image/x-mgn-resamp" };
                                               
    // use in the super constructor for nemonic value                                           
    private static boolean _supportsStandardStreamMetadataFormat    =  false;
    private static boolean _supportsStandardImageMetadataFormat      =  false;

    // this class should support these, FIX IT !!!!

    private static final String readerClassName =
        "jpl.mipl.io.plugins.MgnResampImageReader";
        
    /**
     * No-argument constructor required by Service.
     */
    public MgnResampImageReaderSpi() {
        super(vendorName,
              version,
              names,
              suffixes,
              MIMEtypes,
              readerClassName,
              STANDARD_INPUT_TYPE,
              null,		// no writer for this format
              _supportsStandardStreamMetadataFormat,
              MgnResampMetadata.nativeStreamMetadataFormatName,
              MgnResampMetadata.nativeStreamMetadataFormatClassName,
              null,
	      null,
              _supportsStandardImageMetadataFormat,
              MgnResampMetadata.nativeImageMetadataFormatName,
              MgnResampMetadata.nativeImageMetadataFormatClassName,
              null,
	      null
              );
        if (debug)  System.out.println("MgnResampImageReaderSpi " + version +
					" constructor");
    }

    public void onRegistration() {
        if (debug) {
            System.out.println("MgnResamp reader spi: on registration");
        }
    }

    // No localization
    public String getDescription(Locale locale) {
        return "MgnResamp Image Reader";
    }

    public boolean canDecodeInput(Object input) throws IOException {
        if (debug) {
            System.out.println("In MgnResamp canDecodeInput");
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

        // There is no real marker for this file type.  The first three
        // double's in the file are orbit, reflat, and reflon.  So we simply
        // read these doubles and make sure they are within range for each
        // data type.  Odds are, other formats will not have something that
        // looks like doubles in the proper range heading the file.

        stream.mark();  // Calling routine does this
        VicarDataFormat vdf = new VicarDataFormat("X86-MACOSX", "LOW", "RIEEE");
        DataInput di = vdf.getDataInputWrapper(stream);
        double orbit = di.readDouble();
        double lat = di.readDouble();
        double lon = di.readDouble();
        stream.reset();

        if (orbit <= 0.0 || orbit > 15032.0)
            return false;
        if (orbit != (int)orbit)
            return false;
        if (lat < -90.0 || lat > 90.0)
            return false;
        if (lon < -360.0 || lon > 360.0)    // allow + or - lons
            return false;

        return true;
    }

    public void setDebug(boolean d) {
	debug = d;
    }

    public ImageReader createReaderInstance() {

        if (debug)
	    System.out.println(" createReaderInstance MgnResampImageReader");
        return new MgnResampImageReader(this);
    }

    public ImageReader createReaderInstance(Object extension) {
        // we'll ignore this till we know what it does
        if (debug) System.out.println(" createReaderInstance extension="+extension);
        // find out what an extension might be

        return new MgnResampImageReader(this);
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
        if (reader instanceof jpl.mipl.io.plugins.MgnResampImageReader) {
            return true;
        } else {
            return false;
        }
    }

}

