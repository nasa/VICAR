/**
 * Image Reader for Magellan F-BIDR format.  Should be able to read both
 * FILE_15 (sinusoidal projection) and FILE_13 (oblique sinusoid).
 * <p>
 * The file format is documented in:  Project Magellan Software Interface
 * Specification, Full-Resolution Basic Image data Record, SDPS-101 revision E,
 * Aug 31, 1992.
 * <p>
 * Code based on PdsImageReader.java
 * <p>
 * This class is an <code>ImageReader</code> for reading image files in the
 * Magellan F-BIDR format.  We use the VICAR I/O mechansim because once you
 * get past the header, it is a simple raster of bytes.
 *
 * @version 0.1
 *
 * @author Bob Deen, JPL
 */

package jpl.mipl.io.plugins;

import java.util.*;
import java.io.*;
import javax.imageio.*;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.*;

import java.awt.*;
import java.awt.image.*;
import java.awt.color.ColorSpace;

import javax.media.jai.PlanarImage; // needed for createColorModel()
import com.sun.media.jai.codec.ImageCodec;

import org.w3c.dom.*;

import jpl.mipl.io.vicar.*;

public class MgnFbidrImageReader extends ImageReader {

    protected boolean debug = false;

    /**
     *
     * VicarIO specific variables
     *
     * used to get file info 
     */
     
    protected MgnFbidrInputFile _mif;
    protected SystemLabel _sys;
    protected boolean _haveReadHeader = false;

    protected Object _input_stream;

    MgnFbidrMetadata _mgnFbidrMetadata = new MgnFbidrMetadata();
    
    // if the file contains NO image set this to true
    // we create a 1x1 BufferedImage so we don't pass nulls around everywhere
    boolean _fakeImageNoRead = false;

    protected ImageTypeSpecifier _imageType = null;

/**
 * Constructor taking an ImageReaderSpi required by ImageReaderSpi.
 */
    public MgnFbidrImageReader (ImageReaderSpi mySpi) {
        super(mySpi);
        if (debug)   {
	    System.out.println("**********************");  
            System.out.println("MgnFbidrImageReader constructor");
    	}
    }

    public boolean getFakeImageNoRead() {
    	return _fakeImageNoRead ;
    }

    public void setInput(Object input, boolean isStreamable,
					boolean ignoreMetadata) {

        super.setInput(input, isStreamable, ignoreMetadata);
        setInputInternal(input);
    }

    public void setInput(Object input, boolean isStreamable) {
	if (debug)     
	    System.out.println("MgnFbidrImageReader public void setInput(Object input, boolean isStreamable)");
        super.setInput(input, isStreamable, false); // true for ignoreMetadata ??
        setInputInternal(input);

    }

    public void setInput(Object input) {
        super.setInput(input, false, false);
        setInputInternal(input);		
    }

    /**
     * Enforce that the input must be an <code>ImageInputStream</code>
     */
    public void setInputInternal(Object input) { 
        if (debug) {
	    System.out.println("MgnFbidrImageReader.setInputInternal "+input);
        }

        if (input instanceof ImageInputStream ) {
            if (debug)     
                System.out.println("input is instanceof ImageInputStream ++++++++++++++");
            _input_stream = (ImageInputStream)input;
        } else if ( input instanceof FileImageInputStream) {
            if (debug)     
                System.out.println("input is instanceof FileImageInputStream ++++++++++++++");
            _input_stream = (ImageInputStream)input;
        } else if (input instanceof String) {
	    if (debug)     
		System.out.println("input is instanceof String ++++++++++++++");
	    FileImageInputStream fileStream ;
	    try {
		fileStream = new FileImageInputStream(new File((String)input));
					// do I need to close this stream ???
	    }
	    catch (FileNotFoundException fnfe) {
		System.out.println("FileNotFoundException "+fnfe);
		fileStream = null;
	    }
	    catch (IOException ioe) {
		System.out.println("IOException "+ioe);
		fileStream = null;
	    }
	    _input_stream = fileStream;
	} else {
            if (debug)     
               System.out.println("input is NOT instanceof ImageInputStream ---------- using SeekableStream");
            // this is the input type that the vicarIO lib currently expects
            _input_stream = input;
        }
    }


/**
 * This method returns a RenderedImage. This is useful in at least 2 situations:
 * 1) The data for a RenderedImage is not grabbed until asked for by an
 * application.  A tiled image will only grab the tiles needed. In a file copy
 * the whole image wouldn't need to be loaded into memory.  It could be copied
 * tile by tile.
 * 2) The image has more than 3 bands. It can't be returned as a BufferedImage
 * since no ColorModel exists for > 3 bands. The user can extract bands for
 * display using ImageOps.
 */
    public RenderedImage readAsRenderedImage(int imageIndex,
				ImageReadParam param) throws IOException {

        if (debug)
	    System.out.println("MgnFbidrImageReader.readAsRenderedImage()");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }

        readHeader();

	if (debug) 
            System.out.println("MgnFbidrImageReader.readAsRenderedImage() after readHeader() ");

        // create a VicarRenderedImage using the input stream and the
	// SystemLabel obtained by readHeader()

        VicarRenderedImage image = null;

        try {
            image = new VicarRenderedImage(_mif, param);
        }
        catch (Exception e) {
            System.err.println("readAsRenderedImage ERROR: "+e);
        }
        
        if (debug) { 
            System.out.println(" _mif "+_mif);
            System.out.println(" image "+image);
        }

        return image;
    }


    public void setDebug( boolean d) {
	debug = d;
    }

    /**
     * Reads the entire header, storing all header data, including comments,
     * into a list of tokens.  Each comment, to the end of the line where it
     * occurs, is considered a single token.
     */
    protected void readHeader() throws IOException {

	if (_haveReadHeader)
	    return;			// done already

        if (debug) {   
	    System.out.println("MgnFbidrImageReader.readHeader");  
	    System.out.println("input="+input);    
	    System.out.println("_input_stream="+_input_stream);
	    System.out.println("_input_stream " + _input_stream.getClass().getName()+" ******************* ");
        }

        if (_input_stream == null) {
            throw new IllegalStateException ("Input stream not set");
        }

	_mif = new MgnFbidrInputFile();
	_mif.open(_input_stream);

	_sys = _mif.getSystemLabel();
	if (debug) {
            System.out.println("*** SYSTEM LABEL ***");
            System.out.println(_sys.toString());
	}

	// The MGN F-BIDR format only supports byte data, and single band
	// (BSQ), so this is much simpler than for the sibling formats.

	int biType = BufferedImage.TYPE_BYTE_GRAY;
	_imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
	if (debug) System.out.println("MgnFbidrImageReader.readHeader() _imageType "+_imageType);


	Document document = _mif.getXMLDocument();
	if (document != null) {
            if (debug) {
            	System.out.println("MgnFBidrImageReader new MgnFBidrMetadata with document");
            	System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
            }
            _mgnFbidrMetadata = new MgnFbidrMetadata(document,_mif.getHeader());
	} else {
	    if (debug) {
            	System.out.println("no document avaiable from _mif");
            	System.out.println("MgnFbidrImageReader NO MgnFbidrMetadata **********");
       	    }
            return;
	}

	// creaste a BufferedReader from whatever input type we have

	if (debug) System.out.println("*** end of ReadHeader *****");
        _haveReadHeader = true;
    }

    /**
    * Just get the VicarLabel Object from the read. Put the VicarLabel 
    * into the VicarMetadata Object held by "this" (the reader).
    * The metadata trees will be generated when they are requested from the 
    * VicarMetadata class. 
    */
    protected void readMetadata() throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.readMetadata");

        readHeader();
    }

    public String getFormatName()  throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getFormatName");
        return "mgn-fbidr";
    }

    public int getNumImages() throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getNumImages");
        return 1;			// we only support 1
    }
    
    public int getWidth(int imageIndex) throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getWidth("+imageIndex+")");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        readHeader();
	if (debug) System.out.println("width="+_sys.getNS() );
        return _sys.getNS();
    }

    public int getHeight(int imageIndex) throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getHeight("+imageIndex+")");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        readHeader();
	if (debug) System.out.println("height="+_sys.getNL() );
        return _sys.getNL();
    }


    // I think these are not useful
    public ImageTypeSpecifier getRawImageType(int imageIndex)
						throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getRawImageType");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        readHeader();
        // _imageType is set in ReadHeader

        // figure out what this means in the vicar context
        return _imageType;
    }
    
    public Iterator<ImageTypeSpecifier> getImageTypes(int imageIndex)
				throws IOException
    {

	if (debug) System.out.println("MgnFbidrImageReader.getImageTypes");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        readHeader();

        ArrayList<ImageTypeSpecifier> list =
					 new ArrayList<ImageTypeSpecifier>();
        list.add(_imageType);
        return list.iterator();
    }


    public int getNumImages(boolean allowSearch) throws IOException {

	if (debug) System.out.println("MgnFbidrImageReader.getNumImages");
        if (_input_stream == null) {
            throw new IllegalStateException("No input source set!");
        }
        return 1;
    }
    
    /**
     * Uses the default implementation of ImageReadParam.
     */
    public ImageReadParam getDefaultReadParam() {
	if (debug)
	    System.out.println("MgnFbidrImageReader.getDefaultReadParam");
        return new ImageReadParam();
    }

    /**
     * Since there is only ever 1 image, there is no clear distinction
     * between image metadata and stream metadata, so just use image
     * metadata and always return null for stream metadata.
     */
    public IIOMetadata getStreamMetadata() throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getStreamMetedata");
        return null;
    }

    public IIOMetadata getImageMetadata(int imageIndex) throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.getImageMetadata("+imageIndex+")");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException("imageIndex != 0!");
        }
        readMetadata(); // make sure _mgnFbidrMetadata has valid data in it
        return _mgnFbidrMetadata;
    }

    public void printParam(ImageReadParam param) {
        System.out.println("------------------------------------------");
        System.out.println("MgnFbidrImageReader ImageReadParam:");
   
        if (param == null) {
            System.out.println("param = null");
        } 
        else {
            System.out.println("param "+param);
            Rectangle sourceRegion = param.getSourceRegion();
            if (sourceRegion != null) {
                System.out.println("sourceRegion "+sourceRegion.x+","+
			sourceRegion.y+
			"   "+sourceRegion.width+"x"+sourceRegion.height);
            } 
            else {
                System.out.println("sourceRegion is null");
            }
            System.out.println("param.sourceXSubsampling "+param.getSourceXSubsampling() );
            System.out.println("param.sourceYSubsampling "+param.getSourceYSubsampling() );
            System.out.println("param.subsamplingXOffset "+param.getSubsamplingXOffset() );
            System.out.println("param.subsamplingYOffset "+param.getSubsamplingYOffset() );
         }
         System.out.println("------------------------------------------");
    }

    /**
     * This implementation performs a simple read, leaving any ImageReadParam
     * subsampling to be performed by a postprocess, which has not yet been
     * implemented.  There are currently problems with the color code that
     * appear to be bugs in the Java2D image creation and display chain.  Only
     * bitmap and grayscale images can be read.  May 11, 2000 REV.
     */
    public BufferedImage read(int imageIndex, ImageReadParam param)
						throws IOException {
        if (debug) {
	    System.out.println("*** read read read read read *******");
	    System.out.println("************************************");
            System.out.println("MgnFbidrImageReader.read("+imageIndex+")");
       	} 
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }

        readHeader();

        if (debug) printParam(param);
        boolean parameterizedRead = false;

        if (debug) System.out.println("MgnFbidrImageReader.read() after readHeader() ");
        /**
         * create a BufferedImage for the entire image
         * tiling stuff will come later
         **/
         
        // get these values from ?????
        int width = _sys.getNS();
        int height = _sys.getNL();
        int imageWidth = width;
        int imageHeight = height ;
        int startX = 0; // starting x to begin reading FROM the file
        int startY = 0; // starting y to begin reading FROM the file
        int x_off = 0; // x offset into sample model to place data (origin)
        int y_off = 0; // y offset into sample model to place data (origin)

	if (debug) {
            System.out.println("image is "+width+"x"+height);
	    System.out.println("MgnFbidrImageReader.read() _imageType "+_imageType);
        }
         
        if (width == 0 && height == 0) {
	    width = 1;
	    height = 1;
	    _fakeImageNoRead = true;
	    // we need valid image to continue
	    // this image should never be used
	    if (debug) {
        	System.out.println("PDSImageReader.read() with and height == 0. create an image 1x1");
	    }
        }
        BufferedImage theImage = null;
        SampleModel sampleModel = null;
        ColorModel colorModel = null;

        Point origin = new Point(0, 0);
        if (param != null) {
            Rectangle sourceRegion = param.getSourceRegion();
            if (sourceRegion != null) {
                width = sourceRegion.width;
                height = sourceRegion.height;
                startX = sourceRegion.x;
                startY = sourceRegion.y;
                parameterizedRead = true;
	    }
	}

	if (parameterizedRead == false && _imageType != null) {
            if (debug)  {
		System.out.println("_imageType.createBufferedImage ");
            	System.out.println("_imageType "+_imageType);
            } 
            // let _imageTypeSpecifier create the buffered iamge for us
            theImage = _imageType.createBufferedImage(width, height);
            sampleModel = theImage.getSampleModel();
            colorModel = theImage.getColorModel();
        }
        else {

            // get the SampleModel,ColorModel,Raster, data buffer from vicarIO
            if (debug) {
	        System.out.println("-----------------------------------------");
                System.out.println("_mif.createSampleModel("+width+","+height+")");
            } 
         
            sampleModel = _mif.createSampleModel(width, height);
         
            if (debug) {
                System.out.println("after _mif.createSampleModel()");
                System.out.println("sampleModel "+sampleModel);
            }
         
            WritableRaster raster = Raster.createWritableRaster(sampleModel,
							 new Point(0, 0));

            // get the number of bands
            int bands = sampleModel.getNumBands();
            int dataType = sampleModel.getDataType();

            if (bands <= 3) {

       	        if (dataType == DataBuffer.TYPE_SHORT) {
       		    // what do we do with SHORT ?? the convenience method can't
		    // create a colorModel to override the choice of the
		    // ColorModel
       		    if (bands == 3) {
		        int[] bits  = {16, 16, 16};
		        ColorSpace colorSpace = ColorSpace.getInstance(
							ColorSpace.CS_sRGB);
		        colorModel = new ComponentColorModel(colorSpace, bits, 
					false, false, Transparency.TRANSLUCENT,
					DataBuffer.TYPE_SHORT);
       		    }
       		    else if (bands == 1) {
		        int[] bits  = {16};
		        ColorSpace colorSpace = ColorSpace.getInstance(
							ColorSpace.CS_GRAY);
		        colorModel = new ComponentColorModel(colorSpace, bits, 
					false, false, Transparency.TRANSLUCENT,
					DataBuffer.TYPE_SHORT);
       		    }

       	        }
       	        else {
        	    // this is a dependancy on JAI
        	    colorModel = PlanarImage.createColorModel(sampleModel);
       	        }

	        if (debug) {
         	    System.out.println("bands="+bands+"  colorModel "+colorModel);
         	    // can't create a colorModel for something with a weird
                }

	        if (colorModel == null) {
         	    System.out.println("ERROR  MgnFbidrImageReader.read() colorModel is null. Can't create BufferedImage");
	        }
                else {
        	    theImage = new java.awt.image.BufferedImage(colorModel,
			    raster,
                            colorModel.isAlphaPremultiplied(),new Hashtable());
                }
	    }
            else {			// bands > 3 can't create a color model
	        // get a BufferedImage so we can still return something
                if (debug) {
         	    System.out.println("bands="+bands+"  colorModel "+colorModel);
                }
                // create a child raster using only 3 bands
                int parentX = raster.getMinX();
                int parentY = raster.getMinY();
                int w = raster.getWidth();
                int h = raster.getHeight();
                int childMinX = parentX;
                int childMinY = parentY;
                int[] bandList = {0,1,2};

                // the child should SHARE the parent's raster
                // we will read data into the parent raster
                // the BufferedImage will be from the child so a ColorModel
	        // can be created
                WritableRaster childRaster = raster.createWritableChild(parentX,
			parentY, w, h, childMinX, childMinY, bandList);

                if (debug) System.out.println("sampleModel "+sampleModel);

                colorModel = ImageCodec.createComponentColorModel(sampleModel);
                if (debug) System.out.println("colorModel "+colorModel);
                if (colorModel == null) {
         	    dataType = sampleModel.getDataType();
         	    w = sampleModel.getWidth();
         	    h = sampleModel.getHeight();
         	    int b = 3;

        	    BandedSampleModel fakeSampleModel = new BandedSampleModel(
							dataType, w, h, b);

		    // create a bogus colorModel just to get by
         	    colorModel = PlanarImage.createColorModel(fakeSampleModel);
         	        if (debug)
			    System.out.println("colorModel (fake) "+colorModel);
	        }

                // childRaster
	        theImage = new java.awt.image.BufferedImage(colorModel,
			   childRaster,
                           colorModel.isAlphaPremultiplied(), new Hashtable());

                if (debug) {
         	    System.out.println("theImage "+theImage);
		}
            }

        }
 
        DataBuffer buf = theImage.getRaster().getDataBuffer();
        
        // this tile happens to be the whole image 
        if (debug) {
            System.out.println("MgnFbidrImageReader.read() >>> _mif.readTile >>>>>>>");
            System.out.println(" _fakeImageNoRead "+_fakeImageNoRead);
        }
        if (_fakeImageNoRead == false ) {
            try {
        	if ( startX != 0 || startY != 0 ||
				 width < imageWidth || height < imageHeight) {
        	    // read a subarea of the image
        	    if (debug) System.out.println("readTile "+startX+","+
					startY+" "+ width+"x"+ height+
        				" from "+imageWidth+"x"+imageHeight);
        	    _mif.readTile(startX, startY, width, height, x_off, y_off,
				sampleModel, buf);
		}
            	else {			// standard read of the entire tile
            	    if (debug) System.out.println("readTile BASIC");
            	    _mif.readTile(0,0, sampleModel, buf);
               	}
       	    } 

            catch (IOException e) {
        	e.printStackTrace();
        	throw new RuntimeException("IOException occured while reading MGN F-BIDR image file");
            } 
        }

        if (debug) {
            System.out.println("MgnFbidrImageReader.read() completed");
            System.out.println("sampleModel="+sampleModel);
            System.out.println("colorModel="+colorModel);
        }
        return theImage;
    }


    /**
     * MGN F-BIDR images do not have thumbnails.
     */
    public int getNumThumbnails(int imageIndex) {
	if (debug) System.out.println("MgnFbidrImageReader.getNumThumbnails");
        return 0;
    }

    public BufferedImage readThumbnail(int imageIndex,
                                       int thumbnailIndex)
					throws IOException {
	if (debug) System.out.println("MgnFbidrImageReader.readThumbnail");
        throw new IndexOutOfBoundsException("Bad thumbnail index!");
    }

    public void reset() {
        super.reset();
        _haveReadHeader = false;
    }

    public void dispose() {
        reset();
        _haveReadHeader = false;
    }

}

