/*
 * @(#)VicarImageReader.java	1.0 00/08/30
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 12-2000 ImageIO EA2 version
 * 9-2002 JDK1.4 version
 */

package jpl.mipl.io.plugins;

import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.*;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;
import java.awt.image.IndexColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.io.File;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

// OH joy! package names have changed once again
// from javax.media.imageio to javax.imageio
// no doublt this comes from movement into core
import javax.imageio.IIOException;
import javax.imageio.ImageReader;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.metadata.IIOMetadata; // EA2
// import javax.media.imageio.metadata.ImageMetadata; // EA1
// import javax.media.imageio.metadata.StreamMetadata; // EA1
import javax.imageio.spi.ImageReaderSpi;
// import javax.media.imageio.stream.FileImageInputStream; // EA1
import javax.imageio.stream.ImageInputStream;
import com.sun.imageio.plugins.common.InputStreamAdapter; // EA2
import com.sun.imageio.plugins.common.SubImageInputStream; // EA2

import javax.media.jai.PlanarImage;

// added to example 
import java.io.InputStream;
import java.awt.Point;
import java.util.Hashtable;
import java.awt.image.SampleModel;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.image.RenderedImage;

// import VicarIO stuff
// VicarInputFile  SystemLabel
import jpl.mipl.io.streams.*;
import jpl.mipl.io.vicar.*;

// SeekableStream is in jai - we must remove dependancy on jai
// maybe switch to the ImageInputStream
import com.sun.imageio.plugins.common.BogusColorSpace;
import com.sun.media.jai.codec.SeekableStream;
import com.sun.media.jai.codec.ImageCodec;

import org.w3c.dom.*;


    
    /* static final VicarType[] types = {
        static final int BYTE = 0;
        static final int HALF = 1;
        static final int FULL = 2;
        static final int REAL = 3;
        static final int DOUB = 4;
        static final int COMP = 5;
     }; */

/**
 * This class is an <code>ImageReader</code>
 * for reading image files in the Vicar format.
 *
 * @version 0.5
 */
public class VicarImageReader extends ImageReader {

    private boolean debug = false;
    // private boolean debug = true;

    /**
     *
     * VicarIO specific variables
     *
     * used to get file info 
     */
     
    private VicarInputFile vif;
    private SystemLabel sys;
    private SeekableStream seekableStream;
    // vicarIO currently uses SeekableStream
    // may transition to ImageInputStream ?????

    private ImageInputStream stream; 
    private DataInputStreamWrapper inputStreamWrapper;
    DataInputStream pixelStream = null;

    BufferedImage theImage = null;
    
    private boolean haveReadHeader = false;
    
    boolean gotHeader = false;
    boolean gotMetadata = false;

    VicarMetadata vicarMetadata = new VicarMetadata();
    ImageReadParam lastParam = null;
    
    

    /**
     * The header is kept as array of Strings, one for each token
     * A comment is defined as a single token.
     * All tokens are preserved, in the order they appear in the stream
     */
    private List header = new ArrayList();
    
    // private VicarType vicarType;
    private int type; // Redundant, for convenience
    private int bitDepth; // Redundant, for convenience
    private boolean isBinary; // Redundant, for convenience
    private ImageTypeSpecifier imageType = null; // Redundant, for convenience
    private int width;
    private int height;
    private int maxGray;
    private long streamPos;
    
    /**
     * Constructor taking an ImageReaderSpi required by ImageReaderSpi.
     */
    public VicarImageReader (ImageReaderSpi mySpi) {
        super(mySpi);
        if (debug)     
          System.out.println("VicarImageReader 1.4 constructor");
    }

    /**
     * Enforce that the input must be an <code>ImageInputStream</code>
     */
     public void setInput(Object input, boolean isStreamable) {
     	setInput(input,  isStreamable, false) ;
     }
     	
     public void setInput(Object input) {
     	setInput(input, true, false) ;
     }
    
    public void setInput(Object input, boolean isStreamable, boolean ignoreMeta) {
        super.setInput(input, isStreamable, ignoreMeta);
        /** we will need to see if the VicarIO needs to be changed to use 
         * ImageInputStream
         */
        if (debug)     
              System.out.println("VicarImageReader.setInput  "+input);
        if (input instanceof ImageInputStream) {
            if (debug)     
              System.out.println("input is instanceof ImageInputStream ++++++++++++++");
            this.stream = (ImageInputStream)input;
            
            // VicarIO wants an InputStream
            // ImageInputStream extends DataInput
            this.inputStreamWrapper = new DataInputStreamWrapper((DataInput)input);
        } else {
            if (debug)     
               System.out.println("input is NOT instanceof ImageInputStream ---------- using SeekableStream");
            // this is the input type that the vicarIO lib currently expects
            this.seekableStream = (SeekableStream)input;
            // throw new IllegalArgumentException();
        }
    }

    /** Wrapper for the protected method <code>computeRegions</code>.  So it
     *  can be access from the classes which are not in <code>ImageReader</code>
     *  hierarchy.
     */
    public static void computeRegionsWrapper(ImageReadParam param,
            int srcWidth,
            int srcHeight,
            BufferedImage image,
            Rectangle srcRegion,
            Rectangle destRegion) {
    			computeRegions(param, srcWidth, srcHeight,
    						image, srcRegion, destRegion) ;
    }
    
    
	public void setDebug(boolean d) {
		debug = d;
	}
	
    /**
     * Reads the entire header, storing all header data, including comments,
     * into a list of tokens.  Each comment, to the end of the line where it
     * occurs, is considered a single token.
     */
	
	 private void readHeader() throws IIOException {
		 
		 
		 readHeader((ImageReadParam) null);
	 }
	 
    private void readHeader(ImageReadParam param) throws IIOException {
        
        if (debug) {    
           System.out.println("readHeader");
           System.out.println("ImageReadParam param = "+param);
        }
        
        if (stream == null && seekableStream == null) {
            throw new IllegalStateException ("Input stream not set");
        }
        
        try
        {
            // for now use seekableStream
            vif = new VicarInputFile();
            // System.out.println("input="+input);
            // System.out.println("seekableStream="+seekableStream);
            if (debug) {    
            	vif.setDebug(debug);
            	// vif.setDebug_tile(debug);
            	System.out.println("stream="+stream);
            }
	        // vif.open(input); // this causes the file to be read and data structures to filled
	        if (stream != null) {
	            if (debug)     
                  System.out.println("stream " + stream.getClass().getName()+" ******************* ");
	            vif.open( stream); // vif should be able to deal with ImageInputStream
	        }
	        else if (inputStreamWrapper != null) {
	            if (debug)     
                  System.out.println("inputStreamWrapper " + inputStreamWrapper.getClass().getName());
	            // vif.open((InputStream) stream);
	            vif.open(inputStreamWrapper);
	            // public void open(InputStream is) throws IOException
	            // public synchronized void open(InputStream is, boolean sequential_only) throws IOException
	        }
	        else if (seekableStream != null) {
	            if (debug)     
                  System.out.println("seekableStream " + seekableStream.getClass().getName());
	            vif.open(seekableStream);
	        }
	       
	       if (param != null) {
	        	vif.setImageReadParam(param);
	       }
	       sys = vif.getSystemLabel();
	       
	       // set default values for vif for ImageReadParam
	       
	       String format = sys.getFormat();
	       String org = sys.getOrg(); 
	       int nb = sys.getNB();
	       
	       if (debug) System.out.println("VicarImageReader.readHeader() format="+format+" org="+org);
	       
	       int nbb = sys.getNBB();
		   if (debug)  System.out.println("VicarImageReader nbb = "+nbb);
	       
		   /***/
	       if (format.equals("HALF") && org.equals("BSQ") && nb == 1) {
	            int biType = BufferedImage.TYPE_USHORT_GRAY;
	            imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
	            if (debug) System.out.println("VicarImageReader.readHeader() imageType "+imageType);
	       } 
	       else if (format.equals("USHORT") && org.equals("BSQ") && nb == 1) {
				int biType = BufferedImage.TYPE_USHORT_GRAY;
				imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
				if (debug) System.out.println("VicarImageReader.readHeader() imageType "+imageType);
			} 
	       else if (format.equals("BYTE") && org.equals("BSQ") && nb == 1) {
				int biType = BufferedImage.TYPE_BYTE_GRAY;
				imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
				if (debug) System.out.println("VicarImageReader.readHeader() imageType "+imageType);
	       
	       } else {
	        // imageType = null;
				SampleModel sampleModel = vif.createSampleModel(width, height);
				ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
			
				//if PlanarImage could not come up with a compatible color
				//model, then we will create a dummy color model ourselves 
				//using a bogus color space and more forgiving it deciding
				//compatibility
				if (colorModel == null)
				{
				    if (debug) {
				        System.out.println("creating a dummy color model");
				    }
				    
				    colorModel = getDummyColorModel(sampleModel.getNumBands(),
				                                    sampleModel.getDataType());				    
				}
				
				
				if (debug) {
					System.out.println("after pif.createSampleModel() ");
					System.out.println("sampleModel "+sampleModel);
					System.out.println("colorModel "+colorModel);
				}
					
				try {
					imageType = new ImageTypeSpecifier(colorModel, sampleModel) ;
					}
				catch (IllegalArgumentException iae) {
					if (debug) {
					  System.out.println("VicarImageReader.readHeader() ImageTypeSpecifier ");
					  System.out.println("IllegalArgumentException "+iae);
					}
				
					imageType = null;
				}
				
	       } 
	       
	       if (debug)  {  
        
	        System.out.println("VicarImageReader.readHeader() after vif.open() !@#$%^&*()+");
	        System.out.println(vif.getVicarLabel().toString());

	        System.out.println("System label:"+sys);
	        // System.out.println(sys.toString());
            // readHeader(input);
            System.out.println("--------------- VicarFile opened OK");
            VicarLabel label = vif.getVicarLabel();
            System.out.println("vicar label "+label);
           }
            // input.close(); // we keep the stream around so we can read in the data
        }
        catch (IOException ex)
        {
            System.out.println("IOException Error reading header:"+ex.getMessage());
            ex.printStackTrace();
            return;
        }
        catch (Exception ex)
        {
            System.out.println("Exception Error reading header:"+ex.getMessage());
            ex.printStackTrace();
            return;
        }
        
        
        if (debug) System.out.println("*** end of ReadHeader *****");
        haveReadHeader = true;
    }

    
    /**
     * Returns a color model using the BogusColorSpace along with bandCount and
     * datatype from the sample model.  The compatibility methods of the colormodel
     * are likely to be more forgiving.
     * @param numBands Number of bands in the sample model
     * @param datatype data type of the sample model
     * @return ColorModel
     */
    protected ColorModel getDummyColorModel(int numBands, int datatype)
    {
        ColorModel colorModel = null;
        
        try {
            //use the bogus color space
            ColorSpace cs = new BogusColorSpace(numBands);
            
            //create a trusting color model which accepts sample models and rasters
            colorModel = new ComponentColorModel(cs, false, false, Transparency.OPAQUE, datatype) //DataBuffer.TYPE_BYTE) 
            {
                public boolean isCompatibleSampleModel(SampleModel sm) { return true; }
                public boolean isCompatibleRaster(Raster r) { return true; }
            };
        } catch (Exception ex) {
            colorModel = null;
        }
        return colorModel;
    }
    
    
    
    /**
    * Just get the VicarLabel Object from the read. Put the VicarLabel 
    * into the VicarMetadata Object held by "this" (the reader).
    * The metadata trees will be generated when they are requested from the 
    * VicarMetadata class. 
    **/
    private void readMetadata() throws IIOException {
        if (gotMetadata) {
            return;
        }
        
        if (haveReadHeader == false) {
            readHeader();
        }
        
        
        // String formatName = vicarMetadata.getNativeMetadataFormatName();    
        // VicarLabelToDOM vl2DOM = new VicarLabelToDOM(vif, formatName);
        // vl2DOM.setNativeMetadataFormatName( vicarMetadata.getNativeMetadataFormatName() );
        // org.w3c.dom.Document doc = vl2DOM.getDocument();
        
        VicarLabel vicarLabel = null;
        try {
            vicarLabel = vif.getVicarLabel();
        } catch (IOException e) {
            e.printStackTrace();
            throw new IIOException("Error reading Vicar metadata", e);
        }
        
        // vicarMetadata.setFromTree(formatName, doc, vicarLabel);
        // add vicarLabel to the "native" format IIOMetadata object ???
        
        // just set the VicarLabel
        // the trees will onlybe generated if requested
        vicarMetadata.setVicarLabel(vicarLabel);
        // vicarMetadata.setFromTree(formatName, doc);
        
        
        // VicarLabelToIIOMetadata
        // get the comon format name and get and then set this one too
        
		int nbb = sys.getNBB();
	     
	     // add the binary prefix data to the metadata  
		if (debug)  System.out.println("VicarImageReader readMetadata NBB = "+nbb);
		if (nbb != 0) {
			VicarBinaryLinePrefix vblp = vif.getVicarBinaryLinePrefix();
			if (debug) System.out.println("VicarBinaryLinePrefix "+vblp);
			vicarMetadata.setVicarBinaryLinePrefix(vblp);
		}
		
		int nlb = sys.getNLB();
	     
				 // add the binary prefix data to the metadata  
				if (debug)  System.out.println("VicarImageReader readMetadata NLB = "+nlb);
				if (nlb != 0) {
					VicarBinaryHeader vbh = vif.getVicarBinaryHeader();
					if (debug) System.out.println("VicarBinaryHeader "+vbh);
					vicarMetadata.setVicarBinaryHeader(vbh);
				}
        
		// these items are descriptions of the input file which may be used
		// to create a PDS detached label (anmong other things)
        vicarMetadata.setFront_label_size(vif.getLblsize_front());
        vicarMetadata.setRecord_length(vif.getRecord_size());
        vicarMetadata.setFileRecordCount(vif.getFileRecordCount());

        gotMetadata = true;
        // gotMetadata = false; // set to false since we don't do metadata yet
    }
    
    
    
    
    public String getFormatName()  throws IIOException {
        
        return "vicar";
    }

    public int getNumImages() throws IIOException {
        return 1; // Vicar always have just 1 ???
        // at least that's all we support now
    }
    
    public int getWidth(int imageIndex) throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        // return width;
        return sys.getNS();
    }
    
    public int getHeight(int imageIndex) throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        //  return height;
        return sys.getNL();
    }

	public int getTileWidth(int imageIndex) throws IIOException {
		if (imageIndex != 0) {
			throw new IndexOutOfBoundsException() ;
		}
		if (haveReadHeader == false) {
			readHeader();
		}
		SampleModel sm = vif.createSampleModel();
		
		return sm.getWidth(); 
	
	}
	
	public int getTileHeight(int imageIndex) throws IIOException {
			if (imageIndex != 0) {
				throw new IndexOutOfBoundsException() ;
			}
			if (haveReadHeader == false) {
				readHeader();
			}
			SampleModel sm = vif.createSampleModel();
			return sm.getHeight();
	
		}

    // I think these are not useful
    public ImageTypeSpecifier getRawImageType(int imageIndex)
        throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        
        // figure out what this means in the vicar context
        return imageType;
    }
    
    public Iterator getImageTypes(int imageIndex)
        throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        ArrayList list = new ArrayList();
        list.add(imageType);
        return list.iterator();
    }


    public int getNumImages(boolean allowSearch) throws IIOException {
        if (stream == null) {
            throw new IllegalStateException("No input source set!");
        }
        return 1;
    }
    
    /**
     * Uses a Subclassed implementation of ImageReadParam.
     */
    public ImageReadParam getDefaultReadParam() {
        return new VicarImageReadParam();
    }

    /**
     * Since there is only ever 1 image, there is no clear distinction
     * between image metadata and stream metadata, so just use image
     * metadata and always return null for stream metadata.
     */
    public IIOMetadata getStreamMetadata() throws IIOException {
        return null;
    }
    
    public IIOMetadata getImageMetadata(int imageIndex) throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException("imageIndex != 0!");
        }
        readMetadata(); // make sure vicarMetadata has valid data in it
        return vicarMetadata;
    }

    public void printParam(ImageReadParam param) {
        
        System.out.println("VicarImageReader ImageReadParam ---------------");
        if (param == null) {
           System.out.println("VicarImageReader ImageReadParam = null");
           } 
        else {
            System.out.println("ImageReadParam = "+param);
            Rectangle sourceRegion = param.getSourceRegion();
            if (sourceRegion != null) {
                System.out.println("sourceRegion "+sourceRegion.x+","+sourceRegion.y+
                    "   "+sourceRegion.width+"x"+sourceRegion.height);
                } 
            else {
                System.out.println("sourceRegion is null");
            }
            System.out.println("param.sourceXSubsampling "+param.getSourceXSubsampling() );
            System.out.println("param.sourceYSubsampling "+param.getSourceYSubsampling() );
            System.out.println("param.subsamplingXOffset "+param.getSubsamplingXOffset() );
            System.out.println("param.subsamplingYOffset "+param.getSubsamplingYOffset() );
            System.out.println("------------------------------------------");
         }
         // System.out.println("------------------------------------------");
    }


	/**
	 * This method returns a RenderedImage. This is useful in at least 2 situations.
	 * 1) The data for a RenderedImage is not grabbed until asked for by an application.
	 * A tiled image will only grab the tiles needed. In a file copy the whole image wouldn't
	 * need to be loaded into memory. It could be copied tile by tile.
	 * 2) The image has more than 3 bands. It can't be returned as a BufferedImage since no
	 * ColorModel exists for > 3 bands. The user can extract bands for display using ImageOps.
	 * 
	 * added Steve Levoe 2-2003	 */
	public RenderedImage readAsRenderedImage(int imageIndex, ImageReadParam param)

        throws IIOException {
            
        if (debug) {
        	System.out.println("VicarImageReader.readAsRenderedImage()");
        	System.out.println("ImageReadParam "+param);
        }
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }
        
		if (debug) {		
        	System.out.println("VicarImageReader.readAsRenderedImage() after readHeader() ");
        	printParam(param);
		}
		
		if (haveReadHeader == false) {
            readHeader(param);
        }
        // inside readHeader this is called now
		// vif.setImageReadParam(param);
                
        // create a VicarRenderedImage using the input stream and the SystemLabel obtained by readHeader()         
        VicarRenderedImage image = null ;
        if (imageIndex != 0) {
            throw new IIOException("Illegal page requested from a Vicar image.");
        }
        
        try {
            image = new VicarRenderedImage(vif, param, debug);
        }
        catch (Exception e) {
            System.err.println("readAsRenderedImage ERROR: "+e);
        }
        
        if (debug) { 
        	System.out.println(" vif "+vif);
        	System.out.println(" image "+image);
        }
        if (debug && image instanceof VicarRenderedImage ) {
        	((VicarRenderedImage) image).setDebug(debug);
        }
                 
        if (debug) 
        	System.out.println("VicarImageReader.readAsRenderedImage() after readHeader() ");
        // return vri ;
        return image;
        }

	
    public BufferedImage read() throws IIOException {
    	
    	if (debug) { System.out.println("VicarImageReader.read() "); }
    	return read(0, (ImageReadParam) null);
    }
    
    public BufferedImage read(int imageIndex) throws IIOException {
    	
    	if (debug) { System.out.println("VicarImageReader.read("+imageIndex+") "); }
    	return read(imageIndex, (ImageReadParam) null);
    }
    
    /**
     * This implementation performs a simple read, leaving any ImageReadParam
     * subsampling to be performed by a post process, which has not yet been
     * implemented.  There are currently problems with the color code that
     * appear to be bugs in the Java2D image creation and display chain.  Only
     * bitmap and grayscale images can be read.  May 11, 2000 REV.
     */
    public BufferedImage read(int imageIndex, ImageReadParam param)

    throws IIOException
    {

        if (debug)
            System.out.println("VicarImageReader.read()");
        
        if (imageIndex != 0)
        {
            throw new IndexOutOfBoundsException();
        }
        
        if (haveReadHeader == false)
        {
            readHeader();
        }

        if (debug)
        {
            printParam(param);
        }
        boolean parameterizedRead = false;

        if (debug)
            System.out.println("VicarImageReader.read() after readHeader() ");
        /**
         * create a BufferedImage for the entire image tiling stuff will come
         * later
         **/

        int width = sys.getNS();
        int height = sys.getNL();
        int imageWidth = width;
        int imageHeight = height;
        int startX = 0; // starting x to begin reading FROM the file
        int startY = 0; // starting y to begin reading FROM the file
        int x_off = 0; // x offset into sample model to place data (origin)
        int y_off = 0; // y offset into sample model to place data (origin)
        int bands = sys.getNB();

        if (debug)
        {
            System.out.println("image is " + width + " x " + height);
            System.out
                    .println("VicarImageReader.read() imageType " + imageType);
        }
        BufferedImage theImage = null;
        SampleModel sampleModel = null;
        ColorModel colorModel = null;

        Point origin = new Point(0, 0);
        if (param != null)
        {
            Rectangle sourceRegion = param.getSourceRegion();
            if (sourceRegion != null)
            {
                width = sourceRegion.width;
                height = sourceRegion.height;
                // origin = new Point(sourceRegion.x,sourceRegion.y);
                startX = sourceRegion.x;
                startY = sourceRegion.y;
                parameterizedRead = true;
            }
        }

        if (parameterizedRead == false && imageType != null)
        {
            if (debug)
            {
                System.out.println("imageType " + imageType);
                System.out.println("image is " + width + "x" + height);
            }
            // let imageTypeSpecifier create the buffered imge for us
            theImage = imageType.createBufferedImage(width, height);
            sampleModel = theImage.getSampleModel();
            colorModel = theImage.getColorModel();
        }
        else
        {

            /**/

            /***/
            // get the SampleModel ColorModel Raster and data buffer from
            // vicarIO
            if (debug)
                System.out.println("vif.createSampleModel(" + width + ","
                        + height + ")");

            sampleModel = vif.createSampleModel(width, height);
            // create a SampleModel by hand ???

            int dataType = sampleModel.getDataType();
            int transferType = sampleModel.getTransferType();
            if (debug)
            {
                System.out.println("after vif.createSampleModel()");
                System.out.println("sampleModel " + sampleModel);
                System.out.println("transferType " + transferType
                        + "  dataType " + dataType);
            }

            // public BufferedImage createBufferedImage(int width, int height) {
            WritableRaster raster = Raster.createWritableRaster(sampleModel,
                    new Point(0, 0));
            // WritableRaster raster = Raster.createWritableRaster(sampleModel,
            // origin);

            // only a few data types allow colorModels
            // this may return null in which case we will deal with the problem
            // below
            // colorModel = ImageCodec.createComponentColorModel(sampleModel);
            if (bands <= 3)
            {

                if (dataType == DataBuffer.TYPE_SHORT)
                {
                    // what do we do with SHORT ?? the convenience method can't
                    // create a colorModel
                    // to override the choice of the ColorModel
                    if (bands == 3)
                    {
                        int[] bits = { 16, 16, 16 };

                        ColorSpace colorSpace = ColorSpace
                                .getInstance(ColorSpace.CS_sRGB);
                        colorModel = new ComponentColorModel(colorSpace, bits,
                                false, false, Transparency.TRANSLUCENT,
                                DataBuffer.TYPE_SHORT);
                    }
                    else if (bands == 1)
                    {
                        int[] bits = { 16 };

                        ColorSpace colorSpace = ColorSpace.getInstance(
                                                           ColorSpace.CS_GRAY);
                        colorModel = new ComponentColorModel(colorSpace, bits,
                                                false, false, 
                                                Transparency.TRANSLUCENT,
                                                DataBuffer.TYPE_SHORT);
                    }

                }
                else
                {
                    // colorModel =
                    // ImageCodec.createComponentColorModel(sampleModel);
                    colorModel = PlanarImage.createColorModel(sampleModel);
                }
            }
            else
            {
                //we will attempt to create a color model later...
            }

            if (debug)
            {
                System.out.println("colorModel " + colorModel);
                System.out.println("theRaster " + raster);
            }

            if (colorModel == null)
            {
                // try and get a colorModel from somewhere else ???
                // create a ColorModel by hand
                // get number of bands
                ColorSpace cs = null;
                boolean hasAlpha = false;
                boolean transparency = false;
                boolean hasAlphaPreMultiplied = false;
                transferType = sampleModel.getTransferType();
                dataType = sampleModel.getDataType();

                if (debug)
                {
                    System.out.println("colorModel is NULL");
                    System.out.println("transferType " + transferType
                            + "  dataType " + dataType);
                    System.out.println(
                            " java.awt.image.BufferedImage.TYPE_CUSTOM " +
                            java.awt.image.BufferedImage.TYPE_CUSTOM);
                    System.out.println(
                            " java.awt.image.BufferedImage.TYPE_BYTE_GRAY " +
                            java.awt.image.BufferedImage.TYPE_BYTE_GRAY);
                }

                // this really a hack (cough, cough) if we didn't already get a
                // ColorModel it probably means we couldn't display the data
                // directly
                // This gets us past an Exception for a null or invalid
                // colorModel.
                // To display the image the data will be fixed up along with a
                // new colorModel being created
                // we we get here for INT REAL and FLOAT single band images???

                if (dataType == DataBuffer.TYPE_BYTE)
                {
                    theImage = new java.awt.image.BufferedImage(width, height,
                            java.awt.image.BufferedImage.TYPE_BYTE_GRAY);
                    theImage.setData(raster);
                }
                else if ((dataType == DataBuffer.TYPE_SHORT)
                        || (dataType == DataBuffer.TYPE_USHORT))
                {
                    theImage = new java.awt.image.BufferedImage(width, height,
                            java.awt.image.BufferedImage.TYPE_USHORT_GRAY);
                    theImage.setData(raster);
                }
                else
                {
                    
                    //************** added by nttoole 03.27.2012
                    
                    //last ditch effort, try to create a bogus color model
                    colorModel = getDummyColorModel(sampleModel.getNumBands(), 
                                                    sampleModel.getDataType());
                    if (colorModel != null)
                    {
                        if (debug) System.out.println("colorModel (boguscolorspace) "+colorModel);

                        theImage = new BufferedImage(colorModel, raster,
                                       colorModel.isAlphaPremultiplied(), 
                                       new Hashtable());
                        
                    //    **************************************************
                        
                    }
                    else
                    {
                        // theImage = new java.awt.image.BufferedImage(
                        // width, height,
                        // java.awt.image.BufferedImage.TYPE_USHORT_GRAY);
                        System.out.println("colorModel is NULL, dataType="
                                + dataType);
                        System.out.println("We can't create a BufferedImage "+
                        	    	       "for this image");
                        // throw an exception???
                        throw new RuntimeException("IOException occured " +
                              "while reading vicar image file, couldn't " +
                              "create BufferedImage");
                    }
                }
            }
            else
            {
                theImage = new java.awt.image.BufferedImage(
                                colorModel, raster,
                                colorModel.isAlphaPremultiplied(), 
                                new Hashtable());
            }

        }

        if (debug)
            System.out.println("theImage " + theImage);
        
        DataBuffer buf = theImage.getRaster().getDataBuffer();

        // this tile happens to be the whole image
        if (debug)
            System.out.println("VicarImageReader.read() >>> vif.readTile >>>>>>>");
        
        try
        {
            if (startX != 0 || startY != 0 || width < imageWidth
                    || height < imageHeight)
            {
                // read a subarea of the image
                if (debug)
                    System.out.println("readTile " + startX + "," + startY
                            + " " + width + "x" + height + " from "
                            + imageWidth + "x" + imageHeight);
                vif.readTile(startX, startY, width, height, x_off, y_off,
                        sampleModel, buf);
            }
            else
            { // standard read of the entire tile
                if (debug)
                    System.out.println("readTile BASIC");
                vif.readTile(0, 0, sampleModel, buf);
            }

        } catch (NullPointerException e)
        {
            e.printStackTrace();
            throw new RuntimeException(
                    "IOException occured while reading vicar image file");
        }catch (IOException e)
        {
            e.printStackTrace();
            throw new RuntimeException(
                    "IOException occured while reading vicar image file");
        }

        if (debug)
        {
            System.out.println("VicarImageReader.read() completed");
            System.out.println("sampleModel=" + sampleModel);
            System.out.println("colorModel=" + colorModel);
        }
        return theImage;
    }
    
    
    /**
     * Vicar images do not have thumbnails.
     */
    public int getNumThumbnails(int imageIndex) {
        return 0;
    }

    public BufferedImage readThumbnail(int imageIndex,
                                       int thumbnailIndex)
        throws IIOException {
        throw new IndexOutOfBoundsException("Bad thumbnail index!");
    }

    public void reset() {
        super.reset();
        haveReadHeader = false;
        header.clear();
    }

    public void dispose() {
        reset();
        haveReadHeader = false;
        header = null;
    }

}

