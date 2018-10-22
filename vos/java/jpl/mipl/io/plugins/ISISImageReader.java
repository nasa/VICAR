/*
 * @(#)ISISImageReader.java	1.0 00/08/30
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 1-2002 ImageIO 1.4 version
 */

package jpl.mipl.io.plugins;

import java.awt.Rectangle;
import java.awt.color.ColorSpace;
import java.awt.image.BandedSampleModel;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;
import java.awt.image.IndexColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.Transparency;
import java.io.File;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.io.BufferedReader;

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
import com.sun.media.jai.codec.SeekableStream;
import com.sun.media.jai.codec.ImageCodec;

import javax.media.jai.*;

import javax.imageio.stream.*;

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
public class ISISImageReader extends ImageReader {

    private boolean debug = false;
	// private boolean debug = true;
    /**
     *
     * VicarIO specific variables
     *
     * used to get file info 
     */
     
    // private VicarInputFile vif; // this may become ISISInputFile or RawInputFile
    private ISISInputFile iif;
    private SystemLabel sys; // we need a system label object for all the reader routines
    
    private SeekableStream seekableStream;
    // vicarIO currently uses SeekableStream
    // may transition to ImageInputStream ?????

    private ImageInputStream stream; 
    private DataInputStreamWrapper inputStreamWrapper;
    DataInputStream pixelStream = null;
    BufferedReader bufferedReader = null;

    BufferedImage theImage = null;
    
    private boolean haveReadHeader = false;
    
    boolean gotHeader = false;
    boolean gotMetadata = false;

    ISISMetadata pdsMetadata = new ISISMetadata();
    ImageReadParam lastParam = null;
    
    Document document = null ;
    
    

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
    public ISISImageReader (ImageReaderSpi mySpi) {
        super(mySpi);
        if (debug)     
          System.out.println("ISISImageReader constructor");
    }

   
	public void setInput(Object input, boolean isStreamable, boolean ignoreMetadata) {
		System.out.println("setInput %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
        super.setInput(input, isStreamable, ignoreMetadata);
        setInputInternal(input);
		// this.setInput(input, isStreamable);
	}
	
	public void setInput(Object input, boolean isStreamable) {
		System.out.println("setInput &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&");
        super.setInput(input, isStreamable, false); // true for ignoreMetadata ??
        setInputInternal(input);
		
	}
	
	public void setInput(Object input) {
		System.out.println("setInput ##############################################");
        super.setInput(input, false, false);
        setInputInternal(input);		
	}
    /**
     * Enforce that the input must be an <code>ImageInputStream</code>
     */
    public void setInputInternal(Object input) {
        // super.setInput(input, isStreamable);
        /** we will need to see if the VicarIO needs to be changed to use 
         * ImageInputStream
         */
        debug = true;
        System.out.println("*********************************************");
        System.out.println("ISISImageReader.setInputInternal "+input);
        
        if (input instanceof ImageInputStream ) {
            if (debug)     
              System.out.println("input is instanceof ImageInputStream ++++++++++++++");
            this.stream = (ImageInputStream)input;
            
            // the ISIS reader wants BufferedReader or ImageInputStream
            // anything which has a readLine() method
            // this.bufferedReader = new BufferedReader(new InputStreamReader(input)) ;
            // VicarIO wants an InputStream
            // ImageInputStream extends DataInput
            this.inputStreamWrapper = new DataInputStreamWrapper((DataInput)input);
        } else if ( input instanceof FileImageInputStream) {
            if (debug)     
              System.out.println("input is instanceof FileImageInputStream ++++++++++++++");
            this.stream = (ImageInputStream)input;
            
            // the ISIS reader wants BufferedReader or ImageInputStream
            // anything which has a readLine() method
            // this.bufferedReader = new BufferedReader(new InputStreamReader(input)) ;
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


	/**
	 * This method returns a RenderedImage. This is useful in at least 2 situations.
	 * 1) The data for a RenderedImage is not grabbed until asked for by an application.
	 * A tiled image will only grab the tiles needed. In a file copy the whole image wouldn't
	 * need to be loaded into memory. It could be copied tile by tile.
	 * 2) The image has more than 3 bands. It can't be returned as a BufferedImage since no
	 * ColorModel exists for > 3 bands. The user can extract bands for display using ImageOps.
	 * 
	 * added Steve Levoe 2-2003
	 */
	public RenderedImage readAsRenderedImage(int imageIndex, ImageReadParam param)

        throws IIOException {
            
        if (debug) System.out.println("IsisImageReader.readAsRenderedImage()");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }
        if (haveReadHeader == false) {
            readHeader();
        }

		if (debug) 
        	System.out.println("IsisImageReader.readAsRenderedImage() after readHeader() ");
        // printParam(param);
        // look at the param, decide what to do.
        // for now ignore it
        // boolean parameterizedRead = false;
        
        
        // create a VicarRenderedImage using the input stream and the SystemLabel obtained by readHeader()
         
        VicarRenderedImage image = null ;
        if (imageIndex != 0) {
            throw new IIOException("Illegal page requested from a Vicar image.");
        }
        
        try {
            image = new VicarRenderedImage(iif, param);
        }
        catch (Exception e) {
            System.err.println("readAsRenderedImage ERROR: "+e);
        }
        
        if (debug) { 
        	System.out.println(" vif "+iif);
        	System.out.println(" image "+image);
        }
        
         
       

        if (debug) 
        	System.out.println("IsisImageReader.readAsRenderedImage() after readHeader() ");
        // return vri ;
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
    private void readHeader() throws IIOException {
        
        
        
        if (debug)     
           System.out.println("readHeader");
        if (stream == null && seekableStream == null && inputStreamWrapper == null) {
        	
        	Object input = getInput();
        	System.out.println("ISISImageReader.readHeader input "+input);
            throw new IllegalStateException ("Input stream not set");
        }
        
        
        /********
        
        // isisInputFile
        ****/
        
        
        try
        {
            // for now use seekableStream
            // vif = new VicarInputFile();
            iif = new ISISInputFile();
            
            
            // System.out.println("input="+input);
            // System.out.println("seekableStream="+seekableStream);
            if (debug)     
              System.out.println("stream="+stream);
	        // vif.open(input); // this causes the file to be read and data structures to filled
	        if (stream != null) {
	            if (debug)     
                  System.out.println("stream " + stream.getClass().getName()+" ******************* ");
	            // vif.open( stream); // vif should be able to deal with ImageInputStream
	            iif.open( stream);
	        }
	        else if (inputStreamWrapper != null) {
	            if (debug)     
                  System.out.println("inputStreamWrapper " + inputStreamWrapper.getClass().getName());
	            
	            // vif.open(inputStreamWrapper);
	            iif.open(inputStreamWrapper);
	            // public void open(InputStream is) throws IOException
	            // public synchronized void open(InputStream is, boolean sequential_only) throws IOException
	        }
	        else if (seekableStream != null) {
	            if (debug)     
                  System.out.println("seekableStream " + seekableStream.getClass().getName());
	            // vif.open(seekableStream);
	            iif.open(seekableStream);
	        }
	       
	       sys = iif.getSystemLabel();
	       
	       String format = sys.getFormat();
	       String org = sys.getOrg(); 
	       int nb = sys.getNB();
	       
	       /***
	       if (format.equals("BYTE") && org.equals("BSQ") && nb == 1) {
	            int biType = BufferedImage.TYPE_BYTE_GRAY;
	            imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
	            System.out.println("ISISImageReader.readHeader() imageType "+imageType);
	       } else {
	        imageType = null;
	       }
	       ***/
	       boolean imageTypeRetryFailed = false;
	       
	       if (format.equals("USHORT") && org.equals("BSQ") && nb == 1) {
				int biType = BufferedImage.TYPE_USHORT_GRAY;
				imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
				if (debug) System.out.println("ISISImageReader.readHeader() imageType "+imageType);
			} else if (format.equals("BYTE") && org.equals("BSQ") && nb == 1) {
				int biType = BufferedImage.TYPE_BYTE_GRAY;
				imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
				if (debug) System.out.println("ISISImageReader.readHeader() imageType "+imageType);
	       
	       } else {
			 
	       	// this handles "HALF" 
			SampleModel sampleModel = iif.createSampleModel(width, height);
			ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
			
			if (debug) {
			   System.out.println("after iif.createSampleModel() ");
			   System.out.println("sampleModel "+sampleModel);
			   System.out.println("colorModel "+colorModel);
			}
			
			 try {
				if (imageType == null && imageTypeRetryFailed == false) { 
	       	 		imageType = new ImageTypeSpecifier(colorModel, sampleModel) ;
				}
			 }
			 catch (IllegalArgumentException iae) {
			 	if (debug) {
			 		System.out.println("ISISImageReader.readHeader() ImageTypeSpecifier ");
			 		System.out.println("IllegalArgumentException "+iae);	
			 	}
				imageType = null;	
			 }
			
	       }
	       
	       if (debug)  {  
        
	        System.out.println("ISISImageReader.readHeader() after iif.open() !@#$%^&*()+");
	        // System.out.println(iif.getVicarLabel().toString());

	        System.out.println("System label:"+sys);
	        // System.out.println(sys.toString());
            // readHeader(input);
            System.out.println("--------------- ISISFile opened OK");
            
           }
            // input.close(); // we keep the stream around so we can read in the data
        }
        /**
        catch (IOException ex)
        {
            System.out.println("IOException Error reading header:"+ex.getMessage());
            ex.printStackTrace();
            return;
        } **/
        catch (Exception ex)
        {
            System.out.println("Exception Error reading header:"+ex.getMessage());
            ex.printStackTrace();
            return;
        }
        
        
       document = iif.getISISDocument(); // should use accessor instead
       if (document != null) {
            if (debug) {
            	System.out.println("ISISImageReader new ISISMetadata with document");
            	System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
            }
            pdsMetadata = new ISISMetadata(document);
            gotMetadata = true;
            // return ;
       } else {
       		if (debug) {
            	System.out.println("no document avaiable from iif");
            	System.out.println("ISISImageReader NO ISISMetadata ************");
       		}
            return;
       }
       
       if (debug)  System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
        
        
       // ISISLabelToDOM(BufferedReader input, PrintWriter output);
       // creaste a BufferedReader from whatever input type we have
       
       /***
       BufferedReader input = null;
       if (stream instanceof ImageInputStream) {
            ISISLabelToDOM pdsLabel2Dom = new ISISLabelToDOM((ImageInputStream) stream, null);
            document = pdsLabel2Dom.getDocument();
            System.out.println("ISISImageReader new ISISMetadata with document");
            System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
            pdsMetadata = new ISISMetadata(document);
            gotMetadata = true;
            // return ;
       } else {
            System.out.println("Improper input type, can't read the header "+input);
            System.out.println("ISISImageReader NO ISISMetadata ************");
            return;
       }
       
        System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
        ***/
        
        
        if (debug) System.out.println("*** end of ReadHeader *****");
        haveReadHeader = true;
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
    }
    
    
    
    
    public String getFormatName()  throws IIOException {
        
        return "isis";
    }
    
    
    // all of the rest is inherited from Vicar or PDSImageReader

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
     * Uses the default implementation of ImageReadParam.
     */
    public ImageReadParam getDefaultReadParam() {
        return new ImageReadParam();
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
        return pdsMetadata;
    }

    public void printParam(ImageReadParam param) {
        System.out.println("------------------------------------------");
        
        if (param == null) {
           System.out.println("param = null");
           } 
        else {
           System.out.println("param "+param);
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

        throws IIOException {
            
        if (debug) System.out.println("ISISImageReader.read()");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }
        if (haveReadHeader == false) {
            readHeader();
        }

        printParam(param);
        boolean parameterizedRead = false;

        if (debug) System.out.println("ISISImageReader.read() after readHeader() ");
        /**
         * create a BufferedImage for the entire image
         * tiling stuff will come later
         **/
         
         // get these values from ?????
         int width = sys.getNS();
         int height = sys.getNL();
         int imageWidth = width;
         int imageHeight = height ;
         int startX = 0; // starting x to begin reading FROM the file
         int startY = 0; // starting y to begin reading FROM the file
         int x_off = 0; // x offset into sample model to place data (origin)
         int y_off = 0; // y offset into sample model to place data (origin)
         
         if (debug) System.out.println("image is "+width+"x"+height);
         
         if (debug) System.out.println("ISISImageReader.read() imageType "+imageType);
         BufferedImage theImage = null;
         SampleModel sampleModel = null;
         ColorModel colorModel = null;
         
         Point origin = new Point(0, 0);
         if (param != null) {
            Rectangle sourceRegion = param.getSourceRegion();
            if (sourceRegion != null) {
                width = sourceRegion.width;
                height = sourceRegion.height;
                // origin = new Point(sourceRegion.x,sourceRegion.y);
                startX = sourceRegion.x;
                startY = sourceRegion.y;
                parameterizedRead = true;
                }
            }
         
         if (parameterizedRead == false && imageType != null) {
            if (debug)  System.out.println("imageType "+imageType);
            // let imageTypeSpecifier create the buffered iamge for us
            theImage = imageType.createBufferedImage(width, height);
            sampleModel = theImage.getSampleModel();
            colorModel = theImage.getColorModel();
         }
         else {
            
         
         /**/
         
         
         /***/
         // get the SampleModel ColorModel Raster and data buffer from vicarIO
         if (debug) System.out.println("iif.createSampleModel("+width+","+height+")");
         
         sampleModel = iif.createSampleModel(width, height);
         // SampleModel sampleModel = new 
         /* create my own sample model to test if this is the problem with the 
         * current reader (slooooowwwwwww)
         */
         if (debug) {
         	System.out.println("after iif.createSampleModel()");
         	System.out.println("sampleModel "+sampleModel);
         }
         // RenderedImage renderedImage
         // will vicarIO create a RenderedImage for us???
         
        // public BufferedImage createBufferedImage(int width, int height) {
        WritableRaster raster = Raster.createWritableRaster(sampleModel, new Point(0, 0));
        // WritableRaster raster = Raster.createWritableRaster(sampleModel, origin);
        
        
        // get the number of bands
        int bands = sampleModel.getNumBands();
        if (bands <= 3) {
       
        	colorModel = ImageCodec.createComponentColorModel(sampleModel);
        	if (debug) {
         		System.out.println("bands="+bands+"  colorModel "+colorModel);
         		// can't create a colorModel for something with a weird
         	}
        	theImage = new java.awt.image.BufferedImage(colorModel, raster,
                                 colorModel.isAlphaPremultiplied(),new Hashtable());
         }
         else { // bands > 3 can't create a color model
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
         	// the BuffereImgae will be from the child so a ColorModel can be created
         	WritableRaster childRaster = raster.createWritableChild(parentX, parentY, w, h,
         		 childMinX, childMinY, bandList);
         	
         	// is the sampleModel valid ??
         	// SampleModel childSM = new SampleModel();
         	System.out.println("sampleModel "+sampleModel);
         	int dataType = sampleModel.getDataType();
         	
         	if (dataType == DataBuffer.TYPE_SHORT) {
       			// what do we do with SHORT ?? the convenience method can't create a colorModel
       			// to override the choice of the ColorModel
       			if (bands == 3) {
					int[] bits  = {16, 16, 16};
				
					ColorSpace colorSpace = ColorSpace.getInstance(ColorSpace.CS_sRGB);
					colorModel = new ComponentColorModel(colorSpace, bits, 
						false, false, Transparency.TRANSLUCENT, DataBuffer.TYPE_SHORT);
       			}
       			else if (bands == 1) {
					int[] bits  = {16};
				
					ColorSpace colorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY);
					colorModel = new ComponentColorModel(colorSpace, bits, 
						false, false, Transparency.TRANSLUCENT, DataBuffer.TYPE_SHORT);
       			}

       		}
         	else {
         		colorModel = PlanarImage.createColorModel(sampleModel);
         	}
         	
         	// colorModel = ImageCodec.createComponentColorModel(sampleModel);
         	System.out.println("colorModel "+colorModel);
         	if (colorModel == null) {
         		
         		w = sampleModel.getWidth();
         		h = sampleModel.getHeight();
         		int b = 3;
         	
         		BandedSampleModel fakeSampleModel = new BandedSampleModel(dataType, w, h, b);
         		// System.out.println("childSM "+ childSM);
         	
         		// create a bogus colorModel just to get by
         		colorModel = PlanarImage.createColorModel(fakeSampleModel);
         		// colorModel = ImageCodec.createComponentColorModel(fakeSampleModel);
         		System.out.println("colorModel (fake) "+colorModel);
         	}
         	
         	
         	// alternate simpler way ???
         	/**
         	theImage = new java.awt.image.BufferedImage(w, h, BufferedImage.TYPE_CUSTOM);
         	theImage.setData(raster);
         	// this one will NOT be displayable
         	**/
         	// childRaster
         	theImage = new java.awt.image.BufferedImage(colorModel, childRaster,
                                 colorModel.isAlphaPremultiplied(), new Hashtable());
                                
            if (debug) {
         		System.out.println("theImage "+theImage);
            }
         	
         	
         }
 
        }
        
        DataBuffer buf = theImage.getRaster().getDataBuffer();
        
        // this tile happens to be the whole image 
        if (debug) System.out.println("ISISImageReader.read() >>> vif.readTile >>>>>>>");
        try {
            if ( startX != 0 || startY != 0 || width < imageWidth || height < imageHeight) {
                // read a subarea of the image
                if (debug) System.out.println("readTile "+startX+","+ startY+" "+ width+"x"+ height+
                    " from "+imageWidth+"x"+imageHeight);
                iif.readTile(startX, startY, width, height, x_off, y_off, sampleModel, buf);
                }
            else { // standard read of the entire tile
                if (debug) System.out.println("readTile BASIC");
                iif.readTile(0,0, sampleModel, buf);
                }
            
        } 
        
        catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("IOException occured while reading ISIS image file");
        } 
        
        if (debug) {
        	System.out.println("ISISImageReader.read() completed");
        	System.out.println("sampleModel="+sampleModel);
        	System.out.println("colorModel="+colorModel);
        }
        return theImage;
    }
    
    /**
     * ISIS images do not have thumbnails.
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

