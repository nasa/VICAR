/*
 * @(#)FITSImageReader.java	1.0 
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 6-2003 ImageIO 1.4 version
 * 
 * Uses the java fitsio libraries from heasarc at gsfc
 */

package jpl.mipl.io.plugins;

import jpl.mipl.io.util.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import java.awt.Rectangle;
import java.awt.color.*;
import java.awt.image.BandedSampleModel;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferShort;
import java.awt.image.DataBufferUShort;
import java.awt.image.IndexColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
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
import java.util.Vector;
import java.io.BufferedReader;
import java.awt.image.renderable.ParameterBlock;

import javax.imageio.IIOException;
import javax.imageio.ImageReader;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.metadata.IIOMetadata; 
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

import com.sun.imageio.plugins.common.InputStreamAdapter; 
import com.sun.imageio.plugins.common.SubImageInputStream;



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

// FITSIO libraries
import nom.tam.fits.*;
import nom.tam.image.*;
import nom.tam.util.*;
    
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
 * for reading image files in the FITS format.
 *
 * @version 0.5
 */
public class FITSImageReader extends ImageReader {

    private boolean debug = false;
	// private boolean debug = true;
    
    Fits fits = null; // this is the fits object, everything comes from it
    
    // BasicHDU[] hdus ; // an array of HDU's from the file, most will contain 1 ??? 
    Object[] hdus ; // an array of HDU's from the file, most will contain 1 ??? 
    
    private SeekableStream seekableStream;
    
    private ImageInputStream stream; 
    
    private DataInputStreamWrapper inputStreamWrapper;
    DataInputStream pixelStream = null;
    BufferedReader bufferedReader = null;

    BufferedImage theImage = null;
    
    private boolean haveReadHeader = false;
    
    boolean gotHeader = false;
    boolean gotMetadata = false;

    FITSMetadata fitsMetadata = new FITSMetadata();
    ImageReadParam lastParam = null;
    
    Document document = null ;
    
    String keyTag = "key";
    String quoted = "false";
    
    boolean inputIsSet = false;
    
    // control if the image is flipped
    // this needs to be a part of an ImageReadParam
    // the fits library also supports sub image return, 
    // the generic ImageReadParam can ask for that some day
    boolean flip_image = true;

    /**
     * The header is kept as array of Strings, one for each token
     * A comment is defined as a single token.
     * All tokens are preserved, in the order they appear in the stream
     */
    private List header = new ArrayList();
    
    
    private int type; // Redundant, for convenience
    private int bitDepth; // Redundant, for convenience
    private boolean isBinary; // Redundant, for convenience
    private ImageTypeSpecifier imageType = null; // Redundant, for convenience
    private int imageIndex = 0;
    // private int numBands;
    private int width;
    private int height;
    private int maxGray;
    private long streamPos;
    
    // fits specific arrays, one for each image in the file (may only be 1)
    int hduCount = 0;
    int[] bitpix = {0};
    int[][] naxis = null;
    int[] numBands = {0};
    int[] nl = {0};
    int[] ns = {0};
    int[] bytesPerPixel = {0};
    
    FITSRenderedImage fitsRenderedImage = null;
    
    /**
     * Constructor taking an ImageReaderSpi required by ImageReaderSpi.
     */
    public FITSImageReader (ImageReaderSpi mySpi) {
        super(mySpi);
        if (debug)     
          System.out.println("FITSImageReader constructor");
    }

   
	public void setInput(Object input, boolean isStreamable, boolean ignoreMetadata) {
		if (debug) System.out.println("setInput %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
        super.setInput(input, isStreamable, ignoreMetadata);
        setInputInternal(input);
		// this.setInput(input, isStreamable);
	}
	
	public void setInput(Object input, boolean isStreamable) {
		if (debug) System.out.println("setInput &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&");
        super.setInput(input, isStreamable, false); // true for ignoreMetadata ??
        setInputInternal(input);
		
	}
	
	public void setInput(Object input) {
		if (debug) System.out.println("setInput ##############################################");
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
        // debug = true;
        if (debug) {
        	System.out.println("*********************************************");      
        	System.out.println("FITSImageReader.setInputInternal "+input+"   inputIsSet="+inputIsSet);
        }
        inputIsSet = true;
        /*
         * Fits(java.io.File myFile) 
          Associate FITS object with an uncompressed File 
			Fits(java.io.File myFile, boolean compressed) 
          Associate the Fits object with a File 
			Fits(java.io.InputStream str) 
          Create a Fits object associated with the given uncompressed data stream. 
			Fits(java.io.InputStream str, boolean compressed) 
          Create a Fits object associated with a possibly compressed data stream. 
			Fits(java.lang.String filename) 
          Associate the FITS object with a file or URL. 
			Fits(java.net.URL myURL) 
          Associate the FITS object with a given uncompressed URL 
			Fits(java.net.URL myURL, boolean compressed) 
          Associate the FITS object with a given URL 
         */
        
        
      try {
        if (input instanceof ImageInputStream ) {
            if (debug)     
              System.out.println("input is instanceof ImageInputStream ++++++++++++++");
            
            this.inputStreamWrapper = new DataInputStreamWrapper((DataInput)input);
            fits = new Fits((InputStream) inputStreamWrapper);
        } else if ( input instanceof FileImageInputStream) {
            if (debug)     
              System.out.println("input is instanceof FileImageInputStream ++++++++++++++");
            
            this.inputStreamWrapper = new DataInputStreamWrapper((DataInput)input);
            fits = new Fits((InputStream) inputStreamWrapper);
        } else if (input instanceof File) {
            if (debug)     
               System.out.println("input is instanceof File ");
            fits = new Fits((File) input);
        } else if (input instanceof String) {
            if (debug)     
               System.out.println("input is instanceof String ");
            fits = new Fits((String) input);
        } else if (input instanceof java.net.URL) {
            if (debug)     
               System.out.println("input is instanceof File ");
            fits = new Fits((java.net.URL) input);
        } else {
        	// throw exception for unsupported input type
        	// get the class of input
        	inputIsSet = false;
        	Class c = input.getClass();
        	throw (new FitsException("unsupported FITS input type: " + c.getName() ));
        	
        }
       }
       catch (FitsException fe) {
        	System.out.println("FitsException: "+fe);
        	fe.printStackTrace();
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
	 * 2015 - new version of the FITSIO package now supports tiled image access
	 * Add tiled image read here. previously we could only read the entire file which works 
	 * fine as a BufferedImage
	 */
	public RenderedImage readAsRenderedImage(int ii, ImageReadParam param)	
        throws IIOException {
            
        if (debug) {
        	System.out.println("===================================================================================");
        	System.out.println("FITSImageReader.readAsRenderedImage("+ii+") haveReadHeader "+haveReadHeader+" ********************");
        }
        
        
        if (haveReadHeader == false) {
            readHeader();
        }

		if (debug) {
        	System.out.println("FITSImageReader.readAsRenderedImage() after readHeader() imageindex = "+ii);
            printParam(param);
		}
        // look at the param, decide what to do.
        // for now ignore it
        // boolean parameterizedRead = false;
		// look into doing a subimage read based on the read param, crop and subsample
		// scale values to float using scale and offset
        
        RenderedImage image = null;
        boolean rescaleFITS = false ; // this should come from the param
        // for 16 bit data if zero = 32768.0 then the data is really UNSIGNED short but FITS stores it as signed
        // to have the true data values the data should be rescaled and used as USHORT
        // for now we will leave it as signed SHORT to use for display purposes

        boolean parameterizedRead = false;
        
        int i=0; // how do I use imageIndex ?? 
        // srl 1-2015 attempt to read beyond first image
		if (ii < hduCount) {
			imageIndex = ii;
		} else {
			// throw an error/Exception and return??
			imageIndex = 0;
			if (debug) {
				System.out.printf("FITSImageReader.readAsRenderedImage() imageIndex = %d is too big\n hduCount=%d hdus.length=%d \n",
			          ii, hduCount, hdus.length);
			}
			return image;
			// throw new IndexOutOfBoundsException ();
		}
		// how do I know how many images are in this file ???
		if (debug) {
			System.out.printf("imageIndex = %d hduCount=%d hdus.length=%d \n",
					imageIndex, hduCount, hdus.length);
		}
		
		BasicHDU hdu = (BasicHDU) hdus[imageIndex];
		// setup the renderedImage for the HDU for the image we want
		if (debug) System.out.printf("FITSImageReader.readAsRenderedImage() new FITSRenderedImage(hdu, param, debug); \n");
		fitsRenderedImage = new FITSRenderedImage(hdu, param, debug);
        
		return fitsRenderedImage;
        // return image;
        }
    
	
	/**********************************
	 * setDebug
	 * sets the debug flag for this file
	 * @param d
	 */
	public void setDebug( boolean d) {
		debug = d;
	}
	
	
	/**
	 * FITS images are stored upside down compared to VICAR, PDS and ISIS
	 * This controls if the image gets flipped so the image is properly 
	 * oriented.
	 * This should be a part of the ImageReadParam
	 * on the To-do list
	 * @param flip
	 */
	public void setFlip_image(boolean flip) {
		
		flip_image = flip;
	}
	
	public boolean getFlip_image() {
		
		return flip_image;
	}
	
    /**
     * Reads the FITS headers, HDUs. They are stored in an array for later use
     * Metadata and image data are retrieved from the HDU.
     */
    private void readHeader() throws IIOException {
        
        
        if (fits == null) {
        	System.out.println("readHeader... FITS object is null");
        	// throw an exception
        	return;
        }
        
        if (debug)     
           System.out.println("readHeader haveReadHeader="+haveReadHeader);
        
        if (haveReadHeader) return ; // header has already been read, don't do it again
        
        BasicHDU hdu = null;
        
        Vector hduVector = new Vector();
        int i=0;
        // get all the HDU's from the file, later we'll get metadata and images
        try {
          do {
        	hdu = fits.readHDU();
        	if (hdu != null) {
        		hduVector.add(hdu);
        	}
        	
          i++;	
          } while (hdu != null) ;
        }
        catch (IOException ioe) {
        	System.out.println("IOException: "+ioe);
        	ioe.printStackTrace();
        	haveReadHeader = false;
        	throw new IIOException("IOException in FitsImageReader.readHeader() ");
        	// return;
        }
        catch (FitsException fe) {
        	System.out.println("FitsException: "+fe);
        	fe.printStackTrace();
        	haveReadHeader = false;
        	throw new IIOException("FitsException in FitsImageReader.readHeader() ");
        	// return;
        }
        // convert Vector to array ??? or just use the Vector as is
        // hdus = (BasicHDU[]) hduVector.toArray();
        hdus = hduVector.toArray();
        
        
        // get the metadata now?? or when user request it
        
        
        ImageHDU imageHDU;
        if (debug) {
        	System.out.printf("HDU hdus.length %d fits.getNumberOfHDUs %d verion %s \n", hdus.length, fits.getNumberOfHDUs(), fits.version());
        	
        	for (int ii=0 ; ii< hdus.length ; ii++) {
        		System.out.printf("hdu.info(%d) ", ii);
        		try {
					hdu = fits.getHDU(ii);
					System.out.println("hdu "+hdu);
					if (hdu instanceof nom.tam.fits.ImageHDU) {
						imageHDU = (ImageHDU) hdu;
						System.out.println("instanceof imageHDU "+imageHDU);	
						nom.tam.image.StandardImageTiler tiler = imageHDU.getTiler();
						imageHDU.info(System.out);
												
						int bitpix = imageHDU.getBitPix();
					    
				 		int[] naxis = imageHDU.getAxes();
				 		if (debug) {
				 			System.out.println("bitpix="+bitpix);
				 			System.out.println("naxis="+naxis.length);				 		
				 			for (int j=0 ; j<naxis.length ; j++) {
				 				System.out.println("naxis["+j+"]="+naxis[j]);
				 			}
				 		}
				 		
				 		// how do we find out if the image is compressed??
				 		double min = imageHDU.getMinimumValue();
				 		double max = imageHDU.getMaximumValue();
				 		double scale = imageHDU.getBScale();
				 		double zero = imageHDU.getBZero();
				 		String units = imageHDU.getBUnit();
				 		System.out.println("min "+min+"  max "+max+"  scale "+scale+"  zero "+zero +" units "+units);				 		
					}
					// hdu.info();
					// hdu.info(System.out)
				} catch (FitsException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}     		
        	}
        	System.out.println("*** end of ReadHeader *****");
        }
        getImageStuff(); // sets up arrays of nl,ns, numBands, bytesPerPixel
        haveReadHeader = true;
        
        // printHduInfo();
    }

    


    /**
    * Just get the HDUs from the read. 
    * The metadata trees will be generated when they are requested from the 
    * FITSMetadata class. 
    **/
    private void readMetadata() throws IIOException {
        if (gotMetadata) {
            return;
        }
        
        if (haveReadHeader == false) {
            readHeader();
        }
        
        
        hduCount = hdus.length;
        int cardCt = 0;
    	// int bitpix = new int[hduCount];
    	// int[] naxis = null;
        if (debug) {
        	System.out.println("#######################################");
        	System.out.println("###  HDU                           ####");
        	System.out.println("#######################################");
        	System.out.println("readMetadata hduCount="+hduCount);
        	System.out.println("FITSMetadata.nativeImageMetadataFormatName "+FITSMetadata.nativeImageMetadataFormatName);
        }
    	
    // create a Document for the metadata object to use
        DOMutils domUtils = new DOMutils();
        document = domUtils.getNewDocument();
        
        Element root = (Element) document.createElement(FITSMetadata.nativeImageMetadataFormatName);
        document.appendChild(root);
        Element item, element;
        Node currentNode = root;
        
        Element hdu_count_element = document.createElement("hdu_count");
        Text text = (Text) document.createTextNode(""+hduCount);
        hdu_count_element.appendChild(text);
		root.appendChild(hdu_count_element);
        
    for (int i=0 ; i< hduCount ; i++) { 
    	
    	// try {
    	BasicHDU hdu = (BasicHDU) hdus[i];
    	Element hdu_element = document.createElement("HDU");
    	String hduName = hdu.getClass().getSimpleName();
    	if (debug)  System.out.printf("readMetadata %d %s %s\n", i, hduName, hdu.getClass().getName());
    	hdu_element.setAttribute("name", hduName);
    	hdu_element.setAttribute("index", ""+i);
    	root.appendChild (hdu_element);
    	currentNode = hdu_element;
    	// System.out.println(i+") info -------------------------");
    	// hdu.info();
    	Header header = hdu.getHeader();
    	cardCt = header.getNumberOfCards();
    	if (debug) System.out.println(i+") hdu header has "+cardCt+" cards");
    	Cursor iter = header.iterator();
    	int ci = 0;
    	
    	while (iter.hasNext()) {
    		HeaderCard hc = (HeaderCard) iter.next();
    		String key = hc.getKey();
    		String value = hc.getValue();
    		String comment = hc.getComment();
    		int clen = 0;
    		int klen = 0;
    		if (comment != null) {
    			comment.trim();
    			clen = comment.length();
    		}
    		
    		if (key != null) {
    			key.trim();
    			klen = key.length();
    			if (klen != 0) {
    				if (debug) System.out.println("   "+ci+ ") "+key+" = "+value);
    				ci++;
    			}
    		}
    		if (comment != null && clen != 0) {
    			if (debug) System.out.println("     comment #"+comment+"# "+clen);
    		} else {
    			comment = "";
    		}
    		
    		if (value == null) {
    			value = "";
    		}
    		    		
    		if (key != null && !key.equals("")) {
    		  element = (Element) document.createElement("item");
    		  element.setAttribute(keyTag,key);
    		  element.setAttribute("comment",comment);
    		  text = (Text) document.createTextNode(value);
    		  element.appendChild(text);
    		  currentNode.appendChild(element);
    		  // element.setAttribute("quoted",quoted);
    		}
    		   		
    	}
    		
    	
    	/*
    	 * }
    	 catch (FitsException fe) {
    		
    		System.out.println("FitsException: "+fe);
    		fe.printStackTrace();
    	}
    	 **/
       }
    
    
    /**
     * at this point we should have a completed document
     * create a new FITSmetadata object from the document
     */
    if (document != null) {
    	fitsMetadata = new FITSMetadata(document);
    	// print the XML file out
    	if (debug) {
    		String xmlNodeFile = "fitsNode.xml";
    		String xmlDocFile = "fitsDoc.xml";
    		// document serialization error, figure it out
    		// if we make a valid XML we could make a transcoder and writer
    		// XML useful so a user could determine number of images in the file
    		domUtils.printDocInfo(document);
    		
    		Node firstChild = document.getFirstChild();
    		
    		domUtils.serializeNode(firstChild, xmlNodeFile, "xml") ;
    		domUtils.serializeDocument(document, xmlDocFile, "xml");
    	}
    }
    else {
    	fitsMetadata = new FITSMetadata();
    }
    
     if (debug) {
     	System.out.println("#######################################");
     }
    }
    
    
    
    
    public String getFormatName()  throws IIOException {
        
        return "fits";
    }
    
    
    // does imageIndex correlate to hdus[i] ???
    // all the values will come from an HDU
    // there are Extention HDUs so HDU count may not be image count

    public int getNumImages() throws IIOException {
    	
    	if (haveReadHeader == false) {
    		
    		readHeader();
    	}
        
        return hdus.length;
                	
        // return 1; // Vicar always have just 1 ???
        // at least that's all we support now
    }
    
    
    /**
     * get the values for the image at the supplied index and 
     * set the globals to those values     * @param index     */
    // private void getImageStuff(int index)
    private void getImageStuff()
    { 	 
    	// if (index < hdus.length && index >= 0) {
    		
    	int bitpix = 0;;
    	int[] naxis = null;
    	
    	hduCount = hdus.length;
    	// int bitpix = new int[hduCount];
    	// int[] naxis = null;    	
   		numBands = new int[hduCount];
    	nl = new int[hduCount];
    	ns = new int[hduCount];
    	bytesPerPixel = new int[hduCount];
    	
    	
    for (int i=0 ; i< hduCount ; i++) { 
    		
    	BasicHDU hdu = (BasicHDU) hdus[i];
    	try {
    		bitpix = hdu.getBitPix();
    		naxis = hdu.getAxes();
    	}
    	catch (FitsException fe) {
    		imageIndex = -1; // check this to see if the values are for a valid image number
    		nl[i] = 0;
    		ns[i] = 0;
    		numBands[i] = 0;
    		bytesPerPixel[i] = 0;
    		System.out.println("FitsException: "+fe);
    		fe.printStackTrace();
    	}
    		
    /*
    int formatCode;
	int orgCode;	
	int i;
	int pixel_stride;
	int scanline_stride;
	boolean float_type;
	int tileWidth = 0;
	int tileHeight = 0;
	int data_buffer_type = DataBuffer.TYPE_BYTE;
	*/
		
	
		bytesPerPixel[i] = Math.abs(bitpix/8);
		// int bitpix = 8;
		// int naxis = 1;
		// int naxis1,nazis2,naxis3,naxis4;
	
	
    	int num_axis = 0;
    	if (naxis != null) {
    		num_axis = naxis.length;
    	
    
    	switch (num_axis) {
    	
    		case 2:
    		numBands[i] = 1;
    		nl[i] = naxis[0];
    		ns[i] = naxis[1];
    		break;
    		case 3:
    		numBands[i] = naxis[0];
    		nl[i] = naxis[1];
    		ns[i] = naxis[2];
    		break;
    		case 4:
    		numBands[i] = naxis[1];
    		nl[i] = naxis[2];
    		ns[i] = naxis[3];
    		// what to do with [3] ????
    		break;
    		case 0: // if we got an exception naxis is set to 0;
    		case 1:
    		// I can't image we'll ever get here
    		default:
    			numBands[i] = 0;
    			nl[i] = 0;
    			ns[i] = 0;
   				break;
    		}
    	
    		
    	} else {
    		numBands[i] = 0;
    		nl[i] = 0;
    		ns[i] = 0;
    		bytesPerPixel[i] = 0;
    	}
      }
    } 
    
    public int getWidth(int imageIndex) throws IIOException {
    	if (imageIndex >= hduCount) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        return ns[imageIndex];
        // return width;
        // return sys.getNS();
    }
    
    public int getHeight(int imageIndex) throws IIOException {
    	if (imageIndex >= hduCount) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        //  return height;
        // return sys.getNL();
        return nl[imageIndex];
    }


    // I think these are not useful
    public ImageTypeSpecifier getRawImageType(int imageIndex)
        throws IIOException {
    	if (imageIndex >= hduCount) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        
        // figure out what this means in the non-vicar context
        return imageType;
    }
    
    public Iterator getImageTypes(int imageIndex)
        throws IIOException {
    	if (imageIndex >= hduCount) {
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
        if (inputIsSet == false) {
            throw new IllegalStateException("No input source set!");
        }
        
        // check hdus.length; ??????????
        // srl 1-2015
        // return hduCount;
        // srl 11-15
        return getNumImages();
        // return 1;
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
    	if (imageIndex >= hduCount) {
            throw new IndexOutOfBoundsException("imageIndex != 0!");
        }
        readMetadata(); // make sure fitsMetadata has valid data in it
        return fitsMetadata;
    }

    public void printParam(ImageReadParam param) {
        System.out.println("------- FITSImageReader -----------------------------------");
        
        if (param == null) {
           System.out.println("ImageReadParam = null");
           } 
        else {
           System.out.println("ImageReadParam "+param);
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


 public BufferedImage read(int imageIndex, ImageReadParam param)

        throws IIOException {
            
        if (debug) {
        	System.out.println("FITSImageReader.read() imageIndex = "+imageIndex+"  haveReadHeader="+haveReadHeader);
        	// hduCount
        }
        // srl 1-2015 
        /**
        if (imageIndex >= hduCount) {
            throw new IndexOutOfBoundsException ();
        }
        **/
        if (haveReadHeader == false) {
            readHeader();
        }

        if (debug) printParam(param);
        boolean rescaleFITS = false ; // this should come from the param
        // for 16 bit data if zero = 32768.0 then the data is really UNSIGNED short but FITS stores it as signed
        // to have the true data values the data should be rescaled and used as USHORT
        // for now we will leave it as signed SHORT to use for display purposes
        boolean parameterizedRead = false;

        if (debug) System.out.println("FITSImageReader.read() after readHeader() imageIndex="+imageIndex+" hduCount="+hduCount);
        /**
         * create a BufferedImage for the entire image
         * tiling stuff will come later
         **/
         
         // does hdus[imageIndex] work correctly ???
         
         // get the image
         /**
         if (imageIndex < hdus.length) {
         	BasicHDU h = hdus[imageIndex];      	
         }
         **/
         
         
         
         int i=0; // how do I use imageIndex ?? 
      // srl 1-2015 attempt to read beyond first image
         if (imageIndex < hduCount) {
         	i = imageIndex;
         } else {
        	 // throw an error/Exception and return??
         }
         // how do I know how many images are in this file ???
         if (debug) {
        	 System.out.printf("imageIndex = %d i=%d hduCount=%d hdus.length=%d \n", imageIndex, i, hduCount, hdus.length);
         }
      // check that we can read something for this index ??
         
	BasicHDU h;
	int bitpix ;
	int[] naxis ;
	Object data, actualData;
	SampleModel sampleModel = null;
	ColorModel colorModel = null;
	// FitsReader fr = new FitsReader();
	// JFrameJade jFrameJade, jFrameJade2;
	DataBuffer dataBuffer = null;
	BufferedImage bi = null;
	int dataType;
	
	// do {
	    // h = f.readHDU();
	  try {
	    h = (BasicHDU) hdus[i];
	    if (h != null) {
	    	
	    	if (debug) {
	    		System.out.println("h is "+h);
	    		// h.info();
	    	}
	        // if (i == 0) {
	        if (i < hdus.length) {
		        if (debug) System.out.println("\n\nPrimary header:\n");
	        
		    
		    bitpix = h.getBitPix();
		    
	 		naxis = h.getAxes();
	 		if (debug) {
	 			System.out.println("bitpix="+bitpix);
	 			System.out.println("naxis="+naxis.length);
	 		
	 			for (int j=0 ; j<naxis.length ; j++) {
	 				System.out.println("naxis["+j+"]="+naxis[j]);
	 			}
	 		}
	 		
	 		// how do we find out if the image is compressed??
	 		double min = h.getMinimumValue();
	 		double max = h.getMaximumValue();
	 		double scale = h.getBScale();
	 		double zero = h.getBZero();
	 		String units = h.getBUnit();
	 		if (debug) {
	 		  System.out.println("###############################################################");
	 		  System.out.println(" ");
	 		  System.out.println("min "+min+"  max "+max+"  scale "+scale+"  zero "+zero +" units "+units);
	 		  // h.info();
	 		  System.out.println(" ");
	 		  System.out.println("###############################################################");
	 		}
	 		
	 		data = h.getData();
	 		if (debug) System.out.println("data "+data);
	 		if (data instanceof nom.tam.fits.ImageData) {
	 			
	 			ImageData id = (ImageData) data;
	 			int size = (int) id.getSize();
	 			sampleModel = createSampleModel(bitpix, naxis, zero);
	 			if (debug) {
	 				System.out.println("data is ImageData "+id);
	 				System.out.println("size is "+size);
	 				System.out.println("sampleModel "+sampleModel);
	 			}
	 			actualData = id.getData();
	 			
	 			short[][] s2 = new short[10][10];
	 			short[] s1= new short[10];
	 			
	 			
	 			
	 			if (debug) {
	 			  System.out.println("actualData "+actualData);
	 			  System.out.println("actualData "+actualData.getClass().getName());
	 			  System.out.println("s2 "+s2.getClass().getName());
	 			  System.out.println("s1 "+s1.getClass().getName());
	 			}
	 			
	 			int width  = sampleModel.getWidth();
	 			int height = sampleModel.getHeight();
	 			int bands  = sampleModel.getNumBands();
				int tType  = sampleModel.getTransferType();
				if (debug) System.out.println("sampleModel width="+width+"  height="+height+"  bands="+bands+"  tType="+tType);
	 			
	 			byte[] ba;
	 			
	 			short[] sa;
	 			float[] fa;
	 			int[] ia;
	 			double da[];
	 			int afSize = 0;
	 			
	 			if (debug) System.out.println("actualData: "+ArrayFuncs.arrayDescription(actualData) );
	 			afSize = ArrayFuncs.computeSize(actualData);
	 			
	 			int[] dim = ArrayFuncs.getDimensions(actualData);
	 			if (debug) {
	 				System.out.println("actualData dim "+dim.length);
	 			
	    			for (int k=0 ; k< dim.length ; k++) {
	    				System.out.println("actualData dim["+k+"] = " +dim[k]);
	    			}
	 			}
	 			
	 			switch (tType) {
	    			case DataBuffer.TYPE_BYTE:
	    			
	    			ba = (byte[]) ArrayFuncs.flatten(actualData);
	    			if (debug) {
	    				System.out.println("tType DataBuffer.TYPE_BYTE");
	    				System.out.println("ba "+ba.length+"  afSize="+afSize+"  size "+size);
	    			}
	    			
	    			dataBuffer = new DataBufferByte(ba, afSize);
					break;
					
	    			case DataBuffer.TYPE_SHORT:
	    			
	    			
	    			sa = (short[]) ArrayFuncs.flatten(actualData);	
	    			if (debug) {
	    				System.out.println("tType DataBuffer.TYPE_SHORT");
	    				System.out.println("sa "+sa.length+"  afSize="+afSize+"  size "+size);
	    			}
	    			
	    			dataBuffer = new DataBufferShort(sa, afSize);
	    			break;
	    			
	    			case DataBuffer.TYPE_USHORT:
	    			if (debug) System.out.println("tType DataBuffer.TYPE_USHORT");
	    			sa = (short[]) ArrayFuncs.flatten(actualData);	
	    			dataBuffer = new DataBufferUShort(sa, afSize);
					break;
					
	    			case DataBuffer.TYPE_INT:
	    				if (debug) {
	    					System.out.println("tType DataBuffer.TYPE_INT");
	    				}
	    			ia = (int[]) ArrayFuncs.flatten(actualData);	
		    		dataBuffer = new DataBufferInt(ia, afSize);
					break;
					
	    			case DataBuffer.TYPE_FLOAT:	
	    			
	    			fa = (float[]) ArrayFuncs.flatten(actualData);	
	    			if (debug) {
	    				System.out.println("tType DataBuffer.TYPE_FLOAT");
	    				System.out.println("fa "+fa.length+"  afSize="+afSize+"  size "+size);
	    			}
	    			dataBuffer = new java.awt.image.DataBufferFloat(fa, afSize);
					break;
					
	    			case DataBuffer.TYPE_DOUBLE:
	    					
	    			da = (double[]) ArrayFuncs.flatten(actualData);	    
	    			if (debug) {
	    				System.out.println("tType DataBuffer.TYPE_DOUBLE");
	    				System.out.println("da "+da.length+"  afSize="+afSize+"  size "+size);
	    			}
	    			dataBuffer = new java.awt.image.DataBufferDouble(da, afSize);
					break;
	    
				}
	 			// DataBuffer dataBuffer = sampleModel.createDataBuffer();
	 			// sampleModel.setDataElements(0,0,width,height,actualData,dataBuffer);
	 			
	 			/**
	 			if (tType == 1) {
	 				// short 
	 				short[] = new short actualData ;
	 			}
	 			**/
	 			
	 			if (dataBuffer != null && sampleModel != null ) {
	 			
	 			if (debug) System.out.println("try to create displayable FITS image");
	 			// createWritableRaster(SampleModel sm, Point location) 
	 			 // create Raster
	 			 
	 			// WritableRaster raster = Raster.createWritableRaster(sampleModel, new Point(0, 0));
	 			 
	 			 // do subset of bands ??
	 			 
	 			 // System.out.println("raster.setDataElements");
	 			 // raster.setDataElements(0,0,width,height,actualData);
	 			 // raster.
	 			 // Raster raster = Raster.createRaster(sampleModel, dataBuffer, new Point(0, 0) ) ;
                 WritableRaster raster = Raster.createWritableRaster(sampleModel, dataBuffer, new Point(0, 0));
                 if (debug) {
	 			    System.out.println("raster "+raster);
	 			    System.out.println("bands "+bands);
                 }
	 			 
	 			 WritableRaster newChildRaster = null;
	 			  if (raster != null && bands > 3 ) {
        	
        			if (debug) System.out.println("create new ChildRaster with 3 bands. bands "+bands);
        			int parentX = raster.getMinX();
         			int parentY = raster.getMinY();
         			int rw = raster.getWidth();
         			int rh = raster.getHeight();
         			int childMinX = parentX;
         			int childMinY = parentY;
         			int[] bandList = {0,1,2};
         	
         	
         			// the child should SHARE the parent's raster
         			// we will read data into the parent raster
         			// the BufferedImage will be from the child so a ColorModel can be created
         			newChildRaster = raster.createWritableChild(parentX, parentY, rw, rh,
         		 			childMinX, childMinY, bandList);
         		 			
         		 	//  a new sampleModel for the new Raster
         		 	sampleModel = newChildRaster.getSampleModel();
         		 	
         		 	width  = sampleModel.getWidth();
	 				height = sampleModel.getHeight();
	 				bands  = sampleModel.getNumBands();
					tType  = sampleModel.getTransferType();
	 				if (debug) {
	 					System.out.println("new sampleModel width="+width+"  height="+height+"  bands="+bands+"  tType="+tType);
	 				}
	 				
	 			  }
	 			 
	 			  			 
	 			 colorModel = PlanarImage.createColorModel(sampleModel);
	 			 if (debug) {
	 			   System.out.println("sampleModel "+sampleModel);
	 			   System.out.println("colorModel "+colorModel);
	 			 }
	 			 
	 			 dataType = sampleModel.getDataType();
	 			 
	 			 if (colorModel == null) {
        	
        	 		int transferType = sampleModel.getTransferType();
        	 		
        	
        	
        			if (debug) {
        				System.out.println("colorModel is NULL");
        				System.out.println("transferType "+transferType +"  dataType "+dataType);
        				System.out.println(" java.awt.image.BufferedImage.TYPE_CUSTOM "+ java.awt.image.BufferedImage.TYPE_CUSTOM);
        				System.out.println(" java.awt.image.BufferedImage.TYPE_BYTE_GRAY "+ java.awt.image.BufferedImage.TYPE_BYTE_GRAY);
        			}
      	 
        			if (dataType == DataBuffer.TYPE_BYTE) {
        				bi = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_BYTE_GRAY);
        			}
        			else if (dataType == DataBuffer.TYPE_USHORT) {
        				bi = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_USHORT_GRAY);
        				
        			}
        			
        			else if (dataType == DataBuffer.TYPE_SHORT) {
        				/*
        				public ComponentColorModel(ColorSpace colorSpace,
                           boolean hasAlpha,
                           boolean isAlphaPremultiplied,
                           int transparency,
                           int transferType)
                           Constructs a ComponentColorModel from the specified parameters. 
                           Color components will be in the specified ColorSpace. 
                           The supported transfer types are DataBuffer.TYPE_BYTE, 
                           DataBuffer.TYPE_USHORT, DataBuffer.TYPE_INT, DataBuffer.TYPE_SHORT, 
                           DataBuffer.TYPE_FLOAT, and DataBuffer.TYPE_DOUBLE. 
                           The number of significant bits per color and alpha component 
                           will be 8, 16, 32, 16, 32, or 64, respectively. 
                           The number of color components will be the number of 
                           components in the ColorSpace. There will be an alpha 
                           component if hasAlpha is true. If hasAlpha is true, then 
                           the boolean isAlphaPremultiplied specifies how to interpret 
                           color and alpha samples in pixel values. If the boolean is 
                           true, color samples are assumed to have been multiplied by the 
                           alpha sample. The transparency specifies what alpha values can 
                           be represented by this color model. The acceptable transparency 
                           values are OPAQUE, BITMASK or TRANSLUCENT. The transferType is 
                           the type of primitive array used to represent pixel values. 
						
							*/
						// java.awt.color.ICC_ColorSpace
						ColorSpace colorSpace;
						if (bands == 1) {
							
							colorSpace = ColorSpace.getInstance(ColorSpace.CS_GRAY);
							// colorSpace = new ColorSpace(ColorSpace.CS_GRAY, 1);
						}
						else {
							colorSpace = ColorSpace.getInstance(ColorSpace.CS_sRGB);
							// colorSpace = new ICC_ColorSpace(java.awt.color.ICC_ProfileRGB);
						}
						
						if (debug) {
						  System.out.println("SHORT ********************************************");
        				  System.out.println("SHORT ColorSpace "+colorSpace);
						}
        				// colorModel = ComponentColorModel(new java.awt.color.ICC_ProfileGray(), false, false, false, dataType );
        				// ComponentColorModel(ColorSpace colorSpace, boolean hasAlpha, boolean isAlphaPremultiplied, int transparency, int transferType)
        				colorModel = new ComponentColorModel(colorSpace, false, false, ColorModel.OPAQUE, dataType );
        				if (debug) System.out.println("SHORT ComponentColorModel "+colorModel);
        				if (newChildRaster != null) {
        					if (debug) System.out.println("using newChildRaster SHORT");
        					bi = new java.awt.image.BufferedImage(colorModel, newChildRaster, colorModel.isAlphaPremultiplied(), 
        					new Hashtable());
         				}
         				else {
         					if (debug) System.out.println("using raster SHORT");
        					bi = new java.awt.image.BufferedImage(colorModel, raster, colorModel.isAlphaPremultiplied(), 
        					new Hashtable());
         				}
        				
        				
                           
        			} 
        			else {
        			// theImage = new java.awt.image.BufferedImage(
        			// 	width, height, java.awt.image.BufferedImage.TYPE_USHORT_GRAY);
        			System.out.println("colorModel is NULL, dataType="+dataType);
        			System.out.println("We can't create a BufferedImage for this image");
        			// throw an exception???
        			// throw new RuntimeException("IOException occured while reading vicar image file, couldn't create BufferedImage");
        			}
        	
        	
        	
        		if (newChildRaster != null) {
        			if (debug) System.out.println("using newChildRaster *********");
        			bi.setData(newChildRaster);
        		}
        		else {
        			if (debug) System.out.println("using raster ************");
        			bi.setData(raster);
        		}
        	
        		// a Valid Color model MUST be supplied to this constructor or an Exception will be thrown
        		// theImage = new java.awt.image.BufferedImage(colorModel, raster, false, new Hashtable());
        	}
         	else {
         		if (newChildRaster != null) {
        			if (debug) System.out.println("using newChildRaster @@@@@@@@@@@@@@@");
        			bi = new java.awt.image.BufferedImage(colorModel, newChildRaster, colorModel.isAlphaPremultiplied(), 
        					new Hashtable());
         		}
         		else {
         			if (debug) System.out.println("using raster @@@@@@@@@@@@@@@");
        			bi = new java.awt.image.BufferedImage(colorModel, raster, colorModel.isAlphaPremultiplied(), 
        					new Hashtable());
         		}
         	}
	 			 
	 			 
	 			 
	 			 
	 			 // create BufferedImage
	 			 if (bi != null) {	 
	 			 	
	 			 	// this should be controlled by a ImageParam
					double constant[] = new double[]{1.0};
					double offset[] = new double[]{0.0};
	
					constant[0] = scale;
					offset[0] = zero * scale; // offset is added only for unsigned ??
					// offset[0] = zero; // offset is added only for unsigned ??
					// offset[0] = min * -1.0; // offset is added only for unsigned ??
	
					if (debug) {
						System.out.println("read  constant="+constant[0]+"  offset="+offset[0]);
					}
					
					//------------
					
					ParameterBlock PB=new ParameterBlock();
					PB.addSource(bi).add(null).add(1).add(1);
					RenderedImage extrema=JAI.create("extrema",PB);

					// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
					double escale[][]=(double[][])extrema.getProperty("extrema");
					// double ceiling=Byte.MAX_VALUE*2; // treat as unsigned
					
					double ceiling = getMaxForDataType(dataType) ;
	
	
	
					// double ceiling=Short.MAX_VALUE*2;
					double emax=0,emin=ceiling;
					for(int j=0;j<escale[0].length;j++){
							if (debug) {
								System.out.println(j+") scale[0][j] "+escale[0][j]  +"   scale[1][j] "+escale[1][j]);
							}
							if (escale[1][j] > 0) { emax=Math.max(emax,escale[1][j]); }
							else { emax = escale[1][j]; }
	    					emin=Math.min(emin,escale[0][j]);
					}
					if (debug) {
						System.out.println("before rescale extrema ceiling="+ceiling+"  emin="+emin+"  emax="+emax);
						System.out.println("   from FITS header  min="+min+"  max="+max);
					}
					//-------------
					
					PlanarImage temp;
					if (rescaleFITS) {
						// one rescale for FITS is to turn data stored as signed short to signed short
						// we'll need to create a new sample model, copy data ???
						// check for an operator that will do that for us
						if (debug) {
							System.out.println("rescaleFITS is TRUE.  scaling data");						
						}
					PB=new ParameterBlock();
					// PB.addSource(temp).add(new double[]{ceiling/(max-min)}).add(new double[]{ceiling*min/(min-max)});
					PB.addSource(bi).add(constant).add(offset);
					temp=JAI.create("rescale",PB);
					bi = temp.getAsBufferedImage();
					}
					
					// check the header to see if this information is in the header
					// also look for no data value
					
					//---------------------
					PB=new ParameterBlock();
					PB.addSource(bi).add(null).add(1).add(1);
					extrema=JAI.create("extrema",PB);

					// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
					escale=(double[][])extrema.getProperty("extrema");
					// double ceiling=Byte.MAX_VALUE*2; // treat as unsigned
					
					ceiling = getMaxForDataType(dataType) ;
		
					// double ceiling=Short.MAX_VALUE*2;
					emax=0;
					emin=ceiling;
					for(int j=0;j<escale[0].length;j++){
							if (debug) {
								System.out.println(j+") scale[0][j] "+escale[0][j]  +"   scale[1][j] "+escale[1][j]);
							}
							if (escale[1][j] > 0) { emax=Math.max(emax,escale[1][j]); }
							else { emax = escale[1][j]; }
	    					emin=Math.min(emin,escale[0][j]);
					}
					if (debug) {
						System.out.println("after rescale extrema ceiling="+ceiling+"  emin="+emin+"  emax="+emax);
						System.out.println("   from FITS header  min="+min+"  max="+max);
						System.out.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
					}
	 			 	// return temp.getAsBufferedImage();			  
	 			 	// return bi;    
	 			 	
					// add a flag to determine flip or not
	 			 	// 0 FLIP_VERTICAL
	 				// 1 FLIP_HORIZONTAL
					
					
					// flip the image, doesn't seem to be needed
					if (flip_image) {
					  ParameterBlock pb = new ParameterBlock();
					
					  javax.media.jai.operator.TransposeType 
					  type = javax.media.jai.operator.TransposeDescriptor.FLIP_VERTICAL;
					          // type = javax.media.jai.operator.TransposeDescriptor.FLIP_HORIZONTAL;
					  pb.addSource(bi).add(type);
					  System.out.println("Flip (transpose) the image");
	 				  PlanarImage flippedImage = (PlanarImage) JAI.create("transpose", pb);
	 				  // this how to create a byte image
	 			 	  // ImageUtils iu = new ImageUtils();
	    			  // RenderedImage ri = iu.processFormat(bi, DataBuffer.TYPE_BYTE) ;
					  bi = flippedImage.getAsBufferedImage();
					}
					
					return bi;
	 			}
	 			
	 		   }
	 		  }	
	 		
	        } else {
	        	if (debug) System.out.println("\n\nExtension "+i+":\n");
	        }
		i += 1;
		// h.info();
	    } // h != null
	  }
	  catch (FitsException fe) {
	  	System.out.println("FitsException: "+fe);
	  	fe.printStackTrace();
	  }
	// } while (h != null);
         
    return bi; // this will be null     
 }
     
     
     /**
     * convenience for using format/rescale operators
     * @param dataType
     * @return double
     */
    public double getMaxForDataType(int dataType) {
    	
    	double max = 0.0;
    	if (dataType == DataBuffer.TYPE_BYTE) {
    		max = Byte.MAX_VALUE * 2;//used as unsigned
	        }
	    else if (dataType == DataBuffer.TYPE_SHORT) {
	    	max = Short.MAX_VALUE;
	        }	    
	    else if (dataType == DataBuffer.TYPE_USHORT) {	    	
	    	max = Short.MAX_VALUE * 2;
	        }
	    else if (dataType == DataBuffer.TYPE_INT) {
	    	max = Integer.MAX_VALUE; // or 0.0 ?? // assume unsigned ???
	        }	    
	    else if (dataType == DataBuffer.TYPE_FLOAT) {
	    	max = Float.MAX_VALUE; 
	        }
	    else if (dataType == DataBuffer.TYPE_DOUBLE) {
	    	max = Double.MAX_VALUE; 
	        }
	    
	    return max;
	 } 
   
    /**
     * FITS images do not have thumbnails.
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
    
 /***********************************************************************
 * Creates a SampleModel that is most compatible with the input image.
 * Follow the See Also link for complete description
 * @see VicarIOBase#createSampleModel()
 */
    public SampleModel createSampleModel(int imageIndex)
    {
	// check if header has been read
	try {
		if (haveReadHeader) {
			getImageStuff();
		}
		else {
			readHeader();
		}
		
		width = getWidth(imageIndex);
		height = getWidth(imageIndex);
	} catch (IIOException iioe) {
		System.out.println("IIOException "+iioe);
		iioe.printStackTrace();
		
	}
/**
 * int width, height; 
	width = VICAR_TILE_WIDTH;
	if (!isRandomAccessEasy())	// non-random files use entire width
	    width = _system.getNS();
	if (width > _system.getNS())
	    width = _system.getNS();
	height = VICAR_TILE_HEIGHT;
	if (height > _system.getNL())
	    height = _system.getNL();
**/
	// assume zero is 0.0
	return createSampleModel(bitpix[imageIndex], naxis[imageIndex], 0.0);
    }




//----------------------------
   	/***********************************************************************
 * Creates a SampleModel that is most compatible with the input image.
 * Follow the See Also link for complete description
 * @see VicarIOBase#createSampleModel(int,int)
 * bitpix is bits per pixel
 * naxis gives the lines and samples
 * zero is the offset which must be added to the data to get true values
 */
    public SampleModel createSampleModel(int bitpix, int[] naxis, double zero)
    {
	int i;
	int pixel_stride;
	int scanline_stride;
	int num_bands = 1;
	boolean float_type;
	int tileWidth = 0;
	int tileHeight = 0;
	int data_buffer_type = DataBuffer.TYPE_BYTE;
	
	int ns = 0;
	int nl = 0;
	int nb = 0;
	int formatCode;
	int orgCode;
	int bytesPerPixel = Math.abs(bitpix/8);
	// int bitpix = 8;
	// int naxis = 1;
	// int naxis1,nazis2,naxis3,naxis4;
	
	
    int num_axis = naxis.length;
    
    switch (num_axis) {
    	
    	case 2:
    	num_bands = 1;
    	nl = naxis[0];
    	ns = naxis[1];
    	break;
    	case 3:
    	num_bands = naxis[0];
    	nl = naxis[1];
    	ns = naxis[2];
    	break;
    	case 4:
    	num_bands = naxis[1];
    	nl = naxis[2];
    	ns = naxis[3];
    	// what to do with [3] ????
    	break;
    	case 1:
    	// I can't image we'll ever get here
    	default:
    	return null;
   	
    }
      
	
	int calcSize = (nl*ns*num_bands*bytesPerPixel);
	if (debug) {
	  System.out.println("createSampleModel calcSize "+calcSize+" ns="+ns+" nl="+nl+" num_bands="+num_bands);
	  System.out.println("  bitpix="+bitpix+"  bytePerPixel="+bytesPerPixel+"  zero="+zero);
	}

	orgCode = SystemLabel.ORG_BSQ; // SystemLabel.ORG_BIP SystemLabel.ORG_BIL

	
	// fill in the values
	
	int[] band_offsets = new int[num_bands];
	int[] bank_indices = new int[num_bands];

	float_type = false;

	switch (bitpix) {
	    case 8:
		data_buffer_type = DataBuffer.TYPE_BYTE;
		break;
	    case 16:
	    	// all data is stored as signed,
	    	// if (zero == 32768.0) it is really unsigned
		data_buffer_type = DataBuffer.TYPE_SHORT;
		// data_buffer_type = DataBuffer.TYPE_USHORT;		
		break;
	    case 32:
		data_buffer_type = DataBuffer.TYPE_INT;
		break;
	    case -32:
		data_buffer_type = DataBuffer.TYPE_FLOAT;
		
		break;
	    case -64:
		data_buffer_type = DataBuffer.TYPE_DOUBLE;
		
		break;
	    
	}

	
	if (debug) System.out.println(" data_buffer_type = "+data_buffer_type); 

	// Compute offsets, etc... then create the SM
	tileWidth = ns;
	tileHeight = nl;
	
	
	switch (orgCode) {
	    case SystemLabel.ORG_BSQ:
		// One bank per band of data
		pixel_stride = 1;
		scanline_stride = tileWidth;
		for (i=0; i < num_bands; i++) {
		    band_offsets[i] = 0;
		    bank_indices[i] = i;
		}
		
		
		/**
		if (num_bands == 1)
		    return new PixelInterleavedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				band_offsets);
		else
		**/
		
		return new BandedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				scanline_stride,
				bank_indices, band_offsets);

	    case SystemLabel.ORG_BIL:
		// One bank for all bands of data
		pixel_stride = 1;
		scanline_stride = tileWidth * num_bands;
		for (i=0; i < num_bands; i++) {
		    band_offsets[i] = tileWidth * i;
		    bank_indices[i] = 0;
		}
		

		return new ComponentSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				bank_indices, band_offsets);

	    case SystemLabel.ORG_BIP:
		// One bank for all bands of data
		pixel_stride = num_bands;
		scanline_stride = tileWidth * num_bands;
		for (i=0; i < num_bands; i++) {
		    band_offsets[i] = i;
		    bank_indices[i] = 0;
		}
		
				

		return new PixelInterleavedSampleModel(data_buffer_type,
				tileWidth, tileHeight,
				pixel_stride, scanline_stride,
				band_offsets);
	}

	return null;		// whoops!!  Shouldn't happen!
    }
    
    
    public void printHduInfo() {
    	for (int i=0 ; i< hdus.length ; i++) {    	
    		BasicHDU hdu = (BasicHDU) hdus[i];
    		System.out.println("hdus["+i+"] "+hdu.getClass().getName());
    		hdu.info(System.out);
    	}
    }

}




