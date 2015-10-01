/*
 * @(#)PDSImageReader.java  1.0 00/08/30
 *
 * Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 12-2000 ImageIO EA2 version
 * 12-1-03 new version which chooses between Native and regular 
 * PDSInputFile
 * 3-1024 - srl
 * added pdsMetadata.setFront_label_size() for PDS4 label creation
 */

package jpl.mipl.io.plugins;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.BufferedReader;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import javax.imageio.IIOException;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageReader;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.FileImageInputStream;
import javax.imageio.stream.ImageInputStream;
import javax.media.jai.PlanarImage;

import jpl.mipl.io.streams.DataInputStreamWrapper;
import jpl.mipl.io.vicar.PDSInputFile;
import jpl.mipl.io.vicar.PDSNativeInputFile;
import jpl.mipl.io.vicar.SystemLabel;

import org.w3c.dom.Document;

import com.sun.imageio.plugins.common.BogusColorSpace;
import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.SeekableStream;


    
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
 * for reading image files in the PDS/Vicar format.
 *
 * @version 0.6
 */
public class PDSImageReader extends ImageReader {

   private boolean debug = false;
   //private boolean debug = true;
   
    private boolean imageTypeRetryFailed = false;
    // private boolean debug = true;
    /**
     *
     * VicarIO specific variables
     *
     * used to get file info 
     */
     
    // private VicarInputFile vif; // this may become PDSInputFile or RawInputFile
    private PDSInputFile pif;
    // 20110709, xing
    private PDSImageReadParam pdsImageReadParam;
    private SystemLabel sys; // we need a system label object for all the reader routines
    
    String filename = null; // can used by the native OAL reader code
    
    private SeekableStream seekableStream;
    // vicarIO currently uses SeekableStream
    // may transition to ImageInputStream ?????

    // private FileImageInputStream fileStream; 
    private ImageInputStream stream; 
    private DataInputStreamWrapper inputStreamWrapper;
    DataInputStream pixelStream = null;
    BufferedReader bufferedReader = null;
    
    FileImageInputStream fileStream = null;

    BufferedImage theImage = null;
    
    private boolean haveReadHeader = false;
    
    boolean gotHeader = false;
    boolean gotMetadata = false;

    PDSMetadata pdsMetadata = new PDSMetadata();
    ImageReadParam lastParam = null;
    
    Document document = null ;
    
    // if the file contains NO image set this to true
    // we create a 1x1 BufferedImage so we don't pass nulls around everywhere
    boolean fakeImageNoRead = false;

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
    public PDSImageReader (ImageReaderSpi mySpi) {
        super(mySpi);
        if (debug)   {
            System.out.println("**********************");  
            System.out.println("PDSImageReader constructor");
        }
    }

    public boolean getFakeImageNoRead() {
        return fakeImageNoRead ;        
    }

    public void setInput(Object input, boolean isStreamable, boolean ignoreMetadata) {
        
        super.setInput(input, isStreamable, ignoreMetadata);
        setInputInternal(input);
        // this.setInput(input, isStreamable);
    }
    
    public void setInput(Object input, boolean isStreamable) {
        if (debug)     
                  System.out.println("PDSImageReader public void setInput(Object input, boolean isStreamable)");
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
        // super.setInput(input, isStreamable);
        /** we will need to see if the VicarIO needs to be changed to use 
         * ImageInputStream
         */
        if (debug) {
            System.out.println("PDSImageReader.setInputInternal "+input);
        }
        
        
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
        } 
        else if (input instanceof String) {
            if (debug)     
                System.out.println("input is instanceof String ++++++++++++++");
            FileImageInputStream fileStream ;
            try {
            
            fileStream = new FileImageInputStream(new File((String)input));
            filename = (String)input; // this filename will be used by the native OAL libraries
                        // do I need to close this stream ???
            }
            catch (FileNotFoundException fnfe) {
                System.out.println("FileNotFoundException "+fnfe);
                fileStream = null;
                filename = null;
            }
            catch (IOException ioe) {
                System.out.println("IOException "+ioe);
                fileStream = null;
                filename = null;
            }
            stream = fileStream;
        }
        else {
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
            
        if (debug) System.out.println("PdsImageReader.readAsRenderedImage()");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }
        // 20110709, xing
        this.pdsImageReadParam = (PDSImageReadParam)param;

        if (haveReadHeader == false) {
            readHeader();
        }

        if (debug) 
            System.out.println("PdsImageReader.readAsRenderedImage() after readHeader() ");
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
            image = new VicarRenderedImage(pif, param);
        }
        catch (Exception e) {
            System.err.println("readAsRenderedImage ERROR: "+e);
        }
        
        if (debug) { 
            System.out.println(" vif "+pif);
            System.out.println(" image "+image);
        }
        
         
       

        if (debug) 
            System.out.println("PdsImageReader.readAsRenderedImage() after readHeader() ");
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
        
        this.haveReadHeader = true;

        
        if (debug) {   
            System.out.println("PDSImageReader.readHeader");  
            System.out.println("input="+input);    
            System.out.println("filename="+filename);
            System.out.println("stream="+stream);
            System.out.println("stream " + stream.getClass().getName()+" ******************* ");
            
        }
        
        if (stream == null && seekableStream == null && 
            inputStreamWrapper == null && filename == null) {
            throw new IllegalStateException ("Input stream not set");
        }
        
        
        
        try
        {
            // for now use seekableStream
            // vif = new VicarInputFile();
            
            /* check if PDSNativeInputFile
             * check if Native OAL library can be loaded
             * Then proceed 
             * should be a flag passed in to disable or force attempt to try Native
             * 
             * 
             */
          
            if (filename != null) {
                PDSNativeInputFile pnif = new PDSNativeInputFile();
                if (debug) {    
                    System.out.println("try Native ******************* ");
                }
                if ( pnif.loadOalLib() ) { // returns true if the Native library can be found
                    if (debug) {    
                        System.out.println("PDSImageReader pnif.loadOalLib() returned TRUE");
                        }
                    pif = pnif;
                    // native overloaded methods SHOULD be called
                    } 
                
                    // vif.open( stream); // vif should be able to deal with ImageInputStream
                try {
                
                    pif.open( filename );
                }
                catch (java.lang.UnsatisfiedLinkError ule) {
                    if (debug) {    
                        System.out.println("java.lang.UnsatisfiedLinkError "+ule);
                        System.out.println("using pure java ");
                    }
                    // create a stream from the file and read with pure java
                                        // 20110709, xing
                                        //pif = new PDSInputFile();
                                        pif = new PDSInputFile(this.pdsImageReadParam);
                    FileInputStream fis = new FileInputStream(new File(filename));
                    if (debug) System.out.println("using pure java fis "+fis);
                    pif.open( fis );
                    
                }
            }
            else if (stream != null) {
                if (debug) {     
                  System.out.println("stream " + stream.getClass().getName()+" *@#$%^&* ");
                }
                                // 20110709, xing
                                //pif = new PDSInputFile();
                                pif = new PDSInputFile(this.pdsImageReadParam);
                pif.open( stream);
            }
            else if (inputStreamWrapper != null) {
                if (debug)  {                   
                  System.out.println("inputStreamWrapper " + inputStreamWrapper.getClass().getName());
                }
                
                // vif.open(inputStreamWrapper);
                                // 20110709, xing
                                //pif = new PDSInputFile();
                                pif = new PDSInputFile(this.pdsImageReadParam);
                pif.open(inputStreamWrapper);
                // public void open(InputStream is) throws IOException
                // public synchronized void open(InputStream is, boolean sequential_only) throws IOException
            }
            else if (seekableStream != null) {
                if (debug) { 
                  System.out.println("seekableStream " + seekableStream.getClass().getName());
                }
                // vif.open(seekableStream);
                                // 20110709, xing
                                //pif = new PDSInputFile();
                                pif = new PDSInputFile(this.pdsImageReadParam);
                pif.open(seekableStream);
            }
           
           
            if (debug)     {
                System.out.println( "****************************************");
                System.out.println( "*");           
                System.out.println( "*  PDSImageReader pif.getSystemLabel()");
            }
                
           sys = pif.getSystemLabel();
           
           if (debug)     {
                System.out.println( "*");
                System.out.println( "****************************************");
           }
           
           String format = sys.getFormat();
           String org = sys.getOrg(); 
           int nb = sys.getNB();
           
           
           
           //20120606, nttoole (from VicarImageReader)
           if (format.equals("HALF") && org.equals("BSQ") && nb == 1) 
           {
               int biType = BufferedImage.TYPE_USHORT_GRAY;
               imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
               if (debug) System.out.println("VicarImageReader.readHeader() imageType "+imageType);
           } 
           
           else if (format.equals("USHORT") && org.equals("BSQ") && nb == 1) 
           {
                int biType = BufferedImage.TYPE_USHORT_GRAY;
                imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
                if (debug) System.out.println("PDSImageReader.readHeader() imageType "+imageType);
            } 
           else if (format.equals("BYTE") && org.equals("BSQ") && nb == 1) 
           {
                int biType = BufferedImage.TYPE_BYTE_GRAY;
                imageType = ImageTypeSpecifier.createFromBufferedImageType(biType) ;
                if (debug) System.out.println("PDSImageReader.readHeader() imageType "+imageType);
           
           } 
           else 
           {
               
                // this handles "HALF" 
                SampleModel sampleModel = pif.createSampleModel(width, height);
                
                //ask Planer image to create a color model
                ColorModel colorModel = PlanarImage.createColorModel(sampleModel);
                
//                //20121031, nttoole
//                if (colorModel == null)
//                {
//                    //ask image code to try and create a color model
//                    colorModel = ImageCodec.createComponentColorModel(sampleModel);                    
//                }
//                //--------------------
                 

//                //20121103, nttoole
//                if (colorModel == null)
//                {                
//                    if (sampleModel instanceof BandedSampleModel)
//                    {
//                        BandedSampleModel bsm = (BandedSampleModel) sampleModel;                    
//                        ColorSpace colorSpace = new BogusColorSpace(bsm.getNumBands());                        
//                        try {
//                            imageType = ImageTypeSpecifier.createBanded(colorSpace,
//                                                bsm.getBankIndices(),
//                                                bsm.getBandOffsets(), bsm.getDataType(),
//                                                false, false);
//                            colorModel = imageType.getColorModel();
//                        } catch (IllegalArgumentException iaEx) {
//                            iaEx.printStackTrace();
//                            imageType = null;
//                        }                                       
//                    }
//                }
                
                // 20121104, nttoole
                if (colorModel == null)
                {
                    colorModel = getDummyColorModel(sampleModel.getNumBands(), 
                                                    sampleModel.getDataType());
                }
                //--------------------
                 
                
                if (debug) {
                   System.out.println("after pif.createSampleModel() ");
                   System.out.println("sampleModel "+sampleModel);
                   System.out.println("colorModel "+colorModel);
                }
            
                if (imageType == null)
                {
                 
                    try {
                        //if (imageType == null && imageTypeRetryFailed == false) { 
                        imageType = new ImageTypeSpecifier(colorModel, sampleModel) ;
                        //}
                    } catch (IllegalArgumentException iae) {
                        if (debug) {
                          System.out.println("PDSImageReader.readHeader() ImageTypeSpecifier ");
                          System.out.println("IllegalArgumentException "+iae);
                          // 20121105, nttoole
                          //iae.printStackTrace();
                          // ------------------
                        }
                    
                        imageType = null;
                    }
                }
            
            
            /**
            // RenderedImage ri
            if (imageType == null && imageTypeRetryFailed == false) 
            { 
                imageTypeRetryFailed = true;
                if (debug) System.out.println("imageType == null try using ImageTypeSpecifier from RenderedImage");
                RenderedImage ri = readAsRenderedImage(0, null);
                try {
                    imageType = new ImageTypeSpecifier(ri) ;
                    }
                catch (IllegalArgumentException iae) {
                    System.out.println("PDSImageReader.readHeader() ImageTypeSpecifier from RenderedImage");
                    System.out.println("IllegalArgumentException "+iae);            
                    imageType = null;   
                } 
                if (imageType == null) {
                    System.out.println("****************************************************");
                    System.out.println("*");
                    System.out.println("* imageType == null RETRY failed ");
                    System.out.println("*");
                    System.out.println("****************************************************");
                    imageTypeRetryFailed = true;       
                }
             }
             **/
            
           }
           
           if (debug)  {  
        
            System.out.println("PDSImageReader.readHeader() after pif.open() !@#$%^&*()+");
            // System.out.println(pif.getVicarLabel().toString());

            System.out.println("System label:"+sys);
            // System.out.println(sys.toString());
            // readHeader(input);
            System.out.println("--------------- PDSFile opened OK");
            
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
        
        
       document = pif.getPDSDocument(); // should use accessor instead
       int lblsize_front = pif.getLblsize_front();
       if (document != null) {
            if (debug) {
                System.out.println("PDSImageReader new PDSMetadata with document");
                System.out.println("++ lblsize_front "+lblsize_front+"  +++");
            }
            pdsMetadata = new PDSMetadata(document);
            pdsMetadata.setFront_label_size(lblsize_front);
            gotMetadata = true;
            // return ;
       } else {
            if (debug) {
                System.out.println("no document avaiable from pif");
                System.out.println("PDSImageReader NO PDSMetadata ************");
            }
            return;
       }
       
       if (debug)  System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
        
        
       // PDSLabelToDOM(BufferedReader input, PrintWriter output);
       // creaste a BufferedReader from whatever input type we have
       
       /***
       BufferedReader input = null;
       if (stream instanceof ImageInputStream) {
            PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((ImageInputStream) stream, null);
            document = pdsLabel2Dom.getDocument();
            System.out.println("PDSImageReader new PDSMetadata with document");
            System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++");
            pdsMetadata = new PDSMetadata(document);
            gotMetadata = true;
            // return ;
       } else {
            System.out.println("Improper input type, can't read the header "+input);
            System.out.println("PDSImageReader NO PDSMetadata ************");
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
        if (debug) System.out.println("PDSImageReader.readMetadata");
        if (gotMetadata) {
            return;
        }
        
        if (haveReadHeader == false) {
            readHeader();
        }
    }
    
    
    
    
    public String getFormatName()  throws IIOException {
        if (debug) System.out.println("PDSImageReader.getFormatName");
        return "pds";
    }

    public int getNumImages() throws IIOException {
        if (debug) System.out.println("PDSImageReader.getNumImages");
        return 1; // Vicar always have just 1 ???
        // at least that's all we support now
    }
    
    public int getWidth(int imageIndex) throws IIOException {
        if (debug) System.out.println("PDSImageReader.getWidth("+imageIndex+")");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        // return width;
        if (debug) System.out.println("width="+sys.getNS() );
        return sys.getNS();
    }
    
    public int getHeight(int imageIndex) throws IIOException {
        if (debug) System.out.println("PDSImageReader.getHeight("+imageIndex+")");
       
        
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
        }
        //  return height;
        if (debug) System.out.println("height="+sys.getNL() );
        return sys.getNL();
    }

    public int getTileWidth(int imageIndex) throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false)
            readHeader();
        SampleModel sm = pif.createSampleModel();
        return sm.getWidth();
    }

    public int getTileHeight(int imageIndex) throws IIOException {
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false)
            readHeader();
        SampleModel sm = pif.createSampleModel();
        return sm.getHeight();
    }

    // I think these are not useful
    public ImageTypeSpecifier getRawImageType(int imageIndex)   
        throws IIOException {
        if (debug) System.out.println("PDSImageReader.getRawImageType");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) {
            readHeader();
            // imageType is set in ReadHeader
        }       
        
        // figure out what this means in the vicar context
        return imageType;
    }
    
    public Iterator getImageTypes(int imageIndex) throws IIOException 
    {
        if (debug) 
            System.out.println("PDSImageReader.getImageTypes");
        
        if (imageIndex != 0) 
        {
            throw new IndexOutOfBoundsException();
        }
        if (haveReadHeader == false) 
        {
            readHeader();
        }
        
        ArrayList list = new ArrayList();
        if (imageType == null)
        {
            
        }
        list.add(imageType);
        return list.iterator();
    }


    public int getNumImages(boolean allowSearch) throws IIOException {
        if (debug) System.out.println("PDSImageReader.getNumImages");
        if (stream == null) {
            throw new IllegalStateException("No input source set!");
        }
        return 1;
    }
    
    /**
     * Uses the default implementation of ImageReadParam.
     */
    public ImageReadParam getDefaultReadParam() {
        if (debug) System.out.println("PDSImageReader.getDefaultReadParam");
        // 20110709, xing
        //return new ImageReadParam();
        return new PDSImageReadParam();
    }

    /**
     * Since there is only ever 1 image, there is no clear distinction
     * between image metadata and stream metadata, so just use image
     * metadata and always return null for stream metadata.
     */
    public IIOMetadata getStreamMetadata() throws IIOException {
        if (debug) System.out.println("PDSImageReader.getStreamMetedata");
        return null;
    }
    
    public IIOMetadata getImageMetadata(int imageIndex) throws IIOException {
        if (debug) System.out.println("PDSImageReader.getImageMetadata("+imageIndex+")");
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException("imageIndex != 0!");
        }
        readMetadata(); // make sure vicarMetadata has valid data in it
        return pdsMetadata;
    }

    public void printParam(ImageReadParam param) {
        System.out.println("------------------------------------------");
        
        System.out.println("PDSImageReader ImageReadParam:");
        
        
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
            
        if (debug) {
            System.out.println("*** read read read read read *******");
            System.out.println("************************************");
            System.out.println("PDSImageReader.read("+imageIndex+")");
            } 
        if (imageIndex != 0) {
            throw new IndexOutOfBoundsException ();
        }
        // 20110709, xing
        if (param instanceof PDSImageReadParam)
            this.pdsImageReadParam = (PDSImageReadParam)param;

        if (haveReadHeader == false) {
            readHeader();
        }

        if (debug) printParam(param);
        boolean parameterizedRead = false;

        if (debug) System.out.println("PDSImageReader.read() after readHeader() ");
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
         
         
         if (debug) {
             System.out.println("image is "+width+"x"+height);
             System.out.println("PDSImageReader.read() imageType "+imageType);
         }
         
         if (width == 0 && height == 0) {
             width = 1;
             height = 1;
             fakeImageNoRead = true;
             // we need valid image to continue
             // this image should never be used
             // this is to handle the case where a PDS/ODL file contains NO image data
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
                // origin = new Point(sourceRegion.x,sourceRegion.y);
                startX = sourceRegion.x;
                startY = sourceRegion.y;
                parameterizedRead = true;
                }
            }
         
         if (parameterizedRead == false && imageType != null) 
         {
            if (debug)  {
                System.out.println("imageType.createBufferedImage ");
                System.out.println("imageType "+imageType);
                } 
            // let imageTypeSpecifier create the buffered iamge for us
            theImage = imageType.createBufferedImage(width, height);
            sampleModel = theImage.getSampleModel();
            colorModel = theImage.getColorModel();
         }
         else 
         {
            
     
         // get the SampleModel ColorModel Raster and data buffer from vicarIO
         if (debug) {
            System.out.println("---------------------------------------------");
            System.out.println("pif.createSampleModel("+width+","+height+")");
            } 
         
         sampleModel = pif.createSampleModel(width, height);
         
         if (debug) {
            System.out.println("after pif.createSampleModel()");
            System.out.println("sampleModel "+sampleModel);
         }
         
         
        // public BufferedImage createBufferedImage(int width, int height) {
        WritableRaster raster = Raster.createWritableRaster(sampleModel, new Point(0, 0));
        // WritableRaster raster = Raster.createWritableRaster(sampleModel, origin);
        
        
        // get the number of bands
        int bands = sampleModel.getNumBands();
        int dataType = sampleModel.getDataType();
        
        if (bands <= 3) 
        {
       
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
                // colorModel = ImageCodec.createComponentColorModel(sampleModel);
                
                // this is a dependancy on JAI - do this by han instead of using a Convenience method
                // go find PlanarImage source ??
                colorModel = PlanarImage.createColorModel(sampleModel);
            }
            
            
            if (debug) {
                System.out.println("bands="+bands+"  colorModel "+colorModel);
                // can't create a colorModel for something with a weird
            }
            
            if (colorModel == null) {
                
                System.out.println("ERROR  PDSImageReader.read() colorModel is null. Can't create BufferedImage");
                
            }
            else 
            {
                theImage = new java.awt.image.BufferedImage(colorModel, raster,
                                                            colorModel.isAlphaPremultiplied(),
                                                            new Hashtable());
            }
         }
         else 
         { 
             // bands > 3 can't create a color model
             // get a BufferedImage so we can still return something
             if (debug) {
                System.out.println("bands="+bands+"  colorModel "+colorModel);
             }
            
//             // create a child raster using only 3 bands
//            int parentX = raster.getMinX();
//            int parentY = raster.getMinY();
//            int w = raster.getWidth();
//            int h = raster.getHeight();
//            int childMinX = parentX;
//            int childMinY = parentY;
//            int[] bandList = {0,1,2};
            
            // the child should SHARE the parent's raster
            // we will read data into the parent raster
            // the BuffereImgae will be from the child so a ColorModel can be created
//            WritableRaster childRaster = raster.createWritableChild(parentX, parentY, w, h,
//                 childMinX, childMinY, bandList);
            
            // is the sampleModel valid ??
            // SampleModel childSM = new SampleModel();
            
            if (debug) System.out.println("sampleModel "+sampleModel);
            

            //try asking PlanarImage to suggest a color model that works
            colorModel = PlanarImage.createColorModel(sampleModel);
            
            //try asking Image codec for a color model
            colorModel = ImageCodec.createComponentColorModel(sampleModel);
            
            if (debug) System.out.println("colorModel "+colorModel);
            
            //at this point, create a color model using the BogusColorSpace
            if (colorModel == null) 
            {
                dataType = sampleModel.getDataType();
                
//                // 20121105 nttoole, commented out block
//                w = sampleModel.getWidth();
//                h = sampleModel.getHeight();
//                int b = 3;
            
//                BandedSampleModel fakeSampleModel = new BandedSampleModel(dataType, w, h, b);
//               // System.out.println("childSM "+ childSM);
            
//                // create a bogus colorModel just to get by
//                // colorModel = ImageCodec.createComponentColorModel(fakeSampleModel);
//                //colorModel = PlanarImage.createColorModel(fakeSampleModel);
//                if (debug) System.out.println("colorModel (fake) "+colorModel);
//                //-------------------------
                
                // 20121105, nttoole
                if (colorModel == null)
                {
                    colorModel = getDummyColorModel(sampleModel.getNumBands(), 
                                                    sampleModel.getDataType());                    
                    if (debug) System.out.println("colorModel (boguscolorspace) "+colorModel);
                }
                //-------------------------
            }
            
//            // *** commented out by nttoole on 11.05.2012
//            //childRaster 
//            // childRaster
//            theImage = new java.awt.image.BufferedImage(colorModel, childRaster,
//                                 colorModel.isAlphaPremultiplied(), new Hashtable());
//            // ************************************
            
            
            //************** added by nttoole 11.05.2012
            theImage = new java.awt.image.BufferedImage(colorModel, raster,
                                                        colorModel.isAlphaPremultiplied(), 
                                                        new Hashtable());
            //**************************************************
                                
            if (debug) {
                System.out.println("theImage "+theImage);
            }
            
            
         }
 
        }
        
        DataBuffer buf = theImage.getRaster().getDataBuffer();
        
        // this tile happens to be the whole image 
        if (debug) {
            System.out.println("PDSImageReader.read() >>> pif.readTile >>>>>>>");
            System.out.println(" fakeImageNoRead "+fakeImageNoRead);
        }
        if (fakeImageNoRead == false ) 
        {
            try {
                if ( startX != 0 || startY != 0 || width < imageWidth || height < imageHeight) 
                {
                    // read a subarea of the image
                    if (debug) System.out.println("readTile "+startX+","+ startY+" "+ width+"x"+ height+
                            " from "+imageWidth+"x"+imageHeight);
                    pif.readTile(startX, startY, width, height, x_off, y_off, sampleModel, buf);
                }
                else  // standard read of the entire tile
                {
                    if (debug) System.out.println("readTile BASIC");
                    pif.readTile(0,0, sampleModel, buf);
                 }
            
            } 
        
            catch (IOException e) {
                e.printStackTrace();
                throw new RuntimeException("IOException occured while reading PDS image file");
            } 
        }
        
        if (debug) {
            System.out.println("PDSImageReader.read() completed");
            System.out.println("sampleModel="+sampleModel);
            System.out.println("colorModel="+colorModel);
        }
        return theImage;
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
     * PDS images do not have thumbnails.
     */
    public int getNumThumbnails(int imageIndex) 
    {
        if (debug) System.out.println("PDSImageReader.getNumThumbnails");
        return 0;
    }

    public BufferedImage readThumbnail(int imageIndex,
                                       int thumbnailIndex)
        throws IIOException {
            if (debug) System.out.println("PDSImageReader.readThumbnail");
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

