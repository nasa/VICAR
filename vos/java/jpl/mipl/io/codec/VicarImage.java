/**
 * @(#)VicarImage.java	
 * @version 1.1 11-12-2001
 *
 * @author Steve Levoe NASA/JPL
 
 * This class was formerly included in the same file as 
 * VicarImageDecoder
 
 * changes:
 * 10-4-01 - Steve Levoe
 * to allow easier subclassing with different VicarInputs
 * most of the work in the constructor is moved to an init()
 * which will be called with a subclass of VicarInputFile for 
 * different codecs. The PDS codec will subclass VicarInputFile.
 * <BR>
 * The VicarLabel Object will be placed into a property 
 * "vicar_label".
 * <BR>
 * 11-12-01 - Steve Levoe
 * make public so PDSImage has access
 *  public VicarInputFile vif;
 */


package jpl.mipl.io.codec;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.ComponentColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;

import javax.media.jai.DataBufferFloat;
import javax.media.jai.ComponentSampleModelJAI;


import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;

import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.IOException;

import com.sun.media.jai.codec.ImageCodec;
import com.sun.media.jai.codec.ImageDecoder;
import com.sun.media.jai.codec.ImageDecoderImpl;
import com.sun.media.jai.codec.ImageDecodeParam;
import com.sun.media.jai.codec.SeekableStream;

import jpl.mipl.io.vicar.*;

import jpl.mipl.io.streams.*;



/**
* VicarImage holds the vicar image headers and data
*
*/
public class VicarImage extends SimpleRenderedImage {

    private SeekableStream input;

    private byte[] lineSeparator;

    
    // vicar label derived items
    private String org;    // BSQ BIL BIP
    private String format; // BYTE HALF FULL REAL COMP
    private String host; 
    
    // private String labelString;
    
    // private boolean swap = false; // based on value of host
    
    private int nl;
    private int ns;
    private int nb;
    private int lblsize;


    private int numBands;   // vicar NB 1 or 3

    private int dataType; // BYTE HALF FULL
    
    // private int pixelStride; 
    // private int scanlineStride ;
    // private int[] bandOffsets;
    
    // public boolean debug = true;
	public boolean debug = false;
    
    // 11-12-01 make public so PDSImage has access
    public VicarInputFile vif;
    protected jpl.mipl.io.vicar.SystemLabel sys;

    /**
     * Construct a VicarImage.
     *
     * @param input The SeekableStream for the Vicar file.
     * uses VicarIO classes to read the label
     * Values needed by the codec are extracted from the Vicar system label
     */
    public VicarImage(SeekableStream input) throws Exception {

        init(input, new VicarInputFile()); 
    }
    
    public VicarImage(SeekableStream input, VicarInputFile vicarInputFile) throws Exception {

        init(input, vicarInputFile); 
    }
     
      
       // void init(VicarInputFile vicarInputFile) {
     void init(SeekableStream input, VicarInputFile vicarInputFile) {  
        
        if (debug) {
        	System.out.println("VicarImage.init() ");
        	} 
        
        vif = vicarInputFile;
        // Read file header.
        this.input = input;
        try
        {   
            // vif = new VicarInputFile();
            // System.out.println("input="+input);
	        vif.open(input); // this causes the file to be read and data structures to be filled
	        // System.out.println(vif.getVicarLabel().toString());

	        sys = vif.getSystemLabel();
	        // System.out.println("System label:");
	        // System.out.println(sys.toString());
            // readHeader(input);
            // input.close(); // we keep the stream around so we can read in the data
        }
        catch (IOException ex)
        {
            System.out.println("Error reading header:"+ex.getMessage());
            return ;
        }
        catch (Exception ex)
        {
            System.out.println("Error reading header:"+ex.getMessage());
            return ;
        }
        // System.out.println("--------------- VicarFile opened OK");
        
        // System.out.println("vif.getVicarLabel().getLblsize()");
        // lblsize = vif.getVicarLabel().getLblsize();
	    // tileWidth, tileHeight
	    // use the system label object to get all values needed by the codec to read the file
	    // System.out.println("pull things out of the system label");
	    format = sys.getFormat(); 
	    org = sys.getOrg(); // string
	    ns = sys.getNS();
	    nl = sys.getNL();
	    nb = sys.getNB();
        
        // System.out.println("*** done pulling things out of the system label");
        // System.out.println("input="+input);
        width = ns;
        height = nl;
        
            // sample model used to store pixel values
            // The VicarInputFile has read in the header and knows how to construct an appropriate 
            // SampleModel for this image file
            sampleModel = vif.createSampleModel();
            if (sampleModel == null) {
                System.out.println("ERROR Conmstructor VicarImageCodec vif.createSampleModel( sampleModel is null "   );
                // throw an exception ... any defined for codecs???
                // might try an alternate methode of constructing the sample model if vif can't do it
                return;
            }
            tileWidth = sampleModel.getWidth(); // assume this is the tile width
            tileHeight = sampleModel.getHeight(); // assume this is the tile width
        
            // System.out.println("tileWidth="+tileWidth);
            // System.out.println("tileHeight="+tileHeight);
            colorModel = ImageCodec.createComponentColorModel(sampleModel);
            //if (colorModel == null) {
            //    System.out.println("ERROR Constructor VicarImageCodec ImageCodec.createComponentColorModel colorModel is null ");
            //    System.out.println("sampleModel.getDataType() "+sampleModel.getDataType()  );
            //    return;
            //}
          /***/
        // System.out.println("END OF VicarImage input="+input);
        // properties.put("type","vicar");
        VicarLabel label = null;
        try { 
            label = vif.getVicarLabel();
        }
        catch (IOException ex)
        {
            System.out.println("Error reading header:"+ex.getMessage());
            return ;
        }
        // System.out.println("vicar label "+label);
        
        // putting the label into the images properties allows us to gain access to the
        // image label thru the properies
        if (label != null) {
            properties.put("vicar_label", (Object) label);
        }
        // properties.put("system_label", (Object) sys);
        // properties.put("tile_width", new Integer(tileWidth));
        // properties.put("tile_height", new Integer(tileHeight));
        /****/
        // add a mplib property ??? then it is separate from the vicar IO
    }

    /* 
     * Allow user to change tile size.<br>
     * Intended for developer testing use to see effect of tile size. 
     ****/
    public VicarImage(SeekableStream input, int tileSize) throws Exception {
        // could chnage to a version which params for width and height
        this(input);
        this.tileHeight = tileSize;
        this.tileWidth = tileSize;
        
    }
    
    public String getLabelString() {
        return "Vicar Label" ; // vif.toString();
    }
    
    /**
    * Prints out information about the image.
    */
    public void info()
    {
        // get the info out of the vif.
        
        
        System.out.println("Vicar image decoder:");
        // System.out.println(vif.toString() );
        System.out.println("  lblsize=" + lblsize);
        System.out.println("  ns=" + ns);
        System.out.println("  nl=" + nl);
        System.out.println("  nb=" + nb);
        System.out.println("  tileWidth=" + tileWidth);
        System.out.println("  tileHeight=" + tileHeight);
        System.out.println("  format=" + format);
        System.out.println("  org=" + org);
        System.out.println("  host=" + host);      
        // System.out.println("  pixelStride=" + pixelStride);
        // System.out.println("  scanlineStride=" + scanlineStride );
        System.out.println("  input="+input);
    }

    
    
    
    /**
    * computeTile calculates the rectangle in the image space which 
    * holds the data for the requested tile. 
    * ComputeTile then gets the data from the file and returns the Raster 
    * containing the data.
    *
    */
    private Raster computeTile(int tileX, int tileY) {
        WritableRaster theTile;
        theTile = null;
        //if (theTile != null) {
            // return theTile;
        // }

        Point org;
        
        // set the origin of this tile into the whole image
        // this tells paint() where to draw this tile into the window
        org = new Point(tileXToX(tileX), tileYToY(tileY));
        
        
        boolean isOddTile = true;
        if (tileX %2 != 1 && tileY %2 != 1) isOddTile = false;
        else if (tileX %2 == 1 && tileY %2 == 1) isOddTile = false;
        else isOddTile = true;
        
        // int tileScanlineStride = tileWidth; // get something better from RTL ??
        // int tileScanlineStride = scanlineStride;
        // create a sampleModel for this tile
        
        int xleft = 0;
        int yleft = 0;
        int tWidth = 0;
        int tHeight = 0;
        
        // calculate the x,y of the tile corner
        // this positions the tile when it is drawn to the window
        int tx = tileXToX(tileX);
        int ty = tileYToY(tileY);
        
        /** this is done inside getSampleModel
        * calculates tile size at the edges **/
        xleft = width - tx;
        yleft = height - ty;
        
        
//        if (xleft < tileWidth) 
//            tWidth = xleft;
//        else 
            tWidth = tileWidth;
        
//        if (yleft < tileHeight) 
//            tHeight = yleft;
//        else 
            tHeight = tileHeight;
        
        
        SampleModel tileSampleModel = vif.createSampleModel(tWidth, tHeight);
        
        if (tileSampleModel instanceof ComponentSampleModelJAI) {
            // this allows us to directly display float/double images
            // System.out.println("trying javax.media.jai.PlanarImage.createColorModel ");
             colorModel = javax.media.jai.PlanarImage.createColorModel(tileSampleModel);
          }
          else {
             colorModel = ImageCodec.createComponentColorModel(tileSampleModel);
          }
          
        theTile = Raster.createWritableRaster(tileSampleModel, org);
        Rectangle tileRect = theTile.getBounds();
      
        DataBuffer dbuf = theTile.getDataBuffer(); // I don't care what sub-type the data buffer is
        // the vicar IO library will go read the file to get the data
         try {
            vif.readTile(tx, ty, tileSampleModel, dbuf); // fill the data buffer
         } catch (IOException e) {
              e.printStackTrace();
              throw new RuntimeException( "IOException occured while processing Vicar file.");
         }
        

        return theTile;
    }




    /**
    * getTile is called by any program using this decoder when it wants the data for 
    * a tile in the image. 
    * If the image is not tiled, then tile 0,0 returns the entire image.
    */
    public synchronized Raster getTile(int tileX, int tileY) {
        if (debug) System.out.println("VicarImage.getTile "+tileX+","+tileY);
        return computeTile(tileX, tileY);
    }
    
    

} 
