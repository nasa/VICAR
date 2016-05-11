/*
 * VicarRenderedImage
 * 
 * * @author Steve Levoe
 * 
 *  */ 

package jpl.mipl.io.plugins;

import java.awt.*;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.DataBufferUShort;

import javax.media.jai.DataBufferFloat;
import javax.media.jai.ComponentSampleModelJAI;

import java.awt.image.*;

import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.IOException;

import javax.imageio.ImageReadParam;

import jpl.mipl.io.vicar.*;

import jpl.mipl.io.codec.*;

import jpl.mipl.io.streams.*;

import java.awt.color.ColorSpace;
import javax.media.jai.PlanarImage;

/**
 * This class is used to allow a RenderedImage to be returned by ImageReader.getAsRenderedImage().
 * 
 * The RenderedImage has no ColorModel associated with it. This is not a displayable image.
 * The user must create a BufferedImage with it
 */
public class VicarRenderedImage extends  SimpleRenderedImage {
	
	public VicarInputFile vif;
	// these are in the super class, we don't want to hide them
	// SampleModel sampleModel = null;
	// ColorModel colorModel = null;
	SystemLabel sys = null;
	
	// there is a debug in SimpleRenderedImage
	boolean debug = false;
	// boolean debug = true;
	
	// for fast copies a tileWidth should be the record length
	// for an image display it should be a square
	int defaultTileWidth = 256;
	int defaultTileHeight = 256;
	// int defaultTileWidth = 1024;
	// int defaultTileHeight = 1024;
	
	int _sourceXsubsample = 1;
	int _sourceYsubsample = 1;
	int _subsamplingXOffset = 0;
	int _subsamplingYOffset = 0;
	// these are the same, this is what RawRenderedImage uses:
	// private int scaleX, scaleY, xOffset, yOffset;
	
	/** The destination bounds. */
    private Rectangle destinationRegion;
    private Rectangle sourceRegion;
    private Rectangle originalRegion;
    private Point sourceOrigin;
    private Dimension originalDimension;
    private int maxXTile, maxYTile;

    /** The subsampling parameters. */
    private int scaleX, scaleY, xOffset, yOffset;
    private int[] destinationBands = null;
    private int[] sourceBands = null;
    private int nComp;

    /** Coordinate transform is not needed from the source (image stream)
     *  to the destination.
     */
    private boolean noTransform = true;
	
	// use params to setup how the image data should be read
	// if no params are present the user will get the data as it is.
	// 
	private ImageReadParam imageReadParam;
    
	public VicarRenderedImage(VicarInputFile vicarInputFile, ImageReadParam param, boolean d) throws Exception {
		 this.debug = d;
		 initVicarRenderedImage(vicarInputFile, param);		 
	}
   
    public VicarRenderedImage(VicarInputFile vicarInputFile, ImageReadParam param) throws Exception {
    	initVicarRenderedImage(vicarInputFile, param);
    }

    private void initVicarRenderedImage(VicarInputFile vicarInputFile, ImageReadParam param) throws Exception {
    	
        vif = vicarInputFile; 
        imageReadParam = param;
        boolean useCR = true;
        // tileWidth should be a part of the param
        // assume vicarInputFile has done all the initialization needed ??
        if (debug) {
        	System.out.println("Constructor VicarRenderedImage useCR = "+useCR);
        	System.out.println("ImageReadParam "+param);
        }
        
        sys = vif.getSystemLabel();
        width = sys.getNS();
        height = sys.getNL();
        int width_1 = width;
        int height_1 = height;
        
        // set a default tile size ???
        if (defaultTileWidth < width)
        	tileWidth = defaultTileWidth;
        else 
        	tileWidth = width;
        	
        if (defaultTileHeight < height)
        	tileHeight = defaultTileHeight;
        else
        	tileHeight = height;
        
        // tileWidth = width;
        // tileHeight = height;
        minX = 0;
        minY = 0;
        
        sourceRegion = new Rectangle(0, 0, this.width, this.height);

        originalRegion = (Rectangle)sourceRegion.clone();

        destinationRegion = (Rectangle)sourceRegion.clone();
        
        if (param != null) {
        	/* this makes the reader crop the image to the size and position of the sourceRegion
        	 * works for grayscale and color images (multiband)
        	 * subsampling will be handled by VicarInputFile and ImageInputStreamStride
        	 */
        	
        	if (useCR == false) {
        	Rectangle srcBounds = param.getSourceRegion();
        	if (debug) {
        		System.out.println("VicarRenderedImage using ImageReadParam srcBounds "+srcBounds);
        	}
        	if (srcBounds != null) {
        		
        		if (debug) {
        			System.out.println(" width "+this.width+"  height "+this.height+" tileWidth "+tileWidth+"  tileHeight "+tileHeight);
        			System.out.println(" srcBounds "+srcBounds.x+","+srcBounds.y+"  "+srcBounds.width+"x"+srcBounds.height);
        			System.out.println(" originalRegion "+originalRegion.x+","+originalRegion.y+"  "+originalRegion.width+"x"+originalRegion.height);
        			System.out.println(" sourceRegion "+sourceRegion.x+","+sourceRegion.y+"  "+sourceRegion.width+"x"+sourceRegion.height);
            		System.out.println(" destinationRegion "+destinationRegion.x+","+destinationRegion.y+"  "+destinationRegion.width+"x"+destinationRegion.height);
        		}
        		minX = srcBounds.x;
        		minY = srcBounds.y;
        	
        		tileGridXOffset = minX ;
        		tileGridYOffset = minY ;
        	
        		width = srcBounds.width;
        		height = srcBounds.height;
        		
        		destinationRegion.width = srcBounds.width;
        		destinationRegion.height = srcBounds.height;
        		
        		sourceRegion.x = srcBounds.x;
        		sourceRegion.y = srcBounds.y;
        		sourceRegion.width = srcBounds.width;
        		sourceRegion.height = srcBounds.height;
        	}
        }
        	
        	/***
        	try {
        		VicarImageReader.computeRegionsWrapper(param,
                    this.width, this.height,
                    param.getDestination(),
                    sourceRegion,
                    destinationRegion);
        	} catch (IllegalArgumentException e) {
        		if (debug) {
        			System.out.println(" VicarImageReader.computeRegionsWrapper "+e.toString());
        			// readAsRenderedImage ERROR: java.lang.IllegalArgumentException: Empty region!
        		}
        	}
        	***/
        	
        	BufferedImage bi = param.getDestination();
        	       	
        	if (useCR == true) {
        		if (debug) {
            		System.out.println(" called param.getDestination() bi "+bi);
        			System.out.println(" calling _computeRegions ");
        		}
        	try {
        		_computeRegions(param,
                    this.width, this.height,
                    null,  //param.getDestination(),
                    sourceRegion,
                    destinationRegion);
        	} catch (IllegalArgumentException e) {
        		if (debug) {
        			e.printStackTrace();
        			System.out.println("called VicarImageReader._computeRegions "+e.toString());
        		}
        	}
        	}
        	// use these values to adjust the destinationRegion.width,height and  and width,height
        	// width = width / _sourceXsubsample;
        	// height = height / _sourceYsubsample;
        	// destinationRegion.width = width;
    		// destinationRegion.height = height;
        	// then when we read the data use sourceRegion to construct what should be read and subsampled??
        	
        	_sourceXsubsample = param.getSourceXSubsampling();
        	_sourceYsubsample = param.getSourceYSubsampling();
        	_subsamplingXOffset = param.getSubsamplingXOffset();
        	_subsamplingYOffset = param.getSubsamplingYOffset();
        	
        	// set width and height to destinationRegion.width height
        	// this should take subsampling (and crop) into account
        	width = destinationRegion.width;
        	height = destinationRegion.height;
        	
        	if (debug) {
        		System.out.println("VicarRenderedImage  Constructor  useCR = "+useCR);
        		System.out.println(" width   "+this.width+"  height   "+this.height+"  tileGridXOffset "+tileGridXOffset+"  tileGridYOffset "+tileGridYOffset );
        		
        		System.out.println(" width_1 "+width_1+"  height_1 "+height_1+"  minX "+minX+"   minY "+minY);
        		System.out.println(" originalRegion "+originalRegion.x+","+originalRegion.y+"  "+originalRegion.width+"x"+originalRegion.height);
        		System.out.println(" sourceRegion "+sourceRegion.x+","+sourceRegion.y+"  "+sourceRegion.width+"x"+sourceRegion.height);
        		System.out.println(" destinationRegion "+destinationRegion.x+","+destinationRegion.y+"  "+destinationRegion.width+"x"+destinationRegion.height);
        		System.out.print(" _sourceXsubsample "+_sourceXsubsample+" _sourceYsubsample "+_sourceYsubsample);
        		System.out.println(" _subsamplingXOffset "+_subsamplingXOffset+"  _subsamplingYOffset "+_subsamplingYOffset);
        	}
        	
        	// RawImageReader uses scaleX, scaleY, xOffset, yOffset for the above values
        }
        
        // VicarInputFile uses these when reading the data
        vif.setSourceXSubsample(_sourceXsubsample);    	
    	vif.setSourceYSubsample(_sourceYsubsample) ;
    	
    	vif.setSourceRegion(sourceRegion);
    	vif.setDestinationRegion(destinationRegion);	
    	
        // should this be from sourceRegion ???
        // this.tileGridXOffset = destinationRegion.x;
        // this.tileGridYOffset = destinationRegion.y;
        
        sourceOrigin = new Point(sourceRegion.x, sourceRegion.y);
        if (!destinationRegion.equals(sourceRegion))
            noTransform = false;
        
        if (debug) {
    		System.out.println("  noTransform = "+noTransform);
        }
        
        sampleModel = vif.createSampleModel();
        
        if (debug) {
    		System.out.println("sampleModel "+sampleModel.getWidth()+" "+sampleModel.getHeight()+" "+sampleModel.getNumBands());
        }
        // added by Bob Deen 8-20-03
        colorModel = PlanarImage.createColorModel(sampleModel);
        if (colorModel == null) {
        	if (sampleModel.getNumBands() == 1) {
        		colorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_GRAY),
        			false, false, Transparency.OPAQUE, sampleModel.getTransferType());
        	}
        }
        
		if (colorModel == null) {
			if (debug) System.out.println("Constructor VicarRenderedImage ColorModel is NULL");
			// add code to try again to create a ColorModel
		}	
        
        
        
    }
	
	/*
	 * getter so a user can get the VicarInputFile and modify how the data will be read
	 * Best is to use ImageReadParam to set things up correctly to pull single bands
	 * or 3 when > 3 are present	 */
	public VicarInputFile getVicarInputFile() {
		return vif;
	}
	
	
	
	public SampleModel getSampleModel() {
		if (debug) System.out.println("VicarRenderedImage.getSampleModel");
		// SampleModel tileSampleModel = vif.createSampleModel(tWidth, tHeight);
		if (sampleModel == null)
			sampleModel = vif.createSampleModel();
		
		return sampleModel;
	}
	
	
	/**
	 * getNewSampleModel
	 * force recreation of the sampleModel and ColorModel after the tileWidth and tileHeight have been modified
	 * @return SampleModel
	 */
	public SampleModel getNewSampleModel() {
		if (debug) System.out.println("VicarRenderedImage.getNewSampleModel tileWidth="+tileWidth+"  tileHeight="+tileHeight);
		sampleModel = vif.createSampleModel(tileWidth, tileHeight);
    
    	// added by Bob Deen 8-20-03
    	colorModel = PlanarImage.createColorModel(sampleModel);
    	if (colorModel == null) {
    		if (sampleModel.getNumBands() == 1) {
    			colorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_GRAY),
    					false, false, Transparency.OPAQUE, sampleModel.getTransferType());
    		}
    	}
    	    
    	if (colorModel == null) {
    		if (debug) System.out.println("Constructor VicarRenderedImage ColorModel is NULL");
    		// add code to try again to create a ColorModel
    	}	
	return sampleModel;
	}
	
	/* 
	 * This will not have a ColorModel	 */
	public ColorModel getColorModel() {
		if (debug) System.out.println("VicarRenderedImage.getColorModel");
		return colorModel;
	}
	
	/**
	 *  Setting the tileWidth is an important optimization.<br>
	 * Images used in a display which handle tiling should set tile width to
	 * some square such as 256.This will allow efficient scrolling in a large image.
	 * If the image is being copied from one file to another it will be best to
	 * set tileWidth to the image width (samples). <br>
	 * 	 */
	public void setTileWidth(int w) {
		if (debug) System.out.println("VicarRenderedImage.setTileWidth "+w);
		if (w == 0) {
			// if this is 0 set width to the actual width of the image
			tileWidth = width;
		} else {
			tileWidth = w;
		}
	}
	
	public void setDefaultTileWidth(int w) {
		if (debug) System.out.println("VicarRenderedImage.setDefaultTileWidth "+w);
		defaultTileWidth = w;
	}
	
	/**
	 *  Setting the tileHeight is an important optimization.<br>
	 * Images used in a display which handle tiling should set tile height equal to
	 * the tileWidth.This will allow efficient scrolling in a large image.
	 * If the image is being copied from one file to another it will be best to
	 * set tileHeight to something like 256. <br>
	 * 
	 */
	public void setTileHeight(int h) {
		if (debug) System.out.println("VicarRenderedImage.setTileHeight "+h);
		tileHeight = h;
	}
	
	public void setDefaultTileHeight(int h) {
		if (debug) System.out.println("VicarRenderedImage.setDefaultTileHeight "+h);
		defaultTileHeight = h;
	}
	
	public void setSourceXsubsample(int sourceXsubsample) {
		_sourceXsubsample = sourceXsubsample;
	}
	
	public void setSourceYsubsample(int sourceYsubsample) {
		_sourceYsubsample = sourceYsubsample;
	}
	
 /**
    * computeTile calculates the rectangle in the image space which 
    * holds the data for the requested tile. 
    * ComputeTile then gets the data from the file and returns the Raster 
    * containing the data.
    *
    */
    private Raster computeTile(int tileX, int tileY) {
		if (debug) { 
			System.out.println("VicarRenderedImage.computeTile start "+tileX +" "+tileY+"   "+this.width+" x "+this.height);
		}
        WritableRaster theTile, theSSTile;
        theTile = null;
        theSSTile = null;
        //if (theTile != null) {
            // return theTile;
        // }
        
        // set the origin of this tile into the whole image
        // this tells paint() where to draw this tile into the window
        Point org = new Point(tileXToX(tileX), tileYToY(tileY));
        // override tileXToX(tileX) and tileYToY(tileY) for sourceRegion
        // also getMaxX() and getMaxY() SimpleRenderedImage
        
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
        
        int ss_xleft = 0;
        int ss_yleft = 0;
        int ss_width = 0;
        int ss_height = 0;
        int ss_tHeight = 0;
        int ss_tWidth = 0;
        
        // calculate the x,y of the tile corner
        // this positions the tile when it is drawn to the window
        int tx = tileXToX(tileX);
        int ty = tileYToY(tileY);
        
        /** this is done inside getSampleModel
         * calculates tile size at the edges **/
        // this.tileGridXOffset
        // this.tileGridYOffset
         xleft = (width + this.tileGridXOffset) - tx;
         yleft = (height + this.tileGridYOffset) - ty;
         
         tWidth = tileWidth;       
         tHeight = tileHeight;
        
        int ss_tx = tileXToX(tileX, _sourceXsubsample);
        int ss_ty = tileYToY(tileY, _sourceYsubsample);
        Point ss_org = new Point(ss_tx, ss_ty);
        
        ss_width = width / _sourceXsubsample ;
        ss_height = height / _sourceYsubsample ;
        
        int ss_tileGridXOffset = this.tileGridXOffset / _sourceXsubsample ;
        int ss_tileGridYOffset = this.tileGridYOffset / _sourceYsubsample ;
        
        ss_xleft = (ss_width + ss_tileGridXOffset) - ss_tx;
        ss_yleft = (ss_height + ss_tileGridYOffset) - ss_ty;
      
        int xs = _sourceXsubsample;
        int ys = _sourceYsubsample;
        int sourceYsubsample = 1;
        ss_tWidth = tWidth / _sourceXsubsample ;
        ss_tHeight = tHeight / _sourceYsubsample ;
        
        // limit the tile size at the edges for a subsampled tile
        ss_tWidth = ss_xleft;
        ss_tHeight = ss_yleft;
        
        
        // new versions which use the subsample value
        // getMaxX() getMaxX(_sourceXSubsample)
        // getMaxY() getMaxY(_sourceYSubsample)
               
        if (debug) {
        	System.out.println("XX VicarRenderedImage.computeTile "+tx+","+ty+" "+ tWidth+"x"+tHeight+"  tileX "+tileX+" tileY "+tileY);  
        	
        	System.out.println("  XX tileGridXOffset = "+ tileGridXOffset+"   tileGridYOffset = "+tileGridYOffset );
        	// System.out.println("  defaultTileWidth "+defaultTileWidth+"   defaultTileHeight "+defaultTileHeight+"   ");
        	// System.out.println("  tileWidth "+tileWidth+"   tileHeight "+tileHeight+"   ");
        	System.out.println("  XX (org) tx "+tx+" ty "+ty+"  xleft "+ xleft+" yleft "+yleft+"  width "+width+" height "+height); 
        	System.out.println("  ss_tx "+ss_tx+" ss_ty "+ss_ty+"  ss_xleft "+ ss_xleft+" ss_yleft "+ss_yleft+" ss_width "+ss_width+"  ss_height "+ss_height);
        	System.out.println("  tWidth "+tWidth+"  tHeight "+tHeight+" ss_tWidth "+ss_tWidth+"  ss_tHeight "+ss_tHeight+"  noTransform = "+noTransform);
        	System.out.println("  ss getMaxX() "+getMaxX(_sourceXsubsample) +" ss getMaxY() "+getMaxY(_sourceYsubsample)+" ss_org "+ss_org+", getMaxX() "+getMaxX() +"   getMaxY() "+getMaxY()+" org "+org+"  noTransform = "+noTransform);
        	System.out.println("  _sourceXsubsample "+_sourceXsubsample+"  _sourceYsubsample "+_sourceYsubsample);
        	System.out.println("  _subsamplingXOffset "+_subsamplingXOffset+"  _subsamplingYOffset "+_subsamplingYOffset);
        	System.out.println("  originalRegion "+originalRegion.x+","+originalRegion.y+"  "+originalRegion.width+"x"+originalRegion.height);
        	System.out.println("  sourceRegion "+sourceRegion.x+","+sourceRegion.y+"  "+sourceRegion.width+"x"+sourceRegion.height);
    		System.out.println("  destinationRegion "+destinationRegion.x+","+destinationRegion.y+"  "+destinationRegion.width+"x"+destinationRegion.height);
		// Throwable th = new Throwable();
		// th.printStackTrace() ;
        // set values into vif that we need when reading the data subsampled
        // sourceRegion, _sourceXsubsample+"  _sourceYsubsample
        }
        
        // sub sample versions of everything
        SampleModel ss_tileSampleModel = vif.createSampleModel(ss_tWidth, ss_tHeight);
        theSSTile = Raster.createWritableRaster(ss_tileSampleModel, ss_org);          
        Rectangle ssTileRect = theSSTile.getBounds();              
        DataBuffer ss_dbuf = theSSTile.getDataBuffer();
        
        // limit the tWidth based on the destinationRegion
        if (tWidth > xleft && _sourceXsubsample != 1) {
        	tWidth = xleft;
        	if (debug) {
        	System.out.println("  XXX  tx "+tx+" ty "+ty+"  xleft="+ xleft+" yleft="+yleft+"  width="+width+" height="+height+" tWidth="+tWidth+" tHeight="+tHeight); 
        	}
        }
        
        if (tHeight > yleft && _sourceYsubsample != 1) {
        	tHeight = yleft;
        	if (debug) {
        	System.out.println("  XXX  tx "+tx+" ty "+ty+"  xleft="+ xleft+" yleft="+yleft+"  width="+width+" height="+height+" tWidth="+tWidth+" tHeight="+tHeight); 
        	}
        }
        
        
        SampleModel tileSampleModel = vif.createSampleModel(tWidth, tHeight);                
        theTile = Raster.createWritableRaster(tileSampleModel, org);          
        Rectangle tileRect = theTile.getBounds();              
        DataBuffer dbuf = theTile.getDataBuffer(); // I don't care what sub-type the data buffer is
        // the vicar IO library will go read the file to get the data
        
        
        
        int bands = tileSampleModel.getNumBands();
        int tw = tileSampleModel.getWidth();
        int th = tileSampleModel.getHeight();
        int type = tileSampleModel.getDataType();
        int banks = dbuf.getNumBanks();
        int size = dbuf.getSize();
        int off[] = dbuf.getOffsets();
        int offlen = off.length;
        
        // subsampled versions of these values
        int ss_bands = ss_tileSampleModel.getNumBands();
        int ss_tw = ss_tileSampleModel.getWidth();
        int ss_th = ss_tileSampleModel.getHeight();
        int ss_type = ss_tileSampleModel.getDataType();
        int ss_banks = ss_dbuf.getNumBanks();
        int ss_size = ss_dbuf.getSize();
        int ss_off[] = ss_dbuf.getOffsets();
        int ss_offlen = ss_off.length;
        
       
        if (debug) {
        	// debug test
        	System.out.println("  tileSampleModel bands "+bands+" "+tw+"x"+th);
        	// System.out.println("  tileSampleModel "+tileSampleModel);
        
        	System.out.print("  DataBuffer size="+size+" banks="+banks+" offsets.length="+offlen);
        	for (int i = 0 ; i<offlen ; i++) {
        		System.out.print("["+i+"]=" +off[i]+" ");
        	}
        	System.out.println(" ");
        	System.out.println("  tileRect "+tileRect);
        
        	System.out.println("  ss_tileSampleModel ss_bands "+ss_bands+" "+ss_tw+"x"+ss_th);
        	// System.out.println("  tileSampleModel "+tileSampleModel);
        
        	System.out.print("  SS DataBuffer ss_size="+ss_size+" ss_banks="+ss_banks+" ss_offsets.length="+ss_offlen);
        	for (int i = 0 ; i<ss_offlen ; i++) {
        		System.out.print("["+i+"]=" +ss_off[i]+" ");
        	}
        	System.out.println(" ");
        	System.out.println("  ssTileRect "+ssTileRect);
        }
        boolean doSS = false;
        if ((_sourceXsubsample > 1 || _sourceYsubsample > 1) && doSS) {
        	try {
        		if (debug) System.out.println("subsampled readTile +++++++++++++++++++++++");
            	vif.readTile(ss_tx, ss_ty, ss_tileSampleModel, ss_dbuf); // fill the data buffer
            	
             } catch (IOException e) {
                  e.printStackTrace();
                  throw new RuntimeException( "IOException occured while processing VicarRenderedImage file.");
             }
        	return theSSTile;
                   	
        } else {
        	try {
        		if (debug) System.out.println("normal readTile ------------------");
            	vif.readTile(tx, ty, tileSampleModel, dbuf); // fill the data buffer
            	
             } catch (IOException e) {
                  e.printStackTrace();
                  throw new RuntimeException( "IOException occured while processing VicarRenderedImage file.");
             }
            
            return theTile;
        }
        
        
        
    }




    /**
    * getTile is called by any program using this decoder when it wants the data for 
    * a tile in the image. 
    * If the image is not tiled, then tile 0,0 returns the entire image.
    */
    public synchronized Raster getTile(int tileX, int tileY) {
		if (debug) System.out.println("VicarRenderedImage.getTile "+tileX +" "+tileY+" "+this.width+" x "+this.height);
		// if (debug && tileX == 0 && tileY == 0) new Throwable().printStackTrace();
        return computeTile(tileX, tileY);
    }
    
    
    /**
     * Copies an arbitrary rectangular region of the RenderedImage
     * into a caller-supplied WritableRaster.  The region to be
     * computed is determined by clipping the bounds of the supplied
     * WritableRaster against the bounds of the image.  The supplied
     * WritableRaster must have a SampleModel that is compatible with
     * that of the image.
     *
     * <p> If the raster argument is null, the entire image will
     * be copied into a newly-created WritableRaster with a SampleModel
     * that is compatible with that of the image.
     *
     * @param dest a WritableRaster to hold the returned portion of
     *        the image.
     * @return a reference to the supplied WritableRaster, or to a 
     *         new WritableRaster if the supplied one was null.
     */
    public WritableRaster copyData(WritableRaster dest) {
        Rectangle bounds;
        Raster tile;
		if (debug) System.out.println("VicarRenderedImage.copyData ");           
        if (dest == null) {
            bounds = getBounds();
            Point p = new Point(minX, minY);
            /* A SampleModel to hold the entire image. */
            if (sampleModel == null) getSampleModel();
            SampleModel sm = sampleModel.createCompatibleSampleModel(
                                         width, height);
            dest = Raster.createWritableRaster(sm, p);
        } else {
            bounds = dest.getBounds();
        }
        
        int startX = XToTileX(bounds.x);
        int startY = YToTileY(bounds.y);
        int endX = XToTileX(bounds.x + bounds.width - 1);
        int endY = YToTileY(bounds.y + bounds.height - 1);
        
        if (debug) {
        	System.out.println("VicarRenderedImage.copyData startX="+startX+"  endX="+endX+"  startY="+startY+"  endY="+endY+"  ");   
        }
            
        for (int j = startY; j <= endY; j++) {
            for (int i = startX; i <= endX; i++) {
                tile = getTile(i, j);
                Rectangle tileRect = tile.getBounds();
                Rectangle intersectRect =
                    bounds.intersection(tile.getBounds());
                Raster liveRaster = tile.createChild(intersectRect.x,
                                                     intersectRect.y,
                                                     intersectRect.width,
                                                     intersectRect.height,
                                                     intersectRect.x,
                                                     intersectRect.y,
                                                     null);

                /*
                 * WritableRaster.setDataElements takes into account of
                 * inRaster's minX and minY and add these to x and y. Since
                 * liveRaster has the origin at the correct location, the
                 * following call should not again give these coordinates in
                 * places of x and y.
                 */
                dest.setDataElements(0, 0, liveRaster);
            }
        }
        return dest;
    }
    
    
    /**
     * Computes the source region of interest and the destination
     * region of interest, taking the width and height of the source
     * image, an optional destination image, and an optional
     * <code>ImageReadParam</code> into account.  The source region
     * begins with the entire source image.  Then that is clipped to
     * the source region specified in the <code>ImageReadParam</code>,
     * if one is specified.
     *
     * <p> If either of the destination offsets are negative, the
     * source region is clipped so that its top left will coincide
     * with the top left of the destination image, taking subsampling
     * into account.  Then the result is clipped to the destination
     * image on the right and bottom, if one is specified, taking
     * subsampling and destination offsets into account.
     *
     * <p> Similarly, the destination region begins with the source
     * image, is translated to the destination offset given in the
     * <code>ImageReadParam</code> if there is one, and finally is
     * clipped to the destination image, if there is one.
     *
     * <p> If either the source or destination regions end up having a
     * width or height of 0, an <code>IllegalArgumentException</code>
     * is thrown.
     *
     * <p> The {@link #getSourceRegion <code>getSourceRegion</code>}
     * method may be used if only source clipping is desired.
     *
     * @param param an <code>ImageReadParam</code>, or <code>null</code>.
     * @param srcWidth the width of the source image.
     * @param srcHeight the height of the source image.
     * @param image a <code>BufferedImage</code> that will be the
     * destination image, or <code>null</code>.
     * @param srcRegion a <code>Rectangle</code> that will be filled with
     * the source region of interest.
     * @param destRegion a <code>Rectangle</code> that will be filled with
     * the destination region of interest.
     * @exception IllegalArgumentException if <code>srcRegion</code>
     * is <code>null</code>.
     * @exception IllegalArgumentException if <code>dstRegion</code>
     * is <code>null</code>.
     * @exception IllegalArgumentException if the resulting source or
     * destination region is empty.
     */
    protected void _computeRegions(ImageReadParam param,
                                         int srcWidth,
                                         int srcHeight,
                                         BufferedImage image,
                                         Rectangle srcRegion,
                                         Rectangle destRegion) {
        
    	if (debug) {
    		System.out.println("VicarRenderedImage._computeRegions");
    		System.out.println(" srcWidth "+srcWidth+"  srcHeight "+srcHeight+"  ");
    		System.out.println(" srcRegion "+srcRegion.x+","+srcRegion.y+"  "+srcRegion.width+"x"+srcRegion.height);
        	System.out.println(" destRegion "+destRegion.x+","+destRegion.y+"  "+destRegion.width+"x"+destRegion.height);
    	}
    	
    	if (srcRegion == null) {
            throw new IllegalArgumentException("srcRegion == null!");
        }
        if (destRegion == null) {
            throw new IllegalArgumentException("destRegion == null!");
        }

        // Start with the entire source image
        srcRegion.setBounds(0, 0, srcWidth, srcHeight);

        // Destination also starts with source image, as that is the
        // maximum extent if there is no subsampling
        destRegion.setBounds(0, 0, srcWidth, srcHeight);

        // Clip that to the param region, if there is one
        int periodX = 1;
        int periodY = 1;
        int gridX = 0;
        int gridY = 0;
        if (param != null) {
            Rectangle paramSrcRegion = param.getSourceRegion();
            if (paramSrcRegion != null) {
                srcRegion.setBounds(srcRegion.intersection(paramSrcRegion));
            }
            periodX = param.getSourceXSubsampling();
            periodY = param.getSourceYSubsampling();
            gridX = param.getSubsamplingXOffset();
            gridY = param.getSubsamplingYOffset();
            srcRegion.translate(gridX, gridY);
            srcRegion.width -= gridX;
            srcRegion.height -= gridY;
            destRegion.setLocation(param.getDestinationOffset());
        }

        // Now clip any negative destination offsets, i.e. clip
        // to the top and left of the destination image
        if (destRegion.x < 0) {
            int delta = -destRegion.x*periodX;
            srcRegion.x += delta;
            srcRegion.width -= delta;
            destRegion.x = 0;
        }
        if (destRegion.y < 0) {
            int delta = -destRegion.y*periodY;
            srcRegion.y += delta;
            srcRegion.height -= delta;
            destRegion.y = 0;
        }

        // Now clip the destination Region to the subsampled width and height
        int subsampledWidth = (srcRegion.width + periodX - 1)/periodX;
        int subsampledHeight = (srcRegion.height + periodY - 1)/periodY;
        destRegion.width = subsampledWidth;
        destRegion.height = subsampledHeight;
        
        if (debug) {
    		System.out.println(" periodX "+periodX+" periodY "+periodY+"  gridX "+gridX+" gridY "+gridY+"  ");
    		System.out.println(" subsampledWidth "+subsampledWidth+"  subsampledHeight "+subsampledHeight+"  ");
    		System.out.println(" srcRegion "+srcRegion.x+","+srcRegion.y+"  "+srcRegion.width+"x"+srcRegion.height);
        	System.out.println(" destRegion "+destRegion.x+","+destRegion.y+"  "+destRegion.width+"x"+destRegion.height);
    	}

        // Now clip that to right and bottom of the destination image,
        // if there is one, taking subsampling into account
        if (image != null) {
            Rectangle destImageRect = new Rectangle(0, 0,
                                                    image.getWidth(),
                                                    image.getHeight());
            destRegion.setBounds(destRegion.intersection(destImageRect));
            if (destRegion.isEmpty()) {
                throw new IllegalArgumentException
                    ("Empty destination region!");
            }

            int deltaX = destRegion.x + subsampledWidth - image.getWidth();
            if (deltaX > 0) {
                srcRegion.width -= deltaX*periodX;
            }
            int deltaY =  destRegion.y + subsampledHeight - image.getHeight();
            if (deltaY > 0) {
                srcRegion.height -= deltaY*periodY;
            }
        }
        
        
        if (srcRegion.isEmpty() || destRegion.isEmpty()) {
            throw new IllegalArgumentException("Empty region!");
        }
        
        
        minX = srcRegion.x;
		minY = srcRegion.y;
	
		tileGridXOffset = minX ;
		tileGridYOffset = minY ;
	
		width = srcRegion.width;
		height = srcRegion.height;
		
		if (debug) {
    		System.out.print(" minX="+minX+"  minY="+minY+"  tileGridXOffset="+tileGridXOffset+" tileGridYOffset="+tileGridYOffset);
    		System.out.println(" width="+width+" height="+height);
		}
    }

    /**
     * Converts a horizontal tile index into the X coordinate of its
     * upper left pixel.  This is a convenience method.  No attempt is made
     * to detect out-of-range indices.
     *
     * @param tx the horizontal index of a tile.
     * @return the X coordinate of the tile's upper left pixel.
     */
    public int tileXToX(int tx, int Xsubsample) {
    	int x = tileXToX(tx);
    	x = x / Xsubsample;
        return x;
    }

    /**
     * Converts a vertical tile index into the Y coordinate of its
     * upper left pixel.  This is a convenience method.  No attempt is made
     * to detect out-of-range indices.
     *
     * @param ty the vertical index of a tile.
     * @return the Y coordinate of the tile's upper left pixel.
     */
    public int tileYToY(int ty, int Ysubsample) {
       
    	int y = tileYToY(ty);
    	y = y / Ysubsample;
        return y;
    }
    
    public int getMaxX(int Xsubsample) {
        
    	return getMaxX() / Xsubsample;
        
    }
    
    public int getMaxY(int Ysubsample) {
        
    	return getMaxY() / Ysubsample;
        
    }

    public void setDebug(boolean d) {
    	debug = d;
    }
    
    public boolean getDebug() {
    	return debug ;
    }
    
}
