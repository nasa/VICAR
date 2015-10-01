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
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;

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
	
	// use params to setup how the image data should be read
	// if no params are present the user will get the data as it is.
	// 
	private ImageReadParam imageReadParam;
    
   
    public VicarRenderedImage(VicarInputFile vicarInputFile, ImageReadParam param) throws Exception {

        vif = vicarInputFile; 
        imageReadParam = param;
        // tileWidth should be a part of the param
        // assume vicarInputFile has done all the initialization needed ??
        if (debug) {
        	System.out.println("Constructor VicarRenderedImage");
        	System.out.println("ImageReadParam "+param);
        }
        
        sys = vif.getSystemLabel();
        width = sys.getNS();
        height = sys.getNL();
        
        // set a default tile size ???
        if (defaultTileWidth < width)
        	tileWidth = defaultTileWidth;
        else 
        	tileWidth = width;
        	
        if (defaultTileHeight < height)
        	tileHeight = defaultTileHeight;
        else
        	tileHeight = height;
        	
        sampleModel = vif.createSampleModel();
        
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
        // tileWidth = width;
        // tileHeight = height;
        minX = 0;
        minY = 0;
        
        
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
	
 /**
    * computeTile calculates the rectangle in the image space which 
    * holds the data for the requested tile. 
    * ComputeTile then gets the data from the file and returns the Raster 
    * containing the data.
    *
    */
    private Raster computeTile(int tileX, int tileY) {
		if (debug) System.out.println("VicarRenderedImage.computeTile "+tileX +" "+tileY);
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
                
        tWidth = tileWidth;       
        tHeight = tileHeight;
               
        if (debug) {
        	System.out.println("VicarRenderedImage.computeTile "+tx+","+ty+" "+ tWidth+"x"+tHeight+"  tileX "+tileX+" tileY "+tileY);
        
        // System.out.println("  defaultTileWidth "+defaultTileWidth+"   defaultTileHeight "+defaultTileHeight+"   ");
        // System.out.println("  tileWidth "+tileWidth+"   tileHeight "+tileHeight+"   ");
        System.out.println("  tx "+tx+" ty "+ty+"   xleft "+ xleft+" yleft "+yleft+"  "+width+"x"+height); 
        System.out.println("  getMaxX() "+getMaxX() +"   getMaxY() "+getMaxY()+" org "+org );
		// Throwable th = new Throwable();
		// th.printStackTrace() ;
        }
        
        SampleModel tileSampleModel = vif.createSampleModel(tWidth, tHeight);
                
        theTile = Raster.createWritableRaster(tileSampleModel, org);
          
        Rectangle tileRect = theTile.getBounds();
      
        
        DataBuffer dbuf = theTile.getDataBuffer(); // I don't care what sub-type the data buffer is
        // the vicar IO library will go read the file to get the data
        
        DataBuffer dbuf2 = null;
        int bands = tileSampleModel.getNumBands();
        int tw = tileSampleModel.getWidth();
        int th = tileSampleModel.getHeight();
        int type = tileSampleModel.getDataType();
        int banks = dbuf.getNumBanks();
        int size = dbuf.getSize();
        int off[] = dbuf.getOffsets();
        int offlen = off.length;
       
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
        
        	/**
        	WritableRaster theTile2 = Raster.createBandedRaster(type,tw,th,bands,org) ;
        	dbuf2 = theTile2.getDataBuffer();
        	banks = dbuf2.getNumBanks();
        	size = dbuf2.getSize();
        	off = dbuf2.getOffsets();
        	offlen = off.length;
        	System.out.println(" DataBuffer2 size="+size+" banks="+banks+" offsets.length="+offlen);
        	for (int i = 0 ; i<offlen ; i++) {
        		System.out.print("["+i+"]=" +off[i]+" ");
        		}
        	System.out.println(" ");
        	***/
        }
        
        
         try {
            vif.readTile(tx, ty, tileSampleModel, dbuf); // fill the data buffer
         } catch (IOException e) {
              e.printStackTrace();
              throw new RuntimeException( "IOException occured while processing VicarRenderedImage file.");
         }
        

        return theTile;
    }




    /**
    * getTile is called by any program using this decoder when it wants the data for 
    * a tile in the image. 
    * If the image is not tiled, then tile 0,0 returns the entire image.
    */
    public synchronized Raster getTile(int tileX, int tileY) {
		if (debug) System.out.println("VicarRenderedImage.getTile "+tileX +" "+tileY);
		if (debug && tileX == 0 && tileY == 0) new Throwable().printStackTrace();
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
    
}
