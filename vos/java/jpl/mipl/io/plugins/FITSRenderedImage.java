package jpl.mipl.io.plugins;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.Raster;

import jpl.mipl.io.codec.SimpleRenderedImage;
import jpl.mipl.io.vicar.SystemLabel;
import jpl.mipl.io.vicar.VicarIOBase;

import javax.imageio.ImageReadParam;
import javax.media.jai.PixelAccessor;
import javax.media.jai.PlanarImage;
import javax.media.jai.UnpackedImageData;

import java.awt.image.*;
import java.io.IOException;

//FITSIO libraries
import nom.tam.fits.*;
import nom.tam.image.*;
import nom.tam.util.*;

public class FITSRenderedImage extends SimpleRenderedImage {
	
	
	int defaultTileWidth = 256;
	int defaultTileHeight = 256;
	/** defined in SimpleRenderedImage
	int tileWidth = 256;
	int tileHeight = 256;
	**/
	
	BasicHDU hdu = null;
	
	int data_buffer_type = DataBuffer.TYPE_BYTE;
	
	// boolean flip_image = false;
	// boolean have_read_header = false;
	
	boolean debug = false;
	// ColorModel colorModel = null;
	// SampleModel sampleModel = null;
	

	public FITSRenderedImage() {
		// TODO Auto-generated constructor stub
	}
	
	
	/***
	 * FITSRenderedImage
	 * 
	 * @param fits
	 * @param param
	 * @param d
	 * create on RenderedImage per image if there are multiple images??
	 * pass in the HDU also? or the image index??
	 * Is data read from the HDU of do we need fits?
	 */
	// public FITSRenderedImage(Fits fits, ImageReadParam param, boolean d) {
	public FITSRenderedImage(BasicHDU hdu, ImageReadParam param, boolean d) {
				
		initFITSRenderedImage(hdu, param, d) ;
	}
	
	private void initFITSRenderedImage(BasicHDU h, ImageReadParam param, boolean d) {
		this.debug = d;
		hdu = h;
		// for now ignore ImageReadParam, check if instanceof FITSImageReadParam
		int bitpix = 0;
		int[] naxis = {0,0};
		double min = 0.0;
		double max = 0.0;
		double scale = 1.0;
		double zero = 0.0;
		String units = "none";

		if (hdu == null) {
			if (debug) {
				System.out.printf("FITSRenderedImage hdu is null \n");
			}
			return;
		}
		
		try {
			bitpix = hdu.getBitPix();   
			naxis = hdu.getAxes();
			if (debug) {
				System.out.println("bitpix="+bitpix);
				System.out.println("naxis="+naxis.length);

				for (int j=0 ; j<naxis.length ; j++) {
					System.out.println(" naxis["+j+"]="+naxis[j]);
				}
			}

			// how do we find out if the image is compressed??
			min = hdu.getMinimumValue();
			max = hdu.getMaximumValue();
			scale = hdu.getBScale();
			zero = hdu.getBZero();
			units = hdu.getBUnit();
		} catch (FitsException e) {
			if (debug) {
				System.out.println("###############################################################");
				System.out.println("FitsException ");
				e.printStackTrace();
			}
		}
 		if (debug) {
 		  System.out.println("###############################################################");
 		  System.out.println(" ");
 		  System.out.println("min "+min+"  max "+max+"  scale "+scale+"  zero "+zero +" units "+units);
 		  // h.info();
 		  System.out.println(" ");
 		  System.out.println("###############################################################");
 		}
		
		// create SampleModel
 		sampleModel = createSampleModel(bitpix, naxis, zero);
 		
 		width  = sampleModel.getWidth();
		height = sampleModel.getHeight();
		int bands  = sampleModel.getNumBands();
		int tType  = sampleModel.getTransferType();
		int dataType = sampleModel.getDataType();
		if (debug) {
			System.out.println("sampleModel width="+width+"  height="+height+"  bands="+bands+"  tType="+tType+" dataType="+dataType);
		}
		// create ColorModel
        colorModel = PlanarImage.createColorModel(sampleModel);
        if (colorModel == null) {
        	if (sampleModel.getNumBands() == 1) {
        		colorModel = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_GRAY),
        			false, false, Transparency.OPAQUE, sampleModel.getTransferType());
        	}
        }
        
		if (colorModel == null) {
			if (debug) System.out.println("Constructor FITSRenderedImage ColorModel is NULL");
			// add code to try again to create a ColorModel
		}	
		// set a default tile size ???
		if (defaultTileWidth < width)
			tileWidth = defaultTileWidth;
		else 
			tileWidth = width;

		if (defaultTileHeight < height)
			tileHeight = defaultTileHeight;
		else
			tileHeight = height;

		minX = 0;
		minY = 0;
		if (debug) {
			System.out.printf("FITSRenderedImage tileHeight = %d tileWidth = %d) \n", tileHeight, tileWidth);
		}

		
	}
	
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
    
    /***********************************************************************
     * Creates a SampleModel that is most compatible with the input image.
     * Follow the See Also link for complete description
     * @see VicarIOBase#createSampleModel(int,int)
     * bitpix is bits per pixel
     * naxis gives the lines and samples
     * tileWidth, tileHeight are as the size of this tile
     * zero is the offset which must be added to the data to get true values
     */
        public SampleModel createTileSampleModel(int bitpix, int[] naxis, int tileWidth, int tileHeight, double zero)
        {
    	int i;
    	int pixel_stride;
    	int scanline_stride;
    	int num_bands = 1;
    	boolean float_type;
    	  	
    	int ns = 0;
    	int nl = 0;
    	int nb = 0;
    	int formatCode;
    	int orgCode;
    	int bytesPerPixel = Math.abs(bitpix/8);
    	
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
    	  System.out.println("  tileWidth="+tileWidth+"  tileHeight="+tileHeight);
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
    	
    	
    	switch (orgCode) {
    	    case SystemLabel.ORG_BSQ:
    	    	// One bank per band of data
    	    	pixel_stride = 1;
    	    	scanline_stride = tileWidth;
    	    	for (i=0; i < num_bands; i++) {
    	    		band_offsets[i] = 0;
    	    		bank_indices[i] = i;
    	    	}


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


	
    /**
	 * getTile
	 * @param tileX
	 * @param tileY
	 * @return Raster
	 * This is where the data is actually read
	 */
	@Override
	public Raster getTile(int tileX, int tileY) {
		// TODO Auto-generated method stub
		if (debug) {
			System.out.printf("FITSRenderedImage getTile(%d, %d) ##############################> \n", tileX, tileY);
		}
		return computeTile(tileX, tileY);	
	}
	
	/**
	 * computeTile
	 * @param tileX
	 * @param tileY
	 * @return Raster
	 * This is where the data is actually read
	 * assuming all data is BSQ (band sequential)
	 */
	public Raster computeTile(int tileX, int tileY) {
		
		if (debug) {
			System.out.printf("FITSRenderedImage computeTile(%d, %d) \n", tileX, tileY);
		}
		boolean isOddTile = true;
        if (tileX %2 != 1 && tileY %2 != 1) isOddTile = false;
        else if (tileX %2 == 1 && tileY %2 == 1) isOddTile = false;
        else isOddTile = true;
        
        // create a sampleModel for this tile
        
        int xleft = 0;
        int xright = 0;
        int ytop = 0;
        int ybottom = 0;       
        int tWidth = 0;
        int tHeight = 0;
        int bitpix = 0;
		int[] naxis = {0,0};
		double zero = 0.0;
		WritableRaster theTile = null;
		       
        // calculate the x,y of the tile corner
        // this positions the tile when it is drawn to the window
        int tx = tileXToX(tileX);
        int ty = tileYToY(tileY);
        
        // set the origin of this tile into the whole image
        // this tells paint() where to draw this tile into the window
        Point org = new Point(tx, ty);
        // this is the origin of the outout tile, not the input data
        Point zorg = new Point(0, 0);
        
        /** this is done inside getSampleModel
         * calculates tile size at the edges **/
         xleft =  tx;
         ytop =  ty;
          
         // this is the start x,y of the tile
         
         int x_off = tx;
         int y_off = ty;
         
        
         xright = tx + tileWidth - 1;
         ybottom = ty + tileHeight - 1;
         
         tWidth = tileWidth;       
         tHeight = tileHeight;
        
         // adjust tileWidth for edge tiles
         int maxX = getMaxTileX();
         int maxY = getMaxTileY();
         
         if (debug) {
         	System.out.println("XX FITSRenderedImage.computeTile("+tileX+","+tileY+") x,y: "+tx+","+ty+" "+ tWidth+"x"+tHeight+"  ");          	
         	System.out.println("  XX tileGridXOffset = "+ tileGridXOffset+"   tileGridYOffset = "+tileGridYOffset );
         	// System.out.println("  XX (org) tx "+tx+" ty "+ty+"  xright "+ xright+" ybottom "+ybottom+"  width "+width+" height "+height);
         	System.out.printf("  XX  tx=%d ty=%d xleft=%d xright=%d   ytop=%d ybottom=%d \n", tx, ty, xleft, xright,ytop,ybottom);
         	System.out.printf("  XX  width=%d height=%d \n",width,height);
         	System.out.println("  tWidth "+tWidth+"  tHeight "+tHeight+" org "+org+" zorg "+zorg);
         	System.out.println("  maxX "+maxX+"  maxY "+maxY+" ");
         	System.out.println("  hdu "+hdu);    	
         }
         
         try {
 			bitpix = hdu.getBitPix();   
 			naxis = hdu.getAxes();
 			zero = hdu.getBZero();
 			if (debug) {
 				System.out.println("bitpix="+bitpix);
 				System.out.println("zero="+zero);
 				System.out.println("naxis="+naxis.length);

 				for (int j=0 ; j<naxis.length ; j++) {
 					System.out.println(" naxis["+j+"]="+naxis[j]);
 				}
 			}
 			
 		} catch (FitsException e) {
 			if (debug) {
 				System.out.println("###############################################################");
 				System.out.println("FitsException ");
 				e.printStackTrace();
 			}
 		}
         
         SampleModel tileSampleModel = createTileSampleModel(bitpix, naxis, tWidth, tHeight, zero);
         theTile = Raster.createWritableRaster(tileSampleModel, org);  
        //  theTile = Raster.createWritableRaster(tileSampleModel, zorg);       
         Rectangle tileRect = theTile.getBounds();              
         DataBuffer dbuf = theTile.getDataBuffer(); // I don't care what sub-type the data buffer is       
         
         int dbufDataType = dbuf.getDataType();
                 
         int bands = tileSampleModel.getNumBands();
         int tw = tileSampleModel.getWidth();
         int th = tileSampleModel.getHeight();
         int type = tileSampleModel.getDataType();
         int banks = dbuf.getNumBanks();
         int size = dbuf.getSize();
         int off[] = dbuf.getOffsets();
         int offlen = off.length;
         if (debug) {
        	 System.out.println(" dbufDataType "+dbufDataType+"  type "+type+" bitpix "+bitpix+" dbufSize "+size+"  offlen "+offlen+" banks "+banks);
        	 for (int i=0 ; i< offlen ; i++) {
        		 System.out.printf(" off[%d] = %d \n ",i,off[i]);
        	 }
         }
                 
     	PixelAccessor inPa = new PixelAccessor(tileSampleModel, null);
     	Rectangle inputArea = new Rectangle(0, 0, tWidth, tHeight);
     	// Rectangle inputArea = new Rectangle(x_off, y_off, tWidth, tHeight);
     	
     	
     	Raster outputRaster = Raster.createWritableRaster(tileSampleModel,dbuf,null);
     	if (debug) {
     		System.out.println("inputArea "+inputArea);
         	System.out.println("tileRect "+tileRect);
         	System.out.println("tileSampleModel "+tileSampleModel);   
         	System.out.println(" outputRaster "+outputRaster);
     	}
     	
     	UnpackedImageData data = inPa.getPixels(outputRaster, inputArea, dbuf.getDataType(), true);
        
     	if (hdu instanceof ImageHDU && debug) {
     		System.out.println("  hdu hdu instanceof ImageHDU "+hdu);
     	}
     	// Array pointers that are filled in from UnpackedImageData.getXxxData()
    	byte bdata[], bbdata[][];
    	short sdata[], ssdata[][];
    	int idata[], iidata[][];
    	float fdata[], ffdata[][];
    	double ddata[], dddata[][];
         // now read the data from the file into the data buffer??
    	// check if we have multiple bands
    	int destband = 0;
    	
    	ImageTiler tiler = ((ImageHDU) hdu).getTiler();
			// int [] offsets = {x_off, y_off}; // corners into the data
		int [] offsets = {y_off, x_off}; // corners into the data
			// int [] offsets = {256, 256}; // corners into the data			
		// int [] lengths = {tWidth, tHeight};  
		int [] lengths = {tHeight,tWidth};  
		if (debug) {
			System.out.printf("FITSRenderedImage.computeTile("+tileX+","+tileY+") offsets[%d,%d]   lengths[%d,%d] ", 
				offsets[0],offsets[1],lengths[0],lengths[1]);
		}
		
		for (destband=0; destband < bands; destband++) {
     	switch (dbufDataType) {
     		case  DataBuffer.TYPE_BYTE: // 0
     			bdata = (byte[]) data.getByteData(destband);
     			if (debug) {
     				System.out.println(" dbufDataType "+dbufDataType+"  DataBuffer.TYPE_BYTE "+DataBuffer.TYPE_BYTE+" destband "+destband);     			
     				System.out.println(" before  tiler.getTile fdata.length = "+bdata.length);
     			}
     			
     			try {
     				// sdata = (short[]) tiler.getTile(offsets, lengths);
     				tiler.getTile(bdata, offsets, lengths);
     				int len = bdata.length;
     				  
     				
     				// draw some lines to see how the tiling works
     				/***
     				int jj= 3;
     				for (int j = 0 ; j<= tileX ; j += 1, jj += 3)  {
     					
     					System.out.printf(" computeTile tileX=%d j=%d jj=%d\n", tileX, j, jj);   
     					for (int i=jj ; i< fdata.length ; i += tWidth) {
     						fdata[i] = 0;
     					}
 					} ***/
     			} catch (IOException e) {
     				// TODO Auto-generated catch block
     				e.printStackTrace();
     			}
     			break;
     		case  DataBuffer.TYPE_USHORT: // 1
     			break;
     		case  DataBuffer.TYPE_SHORT: // 2    			
     			sdata = (short[]) data.getShortData(destband);
     			if (debug) {
     				System.out.println(" dbufDataType "+dbufDataType+"  DataBuffer.TYPE_SHORT "+DataBuffer.TYPE_SHORT+" destband "+destband);    			
     				System.out.println(" before  tiler.getTile sdata.length = "+sdata.length);
     			}
     			     			
     			try {
     				// sdata = (short[]) tiler.getTile(offsets, lengths);
     				tiler.getTile(sdata, offsets, lengths);
     				
     			   // draw some lines to see how the tiling works
     				/**
     				int jj= 3;
     				for (int j = 0 ; j<= tileX ; j += 1, jj += 3)  {     					
     					System.out.printf(" computeTile tileX=%d j=%d jj=%d\n", tileX, j, jj);   
     					for (int i=jj ; i< sdata.length ; i += tWidth) {
     						sdata[i] = 0; // 32767
     					}
 					} **/
     				
     			} catch (IOException e) {
     				// TODO Auto-generated catch block
     				e.printStackTrace();
     			}
     			break;
     		case  DataBuffer.TYPE_INT: // 3
     			idata = (int[]) data.getIntData(destband);
     			if (debug) {
     				System.out.println(" dbufDataType "+dbufDataType+"  DataBuffer.TYPE_INT "+DataBuffer.TYPE_INT+" destband "+destband);    			
     				System.out.println(" before  tiler.getTile sdata.length = "+idata.length);
     			}
     			     			
     			try {
     				tiler.getTile(idata, offsets, lengths);
     				
     			   // draw some lines to see how the tiling works
     				/**
     				int jj= 3;
     				for (int j = 0 ; j<= tileX ; j += 1, jj += 3)  {     					
     					System.out.printf(" computeTile tileX=%d j=%d jj=%d\n", tileX, j, jj);   
     					for (int i=jj ; i< sdata.length ; i += tWidth) {
     						idata[i] = 0; // 32767
     					}
 					} **/
     				
     			} catch (IOException e) {
     				// TODO Auto-generated catch block
     				e.printStackTrace();
     			}
     			
     			break;
     		case  DataBuffer.TYPE_FLOAT: // 4
     			fdata = (float[]) data.getFloatData(destband);
     			if (debug) {
     				System.out.println(" dbufDataType "+dbufDataType+"  DataBuffer.TYPE_FLOAT "+DataBuffer.TYPE_FLOAT+" destband "+destband);     			
     				System.out.println(" before  tiler.getTile fdata.length = "+fdata.length);
     			}
     			
     			try {
     				// sdata = (short[]) tiler.getTile(offsets, lengths);
     				tiler.getTile(fdata, offsets, lengths);
     				int len = fdata.length;
     				  
     				
     				// draw some lines to see how the tiling works
     				/***
     				int jj= 3;
     				for (int j = 0 ; j<= tileX ; j += 1, jj += 3)  {
     					
     					System.out.printf(" computeTile tileX=%d j=%d jj=%d\n", tileX, j, jj);   
     					for (int i=jj ; i< fdata.length ; i += tWidth) {
     						fdata[i] = 0;
     					}
 					} ***/
     			} catch (IOException e) {
     				// TODO Auto-generated catch block
     				e.printStackTrace();
     			}
     			break;
     		case  DataBuffer.TYPE_DOUBLE: // 5
     			ddata = (double[]) data.getDoubleData(destband);
     			if (debug) {
     				System.out.println(" dbufDataType "+dbufDataType+"  DataBuffer.TYPE_DOUBLE "+DataBuffer.TYPE_DOUBLE+" destband "+destband);     			
     				System.out.println(" before  tiler.getTile fdata.length = "+ddata.length);
     			}
     			
     			try {
     				// sdata = (short[]) tiler.getTile(offsets, lengths);
     				tiler.getTile(ddata, offsets, lengths);
     				int len = ddata.length;
     				      				
     				// draw some lines to see how the tiling works
     				/***
     				int jj= 3;
     				for (int j = 0 ; j<= tileX ; j += 1, jj += 3)  {
     					
     					System.out.printf(" computeTile tileX=%d j=%d jj=%d\n", tileX, j, jj);   
     					for (int i=jj ; i< fdata.length ; i += tWidth) {
     						fdata[i] = 0;
     					}
 					} ***/
     			} catch (IOException e) {
     				// TODO Auto-generated catch block
     				e.printStackTrace();
     			}
     			break;
     		} // bands loop
		}
         
     	// System.out.println("FITSRenderedImage.computeTile returning theTile \n");
		return theTile;
	}
	
	@Override
	public WritableRaster copyData(WritableRaster dest) {
		Rectangle bounds;
		Raster tile;
		if (debug) System.out.println("FITSRenderedImage.copytData ");           
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
		if (debug) System.out.println("FITSRenderedImage.copyData bounds "+bounds.x+","+bounds.y+" "+bounds.width+"x"+bounds.height);
		return dest;
	 }
	
	/****
	 * getData
	 * Rectangle bounds
	 */
	 public Raster getData(Rectangle bounds) {
		 
		 	setTileWidth(bounds.width);
	        setTileHeight(bounds.height);
	        
	        int startX = XToTileX(bounds.x);
	        int startY = YToTileY(bounds.y);
	        int endX = XToTileX(bounds.x + bounds.width - 1);
	        int endY = YToTileY(bounds.y + bounds.height - 1);
	        Raster tile;
	        
	        // set the tileWidth, tileHeight based on the bounds
	        tileWidth = bounds.width;
	        tileHeight = bounds.height;
	        

	        if (debug) {
	        	System.out.println("\n=====================================================");
	        	System.out.println("============= getData ===========================");
	        	System.out.println("FITSRenderedImage.getData startX "+startX+" startY "+startY+"  endX "+endX+" endY "+endY);
	        	System.out.println("FITSRenderedImage.getData bounds "+bounds);
	        }
	        
	        if ((startX == endX) && (startY == endY)) {
	        	if (debug) {
	        		System.out.printf("FITSRenderedImage.getData getTile(%d,%d) ## 111111111 ##\n",startX, startY);
	        	}
	            tile = getTile(startX, startY);
	            if (debug) {
	            	System.out.println("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");
	            	System.out.println("FITSRenderedImage.getData after getTile("+startX+","+startY+") bounds "+bounds);
	            	System.out.println("FITSRenderedImage.getData tile "+tile);
	            }
	            /**
	            return tile.createChild(bounds.x, bounds.y,
	                                    bounds.width, bounds.height,
	                                    bounds.x, bounds.y, null);
	                                    **/
	            return tile;
	        } else {
	            // Create a WritableRaster of the desired size
	            SampleModel sm =
	                sampleModel.createCompatibleSampleModel(bounds.width,
	                                                       bounds.height);

	            // Translate it
	            WritableRaster dest =
	                Raster.createWritableRaster(sm, bounds.getLocation());

	            for (int j = startY; j <= endY; j++) {
	                for (int i = startX; i <= endX; i++) {
	                	if (debug) {
	                		System.out.printf("FITSRenderedImage.getData getTile(%d,%d) ## 2222222222 ##\n",i,j);
	                	}
	                    tile = getTile(i, j);
	                    Rectangle tileRect = tile.getBounds();
	                    Rectangle intersectRect = bounds.intersection(tile.getBounds());
	                    Raster liveRaster = tile.createChild(intersectRect.x,
	                                                         intersectRect.y,
	                                                         intersectRect.width,
	                                                         intersectRect.height,
	                                                         intersectRect.x,
	                                                         intersectRect.y,
	                                                         null);
	                    dest.setDataElements(0, 0, liveRaster);
	                }
	            }
	            return dest;
	        }
	    }
	// setters and getters
	
	public void setDebug(boolean d) {
    	debug = d;
    }
    
    public boolean getDebug() {
    	return debug ;
    }
    
    public void setTileWidth(int w) {
    	tileWidth = w;
    }
    
    public int getTileWidth() {
    	return tileWidth ;
    }
    
    public void setTileHeight(int h) {
    	tileHeight = h;
    }
    
    public int getTileHeight() {
    	return tileHeight ;
    }

    /**
     * Converts a pixel's X coordinate into a horizontal tile index.
     * This is a convenience method.  No attempt is made to detect
     * out-of-range coordinates.
     *
     * @param x the X coordinate of a pixel.
     * @return the X index of the tile containing the pixel.
     */
    public int XToTileX(int x) {
        return XToTileX(x, getTileGridXOffset(), getTileWidth());
    }

    /**
     * Converts a pixel's Y coordinate into a vertical tile index. 
     * This is a convenience method.  No attempt is made to detect
     * out-of-range coordinates.
     *
     * @param y the Y coordinate of a pixel.
     * @return the Y index of the tile containing the pixel.
     */
    public int YToTileY(int y) {
        return YToTileY(y, getTileGridYOffset(), getTileHeight());
    }

}
