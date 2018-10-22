/*
 * ImageStatistics.java
 * 
 * 
 * This is a place to hold all of the information
 * and gather the information
 * Input is a RenderedImage
 * Values are accessed from getters
 * Setters are used to configure what statistics should be gathered
 * Uses the jai-ext statistics classes to calculate the statistics
 */

package jpl.mipl.io;


import javax.imageio.*;
import javax.media.jai.ROI;
import javax.media.jai.ROIShape;
import javax.media.jai.RenderedOp;
import javax.media.jai.util.Range;

import org.w3c.dom.*;

import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.*;

import it.geosolutions.jaiext.*;

import it.geosolutions.jaiext.stats.*;
import it.geosolutions.jaiext.stats.Statistics.StatsType;
import it.geosolutions.jaiext.contrastenhancement.*;

// Range is in: jt-utilities-1.0.2.jar
// import it.geosolutions.jaiext.range.*;
// import it.geosolutions.rendered.viewer.ImageViewer;
// import it.geosolutions.rendered.viewer.RenderedImageBrowser;


public class ImageStatistics {
	
	boolean debug = false;
	boolean validValues = false;
	
	// vicar keywords: MISSING_CONSTANT INVALID_CONSTANT
	// flag to decide if it should be used ??
	// passed in??
	
	/* global variables */
	RenderedImage ri = null;
	String filename = "";

	int minx = 0; // Minimum X value of the image
	int miny = 0; // Minimum Y value of the image
	int width = 0; // Image Width
	int height = 0; // Image Height
	
	SampleModel sampleModel = null;
	
	int dataType = 0;
	int numBands = 1;
	// create a bands array based on the number of bands in the image
	int bands[] = new int[numBands];
	
	String dataTypeString = "unknown" ;
	
	int numXTiles = 0;
	int numYTiles = 0;
	int tileWidth = 0;
	int tileHeight = 0;
	int minTileX = 0;
	int minTileY = 0;
	
	int tileCt = 0;
	int pixelCt = 0;
	
	/** Array indicating the minimum bounds for each band */
    private double[] minBound ;

    /** Array indicating the maximum bounds for each band */
    private double[] maxBound ;
    
    double minBoundNull ;
    double maxBoundNull ;
    
    Double minValueD = 0.0;
    Double maxValueD = 255.0;
    
    // should all of these be arrays?
    // initValues will allocate arrays based on the number of bands in the image
    /**
    long sampleCt = 0;
    long meanSampleCt = 0 ;
    long medianSampleCt = 0 ;
	double mean = 0;
	double median = 0;
	double min = 0;
	double max = 0;
	double sum = 0;
	double std_dev = 0;
	**/
    long sampleCt[] ;
    long meanSampleCt[] ;
    long medianSampleCt[] ;
    // = new double[] { 0.0 };
	double mean[] ;
	double median[] ;
	double min[] ;
	double max[];
	double sum[] ;
	double std_dev[] ;
	
	// use this to show statistics have been gathered
	boolean valuesInited = false;
	boolean statisticsCollected = false;
	
	
	
	/* constructors */
	/*****************************	 
	 * ImageStatistics
	 * basic constructor
	 */
	public ImageStatistics() {
		
		// user must call 
		
		// loopStats(ri);	
		// which will internally call:
		// 		initValues(ri);
	}
	
	/**
	 * ImageStatistics
	 * @param renderedImage
	 * Creates the class and gathers the statistics
	 * User can then user getters to get all the values
	 */
	public ImageStatistics(RenderedImage renderedImage) {
		
		ri = renderedImage;
		initValues(ri);
		valuesInited = true;
		statisticsCollected = false;
		
		loopStats(ri);	
		statisticsCollected = true;
	}
	
	/**
	 * loopStats
	 * @param ri
	 * Gathers the statistics and saves them
	 * use getters to get the collected statistics
	 * ALL statistics are gathered at once
	 */
	public void loopStats(RenderedImage ri) {
		if (valuesInited == false) {
			initValues(ri);
			valuesInited = true;
			statisticsCollected = false; 
		  }
		
		numBands = sampleModel.getNumBands();
		for (int i = 0 ; i< numBands ; i++) {
			loopStats(ri, i) ;
		}
	}
	
	
	  /**
	   * loopStats
	   * This one only contains what we need  
	   * This class will contain all of values collected
	   * @param source
	   * 
	   * do we do stats on each band, only the first?
	   * make one value that represents all bands?? 
	   * sum is probably for all bands together
	   * add a band argument
	   */
	  public void loopStats(RenderedImage ri, int bandIndex) {
	    	
		    // check if values have been inited ??
	    	
	    	// s[x][y] = pixel value of the source.
	    	// globalStatObj = global statistic container.
	    	// statObj = temporary statistic container.
	    	// insideROI = boolean indicating that the value is inside ROI.
	    	// validData = boolean indicating that the value is not a No Data.
	    	// numTiles,srcHeight,srcWidth = source image tiles number, height, width.
			minx = ri.getMinX(); // Minimum X value of the image
			miny = ri.getMinY(); // Minimum Y value of the image
			width = ri.getWidth(); // Image Width
			height = ri.getHeight(); // Image Height
			
			
	    	
			
			// values used to loop through the tiles
			numXTiles = ri.getNumXTiles();
			numYTiles = ri.getNumYTiles();
			tileWidth = ri.getTileWidth();
			tileHeight = ri.getTileHeight();
			minTileX = ri.getMinTileX();
			minTileY = ri.getMinTileY();
			
			if (debug) {
				// see that the values have been zeroed out
				// if it is a multiband image values for the previous bands may have already been collected
				printImageValues();
			}
			

			/***
			 * What is correct for minBound and maxBound. Can it be null?? 
			 * If there is more than one band do accumulate across all bands or keep a statistic for each band?
			 * The most likely used case will be for a single banded EDR image
			 * The statistics are being collected on a per band basis.
			 * There will be a method to create a single value from the 3 bands.
			 * Everything except median should be fine.
			 ****/
			
			/*******
			 * Gather statistics for one band specified by bandIndex.
			 * The statistics gathered will be put into arrays. whose size is the number of bands
			 */
			
			
			Statistics globalMedianStatObj = StatsFactory.createMedianObject(minBound[0], maxBound[0]);
	    	Statistics globalMeanStatObj = StatsFactory.createMeanObject();   	
	    	Statistics globalMinStatObj = StatsFactory.createMinObject();
	    	Statistics globalMaxStatObj = StatsFactory.createMaxObject();
	    	Statistics globalSumStatObj = StatsFactory.createSumObject();
	    	Statistics globalDevStdStatObj = StatsFactory.createDevStdObject();
	    	
	    	
	    	
	    	// create a sum for all bands combined
	    	// check if this value is the same as adding together the sum for each band
	    	Statistics globalAllBandsSumStatObj = StatsFactory.createSumObject();
	    	
	    	
	    	Raster tile;
	    	boolean edgeTile = false;
	    	tileCt = 0;
	    	pixelCt = 0;
	    	// bandIndex is passed in
	    	int band = bandIndex; 
	    	for (int tx = minTileX ; tx < numXTiles ; tx++) {
	    		for (int ty = minTileY ; ty < numYTiles ; ty++) {
	    			// get this tiles raster and write it out to the file
	                tile = ri.getTile(tx, ty);
	                tileCt++;
	                int _tileWidth = tile.getWidth();
	                int _tileHeight = tile.getHeight();
	                Rectangle r = tile.getBounds();
	                if (debug) { 
	                	System.out.printf("  %d,%d tx=%d ty=%d  _tileWidth=%d _tileHeight=%d bounds=%s\n",
	                			tx, ty, tx, ty, _tileWidth, _tileHeight, r.toString());
	    			}
	                DataBuffer tileDb = null;
	                SampleModel tileSm = null;
	                int tileSmWidth = 0;
	            	int tileSmHeight = 0;
	            	int dbSize = 0;
	                if (tile != null) {
	                	tileSm = tile.getSampleModel();
	                	tileDb = tile.getDataBuffer(); 
	                	tileSmWidth = tileSm.getWidth();
	                	tileSmHeight = tileSm.getHeight();
	                	dbSize = tileDb.getSize();
	                	
	                	if (debug) { 
	                	System.out.printf("  %d,%d  _tileSmWidth=%d _tileSmHeight=%d  dbSize=%d tileDataType %d\n",
	            				tx, ty, tileSmWidth, tileSmHeight, dbSize, tileSm.getDataType());
	                	}
	                	// sm.getSamples(x, y, w, h, b, iArray, data)
	                }
	                // loop thru this tile and add the data to the statistics object
	                // based on datatype get a sample
	                // int pixels[] = null ;
	                // pixels = tile.getSamples(0,0,_tileWidth, _tileHeight, band, pixels);
	                // loop thru the samples and add to the stats object
	                // need to calculate edge tiles
	                int _minx = tx * _tileWidth;
	                int _miny = ty * _tileHeight;
	                int _maxx = (tx * _tileWidth) + _tileWidth;
	                int _maxy = (ty * _tileHeight) + _tileHeight;
	                
	                if (debug) { 
	                	System.out.printf("  %d,%d _minx=%d _miny=%d  _maxx=%d _maxy=%d  width=%d height=%d \n", 
	        				tx, ty, _minx, _miny, _maxx, _maxy,  width, height);
	                }
	                edgeTile = false;
	                int _tileWidth2 = _tileWidth;
	                int _tileHeight2 = _tileHeight;
	                if (_maxx > width) {
	                	_tileWidth2 -= (_maxx - width  );
	                	edgeTile = true;
	                }
	                if (_maxy > height) {
	                	_tileHeight2 -= (_maxy - height );
	                	edgeTile = true;
	                }
	                if (edgeTile) {
	                	// System.out.printf("  %d,%d _minx=%d _miny=%d  _maxx=%d _maxy=%d  width=%d height=%d ** edgeTile ******\n", 
	        			// 	tx, ty, _minx, _miny, _maxx, _maxy,  width, height);
	                	if (debug) { 
	                		System.out.printf("  %d,%d tx=%d ty=%d  _tileWidth2=%d _tileHeight2=%d ** edgeTile ****** \n",
	            				tx, ty, tx, ty, _tileWidth2, _tileHeight2);
	                	}
	                }
	                
	                // check file dataType
	                // These are the buffers used to get data from the tile
	                // The xxxxNull, versions are used as an input argument which forces tileSm.getSamples to internally allocate
	                // a correctly sized buffer for the returned data
	                
	             	int pixelsInt[] = null ;
	             	int pixelsIntNull[] = null ;
	             	
	             	float pixelsFloat[] = null;
	             	float pixelsFloatNull[] = null;
	             	double pixelsDouble[] = null;
	             	double pixelsDoubleNull[] = null;
	             	int z=0;
	     	
	             double unsigned_value = 0;
	             boolean validData = true;
	             long meanSampleCt2 = 0;
	             
	             // System.out.printf("numBands=%d dataType=%d dataTypeString=%s \n", numBands, dataType, dataTypeName);
	             // 5 is double
	             if (dataType == 5) { // double
	               	try {
	               		
	               		// System.out.printf("Float getSamples tileSm.getDataType()=%d %d\n", tileSm.getDataType(), dataType);
	               		
	              		pixelsDouble = tileSm.getSamples(0,0, tileSmWidth, tileSmHeight, band, pixelsDoubleNull, tileDb);             		
	              		
	              		int pixelsLength = pixelsDouble.length ;
	              		if (debug) { 
	              			System.out.printf("Double  %d,%d   pixelslength=%d dbSize=%d _tileWidth=%d _tileHeight=%d _tileWidth2=%d _tileHeight2=%d \n",
	             				tx, ty,pixelsLength, dbSize, _tileWidth, _tileHeight, _tileWidth2, _tileHeight2 );
	              		}
	                     
	             	    for(int y = 0; y<_tileHeight2 ;y++){
	             	        for(int x = 0; x<_tileWidth2;x++){
	             	        	pixelCt++;
	             	        	z = (y*_tileWidth) + x;
	             	           // pixelsDouble is a one dimensional array 
	             	        	
	             	        	// if we start doing this on a per band basis these will become
	             	        	// globalMedianStatObj[band]..addSample((double) pixelsDouble[z]);  
	             	        	// if nodata or range checking is enabled check that the value is useable before adding the sample
	             	        	
	             	        	globalMedianStatObj.addSample((double) pixelsDouble[z]);   
	                     	    globalMeanStatObj.addSample((double) pixelsDouble[z]);            	    
	                     	    globalMinStatObj.addSample((double) pixelsDouble[z]);
	                     	    globalMaxStatObj.addSample((double) pixelsDouble[z]);
	                     	    globalSumStatObj.addSample((double) pixelsDouble[z]);
	                     	    globalDevStdStatObj.addSample((double) pixelsDouble[z]);
	             	        }
	             	    }
	            	    
	              	} catch (Exception e) {
	              		
	              		System.out.println("\nError getSamples 0: Exception !"+e.getMessage());
	              		e.printStackTrace();
	              	}            	 
	              } else if (dataType == 4) { // float 
	              	try {
	              		
	              		// System.out.printf("Float getSamples tileSm.getDataType()=%d %d\n", tileSm.getDataType(), dataType);
	              		
	             		pixelsFloat = tileSm.getSamples(0,0, tileSmWidth, tileSmHeight, band, pixelsFloatNull, tileDb);             		
	             		
	             		int pixelsLength = pixelsFloat.length ;
	             		if (debug) { 
	             			System.out.printf("Float  %d,%d   pixelslength=%d dbSize=%d _tileWidth=%d _tileHeight=%d _tileWidth2=%d _tileHeight2=%d \n",
	            				tx, ty,pixelsLength, dbSize, _tileWidth, _tileHeight, _tileWidth2, _tileHeight2 );
	             		}
	                    
	            	    for(int y = 0; y<_tileHeight2 ;y++){
	            	        for(int x = 0; x<_tileWidth2;x++){
	            	        	pixelCt++;
	            	        	z = (y*_tileWidth) + x;
	            	           // pixelsFloat is a one dimensional array 
	            	        	
	            	        	globalMedianStatObj.addSample((double) pixelsFloat[z]);   
	                    	    globalMeanStatObj.addSample((double) pixelsFloat[z]);            	    
	                    	    globalMinStatObj.addSample((double) pixelsFloat[z]);
	                    	    globalMaxStatObj.addSample((double) pixelsFloat[z]);
	                    	    globalSumStatObj.addSample((double) pixelsFloat[z]);
	                    	    globalDevStdStatObj.addSample((double) pixelsFloat[z]);
	            	        }
	            	    }
	            	    
	             	} catch (Exception e) {
	             		
	             		System.out.println("\nError getSamples 0: Exception !"+e.getMessage());
	             		e.printStackTrace();
	             	}            	 
	             } else { // byte, half, ushort, int
	             // all of these return the data in an int array
	             	try {
	             		// System.out.printf("BYTE,SHORT,INT getSamples tileSm.getDataType()=%d \n", tileSm.getDataType());
	             		pixelsInt = tileSm.getSamples(0,0, tileSmWidth, tileSmHeight, band, pixelsIntNull, tileDb);             		
	             		
	             		int pixelsLength = pixelsInt.length ;
	             		if (debug) { 
	             			System.out.printf("Int  %d,%d   pixelslength=%d dbSize=%d _tileWidth=%d _tileHeight=%d _tileWidth2=%d _tileHeight2=%d \n",
	            				tx, ty,pixelsLength, dbSize, _tileWidth, _tileHeight, _tileWidth2, _tileHeight2 );
	             		}
	                    
	            	    for(int y = 0; y<_tileHeight2 ;y++){
	            	        for(int x = 0; x<_tileWidth2;x++){
	            	        	pixelCt++;
	            	        	z = (y*_tileWidth) + x;
	            	           // pixelsInt is a one dimensional array 
	           	        	
	            	        	globalMedianStatObj.addSample((double) pixelsInt[z]);   
	                    	    globalMeanStatObj.addSample((double) pixelsInt[z]);            	    
	                    	    globalMinStatObj.addSample((double) pixelsInt[z]);
	                    	    globalMaxStatObj.addSample((double) pixelsInt[z]);
	                    	    globalSumStatObj.addSample((double) pixelsInt[z]);
	                    	    globalDevStdStatObj.addSample((double) pixelsInt[z]);
	            	        }
	            	    }            	    
	            	    
	             	} catch (Exception e) {
	             		
	             		System.out.println("\nError getSamples 0: Exception !"+e.getMessage());
	             		// e.printStackTrace();
	             	}
	             } // dataType 0-3, byte half short int	
	             	
	             	
	    		}
	    	}	
	    	
	    	if (debug) {
	    		System.out.printf("Statistics gathered. numBands=%d dataType=%d dataTypeString=%s pixelCt %d\n", numBands, dataType, dataTypeString, pixelCt);
	    	}
	    	
	    	meanSampleCt[bandIndex] = globalMeanStatObj.getNumSamples() ;
	    	medianSampleCt[bandIndex] = globalMedianStatObj.getNumSamples() ;
	    	mean[bandIndex] = (Double)globalMeanStatObj.getResult();
	    	median[bandIndex] = (Double)globalMedianStatObj.getResult();
	    	min[bandIndex] = (Double)globalMinStatObj.getResult();
	    	max[bandIndex] = (Double)globalMaxStatObj.getResult();
	    	sum[bandIndex] = (Double)globalSumStatObj.getResult();
	    	std_dev[bandIndex] = (Double)globalDevStdStatObj.getResult();
	    	if (debug) {
	    		System.out.printf("global %d %s mean %f median %f min %f max %f sum %f std_dev %f meanSampleCt %d medianSampleCt %d tileCt %d pixelCt %d\n",  
	    			bandIndex, dataTypeString, mean[bandIndex], median[bandIndex], min[bandIndex], max[bandIndex], sum[bandIndex], std_dev[bandIndex],
	    			meanSampleCt[bandIndex], medianSampleCt[bandIndex], tileCt, pixelCt );
	    	}
	    	
	    
	    }
	  
	  /**
	   * initValues
	   * @param ri RenderedImage
	   * Create all the arrays to hold data values.
	   * Create an array so each band can have values collected for it.
	   * The normal use of this class is for single banded images.
	   * Multiband is allowed.
	   */
	  public void initValues(RenderedImage ri) {
		  
		  sampleModel = ri.getSampleModel();
			
			dataType = sampleModel.getDataType();
			numBands = sampleModel.getNumBands();
			// create a bands array based on the number of bands in the image
			bands = new int[numBands];
			for (int i=0 ; i< numBands ; i++) {
				bands[i] = i;
			}
						
			dataTypeString = getDataTypeName(dataType) ;
		 
			valuesInited = true;
			statisticsCollected = false;
			
			// long
			sampleCt = new long[numBands];
		    meanSampleCt = new long[numBands];
		    medianSampleCt = new long[numBands];
		    for (int i=0 ; i< numBands ; i++) {
			  	sampleCt[i] = 0;
			  	meanSampleCt[i] = 0;
			  	medianSampleCt[i] = 0;
			  	}
		    
		    // double
			mean = new double[numBands]; 
			median = new double[numBands]; 
			min = new double[numBands]; 
			max = new double[numBands]; 
			sum = new double[numBands]; 
			std_dev = new double[numBands]; 
			for (int i=0 ; i< numBands ; i++) {
				mean[i] = 0.0;
				median[i] = 0.0;
				min[i] = 0.0;
				max[i] = 0.0;
				sum[i] = 0.0;
				std_dev[i] = 0.0;
			}
			
			tileCt = 0;
			pixelCt = 0;	
			
			minValueD = 0.0;
			maxValueD = 255.0;
			
			minBound = new double[] { -3, -3, -3 };
	        maxBound = new double[] { 3, 3, 3 };

		minValueD = getMinForDataType(dataType);
		maxValueD = getMaxForDataType(dataType);
		/***/
		minBound = new double[numBands]; 
		for (int i=0 ; i< numBands ; i++) {
		  	minBound[i] = minValueD; 
		  	}
		 
		maxBound = new double[numBands]; 
		for (int i=0 ; i< numBands ; i++) {
		  maxBound[i] = maxValueD; 
		  }
		 
	  }
	  
	 /********* getters ******************************/
	// these are the statistics values 
	  
	public long[] getSampleCt() {
			return sampleCt;
		}	  
	public long getSampleCt(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return sampleCt[band];
		} 
		return 0;
	}
	
	public long[] getMeanSampleCt() {
			return meanSampleCt;
		}	  
	public long getMeanSampleCt(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return meanSampleCt[band];
		} 
		return 0;
	}
	
	public long[] getMedianSampleCt() {
		return medianSampleCt;
	}
	public long getMedianSampleCt(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return medianSampleCt[band];
		} 
		return 0;
	}
	
	// these are the statistics values 
	public double[] getMean() {
		return mean;
	}
	public double getMean(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return mean[band];
		} 
		return 0.0;
	}
	
	public double[] getMedian() {
		return median;
	}
	public double getMedian(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return median[band];
		} 
		return 0.0;
	}
	
	public double[] getMin() {
		return min;
	}
	public double getMin(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return min[band];
		} 
		return 0.0;
	}
	
	public double[] getMax() {
		return max;
	}
	public double getMax(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return max[band];
		} 
		return 0.0;
	}	
		
	public double[] getSum() {
		return sum;
	}
	public double getSum(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return sum[band];
		} 
		return 0.0;
	}	
	
	public double[] getStd_dev() {
		return std_dev;
	}
	public double getStd_dev(int band) {
		// check for band in range?
		if (band >= 0 && band < numBands) {
			return std_dev[band];
		} 
		return 0.0;
	}	
	
	public int getTileCt() {
		return tileCt;
	}
	
	public int getPixelCt() {
		return pixelCt;
	}
	
	public int getNumBands() {
		return numBands;
	}
	
	public String getDataTypeString() {
		return dataTypeString;
	}
	
	public int getDataType() {
		return dataType;
	}
	
	public boolean getValuesInited() {
		return valuesInited;
	}
	
	public boolean getStatisticsCollected() {
		return statisticsCollected;
	}
	
	public RenderedImage getRenderedImage() {
		return ri;
	}
	
  	
	/**
	 * printAllValues
	 * calls printImageValues();
	 *		printStatisticsValues();	
	 */
		public void printAllValues() {
			printImageValues();
			printStatisticsValues();		
		}
		
		/**
		 * printMinMax
		 * diagnostic print to understand all the values that may be used to set statistics calculations
		 */
		public void printMinMax() {
			System.out.printf("Byte %d  %d \n", Byte.MIN_VALUE, Byte.MAX_VALUE) ;
			System.out.printf("Byte %f  %f \n", (double)Byte.MIN_VALUE, (double)Byte.MAX_VALUE) ;
			System.out.printf("Byte %d  %d unsigned\n", 0, (Byte.MAX_VALUE *2)+1) ;
			
			System.out.printf("Byte %d  %d unsigned (abs)\n", 0, (Byte.MAX_VALUE + Math.abs(Byte.MIN_VALUE))) ;
			
			System.out.printf("Short %d  %d \n", Short.MIN_VALUE, Short.MAX_VALUE) ;
			System.out.printf("Short %f  %f \n", (double)Short.MIN_VALUE, (double)Short.MAX_VALUE) ;
			System.out.printf("Short %d  %d unsigned \n", 0, (Short.MAX_VALUE * 2) +1) ;
			System.out.printf("Short %d  %d unsigned (abs)\n", 0, (Short.MAX_VALUE + Math.abs(Short.MIN_VALUE))) ;
			System.out.printf("Integer %d  %d \n", Integer.MIN_VALUE, Integer.MAX_VALUE) ;
			System.out.printf("Integer %f  %f \n", (double)Integer.MIN_VALUE, (double)Integer.MAX_VALUE) ;
			
			// don't know why %f Float.MIN_VALUE displays as 0.0
			
			System.out.printf("Float %f  %f \n", -Float.MIN_VALUE, Float.MAX_VALUE) ;
			System.out.printf("Float %g  %g \n", -Float.MIN_VALUE, Float.MAX_VALUE) ;
						
			System.out.printf("Double %f  %f \n", -Double.MIN_VALUE, Double.MAX_VALUE) ;
			System.out.printf("Double %g  %g \n", -Double.MIN_VALUE, Double.MAX_VALUE) ;
   			
		
			System.out.printf("displaySizeMinAndMax \n");
			displaySizeMinAndMax(Byte.TYPE, Byte.SIZE, Byte.MIN_VALUE, Byte.MAX_VALUE);
			displaySizeMinAndMax(Short.TYPE, Short.SIZE, Short.MIN_VALUE, Short.MAX_VALUE);
			displaySizeMinAndMax(Character.TYPE, Character.SIZE, (int) Character.MIN_VALUE, (int) Character.MAX_VALUE);
			displaySizeMinAndMax(Integer.TYPE, Integer.SIZE, Integer.MIN_VALUE, Integer.MAX_VALUE);
			displaySizeMinAndMax(Long.TYPE, Long.SIZE, Long.MIN_VALUE, Long.MAX_VALUE);
			displaySizeMinAndMax(Float.TYPE, Float.SIZE, Float.MIN_VALUE, Float.MAX_VALUE);
			displaySizeMinAndMax(Float.TYPE, Float.SIZE, -Float.MIN_VALUE, Float.MAX_VALUE);
			
			
			displaySizeMinAndMax(Double.TYPE, Double.SIZE, Double.MIN_VALUE, Double.MAX_VALUE);
			displaySizeMinAndMax(Double.TYPE, Double.SIZE, -Double.MIN_VALUE, Double.MAX_VALUE);
		}

		/**
		 * displaySizeMinAndMax
		 * @param type
		 * @param size
		 * @param min
		 * @param max
		 */
		public static void displaySizeMinAndMax(Class<?> type, int size, Number min, Number max) {
			System.out.printf("type:%-6s size:%-2s min:%-20s max:%s\n", type, size, min, max);
		}
		
		/**
		 * printImageValues
		 * print all the values for this RenderedImaged
		 */
		public void printImageValues() {
			System.out.printf("Image Values numBands=%d dataType=%d dataTypeString=%s \n", numBands, dataType, dataTypeString);
			System.out.println("bands.length = "+bands.length+"  numBands = "+numBands);
			for (int i = 0; i < bands.length; i++) {
				System.out.println(i+")  "+bands[i]);		
			}
			System.out.printf("minValueD = %f maxValueD = %f \n", minValueD, maxValueD) ;
			System.out.printf("minx=%d miny=%d width=%d height=%d \n", minx, miny, width, height);
			System.out.printf("minTileX=%d minTileY=%d numXTiles=%d numYTiles=%d tileWidth=%d tileHeight=%d \n",
				minTileX, minTileY, numXTiles, numYTiles, tileWidth, tileHeight);
		}
		
		/**
		 * printStatisticsValues
		 */
		public void printStatisticsValues() {
			System.out.printf("Statistics \n numBands=%d dataType=%d dataTypeString=%s \n", numBands, dataType, dataTypeString);
			for (int i=0 ; i< minBound.length ; i++) {
				System.out.printf("band=%d minBound[%d] = %f  maxBound[%d] = %f \n",i, i, minBound[i], i, maxBound[i]);
			
			System.out.printf("mean[%d] %f\n median[%d] %f\n min[%d] %f\n max[%d] %f\n sum[%d] %f\n std_dev[%d] %f\n",  
	    			 i,mean[i], i,median[i], i,min[i], i,max[i], i,sum[i], i,std_dev[i] );
			System.out.printf("meanSampleCt[%d] %d, medianSampleCt[%d] %d, sampleCt[%d] %d\n",  
	    			i,meanSampleCt[i],i, medianSampleCt[i], i,sampleCt[i] );
			}
			System.out.printf(" tileCt %d, pixelCt %d\n",  tileCt, pixelCt );
					
		}
	
		/**
		 * getDataTypeName
		 * 
		 * @param dataType
		 * @return
		 * The int dataType is the value returned from a SampleModel
		 * Human readable type name is returned
		 */
		public String getDataTypeName(int dataType)  {
			   String s = "error";
			   switch (dataType) {
		       case DataBuffer.TYPE_BYTE:
		           s = "BYTE";
		           break;
		       case DataBuffer.TYPE_USHORT:
		           s = "UShort";
		           break;
		       case DataBuffer.TYPE_SHORT:
		           s = "Short";
		           break;
		       case DataBuffer.TYPE_INT:
		           s = "Int";
		           break;
		       case DataBuffer.TYPE_FLOAT:
		           s = "Float";
		           break;
		       case DataBuffer.TYPE_DOUBLE:
		           s = "Double";
		           break;
		       default:
		           
		       }
			   
			   return s;
		   }
	
		 /**
	     * for the median statistics object
	     * 
	     * @param dataType
	     * @return double. The maximum value for the dataType
	     */
	    public double getMaxForDataType(int dataType) {
	    	
	    	double max = 0.0;
	    	if (dataType == DataBuffer.TYPE_BYTE) {
	    		max = Byte.MAX_VALUE * 2;//used as unsigned
	    		max = 255.0;
		        }
		    else if (dataType == DataBuffer.TYPE_SHORT) {
		    	max = Short.MAX_VALUE;
		        }	    
		    else if (dataType == DataBuffer.TYPE_USHORT) {	    	
		    	max = Short.MAX_VALUE * 2;
		    	max = 65535.0;
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
	    * for the median statistics object
	    * 
	    * @param dataType
	    * @return double. The minimum value for the dataType
	    * 
	    * From the web:
	    * While the name is debatable as the "true minimum value" of a float is -Float.MAX_VALUE, 
	    * I suspect MIN_VALUE was chosen for consistency with the other numeric types. 
	    * Using the names MIN_RANGE_VALUE and MAX_RANGE_VALUE (or similar) might have made the difference more clear.
	    */
	   public double getMinForDataType(int dataType) {
	   	
	   	double min = 0.0;
	   	if (dataType == DataBuffer.TYPE_BYTE) {
	   			min = Byte.MIN_VALUE ;
	   			//used as unsigned
	   			min = 0.0;
		        }
		    else if (dataType == DataBuffer.TYPE_SHORT) {
		    	min = Short.MIN_VALUE;
		        }	    
		    else if (dataType == DataBuffer.TYPE_USHORT) {	    	
		    	min = Short.MIN_VALUE;
		    	// or 0.0 ?? // assume unsigned ???
		    	min = 0.0;
		        }
		    else if (dataType == DataBuffer.TYPE_INT) {
		    	min = Integer.MIN_VALUE; 
		    	// or 0.0 ?? // assume unsigned ???
		        }	    
		    else if (dataType == DataBuffer.TYPE_FLOAT) {
		    	min = -Float.MAX_VALUE; 
		        }
		    else if (dataType == DataBuffer.TYPE_DOUBLE) {
		    	min = -Double.MAX_VALUE; 
		        }
		    
		    return min;
		 }   

}

