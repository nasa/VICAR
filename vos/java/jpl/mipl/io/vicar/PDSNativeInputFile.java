/*
 * /
 * @(#)PDSNativeInputFile.java
 *
 * @author Steve Levoe NASA/JPL
 * 
 * 
 * 12-2003
 * This is the Native only version
 * This is a subclass of PDSInputfile which handles native access
 * using the OAL libraries. The methods which call or deal with Native methods
 * are here.
 * 9-02-03 added JNI interface to the PDS OAL (Object Access Library)
 * which is C. The goal is that OAL should handle more image types than 
 * the java version.
 * If the OAL library is NOT found on the system the java only version 
 * will work on the file. The PDSInputFile methods will be called.
 * This is a very preliminary version. It has been delivered as a placeholder
 * and because it is simpler and better to have it around to for proper building 
 * of PDSImageReader.
 *
 */
 
 
package jpl.mipl.io.vicar;

import jpl.mipl.io.vicar.PDSInputFile;

import java.io.*;
import java.awt.image.*;

import javax.imageio.stream.*; 

import javax.media.jai.*;

import java.awt.Rectangle;



			  
public class PDSNativeInputFile extends PDSInputFile {
	
//	native calls, these declarations allow the binding to the native libraries
public native void oal_setup(ImageInputStream inputStream); 

// public native void oal_setup(ImageInputStream inputStream, OaImageKeywords oaKey);

// this could also return a filled in OaImageKeywords 
public native  OaImageKeywords oal_setup(ImageInputStream inputStream, OaImageKeywords oaKey);

public native  OaImageKeywords oal_setup(String filename, OaImageKeywords oaKey);
			
// get individual label values ??
			// get all useful label values at once, fill some object
public native OaImageKeywords oal_getImageKeywords(OaImageKeywords oaKey);
//uses the OaImageKeywords supplied in oal_setup
public native OaImageKeywords oal_getImageKeywords(); 
			// createSystemLabel
public native String oal_getLabelString() ;

public native void oal_readTile(int x, int y, int w, int h, int x_off,
				int y_off, int bandList[], SampleModel sm, DataBuffer db) ;

// public native void oal_readByteBand(int w, int h, int band, byte[] arr) ;	
public native void oal_readByteBand(int w, int h, int band, byte[] arr, ImageInputStream iis) ;	
public native void oal_readShortBand(int w, int h, int band, short[] arr, ImageInputStream iis) ;		
// do one for Unsigned short ???	
 //   public native void oal_readUShortBand(int w, int h, int band, short[] arr) ;	
public native void oal_readIntBand(int w, int h, int band, int[] arr, ImageInputStream iis) ;
public native void oal_readFloatBand(int w, int h, int band, float[] arr, ImageInputStream iis) ;
public native void oal_readDoubleBand(int w, int h, int band, double[] arr, ImageInputStream iis) ;
				
public native void oal_close(); // release native resources			
			  /// ------------------------


// these varaiables are specific to the Native version, otherwise
// all the suoperclass variables are used
			  
//	on linux ?? /usr/local/lib/liboaljni.so.1
   String oalNativeLibName = "oaljni";
   // String oalNativeLibName = "hellop";
  
   boolean overide = false; // this flag allows goNative to be overridden 
   // will be used to forve JAVA-ONLY mode even if native libraries are present
	boolean goNative = false; // this will be set to true if the OAL C native library can
	// be loaded. The flag will then be used to determine if native imnterface calls 
	// should be used.
    
	OaImageKeywords oaImageKeywords = null;
   
// //////////////////////////////////////////////////////////////////////

 // all Constructors are the same as the SuperType
 // None are here
  
    /*
	 * accessor to check if the native libraries are in use
	 */
	public boolean getGoNative() {
		return goNative;
	} 
	
	public boolean getOveride() {
			return overide;
	} 
	
	public void setOveride(boolean over) {
				overide = over;
	} 	
		  
 /***********************************************************************
	  * Opens a file given a filename.
	  * What about URL's?
	  * @throws IOException
	  */
		 public synchronized void open(String fn) throws IOException
		 {
		 filename = fn;
		 if (debug) System.out.println("PDSInputFile.open("+fn+")");
		 loadOalLib(); // sets goNative flag
		
		 if (goNative) {
			 openInternal();
		 } else {
			 open(new RandomAccessFile(fn, "r"));
		 }
				
		 }

 /***********************************************************************
  * Does the actual work of opening the file.  Reads in the first part
  * of the label, and sets up the SystemLabel object.
  *
  * @throws IOException
  */
 /**
	 protected void openInternal() throws IOException
	 {
	 // Make sure we're at the beginning of the file/stream.
	 // Only does anything if random-access.
	 seekToLocation(0);

	 _lblsize_front = 0;
	 _lblsize_eol = 0;
 *********/

 /*
  * attempts to load the native library. If successfull then the goNative flag is 
  * set to true. Then the native libraries will be used.
  * Otherwise the java only code will be used.
  */
 public boolean loadOalLib() {
	
	 // HAVE AN OVERIDE PARAM ??
	 
	 goNative = false;
	 if (overide)
		 return goNative;
	
	 if (debug) {
		 String s2 = System.mapLibraryName(oalNativeLibName);
		 if (debug) {
			
			 System.out.println("loadOalLib "+ oalNativeLibName);
			 System.out.println("mapLibraryName "+ s2);
			 System.out.println("loadOalLib "+ oalNativeLibName);
			 }
		 } 
		
	 try {
		 System.loadLibrary(oalNativeLibName);
		
		 if (debug) System.out.println("loadOalLib "+ oalNativeLibName+ " load successful");
		 goNative = true;
	 }
	 catch (UnsatisfiedLinkError e) {
		 if (debug) {
		
			 System.out.println("UnsatisfiedLinkError: "+e);
			 System.out.println("loadOalLib "+ oalNativeLibName+
				 " could not be loaded, continuing in JAVA only mode");
		 }
		 goNative = false;
	 }
	 return goNative;
 }

 /*
  * 
  * openInternal() has been eliminated.
  * setupLabels() does the PDS specific part and is called from openInternal()
  * in VicarInputFile
  */
 
	 protected void setupLabels() {
	 
	
	 if (debug) {
		 System.out.println("PDSInputFile.setupLabels()"); 
		 System.out.println("input type: "+_input_stream); 
	 }
	
	  // if (filename != null && _input_stream instanceof ImageInputStream) {
	  if (filename != null && goNative) {
		 if (debug) System.out.println("going Native filname: "+filename);
					 // call thru to OaParseLabelFile pass in the ImageInputStream
					 // oal_setup(_inputStream); // then we can call native without _input_stream
					 // get a String from the native reader of the label
					 String label = " ";
					 oaImageKeywords = new OaImageKeywords(); 
					 //OaImageKeywords oal_setup(ImageInputStream inputStream, OaImageKeywords oaKey);
					 oaImageKeywords = oal_setup(filename, oaImageKeywords);
			
		 if (debug) System.out.println("oaImageKeywords "+oaImageKeywords);
			
			
					 // label = oal_getLabelString() ;
					 // probably better to get the label String from this side
					 // there is no way I can see so far to get the label buffer from OAL
					 // create a BufferedReader from the String
					 // BufferedReader labelBr = new BufferedReader(new StringReader(label));
					 //	PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM(labelBr, null);
					 // PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((ImageInputStream) _input_stream, null);
								 // System.out.println("after calling PDSLabelToDOM"); 
				   // 				_PDS_document = pdsLabel2Dom.getDocument();
					 // 			if (debug) System.out.println("_PDS_document: "+_PDS_document); 
			
					 _system = oaImageKeywords.createSystemLabel();
					 _isisSystem = (IsisSystemLabel) _system;
	        
					 _line_suffix_bytes = _isisSystem.getLineSuffixBytes();
					 _line_prefix_bytes = _isisSystem.getLinePrefixBytes(); 
	  }
	  else if (_input_stream instanceof ImageInputStream) {
     	
     	 
		
		 ImageInputStream iis = (ImageInputStream) _input_stream ;
    	
		 // we are only going to go native if the Reader is called with a filename.
		 // this code was for test of the native OAL using ImageInputStream
		 // the ImageInputStream version doesn't work well enough to use
		 boolean skipNative = true;
				
		 // if (goNative) {
		 if (skipNative != true) {
		
			 if (debug) System.out.println("going Native");
			 // call thru to OaParseLabelFile pass in the ImageInputStream
			 // oal_setup(_inputStream); // then we can call native without _input_stream
			 // get a String from the native reader of the label
			 String label = " ";
			 oaImageKeywords = new OaImageKeywords(); 
			 //OaImageKeywords oal_setup(ImageInputStream inputStream, OaImageKeywords oaKey);
			 oaImageKeywords = oal_setup((ImageInputStream) _input_stream, oaImageKeywords);
			
			 if (debug) System.out.println("oaImageKeywords "+oaImageKeywords);
			
			
			 // label = oal_getLabelString() ;
			 // probably better to get the label String from this side
			 // there is no way I can see so far to get the label buffer from OAL
			 // create a BufferedReader from the String
			 // BufferedReader labelBr = new BufferedReader(new StringReader(label));
			 //	PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM(labelBr, null);
			 // PDSLabelToDOM pdsLabel2Dom = new PDSLabelToDOM((ImageInputStream) _input_stream, null);
						 // System.out.println("after calling PDSLabelToDOM"); 
		   // 				_PDS_document = pdsLabel2Dom.getDocument();
			 // 			if (debug) System.out.println("_PDS_document: "+_PDS_document); 
			
			 _system = oaImageKeywords.createSystemLabel();
			 _isisSystem = (IsisSystemLabel) _system;
	        
			 _line_suffix_bytes = _isisSystem.getLineSuffixBytes();
			 _line_prefix_bytes = _isisSystem.getLinePrefixBytes(); 
					
		 }
	 else { 
	 	// PURE JAVA mode **********************************
		super.setupLabels();
	  }
		 
	 }
	 }

	 /***********************************************************************
		  * Does the actual work of opening the file.  Reads in the first part
		  * of the label, and sets up the SystemLabel object.
		  * @throws IOException
		  */
		protected void openInternal() throws IOException
			 {
			 // goNative flag is set in open()
			 if (goNative) {
				 // use the native OAL routines to set everything up
				 if (debug) System.out.println("PDSInputFile.openInternal() going native");
				 setupLabels();
			
				 _data_format = new VicarDataFormat(_system.getHost(),
								 _system.getIntFmt(), _system.getRealFmt());
				 // do all the other stuff below ???
				 // probably NOT needed since all the IO is done on the native side
				 _file_opened = true;
			 } 
			 else {
			 	super.openInternal();
			 }
		}

	
	
	
		/***********************************************************************
			 * High-level read function.
			 * Follow the See Also link for complete description
			 * @see VicarInput#readTile(int,int,SampleModel,DataBuffer)
			 */
				public void readTile(int x, int y, SampleModel m, DataBuffer b)
						throws IOException
				{
					if (debug) System.out.println("PDSInputFile.readTile");			 
						readTile(x, y, m.getWidth(), m.getHeight(), 0, 0, null, m, b);
			
				}

			/***********************************************************************
			 * High-level read function.
			 * Follow the See Also link for complete description
			 * @see VicarInput#readTile(int,int,int,int,int,int,SampleModel,DataBuffer)
			 */
				public void readTile(int x, int y, int w, int h, int x_off, int y_off,
						SampleModel m, DataBuffer b)
						throws IOException
				{
					if (debug) System.out.println("PDSInputFile.readTile");	
				readTile(x, y, w, h, x_off, y_off, null, m, b);
				}

			/***********************************************************************
			 * High-level read function.
			 * Follow the See Also link for complete description
			 * @see VicarInput#readTile(int,int,int,int,int,int,int[],SampleModel,DataBuffer)
			 */
				public synchronized void readTile(int x, int y, int w, int h,
						int x_off, int y_off,
						int bandList[],
						SampleModel sm, DataBuffer db)
						throws IOException
				{
					if (debug) System.out.println("PDSInputFile.readTile goNative "+goNative);	
					 if (goNative) {
							readTileNative(x,y,w,h,x_off,y_off,bandList,sm,db);
					 }
					 else {
						super.readTile(x,y,w,h,x_off,y_off,bandList,sm,db);
					 }
			 
			 
				}
				/***********************************************************************
				 * High-level read function.
				 * Follow the See Also link for complete description
				 * @see VicarInput#readTile(int,int,int,int,int,int,int[],SampleModel,DataBuffer)
				 */
					public synchronized void readTileNative(int x, int y, int w, int h,
							int x_off, int y_off,
							int bandList[],
							SampleModel sm, DataBuffer db)
							throws IOException
					{
					int i;
					int buffer_needed;
					int line, samp;
					int band, band_ind;
					int offset;
					boolean bandListGiven;
					// Array pointers that are filled in from UnpackedImageData.getXxxData()
					byte bdata[], bbdata[][];
					short sdata[], ssdata[][];
					int idata[], iidata[][];
					float fdata[], ffdata[][];
					double ddata[], dddata[][];

					int num_bands = sm.getNumBands();
					if (bandList != null && num_bands > bandList.length)
						num_bands = bandList.length;

					bandListGiven = (bandList != null);	// flag if it was given
					if (!bandListGiven) {			// make one for convenience
						bandList = new int[num_bands];
						for (i=0; i<num_bands; i++)
						bandList[i] = i;
					}

					if (debug) {
						System.out.println("PDSInputFile.readTileNative "+x+","+y+" "+w+"x"+h);
						System.out.println(" _system.getNS() "+_system.getNS()+"  _system.getNL() "+_system.getNL()+"  _system.getNB() "+_system.getNB());
						System.out.println(" _system.getN1() "+_system.getN1()+"  _system.getN2() "+_system.getN2()+"  _system.getN3() "+_system.getN3());
						System.out.println(" num_bands="+num_bands+" bandList ");
						for (i=0; i<num_bands; i++) {
							System.out.print("  "+i+"> " +bandList[i] );
						}
						System.out.println(" ----------------------------------------------------------------");
					}
					if (x + w > _system.getNS())		// last tile may be incomplete
						w = _system.getNS() - x;
					if (y + h > _system.getNL())
						h = _system.getNL() - y;

					if (debug) {
						System.out.print(" "+x+","+y+" "+w+"x"+h);
						System.out.println("  x_off "+x_off+"   y_off "+y_off);
					}

//				   Exception e = new Exception("VicarInputFile.readTile");
//				   e.printStackTrace();

					if (x_off + w > sm.getWidth())
						throw new ArrayIndexOutOfBoundsException(
						"Illegal width in VICAR readTile: " + w + ", x_off=" + x_off +
							", width=" + sm.getWidth());
					if (y_off + h > sm.getHeight())
						throw new ArrayIndexOutOfBoundsException(
						"Illegal height in VICAR readTile: " + h + ", y_off=" + y_off +
							", height=" + sm.getHeight());

					PixelAccessor pa = new PixelAccessor(sm, null);
					Rectangle area = new Rectangle(x_off, y_off, w, h);
					UnpackedImageData data = pa.getPixels(
							Raster.createWritableRaster(sm,db,null),
							area, db.getDataType(), true);

					int data_type = _system.getFormatCode();
					int org_code = _system.getOrgCode();
			
					// for Native read fro OAL images OAL always returns one band 
					// even if the file stores it in some other way
					// always ask for a band
					// assume we are getting a complete band
			
					// test to be sure?? assume for now
					ImageInputStream iis = null;
					if (filename == null) {
						iis = (ImageInputStream) _input_stream ;
					}
					/**
					ByteOrder byteOrder = iis.getByteOrder();
					System.out.println("iis.byteOrder = "+byteOrder);
					System.out.println("ByteOrder.BIG_ENDIAN = "+ ByteOrder.BIG_ENDIAN);
					System.out.println("ByteOrder.LITTLE_ENDIAN = "+ ByteOrder.LITTLE_ENDIAN);
					**/

					// ByteOrder.BIG_ENDIAN or ByteOrder.LITTLE_ENDIAN, indicating which byte order is being used.
			
					// IIS.setByteOrder(ByteOrder.BIG_ENDIAN);
					// iis.setByteOrder(ByteOrder.LITTLE_ENDIAN);

					System.out.println("org_code = "+org_code);
					switch (org_code) {

						case SystemLabel.ORG_BSQ:

						for (band_ind=0; band_ind < num_bands; band_ind++) {
							band = bandList[band_ind];

							switch (data_type) {
							case SystemLabel.TYPE_BYTE:
								bdata = data.getByteData(band);
								offset = data.getOffset(band);						
								// byte[] bdata
								// OAL bands start at 1
								System.out.println("    BYTE");
								oal_readByteBand(w, h, band_ind + 1, bdata, iis ) ;	
						
								break;
							case SystemLabel.TYPE_HALF:
								sdata = data.getShortData(band);
								offset = data.getOffset(band);
						
								// iis.setByteOrder(ByteOrder.LITTLE_ENDIAN);
								System.out.println("    HALF");
								oal_readShortBand(w, h, band_ind + 1,sdata, iis ) ;	
												
								break;
							case SystemLabel.TYPE_USHORT:
								sdata = data.getShortData(band);
								offset = data.getOffset(band);
								// oal_readUShortBand(w, h, band_ind + 1,sdata, iis ) ;	
								System.out.println("    USHORT");
								oal_readShortBand(w, h, band_ind + 1,sdata, iis ) ;	
						
								break;
							case SystemLabel.TYPE_FULL:
								idata = data.getIntData(band);
								offset = data.getOffset(band);
								oal_readIntBand(w, h, band_ind + 1,idata, iis ) ;	
						
								break;
							case SystemLabel.TYPE_REAL:
								fdata = data.getFloatData(band);
								offset = data.getOffset(band);
								oal_readFloatBand(w, h, band_ind + 1,fdata, iis ) ;	
						
								break;
							case SystemLabel.TYPE_DOUB:
								ddata = data.getDoubleData(band);
								offset = data.getOffset(band);
								oal_readDoubleBand(w, h, band_ind + 1,ddata, iis ) ;	
						
								break;
							case SystemLabel.TYPE_COMP:
								throw new UnsupportedOperationException("readTile() for Complex data not implemented yet!");
				/*!!!!
								// "band" is file_nb*2, where evens are real and
								// odds are imaginary.  "band_ind" is a simple
								// index into the output band array.
								for (line=0; line < h; line++) {
								readRecordCompNS(_float_buffer, x, w, 0, line+y,
										band/2);
								int c = band % 2;	// 0=real, 1=imag
								for (i=0; i < w; i++) {
									sm.setSample(x_off+i, y_off+line, band_ind,
									_float_buffer[i+c], db);
								}
								}
								break;
				!!!!*/
							}
						}		// end band loop
						break;

						case SystemLabel.ORG_BIL:
						   throw new UnsupportedOperationException("readTileNative(BIL) ONLY BSQ supported!");

				

						case SystemLabel.ORG_BIP:
						  throw new UnsupportedOperationException("readTileNative(BIP) ONLY BSQ supported!");

				
					}			// end switch org_code
					pa.setPixels(data, false);

				}	  
	 }

