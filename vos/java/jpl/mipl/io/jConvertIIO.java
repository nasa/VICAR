/*
fullRead* jConvertIIO.java
*
*  @version 1.0 11-15-2000
*
* @author Steve Levoe NASA/JPL
* 
* ImageIO image conversion utility
* 
****/
package jpl.mipl.io;

import java.io.*;
import java.util.*;
import java.lang.reflect.*;
import java.beans.*;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.color.ICC_ColorSpace;
import java.awt.image.renderable.ParameterBlock;
import java.awt.image.*;
import java.awt.Dimension;
import java.awt.Rectangle;

import com.sun.media.jai.codec.*;

import javax.media.jai.*;

import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Iterator;

import javax.imageio.IIOException;
import javax.imageio.ImageIO;
import javax.imageio.metadata.*;
import javax.imageio.*;
import javax.imageio.spi.*;
import javax.imageio.event.*;
import javax.imageio.stream.*;

import org.apache.commons.vfs2.FileSystemException;
import org.w3c.dom.*;

import jpl.mipl.io.vicar.*;
import jpl.mipl.io.plugins.*;
import jpl.mipl.io.util.*;
import jpl.mipl.util.*;
import jpl.mipl.jade.*;

// import java.lang.Package ;
import java.net.*;
import java.lang.Runtime;

/***
import org.apache.commons.vfs2.*;
import org.apache.commons.vfs2.operations.FileOperationProvider;
import org.apache.commons.vfs2.provider.*;
import org.apache.commons.vfs2.provider.webdav.*;
// import org.apache.commons.vfs2.provider.webdav.WebdavFileObject;
// import org.apache.commons.vfs2.provider.url.UrlFileObject ;
import org.apache.commons.vfs2.provider.url.* ;
import org.apache.commons.vfs2.provider.http.* ;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.vfs2.operations.*;
// import org.apache.http.client.HttpClient;
***/

/***
// jackrabbit support
import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.apache.jackrabbit.webdav.client.methods.PutMethod;
***/


/**
* Simple program using ImageIO to convert an image from one 
* image format to another.ImageIO plugins are used to do the reading and 
* writing. <br>
* ImageIO is available in JDK1.4 and above <br>
* ImageIO Transcoders can be used to translate metadata from one format to another.
* If no transcoder is available the image data only will be transfered from the 
* input to the output.<br>
* This is a simple testing program but should also be a useful utility.
* <p>
* As we get new plugins they can be added.Any new plugins added to the 
* classpath should automatically become available to this program.<br>
* The program has a number of arguments. <br>
* There are no required arguments. Capitalization is ignored except for filename
* which are case sensitive. Ouptut format names must match the format name.
* The available format names for readerrs and writers are printed to Standrd out 
* when the program is run.<br>
* program arguments: <br> 
* HELP will cause help info to be displayed <br>
* no arguments will also cause the help to be printed <br>
* INP=[inputFileName] <br>
* OUT=[outputFileName] name of the output file.<br>
* FORMAT=[imageFileFormatName] file format name to save the output to. 
* The extention of the output filename is not used in any way. <br>
* OFORM=[outputFormat] convert input data type to the OFORM data type on output <br>
* OFORM is an optional argument, data type will be preserved if no OFORM is specified <br>
* OFORM=DEFAULT will preserve data type <br>
* Valid OFORM= arguments are: BYTE, HALF, SHORT, USHORT, FULL, INT, 
* REAL, FLOAT, DOUB, DOUBLE, DEFAULT <br>
* all other arguments will cause an error <br>
* XSL=[Transcoder XSL file] xsl file to give to the transcoder. <br>
* DISPLAY=[true,false] if this Option is absent the image will NOT be displayed. <br>
* these Options are only valid for FORMAT=pds <br>
* EMBED_VICAR_LABEL=[true,false] , will cause VicarLabel to be embedded in PDS label <br>
* PDS_FILENAME_IN_LABEL=[true,false] only valid for FORMAT=pds, <br>
* will cause output file name to be embedded in the PDS label
* if this Option is absent the filename will NOT be added to the label. <br>
* 
* some debugging Options: <br>
* XML=[true,false] output xml files for debug <br>
* DEBUG=[true,false] add lots of extra debug printing <br>
* jar=fileToFind in the jar this class is contained in. This is a developer test <br>

*
*
*/
public class jConvertIIO 
{
IIOImage iioImage = null;
RenderedImage image = null;
private RenderedImage sourceImage = null;
private RenderedImage scaledImage = null;
private RenderedImage processedImage = null;
private RenderedImage rescaledImage = null; 
private RenderedImage filteredImage = null; 

String[] readerFormatNames = null;
String[] writerFormatNames = null;

JFrameJade jfImageS = null;
JFrameJade jfImageP = null;
JFrameJade jfImageF = null;

// we need these to locate the correct transcoder
ImageReader reader = null;
ImageWriter writer = null;

String forcedReaderFormat = null;

// String outputFormat = "vicar";
// String inputFormat = null; // using readerFormat instead
String outputFormat = null;

int inputDataType;
String inputDataTypeString = "DEFAULT";
String outputDataTypeString = "DEFAULT";
int outputDataType;

// use the input type (don't run the format op)
// these are the file names used as arguments. They MAY be full paths, \but could be relative
// as long as they can be opened we don't care
String inputFileName = "";
String outputFileName = "-";

int imageIndex = 0;

boolean flip_image = false;

// String inputFileNameFullPath = "";
// String outputFileNameFullPath = "";

// since there is only One transcoder this works
// when more transcoders are added we'll need an array
// or hash with one for each transcoder
// this only applies to JPL/MIPL transcoders since we create the jar
// String defaultJarXSLfilename = "VicarToPDS.xsl";
// this will cause the transcoder to get the default XSL script from the jar
String defaultJarXSLfilename = "-"; 

// store the Reader format name, and maybe the Reader class to help 
// find a transcoder
String readerFormat = "-";
String readerClassName = "-";

String transcoderName = "-";
String xslFileName = "-";

String findInJar = "-";
String pds4namespaceFile = "";

// boolean getAsRenderedImage =  false; // true;
boolean getAsRenderedImage =  true;

String RI = "true";
boolean jedi = false; // Myche test for JEDI image reader
boolean marsViewerJAI = false; // Test for MarsViewer use which uses JAI.create
boolean marsViewerFull = false; // Test for MarsViewer use which uses JAI.create

boolean silent = false; // eliminate all printing to stdout

// flag so we don't keep re-registering the codecs, just do it once per program run
static boolean codecsRegistered = false;
static boolean displayImage = false;
boolean outputXML = false;
boolean debug = false;
boolean printInfo = false; // print info about the image, do nothing else
boolean useOutputFilename = false;
boolean useWriteParam = false;
boolean embedVicarLabel = false;
boolean imageHasBeenProcessed = false; // flag to pass to write param if image's data has been processed
boolean imageHasBeenFiltered = false; // flag to pass to write param if image's data has been processed
// the OBJECT = IMAGE from the input header will be thrown away and regenerated

boolean addMerItems = false; // flag to have PDS output writer add MER IMAGE object items
boolean addStatistics = false; // flag to PDS output writer add Statistics (unless flag below is 
							// set dummy values are used)
boolean calculateStatistics = false; // flag to have output writer also calculate statistics used in
							// add MER IMAGE object items
							
boolean multiBand = false; // enables the band selection test code
boolean use_filename = false; // forces reader input to be a filename, normally it is an InputStream
							// to be used with Native readers which will open the file on their own
							// from the filename
boolean pdsDetachedLabel = false; // create a PDS detached label
boolean pdsDetachedOnly = false; // if pds_detached_label=true only create the label
boolean usePIRL = false; // if true use UofA PIRL libraries to parse PDS labels and write them out
boolean fakeImage = false; // rare case when the input file contains NO image data. We create a 1x1 image
// to prevent lots of headaches (null pointer exceptions) pass this thru so we don't  try to use it
// used with PDS detached labels 
    // will first check to see if the PIRL jar files are available on the system

String pds_ptr = null; // values passed to ther PDSImageWriter for PDS
// automatically rescale the data range when converting data to a new format (data type)
//but ONLY going to a smaller data type
boolean rescaleOnFormat = true;
double rescaleMin = 0.0;
double rescaleMax = 0.0;
// for ScaleDescriptor
boolean scaleImage = false;
float scaleXfactor = 1.0F;
float scaleYfactor = 1.0F;

float scaleXtrans = 0.0F;
float scaleYtrans = 0.0F;

/* Interpolation method 
Interpolation.INTERP_BILINEAR;
Interpolation.INTERP_CUBIC; 
Interpolation.INTERP_NEAREST;
Interpolation.INTERP_CUBIC2;
 */  
 
// CropDescriptor
boolean cropImage = false;
float cropX = 0.0F;
float cropY = 0.0F;
float cropWidth = 0.0F;
float cropHeight = 0.0F;

// ClampDescriptor
boolean clampImage = false;
double[] clampLow = {0.0};
double[] clampHigh = {255.0};

boolean bandSelect = false;
int bandList[] = {0,1,2};


/* new values to support ImageReadParams to subsample and crop an image in the reader
 * The above similar items use JAI operators to do the work */

//for SUBSAMPLING
boolean subsampleImage = false;

int subsamplingXoffset = 0;
int subsamplingYoffset = 0;
int sourceXsubsampling = 1;
int sourceYsubsampling = 1;
 

// SOURCE_REGION
boolean sourceRegion = false;
int sourceXoffset   = 0;
int sourceYoffset   = 0;
int sourceWidth  	= 0;
int sourceHeight 	= 0;

int tileSize = 256;
int tileSizeX = 256;
int tileSizeY = 256;
// int tileSizeX = 0;
// int tileSizeY = 0;

// tif write tiles
int tifTileSizeX = 0;
int tifTileSizeY = 0;
boolean useRawReader = false;
String rawReaderName = "raw";

boolean sourceRenderSize = false;
int srcRenderSizeWidth = 0;
int srcRenderSizeHeight = 0;


// READER_BANDS
boolean readerBandSelect = false;
int readerBandList[] = {0,1,2};


// PDS4 Table reader support
// could also use format or just ignore it if any table values are seen
// if we are reading a Table but creating a PDS4 label (not available yet)
// we could want to have format useable ??
boolean pds4_table_list = false;
boolean pds4_table_is_the_output = false; // set this if we see any other values
String pds4_table_fields = ""; // list of fields to use in the output "1,2,3,5" 
int pds4_table_index = 1; // index of the table to read 
// String pds4_table_index = "1";
String pds4_table_field_separator = " ";
// String table_platform = "platform"; // also "unix" "windows"
String pds4_table_output_format = "csv"; // "fixed-width" or "csv"

// output_filename we will use outputFileName from the out= argument
// also available String table_line_separator // options are "platform" "unix" "windows"
// String table_quote_character
// these values will be 

// set the marsviewer getImageInputStreamCallback
boolean useMarsviewerCallback = false;


/***************************************/

boolean singleToRGB = false;  // converts a single banded image to an 3 band RGB image
		// the jpg writer needs a 3 band image 
boolean printPlugins = false; // prints the list of available plugins

boolean keep_line_prefix = true; // attaches line prefix data to the metadata of the image
// sets a flag to the writer to write the data to the output file

// new for MSL 
String  pdsLabelType = "PDS3"; // other choices may be MSL_ODL,ODL3, new 12-13 PDS4
boolean addBinaryHeader = false; // controls if the vicar label of a PDS dualie gets the blob
// don't know if vicar always copies binary header or not - would be useful for reverse transcoding
boolean addBLOB = false; // controls if the PDS label gets the blob


// new for PDS4 
// command line option:
String velocityTemplateFilename = "";
//command line option:
String velocityConfigPath = "";


static final Runtime runtime = Runtime.getRuntime ();

/**
* jConvertIIO INP=500bx2.vic OUT=DISPLAY FORMAT=BYTE
**/
public jConvertIIO(String args[]) 
    {
    // System.out.println("jConvertIIO"); 
    
    // String[] args = new String[1];
    // args[1] = "NULL";
    
    // registerCodecs();
    // codecInfo();
    conv(args);   
    // should print args if silent == false
	if (silent == false) {
		String key, value;
		System.out.println("JConvertIIO");
		for (int i=0; i<args.length ;i++) {
			// System.out.println(i+") "+argv[i]);
			String arg = args[i];
			if (arg.indexOf("=") != -1) {
				key = getKey(arg, "=");
				value = getValue(arg, "=");
			}
			else { // allow just keyword and set value to "TRUE"
				key = arg;
				value = "true";
			}
	    System.out.println(i+") "+key+" = "+value+"");
		}
	}
    }
 
public jConvertIIO() 
{
// do nothing constructor
	
// user must then call convert.conv(args);   

}
 

    /***/
static public void main(String args[])
	{
	// prescann the command line
	String argFile = "";
	String key, value;
	
	boolean reuseConvert = false;
	boolean doMemTest = false;
	
	long mem1 = memoryUsed ();
	long mem2 = 0;
	
	if (doMemTest) {
		System.out.println("main mem = "+mem1);
	} 
	
	jConvertIIO convertOne = null ;
	
	convertOne = new jConvertIIO();
	
	
	for (int i=0; i<args.length ;i++) {
	    // System.out.println(i+") "+args[i]);
	    String arg = args[i];
	    if (arg.indexOf("=") != -1) {
	    	key = convertOne.getKey(arg, "=");
	    	value = convertOne.getValue(arg, "=");
	    }
	    else { // allow just keyword and set value to "TRUE"
	    	key = arg;
	    	value = "true";
	    }
	    	    
		// if (silent != false)
		// System.out.println(i+")> "+key+" = "+value);
			
	    if (key.equalsIgnoreCase("ARGFILE") || key.equalsIgnoreCase("ARG") ) {
	        argFile = value;
	    }
	}
	
	// have a flag for testing reuse the jConvertIIO or create a new one. force garbage collection ?
	
	if (argFile.equalsIgnoreCase("") ) {
		jConvertIIO convert = new jConvertIIO(args);
		
	} else {
		// open the argFile - loop thru
		
		System.out.println("Loop thru the arguments. reuseConvert = "+reuseConvert);
		
		File file = new File(argFile);
		StringBuffer contents = new StringBuffer();
		BufferedReader reader = null;
		
		try {
		reader = new BufferedReader(new FileReader(file));
		String line = null;
		int argFileLine = 1;
		// repeat until all lines is read
		while ((line = reader.readLine()) != null) {
			// contents.append(text).append(System.getProperty("line.separator"));
			// convert the line from the file into an args style array (should be able to split on spaces)
			// do we do new everytime or just call conv(theArgs) again. test to be sure
			// trim the line, checvk if it is empty, skip if it is
			String lineTrimmed = line.trim();
			if (lineTrimmed.length() == 0) {
				// skip this line
				continue;
			} else {
				String[] theArgs = lineTrimmed.split("\\s+");
				boolean status = false;
				jConvertIIO convert = null;
				if (reuseConvert) {
					status = convertOne.conv(theArgs);
				} else {
					// convert = null; // force garbage collect	on?
					convert = new jConvertIIO();
					status = convert.conv(theArgs);
				}
				// check status (might be a string) print the input command line on error and exit
				if (status == false) {
					// catch any exceptions from below? check to see if conv does
					System.out.println("line "+argFileLine+") error: command Failed");
					System.out.println(lineTrimmed);
					System.exit(1);
					// System.exit(0); // success return // non zero is an error (can have error codes for non-zero)
				}
				if ( doMemTest) {
					System.out.println(argFileLine+" completed one command line");
					System.out.println("  "+line);
				}
				
				
				// check memory footprint/GC each time thru the loop
				mem1 = memoryUsed ();
				/***/
				convert = null;
				
				runtime.runFinalization ();				 
	            runtime.gc ();
				Thread.currentThread ().yield ();
				/***/
				
				mem2 = memoryUsed ();
				float mem1f = (float) ((float)mem1) /(1024 * 1024) ;
				float deltaMemf = (float) ((float)(mem1 - mem2)) /(1024 * 1024) ;
				float mem2f = (float) ((float)mem2) /(1024 * 1024) ;
				
				if ( doMemTest) {
					System.out.println(argFileLine+" mem = "+mem1f+" mb   "+mem2f+" mb  "+deltaMemf+ " mb");
					System.out.println();
				}
				
				argFileLine++;
			}
			
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		} finally {
			try {
				if (reader != null) {
					reader.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		
	
		}
	} /****/

static long memoryUsed ()
{
    return runtime.totalMemory () - runtime.freeMemory ();
}

public void registerCodecs() {
     /** auto registration isn't working, try doing it by hand */
     // as of JDK1.4 autoregistration DOES work!!!!
        System.out.println(" IIORegistry.registerServiceProvider() vicarSpi ");
        // import javax.imageio.spi.ImageReaderSpi;
        // javax.imageio.spi.
        
       //  ImageIO.scanForPlugins();
        // add Transcoder too!!!!
         /****** all this code registers plugins by hand, not needed!!!!
        IIORegistry myIIORegistry = IIORegistry.getDefaultInstance();
        myIIORegistry.registerApplicationClasspathSpis();
        
       
        // Vicar reader and writer
        ImageReaderSpi vicarReaderSpi = new jpl.mipl.io.plugins.VicarImageReaderSpi();
        ImageWriterSpi vicarWriterSpi = new jpl.mipl.io.plugins.VicarImageWriterSpi();
        // ImageReaderSpi vicarSpi = new com.sun.media.imageio.plugins.vicar.VicarImageReaderSpi();
        // IIORegistry myIIORegistry = IIORegistry.getDefaultInstance();
        myIIORegistry.registerServiceProvider(vicarReaderSpi);
        myIIORegistry.registerServiceProvider(vicarWriterSpi);
        // IIORegistry.registerImageReaderSpi(vicarSpi);
        
        
        // PDS reader
        ImageReaderSpi pdsReaderSpi = new jpl.mipl.io.plugins.vicar.PDSImageReaderSpi();
        myIIORegistry.registerServiceProvider(pdsReaderSpi);
        
        ImageWriterSpi pdsWriterSpi = new jpl.mipl.io.plugins.vicar.PDSImageWriterSpi();
        myIIORegistry.registerServiceProvider(pdsWriterSpi);
        ***/
}

public void codecInfo() {
        // check to see what plugins are currently registered
        // ImageIO.scanForPlugins() ; // this should be done automatically
                  
        readerFormatNames = ImageIO.getReaderFormatNames();
        System.out.println("registered reader plugin format names "+readerFormatNames.length);
        for (int i=0 ; i< readerFormatNames.length ; i++) {
            System.out.println((i+1) +") "+readerFormatNames[i] );
        }
        
        writerFormatNames = ImageIO.getWriterFormatNames();
        System.out.println("registered writer plugin format names "+writerFormatNames.length);
        for (int i=0 ; i< writerFormatNames.length ; i++) {
            System.out.println((i+1) +") "+writerFormatNames[i] );
        }
        System.out.println("----------------------");
        
}

/* Used to parse the input arguments. Gets a key from a key=value string token
 * Assumes the input String is a single argument with delim as the delimiter 
 * between the key and the value. */
public String getKey(String s, String delim) {
    StringTokenizer st = new StringTokenizer(s, delim);
    String key, value;
    key = st.nextToken();
   // if (st.hasMoreTokens()) {
    value = st.nextToken();
    return key;
}

/* Used to parse the input arguments. Gets the value from a key=value string token
 * Assumes the input String is a single argument with delim as the delimiter 
 * between the key and the value.
 */
public String getValue(String s, String delim) {
    StringTokenizer st = new StringTokenizer(s, delim, true); // return delimiter
    String key;
    String value = "";
    String v = "";
    key = st.nextToken();
    int eqCt = 0;
    
    // change this to a value array ??
    // 		or let let item parse value with "," as the delimiter
    while (st.hasMoreTokens()) { 
    	v = st.nextToken();
    	// System.out.print("["+v+"]");
    	if (v.equals("=")){
    	   eqCt++; 
    	   if (eqCt > 1 ) {
    	   	// System.out.print("<"+eqCt+"-"+v+">");
    		  value += v;
    	   }
    	}
    	else {
    		// System.out.print("{"+eqCt+"-"+v+"}");
    		value += v;
    	}
    }
    // System.out.println(" #");
    return value;
}

/* test method to get a file from ta jar file in the classpath.
 * arg is the file name. If the file is found it is printed to Standard out. */
public void jarTest(String arg) {
    System.out.println("jarTest find: "+arg);
    // java.lang.ClassLoader
    // ClassLoader cl = new ClassLoader();
    // ClassLoader cl = this.getClass().getSystemClassLoader();
    ClassLoader cl = this.getClass().getClassLoader();
    ClassLoader scl = cl.getSystemClassLoader();

    
    System.out.println("cl: "+cl);
    System.out.println("scl: "+scl);
    
    URL url ;
    URI uri;
    // Enumeration enum = getSystemResources(arg);
    // for (int i=0 ; enum.nextElement()
    // System.out.println("u: "+u);
    
    
    try {
        for (Enumeration e = cl.getSystemResources(arg) ; e.hasMoreElements() ;) {
         
         url = (URL) e.nextElement();
        System.out.println("classLoader url "+url);
        InputStream is = url.openStream();
        // convert to something and read in the data
        System.out.println("is "+is);
        BufferedReader br = new BufferedReader(new InputStreamReader(is));
        String line ;
        int i=0;
        
       		do { 
       			line = br.readLine() ;
       			i++;
       			if (line != null) System.out.println(i+") "+line);     			
        	} while (line != null);
        
        }
        
        for (Enumeration e = scl.getSystemResources(arg) ; e.hasMoreElements() ;) {
         System.out.println("scl1 e "+e.nextElement());
        }
        
        /***
        System.out.println("scl Packages[]");
        Package[] pkg = scl.getPackages();


        for (int i = 0;i< pkg.length ; i++) {
         System.out.println("scl "+i +">"+pkg[i]);
        }
        ***/
        /**
        for (Enumeration e = cl.findResources(arg) ; e.hasMoreElements() ;) {
         System.out.println("cl2 e "+e.nextElement());
        }
        
        for (Enumeration e = scl.findResources(arg) ; e.hasMoreElements() ;) {
         System.out.println("scl2 e "+e.nextElement());
        }
        **/
    }
    catch (IOException e) {
        System.out.println("IOException jarTest "+e);
    }
    


    
    // URL u = getSystemResource(arg);
    // System.out.println("u: "+u);

    
}

/*
 * prints out a HELP message to user of the available 
 * command line arguments to this program */
public void printHelp() {
	
    System.out.println("HELP:");
    System.out.println("jConvert INP=[inputFileName] OUT=[outputFileName] FORMAT=[imageFileFormatName] OFORM=<outputFormat>");
    System.out.println("OFORM is the only optional argument, data type will be preserved if no OFORM is specified");
    System.out.println("or OFORM=DEFAULT");
    System.out.println("Valid OFORM= arguments are: BYTE, HALF, SHORT, USHORT, FULL, INT, ");
	System.out.println("REAL, FLOAT, DOUB, DOUBLE, DEFAULT");
	System.out.println("XSL=[Transcoder XSL file] xsl script to be used by the transcoder");
	System.out.println("  XSL=- or XSL will cause the default MER XSL script in the jar to be used");
	System.out.println("DISPLAY=[true,false] will cause the image to be displayed");
	System.out.println("PDS specific items");
	System.out.println("PDS_MER=[no,addMerItems] adds MER non-statistics items to the PDS IMAGE label object");
	System.out.println("  no or absent Keyword the values are not added to a PDS label");
	System.out.println("  addMerItems adds default values to a PDS label");
	System.out.println("PDS_STATS=[no,addStatistics,calcStatistics] adds statistics to the PDS IMAGE label object");
	System.out.println("  no or absent Keyword the values are not added to a PDS label");
	System.out.println("  addStatistics adds default values to a PDS label");
	System.out.println("  calcStatistics calculates real values for a PDS label (takes time)");
	System.out.println("    minimum, maximum, mean, median, standard_deviation, checksum");
	System.out.println("EMBED_VICAR_LABEL=[true,false] only valid for FORMAT=pds, ");	
	System.out.println(" Will cause a Vicar Label to be embedded in the PDS label.");
	System.out.println("PDS_FILENAME_IN_LABEL=[true,false] only valid for FORMAT=pds, ");
	System.out.println(" Will cause output file name to be embedded in the PDS label.");
	System.out.println("PDS_DETACHED_LABEL=[true,false] default is false, true will create a detached label");
	System.out.println("PDS_DETACHED_ONLY=[true,false] default is false, only valid if  PDS_DETACHED_LABEL=true");
	System.out.println(" Will cause the creation of a detached label only. The input image will NOT be");
	System.out.println(" rewritten. Only valid for vicar and ISIS input images.");
	System.out.println(" PDS_DETACHED_ONLY=true implies PDS_DETACHED_LABEL.");
	System.out.println("2RGB=[true,false] if an image is single banded the image will be converted to RGB");
	System.out.println("  required for an image to be output as jpg. Otherwise the image will be bogus"); 
	System.out.println("----------");
	
	System.out.println("RI=[true,false] defaults to false, forces the image to be read as a RenderedImage");
	System.out.println("  only applies to vicar,PDS,and ISIS images. The default return is a BufferedImage");
	System.out.println("HELP will cause this help info to be displayed");
	System.out.println("XML=[true,false] output xml files used for debugging");
	System.out.println("KEEP_LINE_PREFIX=[true,false] if line prefix data is present it will be read and passed");
	System.out.println(" along in the image metadata to a writer. Default is true. The prefix is not part of the");
	System.out.println(" displayed image. KEEP_LINE_PREFIX controls if it will be wrtten to an output file");
	System.out.println(" VICAR, PDS and ISIS can have line prefix data. Currently vicar and PDS support writing line prefixs");
	// ImageReadParam modifies how the data is read
	System.out.println("ImageReadParam arguments applied to the reader");
	System.out.println("READ_BANDS=1,2,3 will only read in the bads listed. bands start at 0 ");
	System.out.println("  READ_BANDS=[1,2,3]  READ_BANDS=(1,2,3) READ_BANDS=1,2,3 will all work.");
	System.out.println("  An image with selected bands may be written to PDS or Vicar, It will fail to tif or jpg");
	System.out.println("  Write to vicar or PDS then to jpg or tif.");
	System.out.println("SUBSAMPLING=(sourceXsubsampling,sourceYsubsampling,subsamplingXoffset,subsamplingYoffset)");
	System.out.println("     values are ints, no spaces allowed");
	System.out.println("SOURCE_REGION=(startX,startY,width,height) values are ints, no spaces allowed");
	System.out.println("RAW_READER_NAME=[raw,rW2] raw uses the jai_imageio.jar raw reader. raw2 uses rawImageReader.jar");
	// SOURCE_RENDERSIZE, USE_RAW_READER
	System.out.println("SOURCE_RENDERSIZE=(width,height) values are ints, no spaces allowed, NOT SUPPORTED by our readers.");
	// ImageWriteParam tile_size=(x,y)
	System.out.println("TIF_WRITE_TILE_SIZE=(X,Y) used in tif ImageWriteParam.setTiling() Sets then output tile size");
	
	// MARSVIEWER_CALLBACK useMarsviewerCallback
	
	// JAI operators after image data has been read
	System.out.println("JAI operators applied to Image data after it has been read");
	System.out.println("BANDS=1,2,3 will create an new image from the band list. bands start at 0 ");
	System.out.println("  BANDS=[1,2,3]  BANDS=(1,2,3) BANDS=1,2,3 will all work.");
	System.out.println("  An image with selected bands may be written to PDS or Vicar, It will fail to tif or jpg");
	System.out.println("  Write to vicar or PDS then to jpg or tif.");
	System.out.println("SCALE=(Xfactor,Yfactor,Xtranslation,Ytranslation) values are floats, no spaces allowed");
	System.out.println("CROP=(cropX,cropY,cropHeight,cropWidth) values are floats, no spaces allowed");
	System.out.println("CLAMP=(minValue,MaxValue) values are floats, no spaces allowed");
	System.out.println("RESCALE=[true/false] default is TRUE, only used if OFORM is to a new data type");
	System.out.println("  for all keywords which are [true/false] the keyword alone sets the the flag to TRUE");
	System.out.println("DEBUG=[true,false] add extra debug printing");
	System.out.println("USE_FILENAME=[true,false] forces a reader to be called with the filename as the input");
	System.out.println("  useful for testing any file readers (such as PDSNativeReader) that require a filname ");
	System.out.println("  as input. This is a developers option. Most readers will throw an Exception!!!");
	System.out.println("READER=[] force a reader to be used reader. Use the same format names as for FORMAT=");
	System.out.println("INFO=[true,false] prints information about the image, does no other processing (Not Yet)");
	System.out.println("jar=fileToFind this is a developers option, locates a file in the jar");
	System.out.println("MBAND=[true,false] some development code for image with > 3 bands (default is false)");
	System.out.println("argument names are NOT case sensitive, except filenames and FORMAT names");
	System.out.println("an argument without a value will default to \"true\" (only for [true,false] args");
	System.out.println("PLUGINS registered reader and writer names are printed ");
	System.out.println("SILENT eliminate all printing to stdout, could be useful when OUT=- which writes output");
	System.out.println(" file to stdout. Useful for piping to another process like ImageMagick \"convert\" ");
	System.out.println("IMAGE_INDEX=n sets the image index to read from the file. Defaults to 0.");	
	System.out.println("  Few files will contain more than one image. "); 
	
	System.out.println("********** new for MSL ********************");
	System.out.println("PDS_LABEL_TYPE=[MSL_ODL,ODL3,PDS3,PDS4] default is PDS3. Applys only to a PDS output file. Controls if ");
	System.out.println("  the label is ODL or PDS or PDS4 (XML).  ");
	System.out.println("ADD_BINARY_HEADER=[true,false] default is false. Useful only for PDS and vicar output formats"	);
	System.out.println("ADD_BLOB=[true,false] default is false. Controls output of a vicar binary header blob into a PDS header");
	System.out.println("    Useful only for PDS output formats"	);
	
	System.out.println("********** new for PDS4 ********************");
	System.out.println("VELO_TEMPLATE=[file path], sets the velocity template for use in creating a PDS4 detached label ");
	System.out.println("VELO_CONFIG=[dir path], sets the directory where velocity setup files for use in creating a ");
	System.out.println("  PDS4 detached label will be found. context-classes.xml, generated-mappings.xml, velocity-tools.xml");
	// Jar should be up to date. jpl/mipl/io/pds4_namespaces.txt
    System.out.println("PDS4_NAMESPACE_FILE=[filename], sets a text file with XML namespace values. prefix = namespace ");
    System.out.println("  used by the PDS4 velocity template input creation, The jar contains a default file: ");
    System.out.println("  jpl/mipl/io/pds4_namespaces.txt");
	System.out.println("***********************************************");
	/*** PIRL libraries are not a part of this delivery. Leaving it in case I deliver PIRL based stuff later
	* System.out.println("USE_PIRL=[true,false] 	default is false. True will request that the UofA PIRL PVL");
	* System.out.println(" libraries be used to parse and write PDS labels. Checks to see if the neccesary jar");
	* System.out.println(" files are available on the system. Otherwise the MIPL/vicar PDS label parser/writer");
	* System.out.println(" is used. The formatting of the PIRL label writer is different than the MIPL/vicar writer.");
	*****/
	System.out.println("**** PDS4 Table reader support ****************");
	System.out.println("PDS4_TABLE_LIST=[true,false], boolean flag, if true will list all products in the file. '0;");
	System.out.println("	Overides all other PDS4_TABLE_ flags");
	System.out.println("PDS4_TABLE_INDEX=n - index n starts at 1, default is 1. Specify the index of the table to access.");
	System.out.println("PDS4_TABLE_FIELDS=1,2,3  comma separated list of filed names or numbers. Default is all fields.");
	System.out.println("	Run once with PDS4_TABLE_LIST to get a list of the field names.");
	System.out.println("PDS4_TABLE_OUPUT_FORMAT=[csv,fixed-width]  default is fixed-width.");
	System.out.println("The only output available at this time for PDS4 Tables is as a text file. ");
	System.out.println("The output filename for PDS4 Tables is out filename (out=filename) ");
	System.out.println("CSV (Comma Separated Value) or fixed width.");
	System.out.println("All of the writers in the system only understand image data. They can't do anything with table data.");
	/*** PDS4 Table reader support
	String table_format = "fixed-width"; // "csv"
	// could also use format or just ignore it if any table values are seen
	// if we are reading a Table but creating a PDS4 label (not available yet)
	// we could want to have format useable ??
	boolean list_table = false;
	boolean table_is_the_output = false; // set this if we see any other values
	String table_fields = null; // list of fields to use in the output "1,2,3,5" 
	int table_index = 1; // index of the table to read 
	String table_field_separator = " ";

	// output_filename we will use outputFileName from the out= argument
	// also available String table_line_separator // options are "platform" "unix" "windows"
	// String table_quote_character
	// these values will be 
	 * ***/
	 

}

/*************************************************
* Does a simple ImageIO read, only gets the RenderedImage. <br>
* No Metadata is returned <br>
* @param String - the name of the file to read
**************************************************/
public RenderedImage simpleRead(String fileName) {
    RenderedImage image = null;
    
    try {
	  image = ImageIO.read(new File(fileName));
	  } catch (Exception exc) {
            System.out.println("\nError: " + fileName +
                               " - exception during read!");
            exc.printStackTrace();
            System.out.println();
      }
   return image;
}



        
        
/*************************************************
* Does a complete ImageIO read, gets the RenderedImage. <br>
* and Metadata which is returned in an IIOImage Object.
* @param String - the name of the file to read
* could be a filename or  URL
**************************************************/
// we will need a URL and stream version eventually
public IIOImage fullRead(String fileName) {
    RenderedImage image = null;
    BufferedImage bufferedImage = null;
    IIOImage iioImage = null;
    InputStream is = null;
    ImageInputStream iis = null;
    IIOMetadata im = null;
    IIOMetadata sm = null;
    int numImages = 0;
    WebdavVFS2file webdavFileIn = null;
    String filePath = ""; // everything except the actual filename
    String fileFullPath = ""; // everything including the actual filename
    String fileNameNoPath = ""; // only the actual filename
    String streamType = "";
    
    boolean save_debug = debug;
    // debug = true;
    
    if (debug) {
    	System.out.println("jConvertIIO open: >" + fileName+"<");
    	System.out.println("pdsDetachedOnly = "+ pdsDetachedOnly);
    }
    
    if (fileName.equals("")) {
    	System.out.println("jConvertIIO open: filename is empty:" + fileName);
    	return null;
    }
    
        try {
        	File f = new File(fileName);
            iis = ImageIO.createImageInputStream(f);
            // filePath = ""; // everything except the actual filename
            // fileNameNoPath = ""; // only the actual filename
            System.out.println("new File iis "+iis);
            if (iis != null) { 
          
            	System.out.println("new File "+fileName);
            	System.out.println("f.getAbsolutePath() "+f.getAbsolutePath());
            	System.out.println("f.getCanonicalPath() "+f.getCanonicalPath());
            	System.out.println("f.getName "+f.getName());
            	System.out.println("f.getPath "+f.getPath());
            	System.out.println("f.toString "+f.toString());
            	System.out.println("f.getParent "+f.getParent());
            	File parentFile = f.getParentFile();
            	System.out.println("parentFile "+parentFile);
            	if (parentFile != null) {
            		System.out.println("f.getParent "+f.getParent());
            		System.out.println("f.getParentFile().getAbsolutePath() "+f.getParentFile().getAbsolutePath());
            		System.out.println("f.getParentFile().getCanonicalPath() "+f.getParentFile().getCanonicalPath());
            	}
            	fileNameNoPath = f.getName();
            	fileFullPath = f.getCanonicalPath() ; // is there ever a difference between f.getAbsolutePath() and f.getCanonicalPath() ?
            	String seperator = java.io.File.separator;
            	System.out.println("seperator "+seperator);
            	filePath = fileFullPath.replace(seperator+fileNameNoPath, "");
            	streamType = "File";
            
            	System.out.println("streamType "+streamType);
            	System.out.println("fileNameNoPath "+fileNameNoPath);
            	System.out.println("fileFullPath   "+fileFullPath);
            	System.out.println("filePath       "+filePath);
            }

            
        } catch (IOException ioe) {
            System.out.println("I/O exception !");
            iis = null;
        }
        
            if (iis == null) {
            	// try getting as a URL    
            	// use URL class to check if it is really a URL?
            	// Caused by: java.io.FileNotFoundException: /etc/httpd/certs/client/client1.P12 (No such file or directory)
            	System.out.println("setProperty javax.net.ssl.trustStore");
                
               
                System.setProperty("javax.net.ssl.keyStoreType","pkcs12");
                // System.setProperty("javax.net.ssl.keyStore","/etc/httpd/certs/client/client1.P12");
                System.setProperty("javax.net.ssl.keyStore","/Volumes/bigraid/PDS4/FY2013/webdav/certs/miplapps3/client3.P12");
                System.setProperty("javax.net.ssl.keyStorePassword","miplapps");
                
                System.setProperty("javax.net.ssl.trustStoreType","jks");
                System.setProperty("javax.net.ssl.trustStore","/Volumes/bigraid/PDS4/FY2013/webdav/certs/miplapps3");
                System.setProperty("javax.net.ssl.trustStorePassword","miplapps");
                
            try {
            	URL url = new URL(fileName);
            	is = url.openStream();
            	iis = ImageIO.createImageInputStream(is);
            	streamType = "URL";
            	
            	System.out.println("new URL "+fileName);
                System.out.println("url.getFile "+url.getFile());
                System.out.println("url.toString "+url.toString());
                System.out.println("url.getPath "+url.getPath());
                
                                				
                System.out.println("url.getRef "+url.getRef());
                
                
                fileFullPath = url.toString(); // url.getFile() is the file path without the http*** 
                // split the path on the last "/"
                int slashIndex = fileFullPath.lastIndexOf("/");
                System.out.println("slashIndex "+slashIndex);
                if (slashIndex == -1) {
                	// should never get here
                	filePath = ".";
                	fileNameNoPath = fileFullPath;
                } else {
                	filePath = fileFullPath.substring(0,slashIndex);
                	fileNameNoPath = fileFullPath.substring(slashIndex+1);
                	
                }
                
                System.out.println("streamType "+streamType);                
                System.out.println("fileNameNoPath "+fileNameNoPath);
                System.out.println("fileFullPath   "+fileFullPath);
                System.out.println("filePath       "+filePath);

            } catch (Exception e) {
                System.out.println("URL exception !");
                iis = null;
            }
            	
            	
            	
            	if (iis == null) {
                    System.out.println("Unable to get a stream! Trying webdav");
                    // try one more time using VFS2                
                                   
                    webdavFileIn = new WebdavVFS2file(fileName, false);
                	
                	try {
                		is = webdavFileIn.getFileContent().getInputStream();
                		webdavFileIn.printInfo();
                		// get the filePath and fileName 
                		// check for is == null ??
                		iis = ImageIO.createImageInputStream(is);
                		streamType = "webdav";
                		if (debug) System.out.println("fullRead open input as a webdav "+fileName);
                		if (iis == null) {
                            System.out.println("Unable to get a stream as webdav! Exit");
                            System.exit(1);// 1 is error return
                		}
                	} catch (IOException ioe) {
                        System.out.println("webdav I/O exception ! Exit");
                        iis = null;
                        System.exit(1);// 1 is error return
                	} catch (NullPointerException ioe) {
                        System.out.println("webdav Null Pointer exception ! Exit");
                        iis = null;
                        System.exit(1);// 1 is error return
                	} 
                	/***catch (FileSystemException e) {
                		// TODO Auto-generated catch block
                		e.printStackTrace();
                		System.out.println("Unable to get a stream! Exit");
                		System.exit(1);// 1 is error return
                	}
                	***/
                    
            	}
            }
            
          try {
			if (forcedReaderFormat != null && !forcedReaderFormat.equalsIgnoreCase("true")) {
				if (debug) System.out.println("fullRead forcedReaderFormat = "+forcedReaderFormat);
				Iterator iter = ImageIO.getImageReadersByFormatName(forcedReaderFormat);
				reader = (ImageReader)iter.next();
				
				readerClassName = reader.getClass().getName() ;
				// get the format we are reading
				readerFormat = reader.getFormatName();
				if (debug) {
					System.out.println("Forced to Use " +readerClassName+"  "+readerFormat);
				}
			}
			else {
			
				if (debug) {
					System.out.println("jConvertIIO iis " + iis);
				}
            	Iterator iter = ImageIO.getImageReaders(iis);
            
            	while (iter.hasNext()) {
                	reader = (ImageReader)iter.next();               	
                	if (debug) System.out.println("Using " +
                               reader.getClass().getName() +
                               " to read.");
                	readerClassName = reader.getClass().getName() ;
                	// get the format we are reading
                	readerFormat = reader.getFormatName();
                	break;
            	}
        	}

            if (reader == null) {
                System.err.println("Unable to find a reader!");
                System.exit(1); // 1 is error return
            }
            
        
        	// if (readerFormat.equalsIgnoreCase("pds")) {
			if (use_filename == true) {			
        		iis.close();
				if (debug) System.out.println("Calling "+readerFormat +" reader with a filename "+fileName);
				reader.setInput(fileName, true);
				if (debug) System.out.println("AFTER Calling PDS reader with a filename ");
        	}
        	else {
				reader.setInput(iis, true);
        	}
            
			if (debug) {
				System.out.println("fullRead() readerFormat = "+readerFormat);
			}
        
            numImages = 1;
            // numImages = reader.getNumImages(true);
            if (debug) {
            	System.out.println("\nThe file contains " + numImages + " image"
                           + (numImages == 1 ? "" : "s") + ".");
            	System.out.println();
            }

            sm = reader.getStreamMetadata();
        
        } catch (IOException ioe) {
            System.out.println("I/O exception");
            System.exit(1); // 1 is error return
        }
        
        if (debug) {
        	if (sm == null) {
            	System.out.println("The file contains no stream metadata.");
        	} else {
            	System.out.println("has Stream metadata");
            	// String nativeFormatName = sm.getNativeMetadataFormatName();
            	// displayMetadata(sm.getAsTree(nativeFormatName));
        	}
        }

		// flag to decide if we get as BufferedImage or RenderedImage
		RenderedImage renderedImage = null;
		
		if ((readerFormat.equalsIgnoreCase("vicar") || readerFormat.equalsIgnoreCase("pds") ||
			readerFormat.equalsIgnoreCase("pds4") || readerFormat.equalsIgnoreCase("isis")) 
			&& !RI.equalsIgnoreCase("FALSE")) { 
			getAsRenderedImage = true;
		}
		else {
			getAsRenderedImage = false;
		}
		
		
		if (getAsRenderedImage) {
			if (debug) {
				System.out.println("get as RenderedImage ** "+readerFormat+"  "+fileName );
				System.out.println("tileSize       "+tileSize);
				System.out.println("tileSizeX       "+tileSizeX);
				System.out.println("tileSizeY       "+tileSizeY);
				System.out.println("streamType "+streamType);
	            System.out.println("fileNameNoPath "+fileNameNoPath);
	            System.out.println("fileFullPath   "+fileFullPath);
	            System.out.println("filePath       "+filePath);
	            System.out.println("reader.getClass().getName()       "+reader.getClass().getName());
	            System.out.println("iis.getClass().getName() "+iis.getClass().getName());
	            System.out.println("iis.getClass().getSimpleName() "+iis.getClass().getSimpleName());
	            // add streamType, fielNameNoPath, fileFullPath, readerFormat to PDSImageReadParam ??
	            // use reader className
	           
			}
			// get the filename and file path. could be a file path or a URL/URI
			try {
				ImageReadParam param = reader.getDefaultReadParam();
				// add something to it
				if (param instanceof PDSImageReadParam) {
					((PDSImageReadParam) param).setDirectoryPath(filePath);
				}
				if (readerFormat.equalsIgnoreCase("pds") && pdsDetachedOnly == true) {
					renderedImage = reader.readAsRenderedImage(0, param);					
				} else { 
					renderedImage = reader.readAsRenderedImage(0, param);
				}
				/***
     			if (debug) {
     				System.out.println("renderedImage.getHeight() "+renderedImage.getHeight() );
     				System.out.println("renderedImage.getWidth() "+renderedImage.getWidth() );
     			}
     			****/
     			if (renderedImage instanceof VicarRenderedImage ) {
     				VicarRenderedImage vri = (VicarRenderedImage) renderedImage;
     				// vri.setTileWidth(vri.getWidth());
     				// if (tileSizeX > 0 && tileSizeY > 0) {
     				if (tileSizeY > 0) {
     					vri.setTileWidth(tileSizeX);
     					vri.setTileHeight(tileSizeY);
     					vri.getNewSampleModel();
     				}
     			}
            } catch (IOException ioe) {
                System.out.println("I/O exception !");
                System.exit(1); // 1 is error return
            }
			
		}
		else {

		 try {
     			bufferedImage = reader.read(0);
     			
     			if (debug) {
     				System.out.println("jConvertIIO.fullRead() reader.read(0); bufferedImage");
     				System.out.println("readerFormat "+readerFormat);
     				System.out.println("fakeImage "+fakeImage);
     			}
     			
            } catch (IOException ioe) {
                System.out.println("I/O exception !");
                System.exit(1); // 1 is error return
            }
		}
		
		if (readerFormat.equalsIgnoreCase("pds") ) {
			fakeImage = ((PDSImageReader) reader).getFakeImageNoRead();
			System.out.println("******************* pds reader *********************************");
			System.out.println("readerFormat "+readerFormat+"   fakeImage "+fakeImage);
			System.out.println("*********************************************************");
		}

		// this forces the reader to read in and store the metadata
            try {
                im = reader.getImageMetadata(0);
            } catch (IOException ioe) {
                System.out.println("I/O exception obtaining Image Metadata!");
                System.out.println("Proceeding conversion without Metadata");
                // System.exit(0);
            }
            
            if (debug) {
            	if (im == null) {
                	System.out.println("\nThe file has no Image metadata.");
            	} else {
                	System.out.println("\nThe file contains Image metadata.");
                	System.out.println("im "+im);
            	}
            }
            
            if (bufferedImage != null) {
            	if (debug) {             	
            		printImageInfo(bufferedImage, "fullRead.bufferedImage");
            	}
            
            	// 2nd argument is a List of thumbnails     
  	 			iioImage  = new IIOImage(bufferedImage, null, im);
            }
            else if (renderedImage != null) {
            	if (debug) {
            		printImageInfo(renderedImage, "fullRead.renderedImage");
            		System.out.println("create IIOImage from renderedImage and im");
            		String nativeFormatName = im.getNativeMetadataFormatName();
           		    System.out.println("PDSMetadata  "+im);
                    Document doc =  (Document) im.getAsTree(nativeFormatName);
                    System.out.println("nativeFormatName "+nativeFormatName+",  document "+doc);
                    final Node root = doc.getDocumentElement();
                    System.out.println("root "+root);
                    System.out.println("new IIOImage(renderedImage, null, im)  ***************************************");
            	}
            
            	// 2nd argument is a List of thumbnails     
  	 			iioImage  = new IIOImage(renderedImage, null, im);
  	 			
  	 			// try getting metadata from this new IOImage
  	 			
  	 			if (debug) {
  	 				IIOMetadata iom = iioImage.getMetadata() ;
            		
            		System.out.println("created new IIOImage from renderedImage and im. get metadata again");
            		String nativeFormatName = iom.getNativeMetadataFormatName();
           		    System.out.println("PDSMetadata  "+iom);
                    Document doc =  (Document) iom.getAsTree(nativeFormatName);
                    System.out.println("nativeFormatName "+nativeFormatName+",  document "+doc);
                    final Node root = doc.getDocumentElement();
                    System.out.println("root "+root);
            	}
            }
            debug = save_debug ;    
   			return iioImage;
/***
        for (int i = 0; i < numImages; i++) {
            System.out.println("\n---------- Image #" + i + " ----------");
            System.out.println();

            try {
                int width = reader.getWidth(i);
                System.out.println("width = " + width);

                int height = reader.getHeight(i);
                System.out.println("height = " + height);

                int numThumbnails = reader.getNumThumbnails(i);
                System.out.println("numThumbnails = " + numThumbnails);

                for (int j = 0; i < numThumbnails; j++) {
                    System.out.println("  width = " +
                                   reader.getThumbnailWidth(i, j) + 
                                   ", height = " +
                                   reader.getThumbnailHeight(i, j));
                }

                // File ff = new File(f);
                // BufferedImage bufferedImage = ImageIO.read(f);
                bufferedImage = reader.read(0);
            } catch (IOException ioe) {
                System.out.println("I/O exception !");
                System.exit(0);
            }
            
            
            if (bufferedImage == null) {
                System.out.println(inputFileName + " - couldn't read!");
                // return;
            }

           
            System.out.println("\n ImageToDOM");
            ImageToDOM i2dom = new ImageToDOM ( bufferedImage );
            Document d = i2dom.getDocument();
            displayMetadata((Node) d);
            makeFrameDomEcho4((Node) d, bufferedImage );
            
            System.out.println("<**************************************>");
            
            System.out.println("\n ImageToPDS_DOM");
            ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( bufferedImage );
            Document d1 = i2PDSdom.getDocument();
            displayMetadata((Node) d1);
            makeFrameDomEcho4((Node) d1, bufferedImage );
            
            System.out.println("<**************************************>");
           
            
            // this forces the reader to read in and store the metadata
            try {
                im = reader.getImageMetadata(i);
            } catch (IOException ioe) {
                System.out.println("I/O exception obtaining Image Metadata!");
                // System.exit(0);
            }
            
            if (im == null) {
                System.out.println("\nThe file has no Image metadata.");
            } else {
                System.out.println("\nThe file contains Image metadata.");
            }
          
            else {
                System.out.println("\nImage metadata:");
                String nativeFormatName = im.getNativeMetadataFormatName();
                
                Node imNode = im.getAsTree(nativeFormatName);
                // this could be loaded into the DomEcho4
                
                System.out.println("ImageDumper.displayMetadata() >>>>>>");
                displayMetadata(imNode);
                System.out.println("<<<<<< ImageDumper.displayMetadata()");
                // makeFrameDomEcho4(d);
                // makeFrameDomEcho4(imNode);
                IIOImage iioImage = new IIOImage(bufferedImage, null, im);
                System.out.println("<**************************************>");
                String fname = f.getName();
                System.out.println(" ");
                // pass image AND stream metadata to the DomEcho
                // we pass it the image so it has the image to wriiten out to
                // a file
                makeFrameDomEcho4(iioImage, sm, fname);
                // makeFrameDomEcho4(imNode, bufferedImage);
                
            }
            
        }
        
        // 2nd argument is a List of thumbnails     
   iioImage  = new IIOImage(bufferedImage, null, im);
   return iioImage;
        ***/
        
   
}

/*
 * prints useful debug information on a RenderedImage. The description is a String 
 * included in the print. */
public void printImageInfo(RenderedImage im, String description) {
	
	if (im == null) return ;
	
	SampleModel sm = im.getSampleModel();
    ColorModel cm = im.getColorModel();
    int  width = im.getWidth();
    int height = im.getHeight();
   
    int dataType = sm.getDataType();
        
    
    System.out.println("RenderedImage "+description+"  -------------------");
    int bands = sm.getNumBands();
    int[] sampleSize = sm.getSampleSize(); // sampleSize[0] equals b0size
    int b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
    
    	System.out.println("DataBuffer.TYPE_BYTE = "+DataBuffer.TYPE_BYTE);
    	System.out.println("DataBuffer.TYPE_SHORT = "+DataBuffer.TYPE_SHORT);
    	System.out.println("DataBuffer.TYPE_USHORT = "+DataBuffer.TYPE_USHORT);
    	System.out.println("DataBuffer.TYPE_INT = "+DataBuffer.TYPE_INT);
    	System.out.println("DataBuffer.TYPE_FLOAT = "+DataBuffer.TYPE_FLOAT);
    	System.out.println("DataBuffer.TYPE_DOUBLE = "+DataBuffer.TYPE_DOUBLE);
    	System.out.println("dataType "+dataType);
    	System.out.println("height="+height+"  width="+width+"  bands="+bands );
    	System.out.println("dataElements="+elements+"  b0size="+b0size   );
    	for (int i=0 ; i< sampleSize.length ; i++) {
        	System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    	}
    
}



	/**
	 * gets a String value for the input image data type.
	 * The value is derived from the dataType of the images SampleModel.<br>
	 * Used as an informational String.
	 * DataBuffer dataTypes are used	 * @param im	 * @return String	 */
	public String dataTypeString(RenderedImage im) {
			
		SampleModel sm = im.getSampleModel();
    
   		String s = "UNDEFINED";
   	
    	int dataType = sm.getDataType();
    
		if ( dataType == DataBuffer.TYPE_BYTE) {
	        s = "BYTE";
	        }	      
	    else if ( dataType == DataBuffer.TYPE_SHORT) {
	        s = "SHORT"; // VICAR IS HALF
	        } 
	    else if ( dataType == DataBuffer.TYPE_USHORT) {
	        s = "USHORT"; // VICAR IS HALF
	        } 
	    else if ( dataType == DataBuffer.TYPE_INT) {
	        s = "INT"; // FULL ???
	        // s = "FULL"; // VICAR USES FULL INSTEAD OF INT
	        } 
	    else if ( dataType == DataBuffer.TYPE_FLOAT) {
	        s = "FLOAT"; // FLOAT (VICAR USES REAL)
	        } 
	    else if ( dataType == DataBuffer.TYPE_DOUBLE) {
	        s = "DOUBLE";
	        }    
	           
	    
	    else  {
	        System.out.println("ERROR: INput format type "+ dataType+" IS unknown");
	        
	        }
	   return s;
	}
	
/**
* This does the real work of converting an image
* @param argv - array of arguments from the command line
**/
public boolean conv(String argv[]) {
    // System.out.println(" ConvertImageJAI.conv "); 
    IIOImage iioImage = null;
    IIOMetadata im = null;
    List thumbnailList = null;
    
    boolean INPfound = false;
    String key, value;
    if (argv.length == 0) {
        printHelp();
        return true;
	    // System.exit(0); // success return
	}
	
	/** create a hash table of the arguments
	* 3 types 
	* 1) key=value 
	* 2) -kvalue  where -k is the key and the rest is value
	* 3) no dash or key= . key will be a number (position #) and 
	* value will be the whole argument
	* */
	/**
	CommandLineParser clp = new CommandLineParser(argv);
	System.out.println("--------------------------------" );
	System.out.println("clp "+clp.toString() );
	System.out.println("--------------------------------" );
	Enumeration e = clp.getKeys();
	while ( e.hasMoreElements()) {
		String k = (String) e.nextElement();
		String v = clp.getValue(k);
		System.out.println("key = " +k+"   value = "+v);
	}
	System.out.println("--------------------------------" );
	***/
    
    StringWriter stringWriter = new StringWriter();
    PrintWriter  argsOut = new PrintWriter(stringWriter);
	
	for (int i=0; i<argv.length ;i++) {
	    // System.out.println(i+") "+argv[i]);
	    String arg = argv[i];
	    if (arg.indexOf("=") != -1) {
	    	key = getKey(arg, "=");
	    	value = getValue(arg, "=");
	    }
	    else { // allow just keyword and set value to "TRUE"
	    	key = arg;
	    	value = "true";
	    }
	    
	    key = key.toUpperCase(); // make compares easier
		// if (silent != false)
		// System.out.println(i+") "+key+" = "+value);
	    // write to a String. print afetr all the arguments have been processed if SILENT == true
	    argsOut.println(i+") "+key+" = "+value+" yy");
	    
			
	    if (key.equalsIgnoreCase("INP") || key.equalsIgnoreCase("IN") ) {
	        inputFileName = value;
	    }
	    else if (key.equalsIgnoreCase("OUT") ) {
	        outputFileName = value;
	        if (value.equalsIgnoreCase("DISPLAY") ) {
	            displayImage = true;
	        }
	    }
		else if (key.equalsIgnoreCase("READER") ) {
	    	
			forcedReaderFormat = value;	    	
		}	    
		else if (key.equalsIgnoreCase("SILENT") ) {
	    	
						silent = true;
	    	
				}
	    else if (key.equalsIgnoreCase("RI") ) {
	    		    		
	        	RI = value;
	        	if (value.equalsIgnoreCase("true") || value.equalsIgnoreCase("t") || value.equalsIgnoreCase("")  ) {
	        		getAsRenderedImage = true;
	        		RI = "true";
	        	} else if (value.equalsIgnoreCase("false") || value.equalsIgnoreCase("f")  ) {
	        		getAsRenderedImage = false;
	        		RI = "false";
	        	}
	    	
	    }
		else if (key.equalsIgnoreCase("BANDS") || key.startsWith("BANDS") ) {
			// split the value on [(,]) chars
			// no checking is currently done to see that bands selected are valid for 
			// the input image
			String s[] = value.split("[\\[\\(,\\)\\]]") ;
			bandSelect = true;
			// System.out.println("BANDS value="+value+"  length="+s.length);  
			int ii = 0;
			int bandCt = 0;
			
			// determine how many bands are in the input and create an array thast size
			// an array with extra items will kill the Operator
			if (debug) {
				System.out.println("count the bands --------------------");
			}
			for (int j=0 ; j< s.length ; j++) {
				// System.out.println(j+">"+s[j]+"< "+s[j].length());
				// sometimes a zero length array member in s[] will exist, ignore it
				if (s[j].length() != 0 && ii < 3) {
					try {					
						bandList[ii] = Integer.parseInt(s[j]);
						}
					catch (NumberFormatException nfe) {
						System.out.println("BANDS NumberFormatException "+nfe);
						// bandList[ii] = 0;
						}
					bandCt++;
				}
			}
			bandList = new int[bandCt];
			
			for (int j=0 ; j< s.length ; j++) {
				// System.out.println(j+">"+s[j]+"< "+s[j].length());
				// sometimes a zero length array member in s[] will exist, ignore it
				if (s[j].length() != 0 && ii < 3) {
					try {					
						bandList[ii] = Integer.parseInt(s[j]);
					}
					catch (NumberFormatException nfe) {
						System.out.println("BANDS NumberFormatException "+nfe);
						bandList[ii] = 0;
					}
					ii++;
				}
			}
			
		}
	    else if (key.equalsIgnoreCase("OFORM") ) {
	        outputDataTypeString = value;
	    }
	    else if (key.equalsIgnoreCase("FORMAT") ) {
	    	if (value != null ) {
	        	outputFormat = value;
	    	}
	    }
	    else if (key.equalsIgnoreCase("DISPLAY") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	displayImage = true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("XML") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	outputXML =true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("USE_RAW_READER")) {
			 if (value == null || value.equalsIgnoreCase("true")) {
				 useRawReader = true;
				 // force the RawImageReader to be used to read a vicar, PDS image
				 // instead of vicario. Test only
			 }
		 }
	    else if (key.equalsIgnoreCase("RAW_READER_NAME")) {
			 if (value != null && (value.equalsIgnoreCase("raw") || value.equalsIgnoreCase("raw2"))) {
				 rawReaderName = value;
				 // raw uses the raw reader in jai_imageio.jar
				 // raw2 uses the raw reader in rawImageReader.jar
			 }
		 }
		else if (key.equalsIgnoreCase("KEEP_LINE_PREFIX") ) {
			System.out.println("KEEP_LINE_PREFIX "+value);
					if (value.equalsIgnoreCase("false")) {
						keep_line_prefix = false;
						// the default is to use the prefix data and attach 
						// it to the metadata
					}
				}
		//		USE_FILENAME_IN_LABEL
		else if (key.equalsIgnoreCase("USE_FILENAME")) {
				 if (value == null || value.equalsIgnoreCase("true")) {
					 use_filename = true;
					 // reader will be called with the filename
					 // used for Native readers
					 // most readers will die
				 }
			 }
	    // PDS_FILENAME_IN_LABEL
	    else if (key.equalsIgnoreCase("PDS_FILENAME")) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	useOutputFilename = true;
	    	}
	    }	
//	  PDS_DETACHED_LABEL
	    else if (key.equalsIgnoreCase("PDS_DETACHED_LABEL")) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	pdsDetachedLabel = true;
	    	}
	    }	
//	  PDS_DETACHED_ONLY
	    else if (key.equalsIgnoreCase("PDS_DETACHED_ONLY")) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	pdsDetachedOnly = true;
	        	pdsDetachedLabel = true;
	    	}
	    }
//	    PDS_PTR
	    else if (key.equalsIgnoreCase("PDS_PTR")) {
	    	// if (value != null ) 
	    	{
	        	pds_ptr = value; // null indicates no value
	    	}
	    }
//		  USE_PIRL
	    else if (key.equalsIgnoreCase("USE_PIRL")) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	usePIRL = true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("EMBED")  || key.equalsIgnoreCase("EMBED_VICAR") || 
	    			key.equalsIgnoreCase("EMBED_VICAR_LABEL")) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	embedVicarLabel =true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("DEBUG") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	debug =true;
	        	System.out.println("*************************************");
	        	System.out.println("DEBUG ************ TRUE *************");
	        	System.out.println("*************************************");
	    	}
	    }
	    else if (key.equalsIgnoreCase("2RGB") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	singleToRGB =true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("PLUGINS") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	printPlugins =true;
	    	}
	    }
	    	    	
	    else if (key.equalsIgnoreCase("MBAND") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	multiBand =true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("JEDI") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	jedi = true;
	    	}
	    }	
	    else if (key.equalsIgnoreCase("MarsViewerJAI") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	marsViewerJAI = true;
	    	}
	    }	else if (key.equalsIgnoreCase("MarsViewerFull") ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	marsViewerFull = true;
	    	}
	    }	
	    // XSL_JAR - find the xsl file in the jar XSL=- or XSL will cause the jar xsl file to be used
	    else if (key.equalsIgnoreCase("XSL")   ) {
	    	if (value == null || value.equalsIgnoreCase("true")) {
	    		xslFileName = defaultJarXSLfilename;
	    	}
	    	else {
	        	xslFileName = value;
	    	}
	    }
	    /**
	     * pds4namespaceFile == "" forces the file to be read from the jar
 		 * This is only needed if the namespace has changed recently. 
	     * Jar should be uptodate. jpl/mipl/io/pds4_namespaces.txt
	     * *this value will be passed to PDSImageWritePaream
	     ***/ 
	    else if (key.equalsIgnoreCase("PDS4_NAMESPACE_FILE")   ) {
	    	if (value == null || value.equalsIgnoreCase("")) {
	    		pds4namespaceFile = "" ;
	    	}
	    	else {
	    		pds4namespaceFile = value;
	    	}
	    }
	    else if (key.equalsIgnoreCase("jar") ) {
	        findInJar = value;
	        jarTest(findInJar);
	    
	        // later we could return something and use it ???
	        // System.exit(0); // success return
	        return true;
	    }
	    else if (key.equalsIgnoreCase("PDS_MER") ) {
	    	// addMerItems 
    		// calculateStatistics calc
    		// make this the default???
	    	if (value != null && (value.equalsIgnoreCase("addMerItems") || 
	    			value.equalsIgnoreCase("add")) ) {
	    		addMerItems = true;
	    	}
	    	
	    	else if (value != null && value.equalsIgnoreCase("no") ) {
	    		addMerItems = false;
	    	}
	    	else {
	    		addMerItems = false;
	    	}
	        
	    }
	    else if (key.equalsIgnoreCase("PDS_STATS") ) {
	    	// addMerItems 
    		// calculateStatistics calc
    		// addStatistics
	    	if (value != null && (value.equalsIgnoreCase("addStatistics") || 
	    			value.equalsIgnoreCase("add")) ) {
	    		addStatistics = true;
	    		calculateStatistics = false;
	    	}
	    	else if (value != null && (value.equalsIgnoreCase("calcStatistics") ||
	    				value.equalsIgnoreCase("calc"))) {
	    		addStatistics = true;
	    		calculateStatistics = true;
	    	}
	    	else if (value != null && value.equalsIgnoreCase("no") ) {
	    		addStatistics = false;
	    		calculateStatistics = false;
	    	}
	    	else {
	    		addStatistics = false;
	    		calculateStatistics = false;
	    	}
	        
	    }
	    else if (key.equalsIgnoreCase("SCALE") ) {
	    	// SCALE=(Xfactor,Yfactor,Xtranslation,Ytranslation) values are floats, no spaces allowed");
	    	if (debug) System.out.println("SCALE "+value);
	    	scaleImage = true;
	    	float[] ff = getValuesFloat(value);
	    	if (ff.length == 2) {
	    		scaleXfactor = ff[0];
	    		scaleYfactor = ff[1];
	    		scaleXtrans = 0.0F;
	    		scaleYtrans = 0.0F;
	    	} else if (ff.length == 4) {
	    		scaleXfactor = ff[0];
	    		scaleYfactor = ff[1];
	    		scaleXtrans = ff[2];
	    		scaleYtrans = ff[3];
	    	}
	    	if (debug) System.out.println("SCALE "+scaleXfactor+" "+scaleYfactor+" "+scaleXtrans+" "+scaleYtrans);
	    	
	    }
	    else if (key.equalsIgnoreCase("CROP") ) {
	    	// CROP=(cropX,cropY,cropHeight,cropWidth) values are floats, no spaces allowed");
	    	if (debug) System.out.println("CROP "+value);
	    	cropImage=true;
	    	float[] ff = getValuesFloat(value);
	    	if (ff.length == 2) {
	    		cropX      = ff[0];
	    		cropY      = ff[1];
	    		cropWidth  = 0.0F;
	    		cropHeight = 0.0F;
	    	} else if (ff.length == 4) {
	    		cropX 		= ff[0];
	    		cropY 		= ff[1];
	    		cropWidth   = ff[2];
	    		cropHeight  = ff[3];
	    	}
	    	System.out.println("CROP "+cropX+" "+cropY+" "+cropWidth+" "+cropHeight);
	    }
	    /* new 4-2014 srl to support ImageReadParam image read subpixel selections 
	     * useRawReader flag will be set true if READER_BANDS, SUBSAMPLING, SOURCE_REGION are set */
	    else if (key.equalsIgnoreCase("READ_BANDS") || key.equalsIgnoreCase("READER_BANDS") ) {
			// split the value on [(,]) chars
			// no checking is currently done to see that bands selected are valid for 
			// the input image
			String s[] = value.split("[\\[\\(,\\)\\]]") ;
			readerBandSelect = true;
			// useRawReader = true;
			// System.out.println("BANDS value="+value+"  length="+s.length);  
			int ii = 0;
			int bandCt = 0;
			
			// determine how many bands are in the input and create an array that size
			// an array with extra items will kill the Operator
			// NO checking is performed. Bad values will cause an exception
			if (debug) System.out.println("count the bands --------------------");
			for (int j=0 ; j< s.length ; j++) {
				// System.out.println(j+">"+s[j]+"< "+s[j].length());
				// sometimes a zero length array member in s[] will exist, ignore it
				if (s[j].length() != 0 && ii < 3) {
					try {					
						readerBandList[ii] = Integer.parseInt(s[j]);
						}
					catch (NumberFormatException nfe) {
						System.out.println("READER_BANDS NumberFormatException "+nfe);
						// bandList[ii] = 0;
						}
					bandCt++;
				}
			}
			readerBandList = new int[bandCt];
			
			for (int j=0 ; j< s.length ; j++) {
				// System.out.println(j+">"+s[j]+"< "+s[j].length());
				// sometimes a zero length array member in s[] will exist, ignore it
				if (s[j].length() != 0 && ii < 3) {
					try {					
						readerBandList[ii] = Integer.parseInt(s[j]);
					}
					catch (NumberFormatException nfe) {
						System.out.println("READER_BANDS NumberFormatException "+nfe);
						readerBandList[ii] = 0;
					}
					ii++;
				}
			}
			
		}
	    else if (key.equalsIgnoreCase("SOURCE_RENDERSIZE")  || key.equalsIgnoreCase("SRC_RENDERSIZE")) {
	    	if (debug) System.out.println("SOURCE_RENDERSIZE "+value);
	    	sourceRenderSize = true;
	    	// useRawReader = true;
	    	int[] ff = getValuesInt(value);
	    	if (ff.length == 2) {
	    		srcRenderSizeWidth = ff[0];
	    		srcRenderSizeHeight = ff[1];
	    	}
	    	
	    }
	    else if (key.equalsIgnoreCase("SUBSAMPLING")  || key.equalsIgnoreCase("SUBSAMP")) {
	    	// SCALE=(Xfactor,Yfactor,Xtranslation,Ytranslation) values are floats, no spaces allowed");
	    	if (debug) System.out.println("SUBSAMPLING "+value);
	    	subsampleImage = true;
	    	// useRawReader = true;
	    	int[] ff = getValuesInt(value);
	    	if (ff.length == 2) {	    		
	    		sourceXsubsampling = ff[0];
	    		sourceYsubsampling = ff[1];
	    		subsamplingXoffset = 0;
	    		subsamplingYoffset = 0;
	    		
	    	} else if (ff.length == 4) {
	    		
	    		sourceXsubsampling = ff[0];
	    		sourceYsubsampling = ff[1];
	    		subsamplingXoffset = ff[2];
	    		subsamplingYoffset = ff[3];
	    		
	    	}
	    	// should check for valid values. Error if incorrect, or fix
	    	if (debug) System.out.println("SUBSAMPLING "+sourceXsubsampling+", "+sourceYsubsampling+", "+subsamplingXoffset+", "+subsamplingYoffset
);
	    	
	    }
	    else if (key.equalsIgnoreCase("TILE_SIZE") ) {
	    	int[] ff = getValuesInt(value);
	    	if (ff.length == 2) {
	    		tileSizeX = ff[0];
	    		tileSizeY = ff[1];
	    	} else if (ff.length == 1) {
	    		tileSizeX = ff[0];
	    		tileSizeY = ff[0];
	    	} 
	    	
	    		
	    	if (debug) System.out.println("TILE_SIZE "+value+",  "+tileSizeX+" x "+tileSizeY);
	    }
	    else if (key.equalsIgnoreCase("TIF_WRITE_TILE_SIZE") ) {
	    	int[] ff = getValuesInt(value);
	    	if (ff.length == 2) {
	    		tifTileSizeX = ff[0];
	    		tifTileSizeY = ff[1];
	    	} else if (ff.length == 1) {
	    		tifTileSizeX = ff[0];
	    		tifTileSizeY = ff[0];
	    	} 
	    	
	    		
	    	if (debug) System.out.println("TIF_WRITE_TILE_SIZE "+value+",  "+tileSizeX+" x "+tileSizeY);
	    }
	    else if (key.equalsIgnoreCase("SOURCE_REGION") ) {
	    	// CROP=(cropX,cropY,cropHeight,cropWidth) values are floats, no spaces allowed");
	    	if (debug) System.out.println("SOURCE_REGION "+value);
	    	sourceRegion=true;
	    	// useRawReader = true;
	    	int[] ff = getValuesInt(value);
	    	if (ff.length == 2) {
	    		// if width and height are 0 use the rest of the image 
	    		
	    		sourceXoffset   = ff[0];
	    		sourceYoffset   = ff[1];
	    		sourceWidth  	= 0;
	    		sourceHeight 	= 0;
	    	} else if (ff.length == 4) {
	    		sourceXoffset = ff[0];
	    		sourceYoffset = ff[1];
	    		sourceWidth   = ff[2];
	    		sourceHeight  = ff[3];
	    	}
	    	// should check for valid values. Error if incorrect, or fix
	    	if (debug)  System.out.println("SOURCE_REGION "+sourceXoffset+" "+sourceYoffset+" "+sourceWidth+" "+sourceHeight);
	    }
	    
	    
	    else if (key.equalsIgnoreCase("CLAMP") ) {
	    	// CLAMP=(minValue,MaxValue) values are floats, no spaces allowed");
	    	if (debug) System.out.println("CLAMP "+value);
	    	clampImage = true;
	    	double[] dd = getValuesDouble(value);
	    	if (dd.length == 2) {
	    		clampLow[0]  = dd[0];
	    		clampHigh[0] = dd[1];
	    	}
	    	if (debug) System.out.println("CLAMP "+clampLow[0]+"  "+clampHigh[0]);
	    }	
	    else if (key.equalsIgnoreCase("RESCALE") ) {
	    	// RESCALE=[true/false] default is TRUE, only used if OFORM is to a new data type");
	    	if (value == null || value.equalsIgnoreCase("true")) {
	    		rescaleOnFormat = true;
	    	} else {
				rescaleOnFormat = false;
	    	}
	    }
	    
	    else if (key.equalsIgnoreCase("INFO") ) {
	    	// INFO=[true,false]
	    	if (value == null || value.equalsIgnoreCase("true")) {
	        	printInfo = true;
	    	}
	    }
	    else if (key.equalsIgnoreCase("MARSVIEWER_CALLBACK") ) {
	    	// MARSVIEWER_CALLBACK=[true,false]
	    	if (value == null || value.equalsIgnoreCase("true") || value.equalsIgnoreCase("yes")) {
	    		useMarsviewerCallback = true;
	    	}
	    }
	    // velcoity engine template file to use for this conversion
	    else if (key.equalsIgnoreCase("VELO_TEMPLATE") || key.toUpperCase().startsWith("VELO_T")) {
	    	// if (value != null ) 
	    	{
	    		velocityTemplateFilename = value; // null indicates no value
	    	}
	    }
	 // velcoity engine configuration file directory. Make a vicar ENV variable for a select??
	    else if (key.equalsIgnoreCase("VELO_CONFIG")  || key.toUpperCase().startsWith("VELO_C")) {
	    	// if (value != null ) 
	    	{
	    		velocityConfigPath = value; // null indicates no value
	    	}
	    }
	    
	    else if (key.equalsIgnoreCase("HELP") ) {
	    	codecInfo();
	        printHelp();
	        // System.exit(0); // success return
	        return true;
	    } else if (key.equalsIgnoreCase("PDS_LABEL_TYPE") ) {
	    	// RESCALE=[true/false] default is TRUE, only used if OFORM is to a new data type");
	    	// PDS_LABEL_TYPE=[MSL_ODL,ODL3,PDS3,PDS4] default is PDS3
	    	if (value != null ) {
	    		pdsLabelType = value;
	    	} 
	    	if (debug) System.out.println("PDS_LABEL_TYPE "+value);
	    	
	    } else if (key.equalsIgnoreCase("ADD_BINARY_HEADER") ) {
	    	
	    	if (value == null || value.equalsIgnoreCase("true")) {
	    		addBinaryHeader = true;
	    	} else {
				addBinaryHeader = false;
	    	}
	    	if (debug) System.out.println("ADD_BINARY_HEADER "+value);
	    } else if (key.equalsIgnoreCase("ADD_BLOB") ) {
	    	
	    	if (value == null || value.equalsIgnoreCase("true")) {
	    		addBLOB = true;
	    	} else {
				addBLOB = false;
	    	}
	    	if (debug) System.out.println("ADD_BLOB "+value);
	    }  // PDS4 Table reader support
	    else if (key.equalsIgnoreCase("PDS4_TABLE_LIST") ) {
	    	
	    	if (value == null || value.equalsIgnoreCase("true")) {
	    		pds4_table_list = true;
	    	} else {
	    		pds4_table_list = false;
	    	}
	    	pds4_table_is_the_output = true;
	    	if (debug) System.out.println("PDS4_TABLE_LIST "+value);
	    	
	    } else if (key.equalsIgnoreCase("PDS4_TABLE_FIELDS") ) { 
	    	
	    	if (value != null ) {
	    		pds4_table_fields = value;
	    	} 
	    	pds4_table_is_the_output = true;
	    	if (debug) System.out.println("PDS4_TABLE_FIELDS "+value);
	    }  else if (key.equalsIgnoreCase("PDS4_TABLE_OUTPUT_FORMAT") ) { 
	    	 // could also use format or just ignore it if any table values are seen
		    // if we are reading a Table but creating a PDS4 label (not available yet)
		    // we could want to have format useable ??
	    	
	    	if (value != null) { // check "csv" or "fixed-width"
	    		value = value.toLowerCase();
	    		if (value.equals("csv")  ) {
	    			pds4_table_output_format = "csv";
	    		} else if (value.startsWith("fixed")  || value.equals("fixed-width")) {
	    			pds4_table_output_format = "fixed-width";
	    		}
	    	} 
	    	pds4_table_is_the_output = true;
	    	if (debug) System.out.println("PDS4_TABLE_OUTPUT_FORMAT "+value+" "+pds4_table_output_format);
	    }  else if (key.equalsIgnoreCase("PDS4_TABLE_INDEX") ) { 
	    	
	    	// is this a String or an int
	    	if (value != null ) { // check "csv" or "fixed-width"
	    		int[] ff = getValuesInt(value);
	    		pds4_table_index = ff[0];
	    		
	    	} 
	    	pds4_table_is_the_output = true;
	    	if (debug) System.out.println("PDS4_TABLE_INDEX "+pds4_table_index+" "+ value);
	    }  else if (key.equalsIgnoreCase("TABLE_FIELD_SEPARATOR") ) { 
	    	// don't use??
	    	if (value != null ) { // check "csv" or "fixed-width"
	    		pds4_table_field_separator = value;
	    	} 
	    	pds4_table_is_the_output = true;
	    	if (debug) System.out.println("TABLE_FIELD_SEPARATOR "+value);
	    }  else if (key.equalsIgnoreCase("IMAGE_INDEX") ) { 
	    	
	    	if (value != null ) { 
	    		int[] ff = getValuesInt(value);
	    		imageIndex = ff[0];
	    	} 
	    	
	    	if (debug) System.out.println("IMAGE_INDEX "+imageIndex+" "+ value+" ");
	    }
		    // output_filename we will use outputFileName from the out= argument
		    // also available String table_line_separator // options are "platform" "unix" "windows"
		    // String table_quote_character
		    // these values will be 
	    
	    // pdsLabelType   addBinaryHeader = false;
	    
	    if (printPlugins) codecInfo();
	    
	    
	    
	

	    

	    
	} // end of loop thru arguments
	
	/**
	System.out.println("silent =  "+silent+" ");
    if (!silent) {	  
    	System.out.println("!silent");
    	System.out.println(argsOut.toString());
    } else {
    	System.out.println("silent");
    }
	**/ 
	
	 
	 
	 if (jedi == true) {
	 	
	 	jediTest(inputFileName, displayImage);
	 	if (debug) { System.out.println("end of JEDI test"); }
	 	return true;
	 	
	 } else if (marsViewerFull == true) {
		 	String readType = "full";
		 	marsViewerTest(inputFileName, displayImage, readType);
		 	if (debug) { System.out.println("end of MarsViewerFull test"); }
		 	return true;
		 	
	} else if (marsViewerJAI == true) {
	 	String readType = "jai";
	 	marsViewerTest(inputFileName, displayImage, readType);
	 	if (debug) { System.out.println("end of MarsViewerJAI test"); }
	 	return true;
	 	
}
	 
	  Node node = null;
	  Document document = null;
	  ImageReadParam irp = new ImageReadParam();
	  if (debug) {
		  System.out.println("calling ImageUtils.fullRead >>>>>>>>>>>>>>>>>>>>>>>>>>");
		  System.out.println("calling ImageUtils.fullRead tileSizeX = "+tileSizeX+" tileSizeY = "+tileSizeY+"  useRawReader "+useRawReader);
		  System.out.println("getAsRenderedImage = "+getAsRenderedImage);
		  System.out.println("ImageReadParam = "+irp);
		 
	  }
	  
	  /**
	  if (table_is_the_output == true) {
		  PDS4TableReadParam pds4TableReadParam = new PDS4TableReadParam();
		  // fill in the values
		  pds4TableReadParam.setFields(table_fields);
		  pds4TableReadParam.setIndex(table_index);
		  System.out.println("table_format ="+table_format+"<");
		  pds4TableReadParam.setOutput_format(table_format);
		  pds4TableReadParam.setOutputFileName(outputFileName);
		  pds4TableReadParam.setLabelFileName(inputFileName);
		  pds4TableReadParam.setFieldSeparator(table_field_separator);
		  // add fakeImage
		  irp = pds4TableReadParam ;		  
	  }
	  ***/
	  
	  /** old way
	  iioImage = fullRead(inputFileName);	
	  if (iioImage == null) {
		  System.out.println("ERROR: could not read input file");	
		  return false;
	  }
	  **/
	  ImageUtils imUtil = new ImageUtils(inputFileName) ;
	  imUtil.setDebug(debug);
	  imUtil.setUseRawReader(useRawReader);
	  imUtil.setRawReaderName(rawReaderName);
	  imUtil.setGetAsRenderedImage(getAsRenderedImage);
	  /** InputStreamWrapper **/
	  imUtil.setUseMarsviewerCallback(useMarsviewerCallback);
	    
	  
	  // check flags and set ImageReadParam values if they are available
	  // all of these values will wok for any ImageReadParam
	  if (readerBandSelect == true) {
	    irp.setSourceBands(readerBandList);
	  }
	  if (subsampleImage == true) {
	    irp.setSourceSubsampling(sourceXsubsampling, sourceYsubsampling, subsamplingXoffset, subsamplingYoffset);
	  }
	  if (sourceRegion == true) {
	    irp.setSourceRegion(new Rectangle(sourceXoffset, sourceYoffset, sourceWidth, sourceHeight));
	  }
	  
	  if (sourceRenderSize == true && srcRenderSizeWidth != 0 && srcRenderSizeHeight != 0) {
		  if (debug) {
			  System.out.println("irp.canSetSourceRenderSize() "+irp.canSetSourceRenderSize() );
		  }
		  irp.setSourceRenderSize(new Dimension (srcRenderSizeWidth, srcRenderSizeHeight));
	  }
	  // add tileSize
	  // if (tileSizeX > 0 && tileSizeY > 0) {
	  if (tileSizeY > 0) {
		  if (debug) {
			  System.out.println("irp.setTileSizeX("+tileSizeX+")   irp.setTileSizeY("+tileSizeY+")");
		  }
		  imUtil.setTileSizeX(tileSizeX);
		  imUtil.setTileSizeY(tileSizeY);
		  if (irp instanceof VicarImageReadParam) {
			  ((VicarImageReadParam) irp).setTileSizeX(tileSizeX);
			  ((VicarImageReadParam) irp).setTileSizeY(tileSizeY);
		  } else if (irp instanceof PDSImageReadParam) {
			  ((PDSImageReadParam) irp).setTileSizeX(tileSizeX);
			  ((PDSImageReadParam) irp).setTileSizeY(tileSizeY);
		  }
	  }
	  
	 
	 
	 try {
		imUtil.setImageIndex(imageIndex);
		
		if (debug) {
			  System.out.println("jConvertIIO.conv irp="+irp+" inputFileName="+inputFileName);
		  }
		
	    iioImage = imUtil.fullRead(inputFileName, irp);
	    
	    // check if metadata is being read in??
	    reader = imUtil.getImageReader();
	    if (debug) {
			  System.out.println("jConvertIIO.conv reader = "+reader);
		  }
	    readerFormat = reader.getFormatName();
	    // could check if the reader is pds, cast and get fakeImage directly
	    fakeImage = imUtil.getFakeImage();
	    
	    if (readerFormat.equalsIgnoreCase("pds") ) {
			fakeImage = ((PDSImageReader) reader).getFakeImageNoRead();
		}
	    
	    RenderedImage rii = iioImage.getRenderedImage();
	    
	    /**** pds4table ***
	    if (readerFormat.equalsIgnoreCase("pds4table")) {
	    	if (reader instanceof PDS4TableReader) {
	    		if (debug) {
	    			System.out.printf("jConvertIIO readerFormat = %s instanceof PDS4TableReader \n", readerFormat);
	    		}
	    		((PDS4TableReader) reader).setDebug(debug);
	    	}
	    	if (debug) {
	    		System.out.println("jConvertIIO AFTER calling imUtil.fullRead() readerFormat = "+readerFormat);
	    		System.out.println("jConvertIIO renderedImage from iioImage is: "+rii);
	    		// force a new read of the file with an ImageReadParam set?
	    		// get the data and do something with it?
	    		// it is a String, not a RenderedImage
	    		// eventually write as something alse//
	    		// get the metadata and print it
	    		String format = "";
	    		String[] formats ;
	    		String docName = "";
	    		DOMutils domUtils = new DOMutils();
	    	
	    		im = iioImage.getMetadata();
	    		System.out.println("readerFormat = "+readerFormat);
	    		System.out.println("im = "+im);
			
	    		formats = im.getMetadataFormatNames();    
						
	    		System.out.println("im.getMetadataFormatNames()"); 
	    		for (int i=0 ; i< formats.length ;i++) {
	    			String formatName = formats[i];
	    			docName = docName + formatName+"_";
	    			System.out.println(" formats["+i+"] " +formats[i]+" - "+formatName+" docName = "+docName); 		
				
	    			// get the metadata and seriaize
	    			Node m = im.getAsTree(formatName);
	    			// String xmlName = formatName+".xml";
	    			String xmlName = String.format("%s.xml", formatName);
	    			domUtils.serializeNode(m, xmlName, "xml");
	    		}
	    	}
			
			// put all the PDS4 table read params into a PDS4TableReadParam 
			PDS4TableReadParam pds4TableReadParam = (PDS4TableReadParam) reader.getDefaultReadParam();
			
			pds4TableReadParam.setListTables(pds4_table_list);
			pds4TableReadParam.setFields(pds4_table_fields);
			pds4TableReadParam.setFieldSeparator(pds4_table_field_separator);
			pds4TableReadParam.setOutput_format(pds4_table_output_format);
			// pds4TableReadParam.setOutputFileName(outputFileName);
			pds4TableReadParam.setOutputFileName("string");
			
			// this shouldn't be used
			// File outputFile = imUtil.getFile(outputFileName) ;
			// pds4TableReadParam.setOutputFile(outputFileName);
			
			pds4TableReadParam.setLabelFileName(inputFileName);			
			// open this file since the Ames utilities expect a File
			File inputFile = imUtil.getFile(inputFileName) ;
			pds4TableReadParam.setLabelFile(inputFile);
			// pds4TableReadParam.getLabelFile();
			
			if (debug) { 
				System.out.println("jConvertIIO line 2297  outputFileName = "+outputFileName ); 
				}	
			
			// set that into the imUtls
			// now do the read using readAsString()
			int imageIndex = pds4_table_index; // does this get used at all??
			// probably the read param does the work
			String table = ((PDS4TableReader) reader).readAsString(imageIndex, pds4TableReadParam);
			// the string may be used for something ??
			// write to a file for now
			if (debug) { 
				System.out.println("jConvertIIO line 2308 ################# ((PDS4TableReader) reader).readAsString ####"); 
				System.out.println("outputFileName = "+outputFileName ); 
				}	
			
			// outputFileName, outputFormat
			// imUtil.writeStringToFile("string_"+outputFileName, table);
			if (outputFileName.equals("")) {
				System.out.println(table);
			} else {
				imUtil.writeStringToFile(outputFileName, table);
			}
			if (debug) { 
				System.out.println("jConvertIIO line 2315 ################# imUtil.writeStringToFile ##############"); 
				}	
	    	return true;
	    }
	     *** pds4table ***/
	    
	    if (displayImage) {
	    	RenderedImage ri = iioImage.getRenderedImage();
	    	imUtil.displayImage(ri);
	    }
	    
	    
	    
			
	} catch (Exception e) {
		System.out.println("jConvertIIO calling imUtil.fullRead() Exception "+e);
		e.printStackTrace();
		System.out.println("jConvertIIO exiting");
		return false;
	}
	  
	  
	 if (debug) {
		  	System.out.println("after imUtil.fullRead >>>>>>>>>>>>>>>>>>>>>>>>>>>");			  
		  	System.out.println("<<<<<<<<<<<<<<<<<<<<<<<<<<< "+reader+"  >>>>>>>>>>>>>>>>>>>>>>>>>>");
		  	System.out.println("iioImage = "+iioImage+" ");
		  	System.out.println("pdsDetachedOnly = "+pdsDetachedOnly+"  readerFormat = "+readerFormat);
		  }
	 // readerFormat is set in fullRead - we don't know the input format yet
	 if (pdsDetachedOnly == true && 
			 ( readerFormat.equalsIgnoreCase("vicar") ||  readerFormat.equalsIgnoreCase("pds") ||
					 readerFormat.equalsIgnoreCase("pds4")) ) {
	 	// later we may add other inputs??
	 	if (debug) System.out.println("calling labelOnlyRead - PDS detached Label ONLY >>>>>>>>>>>>>>>>>>>>>>>>>>");
	 	//	 check metadata, see that is correct
	 	// iioImage = labelOnlyRead();
	 	// if the image is read as a rendered Image then the data isn't read unless it's asked for
	 	// check to see if this is really true !!!
	 	// ignore requests to display the image
	 	// this SHOULD make writing a label only VERY quick
	 	
	 	// iioImage = fullRead(inputFileName);
	 	
	 	
	 	
	 	
		  // find the xsl default file in the jar
		  // if (outputXML ==  true) {
		  	
		  	DOMutils domUtils = new DOMutils();
		  	String xmlFile1 =  inputFileName + ".xml";
		  	
		  	im = iioImage.getMetadata();
	        	            
	            if (im == null) {
	                if (debug) System.out.println("The image "+inputFileName+"  has no ImageMetadata.");
	                // return null;
	            } else {
	                
	                String nativeFormatName = im.getNativeMetadataFormatName();
	                if (debug) System.out.println("Image metadata: "+nativeFormatName);
	                if (nativeFormatName.equalsIgnoreCase("VICAR_LABEL") || 
	                	nativeFormatName.equalsIgnoreCase("PDS_LABEL")) {
	                	document =  (Document) im.getAsTree(nativeFormatName);
	                	if (debug) System.out.println("VICAR or PDS document "+ docInfo(document) );
	                } else {
	                	node =  im.getAsTree(nativeFormatName);
	                	System.out.println("node "+ docInfo(node) );
	                	// convert this to Document
	                	IIOMetadataNode iomNode = (IIOMetadataNode) node;
	                	IIOMetadataToDOM iomDOM = new IIOMetadataToDOM (iomNode) ;
	                	document = iomDOM.getDocument();
	                	// if (debug) System.out.println("document "+ docInfo(document) );
	                }              
	            }
	          
	          if (document != null) {
	          	
	          	if (outputXML)  {
	          		if (debug) System.out.println("write metadata to xml for the image: "+xmlFile1);
	            	domUtils.serializeDocument(document, xmlFile1, "xml");
	            	// a DOMtree viewer could also be called here
	          	}
	          } else {
	          	if (debug) System.out.println("NO metadata to write to xml : "+xmlFile1);
	          }  	
	          
		  // }
	 	
		
		   // can we get image size from iioImage? imageMetadata
		  if (readerFormat.equalsIgnoreCase("pds") ) {
				fakeImage = ((PDSImageReader) reader).getFakeImageNoRead();
			}
			
		try {
			sourceImage = iioImage.getRenderedImage();
		} catch (java.lang.IllegalArgumentException iie) {
			 System.out.println("java.lang.IllegalArgumentException "+iie );
			 iie.printStackTrace();			
		}
	 	// sourceImage = (RenderedImage) null;
	    IIOImage iioImageOut  = new IIOImage(sourceImage, thumbnailList, im);
	    
	    // processSave takes care of any transcoding needed
	    processSave(iioImageOut, inputFileName, outputFileName, outputFormat); 
	    if (debug) { System.out.println("PDS detached label write Done"); }
	    return true; // skip all the rest of the stuff after this
	  } else {
			// read the image in. Then we will do anything else requested
			if (debug) {
				// System.out.println("NOT calling fullRead >>>>>>>>>>>>>>>>>>>>>>>>>>");
				System.out.println("XXXX iioImage = "+iioImage+"  >>>>>>>>>>>>>>>>>>>>>>>>>>");
			}
			// if (debug) System.out.println("calling fullRead >>>>>>>>>>>>>>>>>>>>>>>>>>");
			// iioImage = fullRead(inputFileName);		
			
			/***
			try {
			    iioImage = imUtil.fullRead(inputFileName, irp);
			    reader = imUtil.getImageReader();
			    readerFormat = reader.getFormatName();	
			} catch (Exception e) {
				System.out.println("jConvertIIO callimg imUtil.fullRead() Exception "+e);
				System.out.println("jConvertIIO exiting");
				return false;
			}
			***/
		}
	  
	 	try {
			sourceImage = iioImage.getRenderedImage();
		} catch (Exception e) {
			 System.out.println("Exception "+e );
			 e.printStackTrace();
			 sourceImage = null;
		}
	  
      // sourceImage = iioImage.getRenderedImage();
	 
      if (debug) {
			System.out.println("sourceImage = "+sourceImage+"  ");
		}
      
      
      
      
      SampleModel sm = sourceImage.getSampleModel();
      ColorModel cm = sourceImage.getColorModel();

      int  width = sourceImage.getWidth();
      int height = sourceImage.getHeight();
   
    int dataType = sm.getDataType();
        
    
    int bands = sm.getNumBands();
    int[] sampleSize = sm.getSampleSize(); // sampleSize[0] equals b0size
    int b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
      
    if (debug) 
    	printImageInfo(sourceImage, "sourceImage"); 
      
      inputDataTypeString = dataTypeString(sourceImage);
      
      
      im = iioImage.getMetadata();
      thumbnailList = iioImage.getThumbnails();
      
      if (sourceImage == null) {
          System.out.println("Error: " + inputFileName + " - couldn't read! exiting");
          return false;
          }
	  
	  inputDataType = dataType;
	  outputDataType = DataBuffer.TYPE_BYTE;
	  // get the input data type from b0size ???
	  
	  if (readerFormat.equalsIgnoreCase("fits") && getAsRenderedImage == true) {
			// BufferedImage is flipped in the reader
			flip_image = true;
		}
	  
	  if (printInfo || debug) {
	 	System.out.println("print INFO ***************************");
	 	// printInfo(inputFileName);
	 	System.out.println("File: "+inputFileName );
	 	System.out.println("Image format: "+readerFormat );	   	    
	    System.out.println(" input data type is "+inputDataTypeString);
	    System.out.println(" samples "+width+", lines "+height+", bands "+bands);
	    System.out.println(" sample size "+b0size+", lements "+elements);
		System.out.println(" rescaleOnFormat "+rescaleOnFormat);
		System.out.println(" sourceImage.getTileHeight() "+sourceImage.getTileHeight()+"  sourceImage.getTileWidth() "+sourceImage.getTileWidth()+" ");
		// add organization "BSQ" etc
		// show Operator values for the ones which are in use		
		System.out.println(" flip_imge "+flip_image);
		System.out.println("print INFO ***************************");
	 	if (debug == false) {
	 		getExtrema(sourceImage) ;
	 		return true;
	 	}
	 }
	 
	 if (debug) System.out.println("calling processFilters ***************************");
	 imageHasBeenProcessed = false;
	  filteredImage = processFilters(sourceImage);
	  if (debug) {
	  	System.out.println("after calling processFilters filteredImage "+filteredImage);			
	  	System.out.println("filteredImage "+filteredImage.getWidth()+" "+filteredImage.getHeight());
	  	System.out.println(" filteredImage.getTileHeight() "+filteredImage.getTileHeight()+"  filteredImage.getTileWidth() "+filteredImage.getTileWidth()+" ");
	  }
	  // filteredImage ???
	  
	  if (outputDataTypeString.equals("DEFAULT")) {
	  	// this is the default if there is NO OFORM= argument on the command line
	    // processedImage = sourceImage;
	    processedImage = filteredImage;
	    // imageHasBeenProcessed = false;
	    outputDataTypeString = inputDataTypeString ;
	    outputDataType = dataType;
	    
	  }
	  else {
	    if (outputDataTypeString.equalsIgnoreCase("BYTE")) {
	        outputDataType = DataBuffer.TYPE_BYTE;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("HALF")) {
	        outputDataType = DataBuffer.TYPE_SHORT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("SHORT")) {
	        outputDataType = DataBuffer.TYPE_SHORT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("USHORT")) {
	        outputDataType = DataBuffer.TYPE_USHORT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("FULL")) {
	        outputDataType = DataBuffer.TYPE_INT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("INT")) {
	        outputDataType = DataBuffer.TYPE_INT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("REAL")) {
	        outputDataType = DataBuffer.TYPE_FLOAT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("FLOAT")) {
	        outputDataType = DataBuffer.TYPE_FLOAT;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("DOUB")) {
	        outputDataType = DataBuffer.TYPE_DOUBLE;
	        }
	    else if (outputDataTypeString.equalsIgnoreCase("DOUBLE")) {
	        outputDataType = DataBuffer.TYPE_DOUBLE;
	        }
	    else  {
	        System.out.println("ERROR: output format type "+ outputDataTypeString+" IS unsupported");
	        System.out.println("Valid OFORM= arguments are: BYTE, HALF, SHORT, USHORT, FULL, INT, ");
	        System.out.println("REAL, FLOAT, DOUB, DOUBLE ");
	        System.out.println("exiting ");
	        System.exit(1); // 1 is an error, 0 for success
	        }
	        
	    // processedImage = processFormat(sourceImage, outputDataType, rescaleOnFormat);
	    processedImage = processFormat(filteredImage, outputDataType, rescaleOnFormat);
	    imageHasBeenProcessed = true;	    
	    }
	  
	  if (flip_image) {
		  ParameterBlock pb = new ParameterBlock();
			
		  javax.media.jai.operator.TransposeType 
		  type = javax.media.jai.operator.TransposeDescriptor.FLIP_VERTICAL;
		          // type = javax.media.jai.operator.TransposeDescriptor.FLIP_HORIZONTAL;
		  pb.addSource(processedImage).add(type);
		  if (debug) { 
			  System.out.println("Flip (transpose) the image");
		  }
		  // PlanarImage flippedImage = (PlanarImage) JAI.create("transpose", pb);
		  RenderedImage flippedImage =  JAI.create("transpose", pb);
			  
		  processedImage = flippedImage;
	      imageHasBeenProcessed = true;	
	  }
	    
	   if (debug) {
	    System.out.println("File: "+inputFileName );	    
	    System.out.println(" input data type is "+inputDataTypeString);
	    System.out.println(" output data type is "+outputDataTypeString);
	    System.out.println(" outputDataType "+outputDataType+"  inputDataType "+inputDataType);
		System.out.println(" imageHasBeenProcessed "+imageHasBeenProcessed);
		System.out.println(" outputFormat "+outputFormat);
		System.out.println(" rescaleOnFormat "+rescaleOnFormat);
		System.out.println(" use_filename "+use_filename);
		System.out.println(" outputXML "+outputXML);
		
		// check sampleModel and colorModel
		
		// show Operator values for the ones which are in use
		// if (outputFormat != null && outputFormat.equalsIgnoreCase("pds")) {
		 if (outputFormat != null && outputFormat.toLowerCase().startsWith("pds")) {
			System.out.println(" addMerItems "+addMerItems);
			System.out.println(" addStatistics "+addStatistics);
	    	System.out.println(" calculateStatistics "+calculateStatistics);
	    	// PDS detached label
	    	System.out.println(" pdsDetachedLabel "+pdsDetachedLabel);
	    	System.out.println(" pdsDetachedOnly "+pdsDetachedOnly);
	    	System.out.println(" useOutputFilename "+useOutputFilename);
	    	System.out.println(" pds_ptr "+pds_ptr);
	    	System.out.println(" usePIRL "+usePIRL);
	    	System.out.println(" inputFileName "+inputFileName);
	    	System.out.println(" outputFileName "+outputFileName);
	    	System.out.println(" velocityTemplateFilename "+velocityTemplateFilename);
	    	System.out.println(" velocityConfigPath "+velocityConfigPath);
	    	
	    	System.out.println(" pdsLabelType "+pdsLabelType);
		}
	   }
	    
	  // IIOImage iioImage = new IIOImage(image, null, metadata);  
	  if ( displayImage ) {
	  	String sTitle = "Source: "+inputFileName;
	  	if (bands <= 3) {
	  		if (debug) {
	  			System.out.println("DISPLAY the image");    		
	    		System.out.println(sTitle+" "+outputDataTypeString+" "+outputFormat);
	  		}
	    		RenderedImage cim = conditionImage(sourceImage ) ;
	    		// RenderedImage cim = conditionImageToByte(sourceImage ) ;
	    		jfImageS = new JFrameJade(sTitle);
	    		jfImageS.setImage(cim) ;
	    		jfImageS.pack();
	    		jfImageS.show();
	    
	    if (debug) System.out.println(" imageHasBeenProcessed "+imageHasBeenProcessed);
	    if (imageHasBeenProcessed) {
	    		sTitle = "Filtered: "+inputFileName;
	    		jfImageF = new JFrameJade(sTitle);
	    		jfImageF.setImage(filteredImage) ;
	    		jfImageF.pack();
	    		jfImageF.show();
	    	}
	    
	    // this test stuff only, normally display only one window
	    	if (imageHasBeenProcessed) {
	    		sTitle = "Processed: "+inputFileName;
	    		jfImageP = new JFrameJade(sTitle);
	    		jfImageP.setImage(processedImage) ;
	    		jfImageP.pack();
	    		jfImageP.show();
	    	}
	  	}
	   
	   if (multiBand) {
	    // int[] bandList = {4,5,6};
	    // SampleModel sm = sourceImage.getSampleModel();
	    // int nbands = sm.getNumBands();
	    // int[] bandList = {0,1,2};
	    if (bands == 1) {
	    	int[] bList = {0};
	    	bandList = bList;	
	    	sTitle = "band select:0 "+inputFileName;    	
	    } else {
	    	sTitle = "band select:0,1,2 "+inputFileName;
	    }
	    
	    if (debug) System.out.println("multiBand = TRUE ******** ");
	    // RenderedImage bandImage = processBandSelect(sourceImage, bandList);
	    // RenderedImage bandImage = processBandSelect(processedImage, bandList);
	    RenderedImage bandImage = processedImage ;
	    if (bandImage != null) {
	    	if (debug) System.out.println("bandImage NOT NULL ******** ");
	    	// sTitle = "band select:0,1,2 "+inputFileName;
	    	jfImageP = new JFrameJade(sTitle);
	    	jfImageP.setImage(bandImage, bandList) ;
	    	jfImageP.pack();
	    	jfImageP.show();
	    }
	   }
	    /**
	    int[] bandList2 = {7,7,7};
	    bandImage = processBandSelect(sourceImage, bandList2);
	    if (bandImage != null) {
	    	sTitle = "band select:7,7,7 "+inputFileName;
	    	jfImageP = new JFrameJade(sTitle);
	    	// jfImageP.setImage(bandImage) ; 
	    	jfImageP.setImage(bandImage, bandList2);
	    	jfImageP.pack();
	    	jfImageP.show();
	    }
	    
	    int[] bandList3 = {7};
	    bandImage = processBandSelect(sourceImage, bandList3);
	    if (bandImage != null) {
	    	sTitle = "band select: 7 "+inputFileName;
	    	jfImageP = new JFrameJade(sTitle);
	    	jfImageP.setImage(bandImage) ;
	    	jfImageP.pack();
	    	jfImageP.show();
	    }
	    
	    int[] bandList4 = {9};
	    bandImage = processBandSelect(sourceImage, bandList4);
	    if (bandImage != null) {
	    	sTitle = "band select: 9 "+inputFileName;
	    	jfImageP = new JFrameJade(sTitle);
	    	jfImageP.setImage(bandImage) ;
	    	jfImageP.pack();
	    	jfImageP.show();
	    }	  
	  	
	    }
	    ***/
	  } // end of display image
	 
	  // IIOMetadata metadata = null;
	  
	  if (outputXML ==  true) {
	  	
	  	DOMutils domUtils = new DOMutils();
	  	String xmlFile1 =  inputFileName + ".xml";
	  	
	  	im = iioImage.getMetadata();
        
            
            if (im == null) {
                if (debug) System.out.println("The image "+inputFileName+"  has no ImageMetadata.");
                // return null;
            } else {
                
                String nativeFormatName = im.getNativeMetadataFormatName();
                if (debug) System.out.println("Image metadata: "+nativeFormatName);
                if (nativeFormatName.equalsIgnoreCase("VICAR_LABEL") || 
                	nativeFormatName.equalsIgnoreCase("PDS_LABEL") ||
                	nativeFormatName.equalsIgnoreCase("PDS4_LABEL")) {
                	document =  (Document) im.getAsTree(nativeFormatName);
                	if (debug) System.out.println("VICAR or PDS document "+ docInfo(document) );
                } else {
                	node =  im.getAsTree(nativeFormatName);
                	System.out.println("node "+ node );
                	System.out.println("docInfo(node) "+ docInfo(node) );
                	// convert this to Document
                	try {
                		IIOMetadataNode iomNode = (IIOMetadataNode) node;
                		IIOMetadataToDOM iomDOM = new IIOMetadataToDOM (iomNode) ;
                		document = iomDOM.getDocument();
                	} catch (Exception e) {
                		System.out.println("Exception "+ e);
                		if (outputXML && node != null)  {
                			domUtils.serializeNode((Node) node, "node.xml","xml");
                		}
                	}
                	// if (debug) System.out.println("document "+ docInfo(document) );
                }              
            }
          
          if (document != null) {
          	
          	if (outputXML)  {
          		if (debug) System.out.println("write metadata to xml for the image: "+xmlFile1);
            	domUtils.serializeDocument(document, xmlFile1, "xml");
            	// a DOMtree viewer could also be called here
          	}
          } else {
          	if (debug) System.out.println("NO metadata to write to xml : "+xmlFile1);
          }  	
          
	  }
	  
	  
	  if (outputFormat == null  && outputFileName.equalsIgnoreCase("-") == false) {
	  	System.out.println("no output format specified. use FORMAT=[]");
	 	 if ( displayImage == false)  {
	  		// if the image isn't displayed we can exit now
	  		// if the image is being displayed the program will exit when the user closes the display window (Quit)
	  		if (silent != false)
	  			System.out.println("Done");
	  		// System.exit(0); // success return
	  		return true;
	  	}
	  } 
	
	
	IIOImage iioImageOut = null;
	if (outputFormat == null  && (outputFileName.equalsIgnoreCase("-") == false) &&
			outputFileName.equalsIgnoreCase("DISPLAY") == false ) {
	  	System.out.println("no output format specified. use FORMAT=[format]");
	  	System.exit(1); // error return
	}  
	else if (  outputFileName.equalsIgnoreCase("-") == false ) {
		// the file should be written out
	   
	   if (debug) {
	   System.out.println("--------------------------------------");
	   System.out.println("input image is "+inputFileName );
	   System.out.println("input format "+readerFormat+"   readerClassName "+readerClassName );
	   System.out.println("write out the image to "+outputFileName );
	   System.out.println("XSL filename  "+xslFileName);
	   }
	    // processedImage
	    
	   SampleModel sm1 = processedImage.getSampleModel();
	   ColorModel cm1 = processedImage.getColorModel();
	   TiledImage tiledImage = null;
	   
	   int numBands = sm1.getNumBands();
	   String dts = dataTypeString(processedImage) ;
	    if (debug) {
	    	System.out.println("metadata "+im );
			System.out.println("processedImage "+processedImage.getWidth()+" x "+
				processedImage.getHeight() );
			
			System.out.println("SampleModel sm1 "+sm1);
			System.out.println("sm1.getTransferType() "+sm1.getTransferType() );
			System.out.println("bands="+sm1.getNumBands()+"  "+sm1.getWidth()+" X "+sm1.getHeight());
			
			System.out.println("  dataType =  " +sm1.getDataType()+" "+dts );
			 
			System.out.println("ColorModel cm1 "+cm1+"   ");
			if (cm1 !=null) {
				System.out.println("ColorModel cm1.getClass() "+cm1.getClass()+"  ");
				// CoplorSpace ICC_Profile
			}
			
			System.out.println(" numBands "+numBands);
			System.out.println(" outputFormat >"+outputFormat+"< ");	
	    }	
	    
	    
	    
	    // output data type must be byte for all standard formats except png and tif
	    // must also be 1 or 3 bands. png and tif support 32 bit and 16 bit data 
		if (cm1 == null && 
				((!outputFormat.equalsIgnoreCase("vic")) && 
		    	 (!outputFormat.equalsIgnoreCase("pds")) && 
		    	 (!outputFormat.equalsIgnoreCase("isis")) )
				&& (numBands == 3 || numBands == 1)) {
				 if (debug) {System.out.println("ColorModel is null "+cm1+" numBands = "+numBands+" outputFormat "+outputFormat);}
				
		        if (sm1 instanceof ComponentSampleModelJAI || sm1 instanceof ComponentSampleModel) {
		            // this allows us to directly display float/double images
		        	if (debug) {System.out.println("trying javax.media.jai.PlanarImage.createColorModel ");}
		             cm1 = javax.media.jai.PlanarImage.createColorModel(sm1);
		             if (cm1 == null) {
		            	 if (debug) { System.out.println("trying ImageCodec.createComponentColorModel "); }
		            	 cm1 = ImageCodec.createComponentColorModel(sm1);
		            	 if (cm1 == null) {
		            		 if (numBands == 3) {
		            		    java.awt.color.ColorSpace colorSpace = ICC_ColorSpace.getInstance(ICC_ColorSpace.CS_sRGB);		              
				                 cm1 = new ComponentColorModel(colorSpace, false, false, Transparency.OPAQUE, sm1.getTransferType());
		            		 } else  if (numBands == 3) {
			            		java.awt.color.ColorSpace colorSpace = ICC_ColorSpace.getInstance(ICC_ColorSpace.CS_GRAY);		              
					            cm1 = new ComponentColorModel(colorSpace, false, false, Transparency.OPAQUE, sm1.getTransferType());
			            	 } 
		            		 if (debug) { System.out.println("After creating ComponentColorModel cm1 "+cm1); }
		            	 }
		             }
		             
		          }
		          else {
		             cm1 = ImageCodec.createComponentColorModel(sm1);
		          }
		        
		        if (debug) {
		        	System.out.println("After creating ColorModel "+cm1);
		        	}
		        
		        // processedImage.getWidth();
		        // processedImage.getHeight();
		        tiledImage = new TiledImage(0,0,processedImage.getWidth(),processedImage.getHeight(), 0,0,sm1, cm1);
		     
		        // ColorModel cm2 = tiledImage.getColorModel();
		        if (debug) {
		        	System.out.println("Create tiledImage "+tiledImage);
		        	System.out.println(" use it to create iioImage with a ColorModel");
		        	System.out.println(" cm1 = "+cm1);
		        	}
		        
		        /**
		         * You cannot add a ColorModel to an existing RenderedImage.
		         * Create a new TiledImage (which is a subclass of RenderedImage) with the new 
		         * ColorModel. Then we set the existying RenderedImage (processedImage) into the new image
		         * and it will use the data.
		         */
		        tiledImage.set(processedImage);
		        iioImageOut  = new IIOImage(tiledImage, thumbnailList, im);
		        // processedImage = (RenderedImage) tiledImage;
			} else {
				 if (debug) {System.out.println("ColorModel "+cm1+"  numBands = "+numBands);}
				// check metadata, see that is correct
				    iioImageOut  = new IIOImage(processedImage, thumbnailList, im);
			}
	} 
	    
	    
	    // processSave takes care of any transcoding needed
	if (debug) {
		System.out.println("jConvertIIO processSave 2875 calling processSave %%% fakeImage "+fakeImage+" %%%%%%%%%%%%%%%%%%%");
	}
	    processSave(iioImageOut, inputFileName, outputFileName, outputFormat); 
	    System.out.println("Image write Done");
	    
	  
	  
	  
	  if ( displayImage == false)  {
	  	// if the image isn't displayed we can exit now
	  	// if the image is being displayed the program will exit when the user closes the display window (Quit)
	  	if (debug) System.out.println("Done");
	  	return true;
	  	// System.exit(0); // success return
	  }
	  
	  return true;
	}   

	/**
	 * take an argument value String which is a comma delimited list of values
	 * and returns an array of float values.
	 * Exeptions in the parsing of the string are caught. 
	 * 0 is used as the value if there is an Exception.
	 * @param s
	 * @return float[]
	 */
	public float[] getValuesFloat(String s) {
		String[] ss = getValuesString(s) ;
		float[] f = new float[ss.length];
		
		for (int i=0 ; i< ss.length ; i++) {
			try {
				f[i] = Float.parseFloat(ss[i]);
				
			}
			catch (NumberFormatException nfe) {
				f[i] = 0.0F; // is this the best number to use ??
			}
			
		}
		return f;
		
	}
	
	/**
	 * take an argument value String which is a comma delimited list of values
	 * and returns an array of double values.
	 * Exeptions in the parsing of the string are caught. 
	 * 0 is used as the value if there is an Exception.
	 * @param s
	 * @return double[]
	 */
	public double[] getValuesDouble(String s) {
		String[] ss = getValuesString(s) ;
		double[] d = new double[ss.length];
		
		for (int i=0 ; i< ss.length ; i++) {
			try {
				d[i] = Double.parseDouble(ss[i]);
				
			}
			catch (NumberFormatException nfe) {
				d[i] = 0.0; // is this the best number to use ??
			}
			
		}
		return d;
	}
	
	/**
	 * take an argument value String which is a comma delimited list of values
	 * and return an array of int values.
	 * Exeptions in the parsing of the string are caught. 
	 * 0 is used as the value if there is an Exception.
	 * @param s
	 * @return int[]
	 */
	public int[] getValuesInt(String s) {
		String[] ss = getValuesString(s) ;
		int[] ii = new int[ss.length];
		
		for (int i=0 ; i< ss.length ; i++) {
			try {
				ii[i] = Integer.parseInt(ss[i]);
				
			}
			catch (NumberFormatException nfe) {
				ii[i] = 0; // is this the best number to use ??
			}
			
		}
		return ii;
	}
	
	/**
	 * take an argument value String which is a comma delimited list of values
	 * and returns an array of the values.
	 * @param s
	 * @return String[]
	 */
	public String[] getValuesString(String s) {
		
		// remove parens
		s = s.replaceFirst("\\[","");
	    s = s.replaceFirst("\\]","");		
		s = s.replaceFirst("\\(","");
	    s = s.replaceFirst("\\)","");
	    String[] ss = s.split(",");
	               	
	    return ss;           	   	
	}

	
	/**
	 * 
	 * @param inputFileName name of the image file to read
	 * @param displayImage flag if image should be displayed
	 */
	public void marsViewerTest(String inputFileName, boolean displayImage, String readType) {
		
		// read in the image
		
		RenderedImage renderedImage = null;
	    BufferedImage bufferedImage = null;
	    IIOImage iioImage = null;
	    ImageInputStream iis = null;
	    IIOMetadata im = null;
	    IIOMetadata sm = null;
	    int numImages = 0;
	    
	    // if (debug) 
	    // Filename may be a URL
	    System.out.println("marsViewerTest: " + inputFileName+"  *********************");
	    
	    ImageUtils imUtil = new ImageUtils(inputFileName) ;
	    imUtil.setDebug(true);
	    
	    if (readType.equalsIgnoreCase("jai")) {
	    	try {
	    		imUtil.jaiCreateRead();
			
			} catch (Exception e) {
				System.out.println("imUtil.jaiCreateRead() Exception "+e);
				return ;
			}
	    } else {
	    	/*** 
	    	 * do this in normal part of jConvertIIO
	    	 * fill up the ImageReadParam
	    	javax.imageio.IIOParam
	    	setSourceRegion(Rectangle sourceRegion)
	    	setSourceBands(int[] sourceBands)
	    	setSourceSubsampling(int sourceXSubsampling, int sourceYSubsampling, int subsamplingXOffset, int subsamplingYOffset)
	    	**/
	    	ImageReadParam irp = new ImageReadParam();
	    	// check flags
	    	if (readerBandSelect == true) {
	    		irp.setSourceBands(readerBandList);
	    	}
	    	if (subsampleImage == true) {
	    		irp.setSourceSubsampling(sourceXsubsampling, sourceYsubsampling, subsamplingXoffset, subsamplingYoffset);
	    	}
	    	if (sourceRegion == true) {
	    		irp.setSourceRegion(new Rectangle(sourceXoffset, sourceYoffset, sourceWidth, sourceHeight));
	    	}
	    	
	    	try {
	    		iioImage = imUtil.fullRead(inputFileName, irp);
	    		reader = imUtil.getImageReader();
	    		RenderedImage ri = iioImage.getRenderedImage();
	    		imUtil.displayImage(ri);
			
			} catch (Exception e) {
				System.out.println("imUtil.fullRead2() Exception "+e);
				return ;
			}
	    	
	    }
		
		System.out.println("marsViewerTest: displayImage = " + displayImage+"  *********************");
	    if (displayImage) {
	    	imUtil.displayImage();
	    }
	    
		
	}
	
	
	
/**
 * 
 * @param inputFileName name of the image file to read
 * @param displayImage flag if image should be displayed
 */
public void jediTest(String inputFileName, boolean displayImage) {
	
	// read in the image
	
	RenderedImage renderedImage = null;
    BufferedImage bufferedImage = null;
    IIOImage iioImage = null;
    ImageInputStream iis = null;
    IIOMetadata im = null;
    IIOMetadata sm = null;
    int numImages = 0;
    
    // if (debug) 
    System.out.println("jediTest: " + inputFileName+"  *********************");
    
    ImageUtils imUtil = new ImageUtils(inputFileName) ;
    imUtil.setDebug(debug);
    
	try {
    imUtil.fullRead();
	} catch (IOException e) {
	  System.out.println("IOException "+e);
	  return ;
	}
    
    if (displayImage) {
    	imUtil.displayImage();
    }
    System.out.println("1 ====================================================");
    imUtil.printImageInfo();
	// get the metadata
    System.out.println("2 ====================================================");
	imUtil.displayAllMetadata();
	
	// convert metadata to a hash
	// Hashtable imHash = imUtil.getMetadataHash();
	System.out.println("2.5 ====================================================");
	
	im = imUtil.getIIOMetadata();
	String[] formats = im.getMetadataFormatNames();    
	
	
	System.out.println("im.getMetadataFormatNames()"); 
	for (int i=0 ; i< formats.length ;i++) {
		String formatName = formats[i];
		System.out.println(" formats["+i+"] " +formats[i]+" - "+formatName); 
		Hashtable h= imUtil.getHashFromMetadata(formatName);
		BufferedWriter bw = imUtil.getBufferedWriter(formatName+"_hash.txt");
		imUtil.printHashToFile(h, bw);
		try {
			bw.close();
		} catch (IOException e) {
			System.out.println("ImageUtils.printHashToFile IOException" );
			if (debug) e.printStackTrace();
		}		
	}
	
	System.out.println("2.6 ====================================================");
	
	im = imUtil.getIIOMetadata();
	formats = im.getMetadataFormatNames();    
	
	
	System.out.println("im.getMetadataFormatNames()"); 
	for (int i=0 ; i< formats.length ;i++) {
		String formatName = formats[i];
		System.out.println(" formats["+i+"] " +formats[i]+" - "+formatName); 
		Hashtable h= imUtil.getHashFromMetadata(formatName);
		BufferedWriter bw = imUtil.getBufferedWriter(formatName+"_hashString.txt");
		String hs = imUtil.printHashToString(h);
		try {
			bw.write(hs);
			bw.close();
		} catch (IOException e) {
			System.out.println("ImageUtils.printHashToFile IOException" );
			if (debug) e.printStackTrace();
		}		
	}
	
	System.out.println("3 ====================================================");
	imUtil.doHash();
	
	System.out.println("4 ====================================================");
	Hashtable h = new Hashtable();
	Hashtable vicarHash;
	h = imUtil.getHashFromMetadata(h) ; 
	System.out.println("5 print hash ====================================================");
	imUtil.printHash(h);
	System.out.println("6           ====================================================");
	Object o = h.get("VICAR_LABEL");
	System.out.println("Object from get(\"VICAR_LABEL\" "+o);
	if (o instanceof Hashtable) {
		vicarHash = (Hashtable) o;
		o = vicarHash.get("SYSTEM");
		System.out.println("Object from get(\"SYSTEM\" "+o);
		if (o instanceof Hashtable) {
			Hashtable system = (Hashtable) o;
			String format = (String) system.get("FORMAT");
			System.out.println("Object from get(\"FORMAT\" "+format);
			String nl = (String) system.get("NL");
			System.out.println("Object from get(\"NL\" "+nl);
		}
	}
	
	
	
	
	// ask the hash some questions
	
	// lic void populateHash(IIOMetadata im, Hastable imHash) ;
	
	// print the document
	
	// print out the hash
	
	// example of acessing values using XPATH
	
	
}

	/*
	 * returns a String with the class name of the suppllied Node. Since a Document descends from Node
	 * a Document may be supplied here. Useful in denugiing to see that a proper and compatable Document/Node 
	 * is being used
	 */
	public String docInfo(Node doc) {
	// public String docInfo(Document doc) {
		if (doc == null) {
			return "docInfo doc == null";
		}
		Class c = doc.getClass();
		String docName = c.getName();
		// System.out.println(
		return docName;
	}
	
	public void simpleWrite(IIOImage iioImage, String outputFormat, String outputFilename) {
		
		// get the output format and write the image out
		// see code in ImageFlipper or ImageDumper
		
	}

  
        
        
    // ------- operators -------------------
    
    /**
    * converts an image which is not displayable to one that is
    * from mica
    **/
    private RenderedImage conditionImageToByte(RenderedImage imageOp_base ) {

	
	ParameterBlock PB;
	RenderedImage temp=imageOp_base;
	 SampleModel sm = temp.getSampleModel();
	 
	 int numbands = sm.getNumBands();
	 int type = sm.getDataType();
	 if (debug) System.out.println("DataType = "+type);
	 if (type == DataBuffer.TYPE_BYTE) {
	 	if (debug) System.out.println("conditionImageToByte  Image is BYTE, no conditioning needed");
	 
	 	return temp;
	 } else {
	 	if (debug) System.out.println("conditionImageToByte  Performing image conversions...");
	 }

	

	// rescale the pixel values of the image based on the image extrema
	PB=new ParameterBlock();
	PB.addSource(temp).add(null).add(10).add(10);
	RenderedImage extrema=JAI.create("extrema",PB);

	// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
	double scale[][]=(double[][])extrema.getProperty("extrema");
	
	// double ceiling = getMaxForDataType(newDataType) ;
	
	
	
	double ceiling=Short.MAX_VALUE*2;
	double max=1,min=ceiling;
	for(int i=0;i<scale[0].length;i++){
	    max=Math.max(max,scale[1][i]);
	    min=Math.min(min,scale[0][i]);
	}
	if (debug) System.out.println("conditonImageToByte extrema ceiling="+ceiling+"  min="+min+"  max="+max);
	// round max up to the nearest power of 2. 
	// max=Math.pow(2.0,Math.round(Math.log(max)/Math.log(2)));
	// min=0;
	
	
	// this will be for BYTE output
	double constant[] = new double[numbands];
	double offset[] = new double[numbands];
	
	for(int i=0;i<scale[0].length;i++){
		constant[i] = ceiling /(max-min);
		if (min >= 0.0) {
			offset[i] = min * constant[0] * -1.0; // offset is added only for unsigned ??
		}
		else {
			offset[i] = min * constant[0] ; // offset is added only for unsigned ??
		}
	}
		
	
	
		
	System.out.println("conditionImageToByte  min="+min+"  max="+max);
	
	PB=new ParameterBlock();
	// PB.addSource(temp).add(new double[]{ceiling/(max-min)}).add(new double[]{ceiling*min/(min-max)});
	PB.addSource(temp).add(constant).add(offset);
	temp=JAI.create("rescale",PB);
	
	
	PB=new ParameterBlock();
	
	PB.addSource(temp).add(java.awt.image.DataBuffer.TYPE_BYTE);
	temp=JAI.create("Format",PB);

	imageOp_base=temp;
	if (debug) System.out.println("Conversions complete.");
	return  imageOp_base ;
    }

/**
    * converts an image which is not displayable to one that is
    * from mica
    **/
    private RenderedImage conditionImage(RenderedImage imageOp_base ) {

	
	ParameterBlock PB;
	RenderedImage temp=imageOp_base;
	 SampleModel sm = temp.getSampleModel();
	 int type = sm.getDataType();
	 if (debug) System.out.println("conditionImage DataType = "+type);
	 if (type == DataBuffer.TYPE_USHORT || type == DataBuffer.TYPE_BYTE) {
	 	if (debug) System.out.println("conditionImage  Image is BYTE or USHORT, no conditioning needed");
	 // if (type == DataBuffer.TYPE_BYTE) {
	 // 	if (debug) System.out.println("conditionImage  Image is BYTE, no conditioning needed");
	 	return temp;
	 } else {
	 	if (debug) System.out.println("conditionImage  Performing image conversions...");
	 }

	// convert the image to TYPE_USHORT & fix the null color model
	PB=new ParameterBlock();
	PB.addSource(temp).add(java.awt.image.DataBuffer.TYPE_USHORT);
	// PB.addSource(temp).add(java.awt.image.DataBuffer.TYPE_BYTE);
	temp=JAI.create("Format",PB);

	// rescale the pixel values of the image based on the image extrema
	PB=new ParameterBlock();
	PB.addSource(temp).add(null).add(10).add(10);
	RenderedImage extrema=JAI.create("extrema",PB);

	// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
	double scale[][]=(double[][])extrema.getProperty("extrema");
	double ceiling=Short.MAX_VALUE*2;
	double max=1,min=ceiling;
	for(int i=0;i<scale[0].length;i++){
	    max=Math.max(max,scale[1][i]);
	    min=Math.min(min,scale[0][i]);
	}
	if (debug) System.out.println("conditionImage  min="+min+"  max="+max);
	// round max up to the nearest power of 2. 
	max=Math.pow(2.0,Math.round(Math.log(max)/Math.log(2)));
	min=0;
	PB=new ParameterBlock();
	PB.addSource(temp).add(new double[]{ceiling/(max-min)}).add(new double[]{ceiling*min/(min-max)});
	temp=JAI.create("rescale",PB);

	imageOp_base=temp;
	if (debug) System.out.println("Conversions complete.");
	return  imageOp_base ;
    }
   /*********
    * writes the current RenderedImage to a file
    * will also write metadata if it is available
    * @param IIOimage - contains the RenderedImage, Thumbnails and IIOmetadata for the output file.<br>
    * @param outputFile - name of the file to write the image to<br>
    * @param format - String containing the format name to write to, used to select the ImageWriter<br>
    ****************/
    public void processSave(IIOImage iioImage, String inputFile, String outputFile, String format) {   
        
        if (format == null || format.equals("") || format.equals("-") ) {
        	return;
        }
        // debug = true;
        // System.out.println( "jConvertIIO.processSave() *** ######################### *** debug = "+debug);
        if (debug) {
        	System.out.println( "jConvertIIO.processSave() "+outputFile+" output format is: "+format);
        }
        writer = null;
        // the button for this option should ONLY be created if we have a BufferedImage
        // so a check for a non-null image is not needed
        try {
          Iterator writers = ImageIO.getImageWritersByFormatName( format );
          
          /* if there are multiple writers how do we choose???
           * could put all the writers into the imageWriteParam
           * 	if there is a ParmaController the user could be allowed to choose the Writer
           * */
           if (debug) System.out.println( "jConvertIIO.processSave() writers.next() "); 
           writer = (ImageWriter) writers.next();
        } catch ( NoSuchElementException e) {
			System.out.println( "jConvertIIO.processSave()  NoSuchElementException " +e );
			e.printStackTrace();
		}
       
                
        
        
        if (debug) System.out.println( "jConvertIIO.processSave() writer: "+writer);
        
        // this will be used to locate the transcoder
        ImageWriterSpi spi = writer.getOriginatingProvider();
        if (debug) System.out.println( "jConvertIIO.processSave() spi: "+spi);
        Class[] outputs = spi.getOutputTypes();
        if (debug) {
        	for (int i=0 ; i< outputs.length ; i++) {
            	System.out.println( "outputs: "+outputs[i]);
        	}
        }
        
        
        File fout_x = null;
        FileOutputStream fout = null;
        
        File fin = null;
        String outputFileName = "";
        String inputFileName = "";
        OutputStream fileWriter = null;
        
        WebdavVFS2file webdavFileOut = null;
        OutputStream webdavOutputStream = null;
        boolean isWebdav = false;
        
        // create an OutputStream to a URL - webdav writeable thing
        // Is authentication embedded in the URL?
        // have an argument so we know to try opening as a webdav?
        // WebdavFileObject
        if (debug) {
        	System.out.println( "jConvertIIO.processSave() ********************************** 1 *****");
        	System.out.println( "jConvertIIO.processSave() outputFile: "+outputFile);
        }
        
        /*****
        try {
            Class.forName("jpl.mipl.io.WebdavVFS2file");
            // it exists on the classpath
            System.out.println( "WebdavVFS2file EXISTS ***************");
            // java.lang.NoClassDefFoundError
         } catch(ClassNotFoundException e) {
            // it does not exist on the classpath
        	 System.out.println( "WebdavVFS2file Does NOT EXIST ***************");
        	 System.out.println( "ClassNotFoundException "+e);
         } catch(NoClassDefFoundError e2) {
        	 // it does not exist on the classpath
        	 System.out.println( "WebdavVFS2file Does NOT EXIST ***************");
        	 System.out.println( "NoClassDefFoundError "+e2);
         }
         ******/
        
        if (outputFile.startsWith("webdav") || outputFile.startsWith("http") ||
        		outputFile.startsWith("https")) {
        	isWebdav = true;
        	if (debug) {
        		System.out.println( "isWebdav is true outputFile: "+outputFile);
        	}
        	
        	/**** all of this should be done on the java command line
        	System.out.println("setProperty javax.net.ssl.trustStore");
            System.setProperty("javax.net.ssl.trustStore","/Volumes/bigraid/PDS4/FY2013/webdav/certs/miplapps3");
           
            System.setProperty("javax.net.ssl.keyStoreType","pkcs12");
            // System.setProperty("javax.net.ssl.keyStore","/etc/httpd/certs/client/client1.P12");
            System.setProperty("javax.net.ssl.keyStore","/Volumes/bigraid/PDS4/FY2013/webdav/certs/miplapps3/client3.P12");
            System.setProperty("javax.net.ssl.keyStorePassword","miplapps");
            System.setProperty("javax.net.ssl.trustStoreType","jks");
            
            System.setProperty("javax.net.ssl.trustStorePassword","miplapps");
            ****/
            
        	// check if this class exists
            try {
            	webdavFileOut = new WebdavVFS2file(outputFile, false);
            	webdavOutputStream = webdavFileOut.getOutputStream();
            } catch (NullPointerException npe) {
            	System.out.println( "NullPointerException "+npe);
            	
            } 
        	
        	if (webdavOutputStream == null) {
        		System.out.println( "webdavOutputStream == null. "+outputFile);  		
        	}
        	
        	// FileContent fc = webdavFileOut.getFileContent();
        	// long size = fc.getSize();
        	// on webdavOutputStream
        	// set content to 0, write flush, close, reopen
        	
        } else {   
        	/***
        	 * Changed from using fout as a File to creating a FileOutputStream
        	 * FileOutputStream will truncate the file before writing.
        	 * When we used File to write over an existing file, the final file would 
        	 * be the length of the original file if it was longer than the new data.
			 * It wasn't truncating properly.
			 * **********************************/
        	// fout = new File(outputFile);
        	fout_x = new File(outputFile);
        	try {
				fout = new FileOutputStream(fout_x, false);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				System.out.println( "jConvertIIO.processSave() FileNotFoundException " +e );
				e.printStackTrace();
			}
        	// fout.length()
        	outputFileName = fout_x.getName(); // just the last part of the name        	
        }
        
        // this doesn't read the input file. It is only getting the filename
        // iioImage already has the data from the input file. It may have been filtered also.
        fin = new File(inputFile);
    	inputFileName = fin.getName(); // just the last part of the name
    	// is this available somewhere else??
    	// set this whem the file is opened. Then it will also work for URL's (webdav,https)
        
        if (debug) {
        	System.out.println( "jConvertIIO.processSave() in file: "+fin +"  "+inputFile+ "  "+inputFileName);
        	System.out.println( "jConvertIIO.processSave() outfile: "+fout +"  "+outputFile+ "  "+outputFileName);
        }
        ImageOutputStream ios = null;
        FileOutputStream fos = null;
        
        VicarBinaryHeader vicarBinaryHeader = null;
        
        ImageWriteParam writeParam = writer.getDefaultWriteParam();
        if (debug) {
        	System.out.println( "jConvertIIO.processSave() writeParam: "+writeParam);
        	System.out.println( "processSave - format="+format);
        }
        // look at the specific Type of the ImageWriteParam and output format
        // fill in any ImageWriteParams specific to a particular image format
        // put this before the transcoder, use to controll transcoder ????
        if (format.equalsIgnoreCase("vicar") == true || format.equalsIgnoreCase("vic") == true ) {
        	// placeholder, put any Vicar specific things into the ImageWriteParam
        	if (debug) System.out.println( "processSave vicar");
        	
			if (debug) {
							System.out.println( "processSave vicar - format="+format);
							System.out.println( "xslFileName ="+ xslFileName);
						}
						
			/*******
			if (writeParam instanceof PDSImageWriteParam) { 
				IIOMetadata inData = iioImage.getMetadata();
				if (inData instanceof VicarMetadata) {
				// get the VicarLabel from the input file
				VicarLabel readerVicarLabel = ((VicarMetadata)  inData).getVicarLabel() ;
				((PDSImageWriteParam)  writeParam).setVicarLabel(readerVicarLabel) ;
				}
            *******/	
			if (useOutputFilename || pdsDetachedOnly ) {
				((PDSImageWriteParam)  writeParam).setOutputFileName(outputFileName);
				((PDSImageWriteParam)  writeParam).setInputFileName(inputFileName);
			}
			
			//((PDSImageWriteParam)  writeParam).setOutputFileName(outputFileName);
			
			((PDSImageWriteParam)  writeParam).setDebug(debug);
			((PDSImageWriteParam)  writeParam).setOutputXML(outputXML)	 ;
			
                 
			((PDSImageWriteParam)  writeParam).setDirty(imageHasBeenProcessed);
			// this tells the writer to transcode the imageMetadata
			// this will be true later based on a flag passed in 
			// also waiting for implementation of deault xsl from the jar file
			((PDSImageWriteParam)writeParam).setTranscodeIIOmetadata(true);
            	
			// the transcoding will use this xsl file
			((PDSImageWriteParam)writeParam).setXslFileName(xslFileName);
			
			((PDSImageWriteParam)  writeParam).setReaderFormat(readerFormat) ;
            
            // not for vicar	
			((PDSImageWriteParam)  writeParam).setEmbedVicarLabel(embedVicarLabel) ;    
			((PDSImageWriteParam)writeParam).setAddMerItems(addMerItems);
			((PDSImageWriteParam)writeParam).setAddStatistics(addStatistics);
			((PDSImageWriteParam)writeParam).setCalculateStatistics(calculateStatistics);
			
			((PDSImageWriteParam)writeParam).setAddLinePrefix(keep_line_prefix);
			
			((PDSImageWriteParam)writeParam).setPdsLabelType(pdsLabelType);
			((PDSImageWriteParam)writeParam).setAddBinaryHeader(addBinaryHeader);
			
			
			// add something to allow, force or disable native
         }
		else if (format.equalsIgnoreCase("tif") == true || format.equalsIgnoreCase("tiff") == true) {
					// put tif specific things into the ImageWriteParam
			
			int tilingMode = writeParam.getTilingMode();
			
			
			if (debug) {
				System.out.println( "processSave tif ************************************");
			System.out.println( "tilingMode "+ tilingMode);
			System.out.println( "  MODE_DISABLED "+ImageWriteParam.MODE_DISABLED);
			System.out.println( "  MODE_DEFAULT "+ImageWriteParam.MODE_DEFAULT);
			System.out.println( "  MODE_EXPLICIT "+ImageWriteParam.MODE_EXPLICIT);
			System.out.println( "  MODE_COPY_FROM_METADATA "+ImageWriteParam.MODE_COPY_FROM_METADATA);
			System.out.println( "canWriteTiles "+ writeParam.canWriteTiles());
			System.out.println( "tifTileSizeX="+tifTileSizeX+"  tifTileSizeY="+tifTileSizeY);
			}
			writeParam.setTilingMode(ImageWriteParam.MODE_EXPLICIT);
			
			tilingMode = writeParam.getTilingMode();
			if (debug) {
				System.out.println( "set tilingMode "+ tilingMode);
				}
			
			RenderedImage ri = iioImage.getRenderedImage();
			// get sizes from input image to set tiling
			int h = ri.getHeight();
			
			int w = ri.getWidth();
			if (debug) {
				System.out.println( "image size "+w+"x"+h);
				}
			// calculate an optimal tile height for writing
			// same as the input file??
			
			int tileWidth = w;
			int tileHeight = 100;
			
			/** try 256 x 256 to enable reader to naturally use 256x256 **/
			if (tifTileSizeX != 0 && tifTileSizeY != 0) {
				tileWidth = tifTileSizeX;
				tileHeight = tifTileSizeY;
			}
			if (debug) {
				System.out.println( "tifTileSizeX="+tifTileSizeX+"  tifTileSizeY="+tifTileSizeY);
				System.out.println( "Tilewidth="+tileWidth+"  tileHeight="+tileHeight);
			}
			
			int tileGridXoffset = 0;
			int tileGridYoffset = 0;
			writeParam.setTiling(tileWidth, tileHeight, tileGridXoffset, tileGridYoffset );
			
			int tileW = writeParam.getTileWidth();
			int tileH = writeParam.getTileHeight();
			writeParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
			
			if (debug) {
				System.out.println( "tileWidth="+ tileW+"   tileHeight="+tileH);			
				System.out.println("writeParam.canWriteCompressed "+writeParam.canWriteCompressed());
			
				String[] compTypes = writeParam.getCompressionTypes();
				if (compTypes != null ) {				
					int len = compTypes.length;
					for (int i = 0; i< len ; i++) {						
						System.out.println( i+") compressionTypes "+compTypes[i]);		
						}
					}
				else {
					System.out.println( "compTypes is NULL");
				}
				
				System.out.println("writeParam.getCompressionType() "+writeParam.getCompressionType());
								
				Dimension[] tileSizes = writeParam.getPreferredTileSizes();
			
				if (tileSizes != null ) {			
					int len = tileSizes.length;
					for (int i = 0; i< len ; i++) {
						Dimension d = tileSizes[i];
						System.out.println( i+") TileSizes Dimension "+d.getWidth()+" x "+d.getHeight());			
						}
					}
					else {
						System.out.println( "TileSizes Dimension is NULL");
					}
				} // end of debug printing
		
					//			set the compression Type
			
					 // writeParam.setCompressionType(compTypes[0]);
					 // System.out.println("writeParam.getCompressionType() "+writeParam.getCompressionType());
					 // disable compression
					 // System.out.println("compression DISABLED");
					 // writeParam.setCompressionMode(ImageWriteParam.MODE_DISABLED);
			}
				
      	// else if (format.equalsIgnoreCase("pds") == true  ) {
		else if (format.toLowerCase().startsWith("pds") == true  ) {
			// pds and pds4
      		// add ODL too?
        
        	if (debug) {
        		System.out.println( "processSave pds - format="+format+"   ++++++++++++++++++++++++++");
        		System.out.println( "embedVicarLabel "+embedVicarLabel);       		
        		
        		System.out.println( "inputFileName "+inputFileName);
        		System.out.println( "readerFormat = "+readerFormat);
        		System.out.println( "useOutputFilename "+useOutputFilename);
        		System.out.println( "outputFileName "+outputFileName);
        		System.out.println( "outputFormat = "+outputFormat);
        		System.out.println( "imageHasBeenProcessed "+imageHasBeenProcessed);       		
        		System.out.println( "xslFileName ="+ xslFileName);
        		System.out.println( "pdsDetachedLabel = "+pdsDetachedLabel);
        		System.out.println( "pdsDetachedOnly = "+pdsDetachedOnly);
        		System.out.println( "usePIRL = "+usePIRL);
        		System.out.println( "readerFormat = "+readerFormat);
        		System.out.println( "addBLOB = "+addBLOB);				
        		
        		System.out.println( "addBinaryHeader = "+addBinaryHeader);
        		System.out.println( "fakeImage = "+fakeImage);
        		System.out.println( "writeParam = "+writeParam);
        		System.out.println( "pdsLabelType = "+pdsLabelType);
        		System.out.println( "velocityTemplateFilename = "+velocityTemplateFilename);
        		System.out.println( "velocityConfigPath = "+velocityConfigPath);
        		
        	}
        	
        	// change these defaults to tell the writer it must figure them out from the data??
        	int recordLength = 0;
			int vicarLabelRecordCt = 0;
			int vicarFileRecordCt = 0;
			int front_label_size = 0;
			
        	if (writeParam instanceof PDSImageWriteParam) { 
        		((PDSImageWriteParam)  writeParam).setReaderFormat(readerFormat) ;
            	IIOMetadata inData = iioImage.getMetadata();
            	if (inData instanceof VicarMetadata) {
            		// get the VicarLabel from the input file
            		VicarLabel readerVicarLabel = ((VicarMetadata)  inData).getVicarLabel() ;
            		vicarBinaryHeader = ((VicarMetadata)  inData).getVicarBinaryHeader();
            		// readerVicarLabel.
            		if (debug) System.out.println("VicarBinaryHeader "+vicarBinaryHeader);
            		// put this into the PDSMetadata object after we transcode
            		// ((PDSImageWriteParam)  writeParam).setVicarBinaryHeader(vicarBinaryHeader);
					           		
            		((PDSImageWriteParam)  writeParam).setVicarLabel(readerVicarLabel) ;
            		
            		// get these out of the vicar label
            		recordLength = ((VicarMetadata)  inData).getRecord_length();
					vicarLabelRecordCt = ((VicarMetadata)  inData).getLabelRecordCount();
					front_label_size = ((VicarMetadata)  inData).getFront_label_size();
					vicarFileRecordCt = ((VicarMetadata) inData).getFileRecordCount();
					
					
					((PDSImageWriteParam)  writeParam).setVicarPixelSize(((VicarMetadata) inData).getVicarPixelSize());
					((PDSImageWriteParam)  writeParam).setVicarIntFmt(((VicarMetadata) inData).getVicarIntFmt());
					((PDSImageWriteParam)  writeParam).setVicarFormat(((VicarMetadata) inData).getVicarFormat());
					((PDSImageWriteParam)  writeParam).setVicarRealFmt(((VicarMetadata) inData).getVicarRealFmt());
					((PDSImageWriteParam)  writeParam).setImageStartByte(front_label_size);
					
					if (debug) {
		        		System.out.println( "inData instanceof VicarMetadata");
		        		System.out.println( "  recordLength = "+recordLength);
		        		System.out.println( "   vicarLabelRecordCt = "+vicarLabelRecordCt);
		        		System.out.println( "   front_label_size = "+front_label_size);
		        		System.out.println( "   vicarFileRecordCt = "+vicarFileRecordCt);
		        	}
            	}
            	
            	if (inData instanceof PDSMetadata) {
            		front_label_size = ((PDSMetadata)  inData).getFront_label_size();
            		if (debug)  {
            			System.out.println("inData instanceof PDSMetadata");
            			System.out.println("front_label_size "+front_label_size);
            		}
            			// ((PDSMetadata)  inData).getImageStartByte()_;
            		
            	} else {
            		if (debug) System.out.println("inData instanceof "+inData);
            		// may need to set some values here for not VICAR or PDS inputs
            		// ((PDSImageWriteParam)  writeParam).setImageStartByte(front_label_size);
        		}
            	
            	if (debug) {
	        		System.out.println( "jConvertIIO.processSave() ");
	        		System.out.println( "  fakeImage = "+fakeImage);
            	}
            	
            	((PDSImageWriteParam)  writeParam).setFakeImage(fakeImage);
            	
            	if (useOutputFilename) {
            		((PDSImageWriteParam)  writeParam).setOutputFileName(outputFileName);
            		// fname is just the filename, no path.
            		// outputFileName is the full path
            	}
                ((PDSImageWriteParam)  writeParam).setDebug(debug);
                ((PDSImageWriteParam)  writeParam).setOutputXML(outputXML)	 ;
                ((PDSImageWriteParam)  writeParam).setEmbedVicarLabel(embedVicarLabel) ;      	
                 
                ((PDSImageWriteParam)  writeParam).setDirty(imageHasBeenProcessed);
            	// this tells the writer to transcode the imageMetadata
            	// this will be true later based on a flag passed in 
            	// also waiting for implementation of deault xsl from the jar file
            	((PDSImageWriteParam)writeParam).setTranscodeIIOmetadata(true);
            	
            	// the transcoding will use this xsl file
            	 ((PDSImageWriteParam)writeParam).setXslFileName(xslFileName);
            	 
            	 ((PDSImageWriteParam)writeParam).setAddMerItems(addMerItems);
            	 ((PDSImageWriteParam)writeParam).setAddStatistics(addStatistics);
    			 ((PDSImageWriteParam)writeParam).setCalculateStatistics(calculateStatistics);
            	 
				((PDSImageWriteParam)writeParam).setAddLinePrefix(keep_line_prefix);
				
				((PDSImageWriteParam)writeParam).setPds_ptr(pds_ptr); 
				
				// ((PDSImageWriteParam)writeParam).setDebug(true);
				((PDSImageWriteParam)writeParam).setDebug(debug);
				((PDSImageWriteParam)writeParam).setAddBLOB(addBLOB);				
				((PDSImageWriteParam)writeParam).setPdsLabelType(pdsLabelType);
				((PDSImageWriteParam)writeParam).setAddBinaryHeader(addBinaryHeader);
				
				((PDSImageWriteParam)writeParam).setReaderFormat(readerFormat);
				((PDSImageWriteParam)writeParam).setImageStartByte(front_label_size);
				// PDS4 writer values
				((PDSImageWriteParam)writeParam).setVelocityTemplateFilename(velocityTemplateFilename);
				((PDSImageWriteParam)writeParam).setVelocityConfigPath(velocityConfigPath);
				
				
				ImageInputStream iis = (ImageInputStream) reader.getInput() ;				
				((PDSImageWriteParam)writeParam).setImageInputStream(iis);
				
				
				// find the binaryHeader.
				// add it to the image writeparam
				// better yet -> PDSMetadata.setVicarBinaryHeader
				
				/*** pds_detached_label
//				 PDS detached label support
				boolean detachedLabel = false;
				boolean detachedLabelOnly = false;
				boolean dataFileIsVicarImage = false;
				String dataFilename = null;
				String detachedLabelFilename = null;
				boolean usePIRL = false;
				***/
				((PDSImageWriteParam)writeParam).setDetachedLabel( pdsDetachedLabel);
				((PDSImageWriteParam)writeParam).setDetachedLabelOnly( pdsDetachedOnly);
				
				if (pdsDetachedOnly && ( readerFormat.equalsIgnoreCase("vicar") || readerFormat.equalsIgnoreCase("vic") )) {					
					// get these values from VicarMetadata
					((PDSImageWriteParam)writeParam).setRecordLength(recordLength);
					((PDSImageWriteParam)writeParam).setVicarLabelRecordCt(vicarLabelRecordCt);
					// front_label_size
					((PDSImageWriteParam)writeParam).setVicarImageFileRecords(vicarFileRecordCt);
					((PDSImageWriteParam)writeParam).setDataFileIsVicarImage(true);
					
					if (debug) {
		        		System.out.println( "pdsDetachedOnly && readerFormat is vicar or pds "+readerFormat);
					}
					// THE data type of the input image must be passed along so the PDS
					// label identifies it correctly
					// pixelSize 
					// Format
					// intFmt
					// realFmt
					
					if (useOutputFilename) {
					    ((PDSImageWriteParam)writeParam).setOutputFileName(outputFileName);
					}
					
					((PDSImageWriteParam)  writeParam).setOutputFileName(outputFileName);
					
					
				} else if (pdsDetachedOnly && readerFormat.equalsIgnoreCase("pds") ) {
					
					// need to add new stuff ?? 
					((PDSImageWriteParam)  writeParam).setOutputFileName(outputFileName);
					((PDSImageWriteParam)  writeParam).setInputFileName(inputFileName);
					
					// is this used? all this may be in the xml metadata already
					((PDSImageWriteParam)writeParam).setRecordLength(recordLength);
					((PDSImageWriteParam)writeParam).setVicarLabelRecordCt(vicarLabelRecordCt);
					// front_label_size
					((PDSImageWriteParam)writeParam).setVicarImageFileRecords(vicarFileRecordCt);
					((PDSImageWriteParam)writeParam).setDataFileIsVicarImage(false);
					
					
				}
				
				
				((PDSImageWriteParam)writeParam).setInputFileName( inputFileName);
				// inputFileName is the filename with NO PATH. inputFile, outputFile are full paths 
				// (may be relative) this is the inp= and out= values from the command line
				((PDSImageWriteParam)writeParam).setUsePIRL( usePIRL);
				
				
        		} 
        		// add other write params which aren't PDS specific here
            }
            // add else for other formats here
        
        if (debug) {
    		System.out.println( "**********************************************************************************");
    		System.out.println( "jConvertIIO.processSave() done adding to writeParam ");
    		// System.out.println( "((PDSImageWriteParam)writeParam) "+ ((PDSImageWriteParam)writeParam));
    		System.out.println( "writeParam: "+ writeParam);
			}
        
        // ----------------------------------------------
        if (useWriteParam && writeParam != null) {
            IIOParamController paramController = writeParam.getController();
            // IIOParamController paramController = writeParam.getDefaultController();
            if (paramController != null) {
                // might add the Tree Viewer for Metadata to the paramController
                paramController.activate(writeParam);
            }
            
        }
        
		if (debug) {
		
		  System.out.println( "jConvertIIO.processSave() before transcoder **************");
		  System.out.println( " reader "+reader);
		  System.out.println( " writer "+writer);
		}
       
        ImageTranscoder transcoder = null;
        IIOMetadata transcodedMetadata = null; 
         //  locate a transcoder 
         
         Iterator transcoders = ImageIO.getImageTranscoders(reader, writer);
         // check for case where reader and writer are for the same format, then pass along the metadata from the reader
         // see if we got anything
		if (debug) System.out.println( "jConvertIIO.processSave() transcoders "+transcoders.toString());
         if (transcoders.hasNext()) {
         	transcoder = (ImageTranscoder) transcoders.next();
         	if (debug) System.out.println( "jConvertIIO.processSave() transcoder: "+transcoder);
         	/***
         	 * use the transcoder to convert whatever metadata we have
         	 * The transcoded metadata is substituted into the IIOImage passed to the writer
         	 * **/
         	 RenderedImage ri = iioImage.getRenderedImage();
        	List thumbList = iioImage.getThumbnails();
        		// ignore for now, eventually we MAY use the thumbnails
        
        		
         	 /**
         	  * construct ImageTypeSpecifier
         	 * 
         	 SampleModel  sampleModel = ri.getSampleModel();
            ColorModel colorModel = ri.getColorModel();
         	ImageTypeSpecifier imageType = new ImageTypeSpecifier(colorModel, sampleModel);
         	Some transcoders may use it
         	***/
         	IIOMetadata inData = iioImage.getMetadata();
         	if (debug) {
         		System.out.println( "jConvertIIO.processSave() inData = "+inData);
         		System.out.println( "jConvertIIO.processSave() iioImage = "+iioImage);
         	}
         	ImageTypeSpecifier imageType = new ImageTypeSpecifier(ri); // better ???
         	 transcodedMetadata = transcoder.convertImageMetadata(inData, imageType, writeParam);
         	 // put this into a new iioImage that will be sent to the writer
         	 if (transcodedMetadata != null) {
         	 	// or do we set it no matter what ???
				if (debug) System.out.println( "jConvertIIO.processSave() set transcodedMetadata to iioImage" );
         		 iioImage.setMetadata(transcodedMetadata); 
         		 // now just keep passing this along
         		 if (transcodedMetadata instanceof PDSMetadata) {
         			((PDSMetadata) transcodedMetadata).setVicarBinaryHeader(vicarBinaryHeader);
         			 
         		 }
         	 }    
         }
        
        
        
   try { 
        
      /**
      * the vicar and pds writers use VicarIO which prefers FileOutputStream
      * other writers use ImageOutputStream
      * in the future we must make vicar and pds use ImageOutputStream to be 
      * consistent. Then other apps will be able to blindly call our writers
      **/
          
           // any other format will be here
           if (debug) System.out.println( "Image2File write to "+format+" file");
           
           /*
            * this is how it SHOULD be done,
            * however VicarOutputFile and PDSOutputFile must be modfied to accept ImageOutputStream
            * which is a 1.4 only construct. To mainatin 1.3 comapatabilty NOT YET
            * for now we'll use the code below which differs for pds or vicar formats
            // fos = new FileOutputStream(f);
             * **/
            
            
            
            // -------------- add support for standard out so the output could be piped
            
            if (fout != null && fout.equals("-")) {
            	// OutputStream os = System.out;
				// writer.setOutput( os );
				writer.setOutput( System.out );
				// first null is streamMetadata, might check for it in general case, most readers WILL return NULL
				// // writer.write(null, iioImage, writeParam);
				if (debug) System.out.println( "write iioImage "+iioImage);
				if (format.equalsIgnoreCase("vicar") == true || format.equalsIgnoreCase("pds") == true ||
					format.equalsIgnoreCase("tif") == true  || format.equalsIgnoreCase("tiff") == true ||
				 	format.equalsIgnoreCase("vic") == true) {
					writer.write(null, iioImage, writeParam); // PDS NEEDS WriteParam
				}
				else {
					writer.write(iioImage); // don't bother with WriteParam now
				}
				//		can we close System.out ??
				// os.close(); // writer does NOT close the stream 
            	
            } else {
            	
            	if (isWebdav == true) {
            		if (debug) System.out.println( "write isWebdav is TRUE "+webdavOutputStream);
            		// foutWebdav
            		if (webdavOutputStream == null) {
            			System.out.println( "Could not obtain valid webdavOutputStream, Exiting");
            			System.exit(1);
            		}
            		ios = ImageIO.createImageOutputStream(webdavOutputStream);
            		if (debug) System.out.println( "write isWebdav ios "+ios);
            	} else {
            		if (debug) {
            			System.out.println( "write isWebdav is FALSE "+fout);
            			// System.out.println( "fout.length() "+fout.length()+ " ");
            			System.out.println( "fout.getChannel().size() "+fout.getChannel().size()+ " ");
            		}
            		
            		
            		ios = ImageIO.createImageOutputStream(fout);
            	}
             // check if ios is null
            // ios.
            if (debug) {
            	System.out.println( "#############################################");
            	System.out.println( "ProcessSave() Before write ios.length() "+ios.length()+"  ios.getStreamPosition() "+ios.getStreamPosition());
            	ios.seek(0);
            	ios.flush();
            	// ios.getStreamPosition();
            	System.out.println( "ProcessSave() Before write ios.length() "+ios.length()+"  ios.getStreamPosition() "+ios.getStreamPosition());
            }
           	 
             writer.setOutput( ios);
             // first null is streamMetadata, might check for it in general case, most readers WILL return NULL
             // // writer.write(null, iioImage, writeParam);
             if (debug) System.out.println( "write iioImage "+iioImage);
             if (format.equalsIgnoreCase("vicar") == true || format.equalsIgnoreCase("pds") == true ||
			 	 format.equalsIgnoreCase("tif") == true || format.equalsIgnoreCase("tiff") == true ||
			 	 format.equalsIgnoreCase("vic") == true || format.equalsIgnoreCase("pds4") == true||
            	 format.toLowerCase().startsWith("pds") == true ) {
			 	 	
			 	 	
				if (debug) {
					System.out.println( "write using writeParam "+format+"  **************  writeParam "+writeParam);
					if (writer instanceof PDSImageWriter) {
						((PDSImageWriter) writer).setDebug(debug);
					}
				}
            	writer.write(null, iioImage, writeParam); // PDS NEEDS WriteParam
             } else if (format.equalsIgnoreCase("jpeg2000") == true || format.equalsIgnoreCase("jpeg 2000") == true) {
            	 IIOMetadata im = iioImage.getMetadata();
            	 if (debug) {
 					System.out.println( "write jpeg2000 using writeParam "+format+"  **************  writeParam "+writeParam);
 					System.out.println( "IIOMetadata "+im);
            	 }
            	 // if (im != null && im instanceof com.sun.media.imageio.plugins.jpeg2000.J2KMetadata) {
            	// 	 writer.write(null, iioImage, writeParam); // don't bother with WriteParam now
            	 iioImage.setMetadata(null);
            	 writer.write(null, iioImage, writeParam); // don't bother with WriteParam now
            	 
             }
             else {
            	writer.write(iioImage); // don't bother with WriteParam now
             }
             
             if (debug) {
            	 System.out.println( "processSave() after write");
            	 System.out.println( "ios.getStreamPosition() "+ios.getStreamPosition());
            	 System.out.println( "ios.getFlushedPosition() "+ios.getFlushedPosition());
             }
             
             ios.flush();
             if (debug) {
            	 System.out.println( "processSave() after flush");
            	 System.out.println( "ios.getStreamPosition() "+ios.getStreamPosition());
            	 System.out.println( "ios.getFlushedPosition() "+ios.getFlushedPosition());
             }
             
             ios.close(); // writer does NOT close the stream 
             if (debug) {
            	 System.out.println( "processSave() after close");
            	 System.out.println( "ios.length() "+ios.length());
            	 System.out.println( "ios.getFlushedPosition() "+ios.getFlushedPosition());
             }
             
             if  (fileWriter!=null) {
            	 if (debug) System.out.println( "processSave() fileWriter.close();");
            	  fileWriter.close();
                  }
             if (webdavOutputStream != null) {
            	 webdavOutputStream.close();
             }
   			}
            
           
           /**
           if (format.equals("vicar") == true || format.equals("pds") == true  ) {
           	   fos = new FileOutputStream(f);
          	  writer.setOutput(fos); // do I need to cast to be able to set fos ???
          	  writer.write(null, iioImage, writeParam);
          	  fos.close();
           }
           else {
           	ios = ImageIO.createImageOutputStream(f);
            writer.setOutput( ios);
            // first null is streamMetadata, might check for it in general case, most readers WILL return NULL
            writer.write(null, iioImage, writeParam);
            // writer.write(iioImage);
            ios.close(); // writer does NOT close the stream 
           }
           ***/
       
       // (allowing for multiple writes to a stream)
       if (debug) System.out.println( "Image2File write completed ");
      }
      catch (IIOException iioe) {
	    System.out.println("IIOException in Image2File : "+iioe);
	    iioe.printStackTrace();
	  } 
	  catch (IOException ioe) {
	    System.out.println("IOException in Image2File : "+ioe);
	    ioe.printStackTrace();
	  } 
	  
    } 
    
    /**
     * adds JAI RenederedOps to the image. The Operators are controlled from 
     * the command line arguments. By default this method does nothing.
     * 
     * @param image
     * @return RenderedImage
     */
    public RenderedImage processFilters(RenderedImage image) {
        
        if (debug) System.out.println("processFilters clampImage "+clampImage+"  scaleImage "
        		+scaleImage+"  cropImage "+cropImage);
        		
         RenderedImage temp = image;
         
         RenderedImage cropped = null;
         RenderedImage scaled = null;
         RenderedImage trans = null;
         RenderedImage clamped = null;
         
         SampleModel sm = image .getSampleModel();
         int bands = sm.getNumBands();
         
         // should I add a command line argument to do this or try to be helpful ???
         // if (bands == 1 && 
         //	( outputFormat.equalsIgnoreCase("jpg") || outputFormat.equalsIgnoreCase("jpeg") ||
         //	  outputFormat.startsWith("jpeg") || outputFormat.startsWith("JPEG"))) {
		// if (bands > 3 && ( outputFormat.equalsIgnoreCase("jpg") ||
		//			 outputFormat.equalsIgnoreCase("jpeg"))) {	
		if (bandSelect == true) {
		
			if (debug) {
				System.out.println("creating 3 band image  from "+bands+" bands, use BandSelect **");
				for (int i=0 ; i<bandList.length ; i++) {
					System.out.print(bandList[i]+",");
				}
				System.out.println("");
				
			}
			// int[] bandList = {0,1,2};
			// int[] bandList = {2,4,7};
			ParameterBlockJAI bandSelectPB = new ParameterBlockJAI("BandSelect");
			// bandSelectPB = new ParameterBlock();
			bandSelectPB.addSource(image);
			bandSelectPB.setParameter("bandIndices",bandList);
			// bandSelectPB.add(currentBandList);
			temp = JAI.create("BandSelect", bandSelectPB);
			if (debug) {
				SampleModel smod = temp.getSampleModel();
				int b = smod.getNumBands();
				System.out.println("temp bands = "+b);
				}	
			
			}
         
         if (bands == 1 && singleToRGB){	
         		if (debug) System.out.println("creating 3 band image from 1 band ****************");
         	int[] bandList = {0,0,0};
        		ParameterBlockJAI bandSelectPB = new ParameterBlockJAI("bandselect");
        		// bandSelectPB = new ParameterBlock();
                bandSelectPB.addSource(image);
                bandSelectPB.setParameter("bandIndices",bandList);
                // bandSelectPB.add(currentBandList);
                temp = JAI.create("bandselect", bandSelectPB);	
        		// use BandSelect Operator instead
        	}
        
        
    	if (clampImage) {
    		if (debug) {
    			System.out.println("CLAMP 1 clamp "+clampLow+" - "+clampHigh+"  ");
    			double[][] minmax = getExtrema(temp);
    			System.out.println("CLAMP 1 extrema "+minmax[0][0]+"  "+minmax[1][0]+"  ");
    		}
			ParameterBlock PB=new ParameterBlock();
			PB.addSource(temp).add(clampLow).add(clampHigh);
			clamped = JAI.create("clamp",PB);
			imageHasBeenProcessed = true;
			if (debug) System.out.println("processFilters CLAMP");
			// imageHasBeenFiltered = true;
			temp = clamped;
			if (debug) {
    			double[][] minmax = getExtrema(temp);
    			System.out.println("CLAMP 2 extrema "+minmax[0][0]+"  "+minmax[1][0]+"  ");
    		}
    	}
    
    	if (scaleImage) {
			ParameterBlock PB=new ParameterBlock();
			PB.addSource(temp).add(scaleXfactor).add(scaleYfactor).add(scaleXtrans).add(scaleYtrans);
			// interpolation is 5th argument
			scaled = JAI.create("scale",PB);
			imageHasBeenProcessed = true;
			if (debug) System.out.println("processFilters SCALE");
			temp = scaled;
    	}
      
    
    	if (cropImage) {
    		
    		if (displayImage ) {
    			temp = processFormat(temp, DataBuffer.TYPE_BYTE, true);
    		}
    		
    		if (debug) System.out.println("processFilters CROP "+cropX+","+cropY+"  "+cropWidth+"x"+cropHeight);
			ParameterBlockJAI PBc=new ParameterBlockJAI("crop");
			PBc.setSource(temp,0);
			PBc.setParameter("x", cropX);
			PBc.setParameter("y", cropX);
			PBc.setParameter("width", cropWidth);
			PBc.setParameter("height", cropHeight);
			cropped = JAI.create("crop",PBc);
			
			if (debug) System.out.println("after crop cropped "+cropped);
			if (cropped == null) System.out.println("***** cropped is NULL");
			if (debug) System.out.println("after crop cropped "+cropped.getWidth()+" "+cropped.getHeight());
			
		
			
			temp = cropped;
			
			
			
			
			float x = (float) (cropX*-1.0);
			float y = (float) (cropY*-1.0);
			// float x = (float) (cropX);
			// float y = (float) (cropY);
			// x=0.0F; y=0.0F;
			 
			ParameterBlockJAI PBt=new ParameterBlockJAI("translate");
			PBt.setSource(cropped,0);
			PBt.setParameter("xTrans", x);
			PBt.setParameter("yTrans", y);
			
			// PlanarImage 
			trans = JAI.create("translate",PBt);
			
			imageHasBeenProcessed = true;
			
			ColorModel cm = trans.getColorModel();
			System.out.println("trans ColorModel "+cm);
			
			
			int w = trans.getWidth();
			int h = trans.getHeight();
			if (debug) System.out.println("tiledImage  "+w+"x"+h);
			RenderedImage tiled = new TiledImage(trans, w, h );
			// TiledImage tiled = new TiledImage(trans, false );
			
			temp = tiled;
			
    	}
    /**
    	if (cropImage) {
			ParameterBlock PB=new ParameterBlock();
			System.out.println("processFilters CROP "+cropX+","+cropY+"  "+cropWidth+"x"+cropHeight);
			PB.addSource(temp).add(cropX).add(cropY).add(cropWidth).add(cropHeight);
			temp = JAI.create("crop",PB);
			System.out.println("after crop temp "+temp);
			System.out.println("after crop temp "+temp.getWidth()+" "+temp.getHeight());
			float x = (float) (cropX*-1.0);
			float y = (float) (cropY*-1.0);
			
			ParameterBlock PB2=new ParameterBlock();
			System.out.println("processFilters translate "+x+","+y);
			PB2.addSource(temp).add(x).add(y);
			temp = JAI.create("translate",PB2);
			imageHasBeenProcessed = true;
			
    	}
    	**/
    
    return temp;
    }
    
    /**
     * Get the Extrema(min, Max) for an image
     * @param temp
     * @return double[]
     */
    public double[][] getExtrema(RenderedImage temp) {
    	System.out.println("getExtrema() ");
    	ParameterBlock PB = new ParameterBlock();
		PB.addSource(temp).add(null).add(10).add(10);
		RenderedImage extrema=JAI.create("extrema",PB);

		// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
		double scale[][]=(double[][])extrema.getProperty("extrema");
		
	    System.out.println("extrema min="+scale[0][0]+"  max="+scale[1][0]);
	    	
		return scale;
    }
    
 // convert the input image to a different DataType
    public RenderedImage processFormat(RenderedImage image, int newDataType, boolean rescaleOnFormat) {
        
        // DataBuffer.TYPE_BYTE
        RenderedImage sourceImage = image;
        
        ComponentSampleModel sampleModel = (ComponentSampleModel) image.getSampleModel();
        int oldDataType = sampleModel.getDataType();
        int numbands = sampleModel.getNumBands();
        
        
        // check if (oldDataType == newDataType) return image;
        if (debug) {
        	System.out.println("processFormat "+numbands+" bands   "+oldDataType+ " -> "+newDataType);
        	System.out.println("rescaleOnFormat "+rescaleOnFormat);
        }
        
		   // make a new  SampleModel for the new image data type
		   // get all the stuff we need to know from the old sampleModel
		  int pStride =  sampleModel.getPixelStride();
		  
		  int slStride = sampleModel.getScanlineStride();
		  
		  int[] bandOffsets = sampleModel.getBandOffsets();
		  if (debug) {
			  System.out.println(" *** pStride="+pStride+"  slStride="+slStride+"  bandOffsets="+bandOffsets );
			  System.out.println(" ***   bandOffsets.length="+bandOffsets.length+"  " );
			  for (int i=0 ; i< bandOffsets.length ; i++) {
				  System.out.println("   bandOffsets["+i+"]="+bandOffsets[i] );
			  }
		  }
           //  int w = sampleModel.getWidth();
           // int h = sampleModel.getHeight();
         // ---------------------------------------------------------
         // rescale the pixel values of the image based on the image extrema
         RenderedImage temp = image;
        
     
     /* rescale should apply only if going from a bigger to smaller data type
      * turn it off if going to a wider data type
      * * if it is false here it means the user set it to false and is 
      * requesting NO rescale
      * The default is TRUE
      */   
      
    if (rescaleOnFormat == true) {
         	rescaleOnFormat =  checkRescaleOnFormat(oldDataType, newDataType);
         }
	  
    if (rescaleOnFormat) {
    	if (debug) System.out.println("processFormat rescaleOnFormat is TRUE - rescaling");
		ParameterBlock PB=new ParameterBlock();
		PB.addSource(temp).add(null).add(1).add(1);
		RenderedImage extrema=JAI.create("extrema",PB);

		// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
		double scale[][]=(double[][])extrema.getProperty("extrema");
		// double ceiling=Byte.MAX_VALUE*2; // treat as unsigned
		double ceiling = getMaxForDataType(newDataType) ;
	
	
	
		// double ceiling=Short.MAX_VALUE*2;
		double max=0.0,min=ceiling;
		for(int i=0;i<scale[0].length;i++){
	    	max=Math.max(max,scale[1][i]);
	    	min=Math.min(min,scale[0][i]);
		}
		if (debug) {
			System.out.println("processFormat(1) extrema scale   min="+scale[0][0]+"  max="+scale[1][0]);
			System.out.println("processFormat(1) extrema new ceiling="+ceiling+"  min="+min+"  max="+max);
		}
		// round max up to the nearest power of 2. 
		// max=Math.pow(2.0,Math.round(Math.log(max)/Math.log(2)));
		// min=0;
		
		if (Double.isNaN(min) || Double.isNaN(max)) {
			if (debug) {
				System.out.println("processFormat(1.5) extrema min max are NaN. Using InageStatistics Operator");
			}
		
			// try using the image Statistics - only do this if Extrema returns NaN for min and max
			ImageStatistics imStats = new ImageStatistics(image);
			// check that the statistics have been calculated
			boolean calculated = imStats.getStatisticsCollected();
			int min_i = 0;
			int max_i = 0;
			double max_d = 0.0;
			double min_d = 0.0;
			if (calculated) {
				int bands = imStats.getNumBands();
				int dataType = imStats.getDataType();
				String dataTypeName = imStats.getDataTypeName(dataType);

				if (debug) 
				{  
					System.out.println("*************************************************************");
					System.out.println("****          PDSimageStatistics                        *****");
					System.out.println("*************************************************************");
					System.out.println("-- PDSimageStatistics --- calculated "+calculated+ "  dataType "+dataType+"  dataTypeName "+dataTypeName);
					imStats.printAllValues();
					System.out.println("*************************************************************");
				}

				// replacxe the min, max values
				max = imStats.getMax(0);
				min = imStats.getMin(0);
			}
		}
	
		// this will be for BYTE output
		double constant[] = new double[]{1.0};
		double offset[] = new double[]{0.0};
		
		if (max == 0.0) {
			max = ceiling;
			if (debug) {
				System.out.println("processFormat(1) new max "+max);
			}
		}
	
		if (Double.isNaN(min) || Double.isNaN(max)) {
			System.err.println("Warning... Could not obtain valid min and max values to rescale this image");			
		}
	
		// dst[x][y][b] = src[x][y][b]*constant + offset;
		// offset is added after the value is scaled

		constant[0] = ceiling /(max-min);
		offset[0] = min * constant[0] * -1.0; // offset is added only for unsigned ??
		// 
		
		// why is constant there, only for after??
		// offset[0] = min *  -1.0; // offset is added only for unsigned ??
		// offset[0] = min * -1.0; // offset is added only for unsigned ??
	
		if (debug) {
			System.out.println("processFormat(2) constant="+constant[0]+"  offset="+offset[0]);		
			double min1 = (min * constant[0]) + offset[0];
			double max1 = (max * constant[0]) + offset[0];
			System.out.println("processFormat(3)  min="+min+"  min1="+min1+"  max="+max+"  max1="+max1);
		}
	
		PB=new ParameterBlock();
		// PB.addSource(temp).add(new double[]{ceiling/(max-min)}).add(new double[]{ceiling*min/(min-max)});
		PB.addSource(temp).add(constant).add(offset);
		
		temp=JAI.create("rescale",PB);
    } else {
    	if (debug) System.out.println("processFormat rescaleOnFormat is FALSE - NO rescaling !!!");
    }

   
	if (debug && rescaleOnFormat) {
		// do extrema again after the rescale
	
	
		ParameterBlock PB=new ParameterBlock();
		PB.addSource(temp).add(null).add(1).add(1);
		RenderedImage extrema=JAI.create("extrema",PB);

		// scale all pixels by:   v1= m * v0 +b    (assuming one band per pixel)
		double[][] scale =(double[][])extrema.getProperty("extrema");
		// double ceiling=Short.MAX_VALUE*2;
		double ceiling = getMaxForDataType(newDataType) ;
		double max=1;
		double min=ceiling;
		for(int i=0;i<scale[0].length;i++){
	    	max=Math.max(max,scale[1][i]);
	    	min=Math.min(min,scale[0][i]);
		}
		System.out.println("processFormat(4) new extrema  ceiling "+ceiling+" scale[0][0] "+scale[0][0]+" scale[1][0] "+scale[1][0]);
		System.out.println("processFormat(4) new extrema  min="+min+"  max="+max+"  scale[0].length "+scale[0].length);
	}
	
    
    image = temp;    
        // ---------------------------------------------------------
        // rendering hint with a ImageLayout
        
        
        
        
		ParameterBlock pb = new ParameterBlock();
		pb.addSource(image);
		pb.add(newDataType);
				
		// RenderedImage  formatedImage = JAI.create("format", pb, hints);
		RenderedImage  formatedImage = JAI.create("format", pb);
		
		return (formatedImage);    
    }
    
    /**
     * RescaleOnFormat should only be true if the output format is smaller 
     * than the input format.
     * 
     * @param inDataType
     * @param outDataType
     * @return true if rescale should continue, false if it should not
     */
	public boolean checkRescaleOnFormat(int inDataType, int outDataType) {
    	
    		boolean doRescale = false;
    		
    		if (debug) {
    			System.out.println("checkRescaleOnFormat in="+inDataType+" out="+outDataType);
    		}
    		
			if (inDataType == DataBuffer.TYPE_BYTE) {				
			
				doRescale = false;  // no point to rescaling
				}
			else if (inDataType == DataBuffer.TYPE_SHORT) {
				// how to handle USHORT ?? 
				if (outDataType == DataBuffer.TYPE_BYTE ) {
					doRescale = true;
					}
				else {
					doRescale = false;
					}
				
				}	    
			else if (inDataType == DataBuffer.TYPE_USHORT) {	    	
				if (outDataType == DataBuffer.TYPE_BYTE ||
					outDataType == DataBuffer.TYPE_SHORT ) {
					doRescale = true;
					}
				else {
					doRescale = false;
					}
				}
			else if (inDataType == DataBuffer.TYPE_INT) {
				
				if (outDataType == DataBuffer.TYPE_BYTE ||
					outDataType == DataBuffer.TYPE_SHORT ||
					outDataType == DataBuffer.TYPE_USHORT) {
					doRescale = true;
					}
				else {
					doRescale = false;
					}
				}	    
			else if (inDataType == DataBuffer.TYPE_FLOAT) {
				if (outDataType == DataBuffer.TYPE_BYTE ||
					outDataType == DataBuffer.TYPE_SHORT ||
					outDataType == DataBuffer.TYPE_USHORT ||
					outDataType == DataBuffer.TYPE_INT) {
					doRescale = true;
					}
				else {
					doRescale = false;
					}
				}
			else if (inDataType == DataBuffer.TYPE_DOUBLE) {
				if (outDataType == DataBuffer.TYPE_DOUBLE ) {
					doRescale = false;
					}
				else {
					doRescale = true;
					}
				
				}
		if (debug) {
						System.out.println("checkRescaleOnFormat doRescale="+doRescale);
					}
    		
			return doRescale;
		 }
    /**
     * convenience for using format/rescale operators
     * @param dataType
     * @return double
     */
    public double getMinForDataType(int dataType) {
    	
    	double min = 0.0;
    	if (dataType == DataBuffer.TYPE_BYTE) {
    		// min = Byte.MIN_VALUE;
    		min = 0.0; //used as unsigned
	        }
	    else if (dataType == DataBuffer.TYPE_SHORT) {
	    	min = Short.MIN_VALUE;
	        }	    
	    else if (dataType == DataBuffer.TYPE_USHORT) {	    	
	    	min = 0.0;
	        }
	    else if (dataType == DataBuffer.TYPE_INT) {
	    	min = Integer.MIN_VALUE; // or 0.0 ?? // assume unsigned ???
	        }	    
	    else if (dataType == DataBuffer.TYPE_FLOAT) {
	    	min = Float.MIN_VALUE; 
	        }
	    else if (dataType == DataBuffer.TYPE_DOUBLE) {
	    	min = Double.MIN_VALUE; 
	        }
	    
	    return min;
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
    
    public RenderedImage processBandSelect(RenderedImage image, int[] bandList) {
        
        // DataBuffer.TYPE_BYTE
        RenderedImage ri = null;
        BufferedImage newImage = null;
        BufferedImage bi = null;
        WritableRaster raster = null;
        WritableRaster parentRaster = null;
        ImageTypeSpecifier imageType = null;
        
        ColorModel colorModel = image.getColorModel();
        BandedSampleModel bandedSampleModel = null;
        
        SampleModel sampleModel = image.getSampleModel();
        int numbands = sampleModel.getNumBands();
        System.out.println("processBandSelect sampleModel "+sampleModel);
        System.out.println("numbands "+numbands);
        
        
        if (sampleModel instanceof BandedSampleModel) {
        	System.out.println("sampleModel is a BandedSampleModel"); 
        		bandedSampleModel = (BandedSampleModel) sampleModel;     		
        }
        
        if (image instanceof BufferedImage) {
        	System.out.println("image is a BufferedImage"); 
        		bi = (BufferedImage) image;     
        		raster = bi.getRaster();
        		parentRaster = raster.getWritableParent();     			
        }
        
        if (image instanceof RenderedImage) {
        	System.out.println("image is a RenderedImage"); 
        	// everything is different if this is a RenderedImage	
        	if (numbands > 3) {
        	ParameterBlock pb = new ParameterBlock();
                pb.addSource(image);
                pb.add(bandList);
                ri = JAI.create("bandselect", pb, null);	
        		// use BandSelect Operator instead
        	}
        }
        
        
       
        
       	
        
        
        if (numbands == 1) {
        
        // PlanarImage convertGrayToColor(PlanarImage src, int brightness)
        	PlanarImage pi = null;
        	if (ri != null) {
        	 	pi = new RenderedImageAdapter(ri);
        	}
        	else {
        		pi = new RenderedImageAdapter(image);
        	}
        
        	PlanarImage im = convertGrayToColor(pi, 1);
        	System.out.println("convertGrayToColor PlanarImage pi "+pi+" *************************");
        	return im;
    	}
    	else if (ri != null) {
    		return ri;
    	}
      	else  if (numbands > 3 && bi != null) {
      		if (parentRaster == null) return null;
        
        	numbands = parentRaster.getNumBands();
        	System.out.println("numbands "+numbands);
        	
        	int parentX = parentRaster.getMinX();
         	int parentY = parentRaster.getMinY();
         	int w = parentRaster.getWidth();
         	int h = parentRaster.getHeight();
         	int childMinX = parentX;
         	int childMinY = parentY;
         	
         	
         	// the child should SHARE the parent's raster
         	// we will read data into the parent raster
         	// the BufferedImage will be from the child so a ColorModel can be created
         	WritableRaster newChildRaster = parentRaster.createWritableChild(parentX, parentY, w, h,
         		 childMinX, childMinY, bandList);
         	
         	// is the sampleModel valid ??
         	// SampleModel childSM = new SampleModel();
         	System.out.println("sampleModel "+sampleModel);
         	
         	// olorModel colorModel = ImageCodec.createComponentColorModel(sampleModel);
         	System.out.println("colorModel "+colorModel);
         	
         	int dataType = sampleModel.getDataType();
         	int biType = 0;
         	
         	if (bandList.length == 1) {
         		if (dataType == DataBuffer.TYPE_BYTE) {
	         		biType = BufferedImage.TYPE_BYTE_GRAY;
         		}
	         	else if (dataType == DataBuffer.TYPE_USHORT) {
	         		biType = BufferedImage.TYPE_USHORT_GRAY;
	         	}

	       		// imageType = ImageTypeSpecifier.createFromBufferedImageType(dataType) ;
	            // System.out.println("PDSImageReader.readHeader() imageType "+imageType);
         		
         		// create a new SampleModel
         		// bandedSampleModel.createSubsetSampleModel()
         		// create a new grayscale ColorModel
         		
         		// check biType
         		newImage = new java.awt.image.BufferedImage(w, h, biType);
         		newImage.setData(newChildRaster);
         	}
         	else {
         	/***
         	if (colorModel == null) {
         		int dataType = sampleModel.getDataType();
         		w = sampleModel.getWidth();
         		h = sampleModel.getHeight();
         		int b = 3;
         	
         		BandedSampleModel fakeSampleModel = new BandedSampleModel(dataType, w, h, b);
         		// System.out.println("childSM "+ childSM);
         	
         		// create a bogus colorModel just to get by
         		colorModel = ImageCodec.createComponentColorModel(fakeSampleModel);
         		System.out.println("colorModel (fake) "+colorModel);
         	}
         	**/
         	
         	// childRaster
         	newImage = new java.awt.image.BufferedImage(colorModel, newChildRaster,
                                 colorModel.isAlphaPremultiplied(), new Hashtable());
         	}
        } else {
        	System.out.println("returning image");
        	return image;
        }
        
        if (debug) {
         	System.out.println("newImage "+newImage);
        }  
        		
		return (newImage);    
    }
   
   
    /** produce a 3 band image from a single band gray scale image */
    public static PlanarImage convertGrayToColor(PlanarImage src, int brightness) {
        PlanarImage dst = null;
        double b = (double) brightness;
        double[][] matrix = {
                                { 1.0D, b },
                                { 1.0D, b },
                                { 1.0D, b }
                            };

        if ( src != null ) {
            int nbands = src.getSampleModel().getNumBands();

// MUST check color model here
            if ( nbands == 1 ) {
                ParameterBlock pb = new ParameterBlock();
                pb.addSource(src);
                pb.add(matrix);
                dst = JAI.create("bandcombine", pb, null);
            } else {
                dst = src;
            }
        }

        return dst;
    } 
   
}

