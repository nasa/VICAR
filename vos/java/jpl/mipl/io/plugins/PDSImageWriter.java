/*
 * @(#)PDSImageWriter.java	1.0 00/08/30
 *
  * Steve Levoe JPL/NASA
 */


package jpl.mipl.io.plugins;

import java.util.Iterator;
import java.util.Hashtable;
import java.util.List;
import java.util.Locale;
import java.util.Enumeration;
import java.util.ArrayList;

import javax.imageio.IIOException;
import javax.imageio.IIOImage;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriter;
import javax.imageio.ImageWriteParam;
import javax.imageio.metadata.*;
import javax.imageio.spi.ImageWriterSpi;
import java.awt.image.*;
import java.io.*;
import java.net.URL;

// import VicarIO stuff
// VicarInputFile  SystemLabel
import jpl.mipl.io.streams.*;
import jpl.mipl.io.vicar.*;
import jpl.mipl.io.util.*;

import org.w3c.dom.*;


/**
 * @version 1.0
 * This version includes transcoder capabilities from vicar image files. <br>
 * It might be useful to have transcoders subclass this writer, then it would be able to add
 * the transcoding of new formats and also have access to the writer methods.
 */
public class PDSImageWriter extends ImageWriter {

    
    
    String outputFilename = null;
    String inputFilename = null;
    Node vicarInputDocument = null;
    VicarLabel readerVicarLabel = null;
    boolean embedVicarLabel = false;
    boolean debug = false;
    // boolean debug = true;
    boolean addMerItems = false; 
    boolean addStatistics = false;
    boolean calculateStatistics = false;
    
    boolean detachedLabel = false;
	boolean detachedLabelOnly = false;
	boolean fakeImage = false;
	
	// flag to indicate if FILE_RECORS should be deleted from the output label
	boolean delete_FILE_RECORDS = false;
	
	boolean dataFileIsVicarImage = false;
	String dataFilename = null;
	String detachedLabelFilename = null;
	boolean usePIRL = false;
	
	String pds_ptr = null;
	boolean dirty = true ; // flag to indicate if image data (RenderedImage was changed after read)
	// used to decide if statistics calculated on the input image could be reused
	
	int recordLength = 0;
	int vicarLabelRecordCt = 0;
	int vicarImageFileRecords = 0;
    
    boolean addLinePrefix = false;
    
    boolean addBinaryHeader = false; // flag to control if the embedded vicar label contains a binary header
    
    boolean addBLOB = false; // flag to control if the PDS file contains a vicar binary header as a BLOB
    // this flag also controls creation of the PDS label pointers to the BLOB elements
    
    // this is the output type
    String  pdsLabelType = "PDS3"; // other choices may be ODL,ODL3
    
    String  inputPdsLabelType = ""; // other choices may be MSL_ODL,ODL3
    
    // these are used if the input image is vicar
    String[] PDS_OBJECT_NAME; //  = new String[];
    String[] PDS_OBJECT_LOC;
	String[] PDS_OBJECT_TYPE;
	int[] PDS_OBJECT_PTR;
	int[] PDS_OBJECT_OFFSET;
	int blob_elementCt = 0;
	int blob_elementIndex = 0;
	
	// these are used if the input image is PDS (ODL)
	// specifically for a detached label
    String[] PDS_POINTER_NAME; //  = new String[];
	String[] PDS_POINTER_OFFSET_TYPE; // "byte" or "record" - "record" is the default
	int[] PDS_POINTER_PTR;
	
	int pds_pointer_elementCt = 0;
	int pds_pointer_elementIndex = 0;

	int pds_file_records = 0; // use this only for a detached label
	int pds_record_bytes = 0; // use this only for a detached label
	int pds_label_records = 0; // use this only for a detached label
	
	int vicarLabelBytes = 0;
    
    String readerFormat = "";
    // if we add extention support there will be a Constructor 
    // which takes the Object extention as an adition argument
    public PDSImageWriter(ImageWriterSpi originatingSpi) {
        super (originatingSpi);
    }

    public ImageWriteParam getDefaultWriteParam() {
    	// Locale loc = new Locale();
    	// ImageWriteParam param = new ImageWriteParam(Locale.US);
    	ImageWriteParam param = new PDSImageWriteParam();
    	/***
        ImageWriteParam param = new ImageWriteParam(false, false, false, false, // no tiling
                    null, // passes must equal 1
                    null, // no compression
                    this.getLocale());
        ***/
        if (debug)             System.out.println("PDSImageWriter.getDefaultWriteParam");
      //   param.setController(new JFrame1("PDSImageWriter paramController") );
        return param;
    }

    public boolean canWriteThumbnails() {
        return false;
    }

    public void setOutputFilename(String f) {
        outputFilename = f;
    }

	public void setDebug(boolean d) {
		
		debug = d;
    	System.out.println("PDSImageWriter.setDebug("+d+")   debug="+debug);
	}
	
	// boolean delete_FILE_RECORDS = false;
	public void setDelete_FILE_RECORDS(boolean d) {
		
		delete_FILE_RECORDS = d;
    	if (debug) {
    		System.out.println("PDSImageWriter.setDelete_FILE_RECORDS("+d+") ");
    	}
	}
	
	public boolean getDelete_FILE_RECORDS() {
		
		return delete_FILE_RECORDS;
	}
	
    public IIOMetadata getDefaultStreamMetadata(ImageWriteParam param) {
        return null;
    }
    
    public IIOMetadata
    getDefaultImageMetadata(ImageTypeSpecifier imageType, ImageWriteParam param) {
        return new PDSMetadata();
    }
    
    public IIOMetadata convertStreamMetadata(IIOMetadata inData, ImageWriteParam param) {
        return null;
    }
    
    /**
     * This the heart of the transcoder, outside callers could call this to have a conversion run
     * */
    public IIOMetadata convertImageMetadata(IIOMetadata inData,
        ImageTypeSpecifier imageType,
        ImageWriteParam param) {
        	Node document = null;
        
        // this call is implemented in the VicarToPDStransdcoder
        // we could call that??? nah
            return null;
        
    }
    
    public String docInfo(Node doc) {
	// public String docInfo(Document doc) {
		Class c = doc.getClass();
		String docName = c.getName();
		// System.out.println(
		return docName;
	}
    // -----------------------------------------------------
    
    /**
    * The image is from some other reader or created in memory.
    * Write the image out as PDS.
    * Based on the RenderedImage create a PDSLabel. 
    **/
    public void write(RenderedImage ri) throws IIOException {
        // create a PDSLabel or pass it in as null 
        // let the write method below be responsible for creating a 
        // PDSLabel from just the image
        VicarLabel vl = null;
        write(ri, vl) ;
    }
    
    
    // need
    public void write(IIOImage iioImage, ImageWriteParam writeParam) throws IIOException {
        // do something with the writeParams
      // write(iioImage);
    	IIOMetadata streamMetadata = null;
    	write( streamMetadata, iioImage, writeParam); 
    }
    
     public void write(IIOImage iioImage) throws IIOException {
     	
     	write(null,  iioImage, null);
     }
     
    
    
    public void write( IIOMetadata streamMetadata, IIOImage iioImage, ImageWriteParam  imageWriteParam) throws IIOException {
    	
    	
        if (output == null) {
            throw new IllegalStateException ("Output must be set");
        }
        
        if (debug)    {
        	System.out.println("PDSImageWriter.write(IIOMetadata, IIOImage, ImageWriteParam)");
        	System.out.println("PDSImageWriter.write( ImageWriteParam "+imageWriteParam+")");
        }
         
        boolean doTranscode = false;
        String xslFile = null;
        
        PDSImageWriteParam pdsWriteParam = null;
        if ( imageWriteParam instanceof PDSImageWriteParam) {
        	if (debug)  System.out.println("PDSImageWriter imageWriteParam instanceof PDSImageWriteParam");
        	
        	pdsWriteParam = (PDSImageWriteParam) imageWriteParam ;
        	doTranscode = pdsWriteParam.getTranscodeIIOmetadata();
        	
        	xslFile = pdsWriteParam.getXslFileName();
        	// debug = pdsWriteParam.getDebug();
        	
        	embedVicarLabel = pdsWriteParam.getEmbedVicarLabel();
        	if (embedVicarLabel) {
        		readerVicarLabel = pdsWriteParam.getVicarLabel();
        	}
        	else {
        		readerVicarLabel = null;
        	}
        	
        	addMerItems = pdsWriteParam.getAddMerItems();
    		calculateStatistics = pdsWriteParam.getCalculateStatistics();
    		addStatistics = pdsWriteParam.getAddStatistics();
    		
			addLinePrefix = pdsWriteParam.getAddLinePrefix();
			
			addBinaryHeader = pdsWriteParam.getAddBinaryHeader();			
			addBLOB = pdsWriteParam.getAddBLOB();			
			pdsLabelType = pdsWriteParam.getPdsLabelType() ; // default is "PDS3"; // other choices may be MSL_ODL,ODL3
			
			outputFilename = pdsWriteParam.getOutputFileName();
			inputFilename = pdsWriteParam.getInputFileName();
			readerFormat = pdsWriteParam.getReaderFormat();
			detachedLabel = pdsWriteParam.getDetachedLabel();
			detachedLabelOnly = pdsWriteParam.getDetachedLabelOnly();
			
			dataFileIsVicarImage = pdsWriteParam.getDataFileIsVicarImage();
			dataFilename = pdsWriteParam.getDataFilename();
			// detachedLabelFilename = pdsWriteParam.getD
			usePIRL = pdsWriteParam.getUsePIRL();
			pds_ptr = pdsWriteParam.getPds_ptr();
			recordLength = pdsWriteParam.getRecordLength();
			vicarLabelRecordCt = pdsWriteParam.getVicarLabelRecordCt();
			vicarImageFileRecords = pdsWriteParam.getVicarImageFileRecords();
			
			fakeImage = pdsWriteParam.getFakeImage();
			debug = pdsWriteParam.getDebug();
			
			delete_FILE_RECORDS = pdsWriteParam.getDelete_FILE_RECORDS();
			
			// dirty is true if the image data has changed since it was read from 
			// the input file. If a file is clean (dirty == false) then image statistics 
			// may be reused from the input image
			dirty = pdsWriteParam.getDirty();
        }
        // streamMetadata should be null, ignore even if it isn't since we don't know what to do with it
        if (debug) {
        	System.out.println("PDSImageWriter.write() addBinaryHeader = "+addBinaryHeader);
        	System.out.println("PDSImageWriter.write() addBLOB = "+addBLOB);
        	System.out.println("PDSImageWriter.write() pdsLabelType "+pdsLabelType);
        	System.out.println("PDSImageWriter.write() readerFormat "+readerFormat);
        	System.out.println("PDSImageWriter.write() inputFilename "+inputFilename);
        	System.out.println("PDSImageWriter.write() outputFilename "+outputFilename);
        	System.out.println("PDSImageWriter.write() detachedLabelOnly "+detachedLabelOnly);
        	System.out.println("PDSImageWriter.write() fakeImage "+fakeImage);
        	System.out.println("PDSImageWriter.write() using iioImage -------------------------");
        }
        // look at IIOImage and see if we have enough to write out the image
        RenderedImage ri = iioImage.getRenderedImage();
        List thumbList = iioImage.getThumbnails();
        // ignore for now, eventually we MAY use the thumbnails
        
        IIOMetadata metadata = iioImage.getMetadata();
        /* 3 cases to handle
        * 1) There is metadata and it has a VicarLabel in it.
        * It may have come directly from a reader, it may have built
        * by a program, or it may have come from a transcoder. We don't 
        * care where it came from.
        * 2) There is metadata which does NOT include a VicarLabel.
        * We will build a VicarLabel Object from 1 of 4 places:
        *   a) get basic info out of the RenderedImage
        *   b) get basic info out of the common IIOMetadata format
        *   c) If an ImageWriteParam has been set
        *       use the info there, after checking with the RenderedImage to be
        *       sure it makes sense.
        *   d) combine common IIOMetadata format and ImageWriteParam
        * 3) No metadata at all. Then do a variation on case 2) and create 
        * a VicarLabel Object from some combination of the info from
        * the RenderdImage and any ImageWriteParam
        * 4) streamMetadata must be added in too
        **/
        
        
        /* case 1) We have metadata and it includes an already prepared 
        * VicarLabel Object.
        * This is the simplest HighFidelity case.
        */
        if (metadata instanceof PDSMetadata) {
            if (debug)  System.out.println("PDSImageWriter.write() using PDSMetadata");
            PDSMetadata pm = (PDSMetadata) metadata;
            // VicarLabel vicarLabel = null;
            // readerVicarLabel pass this or set it to use to embed a vicar label
            // vicarLabel = pm.getVicarLabel();
            // write(ri, vicarLabel);
            write(ri, pm);
        } else if (metadata instanceof VicarMetadata) {
        	System.out.println("????????????? we shouldn't get here ????????????????");
            System.out.println("PDSImageWriter.write() using VicarMetadata");
            System.out.println("????????????? we shouldn't get here ????????????????");
            System.out.println("XSL transform of Vicar input must have failed");
            PDSMetadata pm = null;
             
             /***
            VicarMetadata vm = (VicarMetadata) metadata;
            VicarLabel vicarLabel = null;
            if (doTranscode) {
            	DOMutils domUtil = new DOMutils();
            	String xmlFile1 = outputFilename+".1.xml";
            	String xmlFile2 = outputFilename+".2.xml";
            	String xmlFile3 = outputFilename+".3.xml";
            	// use the VicarMetadata and transform it to pdsMetadata using the xsl file
            	String nativeIm = vm.nativeImageMetadataFormatName;
            	Node doc1 =  vm.getAsTree(nativeIm) ;  // getNativeTree();
            	Document doc2 = null;
            	if (doc1 == null) {
            		if (debug) System.out.println("vicarMetadata document returned null *************");
            	}
            	else {
            		if (debug) System.out.println("doc1 "+doc1);
            		domUtil.serializeNode(doc1,xmlFile1,"xml");
            		if (debug) System.out.println("--- transform vicar metadata with "+xslFile);
        			doc2 = domUtil.transformDocument(doc1, xslFile);
        			if (debug) System.out.println("--- transform succeeded");
            	}
        		if (doc2 != null) {
        			domUtil.serializeDocument(doc2, xmlFile2, "xml");
        			if (debug) System.out.println("--- seralize to "+xmlFile2);
        			pm = new PDSMetadata(doc2);
        			if (debug) System.out.println("transform succeeded");
        		}
        		else {
        			if (debug) System.out.println("transform failed, no Document created");
        		}
        		// xslFile 
            }
           ***/
             
           if (debug) System.out.println("write using the renderedImage and the transformed metadata *****************");
            
            
            // write(ri, pm);
            write(ri);
        } else {
        
        /* case 2.a)
        * the image is from some other reader, write the image out as PDS
        * based on the RenderedImage
        * create a PDSLabel 
        **/
        // for now just ignore any metadata which may have been included
        // this case will be identical to ()so we will just call it!!)
        write(ri);
        
        /***
        if (metadata != null) {
            // throw new UnsupportedOperationException ();
        } else { // no metadata, just use the RenderedImage info
        }
        ***/
        }
        

    }
    
    /**
     * Delete all children of this node
     * The parent is keep around in its place in the tree
     * A later merge would be able to go to the right place in the tree
     * 
     * @param node
     * @return
     */
    private boolean deleteChildren(Node node) {
    	boolean success = false;
    	
    	// if (debug) {
    	jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
		
    	String name = node.getNodeName();
    	
    	 if (debug)  {
    	   System.out.println("  %  %  %  %  %  %  %   %  %  %  %  %  %  %  %  %  %  %   %  %  %  %");
    	   System.out.println(" deleteChildren of "+ name);
    	 }
    	
    	NodeList children = node.getChildNodes();
    	Node child ;
    	for (int i = 0 ; i < children.getLength() ; i++) {
    		child = children.item(i);
    		name = child.getNodeName();
    		Hashtable h =  domUtil.getNodeAttributesHash(child );
    		String value = domUtil.getNodeValue(child);
    		if (debug)  {
    		  System.out.println("  "+i+") "+ name+" "+h);
    		  System.out.println("       value "+value);
    		  for (int j=0 ; j < h.size() ; j++) {
    			// System.out.println("  "+h);
    		   }
    		}
    		node.removeChild(child);
    		success = true;
    	}
    	if (debug)  {
    		System.out.println("  %  %  %  %  %  %  %   %  %  %  %  %  %  %  %  %  %  %   %  %  %  %");
    	}
  		
  		  
    	return success;
    }
    
    /* Returns an InputStream if the file name in the input arg is found.in a jar on the classpath.<br>
     * Used by the transcoder to get the default xsl script stored in the jar.<br>
     * The InputStream allows the file to be used like any other opened file.
     * */
     public InputStream InputStreamFromJar(String arg) {
     	if (debug) System.out.println(" InputStreamFromJar: "+arg);
     	// java.lang.ClassLoader
     	// ClassLoader cl = new ClassLoader();
     	// ClassLoader cl = this.getClass().getSystemClassLoader();
     	
     	InputStream is = null;
     	URL url;
     	boolean go = true;
     	
     	url = ClassLoader.getSystemResource(arg);
     	try {
     	is = url.openStream();
     	}
     	catch( IOException ioe) {
     		System.out.println(" InputStreamFromJar: IOException "+ioe);
     	}
     	
     	// if (go) 
     	return is;
     	
     	
     	
 	} 
     /**
      * Delete all children of this node
      * The parent is keep around in its place in the tree
      * A later merge would be able to go to the right place in the tree
      * 
      * @param node
      * @return
      */
     private Node deleteStatisticsChildrenXSL(Node node) {
     	
     	jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
     	Document doc1 = domUtil.getNewDocument();
     	doc1.appendChild(node);
     	Document doc2;
     	
     	// if (debug) {
     	
     	
 		
     	// This should be done with XSL
    	// name of the default xsl script stored in the same jar as this class
     	// just like the transcoder scripts
     	String xslFromJar = "jpl/mipl/io/xsl/removeMerPDSstatistics.xsl"; 
     	// String xslFromJar = "jpl/mipl/io/xsl/VicarToPDSmer1.xsl"; // name of the default xsl script stored
     	//  see VicarToPDSTranscoder.java
     	InputStream xslInputStream = InputStreamFromJar(xslFromJar);
     	doc2 = domUtil.transformDocument(doc1, xslInputStream);
     	
     	Node newNode = doc2.getFirstChild();
     	
     
     	// node.normalize();
     	
     	
     	if (debug) {
     	  System.out.println("  %  %  %  %  %  %  %   %  %  %  %  %  %  %  %  %  %  %   %  %  %  %");
     	  System.out.println(" deleteStatisticsChildren "+doc2);
     	  domUtil.serializeNode(doc1, "stats1.xml", "xml");
     	  domUtil.serializeNode(doc2, "stats2.xml", "xml");
     	 domUtil.serializeNode(newNode, "stats3.xml", "xml");
     	}
     	return newNode;
     }
     
     
    /**
     * Delete all children of this node
     * The parent is keep around in its place in the tree
     * A later merge would be able to go to the right place in the tree
     * 
     * @param node
     * @return
     */
    private boolean deleteStatisticsChildren(Node node) {
    	boolean success = false;
    	
    	
    	jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
			
    	// node.normalize(); // doesn't seem to matter
    	
    	String name = node.getNodeName();
    	if (debug) {
    	  System.out.println("  %  %  %  %  %  %  %   %  %  %  %  %  %  %  %  %  %  %   %  %  %  %");
    	  System.out.println(" deleteStatisticsChildren of "+ name);
    	}
    	
    	String statisticsKeys[] = {"FIRST_LINE","FIRST_LINE_SAMPLE","MISSING_CONSTANT","MEAN",
    			"MINIMUM","MAXIMUM","SAMPLE_BIT_MASK","STANDARD_DEVIATION","MEDIAN","CHECKSUM",
				"INVALID_CONSTANT"};
    	String xPath;
    	for (int j=0 ; j < statisticsKeys.length ; j++) {
    		xPath = "//OBJECT[@name='IMAGE']/item[@key='"+statisticsKeys[j]+"']" ;
			if (debug) System.out.println(" "+j+") xPath "+xPath );
			boolean f = domUtil.deleteNode(node, xPath);
			if (debug) System.out.println(" "+j+") f "+f );
		}
    	// String xPath = "//OBJECT[@name='IMAGE']/item[@key='FIRST_LINE']"; 
		// domUtil.deleteNode(node, xPath);

    	return true;
    	    	
    }
    
    
    
    /**
     * Find and delete a node whose value matches the valueToDelete.
     * 
     * @param inNode - Node to search, could be a Document
     * @param xPath - xPath to find a set of nodes 
     * @param valueToDelete - the value of the node to delete. 
     * The value is tested by a String.contains
     * @return true if a node was found and deleted
     * false if no node was found which matched the xPath and value
     */
    private boolean deleteNodeByValue(Node inNode, String xPath, String valueToDelete) {
    	
		
		boolean includeAttributes = true;
		boolean includeParent = true;
		if (debug) {
		  System.out.println(" ");
		  System.out.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
		  System.out.println(" xPath="+xPath);
		}
		
		jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
		NodeList inList = domUtil.getNodeList(inNode, xPath); 
		
		/**
		 * I can't seem to create the correct xPath expression to 
		 * find an Element with a specific value directly.
		 * Instead  get all xPath nodes.
		 * Loop thru them and find the value of the one we want to remove
		 */
		
		for (int x=0 ;x < inList.getLength() ; x++) {
			Node n = inList.item(x);
			String[] xp = domUtil.getNodeXPath(n,includeAttributes, includeParent);
			String nv = domUtil.getNodeValue(n);
			if (debug) System.out.println(x+") nv="+nv);
			// if (nv.contains(valueToDelete) ) { // String.contains is 1.5 only, 
			if (nv.indexOf(valueToDelete) != -1) {
				if (debug) System.out.println("  MATCH !!!!!!!! "+nv+" delete this node" );
				domUtil.deleteNode(n);
				return true; // node to delete was found
				// exit after first hit
				// could set a flag and loop thru all items
			}
			
			if (debug) {
				for (int k=0; k < xp.length ; k++) {
				
					System.out.println("  xPath "+k+") "+xp[k] );
				}
			}				
		}
		
		// node to delete was NOT found
		return false;
    }
    
    
    /**
     * Find and return a node whose value matches the valueToFind.
     * 
     * @param inNode - Node to search, could be a Document
     * @param xPath - xPath to find a set of nodes 
     * @param valueToDelete - the value of the node to delete. 
     * The value is tested by a String.contains
     * @return true if a node was found and deleted
     * false if no node was found which matched the xPath and value
     */
    private Node getNodeByValue(Node inNode, String xPath, String valueToFind) {
    	
		
		boolean includeAttributes = true;
		boolean includeParent = true;
		if (debug) {
		  System.out.println(" ");
		  System.out.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
		  System.out.println(" xPath="+xPath);
		}
		
		jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
		NodeList inList = domUtil.getNodeList(inNode, xPath); 
		
		/**
		 * I can't seem to create the correct xPath expression to 
		 * find an Element with a specific value directly.
		 * Instead  get all xPath nodes.
		 * Loop thru them and find the value of the one we want to remove
		 */
		
		for (int x=0 ;x < inList.getLength() ; x++) {
			Node n = inList.item(x);
			String[] xp = domUtil.getNodeXPath(n,includeAttributes, includeParent);
			String nv = domUtil.getNodeValue(n);
			if (debug) System.out.println(x+") nv="+nv);
			// if (nv.contains(valueToDelete) ) { // String.contains is 1.5 only, 
			if (nv.indexOf(valueToFind) != -1) {
				if (debug) System.out.println("  MATCH !!!!!!!! "+nv+" delete this node" );
				// domUtil.deleteNode(n);
				return n;
				
				// exit after first hit
				// could set a flag and loop thru all items
			}
			
			if (debug) {
				for (int k=0; k < xp.length ; k++) {
				
					System.out.println("  xPath "+k+") "+xp[k] );
				}
			}				
		}
		
		// node to delete was NOT found
		return null;
    }
    
    
    /**
     * Since this isn't in the ImageWriter API make this a private method.
     * Will be called from another write() method.
     * @param RenderedImage - image to write
     * @param PDSMetadata - metadata to write
     * 
     * @return void
     */
    private void write(RenderedImage ri, PDSMetadata pm) {     
         
        VicarLabel vicarLabel = null;
        if (output == null) {
                throw new IllegalStateException ("Output must be set");
        }
        
        if (debug)  {
        	System.out.println("============= write(RenderedImage ri, PDSMetadata pm)  ==============");
            System.out.println("PDSImageWriter.write() with PDSLabel using vicarIO");
            System.out.println("outputFilename "+outputFilename);
            System.out.println("inputFilename "+inputFilename);
            System.out.println("detachedLabelOnly "+detachedLabelOnly);
            System.out.println("addMerItems "+addMerItems);
            System.out.println("addStatistics "+addStatistics);
            System.out.println("caslculateStatistics "+calculateStatistics);
            System.out.println("pdsLabelType "+pdsLabelType);
            System.out.println("inputPdsLabelType "+inputPdsLabelType);
            System.out.println("readerFormat "+readerFormat);
            System.out.println("fakeImage "+fakeImage);
        }
            
            PDSOutputFile pof = new PDSOutputFile();
                  
            if (pof == null) {
                System.out.println("PDSImageWriter.write(RenderedImage, PDSMetadata) null PDSOutputFile: exiting write");
                // send an IOException ???
                return;
            }

            jpl.mipl.io.util.DOMutils domUtil = new jpl.mipl.io.util.DOMutils();
            
            // boolean debug2 = debug;
            // debug = true;
            // domUtil.setDebug(debug);
            String bltype = null;
			String bintfmt = null;
			String brealfmt = null;
			String bhost = null;
			
			
            
			VicarBinaryLinePrefix vbp = pm.getVicarBinaryLinePrefix();
			int nbb = 0;
			
			if (vbp != null && addLinePrefix == true) {
				nbb = vbp.get_nbb()	;	
				pof.setVicarBinaryLinePrefix(vbp);
				
				// these values will only be used if there is a binary line prefix
				bltype = vbp.get_bltype();
				bintfmt = vbp.get_bintfmt();
				brealfmt = vbp.get_brealfmt();
				bhost = vbp.get_bhost();
				}
			
			
			// this is also in the PDSIMageWriteParam 
			VicarBinaryHeader vbh = pm.getVicarBinaryHeader();
			VicarBinaryLabel vbl = null;
			int nlb = 0;
			if (addBinaryHeader == true && vbh != null) {
				vbl = vbh.getVicarBinaryLabel();
				nlb = vbh.get_nlb();
				// what happens if/when recsize changes ???
				
			}
			
			String blob_bltype = "";
			int blob_recsize =  0;
			int blob_lines = 0;
			int blob_size = 0;
			byte[] blob_data = null;
			
			if (debug) {
				System.out.println("addBLOB is "+addBLOB);
				System.out.println("vbh = "+vbh);
			}
			if (addBLOB == true && vbh != null) {
				vbl = vbh.getVicarBinaryLabel();
				nlb = vbh.get_nlb();
				// what happens if/when recsize changes ???
				
				blob_recsize =  vbh.get_recsize();
				blob_lines = vbh.get_nlb();
				blob_data = vbh.getData();
				
				bltype = vbh.get_bltype();
				// "MSL_CCRMI"
				
				// blob_data = vbl.getBuffer();
				blob_size = vbl.size();
				 if (debug) {
					 System.out.println("===============================================================================");
					 System.out.println("=====               PDSImageWriter.java                     ===================");
					 System.out.println("=====");
					 System.out.println("addBLOB is TRUE");
					 System.out.println("bltype = "+bltype);
					 System.out.println("blob_recsize = "+blob_recsize);
					 System.out.println("blob_lines = "+blob_lines);
					 System.out.println("blob_lines * blob_recsize = "+(blob_lines * blob_recsize));					 
					 System.out.println("blob_size = "+blob_size);
					 System.out.println("pdsLabelType "+pdsLabelType);
					 System.out.println("readerFormat "+readerFormat);
					 
				 }
				
				
			}
			
				
            // pof uses this to write the PDS Label for the image
            if (debug) System.out.println("PDSImageWriter.write() ri "+ri);
            // ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( (BufferedImage) ri );
            // BufferedImage and PlanarImage both implement RenderedImage
            // ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( ri );
		    ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( ri, nbb );
            
            // this class adds the MER items and statistics to the PDS label
            // load it up and set it into ImageToPDS_DOM
            // when ImageToPDS_DOM creates the document it will use this class to
            // add any requested items to the label
            PDSimageStatistics pdsImStats = new PDSimageStatistics();
                       
            i2PDSdom.setDebug(debug);
            
            pdsImStats.setImage(ri);
            
            // this is only until PDSImageStatistics is made active
            // then we can always recalculate statistics and use them instead of ones from the 
            // input image header
            
            /***
            if (dirty == false) {
            	pdsImStats.setAddMerItems(false);
            	pdsImStats.setAddStatistics(false);
            	pdsImStats.setCalculateStatistics(false);
            } else {
            	pdsImStats.setAddMerItems(addMerItems);
            	pdsImStats.setAddStatistics(addStatistics);
            	pdsImStats.setCalculateStatistics(calculateStatistics);
            }
            ****/
            
            // pdsImStats.setAddMerItems(addMerItems);
        	pdsImStats.setAddStatistics(addStatistics);
        	pdsImStats.setCalculateStatistics(calculateStatistics);
            
            pdsImStats.setDebug(debug);
            i2PDSdom.setPDSimageStatistics(pdsImStats);
            
            i2PDSdom.setPdsLabelType(pdsLabelType);
            
            // this isn't really needed. pof outputFilename will overide this one
            if (detachedLabelOnly) {
            	i2PDSdom.setFilename(inputFilename);
            } else {
            	i2PDSdom.setFilename(outputFilename);
            }
            // create a Document of the PDS SYSTEM label from the image
            Document pds_systemLabel_doc = i2PDSdom.getDocument();
            
            if (debug) {
            	System.out.println("pdsLabelType "+pdsLabelType);
            	System.out.println("readerFormat "+readerFormat);
            	System.out.println("pds_systemLabel_doc "+pds_systemLabel_doc);           
            	domUtil.serializeDocument(pds_systemLabel_doc, "pds_systemLabel_doc1.xml", "xml");
            }
            
            // get the Document from the PDSMetadata
            // this metadata comes from a PDSimageReader or a Transcoder
            // all of the system label stuff should have been removed or moved to some place which hides it
            // merge these 2 Documents into one
            
            String xPath = "//PDS_LABEL" ; // this String should be a param somewhere
            // String nativeFormatName = pm.nativeImageMetadataFormatName ;
			String nativeFormatName = pm.getNativeMetadataFormatName()  ;
            if (debug)  {
            	System.out.println("PDSImageWriter.write() nativeFormatName "+nativeFormatName);
             	System.out.println("xPath "+xPath);
            }
            Document pdsDoc = (Document) pm.getAsTree(nativeFormatName);
            
            if (debug)  {
				System.out.println("====================== pdsDoc ============================");   			
    			domUtil.printDocInfo(pdsDoc);
    			domUtil.serializeDocument(pdsDoc, "pdsDoc1.xml", "xml");
    			System.out.println("PDSImageWriter.write() readerFormat = "+readerFormat+"  detachedLabelOnly = "+detachedLabelOnly);
            }
            
            if (detachedLabelOnly == true && readerFormat.equalsIgnoreCase("pds")) {
            	ArrayList xpathDeleteArrayList = new ArrayList();
    			 
    			 // find all items whose name start with ^ - these are pointers
    			//  xPath = "//OBJECT/item[starts-with(@name,'^')]" ;
    			// xPath = "//item[starts-with(@name,'^')]" ;
    			// xPath = "//item[starts-with(@key,'^')]" ;
    			 xPath = "//PDS_LABEL/item[starts-with(@key,'^')]" ;

    			
    			if (debug)  {
    				System.out.println("====================== PDS_POINTER ============================");
                	System.out.println("PDSImageWriter.write() PDS_POINTER");
                 	System.out.println("xPath "+xPath);
                }
    			
    			NodeList npList = domUtil.getNodeList(pdsDoc, xPath); 
    			pds_pointer_elementCt = npList.getLength();
    			// get a list of the POINTERS for a detached label. Do something similar to the BLOB stuff
    			
    			if (debug)  {
    				System.out.println("pds_pointer_elementCt = "+pds_pointer_elementCt);
    			}
    			
    			pds_pointer_elementIndex = 0;
    			
    			PDS_POINTER_NAME = new String[pds_pointer_elementCt]; //  = new String[];
    			PDS_POINTER_OFFSET_TYPE = new String[pds_pointer_elementCt]; // "byte" or "record" - "record" is the default
    			// look for a units attribute ? 
    			PDS_POINTER_PTR = new int[pds_pointer_elementCt]; // this is the value of the element
    			
    			
    			// there may be some ^DESCRIPTION node. Keep them. They will be ignored when the label is written
    			
    			// delete the ones which are NOT ^DESCRIPTION since we will recreate them later using this info
    			String itemNameToKeep = "^DESCRIPTION" ;
    			// String itemNameToKeep = "^DECSRIPTION" ;
    			String valueToDelete ="nothing"; // PDS_OBJECT__PTR
    			boolean includeAttributes = true;
    			boolean includeParent = true ;
    			for (int i=0 ;i < npList.getLength() ; i++) {
    				Node n1 = npList.item(i);
    				String[] xp = domUtil.getNodeXPath(n1,includeAttributes, includeParent);
    				String n1v = domUtil.getNodeValue(n1);
    				if (debug) System.out.println("i="+i+") n1v="+n1v);   				
    				Hashtable n2h =  domUtil.getNodeAttributesHash(n1 );
    				for (Enumeration e = n2h.keys(); e.hasMoreElements();)
    		    		{
    					Object key = e.nextElement();
    					Object value2 = n2h.get(key);
    					String value2s = value2.toString();
    					if (debug) System.out.println ("  i="+ i + "  hash enum key="+key+" value="+value2+" - "+value2s);
    		    		}
    				
    				
					Object value2 = n2h.get("key");
					String value2s = value2.toString();
					
    				PDS_POINTER_NAME[pds_pointer_elementIndex] = value2s ;
    				
    				
    				PDS_POINTER_OFFSET_TYPE[pds_pointer_elementIndex] = "RECORD";
    				int pds_ptr = 0;
    				try {
    			        pds_ptr = Integer.parseInt(n1v);
    			        }
    			        catch (NumberFormatException e) {
    			        	if (debug) {
    			        	System.out.println("NumberFormatException "+e );
    			        	System.out.println("pds_ptr "+n1v +"   pds_+ptr="+pds_ptr );
    			        	}
    			        }  
    				PDS_POINTER_PTR[pds_pointer_elementIndex] = pds_ptr;
    				
    				String xpDelete = xp[0].replaceAll("///", "//");
		  			if (debug) { System.out.println("       matches - delete this node "+xpDelete); }
		  			// domUtil.deleteNode(pdsDoc, xpDelete);
		  			xpathDeleteArrayList.add(xpDelete);
		  			pds_pointer_elementIndex++;
    					
    		   }	
    			
    			
    			int dSize = xpathDeleteArrayList.size();
    			if (debug) {
    				System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
    				System.out.println("xpathDeleteArrayList size = "+dSize);
    				System.out.println("xPath "+xPath);
    			}
    			for (int z=0 ; z<dSize; z++) {
    				String xp = xpathDeleteArrayList.get(z).toString();
    				if (debug) { 
    					System.out.println("PDSImageWriter deleteNode ");
    					System.out.println( z+") xp = "+xp); 
    					}
    				// do we delete these and rebuild from the data ?
    				domUtil.deleteNode(pdsDoc, xp);
    			}
            }
            
            
            
            pds_pointer_elementCt = pds_pointer_elementIndex;
            
            if (debug) {
            	System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
            	System.out.println("PDSImageWriter.write() pds_pointer_elementIndex = "+ pds_pointer_elementIndex );
            	for (int jj=0 ; jj < pds_pointer_elementCt ; jj++) {
            		System.out.println(jj+") PDS_POINTER_NAME["+jj+"]="+PDS_POINTER_NAME[jj]);
            		System.out.println(jj+") PDS_POINTER_TYPE["+jj+"]="+PDS_POINTER_OFFSET_TYPE[jj]);
            		System.out.println(jj+") PDS_POINTER_PTR["+jj+"]="+PDS_POINTER_PTR[jj]);
            		
            		System.out.println("----");
            		// PASS THIS TO DOM2PDFSLABEL??
            	}
            	System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
            }	
            	
            	
            	
            	
        	// ==========================================================================================   
            
            
            
            
            if (addBLOB == true && vbh != null) {
    			// get info from all the BLOB related input 
    			/******
    			<OBJECT name="SOH_AFTER_TABLE_PARMS">

    			<item name="PDS_OBJECT__TYPE" quoted="false">TABLE</item>
    			<item name="PDS_OBJECT__PTR" quoted="false">3</item>
    			<item name="PDS_OBJECT__OFFSET" quoted="false">0</item>
    			*******/
    			
    			
    			
    			
    			
    		
    			/*******
    			 * Get all the PDS_OBJECT_ values - these are for BLOB itens passed in from a vicar label
    			 * 
    			 * They should exist if the image read is not vicar
    			 **********/
    			// xpath to get all the OBJECTS that contain an item of PDS_OBJECT__TYPE
            	ArrayList xpathDeleteArrayList = new ArrayList();
    			    			
    			xPath = "//OBJECT/item[@name='PDS_OBJECT__TYPE']" ;
    			if (debug)  {
                	System.out.println("PDSImageWriter.write() PDS_OBJECT");
                 	System.out.println("xPath "+xPath);
                }
    			
    			NodeList n1List = domUtil.getNodeList(pdsDoc, xPath); 
    			blob_elementCt= n1List.getLength();
    			
    			PDS_OBJECT_NAME = new String[blob_elementCt];
    			PDS_OBJECT_LOC = new String[blob_elementCt];
    			PDS_OBJECT_TYPE = new String[blob_elementCt];
    			PDS_OBJECT_PTR  = new int[blob_elementCt];
    			PDS_OBJECT_OFFSET  = new int[blob_elementCt];
    			
    			String valueToDelete ="nothing"; // PDS_OBJECT__PTR
    			boolean includeAttributes = true;
    			boolean includeParent = true ;
    			String pds_object_name = "";
    			for (int i=0 ;i < n1List.getLength() ; i++) {
    				Node n1 = n1List.item(i);
    				String[] xp = domUtil.getNodeXPath(n1,includeAttributes, includeParent);
    				String n1v = domUtil.getNodeValue(n1);
    				if (debug) {
    					System.out.println("i="+i+"  #####################################");
    					System.out.println("i="+i+") n1v="+n1v);
    				}
    				
    					for (int j=0; j < xp.length ; j++) {
 				
    						if (debug) { System.out.println("  xPath j="+j+") xp="+xp[j] ); }
    						
    						if (j==0)  {
    							// find the OBJECT. loop thru the children
    							// getSingleNode(Node node, String xpath)
    							String xp2 = xp[j] ;
    							String[] ary = xp2.split("/item");
    							String xp3 = ary[0].replaceAll("///", "//");
    							
    							String xp4a = "."; // "*"
    							String xp5a = "parent::*"; // "*"
    							
    							if (debug) { 
    								System.out.println("  xPath xp3  "+xp3 ); 
    								System.out.println("  xPath xp4a  "+xp4a );
    								System.out.println("  xPath xp5a  "+xp5a );
    								}
    							
    							// new
    							Node parent = n1.getParentNode();
    							
    							// xpath to say return everything
    							NodeList nl2 = domUtil.getNodeList(parent, xp4a) ;
    							
    							// NodeList nl2 = domUtil.getNodeList(n1, xp5a) ;

    							
    							// NodeList nl2 = domUtil.getNodeList(pdsDoc, xp3) ;
    							int nl2length = nl2.getLength();
    							if (debug) { System.out.println("    nl2length = "+nl2length +"  *************************" ); }
    							for (int k=0 ;k < nl2length ; k++) {
    			    				Node n2 = nl2.item(k);
    			    				String[] xp4 = domUtil.getNodeXPath(n2,includeAttributes, includeParent);
    			    				
    			    				Hashtable n2h =  domUtil.getNodeAttributesHash(n2 );
    			    				String n2name = n2h.get("name").toString();
    			    				pds_object_name = n2name;
    			    				if (debug) {
    			    					System.out.println("  xPath xp4 "+k+") "+xp4[k] );
    			    					System.out.println("  n2 name="+n2name);
    			    				}
    			    				
    			    				// PDS_OBJECT_NAME[blob_elementIndex] = n2name;
    			        			  			        			
    			    				// get all the children of this node
    			    				NodeList children = n2.getChildNodes();
    			    		    	Node child ;
    			    		    	int childLength = children.getLength();
    			    		    	if (debug) {System.out.println("    childLength = "+childLength +"  *************************" );}
    			    		    	for (int l = 0 ; l < childLength ; l++) {
    			    		    		if (debug) { System.out.println ("i="+i+"  l="+ l ); }
    			    		    		Node child1 = children.item(l);
    			    		    		String child1NodeName = child1.getNodeName();
    			    		    		Hashtable h =  domUtil.getNodeAttributesHash(child1 );
    			    		    		if (debug) {
    			    		    		for (Enumeration e = h.keys(); e.hasMoreElements();)
			    		    		    	{
    			    		    			Object key = e.nextElement();
    			    		    			Object value2 = h.get(key);
    			    		    			String value2s = value2.toString();
    			    		    			System.out.println ("i="+i+"  l="+ l + "  hash enum key="+key+" value="+value2+" - "+value2s);
			    		    		    	}
    			    		    		}
    			    		    		
    			    		    		String child1value = domUtil.getNodeValue(child1);  			    		    		
    			    		    		String[] xp5 = domUtil.getNodeXPath(child1, includeAttributes, includeParent);    			    		    		
    			    		    		String child1nameAttr = h.get("name").toString();
    			    		    		
    			    		    		String deleteChild1nameAttr = "";
    			    		    		if (child1nameAttr.equalsIgnoreCase("PDS_OBJECT__TYPE")) {  			    		    			
    			    		    			PDS_OBJECT_TYPE[blob_elementIndex] = child1value;
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else if (child1nameAttr.equalsIgnoreCase("^PDS_OBJECT")) {
    			    		    			PDS_OBJECT_PTR[blob_elementIndex] = Integer.parseInt(child1value.trim());  
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else if (child1nameAttr.equalsIgnoreCase("PDS_OBJECT__LOC")) {
    			    		    			PDS_OBJECT_LOC[blob_elementIndex] = child1value.trim();  
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else if (child1nameAttr.equalsIgnoreCase("PDS_OBJECT__PTR")) {
    			    		    			PDS_OBJECT_PTR[blob_elementIndex] = Integer.parseInt(child1value.trim());  
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else if (child1nameAttr.equalsIgnoreCase("PDS_OBJECT__OFFSET")) {
    			    		    			PDS_OBJECT_OFFSET[blob_elementIndex] =  Integer.parseInt(child1value.trim());  	
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else if (child1nameAttr.equalsIgnoreCase("PDS_OBJECT__NAME")) {
    			    		    			PDS_OBJECT_NAME[blob_elementIndex] =  child1value.trim();  	
    			    		    			deleteChild1nameAttr = child1nameAttr;
    			    		    		} else {
    			    		    			deleteChild1nameAttr = "";
    			    		    		}
    			    		    		// PDS_OBJECT_TYPE = new String[blob_elementCt];;
    			    	    			// PDS_OBJECT_PTR  = new int[blob_elementCt];
    			    	    			// PDS_OBJECT_OFFSET
    			    		    		
    			    		    		
    			    		    		if (debug)  {
    			    		    		  System.out.println("  i="+ i +" l="+l+") child12NodeName = "+ child1NodeName+" h="+h);
    			    		    		  System.out.println("       child1value "+child1value);
    			    		    		  System.out.println("       child1nameAttr "+child1nameAttr);
    			    		    		}
    			    		    		
    			    		    		  for (int i5 = 0 ; i5 < xp5.length ; i5++) {
    			    		    			  if (debug) { System.out.println("       xp5["+i5+"]=" +xp5[i5] ); }
    			    		    			  
    			    		    			  
    			    		    			  if (!deleteChild1nameAttr.equals("")) {
    			    		    				// String regex = "(?i).*"+child1nameAttr+"*";
        			    		    			  String regex = ".*"+deleteChild1nameAttr+".*";
        			    		    			  // String regex = ".*"+child1nameAttr+".*";
    			    		    				  boolean b = xp5[i5].matches(regex);
    			    		    				  if (debug) { System.out.println("       regex="+regex+"  b="+b);}
    			    		    			  		if (b == true) {
    			    		    			  			
    			    		    			  			String xpDelete = xp5[i5].replaceAll("///", "//");
    			    		    			  			if (debug) { System.out.println("       matches - delete this node "+xpDelete); }
    			    		    			  			// domUtil.deleteNode(pdsDoc, xpDelete);
    			    		    			  			xpathDeleteArrayList.add(xpDelete);
    			    		    				  
    			    		    			  		} else {
    			    		    			  			if (debug) { System.out.println("       does NOT match "); }
    			    		    			  		}
    			    		    		  		}
    			    		    		  }
    			    		    		  
    			    		    		  
    			    		    		
    			    		    		// node.removeChild(child);
    			    		    		// success = true;
    			    		    	}
    			    		    	if (debug) { 
    			    		    		System.out.println ("**** k="+k+"   blob_elementIndex="+blob_elementIndex+"*********************" );
    			    		    		System.out.println( "###########################\n");
    			    		    		System.out.println( "PDS_OBJECT_NAME[blob_elementIndex] = " +PDS_OBJECT_NAME[blob_elementIndex]);
    			    		    		System.out.println( "pds_object_name = "+pds_object_name);
    			    		    		System.out.println( "###########################\n");
    			    		    		}
    			    		    	if (PDS_OBJECT_NAME[blob_elementIndex] == null) {
    			    		    		PDS_OBJECT_NAME[blob_elementIndex] = pds_object_name;
    			    		    	}
    			    		    	blob_elementIndex++;
    			    		    	
    			    		    	
    			    		    	// PDS_OBJECT_NAME[blob_elementIndex] =  child1value.trim();  	
    						}
    					
    				}				
    			}
    			
    		
    		   }	
    			int dSize = xpathDeleteArrayList.size();
    			if (debug) {
    				System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
    				System.out.println("xpathDeleteArrayList size = "+dSize);
    			}
    			for (int z=0 ; z<dSize; z++) {
    				String xp = xpathDeleteArrayList.get(z).toString();
    				if (debug) { 
    					System.out.println( "PDSIMageWriter deleteNode"); 
    					System.out.println( z+") xp = "+xp); 
    					}
    				domUtil.deleteNode(pdsDoc, xp);
    			}
            }
            
            
            
            blob_elementCt = blob_elementIndex;
            
            if (debug) {
            	System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++");
            	System.out.println("PDSImageWriter.write() blob_elementIndex = "+ blob_elementIndex );
            	for (int jj=0 ; jj < blob_elementCt ; jj++) {
            		System.out.println(jj+") PDS_OBJECT_NAME["+jj+"]="+PDS_OBJECT_NAME[jj]);
            		System.out.println(jj+") PDS_OBJECT_LOC["+jj+"]="+PDS_OBJECT_LOC[jj]);
            		System.out.println(jj+") PDS_OBJECT_TYPE["+jj+"]="+PDS_OBJECT_TYPE[jj]);
            		System.out.println(jj+") PDS_OBJECT_PTR["+jj+"]="+PDS_OBJECT_PTR[jj]);
            		System.out.println(jj+") PDS_OBJECT_OFFSET["+jj+"]="+PDS_OBJECT_OFFSET[jj]);  
            		System.out.println("----");
            		// PASS THIS TO DOM2PDFSLABEL??
            	}
            	
            	System.out.println("PDSImageWriter.write() ------ after pm.getAsTree()" );
            	domUtil.serializeDocument(pdsDoc, "pdsDoc2.xml", "xml");
            	System.out.println("PDSImageWriter.write() ------ after serializeDocument" );
            
            	System.out.println("PDSImageWriter.write() mergeDocuments" );
            	System.out.println("pds_systemLabel_doc "+pds_systemLabel_doc);
            	System.out.println("pdsDoc "+pdsDoc);
            	System.out.println("===================================");
            }
            
            
            // pds_file_records = 0; // use this only for a detached label
        	// pds_record_bytes = 0; // use this only for a detached label
        	// pds_label_records = 0; // use this only for a detached label
            
//          remove these items from the pdsDoc 
    		// These items are calculated for the image being written,
    		// remove the values from the input do they won't be duplicated.
    		// some may be wrong for the output image
            if (fakeImage == false) {
            	xPath = "//item[@key='RECORD_TYPE']"; 
            	domUtil.deleteNode(pdsDoc, xPath);
            }
    		
    		xPath = "//item[@key='RECORD_BYTES']"; 
    		String pds_record_bytes_s = domUtil.getNodeValue(pdsDoc, xPath);
    		try {
				pds_record_bytes = Integer.parseInt(pds_record_bytes_s);
		        }
		        catch (NumberFormatException e) {
		        	if (debug) {
		        	System.out.println("NumberFormatException "+e );
		        	System.out.println(xPath+" pds_record_bytes_s = "+ pds_record_bytes_s );
		        	}
		        }  
		    if (fakeImage == false) {
		       domUtil.deleteNode(pdsDoc, xPath);
		      }
    		
    		// we need this value for a PDS detached label. It stays the same.
    		// save this value and use if needed
    		xPath = "//item[@key='FILE_RECORDS']"; 
    		String pds_file_records_s = domUtil.getNodeValue(pdsDoc, xPath);
			try {
				pds_file_records = Integer.parseInt(pds_file_records_s);
		        }
		        catch (NumberFormatException e) {
		        	if (debug) {
		        	System.out.println("NumberFormatException "+e );
		        	System.out.println(xPath+" pds_file_records_s = "+ pds_file_records_s );
		        	}
		        	pds_file_records = -1;
		        }  
			if (debug) {
				System.out.println("FILE_RECORDS pds_file_records = "+ pds_file_records );
	        	System.out.println("FILE_RECORDS pds_file_records_s = "+ pds_file_records_s );
			}
			
			// keep this even for a detached label
		    if (fakeImage == true && delete_FILE_RECORDS) {
		    	if (debug) {
		    		System.out.println("delete FILE_RECORDS delete_FILE_RECORDS = "+delete_FILE_RECORDS +" fakeImage = "+fakeImage+"  "+xPath);
		    	}
		    	domUtil.deleteNode(pdsDoc, xPath);
		    } else {
		    	if (debug) {
		    		System.out.println("NO delete FILE_RECORDS delete_FILE_RECORDS = "+delete_FILE_RECORDS +" fakeImage = "+fakeImage+"  "+xPath);
		    	}
		    }
    		
    		xPath = "//item[@key='LABEL_RECORDS']"; 
    		String pds_label_records_s = domUtil.getNodeValue(pdsDoc, xPath);
    		try {
				pds_label_records = Integer.parseInt(pds_label_records_s);
		        }
		        catch (NumberFormatException e) {
		        	if (debug) {
		        	System.out.println("NumberFormatException "+e );
		        	System.out.println(xPath+" pds_record_bytes_s = "+ pds_label_records_s );
		        	}
		        }  
		    
		       
		   /* if (detachedLabelOnly == false ) {
		    	domUtil.deleteNode(pdsDoc, xPath);
		    } */
    		if (debug) {
	        	System.out.println("deleteNode "+xPath );
    		}
		   domUtil.deleteNode(pdsDoc, xPath);
    		
    		if (debug)  {
    			System.out.println("==========================================="); 
				System.out.println("==========================================="); 
				System.out.println("deleteNode LABEL_RECORDS detachedLabelOnly="+detachedLabelOnly);
				System.out.println("pds_file_records = "+pds_file_records+"   "+pds_file_records_s);
				System.out.println("pds_record_bytes = "+pds_record_bytes+"   "+pds_record_bytes_s);
				System.out.println("pds_label_records = "+pds_label_records+"   "+pds_label_records_s);
				System.out.println("==========================================="); 
				System.out.println("==========================================="); 
    		}
    		
    		pof.setPds_file_records(pds_file_records);  	
    		pof.setPds_label_records(pds_label_records) ;    
    		pof.setPds_record_bytes(pds_record_bytes);
    		 
    		xPath = "//COMMENT"; 
    		// domUtil.deleteNode(pdsDoc, xPath);
    		// deleteNodeByValue(Node inNode, String xPath, String valueToDelete)
    		deleteNodeByValue(pdsDoc, xPath,"FILE DATA ELEMENT");
    		
    		if (detachedLabelOnly == true && readerFormat.equalsIgnoreCase("pds")) {
    			xPath = "//OBJECT[@name='IMAGE_HEADER']/item[@key='BYTES']";
    			String vicarLabelBytes_s = domUtil.getNodeValue(pdsDoc, xPath);
    			try {
    				vicarLabelBytes = Integer.parseInt(vicarLabelBytes_s);
    				pof.setVicarLabelBytes(vicarLabelBytes);
    				if (debug)  {
    					System.out.println("====^=^-^-^-^-^=^=^====="); 
    					System.out.println("vicarLabelBytes_s = "+vicarLabelBytes_s+" ** vicarLabelBytes "+vicarLabelBytes);
    				    }
    		        }
    		        catch (NumberFormatException e) {
    		        	if (debug) {
    		        	System.out.println("NumberFormatException "+e );
    		        	System.out.println("vicarLabelBytes_s = "+ vicarLabelBytes_s );
    		        	}
    		        }  
    		   
    		}
    		
    		if (embedVicarLabel == false) {
    			// remove the comment for IMAGE_HEADER
    			xPath = "//COMMENT";    
    			deleteNodeByValue(pdsDoc, xPath,"IMAGE HEADER DATA ELEMENTS");
    		
    			// remove the HEADER object from the input metadata if it exists
    			
    			xPath = "//OBJECT[@name='IMAGE_HEADER']";
        		domUtil.deleteNode(pdsDoc, xPath);
        		// get rid of the comment too
    		
    		}
    		
    		/**
    		String val[] = domUtil.getNodeValues(pdsDoc, xPath);
    		for (int k=0; k < val.length ; k++) {
				System.out.println("  "+k+") "+val[k] );
			}				
    		System.out.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
    		*/
    		
    		// find out if the input file was ODL or PDS
    		// <PDS3>PDS_VERSION_ID</PDS3> or <ODL3>ODL_VERSION_ID</ODL3>
    		// xPath = "//PDS_LABEL/item[starts-with(@key,'^')]" ;
    		
    		xPath = "//*[contains(text(),'VERSION_ID')]";

			
			if (debug)  {
				System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"); 
    			System.out.println("++"); 
    			System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"); 
				System.out.println("====================== VERSION_ID ============================");
            	System.out.println("PDSImageWriter.write() VERSION_ID");
             	System.out.println("xPath "+xPath);
            }	
			
			NodeList npList = domUtil.getNodeList(pdsDoc, xPath); 
			pds_pointer_elementCt = npList.getLength();
			// get a list of the POINTERS for a detached label. Do something similar to the BLOB stuff
			
			if (debug)  {
				System.out.println("pds_pointer_elementCt = "+pds_pointer_elementCt);
			}
			
			
			boolean includeAttributes = true;
			boolean includeParent = true ;
			for (int i=0 ;i < npList.getLength() ; i++) {
				Node n1 = npList.item(i);
				String[] xp = domUtil.getNodeXPath(n1,includeAttributes, includeParent);
				String n1v = domUtil.getNodeValue(n1);
				String n1name = n1.getNodeName();
				if (debug) {
					System.out.println("i="+i+") n1v="+n1v+"   n1name="+n1name);   				
				}
				for (int j=0; j < xp.length ; j++) {
	 				
					if (debug) { System.out.println("  xPath j="+j+") xp="+xp[j] ); }
										
				}
				if (n1name.startsWith("ODL") || n1name.startsWith("PDS")) {
					inputPdsLabelType = n1name;
					if (debug) {  			 				    			
		    			System.out.println("++ inputPdsLabelType "+ inputPdsLabelType +"  ++"); 
					}
				}
				
					
		   }	
			
			
			if (debug) {  			 		
    			System.out.println("++++++++++++++++++++++++++++++++++++"); 
    			System.out.println("++ inputPdsLabelType "+ inputPdsLabelType +"  ++"); 
    			System.out.println("++++++++++++++++++++++++++++++++++++"); 
			}
    		
    		
            // the pds_systemLabel_doc has an IMAGE OBJECT and pdsDoc may also have one
            // we prefer the one in pdsDoc. Whichever we use must go at the END of the Document

    		xPath = "//OBJECT[@name='IMAGE']";   
    		if (debug) {  			 		
    			System.out.println("++++++++++++++++++++++++++++++++++++"); 
    			System.out.println("+  xPath = "+xPath);
    		}
    		
            Node systemImageObjectNode = domUtil.getSingleNode(pds_systemLabel_doc, xPath);
            if (systemImageObjectNode == null) {
            	xPath = "//object[@name='IMAGE']";
            	systemImageObjectNode = domUtil.getSingleNode(pds_systemLabel_doc, xPath);
            }
            
            xPath = "//OBJECT[@name='IMAGE']";
            Node pdsDocImageObjectNode = domUtil.getSingleNode(pdsDoc, xPath);
            if (pdsDocImageObjectNode == null) {
            	xPath = "//object[@name='IMAGE']";
            	pdsDocImageObjectNode = domUtil.getSingleNode(pdsDoc, xPath);
            }
            
            if (debug) {
            // if (addStatistics == false) {
            	//          remove the ImageObjectNode from the input entirely
            System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"); 
            System.out.println("++"); 
            System.out.println("addStatistics = "+addStatistics+"  calculateStatistics = "+calculateStatistics); 
            System.out.println("dirty = "+dirty+" embedVicarLabel = "+embedVicarLabel);
            System.out.println("detachedLabel = "+detachedLabel+"  detachedLabelOnly = "+detachedLabelOnly);
            System.out.println("fakeImage = "+fakeImage);
            System.out.println("pdsDocImageObjectNode " +pdsDocImageObjectNode);  
            
            System.out.println("pdsLabelType " +pdsLabelType); 
            System.out.println("inputPdsLabelType " +inputPdsLabelType);
            System.out.println("addBinaryHeader " +addBinaryHeader); 
            System.out.println("addBLOB " +addBLOB); 
            System.out.println("delete_FILE_RECORDS "+delete_FILE_RECORDS);
            System.out.println("pds_file_records "+pds_file_records);
            
            if (pdsDocImageObjectNode == null) {
            	System.out.println("pdsDocImageObjectNode == null");
            } else {
            	System.out.println("pdsDocImageObjectNode != null");
            }
            System.out.println("++++++++++++++++++++++++++++++++++++"); 
            System.out.println("systemImageObjectNode "+systemImageObjectNode);
            System.out.println("++++++++++++++++++++++++++++++++++++"); 
             // serialize the nodes
    		domUtil.serializeNode(pdsDocImageObjectNode, "pdsDocImageObjectNode.xml", "xml");
    		domUtil.serializeNode(systemImageObjectNode, "systemImageObjectNode.xml", "xml");
    		domUtil.serializeDocument(pdsDoc, "pdsDoc3.xml", "xml");   
    		
            System.out.println("++"); 
            
            System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"); 
            }
            
            Document doc = null;
            if (pdsDocImageObjectNode != null && systemImageObjectNode != null) {
            	// && addStatistics == true) {
            	// use the pdsDocImageObjectNode, delete systemImageObjectNode
            	// it should only be here if it is valid
            	// otherwise we keep the one we have systemImageObjectNode
            	// if (debug) System.out.println("Image is dirty, remove "+xPath); 
            	if (debug) System.out.println("2 IMAGE objects remove "+xPath+" systemImageObjectNode"); 
            	// this is where a merge should occur
            	//  systemImageObjectNode has precedence over pdsDocImageObjectNode
        		 	
        		if (debug) {
        			System.out.println("++++++++++++++++++++++++++++++++++++++++++++++++++++"); 
        			System.out.println("+++");
        		}
        		// mergeNodesOnElAttrUniq(Node n1, Node n2, String elementName, String attributeName)
        		// domUtil.mergeNodes(pdsDocImageObjectNode, systemImageObjectNode);
        		
        		xPath = "//COMMENT";        		
        		deleteNodeByValue(pdsDoc, xPath,"POINTERS TO DATA OBJECTS");
        		
        		// remove ^IMAGE item and ^IMAGE_HEADER from input document
        		xPath = "//item[@key='^IMAGE_HEADER']"; // item key="^IMAGE_HEADER"
        		domUtil.deleteNode(pdsDoc, xPath);
        		
        		xPath = "//item[@key='^IMAGE']"; // item key="^IMAGE"
        		domUtil.deleteNode(pdsDoc, xPath);
        		
        		// do things based on addStatistics and dirty
        		if (dirty == true || addStatistics == false) {
        			// the file data has changed, we can't use old statistics
        			// or anything else from the system label of the input
        			// pdsDocImageObjectNode is IMAGE OBJECT
        			// domUtil.deleteNode(pdsDocImageObjectNode);
        			
        			// this deletes the node from the parent document
        			Node parent = systemImageObjectNode.getParentNode();
            		// remove it so we can put at the end           		
            		// parent.removeChild(systemImageObjectNode); 
            		// OR
            		// domUtil.deleteNode(systemImageObjectNode);
            		
            		// add it back at the end
            		parent = pdsDocImageObjectNode.getParentNode();
            		// we need the OBJECT IMAGE at the end
            		// adda DUMMY one to put at the end ?
            		// delete all the items and keep the IMAGE OBJECT ??
            		// delete children
            		// delete specific children (statistics only ???)
            		// pdsDocImageObjectNode = 
            		deleteStatisticsChildren(pdsDocImageObjectNode);         		
            		
        			if (debug) {
        				domUtil.serializeNode(pdsDocImageObjectNode, "pdsDocImageObjectNode2.xml", "xml");
            			System.out.println("DIRTY == "+dirty+" addStatistics = "+addStatistics+" deleting children of pdsDocImageNodeObject");
        			}
        			
        		} 
        		
        		
        		// else {
        			
        		    // file data has NOT changed, input values are still valid 
        			// such as statistics
        			// remove statistics from systemImageObjectNode so they will NOT
        			// replace the input ones
        			// at least until the 
        			xPath = "//OBJECT[@name='IMAGE']"; // OBJECT name = IMAGE
        			// n1 is replaced by n2
        			if (debug)  {
        				System.out.println("  mergeNodesOnElAttrUniq  ++++++++++++++++++++++++++++++++++++"); 
        			}
        			domUtil.mergeNodesOnElAttrUniq(pdsDocImageObjectNode, 
        					systemImageObjectNode, "item", "key", xPath);
        			
        			
        			if (debug) {
        				System.out.println("  after mergeNodesOnElAttrUniq  ++++++++++++++++++++++++++++++++++++");
        				System.out.println("++++++++++++++++++++++++++++++++++++"); 
        				System.out.println("+ DIRTY = "+dirty+" addStatistics = "+addStatistics);
        				System.out.println("+    ");
        				System.out.println("+        mergeNodes DONE"); 
        				System.out.println("++++++++++++++++++++++++++++++++++++"); 
        		
        				domUtil.serializeNode(pdsDocImageObjectNode, "pdsDocImageObjectNode3.xml", "xml");
        			}
        		
        			// remove the IMAGE OBJECT node from the 
        			// pds_systemLabel_doc. Otherwise the IMAGE OBJECT is duplicated when we merge below
        			if (systemImageObjectNode != null) {
	        			// systemImageObjectNode
	            		Node parent = systemImageObjectNode.getParentNode();
	            		// remove it so we can put at the end
	            		if (debug) { domUtil.serializeNode(parent, "parent1.xml", "xml"); }
	            		parent.removeChild(systemImageObjectNode); 
	            		if (debug) { domUtil.serializeNode(parent, "parent2.xml", "xml"); }
            		}
        		// }
        		// boolean success = domUtil.deleteNode(systemImageObjectNode); 
        		// if (debug) System.out.println("deleteNode success = "+success);
            doc = pdsDoc; 
            } // else pds_systemLabel_doc is used instead of the system part of the input doc
            
            // srl 1-2014 keep this. make sure the value is for the file the detached label is pointing to
            // should be set in the input. see pds_file_records
            if (detachedLabelOnly && delete_FILE_RECORDS )  {
            	xPath = "//item[@key='FILE_RECORDS']";       		       		
        		domUtil.deleteNode(pdsDoc, xPath);
        		if (debug)  {
    				System.out.println("  detachedLabelOnly                       ++++++++++++++++++++++++++++++++++++");    				 
    				System.out.println("  delete: pdsDoc FILE_RECORDS    +++++"); 
    			}
            } else {
            	if (debug)  {
    				System.out.println("  detachedLabelOnly                       ++++++++++++++++++++++++++++++++++++");    				 
    				System.out.println("  KEEP: pdsDoc FILE_RECORDS    +++++"); 
    			}
            }
            
            if (detachedLabelOnly) {
            	if (debug) {
            		System.out.println("  delete: pds_systemLabel_doc FILE_RECORDS    +++++"); 
            	}
            	xPath = "//item[@key='FILE_RECORDS']";       		       		
            	domUtil.deleteNode(pds_systemLabel_doc, xPath);
            	
            	// <ODL3>ODL_VERSION_ID</ODL3>
            	xPath = "//ODL3";       		       		
            	domUtil.deleteNode(pdsDoc, xPath);
            }
            
            
            if (detachedLabelOnly && fakeImage)  {
    			
    			xPath = "//COMMENT";        		
        		deleteNodeByValue(pdsDoc, xPath,"POINTERS TO DATA OBJECTS");
        		
        		// try to fix the order things get put together
        		/**
        		xPath = "//item[@key='FILE_RECORDS']";       		       		
        		domUtil.deleteNode(pdsDoc, xPath);
        		***/
        		
        		xPath = "//OBJECT[@name='IMAGE']"; // item key="^IMAGE"
        		domUtil.deleteNode(pds_systemLabel_doc, xPath);
        		
        		
        		if (debug)  {
    				System.out.println("  detachedLabelOnly && fakeImage          ++++++++++++++++++++++++++++++++++++"); 
    				System.out.println("  delete: pdsDoc POINTERS TO DATA OBJECTS ++++++++++++++++++++++++++++++++++++"); 
    				System.out.println("  delete: pds_systemLabel_doc //OBJECT[@name='IMAGE']    +++++"); 
    			}
    		}
            
            if (debug) {
    			domUtil.serializeDocument(pds_systemLabel_doc, "pds_systemLabel_doc2.xml", "xml");
    			
    		}
            
            /*******
            if (detachedLabelOnly && fakeImage == true) {
            	// don't merge the system label since there really isn't an image
            	if (debug) {
            		System.out.println("PDSImageWriter.write() do NOT mergeDocuments *** fakeImage ****" );
            	}
            	doc = pdsDoc ;
            } else {
            	// --------------------------------------------------------------
            	xPath = "//PDS_LABEL"; // this String should be a param somewhere
            	// doc2 is added into doc1
            	doc = domUtil.mergeDocuments(pds_systemLabel_doc, pdsDoc, xPath);
            	if (debug) {
            		System.out.println("PDSImageWriter.write() mergeDocuments **************" );
            	}
            } **********/
            
            
            if (detachedLabelOnly && fakeImage)  {
            	
            	// add a new NODE
            	// <OUTPUT_FILENAME>CC0_353400938EIN_F0000000001036288M1.DAT</OUTPUT_FILENAME>
        		
        		// <IMAGE_START_RECORD>LABEL_RECORDS+1</IMAGE_START_RECORD>
            	Node imageStartRecordNode = domUtil.createNode(pdsDoc, "IMAGE_START_RECORD", "LABEL_RECORDS+1");
            	
            	if (debug) {
            		domUtil.serializeNode(pdsDoc, "pdsDoc.xml", "xml");
            	}
            	
            	// <PDS_VERSION_ID>PDS3</PDS_VERSION_UD>
            	Node pdsVeridNode;
            	if (pdsLabelType.equalsIgnoreCase("PDS3")) {           			
            		pdsVeridNode = domUtil.createNode(pdsDoc, "PDS_VERSION_ID", "PDS3");
            	} else {
            		pdsVeridNode = domUtil.createNode(pdsDoc, "ODL_VERSION_ID", "ODL3");
            	}
            	
            	
            	
            	// insert the node
            	xPath = "//PDS_LABEL";
                Node pdsNode = domUtil.getSingleNode(pdsDoc, xPath);
                
            	xPath = "//ODL3";
                Node odl3Node = domUtil.getSingleNode(pdsDoc, xPath);
                
                if (debug) {
                	System.out.println("PDSImageWriter.write() pdsVeridNode "+pdsVeridNode);
                	System.out.println("PDSImageWriter.write() odl3Node "+odl3Node);
                	System.out.println("PDSImageWriter.write() pdsNode "+pdsNode);
                	System.out.println("PDSImageWriter.write() imageStartRecordNode "+imageStartRecordNode);
            		domUtil.serializeNode(pdsVeridNode, "pdsVeridNode.xml", "xml");
            		domUtil.serializeNode(odl3Node, "odl3Node.xml", "xml");
            		domUtil.serializeNode(pdsNode, "pdsNode.xml", "xml");
            		domUtil.serializeNode(imageStartRecordNode, "imageStartRecordNode.xml", "xml");
            	}
                
                Node oldNode = null;
                /**
                if (pdsNode != null) {
                	oldNode = pdsNode.replaceChild(pdsVeridNode, pdsNode );
                	domUtil.serializeNode(oldNode, "oldNodePds.xml", "xml");
                } else if (odl3Node != null) {
                	oldNode = pdsNode.replaceChild(pdsVeridNode, odl3Node );
                	domUtil.serializeNode(oldNode, "oldNodeOdl3.xml", "xml");
                } 
                ***/
                if (pdsNode != null) {
                	Node firstChild = pdsNode.getFirstChild();
                	if (debug) {
                		domUtil.serializeNode(firstChild, "pdsNodeFirstChild.xml", "xml");
                	}
                	// pdsNode.insertBefore(newChild, refChild)
                	pdsNode.insertBefore( pdsVeridNode, firstChild) ;
                	 if (debug) {
                		 domUtil.serializeNode(pdsNode, "pdsNode2.xml", "xml");
                	 }
                }
                
                
                xPath = "//item[@key='RECORD_TYPE']";      
                // <COMMENT>/* IDENTIFICATION DATA ELEMENTS */</COMMENT>
                xPath = "//COMMENT";  
                
        		Node commentNode = getNodeByValue(pdsNode, xPath, "/* IDENTIFICATION DATA ELEMENTS */") ;
               
                              
                pdsNode.insertBefore(imageStartRecordNode, commentNode);
            	
            	doc = pdsDoc;
            	if (debug) {
            		domUtil.serializeNode(pdsVeridNode, "pdsVeridNode.xml", "xml");
            		domUtil.serializeNode(odl3Node, "odl3Node.xml", "xml");
            		domUtil.serializeNode(oldNode, "oldNode.xml", "xml");
            		domUtil.serializeNode(commentNode, "commentNode.xml", "xml");
        			System.out.println("PDSImageWriter.write() DetachedLabelOnly fakeImage NO mergeDocuments **************" );
        		}
            } else {
            	
            	xPath = "//PDS_LABEL"; // this String should be a param somewhere
            	// doc2 is added into doc1
        		doc = domUtil.mergeDocuments(pds_systemLabel_doc, pdsDoc, xPath);
        		if (debug) {
        			System.out.println("PDSImageWriter.write() mergeDocuments **************" );
        			domUtil.serializeDocument(doc, "mergeDocPre.xml", "xml");    
        		}
        		// we need to have FILE_RECORDS before IMAGE_START_RECORD for the label to print properly
        		xPath = "//IMAGE_START_RECORD";
                Node imageStartNode = domUtil.getSingleNode(doc, xPath);
                if (debug) { domUtil.serializeNode(imageStartNode, "imageStartNode.xml", "xml"); }
                
                xPath = "//item[@key='FILE_RECORDS']";    
                Node fileRecordsNode = domUtil.getSingleNode(doc, xPath);
                if (debug) { domUtil.serializeNode(fileRecordsNode, "fileRecordsNode.xml", "xml"); }
                
                /***
                 * The insertBefore does all of this 
                // clone the node
                Node fileRecordsNodeClone = fileRecordsNode.cloneNode(true);
                // delete the original 
                domUtil.deleteNode(doc, xPath);
                
                // insert BEFORE IMAGE_START_RECORD
                // instead of doc. insert into one level lower??
                
                // <PDS_LABEL> get this and insert into this instead of the doc
                xPath = "//PDS_LABEL";
                Node pds_label = domUtil.getSingleNode(doc, xPath);
                pds_label.insertBefore(fileRecordsNodeClone, imageStartNode);
                ***/
                
                // we get: DOMException - HIERARCHY_REQUEST_ERR
                // if we try to do this on the Document
                xPath = "//PDS_LABEL";
                Node pds_label = domUtil.getSingleNode(doc, xPath);
                // insertBefore will delete the Element first, then insert it where we request
                pds_label.insertBefore(fileRecordsNode, imageStartNode);
                
                
                
            }
            
            if (debug) {
            	System.out.println("PDSImageWriter.write() mergeDocuments  " );
            	System.out.println("detachedLabelOnly "+detachedLabelOnly+"   fakeImage "+fakeImage);
            	System.out.println("pds_systemLabel_doc "+pds_systemLabel_doc);
            	System.out.println("pdsDoc "+pdsDoc);
            	System.out.println("doc "+doc);
            	 
            	domUtil.serializeDocument(pdsDoc, "pdsDoc4.xml", "xml");      
            	domUtil.serializeDocument(doc, "mergeDocAfter.xml", "xml");           	           
            }
            
            
            // We want the IMAGE OBJECT node at the end of the document
            // find it so we can remove it, then append at the end 
             // check these am I adding the correct ones ??          
            xPath = "//OBJECT[@name='IMAGE']";
            Node imageObjectNode = domUtil.getSingleNode(doc, xPath);
            
            if (imageObjectNode == null) {
            	xPath = "//object[@name='IMAGE']";
            	// imageObjectNode = domUtil.getSingleNode(pds_systemLabel_doc, xPath);
            	imageObjectNode = domUtil.getSingleNode(doc, xPath);
            }
            
            /**
            if (imageObjectNode != null) {
            	Node parent = imageObjectNode.getParentNode();
            	// remove it so we can put at the end
            	parent.removeChild(imageObjectNode); 
            	// appendChild puts the node at the end
            	parent.appendChild(imageObjectNode);
            }
            **/
            
            if (debug) {
            	System.out.println("PDSImageWriter.write() ------ after domUtil.mergeDocuments" );
            // this document will be used to write the image label
            	domUtil.serializeNode(imageObjectNode, "imageObjectNode.xml", "xml");
            	System.out.println("PDSImageWriter.write() ------ after serializeDocument 2" );
            }
            pof.setPdsDOM(doc);
            pof.setOutputFilename(outputFilename);
            pof.setInputFilename(inputFilename);
            
            // if (addBLOB && vbh!= null) {
            pof.setVicarBinaryHeader(vbh);
            pof.setAddBLOB(addBLOB);
            if (addBLOB == true && vbh != null) {
            	pof.setPDS_OBJECT_NAME(PDS_OBJECT_NAME);
            	pof.setPDS_OBJECT_LOC(PDS_OBJECT_LOC);
            	pof.setPDS_OBJECT_OFFSET(PDS_OBJECT_OFFSET);
            	pof.setPDS_OBJECT_PTR(PDS_OBJECT_PTR);
            	pof.setPDS_OBJECT_TYPE(PDS_OBJECT_TYPE);
            	// PDSOutputFile will get info about the VicarBinaryHeader from the vbh passed in above
            
            }
            
            // set all the values for a PDS detached label
            if (detachedLabelOnly) {
            	pof.setPDS_POINTER_NAME(PDS_POINTER_NAME);
            	
            	pof.setPDS_POINTER_PTR(PDS_POINTER_PTR);
            	pof.setPDS_POINTER_OFFSET_TYPE(PDS_POINTER_OFFSET_TYPE);
 
            	pof.setPds_file_records(pds_file_records);
            	
            	pof.setPDS_OBJECT_NAME(PDS_OBJECT_NAME);
            	pof.setPDS_OBJECT_LOC(PDS_OBJECT_LOC);
            	pof.setPDS_OBJECT_OFFSET(PDS_OBJECT_OFFSET);
            	pof.setPDS_OBJECT_PTR(PDS_OBJECT_PTR);
            	pof.setPDS_OBJECT_TYPE(PDS_OBJECT_TYPE);
            	            	
            }
            
            pof.setInputPdsLabelType(inputPdsLabelType);
            pof.setDetachedLabel(detachedLabel);
            pof.setDetachedLabelOnly(detachedLabelOnly);
            pof.setFakeImage(fakeImage);
            pof.setDebug(debug);
            
            
            // debug = debug2;
            // domUtil.setDebug(debug);
            
             jpl.mipl.io.vicar.SystemLabel systemLabel;
            // should check if we have a param and use it
            // VicarLabel vicarLabel = vof.getVicarLabel();
            // SystemLabel systemLabel = vof.getSystemLabel();
            
            /**
            * The VicarLabel is still VERY important.
            * VicarIO uses the Vicar Label to control how the image data is 
            * written to the file
            **********/
            if (debug) System.out.println("PDSImageWriter.write() ------ create VicarLabel for output" );
            try { // catch Exceptions for all the VicarIO methods called
             if (vicarLabel == null) { // this is NOT the embeded vicar label
             	// this one is for the PDS image and controls the vicario routines which output the data to the file
                // jpl.mipl.io.vicar.VicarLabel 
                vicarLabel = new jpl.mipl.io.vicar.VicarLabel();
                if (debug) System.out.println("null VicarLabel creating one from the image");
                // jpl.mipl.io.vicar.SystemLabel systemLabel = createSystemLabel(sm);
                systemLabel = createSystemLabel(ri);
                // add in binary prefix value 
                systemLabel.setNBB(nbb);
                systemLabel.calcRecsize();
                
                pof.setSystemLabel(systemLabel);
                pof.setVicarLabel(vicarLabel);
              
             } else {
                // when I have a real vicarLabel do I need to also set the 
                // SystemLabel or is it set when I do setVicarLabel ???
                // setPrimaryInput(SystemLabel slbl, VicarLabel vlbl)
                // if systemLabel is null then the systemLabel from vicarLabel is used
                
                if (debug) System.out.println("VicarImageWriter.write() vof.setPrimaryInput");
                pof.setPrimaryInput(null, vicarLabel );
             }

			// set all the values for an emebedded VicarLabel
			// these values are used by PDSOutputFile when the file is written
			/// all these MUST be set before the PDSOoutputFile.open() call
			pof.setEmbeddedVicarLabel(readerVicarLabel);
			// create a System from the RenderedImage to use in the embedded VicarLabel
			// this will be substituted into the readerVicarLabel so the System part is correct relative to the RenderedImage
			SystemLabel embededVicarSystemLabel = createSystemLabel(ri);
			
			if (vbp != null) {
			  embededVicarSystemLabel.setBLType(bltype); // this may allow a user to properly interpret the binary line prefix
			  embededVicarSystemLabel.setBHost(bhost); 
			  embededVicarSystemLabel.setBIntFmt(bintfmt); 
			  embededVicarSystemLabel.setBRealFmt(brealfmt); 
			}
			
			if (vbh != null) {
				/* **/
				//  VicarBinaryLabel new_vbl = pof.createBinaryHeader();
				//  byte[] b = vbh.getData();
				 // copy data in?? 
				 // new_vbl.getBuffer()
				 
				 
				// pof.setBinaryHeader(vbl);
				
			}
						
			// VicarLabelSet sysSet = readerVicarLabel.getSystem();
			/** bltype, bhost, bintfmt, brealfmt are passed in the VicarBinaryLinePrefix Object passed in the metadata *
			* this is an alternate method. Putting the value in the metadata is better since 
			* transcoders can put the value in if needed.
			* 
			*/
			/**
			try {
			SystemLabel readerSystemLabel = new SystemLabel(sysSet);
			String bltype = readerSystemLabel.getBLType();
			embededVicarSystemLabel.setBLType(bltype);
			}
			catch (Exception e) {
			   System.out.println("PDSImageWriter.write() Exception creating SystemLabel from read VicarLabelSet");
			}
			****/
			
			// add back in the binary line prefix 
			// (which isn't in the RenderedImage used to create the system label)
			embededVicarSystemLabel.setNBB(nbb);
			embededVicarSystemLabel.calcRecsize();
			
			embededVicarSystemLabel.setNLB(nlb);
			
			pof.setEmbeddedSystemLabel(embededVicarSystemLabel); 
			pof.setEmbedVicarLabel(embedVicarLabel);
			pof.setDebug(debug);
			pof.setPds_ptr(pds_ptr);
			
			if (debug) {
			  System.out.println("***************************************************");
			  System.out.println("***************************************************");
			  System.out.println("***************************************************");
			  System.out.println("*###");
			  System.out.println("*###");
			  System.out.println("PDSImageWriter.write()" );
			  System.out.println("detachedLabelOnly = "+detachedLabelOnly);
			  System.out.println("dataFileIsVicarImage = "+dataFileIsVicarImage);
			  System.out.println("fakeImage = "+fakeImage);
			}
			if (detachedLabelOnly == true && dataFileIsVicarImage == true) {
				if (debug) {
					System.out.println("***************************************************");
					System.out.println("***");
					System.out.println("***");
					System.out.println("PDSImageWriter.write() ****************************");
					System.out.println("PDSImageWriter.write() detachedLabelOnly is TRUE ");
				}
				// do setup so only a label is written
				pof.setOutputFilename(outputFilename); // this is the filename the detached label is written to
				pof.setInputFilename(inputFilename);// this is the filename put into the ^IMAGE label item
				pof.setDetachedLabelOnly(true);
				pof.setDetachedLabel(true);
				pof.setEmbedVicarLabel(false);
				pof.setRecordLength(recordLength);
				pof.setVicarImageFileRecords(vicarImageFileRecords);
				pof.setVicarLabelRecordCt(vicarLabelRecordCt);
				
				
				if (debug) {
					System.out.println("PDSImageWriter.write() pof.open() *** "+output);
				}
				pof.open( output ); 
				if (debug) {
					System.out.println("PDSImageWriter.write() AFTER pof.open() --- don't write any more!!!!");
					System.out.println("***");
					System.out.println("***");
					System.out.println("***");
					System.out.println("***");
					System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
				}
				return;
			}
			// later we'll handle detachedLabel == true
			// in this case we'll write out the image data too
			
            // System.out.println("PDSImageWriter.write() vicarLabel.createHistoryTask");
            // vicarLabel.createHistoryTask("PDSImageWriter");
           
           // figure out where the output comes from
           
            if (debug) System.out.println("PDSImageWriter.write() pof.open() "+output);
           // pof.open((OutputStream) output); // output stream was supplied to the constructor
           pof.open( output ); // change PDSOutputFile and VicarOutputFile to accept an Object 
           // then that method will detect the Class and do something good
            if (debug) System.out.println("PDSImageWriter.write() after pof.open");
            if (detachedLabelOnly == true) {
            	if (debug) System.out.println("PDSImageWriter.write() detachedLabelOnly = "+detachedLabelOnly+ " don't write any data");
            	return;
            }
            
           
            // ImageWriter.setOutput(Object) which should be ImageOutputStream
            // open writes the label to the file

            // now write the data to the image, write the whole image as a single tile
            int startX = 0;
            int startY = 0;
            int x_off = 0;
            int y_off = 0;
            
            /// sampleModel is for a tile
            SampleModel sm = ri.getSampleModel();
            int tileWidth = sm.getWidth();
            int tileHeight = sm.getHeight();
            
            Raster tile ;
            DataBuffer db ; 
            if (debug) System.out.println("PDSWriter write() tileWidth="+tileWidth+"  tileHeight="+tileHeight );
            
            // loop thru the tiles to write out the entire image ???
            // the tiles must be read in(or grabbed from cache)
            // this loop should be the same as an update of the image
            // int txmin, txmax, tymin, tymax;
            int ti, tj;
            int minTileX = ri.getMinTileX();
            int maxTileX = ri.getMinTileX() + ri.getNumXTiles() - 1;
            int minTileY = ri.getMinTileY();
            int maxTileY = ri.getMinTileY() + ri.getNumYTiles() - 1;
        
            int txmin = minTileX;
            int txmax = maxTileX;
            int tymin = minTileY;
            int tymax = maxTileY;
            
            int tileGridXOffset = ri.getTileGridXOffset();
            int tileGridYOffset = ri.getTileGridYOffset();
            
            // loop thru all the tiles and write them out to the file
            // this may really only work if we write out a single tile
            for (tj = tymin; tj <= tymax; tj++) {
                for (ti = txmin; ti <= txmax; ti++) {
                	
                tile = ri.getTile(ti, tj);
                sm = tile.getSampleModel();
                db = tile.getDataBuffer(); 	
                
                tileWidth = sm.getWidth();
            	tileHeight = sm.getHeight();
                // tx and ty are the tile origin 
                // int tx = TileXtoX(ti);
                int tx = ti*tileWidth + tileGridXOffset;
                // int ty = TileYtoY(tj);
                int ty = tj*tileHeight + tileGridYOffset;
            
                // computeTile then write that tile back out
                //Raster ras = im.getData();
            
                // get this tiles raster and write it out to the file
                
            
                // if (debug) System.out.println("writeTile ti="+ti+" tj="+tj+"  tx="+tx+" ty="+ty );
                pof.writeTile(tx, ty, tileWidth,tileHeight, x_off, y_off, sm, db);
                }
            }
           } // catch all the exceptions from VicarIO calls
           catch (jpl.mipl.io.vicar.AlreadyOpenException aoe) {
                    System.out.println("AlreadyOpenException PDSImageWriter.write() "+aoe);
                    aoe.printStackTrace();
                    return;
           }
           catch (IOException ioe) {
                    System.out.println("IOException PDSImageWriter.write() "+ioe);
                    ioe.printStackTrace();
                    return;
           } 
    }
    
    
    // since this isn't in the ImageWriter API make this a private method.
    // for testing it is currently public
    private void write(RenderedImage ri, VicarLabel vicarLabel) {     
            
        if (output == null) {
                throw new IllegalStateException ("Output must be set");
        }
        DOMutils domUtil = new DOMutils();
        if (debug)  {
        	System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++++");
            System.out.println("PDSImageWriter.write() with PDSLabel using vicarIO");
        }
            
            PDSOutputFile pof = new PDSOutputFile();
                  
            if (pof == null) {
                System.out.println("PDSWriter.write(RenderedImage, VicarLabel) null PDSOutputFile: exiting write");
                // send an IOException ???
                return;
            }

            pof.setDebug(debug);
            // pof uses this to write the PDS Label for the image
            // ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( (BufferedImage) ri );
			ImageToPDS_DOM i2PDSdom = new ImageToPDS_DOM ( ri );
            // this isn't really needed. pof outputFilename will overide this one
            i2PDSdom.setFilename(outputFilename);
            Document d = i2PDSdom.getDocument();
            
            if (debug) {
            	domUtil.serializeNode(d,"pds_dom.xml","xml");
            }
            pof.setPdsDOM(d);
            pof.setOutputFilename(outputFilename);
            // set all the values fed in from the ImageWriteParam
            // these values  are defaults unless overridden by an ImageWriteParam
            
            
            
             jpl.mipl.io.vicar.SystemLabel systemLabel;
            // should check if we have a param and use it
            // VicarLabel vicarLabel = vof.getVicarLabel();
            // SystemLabel systemLabel = vof.getSystemLabel();
            
            /**
            * The VicarLabel is still VERY important.
            * VicarIO uses the Vicar Label to control how the image data is 
            * written to the file
            **********/
            try { // catch Exceptions for all the VicarIO methods called
             if (vicarLabel == null) {
                // jpl.mipl.io.vicar.VicarLabel 
                vicarLabel = new jpl.mipl.io.vicar.VicarLabel();
                if (debug) System.out.println("null VicarLabel creating one from the image");
                // jpl.mipl.io.vicar.SystemLabel systemLabel = createSystemLabel(sm);
                systemLabel = createSystemLabel(ri);
                
                pof.setSystemLabel(systemLabel);
                pof.setVicarLabel(vicarLabel);
              
             } else {
                // when I have a real vicarLabel do I need to also set the 
                // SystemLabel or is it set when I do setVicarLabel ???
                // setPrimaryInput(SystemLabel slbl, VicarLabel vlbl)
                // if systemLabel is null then the systemLabel from vicarLabel is used
                
                if (debug) System.out.println("VicarImageWriter.write() vof.setPrimaryInput");
                pof.setPrimaryInput(null, vicarLabel );
             }

            // System.out.println("PDSImageWriter.write() vicarLabel.createHistoryTask");
            // vicarLabel.createHistoryTask("PDSImageWriter");
           
           // figure out where the output comes from
             long cur_fpos = pof.get_current_file_pos();
            if (debug) {
            	System.out.println("PDSImageWriter.write() pof.open() "+output);
            	System.out.println("  cur_fpos="+cur_fpos+" ");
            	
            }
            // pof.open((OutputStream) output); // output stream was supplied to the constructor
            pof.open( output); // output stream was supplied to the constructor
           
            if (debug) System.out.println("PDSImageWriter.write() after pof.open");
           
            // ImageWriter.setOutput(Object) which should be ImageOutputStream
            // open writes the label to the file

            // now write the data to the image, write the whole image as a single tile
            int startX = 0;
            int startY = 0;
            int x_off = 0;
            int y_off = 0;
            
            /// sampleModel is for a tile
            SampleModel sm = ri.getSampleModel();
            
            int tileWidth = sm.getWidth();
            int tileHeight = sm.getHeight();
             
            Raster tile ;
            DataBuffer db ; 
            if (debug) {
            	System.out.println("PDSWriter sm "+sm );
            	System.out.println("  ri "+ri );
            	System.out.println("  ri.getTileWidth()="+ri.getTileWidth()+"  ri.getTileHeight()="+ri.getTileHeight()+" ");
            	System.out.println("  tileWidth="+tileWidth+"   tileHeight="+tileHeight );
            	System.out.println("  ri.getMinTileX()="+ri.getMinTileX()+"  ri.getNumXTiles() " + ri.getNumXTiles() );
            	System.out.println("  ri.getMinTileY()="+ri.getMinTileY()+"  ri.getNumYTiles() " + ri.getNumYTiles() );
            	System.out.println("  ri.getWidth()="+ri.getWidth()+"  ri.getHeight() " + ri.getHeight() );
            }
            
            // loop thru the tiles to write out the entire image ???
            // the tiles must be read in(or grabbed from cache)
            // this loop should be the same as an update of the image
            // int txmin, txmax, tymin, tymax;
            int ti = 0;
            int tj = 0;
            int minTileX = ri.getMinTileX();
            int maxTileX = ri.getMinTileX() + ri.getNumXTiles() - 1;
            int minTileY = ri.getMinTileY();
            int maxTileY = ri.getMinTileY() + ri.getNumYTiles() - 1;
        
            int txmin = minTileX;
            int txmax = maxTileX;
            int tymin = minTileY;
            int tymax = maxTileY;
            
            int tileGridXOffset = ri.getTileGridXOffset();
            int tileGridYOffset = ri.getTileGridYOffset();
            
            // loop thru all the tiles and write them out to the file
            // this may really only work if we write out a single tile
            cur_fpos = pof.get_current_file_pos();
            int ct=0;
            if (debug) {
            	System.out.println(ct+")  cur_fpos="+cur_fpos+"  tymin="+tymin+"  txmin="+txmin+"   tymax="+tymax+"  txmax="+txmax+"   " );
            	System.out.println("  ri.getTileHeight() "+ri.getTileHeight()+"  ri.getTileWidth() "+ri.getTileWidth());
            	System.out.println("   ");
            }
            int tx = 0;
            int ty = 0;
            int tsmWidth= 0;
            int tsmHeight = 0;
            
            int dbufSize = 0;
            for (tj = tymin; tj <= tymax; tj++) {
                for (ti = txmin; ti <= txmax; ti++) {
                // tx and ty are the tile origin 
                // int tx = TileXtoX(ti);
                tx = ti*tileWidth + tileGridXOffset;
                // int ty = TileYtoY(tj);
                ty = tj*tileHeight + tileGridYOffset;
            
                // computeTile then write that tile back out
                //Raster ras = im.getData();
            
                // get this tiles raster and write it out to the file
                tile = ri.getTile(ti, tj);
                sm = tile.getSampleModel();
                tsmWidth= sm.getWidth();
                tsmHeight= sm.getHeight();
                db = tile.getDataBuffer(); 
                               
                // pof.writeTile(tx, ty, tileWidth,tileHeight, x_off, y_off, sm, db);
                pof.writeTile(tx, ty, tsmWidth,tsmHeight, x_off, y_off, sm, db);
                if (debug) {
                	if (ct < 4 || ct == (tymax-1) || ct == tymax || ct % 1000 == 0) {
                		cur_fpos = pof.get_current_file_pos();
                		dbufSize = db.getSize();
                		System.out.println(ct+") cur_fpos="+cur_fpos+"  dbufSize="+dbufSize+"  ti="+ti+" tj="+tj+"  tx="+tx+" ty="+ty);
                		System.out.println("  "+ct+")       tileHeight() "+tileHeight+"    tileWidth() "+tileWidth );
                		System.out.println("  "+ct+") ri.getTileHeight() "+ri.getTileHeight()+"  ri.getTileWidth() "+ri.getTileWidth());
                		System.out.println("  "+ct+")          tsmHeight="+tsmHeight+"           tsmWidth="+tsmWidth+"  " );
                		System.out.println("  "+ct+")   tile.getHeight() "+tile.getHeight()+"    tile.getWidth() "+tile.getWidth() );
                	}
                }
                ct++;
                }
            }
            
            cur_fpos = pof.get_current_file_pos();
            
            if (debug) {
            	System.out.println("writeTile ct="+ct+"  tsmHeight="+tsmHeight+"   tsmWidth="+tsmWidth+"  " );
            	System.out.println("  ri.getTileHeight() "+ri.getTileHeight()+"  ri.getTileWidth() "+ri.getTileWidth());
            	System.out.println("  cur_fpos="+cur_fpos+" tx="+tx+" ty="+ty+"  tj="+tj+" ti="+ti+" tileHeight="+tileHeight+" tileWidth="+tileWidth+" ");
            }
           } // catch all yhe exceptions from VicarIO calls
           catch (jpl.mipl.io.vicar.AlreadyOpenException aoe) {
                    System.out.println("AlreadyOpenException PDSImageWriter.write() "+aoe);
                    aoe.printStackTrace();
                    return;
           }
           catch (IOException ioe) {
                    System.out.println("IOException PDSImageWriter.write() "+ioe);
                    ioe.printStackTrace();
                    return;
           } 
           
    }
    
    
    

    
    /**
* Creates a system label for the image.
* <br>
* If the image was read in from a vicar file then there is a label which is 
* pushed into the properties of the image. <br>
* One could get that instead of creating a new label. 
* If the image isn't from a vicar file a label MUST be created.
**/

public jpl.mipl.io.vicar.SystemLabel createSystemLabel(RenderedImage im) {
	
    jpl.mipl.io.vicar.SystemLabel sl = new jpl.mipl.io.vicar.SystemLabel();
            
            
    // check colorModel to set some of these items
    // ORG
    // FORMAT
    // HOST
    // INTFMT
    // REALFMT
    SampleModel sm = im.getSampleModel();
    int dataType = sm.getDataType();
    String formatStr = "BYTE";
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "BYTE";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "HALF";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "HALF"; // ??? IS THIS CORRECT
    if (dataType == DataBuffer.TYPE_INT) formatStr = "FULL";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "REAL";
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "DOUB";
    // COMP 
    sl.setFormat(formatStr);
    
    String org = "BSQ" ; // "BSQ" "BIL" "BIP"
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    
    
    // set org before we set other items, then auto calculations wuill be correct
    sl.setOrg(org);
    
    // sample model is for a tile, not the whole image
    // int height = sm.getHeight();
    // int width = sm.getWidth();
    int width = im.getWidth();
    int height = im.getHeight();
    int bands = sm.getNumBands();
    if (bands > 1 ) {
        org = "BIP";
    }
    int[] sampleSize = sm.getSampleSize();
    int b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
    if (debug)  {
    	System.out.println("height="+height+"  width="+width+"  bands="+bands );
    	System.out.println("dataElements="+elements+"  b0size="+b0size   );
    	for (int i=0 ; i< sampleSize.length ; i++) {
        	System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    	}
    }
    
    /****
    * sl.setDefaults();  is called automatically when thew SystemLabel is created
    * setDefaults sets host to "JAVA" but it isn't marked "valid" so it isn't
    * printed to the label when the file is written
    * we must mark it as valid
    * HIGH - x86-linux vax intel
    * LOW - sun java PPC
    * 
    * RIEEE - x86-linux vax intel
    * IEEE - sun java PPC
    ************************************************/
    
   
    
    sl.setHost("JAVA");  
    sl.setHostValid(true);
    
    sl.setIntFmt("HIGH");
    // sl.setIntFmt("LOW");
    sl.setIntFmtValid(true);
    
    if ((dataType == DataBuffer.TYPE_FLOAT) || (dataType == DataBuffer.TYPE_DOUBLE))  {
    	
    	sl.setRealFmt("IEEE");
    	sl.setRealFmtValid(true);
    }else {
		sl.setRealFmtValid(false);
    }
    
    // now set things where the defaults aren't correct
    sl.setNL(height);
    sl.setNS(width);
    sl.setNB(bands);
    // sl.calcRecsize(); // calculates recsize based on all the other values previuosly entered
    // called automatically by setNS etc
    sl.setBufsiz(sl.getRecsize()); // Bufsiz isn't used but should be set
    if (debug) {
    	System.out.println("SystemLabel:");
    	System.out.print(sl.toString());
    	System.out.println("-------------------------");
    }
    return sl;
    }
    
    

    public void dispose() {}

}
