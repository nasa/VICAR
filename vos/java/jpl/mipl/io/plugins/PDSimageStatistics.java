package jpl.mipl.io.plugins;

import java.awt.image.*;

import org.w3c.dom.Document;
import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.*;

/**
 * @author Steve Levoe
 *
 * PDSimageStatistics.java
 * 
 * This class contains the PDS IMAGE object statistics. Some other items are also contained
 * which are not calculated. They are here since they are a part of the IMAGE object.
 */
public class PDSimageStatistics {
	
	boolean dirty = false; // flag to indicate if the image's data has changed
	// later we may add a way to search for a default script in the jar file
	boolean addMerItems = false; // everything besides the statistics variables
	boolean addStatistics = false;
    boolean calculateStatistics = false;
    boolean calculated = false;
    
    boolean debug = false;
    
    /*
     The required parts of the IMAGE object
     (these aren't handled in this class)
     LINES
     LINE_SAMPLES
     SAMPLE_TYPE
     SAMPLE_BITS
     BANDS      */
    
    
    /* these values must be calculated somewhere and passed to the writer via params */
    
    // String unknown = "I don't know";
    // String unknown = "UNK";
    String unknown = "NULL";
    
    int first_line = 1;
    String first_lineString = null;
    int first_line_sample = 1;
    String first_line_sampleString = null;
    String sample_bit_mask = null ; // 2#0000111111111111#";
    
    int invalid_constant = 0; 
    String invalid_constantString = null;
    int missing_constant = 0;
    String missing_constantString = null;
    
    // therse are calculated here
    // flag to indicate values are UNKNOWN? 
    
    double mean = 0.0;
    String meanString = null;
    double median = 0.0;
    String medianString = null;
    double maximum = 0.0;
    String maximumString = null;
    double minimum = 0.0;
    String minimumString = null;
    double standard_devation = 0.0;
    String standard_deviationString = null;
    String checksumString = null;
    
    RenderedImage image = null; // the image that statistics are calculated from
    
    // constructor
    
    /**
     * If no values are added after the object is constructed then when values are requested 
     * from this class then default ("UNK") values will be returned. This is useful when a 
     * placeholder values must be aded to a label.
     * if calculateStatistics is false nothing will be calculated and the defaults 
     * will be returned.     */
    public PDSimageStatistics () {
    	setDefaults();
    	
    }
    
    // constructor where all the values are passed in
    // maybe someday??
    
    public void setDebug(boolean f) {
    	debug = f;
    }
    
    public boolean getADebug() {
    	return debug ;
    }
    
    public void setImage(RenderedImage im) {
    	image = im;
    }
    
    public RenderedImage getImage() {
    	return image ;
    }
    
    public void setAddMerItems(boolean f) {
    	addMerItems = f;
    }
    
    public boolean getAddMerItems() {
    	return addMerItems ;
    }
    
    public void setAddStatistics(boolean f) {
    	addStatistics = f;
    }
    
    public boolean getAddStatistics() {
    	return addStatistics ;
    }
    
    public void setCalculateStatistics(boolean f) {
    	calculateStatistics = f;
    }
    
    public boolean getCalculateStatistics() {
    	return calculateStatistics ;
    }
    
    /*  
     * Calculating the values takes time, set a flag so we don't do it unless we need to.     */
    public boolean isCalculated() {
    	return calculated;
    }
    
    public void setDefaults() {
    	
    	calculated = false;
    	addMerItems = false;
    	addStatistics = false;
    	calculateStatistics = false;
    
    	first_line = 1;
    	first_lineString = "1"; // unknown;
    	first_line_sample = 1;
    	first_line_sampleString = "1"; // unknown;
    	// sample_bit_mask = "2#0000111111111111#";
    	sample_bit_mask = unknown;
    
    	invalid_constant = 0; 
    	invalid_constantString = "0"; // unknown;
    	missing_constant = 0;
    	missing_constantString = "0"; // unknown;
    
    // these are calculated here
    // flag to indicate values are UNKNOWN? 
    
    	mean = 0.0;
   		meanString = unknown;
    	median = 0.0;
    	medianString = unknown;
    	maximum = 0.0;
    	maximumString = unknown;
    	minimum = 0.0;
    	minimumString = unknown;
    	standard_devation = 0.0;
    	standard_deviationString = unknown;
    	checksumString = unknown;
    }
    
    /*
     * Calculate all the statistics values     */
    public void calculateStatistics() {
    	// calculate each statstic
    	
    	// use JAI operators on the image to calculate these
    	// max min mean standard deviation will probably all be calculated
    	// together
    	// checksum will be independant
    	if (debug) {      		
       		System.out.println("-- PDSimageStatistics --- calculateStatistics "+calculated);
    	}
    	
     if (calculated == false )	{
     	if (debug) {      		
       		System.out.println("-- PDSimageStatistics --- calculating Statistics ");
    	}
    	calculated = true;
    	// take the calculated values and set them into the String for 
    	// each value
    	unknown = "calculate_not_implemented";
    	first_lineString = unknown;
    	first_line_sampleString = unknown;
    	sample_bit_mask = unknown ; // 2#0000111111111111#";
    
	    	
	    String invalid_constantString = unknown;
	    missing_constantString = unknown;
    
    // these are calculated here
    // flag to indicate values are UNKNOWN? 
    
    	meanString = unknown;
    	medianString = unknown;
    	maximumString = unknown;
    	minimumString = unknown;
    	standard_deviationString = unknown;
    	checksumString = unknown;
     }
    	
    }
    
    
    /**
     * add "item" elements to the supplied object. Everthing else is setup
     * so a user can call this method.     * @param object the object these items will be added to     */
    public void addItems(Document document, Element object, String keyString) {
    	
    	
       if (debug) {
       		System.out.println("-------------------------------------------");
       		System.out.println("-------------------------------------------");
       		System.out.println("--------------- PDSimageStatistics --------  addItems");
       }
          	
       if (addStatistics && calculateStatistics) { 
          		// only do this if required
          calculateStatistics();
        }
          	
      try {  		
          	    		
    	Element item;
        Text text;
          
    	if (addMerItems) {
    		if (debug) System.out.println("------------ addMerItems ----------------");
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "FIRST_LINE"); 
          	item.setAttribute("quoted", "false"); 
	      	text = (Text) document.createTextNode(first_lineString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "FIRST_LINE_SAMPLE"); 
          	item.setAttribute("quoted", "false"); 
	      	text = (Text) document.createTextNode(first_line_sampleString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "SAMPLE_BIT_MASK"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(sample_bit_mask);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "INVALID_CONSTANT"); 
          	item.setAttribute("quoted", "false"); 
	      	text = (Text) document.createTextNode(invalid_constantString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "MISSING_CONSTANT"); 
          	item.setAttribute("quoted", "false"); 
	      	text = (Text) document.createTextNode(missing_constantString);
	      	item.appendChild(text);
          	object.appendChild(item);
    	 } // end of the MER specific, but not statistics items
    	
        
        if (addStatistics)  {	
        	if (debug) System.out.println("------------ addStatistics ---------------");
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "MEAN"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(meanString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "MEDIAN"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(medianString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "MAXIMUM"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(maximumString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "MINIMUM"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(minimumString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "STANDARD_DEVIATION"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(standard_deviationString);
	      	item.appendChild(text);
          	object.appendChild(item);
          	
          	item = (Element) document.createElement("item"); 
          	item.setAttribute(keyString, "CHECKSUM"); 
          	item.setAttribute("quoted", "true"); 
	      	text = (Text) document.createTextNode(checksumString);
	      	item.appendChild(text);
          	object.appendChild(item);
           }
          
          } catch (Exception e) {
            // Parser with specified options can't be built
            System.out.println("ImageToPDS_DOM.buildDOM() Exception "+ e );
            e.printStackTrace();

        }
        
        if (debug) {
        	System.out.println("PDSimagStatistics.adItems() "+document);
        }
    }

}
