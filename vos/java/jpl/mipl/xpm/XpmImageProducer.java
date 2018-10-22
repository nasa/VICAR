package jpl.mipl.xpm;

import java.awt.Color;
import java.awt.image.*;
import java.util.*;
import java.net.*;
import java.io.*;

/**
 * This class is an implementation of the ImageProducer interface which
 * uses an XPM format to produce pixel values for an Image.
 * 
 * @see ImageProducer
 * @version 1.0 05/14/98
 * @author Vadim Parizher
 */

public class XpmImageProducer implements ImageProducer {

  private static final String KEY_COLOR = "c";  // for color visual
  private static final String KEY_GRAY = "g";	  // for grayscale
  private static final String KEY_GRAY4 = "g4"; // for 4-level grayscale
  private static final String KEY_MASK = "m";   // for mono visual
  private static final String KEY_SYM = "s";    // for symbolic name

  protected String _imageData;
  protected Vector _consumers = new Vector();
  protected ColorModel _model;

  /**
   * Constructs an ImageProducer object which uses a string containing 
   * XPM image to produce data for the Image object.  Default RGB color model 
   * is used.
   * @see java.awt.Component#createImage
   * @see ColorModel#getRGBdefault
   */
  public XpmImageProducer(String imageData) {
    _imageData = imageData;
    _model = ColorModel.getRGBdefault();
  }

  /**
   * Constructs an ImageProducer object which uses a string containing
   * XPM image to produce data for the Image object.
   * @see java.awt.Component#createImage
   */
  public XpmImageProducer(String imageData, ColorModel colorModel) {
    _imageData = imageData;
    _model = colorModel;
  }

  /**
   * Constructs an ImageProducer object which uses a URL containing
   * XPM image file to produce data for the Image object.
   * @see java.awt.Component#createImage
   */
  public XpmImageProducer(URL imageData, ColorModel colorModel) {
    initURL(imageData);
    _model = colorModel;
  }

  /**
   * Constructs an ImageProducer object which uses a URL containing
   * XPM image file to produce data for the Image object.
   * @see java.awt.Component#createImage
   * @see ColorModel#getRGBdefault
   */
  public XpmImageProducer(URL imageData) {
    initURL(imageData);
    _model = ColorModel.getRGBdefault();
  }

  /**
   * Constructs an ImageProducer object which uses a file containing
   * XPM image file to produce data for the Image object.
   * @see java.awt.Component#createImage
   */
  public XpmImageProducer(File file, ColorModel colorModel) {
    initFile(file);
    _model = colorModel;
  }
 
  /**
   * Constructs an ImageProducer object which uses a file containing
   * XPM image file to produce data for the Image object.
   * @see java.awt.Component#createImage
   * @see ColorModel#getRGBdefault
   */
  public XpmImageProducer(File file) {
    initFile(file);
    _model = ColorModel.getRGBdefault();
  }

  /* 
   * Get image data from URL
   */
  private void initURL(URL imageData) {
    try {
      URLConnection conn = imageData.openConnection();
      conn.connect();
      BufferedReader data;
      data = new BufferedReader(new InputStreamReader(conn.getInputStream()));
      String line;
      while ((line = data.readLine()) != null) {
        _imageData += line;
      }
      data.close();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /*
   * Get image data from File
   */
  private void initFile(File file) {
    try {
      BufferedReader data;
      data = new BufferedReader(new InputStreamReader(new FileInputStream(file)));
      String line;
      while ((line = data.readLine()) != null) {
        _imageData += line;
      }
      data.close();
    }
    catch (FileNotFoundException e) {
      System.err.println("File not found!" + e);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Determine if an ImageConsumer is on the list of consumers currently 
   * interested in data for this image.
   * @return true if the ImageConsumer is on the list; false otherwise
   * @see ImageConsumer
   */
  public synchronized boolean isConsumer(ImageConsumer ic) {
    return _consumers.contains(ic);
  }

  /**
   * Remove an ImageConsumer from the list of consumers interested in data 
   * for this image.
   * @see ImageConsumer
   */ 
  public synchronized void removeConsumer(ImageConsumer ic) {
    _consumers.removeElement((Object)ic);
  }

  /**
   * Requests that a given ImageConsumer have the image data delivered one 
   * more time in top-down, left-right order.
   * @see ImageConsumer
   */
  public void requestTopDownLeftRightResend(ImageConsumer ic) {
    // Ignored.  The data is already in TDLR format.
  }

  /**
   * Adds an ImageConsumer to the list of consumers interested in data for 
   * this image, and immediately start delivery of the image data through the 
   * ImageConsumer interface.
   * @see ImageConsumer
   */
  public void startProduction (ImageConsumer ic) {
    addConsumer(ic);
  }

  /**
   * Adds an ImageConsumer to the list of consumers interested in data for
   * this image.
   * @see ImageConsumer
   */
  public synchronized void addConsumer(ImageConsumer ic) {
    if (isConsumer(ic)) {
      return;
    }
    _consumers.addElement(ic);
    try { 

      // initialize the consumer

      ic.setColorModel(_model);
      
      ic.setHints(ImageConsumer.SINGLEFRAME |
		  ImageConsumer.TOPDOWNLEFTRIGHT |
		  ImageConsumer.COMPLETESCANLINES);
      
      // parse _imageData string

      // verify the format

      if (!_imageData.startsWith("/* XPM */")) {
	System.err.println("Not XPM format!");
	ic.imageComplete(ImageConsumer.IMAGEERROR);
	return;
      }

      // remove all the comments

      _imageData = trimComments(_imageData);

      // trim the string to get rid of anything outside of { }

      _imageData = trimBrackets(_imageData);

      int width, height, numColors, cpp, xHotSpot, yHotSpot;
      width = height = numColors = cpp = xHotSpot = yHotSpot = 0;
      boolean xpmExt;

      StringTokenizer st = new StringTokenizer(_imageData, ",");
      String token;
      int starts, ends;
      int i;

      // parse Values section

      while ((token = st.nextToken()) != null) {
	starts = token.indexOf("\"") + 1;
	ends = token.lastIndexOf("\"");
	if (starts == 0) // Check if the line is good
	  continue;

	token = trimQuotes(token); // Trim quotes

	StringTokenizer values = new StringTokenizer(token);
	width = Integer.parseInt(values.nextToken());
	height = Integer.parseInt(values.nextToken());
	numColors = Integer.parseInt(values.nextToken());
	cpp = Integer.parseInt(values.nextToken());
	try {
	  xHotSpot = Integer.parseInt(values.nextToken());
	  yHotSpot = Integer.parseInt(values.nextToken());
	} catch (NoSuchElementException e) {
	  xHotSpot = -1;
	  yHotSpot = -1;
	}
	try {
	  xpmExt = values.nextToken().equals("XPMEXT");
	} catch (NoSuchElementException e) {
	  xpmExt = false;
	}
	break;
      }
      ic.setDimensions(width, height);

      // parse Colors section

      Hashtable colorTable = new Hashtable();
      for (;;) {
	token = st.nextToken();
	starts = token.indexOf("\"") + 1;
	ends = token.lastIndexOf("\"");
	if (starts == 0)
	  continue;
	token = trimQuotes(token);
	String chars = token.substring(0, cpp);
	token = token.substring(cpp);
	Hashtable keyTable = new Hashtable();
	StringTokenizer colors = new StringTokenizer(token);
	for (;;) {
	  try {
	    String key = colors.nextToken();
	    String colorString = colors.nextToken();
	    Color color = XpmColorTable.getColor(colorString.toLowerCase());
	    keyTable.put(key, color);
	  } catch (NoSuchElementException e) { // go on to the next line
	    break;
	  } catch (NullPointerException e) { // from getColor()
	    System.err.println("Unidentified color!");
	  }	  
	}
	colorTable.put(chars, keyTable);
	numColors--;
	if (numColors == 0)
	  break;
      }

      // parse Pixels section

      // icons are typically small, so there is no problem putting them all 
      // in memory.

      int pix[] = new int[width * height];
      int index = 0;
      int numLines = height;
      while (numLines > 0) {
	token = st.nextToken();
	starts = token.indexOf("\"") + 1;
	ends = token.lastIndexOf("\"");
	if (starts == 0)
	  continue;
	token = trimQuotes(token); // strip quotes
	for (int p = 0; p < width; p+=cpp) {
	  String chars = token.substring(p, p+cpp);
	  Hashtable ht = (Hashtable)colorTable.get(chars);
	  Color color = (Color)ht.get(KEY_COLOR);
	  pix[index++] = (255 << 24) | (color.getRed() << 16) | 
	                 (color.getGreen() << 8) | color.getBlue();
	}
	numLines--;
      }
      ic.setPixels(0, 0, width, height, _model, pix, 0, width);
      ic.imageComplete(ImageConsumer.STATICIMAGEDONE);
    } catch (Exception e) {
      System.err.println("Exception:" + e);
      if (isConsumer(ic)) {
	ic.imageComplete(ImageConsumer.IMAGEERROR);
      }
    }
  }

  static protected String trimComments(String string)
  {
    int begin, end;
    while (true) {
      begin = string.indexOf("/*");
      end = string.indexOf("*/", begin);
      if (begin == -1 || end == -1)
        break;
      String string1 = string.substring(0, begin);
      String string2 = string.substring(end+2, string.length());
      string = string1 + string2;
    }
    return string;
  }
  
  static protected String trimBrackets(String string)
  {
    int begin, end;
    begin = string.indexOf('{');
    end = string.lastIndexOf('}');
    string = string.substring(begin+1, end);
    return string;
  }

  static protected String trimQuotes(String string)
  {
    int begin, end;
    begin = string.indexOf('"');
    end = string.lastIndexOf('"');
    string = string.substring(begin+1, end);
    return string;
  }
}

