package jpl.mipl.mars.pig.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import javax.imageio.IIOImage;

import jpl.mipl.io.ImageUtils;

/**
 * Utility class that will load an IIOImage. 
 *
 * @author Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
 * @version $Id: $
 *
 */
public class IIOImageUtils
{
    /**
     * Returns new IIOImage based on image location (local or
     * remote)
     * @param imageLocation Location of image
     * @return IIOImage instance of image, or null if error occurs
     */
    public static IIOImage loadImage(String imageLocation)
    {
        IIOImage image = null;
        
        boolean isLocal = true;
        try {
            File testFile = new File(imageLocation);
            if (!testFile.isFile())
                isLocal = false;
        } catch (Exception ex) {
            isLocal = false;
        }
        
        
        final ImageUtils imageUtils = new ImageUtils();
        imageUtils.setGetAsRenderedImage(true);
        
        if (isLocal)
        {
            try {
                image = imageUtils.fullRead(imageLocation);
            } catch (IOException ioEx) {
                image = null;
            }
        }
        else
        {
            InputStream is = null;
            
            try { 
                URL url = new URL(imageLocation);
                is = url.openStream();                
                image = imageUtils.fullRead(is);
            } catch (Exception ex) {
                is = null;
                image = null;
            } finally {
                if (is != null)
                {
                    try {
                        is.close();
                    } catch (Exception closeEx) {                        
                    }
                }
            }
        }
        
        return image;
    }
    
    //-------------------------------------
    
    public static IIOImage loadImage(InputStream imageStream)
    {
        IIOImage image = null;
              
        final ImageUtils imageUtils = new ImageUtils();
        imageUtils.setGetAsRenderedImage(true);
        
      
        {
            InputStream is = null;
            
            try { 
            
                image = imageUtils.fullRead(imageStream);
            } catch (Exception ex) {
                is = null;
                image = null;
            } finally {
                if (is != null)
                {
                    try {
                        is.close();
                    } catch (Exception closeEx) {                        
                    }
                }
            }
        }
        
        return image;
    }
    
}
