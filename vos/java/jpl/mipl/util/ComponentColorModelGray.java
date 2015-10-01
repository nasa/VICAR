package jpl.mipl.util;

import java.awt.*;
import java.awt.color.*;
import java.awt.image.*;
import javax.media.jai.*;

/**
 * This class is a temporary(we hope) hack to fix Sun's problem
 * handling ColorSpace.TYPE_GRAY images in jdk1.3.
 * Due to changes in java.awt.image.ComponentColorModel the programs 
 * that use ColorSpace.TYPE_GRAY under certain circumstances execute 
 * at least Two Orders of Magnitude slower when using jdk1.3 vs. jdk1.2
 * Sun's Bug Database# 4386450
 * Here is the (un)official Sun's response:
 * <p>
 * "The getRed(), getGreen(), and getBlue() methods in ComponentColorModel
 * were modified in 1.3 to bring them into line with the API
 * specification which says that the RGB component values are returned in
 * the sRGB color space.  The previous grayscale optimization did not do
 * this, so the initial fix was to treat the grayscale case just as any
 * other non-sRGB case.  We anticipated adding gray scale optimizations
 * in a subsequent release.
 * <p>
 * We were aware that it was possible that some uses of the API might
 * rely on these methods in performance-critical code, so we analyzed
 * various grayscale cases in the Java 2D code, and we asked the JAI team
 * if they considered these methods performance-critical.  Unfortunately,
 * we did not consult with the Java3D team.
 * <p>
 * The JAI team did not raise an objection at the time.  Our analysis of
 * grayscale handling in the 2D API was that this would only have an
 * effect when rendering with a grayscale image which included an alpha
 * channel, which we didn't consider to be a common case.
 * <p>
 * We are now looking at how to optimize the grayscale case for the
 * Merlin release.  One possibility would be to special case the
 * LINEAR_GRAY built-in gray space, which would involve a single lookup
 * table.  Another possibility is to cache a lookup table for each
 * grayscale space the first time we see it, but this would mean
 * potentially allocating several lookup tables."
 *
 * <p>
 *
 * The following code shows the typical use of this class:
 * <pre>
 * ColorModel colorModel = 
 *         ComponentColorModelGray.adjustColorModel(
 *                                   image.getSampleModel(),
 *                                   image.getColorModel());
 *
 * </pre>
 */
public class ComponentColorModelGray extends ComponentColorModel {

    /**
     * @serial
     */
    private boolean _supportsAlpha;
    /**
     * @serial
     */
    private boolean _isAlphaPremultiplied;
    /**
     * @serial
     */
    private int     _transferType;
    /**
     * @serial
     */
    private int[]   _nBits;

    /**
     * Constructs a <CODE>ComponentColorModel</CODE> from the specified 
     * parameters. Color components will be in the specified 
     * <CODE>ColorSpace</CODE>.  The <CODE>bits</CODE> array specifies the 
     * number of significant bits per color and alpha component.  Its
     * length should be the number of components in the 
     * <CODE>ColorSpace</CODE> if there is no alpha 
     * information in the pixel values, or one more than this number if 
     * there is alpha information.  An <CODE>IllegalArgumentException</CODE> 
     * is thrown if the length of the array does not match the number of 
     * components.  <CODE>hasAlpha</CODE> indicates whether alpha
     * information is present.  If <CODE>hasAlpha</CODE> is true, then 
     * the boolean <CODE>isAlphaPremultiplied</CODE> 
     * specifies how to interpret color and alpha samples in pixel values.  
     * If the boolean is true, color samples are assumed to have been 
     * multiplied by the alpha sample. The <CODE>transparency</CODE> 
     * specifies what alpha values can be represented by this color model.
     * The acceptable <code>transparency</code> values are
     * <CODE>OPAQUE</CODE>, <CODE>BITMASK</CODE> or <CODE>TRANSLUCENT</CODE>.
     *  The <CODE>transferType</CODE> is the type of primitive array used
     * to represent pixel values.  Note that the <CODE>bits</CODE> array
     * contains the number of significant bits per 
     * color/alpha component after the translation from pixel values.
     *
     * @param colorSpace       The <CODE>ColorSpace</CODE> associated 
     *                         with this color model.
     * @param bits             The number of significant bits per component.
     * @param hasAlpha         If true, this color model supports alpha.
     * @param isAlphaPremultiplied If true, alpha is premultiplied.
     * @param transparency     Specifies what alpha values can be represented
     *                         by this color model.
     * @param transferType     Specifies the type of primitive array used to
     *                         represent pixel values.
     *
     * @throws IllegalArgumentException If the length of the 
     *         <CODE>bits</CODE> array does not match the number of components.
     *
     * @see ComponentColorModel
     * @see ColorSpace
     * @see java.awt.Transparency
     */
    public ComponentColorModelGray (ColorSpace colorSpace,
				    int[] bits,
				    boolean hasAlpha,
				    boolean isAlphaPremultiplied,
				    int transparency,
				    int transferType) 
    {
	super (colorSpace, bits, hasAlpha, isAlphaPremultiplied, 
	       transparency,transferType);

	_supportsAlpha = hasAlpha;
	_isAlphaPremultiplied = isAlphaPremultiplied;
	_nBits = (int[])bits.clone();
	_transferType = transferType;
    }    
 
    /**
     * Given sample Model and Color Model, return original CM if it's okay,
     * or a new CCMG if it's not.
     */
    public static ColorModel adjustColorModel(SampleModel sampleModel,
					      ColorModel colorModel)
    {
	//Check for the JDK 1.3 ColorModel bug.  Only seems to manifest
	//itself (so far) in a single-band BandedSampleModel images.
	if((sampleModel.getNumBands() == 1) && 
	   ((sampleModel instanceof BandedSampleModel) ||
	    (sampleModel instanceof ComponentSampleModelJAI))) {
	    colorModel = new ComponentColorModelGray(
			       colorModel.getColorSpace(),
			       sampleModel.getSampleSize(),
			       colorModel.hasAlpha(),
			       colorModel.isAlphaPremultiplied(),
			       colorModel.getTransparency(),
			       sampleModel.getTransferType());
	}
	return colorModel;
    } 

    /**
     * Returns the red color component for the specified pixel.
     * Assumes ColorSpace.TYPE_GRAY, thus just calls the method 
     * that returns a gray color that can be mapped to RGB values.
     *
     * @param inData The pixel from which you want to get the red color 
     * component, specified by an array of data elements of type 
     * <CODE>transferType</CODE>.
     *
     * @return The red color component for the specified pixel, as an int.
     */
    public int getRed(Object inData) 
    {
	return getGray(inData);
    }
   
    /**
     * Returns the green color component for the specified pixel.
     * Assumes ColorSpace.TYPE_GRAY, thus just calls the method 
     * that returns a gray color that can be mapped to RGB values.
     *
     * @param inData The pixel from which you want to get the green color 
     * component, specified by an array of data elements of type 
     * <CODE>transferType</CODE>.
     *
     * @return The green color component for the specified pixel, as an int.
     */
    public int getGreen(Object inData) 
    {
	return getGray(inData);
    }
   
    /**
     * Returns the blue color component for the specified pixel.
     * Assumes ColorSpace.TYPE_GRAY, thus just calls the method 
     * that returns a gray color that can be mapped to RGB values.
     *
     * @param inData The pixel from which you want to get the blue color 
     * component, specified by an array of data elements of type 
     * <CODE>transferType</CODE>.
     *
     * @return The blue color component for the specified pixel, as an int.
     */
    public int getBlue(Object inData) 
    {
	return getGray(inData);
    }

    /** This method returns a gray color that can be mapped to
     * RGB values in getRGB().  
     * It assumes that the colorspace is TYPE_GRAY.
     */  
    private int getGray(Object inData) 
    {
        boolean needAlpha = (_supportsAlpha && _isAlphaPremultiplied);
        int alp = 0;
        int gray;
        switch (transferType) {
        case DataBuffer.TYPE_BYTE:
            byte bdata[] = (byte[])inData;
            gray = bdata[0] & 0xff;
            if (needAlpha) {
                alp = bdata[1]&0xff;
            }
            break;
        case DataBuffer.TYPE_USHORT:
            short sdata[] = (short[])inData;
            gray = sdata[0] & 0xffff;
            if (needAlpha) {
                alp = sdata[1]&0xff;
            }
            break;
        case DataBuffer.TYPE_INT:
            int idata[] = (int[])inData;
            gray = idata[0];
            if (needAlpha) {
                alp = idata[1]&0xff;
            }
            break;
        default:
            throw new UnsupportedOperationException("This method has not been"+
                                        " implemented for transferType " +
                                                    transferType);
        }

	if (_nBits[0] != 8) {
            int shift = _nBits[0] - 8;
            gray = ((shift > 0)
                    ? (gray>>shift)
                    : (gray<<(-shift)));
        }

        return (!needAlpha
                ? gray
                : ((alp != 0)
		   //? (int)(gray*(1<<nBits[numColorComponents])-1.f)/alp
		   ? (int)(gray*(1<<_nBits[1])-1.f)/alp
                    : 0)
                );

    }

    /**
     * Returns a data element array representation of a pixel in this
     * <CODE>ColorModel</CODE>, given an integer pixel representation 
     * in the default RGB color model.
     * This array can then be passed to the <CODE>setDataElements</CODE> 
     * method of a <CODE>WritableRaster</CODE> object.  
     * If the <CODE>pixel</CODE> 
     * parameter is null, a new array is allocated.
     * Assumes ColorSpace.TYPE_GRAY, thus uses gray scale optimization.
     *
     * @param rgb
     * @param pixel The integer representation of the pixel.
     *
     * @throws UnsupportedOperationException If the transfer type of 
     * this <CODE>ComponentColorModel</CODE>
     * is not one of the supported transfer types:  
     * <CODE>DataBuffer.TYPE_BYTE</CODE>, <CODE>DataBuffer.TYPE_USHORT</CODE>, 
     * or <CODE>DataBuffer.TYPE_INT</CODE>.
     *
     * @see WritableRaster#setDataElements
     * @see SampleModel#setDataElements
     */  
    public Object getDataElements(int rgb, Object pixel) 
    {
        //REMIND: maybe more efficient not to use int array for
        //DataBuffer.TYPE_USHORT and DataBuffer.TYPE_INT
        int intpixel[] = null;
        if (transferType == DataBuffer.TYPE_INT &&
            pixel != null) {
           intpixel = (int[])pixel;
        } else {
            intpixel = new int[1];
        }
        // REMIND: Use rendering hints?
        
	//        if (! is_sRGB) {
	if(true) {
            // REMIND: possible gray scale optimization here
	    //       if (colorSpaceType == ColorSpace.TYPE_GRAY) {
	    if (true) {
                double gray = ((((rgb>>16)&0xff)*.299/255) +
                               (((rgb>>8) &0xff)*.587/255) +
                               (((rgb)    &0xff)*.114/255));
                
                intpixel[0] = (int) (gray * (1 << _nBits[0]));

                if (_supportsAlpha) {
                    if (_nBits[1] == 8) {
                        intpixel[1] = (rgb>>24)&0xff;
                    }
                    else {
                        intpixel[1] =
                            (int)(((rgb>>24)&0xff)/255.f * ((1<<_nBits[1])-1));
                    }
                }
	    }
        }

        switch (_transferType) {
            case DataBuffer.TYPE_BYTE: {
               byte bdata[];
               if (pixel == null) {
                   bdata = new byte[1];
               } else {
                   bdata = (byte[])pixel;
               }
               for (int i = 0; i < 1; i++) {
                   bdata[i] = (byte)(0xff&intpixel[i]);
               }
               return bdata;
            }
            case DataBuffer.TYPE_USHORT:{
               short sdata[];
               if (pixel == null) {
                   sdata = new short[1];
               } else {
                   sdata = (short[])pixel;
               }
               for (int i = 0; i < 1; i++) {
                   sdata[i] = (short)(intpixel[i]&0xffff);
               }
               return sdata;
            }
            case DataBuffer.TYPE_INT:
               return intpixel;
        }
        throw new IllegalArgumentException("This method has not been "+
                 "implemented for transferType " + transferType);
    }
}

