/**
 * ScaleMultiresOpImage
 * 
 * see javax.media.jai.MultiResolutionRenderableImage
 * 
 * This class calls the Scale operator. If it is given a 
 * CollectionImage or RenderedImageList it will choose the 
 * proper image from the List and then pass it to the Scale
 * operator.
 * The functionality is already in javax.media.jai.MultiResolutionRenderableImage
 * This is packaging of that functionality as an Operator so it can be 
 * used in a render chain.
 * 
 *
 *
 * Copyright 2006, by the California Institute of Technology. ALL RIGHTS RESERVED.
 * United States Government Sponsorship acknowledged. Any commercial use must be 
 * negotiated with the Office of Technology Transfer at the California Institute 
 * of Technology.
 * This software may be subject to U.S. export control laws and regulations.  
 * By accepting this document, the user agrees to comply with all applicable U.S. 
 * export laws and regulations. User has the responsibility to obtain export 
 * licenses, or other export authority as may be required before exporting such 
 * information to foreign countries or providing access to foreign persons. 
 *
 * NASA/JPL/MIPL
 *
 * Steve Levoe JPL
 */



package jpl.mipl.io.plugins;

import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.ParameterBlock;
import java.util.Map;
import java.util.Vector;

import javax.media.jai.BorderExtender;
import javax.media.jai.ImageLayout;
import javax.media.jai.Interpolation;
import javax.media.jai.JAI;
import javax.media.jai.RenderedImageList;
import javax.media.jai.ScaleOpImage;

public class ScaleMultiresOpImage extends ScaleOpImage {
	
	 /** An array of RenderedImage sources. */
    protected transient RenderedImage[] renderedSource;
    private int numSources;
    
    private boolean isMultiRes = false;

    /** The aspect ratio, derived from the highest-resolution source. */
    protected float aspect;

    /** The min X coordinate in Renderable coordinates. */
    protected float minX;

    /** The min Y coordinate in Renderable coordinates. */
    protected float minY;

    /** The width in Renderable coordinates. */
    protected float width;

    /** The height in Renderable coordinates. */
    protected float height; // is this the requested height?
    // do I need an absolute height, width of full image?
    float renderedHeight ; // final height after scale is applied
    float renderedWidth ; // final height after scale is applied
    float scaleX;
    float scaleY; // arguments from constructor
    
    int maxResWidth; 
    int maxResHeight;
    
    ScaleOpImage scaleOp = null; // this is the operator we actually create
    // use to delegate any calls to the operator

	public ScaleMultiresOpImage(RenderedImage source, Map configuration,
            boolean cobbleSources, ImageLayout layout,
			BorderExtender extender, Interpolation interp, float scaleX, float scaleY, 
			float transX, float transY) {
		
		/* I'm not sure if I need to create the super or not?? 
		 * May create the specific ScaleOp instead ??*/
		super(source, layout, configuration, cobbleSources, extender, interp,
				scaleX, scaleY, transX, transY);
		
		
		// check if the source is really a CollectionImage or RenderedImageList
		// if it is construct renderedSources from it
		if (source instanceof RenderedImageList) { // CollectionImage
			System.out.println("ScaleMultiresOpImage  RenderedImageList");
			isMultiRes = true;
			RenderedImageList ril = (RenderedImageList)source;
			// create a vector of the RenderedImages in the List
			// didn't we just do this to create the RenderedImageList to begin with ?
			System.out.println("ril sources Vector ");
			Vector renderedSources = new Vector();
			for (int i = 0; i<ril.size() ; i++) {
				System.out.println(i+") "+ril.get(i));
				renderedSources.add(ril.get(i));
			}
			
			numSources = renderedSources.size();
	        this.renderedSource = new RenderedImage[numSources];
	        for (int i = 0; i < numSources; i++) {
	            this.renderedSource[i] =
	                (RenderedImage)renderedSources.elementAt(i);
	        }

	        int maxResWidth = renderedSource[0].getWidth();
	        int maxResHeight = renderedSource[0].getHeight();
	        aspect = (float)maxResWidth/maxResHeight;

	        
	        width = height*aspect;
	        // renderedHeight ; 
	        // renderedWidth ;
			// get minX minY from source ??
			
			// minX = 0.0f;
			// minY = 0.0f;
			// should these come from the selected image?? I think these are the biggest
			// first image, they are used to determine which sub image should be used for 
			// a given absolute scale
			minX = source.getMinX();
			minY = source.getMinY();
			
			height = (float) source.getHeight(); //
		}
		else {
			System.out.println("ScaleMultiresOpImage  RenderedImage");
			// this is a single RenderedImage (PlanarImage? BufferedImage)
			isMultiRes = false;
			numSources = 1;
			this.renderedSource = new RenderedImage[numSources];
			this.renderedSource[0] = source;
               
		}
		
		

	}
	
	/**
	 * This is the probable route from
	 * @param scaleX
	 * @param scaleY
	 * @param hints
	 * @return
	 */
	public RenderedImage createScaledRendering(float scaleX, float scaleY,
            RenderingHints hints) {
		System.out.println("ScaleMultiresOpImage.createScaledRendering scaleX="+
				scaleX+" scaleX="+scaleY);
		
		
		int width  = (int) ((float) maxResWidth * scaleY); 
		int height = (int) ((float) maxResHeight * scaleY);
		
		System.out.println("  maxResWidth="+maxResWidth+" maxResHeight="+maxResHeight+
				"width=" +width+" height="+height);
		// calculate width height based on  scale factor
		RenderedImage ri = createScaledRendering(width, height, hints) ;
		return ri;
	}
	
	//----------------------------------------------------
	 /**
     * Returns a rendering with a given width, height, and rendering
     * hints.
     *
     * <p> If a JAI rendering hint named
     * <code>JAI.KEY_INTERPOLATION</code> is provided, its
     * corresponding <code>Interpolation</code> object is used as an
     * argument to the JAI operator used to scale the image.  If no
     * such hint is present, an instance of
     * <code>InterpolationNearest</code> is used.
     *
     * @param width the width of the rendering in pixels.
     * @param height the height of the rendering in pixels.
     * @param hints a Hashtable of rendering hints.
     * @throws IllegalArgumentException if width or height are non-positive.
     */
    public RenderedImage createScaledRendering(int width,
                                               int height,
                                               RenderingHints hints) {
        if(width <= 0 && height <= 0) {
            throw new IllegalArgumentException("ScaleMultiresOpImage.createScaledRendering width and height <= 0");
			 //   JaiI18N.getString("MultiResolutionRenderableImage1"));
        }

        System.out.println("ScaleMultiresOpImage.createScaledRendering  maxResWidth="+maxResWidth+" maxResHeight="+maxResHeight+
				"width=" +width+" height="+height);
        
        int res = numSources - 1;
        while (res > 0) {
            if(height > 0) {
                int imh = renderedSource[res].getHeight();
                if (imh >= height) {
                    break;
                }
            } else {
                int imw = renderedSource[res].getWidth();
                if (imw >= width) {
                    break;
                }
            }
            res--;
        }

        RenderedImage source = renderedSource[res];
        if(width <= 0) {
            width = (int)Math.round(height*source.getWidth()/source.getHeight());
        } else if(height <= 0) {
            height = (int)Math.round(width*source.getHeight()/source.getWidth());
        }
        double sx = (double)width/source.getWidth();
        double sy = (double)height/source.getHeight();
        double tx = (getMinX() - source.getMinX())*sx;
        double ty = (getMinY() - source.getMinY())*sy;

        Interpolation interp =
            Interpolation.getInstance(Interpolation.INTERP_NEAREST);
        if (hints != null) {
            Object obj = hints.get(JAI.KEY_INTERPOLATION);
            if (obj != null) {
                interp = (Interpolation)obj;
            }
        }

        
        // ParamterBlockJAI
        ParameterBlock pb = new ParameterBlock();
        pb.addSource(source);
        pb.add((float)sx);
        pb.add((float)sy);
        pb.add((float)tx);
        pb.add((float)ty);
        pb.add(interp);

        return JAI.create("scale", pb, hints);
    }

	
	
	
	
	//----------------------------------------------------
	/* 
	 * these are all the methods of ScaleOpImage
	 * see if they are ever called
	 */
	// backwardsMapRect
	 protected Rectangle backwardMapRect(Rectangle destRect,
             int sourceIndex) {
		 System.out.println("ScaleMultiresOpImage backwardMapRect");
		return super.backwardMapRect(destRect, sourceIndex);
	 }
	// computeTile
	 public Raster computeTile(int tileX, int tileY) {

			System.out.println("ScaleMultiresOpImage computeTile "+tileX+" "+tileY);
			return super.computeTile(tileX, tileY);
	 }
	// forwardMapRect
	 protected Rectangle forwardMapRect(Rectangle sourceRect,
             int sourceIndex) {

		 System.out.println("ScaleMultiresOpImage forwardMapRect");
		 return super.forwardMapRect(sourceRect, sourceIndex);
	 }
	// mapDestPoint
	public Point2D mapDestPoint(Point2D destPt, int sourceIndex) {
		System.out.println("ScaleMultiresOpImage mapDestPoint");
		return super.mapDestPoint(destPt, sourceIndex);
	}
	// mapSourcePoint
	public Point2D mapSourcePoint(Point2D sourcePt, int sourceIndex) {
		System.out.println("ScaleMultiresOpImage mapSourcePoint");
	     return super.mapSourcePoint(sourcePt, sourceIndex);
	    }
	// computeRect

}
