/*
 * $RCSfile: ScaleMultiresCRIF.java,v $
 
 * This code is derived from ScaleMultiresCRIF.java
 * package com.sun.media.jai.opimage;
 * Copyright (c) 2005 Sun Microsystems, Inc. All rights reserved.
 *
 *
 */
//
package jpl.mipl.io.plugins;

import com.sun.media.jai.opimage.*;

import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.DataBuffer;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.renderable.RenderedImageFactory;
import java.awt.image.renderable.RenderContext;
import java.awt.image.renderable.ParameterBlock;
import java.awt.image.renderable.RenderableImage;
import javax.media.jai.BorderExtender;
import javax.media.jai.ImageLayout;
import javax.media.jai.Interpolation;
import javax.media.jai.InterpolationNearest;
import javax.media.jai.InterpolationBilinear;
import javax.media.jai.InterpolationBicubic;
import javax.media.jai.InterpolationBicubic2;
import javax.media.jai.InterpolationTable;
import javax.media.jai.TileCache;
import javax.media.jai.CRIFImpl;
import java.util.Map;
import javax.media.jai.*;


/**
 * @see ScaleOpImage
 */
public class ScaleMultiresCRIF extends ScaleCRIF {

    // private static final float TOLERANCE = 0.01F;
    
    ScaleMultiresOpImage multiresOpImage = null;
    RenderedImage scaledRenderedImage = null;
    
    // add other stuff we need
    // level, scale (2 scales) 1 is absolute scale /zoom
    // another is the scale applied to the level image
    float absoluteScale = 1.0F; // scale from the full resolution image
    float relativeScale = 1.0F; // scale from the image we are currently using
    int level = 0; // index for image we are using
    // might also store info on each image in the List
    // to decide which one to use quickly

    /** Constructor. */
    public ScaleMultiresCRIF() {
        super();
        //   super("scale");
    }

    /**
     * Creates a new instance of ScaleOpImage in the rendered layer.
     * This method satisfies the implementation of RIF.
     *
     * @param paramBlock  The source image, the X and Y scale factor,
     *                    and the interpolation method for resampling.
     */
    public RenderedImage create(ParameterBlock paramBlock,
                                RenderingHints renderHints) {

        // Get ImageLayout from renderHints if any.
        ImageLayout layout = RIFUtil.getImageLayoutHint(renderHints);
        
        // Get TileCache from renderHints if any.
        TileCache cache = RIFUtil.getTileCacheHint(renderHints);

        // Get BorderExtender from renderHints if any.
        BorderExtender extender = RIFUtil.getBorderExtenderHint(renderHints);

        RenderedImage source = paramBlock.getRenderedSource(0);
        float xScale = paramBlock.getFloatParameter(0);
        float yScale = paramBlock.getFloatParameter(1);
        float xTrans = paramBlock.getFloatParameter(2);
        float yTrans = paramBlock.getFloatParameter(3);
        Interpolation interp = (Interpolation)paramBlock.getObjectParameter(4);

	// Check and see if we are scaling by 1.0 in both x and y and no
        // translations. If so call the copy operation.
    System.out.println("ScaleMultiresCRIF.create");   
    if (source instanceof CollectionImage) {
    	// ScaleMultiresOpImage(source, renderHints, layout);
    	// ScaleMultiresOpImage(ParameterBlock paramBlock, RenderingHints renderHints);
    	// do we have our own OpImage ?? or are we really going to still create on of the 
    	// ones below and extend it with some extra junk??
    	// ourOpImage gets the ScaleOpImage from below to use adds the extra junk to handle when params change 
    	// selecting proper image to display and then setting the new scale param
    	System.out.println("source instanceof CollectionImage");
    	
    }
    
    Map configuration = null; // what is this? should I make one, get it from somewhere?
    boolean cobbleSources = true;
    
    /**
     * public ScaleMultiresOpImage(RenderedImage source, Map configuration,
            boolean cobbleSources, ImageLayout layout,
			BorderExtender extender, Interpolation interp, float scaleX, float scaleY, 
			float transX, float transY) {
			
			**/
    // tileCache - pass this too, constructor with the parameter block?
    multiresOpImage = new ScaleMultiresOpImage(source, configuration,
            cobbleSources, layout, extender, interp, xScale, yScale, xTrans, yTrans);
    
    // scaleOpRenderedImage = super.create(paramBlock, renderHints) ;
    
    // add our other things to this 
    // keep track of current scale, level 
    // is this a renderedIageList or not
    
    scaledRenderedImage = multiresOpImage.createScaledRendering(xScale, yScale,
            renderHints);
    
    // 
    // all the rest we let the super do
    // we trap any updates do some magic and let the "Real" ScaleOp do the work
    
    // if we returned mutiresOpImage then all calls would go thru it??
    
    return scaledRenderedImage ;
    
    }

}
