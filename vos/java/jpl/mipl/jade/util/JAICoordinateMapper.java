package jpl.mipl.jade.util;

import javax.media.jai.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.util.*;

/** 
  * Carrier class for a set of methods that convert between coordinates in
  * a JAI image chain.  The class is not instantiable and contains only static
  * methods.  Given a point expressed in one image's coordinate space, the
  * methods find the corresponding point expressed in some other (the desired)
  * image's coordinate space.
  * <p>
  * The mapping works as follows (in either direction).  Starting with a given
  * image and a coordinate expressed in that image's space, the coordinate is
  * mapped through <code>RenderedOp.mapDestPoint()</code> or
  * <code>RenderedOp.mapSourcePoint()</code>, thus transforming it to the
  * source (or destination) image's space.  The source or destination image
  * is then obtained, the point is mapped through it, and so on.  This
  * continues until the desired image is found, or the mapping fails.
  * <p>
  * The mapping fails if one of the following conditions holds before the
  * desired image is found:
  * <ul>
  * <li>It runs out of sources (or destinations), i.e. the end of the chain
  * is reached.
  * <li>Any image found in the chain for which a mapping method is needed is
  * not a subclass of <code>RenderedOp</code> (since the mapping methods are
  * only defined on them).  Note that the desired source need not be a
  * <code>RenderedOp</code> since its mapping function is not needed.
  * <li>The mapping method on any of the <code>RenderedOp</code> nodes fails,
  * i.e. returns null.
  * </ul>
  * Note that simple pointer equality (==) is used to find the desired image.
  * Thus you cannot pass in a rendering; the image object used to build the
  * chain must be the one passed in as the desired image.
  *
  * <PRE> 
  *
  * ============================================================================
  * <B>Modification History :</B>
  * ----------------------
  *
  * <B>Date              Who              What</B>
  * ----------------------------------------------------------------------------
  * 11/27/2003        Bob              Initial Release
  * 01/15/2004        Nick             Changed forwardMap to search all 
  *                                    descendant nodes for desired node.
  *                                    Changed backwardMap to search all 
  *                                    ancestral nodes for desired node.
  * 05/22/2008        Nick             backwardMap() checks if source is 
  *                                    desired prior to recursive call, 
  *                                    since call pre-condition may not 
  *                                    apply.  
  *                                    forwardMap() no longer casts current
  *                                    to a RenderedOp, just a PlanarImage.
  *                                    Phoenix 'lands' in 3 days. ;)
  * ============================================================================
  *
  * </PRE>
  * *
  * @class JAICoordinateMapper
  * @author Robert Deen      (Robert.Deen@jpl.nasa.gov)
  * @author Nicholas Toole   (Nicholas.T.Toole@jpl.nasa.gov)
  * @version 2008.05.22
  *
  */

public class JAICoordinateMapper
{

    /***********************************************************************
     * Private constructor to prevent instantiation
     */
    private JAICoordinateMapper() { }

    /***********************************************************************
     * Given a coordinate expressed in the current image, map that coordinate
     * through the list of <em>destination</em> (sink) images until the desired
     * image is found, and return the coordinate expressed in that image.
     * <p>
     * When traversing the image chain, all sinks of each node are
     * traversed.
     * <p>
     * The parameters are <code>Object</code>s rather than
     * <code>RenderedImage<code>s for consistency with backwardMap().  Anything
     * other than <code>RenderedOp</code> will quickly fail; however, the initial
     * image need only be a <code>PlanarImage</code>, since its map function is
     * not used (only the sink is obtained).
     * @returns a <code>Point2D</code> representing the coordinate expressed in
     * the desired image, or null if the mapping failed for some reason (see the
     * class comments).
     */

    public static Point2D forwardMap(Object current, Point2D coord,
                                     Object desired)
    {
        if (current == desired)
        {
            return coord;
        }

        if (!(current instanceof PlanarImage))
        {
            return null;
        }
        
        PlanarImage currentOp;

        try {
            currentOp = (PlanarImage) current;
        } catch (Exception ex) {
            return null;
        }
        
        Vector sinks = currentOp.getSinks();
        int numSinks = sinks.size();
        Point2D    dstCoord;
        Point2D    mappedCoord = null;
        boolean    cont = true;
           
        for (int i = 0; i < numSinks && cont; ++i)
        {    
            try {
                RenderedOp next = (RenderedOp) sinks.get(i);

                // Find the index of the currnt for mapSourcePoint()
                int numSources = next.getNumSources();
                int index;
                for (index = 0; index < numSources; index++) 
                {
                    if (next.getSourceObject(index) == current) 
                    {
                        break;
                    }
                }
                
                if (index >= numSources) //ignore i'th sink
                    continue;    
               
                dstCoord = next.mapSourcePoint(coord, index);
                
                mappedCoord = JAICoordinateMapper.forwardMap(
                              next, dstCoord, desired);

                if (mappedCoord != null)
                    cont = false;
            
            } catch (Exception e) {
                continue; //ignore i'th sink
            }

        } //end_for

        return mappedCoord;
    }
    
    /************************************************************************
     * Given a coordinate expressed in the current image, map that coordinate
     * through the graph of <em>source</em> images until the desired image is
     * found, and return the coordinate expressed in that image.
     * <p>
     * This method searches the entire ancestral graph.
     * <p>
     * The parameters are <code>Object</code>s rather than
     * <code>RenderedImage<code>s to allow the desired source to be anything
     * acceptable to JAI.  However, the mapping will fail if the intermediate 
     * nodes are not actually <code>RenderedOp</code>s.
     * @returns a <code>Point2D</code> representing the coordinate expressed 
     * in the desired image, or null if the mapping failed for some reason 
     * (see the class comments).
     */

    public static Point2D backwardMap(Object current, Point2D coord,
                                      Object desired)
    {
        if (current == desired)
        {
            return coord;
        }

        if (!(current instanceof RenderedOp))
        {
            return null;
        }

        RenderedOp currentOp = (RenderedOp) current;
        int numSources = currentOp.getNumSources();
        Object source;
        Point2D srcCoord;
        Point2D mappedCoord = null;
        boolean cont = true;

        for (int i = 0; i < numSources && cont; ++i)
        {
            try
            {
                srcCoord = currentOp.mapDestPoint(coord, i);
                source = currentOp.getSourceObject(i);
            } catch (Exception ex)
            {
                // ex.printStackTrace();
                continue; //skip this source
            }

            //source may not always be a rendered op, but the
            //source of a rendered op.  As such, we are adding
            //this check prior to the recursive call since the
            //call requires a specific type
            if (source == desired)
            {
                return srcCoord;
            }            
            else if (source instanceof RenderedOp)
            {
                mappedCoord = JAICoordinateMapper.backwardMap(source, srcCoord,
                        desired);

                if (mappedCoord != null)
                {
                    cont = false;
                }
            }
        }

        return mappedCoord;
    }
}

       

