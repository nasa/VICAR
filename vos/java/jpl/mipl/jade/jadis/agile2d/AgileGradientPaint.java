/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/

package jpl.mipl.jade.jadis.agile2d;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.geom.Point2D;

/**
 * A GradientPaint that contains RGB int methods for setting colors, to 
 * avoid allocating Color objects if possible.
 *
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.2 $
 */
public class AgileGradientPaint extends GradientPaint {
    private int c1;
    private int c2;

    /**
     * Constructor for a simple acyclid <code>AgileGradientPaint</code> object.
     * @param x1 y1 coordinates of the first specified
     * <code>Point</code> in user space
     * @param color1 <code>Color</code> at the first specified 
     * <code>Point</code>
     * @param x2 y2 coordinates of the second specified
     * <code>Point</code> in user space
     * @param color2 <code>Color</code> at the second specified 
     * <code>Point</code>
     * @throws NullPointerException if either one of colors is null
     */
    public AgileGradientPaint(float x1, float y1, Color color1,
							  float x2, float y2, Color color2) {
        super(x1, y1, color1, x2, y2, color2);
    }

    /**
     * Constructs a simple acyclic <code>AgileGradientPaint</code> object.
     * @param pt1 the first specified <code>Point</code> in user space
     * @param color1 <code>Color</code> at the first specified 
     * <code>Point</code>
     * @param pt2 the second specified <code>Point</code> in user space
     * @param color2 <code>Color</code> at the second specified 
     * <code>Point</code>
     * @throws NullPointerException if either one of colors or points 
     * is null
     */
    public AgileGradientPaint(Point2D pt1, Color color1,
							  Point2D pt2, Color color2) {
        super(pt1, color1, pt2, color2);
    }

    /**
     * Constructs either a cyclic or acyclic <code>AgileGradientPaint</code>
     * object depending on the <code>boolean</code> parameter.
     * @param x1 y1 coordinates of the first specified
     * <code>Point</code> in user space
     * @param color1 <code>Color</code> at the first specified 
     * <code>Point</code>
     * @param x2 y2 coordinates of the second specified
     * <code>Point</code> in user space
     * @param color2 <code>Color</code> at the second specified 
     * <code>Point</code>
     * @param cyclic <code>true</code> if the gradient pattern should cycle
     * repeatedly between the two colors; <code>false</code> otherwise
     */
    public AgileGradientPaint(float x1, float y1, Color color1,
							  float x2, float y2, Color color2,
					          boolean cyclic) {
        super(x1, y1, color1, x2, y2, color2, cyclic);
    }

    /**
     * Constructs either a cyclic or acyclic <code>AgileGradientPaint</code>
     * object depending on the <code>boolean</code> parameter.
     * @param pt1 the first specified <code>Point</code> 
     * in user space
     * @param color1 <code>Color</code> at the first specified 
     * <code>Point</code>
     * @param pt2 the second specified <code>Point</code> 
     * in user space
     * @param color2 <code>Color</code> at the second specified 
     * <code>Point</code>
     * @param cyclic <code>true</code> if the gradient pattern should cycle
     * repeatedly between the two colors; <code>false</code> otherwise
     * @throws NullPointerException if either one of colors or points 
     * is null
     */
    public AgileGradientPaint(Point2D pt1, Color color1,
							  Point2D pt2, Color color2, boolean cyclic) {
        super(pt1, color1, pt2, color2, cyclic);
    }
    
    /**
     * Constructs either a cyclic or acyclic <code>AgileGradientPaint</code>
     * object depending on the <code>boolean</code> parameter.
     * @param x1 y1 coordinates of the first specified
     * <code>Point</code> in user space
     * @param c1 <code>int</code> value containing the argb color at
     * the first specified <code>Point</code>
     * @param x2 y2 coordinates of the second specified
     * <code>Point</code> in user space
     * @param c2 <code>int</code> value containing the argb color at
     * the second specified <code>Point</code>
     * @param cyclic <code>true</code> if the gradient pattern should cycle
     * repeatedly between the two colors; <code>false</code> otherwise
     */
    public AgileGradientPaint(float x1, float y1, int c1,
							  float x2, float y2, int c2, boolean cyclic) {
        super(x1, y1, null, x2, y2, null, cyclic);
        this.c1 = c1;
        this.c2 = c2;
    }
    
    /**
     * @see java.awt.GradientPaint#getColor1()
     */
    public Color getColor1() {
        Color c = super.getColor1();
        if (c == null) {
            return new Color(c1);
        }
        return c;
    }
    
    public Color getColor2() {
        Color c = super.getColor2();
        if (c == null) {
            return new Color(c2);
        }
        return c;
    }
    
    /**
     * Returns the <code>int</code> argb value of color anchored by
     * the point P1.
     * @return a <code>int</code> value that is the argb value of
     * color anchored by P1.
     */
    public int getC1() {
        return c1;
    }
    
    /**
     * Returns the <code>int</code> argb value of color anchored by
     * the point P2.
     * @return a <code>int</code> value that is the argb value of
     * color anchored by P2.
     */
    public int getC2() {
        return c2;
    }
}
