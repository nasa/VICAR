/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;

import jpl.mipl.jade.jadis.agile2d.geom.VertexArray;
import jpl.mipl.jade.jadis.agile2d.ImageUtils;


/**
 * <code>VertexAttributes</code> supplement <code>VertexArray</code> to associate per-vertex
 * colors for all the vertices in a vertex array.
 *
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.2 $
 */
public class VertexAttributes {
    private java.nio.IntBuffer colorBuffer;
    private java.nio.ByteBuffer bcolorBuffer;
    private int     top;
    private boolean smooth;

    public VertexAttributes(int reserve) {
		bcolorBuffer = VertexArray.newByteBuffer(reserve*4);
    	colorBuffer = bcolorBuffer.asIntBuffer();
    }

    public VertexAttributes() {
        this(256);
    }

    public int getSize() {
        return top;
    }

	/**
	 * Returns a copy of the per-vertex data as an IntBuffer.
	 */
    public java.nio.IntBuffer getData() {
        java.nio.IntBuffer ret = VertexArray.newByteBuffer(top*4).asIntBuffer();
        colorBuffer.rewind();
        ret.put(colorBuffer);
        colorBuffer.position(top);
        return ret;
    }

	/**
	 * Returns a reference to the internal color array.
	 */
	public java.nio.ByteBuffer getColorRef() {
        return bcolorBuffer;
    }

	/**
	 * Empties out the vertex attributes array.
	 */
    public void clear() {
        top = 0;
        colorBuffer.rewind();
    }

	/**
	 * Ensures that the array has at least the specified size.
	 */
	public void reserve(int size) {
        if (colorBuffer.capacity() < size) {
            int newSize = colorBuffer.capacity() * 2;
            if (newSize < size)
                newSize = size;
            java.nio.ByteBuffer newbcolorBuffer = VertexArray.newByteBuffer(newSize*4);
            java.nio.IntBuffer newBuffer = newbcolorBuffer.asIntBuffer();
            colorBuffer.position(0);
            colorBuffer.limit(top);
            newBuffer.put(colorBuffer);

            colorBuffer = newBuffer;
            bcolorBuffer = newbcolorBuffer;
            colorBuffer.position(top);
        }
    }

	/**
	 * Appends a Color attribute to the vertex attributes array.
	 */
	public void addColor(Color color) {
        addColor(color.getRGB());
    }

	/**
	 * Appends a Color attribute to the vertex attributes array.
	 */
	public void addColor(int color) {
        reserve(top+1);
        colorBuffer.put(ImageUtils.ARGBtoRGBA(color));
        top++;
    }

	/**
	 * Returns the color attribute at the specified index.
	 */
	public Color getColorAt(int index) {
        return new Color(ImageUtils.RGBAtoARGB(this.colorBuffer.get(index)), true);
    }

	/**
	 * Returns the OpenGL RGBA color attribute at the specified index.
	 */
	public int getColorValueAt(int index) {
        return this.colorBuffer.get(index);
    }

    public PaintContext createContext(
        ColorModel cm,
        Rectangle deviceBounds,
        Rectangle2D userBounds,
        AffineTransform xform,
        RenderingHints hints) {
        if (top == 0)
            return null;
        return getColorAt(0).createContext(cm, deviceBounds, userBounds, xform, hints);
    }

    public int getTransparency() {
        if (top == 0)
            return 0;
        return (this.colorBuffer.get(0)>>24)&0xFF;
    }

    public boolean isSmooth() {
        return smooth;
    }

    public void setSmooth(boolean b) {
        smooth = b;
    }
}
