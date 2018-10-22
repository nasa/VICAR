/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;

import java.awt.*;
import java.awt.geom.*;

import jpl.mipl.jade.jadis.agile2d.geom.VertexAttributes;

/**
 * PolygonShape is a base class for TriangleShape and QuadShape, which are rendered efficiently on OpenGL.
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.2 $
 */
public abstract class PolygonShape implements Shape {
    protected float[] vertices;
    protected int origin;
    protected boolean closed;
    transient GeneralPath path;
    
    protected PolygonShape(float[] vertices, int origin, boolean closed) {
        this.vertices = vertices;
        this.origin = origin;
        this.closed = closed;
    }
    
    protected PolygonShape(int size, boolean closed) {
        this.vertices = new float[size*2];
        this.origin = 0;
        this.closed = closed;
    }
    
	// JM - removed operations that relied on modifying the path.
	
	public float getXAt(int index) {
        return vertices[origin+2*index];
    }

    public float getYAt(int index) {
        return vertices[origin+2*index+1];
    }
    
    public void setXAt(int index, float value) {
        vertices[origin+2*index] = value;
        path = null;
    }
    
    public void setYAt(int index, float value) {
        vertices[origin+2*index+1] = value;
        path = null;
    }
    
    public int getSize() {
        return vertices.length;
    }
    
    GeneralPath toGeneralPath() {
        if (path == null) {
            path = new GeneralPath();
            if (getSize() != 0) {
                path.moveTo(getXAt(0), getYAt(0));
                for (int i = 1; i < getSize(); i++) {
                    path.lineTo(getXAt(i), getYAt(i));
                }
                if (isClosed())
                    path.closePath();
            }
        }
        return path;
    }

    public boolean contains(double x, double y) {
        return toGeneralPath().contains(x, y);
    }

    public boolean contains(double x, double y, double w, double h) {
        return toGeneralPath().contains(x, y, w, h);
    }

    public boolean contains(Point2D p) {
        return toGeneralPath().contains(p);
    }

    public boolean contains(Rectangle2D r) {
        return toGeneralPath().contains(r);
    }

    public synchronized Shape createTransformedShape(AffineTransform at) {
        return toGeneralPath().createTransformedShape(at);
    }

    public Rectangle getBounds() {
        return toGeneralPath().getBounds();
    }

    public Rectangle2D getBounds2D() {
        return toGeneralPath().getBounds2D();
    }

    public PathIterator getPathIterator(AffineTransform at) {
        return toGeneralPath().getPathIterator(at);
    }

    public PathIterator getPathIterator(
        AffineTransform at,
        double flatness) {
        return toGeneralPath().getPathIterator(at, flatness);
    }

    public boolean intersects(double x, double y, double w, double h) {
        return toGeneralPath().intersects(x, y, w, h);
    }

    public boolean intersects(Rectangle2D r) {
        return toGeneralPath().intersects(r);
    }

    public boolean isClosed() {
        return closed;
    }

	/**
	 * Draws an outline of this polygon using the VertexAttributes for the color information for
	 * each vertex. Colors are smoothly interpolated between vertices.
	 * 
	 * @param vertexAttributes vertex attay containing color attributes for each vertex.
	 * @param index specifies where in the VertexAttributes array the color information starts.
	 */
	void drawSmooth(Graphics2D g, VertexAttributes vertexAttributes, int index) {
		if (vertexAttributes != null && vertexAttributes.isSmooth()) {
			int c = vertexAttributes.getColorValueAt(index);
			for (int i = 1; i < this.getSize(); i++) {
				if (c != vertexAttributes.getColorValueAt(index + i)) {
					// we have to use gradients, lack of better solution
					GradientPaint gp = new GradientPaint(
						this.getXAt(i - 1),
						this.getYAt(i - 1),
						vertexAttributes.getColorAt(index + i - 1),
						this.getXAt(i),
						this.getYAt(i),
						vertexAttributes.getColorAt(index + i),
						true);
					g.setPaint(gp);
					Stroke stroke = g.getStroke();
					g.fill(stroke.createStrokedShape(this));
					return;
				}
			}
			// falls through
		}
		if (vertexAttributes != null) {
			g.setColor(vertexAttributes.getColorAt(index));
		}
		g.draw(this);
	}

	/**
	 * Fills this polygon using the VertexAttributes for the color information for
	 * each vertex. Colors are smoothly interpolated between vertices.
	 * 
	 * @param vertexAttributes vertex attay containing color attributes for each vertex.
	 * @param index specifies where in the VertexAttributes array the color information starts.
	 */
	void fillSmooth(Graphics2D g, VertexAttributes vertexAttributes, int index) {
		if (vertexAttributes != null && vertexAttributes.isSmooth()) {
			int c = vertexAttributes.getColorValueAt(index);
			for (int i = 1; i < this.getSize(); i++) {
				if (c != vertexAttributes.getColorValueAt(index + i)) {
					// we have to use gradients, lack of better solution
					GradientPaint gp = new GradientPaint(
						this.getXAt(i - 1),
						this.getYAt(i - 1),
						vertexAttributes.getColorAt(index + i - 1),
						this.getXAt(i),
						this.getYAt(i),
						vertexAttributes.getColorAt(index + i),
						true);
					g.setPaint(gp);
					g.fill(this);
					return;
				}
			}
			// falls through
		}
		if (vertexAttributes != null) {
			g.setColor(vertexAttributes.getColorAt(index));
		}
		g.fill(this);
	}	
}
