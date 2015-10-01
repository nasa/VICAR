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

import jpl.mipl.jade.jadis.agile2d.geom.*;

/**
 * Memorize the result of a tesselation.
 */
public class VertexArrayList implements Shape {
	private VertexArray[] list;
	private int           top;
	private transient Area area;
    
	/**
	 * Create a new VertexArrayList object.
	 */
	public VertexArrayList() {
		list = new VertexArray[10];
		top = 0;
	}

	/**
	 * Clears the VertexArrayList
	 */
	public void clear() {
		top = 0;
		area = null;
	}

	/**
	 * Reserve more room for VertexArrays
	 *
	 * @param size the new allocated room.
	 */
	public void resize(int size) {
		if (list.length < size) {
			int newSize = list.length * 2;
			if (newSize < size)
				newSize = size;
			VertexArray[] newList = new VertexArray[newSize];
			System.arraycopy(list, 0, newList, 0, list.length);
			list = newList;
		}
	}

	/**
	 * Adds a <code>VertexArray</code>
	 *
	 * @param v the <code>VertexArray</code>
	 */
	public void add(VertexArray v) {
		area = null;        
		resize(top + 1);
		list[top++] = v;
	}

	/**
	 * Returns the size
	 *
	 * @return the size
	 */
	public int size() {
		return top;
	}

	public int capacity() { 
		return list.length;
	}

	/**
	 * Returns the <code>VertexArray</code> at the specified index
	 *
	 * @param index the index
	 *
	 * @return the <code>VertexArray</code> at the specified index
	 */
	public VertexArray getVertexArrayAt(int index) {
		return list[index];
	}

	public void fill(Graphics g) {
		for (int i = 0; i < size(); i++) {
			getVertexArrayAt(i).fill(g);
		}
	}

    // Shape interface
    protected Area getArea() {
        if (area == null) {
            area = new Area(this);
        }
        return area;
    }
    
    public boolean contains(double x, double y, double w, double h) {
        return getArea().contains(x, y, w, h);
    }

    public boolean contains(double x, double y) {
        return getArea().contains(x, y);
    }

    public boolean contains(Point2D p) {
        return getArea().contains(p);
    }

    public boolean contains(Rectangle2D r) {
        return getArea().contains(r);
    }

    public Rectangle getBounds() {
        return getArea().getBounds();
    }

    public Rectangle2D getBounds2D() {
        return getArea().getBounds2D();
    }

    class VAListIterator implements PathIterator {
        int index;
        AffineTransform at;
        PathIterator current;
        
        VAListIterator(AffineTransform at) {
            this.at = at;
            index = -1;
            next();
        }
        
        public int currentSegment(double[] coords) {
            if (current != null)
                return current.currentSegment(coords);
            return SEG_CLOSE;
        }

        public int currentSegment(float[] coords) {
            if (current != null)
                return current.currentSegment(coords);
            return SEG_CLOSE;
        }

        public int getWindingRule() {
            return WIND_EVEN_ODD;
        }

        public boolean isDone() {
            return current == null;
        }

        public void next() {
            index++;
            if (index >= size()) {
                index = -1;
                current = null;
            }
            else {
                current = getVertexArrayAt(index).getPathIterator(at);
            }
        }

    }

    public PathIterator getPathIterator(AffineTransform at, double flatness) {
        return getPathIterator(at);
    }

    public PathIterator getPathIterator(AffineTransform at) {
        return new VAListIterator(at);
    }

    public boolean intersects(double x, double y, double w, double h) {
        return getArea().intersects(x, y, w, h);
    }

    public boolean intersects(Rectangle2D r) {
        return getArea().intersects(r);
    }

}

