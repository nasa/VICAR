/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;


import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import jpl.mipl.jade.jadis.agile2d.geom.*;

/**
 * A VertexArray is a special Shape, made of several simple primitives such
 * as points, lines, triangles, quadrilaterals or convex polygons.
 *
 * <p>
 * VertexArray manages a list of primitives and are rendered very efficiently
 * by hardware accelerated graphics cards when they are available. A
 * VertexArray can hold 9 types of primitives:
 *
 * <ol>
 * <li>
 * Points: a sequence of disconnected points.
 * </li>
 * <li>
 * Lines: a sequence of disconnected lines.
 * </li>
 * <li>
 * Line strip: a sequence of connected lines.
 * </li>
 * <li>
 * Line loop: a sequence of connected lines where the last vertex is
 * connected to the first.
 * </li>
 * <li>
 * Triangles: a sequence of disconnected triangles.
 * </li>
 * <li>
 * Triangle strip: a connected sequence of triangles.
 * </li>
 * <li>
 * Triangle fan: a sequence of triangles sharing the same initial point.
 * </li>
 * <li>
 * Quads: a sequence of disconnected quadrilaterals.
 * </li>
 * <li>
 * Quad strip: a sequence of connected quadrilaterals.
 * </li>
 * <li>
 * Polygon: one convex polygon.
 * </li>
 * </ol>
 * </p>
 *
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
public class VertexArray implements Shape {
	// Constants are the same as in GL.
	/** Constant for an invalid mode */
	public static final short MODE_INVALID = -1;
	/** Constant for the points mode */
	public static final short MODE_POINTS = 0;
	/** Constant for the lines mode */
	public static final short MODE_LINES = 1;
	/** Constant for the line strip mode */
	public static final short MODE_LINE_STRIP = 3;
	/** Constant for the line loop mode */
	public static final short MODE_LINE_LOOP = 2;
	/** Constant for the triangles mode */
	public static final short MODE_TRIANGLES = 4;
	/** Constant for the triangle strip mode */
	public static final short MODE_TRIANGLE_STRIP = 5;
	/** Constant for the triangle fan mode */
	public static final short MODE_TRIANGLE_FAN = 6;
	/** Constant for the quads mode */
	public static final short MODE_QUADS = 7;
	/** Constant for the quad strip mode */
	public static final short MODE_QUAD_STRIP = 8;
	/** Constant for the polygon mode */
	public static final short MODE_POLYGON = 9;

	private int mode;
	private java.nio.FloatBuffer dataBuffer;
	private int top;
	private transient GeneralPath path;

	/**
	 * Constructor for VertexArray.
	 *
	 * @param reserve available number of coords before reallocation is
	 *        needed.
	 */
	public VertexArray(int reserve) {
		dataBuffer = newByteBuffer(reserve*4).asFloatBuffer();
		mode = MODE_INVALID;
	}

	public static ByteBuffer newByteBuffer(int numElements) {
		ByteBuffer bb = ByteBuffer.allocateDirect(numElements);
		bb.order(ByteOrder.nativeOrder());
		return bb;
	}

	/**
	 * Constructor for VertexArray.
	 */
	public VertexArray() {
		this(256);
	}

	/**
	 * Return the number of vertices.
	 *
	 * @return the number of vertices.
	 */
	public int getVertexCount() {
		return top / 2;
	}

	/**
	 * Return the number of coordinates.
	 * i.e. number of vertices * 2.
	 *
	 * @return the number of coordinates.
	 */
	public int getCoordsCount() {
		return top;
	}

	/**
	 * Return a copy of the array of coordinates.
	 *
	 * @return a copy of the array of coordinates.
	 */
	public java.nio.FloatBuffer getData() {
		java.nio.FloatBuffer ret = newByteBuffer(top*4).asFloatBuffer();
		int pos = dataBuffer.position();
		dataBuffer.rewind();
		ret.put(dataBuffer);
		dataBuffer.position(pos);
		ret.position(pos);
		System.out.println("Copy copy\n");
		return ret;
	}

	/**
	 * Return the reference to the internal array of coordinates.
	 *
	 * @return the array of coordinates.
	 */
	public java.nio.FloatBuffer getDataRef() {
		return dataBuffer;
	}

	/**
	 * Clear the vertex array.
	 */
	public void clear() {
		top = 0;
		dataBuffer.rewind();
		mode = MODE_INVALID;
	}

	/**
	 * Returns the mode.
	 *
	 * @return int
	 */
	public int getMode() {
		return mode;
	}

	/**
	 * Set the mode and clears the array.
	 *
	 * @param primitive The mode to set
	 */
	public void setMode(int primitive) {
		this.mode = primitive;
	}

	/**
	 * Make sure enough room is allocated to store coordinates.
	 *
	 * @param size the number of vertex coordinates to reserve.
	 */
	public void reserve(int size) {
		if (dataBuffer.capacity() < size) {
			int newSize = dataBuffer.capacity() * 2;
			if (newSize < size)
				newSize = size;

			// create a new buffer with the requested size, but copy only elements up to the current top
			java.nio.FloatBuffer newBuffer = newByteBuffer(newSize*4).asFloatBuffer();
			dataBuffer.position(0);
			dataBuffer.limit(top);
			newBuffer.put(dataBuffer);

			// the old buffer is disposed and the new one starts with position at top and limit as the size
			dataBuffer = newBuffer;
			dataBuffer.position(top);
			System.out.println("Resizing buffer to " + newSize + " with remaining " + dataBuffer.remaining());
		}
	}

	/**
	 * Add a vertex
	 *
	 * @param coords an array of at least two double values.
	 */
	public void addVertex(double[] coords) {
		//!!!! commented out for compatibility with jdk1.4
		//assert(mode != MODE_INVALID);
		reserve(top + 2);
		dataBuffer.put((float) coords[0]);
		dataBuffer.put((float) coords[1]);
		path = null;
		top += 2;
	}

	/**
	 * Add a vertex
	 *
	 * @param x the X coordinate
	 * @param y the Y coordinate
	 */
	public void addVertex(double x, double y) {
		reserve(top + 2);
		dataBuffer.put((float) x);
		dataBuffer.put((float) y);
		path = null;
		top += 2;
	}

	/**
	 * Add a vertex
	 *
	 * @param coords an array of at least two float values.
	 */
	public void addVertex(float[] coords) {
		reserve(top + 2);
		dataBuffer.put((float) coords[0]);
		dataBuffer.put((float) coords[1]);
		path = null;
		top += 2;
	}

	/**
	 * Add a vertex
	 *
	 * @param x the X coordinate
	 * @param y the Y coordinate
	 */
	public void addVertex(float x, float y) {
		reserve(top + 2);
		dataBuffer.put((float) x);
		dataBuffer.put((float) y);
		path = null;
		top += 2;
	}

	/**
	 * Return the coordinates of a specified vertex.
	 *
	 * @param index the specified vertex
	 * @param coords an array where the coordinates will be stored.
	 *
	 * @throws ArrayIndexOutOfBoundsException DOCUMENT ME!
	 */
	public void getVertex(int index, float[] coords) {
		int i = index * 2;
		if (i < 0 || (i + 1) >= top)
			throw new ArrayIndexOutOfBoundsException(index);
		coords[0] = dataBuffer.get(i);
		coords[1] = dataBuffer.get(i+1);
	}

	/**
	 * Add a line.
	 *
	 * @param coords the two vertices (four coordinates) of the line.
	 */
	public void addLine(float[] coords) {
		addVertex(coords[0], coords[1]);
		addVertex(coords[2], coords[3]);
	}

	/**
	 * Add a line.
	 *
	 * @param x1 the X coordinate of the first vertex
	 * @param y1 the Y coordinate of the first vertex
	 * @param x2 the X coordinate of the second vertex
	 * @param y2 the Y coordinate of the second vertex
	 */
	public void addLine(float x1, float y1, float x2, float y2) {
		addVertex(x1, y1);
		addVertex(x2, y2);
	}

	/**
	 * Add a triangle.
	 *
	 * @param coords the three vertices (six coordinates) of the triangle.
	 */
	public void addTriangle(float[] coords) {
		addVertex(coords[0], coords[1]);
		addVertex(coords[2], coords[3]);
		addVertex(coords[4], coords[5]);
	}

	/**
	 * Add a triangle.
	 *
	 * @param x1 the X coordinate of the first vertex
	 * @param y1 the Y coordinate of the first vertex
	 * @param x2 the X coordinate of the second vertex
	 * @param y2 the Y coordinate of the second vertex
	 * @param x3 the X coordinate of the third vertex
	 * @param y3 the Y coordinate of the third vertex
	 */
	public void addTriangle(
		float x1,
		float y1,
		float x2,
		float y2,
		float x3,
		float y3) {
		addVertex(x1, y1);
		addVertex(x2, y2);
		addVertex(x3, y3);
	}

	/**
	 * Add a quadrilateral.
	 *
	 * @param coords the four vertices (eight coordinates) of the quad.
	 */
	public void addQuad(float[] coords) {
		addVertex(coords[0], coords[1]);
		addVertex(coords[2], coords[3]);
		addVertex(coords[4], coords[5]);
		addVertex(coords[5], coords[6]);
	}

	/**
	 * Add a quadrilateral.
	 *
	 * @param x1 the X coordinate of the first vertex
	 * @param y1 the Y coordinate of the first vertex
	 * @param x2 the X coordinate of the second vertex
	 * @param y2 the Y coordinate of the second vertex
	 * @param x3 the X coordinate of the third vertex
	 * @param y3 the Y coordinate of the third vertex
	 * @param x4 the X coordinate of the fourth vertex
	 * @param y4 the Y coordinate of the fourth vertex
	 */
	public void addQuad(
		float x1,
		float y1,
		float x2,
		float y2,
		float x3,
		float y3,
		float x4,
		float y4) {
		addVertex(x1, y1);
		addVertex(x2, y2);
		addVertex(x3, y3);
		addVertex(x4, y4);
	}

	/**
	 * Add a quadrilateral specified as a rectangle.
	 *
	 * @param x the X coordinate
	 * @param y the Y coordinate
	 * @param w the width
	 * @param h the height
	 */
	public void addRect(float x, float y, float w, float h) {
		addQuad(x, y, x, y + h, x + w, y + h, x + w, y);
	}

	// Implement the Shape interface for compatibility
	/**
	 * @see java.awt.Shape#contains(double, double, double, double)
	 */
	public boolean contains(double x, double y, double w, double h) {
		if (mode < MODE_TRIANGLES)
			return false;
		return toGeneralPath().contains(x, y, w, h);
	}

	/**
	 * @see java.awt.Shape#contains(double, double)
	 */
	public boolean contains(double x, double y) {
		if (mode < MODE_TRIANGLES)
			return false;
		return toGeneralPath().contains(x, y);
	}

	/**
	 * @see java.awt.Shape#contains(Point2D)
	 */
	public boolean contains(Point2D p) {
		return contains(p.getX(), p.getY());
	}

	/**
	 * @see java.awt.Shape#contains(Rectangle2D)
	 */
	public boolean contains(Rectangle2D r) {
		if (mode < MODE_TRIANGLES)
			return false;
		return toGeneralPath().contains(r);
	}

	/**
	 * @see java.awt.Shape#getBounds()
	 */
	public Rectangle getBounds() {
		return getBounds2D().getBounds();
	}

	/**
	 * @see java.awt.Shape#getBounds2D()
	 */
	public Rectangle2D getBounds2D() {
		if (top == 0)
			return null;

		float xmin = 0;
		float ymin = 0;
		float xmax = 0;
		float ymax = 0;

		// TODO improve by relative addressing
		xmin = dataBuffer.get(0);
		xmax = xmin;
		ymin = dataBuffer.get(1);
		ymax = ymin;
		for (int i = 2; i < top; i += 2) {
			float datai = dataBuffer.get(i);
			if (datai < xmin)
				xmin = datai;
			else if (datai > xmax)
				xmax = datai;
			datai = dataBuffer.get(i+1);
			if (datai < ymin)
				ymin = datai;
			else if (datai > ymax)
				ymax = datai;
		}
		return new Rectangle2D.Double(xmin, ymin, xmax - xmin, ymax - ymin);
	}

	GeneralPath toGeneralPath() {
		if (path == null) {
			path = new GeneralPath(this);
		}
		return path;
	}

	/**
	 * @see java.awt.Shape#getPathIterator(AffineTransform, double)
	 */
	public PathIterator getPathIterator(AffineTransform at, double flatness) {
		return getPathIterator(at);
	}

	/**
	 * @see java.awt.Shape#getPathIterator(AffineTransform)
	 */
	public PathIterator getPathIterator(AffineTransform at) {
		VertexArrayIterator iter;
		switch (mode) {
			case MODE_POINTS :
				iter = new PointsIterator();
				break;
			case MODE_LINES :
				iter = new LinesIterator();
				break;
			case MODE_LINE_STRIP :
				iter = new LineStripIterator();
				break;
			case MODE_LINE_LOOP :
				iter = new LineLoopIterator();
				break;
			case MODE_TRIANGLES :
				iter = new TrianglesIterator();
				break;
			case MODE_TRIANGLE_STRIP :
				iter = new TriangleStripIterator();
				break;
			case MODE_TRIANGLE_FAN :
				iter = new TriangleFanIterator();
				break;
			case MODE_QUADS :
				iter = new QuadsIterator();
				break;
			case MODE_QUAD_STRIP :
				iter = new QuadStripIterator();
				break;
			case MODE_POLYGON :
				iter = new PolygonIterator();
				break;
			default :
				return null;
		}

		if (at != null) {
			return new TransformPathIterator(iter, at);
		}
		return iter;
	}

	/**
	 * @see java.awt.Shape#intersects(double, double, double, double)
	 */
	public boolean intersects(double x, double y, double w, double h) {
		return toGeneralPath().intersects(x, y, w, h);
	}

	/**
	 * @see java.awt.Shape#intersects(Rectangle2D)
	 */
	public boolean intersects(Rectangle2D r) {
		return toGeneralPath().intersects(r);
	}

	abstract class VertexArrayIterator implements PathIterator {
		int index;
		float[] c;

		VertexArrayIterator() {
			index = 0;
		}

		/**
		 * @see java.awt.geom.PathIterator#currentSegment(float[)
		 */
		public int currentSegment(double[] coords) {
			if (c == null) {
				c = new float[2];
			}
			int type = currentSegment(c);
			coords[0] = c[0];
			coords[1] = c[1];
			return type;
		}

		/**
		 * @see java.awt.geom.PathIterator#getWindingRule()
		 */
		public int getWindingRule() {
			return WIND_NON_ZERO;
		}

		/**
		 * @see java.awt.geom.PathIterator#isDone()
		 */
		public boolean isDone() {
			return index >= getVertexCount();
		}

		/**
		 * @see java.awt.geom.PathIterator#next()
		 */
		public void next() {
			index += 1;
		}
	}

	class PointsIterator extends VertexArrayIterator {
		public int currentSegment(float[] coords) {
			getVertex(index, coords);
			return SEG_MOVETO;
		}
	}

	class LinesIterator extends VertexArrayIterator {
		public int currentSegment(float[] coords) {
			getVertex(index, coords);
			if ((index & 1) == 0) { // even vertex, SEG_MOVETO
				return SEG_MOVETO;
			}
			else { // odd vertex, SEG_LINETO
				return SEG_LINETO;
			}
		}
	}

	class LineStripIterator extends VertexArrayIterator {
		public int currentSegment(float[] coords) {
			getVertex(index, coords);
			if (index == 0)
				return SEG_MOVETO;
			else
				return SEG_LINETO;
		}
	}

	class LineLoopIterator extends LineStripIterator {
		public boolean isDone() {
			return index > getVertexCount();
		}

		public int currentSegment(float[] coords) {
			if (index == getVertexCount()) {
				return SEG_CLOSE;
			}
			return super.currentSegment(coords);
		}
	}

	class TrianglesIterator extends VertexArrayIterator {
		int count;
		int last;

		TrianglesIterator() {
			index = 0;
			count = 0;
			last = getVertexCount() * 4 / 3;
			// we need to generate one CLOSE every 3 vertex.
		}

		public boolean isDone() {
			return count >= last;
		}

		public void next() {
			if ((count % 4) != 3)
				index++;
			count++;
		}

		public int currentSegment(float[] coords) {
			int rem = count % 4;

			switch (rem) {
				case 0 :
					getVertex(index, coords);
					return SEG_MOVETO;
				case 1 :
				case 2 :
					getVertex(index, coords);
					return SEG_LINETO;
				default :
					return SEG_CLOSE;
			}
		}
	}

	class TriangleStripIterator extends TrianglesIterator {
		TriangleStripIterator() {
			index = 0;
			count = 0;
			last = (getVertexCount() - 2) * 4;
		}

		public void next() {
			if ((count % 4) != 3)
				index++;
			else {
				index -= 2;
			}
			count++;
		}
	}

	class TriangleFanIterator extends TriangleStripIterator {
		TriangleFanIterator() {
			index = 1;
		}

		public void next() {
			if ((count % 4) == 1)
				index++;
			count++;
		}

		public int currentSegment(float[] coords) {
			int rem = count % 4;

			switch (rem) {
				case 0 :
					getVertex(0, coords);
					return SEG_MOVETO;
				case 1 :
				case 2 :
					getVertex(index, coords);
					return SEG_LINETO;
				default :
					return SEG_CLOSE;
			}
		}
	}

	class QuadsIterator extends TrianglesIterator {
		QuadsIterator() {
			index = 0;
			count = 0;
			last = getVertexCount() * 5 / 4;
			// we need to generate one CLOSE every 3 vertex.
		}

		public boolean isDone() {
			return count >= last;
		}

		public void next() {
			if ((count % 5) != 4)
				index++;
			count++;
		}

		public int currentSegment(float[] coords) {
			int rem = count % 5;

			switch (rem) {
				case 0 :
					getVertex(index, coords);
					return SEG_MOVETO;
				case 1 :
				case 2 :
				case 3 :
					getVertex(index, coords);
					return SEG_LINETO;
				default :
					return SEG_CLOSE;
			}
		}
	}

	class QuadStripIterator extends QuadsIterator {
		QuadStripIterator() {
			index = 0;
			count = 0;
			last = (getVertexCount() - 3) * 5;
		}

		public void next() {
			if ((count % 5) != 4)
				index++;
			else {
				index -= 3;
			}
			count++;
		}
	}

	class PolygonIterator extends VertexArrayIterator {
		public boolean isDone() {
			return index > top;
		}

		public int currentSegment(float[] coords) {
			if (index == top)
				return SEG_CLOSE;
			coords[0] = dataBuffer.get(index);
			coords[1] = dataBuffer.get(index + 1);
			if (index == 0)
				return SEG_MOVETO;
			return SEG_LINETO;
		}
	}


	
	/**
	 * Draws the outline of the geometry in this VertexArray using the current Stoke and Paint
	 * attributes of the graphics.
	 */
	public void draw(Graphics g) {
		draw(g, null);
	}

	/**
	 * Draws the outline of the geometry in this VertexArray using the per-vertex color attributes
	 * specified in the VertexAttributes array.
	 * 
	 * For an AgileGraphics2D, this is equivalent to calling agraphics.drawVertexArray(this, attributes).
	 * For a standard Graphics2D object, a less performant series of draw commands are executed.
	 */
	public void draw(Graphics g, VertexAttributes attributes) {
		if (g instanceof VertexArraySupport) {
			((VertexArraySupport)g).drawVertexArray(this, attributes);
		}
		else if (g instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) g;

			switch (mode) {
				case MODE_TRIANGLES :
					{
					TriangleShape shape = new TriangleShape();
					for (int i = 0; i < (top - 5); i += 6) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0( dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2( dataBuffer.get(i + 5));
						shape.drawSmooth(g2d, attributes, i / 2);
					}
				}
					break;
				case MODE_TRIANGLE_STRIP:
					{
					TriangleShape shape = new TriangleShape();
					for (int i = 0; i < (top - 3); i += 4) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0( dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2( dataBuffer.get(i + 5));
						shape.drawSmooth(g2d, attributes, i / 2);
					}
				}
					break;
				case MODE_TRIANGLE_FAN :
					{
					TriangleShape shape = new TriangleShape();
					shape.setX0(dataBuffer.get(0));
					shape.setY0( dataBuffer.get(1));
					for (int i = 2; i < (top - 3); i += 2) {
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2( dataBuffer.get(i + 5));
						shape.drawSmooth(g2d, attributes, i / 2);
					}
				}
					break;
				case MODE_QUADS :
					{
					QuadShape shape = new QuadShape();
					for (int i = 0; i < (top - 7); i += 8) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0(dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1(dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2(dataBuffer.get(i + 5));
						shape.setX3(dataBuffer.get(i + 6));
						shape.setY3(dataBuffer.get(i + 7));
						shape.drawSmooth(g2d, attributes, i / 2);
					}
				}
					break;
				case MODE_QUAD_STRIP :
					{
					QuadShape shape = new QuadShape();
					for (int i = 0; i < (top - 5); i += 6) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0(dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1(dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2(dataBuffer.get(i + 5));
						shape.setX3(dataBuffer.get(i + 6));
						shape.setY3(dataBuffer.get(i + 7));
						shape.drawSmooth(g2d, attributes, i / 2);
					}
				}
					break;
			}
		}
	}

	/**
	 * Fills the geometry in this VertexArray using the current Graphic's Paint
	 * property as the fill color.
	 */
	public void fill(Graphics g) {
		fill(g, null);
	}

	/**
	 * Fills the geometry in this VertexArray using the per-vertex color attributes
	 * specified in the VertexAttributes array.
     *
	 * For an AgileGraphics2D, this is equivalent to calling agraphics.fillVertexArray(this, attributes).
	 * For a standard Graphics2D object, a less performant series of fill commands are executed.
	 */
	public void fill(Graphics g, VertexAttributes attributes) {
		if (g instanceof VertexArraySupport) {
			((VertexArraySupport)g).fillVertexArray(this, attributes);
		}
		else if (g instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) g;
			switch (mode) {
				case MODE_TRIANGLES :
				{
					TriangleShape shape = new TriangleShape();
					for (int i = 0; i < (top - 5); i += 6) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0(dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1(dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2(dataBuffer.get(i + 5));
						shape.fillSmooth(g2d, attributes, i / 2);
					}
					break;
				}
				case MODE_TRIANGLE_STRIP :
				{
					TriangleShape shape = new TriangleShape();
					for (int i = 0; i < (top - 3); i += 4) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0( dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1(dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2(dataBuffer.get(i + 5));
						shape.fillSmooth(g2d, attributes, i / 2);
					}
					break;
				}
				case MODE_TRIANGLE_FAN :
				{
					TriangleShape shape = new TriangleShape();
					shape.setX0(dataBuffer.get(0));
					shape.setY0( dataBuffer.get(1));
					for (int i = 2; i < (top - 3); i += 2) {
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2( dataBuffer.get(i + 5));
						shape.fillSmooth(g2d, attributes, i / 2);
					}
					break;
				}
				case MODE_QUADS :
				{
					QuadShape shape = new QuadShape();
					for (int i = 0; i < (top - 7); i += 8) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0( dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX2(dataBuffer.get(i + 4));
						shape.setY2( dataBuffer.get(i + 5));
						shape.setX3(dataBuffer.get(i + 6));
						shape.setY3( dataBuffer.get(i + 7));
						shape.fillSmooth(g2d, attributes, i / 2);
					}
					break;
				}
				case MODE_QUAD_STRIP :
				{
					QuadShape shape = new QuadShape();
					for (int i = 0; i < (top - 5); i += 6) {
						shape.setX0(dataBuffer.get(i + 0));
						shape.setY0( dataBuffer.get(i + 1));
						shape.setX1(dataBuffer.get(i + 2));
						shape.setY1( dataBuffer.get(i + 3));
						shape.setX3(dataBuffer.get(i + 4));
						shape.setY3( dataBuffer.get(i + 5));
						shape.setX2(dataBuffer.get(i + 6));
						shape.setY2( dataBuffer.get(i + 7));
						shape.fillSmooth(g2d, attributes, i / 2);
					}
					break;
				}
			}
		}
	}
}
