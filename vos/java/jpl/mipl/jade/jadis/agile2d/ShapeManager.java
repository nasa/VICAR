/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;


import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.WeakHashMap;

import javax.media.opengl.GL;

import jpl.mipl.jade.jadis.agile2d.Tesselator;
import jpl.mipl.jade.jadis.agile2d.TesselatorVisitor;
import jpl.mipl.jade.jadis.agile2d.geom.VertexArray;
import jpl.mipl.jade.jadis.agile2d.geom.VertexAttributes;


/**
 * Support for drawing Java2D shapes in OpenGL. The most correct approach is
 * to tesselate the shape in device space, and render the tesselation.
 * However this is very costly. ShapeManager supports two additional flags
 * for optimizations:  immutable - ShapeManager caches immutable shapes in
 * OpenGL display lists, which  are faster to render. Only works for shapes
 * that never change their geometry. convex - ShapeManager renders convex
 * shapes and strokes using GL.GL_POLYGON objects rather than tesselating. May
 * produce unexpected results if the shape is not convex.
 */
class ShapeManager extends VertexArray implements TesselatorVisitor {
    private GL gl;
    private float[] point = new float[8];
    private Tesselator tesselator;
    private double[] modelViewMatrix;
    private WeakReference dataRef;
    private double tolerance = 1;

    // Must use weak references so that the draw lists get freed when the
    // shapes do
    private WeakHashMap drawn = new WeakHashMap();
    private WeakHashMap filled = new WeakHashMap();

    // This is an array of draw lists that need deleting
    private ArrayList toDelete = new ArrayList();

    // ID generation fields
    private int numIDs;

    // ID generation fields
    private int nextID;
    private static final AffineTransform IDENTITY = new AffineTransform();

    // The ShapeInfo class is what is added to the drawn/filled maps.
    //		
    private class ShapeInfo {
        int id; // GL Drawing list for shape
        Object stroke; // Null for filled shapes

        ShapeInfo(int id, Stroke stroke) {
            this.id = id;
            this.stroke = stroke;
        }

        protected void finalize() {
            synchronized (toDelete) {
                toDelete.add(new Integer(id));
            }
        }
    }

    /**
     * Create a new ShapeManager object.
     *
     * @param gl the GL
     * @param glu the GLU
     * @param vertexArray DOCUMENT ME!
     */
    public ShapeManager(Tesselator tesselator, GL gl) {
        this.tesselator = tesselator;
        this.gl = gl;
    }

    // Called to release free draw lists
    void flush() {
        ArrayList toFree = null;
        synchronized (toDelete) {
            if (toDelete.size() > 0) {
                toFree = new ArrayList();
                toFree.addAll(toDelete);
                toDelete.clear();
            }
        }
        if (toFree != null) {
            for (int i = 0; i < toFree.size(); i++) {
                Integer id = (Integer) toFree.get(i);
                gl.glDeleteLists(id.intValue(), 1);
            }
        }
    }

    protected void tesselate(PathIterator path) {
        tesselator.tesselate(path, this);
    }

    protected void tesselate(int[] xPts, int[] yPts, int nPts) {
        tesselator.tesselate(xPts, yPts, nPts, this);
    }

    void setModelViewMatrix(double[] m) {
        modelViewMatrix = m;
    }

    // Draw a stroke
    //
    public void draw(Shape shape, VertexAttributes attributes, float scale, 
					 Stroke stroke, boolean immutable, boolean convex) {
		if (immutable) {
			//
			// Cached route - 
			//    1. flatten the stroke in object space
			//    2. draw as polygon or tesselation according to the convex parameter
			//    3. store the result in a display list
			//
			ShapeInfo info = (ShapeInfo) drawn.get(shape);
			if (info == null || info.stroke != stroke) {
				// Not in cache
				int id = genID();
				gl.glNewList(id, GL.GL_COMPILE);

				if (shape instanceof VertexArray) {
					renderLineMode((VertexArray)shape, attributes);
				} else if (convex) {
					send(getStrokeIterator(shape, scale, stroke), true);
				} else {
					tesselate(getStrokeIterator(shape, scale, stroke));
				}
				gl.glEndList();
				info = new ShapeInfo(id, stroke);
				drawn.put(shape, info);
			}
			gl.glCallList(info.id);
		} else if (shape instanceof VertexArray) {
			renderLineMode((VertexArray)shape, attributes);
		} else if (convex) {
            // Faster route - flatten the stroke in object space and draw as polygon
            send(getStrokeIterator(shape, scale, stroke), true);
		} else {
            // Slowest and most correct route - flatten the stroke in device space & tesselate			
			tesselate(getStrokeIterator(shape, scale, stroke));
        }
    }
    
	PathIterator getStrokeIterator(Shape shape, float scale, Stroke stroke) {
		return stroke.createStrokedShape(shape).getPathIterator(IDENTITY, tolerance/scale);
	}


    // Fill a Shape.
    //
    public void fill(Shape shape, VertexAttributes attributes, float scale, boolean immutable, boolean convex) {
		if (immutable) {
			//
			// Cached route - 
			//    1. flatten the shape in object space
			//    2. draw as polygon or tesselation according to the convex parameter
			//    3. store the result in a display list
			//
			ShapeInfo info = (ShapeInfo) filled.get(shape);
			if (info == null) {
				// Not in cache
				int id = genID();
				gl.glNewList(id, GL.GL_COMPILE);
				
				if (shape instanceof VertexArray) {
					//if (varray.getMode() < VertexArray.MODE_TRIANGLES)
					//	return; // not filled
					render(gl, (VertexArray)shape, attributes);
				} else {
					PathIterator path = shape.getPathIterator(IDENTITY, tolerance/scale);
					tesselate(path);
				}

				gl.glEndList();
				info = new ShapeInfo(id, null);
				filled.put(shape, info);
			}
			gl.glCallList(info.id);
		}
		else if (shape instanceof VertexArray) {
			render(gl, (VertexArray)shape, attributes);
		} else if (convex) {
			// Faster route - flatten the shape in object space and draw as polygon
			PathIterator path = shape.getPathIterator(IDENTITY, tolerance/scale);
			send(path, true);
		}
		else {
			// Slowest and most correct route - flatten the shape in device space & tesselate
			PathIterator path = shape.getPathIterator(IDENTITY, tolerance/scale);
			tesselate(path);
		}
    }

    // Tesselate a polygon defined as point arrays
    //
    public void fill(int[] xPts, int[] yPts, int nPts, boolean convex) {
        switch (nPts) {
            case 1 :
            case 2 :
                return;
            case 3 :
                gl.glBegin(GL.GL_TRIANGLES);
                for (int i = 0; i < 3; i++)
                    gl.glVertex2i(xPts[i], yPts[i]);
                gl.glEnd();
                return;
            case 4 :
                gl.glBegin(GL.GL_QUADS);
                for (int i = 0; i < 4; i++)
                    gl.glVertex2i(xPts[i], yPts[i]);
                gl.glEnd();
                return;
        }
        if (convex) {
            begin(GL.GL_POLYGON);
            for (int i = 0; i < nPts; i++) {
                addVertex(xPts[i], yPts[i]);
            }
            end();
        }
        else {
            // Slower and more correct route - tesselate
            tesselate(xPts, yPts, nPts);
        }
    }

    // Basic routine to iterate over a path and output its points as a GL shape
    //
    public void send(PathIterator path, boolean fill) {
        boolean started = false;
        while (!path.isDone()) {
            switch (path.currentSegment(point)) {
                case PathIterator.SEG_MOVETO :
                    if (started) {
                        end();
                    }
                    else {

                        started = true;
                    }
                    begin(fill ? MODE_POLYGON : MODE_LINE_STRIP);

                    /* FALLTHROUGH */
                case PathIterator.SEG_LINETO :
                    //System.out.println("LINETO " + point[0] + ", " + point[1]);
                    addVertex(point);
                    break;

				case PathIterator.SEG_CLOSE :
                    if (!fill)
                        setMode(MODE_LINE_LOOP);
                    end();
                    started = false;
                    break;
            }
            path.next();
        }
        if (started) {
            end();
        }
    }

    private int genID() {
        if (numIDs == 0) {
            nextID = gl.glGenLists(5000);
            numIDs = 5000;
        }
        int id = nextID++;
        numIDs--;
        return id;
    }
    /**
     * Returns the tolerance.
     *
     * @return double
     */
    public double getTolerance() {
        return tolerance;
    }

    /**
     * Sets the tolerance.
     *
     * @param tolerance The tolerance to set
     */
    public void setTolerance(double tolerance) {
        this.tolerance = tolerance;
    }
    // TesselatorVisitor

    /**
     * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#begin(int)
     */
    public void begin(int mode) {
        clear();
        setMode(mode);
    }

    /**
     * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#combine(double[], double[], float[], double[])
     */
    public void combine(double[] coords, double[] vertex_data, float[] weight, double[] dataOut) {
        Tesselator.defaultCombine(coords, vertex_data, weight, dataOut);
    }

    /**
     * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#error(int)
     */
    public void error(int errorCode) {
        Tesselator.defaultError(errorCode);
    }

    /**
     * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#end()
     */
    public void end() {
        render();
    }

    public Tesselator getTesselator() {
        return tesselator;
    }

	public void render() {
		render(gl, this, null);
	}

	void renderLineMode(VertexArray array, VertexAttributes attributes) {
		if (array.getMode() >= VertexArray.MODE_TRIANGLES) {
			gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_LINE);                    
		}
		render(gl, array, attributes);
		if (array.getMode() >= VertexArray.MODE_TRIANGLES) {
			gl.glPolygonMode(GL.GL_FRONT_AND_BACK, GL.GL_FILL);                    
		}
	}

	// Basic method for rendering a VertexArray
	static void render(GL gl, VertexArray array, VertexAttributes attributes) {
			
		int count = array.getVertexCount();
		java.nio.FloatBuffer arrayData = array.getDataRef();

		if (count == 0) {
			return;
		}

		if (attributes == null) {
			// It would be great if we could avoid setting the vertex pointer, but
			// we couldn't because Java Garbage Collector can move objects at any time.
			gl.glVertexPointer(2, GL.GL_FLOAT, 0, arrayData);
			gl.glDrawArrays(array.getMode(), 0, count);
			return;
		}

		count = Math.min(array.getVertexCount(), attributes.getSize());
		java.nio.ByteBuffer attributeData = attributes.getColorRef();

		if (attributes.isSmooth()) {
			gl.glShadeModel(GL.GL_SMOOTH);
		}

		// It would be great if we could avoid setting the vertex pointer, but
		// we couldn't because Java Garbage Collector can move objects at any time.
		// TODO: JM - we could potentially use the VertexBufferObject in JOGL to address this!
		gl.glEnableClientState(GL.GL_COLOR_ARRAY);
		gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
		gl.glVertexPointer(2, GL.GL_FLOAT, 0, arrayData);
		gl.glColorPointer(4, GL.GL_UNSIGNED_BYTE, 0, attributeData);
		gl.glDrawArrays(array.getMode(), 0, count);
		gl.glDisableClientState(GL.GL_COLOR_ARRAY);
		gl.glDisableClientState(GL.GL_VERTEX_ARRAY);

		if (attributes.isSmooth()) {
			gl.glShadeModel(GL.GL_FLAT);
		}
	}
}
