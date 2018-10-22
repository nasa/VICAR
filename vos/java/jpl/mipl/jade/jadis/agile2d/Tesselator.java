/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;



import java.awt.geom.PathIterator;

import javax.media.opengl.glu.GLU;
import javax.media.opengl.glu.GLUtessellator;
import javax.media.opengl.glu.GLUtessellatorCallbackAdapter;

import jpl.mipl.jade.jadis.agile2d.TesselatorVisitor;


/**
 * Wrapper on the GLU Tess Functions, used to tesselate arbitary flattened
 * paths.
 */
final class Tesselator extends GLUtessellatorCallbackAdapter {
    private GLUtessellator tobj;
    private GLU           glu;
    private double[]          point = new double[8];
	// JM - rather than using a static array like this, now dynamically allocates points
	// private double[][]        coords = new double[10000][3];
    private TesselatorVisitor visitor;

    /**
     * Create a new Tesselator object.
     *
     * @param gl DOCUMENT ME!
     * @param glu DOCUMENT ME!
     * @param visitor DOCUMENT ME!
     */
    public Tesselator(GLU glu) {
        this.glu = glu;
        tobj = glu.gluNewTess();

        // Set up Tess Object
        //
        glu.gluTessCallback(tobj, GLU.GLU_TESS_VERTEX, this);
        glu.gluTessCallback(tobj, GLU.GLU_TESS_BEGIN, this);
        glu.gluTessCallback(tobj, GLU.GLU_TESS_END, this);
        glu.gluTessCallback(tobj, GLU.GLU_TESS_ERROR, this);
        glu.gluTessCallback(tobj, GLU.GLU_TESS_COMBINE, this);
        
        // Important: Specify the normal for the shape as a whole,
        // so that GLU doesn't both calculating it. Since all our flat
        // shapes are in the z=0 plane, the normal is easy to calculate:
        //
        glu.gluTessNormal(tobj, 0.0, 0.0, -1.0);

        // 
        // Ideally, the GLU tesselator would contain a hint to indicate
        // whether or not to use its greedy approach to pick the best mesh
        // type to use. For cases such as font outlines (which are cached) this
        // greedy approach makes sense. However, for immediate-mode rendering
        // the greedy approach is not useful...
        //
    }

    /**
     * Tesselates the interior of a polygon defined by a path. The path must
     * be a flattened path.
     *
     * @param path DOCUMENT ME!
     */
    public void tesselate(PathIterator path, TesselatorVisitor visitor) {
        this.visitor = visitor;
        switch (path.getWindingRule()) {
        case PathIterator.WIND_EVEN_ODD:
            setWindingRule(GLU.GLU_TESS_WINDING_ODD);
            break;
        case PathIterator.WIND_NON_ZERO:
            setWindingRule(GLU.GLU_TESS_WINDING_NONZERO);
            break;
        }

        glu.gluTessBeginPolygon(tobj, (double[])null);

        //System.out.println("BEGIN TESS");
        boolean closed = true;
        int     numCoords = 0;

        while (!path.isDone()) {
            switch (path.currentSegment(point)) {
            case PathIterator.SEG_MOVETO:
                if (!closed) {
                    glu.gluTessEndContour(tobj);
                } else
                    closed = false;
                glu.gluTessBeginContour(tobj);

            //System.out.println("NEW CONTOUR");
            //System.out.print("MOVE");

            /* FALLTHROUGH */
            case PathIterator.SEG_LINETO:
                if (closed) {
                    glu.gluTessBeginContour(tobj);
                    closed = false;
                    //System.out.println("Lineto without moveto");
                }
                //System.out.println("LINETO " + point[0] + ", " + point[1]);
                double[] coord = new double[3]; // coords[numCoords];
                coord[0] = point[0];
                coord[1] = point[1];
                coord[2] = 0;
                glu.gluTessVertex(tobj, coord, 0, coord);
                numCoords++;
                break;
            case PathIterator.SEG_CLOSE:
                glu.gluTessEndContour(tobj);
                closed = true;
                break;
            }
            path.next();
        }
        if (!closed) {
            glu.gluTessEndContour(tobj);
        }
        glu.gluTessEndPolygon(tobj);
        //System.out.println("END TESS");
    }

    /**
     * Tesselates the interior of a polygon defined by a list of points.
     *
     * @param xPts DOCUMENT ME!
     * @param yPts DOCUMENT ME!
     * @param nPts DOCUMENT ME!
     */
    public void tesselate(int[] xPts, int[] yPts, int nPts, TesselatorVisitor visitor) {
        if (nPts < 3)
            return;
        this.visitor = visitor;
        setWindingRule(GLU.GLU_TESS_WINDING_ODD);

        glu.gluTessBeginPolygon(tobj, (double[])null);
        glu.gluTessBeginContour(tobj);

        for (int i = 0; i < nPts; i++) {
            double[] coord = new double[3]; // coords[i];
            coord[0] = xPts[i];
            coord[1] = yPts[i];
            coord[2] = 0;
            glu.gluTessVertex(tobj, coord, 0, coord);
        }

        glu.gluTessEndContour(tobj);
        glu.gluTessEndPolygon(tobj);
    }

    private void setWindingRule(int rule) {
        glu.gluTessProperty(tobj, GLU.GLU_TESS_WINDING_RULE, rule);
    }

    /**
     * Frees the OpenGL TessObj associated with this tesselator
     */
    public void dispose() {
        glu.gluDeleteTess(tobj);
        glu = null;
    }

    /**
     * Implements TessCallback's TESS_VERTEX method.
     *
     * @param v DOCUMENT ME!
     */
    public void tessVertex(double[] v) {
        visitor.addVertex(v);

        //System.out.println("glVertex(" + v[0] + ", " + v[1] + ", " + v[2] + ")");
    }

    /**
     * Implements TessCallback's TESS_BEGIN method.
     *
     * @param which DOCUMENT ME!
     */
    public void tessBegin(int which) {
        visitor.begin(which);

        //System.out.println("glBegin(" + which + ")");
    }

    /**
     * Implements TessCallback's TESS_END method.
     */
    public void tessEnd() {
        visitor.end();

        //System.out.println("glEnd()");
    }

    /**
     * Implements TessCallback's TESS_ERROR method.
     *
     * @param errorCode DOCUMENT ME!
     */
    public void tessError(int errorCode) {
        visitor.error(errorCode);
    }

    /**
     * Implements TessCallback's TESS_COMBINE method. combine is used to
     * create a new vertex when edges intersect.  coordinate location is
     * trivial to calculate, but weight[4] may be used to average color,
     * normal, or texture  coordinate data.
     *
     * @param coords DOCUMENT ME!
     * @param data DOCUMENT ME!
     * @param weight DOCUMENT ME!
     * @param dataOut DOCUMENT ME!
     */
    public void tessCombine(double[] coords, double[] data, float[] weight,
                            double[] dataOut) {
        visitor.combine(coords, data, weight, dataOut);
    }
    
    /**
     * Default implementation of the TesselatorVisitor combine method usable
     * in any implementations.
     *
     * @param coords DOCUMENT ME!
     * @param data DOCUMENT ME!
     * @param weight DOCUMENT ME!
     * @param dataOut DOCUMENT ME!
     */
    public static void defaultCombine(double[] coords, double[] data,
                                      float[] weight, double[] dataOut) {
        dataOut[0] = coords[0];
        dataOut[1] = coords[1];
        dataOut[2] = coords[2];
    }

    /**
     * Default implementation of the TesselatorVisitor error method usable in
     * any implementations.
     *
     * @param errorCode DOCUMENT ME!
     */
    public static void defaultError(int errorCode) {
        System.err.println("Tess Error: " + errorCode);
    }
}
