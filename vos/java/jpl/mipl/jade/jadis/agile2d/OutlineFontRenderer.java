/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;


import java.awt.Font;
import java.awt.Shape;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphMetrics;
import java.awt.font.GlyphVector;
import java.util.Iterator;
import java.util.LinkedList;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;

import jpl.mipl.jade.jadis.agile2d.*;
import jpl.mipl.jade.jadis.agile2d.geom.VertexArray;
import jpl.mipl.jade.jadis.agile2d.geom.VertexArrayList;


/**
 * Render Fonts from their outlines
 *
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
class OutlineFontRenderer extends BasicFontRenderer {
    GlyphMetrics metrics[];
    VertexArrayList vertices[];
    Tesselator tesselator;
    int listBase;    
    Font listFont[] = new Font[256]; // character font currently in display list

	/** 
	 * Tesselates a shape and stores the result in a VertexArrayList
	 */
	static class VATesselatorVisitor implements TesselatorVisitor {	
		VertexArrayList list;
		VertexArray last;

		VATesselatorVisitor(VertexArrayList list) {
			this.list = list;
		}
    
		/**
		 * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#begin(int)
		 */
		public void begin(int mode) {
			VertexArray v = new VertexArray();
			v.setMode(mode);
			list.add(v);
			last = v;
		}

		/**
		 * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#addVertex(double[])
		 */
		public void addVertex(double[] coords) {
			last.addVertex(coords);
		}

		/**
		 * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#addVertex(double, double)
		 */
		public void addVertex(double x, double y) {
			last.addVertex(x, y);
		}

		/**
		 * @see jpl.mipl.jade.jadis.agile2d.TesselatorVisitor#end()
		 */
		public void end() {
			last = null;
		}

		// TesselatorVisitor
		public void combine(double coords[/*3*/], double data[/*4xn*/],  float weight[/*4*/], double[/*3*/] dataOut) {
			Tesselator.defaultCombine(coords, data, weight, dataOut);
		}

		public void error(int errorCode) {
			Tesselator.defaultError(errorCode);
		}   
	}
	
	
	static class CacheInfo {
        Font font;
        GlyphMetrics metrics[];
        VertexArrayList vertices[];

        CacheInfo(Font font) {
            this.font = font;
            metrics = new GlyphMetrics[256];
        }
    }

    LinkedList cache = new LinkedList();
    int maxCacheLength = 20;

    public CacheInfo findCached(Font font) {
        CacheInfo info = null;
        boolean first = true;
        for (Iterator it = cache.iterator(); it.hasNext();) {
            info = (CacheInfo) it.next();
            if (info.font.equals(font)) {
                if (!first) {
                    it.remove();
                    cache.addFirst(info);
                }
                return info;
            }
            first = false;
        }
        info = new CacheInfo(font);
        cache.addFirst(info);
        setMaxCacheLength(maxCacheLength);

        return info;
    }

    /**
     * Returns the maxCacheLength.
     * @return int
     */
    public int getMaxCacheLength() {
        return maxCacheLength;
    }

    /**
     * Sets the maxCacheLength.
     * @param maxCacheLength The maxCacheLength to set
     */
    public void setMaxCacheLength(int maxCacheLength) {
        if (maxCacheLength < 0)
            maxCacheLength = 0;
        this.maxCacheLength = maxCacheLength;
        while (cache.size() > maxCacheLength)
            cache.removeLast();
    }

    public OutlineFontRenderer(Tesselator tesselator) {
        this.tesselator = tesselator;
    }

    public boolean install(GLAutoDrawable drawable, Font font, double scale, boolean aa, boolean ufm) {
        if (this.font != null && this.font.equals(font)) {
            installed = true;
            return true;
        }
        CacheInfo info = findCached(font);
        if (info == null) {
            installed = false;
            return false;
        }
        this.font = info.font;
        this.frc = new FontRenderContext(null, aa, ufm);
        metrics = info.metrics;
        vertices = info.vertices;

        if (vertices == null) {
            setup();
        
            if (listBase == 0) {
                GL gl = drawable.getGL();
                listBase = gl.glGenLists(256);
            }

            vertices = new VertexArrayList[256];
            info.vertices = vertices;
//            for (int i = 0; i < latin1Chars.length; i++) {
//                addTesselation(drawable, latin1Chars[i]);
//            }
        }

        installed = true;
        return true;
    }

    protected VertexArrayList getVertices(GLAutoDrawable drawable, int c) {
        if (vertices[c] == null) {
            addTesselation(drawable, latin1Chars[c]);
        }
        return vertices[c];
    }
    
    protected boolean installChar(GLAutoDrawable drawable, int c) {
        if (listFont[c] == font)
            return true;
        
        VertexArrayList v = getVertices(drawable, c);
        if (v == null)
            return false;
        GL gl = drawable.getGL();
        gl.glNewList(listBase + c, GL.GL_COMPILE);
		for (int i = 0; i < v.size(); i++) 
			ShapeManager.render(gl, v.getVertexArrayAt(i), null);
        gl.glEndList();
        listFont[c] = font;
        return true;
    }

    public void render(GLAutoDrawable drawable, String string, double scale, Font font) {
        if (!installed)
            return;
        int i;

        GL gl = drawable.getGL();

        for (i = 0; i < string.length(); i++) {
            int c = string.charAt(i);
            if (c > metrics.length)
                continue;
            if (installChar(drawable, c)) {
                GlyphMetrics m = metrics[c];
                gl.glCallList(listBase + c);
                gl.glTranslated(m.getAdvanceX(), m.getAdvanceY(), 0);
            }
        }
        installed = false;
    }
    
    public void render(GLAutoDrawable drawable, GlyphVector g, double scale) {
        if (!installed)
            return;
        int i;

        GL gl = drawable.getGL();

        for (i = 0; i < g.getNumGlyphs(); i++) {
            int c = g.getGlyphCode(i);
            if (c > metrics.length)
                continue;
            if (installChar(drawable, c)) {
                GlyphMetrics m = metrics[c];
                gl.glCallList(listBase + c);
                gl.glTranslated(m.getAdvanceX(), m.getAdvanceY(), 0);
            }
        }
        installed = false;
    }

    public void release(GLAutoDrawable drawable) {
        installed = false;
    }

    public boolean addTesselation(GLAutoDrawable drawable, int charIndex) {
        Shape s = glyphs.getGlyphOutline(charIndex);
        if (s == null)
            return false;
        metrics[charIndex] = glyphs.getGlyphMetrics(charIndex);
        VertexArrayList v = new VertexArrayList();
		VATesselatorVisitor visitor = new VATesselatorVisitor(v);
        tesselator.tesselate(s.getPathIterator(null, 0.01), visitor);
        vertices[charIndex] = v;
        return true;
    }
}
