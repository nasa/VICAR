/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/

package jpl.mipl.jade.jadis.agile2d;

import java.awt.Font;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;

import javax.media.opengl.GLAutoDrawable;

/**
 * Basic services for Font renderers.
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
abstract class BasicFontRenderer {
    protected Font font;
    protected AffineTransform transform;
    protected FontRenderContext frc;
    protected GlyphVector glyphs;
    protected boolean installed;

    static char[] latin1Chars;

    static {
        latin1Chars = new char[256];
        for (int i = 0; i < 256; i++)
            latin1Chars[i] = (char) i;
    }

    protected BasicFontRenderer() {
        transform = new AffineTransform();
    }

    /**
     * Installs the resources to render the font and return <code>true</code>
     * if the font has been installed or <code>false</code> otherwise.
     * 
     * @param drawable the GLDrawable
     * @param scale the current scane
     * @param font the font
     * @return <code>true</code> if the font has been installed,
     * <code>false</code> otherwise.
     */
    public boolean install(GLAutoDrawable drawable, Font font, double scale, boolean aa, boolean ufm) {
        installed = false;
        return false;
    }

    protected void setup() {
        glyphs = font.createGlyphVector(frc, latin1Chars);
        Point2D.Double zero = new Point2D.Double();
        for (int i = 0; i < latin1Chars.length; i++) {
            glyphs.setGlyphPosition(i, zero);
        }
    }

    /**
     * Called after install has returned <code>true</code>, this call
     * renders the font on the drawable at the current 0,0 position.
     * 
     * @param drawable
     * @param string
     * @param scale
     * @param font
     */
    public abstract void render(GLAutoDrawable drawable, String string, double scale, Font font);

    /**
     * Returns <code>true</code> if the font is ready to be rendered.
     * @return <code>true</code> if the font is ready to be rendered.
     */
    public boolean isInstalled() {
        return installed;
    }

    public void release(GLAutoDrawable drawable) {
        installed = false;
    }
}
