/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.nio.ByteBuffer;
import java.util.Iterator;
import java.util.LinkedList;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;

import jpl.mipl.jade.jadis.agile2d.AgileState;
import jpl.mipl.jade.jadis.agile2d.BasicFontRenderer;

/**
 * Render Fonts as grayscale images
 *
 * A texture containing a mosaic of character glyphs is used to
 * allow OpenGL to render the fonts.
 *
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
class TextureFontRenderer extends BasicFontRenderer {
    int texture;
    CacheInfo current;
    boolean highQuality = true;
    boolean incremental = false;

    static int maxCacheLength = 100;
    static LinkedList cache = new LinkedList();

    static int texWidth = 512;
    static int texHeight = 512;

    public TextureFontRenderer() {
    }

    public static boolean isSameScale(double scale1, double scale2) {
        return Math.abs(scale1 - scale2) < 0.01;
    }

    public static void setMaxTextureSize(int size) {
        texWidth = size;
        texHeight = size;
    }

    public static int countGlyphs(Font font) {
        int cnt = 0;
        for (int i = 0; i < 256; i++) {
            if (font.canDisplay((char) i))
                cnt++;
        }
        return cnt;
    }

    static class CacheInfo {
        static AffineTransform tmpTransform = new AffineTransform();
        static char[] tmpChar = { 0 };
        Font font;
        FontRenderContext frc;
        double scale;
        FontMetrics metrics;
        BufferedImage image;
        Graphics2D graphics;
        // Texture indices for each defined character
        Rectangle2D.Double[] charTexIndices;
        // Pixel dimension for each defined character
        Rectangle[] charDimensions;
        GlyphVector glyphs;

        CacheInfo(Font font, double scale, boolean aa, boolean ufm) {
            this.font = font;
            tmpTransform.setToScale(scale, scale);
            this.frc = new FontRenderContext(tmpTransform, aa, ufm);
            this.scale = scale;
            charTexIndices = new Rectangle2D.Double[256];
            charDimensions = new Rectangle[256];
        }

        public boolean equals(Font font, double scale, boolean aa, boolean ufm) {
            return this.font.equals(font) &&
                   frc.isAntiAliased() == aa &&
                   frc.usesFractionalMetrics() == ufm &&
                   ((this.scale <= scale && charTexIndices == null) ||
                    isSameScale(this.scale, scale));
        }

        public int createImage() {
            // First, a quick check with just the max char bounds
            // Pessimistic but much faster than the exact computation.
            // We use a "fuge factor" to be a little less pessimistic.  If it is too
            // optimistic, there is a check later.
            final float fudge = 0.5f;

            Rectangle2D maxBounds = font.getMaxCharBounds(frc);
            // getMaxCharBounds doesn't look at the transform in the FontRenderContext
            int minCols = maxBounds.getWidth() == 0 ? 0 : texWidth / (int) Math.ceil(maxBounds.getWidth()*scale);

            if (minCols == 0) {
                charTexIndices = null;
                charDimensions = null;
                return 0;
            }
            int minRows = texHeight / (int) Math.ceil(maxBounds.getHeight()*scale);
            if (minRows == 0 || (minRows * minCols < countGlyphs(font)*fudge)) {
                charTexIndices = null;
                charDimensions = null;
                return 0;
            }

            // We passed the pessimistic computation, now compute the
            // exact required image height.
            // Just allocate in order and wrap to next line when reaching the
            // right side of the image.
            // TODO: sort characters according to their height before storing them
            // in the image so that they are more packed.  If one line contains very
            // small glyphs except for one deep and high one, we need the whole
            // height for the whole line.
            int lastX = 0;
            int lastY = 0;
            int curHeight = 0;
            Point2D.Double zero = new Point2D.Double();

            glyphs = font.createGlyphVector(frc, latin1Chars);

            for (int i = 0; i < 256; i++) {
                if (!font.canDisplay((char) i))
                    continue;
                glyphs.setGlyphPosition(i, zero);
                // The code below is not optimal.
                // The optimal code, commented out, is VERY slow since it forces a
                // rendering of each glyph!
                Rectangle d = glyphs.getGlyphVisualBounds(i).getBounds();
                if (d == null || d.isEmpty())
                    continue;
                d.x -= 1;
                d.y -= 1;
                d.width = 2 + (int)(d.width * scale);
                d.height = 2 + (int)(d.height * scale);
//                Rectangle d = glyphs.getGlyphPixelBounds(i, frc, 0, 0);
//                if (d == null || d.isEmpty())
//                    continue;
                charDimensions[i] = d;
                if ((texWidth - lastX) < d.width) {
                    lastX = 0;
                    lastY += curHeight;
                    curHeight = 0;
                }
                if ((texHeight - lastY) < d.height) {
                    charTexIndices = null;
                    charDimensions = null;
                    //System.err.println("refusing to allocate bitmap");
                    return 0; // ODD: Not enough room, refuse to allocate
                }
                // create a texture index not normalized for now.
                // Normalization will mean present in the mosaic.
                Rectangle2D.Double tex =
                    new Rectangle2D.Double(lastX, lastY, d.width, d.height);
                charTexIndices[i] = tex;
                curHeight = Math.max(curHeight, d.height);
                lastX += d.width;
            }
            int height = lastY + curHeight;
            image = new BufferedImage(texWidth, height, BufferedImage.TYPE_BYTE_GRAY);
            return height;
        }

        public boolean isImageTooLarge() {
            return charTexIndices == null;
        }

        public boolean isCharRendered(char c) {
            return !font.canDisplay(c) ||
                   charTexIndices[c] == null ||
                   charTexIndices[c].getWidth() <= 1.0;
        }

        public synchronized boolean createMosaic(String s, int minGlyphsToRender) {
            int i;

            if (metrics == null) {
                // we never tried to create any character in the mosaic yet
                graphics = (Graphics2D) image.getGraphics();
                graphics.setFont(font.deriveFont(frc.getTransform()));
                graphics.setRenderingHint(
                    RenderingHints.KEY_TEXT_ANTIALIASING,
                    (frc.isAntiAliased() ?
                     RenderingHints.VALUE_TEXT_ANTIALIAS_ON :
                     RenderingHints.VALUE_TEXT_ANTIALIAS_OFF));
                graphics.setRenderingHint(
                    RenderingHints.KEY_FRACTIONALMETRICS,
                    (frc.usesFractionalMetrics() ?
                     RenderingHints.VALUE_FRACTIONALMETRICS_ON :
                     RenderingHints.VALUE_FRACTIONALMETRICS_OFF));
                metrics = graphics.getFontMetrics();
            }
            else if (graphics == null) {
                // we have already filled the mosaic
                return false;
            }
            // we need to make sure the characters in the string are in the
            // mosaic and we may want to create more if we have time
            boolean missing = false;
            for (i = 0; i < s.length(); i++) {
                if (! isCharRendered(s.charAt(i))) {
                    missing = true;
                    break;
                }
            }
            if (! missing) {
                // maybe create other characters, we'll see later
                return false;
            }

            for (i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                if (isCharRendered(c))
                    continue;

                minGlyphsToRender--;
                renderChar(c);
            }
            while (minGlyphsToRender-- > 0) {
                int c = nextGlyphToRender();
                if (c == -1) {
                    //System.out.println("Font rendering finished for "+font);
                    graphics = null;
                    break;
                }
                renderChar((char)c);
            }
            //new ImageBrowser(font.toString(), image);
            return true;
        }

        private int nextGlyphToRender() {
            for (char c = 0; c < 256; c++) {
                if (! isCharRendered(c))
                    return c;
            }
            return -1;
        }

        private void renderChar(char c) {
            Rectangle2D.Double tex = charTexIndices[c];
            Rectangle d = glyphs.getGlyphPixelBounds(c, frc, 0, 0);
            charDimensions[c] = d; // put the exact bounds now.

            tmpChar[0] = c;
            graphics.drawChars(tmpChar, 0, 1, (int)tex.x - d.x, (int)tex.y - d.y);
            tex.setRect(
                tex.x/texWidth, tex.y/texHeight,
                (double)d.width/texWidth, (double)d.height/texHeight);
        }

        public void installImage(GL gl) {
            if (image == null)
                return;
            DataBufferByte buffer =
                (DataBufferByte) image.getData().getDataBuffer();
            byte[] bytes = buffer.getBankData()[0];
            ByteBuffer byteBuffer = ByteBuffer.wrap(bytes);
            gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);
            gl.glTexSubImage2D(
                GL.GL_TEXTURE_2D,
                0,
                0,
                0,
                image.getWidth(),
                image.getHeight(),
                GL.GL_ALPHA,
                GL.GL_UNSIGNED_BYTE,
                byteBuffer);
            gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 4);
        }
    }


    public static CacheInfo getCachedAt(int index) {
        return (CacheInfo) cache.get(index);
    }

    public static CacheInfo findCached(Font font, double scale, boolean aa, boolean ufm) {
        CacheInfo info = null;
        boolean first = true;
        synchronized (cache) {
            for (Iterator it = cache.iterator(); it.hasNext();) {
                info = (CacheInfo) it.next();
                if (info.equals(font, scale, aa, ufm)) {
                    if (!first) {
                        it.remove();
                        cache.addFirst(info);
                    }
                    return info;
                }
                first = false;
            }
            info = new CacheInfo(font, scale, aa, ufm);
            info.createImage();
            cache.addFirst(info);
            setMaxCacheLength(maxCacheLength); // cleanup cache
        }
        return info;
    }

    /**
     * Returns the maxCacheLength.
     * @return int
     */
    public static int getMaxCacheLength() {
        return maxCacheLength;
    }

    public static void setMaxCacheLength(int maxCacheLength) {
        if (maxCacheLength < 0)
            maxCacheLength = 0;
        TextureFontRenderer.maxCacheLength = maxCacheLength;
        while (cache.size() > maxCacheLength)
            cache.removeLast();
    }

    public boolean isHighQuality() {
        return highQuality;
    }

    public void setHighQuality(boolean highQuality) {
        this.highQuality = highQuality;
    }

    protected void bind(AgileState glState) {
        GL gl = glState.getGL();
        // Allocates just one texture (for now) and copy per-font texture mosaic into it.
        if (texture == 0) {
            int maxTex[] = { 0 };
            gl.glGenTextures(1, maxTex, 0);

            texture = maxTex[0];
            glState.bindTexture2D(texture);
            // Use the Nearest filters to avoid messing up anti-aliasing
            // without having to pixel-align the transform, which is very expensive
            gl.glTexParameterf(
                GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_MAG_FILTER,
                GL.GL_NEAREST);
            gl.glTexParameterf(
                GL.GL_TEXTURE_2D,
                GL.GL_TEXTURE_MIN_FILTER,
                GL.GL_NEAREST);
            gl.glTexImage2D(
                GL.GL_TEXTURE_2D,
                0,
                GL.GL_ALPHA,
                texWidth,
                texHeight,
                0,
                GL.GL_ALPHA,
                GL.GL_UNSIGNED_BYTE,
                null);
        }
        else {
            glState.bindTexture2D(texture);
        }
    }

    public boolean install(GLAutoDrawable drawable,
        Font font, double scale, boolean aa, boolean ufm) {
        if (current != null && current.equals(font, scale, aa, ufm)) {
            installed = true;
            return true;
        }
        installed = false;
        double s = highQuality ? scale : (int) (scale * 10 + 9) / 10;
        CacheInfo info = findCached(font, s, aa, ufm);
        if (info.isImageTooLarge())
            return false;

        current = info;
        return true;
    }

    public void render(
        GLAutoDrawable drawable,
        String string,
        double scale,
        Font font) {

        //!!!! ozp GL gl = drawable.getGL();
    	GL gl = drawable.getContext().getGL();
        AgileState glState = AgileState.get(gl);
        bind(glState);
        // next line will install the texture the first time the font
        // is used.
        if (current.createMosaic(string, isIncremental() ? 10 : 256) ||
            ! installed) {
            current.installImage(gl);
            installed = true;
        }
        glState.glEnable(GL.GL_TEXTURE_2D);
        glState.glEnable(GL.GL_BLEND);
        if (glState.setState(GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE)) {
            gl.glTexEnvf(
                GL.GL_TEXTURE_ENV,
                GL.GL_TEXTURE_ENV_MODE,
                GL.GL_MODULATE);
        }
        gl.glScaled(1 / current.scale, 1 / current.scale, 1);
        for (int i = 0; i < string.length(); i++) {
            int c = string.charAt(i);

            Rectangle2D.Double texCoords = current.charTexIndices[c];
            Rectangle dim = current.charDimensions[c];

            if (texCoords != null
                && dim != null
                && !texCoords.isEmpty()) {
                gl.glBegin(GL.GL_QUADS);
                gl.glTexCoord2d(
                    texCoords.getMinX(),
                    texCoords.getMinY());
                gl.glVertex2d(dim.getMinX(), dim.getMinY());
                gl.glTexCoord2d(
                    texCoords.getMaxX(),
                    texCoords.getMinY());
                gl.glVertex2d(dim.getMaxX(), dim.getMinY());
                gl.glTexCoord2d(
                    texCoords.getMaxX(),
                    texCoords.getMaxY());
                gl.glVertex2d(dim.getMaxX(), dim.getMaxY());
                gl.glTexCoord2d(
                    texCoords.getMinX(),
                    texCoords.getMaxY());
                gl.glVertex2d(dim.getMinX(), dim.getMaxY());
                gl.glEnd();
                
            }
            gl.glTranslated(current.metrics.charWidth(c), 0, 0);
        }
        glState.glDisable(GL.GL_TEXTURE_2D);
    }

    public void render(
            GLAutoDrawable drawable,
            GlyphVector g,
            double scale) {
        StringBuffer s = new StringBuffer();
        for (int i = 0; i < g.getNumGlyphs(); i++) {
            for (char c = 0; c < 256; c++) {
                if (current.glyphs.getGlyphCode(c) == g.getGlyphCode(i)) {
                    s.append(c);
                    break;
                }
            }
        }
        render(drawable, s.toString(), scale, g.getFont());
    }

    public void release(GLAutoDrawable drawable) {
        if (!installed)
            return;
        installed = false;
        GL gl = drawable.getGL();
        AgileState glState = AgileState.get(gl);
        //gl.glPopAttrib();
        glState.glDisable(GL.GL_TEXTURE_2D);
    }

    public boolean isIncremental() {
        return incremental;
    }

    public void setIncremental(boolean b) {
        incremental = b;
    }

}
