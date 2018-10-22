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
import java.nio.FloatBuffer;

import javax.media.opengl.GL;

import jpl.mipl.jade.jadis.agile2d.AgileGradientPaint;
import jpl.mipl.jade.jadis.agile2d.AgileState;

/**
 * Used to fill rectangles with a gradient.
 * 
 * Works by computing a 2texels 1D texture and filling the shapes using
 * texture indices computed through glTexGen
 */
class GradientManager {
    GL  gl;
    AgileState glState;
    float   alpha = 1; // global alpha value
    float r1, g1, b1, a1;
    float r2, g2, b2, a2;
    boolean cyclic;
    boolean invalid;
    int texture;
    // Equation for the texture coordinates
    float[]texParams = new float[4];

    GradientManager(GL gl) {
        this.gl = gl;
        this.glState = AgileState.get(gl);
    }

    void setAlpha(float a) {
        alpha = a;
    }
    
    void createTexture() {
        if (texture == 0) {
            int[] texName = { 0 };
            gl.glGenTextures(1, texName, 0);
            texture = texName[0];
            glState.bindTexture1D(texture);
            gl.glTexImage1D(GL.GL_TEXTURE_1D, 0, GL.GL_RGBA, 2, 0, GL.GL_RGBA, GL.GL_FLOAT, null);
            gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
            gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
        }
        else {
            glState.bindTexture1D(texture);
        }

        float[] texPixels = { r1, g1, b1, a1, r2, g2, b2, a2 };
        FloatBuffer buffer = FloatBuffer.wrap(texPixels);
        gl.glTexSubImage1D(GL.GL_TEXTURE_1D, 0, 0, 2, GL.GL_RGBA, GL.GL_FLOAT, buffer);
        gl.glTexParameteri(GL.GL_TEXTURE_1D, GL.GL_TEXTURE_WRAP_S, cyclic ? GL.GL_REPEAT : GL.GL_CLAMP);
    }

    void setGradient(GradientPaint gradient) {
        invalid = false;
        Point2D p1 = gradient.getPoint1();
        float originX = (float)p1.getX();
        float originY = (float)p1.getY();

        Point2D p2 = gradient.getPoint2();
        float vecX = (float)(p2.getX() - originX);
        float vecY = (float)(p2.getY() - originY);
        if (Math.abs(vecX) < 0.1 && Math.abs(vecY) < 0.1) {
            invalid = true;
            return;
        }
        if (gradient instanceof AgileGradientPaint) {
            AgileGradientPaint agl = (AgileGradientPaint)gradient;
            int c1 = agl.getC1();
            r1 = ((c1&0xFF0000)>>16) / 255.0f;
            g1 = ((c1&0xFF00)>>8) / 255.0f;
            b1 = ((c1&0xFF)) / 255.0f;
            a1 = ((c1&0xFF000000)>>>24) / 255.0f;
            
            int c2 = agl.getC2();
            r2 = ((c2&0xFF0000)>>16) / 255.0f;
            g2 = ((c2&0xFF00)>>8) / 255.0f;
            b2 = ((c2&0xFF)) / 255.0f;
            a2 = ((c2&0xFF000000)>>>24) / 255.0f;
        }
        else {
            Color color1 = gradient.getColor1();
            r1 = color1.getRed() / 255.0f;
            g1 = color1.getGreen() / 255.0f;
            b1 = color1.getBlue() / 255.0f;
            a1 = color1.getAlpha() / 255.0f;
    
            Color color2 = gradient.getColor2();
            r2 = color2.getRed() / 255.0f;
            g2 = color2.getGreen() / 255.0f;
            b2 = color2.getBlue() / 255.0f;
            a2 = color2.getAlpha() / 255.0f;
        }

        cyclic = gradient.isCyclic();
        createTexture();
        
        float p1x = originX-vecX/2;
        float p1y = originY-vecY/2;
        float pdx = 2*vecX;
        float pdy = 2*vecY;
        // Compute texture params
        // Very simple: the gradient vector is [pdx pdy 0]
        // The equation of the orthogonal plane is of the form
        // P(x,y) x * pdx + y * pdy + CST = 0
        // CST is then -(p1x * pdx + p1y * pdy)
        // We need to normalize so that P(p2x,p2y) = 1
        // so we divide the parameters by pdx*pdx+pdy*pdy 
        float den = pdx*pdx + pdy*pdy;
        texParams[0] = pdx / den;
        texParams[1] = pdy / den;
        texParams[3] = -(p1x*pdx + p1y*pdy) / den;
    }
    
    public void begin(float alpha) {
        if (invalid)
            return;
        glState.glEnable(GL.GL_TEXTURE_1D);
        glState.glEnable(GL.GL_TEXTURE_GEN_S);
        glState.bindTexture1D(texture);
        if (alpha == 1.0f && a1 == 1.0f && a2 == 1.0f) {
            if (glState.setState(GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE))
                gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_REPLACE);
        }
        else {
            glState.glColor4f(1, 1, 1, alpha);
            if (glState.setState(GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE))
                gl.glTexEnvi(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_BLEND);
            glState.glEnable(GL.GL_BLEND);
        }
        gl.glTexGeni(GL.GL_S, GL.GL_TEXTURE_GEN_MODE, GL.GL_OBJECT_LINEAR);
        gl.glTexGenfv(GL.GL_S, GL.GL_OBJECT_PLANE, texParams, 0);
    }
    
    public void end() {
        if (invalid)
            return;
        glState.glDisable(GL.GL_TEXTURE_1D);
        glState.glDisable(GL.GL_TEXTURE_GEN_S);
    }

}
