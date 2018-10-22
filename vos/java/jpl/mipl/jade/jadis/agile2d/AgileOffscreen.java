/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/

package jpl.mipl.jade.jadis.agile2d;


import java.awt.Component;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;

import javax.media.opengl.GL;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLDrawableFactory;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.GLException;

import jpl.mipl.jade.jadis.agile2d.AgileGraphics2D;
import jpl.mipl.jade.jadis.agile2d.AgileState;

/**
 * Support for offscreen hardware accelerated OpenGL rendering using Agile2D.
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
public class AgileOffscreen  
{
	// For saving/restoring of OpenGL state during ReadPixels
	private int[] swapbytes    = new int[1];
	private int[] lsbfirst     = new int[1];
	private int[] rowlength    = new int[1];
	private int[] skiprows     = new int[1];
	private int[] skippixels   = new int[1];
	private int[] alignment    = new int[1];
//	private int awtFormat;
	private int glFormat;
	private int glType;
//	private int glComps;
	private DataBufferByte   dbByte;
	private DataBufferInt    dbInt;
	protected GLAutoDrawable parentDrawable;
	protected GLAutoDrawable offscreenDrawable;
	protected BufferedImage offscreenImage;
	protected AgileGraphics2D jgraphics;
	protected Component comp;
	protected int width, height;

	public AgileOffscreen(GLAutoDrawable parentDrawable, int width, int height) {
        if (! GLDrawableFactory.getFactory().canCreateGLPbuffer()) {
            throw new RuntimeException("Cannot create Offscreen buffer");
        }
		this.parentDrawable = parentDrawable;
		this.width = width;
		this.height = height;
	}

	public AgileOffscreen(AgileGraphics2D parentGraphics, int width, int height) {
        this(parentGraphics.getDrawable(), width, height);
	}
    
	public BufferedImage render(Component comp) {
		if (offscreenDrawable == null) {
			GLCapabilities caps = new GLCapabilities();
			caps.setDoubleBuffered(false);
            offscreenDrawable = GLDrawableFactory.getFactory().createGLPbuffer(
                    caps, null, width, height, null);
//			offscreenDrawable = parentDrawable.createOffscreenDrawable(caps, width, height);
			offscreenDrawable.addGLEventListener(new EventHandler());
		}
		offscreenDrawable.display();
		return offscreenImage;
	}


	private class EventHandler implements GLEventListener {
		/**
		 * @see gl4java.drawable.GLEventListener#init(GLAutoDrawable)
		 */
		public void init(GLAutoDrawable drawable) 
		{
		}

		/**
		 * @see gl4java.drawable.GLEventListener#cleanup(GLAutoDrawable)
		 */
		public void cleanup(GLAutoDrawable drawable) 
		{
		}

		/**
		 * @see gl4java.drawable.GLEventListener#display(GLAutoDrawable)
		 */
		public void display(GLAutoDrawable drawable) 
		{
			if (jgraphics == null) 
				jgraphics = new AgileGraphics2D(drawable);		

			GL gl = drawable.getGL();
			AgileState glState = AgileState.get(gl);
			// Call the glClear to clear the background
			glState.glDisable(GL.GL_SCISSOR_TEST);
			glState.glDisable(GL.GL_DEPTH_TEST);
			gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

			// Restore all the Java2D Graphics defaults
			jgraphics.resetAll();
			comp.printAll(jgraphics);

			// Save current modes
			gl.glGetIntegerv(GL.GL_PACK_SWAP_BYTES,    swapbytes, 0);
			gl.glGetIntegerv(GL.GL_PACK_LSB_FIRST,     lsbfirst, 0);
			gl.glGetIntegerv(GL.GL_PACK_ROW_LENGTH,    rowlength, 0);
			gl.glGetIntegerv(GL.GL_PACK_SKIP_ROWS,     skiprows, 0);
			gl.glGetIntegerv(GL.GL_PACK_SKIP_PIXELS,   skippixels, 0);
			gl.glGetIntegerv(GL.GL_PACK_ALIGNMENT,     alignment, 0);
		
			// Little endian machines (DEC Alpha, Intel X86, PPC (in LSB
			// mode)...  for example) could benefit from setting
			// GL_PACK_LSB_FIRST to GL_TRUE instead of GL_FALSE, but this
			// would require changing the generated bitmaps too.
			gl.glPixelStorei(GL.GL_PACK_SWAP_BYTES,    GL.GL_FALSE);
			gl.glPixelStorei(GL.GL_PACK_LSB_FIRST,     GL.GL_TRUE);
			gl.glPixelStorei(GL.GL_PACK_ROW_LENGTH,    offscreenImage.getWidth());
			gl.glPixelStorei(GL.GL_PACK_SKIP_ROWS,     0);
			gl.glPixelStorei(GL.GL_PACK_SKIP_PIXELS,   0);
			gl.glPixelStorei(GL.GL_PACK_ALIGNMENT,     1);
		      
		      
			// Must now copy pixels from offscreen context into surface
			if (offscreenImage == null) 
			{
				int awtFormat = BufferedImage.TYPE_INT_ARGB; // check alpha bits
				offscreenImage = new BufferedImage((int)drawable.getWidth(),(int)drawable.getHeight(), awtFormat);
				switch (awtFormat) 
				{
					case BufferedImage.TYPE_3BYTE_BGR:
						glFormat = GL.GL_BGR;
						glType   = GL.GL_UNSIGNED_BYTE;
//						glComps  = 3;
						dbByte   = (DataBufferByte) offscreenImage.getRaster().getDataBuffer();
						break;
		
					case BufferedImage.TYPE_INT_RGB:
						glFormat = GL.GL_BGRA;
						glType   = GL.GL_UNSIGNED_BYTE;
//						glComps  = 4;
						dbInt    = (DataBufferInt) offscreenImage.getRaster().getDataBuffer();
						break;
		
					case BufferedImage.TYPE_INT_ARGB:
						glFormat = GL.GL_BGRA;
						glType   = GL.GL_UNSIGNED_BYTE;
//						glComps  = 4;
						dbInt    = (DataBufferInt) offscreenImage.getRaster().getDataBuffer();
						break;
		
					default:
						// FIXME: Support more off-screen image types (current
						// offscreen context implementations don't use others, and
						// some of the OpenGL formats aren't supported in the 1.1
						// headers, which we're currently using)
						throw new GLException("Unsupported offscreen image type " + awtFormat);
				}
			}
				
			gl.glReadBuffer(GL.GL_BACK);
			if (dbByte != null) {
                ByteBuffer buffer = ByteBuffer.wrap(dbByte.getData());
				gl.glReadPixels(0, 0, offscreenImage.getWidth(), offscreenImage.getHeight(), glFormat, glType, buffer);
			} 
			else if (dbInt != null) {
                IntBuffer buffer = IntBuffer.wrap(dbInt.getData());
				gl.glReadPixels(0, 0, offscreenImage.getWidth(), offscreenImage.getHeight(), glFormat, glType, buffer);
			}
            
			// Restore saved modes.
			gl.glPixelStorei(GL.GL_PACK_SWAP_BYTES,  swapbytes[0]);
			gl.glPixelStorei(GL.GL_PACK_LSB_FIRST,   lsbfirst[0]);
			gl.glPixelStorei(GL.GL_PACK_ROW_LENGTH,  rowlength[0]);
			gl.glPixelStorei(GL.GL_PACK_SKIP_ROWS,   skiprows[0]);
			gl.glPixelStorei(GL.GL_PACK_SKIP_PIXELS, skippixels[0]);
			gl.glPixelStorei(GL.GL_PACK_ALIGNMENT,   alignment[0]);
		      
			gl.glFlush();
			gl.glFinish();
		      
		}

		/**
		 * @see gl4java.drawable.GLEventListener#postDisplay(GLAutoDrawable)
		 */
		public void postDisplay(GLAutoDrawable drawable) 
		{
		}

		/**
		 * @see gl4java.drawable.GLEventListener#preDisplay(GLAutoDrawable)
		 */
		public void preDisplay(GLAutoDrawable drawable) 
		{
		}
        
		public void displayChanged(GLAutoDrawable o,boolean x,boolean y)
		{
        	
		}

		/**
		 * @see gl4java.drawable.GLEventListener#reshape(GLAutoDrawable, int, int)
		 */
		public void reshape(GLAutoDrawable drawable, int x, int y, int w, int h) 
		{
			if(offscreenImage != null && offscreenImage.getWidth() != w && offscreenImage.getHeight() != h) 
			{
				offscreenImage.flush();
				offscreenImage = null;
			}
		}
	}

}
