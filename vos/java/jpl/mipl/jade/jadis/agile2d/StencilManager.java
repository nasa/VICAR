/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;

import java.awt.geom.Rectangle2D;

import javax.media.opengl.GL;

import jpl.mipl.jade.jadis.agile2d.AgileState;

/**
 * Manages the OpenGL Stencil buffer - making it appear to the app
 * as a separate set of stencil planes that can be written or used
 * independently.<p>
 *
 * To use stencils, first render the shape you want to clip against 
 * into the stencil:
 *
 * <pre>
 *    stencilManager.begin(StencilManager.STENCIL_1, shape.getBounds());
 *		fill(shape);
 *    stencilManager.end(); 
 * </pre>
 *
 * Then, to draw shapes and have them be clipped by the shape drawn above
 * do:
 *
 * <pre>
 *    stencilManager.enableClipping(StencilManager.STENCIL_1);
 *		fillShapes(otherShapes); 
 *    stencilManager.disableClipping(StencilManager.STENCIL_1);
 * </pre>
 *
 * You can define multiple shapes to clip against in different stencil
 * planes. The enableClipping method can take a bitmask
 * indicating which planes to clip against, so:
 *
 * <pre>
 *    stencilManager.enableClipping(StencilManager.STENCIL_1|StencilManager.STENCIL_2);
 * </pre>
 *
 * means clip against everything in plane 1 and 2.
 */
 
class StencilManager {
	GL gl;
	int clipMask; // which planes are currently being clipped against
        AgileState glState;
	
	static final int STENCIL_1 = 1;
	static final int STENCIL_2 = 2;
	static final int STENCIL_3 = 4;
	
	StencilManager(GL gl) {
		this.gl = gl;
                this.glState = AgileState.get(gl);
	}
		
	//		
	// Prepares a bitplane of the stencil buffer for writing. If bounds 
	// is null, that entire bitplane is cleared. Otherwise, only the
	// specified portion (in user coordinates) of the bitplane is cleared.
	//
	void begin(int maskBit, Rectangle2D bounds) {			
		// Disable the color and depth buffer
		gl.glColorMask(false, false, false, false);
		gl.glDepthMask(false);

		// Turn on the stencil
		glState.glEnable(GL.GL_STENCIL_TEST);

		gl.glStencilMask(maskBit);

		if (bounds == null) {
			gl.glClear(GL.GL_STENCIL_BUFFER_BIT);
		} else {
			// Clear the area of the stencil buffer under the shape - since this
			// is the only portion of the window we will draw to, we can ignore the
			// rest of the stencil buffer
			//
			gl.glStencilFunc(GL.GL_ALWAYS, maskBit, maskBit);
			gl.glStencilOp(GL.GL_ZERO, GL.GL_ZERO, GL.GL_ZERO);
			gl.glRectf((float)bounds.getMinX() - 1, (float)bounds.getMinY() - 1, 
						(float)bounds.getMaxX() + 1, (float)bounds.getMaxY() + 1);
		}
		
		// Turn on mode to draw into stencil buffer
		gl.glStencilFunc(GL.GL_ALWAYS, maskBit, maskBit);
		gl.glStencilOp(GL.GL_REPLACE, GL.GL_REPLACE, GL.GL_REPLACE);
	}

	//
	// Called after calling begin() - this closes the stencil buffer 
	// for writing and reactivates the normal depth/color buffers.
	//
	void end() {			
		// Now restore the color/depth buffer and disable stencil
		gl.glColorMask(true, true, true, true);
		gl.glDepthMask(true);
		checkClipMask();			
	}

	// After calling this, shapes are only drawn if all the bits 
	// specified in mask are set in the stencil buffer.
	// 
	void enableClipping(int mask) {		
		clipMask |= mask;
		checkClipMask();
	}

	void disableClipping(int mask) {		
		clipMask &= ~mask;
		checkClipMask();
	}

	private void checkClipMask() {
		if (clipMask != 0) {
			glState.glEnable(GL.GL_STENCIL_TEST);			
			gl.glStencilFunc(GL.GL_EQUAL, clipMask, clipMask);
			gl.glStencilOp(GL.GL_KEEP, GL.GL_KEEP, GL.GL_KEEP);
		} else {
			glState.glDisable(GL.GL_STENCIL_TEST);			
		}
	}
}
