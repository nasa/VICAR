/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import jpl.mipl.jade.jadis.StereoJFrame;
import jpl.mipl.jade.jadis.agile2d.swing.JOGLPluggableRenderer;
import jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer;
import jpl.mipl.jade.jadis.agile2d.swing.PluggableRendererSpi;


/**
 * Implements the Service Provider Interface for the Agile2D OpenGL Renderer, providing
 * pluggable renderers that use the JOGL OpenGL pipeline.
 */
public class JOGLPluggableRendererProvider extends PluggableRendererSpi {

	/**
	 * Creates a new GLPluggableRenderer service provider. A public no-argument constructor
	 * is required by the PluggableRendererSpi.getProvider method.
	 */
	public JOGLPluggableRendererProvider() {}
	/**
	 * Returns a renderer to use for the specified frame.
	 */
    public PluggableRenderer createRenderer(StereoJFrame frame) {
        return new JOGLPluggableRenderer(frame);
    }
}
