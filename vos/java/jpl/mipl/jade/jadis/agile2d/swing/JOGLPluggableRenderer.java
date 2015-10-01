/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import javax.swing.*;

// Import standard Java libraries
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import jpl.mipl.jade.jadis.StereoJFrame;
import jpl.mipl.jade.jadis.agile2d.AgileGraphics2D;
import jpl.mipl.jade.jadis.agile2d.swing.JOGLCanvas;
import jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer;
import jpl.mipl.jade.jadis.agile2d.swing.StackLayout;

/**
 * An implementation of PluggableRenderer for rendering with Sun's OpenGL. This class
 * does the work needed to setup a custom JFrame to use OpenGL for rendering.
 */
class JOGLPluggableRenderer implements PluggableRenderer, MouseMotionListener {
	// Top-level custom JFrame
	StereoJFrame frame;
	// A GLCanvas that does the rendering
	JOGLCanvas backPane;
	ArrayList listeners = new ArrayList();
	ArrayList listeners_mm = new ArrayList();

	JOGLPluggableRenderer(StereoJFrame frame) {
		this.frame = frame;
		// we listen to mouse motion to properly set 
		// cursor
		addMouseMotionListener(this);
	}

	/**
	 * If underlying {@link JOGLCanvas} exists and is visible return it's
	 * graphics object. Otherwise return null.
	 * 
	 */
	public Graphics getGraphics() {
		if (backPane != null && backPane.isVisible()) {
			Graphics g = backPane.getGraphics();
			if (g != null && g instanceof AgileGraphics2D)
				((AgileGraphics2D) g).setStereoMode(frame.getStereoMode());
			return g;
		}
		return null;
	}

	/**
	 * Return the name of this pluggable renderer
	 * 
	 * @return "opengl"
	 */
	public String getName() {
		return "opengl";
	}

	/**
	 * Does nothing.
	 * @see PluggableRenderer#flush(Image)
	 * 
	 * @param image
	 */
	public void flush(Image image) {
	}

	/**
	 * Does nothing.
	 * @see PluggableRenderer#dispose()
	 */
	public void dispose() {
		// backPane.cvsDispose();
	}

	/**
	 * Determines if Hardware Stereo is available
	 * by asking underlying canvas.
	 */
	public boolean isStereoHardwareAvailable() {
		return backPane.isStereoHardwareAvailable;
	}

	//
	// This requires some hackery. The JOGL GLCanvas is not a subclass of
	// Container so it
	// cannot have children. Instead, we add a GLCanvas AND a JRootPane to the
	// frame,
	// reroute repaint requests from the root pane of this frame to the canvas.
	// Also, we set the
	// "rootPane" field of the canvas to the root pane of this frame, which
	// tells the canvas to forward
	// paint and event requests to the root pane.
	//
	public void notifyBeforeSetRootPane(JRootPane root) {
		if (backPane == null) {
			frame.setLayout(new StackLayout());
			frame.add(backPane = new JOGLCanvas(this), "canvas", 0);
		}
	}

	/**
	 * Calls setRootPane() on underlying canvas.  Then
	 * makes param JRootPane visible.
	 * 
	 * @param root
	 *   
	 */
	public void notifyAfterSetRootPane(JRootPane root) {
		backPane.setRootPane(root);

		if (root != null) {
			root.setDoubleBuffered(false);
			root.setVisible(true);
		}
	}

	/**
	 * @see PluggableRenderer#notifyRepaint(long, int, int, int, int)
	 */
	public boolean notifyRepaint(long tm, int x, int y, int width, int height) {
		if (backPane != null) {
			backPane.repaint();
		}
		return false; // Don't send up to the StereoJFrame's superclass
	}

	/**
	 * Ignores paints , the canvas handles these
	 * @see PluggableRenderer#notifyPaint(Graphics)
	 * @return False, always
	 */
	public boolean notifyPaint(Graphics g) {
		// Ignore paints - the canvas handles these
		return false; // Don't send up to the StereoJFrame's superclass
	}

	/**
	 * Ignores updates, the canvas handles these
	 * 
	 * @return False, always
	 */
	public boolean notifyUpdate(Graphics g) {
		// Ignore updates - the canvas handles these
		return false; // Don't send up to the StereoJFrame's superclass
	}

	/** Sets cursor on underlying backpane.
	 * @see jpl.mipl.jade.jadis.agile2d.PluggableRenderer#setCursor(Cursor)
	 */
	public void setCursor(Cursor c) {
		backPane.setCursor(c);
	}
	
	/**
	 * @see PluggableRenderer#addAWTEventListener(AWTEventListener)
	 *    
	 */
	public void addAWTEventListener(AWTEventListener l) {
		listeners.add(l);
	}

	/**
	 * @see PluggableRenderer#removeAWTEventListener(AWTEventListener)
	 */
	public void removeAWTEventListener(AWTEventListener l) {
		listeners.remove(l);
	}
	
	/**
	 * @see PluggableRenderer#addMouseMotionListener(MouseMotionListener)
	 */
	public void addMouseMotionListener(MouseMotionListener mml) {
		listeners_mm.add(mml);
	}
	/**
	 * @see PluggableRenderer#removeMouseMotionListener(MouseMotionListener)
	 */
	public void removeMouseMotionListener(MouseMotionListener mml) {
		listeners_mm.remove(mml);
	}
	
	void dispatchEvent(AWTEvent e) {
		
		for (int i = 0; i < listeners.size(); i++) {
			((AWTEventListener) listeners.get(i)).eventDispatched(e);
		}
		frame.dispatchEvent(e);
		
	}

	void dispatchMouseMotionEvent(MouseEvent e) {
		for (int i = 0; i < listeners_mm.size(); i++) {
			((MouseMotionListener)listeners_mm.get(i)).mouseMoved(e);
		}
		frame.dispatchEvent(e);
	}
	
	   /**
     * Does nothing.  Needed only to complete
     * MouseMotionListener implementation
     */
	public void mouseDragged(MouseEvent arg0) {		
	}
	/**
	 * Captures mouse movement events to set the appropriate cursor
	 * for associated StereoJFrame's underlying components.
	 */
	public void mouseMoved(MouseEvent e) {
		// Our frame has two children: the top-most is a Canvas 
		// that renders everything.  The second child
		// is JRootPane container.  We want to set cursor on a Canvas but
		// with a Cursor object gotten from JRootPane's child.
		// see comments to this.notifyBeforeSetRootPane(JRootPane root)
		// above for more info.
		// 
		Component comp = ((Container)frame.getComponent(1)).findComponentAt(e.getX(), e.getY());
		if (comp != null)
			setCursor(comp.getCursor());
	}
}
