/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import java.awt.AWTEvent;
import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLCapabilitiesChooser;
import javax.media.opengl.GLDrawable;
import javax.media.opengl.GLEventListener;

import jpl.mipl.jade.jadis.StereoJFrame;
import jpl.mipl.jade.jadis.agile2d.AgileGraphics2D;
import jpl.mipl.jade.jadis.agile2d.AgileState;
import jpl.mipl.jade.jadis.jogl.JadisGLCapabilitiesChooser;

/**
 * Instances of JOGLCanvas are used by the JOGLPluggableRenderer to route paint,
 * mouse and keyboard events through to the Swing Frame's RootPane.
 */
class JOGLCanvas extends Container implements MouseListener,
		MouseMotionListener, MouseWheelListener, KeyListener {
	GLCanvas glcanvas;
	StereoJFrame frame;
	private JOGLPluggableRenderer pluggableRenderer;
	private AgileGraphics2D jgraphics;
	private Container rootPane; // paint requests sent here

	boolean isDisplayable = false;
	boolean isStereoHardwareAvailable = false;

	static final boolean DEBUG_REPAINT = false;

	static final long PROXY_EVENT_MASK = AWTEvent.FOCUS_EVENT_MASK
			| AWTEvent.KEY_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK
			| AWTEvent.MOUSE_MOTION_EVENT_MASK;

	private class EventHandler implements GLEventListener {
		/**
		 * @see javax.media.opengl.GLEventListener#init(GLDrawable)
		 */
		public void init(GLAutoDrawable drawable) {
			// drawable.setGL(new TraceGL(drawable.getGL(), System.err));
			// drawable.setGL(new DebugGL(drawable.getGL()));
			drawable.setAutoSwapBufferMode(false);
		}

		/**
		 * @see javax.media.opengl.GLEventListener#displayChanged(GLAutoDrawable,
		 *      boolean, boolean)
		 */
		public void displayChanged(GLAutoDrawable o, boolean x, boolean y) {

		}

		/**
		 * @see javax.media.opengl.GLEventListener#reshape(GLAutoDrawable, int,
		 *      int, int, int)
		 */
		public void reshape(GLAutoDrawable d, int x, int y, int w, int h) {

		}

		/**
		 * @see javax.media.opengl.GLEventListener#display(GLDrawable)
		 */
		public void display(GLAutoDrawable drawable) {
			isDisplayable = true;
			try {
				GL gl = drawable.getGL();
				AgileState glState = AgileState.get(gl);
				// Call the glClear to clear the background
				glState.glDisable(GL.GL_SCISSOR_TEST);
				glState.glDisable(GL.GL_DEPTH_TEST);
				gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

				// Restore all the Java2D Graphics defaults
				jgraphics.resetAll();

				// Call client paint routine
				rootPane.paint(jgraphics);

			} finally {

				// Tell pluggableRenderer we are finished painting
				// synchronized (ARepaintManager.class) {
				// ARepaintManager.inPaint = false;
				// Make sure that the Repaint Manager knows that I've been
				// repainted
				// GLPluggableRenderer.dirtyCanvases.remove(this);
				// }
			}
		}

		/**
		 * @see javax.media.opengl.GLEventListener#postDisplay(GLDrawable)
		 */
		public void postDisplay(GLAutoDrawable drawable) {
		}

		/**
		 * @see javax.media.opengl.GLEventListener#preDisplay(GLDrawable)
		 */
		public void preDisplay(GLAutoDrawable drawable) {
			// pluggableRenderer.checkRepaintManager();
		}

	}

	/**
	 * The graphics that is being returned is an instance of AgileGraphics2D The
	 * AffineTransform of this graphics object has been set to Identity first
	 * then translated in opposite direction of JOGLCanvas location.
	 */
	public Graphics getGraphics() {
		if (isDisplayable == false)
			return null;
		// if (this.isGLEnabled()) {
		// if (glcanvas.getContext().makeCurrent() !=
		// GLContext.CONTEXT_NOT_CURRENT) {
		Point pt = this.getLocation();
		java.awt.geom.AffineTransform tx = jgraphics.getTransform();
		tx.setToIdentity();
		tx.translate(-(this.getLocation().x), -(this.getLocation().y));
		jgraphics.setTransform(tx);
		jgraphics.setStereoMode(frame.getStereoMode());
		// glcanvas.getContext().release();
		return jgraphics;
		// }
		// else
		// return super.getGraphics();

	}

	/**
	 * Sets stereo mode on this canvases custom Graphics2D object if it supports
	 * stereo
	 * 
	 * @see java.awt.Container#repaint()
	 */
	public void repaint() {
		if ((jgraphics != null) && (frame != null))
			jgraphics.setStereoMode(frame.getStereoMode());
		this.glcanvas.repaint();
	}

	/**
	 * @see java.awt.Container#repaint(long, int, int, int, int)
	 */
	public void repaint(long tm, int x, int y, int width, int height) {
		if ((jgraphics != null) && (frame != null))
			jgraphics.setStereoMode(frame.getStereoMode());
		this.glcanvas.repaint();
	}
	
	/**
	 * @see java.awt.event.MouseListener#mouseClicked(MouseEvent)
	 */
	public void mouseClicked(MouseEvent e) {
		this.processMouseEvent(e);
	}
	
	/**
	 * @see java.awt.event.MouseListener#mouseEntered(MouseEvent)
	 */
	public void mouseEntered(MouseEvent e) {
		processMouseEvent(e);
	}

	/**
	 * @see java.awt.event.MouseListener#mouseExited(MouseEvent)
	 */
	public void mouseExited(MouseEvent e) {
		processMouseEvent(e);
	}

	/**
	 * @see java.awt.event.MouseListener#mousePressed(MouseEvent)
	 */
	public void mousePressed(MouseEvent e) {
		processMouseEvent(e);
	}
	/**
	 * @see java.awt.event.MouseListener#mouseReleased(MouseEvent)
	 */
	public void mouseReleased(MouseEvent e) {
		processMouseEvent(e);
	}

	/**
	 * @see java.awt.event.MouseMotionListener#mouseDragged(MouseEvent)
	 */
	public void mouseDragged(MouseEvent e) {
		processMouseMotionEvent(e);
	}
	/**
	 * @see java.awt.event.MouseMotionListener#mouseMovedMouseEvent)
	 */
	public void mouseMoved(MouseEvent e) {
		processMouseMotionEvent(e);
	}

	/**
	 * @see java.awt.event.MouseWheelListener#mouseWheelMoved(MouseWheelEvent)
	 */
	public void mouseWheelMoved(MouseWheelEvent e) {
		processMouseWheelEvent(e);
	}

	/**
	 * @see java.awt.event.KeyListener#keyPressed(KeyEvent)
	 */
	public void keyPressed(KeyEvent e) {
		pluggableRenderer.dispatchEvent(e);
	}
	
	/**
	 * @see java.awt.event.KeyListener#keyReleased(KeyEvent)
	 */
	public void keyReleased(KeyEvent e) {
		pluggableRenderer.dispatchEvent(e);
	}

	/**
	 * @see java.awt.event.KeyListener#keyTyped(KeyEvent)
	 */
	public void keyTyped(KeyEvent e) {
		pluggableRenderer.dispatchEvent(e);
	}

	/**
	 * Constructs and initializes member variable.  Most notably
	 * JOGL's GLCanvas and custom Graphics2D object.  Note that we
	 * pass custom GLCapabilitiesChooser that determines if GLCanvas
	 * is capable of supporting stereo.  In this case by stereo, we
	 * mean OpenGL Hardware stereo.
	 * @see jpl.mipl.jade.jadis.jogl.JadisGLCapabilitiesChooser
	 * @param pluggableRenderer
	 */
	public JOGLCanvas(JOGLPluggableRenderer pluggableRenderer) {

		GLCapabilities caps = new GLCapabilities();
		//
		GLCapabilitiesChooser capsChooser = new JadisGLCapabilitiesChooser();
		caps.setHardwareAccelerated(true);
		this.glcanvas = new GLCanvas(caps, capsChooser, null, null);
		// doesn't do anything on Linux but on Windows works as advertized
		// this.glcanvas.setIgnoreRepaint(true);

		// Once we created GLCanvas, query if OpenGL stereo is available
		GLCapabilities capsChosen = this.glcanvas.getChosenGLCapabilities();
		// Need to check for null since mac osX platform apparently doesn't
		// properly support this feature
		if (capsChosen != null)
			isStereoHardwareAvailable = capsChosen.getStereo();
		this.glcanvas.getContext().setSynchronized(true);
		this.pluggableRenderer = pluggableRenderer;
		this.glcanvas.addGLEventListener(new EventHandler());
		this.glcanvas.addMouseListener(this);
		this.glcanvas.addKeyListener(this);
		this.glcanvas.addMouseMotionListener(this);
		this.glcanvas.addMouseWheelListener(this);
		this.frame = pluggableRenderer.frame;
		this.jgraphics = new AgileGraphics2D(glcanvas);
		// this.setLayout(new java.awt.CardLayout());
		this.add(glcanvas, "Center");
		this.glcanvas.setVisible(true);
		// this.setSize(100,100);

		// For Gradient Paints we need a stencil plane
		// stencilBits = 1;

		// glcanvas.enableEvents(PROXY_EVENT_MASK);
		glcanvas.setBackground(Color.white);
	}

	/**
	 * Overrides parent's implementation to set the size
	 * on GLCanvas that this class menages
	 * @see java.awt.Container#setBounds(int, int, int, int)
	 */
	public void setBounds(int x, int y, int w, int h) {
		super.setBounds(x, y, w, h);
		try {
			this.glcanvas.setSize(w, h);
		} catch (Exception e) {
		}

	}

	/**
	 * Sets the rootPane property.  This method is called
	 * by {@link jpl.mipl.jade.jadis.agile2d.swing.JOGLPluggableRender}
	 * @param rootPane
	 */
	public void setRootPane(Container rootPane) {
		this.rootPane = rootPane;
	}

	/**
	 * Returns rootPane contaner
	 * @return
	 */
	public Container getRootPane() {
		return rootPane;
	}

	void damageAll() {
		repaint();
	}

	void damageRect(int x, int y, int w, int h) {
		repaint();
	}

	// Proxy Event Handling
	//
	protected void processFocusEvent(FocusEvent e) {
		pluggableRenderer.dispatchEvent(e);
	}

	public void processKeyEvent(KeyEvent e) {
		pluggableRenderer.dispatchEvent(e);
	}

	/**
	 * Processes the specified MouseWheelEvent by first 
	 * translating the event position to this component's
	 * origin and then dispatching it to associated
	 * pluggable renderer
	 */
	public void processMouseWheelEvent(MouseWheelEvent e) {
		Point p = getLocation();
		e.translatePoint(p.x, p.y);
		pluggableRenderer.dispatchEvent(e);
	}
	
	/**
	 * Processes the specified MouseEvent by first 
	 * translating the event position to this component's
	 * origin and then dispatching it to associated
	 * pluggable renderer
	 */
	public void processMouseEvent(MouseEvent e) {
		Point p = getLocation();
		e.translatePoint(p.x, p.y);
		pluggableRenderer.dispatchEvent(e);
	}
	
	/**
	 * Processes the specified MouseEvent by first 
	 * translating the event position to this component's
	 * origin and then dispatching it to associated
	 * pluggable renderer
	 */
	public void processMouseMotionEvent(MouseEvent e) {
		Point p = getLocation();
		e.translatePoint(p.x, p.y);
		pluggableRenderer.dispatchMouseMotionEvent(e);
	}
}
