/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JRootPane;


/**
 * Interface implemented by pluggable renderers. Each StereoJFrame has an
 * instance of a PluggableRenderer associated with it - the
 * PluggableRenderer is forwarded update, paint and repaint messages from
 * the StereoJFrame. It is responsible for rendering the contents of the
 * frame.
 */
public interface PluggableRenderer {
    /**
     * Returns the name of this renderer. Currently this is either "java2d"
     * (indicating the default renderer provided with Java) or "opengl" (the
     * Agile2D OpenGL Pluggable Renderer).
     *
     * @return the name of this renderer.
     */
    public String getName();
    public Graphics getGraphics();

    /**
     * Returns true if the renderer supports Hardware Stereo such as OpenGL
     * Quad-buffer stereo.
     *
     * @return true if the renderer supports Hardware Stereo such as OpenGL
     * Quad-buffer stereo.
     */
    public boolean isStereoHardwareAvailable();

    /**
     * Flush an image from video memory.
     * 
     * <p>
     * Pluggable renderers may cache the contents of an images using video
     * memory on the graphics card.  Unfortunately, because Java AWT
     * provides no notification mechanism for  discovering when an image's
     * contents have changed, renderers are unable to  detect changes to the
     * image, and hence the renderer's version of an image  can become out
     * of date.  Applications must call this method to force the current
     * renderer to discard  any copies of the image that cached in video
     * memory.
     * </p>
     */
    public void flush(Image image);

    /**
     * Discards any device data used by this renderer. This is called by the
     * Frame's dispose method.
     */
    public void dispose();

    /**
     * Called by the StereoJFrame associated with this renderer to inform the
     * renderer that the frame is about to change its root pane.
     */
    public void notifyBeforeSetRootPane(JRootPane pane);

    /**
     * Called by the StereoJFrame associated with this renderer to inform the
     * renderer that the frame has had its root pane set.
     */
    public void notifyAfterSetRootPane(JRootPane pane);

    /**
     * Called by the StereoJFrame associated with this renderer to inform the
     * renderer that repaint has been called.
     *
     * @return true if the StereoJFrame should forward the message to its
     *         superclass, or false if it should discontinue dispatching the
     *         method.
     */
    public boolean notifyRepaint(long tm, int x, int y, int width, int height);

    /**
     * Called by the StereoJFrame associated with this renderer to inform the
     * renderer that it is being painted.
     *
     * @return true if the StereoJFrame should forward the message to its
     *         superclass, or false if it should discontinue dispatching the
     *         method.
     */
    public boolean notifyPaint(Graphics g);

    /**
     * Called by the StereoJFrame associated with this renderer to inform the
     * renderer that is being updated.
     *
     * @return true if the StereoJFrame should forward the message to its
     *         superclass, or false if it should discontinue dispatching the
     *         method.
     */
    public boolean notifyUpdate(Graphics g);
    
    /**
     * @see java.awt.Component#setCursor(Cursor)
     * @param Cursor
     */
    public void setCursor(Cursor c);

    /**
     * Adds an event listener that is called for all events that are
     * dispatched to the frame by the underlying rendering surface.
     */
    public void addAWTEventListener(AWTEventListener evt);

    /**
     * Removes event listeners added by addAWTEventListener.
     */
    public void removeAWTEventListener(AWTEventListener evt);
    
    /**
     * Adds mouse motion listener that is called for all
     * mouse motion events that are dispatched to the frame by the 
     * underlying rendering surface.
     * @param MouseMotionListener
     */
    public void addMouseMotionListener(MouseMotionListener mml);
    /**
     * Removes event listeners added by addMouseMotionListener()
     * @param MouseMotionListener
     */
    public void removeMouseMotionListener(MouseMotionListener mml);
}
