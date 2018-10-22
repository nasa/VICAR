// License Terms
// 
// Copyright (c) 2008, California Institute of Technology ("Caltech").
// U.S. Government sponsorship acknowledged.
// 
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// * Neither the name of Caltech nor its operating division, the Jet Propulsion
// Laboratory, nor the names of its contributors may be used to endorse or
// promote products derived from this software without specific prior written
// permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER  OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
// 
/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import jpl.mipl.jade.jadis.agile2d.AgileGraphics2D;
import jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer;
import jpl.mipl.jade.jadis.agile2d.swing.PluggableRendererSpi;




/**
 * Top-level container that is capable of displaying swing components in stereo.  
 * Intended to be used in place of Swing's {@link javax.swing.JFrame}.<br/>
 * <p>
 * This is an extension of {@link javax.swing.JFrame} based on Agile2D's AgileJFrame.  Major
 * change is an addition of support for stereo display.  Also overrides JFrame's
 * getGraphics() to return custom Graphics2D object(if available) that supports
 * drawing in stereo. 
 * </p>
 * 
 * <p>
 * This implementation of JFrame uses a "pluggable renderer" mechanism to
 * manage rendering the contents of the frame.
 * </p>
 * 
 * <p>
 * Each StereoJFrame has a PluggableRenderer associated with it. The
 * PluggableRenderer is responsible for rendering performed in the frame. It
 * also  provides additional methods for controlling the renderer at a high
 * level.
 * </p>
 * 
 * <p>
 * Currently, the only implementation that is stereo-capable is JOGL OpenGL renderer.
 * </p>
 * 
 * <p>
 * Normal {@link javax.swing.JPanel} or other Swing containers can be used within this component; 
 * they will be rendered to both eyes.  At the point where stereo UI is desired, 
 * use a {@link jpl.mipl.jade.jadis.StereoJPanel}.
 * <p/>
 * <p>
 * Usage:<br/>
 * <br/>
 *   StereoJFrame stereoJFrame = new StereoJFrame();<br/>
 *   // Note that StereoJFrame can manage any component as content pane's<br/>
 *   // child but to display components in stereo as "left" or "right", user has<br/>
 *   // to use {@link jpl.mipl.jade.jadis.StereoJPanel} as content pane's child<br/>
 *   StereoJPanel stereoJPanel = new StereoJPanel();
 *   stereoJPanel.add(swing_component_1, "left");
 *   stereoJPanel.add(swing_component_2, "right");
 *   stereoJFrame.getContentsPane().add(stereoJPanel);<br/>
 *   //see {@link jpl.mipl.jade.jadis.Jadis} for description of supported  stereo modes <br/>
 *   stereoJFrame.setStereoMode(Jadis.STEREO_ANAGLYPH); <br/>
 *   stereoJFrame.setVisible(true); <br/>
 * </p>
 * <p>
 * When {@link jpl.mipl.jade.jadis.StereoJPanel} is used, for certain Swing operations 
 * like setting cursor, it's not advisable to do it on "left" or "right" component.  
 * Instead do it on it's parent,  <code>StereoJPanel</code> and "left" and "right" children 
 * will inherit this attribute.  Otherwise, if user, for example would setCursor on "right"
 * component, then "left" component wouldn't "know" about it and use a default cursor or whatever
 * cursor was set on it.  Then it will be up to the Swing to determine which component, "left" or
 * "right" owns a given mouse event, and that decision will determine which cursor to use.  So
 * to avoid the risk of being "ignored" by Swing, set attributes that has to be shared by "left"
 * "right" components on it's parent {@link jpl.mipl.jade.jadis.StereoJPanel}  If, by some reason,
 * setting cursor(and other resources) on "left" and "right" parent, <code>StereoJPanel</code> is
 * not feasable, make sure set/unset cursor(and other resources) on BOTH "left" and "right" component.
 * 
 * </p>
 *
 * @see jpl.mipl.jade.jadis.StereoJPanel
 * @see jpl.mipl.jade.jadis.Jadis
 * @see jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer
 */
public class StereoJFrame extends JFrame {
	
    private PluggableRenderer pluggableRenderer;
    private int _stereoMode = Jadis.STEREO_ANAGLYPH;

    /**
     * Store stereoMode in member variable for later use.
     *
     * @param stereoMode switches the mode only for known values
     * @return false if input mode is unrecognized, otherwise
     * set stereo mode and return true
     */
    public boolean setStereoMode(int stereoMode){
        // make sure that only supported mode is assigned
        if (stereoMode == Jadis.STEREO_ANAGLYPH)
                _stereoMode = Jadis.STEREO_ANAGLYPH;
        else if (stereoMode == Jadis.STEREO_GL)
                _stereoMode = Jadis.STEREO_GL;
        else if (stereoMode == Jadis.STEREO_BOTH)
                _stereoMode = Jadis.STEREO_BOTH;
        else // unrecognized mode
                return false;
        return true;
    }
    /**
    *
    * @return the current stereo mode
    */
   public int getStereoMode() {
       return _stereoMode;
   }
   /** Checks if pluggable renderer has a valid graphics.
    * If that graphics object is custom Graphics2D class
    * that supports stereo, set stereo mode to the current one
    * and return this custom graphics object.
    * Returns the graphics object of pluggableRenderer
    */

   public Graphics getGraphics() {
       if((pluggableRenderer != null) && (pluggableRenderer.getGraphics() != null)) {
               // check for null before create
               Graphics g = pluggableRenderer.getGraphics().create();
                       if (g instanceof AgileGraphics2D) {
                               ((AgileGraphics2D)g).setStereoMode(_stereoMode);
                               return g;
                       }
               }
       return super.getGraphics();
   }

    /**
     * Creates a new StereoJFrame object.
     */
    public StereoJFrame() {
    	super();
    }

    /**
     * Create a new StereoJFrame object.
     */
    public StereoJFrame(String name) {
        super(name);
    }

    /**
     * Create a new StereoJFrame object.
     */
    public StereoJFrame(java.awt.GraphicsConfiguration config) {
        super(config);
    }

    /**
     * Create a new StereoJFrame object.
     *
     */
    public StereoJFrame(String name, java.awt.GraphicsConfiguration config) {
        super(name, config);
    }

    /**
     * Get pluggable renderer
     * @return pluggableRenderer
     * @see jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer
     */
    public PluggableRenderer getPluggableRenderer() {
        return pluggableRenderer;
    }

    /**
     * Get name of pluggable renderer
     * @return name of pluggable renderer
     * @see jpl.mipl.jade.jadis.agile2d.swing.PluggableRenderer
     */
    public String getPluggableRendererName() {
        return pluggableRenderer.getName();
    }

    /**
     * Creates pluggable renderer
     * @see jpl.mipl.jade.jadis.agile2d.PluggableRenderer
     */
    private void init() {
        if (pluggableRenderer == null)
            pluggableRenderer = PluggableRendererSpi.getActiveProvider()
                                                    .createRenderer(this);
    }

    /**
     * Checks if underlying hardware supports Hardware Stereo
     * Currently that means Shutter Glasses stereo viewing
     * using graphics card that is capable to send stereo sync 
     * signal out.
     * @return true if available 
     */
    public boolean isStereoHardwareAvailable() {
    	return pluggableRenderer.isStereoHardwareAvailable();
    }
  
    /**
     * Adds an event listener that is called for all events that are
     * dispatched to the frame by the underlying rendering surface.
     *
     * @param evt the AWTEventListener.
     */
    public void addAWTEventListener(AWTEventListener evt) {
        pluggableRenderer.addAWTEventListener(evt);
    }

    /**
     * Removes event listeners added by addAWTEventListener.
     *
     * @param evt the AWTEventListener.
     */
    public void removeAWTEventListener(AWTEventListener evt) {
        pluggableRenderer.removeAWTEventListener(evt);
    }
    
    protected void setRootPane(JRootPane root) {
        if (pluggableRenderer == null) {
            init();
        }

        boolean c = isRootPaneCheckingEnabled();

        try {
            setRootPaneCheckingEnabled(false);
            pluggableRenderer.notifyBeforeSetRootPane(root);
        } finally {
            setRootPaneCheckingEnabled(c);
        }

        super.setRootPane(root);

        pluggableRenderer.notifyAfterSetRootPane(root);
    }

    /**
     * Invokes dispose() on pluggable renderer and then
     * invokes superclass implementation
     */
    public void dispose() {
        if (pluggableRenderer != null)
            pluggableRenderer.dispose();
        super.dispose();
    }

    /**
     * Repaints the specified rectangle of this component within
     * tm milliseconds.
     */
    public void repaint(long tm, int x, int y, int width, int height) {
        if (pluggableRenderer == null ||
                pluggableRenderer.notifyRepaint(tm, x, y, width, height))
            super.repaint(tm, x, y, width, height);
    }

    /**
     * Calls superclass implementation if pluggable renderer
     * is null or notifyPaint returns false.  Otherwise
     * ignores paint request.
     * @see java.awt.Container#paint(Graphics)
     */
    public void paint(Graphics g) {
        if (pluggableRenderer == null || pluggableRenderer.notifyPaint(g))
            super.paint(g);
    }

    /**
     * Calls superclass implementation if pluggable renderer
     * is null or notifyUpdate returns false.  Otherwise ignores
     * update request.
     * @see java.awt.Container#update(Graphics)
     */
    public void update(Graphics g) {
        if (pluggableRenderer == null || pluggableRenderer.notifyUpdate(g))
            super.update(g);
    }
    
    /**
     * Calls superclass and if pluggable renderer exists,
     * calls setCursor on it as well.
     * @see java.awt.Window#setCursor(Cursor)
     */
    public void setCursor(Cursor cursor) {
        super.setCursor(cursor);
        if (pluggableRenderer != null)
            pluggableRenderer.setCursor(cursor);
    }
}
