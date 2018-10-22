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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.io.File;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSlider;
import javax.swing.JViewport;
import javax.swing.RepaintManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jpl.mipl.jade.JViewportImage;
import jpl.mipl.jade.JadeDisplay;
import jpl.mipl.jade.jadis.Jadis;
import jpl.mipl.jade.jadis.StereoJFrame;
import jpl.mipl.jade.jadis.StereoJPanel;

/**
 * <b>Purpose:</b>
 * Example code for stereo mouse cursor with JADIS components.
 * 
 * <PRE>
 * Copyright 2008, California Institute of Technology
 * ALL RIGHTS RESERVED.
 * U.S. Government Sponsorship acknowledged. 2008.
 * </PRE>
 *
 * <PRE>
 * ============================================================================
 * <B>Modification History: </B>
 * ----------------------
 *
 * <B>Date              Who              What</B>
 * ----------------------------------------------------------------------------
 * 04/03/2008        Nick             Initial Release
 * ============================================================================
 * </PRE>
 *
 * @author Nicholas Toole  (Nicholas.T.Toole@jpl.nasa.gov)
 * @version $Id: MouseDepthTest.java,v 1.2 2008/04/08 16:40:36 ntt Exp $
 *
 */
public class Jadis_disparity_3dCursor_test
{
    //Left, right image filepaths
    protected String leftImagePath, rightImagePath;
    
    //Left right images
    protected PlanarImage leftImage, rightImage;
    
    //Stereo frame (from JADIS)
    protected StereoJFrame mainFrame;
    
    //Control frame
    protected JFrame controlFrame;
    
    //Left image viewport, facilitates panning left image independently
    protected JViewportImage vpDispLeft;
    
    //The mouse-tracking overlays for left and right images.  Handles
    //painting of mouse cursor
    protected JadisPanelMouseTrackOverlay leftMouseTracker, rightMouseTracker;
    
    //The left,right JadeDisplays
    protected JadeDisplay leftDisplay, rightDisplay;
    
    //Listener that updates the position of the mouse trackers
    protected MouseLocationUpdater mouseUpdater;
    
    //Scroll pane containing jade displays
    JScrollPane scrollPane;
    
    //---------------------------------------------------------------------
    
    /**
     * Constructor
     * @param leftImage Left image filepath
     * @param rightImage Right image filepath
     */
    
    public Jadis_disparity_3dCursor_test(String leftImage, String rightImage)
    {
        File tempFile;
        tempFile = new File(leftImage);
        if (!tempFile.canRead())
            throw new IllegalArgumentException("Left image not found.");
        this.leftImagePath = tempFile.getAbsolutePath();
        
        //-----------------------------
        
        tempFile = new File(rightImage);
        if (!tempFile.canRead())
            throw new IllegalArgumentException("Right image not found.");
        this.rightImagePath = tempFile.getAbsolutePath();

        init();
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Loads images and initiates UI build
     */
    
    protected void init()
    {
        this.leftImage  = JAI.create("fileload", this.leftImagePath);
        this.rightImage = JAI.create("fileload", this.rightImagePath);
        
        buildUI();
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Builds main and control frames
     */
    
    protected void buildUI()
    {
        buildMainFrame();
        buildControlFrame();
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Builds the main frame that contains the stereo components for 
     * displaying left and right images in stereo.
     */
    
    protected void buildMainFrame()
    {

        //Create left display, then add to its own viewport.
        //This viewport will provide us the ability to move the 
        //left image independent of the right
        leftDisplay = new JadeDisplay(this.leftImage);
        vpDispLeft = new JViewportImage();
        vpDispLeft.setView(leftDisplay);
        

        rightDisplay = new JadeDisplay(this.rightImage);

        // Add left and right view to the panel that can display them
        // simultaneously
        StereoJPanel stereoPanel = new StereoJPanel();
        stereoPanel.add(vpDispLeft,   "Left");
        stereoPanel.add(rightDisplay, "Right");
        
        // Manage the panel using Scrollpane
        JViewportImage viewport = new JViewportImage();
        viewport.setView(stereoPanel);
        scrollPane = new JScrollPane();

        scrollPane.setViewport(viewport);
        
        
        //-------------------------

        // create extended JFrame capable of displaying stereo
        mainFrame = new StereoJFrame();
        mainFrame.setTitle("Stereo Mousing");
        this.mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        mainFrame.getContentPane().add(scrollPane);
        
        // set the stereo mode
        mainFrame.setStereoMode(Jadis.STEREO_ANAGLYPH);

        // disable double buffering
        RepaintManager currentManager = RepaintManager.currentManager(mainFrame);
        currentManager.setDoubleBufferingEnabled(false);

        // set the size of the window and make it visible
        mainFrame.getContentPane().setPreferredSize(new Dimension(200, 300));
        
        //-------------------------
        
        //Instatiate trackers and add as impl's of overlay painters        
        this.leftMouseTracker = new JadisPanelMouseTrackOverlay(leftDisplay);
        this.rightMouseTracker = new JadisPanelMouseTrackOverlay(rightDisplay);
        this.leftMouseTracker.setStereoEnabled(true);
        this.rightMouseTracker.setStereoEnabled(true);
        
        leftDisplay.addOverlayPainter(this.leftMouseTracker);
        rightDisplay.addOverlayPainter(this.rightMouseTracker);
        
        
        //mouse location updater unifies notification of mouse location
        //updates to the overlay painters
        mouseUpdater = new MouseLocationUpdater(leftMouseTracker, 
                                                rightMouseTracker);
        
        //display listener links mouse events to the updater
        DisplayMouseListener displayListener = new DisplayMouseListener(vpDispLeft,
                                                                        mouseUpdater);
        vpDispLeft.addMouseListener(displayListener);
        vpDispLeft.addMouseMotionListener(displayListener);
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Creates control frame for UI controlss
     */
    
    protected void buildControlFrame()
    {

        this.controlFrame = new JFrame("SM Controls");
        this.controlFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        Container w3 = this.controlFrame.getContentPane();
        w3.setLayout(new BoxLayout(w3, BoxLayout.Y_AXIS));
        
        //h and v sliders update the embedded left viewport for independent
        //panning        
        JSlider _hdispSlider = new JSlider(-100, 100, 0);
        JSlider _vdispSlider = new JSlider(-100, 100, 0);        
        ViewportSliderListener vpsListener = new ViewportSliderListener(
                              _vdispSlider, _hdispSlider, vpDispLeft);       
        _hdispSlider.addChangeListener(vpsListener);
        _vdispSlider.addChangeListener(vpsListener);

        //-------------------------
        
        //Depth slider updates the apparent depth of the stereo mouse
        //cursor.        
        JSlider _depthSlider = new JSlider(-100, 100, 0);        
        _depthSlider.addChangeListener(new MouseDisparitySlider(_depthSlider,
                                                                mouseUpdater));
                
        w3.add(new JLabel("Disparity"));
        w3.add(_hdispSlider);
        w3.add(_vdispSlider);
        w3.add(new JSeparator());
        w3.add(new JLabel("Depth"));
        w3.add(_depthSlider);
        
        
        //links disparity slider events to the mouse trackers
        MouseDisparitySliderToCursorWidth dispToWidth = 
            new MouseDisparitySliderToCursorWidth(_depthSlider,
                                                  leftMouseTracker,
                                                  rightMouseTracker);
        _depthSlider.addChangeListener(dispToWidth);
        
        //links mouse wheel events to slider to control depth "remotely"
        MouseWheelToSlider wheelToSlider = new MouseWheelToSlider(_depthSlider);
        vpDispLeft.addMouseWheelListener(wheelToSlider);
        
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Runs the example
     */
    
    public void run()
    {
        mainFrame.pack();
        mainFrame.setVisible(true);
        
        controlFrame.pack();
        controlFrame.setVisible(true);
    }
    
    //=====================================================================
    //== Helper/UI Listener Classes  ======================================
    //=====================================================================
    
    /**
     * Links mouse wheel events to the depth slider
     */    
    class MouseWheelToSlider implements MouseWheelListener 
    {
        JSlider slider;
        
        public MouseWheelToSlider(JSlider slider)
        {
            this.slider = slider;
        }
        
        public void mouseWheelMoved(MouseWheelEvent e) 
        {
            String message;
            int notches = e.getWheelRotation();
            if (notches < 0) 
            {
                slider.setValue(slider.getValue() - 1);
            } 
            else 
            {
                slider.setValue(slider.getValue() + 1);
            }                
         }
    }

    //=====================================================================
    
    /**
     * Links viewport slider events to the viewport position
     */
    class ViewportSliderListener implements ChangeListener
    {
        JSlider verticalSlider, horizontalSlider;
        JViewport viewport;
        
        public ViewportSliderListener(JSlider vertical, JSlider horizontal, 
                                                        JViewport viewport)
        {
            this.verticalSlider = vertical;
            this.horizontalSlider = horizontal;
            this.viewport = viewport;
        }
        
        public void stateChanged(ChangeEvent e)
        {
            if (e.getSource() != this.horizontalSlider && 
                                    e.getSource() != this.verticalSlider)
                return;
            int off_x = horizontalSlider.getValue();
            int off_y = verticalSlider.getValue();
            viewport.setViewPosition(new Point(off_x, off_y));
            //System.out.println("Changed viewport offset to " + off_x + " " + off_y);
        }
    }
    
    //=====================================================================
    
    /**
     * Links depth slider to mouse tracker updater
     */
    class MouseDisparitySlider implements ChangeListener
    {
        JSlider slider;
        MouseLocationUpdater updater;
        
        public MouseDisparitySlider(JSlider slider, MouseLocationUpdater updater)
        {
            this.slider  = slider;
            this.updater = updater;
        }
        
        public void stateChanged(ChangeEvent e)
        {
            if (e.getSource() != this.slider)
                return;
            
            int off_x = slider.getValue();
            
            updater.setHorizontalOffset(off_x);

        }
    }
    
    //=====================================================================
    
    /**
     * Quick way to use the depth slider to also control the "width"
     * of the cursor, making "close" cursors appear larger than "far"
     * ones.
     */
    
    class MouseDisparitySliderToCursorWidth implements ChangeListener
    {
        JSlider slider;
        JadisPanelMouseTrackOverlay left, right;
        int range;
        
        public MouseDisparitySliderToCursorWidth(JSlider slider, 
                                    JadisPanelMouseTrackOverlay left, 
                                    JadisPanelMouseTrackOverlay right)
        {
            this.slider  = slider;
            this.left= left;
            this.right = right;
            this.range = this.slider.getMaximum() - this.slider.getMinimum();
        }
        
        public void stateChanged(ChangeEvent e)
        {
            if (e.getSource() != this.slider)
                return;
            
            
            slider.getMaximum();
            int off_x = slider.getValue();
            double scale = Math.abs( ((double) off_x) / ((double) range));
            left.setCursorScale(scale);
            right.setCursorScale(scale);            
        }
    }
    
    //=====================================================================
    
    /**
     * Unifies the mouse location updates, taking into account the
     * horizontal offset of the right mouse cursor to that of the left.
     */
    class MouseLocationUpdater
    {
        JadisPanelMouseTrackOverlay left, right;
        int horizontalOffset = 0;
        Point position;
        
        public MouseLocationUpdater(JadisPanelMouseTrackOverlay left, 
                                    JadisPanelMouseTrackOverlay right)
        {
            this.left  = left;
            this.right = right;
            this.position = new Point();
        }
        
        public void setHorizontalOffset(int offset)
        {
            if (this.horizontalOffset != offset)
            {                
                this.horizontalOffset = offset;
                update();
            }
        }
        
        public void setMousePosition(int x, int y)
        {
            if (x != this.position.x || y != this.position.y)
            {
                position.setLocation(x,y);
                update();                
            }
        }
        
        protected void update()
        {
            this.left.setCursorCoordinates(position.x,
                                           position.y);
            this.right.setCursorCoordinates(position.x+horizontalOffset,
                                           position.y);
        }
    }
    
    
    //=====================================================================
    
    /**
     * Links mouse motion and action event to the mouse location updater.
     */
    class DisplayMouseListener extends MouseAdapter implements MouseMotionListener
    {

        // enabled, in this case, means not-locked or not-planted. As such
        // mouse motion should result in updates to coordinates when enabled,
        // and should be ignored with not enabled
        boolean _enabled = true;
        MouseLocationUpdater updater;
        JViewport viewport;
        
        //-----------------------------------------------------------------

        public DisplayMouseListener(JViewport viewport, MouseLocationUpdater updater)
        {
            this.viewport = viewport;
            this.updater = updater;
        }
        
        public void mouseExited(MouseEvent me)
        {
            if (this._enabled)
                updateCoordinates(JadisPanelCursorOverlay.NULL_COORDINATE,
                                  JadisPanelCursorOverlay.NULL_COORDINATE);
        }

        //-----------------------------------------------------------------

        public void mouseMoved(MouseEvent me)
        {
            // Get the position and translate it to Viewport coordinate
            int x = me.getX();
            int y = me.getY();

            redispatch(me);
            reactMouseMove(x, y);
        }

        protected void reactMouseMove(int x, int y)
        {
            Point vp = viewport.getViewPosition();
            x += vp.x;
            y += vp.y;

            if (this._enabled)
                updateCoordinates(x, y);
        }

        // -----------------------------------------------------------------

        protected void updateCoordinates(int x, int y)
        {
            updater.setMousePosition(x, y);
        }
        
        protected void togglePlanted()
        {
            this._enabled = !this._enabled;

            // ---------------------

            //setMousePlanted(!this._enabled);

        }

        protected void toggleAlwaysShow()
        {
            //_alwaysShowMouse = !_alwaysShowMouse;

            // ---------------------

            //showMouseCursorAlways(_alwaysShowMouse);
        }

        // -----------------------------------------------------------------

        /**
         * Enables planting of mouse cursors
         */
        
        public void mouseClicked(MouseEvent e)
        {
            redispatch(e);
            if (e.getButton() == MouseEvent.BUTTON1 && e.isControlDown()
                    && !e.isShiftDown())
            {
                togglePlanted();
                if (this._enabled)
                {
                    this.reactMouseMove(e.getX(), e.getY());
                }
            }
            else if (e.getButton() == MouseEvent.BUTTON1 && e.isControlDown())
            {
                toggleAlwaysShow();
            }
        }

        //-----------------------------------------------------------------

        public void mouseDragged(MouseEvent me)
        {
            redispatch(me);
        }

        //-----------------------------------------------------------------

        /**
         * Redispatch an event so it can propogate to a higher level in the
         * hierarchy (as well as being used at the lower level).  This allows
         * multiple listeners to both see the event (in this case, the mouse
         * follower and the mouse scroller).  Cribbed from an old JavaOne 
         * tutorial.
         */

        public void redispatch(MouseEvent e)
        {
            Point origin = e.getComponent().getLocation();
            e.translatePoint(origin.x, origin.y);
            e.getComponent().getParent().dispatchEvent(e);
            e.translatePoint(-origin.x, -origin.y);
        }

        //-----------------------------------------------------------------

        public void reset()
        {
            this._enabled = true;
            updateCoordinates(JadisPanelCursorOverlay.NULL_COORDINATE,
                              JadisPanelCursorOverlay.NULL_COORDINATE);
        }

        //-----------------------------------------------------------------
    }
    
    //=====================================================================
    //== Main method  =====================================================  
    //=====================================================================
    
    public static void main(String[] args)
    {
        if (args.length != 2)
        {
            System.err.println("Required: Left image path and right image path");
            System.exit(1);
        }
        
        Jadis_disparity_3dCursor_test test = null;
        
        try {
            test = new Jadis_disparity_3dCursor_test(args[0], args[1]);
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(1);
        }
        
        test.run();
    }
    
    //=====================================================================
}
