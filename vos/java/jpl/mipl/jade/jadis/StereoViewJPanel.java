package jpl.mipl.jade.jadis;

import javax.swing.*;

import java.awt.*;
import jpl.mipl.jade.jadis.agile2d.*;



/**
 *  StereoViewPanel is a container that is aware of special
 *  custom Graphics2D object.  All requests for graphics
 *  or paint requests will be handled using custom Graphics2D object.
 *  Overrides getGraphics() and paint() methods to control which 
 *  stereo view (Left/Right/Both) to use.
 *  Unlike JPanel, the default is to use BorderLayout.
 *  Generally, this class need not be instantiated by 
 *  users.  See {@link jpl.mipl.jade.jadis.StereoJPanel} which
 *  is intended to be user instantiated class and which 
 *  internally manages one or two StereoViewJPanel(s)
 *  
 *  @see jpl.mipl.jade.jadis.StereoJPanel
 */
public class StereoViewJPanel extends JPanel
{
	/**
	 * Holds the state of stereo mode
	 * defaults to Jadis.STEREO_BOTH
	 */
    private int _stereo_view = Jadis.STEREO_BOTH;
    
    /**
     * Creates a new <code>StereoViewPanel</code> using BorderLayout
     * Jadis users normally don't need to instantiate this class.
     * Instead users need to create an instance of StereoJPanel
     * {@link jpl.mipl.jade.jadis.StereoJPanel}
     * which will manage internally StereoViewJPanel(s)
     */
    public StereoViewJPanel() {
        super(new BorderLayout());        
    }
    /**
     * Sets up the view name for this component.
     * Valid values are "left" and "right".  
     * Everything else defaults to "both"
     * @param stereo_view
     */
    public void setStereoView(String stereo_view)
    {
    	if (stereo_view.equalsIgnoreCase("left"))
    	_stereo_view = Jadis.STEREO_LEFT;
    	else if (stereo_view.equalsIgnoreCase("right"))
        	_stereo_view = Jadis.STEREO_RIGHT;
    	else
    		_stereo_view = Jadis.STEREO_BOTH;
    }
  
    /**
     * Checks if the graphics object returned by the superclass
     * is an instance of custom stereo-capable Graphics2D object.
     * If it is, then sets the stereo_view mode on it and returns 
	 * this graphics object.
     */   
    public Graphics getGraphics()
    {
    	Graphics g = super.getGraphics();
    	if (g instanceof AgileGraphics2D) {
    		AgileGraphics2D jggl = (AgileGraphics2D) g;
    		jggl.setStereoView(_stereo_view);
    		return jggl;
    	}
    	return g;
    }
    /**
     * Checks if the graphics object passed in
     * is an instance of stereo capable custom Graphics2D object.  
     * If it is then set the stereo_mode on it and pass it upstream
     * to paint.
     */
    public void paint(Graphics g)
    {
    	if (g instanceof AgileGraphics2D) {
    		AgileGraphics2D jggl = (AgileGraphics2D) g;
    		jggl.setStereoView(_stereo_view);
    		super.paint(jggl);
    	}
    	else
    		super.paint(g);
    	  
    }
}

