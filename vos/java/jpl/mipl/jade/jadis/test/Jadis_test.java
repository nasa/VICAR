/**Test program for Jadis that utilizes JadeDisplay(s) for image display and management.
 * Requires J2SE run time environment, JOGL and JAI to be available.
 * This is the preferred way to work with images in Jadis.
 * To run it:
 * % Jadis_test left_image right_image(optional)
 * Note that not all Look and Feels are fully supported, if encounter problems add
 * -Dswing.defaultlaf=javax.swing.plaf.metal.MetalLookAndFeel as a VM argument
 * on a command line.
 * Created on April 10, 2008, 10:52 AM
 */

/**
 * Loads one or a pair(for stereo) of images 
 * @author Oleg Pariser  JPL/Caltech
 */
import javax.swing.*;
import java.awt.*;

import javax.media.jai.*;

import jpl.mipl.jade.*;
import jpl.mipl.jade.jadis.StereoJPanel;
import jpl.mipl.jade.jadis.StereoJFrame;
import jpl.mipl.jade.jadis.Jadis;

public class Jadis_test {

    /** Creates a new instance */
    public Jadis_test(String argv[]) {
        super();
        
        PlanarImage image_left = null;
        PlanarImage _image_right = null;
        boolean _is_stereo_pair = false;
        
        // Load the images specified as command line args
        if (argv.length == 0) {
        	System.out.println("Usage: Jadis_test left_image right_image(optional)");
            System.exit(0);
        }
        else {
        	image_left = JAI.create("fileload", argv[0]);            
        }
        if (argv.length > 1) {
        	_image_right = JAI.create("fileload", argv[1]);
        	
            _is_stereo_pair = true;
        }
    
        // Add left and right view to the panel that can display them simultaneously
        StereoJPanel stereoPanel = new StereoJPanel();
        
        JadeDisplay jd_left  = new JadeDisplay(image_left);
        if (_is_stereo_pair) {
        	JadeDisplay jd_right = new JadeDisplay(_image_right);
        	stereoPanel.add(jd_left, "Left");
        	stereoPanel.add(jd_right, "Right");
        }
        else {
        	stereoPanel.add(jd_left);
        }
 
        //Set different cursor
        //this is optional
        stereoPanel.setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
        
        // Manage the panel using Scrollpane
        // this is optional
        JViewportImage viewport = new JViewportImage();
        viewport.setView(stereoPanel);    
        JScrollPane sp = new JScrollPane();
        sp.setViewport(viewport);
        
        
        
        // create new custom JFrame capable of displaying stereo
        StereoJFrame myFrame = new StereoJFrame();
        //JFrame myFrame = new JFrame();
        myFrame.getContentPane().add(sp);
        // set the stereo mode, anaglyph is the default
        //myFrame.setStereoMode(Jadis.STEREO_ANAGLYPH);
        
        
        // disable double buffering
        RepaintManager currentManager = RepaintManager.currentManager(myFrame);
        currentManager.setDoubleBufferingEnabled(false);

        // set the size of the window and make it visible
        myFrame.getContentPane().setPreferredSize(new Dimension(500, 600));
        myFrame.pack();
        myFrame.setVisible(true);
     }

    public static void main(String argv[]){
   
    	new Jadis_test(argv);
  
    }	
}

