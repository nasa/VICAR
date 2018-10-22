/*
 * Simplest test program for Jadis.
 * Only requires J2SE run time environment and JOGL to be available.
 * To run it:
 * % Jadis_simple_test left_image right_image(optional)
 * Note that not all Look and Feels are fully supported, if encounter problems add
 * -Dswing.defaultlaf=javax.swing.plaf.metal.MetalLookAndFeel as a VM argument
 * on a command line.
 * Also note that the mechanism used in this test program to load images via ImageIcon
 * added to JLabel is NOT the preferred way to work with images.  It is only used here
 * to avoid dependencies on JAI which is not part of J2SE and/or JadeDisplay.
 * Please see other test program that does utilize JAI and JadeDisplay for the preferred way
 * to load and display images.
 *
 * Created on April 10, 2008, 10:52 AM
 */

/**
 * Loads one or a pair(for stereo) of images 
 * @author Oleg Pariser  JPL/Caltech
 */
import javax.swing.*;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import jpl.mipl.jade.jadis.StereoJPanel;
import jpl.mipl.jade.jadis.StereoJFrame;

public class Jadis_simple_test {
	   
    public Jadis_simple_test(String argv[]) throws IOException {
    	super();
    	
    	BufferedImage image_left = null;
    	BufferedImage image_right = null;
    	ImageIcon image_icon_left = null;
    	ImageIcon image_icon_right = null;
    	JLabel image_left_comp = null;
    	JLabel image_right_comp = null;
        boolean is_stereo_pair = false;    	
 
        
        // Load the images specified as command line args
        if (argv.length == 0) {
        	System.out.println("Usage: Jadis_simple_test left_image right_image(optional)");
            System.exit(0);
        }
        else {
        	image_left = ImageIO.read(new File(argv[0]));
            image_icon_left = new ImageIcon(image_left);
            image_left_comp = new JLabel(image_icon_left);
        }
        if (argv.length > 1) {
        	image_right = ImageIO.read(new File(argv[1]));
            image_icon_right = new ImageIcon(image_right);
            image_right_comp = new JLabel(image_icon_right);
            is_stereo_pair = true;
        }
                
        // Add left and right view to the panel that can display both views simultaneously      
        StereoJPanel stereoPanel = new StereoJPanel();
        if (is_stereo_pair) {
        	stereoPanel.add(image_left_comp, "Left");
        	stereoPanel.add(image_right_comp, "Right");
        }
        else {//mono image, image will be added to both:left and right view
        	stereoPanel.add(image_left_comp);
        }
        
        // Optional: Manage the panel using Scrollpane
        // If not desired, add stereoPanel directly to the StereoJFrame
        JViewport viewport = new JViewport();
        viewport.setView(stereoPanel);    
        JScrollPane sp = new JScrollPane();
        sp.setViewport(viewport);
        
        
        
        // create new custom JFrame capable of displaying stereo
        StereoJFrame myFrame = new StereoJFrame();
        myFrame.getContentPane().add(sp);
        // set the stereo mode to hardware stereo
        // will work only if underlying hardware supports it
        // otherwise, by default, stereo mode is Jadis.STEREO_ANAGLYPH
        //myFrame.setStereoMode(Jadis.STEREO_GL);

        // set the size of the window and make it visible
        myFrame.getContentPane().setPreferredSize(new Dimension(500, 600));
        myFrame.pack();
        myFrame.setVisible(true);
 
    }

    public static void main(String argv[]){
   
    	try {
			new Jadis_simple_test(argv);
		} catch (IOException e) {
			e.printStackTrace();
		}
  
    }	
}

