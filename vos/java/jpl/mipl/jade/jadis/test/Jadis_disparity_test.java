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
/**
 * Test program
 *
 * This program displays one image in mono or two image as stereo pair
 * Mono or Left image is put in a custom viewport that allows to "scroll"
 * it independently from "right" image.  
 * To scroll both images: use scrollbars.  To scroll left image with respect
 * to right image use slider bars.
 * 
 * Requires J2SE runtime environment, JAI, JOGL and JadeDisplay
 *  * To run it:
 * % Jadis_disparity_test left_image right_image(optional)
 * Note that not all Look and Feels are fully supported, if encounter problems add
 * -Dswing.defaultlaf=javax.swing.plaf.metal.MetalLookAndFeel as a VM argument
 * on a command line.
 * Created on April 14, 2008, 10:52 AM
 * @author Oleg Pariser
 */
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;


import javax.media.jai.*;

import jpl.mipl.jade.*;
import jpl.mipl.jade.jadis.Jadis;
import jpl.mipl.jade.jadis.StereoJPanel;
import jpl.mipl.jade.jadis.StereoJFrame;

public class Jadis_disparity_test {
	
	JSlider _hdispSlider, _vdispSlider;
	JViewportImage _vp_disp_left;
    /** Creates a new instance */
    public Jadis_disparity_test(String argv[]) {
        super();
        
        PlanarImage image_left = null;
        PlanarImage _image_right = null;
        boolean _is_stereo_pair = false;
        
        // Load the images specified as command line args
        if (argv.length == 0) {
        	System.out.println("Usage: Jadis_disparity_test left_image right_image(optional)");
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
        // for this sample program only left image component is put in a viewport to control 
        // For the "real" program, most useful scenario likely would be both "left" and "right" image
        // display components put in "viewport within viewport"
        _vp_disp_left = new JViewportImage();
        _vp_disp_left.setView(jd_left);
        if (_is_stereo_pair) {
        	JadeDisplay jd_right = new JadeDisplay(_image_right);
        	stereoPanel.add(_vp_disp_left, "Left");
        	stereoPanel.add(jd_right, "Right");
        }
        else {
        	stereoPanel.add(jd_left);
        }
 
        
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
        
    	JFrame f3 = new JFrame("controls");
        f3.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        Container w3 = f3.getContentPane();
        w3.setLayout(new BoxLayout(w3, BoxLayout.Y_AXIS));
        
    	_hdispSlider = new JSlider(-700, 700, 0);
       	_vdispSlider = new JSlider(-500, 500, 0);
    	_hdispSlider.addChangeListener(new ChangeListener() {
    		public void stateChanged(ChangeEvent e) {
			int off_x = _hdispSlider.getValue();
			int off_y = _vdispSlider.getValue();
			    _vp_disp_left.setViewPosition(new Point(off_x, off_y));
			    //System.out.println("changing offset to " + off_x + " " + off_y);
		    }
		});
       	_vdispSlider.addChangeListener(new ChangeListener() {
    		public void stateChanged(ChangeEvent e) {
			int off_x = _hdispSlider.getValue();
			int off_y = _vdispSlider.getValue();
			    _vp_disp_left.setViewPosition(new Point(off_x, off_y));
			    //System.out.println("changing offset to " + off_x + " " + off_y);
		    }
		});
		w3.add(new JLabel("disparity"));
		w3.add(_hdispSlider);
		w3.add(_vdispSlider);
        
    	f3.pack();
    	f3.setVisible(true);
     }

    public static void main(String argv[]){
   
    	new Jadis_disparity_test(argv);
  
    }	
}

