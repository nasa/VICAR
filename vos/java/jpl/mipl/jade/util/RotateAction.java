package jpl.mipl.jade.util;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.ActionEvent;

import javax.media.jai.*;
import jpl.mipl.jade.*;

public class RotateAction extends AbstractAction
{

    /**
     * Default constructor of singleton class.
    */
    public RotateAction()
    {
	super("Rotate");
    }

    /**
     * Behavior when action is taken. 
     *     
     */
    public void actionPerformed(ActionEvent evt)
    {
	final JSlider rotateSlider = 
	    new JSlider(JSlider.VERTICAL, -100, 100, 0);
	rotateSlider.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		float angle = 
		    (float)rotateSlider.getValue() * (float)Math.PI / 180.0f;
		if (angle !=((Float)(Jade._rotPB.getObjectParameter("angle")))
		    .floatValue()) {
		    System.out.println("changing rotation to " + angle);
		    Jade._rotPB.setParameter("angle", angle);
		    Jade._rotImage.setParameterBlock(Jade._rotPB);
		}
	    }
	});

	//Add it to the Main Frame
	Jade._jadeFrame.getContentPane().add(rotateSlider, BorderLayout.WEST);
	Jade._jadeFrame.pack();

    }
}
