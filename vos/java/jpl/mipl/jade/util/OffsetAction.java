package jpl.mipl.jade.util;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.event.ActionEvent;

import javax.media.jai.*;
import jpl.mipl.jade.*;

public class OffsetAction extends AbstractAction
{

    public OffsetAction()
    {
        super("Offset");
    }

    /**
     * Behavior when action is taken. 
     *     
     */
    public void actionPerformed(ActionEvent evt)
    {
	final JSlider offsetSlider = 
	    new JSlider(JSlider.VERTICAL, -100, 100, 0);
	offsetSlider.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		int off = offsetSlider.getValue();
		if (off != ((double[])(Jade._rescalePB.getObjectParameter
				       ("offsets")))[0]) {
		    System.out.println("changing offset to " + off);
		    Jade._rescalePB.setParameter("offsets", 
						 new double[] { off });
		    Jade._rescaleImage.setParameterBlock(Jade._rescalePB);
		    System.out.println("setPB done");
		}
	    }
	});
	//Add it to the Main Frame
	Jade._jadeFrame.getContentPane().add(offsetSlider, BorderLayout.EAST);
	Jade._jadeFrame.pack();
    }
}
