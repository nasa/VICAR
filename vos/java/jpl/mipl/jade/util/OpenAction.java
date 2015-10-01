package jpl.mipl.jade.util;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;
import javax.swing.*;
import java.io.File;
import java.util.*;
import java.awt.event.ActionEvent;

import javax.media.jai.*;
import java.awt.image.renderable.ParameterBlock;

import jpl.mipl.jade.*;

public class OpenAction extends AbstractAction 
    implements PropertyChangeListener
{
    private JadeFileChooser _jadeFileChooser;
    /**
     * Default constructor of singleton class.
    */
    public OpenAction()
    {
	super("Open...", new ImageIcon("open.gif"));
	_jadeFileChooser = 
	    new JadeFileChooser(new File(System.getProperty("user.dir")), 
				false);
	_jadeFileChooser.addPropertyChangeListener(this);
	
    }

    /**
     * Behaviour when action is taken.  Opens a file chooser to
     * select any file or directory and initiates processing to
     * open/load the selected object.
    */
    public void actionPerformed(ActionEvent evt)
    {
	_jadeFileChooser.pack();
	_jadeFileChooser.setVisible(true);
    }

    public void propertyChange(PropertyChangeEvent evt)
    {
	if(evt.getPropertyName().equalsIgnoreCase("ReturnValue"))
	{
	    int returnValue = 
		(new Integer(evt.getNewValue().toString())).intValue();
	    
	    if(returnValue == JadeFileChooser.OK_OPTION)
	    {
		if(_jadeFileChooser.getImageLeftCount() == 0)
		    return;
		else if(_jadeFileChooser.getImageLeftCount() == 1)
		{
		    String imageName = 
			(_jadeFileChooser.getImageLeft())[0].toString();

		    Jade._loadImage = JAI.create("fileLoad", imageName);
		    Jade._rotImage.setSource(Jade._loadImage, 0);

		    
		}		
		else if(_jadeFileChooser.getImageLeftCount() == 3)
		{	    
		    String imageName1 = 
			(_jadeFileChooser.getImageLeft())[0].toString();
		    String imageName2 = 
			(_jadeFileChooser.getImageLeft())[1].toString();
		    String imageName3 = 
			(_jadeFileChooser.getImageLeft())[2].toString();

		    ParameterBlock pb = new ParameterBlock();
		    pb.addSource(JAI.create("fileLoad", imageName1));
		    pb.addSource(JAI.create("fileLoad", imageName2));
		    pb.addSource(JAI.create("fileLoad", imageName3));

		    Jade._loadImage = JAI.create("bandMerge", pb);
		    Jade._rotImage.setSource(Jade._loadImage, 0);
		    
		}
		
	    }
	    
	}
    }
}
