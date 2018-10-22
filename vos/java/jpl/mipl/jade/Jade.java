package jpl.mipl.jade;

import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.beans.*;
import java.io.File;
import javax.swing.*;
import javax.swing.border.*;
import javax.media.jai.*;

import javax.media.jai.*;
import com.sun.media.jai.codec.ImageCodec;
import java.awt.image.renderable.ParameterBlock;
import java.awt.event.ComponentListener;
import java.awt.image.RenderedImage;

import jpl.mipl.jade.util.*;

public class Jade extends JFrame {

    public static JadeDisplay _jadeDisplay = null;
    public static JScrollPane _scrollPane = null;
    public static ParameterBlockJAI _loadPB, _rescalePB, _rotPB;
    public static RenderedOp _loadImage, _rescaleImage, _rotImage;
    public static RenderedOp _finalImage;
    public static Jade _jadeFrame;

    public Jade(String imageName)
    {	
	super("JADE");

	_jadeFrame = this;
	JToolBar toolBar = new JToolBar();
       	JButton  bOpen = new SmallButton(new OpenAction(), "Open Image File");
	toolBar.add(bOpen);

	final JMenuBar menuBar = new JMenuBar();

	
	//add Individual menus to the menuBar
	menuBar.add(this.createFileMenu());
	menuBar.add(this.createImageMenu());
	menuBar.add(this.createHelpMenu());

	//getContentPane().add(toolBar, BorderLayout.NORTH);
	getRootPane().setJMenuBar(menuBar);

	// Create input image by reading args[0]

       	
	if(imageName != null)
	{
	   
	    _loadPB = new ParameterBlockJAI("fileload");
	    _loadPB.setParameter("filename",imageName );
	    _loadImage = JAI.create("fileload", _loadPB);
	    System.out.println(_loadImage);
	    
	    _rotPB = new ParameterBlockJAI("rotate");
	    _rotPB.setSource(_loadImage, 0);
	    _rotPB.setParameter("angle", 0.0f);
	    _rotImage = JAI.create("rotate", _rotPB);

	    _rescalePB = new ParameterBlockJAI("rescale");
	    _rescalePB.setSource(_rotImage, 0);
	    _rescalePB.setParameter("constants", new double[] { 1.0 });
	    _rescalePB.setParameter("offsets", new double[] { 0.0 });
	    _rescaleImage = JAI.create("rescale", _rescalePB);

	    _finalImage = _rescaleImage;
	    _scrollPane = 
		JadeDisplay.createDisplay(_finalImage, 300, 300, true);
	    getContentPane().add(_scrollPane, BorderLayout.CENTER);
	
	    _jadeDisplay = (JadeDisplay)_scrollPane.getViewport().getView();
	    _jadeDisplay.setRepaintPolicy(JadeDisplay.REPAINT_CACHE);
	} 
    }

    private JMenu createFileMenu()
    {
	JMenu fileMenu = new JMenu("File");
	fileMenu.setMnemonic('f');
	
	JMenuItem itemOpen = fileMenu.add(new OpenAction());
	fileMenu.add(itemOpen);
	itemOpen.setMnemonic('o');

	JMenuItem itemExit = fileMenu.add(new ExitAction());
	fileMenu.add(itemExit);
	itemExit.setMnemonic('x');

	return fileMenu;
    }

    private JMenu createImageMenu()
    {
	JMenu imageMenu = new JMenu("Image");
	imageMenu.setMnemonic('i');

			
	JMenuItem itemRotate = imageMenu.add(new RotateAction());
	imageMenu.add(itemRotate);
	itemRotate.setMnemonic('r');

	
	JMenuItem itemOffset = imageMenu.add(new OffsetAction());

	imageMenu.add(itemOffset);
	itemOffset.setMnemonic('o');

	return imageMenu;
    }

    private JMenu createHelpMenu()
    {
	JMenu helpMenu = new JMenu("Help");
	helpMenu.setMnemonic('h');
	
	JMenuItem itemAbout = helpMenu.add(new AboutAction());
	helpMenu.add(itemAbout);
	itemAbout.setMnemonic('b');
	
	return helpMenu;
    }

    public static void main(String args[]) {
	if(args.length == 1) {
	    JFrame frame = new Jade(args[0]);
	    frame.setBounds(300, 300, 400, 400);
	    frame.setVisible(true);
	}
	else {
	    System.out.println("Usage: java Jade <image>");
	    System.exit(0);
	}


    }
}


class ExitAction extends AbstractAction {
    
    public ExitAction() 
    {
	super("Exit");
    }
    public void actionPerformed(ActionEvent event) {
	System.exit(0);
    }
}

class AboutAction extends AbstractAction {
    
    public AboutAction() 
    {
	super("About...");
    }
    public void actionPerformed(ActionEvent event) {
	JDialog aboutDialog = new JDialog();
	JLabel lbl = new JLabel(new ImageIcon("dukeWave.gif"));
	aboutDialog.getContentPane().add(lbl);
	aboutDialog.setSize(400, 200);
	aboutDialog.setLocation(250, 400);
	aboutDialog.setVisible(true);
    }
}

class SmallButton extends JButton implements MouseListener
{
    private Border _raised;
    private Border _lowered;
    private Border _inactive;

    public SmallButton(Action action, String tip) 
    {
	super((Icon)action.getValue(Action.SMALL_ICON));
	_raised = new BevelBorder(BevelBorder.RAISED);
	_lowered = new BevelBorder(BevelBorder.LOWERED);
	_inactive = new EmptyBorder(2, 2, 2, 2);
	setBorder(_inactive);
	setMargin(new Insets(1, 1, 1, 1));
	setToolTipText(tip);
	addActionListener(action);
	addMouseListener(this);
	setRequestFocusEnabled(false);
    }
    
    public float getAlignmentY()
    {
	return .5f;
    }

    public void mousePressed(MouseEvent e)
    {
	setBorder(_lowered);
    }
    
    public void mouseReleased(MouseEvent e)
    {
	setBorder(_inactive);
    }
    public void mouseClicked(MouseEvent e)
    {
	
    }
    public void mouseEntered(MouseEvent e)
    {
	setBorder(_raised);
    }
    public void mouseExited(MouseEvent e)
    {
	setBorder(_inactive);
    }
}
