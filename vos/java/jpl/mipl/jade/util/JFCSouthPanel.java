package jpl.mipl.jade.util;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.beans.*;
import java.util.*;
import java.io.*;
import javax.swing.border.*;

/**
 * <code>SouthPanel</code> is a part of <code>FileLoader</code> GUI.
 * It contains FileLoader's control buttons(OK, Apply, Cancel etc.)
 * and Text Fields that show selected file names.
 */
public class JFCSouthPanel extends JComponent 
                           implements ActionListener, 
				      PropertyChangeListener,
                                      FocusListener {

    private boolean _isStereo;

    //Bound Properties
    private File _fileRedLeft;
    private File _fileGreenLeft;
    private File _fileBlueLeft;
    private File _fileRedRight;
    private File _fileGreenRight;
    private File _fileBlueRight;
    private String _buttonPressed;

    //Buttons
    private JButton _okButton = new JButton("OK");
    private JButton _applyButton = new JButton("Apply");
    private JButton _cancelButton = new JButton("Cancel");
    private JButton _helpButton   = new JButton("Help");

    private JPanel _buttonPanel;

    //TextFields
    private JTextField _tfRedLeft = new JTextField("");
    private JTextField _tfGreenLeft = new JTextField("");
    private JTextField _tfBlueLeft = new JTextField("");

    private JTextField _tfRedRight = new JTextField("");
    private JTextField _tfGreenRight = new JTextField("");
    private JTextField _tfBlueRight = new JTextField("");

    private JPanel _tfPanel;

    private Border _defaultBorder;

    /**The support object for bound listeners*/
    private PropertyChangeSupport _boundSupport;

    /**
     * Constructs a <CODE>JFCSouthPanel</CODE>.
     * If <CODE>isStereo</CODE> is false, then <CODE>JFCSouthPanel</CODE>
     * has only 3 text fields: one for each band of the image, otherwise 
     * the number of text fields doubled.
     *
     */
    public JFCSouthPanel(boolean isStereo)
    {
	super();

	_isStereo = isStereo;
	//construct the support objects
	_boundSupport = new PropertyChangeSupport(this);

	populateTextFieldsPanel(isStereo);
	populateButtonPanel();

	setLayout(new BorderLayout());
	add(_tfPanel, BorderLayout.CENTER);
	add(_buttonPanel, BorderLayout.SOUTH);

    }
    private void populateTextFieldsPanel(boolean isStereo)
    {
	_tfPanel = new JPanel();

	_tfRedLeft.addActionListener(this);
	_tfGreenLeft.addActionListener(this);
	_tfBlueLeft.addActionListener(this);
	 
	if(isStereo) {
	    _tfPanel.setLayout(new GridLayout(3, 2));
	    _tfPanel.setBorder(BorderFactory.createTitledBorder(
			       new LineBorder(Color.black, 2),
			       "Left/Right",
			       TitledBorder.LEFT,
			       TitledBorder.DEFAULT_POSITION
			       ));
	    //add Text Fields
	    _tfPanel.add(_tfRedLeft);
	    _tfPanel.add(_tfRedRight);
	    _tfPanel.add(_tfGreenLeft);
	    _tfPanel.add(_tfGreenRight);
	    _tfPanel.add(_tfBlueLeft);
	    _tfPanel.add(_tfBlueRight);

	    _tfRedRight.addActionListener(this);
	    _tfGreenRight.addActionListener(this);
	    _tfBlueRight.addActionListener(this);
	}
	//Mono mode
	else {
	    _tfPanel.setLayout(new GridLayout(3, 1));
	    _tfPanel.setBorder(BorderFactory.createTitledBorder(
			       new LineBorder(Color.black, 2),
			       "Image",
			       TitledBorder.LEFT,
			       TitledBorder.DEFAULT_POSITION
			       ));
	    //add Text Fields
	    _tfPanel.add(_tfRedLeft);
	    _tfPanel.add(_tfGreenLeft);
	    _tfPanel.add(_tfBlueLeft);
	}

	//set Border for the first field
	_defaultBorder = _tfRedLeft.getBorder();
	_tfRedLeft.setBorder(new LineBorder(Color.red, 1));	
    }
    private void populateButtonPanel()
    {
	_buttonPanel = new JPanel();

	_buttonPanel.setLayout(new GridLayout(1, 4));
	_buttonPanel.add(_okButton);
	_buttonPanel.add(_applyButton);
	_buttonPanel.add(_cancelButton);
	_buttonPanel.add(_helpButton);

	
	_okButton.addActionListener(this);
	_applyButton.addActionListener(this);
	_cancelButton.addActionListener(this);
	_helpButton.addActionListener(this);
	
    }
    private void setTFBorders(Border border)
    {
	_tfRedLeft.setBorder(border);
	_tfGreenLeft.setBorder(border);
	_tfBlueLeft.setBorder(border);

	_tfRedRight.setBorder(border);
	_tfGreenRight.setBorder(border);	
	_tfBlueRight.setBorder(border);
    }
    /**Add a bound property listener*/
    public void
    addPropertyChangeListener(PropertyChangeListener listener)
    {
	//defer to the support object
	_boundSupport.addPropertyChangeListener(listener);
    }
    
    /**Remove a bound property listener*/
    public void
    removePropertyChangeListener(PropertyChangeListener listener)
    {
	//defer to the support object
	_boundSupport.removePropertyChangeListener(listener);
    }
    /**
     *The get Method for the FileRedLeft property
     */
    public File getFileRedLeft()
    {
	return _fileRedLeft;
    } 

    /**
     *The set Method for the FileRedLeft property
     */
    public void setFileRedLeft(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileRedLeft;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileRedLeft = new File(newValue);

	if(_fileRedLeft.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    _tfGreenLeft.setBorder(new LineBorder(Color.green, 1));
	}

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileRedLeft", oldValue, 
					 _fileRedLeft);
    }  

    /**
     *The get Method for the FileGreenLeft property
     */
    public File getFileGreenLeft()
    {
	return _fileGreenLeft;
    } 

    /**
     *The set Method for the FileGreenLeft property
     */
    public void setFileGreenLeft(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileGreenLeft;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileGreenLeft = new File(newValue);

	if(_fileGreenLeft.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    _tfBlueLeft.setBorder(new LineBorder(Color.blue, 1));
	}

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileGreenLeft", oldValue, 
					 _fileGreenLeft);
    }  

    /**
     *The get Method for the FileBlueLeft property
     */
    public File getFileBlueLeft()
    {
	return _fileBlueLeft;
    } 

    /**
     *The set Method for the FileBlueLeft property
     */
    public void setFileBlueLeft(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileBlueLeft;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileBlueLeft = new File(newValue);

	if(_fileBlueLeft.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    if(_isStereo)
		_tfRedRight.setBorder(new LineBorder(Color.red, 1));
	    else
		_tfRedLeft.setBorder(new LineBorder(Color.red, 1));
	}

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileBlueLeft", oldValue, 
					 _fileBlueLeft);
    } 
    
    /**
     *The get Method for the FileRedRight property
     */
    public File getFileRedRight()
    {
	return _fileRedRight;
    } 

    /**
     *The set Method for the FileRedRight property
     */
    public void setFileRedRight(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileRedRight;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileRedRight = new File(newValue);

	if(_fileRedRight.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    _tfGreenRight.setBorder(new LineBorder(Color.green, 1));
	}

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileRedRight", oldValue, 
					 _fileRedRight);
    }  

    /**
     *The get Method for the FileGreenRight property
     */
    public File getFileGreenRight()
    {
	return _fileGreenRight;
    } 

    /**
     *The set Method for the FileGreenRight property
     */
    public void setFileGreenRight(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileGreenRight;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileGreenRight = new File(newValue);

	if(_fileGreenRight.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    _tfBlueRight.setBorder(new LineBorder(Color.blue, 1));
	}

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileGreenRight", oldValue, 
					 _fileGreenRight);
    }  

    /**
     *The get Method for the FileBlueRight property
     */
    public File getFileBlueRight()
    {
	return _fileBlueRight;
    } 

    /**
     *The set Method for the FileBlueRight property
     */
    public void setFileBlueRight(String newValue)   
    {
	//This condition will have to be changed
	//when we would want to create new files.
	if(!((new File(newValue)).exists()) &&
	   !newValue.equalsIgnoreCase(""))  //handles delete case
	    return;

	//Save the old value and then change it
	//File oldValue = _fileBlueRight;
	//make sure that propertyChange event always fires
	File oldValue = null;
	_fileBlueRight = new File(newValue);

	if(_fileBlueRight.isFile() || newValue.equalsIgnoreCase("")) {
	    setTFBorders(_defaultBorder);
	    _tfRedLeft.setBorder(new LineBorder(Color.red, 1));
	}
	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("FileBlueRight", oldValue, 
					 _fileBlueRight);
    }  

    /**The get Method for the ButtonPressed property*/
    public String getButtonPressed()
    {
	return _buttonPressed;
    } 

    /**The set Method for the ButtonPressed property*/
    public void setButtonPressed(String newValue)   
    {
	//Save the old value and then change it
	String oldValue = _buttonPressed;
	_buttonPressed = newValue;

	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("ButtonPressed", null, 
					 _buttonPressed);
    }  
    /**
     * Listens for property change events from <CODE>JadeFileLoader</CODE>
     * and transmits them to Text Fields Panel.
     */
    public void propertyChange(PropertyChangeEvent evt)
    {
	if(evt.getPropertyName().equalsIgnoreCase("ImageLeft")) {
	    File[] newValue = (File[])evt.getNewValue();
	    if(newValue[0] != null) {
		if(!_tfRedLeft.getText().
		   equalsIgnoreCase(newValue[0].getName())) {
		   _tfRedLeft.setText(newValue[0].getName());
		   setTFBorders(_defaultBorder);
		   _tfGreenLeft.setBorder(new LineBorder(Color.green, 1));
		}
	    }
	    if(newValue[1] != null) {
		if(!_tfGreenLeft.getText().
		   equalsIgnoreCase(newValue[1].getName())) {
		   _tfGreenLeft.setText(newValue[1].getName());
		   setTFBorders(_defaultBorder);
		   _tfBlueLeft.setBorder(new LineBorder(Color.blue, 1));
		}
	    }
	    if(newValue[2] != null) {
		if(!_tfBlueLeft.getText().
		   equalsIgnoreCase(newValue[2].getName())) {
		   _tfBlueLeft.setText(newValue[2].getName());
		   setTFBorders(_defaultBorder);
		   if(_isStereo)
		       _tfRedRight.setBorder(new LineBorder(Color.red, 1));
		   else
		       _tfRedLeft.setBorder(new LineBorder(Color.red, 1));
		}
	    }
	}
	else if(evt.getPropertyName().equalsIgnoreCase("ImageRight")) {
	    File[] newValue = (File[])evt.getNewValue();
	    if(newValue[0] != null) {
		if(!_tfRedRight.getText().
		   equalsIgnoreCase(newValue[0].getName())) {
		   _tfRedRight.setText(newValue[0].getName());
		   setTFBorders(_defaultBorder);
		   _tfGreenRight.setBorder(new LineBorder(Color.green, 1));
		}
	    }
	    if(newValue[1] != null) {
		if(!_tfGreenRight.getText().
		   equalsIgnoreCase(newValue[1].getName())) {
		   _tfGreenRight.setText(newValue[1].getName());
		   setTFBorders(_defaultBorder);
		   _tfBlueRight.setBorder(new LineBorder(Color.blue, 1));
		}
	    }
	    if(newValue[2] != null) {
		if(!_tfBlueRight.getText().
		   equalsIgnoreCase(newValue[2].getName())) {
		   _tfBlueRight.setText(newValue[2].getName());
		   setTFBorders(_defaultBorder);
		   _tfRedLeft.setBorder(new LineBorder(Color.red, 1));
		}
	    }
	}
    }
    /**
     * Handles action events from JTextField buttons
     */
    public void actionPerformed(ActionEvent evt)
    {
	if(evt.getSource() == _tfRedLeft)
	    setFileRedLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenLeft)
	    setFileGreenLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueLeft)
	    setFileBlueLeft(((JTextField)evt.getSource()).getText());	    
	else if(evt.getSource() == _tfRedRight)
	    setFileRedRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenRight)
	    setFileGreenRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueRight)
	    setFileBlueRight(((JTextField)evt.getSource()).getText());
	else
	    //must be the buttons
	    setButtonPressed(evt.getActionCommand());
    }
    public void focusGained(FocusEvent evt)
    {
	if(evt.getSource() == _tfRedLeft)
	    setFileRedLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenLeft)
	    setFileGreenLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueLeft)
	    setFileBlueLeft(((JTextField)evt.getSource()).getText());	    
	else if(evt.getSource() == _tfRedRight)
	    setFileRedRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenRight)
	    setFileGreenRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueRight)
	    setFileBlueRight(((JTextField)evt.getSource()).getText());
    }
    public void focusLost(FocusEvent evt)
    {
	if(evt.getSource() == _tfRedLeft)
	    setFileRedLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenLeft)
	    setFileGreenLeft(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueLeft)
	    setFileBlueLeft(((JTextField)evt.getSource()).getText());	    
	else if(evt.getSource() == _tfRedRight)
	    setFileRedRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfGreenRight)
	    setFileGreenRight(((JTextField)evt.getSource()).getText());
	else if(evt.getSource() == _tfBlueRight)
	    setFileBlueRight(((JTextField)evt.getSource()).getText());
    }
}
