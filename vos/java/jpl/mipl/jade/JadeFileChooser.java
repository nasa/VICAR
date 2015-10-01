package jpl.mipl.jade;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import javax.swing.event.*;
import java.beans.*;
import java.util.*;
import java.io.*;

import jpl.mipl.jade.util.*;
/**
 * Custom FileLoader that can deal with  VICAR-3File images and Stereo-pairs
 */
public class JadeFileChooser extends JFrame implements PropertyChangeListener {

    // ********************************
    // ***** Dialog Return Values *****
    // ********************************
    
    /**
     * Return value if approve (ok) is chosen.
     */
    public static final int OK_OPTION = 0;
    
    /**
     * Return value if apply is chosen.
     */
    public static final int APPLY_OPTION = 1;
    /**
     * Return value if cancel is chosen.
     */
    public static final int CANCEL_OPTION = 2;
    
    /**
     * Return value if an error occured.
     */
    public static final int ERROR_OPTION = 3;
    
    private static final int RED = 0;
    private static final int GREEN = 1;
    private static final int BLUE = 2;
    
    //Determines whether the mode is set to stereo or mono
    private boolean _isStereo;
    
    //Indexed Bound Properties
    private File[] _imageLeft = new File[3];
    private File[] _imageRight = new File[3];
    
    //The outer pane containing Left and Right Panes
    private JTabbedPane _mainTabbedPane;
    
    private JTabbedPane _paneLeft;
    private JTabbedPane _paneRight;
    
    private JPanel _panelRedLeft;
    private JPanel _panelGreenLeft;
    private JPanel _panelBlueLeft;
    
    private JPanel _panelRedRight;
    private JPanel _panelGreenRight;
    private JPanel _panelBlueRight;
    
    //South Panel contains control buttons(Ok, Cancel etc.)
    //and Text fields for choosed file names.
    private JFCSouthPanel _jfcSouthPanel;
    
    //The whole JadeFileChooser uses a single instance of JFileChooser
    //which is added to the active tab.
    private JFileChooser _fileChooser;
    
    private int _returnValue = ERROR_OPTION;
    //It is here to allow to manipulate JadeFileChooser from within 
    //inner classes.
    private JFrame _jadeFileChooser;
    
    //The support object for bound listeners
    private PropertyChangeSupport _boundSupport;
    
    /**
     * Constructs a <CODE>JadeFileChooser</CODE> using the given current 
     * directory. If <CODE>isStereo</CODE> is true, then JadeFileChooser has 
     * two panes for Left and Right images, otherwise there is only one pane.
     */
    public JadeFileChooser(File currentDirectory, boolean isStereo) {
	super();
	setTitle("JADE File Chooser");
	
	//construct the support objects
	_boundSupport = new PropertyChangeSupport(this);
	
	//Set the stereo flag
	_isStereo = isStereo;
	
	//Initialize South Pane and center TabbedPane
	_mainTabbedPane = new JTabbedPane();
	_jfcSouthPanel = new JFCSouthPanel(_isStereo);
	
	//Organize the Layout
	getContentPane().setLayout(new BorderLayout());
	getContentPane().add(_mainTabbedPane, BorderLayout.CENTER);
	getContentPane().add(_jfcSouthPanel, BorderLayout.SOUTH);
	
	//Initialize and populate PaneLeft.  This one will be on 
	//the screen regardless of stereo/mono setting
	_paneLeft = new JTabbedPane();
	_panelRedLeft = new JPanel();
	_panelGreenLeft = new JPanel();
	_panelBlueLeft = new JPanel();
	_paneLeft.setTabPlacement(SwingConstants.BOTTOM);
	_paneLeft.add(_panelRedLeft, "Red/BW");
	_paneLeft.add(_panelGreenLeft, "Green");
	_paneLeft.add(_panelBlueLeft, "Blue");
		
	//Initialize fileChooser and add it to panelRedLeft
	_fileChooser = new JFileChooser(currentDirectory);
	_panelRedLeft.add(_fileChooser);
	
	if (isStereo) {
	    _mainTabbedPane.add(_paneLeft, "Left Image");
	    _paneRight = new JTabbedPane();
	    _mainTabbedPane.addTab(
				   "Right Image",
				   new ImageIcon("document.gif"),
				   _paneRight,
				   "Right Image Chooser");
	    
	    _panelRedRight = new JPanel();
	    _panelGreenRight = new JPanel();
	    _panelBlueRight = new JPanel();
	    
	    _paneRight.setTabPlacement(SwingConstants.BOTTOM);
	    _paneRight.add(_panelRedRight, "Red/BW");
	    _paneRight.add(_panelGreenRight, "Green");
	    _paneRight.add(_panelBlueRight, "Blue");
	    
	} else {
	    //We are in Mono mode
	    _mainTabbedPane.add(_paneLeft, "Image");
	    
	}
	
	//add listeners
	addMainPaneChangeListeners();
	addLeftPaneChangeListeners();
	if (isStereo)
	    addRightPaneChangeListeners();
	addFileChooserChangeListener();
	
	addPropertyChangeListener(_jfcSouthPanel);
	_jfcSouthPanel.addPropertyChangeListener(this);
	
    }
    
    /**
     *Add a bound property listener
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
	//defer to the support object
	_boundSupport.addPropertyChangeListener(listener);
    }
    
    /**
     *Remove a bound property listener
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
	//defer to the support object
	_boundSupport.removePropertyChangeListener(listener);
    }
    //change Listener for the main Pane.  It switches between
    //Left and Right Image panes.
    private void addMainPaneChangeListeners() {
	_mainTabbedPane.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		JTabbedPane tabbedPane = (JTabbedPane) e.getSource();
		int mainIndex = tabbedPane.getSelectedIndex();
		String title = tabbedPane.getTitleAt(mainIndex);
		
		if (title.equals("Right Image")) {
		    //find out which pane is active and add
		    //JFileChooser to that pane
		    int indexRight = _paneRight.getSelectedIndex();
		    switch (indexRight) {
		    case JadeFileChooser.RED :
			_panelRedRight.add(_fileChooser);
			break;
		    case JadeFileChooser.GREEN :
			_panelGreenRight.add(_fileChooser);
			break;
		    case JadeFileChooser.BLUE :
			_panelBlueRight.add(_fileChooser);
			break;
		    }
		} else if (
			 title.equals("Left Image") || title.equals("Image")) {
		    int indexLeft = _paneLeft.getSelectedIndex();
		    switch (indexLeft) {
		    case JadeFileChooser.RED :
			_panelRedLeft.add(_fileChooser);
			break;
		    case JadeFileChooser.GREEN :
			_panelGreenLeft.add(_fileChooser);
			break;
		    case JadeFileChooser.BLUE :
			_panelBlueLeft.add(_fileChooser);
			break;
		    }
		}
	    }
	});
    }
    
    //Change Listener for the Left Image Pane
    //It switches between Red, Green, Blue panes
    private void addLeftPaneChangeListeners() {
	_paneLeft.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		JTabbedPane tabbedPane = (JTabbedPane) e.getSource();
		int index = tabbedPane.getSelectedIndex();
		switch (index) {
		case JadeFileChooser.RED :
		    _panelRedLeft.add(_fileChooser);
		    break;
		case JadeFileChooser.GREEN :
		    _panelGreenLeft.add(_fileChooser);
		    break;
		case JadeFileChooser.BLUE :
		    _panelBlueLeft.add(_fileChooser);
		    break;
		}
	    }
		});
    }
    
    //Change Listener for the Right Image Pane
    //It switches between Red, Green, Blue panes
    private void addRightPaneChangeListeners() {
	_paneRight.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		JTabbedPane tabbedPane = (JTabbedPane) e.getSource();
		int index = tabbedPane.getSelectedIndex();
		switch (index) {
		case JadeFileChooser.RED :
		    _panelRedRight.add(_fileChooser);
		    break;
		case JadeFileChooser.GREEN :
		    _panelGreenRight.add(_fileChooser);
		    break;
		case JadeFileChooser.BLUE :
		    _panelBlueRight.add(_fileChooser);
		    break;
		}
	    }
	});
    }
    
    //Change Listener for the JFileChooser
    private void addFileChooserChangeListener() {
	_fileChooser.addActionListener(new ActionListener() {
	    public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand()
		    .equals(JFileChooser.APPROVE_SELECTION)) {
		    File file = _fileChooser.getSelectedFile();
		    
		    int index = _mainTabbedPane.getSelectedIndex();
		    String title = _mainTabbedPane.getTitleAt(index);
		    if (title.equals("Left Image") || title.equals("Image")) {
			int indexLeft = _paneLeft.getSelectedIndex();
			switch (indexLeft) {
			case JadeFileChooser.RED :
			    setImageLeft(
					 JadeFileChooser.RED,
					 _fileChooser.getSelectedFile());
			    _paneLeft.setSelectedIndex(
						       JadeFileChooser.GREEN);
			    _panelGreenLeft.add(_fileChooser);
			    break;
			case JadeFileChooser.GREEN :
			    setImageLeft(
					 JadeFileChooser.GREEN,
					 _fileChooser.getSelectedFile());
			    _paneLeft.setSelectedIndex(
						       JadeFileChooser.BLUE);
			    _panelBlueLeft.add(_fileChooser);
			    break;
			case JadeFileChooser.BLUE :
			    setImageLeft(
					 JadeFileChooser.BLUE,
					 _fileChooser.getSelectedFile());
			    if (_isStereo) {
				_mainTabbedPane.setSelectedIndex(1);
				_paneRight.setSelectedIndex(
							  JadeFileChooser.RED);
				_panelRedRight.add(_fileChooser);
			    } else {
				_paneLeft.setSelectedIndex(
							  JadeFileChooser.RED);
				_panelRedLeft.add(_fileChooser);
			    }
			    break;
			}
		    } else if (title.equals("Right Image")) {
			int indexRight = _paneRight.getSelectedIndex();
			switch (indexRight) {
			case JadeFileChooser.RED :
			    setImageRight(
					  JadeFileChooser.RED,
					  _fileChooser.getSelectedFile());
			    _paneRight.setSelectedIndex(
							JadeFileChooser.GREEN);
			    _panelGreenRight.add(_fileChooser);
			    break;
			case JadeFileChooser.GREEN :
			    setImageRight(
					  JadeFileChooser.GREEN,
					  _fileChooser.getSelectedFile());
			    _paneRight.setSelectedIndex(
							JadeFileChooser.BLUE);
			    _panelBlueRight.add(_fileChooser);
			    break;
			case JadeFileChooser.BLUE :
			    setImageRight(
					  JadeFileChooser.BLUE,
					  _fileChooser.getSelectedFile());
			    _mainTabbedPane.setSelectedIndex(0);
			    _paneLeft.setSelectedIndex(JadeFileChooser.RED);
			    _panelRedLeft.add(_fileChooser);
			    break;
			}
		    }
		} else if (
			   e.getActionCommand().equals(
					      JFileChooser.CANCEL_SELECTION)) {
		    //hide fileLoader
		    _jadeFileChooser.setVisible(false);
		}
	    }
	});
    }
    
    /**
     *  The get method for the ImageLeftCount property
     */
    public int getImageLeftCount() {
	// the ImageLeftCount property is derived from the size
	// of the imageLeft Array
	if (_imageLeft[0] != null
	    && _imageLeft[1] != null
	    && _imageLeft[2] != null)
	    //if all 3 not null we have a file for each band
	    return 3;
	else if (_imageLeft[0] != null)
	    //An image consists of a single file
	    return 1;
	else
	    //All other cases
	    return 0;
    }
    /**
     *  The get method for the ImageRightCount property
     */
    public int getImageRightCount() {
	// the ImageLeftCount property is derived from the size
	// of the imageLeft Array
	if (_imageRight[0] != null
	    && _imageRight[1] != null
			&& _imageRight[2] != null)
	    //if all 3 not null we have a file for each band
	    return 3;
	else if (_imageRight[0] != null)
	    //An image consists of a single file
	    return 1;
	else
	    //All other cases
	    return 0;
	
    }
    
    /**
     *The get Method for the LeftImage property array
     */
    public File[] getImageLeft() {
	//allocate an array of Files for the imageLeft names
	File[] files = new File[getImageLeftCount()];
	//copy the elements of the imageLeft ArrayList into
	//the File array, and then return an array
	for (int cnt = 0; cnt < files.length; cnt++)
	    files[cnt] = _imageLeft[cnt];
	return files;
    }
    /**
     *The get Method for the RightImage property array
     */
    public File[] getImageRight() {
	//allocate an array of Files for the imageLeft names
	File[] files = new File[getImageRightCount()];
	//copy the elements of the imageRight ArrayList into
	//the File array, and then return an array
	for (int cnt = 0; cnt < files.length; cnt++)
	    files[cnt] = _imageRight[cnt];
	return files;
	
    }
    /**
     * The set method for ImageLeft property array
     */
    public void setImageLeft(File[] files) {
	// the existing list of imageLefts is removed 
	// in favor of the new set of imageLefts
	_imageLeft = null;
	
	// set the size of the imageLefts arrayList to match
	// the length of the new array
	//	_imageLeft.ensureCapacity(files.length);
	
	//copy the values
	for (int i = 0; i < files.length; i++) {
	    
	    //use the single _imageLeft set method
	    try {
		setImageLeft(i, files[i]);
	    } catch (ArrayIndexOutOfBoundsException ex) {
		
	    }
	}
	//Trims the capacity of this ArrayList instance to be
	// the list's current size.
	//_imageLeft.trimToSize();
    }
    
    /**
     * The set method for ImageRight property array
     */
    public void setImageRight(File[] files) {
	// the existing list of imageLefts is removed 
	// in favor of the new set of imageLefts
	_imageRight = null;
	
	// set the size of the imageRights arrayList to match
	// the length of the new array
	//_imageRight.ensureCapacity(files.length);
	
	//copy the values
	for (int i = 0; i < files.length; i++) {
	    
	    //use the single _imageLeft set method
	    try {
		setImageRight(i, files[i]);
	    } catch (ArrayIndexOutOfBoundsException ex) {
		
	    }
	}
	//Trims the capacity of this ArrayList instance to be
	// the list's current size.
	//_imageRight.trimToSize();
    }
    /** 
     * The get method for single element of imageLeft property
     */
    public File getImageLeft(int index) throws ArrayIndexOutOfBoundsException {
	//make sure the index is in bounds
	if (index < 0 || index >= 3 /*getImageLeftCount()*/)
	    throw new ArrayIndexOutOfBoundsException();
	
	//get the imageLeft and return it
	File file = _imageLeft[index];
	return file;
    }
    
    /** 
     * The get method for single element of imageRight property
     */
    public File getImageRight(int index)
	throws ArrayIndexOutOfBoundsException {
	//make sure the index is in bounds
	if (index < 0 || index >= 3 /*getImageRightCount()*/
	    )
	    throw new ArrayIndexOutOfBoundsException();
	
	//get the imageLeft and return it
	File file = _imageRight[index];
	return file;
    }
    
    /**
     * set an individual element of the _imageFiles property array
     */
    public void setImageLeft(int index, File file)
	throws ArrayIndexOutOfBoundsException {
	// make sure the index is in bounds
	if (index < 0 || index >= 3 /*getImageLeftCount()*/
	    )
	    throw new ArrayIndexOutOfBoundsException();
	
	//Save the old value and then change it	
	File[] oldImage = new File[3];
		for (int cnt = 0; cnt < getImageLeftCount(); cnt++)
		    oldImage[cnt] = _imageLeft[cnt];
		//change the _imageLeft at the specified index
		_imageLeft[index] = file;
		
		//fire the change to any bound listeners
		_boundSupport.firePropertyChange("ImageLeft", oldImage, _imageLeft);
    }
    
    /**
     * set an individual element of the _imageFiles property array
     */
    public void setImageRight(int index, File file)
	throws ArrayIndexOutOfBoundsException {
	// make sure the index is in bounds
	if (index < 0 || index >= 3 /*getImageLeftCount()*/)
	    throw new ArrayIndexOutOfBoundsException();
	
	//Save the old value and then change it	
	File[] oldImage = new File[3];
	for (int cnt = 0; cnt < getImageLeftCount(); cnt++)
	    oldImage[cnt] = _imageRight[cnt];
	//change the _imageRight at the specified index
	_imageRight[index] = file;
	
	//fire the change to any bound listeners
	_boundSupport.firePropertyChange("ImageRight", oldImage, _imageRight);
    }
    /**The get Method for the ReturnValue property*/
    public int getReturnValue() {
	return _returnValue;
    }
    
    /**The set Method for the ReturnValue property*/
	public void setReturnValue(int newValue) {
	    //Save the old value and then change it
	    int oldValue = _returnValue;
	    _returnValue = newValue;
	    
	    //fire the change to any bound listeners
	    _boundSupport.firePropertyChange("ReturnValue", 100, _returnValue);
	}
    /**
     *Event notification that file was changed by other means rather
     *than through JFileChooser or Button was pressed
     */
    public void propertyChange(PropertyChangeEvent evt) {
	if (evt.getPropertyName().equalsIgnoreCase("FileRedLeft")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageLeft[JadeFileChooser.RED] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageLeft[JadeFileChooser.RED] = null;
		
		_mainTabbedPane.setSelectedIndex(0);
		_paneLeft.setSelectedIndex(JadeFileChooser.GREEN);
		_panelGreenLeft.add(_fileChooser);
		
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	    
	} else if (evt.getPropertyName().equalsIgnoreCase("FileGreenLeft")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageLeft[JadeFileChooser.GREEN] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageLeft[JadeFileChooser.GREEN] = null;
		
		_mainTabbedPane.setSelectedIndex(0);
		_paneLeft.setSelectedIndex(JadeFileChooser.BLUE);
		_panelBlueLeft.add(_fileChooser);
		
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	} else if (evt.getPropertyName().equalsIgnoreCase("FileBlueLeft")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageLeft[JadeFileChooser.BLUE] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageLeft[JadeFileChooser.BLUE] = null;
		if (_isStereo) {
		    _mainTabbedPane.setSelectedIndex(1);
		    _paneRight.setSelectedIndex(JadeFileChooser.RED);
		    _panelRedRight.add(_fileChooser);
		} else {
		    _mainTabbedPane.setSelectedIndex(0);
		    _paneLeft.setSelectedIndex(JadeFileChooser.RED);
		    _panelRedLeft.add(_fileChooser);
		}
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	    
	} else if (evt.getPropertyName().equalsIgnoreCase("FileRedRight")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageRight[JadeFileChooser.RED] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageRight[JadeFileChooser.RED] = null;
		_mainTabbedPane.setSelectedIndex(1);
		_paneRight.setSelectedIndex(JadeFileChooser.GREEN);
		_panelGreenRight.add(_fileChooser);
		
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	} else if (evt.getPropertyName().equalsIgnoreCase("FileGreenRight")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageRight[JadeFileChooser.GREEN] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageRight[JadeFileChooser.GREEN] = null;
		_mainTabbedPane.setSelectedIndex(1);
		_paneRight.setSelectedIndex(JadeFileChooser.BLUE);
		_panelBlueRight.add(_fileChooser);
		
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	    
	} else if (evt.getPropertyName().equalsIgnoreCase("FileBlueRight")) {
	    File newValue = (File) evt.getNewValue();
	    if (newValue.isFile()
		|| (newValue.getName()).equalsIgnoreCase("")) {
		if (newValue.isFile()) {
		    _imageRight[JadeFileChooser.BLUE] = newValue;
		    _fileChooser.setSelectedFile(newValue);
		    _fileChooser.ensureFileIsVisible(newValue);
		} else
		    _imageRight[JadeFileChooser.BLUE] = null;
		_mainTabbedPane.setSelectedIndex(0);
		_paneLeft.setSelectedIndex(JadeFileChooser.RED);
		_panelRedLeft.add(_fileChooser);
		
	    } else if (newValue.isDirectory())
		_fileChooser.setCurrentDirectory(newValue);
	    //_fileChooser.approveSelection();
	} else if (evt.getPropertyName().equalsIgnoreCase("ButtonPressed")) {
	    if (((String) evt.getNewValue()).equalsIgnoreCase("OK")) {
		setReturnValue(OK_OPTION);
		setVisible(false);
	    } else if (
		       ((String) evt.getNewValue()).equalsIgnoreCase("Apply")) {
		setReturnValue(OK_OPTION);
	    } else if (
		       ((String) evt.getNewValue()).equalsIgnoreCase("Cancel")) {
				//hide fileLoader
		setReturnValue(CANCEL_OPTION);
		setVisible(false);
	    } else if (((String) evt.getNewValue()).equalsIgnoreCase("Help")) {
				//not yet implemented
	    }
	}
    }
    public int showOpenDialog() {
	return _returnValue;
    }
    public File[] getSelectedFiles() {
	return (File[]) _imageLeft.clone();
    }
    public File[] getSelectedLeftFiles() {
	return (File[]) _imageLeft.clone();
    }
    public File[] getSelectedRightFiles() {
	return (File[]) _imageRight.clone();
    }
}
