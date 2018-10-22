import java.awt.*;
import java.awt.event.*;
import java.io.File;
import javax.swing.*;
import javax.swing.border.*;
import java.awt.event.ComponentListener;

public class TestFileLoader extends JFrame {

    OpenAction _openAction = new OpenAction();

    public TestFileLoader()
    {       
	final JMenuBar menuBar = new JMenuBar();
	
	//add Individual menus to the menuBar
	menuBar.add(this.createFileMenu());

	this.getRootPane().setJMenuBar(menuBar);

	
    }

    private JMenu createFileMenu()
    {
	JMenu fileMenu = new JMenu("File");
	fileMenu.setMnemonic('f');
	
	JMenuItem itemOpenMono = fileMenu.add(new OpenActionMono());
	fileMenu.add(itemOpenMono);
	itemOpenMono.setMnemonic('m');

	JMenuItem itemOpenStereo = fileMenu.add(new OpenActionStereo());
	fileMenu.add(itemOpenStereo);
	itemOpenStereo.setMnemonic('s');

	JMenuItem itemExit = fileMenu.add(new ExitAction());
	fileMenu.add(itemExit);
	itemExit.setMnemonic('x');

	return fileMenu;
    }

    public static void main(String args[]) {
	JFrame frame = new TestFileLoader();
	frame.setBounds(300, 300, 400, 400);
	frame.setVisible(true);
    }
    class OpenActionMono extends AbstractAction {

	private File _currentDirectory = new File(".");
	private FileLoader _fileLoader;
    

	public OpenActionMono()
	{
	    super("Open Mono...");
	}

	public void actionPerformed(ActionEvent event) {
	    _fileLoader = 
	    new FileLoader(new File(System.getProperty("user.dir")), false);
	    _fileLoader.pack();
	    _fileLoader.setVisible(true);
	}
    }    
    class OpenActionStereo extends AbstractAction {

	private File _currentDirectory = new File(".");
	private FileLoader _fileLoader;
    

	public OpenActionStereo()
	{
	    super("Open Stereo...");
	}

	public void actionPerformed(ActionEvent event) {
	    _fileLoader = 
	    new FileLoader(new File(System.getProperty("user.dir")), true);
	    _fileLoader.pack();
	    _fileLoader.setVisible(true);
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
