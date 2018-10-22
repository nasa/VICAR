import java.awt.*;
import java.awt.image.*;
import java.awt.color.*;
import javax.swing.*;
import javax.media.jai.*;
import jpl.mipl.util.ComponentColorModelGray;

public class TestColorModel {

    public static void main(String[] args)
    {
	PlanarImage planarImage = JAI.create("fileload", args[0]);
	    
	BufferedImage bufferedImage = planarImage.getAsBufferedImage();
	
	BufferedImage newBufferedImage = null;

	//adjust ColorModel if necessary
	ColorModel colorModel = 
	    ComponentColorModelGray.adjustColorModel(
					  bufferedImage.getSampleModel(),
					  bufferedImage.getColorModel());

	//create new image with adjusted colorModel
	newBufferedImage = 
	    new BufferedImage(colorModel,
			      bufferedImage.getRaster(),
			      true, null);
		  
	//If this line is commented out, then original image is used 
	//for display purposes, otherwise we are using image with 
	//adjusted colorModel.
	bufferedImage = newBufferedImage;

	//Display Code
	ImageIcon imageIcon = new ImageIcon(bufferedImage);
	JLabel jLabel = new JLabel(imageIcon);
	JFrame jFrame = new JFrame();
	jFrame.getContentPane().setLayout(new BorderLayout());
	jFrame.getContentPane().add(jLabel, BorderLayout.CENTER);
	jFrame.pack();
	jFrame.setSize(jLabel.getSize());
	jFrame.setVisible(true);
    }


}
