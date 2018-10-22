package jpl.mipl.io;

import javax.media.jai.*;
import java.awt.image.*;
import java.awt.Dimension;
import javax.imageio.ImageIO;

/***********************************************************************
 * Extremely simply image file format converter program.  Uses JAI imageread
 * and imagewrite operators to do the conversion.  No metadata is converted,
 * and no image manipulation is done.
 * <p>
 * The three (mandatory) arguments are: input_filename, output_filename, type
 * where type is one of the registered Image I/O writer plugin names.
 * Running the program with no arguments prints a list or available plugins.
 * <p>
 * If the output is a TIFF image, the tile size is set to 8-line strips.
 * Otherwise, the tile size defaults (generally copied from the input image).
 */

public class SimpleConvert
{
    /** See class comments */
    public static void main(String argv[])
    {
	if (argv.length < 3) {
	    System.out.println("Usage:");
	    System.out.println("java SimpleConvert input output format");
	    System.out.println("where input and output are filenames and format is one of the following:");
	    String[] formats = ImageIO.getWriterFormatNames();
	    java.util.Arrays.sort(formats);
	    for (int i=0; i < formats.length; i++)
		System.out.println("  " + formats[i]);
	    System.out.println("Note that not all of the above can handle 16-bit data!");
	    System.out.println("Output tile size for TIFF (only) defaults to 8 lines by the image width");
	    System.exit(0);
	}

	RenderedImage img = JAI.create("imageread", argv[0]);

	ParameterBlockJAI pb = new ParameterBlockJAI("imagewrite");
	pb.addSource(img);
	pb.setParameter("output", argv[1]);
	pb.setParameter("format", argv[2]);
	if (argv[2].equalsIgnoreCase("tiff") ||
	    argv[2].equalsIgnoreCase("tif")) {

	    Dimension tilesize = new Dimension(img.getWidth(), 8);
	    pb.setParameter("tilesize", tilesize);
	}
	JAI.create("imagewrite", pb);
    }
}

