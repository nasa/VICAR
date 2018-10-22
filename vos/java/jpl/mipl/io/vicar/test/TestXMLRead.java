package jpl.mipl.io.vicar.test;

import jpl.mipl.io.vicar.*;
import org.w3c.dom.*;
import org.apache.xml.serialize.*;
import java.io.*;
import javax.xml.parsers.*;
import java.util.*;

/**
 * TestXMLRead program.  Accepts a VICAR label in XML format and converts
 * it to a VicarLabel.  The result is printed.  If a second filename is
 * given, the resultant label is output (as text) to the file for comparison
 * with the original label.
 * <p>
 * Combined, with TestXMLWrite, this can be used to test the XML transfer
 * capability:
 * <pre>
 * % java jpl.mipl.io.vicar.test.TestXMLWrite image.vic image.xml image.lbl
 * % java jpl.mipl.io.vicar.test.TestXMLRead image.xml image.lbl2
 * % diff image.lbl image.lbl2
 * </pre>
 * The results should be identical.  And neither program should produce an
 * error.
 */

public class TestXMLRead
{

    public static void main(String argv[]) throws Exception
    {
	if (argv.length != 1 && argv.length != 2) {
	    System.out.println("Usage:");
	    System.out.println("% java jpl.mipl.io.vicar.test.TestXMLRead xml_file {text_file}");
	    System.out.println("Takes a VICAR XML file, converts it to a VicarLabel, and prints it.");
	    System.out.println("Optionally outputs the VicarLabel as text to the text_file.");
	    System.exit(0);
	}

	Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
				.parse(new FileInputStream(argv[0]));

	ArrayList errorList = new ArrayList();
	try {
	    VicarLabel lbl = new VicarLabel(doc.getDocumentElement(),errorList);
	    System.out.println("VICAR label:");
	    System.out.println(lbl);

	    if (argv.length == 2)
		new PrintStream(new FileOutputStream(argv[1])).println(lbl);

        } catch (Exception e) {
            System.out.println("me: " + e);
            e.printStackTrace();
        }

	if (!errorList.isEmpty()) {
	   System.out.println("errors:");
	   for (int i=0; i<errorList.size(); i++)
		System.out.println(errorList.get(i));
	}

    }
}

