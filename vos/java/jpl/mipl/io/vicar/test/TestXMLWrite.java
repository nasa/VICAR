package jpl.mipl.io.vicar.test;

import java.io.*;
import jpl.mipl.io.vicar.*;
import org.w3c.dom.*;
import org.apache.xml.serialize.*;

/**
 * TestXMLWrite program.  Takes a VICAR file, and creates an XML file with
 * the contents of it.  The XML file will not be pretty (i.e. no formatting
 * is done).  The original label is (optionally) written as text to a third
 * file, for ease of comparison.
 * <p>
 * Note:  <em>This program has dependencies on the Xerces XML parser.</em>
 * These dependencies should be removed when JDK 1.4 becomes available.
 */

public class TestXMLWrite
{

    public static void main(String argv[]) throws Exception
    {
	if (argv.length != 2 && argv.length != 3) {
	    System.out.println("Usage:");
	    System.out.println("% java jpl.mipl.io.vicar.test.TestXMLWrite vicar_file xml_file {text_file}");
	    System.out.println("Takes a VICAR file and writes the label as XML");
	    System.out.println("and optionally writes the VICAR label as text");
	    System.exit(0);
	}

	Document doc = new org.apache.xerces.dom.DocumentImpl();

	try {
	    VicarInputFile vif = new VicarInputFile(argv[0]);

	    VicarLabel vl = vif.getVicarLabel();
	    System.out.println("VICAR label:");
	    System.out.println(vl);

	    if (argv.length == 3)
		new PrintStream(new FileOutputStream(argv[2])).println(vl);

	    Node xml = vl.toXML(doc);

	    doc.appendChild(xml);

	    SerializerFactory factory =
			SerializerFactory.getSerializerFactory("xml");

	    // Write to stdout
	    System.out.println("XML label:");
	    Serializer serializer = factory.makeSerializer(
			System.out, new OutputFormat(doc));
	    serializer.asDOMSerializer().serialize(doc);
	    System.out.println();

	    // Write to output file
	    serializer = factory.makeSerializer(
			new FileOutputStream(argv[1]), new OutputFormat(doc));
	    serializer.asDOMSerializer().serialize(doc);

	} catch (Exception e) {
	    System.out.println("me: " + e);
	    e.printStackTrace();
	}

    }
}

