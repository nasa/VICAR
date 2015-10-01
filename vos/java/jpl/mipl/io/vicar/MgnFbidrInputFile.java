/*
 * This class manages a single Magellan F-BIDR input image file.
 * <p>
 * Based on PDSInputFile.java.
 *
 * @author Bob Deen
 */
 
package jpl.mipl.io.vicar;

import java.io.*;
import jpl.mipl.io.util.DOMutils;
import org.w3c.dom.*;

public class MgnFbidrInputFile extends VicarInputFile
{

    MgnFbidrHeader _fbidr_header = null;	// Holder of F-BIDR metadata

    VicarDataFormat _vdf_hdr;			// VAX format for header

    Document _xml_doc;				// XML for header

    boolean _gotMetadata = false;
    
    boolean debug = false;

    public void setDebug(boolean d) {
	debug = d;
    }

/***********************************************************************
 * Read the labels (metadata) from the file
 */
    protected void setupLabels() throws IOException {

	if (debug) {
	    System.out.println("MgnFbidrInputFile.setupLabels()"); 
	    System.out.println("input type: "+_input_stream); 
	}

	// Create a Vax-format input stream to read the header

	_vdf_hdr = new VicarDataFormat("VAX-VMS", "LOW", "VAX");

	DataInput di_hdr = _vdf_hdr.getDataInputWrapper(_input_stream);

	// Read in the heaer

	_fbidr_header = new MgnFbidrHeader(di_hdr);

	// Create the system label

	_system = createSystemLabel();

	// Fix sizes based on label

	_lblsize_front = 92;		// constant label size
	_current_file_pos = _lblsize_front;	// we read the entire label
        _image_size_bytes = (_system.getNLB() +
                                ((long)_system.getN2() * (long)_system.getN3()))
                        * _system.getRecsize();
	_record_size = _system.getRecsize();


	// put things into the properties of the image
	// setProperty("MgnFbidrHeader", (Object) _fbidr_header);
	// setProperty("MgnFbidrHeader_ClassName",
	// 			(Object) _fbidr_header.getClass().getName());
	// setProperty("ImageFormatName", (Object) "mgn-fbidr");
	// setProperty("SystemLabel", (Object) _system);
	// setProperty("SystemLabel_ClassName",
	// 			(Object) _system.getClass().getName() );

	// Create the XML

	DOMutils domUtils = new DOMutils();
	_xml_doc = domUtils.getNewDocument();
	_fbidr_header.buildDom(_xml_doc);

	if (debug)
	    _fbidr_header.print(System.out);
    }

/**************************************************************
 * Create a SystemLabel from the contents of the header.
 */
    protected SystemLabel createSystemLabel()
    {
	SystemLabel sys = new SystemLabel();

	sys.setFormat("BYTE");
	sys.setType("IMAGE");
	sys.setDim(3);
	sys.setEOL(0);
	sys.setOrg("BSQ");
	sys.setNL(_fbidr_header.getImageLineCount());
	sys.setNS(_fbidr_header.getImageLineLength() - 4); // 4 bytes of prefix
	sys.setNB(1);
	sys.setNBB(4);				// 4 bytes of prefix
	sys.setNLB(0);
	sys.setHost("VAX-VMS");
	sys.setIntFmt("LOW");
	sys.setRealFmt("VAX");
	sys.setBHost("VAX-VMS");
	sys.setBIntFmt("LOW");
	sys.setBRealFmt("VAX");
	sys.setBLType("MGN-FBIDR");

	sys.calcRecsize();

	if (debug) {
	    System.out.println("*** SYSTEM LABEL ***");
	    System.out.println(sys.toString());
	}
	return sys;
    }
    

/**************************************************************
 * @return the XML Document derived from the label
 */
    public Document getXMLDocument() {
	return _xml_doc;
    }




/***********************************************************************
 * There is no Vicar label for MGN F-BIDR's, so we just return null here.
 */
 
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
	return null;
    }

/***********************************************************************
 * Indicates whether or not the <code>PDSLabel</code> has been completely
 * read.  Always true for MGN F-BIDRs since there are no EOL labels.
 */
    public synchronized boolean isLabelComplete()
    {
	return true;
    }

/***********************************************************************
 * Returns the MgnFbidrHeader object.
 */
    public MgnFbidrHeader getHeader()
    {
	return _fbidr_header;
    }

}

