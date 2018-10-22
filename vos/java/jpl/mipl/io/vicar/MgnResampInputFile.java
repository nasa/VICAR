/*
 * This class manages a single Magellan Resampled format input image file.
 * <p>
 * Based on PDSInputFile.java.
 *
 * @author Bob Deen
 */
 
package jpl.mipl.io.vicar;

import java.io.*;
import jpl.mipl.io.util.DOMutils;
import org.w3c.dom.*;
import java.util.*;

public class MgnResampInputFile extends VicarInputFile
{
    
    MgnResampHeader _resamp_header = null;

    VicarDataFormat _vdf;
    DataInput _di;

    Document _xml_doc = null;				// XML for header

    boolean _gotMetadata = false;

    boolean debug = true;

    public void setDebug(boolean d) {
	debug = d;
    }


/***********************************************************************
 * Called at the end of open() by the superclass.  Here we make sure the
 * file is not sequential, as random-access is mandatory for these files.
 */
    protected void openInternalLast() {
        if (!_random_allowed || !_random_easy)
            throw new UnsupportedOperationException("MGN Resamp must be read from a stream allowing random access");
    }
    
/***********************************************************************
 * Read the label (metadata) from the file.
 */
    protected void setupLabels() throws IOException {

	if (debug) {
	    System.out.println("MgnResampInputFile.setupLabels()");
	    System.out.println("input type: "+_input_stream); 
	}

	// Create a Vax-format input stream to read the header

	_vdf = new VicarDataFormat("X86-MACOSX", "LOW", "RIEEE");
        _di = _vdf.getDataInputWrapper(_input_stream);


	// Read in the header

        _resamp_header = new MgnResampHeader(_di);

	// Create the system label

	_system = createSystemLabel();

	// Fix sizes based on label

	_lblsize_front = _system.getRecsize();  // 1 record of header
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

        if (debug)
            _resamp_header.print(System.out);
    }

/**************************************************************
 * Create a SystemLabel from the contents of the header.
 */
    protected SystemLabel createSystemLabel()
    {
	SystemLabel sys = new SystemLabel();

	sys.setFormat("REAL");
	sys.setType("IMAGE");
	sys.setDim(3);
	sys.setEOL(0);
	sys.setOrg("BSQ");
	sys.setNL(_resamp_header.getNumLines());
	sys.setNS(_resamp_header.getNumSamps());
	sys.setNB(1);
	sys.setNBB(0);
	sys.setNLB(0);
	sys.setHost("X86-MACOSX");
	sys.setIntFmt("LOW");
	sys.setRealFmt("RIEEE");
	sys.setBHost("X86-MACOSX");
	sys.setBIntFmt("LOW");
	sys.setBRealFmt("RIEEE");
	sys.setBLType("MGN-RESAMP");

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

        if (_xml_doc == null) {
            // Create the XML

            DOMutils domUtils = new DOMutils();
            _xml_doc = domUtils.getNewDocument();
            Element root = (Element)_xml_doc.createElement("mgn_resamp");

            root.appendChild(_resamp_header.buildDom(_xml_doc));
            _xml_doc.appendChild(root);
        }

	return _xml_doc;
    }

/***********************************************************************
 * There is no Vicar label for MGN Resamp's, so we just return null here.
 */
 
    public synchronized VicarLabel getVicarLabel() throws IOException
    {
	return null;
    }

/***********************************************************************
 * Indicates whether or not the label has been completely
 * read.  Always true for MGN F-BIDRs since there are no EOL labels.
 */
    public synchronized boolean isLabelComplete()
    {
	return true;
    }

/***********************************************************************
 * Returns the MgnResampHeader object.
 */
    public MgnResampHeader getHeader()
    {
	return _resamp_header;
    }

}

