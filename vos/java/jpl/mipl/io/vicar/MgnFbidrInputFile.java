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
import jpl.mipl.io.plugins.MgnFbidrImageReadParam;
import org.w3c.dom.*;
import java.util.*;

public class MgnFbidrInputFile extends VicarInputFile
{
    DataInput _di_hdr;  // Appropriate DI for the header wrapped around stream

    boolean _validPixelsOnly = true;
    
    ArrayList<MgnFbidrHeader> _fbidr_headers = null; // F-BIDR metadata holder
    int _n_headers;
    int _phys_nl, _logical_nl;
    int _logical_ns;
    int _phys_min_line, _phys_max_line;
    int _logical_min_line, _logical_max_line;
    int _min_samp, _max_samp;
    boolean _oblique_projection;

    long _line_ptrs[];          // pointer into the file for this line
    int _header_index[];        // which header value applies to this line

    VicarDataFormat _vdf_hdr;			// VAX format for header

    Document _xml_doc = null;				// XML for header

    boolean _gotMetadata = false;
    
    MgnFbidrImageReadParam _mgnFbidrImageReadParam = null;
        
    boolean debug = false;

    public void setDebug(boolean d) {
	debug = d;
    }

/** Constructor */
    public MgnFbidrInputFile(MgnFbidrImageReadParam param)
    {
        super();
        _mgnFbidrImageReadParam = param;
        if (debug) System.out.println("%%%%%% MgnFbidrInputFile constructor with MgnFbidrImageReadParam");
    }

/***********************************************************************
 * Called at the end of open() by the superclass.  Here we make sure the
 * file is not sequential, as random-access is mandatory for F-BIDRs.
 */
    protected void openInternalLast() {
        if (!_random_allowed || !_random_easy)
            throw new UnsupportedOperationException("F-BIDR must be read from a stream allowing random access");
    }
        
/***********************************************************************
 * Read the labels (metadata) from the file.  This actually reads ALL of the
 * logical records in the file.
 */
    protected void setupLabels() throws IOException {

	if (debug) {
	    System.out.println("MgnFbidrInputFile.setupLabels()"); 
	    System.out.println("input type: "+_input_stream); 
	}

	// We use a fairly large number here just to avoid re-allocs

        _fbidr_headers = new ArrayList<MgnFbidrHeader>(6200);
        
	// Create a Vax-format input stream to read the header

	_vdf_hdr = new VicarDataFormat("VAX-VMS", "LOW", "VAX");

	_di_hdr = _vdf_hdr.getDataInputWrapper(_input_stream);
	//DataInput di_hdr = _vdf_hdr.getDataInputWrapper(_input_stream);

	// Read in the heaers

	//_fbidr_header = new MgnFbidrHeader(di_hdr);
        boolean done = false;
        _n_headers = 0;

        while (!done) {
            try {
                MgnFbidrHeader h = new MgnFbidrHeader(_di_hdr, stream_pos());
                // read successful
                _fbidr_headers.add(_n_headers, h);
                _n_headers++;
                // Skip the data portion
                _di_hdr.skipBytes(h.getDataSize());
            } catch (Exception e) {
                done = true;
            }
        }

        // Scan the headers to create accessor data structures

        scanHeaders();

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

	//DOMutils domUtils = new DOMutils();
	//_xml_doc = domUtils.getNewDocument();
	//_fbidr_header.buildDom(_xml_doc);

	//if (debug)
	//    _fbidr_header.print(System.out);
    
    	if (debug)
            _fbidr_headers.get(0).print(System.out);
    }

    
/**************************************************************
 * Scan the headers and build the per-line accessor tables
 */
    protected void scanHeaders()
    {
        // This can get confusing.  For Oblique, things are in the "right"
        // order, with the smallest line # at the top.  For non-oblique,
        // they're backwards, with the smallext line # at the bottom (these
        // are called C1 values in the SIS).  In all cases both phys and
        // logical min/max refer to the numeric min/max extents, all relative
        // to the projection origin.  But the line number indexes into the
        // arrays are 0..n, where 0 is at the top of the image always.  So for
        // non-oblique, we have to invert the line numbers when going to the
        // line arrays.

        // Get the projection type.  They should be the same throughout the
        // file so we just look at the first.

        int data_class = _fbidr_headers.get(0).getDataClass();
        if (data_class == 2)        // multi-look sinusoidal
            _oblique_projection = false;
        else if (data_class == 66)  // multi-look oblique
            _oblique_projection = true;
        else
            throw new UnsupportedOperationException(
                 "Data must be multi-look image, either sinusoidal or oblique");

        // First prescan to count the number of lines
        _phys_min_line = 1000000;
        _phys_max_line = -1000000;
        _min_samp = 1000000;
        _max_samp = -1000000;
        for (MgnFbidrHeader h : _fbidr_headers) {
            if (_oblique_projection) {
                int start_line = h.getReferencePointOffsetLines();
                if (start_line < _phys_min_line)
                    _phys_min_line = start_line;
                int end_line = start_line + h.getImageLineCount() - 1;
                if (end_line > _phys_max_line)
                    _phys_max_line = end_line;
            }
            else {
                int end_line = h.getReferencePointOffsetLines();
                if (end_line > _phys_max_line)
                    _phys_max_line = end_line;
                int start_line = end_line - h.getImageLineCount() + 1;
                if (start_line < _phys_min_line)
                    _phys_min_line = start_line;
             }
        }

        _logical_min_line = _phys_min_line;
        _logical_max_line = _phys_max_line;
        int max_lines = 50000;      // ultimate default
        if (_mgnFbidrImageReadParam != null) {
            if (_mgnFbidrImageReadParam.isMinSet())
                _logical_min_line = _mgnFbidrImageReadParam.getMinLogicalLine();
            if (_mgnFbidrImageReadParam.isMaxSet())
                _logical_max_line = _mgnFbidrImageReadParam.getMaxLogicalLine();
            max_lines = _mgnFbidrImageReadParam.getMaxNumLines();
        }

        // Keep from getting too big
        if (_logical_max_line-_logical_min_line>max_lines)
            _logical_max_line=_logical_min_line+max_lines;


        for (MgnFbidrHeader h : _fbidr_headers) {
            int ln = h.getReferencePointOffsetLines();
            if (ln > _logical_max_line ||
                    (ln+h.getImageLineCount()) < _logical_min_line)
                continue;           // not in logical range
            int start_samp = h.getReferencePointOffsetPixels();
            if (start_samp < _min_samp)
                _min_samp = start_samp;
            int end_samp = start_samp + h.getImageLineLength() - 1;
            if (end_samp > _max_samp)
                _max_samp = end_samp;
        }

        // Now allocate the arrays
        _phys_nl = (_phys_max_line - _phys_min_line) + 1;
        _logical_nl = (_logical_max_line - _logical_min_line) + 1;
        _logical_ns = (_max_samp - _min_samp) + 1;

        _line_ptrs = new long[_phys_nl];
        _header_index = new int[_phys_nl];
        Arrays.fill(_line_ptrs, -1);
        Arrays.fill(_header_index, -1);

        // Now scan the headers again and fill in the arrays
        // Can't use foreach because we need the loop index for _header_index.
        // Note that we make no assumption the lines are contiguous - this is
        // really a mosaic-like process
        if (debug) {
System.out.println("phys min_line="+_phys_min_line+" phys max_line="+_phys_max_line);
System.out.println("log min_line="+_logical_min_line+" log max_line="+_logical_max_line);
System.out.println("min_samp="+_min_samp+" max_samp="+_max_samp);
System.out.println("phys_nl="+_phys_nl+" log nl="+_logical_nl);
        }
        for (int i=0; i < _n_headers; i++) {
            MgnFbidrHeader h = _fbidr_headers.get(i);
            long pos = h.getDataPosInFile();
            int ref_line = h.getReferencePointOffsetLines();
            int line_index;
            if (_oblique_projection)
                line_index = ref_line - _phys_min_line;
            else
                line_index = _phys_max_line - ref_line;
            int nlines = h.getImageLineCount();
            int len = h.getImageLineLength();
            for (int j=0; j < nlines; j++) {
                _line_ptrs[line_index] = pos;
                _header_index[line_index] = i;
                line_index++;
                pos += len;
            }
        }

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
	sys.setNL(_logical_nl);
	sys.setNS(_logical_ns); // 4 bytes of prefix
	sys.setNB(1);
	sys.setNBB(0);
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
    	        	    
        if (_xml_doc == null) {
            // Create the XML

            DOMutils domUtils = new DOMutils();
            _xml_doc = domUtils.getNewDocument();
            Element root = (Element)_xml_doc.createElement("mgn_fbidr");

            for (MgnFbidrHeader h : _fbidr_headers) {
                root.appendChild(h.buildDom(_xml_doc));
            }
            _xml_doc.appendChild(root);
        }

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
 * Returns the MgnFbidrHeader objects.
 */
    public List<MgnFbidrHeader> getHeaders()
    {
	return _fbidr_headers;
    }



/***********************************************************************
 * Override of VicarInputFile.readRecordNS (byte version).  This reads things
 * in the rather strange F-BIDR format rather than assuming a simple raster
 * of bytes.
 * @see VicarInputFile.readRecord(byte[], int, int, int, int, int, int)
  */
    protected void readRecordNS(byte[] data,
			int start, int length, int offset, int pixelStride,
			int n2, int n3) throws IOException
    {
        

        // Fill data buffer with 0 to start, since there almost always is extra
        // blank space around the image

        if (pixelStride == 1)
            Arrays.fill(data, offset, offset+length, (byte)0);
        else {
            for (int i=0; i < length; i++)
                data[offset+(i*pixelStride)] = 0;
        }

        // Adjust line number for logical window
        int line_index;
        if (_oblique_projection)
            line_index = n2 + (_logical_min_line - _phys_min_line);
        else
            line_index = n2 + (_phys_max_line - _logical_max_line);

        int hdr_index = _header_index[line_index];

        if (hdr_index < 0)
            return;             // no data on line, done!
        MgnFbidrHeader hdr = _fbidr_headers.get(hdr_index);

        long pos = _line_ptrs[line_index];
        if (pos < 0)
            return;         // really shouldn't happen unless hdr_index<0 too

        // Figure out the horizontal extent of *this* line.  The -4 accounts
        // for the prefix while the -1 gives us the ending coordinate
        // file_start/end are in coords relative to the lines in this block
        // min/max_samp_file are in overall file coords (starting at 0)
        // min/max_samp_buf are in overall file coords (starting at 0)

        int file_start = 0;
        int file_end = hdr.getImageLineLength() - 4 - 1;

        // If we only want valid pixels, adjust the start/end based on the
        // valid pixel pointers at the start of the line

        if (_validPixelsOnly) {
            seekToLocation(pos);
            file_start = _di_hdr.readShort() - 1;
            file_end = _di_hdr.readShort() - 1;
            if (file_start == file_end)
                return;                 // no data on this line
        }

        int min_samp_file = file_start + hdr.getReferencePointOffsetPixels()
                            - _min_samp;
        int max_samp_file = file_end + hdr.getReferencePointOffsetPixels()
                            - _min_samp;

        int min_samp_buf = start;
        int max_samp_buf = start + length - 1;
        int new_offset = offset;

// System.out.println("min_samp_file="+min_samp_file+" max="+max_samp_file);
// System.out.println("min_samp_buf= "+min_samp_buf+" max="+max_samp_buf);
// System.out.println("file_start="+file_start+" end="+file_end);

        if (min_samp_file > max_samp_buf)
            return;                             // all before the image
        if (max_samp_file < min_samp_buf)
            return;                             // all after the image

        if (min_samp_buf < min_samp_file) {     // starting before image
            int delta = min_samp_file - min_samp_buf;
            min_samp_buf = min_samp_file;
            new_offset += delta;
        }
        if (min_samp_buf > min_samp_file) {     // starting past image
            int delta = min_samp_buf - min_samp_file;
            min_samp_file += delta;
            file_start += delta;
        }

        if (max_samp_buf > max_samp_file) {     // ending after image
            max_samp_buf = max_samp_file;
        }
        if (max_samp_buf < max_samp_file) {     // ending before image end
            int delta = max_samp_file - max_samp_buf;
            max_samp_file -= delta;
            file_end -= delta;
        }

        // add 4 to skip line prefix
        pos += 4 + file_start;

        int new_length = max_samp_file - min_samp_file + 1;
        if (new_length == 0)
            return;

// System.out.println("final min_samp_file="+min_samp_file+" max="+max_samp_file);
// System.out.println("final min_samp_buf= "+min_samp_buf+" max="+max_samp_buf);
// System.out.println("final file_start="+file_start+" end="+file_end);

	seekToLocation(pos);

	if (_system.getFormatCode() == SystemLabel.TYPE_BYTE) {
            _input_stream_wrap.readBytes(data, new_offset, new_length,
                                        pixelStride);
	}
	else 		//!!!! OTHER CASES: MAYBE DONE IN WRAPPER????!!!!
	    throw new UnsupportedOperationException("Data type conversions not implemented yet!!!!");
    }

}

