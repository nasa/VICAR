/**
 * Structure holding the header for a single Magellan Resampled record.
 *
 * @author Bob Deen, JPL
 */

package jpl.mipl.io.vicar;

import java.io.*;
import org.w3c.dom.*;

import javax.imageio.stream.*;
import jpl.mipl.io.vicar.*;

public class MgnResampHeader {

    // Fields for first entry (mapping params for this actual file).
    // Names come from resamp_magbidrs.f.
    protected double _orbitloc = 0.0;
    protected double _reflat = 0.0;
    protected double _reflon = 0.0;
    protected double _cornerlat = 0.0;
    protected double _cornerlon = 0.0;
    protected double _c1offset = 0.0;
    protected double _c2offset = 0.0;
    protected double _lineloc = 0.0;
    protected double _sampleloc = 0.0;
    protected double _sampspc = 0.0;
    protected double _linespc = 0.0;

    // Fields for second entry (the original mapping params for a resampled
    // file, or all 0's if it's not resampled).  Confusingly, in the .f
    // file, the above was "hdr2" and the below was "hdr1".  To reduce
    // confusion, I'm calling the below "orig" instead because it's the
    // original mapping before the resample.

    protected double _orig_orbitloc = 0.0;
    protected double _orig_reflat = 0.0;
    protected double _orig_reflon = 0.0;
    protected double _orig_cornerlat = 0.0;
    protected double _orig_cornerlon = 0.0;
    protected double _orig_c1offset = 0.0;
    protected double _orig_c2offset = 0.0;
    protected double _orig_lineloc = 0.0;
    protected double _orig_sampleloc = 0.0;
    protected double _orig_sampspc = 0.0;
    protected double _orig_linespc = 0.0;

    // Accessors.  We have no "set" functions because this is read-only

    /** Orbit number */
    public int getOrbitLoc() { return (int)(_orbitloc+0.5); }
    /** Reference latitude (deg) */
    public double getRefLat() { return _reflat; }
    /** Reference longitude (deg) */
    public double getRefLon() { return _reflon; }
    /** Corner latitude (deg) */
    public double getCornerLat() { return _cornerlat; }
    /** Corner longitude (deg) */
    public double getCornerLon() { return _cornerlon; }
    /** Line offset (c2???  .f code is contradictory) */
    public double getC1Offset() { return _c1offset; }
    /** Samp offset (c1???  .f code is contradictory) */
    public double getC2Offset() { return _c2offset; }
    /** Number of lines.  Note that NL seems to include the header so we
     *  subtract 1 */
    public int getNumLines() { return (int)(_lineloc+0.5 - 1); }
    /** Number of samples (each if 4-byte float) */
    public int getNumSamps() { return (int)(_sampleloc+0.5); }
    /** Sample spacing (m) */
    public double getSampSpacing() { return _sampspc; }
    /** Line spacing (m) */
    public double getLineSpacing() { return _linespc; }

    /** Original Orbit number */
    public int getOrigOrbitLoc() { return (int)(_orig_orbitloc+0.5); }
    /** Original Reference latitude (deg) */
    public double getOrigRefLat() { return _orig_reflat; }
    /** Original Reference longitude (deg) */
    public double getOrigRefLon() { return _orig_reflon; }
    /** Original Corner latitude (deg) */
    public double getOrigCornerLat() { return _orig_cornerlat; }
    /** Original Corner longitude (deg) */
    public double getOrigCornerLon() { return _orig_cornerlon; }
    /** Original Line offset (c2???  .f code is contradictory) */
    public double getOrigC1Offset() { return _orig_c1offset; }
    /** Original Samp offset (c1???  .f code is contradictory) */
    public double getOrigC2Offset() { return _orig_c2offset; }
    /** Original Number of lines */
    public int getOrigNumLines() { return (int)(_orig_lineloc+0.5); }
    /** Original Number of samples (each is 4-byte float) */
    public int getOrigNumSamps() { return (int)(_orig_sampleloc+0.5); }
    /** Original Sample spacing (m) */
    public double getOrigSampSpacing() { return _orig_sampspc; }
    /** Original Line spacing (m) */
    public double getOrigLineSpacing() { return _orig_linespc; }



/***********************************************************************
 * Constructor.  Reads the info from the file, as well.
 */
    public MgnResampHeader(DataInput di) throws IOException
    {
	readHeader(di);
    }

/***********************************************************************
 * Actually read the header.  Assumes we're given a DataInput object that
 * reads and translates the proper real/float formats properly (e.g. for
 * endianness)
 * <p>
 * Exceptions are simply returned, and abort the read.
 * <p>
 * No sanity checking is done, e.g. on the contents of the header or types.
 * What you read is what you get.
 */
    public void readHeader(DataInput di) throws IOException
    {

        int pos = 0;
        _orbitloc = di.readDouble();
        pos += 2;
        _reflat = di.readDouble();
        pos += 2;
        _reflon = di.readDouble();
        pos += 2;
        _cornerlat = di.readDouble();
        pos += 2;
        _cornerlon = di.readDouble();
        pos += 2;
        _c1offset = di.readDouble();
        pos += 2;
        _c2offset = di.readDouble();
        pos += 2;
        _lineloc = di.readDouble();
        pos += 2;
        _sampleloc = di.readDouble();
        pos += 2;
        int ns = getNumSamps();
        if (pos > ns-2) return;
        _sampspc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _linespc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;

        _orig_orbitloc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_reflat = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_reflon = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_cornerlat = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_cornerlon = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_c1offset = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_c2offset = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_lineloc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_sampleloc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_sampspc = di.readDouble();
        pos += 2;
        if (pos > ns-2) return;
        _orig_linespc = di.readDouble();

    }

/***********************************************************************
 * Dump the contents, primarily for debugging
 */
    public void print(PrintStream s)
    {

        s.println("Primary entry");
	s.println("  Orbit Number: " + _orbitloc);
        s.println("  Reference Latitude (deg): " + _reflat);
        s.println("  Reference Longitude (deg): " + _reflon);
        s.println("  Corner latitude (deg): " + _cornerlat);
        s.println("  Corner longitude (deg): " + _cornerlon);
        s.println("  Line offset (c2?): " + _c1offset);
        s.println("  Samp offset (c1?): " + _c2offset);
        s.println("  Number of Lines: " + _lineloc);
        s.println("  Number of Samps: " + _sampleloc);
        s.println("  Sample Spacing (m): " + _sampspc);
        s.println("  Line Spacing (m): " + _linespc);

        s.println("Second (original) entry");
	s.println("  Orbit Number: " + _orig_orbitloc);
        s.println("  Reference Latitude (deg): " + _orig_reflat);
        s.println("  Reference Longitude (deg): " + _orig_reflon);
        s.println("  Corner latitude (deg): " + _orig_cornerlat);
        s.println("  Corner longitude (deg): " + _orig_cornerlon);
        s.println("  Line offset (c2?): " + _orig_c1offset);
        s.println("  Samp offset (c1?): " + _orig_c2offset);
        s.println("  Number of Lines: " + _orig_lineloc);
        s.println("  Number of Samps: " + _orig_sampleloc);
        s.println("  Sample Spacing (m): " + _orig_sampspc);
        s.println("  Line Spacing (m): " + _orig_linespc);

    }

/***********************************************************************
 * Build the XML representation of this header into the supplied Document.
 * <p>
 * The XML is very simple:
 * <pre>
 * <mgn_resamp_header>
 *   <primary_header>
 *     <item key="orbit">850.0</item>
 *     <item key="ref_lat">15.553123</item>
 *     ...
 *   </primary_header>
 *   <secondary_header>
 *     <item key="orbit">850.0</item>
 *     <item key="ref_lat">15.553123</item>
 *     ...
 *   </secondary_header>
 * </mgn_resamp_header>
 * </pre>
 */

    public Element buildDom(Document doc)
    {
	Element item;

	Element root = (Element)doc.createElement("mgn_resamp_header");

	Element primary = doc.createElement("primary_header");
	root.appendChild(primary);


	item = doc.createElement("item");
	item.setAttribute("key", "orbit");
	item.appendChild(doc.createTextNode(Double.toString(_orbitloc)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "ref_lat");
	item.appendChild(doc.createTextNode(Double.toString(_reflat)));
	primary.appendChild(item);

        item = doc.createElement("item");
	item.setAttribute("key", "ref_lon");
	item.appendChild(doc.createTextNode(Double.toString(_reflon)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "oorner_let");
	item.appendChild(doc.createTextNode(Double.toString(_cornerlat)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "corner_lon");
	item.appendChild(doc.createTextNode(Double.toString(_cornerlon)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "c1_offset");
	item.appendChild(doc.createTextNode(Double.toString(_c1offset)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "c2_offset");
	item.appendChild(doc.createTextNode(Double.toString(_c2offset)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "num_lines");
	item.appendChild(doc.createTextNode(Double.toString(_lineloc)));
	primary.appendChild(item);

        item = doc.createElement("item");
	item.setAttribute("key", "num_samps");
	item.appendChild(doc.createTextNode(Double.toString(_sampleloc)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "samp_spc");
	item.appendChild(doc.createTextNode(Double.toString(_sampspc)));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "line_spc");
	item.appendChild(doc.createTextNode(Double.toString(_linespc)));
	primary.appendChild(item);


	Element secondary = doc.createElement("secondary_header");
	root.appendChild(secondary);


	item = doc.createElement("item");
	item.setAttribute("key", "orbit");
	item.appendChild(doc.createTextNode(Double.toString(_orig_orbitloc)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "ref_lat");
	item.appendChild(doc.createTextNode(Double.toString(_orig_reflat)));
	secondary.appendChild(item);

        item = doc.createElement("item");
	item.setAttribute("key", "ref_lon");
	item.appendChild(doc.createTextNode(Double.toString(_orig_reflon)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "oorner_let");
	item.appendChild(doc.createTextNode(Double.toString(_orig_cornerlat)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "corner_lon");
	item.appendChild(doc.createTextNode(Double.toString(_orig_cornerlon)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "c1_offset");
	item.appendChild(doc.createTextNode(Double.toString(_orig_c1offset)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "c2_offset");
	item.appendChild(doc.createTextNode(Double.toString(_orig_c2offset)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "num_lines");
	item.appendChild(doc.createTextNode(Double.toString(_orig_lineloc)));
	secondary.appendChild(item);

        item = doc.createElement("item");
	item.setAttribute("key", "num_samps");
	item.appendChild(doc.createTextNode(Double.toString(_orig_sampleloc)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "samp_spc");
	item.appendChild(doc.createTextNode(Double.toString(_orig_sampspc)));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "line_spc");
	item.appendChild(doc.createTextNode(Double.toString(_orig_linespc)));
	secondary.appendChild(item);



        return root;
    }

}

