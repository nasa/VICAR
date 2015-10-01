/**
 * Structure holding the header for the Magellan F-BIDR.  Includes code to
 * read the contents from the actual F-BIDR.
 *
 * @author Bob Deen, JPL
 */

package jpl.mipl.io.vicar;

import java.io.*;
import org.w3c.dom.*;

public class MgnFbidrHeader {

    // Primary Header fields
    protected String _header;
    protected String _logicalRecordSize;

    // Secondary Header fields
    protected int _secondaryLabelType;
    protected int _secondaryLabelLength;
    protected int _orbitNumber;
    protected int _dataClass;
    protected int _dataAnnotationLength;

    // Data Annotation Label
    protected int _imageLineCount;
    protected int _imageLineLength;
    protected float _projectionOriginLatitude;
    protected float _projectionOriginLongitude;
    protected float _referencePointLatitude;
    protected float _referencePointLongitude;
    protected int _referencePointOffsetLines;
    protected int _referencePointOffsetPixels;
    protected int _burstCounter;
    protected String _navUniqueID;

    // Accessors.  We have no "set" functions because this is read-only

    /** Primary header field */
    public String getHeader() { return _header; }
    /** Primary header field */
    public String getLogicalRecordSize() { return _logicalRecordSize; }

    /** Secondary header field */
    public int getSecondaryLabelType() { return _secondaryLabelType; }
    /** Secondary header field */
    public int getSecondaryLabelLength() { return _secondaryLabelLength; }
    /** Secondary header field */
    public int getOrbitNumber() { return _orbitNumber; }
    /** Secondary header field */
    public int getDataClass() { return _dataClass; }
    /** Secondary header field */
    public int getDataAnnotationLength() { return _dataAnnotationLength; }

    /** Data annotation label */
    public int getImageLineCount() { return _imageLineCount; }
    /** Data annotation label */
    public int getImageLineLength() { return _imageLineLength; }
    /** Data annotation label */
    public float getProjectionOriginLatitude() { return _projectionOriginLatitude; }
    /** Data annotation label */
    public float getProjectionOriginLongitude() { return _projectionOriginLongitude; }
    /** Data annotation label */
    public float getReferencePointLatitude() { return _referencePointLatitude; }
    /** Data annotation label */
    public float getReferencePointLongitude() { return _referencePointLongitude; }
    /** Data annotation label */
    public int getReferencePointOffsetLines() { return _referencePointOffsetLines; }
    /** Data annotation label */
    public int getReferencePointOffsetPixels() { return _referencePointOffsetPixels; }
    /** Data annotation label */
    public int getBurstCounter() { return _burstCounter; }
    /** Data annotation label */
    public String getNavUniqueID() { return _navUniqueID; }

/***********************************************************************
 * Constructor.  Reads the info from the file, as well.
 */
    public MgnFbidrHeader(DataInput di) throws IOException
    {
	readHeader(di);
    }

/***********************************************************************
 * Actually read the header.  Assumes we're given a DataInput object that
 * reads and translates the proper VAX/VMS real/float formats (e.g.
 * the return from VicarDataFormat.getDataInputWrapper()).
 * <p>
 * Exceptions are simply returned, and abort the read.
 * <p>
 * No sanity checking is done, e.g. on the contents of the header or types.
 * What you read is what you get.
 */
    public void readHeader(DataInput di) throws IOException
    {
	// DI does not have a "read n bytes" so we do the strings the hard way.

	// Primary header fields

	byte hdr[] = new byte[12];
	for (int i=0; i < 12; i++) {
	    hdr[i] = di.readByte();
	}
	_header = new String(hdr);

	byte sz[] = new byte[8];
	for (int i=0; i < 8; i++) {
	    sz[i] = di.readByte();
	}
	_logicalRecordSize = new String(sz);

	// Secondary Header fields

	_secondaryLabelType = di.readUnsignedShort();
	_secondaryLabelLength = di.readUnsignedShort();
	_orbitNumber = di.readUnsignedShort();
	_dataClass = di.readByte();
	_dataAnnotationLength = di.readByte();

	// Data Annotation Label

	_imageLineCount = di.readUnsignedShort();
	_imageLineLength = di.readUnsignedShort();
	_projectionOriginLatitude = di.readFloat();
	_projectionOriginLongitude = di.readFloat();
	_referencePointLatitude = di.readFloat();
	_referencePointLongitude = di.readFloat();
	_referencePointOffsetLines = di.readInt();
	_referencePointOffsetPixels = di.readInt();
	_burstCounter = di.readInt();		//!!!! unsigned, technically

	byte id[] = new byte[32];
	for (int i=0; i < 32; i++) {
	    id[i] = di.readByte();
	}
	_navUniqueID = new String(id);

    }

/***********************************************************************
 * Dump the contents, primarily for debugging
 */
    public void print(PrintStream s)
    {
	s.println("Primary Header:");
	s.println("  Header: " + _header);
	s.println("  Logical Record Size: " + _logicalRecordSize);

	s.println("Secondary Header:");
	s.println("  Secondary Label Type: " + _secondaryLabelType);
	s.println("  Secondary Label Length: " + _secondaryLabelLength);
	s.println("  Orbit Number: " + _orbitNumber);
	s.println("  Data Class: " + _dataClass);
	s.println("  Data Annotation Length: " + _dataAnnotationLength);

	s.println("Data Annotation Label:");
	s.println("  Image Line Count: " + _imageLineCount);
	s.println("  Image Line Length: " + _imageLineLength);
	s.println("  Projection Origin Latitude: " + _projectionOriginLatitude);
	s.println("  Projection Origin Longitude: " + _projectionOriginLongitude);
	s.println("  Reference Point Latitude: " + _referencePointLatitude);
	s.println("  Reference Point Longitude: " + _referencePointLongitude);
	s.println("  Reference Point Offset Lines: " + _referencePointOffsetLines);
	s.println("  Reference Point Offset Pixels: " + _referencePointOffsetPixels);
	s.println("  Burst Counter: " + _burstCounter);
	s.println("  Nav Unique ID: " + _navUniqueID);
    }

/***********************************************************************
 * Build the XML representation of this header into the supplied Document.
 * <p>
 * The XML is very simple:
 * <pre>
 * <mgn_fbidr_header>
 *   <primary_header>
 *     <item key="header">NJPLI000104</item>
 *     <item key="logicalRecordSize">26904</item>
 *   </primary_header>
 *   <secondary_header>
 *     <item key="secondaryLabelType">2</item>
 *     ...
 *   </secondary_header>
 *   <data_annotation_label>
 *     <item key="imageLineCount">52</item>
 *     ...
 *   </data_annotation_label>
 * </mgn_fbidr_header>
 * </pre>
 */

    public void buildDom(Document doc)
    {
	Element item;

	Element root = (Element)doc.createElement("mgn_fbidr_header");
	doc.appendChild(root);

	Element primary = doc.createElement("primary_header");
	root.appendChild(primary);

	item = doc.createElement("item");
	item.setAttribute("key", "header");
	item.appendChild(doc.createTextNode(_header));
	primary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "logicalRecordSize");
	item.appendChild(doc.createTextNode(_logicalRecordSize));
	primary.appendChild(item);

	Element secondary = doc.createElement("secondary_header");
	root.appendChild(secondary);

	item = doc.createElement("item");
	item.setAttribute("key", "secondaryLabelType");
	item.appendChild(doc.createTextNode(""+_secondaryLabelType));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "secondaryLabelLength");
	item.appendChild(doc.createTextNode(""+_secondaryLabelLength));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "orbitNumber");
	item.appendChild(doc.createTextNode(""+_orbitNumber));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "dataClass");
	item.appendChild(doc.createTextNode(""+_dataClass));
	secondary.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "dataAnnotationLength");
	item.appendChild(doc.createTextNode(""+_dataAnnotationLength));
	secondary.appendChild(item);

	Element annot = doc.createElement("data_annotation_label");
	root.appendChild(annot);

	item = doc.createElement("item");
	item.setAttribute("key", "imageLineCount");
	item.appendChild(doc.createTextNode(""+_imageLineCount));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "imageLineLength");
	item.appendChild(doc.createTextNode(""+_imageLineLength));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "projectionOriginLatitude");
	item.appendChild(doc.createTextNode(""+_projectionOriginLatitude));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "projectionOriginLongitude");
	item.appendChild(doc.createTextNode(""+_projectionOriginLongitude));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "referencePointLatitude");
	item.appendChild(doc.createTextNode(""+_referencePointLatitude));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "referencePointLongitude");
	item.appendChild(doc.createTextNode(""+_referencePointLongitude));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "referencePointOffsetLines");
	item.appendChild(doc.createTextNode(""+_referencePointOffsetLines));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "referencePointOffsetPixels");
	item.appendChild(doc.createTextNode(""+_referencePointOffsetPixels));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "burstCounter");
	item.appendChild(doc.createTextNode(""+_burstCounter));
	annot.appendChild(item);

	item = doc.createElement("item");
	item.setAttribute("key", "navUniqueID");
	item.appendChild(doc.createTextNode(_navUniqueID));
	annot.appendChild(item);
    }

}

