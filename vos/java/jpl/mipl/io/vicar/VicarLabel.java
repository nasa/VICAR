package jpl.mipl.io.vicar;

import jpl.mipl.io.streams.*;
import java.io.*;
import java.util.*;
import java.text.SimpleDateFormat;
import org.w3c.dom.*;

/**
 * This class maintains a VICAR label in various data structures, and provides
 * access to it.
 * <p>
 * The LBLSIZE keyword is automatically maintained and is <em>not</em>
 * included in the item lists (i.e. an iterator through the system label
 * will not see it).
 * <p>
 * No synchronization is performed here; such must be provided by the caller
 * if necessary.  This especially applies to readLabelChunk() if there might
 * be multiple threads reading the stream.
 * <p>
 * The <code>createHistoryTask()</code> function is here as a convenience;
 * it adds a history label with the current user and date/time to the label.
 * This routine should be used rather than adding a history task manually,
 * when possible, so details of formatting the USER and DAT_TIM labels can
 * remain hidden.
 */

public class VicarLabel implements Cloneable, Serializable
{
    /** Amount of space to reserve for LBLSIZE= keyword */
    protected static final int LBLSIZE_SPACE = 24;

    /** System label.
     * @serial */
    protected VicarLabelSet _system;
    /** Property label.
     * @serial */
    protected VicarLabelCategory _property;
    /** History label.
     * @serial */
    protected VicarLabelCategory _history;

    /** The current set being read.  Saved in the object only to handle EOL.
     * @serial */
    protected VicarLabelSet _currentSet;

    /** Was the read complete, or is there EOL left?
     * @serial */
    protected boolean _readComplete;

/***********************************************************************
 * Creates a VicarLabel object.  No label contents are present; other methods
 * must be called to fill it up.
 */
    public VicarLabel()
    {
	_system = new VicarLabelSet(VicarLabelSet.LABEL_SYSTEM);
	_property = new VicarLabelCategory(VicarLabelSet.LABEL_PROPERTY);
	_history = new VicarLabelCategory(VicarLabelSet.LABEL_HISTORY);

	_currentSet = _system;

	_readComplete = false;
    }

/***********************************************************************
 * Creates a new VicarLabel object from an XML DOM &lt;vicar_label&gt; node.
 * See <code>fromXML()</code> for more information (the arguments are
 * identical).
 * @see #fromXML(Element, List)
 */
    public VicarLabel(Element dom, List errorList)
    {
	this();
	fromXML(dom, errorList);
    }

/***********************************************************************
 * Get the System label set.  This may be modified if desired.
 */
    public VicarLabelSet getSystem()
    {
	return _system;
    }

/***********************************************************************
 * Get the Property label category.  This may be modified if desired.
 */
    public VicarLabelCategory getProperty()
    {
	return _property;
    }

/***********************************************************************
 * Get the History label category.  This may be modified if desired.
 */
    public VicarLabelCategory getHistory()
    {
	return _history;
    }

/***********************************************************************
 * Read a chunk of a label, either the main chunk at the beginning of the
 * file, or the EOL labels.  Label chunk must start with LBLSIZE keyword.
 * @return the value of the LBLSIZE keyword (which is removed, i.e. LBLSIZE
 * is not included in the label structures).
 * @throws IOException
 * @throws VicarLabelSyntaxException
 */
    public int readLabelChunk(InputStream s)
			throws IOException, VicarLabelSyntaxException
    {
	VicarLabelItem item;
	FixedLengthInputStream fls = new FixedLengthInputStream(s);
	VicarLabelParser p = new VicarLabelParser(fls);

	// Get the LBLSIZE keyword

	fls.setMaximumSize(LBLSIZE_SPACE);
	item = p.getNextLabel();
	if (item == null || !item.equals("LBLSIZE"))
	    throw new VicarLabelSyntaxException(
			"Main and EOL labels must start with LBLSIZE keyword");

	int lblsize = item.getInteger();

	// Set the stream to stop when LBLSIZE is reached

	fls.setMaximumSize(lblsize);

	// Now loop through the label and add things to the list

	if (_currentSet == null)
	    _currentSet = _system;	// precaution -- should never happen

	while ((item = p.getNextLabel()) != null) {
	    if (item.equals("PROPERTY")) {	// new property set
		_currentSet = new VicarLabelSet(VicarLabelSet.LABEL_PROPERTY,
							item.getString());
		_property.add(_currentSet);
	    }
	    else if (item.equals("TASK")) {	// new history task
		_currentSet = new VicarLabelSet(VicarLabelSet.LABEL_HISTORY,
							item.getString());
		_history.add(_currentSet);
	    }
	    else {			// Normal label, add to current set
		_currentSet.add(item);
	    }
	}

	return lblsize;
    }

/***********************************************************************
 * Read a chunk of a label from an object, which can be either
 * <code>InputStream</code> or <code>DataInput</code>.  A
 * <code>DataInput</code> (that's not also a stream) is simply wrapped into
 * a stream and is thus not recommended unless you have no other choice.
 * Unfortunately, <code>ImageInputStream</code> has no other choice.
 * @throws IOException
 * @throws VicarLabelSyntaxException
 * @throws IllegalArgumentException if the object is not an
 * <code>InputStream</code> or <code>DataInput</code>.
 * @see #readLabelChunk(InputStream)
 */
    public int readLabelChunk(Object obj)
			throws IOException, VicarLabelSyntaxException
    {
	if (obj instanceof InputStream)
	    return readLabelChunk((InputStream)obj);
	if (obj instanceof DataInput)
	    return readLabelChunk(new DataInputStreamWrapper((DataInput)obj));
	throw new IllegalArgumentException(
		"readLabelChunk requires an InputStream or DataInput object");
    }

/***********************************************************************
 * Boolean property that indicates whether or not the label has been
 * completely read.  Incomplete read might be due to EOL labels on a
 * sequential file.  This flag must be maintained externally to this class.
 */
    public boolean isReadComplete() { return _readComplete; }
    public void setReadComplete(boolean b) { _readComplete = b; }

/***********************************************************************
 * Throwaway class used only in <code>VicarLabel.toLabelString</code>
 * to return various information.  Only the lblsize and isComplete fields
 * should normally be accessed by the caller.
 * @see VicarLabelItem#toLabelString
 * @see VicarLabelCategory.ItemIndex
 */
    public class ItemPos
    {
	/** The size of the label that was written */
	public int lblsize;
	/** True if the entire label was written */
	public boolean isComplete;
	/** Which section of the label contains the next one to write */
	public int flavor;
	/** Where in that section do we find the next label to write */
	public VicarLabelCategory.ItemIndex index;
    }

/***********************************************************************
 * Write a chunk of a label, either the main chunk at the beginning of the
 * file, or the EOL labels.  LBLSIZE is automatically filled in by this
 * routine, and will be a multiple of the <code>recsize</code> argument.
 * The output stream will be padded with null's ("0" bytes) up to the next
 * multiple of "recsize" (or the value of <code>max_length</code>, whichever
 * is larger).
 * <p>
 * The EOL label value is managed by this routine.  It will be set to 0 on
 * default.  If the entire label won't fit within the <code>max_length</code>
 * space, EOL will be reset to 1 and the label re-generated (the label is
 * written to the stream only once).  Note that the value of EOL is modified
 * even if we are writing the EOL labels.  It is only written to the stream,
 * however, if it is within the range specified by the <code>start_pos</code>
 * argument.
 * <p>
 * Note that if any non-single-byte characters are in the label, the label
 * will not be of the correct size.  This is illegal for VICAR anyway.
 * <p>
 * @param max_length If non-zero, it represents the maximum size the label
 * can become (normally used to make sure the label fits in the space at the
 * front of a file).  This must be a multiple of <code>recsize</code>.  The
 * output stream is padded with 0's out to this length (i.e. exactly this
 * much is always written).  If zero, there is no maximum and the label will
 * be padded to a multiple of <code>recsize</code>.  (LBLSIZE is set the
 * same as the padding amount).
 *
 * @param recsize The size of a "record" in the VICAR file.  By definition of
 * the file format, label areas, and the LBLSIZE item, must always be a
 * multiple of this value.
 *
 * @param start_pos A VicarLabel.ItemPos structure containing information
 * about where to start writing the label.  This structure should generally
 * be considered opaque and be simply passed in from the return of the
 * previous call (see the return value description).  It may be passed in as
 * null to start at the beginning of the label.
 *
 * @return A VicarLabel.ItemPos structure containing information
 * about where in the label we stopped writing, and what LBLSIZE is.  The
 * <code>lblsize</code> field of this structure contains the LBLSIZE that
 * was written as part of this chunk.  The <code>isComplete</code> field
 * indicates whether the entire label was written.  Although the rest of
 * the field members are public, they should be treated as opaque and the
 * structure simply passed in to the next call to this routine.
 *
 * @throws IOException if there is a problem writing to the stream.
 * @throws IllegalArgumentException if recsize <= 0 or max_length > 0 and
 * is not a multiple of recsize.  This is also thrown if max_length is not
 * big enough to contain the entire system label (if the system label is
 * being written), if EOL is needed but not found, or if EOL is needed but
 * the System label wasn't written.  Finally, it is thrown if non-ASCII
 * characters are present in the final label string.
 */
    public VicarLabel.ItemPos writeLabelChunk(OutputStream os,
			int max_length, int recsize,
			VicarLabel.ItemPos start_pos)
		throws IOException
    {
	VicarLabel.ItemPos pos = new VicarLabel.ItemPos();
	pos.index = null;
	int start_flavor = VicarLabelSet.LABEL_SYSTEM;
	if (start_pos != null) {
	    pos.index = start_pos.index;
	    start_flavor = start_pos.flavor;
	}

	if (recsize <= 0) {
	    throw new IllegalArgumentException(
		"recsize must be positive in VicarLabel.writeLabelChunk");
	}
	if (max_length > 0 && max_length % recsize != 0) {
	    throw new IllegalArgumentException(
		"max_length must be a multiple of recsize in VicarLabel.writeLabelChunk");
	}

	StringBuffer buf = new StringBuffer(max_length>0 ? max_length : 1000);

	// Prime the buffer with 24 characters to leave room for LBLSIZE.
	// 24 matches strlen(SYSTEM_KEY_TEMPLATE) in $V2INC/defines.h.

	buf.append("LBLSIZE=                ");

	// Set EOL to 0.  We'll reset it to 1 if needed later

	_system.add(new VicarLabelItem("EOL", 0));
	pos.isComplete = true;

	// Write system label

	boolean active = false;
	if (start_flavor == VicarLabelSet.LABEL_SYSTEM) {
	    active = true;
	    if (pos.index != null && pos.index.item_index != 0) {
		throw new IllegalArgumentException(
			"Entire system label must be written at once in VicarLabel.writeLabelChunk; input item index is invalid");
	    }
	    int last_item = _system.toLabelString(buf, max_length, 0);
	    if (last_item != -1) {
		throw new IllegalArgumentException(
			"max_length must be big enough to hold the entire System label in VicarLabel.writeLabelChunk");
	    }
	    pos.index = null;		// start from beginning of next
	}

	// Write property label

	if (start_flavor == VicarLabelSet.LABEL_PROPERTY || active) {
	    active = true;

	    pos.index = _property.toLabelString(buf, max_length, pos.index);

	    if (pos.index != null) {		// entire label not written
		pos.flavor = VicarLabelSet.LABEL_PROPERTY;
		active = false;
	    }
	}

	// Write history label

	if (start_flavor == VicarLabelSet.LABEL_HISTORY || active) {
	    active = true;

	    pos.index = _history.toLabelString(buf, max_length, pos.index);

	    if (pos.index != null) {		// entire label not written
		pos.flavor = VicarLabelSet.LABEL_HISTORY;
		active = false;
	    }
	}

	// If we didn't write everything, reset EOL

	if (!active) {
	    if (start_flavor != VicarLabelSet.LABEL_SYSTEM) {
		throw new IllegalArgumentException(
		    "length limits cannot be applied if System label not written in VicarLabel.writeLabelChunk");
	    }
	    int eol_pos = buf.toString().indexOf("EOL=0");
	    if (eol_pos < 0) {
		throw new IllegalArgumentException(
		    "EOL needed but not found in VicarLabel.writeLabelChunk");
	    }
	    VicarLabelItem eol = new VicarLabelItem("EOL", 1);
	    String eol_str = eol.toLabelString();
	    buf.replace(eol_pos, eol_pos+eol_str.length(), eol_str);
	    _system.add(eol);
	    pos.isComplete = false;
	}

	// Pad the buffer to the required max length or multiple of recsize

	if (max_length > 0) {
	    while (buf.length() < max_length)
		buf.append('\u0000');
	}
	else {
	    while ((buf.length() % recsize) != 0)
		buf.append('\u0000');
	}

	// Finally, set up LBLSIZE.  We intentionally left room for it, above.

	pos.lblsize = buf.length();

	String lblsize =
		(new VicarLabelItem("LBLSIZE", pos.lblsize)).toLabelString();
	buf.replace(0, lblsize.length(), lblsize);

	// At last, write the sucker to the stream!!

	byte[] bytes = buf.toString().getBytes();
	if (bytes.length != pos.lblsize) {
	    throw new IllegalArgumentException(
		"non-ASCII characters present in label, in VicarLabel.writeLabelChunk");
	}
	os.write(bytes);

	return pos;
    }

/***********************************************************************
 * Write a chunk of a label to an object, which can be either
 * <code>OutputStream</code> or <code>DataOutput</code>.  A
 * <code>DataOutput</code> (that's not also a stream) is simply wrapped into
 * a stream and is thus not recommended unless you have no other choice.
 * Unfortunately, <code>ImageOutputStream</code> has no other choice.
 * @throws IOException
 * @throws IllegalArgumentException under the same circumstances as the
 * <code>OutputStream</code> version.
 * @see #writeLabelChunk(OutputStream, int, int, VicarLabel.ItemPos)
 */
    public VicarLabel.ItemPos writeLabelChunk(Object obj,
			int max_length, int recsize,
			VicarLabel.ItemPos start_pos)
		throws IOException
    {
	if (obj instanceof OutputStream)
	    return writeLabelChunk((OutputStream)obj,
					max_length, recsize, start_pos);
	if (obj instanceof DataOutput)
	    return writeLabelChunk(new DataOutputStreamWrapper((DataOutput)obj),
					max_length, recsize, start_pos);
	throw new IllegalArgumentException(
		"writeLabelChunk requires an OutputStream or DataOutput object");
    }

/***********************************************************************
 * Returns a human-readable representation of all parts of the label,
 * including all the contents.  This is equivalent to label-list, except
 * the system labels are dumped rather than formatted.
 */
    public String toString()
    {
	StringBuffer buf = new StringBuffer(1000);

	buf.append(_system.toString());
	buf.append(_property.toString());
	buf.append(_history.toString());

	return buf.toString();
    }

/***********************************************************************
 * Reads the entire label from an XML DOM.  The supplied Element must be
 * a &lt;vicar_label&gt;.
 * <p>
 * Like the <code>fromXML()</code> in the subordinate classes, this routine 
 * will augment an existing label with the contents of the XML. Any sets in 
 * the XML are added to the end of their respective category.  If you want 
 * only the XML items, make sure the <code>VicarLabel</code> is empty before 
 * calling this function.
 * <p>
 * If anything is wrong with the supplied node, error/warning messages
 * will be appended to the supplied <code>errorList</code>.  However, the
 * parser keeps working as long as possible, despite errors.  The practical
 * upshot of this is that the errors can usually be ignored; the parser will
 * do the best it can.  However, the errors can be displayed to the user
 * if desired.  An exception will be thrown only if something unrecoverable
 * happens.
 * <p>
 * @param dom The &lt;vicar_label&gt; <code>Element</code> from which to
 * extract the labels.
 * @param errorList A list of <code>String</code>s to which any parsing
 * errors will be appended.  Can be <code>null</code>, in which case no
 * errors are returned.
 * @throws DOMException if something unrecoverable happens.  Improperly
 * formatted items or violations of the DTD do not necessarily result in
 * exceptions; parsing continues as long as possible and problems are appended
 * to the <code>errorList</code>.
 *
 * @see VicarLabelCategory#fromXML(Element, List)
 * @see VicarLabelSet#fromXML(Element, List)
 */
  //   public void fromXML(Element dom, List errorList)
  public void fromXML(Element dom, List errorList)
  // public void fromXML(Node dom, List errorList)
    {
	String tagName = dom.getTagName();
	// System.out.println("VicarLabel.fromXML() tagName "+tagName);
	
	if (errorList != null && !tagName.equalsIgnoreCase("vicar_label"))
	    errorList.add("Root element " + tagName + " should be of type <vicar_label>.");

	
	// The system label is not a Category so we must do it the hard way.

	NodeList sys = dom.getElementsByTagName("SYSTEM");
	if (sys == null || sys.getLength() <= 0) {
	    if (errorList != null)
		  errorList.add("System label is missing <system>");
		sys = dom.getElementsByTagName("system"); // try again
	} 
	
	if (sys == null || sys.getLength() <= 0) {
	    if (errorList != null)
		errorList.add("System label is missing");
	}
	else {
	    if (sys.getLength() > 1) {
		if (errorList != null)
		    errorList.add("More than one system label is present.  First one used.");
	    }
	    Node sys_node = sys.item(0);
	    if (!(sys_node instanceof Element)) {
		if (errorList != null)
		    errorList.add("<system> node is not a DOM Element type");
	    }
	    else {
	    	// System.out.println("VicarLabel.fromXML() (Element) sys_node "+((Element)sys_node).getTagName());
	    	// System.out.println("VicarLabel.fromXML() (Node)    sys_node "+sys_node.getNodeName());
	    	errorList.add("calling _system.fromXML <system>");
		_system.fromXML((Element)sys_node, errorList);
	    }
	}

	// These are categories, so they're easy

	// get the sets for property and history and pass them in
	// or have VicarLabelSets do It ???
	NodeList propList = dom.getElementsByTagName("PROPERTY");
	if (propList == null || propList.getLength() <= 0) {
	    if (errorList != null)
		  errorList.add("no properties available <PROPERTY>");
		propList = dom.getElementsByTagName("property"); // try again
	} 
	
	if (propList == null || propList.getLength() <= 0) {
	    if (errorList != null)
		errorList.add("no properties available <property>");
	}
	else {
		// System.out.println(" <PROPERTY> "+propList.getLength()+"  found");
		Node n;
		Element propEl;
		for (int j=0 ; j<propList.getLength() ; j++) {
			n = (Node) propList.item(j);
			String s = n.getNodeName();
			if (n instanceof Element) {
				// System.out.println(j+" <PROPERTY> "+((Element)n).getTagName()+" "+s);
				_property.fromXML((Element) n, errorList);
			} else {
				if (errorList != null)
					errorList.add(j+" <property> "+s+" is not an Element");
			}
			
		}
		// _property.fromXML(dom, errorList);
		// loop thru ?? 
		// _property.fromXML(property, errorList);
	}
	
	
		_history.fromXML(dom, errorList);
	

    }

/***********************************************************************
 * Returns the entire label as an XML DOM &lt;vicar_label&gt; node.
 * See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * @param document The <code>Document</code> which will contain the node.
 * Note that the returned node is <em>not</em> added to this document
 * automatically.
 * @see VicarLabelItem#toXML(Document)
 * @see VicarLabelSet#toXML(Document)
 */
    public Node toXML(Document doc)
    {
	Element lbl = doc.createElement("vicar_label");
	lbl.appendChild(_system.toXML(doc));
	_property.toXML(doc, lbl);
	_history.toXML(doc, lbl);

	return lbl;
    }

/***********************************************************************
 * Creates a deep copy of the VicarLabel.  Nothing can be done to the
 * returned copy that in any way affects the original.
 */
    public Object clone()
    {
	VicarLabel lbl;
	try {
	    lbl = (VicarLabel)super.clone();
	} catch (Exception e) {
	    return null;
	}

	lbl._system = (VicarLabelSet)lbl._system.clone();
	lbl._property = (VicarLabelCategory)lbl._property.clone();
	lbl._history = (VicarLabelCategory)lbl._history.clone();
	lbl._currentSet = (VicarLabelSet)lbl._currentSet.clone();

	return lbl;
    }

/***********************************************************************
 * Creates a history task at the end of the history label, given the
 * task name.  The date and user are derived from the system and
 * <code>user.name</code> system property.  The task's
 * <code>VicarLabelSet</code> is returned as a convenience, or the user can
 * simply get the last Set from the history Category (i.e. the returned Set
 * can be ignored; it is automatically included in the history Category).
 */
    public VicarLabelSet createHistoryTask(String task)
    {
	// Get the username

	String username;
	try {
	    username = System.getProperty("user.name");
	} catch (SecurityException e) {
	    username = "unknown";
	}
	if (username == null)
	    username = "unknown";

	// Get the date/time

	SimpleDateFormat formatter =
			new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy");
	String date = formatter.format(new Date());

	if (task == null)
	    task = "none";

	VicarLabelSet set = new VicarLabelSet(VicarLabelSet.LABEL_HISTORY,task);

	set.add(new VicarLabelItem("USER", username));
	set.add(new VicarLabelItem("DAT_TIM", date));

	_history.add(set);

	return set;
    }

}

