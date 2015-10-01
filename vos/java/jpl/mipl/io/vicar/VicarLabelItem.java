package jpl.mipl.io.vicar;

import java.util.*;
import java.util.ArrayList;		// just to make javadoc happy
import java.text.Collator;
import java.io.*;
import org.w3c.dom.*;
import jpl.mipl.io.util.*;

/**
 * This class maintains the key/value pair for a single VICAR label item,
 * and provides mechanisms to retrieve and modify the key and value.
 * <p>
 * Items are stored internally as strings in the format required for the
 * label (with one exception, see below)... but this fact is transparent
 * to the caller, who can use the int/float/double get/set methods as
 * appropriate.  The exception is strings... the VICAR standard specifies
 * that single quotes embedded in a string are doubled when written to the
 * file.  However, strings are stored internally with the doubles removed
 * (which are added again in <code>toLabelString</code>).  This is a slight
 * inconsistency of design, but should be transparent to the caller.
 * Note that <code>toString</code> itself converts to a more human-readable
 * format; <code>toLabelString</code> converts to a format suitable for
 * the label.
 * <p>
 * Access to this object is not synchronized in any way.  The caller must
 * synchronize if necessary.  <code>ArrayList</code>s are used instead of
 * <code>Vector</code>s for efficiency.
 * <p>
 * Note that per VICAR convention, keywords are always stored in upper case.
 * However, they use case-insensitive comparisons.
 */

public class VicarLabelItem implements Cloneable, Serializable
{

    /** Keyword.
     * @serial */
    protected String _key;

    /** Value.
     * @serial */
    protected List _value;

    /** Data type if known.
     * @serial */
    protected int _type = TYPE_UNSPECIFIED;

/***********************************************************************
 * These values are used to keep track of the "natural" data type of the
 * label item.  Although all items are stored internally as String, it is
 * often convenient to access them numerically.
 */
    public static final int TYPE_UNSPECIFIED = 0;
/** Integers may have only +-0123456789 */
    public static final int TYPE_INTEGER = 1;
/** Floats are integers plus .eEdD */
    public static final int TYPE_FLOAT = 2;
/** Doubles are not guessed at, but can be set or retrieved */
    public static final int TYPE_DOUBLE = 3;
/** Strings are anything else, or if they have quotes */
    public static final int TYPE_STRING = 4;

/***********************************************************************
 * The STRING representation of each type, for XML.
 */
    public static final String _typeStrings[] = {
	"UNSPECIFIED",
	"INTEGER",
	"FLOAT",
	"DOUBLE",
	"STRING"
    };

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Create an empty label item.  Must call <code>set</code> functions to make
 * this useful.
 */
    public VicarLabelItem()
    {
	_key = null;
	_value = new ArrayList(0);
	_type = TYPE_UNSPECIFIED;
    }

/***********************************************************************
 * Create a label item with keyword only.  Can be useful for equality
 * comparisons (which looks at keyword only, not value).
 */
    public VicarLabelItem(String key)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(0);
	_type = TYPE_UNSPECIFIED;
    }

/***********************************************************************
 * Create a label item from a single string
 */
    public VicarLabelItem(String key, String value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(1);
	_value.add(value);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Create a label item from an array of strings
 */
    public VicarLabelItem(String key, String[] value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(value[i]);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Create a label item from a single integer
 */
    public VicarLabelItem(String key, int value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_INTEGER;
    }

/***********************************************************************
 * Create a label item from an array of integers
 */
    public VicarLabelItem(String key, int[] value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_INTEGER;
    }

/***********************************************************************
 * Create a label item from a single float
 */
    public VicarLabelItem(String key, float value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_FLOAT;
    }

/***********************************************************************
 * Create a label item from an array of floats
 */
    public VicarLabelItem(String key, float[] value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_FLOAT;
    }

/***********************************************************************
 * Create a label item from a single double
 */
    public VicarLabelItem(String key, double value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_DOUBLE;
    }

/***********************************************************************
 * Create a label item from an array of doubles
 */
    public VicarLabelItem(String key, double[] value)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_DOUBLE;
    }

/***********************************************************************
 * Create a label item from a single string, but we don't necessarily
 * know the type, so "guess" at it.  This form is indended for use by
 * the label reader, not by applications (who should know their type!)
 * @param quote <code>true</code> if a quote was present (forces a string),
 * <code>false</code> if no quote (meaning we guess).
 */
    public VicarLabelItem(String key, String value, boolean quote)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(1);
	_value.add(value);
	if (quote)
	    _type = TYPE_STRING;
	else {
	    _type = TYPE_UNSPECIFIED;
	    guessType(value);
	}
    }

/***********************************************************************
 * Create a label item from an array of strings, but we don't know the type.
 * @see #VicarLabelItem(String, String, boolean)
 */
    public VicarLabelItem(String key, String[] value, boolean quote)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.length);
	if (quote)
	    _type = TYPE_STRING;
	else
	    _type = TYPE_UNSPECIFIED;

	for (int i=0; i < value.length; i++) {
	    _value.add(value[i]);
	    if (!quote)
		guessType(value[i]);
	}
    }

/***********************************************************************
 * Create a label item from a vector of strings, but we don't know the type.
 * @see #VicarLabelItem(String, String, boolean)
 */
    public VicarLabelItem(String key, List value, boolean quote)
    {
	_key = key.toUpperCase();
	_value = new ArrayList(value.size());
	if (quote)
	    _type = TYPE_STRING;
	else
	    _type = TYPE_UNSPECIFIED;

	for (int i=0; i < value.size(); i++) {
	    _value.add(value.get(i));
	    if (!quote)
		guessType((String)value.get(i));
	}
    }

/***********************************************************************
 * Create a label item from an XML Element.  See <code>fromXML()</code>
 * for more information (the arguments are identical).
 * @see #fromXML(Element, List)
 */
    public VicarLabelItem(Element item, List errorList)
    {
	this();
	fromXML(item, errorList);
    }

////////////////////////////////////////////////////////////////////////
// Access methods
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Get the keyword
 */
    public String getKeyword()
    {
	return _key;
    }

/***********************************************************************
 * Set the keyword
 */
    public void setKeyword(String key)
    {
	_key = key.toUpperCase();
    }

/***********************************************************************
 * Get the "natural" type.  Will return TYPE_UNSPECIFIED only if there are
 * no values.  There is no <code>setType</code> because that must be
 * consistent with the values themselves.
 */
    public int getType()
    {
	return _type;
    }

/***********************************************************************
 * Get the number of elements in the value.
 */
    public int getNumElements()
    {
	return _value.size();
    }

/***********************************************************************
 * Two VicarLabelItem objects are equal if their Keywords are equal.
 * The values are not considered.  This makes searches in VicarLabelSet
 * much easier.  We also test for equality of the keyword with a String.
 */
    public boolean equals(Object o)
    {
	if (o instanceof String)
	    return _key.equalsIgnoreCase((String)o);
	if (o instanceof VicarLabelItem)
	    return _key.equalsIgnoreCase(((VicarLabelItem)o).getKeyword());
	return false;
    }

////////////////////////////////////////////////////////////////////////
// Value retrieval
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Get the (first) value as a string.  Useful if there's only one value.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public String getString()
    {
	return (String)_value.get(0);
    }

/***********************************************************************
 * Get the (first) value as an integer.  If the type is not TYPE_INTEGER,
 * 0 is returned.  Useful if there's only one value.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public int getInteger()
    {
	return getIntegerFromString((String)_value.get(0));
    }

/***********************************************************************
 * Get the (first) value as a float.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, 0 is returned.  Useful if there's only one
 * value.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public float getFloat()
    {
	return getFloatFromString((String)_value.get(0));
    }

/***********************************************************************
 * Get the (first) value as a double.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, 0 is returned.  Useful if there's only one
 * value.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public double getDouble()
    {
	return getDoubleFromString((String)_value.get(0));
    }

/***********************************************************************
 * Get the given element as a string.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public String getString(int index)
    {
	return (String)_value.get(index);
    }

/***********************************************************************
 * Get the given element as an integer.  If the type is not TYPE_INTEGER,
 * 0 is returned.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public int getInteger(int index)
    {
	return getIntegerFromString((String)_value.get(index));
    }

/***********************************************************************
 * Get the given element as a float.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, 0 is returned.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public float getFloat(int index)
    {
	return getFloatFromString((String)_value.get(index));
    }

/***********************************************************************
 * Get the given element as a double.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, 0 is returned.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public double getDouble(int index)
    {
	return getDoubleFromString((String)_value.get(index));
    }

/***********************************************************************
 * Get the value as a string array.
 */
    public String[] getStringArray()
    {
	return (String[])_value.toArray(new String[_value.size()]);
    }

/***********************************************************************
 * Get the value as an integer array.  If the type is not TYPE_INTEGER,
 * an array full of 0's is returned.
 */
    public int[] getIntegerArray()
    {
	int[] array = new int[_value.size()];
	for (int i=0; i < _value.size(); i++)
	    array[i] = getIntegerFromString((String)_value.get(i));
	return array;
    }

/***********************************************************************
 * Get the value as a float array.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, an array full of 0's is returned.
 */
    public float[] getFloatArray()
    {
	float[] array = new float[_value.size()];
	for (int i=0; i < _value.size(); i++)
	    array[i] = getFloatFromString((String)_value.get(i));
	return array;
    }

/***********************************************************************
 * Get the value as a double array.  If the type is not TYPE_INTEGER,
 * TYPE_FLOAT, or TYPE_DOUBLE, an array full of 0's is returned.
 */
    public double[] getDoubleArray()
    {
	double[] array = new double[_value.size()];
	for (int i=0; i < _value.size(); i++)
	    array[i] = getDoubleFromString((String)_value.get(i));
	return array;
    }

////////////////////////////////////////////////////////////////////////
// Value modification
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Set the value from a string.  When done, the vector will have one and
 * only one element in it (any previous elements are discarded).
 */
    public void setValue(String value)
    {
	_value = new ArrayList(1);
	_value.add(value);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Set the value from an integer.  When done, the vector will have one and
 * only one element in it (any previous elements are discarded).
 */
    public void setValue(int value)
    {
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_INTEGER;
    }

/***********************************************************************
 * Set the value from a float.  When done, the vector will have one and
 * only one element in it (any previous elements are discarded).
 */
    public void setValue(float value)
    {
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_FLOAT;
    }

/***********************************************************************
 * Set the value from a double.  When done, the vector will have one and
 * only one element in it (any previous elements are discarded).
 */
    public void setValue(double value)
    {
	_value = new ArrayList(1);
	_value.add(getLabelString(value));
	_type = TYPE_DOUBLE;
    }

/***********************************************************************
 * Set the given element as a string.  Setting with an index of the current
 * size, or -1, will append the item to the end.  Elements other than the
 * indexed one are left unchanged.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void setValue(String value, int index)
    {
	if (index == -1 || index == _value.size())
	    _value.add(value);
	else
	    _value.set(index, value);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Set the given element as an integer.  Setting with an index of the current
 * size, or -1, will append the item to the end.  Elements other than the
 * indexed one are left unchanged.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void setValue(int value, int index)
    {
	if (index == -1 || index == _value.size())
	    _value.add(getLabelString(value));
	else
	    _value.set(index, getLabelString(value));
	checkConsistentType(TYPE_INTEGER);
    }

/***********************************************************************
 * Set the given element as a float.  Setting with an index of the current
 * size, or -1, will append the item to the end.  Elements other than the
 * indexed one are left unchanged.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void setValue(float value, int index)
    {
	if (index == -1 || index == _value.size())
	    _value.add(getLabelString(value));
	else
	    _value.set(index, getLabelString(value));
	checkConsistentType(TYPE_FLOAT);
    }

/***********************************************************************
 * Set the given element as a double.  Setting with an index of the current
 * size, or -1, will append the item to the end.  Elements other than the
 * indexed one are left unchanged.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void setValue(double value, int index)
    {
	if (index == -1 || index == _value.size())
	    _value.add(getLabelString(value));
	else
	    _value.set(index, getLabelString(value));
	checkConsistentType(TYPE_DOUBLE);
    }

/***********************************************************************
 * Set the entire value to a string array.  Any previous items are discarded.
 */
    public void setValue(String[] value)
    {
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(value[i]);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Set the entire value to an integer array.  Any previous items are discarded.
 */
    public void setValue(int[] value)
    {
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_INTEGER;
    }

/***********************************************************************
 * Set the entire value to a float array.  Any previous items are discarded.
 */
    public void setValue(float[] value)
    {
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_FLOAT;
    }

/***********************************************************************
 * Set the entire value to a double array.  Any previous items are discarded.
 */
    public void setValue(double[] value)
    {
	_value = new ArrayList(value.length);
	for (int i=0; i < value.length; i++)
	    _value.add(getLabelString(value[i]));
	_type = TYPE_DOUBLE;
    }

////////////////////////////////////////////////////////////////////////
// Structure modification
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Removes an element from the value list.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void remove(int index)
    {
	_value.remove(index);
    }

/***********************************************************************
 * Inserts a string at the given index.  The new element appears before
 * the element currently at that index.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @see ArrayList#add(int, Object)
 */
    public void insert(String value, int index)
    {
	_value.add(index, value);
	_type = TYPE_STRING;
    }

/***********************************************************************
 * Inserts an integer at the given index.  The new element appears before
 * the element currently at that index.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @see ArrayList#add(int, Object)
 */
    public void insert(int value, int index)
    {
	_value.add(index, getLabelString(value));
	checkConsistentType(TYPE_INTEGER);
    }

/***********************************************************************
 * Inserts a float at the given index.  The new element appears before
 * the element currently at that index.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @see ArrayList#add(int, Object)
 */
    public void insert(float value, int index)
    {
	_value.add(index, getLabelString(value));
	checkConsistentType(TYPE_FLOAT);
    }

/***********************************************************************
 * Inserts a double at the given index.  The new element appears before
 * the element currently at that index.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @see ArrayList#add(int, Object)
 */
    public void insert(double value, int index)
    {
	_value.add(index, getLabelString(value));
	checkConsistentType(TYPE_DOUBLE);
    }

/***********************************************************************
 * Returns the entire Item as a valid VICAR label string, ready to be
 * added to a label, in key=value form.  Specifically, quotes internal
 * to strings are doubled, per the VICAR standard.  (This is not necessary
 * to do for input because <code>VicarLabelParser</code> handles it).
 */
    public String toLabelString()
    {
	StringBuffer buf = new StringBuffer(50);
	buf.append(_key);
	buf.append("=");
	if (_value.size() <= 1)
	    buf.append(getLabelFormattedString((String)_value.get(0)));
	else {				// multivalued element
	    buf.append("(");
	    for (int i=0; i < _value.size(); i++) {
		if (i != 0)
		    buf.append(",");
		    
		buf.append(getLabelFormattedString((String)_value.get(i)));
		// buf.append(_value.get(i));
	    }
	    buf.append(")");
	}
	return buf.toString();
    }

/***********************************************************************
 * Returns the entire Item as a string, intended for human viewing.  This
 * string is <em>not</em> sufficient to be stuffed in an actual label; use
 * <code>toLabelString</code> for that.  The differences:  internal quotes
 * in strings are not doubled, and spaces are added for readability.
 * @see #toLabelString()
 */
    public String toString()
    {
	StringBuffer buf = new StringBuffer(50);
	buf.append(_key);
	buf.append(" = ");		// more spaces than toLabelString()
	if (_value.size() == 0) {
		// do nothing
		buf.append("NO_VALUE");
	}
	else if (_value.size() <= 1) {
	    if (_type == TYPE_STRING)
		buf.append("'");
		buf.append((String)_value.get(0));
	    if (_type == TYPE_STRING)
		buf.append("'");
	}
	else {				// multivalued element
	    buf.append("(");
	    for (int i=0; i < _value.size(); i++) {
		if (i != 0)
		    buf.append(", ");	// more spaces
		if (_type == TYPE_STRING)
		    buf.append("'");
		buf.append(_value.get(i));
		if (_type == TYPE_STRING)
		    buf.append("'");
	    }
	    buf.append(")");
	}
	return buf.toString();
    }

/***********************************************************************
 * Returns the entire Item as an XML DOM &lt;item&gt; node.  Subnodes will be
 * created for multivalued Items as necessary.  See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * @param document The <code>Document</code> which will contain the node.
 * @see #toLabelString()
 */
    public Node toXML(Document doc)
    {
	// Create the ITEM node, and all the standard attributes

	Element item = doc.createElement("item");
	item.setAttribute("key", _key);
	if (_type != TYPE_UNSPECIFIED)	// no attr if type unknown
	    item.setAttribute("type", _typeStrings[_type]);

	// Check for single or multivalued label value

	if (_value.size() <= 1) {

	    //!!!! HANDLE QUOTES !!!!
	    item.setAttribute("value", (String)_value.get(0));
	}
	else {				// multivalued element
	    int n = _value.size();

	    for (int i=0; i < n; i++) {

		Element sub = doc.createElement("subitem");
		//!!!! HANDLE QUOTES !!!!
		sub.setAttribute("value", (String)_value.get(i));
		item.appendChild(sub);
	    }
	}

	return item;
    }

/***********************************************************************
 * Sets the entire Item (key and value) based on the supplied XML DOM
 * &lt;item&gt; node.  See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * <p>
 * If anything is wrong with the supplied node, error/warning messages
 * will be appended to the supplied <code>errorList</code>.  However, the
 * parser keeps working as long as possible, despite errors.  The practical
 * upshot of this is that the errors can usually be ignored; the parser will
 * do the best it can.  However, the errors can be displayed to the user
 * if desired.  An exception will be thrown only if something unrecoverable
 * happens.  A missing keyword will default to "UNKNOWN".
 *
 * @param item The <code>Element</code> from which to extract the item info.
 * @param errorList A list of <code>String</code>s to which any parsing
 * errors will be appended.  Can be <code>null</code>, in which case no
 * errors are returned.
 * @throws DOMException if something unrecoverable happens.  Improperly
 * formatted items or violations of the DTD do not necessarily result in
 * exceptions; parsing continues as long as possible and problems are appended
 * to the <code>errorList</code>.
 */
    public void fromXML(Element item, List errorList)
    {
	// Make sure it's an ITEM node
	DOMutils domUtils = new DOMutils();

	if (errorList != null) {
	    if (!item.getTagName().equals("item"))
		errorList.add("Element " + item.getTagName() + " should have a tag of 'item'");
	}

	// Set the key

	_key = item.getAttribute("key").toUpperCase();
	if (_key == null || _key.length() == 0) {
	    
		_key = item.getAttribute("name").toUpperCase();
		if (_key == null || _key.length() == 0) {
	    	_key = "UNKNOWN";
	    	if (errorList != null)
			errorList.add("Item has no 'name' or 'key' attribute: " + item.toString()
				+ ".  Keyword of UNKNOWN used");
		}
	}

	// Check for type

	_type = TYPE_UNSPECIFIED;
	String type = item.getAttribute("type");
	if (type == null || type.length() == 0)
	    _type = TYPE_UNSPECIFIED;
	else if (type.equalsIgnoreCase("STRING"))
	    _type = TYPE_STRING;
	else if (type.equalsIgnoreCase("INTEGER"))
	    _type = TYPE_INTEGER;
	else if (type.equalsIgnoreCase("FLOAT"))
	    _type = TYPE_FLOAT;
	else if (type.equalsIgnoreCase("DOUBLE"))
	    _type = TYPE_DOUBLE;
	else if (type.equalsIgnoreCase("UNSPECIFIED"))
	    _type = TYPE_UNSPECIFIED;
	else if (errorList != null)
	    errorList.add("Type attribute " + type + " is invalid.  UNSPECIFIED assumed");

	// Get the subitem list, if any

	NodeList subitems = item.getElementsByTagName("subitem");
	int nel = subitems.getLength();

	// Check for value

//!!!! THIS IS WHAT IT SHOULD BE:
//!!!!	if (item.hasAttribute("value")) {
//!!!! but xerces doesn't support DOM level 2.  When JDK goes to 1.4, replace
//!!!!  the following with the above. }
	if (item.getAttributeNode("value") != null) {

	    // Single value exists

	    String value = item.getAttribute("value");
	    _value = new ArrayList(1);
	    _value.add(value);
	    int old_type = _type;
	    guessType(value);	// Not only guesses, but makes sure it's valid

	    if (errorList != null && old_type != _type &&
					old_type != TYPE_UNSPECIFIED)
		errorList.add("Type mismatch for key " + _key + ".  Specified type is " + _typeStrings[old_type] + " but value is " + _typeStrings[_type]);
	    if (errorList != null && nel != 0)
		errorList.add("Value attribute specified for key " + _key + " but subitems are present.  Subitems ignored");

	}
	else {		// Multiple values

	    if (nel == 0) {			// no subitems either!
		_value = new ArrayList(0);
		
		// value is NOT in an attribute, try to get it
		
		// put something into _value
		
		String v = "";
		// get the value from DOMutils
		// DOMutils domUtils = new DOMutils();
		 
		// String getNodeValueString(Node node) 
		v = domUtils.getNodeValue(item);
		if (v != null && v != "") {
			_value = new ArrayList(1);
			_value.add(v);
			guessType(v);
		} else {
		
			if (errorList != null)
		    	errorList.add("Empty value for key " + _key);
		  }
		
	    }
	    else {
		int old_type = _type;
		_value = new ArrayList(nel);
		for (int i=0; i < nel; i++) {
		    Node subitem = subitems.item(i);
		    String v = "";
		    if (!(subitem instanceof Element)) {
			if (errorList != null)
			    errorList.add("Key " + _key + ", subitem " + i + " is not a DOM Element type");
		    }
		    else {
			Element si = (Element)subitem;
//!!!! THIS IS WHAT IT SHOULD BE:
//!!!!			if (errorList != null && !si.hasAttribute("value"))
//!!!! but xerces doesn't support DOM level 2.  When JDK goes to 1.4, replace
//!!!!  the following with the above.
			v = si.getAttribute("value");	// empty string OK
			if (v != null && v != "") {
				_value = new ArrayList(1);
				_value.add(v);
				guessType(v);
			} else {
				v = domUtils.getNodeValue(si); // value isn't in 
				if (v != null && v != "") {
					// _value = new ArrayList(1);
					_value.add(v);
					guessType(v);
				} else {
					if (errorList != null && si.getAttributeNode("value") == null)
			    		errorList.add("Key " + _key + ", subitem " + i + " has no 'value' attribute");
				}
			// 
		    }
		}
		if (errorList != null && old_type != _type &&
						old_type != TYPE_UNSPECIFIED)
		    errorList.add("Type mismatch for key " + _key + ".  Specified type is " + _typeStrings[old_type] + " but value is " + _typeStrings[_type]);

	    }
	 } // else subitems
	} // multiple values
	 
		
	 
    }

////////////////////////////////////////////////////////////////////////
// Internal utilities
////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Returns a String representation of the given integer, suitable for
 * a VICAR label.
 */
    protected String getLabelString(int value)
    {
	return Integer.toString(value);
    }

/***********************************************************************
 * Returns a String representation of the given float, suitable for
 * a VICAR label.
 */
    protected String getLabelString(float value)
    {
	String s = Float.toString(value);
	// C prints "Inf" or "-Inf" while java prints "Infinity".  Change to
	// the C form for consistency with the VICAR standard (not that
	// infinity is really *allowed*, but...).  NaN is the same in both.
	if (s.equals("Infinity"))
	    return "Inf";
	else if (s.equals("-Infinity"))
	    return "-Inf";
	return s;
    }

/***********************************************************************
 * Returns a String representation of the given double, suitable for
 * a VICAR label.
 */
    protected String getLabelString(double value)
    {
	String s = Double.toString(value);
	// C prints "Inf" or "-Inf" while java prints "Infinity".  Change to
	// the C form for consistency with the VICAR standard (not that
	// infinity is really *allowed*, but...).  NaN is the same in both.
	if (s.equals("Infinity"))
	    return "Inf";
	else if (s.equals("-Infinity"))
	    return "-Inf";
	return s;
    }

/***********************************************************************
 * Returns a String representation of the given string, suitable for
 * a VICAR label.  Quotes are provided around strings, and Internal single
 * quotes are doubled.  Note that strings are <em>not</em> stored this way
 * inside the VicarLabelItem object.  This should only be called when the
 * string is being formatted for output to a label.  This is a slight design
 * inconsistency, since everything else is stored in ready-to-go string format.
 */
    protected String getLabelFormattedString(String value)
    {
	if (_type != TYPE_STRING)
	    return value;

	StringBuffer buf = new StringBuffer(value.length());
	buf.append("'");
	int from = 0;
	int to;
	while ((to = value.indexOf('\'', from)) != -1) {
	    if (from != to)
		buf.append(value.substring(from, to));
	    buf.append("''");			// double the quote
	    from = to + 1;
	}
	to = value.length();			// get the rest
	if (from != to)				// last part of string
	    buf.append(value.substring(from,to));
	buf.append("'");
	return buf.toString();
    }

/***********************************************************************
 * Parses the given String according to the VICAR label rules, and returns
 * an integer.  If the string is not a valid int, 0 is quietly returned.
 */
    protected int getIntegerFromString(String s)
    {
	int value = 0;
	if (s.charAt(0) == '+')		// parseInt doesn't like +
	    s = s.substring(1);		// delete it

	try {
	    value = Integer.parseInt(s);
	} catch (NumberFormatException e) {
	    value = 0;
	}

	return value;
    }

/***********************************************************************
 * Parses the given String according to the VICAR label rules, and returns
 * a float.  If the string is not a valid float, 0.0 is quietly returned.
 */
    protected float getFloatFromString(String s)
    {
	float value = 0.0f;
	s.replace('d', 'e');		// parseFloat doesn't like d/D for
	s.replace('D', 'E');		// exponents, so use e/E

	try {
	    value = Float.parseFloat(s);
	} catch (NumberFormatException e) {
	    value = 0.0f;
	}

	return value;
    }

/***********************************************************************
 * Parses the given String according to the VICAR label rules, and returns
 * a double.  If the string is not a valid double, 0.0 is quietly returned.
 */
    protected double getDoubleFromString(String s)
    {
	double value = 0.0;
	s.replace('d', 'e');		// parseDouble doesn't like d/D for
	s.replace('D', 'E');		// exponents, so use e/E

	try {
	    value = Double.parseDouble(s);
	} catch (NumberFormatException e) {
	    value = 0.0;
	}

	return value;
    }

/***********************************************************************
 * "Guess" at the type of the string by looking for non-digit characters.
 * Updates <code>_type</code> appropriately.  <code>_type</code> will only
 * be "promoted", e.g. unspecified->int->real->string, never the reverse...
 * presumably other elements in the value already constrain it.
 * <p>
 * If the type is already specified, this routine ensures that the string
 * is in fact valid for that type.  If not, the type is quietly demoted to
 * the appropriate less-restrictive type.
 * <p>
 * Integer characters are <code>+-0123456789</code>
 * Float characters are <code>+-012345678.eEdD</code>
 * Double is never guessed by this routine.
 */
    protected void guessType(String s)
    {
	if (_type == TYPE_STRING)		// can't override this...
	    return;

	boolean int_okay = true;
	boolean float_okay = true;
	String int_chars = "+-0123456789";
	String float_chars = "+-0123456789.eEdD";

	int len = s.length();
	for (int i=0; i < len; i++) {
	    int ch = s.charAt(i);
	    if (float_chars.indexOf(ch) == -1)		// not found!
		float_okay = false;
	    if (int_chars.indexOf(ch) == -1)		// not found!
		int_okay = false;
	    if (!float_okay && !int_okay) {
		_type = TYPE_STRING;
		return;				// it won't get better...
	    }
	}

	if (_type == TYPE_UNSPECIFIED) {
	    if (float_okay)
		_type = TYPE_FLOAT;
	    if (int_okay)
		_type = TYPE_INTEGER;
	    return;
	}
	if (_type == TYPE_INTEGER) {
	    if (!int_okay)
		_type = TYPE_FLOAT;
	    if (!float_okay)		// oops, shouldn't happen
		_type = TYPE_STRING;
	    return;
	}
	// must be TYPE_FLOAT or TYPE_DOUBLE
	if (!float_okay)		// oops, shouldn't happen
	    _type = TYPE_STRING;

	return;
    }

/***********************************************************************
 * Checks to see if the entire value (all elements) is consistent with the
 * given type, and sets _type appropriately.  The idea is, someone made a
 * change to a single element with the given type, so we see if that type
 * *can* be applied to the entire value, in preference to anything already
 * there.  After all, the user might have just replaced the only value
 * that was invalid for TYPE_INTEGER, for example.
 */
    protected void checkConsistentType(int type)
    {
	_type = type;			// this is what we want
	if (_type == TYPE_STRING)
	    return;			// no point in checking further

	for (int i=0; i < _value.size(); i++) {
	    guessType((String)_value.get(i));
	}
    }

/***********************************************************************
 * Creates a deep copy of the object.  Since _value is a vector of String,
 * we don't need to copy the strings themselves, just the vector.
 */
    public Object clone()
    {
	VicarLabelItem item;
	try {
	    item = (VicarLabelItem)super.clone();
	} catch (Exception e) {
	    return null;
	}

	item._value = (ArrayList)((ArrayList)(item._value)).clone();

	return item;
    }

}

