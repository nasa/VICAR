package jpl.mipl.io.vicar;

/**
 * This class describes a single label entry and how to translate it
 * to/from Bean properties.  It is merely a data structure, so all
 * fields are public access.
 */

public class LabelEntry
{
    /** Comma-separated list of keywords to try in the label.
     *  <em>NOTE!!!!</em> More than one keyword not yet supported! */
    public String _keywordList;

    /** Code representing the format of the data (see below). */
    public int _type;

    /** Name of property to set for value */
    public String _valuePropertyName;

    /** Name of boolean property to set for valid status (may be null) */
    public String _validFlagPropertyName;

    /** Flag indicating whether or not this field is required.  If the
     *  field is not required, a Valid status property should be defined,
     *  or the field must be defaulted.  */
    public boolean _required;

    /** If this entry represents a single element from a multivalued label,
     *  this field indicates the element number (starting at 1).  A 0 in
     *  this field (the default) means either the label has a single element
     *  only (a scalar), or that this entry contains the entire array of
     *  values. */
    public int _elementNumber;

    /** Data types for _type field */
    public static final int TYPE_INT		= 1;
    public static final int TYPE_FLOAT		= 2;
    public static final int TYPE_DOUBLE		= 3;
    public static final int TYPE_STRING		= 4;
    public static final int TYPE_INT_ARRAY	= 5;
    public static final int TYPE_FLOAT_ARRAY	= 6;
    public static final int TYPE_DOUBLE_ARRAY	= 7;
    public static final int TYPE_STRING_ARRAY	= 8;


/***********************************************************************
 * Create a required scalar (or entire array) value with no valid flag.
 */
    public LabelEntry(String keywordList, int type, String valuePropertyName)
    {
	this(keywordList, type, valuePropertyName, null, true, 0);
    }

/***********************************************************************
 * Create a scalar (or entire array) value with valid flag and required
 * flag.  If an entry is not required, you really should have a valid flag,
 * or a default value.
 */
    public LabelEntry(String keywordList, int type,
		String valuePropertyName, String validFlagPropertyName,
		boolean required)
    {
	this(keywordList, type, valuePropertyName, validFlagPropertyName,
		required, 0);
    }

/***********************************************************************
 * Create an entry with all fields.  May be a single element from a
 * multivalued label item.  If an entry is not required, you really should
 * have a valid flag.
 */
    public LabelEntry(String keywordList, int type,
		String valuePropertyName, String validFlagPropertyName,
		boolean required, int elementNumber)
    {
	_keywordList = keywordList;
	_type = type;
	_valuePropertyName = valuePropertyName;
	_validFlagPropertyName = validFlagPropertyName;
	_required = required;
	_elementNumber = elementNumber;
    }

}

