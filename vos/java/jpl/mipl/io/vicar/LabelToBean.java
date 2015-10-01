package jpl.mipl.io.vicar;

import java.util.*;
import java.beans.*;
import java.lang.reflect.*;

/**
 * This class provides static functions which transfer data between
 * <code>VicarLabelSet</code> objects and a JavaBean object described by an
 * array of <code>LabelEntry</code>'s.  The <code>LabelEntry</code> array
 * describes the VICAR label item as well as the property to be set (or
 * read) in the bean.
 */

public class LabelToBean {

/***********************************************************************
 * Translate from a <code>VicarLabelSet</code> to Bean properties.
 * Items in the set which are not found in the <code>LabelEntry</code>
 * table are added to the <code>extraItems</code> vector, if supplied.
 * <p>
 * <em>NOT IMPLEMENTED YET!!!!:  elementNumber and "required" checks, and
 *     multiple keywords.</em>
 * @param set The <code>VicarLabelSet</code> containing the labels to read.
 * @param bean The JavaBean in which the properties will be set.
 * @param entries An array of <code>LabelEntry</code> objects describing
 *	the relationship between label items and bean properties
 * @param extraItems A <code>List</code> supplied by the caller.  This
 *	function will add any unrecognized <code>VicarLabelItem</code>
 *	entries to this vector.  May be null if you don't need this info.
 *	Note that an item is recognized if <em>any</em> of its elements
 *	are found in the table; the fact that element 2 might have been
 *	present but 1 and 3 aren't, is not tracked.
 * @see VicarLabelSet
 * @see LabelEntry
 * @throws IntrospectionException if the bean's BeanInfo can't be found
 * @throws InvocationTargetException if the set method is invalid.
 * @throws IllegalAccessException if the set method is invalid.
 * @throws IllegalArgumentException if the entry._type field is invalid.
 */

    public static void readLabel(VicarLabelSet set, Object bean,
		LabelEntry[] entries, List extraItems)
		throws IntrospectionException,
		       InvocationTargetException, IllegalAccessException
    {
	BeanInfo info = Introspector.getBeanInfo(bean.getClass());
	PropertyDescriptor descr[] = info.getPropertyDescriptors();

	// Loop through each item in the set and search the table for a match

	for (Iterator it = set.iterator(); it.hasNext(); ) {
	    VicarLabelItem item = (VicarLabelItem)it.next();

	    boolean found = false;
	    for (int i=0; i < entries.length; i++) {
		if (item.equals(entries[i]._keywordList)) {

		    // Found a match.  Now process it
		    addToBean(item, bean, entries[i], descr);

		    found = true;
		    break;		// skip the rest of the loop
		}
	    }

	    if (!found) {		// add to extraItems list
		if (extraItems != null)
		    extraItems.add(item);
	    }
	}
    }

/***********************************************************************
 * Given a <code>VicarLabelItem</code>, use the <code>LabelEntry</code> to
 * put the property in the Bean.  The caller should fetch the bean's
 * PropertyDescriptors, so the call needs to be made only once.
 * <p>
 * <em>NOT IMPLEMENTED YET!!!!:  elementNumber and "required" checks, and
 *     multiple keywords.</em>
 * @throws InvocationTargetException if the set method is invalid.
 * @throws IllegalAccessException if the set method is invalid.
 * @throws IllegalArgumentException if the entry._type field is invalid.
 */

    protected static void addToBean(VicarLabelItem item, Object bean,
		LabelEntry entry, PropertyDescriptor descriptors[])
		throws InvocationTargetException, IllegalAccessException
    {
	Object[] arg = new Object[1];

	switch (entry._type) {
	    case LabelEntry.TYPE_INT:
		arg[0] = new Integer(item.getInteger());
		break;
	    case LabelEntry.TYPE_FLOAT:
		arg[0] = new Float(item.getFloat());
		break;
	    case LabelEntry.TYPE_DOUBLE:
		arg[0] = new Double(item.getDouble());
		break;
	    case LabelEntry.TYPE_STRING:
		arg[0] = item.getString();
		break;
	    case LabelEntry.TYPE_INT_ARRAY:
		arg[0] = item.getIntegerArray();
		break;
	    case LabelEntry.TYPE_FLOAT_ARRAY:
		arg[0] = item.getFloatArray();
		break;
	    case LabelEntry.TYPE_DOUBLE_ARRAY:
		arg[0] = item.getDoubleArray();
		break;
	    case LabelEntry.TYPE_STRING_ARRAY:
		arg[0] = item.getStringArray();
		break;
	    default:
		throw new IllegalArgumentException("Invalid LabelEntry type field");
	}

	for (int i=0; i < descriptors.length; i++) {

	    // Check for matching value property

	    if (entry._valuePropertyName.equals(descriptors[i].getName()) &&
		    !(descriptors[i] instanceof IndexedPropertyDescriptor)) {

		// Found the matching property
		Method set = descriptors[i].getWriteMethod();
		if (set != null)
		    set.invoke(bean, arg);
		break;
	    }
	}

	if (entry._validFlagPropertyName != null) {
	    for (int i=0; i < descriptors.length; i++) {
		// Check for matching valid flag property

		if (entry._validFlagPropertyName.equals(
						descriptors[i].getName()) &&
		       !(descriptors[i] instanceof IndexedPropertyDescriptor)) {

		    // Found the matching valid flag
		    Method valid = descriptors[i].getWriteMethod();
		    if (valid != null)
			valid.invoke(bean, new Object[] { new Boolean(true) });
		    break;
		}
	    }
	}
    }

/***********************************************************************
 * Translate from Bean properties to a <code>VicarLabelSet</code>.
 * Items in the extraItems vector are assumed to be VicarLabelItems as
 * well, and are added to the Set.
 * <p>
 * If a Valid flag property exists in the LabelEntry, then the value is only
 * written out if the Valid flag is true, or if the item is required.
 *
 * <em>NOT IMPLEMENTED YET!!!!:  elementNumber and "required" checks.</em>
 * @param set The <code>VicarLabelSet</code> to be updated.  Any existing
 *	items are left alone in case a name conflicts, in which case they are
 *	overridden.
 * @param bean The JavaBean from which to get the property values.
 * @param entries An array of <code>LabelEntry</code> objects describing
 *	the relationship between label items and bean properties
 * @param extraItems A <code>List</code> supplied by the caller.  If
 *	present, items in this vector are assumed to be
 *	<code>VicarLabelItem</code>s and are added to the end of the Set.
 *	May be null.
 * @see VicarLabelSet
 * @see LabelEntry
 * @throws IntrospectionException if the bean's BeanInfo can't be found
 * @throws InvocationTargetException if the set method is invalid.
 * @throws IllegalAccessException if the set method is invalid.
 * @throws IllegalArgumentException if the entry._type field is invalid.
 */

    public static void writeLabel(VicarLabelSet set, Object bean,
		LabelEntry[] entries, List extraItems)
		throws IntrospectionException,
		       InvocationTargetException, IllegalAccessException
    {
	BeanInfo info = Introspector.getBeanInfo(bean.getClass());
	PropertyDescriptor descr[] = info.getPropertyDescriptors();

	// Loop through each item in the entry table

	for (int i=0; i < entries.length; i++) {
	    addToLabel(set, bean, entries[i], descr);
	}

	// Now add the extra items

	if (extraItems != null) {
	    for (Iterator it = extraItems.iterator(); it.hasNext(); ) {
		set.add((VicarLabelItem)it.next());
	    }
	}
    }

/***********************************************************************
 * Given a <code>LabelEntry</code>, retrieve the value from the bean and
 * put it in the <code>VicarLabelSet</code>.  The caller should fetch the
 * bean's PropertyDescriptors, so the call needs to be made only once.
 * <p>
 * <em>NOT IMPLEMENTED YET!!!!:  elementNumber and "required" checks.</em>
 * @throws InvocationTargetException if the set method is invalid.
 * @throws IllegalAccessException if the set method is invalid.
 * @throws IllegalArgumentException if the entry._type field is invalid.
 */

    protected static void addToLabel(VicarLabelSet set, Object bean,
		LabelEntry entry, PropertyDescriptor descriptors[])
		throws InvocationTargetException, IllegalAccessException
    {

	// Find the value and valid methods

	Method valid_meth = null;
	Method value_meth = null;

	for (int i=0; i < descriptors.length; i++) {

	    // Check for matching value property

	    if (entry._valuePropertyName.equals(descriptors[i].getName()) &&
		    !(descriptors[i] instanceof IndexedPropertyDescriptor)) {

		value_meth = descriptors[i].getReadMethod();
		break;
	    }
	}

	if (entry._validFlagPropertyName != null) {
	    for (int i=0; i < descriptors.length; i++) {

		// Check for matching valid flag property

		if (entry._validFlagPropertyName.equals(
						descriptors[i].getName()) &&
		       !(descriptors[i] instanceof IndexedPropertyDescriptor)) {

		    valid_meth = descriptors[i].getReadMethod();
		    break;
		}
	    }
	}

	// If not valid and not required, we don't write anything.  If no
	// valid flag, write it anyway.

	if (valid_meth != null && !entry._required) {
	    Boolean valid = (Boolean)valid_meth.invoke(bean, (Object[])null);
	    if (!valid.booleanValue())
		return;
	}

	// Retrieve the value.  This should generate an execption if
	// value_meth is null, which is appropriate since that's a bad
	// programming error.

	Object value = value_meth.invoke(bean, (Object[])null);

	// Create the VicarLabelItem

	VicarLabelItem item = new VicarLabelItem(entry._keywordList);

	switch (entry._type) {
	    case LabelEntry.TYPE_INT:
		item.setValue(((Integer)value).intValue());
		break;
	    case LabelEntry.TYPE_FLOAT:
		item.setValue(((Float)value).floatValue());
		break;
	    case LabelEntry.TYPE_DOUBLE:
		item.setValue(((Double)value).doubleValue());
		break;
	    case LabelEntry.TYPE_STRING:
		item.setValue((String)value);
		break;
	    case LabelEntry.TYPE_INT_ARRAY:
		item.setValue((int[])value);
		break;
	    case LabelEntry.TYPE_FLOAT_ARRAY:
		item.setValue((float[])value);
		break;
	    case LabelEntry.TYPE_DOUBLE_ARRAY:
		item.setValue((double[])value);
		break;
	    case LabelEntry.TYPE_STRING_ARRAY:
		item.setValue((String[])value);
		break;
	    default:
		throw new IllegalArgumentException("Invalid LabelEntry type field");
	}

	// Add it to the label

	set.add(item);
    }

}

