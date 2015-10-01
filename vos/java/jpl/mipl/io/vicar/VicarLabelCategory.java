package jpl.mipl.io.vicar;

import java.util.*;
import java.util.ArrayList;		// just to make javadoc happy
import java.io.*;
import org.w3c.dom.*;

/**
 * This class maintains a category of VICAR labels (i.e. a group of sets -
 * all property sets or all history tasks).  Unlike items within a set, the
 * order of sets within a category *is* important (at least for history to
 * indicate time order) so ordering is preserved.  In addition, both history
 * and property labels allow for multiple instances of the same name, so
 * retrieval must be keyed off of both the name *and* the instance.
 * <p>
 * Note that, while the instance number is <em>stored</em> within the
 * <code>VicarLabelSet</code> object (to maintain self-descriptiveness),
 * it is <em>managed</em> by this class to ensure that they count properly
 * (starting at 1, incrementing by one).  Specifically, this means that any
 * modifications to the <code>VicarLabelCategory</code> structure could
 * change the instance numbers of any or all sets (even ones for which the
 * caller has already received a pointer).
 * <p>
 * Even more importantly, <em>instance numbers must not be modified directly
 * by the caller</em>, or undefined behavior will result.  Use functions in
 * this class to add or remove entries, instead.
 * <p>
 * Per VICAR conventions, instances start counting at 1, not 0.
 * <p>
 * Access to this object is not synchronized in any way.  The caller must
 * synchronize if necessary.  <code>ArrayList</code>s are used instead of
 * <code>Vector</code>s for efficiency.
 */

public class VicarLabelCategory implements Cloneable, Serializable
{
    /** Simply to ensure consistency. Values defined by VicarLabelSet
     * @serial */
    protected int _flavor;

    /** The sets in the category.
     * @serial */
    protected List _sets;

/***********************************************************************
 * Create an empty category.
 */
    public VicarLabelCategory()
    {
	_flavor = 0;
	_sets = new ArrayList();
    }

/***********************************************************************
 * Create a category with the given "flavor" (LABEL_PROPERTY or LABEL_HISTORY).
 */
    public VicarLabelCategory(int flavor)
    {
	_flavor = flavor;
	_sets = new ArrayList();
    }

/***********************************************************************
 * Create a category with the given "flavor" (LABEL_PROPERTY or LABEL_HISTORY)
 * and populate it from the given XML Element.  See <code>fromXML()</code> for
 * more information (the arguments are identical, except for
 * <code>flavor</code>).
 * @see #fromXML(Element, List)
 */
    public VicarLabelCategory(int flavor, Element category, List errorList)
    {
	this(flavor);
	fromXML(category, errorList);
    }

/***********************************************************************
 * Get the label flavor (LABEL_PROPERTY, LABEL_HISTORY).  LABEL_SYSTEM is
 * officially not allowed, but no check is made for this.
 */
    public int getFlavor()
    {
	return _flavor;
    }

/***********************************************************************
 * Set the label flavor (LABEL_PROPERTY, LABEL_HISTORY).  LABEL_SYSTEM is
 * officially not allowed, but no check is made for this.
 */
    public void setFlavor(int flavor)
    {
	_flavor = flavor;
    }

/***********************************************************************
 * Get the number of sets
 */
    public int getNumSets()
    {
	return _sets.size();
    }

/***********************************************************************
 * Retrieve a given set, given an index.  The index starts at 0.
 * An index of -1 says to retrieve the last set.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public VicarLabelSet getSet(int index)
    {
	if (index == -1)
	    index = getNumSets() - 1;
	return (VicarLabelSet)_sets.get(index);
    }

/***********************************************************************
 * Retrieve the set matching the given name and instance.  Returns
 * <code>null</code> if not found.
 * @param set The name of the set to return.
 * @param instance The instance number (starting at 1) of the set to return.
 * An instance of 0 means to return the last instance of the given name.
 */
    public VicarLabelSet getSet(String name, int instance)
    {
	int size = _sets.size();
	VicarLabelSet lastSet = null;
	for (int index = 0; index < size; index++) {
	    if (_sets.get(index).equals(name)) {	// found a candidate
		lastSet = (VicarLabelSet)_sets.get(index);
		if (lastSet.getInstance() == instance && instance != 0)
		    return lastSet;	// it's good
	    }
	}
	if (instance == 0)		// we wanted the last one
	    return lastSet;		// (might be null, that's okay)

	return null;			// not found
    }

/***********************************************************************
 * Return the index for the set matching the given name and instance.  Returns
 * -1 if not found.
 * @param set The name of the set to return.
 * @param instance The instance number (starting at 1) of the set to return.
 * An instance of 0 means to return the index of the last instance of the
 * given name.
 * @return The index of the set, or -1 if not found.  Unlike instances,
 * indexes start at 0.
 */
    public int getSetIndex(String name, int instance)
    {
	int size = _sets.size();
	int lastIndex = -1;
	for (int index = 0; index < size; index++) {
	    if (_sets.get(index).equals(name)) {	// found a candidate
		VicarLabelSet set = (VicarLabelSet)_sets.get(index);
		if (set.getInstance() == instance && instance != 0)
		    return index;	// it's good
		lastIndex = index;	// save
	    }
	}
	if (instance == 0)		// we wanted the last one
	    return lastIndex;		// (might be -1, that's okay)

	return -1;			// not found
    }

/***********************************************************************
 * Return an enumeration of the Sets in this category.  The returned
 * <code>Iterator</code> object will generate all sets in the category.
 */
    public Iterator iterator()
    {
	return _sets.listIterator();
    }

/***********************************************************************
 * Return an array of set names that are currently in this category.  Should
 * be used in conjunction with getSetInstances().  Names with more than
 * one instance are duplicated in the list, e.g. names[i] and instances[i]
 * both refer to entry i.
 * @see #getSetInstances()
 */
    public String[] getSetNames()
    {
	String[] names = new String[_sets.size()];
	for (int i=0; i < _sets.size(); i++)
	    names[i] = ((VicarLabelSet)(_sets.get(i))).getName();
	return names;
    }

/***********************************************************************
 * Return an array of set instances that are currently in this category.
 * Should be used in conjunction with getSetNames().  This list parallels
 * the name list, e.g. names[i] and instances[i] both refer to entry i.
 * @see #getSetNames()
 */
    public int[] getSetInstances()
    {
	int[] instances = new int[_sets.size()];
	for (int i=0; i < _sets.size(); i++)
	    instances[i] = ((VicarLabelSet)(_sets.get(i))).getInstance();
	return instances;
    }

/***********************************************************************
 * Counts the number of instances that match the given set name.  Returns
 * 0 if the name is not found.
 */
    public int getInstanceCount(String name)
    {
	int count = 0;
	int size = _sets.size();
	for (int index = 0; index < size; index++) {
	    if (_sets.get(index).equals(name)) {	// found an instance
		count++;
	    }
	}
	return count;
    }

/***********************************************************************
 * Appends a set to the end of the list.  The instance number is updated.
 * @see ArrayList#add(Object)
 */
    public void add(VicarLabelSet new_set)
    {
	int instance = getInstanceCount(new_set.getName());
	new_set.setInstance(instance+1);
	_sets.add(new_set);
    }

/***********************************************************************
 * Inserts a set at the specified position in the list.  All instances of
 * this set's name are updated.
 * @see ArrayList#add(int, Object)
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public void add(int index, VicarLabelSet new_set)
    {
	_sets.add(index, new_set);
	recalculateInstances(new_set.getName());
    }

/***********************************************************************
 * Remove a set, given an index.  As a special case, attempts to remove
 * an index of -1 are ignored, allowing one to say
 * <code>category.remove(getSetIndex(name, instance));</code>
 * <p>
 * The instance number of the returned set is useless (it's what the
 * instance *used* to be before removal, but that is no longer valid).
 * @return the set that was removed, or null if index=-1 is passed in.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @see ArrayList#remove(int)
 */
    public VicarLabelSet removeSet(int index)
    {
	if (index == -1)
	    return null;
	VicarLabelSet oldSet = (VicarLabelSet)_sets.remove(index);
	if (oldSet != null)
	    recalculateInstances(oldSet.getName());
	return oldSet;
    }

/***********************************************************************
 * Returns a human-readable representation of all sets in this category.
 * Includes the items within the sets, i.e. this is almost label-list.
 */
    public String toString()
    {
	StringBuffer buf = new StringBuffer(getNumSets() * 500);

	for (Iterator it = iterator(); it.hasNext(); ) {
	    buf.append(((VicarLabelSet)(it.next())).toString());
	}
	return buf.toString();
    }

/***********************************************************************
 * Returns a set of XML DOM nodes for all the sets in this category.
 * Since this returns a list of nodes, which have no specific parent node
 * for this level (i.e. all sets go under the same VICAR_LABEL item
 * regardless of flavor), this method is slightly different from the other
 * <code>toXML</code> methods in the <code>VicarLabel</code> suite..
 * A <code>Node</code> is required to be passed in; all sets will be appended
 * to this <code>Node</code> (which is then returned, redundantly).
 * See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * @param document The <code>Document which will contain the node.
 * @param parent The <code>Node</code> into which all elements will be put.
 * This parent is also returned as the function return.  May not be null.
 */
    public Node toXML(Document doc, Node parent)
    {
	for (Iterator it = iterator(); it.hasNext(); ) {
	    parent.appendChild(((VicarLabelSet)(it.next())).toXML(doc));
	}
	return parent;
    }

/***********************************************************************
 * Reads a <code>VicarLabelCategory</code> from an XML DOM node.  Only
 * sets that match the "flavor" of this category are read, so the flavor
 * must be set in the category prior to calling this routine.  For example,
 * if the flavor is LABEL_PROPERTY, all of the &lt;property&gt; children will
 * be read from the supplied Node, while &lt;history&gt; and &lt;system&gt;
 * children will be ignored.  See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * <p>
 * Sets found in the XML are added to this category.  This means that
 * existing sets will be retained in the label (instance numbers will be
 * set accordingly).  If you want only the XML items, make sure the
 * Category is empty before calling this function.
 * <p>
 * If anything is wrong with the supplied node, error/warning messages
 * will be appended to the supplied <code>errorList</code>.  However, the
 * parser keeps working as long as possible, despite errors.  The practical
 * upshot of this is that the errors can usually be ignored; the parser will
 * do the best it can.  However, the errors can be displayed to the user
 * if desired.  An exception will be thrown only if something unrecoverable
 * happens.
 * <p>
 * @param category The <code>Element</code> from which to extract the sets.
 * @param errorList A list of <code>String</code>s to which any parsing
 * errors will be appended.  Can be <code>null</code>, in which case no
 * errors are returned.
 * @throws DOMException if something unrecoverable happens.  Improperly
 * formatted items or violations of the DTD do not necessarily result in
 * exceptions; parsing continues as long as possible and problems are appended
 * to the <code>errorList</code>.
 *
 * @see VicarLabelSet#fromXML(Element, List)
 */
    public void fromXML(Element category, List errorList)
    {
	NodeList sets;
	switch (_flavor) {
	    case VicarLabelSet.LABEL_PROPERTY:
		sets = category.getElementsByTagName("property");
		if (sets == null || sets.getLength() <= 0)
			sets = category.getElementsByTagName("PROPERTY");
		String name = category.getNodeName();
		String tagName = category.getTagName();	
		// System.out.println("VicarLabelCategory.fromXML "+name+", "+tagName);
		if (name.equalsIgnoreCase("PROPERTY") ) {
			add(new VicarLabelSet(category, errorList));
		}
		break;
	    case VicarLabelSet.LABEL_HISTORY:
		sets = category.getElementsByTagName("history");
		if (sets == null || sets.getLength() <= 0)
			sets = category.getElementsByTagName("HISTORY");
			// could also try "TASK"
		break;
	    case VicarLabelSet.LABEL_SYSTEM:
		if (errorList != null)
		    errorList.add("SYSTEM label flavor should not be in a Category.  Label ignored.");
		return;
	    default:
		if (errorList != null)
		    errorList.add("Unknown VicarLabelCategory flavor.  Category ignored.");
		return;
	}

	int nsets = 0;
	if (sets != null && sets.getLength() > 0)
	    nsets = sets.getLength();
	for (int i=0; i < nsets; i++) {
	    Node set = sets.item(i);
	    if (!(set instanceof Element)) {
		if (errorList != null)
		    errorList.add("Category " + set.getNodeName() + " set #" + i
					+ " is not a DOM Element type");
	    }
	    else {
		add(new VicarLabelSet((Element)set, errorList));
	    }
	}
    }

/***********************************************************************
 * Throwaway class used only in <code>VicarLabelCategory.toLabelString</code>
 * to return the set and item indices of where this label portion stopped.
 * Both indices represent the first item <em>not</em> put in the string,
 * suitable for passing in to the next call.  A null object means that all
 * items fit.
 */
    public class ItemIndex
    {
	public int set_index;
	public int item_index;
    }

/***********************************************************************
 * Returns in the supplied StringBuffer the part of the category from
 * <code>start_pos</code> to the end as a valid VICAR label string, ready
 * to be put in a file.  If the <code>max_length</code> argument is
 * non-zero, it represents the maximum size the string can become (normally
 * used to make sure the label fits in the space at the front of a file).
 * This includes any data already in buf when the function is called.
 * A 0 means no maximum.
 * <p>
 * Note that the caller is responsible for putting in LBLSIZE= at the
 * beginning of each buffer's label segment.
 * <p>
 * @param start_pos A VicarLabelCategory.ItemIndex structure containing the
 * set and item indices of the first item to be included in the string.
 * If null, the label is written from the beginning.
 * @return A VicarLabelCategory.ItemIndex structure containing the set and
 * item indices of the first item <em>not</em> included in the string.  This
 * should be passed into <code>start_pos</code> on the next call.  Returns
 * null if the entire label was written.
 */
    public VicarLabelCategory.ItemIndex toLabelString(StringBuffer buf,
		int max_length, VicarLabelCategory.ItemIndex start_pos)
    {
	int start_set = 0;
	int start_item = 0;
	if (start_pos != null) {
	    start_set = start_pos.set_index;
	    start_item = start_pos.item_index;
	}
	for (int set = start_set; set < getNumSets(); set++) {
	    int last_item = getSet(set).toLabelString(buf,
						max_length, start_item);
	    if (last_item != -1) {		// Exceeded capacity!
		VicarLabelCategory.ItemIndex rtn =
					new VicarLabelCategory.ItemIndex();
		rtn.set_index = set;
		rtn.item_index = last_item;
		return rtn;
	    }
	    start_item = 0;	// use argument first time through only
	}
	return null;			// everything fit
    }

/***********************************************************************
 * Recalculates instance numbers for all sets of the given name.
 */
    protected void recalculateInstances(String name)
    {
	int size = _sets.size();
	int instance = 1;
	for (int index = 0; index < size; index++) {
	    if (_sets.get(index).equals(name)) {	// found an instance
		((VicarLabelSet)(_sets.get(index))).setInstance(instance);
		instance++;
	    }
	}
    }

/***********************************************************************
 * Recalculates instance numbers for all sets.  This is normally not needed,
 * as instance numbers are maintained automatically.  However, if the caller
 * does something really nasty to a Set, such as changing its name, without
 * going through the Category, the instances could get confused.  It is up
 * to the user to call this routine in that case.  Note that this is
 * implemented very inefficiently, because it should not be needed.
 */
    public void recalculateInstances()
    {
	int size = _sets.size();
	for (int index = 0; index < size; index++) {
	    // inefficiency:  we need to recalc each unique name only once.
	    recalculateInstances(((VicarLabelSet)(_sets.get(index))).getName());
	}
    }

/***********************************************************************
 * Creates a deep copy of the VicarLabelCategory.  Nothing can be done to
 * the returned copy that in any way affects the original.
 */
    public Object clone()
    {
	VicarLabelCategory cat;
	try {
	    cat = (VicarLabelCategory)super.clone();
	} catch (Exception e) {
	    return null;
	}

	cat._sets = (ArrayList)((ArrayList)(cat._sets)).clone();
	for (int i=0; i < cat._sets.size(); i++) {
	    cat._sets.set(i, ((VicarLabelSet)(cat._sets.get(i))).clone());
	}

	return cat;
    }

}

