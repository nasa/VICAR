package jpl.mipl.io.vicar;

import java.util.*;
import java.text.Collator;
import java.io.*;
import org.w3c.dom.*;

/**
 * This class maintains an entire set (property set, history task, or system)
 * of VICAR labels, with a collection of key/value pairs (Item's).
 * <p>
 * Note that by definition of VICAR labels, the order of items within a set
 * is irrelevant and unspecified, and duplicates are not allowed.  This class
 * attempts to maintain the order in which the items are provided to it, but
 * this is in no way guaranteed.  (This is the same behavior as the C RTL).
 * <p>
 * Access to this object is not synchronized in any way.  The caller must
 * synchronize if necessary.  <code>ArrayList</code>s are used instead of
 * <code>Vector</code>s for efficiency.
 * <p>
 * This class ensures that History and Property sets (as defined by the
 * set's "flavor") start with the appropriate keywords ("PROPERTY" for
 * property sets, "TASK", "USER", and "DAT_TIM" for history tasks).  The
 * values of these can be modified but the items themselves cannot be
 * removed or unset.  Note that per VICAR conventions, the property and
 * task names are converted to upper case.
 * <p>
 * The set's "flavor" (system, property, or history) must be set at
 * construction time and cannot be changed (due to the required header labels).
 * <p>
 * This class implements Set so it can be used as a collection.  Additional
 * constraints imposed beyond those of Set:  all elements must be instances
 * of <code>VicarLabelItem</code>.
 */

public class VicarLabelSet implements Cloneable, Serializable, Set
{

    /** Flavor of label:  system, property, or history
     * @serial */
    protected int _flavor;

    /** Name of set
     * @serial */
    protected String _name;

    /** Instance number of set.  Must be maintained externally.
     * @serial */
    protected int _instance;

    /** The items in the set.
     * @serial */
    protected List _items;

/***********************************************************************
 * These values are used to indicate the "flavor" of this label set.
 */
    public static final int LABEL_SYSTEM = 0;
    public static final int LABEL_PROPERTY = 1;
    public static final int LABEL_HISTORY = 2;

/***********************************************************************
 * Create a set with the given parameters.
 */
    public VicarLabelSet(int flavor, String name, int instance)
    {
	_flavor = flavor;
	_name = name;
	_instance = instance;
	initItems();
    }

/***********************************************************************
 * Create a set with the given flavor and name.  The instance is defaulted
 * to 0; presumably it will be set immediately by
 * <code>VicarLabelCategory</code>.
 */
    public VicarLabelSet(int flavor, String name)
    {
	_flavor = flavor;
	_name = name;
	_instance = 0;
	initItems();
    }

/***********************************************************************
 * Create a set with the given flavor.  The instance is defaulted
 * to 0; presumably it will be set immediately by
 * <code>VicarLabelCategory</code>.
 */
    public VicarLabelSet(int flavor)
    {
	_flavor = flavor;
	_name = null;
	_instance = 0;
	initItems();
    }

/***********************************************************************
 * No-args constructor mandated by the <code>Collection</code> interface.
 * It hard-codes the flavor to <code>SYSTEM</code>.  For this reason, this
 * constructor <em>should not be used!!</code>  If such a thing is really
 * needed, a dummy class can be extended from this one for the appropriate
 * flavor with a no-args constructor that calls this(flavor).
 */
    public VicarLabelSet()
    {
	this(LABEL_SYSTEM);
    }

/***********************************************************************
 * Constructor mandated by the <code>Collection</code> interface.  It should
 * be used only with <code>Collection</code>s of type
 * <code>VicarLabelSet</code>.  If another type of collection is provided,
 * this constructor will work, but will behave as described in the no-args
 * constructor w.r.t. flavor... in other words <em>don't use it this way</em>.
 * <p>
 * In any case, all members of the Collection must be a
 * <code>VicarLabelItem</code>.
 * @throws ClassCastException if the provided collection does not contain
 * a <code>VicarLabelItem</code>.
 * @throws IllegalArgumentException if an item is not VicarLabelItem or is
 * an inappropriate reserved keyword.
 * @see #VicarLabelSet()
 * @see #VicarLabelSet(Collection, int)
 */
    public VicarLabelSet(Collection c)
    {
	this(c, (c instanceof VicarLabelSet) ?
			((VicarLabelSet)c).getFlavor() :
			LABEL_SYSTEM);
    }

/***********************************************************************
 * Construct a <code>VicarLabelSet</code> from another <code>Collection</code>,
 * using the specified flavor.  This may be used to change flavors of a
 * <code>VicarLabelSet</code>.  The provided Collection may be an instance
 * of this class, or any other collection.  Any reserved keywords
 * (<code>PROPERTY, TASK, USER, DAT_TIM, LBLSIZE</code>) that appear in the
 * input collection which are not appropriate for the given flavor, will cause
 * an exception to be thrown, in order to maintain label consistency.
 * <p>
 * Note that if the special items are not present, defaults will be used,
 * so the name at least (for property/history) should be set independently.
 * @throws ClassCastException if the provided collection does not contain
 * a <code>VicarLabelItem</code>.
 * @throws IllegalArgumentException if an item is not VicarLabelItem or is
 * an inappropriate reserved keyword.
 * @see #add(VicarLabelItem)
 * @see #addAll(Collection)
 */
    public VicarLabelSet(Collection c, int flavor)
    {
	this(flavor);
	addAll(c);
    }

/***********************************************************************
 * Create a set from an XML Element.  See <code>fromXML()</code> for
 * more information (the arguments are identical).  The only difference is,
 * the flavor is set from the XML (since this is a constructor), while
 * <code>fromXML()</code> is unable to change the flavor.
 * <p>
 * The instance is defaulted to 0; presumably it will be set immediately by
 * <code>VicarLabelCategory</code>.
 * <p>
 * If the supplied node is not &lt;system&gt;, &lt;property&gt;,
 * or &lt;history&gt;, a warning is issued and &lt;history&gt; is assumed
 * (being the least "damaging" of the three types for incorrect values).
 * <p>
 * @see #fromXML(Element, List)
 */
    public VicarLabelSet(Element set, List errorList)
    {
	String tagName = set.getTagName();
	// System.out.println("VicarLabelSet.constructor tagName "+tagName);
	// System.out.println("VicarLabelSet.constructor nodeName "+((Node)set).getNodeName());
	
	if (tagName.equalsIgnoreCase("system")) {
	    _flavor = LABEL_SYSTEM;
	} else if (tagName.equalsIgnoreCase("property")) {
	    _flavor = LABEL_PROPERTY;
	} else if (tagName.equalsIgnoreCase("history")) { // "TASK"
	    _flavor = LABEL_HISTORY;
	} else {
	    if (errorList != null)
		errorList.add("Invalid label set name " + tagName + ".  Requires system, property, or history.  History assumed.");
	    _flavor = LABEL_HISTORY;
	}
	_name = null;
	_instance = 0;
	initItems();

	fromXML(set, errorList);
    }

/***********************************************************************
 * Internal routine to initialize the set with the appropriate header
 * labels.  Note that System labels have no header.  If the set name has
 * not been provided, it is set to "UNSET".  The user and dat_tim
 * elements of the History label are also set to "unset".  These values
 * should be overridden in all cases; they should never appear in a final
 * label if the program is properly written.
 */
    protected void initItems()
    {
	_items = new ArrayList();

	switch (_flavor) {
	    case LABEL_SYSTEM:		// nothing special
		break;
	    case LABEL_PROPERTY:
		add(new VicarLabelItem("PROPERTY",
					(_name == null) ? "UNSET" : _name));
		break;
	    case LABEL_HISTORY:
		add(new VicarLabelItem("TASK",
					(_name == null) ? "UNSET" : _name));
		add(new VicarLabelItem("USER", "unset"));
		add(new VicarLabelItem("DAT_TIM", "unset"));
		break;
	    default:
		throw new IllegalArgumentException(
					"VICAR Label set's flavor is invalid");
	}
    }

/***********************************************************************
 * Get the label flavor (LABEL_SYSTEM, LABEL_PROPERTY, LABEL_HISTORY).
 * The flavor cannot be set other than at construction time.
 */
    public int getFlavor()
    {
	return _flavor;
    }

/***********************************************************************
 * Get the label set name (property set or history task name)
 */
    public String getName()
    {
	return _name;
    }

/***********************************************************************
 * Set the label set name (property set or history task name).  Sets the
 * PROPERTY or TASK headers of property or history labels.  If the flavor
 * is SYSTEM, this call is ignored.
 * <p>
 * <em>WARNING</em>:  Changing the name of the set may mess up the instance
 * number count.  It is up to the caller to rectify this.
 */
    public void setName(String name)
    {
	_name = name;
	if (name == null)
	    name = "UNSET";		// should never appear in label

	if (_flavor == LABEL_PROPERTY)
	    add(new VicarLabelItem("PROPERTY", name));
	if (_flavor == LABEL_HISTORY)
	    add(new VicarLabelItem("TASK", name));
    }

/***********************************************************************
 * Get the label set instance
 */
    public int getInstance()
    {
	return _instance;
    }

/***********************************************************************
 * Set the label set instance
 */
    public void setInstance(int instance)
    {
	_instance = instance;
    }

/***********************************************************************
 * Get the number of items
 */
    public int size()
    {
	return _items.size();
    }

/***********************************************************************
 * Two VicarLabelSet objects are equal for the purposes of this routine
 * if their Names are equal.  The instances are NOT considered.  While this
 * is technically untrue (name and instance should both be equal), there
 * is no facility to take more than one object as a parameter to equals(),
 * and doing it this way enables certain searches in VicarLabelCategory.
 * We also test for equality of the Name with a String.
 */
    public boolean equals(Object o)
    {
	if (o instanceof String)
	    return _name.equalsIgnoreCase((String)o);
	if (o instanceof VicarLabelSet)
	    return _name.equalsIgnoreCase(((VicarLabelSet)o).getName());
	return false;
    }

/***********************************************************************
 * Retrieve a given item, given an index
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 */
    public VicarLabelItem getItem(int index)
    {
	return (VicarLabelItem)_items.get(index);
    }

/***********************************************************************
 * Retrieve the item matching the given keyword.  Returns <code>null</code>
 * if not found.
 */
    public VicarLabelItem getItem(String keyword)
    {
	int index = _items.indexOf(new VicarLabelItem(keyword));
	if (index == -1)
	    return null;
	return (VicarLabelItem)_items.get(index);
    }

/***********************************************************************
 * Return an enumeration of the Items in this set.  The returned
 * <code>Iterator</code> object will generate all items in the set.
 * <p>
 * The iterator returned is actually an instance of <code>ListIterator</code>;
 * despite the fact that we extend <code>Set</code> there is an attempt made
 * to preserve list order (as stated in the class comments).  If
 * <code>ListIterator</code> functions are required, a cast may be made.
 */
    public Iterator iterator()
    {
	return _items.listIterator();
    }

/***********************************************************************
 * Return an array of keyword names that are currently in this set.
 */
    public String[] getKeywordNames()
    {
	String[] names = new String[_items.size()];
	for (int i=0; i < _items.size(); i++)
	    names[i] = ((VicarLabelItem)(_items.get(i))).getKeyword();
	return names;
    }

/***********************************************************************
 * Add an item to the list.  If the given keyword matches one already in
 * the list, the old item is replaced.  Otherwise, the item is added to
 * the end.  The reserved names LBLSIZE, PROPERTY, TASK, USER, and DAT_TIM
 * may only be added to the proper flavor of label (otherwise an
 * <code>IllegalArgumentException</code> will be thrown).
 * <p>
 * If PROPERTY or TASK is set, the value is converted to uppercase, per
 * VICAR convention.
 * <p>
 * <em>Note:</em>  This function violates the contract of <code>Set.add</code>
 * in the following way:  if the item already exists (according to the
 * keyword), the new item <em>replaces</em> the old one.  This modification
 * is needed because item equality is determined only by the keyword; the
 * value is not checked.  Thus it is useful to replace the existing item,
 * as opposed to a true <code>Set</code> where equality is defined by the
 * entire object.
 * @return true always
 * @throws IllegalArgumentException
 */
    public boolean add(VicarLabelItem new_item)
    {
	if (new_item.equals("LBLSIZE") && _flavor != LABEL_SYSTEM)
	    throw new IllegalArgumentException(
				"LBLSIZE may only go in system label");
	if (new_item.equals("PROPERTY")) {
	    if (_flavor != LABEL_PROPERTY)
		throw new IllegalArgumentException(
				"PROPERTY may only go in property label");
	    // Convert to uppercase.  Has the beneficial side-effect of
	    // ensuring a single-valued label item (not an array)
	    _name = new_item.getString().toUpperCase();
	    new_item.setValue(_name);
	}
	if (new_item.equals("TASK")) {
	    if (_flavor != LABEL_HISTORY)
		throw new IllegalArgumentException(
				"TASK may only go in history label");
	    // Convert to uppercase.  Has the beneficial side-effect of
	    // ensuring a single-valued label item (not an array)
	    _name = new_item.getString().toUpperCase();
	    new_item.setValue(_name);
	}
	if (new_item.equals("USER") && _flavor != LABEL_HISTORY)
	    throw new IllegalArgumentException(
				"USER may only go in history label");
	if (new_item.equals("DAT_TIM") && _flavor != LABEL_HISTORY)
	    throw new IllegalArgumentException(
				"DAT_TIM may only go in history label");

	int index = _items.indexOf(new_item);
	if (index == -1)		// new item
	    _items.add(new_item);
	else				// replace existing one
	    _items.set(index, new_item);
	return true;
    }

/***********************************************************************
 * Another version of <code>add</code> that is mandated by the <code>Set</code>
 * interface.  It simply calls the other <code>add()</code>.
 * @throws ClassCastException if the argument is not a
 * <code>VicarLabelItem</code>.
 */
    public boolean add(Object o)
    {
	return add((VicarLabelItem)o);
    }

/***********************************************************************
 * Remove the given item, given an index.
 * @return the item that was removed.
 * @throws ArrayIndexOutOfBoundsException but this need not be declared
 * @throws IllegalArgumentException if an attempt is made to delete TASK,
 * USER, DAT_TIM, or PROPERTY.
 */
    public VicarLabelItem removeItem(int index)
    {
	VicarLabelItem item = (VicarLabelItem)_items.get(index);
	if (item.equals("PROPERTY") || item.equals("TASK") ||
		item.equals("USER") || item.equals("DAT_TIM"))
	    throw new IllegalArgumentException(
		"Cannot remove PROPERTY, TASK, USER, or DAT_TIM label items");

	return (VicarLabelItem)_items.remove(index);
    }

/***********************************************************************
 * Removes an item, given a keyword.
 * @return the item that was removed, or <code>null</code> if not found.
 * @throws IllegalArgumentException if an attempt is made to delete TASK,
 * USER, DAT_TIM, or PROPERTY.
 */
    public VicarLabelItem removeItem(String keyword)
    {
	int index = _items.indexOf(new VicarLabelItem(keyword));
	if (index == -1)
	    return null;
	VicarLabelItem item = (VicarLabelItem)_items.get(index);
	if (item.equals("PROPERTY") || item.equals("TASK") ||
		item.equals("USER") || item.equals("DAT_TIM"))
	    throw new IllegalArgumentException(
		"Cannot remove PROPERTY, TASK, USER, or DAT_TIM label items");

	return (VicarLabelItem)_items.remove(index);
    }

/***********************************************************************
 * Returns a human-readable representation of all items in this set.
 * The returned string is <em>NOT</em> suitable for directly writing to
 * a label!
 */
    public String toString()
    {
	StringBuffer buf = new StringBuffer(size() * 20);

	switch (_flavor) {
	    case LABEL_SYSTEM:
		buf.append("---- System Label ----\n");
		break;
	    case LABEL_PROPERTY:
		buf.append("---- Property: ");
		break;
	    case LABEL_HISTORY:
		buf.append("---- Task: ");
		break;
	    default:			// shouldn't happen
		buf.append("---- Unknown Label Set: ");
		break;
	}
	if (_flavor != LABEL_SYSTEM) {
	    buf.append(_name);
	    buf.append(" ---- Instance: ");
	    buf.append(_instance);
	    buf.append(" ----\n");
	}

	for (Iterator it = iterator(); it.hasNext(); ) {
	    buf.append(((VicarLabelItem)(it.next())).toString());
	    buf.append("\n");
	}

	return buf.toString();
    }

/***********************************************************************
 * Returns in the supplied StringBuffer the part of the set from
 * <code>start_index</code> to the end as a valid VICAR label string, ready
 * to be put in a file.  If the <code>max_length</code> argument is non-zero,
 * it represents the maximum size the string can become (normally used to
 * make sure the label fits in the space at the front of a file).  This
 * includes any data already in buf when the function is called.  A 0 means
 * no maximum.
 * <p>
 * Note that the caller is responsible for putting in LBLSIZE= at the
 * beginning of each buffer's label segment.
 * <p>
 * @return The first index <em>not</em> put in the string (due to overfilling
 * the capacity); i.e. it is the index you would supply to
 * <code>start_index</code> in the next call.  Returns -1 if all items fit.
 */
    public int toLabelString(StringBuffer buf, int max_length, int start_index)
    {
	for (int i = start_index; i < size(); i++) {
	    String item = getItem(i).toLabelString();
	    if ((max_length > 0) &&
		(buf.length() + item.length() + 2 > max_length)) {
		// Exceeded capacity!
		return i;
	    }
	    buf.append(item);
	    buf.append("  ");	// 2 spaces per convention
	}
	return -1;
    }

/***********************************************************************
 * Returns the entire Set as an XML DOM node (either &lt;system&gt;,
 * &lt;property&gt;, or &lt;history&gt;).  Subnodes will be created for
 * each item as necessary.  See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * <p>
 * If the required header items are not present (<code>TASK</code>,
 * <code>DAT_TIM</code>, etc.), the appropriate attributes are filled in
 * with "<code>UNSET</code>" (or "<code>unset</code>").  However, this should
 * not happen... <code>VicarLabelSet</code> enforces the presence of these
 * items.
 * @param document The <code>Document</code> which will contain the node.
 * @throws IllegalArgumentException if the label's flavor is unknown.
 * @see VicarLabelItem#toXML(Document)
 */
    public Node toXML(Document doc)
    {
	Element set = null;
	VicarLabelItem attr;

	// Create the base node and add the flavor-dependent attributes.

	switch (_flavor) {
	    case LABEL_SYSTEM:
		set = doc.createElement("system");
		break;
	    case LABEL_PROPERTY:
		set = doc.createElement("property");
		attr = getItem("PROPERTY");
		set.setAttribute("name", attr==null ? "UNSET":attr.getString());
		break;
	    case LABEL_HISTORY:
		set = doc.createElement("history");
		attr = getItem("TASK");
		set.setAttribute("task", attr==null ? "UNSET":attr.getString());
		attr = getItem("USER");
		set.setAttribute("user", attr==null ? "unset":attr.getString());
		attr = getItem("DAT_TIM");
		set.setAttribute("dat_tim",attr==null?"unset":attr.getString());
		break;
	    default:			// shouldn't happen
		throw new IllegalArgumentException(
					"VICAR Label set's flavor is invalid");
	}

	for (Iterator it = iterator(); it.hasNext(); ) {

	    VicarLabelItem item = (VicarLabelItem)(it.next());

	    // Skip any items taken care of by attributes above

	    if (item.equals("PROPERTY") || item.equals("TASK") ||
		item.equals("USER") || item.equals("DAT_TIM"))
		continue;

	    Node node = item.toXML(doc);
	    set.appendChild(node);
	}

	return set;
    }

/***********************************************************************
 * Reads the Set from an XML DOM node (either &lt;system&gt;,
 * &lt;property&gt;, or &lt;history&gt;).  Items will be created for
 * each subnode as necessary.  See the VICAR label DTD.
 * !!!!TBD: reference to this DTD!!!!
 * <p>
 * Items found in the XML are added to the set.  This means that existing
 * items will be retained in the output if not overridden.  (the behavior
 * of <code>add()</code> is to replace items with the same keyword).  If you
 * want only the XML items, make sure the Set is empty before calling this
 * function.
 * <p>
 * Note that the label "flavor" (system/property/history) cannot be changed
 * by this routine.  If the supplied DOM node does not match the already-set
 * flavor, a warning is issued but the flavor is <em>not</em> changed.  This
 * means that the header items will very likely be wrong.
 * <p>
 * If anything is wrong with the supplied node, error/warning messages
 * will be appended to the supplied <code>errorList</code>.  However, the
 * parser keeps working as long as possible, despite errors.  The practical
 * upshot of this is that the errors can usually be ignored; the parser will
 * do the best it can.  However, the errors can be displayed to the user
 * if desired.  An exception will be thrown only if something unrecoverable
 * happens.
 * <p>
 * If the required header attributes are not present (<code>TASK</code>,
 * <code>DAT_TIM</code>, etc.), the appropriate label items will be unchanged.
 * This generally means they are set with the value "UNSET" (or "unset")
 * per <code>initItems()</code>.  Reasonable values for these should be
 * supplied before writing the label.  Note that warnings will be issued
 * via <code>errorList</code> if the attributes are missing.
 * <p>
 * @param set The <code>Element</code> from which to extract the item info.
 * @param errorList A list of <code>String</code>s to which any parsing
 * errors will be appended.  Can be <code>null</code>, in which case no
 * errors are returned.
 * @throws DOMException if something unrecoverable happens.  Improperly
 * formatted items or violations of the DTD do not necessarily result in
 * exceptions; parsing continues as long as possible and problems are appended
 * to the <code>errorList</code>.
 *
 * @see VicarLabelItem#fromXML(Element, List)
 * @see #initItems()
 */
    public void fromXML(Element set, List errorList)
    {
	// Check the node type and set the flavor appropriately.

	int new_flavor;
	String tagName = set.getTagName();
	// System.out.println("VicarLabelSet.fromXML tagName "+tagName);
	// System.out.println("VicarLabelSet.fromXML nodeName "+((Node)set).getNodeName());
	
	if (tagName.equalsIgnoreCase("system")) {
	    new_flavor = LABEL_SYSTEM;
	} else if (tagName.equalsIgnoreCase("property")) {
	    new_flavor = LABEL_PROPERTY;
	} else if (tagName.equalsIgnoreCase("history")) {
	    new_flavor = LABEL_HISTORY;
	} else {
	    if (errorList != null)
		errorList.add("Invalid label set name " + tagName + ".  Requires system, property, or history.  Flavor unchanged.");
	    new_flavor = _flavor;
	}

	if (_flavor != new_flavor && errorList != null) {
	    errorList.add("Label set flavor cannot be changed to " + tagName
					+ ".  Headers may be incorrect.");
	}

	// Pull out the attributes (they are different for flavor).

	String value;
	switch (_flavor) {
	    case LABEL_SYSTEM:
		break;			// no attributes
	    case LABEL_PROPERTY:
		value = set.getAttribute("name").toUpperCase();
		if (value.length() == 0) {
		    value = null;
		}
		if (value == null && errorList != null)
		    errorList.add("Property name attribute is missing: " +set.toString());
		if (value != null)
		    add(new VicarLabelItem("PROPERTY", value));
		break;
	    case LABEL_HISTORY:
		value = set.getAttribute("task").toUpperCase();
		if (value.length() ==0)
		    value = null;
		if (value == null && errorList != null)
		    errorList.add("History task name attribute is missing: " +set.toString());
		if (value != null)
		    add(new VicarLabelItem("TASK", value));
		value = set.getAttribute("user");
		if (value.length() ==0)
		    value = null;
		if (value == null && errorList != null)
		    errorList.add(
			"History task's user attribute is missing for task "
							+ getItem("TASK"));
		if (value != null)
		    add(new VicarLabelItem("USER", value));

		value = set.getAttribute("dat_tim");
		if (value.length() ==0)
		    value = null;
		if (value == null && errorList != null)
		    errorList.add(
			"History task's dat_tim attribute is missing for task "
							+ getItem("TASK"));
		if (value != null)
		    add(new VicarLabelItem("DAT_TIM", value));

		break;
	    default:			// shouldn't happen
		throw new IllegalArgumentException(
					"VICAR Label set's flavor is invalid");
	}

	// Now get all the items and stuff them in

	NodeList items = set.getElementsByTagName("item");
	int nitems = items.getLength();

	for (int i=0; i < nitems; i++) {
	    Node item = items.item(i);
	    if (!(item instanceof Element)) {
		if (errorList != null)
		    errorList.add(tagName + " label, item #" + i +
				" is not a DOM Element type");
	    }
	    else {
		add(new VicarLabelItem((Element)item, errorList));
	    }
	}
    }

/***********************************************************************
 * Creates a deep copy of the VicarLabelSet.  Nothing can be done to the
 * returned copy that in any way affects the original.
 */
    public Object clone()
    {
	VicarLabelSet set;
	try {
	    set = (VicarLabelSet)super.clone();
	} catch (Exception e) {
	    return null;
	}

	set._items = (ArrayList)((ArrayList)(set._items)).clone();
	for (int i=0; i < set._items.size(); i++) {
	    set._items.set(i, ((VicarLabelItem)(set._items.get(i))).clone());
	}

	return set;
    }

/***********************************************************************
 * Returns true if this set contains no elements.  Mandated by <code>Set</code>.
 */
    public boolean isEmpty()
    {
	return (size() == 0);
    }

/***********************************************************************
 * Returns true if this set contains the specified element.  Mandated by
 * <code>Set</code>.  The Object may be a <code>VicarLabelItem</code>
 * (only the keyword is matched) or a <code>String</code> (likewise, matches
 * the keyword).
 * <p>
 * In order to allow contains(String), <em>both</em> <code>o.equals(e)</code>
 * and <code>e.equals(o)</code> are tested.  This slightly contradicts the
 * <code>Set</code> contract for <code>contains()</code>.
 */
    public boolean contains(Object o)
    {
	for (Iterator it = iterator(); it.hasNext(); ) {
	    VicarLabelItem e = (VicarLabelItem)it.next();
	    if (o.equals(e))
		return true;
	    if (e.equals(o))	// enables contains(String)
		return true;
	}
	return true;
    }

/***********************************************************************
 * Returns an array containing all of the elements in this set.  Mandated
 * by <code>Set</code>.
 * @see Set#toArray()
 */
    public Object[] toArray()
    {
	return _items.toArray();
    }

/***********************************************************************
 * Returns an array containing all of the elements in this set whose
 * runtime type is that of the specified array.  Mandated by <code>Set</code>.
 * @see Set#toArray(Object[])
 */
    public Object[] toArray(Object[] a)
    {
	return _items.toArray(a);
    }

/***********************************************************************
 * Removes the specified element from this set if it is present.  Mandated
 * by <code>Set</code>.  The item must be a <code>String</code> or
 * <code>VicarLabelItem</code>.  In either case, the keyword is extracted
 * and removeItem(keyword) is called.
 * @throws IllegalArgumentException if a reserved keyword is removed
 * @return true if the set contained the specified element
 * @see #removeItem(String)
 * @see Set#remove(Object)
 */
    public boolean remove(Object o)
    {
	if (o instanceof String)
	    return (removeItem((String) o) != null);
	if (o instanceof VicarLabelItem)
	    return (removeItem(((VicarLabelItem)o).getKeyword()) != null);
	return false;
    }

/***********************************************************************
 * Returns true if this set contains all of the elements of the specified
 * collection.  Mandated by <code>Set</code>.
 * @see Set#containsAll(Collection)
 */
    public boolean containsAll(Collection c)
    {
	return _items.containsAll(c);
    }

/***********************************************************************
 * Adds all of the elements in the specified collection to this set.
 * Replaces any items already present as in <code>add()</code>.  Mandated
 * by <code>Set</code>.  All items in the provided Collection must be
 * <code>VicarLabelItem</code>s.
 */
    public boolean addAll(Collection c)
    {
	boolean changed = false;
	// Exceptions are thrown automatically by the cast, and by add()
	for (Iterator it = c.iterator(); it.hasNext(); ) {
	    VicarLabelItem item = (VicarLabelItem)it.next();
	    add(item);
	    changed = true;
	}
	return changed;
    }

/***********************************************************************
 * <code>retainAll</code> is not supported by <code>VicarLabelSet</code>.
 * Specified by <code>Set</code>.
 */
    public boolean retainAll(Collection c)
    {
	throw new UnsupportedOperationException(
		"retainAll() not supported by VicarLabelSet");
    }

/***********************************************************************
 * <code>removeAll</code> is not supported by <code>VicarLabelSet</code>.
 * Specified by <code>Set</code>.
 */
    public boolean removeAll(Collection c)
    {
	throw new UnsupportedOperationException(
		"removeAll() not supported by VicarLabelSet");
    }

/***********************************************************************
 * <code>clear</code> is not supported by <code>VicarLabelSet</code>.
 * Completely clearing a history or property label is not possible due to
 * the required keywords.  Specified by <code>Set</code>.
 */
    public void clear()
    {
	throw new UnsupportedOperationException(
		"clear() not supported by VicarLabelSet");
    }

/***********************************************************************
 * Returns the hash code value for this set.  The hash code is defined as
 * the sum of the hash codes of the keywords in order to be consistent with
 * <code>equals()</code>.  See the specification in <code>Set</code>
 * @see Set#hashCode()
 */
    public int hashCode()
    {
	int hash = 0;
	for (Iterator it = iterator(); it.hasNext(); ) {
	    VicarLabelItem item = (VicarLabelItem)it.next();
	    hash += item.getKeyword().hashCode();
	}
	return hash;
    }

}

