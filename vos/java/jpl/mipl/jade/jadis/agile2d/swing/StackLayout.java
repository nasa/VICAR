/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import java.util.Hashtable;
import java.util.Enumeration;
import java.awt.*;


/**
 * A simple layout manager that forces the children to be the same size and to
 * occupy the full available area of the parent. 
 *
 * Used to stack the GL Canvas under the Swing component.
 */ 
class StackLayout implements LayoutManager2, java.io.Serializable {
    Hashtable tab = new Hashtable();

    public StackLayout() { }


    /**
     * Adds the specified component to this layout's internal
     * table of names. The object specified by <code>constraints</code>
     * must be a string.
     * @param     comp          the component to be added.
     * @param     constraints   a tag that identifies a particular
     *                                        component in the layout.
     * @exception  IllegalArgumentException  if the constraint is not a string.
     */
    public void addLayoutComponent(Component comp, Object constraints) {
      synchronized (comp.getTreeLock()) {
		if (constraints instanceof String) {
		    addLayoutComponent((String)constraints, comp);
		} else {
		    throw new IllegalArgumentException("cannot add to layout: constraint must be a string");
		}
      }
    }

    /**
     * @deprecated   replaced by
     *      <code>addLayoutComponent(Component, Object)</code>.
     */
    public void addLayoutComponent(String name, Component comp) {
      synchronized (comp.getTreeLock()) {
		tab.put(name, comp);
      }
    }

    /**
     * Removes the specified component from the layout.
     * @param   comp   the component to be removed.
     * @see     java.awt.Container#remove(java.awt.Component)
     * @see     java.awt.Container#removeAll()
     */
    public void removeLayoutComponent(Component comp) {
      synchronized (comp.getTreeLock()) {
		for (Enumeration e = tab.keys() ; e.hasMoreElements() ; ) {
		    String key = (String)e.nextElement();
		    if (tab.get(key) == comp) {
				tab.remove(key);
				return;
		    }
		}
      }
    }

    /**
     * Determines the preferred size of the container argument using
     * this layout.
     * @param   parent the name of the parent container.
     * @return  the preferred dimensions to lay out the subcomponents
     *                of the specified container.
     * @see     java.awt.Container#getPreferredSize
     * @see     java.awt.StackLayout#minimumLayoutSize
     */
    public Dimension preferredLayoutSize(Container parent) {
      synchronized (parent.getTreeLock()) {
		Insets insets = parent.getInsets();
		int ncomponents = parent.getComponentCount();
		int w = 0;
		int h = 0;

		for (int i = 0 ; i < ncomponents ; i++) {
		    Component comp = parent.getComponent(i);
		    Dimension d = comp.getPreferredSize();
		    if (d.width > w) {
				w = d.width;
		    }
		    if (d.height > h) {
				h = d.height;
		    }
		}
		return new Dimension(insets.left + insets.right + w,
						     insets.top + insets.bottom + h);
      }
    }

    /**
     * Calculates the minimum size for the specified panel.
     * @param     parent the name of the parent container
     *                in which to do the layout.
     * @return    the minimum dimensions required to lay out the
     *                subcomponents of the specified container.
     * @see       java.awt.Container#doLayout
     * @see       java.awt.StackLayout#preferredLayoutSize
     */
    public Dimension minimumLayoutSize(Container parent) {
      synchronized (parent.getTreeLock()) {
		Insets insets = parent.getInsets();
		int ncomponents = parent.getComponentCount();
		int w = 0;
		int h = 0;

		for (int i = 0 ; i < ncomponents ; i++) {
		    Component comp = parent.getComponent(i);
		    Dimension d = comp.getMinimumSize();
		    if (d.width > w) {
				w = d.width;
		    }
		    if (d.height > h) {
				h = d.height;
		    }
		}
		return new Dimension(insets.left + insets.right + w,
						     insets.top + insets.bottom + h);
      }
    }

    /**
     * Returns the maximum dimensions for this layout given the components
     * in the specified target container.
     * @param target the component which needs to be laid out
     * @see Container
     * @see #minimumLayoutSize
     * @see #preferredLayoutSize
     */
    public Dimension maximumLayoutSize(Container target) {
		return new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE);
    }

    /**
     * Returns the alignment along the x axis.  This specifies how
     * the component would like to be aligned relative to other
     * components.  The value should be a number between 0 and 1
     * where 0 represents alignment along the origin, 1 is aligned
     * the furthest away from the origin, 0.5 is centered, etc.
     */
    public float getLayoutAlignmentX(Container parent) {
		return 0.5f;
    }

    /**
     * Returns the alignment along the y axis.  This specifies how
     * the component would like to be aligned relative to other
     * components.  The value should be a number between 0 and 1
     * where 0 represents alignment along the origin, 1 is aligned
     * the furthest away from the origin, 0.5 is centered, etc.
     */
    public float getLayoutAlignmentY(Container parent) {
		return 0.5f;
    }

    /**
     * Invalidates the layout, indicating that if the layout manager
     * has cached information it should be discarded.
     */
    public void invalidateLayout(Container target) {
    }

    /**
     * Lays out the specified container using this layout.
     * <p>
     * Each component in the <code>parent</code> container is reshaped
     * to be the size of the container, minus space for surrounding
     * insets, horizontal gaps, and vertical gaps.
     *
     * @param     parent the name of the parent container
     *                             in which to do the layout.
     * @see       java.awt.Container#doLayout
     */
    public void layoutContainer(Container parent) {
      synchronized (parent.getTreeLock()) {
		Insets insets = parent.getInsets();
		Dimension sz = parent.getSize();
		int ncomponents = parent.getComponentCount();
		for (int i = 0 ; i < ncomponents ; i++) {
		    Component comp = parent.getComponent(i);
		    comp.setBounds(insets.left, insets.top,
						   sz.width - (insets.left + insets.right),
						   sz.height - (insets.top + insets.bottom));
      }
		  }
    }

    String shortName(String name) {
		    int pos = name.lastIndexOf('.');
				if (pos == -1) return name;
				else return name.substring(pos + 1);
		}

    void dump(Component c, int indent) {
        for (int i = 0; i < indent; i++) System.out.print(" ");
				System.out.print(shortName(c.getClass().getName()));
				Rectangle r = c.getBounds();
				System.out.println(" " + r.x + " " + r.y + " " + r.width + " " + r.height);
				if (c instanceof Container) {
						Container cc = (Container)c;
						for (int j = 0; j < cc.getComponentCount(); j++) {
								dump(cc.getComponent(j), indent+2);
						}
				}
		}
    /**
     * Make sure that the Container really has a StackLayout installed.
     * Otherwise havoc can ensue!
     */
    void checkLayout(Container parent) {
		if (parent.getLayout() != this) {
		    throw new IllegalArgumentException("wrong parent for StackLayout");
		}
    }


    /**
     * Returns a string representation of the state of this stack layout.
     * @return    a string representation of this layout.
     */
    public String toString() {
		    return getClass().getName();
    }
}
