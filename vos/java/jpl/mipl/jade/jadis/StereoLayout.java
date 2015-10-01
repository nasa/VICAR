package jpl.mipl.jade.jadis;

import java.awt.*;

import javax.swing.*;

/**
 * A layout manager that manages two children as "left" and "right" views.
 * It extends Swing's {@link javax.swing.OverlayLayout} which allows
 * multiple children to be displayed simultaneously.  Users don't directly instantiate
 * this class.
 * 
 *
 * Used by {@link jpl.mipl.jade.jadis.StereoJPanel}.
 * 
 * 
 * @see javax.swing.OverlayLayout
 * @see jpl.mipl.jade.jadis.StereoJPanel
 */ 
public class StereoLayout extends OverlayLayout {
 
	/**
	 * Constant to specify components location to be the left view of Stereo Layout
	 */
	Component _left;
	/**
	 * Constant to specify components location to be the right view of Stereo Layout
	 */
	Component _right;
	/**
	 * Constant to specify components location to be the only view or both of Stereo Layout
	 */
	Component _both;
	/**
	 * Constant to specify components location to be the left view of Stereo Layout.
	 */
    public static final String LEFT  = "Left";
	/**
	 * Constant to specify components location to be the right view of Stereo Layout.
	 */ 
    public static final String RIGHT = "Right";
	/**
	 * Constant to specify components location to be the only view or both of Stereo Layout.
	 */ 
    public static final String BOTH = "both";

 
    /**
     * Default constructor, just calls superclass.
     * @param target Container
     */
    public StereoLayout(Container target) {
    	super(target);
    }

    /**
     * Adds the specified component to the layout, using the specified
     * constraint object.  For Stereo layouts, the constraint must be
     * one of the following constants: <code>LEFT</code>,
     * <code>RIGHT</code>, <code>BOTH</code>
     * <p> 
     * This method
     * This method is called when a component is added to a container 
     * using the <code>Container.add</code> method with the same 
     * argument types.
     * @param   comp         the component to be added.
     * @param   constraints  an object that specifies how and where
     *                       the component is added to the layout.
     * @see     java.awt.Container#add(java.awt.Component, java.lang.Object)
     * @exception   IllegalArgumentException  if the constraint object is not
     *                 a string, or if it not one of the five specified
     *              constants.
     * @since   JDK1.1
     */
    public void addLayoutComponent(Component comp, Object constraints) {
      synchronized (comp.getTreeLock()) {
		if ((constraints == null) || constraints instanceof String) {
		    addLayoutComponent((String)constraints, comp);
		} else {
		    throw new IllegalArgumentException("cannot add to layout: constraint must be a string");
		}
      }
    }
    /**
     * @deprecated  replaced by <code>addLayoutComponent(Component, Object)</code>.
     */
    public void addLayoutComponent(String name, Component comp) {
      synchronized (comp.getTreeLock()) {
        /* Special case:  treat null the same as "Mono". */
        if (name == null) {
            name = "Both";
        }
        /* Assign the component to one of the known regions of the layout.
        */
        if ("Both".equals(name)) {
        	_both = comp;
        } else if ("Left".equals(name)) {
            _left = comp;
        } else if ("Right".equals(name)) {
            _right = comp;
        } else {
            throw new IllegalArgumentException("cannot add to layout: unknown constraint: " + name);
        }
        invalidateLayout(comp.getParent());
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
          if (comp == _both) {
              _both = null;
          } else if (comp == _left) {
              _left = null;
          } else if (comp == _right) {
              _right = null;
          }
          invalidateLayout(comp.getParent());
      }
    }
    /**
     * Gets the component that corresponds to the given constraint location
     * based on the target Container's component orientation
     *
     * @param   constraints     the desired absolute position, one of <code>MONO</code>,
     *                          one of <code>LEFT</code>, <code>RIGHT</code>,
     * @param   target     the <code>Container</code> using this <code>BorderLayout</code>
     * @return  the component at the given location, or </code>null</code> if
     *          the location is empty
     * @exception   IllegalArgumentException  if the constraint object is
     *              not one of the five specified constants
     * @exception   NullPointerException  if the target parameter is null
     * @see     #addLayoutComponent(java.awt.Component, java.lang.Object)
     * @since 1.5
     */
    public Component getLayoutComponent(Container target, Object constraints) {
        boolean ltr = target.getComponentOrientation().isLeftToRight();
        Component result = null;

        if (BOTH.equals(constraints)) {
            result = _both;
        } else if (LEFT.equals(constraints)) {
            result = _left;
        } else if (RIGHT.equals(constraints)) {
            result = _right;
        } else {
            throw new IllegalArgumentException("cannot get component: invalid constraint: " + constraints);
        }
        return result;
    }
    /**
     * Gets the constraints for the specified component
     *
     * @param   comp the component to be queried
     * @return  the constraint for the specified component,
     *          or null if component is null or is not present
     *          in this layout
     * @see #addLayoutComponent(java.awt.Component, java.lang.Object)
     * @since 1.5
     */
    public Object getConstraints(Component comp) {
        if (comp == _both) {
            return BOTH;
        } else if (comp == _left) {
            return LEFT;
        } else if (comp == _right) {
            return RIGHT;
        } 
        return null;
    }
    /**
     * Get the component that corresponds to the given constraint location
     *
     * @param   key     The desired absolute position,
     *                  either BOTH, LEFT, RIGHT
     */
    private Component getChild(String key) {
        Component result = null;

        if (key == BOTH) {
            result = _both;
        }
        else if (key == LEFT) {
            result = _left;
        }
        else if (key == RIGHT) {
            result = _right;
        }
        return result;
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
     * Make sure that the Container really has a StereoLayout installed.
     * Otherwise havoc can ensue!
     */
    void checkLayout(Container parent) {
		if (parent.getLayout() != this) {
		    throw new IllegalArgumentException("wrong parent for StereoLayout");
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
