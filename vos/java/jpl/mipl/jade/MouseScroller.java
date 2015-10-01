package jpl.mipl.jade;

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import javax.swing.event.*;

/**
 * A component that implements mouse scrolling for a <code>JViewport</code>.
 * It responds to click-hold-drag of the mouse (left button) over the
 * viewport and scrolls it appropriately.  It can be attached to any
 * <code>JViewport</code> but is usually used with <code>JScrollPane</code>
 * on an image display.  However, because it generates diagonal scroll
 * events (simultaneous X and Y changes), you may want to use it with
 * <code>JViewportImage</code>.
 * <p>
 * TBD: add property to control whether the scroll is constrained to the
 * display or not.
 * <p>
 * TBD: add property to turn on/off scrolling (to disable it).  This can
 * be done now by unregistering the listener but that is less convenient.
 *
 * @author Bob Deen, JPL
 */

public class MouseScroller extends MouseInputAdapter
{
    /** The JViewport we're controlling */
    protected JViewport _viewport;

    /** The position of the mouse last time */
    protected int _mouseX, _mouseY;

/***********************************************************************
 * Constructs a MouseScroller to control the given viewport.
 */
    public MouseScroller(JViewport viewport)
    {
	_viewport = viewport;
	_viewport.addMouseListener(this);
	_viewport.addMouseMotionListener(this);
    }

/***********************************************************************
 * Responds to the mousePressed event, which initiates a scroll.
 */
    public void mousePressed(MouseEvent me)
    {
	_mouseX = me.getX();
	_mouseY = me.getY();
    }

/***********************************************************************
 * Responds to the mouseDragged event, which actually does a scroll.
 */
    public void mouseDragged(MouseEvent me)
    {
	// Get the delta from the last position

	int x = me.getX();
	int y = me.getY();

	if (x == _mouseX && y == _mouseY)
	    return;				// nothing to do(!)

	// Note:  the deltas seem backwards but that makes the scroll direction
	// feel right (as in, grab the image and drag it)

	int delta_x = _mouseX - x;
	int delta_y = _mouseY - y;

	// Get the current view position and add the delta

	Point p = _viewport.getViewPosition();
	p.translate(delta_x, delta_y);

	// Make sure we don't scroll out of bounds

	Dimension d = _viewport.getViewSize();		// size of image
	int w = _viewport.getWidth();			// size of display
	if (p.x + w > d.width)
	    p.x = d.width - w;
	int h = _viewport.getHeight();			// size of display
	if (p.y + h > d.height)
	    p.y = d.height - h;
	if (p.x < 0)
	    p.x = 0;
	if (p.y < 0)
	    p.y = 0;

	// Now set that position back

	_viewport.setViewPosition(p);

	// Save the current values for next time, and we're done!

	_mouseX = x;
	_mouseY = y;

    }

}

