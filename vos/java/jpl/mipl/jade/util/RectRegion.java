package jpl.mipl.jade.util;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.*;
import java.util.ArrayList;
import jpl.mipl.jade.*;

/**
 * A class that consists of a collection of <code>Rectangle</code>s.
 * The rectangles may touch each other but they may not overlap.
 * <p>
 * One use of this class is to maintain a "damage list" of rectangular
 * areas that need to be repainted on a display.  As each tile is repainted,
 * it can be subtracted from the damage list.
 * <p>
 * This functionality was largely lifted from the XvicRegion code in VICAR's
 * Image Widget.
 * <p>
 * Although Rectangle derives from Rectangle2D, we base this in Rectangle
 * to allow direct, fast access to the x/y/width/height fields (rather than
 * go through access functions).  This requires some conversions from
 * Rectangle2D to Rectangle at various places which would not be otherwise
 * needed if we implemented to the Rectangle2D spec.
 * <p>
 * TBD: This class "should" implement <code>Shape</code> in order to be more
 * generally useful.  However, this is not currently done, because the
 * <code>getPathIterator()</code> functions were non-trivial and unnecessary
 * for the intended use.  If a Shape is needed, these functions could be
 * implemented.
 *
 * @author Bob Deen, JPL
 */
public class RectRegion implements Cloneable /*!!!!implements Shape!!!!*/
{

    /** The list of Rectangles */
    protected ArrayList _list;

/***********************************************************************
 * Creates an empty region
 */
    public RectRegion()
    {
	this(10);
    }

/***********************************************************************
 * Creates an empty region with the given initial capacity
 */
    public RectRegion(int n)
    {
	_list = new ArrayList(n);
    }

/***********************************************************************
 * Creates a region from a single rectangle.
 */
    public RectRegion(Rectangle rect)
    {
	this();
	addRectangle(new Rectangle(rect));
    }

/***********************************************************************
 * Creates and returns a new region by intersecting a rectangle with this
 * region.  More efficient than copying the region and then intersecting.
 */
    public RectRegion createIntersect(Rectangle rect)
    {
	RectRegion new_rgn = new RectRegion();

	int max = _list.size();
	for (int i=0; i < max; i++) {

	    // First clip each box to the rectangle

	    Rectangle temp_rect = rect.intersection((Rectangle)_list.get(i));

	    // If the box has not been fully clipped (it's non-empty),
	    // then add it into the destination.

	    if (temp_rect.width != 0 && temp_rect.height != 0)
		new_rgn.addRectangle(temp_rect);
	}

	return new_rgn;
    }

/***********************************************************************
 * Returns an array containing the rectangles in the region.  This is
 * a copy of the internal array so modifying it will not affect this
 * object.  The number of rectangles can be obtained via array.length.
 */
    public Rectangle[] getRectangles()
    {
	return (Rectangle[])_list.toArray(new Rectangle[_list.size()]);
    }

/***********************************************************************
 * Returns a <code>Rectangle</code> that completely encloses the shape
 * (the bounding box).  For an empty shape, a new Rectangle(0,0,0,0) is
 * returned.
 * @see Shape#getBounds()
 */
    public Rectangle getBounds()
    {
	if (_list.size() == 0)
	    return new Rectangle(0,0,0,0);

	// x2/y2 should be e.g. x+width-1 but we dispense with the -1 because
	// it all cancels out.

	int x1 = Integer.MAX_VALUE;
	int x2 = Integer.MIN_VALUE;
	int y1 = Integer.MAX_VALUE;
	int y2 = Integer.MIN_VALUE;

	int max = _list.size();
	for (int i=0; i < max; i++) {
	    Rectangle r = (Rectangle)_list.get(i);
	    if (r.x < x1)
		x1 = r.x;
	    if (r.x + r.width > x2)
		x2 = r.x + r.width;
	    if (r.y < y1)
		y1 = r.y;
	    if (r.y + r.height > y2)
		y2 = r.y + r.height;
	}

	return new Rectangle(x1, y1, x2-x1, y2-y1);
    }

/***********************************************************************
 * Returns a <code>Rectangle2D</code> that completely encloses the shape
 * (the bounding box).  Identical to <code>getBounds</code> except for
 * the return type.  (it actually calls <code>getBounds()</code>,
 * so the other function is more efficient).
 * @see Shape#getBounds2D()
 * @see #getBounds()
 */
    public Rectangle2D getBounds2D()
    {
	return getBounds();		// Rectangle extends Rectangle2D (!)
    }

/***********************************************************************
 * Returns true if the region is empty, false if not.
 */
    public boolean isEmpty()
    {
	return (_list.size() == 0);
    }

/***********************************************************************
 * Returns true if any part of the region is inside the given rectangle.
 */
    public boolean intersects(Rectangle rect)
    {
	int max = _list.size();
	for (int i=0; i < max; i++) {
	    if (rect.intersects((Rectangle)_list.get(i)))
		return true;
	}
	return false;
    }

/***********************************************************************
 * Returns true if any part of the region is inside the given rectangular
 * region.  Mandated by the Shape interface.  Calls
 * <code>intersects(Rectangle)</code> so that routine is more efficient.
 * @see #intersects(Rectangle)
 */
    public boolean intersects(double x, double y, double w, double h)
    {
	return intersects(new Rectangle((int)x, (int)y, (int)w, (int)h));
    }

/***********************************************************************
 * Returns true if any part of the region is inside the given Rectangle2D.
 * Mandated by the Shape interface.  Calls
 * <code>intersects(Rectangle)</code> so that routine is more efficient.
 * @see #intersects(Rectangle)
 */
    public boolean intersects(Rectangle2D rect)
    {
	return intersects(new Rectangle((int)rect.getX(), (int)rect.getY(),
			(int)rect.getWidth(), (int)rect.getHeight()));
    }

/***********************************************************************
 * Modifies the current region by intersecting it with the given
 * rectangle.  This discards any part of the region outside the rectangle.
 */
    public void intersect(Rectangle rect)
    {
	int max = _list.size();
	ArrayList new_list = new ArrayList(max+10);

	for (int i=0; i < max; i++) {

	    // First clip each box to the rectangle

	    Rectangle temp_rect = rect.intersection((Rectangle)_list.get(i));

	    // If the box has not been fully clipped (it's not empty),
	    // then copy it to the new list.

	    if (temp_rect.width != 0 && temp_rect.height != 0)
		new_list.add(temp_rect);
	}

	_list = new_list;
    }

/***********************************************************************
 * Adds an offset to all coordinates in this region.
 */
    public void translate(int xoff, int yoff)
    {
	int max = _list.size();
	for (int i=0; i < max; i++) {
	    ((Rectangle)_list.get(i)).translate(xoff, yoff);
	}
    }

/***********************************************************************
 * Subtracts a rectangle from the region, leaving behind only parts of
 * the region that are outside the rectangle.  The general idea here is
 * that Y clipping is done first, then X.  Any box that straddles the edge
 * of the rectangle is split into two.  The first is completely clippd;
 * the second is left until its turn in the loop to be clipped.
 */
    public void subtract(Rectangle rect)
    {

	// Note:  This code actually changes the size of the array as we
	// loop through it.  Therefore it depends on _list.size() being called
	// on each iteration, and on the ability to modify i inside the loop.

	// The original code from Xiw used x1,y1,x2,y2 instead of
	// x,y,width,height.  At some places those terms are used in comments;
	// +/-1's often cancel out.

	int rect_x2 = rect.x + rect.width - 1;
	int rect_y2 = rect.y + rect.height - 1;

	for (int i=0; i < _list.size(); i++) {

	    Rectangle r = (Rectangle)_list.get(i);

	    // First subtract the Y direction

	    if ((r.y < rect.y && (r.y+r.height-1) >= rect.y) &&
		((r.x+r.width-1) >= rect.x && r.x <= rect_x2)) {

		// Straddles top edge and there is an X intersect, so clip old
		// box to top part and create new box for the rest.

						// y2 unchanged but y1 changed
		addRectangle(new Rectangle(r.x, rect.y,
					r.width, r.y+r.height-rect.y));
		r.height = rect.y - r.y;	// y2=rect.y1 - 1
	    }

	    if ((r.y <= rect_y2 && (r.y+r.height-1) > rect_y2) &&
		((r.x+r.width-1) >= rect.x && r.x <= rect_x2)) {

		// Straddles bottom edge and there is an X intersect, so clip
		// old box to top part and create new box for the bottom.

						// y2 unchanged but y1 changed
		addRectangle(new Rectangle(r.x, rect_y2+1,
					r.width, r.y+r.height-rect_y2-1));
		r.height = rect_y2 - r.y + 1;	// y2 = rect.y2
	    }

	    // Now subtract the X direction.  We know that Y is already clipped.

	    if ((r.x < rect.x && (r.x+r.width-1) >= rect.x) &&
		(r.y >= rect.y && (r.y+r.height-1) <= rect_y2)) {

		// Straddles left edge and there is a Y intersect, so clip
		// old box to left part and create new box for the rest.

						// x2 unchanged
		addRectangle(new Rectangle(rect.x, r.y,
					r.x+r.width-rect.x, r.height));
		r.width = rect.x - r.x;	 	// x2 = rect.x1 - 1
	    }

	    if ((r.x <= rect_x2 && (r.x+r.width-1) > rect_x2) &&
		(r.y >= rect.y && (r.y+r.height-1) <= rect_y2)) {

		// Straddles right edge and there is a Y intersect, so clip
		// old box to left part and create new box for the right.

						// x2 unchanged
		addRectangle(new Rectangle(rect_x2+1, r.y,
					r.x+r.width-rect_x2-1, r.height));
		r.width = rect_x2 - r.x + 1;	// x2 = rect.x2
	    }

	    // Now that the box is completely clipped, see if it is
	    // inside the rectangle.  If so, delete it.

	    if (r.x >= rect.x && (r.x+r.width-1) <= rect_x2 &&
		r.y >= rect.y && (r.y+r.height-1) <= rect_y2) {

		_list.remove(i);
		i--;		// Compensate for i++ to re-do this index
	    }
	}
    }

/***********************************************************************
 * Add (logical union) a rectangle into a region.
 * <p>
 * Possible optimization not yet added: merge rects with matching y2 but
 * different y1's, and put the excess y into a new rect.
 */
    public void union(Rectangle rect)
    {

    // Check for an easy merge: if y's match and x's overlap for any rect,
    // just merge the rects and we're done.  This is a common case due to
    // the way the tiles work.

    int max = _list.size();
    for (int i=0; i < max; i++) {		// check for an easy merge
	Rectangle r = (Rectangle)_list.get(i);
	if (r.y == rect.y && r.height == rect.height) {

	    // Y height matches, check for L side
	    // (the first clause below is rect.x2+1 so we didn't forget the -1)
	    if ((rect.x+rect.width) >= r.x && rect.x < r.x) {
		int x2 = Math.max(r.x+r.width, rect.x+rect.width); //+-1 cancels
		r.x = rect.x;			// left side merge
		r.width = x2 - r.x;
		return;
	    }

	    // Now check for R side
	    if (rect.x <= (r.x+r.width) && (rect.x+rect.width >= r.x+r.width)) {
		r.x = Math.min(r.x, rect.x);
		r.width = rect.x+rect.width-r.x;		// rect.x2
		return;
	    }
	}
    }

    // Not easy, do it the hard way by subtracting the rect from the region
    // (to prevent overlaps), then adding the new rect.

    subtract(rect);
    addRectangle(new Rectangle(rect));
}

/***********************************************************************
 * Appends a rectangle to the region.  Unlike union, this assumes that
 * the new rectangle has no overlaps with the others in the region.  To
 * enforce this, the routine is protected.  The rectangle is NOT copied.
 */
    protected void addRectangle(Rectangle rect)
    {
	_list.add(rect);
    }

/***********************************************************************
 * Tests if the specified coordinates are inside the boundary of the region.
 * @see Shape#contains(double, double)
 */
    public boolean contains(double x, double y)
    {
	int xx = (int)x;
	int yy = (int)y;
	int max = _list.size();
	for (int i=0; i < max; i++) {
	    if (((Rectangle)_list.get(i)).contains(xx, yy))
		return true;
	}
	return false;
    }

/***********************************************************************
 * Tests if a specified <code>Point2d</code> is inside the boundary of
 * the region.
 * @see Shape#contains(Point2D)
 */
    public boolean contains(Point2D p)
    {
	return contains(p.getX(), p.getY());
    }

/***********************************************************************
 * Tests if the specified rectangular area is entirely contained in the
 * region (with no holes).  Not implemented very efficiently.
 * @see Shape#contains(double, double, double, double)
 */
    public boolean contains(double x, double y, double w, double h)
    {
	Rectangle rect = new Rectangle((int)x, (int)y, (int)w, (int)h);

	// Calculate the intersection.  If it's not the same size as the
	// rectangle, we know we're bad.

	RectRegion intersect = createIntersect(rect);
	Rectangle inter_bounds = intersect.getBounds();
	if (inter_bounds.x != rect.x || inter_bounds.y != rect.y ||
	    inter_bounds.width != rect.width ||
	    inter_bounds.height != rect.height) {

	    return false;			// no good
	}

	// Now calculate the area of all the rectangles in the intersection.
	// If it doesn't match the area of the rect, we know we're bad.
	// This is a valid test, given that the intersection is the same
	// size, and the fact that no rectangles overlap within the region.

	Rectangle[] rects = intersect.getRectangles();
	long area = 0;

	for (int i=0; i<rects.length; i++) {
	    area += (rects[i].width * (long)rects[i].height);
	}

	if (area == (rect.width * (long)rect.height))
	    return true;			// good!

	return false;
    }

/***********************************************************************
 * Tests if the specified <code>Rectangle2D</code> is entirely contained in
 * the region (with no holes).  Implemented in terms of
 * <code>contains(double,double,double,double)</code>.
 * @see Shape#contains(Rectangle2D)
 * @see #contains(double,double,double,double)
 */
    public boolean contains(Rectangle2D rect)
    {
	return contains(rect.getX(), rect.getY(),
			rect.getWidth(), rect.getHeight());
    }

/***********************************************************************
 * Returns an iterator object that iterates along the boundary of the
 * region and provides access to the geometry of the outline.  If an
 * optional <code>AffineTransform</code> is specified, the coordinates
 * returned in the iteration are transformed accordingly.
 * @see Shape#getPathIterator(AffineTransform)
 */
//!!!!    public PathIterator getPathIterator(AffineTransform)

/***********************************************************************
 * Exactly like <code>getPathIterator(AffineTransform)</code>; the
 * <code>flatness</code> argument is ignored.
 */
//!!!!    public PathIterator getPathIterator(AffineTransform, double flatness)

/***********************************************************************
 * "Prints" a list of all the rectangles in the region to a string.
 */
    public String toString()
    {
	StringBuffer buf = new StringBuffer();
	int max = _list.size();
	buf.append("RectRegion: # of rects="+max);
	for (int i=0; i < max; i++) {
	    buf.append("\n rect ");
	    buf.append(i);
	    buf.append(" = ");
	    buf.append(((Rectangle)_list.get(i)).toString());
	}
	return buf.toString();
    }

/***********************************************************************
 * Clones the region.  This is a deep copy; no objects are shared between
 * the original and the clone.
 */
    public Object clone()
    {
	int n = _list.size();
	RectRegion new_rgn = new RectRegion(n);

	for (int i=0; i < n; i++) {
	    Rectangle oldr = (Rectangle)_list.get(i);
	    new_rgn.addRectangle((Rectangle)(oldr.clone()));
	}
	return new_rgn;
    }

}

