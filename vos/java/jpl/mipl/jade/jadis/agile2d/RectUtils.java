/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;

import java.awt.Rectangle;

/**
 * Implements more efficient Rectangle operations than are defined in AWT.
 */
class RectUtils {

    private static int min(int a, int b) { return (a < b ? a : b); }
    private static int max(int a, int b) { return (a > b ? a : b); }

    /**
	 *  More efficient version of Sun's Rectangle intersection() method
     */
    static Rectangle intersect(Rectangle r1, Rectangle r2, Rectangle result) {
    	int x1 = max(r1.x, r2.x);
	    int x2 = min(r1.x + r1.width, r2.x + r2.width);
	    int y1 = max(r1.y, r2.y);
	    int y2 = min(r1.y + r1.height, r2.y + r2.height);

        if (((x2 - x1) < 0) || ((y2 - y1) < 0))
            // Width or height is negative. No intersection.
            result.x = result.y = result.width = result.height = 0;
        else {
	        result.x = x1; 
            result.y = y1; 
            result.width = x2 - x1;
            result.height = y2 - y1;
        }
        return result;
    }

    /**
	 * Fixes bug in Sun's Rectangle implementation where the
     * contains() method always returns false if x/y is <= 0.
     */
    static boolean containsOrEquals(Rectangle outer, Rectangle inner) {
    	int width = outer.width;
	    int height = outer.height;
	    int x = outer.x;
	    int y = outer.y;

	    return (inner.x >= x && inner.y >= y &&
		        (inner.x+inner.width) <= x + width &&
		        (inner.y+inner.height) <= y + height);
    }

    /** 
	 * Computes union of rect and (x,y,width,height) without having to
	 * allocate a new Rectangle instance.
	 */
	static void union(Rectangle rect, int x, int y, int width, int height) {
		int x1 = min(x, rect.x);
		int y1 = min(y, rect.y);
		int x2 = max(x + width, rect.x + rect.width);
		int y2 = max(y + height, rect.y + rect.height);

		rect.setBounds(x1, y1, x2-x1, y2-y1);
	}
}
