/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;

/**
 * Takes a PathIterator and generates a new path iterator which applies an AffineTransform 
 * to the first path.
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.2 $
 */
public class TransformPathIterator implements PathIterator {
	PathIterator iter;
	AffineTransform transform;

	/**
	 * Constructor for TransformPathIterator.
	 */
	public TransformPathIterator(PathIterator iter, AffineTransform transform) {
		this.iter = iter;
		this.transform = transform;
	}
	
	

	/**
	 * @see java.awt.geom.PathIterator#currentSegment(double[])
	 */
	public int currentSegment(double[] coords) {
		int type = iter.currentSegment(coords);
		switch(type) {
		case SEG_MOVETO:
		case SEG_LINETO:
			transform.transform(coords, 0, coords, 0, 1);
			break;
		case SEG_QUADTO:
			transform.transform(coords, 0, coords, 0, 2);
			break;
		case SEG_CUBICTO:
			transform.transform(coords, 0, coords, 0, 3);
			break;
		case SEG_CLOSE:
		}
		return type;
	}

	/**
	 * @see java.awt.geom.PathIterator#currentSegment(float[])
	 */
	public int currentSegment(float[] coords) {
		int type = iter.currentSegment(coords);
		switch(type) {
		case SEG_MOVETO:
		case SEG_LINETO:
			transform.transform(coords, 0, coords, 0, 1);
			break;
		case SEG_QUADTO:
			transform.transform(coords, 0, coords, 0, 2);
			break;
		case SEG_CUBICTO:
			transform.transform(coords, 0, coords, 0, 3);
			break;
		case SEG_CLOSE:
		}
		return type;
	}

	/**
	 * @see java.awt.geom.PathIterator#getWindingRule()
	 */
	public int getWindingRule() {
		return iter.getWindingRule();
	}

	/**
	 * @see java.awt.geom.PathIterator#isDone()
	 */
	public boolean isDone() {
		return iter.isDone();
	}

	/**
	 * @see java.awt.geom.PathIterator#next()
	 */
	public void next() {
		iter.next();
	}

}
