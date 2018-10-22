/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;

import jpl.mipl.jade.jadis.agile2d.geom.PolygonShape;

/**
 * Class TriangleShape
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.2 $
 */
public class TriangleShape extends PolygonShape {
    
    public TriangleShape() {
        super(3, true);
    }
    public TriangleShape(float[] vertices, int index) {
        super(vertices, index, true);
    }
    
    public int getSize() {
        return 3;
    }

	public float getX0() { return getXAt(0); }
	public void setX0(float v) { setXAt(0, v); }
	public float getY0() { return getYAt(0); }
	public void setY0(float v) { setYAt(0, v); }

	public float getX1() { return getXAt(1); }
	public void setX1(float v) { setXAt(1, v); }
	public float getY1() { return getYAt(1); }
	public void setY1(float v) { setYAt(1, v); }

	public float getX2() { return getXAt(2); }
	public void setX2(float v) { setXAt(2, v); }
	public float getY2() { return getYAt(2); }
	public void setY2(float v) { setYAt(2, v); }

}
