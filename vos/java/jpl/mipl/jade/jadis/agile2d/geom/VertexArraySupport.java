/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.geom;

import jpl.mipl.jade.jadis.agile2d.geom.VertexArray;
import jpl.mipl.jade.jadis.agile2d.geom.VertexAttributes;

//
// Interface for rendering VertexArrays. Implemented by AgileGraphics2D.
//
public interface VertexArraySupport 
{
	public void drawVertexArray(VertexArray array, VertexAttributes attributes);
	public void fillVertexArray(VertexArray array, VertexAttributes attributes);
}