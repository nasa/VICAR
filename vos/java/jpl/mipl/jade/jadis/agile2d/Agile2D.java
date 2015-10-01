/*
 * Copyright (C) 1998-2002 by University of Maryland, College Park, MD 20742, USA
 * All rights reserved.
 */

package jpl.mipl.jade.jadis.agile2d;

import java.awt.*;

/**
 * A class containing static convenience methods, rendering hints and constants
 * for the Agile2D rendering library.
 */
public class Agile2D {
	
	/**
	 * Default constructor.  Does nothing.
	 *
	 */
	private Agile2D() { }

	/**
	 * Extends {@link RenderingHints.Key} 
	 */
	private static class Key extends RenderingHints.Key {
		/**
		 * Specified Class.
		 */
		Class type;
		/**
		 * Construct a key using the indicated private key
		 * and Class variable.
		 * @param v the specified key
		 * @param type the specified Class
		 */
		Key(int v, Class type) { super(v); this.type = type; }
		/**
		 * Returns true if the specified object is a valid value for this Key.
		 * Valid value constitutes specified object being an instance of the 
		 * same class as member variable type.
		 */
		public boolean isCompatibleValue(Object o) { return type.isInstance(o); }
	}

	/**
	 * Returns true if the specified Graphics object is associated with
	 * an OpenGL drawable.
	 */
	public static boolean isOpenGLGraphics(Graphics g) {
		Graphics2D g2 = (Graphics2D)g;
		return g2.getRenderingHint(KEY_USING_GL_HINT) == Boolean.TRUE;
	}
		
	private static int nextKey = 0;
	private static Key genIntKey() { return new Key(nextKey++, Integer.class); }
	private static Key genBoolKey() { return new Key(nextKey++, Boolean.class); }
	private static Key genObjectKey() { return new Key(nextKey++, Object.class); }
	private static Key genMatrixKey() { return new Key(nextKey++, double[].class); }
	

	/**
	 * Readonly hint indicating if this graphics object is associated with
	 * a GL drawable. This rendering hint will be set to Boolean.TRUE for 
	 * a Graphics2D object that is attached to a GL Drawable.
	 */
	public static final RenderingHints.Key KEY_USING_GL_HINT = genBoolKey();

	/**
	 * Readonly hint containing reference to a GL Drawable if there is one.
	 * This rendering hint will be set to a GL4Java GLDrawable object for Graphics2D 
	 * objects that are drawing using GL4Java. It will be set to null otherwise. 
	 * You can use this hint to obtain the GLFunc and GLUFunc objects attached to the drawable.
	 */
	public static final RenderingHints.Key KEY_GL_DRAWABLE_HINT = genObjectKey();

	/**
	 * <p>Hint providing the matix to use as the initial OpenGL ModelView matrix during
	 * rendering. If this is non-null, it must be a sixteen element double array in the
	 * standard OpenGL arrangement. This matrix is post-multiplied by the
	 * Graphic's AffineTransform to generate the final GL matrix used during rendering.
	 */
	public static final RenderingHints.Key KEY_GL_MODELVIEW_HINT = genMatrixKey();

	/**
	 * <p>Hint providing the matix to use as the initial OpenGL Projection matrix during
	 * rendering. If this is non-null, it must be a sixteen element double array in the
	 * standard OpenGL arrangement.  
	 */
	public static final RenderingHints.Key KEY_GL_PROJECTION_HINT = genMatrixKey();

	/**
	 * Hint indicating if polygons are convex. If this rendering hint is set to Boolean.TRUE, then polygons 
	 * generated using the draw(), fill() or fillPolygon() methods are assumed to be convex, 
	 * and may
	 * be rendered using a faster algorithm. If this is Boolean.FALSE or null (the default), 
	 * the Graphics2D renderer assumes that all polygons are concave.
	 */
	public static final RenderingHints.Key KEY_CONVEX_SHAPE_HINT = genBoolKey();

	/**
	 * <p>Hint indicating if shapes are immutable. If this rendering hint is set to Boolean.TRUE, then Shape and Polygon objects 
	 * drawn using draw() or fill() are assumed to be immutable - that is, the coordinates in the shape
	 * are assumed to never change. For immutable shapes, the renderer may cache information
	 * about the shape for reuse in future renderings. The default is Boolean.FALSE.
	 */
	public static final RenderingHints.Key KEY_IMMUTABLE_SHAPE_HINT = genBoolKey();

	/**
	 * <p>Hint indicating if images are immutable. If this rendering hint is set to Boolean.TRUE, then Images drawn using
	 * drawImage() are assumed to be immutable - that is, the contents of the image
	 * is assumed never to change. For immutable images, the renderer caches 
	 * information about the image for reuse in future renderings. </p>
	 * 
	 * <p>The default is Boolean.TRUE.</p>
	 */
        public static final RenderingHints.Key KEY_IMMUTABLE_IMAGE_HINT = genBoolKey();
    
        /**
         * <p>Hint indicating the Fonts should be created incrementally.
         * If this rendering hint is set, the font glyphs are created just when
         * needed, otherwise, the whole Font is created the first time one
         * glyph is needed.  For zooming interfaces, creating font incrementally
         * improves the performance when animating a zoom.  For other types
         * of interfaces, creating the font once is much faster. 
         * 
         * <p>The default is Boolean.FALSE.</p>
         */
        public static final RenderingHints.Key KEY_INCREMENTAL_FONT_RENDERER_HINT = genBoolKey();
        
}
