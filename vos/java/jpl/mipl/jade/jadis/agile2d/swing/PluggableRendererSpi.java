/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d.swing;

import javax.swing.JRootPane;

import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.MouseMotionListener;


import jpl.mipl.jade.jadis.StereoJFrame;


/**
 * The Service Provider Interface (SPI) for pluggable renderers. A
 * pluggable renderer is responsible for rendering the contents of an StereoJFrame.
 * Two such renderers are implemented: One using the standard Java2D
 * classes built into the Java runtime, and one using the JOGL OpenGL pipeline
 * to perform rendering.
 * 
 * <p>
 * The {@link jpl.mipl.jade.jadis.agile2d.swing} package contains the OpenGL renderer
 * implementation.
 * </p>
 * 
 * <p></p>
 */
public abstract class PluggableRendererSpi {
    static PluggableRendererSpi activeProvider;
    static PluggableRendererSpi defaultProvider;
    static final String         OPENGL_PROVIDER = "jpl.mipl.jade.jadis.agile2d.swing.JOGLPluggableRendererProvider";
    static boolean              enabled = true;


	/**
	 * Returns true if the OpenGL pluggable renderer is enabled, 
	 * false otherwise.
	 */
	public static boolean isOpenGLEnabled() {
		return enabled;	
	}

	/**
	 * Sets whether the OpenGL pluggable renderer is enabled. This
	 * can be used by applications at runtime to override the 
	 * agile2d.provider property.
	 */
	public static void setOpenGLEnabled(boolean val) {
		enabled = val;	
		activeProvider = null;
	}	

	/**
	 * Returns true if the Agile2D OpenGL Renderer is currently available, 
	 * false if Agile2D is using the standard Java renderer.
	 */
	public static boolean isOpenGLAvailable() {
		return !(PluggableRendererSpi.getDefaultProvider() 
			instanceof PluggableRendererSpi.J2DPluggableRendererProvider);
	}


    static class J2DPluggableRenderer implements PluggableRenderer {
        public String getName() {
            return "java2d";
        }
        public Graphics getGraphics() {
            return null;
    }
        public void flush(Image image) {
        }

        public void dispose() {
        }

		public boolean isStereoHardwareAvailable() {
			return false;
		}

        public void notifyBeforeSetRootPane(JRootPane pane) {
        }

        public void notifyAfterSetRootPane(JRootPane pane) {
        }

        public boolean notifyRepaint(long tm, int x, int y, int width,
                                     int height) {
            return true;
        }

        public boolean notifyPaint(Graphics g) {
            return true;
        }

        public boolean notifyUpdate(Graphics g) {
            return true;
        }
        
        public void setCursor(Cursor c) {
        }

        public void addAWTEventListener(AWTEventListener l) {
        }

        public void removeAWTEventListener(AWTEventListener l) {
        }
		public void addMouseMotionListener(MouseMotionListener mml) {
			
		}
		public void removeMouseMotionListener(MouseMotionListener mml) {
			
		}
    }

    static class J2DPluggableRendererProvider extends PluggableRendererSpi {
        public PluggableRenderer createRenderer(StereoJFrame frame) {
            return new J2DPluggableRenderer();
        }
    }

    // Initializes the provider
    /**
     * Returns an instance of the Provider I loaded from my agile2d.provider
     * property
     *
     * @return DOCUMENT ME!
     */
    static PluggableRendererSpi getDefaultProvider() {
        if (defaultProvider != null)
            return defaultProvider;
        PluggableRendererSpi provider = null;
        String               prop;

        // Test if the property called "agile2d.provider" is set - if its set to "java2d", use 
        // the standard Java2D renderer. 
        //
        prop = System.getProperty("agile2d.provider");
        if (prop == null)
            prop = "opengl";
        prop = prop.intern();

        if (prop != "java2d") {
            try {
                String className = (prop == "opengl" ? OPENGL_PROVIDER : prop);
                Class  cls = Class.forName(className);
                provider = (PluggableRendererSpi)cls.newInstance();
            } catch (Exception e) {
                System.err.println("Unable to initialize OpenGL Service Provider (OpenGL library not present?).");
                e.printStackTrace();
            }
        }

        // OpenGL not available - use a Pure-Java toolkit
        if (provider == null) {
            provider = new J2DPluggableRendererProvider();
        }

        defaultProvider = provider;
        return provider;
    }

    /**
     * Returns the currently active provider. This method uses the System
     * property "agile2d.provider" to  determine what kind of provider to
     * use, as follows:
     * 
     * <p>
     * 
     * <ul>
     * <li>
     * If the system property "agile2d.provider" is "java2d", getProvider()
     * returns  a pluggable renderer provider which renders graphics  using
     * the standard builtin Java2D graphics renderer. Such pluggable
     * renderers can be  identified by calling the
     * PluggableRenderer.getName() method - if this returns  "java2d", the
     * standard Java2D renderer is in use.
     * 
     * <p></p>
     * </li>
     * <li>
     * If the system property "agile2d.provider" is "opengl", getProvider()
     * attempts to load the  Agile2D OpenGL Pluggable Renderer package and
     * returns an instance of the provider defined  in that package. Loading
     * the Agile2D OpenGL Pluggable Renderer package may fail  for several
     * reasons (e.g. the package is unavailable, native code needed by the
     * package is unavailable, the application does not have security
     * permission to  load native code, etc.). If the Agile2D OpenGL
     * Pluggable Renderer cannot be loaded,  getProvider() falls back to
     * using the Java2D provider.
     * 
     * <p></p>
     * </li>
     * <li>
     * If the system property "agile.provider" is set to something other than
     * "java2d"  or "opengl", it is assumed to be the name of a Java class
     * which extends PluggableRendererSpi.  getProvider() attempts to load
     * that class and return an instance of it. If the class  does not exist
     * or is an invalid type, getProvider() falls back to using the default
     * provider.
     * </li>
     * </ul>
     * </p>
     *
     */
    public static PluggableRendererSpi getActiveProvider() {
        if (activeProvider == null) {
            if (!enabled)
                activeProvider = getJava2DProvider();
            else
                activeProvider = getDefaultProvider();
        }
        return activeProvider;
    }

    /**
     * Returns an instance of the Java2D Provider
     *
     */
    public static PluggableRendererSpi getJava2DProvider() {
        return new J2DPluggableRendererProvider();
    }

    /**
     * Called by StereoJFrame to create a pluggable renderer instance for the
     * frame. Providers must implement this method to create
     * provider-specific renderers.
     *
     */
    public abstract PluggableRenderer createRenderer(StereoJFrame frame);
}
