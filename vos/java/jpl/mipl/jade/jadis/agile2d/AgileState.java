/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/

package jpl.mipl.jade.jadis.agile2d;

import java.util.WeakHashMap;

import javax.media.opengl.GL;

import jpl.mipl.jade.jadis.agile2d.AgileState;

import sun.text.IntHashtable;

/**
 * <code>AgileState</code> locally keeps OpenGL state to avoid useless
 * state changes that are very expensive in OpenGL.
 * 
 * <p>Note: AgileGraphics2D uses AgileState to manage all state changes. To interact well with
 * AgileGraphics2D, use AgileState to manage state, or use the AgileGraphics2D.run() method to run code
 * in a safe context. e.g.
 * <pre>
 *    AgileState agileState = AgileState.get(gl);
 *    ...
 *    agileState.glEnable(GL.GL_TEXTURE_2D);
 *    agileState.bindTexture2D(0);
 *    if (agileState.setState(GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE)) {
 *          gl.glTexEnvf(GL.GL_TEXTURE_ENV, GL.GL_TEXTURE_ENV_MODE, GL.GL_MODULATE);
 *    }
 *    agileState.glDisable(GL.GL_TEXTURE_2D);
 * </pre>
 * 
 * @author Jean-Daniel Fekete
 * @version $Revision: 1.3 $
 */
public class AgileState {
    IntHashtable state;
    GL gl;
    String version;
    int savedCount;
    int doneCount;
    static int[] tmpValue = { 0 };
    static WeakHashMap gl2state = new WeakHashMap();
    static byte lastR, lastG, lastB, lastA;
    static final boolean DEBUG = false;
    
    /**
     * Get/Create a <code>AgileState</code> object for a GL context.
     * 
     * @param gl the GL context
     * @return the associated AgileState
     */
    public static AgileState get(GL gl) {
        AgileState s = (AgileState)gl2state.get(gl);
        if (s == null) {
            s = new AgileState(gl);
			gl2state.put(gl, s); // JM - Moved this up from the Constructor to make it clearer whats going on
		}
        return s;
    }

    /**
     * AgileState constructor.
     * 
     * @param gl the GL context.
     */
    protected AgileState(GL gl) {
        this.gl = gl;
        state = new IntHashtable();
        state.setDefaultValue(-1);
        initialize();
    }
    
    private void initialize() {
        version = gl.glGetString(GL.GL_VERSION);
        state.put(GL.GL_TEXTURE_1D, 0);
        state.put(GL.GL_TEXTURE_2D, 0);
        state.put(GL.GL_TEXTURE_BINDING_1D, 0);
        state.put(GL.GL_TEXTURE_BINDING_2D, 0);
        state.put(GL.GL_TEXTURE_ENV_MODE, 0);
    }
    
    private int initializeState(int attrib) {
        gl.glGetIntegerv(attrib, tmpValue, 0);
        state.put(attrib, tmpValue[0]);
        return tmpValue[0];
    }
    
    /**
     * Returns a String representing the version of the OpenGL implementation.
     * @return a String representing the version of the OpenGL implementation.
     */
    public String getVersion() {
        return version;
    }
    
    /**
     * Returns the GL context.
     * 
     * @return the GL context.
     */
    public GL getGL() {
        return gl;
    }
    
    /**
     * Returns the value currently associated with the specified GL attribute.
     * If the constant has not been queried before, the GL state is queried
     * first to initialize the local state to the right value.
     * 
     * @param attrib the GL attribute
     * @return the associated value.
     */
    public int getState(int attrib) {
        int s = state.get(attrib);
        if (s == -1) {
            s = initializeState(attrib);
        }
        return s;
    }

    /**
     * Sets the value currently associated with a specified GL attribute.
     * Returns <code>true</code> if the value is different from the one
     * in the GL state, meaning that a GL primitive should be used to set it.
     
     * * @param attrib the attribute
     * @param value the value r
     * @return <code>true</code> if the value is different from the one
     * in the GL state, meaning that a GL primitive should be used to set it.
     */    
    public boolean setState(int attrib, int value) {
        if (getState(attrib) == value) {
            savedCount++;
            return DEBUG;
        }
        doneCount++;
        state.put(attrib, value);
        return true;
    }
    
    protected void checkError() {
        if (gl.glGetError() != 0)
            System.err.println("Error");            
    }

    /**
     * Equivalent to glEnable but checks the value first and skip the
     * GL function is the value is already set to 1.
     * @param attrib the attribute to set.
     */    
    public void glEnable(int attrib) {
        if (setState(attrib, 1)) {
            gl.glEnable(attrib);
            checkError();
        }
    }

    /**
     * Equivalent to glDisable but checks the value first and skip the
     * GL function is the value is already set to 0.
     * @param attrib the attribute to set.
     */    
    public void glDisable(int attrib) {
        if (setState(attrib, 0)) {
            gl.glDisable(attrib);
            checkError();            
        }
    }

    public void bindTexture1D(int tex) {
        if (setState(GL.GL_TEXTURE_BINDING_1D, tex)) {
            gl.glBindTexture(GL.GL_TEXTURE_1D, tex);
            checkError();        
        }
    }
    
    public void bindTexture2D(int tex) {
        if (setState(GL.GL_TEXTURE_BINDING_2D, tex)) {
            gl.glBindTexture(GL.GL_TEXTURE_2D, tex);
            checkError();        
        }
    }
    
    public void glSetShadeModel(int model) {
        if (setState(GL.GL_SHADE_MODEL, model)) {
            gl.glShadeModel(model);
            checkError();
        }
    }
    
    public void glEnableClientState(int mode) {
        if (setState(mode, 1)) {
            gl.glEnableClientState(mode);
            checkError();
        }
    }
    
    public void glDisableClientState(int mode) {
        if (setState(mode, 0)) {
            gl.glDisableClientState(mode);
            checkError();
        }
    }
    
    public void glLogicOp(int op) {
        if (setState(GL.GL_LOGIC_OP, op)) {
            gl.glLogicOp(op);
            checkError();
        }
    }
    
    public void glColor4ub(byte r, byte g, byte b, byte a) {
        if (lastR != r || lastG != g || lastB != b || lastA != a) { 
            lastR = r;
            lastG = g;
            lastB = b;
            lastA = a;
            gl.glColor4ub(r, g, b, a);
            checkError();
        }
    }
    
    public void glColor4f(float r, float g, float b, float a) {
        glColor4ub((byte)(r*255), (byte)(g*255), (byte)(b*255), (byte)(a*255));
    }


	/**
	 * Pushes all GL attributes onto the GL state stack, using glPushAttrib and glPushClientAttrib.
	 */
	public void save() {
		// Save all attributes
		gl.glPushAttrib(GL.GL_ALL_ATTRIB_BITS);
		gl.glPushClientAttrib((int)GL.GL_ALL_CLIENT_ATTRIB_BITS);
	}

	/**
	 * Pops all GL attributes from the GL state stack, using glPopAttrib and glPopClientAttrib.
	 */
	public void restore() {
		// Restore attributes
		gl.glPopClientAttrib();
		gl.glPopAttrib();
	}
}
