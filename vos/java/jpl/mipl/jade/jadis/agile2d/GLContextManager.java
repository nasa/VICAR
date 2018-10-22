package jpl.mipl.jade.jadis.agile2d;

import javax.media.opengl.*;

public class GLContextManager {
	private GLContext glj = null;
	public GLContextManager(GLAutoDrawable drawable) {
		glj = drawable.getContext();
	}
	public GLContext getCurrentContext() {
		return glj;
	}
}
