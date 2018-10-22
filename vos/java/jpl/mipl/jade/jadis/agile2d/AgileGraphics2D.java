// License Terms
// 
// Copyright (c) 2008, California Institute of Technology ("Caltech").
// U.S. Government sponsorship acknowledged.
// 
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// * Neither the name of Caltech nor its operating division, the Jet Propulsion
// Laboratory, nor the names of its contributors may be used to endorse or
// promote products derived from this software without specific prior written
// permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER  OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
// 
/*****************************************************************************
 * Copyright (C) 2006, Jon Meyer, Ben Bederson and Jean-Daniel Fekete        *
 * ------------------------------------------------------------------------- *
 * This software is published under the terms of the BSD Software License    *
 * a copy of which has been included with this distribution in the           *
 * license-agile2d.txt file.                                                 *
 *****************************************************************************/
package jpl.mipl.jade.jadis.agile2d;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.TexturePaint;
import java.awt.Toolkit;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.PathIterator;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferByte;
import java.awt.image.DataBufferInt;
import java.awt.image.ImageObserver;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.awt.image.VolatileImage;
import java.awt.image.renderable.RenderableImage;
import java.nio.ByteBuffer;
import java.nio.IntBuffer;
import java.text.AttributedCharacterIterator;
import java.util.Map;

import javax.media.opengl.GL;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.glu.GLU;
import javax.media.opengl.GLContext;

import jpl.mipl.jade.jadis.Jadis;
import jpl.mipl.jade.jadis.agile2d.geom.VertexArray;
import jpl.mipl.jade.jadis.agile2d.geom.VertexArraySupport;
import jpl.mipl.jade.jadis.agile2d.geom.VertexAttributes;

/**
 * AgileGraphics2D implements a reasonably complete subset of the standard
 * Java2D Graphics2D API, using JOGL OpenGL as the rendering engine.
 * 
 * <p>
 * Note that some API functions are marked "TBD" (to be done). Calling these
 * functions typically has no effect - most Java2D applications should run under
 * OpenGL, although they may not appear correctly rendered.
 * </p>
 * <p>
 * The original AgileGraphics2D has been enhanced to support Stereo functionality.
 * Other changes include modifying copyArea, drawImage and some other graphics calls.
 * The future improvements might include transition from glCopyPixels/glDrawPixels
 * to texture based image manipulation
 * <p>
 */
public final class AgileGraphics2D extends Graphics2D implements Cloneable,
		VertexArraySupport {
	//
	// GRAPHICS STATE
	//
	private Paint paint = Color.black;
	private Color color = Color.black;
	private Color background = Color.white;
	private Color xorColor = null;
	private Composite composite = AlphaComposite.SrcOver;
	private Stroke stroke = DEFAULT_STROKE;
	private Font font = DEFAULT_FONT;
	private RenderingHints renderingHints = new RenderingHints(null);
	private AffineTransform transform = new AffineTransform();
	private Area clipArea = null;
	private SavedClip relClipArea = null;
	private Rectangle tmpRect = new Rectangle();

	private int _stereoView = Jadis.STEREO_BOTH;
	private int _stereoMode = Jadis.STEREO_ANAGLYPH;

	private class SavedClip extends Area {
		Area absClipArea;
		AffineTransform transform;

		SavedClip(Area absClipArea, AffineTransform transform) {
			super(absClipArea);
			this.absClipArea = absClipArea;
			this.transform = transform;
			try {
				transform(transform.createInverse());
			} catch (NoninvertibleTransformException e) {
				this.transform = null;
			}
		}
	}

	//
	// GRAPHICS ENGINE
	//
	// The engine is the object that does the drawing. Multiple Graphics2D
	// objects share one engine. Each engine is associated with a GLDrawable.
	//
	private GraphicsEngine engine = null;

	//
	// DEBUGGING CONSTANTS
	//
	// If this is true, all of the graphics drawn by AgileGraphics2D are
	// rendered in
	// a rubycon red. Good for ensuring that you are really seing OpenGL
	// rendering the
	// stuff, not Java2D.
	//
	private static final boolean DEBUG_SHOW_PAINT = false;

	// If this is true, after every GL Drawing request, a jglCheckGL is called
	// to
	// generate any errors
	//
	private static final boolean DEBUG_CHECK_GL = true;

	// If this is true, clipping information is printed out to stdout
	//
	private static final boolean DEBUG_CLIP = false;

	// If this is true, messages are printed out when an unimplemented feature
	// is called.
	//
	private static final boolean DEBUG_TBD = true;

	//
	// DEFAULTS
	//
	private static final AffineTransform IDENTITY = new AffineTransform();
	private static final BasicStroke DEFAULT_STROKE = new BasicStroke();
	private static final Font DEFAULT_FONT = new Font("Times", Font.BOLD, 12);
	static RenderingHints HINTS = new RenderingHints(
			RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

	static {
		HINTS.put(RenderingHints.KEY_RENDERING,
				RenderingHints.VALUE_RENDER_QUALITY);
		HINTS.put(RenderingHints.KEY_FRACTIONALMETRICS,
				RenderingHints.VALUE_FRACTIONALMETRICS_ON);
	}

	static final int PAINT_SOLID = 0;
	static final int PAINT_GRADIENT = 1;
	static final int PAINT_TEXTURE = 2;

	// GraphicsEngine class
	//
	// In Java, many different Graphics objects can be created which refer
	// to the same underlying canvas - each with their own state.
	//
	// To support this, we define a GraphicsEngine class that contains all of
	// the code
	// for talking to GL. The AgileGraphics2D class routes all requests to an
	// engine object.
	//
	// The Engine owns all the GL specific code. The rest of the
	// AgileGraphics2D class contains only calls to the engine.
	//
	private static final class GraphicsEngine {
		AgileGraphics2D active; // Indicates which Graphics object is currently
								// active

		// GL State
		GLAutoDrawable drawable;
		GL gl;
		GLContext glj;
		GLU glu;
		private AgileState glState;
		private double[] glMatrix = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
				0, 1 };

		private Tesselator tesselator;
		private ShapeManager shapeManager;
		private ImageManager imageManager;
		private StencilManager stencilManager;
		private GradientManager gradientManager;
		private TextureFontRenderer textureFont;
		private OutlineFontRenderer outlineFont;

		private Font font;
		private Shape shapeClip;
		private TexturePaint texturePaint;
		private double scale;
		private double lineWidth = 1.0;
		private double absLineWidth = 1.0;
		private double alpha = 1.0;
		private double[] flatMatrix = new double[6];
		private Rectangle windowBounds = null;
		private int paintMode;

		private boolean frcAntialiasing;
		private boolean frcUsesFractionalMetrics;
		private boolean inited;
		private boolean usePixelAlignment;
		private boolean useShapeClip;
		private boolean isGLStencilAvailable;

		//
		// If useFastShapes is true, some shapes get drawn using more efficient
		// GL routines.
		// If it is false, every shape (including text characters) goes through
		// one of
		// three generic routines: doDrawShape, doFillShape or
		// doFillImmutableShape. This
		// is needed by the GradientGraphicsEngine, which wants to draw all
		// shapes differently
		//
		private boolean useFastShapes = true;

		// If isAffineMatrix is true, we can assume that OpenGL is using
		// the Java2D affine transform. If its false, then there may be
		// some custom non-affine matrix on the GL matrix stack (either a
		// projection or a modelview matrix).
		//
		private boolean isAffineMatrix = true;

		// Rendering hints
		private boolean convexHint;
		private boolean immutableShapeHint;
		private boolean immutableImageHint;
		private boolean incrementalFontHint;
		private boolean antiAliasingHint;
		// will change the value when we can create a MULTISAMPLE_BUFFER with
		// gl4java
		private boolean aaEnabled = false;

		private double[] modelViewMatrix;
		private double[] projectionMatrix;

		// Prototype objects used for drawing round rects, ellipses and arcs
		RoundRectangle2D roundRectProto = new RoundRectangle2D.Double();
		Ellipse2D ovalProto = new Ellipse2D.Double();
		Arc2D arcProto = new Arc2D.Double();
		Line2D lineProto = new Line2D.Double();
		Rectangle2D rectProto = new Rectangle2D.Double();
		double[] point = new double[2];
		Graphics2D g2d;
		BufferedImage buf;
		Graphics2D bg;
		double maxLineWidth;
		int maxTexSize;

		GraphicsEngine(GLAutoDrawable drawable) {
			this.drawable = drawable;
		}

		private void checkForErrors() {
			// No error checking in JOGL - use a DebugGL instead
			// drawable.getGLContext().gljCheckGL();
		}

		void doInit() {
			if (inited)
				return;
			// For images
			// if (maxTexSize > 1024) { // limit texture size to 1024
			maxTexSize = 1024;
			// }
			buf = new BufferedImage(maxTexSize, maxTexSize,
					BufferedImage.TYPE_INT_ARGB);
			bg = (Graphics2D) buf.getGraphics();

			this.gl = drawable.getContext().getGL();
			this.glj = drawable.getContext();

			this.glu = new GLU();
			this.glState = AgileState.get(gl);
			glState.glSetShadeModel(GL.GL_FLAT);
			glState.glEnableClientState(GL.GL_VERTEX_ARRAY);
			gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);
			this.isGLStencilAvailable = (glState.getState(GL.GL_STENCIL_BITS) >= 1); // Need
																						// 1
																						// bit
																						// for
																						// clip

			// maxLineWidth = glState.getState(GL.GL_LINE_WIDTH_RANGE);
			// maxTexSize = glState.getState(GL.GL_MAX_TEXTURE_SIZE);

			// We need a Java Graphics2D to fall back on in certain cases, so
			// make one
			if (drawable instanceof Component) {
				Component c = (Component) drawable;

				g2d = (Graphics2D) c.createImage(1, 1).getGraphics();
				g2d.setRenderingHints(AgileGraphics2D.HINTS);
			} else {
				g2d = bg;
			}

			Dimension sz = new Dimension(drawable.getWidth(), drawable
					.getHeight());
			windowBounds = new Rectangle(0, 0, sz.width, sz.height);

			tesselator = new Tesselator(glu);
			shapeManager = new ShapeManager(tesselator, gl);
			imageManager = new ImageManager(gl, buf, bg);
			stencilManager = new StencilManager(gl);
			textureFont = new TextureFontRenderer();
			// TextureFontRenderer.setMaxTextureSize(maxTexSize);
			outlineFont = new OutlineFontRenderer(tesselator);
			frcAntialiasing = false;
			frcUsesFractionalMetrics = false;
			gradientManager = new GradientManager(gl);

			inited = true;
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doActivate(AgileGraphics2D g) {
			active = g;
		}

		//
		// Prepares the engine for rendering - resets the transform
		//
		void doReset() {
			Dimension sz = new Dimension(drawable.getWidth(), drawable
					.getHeight());
			windowBounds = new Rectangle(0, 0, sz.width, sz.height);

			isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);

			// Setup the Projection Matrix - note that we flip Y
			loadProjectionMatrix();

			shapeManager.flush(); // Deletes any unused call lists
			imageManager.flush(); // Deletes any unused call lists
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doSetRenderingHints(RenderingHints hints) {
			convexHint = (hints.get(AgileRenderingHints.KEY_CONVEX_SHAPE_HINT) == Boolean.TRUE);
			immutableShapeHint = (hints
					.get(AgileRenderingHints.KEY_IMMUTABLE_SHAPE_HINT) == Boolean.TRUE);
			immutableImageHint = (hints
					.get(AgileRenderingHints.KEY_IMMUTABLE_IMAGE_HINT) != Boolean.FALSE);
			incrementalFontHint = (hints
					.get(AgileRenderingHints.KEY_INCREMENTAL_FONT_RENDERER_HINT) != Boolean.FALSE);
			setProjectionMatrix((double[]) hints
					.get(AgileRenderingHints.KEY_GL_PROJECTION_HINT));
			setModelViewMatrix((double[]) hints
					.get(AgileRenderingHints.KEY_GL_MODELVIEW_HINT));
			frcAntialiasing = hints.get(RenderingHints.KEY_TEXT_ANTIALIASING) == RenderingHints.VALUE_TEXT_ANTIALIAS_ON;
			frcUsesFractionalMetrics = hints
					.get(RenderingHints.KEY_FRACTIONALMETRICS) == RenderingHints.VALUE_FRACTIONALMETRICS_ON;
			boolean oldAA = antiAliasingHint;
			antiAliasingHint = (hints.get(RenderingHints.KEY_ANTIALIASING) == RenderingHints.VALUE_ANTIALIAS_ON);
			g2d.setRenderingHints(hints);
			if (oldAA != antiAliasingHint) {
				if (antiAliasingHint) {
					doEnableAntialiasing();
				} else {
					doDisableAntialiasing();
				}
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doEnableAntialiasing() {
			if (!antiAliasingHint)
				return;
			glState.glEnable(GL.GL_MULTISAMPLE);
		}

		void doDisableAntialiasing() {
			glState.glDisable(GL.GL_MULTISAMPLE);
		}

		// PAINT AND COMPOSITE
		void doSetColor(Color color) {
			if (color == null)
				doSetColor(0);
			else {
				doSetColor(color.getRGB());
			}
		}

		void doSetColor(int argb) {
			useFastShapes = true;
			paintMode = PAINT_SOLID;

			byte a = (byte) ((argb & 0xff000000) >> 24);
			byte r = (byte) ((argb & 0xff0000) >> 16);
			byte g = (byte) ((argb & 0x00ff00) >> 8);
			byte b = (byte) (argb & 0xff);

			if (DEBUG_SHOW_PAINT) {
				double lum = .30 * r + .59 * g + .11 * b;
				r = (byte) lum;
				g = 0;
				b = 0;
			}
			if (alpha != 1.0f) {
				if (a < 0) {
					a = (byte) (alpha * (int) (256 + a));
				} else {
					a = (byte) (alpha * (int) a);
				}

			}

			glState.glColor4ub(r, g, b, a);

			if (a != -1) {
				glState.glEnable(GL.GL_BLEND);
			} else {
				glState.glDisable(GL.GL_BLEND);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doSetGradient(GradientPaint gradient) {
			useFastShapes = false;
			paintMode = PAINT_GRADIENT;
			gradientManager.setGradient(gradient);
			gradientManager.setAlpha((float) alpha);
		}

		void doSetTexturePaint(TexturePaint tp) {
			useFastShapes = false;
			gl.glColor4f(1.0f, 1.0f, 1.0f, (float) alpha);
			paintMode = PAINT_TEXTURE;
			texturePaint = tp;
		}

		void doSetComposite(Composite composite) {
			if (composite instanceof AlphaComposite) {
				// (currently ignoring Blend Mode)
				AlphaComposite alphaComposite = (AlphaComposite) composite;
				alpha = alphaComposite.getAlpha();
				active.setPaint(active.paint);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// CLIPPING
		// Sets the Clip rectangle in device coordinates. Disables
		// arbitrary shape clipping.
		//
		void doSetClip(Rectangle rect) {
			if (useShapeClip) {
				stencilManager.disableClipping(StencilManager.STENCIL_1);
				this.useShapeClip = false;
			}

			if (rect == null || RectUtils.containsOrEquals(rect, windowBounds)) {
				// Disable clipping planes and scissor test
				glState.glDisable(GL.GL_SCISSOR_TEST);
				glState.glDisable(GL.GL_CLIP_PLANE0);
				glState.glDisable(GL.GL_CLIP_PLANE1);
				glState.glDisable(GL.GL_CLIP_PLANE2);
				glState.glDisable(GL.GL_CLIP_PLANE3);
			} else {
				if (DEBUG_CLIP) {
					System.out.println("CLIP: " + rect.x + " "
							+ (windowBounds.height - (rect.y + rect.height))
							+ " " + rect.width + " " + rect.height);
				}

				if (isAffineMatrix) {
					// We know that the Java2D AffineTransform is the
					// only applicable transform.
					//
					// Use the Scissor test, which only works in 2D
					//
					gl.glScissor(rect.x, windowBounds.height
							- (rect.y + rect.height), rect.width, rect.height);
					glState.glEnable(GL.GL_SCISSOR_TEST);

					// Disable the clip planes just to be sure
					glState.glDisable(GL.GL_CLIP_PLANE0);
					glState.glDisable(GL.GL_CLIP_PLANE1);
					glState.glDisable(GL.GL_CLIP_PLANE2);
					glState.glDisable(GL.GL_CLIP_PLANE3);
				} else {
					//
					// May be a custom transform in use.
					// Use OpenGL Clip Planes, which work in 3D.
					//
					gl.glPushMatrix();
					loadIdentityMatrix();

					double[] plane = new double[4];

					plane[0] = 1;
					plane[1] = 0;
					plane[3] = -rect.x;
					gl.glClipPlane(GL.GL_CLIP_PLANE0, plane, 0);

					plane[0] = -1;
					plane[1] = 0;
					plane[3] = rect.x + rect.width;
					gl.glClipPlane(GL.GL_CLIP_PLANE1, plane, 0);

					plane[0] = 0;
					plane[1] = 1;
					plane[3] = -rect.y;
					gl.glClipPlane(GL.GL_CLIP_PLANE2, plane, 0);

					plane[0] = 0;
					plane[1] = -1;
					plane[3] = rect.y + rect.height;
					gl.glClipPlane(GL.GL_CLIP_PLANE3, plane, 0);

					glState.glEnable(GL.GL_CLIP_PLANE0);
					glState.glEnable(GL.GL_CLIP_PLANE1);
					glState.glEnable(GL.GL_CLIP_PLANE2);
					glState.glEnable(GL.GL_CLIP_PLANE3);

					glState.glDisable(GL.GL_SCISSOR_TEST);

					gl.glPopMatrix();
				}
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// Sets a clip shape - this is specified in user coordinates
		//
		void doSetClipShape(Shape shape) {
			//
			// Need to render the shape into the Stencil buffer
			//
			stencilManager.begin(StencilManager.STENCIL_1, null);

			// Fill the shape - note that clips are specified
			// in world coordinates, so we need to not use an
			// identity transform...
			gl.glPushMatrix();
			gl.glLoadIdentity();
			shapeManager.fill(shape, null, (float) scale, false, false);
			gl.glPopMatrix();
			stencilManager.end();

			stencilManager.enableClipping(StencilManager.STENCIL_1);
			useShapeClip = true;
		}

		// FONT AND STROKE
		void doSetFont(Font font) {
			this.font = font;
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doSetStroke(Stroke stroke) {
			if (stroke instanceof BasicStroke) {
				BasicStroke basic = (BasicStroke) stroke;
				lineWidth = basic.getLineWidth();
				absLineWidth = lineWidth * scale;

				if (absLineWidth < maxLineWidth) {
					float t = (float) absLineWidth;
					if (t < 1)
						t = 1;
					gl.glLineWidth(t);
				}
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// PAINT MODE
		void doSetPaintXOR(boolean xor) {
			if (xor) {
				glState.glLogicOp(GL.GL_XOR);
			} else {
				glState.glLogicOp(GL.GL_COPY);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// TRANSFORMS
		private void loadProjectionMatrix() {
			gl.glMatrixMode(GL.GL_PROJECTION);

			if (projectionMatrix == null) {
				gl.glLoadIdentity();
				glu.gluOrtho2D(0, windowBounds.width, windowBounds.height, 0);
			} else {
				gl.glLoadMatrixd(projectionMatrix, 0);
			}

			gl.glMatrixMode(GL.GL_MODELVIEW);
		}

		private void loadIdentityMatrix() {
			if (modelViewMatrix == null) {
				gl.glLoadIdentity();
				// Red book, 3rd Edition, p. 677,
				// explains rasterization rules wrt
				// integer coordinates.
				// Filled shapes have integer pixels
				// coordinates whereas stroked
				// shapes coordinates are considered
				// from the center of the pixels.
				// Shifting the model coordinate system
				// fixes the problem, at a cost for
				// non accelerated graphics cards...
				gl.glTranslatef(0.375f, 0.375f, 0.0f);
			} else {
				gl.glLoadMatrixd(modelViewMatrix, 0);
			}
		}

		private void setProjectionMatrix(double[] val) {
			if (val == null && projectionMatrix == null) {
				// do nothing
			} else if (val == null && projectionMatrix != null) {
				projectionMatrix = null;
				loadProjectionMatrix();
				isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);
			} else if (val != null && projectionMatrix == null) {
				projectionMatrix = new double[16];
				System.arraycopy(val, 0, projectionMatrix, 0, val.length);
				loadProjectionMatrix();
				isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);
			} else if (val != null && projectionMatrix != null) {
				for (int i = 0; i < val.length; i++) {
					if (projectionMatrix[i] != val[i]) {
						System.arraycopy(val, 0, projectionMatrix, 0,
								val.length);
						loadProjectionMatrix();
						isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);

						break;
					}
				}
			}
		}

		private void setModelViewMatrix(double[] val) {
			if (val == null && modelViewMatrix == null) {
				// do nothing
			} else if (val == null && modelViewMatrix != null) {
				modelViewMatrix = null;
				shapeManager.setModelViewMatrix(null);
				doSetTransform(active.transform);
				isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);
			} else if (val != null && modelViewMatrix == null) {
				modelViewMatrix = new double[16];
				System.arraycopy(val, 0, modelViewMatrix, 0, val.length);
				shapeManager.setModelViewMatrix(modelViewMatrix);
				doSetTransform(active.transform);
				isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);
			} else if (val != null && modelViewMatrix != null) {
				for (int i = 0; i < val.length; i++) {
					if (modelViewMatrix[i] != val[i]) {
						System
								.arraycopy(val, 0, modelViewMatrix, 0,
										val.length);
						shapeManager.setModelViewMatrix(modelViewMatrix);
						doSetTransform(active.transform);
						isAffineMatrix = (modelViewMatrix == null && projectionMatrix == null);

						break;
					}
				}
			}
		}

		//
		// Called whenever the transform has changed - we need determine the
		// new line width in absolute coordinates. Hmm. expensive...
		//
		private void transformChanged() {
			AffineTransform transform = active.transform;

			usePixelAlignment = (isAffineMatrix ? true : false);

			if (transform.isIdentity()) {
				scale = 1;
			} else if (transform.getShearX() == 0 && transform.getShearY() == 0) {
				scale = Math.abs(Math.max(transform.getScaleX(), transform
						.getScaleY()));
			} else if (transform.getScaleX() == 0 && transform.getScaleY() == 0) {
				scale = Math.abs(Math.max(transform.getShearX(), transform
						.getShearY()));
			} else {
				// Turn off pixel alignment for text if there is a
				// general transform
				usePixelAlignment = false;

				// Transform a vector, then compute its length
				point[0] = 1;
				point[1] = 0;
				transform.deltaTransform(point, 0, point, 0, 1);
				scale = Math
						.sqrt((point[0] * point[0]) + (point[1] * point[1]));
			}

			absLineWidth = lineWidth * scale;
			if (absLineWidth <= 0)
				gl.glLineWidth(1);
			else if (absLineWidth < maxLineWidth) {
				gl.glLineWidth((float) absLineWidth);
			}
			shapeManager.setTolerance(1.0 / scale);
			active.relClipArea = null;
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		public void doScale(double sx, double sy) {
			gl.glScaled(sx, sy, 1);
			transformChanged();
		}

		public void doTranslate(double dx, double dy) {
			gl.glTranslated(dx, dy, 0);
			transformChanged();
		}

		public void doRotate(double theta) {
			gl.glRotated(Math.toDegrees(theta), 0, 0, 1);
			transformChanged();
		}

		void doSetTransform(AffineTransform transform) {
			loadIdentityMatrix();
			doTransform(transform);
		}

		void doTransform(AffineTransform aTransform) {
			switch (aTransform.getType()) {
			case AffineTransform.TYPE_TRANSLATION:
				gl.glTranslated(aTransform.getTranslateX(), aTransform
						.getTranslateY(), 0);
				break;
			case AffineTransform.TYPE_IDENTITY:
				break;
			case AffineTransform.TYPE_UNIFORM_SCALE:
				gl.glScaled(aTransform.getScaleX(), aTransform.getScaleX(), 1);
				break;
			case AffineTransform.TYPE_TRANSLATION
					| AffineTransform.TYPE_UNIFORM_SCALE:
				gl.glTranslated(aTransform.getTranslateX(), aTransform
						.getTranslateY(), 0);
				gl.glScaled(aTransform.getScaleX(), aTransform.getScaleX(), 1);
				break;
			default:
				aTransform.getMatrix(flatMatrix);

				// flatMatrix is m00 m10 m01 m11 m02 m12
				glMatrix[0] = flatMatrix[0];
				glMatrix[1] = flatMatrix[1];
				glMatrix[2] = 0;
				glMatrix[3] = 0;

				glMatrix[4] = flatMatrix[2];
				glMatrix[5] = flatMatrix[3];
				glMatrix[6] = 0;
				glMatrix[7] = 0;

				glMatrix[8] = 0;
				glMatrix[9] = 0;
				glMatrix[10] = 1;
				glMatrix[11] = 0;

				glMatrix[12] = flatMatrix[4];
				glMatrix[13] = flatMatrix[5];
				glMatrix[14] = 0;
				glMatrix[15] = 1;
				gl.glMultMatrixd(glMatrix, 0);
			}
			transformChanged();
		}

		//
		// DRAWING AND FILLING PRIMITIVES
		//
		// If useFastShapes is false, all of the drawing routines fallback
		// to one of two routines: drawShape or fillShape.
		//

		// STROKING
		void doDrawLine(float x1, float y1, float x2, float y2) {
			// !!!! ozp if (useFastShapes && absLineWidth < maxLineWidth) {
			gl.glBegin(GL.GL_LINES);
			gl.glVertex2f(x1, y1);
			gl.glVertex2f(x2, y2);
			gl.glEnd();
			// !!!! ozp
			/*
			 * } else { lineProto.setLine(x1, y1, x2, y2); drawShape(lineProto,
			 * null, false, true); }
			 */
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doDrawLines(int[] xPts, int[] yPts, int nPts, int mode) {
			if (useFastShapes && absLineWidth < maxLineWidth) {
				shapeManager.begin(mode);
				for (int i = 0; i < nPts; i++)
					shapeManager.addVertex(xPts[i], yPts[i]);
				shapeManager.end();
			} else if (nPts > 1) {
				// Convert to path
				GeneralPath path = new GeneralPath();
				path.moveTo(xPts[0], yPts[0]);

				for (int i = 1; i < nPts; i++) {
					path.lineTo(xPts[i], yPts[i]);
				}

				drawShape(path, null, false, false);
			}

			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doDrawRect(int x1, int y1, int width, int height) {
			int x2 = x1 + width;
			int y2 = y1;
			int x3 = x1 + width;
			int y3 = y1 + height;
			int x4 = x1;
			int y4 = y1 + height;

			// !!!! ozp if (useFastShapes) {
			// if (absLineWidth < 3) {
			// For 4 vertices, glBegin/glEnd is faster than vertex arrays
			gl.glBegin(GL.GL_LINE_LOOP);
			gl.glVertex2i(x1, y1);
			gl.glVertex2i(x2, y2);
			gl.glVertex2i(x3, y3);
			gl.glVertex2i(x4, y4);
			gl.glEnd();
			/*
			 * !!!! ozp } else { float lw = (float)(lineWidth / 2.0); if (lw >=
			 * width || lw >= height) { gl.glRectf(x1-lw, y1-lw, x3+lw, y3+lw); }
			 * else { shapeManager.begin(GL.GL_QUADS);
			 * shapeManager.addQuad(x1-lw, y1-lw, x2+lw, y2-lw, x2-lw, y2+lw,
			 * x1+lw, y1+lw); shapeManager.addQuad(x4-lw, y4+lw, x1-lw, y1-lw,
			 * x1+lw, y1+lw, x4+lw, y4-lw); shapeManager.addQuad(x4-lw, y4+lw,
			 * x3+lw, y3+lw, x3-lw, y3-lw, x4+lw, y4-lw);
			 * shapeManager.addQuad(x2+lw, y2-lw, x3+lw, y3+lw, x3-lw, y3-lw,
			 * x2-lw, y2+lw); shapeManager.end(); } } } else {
			 * rectProto.setRect(x1, y1, width, height); drawShape(rectProto,
			 * null, false, true); }
			 */
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doDrawShape(Shape shape) {
			if (useFastShapes && absLineWidth < 3) {
				// Fastest route - flatten the shape in object space and
				// draw the vertices using GL Lines.
				//
				PathIterator path = shape.getPathIterator(null, shapeManager
						.getTolerance()
						/ scale);
				shapeManager.send(path, false);
			} else {
				drawShape(shape, null, immutableShapeHint, convexHint);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doDrawVertexArray(VertexArray array, VertexAttributes attributes) {
			drawShape(array, attributes, immutableShapeHint, false);
		}

		// All drawn shapes go through this primative method (unless faster
		// methods for drawing exist)
		//
		private void drawShape(Shape shape, VertexAttributes attributes,
				boolean immutable, boolean convex) {
			if (paintMode != PAINT_SOLID) {
				// Push strokes drawn with a gradient through the fill
				// routine...
				fillShape(active.stroke.createStrokedShape(shape), attributes,
						false, convex);
			} else {
				shapeManager.draw(shape, attributes, (float) scale,
						active.stroke, immutable, convex);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// FILLING
		void doFillRect(double x1, double y1, double width, double height) {
			if (width <= 0 || height <= 0)
				return;
			if (useFastShapes) {
				gl.glRectd(x1, y1, x1 + width, y1 + height);
			} else {
				rectProto.setRect(x1, y1, width, height);
				fillShape(rectProto, null, false, true);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doFillPolygon(int[] xPts, int[] yPts, int nPts) {
			if (useFastShapes) {
				shapeManager.fill(xPts, yPts, nPts, convexHint);
			} else {
				Polygon p = new Polygon(xPts, yPts, nPts);
				fillShape(p, null, false, false);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doFillShape(Shape shape) {
			fillShape(shape, null, immutableShapeHint, convexHint);
		}

		void doFillShape(Shape shape, boolean convex) {
			fillShape(shape, null, immutableShapeHint, convex);
		}

		void doFillVertexArray(VertexArray array, VertexAttributes attributes) {
			fillShape(array, attributes, immutableShapeHint, false);
		}

		// All fill methods come here
		private void fillShape(Shape shape, VertexAttributes attributes,
				boolean immutable, boolean convex) {
			switch (paintMode) {
			case PAINT_SOLID:
				// Just fill the shape
				shapeManager.fill(shape, attributes, (float) scale, immutable,
						convex);
				break;
			case PAINT_GRADIENT:
				// Installs a 1D texture with a suitable texture generator
				// and fills the shape.
				gradientManager.begin((float) alpha);
				shapeManager.fill(shape, attributes, (float) scale, immutable,
						convex);
				gradientManager.end();
				break;
			case PAINT_TEXTURE:
				// Installs a 2D texture with S and T texture gen
				// and fills the shape.
				Texture texture = imageManager.findTexture(texturePaint
						.getImage(), null, immutableImageHint, true);

				if (texture != null) {
					texture.begin(texturePaint.getAnchorRect());
					shapeManager.fill(shape, attributes, (float) scale,
							immutable, convex);
					texture.end();
				}
				break;
			}

			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// TEXT
		void doDrawString(String string, float x, float y) {
			if (font == null)
				return;
			gl.glPushMatrix();
			gl.glTranslated(x, y, 0);

			if (useFastShapes
					&& textureFont.install(drawable, font, scale,
							frcAntialiasing, frcUsesFractionalMetrics)) {
				// Fits in font cache - draw using texture memory
				textureFont.setIncremental(incrementalFontHint);
				drawTextureString(string);
			} else {
				// Too big to fit in a texture - draw from outlines instead
				drawOutlineString(string);
			}

			gl.glPopMatrix();
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		private void drawOutlineString(String string) {
			if (outlineFont.install(drawable, font, scale, frcAntialiasing,
					frcUsesFractionalMetrics)) {
				outlineFont.render(drawable, string, scale, font);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		private void drawTextureString(String string) {
			doDisableAntialiasing();
			textureFont.render(drawable, string, scale, font);
			doEnableAntialiasing();

			active.setPaint(active.paint);
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		void doDrawGlyphVector(GlyphVector g, float x, float y) {
			Font font = g.getFont();
			FontRenderContext frc = g.getFontRenderContext();

			gl.glPushMatrix();
			gl.glTranslatef(x, y, 0);

			if (useFastShapes
					&& textureFont.install(drawable, font, scale, frc
							.isAntiAliased(), frc.usesFractionalMetrics())) {
				// Fits in font cache - draw using texture memory
				textureFont.setIncremental(incrementalFontHint);
				drawTextureGlyphVector(g);
			} else {
				// Too big to fit in a texture - draw from outlines instead
				drawOutlineGlyphVector(g);
			}

			gl.glPopMatrix();
			if (DEBUG_CHECK_GL)
				checkForErrors();

		}

		private void drawTextureGlyphVector(GlyphVector g) {
			doDisableAntialiasing();
			textureFont.render(drawable, g, scale);
			doEnableAntialiasing();

			active.setPaint(active.paint);
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		private void drawOutlineGlyphVector(GlyphVector g) {
			Font font = g.getFont();
			FontRenderContext frc = g.getFontRenderContext();
			if (outlineFont.install(drawable, font, scale, frc.isAntiAliased(),
					frc.usesFractionalMetrics())) {
				outlineFont.render(drawable, g, scale);
			}
			if (DEBUG_CHECK_GL)
				checkForErrors();
		}

		// IMAGES

		void doDrawTexture(Texture tex, double x1, double y1, double x2,
				double y2) {
			if (alpha == 0)
				return;
			glState.glColor4f(1, 1, 1, (float) alpha);
			tex.paint(x1, y1, x2, y2);
			active.setPaint(active.paint);
		}

		boolean doDrawImage(BufferedImage image, int dx1, int dy1, int dx2,
				int dy2, int sx1, int sy1, int sx2, int sy2) {

			if (image.getType() == BufferedImage.TYPE_INT_ARGB) {

				glState.glEnable(GL.GL_BLEND);
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, image.getWidth());
				int[] data = ((DataBufferInt) (image.getRaster()
						.getDataBuffer())).getData();
				IntBuffer buffer = IntBuffer.wrap(data);

				gl.glPixelZoom(1, -1);
				gl.glPushMatrix();
				gl.glLoadIdentity();

				gl.glRasterPos2i(0, 0);
				gl.glBitmap(0, 0, 0, 0, (float) (active.transform
						.getTranslateX() + dx1), -(float) (active.transform
						.getTranslateY() + dy1), null);

				gl.glDrawPixels(dx2 - dx1, dy2 - dy1, GL.GL_BGRA,
						GL.GL_UNSIGNED_BYTE, buffer);
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, 0);
				gl.glPopMatrix();
				return true;
			}

			else if (image.getType() == BufferedImage.TYPE_INT_RGB) {
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, image.getWidth());

				// Read the data from image
				IntBuffer buffer = null;
				DataBuffer dataBuffer = image.getRaster().getDataBuffer();
				if (dataBuffer instanceof DataBufferInt) {
					int[] data = ((DataBufferInt) (image.getRaster()
							.getDataBuffer())).getData();
					buffer = IntBuffer.wrap(data);
				} else {
					// deal with DataBufferNative
					SampleModel model = image.getSampleModel();
					BufferedImage image2 = null;
					if (model.getNumBands() > 1)
						image2 = new BufferedImage(image.getWidth(), image
								.getHeight(), BufferedImage.TYPE_INT_ARGB);
					else
						image2 = new BufferedImage(image.getWidth(), image
								.getHeight(), BufferedImage.TYPE_BYTE_GRAY);
					image2.getGraphics().drawImage(image, 0, 0,
							image.getWidth(), image.getHeight(), null);
					dataBuffer = image2.getRaster().getDataBuffer();
					if (dataBuffer instanceof DataBufferInt) {
						int[] data = ((DataBufferInt) (image2.getRaster()
								.getDataBuffer())).getData();
						buffer = IntBuffer.wrap(data);
					}
				}

				// gl.glRasterPos2i(dx1, dy1);
				gl.glPixelZoom(1, -1);

				gl.glPushMatrix();
				gl.glLoadIdentity();

				gl.glRasterPos2i(0, 0);
				gl.glBitmap(0, 0, 0, 0, (float) (active.transform
						.getTranslateX() + dx1), -(float) (active.transform
						.getTranslateY() + dy1), null);

				gl.glDrawPixels(dx2 - dx1, dy2 - dy1, GL.GL_BGRA,
						GL.GL_UNSIGNED_BYTE, buffer);
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, 0);
				gl.glPopMatrix();

				return true;
			} else if (image.getType() == BufferedImage.TYPE_BYTE_GRAY) {
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, image.getWidth());

				// Read the data from image
				byte[] data = ((DataBufferByte) (image.getRaster()
						.getDataBuffer())).getData();
				ByteBuffer buffer = ByteBuffer.wrap(data);

				gl.glPixelZoom(1, -1);

				gl.glPushMatrix();
				gl.glLoadIdentity();

				gl.glRasterPos2i(0, 0);
				gl.glBitmap(0, 0, 0, 0, (float) (active.transform
						.getTranslateX() + dx1), -(float) (active.transform
						.getTranslateY() + dy1), null);

				gl.glDrawPixels(dx2 - dx1, dy2 - dy1, GL.GL_LUMINANCE,
						GL.GL_UNSIGNED_BYTE, buffer);
				gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH, 0);

				gl.glPopMatrix();
				return true;
			} else { // need to convert first to one of the formats that
						// OpenGL can handle
				SampleModel model = image.getSampleModel();
				DataBuffer databuffer = image.getRaster().getDataBuffer();
				BufferedImage image2 = null;
				if (model.getNumBands() > 1)
					image2 = new BufferedImage(image.getWidth(), image
							.getHeight(), BufferedImage.TYPE_INT_ARGB);
				else
					image2 = new BufferedImage(image.getWidth(), image
							.getHeight(), BufferedImage.TYPE_BYTE_GRAY);

				image2.getGraphics().drawImage(image, 0, 0, image.getWidth(),
						image.getHeight(), null);

				return doDrawImage(image2, dx1, dy1, dx2, dy2, sx1, sy1, sx2,
						sy2);
			}
		}

		void doCopyArea(Rectangle src, Rectangle dst, int stereoMode,
				int stereoView) {
			int wasBlending = glState.getState(GL.GL_BLEND);
			glState.glDisable(GL.GL_BLEND);

			// gl.glReadBuffer(glState.getState(GL_DOUBLEBUFFER)!=0 ? GL_BACK :
			// GL_FRONT);
			/*
			 * if (glState.getState(GL.GL_DOUBLEBUFFER) == 1)
			 * System.out.println("Using double buffering"); if
			 * (glState.getState(GL.GL_DOUBLEBUFFER) == 0)
			 * System.out.println("NOT using double buffering");
			 */

			gl.glPushMatrix();
			gl.glLoadIdentity();
			gl.glPixelZoom(1, 1);

			// make both src.y and dst.y point to the lower left corners of the
			// regions
			// since glCopyPixels() copies a region whose lower left corner is
			// (x,y),
			// but the source parameters (srcx, srcy) point to the upper left
			// corner.
			src.x = src.x;
			src.y = windowBounds.height - (src.y + src.height);
			dst.y += dst.height;

			// gl.glRasterPos2i(dst.x, dst.height);

			// This is a rather intriguing (yet totally valid) hack... If we
			// were to
			// specify a raster position that is outside the surface bounds, the
			// raster
			// position would be invalid and nothing would be rendered. However,
			// we
			// can use a widely known trick to move the raster position outside
			// the
			// surface bounds while maintaining its status as valid. The
			// following
			// call to glBitmap() renders a no-op bitmap, but offsets the
			// current
			// raster position from (0,0) to the desired location of
			// (dstx,-dsty)...

			gl.glRasterPos2i(0, 0);
			gl.glBitmap(0, 0, 0, 0, (float) dst.x, (float) -dst.y, null);

			if (stereoMode == Jadis.STEREO_GL) {
				if (stereoView == Jadis.STEREO_LEFT) {
					gl.glReadBuffer(GL.GL_FRONT_LEFT);
					gl.glDrawBuffer(GL.GL_FRONT_LEFT);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
				}
				else if (stereoView == Jadis.STEREO_RIGHT) {
					gl.glReadBuffer(GL.GL_FRONT_RIGHT);
					gl.glDrawBuffer(GL.GL_FRONT_RIGHT);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
				}
				else { // Jadis.STEREO_BOTH
					gl.glReadBuffer(GL.GL_FRONT_LEFT);
					gl.glDrawBuffer(GL.GL_FRONT_LEFT);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
					gl.glReadBuffer(GL.GL_FRONT_RIGHT);
					gl.glDrawBuffer(GL.GL_FRONT_RIGHT);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
				}
			} else {
				gl.glReadBuffer(GL.GL_FRONT);
				if (stereoView == Jadis.STEREO_LEFT) {
					gl.glColorMask(true, false, false, true);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
					gl.glColorMask(true, true, true, true);
				} else if (stereoView == Jadis.STEREO_RIGHT) {
					gl.glColorMask(false, true, true, true);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
					gl.glColorMask(true, true, true, true);
				} else { // Jadis.STEREO_BOTH
					gl.glColorMask(true, true, true, true);
					gl.glCopyPixels(src.x, src.y, src.width, src.height,
							GL.GL_COLOR);
				}
			}

			gl.glPopMatrix();

			if (wasBlending != 0)
				glState.glEnable(GL.GL_BLEND);
			if (DEBUG_CHECK_GL)
				checkForErrors();

			gl.glFlush();
		}

		// EXTENSION

		void doRunGL(GLEventListener ev) {
			try {
				glState.save();
				ev.init(drawable);
				ev.reshape(drawable, 0, 0, windowBounds.width,
						windowBounds.height);
				ev.display(drawable);
			} finally {
				glState.restore();
			}
		}
	}

	// SETUP
	//
	// Needed by the clone method
	//
	AgileGraphics2D() {
	}

	/**
	 * Constructs a AgileGraphics2D that renders to a particular GL Drawable.
	 * Users of AgileGraphics2D should call resetAll in their display methods to
	 * make the OpenGL state reflect the default Java2D state.
	 * 
	 * <pre>
	 *     aglGraphics.resetAll(); // Makes the OpenGL State reflect Java2D's defaults
	 *     aglGraphics...
	 * </pre>
	 * 
	 * @param drawable
	 *            the underlying GLDrawable
	 */
	public AgileGraphics2D(GLAutoDrawable drawable) {
		engine = new GraphicsEngine(drawable);
	}

	GLAutoDrawable getDrawable() {
		return engine.drawable;
	}

	/**
	 * Resets the Graphics to its default state.
	 */
	public void resetAll() {
		engine.doInit();
		setBackground(Color.white);
		setColor(Color.black);
		setStroke(DEFAULT_STROKE);
		setTransform(IDENTITY);
		setFont(DEFAULT_FONT);
		setComposite(AlphaComposite.SrcOver);
		setPaintMode();
		setClip(null);
		clearRenderingHints();
		engine.doSetRenderingHints(renderingHints);
		engine.doReset();
	}

	/**
	 * @see java.awt.Graphics#dispose()
	 */
	public void dispose() {
		// Not clear whether its safe to dispose of the drawable so we
		// just do nothing here.
	}

	//
	// Used by the create() method
	//
	/*
	 * @see java.lang.Object#clone()
	 */
	protected Object clone() {
		AgileGraphics2D c = null;

		try {
			c = (AgileGraphics2D) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException("Object.clone failed.");
		}

		c.transform = new AffineTransform(transform);
		c.renderingHints = getRenderingHints();

		if (clipArea != null) {
			c.clipArea = (Area) (clipArea.clone());
		}
		c.relClipArea = null;
		c.setStereoMode(_stereoMode);
		c.setStereoView(_stereoView);
		c.setClip(this.getClipBounds());
		return c;
	}

	private void setStereo() {

		switch (_stereoMode) {
		case Jadis.STEREO_ANAGLYPH:
			engine.gl.glDrawBuffer(GL.GL_FRONT);
			if (_stereoView == Jadis.STEREO_LEFT)
				engine.gl.glColorMask(true, false, false, true);
			else if (_stereoView == Jadis.STEREO_RIGHT)
				engine.gl.glColorMask(false, true, true, true);
			else {
				engine.gl.glColorMask(true, true, true, true);
			}
			break;
		case Jadis.STEREO_GL:
			if (_stereoView == Jadis.STEREO_LEFT)
				engine.gl.glDrawBuffer(GL.GL_LEFT);

			else if (_stereoView == Jadis.STEREO_RIGHT)
				engine.gl.glDrawBuffer(GL.GL_RIGHT);

			else

				engine.gl.glDrawBuffer(GL.GL_FRONT);
			break;
		default: // mono
			// should check for double buffer
			engine.gl.glDrawBuffer(GL.GL_FRONT);
			engine.gl.glColorMask(true, true, true, true);
		}

	}

	private void makeCurrent() {
		if (engine.active != this) {
			engine.doActivate(this);
		}

		engine.doSetTransform(transform);
		engine.doSetColor(color);
		engine.doSetComposite(composite);
		engine.doSetFont(font);
		engine.doSetStroke(stroke);
		if (xorColor != null) {
			engine.doSetPaintXOR(true);
			engine.doSetColor(xorColor);
		} else {
			engine.doSetPaintXOR(false);
		}
		engine.doSetRenderingHints(renderingHints);
		this.updateClip();
		setStereo();
	}

	// STATE MANAGEMENT

	/**
	 * @see java.awt.Graphics2D#getBackground()
	 */
	public Color getBackground() {
		return background;
	}

	/**
	 * @see java.awt.Graphics2D#setBackground(java.awt.Color)
	 */
	public void setBackground(Color color) {
		this.background = color;
	}

	/**
	 * @see java.awt.Graphics#getColor()
	 */
	public Color getColor() {
		return color;
	}

	/**
	 * @see java.awt.Graphics#setColor
	 */
	public void setColor(Color color) {
		this.color = color;
		this.paint = color;
	}

	/**
	 * JadeGraphicsGL extension for specifing a color with an integer.
	 * 
	 * @param argb
	 *            color with 8 bits components for the alpha, red, green and
	 *            blue channels in that order from high bits to low bits.
	 */
	public void setColor(int argb) {
		makeCurrent();
		this.color = null;
		this.paint = null;
		engine.doSetColor(argb);
	}

	/**
	 * @see java.awt.Graphics2D#getPaint()
	 */
	public Paint getPaint() {
		return paint;
	}

	/**
	 * @see java.awt.Graphics2D#setPaint(java.awt.Paint)
	 */
	public void setPaint(Paint paint) {
		if (paint instanceof Color) {
			// easy case
			setColor((Color) paint);
		} else {
			System.out.println("paint is not instance of Color");
			setColor(Color.white); // throw new
									// UnsupportedOperationException("Not Yet
									// Implemented");
		}
	}

	/**
	 * @see java.awt.Graphics2D#getComposite()
	 */
	public Composite getComposite() {
		return composite;
	}

	/**
	 * @see java.awt.Graphics2D#setComposite(java.awt.Composite)
	 */
	public void setComposite(Composite composite) {
		this.composite = composite;
	}

	/**
	 * @see java.awt.Graphics#setFont(java.awt.Font)
	 */
	public void setFont(Font font) {
		if (font == null)
			return;
		this.font = font;
	}

	/**
	 * @see java.awt.Graphics#getFont()
	 */
	public Font getFont() {
		return font;
	}

	/**
	 * @see java.awt.Graphics#getFontMetrics(java.awt.Font)
	 */
	public FontMetrics getFontMetrics(Font f) {
		if (engine.g2d == null)
			return Toolkit.getDefaultToolkit().getFontMetrics(f);
		else
			return engine.g2d.getFontMetrics(f);
	}

	/**
	 * @see java.awt.Graphics2D#getStroke()
	 */
	public Stroke getStroke() {
		return stroke;
	}

	/**
	 * @see java.awt.Graphics2D#setStroke(java.awt.Stroke)
	 */
	public void setStroke(Stroke stroke) {
		this.stroke = stroke;
	}

	/**
	 * @see java.awt.Graphics#setPaintMode()
	 */
	public void setPaintMode() {
		this.xorColor = null;
		setPaint(paint);
	}

	/**
	 * @see java.awt.Graphics#setXORMode(java.awt.Color)
	 */
	public void setXORMode(Color xorColor) {
		this.xorColor = xorColor;
	}

	// TRANSFORMS
	// Note that we need to maintain our own transform because there may be
	// multiple Graphics objects pointing to the same GL engine, each with their
	// own
	// transform. For this reason, we perform all transform to our copy of the
	// transform
	// and then load that into the GL engine.
	//
	/**
	 * @see java.awt.Graphics2D#setTransform(java.awt.geom.AffineTransform)
	 */
	public void setTransform(AffineTransform tm) {
		transform.setTransform(tm);
	}

	/**
	 * @see java.awt.Graphics2D#getTransform()
	 */
	public AffineTransform getTransform() {
		return new AffineTransform(transform);
	}

	/**
	 * @see java.awt.Graphics2D#scale(double, double)
	 */
	public void scale(double sx, double sy) {
		transform.scale(sx, sy);
	}

	/**
	 * @see java.awt.Graphics2D#shear(double, double)
	 */
	public void shear(double sx, double sy) {
		transform.shear(sx, sy);
	}

	/**
	 * @see java.awt.Graphics2D#rotate(double, double, double)
	 */
	public void rotate(double theta, double x, double y) {
		transform.rotate(theta, x, y);
	}

	/**
	 * @see java.awt.Graphics2D#rotate(double)
	 */
	public void rotate(double theta) {
		transform.rotate(theta);
	}

	/**
	 * @see java.awt.Graphics2D#translate(double, double)
	 */
	public void translate(double x, double y) {
		transform.translate(x, y);
	}

	/**
	 * @see java.awt.Graphics#translate(int, int)
	 */
	public void translate(int x, int y) {
		transform.translate(x, y);
	}

	/**
	 * @see java.awt.Graphics2D#transform(java.awt.geom.AffineTransform)
	 */
	public void transform(AffineTransform transform) {
		transform.concatenate(transform);
	}

	// RENDERING HINTS
	private void clearRenderingHints() {
		renderingHints.clear();
		renderingHints.put(Agile2D.KEY_USING_GL_HINT, Boolean.TRUE);
		renderingHints.put(Agile2D.KEY_IMMUTABLE_IMAGE_HINT, Boolean.TRUE);
		renderingHints.put(Agile2D.KEY_INCREMENTAL_FONT_RENDERER_HINT,
				Boolean.FALSE);
		renderingHints.put(Agile2D.KEY_GL_DRAWABLE_HINT, engine.drawable);
	}

	/**
	 * @see java.awt.Graphics2D#addRenderingHints(java.util.Map)
	 */
	public void addRenderingHints(Map hints) {
		renderingHints.putAll(hints);
	}

	/**
	 * @see java.awt.Graphics2D#getRenderingHint(java.awt.RenderingHints.Key)
	 */
	public Object getRenderingHint(RenderingHints.Key key) {
		return renderingHints.get(key);
	}

	/**
	 * @see java.awt.Graphics2D#setRenderingHint(java.awt.RenderingHints.Key,
	 *      java.lang.Object)
	 */
	public void setRenderingHint(RenderingHints.Key key, Object value) {
		renderingHints.put(key, value);
	}

	/**
	 * @see java.awt.Graphics2D#setRenderingHints(java.util.Map)
	 */
	public void setRenderingHints(Map hints) {
		clearRenderingHints();
		renderingHints.putAll(hints);
	}

	/**
	 * @see java.awt.Graphics2D#getRenderingHints()
	 */
	public RenderingHints getRenderingHints() {
		// !!!! ozp 1.5 error return new RenderingHints(renderingHints);
		return (RenderingHints) renderingHints.clone();
	}

	// CLIPPING REGION
	private void updateClip() {
		if (clipArea == null) {
			engine.doSetClip(null);
		} else if (clipArea.isRectangular()) {
			engine.doSetClip(clipArea.getBounds());
		} else {
			engine.doSetClipShape(clipArea);
		}
		relClipArea = null;
	}

	/**
	 * @see java.awt.Graphics#setClip(java.awt.Shape)
	 */
	public void setClip(Shape clip) {
		if (DEBUG_CLIP) {
			System.out.println("setClip(" + clip + ")");
		}

		if (clip == null) {
			clipArea = null;
			return;
		} else if (clip instanceof SavedClip) {
			SavedClip saved = (SavedClip) clip;
			if (saved.transform.equals(transform)) {
				clipArea = saved.absClipArea;
				relClipArea = saved;
				return;
			}
		}
		clipArea = new Area(clip);
		clipArea.transform(transform);
	}

	/**
	 * @see java.awt.Graphics#setClip(int,int,int,int)
	 */
	public void setClip(int x, int y, int width, int height) {
		tmpRect.setBounds(x, y, width, height);
		setClip(tmpRect);
	}

	/**
	 * @see java.awt.Graphics2D#clip(java.awt.Shape)
	 */
	public void clip(Shape clip) {
		if (DEBUG_CLIP) {
			System.out.println("clip(" + clip + ")");
		}

		Area a = new Area(clip);
		a.transform(transform);
		if (clipArea != null) {
			a.intersect(clipArea);
		}

		clipArea = a;
	}

	/**
	 * @see java.awt.Graphics#clipRect(int, int, int, int)
	 */
	public void clipRect(int x, int y, int w, int h) {
		if (DEBUG_CLIP) {
			System.out.println("clipRect(" + tmpRect + ")");
		}

		tmpRect.setBounds(x, y, w, h);
		clip(tmpRect);
	}

	/**
	 * @see java.awt.Graphics#getClip()
	 */
	public Shape getClip() {
		if (clipArea == null)
			return null;
		if (relClipArea == null) {
			relClipArea = new SavedClip(clipArea, transform);
		}
		return relClipArea;
	}

	/**
	 * @see java.awt.Graphics#getClipBounds()
	 */
	public Rectangle getClipBounds() {
		return getClipBounds(new Rectangle());
	}

	/**
	 * @see java.awt.Graphics#getClipBounds(java.awt.Rectangle)
	 */
	public Rectangle getClipBounds(Rectangle r) {

		if (relClipArea != null) {
			r.setBounds(relClipArea.getBounds());
			return r;
		}
		Area a = clipArea;

		if (a == null) {
			a = new Area(engine.windowBounds);
		}

		try {
			// r.setBounds(engine.windowBounds);
			AffineTransform tm = transform.createInverse();
			r.setBounds(a.createTransformedArea(tm).getBounds());
		} catch (NoninvertibleTransformException ex) {
			r.setBounds(engine.windowBounds);
		}

		return r;

	}

	/**
	 * @see java.awt.Graphics#hitClip(int, int, int, int)
	 */
	public boolean hitClip(int x, int y, int width, int height) {
		return new Rectangle(x, y, width, height).intersects(getClipBounds());
	}

	// STROKING PRIMITIVES

	/**
	 * @see java.awt.Graphics#drawLine(int, int, int, int)
	 */
	public void drawLine(int x1, int y1, int x2, int y2) {

		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawLine");
			System.exit(0);
			return;
		}

		makeCurrent();
		engine.doDrawLine(x1, y1, x2, y2);
		engine.gl.glFlush();

		glj.release();

	}

	/**
	 * @see java.awt.Graphics#drawPolygon(int[], int[], int)
	 * @see java.awt.Graphics#drawPolyline(int[], int[], int)
	 */
	public void drawPolyline(int[] xPts, int[] yPts, int nPts) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawPolyLine");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doDrawLines(xPts, yPts, nPts, GL.GL_LINE_STRIP);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#drawPolygon(int[], int[], int)
	 */
	public void drawPolygon(int[] xPts, int[] yPts, int nPts) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawPolygon");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doDrawLines(xPts, yPts, nPts, GL.GL_LINE_LOOP);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#drawRect(int, int, int, int)
	 */
	public void drawRect(int x1, int y1, int width, int height) {
		if ((width < 0) || (height < 0)) {
			return;
		}
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawRect");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doDrawRect(x1, y1, width, height);
		engine.gl.glFlush();
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#drawRoundRect(int, int, int, int, int, int)
	 */
	public void drawRoundRect(int x, int y, int width, int height,
			int arcWidth, int arcHeight) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawRoundRect");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.roundRectProto.setRoundRect(x, y, width, height, arcWidth,
				arcHeight);
		draw(engine.roundRectProto);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#drawOval(int, int, int, int)
	 */
	public void drawOval(int x, int y, int width, int height) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawOval");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.ovalProto.setFrame(x, y, width, height);
		draw(engine.ovalProto);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#drawArc(int, int, int, int, int, int)
	 */
	public void drawArc(int x, int y, int width, int height, int startAngle,
			int arcAngle) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in drawArc");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.arcProto.setArc(x, y, width, height, startAngle, arcAngle,
				Arc2D.OPEN);
		draw(engine.arcProto);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics2D#draw(java.awt.Shape)
	 */
	public void draw(Shape shape) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in draw");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doDrawShape(shape);
		glj.release();
	}

	// FILLING PRIMITIVES

	/**
	 * @see java.awt.Graphics#fillRect(int, int, int, int)
	 */
	public void fillRect(int x1, int y1, int width, int height) {
		if ((width < 0) || (height < 0)) {
			return;
		}
		GLContext glj = engine.drawable.getContext();
		if (glj.makeCurrent() == glj.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fillRect");
			System.exit(0);
			return;
		}
		makeCurrent();
		Rectangle clipRect = this.getClipBounds();
		Rectangle rect = new Rectangle(x1, y1, width, height);
		Rectangle clippedRect = rect.intersection(clipRect);
		engine.doFillRect(clippedRect.x, clippedRect.y, clippedRect.width,
				clippedRect.height);
		engine.gl.glFlush();
		glj.release();

	}

	/**
	 * @see java.awt.Graphics#clearRect(int, int, int, int)
	 */
	public void clearRect(int x, int y, int width, int height) {
		GLContext glj = engine.drawable.getContext();
		if (glj.getCurrent() == null) {
			if (glj.makeCurrent() == glj.CONTEXT_NOT_CURRENT) {
				System.out.println("Problem in clearRect");
				System.exit(0);
				return;
			}
		}

		Paint p = paint;

		setColor(background);
		makeCurrent();
		fillRect(x, y, width, height);
		setPaint(p);

		glj.release();
	}

	/**
	 * @see java.awt.Graphics#fillRoundRect(int, int, int, int, int, int)
	 */
	public void fillRoundRect(int x, int y, int width, int height,
			int arcWidth, int arcHeight) {
		engine.roundRectProto.setRoundRect(x, y, width, height, arcWidth,
				arcHeight);
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fillRoundRect");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doFillShape(engine.roundRectProto, true);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#fillOval(int, int, int, int)
	 */
	public void fillOval(int x, int y, int width, int height) {
		engine.ovalProto.setFrame(x, y, width, height);
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fillOval");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doFillShape(engine.ovalProto, true);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#fillArc(int, int, int, int, int, int)
	 */
	public void fillArc(int x, int y, int width, int height, int startAngle,
			int arcAngle) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fillArc");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.arcProto.setArc(x, y, width, height, startAngle, arcAngle,
				Arc2D.PIE);
		fill(engine.arcProto);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#fillPolygon(int[], int[], int)
	 */
	public void fillPolygon(int[] xPts, int[] yPts, int nPts) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fillPolygon");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doFillPolygon(xPts, yPts, nPts);
		glj.release();
	}

	/**
	 * @see java.awt.Graphics2D#fill(java.awt.Shape)
	 */
	public void fill(Shape shape) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fill");
			System.exit(0);
			return;
		}
		makeCurrent();
		if (shape instanceof Rectangle2D) {
			Rectangle2D rect = (Rectangle2D) shape;
			engine.doFillRect(rect.getX(), rect.getY(), rect.getWidth(), rect
					.getHeight());
		} else
			engine.doFillShape(shape);
		
		glj.release();
	}

	// TEXT
	/**
	 * @see java.awt.Graphics#drawString(java.lang.String, int, int)
	 */
	public void drawString(String s, int x, int y) {
		drawString(s, (float) x, (float) y);
	}

	/**
	 * @see java.awt.Graphics2D#drawString(java.lang.String, float, float)
	 */
	public void drawString(String string, float x, float y) {
		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in fill");
			System.exit(0);
			return;
		}
		makeCurrent();
		engine.doDrawString(string, x, y);
		engine.gl.glFlush();
		glj.release();
	}

	/**
	 * @see java.awt.Graphics#create()
	 */
	public Graphics create() {
		return (Graphics) clone();
	}

	/**
	 * @see java.awt.Graphics#copyArea(int, int, int, int, int, int)
	 */
	public void copyArea(int x, int y, int width, int height, int dx, int dy) {
		Area a1 = new Area(new Rectangle(x, y, width, height));
		Area a2 = new Area(new Rectangle(x + dx, y + dy, width, height));

		a1.transform(transform);
		a2.transform(transform);

		Rectangle src = a1.getBounds();
		Rectangle dst = a2.getBounds();

		GLContext glj = engine.drawable.getContext();

		if (glj.makeCurrent() == glj.CONTEXT_NOT_CURRENT) {
			System.out.println("Problem in doCopyArea");
			System.exit(0);
			return;
		}

		makeCurrent();
		engine.doCopyArea(src, dst, _stereoMode, _stereoView);
		glj.release();

	}

	/**
	 * Sets the current stereo view which can be
	 * left, right or both
	 */
	public void setStereoView(int stereo_view) {
		_stereoView = stereo_view;
	}

	/**
	 * Returns current stereo view which can be
	 * left, right or both
	 */
	public int getStereoView() {
		return _stereoView;
	}

	/**
	 * Sets the stereo mode.  Valid values are 
	 * ANAGLYPH and STEREO_GL
	 * @param stereo_mode
	 */
	public void setStereoMode(int stereo_mode) {
		_stereoMode = stereo_mode;
		return;
	}

	/**
	 * Returns current stereo mode
	 * @return current stereo mode
	 */
	public int getStereoMode() {
		return _stereoMode;
	}

	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int,
	 *      java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int x, int y, ImageObserver observer) {
		int imgWidth = img.getWidth(observer);
		int imgHeight = img.getHeight(observer);

		boolean okey = drawImage(img, x, y, x + imgWidth, y + imgHeight, 0, 0,
				imgWidth, imgHeight, null, observer);

		return okey;
	}

	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int, int, int,
	 *      java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int x, int y, int width, int height,
			ImageObserver observer) {
		int imgWidth = img.getWidth(observer);
		int imgHeight = img.getHeight(observer);

		return drawImage(img, x, y, x + width, y + height, 0, 0, imgWidth,
				imgHeight, null, observer);
	}

	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int,
	 *      java.awt.Color, java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int x, int y, Color bgcolor,
			ImageObserver observer) {
		int imgWidth = img.getWidth(observer);
		int imgHeight = img.getHeight(observer);

		return drawImage(img, x, y, x + imgWidth, y + imgHeight, 0, 0,
				imgWidth, imgHeight, bgcolor, observer);
	}

	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int, int, int,
	 *      java.awt.Color, java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int x, int y, int width, int height,
			Color bgcolor, ImageObserver observer) {
		int imgWidth = img.getWidth(observer);
		int imgHeight = img.getHeight(observer);

		return drawImage(img, x, y, x + width, y + height, 0, 0, imgWidth,
				imgHeight, bgcolor, observer);
	}

	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int, int, int, int,
	 *      int, int, int, java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
			int sx1, int sy1, int sx2, int sy2, ImageObserver observer) {

		return drawImage(img, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2, null,
				observer);
	}

	/**
	 * @see java.awt.Graphics2D#drawImage(java.awt.Image,
	 *      java.awt.geom.AffineTransform, java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, AffineTransform xform, ImageObserver obs) {
		AffineTransform tm = getTransform();
		transform(xform);

		boolean result = drawImage(img, 0, 0, obs);
		setTransform(tm);

		return result;
	}

	private boolean isTransformTranslation() {
		if (engine.scale != 1)
			return false;

		switch (transform.getType()) {
		case AffineTransform.TYPE_IDENTITY:
		case AffineTransform.TYPE_TRANSLATION:
			return true;
		default:
			return false;
		}
	}

	// ALL IMAGE METHODS GO VIA THIS ROUTINE
	/**
	 * @see java.awt.Graphics#drawImage(java.awt.Image, int, int, int, int, int,
	 *      int, int, int, java.awt.Color, java.awt.image.ImageObserver)
	 */
	public boolean drawImage(Image img, int dx1, int dy1, int dx2, int dy2,
			int sx1, int sy1, int sx2, int sy2, Color bgcolor,
			ImageObserver observer) {
		/*
		 * // Ensure that sx/sy are positive (dx/dy can be negative) if (sx2 <
		 * sx1) { int t = sx2; sx2 = sx1; sx1 = t; t = dx2; dx2 = dx1; dx1 = t; }
		 * 
		 * 
		 * if (sy2 < sy1) { int t = sy2; sy2 = sy1; sy1 = t; t = dy2; dy2 = dy1;
		 * dy1 = t; }
		 */
		int imgWidth = img.getWidth(null);
		int imgHeight = img.getHeight(null);

		int width = sx2 - sx1;
		int height = sy2 - sy1;
		int dw = dx2 - dx1;
		int dh = dy2 - dy1;

		if (engine.glj.getCurrent() == null) {
			if (engine.glj.makeCurrent() == GLContext.CONTEXT_NOT_CURRENT) {
				System.out.println("Problem in drawImage");
				System.exit(0);
				return false;
			}
		}

		makeCurrent();

		/*
		 * if ((img instanceof BufferedImage) &&
		 * engine.doDrawImage((BufferedImage)img, dx1, dy1, dx2, dy2, sx1, sy1,
		 * sx2, sy2)) return true;
		 */
		if (img instanceof BufferedImage) {
			BufferedImage buf = new BufferedImage(((BufferedImage) img)
					.getWidth(), ((BufferedImage) img).getHeight(),
					BufferedImage.TYPE_INT_ARGB);
			Graphics g = buf.getGraphics();

			g.drawImage(img, 0, 0, dx2 - dx1, dy2 - dy1, sx1, sy1, sx2, sy2,
					bgcolor, null);
			engine.doDrawImage(buf, dx1, dy1, dx2, dy2, 0, 0, dx2 - dx1, dy2
					- dy1);
			g.dispose();
		}

		if ((img instanceof VolatileImage)) {

			BufferedImage buf = new BufferedImage(((VolatileImage) img)
					.getWidth(), ((VolatileImage) img).getHeight(),
					BufferedImage.TYPE_INT_ARGB);
			Graphics g = buf.getGraphics();

			// g.drawImage(img, 0, 0, ((VolatileImage)img).getWidth(),
			// ((VolatileImage)img).getHeight(), null);
			g.drawImage(img, 0, 0, dx2 - dx1, dy2 - dy1, sx1, sy1, sx2, sy2,
					bgcolor, null);

			// engine.doDrawImage(buf, dx1, dy1, dx2, dy2, sx1, sy1, sx2, sy2);
			engine.doDrawImage(buf, dx1, dy1, dx2, dy2, 0, 0, dx2 - dx1, dy2
					- dy1);

			g.dispose();

		}
		engine.gl.glFlush();
		engine.glj.release();
		return true;

	}

	/**
	 * @see java.awt.Graphics2D#drawGlyphVector(java.awt.font.GlyphVector,
	 *      float, float)
	 */
	public void drawGlyphVector(GlyphVector g, float x, float y) {
		makeCurrent();
		engine.doDrawGlyphVector(g, x, y);
	}

	/**
	 * Falls back on Java2D: Uses a Java2D graphics to do hit testing
	 * 
	 * @see java.awt.Graphics2D#hit(Rectangle, Shape, boolean)
	 */
	public boolean hit(Rectangle rect, Shape s, boolean onStroke) {
		Graphics2D g2 = engine.g2d;
		g2.setTransform(transform);
		g2.setStroke(stroke);
		g2.setClip(clipArea);

		boolean result = g2.hit(rect, s, onStroke);
		g2.setTransform(IDENTITY);
		g2.setClip(null);

		return result;
	}

	/**
	 * Falls back on Java2D: Returns Java2D's current device configuration
	 * 
	 * @see java.awt.Graphics2D#getDeviceConfiguration()
	 */
	public GraphicsConfiguration getDeviceConfiguration() {
		return engine.g2d.getDeviceConfiguration();
	}

	/**
	 * Returns new FontRenderContext(null, true, true).
	 * 
	 * @return new FontRenderContext(null, true, true).
	 */
	public FontRenderContext getFontRenderContext() {
		return new FontRenderContext(null, engine.frcAntialiasing,
				engine.frcUsesFractionalMetrics);
	}

	// TBD: Graphics FUNCTIONS
	/**
	 * TBD
	 * 
	 * @param iterator
	 *            DOCUMENT ME!
	 * @param x
	 *            DOCUMENT ME!
	 * @param y
	 *            DOCUMENT ME!
	 */
	public void drawString(AttributedCharacterIterator iterator, int x, int y) {
		if (DEBUG_TBD) {
			System.out
					.println("IGNORE: drawString(AttributedCharacterIterator,int,int)");
		}

		// TBD
	}

	// TBD: Graphics2D FUNCTIONS
	/**
	 * TBD
	 * 
	 * @param img
	 *            DOCUMENT ME!
	 * @param op
	 *            DOCUMENT ME!
	 * @param x
	 *            DOCUMENT ME!
	 * @param y
	 *            DOCUMENT ME!
	 */
	public void drawImage(BufferedImage img, BufferedImageOp op, int x, int y) {
		if (DEBUG_TBD) {
			System.out
					.println("IGNORE: drawImage(BufferedImage,BufferedImageOp,int,int)");
		}

		// TBD
	}

	/**
	 * TBD
	 * 
	 * @param img
	 *            DOCUMENT ME!
	 * @param xform
	 *            DOCUMENT ME!
	 */
	public void drawRenderedImage(RenderedImage img, AffineTransform xform) {
		if (DEBUG_TBD) {
			System.out
					.println("IGNORE: drawRenderedImage(RenderedImage,AffineTransform)");
		}
	}

	/**
	 * TBD
	 * 
	 * @param img
	 *            DOCUMENT ME!
	 * @param xform
	 *            DOCUMENT ME!
	 */
	public void drawRenderableImage(RenderableImage img, AffineTransform xform) {
		if (DEBUG_TBD) {
			System.out
					.println("IGNORE: drawRenderableImage(RenderableImage,AffineTransform)");
		}
	}

	/**
	 * Runs a GL program in the AgileGraphics2D context.
	 * 
	 * The GL state is left as it was when the <code>GLEventListener</code> it
	 * called: transform, clip, states. All the graphic attributes are saved,
	 * but the GL state can still be broken if the stacks are popped for
	 * example.
	 * 
	 * @param glevent
	 *            the GLEventListener which will be called.
	 */
	public void runGL(GLEventListener glevent) {
		makeCurrent();
		engine.doRunGL(glevent);
	}

	/**
	 * TBD
	 * 
	 * @param iterator
	 *            DOCUMENT ME!
	 * @param x
	 *            DOCUMENT ME!
	 * @param y
	 *            DOCUMENT ME!
	 */
	public void drawString(AttributedCharacterIterator iterator, float x,
			float y) {
		if (DEBUG_TBD) {
			System.out
					.println("IGNORE: drawString(AttributedCharacterIterator,float,float)");
		}
	}

	/**
	 * Draws the stroke represented by a VertexArray, using the specified
	 * per-vertex colors specified in VertexAttributes.
	 */
	public void drawVertexArray(VertexArray array, VertexAttributes attributes) {
		makeCurrent();
		engine.doDrawVertexArray(array, attributes);
	}

	/**
	 * Fills the geometry specified in a VertexArray, using the specified
	 * per-vertex color attributes. If attributes is null, the geometry is
	 * filled with the current paint.
	 */
	public void fillVertexArray(VertexArray array, VertexAttributes attributes) {
		makeCurrent();
		engine.doFillVertexArray(array, attributes);
	}

}
