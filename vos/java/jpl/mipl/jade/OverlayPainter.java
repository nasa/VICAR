package jpl.mipl.jade;

import java.awt.Graphics;

/**
 * An interface that allows overlays to be painted on a display component
 * such as <code>JadeDisplay</code>.  Users register an instance of this
 * interface with the display, which then calls <code>paintOverlay()</code>
 * when a part of the overlay needs to be painted.
 * <p>
 * Depending on how an instance of this interface is registered with
 * a <code>JadeDisplay</code>, it may be called in one of two ways.
 * The first is in immediate mode, where each exposed area triggers a call
 * to <code>paintOverlay()</code>.  There may be a lot of such calls.
 * The second is in batch mode, where updates are queued and then painted
 * all at once.  A batched call will often cover the entire visible display
 * area, but that's not necessarily the case.  Both modes look the same
 * to the <code>paintOverlay()</code> function; the difference is simply
 * in the area that needs to be painted.
 * <p>
 * It is acceptable, if appropriate, to implement the
 * <code>paintOverlay()</code> function as a call to a component's
 * <code>paint()</code> function.  The signatures are the same.
 * However, you must be careful of Swing's double buffering, if it is
 * a Swing component.  The supplied <code>Graphics</code> object is what
 * must be painted into, and it is valid only during the call to this
 * method.  Double-buffered components may have issues with this.
 * <p>
 * Note that if you want to change the contents of the overlay (thus forcing
 * repaint), you must notify the display component to repaint both the old
 * area (to erase the existing graphic), and the new area (to paint the new
 * graphic).  These can be merged if appropriate (e.g. if they're adjacent).
 * This notification should usually be done by calling
 * <code>repaint(x,y,width,height)</code> one or more times on the display
 * component, but can also be done in the case of <code>JadeDisplay</code>
 * by sending the component a <code>RenderingChangeEvent</code>.  The
 * <code>paintOverlay</code> method will then be called to redraw the
 * modified areas.
 * <p>
 * If your overlay is complex it is recommended that the first thing you
 * do is get the clipping rectangle from the supplied <code>Graphics</code>
 * object (via <code>g.getClipBounds()</code>), and compare that against
 * the bounding box of the overlay graphics.  Overlay painters are called
 * very often (once per tile during a full-screen repaint) so if there's
 * nothing to do, it's often faster to check once rather than having each
 * draw method clip to nothing.
 * <p>
 * <em>Important!</em>  The <code>Graphics</code> object provided here has
 * been translated into <em>Image</em> coordinates, so you can draw using the
 * same coordinate space as the image itself.  However, <code>repaint</code>,
 * <code>paintNoErase</code>, and similar routines in <code>JadeDisplay</code>
 * work in <em>Viewport</em> coordinates, which differ by an offset of the
 * image origin (see <code>JadeDisplay.getCurrentImageOrigin()</code>).
 * However, a <code>RenderingChangeEvent</code> is in <em>Image</em>
 * coordinates, so no offset is necessary.  Be careful to apply this offset
 * in the appropriate cases!
 * <p>
 * The <code>paintOverlay</code> function will always be called in the
 * context of the AWT/Swing event thread.
 *
 * @see JadeDisplay#addOverlayPainter
 * @author Bob Deen, JPL
 */

public interface OverlayPainter
{

/***********************************************************************
 * The function should repaint the overlay for the area specified.  It is
 * called by the display component.
 * <p>
 * The area needing painting should be obtained from the supplied
 * <code>Graphics</code> object via <code>g.getClipBounds()</code>.
 * The bounds should not be <code>null</code>, but in case it is, 
 * the entire overlay should be repainted.
 * <p>
 * @param g The <code>Graphics</code> into which to paint.  If the display
 * component is a <code>JadeDisplay</code>, then this is guaranteed to be
 * a <code>Graphics2D</code> object.
 *
 * @see JadeDisplay
 * @see Graphics#getClipBounds
 */
    public void paintOverlay(Graphics g);
}

