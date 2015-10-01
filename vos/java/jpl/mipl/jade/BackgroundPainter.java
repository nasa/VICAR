package jpl.mipl.jade;

import java.awt.Graphics;

/**
 * An interface that allows backgrounds to be painted on a JadeDisplay
 * component before the actual image is drawn.  This allows customization
 * of the component's appearance while waiting for tiles to be painted.
 * For example, a Photoshop-style checkerboard could be drawn instead of
 * simply erasing to black.
 * <p>
 * See <code>JadeDisplay.backgroundPainter()</code> for an example
 * implementation.
 * @author Bob Deen, JPL
 */

public interface BackgroundPainter
{

/***********************************************************************
 * This function is called to paint the background for the area specified.
 * It is called by the <code>JadeDisplay</code> component.
 * <p>
 * This routine should be <em>fast</em>.  The idea is to not slow down the
 * actual image display, but simply put something up indicating the image
 * is not yet displayed.
 * <p>
 * The first parameters provide the <code>Graphics</code> object on which
 * to draw, and the x/y/width/height of the area to draw.  These parameters
 * are measured in image coordinates and may be passed in exactly as-is to
 * <code>Graphics.fillRect(x,y,w,h)</code> - the supplied <code>Graphics</code>
 * object is properly translated, and is clipped to the passed-in bounds.
 * Thus, you can paint outside the bounds if necessary; the excess will
 * be clipped off.
 * <p>
 * Several flags are passed in which help to determine why the area is
 * being erased.  These should have an impact on what you draw:
 * <ul>
 * <li> isNoErase:  True if this paint was due to a call to
 * <code>paintNoErase()</code>.  It requests that the background <em>not</em>
 * be erased.  Even if you do something different normally, you should
 * generally not do anything if this flag is on.
 *
 * <li> isInsideImage:  True if this paint is inside the image region; false
 * if it is outside the image.  False means that the image will <em>not</em>
 * be drawn on top (since there is no image), so you might want to paint
 * different background graphics (e.g. erase instead of checkerboard).
 * Graphics overlays may still be painted on top, however, regardless of the
 * setting of this flag.  Any given call to paintBackground() is guaranteed
 * to be either entirely inside or entirely outside the image area.
 *
 * <li> isDeferredTile:  True if the area specified is being deferred
 * (queued for later computation).  Note that <code>paintBackground()</code>
 * will be called <em>twice</em> for such tiles: once with this false (as
 * with all tiles), and again with this true, once it is determined that the
 * tile is being deferred.  Generally you will want to do nothing if
 * <code>isDeferredTile</code> is true.  But if you want to, say, paint
 * a checkerboard only if the tile is deferred, that could be done via this
 * flag.  Note, however, that other, non-deferred tiles may have already
 * been painted, so this may not be called immediately upon the tile being
 * exposed.
 * </ul>
 * <p>
 * A note regarding the passed-in <code>Graphics</code> object.  This object
 * is used only for background painting.  Thus, you can set any parameters
 * in the <code>Graphics</code> object that you want, and do not necessarily
 * have to re-set them when done.  However, while the same object <em>may</em>
 * be re-used in subsequent calls to the background painter, there is
 * <em>no</em> guarantee of this.  So while you don't have to reset to
 * initial state at the end, you <em>do</em> have to set all state parameters
 * you want before starting the paint.
 * <p>
 *
 * @param g The <code>Graphics</code> into which to paint.  This is most
 * likely a <code>Graphics2D</code> instance, but that is not guaranteed
 * by <code>JadeDisplay</code>.  It is usually whatever was passed in to
 * <code>paint(g)</code>, translated and clipped appropriately.
 * @param x The X coordinate of the rectangle to be painted
 * @param y The Y coordinate of the rectangle to be painted
 * @param width The width of the rectangle to be painted
 * @param height The height of the rectangle to be painted
 * @param isNoErase True if called from <code>paintNoErase()</code>
 * @param isInsideImage True if rectangle is inside the image bounds
 * @param isDeferredTile True if tile is deferred (this is the second call)
 *
 * @see JadeDisplay
 * @see JadeDisplay#setBackgroundPainter
 * @see Graphics#fillRect(int,int,int,int)
 */
    public void paintBackground(Graphics g, int x, int y, int width, int height,
			boolean isNoErase, boolean isInsideImage,
			boolean isDeferredTile);

}

