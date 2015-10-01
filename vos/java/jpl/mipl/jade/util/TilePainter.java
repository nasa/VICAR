package jpl.mipl.jade.util;

import java.awt.*;
import java.awt.image.*;
import javax.swing.*;
import javax.media.jai.*;
import jpl.mipl.jade.*;

/**
 * A class that manages painting of a single tile in an asynchronous
 * environment.  This class keeps track of whether a tile request is
 * currently queued, as well as managing the repainting of the tile
 * once an asynchronous request is completed.
 * <p>
 * This class should only be used by <code>JadeDisplay</code>; it is
 * not intended to be called from user code.
 *
 * @author Bob Deen, JPL
 */
public class TilePainter implements TileComputationListener
{
    /** The JadeDisplay we're painting on.  We get the damage list and Graphics
     *  objects from here. */
    protected JadeDisplay _jadeDisplay;

    /** The Image being displayed.  Must be a JAI PlanarImage for scheduling. */
    protected PlanarImage _image;

    /** The cached ColorModel for the image */
    protected ColorModel _colorModel;

    /** The tile index for the tile we're managing */
    protected int _tileX, _tileY;

    /** The size of the tile */
    protected int _tileWidth, _tileHeight;

    /** The bounding box for this tile in image coordinates */
    protected Rectangle _bounds;

    /** Is the tile currently being processed? */
    protected volatile boolean _queued;
    /** The TileRequest for the queued tile */
    protected TileRequest _queue_request;
    /** Sync lock for the _queued variable */
    private Object _queued_lock = new Object();

    /** Repaint policy.  Determines whether tiles are computed and displayed
     *  immediately, or deferred. */
    protected int _repaintPolicy;

    /** Single-element array of Point's representing this tile */
    private Point[] _tileList = new Point[1];

//------ Constants ------

    /** Repaint immediately in all cases.  No queueing or background
     *  processing. */
    static public final int REPAINT_IMMEDIATE = JadeDisplay.REPAINT_IMMEDIATE;

    /** Defer tile computation to asynchronous threads in all possible cases.
     *  Repaints happen when the tile is computed. */
    static public final int REPAINT_DEFERRED = JadeDisplay.REPAINT_DEFERRED;

    /** If the tile is in the cache, same as REPAINT_IMMEDIATE.  If not,
     *  same as REPAINT_DEFERRED. */
    static public final int REPAINT_CACHE = JadeDisplay.REPAINT_CACHE;

    /** Cached Point instance for use in creating WritableRenderedImage's */
    private static final Point _zeroPoint = new Point(0, 0);

    /** Enable debug prints */
    private static final boolean DPR = false;


/***********************************************************************
 * Constructs a new TilePainter to deal with the given tile.
 */
    public TilePainter(JadeDisplay jadeDisplay,
			PlanarImage image, ColorModel colorModel,
			int tileX, int tileY, int repaintPolicy)
    {
	_jadeDisplay = jadeDisplay;
	_image = image;
	_colorModel = colorModel;
	_tileX = tileX;
	_tileY = tileY;
	_repaintPolicy = repaintPolicy;

	_tileWidth = _image.getTileWidth();
	_tileHeight = _image.getTileHeight();
	_tileList[0] = new Point(_tileX, _tileY);
	_queued = false;

	recalcBounds();
    }


/***********************************************************************
 * Accessor for tile indices
 */
    public int getTileX()
    {
	return _tileX;
    }

/***********************************************************************
 * Accessor for tile indices
 */
    public int getTileY()
    {
	return _tileY;
    }

/***********************************************************************
 * Returns the bounding box of the tile.  Note that for efficiency, a
 * pointer is returned to the internal bounding box.  THIS MUST NOT BE
 * MODIFIED!  If you want to modify the bounds, copy them to a new
 * </code>Rectangle</code> first.
 */
    public Rectangle getBounds()
    {
	return _bounds;
    }

/***********************************************************************
 * Returns the current repaint policy.
 */
    public int getRepaintPolicy()
    {
	return _repaintPolicy;
    }

/***********************************************************************
 * Changes the current repaint policy.
 */
    public void setRepaintPolicy(int repaintPolicy)
    {
	_repaintPolicy = repaintPolicy;
    }

/***********************************************************************
 * Changes the image.  The tile size must match, and the tile indices
 * of this object may not change.
 *
 * @throws IllegalArgumentException if the tile size is different.
 */
    public void setImage(PlanarImage image)
    {
	cancelTile();		// cancel any computation of the old image
	_image = image;
	if (_image.getTileWidth() != _tileWidth ||
	    _image.getTileHeight() != _tileHeight)
	    throw new IllegalArgumentException(
		"TilePainter.setImage: new image tile size doesn't match");

	recalcBounds();
    }

/***********************************************************************
 * Recalculates the bounding box.
 */
    private void recalcBounds()
    {
	int x = _tileX * _tileWidth + _image.getTileGridXOffset();
	int y = _tileY * _tileHeight + _image.getTileGridYOffset();
	_bounds = new Rectangle(x, y, _tileWidth, _tileHeight);
    }

/***********************************************************************
 * This function is called when a tile needs to be repainted.  The caller
 * should have already checked to see if this tile needs to be repainted.
 * The tile will be immediately gathered and painted, or enqueued for
 * computation on a JAI worker thread, depending on the setting of
 * <code>repaintPolicy</code> and whether or not the tile is in the cache.
 * <p>
 * This function assumes that the caller has already updated the damage
 * list based on whatever triggered this call.  However,
 * <code>TilePainter</code> is responsible for removing the tile from the
 * damage list once it is actually painted to the screen.
 * <p>
 * If <code>repaintPolicy</code> is anything other than
 * <code>REPAINT_DEFERRED</code> (i.e. there's any chance of immediate
 * painting), then this function <em>must</em> be called from the AWT/Swing
 * event thread, in order to avoid confusing the single-threaded Swing.
 * <p>
 * If a this is called from a <code>paint</code> method (or its cousins),
 * then a <code>Graphics</code> object should be passed in.  This object will
 * be used only for immediate-mode painting.  If it is null, or if painting
 * is deferred, then <code>JComponent.getGraphics()</code> will be used
 * instead to get a <code>Graphics</code> to use.  This is unfortunately
 * necessary due to Swing's double buffering scheme... if we paint from within
 * a <code>paint</code> method, we could be double buffered whether we want
 * to be or not (e.g. <code>JViewport</code> seems to always double-buffer,
 * regardless of settings in the component or the <code>RepaintManager</code>).
 * So, we have to paint in the supplied <code>Graphics</code> object.  In
 * deferred mode, we paint in an <code>invokeLater</code> method, which
 * executes in the event thread and is by definition not inside any repaint
 * method calls, so direct access to the <code>JComponent.getGraphics()</code>
 * is fine.  Yuck.
 *
 * @param cache the TileCache associated with the image.  Used only for
 * <code>repaintPolicy</code> == <code>REPAINT_CACHE</code>.  <code>null</code>
 * is acceptable and means no cache is available, so all tiles will be
 * deferred.
 * @param rendering the RenderedImage used as a key to look up the tile in
 * the cache.  Renderings may change even though the image object itself
 * doesn't.
 * @param g the Graphics object to use for immediate mode if we're in a
 * <code>paint</code> method (or one of its relatives), or <code>null</code>
 * otherwise.
 * @return true if the tile was painted; false if it was deferred
 */
    public boolean paintTile(TileCache cache, RenderedImage rendering,
						Graphics g)
    {

	int policy = _repaintPolicy;
	Raster tile = null;

	// If the tile is already queued, then we don't want to paint
	// immediately... because immediate painting only allows you to
	// paint the area damaged by this specific paint() call (due to
	// double buffering).  If the tile is queued, we probably need to
	// paint more than the immediately damaged area... and if we
	// painted now, the entire tile would be removed from the damage
	// list and we'd have a hole.  We could modify the code to only
	// remove the immediate damage area from the list, but it's easier,
	// and probably cheaper in the long run, to simply let the async
	// notification do the entire update.  This case happens when a tile
	// is complete and thus in the queue but the notification has not
	// been processed yet - in which case below we'd choose immediate
	// normally.  Note that the same effect occurs between the time we're
	// notified from JAI and the time Swing.invokeLater actually gets
	// around to painting us.

	if (_queued)
	    policy = REPAINT_DEFERRED;

	// If requested, check the cache to determine the repaint policy.

	if (policy == REPAINT_CACHE) {
	    if (cache == null)
		policy = REPAINT_DEFERRED;	// no cache, so always defer
	    else {
		tile = cache.getTile(rendering, _tileX, _tileY);
		if (tile == null)
		    policy = REPAINT_DEFERRED;
		else
		    policy = REPAINT_IMMEDIATE;
	    }
	}

	// Repaint immediately

	if (policy == REPAINT_IMMEDIATE) {

	    if (DPR) System.out.println("painting immediately "+_tileList[0]);
	    if (tile == null)
		tile = _image.getTile(_tileX, _tileY);	// Compute tile now

	    paintTileNow(tile, g);

	    return true;		// painted
	}

	// Repaint deferred

	else {

//!!!! NOTE:  This queues a single tile at a time.  According to Brian
//!!!! Burkhalter, this is significantly less efficient then queueing a
//!!!! batch at once ("quick q" thread in rgd's jai-ex mail folder from
//!!!! Feb. 6-7 '01).  If that doesn't change, we could return a flag here
//!!!! which asks the parent to do the queueing for us.

	    synchronized (_queued_lock) {
		if (!_queued) {	// Only re-queue if we're not already queued
		    if (DPR) System.out.println("queueing tile "+_tileList[0]);
		    _queued = true;
		    _queue_request = _image.queueTiles(_tileList);
		}
		else
		    if (DPR) System.out.println("ALREADY queued "+_tileList[0]);
	    }
	}
	return false;			// deferred
    }

/***********************************************************************
 * Paint the tile immediately.  We must already be in the AWT/Swing event
 * thread.  We use the supplied <code>Graphics</code> object if present,
 * or get a new graphics (indirectly via <code>JComponent.getGraphics()</code>)
 * if it is null.  The <code>Graphics</code> object should be supplied if
 * this is called from within a <code>paint</code> method in order to handle
 * Swing double buffering.
 *
 * @see #paintTile
 */
    protected void paintTileNow(Raster tile, Graphics g)
    {
	if (DPR) System.out.println("painting tile now: "+_tileList[0]);

	_queued = false;		// See comment near the top of paintTile

	// Get a Graphics object.  The clip region will be set to the
	// intersection of the visible damaged area and the bounds of this
	// tile, and it will have been translated appropriately to draw
	// this tile directly (i.e. image coords = graphics coords; screen
	// coord conversion is done by the translation).

	Graphics2D g2d = _jadeDisplay.getGraphics(_bounds, g);

	_jadeDisplay.subtractDamage(_bounds);

	if (g2d == null)		// not currently visible
	    return;

	WritableRaster wr = tile.createWritableRaster(
				tile.getSampleModel(),
				tile.getDataBuffer(), _zeroPoint);
	BufferedImage bi = new BufferedImage(_colorModel, wr,
				_colorModel.isAlphaPremultiplied(), null);

	if (DPR) System.out.println("drawing image at "+_bounds.x+","+_bounds.y
				+", w="+bi.getWidth()+", h="+bi.getHeight()
				+", clip="+g2d.getClipBounds());
	g2d.drawImage(bi, _bounds.x, _bounds.y, null);

	_jadeDisplay.repaintOverlay(g2d);

	g2d.dispose();
    }

/***********************************************************************
 * Cancels the tile, if it is currently being computed.  Note that
 * cancellation is advisory; we could get a completion notification anyway.
 */
    public void cancelTile()
    {
	synchronized (_queued_lock) {
	    if (_queued) {		// Don't bother if we're not queued
		_queued = false;
		_queue_request.cancelTiles(_tileList);
		_jadeDisplay.subtractDamage(_bounds);
		if (DPR) System.out.println("canceling tile: "+_tileList[0]);
	    }
	}
    }

/***********************************************************************
 * TileComputationListener functions.
 */
    public void tileComputationFailure(Object eventSource,
		TileRequest[] requests, PlanarImage image, int tileX, int tileY,
		Throwable situation)
    {
	// Mark the request as no longer being queued, but keep the damage
	// list the way it is.  That way, if someone ever requests this tile
	// again, the damage could eventually be repaired.
	//!!!! Is there anything else we should be doing with tile failures????
	//!!!! Like, perhaps, providing a facility to pass the Throwables on
	//!!!! to an application so the text can be displayed somehow?

	_queued = false;

	System.out.println("Tile Computation Failed!\n" + situation);
    }

/***********************************************************************
 * TileComputationListener functions.  Does the real work.
 * <p>
 * We check the <code>image</code> parameter to make sure it matches the
 * current rendering.  If not, we ignore the tile.  This could happen if
 * a tile was cancelled, but because it was already computing the cancellation
 * didn't take effect, in which case we'd be notified here of an "old"
 * tile.
 */
    public void tileComputed(Object eventSource, TileRequest[] requests,
		PlanarImage image, int tileX, int tileY, Raster tile)
    {
	PlanarImage currentImage = _image;
	if (_image instanceof RenderedOp)
	    currentImage = ((RenderedOp)_image).getCurrentRendering();

	if (image != currentImage)
	    return;

	if (DPR) System.out.println("tile computation done: tilex="+tileX
							+", tiley="+tileY);

	// You would expect us to set _queued=false here.  See large comment
	// near the top of paintTile() for an explanation.  Also, unless the
	// image or rendering changes, the tile is still good even if someone
	// adds damage before invokeLater gets around to calling us, so there's
	// no need to re-queue anything anyway, until we paint.

	TilePainterHelper tph = new TilePainterHelper(this, image, tile);

	SwingUtilities.invokeLater(tph);
    }

/***********************************************************************
 * TileComputationListener functions.  There's nothing to do if we're
 * notified of tile cancellation so we just ignore this call (cancelTile()
 * does all the work).
 */
    public void tileCancelled(Object eventSource, TileRequest[] requests,
		PlanarImage image, int tileX, int tileY)
    {
	if (DPR) System.out.println("Cancel notify: tilex="+tileX+", y="+tileY);
	return;
    }

/***********************************************************************
 * Nested class that updates the display with a new tile.  Must be run
 * in the event thread via invokeLater().  This is necessary because the
 * <code>run()</code> interface provides no way to pass in the appropriate
 * tile, and if we stored it in an instance variable, there would be some
 * small chance that another tile request might complete before invokeLater()
 * gets around to running, and wipe out the tile.
 * <p>
 * We save the image that was associated with this tile and check to make
 * sure it's still the current image before repainting.  It's possible that
 * the rendering might have changed between the time the tile was finished
 * computing and the time Swing's invokeLater() gets around to calling us...
 * in which case we don't want to display the (old) tile any more.  It should
 * have been already re-queued elsewhere.
 */
    private class TilePainterHelper implements Runnable
    {
	TilePainter _tp;
	PlanarImage _rendering;
	Raster _tile;

	TilePainterHelper(TilePainter tp, PlanarImage rendering, Raster tile)
	{
	    _tp = tp;
	    _rendering = rendering;
	    _tile = tile;
	}

	public void run() {
	    PlanarImage currentImage = _tp._image;
	    if (_tp._image instanceof RenderedOp)
		currentImage = ((RenderedOp)_image).getCurrentRendering();

	    if (_rendering == currentImage)
		_tp.paintTileNow(_tile, null);

	}
    }

}

