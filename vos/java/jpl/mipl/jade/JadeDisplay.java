package jpl.mipl.jade;

import java.awt.*;
import java.awt.image.*;
import java.awt.event.*;
import java.util.*;
import java.beans.*;
import javax.swing.*;
import javax.media.jai.*;
import jpl.mipl.jade.util.*;
import jpl.mipl.util.ComponentColorModelGray;

/**
 * The primary image display component for JADE.  This class supports
 * asynchronous computation of tiles for display, allowing the user to
 * scroll and otherwise manipulate the image while it is being computed
 * and painted.
 * <p>
 * JAI 1.1 or later is required to support the asynchronous features.  If fed
 * an image that is not a JAI image, it will be wrapped (via
 * <code>PlanarImage.wrapRenderedImage()</code>) to make it one.
 * <p>
 * This is a Swing lightweight component.  It is intended to display the
 * entire image; for scrolling, put it in a <code>JScrollPane</code>.
 * <p>
 * <b>Repaint Policy</b>
 * <p>
 * The caller can control when the tiles are painted via the
 * <code>repaintPolicy</code> property.  The choices are:
 * <ul>
 * <li><code>REPAINT_IMMEDIATE</code> All tiles are computed and painted
 * immediately, with no background processing.  This is the way most
 * simple displays work, including the sample widgets provided by JAI.
 * It might be necessary to use this mode if one were for example rendering
 * into an offscreen buffer, or rendering for printing.
 * <li><code>REPAINT_DEFERRED</code> All tiles are deferred, meaning they
 * are computed in the background (by JAI worker threads) and displayed
 * when they are complete.  This includes cached tiles.
 * <li><code>REPAINT_CACHE</code> A combination of the other two.  If the
 * tile can be found in the JAI tile cache, it is painted immediately.
 * If not, its computation is queued and the painting is deferred.  This is
 * the most efficient option in most cases, and is the default.
 * </ul>
 * <p>
 * <b>Image Origin</b>
 * <p>
 * The <code>imageOrigin</code> property defines the image coordinate that is
 * shown in the upper left corner of the component.  Any part of the image
 * above and to the left of that coordinate will not be displayed.  Note that
 * the logical location of the image is taken into account, so if the image
 * starts at (-100,-100) (see <code>RenderedImage.getMinX/Y()</code>), then
 * the <code>imageOrigin</code> would also need to be set to (-100,-100) in
 * order to display the whole image.  This <code>imageOrigin</code> is a
 * hard cropping and should not be confused with a Pan value, which is
 * managed entirely by the <code>JScrollPane</code> (or other higher-level
 * component).  If <code>imageOrigin</code> is set to <code>null</code>, then
 * the origin will automatically be set to the image's <code>minX</code> and
 * <code>minY</code> values, and will be reset whenever those change.
 * <p>
 * <b>Double Buffering</b>
 * <p>
 * By default, Swing double-buffers its components.  This reduces flicker
 * for most components.  However, the entire point of this image display
 * is to display things deferred!  This creates flicker, intentionally (you
 * see the background before the image gets painted).  This completely
 * defeats the purpose of double-buffering.  Since double-buffering actually
 * creates extra (unnecessary, here) work, by default <code>JadeDisplay</code>
 * turns it off.
 * <p>
 * With one exception, the component will work fine either way, with double
 * buffering on or off.  However, if you use <code>paintNoErase()</code> with
 * tiles painted using <code>REPAINT_DEFERRED</code> (or
 * <code>REPAINT_CACHE</code> when the tiles aren't in the cache), double
 * buffering will create problems with garbage flashing on the screen.  The
 * final image will look fine after all repaints are done, but the purpose
 * of <code>paintNoErase</code> is to not erase the area being drawn, which
 * interacts with double buffering in a bad way.
 * <p>
 * Although Swing has a <code>setDoubleBuffered</code> call, it is less
 * useful than you might think.  In order to disable double buffering, you
 * have to turn it off on every component in the hierarchy up to the root.
 * This can have the side-effect of turning off double buffering for other
 * components in the same window (which may benefit from it being on).
 * It turns out that <code>JPanel</code> turns double-buffering on by
 * default, so most subtrees beyond the display component will be
 * double-buffered.  However, it may be necessary to manually enable
 * double buffering for certain components.  Only the top-level component
 * of a subtree needs to have double-buffering enabled, although it doesn't
 * hurt to enable children.
 * <p>
 * The <code>disableDoubleBuffering</code> property controls whether or not
 * this disabling happens.  <em>Note:</em> this property does <em>not</em>
 * directly enable or disable double buffering.  It simply enables or
 * disables the <it>process</it> by which <code>JadeDisplay</code> turns
 * off double buffering on all of its ancestor components.  If true (the
 * default), then all ancestors have double buffering turned off whenever
 * a <code>HierarchyEvent</code> is received indicating a change in parents.
 * If false, nothing is done on receiving the event (<code>JadeDisplay</code>
 * never explicitly turns on double buffering, anywhere).  Practically
 * speaking, this means that you should set this flag only in the constructor,
 * or at least before you add the component into any container.  Setting the
 * flag to false means that any manual settings of double buffering you might
 * make are honored; <code>JadeDisplay</code> will not change any of them.
 * <p>
 * <b>Parallelism</b>
 * <p>
 * Because of the use of background processing, the Parallelism setting in
 * the <code>TileScheduler</code> attached to the image can make quite a
 * difference.  This component does not manage the parallelism however; this
 * is up to the application (e.g. via
 * <code>JAI.getDefaultInstance().getTileScheduler().setParallelism()</code>).
 * <p>
 * <b>RenderingChangeEvent response</b>
 * <p>
 * <code>JadeDisplay</code> monitors <code>RenderingChangeEvent</code>s (RCEs)
 * generated by the image, and automatically repaints portions of the screen
 * affected by that change.  So no manual painting is necessary when e.g.
 * JAI operator parameters are modified.
 * <p>
 * Exactly <em>how</em> that repaint happens, though, is controllable via
 * the RCE mode.  The values are:
 * <ul>
 * <li><code>RCE_REPAINT</code>: The default mode.  The affected areas are
 * repainted via a call to <code>repaint()</code>.  The background painter
 * is called (usually to erase the background to black) before the image is
 * repainted, just as with any normal repaint.
 * <li><code>RCE_FULL_NOERASE</code>: This mode uses <code>paintNoErase()</code>
 * to do the repainting.  This can eliminate flashing due to repainting the
 * background when the image changes, but could also lead to image areas that
 * contain garbage while waiting for the tile to be processed.  The garbage
 * could possibly be reduced by having a background painter that looks for
 * isDeferredTile==true and paints backgrounds for deferred tiles even if
 * isNoErase==true.  However, since by definition you have a new tile that
 * is not in the cache when an RCE is received (and thus it is virtually
 * certain to be deferred), this may be of limited usefulness.
 * <li><code>RCE_PARTIAL_NOERASE</code>: Like <code>RCE_FULL_NOERASE</code>,
 * but <code>paintNoErase()</code> is used only when the RCE indicates a
 * partial-image update is needed.  Full-image updates use
 * <code>repaint()</code>.  This may be useful for e.g. a mosaic operator,
 * where partial-image updates are common (when input images are changed),
 * since wholesale image changes will not leave unpainted debris on the
 * screen.
 * </ul>
 * <p>
 * Note that anything that causes the geometry of the image to change
 * (size, tiling, data type, etc.) will cause <code>repaint()</code> to
 * be used, regardless of the mode setting.  This is often the case for
 * JAI operators like Rotate, which change the output size with rotation
 * changes.  Also, despite the mode setting, other forms of repainting
 * (scrolls, exposes, etc.) will still happen as usual.  This mode affects
 * repaints <em>only</em> in response to a <code>RenderingChangeEvent</code>.
 * <p>
 * Because <code>RenderingChangeEvent</code> delivery is synchronous, it is
 * possible for an application to set the RCE mode before making an update,
 * make the update which generates the RCE (via
 * <code>RenderedOp.setParameterBlock()</code> or similar), then re-set the
 * RCE mode afterwards.  Thus the given mode applies only to one operation.
 * This could be very useful for cases where it is known a priori that the
 * display flashing will be undesirable when certain types of image updates
 * are performed.
 * <p>
 * <b>Miscellaneous</b>
 * <p>
 * This component owes a lot of heritage to the <code>XvicImage</code>
 * X-windows/Motif-based image display widget used in <code>xvd</code>.  If
 * not actual code, at least the concepts.
 * <p>
 * This may or may not still be true (copied from the JAI ImageCanvas source):
 * Due to the limitations of BufferedImage, only TYPE_BYTE of band
 * 1, 2, 3, 4, and TYPE_USHORT of band 1, 2, 3 images can be displayed
 * using this component.
 * <p>
 * The component does not work terribly well if the tile sizes are too small,
 * due to the overhead of tile processing.  This is especially apparent with
 * certain TIFF files, where tiles are a single line.  The user should reformat
 * the image into larger tiles in that case.
 * <p>
 * TBD: It is unknown if this component will work for printing.  It should,
 * in immediate mode, but this has not been tested.
 * <p>
 * TBD: Should property events be generated when properties are changed?
 * None have been needed to date.
 *
 * @author Bob Deen, JPL
 */
//!!!! Properties:
//!!!!  image
//!!!!  repaintPolicy
//!!!!  imageOrigin

//!!!! NEED to be notified if insets change, or image properties, or size
//!!!! of component.

public class JadeDisplay extends JPanel implements TileComputationListener,
		BackgroundPainter, PropertyChangeListener, HierarchyListener
{

    /** The source PlanarImage. */
    protected PlanarImage _im;

    /** The list of TilePainter objects.  Dimensions are [x][y]. */
    protected TilePainter _tilePainters[][];

    /** The image's ColorModel or one we supply. */
    protected ColorModel _colorModel;

    /** The image's min/max X and Y tile indices. */
    protected int _minTileX, _maxTileX, _minTileY, _maxTileY;
    /** The image's tile width/height. */
    protected int _tileWidth, _tileHeight;
    /** The image's tile grid X/Y offset. */
    protected int _tileGridXOffset, _tileGridYOffset;
    /** The number of tiles in the X/Y directions */
    protected int _numTileX, _numTileY;
    /** The minimum X/Y coordinate of the image */
    protected int _minX, _minY;
    /** The image size */
    protected int _imageWidth, _imageHeight;
    /** The w/h of the image in screen coordinates.  Used to clip to image
     *  bounds if the view is bigger than the actual image. */
    protected int _imageScreenWidth, _imageScreenHeight;

    /** The pixel to display in the UL corner of the component, or null. */ 
    protected Point _imageOrigin;

    /** Repaint policy.  Determines whether tiles are computed and displayed
     *  immediately, or deferred. */
    protected int _repaintPolicy;

    /** RenderingChangeEvent mode.  Determines how tiles are repainted when
     *  the image changes.  See the class comments. */
    protected int _RCE_mode;

    /** Whether or not to ensure double buffering is off. */
    protected boolean _disableDoubleBuffering;

    /** Whether or not to erase before repainting.
     *  @see #paintNoErase
     */
    //!!!! Should this be a user-settable parameter?  If so, need a user
    //!!!! setting and an active setting so we can restore user prefs.
    protected boolean _eraseBeforeRepaint;

//!!!! updates to insets??!
    /** Caches insets */
    // Initialized to null to serve as a flag indicating if _insets have
    // been properly set.
    protected Insets _insets = null;
    /** Insets as a clipping rectangle */
    protected Rectangle _insets_clip = new Rectangle(0, 0, 0, 0);
    /** Conversion between screen and image coordinates (img = scr + scr2img) */
    protected int _scr2imgX, _scr2imgY;

    /** The damage list */
    protected RectRegion _damageList;

    /** Overlay painters */
    protected ArrayList _immediateOverlayPainters = null;
    protected ArrayList _batchOverlayPainters = null;

    /** Background painter */
    protected BackgroundPainter _backgroundPainter = this;

//private RectRegionTest _region_tester;


//------ Constants ------

    /** Repaint immediately in all cases.  No queueing or background
     *  processing. */
    static public final int REPAINT_IMMEDIATE = 0;

    /** Defer tile computation to asynchronous threads in all possible cases.
     *  Repaints happen when the tile is computed. */
    static public final int REPAINT_DEFERRED = 1;

    /** If the tile is in the cache, same as REPAINT_IMMEDIATE.  If not,
     *  same as REPAINT_DEFERRED. */
    static public final int REPAINT_CACHE = 2;

    /** When a RenderingChangeEvent is received, use <code>repaint()</code>.
     *  This is the default. */
    static public final int RCE_REPAINT = 0;

    /** When a RenderingChangeEvent is received, use
     *  <code>paintNoErase()</code>. */
    static public final int RCE_FULL_NOERASE = 1;

    /** When a RenderingChangeEvent is received and it is a partial-image
     *  update, use <code>paintNoErase()</code>. */
    static public final int RCE_PARTIAL_NOERASE = 2;

    /** Enable debug prints */
    private static final boolean DPR = false;

/***********************************************************************
 * Factory function that creates and returns a <code>JScrollPane</code> which
 * contains a <code>JadeDisplay</code>.  The returned <code>JScrollPane</code>
 * can be added to a <code>JPanel</code> or whatever else is desired.
 * <p>
 * Any event handlers, such as mouse trackers, should be added to the
 * <code>JViewport</code> returned by <code>sp.getViewport())</code>
 * (where <code>sp</code> is the component returned by this method).
 * The <code>JadeDisplay</code> itself can be retrieved via
 * <code>sp.getViewport().getView()</code>.
 * <p>
 * The size of the viewport is set to the <code>h</code> and <code>v</code>
 * parameters.  If either is 0, the actual image's dimension is used instead.
 * Be careful doing this with large images!  Also, the process of determining
 * the image's dimension may cause it to be rendered (depending on what the
 * image is).  Be cautious if you default the display size.
 * <p>
 * A <code>MouseScroller</code> is attached to the image if the
 * <code>mouseScroller</code> parameter is <code>true</code>.  This enables
 * simple mouse panning of the image.  If <code>false</code>, no panner is
 * added (although the application is free to to so).  If you need to control
 * the panner, create it yourself; there is no way to get a handle to the one
 * created by this method.
 * <p>
 * This factory is a convenience function.  The display can be created on
 * your own if you want more control.  The guts look like this:
 * <p><pre>
 *     if (w == 0) w = image.getWidth();
 *     if (h == 0) h = image.getHeight();
 *     JPanel img_panel = new JadeDisplay(image);
 *     JScrollPane sp = new JScrollPane(img_panel);
 *     sp.setViewport(new JViewportImage());
 *     sp.setViewportView(img_panel);
 *     if (mouseScroller)
 *         new MouseScroller(sp.getViewport());
 *     sp.setPreferredSize(new Dimension(w, h));
 *     return sp;
 * </pre><p>
 * Note that a <code>JViewportImage</code> should be used instead of Swing's
 * <code>JViewport</code>.  It will give you smoother diagonal scrolling.
 * It should <em>work</em> with <code>JViewport</code> but mouse-based
 * scrolling will be very distracting to the user.
 */
    public static JScrollPane createDisplay(RenderedImage image,
			int w, int h, boolean mouseScroller)
    {
	if (w == 0) w = image.getWidth();
	if (h == 0) h = image.getHeight();

	JPanel img_panel = new JadeDisplay(image);
	JScrollPane sp = new JScrollPane(img_panel);
	sp.setViewport(new JViewportImage());
	sp.setViewportView(img_panel);

	if (mouseScroller)
	    new MouseScroller(sp.getViewport());

	sp.setPreferredSize(new Dimension(w, h));    // w,h determined earlier

	return sp;
    }

/***********************************************************************
 * Constructs a JadeDisplay to display a RenderedImage with the given
 * image origin, repaint policy, and double buffer disable flag.  A JAI
 * <code>PlanarImage</code> is actually required but if the supplied image
 * is not one, it will be automatically wrapped.
 *
 * @param im the RenderedImage to be displayed.
 * @param imageOrigin a Point specifying the image origin.
 * @param repaintPolicy the repaint policy.
 * @param disableDoubleBuffering whether or not to disable double buffering.
 * See the class comments for a discussion.
 */
    public JadeDisplay(RenderedImage im, Point imageOrigin, int repaintPolicy,
			boolean disableDoubleBuffering)
    {
	super();

	setLayout(null);
	_damageList = new RectRegion();

	_eraseBeforeRepaint = true;

	if (imageOrigin == null)
	    _imageOrigin = null;
	else
	    _imageOrigin = new Point(imageOrigin);
	_repaintPolicy = repaintPolicy;
	_disableDoubleBuffering = disableDoubleBuffering;
	_RCE_mode = RCE_REPAINT;

	setImageInternal(im);

//!!!!
//_region_tester = new RectRegionTest(_damageList);
//_region_tester.setPreferredSize(new Dimension(im.getWidth(), im.getHeight()));
//JFrame window = new JFrame("region");
//window.getContentPane().add(_region_tester);
//window.pack();
//window.show();
//!!!!

	addComponentListener(new ComponentAdapter() {
	    public void componentResized(ComponentEvent e) {
		handleResize();
	    }
	});

	// Register for HierarchyEvents to handle double buffer disabling
	addHierarchyListener(this);

    }

/***********************************************************************
 * Constructs a JadeDisplay to display a RenderedImage with the given
 * image origin and repaint policy.  A JAI <code>PlanarImage</code> is
 * actually required but if the supplied image is not one, it will be
 * automatically wrapped.  Double-buffering will be disabled by default.
 *
 * @param im the RenderedImage to be displayed.
 * @param imageOrigin a Point specifying the image origin.
 * @param repaintPolicy the repaint policy.
 *
 * @see #JadeDisplay(RenderedImage, Point, int, boolean)
 */
    public JadeDisplay(RenderedImage im, Point imageOrigin, int repaintPolicy)
    {
	this(im, imageOrigin, repaintPolicy, true);
    }

/***********************************************************************
 * Constructs a JadeDisplay to display a RenderedImage with default
 * origin (<code>null</code>), repaint policy (<code>REPAINT_CACHE</code>),
 * and double buffer disable flag (<code>false</code>).
 * @see #JadeDisplay(RenderedImage, Point, int, boolean)
 */
    public JadeDisplay(RenderedImage im)
    {
	this(im, null, REPAINT_CACHE, true);
    }

/***********************************************************************
 * Returns the image currently being displayed.
 */
    public RenderedImage getImage()
    {
	return _im;
    }

/***********************************************************************
 * Changes the source image to a new RenderedImage.
 * <p>
 * If the supplied image is <code>null</code>, the display will simply
 * be cleared to the background color (and remain that way until an image
 * is set).  No background or overlay painters are activated.
 */
    public void setImage(RenderedImage im)
    {
        setImageInternal(im);
        repaint();
//!!!! fire event
    }

/***********************************************************************
 * Changes the source image to a new RenderedImage while simultaneously
 * changing the image origin.  Useful if the new image has a different
 * minX/Y value to prevent multiple updates.
 * <p>
 * If the supplied image is <code>null</code>, the display will simply
 * be cleared to the background color (and remain that way until an image
 * is set).  No background or overlay painters are activated.
 *
 * @see #setImageOrigin(Point)
 */
    public void setImage(RenderedImage im, Point origin)
    {
	if (_imageOrigin == null)
	    _imageOrigin = null;
	else
	    _imageOrigin = new Point(origin);
        setImageInternal(im);
        repaint();
//!!!! fire event
    }

/***********************************************************************
 * Sets up a new image, registering and unregistering Listener's and
 * updating any derived values.  For internal use only.
 */
    protected void setImageInternal(RenderedImage im)
    {
	if (im != _im) {		// The image itself changed

	    // Register ourselves as a tile computation listener and a
	    // property change listener.

	    if (_im != null) {
		_im.removeTileComputationListener(this);
		_im.removePropertyChangeListener(this);
	    }

	    if (im == null) {
		_im = null;
	    }
	    else {
		if (im instanceof PlanarImage)
		    _im = (PlanarImage)im;
		else
		    _im = PlanarImage.wrapRenderedImage(im);

		_im.addTileComputationListener(this);
		_im.addPropertyChangeListener(this);
	    }
	}

	// If given a null image, do nothing

	if (im == null)
	    return;

	// Check to make sure the image has a suitable ColorModel

	_colorModel = _im.getColorModel();
	if (_colorModel == null) {
	    // If not, then create one.
	    _colorModel = PlanarImage.createColorModel(_im.getSampleModel());
	    if (_colorModel == null) {
		throw new IllegalArgumentException("Unable to display the supplied RenderedImage due to an invalid ColorModel.");
	    }
	}

	// Check for the JDK 1.3 ColorModel bug.

	_colorModel = ComponentColorModelGray.adjustColorModel(
			_im.getSampleModel(), _colorModel);

/*!!!! Is this needed??  Might be for transparent images.  Need to make sure
  !!!! the background is cleared for deferred tiles... is Swing doing this or
  !!!! must we??
        Object col = _im.getProperty("background_color");
        if (col != Image.UndefinedProperty) {
            _backgroundColor = (Color)col;
        }
!!!!*/

	// Update cached values from the image

	_numTileX = _im.getNumXTiles();
	_numTileY = _im.getNumYTiles();
        _minTileX = _im.getMinTileX();
        _maxTileX = _im.getMinTileX() + _numTileX - 1;
        _minTileY = _im.getMinTileY();
        _maxTileY = _im.getMinTileY() + _numTileY - 1;
        _tileWidth  = _im.getTileWidth();
        _tileHeight = _im.getTileHeight();
        _tileGridXOffset = _im.getTileGridXOffset();
        _tileGridYOffset = _im.getTileGridYOffset();
	_minX = _im.getMinX();
	_minY = _im.getMinY();
	_imageWidth = _im.getWidth();
	_imageHeight = _im.getHeight();

	// Set the preferred size of the component to the size of the
	// image.  The caller can adjust the actual size if desired.

	sizeComponent();

	// Set up the TilePainters.  There is one object per tile.
	//!!!! These could be set up lazily, as needed, which could
	//!!!! help for very small tiles (like a TIFF where tiles are a
	//!!!! single line).  The component does not work particulary well
	//!!!! in cases like that.

	_tilePainters = new TilePainter[_numTileX][_numTileY];
	for (int i=0; i < _numTileX; i++) {
	    for (int j=0; j < _numTileY; j++) {
		_tilePainters[i][j] = new TilePainter(this, _im, _colorModel,
				i + _minTileX, j + _minTileY, _repaintPolicy);
	    }
	}

	handleResize();		// set up any component-size-dependent things
    }

/***********************************************************************
 * Adds an <code>OverlayPainter</code> to this component at the given
 * position in the list.  If the supplied index is bigger than the
 * size of the list, the painter is simply added to the end of the list.
 * (the end of the list is painted last, and is thus on top).
 * <p>
 * If <code>isImmediate</code> is true, the painter is added in immediate
 * mode, meaning it is called for each and every screen update.  If it is
 * false, it is added in batch mode, meaning it is called when the damage
 * list is empty for a much larger area (often the whole window).
 * <p>
 * This method is safe to call from any thread.
 * <p>
 * <em>Note:</em>  Batch mode is not yet implemented.  Attempting to use
 * it will cause an UnsupportedOperationException.
 *
 * @param index index at which the specified element is to be inserted.
 * @param painter the <code>OverlayPainter</code> being registered.
 * @param isImmediate flag indicating immediate or batch mode.
 *
 * @see java.util.List#add(int, Object)
 * @throws IndexOutOfBoundsException if the index is < 0.
 */
    public synchronized void addOverlayPainter(int index,
				OverlayPainter painter, boolean isImmediate)
    {
	addOverlayPainterNRP(index, painter, isImmediate);
	repaint();		// force an update...
    }

/***********************************************************************
 * Adds an immediate-mode OverlayPainter to the end of the list (i.e.
 * on top of everything else.
 * <p>
 * This method is safe to call from any thread.
 *
 * @see #addOverlayPainter(int, OverlayPainter, boolean)
 */
    public synchronized void addOverlayPainter(OverlayPainter painter)
    {
	int n = 0;
	if (_immediateOverlayPainters != null)
	    n = _immediateOverlayPainters.size();

	addOverlayPainter(n, painter, true);
    }

/***********************************************************************
 * Adds an <code>OverlayPainter</code> to this component.  This is exactly
 * like <code>addOverlayPainter()</code> except that <code>repaint()</code>
 * is <em>not</em> called to refresh the display.  It is <em>imperative</em>
 * that the caller immediately call a variant of <code>repaint()</code> to
 * repaint the affected area.
 * <p>
 * The intent of this variant is to support <code>OverlayPainter</code>s with
 * limited bounding boxes.  Instead of requiring the entire image be redrawn,
 * only the area affected by the painter needs to be redrawn.
 * <p>
 * This method is safe to call from any thread.
 *
 * @see #addOverlayPainter(int, OverlayPainter, boolean)
 */
    public synchronized void addOverlayPainterNRP(int index,
				OverlayPainter painter, boolean isImmediate)
    {
	if (isImmediate) {
	    if (_immediateOverlayPainters == null)
		_immediateOverlayPainters = new ArrayList(1);
	    if (index > _immediateOverlayPainters.size())
		index = _immediateOverlayPainters.size();
	    _immediateOverlayPainters.add(index, painter);
	}
	else {
	    throw new UnsupportedOperationException("Batch not implemented yet!!!!\n");
	}
    }

/***********************************************************************
 * Removes the specified <code>OverlayPainter</code> from this component.
 * If the painter is not found, nothing happens and no error is generated.
 * If the painter happens to be in the list twice, only the first one found
 * will be removed (batch painters are removed first).
 * <p>
 * This method is safe to call from any thread.
 *
 * @param painter the <code>OverlayPainter</code> to remove.
 *
 * @see #addOverlayPainter
 */
    public synchronized void removeOverlayPainter(OverlayPainter painter)
    {
	removeOverlayPainterNRP(painter);
	repaint();			// refresh the screen
    }

/***********************************************************************
 * Removes the specified <code>OverlayPainter</code> from this component.
 * This is exactly like <code>removeOverlayPainter()</code> except that
 * <code>repaint()</code> is <em>not</em> called to refresh the display.
 * It is <em>imperative</em> that the caller immediately call a variant
 * of <code>repaint()</code> to repaint the affected area.
 * <p>
 * The intent of this variant is to support <code>OverlayPainter</code>s with
 * limited bounding boxes.  Instead of requiring the entire image be redrawn,
 * only the area affected by the painter needs to be redrawn.
 * <p>
 * This method is safe to call from any thread.
 *
 * @see #removeOverlayPainter(OverlayPainter)
 */
    public synchronized void removeOverlayPainterNRP(OverlayPainter painter)
    {
	if (_batchOverlayPainters != null) {
	    int index = _batchOverlayPainters.indexOf(painter);
	    if (index != -1) {
		_batchOverlayPainters.remove(index);
		return;
	    }
	}
	if (_immediateOverlayPainters != null) {
	    int index = _immediateOverlayPainters.indexOf(painter);
	    if (index != -1) {
		_immediateOverlayPainters.remove(index);
		return;
	    }
	}
    }

/***********************************************************************
 * Sets the given object to be the <code>BackgroundPainter</code> for this
 * component.  This replaces any old <code>BackgroundPainter</code>s (there
 * is only one).  The previous <code>BackgroundPainter</code> is returned
 * (which could be <code>null</code> if it's the default).
 * <p>
 * If <code>null</code> is passed in, the default background painting
 * behavior will be restored (which simply erases the background).  See
 * <code>BackgroundPainter</code> for the actual code.
 * <p>
 * This method is safe to call from any thread.
 * <p>
 *
 * @param bgpaint the <code>BackgroundPainter</code> being registered.
 *
 * @return the previous <code>BackgroundPainter</code> or <code>null</code>
 * if it's the default method.
 *
 * @see BackgroundPainter
 */
    public synchronized BackgroundPainter setBackgroundPainter(
					BackgroundPainter bgpaint)
    {
	BackgroundPainter bg_old = _backgroundPainter;
	if (bg_old == this)
	    bg_old = null;
	if (bgpaint == null)
	    _backgroundPainter = this;
	else
	    _backgroundPainter = bgpaint;
	return bg_old;
    }				// no need to repaint

/***********************************************************************
 * Default background painter method.  Implemented as follows:
 * <pre>
 *  private void paintBackground(Graphics g,
 *                              int x, int y, int width, int height,
 *                              boolean isNoErase, boolean isInsideImage,
 *                              boolean isDeferredTile)
 *  {
 *      if (isDeferredTile || isNoErase)
 *          return;
 *       g.setColor(this.getBackground());
 *      g.fillRect(x, y, width, height);
 *  }
 * </pre>
 * <p>
 * This method is public only because it implements the
 * <code>BackgroundPainter<</code> interface.  It should not be referenced
 * from outside.  In order to use this default, call
 * <code>setBackgroundPainter(null)</code>.
 * @see BackgroundPainter
 * @see #setBackgroundPainter
 */
    public void paintBackground(Graphics g,
                                int x, int y, int width, int height,
                                boolean isNoErase, boolean isInsideImage,
                                boolean isDeferredTile)
    {
        if (isDeferredTile || isNoErase)
            return;
        g.setColor(this.getBackground());
        g.fillRect(x, y, width, height);
    }

/***********************************************************************
 * Handles the component being resized by resetting all size-dependent
 * member variables.
 */
    protected void handleResize()
    {
	// Don't do anything if component's size has not been
	// properly set.
	if ((getWidth() == 0) || (getHeight() == 0))
	    return;
	_insets = getInsets();

	// Insets as a clipping rectangle (w/h)
//!!!! CHANGE THIS if the insets change
	if (_imageOrigin == null) {
	    _insets_clip = new Rectangle(_insets.left, _insets.top,
		getWidth() - _minX - _insets.left - _insets.right,
		getHeight() - _minY - _insets.top - _insets.bottom);
	}
	else {
	    _insets_clip = new Rectangle(_insets.left, _insets.top,
		getWidth() - _imageOrigin.x - _insets.left - _insets.right,
		getHeight() - _imageOrigin.y - _insets.top - _insets.bottom);
	}


	// Converts from Screen to Image coordinates (img = scr + scr2imgX/Y)

	if (_imageOrigin == null) {
	    _scr2imgX = _minX - _insets.left;
	    _scr2imgY = _minY - _insets.top;
	}
	else {
	    _scr2imgX = _imageOrigin.x - _insets.left;
	    _scr2imgY = _imageOrigin.y - _insets.top;
	}

	if (DPR) System.out.println("scr2imgX="+_scr2imgX+", scr2imgY="+_scr2imgY);
    }

/***********************************************************************
 * Sizes the component based on the image size.
 */
    protected void sizeComponent()
    {
	// Insets as a clipping rectangle (w/h)
//!!!! CHANGE THIS if the insets change
	// We use local variable to store insets.  Member variable _insets
	// is only assigned in handleResize() method.
	Insets insets = getInsets(_insets);

	// Set the preferred size of the component to the size of the
	// image.  The caller can adjust the actual size if desired.

	int w, h;
	if (_imageOrigin == null) {	// origin == min so they cancel
	    w = _imageWidth + insets.left + insets.right;
	    h = _imageHeight - insets.top + insets.bottom;
	}
	else {
	    w = _imageWidth - (_imageOrigin.x - _minX) +
						insets.left + insets.right;
	    h = _imageHeight - (_imageOrigin.y - _minY) +
						insets.top + insets.bottom;
	}
	_imageScreenWidth = w;
	_imageScreenHeight = h;
	setPreferredSize(new Dimension(w, h));
	// Don't call setSize()!!  It doesn't allow the ViewportLayout to
	// do its job.

	// Needed to get scrollbars to disappear and reappear when the image
	// changes (via no action by the user... i.e. an image rotate).
	revalidate();
    }

/***********************************************************************
 * Changes the <code>imageOrigin</code> property.  This property defines
 * the image coordinate that is shown in the upper left corner of the
 * component.  Any part of the image above and to the left of that
 * coordinate will not be displayed.  Note that the logical location of
 * the image is taken into account, so if the image starts at (-100,-100)
 * (see <code>RenderedImage.getMinX/Y()</code>), then the
 * <code>imageOrigin</code> would also need to be set to (-100,-100) in
 * order to display the whole image.  This <code>imageOrigin</code> is a
 * hard cropping and should not be confused with a Pan value, which is
 * managed entirely by the <code>JScrollPane</code> (or other higher-level
 * component).
 * <p>
 * If the origin is set to <code>null</code>, the origin will automatically
 * be set to the image's <code>minX</code> and <code>minY</code> values, and
 * will be reset whenever those change.  This means that the origin is pinned
 * to the actual corner of the image.
 */
    public void setImageOrigin(Point origin)
    {
	if (_imageOrigin == null)
	    _imageOrigin = null;
	else
	    _imageOrigin = new Point(origin);
	sizeComponent();
	handleResize();
//!!!! event??
        repaint();
    }
   
/***********************************************************************
 * Returns a copy of the image origin value.  Will return <code>null</code>
 * if that's the way the user set it (in which case the image's
 * <code>getMinX()</code> and <code>getMinY()</code> will need to be called
 * by the user to get the values).
 * @see #setImageOrigin
 * @see #getCurrentImageOrigin
 */
    public Point getImageOrigin()
    {
	if (_imageOrigin == null)
	    return null;
	return new Point(_imageOrigin);
    }

/***********************************************************************
 * Returns a copy of the current image origin value.  This differs from
 * <code>getImageOrigin()</code> because it will never return <code>null</code>.
 * If the origin has not been set, the image's <code>getMinX/Y()</code> will
 * be called as a convenience, and returned.  Note that this value can
 * change if the image min does, so it should be used immediately and not
 * stored.
 * @see #setImageOrigin
 * @see #getImageOrigin
 */
    public Point getCurrentImageOrigin()
    {
	if (_imageOrigin == null)
	    return new Point(_minX, _minY);
	return new Point(_imageOrigin);
    }

/***********************************************************************
 * Sets the repaint policy.  See the field definitions and class comments
 * for details.
 */
    public void setRepaintPolicy(int policy)
    {
	if (policy != REPAINT_IMMEDIATE && policy != REPAINT_DEFERRED &&
					   policy != REPAINT_CACHE) {
	    throw new IllegalArgumentException("Invalid repaint policy: "
							+ policy);
	}
	_repaintPolicy = policy; // takes effect next repaint; no need to do now

	// Notify all the tile painters of the change

	for (int i=0; i < _numTileX; i++) {
	    for (int j=0; j < _numTileY; j++) {
		_tilePainters[i][j].setRepaintPolicy(_repaintPolicy);
	    }
	}
    }

/***********************************************************************
 * Gets the repaint policy.  See the field definitions and class comments
 * for details.
 */
    public int getRepaintPolicy()
    {
	return _repaintPolicy;
    }

/***********************************************************************
 * Sets the RenderingChangeEvent repaint mode.  See the field definitions
 * and class comments for details.
 */
    public void setRCEmode(int mode)
    {
	if (mode != RCE_REPAINT && mode != RCE_FULL_NOERASE &&
				   mode != RCE_PARTIAL_NOERASE) {
	    throw new IllegalArgumentException("Invalid RCE mode: " + mode);
	}

	_RCE_mode = mode;
    }

/***********************************************************************
 * Gets the RenderingChangeEvent repaint mode.  See the field definitions
 * and class comments for details.
 */
    public int getRCEmode()
    {
	return _RCE_mode;
    }

/***********************************************************************
 * Sets the current state of the double-buffer disable flag.  Note that
 * this does not change the double-buffer status of any actual components;
 * that happens when a <code>HierarchyChanged</code> event is received.
 * For that matter, it is recommended that this function be called before
 * adding the component to a container... or better yet, set the flag in the
 * constructor.  See the class comments for a discussion.
 */
    public void setDisableDoubleBuffering(boolean flag)
    {
	_disableDoubleBuffering = flag;
    }

/***********************************************************************
 * Gets the current state of the double-buffer disable flag.  See the
 * class comments for a discussion.
 */
    public boolean getDisableDoubleBuffering()
    {
	return _disableDoubleBuffering;
    }

/***********************************************************************
 * Internal routines to translate image coordinates to tile indices.
 */
    private int XtoTileX(int x)
    {
        return (int) Math.floor((double) (x - _tileGridXOffset)/_tileWidth);
    }

    private int YtoTileY(int y)
    {
        return (int) Math.floor((double) (y - _tileGridYOffset)/_tileHeight);
    }
    
    private int TileXtoX(int tx)
    {
        return tx*_tileWidth + _tileGridXOffset;
    }
    
    private int TileYtoY(int ty)
    {
        return ty*_tileHeight + _tileGridYOffset;
    }

/***********************************************************************
 * Adds a Rectangle to the damage list.  Normally called by
 * <code>paintComponent()</code> with the current clip area.
 * Also returns the bounding rectangle of the damage list after
 * the new piece is added (this is done here solely to avoid another
 * synchronize).
 */
    protected Rectangle addDamage(Rectangle rect)
    {
	Rectangle bounds = null;
	synchronized(_damageList) {
	    _damageList.union(rect);
//_region_tester.update();
	    bounds = _damageList.getBounds();
	}
	return bounds;
    }

/***********************************************************************
 * Subtracts a Rectangle from the damage list.  Normally called when
 * a tile has been successfully painted.
 * <p>
 * <em>Warning!</em>  This routine is only intended to be called by
 * <code>TilePainter</code>.  Do not call it from application code.
 */
    public void subtractDamage(Rectangle rect)
    {
	synchronized(_damageList) {
	    _damageList.subtract(rect);
//_region_tester.update();
	}
    }

/***********************************************************************
 * Subtracts a Rectangle from the damage list.  Normally called when
 * graphics outside the image have been successfully painted.  Also returns
 * the bounding rectangle of the damage list after the new piece is
 * subtracted (this is done here solely to avoid another synchronize).
 */
    protected Rectangle subtractDamage2(Rectangle rect)
    {
	Rectangle bounds = null;
	synchronized(_damageList) {
	    _damageList.subtract(rect);
//_region_tester.update();
	    bounds = _damageList.getBounds();
	}
	return bounds;
    }

/***********************************************************************
 * Paints a portion of the display without erasing it first.  This also
 * causes the painting to happen immediately, without waiting for
 * <code>repaint</code> to batch up requests.  This is intended for use
 * with and by graphics overlays.  If the background image didn't change,
 * and only the overlay did, then erasing the background first causes
 * undesirable flashing.  Calling this routine instead of <code>repaint</code>
 * should eliminate most of that flashing.
 * <p>
 * We don't paint everything this way because it is very distracting while
 * scrolling.  The image to paint changes when you scroll, but that could
 * take some time.  In the meantime, you see leftover garbage, which is
 * objectionable.  Erasing first gives the user some clue as to how much
 * is left to repaint.
 * <p>
 * <em>Note:</em>  This routine does not work very well if Swing double
 * buffering is enabled.  You can get undesirable flashing, which is what
 * the routine is supposed to eliminate in the first place.  Double buffering
 * is disabled by default, though.  See the discussion on double buffering
 * in the class comments.
 * <p>
 * <em>Important!</em>  As with all other paint-related calls, the coordinates
 * here are <em>Viewport</em> coordinates, which are not necessarily the same
 * as <em>Image</em> coordinates!!  There is an offset if the image origin
 * does not start at zero.  <code>getCurrentImageOrigin()</code> can be
 * used to get this offset.  This may cause confusion because
 * <code>OverlayPainter</code>s are called using <em>Image</em> coordinates.
 * <p>
 * <em>Note:</em>  Unlike <code>repaint()</code>, this method must be
 * called from the Swing/AWT Event thread.  No check is made, and the
 * behavior is undefined if the wrong thread is used.
 * <code>SwingUtilities.invokeLater()</code> can be used if you're on the
 * wrong thread.
 * <p>
 * We used to override <code>paintImmediately()</code> for this function,
 * but <code>paintImmediately</code> is called other places in the viewport
 * framework, and suppressing erases caused really objectionable flashing.
 * This routine calls <code>paintImmediately</code> (after setting a flag)
 * so any restrictions with that routine apply here as well.
 *
 * @see #repaint
 * @see #addOverlayPainter
 * @see JComponent#paintImmediately(int,int,int,int)
 * @see SwingUtilities#invokeLater
 */
    public void paintNoErase(int x, int y, int w, int h)
    {
	// We could save and restore _eraseBeforeRepaint but you run the
	// risk of this being called reentrantly somehow and thus restoring
	// a bad value... so why bother.  That could be an issue if we allow
	// user setting of this preference though (in which case we'd need
	// to keep both a user setting and a current setting).

	_eraseBeforeRepaint = false;
	super.paintImmediately(x, y, w, h);
	_eraseBeforeRepaint = true;
    }

/***********************************************************************
 * Exactly like <code>paintNoErase(x,y,w,h)</code> except that the
 * area to be repainted is a <code>Rectangle</code>.  See that routine
 * for details.
 *
 * @see #paintNoErase(int,int,int,int)
 */
    public void paintNoErase(Rectangle r)
    {
	_eraseBeforeRepaint = false;
	super.paintImmediately(r);
	_eraseBeforeRepaint = true;
    }

/***********************************************************************
 * Special-purpose routine that repaints one overlay object only, without
 * repainting the background image or other overlays.
 * <p>
 * <em>WARNING!</em>  This routine is dangerous.  It should ONLY be called
 * to <em>exactly</em> overpaint an existing graphic, such as for color
 * cycling.  The exact shape of the existing overlay must be repainted, or
 * expanded.  The graphic cannot shrink, since the underlying image is not
 * refreshed and thus the old graphic cannot be erased.  Also, the overlays
 * "on top" of this one are NOT repainted, so they may be overwritten
 * (stacking order is not preserved with this routine).  In addition, any
 * kind of translucency or blending effect that other overlays may try to
 * do will not work with this routine.
 * <p>
 * So, users of this routine must be aware of the global overlay environment,
 * and must only repaint (e.g. changing colors) or expand the graphics painted
 * by the overlay.
 * <p>
 * Note that if the region being requested is not completely drawn, the
 * call will be converted into a call to <code>repaint(r)</code>, which
 * will cause ALL overlays in the area to be redrawn, not just this one
 * (thus it may no longer be on top).
 * <p>
 * It is legal to call this routine with an <code>OverlayPainter</code> that
 * is not registered as a painter (via <code>addOverlayPainter</code>
 * <it>et al</it>.  This can be used to advantage to paint special overlays
 * (color cycling, etc).  However, in this case the specified painter
 * <em>WILL NOT</em> be called on expose events, or if the call to this
 * routine is converted into a call to <code>repaint</code>, as described
 * above.  Thus this feature must be used with extreme caution.
 * <p>
 * <em>Important!</em>  As with all other paint-related calls, the coordinates
 * of <code>r</code> are <em>Viewport</em> coordinates, which are not
 * necessarily the same as <em>Image</em> coordinates!!  There is an offset if
 * the image origin does not start at zero.
 * <code>getCurrentImageOrigin()</code> can be used to get this offset.  This
 * may cause confusion because <code>OverlayPainter</code>s are called using
 * <em>Image</em> coordinates.
 * <p>
 * This routine must be called from the Swing/AWT event thread.
 *
 * @param painter Specifies which <code>OverlayPainter</code> object should
 * do the painting.  May not be called if the area is not completely drawn
 * (see class comments).  Only immediate-mode <code>OverlayPainter</code>s
 * are supported.
 * @param r The area to redraw.
 * @throws IllegalArgumentException if <code>index</code> is out of range.
 *
 * @see #addOverlayPainter(int, OverlayPainter, boolean)
 * @see JComponent#repaint(Rectangle)
 */
    public void paintOneOverlay(OverlayPainter painter, Rectangle r)
    {
	if (!isShowing())
	    return;			// nothing to do...

	Rectangle image_r = (Rectangle)r.clone();
	image_r.translate(_scr2imgX, _scr2imgY);	// in Image coords

	boolean intersects = false;
	synchronized(_damageList) {
	    intersects = _damageList.intersects(image_r);
	}

	if (intersects) {	// We intersect the damage area - just repaint
	    repaint(r);
	    return;
	}

	// We're good.  Do the repaint.  We can paint directly on the component.

	Graphics2D g = (Graphics2D)getGraphics();
	g.clip(r);			// screen coords

	painter.paintOverlay(g);
    }

/***********************************************************************
 * Called by Swing to repaint the component.  Goes through each tile and
 * tells any TilePainters that intersect the new damaged area to do their
 * thing.
 */
    public void paintComponent(Graphics g)
    {

        if (_im == null) {	// No image, just erase and return
            // Per Javadoc comment, we are using fillRect() and not
            // clearRect() to ensure that an offscreen image is 
            // cleared to a specific color.
	    g.setColor(this.getBackground());
	    g.fillRect(0, 0, getWidth(), getHeight());
	    return;
        }

        // handleResize() will properly set _insets if needed.
        if (_insets == null )
            handleResize();

        // Get the clipping rectangle and translate it into image coordinates. 
	// This is the new damaged area.

        Rectangle damage = g.getClipBounds();
        if (damage == null) {		// whole thing is damaged?
            damage = new Rectangle(0, 0, getWidth(), getHeight());
        }

	// Clip damage area to exclude insets

	damage = damage.intersection(_insets_clip);

	if (damage.isEmpty())
	    return;			// nothing to do!

	if (DPR) System.out.println("damage="+damage);

	// Convert damage area from screen to image coordinates

	damage.translate(_scr2imgX, _scr2imgY);

	// See if there is any damage outside the image bounds, and deal with
	// it separately.  We don't have to worry about <min because the
	// ViewportLayout won't allow it.  But it will make us bigger than
	// the image on occasion, so we have to worry about the right/bottom
	// sides.
	// We have to add the outside area to the damage list temporarily so
	// getGraphics() won't clip it out entirely.  This shouldn't happen
	// all that often though.

	if (damage.x+damage.width > _minX+_imageWidth) {

	    // Determine area outside the image on the right

	    Rectangle outside_right = new Rectangle(damage);
	    int maxx = outside_right.x + outside_right.width - 1;
	    outside_right.x = Math.max(outside_right.x, _minX+_imageWidth);
	    outside_right.width = maxx - outside_right.x + 1;

	    addDamage(outside_right);
	    Graphics g_over = getGraphics(outside_right, g);
	    if (g_over != null) {
		_backgroundPainter.paintBackground(g_over,
			outside_right.x, outside_right.y,
			outside_right.width, outside_right.height,
			!_eraseBeforeRepaint, false, false);
		repaintOverlay(g_over);
		g_over.dispose();
	    }
	    subtractDamage(outside_right);

	    // Reduce the damage by that amount (damage covers only image area)

	    damage.width = Math.min(damage.width, outside_right.x-damage.x);
	}

	if (damage.y+damage.height > _minY+_imageHeight) {

	    // Determine area outside the image on the bottom

	    Rectangle outside_bottom = new Rectangle(damage);
	    int maxy = outside_bottom.y + outside_bottom.height - 1;
	    outside_bottom.y = Math.max(outside_bottom.y, _minY+_imageHeight);
	    outside_bottom.height = maxy - outside_bottom.y + 1;

	    addDamage(outside_bottom);
	    Graphics g_over = getGraphics(outside_bottom, g);
	    if (g_over != null) {
		_backgroundPainter.paintBackground(g_over,
			outside_bottom.x, outside_bottom.y,
			outside_bottom.width, outside_bottom.height,
			!_eraseBeforeRepaint, false, false);
		repaintOverlay(g_over);
		g_over.dispose();
	    }
	    subtractDamage(outside_bottom);

	    // Reduce the damage by that amount (damage covers only image area)

	    damage.height = Math.min(damage.height, outside_bottom.y-damage.y);
	}

	if (damage.isEmpty())
	    return;			// nothing left to do!

	// Now add the damage to the list

	Rectangle damage_bounds = addDamage(damage);

	// Clear the damaged area (in case the repaint is deferred)
	// only if the flag is not set.  We don't want to clear during
	// an immediate repaint.
	Graphics g_img = g.create();
	g_img.translate(- _scr2imgX, - _scr2imgY);	// make image coords
	g_img.setClip(damage);
	_backgroundPainter.paintBackground(g_img,
			damage.x, damage.y, damage.width, damage.height,
			!_eraseBeforeRepaint, true, false);

        // Determine the extent of the damage in tile coordinates.
	// We use the overall damage here, not the damage just in this call,
	// in order to cancel tiles that are no longer relevant (without
	// looping over the entire image).

        int txmin, txmax, tymin, tymax;
        int ti, tj;

        txmin = XtoTileX(damage_bounds.x);
        txmin = Math.max(txmin, _minTileX);
        txmin = Math.min(txmin, _maxTileX);

        txmax = XtoTileX(damage_bounds.x + damage_bounds.width - 1);
        txmax = Math.max(txmax, _minTileX);
        txmax = Math.min(txmax, _maxTileX);

        tymin = YtoTileY(damage_bounds.y);
        tymin = Math.max(tymin, _minTileY);
        tymin = Math.min(tymin, _maxTileY);

        tymax = YtoTileY(damage_bounds.y + damage_bounds.height - 1);
        tymax = Math.max(tymax, _minTileY);
        tymax = Math.min(tymax, _maxTileY);

	// Determine the cache for the image

	RenderedImage rendering = _im;
	TileCache cache = JAI.getDefaultInstance().getTileCache();
	if (_im instanceof RenderedOp) {
	    rendering = ((RenderedOp)_im).getCurrentRendering();
	    cache = (TileCache)((RenderedOp)_im).getRenderingHints().get(
							JAI.KEY_TILE_CACHE);
	}

        // Loop over tiles within the damaged area

	Rectangle visible = getVisibleRect().intersection(_insets_clip);
	if (visible.isEmpty()) {
	    g_img.dispose();
	    return;			// no visible area
	}
	visible.translate(_scr2imgX, _scr2imgY);	// in image coords

	for (ti = txmin; ti <= txmax; ti++) {
	    for (tj = tymin; tj <= tymax; tj++) {
		TilePainter tp = _tilePainters[ti-_minTileX][tj-_minTileY];
		Rectangle bounds = tp.getBounds();
		if (visible.intersects(bounds)) {	// Tile is visible
		    if (damage.intersects(bounds)) {	// and damaged
			boolean painted = tp.paintTile(cache, rendering, g);
			if (!painted) {
			    Rectangle bkgnd = damage.intersection(bounds);
			    g_img.setClip(bkgnd);
			    _backgroundPainter.paintBackground(g_img,
				    bkgnd.x, bkgnd.y, bkgnd.width, bkgnd.height,
				    !_eraseBeforeRepaint, true, true);
			}
		    }
		}
		else {
		    tp.cancelTile();		// no longer visible
		}
	    }
	}

	g_img.dispose();

	// Done!

    }

/***********************************************************************
 * Abort all queued computations.  This is called by the hierarchy
 * listener when it determines the component is no longer showing, or
 * when we get a RenderingChangeEvent.
 * <p>
 * <em>Warning:</em> Calling this routine when the component is showing
 * may result in unpainted holes.
 */

    protected void abortQueuedTiles()
    {
	for (int i=0; i < _numTileX; i++) {
	    for (int j=0; j < _numTileY; j++) {
		_tilePainters[i][j].cancelTile();
	    }
	}
    }

/***********************************************************************
 * TileComputationListener functions.  This function is a "broker", or
 * central switching yard, which passes the failure on to the appropriate
 * TilePainter to handle.
 */
    public void tileComputationFailure(Object eventSource,
		TileRequest[] requests, PlanarImage image, int tileX, int tileY,
		Throwable situation)
    {
        //get local reference to painter, in case things change
        //from another thread
        final TilePainter tp = getTilePainter(tileX, tileY);
        
        //if non-null, then dispatch call to it
        if (tp != null)
        {
            tp.tileComputationFailure(eventSource, requests, image, 
                                      tileX, tileY, situation);
        }         
    }

/***********************************************************************
 * TileComputationListener functions.  This function is a "broker", or
 * central switching yard, which gets notifications for each tile that is
 * completed, and routes that notification to the appropriate TilePainter.
 * This must be done here rather than in TilePainter directly for efficiency
 * reasons.  If JAI adds a per-tile notification mechanism, this can be
 * dispensed with (and this class would no longer need to implement
 * TileComputationListener).
 */
    public void tileComputed(Object eventSource, TileRequest[] requests,
			PlanarImage image, int tileX, int tileY,
			Raster tile)
    {
        //get local reference to painter, in case things change
        //from another thread
        final TilePainter tp = getTilePainter(tileX, tileY);
        
        //if non-null, then dispatch call to it
        if (tp != null)
        {
            tp.tileComputed(eventSource, requests, image, tileX, tileY, tile);
        }        
    }

/***********************************************************************
 * TileComputationListener functions.  This function is a "broker", or
 * central switching yard, which gets notifications for each tile that is
 * cancelled, and routes that notification to the appropriate TilePainter.
 */
    public void tileCancelled(Object eventSource, TileRequest[] requests,
			PlanarImage image, int tileX, int tileY)
    {
        //get local reference to painter, in case things change
        //from another thread
        final TilePainter tp = getTilePainter(tileX, tileY);
        
        //if non-null, then dispatch call to it
        if (tp != null)
        {
            tp.tileCancelled(eventSource, requests, image, tileX, tileY);
        }
    }

    
/***********************************************************************
 * Convenience method to retrieve the TilePainter for
 * a specific tile coordinate.  This method gracefully
 * handlers when the indices are out of range, in which
 * case, null would be returned.
 * @param tileX Tile's x index
 * @param tileY Tile's y index
 * @return TilePainter at tile index, or null.
 */
    protected TilePainter getTilePainter(int tileX, int tileY)
    {
        TilePainter tp = null;
        
        //we check that tile indices are legal, but things can
        //change from another thread, so we will still wrap it
        //up with a try/catch to gracefully handle that issue
        try {
            if (tileX >= _minTileX && tileX <= _maxTileX &&
                tileY >= _minTileY && tileY <= _maxTileY)
            {
                int indexX = tileX-_minTileX;
                int indexY = tileY-_minTileY;
                tp = _tilePainters[indexX][indexY];
            }
        } catch (ArrayIndexOutOfBoundsException arEx) {
            tp = null;
        } catch (NullPointerException npEx) {
            //shouldn't happen, but...
            tp = null;
        }
        
        return tp;
    }
    
    
/***********************************************************************
 * Returns a <code>Graphics2D</code> object that can be used to paint a
 * tile in the given rectangular bounds.  The clip region of the returned
 * <code>Graphics2D</code> will be set to the intersection of the damage
 * list, the visible area, and the supplied bounds.  In addition, the
 * <code>Graphics2D</code> object will be translated appropriately to be
 * able to draw the tile directly in image coords, so that coordinate (0,0)
 * is at the upper left corner of the image.
 * <p>
 * The supplied <code>Graphics</code> object is used as a basis if it
 * exists (the clip area and translation is set), or if it doesn't exist,
 * a <code>Graphics</code> object is obtained from
 * <code>JComponent.getGraphics()</code>.
 * <p>
 * It is recommended (not not required) to call <code>Graphics.dispose()</code>
 * when you are done with the returned Graphics object.  A new one is created
 * even if <code>g</code> is passed in.
 *
 * @param bounds The bounding rectangle of the tile
 * @param g The graphics object to use, or null to get a fresh one
 */
    public Graphics2D getGraphics(Rectangle bounds, Graphics g)
    {
	Graphics2D g2d = null;
	if (g == null)
	    g2d = (Graphics2D)getGraphics();
	else
	    g2d = (Graphics2D)g.create();

	if (g2d == null)
	    return null;		// not currently visible

        // handleResize() will properly set _insets if needed.
        if (_insets == null )
            handleResize();

	// Intersect the tile and visible bounds first to get the smallest
	// possible area

	Rectangle visible = getVisibleRect().intersection(_insets_clip);
	if (visible.isEmpty())
	    return null;		// no visible area

	visible.translate(_scr2imgX, _scr2imgY);

	// Now that visible is in image coordinates, intersect with tile bounds

	Rectangle clip_tile = bounds.intersection(visible);
	if (clip_tile.isEmpty())
	    return null;		// no visible area for this tile

	// Now intersect that with the damage list and take the bounds of
	// that region (since Graphics clipping can only handle a single
	// rectangle).  We do the above first to minimize the clipping bounds.

	RectRegion dmg;
	synchronized(_damageList) {
	    dmg = _damageList.createIntersect(clip_tile);
	}

	Rectangle clip = dmg.getBounds();
	if (clip.isEmpty())
	    return null;

	clip.translate(-_scr2imgX, -_scr2imgY);
	g2d.setClip(clip);

	// Translate the Graphics' origin so the tile is at (0,0)

	g2d.translate(- _scr2imgX, - _scr2imgY);


	return g2d;
    }

/***********************************************************************
 * Called by <code>TilePainter</code> to refresh an area of the overlay.
 * If there are any immediate-mode overlay painters, they are called
 * immediately.  If there are any batch mode painters, the area is added
 * to the overlay damage list for later painting.  The area to paint is
 * the <code>clipRect</code> of the supplied <code>Graphics</code> object.
 * <p>
 * <em>User code should not call this routine!</em>
 */
    public void repaintOverlay(Graphics g)
    {
	if (_immediateOverlayPainters != null) {
	    ArrayList l = null;
	    synchronized(this) {
		l = (ArrayList)_immediateOverlayPainters.clone();
	    }
	    Iterator i = l.iterator();
	    while (i.hasNext()) {
		OverlayPainter painter = (OverlayPainter)i.next();
		painter.paintOverlay(g);
	    }
	}
	if (_batchOverlayPainters != null)
	    throw new UnsupportedOperationException("batch mode not implemented yet!!!!!");
    }

/***********************************************************************
 * Responds to a PropertyChangeEvent from the image.  We look for
 * RenderingChangeEvents and use them to update the display.  Other
 * properties are ignored.
 */
    public void propertyChange(PropertyChangeEvent evt)
    {
	if (DPR) System.out.println("EVENT: " + evt.getClass());

	if (evt instanceof RenderingChangeEvent) {
	    RectRegion old_damage;
	    synchronized(_damageList) {
		old_damage = (RectRegion)_damageList.clone();
	    }
	    abortQueuedTiles();

	    // Check to see if the old tile grid is still good.  If any of
	    // the image's geometry parameters have changed, we need to
	    // replace the TilePainters.

	    if (_imageWidth != _im.getWidth() ||
		_imageHeight != _im.getHeight() ||
		_minX != _im.getMinX() ||
		_minY != _im.getMinY() ||
		_tileWidth != _im.getTileWidth() ||
		_tileHeight != _im.getTileHeight() ||
		_minTileX != _im.getMinTileX() ||
		_minTileY != _im.getMinTileY() ||
		_numTileX != _im.getNumXTiles() ||
		_numTileY != _im.getNumYTiles() ||
		_tileGridXOffset != _im.getTileGridXOffset() ||
		_tileGridYOffset != _im.getTileGridYOffset() ||
		_colorModel != _im.getColorModel()) {

		if (DPR) System.out.println("image geometry changed");
		setImageInternal(_im);
		repaint();
	    }
	    else {		// Same geometry, just the pixels changed
		RenderingChangeEvent rce = (RenderingChangeEvent)evt;
		Shape dirty = rce.getInvalidRegion();
		if (dirty == null) {		// entire image changed
		    if (DPR) System.out.println("event: entire image dirty");
		    if (_RCE_mode == RCE_REPAINT)
		        repaint();
		    else	// RCE_FULL_NOERASE or RCE_PARTIAL_NOERASE
			paintNoErase(0, 0, getWidth(), getHeight());
		}
		else {				// just part of it changed
		    Rectangle bounds = dirty.getBounds();
		    if (DPR) System.out.println("event: only part of image dirty: " + bounds);
		    // Merge dirty area with the damage list we aborted.
		    // Most of the time that will be empty so this in effect
		    // repaints just the change.  But if there were outstanding
		    // damage *outside* of the changed area, it still needs
		    // repainting.

		    old_damage.union(bounds);
		    Rectangle repaints[] = old_damage.getRectangles();
		    for (int i=0; i < repaints.length; i++) {
			Rectangle r = repaints[i];
			// we can modify the array since old_damage is a temp.
			r.translate(- _scr2imgX, - _scr2imgY); // to scr coords
			if (!r.isEmpty()) {
			    if (_RCE_mode == RCE_PARTIAL_NOERASE)
				paintNoErase(r);
			    else
				repaint(r);
			}
		    }
		}
	    }

	}
    }

/***********************************************************************
 * We have to monitor hierarchy events for two reasons:
 * <ul>
 * <li>If any parent changed, and we're disabling double buffering,
 * then we must disable it all the way to the root component.
 * Hopefully other sibling components will have DB turned on and
 * will thus be buffered (JPanel has DB turned on by default).
 * See the class comments for a discussion.
 *
 * <li>If we are no longer showing, then we should abort all queued
 * repaints.  This is an efficiency measure; getGraphics() will return
 * null if we're no longer showing, but there's no point in computing the
 * tiles then.  This commonly occurs when the image is in a JTabbedPane
 * and the user clicks a different tab before the repaints are done.
 * </ul>
 */

    public void hierarchyChanged(HierarchyEvent e)
    {
	if (DPR) System.out.println("Hierarchy event: " + e);

	if (e.getID() != HierarchyEvent.HIERARCHY_CHANGED)
	    return;

	// First look at the showing status

	if ((e.getChangeFlags() & HierarchyEvent.SHOWING_CHANGED) != 0) {
	    if (!isShowing()) {		// we got turned off!
		abortQueuedTiles();
	    }
	}

	// Now look at reparenting for double buffering

	if (!_disableDoubleBuffering)
	    return;
	if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) == 0)
	    return;
	Component c = this;
	while (c != null) {
	    if (c instanceof JComponent)
		((JComponent)c).setDoubleBuffered(false);
	    c = c.getParent();
	}
    }


}

