import javax.media.jai.*;
import javax.media.jai.widget.*;
import javax.media.jai.iterator.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.util.*;
import javax.swing.event.*;
import jpl.mipl.jade.*;


/** Sample program that loads an image, does a few sample operators, and
  * displays the result using the JADE Display component.  The file to load
  * is the (only) command-line argument.
  */

public class JadeDisplayTest implements OverlayPainter, BackgroundPainter
{
    public static void main(String argv[])
    {
//	new tilecachetool.TCTool();
	new JadeDisplayTest(argv);
    }

/***********************************************************************/

    ParameterBlockJAI _loadPB, _rescalePB, _rotPB, _zoomPB;
    RenderedOp _loadImage, _rescaleImage, _rotImage, _zoomImage;
    JSlider _offsetSlider, _rotSlider, _zoomSlider;
    JadeDisplay _panel1, _panel2;
    RenderedOp _finalImage;
    JadeDisplay _jadeDisplay;

    // The following are needed for talking to the anonymous inner classes
    OverlayPainter _painter;
    BackgroundPainter _backgroundPainter;
    JadeDisplayTest.TiepointPainter _tiepoint_painter;
    JadeDisplayTest.MouseFollower _mouse_follower;

    static Color _colors[] = { Color.red, Color.green, Color.blue,
		Color.yellow, Color.magenta, Color.cyan, Color.white,
		Color.orange, Color.pink, Color.gray };
    static int _color_index = 0;

    public JadeDisplayTest(String argv[])
    {

	JFrame window1;

	System.out.println("Cache size:" +
		JAI.getDefaultInstance().getTileCache().getMemoryCapacity());
//	JAI.getDefaultInstance().getTileCache().setMemoryCapacity(100000000);
	System.out.println("Cache thresh:" +
		JAI.getDefaultInstance().getTileCache().getMemoryThreshold());

	System.out.println("default tile size: " +
		JAI.getDefaultTileSize().getWidth()+"," +
		JAI.getDefaultTileSize().getHeight());

	// Create input image by reading argv[0]

	_loadPB = new ParameterBlockJAI("fileload");
	_loadPB.setParameter("filename", argv[0]);
	_loadImage = JAI.create("fileload", _loadPB, new RenderingHints(JAI.KEY_TILE_CACHE,null));

	System.out.println("Loaded tile: " + _loadImage.getTileWidth() + "," +
		_loadImage.getTileHeight());



	window1 = new JFrame("main window");

/*
GraphicsConfiguration gconfig = window1.getGraphicsConfiguration();
BufferedImage compat = gconfig.createCompatibleImage(1,1);	// _loadImage.getTileWidth(), _loadImage.getTileHeight());
ImageLayout il = new ImageLayout(compat);
il.setHeight(_loadImage.getHeight());
il.setWidth(_loadImage.getWidth());
Dimension tsize = JAI.getDefaultTileSize();
il.setTileWidth((int)tsize.getWidth());
il.setTileHeight(100);
ParameterBlockJAI pbf = new ParameterBlockJAI("format");
pbf.setSource(_loadImage,0);
RenderingHints rh = new RenderingHints(null);	// (JAI.KEY_TILE_CACHE, null);
rh.put(JAI.KEY_IMAGE_LAYOUT,il);
RenderedImage formatImage = JAI.create("format", pbf, rh);
System.out.println("formatted to " + formatImage.getSampleModel());
System.out.println("s/b " + compat.getSampleModel());

System.out.println("Format tile: "+formatImage.getTileWidth()+","+formatImage.getTileHeight());
*/


	// Add Rotation operator

	_rotPB = new ParameterBlockJAI("rotate");
//	_rotPB.setSource(formatImage, 0);
	_rotPB.setSource(_loadImage, 0);
	_rotPB.setParameter("angle", 0.0f);
	_rotImage = JAI.create("rotate", _rotPB, new RenderingHints(JAI.KEY_TILE_CACHE,null));

//	System.out.println("Rot tile: "+_rotImage.getTileWidth()+","+_rotImage.getTileHeight());
	// Add Scale (zoom) operator

	_zoomPB = new ParameterBlockJAI("scale");
	_zoomPB.setSource(_rotImage, 0);
	_zoomImage = JAI.create("scale", _zoomPB);

	// Add Rescale (contrast enhancement) operator

//	System.out.println("Zoom tile: "+_zoomImage.getTileWidth()+","+_zoomImage.getTileHeight());
	_rescalePB = new ParameterBlockJAI("rescale");
	_rescalePB.setSource(_zoomImage, 0);
	_rescalePB.setParameter("constants", new double[] { 1.0 });
	_rescalePB.setParameter("offsets", new double[] { 0.0 });
	_rescaleImage = JAI.create("rescale", _rescalePB);

//	System.out.println("Rescale tile: "+_rescaleImage.getTileWidth()+","+_rescaleImage.getTileHeight());
	_finalImage = _rescaleImage;



//	System.out.println("Final tile: "+_finalImage.getTileWidth()+","+_finalImage.getTileHeight());



	// Display the image in a scrollbar

	JScrollPane sp = JadeDisplay.createDisplay(_finalImage, 300, 300, true);

        window1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	window1.getContentPane().add(sp);
	window1.pack();
	window1.setVisible(true);





	// Get the JadeDisplay for future manipulation...

	_jadeDisplay = (JadeDisplay)(sp.getViewport().getView());

	// Set up the controls

	JFrame f3 = new JFrame("controls");
        f3.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	Container w3 = f3.getContentPane();
	w3.setLayout(new BoxLayout(w3, BoxLayout.Y_AXIS));

	// Slider to change the "offset" value for the Rescale.  Note that
	// no explicit repaint is necessary (or desirable) - JAI events
	// automatically trigger a repaint.

	_offsetSlider = new JSlider(-100, 100, 0);
	_offsetSlider.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		int off = _offsetSlider.getValue();
		if (off != ((double[])(_rescalePB.getObjectParameter("offsets")))[0]) {
		    System.out.println("changing offset to " + off);
		    _rescalePB.setParameter("offsets", new double[] { off });
		    _rescaleImage.setParameterBlock(_rescalePB);
		}
	    }
	});

	w3.add(new JLabel("offset"));
	w3.add(_offsetSlider);


	// Slider to change the rotation value.  Note again how
	// no explicit repaint is necessary (or desirable) - JAI events
	// automatically trigger a repaint.

	_rotSlider = new JSlider(-180, 180, 0);
	_rotSlider.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		float angle = (float)_rotSlider.getValue() *
						(float)Math.PI / 180.0f;
		if (angle != ((Float)(_rotPB.getObjectParameter("angle"))).floatValue()) {
		    System.out.println("changing rotation to " + angle);
		    _rotPB.setParameter("angle", angle);
		    _rotImage.setParameterBlock(_rotPB);
		}
	    }
	});

	w3.add(new JLabel("rotate"));
	w3.add(_rotSlider);


	// Slider to change the zoom value.  Note that -n means factor of 1/n.

	_zoomSlider = new JSlider(-16, 16, 0);
	_zoomSlider.addChangeListener(new ChangeListener() {
	    public void stateChanged(ChangeEvent e) {
		float zoom = (float)_zoomSlider.getValue();
		if (zoom == 0)
		    zoom = 1;
		if (zoom < 0)
		    zoom = 1.0f / (-zoom);
		if (zoom != ((Float)(_zoomPB.getObjectParameter("xScale"))).floatValue()) {
		    System.out.println("changing zoom to " + zoom);
		    _zoomPB.setParameter("xScale", zoom);
		    _zoomPB.setParameter("yScale", zoom);
		    _zoomImage.setParameterBlock(_zoomPB);
		}
	    }
	});

	w3.add(new JLabel("zoom"));
	w3.add(_zoomSlider);

	// Add check box that will outline the tiles using the overlay
	// as they are being painted

	JCheckBox toggleButton = new JCheckBox("Repaint Overlay On");
	_painter = this;
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _jadeDisplay.removeOverlayPainter(_painter);
		else
		    _jadeDisplay.addOverlayPainter(_painter);
	    }
	});
	w3.add(toggleButton);

	// Add check box that will enable a software cursor (drawn using
	// the overlay) that follows the mouse

	toggleButton = new JCheckBox("Mouse Follower On");
	_mouse_follower = new JadeDisplayTest.MouseFollower(_jadeDisplay);
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _mouse_follower.off();
		else
		    _mouse_follower.on();
	    }
	});
	w3.add(toggleButton);

	// Add check box that will turn on or off "tiepoints".  See the
	// TiepointPainter inner class (below) for what this means.

	toggleButton = new JCheckBox("Tiepoint Overlay On");
	_tiepoint_painter = new JadeDisplayTest.TiepointPainter(_jadeDisplay);
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _tiepoint_painter.off();
		else
		    _tiepoint_painter.on();
	    }
	});
	w3.add(toggleButton);

	// Add a check box to set CACHE or DEFERRED mode, in order to
	// illustrate the difference.  You may want to switch DEFERRED to
	// IMMEDIATE for experimentation, as well.

	toggleButton = new JCheckBox("CACHE (off) or DEFERRED (on)");
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _jadeDisplay.setRepaintPolicy(JadeDisplay.REPAINT_CACHE);
		else
		    _jadeDisplay.setRepaintPolicy(JadeDisplay.REPAINT_DEFERRED);
	    }
	});
	w3.add(toggleButton);

	// Add a check box to turn on or off a special background painter.
	// It draws a (rather ugly) grid a la Photoshop.  Obviously you'd
	// want to change the colors before using it but these show up
	// nicely for testing.

	toggleButton = new JCheckBox("Background Painter");
	_backgroundPainter = this;
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _jadeDisplay.setBackgroundPainter(null);
		else
		    _jadeDisplay.setBackgroundPainter(_backgroundPainter);
	    }
	});
	w3.add(toggleButton);

	// Add a check button to toggle the RCE mode.  The difference is
	// obvious when you change the offset slider.  It does not affect
	// the rotation slider because rotation changes the image size,
	// which always causes a normal repaint.
	// Note: RCE_PARTIAL_NOERASE is not useful in this test program since
	// nothing will generate such an event!  (try a Mosaic)

	toggleButton = new JCheckBox(
				"RCE mode: REPAINT(off) or FULL_NOERASE(on)");
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
	if (e.getStateChange() == ItemEvent.DESELECTED)
	    _jadeDisplay.setRCEmode(JadeDisplay.RCE_REPAINT);
	else
	    _jadeDisplay.setRCEmode(JadeDisplay.RCE_FULL_NOERASE);
	    }
	});
	w3.add(toggleButton);

	// Add a check button to toggle setting the image to null.  This
        // should blank the display.

	toggleButton = new JCheckBox("SetImage(null)");
	toggleButton.addItemListener(new ItemListener() {
	    public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange() == ItemEvent.DESELECTED)
		    _jadeDisplay.setImage(_finalImage);
		else
		    _jadeDisplay.setImage(null);
	    }
	});

	w3.add(toggleButton);
	f3.pack();
	f3.setVisible(true);
    }

/***********************************************************************
 * Paint an overlay that outlines the area being drawn (with an X over
 * it).  This illustrates how the repaint system works.  The colors are
 * cycled to help show the order things are painted in.
 */

    public void paintOverlay(Graphics g)
    {
	Rectangle bounds = g.getClipBounds();
	int x = bounds.x;
	int y = bounds.y;

	int x2 = x + bounds.width - 1;
	int y2 = y + bounds.height - 1;

	g.setColor(_colors[_color_index]);
	if (++_color_index >= _colors.length)
	    _color_index = 0;

	g.drawRect(x, y, bounds.width-1, bounds.height-1);
	g.drawLine(x, y, x2, y2);
	g.drawLine(x, y2, x2, y);

    }

/***********************************************************************
 * Background painter...  Paints a checkerboard grid over the image.
 * I wouldn't inflict these colors on my worst enemy but they are quite
 * visible for debugging...
 * <p>
 * Note... it's noticeably faster to paint the whole background one color
 * then paint the alternate squares on top, than it is to alternate painting
 * different-color squares.  This may be due to time spent switching colors
 * all the time in the second approach, I'm not sure.
 */
    public void paintBackground(Graphics g, int x, int y, int width, int height,
				boolean isNoErase, boolean isInsideImage,
				boolean isDeferredTile)
    {
	if (isNoErase)                          // honor no-erase requests
	    return;
	if (isInsideImage) {                    // draw checkerboard
	    Color oldColor = g.getColor();
	    g.setColor(Color.RED);
	    g.fillRect(x, y, width, height);
	    if (isDeferredTile) // just to distinguish for test
		g.setColor(Color.GREEN);
	    else
		g.setColor(Color.BLUE);
	    int grid = 10;                      // size of grid
	    int startx = (x / grid) * grid;
	    if (x < 0)
		startx = ((x-grid+1)/grid)*grid;
	    int starty = (y / grid) * grid;
	    if (y < 0)
		starty = ((y-grid+1)/grid)*grid;
	    for (int yy = starty; yy < (y + height); yy += grid) {
		for (int xx = startx; xx < (x + width); xx += grid) {
		    if (((xx+yy) % (2*grid)) == 0) {
			g.fillRect(xx, yy, grid, grid);
		    }
		}
	    }
	    // We normally don't need to reset state.  However, because we
	    // just assume the background color for fillRect (below), we need
	    // to in this case, in case this g comes back to us again.
	    g.setColor(oldColor);
	}
	else                            // outside the image, just erase
	    g.fillRect(x, y, width, height);
    }

/***********************************************************************
 * Redispatch an event so it can propogate to a higher level in the
 * hierarchy (as well as being used at the lower level).  This allows
 * multiple listeners to both see the event (in this case, the mouse
 * follower and the mouse scroller).  Cribbed from an old JavaOne tutorial.
 */
    public void redispatch(MouseEvent e)
    {
	Point origin = e.getComponent().getLocation();
	e.translatePoint(origin.x, origin.y);
	e.getComponent().getParent().dispatchEvent(e);
	e.translatePoint(- origin.x, - origin.y);
    }

/***********************************************************************
 * Internal class to track the mouse and essentially display a cursor
 * (a big red +) using the overlay.
 */
    protected class MouseFollower implements MouseMotionListener, OverlayPainter
    {
	JadeDisplay _disp;
	int _x, _y;
	static final int SIZE = 50;

	public MouseFollower(JadeDisplay disp)
 	{
	    _disp = disp;
	    _x = 0;
	    _y = 0;
	}
	public void on()
	{
	    _disp.addMouseMotionListener(this);
	    _disp.addOverlayPainter(this);
	}
	public void off()
	{
	    _disp.removeMouseMotionListener(this);
	    _disp.removeOverlayPainterNRP(this);
	    Point origin = _disp.getCurrentImageOrigin();
	    _disp.paintNoErase(_x-SIZE-origin.x, _y-SIZE-origin.y,
				2*SIZE+1, 2*SIZE+1);
	}
	public void mouseMoved(MouseEvent e)
	{
	    Point old_scroll = _disp.getLocation();
	    redispatch(e);		// in case we're mouse-scrolling
	    Point new_scroll = _disp.getLocation();

	    Rectangle oldR=new Rectangle(_x-SIZE, _y-SIZE, 2*SIZE+1, 2*SIZE+1);

	    // Convert to image coordinates (they're in Viewport coords now)
	    Point origin = _disp.getCurrentImageOrigin();
	    _x = e.getX() + origin.x - (int)new_scroll.getX() +
							(int)old_scroll.getX();
	    _y = e.getY() + origin.y - (int)new_scroll.getY() +
							(int)old_scroll.getY();
	    Rectangle newR = new Rectangle(_x-SIZE, _y-SIZE, 2*SIZE+1, 2*SIZE+1);
	    newR.add(oldR);

	    // Convert back to Viewport coordinates for paintNoErase()
	    newR.translate(-origin.x, -origin.y);

	    _disp.paintNoErase(newR);
	}
	public void mouseDragged(MouseEvent e)
	{
	    redispatch(e);
	}
	public void paintOverlay(Graphics g)
	{
	    Rectangle bounds = g.getClipBounds();
	    if (bounds != null) {
		if (bounds.x > _x+SIZE)
		    return;
		if (bounds.y > _y+SIZE)
		    return;
		if (bounds.x + bounds.width <= _x-SIZE)
		    return;
		if (bounds.y + bounds.height <= _y-SIZE)
		    return;
	    }
	    g.setColor(Color.red);
	    g.drawLine(_x-SIZE, _y, _x+SIZE, _y);
	    g.drawLine(_x, _y-SIZE, _x, _y+SIZE);
	}
    }

/***********************************************************************
 * Internal class to paint and manage "tiepoints".  This is simply a list
 * of coordinates at which the user clicked the mouse.  We draw a little
 * circle at each coordinate to show where it is.  Just to make things
 * interesting (actually, to test the paintOneOverlay method), we color-cycle
 * any of them in the upper left corner using a timer.  Note:  READ THE
 * JAVADOCS if you intend to use paintOneOverlay.  It is dangerous!!
 * <p>
 * Hey, it's a demo...
 * <p>
 * Rotation is not taken into account (but the "tiepoints" do stay
 * put on the image as you scroll).  In order to take rotation into
 * account, make use of RenderedOp.mapDestPoint() (new in JAI 1.1.2).
 * Given the final image and the cursor position, use mapDestPoint()
 * to walk back up the chain to the source, transforming the coordinate
 * as you go.  That gives you the location of the tiepoint in the
 * file.  When displaying, start with the tiepoint location and
 * original image, and walk back down the chain using mapSourcePoint().
 * Once you reach the final image, your point will be transformed to
 * the proper position for display.  The result will be that the
 * tiepoint is "stuck" to the same position in the image, no matter
 * what geometric transforms are done.
 * <p>
 * This mapping is a new capability in JAI 1.1.2, and I just haven't
 * gotten around to implementing it yet...  But, the above should give
 * you an idea of how it should work, if you want to try....
 */
    protected class TiepointPainter implements MouseListener,
					MouseMotionListener, OverlayPainter
    {
	JadeDisplay _disp;
	ArrayList _tiepoints = new ArrayList(10);
	static final int SIZE = 5;
	int _colorIndex = 0;
	javax.swing.Timer _timer;
	Color _nonCycleColor;
	Rectangle _cycleArea;
	OverlayPainter _this;		// for anonymous inner class

	public TiepointPainter(JadeDisplay disp)
 	{
	    _disp = disp;
	    _nonCycleColor = Color.green;
	    _this = this;
	}
	public void on()
	{
	    _disp.addMouseListener(this);
	    _disp.addMouseMotionListener(this);	// just for redispatch()
	    _disp.addOverlayPainter(this);

	    ActionListener colorCycler = new ActionListener() {
		public void actionPerformed(ActionEvent evt) {
		    if (++_colorIndex  >= _colors.length)
			_colorIndex = 0;
		    _cycleArea = new Rectangle(20,20,100,100);
		    _disp.paintOneOverlay(_this, _cycleArea);
		}
	    };
	    _timer = new javax.swing.Timer(500, colorCycler);
	    _timer.start();
	}
	public void off()
	{
	    _timer.stop();
	    _disp.removeMouseListener(this);
	    _disp.removeMouseMotionListener(this);
	    _disp.removeOverlayPainterNRP(this);
	    Point origin = _disp.getCurrentImageOrigin();
	    _disp.repaint();		// could be smarter...
	}
	public void mousePressed(MouseEvent e)
	{
	    // Convert to image coordinates (they're in Viewport coords now)
	    Point origin = _disp.getCurrentImageOrigin();
	    int x = e.getX() + origin.x;
	    int y = e.getY() + origin.y;

	    _tiepoints.add(new Point(x,y));

	    Rectangle newR = new Rectangle(x-SIZE, y-SIZE, 2*SIZE+1, 2*SIZE+1);
	    // Convert back to Viewport coordinates for paintNoErase()
	    newR.translate(-origin.x, -origin.y);
	    _disp.paintNoErase(newR);
	    redispatch(e);
	}
	public void mouseReleased(MouseEvent e)
	{
	    redispatch(e);
	}
	public void mouseClicked(MouseEvent e)
	{
	    redispatch(e);
	}
	public void mouseEntered(MouseEvent e)
	{
	    redispatch(e);
	}
	public void mouseExited(MouseEvent e)
	{
	    redispatch(e);
	}

	public void mouseDragged(MouseEvent e)
	{
	    redispatch(e);
	}

	public void mouseMoved(MouseEvent e)
	{
	    redispatch(e);
	}

	public void paintOverlay(Graphics g)
	{
	    Iterator it = _tiepoints.iterator();
	    g.setColor(_colors[_colorIndex]);
	    while (it.hasNext()) {
		Point p = (Point)it.next();
		if (_cycleArea.contains(p))
		    g.setColor(_colors[_colorIndex]);
		else
		    g.setColor(_nonCycleColor);
		g.drawOval(p.x-SIZE, p.y-SIZE, 2*SIZE+1, 2*SIZE+1);
	    }
	}
    }

}

