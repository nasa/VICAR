import javax.swing.*;
import java.awt.*;
import jpl.mipl.jade.util.*;

/** <em>THIS IS NOT A STANDALONE TEST PROGRAM!!!!</em>
 *  <p>
 *  It must be used with another program driving it.  Currently, that
 *  is JadeDisplay itself.  Code using this test class is commented out.
 *  If that code is enabled, this class will show the current region
 *  graphically.  This is intended more for debugging than for testing.
 */

public class RectRegionTest extends JPanel
{
    RectRegion _rgn;

    public RectRegionTest(RectRegion rgn)
    {
	super();
	_rgn = rgn;
	repaint();
    }

    public void update()
    {
	paintImmediately(new Rectangle(0, 0, getWidth(), getHeight()));
//	repaint();
    }

    public void paintComponent(Graphics gg)
    {
	Graphics g = gg.create();
//	Graphics g = getGraphics().create();
	Color c = g.getColor();
	g.setClip(0,0,getWidth(),getHeight());
	g.clearRect(0,0,getWidth(), getHeight());
	g.setColor(Color.blue);
	Rectangle[] r = _rgn.getRectangles();
	for (int i=0; i < r.length; i++) {
	    // The -1 compensates for the weird way Graphics draws things
	    g.drawRect(r[i].x, r[i].y, r[i].width-1, r[i].height-1);
	}
	g.setColor(c);
	g.dispose();
    }
}

