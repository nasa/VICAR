package jpl.mipl.io.vicar.test;

import jpl.mipl.io.vicar.*;
import java.io.*;
import java.awt.image.*;
import java.awt.Point;

/**
 * Test program for threaded reads.  Spawns off a bunch of threads and makes
 * sure the right data is read from each.  The input file should be as output
 * from the VICAR command:  "$R2LIB/gen a nl ns nb"
 */

public class TestThreadRead extends Thread
{
    protected VicarInput _file;
    protected int _nl, _ns, _nb, _tileHeight, _tileWidth;
    protected int _passes, _threadNum;

    TestThreadRead(VicarInput file, int nl, int ns, int nb,
		int tileHeight, int tileWidth, int passes, int threadNum)
    {
	_file = file;
	_nl = nl;
	_ns = ns;
	_nb = nb;
	_tileHeight = tileHeight;
	_tileWidth = tileWidth;
	_passes = passes;
	_threadNum = threadNum;
    }

    public void run() {
	boolean valid;

	int ntile_x = (_ns-1) / _tileWidth + 1;
	int ntile_y = (_nl-1) / _tileHeight + 1;

	for (int pass=0; pass < _passes; pass++) {

	    for (int y_tile = 0; y_tile < ntile_y; y_tile++) {
		for (int x_tile = 0; x_tile < ntile_x; x_tile++) {

		    Raster r = createTile(x_tile, y_tile, _file);
		    try {
			_file.readTile(r.getMinX(), r.getMinY(),
				r.getSampleModel(), r.getDataBuffer());
		    } catch (Exception e) {
			e.printStackTrace();
			System.exit(0);
		    }
		    valid = checkTile(r, x_tile, y_tile);
		    if (!valid) {
			throw new RuntimeException("***ERROR*** reading tile in thread "+_threadNum+" at location y_tile="+y_tile+", x_tile="+x_tile);
		    }
		}
	    }
	    System.out.print(" "+_threadNum+"."+pass);
	}
	System.out.println("");
	System.out.println("Thread "+_threadNum+" completed successfully");
    }


/***********************************************************************
 * Create a tile given tile indices.
 */
    public Raster createTile(int x_tile, int y_tile, VicarInput vi)
    {
	SampleModel sm = vi.createSampleModel(_tileWidth, _tileHeight);
	DataBuffer db = sm.createDataBuffer();

	int x = x_tile * _tileWidth;
	int y = y_tile * _tileHeight;

	return Raster.createRaster(sm, db, new Point(x, y));
    }

/***********************************************************************
 * Check a tile for the correct data.
 */
    public boolean checkTile(Raster r, int x_tile, int y_tile)
    {
	SampleModel sm = r.getSampleModel();
	DataBuffer db = r.getDataBuffer();

	int x = x_tile * _tileWidth;
	int y = y_tile * _tileHeight;

	int maxx = _tileWidth;
	if ((x + _tileWidth) > _ns)
	    maxx = _ns - x;
	int maxy = _tileHeight;
	if ((y + _tileHeight) > _nl)
	    maxy = _nl - y;

	for (int line=0; line < maxy; line++) {
	    for (int samp=0; samp < maxx; samp++) {
		for (int band=0; band < _nb; band++) {
		    int val = (band + (line+y) + (samp+x)) & 0xFF;
		    if (sm.getSample(samp, line, band, db) != val)
			return false;
		}
	    }
	}
	return true;
    }

/***********************************************************************
 * Main parses args and spawns all the threads.
 */

    public static void main(String argv[])
    {
	if (argv.length != 8) {
	    System.out.println("Usage:");
	    System.out.println("java TestThreadRead infile NL NS NB tileHeight tileWidth passes threads");
	    System.out.println("  where infile is from 'gen infile nl ns nb'");
	    System.out.println("  NL, NS, NB are obvious");
	    System.out.println("  tileHeight and tileWidth are the tile sizes to read");
	    System.out.println("  passes is the number of passes through the file for each thread");
	    System.out.println("  and threads is the number of threads to spawn");
	    System.out.println("No checking is done of arguments (it's a test program); bad values will crash.");

	    System.exit(0);
	}

	System.out.println("Note:  Threads will complete in a random order.");
	System.out.println("As long as no errors are reported, the test is successful.");

	// Parse arguments

	String infile;
	int nl=0, ns=0, nb=0, tileHeight=0, tileWidth=0, passes=0, threads=0;

	infile = argv[0];
	try {
	    nl = Integer.parseInt(argv[1]);
	    ns = Integer.parseInt(argv[2]);
	    nb = Integer.parseInt(argv[3]);
	    tileHeight = Integer.parseInt(argv[4]);
	    tileWidth = Integer.parseInt(argv[5]);
	    passes = Integer.parseInt(argv[6]);
	    threads = Integer.parseInt(argv[7]);
	}
	catch (Exception e) {
	    e.printStackTrace();
	    System.exit(0);
	}

	VicarInput vif = new VicarInputImage();

	try {
	    vif.open(argv[0]);
	} catch (IOException e) {
	    e.printStackTrace();
	    System.exit(0);
	}

	for (int i=0; i < threads; i++) {
	    System.out.println("creating thread " + i);
	    Thread th = new TestThreadRead(vif, nl, ns, nb,
				tileHeight, tileWidth, passes, i);
	    th.start();
	}
    }

}

