package jpl.mipl.io.vicar.test;

import jpl.mipl.io.vicar.*;
import java.io.*;
import java.awt.image.*;
import java.awt.Point;

/**
 * Test program for VICAR I/O reads.  Reads a file and checks that the contents
 * match the pattern from the VICAR program "gen".  No attempt is made to do
 * "nice" argument parsing.
 */

public class TestGenRead
{
    String _infile;
    int _nl;
    int _ns;
    int _nb;
    String _datatype;
    int _datatype_code;
    String _org;
    String _method;
    double _linc;
    double _sinc;
    double _binc;
    int _tileHeight;
    int _tileWidth;
    int _pixelStride;

/***********************************************************************
 * Main program.  Run with no args for usage comment.
 */
    public static void main(String argv[])
    {
	// Catch all exceptions here so we don't need to worry about them

	try {
	    new TestGenRead(argv);
	}
	catch (Exception e) {
	    e.printStackTrace();
	}
	System.exit(0);
    }

/***********************************************************************
 */
    public TestGenRead(String argv[]) throws Exception
    {
	if (argv.length != 13) {
	    System.out.println("Usage:");
//						    0       1  2  3     4      5     6    7    8    9      10         11        12
	    System.out.println("java TestGenRead infile(s) NL NS NB datatype org method LINC SINC BINC tileHeight tileWidth pixelStride");
	    System.out.println("  where NL, NS, NB, LINC, SINC, BINC are as in gen");
	    System.out.println("  infile(s) is a single string with 1 or more comma-sep filenames (a la xvd)");
	    System.out.println("  datatype is byte, half, full, real, or double (complex not supported)");
	    System.out.println("  org is bsq, bil, or bip");
	    System.out.println("  method is line or tile to indicate the reading method");
	    System.out.println("  and tileHeight, tileWidth are always required but used only for method=tile");
	    System.out.println("  pixelStride tests that aspect of reading (used only for line");
	    System.out.println("No checking is done of arguments (it's a test program); bad values will crash.");

	    System.exit(0);
	}

	// Parse arguments

	_infile = argv[0];
	_nl = Integer.parseInt(argv[1]);
	_ns = Integer.parseInt(argv[2]);
	_nb = Integer.parseInt(argv[3]);
	_datatype = argv[4];
	_org = argv[5];
	_method = argv[6];
	_linc = Double.parseDouble(argv[7]);
	_sinc = Double.parseDouble(argv[8]);
	_binc = Double.parseDouble(argv[9]);
	_tileHeight = Integer.parseInt(argv[10]);
	_tileWidth = Integer.parseInt(argv[11]);
	_pixelStride = Integer.parseInt(argv[12]);

	// Create the input file object

	VicarInputFile vif = new VicarInputFile();

	vif.open(_infile);

	// These checks aren't really because the IO system requires it;
	// they're more to test that the IO system is getting it right!

	SystemLabel sys = vif.getSystemLabel();
	if (!_org.equalsIgnoreCase(sys.getOrg()))
	    throw new IllegalArgumentException("***Org doesn't match file"
+_org+" "+sys.getOrg());
	if (_nl != sys.getNL())
	    throw new IllegalArgumentException("***NL doesn't match file");
	if (_ns != sys.getNS())
	    throw new IllegalArgumentException("***NS doesn't match file");
	if (_nb != sys.getNB())
	    throw new IllegalArgumentException("***NB doesn't match file");
	if (!_datatype.equalsIgnoreCase(sys.getFormat()))
	    throw new IllegalArgumentException("***Format doesn't match file");
	_datatype_code = sys.getFormatCode();

	// Check for items from the GEN label, just to test label reads.
	// We continue on error from these.

	VicarLabel lbl = vif.getVicarLabel();
	VicarLabelSet task = lbl.getHistory().getSet(-1);
	if (task.getItem("IVAL").getFloat() != 0.0f)
	    System.out.println("***IVAL not found or bad in label");
	if (task.getItem("SINC").getFloat() != _sinc)
	    System.out.println("***SINC not found in label, or doesn't match parameter");
	if (task.getItem("LINC").getFloat() != _linc)
	    System.out.println("***LINC not found in label, or doesn't match parameter");
	if (task.getItem("BINC").getFloat() != _binc)
	    System.out.println("***BINC not found in label, or doesn't match parameter");
	if (task.getItem("MODULO").getFloat() != 0.0f)
	    System.out.println("***MODULO not found or bad in label");

	if (_method.equalsIgnoreCase("line")) {
	    if (_org.equalsIgnoreCase("bip"))
		testLine(vif, _nl, _ns, _nb, _linc, _sinc, _binc, _pixelStride);
	    else if (_org.equalsIgnoreCase("bil"))
		testLine(vif, _nl, _nb, _ns, _linc, _binc, _sinc, _pixelStride);
	    else		// bsq
		testLine(vif, _nb, _nl, _ns, _binc, _linc, _sinc, _pixelStride);
	}
	else {					// tile
	    testTile(vif);
	}

	vif.close();
    }

/***********************************************************************
 * Test via record (line) oriented reads
 */
    public void testLine(VicarInput vi, int n3, int n2, int n1,
			double inc3, double inc2, double inc1, int pixelStride)
			throws IOException
    {
	boolean valid = true;
	int i1=0, i2=0, i3=0;

	switch (_datatype_code) {
	    case SystemLabel.TYPE_BYTE:

		byte[] bbuf = new byte[n1*pixelStride];
		outb:
		for (i3=0; i3 < n3; i3++) {
		    for (i2=0; i2 < n2; i2++) {
			vi.readRecord(bbuf, 0, n1, 0, pixelStride, i2, i3);
			for (i1=0; i1 < n1; i1++) {
			    if (bbuf[i1*pixelStride] != (byte)((i3*inc3) +
							(i2*inc2) +(i1*inc1))) {
				valid = false;
				break outb;
			    }
			    for (int i4=1; i4<pixelStride; i4++) {
				if (bbuf[i1*pixelStride+i4] != 0) {
				    valid = false;
				    break outb;
				}
			    }
			}
		    }
		}
		break;

	    case SystemLabel.TYPE_HALF:

		short[] sbuf = new short[n1*pixelStride];
		outs:
		for (i3=0; i3 < n3; i3++) {
		    for (i2=0; i2 < n2; i2++) {
			vi.readRecord(sbuf, 0, n1, 0, pixelStride, i2, i3);
			for (i1=0; i1 < n1; i1++) {
			    if (sbuf[i1*pixelStride] != (short)((i3*inc3) +
							(i2*inc2)+(i1*inc1))) {
				valid = false;
				break outs;
			    }
			    for (int i4=1; i4<pixelStride; i4++) {
				if (sbuf[i1*pixelStride+i4] != 0) {
				    valid = false;
				    break outs;
				}
			    }
			}
		    }
		}
		break;

	    case SystemLabel.TYPE_FULL:

		int[] ibuf = new int[n1*pixelStride];
		outf:
		for (i3=0; i3 < n3; i3++) {
		    for (i2=0; i2 < n2; i2++) {
			vi.readRecord(ibuf, 0, n1, 0, pixelStride, i2, i3);
			for (i1=0; i1 < n1; i1++) {
			    if (ibuf[i1*pixelStride] != (int)((i3*inc3) +
						(i2*inc2) + (i1*inc1))) {
				valid = false;
				break outf;
			    }
			    for (int i4=1; i4<pixelStride; i4++) {
				if (ibuf[i1*pixelStride+i4] != 0) {
				    valid = false;
				    break outf;
				}
			    }
			}
		    }
		}
		break;

	    case SystemLabel.TYPE_REAL:

		float[] fbuf = new float[n1*pixelStride];
		outr:
		for (i3=0; i3 < n3; i3++) {
		    for (i2=0; i2 < n2; i2++) {
			vi.readRecord(fbuf, 0, n1, 0, pixelStride, i2, i3);
			for (i1=0; i1 < n1; i1++) {
			    if (fbuf[i1*pixelStride] != (float)((i3*inc3) +
							(i2*inc2)+(i1*inc1))) {
				valid = false;	
				break outr;
			    }
			    for (int i4=1; i4<pixelStride; i4++) {
				if (fbuf[i1*pixelStride+i4] != 0) {
				    valid = false;
				    break outr;
				}
			    }
			}
		    }
		}
		break;

	    case SystemLabel.TYPE_DOUB:

		double[] dbuf = new double[n1*pixelStride];
		outd:
		for (i3=0; i3 < n3; i3++) {
		    for (i2=0; i2 < n2; i2++) {
			vi.readRecord(dbuf, 0, n1, 0, pixelStride, i2, i3);
			for (i1=0; i1 < n1; i1++) {
			    if (dbuf[i1] != (double)((i3*inc3) +(i2*inc2)+(i1*inc1))) {
				valid = false;
				break outd;
			    }
			    for (int i4=1; i4<pixelStride; i4++) {
				if (dbuf[i1*pixelStride+i4] != 0) {
				    valid = false;
				    break outd;
				}
			    }
			}
		    }
		}
		break;

	} 

	if (!valid) {
	    throw new IOException("***ERROR*** reading file at location i3="+i3+", i2="+i2+", i1="+i1);
	}
    }

/***********************************************************************
 * Test tile-oriented reads.  Note that ORG doesn't matter here; it's
 * de-scrambled by the vicar package itself.
 */

    public void testTile(VicarInput vi) throws IOException
    {
	boolean valid = true;
	int y_tile=0, x_tile=0;

	// Read tiles top-to-bottom first in order to test random access

	int ntile_x = (_ns-1)/_tileWidth + 1;
	int ntile_y = (_nl-1)/_tileHeight + 1;

	out:
	for (y_tile = 0; y_tile < ntile_y; y_tile++) {
	    for (x_tile = 0; x_tile < ntile_x; x_tile++) {
		Raster r = createTile(x_tile, y_tile, vi);
		vi.readTile(r.getMinX(), r.getMinY(),r.getSampleModel(),
			r.getDataBuffer());
		valid = checkTile(r, x_tile, y_tile);
		if (!valid)
		    break out;
	    }
	}
	if (!valid) {
	    throw new IOException("***ERROR*** reading file at location y_tile="+y_tile+", x_tile="+x_tile);
	}
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
		    double val = (band*_binc) + (line+y)*_linc + (samp+x)*_sinc;
		    if (_datatype_code == SystemLabel.TYPE_BYTE)
			val = (double)((int)val & 0xFF);
		    else if (_datatype_code == SystemLabel.TYPE_HALF)
			val = (double)((short)val);
		    else if (_datatype_code == SystemLabel.TYPE_FULL)
			val = (double)((int)val);
		    if (sm.getSampleDouble(samp, line, band, db) != val)
			return false;
		}
	    }
	}
	return true;
    }

}

