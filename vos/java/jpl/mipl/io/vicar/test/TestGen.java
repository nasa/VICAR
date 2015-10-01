package jpl.mipl.io.vicar.test;

import jpl.mipl.io.vicar.*;
import java.io.*;
import java.awt.image.*;
import java.awt.Point;

/**
 * Test program for VICAR I/O writes.  Simulates much of the functionality of
 * the VICAR program "gen".  No attempt is made to do "nice" argument parsing.
 */

public class TestGen
{
    String _outfile;
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
    String _infile = null;

/***********************************************************************
 * Main program.  Run with no args for usage comment.
 */
    public static void main(String argv[])
    {
	// Catch all exceptions here so we don't need to worry about them

	try {
	    new TestGen(argv);
	}
	catch (Exception e) {
	    e.printStackTrace();
	}
	System.exit(0);
    }

/***********************************************************************
 */
    public TestGen(String argv[]) throws Exception
    {
	if (argv.length != 12 && argv.length != 13) {
	    System.out.println("Usage:");
//						0       1  2  3     4      5     6    7    8    9      10         11        12          13
	    System.out.println("java TestGen outfile(s) NL NS NB datatype org method LINC SINC BINC tileHeight tileWidth pixelStride [infile]");
	    System.out.println("  where NL, NS, NB, LINC, SINC, BINC are as in gen");
	    System.out.println("  outfile(s) is a single string with 1 or more comma-sep filenames (a la xvd)");
	    System.out.println("  datatype is byte, half, full, real, or double (complex not supported)");
	    System.out.println("  org is bsq, bil, or bip");
	    System.out.println("  method is line or tile to indicate the writing method");
	    System.out.println("  tileHeight and tileWidth are always required but used only for method=tile");
	    System.out.println("  pixelStride tests that aspect of line I/O (ignored for tiles");
	    System.out.println("  and infile is an optional input file; used only to test copying label from");
	    System.out.println("    primary input");
	    System.out.println("No checking is done of arguments (it's a test program); bad values will crash.");

	    System.exit(0);
	}

	// Parse arguments

	_outfile = argv[0];
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
	if (argv.length == 14)
	    _infile = argv[13];

	// Set up and create the output file

	VicarOutputFile voif = new VicarOutputFile();

	if (_infile != null) {			// set primary input
	    voif.setPrimaryInput(new VicarInputImage(_infile));
	}

	SystemLabel sys = voif.getSystemLabel();
	sys.setOrg(_org);
	sys.setNL(_nl);
	sys.setNS(_ns);
	sys.setNB(_nb);
	sys.setFormat(_datatype);
	_datatype_code = sys.getFormatCode();

	voif.setSystemLabel(sys);

	voif.open(_outfile);

	// Update the label, stuff it with things like GEN does

	VicarLabel lbl = voif.getVicarLabel();
	VicarLabelSet task = lbl.createHistoryTask("TestGen");
	task.add(new VicarLabelItem("IVAL", 0.0f));
	task.add(new VicarLabelItem("SINC", _sinc));
	task.add(new VicarLabelItem("LINC", _linc));
	task.add(new VicarLabelItem("BINC", _binc));
	task.add(new VicarLabelItem("MODULO", 0.0f));

	voif.setVicarLabel(lbl);

	if (_method.equalsIgnoreCase("line")) {
	    if (_org.equalsIgnoreCase("bip"))
		testLine(voif, _nl, _ns, _nb, _linc, _sinc, _binc,_pixelStride);
	    else if (_org.equalsIgnoreCase("bil"))
		testLine(voif, _nl, _nb, _ns, _linc, _binc, _sinc,_pixelStride);
	    else		// bsq
		testLine(voif, _nb, _nl, _ns, _binc, _linc, _sinc,_pixelStride);
	}
	else {					// tile
	    testTile(voif);
	}

	voif.close();
    }

/***********************************************************************
 * Test via record (line) oriented writes.  The array is allocated
 * n1*pixelStride and every stride'th pixel is set.  The write should only
 * pick up those array elements.
 */
    public void testLine(VicarOutput vo, int n3, int n2, int n1,
			double inc3, double inc2, double inc1, int pixelStride)
			throws IOException
    {
	switch (_datatype_code) {
	    case SystemLabel.TYPE_BYTE:

		byte[] bbuf = new byte[n1*pixelStride];
		for (int i3=0; i3 < n3; i3++) {
		    for (int i2=0; i2 < n2; i2++) {
			for (int i1=0; i1 < n1; i1++) {
			    bbuf[i1*pixelStride] =
				(byte)((i3*inc3) + (i2*inc2) +(i1*inc1));
			}
			vo.writeRecord(bbuf, 0, n1, 0, pixelStride, i2, i3);
		    }
		}
		break;

	    case SystemLabel.TYPE_HALF:

		short[] sbuf = new short[n1*pixelStride];
		for (int i3=0; i3 < n3; i3++) {
		    for (int i2=0; i2 < n2; i2++) {
			for (int i1=0; i1 < n1; i1++) {
			    sbuf[i1*pixelStride] =
				(short)((i3*inc3) + (i2*inc2)+(i1*inc1));
			}
			vo.writeRecord(sbuf, 0, n1, 0, pixelStride, i2, i3);
		    }
		}
		break;

	    case SystemLabel.TYPE_FULL:

		int[] ibuf = new int[n1*pixelStride];
		for (int i3=0; i3 < n3; i3++) {
		    for (int i2=0; i2 < n2; i2++) {
			for (int i1=0; i1 < n1; i1++) {
			    ibuf[i1*pixelStride] =
				(int)((i3*inc3) + (i2*inc2) + (i1*inc1));
			}
			vo.writeRecord(ibuf, 0, n1, 0, pixelStride, i2, i3);
		    }
		}
		break;

	    case SystemLabel.TYPE_REAL:

		float[] fbuf = new float[n1*pixelStride];
		for (int i3=0; i3 < n3; i3++) {
		    for (int i2=0; i2 < n2; i2++) {
			for (int i1=0; i1 < n1; i1++) {
			    fbuf[i1*pixelStride] =
				(float)((i3*inc3) + (i2*inc2)+(i1*inc1));
			}
			vo.writeRecord(fbuf, 0, n1, 0, pixelStride, i2, i3);
		    }
		}
		break;

	    case SystemLabel.TYPE_DOUB:

		double[] dbuf = new double[n1*pixelStride];
		for (int i3=0; i3 < n3; i3++) {
		    for (int i2=0; i2 < n2; i2++) {
			for (int i1=0; i1 < n1; i1++) {
			    dbuf[i1*pixelStride] =
				(double)((i3*inc3) +(i2*inc2)+(i1*inc1));
			}
			vo.writeRecord(dbuf, 0, n1, 0, pixelStride, i2, i3);
		    }
		}
		break;

	} 
    }

/***********************************************************************
 * Test tile-oriented writes.  Note that ORG doesn't matter here; it's
 * de-scrambled by the vicar package itself.
 */

    public void testTile(VicarOutput vo) throws IOException
    {
	// Write tiles top-to-bottom first in order to test random access

	int ntile_x = (_ns-1)/_tileWidth + 1;
	int ntile_y = (_nl-1)/_tileHeight + 1;

	for (int y_tile = 0; y_tile < ntile_y; y_tile++) {
	    for (int x_tile = 0; x_tile < ntile_x; x_tile++) {
		Raster r = createTile(x_tile, y_tile, vo);
		vo.writeTile(r.getMinX(), r.getMinY(), r.getSampleModel(),
			r.getDataBuffer());
	    }
	}
    }

/***********************************************************************
 * Create a tile given tile indices.
 */
    public Raster createTile(int x_tile, int y_tile, VicarOutput vo)
    {
	SampleModel sm = vo.createSampleModel(_tileWidth, _tileHeight);
	DataBuffer db = sm.createDataBuffer();

	int x = x_tile * _tileWidth;
	int y = y_tile * _tileHeight;

	for (int line=0; line<_tileHeight; line++) {
	    for (int samp=0; samp<_tileWidth; samp++) {
		for (int band=0; band<_nb; band++) {
		    sm.setSample(samp, line, band,
			((band*_binc) + (line+y)*_linc + (samp+x)*_sinc), db);
		}
	    }
	}
	return Raster.createRaster(sm, db, new Point(x, y));
    }

}

