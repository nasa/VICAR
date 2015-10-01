package jpl.mipl.io.vicar;

import java.io.*;
import java.awt.image.*;

/**
 * This interface includes everything that's common between the VicarInput
 * and VicarOutput interfaces, such as file opening and label access.  Since
 * some classes are likely to implement both interfaces, it is cleaner to put
 * the common things here rather than duplicate them in each interface.
 * <p>
 * <p>
 * @see VicarInput
 * @see VicarOutput
 */

public interface VicarIOBase
{

    /** The default tile size for a VICAR file.  Actual default is the
     *  minimum of this and the file size... except sequential or random-hard
     *  files default to tile_width == the width of the file. */
    public static final int VICAR_TILE_HEIGHT = 100;
    public static final int VICAR_TILE_WIDTH = 1024;

/***********************************************************************
 * Opens a file given a filename.  Classes implementing this interface are
 * likely to have many more open() calls with e.g. streams, files, etc.
 * Implementing classes that manage multiple physical files should accept
 * a list of files here with comma or space separators.
 * @throws IOException
 */
    public void open(String fn) throws IOException;

/***********************************************************************
 * Closes the file.  Releases all resources, so nothing is valid any more
 * (labels, streams, etc.).  For output files, this also flushes any
 * unwritten data and labels.
 */
    public void close() throws IOException;

/***********************************************************************
 * Retrieves a deep copy of the <code>VicarLabel</code> object associated
 * with this file.  In most cases, the VicarLabel object must be created
 * from the header of the file.  This routine should <em>not</em> be used
 * to retrieve the system label bean; use <code>getSystemLabel()</code>
 * instead.
 * <p>
 * Note that requesting the label via this routine will cause the EOL labels
 * to be read if needed.  On a sequential-only stream, this could get you
 * into trouble and thus this should be done only after the image data has
 * been read.  If random access is possible but not easy, this can be done
 * before reading the data but you could take a significant performance hit.
 * If it is easy, then this can be called any time.
 * <p>
 * For existing images, e.g. a file opened for read or update, this routine
 * returns the label of that image.  For images implementing
 * <code>VicarOutput</code>, however, the situation is a little more complex.
 * This routine will return the first item from the following that is set:
 * <ul>
 * <li>Label set via <code>setVicarLabel</code>
 * <li>Label from file itself if the file already exists
 * <li>Primary input's label
 * <li>Empty (newly constructed) label
 * <p>
 * @see #getSystemLabel()
 * @throws IOException if an EOL read is required, and fails.  Will not occur
 * for random-easy streams.
 */
    public VicarLabel getVicarLabel() throws IOException;

/***********************************************************************
 * Indicates whether or not the <code>VicarLabel</code> has been completely
 * read.  This can be used with sequential or random-hard streams to determine
 * whether or not <code>getVicarLabel()</code> will do bad things.  The label
 * will always be completely read for random-easy streams, or for any stream
 * if there are no EOL labels.
 * @see #getVicarLabel()
 */
    public boolean isLabelComplete();

/***********************************************************************
 * Retrieves a deep copy of the <code>SystemLabel</code> object associated
 * with this image.  This should not be confused with
 * <code>VicarLabel.getSystem()</code>, which returns a
 * <code>VicarLabelSet</code> rather than a <code>SystemLabel</code> object.
 * The <code>SystemLabel</code> object returned by this function need not
 * contain the same information as the System part of the
 * <code>VicarLabel</code> object (e.g. a synthetic image made up of multiple
 * files).
 * <p>
 * For existing images, e.g. a file opened for read or update, this routine
 * returns the label of that image.  For images implementing
 * <code>VicarOutput</code>, however, the situation is a little more complex.
 * This routine will return the first item from the following that is set:
 * <ul>
 * <li>Label set via <code>setSystemLabel</code>
 * <li>Label from file itself if the file already exists
 * <li>Primary input's label
 * <li>Default (newly constructed) label
 * </ul>
 */
    public SystemLabel getSystemLabel();

/***********************************************************************
 * Returns true if random access to this file is <em>possible</em>.  It
 * might be expensive, however.
 * @see #isRandomAccessEasy()
 */
    public boolean isRandomAccessAllowed();

/***********************************************************************
 * Returns true if random access to this file is <em>easy</em>, meaning
 * that the underlying stream/file supports seekable random access, as
 * opposed to drastic means like mark/reset, contents caching, or re-opening.
 */
    public boolean isRandomAccessEasy();

////////////////////////////////////////////////////////////////////////

/***********************************************************************
 * Creates a SampleModel that is most compatible with the image.
 * The algorithm is subject to change, so you must not depend on it, but
 * currently it is:
 * <ul>
 * <li>If the file is double, float, or complex, create ComponentSampleModelJAI.
 * (complex creates twice as many bands)
 * <li>Else, if the organization is BIP, create PixelInterleavedSampleModel.
 * <li>Else, if the organization is BIL, create ComponentSampleModel.
 * <li>Else, create BandedSampleModel (BSQ).
 * </ul>
 * The size of the sample model is the default VICAR tile size, which is
 * currently 100 lines by 2048 samples (or smaller, if the file size is
 * smaller).  If the file is sequential-only or random-hard, the default tile
 * width is set to the image width so we don't have to back up.
 * <p>
 * This routine is mainly intended for input images; on output you usually
 * already have a SampleModel.  However, if you don't care what SM you use,
 * this routine will return one that fits the output file.
 */
    public SampleModel createSampleModel();

/***********************************************************************
 * Creates a SampleModel that is most compatible with the image.
 * The size of the sample model is specified by the parameters.  If a
 * parameter is specified as 0, the size of the file is used.  Note that
 * the size of the SM is <em>not</em> limited by the size of the file;
 * you can create an SM as big as you want with this method.
 * <p>
 * Follow the See Also link for algorithm details.
 * @see #createSampleModel()
 */
    public SampleModel createSampleModel(int tileWidth, int tileHeight);

}

