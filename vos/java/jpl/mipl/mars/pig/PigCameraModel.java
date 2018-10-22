package jpl.mipl.mars.pig;

/***********************************************************************
 * Interface for camera models.  Responsible for maintaining calibration
 * information for a camera and doing the actual ray projection calculations.
 * <p>
 * Calibration parameters might consist of initial CAHVOR parameters for that
 * model, or things like focal length, boresight offset, and radial distortion
 * for an orbital model.
 * <p>
 * Modeled after the C version of PigCameraModel.  Not implemented
 * currently: coord system support
 * <p>
 * Unlike the C version, we make a distinction here between a simple camera
 * model (defined here) and a pointable camera model.  The pointable model
 * contains the additional complexity of moveCamera, scale, shift, align, etc.
 * This interface simply defines how the projection math works.
 * <p>
 * Note that all image-space coordinates are 0-based, unlike VICAR files.
 * <p>
 * Note that conversion from file to camera model coords is not supported in
 * this version, because it is no longer necessary.  MER, PHX, and MSL take
 * care of subframes by modifying the camera model itself.  Older missions did
 * not, and thus had to add offsets to file coords to get camera model coords
 * if the file was a subframe.  If you do work with an older mission (MPF?)
 * which does not modify the camera models for subframes, make sure to do this
 * conversion yourself.
 * <p>
 * @author Bob Deen, JPL
 */

public interface PigCameraModel
{
    /**
     * Retrieves the image-space coordinates of the "pointing axis" of the camera.
     * This is the location on the CCD containing the pixel the camera is defined
     * to be "looking at" and is generally near the center of the CCD.  This can
     * be used to relate "logical" LS coordinates to the physical coordinates of
     * a file.  Coordinates are 0-based.
     *
     * @returns PigImageCoordinate containing line/sample.
     */
    PigImageCoordinate getCameraCenter();

    /**
     * Returns the angle subtended by a pixel, in either the line or sample
     * directions (normally the same).  The angle is in radians, so the units are
     * radians/pixel.  If this is not constant across the camera, the angle for
     * the central pixel (looking down the pointing vector) is returned.  If
     * "which" is 0, the Line direction is returned; 1 returns the Sample direction.
     * @param which Either PigConstants.TYPE_LINE or PigConstants.TYPE_SAMPLE
     */
    double getPixelAngle(int which);

    /**
     * Translate a line/samp into an origin and look direction vector.  Note that
     * no surface is involved here.  The result is returned in a PigLookVector.
     */
    //!!!! CS params
    PigLookVector LStoLookVector(PigImageCoordinate coordiante);

    /**
     * Translate a 3D point into a line/samp location.  The result is returned
     * in a PigImageCoordinate.  If the infinity_flag is true, the xyz point 
     * should actually be a unit vector pointing in the infinity direction.
     */
    //!!!! CS params
    PigImageCoordinate XYZtoLS(PigPoint xyz, boolean infinity_flag);


    public String getRefFrame();
    
}

