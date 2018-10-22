package jpl.mipl.mars.pig;

/***********************************************************************
 * Implements the CAHV camera model.
 * <p>
 * This is an immutable class, in that the vectors can only be set by the
 * constructor and cannot be modified.  Create a new instance of the class
 * if you need a modification.
 *
 * @author Bob Deen, JPL
 */

public class PigCoreCAHV implements PigCameraModel
{
    protected final PigPoint  _c;
    protected final PigVector _a;
    protected final PigVector _h;
    protected final PigVector _v;
    protected final String    _referenceFrame; 


    /**
     * Constructor.  This should generally be called by a factory routine that
     * knows how to read some given format rather than directly by applications.
     */
    //!!!! CS param
    public PigCoreCAHV(PigPoint c, PigVector a, PigVector h, PigVector v)
    {
        this(c,a,h,v,null);
    }

    public PigCoreCAHV(PigPoint c, PigVector a, PigVector h, PigVector v, String referenceFrame)
    {
        _c = c;
        _a = a;
        _h = h;
        _v = v;
        _referenceFrame = referenceFrame;
    }
    
    /**
     * Retrieves the image-space coordinates of the "pointing axis" of the camera.
     * This is encoded in the H and V vectors (basically, it is the origin of the
     * image plane during calibration).
     */
    public PigImageCoordinate getCameraCenter()
    {
    	double line = _a.dot(_v);		// line
    	double samp = _a.dot(_h);		// samp
    	PigImageCoordinate rtn = new PigImageCoordinate(line, samp);
    	return rtn;
    }

    /**
     * Returns the angle subtended by a pixel, in either the line or sample
     * directions (normally the same).  The angle is in radians, so the units are
     * radians/pixel.  If this is not constant across the camera, the angle for
     * the central pixel (looking down the pointing vector) is returned.  If
     * "which" is 0, the Line direction is returned; 1 returns the Sample direction.
     * <p>
     * Derivation:  H is defined as f*H' + i0*A (see CAHV paper).  We derive
     * f (focal length) and then construct a right triangle with f as the long
     * side and 1.0 (pixels on the focal plane) as the short side.  The arctan
     * of those is thus the angle subtended by that one pixel.
     */
    public double getPixelAngle(int which)
    {
    	double f, ang;
    	if (which == 0) {			// Line (v) direction
    	    // f = (_v - _a * (_v % _a)).magnitude();
    	    f = (_v.subtract(_a.multiply(_v.dot(_a)))).magnitude();
    	    ang = Math.atan(1.0 / f);
    	} else {
    	    // f = (_h - _a * (_h % _a)).magnitude();
    	    f = (_h.subtract(_a.multiply(_h.dot(_a)))).magnitude();
    	    ang = Math.atan(1.0 / f);
    	}
    	return ang;
    }

    /**
     * Translate a line/samp into an origin and look direction vector.  Note that
     * no surface is involved here.  The result is returned in a PigLookVector.
     */
    //!!!! CS params
    public PigLookVector LStoLookVector(PigImageCoordinate coordiante)
    {
        double line = coordiante.getLine();
        double samp = coordiante.getSample();
        
    	PigPoint origin;
    	PigVector look_direction;
    
    	// The projection point is merely the C of the camera model
    	origin = _c;
    
    	// Calculate the projection ray assuming normal vector directions
    	PigVector f = _a.multiply(line);
    	f = _v.subtract(f);
    	PigVector g = _a.multiply(samp);
    	g = _h.subtract(g);
    	look_direction = f.cross(g);
    	look_direction.normalize();
    
    	// Check and optionally correct for vector directions
    
    	PigVector t = _v.cross(_h);
    	if (t.dot(_a) < 0) {
    	    look_direction = look_direction.multiply(-1.0);
    	}
    
            //!!!! omit partial derivative computation
    
            //!!!! CS conversion
  
    	
    	PigLookVector rtn = new PigLookVector(origin, look_direction);
    	return rtn;
    }

    
    /**
     * Translate a 3D point into a line/samp location.  The result is returned
     * in a PigImageCoordinate.  If the
     * infinity_flag is true, the xyz point should actually be a unit vector
     * pointing in the infinity direction.
     */
    //!!!! CS params
    public PigImageCoordinate XYZtoLS(PigPoint xyz, boolean infinity_flag)
    {
    	double line, samp;
    
    	// If infinity, assume XYZ point is the look vector, and project the
    	// vanishing point.  This should use the cmod_cahv code, but a)
    	// cmod_cahv_3d_to_2d_ray() calculates a bunch of stuff we don't need
    	// (the back projection of the ray) and this is fairly time-critical
    	// code, and b) there appears to be no cahvor form of this function.
    	// Note that for the infinity case, the CS conversion needs to use
    	// the vector form, not the point form!
    
    	if (infinity_flag) {
    	    PigVector look = xyz;
    	    //!!!! cs convert
    	    double x = look.dot(_a);
    	    samp = (look.dot(_h)) / x;
    	    line = (look.dot(_v)) / x;
    	}
    	else {
    	    //!!!! cs convert
    
    	    // Calculate the projection
    	    PigVector d = xyz.subtract(_c);
    	    double range = d.dot(_a);
    	    double r_1 = 1.0 / range;
    	    samp = d.dot(_h) * r_1;
    	    line = d.dot(_v) * r_1;
    	}
    
    	PigImageCoordinate rtn = new PigImageCoordinate(line, samp);    	
    	return rtn;
    }

    public PigPoint  getC() { return _c; }
    public PigVector getA() { return _a; }
    public PigVector getH() { return _h; }
    public PigVector getV() { return _v; }
    
    public String    getRefFrame() 
    { 
        return this._referenceFrame; 
    }

}

