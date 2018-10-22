package jpl.mipl.mars.pig;

import Jama.Matrix;
import java.awt.geom.Point2D;

/***********************************************************************
 * Implements the DLT camera model. This is a model based on Ron Li's paper
 * (according to the original code by Dongjoe Shin). Need a specific
 * reference!!!! Also, requires the JAMA matrix classes.
 * <p>
 * This is an immutable class, in that the values can only be set by the
 * constructor and cannot be modified. Create a new instance of the class if you
 * need a modification.
 * <p>
 * !!!! THIS IS INCOMPLETE!!!! Need to flesh out more before using. Now !!!!
 * only conversion from CAHV and access to the matrix is implemented. !!!!
 * CAHVOR is sort-of implemented but requires external work
 * 
 * @author Bob Deen, JPL
 */

public class PigCoreDLT implements PigCameraModel
{
    protected final Matrix _DLTcam;

    protected final PigVector _hvec; // these are for the distortion stuff
    protected final PigVector _vvec;
    protected final PigVector _ovec;
    protected final PigVector _rvec;
    protected boolean _isRadial;
    protected String _referenceFrame;

    /**
     * Constructor. This should generally be called by a factory routine that
     * knows how to read some given format rather than directly by applications.
     */
    public PigCoreDLT(Matrix dlt)
    {
        this(dlt, null);
    }

    public PigCoreDLT(Matrix dlt, String referenceFrame)
    {
        _DLTcam = dlt.copy(); // copy matrix so immutable
        _hvec = null;
        _vvec = null;
        _ovec = null;
        _rvec = null;
        _isRadial = false;
        _referenceFrame = referenceFrame;

    }

    /**
     * Constructor to convert from CAHV or CAHVOR. CAHVOR must be dealt with
     * explicitly via the distortion methods.
     */
    public PigCoreDLT(PigCameraModel model)
    {
        PigCoreCAHV cahv = (PigCoreCAHV) model; // only CAHV supported now

        PigPoint c = cahv.getC();
        PigVector a = cahv.getA();
        PigVector h = cahv.getH();
        PigVector v = cahv.getV();
        _hvec = h;
        _vvec = v;
        _referenceFrame = model.getRefFrame();

        double[][] dTemp = new double[3][4];
        double L = -1.0 / (a.dot(c));
        dTemp[0][0] = L * h.getX();
        dTemp[0][1] = L * h.getY();
        dTemp[0][2] = L * h.getZ();
        dTemp[0][3] = -L * (h.dot(c));
        dTemp[1][0] = L * v.getX();
        dTemp[1][1] = L * v.getY();
        dTemp[1][2] = L * v.getZ();
        dTemp[1][3] = -L * (v.dot(c));
        dTemp[2][0] = L * a.getX();
        dTemp[2][1] = L * a.getY();
        dTemp[2][2] = L * a.getZ();
        dTemp[2][3] = 1;

        _DLTcam = new Matrix(dTemp);

        if (model instanceof PigCoreCAHVOR)
        { // save for distortion stuff
            _ovec = ((PigCoreCAHVOR) model).getO();
            _rvec = ((PigCoreCAHVOR) model).getR();
            _isRadial = true;
        }
        else
        {
            _ovec = ((PigCoreCAHV) model).getA();
            _rvec = null;
            _isRadial = false;
        }
    }

    /**
     * Retrieve the DLT matrix.
     */
    public Matrix getDLTMatrix()
    {
        return _DLTcam;
    }

    /**
     * True if there's radial distortion (CAHVOR).
     */
    public boolean isRadial()
    {
        return _isRadial;
    }

    /**
     * Get distortion center. Algorithm copied from Dongjoe's code.
     */
    public Point2D.Double getDistortionCenterOnImg()
    {
        // If no radial, use A instead of O - but that's set in the ctor
        // so we can always use O here.

        Point2D.Double ptRes = new Point2D.Double(0, 0);
        ptRes.x = _ovec.dot(_hvec);
        ptRes.y = _ovec.dot(_vvec);

        return ptRes;
    }

    /**
     * Get undistorted point (I think this means, where the point falls in the
     * image after the image is rectified to remove radial distortion).
     * Algorithm copied from Dongjoe's code.
     */

    public Point2D getUndistortedPt(Point2D ptIn)
    {

        Point2D ptRes = (Point2D) ptIn.clone();
        boolean bUpdate = false;
        Point2D ptCentre = new Point2D.Double(0, 0);
        double dCoeff = 0.d;
        double r = 1.;
        if (isRadial())
        {
            ptCentre = getDistortionCenterOnImg();
            r = ptCentre.distance(ptIn);
            // !!!! In Dongjoe's code, getRadialDistCoeff() was called only
            // !!!! for the Right-side camera. I think that was a bug so I
            // !!!! put it in for all cameras. rgd
            dCoeff = getRadialDistCoeff(r) / r;
            bUpdate = true;
        }

        // update new data
        if (bUpdate)
        {
            ptRes = new Point2D.Double(
                    (ptIn.getX() - ptCentre.getX()) * dCoeff + ptCentre.getX(),
                    (ptIn.getY() - ptCentre.getY()) * dCoeff + ptCentre.getY());
        }
        else
        {
            ptRes = (Point2D) ptIn.clone();
        }

        return ptRes;
    }

    /**
     * Get radial distortion coefficient. Algorithm copied from Dongjoe's code.
     */
    public double getRadialDistCoeff(double r)
    {
        // dr = k0*r + k1*r^3 + k2*r^5
        // k0 = m_matR(0,0)
        // k1 = m_matR(1,0)/fav^2
        // k2 = m_matR(2,0)/fav^4
        // fav = (fx+fy)/2
        // fx = |O cross H|
        // fx = |O cross V|

        double dRes = 0.d;
        double[] k = new double[3];
        double fav = 1.f;
        double fx, fy;

        // h[0] = m_matO.get(1, 0) * m_matH.get(2, 0) - m_matO.get(2, 0) *
        // m_matH.get(1, 0);
        // h[1] = m_matO.get(2, 0) * m_matH.get(0, 0) - m_matO.get(0, 0) *
        // m_matH.get(2, 0);
        // h[2] = m_matO.get(0, 0) * m_matH.get(1, 0) - m_matO.get(1, 0) *
        // m_matH.get(0, 0);

        // v[0] = m_matO.get(1, 0) * m_matV.get(2, 0) - m_matO.get(2, 0) *
        // m_matV.get(1, 0);
        // v[1] = m_matO.get(2, 0) * m_matV.get(0, 0) - m_matO.get(0, 0) *
        // m_matV.get(2, 0);
        // v[2] = m_matO.get(0, 0) * m_matV.get(1, 0) - m_matO.get(1, 0) *
        // m_matV.get(0, 0);

        // fx = Math.sqrt(h[0]*h[0] + h[1]*h[1] + h[2]*h[2]);
        // fy = Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

        fx = (_ovec.cross(_hvec)).magnitude();
        fy = (_ovec.cross(_vvec)).magnitude();

        fav = (fx + fy) / 2.d;
        k[0] = _rvec.getX();
        k[1] = _rvec.getY() / fav / fav;
        k[2] = _rvec.getZ() / fav / fav / fav / fav;

        dRes = k[0] * r + k[1] * r * r * r + k[2] * r * r * r * r * r;
        return dRes;
    }

    /**
     * Retrieves the image-space coordinates of the "pointing axis" of the
     * camera.
     */
    public PigImageCoordinate getCameraCenter()
    {
        throw new UnsupportedOperationException();
    }

    /**
     * Returns the angle subtended by a pixel, in either the line or sample
     * directions (normally the same). The angle is in radians, so the units are
     * radians/pixel. If this is not constant across the camera, the angle for
     * the central pixel (looking down the pointing vector) is returned. If
     * "which" is 0, the Line direction is returned; 1 returns the Sample
     * direction.
     */
    public double getPixelAngle(int which)
    {
        throw new UnsupportedOperationException();
    }

    /**
     * Translate a line/samp into an origin and look direction vector. Note that
     * no surface is involved here. The result is returned in an array[2] of
     * PigVectors. The first element is the origin, and the second is the look
     * direction (as a unit vector). The origin really is a PigPoint and can be
     * cast that way if desired.
     */
    // !!!! CS params
    public PigLookVector LStoLookVector(PigImageCoordinate coordiante)
    {
        throw new UnsupportedOperationException();
    }

    /**
     * Translate a 3D point into a line/samp location. The result is returned in
     * an array[2] of double's where the first element is the line (y)
     * coordinate and the second is the sample (x) coordinate. If the
     * infinity_flag is true, the xyz point should actually be a unit vector
     * pointing in the infinity direction.
     */
    // !!!! CS params
    public PigImageCoordinate XYZtoLS(PigPoint xyz, boolean infinity_flag)
    {
        throw new UnsupportedOperationException();
    }

    public String getRefFrame()
    {
        return this._referenceFrame;
    }

}
