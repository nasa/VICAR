package jpl.mipl.mars.pig;

/***********************************************************************
 * Implements the CAHVOR camera model core functionality.
 * <p>
 * This is an immutable class, in that the vectors can only be set by the
 * constructor and cannot be modified. Create a new instance of the class if you
 * need a modification.
 *
 * @author Bob Deen, JPL
 */

public class PigCoreCAHVOR extends PigCoreCAHV
{
    protected final PigVector _o;
    protected final PigVector _r;

    protected static final int MAXITER = 20; // max # of iterations allowed

    protected static final double CONV = 1.0e-6; // convergence tolerance

    /**
     * Constructor. This should generally be called by a factory routine that
     * knows how to read some given format rather than directly by applications.
     */
    // !!!! CS param
    public PigCoreCAHVOR(PigPoint c, PigVector a, PigVector h, PigVector v,
            PigVector o, PigVector r)
    {
        this(c, a, h, v, o, r, null);
    }

    public PigCoreCAHVOR(PigPoint c, PigVector a, PigVector h, PigVector v,
            PigVector o, PigVector r, String referenceFrame)
    {
        super(c, a, h, v, referenceFrame);
        _o = o;
        _r = r;
    }

    /**
     * Translate a line/samp into an origin and look direction vector. Note that
     * no surface is involved here. The result is returned in a PigLookVector.
     */
    // !!!! CS params
    public PigLookVector LStoLookVector(PigImageCoordinate coordiante)
    {
        double line = coordiante.getLine();
        double samp = coordiante.getSample();
   
        PigPoint origin;
        PigVector look_direction;

        // The projection point is merely the C of the camera model
        origin = _c;

        // Calculate the projection ray assuming normal vector directions,
        // neglecting distortion.
        PigVector f = _a.multiply(line);
        f = _v.subtract(f);
        PigVector g = _a.multiply(samp);
        g = _h.subtract(g);
        PigVector rr = f.cross(g);
        rr.normalize();

        // Check and optionally correct for vector directions

        PigVector t = _v.cross(_h);
        if (t.dot(_a) < 0)
        {
            rr = rr.multiply(-1.0);
        }

        // !!!! omit partial derivative computation

        // Remove the radial lens distortion. Preliminary values of omega,
        // lambda, and tau are computed from the rr vector including
        // distortion, in order to obtain the coefficients of the equation
        // k5*u^5 + k3*u^3 + k1*u = 1, which is solved for u by means of
        // Newton's method. This value is used to compute the corrected rr.
        double omega = rr.dot(_o);
        double omega_2 = omega * omega;
        PigVector wo = _o.multiply(omega);
        PigVector lambda = rr.subtract(wo);
        double tau = lambda.dot(lambda) / omega_2;
        double k1 = 1 + _r.getX(); // 1 + rho0
        double k3 = _r.getY() * tau; // rho1*tau
        double k5 = _r.getZ() * tau * tau; // rho2*tau^2
        double mu = _r.getX() + k3 + k5;
        double u = 1.0 - mu; // initial approximation for iterations

        int i;
        for (i = 0; i < MAXITER; i++)
        {
            double u_2 = u * u;
            double poly = ((k5 * u_2 + k3) * u_2 + k1) * u - 1;
            double deriv = (5 * k5 * u_2 + 3 * k3) * u_2 + k1;
            if (deriv <= PigConstants.EPSILON)
            {
                System.err.println("cahvor 2d to 3d: Distortion is too negative");
                break;
            }
            else
            {
                double du = poly / deriv;
                u -= du;  //was "u -= u;" originally??  BOB!!
                
                if (Math.abs(du) < CONV)
                {
                    break;
                }
            }
        }
        if (i >= MAXITER)
        {
            System.err.println("cahvor 2d to 3d: Too many iterations");
        }
        mu = 1 - u;
        PigVector pp = lambda.multiply(mu);
        look_direction = rr.subtract(pp);
        look_direction.normalize();

        // !!!! Skip partial derivative calculation

        PigLookVector rtn = new PigLookVector(origin, look_direction);
        return rtn;
    }

    /**
     * Translate a 3D point into a line/samp location. The result is returned in
     * a PigImageCoordinate. If the
     * infinity_flag is true, the xyz point should actually be a unit vector
     * pointing in the infinity direction.
     */
    // !!!! CS params
    public PigImageCoordinate XYZtoLS(PigPoint xyz, boolean infinity_flag)
    {
        double line, samp;

        // If infinity, assume XYZ point is the look vector, and project the
        // vanishing point.

        // Calculate (p' - c). Note p' is never computed directly,
        // but is understood to be a unit distance from c in the
        // direction or the vanishing point.

        if (infinity_flag)
        {
            PigVector look = xyz;
            // !!!! cs convert
            double omega = look.dot(_o);
            double omega_2 = omega * omega;
            PigVector wo = _o.multiply(omega);
            PigVector lambda = look.subtract(wo);
            double tau = lambda.dot(lambda) / omega_2;
            double mu = _r.getX() + (_r.getY() * tau) + (_r.getZ() * tau * tau);
            PigVector pp_c = lambda.multiply(mu);
            pp_c = look.add(pp_c);

            // Calculate alpha, beta, gamma, which are (p' - c)
            // dotted with a, h, v, respectively
            double alpha = pp_c.dot(_a);
            double beta = pp_c.dot(_h);
            double gamma = pp_c.dot(_v);

            // Calculate the projection
            samp = beta / alpha;
            line = gamma / alpha;

            // !!!! skip partial derivative
        }
        else
        {

            // Calculate p' and other necessary quantities

            PigVector p_c = xyz.subtract(_c);
            double omega = p_c.dot(_o);
            double omega_2 = omega * omega;
            PigVector wo = _o.multiply(omega);
            PigVector lambda = p_c.subtract(wo);
            double tau = lambda.dot(lambda) / omega_2;
            double mu = _r.getX() + (_r.getY() * tau) + (_r.getZ() * tau * tau);
            PigVector pp = lambda.multiply(mu);
            pp = xyz.add(pp);

            // Calculate alpha, beta, gamma, which are (p' - c)
            // dotted with a, h, v, respectively

            PigVector pp_c = pp.subtract(_c);
            double alpha = pp_c.dot(_a);
            double beta = pp_c.dot(_h);
            double gamma = pp_c.dot(_v);

            // Calculate the projection
            samp = beta / alpha;
            line = gamma / alpha;

            // !!!! Skip partial derivative and range
        }

        PigImageCoordinate rtn = new PigImageCoordinate(line, samp);
        
        return rtn;
    }

    public PigVector getO()
    {
        return _o;
    }

    public PigVector getR()
    {
        return _r;
    }

}
