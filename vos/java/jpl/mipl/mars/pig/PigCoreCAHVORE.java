package jpl.mipl.mars.pig;

/***********************************************************************
 * Implements the CAHVORE camera model core functionality.
 * <p>
 * This is an immutable class, in that the vectors can only be set by the
 * constructor and cannot be modified. Create a new instance of the class if you
 * need a modification.
 *
 * @author Bob Deen, JPL
 * @author Nicholas Toole, JPL
 * Based on Todd Litwin's cmod C++ code
 */

public class PigCoreCAHVORE extends PigCoreCAHVOR
{

    protected final PigVector  _e;
    
    public static final int    DEFAULT_PUPIL_TYPE  = PigConstants.CAHVORE_PUPILTYPE_GENERAL;
    public static final double DEFAULT_PUPIL_PARAM = 0.0;
    
    protected final int        _pupilType;
    protected final double     _pupilParam;    
    protected       double     _linearity;
    
    protected final double CHIP_LIMIT = 1e-8;
    protected final double NEWTON_ITERATION_MAX = 100;
    
    
 
   
    /**
     * Constructor. This should generally be called by a factory routine that
     * knows how to read some given format rather than directly by applications.
     */
    // !!!! CS param
    public PigCoreCAHVORE(PigPoint c, PigVector a, PigVector h, PigVector v,
                          PigVector o, PigVector r, PigVector e)
    {
        this(c, a, h, v, o, r, e, DEFAULT_PUPIL_TYPE, DEFAULT_PUPIL_PARAM, null);
    }

    public PigCoreCAHVORE(PigPoint c, PigVector a, PigVector h, PigVector v,
                          PigVector o, PigVector r, PigVector e, 
                          int pupilType, double pupilParam, 
                          String referenceFrame)
    {
        super(c, a, h, v, o, r, referenceFrame);
        _e = e;
        _pupilType  = pupilType;
        _pupilParam = pupilParam;
        
        if (_pupilType == PigConstants.CAHVORE_PUPILTYPE_PERSPECTIVE)
            _linearity = PigConstants.CAHVORE_LINEARITY_PERSPECTIVE;
        else if (_pupilType == PigConstants.CAHVORE_PUPILTYPE_FISHEYE)
            _linearity = PigConstants.CAHVORE_LINEARITY_FISHEYE;
        else if (_pupilType == PigConstants.CAHVORE_PUPILTYPE_GENERAL)
            _linearity = _pupilParam;
    }

    public PigCoreCAHVORE(PigPoint c, PigVector a, PigVector h, PigVector v,
            PigVector o, PigVector r, PigVector e, 
            int pupilType, double pupilParam)
    {
        this(c, a, h, v, o, r, e, pupilType, pupilParam, null);
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

        boolean approxFlag = true;
        
        PigPoint origin;
        PigVector look_direction;

        
        // The projection point is merely the C of the camera model
        origin = _c;

       
        //----------
        
        PigVector f = _a.multiply(line);
        f = _v.subtract(f);
        PigVector g = _a.multiply(samp);
        g = _h.subtract(g);        
        PigVector w3 = f.cross(g);
        
        
        f = _v.cross(_h);
        double adotf = _a.dot(f);
        double inv_adotf = 1.0 / adotf;        
        PigVector rp = w3.multiply(inv_adotf);
        
        double zetap = rp.dot(_o);         
        
        f = _o.multiply(zetap);
        PigVector lambdap = rp.subtract(f);
        
        double lambdapMag = lambdap.magnitude();
        
        double chip = lambdapMag / zetap; 
        
        PigVector centerPoint, rayOfIncidence;
        
        //Approx for small angles...
        if (chip < CHIP_LIMIT)
        {
            centerPoint = _c;
            rayOfIncidence = _o;
        }
        else
        {
            //full calculations
            int iterationCount;
            double chi, chi2, chi3, chi4, chi5;
            double dchi, s;
            
            //Newtons method, obbbbviously
            iterationCount = 0;
            chi = chip;
            dchi = 1;
            
            boolean cont = true;
            while (cont)
            {
                double deriv;
                
                //compute terms from the current value of chi
                chi2 = chi  * chi;
                chi3 = chi2 * chi;
                chi4 = chi3 * chi;
                chi5 = chi4 * chi;
                
                
                //update chi and dchi
                deriv = (1.0 + _r.getX()) + (3.0 * _r.getY() * chi2) + ( 5.0 * _r.getZ() * chi4);                
                dchi = (deriv == 0.0) ? 0.0 :
                       ((1.0 + _r.getX()) * chi) + (_r.getY() * chi3) + ((_r.getZ() * chi5) - chip) / deriv;                
                chi -= dchi;
                
                //this is our exit criterion
                if (Math.abs(dchi) < CHIP_LIMIT)
                {
                    cont = false;                   
                }
                                
                if (++iterationCount > NEWTON_ITERATION_MAX)
                {
                    System.err.println("PigCoreCAHVORE: Too many iterations without sufficient convergence");
                    cont = false;
                }
            }
            
            //compute the incoming ray's angle
            double  theta, theta2, theta3, theta4;            
            double linchi = _linearity * chi;
            if (_linearity < (-1.0 * PigConstants.EPSILON))
                theta = Math.asin(linchi) / _linearity;
            if (_linearity < PigConstants.EPSILON)
                theta = Math.atan(linchi) / _linearity;
            else
                theta = chi;
            
            theta2 = theta  * theta;
            theta3 = theta2 * theta;
            theta4 = theta3 * theta;
            
            //compute the shift of the entrance pupil
            s = ((theta / Math.sin(theta)) - 1) * 
                        (_e.getX() + _e.getY()*theta2 + _e.getZ()*theta4);
            
            //the position of the entrance pupil
            centerPoint = _o.multiply(s);
            centerPoint = _c.add(centerPoint);
            
            //unit vector along the ray
            f = lambdap.normalize();
            f = f.multiply(Math.sin(theta));
            g = _o.multiply(Math.cos(theta));
            rayOfIncidence = f.add(g);           
        }
     
        origin         = new PigPoint(centerPoint);
        look_direction = rayOfIncidence;
        
        
        
        // !!!! Skip partial derivative calculation
        
        
        
        
        if (!approxFlag)
        {
            System.err.println("PigCoreCAHVORE: Only approximations are currently supported for calculations");
        }
        
        PigLookVector rtn = new PigLookVector(origin, look_direction);
        return rtn;
        
    }

    /**
     * Translate a 3D point into a line/samp location. The result is returned in
     * a PigImageCoordinate. If the infinity_flag is true, the xyz point should 
     * actually be a unit vector pointing in the infinity direction.
     */
    // !!!! CS params
    public PigImageCoordinate XYZtoLS(PigPoint xyz, boolean infinity_flag)
    {
        double line, samp;
        
        //Basic Computations

        //calculate initial terms
        PigVector p_c = xyz.subtract(_c);
        double zeta = p_c.dot(_o);
        PigVector f = _o.multiply(zeta);
        PigVector lambda = p_c.subtract(f);
        double lambdaMag = lambda.magnitude();
        
        //calculate using NEWTON's method
        int iterationCount = 0;
        double theta = Math.atan2(lambdaMag, zeta);
        double dtheta = 1.0;
        
        double cosTheta, sinTheta, theta2, theta3, theta4, upsilon;
        boolean cont = true;
        while (cont)
        {
            //compute the terms from the current value of theta
            cosTheta = Math.cos(theta);
            sinTheta = Math.sin(theta);
            theta2 = theta  * theta;
            theta3 = theta2 * theta;
            theta4 = theta3 * theta;
            upsilon = (zeta * cosTheta) + (lambdaMag * sinTheta) 
                      - ((  1.0 - cosTheta) * (_e.getX() +   _e.getY()*theta2 +   _e.getZ()*theta4))
                      - ((theta - sinTheta) * (          + 2*_e.getY()*theta  + 4*_e.getZ()*theta3));
            
            //update theta
            dtheta = ((zeta*sinTheta - lambdaMag*cosTheta) -
                     (theta - sinTheta) * (_e.getX() + _e.getY()*theta2 + _e.getZ()*theta4)) /
                     ( upsilon );
            theta -= dtheta;
            
            
            //check exit criterion for last update
            if (Math.abs(dtheta) < CHIP_LIMIT)
            {
                cont = false;
            }
            
            
            //make sure we don't iterate forever
            if (++iterationCount > NEWTON_ITERATION_MAX)
            {
                System.err.println("PigCoreCAHVORE: Too many iterations without sufficient convergence");
                cont = false;
            }            
        }
        
        //check value of theta
        if (theta * Math.abs(_linearity) > (Math.PI / 2))
        {
            System.err.println("PigCoreCAHVORE: Theta out of bounds");
            return null;
        }
      
        
        PigVector rp = null;
        
                
        //approximation for small theta
        if (theta < CHIP_LIMIT)
        {
            rp = p_c;
            
            //skip partial derivative calculations
        }
        else
        {
            //full calculations
            double linth, chi, chi2, chi3, chi4, psi, zetap, mu, nu;
            
            linth = _linearity * theta;
            if (_linearity < (-1 * PigConstants.EPSILON))
                chi = Math.sin(linth) / _linearity;
            else if (_linearity > PigConstants.EPSILON) 
                chi = Math.tan(linth) / _linearity;
            else
                chi = theta;
            
            chi2 = chi  * chi;
            chi3 = chi2 * chi;
            chi4 = chi3 * chi;
            
            zetap = lambdaMag / chi;
            
            mu = _r.getX() + _r.getY()*chi2 + _r.getZ()*chi4;
            
            f = _o.multiply(zetap);
            PigVector g = lambda.multiply(1 + mu);
            
            rp = f.add(g);
            
            //skip partial derivative calculations
        }
  
        //calculate the projection
        double alpha = rp.dot(_a);
        double beta  = rp.dot(_h);
        double gamma = rp.dot(_v);
        
        samp = beta  / alpha;
        line = gamma / alpha;
        
        
        //skip partial derivative calculations
        
        
        PigImageCoordinate rtn = new PigImageCoordinate(line, samp);
        return rtn;
    }

    public PigVector getE()          { return _e; }
    public int       getPupilType()  { return _pupilType; }
    public double    getPupilParam() { return _pupilParam; }
}
