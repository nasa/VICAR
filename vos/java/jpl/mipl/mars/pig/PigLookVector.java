package jpl.mipl.mars.pig;

public class PigLookVector
{
    protected PigPoint cp;
    protected PigVector lv;
    
    public PigLookVector(PigPoint origin, PigVector lookAt)
    {
        this.cp  = origin;
        this.lv  = lookAt;
    }
    
    public PigPoint getOrigin()
    {
        return this.cp;
    }
    
    public PigVector getLookAt()
    {
        return this.lv;
    }
    
    public String toString()
    {
        String ctStr = cp == null ? "null" : 
                       "("+cp.getX()+","+cp.getY()+","+cp.getZ()+")";
        String lvStr = lv == null ? "null" : 
            "("+lv.getX()+","+lv.getY()+","+lv.getZ()+")";
        
        return "PigLookVector{center="+ctStr+";lookAt="+lvStr+"}";
    }

}
