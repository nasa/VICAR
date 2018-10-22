package jpl.mipl.mars.pig;

public class PigImageCoordinate
{
    
    protected double _dSample;
    protected double _dLine;
    

    
    /**
     * Constructor using double values
     * @param line Line coordinate
     * @param sample Sample coordinate
     */
    public PigImageCoordinate(double line, double sample)
    {
        this._dSample = sample;
        this._dLine   = line;
    }
    
    protected static int doubleToInt(double dVal)
    {
        return ((int) Math.floor(dVal + 0.5));
    }
    
    /**
     * Get the sample as an integer
     * @return Sample value
     */
    public double getSample()
    {
        return this._dSample;
    }
    
    /**
     * Get the line as an integer
     * @return Line value
     */
    public double getLine()
    {
        return this._dLine;
    }
    

    
    public String toString()
    {
        return "PigImageCoordinate{sample="+this._dSample+",line="+this._dLine+"}";
    }
}
