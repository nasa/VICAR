import javax.media.jai.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.util.*;
import java.io.*;
import jpl.mipl.jade.util.JAICoordinateMapper;
import java.awt.Point;

/**
 *  Test program tests forward mapping using the JAICoordinateMapper.
 *  Specifically to test changes made such that all descendant nodes
 *  are searched, whereas before it was only the 1st child searched.
 *
 *
 * <PRE>
 *  Render Graph:
 *
 *            __________>  Scale x 4 _
 *           /                        \
 *  Constant (300x200)                 ===> Add --> Scale x .5
 *           \__________             _/
 *                      >  Scale x 6 
 *
 *  Tracking point: (150, 100)
 *
 * </PRE>
 *
 *  @author Nicholas Toole   (Nicholas.T.Toole@jpl.nasa.gov)
 *  @version 1.0 
 */

public class TestForwardMap
{
    ParameterBlockJAI _constPB;
    ParameterBlockJAI _scalePB;
    RenderedOp _scaleImage;
    RenderedOp _constImage;
    ParameterBlockJAI _scale2PB;
    RenderedOp _scale2Image;
    ParameterBlockJAI _addPB;
    RenderedOp _addImage;
    ParameterBlockJAI _scale3PB;
    RenderedOp _scale3Image;

    public TestForwardMap()
    {
        _constPB = new ParameterBlockJAI("constant");
        _constPB.setParameter("width", 300.0f);
        _constPB.setParameter("height", 200.0f);        
        Byte[] bandValues = new Byte[1];
        bandValues[0] = new Byte((byte) 0);
        _constPB.setParameter("bandValues", bandValues);
        _constImage = JAI.create("constant", _constPB);

        _scalePB = new ParameterBlockJAI("scale");
        _scalePB.setSource(_constImage, 0);
        _scalePB.setParameter("xScale", 4.0f);
        _scalePB.setParameter("yScale", 4.0f);
        _scaleImage = JAI.create("scale", _scalePB);

        _scale2PB = new ParameterBlockJAI("scale");
        _scale2PB.setSource(_constImage, 0);
        _scale2PB.setParameter("xScale", 6.0f);
        _scale2PB.setParameter("yScale", 6.0f);
        _scale2Image = JAI.create("scale", _scale2PB);

        _addPB = new ParameterBlockJAI("scale");
        _addPB.setSource(_scaleImage, 0);
        _addPB.setSource(_scale2Image, 1);
        _addImage = JAI.create("add", _addPB);

        _scale3PB = new ParameterBlockJAI("scale");
        _scale3PB.setSource(_addImage, 0);
        _scale3PB.setParameter("xScale", 0.5f);
        _scale3PB.setParameter("yScale", 0.5f);
        _scale3Image = JAI.create("scale", _scale3PB);


        Point2D originalPt = new Point(150, 100);
        Point2D nextPt = null;

        System.out.println("\nOriginal Point = "+originalPt+"\n");
        nextPt = JAICoordinateMapper.forwardMap(_constImage,
                                                originalPt,
                                                _constImage);
        System.out.println("Mapped Point self = "+nextPt);
        System.out.println("Should be (150,100)\n");

        nextPt = JAICoordinateMapper.forwardMap(_constImage,
                                                originalPt,
                                                _scaleImage);
        System.out.println("Mapped Point scale of 4 = "+nextPt);
        System.out.println("Should be about (600,400)\n");

        nextPt = JAICoordinateMapper.forwardMap(_constImage,
                                                originalPt,
                                                _scale2Image);
        System.out.println("Mapped Point scale of 6 = "+nextPt);
        System.out.println("Should be about (900,600)\n");

        nextPt = JAICoordinateMapper.forwardMap(_constImage,
                                                originalPt,
                                                _addImage);
        System.out.println("Mapped Point add scales = "+nextPt);
        System.out.println("Should be about (600,400)\n");

        nextPt = JAICoordinateMapper.forwardMap(_constImage,
                                                originalPt,
                                                _scale3Image);
        System.out.println("Mapped Point scale add down 2 = "+nextPt);
        System.out.println("Should be about (300,200)\n");

        System.out.println("Test complete.\n");
    }
    
    public static void main(String[] args)
    {
        TestForwardMap test = new TestForwardMap();        
    }

}
