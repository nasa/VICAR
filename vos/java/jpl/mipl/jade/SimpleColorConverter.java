package jpl.mipl.jade;

import javax.media.jai.*;
import java.io.*;
import java.util.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.color.ColorSpace;
import java.awt.*;
import javax.swing.*;
import jpl.mipl.jade.*;

/**
 * <b>Purpose:</b>
 * This program accepts three images (or a three-band image), decomposes 
 * it to HSI space, manipulates HSI values, converts back to RGB, and 
 * writes result.  Current image manipulation is adding/subtracting 
 * constants from HSI bands.
 *
 *   <PRE>
 *   Copyright 2004, California Institute of Technology.
 *   ALL RIGHTS RESERVED.
 *   U.S. Government Sponsorship acknowledge. 2003.
 *   </PRE>
 *
 * <PRE>
 * ============================================================================
 * <B>Modification History :</B>
 * ----------------------
 *
 * <B>Date              Who              What</B>
 * ----------------------------------------------------------------------------
 * 03/11/2004        Nick             Initial Release.
 * ============================================================================
 * </PRE>
 *
 * @class SimpleColorConverter
 * @author Nicholas Toole	(Nicholas.T.Toole@jpl.nasa.gov)
 * @version 2004.03.11
 *
 */

public class SimpleColorConverter
{

    boolean DEBUG1 = false;
    boolean DEBUG2 = false;

    private final String className_ = "SimpleColorConverter";

    public static final double HUE_MAX =  180.0;
    public static final double HUE_MIN = -180.0;
    public static final double SAT_MAX =  100.0;    
    public static final double SAT_MIN = -100.0;
    public static final double INT_MAX =  100.0;    
    public static final double INT_MIN = -100.0;

    protected RenderedOp _finalImage;

    protected double _intDelta;
    protected double _hueDelta;
    protected double _satDelta;
    protected double _intAdjust;
    protected double _hueAdjust;
    protected double _satAdjust;

    protected String[] _rgbFilenames;  
    protected String[] _uniFilenames;
    protected int _inputFileType;

    protected final int SINGLE_FILE = 0;
    protected final int MULTI_FILE  = 1;
    protected final int RED_INDEX   = 0;
    protected final int GREEN_INDEX = 1;
    protected final int BLUE_INDEX  = 2;

    protected String _outFilename;

    protected boolean _adjustInt;
    protected boolean _adjustHue;
    protected boolean _adjustSat;

    boolean _rangeMinOverriden, _rangeMaxOverriden;
    double  _rangeMinOverride, _rangeMaxOverride;
    String prefix = "/home/ntt/dev/marsview/jpl/mipl/mars/"; //for DEBUG

    boolean _printHelp;   

    //---------------------------------------------------------------------

    public SimpleColorConverter(String[] args)
    {  
        _printHelp = false;

        //init data to defaults
        _adjustInt = false;
        _adjustHue = false;
        _adjustSat = false;
        _rangeMinOverriden = false;
        _rangeMaxOverriden = false;
        _intAdjust = 0.0;
        _hueAdjust = 0.0;
        _satAdjust = 0.0;
        _intDelta  = 0.0;
        _hueDelta  = 0.0;
        _satDelta  = 0.0;

        _inputFileType = -1;

        //init RGB filename array (3 strings)
        _rgbFilenames = new String[3];
        for (int i = 0; i < _rgbFilenames.length; ++i)
            _rgbFilenames[i] = null;

        //init single filename array (1 string)
        _uniFilenames = new String[1];
        for (int i = 0; i < _uniFilenames.length; ++i)
            _uniFilenames[i] = null;

        //parse the arguments
        boolean parseSuccess = parseArguments(args);
        if (!parseSuccess)
        {
            if (_printHelp)
            {
                printHelp(System.out);
                System.exit(0);
            }
            else 
            {
                System.err.println(className_+":: Could not parse command"+
                            "-line arguments.");
                System.err.println(className_+":: Use -help option for help.");
                System.err.println(className_+":: Exiting...\n");
                System.exit(1);
            }
        }

        //perform the conversion
        try {
            convert();
        } catch (Exception ex) {
            ex.printStackTrace();
            System.err.println(className_+":: Error occurred during "+
                               "conversion.");
            System.err.println(className_+":: Exiting...\n");
            System.exit(1);
        }

        //write result to file
        try {
            writeImage();
        } catch (Exception ex) {
            ex.printStackTrace();
            System.err.println(className_+":: Error occurred during image "+
                               "write.");
            System.err.println(className_+":: Exiting...\n");
            System.exit(1);
        }
    }

    //---------------------------------------------------------------------

    public void printUsage(OutputStream os)
    {
        StringBuffer buf = new StringBuffer("\n");
        buf.append("  Usage:  java jpl.mipl.jade.SimpleColorConverter ").
            append("[parameters]\n");

        try {
            os.write(buf.toString().getBytes());
        } catch (IOException ioEx) {
            System.err.println(className_+"::printUsage(): "+
                               "Could not write to output stream.\n"+
                               "Error: "+ioEx.getMessage());
            return;
        }
    }

    //---------------------------------------------------------------------

    public void printHelp(OutputStream os)
    {
       StringBuffer buf = new StringBuffer("\n");

       buf.append("\n").
           append("  Purpose:\n\n").
           append("    Simple color conversion program that accepts an RGB\n").
           append("    three-banded image, or three single-banded images,\n").
           append("    decomposes to IHS (Intensity/Hue/Saturation) color\n").  
           append("    space, manipulates IHS values, converts back to RGB,\n"). 
           append("    and writes results.\n").            
           append("\n");
               
       buf.append("  Usage:\n\n").
           append("    java jpl.mipl.jade.SimpleColorConverter [parameters]\n");
     
       buf.append("\n").
           append("  Parameters:\n\n").
           append("     + File Specification (Required):\n").
           append("        - Input. Choose one of the two input file options:\n").
           append("          (1)  -in  image_file         }- specify 3-banded image\n").
           append("                                  XOR                \n").
           append("          (2)  -red red_image_file     }  specify each band\n").
           append("               -grn grn_image_file     }- individually.\n").
           append("               -blu blu_image_file     }  (all required)\n").
           append("\n").
           append("        - Ouput.  Specify filename to which result is written:\n").
           append("          (1)  -out output_file        }- output filename\n").
           append("\n").
           append("     + IHS Color Delta Specification (Optional):\n").
           append("        - Specify zero or more of the following:\n").
           append("          (1)  -int intensity_delta    }- range: [-100,100]\n").
           append("          (2)  -hue hue_delta          }- range: [-180,180]\n").
           append("          (3)  -sat saturation_delta   }- range: [-100,100]\n").
           append("\n").
           append("     + Data Range Specification (Optional):\n").
           append("        - Specify zero or more of the following:\n").
           append("          (1)  -min dataRangeMin        }- minimum DN value\n").
           append("          (2)  -max dataRangeMax        }- maximum DN value\n").
           append("\n");
         

       try {
            os.write(buf.toString().getBytes());
        } catch (IOException ioEx) {
            System.err.println(className_+"::printHelp(): "+
                               "Could not write to output stream.\n"+
                               "Error: "+ioEx.getMessage());
            return;
        }
    }

    //---------------------------------------------------------------------

    // SINGLE FILE OR THREE FILE: -f XOR (-r AND -b AND -g)
    // INT OR HUE OR SAT: -i OR -h OR -s 
    // OUTPUT: -o

    protected boolean parseArguments(String[] args)
    {
        String argKey, argValue;
        File tmpFile;

        if (args.length == 0)
        {
            _printHelp = true;
            return false;
        }

        for (int i = 0; i < args.length; ++i)
        {
            argKey = args[i];

            //---------------
            
            if (argKey.equalsIgnoreCase("-file")  ||
                argKey.equalsIgnoreCase("-f")     ||
                argKey.equalsIgnoreCase("-input") ||
                argKey.equalsIgnoreCase("-in"))
            {      
                //System.err.println("DEBUG: parseArgs: -f argKey = "+argKey+
                //                   "; argValue = "+args[i+1]);

                if (_inputFileType == MULTI_FILE)
                {
                    //ERR - cant mix types
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Cannot specify -f option with band "+
                               "image option [-r,-g,-b]");
                    return false;
                }
                _inputFileType = SINGLE_FILE;

                ++i;
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing image file parameter value");
                    return false;
                }
                
                argValue = args[i];
                
		//check that file exists
                tmpFile = new File(argValue);
                if (!tmpFile.canRead())
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Image file \'"+argValue+
                              "\' \ndoes not exist or is not readable.");
                    return false;
                }
                
                if (_uniFilenames[0] != null)
                {
                    //ERR - duplicate parameter
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Duplicate -f parameter encountered.");
                    return false;
                }
                _uniFilenames[0] = tmpFile.getAbsolutePath();
            }

            //---------------
            
            else if (
                argKey.equalsIgnoreCase("-r")     || 
                argKey.equalsIgnoreCase("-red")   ||
                argKey.equalsIgnoreCase("-g")     || 
                argKey.equalsIgnoreCase("-grn")   ||
                argKey.equalsIgnoreCase("-green") ||
                argKey.equalsIgnoreCase("-b")     || 
                argKey.equalsIgnoreCase("-blu")   ||
                argKey.equalsIgnoreCase("-blue"))
            {

                //System.err.println("DEBUG: parseArgs: -rgb argKey = "+argKey+
                //                   "; argValue = "+args[i+1]);

                if (_inputFileType == SINGLE_FILE)
                {
                    //ERR - can't mix single with multi 
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Cannot specify band image [-r,-g,-b] option "+
                               "with -f option");
                    return false;
                }
                _inputFileType = MULTI_FILE;              

                int index = 0;
                ++i;
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                          "Missing image file parameter value for "+argKey);
                    return false;
                }
                argValue = args[i];
                tmpFile = new File(argValue);
                if (!tmpFile.canRead())
                {
                    //ERR - cannot read file
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Image file \'"+argValue+
                              "\' \ndoes not exist or is not readable.");
                    return false;
                }
              
                if (argKey.startsWith("-r"))
                    index = RED_INDEX;
                else if (argKey.startsWith("-g"))
                    index = GREEN_INDEX;
                else if (argKey.startsWith("-b"))
                    index = BLUE_INDEX;

                if (_rgbFilenames[index] != null)
                {
                    //ERR - duplicate
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Duplicate "+argKey+" parameter encountered."); 
                    return false;
                }
                _rgbFilenames[index] = tmpFile.getAbsolutePath();
            }

            //---------------
            
            else if (argKey.equalsIgnoreCase("-output_file") || 
                     argKey.equalsIgnoreCase("-output") ||
                     argKey.equalsIgnoreCase("-out") ||
                     argKey.equalsIgnoreCase("-o"))
            {               
                ++i;

                //System.err.println("DEBUG: parseArgs: -o argKey = "+argKey+
                //                   "; argValue = "+args[i]);
               
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Missing output file parameter value");
                    return false;
                }
                argValue = args[i];

                if (_outFilename != null)
                {
                    //ERR -duplicate   
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Duplicate "+argKey+" parameter encountered."); 
                    return false;
                }
                tmpFile = new File(argValue);                
                _outFilename =tmpFile.getAbsolutePath();
            }
                 
            //---------------
            
            else if (args[i].equalsIgnoreCase("-min") || 
                     args[i].equalsIgnoreCase("-rangeMin") || 
                     args[i].equalsIgnoreCase("-mn"))
            {
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing range min parameter value");
                    return false;
                }

                ++i;
                
                //<DEBUG>
                //System.err.println("DEBUG: parseArgs: -min argKey = "+argKey+
                //                   "; argValue = "+args[i]);
                //</DEBUG>
               				
                argValue = args[i];
                try {
                    _rangeMinOverride = Double.parseDouble(argValue);
                } catch (Exception ex) {                    
                    //ERR
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Unable to parse range min value: "+argValue); 
                    return false;
                }
                _rangeMinOverriden = true;               
            }

            //---------------

            else if (args[i].equalsIgnoreCase("-max") || 
                     args[i].equalsIgnoreCase("-rangeMax") || 
                     args[i].equalsIgnoreCase("-mx"))
            {
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing range max parameter value");
                    return false;
                }

                ++i;
                
                //<DEBUG>
                //System.err.println("DEBUG: parseArgs: -max argKey = "+argKey+
                //                   "; argValue = "+args[i]);
                //</DEBUG>
               				
                argValue = args[i];
                try {
                    _rangeMaxOverride = Double.parseDouble(argValue);
                } catch (Exception ex) {                    
                    //ERR
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Unable to parse range max value: "+argValue); 
                    return false;
                }
                _rangeMaxOverriden = true;               
            }

            //---------------
            
            else if (args[i].equalsIgnoreCase("-intensity") || 
                     args[i].equalsIgnoreCase("-int") || 
                     args[i].equalsIgnoreCase("-i"))
            {
                ++i;

                //System.err.println("DEBUG: parseArgs: -i argKey = "+argKey+
                //                   "; argValue = "+args[i]);
               
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing intensity parameter value");
                    return false;
                }
				
                argValue = args[i];
                try {
                    _intAdjust = Double.parseDouble(argValue);
                } catch (Exception ex) {                    
                    //ERR
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Unable to parse intensity value: "+argValue); 
                    return false;
                }
                if (_intAdjust < INT_MIN || _intAdjust > INT_MAX)
                {
                    if (_intAdjust > INT_MAX)
                    {
                        _intAdjust = INT_MAX;
                    }
                    if (_intAdjust < INT_MIN)
                    {
                        _intAdjust = INT_MIN;
                    }
                    System.err.println(className_+"::parseArguments(): "+
                                       "Intensity value exceeds range.  "+
                                       "Truncated to: "+_intAdjust);
                }
                _adjustInt = true;               
            }            
                   
            //---------------
            
            else if (args[i].equalsIgnoreCase("-hue") ||
                     args[i].equalsIgnoreCase("-h"))
            {     
                i++;

                //System.err.println("DEBUG: parseArgs: -h argKey = "+argKey+
                //                   "; argValue = "+args[i]);
    
                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing hue parameter value");
                    return false;
                }
				
                argValue = args[i];
                try {
                    _hueAdjust = Double.parseDouble(argValue);
                } catch (Exception ex) {                    
                    //ERR      
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Unable to parse hue value: "+argValue); 
                    return false;
                }
                if (_hueAdjust < HUE_MIN || _hueAdjust > HUE_MAX)
                {
                    double hueWidth = HUE_MAX - HUE_MIN;
                    while (_hueAdjust > HUE_MAX)
                    {
                        _hueAdjust -= hueWidth;
                    }
                    while (_hueAdjust < HUE_MIN)
                    {
                        _hueAdjust += hueWidth;
                    }
                    System.err.println(className_+"::parseArguments(): "+
                                       "Hue value exceeds range.  "+
                                       "Reduced to: "+_hueAdjust);
                }

                _adjustHue = true;
            }
            
            //---------------
            
            else if (args[i].equalsIgnoreCase("-saturation") || 
                     args[i].equalsIgnoreCase("-sat") || 
                     args[i].equalsIgnoreCase("-s"))
            {
                i++;

                //System.err.println("DEBUG: parseArgs: -s argKey = "+argKey+
                //                   "; argValue = "+args[i]);

                if (i >= args.length)
                {
                    System.err.println(className_+"::parseArguments(): *** "+
                               "Missing saturation parameter value");
                    return false;
                }
				
                argValue = args[i];
                try {
                    _satAdjust = Double.parseDouble(argValue);
                } catch (Exception ex) {                    
                    //ERR  
                    System.err.println(className_+"::parseArguments(): *** "+
                              "Unable to parse saturation value: "+argValue); 
                    return false;
                }
                if (_satAdjust < SAT_MIN || _satAdjust > SAT_MAX)
                {
                    if (_satAdjust > SAT_MAX)
                    {
                        _satAdjust = SAT_MAX;
                    }
                    if (_satAdjust < SAT_MIN)
                    {
                        _satAdjust = SAT_MIN;
                    }
                    System.err.println(className_+"::parseArguments(): "+
                                       "Saturation value exceeds range.  "+
                                       "Truncated to: "+_satAdjust);
                }
                _adjustSat = true;
            }

            //---------------
            
            else if (args[i].equalsIgnoreCase("-help") || 
                     args[i].equalsIgnoreCase("-?"))
            {               
                //System.err.println("DEBUG: parseArgs: -help");
               
                _printHelp = true;
                return false;
            }

            //---------------
            
            else 
            {
                System.err.println(className_+"::parseArguments(): *** "+
                          "Unrecognized argument keyword: "+argKey);
                return false;
            }
        } //end_for

        //CHECK that necessary filenames were included
        if (_inputFileType != SINGLE_FILE && _inputFileType != MULTI_FILE)
        {
            //ERR - must be one or the other
            System.err.println(className_+"::parseArguments(): *** "+
                      "No input image file(s) specified.");
            return false;
        }
        else if (_inputFileType == SINGLE_FILE && _uniFilenames[0] == null)
        {
            //ERR    
            System.err.println(className_+"::parseArguments(): *** "+
                      "No output image filename specified.");
            return false;
        }
        else if (_inputFileType == MULTI_FILE)
        {
            for (int j = 0; j < _rgbFilenames.length; ++j)
            {
                if (_rgbFilenames[j] == null)
                {
                    //ERR   
                    System.err.println(className_+"::parseArguments(): *** "+
                          "Must specify three images if -rgb parameters used");
                    return false;
                }
            }
        }
        if (_outFilename == null)
        {
            //ERR  
            System.err.println(className_+"::parseArguments(): *** "+
                          "No output image filename specified.");
            return false;
        }

        if (_rangeMinOverriden && _rangeMaxOverriden)
        {
            if (_rangeMinOverride > _rangeMaxOverride)
            {
                //ERR
                System.err.println(className_+"::parseArguments(): *** "+
                                   "Override for range min must be less than"+
                                   " or equal to override for range max.");
                return false;
            }
        }

        return true;
    }
    




    //---------------------------------------------------------------------


   

    protected void convert() throws Exception
    {
        //load image(s)
        String[] imagePaths;
        if (_inputFileType == SINGLE_FILE)
            imagePaths = _uniFilenames;
        else
            imagePaths = _rgbFilenames;

        RenderedOp loadImage = loadImage(imagePaths);
        if (loadImage == null)
        {
            throw new Exception(className_+"::convert(): Could not load input "+
                                "image(s).");
        } 

        SampleModel sampleModel = loadImage.getSampleModel();
        ColorModel  colorModel  = loadImage.getColorModel();
        int numBands = sampleModel.getNumBands();
        int dataType = sampleModel.getDataType();
        boolean floatType = false;

        double dataTypeMin, dataTypeMax, dataTypeRange;

        switch (dataType)
        {
            case DataBuffer.TYPE_BYTE:
                 dataTypeMin =      0.0;
                 dataTypeMax =    255.0;
                 floatType   =    false;
                 break;                
            case DataBuffer.TYPE_SHORT:
                //dataTypeMin = (double) 0;  
                 dataTypeMin = -32768.0;
                 dataTypeMax =  32767.0;
                 floatType   =    false;
                 break;
            case DataBuffer.TYPE_USHORT:
                 dataTypeMin =      0.0;
                 dataTypeMax =  65535.0;
                 floatType   =    false;
                 break;
            case DataBuffer.TYPE_INT:
                 //dataTypeMin =      0.0;
                 dataTypeMin = (double) Integer.MIN_VALUE;
                 dataTypeMax = (double) Integer.MAX_VALUE;
                 floatType   =    false;
                 break;
            case DataBuffer.TYPE_FLOAT:
                 dataTypeMin =      0.0;
                 dataTypeMax =      1.0;
                 floatType   =     true;
                 break;
            case DataBuffer.TYPE_DOUBLE:  
                 dataTypeMin =      0.0;
                 dataTypeMax =      1.0;
                 floatType   =     true;
                 break;
            default:
                 dataTypeMin =      0.0;
                 dataTypeMax =    255.0;
                 floatType   =    false;
        }

        //override primite ranges with user entries
        if (_rangeMinOverriden)
            dataTypeMin = _rangeMinOverride;
        if (_rangeMaxOverriden)
            dataTypeMax = _rangeMaxOverride;

        if (dataTypeMin >= dataTypeMax)
        {
            throw new IllegalArgumentException(className_+"::convert(): "+
                   "Data type range is invalid.  MIN = "+dataTypeMin+", "+
                   "MAX = "+dataTypeMax+"");
        }
        
        dataTypeRange = dataTypeMax - dataTypeMin;

        if (DEBUG1)
        {
            System.err.println("DEBUG:: Load Image Width  = "+loadImage.getWidth());
            System.err.println("DEBUG:: Load Image Height = "+loadImage.getHeight());
            System.err.println("DEBUG:: Load Image Sample Model  = "+sampleModel);
            System.err.println("DEBUG:: Load Image Data   Type   = "+sampleModel.getDataType());    
            System.err.println("DEBUG:: Load Image Sample Width  = "+sampleModel.getWidth());
            System.err.println("DEBUG:: Load Image Sample Height = "+sampleModel.getHeight());
            System.err.println("DEBUG:: Load Image Colour Model  = "+colorModel);
            System.err.println();
        }
        

        if (DEBUG2)
        {
            // <DEBUG>        
            System.err.print("LOAD IMAGE: ");
            Raster rawData = loadImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print("  (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(rawData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }
            System.err.println();            
            // </DEBUG>
        }

        //-------------------------------

        //RESCALE - seemingly required for formatting purposed...hmm.
        ParameterBlockJAI rescalePB = new ParameterBlockJAI("rescale");
        rescalePB.setSource(loadImage, 0);
        rescalePB.setParameter("constants", new double[] {1.0});
        RenderedOp rescaleImage = JAI.create("rescale", rescalePB);
        
        //-------------------------------

        //format change to DOUBLE
        
        ParameterBlockJAI formatPB = new ParameterBlockJAI("format");
        formatPB.setSource(rescaleImage, 0);
        formatPB.setParameter("dataType", DataBuffer.TYPE_DOUBLE);
        RenderedOp formatImage = JAI.create("format", formatPB);        

        //-------------------------------

        //RESCALE to range 0-1
        double diff = dataTypeRange;
        if (diff == 0.0)
            diff = 0.00001;
        double constant01 =  1.0 / diff;
        double offset01   = -1.0 * dataTypeMin * constant01;
        ParameterBlockJAI rescale01PB = new ParameterBlockJAI("rescale");
        rescale01PB.setSource(formatImage, 0);
        rescale01PB.setParameter("constants", new double[] {constant01});
        rescale01PB.setParameter("offsets", new double[] {offset01});      
        RenderedOp rescale01Image = JAI.create("rescale", rescale01PB);

        //-------------------------------

        //RenderedOp formattedImage = _formatImage;
        RenderedOp formattedImage = rescale01Image;

        //-------------------------------

        ColorModel formatColorModel = formattedImage.getColorModel();
        
        if (DEBUG1)
        {
            System.err.println("DEBUG:: Format Image Width  = "+formattedImage.getWidth());
            System.err.println("DEBUG:: Format Image Height = "+formattedImage.getHeight());
            System.err.println("DEBUG:: Format Image Sample Model  = "+formattedImage.getSampleModel()); 
            System.err.println("DEBUG:: Format Image Data   Type   = "+formattedImage.getSampleModel().getDataType()); 
            System.err.println("DEBUG:: Format Image Sample Width  = "+formattedImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: Format Image Sample Height = "+formattedImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: Format Image Colour Model  = "+formattedImage.getColorModel());
            System.err.println();
        }

        ImageLayout ccLayout = new ImageLayout();
        ccLayout.setSampleModel(formattedImage.getSampleModel());
        RenderingHints ccHint = new RenderingHints(JAI.KEY_IMAGE_LAYOUT,
                                                   ccLayout);
        //ccHint = null;
       
        //verify that image has three bands
        if (numBands != 3)
        {
            throw new Exception(className_+"::convert(): Image must have "+
                                "3 bands or be formed from 3 single-banded "+
                                "images.  Found "+numBands+" bandes.");
        }

        //determine hue,int,sat increments
        adjustDeltas(dataTypeMin, dataTypeMax);

        ComponentColorModel ihsColorModel = new ComponentColorModel(
                                            IHSColorSpace.getInstance(),
                                            new int[] {64,64,64},
                                            false, false,
                                            Transparency.OPAQUE,
                                            DataBuffer.TYPE_DOUBLE);


        //convert formatted image to IHS color space 
        ParameterBlockJAI ihsPB = new ParameterBlockJAI("colorconvert");
        ihsPB.setParameter("colorModel", ihsColorModel);
        ihsPB.setSource(formattedImage, 0);
        RenderedOp ihsImage = JAI.create("colorconvert", ihsPB, ccHint);

        // <DEBUG>
        if (DEBUG2)
        {
            System.err.print("Color Convert Image: ");
            Raster ccData = ihsImage.getData(); 
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print(" (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(ccData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }           
            System.err.println();
            
            // </DEBUG>
        }

        /*
        JScrollPane curDisplay = JadeDisplay.createDisplay(_ihsImage,0,0,true);
        JFrame frame = new JFrame("Jade Display");
        frame.getContentPane().add(curDisplay);frame.pack();frame.setVisible(true);
        */
                
        if (DEBUG1) 
        {
            System.err.println("DEBUG:: IHS Image Width  = "+ihsImage.getWidth());
            System.err.println("DEBUG:: IHS Image Height = "+ihsImage.getHeight());
            System.err.println("DEBUG:: IHS Image Sample Model  = "+ihsImage.getSampleModel());   
            System.err.println("DEBUG:: IHS Image Data   Type   = "+ihsImage.getSampleModel().getDataType());
            System.err.println("DEBUG:: IHS Image Sample Width  = "+ihsImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: IHS Image Sample Height = "+ihsImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: IHS Image Colour Model  = "+ihsImage.getColorModel());
            System.err.println();
        }

        //-------------------------------

        //extract hue, satuation, and intensity
        ParameterBlockJAI intPB = new ParameterBlockJAI("bandselect");
        intPB.setParameter("bandIndices", new int[] {0});
        intPB.setSource(ihsImage, 0);
        RenderedOp intImage = JAI.create("bandselect", intPB);
        

        if (DEBUG1)
        {
            System.err.println("DEBUG:: INT Image Width  = "+intImage.getWidth());
            System.err.println("DEBUG:: INT Image Height = "+intImage.getHeight());
            System.err.println("DEBUG:: INT Image Sample Model  = "+intImage.getSampleModel());   
            System.err.println("DEBUG:: INT Image Data   Type   = "+intImage.getSampleModel().getDataType());     
            System.err.println("DEBUG:: INT Image Sample Width  = "+intImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: INT Image Sample Height = "+intImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: INT Image Colour Model  = "+intImage.getColorModel());
            System.err.println();
        }

        //-------------------------------

        ParameterBlockJAI huePB = new ParameterBlockJAI("bandselect");
        huePB.setParameter("bandIndices", new int[] {1});
        huePB.setSource(ihsImage, 0);
        RenderedOp hueImage = JAI.create("bandselect", huePB);



        if (DEBUG1) 
        {
            System.err.println("DEBUG:: HUE Image Width  = "+hueImage.getWidth());
            System.err.println("DEBUG:: HUE Image Height = "+hueImage.getHeight());
            System.err.println("DEBUG:: HUE Image Sample Model = "+hueImage.getSampleModel());
            System.err.println("DEBUG:: HUE Image Colour Model = "+hueImage.getColorModel());
            System.err.println();
        }
    
        //-------------------------------
    
        ParameterBlockJAI satPB = new ParameterBlockJAI("bandselect");
        satPB.setParameter("bandIndices", new int[] {2});
        satPB.setSource(ihsImage, 0);
        RenderedOp satImage = JAI.create("bandselect", satPB);
        
        if (DEBUG1)
        {
            System.err.println("DEBUG:: SAT Image Width  = "+satImage.getWidth());
            System.err.println("DEBUG:: SAT Image Height = "+satImage.getHeight());
            System.err.println("DEBUG:: SAT Image Sample Model = "+satImage.getSampleModel());
            System.err.println("DEBUG:: SAT Image Colour Model = "+satImage.getColorModel());
            System.err.println();
        }

        //-------------------------------

        ParameterBlockJAI intAddPB, intClampPB, intPrimePB;
        RenderedOp intAddImage = null;
        RenderedOp intClampImage = null;
        RenderedOp intPrimeImage = null;
        
        if (_adjustInt)
        {
            intAddPB	= new ParameterBlockJAI("addconst");
            intAddPB.setSource(intImage, 0);
            intAddPB.setParameter("constants", 
                   new double[] {_intDelta});  
            intAddImage = JAI.create("addconst", intAddPB);

            intClampPB	= new ParameterBlockJAI("clamp");
            intClampPB.setSource(intAddImage, 0);
            intClampPB.setParameter("low", 
                   new double[] {0.0});  
            intClampPB.setParameter("high", 
                   new double[] {dataTypeMax});
            intClampImage = JAI.create("clamp", intClampPB);

            intPrimePB	= new ParameterBlockJAI("rescale");
            intPrimePB.setSource(intClampImage, 0);
            intPrimePB.setParameter("constants", 
                                     new double[] {1.0 / 1.0});  
            intPrimeImage = JAI.create("rescale", intPrimePB);  
        }
        else
        {
            intPrimePB	= new ParameterBlockJAI("rescale");
            intPrimePB.setSource(intImage, 0);
            intPrimePB.setParameter("constants", 
                   new double[] {1.0 / 1.0});  
            intPrimeImage = JAI.create("rescale", intPrimePB); 
        }

        //-------------------------------

        ParameterBlockJAI hueAddPB,hueModPB, hueClampPB, huePrimePB;
        RenderedOp hueAddImage   = null;
        RenderedOp hueModImage   = null;
        RenderedOp hueClampImage = null; 
        RenderedOp huePrimeImage = null;
       

        if (_adjustHue)
        {
            hueAddPB =  new ParameterBlockJAI("addConst");
            hueAddPB.setSource(hueImage, 0);
            hueAddPB.setParameter("constants", 
                   new double[] {_hueDelta});
            hueAddImage = JAI.create("addconst", hueAddPB);
            
            hueModPB =  new ParameterBlockJAI("modulus");
            hueModPB.setSource(hueAddImage, 0);
            hueModPB.setParameter("range_min", 
                   new double[] {0.0});
            hueModPB.setParameter("range_max", 
                   new double[] {2.0 * Math.PI});
            hueModImage = JAI.create("modulus", hueModPB);
            
            hueClampPB	= new ParameterBlockJAI("clamp");
            hueClampPB.setSource(hueModImage, 0);
            hueClampPB.setParameter("low", 
                   new double[] {0.0});  
            hueClampPB.setParameter("high", 
                   new double[] {2.0 * Math.PI});
            hueClampImage = JAI.create("clamp", hueClampPB);

            huePrimePB =  new ParameterBlockJAI("threshold");
            huePrimePB.setSource(hueClampImage, 0);
            huePrimePB.setParameter("low", 
                        new double[] {(Math.PI*4.0/3.0)});
            huePrimePB.setParameter("high", 
                        new double[] {(Math.PI*4.0/3.0)}); 
            huePrimePB.setParameter("constants", 
                        new double[] {(Math.PI*4.0/3.0)+.00000001}); 
            huePrimeImage = JAI.create("threshold", huePrimePB);
        }
        else
        { 
            huePrimePB =  new ParameterBlockJAI("threshold");
            huePrimePB.setSource(hueImage, 0);
            huePrimePB.setParameter("low", 
                        new double[] {(Math.PI*4.0/3.0)});
            huePrimePB.setParameter("high", 
                        new double[] {(Math.PI*4.0/3.0)}); 
            huePrimePB.setParameter("constants", 
                        new double[] {(Math.PI*4.0/3.0)+.000000001});
            huePrimeImage = JAI.create("threshold", huePrimePB);
        }

        //-------------------------------

        ParameterBlockJAI satAddPB, satClampPB, satPrimePB;
        RenderedOp satAddImage = null; 
        RenderedOp satClampImage = null;
        RenderedOp satPrimeImage = null;        
        if (_adjustSat)
        {        
            satAddPB =  new ParameterBlockJAI("addconst");
            satAddPB.setSource(satImage, 0);
            satAddPB.setParameter("constants", 
                           new double[] {_satDelta});
            satAddImage = JAI.create("addconst", satAddPB);

            satClampPB =  new ParameterBlockJAI("clamp");
            satClampPB.setSource(satAddImage, 0);
            satClampPB.setParameter("low", 
                           new double[] {0.0}); 
            satClampPB.setParameter("high", 
                           new double[] {1.0});
            satClampImage = JAI.create("clamp", satClampPB);

            satPrimePB =  new ParameterBlockJAI("rescale");
            satPrimePB.setSource(satClampImage, 0);
            satPrimePB.setParameter("constants", 
                          new double[] {1.0});
            satPrimeImage = JAI.create("rescale", satPrimePB);
        }
        else
        {    
            satPrimePB =  new ParameterBlockJAI("rescale");
            satPrimePB.setSource(satImage, 0);
            satPrimePB.setParameter("constants", 
                          new double[] {1.0});
            satPrimeImage = JAI.create("rescale", satPrimePB);
            satPrimeImage = satImage;
        }       

        // <DEBUG>
        if (DEBUG2)
        {
            System.err.print("IHS Component Image: ");
            Raster intData = intImage.getData();
            Raster hueData = hueImage.getData();
            Raster satData = satImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print(" ("+intData.getSampleDouble(i,j,0)+","+
                                     hueData.getSampleDouble(i,j,0)+","+
                                     satData.getSampleDouble(i,j,0)+")");
                }
            System.err.println();
            
            // </DEBUG>
        }

        if (DEBUG2)
        {
            // <DEBUG>
            
            System.err.print("\nIHS ADD Component Image: ");
            Raster intAData, hueAData, satAData;
            if (_adjustInt)
                intAData = intAddImage.getData();
            else
                intAData = intImage.getData();
            if (_adjustHue)
                hueAData = hueModImage.getData();
            else
                hueAData = hueImage.getData();   
            if (_adjustSat)
                satAData = satAddImage.getData();
            else
                satAData = satImage.getData();
            
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print(" ("+intAData.getSampleDouble(i,j,0)+","+
                                     hueAData.getSampleDouble(i,j,0)+","+
                                     satAData.getSampleDouble(i,j,0)+")");
                }
            System.err.println();
            
            // </DEBUG>
        }

        if (DEBUG2)
        {
            // <DEBUG>            
            System.err.print("IHS Prime Component Image: ");
            Raster intPData = intPrimeImage.getData();
            Raster huePData = huePrimeImage.getData();
            Raster satPData = satPrimeImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print(" ("+intPData.getSampleDouble(i,j,0)+","+
                                     huePData.getSampleDouble(i,j,0)+","+
                                     satPData.getSampleDouble(i,j,0)+")");
                }
            System.err.println();
        }
        // </DEBUG>

        /*
        System.err.println("DEBUG:: Writing INT image...");
        writeImage(_intImage, prefix+"tstIntImage.IMG");
        System.err.println("DEBUG:: Writing HUE image...");
        writeImage(_hueImage, prefix+"tstHueImage.IMG");
        System.err.println("DEBUG:: Writing SAT image...");
        writeImage(_satImage, prefix+"tstSatImage.IMG");
        */
        //-------------------------------

        //merge into (IHS)'
        ImageLayout ihsLayout = new ImageLayout();
        ihsLayout.setColorModel(ihsColorModel);
        RenderingHints layoutHint = new RenderingHints(JAI.KEY_IMAGE_LAYOUT,
                                         ihsLayout);

        ParameterBlockJAI ihsMergePB = new ParameterBlockJAI("bandmerge");
        ihsMergePB.setSource(intPrimeImage, 0);
        ihsMergePB.setSource(huePrimeImage, 1);
        ihsMergePB.setSource(satPrimeImage, 2);
        RenderedOp ihsMergeImage = JAI.create("bandmerge", ihsMergePB, layoutHint);
        
        if (DEBUG2)
        {
            // <DEBUG>
           
            System.err.print("\nIHS MERGE IMAGE: ");
            Raster ihsmData = ihsMergeImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print("  (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(ihsmData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }
            System.err.println();
        }
        // </DEBUG>

        if (DEBUG1)
        {
            System.err.println("DEBUG:: IHS Merge Image Width  = "+ihsMergeImage.getWidth());
            System.err.println("DEBUG:: IHS Merge Image Height = "+ihsMergeImage.getHeight());
            System.err.println("DEBUG:: IHS Merge Image Sample Model  = "+ihsMergeImage.getSampleModel());        
            System.err.println("DEBUG:: IHS Merge Image Data   Type   = "+ihsMergeImage.getSampleModel().getDataType());
            System.err.println("DEBUG:: IHS Merge Image Sample Width  = "+ihsMergeImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: IHS Merge Image Sample Height = "+ihsMergeImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: IHS Merge Image Colour Model  = "+ihsMergeImage.getColorModel());
            System.err.println();
        }

        //convert (IHS)' to (RGB)'
        ParameterBlockJAI rgbPB = new ParameterBlockJAI("colorconvert");
        
        /*
        _rgbPB.setParameter("colorModel", new ComponentColorModel(
                                   ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                   /* new int[] {_colorModelBits,
                                              _colorModelBits,
                                              _colorModelBits}, * / //
                                   new int[] {8,8,8},
                                   false, false,
                                   Transparency.OPAQUE,
                                   //dataType)); //
                                   DataBuffer.TYPE_DOUBLE));
        */

        rgbPB.setParameter("colorModel", formatColorModel);
        rgbPB.setSource(ihsMergeImage, 0);
        RenderedOp rgbImage = JAI.create("colorconvert", rgbPB);

        // <DEBUG>
        if (DEBUG2)
        {
            System.err.print("\nRGB IMAGE: ");
            Raster rgbData = rgbImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print("  (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(rgbData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }
            System.err.println();            
            // </DEBUG>
        }
        
        if (DEBUG1)
        {
            System.err.println("DEBUG:: RGB Image Width  = "+rgbImage.getWidth());
            System.err.println("DEBUG:: RGB Image Height = "+rgbImage.getHeight());
            System.err.println("DEBUG:: RGB Image Sample Model  = "+rgbImage.getSampleModel());
            System.err.println("DEBUG:: RGB Image Data   Type   = "+rgbImage.getSampleModel().getDataType());
            System.err.println("DEBUG:: RGB Image Sample Width  = "+rgbImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: RGB Image Sample Height = "+rgbImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: RGB Image Colour Model  = "+rgbImage.getColorModel());
            System.err.println();
        }

        //-------------------------------

        ParameterBlockJAI rescaleBackPB = new ParameterBlockJAI("rescale");
        rescaleBackPB.setSource(rgbImage, 0);
        //System.err.println("\nDEBUG:: Range max = "+dataTypeMax+"\n");        
        rescaleBackPB.setParameter("constants", new double[] {dataTypeRange});
        rescaleBackPB.setParameter("offsets", new double[] {dataTypeMin});       
        RenderedOp rescaleBackImage = JAI.create("rescale", rescaleBackPB);

        // <DEBUG>
        if (DEBUG2)
        {
            System.err.print("RESCALE BACK IMAGE: ");
            Raster rbData = rescaleBackImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print("  (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(rbData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }
            System.err.println();
        }
        // </DEBUG>

        //-------------------------------

        ParameterBlockJAI rescaleRoundPB = new ParameterBlockJAI("rescale");
        rescaleRoundPB.setSource(rescaleBackImage, 0);
        if (!floatType)
            rescaleRoundPB.setParameter("offsets", new double[] {0.5});        
        RenderedOp rescaleRoundImage = JAI.create("rescale", rescaleRoundPB);

        //-------------------------------
        
        ImageLayout fbLayout = new ImageLayout();
        fbLayout.setSampleModel(sampleModel);
        RenderingHints fbHint = new RenderingHints(JAI.KEY_IMAGE_LAYOUT,
                                                   fbLayout);

        ParameterBlockJAI formatBackPB = new ParameterBlockJAI("format");
        formatBackPB.setSource(rescaleRoundImage, 0);
        formatBackPB.setParameter("dataType", dataType);
        RenderedOp formatBackImage = JAI.create("format", formatBackPB, fbHint);

        // <DEBUG>
        if (DEBUG2)
        {
            System.err.print("FORMAT BACK IMAGE: ");
            Raster fbData = formatBackImage.getData();
            for (int i = 150; i < 152; ++i)       
                for (int j = 90; j < 91; ++j)
                {
                    System.err.print("  (");
                    for (int k = 0; k < 3; ++k)    
                    {                
                        System.err.print(fbData.getSampleDouble(i,j,k)+" ");
                    }
                    System.err.print(")");
                }
            System.err.println();
        }
        // </DEBUG>

        _finalImage = formatBackImage;


        //clamp values close to 0.0 -> 0 and close to 1 -> 1
        if (floatType)
        {
            ParameterBlockJAI threshold1PB = new ParameterBlockJAI("threshold");
            threshold1PB.setSource(formatBackImage, 0);
            threshold1PB.setParameter("low", new double[] {0.0});
            threshold1PB.setParameter("high", new double[] {0.000025});
            threshold1PB.setParameter("constants", new double[] {0.0});
            RenderedOp threshold1Image = JAI.create("threshold", threshold1PB, null);
            
            ParameterBlockJAI threshold2PB = new ParameterBlockJAI("threshold");
            threshold2PB.setSource(threshold1Image, 0);
            threshold2PB.setParameter("low", new double[] {0.999975});
            threshold2PB.setParameter("high", new double[] {1.0});
            threshold2PB.setParameter("constants", new double[] {1.0});
            RenderedOp threshold2Image = JAI.create("threshold", threshold2PB, null);
        
            _finalImage = threshold2Image;
        }
       
        
        if (DEBUG1)
        {
            System.err.println("DEBUG:: Final Image Width  = "+_finalImage.getWidth());
            System.err.println("DEBUG:: Final Image Height = "+_finalImage.getHeight());
            System.err.println("DEBUG:: Final Image Sample Model  = "+_finalImage.getSampleModel());
            System.err.println("DEBUG:: Final Image Data   Type   = "+_finalImage.getSampleModel().getDataType());
            System.err.println("DEBUG:: Final Image Sample Width  = "+_finalImage.getSampleModel().getWidth());
            System.err.println("DEBUG:: Final Image Sample Height = "+_finalImage.getSampleModel().getHeight());
            System.err.println("DEBUG:: Final Image Colour Model  = "+_finalImage.getColorModel());
            System.err.println();
        }       
    } 

    //---------------------------------------------------------------------

    protected void writeImage()
    {
        writeImage(_finalImage, _outFilename);
    }

    //---------------------------------------------------------------------

    protected void writeImage(RenderedImage image, String filepath)
    {               
        int index = filepath.indexOf(".");
        if (index == -1)
            filepath = filepath + ".IMG";                   
        
        JAI.create("imagewrite", image, filepath);
    }

    //---------------------------------------------------------------------  

    //loads images such that, if one image, loads, if
    //three images, loads all of em and combines.

    protected RenderedOp loadImage(String[] images)
    {
        int numImages = images.length;

        File file;
        ParameterBlockJAI loadPB = new ParameterBlockJAI("fileload");
        RenderedOp[] loadedImages = new RenderedOp[numImages];
        RenderedOp returnImage;

        for (int i = 0; i < numImages; ++i)
        {
            file = (new File(images[i])).getAbsoluteFile();
            if (!file.canRead())
            {
                //ERR
                System.err.println(className_+"::loadImage: *** "+
                          "Can not read file "+images[i]);
                return null;
            }

            loadPB.setParameter("filename", file.getAbsolutePath());
            loadedImages[i] = JAI.create("fileload", loadPB); 
        }
        
        if (numImages == 1)
        {
            returnImage = loadedImages[0];
        }
        else
        {
           ParameterBlockJAI mergePB = new ParameterBlockJAI("bandmerge");
           for (int i = 0; i < numImages; ++i)          
               mergePB.setSource(loadedImages[i], i);
           returnImage = JAI.create("bandmerge", mergePB, null);           
        }

        //System.err.println("DEBUG: loadImage returning "+returnImage);
        return returnImage;
    }

    //---------------------------------------------------------------------

    //Range is assumed to be for double [0.0, 1.0]

    protected void adjustDeltas(double dataTypeMin, double dataTypeMax)
    {            
        double constant = (double) 1.0;
        double offset   = 0.0;

        //double INT_RANGE = dataTypeMax;
        double HUE_RANGE = 2.0 * Math.PI;
        double SAT_RANGE = 1.0;
        double INT_RANGE = 1.0;

        _hueDelta = ((_hueAdjust / HUE_MAX) * constant) + offset;
        _satDelta = ((_satAdjust / SAT_MAX) * constant) + offset;    
        _intDelta = ((_intAdjust / INT_MAX) * constant) + offset;

        if (_hueDelta >= 0)
            _hueDelta = (_hueDelta * Math.PI);
        else
            _hueDelta = (2.0 * Math.PI) + (_hueDelta * Math.PI);
        
        if (_satDelta >= 0)
            _satDelta = (_satDelta * SAT_RANGE);
        else
            _satDelta = (_satDelta * SAT_RANGE);   

        if (_intDelta >= 0)
            _intDelta = (_intDelta * INT_RANGE);
        else
            _intDelta = (_intDelta * INT_RANGE);
                
        //System.err.println("DEBUG:: Int  Delta = "+_intDelta);
        //System.err.println("DEBUG:: Hue  Delta = "+_hueDelta);
        //System.err.println("DEBUG:: Sat  Delta = "+_satDelta);
    }


    //---------------------------------------------------------------------
    //---------------------------------------------------------------------
    //---------------------------------------------------------------------

    public static void main(String[] args)
    {
        SimpleColorConverter converter = new SimpleColorConverter(args);
    }

    //---------------------------------------------------------------------
    //---------------------------------------------------------------------

}
