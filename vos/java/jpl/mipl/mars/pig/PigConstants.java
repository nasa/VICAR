package jpl.mipl.mars.pig;

/**
 * Constants for the Java PIG library
 *
 * @author Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
 * @version $Id: $
 *
 */
public interface PigConstants
{
    //---------------------------------------------------------------------
    //Library constants
    
    public static final String VERSION = "0.1.1";   // Jul 05, 2016
    public static final String NAME    = "jPIG";
    public static final String NAME_LONG    = "Java implementation of Planetry Imaging Graphics";


    //---------------------------------------------------------------------
    //Useful constants
    public static final int TYPE_LINE        = 0;
    public static final int TYPE_SAMPLE      = 1;

    
    //---------------------------------------------------------------------
    //CAMERA MODEL TYPE STRINGS
    public static final String MODELTYPE_CAHV     =  "CAHV";
    public static final String MODELTYPE_CAHVOR   =  "CAHVOR";
    public static final String MODELTYPE_CAHVORE  =  "CAHVORE";
    
    //CAHVORE model types
    public static final int CAHVORE_PUPILTYPE_PERSPECTIVE  = 1;
    public static final int CAHVORE_PUPILTYPE_FISHEYE      = 2;
    public static final int CAHVORE_PUPILTYPE_GENERAL      = 3;
    
    public static final int CAHVORE_LINEARITY_PERSPECTIVE  = 1;
    public static final int CAHVORE_LINEARITY_FISHEYE      = 0;
    
    //---------------------------------------------------------------------
    //Image metadata/label strings
    
    public static final String LABEL_PDSLABEL   =  "PDS_LABEL";
    
    
    //LABEL NAMES
    public static final String LABEL_GEOMODEL   =  "GEOMETRIC_CAMERA_MODEL";
    public static final String LABEL_MODELTYPE  =  "MODEL_TYPE";
    
    //CAMERA MODEL COMPONENTS 
    public static final String LABEL_MODELCOMP1 =  "MODEL_COMPONENT_1";
    public static final String LABEL_MODELCOMP2 =  "MODEL_COMPONENT_2";
    public static final String LABEL_MODELCOMP3 =  "MODEL_COMPONENT_3";
    public static final String LABEL_MODELCOMP4 =  "MODEL_COMPONENT_4";
    public static final String LABEL_MODELCOMP5 =  "MODEL_COMPONENT_5";
    public static final String LABEL_MODELCOMP6 =  "MODEL_COMPONENT_6";
    public static final String LABEL_MODELCOMP7 =  "MODEL_COMPONENT_7";
    public static final String LABEL_MODELCOMP8 =  "MODEL_COMPONENT_8";
    public static final String LABEL_MODELCOMP9 =  "MODEL_COMPONENT_9";
    
    public static final String LABEL_REFFRAMENAME =  "REFERENCE_COORD_SYSTEM_NAME";
    
    //---------------------------------------------------------------------
    //Java system properties
    
    public static final String PROPERTY_PIG_DEBUG_ENABLED = "pig.debug.enabled";
    
    //---------------------------------------------------------------------
    //numerical constants
    
    //Epsilon for error
    public static final double EPSILON = 1.0e-15;
    
}
