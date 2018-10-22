package jpl.mipl.mars.pig;

import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;

import javax.imageio.IIOImage;
import javax.imageio.metadata.IIOMetadata;
import javax.media.jai.PlanarImage;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import jpl.mipl.io.plugins.PDSMetadata;
import jpl.mipl.io.plugins.VicarMetadata;
import jpl.mipl.io.util.DOMutils;
import jpl.mipl.io.vicar.VicarLabel;
import jpl.mipl.io.vicar.VicarLabelCategory;
import jpl.mipl.io.vicar.VicarLabelItem;
import jpl.mipl.io.vicar.VicarLabelSet;
import jpl.mipl.mars.pig.util.IIOImageUtils;

/***********************************************************************
 * Factory class containing static methods to create camera models.
 *
 * @author Bob Deen, JPL
 * @author Nicholas Toole, JPL
 */

public class PigCameraModelFactory implements PigConstants
{
    protected static boolean debug = false;
    
    //check if debug was enabled at runtime
    static {
        String propVal = System.getProperty(PigConstants.PROPERTY_PIG_DEBUG_ENABLED);
        if (propVal != null)
            debug = Boolean.parseBoolean(propVal);
    }
    
    
    //---------------------------------------------------------------------
    
    /**
     * Creates an appropriate camera model from the metadata in the given Object,
     * which can be anything ImageIO will accept (String filename, File, URL,
     * URI).  Also accepts IIOImages.
     *
     * @return null if model can't be created for any reason
     */
    public static PigCameraModel create(Object source)
    {
        String location = null;
        IIOImage iioImage = null;
        
        if (source instanceof String)
            location = source.toString();
        else if (source instanceof File)
            location = ((File) source).getAbsolutePath();
        else if (source instanceof URL)
            location = ((URL) source).toExternalForm();
        else if (source instanceof URI)
            location = ((URI) source).toString();
        else if (source instanceof InputStream)
        {
            iioImage = IIOImageUtils.loadImage((InputStream) source);
        }
        else if (source instanceof PlanarImage)
        {
            return createFromPlanarImage((PlanarImage) source);
        }
        else if (source instanceof IIOImage)
        {
            iioImage = (IIOImage) source;
        }
        
        // use the location string based loader
        if (iioImage == null)
        {
            if (location == null)
                return null;
        
            iioImage = IIOImageUtils.loadImage(location);        
        }
        
        if (iioImage == null)
            return null;
        
        final IIOMetadata meta = iioImage.getMetadata();
        
        if (meta == null)
            return null;
        
        return createFromIioMetadata(meta);        
    }

    //---------------------------------------------------------------------
    
    /**
     * Creates an appropriate camera model from the metadata of the an IIOImage.  
     * <p>
     * Currently supported formats are:  VICAR, PDS
     *
     * @return null if model can't be created for any reason
     */
    public static PigCameraModel createFromIioMetadata(IIOMetadata metadata)
    {
        if (metadata == null)
            return null;
        
        if (metadata instanceof VicarMetadata) 
        {
            if (debug) System.out.println("got VICAR");    //!!!!
            VicarMetadata vicarMetadata = (VicarMetadata) metadata;
            VicarLabel vlbl = vicarMetadata.getVicarLabel();          
            
            if (vlbl == null)
                return null;
            
            return createFromVicarLabel(vlbl);
        }
    
        if (metadata instanceof PDSMetadata) 
        {
            if (debug) System.out.println("got PDS");  //!!!!
            PDSMetadata pdsMetadata = (PDSMetadata) metadata;
            
            Node pdsLabelRoot = pdsMetadata.getAsTree(LABEL_PDSLABEL);
            
            if (pdsLabelRoot == null)
                return null;            
            return createFromPdsLabel(pdsLabelRoot);            
        }

        return null;
    }
    
    //---------------------------------------------------------------------
    
    /**
     * Creates an appropriate camera model from the metadata of the open ImageRead
     * operator (expressed as a PlanarImage).  Looks at the file type and figures
     * out where to go from there to read the metadata.
     * <p>
     * Currently supported formats are:  VICAR, PDS
     *
     * @return null if model can't be created for any reason
     */
    public static PigCameraModel createFromPlanarImage(PlanarImage img)
    {
    	if (img == null)
    	    return null;

    	IIOMetadata metadata = null;
    	
    	Object prop = img.getProperty("JAI.ImageMetadata");    	
    	if (prop != null  && (prop instanceof IIOMetadata))    	
    	{
    	    metadata = (IIOMetadata) prop;
    	}
    	    
    	if (metadata == null)
    	    return null;
    	
    	return createFromIioMetadata(metadata);
    	
    }
    
    //---------------------------------------------------------------------

    /**
     * Creates an appropriate camera model from the given VicarLabel object.
     * Not really intended for outside access (it's called by createFromImage())
     * but it's declared public just in case it might be useful.
     */
    public static PigCameraModel createFromVicarLabel(VicarLabel vlbl)
    {
        VicarLabelCategory vlcat = vlbl.getProperty();
        if (vlcat == null)
            return null;
        VicarLabelSet vlset = vlcat.getSet(LABEL_GEOMODEL, 0);
        if (vlset == null)
            return null;

        VicarLabelItem item = vlset.getItem(LABEL_MODELTYPE);
        if (item == null)
            return null;
        String type = item.getString().toUpperCase();

        // Retrieve the parameters

        PigPoint c = null;
        PigVector a = null, h = null, v = null, o = null, r = null, e = null;
        int t = PigCoreCAHVORE.DEFAULT_PUPIL_TYPE;
        double p = PigCoreCAHVORE.DEFAULT_PUPIL_PARAM;
        
        if (type.startsWith(MODELTYPE_CAHV)) 
        {
            c = (PigPoint)getPigVectorVicar(vlset, LABEL_MODELCOMP1);
            a =           getPigVectorVicar(vlset, LABEL_MODELCOMP2);
            h =           getPigVectorVicar(vlset, LABEL_MODELCOMP3);
            v =           getPigVectorVicar(vlset, LABEL_MODELCOMP4);
            if (c==null || a==null || h==null || v==null)
                return null;
        }
        if (type.startsWith(MODELTYPE_CAHVOR)) 
        {
            o = getPigVectorVicar(vlset, LABEL_MODELCOMP5);
            r = getPigVectorVicar(vlset, LABEL_MODELCOMP6);
            if (o==null || r==null)
                return null;
        }
        if (type.startsWith(MODELTYPE_CAHVORE)) 
        {
            e = getPigVectorVicar(vlset, LABEL_MODELCOMP7);
            if (e==null)
                return null;
            item = vlset.getItem(LABEL_MODELCOMP8);
            if (item == null)
                return null;
            t = item.getInteger();
            item = vlset.getItem(LABEL_MODELCOMP9);
            if (item == null)
                return null;
            p = item.getDouble();
        }

        // Now create and fill in the proper camera model object

        if (type.equals(MODELTYPE_CAHV))
            return new PigCoreCAHV(c, a, h, v);
        if (type.equals(MODELTYPE_CAHVOR))
            return new PigCoreCAHVOR(c, a, h, v, o, r);
        if (type.equals(MODELTYPE_CAHVORE))
            return new PigCoreCAHVORE(c, a, h, v, o, r, e, t, p); 
        

        return null;

    }


    //---------------------------------------------------------------------
    
    /**
     * Creates an appropriate camera model from the given PDS label DOM node.
     * Not really intended for outside access (it's called by createFromImage())
     * but it's declared public just in case it might be useful.
     */
    public static PigCameraModel createFromPdsLabel(Node root)
    {
        PigPoint  cPoint = null;
        PigVector aVect, hVect, vVect, oVect, rVect;
        PigVector e1Vect, e2Vect, e3Vect;
        
        DOMutils domUtils = new DOMutils();

        String  geoCamXpath = getItemXpath(LABEL_GEOMODEL, null);
        Node camModNode =  domUtils.getSingleNode(root, geoCamXpath);
        

        String xpathModelType = getItemXpath(LABEL_GEOMODEL, LABEL_MODELTYPE);
        String xpathRefFrame  = getItemXpath(LABEL_GEOMODEL, LABEL_REFFRAMENAME);
        
        
        
        //String geoCamXpathModelType = "//GROUP[@name='GEOMETRIC_CAMERA_MODEL']/item[@key='MODEL_TYPE']" ;
                
        //String xpathStr = "//GROUP[@name='GEOMETRIC_CAMERA_MODEL']/item[@key='"+compName+"']" ;
        String xpathModComp1 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP1);
        String xpathModComp2 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP2);
        String xpathModComp3 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP3);
        String xpathModComp4 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP4);
        String xpathModComp5 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP5);
        String xpathModComp6 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP6);
        String xpathModComp7 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP7);
        String xpathModComp8 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP8);
        String xpathModComp9 = getItemXpath(LABEL_GEOMODEL, LABEL_MODELCOMP9);
        
        
        String cameraModelType = domUtils.getItemValue(root, xpathModelType);
        String refFrameName    = domUtils.getItemValue(root, xpathRefFrame);
        
        
        if (cameraModelType == null)
            return null;
        
        
        Node mc1Node = null, mc2Node = null, mc3Node = null, mc4Node = null;
        Node mc5Node = null, mc6Node = null;
        Node mc7Node = null, mc8Node = null, mc9Node = null;
        
        if (cameraModelType.startsWith(MODELTYPE_CAHV))
        {
            mc1Node = domUtils.getSingleNode(camModNode, xpathModComp1);
            mc2Node = domUtils.getSingleNode(camModNode, xpathModComp2);
            mc3Node = domUtils.getSingleNode(camModNode, xpathModComp3);
            mc4Node = domUtils.getSingleNode(camModNode, xpathModComp4);
        }
        if (cameraModelType.startsWith(MODELTYPE_CAHVOR))
        {
            mc5Node = domUtils.getSingleNode(camModNode, xpathModComp5);
            mc6Node = domUtils.getSingleNode(camModNode, xpathModComp6);
        }
        if (cameraModelType.startsWith(MODELTYPE_CAHVORE))
        {
            mc7Node = domUtils.getSingleNode(camModNode, xpathModComp7);
            mc8Node = domUtils.getSingleNode(camModNode, xpathModComp8);
            mc9Node = domUtils.getSingleNode(camModNode, xpathModComp9);
            
//            //E         Triple
//            CameraModel->ModelComponent7[0].Valid &&         //E
//            CameraModel->ModelComponent7[1].Valid &&
//            CameraModel->ModelComponent7[2].Valid &&
//            
//            //TYPE  (int)
//            CameraModel->ModelComponent8[0].Valid &&        //TYPE
//            
//            //PARAM  (String)
//            CameraModel->ModelComponent9[0].Valid )) {      //PARM

        }
        
        cPoint = (mc1Node == null) ? null : getPigVectorPds(mc1Node, domUtils);
        aVect  = (mc2Node == null) ? null : getPigVectorPds(mc2Node, domUtils);
        hVect  = (mc3Node == null) ? null : getPigVectorPds(mc3Node, domUtils);
        vVect  = (mc4Node == null) ? null : getPigVectorPds(mc4Node, domUtils);
        oVect  = (mc5Node == null) ? null : getPigVectorPds(mc5Node, domUtils);
        rVect  = (mc6Node == null) ? null : getPigVectorPds(mc6Node, domUtils);
        e1Vect = (mc7Node == null) ? null : getPigVectorPds(mc7Node, domUtils);
        
        Double modelType  = (mc8Node == null) ? null : getPigValuePds(mc8Node, domUtils);
        Double modelParam = (mc9Node == null) ? null : getPigValuePds(mc9Node, domUtils);

        
        if (debug) System.out.println("PigCameraModelFactory:: Model type: "+cameraModelType);
        
        
        if (cameraModelType.equals(MODELTYPE_CAHV))
            return new PigCoreCAHV(cPoint, aVect, hVect, vVect, refFrameName);
        if (cameraModelType.equals(MODELTYPE_CAHVOR))
            return new PigCoreCAHVOR(cPoint, aVect, hVect, vVect, oVect, rVect, refFrameName);
        if (cameraModelType.equals(MODELTYPE_CAHVORE))
        {
            int    mType = modelType == null ? PigCoreCAHVORE.DEFAULT_PUPIL_TYPE :
                                               modelType.intValue();
            double mParm = modelParam == null ? PigCoreCAHVORE.DEFAULT_PUPIL_PARAM :
                                                modelParam.doubleValue();
            
            return new PigCoreCAHVORE(cPoint, aVect, hVect, vVect, oVect, rVect, 
                                      e1Vect, mType, mParm, refFrameName);
        }
            
        
   
        return null;
    }
    
    //---------------------------------------------------------------------
    
    
    public static PigPoint getPigVectorPds(Node modelNode, DOMutils domUtils)
    {
        PigPoint vector = null;
        
        NodeList modelSubNodes = modelNode.getChildNodes();
        int count = modelSubNodes.getLength();
        
        if (count == 3)
        {
            //return values
            double[] values = new double[count];
            
            //iterate and parse the double values
            for (int i = 0; i < count; ++i)
            {
                Node modelSubNode = modelSubNodes.item(i);
                String valStr = domUtils.getNodeValue(modelSubNode);
                double val = Double.parseDouble(valStr);
                values[i] = val;
            }
            
            vector = new PigPoint(values);
        }
        
        return vector;
    }
    
    //---------------------------------------------------------------------
    
    public static Double getPigValuePds(Node modelNode, DOMutils domUtils)
    {
        Double value = null;
        
        NodeList modelSubNodes = modelNode.getChildNodes();
        int count = modelSubNodes.getLength();
        
        if (count == 1)
        {
            try {
                Node modelSubNode = modelSubNodes.item(0);
                String valStr = domUtils.getNodeValue(modelSubNode);
                value = Double.parseDouble(valStr);
            } catch (NumberFormatException nfEx) {
                value = null;
            }
        }
        
        return value;
    }

    //---------------------------------------------------------------------
    
    /**
     * Utility routine to extract a vector from the vicar label.  Note that
     * a PigPoint is actually constructed so that it can be used either way,
     * as a point or a vector.  It's up to the caller to decide how it should
     * be used (cast to PigPoint if needed).
     */
    protected static PigVector getPigVectorVicar(VicarLabelSet vlset, String key)
    {
        VicarLabelItem item = vlset.getItem(key);
        if (item == null)
            return null;
        if (item.getNumElements() != 3)
            return null;
        double x[] = item.getDoubleArray();
        return new PigPoint(x);
    }

    //---------------------------------------------------------------------
    
    protected static String getItemXpath(String group, String item)
    {
        String xpathStr = null;
        
        if (group != null)
        {
            if (item != null)
                xpathStr = "//GROUP[@name='"+group+"']/item[@key='"+item+"']" ;
            else
                xpathStr = "//GROUP[@name='"+group+"']" ;
        }
        
        return xpathStr;
    }
    
    
    //---------------------------------------------------------------------
    

    //---------------------------------------------------------------------
}
