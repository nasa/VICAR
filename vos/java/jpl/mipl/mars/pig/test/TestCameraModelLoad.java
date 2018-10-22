package jpl.mipl.mars.pig.test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.IIOImage;
import javax.imageio.metadata.IIOMetadata;

import jpl.mipl.mars.pig.PigCameraModel;
import jpl.mipl.mars.pig.PigCameraModelFactory;
import jpl.mipl.mars.pig.PigImageCoordinate;
import jpl.mipl.mars.pig.PigLookVector;
import jpl.mipl.mars.pig.PigVector;

/**
 * <B>Purpose:<B>
 * Test class which accepts a list of image products, attempts
 * to extract camera model, and displays the results of queries to
 * extracted camera model.
 *
 * @author Nicholas Toole (Nicholas.T.Toole@jpl.nasa.gov)
 * @version $Id: $
 *
 */
public class TestCameraModelLoad
{

    protected String productLocation;
    
    protected PigCameraModel cameraModel;
    
   
    protected IIOImage iioImage;
    protected IIOMetadata iioMetadata;
    
    public TestCameraModelLoad(String location)
    {
        this.productLocation = location;
        
        init();
    }
    

    protected void init()
    {             
    }
    
    public void run()
    {
        loadImage();
        loadCameraModel();
        displayResults();
    }
    
    
    
    protected void loadImage()
    {

//        {
//            try {
//                iioImage = IIOImageUtils.loadImage(productLocation);                
//            } catch (Exception ioEx) {
//                System.err.println("Error occurred while loading image: "+ioEx.getMessage());
//                System.exit(1);
//            }            
//            
//            iioMetadata = iioImage.getMetadata();
//        }
//

        
    }
    
    protected void loadCameraModel()
    {
        
        cameraModel = PigCameraModelFactory.create(productLocation);
//        if (this.iioMetadata != null)
//        {            
//            cameraModel = PigCameraModelFactory.createFromIioMetadata(this.iioMetadata);    
//        }
        
//        if (cameraModel == null)          
//        {
//            System.err.println("Camera model is null.  WTF??");
//            System.exit(1);     
//        }
        
    }
    
    protected void displayResults()
    {
        if (cameraModel == null)
        {
            System.err.println("Camera model is null.  WTF??");
            return;
        }
       
        PigImageCoordinate ic = cameraModel.getCameraCenter();
        PigLookVector vec = cameraModel.LStoLookVector(ic);
        
        if (vec == null)
        {
            System.err.println("Received no lookat vector, what a bloody shame!");
            return;
        }

        PigVector center = vec.getOrigin();
        PigVector look   = vec.getLookAt();
        
        System.out.println("Camera center points at = "+ic.getLine()+", "+ic.getSample());
        System.out.println("Camera center: ("+center.getX()+", "+center.getY()+", "+center.getZ()+")");
        System.out.println("Camera center: Az="+center.getAz()+", El="+center.getEl());
        System.out.println("Camera lookAt: ("+look.getX()+", "+look.getY()+", "+look.getZ()+")");
        System.out.println("Camera lookAt: Az="+look.getAz()+",   El="+look.getEl());
        
    }
    
    
    
    public PigCameraModel getCameraModel()
    {
        return this.cameraModel;
    }
    
    public static List<String> getFileList(String[] args)
    {
        List<String> fileList = new ArrayList<String>();
        
        boolean argIsDir = false;
        
        //check for directory as parameter
        if (args.length == 1)
        {
            File file = new File(args[0]);
            if (file.isDirectory())
            {
                argIsDir = true;
                String[] filesInDir = file.list();
                for (String fileInDir : filesInDir)
                {
                    File curFile = new File(file, fileInDir);
                    if (curFile.isFile())
                        fileList.add(curFile.getAbsolutePath());
                }
            }            
        }
        
        //not a single dir as parameter
        if (!argIsDir)
        {
            for (String arg : args)
            {
                File curFile = new File(arg);
                if (curFile.isFile())
                    fileList.add(curFile.getAbsolutePath());
            }
        }
        
        
        return fileList;
    }
    
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
       
        
        if (args.length == 0)
        {
            System.err.println("Missing list PDS product paths or a directory containing products");           
            System.exit(1);            
        }
        
        List<String> fileList = getFileList(args);
        
        for (String productLocation : fileList)
        {
            try {
                
                System.out.println(" ================================================= ");
                System.out.println("Running camera model test for :"+productLocation);
                TestCameraModelLoad test = new TestCameraModelLoad(productLocation);
                test.run();
            } catch (Exception ex) {
                System.err.println("Error occured while processing: "+productLocation);
                ex.printStackTrace();
            }
        }
    }

}
