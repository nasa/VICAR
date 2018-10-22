/*
 * @(#)VicarImageWriter.java	1.0 00/08/30
 *
  * Steve Levoe JPL/NASA
 */


package jpl.mipl.io.plugins;

// import java.util.Iterator;
import java.util.List;
import javax.imageio.IIOException;
import javax.imageio.IIOImage;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriter;
import javax.imageio.ImageWriteParam;
import javax.imageio.metadata.*;
import javax.imageio.spi.ImageWriterSpi;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.*;
import java.io.*;

// import VicarIO stuff
// VicarInputFile  SystemLabel
// import jpl.mipl.io.streams.*;
import jpl.mipl.io.vicar.*;

/**
 * @version 0.6
 * 
 * changed the Object returned by getDefaultWriteParam() 
 * from ImageWriteParam to  PDSImageWriteParam 
 * this allows the writer to be passed some values in PDSImageWriteParam
 */
public class VicarImageWriter extends ImageWriter {

	 boolean _debug = false; // flag to control annoying printing
	// boolean _debug = true;
	
    // if we add extention support there will be a Constructor 
    // which takes the Object extention as an adition argument
    public VicarImageWriter(ImageWriterSpi originatingSpi) {
        super (originatingSpi);
    }

    public ImageWriteParam getDefaultWriteParam() {
        // ImageWriteParam param = new ImageWriteParam( this.getLocale());
        
        // this allows us to use the extra info carried by the PDSImageWriteParam
		ImageWriteParam param = new PDSImageWriteParam();
        
                    if (_debug) System.out.println("VicarImageWriter.getDefaultWriteParam PDSImageWriteParam");
       //  param.setController(new JFrame1("VicarImageWriter paramController") );
        return param;
    }

    public boolean canWriteThumbnails() {
        return false;
    }

	public void setDebug( boolean b) {
		_debug = b;
	}
    

    public IIOMetadata getDefaultStreamMetadata(ImageWriteParam param) {
        return null;
    }
    
    public IIOMetadata
    getDefaultImageMetadata(ImageTypeSpecifier imageType, ImageWriteParam param) {
        return new VicarMetadata();
    }
    
    public IIOMetadata convertStreamMetadata(IIOMetadata inData, ImageWriteParam param) {
        return null;
    }
    
    public IIOMetadata convertImageMetadata(IIOMetadata inData,
        ImageTypeSpecifier imageType,
        ImageWriteParam param) {
        // We only understand our own metadata
        if (inData instanceof VicarMetadata) {
            return inData;
        } else {
            return null;
        }
    }
    
    
    // -----------------------------------------------------
    
    /**
    * The image is from some other reader or created in memory.
    * Write the image out as Vicar.
    * Based on the RenderedImage create a VicarLabel. 
    **/
    public void write(RenderedImage ri) throws IIOException {
        // create a VicarLabel or pass it in as null 
        // let the write method below be responsible for creating a 
        // VicarLabel from just the image
        VicarLabel vl = null;
        write(ri, vl) ;
    }
    
    
    // need
    public void write(IIOImage iioImage, ImageWriteParam writeParam) throws IIOException {
        // do something with the writeParams
        write(iioImage);
    }
    
    public void write(IIOImage iioImage) throws IIOException {
        if (output == null) {
            throw new IllegalStateException ("Output must be set");
        }
        
        // look at IIOImage and see if we have enough to write out the image
        RenderedImage ri = iioImage.getRenderedImage();
        List thumbList = iioImage.getThumbnails();
        // ignore for now, eventually we MAY use the thumbnails
        
        IIOMetadata metadata = iioImage.getMetadata();
        /* 3 cases to handle
        * 1) There is metadata and it has a VicarLabel in it.
        * It may have come directly from a reader, it may have built
        * by a program, or it may have come from a transcoder. We don't 
        * care where it came from.
        * 2) There is metadata which does NOT include a VicarLabel.
        * We will build a VicarLabel Object from 1 of 4 places:
        *   a) get basic info out of the RenderedImage
        *   b) get basic info out of the common IIOMetadata format
        *   c) If an ImageWriteParam has been set
        *       use the info there, after checking with the RenderedImage to be
        *       sure it makes sense.
        *   d) combine common IIOMetadata format and ImageWriteParam
        * 3) No metadata at all. Then do a variation on case 2) and create 
        * a VicarLabel Object from some combination of the info from
        * the RenderdImage and any ImageWriteParam
        * 4) streamMetadata must be added in too
        **/
        
        
        /* case 1) We have metadata and it includes an already prepared 
        * VicarLabel Object.
        * This is the simplest HighFidelity case.
        */
        if (metadata instanceof VicarMetadata) {
            if (_debug) System.out.println("VicarImageWriter.write() using VicarMetadata");
            VicarMetadata vm = (VicarMetadata) metadata;
            VicarLabel vicarLabel = vm.getVicarLabel();
            write(ri, vicarLabel);
        } else {
        
        /* case 2.a)
        * the image is from some other reader, write the image out as Vicar
        * based on the RenderedImage
        * create a VicarLabel 
        **/
        // for now just ignore any metadata which may have been included
        // this case will be identical to ()so we will just call it!!)
        write(ri);
        
        /***
        if (metadata != null) {
            // throw new UnsupportedOperationException ();
        } else { // no metadata, just use the RenderedImage info
        }
        ***/
        }
        

    }
    
    // since this isn't in the ImageWriter API make this a private method.
    // for testing it is currently public
    private void write(RenderedImage ri, VicarLabel vicarLabel) {     
            
        if (output == null) {
                throw new IllegalStateException ("Output must be set");
        }
        
        if (_debug)  {
        System.out.println("+++++++++++++++++++++++++++++++++++++++++++++++++");
        System.out.println("VicarImageWriter.write() with VicarLabel using vicarIO");
        }
            
            VicarOutputFile vof = new VicarOutputFile();
                  
            if (vof == null) {
                System.out.println("VicarWriter.write(RenderedImage, VicarLabel) null VicarOutputFile: exiting write");
                // send an IOException ???
                return;
            }


            
            
             jpl.mipl.io.vicar.SystemLabel systemLabel;
            // should check if we have a param and use it
            // VicarLabel vicarLabel = vof.getVicarLabel();
            // SystemLabel systemLabel = vof.getSystemLabel();
            
            try { // catch Exceptions for all the VicarIO methods called
             if (vicarLabel == null) {
                // jpl.mipl.io.vicar.VicarLabel 
                vicarLabel = new jpl.mipl.io.vicar.VicarLabel();
                if (_debug) System.out.println("null VicarLabel creating one from the image");
                // jpl.mipl.io.vicar.SystemLabel systemLabel = createSystemLabel(sm);
                systemLabel = createSystemLabel(ri);
                
                vof.setSystemLabel(systemLabel);
                vof.setVicarLabel(vicarLabel);
              
             } else {
                // when I have a real vicarLabel do I need to also set the 
                // SystemLabel or is it set when I do setVicarLabel ???
                // setPrimaryInput(SystemLabel slbl, VicarLabel vlbl)
                // if systemLabel is null then the systemLabel from vicarLabel is used
                
                // System.out.println("VicarImageWriter.write() vof.setPrimaryInput");
                // vof.setPrimaryInput(null, vicarLabel );
                
                // I MUST create a new SystemLabel from the rendered image
                // we can't rely on the System part in the VicarLabel since the format and other System
                // attributes may have changed after the data was read into memory
				if (_debug) System.out.println("VicarImageWriter.write() using old vicarLabel + new system");
                systemLabel = createSystemLabel(ri);
                 vof.setPrimaryInput(systemLabel, vicarLabel );
             }

            if (_debug) System.out.println("VicarImageWriter.write() vicarLabel.createHistoryTask");
            vicarLabel.createHistoryTask("VicarImageWriter");
           
           // figure out where the output comes from
           
            if (_debug) System.out.println("VicarImageWriter.write() vof.open() "+output);
            // vof.open((OutputStream) output); // output stream was supplied to the constructor
            vof.open( output); // output stream was supplied to the constructor
            if (_debug) System.out.println("VicarImageWriter.write() after vof.open");
           
            // ImageWriter.setOutput(Object) which should be ImageOutputStream
            // open writes the label to the file

            // now write the data to the image, write the whole image as a single tile
            int startX = 0;
            int startY = 0;
            int x_off = 0;
            int y_off = 0;
            
            Raster tile ;
            DataBuffer db ; 
            
            
            /// sampleModel is for a tile
            SampleModel sm = ri.getSampleModel();
            
            tile = ri.getTile(0,0);
            if (tile != null) {            	
            	sm = tile.getSampleModel();
            }
            // db = tile.getDataBuffer(); 
                
            int tileWidth = sm.getWidth();
            int tileHeight = sm.getHeight();
            // tileWidth = ri.getWidth();
            
            
            if (_debug) System.out.println("VicarWriter write() tileWidth="+tileWidth+"  tileHeight="+tileHeight );
            
            // loop thru the tiles to write out the entire image ???
            // the tiles must be read in(or grabbed from cache)
            // this loop should be the same as an update of the image
            // int txmin, txmax, tymin, tymax;
            int ti, tj;
            /*
            int minTileX = ri.getMinTileX();
            int maxTileX = ri.getMinTileX() + ri.getNumXTiles() - 1;
            int minTileY = ri.getMinTileY();
            int maxTileY = ri.getMinTileY() + ri.getNumYTiles() - 1;
            */
        
        	int minTileX = 0;
            int maxTileX = minTileX + ri.getNumXTiles() - 1;
            int minTileY = 0;
            int maxTileY = minTileY + ri.getNumYTiles() - 1;
            
            int txmin = minTileX;
            int txmax = maxTileX;
            int tymin = minTileY;
            int tymax = maxTileY;
            
            int tileGridXOffset = ri.getTileGridXOffset();
            int tileGridYOffset = ri.getTileGridYOffset();
            tileGridXOffset = 0;
            tileGridYOffset = 0;
            
            if (_debug) {
            	System.out.println("minTileX "+minTileX+"  minTileY "+minTileY );
            	System.out.println("tileGridXOffset "+tileGridXOffset+" ,  tileGridYOffset "+tileGridYOffset);
            }
            // loop thru all the tiles and write them out to the file
            // this may really only work if we write out a single tile
            for (tj = tymin; tj <= tymax; tj++) {
                for (ti = txmin; ti <= txmax; ti++) {
                // tx and ty are the tile origin 
                // int tx = TileXtoX(ti);
                
            
                // computeTile then write that tile back out
                //Raster ras = im.getData();
            
                // get this tiles raster and write it out to the file
                tile = ri.getTile(ti, tj);
                if (tile != null) {
                	sm = tile.getSampleModel();
                	db = tile.getDataBuffer(); 
                	tileWidth = sm.getWidth();
                	tileHeight = sm.getHeight();
                } else {
                	Raster ras = ri.getData();
                	db  = ras.getDataBuffer();
                	// tileWidth = ri.getWidth();
                    // tileHeight = ri.getHeight();
                }
                
                
                
                int tx = ti*tileWidth + tileGridXOffset;
                // int ty = TileYtoY(tj);
                int ty = tj*tileHeight + tileGridYOffset;
            
                if (_debug) System.out.println("writeTile ti="+ti+" tj="+tj+"  tx="+tx+" ty="+ty );
                vof.writeTile(tx, ty, tileWidth,tileHeight, x_off, y_off, sm, db);
                }
            }
           } // catch all yhe exceptions from VicarIO calls
           catch (jpl.mipl.io.vicar.AlreadyOpenException aoe) {
                    System.out.println("AlreadyOpenException VicarImageWriter.write() "+aoe);
                    aoe.printStackTrace();
                    return;
           }
           catch (IOException ioe) {
                    System.out.println("IOException VicarImageWriter.write() "+ioe);
                    ioe.printStackTrace();
                    return;
           } 
    }
    
    
    
    
    public void write(IIOMetadata streamMetadata,
                      IIOImage image,
                      ImageWriteParam param) throws IIOException {
        if (output == null) {
            throw new IllegalStateException ("Output must be set");
        }
       
        if (streamMetadata != null) {
            // do something with the streamMetadata
            // throw new UnsupportedOperationException ();
        } 
        
        
        write(image); 
    }

    
    /**
* Creates a system label for the image.
* <br>
* If the image was read in from a vicar file then there is a label which is 
* pushed into the properties of the image. <br>
* One could get that instead of creating a new label. 
* If the image isn't from a vicar file a label MUST be created.
**/

public jpl.mipl.io.vicar.SystemLabel createSystemLabel(RenderedImage im) {
    jpl.mipl.io.vicar.SystemLabel sl = new jpl.mipl.io.vicar.SystemLabel();
            
            
    // check colorModel to set some of these items
    // ORG
    // FORMAT
    // HOST
    // INTFMT
    // REALFMT
    SampleModel sm = im.getSampleModel();
    int dataType = sm.getDataType();
    String formatStr = "BYTE";
    if (dataType == DataBuffer.TYPE_BYTE) formatStr = "BYTE";
    if (dataType == DataBuffer.TYPE_SHORT) formatStr = "HALF";
    if (dataType == DataBuffer.TYPE_USHORT) formatStr = "HALF"; // ??? IS THIS CORRECT
    if (dataType == DataBuffer.TYPE_INT) formatStr = "FULL";
    if (dataType == DataBuffer.TYPE_FLOAT) formatStr = "REAL";
    if (dataType == DataBuffer.TYPE_DOUBLE) formatStr = "DOUB";
    // COMP 
    sl.setFormat(formatStr);
    
    String org = "BSQ"; // BIL BIP
    // if (sm instanceof ComponentSampleModel) org = "BSQ";
    
    
    // set org before we set other items, then auto calculations wuill be correct
    sl.setOrg(org);
    
    // sample model is for a tile, not the whole image
    // int height = sm.getHeight();
    // int width = sm.getWidth();
    int width = im.getWidth();
    int height = im.getHeight();
    int bands = sm.getNumBands();
    int[] sampleSize = sm.getSampleSize();
    int b0size = sm.getSampleSize(0);
    int elements = sm.getNumDataElements();
    if (_debug)  {
    	System.out.println("height="+height+"  width="+width+"  bands="+bands );
    	System.out.println("dataElements="+elements+"  b0size="+b0size   );
   
    	for (int i=0 ; i< sampleSize.length ; i++) {
        	System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    	}
    }
    
    /****
    * sl.setDefaults();  is called automatically when thew SystemLabel is created
    * setDefaults sets host to "JAVA" but it isn't marked "valid" so it isn't
    * printed to the label when the file is written
    * we must mark it as valid
    * HIGH - x86-linux vax intel
    * LOW - sun java PPC
    ************************************************/
    
    sl.setHost("JAVA");  
    sl.setHostValid(true);
    
    sl.setIntFmt("HIGH");
    sl.setIntFmtValid(true);
    
	if ((dataType == DataBuffer.TYPE_FLOAT) || (dataType == DataBuffer.TYPE_DOUBLE))  {
    	
			sl.setRealFmt("IEEE");
			sl.setRealFmtValid(true);
		}
    
    // now set things where the defaults aren't correct
    sl.setNL(height);
    sl.setNS(width);
    sl.setNB(bands);
    // sl.calcRecsize(); // calculates recsize based on all the other values previuosly entered
    // called automatically by setNS etc
    sl.setBufsiz(sl.getRecsize()); // Bufsiz isn't used but should be set
    
    if (_debug) {
    	System.out.println("SystemLabel:");
    	System.out.print(sl.toString());
    	System.out.println("-------------------------");
    }
    return sl;
    }
    
    

    public void dispose() {}

}
