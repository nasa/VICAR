/*
 * @(#)VicarImageEncoder.java	
 * @version 1.0 11-15-2000
 *
 * @author Steve Levoe NASA/JPL
 * Copyright (c) 1997-1999 Sun Microsystems, Inc. All Rights Reserved.
 *
 */

package jpl.mipl.io.codec;

import java.awt.Rectangle;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import com.sun.media.jai.codec.ImageEncoderImpl;
import com.sun.media.jai.codec.ImageEncodeParam;


import jpl.mipl.io.vicar.*;

import jpl.mipl.io.streams.*;



/**
 * An <code>ImageEncoder</code> for the Vicar family of file formats.
 *
 * <p> The Vicar file format includes BSQ for
 * grey scale images, and BIP for color images. When writing the
 * source data out, the encoder chooses the appropriate file variant
 * based on the actual SampleModel of the source image. In case the
 * source image data is unsuitable for the Vicar file format, for
 * example when source has other than 1 or 3 bands, the encoder
 * throws an Error. <BR>
 * The data type of the input RenderedImage is written to file.
 * 
 */
public class VicarImageEncoder extends ImageEncoderImpl {

    
    private static final int SPACE      = ' ';

    private static final String COMMENT = 
        "# written by VicarImageEncoder";

   

    public VicarImageEncoder(OutputStream output,
                                 ImageEncodeParam param) {
        super(output, param);
        // System.out.println("VicarImageEncoder constructor ");
        
        }
    

    /**
     * Encodes a RenderedImage and writes the output to the
     * OutputStream associated with this ImageEncoder.
     */
    public void encode(RenderedImage im) throws IOException {
        int minX = im.getMinX();
        int minY = im.getMinY();
        int width = im.getWidth();
        int height = im.getHeight();
        SampleModel sampleModel = im.getSampleModel();
        
        System.out.println("********************************");
        System.out.println("VicarImageEncoder encode ++++++++++++++++++++++++");
        // get all the proerties of the images data so we know how to access it
        // and then write it out to a file

        int dataType = sampleModel.getTransferType();
        /****
        if ((dataType == DataBuffer.TYPE_FLOAT) ||
             (dataType == DataBuffer.TYPE_DOUBLE)) {
                System.out.println("Source image has float/double data type, we don't do that yet ;-)");
            throw new RuntimeException(
        "Source image has float/double data type, unsuitable for Vicar file format.");
        }
        ****/
        
        boolean variant = false; 
        int[] sampleSize = sampleModel.getSampleSize();
        int numBands = sampleModel.getNumBands();
        
        
        if (numBands == 1 || numBands ==3) {
            variant = true;
        } else {
            System.out.println("Source image has unsuitable number of bands ("+numBands+") for Vicar file format.");
            throw new RuntimeException(
           "Source image has unsuitable number of bands for Vicar file format.");
        }

        /*
        System.out.println("VicarEncoder width="+width+"  height="+height );
        System.out.println("sampleSize[0]="+sampleSize[0]+"  numBands="+numBands+"  dataType="+dataType );
        System.out.println("minX="+minX+"   minY="+minY );
        */
        
        if (variant == false) {
           // System.out.println("ERROR currently we ONLY handle 8bit grayscale/color images");
           System.out.println("currently we ONLY handle 1 or 3 band grayscale/color images");
           // throw new RuntimeException( "ERROR currently we ONLY handle 8bit grayscale/color images");
           throw new RuntimeException( "ERROR currently we ONLY handle 1 or 3 band grayscale/color images");
           // return ;
        }
        
        // calculate values needed for the label
        int bytesPerSample = 1;
        String format = "BYTE";
        String org = "BSQ";
        
        switch (dataType) {
            case DataBuffer.TYPE_FLOAT :
                bytesPerSample = 4;
                format = "REAL";
                break;
            case DataBuffer.TYPE_DOUBLE :
                bytesPerSample = 8;
                format = "DOUBLE" ; // is there one for double ???
                break;
            case DataBuffer.TYPE_BYTE :
                bytesPerSample = 1;
                format = "BYTE";
                break;
            case DataBuffer.TYPE_SHORT : // do we need to treat ushort and short differently???
            case DataBuffer.TYPE_USHORT :
                bytesPerSample = 2;
                format = "HALF";
                break;
            case DataBuffer.TYPE_INT :
                bytesPerSample = 4;
                format = "INT";
                break;
        }
        
        int ns = width ;
        int nl = height ;
        
        int lblsize = ns * bytesPerSample;
        int recsize = lblsize;
        // MAKE SURE THE LABEL IS BIG ENOUGH TO ALL THE BASIC INFO
        if (ns < 500) {
            int m = ((int) (500/ns)) + 1;
            lblsize = m * ns * bytesPerSample;
        }
        // should add checks to make sure this is big enough
        // while (lblsize < 512 ) {
        //  lblsize *= 2;
        // }
        int bufsiz = lblsize;
        
        
            System.out.println("VicarImageEncoder.encode() using vicarIO");
            // construct a vicar image label just from the info we have here
            // avaentally we need a way like encodeParam to get the input file's 
            // complete vicar label
            VicarOutputFile vof = new VicarOutputFile();
                  
            if (vof == null) {
                System.out.println("VicarImageEncoder.encode() null VicarOutputFile: exiting write");
                // send an IOException ???
                return;
            }

            // should check if we have a param and use it
            // VicarLabel vicarLabel = vof.getVicarLabel();
            // SystemLabel systemLabel = vof.getSystemLabel();
            jpl.mipl.io.vicar.VicarLabel vicarLabel = new jpl.mipl.io.vicar.VicarLabel();
            // SystemLabel systemLabel = new SystemLabel();
            // set values into the labels
            // vicarLabel.createHistoryTask("VicarImageEncoder");
            
            SampleModel sm = im.getSampleModel();
            // jpl.mipl.io.vicar.SystemLabel systemLabel = createSystemLabel(sm);
            jpl.mipl.io.vicar.SystemLabel systemLabel = createSystemLabel(im);
            
            
            // set these modified labels into the output image
            vof.setSystemLabel(systemLabel);
            vof.setVicarLabel(vicarLabel);
            
            /********/
            vof.open(output); // output stream was supplied to the constructor
            // open writes the label to the file

            // now write the data to the image, write the whole image as a single tile
            int startX = 0;
            int startY = 0;
            int x_off = 0;
            int y_off = 0;
            
            /// sampleModel is for a tile
            int tileWidth = sm.getWidth();
            int tileHeight = sm.getHeight();
            
            Raster tile ;
            DataBuffer db ; 
            // System.out.println("VicarEncoder writeTile() width="+width+"  height="+height );
            
            // loop thru the tiles to write out the entire image ???
            // the tiles must be read in(or grabbed from cache)
            // this loop should be the same as an update of the image
            // int txmin, txmax, tymin, tymax;
            int ti, tj;
            int minTileX = im.getMinTileX();
            int maxTileX = im.getMinTileX() + im.getNumXTiles() - 1;
            int minTileY = im.getMinTileY();
            int maxTileY = im.getMinTileY() + im.getNumYTiles() - 1;
        
            int txmin = minTileX;
            int txmax = maxTileX;
            int tymin = minTileY;
            int tymax = maxTileY;
            
            int tileGridXOffset = im.getTileGridXOffset();
            int tileGridYOffset = im.getTileGridYOffset();
            
            // loop thru all the tiles and write them out to the file
            // this may really only work if we write out a single tile
            for (tj = tymin; tj <= tymax; tj++) {
                for (ti = txmin; ti <= txmax; ti++) {
                // tx and ty are the tile origin 
                // int tx = TileXtoX(ti);
                int tx = ti*tileWidth + tileGridXOffset;
                // int ty = TileYtoY(tj);
                int ty = tj*tileHeight + tileGridYOffset;
            
                // computeTile then write that tile back out
                //Raster ras = im.getData();
            
                // get this tiles raster and write it out to the file
                tile = im.getTile(ti, tj);
                sm = tile.getSampleModel();
                db = tile.getDataBuffer(); 
            
                // System.out.println("writeTile ti="+ti+" tj="+tj+"  tx="+tx+" ty="+ty );
                vof.writeTile(tx, ty, tileWidth,tileHeight, x_off, y_off, sm, db);
                }
            }
            
    return;
        
        
        
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
    System.out.println("height="+height+"  width="+width+"  bands="+bands );
    System.out.println("dataElements="+elements+"  b0size="+b0size   );
    for (int i=0 ; i< sampleSize.length ; i++) {
        System.out.println(" sampleSize["+i+"]="+sampleSize[i]);
    }
    
    /****
    * sl.setDefaults();  is called automatically when thew SystemLabel is created
    * setDefaults sets host to "JAVA" but it isn't marked "valid" so it isn't
    * printed to the label when the file is written
    * we must mark it as valid
    ************************************************/
    
    sl.setHostValid(true);
    
    // now set things where the defaults aren't correct
    sl.setNL(height);
    sl.setNS(width);
    sl.setNB(bands);
    // sl.calcRecsize(); // calculates recsize based on all the other values previuosly entered
    // called automatically by setNS etc
    sl.setBufsiz(sl.getRecsize()); // Bufsiz isn't used but should be set
    
    System.out.println("SystemLabel:");
    System.out.print(sl.toString());
    System.out.println("-------------------------");
    return sl;
    }
    
} 
