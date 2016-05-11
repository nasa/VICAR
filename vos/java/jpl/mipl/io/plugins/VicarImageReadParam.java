package jpl.mipl.io.plugins;

import javax.imageio.ImageReadParam;

/* 4-24-2014 srl*/

public class VicarImageReadParam extends ImageReadParam {
    private String directoryPath = null;    
    private int tileSize = 256;
    private int tileSizeX = 256;
    private int tileSizeY = 256;
    // flag. If true we will calculate md5 checksums on the input file
    // add the info to the metadata (DOM) 
    private boolean md5sum = false;
    

    public void setDirectoryPath(String path) {
        this.directoryPath = path;
    }

    public String getDirectoryPath() {
        return this.directoryPath;
    }
    
    public void setTileSize(int t) {
        this.tileSize = t;
    }

    public int getTileSize() {
        return this.tileSize;
    }
    
    public void setTileSizeX(int t) {
        this.tileSizeX = t;
    }

    public int getTileSizeX() {
        return this.tileSizeX;
    }
    
    public void setTileSizeY(int t) {
        this.tileSizeY = t;
    }

    public int getTileSizeY() {
        return this.tileSizeY;
    }
    
    // new for PDS4 velocity template use
    
    public void setMd5sum(boolean f) {
    	md5sum = f;
    }
    
    public boolean getMd5sum() {
    	return md5sum;
    }
    
}
