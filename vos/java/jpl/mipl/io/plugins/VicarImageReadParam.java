package jpl.mipl.io.plugins;

import javax.imageio.ImageReadParam;

/* 4-24-2014 srl*/

public class VicarImageReadParam extends ImageReadParam {
    private String directoryPath = null;    
    private int tileSize = 256;
    private int tileSizeX = 256;
    private int tileSizeY = 256;
    

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
    
}
