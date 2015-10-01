/*
 * Copyright (C) 1998-2002 by University of Maryland, College Park, MD 20742, USA
 * All rights reserved.
 */

package jpl.mipl.jade.jadis.agile2d;

import java.util.*;
import java.awt.*;
import java.awt.image.*;
import java.awt.color.*;

import java.nio.IntBuffer;
import java.nio.ByteBuffer;
import javax.media.opengl.GL;

/**
 * Manages the mapping between Java AWT Images and OpenGL Texture objects.
 */
final class ImageManager {
	private GL gl;			// GL Context
    private AgileState glState;
	private BufferedImage buf;  // Temporary buffer to use to get pixels for image
	private Graphics      bg;   // Graphics object associated with buf.
	private WeakHashMap resident = new WeakHashMap(); // from image -> pool entry
	private ArrayList toDelete = new ArrayList(); // For flusing images from the cache
	private MediaTracker tracker = new MediaTracker(new Label());
        // When the image is not cache, we reuse the same texture
        private Texture defaultTexture;

	private class Entry {
		int id;
		Texture texture;
		Rectangle bounds;

		Entry(Texture texture, Rectangle bounds) {
			this.texture = texture;
			this.bounds = bounds;
		}

		protected void finalize() {
			synchronized (toDelete) { toDelete.add(texture); }
		}
	}

	//
	// Constructs a texture manager. The buffered image and Graphics
	// objects are passed in from the GLGraphics2D class - they are used
	// as temporary store when an image is loaded into texture memory. 
	//
	ImageManager(GL gl, BufferedImage buf, Graphics bg) {
		this.gl = gl;
		this.glState = AgileState.get(gl);
		this.buf = buf;
		this.bg = bg;
	}

	// Given a (image+bounds), return a texture object for that image.
	//
	private Entry getTextureEntry(Image image, Rectangle bounds) {
		// Test if texture is already loaded
		synchronized (resident) {
			ArrayList entries;

			entries = (ArrayList)resident.get(image);
			if (entries != null) {
				if (bounds == null) {
					bounds = new Rectangle(0, 0, image.getWidth(null), image.getHeight(null));
				}

				// Search for the correct entry
				for (int i = 0; i < entries.size(); i++) {
					Entry entry = (Entry)entries.get(i);
					if (entry.bounds.equals(bounds)) {
						return entry;
					}
				}
			}
			// No entry yet
		}
		return null;
	}

	public void removeTexture(Image image) {
		synchronized (resident) {
			ArrayList entries = (ArrayList)resident.get(image);
			if (entries != null) {
				// Search for the correct entry
				for (int i = 0; i < entries.size(); i++) {
					Entry entry = (Entry)entries.get(i);
					entry.texture.dispose();
				}
			}
			resident.remove(image);
		}
	}
	
        public Texture getTexture(Image image, Rectangle bounds, boolean immutable) {
            Entry entry = getTextureEntry(image, bounds);

            if (entry != null && immutable) {
                return entry.texture;
            }
            
            return null;
        }
        
        public Texture getDefaultTexture() {
            if (defaultTexture == null) {
                defaultTexture = new Texture(gl, 4, buf.getWidth(), buf.getHeight(), false);
            }
            return defaultTexture;
        }
 
	//
	// Loads an AWT image into an OpenGL texture
	//
	public Texture findTexture(Image image, Rectangle bounds, boolean immutable,
                boolean forTexturePaint) {
		

		Entry entry = getTextureEntry(image, bounds);
	

                // For texturePaint, we need to have the texture filled, i.e.
                // grow the image to fit the whole texture width and height
                // to use OpenGL 2D repeated textures. 
		if (entry != null && immutable) {
                    if (! forTexturePaint || 
                        (forTexturePaint && entry.texture.isNormalized()))
			return entry.texture;
		}

		//System.out.println("LOAD TEXTURE " + entry.texture.getHeight() + "x" + entry.texture.getWidth());
		if (!(image instanceof BufferedImage)) {
			// Force AWT images to load
			tracker.addImage(image, 1);
			try { tracker.waitForID(1); } catch (InterruptedException ie) { }
			tracker.removeImage(image);
		}

		if (bounds == null) {
			bounds = new Rectangle(0, 0, image.getWidth(null), image.getHeight(null));
		}
		
		int x = (int)bounds.getX();
                int y = (int)bounds.getY();
		int width = (int)bounds.getWidth();
                int height = (int)bounds.getHeight();
                int twidth = forTexturePaint ? ImageUtils.nextPowerOf2(width) : width;
                int theight = forTexturePaint ? ImageUtils.nextPowerOf2(height) : height;
                
                // Create the texture
                 Texture texture;
                 if (entry != null)  { // was immutable before, now said to be mutable
                         texture = entry.texture;
                     }
                     else if (! immutable && 
                         (! forTexturePaint || 
                          (twidth == buf.getWidth() && theight == buf.getHeight()))) {
                         texture = getDefaultTexture();
                     }
                 else {
                     texture = new Texture(gl, 4, twidth, theight, false);
                 }

                BufferedImage img =
                    (image instanceof BufferedImage) ? ((BufferedImage)image) : null;
                if (img != null && 
                    (width == twidth || height == theight) &&
                    (img.getType() == BufferedImage.TYPE_INT_ARGB  ||
                     img.getType() == BufferedImage.TYPE_INT_RGB)) {
                    // we can directly use the image
                    texture.loadPixels(img, new Rectangle(x, y, twidth, theight));
                }
                else {
                    // We draw the image into a temporary buffer, and then load the 
                    // pixels from that buffer. This ensures that the pixel data is accessible
                    // in a known format.
                            //
                    if (!ImageUtils.isOpaque(image)) {
                        ImageUtils.clearArea(buf, twidth, theight);
                    }
                    bg.drawImage(image, 0, 0, twidth, theight, x, y, x+width, y+height, null);
                    // Load the pixels into the texture
                    texture.loadPixels(buf, new Rectangle(0, 0, twidth, theight));
                }

		// Create and register the entry
		if (immutable && entry == null) {
			entry = new Entry(texture, bounds);
			synchronized (resident) {
				ArrayList entries = (ArrayList)resident.get(image);
				if (entries == null) {
					entries = new ArrayList();
					resident.put(image, entries);
				}
				entries.add(entry);
			}
		}
		
		return texture;
	}

	void flush() {
		synchronized (toDelete) {
			if (toDelete.size() > 0) {
				for (int i = 0; i < toDelete.size(); i++) {
					Texture t = ((Texture)toDelete.get(i));
					t.dispose();
				}
				toDelete.clear();
			}
		}
	}
}
