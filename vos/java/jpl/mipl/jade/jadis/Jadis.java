// License Terms
// 
// Copyright (c) 2008, California Institute of Technology ("Caltech").
// U.S. Government sponsorship acknowledged.
// 
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// * Neither the name of Caltech nor its operating division, the Jet Propulsion
// Laboratory, nor the names of its contributors may be used to endorse or
// promote products derived from this software without specific prior written
// permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER  OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
// 
package jpl.mipl.jade.jadis;


/**
 * A class containing static convenience methods, rendering hints and constants
 * for JADIS.  Used by {@link jpl.mipl.jade.jadis.StereoJFrame}, 
 * {@link jpl.mipl.jade.jadis.StereoJPanel} and by other stereo infrastructure
 * classes.
 */
public class Jadis {
	
	/**
	 * Default constructor.  Does nothing.
	 *
	 */
	private Jadis() { }

        /**
         * The rendering will be done to both left and right view.
         * It could be done either by drawing once to both eyes  
         * or drawing identical stuff to left view and then to right view.
         */
        public static final int STEREO_BOTH = 1;
        /**
         * The rendering will be done to the Left eye
         * How it's going to be done is stereo mode specific.
         * For example OpenGL quad-buffer stereo would render
         * to the left buffer.  If double buffering is enabled
         * it would render to the back left buffer, otherwise to
         * the front left buffer.  In anaglyph mode, left view
         * will be rendered with color mask set to allow only red channel
         * to go through.
         */
        public static final int STEREO_LEFT = 2;
        /**
         * The rendering will be done to the Right eye
         * How it's going to be done is stereo mode specific.
         * For example OpenGL quad-buffer stereo would render
         * to the right buffer.  If double buffering is enabled
         * it would render to the back right buffer, otherwise to
         * the front right buffer.  In anaglyph mode, left view
         * will be rendered with color mask set to allow only red channel
         * to go through.
         */
        public static final int STEREO_RIGHT = 3;
        
        /**
         * The rendering will be done with stereo 
         * capabilities turned off or ignored.
         */
        public static final int STEREO_OFF = 4;
        
        /**
         * Anaglyph mode supports single band grayscale images as well as 
         * 3-band color RGB images.  Anaglyph's RGB image is constructed using Left
         * image's Red(or the only plane in case of grayscale) plane and
         * Right image's Green and Blue planes.  In case if right image is greyscale
         * Green and Blue planes are identical and represent input image.
         */
        public static final int STEREO_ANAGLYPH = 5;
        /**
         * In STEREO_GL mode left view is rendered to
         * the left buffer and right view is rendered to the
         * right buffer.  This mode requires computer's
         * hardware to support quad-buffer configuration
         * and ability to sync shutter glasses.
         */
        public static final int STEREO_GL = 6;
        
}

