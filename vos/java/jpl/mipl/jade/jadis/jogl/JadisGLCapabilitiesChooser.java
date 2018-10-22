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
package jpl.mipl.jade.jadis.jogl;

import javax.media.opengl.DefaultGLCapabilitiesChooser;
import javax.media.opengl.GLCapabilities;

/**
 * Extends JOGL's DefaultGLCapabilitiesChooser to better support
 * stereo activation.
 * <p>
 * JOGL's default chooser doesn't return valid configuration if stereo=true
 * but none of the available configurations support stereo.  This implementation
 * cycles through all available configurations and checks if any of configuration
 * supports stereo.  If none available configurations support stereo, desired stereo
 * flag is set to false and then JOGL's default capability chooser is called.
 * Inversely, if desired stereo flag was set to false, but any of available configurations
 * support stereo, then desired stereo flag would be set to true and passed to default
 * capability chooser.
 * <p/>
 *
 */
public class JadisGLCapabilitiesChooser extends DefaultGLCapabilitiesChooser {
	/**
	 * JOGL's default chooser doesn't return valid configuration if stereo=true
	 * but none of the available configurations support stereo.  This implementation
	 * cycles through all available configurations and checks if any of configuration
	 * supports stereo.  If none available configurations support stereo, desired stereo
	 * flag is set to false and then JOGL's default capability chooser is called.
	 * Inversely, if desired stereo flag was set to false, but any of available configurations
	 * support stereo, then desired stereo flag would be set to true and passed to default
	 * capability chooser.
	 */
    public int chooseCapabilities(GLCapabilities desired,
            GLCapabilities[] available,
            int windowSystemRecommendedChoice) {
        boolean anyHaveStereoEnabled = false;
        for (int i = 0; i < available.length; i++) {
                GLCapabilities caps = available[i];
                if (caps != null && caps.getStereo()) {
                        anyHaveStereoEnabled = true;
                        break;
                }
        }
        //If none of the available configurations support stereo, turn it off
        //before passing desired GLCapabilities to 
        // DefaultGLCapabilitiesChooser.chooseCapabilities()
        if (!anyHaveStereoEnabled) {
                desired.setStereo(false);
                System.out.println("Hardware Stereo is not available");
        }
        else {
        	desired.setStereo(true);
        	//testing showed that if windowSystemRecommendedChoice capability
        	//has stereo=false,  that would be honored by superclass' 
        	//chooseCapabilities, even though we set desired's stereo flag to
        	//true before passing it to superclass' method.
        	//So we set windowSystemRecommendedChoice to an invalid index
        	//if corresponding GLCapability has stereo flag as false.
        	if (available[windowSystemRecommendedChoice].getStereo() != true)
        		windowSystemRecommendedChoice = -1;
        	System.out.println("Hardware Stereo is supported");
        }
        return super.chooseCapabilities(desired, available, windowSystemRecommendedChoice);
    }

}
