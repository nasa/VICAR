/*
 * Created on July 6, 2006
 *
 *
 * Copyright 2006, by the California Institute of Technology. ALL RIGHTS RESERVED.
 * United States Government Sponsorship acknowledged. Any commercial use must be 
 * negotiated with the Office of Technology Transfer at the California Institute 
 * of Technology.
 * This software may be subject to U.S. export control laws and regulations.  
 * By accepting this document, the user agrees to comply with all applicable U.S. 
 * export laws and regulations. User has the responsibility to obtain export 
 * licenses, or other export authority as may be required before exporting such 
 * information to foreign countries or providing access to foreign persons. 
 *
 * NASA/JPL/MIPL
 *
 */
 
package jpl.mipl.io.plugins;

// import ScaleBySubsampleRIF;

import javax.media.jai.*;
import javax.media.jai.registry.RIFRegistry;
import javax.media.jai.registry.RenderedRegistryMode;

import java.awt.image.renderable.RenderedImageFactory;

/**
 * @author Steve Levoe MIPL/JPL/NASA
 *
 * This class uses the JAI <code>OperationRegistrySpi</code> interface to
 * automatically register the ScaleMultires operator.  The 
 * <code>updateRegistry()</code> is called during JAI startup in response 
 * to an entry naming this class in the 
 * "<code>META-INF/services/javax.media.jai.OperationRegistrySpi</code>"
 * file in the jar.
 * @see OperationRegistrySpi 
 */
public class RegisterScaleMultiresOpImage implements OperationRegistrySpi
	{
		private final String className_ = "RegisterScaleMultiresOpImage";

		//---------------------------------------------------------------------
		/**
		 * No-arg constructor is required by the <code>OperationRegistrySpi</code>
		 * rules.
		 */

		public RegisterScaleMultiresOpImage()
		{
		}


		//---------------------------------------------------------------------
		/**
		 * Does the work of actually registering the operator. 
		 */

		public void updateRegistry(OperationRegistry registry)
		{
			OperationDescriptor desc = new  ScaleMultiresDescriptor (); // ScaleMultiresOpImageDescriptor ();

			/*** don't do anything now, it will just cause an exception **/
			// System.out.println("RegisterScaleMultiresOpImage ****************************");
			// register descriptor
			registry.registerDescriptor(desc);

			
			//register rendered image factory
			RIFRegistry.register(registry, "ScaleMultires", "jpl.mipl.io", new ScaleMultiresCRIF() );
			// somehow we may want this to just be a second version of the Scale operator
			
			// see JAIExample.java - it has a static intializer which demostrates registering an op
			// under an the name "Scale" and then setting preference
			/**
			 * RIFRegistry.register(registry, "Scale", "jaiexample",
                             new ScaleBySubsampleRIF());
        	registry.setProductPreference(RenderedRegistryMode.MODE_NAME,
                                      "Scale", "jaiexample",
                                      "com.sun.media.jai");
			 */
								
		}
	}




