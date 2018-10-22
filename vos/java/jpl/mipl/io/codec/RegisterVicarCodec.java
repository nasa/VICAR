package jpl.mipl.io.codec;

import javax.media.jai.*;
import com.sun.media.jai.codec.*;
import jpl.mipl.io.codec.*;

/**
 * This class uses the JAI <code>OperationRegistrySpi</code> interface to
 * automatically register the VICAR codec.  The <code>updateRegistry()</code>
 * is called during JAI startup in response to an entry naming this class
 * in the "<code>META-INF/services/javax.media.jai.OperationRegistrySpi</code>"
 * file in the jar.
 * @see OperationRegistrySpi
 */

public class RegisterVicarCodec implements OperationRegistrySpi
{

/***********************************************************************
 * No-arg constructor is required by the <code>OperationRegistrySpi</code>
 * rules.
 */
    public RegisterVicarCodec()
    {
    }

/***********************************************************************
 * Do the work of actually registering the codec.  The passed-in
 * <code>OperationRegistry</code> is ignored; the codecs use a different
 * mechanism and we're just hooking into it here.
 */
    public void updateRegistry(OperationRegistry registry)
    {
	ImageCodec.registerCodec(new VicarCodec());
    }
}

