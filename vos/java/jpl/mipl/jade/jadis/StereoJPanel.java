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

import javax.swing.*;

import java.awt.*;

/**
 * <p>
 * This component is what is used to manage stereo children. It supports two
 * child components. Add a component as "left" and another as "right" and they
 * will appear in the respective eyes. Adding as "both" causes the component to
 * be rendered for both eyes. A StereoJPanel must ultimately have a
 * {@link jpl.mipl.jade.jadis.StereoJFrame} as an ancestor, but the StereoJPanel
 * can appear anywhere in the component hierarchy. Thus, most(non-stereo) UI
 * components can use standard Swing {@link javax.swing.JPanel} or other
 * containers, with StereoJPanel used only at the point user actually wants
 * stereo components. <p/>
 * <p>
 * StereoJPanel uses {@link jpl.mipl.jade.jadis.StereoLayout} as a Layout Manager
 * to ensure that the children appear properly aligned with each other and
 * displayed simultaneously.  StereoJPanel also creates 
 * {@link jpl.mipl.jade.jadis.StereoViewJPanel} for each child, which specifically
 * render to one eye or another.  All requests for Graphics objects, will be 
 * handled using the custom stereo-enabled Graphics2D object.  Paint requests
 * will be forwarded to each eye as appropriate.
 * <p/>
 * <br/> Usage:
 * <p>
 * StereoJPanel stereoPanel = new StereoJPanel();<br />
 * stereoJPanel.add(component1, "left");<br />
 * stereoJPanel.add(component2, "right");<br />
 * or stereoJPanel.add(component1, "both"); // mono case<br /> // stereoJPanel
 * needs to be added to <br /> // stereo capable custom JFrame<br />
 * <br/> StereoJFrame stereoJFrame = new StereoJFrame()<br />
 * stereoJFrame.getContentPane().add(stereoJPanel);<br />
 * </p>
 * Note that for certain Swing operations like setting cursor, it's not advisable
 * to do it on "left" or "right" component.  Instead do it on it's parent, 
 * <code>StereoJPanel</code> and "left" and "right" children will inherit this 
 * attribute.
 * 
 * @see jpl.mipl.jade.jadis.StereoJFrame
 * @see jpl.mipl.jade.jadis.StereoLayout
 * @see jpl.mipl.jade.jadis.StereoViewJPanel
 */
public class StereoJPanel extends JPanel {
	/**
	 * Creates new <code>StereoJPanel</code> with
	 * {@link jpl.mipl.jade.jadis.StereoLayout} as a Layout Manager
	 */
	public StereoJPanel() {
		super();
		setLayout(new StereoLayout(this));
	}

	/**
	 * Creates a new <code>StereoJPanel</code> with
	 * {@link jpl.mipl.jade.jadis.StereoLayout} as a Layout Manager and the
	 * specified buffering strategy.
	 */
	public StereoJPanel(boolean isDoubleBuffered) {
		super(isDoubleBuffered);
		setLayout(new StereoLayout(this));
	}

	/**
	 * Adds specified component with specified constraints to the newly created
	 * {@link jpl.mipl.jade.jadis.StereoViewJPanel} The valid constraints are
	 * "left", "right" and "both". Every other value defaults to "both".
	 */
	public void add(Component comp, Object constraints) {
		StereoViewJPanel stereoViewPanel = new StereoViewJPanel();
		stereoViewPanel.add(comp);
		stereoViewPanel.setStereoView(constraints.toString());
		super.add(stereoViewPanel);
	}
}
