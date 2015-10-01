Java Advanced Display Infrastracture for Stereo (JADIS)

Public Release version 1.0
--------------------------

Jadis provides a common interface for displaying Swing GUI components in stereo
using either specialized stereo display hardware (e.g. liquid crystal shutter
or polarized glasses) or anaglyph display (red/blue glasses) on standard
computer displays.  An application using this infrastructure will work without
modification in either environment, allowing stereo software to reach a wider
user base without sacrificing high-quality display on dedicated hardware.

Jadis is a low-level toolkit that accomplishes simply the display and
management of components in stereo (including the JadeDisplay image display
component).  There are test programs submitted with this release that utilize
Jadis to implement higher-level functions such as disparity adjustment and
a stereo cursor.  Jadis was built utilizing the Agile2D framework:

http://sourceforge.net/projects/agile2d/

which has been modified to support stereo rendering.  Interaction between
Agile2D and Swing has been modified as well to support more efficient image
scrolling and to minimize full component repaints when possible.

On the lowest level, both Hardware and Anaglyph stereo modes are rendered
using native OpenGL libraries.  OpenGL is accessed and managed from Java,
using the JOGL library:

https://jogl.dev.java.net

Note that version 1.1 or later of JOGL is required.

Almost all Jadis code is stereo-mode agnostic.  Application developers only
have to specify if a given component should be viewed by Left, Right or Both
eyes.  Anaglyph mode uses OpenGL's glcolorMask(), with Left components being
drawn to the red channel, and Right components being drawn to the blue and
green channels.  Hardware stereo mode uses OpenGL's glDrawBuffer() to render
Left and Right views in separate physical buffers.

Release 1.0 of Jadis is an Open Source release (using a BSD-style license).  

Features
--------

* Written in Java using standard mechanisms for cross-platform compatibility.
* Support for any Swing component that uses the standard Java
  Graphics/Graphics2D rendering mechanism.
* Uses OpenGL to perform all rendering and control stereo-capable
  graphics card (if available)
* Designed to support most essential features of stereo image display:
  - Works with JadeDisplay
  - Capable of supporting stereo cursor rendering
  - Capable of vertical and horizontal disparity adjustment

Limitations/Bugs
----------------

* Use of the Metal Look and Feel is preferred; Windows and Mac OS X look and
  feels have not been fully tested and debugged, and certain Swing components
  might not display properly.  The Aqua L&F specifically plays tricks outside
  of strict use of the Graphics/Graphics2D rendering path, which will not work
  with Jadis.

* Hardware stereo was tested only on limited platforms(Linux and Windows) using
  only NVIDIA Quadro4 graphics cards.  It should however work on any platform
  whose OpenGL implementation supports it.

* It does not work properly on a remote X-windows connection, for reasons
  as yet unknown.

* When using "viewport within viewport" (for disparity adjustment, etc.,
  as demonstrated by Jadis_disparity_test.java), painting outside the image
  boundaries might result in erroneous data showing up on the screen.
  Also, resizing the Swing window might result in "losing" the inner
  viewports' position knowledge.

* Mac OS X 10.5 (Leopard): Using jdk 1.6 beta currently Jadis doesn't work at
  all.  Using jdk 1.5 Jadis works but operations like resizing the window
  generate an exception: Exception in thread "AWT-EventQueue-0"
  java.lang.ClassCastException: jpl.mipl.jade.jadis.agile2d.AgileGraphics2D
   at sun.awt.RepaintArea.paint(RepaintArea.java:215)
  This happens only on Leopard; the same JDK works fine on Tiger and doesn't
  throw this exception.  We have no access to Apple's java source code, so we
  can't tell for sure what's causing the exception.  We are actively evaluating
  available options to running Jadis on Leopard, please contact us if you
  have any suggestions.

Usage Notes
-----------

Here is pseudo-code showing use of Jadis.  For a complete sample program,
see Jadis_simple_test.java in the test directory.

// User creates standard Swing components for the left and right views 
     StereoJPanel stereoPanel = new StereoJPanel();
     stereoPanel.add(swing_component1, "Left");
     stereoPanel.add(swing_component2, "Right");
// Stereo panel could be treated as ordinary swing component and be managed
// by Swing's JViewport to support scrolling, for example
    jviewport.setView(stereoPanel);
    jscrollPane.setViewport(jviewport);
// then add Swing container that manages stereoPanel to the top-level frame
    StereoJFrame stereoJFrame = new StereoJFrame();
    stereoJFrame.getContentPane().add(jscrollPane);
// or add stereoPanel directly
    stereoJFrame.getContentPane().add(stereoPanel);

// specify stereo mode for application to use
    stereoJFrame.setStereoMode(Jade2D.STEREO_ANAGLYPH);  // anaglyph is default
    stereoJFrame.setVisible(true);

--

More extensive examples, exercising other stereo image features, including
use of JadeDisplay, disparity adjustment, and stereo cursor display, are part
of the download package under the "test" directory.

List of Test Programs
---------------------

Jadis_simple.test.java	  - Simplest test program that displays stereo pair
Jadis_test.java		  - Simple test program that uses JadeDisplay(s) and JAI
			    to load and manage images
Jadis_disparity_test.java - Similar to Jadis_test but implements "viewport
			    within a viewport" to allow vertical and horizontal
			    disparity adjustment
jadis_disparity_3dCursor/Jadis_disparity_3dCursor_test.java
			  - More sophisticated test program that has GUI
			    controls for disparity adjustments and stereo
			    cursor display and control

Obtaining Jadis
---------------

Jadis is available (free) in Open Source form using a BSD-style license.
The official distribution is handled by Open Channel (an organization that
maintains software downloads for NASA).

<http://www.openchannelfoundation.org/projects/Jadis/>

Installation and Platforms
--------------------------

Jadis requires Java J2SE 1.5 or later and JOGL (JSR-231) v1.1.0 or later.

The test programs, except for the simplest one, utilize JadeDisplay:

http://www.openchannelfoundation.org/projects/JadeDisplay

and Java Advanced Imaging (JAI).

The above requirements are sufficient for displaying stereo in anaglyph mode.

For Hardware Stereo mode display the following is required:

  - CRT or Front/Rear Projection Display (DLP-based) 
    preferably capable of high refresh rate(100-120Hz)
  - Graphics card that has stereo port and supports OpenGL quad-buffer stereo,
    such as NVIDIA Quadro4 series card.
  - Liquid-crystal shutter glasses and stereo sync emitter.

Other stereo hardware configurations may work as well if properly supported
by OpenGL, but have not been tested.  Note that LCD monitors typically do not
work well as stereo displays due to their persistence; CRT's are required.
  
To use Jadis, simply extract jadis.jar, and add it to your CLASSPATH using
the standard mechanisms.

For X-windows platforms (generally Linux and Solaris), you'll most likely need
to run JOGL with a special flag provided to Java at startup in order to avoid
application lockup:

    -Djogl.GLContext.optimize

The sample programs simply require extracting and compiling.  Assuming you've
installed JOGL, and JAI is inside your JDK, the following will compile and run
them for Solaris, Linux, or Mac OS X (from a terminal).  Note that depending
on your installation, you may need pathnames before each classpath component.

% javac -classpath jadis.jar:jogl.jar:gluegen-rt.jar Jadis_simple_test.java

% java -Djogl.GLContext.optimize -classpath .:jadis.jar:jogl.jar:gluegen-rt.jar Jadis_simple_test.java <your_image_filename_left> <your_image_filename_right>


% javac -classpath jadis.jar:jogl.jar:gluegen-rt.jar:jade_display.jar Jadis_test.java

% java -Djogl.GLContext.optimize -classpath .:jadis.jar:jogl.jar:gluegen-rt.jar:jade_display.jar Jadis_test.java  <your_image_filename_left> <your_image_filename_right>

For Windows, use a ; instead of : between the classpath elements.

You may need to add JOGL and/or JAI to the classpath, depending on your
installation.

Support
-------

Jadis is provided as-is.  It was developed to support JPL space missions.
It is being provided for download basically as a public service (your tax
dollars at work).  However, we are not paid to support the public users in
any way, add features, answer questions, or even fix bugs.

That said, we DO support this product for our own uses.  So please contact
us to report any bugs you find, or to request features.  While we can't
promise to fix/add any, we can take that into account when prioritizing our
own work.  If the feature is useful to us, we might add it.  If it's a bug,
we'll probably fix it since it might affect us in the future too.  If it's
a question, we'll try to answer it.  

Of course, since it is open source, you can also fix problems or add features
yourself.  While not a requirement, we would appreciate hearing about any
changes you make to the code.  Also, it would be nice to hear about the uses
to which Jadis is put.  This software was developed by Bob Deen and Oleg
Pariser at the JPL Multimission Image Processing Lab (MIPL), under NASA
sponsorship and funding.  Public Release 1.0 is derived from VICAR delivery
36.0 (VICAR is the image processing system developed by MIPL).

Contact
-------
<http://www-mipl.jpl.nasa.gov>

Bob Deen
Jet Propulsion Laboratory
4800 Oak Grove Dr.  MS 168-514
Pasadena, CA 91109
USA

818-354-7492

Bob.Deen at jpl dot nasa dot gov

Oleg Pariser
Jet Propulsion Laboratory
4800 Oak Grove Dr.  MS 168-514
Pasadena, CA 91109
USA

818-354-8443

Oleg.Pariser at jpl dot nasa dot gov



