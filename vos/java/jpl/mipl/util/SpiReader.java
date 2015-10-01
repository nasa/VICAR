package jpl.mipl.util;

import java.util.*;
import java.io.*;
import java.net.*;

/**
 * This class assists in reading a Service Provider Interface (SPI) file for
 * use in auto-registration of services.  This duplicates some functionality
 * in both IIO and JAI 1.1, but since those classes are not public, we have
 * to roll our own.  All functions are static; the class cannot be instantiated.
 * <p>
 * An SPI file lives in the <code>META-INF/services</code> directory of a jar
 * file (or in an exposed classpath directory).  It typically lists class
 * names that exist in that jar/directory that should be globally registered
 * via some mechanism.  For example, MICA uses this to register its Tools.
 * IIO uses it to register file format plugins.  JAI 1.1 uses it to register
 * operators and codecs.
 * <p>
 * The nice thing about this mechanism is that the user merely has to dump a
 * jar file containing the registration file into some place in the CLASSPATH,
 * and it will be automatically found and dealt with.
 * <p>
 * This class provides assistance for finding and reading the SPI files;
 * actually doing something with the lines in the SPI file is up to the
 * caller.  Comments (initial #) will be stripped, blank lines skipped, and
 * leading/trailing blanks on the line will be eliminated.
 * <p>
 * The caller should find SPI files via the following mechanism:
 * <pre>
 *     ClassLoader loader = this.getClass().getClassLoader();  // see below
 *     Enumeration enumm = loader.getResources(
 *                        "META-INF/services/jpl.mipl.mica.MicaToolsSpi");
 *     while (enumm.hasMoreElements()) {
 *         Collection lines = SpiReader.readSpi((URL)enumm.nextElement());
 *         Iterator it = lines.iterator();
 *         while (it.hasNext()) {
 *             String l = (String)it.next();
 *             ... process this line ...
 *         }
 *     }
 * </pre>
 * Note the two loops.  The outer loops over each SPI file (e.g. each jar
 * file), while the inner loops over each line in the file.  A
 * <code>Collection</code> of <code>String</code>s is returned after comments,
 * blank lines, and leading/trailing blanks have been stripped.
 * <p>
 * The SPI filename should start with "<code>META-INF/services/</code>" and the
 * filename part should include the fully-qualified package name of the
 * entity doing the registration (MICA, in this case).  This will prevent
 * name collisions.
 * <p>
 * The <code>ClassLoader</code> is usually the system default loader; this will
 * cause <code>$CLASSPATH</code> to be searched.  If you want to load a plugin
 * at runtime, you need a special class loader like <code>URLClassLoader</code>
 * for the jar anyway; this is then what should be used to get the resources.
 * <p>
 * As a convenience, for users that do not need to keep track of different
 * jar files, a single Collection of lines from all SPI files (concatenated
 * together) may be obtained via <code>SpiReader.readAllSpis()</code>.  Then
 * the above example becomes:
 * <pre>
 *     Collection lines = SpiReader.readAllSpis(
 *                     "META-INF/services/jpl.mipl.mica.MicaToolsSpi", null);
 *     Iterator it = lines.iterator();
 *     while (it.hasNext()) {
 *         String l = (String)it.next();
 *         ... process this line ...
 *     }
 * </pre>
 */

public class SpiReader
{

/***********************************************************************
 * Reads a file and returns a Collection of valid lines.  The following are
 * stripped out:
 * <ul>
 * <li>Comments starting with # in the first column
 * <li>Blank lines
 * <li>Leading and trailing blanks on each line
 * </ul>
 * There is actually nothing specific to SPI's about this function; it could
 * be used for reading any kind of configuration or similar file.
 * @throws IOException if the URL being read does
 */
    public static Collection readSpi(URL url) throws IOException
    {
	BufferedReader br = new BufferedReader(new InputStreamReader(
						url.openStream()));

	ArrayList list = new ArrayList();

	while (true) {
	    String line = br.readLine();
	    if (line == null)
		break;

	    line = line.trim();			// leading/trailing blanks

	    if (line.length() == 0)		// blank line
		continue;

	    if (line.startsWith("#"))		// comment
		continue;

	    list.add(line);
	}

	return list;
    }

/***********************************************************************
 * Convenience routine to read in all SPI's at once.  Use this if you don't
 * care which entries came from which jar file.
 * <p>
 * @param name The name of the file, should start with
 * <code>META-INF/services/</code>.  See class comments.
 * @param loader The class loader to use.  If null, the system class loader
 * is used.
 * @return A single <code>Collection</code> containing all the lines from
 * the files, as massaged by <code>readSpi()</code>.
 * @throws IOException if any called routines do.
 */
    public static Collection readAllSpis(String name, ClassLoader loader)
					throws IOException
    {
	if (loader == null)
	    loader = ClassLoader.getSystemClassLoader();

	ArrayList list = new ArrayList();

	Enumeration enumm = loader.getSystemResources(name);
	while (enumm.hasMoreElements()) {
	    Collection lines = SpiReader.readSpi((URL)enumm.nextElement());
	    list.addAll(lines);
	}

	return list;
    }

/***********************************************************************
 * Attempts to determine the pathname of the entity containing the SPI, given
 * the URL.  If the entity is a jar file, the pathname of the jar (with
 * path and extensions) is returned.  If it is a file, the last directory
 * element before META-INF is returned.  If the name cannot be determined,
 * null is returned.
 * @see java.net.JarURLConnection
 * @see #getContainerName
 */
    public static String getContainerPathname(URL url)
    {
	String protocol = url.getProtocol();

	if (protocol.equalsIgnoreCase("jar")) {

	    // Get a JarURLConnection so we can extract the jar file part.
	    // We could look for "!" in the URL directly, but this is probably
	    // more "correct".

	    URLConnection conn = null;
	    try {
		conn = url.openConnection();
	    } catch (Exception e) {
		return null;			// ignore errors here
	    }
	    if (! (conn instanceof JarURLConnection))
		return null;
	    JarURLConnection jconn = (JarURLConnection)conn;

	    String jarfile = jconn.getJarFileURL().getFile();

	    return jarfile;
	}

	if (protocol.equalsIgnoreCase("file")) {

	    // Look at the URL directly and find the element before /META-INF.

	    String path = url.getFile();

	    int meta_inf = path.indexOf("/META-INF");
	    if (meta_inf == -1)			// not found
		return null;

	    path = path.substring(0, meta_inf);

	    return path;
	}

	return null;			// unknown protocol
    }

/***********************************************************************
 * Attempts to determine the "name" of the entity containing the SPI, given
 * the URL.  If the entity is a jar file, the simple name of the jar (without
 * path or extensions) is returned.  If it is a file, the last directory
 * element before META-INF is returned.  If the name cannot be determined,
 * null is returned.
 * @see java.net.JarURLConnection
 * @see #getContainerPathname
 */
    public static String getContainerName(URL url)
    {
	String protocol = url.getProtocol();

	if (protocol.equalsIgnoreCase("jar")) {

	    String jarfile = getContainerPathname(url);

	    // Get the last component and strip off .jar

	    String name = jarfile.substring(jarfile.lastIndexOf("/")+1,
					    jarfile.lastIndexOf("."));

	    if (name.length() == 0)
		name = null;
	    return name;
	}

	if (protocol.equalsIgnoreCase("file")) {

	    String path = getContainerPathname(url);

	    // Now /META-INF is gone so look for the previous / and start there.

	    String name = path.substring(path.lastIndexOf("/")+1);

	    if (name.length() == 0)
		name = null;
	    return name;
	}

	return null;			// unknown protocol
    }

}

