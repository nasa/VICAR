/**
 * class WebdavVFS2file
 * 
 * * @(#)WebdavVFS2file.java	1.0 13/03/05
 *
 * @author Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 
 * Uses apache commons-vfs2 packages
 * 
 * VFS2 provides a number of protocols
 * 
 * Only webdav, http and https are implemented by this class
 * To-Do:
 * MGSS token authentication should be added to this class.
 * Don't know how it works yet.
 *
 */
package jpl.mipl.io;


/***
 * jars which provide these classes:
 * 
 ***/
import org.apache.commons.vfs2.*;
import org.apache.commons.vfs2.auth.*;
import org.apache.commons.vfs2.impl.*;
import org.apache.commons.vfs2.util.*;
import org.apache.commons.vfs2.provider.*;
import org.apache.commons.vfs2.provider.webdav.*;
import org.apache.commons.vfs2.provider.http.*;
import org.apache.commons.vfs2.provider.https.*;
import org.apache.commons.vfs2.provider.temp.*;
import org.apache.commons.vfs2.cache.*;
import org.apache.commons.vfs2.operations.*;

/*********/

import java.io.InputStream;
import java.io.OutputStream;

import java.io.IOException;
import java.io.FileNotFoundException;


import java.util.*;
import java.net.*;

/**
 * 
 * @author slevoe
 *
 * This class can be used to open a webdav file.
 * The user can obtain the InputStream or OutputStream to read or write to the file.
 */
public class WebdavVFS2file {
	
	// global variables
		public String fileUrl = ""; // The is the input String to access the file/URL/URI
		
		// when StaticUserAuthenticator is used then the username and password (if they are there) 
		// are stripped from the URL, This is the new URL created
		String fileUrlNoUsernamePassword = "";
				
		DefaultFileSystemManager manager = null;
		// flag to know if we have tried to open the file? file state?
		
		// all of these are derived from fileUrl
		FileObject fileObject = null;
		FileContent fileContent = null;
		URL fileObjectUrl = null;
		URLFileName urlFileName = null;
		
		// default to true or false??
		boolean useStaticUserAuthenticator = true;
		StaticUserAuthenticator auth = null;
		/****
		 *  later we may change this
		 *  String authType = "url"; // "url", "staticAuthenticatior", some other TBD type
		 */
		// boolean debug = true;
		boolean debug = false;
		
		
		String host = "";
		int port = 0;
		
		String username = "";
		String password = "";
		String scheme = "";
		String filename = "";
		String directory = "";
		String protocol = "";
		String path = "";
		String ref = "";
		String parent = "";
		int ct = 0;
		
		/*************************
		 *  default constructor
		 *************************/
		public WebdavVFS2file() {
			// get a manager
			
	    	Properties props = System.getProperties();
	    		    	
	    	if (debug) {
	    		props.setProperty("log.level", "DEBUG");
	    		/* this will cause tons of information about the webdav transaction to be logged */
	    	} else {
	    		props.setProperty("log.level", "ERROR");
	    	}
	    	
	    	props.setProperty("jetty.log", "/tmp/jetty.log");
	    	props.setProperty("jackrabbit.log", "/tmp/jackrabbit.log");
	    	/** this also seems to work. don't know if this has any unwelcomed side effects
	    	props.setProperty("jackrabbit.log", "/dev/null");
	    	props.setProperty("jackrabbit.log", "/dev/null");
	    	***/

			/***
			 * The call to getDefaultFileSystemManager() causes the jackrabbit library to 
			 * start a logger. The jar file jackrabbit-standalone-2.4.3.jar
			 * contains the file logback.xml which is used to configure the loggers for this library.
			 * logback.xml uses 3 system properties: log.level, jetty.log and jackrabbit.log
			 * If these values are defined they will be used. Otherwise 2 log files will be created in 
			 * the directory where the program is run. 
			 * jackrabbit.log_IS_UNDEFINED and jetty.log_IS_UNDEFINED
			 * If log.level is not set then the default logging level will be DEBUG. This will 
			 * cause the log file to include the HEX representation of the entire file that is read 
			 * and/or written. This file can be HUGE. (gigabytes)
			 * By setting the value of these properties above, the log files will be created but they 
			 * will be in /tmp
			 * Under normal circumstances with the log level set to ERROR there will be NO 
			 * output written to the logs.
			 * 
			 * Configuring logging is described here:
			 * http://logging.apache.org/log4j/2.x/manual/configuration.html
			 * The docs say that you can put your own file, logback-test.xml or logback.xml in the classpath. 
			 * That file will be used instead of the one in the jar file. logback-test.xml is read first,
			 * then logback.xml. I was unable to get that to work. I put my logback-test.xml in the same 
			 * directory as the jackrabbit jar file.
			 * That would be the best solution. Setting the properties above is the best solution I
			 * could find which does work.
			 */
			try {
	    		this.manager = getDefaultFileSystemManager();
			} catch ( Exception e) {
				System.out.println("getDefaultFileSystemManager exception e="+e);
			}
	    	
	    	// turn off logging
	    	// org.apache.commons.logging.LogFactory.getLog(LogFactory.java:685)
			if (debug) {
				org.apache.commons.logging.Log log = org.apache.commons.logging.LogFactory.getLog(getClass());	    	
	    		System.out.println("getDefaultFileSystemManager log"+log);
	    	}
	    	// log.
    	
	    	
	    	
	    	 
	    	try {
				this.manager.init();
			} catch (FileSystemException e) {
				// TODO Auto-generated catch block
				// e.printStackTrace();
				System.out.println("manager.init exception e="+e);
			}
	    	
		}
		
		/*************************
		 *  URL constructor
		 *************************/
	    public WebdavVFS2file(String fileUrl) {
	    	
	    	this(); // call the default
	    	this.fileUrl = fileUrl;	 
	    	// should this be true ir false by default???
	    	this.useStaticUserAuthenticator = true;
	    	if (debug) {
	    		System.out.println("fileUrl="+fileUrl);			
	    	}
	    	
	    	boolean forceLoad = true;
	    	loadInfo(forceLoad);
	    	
	    	if (debug) {
	    		printInfo();
	    	}
	    		    	
		}
	    
	    /*************************
		 *  URL constructor
		 *************************/
	    public WebdavVFS2file(String fileUrl, boolean useStaticUserAuthenticator) {
	    	
	    	this(); // call the default
	    	this.fileUrl = fileUrl;	
	    	this.useStaticUserAuthenticator = useStaticUserAuthenticator;
	    	if (debug) {
	    		System.out.println("fileUrl="+fileUrl);			
	    	}
	    	
	    	boolean forceLoad = true;
	    	loadInfo(forceLoad);
	    	
	    	if (debug) {
	    		printInfo();
	    	}

		}
	    // add other constructors which have all the URL parts as arguments
	    // scheme, host, port, username, password, directory, filename
	
	    
	    /*******************************************************
	     * getDefaultFileSystemManager
	     * @return DefaultFileSystemManager
	     * 
	     * constructs a FileSystemManager with all of the VFS2 FileProviders 
	     * we want to use.
	     * Other providers could be added if there was a need.
	     * ftp, smb etc.
	     */
	    DefaultFileSystemManager getDefaultFileSystemManager() {
	    	if (debug) {
	    		System.out.println("getDefaultFileSystemManager **************************************");
	    	}
			
			// DefaultFileSystemManager manager = null;
			try {
	            manager = new DefaultFileSystemManager();
	            // vfs2
	            manager.addProvider("webdav", new WebdavFileProvider());
	            manager.addProvider("http",   new HttpFileProvider());
	            manager.addProvider("https",  new HttpFileProvider());
	            
	            // manager.setCacheStrategy(CacheStrategy.ON_CALL);
	            manager.setCacheStrategy(CacheStrategy.ON_RESOLVE );
	            manager.setFilesCache(new SoftRefFilesCache());
	            // used internally by the webdav when writing a file
	            manager.addProvider("tmp", new TemporaryFileProvider());
	            return manager;
	        } catch (FileSystemException e) {
	            e.printStackTrace();
	        }
			return manager;
		}

	    
	    
	    // loadInfo() loads class variables
	    
	    public void loadInfo(boolean forceLoad) {
			
	    	
	    	if (debug) {
	    		System.out.println("loadInfo fileUrl = "+fileUrl+ "   forceLoad = "+forceLoad);
	    	}	    
			/**
			the user sets fileUrl,
			all other values are derived from it
			String fileUrl = "";
			FileObject fileObject = null;
			FileContent fileContent = null;
			URL fileObjectUrl = null;
			URLFileName urlFileName = null;
			****/
	    	
	    	if (fileUrl.equals("")) {
	    		if (debug) {
		    		System.out.println("loadInfo fileUrl is empty");
		    	}	    	
	    		return;
	    	}
	    	if (forceLoad && this.fileObject == null) {
	    		try {
					this.urlFileName = (URLFileName)manager.resolveURI( fileUrl );
				} catch (FileSystemException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				// check if the value is null, load values if it is not null
		    	if (this.urlFileName != null) {
		    		
		    		this.filename = urlFileName.getBaseName();
		    		this.host = urlFileName.getHostName();
		    		this.path = urlFileName.getPath();
		    		this.port = urlFileName.getPort();
		    		this.scheme = urlFileName.getScheme();
		    		this.password = urlFileName.getPassword();
		    		this.username = urlFileName.getUserName();
		    		this.scheme = urlFileName.getScheme();	   
		    		
		    	}
		    	
	    		// forceLoad, load all variables, even they have already been loaded
	    		if (this.useStaticUserAuthenticator = true && this.urlFileName != null) {
	    			// get all the parts from the URL, reconstruct the url without username and password
	    			// flag to know if username and password were supplied outside of fileUrl
	    			// reconstruct a Url   webdav://apps:sppa@localhost/webdav/Narrows_VFS2_copy.png
	    			this.fileUrlNoUsernamePassword = String.format("%s://%s:%d%s", this.scheme, this.host, this.port, this.path);
	    			if (debug) {
	    			   System.out.println("useStaticUserAuthenticator");
	    			   System.out.println("fileUrl = "+this.fileUrl);
	    			   System.out.println("urlFileName = "+this.urlFileName);
	    			   System.out.println("fileUrlNoUsernamePassword = "+this.fileUrlNoUsernamePassword);
	    			}
	    			
	             /**   public StaticUserAuthenticator(String domain, String username, String password)                                                 
	   	    	 * StaticUserAuthenticator auth = new StaticUserAuthenticator(null, "username", "password"); 
	   	    	 * FileSystemOptions opts = new FileSystemOptions(); 
	   	    	 * DefaultFileSystemConfigBuilder.getInstance().setUserAuthenticator(opts, auth); 
	   	    	 * FileObject fo = VFS.getManager().resolveFile("smb://host/anyshare/dir", opts);
	   	    	 * look at the webserver logs to when authentication happens
	   	    	 * * see whether usname, pw can be seen in the traffic
	   	    	 * ***/
	    			try {
	    				this.auth  = new StaticUserAuthenticator(null, this.username, this.password); 
	   	   	    	    FileSystemOptions opts = new FileSystemOptions(); 
	   	   	    	    DefaultFileSystemConfigBuilder.getInstance().setUserAuthenticator(opts, auth); 
	    				this.fileObject = manager.resolveFile(fileUrlNoUsernamePassword, opts);
	    				if (debug) {
	    					System.out.println("auth = "+this.auth);
	    				    System.out.println("this.fileObject = "+this.fileObject);
	    			    }
	    			} catch (FileSystemException e) {
	    				// TODO Auto-generated catch block
	    				e.printStackTrace();
	    			}
	    			
	    		} else {
	    			try {
	    				this.fileObject = manager.resolveFile(fileUrl);
	    			} catch (FileSystemException e) {
	    				// TODO Auto-generated catch block
	    				e.printStackTrace();
	    			}
	    		}
	    	}
		    
	    	if (forceLoad && this.fileObject != null) {
		    	try {
					this.fileContent = this.fileObject.getContent();
				} catch (FileSystemException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
		    	// pull all of the pieces of the fileUrl apart
		    	try {
					this.fileObjectUrl = this.fileObject.getURL();
				} catch (FileSystemException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}	    		
	    	} 
	    		
	}
	

	    /***********************************
	     * 
	     * printInfo() 
	     * 
	     * just prints out information about everything that has been loaded for this URL
	     * Could have returned a String which the user could print.
	     * That String could then be printed to a log file.
	     */
	public void printInfo() {
		
		/**
		 * String fileUrl = "";
		FileObject fileObject = null;
		FileContent fileContent = null;
		URL fileObjectUrl = null;
		URLFileName urlFileName = null;
		 */
		System.out.println("********* printInfo **********>");	
		if (fileUrl.equals("")) {
    		
	    	System.out.println("printInfo: fileUrl is empty");	    	
    		return;
    	}
		loadInfo(false);
		
		System.out.println("fileUrl = "+this.fileUrl);
		System.out.println("urlFileName = "+this.urlFileName);
		System.out.println("fileUrlNoUsernamePassword = "+this.fileUrlNoUsernamePassword);
		// add a flag to use StaticUserAuthenticator ??
		System.out.println("useStaticUserAuthenticator = "+this.useStaticUserAuthenticator);
    	
    	if (this.urlFileName != null) {
    		System.out.println("urlFileName = "+this.urlFileName );
    		System.out.println(" filename = "+this.filename);
    		System.out.println(" host = "+this.host );
    		System.out.println(" path = "+this.path );
    		System.out.println(" port = "+this.port );
    		System.out.println(" scheme = "+this.scheme );
    		System.out.println(" password = "+this.password );
    		System.out.println(" username = "+this.username );
    		    		
    	}
    	
    	if (this.fileObject != null) {
    		System.out.println("fileObject = "+this.fileObject);
    		try {
				System.out.println("fileObject.exists() = "+this.fileObject.exists() );						
				System.out.println("this.fileObject.getType() = "+this.fileObject.getType());
				System.out.println("this.fileObject.isReadable() = "+this.fileObject.isReadable());
				System.out.println("this.fileObject.isWriteable() = "+this.fileObject.isWriteable());
				System.out.println("this.fileObject.getFileOperations() = "+this.fileObject.getFileOperations());
				// print the Collection of file
				// FileOperations fileOperations = this.fileObject.getFileOperations() ;
				// System.out.println("fileOperations");
				
    		} catch (FileSystemException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}		
    		
    	}
    	
    	if (debug) {
    		System.out.println("fileUrl="+fileUrl);
    		System.out.println("urlFileName="+urlFileName);
    		System.out.println("filename="+filename);	
    		System.out.println("host="+host);	
    		System.out.println("path="+path);	
    		System.out.println("port="+port);	
    		System.out.println("password="+password);	
    		System.out.println("username="+username);	
    		System.out.println("scheme="+scheme);		    			
    	}
    	System.out.println("********* printInfo **********<");	
	}
	    
	
	 /**********************************
	  *  setters and getters
	  *  outputStream
	  *  inputStream
	  *  url parts?
	  *  host, port, scheme, usrname, password, path, name
	  *  authToken (what ever that means)
	  *  isWriteable()
	  *  
	  */
	public String getFileUrl() {
		return this.fileUrl;
	}
	
	public void setFileUrl(String fileUrl) {
		this.fileUrl = fileUrl;
		loadInfo(true);
	}
	
	
	/**
	 * getManager
	 * @return DefaultFileSystemManager
	 */
	public DefaultFileSystemManager getManager() {
		if (manager == null) {
		    manager = getManager();
		} 
		return manager;		
	}
	
	public Collection getProviderCapabilities() {			
		Collection c = null;
		try {
			c = manager.getProviderCapabilities(this.scheme);
		} catch (FileSystemException e) {
			e.printStackTrace();
		}
		return c;
    }
	
	public FileObject getFileObject() {
		return this.fileObject;
	}
	
	/**
	 * getFileContent
	 * @return FileContent
	 */
	public FileContent getFileContent() {
		// check if this.fileContent is null call loadInfo(true)
		return this.fileContent;
	}
	
	/**
	 * getOutputStream
	 * @return OutputStream 
	 * user should check for a null return
	 */
	public OutputStream getOutputStream() {
		OutputStream os = null;
		try {
			os = this.fileContent.getOutputStream();
		} catch (FileSystemException e) {
			e.printStackTrace();
		}
		// or I could try/catch and set return to null.
		// let user test for null return
		return os;
	}
	
	/**
	 * getInputStream
	 * @return InputStream 
	 * user should check for a null return
	 */
	public InputStream getInputStream() {
		InputStream is = null;
		try {
			is = this.fileContent.getInputStream();
		} catch (FileSystemException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return is;
	}
	
	
	public String getScheme() {
		return this.scheme;
	}
	
	/****************************************
	 * 
	 * setUsername
	 * @param usrname
	 * Allows the username to set outside of the URL
	 */
	public void setUsername(String usrname) {
		this.username = usrname;
	}
	
	/****************************************
	 * 
	 * setPassword
	 * @param password
	 * Allows the password to set outside of the URL
	 */
	public void setPassword(String password) {
		this.password = password;
	}
	/***************************************
	 * setUseStaticAuthenticator
	 * 
	 * 
	 * @param useStaticUserAuthenticator
	 * 
	 * StaticAuthenticator allows the authentication information to be 
	 * outside of the URL
	 */
	public void setUseStaticAuthenticator(boolean useStaticUserAuthenticator) {
		
		if (this.useStaticUserAuthenticator != useStaticUserAuthenticator) {
			this.useStaticUserAuthenticator = useStaticUserAuthenticator;
			this.loadInfo(true); // force reloading of the info
		} else {
			this.useStaticUserAuthenticator = useStaticUserAuthenticator;
		}
	}
	
	public boolean getUseStaticUserAuthenticator() {
		return this.useStaticUserAuthenticator;
	}
	
	/******
	 * Other functions which could be added:
	 * deleteFile
	 * createDirectory	 * 
	 */
	/****** scheme axamples are "webdav", "http", "https"
	public void setScheme(String scheme) {
		this.scheme = scheme;
	}*******/

} // end of the class



/***
This note is from the VFS2 website.
You can put the credentials into the url, but the drawback here is, that it is easily possible to get access to the password.
To solve you can use the UserAuthenticator
For example: StaticUserAuthenticator auth = new StaticUserAuthenticator("username", "password", null); 
FileSystemOptions opts = new FileSystemOptions(); 
DefaultFileSystemConfigBuilder.getInstance().setUserAuthenticator(opts, auth); 
FileObject fo = VFS.getManager().resolveFile("smb://host/anyshare/dir", opts);
Internally the UserAuthenticator uses char arrays which will be zeroed before it is freed for garbage collection.
Unhappily none of the current libraries use char arrays and so VFS has to create a string. 
Thus, the main advantage of this solution - security - is lost, but hey, thats not VFS fault ;-)
VFS calls UserAuthenticator.requestAuthentication each time it requires credentials, 
it depends on the filesystem implementation how often this might be. 
For example, with FTP this is on every connection, in SMB/JCIFS this is for EVERY OBJECT. 
It is up to you how long you will cache credentials of if you would like to provide a "save credentials" checkbox.
End of VFS2 note.

To handle this idea I have done the following in this class:
pull out the credentials from the URL so they are not sent over the wire
allow credentials to be added to the object
allow queries for all the URL and VFS2 info

Eventually we would like to add the MGSS token authentication scheme. I have no idea how that may work.
Presumably the user would login once and obtain an opaque token which is then passed around anytime authentication 
is required.

*****/