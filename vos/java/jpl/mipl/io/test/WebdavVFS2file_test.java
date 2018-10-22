/**
 * class WebdavVFS2file_test
 * 
 * * @(#)WebdavVFS2file_test.java	1.0 13/05/15
 *
 * @author Steve Levoe
 * Jet Propulsion Laboratory
 * Multimission Image Processing Laboratory
 * 
 * Uses apache commons-vfs2 packages
 *
 */

package jpl.mipl.io.test;

import java.io.InputStream;
import java.io.OutputStream;

import java.io.IOException;


import java.util.*;
import java.net.*;

import jpl.mipl.io.WebdavVFS2file;

import org.apache.commons.vfs2.FileSystemException;

public class WebdavVFS2file_test {
	
	
	boolean debug = true;
    /* test functions */
public void readWriteTest(WebdavVFS2file inputWebdavVFS2file,  WebdavVFS2file outputWebdavVFS2file )
{
	
	if (debug) {
		System.out.println("runReadWriteTest **************************************");
	}
	String inputFileUrl = inputWebdavVFS2file.getFileUrl();
	String outputFileUrl = outputWebdavVFS2file.getFileUrl();
	if (debug) {
		System.out.println("start inputFileUrl = " + inputFileUrl+ " outputFileUrl = "+outputFileUrl);
	    System.out.flush();
	}
    
    // do the test
    // URI Format
    // webdav://[ username[: password]@] hostname[: port][ absolute-path]
	
	if (debug) {
		inputWebdavVFS2file.printInfo();
		outputWebdavVFS2file.printInfo();
	}
	// FileContent fcOut = fileObjectOut.getContent();
    
	// check if isWriteable() exists()
	
	InputStream is = null;
	try {
		is = inputWebdavVFS2file.getFileContent().getInputStream();
	} catch (FileSystemException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
	
	// check if isWriteable() exists()
	OutputStream os = null;
	try {
		os = outputWebdavVFS2file.getFileContent().getOutputStream();
	} catch (FileSystemException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
    
	System.out.println("InputStream is = " +is);
	System.out.println("fcOut.getOuputStream() = "+os);
    
	// read the input file and create the output file
	System.out.println("read the input file and create the output file");
	// randomAccessIs
    byte data[] = new byte[2048];
    int bufLen = 2048;
    int readCt = 0;
    int bytesRead = 0;
    int bytesWritten = 0;
    if (os != null && is != null) {
        // readCt = is.read(data, bytesRead, bufLen);
        try {
 	       // while ((readCt = is.read(data, bytesRead, bufLen)) != -1) {
 	      while ((readCt = is.read(data)) != -1) {
 		       bytesRead += readCt; 
 		       bytesWritten += readCt; 
 		       // System.out.println("bytesRead "+bytesRead+",  readCt "+readCt);
 		       os.write(data);
 	       }
 	    
 	    } catch (IOException x) {
 	        System.err.println(x);
 	        System.out.println("IOException "+x.getMessage() );
 	    }
    }
    
    if (debug) {
    	System.out.println("bytesRead "+bytesRead+",  readCt "+readCt+"  bytesWritten "+bytesWritten);
    }
    
    try {
		is.close();
		os.close();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
    
    

	System.out.println("Done");
}


/**
 * @param args
 * input:
 * output:
 * both are URI's
 * may add some other options
 * String read_wuri1 = "webdav://apps:sppa@localhost/webdav/Narrows.png";     
 * String write_wuri1 = "webdav://apps:sppa@localhost/webdav/Narrows_copy1.png";
 * 
 * readTest
 * add another test function which opens a file and prints info about the file
 * writeTest, or readWriteTest
 * test to create a new directory??
 * check if it is possible to create a directory when the creating a new file
 */
static public void main(String[] args) {
	// TODO Auto-generated method stub
	int argct = args.length;
	System.out.println("webdav_test argct = "+argct);
	String input = "";
	String output = "";
	
	
	
	for (int i=0; i<argct ;i++) {	
		switch (i) { 
		case 0: 
			input = args[0];
		case 1: 
			output = args[1];
		
		default: 
			System.out.println("i="+i+" "+args[i]);
		}			
	}
	
	System.out.println("input="+input);
	System.out.println("output="+output);
	
	WebdavVFS2file webdavFileIn = new WebdavVFS2file(input, false);
	// WebdavVFS2file webdavFileOut = new WebdavVFS2file(output);
	WebdavVFS2file webdavFileOut = new WebdavVFS2file(output, true);
	
	
	WebdavVFS2file_test webdavVFS2file_test = new WebdavVFS2file_test();
	// get 2 file objects
	// test uses the 2 file objects
	webdavVFS2file_test.readWriteTest(webdavFileIn, webdavFileOut);
	
	/**
	 * other tests:
	 * try http URL for reading
	 * add in username password separately (not in URL)
	 */
}

}
