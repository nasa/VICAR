/*
 * SpiceLib.java
 *
 * Created on August 9, 2000, 2:15 PM
 */

package jpl.mipl.spice.jni;

/** Wrapper for the C language SPICE Toolkit.
 * @author Michael Brady
 */
public class SpiceLib extends Object 
{  
   /*
   *  Loads the library when this class is loaded.
   */
   static
   {
      init();
   }

  
   /** Creates new SpiceLib */
   private SpiceLib() {
   }

   //--------------------------------------------------------------------------    
   //  Private native methods.
   //--------------------------------------------------------------------------    
    
    
   /**
    */
   static private native String n_erract(String getOrSet, String action);

   /**
    */
   static private native String n_getmsg_Short();

   /**
    */
   static private native String n_getmsg_Explain();

   /**
    */
   static private native String n_getmsg_Long();

   /**
    */
   static private native int n_trcdep();

   /**
    */
   static private native String n_trcnam(int index);

   /**
    */
   static private native boolean n_failed();
 
   /**
    */
   static private native void n_reset();    
    
   /**
    */
   static private native void n_ldpool(String fileName);
    
   /**
    */
   static private native void n_clpool();
    
   /**
    */
   static private native void n_furnsh(String fileName);
    
   /**
    */
   static private native void n_unload(String fileName);
    
   /**
    */
   static private native int n_ekopn(String fileName, 
                                     String internalFileName,
                                     int numCommentChars);
    
   /**
    */
   static private native int n_ekopr(String fileName);

   /**
    */
   static private native int n_ekopw(String fileName);

    
   /**
    */
   static private native void n_ekcls(int handle);


   /**
    */
   static private native int n_eklef(String fileName);


   /**
    */
   static private native void n_ekuef(int handle);


   /**
    * @return the segmentNumber of the new segment.
    */
   static private native int n_ekifld(int handle, 
                                      String tableName,
                                      int numRows,
                                      String[] columnNames,
                                      String[] columnDescripions,
                                      int[] records);


   /**
    */
   static private native void n_ekffld(int handle, int segmentNumber, int[] records);


   /**
    */
   static private native void n_ekaclc(int handle,
                                       int segmentNumber,
                                       String columnName,
                                       int[] records,
                                       String[] vals,
                                       int[] valSizes,
                                       int[] nullFlags,
                                       boolean isIndexed);


   /**
    */
   static private native void n_ekacld(int handle,
                                       int segmentNumber,
                                       String columnName,
                                       int[] records,
                                       double[] vals,
                                       int[] valSizes,
                                       int[] nullFlags,
                                       boolean isIndexed);


   /**
    */
   static private native void n_ekacli(int handle,
                                       int segmentNumber,
                                       String columnName,
                                       int[] records,
                                       int[] vals,
                                       int[] valSizes,
                                       int[] nullFlags,
                                       boolean isIndexed);


   /**
    */
   static private native int n_ekbseg(int handle,
                                      String tableName,
                                      String[] columnNames,
                                      String[] columnDescriptions);


   /**
    */
   static private native int n_ekappr(int fileHandle, int segment);


   /**
    */
   static private native void n_ekdelr(int fileHandle, int segmet, int record);


   /**
    */
   static private native void n_ekinsr(int fileHandle, int segment, int record);


   /**
    */
   static private native void n_ekacec(int fileHandle, 
                                       int segment, 
                                       int record,
                                       String columnName,
                                       String[] data,
                                       boolean isNull);


   /**
    */
   static private native void n_ekaced(int fileHandle, 
                                       int segment, 
                                       int record,
                                       String columnName,
                                       double[] data,
                                       boolean isNull);


   /**
    */
   static private native void n_ekacei(int fileHandle, 
                                       int segment, 
                                       int record,
                                       String columnName,
                                       int[] data,
                                       boolean isNull);


   /**
    * Call wasError to find if there was an error in processing the query.
    * @return the number of rows found.
    */
   static private native int n_ekfind(String query);
    
   /**
    *  Returns true if an error occured during the last ekfind call.
    *  If true, call getError for a description of the error.
    */
   static private native boolean n_wasError();
    
   /**
    *  Returns a description of the last error to occur during an ekfind
    */
   static private native String n_getError();

   /**
    *  Call wasNull to find out if the value was null,
    *  Call wasFound to find if this value exists.
    */
   static private native String n_ekgc_fixedLength(int selectIndex, 
                                                   int row, 
                                                   int element,
                                                   int stringLength);

   /**
    *  Call wasNull to find out if the value was null,
    *  Call wasFound to find if this value exists.
    */
   static private native String n_ekgc_variableLength(int selectIndex, 
                                                      int row,
                                                      int arrayElement);

   /**
    *  Call wasNull to find out if the value was null,
    *  Call wasFound to find if this value exists.
    */
   static private native double n_ekgd(int selectIndex, 
                                       int row, 
                                       int element);


   /**
    *  Call wasNull to find out if the value was null,
    *  Call wasFound to find if this value exists.    
    */
   static private native int n_ekgi(int selectIndex, 
                                    int row, 
                                    int element);

   /**
    *  Returns true if the value returned by the last ekg* call was NULL.
    */
   static private native boolean n_wasNull();
    
   /**
    *  Returns false if the no value was found for the last ekg* call.
    */
   static private native boolean n_wasFound();

   /**
    */
   static private native int n_eknelt(int selectIndex, int row);


   /**
    */
   static private native int n_eknseg(int fileHandle);

   /**
    *  @file the handle of the file in which the desired segment is located.
    *  @segment the index of the desired segment
    *  @return the number of rows in the specified segment
    */
   static private native int n_ssum(int file, int segment);
    
   /**
    *  Returns the name of the table to which the segment specified in
    *  the last ssum call belongs.
    */
   static private native String n_getTableName();
   
   /**
    *  Returns the number of columns in the table to which the segment
    *  specified in
    *  the last ssum call belongs.
    */    
   static private native int n_getNumColumns();
    
   /**
    *  Returns the name of the specified column in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */
   static private native String n_getColumnName(int columnIndex);
    
   /**
    *  Returns the data type of the specified column in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */    
   static private native int n_getDataType(int columnIndex);
    
   /**
    *  Returns the string length of the specified column in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */    
   static private native int n_getStringLength(int columnIndex);
    
   /**
    *  Returns the array length of the specified column in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */    
   static private native int n_getArrayLength(int columnIndex);
    
   /**
    *  Returns <CODE>true</CODE> if the specified column is indexed
    *  in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */    
   static private native boolean n_isIndexed(int columnIndex);
    
   /**
    *  Returns <CODE>true</CODE> if NULLs are allowed in 
    *  the specified column in the table to which 
    *  the segment specified in
    *  the last ssum call belongs.
    */    
   static private native boolean n_nullsOk(int columnIndex);
    
   /**
    *  psel (parse a select clause).
    *
    *  Starts the process of getting query meta-data.
    *  pselEnd must be called after this, or memory will leak.
    *
    *  @return the number of items in the select clause.
    */
   static private native int n_psel(String query);
    
   /**
    *  Returns the data type for the specified item the
    *  last time psel was called.
    *
    *  If psel has not been called, behavior is undefined.
    *  @param the index (as listed in the select clause) of the item
    *         about which information is desired.
    *  @return a data type as listed in SpiceEK.h
    */
   static private native int n_getQueryItemDataType(int item);

   /**
    *  Returns the class for the specified item the
    *  last time psel was called.
    *
    *  If psel has not been called, behavior is undefined.
    *  @param the index (as listed in the select clause) of the item
    *         about which information is desired.
    *  @return a class as listed in SpiceEK.h
    */
   static private native int n_getQueryItemClass(int item);

   /**
    *  Returns the table name for the specified item the
    *  last time psel was called.
    *
    *  If psel has not been called, behavior is undefined.
    *  @param the index (as listed in the select clause) of the item
    *         about which information is desired.
    *  @return the name of the table
    */
   static private native String n_getQueryItemTableName(int item);

   /**
    *  Returns the column name for the specified item the
    *  last time psel was called.
    *
    *  If psel has not been called, behavior is undefined.
    *  @param the index (as listed in the select clause) of the item
    *         about which information is desired.
    *  @return the name of the column
    */
   static private native String n_getQueryItemColumnName(int item);

    
    
   /**
    *  Converts a time string to an ephemeris time
    *  in double form.
    *
    *  @param time the string to be converted. See the
    *         comments in str2et_c.c for details about acceptable formats. 
    */
   static private native double n_str2et (String time);
    
    
    
   /**
    *  Converts an ephemeris time in double form to an ephemeris time
    *  in string form, formated as specified.
    *
    *  @param et  the ephemeris time to be converted.
    *  @param format the desired format of the output.  See the
    *         comments in timout_c.c for details about acceptable formats.
    */
   static private native String n_timout (double et, String format);

   /**
    *  Converts a spacecraft clock time to an ephemeris time.
    *
    *  @param the NAIF integer code for the spacecraft
    *  @sclkTime the spacecraft clock time to be converted
    */
   static private native double n_sct2e (int spacecraftId, double sclkTime);

   /**
    *  Converts a spacecraft clock time to an ephemeris time.
    *
    *  @param the NAIF integer code for the spacecraft
    *  @sclkTime the spacecraft clock time to be converted
    */
   static private native double n_scs2e (int spacecraftId, String sclkTime);
  
  
   //--------------------------------------------------------------------------    
   //  Public interface
   //--------------------------------------------------------------------------    
  
   /** a SPICE toolkit error action
    */
   static public final short ABORT = 1;
   /** a SPICE toolkit error action
    */
   static public final short IGNORE = 2;
   /** a SPICE toolkit error action
    */
   static public final short REPORT = 3;
   /** a SPICE toolkit error action
    */
   static public final short RETURN = 4;
   /** a SPICE toolkit error action
    */
   static public final short DEFAULT = 5;

   static private final String ABORT_STR = "ABORT";
   static private final String IGNORE_STR = "IGNORE";
   static private final String REPORT_STR = "REPORT";
   static private final String RETURN_STR = "RETURN";
   static private final String DEFAULT_STR = "DEFAULT";

   // The following values are defined in the SPICE Toolkit's SpiceEK.h
  
   private static final int SPICE_STRING  = 0;  // as defined in SpiceEK.h
   private static final int SPICE_DOUBLE  = 1;  // as defined in SpiceEK.h
   private static final int SPICE_INTEGER = 2;  // as defined in SpiceEK.h
   private static final int SPICE_TIME    = 3;  // as defined in SpiceEK.h  
  

   /**
    * Loads the native code and initizes error handling to return.
    *  This means that SPICE errors will turn into SpiceExceptions.  
    */
   static private void init()
   {
      init(SpiceLib.RETURN);
   }
  
   /**
    *  Loads the native code and initializes error handling.
    * @param errorAction can be ABORT, IGNORE, RETURN, REPORT, DEFAULT
    */
   static private void init(short errorAction)
   {
      String libName = "jpl_mipl_spice";

      System.loadLibrary(libName);      
      setErract(errorAction);
   }
  
  
   /** Checks if the SPICE library is currently is an error state.
    * If so, throws a SpiceException.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static private void checkForError()
      throws SpiceException
   {
      if (SpiceLib.n_failed())
      {
         String[] stackTrace = new String[n_trcdep()];
         for (int i=0; i < stackTrace.length; ++i)
         {
            stackTrace[i] = n_trcnam(i + 1);
         }
         
         SpiceException ex =  new SpiceException( n_getmsg_Short(),
                                                  n_getmsg_Explain(),
                                                  n_getmsg_Long(),
                                                  stackTrace );

         SpiceLib.n_reset();
         throw ex;
      }
   }



   /** Returns the current error-handling mode.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    *
    *
    * @return ABORT, IGNORE, REPORT, RETURN, DEFAULT
    */
   public static short getErract()
   {
      String action = new String();
      SpiceLib.n_erract("GET", action);

      if (action.equals(ABORT_STR))
         return ABORT;
      else if (action.equals(IGNORE_STR))
         return IGNORE;
      else if (action.equals(REPORT_STR))
         return REPORT;
      else if (action.equals(RETURN_STR))
         return RETURN;
      else if (action.equals(DEFAULT_STR))
         return DEFAULT;
      else
         throw new RuntimeException("SpiceLib.getErract:  " +
                                  "Unknown return string from erract('GET')");
   }

   /** Sets the action which the toolkit will take when an error is detected.
    * @param action must be ABORT, IGNORE, REPORT, RETURN, or DEFAULT
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   public static void setErract(short action)
   {
      String actionStr;
      switch (action)
      {
      case ABORT: actionStr = ABORT_STR; break;
      case IGNORE: actionStr = IGNORE_STR; break;
      case REPORT: actionStr = REPORT_STR; break;
      case RETURN: actionStr = RETURN_STR; break;
      case DEFAULT: actionStr = DEFAULT_STR; break;
      default:
         throw new java.security.InvalidParameterException(
                                               "SpiceLib.setErract: " +
                                               "Unknown action.");
      }

      SpiceLib.n_erract("SET", actionStr);
   }



   /**
    * Loads the information from the specified file into the pool.
    */
   static void ldpool(String fileName)
      throws SpiceException
   {
      SpiceLib.n_ldpool(fileName);
      checkForError();
   }
  
   /**
    *  Clears all info which was loaded via {@link #ldpool(String)}.
    */
   static void clpool()
      throws SpiceException
   {
      SpiceLib.n_clpool();
      checkForError();
   }
  
   /**
    *  Loads the specified kernel or kernels listed in the specified meta-kernel.
    */
   static void furnsh(String fileName)
      throws SpiceException  
   {
      n_furnsh(fileName);
      checkForError();
   }
  
   /**
    * Unloads the specified kernel or kernels listed 
    * in the specified meta-kernel.
    */
   static void unload(String fileName)
      throws SpiceException  
   {
      n_unload(fileName);
      checkForError();
   }
  
   /** Creates and opens a new E kernel.
    * @return the handle of the new file
    * @param fileName the name of the new file.
    * @param internalFileName the name used internally by the file
    * @param numCommentChars the number of characters reserved for comments
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static int ekopn(String fileName,
                    String internalFileName,
                    int numCommentChars)
      throws SpiceException
   {
      int handle =
         SpiceLib.n_ekopn(fileName, internalFileName, numCommentChars);
      checkForError();
      return handle;
   }

   /** Opens an E kernel for reading.
    * @param fileName the name of the file to open
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the handle of the opened file
    */
   static int ekopr(String fileName)
      throws SpiceException
   {
      int handle = SpiceLib.n_ekopr(fileName);
      checkForError();
      return handle;
   }

   /** Opens an E kernel for writing.
    * @param fileName the name of the file to open
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the handle of the opened file
    */

   static int ekopw(String fileName)
      throws SpiceException
   {

      int handle = SpiceLib.n_ekopw(fileName);
      checkForError();
      return handle;
   }

   /**
    * Closes an E kernel file.
    *
    * @param handle the handle of the file to close
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekcls(int handle)
      throws SpiceException
   {
      SpiceLib.n_ekcls(handle);
      checkForError();
   }

   /** Loads an E kernel.
    * @param fileName the name of the file to load
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the handle of the opened file
    */
   static int eklef(String fileName)
      throws SpiceException
   {

      int handle = SpiceLib.n_eklef(fileName);
      checkForError();
      return handle;
   }

   /** Unloads an E kernel.
    * @param handle The handle of the kernel to unload
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekuef(int handle)
      throws SpiceException
   {
      SpiceLib.n_ekuef(handle);
      checkForError();
   }

   /** Initiates a segment for fast loading.
    * <P>
    * This is used with {@link #ekaclc }, {@link #ekacld }, {@link #ekacli },
    * and {@link #ekffld } to quickly add many records to a database.
    * @param handle the handle of the file to which the segment is added
    * @param tableName the name of the table to which the segment will be added
    * @param numRows the number of rows in the new segment to be added
    * @param columns the columns in the new segment
    * @return the number of the newly-created segment
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @param records array of records of length <CODE>numRows</CODE>
    * which must be passed to {@link #ekaclc },
    * {@link #ekacld }, and {@link #ekacli }
    */
   static int ekifld(int handle,
                     String tableName,
                     int numRows,
                     Column[] columns,
                     int[] records)
      throws SpiceException
   {
      String[] columnNames = new String[columns.length];
      String[] columnDescriptions = new String[columns.length];
      Bridge.columnsToNamesDescs(columns, columnNames, columnDescriptions);

      if (records.length != numRows)
      {
         throw new java.security.InvalidParameterException(
                  "SpiceLib.ekifld:" +
                  "  array 'records' length must equal 'numRows'");
      }

      int segmentNumber =
         SpiceLib.n_ekifld(handle,
                           tableName,
                           numRows,
                           columnNames,
                           columnDescriptions,
                           records);

      checkForError();
      return segmentNumber;
   }

   /** Finishs the fast load of a segment begun with {@link #ekifld }
    * @param handle The handle of the file containing the segment to be finished
    * @param segmentNumber the index of the segment to be finished
    * @param records the record set returned by {@link #ekifld }
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekffld(int handle, int segmentNumber, int[] records)
      throws SpiceException
   {
      SpiceLib.n_ekffld(handle, segmentNumber, records);
      checkForError();
   }

   /** Adds a column of strings to a segment that was begun with {@link #ekifld }.
    * @param handle the file containing the segment
    * @param segmentNumber the segment to add to
    * @param columnName the name of the column to add to
    * @param records the records returned from {@link #ekifld }
    * @param vals the strings to be added
    * @param valSizes Specifies the array size of each row's entry
    * if this column has variable-size arrays.  Otherwise ignored.
    * @param nullFlags Specifies whether any row's entry is NULL.
    * Ignored if this column doesn't allow NULL values.
    * @param isIndexed should be <CODE>true</CODE> if this column was declared to be indexed in {@link #ekifld}
    * @param stringLength the length of strings in this column, as defined in the call to {@link #ekifld} which started this segment
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekaclc(int handle,
                      int segmentNumber,
                      String columnName,
                      int[] records,
                      String[] vals,
                      int[] valSizes,
                      boolean[] nullFlags,
                      boolean isIndexed)
      throws SpiceException
   {
      if (valSizes == null)
      {
         valSizes = new int[1];
      }
      if (nullFlags == null)
      {
         nullFlags = new boolean[1];
      }

      int[] intNullFlags = Bridge.booleanToInt(nullFlags);
    
      SpiceLib.n_ekaclc(handle,
                        segmentNumber,
                        columnName,
                        records,
                        vals,
                        valSizes,
                        intNullFlags,
                        isIndexed);
      checkForError();
   }

   /** Adds a column of doubles to a segment that was begun with {@link #ekifld }.
    * @param handle the file containing the segment
    * @param segmentNumber the segment to add to
    * @param columnName the name of the column to add to
    * @param records the records returned from {@link #ekifld }
    * @param vals the doubles to be added
    * @param valSizes Specifies the array size of each row's entry
    * if this column has variable-size arrays.  Otherwise ignored.
    * @param nullFlags Specifies whether any row's entry is NULL.
    * Ignored if this column doesn't allow NULL values.
    * @param isIndexed should be <CODE>true</CODE> if this column was declared to be indexed in {@link #ekifld}
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekacld(int handle,
                      int segmentNumber,
                      String columnName,
                      int[] records,
                      double[] vals,
                      int[] valSizes,
                      boolean[] nullFlags,
                      boolean isIndexed)
      throws SpiceException
   {
      if (valSizes == null)
      {
         valSizes = new int[1];
      }
      if (nullFlags == null)
      {
         nullFlags = new boolean[1];
      }

      int[] intNullFlags = Bridge.booleanToInt(nullFlags);    
    
      SpiceLib.n_ekacld(handle,
                        segmentNumber,
                        columnName,
                        records,
                        vals,
                        valSizes,
                        intNullFlags,
                        isIndexed);
      checkForError();
   }

   /** Adds a column of integers to a segment that was begun with {@link #ekifld }.
    * @param handle the file containing the segment
    * @param segmentNumber the segment to add to
    * @param columnName the name of the column to add to
    * @param records the records returned from {@link #ekifld }
    * @param vals the integers to be added
    * @param valSizes Specifies the array size of each row's entry
    * if this column has variable-size arrays.  Otherwise ignored.
    * @param nullFlags Specifies whether any row's entry is NULL.
    * Ignored if this column doesn't allow NULL values.
    * @param isIndexed should be <CODE>true</CODE> if this column was declared to be indexed in {@link #ekifld}
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekacli(int handle,
                      int segmentNumber,
                      String columnName,
                      int[] records,
                      int[] vals,
                      int[] valSizes,
                      boolean[] nullFlags,
                      boolean isIndexed)
      throws SpiceException
   {
      if (valSizes == null)
      {
         valSizes = new int[1];
      }
      if (nullFlags == null)
      {
         nullFlags = new boolean[1];
      }

      int[] intNullFlags = Bridge.booleanToInt(nullFlags);

      SpiceLib.n_ekacli(handle,
                        segmentNumber,
                        columnName,
                        records,
                        vals,
                        valSizes,
                        intNullFlags,
                        isIndexed);
      checkForError();
   }

   /** Begins a new segment in an E kernel file.
    * <P>
    * Segments begun with this routine allow single rows to be manipulated.  To add many rows quickly, start a segment with {@link #ekifld } instead.
    * <P>
    * Use {@link #ekappr }, {@link #ekdelr }, and {@link #ekinsr } on segments created with this routine.
    * @param handle the file in which the new segment will be created
    * @param tableName the name of the table to which the new segmetn will be added
    * @param columns the columns in the table.  These must match the columns declared in other segments in the table.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the number of the new segment
    */
   static int ekbseg(int handle,
                     String tableName,
                     Column[] columns)
      throws SpiceException
   {
      String[] columnNames = new String[columns.length];
      String[] columnDescriptions = new String[columns.length];
      Bridge.columnsToNamesDescs(columns, columnNames, columnDescriptions);

      int segmentNumber =
         SpiceLib.n_ekbseg(handle,
                           tableName,
                           columnNames,
                           columnDescriptions);
      checkForError();

      return segmentNumber;
   }

   /** Appends a new empty record to a segment.
    * @param fileHandle The file in which the new row will be added
    * @param segment the index of the segment to which the new row will be added
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the index of the newly created row
    */
   static int ekappr(int fileHandle, int segment)
      throws SpiceException
   {
      int recordNumber = SpiceLib.n_ekappr(fileHandle, segment);
      checkForError();
      return recordNumber;
   }

   /** Deletes a row (record).
    * @param fileHandle the file in which the record will be deleted.
    * @param segment the index of the segment in which the record will be deleted
    * @param record the index of the record to delete
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekdelr(int fileHandle, int segment, int record)
      throws SpiceException
   {
      SpiceLib.n_ekdelr(fileHandle, segment, record);
      checkForError();
   }

   /** Inserts a new empty record.
    * @param fileHandle The file in which the new record will be added
    * @param segment the index of the segment to which the new record will be added
    * @param record the index of the record after which the new record will be added
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekinsr(int fileHandle, int segment, int record)
      throws SpiceException
   {
      SpiceLib.n_ekinsr(fileHandle, segment, record);
      checkForError();
   }

   /** Adds string data to a single column of a record.
    * @param fileHandle the file to which data is to be added
    * @param segment the index of the segment to which data is to be added
    * @param record the index of the record to which data is to be added
    * @param columnName the name of the column to which data is to be added
    * @param data the data to be added.  If null, an SQL null value is added.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekacec(int fileHandle,
                      int segment,
                      int record,
                      String columnName,
                      String[] data)
      throws SpiceException
   {
      boolean isNull = (data == null);
      if (isNull)
      {
         data = new String[1];
      }

      SpiceLib.n_ekacec(fileHandle,
                        segment,
                        record,
                        columnName,
                        data,
                        isNull);
      checkForError();
   }

   /** Adds double data to a single column of a record.
    * @param fileHandle the file to which data is to be added
    * @param segment the index of the segment to which data is to be added
    * @param record the index of the record to which data is to be added
    * @param columnName the name of the column to which data is to be added
    * @param data the data to be added.  If null, an SQL null value is added.
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekaced(int fileHandle,
                      int segment,
                      int record,
                      String columnName,
                      double[] data)
      throws SpiceException
   {
      boolean isNull = (data == null);
      if (isNull)
      {
         data = new double[1];
      }
      
      SpiceLib.n_ekaced(fileHandle,
                        segment,
                        record,
                        columnName,
                        data,
                        isNull);
      checkForError();
   }
   
   
   /** Adds integer data to a single column of a record.
    * @param fileHandle the file to which data is to be added
    * @param segment the index of the segment to which data is to be added
    * @param record the index of the record to which data is to be added
    * @param columnName the name of the column to which data is to be added
    * @param data the data to be added
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static void ekacei(int fileHandle,
                      int segment,
                      int record,
                      String columnName,
                      int[] data)
      throws SpiceException
   {
      boolean isNull = (data == null);
      if (isNull)
      {
         data = new int[1];
      }

      SpiceLib.n_ekacei(fileHandle,
                        segment,
                        record,
                        columnName,
                        data,
                        isNull);
      checkForError();
   }

   /** Perfoms a SQL query on an E kernel database.
    * @param query the query to be executed (i.e. "SELECT NAME, AGE FROM STUDENTS")
    * @return the number of records in the result set
    * @throws SpiceException if an error was detected in the SPICE toolkit
    */
   static int ekfind(String query)
      throws SpiceException, QueryException
   {
      int numRowsFound = SpiceLib.n_ekfind(query);
      checkForError();
      if (SpiceLib.n_wasError())
      {
         StringBuffer buf = new StringBuffer(query);
         for (int i=0; i < buf.length(); i += 40)
         {
            buf.insert(i, '\n');
         }
         throw new QueryException(SpiceLib.n_getError() + "\n" +
                                  "Query was: " + buf.toString());
      }
      return numRowsFound;
   }

   /** Gets a fixed-length string from an E kernel query result.
    * @param selectIndex The (1-based) index of the column as listed in the query.  For example, if the query was "SELECT NAME, AGE FROM ...", then NAME has an index of 1, and age has an index of 2.
    * @param row The number of the record from which to read
    * @param element if the column type is array, the index of the element to read
    * @param stringLength the length of the strings in this column.
    * @param wasNull <CODE>true</CODE> if the value specified was a NULL value
    * @param wasFound <CODE>false</CODE> if no such data was found
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the data requested
    */
   static String ekgc_fixedLength(int selectIndex,
                                  int row,
                                  int element,
                                  int stringLength)
      throws SpiceException, ResultNotFoundException
   {
      String data = SpiceLib.n_ekgc_fixedLength( selectIndex,
                                                 row,
                                                 element,
                                                 stringLength);
      checkForError();
      if (!SpiceLib.n_wasFound())
      {
         throw new ResultNotFoundException();
      }
      return data;
   }

   /** Gets a variable-length string from an E kernel query result.
    * @param selectIndex The (1-based) index of the column as listed in the query.  For example, if the query was "SELECT NAME, AGE FROM ...", then NAME has an index of 1, and age has an index of 2.
    * @param row The number of the record from which to read
    * @param wasNull <CODE>true</CODE> if the value specified was a NULL value
    * @param wasFound <CODE>false</CODE> if no such data was found
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the data requested
    */
   static String ekgc_variableLength(int selectIndex,
                                     int row,
                                     int arrayElement)
      throws SpiceException, ResultNotFoundException
   {
      String data = SpiceLib.n_ekgc_variableLength( selectIndex,
                                                    row,
                                                    arrayElement);
      checkForError();
      if (!SpiceLib.n_wasFound())
      {
         throw new ResultNotFoundException();
      }
      return data;
   }  
  
   /** Gets a double from an E kernel query result.
    * @param selectIndex The (1-based) index of the column as listed in the query.  For example, if the query was "SELECT NAME, AGE FROM ...", then NAME has an index of 1, and age has an index of 2.
    * @param row The number of the record from which to read
    * @param element if the column type is array, the index of the element to read
    * @param wasNull <CODE>true</CODE> if the value specified was a NULL value
    * @param wasFound <CODE>false</CODE> if no such data was found
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the data requested
    */
   static double ekgd(int selectIndex,
                      int row,
                      int element)
      throws SpiceException, ResultNotFoundException
   {
      double data = SpiceLib.n_ekgd(selectIndex,
                                    row,
                                    element);
      checkForError();
      if (!SpiceLib.n_wasFound())
      {
         throw new ResultNotFoundException();
      }
      return data;
   }

   /** Gets an integer from an E kernel query result.
    * @param selectIndex The (1-based) index of the column as listed in the query.  For example, if the query was "SELECT NAME, AGE FROM ...", then NAME has an index of 1, and age has an index of 2.
    * @param row The number of the record from which to read
    * @param element if the column type is array, the index of the element to read
    * @param wasNull <CODE>true</CODE> if the value specified was a NULL value
    * @param wasFound <CODE>false</CODE> if no such data was found
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the data requested
    */
   static int ekgi(int selectIndex,
                   int row,
                   int element)
      throws SpiceException, ResultNotFoundException
   {
      int data = SpiceLib.n_ekgi( selectIndex,
                                  row,
                                  element);
      checkForError();
      if (!SpiceLib.n_wasFound())
      {
         throw new ResultNotFoundException();
      }
      return data;
   }

   static boolean wasNull()
   {
      return SpiceLib.n_wasNull();
   }

   /** Returns the number of array elements in a cell returned from an E kernel database query.
    * @param selectIndex The (1-based) index of the column as listed in the query.  For example, if the query was "SELECT NAME, AGE FROM ...", then NAME has an index of 1, and age has an index of 2.
    * @param row The number of the record from which to read
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the number of elements in the array
    */
   static int eknelt(int selectIndex, int row)
      throws SpiceException
   {
      int elements = SpiceLib.n_eknelt( selectIndex, row);
      checkForError();
      return elements;
   }

   /** Returns the number of segments in the specified file.
    * @param fileHandle the handle of an E kernel opened for reading
    * @throws SpiceException if an error was detected in the SPICE toolkit
    * @return the number of segments
    */
   static int eknseg(int fileHandle)
      throws SpiceException
   {
      int segments = SpiceLib.n_eknseg(fileHandle);
      checkForError();
      return segments;
   }

   /**
    *
    */
   static Table ekssum(int fileHandle, int segmentIndex)
      throws SpiceException
   {
      int numRows = SpiceLib.n_ssum(fileHandle, segmentIndex);
      checkForError();

      Table table = new Table(SpiceLib.n_getTableName());
      int numColumns = SpiceLib.n_getNumColumns();

      for (int i = 0; i < numColumns; ++i)
      {
         Column column = new Column();
         column.setName(SpiceLib.n_getColumnName(i));
         column.setStringLength(SpiceLib.n_getStringLength(i));
         switch (SpiceLib.n_getDataType(i))
         {
         case SPICE_STRING:
            column.setDataType(
                               (column.getStringLength() < 0) ? Column.STRING_VAR_LENGTH : Column.STRING);
            break;

         case SPICE_DOUBLE:
            column.setDataType(Column.DOUBLE);
            break;

         case SPICE_INTEGER:
            column.setDataType(Column.INTEGER);
            break;

         case SPICE_TIME:
            column.setDataType(Column.TIME);
            break;
        
         default:
            throw new java.security.InvalidParameterException(
                                                              "SpiceLib.edssum:  unknown column data type " +
                                                              String.valueOf(SpiceLib.n_getDataType(i)));
         }
         column.setArrayLength(SpiceLib.n_getArrayLength(i));
         column.setIndexed(SpiceLib.n_isIndexed(i));
         column.setNullsOk(SpiceLib.n_nullsOk(i));

         table.addColumn(column);
      }
      return table;
   }
  
  
   private static int spiceType2SqlType(int spiceType)
   {
      switch (spiceType)
      {
      case SpiceLib.SPICE_DOUBLE:  return java.sql.Types.DOUBLE;
      case SpiceLib.SPICE_INTEGER: return java.sql.Types.INTEGER;
      case SpiceLib.SPICE_STRING:  return java.sql.Types.VARCHAR;
      case SpiceLib.SPICE_TIME:    return java.sql.Types.TIME;
      default:                     return java.sql.Types.OTHER;
      }
   }  
  
   /**
    *  
    */
   static jpl.mipl.spice.jni.JdbcResultSetMetaData psel (String query)
      throws SpiceException
   {
      int numItems = SpiceLib.n_psel(query);
      checkForError();
    
      int[] dataTypes = new int[numItems];
      int[] classes   = new int[numItems];
      String[] tableNames = new String[numItems];
      String[] columnNames = new String[numItems];
    
      for (int i = 0; i < numItems; ++i)
      {
         dataTypes[i]   = spiceType2SqlType(
                                            SpiceLib.n_getQueryItemDataType(i));
         classes[i]     = SpiceLib.n_getQueryItemClass(i);
         tableNames[i]  = SpiceLib.n_getQueryItemTableName(i);
         columnNames[i] = SpiceLib.n_getQueryItemColumnName(i);
      }
      return new jpl.mipl.spice.jni.JdbcResultSetMetaData(query,
                                                          numItems,
                                                          dataTypes,
                                                          classes,
                                                          tableNames,
                                                          columnNames);
   }
  
   /**
    *  Converts a time string to an ephemeris time
    *  in double form.
    *
    *  @param time the string to be converted. See the
    *         comments in str2et_c.c for details about acceptable formats.
    */
   static double str2et (String time)
      throws SpiceException
   {
      double ret = SpiceLib.n_str2et(time);
      checkForError();
      return ret;
   }
  
   /**
    *  Converts an ephemeris time in double form to an ephemeris time
    *  in string form, formated as specified.
    *
    *  @param et  the ephemeris time to be converted.
    *  @param format the desired format of the output.  See the
    *         comments in timout_c.c for details about acceptable formats.
    */
   static String timout (double et, String format)
      throws SpiceException
   {
      String ret = SpiceLib.n_timout(et, format);
      checkForError();
      return ret;
   }

   /**
    *  Converts a spacecraft clock time to an ephemeris time.
    *
    *  @param the NAIF integer code for the spacecraft
    *  @sclkTime the spacecraft clock time to be converted
    */
   static double sct2e (int spacecraftId, double sclkTime)
      throws SpiceException
   {
      double ret = SpiceLib.n_sct2e (spacecraftId, sclkTime);
      checkForError();
      return ret;
   }

   /**
    *  Converts a spacecraft clock time to an ephemeris time.
    *
    *  @param the NAIF integer code for the spacecraft
    *  @sclkTime the spacecraft clock time to be converted
    */
   static double scs2e (int spacecraftId, String sclkTime)
      throws SpiceException
   {
      double ret = SpiceLib.n_scs2e (spacecraftId, sclkTime);
      checkForError();
      return ret;
   }

}
