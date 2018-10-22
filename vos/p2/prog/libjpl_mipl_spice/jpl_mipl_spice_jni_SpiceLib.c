#include "jpl_mipl_spice_jni_SpiceLib.h"

#include "SpiceUsr.h"
#include "SpiceZfc.h" /* for trcdep_ and trcnam_ */

#include <string.h>

#include <stdio.h>
#include <stdlib.h>


static void sanityCheck()
{
  const char* msg = "Warning:  the SPICELIB JNI bridges may be broken!  "
                    "(%s is no longer the same size as %s)\n";

  if (sizeof(jint) != sizeof(SpiceBoolean))
  {
    printf(msg, "jboolean", "SpiceBoolean");
  }
  if (sizeof(jint) != sizeof(SpiceInt))
  {
    printf(msg, "jint", "SpiceInt");
  }
  if (sizeof(char) != sizeof(SpiceChar))
  {
    printf(msg, "char", "SpiceChar");
  }
}

static void throwOutOfMemory(JNIEnv* env)
{
  jclass outOfMemory = 0;
  outOfMemory = (*env)->FindClass(env, "java/lang/OutOfMemoryError");

  /*************************************************************
   The exception appears to work even when FindClass returns 0.
   Puzzling....
  **************************************************************/

  if (0 == outOfMemory)
  {
    printf("outOfMemory was %d\n", (int)outOfMemory);
    printf("Malloc failed, but class java.lang.OutOfMemoryError "
           "could not be found.  Prepare for undefined behavior!\n");
  }
  (*env)->ThrowNew(env, outOfMemory, "malloc failed");
}


static void* checked_malloc(JNIEnv* env, size_t n)
{
  void* ret = (void*)malloc(n);
 if (ret == 0)
  {
    throwOutOfMemory(env);
  }
  return ret;
}

/**
 *  Converts an array of strings to a 2-dimensional array of chars.
 *
 *  @param ar the array of strings to be converted.
 *  @param ret the returned array of chars.
 *  @param stringlen the length of the strings in the returned array
 *                   (the y-length of the returned array).
 *                   If less than 1, the length of the longest string
 *                   in <ar> will be used.
 *
 *  @return the length of the char array 
 *          (the length of the longest string + 1).
 */
SpiceInt stringArrayToChars(JNIEnv* env, 
			    jobjectArray ar,
			    char** ret)
{
    int arLen = (*env)->GetArrayLength(env, ar);    
    int sLen = 0;
    jsize i = 0;
    jstring jstr;
    const char* str = 0;
    void* tmp = 0;


    /* Find the length of the longest string. */
    
    jsize longest = 0;
    jsize len;

    for (i=0; i < arLen; ++i)
    {
       jstr = (*env)->GetObjectArrayElement(env, ar, i);
       str  = (*env)->GetStringUTFChars(env, jstr, 0);
       len  = (*env)->GetStringLength(env, jstr);
       
#if defined STRING_COPY_VERBOSE
       printf("String %d is length %d\n", i, len);
#endif
       
       if (longest < len)
       {
	  longest = len;
       }

       (*env)->ReleaseStringUTFChars(env, jstr, str);
    }
    sLen = (int)longest;


    /* Add one to sLen so there will be a final null char
     * at the end of each string.
     */
    ++sLen;


    tmp = (void*)checked_malloc(env, sizeof(char) * arLen * sLen);
    (*ret) = (char*)tmp;
    memset(*ret, '\0', sizeof(char) * arLen * sLen);

#if defined STRING_COPY_VERBOSE
    printf("There are %d strings of length %d\n", arLen, sLen);
#endif    
 
    for (i=0; i < arLen; ++i)
    {
	jstr =(*env)->GetObjectArrayElement(env, ar, i);
	str = (*env)->GetStringUTFChars(env, jstr, 0);

#if defined STRING_COPY_VERBOSE
	printf("Copying string: ");
	printf("%s\n", str);
#endif

	strcpy((*ret) + (i * sLen), str);

#if defined STRING_COPY_VERBOSE
      	printf("Copied string: %s\n", (*ret) + (i * sLen));
#endif	 

	(*env)->ReleaseStringUTFChars(env, jstr, str);	
    }
    return (SpiceInt)sLen;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1erract
  (JNIEnv* env, jclass jcl, jstring jGetOrSet, jstring jAction)
{
    ConstSpiceChar* getOrSet = (*env)->GetStringUTFChars(env, jGetOrSet, 0);
    const char* action =  (*env)->GetStringUTFChars(env, jAction, 0);

    char buf[64];
    memset(buf, '\0', sizeof(buf));

    if (strcmp(getOrSet, "SET") ==0)
    {
	strncpy(buf, action, sizeof(buf)-1);
    }

    erract_c(getOrSet, sizeof(buf) - 1, buf);

    (*env)->ReleaseStringUTFChars(env, jGetOrSet, getOrSet);
    (*env)->ReleaseStringUTFChars(env, jAction, action);

    if (strcmp(getOrSet, "GET") == 0)
    {
	return (*env)->NewStringUTF(env, buf);
    }
    else
    {
	return (*env)->NewStringUTF(env, "");
    }

}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1failed
  (JNIEnv* env, jclass jcl)
{
    return (jboolean) failed_c();
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1reset
  (JNIEnv* env, jclass jcl)
{
   reset_c();
}

/* Strings for calling getmsg_c */

static const char* SHORT_STR = "SHORT";
static const char* EXPLAIN_STR = "EXPLAIN";
static const char* LONG_STR = "LONG";

/* The lengths below are specified in getmsg_c.c */

#define MAX_SHORT_MESSAGE_LENGTH 25
#define MAX_DESCRIPTION_LENGTH   80
#define MAX_LONG_MESSAGE_LENGTH  1840


JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getmsg_1Short
  (JNIEnv *env, jclass jcl)
{
   char name[MAX_SHORT_MESSAGE_LENGTH + 1];
   memset(name, '\0', sizeof(name));
   getmsg_c(SHORT_STR, sizeof(name), name);
   
   return (*env)->NewStringUTF(env, name);
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getmsg_1Explain
  (JNIEnv *env, jclass jcl)
{
   char description[MAX_DESCRIPTION_LENGTH + 1];
   memset(description, '\0', sizeof(description));
   getmsg_c(EXPLAIN_STR, sizeof(description), description);
   
   return (*env)->NewStringUTF(env, description);
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getmsg_1Long
  (JNIEnv *env, jclass jcl)
{
   char longMessage[MAX_LONG_MESSAGE_LENGTH + 1];
   memset(longMessage, '\0', sizeof(longMessage));
   getmsg_c(LONG_STR, sizeof(longMessage), longMessage);
   
   return (*env)->NewStringUTF(env, longMessage);
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1trcdep
  (JNIEnv *env, jclass jcl)
{
   SpiceInt traceDepth = 0;
   trcdep_(&traceDepth);

   return traceDepth;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1trcnam
  (JNIEnv *env, jclass jcl, jint jIndex)
{
   SpiceInt index = jIndex;

   char functionName[128];
   memset(functionName, '\0', sizeof(functionName));
   trcnam_(&index, functionName, sizeof(functionName));
   
   /* Chop off the string:
    *  (1) at the first newline.
    *  (2) at the first space.
    *  (3) if all else fails, at the end of the buffer.
    */
   {
      unsigned int i;
      for (i=0; i < sizeof(functionName); ++i)
      {
         if (functionName[i] == ' ')
         {
            functionName[i] = '\0';
            break;
         }
         else if (functionName[i] == '\n')
         {
            functionName[i] = '\0';
            break;
         }
         else if (i == (sizeof(functionName) - 1))
         {
            functionName[i] = '\0';
            break; 
         }
      }
   }

   return (*env)->NewStringUTF(env, functionName);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ldpool
  (JNIEnv* env, jclass jcl, jstring jFileName)
{    
   ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);
   ldpool_c(fileName);
   (*env)->ReleaseStringUTFChars(env, jFileName, fileName);
}


JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1clpool
  (JNIEnv* env, jclass jcl)
{
   clpool_c();
}


JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1furnsh
  (JNIEnv* env, jclass jcl, jstring jFileName)
{
   ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);
   furnsh_c(fileName);
   (*env)->ReleaseStringUTFChars(env, jFileName, fileName);
}


JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1unload
  (JNIEnv* env, jclass jcl, jstring jFileName)
{
   ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);
   unload_c(fileName);
   (*env)->ReleaseStringUTFChars(env, jFileName, fileName);
}


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekopn
  (JNIEnv* env, jclass jcl, 
   jstring jFileName, 
   jstring jInternalName, 
   jint numCommentChars)
{
    ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);
    ConstSpiceChar* internalName = 
	(*env)->GetStringUTFChars(env, jInternalName, 0);    

    SpiceInt ret = 0;
    ekopn_c(fileName, 
	    internalName,
	    (SpiceInt)numCommentChars,
	    &ret);

    (*env)->ReleaseStringUTFChars(env, jFileName, fileName);

    return (jint)ret;
}


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekopr
  (JNIEnv* env, jclass jcl, jstring jFileName)
{
    ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);    
    SpiceInt ret = 0;
    ekopw_c(fileName, &ret);

    (*env)->ReleaseStringUTFChars(env, jFileName, fileName);

    return (jint)ret;
}


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekopw
  (JNIEnv* env, jclass jcl, jstring jFileName)
{
    ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);    
    SpiceInt ret = 0;
    ekopw_c(fileName, &ret);

    (*env)->ReleaseStringUTFChars(env, jFileName, fileName);

    return (jint)ret;
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekcls
  (JNIEnv* env, jclass jcl, jint fileHandle)
{
    ekcls_c(fileHandle);
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1eklef
  (JNIEnv* env, jclass jcl, jstring jFileName)
{
    ConstSpiceChar* fileName = (*env)->GetStringUTFChars(env, jFileName, 0);    
    SpiceInt ret = 0;
    eklef_c(fileName, &ret);

    (*env)->ReleaseStringUTFChars(env, jFileName, fileName);

    return (jint)ret;
  
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekuef
  (JNIEnv* env, jclass jcl, jint jFileHandle)
{
  SpiceInt fileHandle = (SpiceInt) jFileHandle;
  ekuef_c(fileHandle);
}


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekifld
  (JNIEnv* env, jclass jcl, jint jHandle, jstring jTableName, jint jNumRows, jobjectArray jColumnNames, jobjectArray jColumnDescriptions, jintArray jRecords)
{
    SpiceInt handle = (SpiceInt)jHandle;
    ConstSpiceChar* tableName = (*env)->GetStringUTFChars(env, jTableName, 0);
    char* names = 0;
    SpiceInt nameLength = 0;
    char* descriptions = 0;
    SpiceInt descLength = 0;
    jint* records = (*env)->GetIntArrayElements(env, jRecords, 0);
    SpiceInt* sRecords = (SpiceInt*)records;
    
    SpiceInt segmentNumber = -1;

    SpiceInt numRows = (SpiceInt)jNumRows;
    SpiceInt numColumns = (SpiceInt)(*env)->GetArrayLength(env, jColumnNames);
    
    nameLength = stringArrayToChars(env, jColumnNames, &names); 
    descLength = stringArrayToChars(env, jColumnDescriptions, &descriptions);

   
   ekifld_c((SpiceInt) handle,
	    (ConstSpiceChar*) tableName,
	    (SpiceInt) numColumns,
	    (SpiceInt) numRows,
	    (SpiceInt) nameLength,
	    (void*)names,
	    (SpiceInt) descLength,
	    (void*)descriptions,
	    (SpiceInt*) &segmentNumber,
	    (SpiceInt*) sRecords);
   
   free(names);
   free(descriptions);

    (*env)->ReleaseStringUTFChars(env, jTableName, tableName);
    (*env)->ReleaseIntArrayElements(env, jRecords, records, 0);

    return (jint)segmentNumber;
}


JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekffld
  (JNIEnv* env, jclass jcl, jint handle, jint segmentNumber, jintArray jRecords)
{
    jint* records = (*env)->GetIntArrayElements(env, jRecords, 0);
    SpiceInt* sRecords = (SpiceInt*)records;

    ekffld_c(handle, segmentNumber, sRecords);

    (*env)->ReleaseIntArrayElements(env, jRecords, records, 0);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekaclc
  (JNIEnv* env, jclass jcl, 
   jint handle, 
   jint segmentNumber, 
   jstring jColumnName, 
   jintArray jRecords, 
   jobjectArray jVals,
   jintArray jValSizes,
   jintArray jNullFlags, 
   jboolean isIndexed)
{
    char* vals = 0;
    SpiceInt stringLen = 0;
    int numRows = (*env)->GetArrayLength(env, jVals);


    jint* records = (*env)->GetIntArrayElements(env, jRecords, 0);
    jint* valSizes = (*env)->GetIntArrayElements(env, jValSizes, 0);
    jint* nullFlags = (*env)->GetIntArrayElements(env, jNullFlags, 0);

    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);
    SpiceInt* workIndex = 
      (SpiceInt*)checked_malloc(env, sizeof(SpiceInt) * numRows);

    sanityCheck();

    stringLen = stringArrayToChars(env, jVals, &vals);


    ekaclc_c(handle,
	     segmentNumber,
	     columnName,
	     stringLen,
	     vals,
	     valSizes,
	     nullFlags,
	     records,
	     workIndex);

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    (*env)->ReleaseIntArrayElements(env, jNullFlags, nullFlags, 0);
    (*env)->ReleaseIntArrayElements(env, jRecords, records, 0);    
    (*env)->ReleaseIntArrayElements(env, jValSizes, valSizes, 0);

    free(vals);
    free(workIndex);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekacld
  (JNIEnv* env, jclass jcl, 
   jint handle, 
   jint segmentNumber, 
   jstring jColumnName, 
   jintArray jRecords, 
   jdoubleArray jVals, 
   jintArray jValSizes, 
   jintArray jNullFlags, 
   jboolean isIndexed)
{
    double* vals = (*env)->GetDoubleArrayElements(env, jVals, 0);
    int numRows = (*env)->GetArrayLength(env, jVals);


    jint* records = (*env)->GetIntArrayElements(env, jRecords, 0);
    jint* valSizes = (*env)->GetIntArrayElements(env, jValSizes, 0);
    jint* nullFlags = (*env)->GetIntArrayElements(env, jNullFlags, 0);

    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);

    SpiceInt* workIndex = 
      (SpiceInt*)checked_malloc(env, sizeof(SpiceInt) * numRows);

    sanityCheck();


    ekacld_c(handle,
	     segmentNumber,
	     columnName,
	     vals,
	     valSizes,
	     nullFlags,
	     records,
	     workIndex);

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    (*env)->ReleaseIntArrayElements(env, jNullFlags, nullFlags, 0);
    (*env)->ReleaseIntArrayElements(env, jRecords, records, 0);    
    (*env)->ReleaseIntArrayElements(env, jValSizes, valSizes, 0);

    (*env)->ReleaseDoubleArrayElements(env, jVals, vals, 0);

    free(workIndex);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekacli
  (JNIEnv* env, jclass jcl, 
   jint handle, 
   jint segmentNumber, 
   jstring jColumnName, 
   jintArray jRecords, 
   jintArray jVals, 
   jintArray jValSizes, 
   jintArray jNullFlags, 
   jboolean isIndexed)
{
    jint* vals = (*env)->GetIntArrayElements(env, jVals, 0);
    int numRows = (*env)->GetArrayLength(env, jVals);

    SpiceInt* sVals = (SpiceInt*)vals;

    jint* records = (*env)->GetIntArrayElements(env, jRecords, 0);
    jint* valSizes = (*env)->GetIntArrayElements(env, jValSizes, 0);
    jint* nullFlags = (*env)->GetIntArrayElements(env, jNullFlags, 0);

    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);

    SpiceInt* workIndex = 
      (SpiceInt*)checked_malloc(env, sizeof(SpiceInt) * numRows);

    sanityCheck();

    ekacli_c(handle,
	     segmentNumber,
	     columnName,
	     sVals,
	     valSizes,
	     nullFlags,
	     records,
	     workIndex);

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    (*env)->ReleaseIntArrayElements(env, jNullFlags, nullFlags, 0);
    (*env)->ReleaseIntArrayElements(env, jRecords, records, 0);    
    (*env)->ReleaseIntArrayElements(env, jValSizes, valSizes, 0);

    (*env)->ReleaseIntArrayElements(env, jVals, vals, 0);

    free(workIndex);
}



JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekbseg
  (JNIEnv* env, jclass jcl, jint jHandle, jstring jTableName, jobjectArray jColumnNames, jobjectArray jColumnDescriptions)
{
   ConstSpiceChar* tableName = (*env)->GetStringUTFChars(env, jTableName, 0);  
   SpiceInt numColumns = (SpiceInt)(*env)->GetArrayLength(env, jColumnNames);
   char* names = 0;
   char* descriptions = 0;
   SpiceInt nameLength = stringArrayToChars(env, jColumnNames, &names); 
   SpiceInt descLength = stringArrayToChars(env, 
                                            jColumnDescriptions, 
                                            &descriptions);
   SpiceInt handle = (SpiceInt)jHandle;
   SpiceInt segNum = 0;

   ekbseg_c ( handle,
              tableName,
              numColumns,
              nameLength,
              names,
              descLength,
              descriptions,
              &segNum );   

   (*env)->ReleaseStringUTFChars(env, jTableName, tableName);

   free(names);
   free(descriptions);

   return (jint)segNum;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekappr
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegmentNumber)
{
   SpiceInt recordNumber = 0;
   
   ekappr_c ( (SpiceInt)jHandle,
              (SpiceInt)jSegmentNumber,
              &recordNumber );

   return (jint)recordNumber;
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekdelr
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegment, jint jRecord)
{
   ekdelr_c ( (SpiceInt)jHandle,
              (SpiceInt)jSegment,
              (SpiceInt)jRecord );
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekinsr
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegment, jint jRecord)
{
   ekinsr_c ( (SpiceInt)jHandle,
              (SpiceInt)jSegment,
              (SpiceInt)jRecord );
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekacec
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegment, jint jRecord, jstring jColumnName, jobjectArray jVals, jboolean jIsNull)
{
    char* vals = 0;
    SpiceInt stringLen = 0;
    int numVals = (*env)->GetArrayLength(env, jVals);

    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);

    sanityCheck();

    stringLen = stringArrayToChars(env, jVals, &vals);

    ekacec_c ( (SpiceInt)jHandle,
               (SpiceInt)jSegment,
               (SpiceInt)jRecord,
               columnName,
               numVals,
               stringLen,
               vals,
               (SpiceBoolean)jIsNull );

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    free(vals);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekaced
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegment, jint jRecord, jstring jColumnName, jdoubleArray jVals, jboolean jIsNull)
{
    double* vals = (*env)->GetDoubleArrayElements(env, jVals, 0);
    int numVals = (*env)->GetArrayLength(env, jVals);
    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);

    sanityCheck();


    ekaced_c((SpiceInt)jHandle,
             (SpiceInt)jSegment,
             (SpiceInt)jRecord,
             columnName,
             numVals,
             vals,
             (SpiceBoolean)jIsNull);

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    (*env)->ReleaseDoubleArrayElements(env, jVals, vals, 0);
}

JNIEXPORT void JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekacei
  (JNIEnv* env, jclass jcl, jint jHandle, jint jSegment, jint jRecord, jstring jColumnName, jintArray jVals, jboolean jIsNull)
{
    jint* vals = (*env)->GetIntArrayElements(env, jVals, 0);
    int numVals = (*env)->GetArrayLength(env, jVals);
    SpiceInt* sVals = (SpiceInt*)vals;

    ConstSpiceChar* columnName = 
      (*env)->GetStringUTFChars(env, jColumnName, 0);

    sanityCheck();


    ekacei_c((SpiceInt)jHandle,
             (SpiceInt)jSegment,
             (SpiceInt)jRecord,
             columnName,
             numVals,
             sVals,
             (SpiceBoolean)jIsNull);

    (*env)->ReleaseStringUTFChars(env, jColumnName, columnName);

    (*env)->ReleaseIntArrayElements(env, jVals, vals, 0);
}


static SpiceBoolean  gWasError = SPICEFALSE;
static SpiceChar gError[128];

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekfind
  (JNIEnv* env, jclass jcl, jstring jQuery)
{
    ConstSpiceChar* query = (*env)->GetStringUTFChars(env, jQuery, 0);    
    SpiceInt numRows = 0;
    SpiceInt lenErrorString = (SpiceInt) sizeof(gError);
    memset(gError, '\0', sizeof(gError));

    ekfind_c ( query,
	       lenErrorString,
	       &numRows,
	       &gWasError,
	       gError );
    
    (*env)->ReleaseStringUTFChars(env, jQuery, query);

    return numRows;
}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1wasError
  (JNIEnv* env, jclass jcl)
{
  return (jboolean) gWasError;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getError
  (JNIEnv* env, jclass jcl)
{
  return (*env)->NewStringUTF(env, gError);
}


static SpiceBoolean gWasNull = SPICEFALSE;
static SpiceBoolean gWasFound = SPICEFALSE;


JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekgc_1fixedLength
  (JNIEnv* env, jclass jcl, jint jSelectIndex, jint jRow, 
   jint jArrayElement, jint jStringLength)
{
  SpiceInt selectIndex  = (SpiceInt) jSelectIndex;
  SpiceInt row          = (SpiceInt) jRow;
  SpiceInt arrayElement = (SpiceInt) jArrayElement;

  jstring ret = 0;
  
  SpiceInt bufLen = (SpiceInt)jStringLength + 1;
  SpiceChar* buf = checked_malloc(env, (int)bufLen);
  memset(buf, '\0', (int)bufLen);

  ekgc_c ( selectIndex,
	   row,
	   arrayElement,
	   bufLen,
	   buf,
	   &gWasNull,
	   &gWasFound);

  ret = (*env)->NewStringUTF(env, buf);
  free(buf);
  return ret;
}

#define VARIABLE_LENGTH_STRING_MAX_LENGTH 1024

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekgc_1variableLength
  (JNIEnv* env, jclass jcl, jint jSelectIndex, jint jRow, jint jArrayElement)
{
  SpiceInt selectIndex  = (SpiceInt) jSelectIndex;
  SpiceInt row          = (SpiceInt) jRow;
  SpiceInt arrayElement = (SpiceInt) jArrayElement;

  jstring ret = 0;

  SpiceChar buf[VARIABLE_LENGTH_STRING_MAX_LENGTH];
  memset(buf, '\0', sizeof(buf));

  ekgc_c ( selectIndex,
	   row,
	   arrayElement,
	   (SpiceInt)sizeof(buf),
	   buf,
	   &gWasNull,
	   &gWasFound);      


  ret = (*env)->NewStringUTF(env, buf);
  return ret;
}

JNIEXPORT jdouble JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekgd
  (JNIEnv* env, jclass jcl, jint jSelectIndex, jint jRow, jint jArrayElement)
{
  SpiceInt selectIndex  = (SpiceInt) jSelectIndex;
  SpiceInt row          = (SpiceInt) jRow;
  SpiceInt arrayElement = (SpiceInt) jArrayElement;

  SpiceDouble data = 0.0;

  ekgd_c ( selectIndex,
	   row,
	   arrayElement,
	   &data,
	   &gWasNull,
	   &gWasFound);

  return (jdouble)data;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ekgi
  (JNIEnv* env, jclass jcl, jint jSelectIndex, jint jRow, jint jArrayElement)
{
  SpiceInt selectIndex  = (SpiceInt) jSelectIndex;
  SpiceInt row          = (SpiceInt) jRow;
  SpiceInt arrayElement = (SpiceInt) jArrayElement;

  SpiceInt data = 0;

  ekgi_c ( selectIndex,
	   row,
	   arrayElement,
	   &data,
	   &gWasNull,
	   &gWasFound);

  return (jint)data;
}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1wasNull
  (JNIEnv* env, jclass jcl)
{
  return (jboolean)gWasNull;
}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1wasFound
  (JNIEnv* env, jclass jcl)
{
  return (jboolean)gWasFound;
}


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1eknelt
  (JNIEnv* env, jclass jcl, jint jColumn, jint jRow)
{
   SpiceInt column = (SpiceInt)jColumn;
   SpiceInt row    = (SpiceInt)jRow;

   SpiceInt ret = eknelt_c(column, row);

   jint jRet = (jint)ret;
   return jRet;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1eknseg
  (JNIEnv* env, jclass jcl, jint jFileHandle)
{
  SpiceInt fileHandle = (SpiceInt) jFileHandle;
  return (jint) eknseg_c(fileHandle);
}



static SpiceEKSegSum gSegmentSummary;


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1ssum
  (JNIEnv* env, jclass jcl, jint jFileHandle, jint jSegmentIndex)
{
  SpiceInt fileHandle = (SpiceInt) jFileHandle;
  SpiceInt segmentIndex = (SpiceInt) jSegmentIndex;
  memset(&gSegmentSummary, 0, sizeof(gSegmentSummary));

  ekssum_c(fileHandle, segmentIndex, &gSegmentSummary);

  return (jint) gSegmentSummary.nrows;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getTableName
  (JNIEnv* env, jclass jcl)
{
  return (*env)->NewStringUTF(env, gSegmentSummary.tabnam);
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getNumColumns
  (JNIEnv* env, jclass jcl)
{
  return (jint) gSegmentSummary.ncols;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getColumnName
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (*env)->NewStringUTF(env, gSegmentSummary.cnames[columnIndex]);
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getDataType
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (jint) gSegmentSummary.cdescrs[columnIndex].dtype;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getStringLength
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (jint) gSegmentSummary.cdescrs[columnIndex].strlen;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getArrayLength
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (jint) gSegmentSummary.cdescrs[columnIndex].size;
}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1isIndexed
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (jboolean) gSegmentSummary.cdescrs[columnIndex].indexd;
}

JNIEXPORT jboolean JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1nullsOk
  (JNIEnv* env, jclass jcl, jint columnIndex)
{
  return (jboolean) gSegmentSummary.cdescrs[columnIndex].nullok;
}



/*********************************************************
 psel
 *********************************************************/

/* globals */
/*         */
/* SPICE_EK_MAXQSEL is listed in SpiceEK.h */

static SpiceEKDataType s_queryItemTypes[SPICE_EK_MAXQSEL];
static SpiceEKExprClass s_queryItemClasses[SPICE_EK_MAXQSEL];
static SpiceChar s_queryItemTableNames[SPICE_EK_MAXQSEL][SPICE_EK_TSTRLN];
static SpiceChar s_queryItemColumnNames[SPICE_EK_MAXQSEL][SPICE_EK_CSTRLN];
static SpiceBoolean s_wasError = SPICEFALSE;
static SpiceChar s_errorMsg[128];


JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1psel
  (JNIEnv* env, jclass jcl, jstring jQuery)
{
   SpiceInt numColumns = -1;
   ConstSpiceChar* query = (*env)->GetStringUTFChars(env, jQuery, 0);    

   SpiceInt beginPositions[SPICE_EK_MAXQSEL];
   SpiceInt endPositions[SPICE_EK_MAXQSEL];
   memset(beginPositions, 0, sizeof(beginPositions));
   memset(endPositions, 0, sizeof(endPositions));

   memset(s_queryItemTypes, -1, sizeof(s_queryItemTypes));
   memset(s_queryItemClasses, -1, sizeof(s_queryItemClasses));
   memset(s_queryItemTableNames, '\0', sizeof(s_queryItemTableNames));
   memset(s_queryItemColumnNames, '\0', sizeof(s_queryItemTableNames));
   s_wasError = SPICEFALSE;
   memset(s_errorMsg, '\0', sizeof(s_errorMsg));

   ekpsel_c(query,
	  (SpiceInt) sizeof(s_errorMsg),
	  SPICE_EK_TSTRLN,
	  SPICE_EK_CSTRLN,
	  &numColumns,
	  beginPositions,
	  endPositions,
	  s_queryItemTypes,
	  s_queryItemClasses,
	  s_queryItemTableNames,
	  s_queryItemColumnNames,
	  &s_wasError,
	  s_errorMsg);
	      
    (*env)->ReleaseStringUTFChars(env, jQuery, query);

    return (jint)numColumns;
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getQueryItemDataType
  (JNIEnv* env, jclass jcl, jint item)
{
   return (jint)s_queryItemTypes[item];
}

JNIEXPORT jint JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getQueryItemClass
  (JNIEnv* env, jclass jcl, jint item)
{
   return (jint)s_queryItemClasses[item];
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getQueryItemTableName
  (JNIEnv* env, jclass jcl, jint item)
{
   return (*env)->NewStringUTF(env, s_queryItemTableNames[item]);
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1getQueryItemColumnName
  (JNIEnv* env, jclass jcl, jint item)
{
   return (*env)->NewStringUTF(env, s_queryItemColumnNames[item]);   
}

JNIEXPORT jdouble JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1str2et
  (JNIEnv *env, jclass jcl, jstring jTime)
{
   ConstSpiceChar* time = 
      (*env)->GetStringUTFChars(env, jTime, 0);

   SpiceDouble et = 0.0;
   str2et_c (time, &et);

   (*env)->ReleaseStringUTFChars(env, jTime, time);

   return (jdouble) et;
}

JNIEXPORT jstring JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1timout
  (JNIEnv* env, jclass jcl, jdouble jEt, jstring jFormat)
{
   SpiceDouble et = (SpiceDouble) jEt;
   ConstSpiceChar* format = 
      (*env)->GetStringUTFChars(env, jFormat, 0);
   
   char ret[128];
   memset(ret, '\0', sizeof(ret));
   
   timout_c ( et, format, sizeof(ret), ret);
   
   (*env)->ReleaseStringUTFChars(env, jFormat, format);
   return (*env)->NewStringUTF(env, ret);   
}

JNIEXPORT jdouble JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1sct2e
  (JNIEnv* env, jclass jcl, jint jSpacecraftId, jdouble jSclk)
{
   SpiceInt spacecraftId = (SpiceInt) jSpacecraftId;
   SpiceDouble sclk = (SpiceDouble) jSclk;
   SpiceDouble et = 0.0;

   sct2e_c ( spacecraftId, sclk, &et );

   return (jdouble) et;
}

JNIEXPORT jdouble JNICALL Java_jpl_mipl_spice_jni_SpiceLib_n_1scs2e
  (JNIEnv* env, jclass jcl, jint jSpacecraftId, jstring jSclk)
{
   SpiceInt spacecraftId = (SpiceInt) jSpacecraftId;
   SpiceDouble et = 0.0;

   ConstSpiceChar* sclk = 
      (*env)->GetStringUTFChars(env, jSclk, 0);

   scs2e_c ( spacecraftId, sclk, &et );
   
   (*env)->ReleaseStringUTFChars(env, jSclk, sclk);
   
   return (jdouble) et;   
}
