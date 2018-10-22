#include "vicmain_c"

/* Testprogramm for Function FILE_NO_PATH.C   */

void main44()

{

char filename[120];
int count;

zvp("filename", filename, &count);

zvmessage("","");
zvmessage("filename from TAE","");
zvmessage(filename,"");

file_no_path(filename);

zvmessage("","");
zvmessage("filename without path","");
zvmessage(filename,"");

zvmessage("","");
zvmessage("TSTFILE_NO_PATH succesfully completed", "");

} 
