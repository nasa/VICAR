/*
 
  Written by Thomas Roatsch, DLR             2-Sep-1993
  Prototypes added by Vadim Parizher, JPL   27-Jun-1997

*/
 
/* Function file_no_path returns the filename without path */

#ifndef FILE_NO_PATH_H
#define FILE_NO_PATH_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NO_PROTO
/*
 *=============================================================================
 * Function prototypes
 *=============================================================================
 */

    void file_no_path(char *filename);
 
#else 

    void file_no_path();

#endif

#ifdef __cplusplus
}
#endif

#endif
