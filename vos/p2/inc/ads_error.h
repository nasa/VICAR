// ads_error.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADS_ERROR_H_INCLUDED
#define ADS_ERROR_H_INCLUDED

/** If there is a SPICE error currently in effect, 
 *  resets the SPICE error status to 'no error' and throws a ToolkitException
 *  based on the current error.
 */
void ads_checkForError();

#endif // !defined ADS_ERROR_H_INCLUDED
