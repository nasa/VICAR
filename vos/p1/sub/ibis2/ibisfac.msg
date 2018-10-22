!
!	Message file for facility IBIS
!
!  This file contains the message id's and detailed texts for
!  the IBIS2 sublib internal errors.
!
!
.KEY ALRDYOPN 	 !Symbolic Name: IBIS_FILE_ALREADY_OPENED
Explanation: 
The file unit passed to IBISFileUnit has already been installed
in another ibis descriptor.

User action:
This is a program error; the cognizant programmer should be notified.

Programmer action:
Make sure that previous IBIS file descriptors that were passed this
VICAR file unit have been closed and deleted.


.KEY CANTTRANS 	 !Symbolic Name: IBIS_CANT_TRANSLATE
Explanation:
Either an ASCII formatted column has just had its U_FORMAT set to
a numerical format, or a numerically formatted column has just had
its U_FORMAT set to an ASCII format. This may happen in either the
IBISColumnSet routines or the IBISRecord routines

User action:
Do not try to perform numerical operations on ASCII columns and vis-versa.
This may also be a program error.

Programmer action:
Make sure to check the FORMAT of a column before setting the
U_FORMAT attribute.


.KEY EMPTYGRP 	 !Symbolic Name: IBIS_GROUP_IS_EMPTY
Explanation:
A requested group of columns turned out to contain no columns. This
may happen if a group-expression contains no columns satisfying expression,
or if a null set of columns is passed into a group or record-defining routine.

User action:
If you are defining a group of columns using a group-expression,
make sure that the defined group contains some columns.

Programmer action:
Make sure that if a null pointer is passed to IBISRecordOpen, that a
nontrivial group is passed in instead.


.KEY GENERR 	 !Generic error in IBIS
Explanation: 
An error status was returned by IBIS.

User action: 
Refer to the Key indicated on the following line.


.KEY GRPEXISTS 	 !Symbolic Name: IBIS_GROUP_ALREADY_EXISTS
Explanation: 
An attempt was made to define a group name which has already been defined.

User action: 
Check the name of the group you have defined against the names defined in
the IBIS property label.


.KEY IMAGEDAT 	 !Symbolic Name: IBIS_CONTAINS_IMAGE_DATA
Explanation:
A VICAR image has been appended to the IBIS file, and the current
operation requires that the IBIS data be extended beyond its current
boundaries; the IBIS-2 library cannot currently move the image data down.

User action:
Copy the IBIS data to a separate file before modifying it.


.KEY INVALCPAR 	 !Symbolic Name: IBIS_COLUMN_PARM_INVALID
Explanation:
An attribute was requested in IBISColumnGet/Set which does not exist.

User action:
This is a program error; contact the cognizant programmer.

Programmer action:
Check the keywords passed into the Column routines for spelling. The
"ibisfile.h" include file has all of the defined keywords for the
C-language interface.


.KEY INVALFMT 	 !Symbolic Name: IBIS_INVALID_FORMAT
Explanation:
A format was specified which was not recognized or invalid for
this routine. This error may also be returned if an attempt is made
to set the pixel format of a file to an ASCII type.

User action:
Make sure the format parameters are spelled correctly. This may also be
a program or subroutine system error; contact the cognizant programmer.

Programmer action:
Check format parameters passed into routines. This error may also result
from an internal IBIS error on closing or deleting a file; if so, contact
the cognizant system programmer.


.KEY INVALNAM 	 !Symbolic Name: IBIS_INVALID_GRPNAME
Explanation:
An attempt was made to name a group using either colons or  non-printable
characters.

User action:
Do not use colons in defining a group name. Any other printable character
is permissible, including spaces. If none were used, this is most likely
a program error; contact the cognizant programmer.

Programmer action:
Make sure that valid strings are being passed into the group
definition routines.


.KEY INVALPAR 	 !Symbolic Name: IBIS_INVALID_PARM
Explanation:
An invalid keyword was passed into an IBIS routine.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check the keywords passed into the IBISFile, IBISGroup and
IBISRecord routines.


.KEY INVALTYP 	 !Symbolic Name: IBIS_INVALID_TYPE
Explanation:
An invalid group type was passed to a routine. 

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check "type" names passed into routines and group-expressions. Valid names
are singular, case-insensitive, and are in the list 
{FORMAT | UNIT | GROUP | LOCAL }. Not all of these are valid for all 
routines; for example, FORMAT groups may not be deleted or defined.


.KEY LASTROW 	 !Symbolic Name: IBIS_LAST_ROW
Explanation:
An attempt was made to read or write a row of data past the last
row of the file.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
In order to append rows to the end of a file, use IBISFileSet to change the
'NR' value of the file first; this will size up the file and change the label
to permit appending new rows.


.KEY LENGTHREQ 	 !Symbolic Name: IBIS_LENGTH_REQUIRED
Explanation:
An attempt was made to access a string-array without specifying the inner
length of the array.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
This error should only arise in the C-interface routines; check that the
"length" parameter was not defaulted for multi-valued string arrays.
If your code is using the FORTRAN bridge routines, this error is an
internal IBIS subroutine error; contact the cognizant system programmer.


.KEY LIMEXCD 	 !Symbolic Name: IBIS_COLUMN_LIMIT_EXCEEDED
Explanation:
An attempt was made to create an IBIS file with more than the current limit.

User action:
An IBIS file currently may have no more than 1024 columns. There is no
limit on the number of rows, so check to see if you can accomplish your
task by storing data in successive rows instead.


.KEY LOCKCOL 	 !Symbolic Name: IBIS_COLUMN_LOCKED
Explanation:
An attempt was made to delete a column which is locked by an open record.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Close all records containing this column before deleting it.


.KEY MEMFAIL 	 !Symbolic Name: IBIS_MEMORY_FAILURE
Explanation:
There is no more memory available for program. 

User action:
This may be a program error. Contact the cognizant programmer.

Programmer action:
A likely cause of this error is the attempt to read in a very large column
of data all at once, whose buffer ate up too much memory for the IBIS routines
to be able to do their work. Try working with only part of a column at a
time, using the "srow, nrows" parameters in IBISColumnRead/Write.
Alternatively, if you are working with records, try sizing down some of
the records (the NR value) with IBISRecordSet to free up some more space.


.KEY MODFORMAT 	 !Symbolic Name: IBIS_CANT_MODIFY_FORMAT
Explanation:
FORMAT groups are implicitly defined by setting the format of the column.
You cannot explicitly create or delete them.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Do not attempt to directly modify FORMAT groups. You can always create other
groups which are copies of the FORMAT group, and then modify as you wish.


.KEY NCREQD 	 !Symbolic Name: IBIS_NC_REQUIRED
Explanation:
This is an IBIS-1 GRAPHICS file, which contains no explicit NC (Dimension)
information.

User action:
You must supply a dimension parameter to a GRAPHICS-1 file, as there is
no default dimension and no dimension information in the file.

Programmer action:
To make an IBIS program "GRAPHICS-1 Friendly", you should provide a "G1DIM"
parameter to permit the specification of graphics file dimensions.


.KEY NOSUCHCOL 	 !Symbolic Name: IBIS_NO_SUCH_COLUMN
Explanation:
A column was referenced which does not exist, according to the current
IBIS property label.

User action:
Check the list of columns requested in your parameter list. This may
also be a program error; inform the cognizant programmer.

Programmer action:
Make sure that the columns passed into the IBISRecord/IBISColumn routines
are all valid columns.



.KEY NOTFOUND 	 !Symbolic Name: IBIS_GROUP_NOT_FOUND
Explanation:
A group was referenced which does not appear in the list of groups defined
for this file.

User action:
Check the list of defined groups in the IBIS property label.


.KEY NOTIBIS 	 !Symbolic Name: IBIS_FILE_IS_NOT_IBIS
Explanation:
The VICAR label of this file neither contains an IBIS property label,
nor does it have the attributes of an IBIS-1 file. 

User action:
Check the file label.


.KEY NOTOPEN 	 !Symbolic Name: IBIS_FILE_NOT_OPEN
Explanation:
The IBIS file descriptor contains a VICAR file unit which is not
currently open.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
If you have called IBISFileUnit, then you must now call IBISFileUnitOpen
to access or create the IBIS file. This also applies to file descriptors
that were closed with the "UKEEP" option in IBISFileClose.


.KEY NSNOTSET 	 !Symbolic Name: IBIS_MUST_SET_NS_FIRST
Explanation:
An attempt was made to change the VICAR image 'FORMAT' or 'HOST' values before
the pixel 'NS' was set.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
To set the VICAR image data fields prior to creating a new IBIS file,
first set the 'NS' sample count; from there you may then modify the
FORMAT and HOST values without error.


.KEY NVALMOD 	 !Symbolic Name: IBIS_INVALID_OPEN_MODE
Explanation:
A mode was chosen in IBISFileOpen or IBISFileUnit which was not "read",
"write", "owrite", or "update".

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Check the keywords passed into IBISFile routines. If you are using "C",
use the macros defined in "ibisfile.h".


.KEY OLDIBIS 	 !Symbolic Name: IBIS_FILE_OLD_IBIS
Explanation:
The requested operation is not supported for old IBIS files. This includes
setting a column format to a size larger than 4-bytes per column element,
and other features introduced in the IBIS-2 format specifications.

User action:
Do not try to set column formats to anything requiring  more than 4 bytes
per element. This may also be a program error.

Programmer action:
Check the file version type before modifying the FORMAT attribute of a column.
The routine IBISColumnMove will also return this error for any IBIS-1
format file.


.KEY RDONLY 	 !Symbolic Name: IBIS_FILE_OPENED_READONLY
Explanation:
An attempt was made to modify an IBIS file which is currently opened read-only.

User action:
This is most likely a program error. Contact the cognizant programmer.

Programmer action:
Open the file with 'update' mode in order to modify it.

.KEY VICMSG	!Error reported to IBIS from VICAR
Explanation:
An error was returned from IBIS from the VICAR2 runtime
library. Refer to the 'VIC2' keyword on the line following.

