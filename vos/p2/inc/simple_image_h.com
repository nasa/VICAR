$!****************************************************************************
$!
$! Build proc for MIPL module simple_image_h
$! VPACK Version 1.9, Tuesday, March 04, 2014, 11:01:00
$!
$! Execute by entering:		$ @simple_image_h
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module simple_image_h ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$!
$ if (Create_Source .or. Create_Repack) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to simple_image_h.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Build = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("simple_image_h.bld") .eqs. "" then gosub Build_File
$   if (primary .eqs. " ")
$   then
$      @simple_image_h.bld "STD"
$   else
$      @simple_image_h.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create simple_image_h.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack simple_image_h.com -mixed -
	-s SimpleImage.h
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create SimpleImage.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef SIMPLEIMAGE_H
#define SIMPLEIMAGE_H

#include "zvproto.h"

/************************************************************************
 * Simple class to manage an image in memory, and provide inline
 * accessors to it.
 */

template<class T>
class SimpleImage {
    private:
        T *_data;
        int _nl;                // logical image height
        int _ns;                // logical image width
        int _array_width;       // physical size of each line (pixels)
        int _total_size;        // total allocated size, or -1 for no alloc

    public:

	/** Empty image constructor (no data) */
        SimpleImage() { _data = NULL; }

	/** Allocated image constructor */
        SimpleImage(int nl, int ns) {
            _data = NULL;
            alloc(nl, ns);
        }

	/** Create a subimage of the given image.  NOTE: Subimage may be
	 *  freed or deleted at will but do NOT free or delete the primary
	 *  image while any subimages are active!!  We are not reference
	 *  counting subimages.
	 */
	SimpleImage(SimpleImage<T> &parent,
		int sl, int ss, int nl, int ns) {
	    _data = parent.linePtr(sl) + ss;
	    _nl = nl;
	    _ns = ns;
	    _array_width = parent._array_width;
	    _total_size = -1;
	}

	/** Create an image wrapper using external memory.  Note that
	 *  array_width is measured in pixels, not bytes.  As above, we
	 *  set total_size to -1 to indicate we do not own the memory.
	 */
	SimpleImage(T *data, int nl, int ns, int array_width) {
	    _data = data;
	    _nl = nl;
	    _ns = ns;
	    _array_width = array_width;
	    _total_size = -1;
	}

	/** Allocate memory for the image (if needed) */
        void alloc(int nl, int ns) {
            if (_data != NULL && (nl*ns > _total_size)) {
		free();
            }
            if (_data == NULL) {
                _total_size = nl * ns;
                _data = new T[_total_size];
                if (_data == NULL) {
                    zvmessage("Unable to allocate memory for image!","");
                    zabend();           // maybe should return non-0?
                }
                _nl = nl;
                _ns = ns;
                _array_width = ns;
            }
        }

	/** Free the storage.  If size is -1, we don't own the memory. */
	void free() {
	    if (_data != NULL && _total_size != -1)
		delete _data;
	    _data = NULL;
	}

	/** Metadata accessors */
        inline int getNL() { return _nl; }
        inline int getNS() { return _ns; }
        inline int getPhysWidth() { return _array_width; }

	/** Pixel accessors/setters */
        inline T get(int line, int samp)
                { return _data[line * _array_width + samp]; }
        inline void set(int line, int samp, T value)
                { _data[line * _array_width + samp] = value; }

	/** Pixel modifiers */
	inline void addto(int line, int samp, T add)
		{ _data[line * _array_width + samp] += add; }

	/** Pointer to the given image line */
        inline T *linePtr(int line)
                { return _data + line * _array_width; }

	/** Clear the image */
	void zero() {
	    // If we own the memory, we're the parent and thus there are
	    // no bigger images - we can zero the whole array.
	    if (_total_size != -1)
		 memset(_data, 0, _total_size*sizeof(T));
	    // If ns == array_width then there are no gaps... lines are
	    // contiguous so again, we can zero our whole array.
	    else if (_ns == _array_width)
		 memset(_data, 0, _ns * _ns * sizeof(T));
	    // Otherwise we have to do it the hard way, to avoid clobbering
	    // parent data outside our piece.
	    else {
		for (int i = 0; i < _nl; i++) {
		    memset(linePtr(i), 0, _ns*sizeof(T));
		}
	    }
	}
};

#endif

$ VOKAGLEVE
$ Return
$!#############################################################################
