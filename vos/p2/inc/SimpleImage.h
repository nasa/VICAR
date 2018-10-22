#ifndef SIMPLEIMAGE_H
#define SIMPLEIMAGE_H

#include "zvproto.h"
#include <string.h>     // for memset()
/************************************************************************
 * Simple class to manage an image in memory, and provide inline
 * accessors to it.
 *
 * The logical size of the image is nb/nl/ns.  But that need not be the
 * physical size of the array.  This allows subimages to be created within
 * a larger image.  This is supported by line_stride and band_stride, which
 * are the number of pixels (not bytes!) between the same pixel on adjacent
 * lines, and the same line/samp between adjacent bands.  Basically to move
 * by a line, add line_stride, and to move by a band, add band_stride.
 * The stride's are long int to accommodate really big images if needed.
 * It is overkill for line_stride to be long int but it's cleaner to have it
 * match band_stride.
 *
 * There is both a 2-D and a 3-D API.  Either may be used with any image.
 * The 2D API creates a SimpleImage with one band, and assumes band=0 on
 * accessor routines.  Thus the 2D API will access the first band of a
 * multiband image.  Of course the 3D API is better for multi-band images.
 *
 * Subimages, or SimpleImage wrappers pointing to external memory, do not
 * own their memory and thus cannot free it.  This is controlled by total_size;
 * a value of -1 means we do not own the memory.
 *
 * This is intentionally a very simple, lightweight class!  No error checking
 * is done; if you tell it to write off the edge of the image it will happily
 * do so.  It is up to the caller to avoid this.
 *
 * The entire implementation is in the .h file, making it easy for the compiler
 * to inline most of these functions (especially the critical get/set).  By
 * the same token, this is not subclassable, as that would require a virtual
 * function pointer which would negate inlining.  Fortunately the template
 * stuff does not negate inlining.
 */

template<class T>
class SimpleImage {
    private:
        T *_data;
	int _nb;		// logical # bands
        int _nl;                // logical image height
        int _ns;                // logical image width
	long int _band_stride;	// physical size of a band (pixels),
				// i.e. dist btwn same pixel in adjacent bands
        long int _line_stride;  // physical size of each line (pixels),
				// i.e. dist btwn same pixel in adjacent lines
        long int _total_size;   // total allocated size, or -1 for no alloc
      
        /** Note: This is used for the public getVicarTypeString 
         *  function to pass type to private _getVicarTypeString 
         *  functions. It should never be used for other purposes.
         */ 
        template<class IDENTITY_TEMPLATE> 
        struct identity {
            typedef IDENTITY_TEMPLATE type;
        }; 
       
        /** Handling all the undefined vicar types.
         *  Note: this function serves as a template, it should 
         *  never be called directly. 
         */ 
        template <class VTYPE_TEMPLATE>
        inline const char *_getVicarTypeString(identity<VTYPE_TEMPLATE>)
                 { return "Undefined"; }

        /** The functions below handle valid vicar types */
        inline const char *_getVicarTypeString(identity<short int>) 
                 { return "HALF"; }
        inline const char *_getVicarTypeString(identity<float>) 
                 { return "REAL"; }
        inline const char *_getVicarTypeString(identity<double>) 
                 { return "DOUB"; }
        inline const char *_getVicarTypeString(identity<unsigned char>) 
                 { return "BYTE"; }
        inline const char *_getVicarTypeString(identity<int>) 
                 { return "FULL"; }
        inline const char *_getVicarTypeString(identity<char>) 
                 { return "BYTE"; }
        inline const char *_getVicarTypeString(identity<unsigned short int>)
                 { return "HALF"; }
        inline const char *_getVicarTypeString(identity<unsigned int>) 
                 { return "FULL"; }

    public:

	/** Empty image constructor (no data) */
        SimpleImage() { _data = NULL; _total_size = 0; }

	/** Allocated image constructor */
        SimpleImage(int nl, int ns) {
            _data = NULL;
	    _total_size = 0;
            alloc(nl, ns);
        }

	/** Allocated image constructor, 3D */
        SimpleImage(int nb, int nl, int ns) {
            _data = NULL;
	    _total_size = 0;
            alloc(nb, nl, ns);
        }

	/** Create a subimage of the given image.  NOTE: Subimage may be
	 *  freed or deleted at will but do NOT free or delete the primary
	 *  image while any subimages are active!!  We are not reference
	 *  counting subimages.
	 */
	SimpleImage(SimpleImage<T> &parent,
		int sl, int ss, int nl, int ns) {
	    _data = parent.linePtr(sl) + ss;
	    _nb = 1;
	    _nl = nl;
	    _ns = ns;
	    _line_stride = parent._line_stride;
	    _band_stride = parent._band_stride;
	    _total_size = -1;
	}
	    
	/** Create a subimage of the given image, 3D.  NOTE: Subimage may be
	 *  freed or deleted at will but do NOT free or delete the primary
	 *  image while any subimages are active!!  We are not reference
	 *  counting subimages.
	 */
	SimpleImage(SimpleImage<T> &parent,
		int sb, int sl, int ss, int nb, int nl, int ns) {
	    _data = parent.linePtr(sb, sl) + ss;
	    _nb = nb;
	    _nl = nl;
	    _ns = ns;
	    _line_stride = parent._line_stride;
	    _band_stride = parent._band_stride;
	    _total_size = -1;
	}

	/** Create an image wrapper using external memory.  Note that
	 *  line_stride is measured in pixels, not bytes.  As above, we
	 *  set total_size to -1 to indicate we do not own the memory.
	 */
	SimpleImage(T *data, int nl, int ns, long int line_stride) {
	    _data = data;
	    _nb = 1;
	    _nl = nl;
	    _ns = ns;
	    _line_stride = line_stride;
	    // Don't have anything better for band_stride...
	    _band_stride = _line_stride * _nl;
	    _total_size = -1;
	}

	/** Create an image wrapper using external memory, 3D.  Note that
	 *  strides are measured in pixels, not bytes.  As above, we set
	 *  total_size to -1 to indicate we do not own the memory.
	 */
	SimpleImage(T *data, int nb, int nl, int ns,
		    long int line_stride, long int band_stride) {
	    _data = data;
	    _nb = nb;
	    _nl = nl;
	    _ns = ns;
	    _line_stride = line_stride;
	    _band_stride = band_stride;
	    _total_size = -1;
	}


	/** Allocate memory for the image (if needed) */
        void alloc(int nl, int ns) {
	    alloc(1, nl, ns);
	}

	/** Allocate memory for the image (if needed), 3D */
        void alloc(int nb, int nl, int ns) {
            if (_total_size == -1) {
                if (nl != _nl || ns != _ns || nb != _nb) {
                    zvmessage("Unable to modify size of image using external memory wrapper!","");
                    zabend();
                }
            } else {
                if (_data != NULL && (((long int)nb)*nl*ns > _total_size)) {
		    free();
                }
                if (_data == NULL) {
                    _total_size = ((long int)nb) * nl * ns;
                    _data = new T[_total_size];
                    if (_data == NULL) {
                        zvmessage("Unable to allocate memory for image!","");
                        zabend();           // maybe should return non-0?
                    }
		}
		// Whether we re-alloc'd or not, set the sizes properly...
		_nb = nb;
                _nl = nl;
                _ns = ns;
                _line_stride = (long int)_ns;
		_band_stride = ((long int)_ns) * _nl;
            }
        }

	/** Free the storage.  If size is -1, we don't own the memory. */
	void free() {
	    if (_data != NULL && _total_size != -1)
		delete _data;
	    _data = NULL;
	}

	/** Metadata accessors */
        inline int getNB() { return _nb; }
        inline int getNL() { return _nl; }
        inline int getNS() { return _ns; }
	/** Deprecated name, use getLineStride() instead */
        inline long int getPhysWidth() { return _line_stride; }
        inline long int getLineStride() { return _line_stride; }
        inline long int getBandStride() { return _band_stride; }

	/** Convenience routines */
	/** 2D version does NOT check band at all */
	inline int inBounds(int line, int samp)
		{ return (line >= 0 && samp >= 0 && line < _nl && samp < _ns); }

	/** 3D version */
	inline int inBounds(int band, int line, int samp)
		{ return (band >= 0 && line >= 0 && samp >= 0 &&
			  band < _nb && line < _nl && samp < _ns); }

	/** Pixel accessors/setters, 2D */
        inline T get(int line, int samp)
                { return _data[line * _line_stride + samp]; }
        inline void set(int line, int samp, T value)
                { _data[line * _line_stride + samp] = value; }

	/** Pixel accessors/setters, 3D */
        inline T get(int band, int line, int samp)
                { return _data[band*_band_stride + line*_line_stride + samp]; }
        inline void set(int band, int line, int samp, T value)
                { _data[band*_band_stride + line*_line_stride + samp] = value; }

	/** Pixel modifiers, 2D */
	inline void addto(int line, int samp, T add)
		{ _data[line * _line_stride + samp] += add; }

	/** Pixel modifiers, 3D */
	inline void addto(int band, int line, int samp, T add)
		{ _data[band*_band_stride + line*_line_stride + samp] += add; }

	/** Pointer to the given image line, 2D */
        inline T *linePtr(int line)
                { return _data + line * _line_stride; }

	/** Pointer to the given image line, 3D */
        inline T *linePtr(int band, int line)
                { return _data + band * _band_stride + line * _line_stride; }

	/** Clear the image */
	void zero() {
	    // If we own the memory, we're the parent and thus there are
	    // no bigger images - we can zero the whole array.
	    if (_total_size != -1)
		 memset(_data, 0, _total_size*sizeof(T));
	    // If ns == line_stride and nl*ns = band_stride then there are no
	    // gaps... lines are contiguous so we can zero our whole array.
	    else if (_ns == _line_stride && ((long int)_nl)*_ns == _band_stride)
		 memset(_data, 0, _ns * _nl *_nb * sizeof(T));
	    // Otherwise we have to do it the hard way, to avoid clobbering
	    // parent data outside our piece.
	    else {
		for (int j = 0; j < _nb; j++) {
		    for (int i = 0; i < _nl; i++) {
		        memset(linePtr(j, i), 0, _ns*sizeof(T));
		    }
		}
	    }
	}
      
        /** Return VICAR type */ 
        inline const char *getVicarTypeString()
                { return _getVicarTypeString(identity<T>()); }
};
#endif
