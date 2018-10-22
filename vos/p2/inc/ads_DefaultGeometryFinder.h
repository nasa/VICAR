// ads_DefaultGeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED
#define ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

#include "jpl_mipl_spice_corba.C.h"

using namespace jpl::mipl::spice::corba;

/**
 *  A default implementation of GeometryFinder which will work for
 *  instruments which have their own C kernel.
 */
class ads_DefaultGeometryFinder : public ads_GeometryFinder
{
public:
  
  /** Destructor. */
  virtual ~ads_DefaultGeometryFinder();
     
  /** Inherited from ads_GeometryFinder. */
  virtual void getGeometry (int instNaifId,
                            int targetNaifId,
                            const char* utcTime,
                            const char* referenceFrame,
                            double tolerance,	
                            Geometry_out theGeometry,
                            GeometryMetaData_out metaData)
    throw ( SpiceLib::ToolkitException,
	    MiplSpiceLib::PointingNotFound );

};


#endif // !defined ADS_DEFAULT_GEOMETRY_FINDER_H_INCLUDED
