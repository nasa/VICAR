// ads_GeometryFinder.h
//
// Apr 18, 2002
// Michael Brady

#if !defined ADS_GEOMETRYFINDER_H_INCLUDED
#define ADS_GEOMETRYFINDER_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

using namespace jpl::mipl::spice::corba;

/**
 * Abstract class which defines the GeometryFinder interface.
 * This is the base class of a decorator hierarchy.
 * (For more information on the Decorator pattern, see "Design Patterns"
 *  by Gamma, et al).
 */
class ads_GeometryFinder
{
public:


   virtual ~ads_GeometryFinder() = 0;

   virtual void getGeometry (int instNaifId,
                             int targetNaifId,
                             const char* utcTime,
                             const char* referenceFrame,
                             double tolerance,
                             Geometry_out theGeometry,
                             GeometryMetaData_out metaData )
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound ) = 0;   

};

#endif // !defined ADS_GEOMETRYFINDER_H_INCLUDED
