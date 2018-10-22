// ads_DefaultDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DEFAULT_DATA_FINDER_H_INCLUDED
#define ADS_DEFAULT_DATA_FINDER_H_INCLUDED


#include "ads_MissionDataFinder.h"
#include "ads_DefaultGeometryFinder.h"

using namespace jpl::mipl::spice::corba;

/**
 *  Finds image information.  
 *  A default implementation of ads_MissionDataFinder.
 */
class ads_DefaultDataFinder : public ads_MissionDataFinder
{
public:

   /** Creates a new DefaultDataFinder */
   ads_DefaultDataFinder();
  
  /** Destructor. */
  virtual ~ads_DefaultDataFinder();
     
  /** Inherited from ads_MissionDataFinder. */
  virtual void getGeometryNoUpdates (int instNaifId,
                                     int targetNaifId,
                                     const char* utcTime,
                                     const char* referenceFrame,
                                     double tolerance,
                                     Geometry_out theGeometry,
                                     GeometryMetaData_out metaData)
     throw ( SpiceLib::ToolkitException,
             MiplSpiceLib::PointingNotFound );

   /** Inherited from ads_MissionDataFinder. */
   virtual void getGeometry ( const ImageData & image,
                              const GeometryMetaData & criteria,
                              Geometry_out theGeometry,
                              GeometryMetaData_out metaData )
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );
          
   /** Inherited from ads_MissionDataFinder. */
   virtual void storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData
      )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException);

   /** Inherited from ads_MissionDataFinder. */
   virtual void getInstrument ( const ImageData & image,
                                InstrumentData_out theInstrument)
      throw ( SpiceLib::ToolkitException );
          
   /** Inherited from ads_MissionDataFinder. */
   virtual void getTarget ( const ImageData & image,
                            Target_out theTarget )
      throw ( SpiceLib::ToolkitException );

};


#endif // !defined ADS_DEFAULT_DATA_FINDER_H_INCLUDED
