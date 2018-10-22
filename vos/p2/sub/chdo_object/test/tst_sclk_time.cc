/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

/**  History:                                                           **/
/**  6-16-1998  T. Nguyen  For Y2K task,  added test cases for testing  **/
/**                        time routines.                               **/
/*************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SfocTime.h"
#include "SclkTime.h"
#include "rts_time.h"
#include "return_status.h"

#define  MAX_RANGE_YEARS        136
#define  DAYS_PER_LEAP          366
#define  DAYS_PER_YEAR          365
#define  SECONDS_PER_DAY        86400


void	ingest_test(int);
void	inverse_test(int);
void	specific_test(int);
void	test_clock(char *, int);

main()
{ int	lc,
	NewEpoch,
	Status,
	Year;
  char	utc[24];
  SclkTime	PlayThing(1980);


  for (Year=DEFAULT_SCLK_EPOCH; Year<=DEFAULT_SCLK_EPOCH+30; Year+=9)
  {
    printf("\n*********************************************");
    printf("\n*********************************************");
    printf("\n***                                       ***");
    printf("\n***     Checks for Epoch Year: %-4d       ***",Year);
    printf("\n***                                       ***");
    printf("\n*********************************************");
    printf("\n*********************************************\n");

    ingest_test(Year);

    specific_test(Year);

    inverse_test(Year);

  }


  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Epoch Conversion Checks        ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");

  for (lc=1999; lc<2009; lc++)
  { for (Year=1; Year<45; Year+=5)
    { PlayThing.changeEpoch(lc,0);
      sprintf(utc,"%4d-01-01T00:00:00.000",PlayThing.epoch());
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }

      PlayThing.changeEpoch(PlayThing.epoch()-Year);
      if (strcmp(utc,PlayThing.formatted(0)) != 0)
      { printf("\n*** Failure: %s -(%u)-> %s\n",
                utc,PlayThing.seconds(),PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }

//      printf("%s -[%d]-> %s (%u)\n\n",
//             utc,PlayThing.epoch(),PlayThing.formatted(0),
//             PlayThing.seconds());
    }
    if (RTN_SUCCESS(Status))
    { printf("\nVerified EPOCH conversion from %d to ...\n\t",lc);
      for(Year=1; Year<45; Year+=5) printf("  %04d",(lc-Year));
      printf("\n");
    }
  }

  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***        Misc Capability Checks         ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);

  printf("\n*** Increment Time Values\n");
  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { printf("     Current UTC: %s \n",utc);
    PlayThing.incrementSeconds(SECONDS_PER_DAY);
    printf("     Added 1 Day: %s\n",PlayThing.formatted());
    PlayThing.incrementPartialSeconds(128);
    printf("Added 1/2 Second: %s\n",PlayThing.formatted());
  }


  printf("\n*********************************************");
  printf("\n*********************************************");
  printf("\n***                                       ***");
  printf("\n***      Error Identification Checks      ***");
  printf("\n***                                       ***");
  printf("\n*********************************************");
  printf("\n*********************************************\n");


  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);

  strcpy(utc,rts_utc_time());
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  } else
  { Status = PlayThing.incrementSeconds(-(PlayThing.seconds()+10));
    if (RTN_FAILURE(Status))
    { printf("\nDecrementing: %s\n",RTN_DFLT_MSG(Status));
      printf("\t%s\n",PlayThing.errorMessage());
      printf("\t%s\n",utc);
    }
  }


  sprintf(utc,"%4d-13-01T00:00:00.000",PlayThing.epoch()+34);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-02-29T00:00:00.000",PlayThing.epoch()+45);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-429T00:00:00.000",PlayThing.epoch()+50);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-029T00:00:00.000",PlayThing.epoch()-5);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  sprintf(utc,"%4d-10-31T00:00:00.000",PlayThing.epoch()+MAX_RANGE_YEARS);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);
  sprintf(utc,"%4d-07-04T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() + 10;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }

  PlayThing.changeEpoch(DEFAULT_SCLK_EPOCH,0);
  sprintf(utc,"%4d-11-11T00:00:00.000",PlayThing.epoch()+10);
  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("\nIngestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%s\n",utc);
  }
  NewEpoch = PlayThing.epoch() - MAX_RANGE_YEARS;
  Status = PlayThing.changeEpoch(NewEpoch);
  if (RTN_FAILURE(Status))
  { printf("\nEPOCH Change: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",PlayThing.errorMessage());
    printf("\t%d -> %d for %s\n",PlayThing.epoch(),NewEpoch,utc);
  }

  return RTN_NORMAL;
}
//////////////////////////////////////////////////////////////////////////////
//
//				specific_test
//
//////////////////////////////////////////////////////////////////////////////

void	specific_test(
  int	NewEpoch)
{ char	utc[24];

  printf("\n***\n***  Specific Time and Increment Checks %s\n***\n",
         "- expect millisecond round-off errors");

  strcpy(utc,rts_utc_time());
  printf(" Current UTC: %s \n",utc);
  test_clock(utc,NewEpoch);

  if (NewEpoch < 2000)
  { strcpy(utc,"2000-01-01T00:00:00.001");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-02-28T00:00:00.010");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-02-29T00:00:00.100");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-03-01T00:00:00.020");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-12-30T00:00:00.200");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2000-12-31T00:00:00.005");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  if (NewEpoch < 2009)
  { strcpy(utc,"2009-12-31T00:00:00.500");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2009-12-30T00:00:00.999");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  if (NewEpoch < 2100)
  { strcpy(utc,"2100-12-31T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2101-01-01T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);

    strcpy(utc,"2101-01-02T00:00:00.000");
    printf(" Testing UTC: %s \n",utc);
    test_clock(utc,NewEpoch);
  }

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//                              inverse_test
//
//////////////////////////////////////////////////////////////////////////////

void	inverse_test(
  int	NewEpoch)
{ int	Ydays,
	Year,
	LeapYear,
	Status;
  unsigned int	Tseconds;
  char	utc[24];
  SclkTime	PlayThing(NewEpoch);

  printf("\n***\n***  Iterative Inverse Checks (%d)\n***\n\n",NewEpoch);

  sprintf(utc,"%4d-01-01T00:00:00.000",NewEpoch);
  printf("Start Date: %s  ... incrementing by whole days\n",utc);

  Status = PlayThing.ingestUTC(utc);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n",utc);
    return;
  }


  Tseconds = 0;
  for (Year=NewEpoch; Year<(NewEpoch+MAX_RANGE_YEARS); Year++)
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    for (Ydays=1; Ydays<=(LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR); Ydays++)
    { sprintf(utc,"%4d-%03dT00:00:00.000",Year,Ydays);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }

      if (Tseconds != PlayThing.seconds())
      { printf("(%d) %s %u != %u\n\n",NewEpoch,utc,Tseconds,PlayThing.seconds());
        Status = RTN_ERROR;
        break;
      } else if (strcmp(utc,PlayThing.formatted(1)) != 0)
      { printf("*** Failure: %s -(%d)-> %s\n",
               utc,NewEpoch,PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
      Tseconds += SECONDS_PER_DAY;
    }
    if (RTN_FAILURE(Status)) break;
  }
  
  if (RTN_FAILURE(Status))
     printf("Final Date: %d-%03d (%u) vs %s (%u)\n\n",
            Year,Ydays,Tseconds,PlayThing.formatted(1),PlayThing.seconds());
  else  printf("Final Date: %d-%03d (%u) vs %s (%u)\n\n",
               (Year-1),(Ydays-1),(Tseconds-SECONDS_PER_DAY),
               PlayThing.formatted(1),PlayThing.seconds());


  return;
}


//////////////////////////////////////////////////////////////////////////////
//
//				ingest_test
//
//////////////////////////////////////////////////////////////////////////////

void	ingest_test(
  int	NewEpoch)
{ int	lc,
	Tdays,
	Ydays,
	Year,
	LeapYear,
	Status;
  char	utc[24];
  SclkTime	PlayThing(NewEpoch);

  printf("\n***\n***  Time Ingest Checks (%d)\n***\n",NewEpoch);

  Status = RTN_NORMAL;
  Year = PlayThing.epoch();
  Ydays = 0;
  Tdays = 0;
  do
  { LeapYear = ((Year % 4) == 0) && ((Year % 100) ? 1 : ((Year % 400) == 0));
    Ydays = (LeapYear ? DAYS_PER_LEAP : DAYS_PER_YEAR);
    for (lc = 1; lc<=Ydays; lc++)
    { sprintf(utc,"%04d-%03dT00:00:00.000",Year,lc);
      Status = PlayThing.ingestUTC(utc);
      if (RTN_FAILURE(Status))
      { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
        printf("\t%s\n",PlayThing.errorMessage());
        printf("\t%s\n",utc);
        break;
      }
      if ((Tdays*SECONDS_PER_DAY) != PlayThing.seconds())
      { printf("\n***Failure: %s -(%u vs %u)-> %s\n",
               utc,Tdays*SECONDS_PER_DAY,PlayThing.seconds(),
               PlayThing.formatted(1));
        Status = RTN_ERROR;
        break;
      }
      Tdays++;
    }
    if (RTN_FAILURE(Status)) break;
    Year++;
  } while (Year < PlayThing.epoch()+MAX_RANGE_YEARS);

  printf("\nChecked: %d -> %d  Total Seconds: %u(control) vs %u(test)\n\n",
         PlayThing.epoch(),Year,(Tdays-1)*SECONDS_PER_DAY,PlayThing.seconds());

  return;
}

//////////////////////////////////////////////////////////////////////////////
//
//				test_dates
//
//////////////////////////////////////////////////////////////////////////////

void test_clock(
  char *utcTime,
  int	NewEpoch)
{ int	Status;
  char		TimeBufs[2][128];
  SclkTime	PlayThings[2];

  PlayThings[0].changeEpoch(NewEpoch,0);
  PlayThings[1].changeEpoch(NewEpoch,0);

  strcpy(TimeBufs[0],utcTime);

  Status = PlayThings[0].ingestUTC(TimeBufs[0]);
  if (RTN_FAILURE(Status))
  { printf("Ingestion: %s\n",RTN_DFLT_MSG(Status));
    printf("\t%s\n\n",PlayThings[0].errorMessage());
    return;
  }

  printf("  Julian UTC: %s    (%u.%03d)\n",
         PlayThings[0].formatted(1),
         PlayThings[0].seconds(),PlayThings[0].partialSeconds());

  PlayThings[1] = PlayThings[0];
  PlayThings[1].incrementSeconds(3600*24);
  printf("Tomorrow UTC: %s  (%u.%03d)\n",
         PlayThings[1].formatted(0),
         PlayThings[1].seconds(),PlayThings[1].partialSeconds());

  if (PlayThings[0] > PlayThings[1])
     printf("Today follows Tomorrow ... *** wrong ***\n\n");
  else if (PlayThings[0] == PlayThings[1])
     printf("Today is the same as Tomorrow ... *** wrong ***n\n");
  else printf("Tomorrow follows Today\n\n");

  return;
}

