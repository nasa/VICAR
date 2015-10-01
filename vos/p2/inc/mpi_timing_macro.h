
/* 
   Copyright JPL (2001)
   Author:  Gerhard Klimeck
*/
/* This file contains several macros that aid with the timing of
   software.  
   A particular piece of code needs to be enclosed by a "TIME_START(foo);" and
   a "TIME_EVAL(foo);" instrumentation.   This will utilize variable
   "time_foo_start" to memorize the starting time and it will add
   the expired time into the variable "time_foo".
   
   These auxialiary variables need to be defined and possibly declared.
   We recommend defining all the timing variables like "foo", "foo2",...etc
   in the main part of the code where the final time reporting will be done.
   Definition is performed with the macro "TIME_DEFINE(foo)".

   If the calls to "TIME_START(foo)" or "TIME_EVAL(foo)" are in a different file
   from the main file one must declare the auxiliary variables using
   "DECLARE(foo)" at the top of the file, after the '#include "timing_macro.h"'
   statement.

   The final timing data can be reported at the end of the run with the 
   statement;
   "TIME_REPORT_MPIID"  which prints out the mpi_id
   "TIME_REPORT(foo)"
   "TIME_REPORT(foo2)"  which prints the times foo and foo2
   "TIME_REPORT_END"    which sends a cariage return and flushes the output.

   "TIME_REPORT_wCOUNT" is used similarly, however it also reports the number of times
   the timer was evaluated and it reports the average time per evaluation.

   It might occur often that several code segments that follow
   each other will be timed.  The sequence
   TIME_EVAL(foo1);
   TIME_START(foo2);
   would call the system for a time information twice, inflating the cost for 
   timing.  We have therefore also the macro
   TIME_EVAL_START(foo1,foo2)
   which only needs one call to the timing function.
   
   Different functions to timing libraries cann be specified 
   in the TIMECALL macro.

   TIMENORM is used to convert from seconds into minutes in the reporting stage.

   Here are two examples applications of the times:

   TIME_DEFINE(total);
   TIME_DEFINE(setup);
   TIME_DEFINE(work);
   TIME_DEFINE(wrapup);
   
   main(){
     ...
     TIME_START(total);       
     TIME_START(setup);
     ....do some work....
     TIME_EVAL_START(setup,work);          // evaluate setuptime and start work 
     ...do some work....
     TIME_EVAL(work1);
     ...do some work that is only timed by the "total" count now
     for/while{
       TIME_START(bogus);                   // start/restart the timer bogus
       ...
       TIME_EVAL_START(bogus,oh_man);       // accumulate the time into bogus and start oh_man
       ...
       TIME_EVAL(oh_man);                   // accumulate the time into oh_man
       .... this work right here is only accounted for by "total"
     }
     ...do some work that is only timed by the "total" count now
     
     TIME_EVAL(total);

     // generate 2 separate time reports, just for kicks.
     TIME_REPORT_MPIID;    // this is an optional notification of the CPU id
     TIME_REPORT(total);
     TIME_REPORT(setup);
     TIME_REPORT(work);
     TIME_REPORT_END;

     TIME_REPORT_MPIID;    // this is an optional notification of the CPU id
     TIME_REPORT(bogus);
     TIME_REPORT(oh_man);
     TIME_REPORT_END;

     return;
   }
   
   A reporting option is also available that gatheres all the timing data
   accross all the CPU's and puts into a single file.
   The option is TIME_GATHER_REPORT.  It is used similar to TIME_REPORT 
   and creates a file entitled "timing_data_#", where # is the number of CPUs
   that are used in the run.  The file contains one line with the variable/tag 
   name that is being timed and then a list of times for CPUs 0..N-1.  Multiple
   calls to TIME_GATHER_REPORT get appended into a single file.  The usage is as follows:
   TIME_GATHER_REPORT(total);
   TIME_GATHER_REPORT(setup);
   TIME_GATHER_REPORT(work);



   There is also a time array structure available which
   enables timing within a loop.  
   TIME_ARRAY_DEFINE(foo,max_arraylength), TIME_ARRAY_START(foo), and TIME_ARRAY_EVAL(foo)
   are similar in their functionality to TIME_DEFINE, TIME_START, and TIME_EVAL.
   However subsequent calls to TIME_ARRAY_START/TIME_ARRAY_EVAL do not accumulate the
   results into one variable, as TIME_START/TIME_EVAL do, but they keep track of the 
   individual timing values.  
   So each time TIME_ARRAY_START/EVAL is called a new timer is started and stored.

   The memory to the array of times is allocated statically and must be given
   in the definition of TIME_ARRAY_DEFINE(foo,max_arraylength).
   If more calls to  TIME_ARRAY_START/EVAL are prefromed than max_arraylength
   the timing data is just added into the last array element.

   There may be a need to count some events within one of these timing evaluations,
   such as number of calls to an expensive computation.
   That can be done with TIME_ARRAY_INCR_CNT.
   
   The structure of the calls is as follows:

   TIME_ARRAY_DEFINE(foo);
   for/while{
      TIME_ARRAY_START(foo)      
      ....do some work....
               ...do another loop
               for/while{ 
                  ....do some work
                  now we have the ability to count some indicences here with the following statement
                  TIME_ARRAY_INCR_CNT(foo);
    
                  ....maybe this is more compicated and the time consuming part will be
                      called again...
                  for/while{
		      ...do some work
                      TIME_ARRAY_INCR_CNT(foo);
                  }
               }
      ....do some work....
      TIME_ARRAY_EVAL(foo)
   }

*/

#if defined(ENABLE_MPI) && defined(MPI_TIMING)

#define auxMAX(A,B) ((A) > (B) ? (A) : (B))
#define auxMIN(A,B) ((A) < (B) ? (A) : (B))
#define TIMECALL              MPI_Wtime()
#define TIMENORM              60.0          /* Always define >0 */
#define TIME_DEFINE(X)        double auxtime_##X##_start,auxtime_##X##=0.0;int auxcount_##X##=0
#define TIME_DECLARE(X)       extern double auxtime_##X##_start; extern double auxtime_##X##;extern int auxcount_##X## 
#define TIME_START(X)         auxtime_##X##_start = TIMECALL
#define TIME_EVAL(X)          auxtime_##X## += TIMECALL - auxtime_##X##_start,auxcount_##X##++
#define TIME_EVAL_START(X,Y)  auxtime_##Y##_start=TIMECALL, auxtime_##X##+=auxtime_##Y##_start-auxtime_##X##_start,auxcount_##X##++
#define TIME_REPORT_MPIID     printf("CPU=%3d ",mpi_id)
#define TIME_REPORT(X)        printf(###X## "=%5.3g ",auxtime_##X##/TIMENORM)
#define TIME_REPORT_wCOUNT(X) printf(###X## "=%5.3g m/%d calls (%g sec/call) ",auxtime_##X##/TIMENORM,auxcount_##X##,auxtime_##X##/auxMAX(1,auxcount_##X##))
#define TIME_REPORT_END       printf("\n"),fflush(stdout)

#define TIME_ARRAY_DEFINE(X,Y) double auxtimearray_##X##[##Y##], auxtimearray_##X##_start; int auxcountarray_indiv_##X##=0, auxcountarray_##X##[##Y##], auxtimearray_max_##X##=##Y## 
#define TIME_ARRAY_DECLARE(X) extern double auxtimearray_##X##[];extern double auxtimearray_##X##_start; extern int auxcountarray_indiv_##X##; extern int auxcountarray_##X##[]; extern int auxtimearray_max_##X## 
#define TIME_ARRAY_START(X)   auxtimearray_##X##_start=TIMECALL,auxcountarray_##X##[auxcountarray_indiv_##X##]=0
#define TIME_ARRAY_EVAL(X)    auxtimearray_##X##[auxcountarray_indiv_##X##]=TIMECALL-auxtimearray_##X##_start,auxcountarray_indiv_##X##=auxMIN(auxtimearray_max_##X##,auxcountarray_indiv_##X##+1)
#define TIME_ARRAY_INCR_CNT(X) auxcountarray_##X##[auxcountarray_indiv_##X##]++
#define TIME_ARRAY_REPORT(X) {int ini;for(ini=0;ini<auxcountarray_indiv_##X##;ini++)printf("CPU=%d array_" ###X## "=%d: %5.3g min/%d calls  -> %g sec/call\n",mpi_id,ini,auxtimearray_##X##[ini]/TIMENORM, auxcountarray_##X##[ini], auxtimearray_##X##[ini]/auxcountarray_##X##[ini]);if(auxcountarray_indiv_##X## == auxtimearray_max_##X## )printf("CPU=%d array_" ###X## "=%d may have multiple timing incidences.\n",mpi_id,auxcountarray_indiv_##X## -1);fflush(stdout);}
#define TIME_GATHER_REPORT(X)  {double *auxtime_gatherarray_##X## = (double *) calloc ((mpi_numprocs + 1), sizeof (double));MPI_Gather(&auxtime_##X##, 1, MPI_DOUBLE, auxtime_gatherarray_##X##, 1, MPI_DOUBLE, mpi_masterid, MPI_COMM_WORLD);if (mpi_id==mpi_masterid){int ini;char aux_filename[100];FILE *auxgather_fp=NULL;sprintf(aux_filename,"timing_data_%d",mpi_numprocs);auxgather_fp=fopen(aux_filename,"a");fprintf(auxgather_fp,###X## " "); for (ini=0;ini<mpi_numprocs;ini++){fprintf(auxgather_fp,"%g ",auxtime_gatherarray_##X##[ini]);}fprintf(auxgather_fp,"\n"); fclose(auxgather_fp);}free(auxtime_gatherarray_##X##);}

#else

#define TIME_DEFINE(X)
#define TIME_DECLARE(X)
#define TIME_START(X)
#define TIME_EVAL(X)
#define TIME_EVAL_START(X,Y)
#define TIME_REPORT_MPIID     
#define TIME_REPORT(X)        
#define TIME_REPORT_wCOUNT(X)        
#define TIME_REPORT_END       
#define TIME_ARRAY_DEFINE(X,Y)  
#define TIME_ARRAY_DECLARE(X)  
#define TIME_ARRAY_START(X)   
#define TIME_ARRAY_EVAL(X)    
#define TIME_ARRAY_INCR_CNT(X) 
#define TIME_ARRAY_REPORT(X) 
#define TIME_GATHER_REPORT(X)

#endif

