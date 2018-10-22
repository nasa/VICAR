C*****************************************************************************
C Unit test program TTIECHV.F for subroutine TIECHV
C 
C Ported to UNIX 7/13/93
C*****************************************************************************
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44()
      real tp(20),tiep(20),conv(2216),isres(404)
      data TIEP/400.,400.,2.6662,179.1568,
     *           400.,500.,8.8652,156.9667,
     *           500.,500.,-13.8541,149.8141,
     *           600.,400.,-47.9389,172.3825,
     *           600.,500.,-39.3800,140.5490/
      data TP/400.,400.,2.6662,179.1568,
     *           400.,500.,8.8652,156.9667,
     *           500.,500.,-13.8541,149.8141,
     *           600.,400.,-47.9389,172.3825,
     *           600.,500.,-39.3800,140.5490/
C**********************************
C FORTRAN-callable
C**********************************
      call xvmessage('**** FORTRAN-callable RFT2 ****',' ')
      call xvmessage(
     *' test of tiechv without distortion correction',' ')
       CALL XVMESSAGE(
     *' these points were determined using PHOTCALV2 with', ' ')
       CALL XVMESSAGE(
     +' vgr image 1636832 using nominal GEOMA correction ',' ')
       CALL XVMESSAGE('parameters',' ')
       call xvmessage(
     +'                      LINE      SAMPLE    LATITUDE 
     * LONGITUDE   ',' ')
       do i=1,5
          call prnt(7,4,tiep(i*4-3),' new tiepoint=')
       enddo
       call tiechv(ind,0,tiep,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tiepoints are in correct format',' ')
       call prnt(4,1,IND,' tiechv return ind=.')
       call getres(isres,7)
       call geomav(conv,7,isres)
       call xvmessage(' tiepoints after geometric correction',' ')
       call tiechv(ind,1,tiep,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tiepoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tiechv return ind=.')
       do i=1,5
          call prnt(7,4,tiep(i*4-3),' new tiepoint=')
       enddo
C*****************************************************************************
c C CALLable
C*****************************************************************************
        call xvmessage('**** C-callable RFT2 ****',' ')
       call xvmessage(
     *' test of tiechv without distortion correction',' ')
       CALL XVMESSAGE(
     *' these points were determined using PHOTCALV2 with', ' ')
       CALL XVMESSAGE(
     +' vgr image 1636832 using nominal GEOMA correction ',' ')
       CALL XVMESSAGE('parameters',' ')
       call xvmessage(
     +'                      LINE      SAMPLE    LATITUDE 
     * LONGITUDE   ',' ')
       do i=1,5
          call prnt(7,4,tp(i*4-3),' new tpoint=')
       enddo
       call tztiechv(ind,0,tp,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tpoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tztiechv return ind=.')
       call getres(isres,7)
       call geomav(conv,7,isres)
       call xvmessage(' tpoints after geometric correction',' ')
       call tztiechv(ind,1,tp,5,conv)
       CALL XVMESSAGE(
     +' A zero returned indicates tpoints are in correct
     * format',' ')
       call prnt(4,1,IND,' tztiechv return ind=.')
       do i=1,5
          call prnt(7,4,tp(i*4-3),' new tpoint=')
       enddo
       return
       end
