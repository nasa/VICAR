c-----Program tpbname
c-----Test program for subroutine PBNAME and PBID
c
      include 'VICMAIN_FOR'
      subroutine main44
      character*12 name,name1
      character*35 pbuf1
      character*30 pbuf
      integer*2 ibuf(15)
c
      call MVCL(' id = xxx  name = xxxxxxxx',ibuf,26)
      call MVLC(ibuf, PBUF, 26)
c
      do 10 i=1,999
        call Pbname(i, name, *10)
        call Pbid(name, j, *10)
        write(Pbuf(7:9),'(I3)') j
        do 20  k=1,12
          k1 = k + 18
          PBUF(k1:k1) = Name(k:k)
20      continue          
        call Xvmessage(Pbuf, ' ')
10    continue

c   test for IDA:
      name = 'IDA'
      call pbid(name,j,*30)
      call pbname(j,name1,*30)
      call MVCL(' id = xxxxxxxx  name = xxxxxxxx',ibuf,26)
      call MVLC(ibuf, PBUF1, 26)
      write(pbuf1(7:14),'(i8)') j
      pbuf1(24:35) = name1
      call xvmessage( pbuf1, ' ')
      go to 40
30    call xvmessage(' IDA not found', ' ')
c
c   Testing the C-Bridge  
c
40    call tzpbname
c
      return
      end
