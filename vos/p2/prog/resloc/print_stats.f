ccccccccccccccccccccccccccccccccccccccc
c Print the statistics for each reseau mark in reseau grid format....

      subroutine print_stats(cdn,dc,cdedge,ceps,crho,cg,ch,cq)
      implicit none
c Inputs...
      real*4 cdn(202),dc(202),cdedge(202),ceps(202)
      real*4 crho(202),cg(202),ch(202),cq(202)

      call xvmessage('.page',0)
      call xvmessage('dn...',0)
      call p_stats(cdn,1.)

      call xvmessage('.page',0)
      call xvmessage('dc...',0)
      call p_stats(dc,1.)

      call xvmessage('.page',0)
      call xvmessage('dedge...',0)
      call p_stats(cdedge,1.)

      call xvmessage('.page',0)
      call xvmessage('10*eps...',0)
      call p_stats(ceps,10.)

      call xvmessage('.page',0)
      call xvmessage('100*rho...',0)
      call p_stats(crho,100.)

      call xvmessage('.page',0)
      call xvmessage('100*g...',0)
      call p_stats(cg,100.)

      call xvmessage('.page',0)
      call xvmessage('100*h...',0)
      call p_stats(ch,100.)

      call xvmessage('.page',0)
      call xvmessage('100*q...',0)
      call p_stats(cq,100.)
      return
      end
