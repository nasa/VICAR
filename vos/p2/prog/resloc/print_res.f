ccccccccccccccccccccccccccccccccccccccc
c Print output res...

      subroutine print_res(res)
      implicit none
c Inputs...
      real*4 res(2,202)

      call xvmessage('.page',0)
      call xvmessage('Output reseau locations...',0)
      call pmjs(res,1)
      return
      end

ccccccccccccccccccccccccccccccccccccccc
c Print nominals...

      subroutine print_nom(nom)
      implicit none
c Inputs...
      real*4 nom(2,202)

      call xvmessage('.page',0)
      call xvmessage('Nominal locations...',0)
      call pmjs(nom,1)
      return
      end

