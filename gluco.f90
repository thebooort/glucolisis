!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!   pp2 :    Basic computations for continuous dynamical systems
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP) 
!--------- ---- 

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM, IJAC, ICP(*)
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
  DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
  DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,*), DFDP(NDIM,*)
 

  F(1) = -U(1)+(PAR(1)*U(2)) + U(1)*U(1)*U(2)
  F(2) = PAR(2) - PAR(1)*U(2)-U(1)*U(1)*U(2) 

END SUBROUTINE FUNC
!---------------------------------------------------------------------- 

SUBROUTINE STPNT(NDIM,U,PAR,T) 
!--------- ----- 

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM), PAR(*)
  DOUBLE PRECISION, INTENT(IN) :: T

  
  PAR(2) = (0.6)
  PAR(1)=(0.02)
  U = 0.0

END SUBROUTINE STPNT
!---------------------------------------------------------------------- 

SUBROUTINE PVLS(NDIM,U,PAR)
!--------- ----

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: NDIM
  DOUBLE PRECISION, INTENT(IN) :: U(NDIM)
  DOUBLE PRECISION, INTENT(INOUT) :: PAR(*)
  DOUBLE PRECISION GETP

! Set PAR(9) equal to U1 
  PAR(9)=GETP('BV0',1,U)

END SUBROUTINE PVLS

SUBROUTINE BCND 
END SUBROUTINE BCND

SUBROUTINE ICND 
END SUBROUTINE ICND

SUBROUTINE FOPT 
END SUBROUTINE FOPT
!---------------------------------------------------------------------- 
