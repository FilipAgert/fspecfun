program main
    use fspec
    implicit none
    real(kind) :: x, g
    real(kind),parameter :: gamma_01 = 9.5135076986687318362924871772654021925505786260883773430500007704342_kind 
    real(kind),parameter :: gamma_05 = 1.77245385090551602729816748334114518279754945612238712821380778985291128459_kind
    integer :: numdigits
    g = gamma(0.1_kind)
    

    write(*,'(A13,E40.30)') "gamma(0.1) = ", g
    write(*,'(A13,E40.30)') "actual     = ", gamma_01
    numdigits = abs(log10(abs(g-gamma_01)))
    write(*,'(A,I15)') "Number of correct digits:", numdigits
    !g = gamma(0.5_kind)
    ! write(*,'(A13,F40.35)') "gamma(1/2) = ", g
    ! write(*,'(A13,F40.35)') "sqrt(pi)   = ", gamma_05
    ! numdigits = abs(log10(abs(g-gamma_05)))
    ! write(*,'(A,I15)') "Number of correct digits:", numdigits
end program