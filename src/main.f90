program main
    use fspec
    implicit none
    real(kind) :: x, g
    
    g = gammaf(1.0_kind)


    write(*,*) "gamma(1/2) = ", g

end program