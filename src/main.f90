program main
    use fspec
    implicit none
    real(kind) :: x, g
    
    g = gamma(0.1_kind)
    

    write(*,*) "gamma(0.1) = ", g

end program