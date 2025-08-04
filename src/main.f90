program main
    use fspec
    implicit none
    real(kind), parameter :: x(5) = [0.1_kind, 0.5_kind, 1.5_kind, 3.7_kind, 7.3_kind]
    real(kind), parameter :: gamma_ref(5) = [ &
       9.513507698668731836292487177265402192551_kind, &    ! gamma(0.1)
       1.772453850905516027298167483341145182798_kind, &    ! gamma(0.5)
       0.886226925452758013649083741670572591398_kind, &    ! gamma(1.5)
       4.1706517837966031653936029986179837279404455809898292945722466324606_kind, &    ! gamma(3.7)
       1271.42363366390927305799362667845833785419537823001188710676314893779421_kind ]   ! gamma(7.3)
    integer :: numdigits, i
    real(kind) :: g

    do i = 1, 5
        g = gamma(x(i))
        write(*,'(A,F6.2,A,E40.30)') "gamma(", x(i), ") = ", g
        write(*,'(A,E40.30)') "expected        = ", gamma_ref(i)
        numdigits = int(abs(log10(abs(g - gamma_ref(i)))))
        write(*,'(A,I0)') "Number of correct digits: ", numdigits
        print *, ""
     end do
end program