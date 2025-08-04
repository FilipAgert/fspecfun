module fspec
    implicit none

    integer, parameter :: kind = 16 !!double precision
    integer, parameter :: gamma_exp_order = 15 !!expansion order in the gamma series
    private
    public :: kind, gamma

    integer, parameter :: gammag = 5 !!g in the Lanczos approximation

    interface
        module real(kind) function gamma_Ag(x)
            real(kind), intent(in) :: x
        end function

        real(kind) module function fac(n) !!factorial
            integer, intent(in) :: n
        end function

        real(kind) module function ffac(n) !! double factorial
            integer, intent(in) :: n
        end function

        real(kind) module function fac_hint(n)!!half integer factorial defined as n+1/2 !
            integer, intent(in) :: n
        end function

    end interface
    contains


    real(kind) function gamma(x)!!Gamma function for real numbers.
        real(kind), intent(in) :: x !!x>0
        real(kind), parameter :: pi = ACOS(-1.0_kind)
        if(x == nint(x)) then !!if integer argument, just call factorial
            gamma = fac(nint(x)-1)
            return
        else if(x + 0.5 == nint(x+0.5)) then !!if half integer argument, call half int factorial
            gamma = fac_hint(nint(x-0.5)-1)
            return
        endif
        if(x+gammag+0.5 .le. 0) error stop "x + gamma_g +0.5 must be grt 0"
        !!https://en.wikipedia.org/wiki/Lanczos_approximation
        gamma = sqrt(2*pi) * (x-0.5_kind+gammag)**(x-0.5_kind) * exp(-(x+gammag-0.5_kind)) * gamma_Ag(x-1)
    end function

    


end module fspec