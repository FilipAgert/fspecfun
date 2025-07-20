module fspec
    implicit none

    integer, parameter :: kind = 8 !!double precision
    integer, parameter :: gamma_exp_order = 15 !!expansion order in the gamma series
    private
    public :: kind, gammaf, fac_hint

    integer, parameter :: gammag = 2 !!g in the Lanczos approximation

    contains


    real(kind) function gammaf(x)!!Gamma function for real numbers.
        real(kind), intent(in) :: x !!x>0
        real(kind), parameter :: pi = ACOS(-1.0_kind)
        if(x == nint(x)) then !!if integer argument, just call factorial
            gammaf = fac(nint(x)-1)
            return
        else if(x + 0.5 == nint(x+0.5)) then !!if half integer argument, call half int factorial
            gammaf = fac_hint(nint(x-0.5)-1)
            return
        endif
        if(x+gammag+0.5 .le. 0) error stop "x + gamma_g +0.5 must be grt 0"
        !!https://en.wikipedia.org/wiki/Lanczos_approximation using g = 1
        gammaf = sqrt(2*pi) * (x-0.5_kind+gammag)**(x-0.5_kind) * exp(-(x+gammag-0.5_kind)) * gamma_Ag(x-1)
    end function

    real(kind) function gamma_Ag(x)
        real(kind), intent(in) ::x
        integer :: k
        real(kind) :: s, fac
        s = 0.5_kind * gamma_pcoeff(0)
        fac = 1
        do k =1, gamma_exp_order
            fac = fac * (x+1-k)/(x+k) !Goes as z/z+1 , z(z-1) / (z+1)(z+2) ... as k = 1,2,...
            s = s + fac*gamma_pcoeff(k)
        end do
        gamma_Ag = s

    end function
    !!Gamma p coefficient for g = -0.5
    real(kind) function gamma_pcoeff(k)
        integer, intent(in) ::k
        logical,save :: first_time = .true.
        integer ::ii
        real(kind),save :: pc(0:gamma_exp_order)
        if(k < 0 .or. k>gamma_exp_order) error stop "invalid coefficient. 0 to gamma_exp_order allowed"
        if(first_time) then
            do ii = 0, gamma_exp_order
                pc(ii) = gamma_pcoeffg(real(gammag,kind),ii)
            end do
            first_time = .false.
        endif
        gamma_pcoeff = pc(k)
    end function

    !!Gamma p coefficient for arbitrary g
    real(kind) function gamma_pcoeffg(g,k)
        real(kind), intent(in) :: g !!arbitrary number chosen s.t. Re(z + g + 1/2) > 0 in evaluating the gamma function
        integer, intent(in) :: k
        logical, save :: first_time = .true.
        real(kind),save :: Cmat(gamma_exp_order*2+1,gamma_exp_order*2+1)
        real(kind) :: s
        real(kind), parameter :: pi = ACOS(-1.0_kind)
        integer :: n, m, l

        if(first_time) then
            Cmat = 0    
            Cmat(1,1) = 1!!C(1,2 not initialized)
            Cmat(2,2) = 1
            do n = 2,size(Cmat,1)-1
                Cmat(n+1,1) = -Cmat(n-1,1)
                Cmat(n+1,n+1) = 2*Cmat(n,n)
            end do  
            do m = 1, size(Cmat,1)-1
                do n = m+1, size(Cmat,1)-1
                    Cmat(n+1,m+1) = 2*Cmat(n,m) -Cmat(n-1,m+1)
                end do
            end do
            first_time = .false.
        end if
       

        s = 0
        do l = 0, k
            s = s + Cmat(2*k+1,2*l+1)*fac_hint(l-1)/(l+g+0.5_kind)**(l+0.5_kind) *exp(l+g+0.5_kind)
        end do

        gamma_pcoeffg = s*sqrt(2.0_kind)/pi


    end function

    real(kind) function fac(n) !!factorial
        integer, intent(in) :: n
        logical, save :: first_time = .true.
        real(kind), save, allocatable :: f(:)
        integer :: sz, i
        if(kind == 8) then
            sz = 169
        elseif(kind==16) then
            sz = 1754
        endif
        if(n > sz) error stop "try to compute factorial for too large number. Increase precision"
        if(first_time) then

            allocate(f(0:sz))
            f(0) = 1
            do i =  1,sz
                f(i) = f(i-1)*i
            end do
            first_time = .false.
        endif

        fac = f(n)
    end function

    real(kind) function fac_hint(n)!!half integer factorial defined as n+1/2 !
        integer, intent(in) :: n
        logical, save :: first_time = .true.
        real(kind), save, allocatable :: g(:)
        integer :: sz, i
        real(kind), parameter :: pi = ACOS(-1.0_kind)
        if(kind == 8) then
            sz = 84
        elseif(kind==16) then
            sz = 876
        endif
        if(abs(n) > sz) error stop "try to compute factorial for too large number. Increase precision"
        if(first_time) then
            allocate(g(-sz:sz))
            do i = 0,sz !! gamma(n+1/2)
                g(i) = sqrt(pi)*fac(2*i)/(4**i * fac(i))
            end do
            do i = 1, sz !! gamma(1/2-n)
                g(-i) = sqrt(pi)*(-4)**i * fac(i)/fac(2*i) 
            end do
            first_time = .false.
        endif

        fac_hint = g(n+1)
    end function

end module fspec