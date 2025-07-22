module fspec
    implicit none

    integer, parameter :: kind = 16 !!double precision
    integer, parameter :: gamma_exp_order = 15 !!expansion order in the gamma series
    private
    public :: kind, gamma

    integer, parameter :: gammag = 5 !!g in the Lanczos approximation

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

    real(kind) function gamma_Ag(x)
        real(kind), intent(in) :: x
        logical :: first_time = .true.
        integer(16) ::   B(0:gamma_exp_order,0:gamma_exp_order)
        real(16) :: Z(0:gamma_exp_order),C(0:gamma_exp_order,0:gamma_exp_order), fg(0:gamma_exp_order),D(0:gamma_exp_order,0:gamma_exp_order)
        real(16), save::cvec(0:gamma_exp_order)
        integer :: row, col, n, m, sign
        integer(16) :: bc


        if(first_time)then
            D(0,0) = 1
            D(1,1) = -1
            D(2,2) = -6
            do row = 2,gamma_exp_order-1
                D(row+1,row+1) = -fac(2*row+2)/(2_16*fac(row) * fac(row+1))
     
            end do


            B = 0
            B(0,:) = 1
            do row = 1, gamma_exp_order
                do col = row,gamma_exp_order
                    m = row*2-1
                    n = row+col-1

                    bc = bin(n,m)
                    if(mod(col-row,2) == 0)then
                         sign = 1
                    else
                         sign = -1
                    endif
                    B(row,col) = bc * sign
                end do

            end do
            C = gamma_CMAT()
            fg = gamma_fl(real(gammag,kind))
            cvec = MATMUL(MATMUL(MATMUL(D,B),C), fg)
            first_time = .false.
        endif

        Z(0) = 1
        do row = 1, gamma_exp_order
            Z(row) = 1.0_kind/(row+x)
        end do
        gamma_AG = dot_product(Z,cvec)
    end function
    !!Gamma p coefficient for arbitrary g
    function gamma_fl(g)
        real(kind), intent(in) :: g !!arbitrary number chosen s.t. Re(z + g + 1/2) > 0 in evaluating the gamma function
        logical, save :: first_time = .true.
        real(kind),save :: Cmat(gamma_exp_order*2+1,gamma_exp_order*2+1), CmatCompressed(0:gamma_exp_order, 0:gamma_exp_order), pk(0:gamma_exp_order)
        real(kind) :: s
        real(kind), parameter :: pi = ACOS(-1.0_kind)
        integer :: n, m, l
        real(kind), save :: Fg(0:gamma_exp_order)
        real(kind) :: gamma_fl(0:gamma_exp_order)

        if(first_time) then            
            do l = 0,gamma_exp_order
                Fg(l) = sqrt(2.0_kind/pi) * ffac(2*l-1) * exp(l+g+0.5_kind) / (2_kind** l *(l+g+0.5_kind)**(l+0.5_kind))!https://www.numericana.com/answer/info/godfrey.htm
            end do
            first_time = .false.
        end if
        gamma_fl=Fg
    end function

    function gamma_CMAT()
        integer(16) :: CMAT(gamma_exp_order*2+1,gamma_exp_order*2+1)
        real(16) :: gamma_CMAT(0:gamma_exp_order,0:gamma_exp_order)
        integer :: m,n
        Cmat = 0    
        Cmat(1,1) = 1!!C(1,2 not initialized)
        Cmat(2,2) = 1
        do n = 2,size(Cmat,1)-1
            Cmat(n+1,1) = -Cmat(n-1,1)
            Cmat(n+1,n+1) = 2_kind*Cmat(n,n)
        end do  
        do m = 1, size(Cmat,1)-1
            do n = m+1, size(Cmat,1)-1
                Cmat(n+1,m+1) = 2_kind*Cmat(n,m) -Cmat(n-1,m+1)
            end do
        end do

        do n = 0,gamma_exp_order
            do m = 0,gamma_exp_order
                gamma_CMAT(n,m) = Cmat(2*n+1,2*m+1)
            end do
        end do
        gamma_CMAT(0,0) = 0.5_16
    end function

    integer(16) function bin(n,m)
        integer, intent(in) :: n,m
        integer :: u,b
        bin = 1
        do u = n,n-m+1,-1
            bin = bin *u
        end do
        do b = m,1,-1
            bin = bin/b
        end do
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

    real(kind) function ffac(n) !! double factorial
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

            allocate(f(-1:sz))
            f(-1) = 1
            f(0) = 1
            do i =  1,sz
                f(i) = f(i-2)*i
            end do
            first_time = .false.
        endif

        ffac = f(n)
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
                g(i) = sqrt(pi)*fac(2*i)/(4_kind**i * fac(i))
            end do
            do i = 1, sz !! gamma(1/2-n)
                g(-i) = sqrt(pi)*(-4_kind)**i * fac(i)/fac(2*i) 
            end do
            first_time = .false.
        endif

        fac_hint = g(n+1)
    end function

end module fspec