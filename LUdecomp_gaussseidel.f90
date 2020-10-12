program LUdecomp_gaussseidel
    ! This program does LU decomposition of a general nxn matrix A in the system Ax=b, then solves for x using back substitution
    ! After obtaining x and A it improves the existing solution using simple gauss seidel iteration
    ! LU matrix is stored in a single matrix with L's diagonal terms alll being 1 and hence not stored (do-little's way).
    ! The variable names are pretty straightforward, with Ax=b => LUx=b where Ux = y, thus Ly = b
    implicit none
    real (kind=8), allocatable :: A(:,:), LU(:,:),b(:),x(:),y(:)
    integer :: n , i , j , k


    !READ MATRIX BELOW
    print*, "Give dimension of matrix"
    Read*, n
    allocate(A(n,n), b(n), x(n),y(n))
    print*, 'Give A row wise'
    do i=1,n
        read*, A(i,:)
    enddo
    print*, 'Give b' ; read *, b
    print*, "Reading complete" ; print*, '-------------------------LU decomposition:---------------------------------'



    ! READING COMPLETE NOW DO LU DECOMPOSITION AND SUBSEQUENTLY BACKSUBSTITTUTE
    call LUdecomposition(A,LU)
    call backsubstituteLU(LU,x,y,b)
    print*, '--------------------------------------------------------------------------------------'

    !Do gauss seidel now and print final soltuion
    print*, "Give number of iterations you want for gauss seidel" ; read*, k
    do i = 1,k
        call gausseidel(x,b,A)
        print*, 'Iteration', i, "complete, new solution :", x
        print*, '--------------------------------------------------------------------------------------'
    enddo
    print*, "The final solutions are"
    print*, x    
    contains

    SUBROUTINE LUdecomposition(A,LU)
        !This subroutine does LU decomposition given a matrix A using Dolittle (i.e. diagonal terms of L = 1). 
        !The LU decomposition is stored in one matrix only with diagonal of L not stored at all (since they are all 1)

        real(kind=8) :: A(:,:),sum
        real(kind=8) , allocatable :: LU(:,:)
        integer :: n(2),i,j,k

        n = shape(A)
        if (n(1) .ne. n(2)) print*, "Bad matrix"
        allocate(LU(n(1),n(2)))
        LU = 0
        do i=1,n(1)                                     !calculate upper triangular terms
            do k=i,n(1)
                sum=0
                do j=1,i
                    sum = sum + LU(i,j)*LU(j,k)         !couldve used dot product intrinsic function to get rid of this do loop
                enddo
                LU(i,k) = A(i,k) - sum
            enddo
            
            do k=i,n(1)                                 ! now calculate lower triangular 
                if (i ==k) cycle            
                sum =0
                do j=1,i
                    sum = sum + LU(k,j)*LU(j,i)
                enddo
                LU(k,i) = (A(k,i) - sum)/LU(i,i)
            enddo
        enddo
        do i=1,n(1)
            print*, LU(i,:)
        enddo
    endsubroutine

    SUBROUTINE backsubstituteLU(LU,x,y,b)
        !This subroutine backsubstitutes to get x and y vectors in a Ax=LUx=b equation/matrix system, where Ux=y.
        !LU matrix is LU decomposition of A stored in one matrix with diagonal elements of L =1 (and thus not stored at all).
        ! valid for any nxn system. x,y are pre-allocated empty matrices to be filled by this subroutine
        real(kind=8) :: LU(:,:),x(:),y(:),b(:),sum
        integer :: i,j,k, n(2)
        n = shape(LU)
        !get y values from back substitution below
        y(1) = b(1)
        do i=2,n(1)
            sum =0
            do j=1,i
                sum = sum+ LU(i,j)*y(j)
            enddo
            y(i) = b(i) - sum
        enddo
        ! get x values from back substitution below
        x(n(1)) =  y((n(1)))/LU(n(1),n(1))
        do i=n(1)-1,1,-1
            sum = 0
            do j=i+1,n(1)
                sum = sum + LU(i,j)*x(j)
            enddo
            x(i) = (y(i) - sum)/LU(i,i)
        enddo
    endsubroutine

    SUBROUTINE gausseidel(x,b,A)
        ! This subroutine does one step of gauss-seidel error correction.
        ! the system is of form Ax=b , this subroutine updates x itself valid for any nxn system
        real(kind=8) :: x(:), b(:), A(:,:), sum
        real(kind=8), allocatable :: temp(:)
        integer :: n(2), i, j, k
        n = shape(A)
        allocate(temp(n(1)))

        do i=1,n(1)
            sum=0
            do j = 1,i-1
                sum = sum + A(i,j)*temp(j)
            enddo
            do j = i+1,n(1)
                sum = sum + A(i,j)*x(j)
            enddo
            temp(i) = (b(i)-sum)/A(i,i)
        enddo
        
        x = temp
        deallocate(temp)

    endsubroutine
end program LUdecomp_gaussseidel