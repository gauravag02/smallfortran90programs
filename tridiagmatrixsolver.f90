program tridiagnalmat
    !This program solves a tri diagonal matrix, A only has the diagonal matrix elements
    implicit none
    real, allocatable :: A(:,:), b(:), x(:)
    integer :: n,i

    print*, 'Give dimension of matrix' ; read*, n
    allocate(A(n,3), b(n),x(n))
    print*, 'Give matrix row-wise, only the tridiagonal terms with the first row being " 0 x x " and last being "x x 0"'
    do i=1,n
        read*, A(i,:)
    enddo
    if ((A(n,3) .ne. 0) .or. (A(1,1) .ne. 0) ) print*, "ERROR : Matrix non-conforming to the input format"
    print*, 'Give b'; read*, b
    print*, '--------------------Read complete, tridiagonal A: ---------------------'
    do i=1,n
        print*, A(i,:), b(i)
    enddo
    do i =2,n
        b(i) = b(i) - A(i,1)*b(i-1)/A(i-1,2)
        A(i,1:2) = A (i,1:2) - A(i,1)*A(i-1,2:3)/A(i-1,2)
    enddo
    do i=n-1,1,-1
 !       A(i,3) = A(i,3) - A(i+1,2) * (A(i,3)/A(i+1,2))  !unnecessary step, will not be used for solution, only for completeness
        b(i) = b(i) - b(i+1)*(A(i,3)/A(i+1,2))
    enddo
    do i=1,n
        x(i) = b(i)/A(i,2)
    enddo
    print*, "------------------------------------------------"; print*, "The solutions are:"
    print*, x
end program tridiagnalmat