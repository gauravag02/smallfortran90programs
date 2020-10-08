program gaussseli
    ! This program simply takes in an nxn matrix, and an nx1 matrix, converts into a reduced row echelon form using gauss jordan pivoting,
    ! ,then calculates the determinant of nxn, if determinant is non-singular the program proceeds to calculate inverse
    ! and solution of (nXn) x = (nX1) form equation (i.e. Ax=b)
    !Various parts of the code are highlighted by comments as to what they do.
    implicit none
    integer :: n,i,k,j
    real , allocatable :: A(:,:), temp(:)
    real :: q, ep=10**(-7)
    print*, "Give the dimension of matrix A in form AX=b"
    read*, n
    allocate(A(n,n+1+n), temp(n+n+1))   !Allocate nxn for original matrix, nx1 for b, nxn for I all concatenated
    !!!!!!!!!!!!!!!!!!!!!!!!!!!! READING MATRIX FROM USER !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print*, "Give the rows of matrix A"
    do i=1,n
        read*, A(i,1:n)
    enddo
    print*,"Give values of b"
    do i=1,n
        read*, A(i,n+1)
    enddo
    !!!!!!!!!!!!! MATRIX FORM IS A(augmented) = A(read)|b(read)|I CONCATENATED AND PROCESSED TOGETHER THROUGHOUT !!!!!!!!!!!!!!
    do i=1,n
        A(i,n+i+1) =1   !getting the identity of the third part in matrix
    enddo
    print*, "MATRIX = A | b | Identity"
    do i=1,n
        print*,A(i,:)
    enddo
    print*, "================================================"
    !!!!!!!!!!!!!!!! CONVERTING THE MATRIX TO UPPER TRIANGULAR REDUCED ECHELON !!!!!!!!!!!!!!!!!!!
    do k = 1,n
        do i=k,n
            if (abs(A(i,k)) .gt. abs(A(k,k))) then
                temp = A(k,:) ; A(k,:) = A (i,:); A(i,:) = temp    !swapping higher terms to higher position in matrix
            endif
        enddo
        do j=k+1,n
            q=(A(j,k))/A(k,k)
            do i=k,n+n+1
                A(j,i) = A(j,i) - q* A(k,i)       !finally making the lower triangular terms zero
            enddo
        enddo
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! CONVERSION DONE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print*, "The modified/augmented matrix:"
    do i=1,n
        print*,A(i,:)
    enddo
    print*, "====================================================="
    !!!!!!!!!! CHECK DETERMINANT AFTER REDUCING IT TO UPPER TRIANGULAR !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print*, "Checking determinant"
    q = 1    ! determinant
    do i=1,n
        q = q * A(i,i)      !determinant is simply the product of diagonal elements in upper diagonal
    enddo
    print*, "The determinant is:", q
    If (abs(q) .le. ep) then 
        print*, "Determinant is SINGULAR, unsolvable"
        STOP
    endif
    print*, "====================================================="
    !!!!!!!!!!!!!!!!!!!!!!!!!!!CALCULATE FINAL RESULT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    temp =0
    temp(n) = (A(n,n+1))/A(n,n)
    do i = n-1,1,-1
        q=0
        do j=i+1,n
            q = q + A(i,j)*temp(j)
        enddo
        temp(i)= (A(i,n+1) -q)/A(i,i)
    enddo
    print*, "The solutions are", temp(1:n)
    print*, "========================================================="
    !!!!!!!!!!!!!!!!!!!!!!!!!!!! COMPUTE INVERSE NOW !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print*, "The inverse of A is"
    do i = n,1,-1
        A(i,:) = A(i,:)/A(i,i)
    enddo
    do i = n,1,-1
        do j=1,i-1
            A(j,:) = A(j,:) - A(i,:)*A(j,i)
        enddo
    enddo
    do i=1,n
        print*,A(i,n+2:)
    enddo
    print*, "=========================================================" 
end program gaussseli

