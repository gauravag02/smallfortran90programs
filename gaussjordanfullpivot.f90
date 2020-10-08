program gaussseli
    !This program simply takes in an nxn matrix, and an nx1 matrix, converts into a reduced row echelon form using gauss jordan FULL pivoting,
    !,then calculates the determinant of nxn, if determinant is non-singular the program proceeds to calculate inverse
    !and solution of (nXn) x = (nX1) form equation (i.e. Ax=b)
    !Various parts of the code are highlighted by comments as to what they do.
    implicit none
    integer :: n,i,k,j
    integer, allocatable :: indices(:)
    real , allocatable :: A(:,:), temp(:)
    real :: q, ep=10**(-7)
    print*, "Give the dimension of matrix A in form AX=b"
    read*, n
    allocate(A(n,n+1+n), temp(n+n+1), indices(n))   !Allocate nxn for original matrix, nx1 for b, nxn for I all concatenated
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
    do i=1,n
        indices(i) = i    ! to keep track of solution number for later during full pivoting column shifts
    enddo
    print*, "================================================"
    !!!!!!!!!!!!!!!! CONVERTING THE MATRIX TO UPPER TRIANGULAR REDUCED ECHELON !!!!!!!!!!!!!!!!!!!
    do k = 1,n
        call fullpivot(A(k:,k:),A,indices,k)
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
    do i=1,n
    print*, "The", indices(i), ' solution is', temp(i)
    enddo
    print*, "========================================================="

    contains
    subroutine fullpivot(submatrix,matrix,solindices,row_ind)
        real :: submatrix(:,:),matrix(:,:)
        real, allocatable :: tempmat(:)
        integer :: q(2),pos(2),row_ind,dummy, solindices(:)

        q=shape(submatrix)
        print*, "=============================submatrix==================================="
        do i=1,q(1)
            print*,submatrix(i,:)
        enddo
        pos = maxloc(abs(submatrix(:,:q(1))))   !position of the biggest element in the square submatrix
        
        if (pos(2) .ne. 1) then      ! check if highest element belongs to first column, if no then swap columns first
            allocate(tempmat(q(1)))
            tempmat = A(:,pos(2)+row_ind-1) ; A(:,pos(2)+row_ind-1) = A(:,row_ind) ; A(:,row_ind) = tempmat
            dummy = solindices(pos(2)+row_ind-1) ; solindices(pos(2)+row_ind-1) = solindices(row_ind)
            solindices(row_ind) = dummy
            deallocate(tempmat)     !the above lines do column swap and swap the indices as well to keep track of variables/indices
        endif
        if (pos(1) .ne. 1) then         !check if highest element now belongs to the first row, if not then swap rows
            allocate(tempmat(q(2)))
            tempmat = submatrix(pos(1),:) ; submatrix(pos(1),:) = submatrix(1,:) ; submatrix(1,:) = tempmat
            deallocate(tempmat)
        endif    
    end subroutine fullpivot

end program gaussseli

