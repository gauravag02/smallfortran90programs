program arraysort
    ! README
    ! This program sorts a nxn array in ascending order by row or by column.
    ! There is code repetition that can be removed b/w row sorting and column sorting to reduce size of code,
    ! however, it doesnt affect the speed of the program as these lie in condiitonals.
    
    ! code populates randomly a nxn array called arr, then construct an empty array of nxn called temp and n called temp2. temp is the new array and 
    ! temp2 stores the index of max values in arr as it gets updated (on every loop, these max values are removed from arr)
    
        implicit none
        integer :: n,i,j
        character :: a
        real, dimension(:,:), allocatable :: arr1,temp
        integer, dimension(:), allocatable :: temp2
        print*, "Give n for nxn matrix, and choose whether to sort by row(r) or column(c)"
        read ('(I4,A1)'), n,a
        allocate (arr1(n,n), temp(n,n), temp2(n))
        call random_number(arr1)
        print*, "---------------------RANDOM ARRAY IS: ----------------------------------"
        do i=1,n
            print*, arr1(i,:)
        end do
        if (a == 'c' .or. a =='c') then
            print*, " ----------------------sorting by column------------------------------"
            do i=1,n
                temp2 = maxloc(arr1,1)
                temp(i,:) = maxval(arr1,dim=1)
                do j=1,n
                    arr1(temp2(j),j) =0    !this is fine since our values are between 0 and 1, otherwise it could be set to minimum value in whole arr
                end do
            end do
        else if (a == 'r' .or. a == 'R') then
            print*, "--------------------------sorting by row ---------------------------------"
            do i=1,n
                temp2 = maxloc(arr1,2)
                temp(:,i) = maxval(arr1,dim=2)
                do j=1,n
                    arr1(j,temp2(j)) =0
                end do
            end do
        else
            print*, "check output"; stop
        end if
        do i=1,n
            print*, temp(i,:)
        end do
    
    end program arraysort
