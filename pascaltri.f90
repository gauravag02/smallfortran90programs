program pascalsTriangle

    ! This program prints n number of lines in pascal's triangle. 'line' represents the line number of pascal's triangle, 
    ! 'spaces' is the number of spaces that are required, k is the iterable number/ number of element in each line in triangle,
    ! and c is the actual coefficient/number to be printed.
    ! This program prints in a pretty triangle, if the triangle is skewed adjust line 17 and 14.

    implicit none
    integer:: n
    integer(kind =16) :: line, spaces, k, c
    write(*,*) "Enter the number of rows of the Pascal Triangle: "
    read (*,*) n
    do line = 0,n-1
        c = 1
        do spaces = 0,n-line-1         !write total n-lines (-1 is with experiment for adjusting) spaces before printing number
            write(*,'(a)',advance="no") "   "
        end do
        do k = 0,line
            write(*,'(i5,a)',advance="no") c, " "   ! i5 works best for me, otherwise i have to change the number of spaces in above loop
            c = c * (line-k)/(k+1)                !c calculates the coefficient to write every step
        end do
        write(*,*) " "                      ! this is there to end the line finally and move to next line
    end do
end program pascalsTriangle