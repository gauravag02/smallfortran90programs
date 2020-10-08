program goldenratio
    implicit none
    real :: temp,bp,ar,br,power,recursion
    integer :: i
    print*, "Iteration(n) | ----------- RECURSION VALUES ---------------------|----------------- POWER VALUES -----------|--&
    ------------------------ Ratio -----------|"
    bp=(-sqrt(5.0)+3)/2
    br=(-sqrt(5.0)+3)/2 ; ar =(sqrt(5.0)-1)/2
    do i=2,25
        write(*,'(I3,f50.30)', advance = 'no') i,recursion(ar,br)
        temp = br ; br = recursion(ar,br); ar = temp
        write(*,'(f50.30,f50.30)') power(bp), abs(br/power(bp))
        bp = power(bp)
    end do
end program goldenratio

function power(a)
    real ::a
    power = a*(sqrt(5.0)-1)/2
end function

function recursion(a,b)
    real :: a,b,recursion
    recursion = (a-b)
end function