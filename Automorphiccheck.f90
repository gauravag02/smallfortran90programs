program Automorphic_check
    implicit none
    Integer(kind=16) :: a,sq
    print*, "Give an integer to check whether it is automorphic or not"
    read *, a
    sq = a*a
    print *, "Square is:", sq
    do while (a>0)
        if (mod(a,10) .ne. mod(sq,10)) then
            print*, "Non-automorphic"
            stop
        end if
        a= a/10
        sq = sq/10
    end do
    print *, "Automorphic"
end program Automorphic_check