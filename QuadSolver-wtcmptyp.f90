program quad
    implicit none
    real(kind=8) :: a,b,c,d,e
    read *, a,b,c

    if (b**2-4*a*c .lt. 0) then
        10 format("The complex roots are: ", f5.2 , X , "+ i(" , f5.2 , ") and " , f5.2 , X , "- i(" &
        , f5.2 , ")")
        print 10, -b/2*a, sqrt(-(b**2-4*a*c))/2*a, -b/2*a, sqrt(-(b**2-4*a*c))/2*a
    else
        20 format(" The roots are: ", f5.2, ' and ', f5.2)
        print 20, (-b+sqrt(b**2-4*a*c))/2*a,(-b-sqrt(b**2-4*a*c))/2*a
    endif

end program quad
