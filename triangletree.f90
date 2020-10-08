program triangle
    implicit none
    real,dimension(2) :: p1,p2,p3 !points to read
    real :: d1,d2,d3, dist !distances from each
    print *, "Give points [2D], give x and y one after the other for each point"
    read *, p1,p2,p3
    d1 = dist(p1,p2)
    d2 = dist(p2,p3)
    d3 = dist(p3,p1)

    if ((d1.eq.d2).and.(d1.eq.d3)) then
        print *, "You've got yourself an equilateral triangle."
        stop
    elseif ((d1.ne.d2).and.(d2.ne.d3).and.(d3.ne.d1)) then
        print *, "Scalene it is."
    else 
        print *, "Isoceles."
    endif
    print *, d1,d2,d3
    if ( (max(d1,d2,d3)**2) /100 .eq. (d1**2+d2**2+d3**2-max(d1,d2,d3)**2) /100) then
        !check for right triangle, divide by 100 to reduce precision as some precision gets lost in sqrt.
        print *, "|\"
        print *, "| \"
        print *, "|  \"
        print *, "|_  \"
        print *, "|_|__\"
    endif
    

end program triangle

function dist(pt1,pt2)
    implicit none
    real::dist
    real, dimension(2) :: pt1,pt2
    dist = sqrt((pt1(1)-pt2(1))**2+(pt1(2)-pt2(2))**2)
end function dist