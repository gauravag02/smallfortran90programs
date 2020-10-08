program list_primes

    ! this program takes an integer and then constructs and array of primes upto that number, and only checks
    ! the number against primes it has already calculated and stored in an array. It also skips all even numbers.

    implicit none
    integer(kind=16) :: a,i,p,k    !a is number to list primes till, i p k serve as counters for loop, array element, array loop
    integer, dimension(:), allocatable :: arr
    print *, "Give integer(>2) to list primes upto"
    read *, a
    if (a < 2) stop
    allocate (arr(a))    !could replace 'a' in this by estimates of prime counting to reduce memory usage
    arr(1) = 1
    arr(2)=2
    p=2
    do i=3,a,2
        do k=2,p
            if (mod(i,arr(k)) .eq. 0) exit
            if (k==p) arr(p+1) = i     !if k has cycled through all and still not found 0 then add to array as prime
            if (k==p) p = p+1          ! update element counter for the addition to the array above
        end do
    end do
    print *, 'Total Primes:',p
    print *, ' ---------------------------------------------------------------------------------------------------'
    print *,  arr(1:p)
end program list_primes