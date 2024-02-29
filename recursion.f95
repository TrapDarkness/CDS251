program recursion
    implicit none

    integer :: n, factorial, fact

    print*, "Enter n! you want below as n"
    read(*,*) n
    fact = factorial(n)
    print*, fact

end program recursion


recursive function factorial(n) result(fact)

   implicit none

   integer ::  n, fact 

   if (n == 0) then
        fact = 1
    else
        fact = n * factorial(n-1)
    endif 
      
end function factorial