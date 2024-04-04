program reduce

   ! Daniel Sponseller
   !
   ! This program allows the user to input a fraction with two integers, a
   ! numerator and a denominator. The program uses a greatest common divisor
   ! function to reduce the fraction and prints it to the screen.

   implicit none
   
   integer :: Numerator, Denominator, gcd, divisor
   
   ! Prompt and input the fraction.
   print*,'Enter the numerator'
   read*, Numerator
   print*,'Enter the denominator'
   read*, Denominator
   
   ! Find the greatest common divisor.
   divisor = gcd(Numerator, Denominator)

   ! Print out reduced fraction.
   print*
   print*,'Divisor = ', divisor
   print*
   print*,'Reduced fraction is:'
   print*,Numerator/divisor
   print*,'----------------'
   print*,Denominator/divisor
   
end program reduce

! Write your function here.

recursive function gcd(num, den) result(divisor)

   implicit none

   integer :: num, den, divisor 

   if (den == 0) then
      divisor = num
   else if (den /= 0) then
      divisor = gcd(den, mod(num, den))
   end if 

      
end function gcd