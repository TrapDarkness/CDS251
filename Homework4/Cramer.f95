program CramersRule

   ! System of equations. 2x2, 3x3
   ! The main program is written for you. Read through the comments and
   ! see how the main program works.
   ! 2 Special Notes!!!!!
   ! 1: Take note of how the logical variable 'Success' will either write
   !    the solution or 'No Solution' to the output file.
   ! 2: Take note of how inside the do loop, allocating and deallocating
   !    memory for the arrays Matrix1, b, and x are done so the amount of
   !    memory allocated changes for each system. You cannot allocate more
   !    memory for an array until currently allocated memory is deallocated.
   
   implicit none
   
   ! Declare varialble
   integer :: n, row, col, i
   real, allocatable :: Matrix1(:,:), b(:), x(:)
   real :: detA, detM, determinant
   logical :: Success
   
   ! Open the input and output files.
   open(42,file='hw4Data2.txt')
   open(43,file='hw4Data2Out.txt')
   
   ! Solve each system in the input files.
   do
      ! Read in size of first system.
      read(42,*) n
      if (n .eq. 0) exit  ! Quit if zero.
      
      ! Allocate memory for system, right hand side, and solution vector.
      allocate(Matrix1(n,n), b(n), x(n))
      
      ! Read in the system. Ask if you do not understand how this works!
      do row = 1, n
         read(42,*) (Matrix1(row, col), col = 1, n), b(row)
      enddo
      
      ! Use cramers rule to get solution.
      call Cramer(Matrix1, b, n, x, Success)


      if (Success) then
         ! Write solution to file
         print*, "Success, writing sol"
         do row = 1, n
            write(43,*) x(row)
         enddo
         write(43,*)
      else ! This happens when there is no unique solution.
         write(43,*) 'No Solution'
         write(43,*)
      endif
      
      ! clean up memory and go back up to top for next system.
      deallocate(Matrix1, b, x)
      
   enddo
   
   ! close files
   close(42)
   close(43)

end program CramersRule

subroutine Cramer(M, b, n, x, Success)

   ! This subroutine does Cramer's Rule
    implicit none
   
   ! Declare and initialize your variables first.
    integer :: n, i
    real*4 :: M(n,n), MatOut(n,n), b(n), x(n), Determinant
    logical :: Success
    real*4, allocatable :: Mi(:,:)
   ! Find the determinant of M first. print it to screen.
   print*, "Determinant: ", Determinant(M,n)
   ! If it is zero, set the Success logical variable and quit.
    Success = .true.
    if (Determinant(M,n) == 0) then
        print*, "Det = 0, flagging false"
        Success = .false. 
        return
        
    end if  
   ! Allocate memory for a working matrix for column substituion. Then, for each
   ! column, i, substitute column i with vector b and get that determinant. 
   ! Compute the ith solution.
    allocate(Mi(n,n))

    do i = 1, n
        call ColumnInsert(M, b, n, i, MatOut)
        !print*, MatOut
        Mi = MatOut
       ! print*, Determinant(working_matrix, n)
       x(i) = Determinant(Mi,n) / Determinant(M,n)
    end do

   ! deallocate memory for the working matrix.
   deallocate(Mi)

end subroutine Cramer

subroutine ColumnInsert(M, b, n, col, MatOut)

    implicit none
    
    integer :: n, col
    real*4 :: M(n,n), MatOut(n,n), b(n)
        
   ! This subroutine takes vector b and inserts in into matrix M at column col.
   ! Don't forget to set MatOut = M before you substitute the column in.
    MatOut = M
    MatOut(1:n,col) = b
    print*, "Matrix testing", M, "b ", b, "col ", col,"Changed matrix", MatOut

end subroutine ColumnInsert

function Determinant(M, n) result(Det)

   implicit none
   ! Write this function so that it can compute the determinant of a 2x2 or
   ! 3x3 matric depending on the value of n.
   integer :: n
   real :: Det
   real :: M(n,n)


   if (n == 2) then
      Det = M(1,1)*M(2,2) - M(1,2)*M(2,1)
   else if (n == 3) then
      ! Det = M(1,1)*M(2,2)*M(3,3) + M(1,2)*M(2,3)*M(3,1) + M(1,3)*M(2,1)*M(3,2)
      ! Det = Det - M(1,3)*M(2,2)*M(3,1) - M(1,2)*M(2,1)*M(3,3) - M(1,1)*M(2,3)*M(3,1)
      Det = M(1,1)*(M(2,2)*M(3,3) - M(2,3)*M(3,2)) - M(1,2)*(M(2,1)*M(3,3) - M(2,3)*M(3,1)) + M(1,3)*(M(2,1)*M(3,2) - M(2,2)*M(3,1))
   end if 
      

end function Determinant
