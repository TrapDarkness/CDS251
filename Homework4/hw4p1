program Homework4
    ! Alexander Shumway
    ! CDS251
    ! Homework 4.1
    ! 02/15/2024
    
    ! This program reads in data from a file to set an encryption_matrix and then decrypt the data within 

    implicit none
    !declare variables here
    integer i, n !iterator and matrix size
    integer, allocatable :: hw4_data(:) !variable sized array for holding data from the file
    integer, allocatable :: encryption_matrix(:,:), inverted_matrix(:,:) !matrix variables
    integer :: Determinant !Function return type
    character(len=32) :: string !string to hold decoded message
   

    n = 2 !set matrix size
    allocate(encryption_matrix(n,n))
    allocate(inverted_matrix(n,n))

    !open file, and read in encryption_matrix from file
    open(42,file='hw4Data3.txt')
    read(42,*) encryption_matrix(1,1:2), encryption_matrix(2,1:2)

    !invert encryption_matrix
    inverted_matrix(1,1) = encryption_matrix(2,2)
    inverted_matrix(2,2) = encryption_matrix(1,1)
    inverted_matrix(1,2) = -1*encryption_matrix(1,2)
    inverted_matrix(2,1) = -1*encryption_matrix(2,1)
    inverted_matrix = inverted_matrix / Determinant(encryption_matrix, n)

    !Allocate an array of length 2
    allocate(hw4_data(2))
    
    !Loop for Decryption
    do i = 1, 32, 2 
        read(42,*) hw4_data(1:2) !read in data value
        hw4_data = matmul(inverted_matrix, hw4_data) !decrypt characters
        string(i:i+1) = char(hw4_data(1))//char(hw4_data(2)) !add chars to string
    end do

    print*, string !Print decoded message to string

    !clean-up
    deallocate(hw4_data, inverted_matrix,encryption_matrix)
    close(42)

end program Homework4

function Determinant(M, n) result(Det)

   implicit none
   ! Write this function so that it can compute the determinant of a 2x2 or
   ! 3x3 matrix depending on the value of n.
   integer :: n
   integer :: Det
   integer :: M(n,n)


   if (n == 2) then
      Det = M(1,1)*M(2,2) - M(1,2)*M(2,1)
   else if (n == 3) then
      ! Det = M(1,1)*M(2,2)*M(3,3) + M(1,2)*M(2,3)*M(3,1) + M(1,3)*M(2,1)*M(3,2)
      ! Det = Det - M(1,3)*M(2,2)*M(3,1) - M(1,2)*M(2,1)*M(3,3) - M(1,1)*M(2,3)*M(3,1)
      Det = M(1,1)*(M(2,2)*M(3,3) - M(2,3)*M(3,2)) - M(1,2)*(M(2,1)*M(3,3) - M(2,3)*M(3,1)) + M(1,3)*(M(2,1)*M(3,2) - M(2,2)*M(3,1))
   end if 

      
end function Determinant
