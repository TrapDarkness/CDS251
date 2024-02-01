program Homework3
    ! Alexander Shumway
    ! CDS251
    ! Homework 3
    ! 02/08/2024
    
    !This program reads in data from a file to both set an array size and to calculate the average of the data within

    implicit none
    !declare variables here
    integer size,i
    real*4 sum, average
    real*4, allocatable :: lab2_data(:)
 
    !open file, and read first integer in that file
    open(42,file='CDS251Lab2Data.txt')
    read(42,*) size
    !Allocate an array of that size
    allocate(lab2_data(size))
    !In one do loop, read in one number into the array and also sum it up.
    sum = 0
    do i = 1, size 
        read(42,*) lab2_data(i) 
        sum = sum + lab2_data(i)
    end do 
    !After the do loop compute the traditional average by dividing by the number of numbers
    average = sum / size
    !Print the average to the screen (with a label).
    print*, "Average of Data: ", average
    
    !The answer should be: -8.2000417

    deallocate(lab2_data)
    close(42)
end program Homework3