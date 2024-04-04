program CDS251Lab2
    ! Alexander Shumway
    ! CDS251
    ! Lab 2
    ! 02/01/2024
    
    !This program reads in data from a file to both set an array size and to calculate the average of the data within

    implicit none
    !declare variables here
    integer size,i !iterator and array_size
    real*4 sum, average, update_average !variables for keeping track of running sum and an eventual average
    real*4, allocatable :: lab2_data(:) !variable sized array for holding data from the file
 
    !open file, and read first integer in that file
    open(42,file='CDS251Lab2Data.txt')
    read(42,*) size

    !Allocate an array of that size
    allocate(lab2_data(size))

    !In one do loop, read in one number into the array and also sum it up.
    sum = 0.0
    update_average = 0.0
    do i = 1, size 
        read(42,*) lab2_data(i) 
        sum = sum + lab2_data(i)
        update_average = update_average + (lab2_data(i) - update_average) /float(i)
    end do

    !After the do loop compute the traditional average by dividing by the number of numbers
    average = sum / size

    !Print the average to the screen (with a label).
    print*, "Traditional Average of Data: ", average, " Update Average: ", update_average
    !The answer should be: -8.2000417

    deallocate(lab2_data)
    close(42)

end program CDS251Lab2