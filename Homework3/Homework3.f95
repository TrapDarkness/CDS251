program Homework3
    ! Alexander Shumway
    ! CDS251
    ! Homework 3
    ! 02/08/2024
    
    !This program reads in data from a file to both set an array size and to calculate the average of the data within

    implicit none
    !declare variables here
    integer size,i !iterator and array_size
    real*4 u_previous, u, q, standard_deviation !variables for keeping track of average, q values, and standard deviation
    real*4, allocatable :: hw3_data(:) !variable sized array for holding data from the file
 
    !Initialize math variables
    u_previous = 0.0
    q = 0.0

    !open file, and read first integer in that file
    open(42,file='hw3data.txt')
    read(42,*) size

    !Allocate an array of that size
    allocate(hw3_data(size))

    !Loop for Average and Q calculations
    do i = 1, size 
        read(42,*) hw3_data(i) !read in data value
        u = u_previous + ((hw3_data(i) - u_previous) / float(i)) !update average
        q = q + (hw3_data(i) - u_previous)*(hw3_data(i) - u) !update q
    end do

    !Sqrt at end of computation for efficiency
    standard_deviation = sqrt(q/float(size))

    !Print the average to the screen (with a label) + done statement.
    print*, "Update Average: ", u, "Standard Deviation: ", standard_deviation
    print*, "Done!"
    
    !clean-up
    deallocate(hw3_data)
    close(42)

end program Homework3