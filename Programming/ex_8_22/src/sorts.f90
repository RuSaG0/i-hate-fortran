module Sorts
   use Environment

   implicit none

contains
   subroutine sortMatrix(A)
      real(R_), intent(inout)   :: A(:, :)
      integer                   :: j
      
      do concurrent (j = 1: Ubound(A, 1))
        call sortArray(A(:, j))
      end do
   end subroutine sortMatrix
   
    pure recursive subroutine sortArray(a)
        real, intent(inout)                :: a(:)
        real                               :: x
        integer                            :: first, last
        integer                            :: i, j

        first = 1
        last = size(a, 1)
        x = a( (first+last) / 2 )
        i = first
        j = last
        
        do
           do while (a(i) < x)
              i = i+1
           end do
           
           do while (x < a(j))
              j = j-1
           end do
           
           if (i >= j) exit
           
           a([i, j]) = a([j, i])
           i = i+1
           j = j-1
        end do
        
        if (first < i-1) call sortArray(a(first : i-1))
        if (j+1 < last)  call sortArray(a(j+1 : last))
    end subroutine sortArray
end module Sorts 
