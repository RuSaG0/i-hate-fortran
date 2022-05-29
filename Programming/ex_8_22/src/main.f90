program exercise_8_22
   use Environment
   use IO
   use Sorts 

   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   
   real(R_), allocatable   :: A(:, :)
   integer(I_)             :: M = 0, N = 0

   call ReadP(input_file, N, M, A)
   
   call OutputP(output_file, A)

   call sortMatrix(A)
   
   call OutputMatrix(output_file, A)
end program exercise_8_22
