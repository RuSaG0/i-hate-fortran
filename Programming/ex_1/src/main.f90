program exercise_1_5
   use Environment
   
   implicit none
   character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
   character(:), allocatable  :: fmt
   integer                    :: In = 0, Out = 0, k = 0, tmp = 1
   real(R_)                   :: x = 0, result
   real(R_)                   :: cosTaylorCandidates(4) = 0

    open (file = input_file, newunit = In)
      read (In, *) x
   close (In)

   open (file = output_file, encoding = E_, newunit = Out)
      fmt = "(a, T7, '= ', f6.2)"
      write (Out, fmt) "x", x
   close (Out)

   cosTaylorCandidates(1) = 1
   
   do k = 2, 4
        cosTaylorCandidates(k) = cosTaylorCandidates(k-1) * (-(x**2)) / (k * (k-1) * tmp)
        tmp = k * (k-1)
   end do
   
   result = Sum(cosTaylorCandidates)


   open (file = output_file, encoding = E_, newunit = Out, position='append')
      write (Out, fmt) "result", result
   close (Out)
   
end program exercise_1_5
