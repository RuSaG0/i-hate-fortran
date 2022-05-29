program exercise_3
   use Environment
   
   implicit none
   integer, parameter        :: IK = selected_int_kind(18)
   character(*), parameter   :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                   :: In = 0, Out = 0, N = 0
   real(R_)                  :: P
    

   open (file = input_file, newunit = In)
      read (In, *) N
   close (In)
   P = ProdImp()

   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, "(i0)") N
      write (Out, *)
      write (Out, "('Answer: = ', f0.2)") P
   close (Out)

contains
   
   pure function ProdImp() result(Prod)
      real(R_)    Prod
      real(R_), allocatable:: fact(:)
      integer     i
      allocate(fact(N))

      Prod = 1
      fact(1) = 1
      
      do i = 2, N
         fact(i) = fact(i-1) * i
      end do
      fact = [((((i+1)/fact(i))+i), i = 1, N)]
      
      Prod = Product(fact) 
        
     end function ProdImp
 

end program exercise_3
