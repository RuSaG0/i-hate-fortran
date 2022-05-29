program exercise_7_3
   use Environment

   implicit none
   character(*), parameter:: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0, N = 0, i = 0, j = 0
   real(R_), allocatable   :: A(:, :), absA(:, :)
   real(R_)                :: minValue
    
   integer, allocatable    :: Indexes(:,:), IndexesA(: ,:)
   logical, allocatable    :: Mask(:)
   

   open (file = input_file, newunit = In)
      read (In, *) M, N
      allocate (A(M, N), absA(M, N), IndexesA(M*N, 2))
      read (In, *) (A(i, :), i = 1, M)
   close (In)

   allocate(Mask(M*N), source = .false.)

   open (file = output_file, encoding = E_, newunit = Out)
      write (Out, "("//N//"f6.2)") (A(i, :), i = 1, M)
   close (Out)

   IndexesA(:,1) = [((i, i = 1, M), j = 1, N)]
   IndexesA(:,2) = [((j, i = 1, M), j = 1, N)]

   absA = abs(A)
   minValue = minVal(absA)
  
   Mask = [minValue == absA]

   allocate(Indexes(Count(Mask), 2))

   Indexes(:, 1) = Pack(IndexesA(:,1), Mask)
   Indexes(:, 2) = Pack(IndexesA(:,2), Mask) 

   open (file = output_file, encoding = E_, newunit = Out, position='append')
    write(Out, *) "Indexes of min values: "
    write (Out, '(2i3)' )(Indexes(i, :), i = 1, Ubound( Indexes, 1 ) )
    write(Out, *) "minimal value: ", minValue   
   close (Out)

    end program exercise_7_3
