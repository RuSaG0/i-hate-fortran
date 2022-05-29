module Group_Process
   use Environment
   use Group_IO

   implicit none

contains

   pure recursive subroutine lexComparer(candidateList, N)
      type(CandidateType), intent(inout)  :: candidateList(:)
      integer, intent(in):: N

      if(N >= 2) then
         call dropDown(candidateList, 1, N-1)
         call lexComparer(candidateList, N-1)
      end if
   end subroutine lexComparer
  
   pure recursive subroutine dropDown(candidateList, j, N)
      type(CandidateType), intent(inout)  :: candidateList(:)
      integer, intent(in)                 :: j, N

      if (swap(candidateList, j)) &
          candidateList(j+1:j) = candidateList(j:j+1:-1)

     if (j < N) &
      call dropDown(candidateList, j+1, N)
   end subroutine dropDown


   pure logical function swap(candidateList, j)
      type(CandidateType), intent(in)  :: candidateList(:)
      integer, intent(in)         :: j

      swap = .false.
      if (candidateList(j+1)%surname < candidateList(j)%surname)then
         swap = .true.
      end if
   end function swap


end module Group_Process
