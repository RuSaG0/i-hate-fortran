module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains

   pure subroutine lexComparer(candidateList)
      type(CandidateType), intent(inout)  :: candidateList(:)
      integer                             :: i, j

      do i = Size(candidateList), 2, -1
         do j = 1, i - 1
            if (candidateList(j+1)%surname < candidateList(j)%surname) &
               candidateList([j+1, j]) = candidateList([j, j+1])
         end do
      end do
   end subroutine lexComparer

end module group_process
