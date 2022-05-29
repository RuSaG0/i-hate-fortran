module Group_Process
   use Environment
   use Group_IO

   implicit none

contains
   pure recursive subroutine getListReg(candidateList, categoryList, registrationCharset, NOT_SERVED_CHARSET, categoryAmount)
      type(CandidateType), intent(in)        :: candidateList
      type(CandidateType), pointer           :: categoryList
      character(*, kind = CH_), intent(in)   :: registrationCharset, NOT_SERVED_CHARSET
      integer(I_), intent(inout)             :: categoryAmount
     
      if (candidateList%registrationCharset == registrationCharset &
         .and. candidateList%isServedCharset /= NOT_SERVED_CHARSET) then

         categoryAmount = categoryAmount+1
         allocate (categoryList, source = candidateList)
         
         if (Associated(candidateList%next)) &
            call getListReg(candidateList%next, categoryList%next, registrationCharset, NOT_SERVED_CHARSET, categoryAmount)
         else if (Associated(candidateList%next)) then
            call getListReg(candidateList%next, categoryList, registrationCharset, NOT_SERVED_CHARSET, categoryAmount)
         else
            categoryList => Null()
      end if
   end subroutine getListReg
 
   pure recursive subroutine lexComparer(candidate, N)
      type(CandidateType), pointer, intent(inout)  :: candidate
      integer, intent(in)                          :: N
      
      if (N >= 2) then
         call dropDown(candidate, 1, N-1)
         call lexComparer(candidate, N-1)
   end if
   end subroutine lexComparer

   pure recursive subroutine dropDown(candidate, j, N)
      type(CandidateType), pointer           :: candidate
      integer, intent(in)                    :: j, N

      if (candidate%next%surname < candidate%surname) &
         call swapFromCurrent(candidate)

      if (j < N) &
         call dropDown(candidate%next, j+1, N)
   end subroutine dropDown

   pure subroutine swapFromCurrent(Current)
      type(CandidateType), pointer  :: Current
      type(CandidateType), pointer  :: tmpCandidate
               
      tmpCandidate => Current%next                         
      Current%next => Current%next%next                        
      tmpCandidate%next => Current             
      Current => tmpCandidate           
   end subroutine swapFromCurrent

end module Group_process
