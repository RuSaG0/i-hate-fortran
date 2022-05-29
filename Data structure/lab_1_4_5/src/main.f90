program reference_lab_1_4_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable:: input_file, output_file
   
   character(MILITARY_LENGTH, kind = CH_), parameter     :: NOT_SERVED_CHARSET = Char(1053, CH_) // & 
                                                                                Char(1077, CH_) // & 
                                                                                Char(1090, CH_)

   character(kind = CH_), parameter                      :: P_CHAR = Char(1055, CH_), G_CHAR = Char(1057, CH_)
   
   type(CandidateType), pointer  :: candidatesList => Null(), SPBList => Null(), guestList => Null()
   integer(I_)                   :: SPBAmount = 0, guestAmount = 0

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   
   candidatesList => readClassList(input_file)

   if (Associated(candidatesList)) then
      call outputData(output_file, candidatesList, "Исходный список:", "rewind")

      call getListReg(candidatesList, SPBList, P_CHAR, NOT_SERVED_CHARSET, SPBAmount)
      call getListReg(candidatesList, guestList, G_CHAR, NOT_SERVED_CHARSET, guestAmount)
  
      call lexComparer(SPBList, SPBAmount)
      call lexComparer(guestList, guestAmount)
   
      if (Associated(SPBList)) &
         call outputData(output_file, SPBList, "Служившие Питер:", "append")
      if (Associated(guestList)) &
         call outputData(output_file, guestList, "Служившие Гости:", "append")
   end if

end program reference_lab_1_4_5
