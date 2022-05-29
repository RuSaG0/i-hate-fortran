program reference_lab_1_4_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable:: input_file, output_file, data_file

   character(MILITARY_LENGTH, kind = CH_), parameter   :: NOT_SERVED_CHARSET = Char(1053, CH_) // Char(1077, CH_) // Char(1090, CH_)
   character(kind = CH_), parameter                    :: P_CHAR = Char(1055, CH_), &
                                                          G_CHAR = Char(1057, CH_) 
   
   type(CandidateType)                                  :: candidateList(CANDIDATES_AMOUNT)
   type(CandidateType), allocatable                     :: SPBMiltary(:), guestMiltary(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call createDataFile(input_file, data_file)
   
   candidateList = readClassList(data_file)

   call outputData(output_file, candidateList, "Исходный список:", "rewind")

   SPBMiltary  = Pack(candidateList, candidateList%registrationCharset == P_CHAR .and. &
                      candidateList%isServedCharset /= NOT_SERVED_CHARSET)

   guestMiltary  = Pack(candidateList, candidateList%registrationCharset == G_CHAR .and. & 
                        candidateList%isServedCharset /= NOT_SERVED_CHARSET)

   call lexComparer(SPBMiltary)
   call lexComparer(guestMiltary)

   call outputData(output_file, SPBMiltary, "Служившие Питер:", "append")
   call outputData(output_file, guestMiltary, "Служившие Гости:", "append")

end program reference_lab_1_4_3
