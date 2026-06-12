module array_test(
  input bit [1:0] addr,
  output bit [2:0] output1,
  output bit [1:0] array_word1, array_word2, array_word3, array_word4, array_word5, array_word6, array_word7, array_word8, array_word9
);  
  bit [1:0] array_name [9:0];
  
  assign array_name[1] = 1'd1;
  
  always_comb begin
    array_name[0] = addr;
    array_name[2] = addr;
    array_name[3] = addr;
    array_name[4] = addr;
    array_name[5] = addr;
    array_name[6] = addr;
    array_name[7] = addr;
    array_name[8] = addr;
    array_name[9] = addr;
  end
  
   assign output1 = array_name[0];
   assign array_word1= array_name[9];
   assign array_word2= array_name[1];
   assign array_word3= array_name[2];
   assign array_word4= array_name[3];
   assign array_word5= array_name[4];
   assign array_word6= array_name[5];
   assign array_word7= array_name[6];
   assign array_word8= array_name[7];
   assign array_word9= array_name[8];
   
endmodule