module array_test(
  input bit addr,
  output bit [2:0] output1, output2
);  
  bit [1:0] array_name [1:0];
  
  assign array_name[1] = 1'd1;
  
  always_comb begin
    array_name[0] = addr;
  end
  
   assign output1 = array_name[0];
   assign output2 = array_name[1];
   
endmodule