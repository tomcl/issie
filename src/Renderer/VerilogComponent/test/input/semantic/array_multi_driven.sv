module array_test(
  // Write your IO Port Declarations here
  input bit address,
  output bit [2:0] output1, output2, output3,
  output bit output4
);  
  // Write your Assignments here
  bit [2:0] array_name [2:0];
  bit [1:0] i;
  
  assign array_name[0] = 1'd1;
  
  always_comb begin
    array_name[0] = 1'd0;
  end
  
   assign output1 = array_name[0];
   assign output2 = array_name[1];
   assign output3 = array_name[2];
   
   assign output4 = array_name[2][0];
endmodule