module array_test(
  // Write your IO Port Declarations here
  output bit [2:0] output1, output2
);  
  // Write your Assignments here
  bit [1:0] array_name [1:0];
  
  assign array_name[1] = 1'd1;
  
  always_comb begin
    array_name[0] = 1'd0;
  end
  
   assign output1 = array_name[0];
   assign output2 = array_name[1];
   
endmodule