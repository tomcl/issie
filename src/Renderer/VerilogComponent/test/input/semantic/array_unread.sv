module array_test(
  // Write your IO Port Declarations here
  input bit address,
  input bit clk,
  output bit [2:0] output1, output2, output3,
  output bit output4
);  
  // Write your Assignments here
  bit [2:0] array_name [2:0];
  bit [1:0] i;
  
  always_ff @(posedge clk) begin
    for (i=2'd0;i<=2'd2;i=i+1'd1) begin
      array_name[i] <= address;
    end
  end
  
   assign output1 = array_name[0];
   assign output2 = array_name[1];
   assign output3 = array_name[1];
   
   assign output4 = array_name[2][0];

endmodule