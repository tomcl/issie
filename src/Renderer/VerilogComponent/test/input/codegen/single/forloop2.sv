module  for_loop(
  input bit a,
  input bit clk,
  output bit [25:0] d_out
 );

 bit [6:0] i;
 //------------Internal Variables--------
 //-------------Code Starts Here---------

  always_ff @ (posedge clk) begin
      for (i = 7'd0; i < 7'd15; i = i + 7'd1) begin
          d_out[i] <= a;
      end
  end
 
endmodule //End Of Module 