module  for_loop(
  output bit [2:0] d_out
 );

 bit [2:0] i;
 //------------Internal Variables--------
 //-------------Code Starts Here---------

  always_comb begin
      i = 1'b0;
      d_out = 1'b0;
      for (i = 4'b0; i < 3'd3; i = i + 3'd1) begin
          d_out[i] = 1'b1;
      end
  end
 
endmodule //End Of Module 