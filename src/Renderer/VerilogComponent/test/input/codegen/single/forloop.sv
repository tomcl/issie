module  for_loop(
  input bit a,
  output bit [2:0] d_out
 );

 bit [2:0] i;
 //------------Internal Variables--------
 //-------------Code Starts Here---------

  always_comb begin
      for (i = 3'd0; i < 3'd3; i = i + 3'd1) begin
          d_out[i] = a;
      end
  end
 
endmodule //End Of Module 