module  for_loop(
  output bit d_out
 );

 bit [2:0] i;
 //------------Internal Variables--------
 //-------------Code Starts Here---------

  always_comb begin
      for (i = 4'b0; i < 3'd3; i = i + 3'd1) begin
          i = i + 1'd1;
          d_out = d_out + 1'd1;
      end
  end
 
endmodule //End Of Module 