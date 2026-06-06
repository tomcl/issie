module forloop_infinite2(
  output bit [2:0] d_out
 );

  bit [3:0] i;
  
 
  always_comb begin
      for (i = 3'd1; i > 3'd3; i = i + 3'd1) begin
          d_out[i] = 1'd0;
      end
  end
 
endmodule