module forloop_infinite3(
  output bit [2:0] d_out
 );

  bit [3:0] i;
  
   always_comb begin
      for (i = 3'd1; i <= 3'd2; i = i + 3'd5) begin
          d_out[i] = 1'd0;
      end
  end
 
endmodule