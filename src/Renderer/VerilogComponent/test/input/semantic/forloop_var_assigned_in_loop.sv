module forloop_var_assigned_in_loop(
  output bit [2:0] d_out
 );
 
  bit [2:0] i;
 
  always_comb begin
      d_out = 3'b000;
      for (i = 3'd0; i < 3'd3; i = i + 3'd1) begin
          i = 3'd1; // for loop variable assigned to in loop
      end
  end
 
endmodule