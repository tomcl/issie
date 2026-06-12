module parameter_check2 #(
  WIDTH = 5,
  CONST = 5 + WIDTH,
  d_out = 10
)(
  output bit [WIDTH-1:0] d_out
 );

  assign d_out = 5'd(CONST);
 
endmodule