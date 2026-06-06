module parameter_check1 #(
  WIDTH = 5,
  CONST = 5 + WIDTH - a
)(
  output bit [2:0] d_out
 );

  bit [WIDTH-1:0] i;
  
  assign i = CONST;
 
endmodule