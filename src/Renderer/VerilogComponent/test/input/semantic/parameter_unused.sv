module parameter_check2 #(
  WIDTH = 5,
  CONST = 5 + WIDTH,
  EXTRA = 10
)(
  output bit [WIDTH-1:0] d_out
 );
  
  bit [WIDTH-1:0] a;
  
  assign a = 5'd(CONST);
  assign d_out = a;
 
endmodule