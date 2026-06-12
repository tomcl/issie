module example #(
    WIDTH = 2
)( 
  input bit A, 
  input bit [WIDTH-1:0]  B, 
  output bit O
);

    assign O = A & B[WIDTH-1];
endmodule