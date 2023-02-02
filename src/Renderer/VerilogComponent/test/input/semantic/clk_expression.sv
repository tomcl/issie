module always_ff_blocking(
    clk,
binary_out //  4 bit binary Output
);
output bit binary_out  ;
input bit clk;

always_ff @(posedge clk)
    binary_out <= clk; 

endmodule