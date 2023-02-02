module always_ff_blocking(
clk,
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
output bit clk;
 
always_ff @(posedge clk)
    binary_out <= 4'd1; 

endmodule