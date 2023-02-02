module always_ff_blocking(
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
bit clk = 1'b1;
 
always_comb
    binary_out = 4'd1; 

endmodule