module always_comb_nonblocking(
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
 
always_comb
    binary_out <= 4'd1; 

endmodule