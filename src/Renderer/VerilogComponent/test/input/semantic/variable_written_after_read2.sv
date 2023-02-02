module always_comb_nonblocking(
binary_out, //  4 bit binary Output
out2
);
output bit [3:0] binary_out  ;
output bit [3:0] out2;
 
always_comb begin
    out2 = 4'b0;
    if (binary_out)
        out2 = 4'b1;
    binary_out = 4'd1; 
end


endmodule