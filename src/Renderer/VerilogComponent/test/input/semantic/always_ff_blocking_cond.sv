module always_comb_nonblocking_cond(
clk,
in,
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
input bit [3:0] in;
input bit clk;
      
 always_ff @ (posedge clk) begin
    if(1'b1)
        binary_out = 4'd1; 
    else
        binary_out <= in;
 end

endmodule