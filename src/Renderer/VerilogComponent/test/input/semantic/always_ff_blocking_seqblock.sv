module always_ff_blocking_seqblock(
clk,
in,
out,
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
output bit out;
input bit [3:0] in;
input bit clk;
      
always_ff @ (posedge clk) begin
   binary_out <= 4'd1; 
   out = in[0];
end

endmodule