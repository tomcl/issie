module always_comb_nonblocking_seqblock(
in,
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
input bit [3:0] in;
      
always_comb begin
   binary_out <= 4'd1; 
   binary_out = in;
end

endmodule