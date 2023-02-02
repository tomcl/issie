module always_comb_nonblocking_cond_else(
in,
binary_out //  4 bit binary Output
);
output bit [3:0] binary_out  ;
input bit [3:0] in;
      
 always_comb begin
    if(1'b1) begin
        binary_out = 4'd1; 
    end
    else begin
        binary_out <= in;
    end
 end

endmodule