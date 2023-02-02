module always_ff_blocking_case(
clk,
in,
binary_out //  4 bit binary Output
);
output bit [1:0] binary_out  ;
input bit [1:0] in;
input bit clk;

 always_ff @(posedge clk) begin
    case (in)
        2'b00:    binary_out = 2'd0; 
        2'b01:    binary_out <= 2'd1; 
        2'b10:    binary_out <= 2'd2; 
        2'b11:    binary_out <= 2'd3; 
    endcase
 end

endmodule