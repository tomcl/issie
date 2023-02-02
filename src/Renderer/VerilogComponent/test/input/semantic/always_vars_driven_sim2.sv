module always_vars_driven_sim(
clk,
in,
binary_out //  4 bit binary Output
);
output bit [1:0] binary_out  ;
input bit [1:0] in;
input bit clk;
      
 always_comb begin
    case (in)
        2'b00:      binary_out = 2'd0; 
        2'b01:      binary_out = 2'd1; 
        default:    binary_out = 2'd2; 
    endcase
 end

 always_ff @(posedge clk) begin
    binary_out <= 2'd1;
 end

endmodule