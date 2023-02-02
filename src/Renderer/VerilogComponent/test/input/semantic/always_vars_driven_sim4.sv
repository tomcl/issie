module always_vars_driven_sim(
clk,
in,
binary_out //  4 bit binary Output
);
output bit [1:0] binary_out  ;
input bit [1:0] in;
input bit clk;
      
 always_ff @ (posedge clk) begin
    case (in)
        2'b00:      binary_out <= 2'd0; 
        2'b01:      binary_out <= 2'd1; 
        default:    binary_out <= 2'd2; 
    endcase
 end


    assign binary_out = 2'd1;

endmodule