module always_comb_case(
    in,
    binary_out, //  4 bit binary Output
    clk
    );
    output bit [1:0] binary_out  ;
    input bit [1:0] in;
    input bit clk;
          
     always_ff @(posedge clk) begin
        case (in)
            3'b00:    binary_out <= 2'd2;
            2'b01:    binary_out <= 2'd2;
        endcase
     end

    
    endmodule