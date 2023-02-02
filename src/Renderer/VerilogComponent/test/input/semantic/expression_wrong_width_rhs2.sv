module always_comb_case(
    binary_out, //  4 bit binary Output
    in,
    clk
    );
    output bit binary_out  ;
    input bit [1:0] in;
    input bit clk;
          
     always_ff @ (posedge clk) begin
        binary_out <= in[5];
     end

    
    endmodule