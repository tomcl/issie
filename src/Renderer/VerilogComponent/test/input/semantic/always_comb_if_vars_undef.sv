module always_comb_case(
    in,
    binary_out //  4 bit binary Output
    );
    output bit [1:0] binary_out  ;
    input bit [1:0] in;
          
     always_comb begin
        if (in) begin
            binary_out = 2'd2;
        end
     end

    
    endmodule