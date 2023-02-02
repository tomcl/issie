module always_comb_case(
    binary_out //  4 bit binary Output
    );
    output bit [1:0] binary_out  ;
          
     always_comb begin
        binary_out[4] = 1'd1;
        binary_out = 2'd0;
     end

    
    endmodule