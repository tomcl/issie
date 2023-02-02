module always_comb_case(
    in,
    binary_out //  4 bit binary Output
    );
    output bit [1:0] binary_out  ;
    input bit [1:0] in;
          
     always_comb begin
        case (in)
            2'b00:    binary_out = 2'd2;
            2'b01:    binary_out = 2'd2;
            2'b10:    binary_out = 2'd2;
            2'b11:    binary_out = 2'd2;
            2'b00:    binary_out = 2'd3; 
        endcase
     end

    
    endmodule