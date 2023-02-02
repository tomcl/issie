module always_comb_case(
    binary_out,
    in //  4 bit binary Output
    );
    output bit binary_out  ;
    input bit in;
          
     always_comb begin
        case (in)
        0'b1: binary_out = in;
        default: binary_out = in;

        endcase
        
     end

    
    endmodule