module always_comb_case(
    binary_out, //  4 bit binary Output
    in
    );
    output bit binary_out  ;
    input bit [1:0] in;
          
     always_comb begin
        binary_out = in[5];
     end

    
    endmodule