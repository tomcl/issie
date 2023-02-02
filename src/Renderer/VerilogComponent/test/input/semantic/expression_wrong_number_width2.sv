module always_comb_0_width_number(
    out,
    in //  4 bit binary Output
    );
    output bit out  ;
    input bit in;
          
     always_comb begin
        if (1'b10)
            out=in;
        out=1'b1;
        
     end

    
    endmodule