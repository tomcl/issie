module if_comb_expression(input bit [1:0] in, output bit [1:0] out );
always_comb
    if(in==1'b1)
        out = 2'd2;
    else out=2'd3;
endmodule