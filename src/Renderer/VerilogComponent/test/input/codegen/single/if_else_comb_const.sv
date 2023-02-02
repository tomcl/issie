module if_comb (input bit in, output bit [1:0] out, output bit dummy);
assign dummy=in;
always_comb
    if(3'd5)
        out=2'd3;
    else
        out=2'd1;
endmodule