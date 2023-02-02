module if_comb (input bit in, output bit [1:0] out, output bit dummy);
assign dummy=in;
always_comb begin
    out=2'b0;
    if(1'b1)
        out=2'd2;
end
endmodule