module always_comb_blocking_constant (input bit [1:0] in, output bit [3:0] out);
always_comb
    out = 2'd1 + in * 2'd2 ;
endmodule