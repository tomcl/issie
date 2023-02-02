module always_comb_blocking_constant (input bit in, output bit [3:0] out, output bit out2);
always_comb
    out = 4'd4;
assign out2 = in;
endmodule