module always_ff_nonblocking_constant (input bit clk, input bit in, output bit [3:0] out, output bit out2);
always_ff @(posedge clk)
    out <= 4'd4;
assign out2 = in;
endmodule