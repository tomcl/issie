module always_ff_nonblocking_expression (input bit clk, input bit [1:0] in1, input bit [2:0] in2, output bit [4:0] out);
always_ff @(posedge clk)
    out <= {in1, in2};
endmodule