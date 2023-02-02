module if_ff_const (input bit clk, input bit [1:0] in, output bit [1:0] out);
always_ff @(posedge clk)
    if (5'd4)
        out<=in;

endmodule