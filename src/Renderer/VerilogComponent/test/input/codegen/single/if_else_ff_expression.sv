module if_ff_const (input bit clk, input bit [1:0] in, output bit [1:0] out);
always_ff @(posedge clk)
    if (in==1'b1)
        out<=in;
    else out <= out-1'b1;

endmodule