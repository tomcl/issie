        module accum (clk, clr, d, q);
        input  bit      clk, clr;
        input  bit [3:0] d;
        output bit [3:0] q;
        bit    [3:0] tmp;
        always_ff @(posedge clk)
        begin
           if (clr)
              tmp <= 4'b0000;
           else
              tmp <= tmp + d;
        end
           assign q = tmp;
        endmodule