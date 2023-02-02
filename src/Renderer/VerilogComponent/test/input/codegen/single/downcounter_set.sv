        module counter (clk, s, q);
        input bit       clk, s;
        output bit [3:0] q;
        bit    [3:0] tmp;
        always_ff @(posedge clk)
        begin
           if (s)
              tmp <= 4'b1111;
           else
              tmp <= tmp - 1'b1;
        end
           assign q = tmp;
        endmodule