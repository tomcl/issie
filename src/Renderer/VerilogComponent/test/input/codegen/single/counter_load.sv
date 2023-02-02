        module counter (clk, load, d, q);
        input bit       clk, load;
        input bit [3:0] d;
        output bit [3:0] q;
        bit    [3:0] tmp;
        always_ff @(posedge clk)
        begin
           if (load)
              tmp <= d;
           else
              tmp <= tmp + 1'b1;
        end
           assign q = tmp;
        endmodule 