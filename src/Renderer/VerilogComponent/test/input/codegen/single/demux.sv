module demux(input bit f, input bit [1:0] sel, output bit a,b,c,d);
    always_comb begin
        a = f & ~sel[1] & ~sel[0];
        b = f & sel[1] & ~sel[0];
        c = f & ~sel[1] & sel[0];
        d = f & sel[1] & sel[0];
    end
endmodule