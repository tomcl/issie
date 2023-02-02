module decoder (input bit en, input bit[3:0] in, output bit[15:0] out);

always_comb begin
    out = en ? 16'b1 << in : 16'd0;
end
endmodule