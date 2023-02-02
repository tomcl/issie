module case_comb (input bit [1:0] in, output bit [1:0] out);
always_comb
    case (in)
    2'd1, 2'd3: out=2'd1;
    2'd2: out = 2'd3;
    default: out = 2'h2;
    endcase
endmodule