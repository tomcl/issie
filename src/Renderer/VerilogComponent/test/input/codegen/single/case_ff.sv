module case_comb (input bit clk, input bit [1:0] in, output bit [1:0] out);
always_ff @(posedge clk)
    case (in==1'b1||in==2'd2)
    1'b1: out<=out << 1'b1;
    1'b0: out<=in>>1'b1;
    default: out<= 2'h2;
    endcase
endmodule