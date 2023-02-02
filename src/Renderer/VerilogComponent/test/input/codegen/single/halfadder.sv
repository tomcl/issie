module ha(input bit a,b, output bit sum, cout);

bit [1:0] tmp;
always_comb begin
    tmp = a+b;
    cout=tmp[1];
    sum=tmp[0];
end
endmodule