module fa(input bit a,b, cin,  output bit sum, cout);

bit [1:0] tmp;
always_comb begin
    tmp = a+b+cin;
    cout=tmp[1];
    sum=tmp[0];
end
endmodule