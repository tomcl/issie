module fulladder(sum, cout, a, b, cin);
//input output port declarations
   output bit [3:0] sum;
   output bit cout;
   input bit [3:0] a, b;
   input bit cin;
   bit c1, c2, c3;
// Instantiate four 1-bit full adders
   fulladd f0 (.s(sum[0]), .c_out(c1), .ain(a[0]), .bin(b[0]), .c_in(cin));
   fulladd f1 (.s(sum[1]), .c_out(c2), .ain(a[1]), .bin(b[1]), .c_in(c1));
   fulladd f2 (.s(sum[2]), .c_out(c3), .ain(a[2]), .bin(b[2]), .c_in(c2));
   fulladd f3 (.s(sum[3]), .c_out(cout), .ain(a[3]), .bin(b[3]), .c_in(c3));
endmodule