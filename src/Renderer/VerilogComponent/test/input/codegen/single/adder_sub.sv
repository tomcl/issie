	module addsub(a, b, oper, res);
	input bit       oper;
	input bit [7:0] a;
	input bit [7:0] b;
	output bit [7:0] res;

	always_comb
	begin
	   if (oper == 1'b0)
	      res = a + b;
	   else
	      res = a - b;
        end
        endmodule