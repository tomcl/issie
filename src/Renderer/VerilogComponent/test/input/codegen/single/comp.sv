	module compar(a, b, cmp);
	input bit [7:0] a;
	input bit [7:0] b;
	output bit      cmp;

	   assign cmp = (a >= b) ?  1'b1 : 1'b0;

        endmodule