	module counter (clk, clr, up_down, q);
	input bit       clk, clr, up_down;
	output bit [3:0] q;
	bit    [3:0] tmp;
	always_ff @(posedge clk)
	begin
	   if (clr)
	      tmp <= 4'b0000;
	   else if (up_down) 
	      tmp <= tmp + 1'b1;
	   else
	      tmp <= tmp - 1'b1;
	end
	   assign q = tmp;
        endmodule
        