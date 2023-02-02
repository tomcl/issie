	module flop (clk, d, q);
	input  bit clk, d;
	output bit q;
        
	always_ff @(posedge clk)
	begin
	   q <= d;
	end
        endmodule