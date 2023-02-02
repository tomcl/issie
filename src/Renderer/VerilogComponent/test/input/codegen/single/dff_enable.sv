	module flop (clk, d, ce, q);
	input  bit clk, d, ce;
	output bit q;
	always_ff @(posedge clk) 
        begin
	        if (ce)
              q <= d;
	end
        endmodule
        