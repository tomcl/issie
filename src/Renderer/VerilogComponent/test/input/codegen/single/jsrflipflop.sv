module jsrflipflop(q,clk,rst,sr);
	output bit q;
	//output bit qbar;
	input bit clk, rst;
	input bit [1:0] sr;

	//assign qbar = ~q;

	always_ff @(posedge clk)
	begin
		if (rst)
			q <= 1'b0;
		else
			case(sr)
				2'b00: q <= q;
				2'b01: q <= 1'b0;
				2'b10: q <= 1'b1;
				2'b11: q <= 1'b0;
			endcase
	end
endmodule