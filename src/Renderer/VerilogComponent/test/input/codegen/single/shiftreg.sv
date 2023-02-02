	module shift (clk, si, so);
	input bit       clk,si;
	output bit      so;
	bit    [7:0] tmp;
	always_ff @(posedge clk)
	begin
	   tmp    <= tmp << 1;
	   tmp[0] <= si;
	end
	   assign so = tmp[7];
        endmodule