	module mult(clk, a, b, mult);
	input  bit       clk;
	input bit [17:0] a;
	input bit [17:0] b;
	output bit [35:0] mult;
	bit    [17:0] a_in, b_in;
	bit    [35:0] mult_res;
	bit    [35:0] pipe_2, pipe_3;
	always_ff @(posedge clk)
	begin
	   a_in     <= a;
	   b_in     <= b;
	   mult_res <= a_in * b_in;
	   pipe_2   <= mult_res;
	   pipe_3   <= pipe_2;
	   mult     <= pipe_3;
	end
        endmodule