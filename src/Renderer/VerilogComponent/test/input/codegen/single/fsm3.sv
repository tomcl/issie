	module fsm (clk, reset, x1, outp);
	input bit        clk, reset, x1;
	output bit      outp;
	bit    [1:0] state;
	bit [1:0] s1 = 2'b00; bit [1:0] s2 = 2'b01;
	bit [1:0] s3 = 2'b10; bit [1:0] s4 = 2'b11;
	always_ff @(posedge clk)
	begin
	   if (reset) begin
	      state <= s1; outp <= 1'b1;
	   end 
	   else begin
	      case (state)
		 2'b00: begin 
			if (x1 == 1'b1) begin
			   state <= s2;
                           outp  <= 1'b1;
			end
			else begin
			   state <= s3;
                           outp  <= 1'b1;
			end
		     end
		 2'b01: begin
			state <= s4; 
                        outp  <= 1'b0;
		     end
		 2'b10: begin
			state <= s4; 
                        outp  <= 1'b0;
		     end
		 2'b11: begin
			state <= s1; 
                        outp  <= 1'b1;
		     end
	      endcase
	   end
	end
        endmodule