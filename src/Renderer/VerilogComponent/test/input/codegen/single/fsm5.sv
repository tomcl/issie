	module fsm (clk, reset, x1, outp);
	input bit       clk, reset, x1;
	output bit      outp;

	bit    [1:0] state;
	bit    [1:0] next_state;
	bit [1:0] s1 = 2'b00; bit [1:0] s2 = 2'b01;
	bit [1:0] s3 = 2'b10; bit [1:0] s4 = 2'b11;
	always_ff @(posedge clk)
	begin
	   if (reset)
	      state <= s1;
	   else 
	      state <= next_state;
	end

	always_comb
	begin
	   case (state)
	      2'b00: if (x1 == 1'b1)
		     next_state = s2;
		  else
		     next_state = s3;
	      2'b01: next_state = s4;
	      2'b10: next_state = s4;
	      2'b11: next_state = s1;
	   endcase
    end

    always_comb begin
	   case (state)
	      2'b00: outp = 1'b1;
	      2'b01: outp = 1'b1;
	      2'b10: outp = 1'b0;
	      2'b11: outp = 1'b0;
	   endcase
	end
    
    endmodule