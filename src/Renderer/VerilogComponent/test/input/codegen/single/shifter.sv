	module lshift (di, sel, so);
        input bit [7:0] di;
	input bit [1:0] sel;
	output bit [7:0] so;

	always_comb
	begin
	   case (sel)
	      2'b00   : so = di;
	      2'b01   : so = di << 1;
	      2'b10   : so = di << 2;
	      default : so = di << 3;
	   endcase
	end
        endmodule