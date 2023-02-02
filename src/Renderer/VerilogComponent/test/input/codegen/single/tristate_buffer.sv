module tristate_buffer(a,c,y);
	input bit a,c;
	output bit y;
	assign y= c ? a : 1'b0;	
endmodule