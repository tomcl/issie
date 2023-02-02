// the N bit inverter doesn't seem to work
module jinverter(y,a);
	output bit y;
	input bit a;
	
	assign y=~a;
	
endmodule