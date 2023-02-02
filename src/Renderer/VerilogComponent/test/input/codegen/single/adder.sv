	module adder(a, b, ci, sum);
	input bit [7:0] a;
	input bit [7:0] b;
	input  bit      ci;
	output bit [7:0] sum;
        
	   assign sum = a + b + ci;

        endmodule