module counter #(
    parameter N = 2,
    parameter DOWN = 0
)( 
    input 	bit	    	clk,
    input 	bit	    	rstn,
    input 	bit	    	en,
 	output 	bit [N-1:0] out);

 always_ff @ (posedge clk) begin
 if (rstn) begin
 out <= 0;
 end else begin
 if (en)
 if (DOWN)
 out <= out - 1;
 else
 	out <= out + 1;
 else
 out <= out;
 end
 end
endmodule