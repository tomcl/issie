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
            out <= 2'd0;
        end else begin
            if (en) begin
                if (DOWN)
                    out <= out - 2'd1;
                else
                    out[DOWN] <= out[0];
                    out[1] <= out[DOWN];
            end else begin
                out <= out;
            end
        end
    end
endmodule