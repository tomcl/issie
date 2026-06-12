module parameter_newstyle #(
    parameter N = 2,
    parameter DOWN = 0
)( 
    input 	bit	    	clk,
    input 	bit	    	rstn,
    input 	bit	    	en,
    output 	bit     [N-1:0] out);

    always_ff @ (posedge clk) begin
        if (rstn) begin
            out <= 2'd0;
        end else begin
            if (en) begin
                if (DOWN)
                    out <= out - 2'd1;
                else
                    out[0] <= out[1];
                    out[1] <= out[0];
            end else begin
                out <= 2'd(DOWN);
            end
        end
    end
endmodule