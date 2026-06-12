module parameter_newstyle #(
    parameter N = 6,
    parameter DOWN = 31
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
                if (0)
                    out <= out - 2'd1;
                else
                    out[0] <= out[5];
                    out[1] <= out[0];
                    out[2] <= out[1];
                    out[3] <= out[2];
                    out[4] <= out[3];
                    out[5] <= out[4];
            end else begin
                out <= 5'd(DOWN);
            end
        end
    end
endmodule