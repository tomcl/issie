// adapted from chip verify
module lshift (input bit				clk,				// Clock input
                   input bit   				rstn,				// Active low reset input
                   input bit [7:0] 			load_val, 	// Load value
                   input bit load_en, 		// Load enable
                   output bit [7:0] op); 				// Output register value

	 bit [2:0] i;

	 // At posedge of clock, if reset is low set output to 0
	 // If reset is high, load new value to op if load_en=1
	 // If reset is high, and load_en=0 shift register to left
	 always_ff @ (posedge clk) begin
	    if (rstn) begin
	      op <= 8'd0;
	    end else begin

	    	// If load_en is 1, load the value to op
	    	// else keep shifting for every clock
	    	if (load_en) begin
	      	op <= load_val;
	      end else begin
            for (i = 3'd0; i < 3'd7; i = i + 3'd1) begin
              op[i+1] <= op[i];
            end
            op[0] <= op[7];
	      end
	    end
	  end
endmodule