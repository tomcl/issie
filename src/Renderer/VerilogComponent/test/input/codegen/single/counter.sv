module synchronouscounter(clk,reset,count);
	input bit clk,reset;
	output bit [3:0] count;

        always_ff@(posedge clk)
        begin
                if(reset)
                        count <= 4'b0000;
                else
                        count <= count + 1'b1;
        end

endmodule