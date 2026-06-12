// A 4-bit left shift register.

module left_shift_register_behavioral (
    input            clk,           // clk
    input            rst,           // Reset
    input            d,             // d
    output bit [3:0] out);          // out

    // LEFT SHIFT REGISTER
    // ALWAYS BLOCK with NON-BLOCKING PROCEDURAL ASSIGNMENT STATEMENT
    always_ff @ (posedge clk) begin
        if (rst) begin
            out <= 4'b0000;
        end else begin
            out <= {out[2:0], d};
        end
    end

endmodule