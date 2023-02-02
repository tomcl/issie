module juniversalShiftRegister(DATAOUT, clk, reset, MODE, DATAIN);
  output bit [3:0] DATAOUT;
  input bit clk, reset;
  input bit [1:0] MODE;
  input bit [3:0] DATAIN;
  
  always_ff @(posedge clk)
  begin
    if(reset)
      DATAOUT <= 4'b0;
    else
      begin
        case(MODE)
          2'b00 : DATAOUT <= DATAOUT;      // locked mode, do nothing
          2'b01 : DATAOUT <= {DATAIN[0], DATAOUT[3:1]};//DATAOUT >> 1; // RFSR
          2'b10 : DATAOUT <= {DATAOUT[2:0], DATAIN[0]};//DATAOUT << 1; // LFSR
          2'b11 : DATAOUT <= DATAIN;       // parallel in parallel out
        endcase
      end
  end
  
endmodule