module jparityGenerator(DOUT, parity, DIN);
  output bit [4:0] DOUT;
  output bit parity;
  input bit [3:0] DIN;
  
  assign parity = DIN[0] ^ DIN[1] ^ DIN[2] ^ DIN[3];
  assign DOUT = { DIN, parity };
  
endmodule