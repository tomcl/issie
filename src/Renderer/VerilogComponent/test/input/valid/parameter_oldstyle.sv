// Verilog 1995 style port declaration
module design_ip ( addr,
 wdata,
 write,
 sel,
 rdata);

 parameter BUS_WIDTH = 32,
 DATA_WIDTH = 64,
 FIFO_DEPTH = 512;

 input bit addr;
 input bit wdata;
 input bit write;
 input bit sel;
 output bit rdata;

 bit [BUS_WIDTH-1:0] addr;
 bit [DATA_WIDTH-1:0] wdata;
 bit [DATA_WIDTH-1:0] rdata;

 bit [FIFO_DEPTH-1:0] fifo;

 // Design code goes here ...
endmodule