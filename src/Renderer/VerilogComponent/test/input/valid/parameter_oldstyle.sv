// Verilog 1995 style port declaration
// module parammeter_oldstyle ( addr,
//  wdata,
//  write,
//  sel,
//  rdata);
 
//  parameter BUS_WIDTH = 32,
//  DATA_WIDTH = 64,
//  FIFO_DEPTH = 512;
//  input bit [BUS_WIDTH-1:0] addr;
//  input bit [DATA_WIDTH-1:0] wdata;
//  input bit write;
//  input bit sel;
//  output bit [DATA_WIDTH-1:0] rdata;
//  input bit clk;
 
//  bit [DATA_WIDTH-1:0] fifo [FIFO_DEPTH-1:0];
 
//  always_ff @ (posedge clk) begin
//      if (write) begin
//          fifo[addr] <= wdata;
//      end else if (sel) begin
//          rdata <= fifo[addr];
//      end else begin
//          rdata <= 0;
//      end
//  end
 
// endmodule

module parammeter_oldstyle (
    addr,
    wdata,
    write,
    sel,
    rdata
);

parameter BUS_WIDTH  = 32,
          DATA_WIDTH = 64,
          FIFO_DEPTH = 512;

input  bit [BUS_WIDTH-1:0]  addr;
input  bit [DATA_WIDTH-1:0] wdata;
input  bit                  write;
input  bit                  sel;
input  bit                  clk;
output bit [DATA_WIDTH-1:0] rdata;

bit [DATA_WIDTH-1:0] fifo [FIFO_DEPTH-1:0];

bit i;

always_ff @(posedge clk) begin
  for (i = 1'd0; i < FIFO_DEPTH; i = i + 1) begin
      if (addr == i) 
        fifo[i] <= wdata;
  end
end

endmodule