`timescale 1ns / 1ps
`include "LEDmatrix.v"

module tb_LEDmatrix(clk,sclk,colEn,sdo);

    /* Clock simulation */
    localparam clock_tick = 83;         // in nanoseconds

    output reg clk;
    output sclk,sdo;
    output [15:0] colEn;

    LEDmatrix uut(
        .clk(clk),
        .ramIn(12'b0),
        .wrAdd(8'b0),
        .we(1'b0),
        .colEn(colEn),
        .sclk(sclk),
        .sdo(sdo)
    );

    //** Clock ****************************************************     
    
    always begin
        clk = 0;
        #(clock_tick/2);
        clk = 1;
        #(clock_tick/2);
    end
    
    initial begin
        $dumpfile("out.vcd");
        $dumpvars(0,tb_LEDmatrix);
        #(100000);
             
        $finish;
  
    end

endmodule