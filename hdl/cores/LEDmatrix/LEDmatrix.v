

module LEDmatrix (clk,ramIn,wrAdd,we,colEn,sclk,sdo);
    parameter PRESCALE = 6;
    parameter COL_PAD = 8; //Extra prescaler cycles to add to column interval before sending row data
    parameter COL_N = 16;
    parameter ROW_N = 16;
    parameter BLANK_PRE = 12;
    parameter BLANK_POST = 12;
    parameter BITS_PER_COL = 4;
    parameter COLOURS = 3;

    localparam DATA_W = BITS_PER_COL * COLOURS;
    localparam SERIAL_W = ROW_N * COLOURS;
    localparam COL_W = $clog2(COL_N);
    localparam ROW_W = $clog2(ROW_N);
    localparam ADD_W = COL_W + ROW_W;
    
    input clk;
    input [DATA_W-1 : 0] ramIn;
    input [ADD_W - 1 : 0] wrAdd;
    input we;
    output reg [COL_N-1:0] colEn = 0;
    output reg sclk = 0;
    output sdo;

    reg[7:0] prescaler = 0;
    reg[7:0] colTimer = 0;
    reg[COL_W-1:0] colCount = 0;
    reg[ROW_W-1:0] rowCount = 0;
    reg[COLOURS-1:0] rgbCycle = 3'b100;
    reg[7:0] blankCount = 0;
    reg[BITS_PER_COL-1:0] value, pwmRef;
    reg[BITS_PER_COL-1:0] frCount = 0;
    wire sclk_int;
    wire ps_out;
    wire [DATA_W-1:0] ramOut;
    wire [ADD_W-1:0] rdAdd;
    integer i;

    //Video RAM
    VRAM #(.D_WIDTH(DATA_W), .A_WIDTH(ADD_W)) vram (.q(ramOut), .ra(rdAdd), .wa(wrAdd), .d(ramIn), .we(we), .clk(clk));

    //Prescaler
    assign ps_out = (prescaler == 0);
    always@(posedge clk) begin
        if (ps_out)
            prescaler <= PRESCALE - 1;
        else
            prescaler <= prescaler - 1;
    end

    //Column Timer
    assign colAdv = (colTimer == 0);
    always@(posedge clk) begin
        if (ps_out)
            if (colAdv)
                colTimer <= COL_PAD + SERIAL_W * 2 - 1;
            else
                colTimer <= colTimer - 1;
    end

    //Column Counter
    assign frAdv = (colCount == 0);
    always@(posedge clk) begin
        if (ps_out && colAdv)
            if (frAdv) begin
                colCount <= COL_N - 1;
                frCount <= frCount + 1;
            end
            else
                colCount <= colCount - 1;
    end

    //Column select, latch and blanking
    always@(posedge clk) begin
        if (ps_out && colAdv)
            blankCount <= BLANK_PRE + BLANK_POST - 1;
        else if (blankCount != 0)
            blankCount <= blankCount - 1;
        
        if (blankCount == BLANK_POST)
            for (i = 0; i < COL_N; i = i + 1) //Loop creates a line decoder
                colEn[i] <= (colCount == i);
    end
    assign blank = (blankCount != 0);
    assign lat = blank;

    //Serial output
    always@(posedge clk) begin
        
        // Serial Idle until final ROW_N * 3 * 2 prescaler cycles of each column
        if (colTimer > ROW_N * COLOURS * 2) begin
            sclk <= 1'b0;
            rgbCycle = 1 << COLOURS - 1;
            rowCount = ROW_N-1;
        end
        // Run serial for final cycles
        else if (colTimer && ps_out) begin
            //Data update on serial clock falling edge
            if (colTimer[0]) begin
                //Cycle colours, increment row after last colour
                rgbCycle <= {rgbCycle[0], rgbCycle[COLOURS-1 : 1]};
                if (rgbCycle[0])
                    rowCount <= rowCount - 1;
                sclk <= 0;
            end
            else begin
                sclk <= 1;
            end
        end
            
    end

    //Generate PWM reference by reversing LSBs of frame count
    integer j;
    always @(*) begin
        for (j = 0; j < BITS_PER_COL; j = j + 1)
            pwmRef[j] = frCount[BITS_PER_COL - 1 - j];
    end

    //Pixel Lookup
    assign rdAdd = {rowCount,colCount};
    integer q;
    //Select bits of VRAM output depending on which colour is transmitted next
    always @(*) begin
        value <= ramOut[BITS_PER_COL - 1 : 0];
        for (q = 0; q < COLOURS; q = q + 1) //Loop + if creates a multiplexer
            if (rgbCycle[q])
                value <= ramOut[BITS_PER_COL*q +: BITS_PER_COL];
    end

    //Generate serial output from PWM comparator
    assign sdo = (value > pwmRef);



endmodule

module VRAM(q, ra, d, wa, we, clk);
    parameter D_WIDTH = 12;
    parameter A_WIDTH = 8;
    output reg [D_WIDTH-1:0] q;
    input [D_WIDTH-1:0] d;
    input [A_WIDTH-1:0] ra;
    input [A_WIDTH-1:0] wa;
    input we, clk;
    reg [D_WIDTH-1:0] ram [A_WIDTH-1:0];
     always @(posedge clk) begin
         if (we)
             ram[wa] <= d;
         q <= ram[ra];
     end

    integer i,j;
    reg[3:0] red,grn,blu;
    reg[7:0] add;
    initial begin
        $display("Initialising VRAM");
        //$readmemh("disp_init.hex", ram);
        for (i = 0; i < 256; i = i + 1)
            ram[i] = 0;
        // for (i = 0; i < 16; i = i + 1)
        //     for (j = 0; j < 16; j = j + 1) begin
        //         red = i+j;
        //         grn = i+j+16/3;
        //         blu = i+j+32/3;
        //         add = i*16+j;
        //         ram[add] = {grn,blu,red};
        //     end
    end
endmodule