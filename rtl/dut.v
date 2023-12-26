
`include "defines.vh"

//---------------------------------------------------------------------------
// DUT 
//---------------------------------------------------------------------------
module MyDesign(
//---------------------------------------------------------------------------
//System signals
  input wire reset_n                      ,  
  input wire clk                          ,

//---------------------------------------------------------------------------
//Control signals
  input wire dut_valid                    , 
  output wire dut_ready                   ,

//---------------------------------------------------------------------------
//q_state_input SRAM interface
  output wire                                               q_state_input_sram_write_enable  ,
  output wire [`Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_input_sram_write_address ,
  output wire [`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:0]    q_state_input_sram_write_data    ,
  output wire [`Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_input_sram_read_address  , 
  input  wire [`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:0]    q_state_input_sram_read_data     ,

//---------------------------------------------------------------------------
//q_state_output SRAM interface
  output wire                                                q_state_output_sram_write_enable  ,
  output wire [`Q_STATE_OUTPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_output_sram_write_address ,
  output wire [`Q_STATE_OUTPUT_SRAM_DATA_UPPER_BOUND-1:0]    q_state_output_sram_write_data    ,
  output wire [`Q_STATE_OUTPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_output_sram_read_address  , 
  input  wire [`Q_STATE_OUTPUT_SRAM_DATA_UPPER_BOUND-1:0]    q_state_output_sram_read_data     ,

//---------------------------------------------------------------------------
//scratchpad SRAM interface                                                       
  output wire                                                scratchpad_sram_write_enable        ,
  output wire [`SCRATCHPAD_SRAM_ADDRESS_UPPER_BOUND-1:0]     scratchpad_sram_write_address       ,
  output wire [`SCRATCHPAD_SRAM_DATA_UPPER_BOUND-1:0]        scratchpad_sram_write_data          ,
  output wire [`SCRATCHPAD_SRAM_ADDRESS_UPPER_BOUND-1:0]     scratchpad_sram_read_address        , 
  input  wire [`SCRATCHPAD_SRAM_DATA_UPPER_BOUND-1:0]        scratchpad_sram_read_data           ,

//---------------------------------------------------------------------------
//q_gates SRAM interface                                                       
  output wire                                                q_gates_sram_write_enable           ,
  output wire [`Q_GATES_SRAM_ADDRESS_UPPER_BOUND-1:0]        q_gates_sram_write_address          ,
  output wire [`Q_GATES_SRAM_DATA_UPPER_BOUND-1:0]           q_gates_sram_write_data             ,
  output wire [`Q_GATES_SRAM_ADDRESS_UPPER_BOUND-1:0]        q_gates_sram_read_address           ,  
  input  wire [`Q_GATES_SRAM_DATA_UPPER_BOUND-1:0]           q_gates_sram_read_data              
);

// CONTROL SIGNALS REGISTERS
reg [1:0] wo_addr_sel;
reg [1:0]      dc_sel;
reg [1:0]     col_sel;
reg [1:0]     row_sel;
reg [1:0]      qs_sel;
reg [1:0]  qg_add_sel;
reg [1:0]   mac_i_sel;
reg           mac_sel;
reg [5:0]     present;
reg [5:0]        next;
reg       wo_enable_r;

reg       dut_ready_r;
reg [1:0]     mat_Sel;



reg [1:0]   r_ary_sel;
reg [1:0]   w_arry_sel;
reg [63:0] r_arry_add;
reg [63:0] w_arry_add;
reg [1:0]  w_arry_enable;

// Counter registers
reg [(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0] row_counter                            ;
reg [(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0] col_counter                            ;
reg [(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0] matrix_counter;
// q_gate SRAM registers
reg [`Q_GATES_SRAM_ADDRESS_UPPER_BOUND-1:0] q_gates_sram_read_address_r                   ;             

//q_state_output SRAM registers
reg [`Q_STATE_OUTPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_output_sram_write_address_r    ;
reg [(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0] dc_a                                   ;


// q_state_input SRAM registers
reg [`Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND-1:0] q_state_input_sram_read_address_r       ;


// Temporary mem buffers
reg[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:0]  mem_buf [0:15];
reg[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:0] mem_buf1 [0:15];



assign q_gates_sram_read_address = q_gates_sram_read_address_r;
assign q_state_output_sram_write_address = q_state_output_sram_write_address_r;
assign q_state_input_sram_read_address = q_state_input_sram_read_address_r;
assign q_state_output_sram_write_enable = wo_enable_r;
assign dut_ready = dut_ready_r;
assign q_state_input_sram_write_enable = 0;
assign q_gates_sram_write_enable = 0;


localparam inst_sig_width = 52;
localparam inst_exp_width = 11;
localparam inst_ieee_compliance = 3;

// Initialising registers for DW FP MACS

  reg  [inst_sig_width+inst_exp_width : 0] inst_a1;
  reg  [inst_sig_width+inst_exp_width : 0] inst_b1;
  reg  [inst_sig_width+inst_exp_width : 0] inst_c1;
  reg  [2 : 0] inst_rnd1                          ;
  wire [inst_sig_width+inst_exp_width : 0] z_inst1;
  wire [7 : 0] status_inst1                       ;

  reg  [inst_sig_width+inst_exp_width : 0] inst_a2;
  reg  [inst_sig_width+inst_exp_width : 0] inst_b2;
  reg  [inst_sig_width+inst_exp_width : 0] inst_c2;
  reg  [2 : 0] inst_rnd2                          ;
  wire [inst_sig_width+inst_exp_width : 0] z_inst2;
  wire [7 : 0] status_inst2                       ;

  reg  [inst_sig_width+inst_exp_width : 0] inst_a3;
  reg  [inst_sig_width+inst_exp_width : 0] inst_b3;
  reg  [2 : 0] inst_rnd3                          ;
  wire [inst_sig_width+inst_exp_width : 0] z_inst3;
  wire [7 : 0] status_inst3                       ;

  reg  [inst_sig_width+inst_exp_width : 0] inst_a4;
  reg  [inst_sig_width+inst_exp_width : 0] inst_b4;
  reg  [2 : 0] inst_rnd4                          ;
  wire [inst_sig_width+inst_exp_width : 0] z_inst4;
  wire [7 : 0] status_inst4                       ;
  assign  q_state_output_sram_write_data = {z_inst4,z_inst3};
  //assign  scratchpad_sram_write_data     = {z_inst4,z_inst3};
  
  // This is test stub for passing input/outputs to a DP_fp_mac, there many
  // more DW macros that you can choose to use
  DW_fp_mac_inst FP_MAC1( 
    .inst_a(inst_a1),
    .inst_b(inst_b1),
    .inst_c(inst_c1),
    .inst_rnd(inst_rnd1),
    .z_inst(z_inst1),
    .status_inst(status_inst1)
  );

  DW_fp_mac_inst FP_MAC2( 
    .inst_a(inst_a2),
    .inst_b(inst_b2),

    .inst_c(inst_c2),
    .inst_rnd(inst_rnd2),
    .z_inst(z_inst2),
    .status_inst(status_inst2)
  );

  DW_fp_mac_inst FP_MAC3( 
    .inst_a(inst_a3),
    .inst_b(inst_b3),
    .inst_c(z_inst2),
    .inst_rnd(inst_rnd3),
    .z_inst(z_inst3),
    .status_inst(status_inst3)
  );

  DW_fp_mac_inst FP_MAC4( 
    .inst_a(inst_a4),
    .inst_b(inst_b4),
    .inst_c(z_inst1),
    .inst_rnd(inst_rnd4),
    .z_inst(z_inst4),
    .status_inst(status_inst4)
  );

//endmodule

// DATA FLOW CODE

//Write Address to Q STATE OUTPUT SRAM
always @(posedge clk)
begin
  case(wo_addr_sel)
    2'b00:    q_state_output_sram_write_address_r <=                                                  `Q_STATE_OUTPUT_SRAM_ADDRESS_UPPER_BOUND'b0;
    2'b01:    q_state_output_sram_write_address_r <=    q_state_output_sram_write_address_r + `Q_STATE_OUTPUT_SRAM_ADDRESS_UPPER_BOUND'b1;
    2'b10:    q_state_output_sram_write_address_r <=                                                  q_state_output_sram_write_address_r;
    default:  q_state_output_sram_write_address_r <=                                                  q_state_output_sram_write_address_r;
endcase
end

// Write & Read array address generator
always @(posedge clk ) begin
  case (r_ary_sel)
    2'b00: r_arry_add <= 64'b0;
    2'b01: r_arry_add <= r_arry_add + 64'b1;
    2'b10: r_arry_add <= r_arry_add;
    default: r_arry_add <= r_arry_add;
  endcase
end

always @(posedge clk ) begin
  case (w_arry_sel)
    2'b00: w_arry_add <= 64'b0;
    2'b01: w_arry_add <= w_arry_add + 64'b1;
    2'b10: w_arry_add <= w_arry_add;
    default: w_arry_add <= w_arry_add;
  endcase
end


// module to write to the arrays
always @(posedge clk ) begin
  mem_buf1[w_arry_add] = mem_buf1[w_arry_add];
  if(w_arry_enable == 2'b01) mem_buf1[w_arry_add] = {z_inst4,z_inst3};
  else if(w_arry_enable == 2'b10) begin
    mem_buf[0]  <=  mem_buf1[0];
    mem_buf[1]  <=  mem_buf1[1];
    mem_buf[2]  <=  mem_buf1[2];
    mem_buf[3]  <=  mem_buf1[3];
    mem_buf[4]  <=  mem_buf1[4];
    mem_buf[5]  <=  mem_buf1[5];
    mem_buf[6]  <=  mem_buf1[6];
    mem_buf[7]  <=  mem_buf1[7];
    mem_buf[8]  <=  mem_buf1[8];
    mem_buf[9]  <=  mem_buf1[9];
    mem_buf[10] <= mem_buf1[10];
    mem_buf[11] <= mem_buf1[11];
    mem_buf[12] <= mem_buf1[12];
    mem_buf[13] <= mem_buf1[13];
    mem_buf[14] <= mem_buf1[14];
    mem_buf[15] <= mem_buf1[15];
  end
  else mem_buf1[w_arry_add] = mem_buf1[w_arry_add];
end


// Coloumn & Row counter stabilizer
// for dc_a
always @(posedge clk)
begin
  case (dc_sel)
    2'b00:   dc_a <= 1<<q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2]   ;
    2'b01:   dc_a <= 64'b0   ;
    2'b10:   dc_a <= dc_a                                                                                                             ;
    default: dc_a <= dc_a                                                                                                             ;
  endcase  
end

// Column Counter 
//
//
//
// TODO I think the two power module is repeating twice, please check on that.
//
//
//
//
always @(posedge clk ) begin
  case (col_sel)
    2'b00:    col_counter <= 1<<q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)] ;
    2'b01:    col_counter <= dc_a;
    2'b10:    col_counter <= col_counter - 64'b1;
    default:  col_counter <= col_counter                          ;
  endcase
end

// Row Counter
always @(posedge clk ) begin
  case (row_sel)
    2'b00:    row_counter <= 1<<q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)] ;
    2'b01:	  row_counter <= dc_a;
	2'b10:    row_counter <= row_counter - 1; //`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2'b1       
    2'b11:    row_counter <= row_counter; 
    default:  row_counter <= row_counter                                                                                                      ;
  endcase
end

// Matrix counter
always @(posedge clk) begin
  case (mat_Sel)
	2'b00: matrix_counter <= q_state_input_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0];
	2'b01: matrix_counter <= matrix_counter - 1;
	default: matrix_counter <= matrix_counter;
  endcase
end

// Q State Input SRAM address generation
always @(posedge clk ) begin
  case (qs_sel)
    2'b00:   q_state_input_sram_read_address_r <= `Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND'b0                                    ;
    2'b01:   q_state_input_sram_read_address_r <= `Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND'b1                                    ;
    2'b10:   q_state_input_sram_read_address_r <= q_state_input_sram_read_address_r + `Q_STATE_INPUT_SRAM_ADDRESS_UPPER_BOUND'b1;
    default: q_state_input_sram_read_address_r <= q_state_input_sram_read_address_r                                             ;
  endcase
  
end

// Q Gate SRAM address generation
always @(posedge clk ) begin
  case (qg_add_sel)
    2'b00:   q_gates_sram_read_address_r <= `Q_GATES_SRAM_ADDRESS_UPPER_BOUND'b0                                  ;
    2'b01:   q_gates_sram_read_address_r <= q_gates_sram_read_address_r + `Q_GATES_SRAM_ADDRESS_UPPER_BOUND'b1    ; 
    default: q_gates_sram_read_address_r <= q_gates_sram_read_address_r                                           ;
  endcase
  
end


// accumulator Module: TODO:

always @(posedge clk ) begin
  case (mac_sel)
    1'b0: begin 
      inst_c1 <= 64'b0;
      inst_c2 <= 64'b0;
    end
    1'b1: begin
      inst_c1 <= z_inst4;
      inst_c2 <= z_inst3;
    end
    default: begin
      inst_c1 <= 64'b0;
      inst_c2 <= 64'b0;
    end
  endcase  
end

always @(posedge clk ) begin
  case (mac_i_sel)
    2'b00: begin
      inst_a1   <=   64'b0;
      inst_b1   <=   64'b0;

      inst_a2   <=   64'b0;
      inst_b3   <=   64'b0;

      inst_a3   <=   64'b0;
      inst_b2   <=   64'b0;

      inst_a4   <=   64'b0;
      inst_b4   <=   64'b0;
    end
    2'b01: begin
      inst_a1   <=    q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2];
      inst_b1   <=    q_gates_sram_read_data[`Q_GATES_SRAM_DATA_UPPER_BOUND-1:`Q_GATES_SRAM_DATA_UPPER_BOUND/2]                  ;

      inst_a2   <=    q_state_input_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0]                                 ;
      inst_b2   <=    q_gates_sram_read_data[`Q_GATES_SRAM_DATA_UPPER_BOUND-1:`Q_GATES_SRAM_DATA_UPPER_BOUND/2]                  ;

      inst_a3   <=    q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2];
      inst_b3   <=    q_gates_sram_read_data[(`Q_GATES_SRAM_DATA_UPPER_BOUND/2)-1:0]                                             ;

      inst_a4   <=    {~q_state_input_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1],q_state_input_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-2:0]};
      inst_b4   <=    q_gates_sram_read_data[(`Q_GATES_SRAM_DATA_UPPER_BOUND/2)-1:0]                                             ;
    end
    2'b10: begin
      //inst_a1   <=    scratchpad_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2]           ;
      inst_b1   <=    mem_buf[r_arry_add][`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2]                 ;
      inst_a1   <=    q_gates_sram_read_data[`Q_GATES_SRAM_DATA_UPPER_BOUND-1:`Q_GATES_SRAM_DATA_UPPER_BOUND/2]                          ;

      //inst_a2   <=    scratchpad_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0];
      inst_a2   <=    q_gates_sram_read_data[`Q_GATES_SRAM_DATA_UPPER_BOUND-1:`Q_GATES_SRAM_DATA_UPPER_BOUND/2]                          ;
      inst_b2   <=    mem_buf[r_arry_add][(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1:0];

      //inst_a3   <=    scratchpad_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2]           ;
      inst_a3   <=    q_gates_sram_read_data[(`Q_GATES_SRAM_DATA_UPPER_BOUND/2)-1:0]                                                     ;
      inst_b3   <=    mem_buf[r_arry_add][`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2];

      //inst_a4   <=    {~scratchpad_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1],scratchpad_sram_read_data[(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-2:0]};
      inst_a4   <=     q_gates_sram_read_data[(`Q_GATES_SRAM_DATA_UPPER_BOUND/2)-1:0]                                             ;
      inst_b4   <=    {~mem_buf[r_arry_add][(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-1],mem_buf[r_arry_add][(`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2)-2:0]};
    end
    default: begin
      inst_a1   <=   64'b0;
      inst_b1   <=   64'b0;

      inst_a2   <=   64'b0;
      inst_b3   <=   64'b0;

      inst_a3   <=   64'b0;
      inst_b2   <=   64'b0;

      inst_a4   <=   64'b0;
      inst_b4   <=   64'b0;
    end
  endcase
end




// Control Flow

parameter[5:0] 
s0 =  6'b000000,
s1 =  6'b000001,
s2 =  6'b000010,
s3 =  6'b000011,
s4 =  6'b000100,
s5 =  6'b000101,
s6 =  6'b000110,
s7 =  6'b000111,
s8 =  6'b001000,
s9 =  6'b001001,
s10 = 6'b001010,
s11 = 6'b001011,
s12 = 6'b001100,
s13 = 6'b001101,
s14 = 6'b001110,
s15 = 6'b001111,
s16 = 6'b010000,
s17 = 6'b010001,
s18 = 6'b010010,
s19 = 6'b010011,
s20 = 6'b010100,
s21 = 6'b010101,
s22 = 6'b010110,
s23 = 6'b010111,
s24 = 6'b011000,
s25 = 6'b011001,
s26 = 6'b011010,
s27 = 6'b011011,
s28 = 6'b011100,
s29 = 6'b011101,
s30 = 6'b011110,
s31 = 6'b011111,
s32 = 6'b100000,
s33 = 6'b100001,
s34 = 6'b100010,
s35 = 6'b100011,
s36 = 6'b100100,
s37 = 6'b100101,
s38 = 6'b100110,
s39 = 6'b100111,
s40 = 6'b101000,
s41 = 6'b101001,
s42 = 6'b101010,
s43 = 6'b101011,
s44 = 6'b101100,
s45 = 6'b101101,
s46 = 6'b101110,
s47 = 6'b101111;


always @(posedge clk or negedge reset_n)
	begin
		if(!reset_n) present <= s0;
		else present <= next;
	end

always @(*) 
begin
      qs_sel          =  2'b00;
      qg_add_sel      =  2'b00;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b0;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b00;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b00;
      col_sel         =  2'b00;
      mat_Sel         =  2'b00;
      wo_addr_sel     =  2'b00;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b00;
      dut_ready_r  	  =  1'b0;
      inst_rnd1 = 3'b000;
      inst_rnd2 = 3'b000;
      inst_rnd3 = 3'b000;
      inst_rnd4 = 3'b000;
  casex (present)
    s0: begin
      
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b0;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
  
	    dut_ready_r  	  =  1'b1;


      inst_rnd1 = 3'b000;
      inst_rnd2 = 3'b000;
      inst_rnd3 = 3'b000;
      inst_rnd4 = 3'b000;

	  if(dut_valid == 1) next = s1;
	  else next = s0;

    end
    s1: begin

      dut_ready_r = 1'b0;
      
      qs_sel          =  2'b00;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s2;
    end
    s2: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b00;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s3;

    end
	s3: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b00;
      col_sel         =  2'b00;
      mat_Sel         =  2'b00;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b00;

      if(q_state_input_sram_read_data[`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND-1:`Q_STATE_INPUT_SRAM_DATA_UPPER_BOUND/2] == 1) next = s29;
	    else next = s4;
      // add for q == 1

    end
	s4: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      //if(dc_a == 2) next = s0;
	    next = s5;
    end
 	s5: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      if(col_counter != 3) next = s5;
      else if(row_counter != 1) next = s6;
      else next = s10;
    end
 	s6: begin
      
      qs_sel          =  2'b01;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      
	    if(row_counter == dc_a) next = s7;
      else next = s9;
    end
 s7: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b00;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

	    next = s8;
    end
 s8: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s5;
    end
 s9: begin
      
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s8;
    end
 s10: begin
      
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s11;
    end
 s11: begin
      
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

	    next = s12;
    end
 s12: begin
      
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
	    next = s13;
    end
s13: begin
      
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01; //10
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b10;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

	    next = s15; // change back to 14
end
s14: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s15;
end
s15: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b01;
      col_sel         =  2'b01;
      mat_Sel         =  2'b01;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      next = s16;
end
s16: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      next = s17;
end
s17: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      if(col_counter != 3) next = s17; // added line
      else if(row_counter == 1) next = s22;
      else next = s18;
end
s18: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      // Changes made
      if(matrix_counter == 1 && row_counter == dc_a) next = s24;
      else if(matrix_counter == 1 && row_counter != dc_a) next = s26;
      else if(matrix_counter != 1 && row_counter == dc_a) next = s19;
      else next = s21;
end
s19: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b00;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s20;
end
s20: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s17;
end
s21: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      next = s20;
end
s22: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
    
      if(matrix_counter != 1) next = s23;  
      else next = s27;
end
s23: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      next = s12;
end
s24: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b00;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
      
      next = s25;
end
s25: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s17;
end
s26: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b01;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;
     
      next = s25;
end
s27: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b01;
      wo_addr_sel     =  2'b01;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s28;
end
s28: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s0;
end
s29: begin
      qs_sel          =  2'b01;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

        if(matrix_counter == 1) next = s44;
        else next = s30;
end
s30: begin
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b00;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s31;
end
s31: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s32;
end
s32: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s33;
end
s33: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s34;
end
s34: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b10;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s35;
end
s35: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b01;
      col_sel         =  2'b01;
      mat_Sel         =  2'b01;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s36;
end
s36: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      if(matrix_counter == 1)next = s40;
      else next = s37;
end
s37: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b00;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s38;
end
s38: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b01;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s39;
end
s39: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b01;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s33;
end
s40: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b00;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b01;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b00;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s41;
end
s41: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b01;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s42;
end
s42: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b10;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b01;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s43;
end
s43: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s0;
end
s44: begin
      qs_sel          =  2'b10;
      qg_add_sel      =  2'b01;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b00;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s45;
end
s45: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b10;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s46;
end
s46: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b1;
      mac_i_sel       =  2'b01;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b10;
      col_sel         =  2'b01;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b01;
      wo_enable_r     =  1'b0; 
      dc_sel          =  2'b10;

      next = s47;
end
s47: begin
      qs_sel          =  2'b11;
      qg_add_sel      =  2'b10;
      mac_sel         =  1'b0;
      mac_i_sel       =  2'b00;
      r_ary_sel       =  2'b10;
      w_arry_sel      =  2'b10;
      w_arry_enable   =  2'b00;
      row_sel         =  2'b11;
      col_sel         =  2'b11;
      mat_Sel         =  2'b10;
      wo_addr_sel     =  2'b10;
      wo_enable_r     =  1'b1; 
      dc_sel          =  2'b10;

      next = s0;
end
    default: next = s0; 
  endcase
  
end

assign dut_ready = dut_ready_r;


endmodule

module DW_fp_mac_inst #(
  parameter inst_sig_width = 52,
  parameter inst_exp_width = 11,
  parameter inst_ieee_compliance = 1   // These need to be fixed to decrease error 3 (had 1)
) ( 
  input wire [inst_sig_width+inst_exp_width : 0] inst_a,
  input wire [inst_sig_width+inst_exp_width : 0] inst_b,
  input wire [inst_sig_width+inst_exp_width : 0] inst_c,
  input wire [2 : 0] inst_rnd,
  output wire [inst_sig_width+inst_exp_width : 0] z_inst,
  output wire [7 : 0] status_inst
);

// Instance of DW_fp_mac
  DW_fp_mac #(inst_sig_width, inst_exp_width, inst_ieee_compliance) U1 (
    .a(inst_a),
    .b(inst_b),
    .c(inst_c),
    .rnd(inst_rnd),
    .z(z_inst),
    .status(status_inst) 
  );

endmodule

