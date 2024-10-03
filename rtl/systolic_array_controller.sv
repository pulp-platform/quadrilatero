// Copyright 2024 EPFL
// Solderpad Hardware License, Version 2.1, see LICENSE.md for details.
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Author: Saverio Nasturzio

module systolic_array_controller #(
    parameter N_SLOTS = 3,
    parameter DATA_WIDTH = 32
) (
    input logic clk_i,
    input logic rst_ni,

    output logic issue_queue_full_o,
    input logic dispatch_i,
    input matrix_cps_pkg::sa_instr_t dispatched_instr_i,

    // To Systolic Array
    input logic wl_ready_i,  // WL stage is ready for new instruction
    output logic start_o,  // WL will start executing new instruction
    output matrix_cps_pkg::sa_instr_t issued_instr_o  // issued instruction
);

  logic issue_queue_empty;

  logic pop_instr;
  assign pop_instr = wl_ready_i & !issue_queue_empty;

  assign start_o   = pop_instr;

  fifo_v3 #(
      .FALL_THROUGH(0),
      .DATA_WIDTH  (DATA_WIDTH),
      .DEPTH       (N_SLOTS),
      .dtype       (matrix_cps_pkg::sa_instr_t)
  ) issue_queue_inst (
      .clk_i,
      .rst_ni,

      .flush_i   (1'b0),
      .testmode_i(1'b0),

      .usage_o(  /* unused */),
      .full_o (issue_queue_full_o),
      .empty_o(issue_queue_empty),

      .data_i(dispatched_instr_i),  // data to push into the queue
      .push_i(dispatch_i),          // data is valid and can be pushed to the queue
      .data_o(issued_instr_o),      // output data
      .pop_i (pop_instr)            // pop head from queue
  );


endmodule
