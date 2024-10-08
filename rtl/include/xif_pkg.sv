// Copyright 2024 EPFL
// Solderpad Hardware License, Version 2.1, see LICENSE.md for details.
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1
//
// Author: Saverio Nasturzio
package xif_pkg;

  // ------------------------------
  // cv-x-if structs and parameters
  // ------------------------------

  parameter int X_NUM_RS = 3;
  parameter int X_ID_WIDTH = 4;
  parameter int X_MEM_WIDTH = 32;
  parameter int X_RFR_WIDTH = 32;
  parameter int X_RFW_WIDTH = 32;
  parameter logic [31:0] X_MISA = '0;
  parameter logic [1:0] X_ECS_XS = '0;

  localparam int XLEN = 32;
  localparam int FLEN = 32;

  // Offloaded to dispatcher
  typedef struct packed {
    logic [X_NUM_RS-1:0][X_RFR_WIDTH-1:0] rs;
    logic [X_NUM_RS-1:0]                  rs_valid;
    logic [31:0]                          instr_data;
    logic [X_ID_WIDTH-1:0]                id;
    logic [1:0]                           mode;
  } offloaded_data_t;


  typedef struct packed {
    logic [15:0]           instr;  // Offloaded compressed instruction
    logic [1:0]            mode;   // Privilege level
    logic [X_ID_WIDTH-1:0] id;     // Identification number of the offloaded compressed instruction
  } x_compressed_req_t;

  typedef struct packed {
    logic [31:0] instr;  // Uncompressed instruction
    logic accept;  // Is the offloaded compressed instruction (id) accepted by the coprocessor?
  } x_compressed_resp_t;

  typedef struct packed {
    logic [31:0] instr;  // Offloaded instruction
    logic [1:0] mode;  // Privilege level
    logic [X_ID_WIDTH-1:0] id;  // Identification of the offloaded instruction
    logic [X_NUM_RS  -1:0][X_RFR_WIDTH-1:0] rs;        // Register file source operands for the offloaded instruction
    logic [X_NUM_RS  -1:0] rs_valid;  // Validity of the register file source operand(s)
    logic [5:0] ecs;  // Extension Context Status ({mstatus.xs, mstatus.fs, mstatus.vs})
    logic ecs_valid;  // Validity of the Extension Context Status
  } x_issue_req_t;

  typedef struct packed {
    logic accept;  // Is the offloaded instruction (id) accepted by the coprocessor?
    logic writeback;  // Will the coprocessor perform a writeback in the core to rd?
    logic dualwrite;  // Will the coprocessor perform a dual writeback in the core to rd and rd+1?
    logic [2:0] dualread;   // Will the coprocessor require dual reads from rs1\rs2\rs3 and rs1+1\rs2+1\rs3+1?
    logic loadstore;  // Is the offloaded instruction a load/store instruction?
    logic ecswrite;  // Will the coprocessor write the Extension Context Status in mstatus?
    logic       exc;        // Can the offloaded instruction possibly cause a synchronous exception in the coprocessor itself?
  } x_issue_resp_t;

  typedef struct packed {
    logic [X_ID_WIDTH-1:0] id;           // Identification of the offloaded instruction
    logic                  commit_kill;  // Shall an offloaded instruction be killed?
  } x_commit_t;

  typedef struct packed {
    logic [X_ID_WIDTH   -1:0] id;  // Identification of the offloaded instruction
    logic [31:0] addr;  // Virtual address of the memory transaction
    logic [1:0] mode;  // Privilege level
    logic we;  // Write enable of the memory transaction
    logic [2:0] size;  // Size of the memory transaction
    logic [X_MEM_WIDTH/8-1:0] be;  // Byte enables for memory transaction
    logic [1:0] attr;  // Memory transaction attributes
    logic [X_MEM_WIDTH  -1:0] wdata;  // Write data of a store memory transaction
    logic last;  // Is this the last memory transaction for the offloaded instruction?
    logic spec;  // Is the memory transaction speculative?
  } x_mem_req_t;

  typedef struct packed {
    logic exc;  // Did the memory request cause a synchronous exception?
    logic [5:0] exccode;  // Exception code
    logic dbg;  // Did the memory request cause a debug trigger match with ``mcontrol.timing`` = 0?
  } x_mem_resp_t;

  typedef struct packed {
    logic [X_ID_WIDTH -1:0] id;  // Identification of the offloaded instruction
    logic [X_MEM_WIDTH-1:0] rdata;  // Read data of a read memory transaction
    logic err;  // Did the instruction cause a bus error?
    logic dbg;  // Did the read data cause a debug trigger match with ``mcontrol.timing`` = 0?
  } x_mem_result_t;

  typedef struct packed {
    logic [X_ID_WIDTH      -1:0] id;  // Identification of the offloaded instruction
    logic [X_RFW_WIDTH     -1:0] data;  // Register file write data value(s)
    logic [4:0] rd;  // Register file destination address(es)
    logic [X_RFW_WIDTH/XLEN-1:0] we;  // Register file write enable(s)
    logic [5:0] ecsdata;  // Write data value for {mstatus.xs, mstatus.fs, mstatus.vs}
    logic [2:0] ecswe;  // Write enables for {mstatus.xs, mstatus.fs, mstatus.vs}
    logic exc;  // Did the instruction cause a synchronous exception?
    logic [5:0] exccode;  // Exception code
    logic err;  // Did the instruction cause a bus error?
    logic dbg;  // Did the instruction cause a debug trigger match with ``mcontrol.timing`` = 0?
  } x_result_t;

endpackage
