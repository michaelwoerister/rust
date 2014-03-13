# Copyright 2014 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

from __future__ import print_function
import lldb
import os
import sys
import threading
import re

# Set this to True for additional output
DEBUG_OUTPUT = False

def print_debug(s):
  '''Print something if DEBUG_OUTPUT is True'''
  global DEBUG_OUTPUT
  if DEBUG_OUTPUT:
    print("DEBUG: " + str(s))

# This callback is registered with every breakpoint and makes sure that the frame containing the
# breakpoint location is selected
def breakpoint_callback(frame, bp_loc, dict):
  '''Called whenever a breakpoint is hit'''
  print_debug("breakpoint hit!")

  # Select the frame and the thread containing it
  frame.thread.process.SetSelectedThread(frame.thread)
  frame.thread.SetSelectedFrame(frame.idx)

  # Returning True means that we actually want to stop at this breakpoint
  return True


# This is a list of breakpoints that are not registered with the breakpoint callback. The list is
# populated by the breakpoint listener and checked/emptied whenever a command has been executed
breakpoints = []

def execute_command(ci, cmd):
  '''Executes a single CLI command'''
  global breakpoints

  res = lldb.SBCommandReturnObject()
  print(cmd)
  ci.HandleCommand(cmd, res)

  if res.Succeeded():
      if res.HasResult():
          print(res.GetOutput(), end = '')

      # If the command introduced any breakpoints, make sure to register them with the breakpoint
      # callback
      while len(breakpoints) > 0:
        res.Clear()
        breakpoint_id = breakpoints.pop()
        print_debug("registering breakpoint callback, id = " + str(breakpoint_id))
        ci.HandleCommand('breakpoint command add -F breakpoint_callback ' + str(breakpoint_id), res)

        if res.Succeeded():
          print_debug("registering breakpoint callback, id = " + str(breakpoint_id))
        else:
          print_debug("Error while trying to register breakpoint callback")
  else:
      print(res.GetError())



def start_breakpoint_listener(target):
  '''Listens for breakpoints being added and adds new ones to the callback registration list'''
  listener = lldb.SBListener('breakpoint listener')

  def listen():
    event = lldb.SBEvent()
    try:
      while True:
        if listener.WaitForEvent(120, event):
          if lldb.SBBreakpoint.EventIsBreakpointEvent(event) and lldb.SBBreakpoint.GetBreakpointEventTypeFromEvent(event) == lldb.eBreakpointEventTypeAdded:
            global breakpoints
            breakpoint = lldb.SBBreakpoint.GetBreakpointFromEvent(event)
            print_debug("breakpoint added, id = " + str(breakpoint.id))
            breakpoints.append(breakpoint.id)
    except:
      print_debug("breakpoint listener shutting down")

  # Start the listener and let it run as a daemon
  listener_thread = threading.Thread(target = listen)
  listener_thread.daemon = True
  listener_thread.start()

  # Register the listener with the target
  target.GetBroadcaster().AddListener(listener, lldb.SBTarget.eBroadcastBitBreakpointChanged)


####################################################################################################
# ~main
####################################################################################################

if len(sys.argv) != 3:
  print("usage: python lldb_batchmode.py target-path script-path")
  sys.exit(1)

target_path = sys.argv[1]
script_path = sys.argv[2]


# Create a new debugger instance
debugger = lldb.SBDebugger.Create()

# When we step or continue, don't return from the function until the process
# stops. We do this by setting the async mode to false.
debugger.SetAsync(False)

# Create a target from a file and arch
print("Creating a target for '%s'" % target_path)
target = debugger.CreateTargetWithFileAndArch(target_path, lldb.LLDB_ARCH_DEFAULT)

if not target:
  print >> sys.stderr, "Could not create debugger target '%s'. Aborting.", target_path
  sys.exit(1)


# Register the breakpoint callback for every breakpoint
start_breakpoint_listener(target)

ci = debugger.GetCommandInterpreter()

try:
  script_file = open(script_path, 'r')

  for line in script_file:
    command = line.strip()
    if command != '':
      execute_command(ci, command)

except IOError as e:
  print("Could not read debugging script '%s'." % script_path, file = sys.stderr)
  print(e, file = sys.stderr)
  print("Aborting.", file = sys.stderr)
  sys.exit(1)
finally:
  script_file.close()

## Throws an exception?
# try:
#   lldb.SBDebugger.Terminate()
# except:
#   pass
