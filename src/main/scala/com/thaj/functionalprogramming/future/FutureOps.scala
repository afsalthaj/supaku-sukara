package com.thaj.functionalprogramming.future

/**
 * Created by afsalthaj on 6/04/17.
 */
class FutureOps {

  // A Future is a placeholder for a value that may not exist
  // The value of the future is supplied concurrently and can subsequently be used.
  // It needs an execution context, similar to Executor, which is basically an object
  // that runs Runnable tasks in separate thread
  // You can convert an Execution Context to Executor or you can probably extend and Execution Context

  // The global execution context is an execution
  // ExecutionContext.global is an ExecutionContext backed by a ForkJoinPool.
  // Fork Join pool is an implementation of Executor, where it makes use of multiple processors.
  // It work on work-steeling algorithm, where worker threads that run out of things to do can steal tasks
  // from other threads that are still busy. The center of the fork/join framework is the ForkJoinPool class, an extension of the AbstractExecutorService class.
  // ForkJoinPool implements the core work-stealing algorithm and can execute ForkJoinTask processes.

}
