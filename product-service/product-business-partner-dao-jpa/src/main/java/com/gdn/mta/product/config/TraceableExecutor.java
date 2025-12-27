package com.gdn.mta.product.config;

import java.util.concurrent.Executor;

import brave.Tracer;
import brave.propagation.CurrentTraceContext;
import brave.propagation.TraceContext;

class TraceableExecutor implements Executor {
  private final Executor delegate;
  private final Tracer tracer;
  private final CurrentTraceContext currentTraceContext;

  public TraceableExecutor(Executor delegate, Tracer tracer, CurrentTraceContext currentTraceContext) {
    this.delegate = delegate;
    this.tracer = tracer;
    this.currentTraceContext = currentTraceContext;
  }

  @Override
  public void execute(Runnable command) {
    if (tracer.currentSpan() == null) {
      delegate.execute(command);
      return;
    }

    TraceContext context = tracer.currentSpan().context();
    delegate.execute(() -> {
      try (CurrentTraceContext.Scope scope = currentTraceContext.newScope(context)) {
        command.run();
      }
    });
  }
}
