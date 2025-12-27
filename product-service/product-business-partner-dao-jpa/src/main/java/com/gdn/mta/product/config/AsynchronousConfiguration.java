package com.gdn.mta.product.config;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

import brave.Tracer;
import brave.propagation.CurrentTraceContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
@EnableAsync
public class AsynchronousConfiguration implements AsyncConfigurer {

  private static final String ASYNC_EXECUTOR_THREAD_NAME_PREFIX = "AsyncThread-";

  @Autowired
  private Tracer tracer;

  @Autowired
  private CurrentTraceContext currentTraceContext;

  @Value("${async.executor.pool.size}")
  private int asyncExecutorPoolSize;

  @Value("${async.executor.max.pool.size}")
  private int asyncExecutorMaxPoolSize;

  @Value("${async.executor.queue.capacity}")
  private int asyncExecutorQueueCapacity;

  @Override
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(asyncExecutorPoolSize);
    executor.setMaxPoolSize(asyncExecutorMaxPoolSize);
    executor.setQueueCapacity(asyncExecutorQueueCapacity);
    executor.setThreadNamePrefix(ASYNC_EXECUTOR_THREAD_NAME_PREFIX);
    executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    executor.initialize();
    return new TraceableExecutor(executor, tracer, currentTraceContext);
  }
}

