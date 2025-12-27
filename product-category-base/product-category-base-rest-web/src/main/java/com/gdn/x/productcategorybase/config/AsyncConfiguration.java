package com.gdn.x.productcategorybase.config;

import java.util.concurrent.Executor;

import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
@EnableAsync
public class AsyncConfiguration implements AsyncConfigurer {

  @Value("${application.pcb_default_async.core_pool_size}")
  private int corePoolSize;

  @Value("${application.pcb_default_async.max_pool_size}")
  private int maxPoolSize;

  @Value("${application.pcb_default_async.queue_capacity}")
  private int queueCapacity;

  @Value("${application.pcb_default_async.wait_for_task_complete}")
  private boolean waitForTasksToCompleteOnShutDown;

  @Value("${application.pcb_default_async.thread_name_prefix}")
  private String threadNamePrefix;

  @Override
  @Bean(name = "domainEventPublisherTaskExecutor")
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(this.corePoolSize);
    executor.setMaxPoolSize(this.maxPoolSize);
    executor.setQueueCapacity(this.queueCapacity);
    executor.setWaitForTasksToCompleteOnShutdown(this.waitForTasksToCompleteOnShutDown);
    executor.setThreadNamePrefix(this.threadNamePrefix);
    executor.initialize();
    return executor;
  }

  @Override
  public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
    return new AsyncExceptionHandler();
  }
}
