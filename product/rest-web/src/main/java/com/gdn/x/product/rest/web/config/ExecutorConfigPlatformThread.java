package com.gdn.x.product.rest.web.config;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.core.task.TaskExecutor;
import org.springframework.core.task.support.ExecutorServiceAdapter;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
@EnableAutoConfiguration
@EnableAspectJAutoProxy
@ConditionalOnProperty(name = "spring.threads.virtual.enabled", havingValue = "false")
public class ExecutorConfigPlatformThread {
  @Value("${domainEvent.publisher.executor.pool.size}")
  private int publisherExecutorPoolSize;

  @Value("${domainEvent.publisher.executor.keep.alive}")
  private int publisherExecutorKeepAlive;

  @Value("${domainEvent.publisher.executor.queue.capacity}")
  private int publisherExecutorQueueCapacity;

  @Value("${domainEvent.publisher.executor.backoffDuration}")
  private int publisherExecutorBackOffDuration;

  @Value("${thread.pool.size}")
  private int poolSize;

  @Bean(value = "domainEventPublisherTaskExecutor")
  public ThreadPoolTaskExecutor domainEventPublisherTaskExecutor() {
    ThreadPoolTaskExecutor scheduledExecutorTask = new ThreadPoolTaskExecutor();
    scheduledExecutorTask.setCorePoolSize(publisherExecutorPoolSize);
    scheduledExecutorTask.setKeepAliveSeconds(publisherExecutorKeepAlive);
    scheduledExecutorTask.setQueueCapacity(publisherExecutorQueueCapacity);
    scheduledExecutorTask.setMaxPoolSize(publisherExecutorPoolSize);
    return scheduledExecutorTask;
  }

  @Bean(value = "domainEventPublisherExecutor", autowireCandidate = false)
  ExecutorService domainEventPublisherExecutor() {
    return new ExecutorServiceAdapter(domainEventPublisherTaskExecutor());
  }

  @Bean(name = "taskExecutor")
  public TaskExecutor taskExecutor() {
    ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
    taskExecutor.setCorePoolSize(poolSize);
    taskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    return taskExecutor;
  }

  @Bean(name = "executorService")
  public ExecutorService executorService() {
    return new ExecutorServiceAdapter(taskExecutor());
  }
}
