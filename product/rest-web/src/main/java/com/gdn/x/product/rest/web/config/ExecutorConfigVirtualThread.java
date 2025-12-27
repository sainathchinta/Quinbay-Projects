package com.gdn.x.product.rest.web.config;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Primary;
import org.springframework.core.task.TaskExecutor;
import org.springframework.core.task.support.ExecutorServiceAdapter;
import org.springframework.core.task.support.TaskExecutorAdapter;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
@EnableAutoConfiguration
@EnableAspectJAutoProxy
@ConditionalOnProperty(name = "spring.threads.virtual.enabled", havingValue = "true")
public class ExecutorConfigVirtualThread {

  @Value("${domainEvent.publisher.executor.pool.size}")
  private int publisherExecutorPoolSize;

  @Value("${domainEvent.publisher.executor.keep.alive}")
  private int publisherExecutorKeepAlive;

  @Value("${domainEvent.publisher.executor.queue.capacity}")
  private int publisherExecutorQueueCapacity;

  @Value("${thread.pool.size}")
  private int poolSize;

  @Bean(value = "domainEventPublisherTaskExecutor")
  @ConditionalOnProperty(name = "application.domain.event.publish.task.executor.virtual.thread.enabled", havingValue = "false")
  public TaskExecutor domainEventPublisherTaskExecutorPlatformThread() {
    ThreadPoolTaskExecutor scheduledExecutorTask = new ThreadPoolTaskExecutor();
    scheduledExecutorTask.setCorePoolSize(publisherExecutorPoolSize);
    scheduledExecutorTask.setKeepAliveSeconds(publisherExecutorKeepAlive);
    scheduledExecutorTask.setQueueCapacity(publisherExecutorQueueCapacity);
    scheduledExecutorTask.setMaxPoolSize(publisherExecutorPoolSize);
    return scheduledExecutorTask;
  }

  @Bean(value = "domainEventPublisherTaskExecutor")
  @ConditionalOnProperty(name = "application.domain.event.publish.task.executor.virtual.thread.enabled", havingValue = "true")
  public TaskExecutor domainEventPublisherTaskExecutorVirtualThread() {
    return new TaskExecutorAdapter(Executors.newVirtualThreadPerTaskExecutor());
  }

  @Bean(name = "executorService")
  @ConditionalOnProperty(name = "application.executor.service.virtual.thread.enabled", havingValue = "false")
  public ExecutorService executorServicePlatformThread() {
    ThreadPoolTaskExecutor taskExecutor = new ThreadPoolTaskExecutor();
    taskExecutor.setCorePoolSize(poolSize);
    taskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    return new ExecutorServiceAdapter(taskExecutor);
  }

  @Bean(name = "executorService")
  @ConditionalOnProperty(name = "application.executor.service.virtual.thread.enabled", havingValue = "true")
  public ExecutorService executorServiceVirtualThread() {
    return new ExecutorServiceAdapter(new TaskExecutorAdapter(Executors.newVirtualThreadPerTaskExecutor()));
  }

  @Bean
  @ConditionalOnProperty(name = "application.async.task.executor.virtual.thread.enabled", havingValue = "true")
  @Primary
  public TaskExecutor applicationTaskExecutor() {
    return new TaskExecutorAdapter(Executors.newVirtualThreadPerTaskExecutor());
  }
}
