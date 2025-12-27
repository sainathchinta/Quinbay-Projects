package com.gdn.x.productcategorybase.config;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.support.ExecutorServiceAdapter;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
public class ImageUploadExecutorPoolConfiguration {

  @Value("${image.upload.task.executor.core.pool.size}")
  private int corePoolSize;

  @Value("${image.upload.task.executor.max.pool.size}")
  private int maxPoolSize;

  @Value("${image.upload.task.executor.queue.size}")
  private int queueSize;

  @Bean("imageUploadTaskExecutor")
  public ThreadPoolTaskExecutor imageUploadTaskExecutor() {
    ThreadPoolTaskExecutor threadPoolTaskExecutor = new ThreadPoolTaskExecutor();
    threadPoolTaskExecutor.setCorePoolSize(corePoolSize);
    threadPoolTaskExecutor.setMaxPoolSize(maxPoolSize);
    threadPoolTaskExecutor.setQueueCapacity(queueSize);
    threadPoolTaskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    return threadPoolTaskExecutor;
  }

  @Bean("imageUploadExecutorService")
  public ExecutorService imageUploadExecutorService() {
    return new ExecutorServiceAdapter(imageUploadTaskExecutor());
  }

}
