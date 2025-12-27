package com.gdn.micro.graphics.config;

import java.util.concurrent.ThreadPoolExecutor;

import org.gm4java.engine.support.GMConnectionPoolConfig;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.core.task.support.ExecutorServiceAdapter;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import com.gdn.common.base.mapper.impl.OrikaMapper;
import com.gdn.micro.graphics.web.model.MandatoryParameterDefaultProperties;

import brave.Tracer;
import ma.glasnost.orika.impl.DefaultMapperFactory;

@Configuration
public class BeanConfiguration {

  @Value("${gm.pool.maxactive}")
  private int gmPoolMaxActive;

  @Value("${gm.path}")
  private String gmPath;

  @Value("${thread.pool.size}")
  private int corePoolSize;

  @Value("${thread.max.pool.size}")
  private int maxPoolSize;

  @Value("${thread.queue.size}")
  private int queueSize;


  @Autowired
  private Tracer tracer;

  @Autowired
  private TracerHelper tracerHelper;

  @Autowired
  private MandatoryParameterDefaultProperties mandatoryParameterDefaultProperties;

  @Bean
  public OrikaMapper orikaMapper() {
    return new OrikaMapper(new DefaultMapperFactory.Builder().build());
  }

  @Bean
  public MandatoryParameterInterceptor mandatoryParameterInterceptor() {
    return new MandatoryParameterInterceptor(mandatoryParameterHelper());
  }

  @Bean
  public MandatoryParameterHelper mandatoryParameterHelper() {
    return new MandatoryParameterHelper("system", tracer, tracerHelper, mandatoryParameterDefaultProperties);
  }

  @Bean
  public GMConnectionPoolConfig gmPool() {
    GMConnectionPoolConfig gmConnectionPoolConfig = new GMConnectionPoolConfig();
    gmConnectionPoolConfig.setMaxActive(gmPoolMaxActive);
    gmConnectionPoolConfig.setGMPath(gmPath);
    return gmConnectionPoolConfig;
  }

  @Bean
  public TaskExecutor taskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(corePoolSize);
    executor.setMaxPoolSize(maxPoolSize);
    executor.setQueueCapacity(queueSize);
    executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    return executor;
  }

  @Bean
  public ExecutorServiceAdapter executorService() {
    return new ExecutorServiceAdapter(taskExecutor());
  }
}