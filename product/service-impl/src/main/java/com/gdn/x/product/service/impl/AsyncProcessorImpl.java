package com.gdn.x.product.service.impl;

import java.util.concurrent.Executor;
import java.util.concurrent.RejectedExecutionException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.service.executor.api.AsyncProcessor;

@Service
public class AsyncProcessorImpl implements AsyncProcessor {

  private static final Logger LOG = LoggerFactory.getLogger(AsyncProcessorImpl.class);

  @Autowired
  @Qualifier("domainEventPublisherTaskExecutor")
  private Executor publisherTaskExecutor;

  @Value("${domainEvent.publisher.executor.backoffDuration:3000}")
  private long backoffDuration;

  @Override
  public void submitWithBackoff(String commandDesc, Runnable command) {
    boolean submitted = false;
    while (!submitted) {
      try {
        publisherTaskExecutor.execute(command);
        submitted = true;
      } catch (RejectedExecutionException e) {
        LOG.debug("Thread pool and queue is full, backoff {} for {} ms",
            commandDesc,
            backoffDuration);
        try {
          Thread.sleep(backoffDuration);
        } catch (InterruptedException ie) {
        }
      }
    }
  }

}
