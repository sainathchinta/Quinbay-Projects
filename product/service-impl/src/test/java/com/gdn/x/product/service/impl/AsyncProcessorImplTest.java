package com.gdn.x.product.service.impl;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.concurrent.Executor;
import java.util.concurrent.RejectedExecutionException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

public class AsyncProcessorImplTest {

  private static final String COMMAND_DESC = "commandDesc";
  private static final Runnable COMMAND = new Runnable() {
    @Override
    public void run() {

    }
  };

  @InjectMocks
  private AsyncProcessorImpl asyncProcessorImpl;

  @Mock
  private Executor publisherTaskExecutor;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @Test
  public void submitWithBackoff_success() throws Exception {
    asyncProcessorImpl.submitWithBackoff(COMMAND_DESC,COMMAND);
    verify(publisherTaskExecutor).execute(COMMAND);
  }

  @Test
  public void submitWithBackoff_failed() throws Exception {
    doThrow(new RejectedExecutionException())
        .doNothing()
        .when(publisherTaskExecutor).execute(COMMAND);
    asyncProcessorImpl.submitWithBackoff(COMMAND_DESC,COMMAND);
    verify(publisherTaskExecutor,times(2)).execute(COMMAND);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(publisherTaskExecutor);
  }

}
