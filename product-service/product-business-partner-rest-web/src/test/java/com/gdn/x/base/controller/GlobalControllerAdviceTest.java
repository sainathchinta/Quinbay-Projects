package com.gdn.x.base.controller;

import jakarta.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;

public class GlobalControllerAdviceTest {

  @Mock
  private HttpServletResponse httpServletResponse;

  @Mock
  private Exception exception;

  @Mock
  private Throwable throwable;

  private ApplicationException applicationException;
  private ApplicationRuntimeException applicationRuntimeException;

  @InjectMocks
  private GlobalControllerAdvice globalControllerAdvice;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.applicationException = new ApplicationException(ErrorCategory.UNSPECIFIED);
    this.applicationRuntimeException = new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED);
  }

  @Test
  public void handleApplicationExceptionTest() throws Exception {
    this.globalControllerAdvice.handleApplicationException(this.applicationException,
        this.httpServletResponse);
    Assertions.assertNotNull(this.applicationException);
  }

  @Test
  public void handleApplicationRuntimeExceptionTest() throws Exception {
    this.globalControllerAdvice.handleApplicationRuntimeException(this.applicationRuntimeException,
        this.httpServletResponse);
    Assertions.assertNotNull(this.applicationRuntimeException);
  }

  @Test
  public void handleExceptionInternalTest() throws Exception {
    this.globalControllerAdvice.handleExceptionInternal(this.exception, null, null,
        HttpStatus.INTERNAL_SERVER_ERROR, null);
    Assertions.assertNotNull(this.exception);
  }

  @Test
  public void handleThrowableTest() {
    this.globalControllerAdvice.handleThrowable(this.throwable, this.httpServletResponse);
    Assertions.assertNotNull(this.throwable);
  }

}

