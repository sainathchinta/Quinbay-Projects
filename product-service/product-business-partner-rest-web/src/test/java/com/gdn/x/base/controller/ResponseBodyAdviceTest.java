package com.gdn.x.base.controller;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.core.MethodParameter;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;

import com.gdn.mta.product.controller.ProductController;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;

public class ResponseBodyAdviceTest {

  @InjectMocks
  private ResponseBodyAdvice advice;

  @BeforeEach
  public void before() {
    MockitoAnnotations.initMocks(this);
    this.converterType = StringHttpMessageConverter.class;
  }

  private Class<? extends HttpMessageConverter<?>> converterType;
  private MethodParameter returnType;

  @Test
  public void checkWithTrueAnnotation() throws NoSuchMethodException, SecurityException {
    Class[] cArg = new Class[6];
    cArg[0] = String.class;
    cArg[1] = String.class;
    cArg[2] = String.class;
    cArg[3] = String.class;
    cArg[4] = String.class;
    cArg[5] = ActivateImageRequest.class;
    returnType = new MethodParameter(ProductController.class.getMethod("updateProductImageName", cArg), -1);

    boolean result = advice.supports(returnType, converterType);

    Assertions.assertEquals(true, result);
  }

  @Test
  public void checkWithFalseAnnotation() throws NoSuchMethodException, SecurityException {
    Class[] cArg = new Class[6];
    cArg[0] = String.class;
    cArg[1] = String.class;
    cArg[2] = String.class;
    cArg[3] = String.class;
    cArg[4] = String.class;
    cArg[5] = String.class;
    returnType = new MethodParameter(ProductController.class.getMethod("getProduct", cArg), -1);

    boolean result = advice.supports(returnType, converterType);
    Assertions.assertFalse(result);
  }

  @Test
  public void beforeBodyWrite() {
    Object body = new Object();
    Object data = advice.beforeBodyWrite(body, null, null, null, null, null);

    Assertions.assertEquals(body, data);
  }
}

