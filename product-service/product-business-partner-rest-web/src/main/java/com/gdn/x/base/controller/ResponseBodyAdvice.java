package com.gdn.x.base.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;

import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;

@ControllerAdvice(basePackages = {"com.gdn"})
public class ResponseBodyAdvice
    implements org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice<Object> {

  private static final Logger LOG = LoggerFactory.getLogger(GlobalControllerAdvice.class);

  @Override
  public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) {

    boolean exist = returnType.getMethod().isAnnotationPresent(AuditLog.class);
    return exist;
  }

  @Override
  public Object beforeBodyWrite(Object body, MethodParameter returnType, MediaType selectedContentType,
      Class<? extends HttpMessageConverter<?>> selectedConverterType, ServerHttpRequest request,
      ServerHttpResponse response) {
    LOG.info(LoggerStandard.appendAuditLogTemplate(MDC.get(LoggerParam.GENERIC_LOGGER.getParam())));
    return body;
  }
}

