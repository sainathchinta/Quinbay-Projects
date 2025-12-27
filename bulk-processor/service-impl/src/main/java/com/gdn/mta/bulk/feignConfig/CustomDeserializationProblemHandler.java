package com.gdn.mta.bulk.feignConfig;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.deser.DeserializationProblemHandler;

public class CustomDeserializationProblemHandler extends DeserializationProblemHandler {
  private static final Logger LOG = LoggerFactory.getLogger(CustomDeserializationProblemHandler.class);

  @Override
  public boolean handleUnknownProperty(DeserializationContext ctxt, JsonParser jp, JsonDeserializer<?> deserializer,
      Object beanOrClass, String propertyName) throws IOException {
    CustomDeserializationProblemHandler.LOG.warn("unknown field : {}", propertyName);
    return true;
  }
}