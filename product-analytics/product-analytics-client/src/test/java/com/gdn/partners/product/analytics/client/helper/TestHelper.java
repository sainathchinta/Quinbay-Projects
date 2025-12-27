package com.gdn.partners.product.analytics.client.helper;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * @author Pradeep Reddy
 */
public abstract class TestHelper {

  @Autowired
  protected ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() {
  }

  public String toJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (JsonProcessingException e) {
      return StringUtils.EMPTY;
    }
  }

}
