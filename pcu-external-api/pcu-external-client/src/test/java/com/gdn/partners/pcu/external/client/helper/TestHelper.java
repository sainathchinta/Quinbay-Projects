package com.gdn.partners.pcu.external.client.helper;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.model.Constants;

/**
 * @author Pradeep Reddy
 */
public abstract class TestHelper {

  @Autowired
  protected ObjectMapper objectMapper;

  @Autowired
  protected MandatoryParameterHelper mandatoryParameterHelper;

  public static final String CLIENT_ID = "clientId";
  public static final String USERNAME = "username";
  public static final String REQUEST_ID = "requestId";
  public static final String CHANNEL_ID = "channelId";
  public static final String STORE_ID = "storeId";

  @BeforeEach
  public void setUp() {
    mandatoryParameterHelper.validateAndSet(Constants.STORE_ID, STORE_ID);
    mandatoryParameterHelper.validateAndSet(Constants.CHANNEL_ID, CHANNEL_ID);
    mandatoryParameterHelper.validateAndSet(Constants.CLIENT_ID, CLIENT_ID);
    mandatoryParameterHelper.validateAndSet(Constants.REQUEST_ID, REQUEST_ID);
    mandatoryParameterHelper.validateAndSet(Constants.USER_NAME, USERNAME);
  }

  public String toJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (JsonProcessingException e) {
      return StringUtils.EMPTY;
    }
  }

}
