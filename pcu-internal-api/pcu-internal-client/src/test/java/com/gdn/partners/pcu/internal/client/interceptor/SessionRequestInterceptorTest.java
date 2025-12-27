package com.gdn.partners.pcu.internal.client.interceptor;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.partners.pcu.internal.client.constants.ClientParameter;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;
import feign.RequestTemplate;

public class SessionRequestInterceptorTest {

  private static final String CLIENT_ID = "clientId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String CHANNEL_ID = "channelId";
  private static final String STORE_ID = "storeId";

  private static final String REQUEST_ID_WITH_SPECIAL_CHARS = "request+id";
  private static final String USERNAME_WITH_SPECIAL_CHARS = "user+name";

  private static final String REQUEST_ID_ENCODED = "request%2Bid";
  private static final String USERNAME_ENCODED = "user%2Bname";

  @Autowired
  protected MockMvc mockMvc;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private ClientParameter clientParameter;

  @Mock
  private ClientParameterProperties clientParameterProperties;

  @InjectMocks
  private SessionRequestInterceptor sessionRequestInterceptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.clientParameterHelper);
  }

  @Test
  public void applyTest() {
    RequestTemplate template = new RequestTemplate();
    when(this.clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(this.clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    sessionRequestInterceptor.apply(template);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getUsername();
    Assertions.assertEquals(REQUEST_ID, Constants.REQUEST_ID);
    Assertions.assertEquals(USERNAME, Constants.USER_NAME);
  }

}
