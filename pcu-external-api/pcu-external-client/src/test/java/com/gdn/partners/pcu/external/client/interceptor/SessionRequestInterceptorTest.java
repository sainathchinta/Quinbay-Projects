package com.gdn.partners.pcu.external.client.interceptor;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import feign.RequestTemplate;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class SessionRequestInterceptorTest {

  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";

  private static final String REQUEST_ID_WITH_SPECIAL_CHARS = "request+id";
  private static final String USERNAME_WITH_SPECIAL_CHARS = "user+name";

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private SessionRequestInterceptor sessionRequestInterceptor;

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.mandatoryParameterHelper);
  }

  @Test
  public void applyTest() {
    RequestTemplate template = new RequestTemplate();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME);
    sessionRequestInterceptor.apply(template);
    verify(mandatoryParameterHelper, times(1)).getRequestId();
    verify(mandatoryParameterHelper, times(1)).getUsername();
  }


  @Test
  public void applyTest_withSpecialCharacters() {
    RequestTemplate template = new RequestTemplate();
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID_WITH_SPECIAL_CHARS);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USERNAME_WITH_SPECIAL_CHARS);
    sessionRequestInterceptor.apply(template);
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper, times(2)).getUsername();
    Mockito.verify(mandatoryParameterHelper, times(2)).validateAndSet(anyString(), anyString());
  }
}
