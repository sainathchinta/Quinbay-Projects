package com.gdn.partners.pcu.internal.client.interceptor;

import java.util.Objects;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ClientParameterProperties;

import feign.RequestInterceptor;
import feign.RequestTemplate;

/**
 * @author Pradeep Reddy
 */
@Component
public class SessionRequestInterceptor implements RequestInterceptor {

  @Autowired
  private ClientParameterProperties clientParameterProperties;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  private static final String PLUS_ENCODING = "%2B";
  private static final String PLUS_CHARACTER = "+";

  @Override
  public void apply(RequestTemplate template) {
    escapeSpecialCharacters(clientParameterHelper);

  }

  private void escapeSpecialCharacters(ClientParameterHelper clientParameterHelper) {
    if (clientParameterHelper.getUsername().contains(PLUS_CHARACTER)) {
      String userName = clientParameterHelper.getUsername().replace(PLUS_CHARACTER, PLUS_ENCODING);
      clientParameterHelper.set(Constants.USER_NAME, userName);
    }

    if (clientParameterHelper.getRequestId().contains(PLUS_CHARACTER)) {
      String requestId = clientParameterHelper.getRequestId().replace(PLUS_CHARACTER, PLUS_ENCODING);
      clientParameterHelper.set(Constants.REQUEST_ID, requestId);
    }
  }
}
