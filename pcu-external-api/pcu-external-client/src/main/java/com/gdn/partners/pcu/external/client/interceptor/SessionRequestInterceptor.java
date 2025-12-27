package com.gdn.partners.pcu.external.client.interceptor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import feign.RequestInterceptor;
import feign.RequestTemplate;

/**
 * @author Pradeep Reddy
 */
@Component
public class SessionRequestInterceptor implements RequestInterceptor {

  private static final String PLUS_CHARACTER = "+";
  private static final String PLUS_ENCODING = "%2B";

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Override
  public void apply(RequestTemplate template) {
    escapeSpecialCharacters();
  }

  private void escapeSpecialCharacters() {
    if (mandatoryParameterHelper.getUsername().contains(PLUS_CHARACTER)) {
      mandatoryParameterHelper.validateAndSet(Constants.USER_NAME,
          mandatoryParameterHelper.getUsername().replace(PLUS_CHARACTER, PLUS_ENCODING));
    }

    if (mandatoryParameterHelper.getRequestId().contains(PLUS_CHARACTER)) {
      mandatoryParameterHelper.validateAndSet(Constants.REQUEST_ID,
          mandatoryParameterHelper.getRequestId().replace(PLUS_CHARACTER, PLUS_ENCODING));
    }
  }
}
