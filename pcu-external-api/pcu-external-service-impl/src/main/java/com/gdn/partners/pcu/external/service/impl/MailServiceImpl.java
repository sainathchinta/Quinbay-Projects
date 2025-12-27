package com.gdn.partners.pcu.external.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.service.MailService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class MailServiceImpl implements MailService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public void sendMail(String businessPartnerCode) {
    GdnRestSimpleResponse<Boolean> response = pbpFeign.notifyMailVisibilityOptionForProductWip(businessPartnerCode);
    ResponseHelper.validateResponse(response);
    boolean enableEmailSend = response.getValue();
    if (enableEmailSend) {
      GdnBaseRestResponse saveResponse = pbpFeign.sendEmailForExceededActivation(businessPartnerCode);
      ResponseHelper.validateResponse(saveResponse);
    }
  }

  @Override
  public boolean notifyMailVisibilityOptionForProductWip(String businessPartnerCode) {
    GdnRestSimpleResponse<Boolean> response = pbpFeign.notifyMailVisibilityOptionForProductWip(businessPartnerCode);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }
}
