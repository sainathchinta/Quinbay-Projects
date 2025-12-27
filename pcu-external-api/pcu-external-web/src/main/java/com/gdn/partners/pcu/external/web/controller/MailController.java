package com.gdn.partners.pcu.external.web.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.MailApiPath;
import com.gdn.partners.pcu.external.service.MailService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Mail API")
@RestController
@RequestMapping(value = MailApiPath.BASE_PATH)
@Validated
public class MailController {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private MailService mailService;

  @Operation(summary ="Get the send email for merchant-care")
  @GetMapping(value = MailApiPath.SEND_MAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse sendMail() {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method: API to send merchant care email for business partner : {}", businessPartnerCode);
    this.mailService.sendMail(businessPartnerCode);
    return new GdnBaseRestResponse(null, null, true, mandatoryParameterHelper.getRequestId());
  }

  @Operation(summary ="API to check the eligibilty of merchant-care notification email")
  @GetMapping(value = MailApiPath.NOTIFY_MAIL_VISIBILITY_OPTION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<Boolean> notifyMailVisibilityOption() {
    String businessPartnerCode = mandatoryParameterHelper.getBusinessPartnerCode();
    log.info("Method: API to check the eligibilty of merchant-care notification email for business partner : {}",
        businessPartnerCode);
    boolean enableEmailSend = this.mailService.notifyMailVisibilityOptionForProductWip(businessPartnerCode);
    return new SingleBaseResponse(null, null, true, mandatoryParameterHelper.getRequestId(),
        enableEmailSend);
  }
}
