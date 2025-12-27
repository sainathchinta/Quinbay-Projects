package com.gdn.x.productcategorybase.service.businessPartner;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.outbound.xbp.feign.XbpFeign;
import com.gdn.x.productcategorybase.service.BusinessPartner.BusinessPartnerService;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;
import com.gdn.x.productcategorybase.service.notificationService.NotificationService;

@Service
public class BusinessPartnerServiceImpl implements BusinessPartnerService {

  private static final String BRAND_NAME = "brandName";
  private static final String NOTES = "notes";
  private static final String MESSAGE_TEMPLATE_ID_BRAND_APPROVED = "brandApprovedMessageTemplateId";
  private static final String MESSAGE_TEMPLATE_ID_BRAND_REJECTED = "brandRejectedMessageTemplateId";
  private static final String MAIL_FROM = "no-reply@blibli.com";
  private static final String MESSAGE_IDENTIFIER_KEY = "brandRequestCode";
  private static final String BRAND_APPROVE_SUBJECT = "Blibli.com - Notifikasi Persetujuan Brand";
  private static final String BRAND_REJECT_SUBJECT = "Blibli.com - Notifikasi Penolakan Brand";

  @Autowired
  private XbpFeign xbpFeign;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Autowired
  private NotificationService notificationService;

  @Override
  public ProfileResponse getBusinessPartnerProfile(String businessPartnerCode) throws Exception {
    GdnRestSingleResponse<ProfileResponse> profileByBusinessPartnerCode =
        xbpFeign.getBusinessPartnerDetails(GdnMandatoryParameterUtil.getStoreId(),
            GdnMandatoryParameterUtil.getChannelId(), GdnMandatoryParameterUtil.getClientId(),
            GdnMandatoryParameterUtil.getRequestId(), GdnMandatoryParameterUtil.getUsername(),
            businessPartnerCode);
    if (Objects.isNull(profileByBusinessPartnerCode.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          " No data found for businessPartnerCode : " + businessPartnerCode);
    }
    return profileByBusinessPartnerCode.getValue();
  }

  @Async
  @Override
  public void createBrandStatusChangeNotification(BrandWip brandWip) throws Exception {
    ProfileResponse profileResponse =
        getBusinessPartnerProfile(brandWip.getBusinessPartnerCode());
    if (Objects.isNull(profileResponse) || Objects.isNull(profileResponse.getResponsiblePerson())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "No profile found with businessPartnerCode : " + brandWip.getBusinessPartnerCode());
    }
    createBrandStatusChangeEmail(brandWip, profileResponse.getResponsiblePerson().getEmail());
    notificationService.createWebNotification(brandWip);
  }

  /**
   * Send email regarding brand approval status to the merchant
   *
   * @param brandWip
   * @param mailTo
   */
  private void createBrandStatusChangeEmail(BrandWip brandWip, String mailTo) {
    Map<String, Object> variables = new HashMap<>();
    variables.put(BRAND_NAME, brandWip.getBrandName());
    if (BrandWipState.APPROVED.equals(brandWip.getState())) {
      mailDeliveryService.sendMail(MESSAGE_TEMPLATE_ID_BRAND_APPROVED, MAIL_FROM, BRAND_APPROVE_SUBJECT, variables,
          MESSAGE_IDENTIFIER_KEY, brandWip.getBrandRequestCode(), new MailRecipientRequest(null, mailTo));
    } else {
      variables.put(NOTES, new String(brandWip.getNotes()));
      mailDeliveryService.sendMail(MESSAGE_TEMPLATE_ID_BRAND_REJECTED, MAIL_FROM, BRAND_REJECT_SUBJECT, variables,
          MESSAGE_IDENTIFIER_KEY, brandWip.getBrandRequestCode(), new MailRecipientRequest(null, mailTo));
    }
  }
}
