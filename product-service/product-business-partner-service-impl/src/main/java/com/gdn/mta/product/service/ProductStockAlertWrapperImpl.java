package com.gdn.mta.product.service;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

@Service
@Slf4j
public class ProductStockAlertWrapperImpl implements ProductStockAlertWrapper {

  private static final String SYSTEM = "System";
  private static final String CM_MERCHANT = "CM";

  @Autowired
  private ProductStockAlertService productStockAlertService;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Trace(dispatcher=true)
  @Override
  @Async("sendMailAndNotificationStockAlertExecutor")
  public void sendMailAndNotification(String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    int maxStockAlertAttempt =
        Integer.parseInt(this.applicationProperties.getMaxStockAlertAttempt());
    List<String> businessPartnerCodes =
        this.productStockAlertService.findListBusinessPartnerMinimumStock(maxStockAlertAttempt);
    if (CollectionUtils.isNotEmpty(businessPartnerCodes)) {
      for (String businessPartnerCode : businessPartnerCodes) {
        try {
          List<PbpStockAlert> pbpStockAlerts = this.productStockAlertService
              .findPbpStockAlertByBusinessPartnerCode(businessPartnerCode, maxStockAlertAttempt);
          ProfileResponse businessPartner = this.businessPartnerRepository
              .filterDetailByBusinessPartnerCode(pbpStockAlerts.get(0).getBusinessPartnerCode());
          if (Objects.nonNull(businessPartner) && MerchantStatus.ACTIVE
              .equals(MerchantStatus.valueOf(businessPartner.getMerchantStatus()))) {
            if (Objects.nonNull(businessPartner.getCompany()) && CM_MERCHANT
                .equalsIgnoreCase(businessPartner.getCompany().getMerchantType())
                && !businessPartner.getCompany().isOfflineToOnlineFlag()) {
              if (CollectionUtils.isNotEmpty(pbpStockAlerts)) {
                for (PbpStockAlert pbpStockAlert : pbpStockAlerts) {
                  this.productStockAlertService.updateStockAlertForMailer(pbpStockAlert);
                }
                this.productStockAlertService
                    .sendMailAndNotificationForStockAlert(pbpStockAlerts, businessPartner);
              }
            }
          }
        } catch (Exception e) {
          log.error(
              "Error sending email notification products with stock alert, businessPartnerCode : {}",
              businessPartnerCode, e);
        }
      }
    }
    log.info("Completed sending of all stock alert email and notification");
  }
}
