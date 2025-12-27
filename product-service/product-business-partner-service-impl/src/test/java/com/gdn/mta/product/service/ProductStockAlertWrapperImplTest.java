package com.gdn.mta.product.service;

import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.x.businesspartner.commons.enums.MerchantStatus;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.Arrays;

public class ProductStockAlertWrapperImplTest {

  private static final int MAX_ALERT_ATTEMPT = 3;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-000001";
  private static final String DEFAULT_GDN_SKU = "gdnSku";
  private static final String SYSTEM = "System";
  private static final String CM_MERCHANT = "CM";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_USERNAME = "username";

  private PbpStockAlert pbpStockAlert = new PbpStockAlert();
  private ProfileResponse profileResponse = new ProfileResponse();

  @Mock
  private ProductStockAlertService productStockAlertService;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @InjectMocks
  private ProductStockAlertWrapperImpl productStockAlertWrapperImpl;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    pbpStockAlert.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    profileResponse.setMerchantStatus(MerchantStatus.ACTIVE.toString());
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(CM_MERCHANT);

    Mockito.doNothing().when(this.productStockAlertService)
        .updateStockAlertForMailer(Mockito.any(PbpStockAlert.class));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productStockAlertService);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(applicationProperties);
  }

  @Test
  public void sendMailAndNotificationTest() throws Exception {
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productStockAlertService)
        .sendMailAndNotificationForStockAlert(Arrays.asList(pbpStockAlert), profileResponse);
    Mockito.verify(this.productStockAlertService).updateStockAlertForMailer(pbpStockAlert);
  }

  @Test
  public void sendMailAndNotificationTest_emptyMerchants() throws Exception {
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(new ArrayList<>());
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
  }

  @Test
  public void sendMailAndNotificationTest_emptyProfileResponse() throws Exception {
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendMailAndNotificationTest_inactiveMerchant() throws Exception {
    profileResponse.setMerchantStatus(MerchantStatus.INACTIVE.toString());
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendMailAndNotificationTest_nonCMTypeMerchant() throws Exception {
    profileResponse.getCompany().setMerchantType(DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendMailAndNotificationTest_O2OMerchant() throws Exception {
    profileResponse.getCompany().setOfflineToOnlineFlag(true);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendMailAndNotificationTest_emptyProfileResponseCompany() throws Exception {
    profileResponse.setCompany(null);
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(pbpStockAlert));
    Mockito.when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
    Mockito.verify(this.productStockAlertService)
        .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
    Mockito.verify(this.productStockAlertService)
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendMailAndNotificationTest_expectException() throws Exception {
    Mockito.when(this.applicationProperties.getMaxStockAlertAttempt())
        .thenReturn(String.valueOf(MAX_ALERT_ATTEMPT));
    Mockito
        .when(this.productStockAlertService.findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT))
        .thenReturn(Arrays.asList(DEFAULT_BUSINESS_PARTNER_CODE));
    Mockito.when(this.productStockAlertService
        .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT))
        .thenThrow(Exception.class);
    try {
      productStockAlertWrapperImpl.sendMailAndNotification(DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    } finally {
      Mockito.verify(this.productStockAlertService)
          .findListBusinessPartnerMinimumStock(MAX_ALERT_ATTEMPT);
      Mockito.verify(this.applicationProperties).getMaxStockAlertAttempt();
      Mockito.verify(this.productStockAlertService)
          .findPbpStockAlertByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE, MAX_ALERT_ATTEMPT);
    }
  }
}
