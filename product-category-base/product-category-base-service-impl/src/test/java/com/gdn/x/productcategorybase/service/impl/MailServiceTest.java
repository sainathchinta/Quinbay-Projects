package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Date;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.service.CategoryConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;

public class MailServiceTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String POST_LIVE_STATUS = "Post-live";
  private static final String PRE_LIVE_STATUS = "Pre-live";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT = "Neutral";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String ACTIVITY = "Registered";
  private static final String CATEGORY_NAME = "category";
  private Pageable pageable = PageRequest.of(1, 50);
  private static final long TOTAL_RECORDS = 50;

  private CategoryConfigurationHistory categoryConfigurationHistory;
  private MerchantConfigurationHistory merchantConfigurationHistory;
  private Page<CategoryConfigurationHistory> configurationHistoryPage;
  private Page<MerchantConfigurationHistory> merchantConfigurationHistoryPage;

  @InjectMocks
  private MailServiceBean mailServiceBean;

  @Mock
  private MerchantConfigurationHistoryService merchantConfigurationHistoryService;

  @Mock
  private CategoryConfigurationHistoryService categoryConfigurationHistoryService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    categoryConfigurationHistory = new CategoryConfigurationHistory();
    categoryConfigurationHistory.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationHistory.setCategoryName(CATEGORY_NAME);
    categoryConfigurationHistory.setOldValue(PRE_LIVE_STATUS);
    categoryConfigurationHistory.setNewValue(POST_LIVE_STATUS);
    categoryConfigurationHistory.setActivity(ACTIVITY);

    merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistory.setMerchantCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationHistory.setMerchantName(DEFAULT_BUSINESS_PARTNER_NAME);
    merchantConfigurationHistory.setOldValue(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationHistory.setNewValue(DEFAULT_REVIEW_CONFIG_FLAG_FOR_MERCHANT);
    merchantConfigurationHistory.setActivity(ACTIVITY);
  }

  @Test
  public void sendConfigurationMailForCategoryTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Date date = new Date();
    configurationHistoryPage = new PageImpl<>(Arrays.asList(categoryConfigurationHistory), pageable,
        Arrays.asList(categoryConfigurationHistory).size());

    when(this.categoryConfigurationHistoryService
        .getCategoryConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class))).thenReturn(configurationHistoryPage);
    this.mailServiceBean.sendConfigurationChangesMailForCategory(date);
    verify(this.categoryConfigurationHistoryService)
        .getCategoryConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class));
    verify(this.mailDeliveryService, Mockito.times(1))
        .sendMail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(Map.class),
            Mockito.any(), Mockito.any(), Mockito.any(MailRecipientRequest.class));
  }

  @Test
  public void sendConfigurationMailForMerchantTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Date date = new Date();
    merchantConfigurationHistoryPage = new PageImpl<>(Arrays.asList(merchantConfigurationHistory), pageable,
        Arrays.asList(merchantConfigurationHistory).size());
    when(this.merchantConfigurationHistoryService
        .getMerchantConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class))).thenReturn(merchantConfigurationHistoryPage);
    this.mailServiceBean.sendConfigurationChangesMailForMerchant(date);
    verify(this.merchantConfigurationHistoryService)
        .getMerchantConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class));
    verify(this.mailDeliveryService, Mockito.times(1))
        .sendMail(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(Map.class),
            Mockito.any(), Mockito.any(), Mockito.any(MailRecipientRequest.class));
  }

  @Test
  public void sendConfigurationMailWithZeroConfigurationTest() throws Exception {
    MDC.put(GdnMandatoryParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID);
    Date date = new Date();
    merchantConfigurationHistory = new MerchantConfigurationHistory();
    merchantConfigurationHistoryPage =
        new PageImpl<>(Arrays.asList(), pageable, Arrays.asList(merchantConfigurationHistory).size());
    when(this.merchantConfigurationHistoryService
        .getMerchantConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class))).thenReturn(merchantConfigurationHistoryPage);
    this.mailServiceBean.sendConfigurationChangesMailForMerchant(date);
    verify(this.merchantConfigurationHistoryService)
        .getMerchantConfigurationByCreatedDate(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(date),
            Mockito.any(Pageable.class));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.mailDeliveryService);
    Mockito.verifyNoMoreInteractions(this.merchantConfigurationHistoryService);
    Mockito.verifyNoMoreInteractions(this.categoryConfigurationHistoryService);
  }
}
