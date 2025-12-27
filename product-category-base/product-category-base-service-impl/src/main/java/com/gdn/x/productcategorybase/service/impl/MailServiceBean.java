package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.mail.MailRecipientRequest;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.entity.MailEvent.ConfigMailEventEnums;
import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;
import com.gdn.x.productcategorybase.service.CategoryConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.MailService;
import com.gdn.x.productcategorybase.service.MerchantConfigurationHistoryService;
import com.gdn.x.productcategorybase.service.mailservice.MailDeliveryService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class MailServiceBean implements MailService {

  private static final String MAIL_SUBJECT_FOR_SELLER = " sellers are added to the configuration";
  private static final String MAIL_SUBJECT_FOR_CATEGORY = " categories are added to the configuration";
  private static final String TOTAL_COUNT = "totalConfigurations";
  private static final int SIZE = 50;
  private static final int PAGE = 0;
  private static final int MAIL_RECORDS_LIMIT = 10;

  @Autowired
  private MerchantConfigurationHistoryService merchantConfigurationHistoryService;

  @Autowired
  private CategoryConfigurationHistoryService categoryConfigurationHistoryService;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${review.configuration.mail.sender}")
  private String mailSender;

  @Value("${review.configuration.category.mail.to}")
  private String categoryMailTo;

  @Value("${review.configuration.merchant.mail.to}")
  private String merchantMailTo;

  @Value("${review.configuration.category.mail.cc}")
  private String categoryMailCc;

  @Value("${review.configuration.merchant.mail.cc}")
  private String merchantMailCc;

  @Value("${review.configuration.category.mail.templateId}")
  private String categoryTemplateId;

  @Value("${review.configuration.merchant.mail.templateId}")
  private String merchantTemplateId;

  @Override
  public void sendConfigurationChangesMailForCategory(Date date) throws Exception {
    GdnPreconditions.checkArgument(Objects.nonNull(date), ErrorMessage.CREATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    long totalCount = 0;
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    log.info("Initiating the email event for category configuration");
    List<List<String>> categoryConfigDatas = new ArrayList<>();
    Map<String, String> categoryMap = new HashMap<>();

    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<CategoryConfigurationHistory> configurationHistoryPage;
    do {
      configurationHistoryPage =
          this.categoryConfigurationHistoryService.getCategoryConfigurationByCreatedDate(storeId, date, pageable);
      for (CategoryConfigurationHistory categoryConfigurationHistory : configurationHistoryPage) {
        List<String> categoryData = new ArrayList<>();
        if (!categoryMap.containsKey(categoryConfigurationHistory.getCategoryCode())) {
          categoryMap
              .put(categoryConfigurationHistory.getCategoryCode(), categoryConfigurationHistory.getCategoryCode());
          if (categoryConfigDatas.size() < MAIL_RECORDS_LIMIT) {
            categoryData.add(categoryConfigurationHistory.getCategoryCode());
            categoryData.add(categoryConfigurationHistory.getCategoryName());
            categoryData.add(categoryConfigurationHistory.getOldValue());
            categoryData.add(categoryConfigurationHistory.getNewValue());
            categoryConfigDatas.add(categoryData);
          }
          totalCount++;
        }
      }
      pageable = configurationHistoryPage.nextPageable();
    } while (configurationHistoryPage.hasNext());

    sendConfigurationMailEvent(categoryConfigDatas, ConfigMailEventEnums.CATEGORY_CONFIG_ADDED.name(),
        totalCount, categoryMailTo, categoryMailCc, categoryTemplateId);
  }

  @Override
  public void sendConfigurationChangesMailForMerchant(Date date) throws Exception {
    if (Objects.isNull(date)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.CREATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    }
    long totalCount = 0;
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    log.info("Initiating the email event for merchant configuration");
    List<List<String>> merchantConfigDatas = new ArrayList<>();
    Map<String, String> merchantMap = new HashMap<>();

    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Page<MerchantConfigurationHistory> configurationHistoryPage;
    do {
      configurationHistoryPage =
          this.merchantConfigurationHistoryService.getMerchantConfigurationByCreatedDate(storeId, date, pageable);
      for (MerchantConfigurationHistory merchantConfigurationHistory : configurationHistoryPage) {
        List<String> merchantData = new ArrayList<>();
        if (!merchantMap.containsKey(merchantConfigurationHistory.getMerchantCode())) {
          merchantMap
              .put(merchantConfigurationHistory.getMerchantCode(), merchantConfigurationHistory.getMerchantCode());
          if (merchantConfigDatas.size() < MAIL_RECORDS_LIMIT) {
            merchantData.add(merchantConfigurationHistory.getMerchantCode());
            merchantData.add(merchantConfigurationHistory.getMerchantName());
            merchantData.add(merchantConfigurationHistory.getOldValue());
            merchantData.add(merchantConfigurationHistory.getNewValue());
            merchantConfigDatas.add(merchantData);
          }
          totalCount++;
        }
      }
      pageable = configurationHistoryPage.nextPageable();
    } while (configurationHistoryPage.hasNext());

    sendConfigurationMailEvent(merchantConfigDatas, ConfigMailEventEnums.MERCHANT_CONFIG_ADDED.name(),
        totalCount, merchantMailTo, merchantMailCc, merchantTemplateId);
  }

  private void sendConfigurationMailEvent(List<List<String>> configurationDatas, String event, long totalCount,
      String mailCc, String mailTo, String templateId) {
    if (totalCount > 0) {
      log.info("Publishing the email event for {} ", event);
      String mailSubject = ConfigMailEventEnums.MERCHANT_CONFIG_ADDED.name().equals(event) ?
          totalCount + MAIL_SUBJECT_FOR_SELLER :
          totalCount + MAIL_SUBJECT_FOR_CATEGORY;
      Map<String, Object> configDatas = new HashMap<>();
      configDatas.put("configurationDatas", configurationDatas);
      configDatas.put(Constants.REQ_ID, GdnMandatoryParameterUtil.getRequestId());
      configDatas.put(TOTAL_COUNT, totalCount);
      MailRecipientRequest recipientRequest = new MailRecipientRequest(mailCc, mailTo);
      mailDeliveryService.sendMail(templateId, mailSender, mailSubject, configDatas, "event", event, recipientRequest);
    }else {
      log.info("There is no {}", event);
    }
  }
}
