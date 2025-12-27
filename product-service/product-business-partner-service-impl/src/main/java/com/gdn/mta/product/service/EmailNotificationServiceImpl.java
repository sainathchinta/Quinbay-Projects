package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.MessageEmailRequest;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.FailedRetryProductsResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCollectionElement;
import com.gda.mta.product.dto.ProductItemBusinessPartnerResponse;
import com.gda.mta.product.dto.StuckProductsResponse;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.EntityNotificationMode;
import com.gdn.mta.product.entity.EntityNotificationType;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.entity.ProductWfState;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.merchantEducation.MerchantEducationOutbound;
import com.gdn.partners.pbp.outbound.merchantEducation.NotificationSettings;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EmailNotificationServiceImpl implements EmailNotificationService {

   @Autowired
   private MerchantEducationOutbound merchantEducationOutbound;

   @Autowired
   private BusinessPartnerRepository businessPartnerRepository;

   @Autowired
   private KafkaPublisher kafkaProducer;

   @Autowired
   private ApplicationProperties applicationProperties;

   @Autowired
   private ProductLevel3RetryService productLevel3RetryService;

   @Autowired
   private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${send.failed.retry.product.summary.email.address}")
  private String sendFailedRetryProductSummaryEmailAddress;

  @Value("${merchant.care.mail}")
  private String merchantCareMailAddress;

  @Value("${send.stuck.product.summary.email.address}")
  private String sendStuckProductSummaryEmailAddress;

  @Override
  public void sendEmailDeleteProductBusinessPartner(ProductDetailResponse product,
      Page<ProductBusinessPartnerResponse> productBusinessPartners, String notes, String productName)
      throws Exception {
    Map<String, ProductBusinessPartnerResponse> businessPartnerIdProductBusinessPartnerMap =
        productBusinessPartners.getContent().stream().collect(Collectors.toMap(
            ProductBusinessPartnerResponse::getBusinessPartnerId, Function.identity()));
    Map<String, String> productItemIdGeneratedNameMap = product.getProductItemResponses().stream()
        .collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getGeneratedItemName));
    for (Map.Entry<String, ProductBusinessPartnerResponse> productBusinessPartnerResponseEntry :
        businessPartnerIdProductBusinessPartnerMap.entrySet()) {
      List<List<String>> deletedProducts = getDeletedProducts(
          productItemIdGeneratedNameMap, productBusinessPartnerResponseEntry);
      ProfileResponse businessPartner =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartnerResponseEntry.getKey());
      MessageEmailRequest email = new MessageEmailRequest();
      try {
        if (isNotificationEnabled(product.getStoreId(), businessPartner,
            EntityNotificationType.PRODUCT_REJECT_BY_TEAM_QC.getName(), EntityNotificationMode.EMAIL.getName())) {
          Map<String, Object> variables = new HashMap<>();
          variables.put("name", businessPartner.getResponsiblePerson().getName());
          variables.put("deletedProducts", deletedProducts);
          variables.put("notes", notes);
          variables.put("productName", productName);
          String templateName = Objects.nonNull(productName) ? "rejectDuplicateProduct" : "deleteProductBusinessPartner";
          email = ConverterUtil.getMessageEmailRequest(templateName, businessPartner.getResponsiblePerson().getEmail(),
              product.getCreatedBy(), applicationProperties.getProductRejectionAlertEmailSubject(),
              Constants.NOTIFY_MERCHANT_IDENTIFIER_KEY, UUID.randomUUID().toString(), variables,
              applicationProperties.getProductEventAlertEmailFrom());
          setMandatoryParamsToNotificationKafka(email);
          log.info("Sending email to merchant on delete product. Email : {}", email);
          kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
        }
      } catch (Exception e) {
        log.error("Error while sending email to merchant on delete product.Email : {}", email, e);
      }
    }
  }

  @Override
  public void sendEmailForProductLive(String storeId, List<ProfileResponse> profileList,
      Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap) {
    for (ProfileResponse profile : profileList) {
      MessageEmailRequest email = new MessageEmailRequest();
      try {
        if (isNotificationEnabled(storeId, profile, EntityNotificationType.PRODUCT_LIVE.getName(),
            EntityNotificationMode.EMAIL.getName())) {
          Map<String, Object> objModelParams = new HashMap<>();
          objModelParams.put("name", profile.getResponsiblePerson().getName());
          objModelParams.put("productCollections", productCollectionUpdateByMap.get(profile.getBusinessPartnerCode()));
          email = ConverterUtil.getMessageEmailRequest(Constants.NOTIFY_MERCHANT_PROCDUCT_ACTIVATED_TEMPLATE,
              profile.getResponsiblePerson().getEmail(), null, null,
              Constants.NOTIFY_MERCHANT_IDENTIFIER_KEY, UUID.randomUUID().toString(), objModelParams,
              applicationProperties.getProductEventAlertEmailFrom());
          setMandatoryParamsToNotificationKafka(email);
          log.info("invoking send email to merchant for product becoming live. Email : {}", email);
          kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
        }
      } catch (Exception e) {
        log.error("Error while sending email to merchant for product becoming live.Email : {}", email, e);
      }
    }
  }

  @Override
  public void sendFailedRetryProductsMail(List<ProductLevel3FailedEntity> failedRetryProductList) {
    List<FailedRetryProductsResponse> failedRetryProductsResponseList = failedRetryProductList.stream().map(
        productLevel3FailedEntity -> new FailedRetryProductsResponse(productLevel3FailedEntity.getProductSku(),
            productLevel3FailedEntity.getCreatedDate().toString().split(Constants.DOT_REGEX)[0],
            productLevel3FailedEntity.getUpdatedDate().toString().split(Constants.DOT_REGEX)[0])).collect(Collectors.toList());
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<>();
    mailObjectWrapper.put(Constants.OBJECT, failedRetryProductsResponseList);
    mailObjectWrapper.put(Constants.TOTAL, failedRetryProductList.size());
    emailObject.put(Constants.EMAIL_OBJECT, mailObjectWrapper);
    MessageEmailRequest email = ConverterUtil.getMessageEmailRequest(Constants.FAILED_RETRY_PRODUCT_TEMPLATE_ID,
        sendFailedRetryProductSummaryEmailAddress, null, Constants.FAILED_RETRY_PRODUCT_EMAIL_SUBJECT,
        Constants.FAILED_RETRY_PRODUCT_TEMPLATE_ID, UUID.randomUUID().toString(), emailObject, null);
    try {
      setMandatoryParamsToNotificationKafka(email);
      kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
      for (ProductLevel3FailedEntity productLevel3FailedEntity : failedRetryProductList) {
        productLevel3FailedEntity.setMarkForDelete(true);
      }
      productLevel3RetryService.updateFailedRetryProductsAfterMail(failedRetryProductList);
      log.info("Email for Failed Retry Products is sent : {}", email.getMessageTo());
    } catch (Exception e) {
      log.error("Error while sending mail for failed retry products", e);
    }
  }

  @Override
  public void sendExceedActivationEmail(ProfileResponse profileResponse, String username,
      ProductBusinessPartnerConfigRequest request, List<ProductLevel3WipDTO> productLevel3Wips,
      String submissionDateString) {
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    mailObjectWrapper.put("destination", merchantCareMailAddress);
    mailObjectWrapper.put("bpCode", request.getBpCode());
    mailObjectWrapper.put("count", String.valueOf(productLevel3Wips.size()));
    mailObjectWrapper.put("businessPartnerName", profileResponse.getCompany().getBusinessPartnerName());
    mailObjectWrapper.put("date", submissionDateString);
    String subject = Constants.EXCEED_ACTIVATION_MAIL_DESCRIPTION.replace("{0}", String.valueOf(productLevel3Wips.size()));
    log.info("Sending email for products which exceeded activation for business partner : {}",
        profileResponse.getBusinessPartnerCode());
    MessageEmailRequest emailRequest = ConverterUtil.getEmailRequest(
        Constants.MAIL_FOR_EXCEED_ACTIVATION_FOR_PRODUCT_WIP_ID, Constants.MAIL_SENDER, subject, mailObjectWrapper,
        Constants.MESSAGE_IDENTIFIER_KEY, username, merchantCareMailAddress);
    setMandatoryParamsToNotificationKafka(emailRequest);
    kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, emailRequest.getMessageIdentifierValue(), emailRequest);
    log.info("Email request has been sent. mailTemplateId: {}, recipient: {}",
        emailRequest.getMessageId(), emailRequest.getMessageTo());
  }

  @Override
  public void sendEmailToBusinessPartnerForDeletedProducts(String storeId, String businessPartnerCode,
      List<ProductCollection> productCollections) {
    if (CollectionUtils.isNotEmpty(productCollections)) {
      List<List<String>> productDatas = new ArrayList<>();
      for (ProductCollection productCollection : productCollections) {
        List<String> productData = new ArrayList<>();
        productData.add(productCollection.getProductCode());
        productData.add(productCollection.getProductName());
        productDatas.add(productData);
      }
      MessageEmailRequest email = new MessageEmailRequest();
      String templateName;
      try {
        ProfileResponse businessPartner = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
        if (businessPartner.getCompany().isInternationalFlag()) {
          templateName = Constants.TEMPLATE_AUTO_DELETE_NEED_CORRECTION_PRODUCT_ENGLISH;
          email.setMessageSubject(productDatas.size() + " " + Constants.PRODUCT_AUTO_REJECTION_SUBJECT_ENGLISH);
        } else {
          templateName = Constants.TEMPLATE_AUTO_DELETE_NEED_CORRECTION_PRODUCT;
          email.setMessageSubject(productDatas.size() + " " + Constants.PRODUCT_AUTO_REJECTION_SUBJECT);
        }
        Map<String, Object> variables = new HashMap<>();
        variables.put("name", businessPartner.getResponsiblePerson().getName());
        variables.put("deletedProducts", productDatas);
        email = ConverterUtil.getMessageEmailRequest(templateName, businessPartner.getResponsiblePerson().getEmail(),
            null, applicationProperties.getProductRejectionAlertEmailSubject(),
            Constants.NOTIFY_MERCHANT_IDENTIFIER_KEY, UUID.randomUUID().toString(), variables,
            applicationProperties.getProductEventAlertEmailFrom());
        setMandatoryParamsToNotificationKafka(email);
        log.info("Sending email to merchant on delete product. Email : {}", email);
        kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
      } catch (Exception e) {
        log.error("Error while sending email to merchant on delete product.Email : {}", email, e);
      }
    }
  }

  @Override
  public void sendProductStuckAlertMail(List<ProductWfState> productsAboveRetryCount, Integer batchSize) {
    List<StuckProductsResponse> stuckProductsResponsesList = productsAboveRetryCount.stream()
        .map(p -> new StuckProductsResponse(p.getProductCode(), p.getState(),
            p.getCreatedDateAsString().split(Constants.DOT_REGEX)[0], p.getUpdatedDateAsString()
            .split(Constants.DOT_REGEX)[0])).collect(Collectors.toList());
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<>();
    mailObjectWrapper.put(Constants.OBJECT, stuckProductsResponsesList);
    mailObjectWrapper.put(Constants.TOTAL, CollectionUtils.isEmpty(productsAboveRetryCount) ? 0 :
        productsAboveRetryCount.size());
    mailObjectWrapper.put(Constants.BATCH_SIZE, batchSize);
    emailObject.put(Constants.EMAIL_OBJECT, mailObjectWrapper);
    MessageEmailRequest email = ConverterUtil.getMessageEmailRequest(Constants.RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID,
        sendStuckProductSummaryEmailAddress, null, Constants.RETRY_PRODUCT_POST_LIVE_EMAIL_SUBJECT,
        Constants.RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID, UUID.randomUUID().toString(), emailObject,
        Constants.RETRY_PRODUCT_ACTIVATION_EMAIL);
    try {
      setMandatoryParamsToNotificationKafka(email);
      kafkaProducer.send(Constants.SEND_EMAIL_TO_OTHERS_EVENT, email.getMessageIdentifierValue(), email);
      log.info("Email for PBP retry job is sent : {}", email.getMessageTo());
    } catch (Exception e) {
      log.error("Product Retry Summary Email failed to send", e);
    }
  }

  private boolean isNotificationEnabled(String storeId, ProfileResponse businessPartner, String notificationType, String notificationMode) throws Exception {
    NotificationSettings notificationSettings = merchantEducationOutbound
        .findByUsernameAndStoreCode(storeId, businessPartner.getResponsiblePerson().getEmail(),
            businessPartner.getBusinessPartnerCode());
    return Optional.ofNullable(notificationSettings)
        .map(NotificationSettings::getNotificationSettings)
        .map(notificationTypeSetting -> notificationTypeSetting.get(notificationType))
        .map(notificationModeSetting -> notificationModeSetting.get(notificationMode))
        .orElse(false);
  }

  private List<List<String>> getDeletedProducts(Map<String, String> productItemIdGeneratedNameMap,
      Map.Entry<String, ProductBusinessPartnerResponse> productBusinessPartnerResponseEntry) {
    List<List<String>> deletedProducts = new ArrayList<>();
    for (ProductItemBusinessPartnerResponse productItemBusinessPartner :
        productBusinessPartnerResponseEntry.getValue().getProductItemBusinessPartners()) {
      List<String> deletedProduct = new ArrayList<>();
      deletedProduct.add(productBusinessPartnerResponseEntry.getValue().getCreatedDate().toString());
      if (StringUtils.isNoneBlank(productItemIdGeneratedNameMap.get(productItemBusinessPartner.getProductItemId()))) {
        deletedProduct.add(productItemBusinessPartner.getGdnProductItemSku());
        deletedProduct.add(productItemIdGeneratedNameMap.get(productItemBusinessPartner.getProductItemId()));
      }
      deletedProducts.add(deletedProduct);
    }
    return deletedProducts;
  }

  private void setMandatoryParamsToNotificationKafka(MessageEmailRequest messageEmailRequest) {
    messageEmailRequest.setStoreId(mandatoryParameterHelper.getStoreId());
    messageEmailRequest.setRequestId(mandatoryParameterHelper.getRequestId());
    messageEmailRequest.setChannelId(mandatoryParameterHelper.getChannelId());
    messageEmailRequest.setUsername(mandatoryParameterHelper.getUsername());
    messageEmailRequest.setClientId(mandatoryParameterHelper.getClientId());
    if(StringUtils.isBlank(messageEmailRequest.getStoreId())) {
      messageEmailRequest.setStoreId(Constants.DEFAULT_STORE_ID);
    }
  }
}
