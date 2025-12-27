package com.gdn.mta.product.service;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import com.gda.mta.product.dto.MessageEmailRequest;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCollectionElement;
import com.gda.mta.product.dto.ProductItemBusinessPartnerResponse;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.EntityNotificationMode;
import com.gdn.mta.product.entity.EntityNotificationType;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.entity.ProductWfState;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.valueobject.ProductLevel3WipDTO;
import com.gdn.partners.pbp.outbound.merchantEducation.MerchantEducationOutbound;
import com.gdn.partners.pbp.outbound.merchantEducation.NotificationSettings;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class EmailNotificationServiceImplTest {

  @Mock
  private MerchantEducationOutbound merchantEducationOutbound;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private ProductLevel3RetryService productLevel3RetryService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private EmailNotificationServiceImpl emailNotificationService;

  private NotificationSettings notificationSettings;
  private Map<String, Map<String, Boolean>> notificationSettingsMap;
  private ProductDetailResponse productDetailResponse;
  private Page<ProductBusinessPartnerResponse> productBusinessPartnerResponsePage;
  private ProfileResponse profileResponse;

  private static final String BUSINESS_PARTNER_EMAIL_ADDRESS = "business_partner@email.com";
  private static final String BUSINESS_PARTNER_CODE = "bus-0001";
  private static final String ITEM_ID_1 = "itemId1";
  private static final String ITEM_ID_2 = "itemId2";
  private static final String ITEM_ID_3 = "itemId3";
  private static final String ITEM_SKU_1 = "itemSku1";
  private static final String ITEM_SKU_3 = "itemSku3";
  private static final String GENERATED_ITEM_NAME_1 = "generatedItemName1";
  private static final String GENERATED_ITEM_NAME_2 = "generatedItemName2";
  private static final String NOTES = "notes";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String USER_NAME = "userName";
  private static final String SUBMISSION_DATE = new Date().toString();
  private static final String STORE_ID = "10001";
  private static final String STUCK_STATE = "stuck";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    notificationSettings = new NotificationSettings();
    notificationSettingsMap = new HashMap<>();
    notificationSettings.setUsername(BUSINESS_PARTNER_EMAIL_ADDRESS);
    notificationSettings.setStoreCode(BUSINESS_PARTNER_CODE);
    notificationSettings.setNotificationSettings(notificationSettingsMap);
    productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse1 = new ProductItemResponse();
    productItemResponse1.setId(ITEM_ID_1);
    productItemResponse1.setGeneratedItemName(GENERATED_ITEM_NAME_1);
    ProductItemResponse productItemResponse2 = new ProductItemResponse();
    productItemResponse2.setId(ITEM_ID_2);
    productItemResponse2.setGeneratedItemName(GENERATED_ITEM_NAME_2);
    Set<ProductItemResponse> productItemResponseSet = new HashSet<>();
    productItemResponseSet.add(productItemResponse1);
    productItemResponseSet.add(productItemResponse2);
    productDetailResponse.setProductItemResponses(productItemResponseSet);
    List<ProductBusinessPartnerResponse> productBusinessPartnerResponseList = new ArrayList<>();
    ProductBusinessPartnerResponse productBusinessPartnerResponse = new ProductBusinessPartnerResponse();
    productBusinessPartnerResponse.setBusinessPartnerId(BUSINESS_PARTNER_CODE);
    ProductItemBusinessPartnerResponse productItemBusinessPartnerResponse1 = new ProductItemBusinessPartnerResponse();
    productItemBusinessPartnerResponse1.setProductItemId(ITEM_ID_1);
    productItemBusinessPartnerResponse1.setGdnProductItemSku(ITEM_SKU_1);
    ProductItemBusinessPartnerResponse productItemBusinessPartnerResponse2 = new ProductItemBusinessPartnerResponse();
    productItemBusinessPartnerResponse2.setProductItemId(ITEM_ID_3);
    productItemBusinessPartnerResponse2.setGdnProductItemSku(ITEM_SKU_3);
    productBusinessPartnerResponse.setProductItemBusinessPartners(
        Arrays.asList(productItemBusinessPartnerResponse1, productItemBusinessPartnerResponse2));
    productBusinessPartnerResponse.setCreatedDate(new Date());
    productBusinessPartnerResponseList.add(productBusinessPartnerResponse);
    productBusinessPartnerResponsePage = new PageImpl<>(productBusinessPartnerResponseList);
    profileResponse = new ProfileResponse();
    ResponsiblePersonDTO responsiblePersonDTO = new ResponsiblePersonDTO();
    responsiblePersonDTO.setEmail(BUSINESS_PARTNER_EMAIL_ADDRESS);
    profileResponse.setResponsiblePerson(responsiblePersonDTO);
    profileResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setInternationalFlag(false);
    profileResponse.setCompany(companyDTO);
    Mockito.when(merchantEducationOutbound.findByUsernameAndStoreCode(Mockito.any(), Mockito.eq(BUSINESS_PARTNER_EMAIL_ADDRESS),
        Mockito.eq(BUSINESS_PARTNER_CODE))).thenReturn(notificationSettings);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getChannelId()).thenReturn(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getClientId()).thenReturn(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    Mockito.when(mandatoryParameterHelper.getUsername()).thenReturn(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(merchantEducationOutbound);
    Mockito.verifyNoMoreInteractions(businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(applicationProperties);
    Mockito.verifyNoMoreInteractions(productLevel3RetryService);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void sendEmailDeleteProductBusinessPartner() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), true);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_REJECT_BY_TEAM_QC.getName(), emailMap);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    emailNotificationService.sendEmailDeleteProductBusinessPartner(
        productDetailResponse, productBusinessPartnerResponsePage, NOTES, PRODUCT_NAME);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(productDetailResponse.getStoreId(), BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailDeleteProductBusinessPartnerProductNameNull() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), true);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_REJECT_BY_TEAM_QC.getName(), emailMap);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    emailNotificationService.sendEmailDeleteProductBusinessPartner(
        productDetailResponse, productBusinessPartnerResponsePage, NOTES, null);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(productDetailResponse.getStoreId(), BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailDeleteProductBusinessPartnerException() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), true);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_REJECT_BY_TEAM_QC.getName(), emailMap);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
    emailNotificationService.sendEmailDeleteProductBusinessPartner(
        productDetailResponse, productBusinessPartnerResponsePage, NOTES, PRODUCT_NAME);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(productDetailResponse.getStoreId(), BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailDeleteProductBusinessPartnerNotificationDisabled() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), false);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_REJECT_BY_TEAM_QC.getName(), emailMap);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    emailNotificationService.sendEmailDeleteProductBusinessPartner(
        productDetailResponse, productBusinessPartnerResponsePage, NOTES, PRODUCT_NAME);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(productDetailResponse.getStoreId(), BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendEmailForProductLive() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), true);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_LIVE.getName(), emailMap);
    Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap = new HashMap<>();
    ProductCollectionElement productCollectionElement = new ProductCollectionElement();
    productCollectionUpdateByMap.put(BUSINESS_PARTNER_CODE, Arrays.asList(productCollectionElement));
    emailNotificationService.sendEmailForProductLive(STORE_ID, Arrays.asList(profileResponse), productCollectionUpdateByMap);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(STORE_ID, BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailForProductLiveException() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), true);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_LIVE.getName(), emailMap);
    Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap = new HashMap<>();
    ProductCollectionElement productCollectionElement = new ProductCollectionElement();
    productCollectionUpdateByMap.put(BUSINESS_PARTNER_CODE, Arrays.asList(productCollectionElement));
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
    emailNotificationService.sendEmailForProductLive(STORE_ID, Arrays.asList(profileResponse), productCollectionUpdateByMap);
    Mockito.verify(merchantEducationOutbound)
        .findByUsernameAndStoreCode(STORE_ID, BUSINESS_PARTNER_EMAIL_ADDRESS,
            BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailForProductLiveNotificationDisabled() throws Exception {
    Map<String, Boolean> emailMap = new HashMap<>();
    emailMap.put(EntityNotificationMode.EMAIL.getName(), false);
    notificationSettingsMap.put(EntityNotificationType.PRODUCT_LIVE.getName(), emailMap);
    Map<String, List<ProductCollectionElement>> productCollectionUpdateByMap = new HashMap<>();
    ProductCollectionElement productCollectionElement = new ProductCollectionElement();
    productCollectionUpdateByMap.put(BUSINESS_PARTNER_CODE, Arrays.asList(productCollectionElement));
    emailNotificationService.sendEmailForProductLive(STORE_ID, Arrays.asList(profileResponse), productCollectionUpdateByMap);
    Mockito.verify(merchantEducationOutbound).findByUsernameAndStoreCode(STORE_ID, BUSINESS_PARTNER_EMAIL_ADDRESS,
        BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendFailedRetryProductsMail() {
    List<ProductLevel3FailedEntity> failedRetryProductList = new ArrayList<>();
    ProductLevel3FailedEntity productLevel3FailedEntity = new ProductLevel3FailedEntity();
    productLevel3FailedEntity.setProductSku(PRODUCT_SKU);
    productLevel3FailedEntity.setCreatedDate(new Date());
    productLevel3FailedEntity.setUpdatedDate(new Date());
    failedRetryProductList.add(productLevel3FailedEntity);
    emailNotificationService.sendFailedRetryProductsMail(failedRetryProductList);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
    productLevel3FailedEntity.setMarkForDelete(true);
    Mockito.verify(productLevel3RetryService).updateFailedRetryProductsAfterMail(failedRetryProductList);
  }

  @Test
  public void sendFailedRetryProductsMailException() {
    List<ProductLevel3FailedEntity> failedRetryProductList = new ArrayList<>();
    ProductLevel3FailedEntity productLevel3FailedEntity = new ProductLevel3FailedEntity();
    productLevel3FailedEntity.setProductSku(PRODUCT_SKU);
    productLevel3FailedEntity.setCreatedDate(new Date());
    productLevel3FailedEntity.setUpdatedDate(new Date());
    failedRetryProductList.add(productLevel3FailedEntity);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
    emailNotificationService.sendFailedRetryProductsMail(failedRetryProductList);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendExceedActivationEmail() {
    ProductBusinessPartnerConfigRequest request = new ProductBusinessPartnerConfigRequest();
    List<ProductLevel3WipDTO > productLevel3Wips = new ArrayList<>();
    emailNotificationService.sendExceedActivationEmail(
        profileResponse, USER_NAME, request, productLevel3Wips, SUBMISSION_DATE);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendExceedActivationEmailException() {
    try {
      ProductBusinessPartnerConfigRequest request = new ProductBusinessPartnerConfigRequest();
      List<ProductLevel3WipDTO> productLevel3Wips = new ArrayList<>();
      Mockito.doThrow(RuntimeException.class).when(kafkaProducer).send(Mockito.anyString(), Mockito.any(), Mockito.any());
      Assertions.assertThrows(Exception.class, () -> {
        emailNotificationService.sendExceedActivationEmail(profileResponse, USER_NAME, request, productLevel3Wips,
            SUBMISSION_DATE);
      });
    } finally {
      verify(mandatoryParameterHelper).getStoreId();
      verify(mandatoryParameterHelper).getUsername();
      verify(mandatoryParameterHelper).getChannelId();
      verify(mandatoryParameterHelper).getRequestId();
      verify(mandatoryParameterHelper).getClientId();
      Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void sendEmailToBusinessPartnerForDeletedProducts() throws Exception {
    List<ProductCollection> productCollections = new ArrayList<>();
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductName(PRODUCT_NAME);
    productCollections.add(productCollection);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    emailNotificationService.sendEmailToBusinessPartnerForDeletedProducts(
        STORE_ID, BUSINESS_PARTNER_CODE, productCollections);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailToBusinessPartnerForDeletedProductsEmptyList() throws Exception {
    List<ProductCollection> productCollections = new ArrayList<>();
    emailNotificationService.sendEmailToBusinessPartnerForDeletedProducts(
        STORE_ID, BUSINESS_PARTNER_CODE, productCollections);
    Mockito.verify(businessPartnerRepository, times(0)).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void sendEmailToBusinessPartnerForDeletedProductsException() throws Exception {
    List<ProductCollection> productCollections = new ArrayList<>();
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductName(PRODUCT_NAME);
    productCollections.add(productCollection);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
    emailNotificationService.sendEmailToBusinessPartnerForDeletedProducts(
        STORE_ID, BUSINESS_PARTNER_CODE, productCollections);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendEmailToBusinessPartnerForDeletedProductsInternationalSeller() throws Exception {
    profileResponse.getCompany().setInternationalFlag(true);
    List<ProductCollection> productCollections = new ArrayList<>();
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setProductName(PRODUCT_NAME);
    productCollections.add(productCollection);
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    emailNotificationService.sendEmailToBusinessPartnerForDeletedProducts(
        STORE_ID, BUSINESS_PARTNER_CODE, productCollections);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(applicationProperties).getProductRejectionAlertEmailSubject();
    Mockito.verify(applicationProperties).getProductEventAlertEmailFrom();
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductStuckAlertMail() {
    List<ProductWfState> productsAboveRetryCount = new ArrayList<>();
    ProductWfState productWfState = new ProductWfState();
    productWfState.setState(STUCK_STATE);
    productWfState.setCreatedDate(new Date());
    productWfState.setCreatedDate(new Date());
    productsAboveRetryCount.add(productWfState);
    emailNotificationService.sendProductStuckAlertMail(productsAboveRetryCount, 5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductStuckAlertMailEmptyList() {
    List<ProductWfState> productsAboveRetryCount = new ArrayList<>();
    emailNotificationService.sendProductStuckAlertMail(productsAboveRetryCount, 5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendProductStuckAlertMailException() {
    List<ProductWfState> productsAboveRetryCount = new ArrayList<>();
    ProductWfState productWfState = new ProductWfState();
    productWfState.setState(STUCK_STATE);
    productWfState.setCreatedDate(new Date());
    productWfState.setCreatedDate(new Date());
    productsAboveRetryCount.add(productWfState);
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(MessageEmailRequest.class));
    emailNotificationService.sendProductStuckAlertMail(productsAboveRetryCount, 5);
    Mockito.verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(),
        Mockito.any(MessageEmailRequest.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }
}