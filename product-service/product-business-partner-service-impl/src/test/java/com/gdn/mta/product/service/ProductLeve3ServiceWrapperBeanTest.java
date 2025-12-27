package com.gdn.mta.product.service;


import com.gda.mta.product.dto.ProductL3RetryListRequest;
import com.gda.mta.product.dto.ProductL3RetryRequest;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class ProductLeve3ServiceWrapperBeanTest {

  private static final String DEFAULT_PRODUCT_CODE = "BLI-00001";
  private static final String DEFAULT_PRODUCT_SKU = "BLI-00001-00001";
  private static final int MAX_RETRY_COUNT = 3;
  private static final int MAX_RETRY_PRODUCTS = 10;
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String BUSINESS_PARTNER_CODE = "BP_CODE";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_ID = "productId";
  private String CM_MERCHANT = "CM";
  private String NON_CM_MERCHANT = "NON-CM";
  private int MONTHS_TO_ARCHIVE_FOR = 6;
  private static final String ITEM_NAME = "itemName";
  private static final String RETRY_STATUS = "FAILED";
  private static final int THRESHOLD_LIMIT_FOR_MAIL = 1;

  private CompanyDTO companyDTO = new CompanyDTO();
  private Map<String, List<List<String>>> EMAILS_MAP = new HashMap<>();
  private List<String> SKU_LIST = new ArrayList<>();
  private List<String> SKU_NAME = new ArrayList<>();
  private List<List<String>> ITEM_DATA = new ArrayList<>();
  private Long BULK_ARCHIVE_SIZE = 10L;
  private Boolean ARCHIVE_WITHOUT_SENDING_MAIL = Boolean.FALSE;
  private ProductResponse productResponse;
  private SuspensionProductRequest suspensionProductRequest;
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ProductCollection productCollection = new ProductCollection();
  private ProductLevel3FailedEntity productLevel3FailedEntity = new ProductLevel3FailedEntity();
  private List<PbpStockAlert> pbpStockAlerts = new ArrayList<>();
  private Pageable pageable;
  private ProfileResponse profileResponse = new ProfileResponse();
  private Map<String, ProfileResponse> profileResponseMap = new HashMap<>();


  @InjectMocks
  private ProductLeve3ServiceWrapperBean productLeve3ServiceWrapperBean;

  @Mock
  private ProductStockAlertRepository productStockAlertRepository;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductMailEventService productMailEventService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3RetryService productLevel3RetryService;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private EmailNotificationService emailNotificationService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setGdnSku(ITEM_SKU);
    pbpStockAlert.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pbpStockAlert.setProductName(ITEM_NAME);
    pbpStockAlerts.add(pbpStockAlert);
    pageable = PageRequest.of(0, 100);
    companyDTO.setMerchantType(CM_MERCHANT);
    companyDTO.setOfflineToOnlineFlag(Boolean.FALSE);
    profileResponse.setCompany(companyDTO);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(anyString()))
        .thenReturn(profileResponse);
    SKU_LIST.add(ITEM_SKU);
    SKU_NAME.add(ITEM_NAME);
    ITEM_DATA.add(SKU_LIST);
    ITEM_DATA.add(SKU_NAME);
    EMAILS_MAP.put(BUSINESS_PARTNER_CODE, ITEM_DATA);
    profileResponseMap.put(BUSINESS_PARTNER_CODE, profileResponse);

    productResponse = new ProductResponse();
    suspensionProductRequest = new SuspensionProductRequest();
    ProductLevel3Request productLevel3Request = new ProductLevel3Request();
    productLevel3Request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productLevel3Request.setProductCode(DEFAULT_PRODUCT_CODE);
    productLevel3Request.setProductSku(DEFAULT_PRODUCT_SKU);
    suspensionProductRequest.setProducts(Arrays.asList(productLevel3Request));

    ReflectionTestUtils.setField(productLeve3ServiceWrapperBean, "maxRetryCount", MAX_RETRY_COUNT);

    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    productBusinessPartner.setProductId(PRODUCT_ID);

    productLevel3FailedEntity.setProductSku(DEFAULT_PRODUCT_SKU);
    productLevel3FailedEntity.setRetryCount(3);
    productLevel3FailedEntity.setRetryStatus(RETRY_STATUS);
    productLevel3FailedEntity.setCreatedDate(new Date());
    productLevel3FailedEntity.setUpdatedDate(new Date());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel3Service, productStockAlertRepository, productMailEventService,
        businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(emailNotificationService);
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts() throws Exception {
    Mockito.doNothing().when(this.productMailEventService)
        .sendMailForArchivedItemSkuDueToOos(EMAILS_MAP, profileResponseMap);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            Mockito.anyString(), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    Mockito.doNothing().when(this.productLevel3Service).toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            Mockito.anyString(), Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productMailEventService).sendMailForArchivedItemSkuDueToOos(EMAILS_MAP, profileResponseMap);
    Mockito.verify(this.productLevel3Service).toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProductsFalseTest() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            eq(STORE_ID), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.productLevel3Service).toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            eq(STORE_ID), Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Service).toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_nonCMMerchant() throws Exception {
    companyDTO.setMerchantType(NON_CM_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            eq(STORE_ID), Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(
            eq(STORE_ID), Mockito.any(Date.class), Mockito.any(Pageable.class));
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_O2OFlagEnabled() throws Exception {
    companyDTO.setOfflineToOnlineFlag(Boolean.TRUE);
    profileResponse.setCompany(companyDTO);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR, ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_emptyStockAlerts() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(null);
    try {
      this.productLeve3ServiceWrapperBean
          .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
              ARCHIVE_WITHOUT_SENDING_MAIL);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.productStockAlertRepository)
          .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
              Mockito.any(Date.class), Mockito.any(Pageable.class));
    }
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_O2OFlagTrue() throws Exception {
    companyDTO.setOfflineToOnlineFlag(Boolean.TRUE);
    profileResponse.setCompany(companyDTO);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_bulkArchiveSizeCheck() throws Exception {
    BULK_ARCHIVE_SIZE = 0L;
    companyDTO.setOfflineToOnlineFlag(Boolean.TRUE);
    profileResponse.setCompany(companyDTO);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(eq(STORE_ID),
            Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    try {
      this.productLeve3ServiceWrapperBean
          .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
              ARCHIVE_WITHOUT_SENDING_MAIL);
    } catch (IllegalArgumentException e) {
    }
  }

  @Test
  public void bulkArchiveSixMonthsOldOosProducts_checkOnProductWithinPage() throws Exception {
    PbpStockAlert pbpStockAlert2 = new PbpStockAlert();
    pbpStockAlert2.setGdnSku(ITEM_SKU_2);
    pbpStockAlert2.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    pbpStockAlerts.add(pbpStockAlert2);
    BULK_ARCHIVE_SIZE = 1L;
    Mockito.doNothing().when(this.productMailEventService)
        .sendMailForArchivedItemSkuDueToOos(EMAILS_MAP, profileResponseMap);
    Mockito.when(this.productStockAlertRepository
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(Mockito.anyString(),
            Mockito.any(Date.class), Mockito.any(Pageable.class))).thenReturn(new PageImpl<>(pbpStockAlerts));
    Mockito.doNothing().when(this.productLevel3Service)
        .toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
    this.productLeve3ServiceWrapperBean
        .bulkArchiveOldOosProducts(STORE_ID, BULK_ARCHIVE_SIZE, MONTHS_TO_ARCHIVE_FOR,
            ARCHIVE_WITHOUT_SENDING_MAIL);
    Mockito.verify(this.productStockAlertRepository)
        .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(Mockito.anyString(),
            Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productMailEventService).sendMailForArchivedItemSkuDueToOos(EMAILS_MAP, profileResponseMap);
    Mockito.verify(this.productLevel3Service).toggleArchiveItem(pbpStockAlerts.get(0).getGdnSku(), Boolean.TRUE);
  }

  @Test
  public void doBulkProductSuspensionTest() throws Exception {
    Mockito.when(
        this.productLevel3Service.getProductsByProductCodeAndMerchantCode(DEFAULT_PRODUCT_CODE, BUSINESS_PARTNER_CODE))
        .thenReturn(Arrays.asList(productResponse));
    Mockito.doNothing().when(this.productLevel3Service)
        .doSuspensionProductsActions(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(suspensionProductRequest));
    this.productLeve3ServiceWrapperBean
        .doBulkProductSuspension(STORE_ID, USERNAME, Arrays.asList(suspensionProductRequest));
    Mockito.verify(productLevel3Service)
        .getProductsByProductCodeAndMerchantCode(DEFAULT_PRODUCT_CODE, BUSINESS_PARTNER_CODE);
    Mockito.verify(productLevel3Service)
        .doSuspensionProductsActions(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(suspensionProductRequest));
  }

  @Test
  public void doBulkProductSuspension_whenProductResponseEmptyTest() throws Exception {
    Mockito.when(
        this.productLevel3Service.getProductsByProductCodeAndMerchantCode(DEFAULT_PRODUCT_CODE, BUSINESS_PARTNER_CODE))
        .thenReturn(Collections.emptyList());
    Mockito.doNothing().when(this.productLevel3Service)
        .doSuspensionProductsActions(Mockito.eq(STORE_ID), Mockito.eq(USERNAME), Mockito.eq(suspensionProductRequest));
    this.productLeve3ServiceWrapperBean
        .doBulkProductSuspension(STORE_ID, USERNAME, Arrays.asList(suspensionProductRequest));
    Mockito.verify(productLevel3Service)
        .getProductsByProductCodeAndMerchantCode(DEFAULT_PRODUCT_CODE, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void retryL3CreationJobTest() throws Exception {
    productCollection.setState(ProductLevel1State.ACTIVE);
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Arrays.asList(ProductLevel3FailedEntity.builder().productSku(DEFAULT_PRODUCT_SKU).build()));
    Mockito.when(
        this.productBusinessPartnerService.findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU)))
      .thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(this.productLevel1CollectionService.findByProductId(PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(Arrays.asList(productLevel3FailedEntity));
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productBusinessPartnerService)
      .findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(this.productLevel1CollectionService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerService)
      .retryCreate(STORE_ID, productBusinessPartner.getId(), productBusinessPartner);
    Mockito.verify(this.productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
    Mockito.verify(emailNotificationService).sendFailedRetryProductsMail(Arrays.asList(productLevel3FailedEntity));
  }

  @Test
  public void retryL3CreationJob_emptyRetryListTest() throws Exception {
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Collections.EMPTY_LIST);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(Collections.EMPTY_LIST);
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
  }

  @Test
  public void retryL3CreationJob_productCollectionDeletedTest() {
    productCollection.setMarkForDelete(true);
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Arrays.asList(ProductLevel3FailedEntity.builder().productSku(DEFAULT_PRODUCT_SKU).build()));
    Mockito.when(
        this.productBusinessPartnerService.findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU)))
      .thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(this.productLevel1CollectionService.findByProductId(PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(Arrays.asList(productLevel3FailedEntity));
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productBusinessPartnerService)
      .findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(this.productLevel1CollectionService).findByProductId(PRODUCT_ID);
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID,
      DEFAULT_PRODUCT_SKU, ProductLevel3RetryStatus.OMITTED.name());
    Mockito.verify(this.productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
    Mockito.verify(emailNotificationService).sendFailedRetryProductsMail(Arrays.asList(productLevel3FailedEntity));
  }

  @Test
  public void retryL3CreationJob_productBusinessPartnerDeletedTest() {
    productBusinessPartner.setMarkForDelete(true);
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Arrays.asList(ProductLevel3FailedEntity.builder().productSku(DEFAULT_PRODUCT_SKU).build()));
    Mockito.when(
        this.productBusinessPartnerService.findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU)))
      .thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(this.productLevel1CollectionService.findByProductId(PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(Arrays.asList(productLevel3FailedEntity));
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productBusinessPartnerService)
      .findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(this.productLevel1CollectionService).findByProductId(PRODUCT_ID);
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID,
      DEFAULT_PRODUCT_SKU, ProductLevel3RetryStatus.OMITTED.name());
    Mockito.verify(this.productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
    Mockito.verify(emailNotificationService).sendFailedRetryProductsMail(Arrays.asList(productLevel3FailedEntity));
  }

  @Test
  public void retryL3CreationJob_productDeletedStateTest() {
    productCollection.setState(ProductLevel1State.DELETED);
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Arrays.asList(ProductLevel3FailedEntity.builder().productSku(DEFAULT_PRODUCT_SKU).build()));
    Mockito.when(
        this.productBusinessPartnerService.findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU)))
      .thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(this.productLevel1CollectionService.findByProductId(PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(new ArrayList<>());
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productBusinessPartnerService)
      .findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(this.productLevel1CollectionService).findByProductId(PRODUCT_ID);
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID,
      DEFAULT_PRODUCT_SKU, ProductLevel3RetryStatus.OMITTED.name());
    Mockito.verify(this.productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
  }

  @Test
  public void retryL3CreationJob_exceptionTest() throws Exception {
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenThrow(RuntimeException.class);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
        .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
        .thenReturn(new ArrayList<>());
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
  }

  @Test
  public void retryL3CreationJob_exceptionOnRetryTest() throws Exception {
    productCollection.setState(ProductLevel1State.ACTIVE);
    Mockito.doThrow(Exception.class).when(this.productBusinessPartnerService)
      .retryCreate(STORE_ID, productBusinessPartner.getId(), productBusinessPartner);
    Mockito.when(
        this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(MAX_RETRY_PRODUCTS)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS))
      .thenReturn(Arrays.asList(ProductLevel3FailedEntity.builder().productSku(DEFAULT_PRODUCT_SKU).build()));
    Mockito.when(
        this.productBusinessPartnerService.findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU)))
      .thenReturn(Arrays.asList(productBusinessPartner));
    Mockito.when(this.productLevel1CollectionService.findByProductId(PRODUCT_ID)).thenReturn(productCollection);
    Mockito.when(this.productSystemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER))
      .thenReturn(ProductSystemParameter.builder().value(String.valueOf(THRESHOLD_LIMIT_FOR_MAIL)).build());
    Mockito.when(this.productLevel3RetryService.findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT))
      .thenReturn(Arrays.asList(productLevel3FailedEntity));
    productLeve3ServiceWrapperBean.retryL3CreationJob(STORE_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
      Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_CHANNEL_ID);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.MAX_LIMIT_L3_RETRY);
    Mockito.verify(this.productLevel3RetryService).findProductsForRetryJob(STORE_ID, MAX_RETRY_COUNT, MAX_RETRY_PRODUCTS);
    Mockito.verify(this.productBusinessPartnerService)
      .findByProductSkuList(STORE_ID, Collections.singletonList(DEFAULT_PRODUCT_SKU));
    Mockito.verify(this.productLevel1CollectionService).findByProductId(PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerService)
      .retryCreate(STORE_ID, productBusinessPartner.getId(), productBusinessPartner);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.THRESHOLD_LIMIT_MAILER);
    Mockito.verify(this.productLevel3RetryService).findProductsForSendingMail(STORE_ID, MAX_RETRY_COUNT);
    Mockito.verify(this.productLevel3RetryService)
      .upsertProductLevel3FailureLog(STORE_ID, productBusinessPartner.getGdnProductSku());
    Mockito.verify(emailNotificationService).sendFailedRetryProductsMail(Arrays.asList(productLevel3FailedEntity));
  }

  @Test
  public void overrideL3RetryTest() {
    ProductL3RetryRequest productL3RetryRequest =
      ProductL3RetryRequest.builder().productSku(DEFAULT_PRODUCT_SKU).retryCount(3)
        .state(ProductLevel3RetryStatus.FAILED.name()).build();
    this.productLeve3ServiceWrapperBean.overrideL3Retry(STORE_ID, USERNAME, USERNAME,
      ProductL3RetryListRequest.builder()
        .productL3RetryRequestList(Arrays.asList(productL3RetryRequest)).build());
    Mockito.verify(this.productLevel3RetryService).updateRetryProduct(STORE_ID, DEFAULT_PRODUCT_SKU,
      3, ProductLevel3RetryStatus.FAILED.name());
  }

  @Test
  public void overrideL3Retry_exceptionTest() {
    ProductL3RetryRequest productL3RetryRequest =
      ProductL3RetryRequest.builder().productSku(DEFAULT_PRODUCT_SKU).retryCount(3)
        .state(ProductLevel3RetryStatus.FAILED.name()).build();
    Mockito.doThrow(RuntimeException.class).when(this.productLevel3RetryService)
      .updateRetryProduct(STORE_ID, DEFAULT_PRODUCT_SKU, 3, ProductLevel3RetryStatus.FAILED.name());
    this.productLeve3ServiceWrapperBean.overrideL3Retry(STORE_ID, USERNAME, USERNAME,
      ProductL3RetryListRequest.builder()
        .productL3RetryRequestList(Arrays.asList(productL3RetryRequest)).build());
    Mockito.verify(this.productLevel3RetryService).updateRetryProduct(STORE_ID, DEFAULT_PRODUCT_SKU,
      3, ProductLevel3RetryStatus.FAILED.name());
  }
}