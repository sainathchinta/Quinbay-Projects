package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepositoryBean;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ImageQcStatus;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;

public class ScheduledJobServiceImplTest {

  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE_3 = "categoryCode3";
  private static final String CATEGORY_CODE_4 = "categoryCode4";
  private static final String CATEGORY_CODE_5 = "categoryCode5";
  private static final String CATEGORY_CODE_6 = "categoryCode6";
  private static final String MERCHANT_CODE_1 = "merchantCode1";
  private static final String MERCHANT_CODE_2 = "merchantCode2";
  private static final String MERCHANT_CODE_3 = "merchantCode3";
  private static final String MERCHANT_CODE_4 = "merchantCode4";
  private static final String MERCHANT_CODE_5 = "merchantCode5";
  private static final String MERCHANT_CODE_6 = "merchantCode6";
  private static final String PRODUCT_CODE_1 = "productCode1";
  private static final String PRODUCT_CODE_2 = "productCode2";
  private static final String PRODUCT_CODE_3 = "productCode3";
  private static final String PRODUCT_CODE_4 = "productCode4";
  private static final String PRODUCT_CODE_5 = "productCode5";
  private static final String PRODUCT_CODE_6 = "productCode6";
  private static final String PRODUCT_CODE_7 = "productCode7";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "PBP";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String PRODUCT_SKU_1 = "productSku1";
  private static final String PRODUCT_SKU_2 = "productSku2";
  private static final String ORDER_BY = "orderBy";
  private static final String ORDER_IN = "orderIn";
  private static final String BACKLOG = "Backlog-";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final long TIME = Calendar.getInstance().getTimeInMillis();

  private ProductSystemParameter queryLimit;
  private ProductSystemParameter productMigrationBatchSize;
  private ProductSystemParameter productThreadCount;
  private ProductSystemParameter sleepTime;
  private ProductSystemParameter imageQcBatchSize;
  private ProductSystemParameter syncProductStartDate;
  private ProductSystemParameter syncProductEndDate;

  private ProductSystemParameter syncActiveProductStartDate;
  private ProductSystemParameter syncActiveProductEndDate;
  private ProductSystemParameter revisedEventEnabled;
  private List<String> productMigrationList;
  private ProductImageQcBacklog productImageQcBacklog;
  private ProductCollection productCollection;
  private ProductCollection productCollection1;
  private PDTProductDomainEventModel pdtProductDomainEventModel;
  private PDTProductDomainEventModel pdtProductDomainEventModel1;

  @InjectMocks
  private ScheduledJobServiceImpl scheduledJobService;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ProductMigrationWrapperService productMigrationWrapperService;

  @Mock
  private ProductMigrationService productMigrationService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductImageQcBacklogService productImageQcBacklogService;
  @Mock
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Mock
  private ProductPublisherService productPublisherService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>> configurationStatusRequestListArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductImageQcBacklog> productImageQcBacklogArgumentCaptor;

  @Captor
  ArgumentCaptor<ProductDataAutoFixHistoryListRequest> productDataAutoFixHistoryListRequestArgumentCaptor;

  @Captor
  ArgumentCaptor<AddProductToVendorCombinedEventModel> addProductToVendorCombinedEventModelArgumentCaptor;

  @Captor
  ArgumentCaptor<AddDeleteVariantRetryPublishEventModel> addDeleteVariantRetryPublishEventModelArgumentCaptor;

  private ProfileResponse profileResponse;
  private SellerDetailResponse sellerDetailResponse;
  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    sellerDetailResponse = new SellerDetailResponse();
    sellerDetailResponse.setSellerBadge("gold");
    productMigrationBatchSize = new ProductSystemParameter();
    productMigrationBatchSize.setValue("5");

    imageQcBatchSize = new ProductSystemParameter();
    imageQcBatchSize.setValue("10");

    productImageQcBacklog = new ProductImageQcBacklog();
    productImageQcBacklog.setProductCode(PRODUCT_CODE_1);

    queryLimit = new ProductSystemParameter();
    queryLimit.setValue("2");

    revisedEventEnabled = new ProductSystemParameter();
    revisedEventEnabled.setValue("true");

    productThreadCount = new ProductSystemParameter();
    productThreadCount.setValue("1");

    sleepTime = new ProductSystemParameter();
    sleepTime.setValue("100");

    productMigrationList = new ArrayList<>();
    productMigrationList.add(PRODUCT_SKU_1);
    productMigrationList.add(PRODUCT_SKU_2);

    productCollection = new ProductCollection();
    productCollection.setEdited(true);
    productCollection.setProductId("Id");
    productCollection.setResubmitCount(0);
    productCollection.setProductCode(PRODUCT_CODE_1);

    productCollection1 = new ProductCollection();
    productCollection1.setEdited(true);
    productCollection1.setResubmitCount(0);
    productCollection1.setProductCode(PRODUCT_CODE_2);

    pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel1 = new PDTProductDomainEventModel();

    syncProductStartDate = new ProductSystemParameter();
    syncProductEndDate = new ProductSystemParameter();

    syncActiveProductStartDate = new ProductSystemParameter();
    syncActiveProductStartDate.setValue("60");
    syncActiveProductEndDate = new ProductSystemParameter();
    syncActiveProductEndDate.setValue("60");

    ReflectionTestUtils.setField(scheduledJobService, "fetchPreLiveProductsSize", 50);
    profileResponse =
      ProfileResponse.builder().businessPartnerCode(MERCHANT_CODE_1).trustedSeller(false).activated(true).build();

    Mockito.when(kafkaTopicProperties.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productOutbound);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(productServiceWrapper);
    Mockito.verifyNoMoreInteractions(productMigrationWrapperService);
    Mockito.verifyNoMoreInteractions(productMigrationService);
    Mockito.verifyNoMoreInteractions(productSystemParameterService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(productPublisherService,productAnalyticsOutbound);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepositoryBean);
  }

  @Test
  public void runPostLiveConfigChangesEmptyCollectionTest() throws Exception {
    List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
    Mockito.when(productOutbound.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, TIME);
    Mockito.verify(productOutbound).getReviewConfigurationChanges(TIME);
  }

  @Test
  public void runPostLiveConfigChangesTest() throws Exception {
    List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
    ConfigurationStatusResponse configurationStatusResponse1 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_1).reviewConfig(Constants.PRE_LIVE_STATUS).build();
    ConfigurationStatusResponse configurationStatusResponse2 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_2).reviewConfig(Constants.PRE_LIVE_FLAG).build();
    ConfigurationStatusResponse configurationStatusResponse3 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_3).reviewConfig(Constants.POST_LIVE_STATUS).build();
    ConfigurationStatusResponse configurationStatusResponse4 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_4).reviewConfig(Constants.POST_LIVE_STATUS).build();
    ConfigurationStatusResponse configurationStatusResponse5 = ConfigurationStatusResponse.builder()
        .merchantCode(MERCHANT_CODE_1).reviewConfig(Constants.PRE_LIVE_FLAG).build();
    ConfigurationStatusResponse configurationStatusResponse6 = ConfigurationStatusResponse.builder()
        .merchantCode(MERCHANT_CODE_2).reviewConfig(Constants.POST_LIVE_STATUS).build();
    ConfigurationStatusResponse configurationStatusResponse7 = ConfigurationStatusResponse.builder()
        .merchantCode(MERCHANT_CODE_3).reviewConfig(Constants.NEUTRAL_STATUS).build();
    ConfigurationStatusResponse configurationStatusResponse8 = ConfigurationStatusResponse.builder()
        .merchantCode(MERCHANT_CODE_4).reviewConfig(Constants.PRE_LIVE_FLAG).build();
    ConfigurationStatusResponse configurationStatusResponse9 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_5).reviewConfig(Constants.PRE_LIVE_FLAG).build();
    configurationStatusResponseList.add(configurationStatusResponse1);
    configurationStatusResponseList.add(configurationStatusResponse2);
    configurationStatusResponseList.add(configurationStatusResponse3);
    configurationStatusResponseList.add(configurationStatusResponse4);
    configurationStatusResponseList.add(configurationStatusResponse5);
    configurationStatusResponseList.add(configurationStatusResponse6);
    configurationStatusResponseList.add(configurationStatusResponse7);
    configurationStatusResponseList.add(configurationStatusResponse8);
    configurationStatusResponseList.add(configurationStatusResponse9);
    List<ProductCollection> productCollections = new ArrayList<>();
    ProductCollection productCollection1 = new ProductCollection();
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection1.setCategoryCode(CATEGORY_CODE_1);
    productCollection1.setProductCode(PRODUCT_CODE_1);
    productCollection1.setPostLive(false);
    ProductCollection productCollection2 = new ProductCollection();
    productCollection2.setBusinessPartnerCode(MERCHANT_CODE_2);
    productCollection2.setCategoryCode(CATEGORY_CODE_2);
    productCollection2.setProductCode(PRODUCT_CODE_2);
    ProductCollection productCollection3 = new ProductCollection();
    productCollection3.setBusinessPartnerCode(MERCHANT_CODE_3);
    productCollection3.setCategoryCode(CATEGORY_CODE_3);
    productCollection3.setProductCode(PRODUCT_CODE_3);
    ProductCollection productCollection4 = new ProductCollection();
    productCollection4.setBusinessPartnerCode(MERCHANT_CODE_4);
    productCollection4.setCategoryCode(CATEGORY_CODE_4);
    productCollection4.setProductCode(PRODUCT_CODE_4);
    productCollection4.setPostLive(true);
    ProductCollection productCollection5 = new ProductCollection();
    productCollection5.setBusinessPartnerCode(MERCHANT_CODE_3);
    productCollection5.setCategoryCode(CATEGORY_CODE_6);
    productCollection5.setProductCode(PRODUCT_CODE_5);
    productCollection5.setPostLive(true);
    ProductCollection productCollection6 = new ProductCollection();
    productCollection6.setBusinessPartnerCode(MERCHANT_CODE_5);
    productCollection6.setCategoryCode(CATEGORY_CODE_5);
    productCollection6.setProductCode(PRODUCT_CODE_6);
    productCollection6.setPostLive(false);
    ProductCollection productCollection7 = new ProductCollection();
    productCollection7.setBusinessPartnerCode(MERCHANT_CODE_6);
    productCollection7.setCategoryCode(CATEGORY_CODE_6);
    productCollection7.setProductCode(PRODUCT_CODE_7);
    productCollection7.setPostLive(false);
    productCollections.add(productCollection1);
    productCollections.add(productCollection2);
    productCollections.add(productCollection3);
    productCollections.add(productCollection4);
    productCollections.add(productCollection5);
    productCollections.add(productCollection6);
    productCollections.add(productCollection7);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollections);
    Mockito.when(productOutbound.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    ConfigurationStatusRequest configurationStatusRequest1 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_3).categoryCode(CATEGORY_CODE_6).build();
    ConfigurationStatusResponse configurationStatusResponse10 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.PRE_LIVE_FLAG).build();
    Mockito.when(productOutbound.getReviewConfiguration(Collections.singletonList(configurationStatusRequest1)))
        .thenReturn(Collections.singletonList(configurationStatusResponse10));
    ConfigurationStatusRequest configurationStatusRequest2 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_5).categoryCode(CATEGORY_CODE_5).build();
    ConfigurationStatusResponse configurationStatusResponse11 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.POST_LIVE_STATUS).build();
    Mockito.when(productOutbound.getReviewConfiguration(Collections.singletonList(configurationStatusRequest2)))
        .thenReturn(Collections.singletonList(configurationStatusResponse11));
    Mockito.when(productService.getDraftProducts(
        Mockito.anyString(), Mockito.any(Pageable.class))).thenReturn(productCollectionPage);
    Mockito.when(productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(Mockito.any())).thenReturn(true);
    Mockito.doThrow(Exception.class).when(productServiceWrapper)
        .updatePostLiveFlagAndSkipScreening(Mockito.eq(productCollection2));
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_USERNAME, TIME);
    Mockito.verify(productOutbound).getReviewConfigurationChanges(TIME);
    Mockito.verify(productOutbound, times(2))
        .getReviewConfiguration(configurationStatusRequestListArgumentCaptor.capture());
    Mockito.verify(productService).getDraftProducts(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.any(Pageable.class));
    Mockito.verify(productServiceWrapper, times(3))
        .updatePostLiveFlagAndSkipScreening(productCollectionArgumentCaptor.capture());
    Mockito.verify(productServiceWrapper, times(6)).checkIfProductIsEligibleForScreeningSkip(Mockito.any());
    Assertions.assertEquals(PRODUCT_CODE_2, productCollectionArgumentCaptor.getAllValues().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE_3, productCollectionArgumentCaptor.getAllValues().get(1).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE_6, productCollectionArgumentCaptor.getAllValues().get(2).getProductCode());
  }

  @Test
  public void migrateProductsTest() throws Exception {
    Mockito.when(productMigrationWrapperService.migrateProductByProductCode(Mockito.anyString(), Mockito.eq(false))).thenReturn(1);
    scheduledJobService.migrateProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE_1, PRODUCT_CODE_2));
    Mockito.verify(productMigrationWrapperService, times(2)).migrateProductByProductCode(Mockito.anyString(), Mockito.eq(false));
  }

  @Test
  public void migrateProductsWithEmptyProductCodeListTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT)).thenReturn(queryLimit);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE))
        .thenReturn(productMigrationBatchSize);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT))
        .thenReturn(productThreadCount);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS)).thenReturn(sleepTime);
    Mockito.when(productMigrationService.findDistinctProductCodesForMigration(2))
        .thenReturn(Arrays.asList(PRODUCT_CODE_1, PRODUCT_CODE_2));
    scheduledJobService.migrateProducts(DEFAULT_STORE_ID, new ArrayList<>());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS);
    Mockito.verify(productMigrationService, times(3)).findDistinctProductCodesForMigration(2);
    Mockito.verify(productMigrationService, times(3))
        .updateProductMigrationStatusByProductCodes(Arrays.asList(PRODUCT_CODE_1, PRODUCT_CODE_2),
        MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(kafkaProducer, times(3))
        .send(Mockito.eq(DomainEventName.MIGRATE_PRODUCTS_EVENT), Mockito.any());
  }
  @Test
  public void syncRevisedProductPublishTestfornull() throws Exception {
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(null);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.NEED_CORRECTION);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewProductsContentPublishTestforNull() throws Exception {
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(null);
    revisedEventEnabled.setValue("false");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setReviewType("CONTENT");
    pdtProductDomainEventModel.setReviewType(ReviewType.IMAGE);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }
  @Test
  public void syncInReviewImagePublishProductsErrorFromPDTTestForNull() throws Exception {
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(null);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.doThrow(Exception.class).when(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_2),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void migrateProductsWithEmptyProductCodeListForProductSkusTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT)).thenReturn(queryLimit);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE))
        .thenReturn(productMigrationBatchSize);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT))
        .thenReturn(productThreadCount);
    Mockito.when(productMigrationService.findDistinctProductCodesForMigration(2)).thenReturn(new ArrayList<>());
    Mockito.when(productMigrationService.findProductForMigrationWithNullProductCode(2))
        .thenReturn(productMigrationList);
    Mockito.when(productMigrationWrapperService.migrateProductByProductSkus(productMigrationList)).thenReturn(2);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS)).thenReturn(sleepTime);
    scheduledJobService.migrateProducts(DEFAULT_STORE_ID, new ArrayList<>());
    Mockito.verify(productMigrationService, times(3))
        .updateProductMigrationStatusByProductSkus(productMigrationList, MigrationStatus.PENDING.getMigrationStatus());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS);
    Mockito.verify(productMigrationService, times(3)).findDistinctProductCodesForMigration(2);
    Mockito.verify(productMigrationService, times(3)).findProductForMigrationWithNullProductCode(2);
    Mockito.verify(kafkaProducer, times(3))
        .send(Mockito.eq(DomainEventName.MIGRATE_PRODUCTS_EVENT), Mockito.any());
  }

  @Test
  public void migrateProductsExceptionTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT)).thenReturn(queryLimit);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE))
        .thenReturn(productMigrationBatchSize);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT))
        .thenReturn(productThreadCount);
    Mockito.doThrow(RuntimeException.class).when(productMigrationService).findDistinctProductCodesForMigration(2);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS)).thenReturn(sleepTime);
    scheduledJobService.migrateProducts(DEFAULT_STORE_ID, new ArrayList<>());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS);
    Mockito.verify(productMigrationService).findDistinctProductCodesForMigration(2);
  }

  @Test
  public void retryFailedMigratedProductsProductSkuTest() throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("true");
    Mockito.doNothing().when(productMigrationWrapperService)
        .retryFailedMigratedProductsByProductCodes(Arrays.asList(PRODUCT_CODE_1));
    scheduledJobService.retryFailedMigratedProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE_1));
    Mockito.verify(productMigrationWrapperService)
        .retryFailedMigratedProductsByProductCodes(Arrays.asList(PRODUCT_CODE_1));
  }

  @Test
  public void retryFailedMigratedProductsBatchTest() throws Exception {
    ProductSystemParameter productSystemParameter1 = new ProductSystemParameter();
    productSystemParameter1.setValue("true");
    ProductSystemParameter productSystemParameter2 = new ProductSystemParameter();
    productSystemParameter2.setValue("100");
    ProductSystemParameter productSystemParameter3 = new ProductSystemParameter();
    productSystemParameter3.setValue("3");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_BATCH_SIZE))
        .thenReturn(productSystemParameter2);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_COUNT))
        .thenReturn(productSystemParameter3);
    Mockito.doNothing().when(productMigrationWrapperService)
        .retryFailedMigratedProducts(100, 3);
    scheduledJobService.retryFailedMigratedProducts(DEFAULT_STORE_ID, null);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_BATCH_SIZE);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_COUNT);
    Mockito.verify(productMigrationWrapperService)
        .retryFailedMigratedProducts(100, 3);
  }

  @Test
  public void retryFailedMigratedProductsProductSkuExceptionTest() throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("true");
    Mockito.doThrow(Exception.class).when(productMigrationWrapperService)
        .retryFailedMigratedProductsByProductCodes(Arrays.asList(PRODUCT_CODE_1));
    scheduledJobService.retryFailedMigratedProducts(DEFAULT_STORE_ID, Arrays.asList(PRODUCT_CODE_1));
    Mockito.verify(productMigrationWrapperService)
        .retryFailedMigratedProductsByProductCodes(Arrays.asList(PRODUCT_CODE_1));
  }

  @Test
  public void updateMigrationStatusTest() throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("100");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_BEFORE_MINUTES))
        .thenReturn(productSystemParameter);
    Mockito.doNothing().when(productMigrationService)
        .updateProductMigrationStatus(Mockito.any(), Mockito.any(), Mockito.any());
    scheduledJobService.updateMigrationStatus(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_BEFORE_MINUTES);
    Mockito.verify(productMigrationService, times(2))
        .updateProductMigrationStatus(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void updateMigrationStatusExceptionTest() throws Exception {
    ProductSystemParameter productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("100");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_BEFORE_MINUTES))
        .thenReturn(productSystemParameter);
    Mockito.doThrow(RuntimeException.class).when(productMigrationService)
        .updateProductMigrationStatus(Mockito.anyString(), Mockito.anyString(), Mockito.any());
    scheduledJobService.updateMigrationStatus(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_BEFORE_MINUTES);
    Mockito.verify(productMigrationService)
        .updateProductMigrationStatus(Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void publishImageQcBacklogProductsTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE)).thenReturn(imageQcBatchSize);
    scheduledJobService.publishImageQcBacklogProducts(DEFAULT_STORE_ID, ORDER_BY, ORDER_IN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE);
    Mockito.verify(productImageQcBacklogService)
        .findProductImageQcBacklogByStoreIdAndStatus(DEFAULT_STORE_ID, ImageQcStatus.PENDING.getImageQcStatus(),
            ORDER_BY, ORDER_IN, Integer.parseInt(imageQcBatchSize.getValue()));
  }

  @Test
  public void publishImageQcBacklogProductsNotEmptyTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE)).thenReturn(imageQcBatchSize);
    Mockito.when(productImageQcBacklogService
        .findProductImageQcBacklogByStoreIdAndStatus(DEFAULT_STORE_ID, ImageQcStatus.PENDING.getImageQcStatus(),
            ORDER_BY, ORDER_IN, Integer.parseInt(imageQcBatchSize.getValue())))
        .thenReturn(Collections.singletonList(productImageQcBacklog));
    Mockito.when(productImageQcBacklogService.saveProductImageQcBacklog(productImageQcBacklog))
        .thenReturn(productImageQcBacklog);
    Mockito.when(productServiceWrapper.getImageQcRequestDomainEvent(productImageQcBacklog.getProductCode()))
        .thenReturn(new ImageQcRequestDomainEvent());
    scheduledJobService.publishImageQcBacklogProducts(DEFAULT_STORE_ID, ORDER_BY, ORDER_IN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE);
    Mockito.verify(productImageQcBacklogService)
        .findProductImageQcBacklogByStoreIdAndStatus(DEFAULT_STORE_ID, ImageQcStatus.PENDING.getImageQcStatus(),
            ORDER_BY, ORDER_IN, Integer.parseInt(imageQcBatchSize.getValue()));
    Mockito.verify(productImageQcBacklogService).saveProductImageQcBacklog(productImageQcBacklog);
    Mockito.verify(productServiceWrapper).getImageQcRequestDomainEvent(productImageQcBacklog.getProductCode());
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    imageQcRequestDomainEvent.setProductCode(BACKLOG + productImageQcBacklog.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, productImageQcBacklog.getProductCode(),
            imageQcRequestDomainEvent);
  }

  @Test
  public void publishImageQcBacklogProductsFailedTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE)).thenReturn(imageQcBatchSize);
    Mockito.when(productImageQcBacklogService
        .findProductImageQcBacklogByStoreIdAndStatus(DEFAULT_STORE_ID, ImageQcStatus.PENDING.getImageQcStatus(),
            ORDER_BY, ORDER_IN, Integer.parseInt(imageQcBatchSize.getValue())))
        .thenReturn(Collections.singletonList(productImageQcBacklog));
    Mockito.when(productImageQcBacklogService.saveProductImageQcBacklog(productImageQcBacklog))
        .thenReturn(productImageQcBacklog);
    Mockito.when(productServiceWrapper.getImageQcRequestDomainEvent(productImageQcBacklog.getProductCode()))
        .thenReturn(new ImageQcRequestDomainEvent());
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer).send(Mockito.any(), Mockito.any(), Mockito.any());
    scheduledJobService.publishImageQcBacklogProducts(DEFAULT_STORE_ID, ORDER_BY, ORDER_IN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE);
    Mockito.verify(productImageQcBacklogService)
        .findProductImageQcBacklogByStoreIdAndStatus(DEFAULT_STORE_ID, ImageQcStatus.PENDING.getImageQcStatus(),
            ORDER_BY, ORDER_IN, Integer.parseInt(imageQcBatchSize.getValue()));
    Mockito.verify(productImageQcBacklogService, times(2))
        .saveProductImageQcBacklog(productImageQcBacklogArgumentCaptor.capture());
    Mockito.verify(productServiceWrapper).getImageQcRequestDomainEvent(productImageQcBacklog.getProductCode());
    ImageQcRequestDomainEvent imageQcRequestDomainEvent = new ImageQcRequestDomainEvent();
    imageQcRequestDomainEvent.setProductCode(BACKLOG + productImageQcBacklog.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, productImageQcBacklog.getProductCode(),
            imageQcRequestDomainEvent);
    Assertions.assertEquals(ImageQcStatus.FAILED.getImageQcStatus(),
        productImageQcBacklogArgumentCaptor.getAllValues().get(1).getStatus());
  }

  @Test
  public void syncInReviewImagePublishProductsErrorFromPDTTestForCatch() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    ProductAnalyticsOutbound productAnalyticsOutbound1 = mock(ProductAnalyticsOutbound.class);
    Mockito.doThrow(RuntimeException.class).when(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.doThrow(Exception.class).when(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_2),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncRevisedProductPublishTestForException() throws Exception {
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    Mockito.doThrow(RuntimeException.class).when(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.NEED_CORRECTION);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewProductsContentPublishTestForException() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.doThrow(RuntimeException.class).when(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    revisedEventEnabled.setValue("false");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setReviewType("CONTENT");
    pdtProductDomainEventModel.setReviewType(ReviewType.IMAGE);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncActiveProductsTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId("ID");
    Page<ProductCollection> productCollectionPage = new PageImpl<>(Arrays.asList(productCollection, productCollection1));
    ReflectionTestUtils.setField(scheduledJobService, "fetchActiveProductsSize", 50);
    syncActiveProductStartDate.setValue("180");
    syncActiveProductEndDate.setValue("60");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN)).thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncActiveProductEndDate);
    Mockito.when(
            productService.fetchActiveProductsToSync(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productCollectionPage);
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(DEFAULT_STORE_ID,
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    scheduledJobService.syncActiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchActiveProductsToSync(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService, Mockito.times(2))
        .findInactiveProductBusinessPartnersOfActiveL1(Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService).retryCreate(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.any(),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
  }

  @Test
  public void syncActiveProductsTest1() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId("ID");
    Page<ProductCollection> productCollectionPage =
        new PageImpl<>(Arrays.asList(productCollection), PageRequest.of(0, 1), 2L);
    Page<ProductCollection> productCollectionPage2 =
        new PageImpl<>(Arrays.asList(productCollection1), PageRequest.of(1, 1), 2L);
    ReflectionTestUtils.setField(scheduledJobService, "fetchActiveProductsSize", 1);
    syncActiveProductStartDate.setValue("180");
    syncActiveProductEndDate.setValue("60");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN)).thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncActiveProductEndDate);
    Mockito.when(productService.fetchActiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(), Mockito.any(),
        Mockito.eq(PageRequest.of(0, 1)))).thenReturn(productCollectionPage);
    Mockito.when(productService.fetchActiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID), Mockito.any(), Mockito.any(),
        Mockito.eq(PageRequest.of(1, 1)))).thenReturn(productCollectionPage2);
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(DEFAULT_STORE_ID,
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    scheduledJobService.syncActiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService, Mockito.times(2))
        .fetchActiveProductsToSync(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService, Mockito.times(2))
        .findInactiveProductBusinessPartnersOfActiveL1(Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService).retryCreate(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.any(),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
  }


  @Test
  public void syncActiveProductsExceptionTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId("ID");
    Page<ProductCollection> productCollectionPage = new PageImpl<>(Arrays.asList(productCollection, productCollection1));
    ReflectionTestUtils.setField(scheduledJobService, "fetchActiveProductsSize", 50);
    syncActiveProductStartDate.setValue("180");
    syncActiveProductEndDate.setValue("60");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN)).thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncActiveProductEndDate);
    Mockito.when(
            productService.fetchActiveProductsToSync(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productCollectionPage);
    Mockito.doThrow(RuntimeException.class).when(productBusinessPartnerService)
        .retryCreate(Mockito.anyString(), Mockito.anyString(), Mockito.any());
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(DEFAULT_STORE_ID,
        productCollection.getProductId())).thenReturn(Arrays.asList(productBusinessPartner));
    scheduledJobService.syncActiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchActiveProductsToSync(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService, Mockito.times(2))
        .findInactiveProductBusinessPartnersOfActiveL1(Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService).retryCreate(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void syncActiveProductsException1Test() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId("ID");
    Page<ProductCollection> productCollectionPage = new PageImpl<>(Arrays.asList(productCollection, productCollection1));
    ReflectionTestUtils.setField(scheduledJobService, "fetchActiveProductsSize", 50);
    syncActiveProductStartDate.setValue("180");
    syncActiveProductEndDate.setValue("60");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN)).thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncActiveProductEndDate);
    Mockito.when(
            productService.fetchActiveProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productCollectionPage);
    Mockito.when(productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(DEFAULT_STORE_ID,
        productCollection.getProductId())).thenThrow(Exception.class);
    scheduledJobService.syncActiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchActiveProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(productBusinessPartnerService, Mockito.times(1))
        .findInactiveProductBusinessPartnersOfActiveL1(Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void syncInReviewImagePublishProductsTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
      .thenReturn(pdtProductDomainEventModel1);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_2),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewImagePublishProductsErrorFromPDTTest() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.doThrow(Exception.class).when(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_2),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewNoEditEventTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    pdtProductDomainEventModel.setMarkForDelete(false);
    pdtProductDomainEventModel.setEdited(true);
    pdtProductDomainEventModel1.setReviewType(ReviewType.IMAGE);
    pdtProductDomainEventModel1.setMarkForDelete(false);
    pdtProductDomainEventModel1.setEdited(true);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);

    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
  }

  @Test
  public void syncInReviewNoEditEventTest2() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection1.setReviewType("IMAGE");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT_AND_IMAGE);
    pdtProductDomainEventModel.setMarkForDelete(false);
    pdtProductDomainEventModel.setEdited(true);
    pdtProductDomainEventModel1.setReviewType(ReviewType.IMAGE);
    pdtProductDomainEventModel1.setMarkForDelete(true);
    pdtProductDomainEventModel1.setEdited(true);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
        .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
        .thenReturn(pdtProductDomainEventModel1);
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection1.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_2),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewProductsContentPublishTest() throws Exception {
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    revisedEventEnabled.setValue("false");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setReviewType("CONTENT");
    pdtProductDomainEventModel.setReviewType(ReviewType.IMAGE);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
      .thenReturn(pdtProductDomainEventModel1);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewProductsPublishContentAndImageForPDTEmptyTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setReviewType("CONTENT");
    productCollection.setReviewType("IMAGE");
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.eq(PRODUCT_CODE_1),
            addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            productDataAutoFixHistoryListRequestArgumentCaptor.capture());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncInReviewImagePublishNoProductsTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.CONTENT_AND_IMAGE_NEED_CORRECTION);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(pdtProductDomainEventModel);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
  }

  @Test
  public void syncInReviewPDTErrorTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.CONTENT_AND_IMAGE_NEED_CORRECTION);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
        .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
        .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.doThrow(Exception.class).when(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
  }

  @Test
  public void syncInReviewProductsNeedCorrectionNoPublishTest() throws Exception {
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setReviewType("CONTENT");
    productCollection.setReviewType("IMAGE");
    productCollection1.setState("NEED_CORRECTION");
    productCollection1.setResubmitCount(1);
    productCollection1.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.CONTENT_AND_IMAGE_NEED_CORRECTION);
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_2))
      .thenReturn(pdtProductDomainEventModel);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_2);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.ADD_PRODUCT_TO_VENDOR_COMBINED_EVENT), Mockito.anyString(),
        addProductToVendorCombinedEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncRevisedProductPublishTest() throws Exception {
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.NEED_CORRECTION);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
        .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(pdtProductDomainEventModel);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
            Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void syncPreLiveProductsNoRecordsTest() {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)))).thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 50), 0));
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
  }

  @Test
  public void syncPreLiveProductsNotExistsInPDTTest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50))))
        .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 0));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(profileResponse);
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.anyString(),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productPublisherService).publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
        productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
        productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
      false, productCollection.getProductId(), productCollection);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());

  }

  @Test
  public void syncPreLiveProductsNotExistsInPDTTest_forTrustedSellers() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
      .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
      .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
          Mockito.any(),Mockito.eq(PageRequest.of(0, 50))))
      .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 0));
    Mockito.when(businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any())).thenReturn(profileResponse);
    profileResponse.setTrustedSeller(true);
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
      .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
        Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.anyString(),
      Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productPublisherService).publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
      productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
      productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
      productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
      true, productCollection.getProductId(), productCollection);
    Mockito.verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
  }

  @Test
  public void syncPreLiveProductsPDTExceptionTest() {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50))))
        .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 0));
    Mockito.doThrow(RuntimeException.class).when(productService)
        .checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
  }

  @Test
  public void syncPreLiveProductsExistsInPDTTest() {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50))))
        .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 0));
    Mockito.when(productService.checkIfProductExistsInPDT(productCollection.getProductCode(), true)).thenReturn(true);
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
  }

  @Test
  public void syncPreLiveProductsPublisherErrorTest() throws Exception {
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50))))
        .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 0));
    Mockito.doThrow(Exception.class).when(productPublisherService).publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
        productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
        productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
      false, productCollection.getProductId(), productCollection);
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 50)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    Mockito.verify(productPublisherService).publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
        productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
        productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
      false, productCollection.getProductId(), productCollection);
  }

  @Test
  public void syncPreLiveProductsNotExistsInPDTMultiplePagesTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "fetchPreLiveProductsSize", 1);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN))
        .thenReturn(syncActiveProductStartDate);
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN))
        .thenReturn(syncActiveProductEndDate);
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 1))))
        .thenReturn(new PageImpl<>(Collections.singletonList(productCollection), PageRequest.of(0, 1), 2));
    Mockito.when(productService
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(1, 1))))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(1, 1), 2));
    scheduledJobService.syncPreLiveProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(0, 1)));
    Mockito.verify(productService)
        .fetchPreLiveProductsToSync(Mockito.eq(DEFAULT_STORE_ID),Mockito.eq(IN_PROGRESS),Mockito.eq(false),Mockito.eq(false),Mockito.eq(0), Mockito.any(),
            Mockito.any(),Mockito.eq(PageRequest.of(1, 1)));
    Mockito.verify(productService).checkIfProductExistsInPDT(productCollection.getProductCode(), true);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.anyString(),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(productPublisherService).publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
        productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
        productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
        productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
      false, productCollection.getProductId(), productCollection);
  }

  @Test
  public void syncRevisedPassedStateProductPublishTest() throws Exception {
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.PASSED);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
      .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(pdtProductDomainEventModel);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
    Mockito.verify(productDistributionTaskRepositoryBean)
      .productRetryStatusUpdate(Mockito.anyString(), Mockito.any());
  }

  @Test
  public void syncRevisedProductNotPresentPublishTest() throws Exception {
    SellerDetailResponse sellerDetailResponse=new SellerDetailResponse();
    sellerDetailResponse.setSellerType("gold");
    Mockito.when(productAnalyticsOutbound.getSellerDetail(MERCHANT_CODE_1)).thenReturn(sellerDetailResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED)).thenReturn(revisedEventEnabled);
    syncProductStartDate.setValue("180");
    syncProductEndDate.setValue("60");
    productCollection.setBusinessPartnerCode(MERCHANT_CODE_1);
    productCollection.setResubmitCount(1);
    productCollection1.setEdited(false);
    productCollection.setState("IN_PROGRESS");
    pdtProductDomainEventModel.setReviewType(ReviewType.CONTENT);
    pdtProductDomainEventModel.setState(WorkflowState.PASSED);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)).thenReturn(syncProductStartDate);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)).thenReturn(syncProductEndDate);
    Mockito
      .when(productService.fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any()))
      .thenReturn(Arrays.asList(productCollection, productCollection1));
    Mockito.when(productBusinessPartnerService.findFirstByStoreIdAndProductId(any(), any()))
      .thenReturn(new ProductBusinessPartner());
    Mockito.when(productService.getPDTDomainModelResponseByCode(PRODUCT_CODE_1))
      .thenReturn(null);
    scheduledJobService.syncInReviewProducts(DEFAULT_STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN);
    Mockito.verify(productAnalyticsOutbound).getSellerDetail(MERCHANT_CODE_1);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN);
    Mockito.verify(productService)
      .fetchProductsToSync(Mockito.anyString(), Mockito.any(), Mockito.any());
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(DEFAULT_STORE_ID,
      SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    Mockito.verify(productService).getPDTDomainModelResponseByCode(PRODUCT_CODE_1);
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.any(), Mockito.anyString(), Mockito.any());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY), Mockito.eq(PRODUCT_CODE_1),
        Mockito.any(ProductDataAutoFixHistoryListRequest.class));
    Mockito.verify(kafkaTopicProperties).getVendorCombinedEventNoPriority();
  }

  @Test
  public void addDeleteVariantRetryPublishEventsTest() throws Exception {
    Mockito.when(productService.findProductsByAddDeleteVariantByPendingStatus(DEFAULT_STORE_ID, PRODUCT_CODE_1,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME)).thenReturn(Collections.singletonList(PRODUCT_CODE_1));
    scheduledJobService.addDeleteVariantRetryPublishEvents(DEFAULT_STORE_ID, PRODUCT_CODE_1,
        DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    verify(productService).findProductsByAddDeleteVariantByPendingStatus(DEFAULT_STORE_ID, PRODUCT_CODE_1,
        DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()), Mockito.eq(PRODUCT_CODE_1),
        addDeleteVariantRetryPublishEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaTopicProperties,Mockito.times(3)).getAddDeleteVariantRetryPublishEvent();
    Assertions.assertEquals(PRODUCT_CODE_1,
        addDeleteVariantRetryPublishEventModelArgumentCaptor.getValue().getProductCode());
  }

  @Test
  public void addDeleteVariantRetryPublishEventsTestEmptyProductList() throws Exception {
    scheduledJobService.addDeleteVariantRetryPublishEvents(DEFAULT_STORE_ID, PRODUCT_CODE_1,
        DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
   verify(productService).findProductsByAddDeleteVariantByPendingStatus(DEFAULT_STORE_ID, PRODUCT_CODE_1,
        DEFAULT_REQUEST_ID, DEFAULT_USERNAME);
  }

}