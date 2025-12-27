package com.gdn.x.mta.distributiontask.service.impl;

import static com.gdn.x.mta.distributiontask.model.Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS;
import static com.gdn.x.mta.distributiontask.model.Constants.INTERNAL_BUSINESS_PARTNER;
import static com.gdn.x.mta.distributiontask.model.Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION;
import static com.gdn.x.mta.distributiontask.model.Constants.PDT_RETRY_DELETE;
import static com.gdn.x.mta.distributiontask.model.Constants.PRODUCT_CODE;
import static com.gdn.x.mta.distributiontask.model.Constants.THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductDeleteEventModel;
import org.apache.commons.lang3.StringUtils;
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
import org.springframework.data.domain.SliceImpl;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationType;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductMigrationService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class ScheduledJobServiceImplTest {

  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_CODE_3 = "categoryCode3";
  private static final String CATEGORY_CODE_4 = "categoryCode4";
  private static final String CATEGORY_CODE_5 = "categoryCode5";
  private static final String CATEGORY_CODE_6 = "categoryCode6";
  private static final String CATEGORY_CODE_7 = "categoryCode7";
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
  private static final String PRODUCT_CODE_8 = "productCode8";
  private static final String PRODUCT_CODE_9 = "productCode9";
  private static final String PRODUCT_CODE_10 = "productCode10";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final int DEFAULT_NUM_OF_HOURS = 1;
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_ID_1 = "productId1";
  private static final int DAYS = 1;
  private static final int BATCH = 1;
  private static final long TIME = Calendar.getInstance().getTimeInMillis();
  private static final String NOTE = "note";
  private static final String MAX_ALLOWED_PRODUCT_AUTO_APPROVALS_VALUE = "100";
  private static final String MAX_NUMBER_OF_DAYS_TO_APPROVE = "10";
  private static final String PAGE_SIZE = "9";
  private static final Integer MAX_ALLOWED_PRODUCT_VALUE = 100;
  private static final Integer MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE = 10;
  private static final String MAX_ALLOWED_PRODUCT_AUTO_APPROVALS= "maximumAllowedProductAutoApprovals";
  private static final String MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS = "maxNumberOfDaysToApproveAssignedProducts";
  private static final String PENDING = "PENDING";
  private static final String CREATED_DATE = "createdDate";
  private static final String UPDATED_DATE = "updatedDate";
  private static final String ASC = "asc";
  private static final String DESC = "desc";
  private static final String ACTION = "AUTO_NEED_REVISION";
  private static final String ACTION_1 = "ACTION_1";
  private static final String MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES = "maxNoOfDaysToAllowConfigChanges";
  private static final String PAGE_SIZE_FOR_CONFIG_CHANGES = "pageSizeForConfigChanges";
  private static final Date PRODUCT_DATE = new Date();
  private static final String WRONG_MIGRATION_TYPE = "wrongMigrationType";
  private static final String RETRY_JOB_INELIGIBLE = "Product is ineligible for retry job";
  private static final String PDT_AUTO_REJECTION = "PDT_AUTO_REJECT";
  private static final String DELETE_PRODUCT_EVENT_TOPIC = "delete-product-event-topic";
  private static final int PRODUCT_DELETE_EVENT_PAYLOAD_SIZE = 2;


  @InjectMocks
  private ScheduledJobServiceImpl scheduledJobService;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private ProductService productService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @Mock
  private ProductAutoApprovalService productAutoApprovalService;

  @Mock
  private ProductActionRetryService productActionRetryService;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private ProductMigrationService productMigrationService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductWrapperService productWrapperService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<Boolean> booleanArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ConfigurationStatusRequest>> configurationStatusRequestListArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductActionRetry>> productActionRetryListArgumentCaptor;

  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig systemParameterConfigMaxNumberOfDaysToApprove;
  private List<ProductAutoApproval> productAutoApprovalList;
  private List<ProductActionRetry> productActionRetryList;
  private ProductActionRetry productActionRetry;
  private ProductResponse productResponse;
  private Product product;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(MAX_ALLOWED_PRODUCT_AUTO_APPROVALS_VALUE);
    systemParameterConfigMaxNumberOfDaysToApprove = new SystemParameterConfig();
    systemParameterConfigMaxNumberOfDaysToApprove.setValue(MAX_NUMBER_OF_DAYS_TO_APPROVE);
    productAutoApprovalList = new ArrayList<>();
    productActionRetryList = new ArrayList<>();
    productActionRetry = new ProductActionRetry(PRODUCT_CODE, ACTION, 1, StringUtils.EMPTY, StringUtils.EMPTY, ActionRetryStatus.PENDING);
    productActionRetry.setMarkForDelete(false);
    productActionRetryList.add(productActionRetry);
    productResponse = new ProductResponse();
    product = new Product();
    productResponse.setMarkForDelete(false);
    productResponse.setReviewPending(false);
    product.setMarkForDelete(false);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productServiceRepository);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(solrVendorCollectionService);
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(productWrapperService);
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(kafkaTopicPropertiesConsumer);
  }

  @Test
   void runPostLiveConfigChangesEmptyCollectionTest() throws Exception {
    List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
    Mockito.when(productServiceRepository.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, REQUEST_ID, USERNAME, TIME);
    Mockito.verify(productServiceRepository).getReviewConfigurationChanges(TIME);
  }

  @Test
   void runPostLiveConfigChangesEmptyProductCollectionTest() throws Exception {
    List<ConfigurationStatusResponse> configurationStatusResponseList = new ArrayList<>();
    ConfigurationStatusResponse configurationStatusResponse1 = ConfigurationStatusResponse.builder()
        .categoryCode(CATEGORY_CODE_1).reviewConfig(Constants.PRE_LIVE_STATUS).build();
    configurationStatusResponseList.add(configurationStatusResponse1);
    Mockito.when(productService.getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        Mockito.anyString(), Mockito.anyList(),
        Mockito.eq(false), Mockito.eq(false), Mockito.any(Date.class), anyInt())).thenReturn(new ArrayList<>());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    systemParameterConfig.setValue(PAGE_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        PAGE_SIZE_FOR_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    Mockito.when(productServiceRepository.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, REQUEST_ID, USERNAME, TIME);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, PAGE_SIZE_FOR_CONFIG_CHANGES);
    Mockito.verify(productServiceRepository).getReviewConfigurationChanges(TIME);
    Mockito.verify(productService).getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.anyList(),
        eq(false), eq(false), Mockito.any(Date.class), anyInt());
  }

  @Test
   void runPostLiveConfigChangesTest() throws Exception {
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
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setBusinessPartnerCode(MERCHANT_CODE_1);
    product1.setCategoryCode(CATEGORY_CODE_1);
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setBusinessPartnerCode(MERCHANT_CODE_2);
    product2.setCategoryCode(CATEGORY_CODE_2);
    product2.setProductCode(PRODUCT_CODE_2);
    Product product3 = new Product();
    product3.setBusinessPartnerCode(MERCHANT_CODE_3);
    product3.setCategoryCode(CATEGORY_CODE_3);
    product3.setProductCode(PRODUCT_CODE_3);
    Product product4 = new Product();
    product4.setBusinessPartnerCode(MERCHANT_CODE_4);
    product4.setCategoryCode(CATEGORY_CODE_4);
    product4.setProductCode(PRODUCT_CODE_4);
    Product product5 = new Product();
    product5.setBusinessPartnerCode(MERCHANT_CODE_3);
    product5.setCategoryCode(CATEGORY_CODE_6);
    product5.setProductCode(PRODUCT_CODE_5);
    Product product6 = new Product();
    product6.setBusinessPartnerCode(MERCHANT_CODE_5);
    product6.setCategoryCode(CATEGORY_CODE_5);
    product6.setProductCode(PRODUCT_CODE_6);
    Product product7 = new Product();
    product7.setBusinessPartnerCode(MERCHANT_CODE_6);
    product7.setCategoryCode(CATEGORY_CODE_6);
    product7.setProductCode(PRODUCT_CODE_7);
    Product product8 = new Product();
    product8.setBusinessPartnerCode(MERCHANT_CODE_6);
    product8.setCategoryCode(CATEGORY_CODE_7);
    product8.setProductCode(PRODUCT_CODE_8);
    product8.setCreatedDate(PRODUCT_DATE);
    Product product9 = new Product();
    product9.setBusinessPartnerCode(MERCHANT_CODE_3);
    product9.setCategoryCode(CATEGORY_CODE_3);
    product9.setProductCode(PRODUCT_CODE_9);
    Product product10 = new Product();
    product10.setBusinessPartnerCode(INTERNAL_BUSINESS_PARTNER);
    product10.setCategoryCode(CATEGORY_CODE_3);
    product10.setProductCode(PRODUCT_CODE_10);
    products.add(product1);
    products.add(product2);
    products.add(product3);
    products.add(product4);
    products.add(product9);
    products.add(product5);
    products.add(product6);
    products.add(product7);
    products.add(product8);
    Page<Product> ProductPage = new PageImpl<>(products);
    Mockito.when(productServiceRepository.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    ConfigurationStatusRequest configurationStatusRequest1 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_3).categoryCode(CATEGORY_CODE_6).build();
    ConfigurationStatusResponse configurationStatusResponse10 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.PRE_LIVE_FLAG).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest1)))
        .thenReturn(Collections.singletonList(configurationStatusResponse10));
    ConfigurationStatusRequest configurationStatusRequest2 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_5).categoryCode(CATEGORY_CODE_5).build();
    ConfigurationStatusResponse configurationStatusResponse11 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.POST_LIVE_STATUS).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest2)))
        .thenReturn(Collections.singletonList(configurationStatusResponse11));
    ConfigurationStatusRequest configurationStatusRequest3 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_6).categoryCode(CATEGORY_CODE_7).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest3)))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(productService.getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        Mockito.anyString(), Mockito.anyList(),
        Mockito.eq(false), Mockito.eq(false), Mockito.any(Date.class), anyInt())).thenReturn(ProductPage.getContent(), new ArrayList<>());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    systemParameterConfig.setValue(PAGE_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        PAGE_SIZE_FOR_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    systemParameterConfig.setValue(PAGE_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        PAGE_SIZE_FOR_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    Mockito.doThrow(new ApplicationRuntimeException())
        .when(productServiceRepository).updateProductCollectionAsPostLive(anyString(), Mockito.eq(PRODUCT_CODE_9));
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, REQUEST_ID, USERNAME, TIME);
    Mockito.verify(productServiceRepository).getReviewConfigurationChanges(TIME);
    Mockito.verify(productServiceRepository, times(2))
        .getReviewConfiguration(configurationStatusRequestListArgumentCaptor.capture());
    Mockito.verify(productService, times(2)).getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.any(),
        eq(false), eq(false), Mockito.any(), anyInt());
    Mockito.verify(productServiceRepository, times(4))
        .updateProductCollectionAsPostLive(eq(GdnMandatoryRequestParameterUtil.getStoreId()), stringArgumentCaptor.capture());
    Mockito.verify(productService, times(3))
        .updateProductAsPostLiveTrue(stringArgumentCaptor.capture());
    Mockito.verify(this.solrVendorCollectionService, times(4))
        .updatePostLiveFlag(Mockito.anyString(), eq(true));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, PAGE_SIZE_FOR_CONFIG_CHANGES);
    Assertions.assertEquals(PRODUCT_CODE_2, stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertEquals(PRODUCT_CODE_3, stringArgumentCaptor.getAllValues().get(1));
    Assertions.assertEquals(PRODUCT_CODE_9, stringArgumentCaptor.getAllValues().get(2));
    Assertions.assertEquals(PRODUCT_CODE_6, stringArgumentCaptor.getAllValues().get(3));
    Assertions.assertEquals(PRODUCT_CODE_2, stringArgumentCaptor.getAllValues().get(4));
    Assertions.assertEquals(PRODUCT_CODE_3, stringArgumentCaptor.getAllValues().get(5));
    Assertions.assertEquals(PRODUCT_CODE_6, stringArgumentCaptor.getAllValues().get(6));
  }

  @Test
   void runPostLiveConfigChangesTest1() throws Exception {
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
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setBusinessPartnerCode(MERCHANT_CODE_1);
    product1.setCategoryCode(CATEGORY_CODE_1);
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setBusinessPartnerCode(MERCHANT_CODE_2);
    product2.setCategoryCode(CATEGORY_CODE_2);
    product2.setProductCode(PRODUCT_CODE_2);
    Product product3 = new Product();
    product3.setBusinessPartnerCode(MERCHANT_CODE_3);
    product3.setCategoryCode(CATEGORY_CODE_3);
    product3.setProductCode(PRODUCT_CODE_3);
    Product product4 = new Product();
    product4.setBusinessPartnerCode(MERCHANT_CODE_4);
    product4.setCategoryCode(CATEGORY_CODE_4);
    product4.setProductCode(PRODUCT_CODE_4);
    Product product5 = new Product();
    product5.setBusinessPartnerCode(MERCHANT_CODE_3);
    product5.setCategoryCode(CATEGORY_CODE_6);
    product5.setProductCode(PRODUCT_CODE_5);
    Product product6 = new Product();
    product6.setBusinessPartnerCode(MERCHANT_CODE_5);
    product6.setCategoryCode(CATEGORY_CODE_5);
    product6.setProductCode(PRODUCT_CODE_6);
    Product product7 = new Product();
    product7.setBusinessPartnerCode(MERCHANT_CODE_6);
    product7.setCategoryCode(CATEGORY_CODE_6);
    product7.setProductCode(PRODUCT_CODE_7);
    Product product8 = new Product();
    product8.setBusinessPartnerCode(MERCHANT_CODE_6);
    product8.setCategoryCode(CATEGORY_CODE_7);
    product8.setProductCode(PRODUCT_CODE_8);
    product8.setCreatedDate(PRODUCT_DATE);
    Product product9 = new Product();
    product9.setRestrictedKeywordsPresent(true);
    product9.setBusinessPartnerCode(MERCHANT_CODE_3);
    product9.setCategoryCode(CATEGORY_CODE_3);
    product9.setProductCode(PRODUCT_CODE_9);
    Product product10 = new Product();
    product10.setBusinessPartnerCode(INTERNAL_BUSINESS_PARTNER);
    product10.setCategoryCode(CATEGORY_CODE_3);
    product10.setProductCode(PRODUCT_CODE_10);
    products.add(product1);
    products.add(product2);
    products.add(product3);
    products.add(product4);
    products.add(product9);
    products.add(product5);
    products.add(product6);
    products.add(product7);
    products.add(product8);
    Page<Product> ProductPage = new PageImpl<>(products);
    Mockito.when(productServiceRepository.getReviewConfigurationChanges(TIME)).thenReturn(configurationStatusResponseList);
    ConfigurationStatusRequest configurationStatusRequest1 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_3).categoryCode(CATEGORY_CODE_6).build();
    ConfigurationStatusResponse configurationStatusResponse10 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.PRE_LIVE_FLAG).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest1)))
        .thenReturn(Collections.singletonList(configurationStatusResponse10));
    ConfigurationStatusRequest configurationStatusRequest2 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_5).categoryCode(CATEGORY_CODE_5).build();
    ConfigurationStatusResponse configurationStatusResponse11 =
        ConfigurationStatusResponse.builder().reviewConfig(Constants.POST_LIVE_STATUS).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest2)))
        .thenReturn(Collections.singletonList(configurationStatusResponse11));
    ConfigurationStatusRequest configurationStatusRequest3 =
        ConfigurationStatusRequest.builder().businessPartnerCode(MERCHANT_CODE_6).categoryCode(CATEGORY_CODE_7).build();
    Mockito.when(productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest3)))
        .thenThrow(new ApplicationRuntimeException());
    Mockito.when(productService.getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        Mockito.anyString(), Mockito.anyList(),
        Mockito.eq(false), Mockito.eq(false), Mockito.any(Date.class), anyInt())).thenReturn(ProductPage.getContent(), new ArrayList<>());
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    systemParameterConfig.setValue(PAGE_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        PAGE_SIZE_FOR_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    systemParameterConfig.setValue(PAGE_SIZE);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        PAGE_SIZE_FOR_CONFIG_CHANGES)).thenReturn(systemParameterConfig);
    Mockito.doThrow(new ApplicationRuntimeException())
        .when(productServiceRepository).updateProductCollectionAsPostLive(anyString(), Mockito.eq(PRODUCT_CODE_9));
    scheduledJobService.runPostLiveConfigChanges(DEFAULT_STORE_ID, REQUEST_ID, USERNAME, TIME);
    Mockito.verify(productServiceRepository).getReviewConfigurationChanges(TIME);
    Mockito.verify(productServiceRepository, times(2))
        .getReviewConfiguration(configurationStatusRequestListArgumentCaptor.capture());
    Mockito.verify(productService, times(2)).getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
        eq(GdnMandatoryRequestParameterUtil.getStoreId()), Mockito.any(),
        eq(false), eq(false), Mockito.any(), anyInt());
    Mockito.verify(productServiceRepository, times(3))
        .updateProductCollectionAsPostLive(eq(GdnMandatoryRequestParameterUtil.getStoreId()), stringArgumentCaptor.capture());
    Mockito.verify(productService, times(3))
        .updateProductAsPostLiveTrue(stringArgumentCaptor.capture());
    Mockito.verify(this.solrVendorCollectionService, times(3))
        .updatePostLiveFlag(Mockito.anyString(), eq(true));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, PAGE_SIZE_FOR_CONFIG_CHANGES);
    Assertions.assertEquals(PRODUCT_CODE_2, stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertEquals(PRODUCT_CODE_3, stringArgumentCaptor.getAllValues().get(1));
  }


  @Test
   void deleteProductsTest() throws Exception {
    Pageable pageable = PageRequest.of(0, BATCH);
    Object[][] object = {{PRODUCT_ID_1, PRODUCT_CODE_1}};
    Mockito.when(productService
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(object), pageable, 1));
    Mockito.doNothing().when(productService)
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.anyList(), Mockito.anyList(), eq(1));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 1, false);
    Mockito.verify(productService)
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.eq(pageable));
    Mockito.verify(productService)
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.eq(List.of(PRODUCT_ID_1)), Mockito.eq(List.of(PRODUCT_CODE_1)), eq(1));
  }

  @Test
  void deleteProductsTest_countLessThanMaxBatchSize() throws Exception {
    Pageable pageable = PageRequest.of(0, BATCH);
    Object[][] object = {{PRODUCT_ID_1, PRODUCT_CODE_1}};
    Mockito.when(productService.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class),
            Mockito.any(Pageable.class))).thenReturn(new SliceImpl<>(Arrays.asList(object), pageable, true))
        .thenReturn(new SliceImpl<>(Arrays.asList(object), pageable, false));
    Mockito.doNothing().when(productService)
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.anyList(), Mockito.anyList(), eq(1));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 2, false);
    Mockito.verify(productService, times(2))
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(productService, times(2))
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.eq(List.of(PRODUCT_ID_1)), Mockito.eq(List.of(PRODUCT_CODE_1)),
            eq(1));
  }

  @Test
  void deleteProductsTest_countLessThanMaxBatchSize10() throws Exception {
    Pageable pageable = PageRequest.of(0, BATCH);
    Object[][] object = {{PRODUCT_ID_1, PRODUCT_CODE_1}};
    Mockito.when(productService.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class),
            Mockito.any(Pageable.class))).thenReturn(new SliceImpl<>(Arrays.asList(object), pageable, true))
        .thenReturn(new SliceImpl<>(Arrays.asList(object), pageable, false));
    Mockito.doNothing().when(productService)
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.anyList(), Mockito.anyList(), eq(1));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, false);
    Mockito.verify(productService, times(2))
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.any(Pageable.class));
    Mockito.verify(productService, times(2))
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.eq(List.of(PRODUCT_ID_1)), Mockito.eq(List.of(PRODUCT_CODE_1)),
            eq(1));
  }

  @Test
   void deleteProductsEmptyContentTest() throws Exception {
    Pageable pageable = PageRequest.of(0, BATCH);
    Mockito.when(productService
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl<>(new ArrayList(), pageable, 0));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 2, false);
    Mockito.verify(productService)
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.eq(pageable));
  }

  @Test
   void deleteProductsExceptionTest() throws Exception {
    Pageable pageable = PageRequest.of(0, BATCH);
    Object[][] object = {{PRODUCT_ID_1, PRODUCT_CODE_1}};
    Mockito.when(productService
        .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new PageImpl<>(Arrays.asList(object), pageable, 1));
    Mockito.doThrow(Exception.class).when(productService)
        .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.anyList(), Mockito.anyList(), eq(1));
    try {
      scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 2, false);
    } finally {
      Mockito.verify(productService)
          .findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(Mockito.any(Date.class), Mockito.eq(pageable));
      Mockito.verify(productService)
          .deleteProducts(eq(DEFAULT_STORE_ID), Mockito.eq(List.of(PRODUCT_ID_1)), Mockito.eq(
                  List.of(PRODUCT_CODE_1)), eq(1));
    }
  }

  @Test
  void deleteProductsEventDeleteFlowTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize", PRODUCT_DELETE_EVENT_PAYLOAD_SIZE);
    Pageable pageable = PageRequest.of(0, BATCH);
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE_2);
    List<Product> productList = Arrays.asList(product1, product2);
    Mockito.when(productService
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(productList, pageable, false));
    Mockito.doNothing().when(productService).saveBulkProducts(Mockito.anyList());
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(DELETE_PRODUCT_EVENT_TOPIC);
    Mockito.doNothing().when(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, true);
    Mockito.verify(productService)
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.eq(pageable));
    Mockito.verify(productService).saveBulkProducts(productList);
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteProductEvent();
    Mockito.verify(kafkaProducer).send(eq(DELETE_PRODUCT_EVENT_TOPIC), eq(PRODUCT_CODE_1), Mockito.any(ProductDeleteEventModel.class));
    Assertions.assertTrue(product1.isPickedForDeletion());
    Assertions.assertTrue(product2.isPickedForDeletion());
  }

  @Test
  void deleteProductsEventDeleteFlowEmptyContentTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize", PRODUCT_DELETE_EVENT_PAYLOAD_SIZE);
    Pageable pageable = PageRequest.of(0, BATCH);
    Mockito.when(productService
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(new ArrayList<>(), pageable, false));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, true);
    Mockito.verify(productService)
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.eq(pageable));
    Mockito.verify(productService, Mockito.never()).saveBulkProducts(Mockito.anyList());
    Mockito.verify(kafkaProducer, Mockito.never()).send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    Assertions.assertFalse(Mockito.mockingDetails(productService).getInvocations().stream()
        .anyMatch(invocation -> invocation.getMethod().getName().equals("saveBulkProducts")));
  }

  @Test
  void deleteProductsEventDeleteFlowExceptionTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize", PRODUCT_DELETE_EVENT_PAYLOAD_SIZE);
    Pageable pageable = PageRequest.of(0, BATCH);
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Mockito.when(productService
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(Arrays.asList(product1), pageable, false));
    Mockito.doThrow(Exception.class).when(productService).saveBulkProducts(Mockito.anyList());
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, true);
    Mockito.verify(productService)
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class), Mockito.eq(pageable));
    Mockito.verify(productService).saveBulkProducts(Mockito.anyList());
    Mockito.verify(kafkaProducer, Mockito.never()).send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    Assertions.assertEquals(0, Mockito.mockingDetails(kafkaProducer).getInvocations().stream()
        .filter(invocation -> invocation.getMethod().getName().equals("send")).count());
  }

  @Test
  void deleteProductsEventDeleteFlowMultipleIterationsTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize",
        PRODUCT_DELETE_EVENT_PAYLOAD_SIZE);
    Pageable pageable1 = PageRequest.of(0, BATCH);
    Pageable pageable2 = PageRequest.of(1, BATCH);
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE_2);
    Mockito.when(productService.findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(
            Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(Arrays.asList(product1), pageable1, true))
        .thenReturn(new SliceImpl<>(Arrays.asList(product2), pageable2, false));
    Mockito.doNothing().when(productService).saveBulkProducts(Mockito.anyList());
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(DELETE_PRODUCT_EVENT_TOPIC);
    Mockito.doNothing().when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, true);
    Mockito.verify(productService, times(2))
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class),
            Mockito.any(Pageable.class));
    Mockito.verify(productService, times(2)).saveBulkProducts(Mockito.anyList());
    Mockito.verify(kafkaProducer, times(2))
        .send(eq(DELETE_PRODUCT_EVENT_TOPIC), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    Mockito.verify(kafkaTopicPropertiesConsumer, times(2)).getDeleteProductEvent();
    Assertions.assertEquals(2, Mockito.mockingDetails(productService).getInvocations().stream()
        .filter(invocation -> invocation.getMethod().getName().equals("saveBulkProducts")).count());
  }

  @Test
  void deleteProductsEventDeleteFlowMaxBatchSizeReachedTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize",
        PRODUCT_DELETE_EVENT_PAYLOAD_SIZE);
    Pageable pageable1 = PageRequest.of(0, BATCH);
    Pageable pageable2 = PageRequest.of(1, BATCH);
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE_2);
    Mockito.when(productService.findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(
            Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(Arrays.asList(product1), pageable1, true))
        .thenReturn(new SliceImpl<>(Arrays.asList(product2), pageable2, true));
    Mockito.doNothing().when(productService).saveBulkProducts(Mockito.anyList());
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(DELETE_PRODUCT_EVENT_TOPIC);
    Mockito.doNothing().when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 1, true);
    Mockito.verify(productService)
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class),
            Mockito.eq(pageable1));
    Mockito.verify(productService).saveBulkProducts(Arrays.asList(product1));
    Mockito.verify(kafkaProducer)
        .send(eq(DELETE_PRODUCT_EVENT_TOPIC), eq(PRODUCT_CODE_1), Mockito.any(ProductDeleteEventModel.class));
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteProductEvent();
    Assertions.assertEquals(1, Mockito.mockingDetails(productService).getInvocations().stream()
        .filter(invocation -> invocation.getMethod().getName().equals("saveBulkProducts")).count());
  }

  @Test
  void deleteProductsEventDeleteFlowPartitionTest() throws Exception {
    ReflectionTestUtils.setField(scheduledJobService, "productDeleteEventPayloadSize", 1);
    Pageable pageable = PageRequest.of(0, BATCH);
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE_2);
    List<Product> productList = Arrays.asList(product1, product2);
    Mockito.when(productService.findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(
            Mockito.any(Date.class), Mockito.any(Pageable.class)))
        .thenReturn(new SliceImpl<>(productList, pageable, false));
    Mockito.doNothing().when(productService).saveBulkProducts(Mockito.anyList());
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(DELETE_PRODUCT_EVENT_TOPIC);
    Mockito.doNothing().when(kafkaProducer)
        .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    scheduledJobService.deleteProducts(DEFAULT_STORE_ID, DAYS, BATCH, 10, true);
    Mockito.verify(productService).saveBulkProducts(productList);
    Mockito.verify(kafkaProducer, times(2))
        .send(eq(DELETE_PRODUCT_EVENT_TOPIC), Mockito.anyString(), Mockito.any(ProductDeleteEventModel.class));
    Mockito.verify(kafkaTopicPropertiesConsumer, times(2)).getDeleteProductEvent();
    Mockito.verify(productService)
        .findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(Mockito.any(Date.class),
            Mockito.any(Pageable.class));
    Assertions.assertTrue(product1.isPickedForDeletion());
    Assertions.assertTrue(product2.isPickedForDeletion());
  }

  @Test
   void autoApprovePendingProductsOrderByUpdatedDateDescTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    when(productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productAutoApprovalList);
    scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, UPDATED_DATE, DESC, PENDING);
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    verify(productAutoApprovalService).findProductsToAutoApprovalOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoApprovePendingProducts(DEFAULT_STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
  }

  @Test
   void autoApprovePendingProductsOrderByUpdatedDateAscTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    when(productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productAutoApprovalList);
    scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, UPDATED_DATE, ASC, PENDING);
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    verify(productAutoApprovalService).findProductsToAutoApprovalOrderByUpdatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoApprovePendingProducts(DEFAULT_STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
  }

  @Test
   void autoApprovePendingProductsOrderByCreatedDateDescTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    when(productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productAutoApprovalList);
    scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, CREATED_DATE, DESC, PENDING);
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    verify(productAutoApprovalService).findProductsToAutoApprovalOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoApprovePendingProducts(DEFAULT_STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
  }

  @Test
   void autoApprovePendingProductsOrderByCreatedDateAscTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    when(productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productAutoApprovalList);
    scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, CREATED_DATE, ASC, PENDING);
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    verify(productAutoApprovalService).findProductsToAutoApprovalOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoApprovePendingProducts(DEFAULT_STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
  }

  @Test
   void autoApprovePendingProductsErrorStatusTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS)).thenReturn(systemParameterConfigMaxNumberOfDaysToApprove);
    when(productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productAutoApprovalList);
    scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, CREATED_DATE, ASC, "");
    verify(systemParameterConfigService, times(2)).findValueByStoreIdAndVariable(eq(DEFAULT_STORE_ID), anyString());
    verify(productAutoApprovalService).findProductsToAutoApprovalOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        AutoApprovalStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoApprovePendingProducts(DEFAULT_STORE_ID, productAutoApprovalList, MAX_NUMBER_OF_DAYS_TO_APPROVE_VALUE);
  }

  @Test
   void autoApprovePendingProductsTestException() throws Exception {
    this.systemParameterConfig.setValue(NOTE);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAX_ALLOWED_PRODUCT_AUTO_APPROVALS)).thenReturn(systemParameterConfig);
    try {
      scheduledJobService.autoApprovePendingProducts(DEFAULT_STORE_ID, CREATED_DATE, ASC, PENDING);
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
    }
  }

  @Test
   void retryProductsByActionOrderByUpdatedDateDescAutoRejectTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
            THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
           PDT_AUTO_REJECTION , ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, UPDATED_DATE, DESC, PDT_AUTO_REJECTION);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
            THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
            PDT_AUTO_REJECTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productWrapperService).pdtAutoRejectForPendingProducts(productActionRetryList);
  }

  @Test
   void retryProductsByActionOrderByUpdatedDateDescTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, UPDATED_DATE, DESC, ACTION);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByUpdatedDateDesc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoNeedReviseForPendingProducts(DEFAULT_STORE_ID, productActionRetryList);
  }

  @Test
   void retryProductsByActionOrderByUpdatedDateAscTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByUpdatedDateAsc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, UPDATED_DATE, ASC, ACTION);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByUpdatedDateAsc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoNeedReviseForPendingProducts(DEFAULT_STORE_ID, productActionRetryList);
  }

  @Test
   void retryProductsByActionOrderByCreatedDateDescTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, DESC, ACTION);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateDesc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoNeedReviseForPendingProducts(DEFAULT_STORE_ID, productActionRetryList);
  }

  @Test
   void retryProductsByActionOrderByCreatedDateAscTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, ACTION);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        ACTION, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productService).autoNeedReviseForPendingProducts(DEFAULT_STORE_ID, productActionRetryList);
  }
  @Test
   void retryProductsByActionOrderByCreatedDateAscActionTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(productResponse);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
    verify(productWrapperService).removeProductAndDeleteOriginalImages(any());
    verify(kafkaProducer).send(eq(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY),anyString(),
       any(ProductDataAutoFixHistoryListRequest.class));
  }

  @Test
   void retryProductsByActionOrderByCreatedDateAscActionExceptionTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(null);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
    verify(productActionRetryService).saveProductActionRetryList(productActionRetryListArgumentCaptor.capture());
    Assertions.assertEquals(productActionRetryListArgumentCaptor.getValue().get(0).getStatus(),
        ActionRetryStatus.FAILED);
  }
  @Test
   void retryProductsByActionOrderByCreatedDateAscActionProductMfdTrueTest() throws Exception{
    product.setMarkForDelete(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(productResponse);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
    verify(productActionRetryService).saveProductActionRetryList(productActionRetryListArgumentCaptor.capture());
    Assertions.assertEquals(1, productActionRetryListArgumentCaptor.getValue().size());
    Assertions.assertEquals(ActionRetryStatus.SUCCESS,
        productActionRetryListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(RETRY_JOB_INELIGIBLE,
        productActionRetryListArgumentCaptor.getValue().get(0).getData());
  }
  @Test
   void retryProductsByActionOrderByCreatedDateAscActionProductResponseReviewPendingTrueTest() throws Exception{
    productResponse.setReviewPending(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(productResponse);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
  }

  @Test
   void retryProductsByActionOrderByCreatedDateAscActionProductResponseReviewPendingTrueProductMfdTrueTest()
      throws Exception {
    productResponse.setReviewPending(true);
    product.setMarkForDelete(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(productResponse);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
  }

  @Test
   void retryProductsByActionOrderByCreatedDateAscActionResponseReviewPending_ProductMfdTruePRAMfdTrueTest()
      throws Exception {
    productResponse.setReviewPending(true);
    product.setMarkForDelete(true);
    productActionRetry.setMarkForDelete(true);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    when(productServiceRepository.getProductBasicDetailByProductCode(anyString())).thenReturn(productResponse);
    when(productRepository.findByProductCode(anyString())).thenReturn(product);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PDT_RETRY_DELETE);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        PDT_RETRY_DELETE, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
    verify(productServiceRepository).getProductBasicDetailByProductCode(anyString());
    verify(productRepository).findByProductCode(anyString());
  }



  @Test
   void retryProductsByActionOrderByCreatedDateAscDiffActionTest() throws Exception{
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    when(productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        ACTION_1, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE)).thenReturn(productActionRetryList);
    scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, ACTION_1);
    verify(systemParameterConfigService, times(1)).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    verify(productActionRetryService).findProductsToRetryActionOrderByCreatedDateAsc(DEFAULT_STORE_ID,
        ACTION_1, ActionRetryStatus.PENDING, MAX_ALLOWED_PRODUCT_VALUE);
  }

  @Test
   void retryProductsByActionTestException() throws Exception {
    this.systemParameterConfig.setValue(NOTE);
    when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY)).thenReturn(systemParameterConfig);
    try {
      scheduledJobService.retryProductsByAction(DEFAULT_STORE_ID, CREATED_DATE, ASC, PENDING);
    } finally {
      verify(systemParameterConfigService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
          THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
    }
  }

  @Test
   void addProductsForAutoApprovalOnConfigChangeTest() {
    Mockito.doNothing().when(productAutoApprovalService).processPendingAutoQcConfigChange(DEFAULT_STORE_ID);
    scheduledJobService.addProductsForAutoApprovalOnConfigChange(DEFAULT_STORE_ID);
    Mockito.verify(productAutoApprovalService).processPendingAutoQcConfigChange(DEFAULT_STORE_ID);
  }

  @Test
   void publishCommonImageMigrationRecordsTest() {
    SystemParameterConfig maxNumberOfRecordsToFetch = new SystemParameterConfig();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameterConfig batchSize = new SystemParameterConfig();
    batchSize.setValue("1");
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(
        productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyInt())).thenReturn(List.of(commonImageMigration));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS)).thenReturn(batchSize);
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID,
        ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), new ProductCodeListRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationMigrationByStoreIdAndStatus(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2))
        .saveProductMigration(commonImageMigration);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS);
  }

  @Test
   void publishCommonImageMigrationRecordsEmptyProductsListTest() {
    SystemParameterConfig maxNumberOfRecordsToFetch = new SystemParameterConfig();
    maxNumberOfRecordsToFetch.setValue("2");
    SystemParameterConfig batchSize = new SystemParameterConfig();
    batchSize.setValue("1");
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(
            productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyInt()))
        .thenReturn(List.of(commonImageMigration));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION)).thenReturn(maxNumberOfRecordsToFetch);
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID,
        BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS)).thenReturn(batchSize);
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), new ProductCodeListRequest());
    Mockito.verify(productMigrationService, Mockito.times(2))
        .findProductMigrationMigrationByStoreIdAndStatus(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), 1);
    Mockito.verify(productMigrationService, Mockito.times(2))
        .saveProductMigration(commonImageMigration);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS);
  }

  @Test
   void publishCommonImageMigrationRecordsNonEmptyProductTest() {
    ProductCodeListRequest productCodeListRequest = new ProductCodeListRequest();
    productCodeListRequest.setProductList(List.of(PRODUCT_CODE));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    Mockito.when(productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyList())).thenReturn(List.of(commonImageMigration));
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), productCodeListRequest);
    Mockito.verify(productMigrationService)
        .findProductMigrationByProductCodes(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
            List.of(PRODUCT_CODE));
    Mockito.verify(productMigrationService, times(2)).saveProductMigration(commonImageMigration);
  }

  @Test
   void publishCommonImageMigrationRecordsNonEmptyProductListTest() {
    ProductCodeListRequest productCodeListRequest = new ProductCodeListRequest();
    productCodeListRequest.setProductList(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    List<ProductMigration> productMigrationList = new ArrayList<>();
    productMigrationList.add(commonImageMigration);
    Mockito.when(
            productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyList()))
        .thenReturn(productMigrationList);
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(), productCodeListRequest);
    Mockito.verify(productMigrationService)
        .findProductMigrationByProductCodes(DEFAULT_STORE_ID, ProductMigrationType.COMMON_IMAGE_MIGRATION.name(),
            Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    Mockito.verify(productMigrationService, times(2)).saveProductMigration(commonImageMigration);
  }

  @Test
   void publishCommonImageMigrationRecordsNonEmptyProductListWrongMigrationTypeTest() {
    ProductCodeListRequest productCodeListRequest = new ProductCodeListRequest();
    productCodeListRequest.setProductList(Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
    ProductMigration commonImageMigration = new ProductMigration();
    commonImageMigration.setProductCode(PRODUCT_CODE);
    commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
    List<ProductMigration> productMigrationList = new ArrayList<>();
    productMigrationList.add(commonImageMigration);
    Mockito.when(
        productMigrationService.findProductMigrationByProductCodes(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList())).thenReturn(productMigrationList);
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID, WRONG_MIGRATION_TYPE,
        productCodeListRequest);
    Mockito.verify(productMigrationService)
        .findProductMigrationByProductCodes(DEFAULT_STORE_ID, WRONG_MIGRATION_TYPE,
            Arrays.asList(PRODUCT_CODE, PRODUCT_CODE_1));
  }

  @Test
   void publishCommonImageMigrationRecordsEmptyProductsListWrongMigrationTypeTest() {
    scheduledJobService.publishPendingProductMigrationRecords(DEFAULT_STORE_ID, WRONG_MIGRATION_TYPE,
        new ProductCodeListRequest());
  }

  @Test
   void syncNeedCorrectionProductsWhenPbpStatusActiveTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productPage);
    Mockito.when(
        pbpFeign.getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse("ACTIVE"), REQUEST_ID));
    scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    Mockito.verify(productService)
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(pbpFeign, times(2))
        .getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void syncNeedCorrectionProductsWhenPbpStatusInProgressTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productPage);
    Mockito.when(
        pbpFeign.getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse("IN_PROGRESS"), REQUEST_ID));
    scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    Mockito.verify(productService)
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(pbpFeign, times(2))
        .getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void syncNeedCorrectionProductsNullTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(null);
    try{
      scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    }catch (Exception e){
      Assertions.assertTrue(
          e.getMessage().contains("Error while reconciliation of active products"));
    }
    Mockito.verify(productService)
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
   void syncNeedCorrectionProductsWhenPbpStatusInNeedCorrectionTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productPage);
    Mockito.when(
        pbpFeign.getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse("NEED_CORRECTION"), REQUEST_ID));
    scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    Mockito.verify(productService)
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(pbpFeign, times(2))
        .getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void syncNeedCorrectionProductsTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products, PageRequest.of(0, 1), 2);
    Page<Product> secondProductPage = new PageImpl<>(products, PageRequest.of(1, 1), 2);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productPage).thenReturn(secondProductPage);
    Mockito.when(
        pbpFeign.getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new SimpleStringResponse("NEED_CORRECTION"), REQUEST_ID));
    scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    Mockito.verify(productService, times(2))
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(pbpFeign, times(4))
        .getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void syncNeedCorrectionProductsExceptionTest() {
    ReflectionTestUtils.setField(scheduledJobService, "needCorrectionProductSizeForPagination", 1);
    List<Product> products = new ArrayList<>();
    Product product1 = new Product();
    product1.setProductCode(PRODUCT_CODE_1);
    Product product2 = new Product();
    product2.setProductCode(PRODUCT_CODE);
    products.add(product1);
    products.add(product2);
    Page<Product> productPage = new PageImpl<>(products);
    Mockito.when(
            productService.fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(productPage);
    Mockito.when(
        pbpFeign.getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString())).thenThrow(ApplicationRuntimeException.class);
    try{
      scheduledJobService.syncNeedCorrectionProducts(DEFAULT_STORE_ID, DEFAULT_NUM_OF_HOURS);
    }catch (Exception e){
      Assertions.assertTrue(
          e.getMessage().contains("Error while fetching product from PBP where productCode"));
    }
    Mockito.verify(productService)
        .fetchNeedCorrectionProducts(Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(pbpFeign, times(2))
        .getProductStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

}
