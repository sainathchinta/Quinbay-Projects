package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.product.dao.api.ProductRetryEventPublishRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeltaReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.rest.web.model.dto.ProductScoreRetryDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.service.api.ArchiveAndDeleteRejectedMerchantProductDataService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.event.listener.AdjustmentProductChangeListenerV2;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.promotion.domain.event.config.PromotionDomainEventName;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.google.common.collect.ImmutableSet;

public class ProductRetryEventPublishServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String ITEM_SKU = "item-123";
  private static final String ITEM_SKU_2 = "item-456";
  private static final String MESSAGE = "message";
  private static final String MESSAGE_SCORE =
      "{\"storeId\":\"10001\", \"productSku\":\"productSku\", \"productCode\":null}";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String CAMPAIGN_CODE = "campaign code";
  private static final Date CAMPAIGN_START_DATE = new Date();
  private static final Date CAMPAIGN_END_DATE = new Date(CAMPAIGN_START_DATE.getTime() + 10000);
  private static final String ADJUSTMENT_NAME = "flash sale";
  private static final String PICKUP_POINT_CODE = "pp-code";
  private static final String DELETE_REJECTED_MERCHANT_PRODUCT =
      "{\"storeId\":10001,\"productCode\":\"MTA-1037142\", \"timestamp\":0}";
  private static final String PRODUCT_CODE = "productCode";
  private static final String X_PRODUCT_RETRY_REQUEST = "x-product-retry";
  private static final String MASTER_SKU = "masterSku";
  private static final String MASTER_SKU_MAPPING =
      "{\"storeId\":10001,\"itemSku\":\"item-123\", \"masterItemSku\":\"masterSku\"}";

  private ProductRetryEventPublish productRetryEventPublish = new ProductRetryEventPublish();
  private ProductRetryEventPublish productRetryEventPublish1 = new ProductRetryEventPublish();
  private ProductRetryEventPublish productRetryEventPublish2 = new ProductRetryEventPublish();
  private ProductRetryEventPublish productRetryEventPublish3 = new ProductRetryEventPublish();
  private ProductRetryEventPublish productRetryEventPublish4 = new ProductRetryEventPublish();
  private ProductRetryEventPublish productRetryEventPublish5 = new ProductRetryEventPublish();
  private List<ProductRetryEventPublish> productRetryEventPublishes = new ArrayList<>();
  private Item item = new Item();
  private AdjustmentProductChange adjustmentV2;
  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();

  private Product product;

  @InjectMocks
  private ProductRetryEventPublishServiceImpl productRetryEventPublishService;

  @Mock
  private ProductRetryEventPublishRepository productRetryEventPublishRepository;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private ArchiveAndDeleteRejectedMerchantProductDataService archiveAndDeleteRejectedMerchantProductData;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @Mock
  private AdjustmentProductChangeListenerV2 adjustmentProductChangeListenerV2;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private PromotionOutbound promotionOutbound;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Captor
  private ArgumentCaptor<ProductRetryEventPublish> productRetryEventPublishArgumentCaptor;


  @BeforeEach
  public void init() throws Exception {
    openMocks(this);
    ReflectionTestUtils.setField(productRetryEventPublishService, "maxRetryPublish", 3);
    ReflectionTestUtils
        .setField(productRetryEventPublishService, "sendFailedRetryProductSummaryEmailAddress", "abc.gmail.com");
    Mockito.when(this.systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE, "20", "RETRY_PUBLISH_BATCH_SIZE"));
    productRetryEventPublish =
        ProductRetryEventPublish.builder().identifier(ITEM_SKU).topicName(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME)
            .retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0).clearCache(true).build();
    productRetryEventPublish1 = ProductRetryEventPublish.builder().identifier(ITEM_SKU_2)
        .topicName(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME).retryPublishStatus(RetryPublishStatus.PENDING)
        .retryCount(3).clearCache(false).build();
    productRetryEventPublish2 = ProductRetryEventPublish.builder().identifier(ITEM_SKU)
        .topicName(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME).retryPublishStatus(RetryPublishStatus.PENDING)
        .retryCount(3).clearCache(false).build();
    productRetryEventPublish3 =
      ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING)
        .retryCount(0).identifier(MESSAGE)
        .topicName(PromotionDomainEventName.ADJUSTMENT_PRODUCT_CHANGE_EVENT_NAME_V2)
        .clearCache(Boolean.FALSE).build();
    productRetryEventPublish4 =  ProductRetryEventPublish.builder().identifier(PRODUCT_SKU)
      .topicName(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR).retryPublishStatus(RetryPublishStatus.PENDING)
      .retryCount(0).clearCache(true).build();
    adjustmentV2 =
      AdjustmentProductChange.builder().productSku(ITEM_SKU).campaignCode(CAMPAIGN_CODE)
        .startDate(CAMPAIGN_START_DATE).endDate(CAMPAIGN_END_DATE).adjustmentName(ADJUSTMENT_NAME)
        .priority(0).build();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    productRetryEventPublishes.add(productRetryEventPublish);
    productRetryEventPublishes.add(productRetryEventPublish1);
    productRetryEventPublishes.add(productRetryEventPublish2);
    productRetryEventPublishes.add(productRetryEventPublish3);
    productRetryEventPublishes.forEach(event -> {
      event.setCreatedDate(new Date());
      event.setUpdatedDate(new Date());
    });
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(productRetryEventPublishes);
    item.setMarkForDelete(true);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(new Item());
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2)).thenReturn(item);
    doNothing().when(cacheEvictHelperService).evictItemData(eq(STORE_ID), any());
    Mockito.when(this.objectMapper.writeValueAsString(Mockito.any(ProductRetryEventPublish.class)))
      .thenReturn(MESSAGE);
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(any(AdjustmentProductChange.class))).thenReturn(itemPickupPoint);
    product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    product.setPickupPointCodes(ImmutableSet.of(PICKUP_POINT_CODE));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productRetryEventPublishRepository);
    verifyNoMoreInteractions(systemParameterService);
    verifyNoMoreInteractions(itemService);
    verifyNoMoreInteractions(cacheEvictHelperService, saveAndPublishService);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(adjustmentProductChangeListenerV2);
    verifyNoMoreInteractions(productService);
    verifyNoMoreInteractions(archiveAndDeleteRejectedMerchantProductData);
    verifyNoMoreInteractions(promotionOutbound);
    verifyNoMoreInteractions(objectConverterService);
  }

  @Test
  public void schedulerRetryPublish() throws Exception {
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2);
    verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), any());
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(saveAndPublishService, times(2)).publishListOfItems(anyList());
    verify(productRetryEventPublishRepository, times(3)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void schedulerRetryPublishForProductScore() throws Exception {
    productRetryEventPublish4 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(MESSAGE_SCORE).topicName(DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME)
            .clearCache(Boolean.FALSE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish4)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(ProductScoreRetryDTO.class)))
      .thenReturn(
        ProductScoreRetryDTO.builder().storeId(STORE_ID).productCode(null).productSku(PRODUCT_SKU)
          .updateCategory(false).userName("").requestId("").build());
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(ProductScoreRetryDTO.class));
    verify(productService).generateProductScoreByProductSku(STORE_ID, PRODUCT_SKU, null,"",""
      ,false, null);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void schedulerRetryPublishForProductScoreException() throws Exception {
    productRetryEventPublish4 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(MESSAGE_SCORE).topicName(DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME)
            .clearCache(Boolean.FALSE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish4)));
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish4)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(ProductScoreRetryDTO.class)))
        .thenThrow(ApplicationRuntimeException.class);
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(ProductScoreRetryDTO.class));
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void schedulerRetryPublishDeleteRejectedMerchantProductTest() throws Exception {
    productRetryEventPublish5 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(DELETE_REJECTED_MERCHANT_PRODUCT)
            .topicName(ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT).clearCache(Boolean.FALSE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish5)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(ProductCodeDomainEventModel.class)))
        .thenReturn(new ProductCodeDomainEventModel(STORE_ID, PRODUCT_CODE));
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(ProductCodeDomainEventModel.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
    verify(archiveAndDeleteRejectedMerchantProductData).archiveAndDeleteRejectedMerchantProductDataRetry(Mockito.any());
  }

  @Test
  public void schedulerRetryPublishDeleteRejectedMerchantProductException() throws Exception {
    productRetryEventPublish5 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(DELETE_REJECTED_MERCHANT_PRODUCT)
            .topicName(ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT).clearCache(Boolean.FALSE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish5)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(ProductCodeDomainEventModel.class)))
        .thenThrow(ApplicationRuntimeException.class);
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(ProductCodeDomainEventModel.class));
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void schedulerRetryMasterSkuMappingTest() throws Exception {
    productRetryEventPublish5 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(MASTER_SKU_MAPPING)
            .topicName(ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING).clearCache(Boolean.TRUE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish5)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(MasterSkuMappingEventModel.class)))
        .thenReturn(new MasterSkuMappingEventModel(STORE_ID, ITEM_SKU, MASTER_SKU));
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(MasterSkuMappingEventModel.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
    verify(itemService).updateMasterSku(Mockito.any());
  }

  @Test
  public void schedulerRetryMasterSkuMappingTestException() throws Exception {
    productRetryEventPublish5 =
        ProductRetryEventPublish.builder().retryPublishStatus(RetryPublishStatus.PENDING).retryCount(0)
            .identifier(MASTER_SKU_MAPPING)
            .topicName(ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING).clearCache(Boolean.TRUE).build();
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(RetryPublishStatus.PENDING, 20))
        .thenReturn(new ArrayList<>(Arrays.asList(productRetryEventPublish5)));
    Mockito.when(objectMapper.readValue(Mockito.anyString(), eq(MasterSkuMappingEventModel.class)))
        .thenThrow(ApplicationRuntimeException.class);
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(objectMapper).readValue(Mockito.anyString(), eq(MasterSkuMappingEventModel.class));
    verify(productRetryEventPublishRepository, times(1)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void schedulerRetryPublish_Exception() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(saveAndPublishService).publishListOfItems(anyList());
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2);
    verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), any());
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(saveAndPublishService, times(2)).publishListOfItems(anyList());
    verify(productRetryEventPublishRepository).saveAll(anyList());
    verify(productRetryEventPublishRepository, times(3)).save(any(ProductRetryEventPublish.class));
    verify(kafkaProducer).send(eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
      Mockito.any(MessageEmailRequest.class));
  }

  @Test
  public void schedulerRetryPublish_Message_Exception() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(saveAndPublishService).publishListOfItems(anyList());
    doThrow(ApplicationRuntimeException.class).when(kafkaProducer).send(any(), any());
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2);
    verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), any());
    verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(saveAndPublishService, times(2)).publishListOfItems(anyList());
    verify(productRetryEventPublishRepository, times(3)).save(any(ProductRetryEventPublish.class));
    verify(kafkaProducer).send(eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT),
      Mockito.any(MessageEmailRequest.class));
  }

  @Test
  public void schedulerRetryPublish_promotionEventTest() throws Exception {
    this.productRetryEventPublish3.setIdentifier(ITEM_SKU);
    this.productRetryEventPublish3.setSecondaryIdentifier(PICKUP_POINT_CODE);
    this.adjustmentV2.setPickupPointCode(PICKUP_POINT_CODE);
    List<AdjustmentProductChangeResponseVO> changeResponseVOS =
      Collections.singletonList(AdjustmentProductChangeResponseVO.builder().productSku(ITEM_SKU)
        .adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
        .pickupPointCode(PICKUP_POINT_CODE).exclusiveProduct(true).build());
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish3)));
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.promotionOutbound.getAdjustmentProductBySkuAndPickupPointCode(anyString(), eq(
      Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build())))).thenReturn(changeResponseVOS);
    Mockito.when(objectConverterService.convertToAdjustmentProductChangeList(changeResponseVOS,
     Collections.emptyList())).thenReturn(Collections.singletonList(adjustmentV2));
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2))
      .thenReturn(itemPickupPoint);
    this.productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(
      RetryPublishStatus.PENDING, 20);
    verify(skuValidator).isItemSku(ITEM_SKU);
    verify(promotionOutbound).getAdjustmentProductBySkuAndPickupPointCode(isNull(), anyList());
    verify(objectConverterService).convertToAdjustmentProductChangeList(Collections.emptyList(),
      Collections.emptyList());
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
  }

  @Test
  public void schedulerRetryPublish_promotionV1EventTest() throws Exception {
    this.productRetryEventPublish3.setIdentifier(ITEM_SKU);
    List<AdjustmentProductResponse> adjustmentProductResponses = Collections.singletonList(
      AdjustmentProductResponse.builder().productSku(ITEM_SKU).adjustmentName(ADJUSTMENT_NAME)
        .campaignCode(CAMPAIGN_CODE).startDate(CAMPAIGN_START_DATE).endDate(CAMPAIGN_END_DATE)
        .activated(false).priority(0).build());
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish3)));
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(
      promotionOutbound.getAdjustmentProduct(X_PRODUCT_RETRY_REQUEST, Constants.DEFAULT_USERNAME,
        Collections.singletonList(ITEM_SKU))).thenReturn(adjustmentProductResponses);
    Mockito.when(objectConverterService.convertToAdjustmentProductChangeList(Collections.emptyList(),
      adjustmentProductResponses)).thenReturn(Collections.singletonList(adjustmentV2));
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2))
      .thenReturn(itemPickupPoint);
    this.productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(
      RetryPublishStatus.PENDING, 20);
    verify(skuValidator).isItemSku(ITEM_SKU);
    verify(promotionOutbound).getAdjustmentProduct(anyString(), anyString(), anyList());
    verify(adjustmentProductChangeListenerV2).processAdjustmentEventV2(adjustmentV2);
    verify(objectConverterService).convertToAdjustmentProductChangeList(Collections.emptyList(),
      adjustmentProductResponses);
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
  }

  @Test
  public void schedulerRetryPublish_promotionNullL5Test() throws Exception {
    Mockito.doNothing().when(cacheEvictHelperService)
      .evictItemPickupPointData(STORE_ID, itemPickupPoint, PICKUP_POINT_CODE);
    productRetryEventPublish3.setClearCache(Boolean.TRUE);
    this.productRetryEventPublish3.setIdentifier(ITEM_SKU);
    this.productRetryEventPublish3.setSecondaryIdentifier(PICKUP_POINT_CODE);
    this.adjustmentV2.setPickupPointCode(PICKUP_POINT_CODE);
    List<AdjustmentProductChangeResponseVO> changeResponseVOS = Collections.singletonList(
      AdjustmentProductChangeResponseVO.builder().productSku(ITEM_SKU)
        .adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
        .pickupPointCode(PICKUP_POINT_CODE).build());
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish3)));
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.promotionOutbound.getAdjustmentProductBySkuAndPickupPointCode(anyString(), eq(
      Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build())))).thenReturn(changeResponseVOS);
    Mockito.when(
        objectConverterService.convertToAdjustmentProductChangeList(changeResponseVOS,
          Collections.emptyList()))
      .thenReturn(Collections.singletonList(adjustmentV2));
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2))
      .thenReturn(null);
    this.productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(
      RetryPublishStatus.PENDING, 20);
    verify(skuValidator).isItemSku(ITEM_SKU);
    verify(promotionOutbound).getAdjustmentProductBySkuAndPickupPointCode(isNull(), anyList());
    verify(objectConverterService).convertToAdjustmentProductChangeList(Collections.emptyList(),
      Collections.emptyList());
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
  }


  @Test
  public void schedulerRetryPublish_promotionEventCacheEviction_test() throws Exception {
    Mockito.doNothing().when(cacheEvictHelperService)
      .evictItemPickupPointData(STORE_ID, itemPickupPoint, PICKUP_POINT_CODE);
    productRetryEventPublish3.setClearCache(Boolean.TRUE);
    this.productRetryEventPublish3.setIdentifier(ITEM_SKU);
    this.productRetryEventPublish3.setSecondaryIdentifier(PICKUP_POINT_CODE);
    this.adjustmentV2.setPickupPointCode(PICKUP_POINT_CODE);
    List<AdjustmentProductChangeResponseVO> changeResponseVOS = Collections.singletonList(
      AdjustmentProductChangeResponseVO.builder().productSku(ITEM_SKU)
        .adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
        .pickupPointCode(PICKUP_POINT_CODE).build());
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish3)));
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.promotionOutbound.getAdjustmentProductBySkuAndPickupPointCode(anyString(), eq(
      Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build())))).thenReturn(changeResponseVOS);
    Mockito.when(
        objectConverterService.convertToAdjustmentProductChangeList(changeResponseVOS,
          Collections.emptyList()))
      .thenReturn(Collections.singletonList(adjustmentV2));
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2))
      .thenReturn(itemPickupPoint);
    this.productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(
      RetryPublishStatus.PENDING, 20);
    verify(skuValidator).isItemSku(ITEM_SKU);
    verify(promotionOutbound).getAdjustmentProductBySkuAndPickupPointCode(isNull(), anyList());
    verify(objectConverterService).convertToAdjustmentProductChangeList(Collections.emptyList(),
      Collections.emptyList());
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
  }

  @Test
  public void schedulerRetryPublish_promotionEventNoResponseTest() throws Exception {
    this.productRetryEventPublish3.setIdentifier(ITEM_SKU);
    this.productRetryEventPublish3.setSecondaryIdentifier(PICKUP_POINT_CODE);
    this.adjustmentV2.setPickupPointCode(PICKUP_POINT_CODE);
    List<AdjustmentProductChangeResponseVO> changeResponseVOS =
      Collections.singletonList(AdjustmentProductChangeResponseVO.builder().productSku(ITEM_SKU)
        .adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
        .pickupPointCode(PICKUP_POINT_CODE).build());
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish3)));
    Mockito.when(skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.promotionOutbound.getAdjustmentProductBySkuAndPickupPointCode(anyString(), eq(
      Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build())))).thenReturn(Collections.emptyList());
    Mockito.when(objectConverterService.convertToAdjustmentProductChangeList(anyList(),
      anyList())).thenReturn(Collections.emptyList());
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2))
      .thenReturn(itemPickupPoint);
    this.productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(
      RetryPublishStatus.PENDING, 20);
    verify(skuValidator).isItemSku(ITEM_SKU);
    verify(promotionOutbound).getAdjustmentProductBySkuAndPickupPointCode(isNull(), anyList());
    verify(objectConverterService).convertToAdjustmentProductChangeList(anyList(),
      anyList());
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
  }

  @Test
  public void schedulerRetryPublish_promotionEventWithNull_response() throws Exception {
    Mockito.doNothing().when(cacheEvictHelperService).evictItemPickupPointData(STORE_ID,
      itemPickupPoint,PICKUP_POINT_CODE);
    Mockito.when(adjustmentProductChangeListenerV2.processAdjustmentEventV2(adjustmentV2)).thenReturn(null);
    productRetryEventPublish3.setClearCache(Boolean.TRUE);
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_2);
    verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), any());
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(saveAndPublishService, times(2)).publishListOfItems(anyList());
    verify(productRetryEventPublishRepository, times(3)).save(any(ProductRetryEventPublish.class));
  }

  @Test
  public void retryL3SolrReindexingTest(){
    productRetryEventPublish4.setTopicName(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR);
    productRetryEventPublish4.setIdentifier(PRODUCT_SKU);
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish4)));
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
      .thenReturn(product);
    Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    when(objectConverterService.convertToProductEventModel(any(Product.class))).thenReturn(
      new ProductEventModel());
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(objectConverterService).convertToProductEventModel(any(Product.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR),
      isNull(), Mockito.any(DeltaReindexToSolrEventModel.class));
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(productRetryEventPublishRepository).save(productRetryEventPublishArgumentCaptor.capture());
    Assertions.assertEquals(RetryPublishStatus.FINISHED, productRetryEventPublishArgumentCaptor.getValue().getRetryPublishStatus());
  }

  @Test
  public void retryL3SolrReindexing_ExceptionTest(){
    productRetryEventPublish4.setTopicName(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR);
    productRetryEventPublish4.setIdentifier(PRODUCT_SKU);
    Mockito.when(productRetryEventPublishRepository.findByStateForRetryPublish(
        RetryPublishStatus.PENDING, 20))
      .thenReturn(new ArrayList<>(Collections.singletonList(productRetryEventPublish4)));
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
      .thenReturn(product);
    Mockito.when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    doThrow(ApplicationRuntimeException.class).when(objectConverterService).convertToProductEventModel(any(Product.class));
    productRetryEventPublishService.schedulerRetryPublish(STORE_ID);
    verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.RETRY_PUBLISH_BATCH_SIZE);
    verify(productRetryEventPublishRepository).findByStateForRetryPublish(RetryPublishStatus.PENDING, 20);
    verify(productRetryEventPublishRepository).save(any(ProductRetryEventPublish.class));
    verify(objectConverterService).convertToProductEventModel(any(Product.class));
  }
}
