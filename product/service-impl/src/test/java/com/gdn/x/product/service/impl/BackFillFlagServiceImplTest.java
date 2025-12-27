package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.BackFillFbbFlagRequest;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class BackFillFlagServiceImplTest {

  private static final String PRODUCT_SKU = "productSku";
  private static final String STORE_ID = "store-id";
  private static final String PP_CODE = "pp-code";
  private BackFillFbbFlagRequest backFillFbbFlagRequest;
  private Product product;
  private ItemPickupPoint itemPickupPoint;
  private ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel;

  @InjectMocks
  private BackFillFbbFlagServiceImpl backFillFlagService;
  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModelArgumentCaptor;

  private BusinessPartner businessPartner;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.openMocks(this);
    backFillFbbFlagRequest =
      BackFillFbbFlagRequest.builder().pickupPointCode(PP_CODE).identifier(PRODUCT_SKU)
        .storeId(STORE_ID).build();
    product = new Product();
    product.setProductSku(PRODUCT_SKU);
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PP_CODE);
    itemPickupPoint.setStoreId(STORE_ID);

    itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(Collections.singletonList(
      ItemPickupPointChangeEventType.FBB_MIGRATION));
    businessPartner = new BusinessPartner();
    List<String> sellerChannel = new ArrayList<>();
    sellerChannel.add(Constants.B2B_SELLER_CHANNEL);
    sellerChannel.add(Constants.B2C_SELLER_CHANNEL);
    businessPartner.setSalesChannel(sellerChannel);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productRepository);
    Mockito.verifyNoMoreInteractions(itemPickupPointRepository);
    Mockito.verifyNoMoreInteractions(cacheEvictHelperService);
    Mockito.verifyNoMoreInteractions(productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(objectConverterService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(businessPartnerService);
  }

  @Test
  public void backFillFbbFlagTest() {
    ReflectionTestUtils.setField(backFillFlagService,
      "newlyAddedItemPickupPointDataChangeEventTypes", "");
    ReflectionTestUtils.setField(backFillFlagService, "publishL5Event", true);
    Mockito.when(productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU, false)).thenReturn(product);
    Mockito.when(
      itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_SKU, PP_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(
      itemPickupPointRepository.updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU,
        PP_CODE, true)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(productRepository.updateFbbFlagAtProduct(STORE_ID, PRODUCT_SKU, true))
      .thenReturn(product);
    Mockito.when(
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false))
      .thenReturn(itemPickupPointDataChangeEventModel);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    backFillFlagService.backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.verify(productRepository)
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(productRepository).updateFbbFlagAtProduct(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointRepository)
      .updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU, PP_CODE, true);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(product,
      new ArrayList<>(), false);
    Mockito.verify(objectConverterService)
      .convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    verify(this.kafkaProducer).send(
      eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
      eq(itemPickupPointDataChangeEventModel));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void backFillFbbFlagProductNullTest() {
    Mockito.when(productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU, false)).thenReturn(null);
    backFillFlagService.backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.verify(productRepository)
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);

  }

  @Test
  public void backFillFbbFlagFbbTrueTest() {
    ReflectionTestUtils.setField(backFillFlagService, "newL5DataChangeTypeEnabled", true);
    ReflectionTestUtils.setField(backFillFlagService,
      "newlyAddedItemPickupPointDataChangeEventTypes",
      ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
        .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    ReflectionTestUtils.setField(backFillFlagService, "publishL5Event", true);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPickupPointCode(PP_CODE);
    itemPickupPoint1.setStoreId(STORE_ID);
    product.setFbbActivated(true);
    itemPickupPoint.setFbbActivated(true);
    Mockito.when(productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU, false)).thenReturn(product);
    Mockito.when(
      itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_SKU, PP_CODE)).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    Mockito.when(
      itemPickupPointRepository.updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU,
        PP_CODE, true)).thenReturn(Collections.singletonList(itemPickupPoint1));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint1, false))
      .thenReturn(itemPickupPointDataChangeEventModel);
    backFillFlagService.backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.verify(productRepository)
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(itemPickupPointRepository)
      .updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU, PP_CODE, true);
    Mockito.verify(objectConverterService)
      .convertToItemPickupPointChangeEventModel(itemPickupPoint1, false);
    verify(this.kafkaProducer).send(
      eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
      eq(itemPickupPointDataChangeEventModel));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void backFillFbbFlagNoL5PublishTest() {
    ReflectionTestUtils.setField(backFillFlagService, "publishL5Event", false);
    Mockito.when(productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
      PRODUCT_SKU, false)).thenReturn(product);
    Mockito.when(
      itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_SKU, PP_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(
      itemPickupPointRepository.updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU,
        PP_CODE, true)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(productRepository.updateFbbFlagAtProduct(STORE_ID, PRODUCT_SKU, true))
      .thenReturn(product);
    backFillFlagService.backFillFbbFlag(backFillFbbFlagRequest);
    Mockito.verify(productRepository)
      .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(itemPickupPointRepository)
      .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        PP_CODE);
    Mockito.verify(productRepository).updateFbbFlagAtProduct(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointRepository)
      .updateFbbFlagByProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU, PP_CODE, true);
    Mockito.verify(this.cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(product,
      new ArrayList<>(), false);
  }

}
