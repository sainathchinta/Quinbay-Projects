package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

import com.gdn.x.product.service.config.KafkaPublisher;
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
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductEventModel;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductStatusEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.DeleteTerminatedSellerProductStatus;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

public class DeleteTerminatedSellerProductServiceImplTest {
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SERVICE_NAME = "xproduct";
  private static final String STATUS_EVENT_NAME = "com.gdn.product.analytics.permanent.delete.product.result";

  @InjectMocks
  private DeleteTerminatedSellerProductServiceImpl deleteTerminatedSellerProductService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<ProductAndItemEventModel> productAndItemEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<DeleteTerminatedSellerProductStatusEventModel>
      deleteTerminatedSellerProductStatusEventModelArgumentCaptor;

  private DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel;
  private ItemPickupPoint itemPickupPoint;
  private Item item;
  private Product product;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);

    deleteTerminatedSellerProductEventModel =
        DeleteTerminatedSellerProductEventModel.builder().productCode(PRODUCT_CODE).sellerCode(SELLER_CODE).build();

    product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);

    item = new Item();
    item.setStoreId(STORE_ID);
    item.setProductSku(PRODUCT_SKU);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setProductSku(PRODUCT_SKU);

    ReflectionTestUtils.setField(deleteTerminatedSellerProductService,
        "deleteTerminatedSellerProductXProductServiceName", SERVICE_NAME);

    Mockito.when(kafkaTopicProperties.getDeleteTerminatedSellerProductStatusEvent()).thenReturn(STATUS_EVENT_NAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productService, itemService, itemPickupPointService, cacheEvictHelperService,
        kafkaProducer, kafkaTopicProperties);
  }

  @Test
  public void deleteTerminatedSellerProductData() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);
    Mockito.when(productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY)).thenReturn(product);
    Mockito.when(
            itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemService.deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointCache(STORE_ID, Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.doNothing().when(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.doNothing().when(cacheEvictHelperService).evictProductData(STORE_ID, product);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU));
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointCache(STORE_ID, Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(cacheEvictHelperService).evictItemData(STORE_ID, item);
    Mockito.verify(cacheEvictHelperService).evictProductData(STORE_ID, product);
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();

    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertTrue(productAndItemEventModelArgumentCaptor.getValue().isRejected());
    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.SUCCESS.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_NoItemsRetunedToEvictCache() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);
    Mockito.when(productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY)).thenReturn(product);
    Mockito.when(
            itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU)))
        .thenReturn(new ArrayList<>());
    Mockito.when(itemService.deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(new ArrayList<>());
    Mockito.when(productService.deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(new ArrayList<>());

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU));
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();

    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertTrue(productAndItemEventModelArgumentCaptor.getValue().isRejected());
    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.SUCCESS.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_ErrorWhileClearingCache() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);
    Mockito.when(productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY)).thenReturn(product);
    Mockito.when(
            itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemService.deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(product));
    Mockito.doThrow(ApplicationRuntimeException.class).when(cacheEvictHelperService)
        .evictItemPickupPointCache(STORE_ID, Arrays.asList(item), Arrays.asList(itemPickupPoint));

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU));
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointCache(STORE_ID, Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU),
        productAndItemEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModelArgumentCaptor.getValue().getProductSku());
    Assertions.assertTrue(productAndItemEventModelArgumentCaptor.getValue().isRejected());
    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.SUCCESS.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_ErrorWhileDeletingFromDB() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);
    Mockito.when(productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY)).thenReturn(product);
    Mockito.when(
            itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemService.deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU)))
        .thenThrow(ApplicationRuntimeException.class);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService, Mockito.times(2)).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY);
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(), Set.of(PRODUCT_SKU));
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(STORE_ID, Set.of(PRODUCT_SKU));
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.FAILED.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_ErrorWhileUpdatingPickedForDeletionFlag() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);
    Mockito.when(productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY))
        .thenThrow(ApplicationRuntimeException.class);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.FAILED.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_AlreadyPickedForDeletion() {
    product.setPickedForDeletion(true);
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(product);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.SUCCESS.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_ProductReturnedNull() {
    Mockito.when(productService.findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE)).thenReturn(null);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(productService).findByProductCodeAndSellerCode(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.SUCCESS.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_EmptyProductCode() {
    deleteTerminatedSellerProductEventModel.setProductCode(StringUtils.EMPTY);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(StringUtils.EMPTY),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(StringUtils.EMPTY,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(SELLER_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.FAILED.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }

  @Test
  public void deleteTerminatedSellerProductData_EmptySellerCode() {
    deleteTerminatedSellerProductEventModel.setSellerCode(StringUtils.EMPTY);

    deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);

    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getDeleteTerminatedSellerProductStatusEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(STATUS_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.capture());

    Assertions.assertEquals(PRODUCT_CODE,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(StringUtils.EMPTY,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(SERVICE_NAME,
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getService());
    Assertions.assertEquals(DeleteTerminatedSellerProductStatus.FAILED.name(),
        deleteTerminatedSellerProductStatusEventModelArgumentCaptor.getValue().getResult());
  }
}