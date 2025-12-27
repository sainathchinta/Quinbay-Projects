package com.gdn.x.product.service.impl;

import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemArchiveService;
import com.gdn.x.product.service.api.ItemPickupPointArchiveService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductArchiveService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;

public class ArchiveAndDeleteRejectedMerchantProductDataServiceImplTest {

  @InjectMocks
  private ArchiveAndDeleteRejectedMerchantProductDataServiceImpl archiveAndDeleteRejectedMerchantProductDataService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ProductArchiveService productArchiveService;

  @Mock
  private ItemArchiveService itemArchiveService;

  @Mock
  private ItemPickupPointArchiveService itemPickupPointArchiveService;

  @Mock
  private ItemService itemService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaPublisher kafkaProducer;

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";

  private Product product;
  private ProductCodeDomainEventModel productCodeDomainEventModel;
  private Item item;
  private ItemPickupPoint itemPickupPoint;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);

    productCodeDomainEventModel = new ProductCodeDomainEventModel();
    productCodeDomainEventModel.setStoreId(STORE_ID);
    productCodeDomainEventModel.setProductCode(PRODUCT_CODE);

    item = new Item();
    item.setStoreId(STORE_ID);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    ReflectionTestUtils.setField(archiveAndDeleteRejectedMerchantProductDataService, "deleteL3ForTerminatedSellers",
        true);
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductDataTest() throws Exception {
    ReflectionTestUtils.setField(archiveAndDeleteRejectedMerchantProductDataService, "deleteL3ForTerminatedSellers",
        false);
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemList);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    Mockito.when(itemPickupPointService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemPickupPointList);

    archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel);

    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productArchiveService).addProductsToProductArchive(Mockito.anyList());
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService).evictProductData(Mockito.anyString(), Mockito.any());
    Mockito.verify(itemArchiveService).addItemsToItemArchive(Mockito.anyList());
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(Mockito.anyString(), Mockito.any());
    Mockito.verify(itemPickupPointArchiveService).addItemPickupPointsToItemPickupPointArchive(Mockito.anyList());
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointCache(Mockito.anyString(), Mockito.anyList(), Mockito.anyList());
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductDataRetryTest() throws Exception {
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemList);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    Mockito.when(itemPickupPointService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemPickupPointList);

    archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductDataRetry(
        productCodeDomainEventModel);

    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productArchiveService).addProductsToProductArchive(Mockito.anyList());
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService).evictProductData(Mockito.anyString(), Mockito.any());
    Mockito.verify(itemArchiveService).addItemsToItemArchive(Mockito.anyList());
    Mockito.verify(itemService).deleteItemByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(Mockito.anyString(), Mockito.any());
    Mockito.verify(itemPickupPointArchiveService).addItemPickupPointsToItemPickupPointArchive(Mockito.anyList());
    Mockito.verify(itemPickupPointService)
        .deleteItemPickupPointByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointCache(Mockito.anyString(), Mockito.anyList(), Mockito.anyList());
    Mockito.verify(this.kafkaProducer).send(Mockito.eq(ProductDomainEventName.UPDATE_TO_SOLR), Mockito.eq(PRODUCT_SKU),
        Mockito.any(ProductAndItemEventModel.class));
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductInvalidStoreIdTest() throws Exception {
    productCodeDomainEventModel.setStoreId(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel));
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductInvalidProductCodeTest() throws Exception {
    productCodeDomainEventModel.setProductCode(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel));
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductDataProductListEmptyTest() throws Exception {
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(new ArrayList<>());
    archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel);
    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductItemsToDeleteEmptyTest() throws Exception {
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    List<Item> itemList = new ArrayList<>();
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU))
        .thenReturn(new ArrayList<>());

    archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel);

    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productArchiveService).addProductsToProductArchive(Mockito.anyList());
    Mockito.verify(productService).deleteProductByStoreIdAndProductSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(cacheEvictHelperService).evictProductData(Mockito.anyString(), Mockito.any());
  }

  @Test
  public void archiveAndDeleteRejectedMerchantProductDataExceptionTest() throws Exception {
    List<Product> productList = new ArrayList<>();
    productList.add(product);
    Mockito.when(productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE)).thenReturn(productList);
    Mockito.doThrow(new ApplicationRuntimeException())
        .when(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);

    archiveAndDeleteRejectedMerchantProductDataService.archiveAndDeleteRejectedMerchantProductData(
        productCodeDomainEventModel);

    Mockito.verify(productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productCacheableService).findProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(productRetryEventPublishService).insertToRetryPublish(Mockito.any());
  }
}
