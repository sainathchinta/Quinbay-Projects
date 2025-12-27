package com.gdn.x.product.service.impl;

import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductService;

public class DataSourceWrapperServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "ppCode";

  @InjectMocks
  private DataSourceWrapperServiceImpl dataSourceWrapperService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(itemService);
    Mockito.verifyNoMoreInteractions(itemPickupPointService);
    Mockito.verifyNoMoreInteractions(itemCacheableService);
  }

  @Test
  public void testGetProductByStoreIdAndProductSku() {
    dataSourceWrapperService.getProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testGetProductByStoreIdAndProductSkuPrimary() {
    dataSourceWrapperService.getProductByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(productService).getProductReadFromPrimary(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindProductByStoreIdAndProductSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindProductByStoreIdAndProductSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(ITEM_SKU), false);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU));
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimary(STORE_ID, Collections.singletonList(ITEM_SKU));
  }

  @Test
  public void testFindItemPickupPointByItemSkuAndPickupPointCode() {
    dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void testFindItemPickupPointByItemSkuAndPickupPointCodePrimary() {
    dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true);
    Mockito.verify(itemPickupPointService)
        .findByItemSkuAndPickupPointCodeReadFromPrimary(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        Collections.singleton(ITEM_SKU), false, false);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, Collections.singleton(ITEM_SKU),
            false);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedPrimary() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        Collections.singleton(ITEM_SKU), false, true);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimary(STORE_ID,
            Collections.singleton(ITEM_SKU), false);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU,
            PICKUP_POINT_CODE);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndProductSku() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemPickupPointService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindItemPickupPointByStoreIdAndProductSkuPrimary() {
    dataSourceWrapperService.findItemPickupPointByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointService).findByStoreIdAndProductSkuReadFromPrimary(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindItemPickupPointsByProductSkuAndPickupPointCodes() {
    dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE), false);
    Mockito.verify(itemPickupPointService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE));
  }

  @Test
  public void testFindItemPickupPointsByProductSkuAndPickupPointCodesPrimary() {
    dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE), true);
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimary(STORE_ID, PRODUCT_SKU,
            Collections.singletonList(PICKUP_POINT_CODE));
  }

  @Test
  public void testGetItemPickupPointsByProductSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemPickupPointService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testGetItemPickupPointsByProductSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void testFindItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false);
    Mockito.verify(itemPickupPointService).findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        true);
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive() {
    dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID,
        ITEM_SKU, false, false);
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void testFindItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActivePrimary() {
    dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID,
        ITEM_SKU, false, true);
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimary(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void testFindItemByStoreIdAndItemSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemByStoreIdAndItemSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true);
    Mockito.verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
  }

  @Test
  public void TestFindItemByStoreIdAndItemSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false, false);
    Mockito.verify(itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false, false, false);
  }

  @Test
  public void TestFindItemByStoreIdAndItemSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false, true);
    Mockito.verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
  }

  @Test
  public void testFindItemsByStoreIdAndProductSkuAndMarkForDeleteFalse() {
    dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false,
        false);
    Mockito.verify(itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false, false);
  }

  @Test
  public void testFindItemsByStoreIdAndProductSkuAndMarkForDeleteFalsePrimary() {
    dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false,
        true);
    Mockito.verify(itemService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimary(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void testFindItemByStoreIdAndItemSkus() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkus(STORE_ID, Collections.singleton(ITEM_SKU), false);
    Mockito.verify(itemService).findByStoreIdAndItemSkus(STORE_ID, Collections.singleton(ITEM_SKU));
  }

  @Test
  public void testFindItemByStoreIdAndItemSkusPrimary() {
    dataSourceWrapperService.findItemByStoreIdAndItemSkus(STORE_ID, Collections.singleton(ITEM_SKU), true);
    Mockito.verify(itemService).findByStoreIdAndItemSkusReadFromPrimary(STORE_ID, Collections.singleton(ITEM_SKU));
  }

  @Test
  public void testFindItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated() {
    dataSourceWrapperService.findItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID,
        PRODUCT_SKU, false, false);
    Mockito.verify(itemService)
        .findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void testFindItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedPrimary() {
    dataSourceWrapperService.findItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID,
        PRODUCT_SKU, false, true);
    Mockito.verify(itemService)
        .findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimary(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfigTest() {
    dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(STORE_ID,
        ITEM_SKU);
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(STORE_ID, ITEM_SKU, Constants.CNC);
  }
}