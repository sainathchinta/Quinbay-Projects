package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.service.api.SaveOperationService;

public class Off2OnHelperServiceImplTest {

  private static final String MERCHANT_CODE = "merchantCode";

  private static final String EMPTY_STRING = "";

  private static final String PRODUCT_SKU = "productSku";

  private static final ArrayList<String> EMPTY_LIST = new ArrayList<String>();

  private static final String ITEM_SKU = "itemSku";

  private static final String STORE_ID = "storeId";

  @InjectMocks
  private Off2OnHelperServiceImpl off2OnHelperService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Test
  public void changeOff2OnChannelActiveByItemSkuListApplicationExceptionTest() {
    when(
        this.saveOperationService.updateOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true))
        .thenThrow(new ApplicationRuntimeException());
    ArrayList<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnHelperServiceImplTest.ITEM_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, itemSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByItemSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true);
    assertEquals(result, itemSkus);
  }

  @Test
  public void changeOff2OnChannelActiveByItemSkuListExceptionTest() {
    when(
        this.saveOperationService.updateOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true))
        .thenThrow(new RuntimeException());
    ArrayList<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnHelperServiceImplTest.ITEM_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, itemSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByItemSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true);
    assertEquals(result, itemSkus);
  }

  @Test
  public void changeOff2OnChannelActiveByItemSkuListTest() {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), new ArrayList<Item>());
    when(
        this.saveOperationService.updateOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true))
        .thenReturn(productAndItemsVO);
    ArrayList<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnHelperServiceImplTest.ITEM_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, itemSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByItemSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true);
    assertEquals(result, Off2OnHelperServiceImplTest.EMPTY_LIST);
  }

  @Test
  public void changeOff2OnChannelActiveByItemSkuNullStoreIdTest() {
    try {
      this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(null,
          Off2OnHelperServiceImplTest.ITEM_SKU, true);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void changeOff2OnChannelActiveByItemSkuTest() {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), new ArrayList<Item>());
    when(
        this.saveOperationService.updateOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true))
        .thenReturn(productAndItemsVO);
    boolean result =
        this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByItemSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.ITEM_SKU, true);
    assertEquals(result, true);
  }

  @Test
  public void changeOff2OnChannelActiveByMerchantCodeTest() {
    Set<ProductAndItemSolr> setResult = new HashSet<ProductAndItemSolr>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(Off2OnHelperServiceImplTest.PRODUCT_SKU);
    setResult.add(productAndItemSolr);
    when(
        this.productAndItemSolrRepository
            .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(
                Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.MERCHANT_CODE,
                false)).thenReturn(setResult);
    when(
        this.saveOperationService.updateOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true))
        .thenReturn(productAndItemsVO);
    List<String> listResult = new ArrayList<String>();
    listResult.add(Off2OnHelperServiceImplTest.PRODUCT_SKU);
    this.off2OnHelperService.changeOff2OnChannelActiveByMerchantCode(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.MERCHANT_CODE, true);
    verify(this.productAndItemSolrRepository)
        .findByStoreIdAndMerchantCodeAndOff2OnChannelActiveAndMarkForDeleteFalse(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.MERCHANT_CODE, false);
    verify(this.saveOperationService).updateOff2OnChannelActiveByProductSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuEmptyItemSkuTest() {
    try {
      this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(
          Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.EMPTY_STRING, true);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuEmptyProductSkuTest() {
    try {
      this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(
          Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.EMPTY_STRING, true);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuListApplicationExceptionTest() {
    when(
        this.saveOperationService.updateOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true))
        .thenThrow(new ApplicationRuntimeException());
    List<String> productSkus = new ArrayList<String>();
    productSkus.add(Off2OnHelperServiceImplTest.PRODUCT_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, productSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByProductSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    assertEquals(result, productSkus);
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuListExceptionTest() {
    when(
        this.saveOperationService.updateOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true))
        .thenThrow(new RuntimeException());
    List<String> productSkus = new ArrayList<String>();
    productSkus.add(Off2OnHelperServiceImplTest.PRODUCT_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, productSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByProductSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    assertEquals(result, productSkus);
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuListTest() {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), new ArrayList<Item>());
    when(
        this.saveOperationService.updateOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true))
        .thenReturn(productAndItemsVO);
    List<String> productSkus = new ArrayList<String>();
    productSkus.add(Off2OnHelperServiceImplTest.PRODUCT_SKU);
    List<String> result =
        this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, productSkus, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByProductSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    assertEquals(result, new ArrayList<String>());
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuNullStoreIdTest() {
    try {
      this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(null,
          Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void changeOff2OnChannelActiveByProductSkuTest() {
    ProductAndItemsVO productAndItemsVO =
        new ProductAndItemsVO(new Product(), new ArrayList<Item>());
    when(
        this.saveOperationService.updateOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true))
        .thenReturn(productAndItemsVO);
    boolean result =
        this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(
            Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    verify(this.saveOperationService).updateOff2OnChannelActiveByProductSku(
        Off2OnHelperServiceImplTest.STORE_ID, Off2OnHelperServiceImplTest.PRODUCT_SKU, true);
    assertEquals(result, true);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.saveOperationService);
    verifyNoMoreInteractions(this.productAndItemSolrRepository);
  }
}
