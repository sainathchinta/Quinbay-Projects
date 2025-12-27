package com.gdn.x.product.service.impl;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataConcurrentService;
import com.gdn.x.product.service.util.FormulaUtil;

public class MasterDataHelperServiceImplTest {

  private static final int CONCURRENT_SIZE = 2;
  private static final int ZERO_CONCURRENT_SIZE = 0;
  private static final String ITEM_CODE = "itemCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final boolean IN_ALL_PRODUCTS = false;

  @InjectMocks
  private MasterDataHelperServiceImpl masterDataHelperServiceImpl;

  @Mock
  private MasterDataConcurrentService masterDataConcurrentService;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @Mock
  private FormulaUtil formulaUtil;

  @Test
  public void getMasterDataItemManyCodesConcurrentTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.ITEM_CODE);
    codes.add(MasterDataHelperServiceImplTest.PRODUCT_CODE);

    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    HashMap<String, MasterDataItem> expectedResult = new HashMap<String, MasterDataItem>();
    when(
        this.masterDataConcurrentService.doConcurrentCall(MasterDataItem.class,
            MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
            codes, MasterDataHelperServiceImplTest.CONCURRENT_SIZE, false)).thenReturn(expectedResult);
    Map<String, MasterDataItem> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataItem.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, false);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    verify(this.masterDataConcurrentService).doConcurrentCall(MasterDataItem.class,
        MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
        codes, MasterDataHelperServiceImplTest.CONCURRENT_SIZE, false);
    assertEquals(result, expectedResult);
  }

  @Test
  public void getMasterDataItemTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.ITEM_CODE);
    codes.add(null);
    MasterDataItem masterDataItem = new MasterDataItem();

    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    when(
        this.masterDataCacheService.getMasterDataItem(MasterDataHelperServiceImplTest.REQUEST_ID,
            MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.ITEM_CODE))
        .thenReturn(masterDataItem);
    Map<String, MasterDataItem> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataItem.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, true);

    verify(this.masterDataCacheService).getMasterDataItem(
        MasterDataHelperServiceImplTest.REQUEST_ID, MasterDataHelperServiceImplTest.USERNAME,
        MasterDataHelperServiceImplTest.ITEM_CODE);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    assertEquals(result.get(MasterDataHelperServiceImplTest.ITEM_CODE), masterDataItem);
  }

  @Test
  public void getMasterDataItemWithUseCacheFalseTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.ITEM_CODE);
    codes.add(null);
    MasterDataItem masterDataItem = new MasterDataItem();

    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    when(
        this.masterDataCacheService.getMasterDataItemWithoutCache(MasterDataHelperServiceImplTest.REQUEST_ID,
            MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.ITEM_CODE))
        .thenReturn(masterDataItem);
    Map<String, MasterDataItem> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataItem.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, false);

    verify(this.masterDataCacheService).getMasterDataItemWithoutCache(
        MasterDataHelperServiceImplTest.REQUEST_ID, MasterDataHelperServiceImplTest.USERNAME,
        MasterDataHelperServiceImplTest.ITEM_CODE);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    assertEquals(result.get(MasterDataHelperServiceImplTest.ITEM_CODE), masterDataItem);
  }

  @Test
  public void getMasterDataItemWithUseCacheFalseAndWithoutMasterDataItemClassTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.ITEM_CODE);
    codes.add(null);
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setActivated(true);
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);

    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    when(this.masterDataCacheService.getMasterDataProductAndItemsWithoutCache(MasterDataHelperServiceImplTest.USERNAME,
        MasterDataHelperServiceImplTest.REQUEST_ID, MasterDataHelperServiceImplTest.ITEM_CODE, false))
        .thenReturn(masterDataProductAndItemsVO);
    Map<String, MasterDataItem> result = this.masterDataHelperServiceImpl
        .getMasterData(null, MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, false);

    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    verify(this.masterDataCacheService)
        .getMasterDataProductAndItemsWithoutCache(MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, MasterDataHelperServiceImplTest.ITEM_CODE, false);
    assertNotNull(result);
  }

  @Test
  public void getMasterDataItemZeroConcurrentTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.ITEM_CODE);
    codes.add(null);

    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.ZERO_CONCURRENT_SIZE);

    Map<String, MasterDataItem> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataItem.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, true);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    assertTrue(result.isEmpty());
  }

  @Test
  public void getMasterDataNullCodesTest() throws Exception {
    try {
      this.masterDataHelperServiceImpl.getMasterData(MasterDataItem.class,
          MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
          MasterDataHelperServiceImplTest.REQUEST_ID, null, IN_ALL_PRODUCTS, true);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void getMasterDataProductAndItemsVOTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.PRODUCT_CODE);
    codes.add(null);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setActivated(true);
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);
    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    when(
        this.masterDataCacheService.getMasterDataProductAndItems(
            MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
            MasterDataHelperServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS)).thenReturn(masterDataProductAndItemsVO);
    Map<String, MasterDataProductAndItemsVO> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataProductAndItemsVO.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, true);

    verify(this.masterDataCacheService).getMasterDataProductAndItems(
        MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
        MasterDataHelperServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    assertEquals(result.get(MasterDataHelperServiceImplTest.PRODUCT_CODE),
        masterDataProductAndItemsVO);
  }

  @Test
  public void getMasterDataProductTest() throws Exception {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataHelperServiceImplTest.PRODUCT_CODE);
    codes.add(null);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setActivated(true);
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);
    when(this.formulaUtil.getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes))
        .thenReturn(MasterDataHelperServiceImplTest.CONCURRENT_SIZE);
    when(
        this.masterDataCacheService.getMasterDataProductAndItems(
            MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
            MasterDataHelperServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS)).thenReturn(masterDataProductAndItemsVO);
    Map<String, MasterDataProduct> result =
        this.masterDataHelperServiceImpl.getMasterData(MasterDataProduct.class,
            MasterDataHelperServiceImplTest.STORE_ID, MasterDataHelperServiceImplTest.USERNAME,
            MasterDataHelperServiceImplTest.REQUEST_ID, codes, IN_ALL_PRODUCTS, true);

    verify(this.masterDataCacheService).getMasterDataProductAndItems(
        MasterDataHelperServiceImplTest.USERNAME, MasterDataHelperServiceImplTest.REQUEST_ID,
        MasterDataHelperServiceImplTest.PRODUCT_CODE, IN_ALL_PRODUCTS);
    verify(this.formulaUtil).getConcurrentSize(MasterDataHelperServiceImplTest.STORE_ID, codes);
    assertEquals(result.get(MasterDataHelperServiceImplTest.PRODUCT_CODE), masterDataProduct);
  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.masterDataConcurrentService);
    verifyNoMoreInteractions(this.masterDataCacheService);
    verifyNoMoreInteractions(this.formulaUtil);
  }

}
