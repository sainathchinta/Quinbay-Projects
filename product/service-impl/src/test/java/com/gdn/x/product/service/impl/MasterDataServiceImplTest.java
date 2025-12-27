package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.MarkForDeleteHelperService;
import com.gdn.x.product.service.api.MasterDataHelperService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

public class MasterDataServiceImplTest {

  private static final String ITEM_CODE = "item-code";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String PRODUCT_CODE = "product-code";

  private static final String STORE_ID = "storeId";

  private static final boolean IN_ALL_PRODUCTS = false;

  @InjectMocks
  private MasterDataServiceImpl masterDataServiceImpl;

  @Mock
  private MarkForDeleteHelperService markForDeleteHelperService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private MasterDataHelperService masterDataHelperService;

  @Test
  public void getMasterDataItemsTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(MasterDataServiceImplTest.ITEM_CODE);
    HashMap<String, MasterDataItem> expectedReturn = new HashMap<String, MasterDataItem>();
    when(
        this.masterDataHelperService.getMasterData(MasterDataItem.class,
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true)).thenReturn(expectedReturn);
    Map<String, MasterDataItem> result =
        this.masterDataServiceImpl.getMasterDataItems(MasterDataServiceImplTest.STORE_ID,
            MasterDataServiceImplTest.USERNAME, MasterDataServiceImplTest.REQUEST_ID, itemCodes);
    verify(this.masterDataHelperService).getMasterData(MasterDataItem.class,
        MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
        MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true);
    assertEquals(result, expectedReturn);
  }

  @Test
  public void getMasterDataProductDetailResponseTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(MasterDataServiceImplTest.ITEM_CODE);
    HashMap<String, MasterDataProductAndItemsVO> expectedReturn =
        new HashMap<String, MasterDataProductAndItemsVO>();
    when(
        this.masterDataHelperService.getMasterData(MasterDataProductAndItemsVO.class,
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true)).thenReturn(expectedReturn);
    Map<String, MasterDataProductAndItemsVO> result =
        this.masterDataServiceImpl.getMasterDataProductDetailResponse(
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS);
    verify(this.masterDataHelperService).getMasterData(MasterDataProductAndItemsVO.class,
        MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
        MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true);
    assertEquals(result, expectedReturn);
  }

  @Test
  public void getMasterDataProductsTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(MasterDataServiceImplTest.ITEM_CODE);
    HashMap<String, MasterDataProduct> expectedReturn = new HashMap<String, MasterDataProduct>();
    when(
        this.masterDataHelperService.getMasterData(MasterDataProduct.class,
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true)).thenReturn(expectedReturn);
    Map<String, MasterDataProduct> result =
        this.masterDataServiceImpl.getMasterDataProducts(MasterDataServiceImplTest.STORE_ID,
            MasterDataServiceImplTest.USERNAME, MasterDataServiceImplTest.REQUEST_ID, itemCodes);
    verify(this.masterDataHelperService).getMasterData(MasterDataProduct.class,
        MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
        MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, true);
    assertEquals(result, expectedReturn);
  }

  @Test
  public void getProductDetailFromMasterDataEmptyProductCodeTest() throws Exception {
    try {
      ProductDetailResponse result =
          this.masterDataServiceImpl.getProductDetailFromMasterData(
              MasterDataServiceImplTest.USERNAME, MasterDataServiceImplTest.REQUEST_ID, null);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void getProductDetailFromMasterDataProductCodeNotFoundTest() throws Exception {
    when(
        this.productCategoryBaseOutbound.getProductDetailByProductCode(
            MasterDataServiceImplTest.REQUEST_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.PRODUCT_CODE)).thenReturn(null);
    try {
      ProductDetailResponse result =
          this.masterDataServiceImpl.getProductDetailFromMasterData(
              MasterDataServiceImplTest.USERNAME, MasterDataServiceImplTest.REQUEST_ID,
              MasterDataServiceImplTest.PRODUCT_CODE);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      verify(this.productCategoryBaseOutbound).getProductDetailByProductCode(
          MasterDataServiceImplTest.REQUEST_ID, MasterDataServiceImplTest.USERNAME,
          MasterDataServiceImplTest.PRODUCT_CODE);
    }
  }

  @Test
  public void getProductDetailFromMasterDataTest() throws Exception {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse(new ProductResponse());
    when(
        this.productCategoryBaseOutbound.getProductDetailByProductCode(
            MasterDataServiceImplTest.REQUEST_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.PRODUCT_CODE)).thenReturn(productDetailResponse);
    when(this.markForDeleteHelperService.removeAllMarkForDelete(productDetailResponse)).thenReturn(
        productDetailResponse);
    ProductDetailResponse result =
        this.masterDataServiceImpl.getProductDetailFromMasterData(
            MasterDataServiceImplTest.USERNAME, MasterDataServiceImplTest.REQUEST_ID,
            MasterDataServiceImplTest.PRODUCT_CODE);
    verify(this.productCategoryBaseOutbound).getProductDetailByProductCode(
        MasterDataServiceImplTest.REQUEST_ID, MasterDataServiceImplTest.USERNAME,
        MasterDataServiceImplTest.PRODUCT_CODE);
    verify(this.markForDeleteHelperService).removeAllMarkForDelete(productDetailResponse);
    assertEquals(result, productDetailResponse);
  }

  @Test
  public void getMasterDataProductDetailResponseWithoutCacheTest() throws Exception {
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(MasterDataServiceImplTest.ITEM_CODE);
    HashMap<String, MasterDataProductAndItemsVO> expectedReturn =
        new HashMap<String, MasterDataProductAndItemsVO>();
    when(
        this.masterDataHelperService.getMasterData(MasterDataProductAndItemsVO.class,
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, false)).thenReturn(expectedReturn);
    Map<String, MasterDataProductAndItemsVO> result =
        this.masterDataServiceImpl.getMasterDataProductDetailResponseWithoutCache(
            MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
            MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS);
    verify(this.masterDataHelperService).getMasterData(MasterDataProductAndItemsVO.class,
        MasterDataServiceImplTest.STORE_ID, MasterDataServiceImplTest.USERNAME,
        MasterDataServiceImplTest.REQUEST_ID, itemCodes, IN_ALL_PRODUCTS, false);
    assertEquals(result, expectedReturn);
  }

  @BeforeEach
  public void init() {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.masterDataHelperService);
    verifyNoMoreInteractions(this.productCategoryBaseOutbound);
    verifyNoMoreInteractions(this.markForDeleteHelperService);
  }

}
