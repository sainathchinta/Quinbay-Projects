package com.gdn.mta.product.util;

import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.service.util.PbpStockAlertConverterUtilBean;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;

public class PbpStockAlertConverterUtilTest {

  @InjectMocks
  private PbpStockAlertConverterUtilBean instance;

  private ProductLevel3 productLevel3;
  
  private ProductAndItemsResponse productAndItemsResponse;

  private PbpStockAlert pbpStockAlert;

  private static final boolean IS_MINIMUM_STOCK = true;
  private static final boolean IS_OOS = true;
  private static final boolean IS_NEW_ALERT = true;
  private static final String GDN_SKU = "GDN_SKU";
  private static final String BP_CODE = "BP_CODE";
  private static final String STORE_ID = "10001";
  private static final String PRODUCT_NAME = "Test Product";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";
  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.productLevel3 = new ProductLevel3();
    this.pbpStockAlert = new PbpStockAlert();
    
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    masterDataProduct.setProductName(PRODUCT_NAME);
    
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterDataProduct(masterDataProduct);
    
    productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(productResponse);
  }

  @AfterEach
  public void tearDown() throws Exception {}

  @Test
  public void converterForPbpStockAlertTest_create() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }
  
  @Test
  public void converterForPbpStockAlertTest_create_1() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }

  @Test
  public void converterForPbpStockAlertTest_update() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }
  
  @Test
  public void converterForPbpStockAlertTest_update_1() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }

  @Test
  public void converterForPbpStockAlertTest_updateMarkForDeleteTrue() {
    this.pbpStockAlert.setMarkForDelete(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
    Assertions.assertFalse(response.isMarkForDelete());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateMarkForDeleteTrue_1() {
    this.pbpStockAlert.setMarkForDelete(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, IS_MINIMUM_STOCK, IS_OOS, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
    Assertions.assertFalse(response.isMarkForDelete());
  }

  @Test
  public void converterForPbpStockAlertTest_updateIsMinimumAndIsOOSNull() {
    this.pbpStockAlert.setMarkForDelete(true);
    this.pbpStockAlert.setIsMinimumStock(true);
    this.pbpStockAlert.setIsOos(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, null, null, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateIsMinimumAndIsOOSNull_1() {
    this.pbpStockAlert.setMarkForDelete(true);
    this.pbpStockAlert.setIsMinimumStock(true);
    this.pbpStockAlert.setIsOos(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, null, null, !IS_NEW_ALERT);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateToMarkfordeleteTrue() {
    this.pbpStockAlert.setIsMinimumStock(false);
    this.pbpStockAlert.setIsOos(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, null, false, !IS_NEW_ALERT);
    Assertions.assertFalse(response.getIsMinimumStock());
    Assertions.assertFalse(response.getIsOos());
    Assertions.assertFalse(response.isMarkForDelete());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateToMarkfordeleteTrue_1() {
    this.pbpStockAlert.setIsMinimumStock(false);
    this.pbpStockAlert.setIsOos(true);
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, null, false, !IS_NEW_ALERT);
    Assertions.assertFalse(response.getIsMinimumStock());
    Assertions.assertFalse(response.getIsOos());
    Assertions.assertFalse(response.isMarkForDelete());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateToMarkfordeleteTrue2() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productLevel3,
            this.pbpStockAlert, true, false, !IS_NEW_ALERT);
    Assertions.assertFalse(response.isMarkForDelete());
  }
  
  @Test
  public void converterForPbpStockAlertTest_updateToMarkfordeleteTrue2_1() {
    PbpStockAlert response =
        instance.converterForPbpStockAlert(STORE_ID, GDN_SKU, 1, 1, BP_CODE, this.productAndItemsResponse,
            this.pbpStockAlert, true, false, !IS_NEW_ALERT);
    Assertions.assertFalse(response.isMarkForDelete());
  }
  
  @Test
  public void converterNewPbpStockAlert() {
    PbpStockAlert response = instance.converterNewPbpStockAlert(STORE_ID, BP_CODE, GDN_SKU,
        PRODUCT_NAME, 1, 1, IS_MINIMUM_STOCK, IS_OOS, new Date(),PICKUP_POINT_CODE);
    Assertions.assertTrue(response.getIsMinimumStock());
    Assertions.assertTrue(response.getIsOos());
  }
}
