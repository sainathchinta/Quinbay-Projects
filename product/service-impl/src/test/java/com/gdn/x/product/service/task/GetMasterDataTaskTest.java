package com.gdn.x.product.service.task;

import java.util.Arrays;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.service.api.MasterDataCacheService;

public class GetMasterDataTaskTest {

  private static final String CODE = "CODE";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String USERNAME = "USERNAME";

  private GetMasterDataTask getMasterDataTaskItem;
  private GetMasterDataTask getMasterDataTaskProduct;
  private GetMasterDataTask getMasterDataTaskProductAndItemsVo;
  private GetMasterDataTask getMasterDataTaskProductAndItemsVoInAllProducts;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    getMasterDataTaskItem =
        new GetMasterDataTask(MasterDataItem.class, Arrays.asList(CODE), REQUEST_ID, USERNAME, masterDataCacheService);
    getMasterDataTaskProduct = new GetMasterDataTask(MasterDataProduct.class, Arrays.asList(CODE), REQUEST_ID, USERNAME,
        masterDataCacheService);
    getMasterDataTaskProductAndItemsVo = new GetMasterDataTask(MasterDataProductAndItemsVO.class, Arrays.asList(CODE), REQUEST_ID, USERNAME,
        masterDataCacheService);
    getMasterDataTaskProductAndItemsVoInAllProducts = new GetMasterDataTask(MasterDataProductAndItemsVO.class, Arrays.asList(CODE), REQUEST_ID, USERNAME,
        masterDataCacheService, true);
  }

  @Test
  public void callItemTest() {
    MasterDataItem masterDataItem = new MasterDataItem();
    Mockito.when(masterDataCacheService.getMasterDataItem(REQUEST_ID, USERNAME, CODE)).thenReturn(masterDataItem);
    Map<String, MasterDataItem> result = getMasterDataTaskItem.call();
    Assertions.assertNotNull(result.get(CODE));
    Mockito.verify(masterDataCacheService).getMasterDataItem(REQUEST_ID, USERNAME, CODE);
  }

  @Test
  public void callItemTestException() {
    Mockito.when(masterDataCacheService.getMasterDataItem(REQUEST_ID, USERNAME, CODE)).thenThrow(
        ApplicationRuntimeException.class);
    Map<String, MasterDataItem> result = getMasterDataTaskItem.call();
    Assertions.assertNull(result.get(CODE));
  }

  @Test
  public void callProductTest() throws Exception {
    MasterDataProductAndItemsVO masterDataItem = new MasterDataProductAndItemsVO();
    masterDataItem.setMasterDataProduct(new MasterDataProduct());
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(REQUEST_ID, USERNAME, CODE, false))
        .thenReturn(masterDataItem);
    Map<String, MasterDataItem> result = getMasterDataTaskProduct.call();
    Assertions.assertNotNull(result.get(CODE));
    Mockito.verify(masterDataCacheService).getMasterDataProductAndItems(REQUEST_ID, USERNAME, CODE, false);
  }

  @Test
  public void callProductTestException() throws Exception {
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(REQUEST_ID, USERNAME, CODE, false))
        .thenThrow(Exception.class);
    Map<String, MasterDataItem> result = getMasterDataTaskProduct.call();
    Assertions.assertNull(result.get(CODE));
  }

  @Test
  public void callProductAndItemTest() throws Exception {
    MasterDataProductAndItemsVO masterDataItem = new MasterDataProductAndItemsVO();
    masterDataItem.setMasterDataProduct(new MasterDataProduct());
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(USERNAME, REQUEST_ID, CODE, false))
        .thenReturn(masterDataItem);
    Map<String, MasterDataItem> result = getMasterDataTaskProductAndItemsVo.call();
    Assertions.assertNotNull(result.get(CODE));
    Mockito.verify(masterDataCacheService).getMasterDataProductAndItems(USERNAME, REQUEST_ID, CODE, false);
  }

  @Test
  public void callProductAndItemTestException() throws Exception {
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(REQUEST_ID, USERNAME, CODE, false))
        .thenThrow(Exception.class);
    Map<String, MasterDataItem> result = getMasterDataTaskProductAndItemsVo.call();
    Assertions.assertNull(result.get(CODE));
  }

  @Test
  public void callProductAndItem_inAllProductsTest() throws Exception {
    MasterDataProductAndItemsVO masterDataItem = new MasterDataProductAndItemsVO();
    masterDataItem.setMasterDataProduct(new MasterDataProduct());
    Mockito.when(masterDataCacheService.getMasterDataProductAndItems(USERNAME, REQUEST_ID, CODE, true))
        .thenReturn(masterDataItem);
    Map<String, MasterDataItem> result = getMasterDataTaskProductAndItemsVoInAllProducts.call();
    Assertions.assertNotNull(result.get(CODE));
    Mockito.verify(masterDataCacheService).getMasterDataProductAndItems(USERNAME, REQUEST_ID, CODE, true);
  }
}
