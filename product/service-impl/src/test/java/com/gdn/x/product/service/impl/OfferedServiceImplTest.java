package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.service.PristineService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductService;

/**
 * Created by w.william on 2/27/2018.
 */
public class OfferedServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_CODE = "itemCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String PRISTINE_ID = "pristineId";
  private static final String DEFAULT_SKU = "defaultSku";
  private static final boolean IN_ALL_PRODUCTS = false;

  @InjectMocks
  private OfferedServiceImpl offeredServiceImpl;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private PristineService pristineService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private Item item;
  private Product product;
  private OfferedSummaryVo offeredSummaryVo;
  private MandatoryRequestParam param;

  @Test
  public void getOfferPageHeaderByItemCode_SuccessTest() throws Exception {
    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummaryByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, null);

    assertNotNull(response);
    assertEquals(response.getBrand(), "brand");

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU);
  }

  @Test
  public void getOfferPageHeaderByItemCode_withL4DefaultSku_returnsItemSkuOfItemWithDefaultSku() throws Exception {
    Item defaultSkuItem = new Item();
    defaultSkuItem.setItemSku(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(defaultSkuItem);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, DEFAULT_SKU)).thenReturn(offeredSummaryVo);

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummaryByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, DEFAULT_SKU);

    assertNotNull(response);
    assertEquals(response.getBrand(), "brand");

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, DEFAULT_SKU);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
  }

  @Test
  public void getOfferPageHeaderByItemCode_withL5DefaultSku_returnsItemSkuOfOfflineItem() throws Exception {
    ItemPickupPoint defaultSkuOfflineItem = new ItemPickupPoint();
    defaultSkuOfflineItem.setItemSku(ITEM_SKU);
    defaultSkuOfflineItem.setOfflineItemId(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(null);
    when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU)).thenReturn(defaultSkuOfflineItem);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummaryByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, DEFAULT_SKU);

    assertNotNull(response);
    assertEquals(response.getBrand(), "brand");

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
  }

  @Test
  public void getOfferPageHeaderByItemCode_ExceptionTest() throws Exception{
    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenThrow(new RuntimeException());

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummaryByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, null);

    assertEquals(response, new OfferedSummaryVo());

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
  }

  @Test
  public void getOfferPageHeaderByItemCode_NullStoreIdTest() throws Exception{
    Assertions.assertThrows(RuntimeException.class, () -> offeredServiceImpl.getOfferedSummaryByItemCode(null, USERNAME, REQUEST_ID, ITEM_CODE, null));
  }

  @Test
  public void getOfferPageHeaderByItemCode_NullItemCodeTest() throws Exception{
    Assertions.assertThrows(RuntimeException.class, () ->  offeredServiceImpl.getOfferedSummaryByItemCode(STORE_ID, USERNAME, REQUEST_ID, null, null));
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    param = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID,CHANNEL_ID, CLIENT_ID, REQUEST_ID);

    item = new Item();
    item.setProductSku(PRODUCT_SKU);
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setStoreId(STORE_ID);

    product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);

    offeredSummaryVo = new OfferedSummaryVo();
    offeredSummaryVo.setBrand("brand");
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.objectConverterService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.masterDataService);
    verifyNoMoreInteractions(this.itemPickupPointService);
  }

  @Test
  public void getOfferedComboPageHeader_SuccessTest_1() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(pristineService.getPristineItemsMappingByPristineId(param, PRISTINE_ID, ITEM_SKU))
        .thenReturn(pristineItemDetailAndMappingVo);
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        pristineItemDetailAndMappingVo)).thenReturn(offeredSummaryVo);

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, PRISTINE_ID, ITEM_CODE, ITEM_SKU, null);

    verify(pristineService).getPristineItemsMappingByPristineId(param, PRISTINE_ID, ITEM_SKU);
    verify(objectConverterService).convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        pristineItemDetailAndMappingVo);
  }

  @Test
  public void getOfferedComboPageHeader_SuccessTest_2() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);


    param.setUsername(USERNAME);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, ITEM_CODE, ITEM_SKU, null);
    assertNotNull(response);

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU);
  }

  @Test
  public void getOfferedComboPageHeader_byItemCodeAndWithL4DefaultSku_returnsItemSkuOfItemWithTheSameDefaultSku()
      throws Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    Item itemWithTheSameDefaultSku = new Item();
    itemWithTheSameDefaultSku.setItemSku(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(itemWithTheSameDefaultSku);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, DEFAULT_SKU)).thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, ITEM_CODE, ITEM_SKU, DEFAULT_SKU);
    assertNotNull(response);

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, DEFAULT_SKU);
  }

  @Test
  public void getOfferedComboPageHeader_byItemCodeAndWithL5DefaultSku_returnsItemSkuOfOfflineItemWithTheSameDefaultSku()
      throws Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    ItemPickupPoint offlineItemWithTheSameDefaultSku = new ItemPickupPoint();
    offlineItemWithTheSameDefaultSku.setItemSku(ITEM_SKU);
    offlineItemWithTheSameDefaultSku.setOfflineItemId(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(null);
    when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(offlineItemWithTheSameDefaultSku);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, ITEM_CODE, ITEM_SKU, DEFAULT_SKU);
    assertNotNull(response);

    verify(itemService).getItemByItemCode(STORE_ID, ITEM_CODE);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(objectConverterService)
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU);
  }

  @Test
  public void getOfferedComboPageHeader_SuccessTest_3() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
            false, false, false, null, false, false)).thenReturn(item);
    try {
      param.setUsername(USERNAME);
      OfferedSummaryVo response =
          offeredServiceImpl.getOfferedComboSummary(param, null, null, ITEM_SKU, null);
      assertNotNull(response);
    } catch (Exception e) {}
    param.setUsername(USERNAME);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, null, ITEM_SKU, null);
    assertNotNull(response);
    verify(itemService, times(2)).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false);
  }


  @Test
  public void getOfferedComboPageHeader_SuccessTest_4() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false)).thenReturn(item);

    param.setUsername(USERNAME);
    product.setProductCode(null);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, null, ITEM_SKU, null);
    assertNotNull(response);

    verify(itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false);
  }

  @Test
  public void getOfferedComboPageHeader_SuccessTest_5() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_SKU)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);

    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProductAndItemsVO,
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    item.setItemCode(null);
    item.setSynchronized(false);
    item.setProductSku(PRODUCT_SKU);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false)).thenReturn(item);
    when(this.productService.getProduct(param.getStoreId(), PRODUCT_SKU)).thenReturn(product);
    when(this.objectConverterService.convertProductToOfferedSummaryVo(param, product, item))
        .thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    product.setProductCode(null);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedComboSummary(param, null, null, ITEM_SKU, null);
    assertNotNull(response);

    verify(this.productService).getProduct(param.getStoreId(), PRODUCT_SKU);
    verify(this.objectConverterService).convertProductToOfferedSummaryVo(param, product, item);
    verify(this.itemService).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false);
  }

  @Test
  public void getOfferedSummaryTest() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProducts.get(PRODUCT_CODE),
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false)).thenReturn(item);
    when(this.productService.getProduct(param.getStoreId(), PRODUCT_SKU)).thenReturn(product);
    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(this.objectConverterService.convertProductToOfferedSummaryVo(param, product, item))
        .thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    product.setProductCode(PRODUCT_CODE);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummary(param, null, ITEM_CODE, ITEM_SKU, null);
    assertNotNull(response);

    verify(objectConverterService).convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
        masterDataProducts.get(PRODUCT_CODE), ITEM_CODE, ITEM_SKU);
    verify(this.productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
            .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }

  @Test
  public void getOfferedSummaryTest_byItemCodeAndWithDefaultSku_returnsItemWithTheSameDefaultSku()
      throws Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    Item itemWithTheSameDefaultSku = new Item();
    itemWithTheSameDefaultSku.setItemSku(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(itemWithTheSameDefaultSku);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProducts.get(PRODUCT_CODE),
            ITEM_CODE, DEFAULT_SKU)).thenReturn(offeredSummaryVo);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false)).thenReturn(item);
    when(this.productService.getProduct(param.getStoreId(), PRODUCT_SKU)).thenReturn(product);
    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(this.objectConverterService.convertProductToOfferedSummaryVo(param, product, item))
        .thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    product.setProductCode(PRODUCT_CODE);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummary(param, null, ITEM_CODE, ITEM_SKU, DEFAULT_SKU);
    assertNotNull(response);

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(objectConverterService).convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
        masterDataProducts.get(PRODUCT_CODE), ITEM_CODE, DEFAULT_SKU);
    verify(this.productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }

  @Test
  public void getOfferedSummaryTest_byItemCodeAndWithDefaultSku_returnsOfflineItemWithTheSameDefaultSku()
      throws Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    ItemPickupPoint offlineItemWithTheSameDefaultSku = new ItemPickupPoint();
    offlineItemWithTheSameDefaultSku.setItemSku(ITEM_SKU);
    offlineItemWithTheSameDefaultSku.setOfflineItemId(DEFAULT_SKU);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(null);
    when(itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU))
        .thenReturn(offlineItemWithTheSameDefaultSku);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(objectConverterService
        .convertMasterDataProductAndItemsVoToOfferPageHeaderVo(masterDataProducts.get(PRODUCT_CODE),
            ITEM_CODE, ITEM_SKU)).thenReturn(offeredSummaryVo);

    when(itemService.getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false)).thenReturn(item);
    when(this.productService.getProduct(param.getStoreId(), PRODUCT_SKU)).thenReturn(product);
    when(masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProducts);
    when(this.objectConverterService.convertProductToOfferedSummaryVo(param, product, item))
        .thenReturn(offeredSummaryVo);

    param.setUsername(USERNAME);
    product.setProductCode(PRODUCT_CODE);
    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummary(param, null, ITEM_CODE, ITEM_SKU, DEFAULT_SKU);
    assertNotNull(response);

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(itemPickupPointService).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, DEFAULT_SKU);
    verify(objectConverterService).convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
        masterDataProducts.get(PRODUCT_CODE), ITEM_CODE, ITEM_SKU);
    verify(this.productService).getProduct(STORE_ID, PRODUCT_SKU);
    verify(masterDataService)
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }

  @Test
  public void getOfferedSummaryTest_2() throws  Exception {
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo = new PristineItemDetailAndMappingVo();
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        new PristineItemDetailAndMappingVo())).thenReturn(offeredSummaryVo);

    when(itemService.getItemByItemCode(STORE_ID, ITEM_CODE)).thenReturn(item);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(product);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();

    Set<String> productCodes = new HashSet<>();
    productCodes.add(PRODUCT_CODE);

    Map<String, MasterDataProductAndItemsVO> masterDataProducts = new HashMap<>();
    masterDataProducts.put(PRODUCT_CODE, masterDataProductAndItemsVO);

    when(pristineService.getPristineItemsMappingByPristineId(param, PRISTINE_ID, ITEM_SKU))
    .thenReturn(pristineItemDetailAndMappingVo);
    when(objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        pristineItemDetailAndMappingVo)).thenReturn(offeredSummaryVo);

    OfferedSummaryVo response =
        offeredServiceImpl.getOfferedSummary(param, PRISTINE_ID, null, ITEM_SKU, null);
    assertNotNull(response);

    verify(pristineService).getPristineItemsMappingByPristineId(param, PRISTINE_ID, ITEM_SKU);
    verify(objectConverterService).convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
        pristineItemDetailAndMappingVo);
  }

  @Test
  public void getOfferedSummaryTest_3() throws  Exception {
    try {
      OfferedSummaryVo response =
          offeredServiceImpl.getOfferedSummary(param, null, null, ITEM_SKU, null);
      assertNotNull(response);
    } catch (Exception e) {}
    verify(itemService, times(0)).getItem(STORE_ID, REQUEST_ID, USERNAME, ITEM_SKU, true,
        false, false, false, null, false, false);

  }
}
