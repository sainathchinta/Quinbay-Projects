package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.BeanUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.vo.CacheItemVO;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.PristineItemAndSiblingsVO;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.PristineCacheableService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.util.ProductAttributesUtilImpl;

/**
 * Created by govind on 22/12/2017 AD.
 */
public class PristineServiceImplTest {

  @InjectMocks
  private PristineServiceImpl pristineServiceImpl;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private PristineCacheableService pristineCacheableService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductAttributesUtilImpl productAttributesUtil;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  private PristineDataItem pristineItem;

  private PristineDataItem pristineItem2;

  private List<PristineDataItem> pristineItemList;

  private CacheItemVO cacheItemVO;

  private List<Item> itemList = new ArrayList<>();

  private List<Product> productList = new ArrayList<>();

  private List<PristineDataItem> siblings;

  private Map<String, MasterDataItem> masterDataItems;

  private MandatoryRequestParam mandatoryRequestParam;

  private Item item;

  private Product product;

  private static final String STORE_ID = "storeId";

  private static final String USERNAME = "username";

  private static final String PRISTINE_ID = "PRISTINE_ID";

  private static final String PRODUCT_SKU = "productSku";

  private static final String PRODUCT_CODE = "productCode";

  private static final String PRISTINE_MASTER_ID = "PRISTINE_MASTER_ID";

  private static final String CHANNEL_ID = "channelId";

  private static final String CLIENT_ID = "clientId";

  private static final String REQUEST_ID = "requestId";

  private static final String ITEM_CODE = "ITEM_CODE1";

  private static final String ITEM_SKU = "itemSku";

  private static final String PRISTINE_LISTING_ATTRIBUTE =
      "{\"HANDPHONE\" : \"color,rom\", \"COMPUTER\" : \"color\"}";

  private static final String PRISTINE_ATTRIBUTE_VALUE_TRANSLATION =
      "{\"color\":\"Warna\", \"rom\":\"Kapasitas Memori\"}";

  Map<String, String> listingParameters = new HashMap<>();
  Map<String, String> attributeNameTranslationMap = new HashMap<>();



  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);
    mandatoryRequestParam.setUsername(USERNAME);

    listingParameters.put("HANDPHONE", "color,rom");
    listingParameters.put("COMPUTER","color");

    attributeNameTranslationMap.put("color", "Warna");
    attributeNameTranslationMap.put("rom","Kapasitas Memori");

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_LISTING_ATTRIBUTE),
        Mockito.any(new TypeReference<Map<String, String>>() {}.getClass())))
        .thenReturn(listingParameters);

    when(this.objectMapper.readValue(Mockito.eq(PRISTINE_ATTRIBUTE_VALUE_TRANSLATION),
        Mockito.any(new TypeReference<Map<String, String>>() {}.getClass())))
        .thenReturn(attributeNameTranslationMap);

    Map<String, String> pristineListingAttributes = new HashMap<>();
    pristineListingAttributes.put("COLOR", "WHITE");
    pristineListingAttributes.put("ROM", "8 GB");
    pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    pristineItem.setPristineMasterId("PRISTINE_MASTER_ID");
    pristineItem.setPcbProductItemId("ITEM_CODE1");
    pristineItem.setPristineBrand("APPLE");
    pristineItem.setPristineCategory("HANDPHONE");
    pristineItem.setPristineModel("IPHONE");
    pristineItem.setProductCondition("Refurbished");
    pristineItem.setPristineProductName("PRODUCT_NAME1");
    pristineItem.setPristineListingAttributes(pristineListingAttributes);
    pristineItem.setPristineCategoriesHierarchy(new ArrayList<ItemCatalogVO>());
    pristineItem.setSalesCategorySequences(new ArrayList<SalesCategorySequence>());
    pristineItem.setDefaultProductCode("DEFAULT_PRISTINE_CODE");

    pristineItem2 = new PristineDataItem();
    pristineItem2.setPristineId("PRISTINE_ID2");
    pristineItem2.setPristineMasterId("PRISTINE_MASTER_ID2");
    pristineItem2.setPcbProductItemId("ITEM_CODE2");
    pristineItem2.setPristineBrand("APPLE");
    pristineItem2.setPristineCategory("HANDPHONE");
    pristineItem2.setPristineModel("IPHONE");
    pristineItem2.setProductCondition("Refurbished");
    pristineItem2.setPristineProductName("PRODUCT_NAME2");
    pristineItem2.setPristineListingAttributes(pristineListingAttributes);
    pristineItem2.setPristineCategoriesHierarchy(new ArrayList<ItemCatalogVO>());
    pristineItem2.setSalesCategorySequences(new ArrayList<SalesCategorySequence>());

    pristineItemList = new ArrayList<>();
    pristineItemList.add(pristineItem);
    pristineItemList.add(pristineItem2);

    item = new Item();
    item.setPristineDataItem(pristineItem);
    item.setItemSku(PristineServiceImplTest.ITEM_SKU);
    item.setItemCode(PristineServiceImplTest.ITEM_CODE);
    itemList.add(item);

    product = new Product();
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    productList.add(product);

    masterDataItems = new HashMap<>();
    MasterDataItem masterDataItem = new MasterDataItem();

    masterDataItems.put(PristineServiceImplTest.ITEM_CODE, masterDataItem);

    cacheItemVO = new CacheItemVO(PristineServiceImplTest.ITEM_CODE);

    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setLocationPath("LOCATION_PATH1");
    List<MasterDataItemImage> masterDataItemImages = new ArrayList<>();
    masterDataItemImages.add(masterDataItemImage);
    masterDataItem.setMasterDataItemImages(masterDataItemImages);

    String[] categoryListingParameters = {"color", "rom"};
    Map<String, String> attributes = new LinkedHashMap<>();
    attributes.put("Warna", "WHITE");
    attributes.put("Kapasitas Memori", "8 GB");

    when(productAttributesUtil.getCategoryListingParameterKey(eq(pristineItem))).thenReturn(categoryListingParameters);
    when(productAttributesUtil.translatePristineListingAttributeName(eq(pristineItem.getPristineListingAttributes()),
        Mockito.any(String[].class))).thenReturn(attributes);

  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.masterDataService, this.cacheItemHelperService, this.itemCacheableService,
        this.pristineItemRepository, this.productCacheableService, this.skuValidator, this.productAttributesUtil, masterDataConstructorService);
  }

  @Test
  public void getPristineItemsMappingByPristineId_PristineExists() throws Exception {

    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService.getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet())).thenReturn(masterDataItems);
    when(pristineCacheableService.findItemByPristine(
        mandatoryRequestParam.getStoreId(), pristineItem)).thenReturn(cacheItemVO);

    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");

    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService).getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(),
        pristineItem);

    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(pristineItem.getPristineProductName(), result.getName());
    Assertions.assertEquals(pristineItem.getPristineBrand(), result.getBrand());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(
        masterDataItems.get(cacheItemVO.getItemCode()).getMasterDataItemImages().get(0).getLocationPath(),
        result.getImageUrl());
    Assertions.assertEquals(pristineItem2.getPristineProductName(), result.getOtherPristineItems().get(1).getName());
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getOtherPristineItems().get(1).getId());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(expectedPristineAttributes,
        result.getOtherPristineItems().get(1).getAttributes());
  }

  @Test
  public void getPristineItemsMappingByPristineId_WithDefaultSku() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService.getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet())).thenReturn(masterDataItems);
    when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        PristineServiceImplTest.STORE_ID, PristineServiceImplTest.ITEM_SKU))
        .thenReturn(item);

    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID",
            PristineServiceImplTest.ITEM_SKU);
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService).getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet());
    verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU);
    verify(productAttributesUtil, times(1)).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
  }

  @Test
  public void getPristineItemsMappingByPristineId_WithDefaultSkuButItemNotFound() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        PristineServiceImplTest.STORE_ID, PristineServiceImplTest.ITEM_SKU))
        .thenReturn(null);
    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID",
            PristineServiceImplTest.ITEM_SKU);
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU);
    verify(productAttributesUtil, times(1)).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
  }

  @Test
  public void getPristineItemsMappingByPristineId_ItemNotExistTest() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem2, pristineItemList);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem2.getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    when(pristineCacheableService.findItemByPristine(
        mandatoryRequestParam.getStoreId(), pristineItem)).thenReturn(null);
    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, pristineItem2.getPristineId(), null);
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem2.getPristineId());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(),
        pristineItem2);
    verify(productAttributesUtil, times(1)).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getId());
  }

  @Test
  public void getPristineItemsMappingByPristineId_whenPristineIdNotExistTest() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO();
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);
    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(null, result.getId());
  }

  @Test
  public void getPristineItemsMappingByPristineId_ItemHasNoImages() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);

    Map<String, MasterDataItem> masterDataItems = new HashMap<>();
    masterDataItems.put(PristineServiceImplTest.ITEM_CODE, mock(MasterDataItem.class));
    when(masterDataService.getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet())).thenReturn(masterDataItems);
    when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        PristineServiceImplTest.STORE_ID, PristineServiceImplTest.ITEM_SKU))
        .thenReturn(item);

    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID",
            PristineServiceImplTest.ITEM_SKU);
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService).getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet());
    verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU);
    verify(productAttributesUtil, times(1)).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(StringUtils.EMPTY, result.getImageUrl());
  }

  @Test
  public void getPristineMasterIdByPristineIdTest() throws Exception {
    when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(pristineItem);
    when(skuValidator.isPristineId(PRISTINE_ID)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID);
    verify(pristineItemRepository).findByPristineId(PRISTINE_ID);
    verify(skuValidator).isPristineId(PRISTINE_ID);
    Assertions.assertEquals(PRISTINE_MASTER_ID, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByPristineIdTest_EmptyResponse() throws Exception {
    when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(null);
    when(skuValidator.isPristineId(PRISTINE_ID)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID);
    verify(pristineItemRepository).findByPristineId(PRISTINE_ID);
    verify(skuValidator).isPristineId(PRISTINE_ID);
    Assertions.assertEquals(StringUtils.EMPTY, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByProductSkuTest() throws Exception {
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(itemList);
    when(skuValidator.isPristineId(PRODUCT_SKU)).thenReturn(false);
    when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(skuValidator).isPristineId(PRODUCT_SKU);
    verify(skuValidator).isProductSku(PRODUCT_SKU);
    Assertions.assertEquals(PRISTINE_MASTER_ID, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByProductSkuTest_EmptyResponse() throws Exception {
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(new ArrayList<>());
    when(skuValidator.isPristineId(PRODUCT_SKU)).thenReturn(false);
    when(skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(skuValidator).isPristineId(PRODUCT_SKU);
    verify(skuValidator).isProductSku(PRODUCT_SKU);
    Assertions.assertEquals(StringUtils.EMPTY, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByProductCodeTest() throws Exception {
    when(productCacheableService.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productList);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
        STORE_ID, productList.get(0).getProductSku())).thenReturn(itemList);
    when(skuValidator.isPristineId(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductSku(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductCode(PRODUCT_CODE)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE);
    verify(productCacheableService).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
        STORE_ID, productList.get(0).getProductSku());
    verify(skuValidator).isPristineId(PRODUCT_CODE);
    verify(skuValidator).isProductSku(PRODUCT_CODE);
    verify(skuValidator).isProductCode(PRODUCT_CODE);
    Assertions.assertEquals(PRISTINE_MASTER_ID, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByProductCodeTest_EmptyResponse() throws Exception {
    when(productCacheableService.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(productList);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
        STORE_ID, productList.get(0).getProductSku())).thenReturn(new ArrayList<>());
    when(skuValidator.isPristineId(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductSku(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductCode(PRODUCT_CODE)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE);
    verify(productCacheableService).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
        STORE_ID, productList.get(0).getProductSku());
    verify(skuValidator).isPristineId(PRODUCT_CODE);
    verify(skuValidator).isProductSku(PRODUCT_CODE);
    verify(skuValidator).isProductCode(PRODUCT_CODE);
    Assertions.assertEquals(StringUtils.EMPTY, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterIdByProductCodeTest_EmptyProductList() throws Exception {
    List<Product> products = new ArrayList<>();
    when(productCacheableService.findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE))
        .thenReturn(products);
    when(skuValidator.isPristineId(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductSku(PRODUCT_CODE)).thenReturn(false);
    when(skuValidator.isProductCode(PRODUCT_CODE)).thenReturn(true);
    String pristineMasterIdResponse = pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, PRODUCT_CODE);
    verify(productCacheableService).findByStoreIdAndProductCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_CODE);
    verify(skuValidator).isPristineId(PRODUCT_CODE);
    verify(skuValidator).isProductSku(PRODUCT_CODE);
    verify(skuValidator).isProductCode(PRODUCT_CODE);
    Assertions.assertEquals(StringUtils.EMPTY, pristineMasterIdResponse);
  }

  @Test
  public void getPristineMasterId_ApplicationRuntimeExceptionTest_NullStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        null, USERNAME, REQUEST_ID, PRISTINE_ID));
  }

  @Test
  public void getPristineMasterId_ApplicationRuntimeExceptionTest_NullPristineIdOrProductCodeOrSku()
      throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> pristineServiceImpl.getPristineMasterIdByPristineIdOrProductCodeOrSku(
        STORE_ID, USERNAME, REQUEST_ID, null));
  }

  @Test
  public void getDefaultItemSkuTest_success() throws Exception{
    when(itemCacheableService.findItemsByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID, false))
        .thenReturn(itemList);
    DefaultItemSkuVO response =
        pristineServiceImpl.getDefaultItemSkuByPristineId(mandatoryRequestParam, PRISTINE_ID);

    verify(itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(ITEM_SKU, response.getDefaultItemSku());
  }

  @Test
  public void getDefaultItemSkuTest_noDiscoverableAndBuyableItem() throws Exception {
    when(itemCacheableService.findItemsByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID, false))
        .thenReturn(Collections.emptyList());
    DefaultItemSkuVO response =
        pristineServiceImpl.getDefaultItemSkuByPristineId(mandatoryRequestParam, PRISTINE_ID);

    verify(itemCacheableService).findItemsByPristineId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID, false);
    Assertions.assertNotNull(response);
    Assertions.assertEquals(StringUtils.EMPTY, response.getDefaultItemSku());
  }

  @Test
  public void getDefaultItemSkuTest_ApplicationRuntimeExceptionTest_NullStoreId()
      throws Exception {
    try {
      mandatoryRequestParam.setStoreId(null);
      pristineServiceImpl.getDefaultItemSkuByPristineId(mandatoryRequestParam, PRISTINE_ID);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void getDefaultItemSkuTest_ApplicationRuntimeExceptionTest_NullPristineId()
      throws Exception {
    try {
      pristineServiceImpl.getDefaultItemSkuByPristineId(mandatoryRequestParam, null);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
    }
  }

  @Test
  public void getPristineMasterIdByPristineIdWithRandomPristineCodeTest() throws Exception {
    when(skuValidator.isPristineId(PRISTINE_ID)).thenReturn(false);
    when(skuValidator.isProductSku(PRISTINE_ID)).thenReturn(false);
    when(skuValidator.isProductCode(PRISTINE_ID)).thenReturn(false);
    String pristineMasterIdResponse = pristineServiceImpl
        .getPristineMasterIdByPristineIdOrProductCodeOrSku(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_ID);
    verify(skuValidator).isPristineId(PRISTINE_ID);
    verify(skuValidator).isProductSku(PRISTINE_ID);
    verify(skuValidator).isProductCode(PRISTINE_ID);
    Assertions.assertEquals(StringUtils.EMPTY, pristineMasterIdResponse);
  }

  @Test
  public void getPristineItemsMappingByPristineId_PristineExistsWithNullBrandNullModel() throws Exception {
    pristineItem.setPristineBrand(null);
    pristineItem.setPristineModel(null);
    pristineItem.setPristineListingAttributes(null);
    pristineItemList.get(0).setPristineBrand(null);
    pristineItemList.get(0).setPristineModel(null);
    pristineItemList.get(0).setPristineListingAttributes(null);
    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(pristineItem, pristineItemList);
    PristineDataItem pristineDataItem = new PristineDataItem();
    BeanUtils.copyProperties(pristineItem, pristineDataItem);
    when(pristineCacheableService
        .findPristineItemAndItsSiblingsByPristineId(mandatoryRequestParam.getStoreId(), pristineItem.getPristineId()))
        .thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService
        .getMasterDataItems(eq(mandatoryRequestParam.getStoreId()), eq(mandatoryRequestParam.getUsername()),
            eq(mandatoryRequestParam.getRequestId()), Mockito.anySet())).thenReturn(masterDataItems);
    when(pristineCacheableService.findItemByPristine(mandatoryRequestParam.getStoreId(), pristineItem))
        .thenReturn(cacheItemVO);
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(item)).thenReturn(item);
    when(pristineCacheableService.findFirstItemByPristine(pristineItem)).thenReturn(item);
    PristineItemDetailAndMappingVo result =
        this.pristineServiceImpl.getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3))
        .translatePristineListingAttributeName(Mockito.anyMap(), Mockito.any());
    verify(pristineCacheableService)
        .findPristineItemAndItsSiblingsByPristineId(mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService)
        .getMasterDataItems(eq(mandatoryRequestParam.getStoreId()), eq(mandatoryRequestParam.getUsername()),
            eq(mandatoryRequestParam.getRequestId()), Mockito.anySet());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(), pristineItem);

    verify(masterDataConstructorService, times(2)).constructPristineDataItemWithMasterData(item);
    verify(pristineCacheableService, times(2)).findFirstItemByPristine(pristineItem);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(pristineItem.getPristineProductName(), result.getName());
    Assertions.assertEquals(pristineDataItem.getPristineBrand(), result.getBrand());
    Assertions.assertEquals(masterDataItems.get(cacheItemVO.getItemCode()).getMasterDataItemImages().get(0).getLocationPath(),
            result.getImageUrl());
    Assertions.assertEquals(pristineItem2.getPristineProductName(), result.getOtherPristineItems().get(1).getName());
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getOtherPristineItems().get(1).getId());

    Assertions.assertEquals(new HashMap<>(), result.getAttributes());
    Assertions.assertEquals(new HashMap<>(), result.getOtherPristineItems().get(0).getAttributes());
    Assertions.assertEquals(expectedPristineAttributes, result.getOtherPristineItems().get(1).getAttributes());
  }

  @Test
  public void getPristineItemsMappingByPristineId_PristineExistsWithNullBrand() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    PristineDataItem pristineDataItem = new PristineDataItem();
    BeanUtils.copyProperties(pristineItem, pristineDataItem);
    pristineItem.setPristineBrand(null);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService.getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet())).thenReturn(masterDataItems);
    when(pristineCacheableService.findItemByPristine(
        mandatoryRequestParam.getStoreId(), pristineItem)).thenReturn(cacheItemVO);
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(item)).thenReturn(item);
    when(pristineCacheableService.findFirstItemByPristine(pristineItem)).thenReturn(item);

    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");

    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService).getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(),
        pristineItem);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(pristineItem.getPristineProductName(), result.getName());
    Assertions.assertNull(result.getBrand());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(
        masterDataItems.get(cacheItemVO.getItemCode()).getMasterDataItemImages().get(0).getLocationPath(),
        result.getImageUrl());
    Assertions.assertEquals(pristineItem2.getPristineProductName(), result.getOtherPristineItems().get(1).getName());
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getOtherPristineItems().get(1).getId());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(expectedPristineAttributes,
        result.getOtherPristineItems().get(1).getAttributes());
  }

  @Test
  public void getPristineItemsMappingByPristineId_PristineExistsWithNullModel() throws Exception {
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(
        pristineItem, pristineItemList);
    PristineDataItem pristineDataItem = new PristineDataItem();
    BeanUtils.copyProperties(pristineItem, pristineDataItem);
    pristineItem.setPristineModel(null);
    when(pristineCacheableService.findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId())).thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService.getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet())).thenReturn(masterDataItems);
    when(pristineCacheableService.findItemByPristine(
        mandatoryRequestParam.getStoreId(), pristineItem)).thenReturn(cacheItemVO);
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    when(masterDataConstructorService.constructPristineDataItemWithMasterData(item)).thenReturn(item);
    when(pristineCacheableService.findFirstItemByPristine(pristineItem)).thenReturn(item);

    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");

    PristineItemDetailAndMappingVo result = this.pristineServiceImpl
        .getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil, times(3)).translatePristineListingAttributeName(
        eq(pristineItem.getPristineListingAttributes()), Mockito.any());
    verify(pristineCacheableService).findPristineItemAndItsSiblingsByPristineId(
        mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService).getMasterDataItems(eq(mandatoryRequestParam.getStoreId()),
        eq(mandatoryRequestParam.getUsername()), eq(mandatoryRequestParam.getRequestId()),
        Mockito.anySet());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(),
        pristineItem);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(pristineItem.getPristineProductName(), result.getName());
    Assertions.assertEquals(pristineDataItem.getPristineBrand(), result.getBrand());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(
        masterDataItems.get(cacheItemVO.getItemCode()).getMasterDataItemImages().get(0).getLocationPath(),
        result.getImageUrl());
    Assertions.assertEquals(pristineItem2.getPristineProductName(), result.getOtherPristineItems().get(1).getName());
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getOtherPristineItems().get(1).getId());
    Assertions.assertEquals(expectedPristineAttributes, result.getAttributes());
    Assertions.assertEquals(expectedPristineAttributes,
        result.getOtherPristineItems().get(1).getAttributes());
  }

  @Test
  public void getPristineItemsMappingByPristineId_nullPristineItem() throws Exception {
    pristineItem.setPristineBrand(null);
    pristineItem.setPristineModel(null);
    pristineItem.setPristineListingAttributes(null);
    pristineItemList.get(0).setPristineBrand(null);
    pristineItemList.get(0).setPristineModel(null);
    pristineItemList.get(0).setPristineListingAttributes(null);
    Map<String, String> expectedPristineAttributes = new LinkedHashMap<>();
    expectedPristineAttributes.put("Warna", "WHITE");
    expectedPristineAttributes.put("Kapasitas Memori", "8 GB");
    PristineItemAndSiblingsVO pristineItemAndSiblingsVO = new PristineItemAndSiblingsVO(pristineItem, pristineItemList);
    PristineDataItem pristineDataItem = new PristineDataItem();
    BeanUtils.copyProperties(pristineItem, pristineDataItem);
    when(pristineCacheableService
        .findPristineItemAndItsSiblingsByPristineId(mandatoryRequestParam.getStoreId(), pristineItem.getPristineId()))
        .thenReturn(pristineItemAndSiblingsVO);

    when(masterDataService
        .getMasterDataItems(eq(mandatoryRequestParam.getStoreId()), eq(mandatoryRequestParam.getUsername()),
            eq(mandatoryRequestParam.getRequestId()), Mockito.anySet())).thenReturn(masterDataItems);
    when(pristineCacheableService.findItemByPristine(mandatoryRequestParam.getStoreId(), pristineItem))
        .thenReturn(cacheItemVO);
    when(pristineCacheableService.findFirstItemByPristine(pristineItem)).thenReturn(null);
    PristineItemDetailAndMappingVo result =
        this.pristineServiceImpl.getPristineItemsMappingByPristineId(mandatoryRequestParam, "PRISTINE_ID", null);
    verify(productAttributesUtil).getCategoryListingParameterKey(eq(pristineItem));
    verify(productAttributesUtil)
        .translatePristineListingAttributeName(Mockito.anyMap(), Mockito.any());
    verify(productAttributesUtil)
        .translatePristineListingAttributeName(Mockito.isNull(), Mockito.any());
    verify(pristineCacheableService)
        .findPristineItemAndItsSiblingsByPristineId(mandatoryRequestParam.getStoreId(), pristineItem.getPristineId());
    verify(masterDataService)
        .getMasterDataItems(eq(mandatoryRequestParam.getStoreId()), eq(mandatoryRequestParam.getUsername()),
            eq(mandatoryRequestParam.getRequestId()), Mockito.anySet());
    verify(pristineCacheableService).findItemByPristine(mandatoryRequestParam.getStoreId(), pristineItem);

    verify(pristineCacheableService, times(2)).findFirstItemByPristine(pristineItem);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(pristineItem.getPristineId(), result.getId());
    Assertions.assertEquals(pristineItem.getPristineProductName(), result.getName());
    Assertions.assertEquals(pristineDataItem.getPristineBrand(), result.getBrand());
    Assertions.assertEquals(masterDataItems.get(cacheItemVO.getItemCode()).getMasterDataItemImages().get(0).getLocationPath(),
            result.getImageUrl());
    Assertions.assertEquals(pristineItem2.getPristineProductName(), result.getOtherPristineItems().get(0).getName());
    Assertions.assertEquals(pristineItem2.getPristineId(), result.getOtherPristineItems().get(0).getId());
    Assertions.assertEquals(new HashMap<>(), result.getAttributes());
    Assertions.assertEquals(expectedPristineAttributes, result.getOtherPristineItems().get(0).getAttributes());
  }

}
