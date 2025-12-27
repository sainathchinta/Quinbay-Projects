package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.util.ReflectionTestUtils.setField;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.promotion.enums.PromoBundlingType;

public class ProductAndItemSolrConstructorServiceImplTest {

  private static final String XPRODUCT_SOLR_REINDEX = "xproduct-solr-reindex";
  private static final String SOLR_DELIMITER = "#_#";
  private static final String CHANNEL_2 = "channel2";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String STORE_ID = "10001";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String CATALOG_CODE = "catalogCode";
  public static final String ITEM_CODE = "itemCode";
  private List<String> oldSalesCategoryList;
  private List<String> newSalesCategoryList;
  private List<String> newUmkmSalesCategoryList;
  private List<String> solrSalesCategory;
  private List<String> solrUmkmSalesCategory;
  private static final String SALES_CATEGORY_WITH_CATALOG_CODE = "12051#_#";
  private static final String OLD_CAT_1 = "55024";
  private static final String OLD_CAT_2 = "AP-1000019";
  private static final String OLD_CAT_3 = "DR-1000007";
  private static final String NEW_CAT_1 = "54914";
  private static final String NEW_CAT_2 = "FA-1000030";
  private static final String NEW_UMKM_CAT_1 = "newUmkm-1";
  private static final String NEW_UMKM_CAT_2 = "newUmkm-2";
  private static final String PRODUCT_SKU = "productSku";
  private static final String LOCATION_PATH = "location";

  @InjectMocks
  private ProductAndItemSolrConstructorServiceImpl service;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  private ProductAndItemSolr productAndItemSolr;

  private ProductDomainEventModel productDomainEventModel;

  private Item item;

  private ItemPickupPoint itemPickupPoint;

  private PristineDataItem pristineDataItem;

  private Product product;

  private OfflineItem offlineItem;

  private List<OfflineItem> offlineItemList;

  private Map<String, Double> productAndTotalScoreMap = new HashMap<>();

  private SystemParameter systemParameter;

  private ImageResponse imageResponse = new ImageResponse();

  @Test
  public void constructByMasterDataChangeModelBlankCatalogCodeTest() {
    String catalogString = ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.CATEGORY_CODE;
    this.productDomainEventModel.getProductCategories().get(0).getCategory().getCatalog().setCatalogCode("");
    this.productAndItemSolr.setMasterCatalog(catalogString);
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 2);
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelSyncTest() {
    this.productAndItemSolr.setSynchronized(true);
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 5);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(result.get(SolrFieldNames.PRODUCT_NAME).getNewValue(), productDomainEventModel.getName());
    assertEquals(result.get(SolrFieldNames.ITEM_NAME).getNewValue(),
        productDomainEventModel.getProductItems().get(0).getGeneratedItemName());
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelTest() {
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelTest_totalScoreChange() {
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
    assertEquals(10.71, Double.parseDouble(result.get(SolrFieldNames.TOTAL_SCORE).getNewValue()), 0.0);
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeTest() {
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(oldSalesCategoryList, newSalesCategoryList, new ArrayList<>(),
            new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeWithNewUmkmSalesCategoryTest() {
    Mockito.when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(false);
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(oldSalesCategoryList, newSalesCategoryList, newUmkmSalesCategoryList,
            new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeWithNewUmkmSalesCategoryForUmkmMerchantTest() {
    Mockito.when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(oldSalesCategoryList, newSalesCategoryList, newUmkmSalesCategoryList,
            new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeWithyNewUmkmSalesTest() {
    Mockito.when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(oldSalesCategoryList, null, newUmkmSalesCategoryList, new ArrayList<>(),
            new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeWithOnlyNewUmkmSalesTest() {
    Mockito.when(businessPartnerService.isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE)).thenReturn(true);
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(null, null, newUmkmSalesCategoryList, new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Mockito.verify(businessPartnerService).isBusinessPartnerUmkmMerchant(STORE_ID, MERCHANT_CODE);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeEmptyOldSalesCategoryTest() {
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(null, newSalesCategoryList, null, new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeEmptyNewSalesCategoryTest() {
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(oldSalesCategoryList, null, null, new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructByMasterDataChangeModelWithCategoryChangeEmptySalesCategoryTest() {
    productDomainEventModel.setProductSalesCategoryMapping(
        new ProductSalesCategoryMapping(null, null, null, new ArrayList<>(), new ArrayList<>(),true));
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertTrue(result.size() == 3);
    assertEquals(result.get(SolrFieldNames.MASTER_CATALOG).getNewValue(), (
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE));
    assertEquals(ModelHelper.IS_MAIN_IMAGE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.LOCATION_PATH + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.SEQUENCE, result.get(SolrFieldNames.ITEM_IMAGES).getNewListValues().get(0));
  }

  @Test
  public void constructItemSyncAndGetMasterDataTest() throws Exception {
    when(this.productHelperService.setMasterDataItemFromMasterData(eq(this.item.getStoreId()), ArgumentMatchers.anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), Mockito.any(Item.class)))
        .thenReturn(this.item);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.item.setSynchronized(true);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, true);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem().getGeneratedItemName()));
    verify(this.productHelperService).setMasterDataItemFromMasterData(eq(this.item.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), Mockito.any(Item.class));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
  }

  @Test
  public void getOfflineItemPriceTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(100.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(100.0);
    itemPickupPoint.setPickupPointCode(ITEM_SKU);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    List<String> offlinePrice =
        this.service.getOfflinePricesByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    assertEquals(Arrays.asList("itemSku#_#100.0#_#100.0"), offlinePrice);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
  }

  @Test
  public void constructItemSyncTest() throws Exception {
    item.setPristineDataItem(pristineDataItem);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.item.setSynchronized(true);
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(new DiscountPrice());
      return price;
    });
    this.item.setMerchantPromoDiscount(true);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem().getGeneratedItemName()));
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    assertEquals(this.item.getMasterDataItem().getGeneratedItemName(), this.productAndItemSolr.getItemName());
    assertEquals(ModelHelper.CHANNEL + "#_#0", productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscount());
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscountActivated());
  }

  @Test
  public void constructItemWholesalePriceExistsTrueTest() throws Exception{
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.item.setSynchronized(true);
    this.item.setWholesalePriceExists(true);
    Set<String> activePromoBundlings = new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE));
    this.item.setActivePromoBundlings(activePromoBundlings);
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(new DiscountPrice());
      return price;
    });
    this.item.setMerchantPromoDiscount(true);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem()
        .getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    assertEquals(ModelHelper.CHANNEL + "#_#0" , productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscount());
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscountActivated());
    Assertions.assertTrue(productAndItemSolr.getWholesalePriceActivated());
    Assertions.assertNull(productAndItemSolr.getPristineId());
  }

  @Test
  public void constructItemWholesalePriceExistsTrueWithoutWholesalePriceTest() throws Exception{
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    this.item.setSynchronized(true);
    this.item.setWholesalePriceExists(true);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Set<String> activePromoBundlings = new HashSet<>(Arrays.asList(PromoBundlingType.WHOLESALE.getPromoType()));
    this.item.setActivePromoBundlings(activePromoBundlings);
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(new DiscountPrice());
      return price;
    });
    this.item.setMerchantPromoDiscount(true);
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem()
        .getGeneratedItemName()));
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    assertEquals(ModelHelper.CHANNEL + "#_#0" , productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscount());
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscountActivated());
    Assertions.assertFalse(productAndItemSolr.getWholesalePriceActivated());
  }

  @Test
  public void constructItemWholesalePriceExistsFalseTest() throws Exception {
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    this.item.setSynchronized(true);
    this.item.setWholesalePriceExists(false);
    Set<String> activePromoBundlings = new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE));
    this.item.setActivePromoBundlings(activePromoBundlings);
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(new DiscountPrice());
      return price;
    });
    this.item.setMerchantPromoDiscount(true);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem()
        .getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    assertEquals(ModelHelper.CHANNEL + "#_#0" , productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscount());
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscountActivated());
    Assertions.assertNull(productAndItemSolr.getWholesalePriceActivated());
  }

  @Test
  public void constructItemUnsynchronousTest() throws Exception {
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(null);
      return price;
    });
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem().getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    assertEquals(this.item.getMasterDataItem().getGeneratedItemName(), this.productAndItemSolr.getItemName());
    assertEquals(ModelHelper.CHANNEL + "#_#200", productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertFalse(productAndItemSolr.isMerchantPromoDiscount());
  }

  @Test
  public void constructItemUnsynchronousFetchOnlyItemImageDataTest() throws Exception {
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(null);
      return price;
    });
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.emptyList());
    if (response == null) {
      response = new GdnRestListResponse<ItemImageResponse>();
    }
    systemParameter.setValue("true");
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
    Mockito.when(productCategoryBaseOutbound.getProductItemImagesByItemCode(skuCodesRequest)).thenReturn(response);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals("generatedItemName", (this.item.getMasterDataItem().getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(productCategoryBaseOutbound).getProductItemImagesByItemCode(skuCodesRequest);
    assertEquals("generatedItemName", this.productAndItemSolr.getItemName());
    assertEquals(ModelHelper.CHANNEL + "#_#200", productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertFalse(productAndItemSolr.isMerchantPromoDiscount());
  }

  @Test
  public void constructItemUnsynchronousFetchOnlyItemImageDataWithImagesTest() throws Exception {
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(null);
      return price;
    });
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_SKU));
    item.setItemCode(ITEM_SKU);
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.singletonList(ItemImageResponse.builder().imageResponses(Collections.singletonList(imageResponse)).build()));
    systemParameter.setValue("true");
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    Mockito.when(productCategoryBaseOutbound.getProductItemImagesByItemCode(skuCodesRequest)).thenReturn(response);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem().getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(productCategoryBaseOutbound).getProductItemImagesByItemCode(skuCodesRequest);
    assertEquals(this.item.getMasterDataItem().getGeneratedItemName(), this.productAndItemSolr.getItemName());
    assertEquals(ModelHelper.CHANNEL + "#_#200", productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertFalse(productAndItemSolr.isMerchantPromoDiscount());
  }

  @Test
  public void constructItemNullItemCodeFetchOnlyItemImageDataWithImagesTest() throws Exception {
    systemParameter.setValue("true");
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_SKU));
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.emptyList());
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(productCategoryBaseOutbound.getProductItemImagesByItemCode(skuCodesRequest)).thenReturn(response);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    this.item.setSynchronized(true);
    this.item.setItemCode(null);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setLocationPath("image-path");
    item.getMasterDataItem().setMasterDataItemImages(Arrays.asList(masterDataItemImage));
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.service.constructItem(this.productAndItemSolr, this.item, true);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    assertEquals("true#_#image-path#_#0", productAndItemSolr.getItemImages().get(0));
  }

  @Test
  public void constructItemViewConfigsTest() {
    List<String> buyables = new ArrayList<String>();
    List<String> discoverables = new ArrayList<String>();
    buyables.add(ModelHelper.CHANNEL + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ModelHelper.BUYABLE);
    buyables.add(ProductAndItemSolrConstructorServiceImplTest.CHANNEL_2
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.BUYABLE);
    discoverables.add(ModelHelper.CHANNEL
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.DISCOVERABLE);

    Set<ItemViewConfig> expectedResult = new HashSet<ItemViewConfig>();
    expectedResult.add(new ItemViewConfig(ModelHelper.BUYABLE, ModelHelper.DISCOVERABLE,
        ModelHelper.CHANNEL, null, null));
    expectedResult.add(new ItemViewConfig(ModelHelper.BUYABLE, false,
        ProductAndItemSolrConstructorServiceImplTest.CHANNEL_2, null, null));

    Set<ItemViewConfig> result = this.service.constructItemViewConfigs(buyables, discoverables);
    assertEquals(result, (expectedResult));
  }

  @Test
  public void constructItemNullItemCodeTest() throws Exception {
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    this.item.setSynchronized(true);
    this.item.setItemCode(null);
    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setLocationPath("image-path");
    item.getMasterDataItem().setMasterDataItemImages(Arrays.asList(masterDataItemImage));
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.service.constructItem(this.productAndItemSolr, this.item, true);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    assertEquals("true#_#image-path#_#0", productAndItemSolr.getItemImages().get(0));
  }

  @Test
  public void constructMasterCatalogTest() {
    String catalogString =
        ModelHelper.CATALOG_CODE + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
            + ModelHelper.CATEGORY_CODE;
    MasterCatalog expectedResult =
        new MasterCatalog(ModelHelper.CATALOG_CODE, new Category(ModelHelper.CATEGORY_CODE,
            ModelHelper.CATEGORY_CODE));
    MasterCatalog result = this.service.constructMasterCatalog(catalogString);
    assertEquals(result, (expectedResult));
  }

  @Test
  public void constructMasterDataItemMainImagesTest() {
    List<String> imageString = new ArrayList<String>();
    imageString.add(ModelHelper.IS_MAIN_IMAGE
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.LOCATION_PATH
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.SEQUENCE);
    List<MasterDataItemImage> expectedResult = new ArrayList<MasterDataItemImage>();
    expectedResult.add(new MasterDataItemImage(ModelHelper.IS_MAIN_IMAGE,
        ModelHelper.LOCATION_PATH, ModelHelper.SEQUENCE));
    List<MasterDataItemImage> result = this.service.constructMasterDataItemMainImages(imageString);
    assertEquals(result, (expectedResult));
  }

  @Test
  public void constructProductSyncGetMasterDataTrueTest() throws Exception {
    this.product.setSynchronized(true);
    product.setCurationStatus(CurationStatus.NONE);
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90.7100001));
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(90.71, productAndItemSolr.getProductScoreTotal(), 0);
  }


  @Test
  public void nullCheckForMasterCatalogInConstructProductTest() throws Exception {
    this.product.setSynchronized(true);
    product.setCurationStatus(CurationStatus.NONE);
    product.setProductCode(null);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(new Category());
    product.setMasterCatalog(masterCatalog);
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90.7100001));
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(90.71, productAndItemSolr.getProductScoreTotal(), 0);
  }

  @Test
  public void nullCheckforMasterCatalogAndMasterCategoryInConstructProductTest() throws Exception {
    this.product.setSynchronized(true);
    product.setCurationStatus(CurationStatus.NONE);
    product.setProductCode(null);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(null);
    product.setMasterCatalog(masterCatalog);
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90.7100001));
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(90.71, productAndItemSolr.getProductScoreTotal(), 0);
  }

  @Test
  public void constructProductSyncGetMasterDataMasterCategoryNullTest() throws Exception {
    this.product.setSynchronized(true);
    product.setCurationStatus(CurationStatus.NONE);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CATALOG_CODE);
    product.setMasterCatalog(masterCatalog);
    product.getMasterDataProduct().setMasterCatalog(masterCatalog);
    product.getMasterCatalog().setCategory(new Category());
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90.7100001));
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(90.71, productAndItemSolr.getProductScoreTotal(), 0);
  }

  @Test
  public void constructProductSyncGetMasterDataMasterCategoryNull2Test() throws Exception {
    this.product.setSynchronized(true);
    product.setCurationStatus(CurationStatus.NONE);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CATALOG_CODE);
    product.setMasterCatalog(masterCatalog);
    product.getMasterDataProduct().setMasterCatalog(masterCatalog);
    product.getMasterCatalog().setCategory(null);
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90.7100001));
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(90.71, productAndItemSolr.getProductScoreTotal(), 0);
  }

  @Test
  public void constructProductWithTotalScore() throws Exception {
    this.product.setSynchronized(true);
    this.product.setProductScore(new ProductScore());
    this.product.getProductScore().setTotalScore(80.0);
    product.setCurationStatus(CurationStatus.NONE);
    when(
        this.productHelperService.setMasterDataProductFromMasterData(eq(this.product.getStoreId()),
            anyString(), eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX),
            eq(this.product))).thenReturn(this.product);
    this.service.constructProduct(this.productAndItemSolr, this.product, true);
    verify(this.productHelperService).setMasterDataProductFromMasterData(
        eq(this.product.getStoreId()), anyString(),
        eq(ProductAndItemSolrConstructorServiceImplTest.XPRODUCT_SOLR_REINDEX), eq(this.product));
    assertEquals(80.0, productAndItemSolr.getProductScoreTotal(), 1);
  }

  @Test
  public void constructProductSyncTest() {
    this.product.setSynchronized(true);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    this.product.setPreOrder(preOrder);
    product.setCurationStatus(CurationStatus.NONE);
    this.service.constructProduct(this.productAndItemSolr, this.product, false);
    Assertions.assertEquals(productAndItemSolr.getProductCode(), product.getProductCode());
  }

  @Test
  public void constructProductUnsynchronousTest() {
    this.product.setPreOrder(new PreOrder());
    this.product.setCurationStatus(CurationStatus.NONE);
    this.service.constructProduct(this.productAndItemSolr, this.product, false);
    Assertions.assertEquals(productAndItemSolr.getProductCode(), product.getProductCode());
  }

  @Test
  public void constructSalesCatalogsTest() {
    List<String> catalogString = new ArrayList<String>();
    List<SalesCatalog> expectedResult = new ArrayList<SalesCatalog>();
    List<Category> listOfCategories = new ArrayList<Category>();

    catalogString.add(ModelHelper.CATALOG_CODE
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.CATEGORY_CODE);
    catalogString.add(ModelHelper.CATALOG_CODE
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER
        + ProductAndItemSolrConstructorServiceImplTest.CATEGORY_CODE_2);
    listOfCategories.add(new Category(ModelHelper.CATEGORY_CODE, ModelHelper.CATEGORY_CODE));
    listOfCategories.add(new Category(ProductAndItemSolrConstructorServiceImplTest.CATEGORY_CODE_2,
        ProductAndItemSolrConstructorServiceImplTest.CATEGORY_CODE_2));

    SalesCatalog salesCatalog = new SalesCatalog(ModelHelper.CATALOG_CODE);
    salesCatalog.setListOfCategories(listOfCategories);

    expectedResult.add(salesCatalog);

    List<SalesCatalog> result = this.service.constructSalesCatalogs(catalogString);
    assertEquals(result, (expectedResult));
  }

  @Test
  public void constructSalesCatalogsWithNullParamTest() {
    List<String> catalogString = null;
    List<SalesCatalog> result = this.service.constructSalesCatalogs(catalogString);
    assertEquals(result, (new ArrayList<SalesCatalog>()));
  }

  @Test
  public void setSolrStringDelimiterTest() {
    String solrStringDelimiter = null;
    this.service.setSolrStringDelimiter(solrStringDelimiter);
  }

  @BeforeEach
  public void setUp() throws Exception {
    ModelHelper modelHelper = new ModelHelper();
    openMocks(this);
    setField(this.service, "solrStringDelimiter",
        ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER);
    this.productAndItemSolr = modelHelper.setupProductAndItemSolr();
    this.productDomainEventModel = modelHelper.setupProductDomainEventModel();
    this.item = modelHelper.setupItem();
    this.item.setStoreId(STORE_ID);

    this.itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint = modelHelper.setupItemPickupPoint();
    this.product = modelHelper.setupProduct();

    this.offlineItem = new OfflineItem();
    this.offlineItemList = new ArrayList<>();
    this.offlineItemList.add(offlineItem);

    oldSalesCategoryList = new ArrayList<>(Arrays.asList(OLD_CAT_1, OLD_CAT_2, OLD_CAT_3));
    newSalesCategoryList = new ArrayList<>(Arrays.asList(NEW_CAT_1,NEW_CAT_2));
    newUmkmSalesCategoryList = new ArrayList<>(Arrays.asList(NEW_UMKM_CAT_1, NEW_UMKM_CAT_2));

    solrSalesCategory = new ArrayList<>(Arrays.asList(SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_CAT_1),
        SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_CAT_2)));

    solrSalesCategory = new ArrayList<>(Arrays.asList(SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_CAT_1),
        SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_CAT_2)));

    solrUmkmSalesCategory = new ArrayList<>(Arrays.asList(SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_UMKM_CAT_1),
        SALES_CATEGORY_WITH_CATALOG_CODE.concat(NEW_UMKM_CAT_2)));

    ReflectionTestUtils.setField(service, "salesCategoryCatalogCode", "12051");
    productAndItemSolr.setProductSku(PRODUCT_SKU);
    productAndItemSolr.setProductScoreTotal(20.710001);
    productAndTotalScoreMap.put(PRODUCT_SKU, 10.7100001);
    pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("PRISTINE_ID");

    systemParameter = new SystemParameter();
    systemParameter.setValue("false");

    imageResponse = new ImageResponse();
    imageResponse.setMainImage(true);
    imageResponse.setLocationPath(LOCATION_PATH);
    imageResponse.setSequence(0);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.offlineItemService);
    verifyNoMoreInteractions(this.businessPartnerService, this.productCategoryBaseOutbound);
    verifyNoMoreInteractions(this.itemPickupPointService, this.systemParameterService);
  }

  @Test
  public void constructItemSyncTest_emptyMasterDataImages() throws Exception {
    item.getMasterDataItem().setMasterDataItemImages(null);
    Mockito.when(offlineItemService.findByItemSku(STORE_ID, ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(productHelperService
        .setMasterDataItemFromMasterData(stringArgumentCaptor.capture(), stringArgumentCaptor.capture(),
            stringArgumentCaptor.capture(), itemArgumentCaptor.capture())).thenReturn(item);
    Mockito.when(
        itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(anyString(), anyString())).thenReturn(systemParameter);
    this.item.setSynchronized(true);
    item.getPrice().stream().findFirst().map(price -> {
      price.setMerchantPromoDiscountPrice(new DiscountPrice());
      return price;
    });
    this.item.setMerchantPromoDiscount(true);
    this.service.constructItem(this.productAndItemSolr, this.item, false);
    assertEquals(this.productAndItemSolr.getItemName(), (this.item.getMasterDataItem().getGeneratedItemName()));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(productHelperService).setMasterDataItemFromMasterData(stringArgumentCaptor.getAllValues().get(0),
        stringArgumentCaptor.getAllValues().get(1), stringArgumentCaptor.getAllValues().get(2),
        itemArgumentCaptor.getValue());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(anyString(), anyString());
    assertEquals(this.item.getMasterDataItem().getGeneratedItemName(), this.productAndItemSolr.getItemName());
    assertEquals(ModelHelper.CHANNEL + "#_#0", productAndItemSolr.getOfferPrice().get(0));
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscount());
    Assertions.assertTrue(productAndItemSolr.isMerchantPromoDiscountActivated());
  }

  @Test
  public void constructByMasterDataChangeModelTest_emptyScoreMap_noChange() {
    productAndTotalScoreMap = new HashMap<>();
    productAndItemSolr.setMasterCatalog(CATALOG_CODE + SOLR_DELIMITER + CATEGORY_CODE);
    productDomainEventModel.getProductItems().get(0).setImages(null);
    Map<String, FieldValueObject> result =
        this.service.constructByMasterDataChangeModel(this.productAndItemSolr, this.productDomainEventModel,
            productAndTotalScoreMap);
    Assertions.assertNull(result);
  }

  @Test
  public void constructMasterDataItemImagesTest() {
    List<String> imageString = new ArrayList<>();
    imageString.add(ModelHelper.IS_MAIN_IMAGE
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.LOCATION_PATH
        + ProductAndItemSolrConstructorServiceImplTest.SOLR_DELIMITER + ModelHelper.SEQUENCE);
    List<MasterDataItemImage> expectedResult = new ArrayList<MasterDataItemImage>();
    expectedResult.add(new MasterDataItemImage(ModelHelper.IS_MAIN_IMAGE,
        ModelHelper.LOCATION_PATH, ModelHelper.SEQUENCE));
    List<MasterDataItemImage> result = this.service.constructMasterDataItemImages(imageString);
    assertEquals(result, (expectedResult));
  }

  @Test
  public void constructMasterDataItemImagesNullTest() {
    List<String> imageString = null;
    List<MasterDataItemImage> expectedResult = new ArrayList<MasterDataItemImage>();
    List<MasterDataItemImage> result = this.service.constructMasterDataItemImages(imageString);
    assertEquals(result, (expectedResult));
  }
}
