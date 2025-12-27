package com.gdn.x.product.service.util;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

/**
 * @author nitinmathew - created on 31/01/2020
 **/
public class ItemSummaryUtilImplTest {
  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String PRODUCT_NAME = "product-name";

  private static final String PRODUCT_SKU = "PRODUCT-SKU";

  private static final boolean IS_LATE_FULFILLMENT = true;

  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ITEM_NAME = "item-name";

  private static final String ITEM_SKU = "ITEM-SKU";

  private static final String DELIMITER = "delimiter";

  private static final String MASTER_CATALOG = "catalog" + ItemSummaryUtilImplTest.DELIMITER
      + "categoryCode";

  private static final List<String> SALES_CATALOG = Arrays.asList("salesCatalog1"
      + ItemSummaryUtilImplTest.DELIMITER + "salesCategoryCode1", "salesCatalog1"
      + ItemSummaryUtilImplTest.DELIMITER + "salesCategoryCode2", "salesCatalog2"
      + ItemSummaryUtilImplTest.DELIMITER + "salesCategoryCode3", "salesCatalog2"
      + ItemSummaryUtilImplTest.DELIMITER + "salesCategoryCode4");

  private static final List<String> DISCOVERABLE = Arrays.asList(ChannelName.DEFAULT
      + ItemSummaryUtilImplTest.DELIMITER + "true", ChannelName.ANDROID
      + ItemSummaryUtilImplTest.DELIMITER + "false");

  private static final List<String> BUYABLE = Arrays.asList(ChannelName.DEFAULT
      + ItemSummaryUtilImplTest.DELIMITER + "true", ChannelName.ANDROID
      + ItemSummaryUtilImplTest.DELIMITER + "false");

  private static final List<String> ITEM_IMAGES = Arrays.asList("true"
      + ItemSummaryUtilImplTest.DELIMITER + "imagepath1" + ItemSummaryUtilImplTest.DELIMITER
      + "0", "false" + ItemSummaryUtilImplTest.DELIMITER + "imagepath2"
      + ItemSummaryUtilImplTest.DELIMITER + "1");

  private static final List<String> OFFLINE_PRICES = Arrays.asList(
      ItemSummaryUtilImplTest.PICKUP_POINT_CODE + ItemSummaryUtilImplTest.DELIMITER + "10000",
      "pp-02" + ItemSummaryUtilImplTest.DELIMITER + "20000",
      "pp-03" + ItemSummaryUtilImplTest.DELIMITER + "WRONG_PRICE");

  @InjectMocks
  private  ItemSummaryUtilImpl itemSummaryUtil;
  
  @Mock
  private ProductAndItemSolrConstructorService solrDataConstructor;

  @Mock
  private ItemHelperService itemHelperService;
  
  @Mock
  private Page<ProductAndItemSolr> solrResult;

  private List<ProductAndItemSolr> solrContentResult;

  private ProductAndItemSolr solrResultElement;

  private Map<String, Item> itemSkuItemMap;
  private Map<String, Set<Price>> mapOfPrices;
  private Map<String, Double> mapOfOriginalPrice;

  private MasterCatalog masterCatalog;
  private Map<String, Set<ItemViewConfig>> itemViewConfigMap;
  
  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.solrResultElement = new ProductAndItemSolr();
    this.solrResultElement.setBrand("brand");
    this.solrResultElement.setBuyable(ItemSummaryUtilImplTest.BUYABLE);
    this.solrResultElement.setDiscoverable(ItemSummaryUtilImplTest.DISCOVERABLE);
    this.solrResultElement.setItemCode("item-code");
    this.solrResultElement.setItemImages(ItemSummaryUtilImplTest.ITEM_IMAGES);
    this.solrResultElement.setItemName(ItemSummaryUtilImplTest.ITEM_NAME);
    this.solrResultElement.setItemSku(ItemSummaryUtilImplTest.ITEM_SKU);
    this.solrResultElement.setLatefulfillment(false);
    this.solrResultElement.setMasterCatalog(ItemSummaryUtilImplTest.MASTER_CATALOG);
    this.solrResultElement.setMerchantCode(ItemSummaryUtilImplTest.MERCHANT_CODE);
    this.solrResultElement.setMerchantSku(ItemSummaryUtilImplTest.MERCHANT_SKU);
    this.solrResultElement.setPickupPointCode(ItemSummaryUtilImplTest.PICKUP_POINT_CODE);
    this.solrResultElement.setProductType(ProductType.REGULAR.toString());
    this.solrResultElement.setProductSku(ItemSummaryUtilImplTest.PRODUCT_SKU);
    this.solrResultElement.setProductName(ItemSummaryUtilImplTest.PRODUCT_NAME);
    this.solrResultElement.setSalesCatalog(ItemSummaryUtilImplTest.SALES_CATALOG);
    this.solrResultElement.setOfflinePrices(ItemSummaryUtilImplTest.OFFLINE_PRICES);
    this.solrResultElement.setMerchantPromoDiscount(Boolean.TRUE);
    this.solrResultElement.setMerchantPromoDiscountActivated(Boolean.TRUE);
    this.solrContentResult = new ArrayList<>();
    this.solrContentResult.add(this.solrResultElement);
    when(this.solrResult.getContent()).thenReturn(this.solrContentResult);

    Set<Price> prices = new HashSet<>();
    prices.add(new Price("IDR", 1000, 1100, "default", "system", new Date()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(new ItemViewConfig());
    Item item = new Item();
    item.setSynchronized(false);
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    item.setPrice(prices);
    item.setItemViewConfigs(itemViewConfigs);
    item.setPickupPointCode(PICKUP_POINT_CODE);
    item.setMerchantSku(MERCHANT_SKU);
    item.setLateFulfillment(IS_LATE_FULFILLMENT);
    itemSkuItemMap = new HashMap<>();
    itemSkuItemMap.put(ITEM_SKU, item);
    mapOfPrices = new HashMap<>();
    mapOfPrices.put(ITEM_SKU, item.getPrice());
    String[] masterCatalogAttr = MASTER_CATALOG.split(ItemSummaryUtilImplTest.DELIMITER);
    mapOfOriginalPrice = new HashMap<>();
    mapOfOriginalPrice.put(ITEM_SKU, 10000.0);
    itemViewConfigMap =
        ImmutableMap.of(ITEM_SKU, ImmutableSet.of(new ItemViewConfig()));
    masterCatalog = new MasterCatalog(masterCatalogAttr[0], new Category(masterCatalogAttr[1], masterCatalogAttr[1]));
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(solrDataConstructor, itemHelperService);
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsTest() {
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService)
        .getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertFalse(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsProductTypeNull() {
    try {
      when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
      solrResult.getContent().get(0).setProductType(null);
      ItemSummaryPageResponseVo response = itemSummaryUtil
          .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
              mapOfOriginalPrice, itemViewConfigMap);
    } catch (Exception e) {
    } finally {
      verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
      verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
      verify(this.itemHelperService).getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
    }
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsEmptyMap() {
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    solrResult.getContent().get(0).setItemSku(ITEM_NAME);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService).getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsPriceTest() {
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService)
        .getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertFalse(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
    Assertions.assertEquals( response.getItemSummaryResponses().get(0).getOriginalSellingPrice(),10000, 0);
  }

  @Test
  public void constructItemSummaryResponseForPromoItems_whenMasterCatalogNullTest() {
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(null);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsExceptionTest() {
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
  }

  @Test
  public void constructItemSummaryResponseForPromoItemsWithWholesaleActivatedFlagTest() {
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    solrResult.getContent().get(0).setWholesalePriceActivated(true);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService)
        .getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).isMerchantPromoDiscountActivated());
    Assertions.assertTrue(response.getItemSummaryResponses().get(0).getWholesalePriceActivated());
  }

  @Test
  public void constructItemSummaryResponseForItemViewConfigNull() {
    itemSkuItemMap.put(ITEM_SKU, null);
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    solrResult.getContent().get(0).setWholesalePriceActivated(true);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService)
        .getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
  }

  @Test
  public void constructItemSummaryResponseForPricesNull() {
   itemSkuItemMap.get(ITEM_SKU).setPrice(new HashSet<>());
    when(this.solrDataConstructor.constructMasterCatalog(MASTER_CATALOG)).thenReturn(masterCatalog);
    ItemSummaryPageResponseVo response = itemSummaryUtil
        .constructItemSummaryResponseForPromoItems(REQUEST_ID, USERNAME, this.solrResult, mapOfPrices,
            mapOfOriginalPrice, itemViewConfigMap);
    verify(this.solrDataConstructor).constructMasterCatalog(MASTER_CATALOG);
    verify(this.solrDataConstructor).constructMasterDataItemMainImages(ITEM_IMAGES);
    verify(this.itemHelperService)
        .getCategoryNameByCategoryCode(eq(REQUEST_ID), eq(USERNAME), anyString());
  }

  @Test
  public void getProductCodeFromItemCodeTest(){
    String productCode = itemSummaryUtil.getProductCodeFromItemCode("Item-Code-1");
    Assertions.assertEquals("Item-Code", productCode);
  }


}