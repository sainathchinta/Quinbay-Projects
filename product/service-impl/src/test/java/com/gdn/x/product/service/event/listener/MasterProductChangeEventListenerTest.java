package com.gdn.x.product.service.event.listener;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.google.common.collect.ImmutableSet;

/**
 * This unit test class template is the work of maria.o.a.sunaryo and felix.w.wijaya as
 * participation in Blibli Hackathon #1. Generated on 24 Jun 2016 10:19:12
 */
public class MasterProductChangeEventListenerTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String ITEM_CODE = "itemCode";
  private static final String STORE_ID = "10001";
  private List<String> oldSalesCategoryList;
  private List<String> newSalesCategoryList;
  private static final String OLD_CAT_1 = "55024";
  private static final String OLD_CAT_2 = "AP-1000019";
  private static final String OLD_CAT_3 = "DR-1000007";
  private static final String NEW_CAT_1 = "54914";
  private static final String NEW_CAT_2 = "FA-1000030";
  private static final String MESSAGE = "message";

  @InjectMocks
  private MasterProductChangeEventListener masterProductChangeEventListener;

  @Mock
  private MasterDataCacheService cacheService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductService productService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ObjectMapper objectMapper;

  private ProductDomainEventModel message ;

  private ProductItemDomainEventModel itemDomainEventModel;

  private SystemParameter systemParameter;

  private Map<String, Double> productAndScoreMap = new HashMap<>();
  private Map<String, Boolean> itemsChanged = new HashMap<>();

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    itemDomainEventModel = new ProductItemDomainEventModel();
    itemDomainEventModel.setSkuCode(MasterProductChangeEventListenerTest.ITEM_CODE);
    itemsChanged.put(itemDomainEventModel.getSkuCode(), itemDomainEventModel.isContentChanged());
    List<ProductItemDomainEventModel> items = Arrays.asList(itemDomainEventModel);
    ReflectionTestUtils.setField(masterProductChangeEventListener, "storeId", "10001");
    message = new ProductDomainEventModel();
    message.setStoreId(STORE_ID);
    message.setProductCode(MasterProductChangeEventListenerTest.PRODUCT_CODE);
    message.setProductItems(items);
    message.setScoreUpdated(true);
    message.setSolrUpdateRequired(true);
    message.setPristineCategory(true);
    message.setUpdateMasterData(true);
    ReflectionTestUtils.setField(masterProductChangeEventListener, "isPristineSettingEnabled", true);

    oldSalesCategoryList = new ArrayList<>();
    oldSalesCategoryList.add(OLD_CAT_1);
    oldSalesCategoryList.add(OLD_CAT_2);
    oldSalesCategoryList.add(OLD_CAT_3);
    newSalesCategoryList = new ArrayList<>();
    newSalesCategoryList.add(NEW_CAT_1);
    newSalesCategoryList.add(NEW_CAT_2);

    productAndScoreMap.put(PRODUCT_CODE, 100.0);

    ReflectionTestUtils.setField(masterProductChangeEventListener, "masterDataChangeSolrReindexEnabled", true);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void onDomainEventConsumed_whenMessageStoreIdNotNullTest() throws Exception {
    this.message.setStoreId("10001");
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void onDomainEventConsumed_whenMessageIsActivatedAndIsViewedTrueTest() throws Exception {

    this.message.setStoreId("10001");
    this.message.setActivated(true);
    this.message.setViewable(true);
    this.message.setMigratedProduct(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.itemService).publishItemSkus(eq(STORE_ID), eq(itemsChanged),
        Mockito.anySet(), eq(message));
  }

  @Test public void onDomainEventConsumed_isPristineSettingEnabledFalseTest() throws Exception {

    this.message.setStoreId("10001");
    this.message.setActivated(true);
    this.message.setViewable(true);
    ReflectionTestUtils.setField(masterProductChangeEventListener, "isPristineSettingEnabled", false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {

    this.message.setStoreId("10001");
    this.message.setActivated(true);
    this.message.setViewable(true);
    this.message.setMigratedProduct(true);

    doThrow(RuntimeException.class).when(this.itemService).publishItemSkus(eq(STORE_ID),
         eq(itemsChanged), Mockito.anySet(), eq(message));
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.itemService).publishItemSkus(eq(STORE_ID), eq(itemsChanged),
        Mockito.anySet(), eq(message));
  }

  @Test
  public void onDomainEventConsumedCategoryChangeTest() throws Exception {
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productService).updateProductCatalogByProductCodeOnCategoryChange(STORE_ID, PRODUCT_CODE,
        message.getProductSalesCategoryMapping(), message.getProductCategories(), false);
  }

  @Test
  public void onDomainEventConsumedCategoryChangeOnlyNewUmkmSalesTest() throws Exception {
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setNewSalesCategoryCodes(null);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setOldSalesCategoryCodes(null);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping()
        .setNewUmkmSalesCategoryCodes(newSalesCategoryList);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productService).updateProductCatalogByProductCodeOnCategoryChange(STORE_ID, PRODUCT_CODE,
        message.getProductSalesCategoryMapping(), message.getProductCategories(), false);
  }

  @Test
  public void onDomainEventConsumedCategoryChangeEmptyNewSalesCategoryTest() throws Exception {
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setNewSalesCategoryCodes(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productService).updateProductCatalogByProductCodeOnCategoryChange(STORE_ID, PRODUCT_CODE,
        message.getProductSalesCategoryMapping(), message.getProductCategories(), false);
  }

  @Test
  public void onDomainEventConsumedCategoryChangeEmptyNewSalesCategoryIgnoreUpdateTest() throws Exception {
    message.setIgnoreSalesCategoryPublish(true);
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setNewSalesCategoryCodes(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
        .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void onDomainEventConsumedCategoryChangeEmptyOldSalesCategoryTest() throws Exception {
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setOldSalesCategoryCodes(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productService).updateProductCatalogByProductCodeOnCategoryChange(STORE_ID, PRODUCT_CODE,
        message.getProductSalesCategoryMapping(), message.getProductCategories(), false);
  }

  @Test
  public void onDomainEventConsumedCategoryChangeEmptySalesCategoryTest() throws Exception {
    ProductDomainEventModel productDomainEventModelForCategoryChange =
        getProductDomainEventModelForCategoryChange(message);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setOldSalesCategoryCodes(null);
    productDomainEventModelForCategoryChange.getProductSalesCategoryMapping().setNewSalesCategoryCodes(null);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(productDomainEventModelForCategoryChange);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            productDomainEventModelForCategoryChange);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void onDomainEventConsumedPostLiveRejectionTest() throws Exception {
    this.message.setStoreId(STORE_ID);
    this.message.setActivated(true);
    this.message.setViewable(true);
    this.message.setproductMarkForDelete(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProductWithoutSolrIndex(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.itemService).archiveAndDeleteActiveProduct(eq(message), isNull());
  }

  private ProductDomainEventModel getProductDomainEventModelForCategoryChange(ProductDomainEventModel message) {
    ProductSalesCategoryMapping productSalesCategoryMapping = new ProductSalesCategoryMapping();
    productSalesCategoryMapping.setNewSalesCategoryCodes(newSalesCategoryList);
    productSalesCategoryMapping.setOldSalesCategoryCodes(oldSalesCategoryList);
    message.setProductSalesCategoryMapping(productSalesCategoryMapping);
    return message;
  }

  @Test
  public void onDomainEventConsumedObBrandChangeTest() throws Exception {
    this.message.setBrandChanged(true);
    this.message.setActivated(true);
    this.message.setViewable(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE, message);
    verify(this.productAndItemSolrIndexerService)
        .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService).evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.productService).updateBrandForUnsyncProducts(STORE_ID, message);
    verify(this.itemService).publishItemSkus(eq(STORE_ID), eq(itemsChanged),
        Mockito.anySet(), eq(message));
  }

  @Test
  public void onDomainEventConsumedObBrandChangeExceptionTest() throws Exception {
    this.message.setBrandChanged(true);
    this.message.setActivated(true);
    this.message.setViewable(true);
    doThrow(ApplicationRuntimeException.class).when(this.productService)
        .updateBrandForUnsyncProducts(STORE_ID, message);
    try {
      Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
        .thenReturn(message);
      this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
      Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    } finally {
      verify(this.cacheService)
          .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE, message);
      verify(this.productAndItemSolrIndexerService)
          .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
      verify(this.cacheService)
          .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
      verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
      verify(this.productService).updateBrandForUnsyncProducts(STORE_ID, message);
      verify(this.itemService).publishItemSkus(eq(STORE_ID), eq(itemsChanged),
          Mockito.anySet(), eq(message));
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.cacheService);
    verifyNoMoreInteractions(this.systemParameterService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.productAndItemSolrIndexerService);
  }


  @Test
  public void onDomainEventConsumedItemPublishTest() throws Exception {
    message.setScoreUpdated(false);
    message.setSolrUpdateRequired(false);
    message.setItemUpdatePublish(true);
    message.setPristineCategory(false);
    message.setActivated(true);
    message.setViewable(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService).evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE, message);
    verify(this.cacheService).evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.itemService).publishItemSkus(eq(STORE_ID), eq(itemsChanged)
        , Mockito.anySet(), eq(message));
  }

  @Test
  public void onDomainEventConsumedItemPublishSpecificL4sTest() throws Exception {
    message.setScoreUpdated(false);
    message.setSolrUpdateRequired(false);
    message.setItemUpdatePublish(true);
    message.setPristineCategory(false);
    message.setActivated(true);
    message.setViewable(true);
    message.getProductItems().get(0).setPublishL4(true);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
        .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService).evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE, message);
    verify(this.cacheService).evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    verify(this.itemService).publishItemSkus(STORE_ID, itemsChanged,
        ImmutableSet.of(message.getProductItems().get(0).getSkuCode()), message);
  }

  @Test
  public void onDomainEventConsumedNotUpdateTest() throws Exception {
    message.setUpdateMasterData(false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
      .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
      .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
        message);
    verify(this.cacheService)
      .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.productAndItemSolrIndexerService)
      .updateInSolrByProductCode(STORE_ID, PRODUCT_CODE);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }

  @Test
  public void test() {
    System.out.println(ItemChangeEventType.valueOf(
        com.gdn.x.product.enums.ItemChangeEventType.ITEM_DATA_CHANGE.name()));
  }

  @Test
  public void onDomainEventConsumedSolrReindexSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(masterProductChangeEventListener, "masterDataChangeSolrReindexEnabled", false);
    Mockito.when(this.objectMapper.readValue(MESSAGE, ProductDomainEventModel.class))
        .thenReturn(message);
    this.masterProductChangeEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, ProductDomainEventModel.class);
    verify(this.cacheService)
        .evictMasterDataProduct(MasterProductChangeEventListenerTest.PRODUCT_CODE,
            message);
    verify(this.cacheService)
        .evictMasterDataItem(MasterProductChangeEventListenerTest.ITEM_CODE, itemDomainEventModel);
    verify(this.cacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
  }
}
