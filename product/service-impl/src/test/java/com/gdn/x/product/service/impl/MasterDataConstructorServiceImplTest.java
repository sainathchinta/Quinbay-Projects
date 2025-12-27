package com.gdn.x.product.service.impl;

import static com.mongodb.assertions.Assertions.assertFalse;
import static com.mongodb.assertions.Assertions.assertNull;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Stream;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.google.common.collect.ImmutableSet;

;

public class MasterDataConstructorServiceImplTest {

  private static final int DANGEROUS_LEVEL = 1;
  private static final String CATALOG_CODE = "catalogCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String BRAND = "brand";
  private static final String ITEM_NAME = "itemName";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_CODE_1 = "itemCode1";
  private static final String PRODUCT_CODE = "productCode";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String SIZE_ATTRIBUTE_CODE = "sizeAttributeCode";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_SKU = "itemSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ATTRIBUTE_VALUE = "GOLD";
  private static final String ATTRIBUTE_CODE = "WA-0000002";
  private static final String ATTRIBUTE_NAME = "Warna";
  private static final String DOCUMENT_TYPE = "Document type";
  private static final Double WIDTH = 5.0;
  private static final Double WEIGHT = 4.0;
  private static final Double LENGTH = 3.0;
  private static final Double HEIGHT = 2.0;
  private static final Double SHIPPING_WEIGHT = 1.0;
  private static final Double ZERO_SCALE = 0.0;
  private static final boolean IN_ALL_PRODUCTS = false;
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String MERCHANT_CODE = "merchant_code";
  public static final String ATTRIBUTE_CODE1 = "ATTR1";
  public static final String ATTRIBUTE_VALUE1 = "YES";


  @InjectMocks
  private MasterDataConstructorServiceImpl masterDataConstructorService;

  @Mock
  private MasterDataService masterDataService;
  private Set<String> productCodes;
  private Product product;
  private Item item;
  private MasterDataItem masterDataItem;
  private MasterDataProduct masterDataProduct;
  private MasterCatalog masterCatalog;
  private Map<String, MasterDataProductAndItemsVO> masterDataProductMap;
  private ProductAndItemSolr productAndItemSolr;
  private CategoryNamesResponse categoryNamesResponse;
  private PreOrder preOrder;
  private ProductMasterDataResponse productMasterDataResponse = new ProductMasterDataResponse();
  private SystemParameter concurrentMasterDataFetchParameter = new SystemParameter();

  private Future<ProductMasterDataResponse> result = new Future<ProductMasterDataResponse>() {
    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }

    @Override
    public boolean isCancelled() {
      return false;
    }

    @Override
    public boolean isDone() {
      return false;
    }

    @Override
    public ProductMasterDataResponse get() throws InterruptedException, ExecutionException {
      return productMasterDataResponse;
    }

    @Override
    public ProductMasterDataResponse get(long timeout, TimeUnit unit)
      throws InterruptedException, ExecutionException, TimeoutException {
      return null;
    }
  };

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private ExecutorService executorService;

  @Mock
  private SystemParameterService systemParameterService;

  @Test
  public void constructItemDimensionFieldsHasFieldAlreadyTest() {
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataItem.setItemDeliveryWeight(MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    masterDataItem.setItemWeight(MasterDataConstructorServiceImplTest.WEIGHT);
    masterDataItem.setItemWidth(MasterDataConstructorServiceImplTest.WIDTH);
    masterDataItem.setItemHeight(MasterDataConstructorServiceImplTest.HEIGHT);
    masterDataItem.setItemLength(MasterDataConstructorServiceImplTest.LENGTH);
    masterDataProduct.setShippingWeight(MasterDataConstructorServiceImplTest.ZERO_SCALE);
    masterDataProduct.setWeight(MasterDataConstructorServiceImplTest.ZERO_SCALE);
    masterDataProduct.setWidth(MasterDataConstructorServiceImplTest.ZERO_SCALE);
    masterDataProduct.setHeight(MasterDataConstructorServiceImplTest.ZERO_SCALE);
    masterDataProduct.setLength(MasterDataConstructorServiceImplTest.ZERO_SCALE);

    masterDataItem =
        this.masterDataConstructorService.constructItemDimensionFields(masterDataItem,
            masterDataProduct);

    assertEquals(masterDataItem.getItemDeliveryWeight(),
        MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    assertEquals(masterDataItem.getItemHeight(), MasterDataConstructorServiceImplTest.HEIGHT);
    assertEquals(masterDataItem.getItemLength(), MasterDataConstructorServiceImplTest.LENGTH);
    assertEquals(masterDataItem.getItemWeight(), MasterDataConstructorServiceImplTest.WEIGHT);
    assertEquals(masterDataItem.getItemWidth(), MasterDataConstructorServiceImplTest.WIDTH);
  }

  @Test
  public void constructItemDimensionFieldsNullItemTest() {
    MasterDataItem masterDataItem = null;
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    MasterDataItem result =
        this.masterDataConstructorService.constructItemDimensionFields(masterDataItem,
            masterDataProduct);
    assertEquals(result, null);
  }

  @Test
  public void constructItemDimensionFieldsNullProductTest() {
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = null;
    MasterDataItem result =
        this.masterDataConstructorService.constructItemDimensionFields(masterDataItem,
            masterDataProduct);
    assertEquals(result, masterDataItem);
  }

  @Test
  public void constructItemDimensionFieldsTest() {
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setShippingWeight(MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    masterDataProduct.setWeight(MasterDataConstructorServiceImplTest.WEIGHT);
    masterDataProduct.setWidth(MasterDataConstructorServiceImplTest.WIDTH);
    masterDataProduct.setHeight(MasterDataConstructorServiceImplTest.HEIGHT);
    masterDataProduct.setLength(MasterDataConstructorServiceImplTest.LENGTH);
    masterDataItem =
        this.masterDataConstructorService.constructItemDimensionFields(masterDataItem,
            masterDataProduct);

    assertEquals(masterDataItem.getItemDeliveryWeight(),
        MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    assertEquals(masterDataItem.getItemHeight(), MasterDataConstructorServiceImplTest.HEIGHT);
    assertEquals(masterDataItem.getItemLength(), MasterDataConstructorServiceImplTest.LENGTH);
    assertEquals(masterDataItem.getItemWeight(), MasterDataConstructorServiceImplTest.WEIGHT);
    assertEquals(masterDataItem.getItemWidth(), MasterDataConstructorServiceImplTest.WIDTH);
  }

  @Test
  public void constructItemsDimensionFieldsNullItemTest() {
    MasterDataItem masterDataItem = null;
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setShippingWeight(MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    masterDataProduct.setWeight(MasterDataConstructorServiceImplTest.WEIGHT);
    masterDataProduct.setWidth(MasterDataConstructorServiceImplTest.WIDTH);
    masterDataProduct.setHeight(MasterDataConstructorServiceImplTest.HEIGHT);
    masterDataProduct.setLength(MasterDataConstructorServiceImplTest.LENGTH);

    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setMasterDataProduct(masterDataProduct);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setMasterDataItem(masterDataItem);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ProductAndItemsVO result =
        this.masterDataConstructorService.constructItemsDimensionFields(productAndItemsVO);
    assertEquals(result.getItems().get(0).getMasterDataItem(), null);
  }

  @Test
  public void constructItemsDimensionFieldsTest() {
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setShippingWeight(MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    masterDataProduct.setWeight(MasterDataConstructorServiceImplTest.WEIGHT);
    masterDataProduct.setWidth(MasterDataConstructorServiceImplTest.WIDTH);
    masterDataProduct.setHeight(MasterDataConstructorServiceImplTest.HEIGHT);
    masterDataProduct.setLength(MasterDataConstructorServiceImplTest.LENGTH);

    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setMasterDataProduct(masterDataProduct);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setMasterDataItem(masterDataItem);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ProductAndItemsVO result =
        this.masterDataConstructorService.constructItemsDimensionFields(productAndItemsVO);

    MasterDataItem resultMasterDataItem = result.getItems().get(0).getMasterDataItem();
    assertEquals(resultMasterDataItem.getItemDeliveryWeight(),
        MasterDataConstructorServiceImplTest.SHIPPING_WEIGHT);
    assertEquals(resultMasterDataItem.getItemHeight(), MasterDataConstructorServiceImplTest.HEIGHT);
    assertEquals(resultMasterDataItem.getItemLength(), MasterDataConstructorServiceImplTest.LENGTH);
    assertEquals(resultMasterDataItem.getItemWeight(), MasterDataConstructorServiceImplTest.WEIGHT);
    assertEquals(resultMasterDataItem.getItemWidth(), MasterDataConstructorServiceImplTest.WIDTH);
  }

  @Test
  public void constructProductAndItemWithMasterDataNullProductCodeTest() throws Exception {
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item));

    assertEquals(result, productAndItemsVO);
  }
  
  @Test
  public void constructProductAndItemWithMasterDataSyncProductTest() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap =
        new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE,
        masterDataProductAndItemsVO);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item));

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterDataTest() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item));

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }
  
  @Test
  public void constructProductAndItemWithMasterDataWhenItemIsNull() throws Exception {
    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, new ArrayList<>());

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }
  
  @Test
  public void constructProductAndItemWithMasterDataAndEvictCachesTest() throws Exception {
    ReflectionTestUtils.setField(masterDataConstructorService,
        "valueTypeAdditionForDefiningAttributes", true);
    List<String> itemCodesEvict = new ArrayList<>();
    itemCodesEvict.add(ITEM_CODE);
    List<String> productCodesEvict = new ArrayList<>();
    productCodesEvict.add(PRODUCT_CODE);
    
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setSizeAttributeCode(SIZE_ATTRIBUTE_CODE);
    product.setMasterDataProduct(masterDataProduct);
    product.setSynchronized(true);

    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setMasterDataItem(new MasterDataItem());
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setDangerousLevel(MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
    List<MasterDataItemAttributeValue> itemAttributeValues = new ArrayList<>();
    
    MasterDataItemAttributeValue itemAttributeValue = new MasterDataItemAttributeValue();
    itemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
    itemAttributeValues.add(itemAttributeValue);
    
    MasterDataItemAttributeValue itemAttributeValue2 = new MasterDataItemAttributeValue();
    itemAttributeValue2.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    itemAttributeValue2.setMasterDataAttribute(masterDataAttribute2);
    itemAttributeValues.add(itemAttributeValue2);

    MasterDataItemAttributeValue itemAttributeValue3 = new MasterDataItemAttributeValue();
    itemAttributeValue3.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute3 = new MasterDataAttribute();
    masterDataAttribute3.setAttributeCode("WARNA_ATTRIBUTE");
    masterDataAttribute3.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute3.setVariantCreation(true);
    itemAttributeValue3.setMasterDataAttribute(masterDataAttribute3);
    itemAttributeValues.add(itemAttributeValue3);
    masterDataItem.setMasterDataItemAttributeValues(itemAttributeValues);

    MasterCatalog masterCatalog =
        new MasterCatalog(MasterDataConstructorServiceImplTest.CATALOG_CODE, null);
    masterDataProduct.setMasterCatalog(masterCatalog);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap =
        new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE,
        masterDataProductAndItemsVO);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterDataAndEvictCaches(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item));

    verify(this.masterDataCacheService).evictCachesMasterDataItem(itemCodesEvict);
    verify(this.masterDataCacheService).evictCachesMasterDataProduct(productCodesEvict);
    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    verify(this.masterDataCacheService).evictCacheMasterDataForTransaction(ITEM_CODE);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
    assertEquals("WARNA_ATTRIBUTE", productAndItemsVO.getProduct().getDefiningAttributes().get(0).getProductAttributeDetails().get(1).getAttributeCode());
    assertEquals(SIZE_ATTRIBUTE_CODE, productAndItemsVO.getProduct().getSizeAttributeCode());
  }

  @Test
  public void constructProductsAndItemsWithMasterDataTest() throws Exception {
    ProductAndItemsVO expectedResult = new ProductAndItemsVO(product, Arrays.asList(item));
    List<Item> items = Stream.of(item).collect(toList());
    Map<String, Product> productMap =
        Stream.of(product).collect(toMap(Product::getProductSku, Function.identity()));

    when(this.masterDataService.getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID,
        productCodes, IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);

    List<ProductAndItemsVO> result = masterDataConstructorService.constructProductsAndItemsWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, productMap, items, false);

    assertEquals(expectedResult, result.get(0));
    assertEquals(masterCatalog,
        result.get(0).getProduct().getMasterDataProduct().getMasterCatalog());
    assertEquals(DANGEROUS_LEVEL,
        result.get(0).getItems().get(0).getMasterDataItem().getDangerousLevel());

    verify(this.masterDataService).getMasterDataProductDetailResponse(STORE_ID, USERNAME,
        REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }

  @Test
  public void constructProductsAndItemsWithEmptyMasterDataTest() throws Exception {
    Map<String, Product> productMap = new HashMap<>();
    when(this.masterDataService
        .getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProductMap);
    List<ProductAndItemsVO> result = masterDataConstructorService
        .constructProductsAndItemsWithMasterData(STORE_ID, USERNAME, REQUEST_ID, productMap, Collections.EMPTY_LIST,
            false);
   Assertions.assertEquals(result.size(), 0);
  }

  @Test
  public void constructProductsAndItemsWithMasterDataTest_whenMasterDataItemIsNull() throws Exception {
    ProductAndItemsVO expectedResult = new ProductAndItemsVO(product, Arrays.asList(item));
    item.setMasterDataItem(null);
    List<Item> items = Stream.of(item).collect(toList());
    Map<String, Product> productMap =
        Stream.of(product).collect(toMap(Product::getProductSku, Function.identity()));

    when(this.masterDataService.getMasterDataProductDetailResponse(STORE_ID, USERNAME, REQUEST_ID,
        productCodes, IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);

    List<ProductAndItemsVO> result = masterDataConstructorService.constructProductsAndItemsWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, productMap, items, false);

    assertEquals(expectedResult, result.get(0));
    assertEquals(masterCatalog,
        result.get(0).getProduct().getMasterDataProduct().getMasterCatalog());

    verify(this.masterDataService).getMasterDataProductDetailResponse(STORE_ID, USERNAME,
        REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
    productCodes = new HashSet<String>();
    productCodes.add(PRODUCT_CODE);
    product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setMasterDataProduct(new MasterDataProduct());
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));

    item = new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
        MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setMasterDataItem(new MasterDataItem());
    item.setProductSku(PRODUCT_SKU);
    item.setPristineDataItem(new PristineDataItem());

    masterCatalog = new MasterCatalog(MasterDataConstructorServiceImplTest.CATALOG_CODE, null);

    masterDataItem = new MasterDataItem();
    masterDataItem.setDangerousLevel(MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
    masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(masterCatalog);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE,
        masterDataProductAndItemsVO);

    productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setMasterCatalog(CATALOG_CODE+ Constants.DELIMETER + CATEGORY_CODE);
    productAndItemSolr.setBrand(BRAND);
    productAndItemSolr.setItemName(ITEM_NAME);

    categoryNamesResponse = new CategoryNamesResponse();
    categoryNamesResponse.getCategoryMap().put(CATEGORY_CODE, CATEGORY_NAME);
    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(10);

    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(ITEM_CODE);
    productMasterDataResponse.setProductItemResponses(Collections.singleton(productItemResponse));
    concurrentMasterDataFetchParameter.setValue(String.valueOf(10));
  }

  @Test
  public void constructProductAndItemWithMasterDataNullProductCodePVTest() throws Exception {
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setPreOrder(preOrder);
    product.setSizeChartCode(SIZE_CHART_CODE);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    assertEquals(result, productAndItemsVO);
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getProduct().getPreOrder().getPreOrderValue());
    assertNull(result.getProduct().getPreOrder().getPreOrderDate());
    assertEquals(SIZE_CHART_CODE, result.getProduct().getSizeChartCode());
  }

  @Test
  public void constructProductAndItemWithMasterDataWithNoPristineItem() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap =
        new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE,
        masterDataProductAndItemsVO);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterDataWithNoDefaultProductCode() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(new PristineDataItem());
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE, masterDataProductAndItemsVO);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);

    when(this.masterDataService.getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);

    ProductAndItemsVO result = this.masterDataConstructorService.constructProductAndItemWithMasterData(MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, product,
        Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  public void constructProductAndItemWithMasterDataEmptyProductCodePVTest() throws Exception {
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Collections.EMPTY_LIST);
    ProductAndItemsVO result = this.masterDataConstructorService
        .constructProductAndItemWithMasterData(MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, product,
            Collections.EMPTY_LIST, true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
   Assertions.assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterDataEmptyProductCodePVTest1() throws Exception {
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setPristineDataItem(new PristineDataItem());
    ProductAndItemsVO result = this.masterDataConstructorService
        .constructProductAndItemWithMasterData(MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, product,
            Collections.singletonList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
  }

  @Test
  public void constructProductAndItemWithMasterDataEmptyProductCodePVTest2() throws Exception {
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setPristineDataItem(null);
    ProductAndItemsVO result = this.masterDataConstructorService
        .constructProductAndItemWithMasterData(MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, product,
            Collections.singletonList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
  }

  @Test
  public void constructProductAndItemWithMasterDataSyncProductPVTest() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add("DEFAULT_PRODUCT_CODE");
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("PRISTINEID");
    pristineDataItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE");
    item.setPristineDataItem(pristineDataItem);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap =
        new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE,
        masterDataProductAndItemsVO);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterDataSyncProductPVTProductNullest() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add("DEFAULT_PRODUCT_CODE");
    productCodes.add(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode(MasterDataConstructorServiceImplTest.PRODUCT_CODE);
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("PRISTINEID");
    pristineDataItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE");
    item.setPristineDataItem(pristineDataItem);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);
    when(this.masterDataService.getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);
    ProductAndItemsVO result = this.masterDataConstructorService.constructProductAndItemWithMasterData(
        MasterDataConstructorServiceImplTest.STORE_ID, MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterDataSyncProductPVProductCodeEqualsDefaultCodeTest() throws Exception {
    Set<String> productCodes = new HashSet<String>();
    productCodes.add("DEFAULT_PRODUCT_CODE");
    Product product = new Product(MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    product.setProductCode("DEFAULT_PRODUCT_CODE");
    product.setSynchronized(true);
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId("PRISTINEID");
    pristineDataItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE");
    item.setPristineDataItem(pristineDataItem);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    MasterDataItem masterDataItem = new MasterDataItem();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE, masterDataProductAndItemsVO);
    masterDataProductMap.put("DEFAULT_PRODUCT_CODE", masterDataProductAndItemsVO);
    when(this.masterDataService.getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);
    ProductAndItemsVO result = this.masterDataConstructorService.constructProductAndItemWithMasterData(
        MasterDataConstructorServiceImplTest.STORE_ID, MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
  }

  @Test
  public void constructProductAndItemWithMasterData_PVSwitchfalseTest() throws Exception {
    item.setPristineDataItem(null);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ReflectionTestUtils.setField(masterDataConstructorService, "isProductVisibilityEnabled", false);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), false, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @Test
  public void constructProductAndItemWithMasterData_PVSwitchTrueTest() throws Exception {
    item.setPristineDataItem(null);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
  }

  @Test
  public void constructPristineDataItemWithMasterDataTest() throws Exception {
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setSynchronized(true);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(new PristineDataItem());
    item.setMerchantCode(MERCHANT_CODE);
    MasterDataItem masterDataItem = new MasterDataItem();
    List<MasterDataItemAttributeValue> itemAttributeValues = new ArrayList<>();

    MasterDataItemAttributeValue itemAttributeValue = new MasterDataItemAttributeValue();
    itemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
    itemAttributeValues.add(itemAttributeValue);

    MasterDataItemAttributeValue itemAttributeValue2 = new MasterDataItemAttributeValue();
    itemAttributeValue2.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    itemAttributeValue2.setMasterDataAttribute(masterDataAttribute2);
    itemAttributeValues.add(itemAttributeValue2);

    MasterDataItemAttributeValue itemAttributeValue3 = new MasterDataItemAttributeValue();
    itemAttributeValue3.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute3 = new MasterDataAttribute();
    masterDataAttribute3.setAttributeCode("WARNA_ATTRIBUTE");
    masterDataAttribute3.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute3.setVariantCreation(true);
    itemAttributeValue3.setMasterDataAttribute(masterDataAttribute3);
    itemAttributeValues.add(itemAttributeValue3);
    masterDataItem.setMasterDataItemAttributeValues(itemAttributeValues);
    item.setMasterDataItem(masterDataItem);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);

    Mockito.when(masterDataService
        .getMasterDataItems(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.anySet())).thenReturn(masterDataItemMap);
    Mockito.when(productAndItemSolrRepository.findOne(ITEM_SKU, MERCHANT_CODE)).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);

    Item result =
        this.masterDataConstructorService.constructPristineDataItemWithMasterData(item);

    verify(this.masterDataService).getMasterDataItems(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.anySet());
    verify(this.productAndItemSolrRepository).findOne(ITEM_SKU, MERCHANT_CODE);
    verify(this.productCategoryBaseOutbound).getCategoryNames(Arrays.asList(CATEGORY_CODE));
    assertEquals(BRAND, result.getPristineDataItem().getPristineBrand());
    assertEquals(CATEGORY_NAME, result.getPristineDataItem().getPristineCategory());
    assertEquals(ITEM_NAME, result.getPristineDataItem().getPristineProductName());
    assertNotNull(result.getPristineDataItem().getPristineListingAttributes());
    assertEquals(2, result.getPristineDataItem().getPristineListingAttributes().size());
  }

  @Test
  public void constructPristineDataItemWithMasterDataPCBNullResponseTest() throws Exception {
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setSynchronized(true);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(new PristineDataItem());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(null);
    item.setMasterDataItem(masterDataItem);
    item.setMerchantCode(MERCHANT_CODE);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);

    Mockito.when(masterDataService
        .getMasterDataItems(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_USERNAME),
            Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.anySet())).thenReturn(masterDataItemMap);
    Mockito.when(productAndItemSolrRepository.findOne(ITEM_SKU, MERCHANT_CODE)).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(null);

    Item result =
        this.masterDataConstructorService.constructPristineDataItemWithMasterData(item);

    verify(this.masterDataService).getMasterDataItems(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.eq(Constants.DEFAULT_USERNAME),
        Mockito.eq(Constants.DEFAULT_REQUEST_ID), Mockito.anySet());
    verify(this.productAndItemSolrRepository).findOne(ITEM_SKU, MERCHANT_CODE);
    verify(this.productCategoryBaseOutbound).getCategoryNames(Arrays.asList(CATEGORY_CODE));
    assertEquals(BRAND, result.getPristineDataItem().getPristineBrand());
    assertNull(result.getPristineDataItem().getPristineCategory());
    assertEquals(ITEM_NAME, result.getPristineDataItem().getPristineProductName());
    assertNotNull(result.getPristineDataItem().getPristineListingAttributes());
    assertEquals(0, result.getPristineDataItem().getPristineListingAttributes().size());
  }

  @Test
  public void constructPristineDataItemWithMasterDataSynchronizedFalseTest() throws Exception {
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU,
            MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(new PristineDataItem());
    item.setMerchantCode(MERCHANT_CODE);
    MasterDataItem masterDataItem = new MasterDataItem();
    List<MasterDataItemAttributeValue> itemAttributeValues = new ArrayList<>();

    MasterDataItemAttributeValue itemAttributeValue = new MasterDataItemAttributeValue();
    itemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
    itemAttributeValues.add(itemAttributeValue);

    MasterDataItemAttributeValue itemAttributeValue2 = new MasterDataItemAttributeValue();
    itemAttributeValue2.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    itemAttributeValue2.setMasterDataAttribute(masterDataAttribute2);
    itemAttributeValues.add(itemAttributeValue2);

    MasterDataItemAttributeValue itemAttributeValue3 = new MasterDataItemAttributeValue();
    itemAttributeValue3.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute3 = new MasterDataAttribute();
    masterDataAttribute3.setAttributeCode("WARNA_ATTRIBUTE");
    masterDataAttribute3.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute3.setVariantCreation(true);
    itemAttributeValue3.setMasterDataAttribute(masterDataAttribute3);
    itemAttributeValues.add(itemAttributeValue3);
    masterDataItem.setMasterDataItemAttributeValues(itemAttributeValues);
    item.setMasterDataItem(masterDataItem);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);

    Mockito.when(productAndItemSolrRepository.findOne(ITEM_SKU, MERCHANT_CODE)).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseOutbound.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);

    Item result =
        this.masterDataConstructorService.constructPristineDataItemWithMasterData(item);

    verify(this.productAndItemSolrRepository).findOne(ITEM_SKU, MERCHANT_CODE);
    verify(this.productCategoryBaseOutbound).getCategoryNames(Arrays.asList(CATEGORY_CODE));
    assertEquals(BRAND, result.getPristineDataItem().getPristineBrand());
    assertEquals(CATEGORY_NAME, result.getPristineDataItem().getPristineCategory());
    assertEquals(ITEM_NAME, result.getPristineDataItem().getPristineProductName());
    assertNotNull(result.getPristineDataItem().getPristineListingAttributes());
    assertEquals(2, result.getPristineDataItem().getPristineListingAttributes().size());
  }

  @Test
  public void constructPristineDataItemWithMasterDataMasterCatalogNullTest() throws Exception {
    Item item =
        new Item(MasterDataConstructorServiceImplTest.ITEM_SKU, MasterDataConstructorServiceImplTest.PRODUCT_SKU);
    item.setItemCode(MasterDataConstructorServiceImplTest.ITEM_CODE);
    item.setPristineDataItem(new PristineDataItem());
    item.setMerchantCode(MERCHANT_CODE);
    MasterDataItem masterDataItem = new MasterDataItem();
    List<MasterDataItemAttributeValue> itemAttributeValues = new ArrayList<>();

    MasterDataItemAttributeValue itemAttributeValue = new MasterDataItemAttributeValue();
    itemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    itemAttributeValue.setMasterDataAttribute(masterDataAttribute);
    itemAttributeValues.add(itemAttributeValue);

    MasterDataItemAttributeValue itemAttributeValue2 = new MasterDataItemAttributeValue();
    itemAttributeValue2.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    itemAttributeValue2.setMasterDataAttribute(masterDataAttribute2);
    itemAttributeValues.add(itemAttributeValue2);

    MasterDataItemAttributeValue itemAttributeValue3 = new MasterDataItemAttributeValue();
    itemAttributeValue3.setAttributeValue(ATTRIBUTE_VALUE);
    MasterDataAttribute masterDataAttribute3 = new MasterDataAttribute();
    masterDataAttribute3.setAttributeCode("WARNA_ATTRIBUTE");
    masterDataAttribute3.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute3.setVariantCreation(true);
    itemAttributeValue3.setMasterDataAttribute(masterDataAttribute3);
    itemAttributeValues.add(itemAttributeValue3);
    masterDataItem.setMasterDataItemAttributeValues(itemAttributeValues);
    item.setMasterDataItem(masterDataItem);

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataItemMap.put(MasterDataConstructorServiceImplTest.ITEM_CODE, masterDataItem);
    productAndItemSolr.setMasterCatalog(null);
    Mockito.when(productAndItemSolrRepository.findOne(ITEM_SKU, MERCHANT_CODE)).thenReturn(productAndItemSolr);

    Item result = this.masterDataConstructorService.constructPristineDataItemWithMasterData(item);

    verify(this.productAndItemSolrRepository).findOne(ITEM_SKU, MERCHANT_CODE);
    assertEquals(BRAND, result.getPristineDataItem().getPristineBrand());
    assertNull(result.getPristineDataItem().getPristineCategory());
    assertEquals(ITEM_NAME, result.getPristineDataItem().getPristineProductName());
    assertNotNull(result.getPristineDataItem().getPristineListingAttributes());
    assertEquals(2, result.getPristineDataItem().getPristineListingAttributes().size());
  }

  public void constructProductAndItemWithMasterDataPVSwitchTrueAndPristineItemTest() throws Exception {
    item.setPristineDataItem(new PristineDataItem());
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ReflectionTestUtils.setField(masterDataConstructorService, "isProductVisibilityEnabled", true);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @Test
  public void constructProductAndItemWithMasterDataPVSwitchFalseAndPristineItemTest() throws Exception {
    item.setPristineDataItem(new PristineDataItem());
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ReflectionTestUtils.setField(masterDataConstructorService, "isProductVisibilityEnabled", true);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), false, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.masterDataService);
    verifyNoMoreInteractions(this.masterDataCacheService);
    verifyNoMoreInteractions(this.productAndItemSolrRepository);
    verifyNoMoreInteractions(this.productCategoryBaseOutbound);
    verifyNoMoreInteractions(this.executorService);
    verifyNoMoreInteractions(this.systemParameterService);
  }

  @Test
  public void constructProductAndItemWithMasterData_PVSwitchTrueTest_disabledTrue() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
  }

  @Test
  public void constructProductAndItemWithMasterData_PVSwitchfalseTest_disabledPVSwitch() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ReflectionTestUtils.setField(masterDataConstructorService, "isProductVisibilityEnabled", true);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @Test
  public void constructProductAndItemWithMasterData_PVSwitchfalseTest_disabledPVSwitch_emptyPristineDataItem()
      throws Exception {
    item.setPristineDataItem(null);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ReflectionTestUtils.setField(masterDataConstructorService, "isProductVisibilityEnabled", true);

    when(
        this.masterDataService.getMasterDataProductDetailResponse(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
        masterDataProductMap);

    ProductAndItemsVO result =
        this.masterDataConstructorService.constructProductAndItemWithMasterData(
            MasterDataConstructorServiceImplTest.STORE_ID,
            MasterDataConstructorServiceImplTest.USERNAME,
            MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    verify(this.masterDataService).getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
        MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @Test
  public void constructProductAndItemWithMasterDataTestNonEmptyProductCode() throws Exception {
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    MasterDataProductAndItemsVO productAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    Map<String, MasterDataProductAndItemsVO> productAndItemsVOMap = new HashMap<>();
    productAndItemsVOMap.put(PRODUCT_CODE, productAndItemsVO);
    when(masterDataService
        .getMasterDataProductDetailResponseWithoutCache(STORE_ID, USERNAME, REQUEST_ID, ImmutableSet.of(PRODUCT_CODE),
            true)).thenReturn(productAndItemsVOMap);
    masterDataConstructorService
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true);
    verify(masterDataService)
        .getMasterDataProductDetailResponseWithoutCache(STORE_ID, USERNAME, REQUEST_ID, ImmutableSet.of(PRODUCT_CODE),
            true);
  }

  @Test
  public void constructProductAndItemWithMasterDataTestEmptyProductCode() throws Exception {
    product.setProductCode(StringUtils.EMPTY);
    masterDataConstructorService
        .constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true);
  }

  @Test
  public void constructProductAndItemWithMasterDataAttributesTest() throws Exception {
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setExtractedValue(true);
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    this.masterDataProductMap.forEach((key, value) -> {
      masterDataProduct.setMasterDataProductAttributes(
        Collections.singletonList(masterDataProductAttribute));
      value.setMasterDataProduct(masterDataProduct);
    });
    product.getMasterDataProduct().setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttribute));
    when(
      this.masterDataService.getMasterDataProductDetailResponse(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS)).thenReturn(
      masterDataProductMap);

    ProductAndItemsVO result =
      this.masterDataConstructorService.constructProductAndItemWithMasterData(
        MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME,
        MasterDataConstructorServiceImplTest.REQUEST_ID, product, Arrays.asList(item));

    verify(this.masterDataService).getMasterDataProductDetailResponse(
      MasterDataConstructorServiceImplTest.STORE_ID,
      MasterDataConstructorServiceImplTest.USERNAME,
      MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(result, productAndItemsVO);
    assertEquals(result.getProduct().getMasterDataProduct().getMasterCatalog(), masterCatalog);
    assertEquals(result.getItems().get(0).getMasterDataItem().getDangerousLevel(),
      MasterDataConstructorServiceImplTest.DANGEROUS_LEVEL);
  }

  @Test
  public void fetchMasterDataMapForTransaction_singleItemTest() throws Exception {
    Mockito.when(this.masterDataCacheService.getProductMasterDataForTransaction(ITEM_CODE))
      .thenReturn(productMasterDataResponse);
    Map<String, ProductMasterDataResponse> response =
      this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID, Arrays.asList(ITEM_CODE));
    Mockito.verify(this.masterDataCacheService).getProductMasterDataForTransaction(ITEM_CODE);
   assertTrue(response.containsKey(ITEM_CODE));
  }

  @Test
  public void fetchMasterDataMapForTransaction_exceptionOnInvokeTest() throws Exception {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
      SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE)).thenReturn(concurrentMasterDataFetchParameter);
    Mockito.when(this.executorService.invokeAll(Mockito.anyList())).thenThrow(
      ApplicationRuntimeException.class);
    Map<String, ProductMasterDataResponse> response =
      this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID,
        Arrays.asList(ITEM_CODE, ITEM_CODE_1));
    Mockito.verify(this.systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MASTER_DATA_CONCURRENT_SIZE);
    Mockito.verify(this.executorService).invokeAll(Mockito.anyList());
   assertTrue(MapUtils.isEmpty(response));
  }

  @Test
  public void fetchMasterDataMapForTransaction_emptyListTest() {
    Map<String, ProductMasterDataResponse> response =
      this.masterDataConstructorService.fetchMasterDataMapForTransaction(STORE_ID,
        Collections.emptyList());
   assertTrue(MapUtils.isEmpty(response));
  }

  @Test
  public void testRemoveHideCustomerAttributeFromResponse_WhenExcludeEnabledTest() throws Exception {
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode("ATTR1");
    specialAttributes.add(specialAttribute1);
    ProductSpecialAttribute specialAttribute2 = new ProductSpecialAttribute();
    specialAttribute2.setAttributeCode("ATTR2");
    specialAttributes.add(specialAttribute2);
    product.setProductSpecialAttributes(specialAttributes);
    List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();
    MasterDataProductAttribute masterDataAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute attribute1 = new MasterDataAttribute();
    attribute1.setAttributeCode("ATTR1");
    attribute1.setHideOnCustomerSide(false);
    masterDataAttribute1.setMasterDataAttribute(attribute1);
    masterDataAttributes.add(masterDataAttribute1);
    MasterDataProductAttribute masterDataAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute attribute2 = new MasterDataAttribute();
    attribute2.setAttributeCode("ATTR2");
    attribute2.setHideOnCustomerSide(true);
    masterDataAttribute2.setMasterDataAttribute(attribute2);
    masterDataAttributes.add(masterDataAttribute2);
    masterDataProduct.setMasterDataProductAttributes(masterDataAttributes);
    Map<String, MasterDataProductAndItemsVO> response = new HashMap<>();
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    MasterDataProductAndItemsVO productAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE, masterDataProductAndItemsVO);
    when(this.masterDataService.getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);
    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);
    masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
        Arrays.asList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
    List<MasterDataProductAttribute> remainingAttributes = masterDataProduct.getMasterDataProductAttributes();
    assertEquals(1, remainingAttributes.size());
    assertEquals("ATTR1", remainingAttributes.get(0).getMasterDataAttribute().getAttributeCode());
    assertFalse(remainingAttributes.get(0).getMasterDataAttribute().isHideOnCustomerSide());
    List<ProductSpecialAttribute> remainingSpecialAttributes = product.getProductSpecialAttributes();
    assertEquals(1, remainingSpecialAttributes.size());
    assertEquals("ATTR1", remainingSpecialAttributes.get(0).getAttributeCode());
  }

  @Test
  public void testRemoveHideCustomerAttributeFromResponse_ExceptionHandling() throws Exception {
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode("ATTR1");
    specialAttributes.add(specialAttribute1);
    ProductSpecialAttribute specialAttribute2 = new ProductSpecialAttribute();
    specialAttribute2.setAttributeCode("ATTR2");
    specialAttributes.add(specialAttribute2);
    product.setProductSpecialAttributes(specialAttributes);
    List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();
    MasterDataProductAttribute masterDataAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute attribute1 = new MasterDataAttribute();
    attribute1.setAttributeCode("ATTR1");
    attribute1.setHideOnCustomerSide(false);
    masterDataAttribute1.setMasterDataAttribute(attribute1);
    masterDataAttributes.add(masterDataAttribute1);
    MasterDataProductAttribute masterDataAttribute2 = Mockito.mock(MasterDataProductAttribute.class);
    MasterDataAttribute attribute2 = Mockito.mock(MasterDataAttribute.class);
    Mockito.when(masterDataAttribute2.getMasterDataAttribute()).thenThrow(new RuntimeException("Test Exception"));
    masterDataAttributes.add(masterDataAttribute2);
    masterDataProduct.setMasterDataProductAttributes(masterDataAttributes);
    Map<String, MasterDataProductAndItemsVO> response = new HashMap<>();
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    MasterDataProductAndItemsVO productAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    MasterDataProductAndItemsVO masterDataProductAndItems = new MasterDataProductAndItemsVO();
    masterDataProductAndItems.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItems.setMasterDataItems(masterDataItemMap);
    masterDataProductMap.put(PRODUCT_CODE, masterDataProductAndItems);
    when(masterDataService.getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProductMap);

    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);

    masterDataConstructorService.constructProductAndItemWithMasterData(
        STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    List<ProductSpecialAttribute> remainingSpecialAttributes = product.getProductSpecialAttributes();
    assertEquals(2, remainingSpecialAttributes.size());
    assertEquals("ATTR1", remainingSpecialAttributes.get(0).getAttributeCode());
    assertEquals("ATTR2", remainingSpecialAttributes.get(1).getAttributeCode());
    List<MasterDataProductAttribute> remainingAttributes = masterDataProduct.getMasterDataProductAttributes();
    assertEquals(2, remainingAttributes.size());
    assertEquals("ATTR1", remainingAttributes.get(0).getMasterDataAttribute().getAttributeCode());
    assertFalse(remainingAttributes.get(0).getMasterDataAttribute().isHideOnCustomerSide());

    verify(masterDataService).getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }


  @Test
  public void testRemoveHideCustomerAttributeFromResponse_WhenMasterDataProductAndItemsIsNull() throws Exception {
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute = new ProductSpecialAttribute();
    specialAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    specialAttribute.setAttributeValue(ATTRIBUTE_VALUE1);
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode(ATTRIBUTE_CODE1);
    specialAttribute1.setAttributeValue(ATTRIBUTE_VALUE1);
    specialAttributes.add(specialAttribute);
    specialAttributes.add(specialAttribute1);
    product.setProductSpecialAttributes(specialAttributes);
    ReflectionTestUtils.setField(masterDataConstructorService,"imeiAttributeCode",ATTRIBUTE_CODE1);
    ReflectionTestUtils.setField(masterDataConstructorService,"imeiAllowedValues", List.of("YES"));
    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);
    ProductAndItemsVO productAndItemsVO = masterDataConstructorService.constructProductAndItemWithMasterData(
        STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);
    verify(masterDataService).getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    List<ProductSpecialAttribute> resultSpecialAttributes = product.getProductSpecialAttributes();
    assertEquals(2, resultSpecialAttributes.size());
    assertTrue(productAndItemsVO.getProduct().isImeiRequired());
    assertEquals("ATTR1", resultSpecialAttributes.get(0).getAttributeCode());
  }


  @Test
  public void testRemoveHideCustomerAttributeFromResponse_WhenMasterDataProductAndItemsIsEmpty() throws Exception {
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode("ATTR1");
    specialAttributes.add(specialAttribute1);
    ProductSpecialAttribute specialAttribute2 = new ProductSpecialAttribute();
    specialAttribute2.setAttributeCode("ATTR2");
    specialAttributes.add(specialAttribute2);
    product.setProductSpecialAttributes(specialAttributes);
    List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();
    MasterDataProductAttribute masterDataAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute attribute1 = new MasterDataAttribute();
    attribute1.setAttributeCode("ATTR1");
    attribute1.setHideOnCustomerSide(false);
    masterDataAttribute1.setMasterDataAttribute(attribute1);
    masterDataAttributes.add(masterDataAttribute1);
    MasterDataProductAttribute masterDataAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute attribute2 = new MasterDataAttribute();
    attribute2.setAttributeCode("ATTR2");
    attribute2.setHideOnCustomerSide(true);
    masterDataAttribute2.setMasterDataAttribute(attribute2);
    masterDataAttributes.add(masterDataAttribute2);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    Map<String, MasterDataProductAndItemsVO> response = new HashMap<>();
    Map<String, MasterDataProductAndItemsVO> masterDataProductMap = new HashMap<String, MasterDataProductAndItemsVO>();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    MasterDataProductAndItemsVO productAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    MasterDataProductAndItemsVO masterDataProductAndItemsVO =
        new MasterDataProductAndItemsVO(masterDataProduct, masterDataItemMap);
    masterDataProductMap.put(MasterDataConstructorServiceImplTest.PRODUCT_CODE, masterDataProductAndItemsVO);
    when(this.masterDataService.getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS)).thenReturn(masterDataProductMap);
    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);
    masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME, REQUEST_ID, product,
        Arrays.asList(item), true, IN_ALL_PRODUCTS);
    verify(this.masterDataService).getMasterDataProductDetailResponse(MasterDataConstructorServiceImplTest.STORE_ID,
        MasterDataConstructorServiceImplTest.USERNAME, MasterDataConstructorServiceImplTest.REQUEST_ID, productCodes,
        IN_ALL_PRODUCTS);
    List<MasterDataProductAttribute> remainingAttributes = masterDataProduct.getMasterDataProductAttributes();
    assertEquals(0, remainingAttributes.size());
  }

  @Test
  public void testRemoveHideCustomerAttributeFromResponse_NullAttributeChecks() throws Exception {
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode("ATTR1");
    specialAttributes.add(specialAttribute1);
    
    ProductSpecialAttribute specialAttribute2 = new ProductSpecialAttribute();
    specialAttribute2.setAttributeCode("ATTR2");
    specialAttributes.add(specialAttribute2);
    
    product.setProductSpecialAttributes(specialAttributes);
    List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();
    MasterDataProductAttribute masterDataAttribute1 = null;
    masterDataAttributes.add(masterDataAttribute1);

    MasterDataProductAttribute masterDataAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute attribute2 = new MasterDataAttribute();
    attribute2.setAttributeCode("ATTR2");
    attribute2.setHideOnCustomerSide(true);
    masterDataAttribute2.setMasterDataAttribute(attribute2);
    masterDataAttributes.add(masterDataAttribute2);

    masterDataProduct.setMasterDataProductAttributes(masterDataAttributes);
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);
    
    MasterDataProductAndItemsVO masterDataProductAndItems = new MasterDataProductAndItemsVO();
    masterDataProductAndItems.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItems.setMasterDataItems(masterDataItemMap);
    masterDataProductMap.put(PRODUCT_CODE, masterDataProductAndItems);

    when(masterDataService.getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProductMap);

    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);

    masterDataConstructorService.constructProductAndItemWithMasterData(
        STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);

    List<MasterDataProductAttribute> remainingAttributes = masterDataProduct.getMasterDataProductAttributes();
    assertEquals(1, remainingAttributes.size());
    verify(masterDataService).getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
  }


  @Test
  public void testRemoveHideCustomerAttributeFromResponse_NullAttributeValueChecks() throws Exception {
    ReflectionTestUtils.setField(masterDataConstructorService,"imeiAttributeCode","ATTR2");
    ReflectionTestUtils.setField(masterDataConstructorService,"imeiAllowedValues", List.of("YES"));
    List<ProductSpecialAttribute> specialAttributes = new ArrayList<>();
    ProductSpecialAttribute specialAttribute1 = null;
    ProductSpecialAttribute specialAttribute2 = new ProductSpecialAttribute();
    specialAttribute2.setAttributeCode("ATTR2");
    specialAttribute2.setAttributeValue("YES");
    specialAttributes.add(specialAttribute2);
    specialAttributes.add(specialAttribute1);

    product.setProductSpecialAttributes(specialAttributes);
    List<MasterDataProductAttribute> masterDataAttributes = new ArrayList<>();

    MasterDataProductAttribute masterDataAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute attribute1 = null;
    masterDataAttribute1.setMasterDataAttribute(null);
    masterDataAttributes.add(masterDataAttribute1);

    MasterDataProductAttribute masterDataAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute attribute2 = new MasterDataAttribute();
    attribute2.setAttributeCode("ATTR2");
    attribute2.setHideOnCustomerSide(true);
    masterDataAttribute2.setMasterDataAttribute(attribute2);
    masterDataAttributes.add(masterDataAttribute2);

    masterDataProduct.setMasterDataProductAttributes(masterDataAttributes);
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE, masterDataItem);

    MasterDataProductAndItemsVO masterDataProductAndItems = new MasterDataProductAndItemsVO();
    masterDataProductAndItems.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItems.setMasterDataItems(masterDataItemMap);
    masterDataProductMap.put(PRODUCT_CODE, masterDataProductAndItems);
    when(masterDataService.getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS))
        .thenReturn(masterDataProductMap);
    ReflectionTestUtils.setField(masterDataConstructorService, "excludeHideOnCustomerSideAttributesEnabled", true);
    masterDataConstructorService.constructProductAndItemWithMasterData(
        STORE_ID, USERNAME, REQUEST_ID, product, Arrays.asList(item), true, IN_ALL_PRODUCTS);
    List<MasterDataProductAttribute> remainingAttributes = masterDataProduct.getMasterDataProductAttributes();
    verify(masterDataService).getMasterDataProductDetailResponse(
        STORE_ID, USERNAME, REQUEST_ID, productCodes, IN_ALL_PRODUCTS);
    assertEquals(1, remainingAttributes.size());
  }
}
