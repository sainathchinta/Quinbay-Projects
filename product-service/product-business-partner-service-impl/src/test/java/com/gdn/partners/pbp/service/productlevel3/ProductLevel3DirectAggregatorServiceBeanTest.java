package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorInventoryCriteria;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Aggregator.ProductLevel3AggregatorState;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class ProductLevel3DirectAggregatorServiceBeanTest {
  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  protected CategoryRepository categoryRepository;

  @Mock
  protected PickupPointRepository pickupPointRepository;

  @Mock
  protected ProductLevel3Converter modelConverter;
  
  @Mock
  private ProductLevel3AggregatorService productLevel3AggregatorService;

  @InjectMocks
  private ProductLevel3DirectAggregatorServiceBean aggregatorService;

  private static final String ITEM_SKU_1 = "BLI-00001-00001-00001";
  private static final String ITEM_SKU_2 = "BLI-00001-00001-00002";
  private static final String ITEM_SKU_3 = "BLI-00001-00001-00003";
  private static final String ITEM_SKU_4 = "BLI-00001-00002-00001";
  private static final String ITEM_SKU_5 = "BLI-00001-00002-00002";

  private static final int STOCK = 10;


  private static final String ITEM_NAME = "Produk Testing";
  private static final String ITEM_CODE = "MTA-0000001";
  private static final String MERCHANT_SKU = "MSKU-00001";
  private static final String PICKUP_POINT_CODE_1 = "PP-0000001";
  private static final String PICKUP_POINT_CODE_2 = "PP-0000002";
  private static final String BUSINESS_PARTNER_CODE = "BLI-00001";
  private static final String CATEGORY_CODE_1 = "CAT-0000001";
  private static final String CATEGORY_CODE_2 = "CAT-0000002";
  private static final String CATEGORY_CODE_3 = "CAT-0000003";
  private static final Pageable PAGEABLE = PageRequest.of(0, 10);
  private static final SortOrder SORT = new SortOrder("", "");

  private static final String PRODUCT_CODE = "product-code-10102";
  private static final String PRODUCT_SKU = "product-sku-10102";
  private static final String PROMO_BUNDLING = "PROMO_BUNDLING";
  private static final String PRODUCT_NAME = "product-name-10102";

  private ItemSummaryResponse itemWithCompleteData;
  private ItemSummaryResponse itemWithoutMasterDataCatalog;
  private ItemSummaryResponse itemWithoutMasterDataCategory;

  private Page<ItemSummaryResponse> pageOfItems;
  private List<ProductLevel3Inventory> inventories;
  private Map<String, ProductLevel3Inventory> mapOfProductInventories;
  private List<String> listOfGdnSkus;
  private PickupPointResponse pickupPoint = new PickupPointResponse();


  private Map<String, List<CategoryResponse>> mapOfProductCategories;
  private List<CategoryResponse> singleCategories = new ArrayList<>();
  private List<CategoryResponse> hierarchyCategories = new ArrayList<>();

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);

    // This is how data from x-product and x-inventory should be aggregated
    CategoryResponse category1 = new CategoryResponse();
    category1.setCategoryCode(CATEGORY_CODE_1);
    category1.setParentCategoryId(CATEGORY_CODE_2);
    category1.setName(CATEGORY_CODE_1);

    CategoryResponse category2 = new CategoryResponse();
    category2.setCategoryCode(CATEGORY_CODE_2);
    category2.setParentCategoryId(CATEGORY_CODE_3);
    category2.setName(CATEGORY_CODE_2);

    CategoryResponse category3 = new CategoryResponse();
    category3.setCategoryCode(CATEGORY_CODE_3);
    category3.setName(CATEGORY_CODE_3);

    singleCategories.add(category1);

    hierarchyCategories.add(category1);
    hierarchyCategories.add(category2);
    hierarchyCategories.add(category3);
  }

  private ProductLevel3Inventory generateInventory(ItemSummaryResponse item) {
    ProductLevel3Inventory inventory = new ProductLevel3Inventory();
    inventory.setWebItemSku(item.getItemSku());
    inventory.setNonDistributionReserved(10);
    inventory.setNonDistributionAvailable(20);
    return inventory;
  }

  private Map<String, ProductLevel3Inventory> generateMapOfProductInventories(
      List<ItemSummaryResponse> items, List<ProductLevel3Inventory> inventories) {
    Map<String, ProductLevel3Inventory> map = new HashMap<>();
    items.forEach(item -> {
      inventories.forEach(inventory -> {
        if (inventory.getWebItemSku().equals(item.getItemSku())) {
          map.put(item.getItemSku(), inventory);
        }
      });
    });
    return map;
  }


  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    item.setProductCode(PRODUCT_CODE);
    item.setProductSku(PRODUCT_SKU);
    item.setPriceEditDisabled(true);
    item.setProductScore(90.0);
    item.setProductName(PRODUCT_NAME);
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    Assertions.assertEquals(result.getContent().get(0).getProductCode(), PRODUCT_CODE);
    Assertions.assertEquals(result.getContent().get(0).getProductSku(), PRODUCT_SKU);

    Assertions.assertEquals(result.getContent().get(0).getNonDistributionAvailable().intValue(), 20);
    Assertions.assertEquals(result.getContent().get(0).getNonDistributionReserved().intValue(), 10);
    Assertions.assertEquals(90.0, result.getContent().get(0).getProductScore(), 0);
    Assertions.assertTrue(result.getContent().get(0).isPriceEditDisabled());
    Assertions.assertEquals(PRODUCT_NAME, result.getContent().get(0).getProductName());
  }

  @Test
  public void aggregateProductLevel3SummaryPromoTypesTest()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    item.setProductCode(PRODUCT_CODE);
    item.setProductSku(PRODUCT_SKU);
    item.setPromoTypes(Arrays.asList(PROMO_BUNDLING));
    item.setForceReview(true);
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    Assertions.assertEquals(result.getContent().get(0).getProductCode(), PRODUCT_CODE);
    Assertions.assertEquals(result.getContent().get(0).getProductSku(), PRODUCT_SKU);

    Assertions.assertEquals(result.getContent().get(0).getNonDistributionAvailable().intValue(), 20);
    Assertions.assertEquals(result.getContent().get(0).getNonDistributionReserved().intValue(), 10);

    Assertions.assertEquals(result.getContent().get(0).getPromoTypes().size(), 1);
    Assertions.assertEquals(result.getContent().get(0).getPromoTypes().get(0), PROMO_BUNDLING);
    Assertions.assertTrue(result.getContent().get(0).isForceReview());
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_WithHirerarchyCategories_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        hierarchyCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));

    String expectedCategoryName = "";
    StringBuilder expectedCategoryHierarchy = new StringBuilder();
    if (hierarchyCategories.size() > 1) {
      int i = 0;
      ListIterator<CategoryResponse> iterator =
          hierarchyCategories.listIterator(hierarchyCategories.size());
      while (iterator.hasPrevious()) {
        CategoryResponse category = iterator.previous();
        if (i == hierarchyCategories.size() - 1) {
          expectedCategoryName = category.getName();
        }
        expectedCategoryHierarchy.append(category.getName());
        if (hierarchyCategories.size() > 1 && i < hierarchyCategories.size() - 1) {
          expectedCategoryHierarchy.append(" > ");
        }
        i++;

      }
    } else {
      expectedCategoryName = hierarchyCategories.get(0).getName();
      expectedCategoryHierarchy.append(expectedCategoryName);
    }

    for (ProductLevel3Summary product : result.getContent()) {
      System.out.println("Category name: " + product.getCategoryName());
      System.out.println("Category hierarchy: " + product.getCategoryHierarchy());
      Assertions.assertEquals(expectedCategoryName, product.getCategoryName());
      Assertions.assertEquals(expectedCategoryHierarchy.toString(), product.getCategoryHierarchy());
    }
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_InventoryDataNotFound_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    // inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());

    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_NotFoundCategory_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenThrow(
        new RuntimeException("Test not found category"));

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    result.getContent().forEach(product -> {
      Assertions.assertTrue(StringUtils.isBlank(product.getCategoryName()));
      Assertions.assertTrue(StringUtils.isBlank(product.getCategoryHierarchy()));
    });
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_TwoItemsWithSameCategory_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item1 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);

    ItemSummaryResponse item2 =
        new ItemSummaryResponse(ITEM_SKU_2, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_2, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);

    item1.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    item2.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item1);
    items.add(item2);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item1));
    inventories.add(generateInventory(item2));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(1)).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WholesalePrice() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item1 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
            new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item1.setWholesalePriceActivated(true);

    ItemSummaryResponse item2 =
        new ItemSummaryResponse(ITEM_SKU_2, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_2,
            new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item2.setWholesalePriceActivated(false);

    item1.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    item2.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item1);
    items.add(item2);
    Page<ItemSummaryResponse> pageOfItems = new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item1));
    inventories.add(generateInventory(item2));

    Map<String, ProductLevel3Inventory> mapOfProductInventories = generateMapOfProductInventories(items, inventories);

    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito.when(modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems.getContent())))
        .thenReturn(listOfGdnSkus);

    Mockito.when(productLevel3InventoryService
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(), listOfGdnSkus))
        .thenReturn(inventories);

    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);

    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(), listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(1)).findHierarchyByCategoryCode(Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    Assertions.assertTrue(result.getContent().get(0).getWholesalePriceActivated());
    Assertions.assertFalse(result.getContent().get(1).getWholesalePriceActivated());
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_TwoItemsWithSamePickupPoint_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item1 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);

    ItemSummaryResponse item2 =
        new ItemSummaryResponse(ITEM_SKU_2, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);

    item1.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    item2.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_2, null));
    item1.setMerchantPromoDiscount(true);
    item1.setMerchantPromoDiscountActivated(true);
    items.add(item1);
    items.add(item2);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item1));
    inventories.add(generateInventory(item2));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(1)).findByPickupPointCode(
        Mockito.anyString());

    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
    Assertions.assertTrue(result.getContent().get(0).isMerchantPromoDiscount());
    Assertions.assertTrue(result.getContent().get(0).isMerchantPromoDiscountActivated());
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_TwoItems_OneWithoutMasterDataCatalog_And_OneWithoutCategory_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item1 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    ItemSummaryResponse item2 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, null,
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR,
            PICKUP_POINT_CODE_2, new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    items.add(item1);
    items.add(item2);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item1));
    inventories.add(generateInventory(item2));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        Mockito.eq(filterRequest));
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            Mockito.eq(filterRequest.getBusinessPartnerCode()), Mockito.eq(listOfGdnSkus));
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(
        Mockito.eq(inventories));
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_Minified_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_WithoutGdnSkuList_Minified_InventoryDataNotFound_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    // inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_GdnSkuList_Minified_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setGdnSku(ITEM_SKU_1);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(),
            listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }


  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_GdnSkuList_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setGdnSku(ITEM_SKU_1);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Mockito
        .when(
            modelConverter.convertItemSummaryResponseToListOfGdnSku(Mockito.eq(pageOfItems
                .getContent()))).thenReturn(listOfGdnSkus);

    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            filterRequest.getBusinessPartnerCode(), listOfGdnSkus)).thenReturn(inventories);

    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);
    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(filterRequest.getBusinessPartnerCode(), listOfGdnSkus);
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(inventories);
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_StockFilter_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setStock(STOCK);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });

    Mockito.when(
        productLevel3InventoryService.findInventoryByInventoryFilter(
            filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
            filterRequest.getInventoryFilter())).thenReturn(inventories);

    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null)))
      .thenReturn(mapOfProductInventories);

    Mockito.when(
        modelConverter.convertProductLevel3InventoryToListOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(listOfGdnSkus);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);


    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService).findInventoryByInventoryFilter(
        filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
        filterRequest.getInventoryFilter());
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null));
    Mockito.verify(categoryRepository, Mockito.times(items.size())).findHierarchyByCategoryCode(
        Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(items.size())).findByPickupPointCode(
        Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_WithoutInventoryFilter_StockFilter_Minified_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setStock(STOCK);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });

    Mockito.when(
        productLevel3InventoryService.findInventoryByInventoryFilter(
            filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
            filterRequest.getInventoryFilter())).thenReturn(inventories);

    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null)))
      .thenReturn(mapOfProductInventories);

    Mockito.when(
        modelConverter.convertProductLevel3InventoryToListOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(listOfGdnSkus);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService).findInventoryByInventoryFilter(
        filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
        filterRequest.getInventoryFilter());
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null));
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithoutStockFilter_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
            new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems = new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories = generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku("test");
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
        new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(
        this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
        productLevel3Aggregators);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
            Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
            Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
        mapOfProductInventories);
    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);
    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
        Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
        Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Mockito.verify(this.pickupPointRepository).findByPickupPointCode(Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithGdnSKUs_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    filterRequest.setItemSkus(Arrays.asList(ITEM_SKU_1));
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
      new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
        new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
        new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems = new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories = generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku(ITEM_SKU_1);
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
      new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
      .thenReturn(itemFilterRequest);
    Mockito.when(
      this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
        Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
      productLevel3Aggregators);
    Mockito.when(
      productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
        Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
      productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
        Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
      mapOfProductInventories);
    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);
    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
      Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
      Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
      Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
      Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Mockito.verify(this.pickupPointRepository).findByPickupPointCode(Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithNonMatchingGdnSKUs_NoDataTest() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    filterRequest.setItemSkus(Arrays.asList(ITEM_SKU_2));
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
      new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
        new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
        new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku(ITEM_SKU_1);
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
      new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
      .thenReturn(itemFilterRequest);
    Mockito.when(
      this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
        Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
      productLevel3Aggregators);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);
    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
      Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
      Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));

    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithNullGdnSKUs_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    filterRequest.setItemSkus(Arrays.asList(ITEM_SKU_1));
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
      new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
        new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
        new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems = new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories = generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator1 = new ProductLevel3Aggregator();
    productLevel3Aggregator1.setGdnSku(ITEM_SKU_1);
    ProductLevel3Aggregator productLevel3Aggregator2 = new ProductLevel3Aggregator();
    productLevel3Aggregator2.setGdnSku(null);
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
      new PageImpl<>(Arrays.asList(productLevel3Aggregator1, productLevel3Aggregator2));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
      .thenReturn(itemFilterRequest);
    Mockito.when(
      this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
        Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
      productLevel3Aggregators);
    Mockito.when(
      productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
        Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
      productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
        Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
      mapOfProductInventories);
    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);
    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
      Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
      Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
      Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
      Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Mockito.verify(this.pickupPointRepository).findByPickupPointCode(Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_EmptyResponseForProductLevel3Aggregator_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory->{
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    Page<ProductLevel3Aggregator> productLevel3Aggregators = new PageImpl<>(new ArrayList<>());
    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(this.productLevel3AggregatorService
        .findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class)))
        .thenReturn(productLevel3Aggregators);
    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService)
        .findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithoutStockFilter_2_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.STOCK_ALERT);
    filterRequest.setArchived(false);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(false);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, new MasterCatalogDTO(),
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR, PICKUP_POINT_CODE_1,
            new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems = new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories = generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku("test");
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
        new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(
        this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
        productLevel3Aggregators);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
            Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
            Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
        mapOfProductInventories);
    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(pickupPoint);
    Page<ProductLevel3Summary> result = aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
        Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
        Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Mockito.verify(this.pickupPointRepository).findByPickupPointCode(Mockito.anyString());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithoutStockFilter_Minified_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku("test");
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
        new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(
        this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
        productLevel3Aggregators);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
            Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
            Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
        mapOfProductInventories);
    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
        Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
        Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3SummaryMinified_EmptyResponseForProductLevel3Aggregator_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.OOS);
    filterRequest.setArchived(true);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(true);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
        new PageImpl<>(new ArrayList<>());
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(
        this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
        productLevel3Aggregators);
    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
        Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(modelConverter)
        .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }
  
  @Test
  public void aggregateProductLevel3Summary_InventoryFilter_WithoutStockFilter_Minified_2_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setInventoryFilter(ProductLevel3InventoryCriteria.STOCK_ALERT);
    filterRequest.setArchived(false);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    itemFilterRequest.setArchived(false);
    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());
    List<String> listOfGdnSkus = new ArrayList<>();
    List<ProductLevel3Inventory> inventories = new ArrayList<>();
    inventories.add(generateInventory(item));
    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);
    inventories.forEach(inventory -> {
      listOfGdnSkus.add(inventory.getWebItemSku());
    });
    ProductLevel3Aggregator productLevel3Aggregator = new ProductLevel3Aggregator();
    productLevel3Aggregator.setGdnSku("test");
    Page<ProductLevel3Aggregator> productLevel3Aggregators =
        new PageImpl<>(Arrays.asList(productLevel3Aggregator));
    Mockito.when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);
    Mockito.when(
        this.productLevel3AggregatorService.findByBusinessPartnerCodeAndInventoryFilterAndState(Mockito.anyString(),
            Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
            Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class))).thenReturn(
        productLevel3Aggregators);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class), Mockito.any(Pageable.class),
            Mockito.any(SortOrder.class))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(Mockito.anyString(),
            Mockito.anyList())).thenReturn(inventories);
    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList())).thenReturn(
        mapOfProductInventories);
    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);
    Mockito.verify(this.productLevel3AggregatorService).findByBusinessPartnerCodeAndInventoryFilterAndState(
        Mockito.anyString(), Mockito.any(ProductLevel3AggregatorInventoryCriteria.class),
        Mockito.any(ProductLevel3AggregatorState.class), Mockito.any(Pageable.class));
    Mockito.verify(this.productLevel3Repository).findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.any(Pageable.class), Mockito.any(SortOrder.class));
    Mockito.verify(this.productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndListOfGdnSku(
        Mockito.anyString(), Mockito.anyList());
    Mockito.verify(this.modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.anyList());
    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilterAndStockFilter_InventoryDataNotFound_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setStock(STOCK);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        productLevel3InventoryService.findInventoryByInventoryFilter(
            filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
            filterRequest.getInventoryFilter())).thenReturn(inventories);

    Mockito.when(
        modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories),
            Mockito.eq(Arrays.asList(filterRequest.getGdnSku())))).thenReturn(mapOfProductInventories);

    Mockito.when(
        modelConverter.convertProductLevel3InventoryToListOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(listOfGdnSkus);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);


    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService).findInventoryByInventoryFilter(
        filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
        filterRequest.getInventoryFilter());
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null));
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductLevel3Summary_InventoryFilterAndStockFilter_InventoryDataNotFound_Minified_Test()
      throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setStock(STOCK);
    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);


    List<ItemSummaryResponse> items = new ArrayList<>();
    ItemSummaryResponse item =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));
    items.add(item);
    Page<ItemSummaryResponse> pageOfItems =
        new PageImpl<ItemSummaryResponse>(items, PAGEABLE, items.size());

    List<String> listOfGdnSkus = new ArrayList<>();

    List<ProductLevel3Inventory> inventories = new ArrayList<>();

    Map<String, ProductLevel3Inventory> mapOfProductInventories =
        generateMapOfProductInventories(items, inventories);

    Mockito.when(
        productLevel3InventoryService.findInventoryByInventoryFilter(
            filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
            filterRequest.getInventoryFilter())).thenReturn(inventories);

    Mockito.when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories),
      Mockito.eq(Arrays.asList(filterRequest.getGdnSku()))))
      .thenReturn(mapOfProductInventories);

    Mockito.when(
        modelConverter.convertProductLevel3InventoryToListOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(listOfGdnSkus);

    Mockito.when(
        modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
        .thenReturn(itemFilterRequest);

    Mockito.when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, SORT))
        .thenReturn(pageOfItems);


    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        singleCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.anyString())).thenReturn(
        pickupPoint);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(modelConverter).convertProductLevel3SummaryFilterRequestToItemSummaryRequest(
        filterRequest);
    Mockito.verify(productLevel3InventoryService).findInventoryByInventoryFilter(
        filterRequest.getBusinessPartnerCode(), filterRequest.getStock(),
        filterRequest.getInventoryFilter());
    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories), Mockito.eq(null));
    Assertions.assertTrue(CollectionUtils.isEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductSummaryWithoutInventoryTest() throws Exception {
    ProductLevel3SummaryFilter filterRequest = ProductLevel3SummaryFilter.builder()
      .businessPartnerCode(BUSINESS_PARTNER_CODE)
      .build();

    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    ItemSummaryResponse item = new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
        new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
        ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
        BUSINESS_PARTNER_CODE, null);
    item.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));

    Mockito
      .when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
      .thenReturn(itemFilterRequest);

    Mockito
      .when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, null))
      .thenReturn(new PageImpl<>(Arrays.asList(item), PAGEABLE, 1));

    Mockito
      .when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1))
      .thenReturn(singleCategories);

    Page<ProductLevel3Summary> result =
      aggregatorService.aggregateProductSummaryWithoutInventory(filterRequest, PAGEABLE);

    Mockito.verify(modelConverter)
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);

    Mockito.verify(productLevel3Repository).findSummaryByFilter(itemFilterRequest, PAGEABLE, null);
    Mockito.verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(result.getContent()));
  }

  @Test
  public void aggregateProductSummaryWithoutInventoryTest_whenProductClientFails() throws Exception {
    ProductLevel3SummaryFilter filterRequest = ProductLevel3SummaryFilter.builder()
      .businessPartnerCode(BUSINESS_PARTNER_CODE)
      .build();

    ItemSummaryRequest itemFilterRequest = new ItemSummaryRequest();
    itemFilterRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    Mockito
      .when(modelConverter.convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest))
      .thenReturn(itemFilterRequest);

    Mockito
      .when(productLevel3Repository.findSummaryByFilter(itemFilterRequest, PAGEABLE, null))
      .thenThrow(Exception.class);

    try {
      aggregatorService.aggregateProductSummaryWithoutInventory(filterRequest, PAGEABLE);
    } catch(Exception e){
      Mockito.verify(modelConverter)
        .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(filterRequest);
      Mockito.verify(productLevel3Repository).findSummaryByFilter(itemFilterRequest, PAGEABLE, null);
    }
  }

}
