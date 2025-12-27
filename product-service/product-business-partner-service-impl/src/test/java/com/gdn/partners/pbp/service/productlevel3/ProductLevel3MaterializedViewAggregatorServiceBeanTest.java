package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

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

import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepository;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class ProductLevel3MaterializedViewAggregatorServiceBeanTest {
  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private MerchantProductMVRepository merchantProductMVRepository;

  @Mock
  protected CategoryRepository categoryRepository;

  @Mock
  protected PickupPointRepository pickupPointRepository;

  @Mock
  protected ProductLevel3Converter modelConverter;

  @InjectMocks
  private ProductLevel3MaterializedViewAggregatorServiceBean aggregatorService;

  private static final String ITEM_SKU_1 = "BLI-00001-00001-00001";
  private static final String ITEM_SKU_2 = "BLI-00001-00001-00002";
  private static final String ITEM_SKU_3 = "BLI-00001-00001-00003";
  private static final String ITEM_SKU_4 = "BLI-00001-00002-00001";
  private static final String ITEM_SKU_5 = "BLI-00001-00002-00002";
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

  Page<MerchantProductMV> pageOfMerchantProductMV;
  Page<ItemSummaryResponse> pageOfItems;
  List<ProductLevel3Inventory> inventories;
  Map<String, ProductLevel3Inventory> mapOfProductInventories;

  List<ItemSummaryResponse> skippedItems;
  List<ItemSummaryResponse> notSkippedItems;

  List<CategoryResponse> masterDataCategories;

  PickupPointResponse pickupPoint1;
  PickupPointResponse pickupPoint2;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);

    List<MerchantProductMV> merchantProductMVs = new ArrayList<>();
    List<ItemSummaryResponse> totalItems = new ArrayList<>();
    inventories = new ArrayList<>();
    mapOfProductInventories = new HashMap<>();
    skippedItems = new ArrayList<>();
    notSkippedItems = new ArrayList<>();
    ItemSummaryResponse item1 =
        new ItemSummaryResponse(ITEM_SKU_1, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item1.setWholesalePriceActivated(true);
    item1.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));

    ItemSummaryResponse item2 =
        new ItemSummaryResponse(ITEM_SKU_2, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_1, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item2.setWholesalePriceActivated(false);
    item2.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_1, null));

    ItemSummaryResponse item3 =
        new ItemSummaryResponse(ITEM_SKU_3, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_2, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);
    item3.setWholesalePriceActivated(null);
    item3.getMasterCatalog().setCategory(new CategoryDTO(CATEGORY_CODE_2, null));

    ItemSummaryResponse skippedItem1 =
        new ItemSummaryResponse(ITEM_SKU_4, ITEM_CODE, MERCHANT_SKU, ITEM_NAME,
            new MasterCatalogDTO(), new ArrayList<>(), new HashSet<>(), new HashSet<>(),
            ProductType.REGULAR, PICKUP_POINT_CODE_2, new ArrayList<>(), false,
            BUSINESS_PARTNER_CODE, null);

    ItemSummaryResponse skippedItem2 =
        new ItemSummaryResponse(ITEM_SKU_5, ITEM_CODE, MERCHANT_SKU, ITEM_NAME, null,
            new ArrayList<>(), new HashSet<>(), new HashSet<>(), ProductType.REGULAR,
            PICKUP_POINT_CODE_2, new ArrayList<>(), false, BUSINESS_PARTNER_CODE, null);

    totalItems.add(item1);
    totalItems.add(item2);
    totalItems.add(item3);
    totalItems.add(skippedItem1);
    totalItems.add(skippedItem2);

    skippedItems.add(skippedItem1);
    skippedItems.add(skippedItem2);

    notSkippedItems.add(item1);
    notSkippedItems.add(item2);
    notSkippedItems.add(item3);

    pageOfItems = new PageImpl<>(totalItems);

    totalItems.forEach(item -> {
      MerchantProductMV merchantProductMV = new MerchantProductMV();
      merchantProductMV.setItemSku(item.getItemSku());

      ProductLevel3Inventory inventory = new ProductLevel3Inventory();
      inventory.setWebItemSku(item.getItemSku());

      merchantProductMVs.add(merchantProductMV);
      inventories.add(inventory);
      mapOfProductInventories.put(item.getItemSku(), inventory);
    });

    pageOfMerchantProductMV = new PageImpl<>(merchantProductMVs);

    masterDataCategories = new ArrayList<>();

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

    masterDataCategories.add(category1);
    masterDataCategories.add(category2);
    masterDataCategories.add(category3);

    pickupPoint1 = new PickupPointResponse();
    pickupPoint1.setCode(PICKUP_POINT_CODE_1);

    pickupPoint2 = new PickupPointResponse();
    pickupPoint2.setCode(PICKUP_POINT_CODE_2);
  }

  @Test
  public void aggregateProductLevel3Summary_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    Mockito.when(
        merchantProductMVRepository.findByFilter(Mockito.eq(filterRequest), Mockito.eq(PAGEABLE),
            Mockito.eq(SORT))).thenReturn(pageOfMerchantProductMV);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
            Mockito.eq(PAGEABLE), Mockito.eq(SORT))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            Mockito.eq(filterRequest.getBusinessPartnerCode()), Mockito.anyList())).thenReturn(
        inventories);
    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString())).thenReturn(
        masterDataCategories);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.eq(PICKUP_POINT_CODE_1)))
        .thenReturn(pickupPoint1);

    Mockito.when(pickupPointRepository.findByPickupPointCode(Mockito.eq(PICKUP_POINT_CODE_2)))
        .thenReturn(pickupPoint2);


    Page<ProductLevel3Summary> result =
        aggregatorService.aggregateProductLevel3Summary(filterRequest, PAGEABLE, SORT);

    Mockito.verify(merchantProductMVRepository).findByFilter(Mockito.eq(filterRequest),
        Mockito.eq(PAGEABLE), Mockito.eq(SORT));

    Mockito.verify(productLevel3Repository).findSummaryByFilter(
        Mockito.any(ItemSummaryRequest.class), Mockito.eq(PAGEABLE), Mockito.eq(SORT));

    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            Mockito.eq(filterRequest.getBusinessPartnerCode()), Mockito.anyList());

    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(
        Mockito.eq(inventories));

    Mockito.verify(categoryRepository, Mockito.times(notSkippedItems.size()))
        .findHierarchyByCategoryCode(Mockito.anyString());
    Mockito.verify(pickupPointRepository, Mockito.times(2)).findByPickupPointCode(
        Mockito.anyString());

    Assertions.assertEquals(notSkippedItems.size(), result.getContent().size());
  }

  @Test
  public void aggregateProductLevel3Summary_Minified_Test() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    Mockito.when(
        merchantProductMVRepository.findByFilter(Mockito.eq(filterRequest), Mockito.eq(PAGEABLE),
            Mockito.eq(SORT))).thenReturn(pageOfMerchantProductMV);
    Mockito.when(
        productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
            Mockito.eq(PAGEABLE), Mockito.eq(SORT))).thenReturn(pageOfItems);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            Mockito.eq(filterRequest.getBusinessPartnerCode()), Mockito.anyList())).thenReturn(
        inventories);
    Mockito
        .when(modelConverter.convertProductLevel3InventoryToMapOfGdnSku(Mockito.eq(inventories)))
        .thenReturn(mapOfProductInventories);

    Page<ProductLevel3SummaryMinified> result =
        aggregatorService.aggregateProductLevel3SummaryMinified(filterRequest, PAGEABLE, SORT);

    Mockito.verify(merchantProductMVRepository).findByFilter(Mockito.eq(filterRequest),
        Mockito.eq(PAGEABLE), Mockito.eq(SORT));

    Mockito.verify(productLevel3Repository).findSummaryByFilter(
        Mockito.any(ItemSummaryRequest.class), Mockito.eq(PAGEABLE), Mockito.eq(SORT));

    Mockito.verify(productLevel3InventoryService)
        .findInventoryByBusinessPartnerCodeAndListOfGdnSku(
            Mockito.eq(filterRequest.getBusinessPartnerCode()), Mockito.anyList());

    Mockito.verify(modelConverter).convertProductLevel3InventoryToMapOfGdnSku(
        Mockito.eq(inventories));

    Assertions.assertEquals(notSkippedItems.size(), result.getContent().size());
  }

  @Test
  public void aggregateProductSummaryWithoutInventoryTest() throws Exception {
    ProductLevel3SummaryFilter filterRequest = new ProductLevel3SummaryFilter();
    Mockito.when(merchantProductMVRepository.findByFilter(filterRequest, PAGEABLE, null))
      .thenReturn(pageOfMerchantProductMV);

    Mockito.when(productLevel3Repository.findSummaryByFilter(Mockito.any(ItemSummaryRequest.class),
        Mockito.eq(PAGEABLE), Mockito.eq(null)))
      .thenReturn(pageOfItems);

    Mockito.when(categoryRepository.findHierarchyByCategoryCode(Mockito.anyString()))
      .thenReturn(masterDataCategories);

    Page<ProductLevel3Summary> result =
      aggregatorService.aggregateProductSummaryWithoutInventory(filterRequest, PAGEABLE);

    Mockito.verify(merchantProductMVRepository).findByFilter(Mockito.eq(filterRequest),
      Mockito.eq(PAGEABLE), Mockito.eq(null));

    Mockito.verify(productLevel3Repository).findSummaryByFilter(
      Mockito.any(ItemSummaryRequest.class), Mockito.eq(PAGEABLE), Mockito.eq(null));

    Mockito.verify(categoryRepository, Mockito.times(notSkippedItems.size()))
      .findHierarchyByCategoryCode(Mockito.anyString());

    Assertions.assertEquals(notSkippedItems.size(), result.getContent().size());
  }


}
