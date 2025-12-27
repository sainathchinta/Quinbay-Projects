package com.gdn.partners.pbp.service.productlevel3;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;


import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3SummaryDetailsImage;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilterDetails;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.WebInventoryResponseV2DTO;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryResponseDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.product.rest.web.model.request.ItemsSummaryDetailRequest;

public class ProductLevel3ConverterBeanTest {

  private static final String BUSINESS_PARTNER_CODE = "BusinessPartnerCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_SKU_1 = "itemSku1";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_NAME = "itemName";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final Integer AVAILABLE_STOCK = 1;
  private static final Integer ORIGINAL_STOCK = 2;
  private static final Integer MIN_STOCK = 3;
  private static final Boolean SYNC_STOCK = true;
  private static final Boolean BUYABLE = true;
  private static final Boolean IS_ARCHIVED = true;
  private static final Boolean INCLUDE_ALL_TRADING_PRODUCT = false;
  private static final double ORIGINAL_PRICE = 3.3;

  @InjectMocks
  private ProductLevel3ConverterBean productLevel3ConverterBean;

  private WebInventoryResponseDTO webInventoryResponseDTO;
  private WebInventoryResponseV2DTO webInventoryResponseV2DTO;
  private WarehouseInventoryResponseDTO warehouseInventoryResponseDTO;
  private List<WarehouseInventoryResponseDTO> warehouseInventoryResponseDTOList;
  private InventoryDetailInfoResponseV2DTO inventoryDetailInfoResponseV2DTO;
  private InventoryDetailInfoResponseDTO inventoryDetailInfoResponseDTO;
  private ProductLevel3Inventory productLevel3Inventory1;
  private ProductLevel3Inventory productLevel3Inventory2;
  private List<ProductLevel3Inventory> productLevel3InventoryList;
  private Map<String, ProductLevel3Inventory> productLevel3InventoryMap;
  private ItemSummaryResponse itemSummaryResponse;
  private ItemSummaryResponse itemSummaryResponse1;
  private ItemSummaryResponse itemSummaryResponse2;
  private List<ItemSummaryResponse> itemSummaryResponseList;
  private List<String> gdnSkus;
  private ProductLevel3SummaryFilter productLevel3SummaryFilter;
  private ProductLevel3ItemSearch productLevel3ItemSearch;

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);

    this.webInventoryResponseV2DTO = new WebInventoryResponseV2DTO();
    this.webInventoryResponseV2DTO.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.webInventoryResponseV2DTO
        .setWebMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.webInventoryResponseV2DTO.setWarehouseItemSku(ProductLevel3ConverterBeanTest.ITEM_CODE);
    this.webInventoryResponseV2DTO
        .setWarehouseMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.webInventoryResponseV2DTO.setAvailableStock(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK);
    this.webInventoryResponseV2DTO.setOriginalStock(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK);
    this.webInventoryResponseV2DTO.setMinimumStockAlert(ProductLevel3ConverterBeanTest.MIN_STOCK);
    this.webInventoryResponseV2DTO.setSyncStock(ProductLevel3ConverterBeanTest.SYNC_STOCK);

    this.webInventoryResponseDTO = new WebInventoryResponseDTO();
    this.webInventoryResponseDTO.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.webInventoryResponseDTO
        .setWebMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.webInventoryResponseDTO.setWarehouseItemSku(ProductLevel3ConverterBeanTest.ITEM_CODE);
    this.webInventoryResponseDTO
        .setWarehouseMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.webInventoryResponseDTO.setAvailableStock(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK);
    this.webInventoryResponseDTO.setOriginalStock(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK);
    this.webInventoryResponseDTO.setMinimumStockAlert(ProductLevel3ConverterBeanTest.MIN_STOCK);
    this.webInventoryResponseDTO.setSyncStock(ProductLevel3ConverterBeanTest.SYNC_STOCK);
    this.warehouseInventoryResponseDTO = new WarehouseInventoryResponseDTO();
    this.warehouseInventoryResponseDTO
        .setAvailableStock(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK);
    this.warehouseInventoryResponseDTO
        .setOriginalStock(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK);
    this.warehouseInventoryResponseDTOList = new ArrayList<>();
    this.warehouseInventoryResponseDTOList.add(this.warehouseInventoryResponseDTO);
    this.inventoryDetailInfoResponseV2DTO = new InventoryDetailInfoResponseV2DTO();
    this.inventoryDetailInfoResponseV2DTO.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.inventoryDetailInfoResponseV2DTO
        .setWebMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.inventoryDetailInfoResponseV2DTO.setWebInventoryResponse(this.webInventoryResponseV2DTO);
    this.inventoryDetailInfoResponseV2DTO
        .setWarehouseInventoryResponseList(this.warehouseInventoryResponseDTOList);

    this.inventoryDetailInfoResponseDTO = new InventoryDetailInfoResponseDTO();
    this.inventoryDetailInfoResponseDTO.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.inventoryDetailInfoResponseDTO
        .setWebMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.inventoryDetailInfoResponseDTO.setWebInventoryResponse(this.webInventoryResponseDTO);
    this.inventoryDetailInfoResponseDTO
        .setWarehouseInventoryResponseList(this.warehouseInventoryResponseDTOList);
    this.itemSummaryResponse = new ItemSummaryResponse();
    this.itemSummaryResponse.setMerchantCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.itemSummaryResponse.setProductSku(ProductLevel3ConverterBeanTest.PRODUCT_SKU);
    this.itemSummaryResponse.setProductCode(ProductLevel3ConverterBeanTest.PRODUCT_CODE);
    this.itemSummaryResponse.setItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.itemSummaryResponse.setItemCode(ProductLevel3ConverterBeanTest.ITEM_CODE);
    this.itemSummaryResponse.setMerchantSku(ProductLevel3ConverterBeanTest.MERCHANT_SKU);
    this.itemSummaryResponse.setGeneratedItemName(ProductLevel3ConverterBeanTest.ITEM_NAME);
    this.productLevel3Inventory1 = new ProductLevel3Inventory();
    this.productLevel3Inventory1.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU_1);
    this.productLevel3Inventory2 = new ProductLevel3Inventory();
    this.productLevel3Inventory2.setWebItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU_2);
    this.productLevel3InventoryList = new ArrayList<>();
    this.productLevel3InventoryList.add(this.productLevel3Inventory1);
    this.productLevel3InventoryList.add(this.productLevel3Inventory2);
    this.productLevel3InventoryMap = new HashMap<>();
    this.productLevel3InventoryMap.put(ProductLevel3ConverterBeanTest.ITEM_SKU_1,
        this.productLevel3Inventory1);
    this.productLevel3InventoryMap.put(ProductLevel3ConverterBeanTest.ITEM_SKU_2,
        this.productLevel3Inventory2);
    this.itemSummaryResponse1 = new ItemSummaryResponse();
    this.itemSummaryResponse1.setItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU_1);
    this.itemSummaryResponse2 = new ItemSummaryResponse();
    this.itemSummaryResponse2.setItemSku(ProductLevel3ConverterBeanTest.ITEM_SKU_2);
    this.itemSummaryResponseList = new ArrayList<>();
    this.itemSummaryResponseList.add(this.itemSummaryResponse1);
    this.itemSummaryResponseList.add(this.itemSummaryResponse2);
    this.gdnSkus = new ArrayList<>();
    this.gdnSkus.add(ProductLevel3ConverterBeanTest.ITEM_SKU_1);
    this.gdnSkus.add(ProductLevel3ConverterBeanTest.ITEM_SKU_2);
    this.productLevel3ItemSearch = new ProductLevel3ItemSearch();
    this.productLevel3ItemSearch
        .setBusinessPartnerCode(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE);
    this.productLevel3ItemSearch.setItemNameKeyword(ProductLevel3ConverterBeanTest.ITEM_NAME);
    this.productLevel3ItemSearch.setItemSkuKeyword(ProductLevel3ConverterBeanTest.ITEM_SKU);
    this.productLevel3ItemSearch.setBuyable(ProductLevel3ConverterBeanTest.BUYABLE);
    this.productLevel3ItemSearch.setIsArchived(ProductLevel3ConverterBeanTest.IS_ARCHIVED);
    this.productLevel3ItemSearch
        .setIncludeAllTradingProduct(ProductLevel3ConverterBeanTest.INCLUDE_ALL_TRADING_PRODUCT);
  }

  @AfterEach
  public void _finalize() {}

  @Test
  public void testConvertInventoryDetailInfoResponseDtoToProductLevel3Inventory() {
    this.warehouseInventoryResponseDTOList.add(null);
    ProductLevel3Inventory result =
        this.productLevel3ConverterBean
            .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(this.inventoryDetailInfoResponseV2DTO);
    assertNotNull(result);
    assertThat(result.getWebItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_SKU));
    assertThat(result.getWebMerchantCode(),
        equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWebAvailable(), equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
        - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebMinAlert(), equalTo(ProductLevel3ConverterBeanTest.MIN_STOCK));
    assertTrue(result.isWebSyncStock());
    assertThat(result.getWarehouseItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_CODE));
    assertThat(result.getWarehouseMerchantCode(),
        equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWarehouseAvailable(),
        equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWarehouseReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
        - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
  }

  @Test
  public void testConvertInventoryDetailInfoResponseV2DtoToProductLevel3Inventory() {
    this.warehouseInventoryResponseDTOList.add(null);
    ProductLevel3Inventory result =
        this.productLevel3ConverterBean
            .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(this.inventoryDetailInfoResponseDTO);
    assertNotNull(result);
    assertThat(result.getWebItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_SKU));
    assertThat(result.getWebMerchantCode(),
        equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWebAvailable(), equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
        - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebMinAlert(), equalTo(ProductLevel3ConverterBeanTest.MIN_STOCK));
    assertTrue(result.isWebSyncStock());
    assertThat(result.getWarehouseItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_CODE));
    assertThat(result.getWarehouseMerchantCode(),
        equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWarehouseAvailable(),
        equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWarehouseReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
        - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
  }

  @Test
  public void testConvertInventoryDetailInfoResponseDtoToProductLevel3Inventory_withNonDistribution() {
    this.warehouseInventoryResponseDTOList.add(null);

    WarehouseInventoryResponseDTO nonDistributionWarehouse = new WarehouseInventoryResponseDTO();
    nonDistributionWarehouse.setAvailableStock(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK);
    nonDistributionWarehouse.setOriginalStock(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK);

    this.inventoryDetailInfoResponseV2DTO
      .setNonDistributionWarehouseInventoryResponseList(Arrays.asList(nonDistributionWarehouse, null));

    ProductLevel3Inventory result =
      this.productLevel3ConverterBean
        .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(this.inventoryDetailInfoResponseV2DTO);

    assertNotNull(result);
    assertThat(result.getWebItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_SKU));
    assertThat(result.getWebMerchantCode(),
      equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWebAvailable(), equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
      - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWebMinAlert(), equalTo(ProductLevel3ConverterBeanTest.MIN_STOCK));
    assertTrue(result.isWebSyncStock());
    assertThat(result.getWarehouseItemSku(), equalTo(ProductLevel3ConverterBeanTest.ITEM_CODE));
    assertThat(result.getWarehouseMerchantCode(),
      equalTo(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE));
    assertThat(result.getWarehouseAvailable(),
      equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getWarehouseReserved(), equalTo(ProductLevel3ConverterBeanTest.ORIGINAL_STOCK
      - ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));

    assertThat(result.getNonDistributionAvailable(), equalTo(ProductLevel3ConverterBeanTest.AVAILABLE_STOCK));
    assertThat(result.getNonDistributionReserved(), equalTo(ORIGINAL_STOCK - AVAILABLE_STOCK));
  }

  @Test
  public void testConvertInventoryDetailInfoResponseDtoToProductLevel3Inventory_sourceNull() {
    this.inventoryDetailInfoResponseV2DTO = null;
    ProductLevel3Inventory result =
        this.productLevel3ConverterBean
            .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(this.inventoryDetailInfoResponseV2DTO);
    assertNull(result);
  }

  @Test
  public void testConvertInventoryDetailInfoResponseDtoToProductLevel3Inventory_webInvNull_warehouseInvsNull() {
    this.inventoryDetailInfoResponseV2DTO.setWebInventoryResponse(null);
    this.inventoryDetailInfoResponseV2DTO.setWarehouseInventoryResponseList(null);
    ProductLevel3Inventory result =
        this.productLevel3ConverterBean
            .convertInventoryDetailInfoResponseDtoToProductLevel3Inventory(this.inventoryDetailInfoResponseV2DTO);
    assertNotNull(result);
    assertNull(result.getWebItemSku());
    assertNull(result.getWebMerchantCode());
    assertNull(result.getWebAvailable());
    assertNull(result.getWebReserved());
    assertNull(result.getWebMinAlert());
    assertFalse(result.isWebSyncStock());
    assertNull(result.getWarehouseItemSku());
    assertNull(result.getWarehouseMerchantCode());
    assertNull(result.getWarehouseAvailable());
    assertNull(result.getWarehouseReserved());
  }

  @Test
  public void testConvertItemSummaryResponseToListOfGdnSku() {
    List<String> result =
        this.productLevel3ConverterBean
            .convertItemSummaryResponseToListOfGdnSku(this.itemSummaryResponseList);
    assertNotNull(result);
    assertThat(result.size(), equalTo(2));
    assertThat(result, equalTo(this.gdnSkus));
  }

  @Test
  public void testConvertItemSummaryResponseToListOfGdnSku_sourceNull() {
    this.itemSummaryResponseList = null;
    List<String> result =
        this.productLevel3ConverterBean
            .convertItemSummaryResponseToListOfGdnSku(this.itemSummaryResponseList);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void convertItemSummaryResponseToItemSkuAndPickupCodeMapEmptyList() {
    productLevel3ConverterBean.convertItemSummaryResponseToItemSkuAndPickupCodeMap(new ArrayList<>());
  }

  @Test
  public void convertItemSummaryResponseToItemSkuAndPickupCodeMap() {
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    itemSummaryResponse.setPickupPointCode(PICKUP_POINT_CODE);
    productLevel3ConverterBean.convertItemSummaryResponseToItemSkuAndPickupCodeMap(Collections
        .singletonList(itemSummaryResponse));
  }

  @Test
  public void testConvertProductLevel3InventoryToListOfGdnSku() {
    List<String> result =
        this.productLevel3ConverterBean
            .convertProductLevel3InventoryToListOfGdnSku(this.productLevel3InventoryList);
    assertNotNull(result);
    assertThat(result.size(), equalTo(2));
    assertThat(result, equalTo(this.gdnSkus));
  }

  @Test
  public void testConvertProductLevel3InventoryToListOfGdnSku_sourceNull() {
    this.productLevel3InventoryList = null;
    List<String> result =
        this.productLevel3ConverterBean
            .convertProductLevel3InventoryToListOfGdnSku(this.productLevel3InventoryList);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testConvertProductLevel3InventoryToMapOfGdnSku() {
    Map<String, ProductLevel3Inventory> result =
        this.productLevel3ConverterBean
            .convertProductLevel3InventoryToMapOfGdnSku(this.productLevel3InventoryList);
    assertNotNull(result);
    assertThat(result.size(), equalTo(2));
    assertThat(result, equalTo(this.productLevel3InventoryMap));
  }

  @Test
  public void testConvertProductLevel3InventoryToMapOfGdnSku_sourceNull() {
    this.productLevel3InventoryList = null;
    Map<String, ProductLevel3Inventory> result =
        this.productLevel3ConverterBean
            .convertProductLevel3InventoryToMapOfGdnSku(this.productLevel3InventoryList);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void testConvertProductLevel3InventoryToMapOfGdnSkuWithFilter() {
    this.productLevel3InventoryMap.remove(ProductLevel3ConverterBeanTest.ITEM_SKU_2);
    Map<String, ProductLevel3Inventory> result =
        this.productLevel3ConverterBean.convertProductLevel3InventoryToMapOfGdnSku(
            this.productLevel3InventoryList, Arrays.asList(ProductLevel3ConverterBeanTest.ITEM_SKU_1));
    assertNotNull(result);
    assertThat(result.size(), equalTo(1));
    assertThat(result, equalTo(this.productLevel3InventoryMap));
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setGdnSku(ITEM_SKU);
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(summaryRequest.getItemSkus()));
    Assertions.assertEquals(ITEM_SKU, summaryRequest.getItemSkus().get(0));
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest_withExcludedItemSkus() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setGdnSku(ITEM_SKU);
    summaryFilter.setExcludedItemSkus(Arrays.asList(ITEM_SKU, ITEM_SKU_1, ITEM_SKU_2));
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(summaryRequest.getItemSkus()));
    Assertions.assertEquals(ITEM_SKU, summaryRequest.getItemSkus().get(0));
    Assertions.assertEquals(Arrays.asList(ITEM_SKU, ITEM_SKU_1, ITEM_SKU_2), summaryRequest.getExcludedItemSkus());
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest_withItemSKUs() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setItemSkus(Arrays.asList(ITEM_SKU));
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(summaryRequest.getItemSkus()));
    Assertions.assertEquals(ITEM_SKU, summaryRequest.getItemSkus().get(0));
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest_withProductSKUs() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setProductSkuList(Arrays.asList(PRODUCT_SKU));
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
        .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);
    Assertions.assertEquals(PRODUCT_SKU, summaryRequest.getProductSkus().get(0));
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest_withExcludeMerchant() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setLinkedPartnerCode(BUSINESS_PARTNER_CODE);

    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, summaryRequest.getLinkedPartnerCode());
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequest_withCategoryCodesAndIndividual() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setGdnSku(ITEM_SKU);
    summaryFilter.setItemSkus(Arrays.asList(ITEM_SKU_1, ITEM_SKU_2));
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
      .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);

    Assertions.assertTrue(CollectionUtils.isNotEmpty(summaryRequest.getItemSkus()));
    Assertions.assertTrue(summaryRequest.getItemSkus().contains(ITEM_SKU));
    Assertions.assertTrue(summaryRequest.getItemSkus().contains(ITEM_SKU_1));
    Assertions.assertTrue(summaryRequest.getItemSkus().contains(ITEM_SKU_2));
  }

  @Test
  public void testConvertItemPricesToProductLevel3Prices() {
    List<PriceDTO> prices = new ArrayList<>();
    PriceDTO priceWithDiscount = new PriceDTO();
    List<DiscountPriceDTO> discounts = new ArrayList<>();
    discounts.add(new DiscountPriceDTO());
    priceWithDiscount.setListOfDiscountPrices(discounts);
    prices.add(new PriceDTO());
    prices.add(priceWithDiscount);
    this.productLevel3ConverterBean.convertItemPricesToProductLevel3Prices(prices);
  }

  @Test
  public void testConvertItemViewConfigsToProductLevel3ViewConfigs() {
    List<ItemViewConfigDTO> viewConfigs = new ArrayList<>();
    viewConfigs.add(new ItemViewConfigDTO());
    this.productLevel3ConverterBean.convertItemViewConfigsToProductLevel3ViewConfigs(viewConfigs);
  }

  @Test
  public void testConvertMasterDataItemImagesToProductLevel3Images() {
    List<MasterDataItemImageDTO> images = new ArrayList<>();
    images.add(new MasterDataItemImageDTO());
    this.productLevel3ConverterBean.convertMasterDataItemImagesToProductLevel3Images(images);
  }

  @Test
  public void testConvertProductLevel3ItemSearchToItemSummaryRequest_sourceNull() {
    ItemSummaryRequest result =
        this.productLevel3ConverterBean.convertProductLevel3ItemSearchToItemSummaryRequest(null);
    assertNotNull(result);
    assertNull(result.getMerchantCode());
    assertNull(result.getProductItemName());
    assertNull(result.getItemSkuKeyword());
    assertNull(result.getBuyable());
    assertNull(result.getArchived());
    assertNull(result.getIsTradingProduct());
  }

  @Test
  public void testConvertProductLevel3ItemSearchToItemSummaryRequest_includeAllTradingProductFalse() {
    ItemSummaryRequest result = this.productLevel3ConverterBean
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    assertNotNull(result);
    assertEquals(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE, result.getMerchantCode());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_NAME, result.getProductItemName());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_SKU, result.getItemSkuKeyword());
    assertEquals(ProductLevel3ConverterBeanTest.BUYABLE, result.getBuyable());
    assertEquals(ProductLevel3ConverterBeanTest.IS_ARCHIVED, result.getArchived());
    assertNull(result.getIsTradingProduct());
  }

  @Test
  public void testConvertProductLevel3ItemSearchToItemSummaryRequest_includeAllTradingProductTrue() {
    this.productLevel3ItemSearch.setIncludeAllTradingProduct(true);
    ItemSummaryRequest result = this.productLevel3ConverterBean
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    assertNotNull(result);
    assertNull(result.getMerchantCode());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_NAME, result.getProductItemName());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_SKU, result.getItemSkuKeyword());
    assertEquals(ProductLevel3ConverterBeanTest.BUYABLE, result.getBuyable());
    assertEquals(ProductLevel3ConverterBeanTest.IS_ARCHIVED, result.getArchived());
    assertEquals(Boolean.TRUE, result.getIsTradingProduct());
  }

  @Test
  public void testConvertItemSummaryResponseToProductLevel3Item() {
    ProductLevel3Item result = this.productLevel3ConverterBean
        .convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse);
    assertNotNull(result);
    assertEquals(ProductLevel3ConverterBeanTest.BUSINESS_PARTNER_CODE,
        result.getBusinessPartnerCode());
    assertEquals(ProductLevel3ConverterBeanTest.PRODUCT_SKU, result.getProductSku());
    assertEquals(ProductLevel3ConverterBeanTest.PRODUCT_CODE, result.getProductCode());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_SKU, result.getItemSku());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_CODE, result.getItemCode());
    assertEquals(ProductLevel3ConverterBeanTest.MERCHANT_SKU, result.getMerchantSku());
    assertEquals(ProductLevel3ConverterBeanTest.ITEM_NAME, result.getItemName());
  }

  @Test
  public void testConvertProductLevel3SummaryFilterRequestToItemSummaryRequestWithListArguments() {
    ProductLevel3SummaryFilter summaryFilter = new ProductLevel3SummaryFilter();
    summaryFilter.setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    summaryFilter.setPickupPointCodes(Arrays.asList(PICKUP_POINT_CODE));
    ItemSummaryRequest summaryRequest = this.productLevel3ConverterBean
        .convertProductLevel3SummaryFilterRequestToItemSummaryRequest(summaryFilter);
    Assertions.assertEquals(PICKUP_POINT_CODE, summaryRequest.getPickupPointCodes().get(0));
    Assertions.assertEquals(CATEGORY_CODE, summaryRequest.getCategoryCodes().get(0));
  }

  @Test
  public void convertLogisticDetailsToItemLevel3Logistics() throws Exception {
    List<GetSkuLogisticProductResponse> getSkuLogisticProductResponseList = new ArrayList<>();
    getSkuLogisticProductResponseList.add(new GetSkuLogisticProductResponse());
    List<ProductLevel3Logistics> productLevel3LogisticsList = this.productLevel3ConverterBean
        .convertLogisticDetailsToItemLevel3Logistics(getSkuLogisticProductResponseList);
    assertEquals(productLevel3LogisticsList.size(), 1);
  }

  @Test
  public void convertProductLevel3SummaryDetailsRequestToItemSummaryRequestTest() {
    ProductLevel3SummaryFilterDetails filterRequest = new ProductLevel3SummaryFilterDetails();
    filterRequest.setProductSku(PRODUCT_SKU);
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    filterRequest.setItemSku(ITEM_SKU);
    ItemsSummaryDetailRequest itemFilterRequest =
        productLevel3ConverterBean.convertProductLevel3SummaryDetailsRequestToItemSummaryRequest(filterRequest);
    Assertions.assertEquals(PRODUCT_SKU, itemFilterRequest.getProductSku());
    Assertions.assertEquals(ITEM_SKU, itemFilterRequest.getItemSku());
  }

  @Test
  public void convertProductLevel3SummaryDetailsRequestToItemSummaryRequestNoItenSkuTest() {
    ProductLevel3SummaryFilterDetails filterRequest = new ProductLevel3SummaryFilterDetails();
    filterRequest.setProductSku(PRODUCT_SKU);
    filterRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    ItemsSummaryDetailRequest itemFilterRequest =
        productLevel3ConverterBean.convertProductLevel3SummaryDetailsRequestToItemSummaryRequest(filterRequest);
    Assertions.assertEquals(PRODUCT_SKU, itemFilterRequest.getProductSku());
  }

  @Test
  public void convertMasterDataItemImagesToProductLevel3SummaryDetailsImageTest() {
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("image-location");
    masterDataItemImageDTO.setMainImage(true);
    masterDataItemImageDTO.setSequence(1);
    ProductLevel3SummaryDetailsImage productImages =
        productLevel3ConverterBean.convertMasterDataItemImagesToProductLevel3SummaryDetailsImage(masterDataItemImageDTO);
    assertEquals("image-location", productImages.getLocationPath());
    assertTrue(productImages.getMainImage());
    assertFalse(productImages.isMarkForDelete());
    assertEquals(StringUtils.EMPTY, productImages.getReviewType());
  }

  @Test
  public void convertCampaignPriceResponseListToCampaignPriceSkuResponseEmptyMapTest() {
    List<CampaignPriceResponse> campaignPriceResponseList = new ArrayList<>();
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    CampaignPriceResponse campaignPriceResponse1 = new CampaignPriceResponse();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setCampaignPrice(10000.0);
    campaignPriceSkuResponse.setLive(true);
    campaignPriceSkuResponse.setMaxAllowedPrice(10000.0);
    campaignPriceSkuResponse.setMinAllowedPrice(1000.0);
    campaignPriceSkuResponse.setRegistered(true);
    Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponse = new HashMap<>();
    itemSkuToPriceResponse.put(ITEM_SKU, campaignPriceSkuResponse);
    Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponse1 = new HashMap<>();
    itemSkuToPriceResponse1.put(ITEM_SKU_1, campaignPriceSkuResponse);
    campaignPriceResponse.setItemSkuToPriceResponse(itemSkuToPriceResponse);
    campaignPriceResponse1.setItemSkuToPriceResponse(itemSkuToPriceResponse1);
    campaignPriceResponseList.add(campaignPriceResponse);
    campaignPriceResponseList.add(campaignPriceResponse1);
    productLevel3ConverterBean
        .convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(campaignPriceResponseList);
  }

  @Test
  public void convertCampaignPriceResponseListToCampaignPriceSkuResponseMapNullTest() {
    productLevel3ConverterBean
        .convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(null);
  }

  @Test
  public void convertCampaignPriceResponseListToCampaignPriceSkuResponseMapTest() {
    List<CampaignPriceResponse> campaignPriceResponseList = new ArrayList<>();
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setItemSku(ITEM_SKU);
    campaignPriceSkuResponse.setMaxAllowedPrice(10000.0);
    campaignPriceSkuResponse.setLockPriceUpdate(true);
    campaignPriceSkuResponse.setCampaignPrice(10000.0);
    campaignPriceSkuResponse.setLive(true);
    campaignPriceSkuResponse.setRegistered(true);
    campaignPriceSkuResponse.setMinAllowedPrice(1000);
    campaignPriceResponse.setItemInfoToPriceResponse(Arrays.asList(campaignPriceSkuResponse, campaignPriceSkuResponse));
    campaignPriceResponseList.add(campaignPriceResponse);
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = productLevel3ConverterBean
        .convertCampaignPriceResponseListToCampaignPriceSkuResponseMap(campaignPriceResponseList);
    assertEquals(1, campaignPriceSkuResponseMap.size());
    assertEquals(10000.0, campaignPriceSkuResponseMap.get(ITEM_SKU).getCampaignPrice(), 0);
    assertEquals(10000.0, campaignPriceSkuResponseMap.get(ITEM_SKU).getMaxAllowedPrice(), 0);
    assertEquals(1000.0, campaignPriceSkuResponseMap.get(ITEM_SKU).getMinAllowedPrice(), 0);
    assertTrue(campaignPriceSkuResponseMap.get(ITEM_SKU).isLive());
    assertTrue(campaignPriceSkuResponseMap.get(ITEM_SKU).isRegistered());
  }

  @Test
  public void convertItemSkusToCampaignPriceRequestTest() {
    Map<String, String> itemSkuAndPickupCodeMap = new HashMap<>();
    Map<String, Double> l5AndOfferPriceMap = new HashMap<>();
    List<String> itemSku = new ArrayList<>();
    itemSku.add(ITEM_SKU);
    itemSku.add(ITEM_SKU_1);
    itemSku.add(ITEM_SKU_2);
    itemSkuAndPickupCodeMap.put(ITEM_SKU, PICKUP_POINT_CODE);
    l5AndOfferPriceMap.put(CommonUtils.toL5Id(ITEM_SKU, PICKUP_POINT_CODE), ORIGINAL_PRICE);
    CampaignPriceRequest campaignPriceRequest =
        productLevel3ConverterBean.convertItemSkusToCampaignPriceRequest(itemSku, CATEGORY_CODE, itemSkuAndPickupCodeMap, l5AndOfferPriceMap);
    assertEquals(ITEM_SKU, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getItemSku());
    assertEquals(PICKUP_POINT_CODE, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getPickUpPointCode());
    assertEquals(ORIGINAL_PRICE, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getSellingPrice(), 0);
    assertEquals(ITEM_SKU_1, campaignPriceRequest.getCampaignPriceSkuRequestList().get(1).getItemSku());
    assertEquals(ITEM_SKU_2, campaignPriceRequest.getCampaignPriceSkuRequestList().get(2).getItemSku());
    assertEquals(CATEGORY_CODE, campaignPriceRequest.getCampaignPriceSkuRequestList().get(0).getCategoryCode());
  }

  @Test
  public void convertMasterDataProductImagesToProductLevel3SummaryDetailsImageTest() {
    ProductImage masterDataItemImages = new ProductImage();
    masterDataItemImages.setLocationPath("image-location");
    masterDataItemImages.setSequence(0);
    masterDataItemImages.setMainImages(true);
    ProductLevel3SummaryDetailsImage productLevel3SummaryDetailsImage = productLevel3ConverterBean
        .convertMasterDataProductImagesToProductLevel3SummaryDetailsImage(masterDataItemImages);
    assertEquals("image-location", productLevel3SummaryDetailsImage.getLocationPath());
    assertTrue(productLevel3SummaryDetailsImage.getMainImage());
    assertFalse(productLevel3SummaryDetailsImage.isMarkForDelete());
    assertEquals(StringUtils.EMPTY, productLevel3SummaryDetailsImage.getReviewType());
  }
}
