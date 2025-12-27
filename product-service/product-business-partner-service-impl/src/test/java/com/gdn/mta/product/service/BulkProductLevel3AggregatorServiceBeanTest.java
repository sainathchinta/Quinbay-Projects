package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.BulkProductLevel3AggregatorServiceBean;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Converter;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class BulkProductLevel3AggregatorServiceBeanTest {
  @Mock
  private ProductLevel3Converter modelConverter;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;
  @InjectMocks
  private BulkProductLevel3AggregatorServiceBean bulkProductLevel3AggregatorServiceBean;

  private static final String SKU_1 = "sku-1";
  private static final String SKU_2 = "sku-2";
  private static final String BUSINESS_PARTNER_CODE = "business-Partner-Code";
  private static final Integer WAREHOUSE_AVAILABLE = 100;
  private static final Integer WAREHOUSE_RESERVED = 10;
  private static final Integer WEB_RESERVED = 100;
  private static final Integer WEB_AVAILABLE = 10;
  private static final Integer WEB_MIN_ALERT = 10;
  private static final Integer NON_DISTRIBUTED_AVAILABLE = 100;
  private static final Integer NON_DISTRIBUTED_RESERVED = 10;
  private static final boolean WEB_SYNC_STOCK = true;
  private static final boolean DISPLAY = true;
  private static final boolean BUYABLE = true;
  private static final Double PRICE = 1000.00;
  private static final double SALE_PRICE_1 = 2000.00;
  private static final String ITEM_NAME = "item-name";
  private static final String SELLER_SKU = "seller-sku";
  private static final String ITEM_CODE = "item-code";
  private static final String ITEM_SKU = "item-sku";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private List<ItemLevel5Response> productDatas = new ArrayList<>();
  private List<ProductLevel3Inventory> inventories = new ArrayList<>();
  private List<ProductLevel3Inventory> inventories2 = new ArrayList<>();
  private Map<String, ProductLevel3Inventory> inventoryDatas = new HashMap<>();
  private List<String> listOfGdnSkus = new ArrayList<>();
  private ProductLevel3SummaryFilter filterRequest1 = new ProductLevel3SummaryFilter();
  private List<String> productSkuList = new ArrayList<>();
  private ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
  private ItemLevel5Response itemLevel5Response2 = new ItemLevel5Response();

  private ProductLevel3Inventory inventory = new ProductLevel3Inventory();
  private ProductLevel3Inventory inventory2 = new ProductLevel3Inventory();
  private List<PriceResponse> prices = new ArrayList<>();
  private List<ViewConfigResponse> viewConfigs = new ArrayList<>();
  private PriceResponse priceResponse = new PriceResponse();
  private ViewConfigResponse viewConfigResponse = new ViewConfigResponse();

  private BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response = new BulkDownloadProductLevel3Response();
  private InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
    new InventoryDetailInfoRequestDTO();
  private InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO2 =
    new InventoryDetailInfoRequestDTO();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    productSkuList.add(SKU_1);
    productSkuList.add(SKU_2);
    filterRequest1.setProductSkuList(productSkuList);
    filterRequest1.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    listOfGdnSkus.add(ITEM_SKU);
    itemLevel5Response.setItemSku(ITEM_SKU);
    itemLevel5Response.setProductSku(PRODUCT_SKU);
    itemLevel5Response.setGeneratedItemName(ITEM_NAME);
    itemLevel5Response.setPickupPointCode(PICKUP_POINT_CODE);
    itemLevel5Response.setItemCode(ITEM_CODE);
    itemLevel5Response.setSellerSku(SELLER_SKU);
    itemLevel5Response.setOff2OnChannelActive(Boolean.TRUE);
    itemLevel5Response.setCncActivated(Boolean.TRUE);
    priceResponse.setSalePrice(SALE_PRICE_1);
    priceResponse.setPrice(PRICE);
    prices.add(priceResponse);
    viewConfigResponse.setDisplay(DISPLAY);
    viewConfigResponse.setBuyable(BUYABLE);
    viewConfigs.add(viewConfigResponse);
    itemLevel5Response.setPrices(prices);
    itemLevel5Response.setViewConfigs(viewConfigs);
    itemLevel5Response.setMerchantCode(MERCHANT_CODE);
    itemLevel5Response.setCncActive(true);
    productDatas.add(itemLevel5Response);
    inventory.setWarehouseAvailable(WAREHOUSE_AVAILABLE);
    inventory.setWarehouseReserved(WAREHOUSE_RESERVED);
    inventory.setWebAvailable(WEB_AVAILABLE);
    inventory.setWebReserved(WEB_RESERVED);
    inventory.setWebMinAlert(WEB_MIN_ALERT);
    inventory.setWebSyncStock(WEB_SYNC_STOCK);
    inventory.setNonDistributionAvailable(NON_DISTRIBUTED_AVAILABLE);
    inventory.setNonDistributionReserved(NON_DISTRIBUTED_RESERVED);
    inventory.setWebItemSku(ITEM_SKU);
    inventoryDatas.put(ITEM_SKU, inventory);

    inventoryDetailInfoRequestDTO.setWebMerchantCode(MERCHANT_CODE);
    inventoryDetailInfoRequestDTO.setPickupPointCode(PICKUP_POINT_CODE);
    inventoryDetailInfoRequestDTO.setWebItemSku(ITEM_SKU);

    ReflectionTestUtils.setField(bulkProductLevel3AggregatorServiceBean, "productFetchMaximumSize", "1");

    itemLevel5Response2.setItemSku(ITEM_SKU_2);
    itemLevel5Response2.setProductSku(PRODUCT_SKU);
    itemLevel5Response2.setGeneratedItemName(ITEM_NAME);
    itemLevel5Response2.setPickupPointCode(PICKUP_POINT_CODE);
    itemLevel5Response2.setItemCode(ITEM_CODE);
    itemLevel5Response2.setSellerSku(SELLER_SKU);
    itemLevel5Response2.setOff2OnChannelActive(Boolean.TRUE);
    itemLevel5Response2.setCncActivated(Boolean.TRUE);
    itemLevel5Response2.setPrices(prices);
    itemLevel5Response2.setViewConfigs(viewConfigs);
    itemLevel5Response2.setMerchantCode(MERCHANT_CODE);
    itemLevel5Response2.setCncActive(true);

    inventoryDetailInfoRequestDTO2.setWebMerchantCode(MERCHANT_CODE);
    inventoryDetailInfoRequestDTO2.setPickupPointCode(PICKUP_POINT_CODE);
    inventoryDetailInfoRequestDTO2.setWebItemSku(ITEM_SKU_2);

    inventory2.setWarehouseAvailable(WAREHOUSE_AVAILABLE);
    inventory2.setWarehouseReserved(WAREHOUSE_RESERVED);
    inventory2.setWebAvailable(WEB_AVAILABLE);
    inventory2.setWebReserved(WEB_RESERVED);
    inventory2.setWebMinAlert(WEB_MIN_ALERT);
    inventory2.setWebSyncStock(WEB_SYNC_STOCK);
    inventory2.setNonDistributionAvailable(NON_DISTRIBUTED_AVAILABLE);
    inventory2.setNonDistributionReserved(NON_DISTRIBUTED_RESERVED);
    inventory2.setWebItemSku(ITEM_SKU_2);

  }

  @Test
  public void aggregateProductLevel3SummaryByDbMppTest() throws Exception {
    inventories.add(inventory);
    Mockito.when(xProductOutbound.getL5ItemListing(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(false),
            Mockito.eq(null)))
        .thenReturn(productDatas);
    Mockito.when(productLevel3InventoryService
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Arrays.asList(inventoryDetailInfoRequestDTO))).thenReturn(inventories);

    bulkDownloadProductLevel3Response =
      bulkProductLevel3AggregatorServiceBean.aggregateProductLevel3SummaryByDb(filterRequest1, false,
          null);

    Mockito.verify(productLevel3InventoryService)
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Arrays.asList(inventoryDetailInfoRequestDTO));
    Mockito.verify(xProductOutbound).getL5ItemListing(new HashSet<>(productSkuList), null, null, false,
        null);
  }

  @Test
  public void aggregateProductLevel3SummaryByPartitionListTest() throws Exception {
    productDatas.add(itemLevel5Response2);
    inventories.add(inventory);
    inventories2.add(inventory2);
    Mockito.when(xProductOutbound.getL5ItemListing(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.eq(false),
            Mockito.eq(null)))
      .thenReturn(productDatas);
    Mockito.when(productLevel3InventoryService
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Arrays.asList(inventoryDetailInfoRequestDTO))).thenReturn(inventories);

    Mockito.when(productLevel3InventoryService
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Arrays.asList(inventoryDetailInfoRequestDTO2))).thenReturn(inventories2);

    bulkDownloadProductLevel3Response =
      bulkProductLevel3AggregatorServiceBean.aggregateProductLevel3SummaryByDb(filterRequest1, false,
          null);

    Mockito.verify(productLevel3InventoryService)
      .findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Arrays.asList(inventoryDetailInfoRequestDTO));
    Mockito.verify(xProductOutbound).getL5ItemListing(new HashSet<>(productSkuList), null, null, false,
        null);
    Assertions.assertEquals(2,
      bulkDownloadProductLevel3Response.getProductLevel3SummaryResponses().size());
  }

}
