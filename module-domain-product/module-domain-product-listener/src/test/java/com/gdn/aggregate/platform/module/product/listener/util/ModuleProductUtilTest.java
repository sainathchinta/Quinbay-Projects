package com.gdn.aggregate.platform.module.product.listener.util;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryInfoChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.WarehouseInfo;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.UpdateSearchEventWarehouseDetails;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;
import com.gdn.aggregate.platform.module.product.listener.model.util.TerminatedSellerDeletionEventModel;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignOwner;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignPriority;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat; // Using AssertJ

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import java.util.Optional;
import org.apache.kafka.clients.consumer.ConsumerRecord;

class ModuleProductUtilTest {

  public static final String TRACE_ID = "trace-123";
  public static final String PICKUP_POINT = "pickup-point";
  public static final String PRODUCT_EVENT = "PRODUCT_EVENT";
  public static final String PRODUCT_CODE = "PROD-CODE-123";
  public static final String PRODUCT_SKU = "PROD-SKU-123";
  public static final String MERCHANT_CODE = "MERCHANT-123";
  public static final String OUT_OF_STOCK = "OUT_OF_STOCK";
  public static final String ITEM_SKU = "sku";
  public static final String PICKUP_POINT_CODE = "code";
  public static final String AVAILABLE = "AVAILABLE";
  public static final String ITEM_SKU1 = "sku1";
  public static final String WEB_ITEM_SKU = "sku2";
  private ObjectMapper objectMapper = new ObjectMapper();


  // --- Constants for Test Data ---
  private static final String SKU_1 = "SKU-1";
  private static final String MC_1 = "MC-1";
  private static final String NAME_1 = "Name-1";
  private static final String URL_NAME_1 = "url-name-1";
  private static final String CODE_1 = "Code-1";
  private static final String BRAND_1 = "Brand-1";

  private static final String ITEM_PROD_SKU = "ITEM-PROD-SKU";
  private static final String LIST_PROD_SKU = "LIST-PROD-SKU";
  private static final String DERIVED_SKU_VARIANT = "DERIVED-SKU-VARIANT1";
  private static final String DERIVED_SKU = "DERIVED-SKU";
  private static final String IGNORED_ITEM_SKU = "IGNORED-ITEM-SKU";

  private static final String ITEM_MC = "ITEM-MC";
  private static final String LIST_MC = "LIST-MC";

  private static final String PROD_MASTER_SKU = "PROD-MASTER";
  private static final String MASTER_PRODUCT_NAME = "Master Product Name";
  private static final String MASTER_CODE = "MASTER-CODE";
  private static final String MASTER_BRAND = "MasterBrand";

  private static final String PROD_DERIVE_SKU = "PROD-DERIVE";
  private static final String ITEMCODE_DERIVE = "ITEMCODE-DERIVE";

  private static final String DEFAULT_ITEM_SKU = "PROD-DEFAULT-VAR";
  private static final String DEFAULT_ITEM_CODE = "ITEMCODE-DEFAULT";
  private static final String DEFAULT_PROD_SKU = "PROD-DEFAULT";
  private static final String DEFAULT_MERCHANT_CODE = "MC-DEFAULT";
  public static final String ITEM_CODE = "ITEM-CODE";
  public static final String EXPECTED = "ITEM-PROD";
  private static final String TEST_ITEM_SKU = "test-item-sku";
  private static final List<String> ELIGIBLE_CHANGE_TYPES = List.of("PRODUCT_REJECTED");

  private static final String FLASHSALE_CAMPAIGN_CODE = "FLASH_CAMPAIGN_001";
  private static final Integer FLASHSALE_SESSION_ID = 12345;
  private static final Long FLASHSALE_START_DATE = 1639900000000L;
  private static final Long FLASHSALE_END_DATE = 1639999999999L;
  private static final String ADJUSTMENT_PROMO_TYPE = "FLASH_SALE";
  private static final String NON_FLASHSALE_PROMO_TYPE = "REGULAR_PROMO";
  private static final List<String> FLASHSALE_PROMO_TYPES = List.of("FLASH_SALE", "SUPER_FLASH");

  private static final String TEST_PICKUP_POINT_CODE = "PP-001";
  private static final String TEST_PICKUP_POINT_ITEM_SKU = "ITEM-SKU-001";
  private static final String TEST_PICKUP_POINT_PRODUCT_SKU = "PROD-SKU-001";
  private static final String PICKUP_POINT_ID_KEY = "PICKUP_POINT_ID";
  private static final String ITEM_SKU_KEY = "ITEM_SKU";
  private static final String OTHER_KEY = "OTHER_KEY";

  private static final String TEST_MERCHANT_CODE = "MC-12345";
  private static final String TEST_SKU_WITH_MERCHANT = "ABC-DEF-12345-00001";
  private static final String TEST_SKU_WITHOUT_MERCHANT = "INVALID";
  private static final String TEST_SKU_SINGLE_PART = "SINGLE";
  private static final String EXPECTED_MERCHANT_FROM_SKU = "ABC-DEF";


  @Test
  void testParseToEntry_withValidRecord_shouldReturnEntry() throws JsonProcessingException {
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(SivaItem.builder().itemSku(TEST_ITEM_SKU).build())
        .build();
    String jsonPayload = objectMapper.writeValueAsString(eventModel);
    ConsumerRecord<String, String> record = new ConsumerRecord<>("topic", 0, 0L, "key", jsonPayload);

    Optional<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>> result =
        ModuleProductUtil.parseToEntry(record, objectMapper);

    assertThat(result).isPresent();
    assertThat(result.get().getKey()).isEqualTo(record);
    assertThat(result.get().getValue().getSivaItem().getItemSku()).isEqualTo(TEST_ITEM_SKU);
  }

  @Test
  void testParseToEntry_withInvalidRecord_shouldReturnEmpty() {
    ConsumerRecord<String, String> record = new ConsumerRecord<>("topic", 0, 0L, "key", "invalid-json");

    Optional<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>> result =
        ModuleProductUtil.parseToEntry(record, objectMapper);

    assertThat(result).isNotPresent();
  }

  @Test
  void testParseEventModel_withValidJson_shouldReturnModel() throws JsonProcessingException {
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(SivaItem.builder().itemSku(TEST_ITEM_SKU).build())
        .build();
    String jsonPayload = objectMapper.writeValueAsString(eventModel);
    ConsumerRecord<String, String> record = new ConsumerRecord<>("topic", 0, 0L, "key", jsonPayload);

    SivaItemCombinedUpsertEventModel result = ModuleProductUtil.parseEventModel(record, objectMapper);

    assertThat(result).isNotNull();
    assertThat(result.getSivaItem().getItemSku()).isEqualTo(TEST_ITEM_SKU);
  }

  @Test
  void testExtractItemSku_fromSivaItem_shouldReturnSku() {
    SivaItemCombinedUpsertEventModel model = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(SivaItem.builder().itemSku("siva-sku").build())
        .build();
    String sku = ModuleProductUtil.extractItemSku(model);
    assertThat(sku).isEqualTo("siva-sku");
  }

  @Test
  void testExtractItemSku_fromItem_shouldReturnSku() {
    SivaItemCombinedUpsertEventModel model = SivaItemCombinedUpsertEventModel.builder()
        .item(Item.builder().itemSku("item-sku").build())
        .build();
    String sku = ModuleProductUtil.extractItemSku(model);
    assertThat(sku).isEqualTo("item-sku");
  }

  @Test
  void testExtractItemSku_whenBothNull_shouldReturnNull() {
    SivaItemCombinedUpsertEventModel model = new SivaItemCombinedUpsertEventModel();
    String sku = ModuleProductUtil.extractItemSku(model);
    assertThat(sku).isNull();
  }

  @Test
  void testExtractItemSku_whenSivaItemPresent_shouldPrioritizeSivaItem() {
    SivaItemCombinedUpsertEventModel model = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(SivaItem.builder().itemSku("siva-sku").build())
        .item(Item.builder().itemSku("item-sku").build())
        .build();
    String sku = ModuleProductUtil.extractItemSku(model);
    assertThat(sku).isEqualTo("siva-sku");
  }

  // --- Helper Methods to create test data ---

  private SivaItem createBaseSivaItem() {
    SivaItem sivaItem = new SivaItem();
    sivaItem.setProductSku(null);
    sivaItem.setMerchantCode(null);
    sivaItem.setProductName(null);
    sivaItem.setProductUrlName(null);
    sivaItem.setProductCode(null);
    sivaItem.setBrand(null);
    return sivaItem;
  }

  private Item createBaseItem() {
    Item item = new Item();
    item.setItemSku(DEFAULT_ITEM_SKU);
    item.setItemCode(DEFAULT_ITEM_CODE);
    item.setProductSku(DEFAULT_PROD_SKU);
    item.setMerchantCode(DEFAULT_MERCHANT_CODE);
    return item;
  }

  private CompleteItemData createBaseCompleteData() {
    CompleteItemData data = new CompleteItemData();
    data.setAllItems(new ArrayList<>());
    data.setAllMasterDataProduct(new ArrayList<>());
    return data;
  }

  private MasterDataProduct createMasterDataProduct() {
    MasterDataProduct mdp = new MasterDataProduct();
    mdp.setProductCode(PROD_MASTER_SKU);
    mdp.setProductName(MASTER_PRODUCT_NAME);
    mdp.setProductCode(MASTER_CODE);
    mdp.setBrand(MASTER_BRAND);
    return mdp;
  }


  @Test
  void testSetMandatoryData_whenAllFieldsPresent_shouldNotChange() {
    SivaItem result = createBaseSivaItem();
    result.setProductSku(SKU_1);
    result.setMerchantCode(MC_1);
    result.setProductName(NAME_1);
    result.setProductUrlName(URL_NAME_1);
    result.setProductCode(CODE_1);
    result.setBrand(BRAND_1);

    Item item = createBaseItem();
    CompleteItemData data = createBaseCompleteData();

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductSku()).isEqualTo(SKU_1);
    assertThat(result.getMerchantCode()).isEqualTo(MC_1);
    assertThat(result.getProductName()).isEqualTo(NAME_1);
    assertThat(result.getProductUrlName()).isEqualTo(URL_NAME_1);
    assertThat(result.getProductCode()).isEqualTo(CODE_1);
    assertThat(result.getBrand()).isEqualTo(BRAND_1);
  }

  @Test
  void testSetMandatoryData_whenProductSkuBlank_shouldSetFromItem() {
    SivaItem result = createBaseSivaItem();
    Item item = createBaseItem();
    item.setProductSku(ITEM_PROD_SKU);
    CompleteItemData data = createBaseCompleteData();

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductSku()).isEqualTo(ITEM_PROD_SKU);
  }

  @Test
  void testSetMandatoryData_whenProductSkuBlankInItem_shouldSetFromAllItems() {
    SivaItem result = createBaseSivaItem();
    Item item = createBaseItem();
    item.setProductSku(null);
    item.setItemSku(IGNORED_ITEM_SKU);

    Item itemInList = createBaseItem();
    itemInList.setProductSku(LIST_PROD_SKU);

    CompleteItemData data = createBaseCompleteData();
    data.setAllItems(List.of(itemInList));

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductSku()).isEqualTo(LIST_PROD_SKU);
  }

  @Test
  void testSetMandatoryData_whenProductSkuBlankEverywhere_shouldDeriveFromItemSku() {
    SivaItem result = createBaseSivaItem();
    Item item = createBaseItem();
    item.setProductSku(null);
    item.setItemSku(DERIVED_SKU_VARIANT);

    CompleteItemData data = createBaseCompleteData();
    Item itemInList1 = createBaseItem();
    itemInList1.setProductSku(null);
    data.setAllItems(List.of(itemInList1));

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductSku()).isEqualTo(DERIVED_SKU);
  }

  @Test
  void testSetMandatoryData_whenMerchantCodeBlank_shouldSetFromItem() {
    SivaItem result = createBaseSivaItem();
    result.setProductSku(SKU_1); // Set necessary fields

    Item item = createBaseItem();
    item.setMerchantCode(ITEM_MC);
    CompleteItemData data = createBaseCompleteData();

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getMerchantCode()).isEqualTo(ITEM_MC);
  }

  @Test
  void testSetMandatoryData_whenMerchantCodeBlankInItem_shouldSetFromAllItems() {
    SivaItem result = createBaseSivaItem();
    result.setProductSku(SKU_1); // Set necessary fields

    Item item = createBaseItem();
    item.setMerchantCode(null);

    Item itemInList = createBaseItem();
    itemInList.setMerchantCode(LIST_MC);

    CompleteItemData data = createBaseCompleteData();
    data.setAllItems(List.of(itemInList));

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getMerchantCode()).isEqualTo(LIST_MC);
  }

  @Test
  void testSetMandatoryData_whenMasterDataBlank_shouldSetFromMasterDataProduct() {
    SivaItem result = createBaseSivaItem();
    result.setProductSku(PROD_MASTER_SKU); // MUST have productSku

    Item item = createBaseItem();
    MasterDataProduct mdp = createMasterDataProduct();
    CompleteItemData data = createBaseCompleteData();
    data.setAllMasterDataProduct(List.of(mdp));

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductName()).isEqualTo(MASTER_PRODUCT_NAME);
    assertThat(result.getProductUrlName()).isNotNull(); // Assuming derived
    assertThat(result.getProductCode()).isEqualTo(MASTER_CODE);
    assertThat(result.getBrand()).isEqualTo(MASTER_BRAND);
  }

  @Test
  void testSetMandatoryData_whenMasterDataMissing_shouldDeriveFields() {
    SivaItem result = createBaseSivaItem();
    result.setProductSku(PROD_DERIVE_SKU);
    result.setItemCode(ITEMCODE_DERIVE); // Set for productCode derivation

    Item item = createBaseItem();
    item.setItemCode(ITEMCODE_DERIVE);

    CompleteItemData data = createBaseCompleteData(); // No MasterDataProduct

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductCode()).isNull();
  }

  @Test
  void testSetMandatoryData_whenCompleteDataListsEmpty_shouldHandleGracefully() {
    SivaItem result = createBaseSivaItem();
    Item item = createBaseItem();
    item.setProductSku(ITEM_PROD_SKU);
    item.setMerchantCode(ITEM_MC);
    item.setItemSku(ITEM_PROD_SKU + "-VAR");
    item.setItemCode(ITEM_CODE);

    CompleteItemData data = createBaseCompleteData(); // Empty lists

    ModuleProductUtil.setMandatoryDataInSivaItem(result, item, data);

    assertThat(result.getProductSku()).isEqualTo(ITEM_PROD_SKU);
    assertThat(result.getMerchantCode()).isEqualTo(ITEM_MC);
  }


  @Test
  void testIsProductRejected_whenDeleteProductDataOnTerminationTrueAndChangeTypesContainsProductRejected_shouldReturnTrue() {
    List<String> changeTypes = List.of("ARCHIVE_FLAG_CHANGE", "PRODUCT_REJECTED");
    boolean deleteProductDataOnTermination = true;
    boolean result = ModuleProductUtil.isProductRejected(changeTypes, TRACE_ID, PICKUP_POINT,
      PRODUCT_EVENT, deleteProductDataOnTermination, ELIGIBLE_CHANGE_TYPES);
    assertThat(result).isTrue();
  }

  @Test
  void testIsProductRejected_whenDeleteProductDataOnTerminationFalse_shouldReturnFalse() {
    List<String> changeTypes = List.of("PRODUCT_REJECTED");
    boolean deleteProductDataOnTermination = false;
    boolean result = ModuleProductUtil.isProductRejected(changeTypes, TRACE_ID, PICKUP_POINT,
      PRODUCT_EVENT, deleteProductDataOnTermination, ELIGIBLE_CHANGE_TYPES);
    assertThat(result).isFalse();
  }

  @Test
  void testIsProductRejected_whenChangeTypesDoesNotContainProductRejected_shouldReturnFalse() {
    List<String> changeTypes = List.of("ARCHIVE_FLAG_CHANGE");
    boolean deleteProductDataOnTermination = true;
    boolean result = ModuleProductUtil.isProductRejected(changeTypes, TRACE_ID, PICKUP_POINT,
      PRODUCT_EVENT, deleteProductDataOnTermination, ELIGIBLE_CHANGE_TYPES);
    assertThat(result).isFalse();
  }

  @Test
  void testIsProductRejected_whenChangeTypesIsNull_shouldReturnFalse() {
    List<String> changeTypes = null;
    boolean deleteProductDataOnTermination = true;
    boolean result = ModuleProductUtil.isProductRejected(changeTypes, TRACE_ID, PICKUP_POINT,
      PRODUCT_EVENT, deleteProductDataOnTermination, ELIGIBLE_CHANGE_TYPES);
    assertThat(result).isFalse();
  }


  @Test
  void testToTerminatedSellerDeletionEventModel_withNullProduct_shouldReturnNull() {
    TerminatedSellerDeletionEventModel result =
      ModuleProductUtil.toTerminatedSellerDeletionEventModel(null);
    assertThat(result).isNull();
  }

  @Test
  void testToTerminatedSellerDeletionEventModel_withValidProduct_shouldMapAllFields() {
    Product product = Product.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU)
      .merchantCode(MERCHANT_CODE).sharedProduct(true).build();

    TerminatedSellerDeletionEventModel result =
      ModuleProductUtil.toTerminatedSellerDeletionEventModel(product);

    assertThat(result).isNotNull();
    assertThat(result.getProductCode()).isEqualTo(PRODUCT_CODE);
    assertThat(result.getProductSku()).isEqualTo(PRODUCT_SKU);
    assertThat(result.getSellerCode()).isEqualTo(MERCHANT_CODE);
    assertThat(result.isSharedProduct()).isTrue();
  }

  @Test
  void testToTerminatedSellerDeletionEventModel_withNullFields_shouldMapNullsCorrectly() {
    Product product =
      Product.builder().productCode(null).productSku(null).merchantCode(null).sharedProduct(false)
        .build();

    TerminatedSellerDeletionEventModel result =
      ModuleProductUtil.toTerminatedSellerDeletionEventModel(product);

    assertThat(result).isNotNull();
    assertThat(result.getProductCode()).isNull();
    assertThat(result.getProductSku()).isNull();
    assertThat(result.getSellerCode()).isNull();
    assertThat(result.isSharedProduct()).isFalse();
  }

  @Test
  void testPartitionConsumerRecords_withRecordsLessThanSize_shouldReturnSinglePartition() {
    int size = 10;
    List<ConsumerRecord<String, String>> records = createConsumerRecords(7);

    List<List<ConsumerRecord<String, String>>> result =
      ModuleProductUtil.partitionConsumerRecords(records, size);

    assertThat(result).hasSize(1);
    assertThat(result.get(0)).hasSize(7);
    for (int i = 0; i < 7; i++) {
      assertThat(result.get(0).get(i)).isEqualTo(records.get(i));
    }
  }

  @Test
  void testPartitionConsumerRecords_withRecordsGreaterThanSize_shouldReturnMultiplePartitions() {
    int size = 3;
    List<ConsumerRecord<String, String>> records = createConsumerRecords(10);
    List<List<ConsumerRecord<String, String>>> result =
      ModuleProductUtil.partitionConsumerRecords(records, size);
    assertThat(result).hasSize(4);
    assertThat(result.get(0)).hasSize(3);
    assertThat(result.get(1)).hasSize(3);
    assertThat(result.get(2)).hasSize(3);
    assertThat(result.get(3)).hasSize(1);

    int index = 0;
    for (List<ConsumerRecord<String, String>> partition : result) {
      for (ConsumerRecord<String, String> record : partition) {
        assertThat(record).isEqualTo(records.get(index));
        index++;
      }
    }
  }


  @Test
  void testPartitionConsumerRecords_withVeryLargeSize_shouldReturnSinglePartition() {
    int size = 1000;
    List<ConsumerRecord<String, String>> records = createConsumerRecords(50);

    List<List<ConsumerRecord<String, String>>> result =
        ModuleProductUtil.partitionConsumerRecords(records, size);

    assertThat(result).hasSize(1);
    assertThat(result.get(0)).hasSize(50);
    assertThat(result.get(0)).isEqualTo(records);
  }


  // Helper method to create test ConsumerRecord objects
  private List<ConsumerRecord<String, String>> createConsumerRecords(int count) {
    List<ConsumerRecord<String, String>> records = new ArrayList<>();
    for (int i = 0; i < count; i++) {
      records.add(
        new ConsumerRecord<>("test-topic", 0, (long) i, "test-key-" + i, "test-value-" + i));
    }
    return records;
  }

  @Test
  void testToUpdatedStockFromStockUpdateSearchEvent_whenNullIdsPresent_shouldReturnEmptyStock() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(null)
        .itemSku(ITEM_SKU)
        .build();
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setWebItemSku(ITEM_SKU);
    event.setPickupPointCode(PICKUP_POINT_CODE);
    Stock result =
      ModuleProductUtil.toUpdatedStockFromStockUpdateSearchEvent(pickupPoint.getItemSku(),
        pickupPoint.getPickupPointCode(), event, null);
    assertThat(result).isNotNull();
  }

  @Test
  void testToUpdatedStockFromStockUpdateSearchEvent_whenSkuMismatch_shouldReturnNull() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(PICKUP_POINT_CODE)
        .itemSku(ITEM_SKU1)
        .build();
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setWebItemSku(WEB_ITEM_SKU);
    event.setPickupPointCode(PICKUP_POINT_CODE);
    Stock result =
      ModuleProductUtil.toUpdatedStockFromStockUpdateSearchEvent(pickupPoint.getItemSku(),
        pickupPoint.getPickupPointCode(), event, null);
    assertThat(result).isNotNull();
  }

  @Test
  void testToUpdatedStockFromStockUpdateSearchEvent_whenValidMatch_shouldReturnStock() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(PICKUP_POINT_CODE)
        .itemSku(ITEM_SKU)
        .build();
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setWebItemSku(ITEM_SKU);
    event.setPickupPointCode(PICKUP_POINT_CODE);
    event.setSyncStock(false);
    event.setOutOfStock(false);
    Stock result =
      ModuleProductUtil.toUpdatedStockFromStockUpdateSearchEvent(pickupPoint.getItemSku(),
        pickupPoint.getPickupPointCode(), event, null);
    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
    assertThat(result.isExists()).isTrue();
  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_shouldReturnEmptyStock() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setItemSku(ITEM_SKU);
    pickupPointInventory.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointInventory.setInStock(true);
    pickupPointInventory.setSyncStock(true);
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(PICKUP_POINT_CODE)
        .itemSku(ITEM_SKU)
        .build();
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(new ArrayList<>());
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, pickupPointInventory);
    assertThat(result).isNotNull();
  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_shouldOOS() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setItemSku(ITEM_SKU);
    pickupPointInventory.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointInventory.setInStock(false);
    pickupPointInventory.setSyncStock(false);
    PickupPoint pickupPoint = PickupPoint.builder()
      .pickupPointCode(PICKUP_POINT_CODE)
      .itemSku(ITEM_SKU)
      .build();
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(new ArrayList<>());
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, pickupPointInventory);
    assertThat(result).isNotNull();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);

  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_NullData() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setItemSku(ITEM_SKU);
    pickupPointInventory.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointInventory.setInStock(false);
    pickupPointInventory.setSyncStock(false);
    PickupPoint pickupPoint = PickupPoint.builder()
      .pickupPointCode(null)
      .itemSku(ITEM_SKU)
      .build();
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(new ArrayList<>());
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, pickupPointInventory);
    assertThat(result).isNull();
  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_NullDataItem() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setItemSku(ITEM_SKU);
    pickupPointInventory.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointInventory.setInStock(false);
    pickupPointInventory.setSyncStock(false);
    PickupPoint pickupPoint = PickupPoint.builder()
      .pickupPointCode(PICKUP_POINT)
      .itemSku(null)
      .build();
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(new ArrayList<>());
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, pickupPointInventory);
    assertThat(result).isNull();
  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_WithItem() {
    PickupPoint pickupPoint = PickupPoint.builder()
      .pickupPointCode(PICKUP_POINT)
      .itemSku(ITEM_SKU)
      .build();
    List<SivaItem> sivaItem = Collections.singletonList(
      SivaItem.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT).inventory(
        Stock.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT).exists(true).build()).build());
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(sivaItem);
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, null);
    assertThat(result).isNotNull();
  }

  @Test
  void testGetCheapestStock_whenNoMatchingItems_shouldReturnNullStock() {
    PickupPoint pickupPoint = PickupPoint.builder()
      .pickupPointCode(PICKUP_POINT_CODE)
      .itemSku(ITEM_SKU)
      .build();
    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(new ArrayList<>());
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, null);
    assertThat(result).isNull();
  }


  @Test
  void testGetCheapestStock_whenMatchingItemFound_shouldReturnStock() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(PICKUP_POINT_CODE)
        .itemSku(ITEM_SKU)
        .build();
    PickupPointInventory existingPickupPointInventory = new PickupPointInventory();
    existingPickupPointInventory.setItemSku(ITEM_SKU);
    existingPickupPointInventory.setPickupPointCode(PICKUP_POINT_CODE);
    existingPickupPointInventory.setInStock(true);
    existingPickupPointInventory.setSyncStock(true);
    SivaItem sivaItem = new SivaItem();
    sivaItem.setItemSku(ITEM_SKU);
    sivaItem.setPickupPointCode(PICKUP_POINT_CODE);
    Stock inventory = new Stock();
    inventory.setStatus(AVAILABLE);
    sivaItem.setInventory(inventory);

    CompleteItemData completeItemData = new CompleteItemData();
    completeItemData.setAllSivaItems(List.of(sivaItem));
    Stock result =
      ModuleProductUtil.getCheapestStock(pickupPoint.getItemSku(), pickupPoint.getPickupPointCode(),
        completeItemData, existingPickupPointInventory);
    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
  }

  @Test
  void testToPickupPointTypeFromStockSearch_whenWarehouseCodesPresent_shouldReturnWarehouseType() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(List.of("code1"));
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.toPickupPointTypeFromStockSearch(event);
    assertThat(result).isEqualTo("ONLINE_WAREHOUSE");
  }

  @Test
  void testToPickupPointTypeFromStockSearch_whenWarehouseCodesPresent() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setOosWarehouseCodes(List.of("code1"));
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.toPickupPointTypeFromStockSearch(event);
    assertThat(result).isEqualTo("ONLINE_WAREHOUSE");
  }

  @Test
  void testToPickupPointTypeFromStockSearch_whenNoWarehouseCodes_shouldReturnMerchantType() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(new ArrayList<>());
    details.setOosWarehouseCodes(new ArrayList<>());
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.toPickupPointTypeFromStockSearch(event);
    assertThat(result).isEqualTo("ONLINE_MERCHANT");
  }

  @Test
  void testConvertStockStatusByAvailability_whenEventNull_shouldReturnOOS() {
    String result = ModuleProductUtil.convertStockStatusByAvailability(null);
    assertThat(result).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testConvertStockStatusByAvailability_whenNotSyncStock_shouldCheckOutOfStock() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(false);
    event.setOutOfStock(false);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(AVAILABLE);
  }

  @Test
  void testConvertStockStatusByAvailability_whenSyncStockWithNonOosWarehouses_shouldReturnAvailable() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(true);
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(List.of("code1"));
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(AVAILABLE);
  }

  @Test
  void testConvertStockStatusByAvailability_whenSyncStockWithNoNonOosWarehouses_shouldReturnOOS() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(true);
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(new ArrayList<>());
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testConvertStockStatusByAvailability_whenSyncStockWithNoNonOosWarehousesSyncStockFalse() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(false);
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(new ArrayList<>());
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(AVAILABLE);
  }

  @Test
  void testConvertStockStatusByAvailability_whenSyncStockWithOOS() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(false);
    event.setOutOfStock(true);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testConvertStockStatusByAvailability_whenWarehouseDetailsNull_shouldReturnOOS() {
    // Given
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(true);
    event.setWarehouseDetails(null);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testConvertStockStatusByAvailability_whenNonOosWarehouseCodesNull_shouldReturnOOS() {
    StockUpdateSearchEvent event = new StockUpdateSearchEvent();
    event.setSyncStock(true);
    UpdateSearchEventWarehouseDetails details = new UpdateSearchEventWarehouseDetails();
    details.setNonOosWarehouseCodes(null);
    event.setWarehouseDetails(details);
    String result = ModuleProductUtil.convertStockStatusByAvailability(event);
    assertThat(result).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenNullIdsPresent_shouldReturnEmptyStock() {
    // Test with null itemSku
    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(null, PICKUP_POINT_CODE,
      Level2InventoryQuantityChangedEvent.builder().level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).build(), null);
    assertThat(result).isNotNull();
    assertThat(result.isExists()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);

    // Test with null pickupPointCode
    result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, null,
      Level2InventoryQuantityChangedEvent.builder().level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).build(), null);
    assertThat(result).isNotNull();
    assertThat(result.isExists()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);

    // Test with null event level2Id
    result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE,
      Level2InventoryQuantityChangedEvent.builder().level2Id(null)
        .pickupPointCode(PICKUP_POINT_CODE).build(), null);
    assertThat(result).isNotNull();
    assertThat(result.isExists()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);

    // Test with null event pickupPointCode
    result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE,
      Level2InventoryQuantityChangedEvent.builder().level2Id(ITEM_SKU).pickupPointCode(null)
        .build(), null);
    assertThat(result).isNotNull();
    assertThat(result.isExists()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenSkuMismatch_shouldReturnEmptyStock() {
    Level2InventoryQuantityChangedEvent event =
      Level2InventoryQuantityChangedEvent.builder().level2Id(ITEM_CODE)
        .pickupPointCode(PICKUP_POINT_CODE).build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event , null);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenSyncStock() {
    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
      .level2Id(ITEM_SKU).warehouseInfos(Collections.emptyList()).syncStock(true)
      .pickupPointCode(PICKUP_POINT_CODE)
      .build();
    PickupPoint p =
      PickupPoint.builder().pickupPointCode(PICKUP_POINT).itemSku(ITEM_SKU).inStock(false).build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, p);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenWebStock_shouldHandleCorrectly() {
    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(false)
        .availableStock(10)
        .build();
    PickupPoint p =
      PickupPoint.builder().pickupPointCode(PICKUP_POINT).itemSku(ITEM_SKU).inStock(true).build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, p);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
    assertThat(result.isWarehouse()).isFalse();
  }


  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenWarehouseStock_shouldHandleCorrectly() {
    List<WarehouseInfo> warehouseInfos = List.of(
        WarehouseInfo.builder().availableStock(5).build(),
        WarehouseInfo.builder().availableStock(0).build()
    );

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .warehouseInfos(warehouseInfos)
        .build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, null);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
    assertThat(result.isWarehouse()).isTrue();
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenWarehouseStock_AllZeroStock() {
    List<WarehouseInfo> warehouseInfos = List.of(
      WarehouseInfo.builder().availableStock(0).build(),
      WarehouseInfo.builder().availableStock(0).build()
    );

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
      .level2Id(ITEM_SKU)
      .pickupPointCode(PICKUP_POINT_CODE)
      .syncStock(true)
      .warehouseInfos(warehouseInfos)
      .build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, null);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.isWarehouse()).isTrue();
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenWarehouseStock_AllNullStock() {
    List<WarehouseInfo> warehouseInfos = List.of(
      WarehouseInfo.builder().availableStock(null).build(),
      WarehouseInfo.builder().availableStock(null).build()
    );

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
      .level2Id(ITEM_SKU)
      .pickupPointCode(PICKUP_POINT_CODE)
      .syncStock(true)
      .warehouseInfos(warehouseInfos)
      .build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, null);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.isWarehouse()).isTrue();
  }

  @Test
  void testToUpdatedStockFromStockRepublishEvent_whenOutOfStock_shouldReturnOOS() {
    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(false)
        .availableStock(0)
        .build();

    Stock result = ModuleProductUtil.toUpdatedStockFromStockRepublishEvent(ITEM_SKU, PICKUP_POINT_CODE, event, null);

    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenNullPickupPoint_shouldNotUpdate() {
    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(null, event);
    // No assertions needed - just verifying no NPE is thrown
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenNullEvent_shouldNotUpdate() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, null);

    assertThat(pickupPoint.isInStock()).isFalse(); // Should remain unchanged
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenSkuMismatch_shouldNotUpdate() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id("different-sku")
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isFalse(); // Should remain unchanged
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenWebStock_shouldUpdateCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(false)
        .availableStock(10)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isTrue();
    assertThat(pickupPoint.isWarehouse()).isFalse();
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenWarehouseStockAvailable_shouldUpdateCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    List<WarehouseInfo> warehouseInfos = List.of(
        WarehouseInfo.builder().availableStock(5).build(),
        WarehouseInfo.builder().availableStock(0).build()
    );

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .warehouseInfos(warehouseInfos)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isTrue();
    assertThat(pickupPoint.isWarehouse()).isTrue();
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenWarehouseStockEmpty_shouldUpdateCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .warehouseInfos(Collections.emptyList())
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isFalse();
    assertThat(pickupPoint.isWarehouse()).isTrue();
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenWarehouseStockAllZero_shouldUpdateCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    List<WarehouseInfo> warehouseInfos = List.of(
        WarehouseInfo.builder().availableStock(0).build(),
        WarehouseInfo.builder().availableStock(0).build()
    );

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(true)
        .warehouseInfos(warehouseInfos)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isFalse();
    assertThat(pickupPoint.isWarehouse()).isTrue();
  }

  @Test
  void testSetStockFromLevel2InventoryQuantityChangedEvent_whenWebStockZero_shouldUpdateCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    Level2InventoryQuantityChangedEvent event = Level2InventoryQuantityChangedEvent.builder()
        .level2Id(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .syncStock(false)
        .availableStock(0)
        .build();

    ModuleProductUtil.setStockFromLevel2InventoryQuantityChangedEvent(pickupPoint, event);

    assertThat(pickupPoint.isInStock()).isFalse();
    assertThat(pickupPoint.isWarehouse()).isFalse();
  }


  @Test
  void testToPickupPointInventoryFromPickupPoint_whenOptionalIsEmpty_shouldReturnNull() {
    Optional<PickupPoint> emptyOptional = Optional.empty();
    PickupPointInventory result = ModuleProductUtil.toPickupPointInventoryFromPickupPoint(emptyOptional);
    assertThat(result).isNull();
  }

  @Test
  void testToPickupPointInventoryFromPickupPoint_whenOptionalIsPresent_shouldMapAllFields() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .id(ITEM_SKU.concat("-").concat(PICKUP_POINT))
        .pickupPointCode(PICKUP_POINT)
        .itemSku(ITEM_SKU)
        .warehouse(true)
        .inStock(true)
        .build();
    Optional<PickupPoint> optionalPickupPoint = Optional.of(pickupPoint);

    PickupPointInventory result = ModuleProductUtil.toPickupPointInventoryFromPickupPoint(optionalPickupPoint);

    assertThat(result).isNotNull();
    assertThat(result.getId()).isEqualTo(ITEM_SKU.concat("-").concat(PICKUP_POINT));
    assertThat(result.getSyncStock()).isTrue();
    assertThat(result.isInStock()).isTrue();
  }

  @Test
  void testToPickupPointInventoryFromPickupPoint_whenOptionalHasPickupPointWithNullFields_shouldMapNullFields() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .id(null)
        .pickupPointCode(null)
        .itemSku(null)
        .warehouse(false)
        .inStock(false)
        .build();
    Optional<PickupPoint> optionalPickupPoint = Optional.of(pickupPoint);

    PickupPointInventory result = ModuleProductUtil.toPickupPointInventoryFromPickupPoint(optionalPickupPoint);

    assertThat(result).isNotNull();
    assertThat(result.getId()).isNull();
    assertThat(result.getPickupPointCode()).isNull();
    assertThat(result.getItemSku()).isNull();
    assertThat(result.getSyncStock()).isFalse();
    assertThat(result.isInStock()).isFalse();
  }

  @Test
  void testToPickupPointInventoryFromPickupPoint_whenOptionalHasWarehousePickupPoint_shouldMapWarehouseCorrectly() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .id(ITEM_SKU.concat("-").concat(PICKUP_POINT))
        .pickupPointCode(PICKUP_POINT)
        .itemSku(ITEM_SKU)
        .warehouse(true)
        .inStock(false)
        .build();
    Optional<PickupPoint> optionalPickupPoint = Optional.of(pickupPoint);

    PickupPointInventory result = ModuleProductUtil.toPickupPointInventoryFromPickupPoint(optionalPickupPoint);

    assertThat(result).isNotNull();
    assertThat(result.getId()).isEqualTo(ITEM_SKU.concat("-").concat(PICKUP_POINT));
    assertThat(result.getSyncStock()).isTrue();
    assertThat(result.isInStock()).isFalse();
  }

//  @Test
//  void testToPickupPointInventoryFromPickupPoint_whenOptionalHasMerchantPickupPoint_shouldMapMerchantCorrectly() {
//    PickupPoint pickupPoint = PickupPoint.builder()
//        .id("merchant-id")
//        .pickupPointCode("MC-001")
//        .itemSku("MERCHANT-ITEM")
//        .warehouse(false)
//        .inStock(true)
//        .build();
//    Optional<PickupPoint> optionalPickupPoint = Optional.of(pickupPoint);
//
//    PickupPointInventory result = ModuleProductUtil.toPickupPointInventoryFromPickupPoint(optionalPickupPoint);
//
//    assertThat(result).isNotNull();
//    assertThat(result.getId()).isEqualTo("merchant-id");
//    assertThat(result.getPickupPointCode()).isEqualTo("MC-001");
//    assertThat(result.getItemSku()).isEqualTo("MERCHANT-ITEM");
//    assertThat(result.getSyncStock()).isFalse();
//    assertThat(result.isInStock()).isTrue();
//  }
//
//  // Test cases for buildBasicStock method
//
//  @Test
//  void testBuildBasicStock_whenPickupPointInventoryHasInStockTrue_shouldReturnAvailableStock() {
//    PickupPointInventory pickupPointInventory = new PickupPointInventory();
//    pickupPointInventory.setSyncStock(false);
//    pickupPointInventory.setInStock(true);
//
//    Stock result = ModuleProductUtil.buildBasicStock("ITEM-SKU-123", "PP-CODE-123", pickupPointInventory);
//
//    assertThat(result).isNotNull();
//    assertThat(result.getItemSku()).isEqualTo("ITEM-SKU-123");
//    assertThat(result.getPickupPointCode()).isEqualTo("PP-CODE-123");
//    assertThat(result.isWarehouse()).isFalse();
//    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
//    assertThat(result.isExists()).isTrue();
//  }
//
//  @Test
//  void testBuildBasicStock_whenPickupPointInventoryHasInStockFalse_shouldReturnOutOfStockStock() {
//    PickupPointInventory pickupPointInventory = new PickupPointInventory();
//    pickupPointInventory.setSyncStock(true);
//    pickupPointInventory.setInStock(false);
//
//    Stock result = ModuleProductUtil.buildBasicStock("ITEM-SKU-456", "PP-CODE-456", pickupPointInventory);
//
//    assertThat(result).isNotNull();
//    assertThat(result.getItemSku()).isEqualTo("ITEM-SKU-456");
//    assertThat(result.getPickupPointCode()).isEqualTo("PP-CODE-456");
//    assertThat(result.isWarehouse()).isTrue();
//    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
//    assertThat(result.isExists()).isTrue();
//  }

  @Test
  void testBuildBasicStock_whenPickupPointInventoryIsWarehouse_shouldSetWarehouseTrue() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setSyncStock(true);
    pickupPointInventory.setInStock(true);

    Stock result = ModuleProductUtil.buildBasicStock(ITEM_SKU, PICKUP_POINT, pickupPointInventory);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT);
    assertThat(result.isWarehouse()).isTrue();
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
    assertThat(result.isExists()).isTrue();
  }

  @Test
  void testBuildBasicStock_whenPickupPointInventoryIsNotWarehouse_shouldSetWarehouseFalse() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setSyncStock(false);
    pickupPointInventory.setInStock(false);

    Stock result = ModuleProductUtil.buildBasicStock(ITEM_SKU, PICKUP_POINT, pickupPointInventory);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT);
    assertThat(result.isWarehouse()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.isExists()).isTrue();
  }

  @Test
  void testBuildBasicStock_withNullItemSku_shouldAcceptNullValues() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setSyncStock(true);
    pickupPointInventory.setInStock(true);

    Stock result = ModuleProductUtil.buildBasicStock(null, PICKUP_POINT, pickupPointInventory);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isNull();
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT);
    assertThat(result.isWarehouse()).isTrue();
    assertThat(result.getStatus()).isEqualTo(AVAILABLE);
    assertThat(result.isExists()).isTrue();
  }

  @Test
  void testBuildBasicStock_withNullPickupPointCode_shouldAcceptNullValues() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setSyncStock(false);
    pickupPointInventory.setInStock(false);

    Stock result = ModuleProductUtil.buildBasicStock(ITEM_SKU, null, pickupPointInventory);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isNull();
    assertThat(result.isWarehouse()).isFalse();
    assertThat(result.getStatus()).isEqualTo(OUT_OF_STOCK);
    assertThat(result.isExists()).isTrue();
  }

  @Test
  void testBuildBasicStock_ensuresExistsIsAlwaysTrue() {
    PickupPointInventory pickupPointInventory = new PickupPointInventory();
    pickupPointInventory.setSyncStock(true);
    pickupPointInventory.setInStock(false);

    Stock result = ModuleProductUtil.buildBasicStock("TEST-ITEM", "TEST-PP", pickupPointInventory);
    assertThat(result).isNotNull();
    assertThat(result.isExists()).isTrue(); // Should always be true for this method
  }

  @Test
  void initializeStockDataTest_withPickupPointData(){
    Stock stock = ModuleProductUtil.initializeStockData(ITEM_SKU, PICKUP_POINT,
      PickupPoint.builder().pickupPointCode(PICKUP_POINT).itemSku(ITEM_SKU).inStock(true).build());
    assertThat(stock.isExists()).isTrue();

  }

  @Test
  void initializeStockDataTest_withPickupPointData_OOS(){
    Stock stock = ModuleProductUtil.initializeStockData(ITEM_SKU, PICKUP_POINT,
      PickupPoint.builder().pickupPointCode(PICKUP_POINT).itemSku(ITEM_SKU).inStock(false).build());
    assertThat(stock.isExists()).isFalse();

  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withPickupPointId_shouldSetPickupPointCode() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(TEST_PICKUP_POINT_CODE)
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, PICKUP_POINT_ID_KEY);

    assertThat(eventModel.getId()).isEqualTo("ITEM-SKU-001-PP-001");
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withItemSku_shouldSetItemSku() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(TEST_PICKUP_POINT_CODE)
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, ITEM_SKU_KEY);

    assertThat(eventModel.getId()).isEqualTo(TEST_PICKUP_POINT_ITEM_SKU);
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withOtherKey_shouldSetProductSku() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(TEST_PICKUP_POINT_CODE)
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, OTHER_KEY);

    assertThat(eventModel.getId()).isEqualTo(TEST_PICKUP_POINT_PRODUCT_SKU);
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withBlankPickupPointCode_shouldNotSetId() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode("")
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, PICKUP_POINT_ID_KEY);

    assertThat(eventModel.getId()).isEqualTo("ITEM-SKU-001-");
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withNullPickupPointCode_shouldNotSetId() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(null)
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, PICKUP_POINT_ID_KEY);

    assertThat(eventModel.getId()).isEqualTo("ITEM-SKU-001-");
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withBlankItemSku_shouldNotSetId() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(TEST_PICKUP_POINT_CODE)
        .itemSku("")
        .productSku(TEST_PICKUP_POINT_PRODUCT_SKU)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, ITEM_SKU_KEY);

    assertThat(eventModel.getId()).isNull();
  }

  @Test
  void testSetPartitionKeyForPickupPointEvent_withBlankProductSku_shouldNotSetId() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .pickupPointCode(TEST_PICKUP_POINT_CODE)
        .itemSku(TEST_PICKUP_POINT_ITEM_SKU)
        .productSku("")
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();

    ModuleProductUtil.setPartitionKeyForPickupPointEvent(eventModel, OTHER_KEY);

    assertThat(eventModel.getId()).isNull();
  }

  @Test
  void testExtractMerchantCodeFromPickupPoint_withNullPickupPoint_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCode((PickupPoint) null);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromPickupPoint_withMerchantCode_shouldReturnMerchantCode() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .merchantCode(TEST_MERCHANT_CODE)
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(pickupPoint);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromPickupPoint_withBlankMerchantCode_shouldExtractFromSku() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .merchantCode("")
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(pickupPoint);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromPickupPoint_withNullMerchantCode_shouldExtractFromSku() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .merchantCode(null)
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(pickupPoint);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromSivaProductEventModel_withNullEventModel_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCode((SivaProductCombinedUpsertEventModel) null);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSivaProductEventModel_withNullSivaProduct_shouldReturnNull() {
    SivaProductCombinedUpsertEventModel eventModel = SivaProductCombinedUpsertEventModel.builder()
        .sivaProduct(null)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSivaProductEventModel_withMerchantCode_shouldReturnMerchantCode() {
    SivaProduct sivaProduct = SivaProduct.builder()
        .merchantCode(TEST_MERCHANT_CODE)
        .productSku(TEST_SKU_WITH_MERCHANT)
        .build();
    SivaProductCombinedUpsertEventModel eventModel = SivaProductCombinedUpsertEventModel.builder()
        .sivaProduct(sivaProduct)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromSivaProductEventModel_withBlankMerchantCode_shouldExtractFromSku() {
    SivaProduct sivaProduct = SivaProduct.builder()
        .merchantCode("")
        .productSku(TEST_SKU_WITH_MERCHANT)
        .build();
    SivaProductCombinedUpsertEventModel eventModel = SivaProductCombinedUpsertEventModel.builder()
        .sivaProduct(sivaProduct)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromSivaItemEventModel_withNullEventModel_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCode((SivaItemCombinedUpsertEventModel) null);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSivaItemEventModel_withNullSivaItem_shouldReturnNull() {
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(null)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSivaItemEventModel_withMerchantCode_shouldReturnMerchantCode() {
    SivaItem sivaItem = SivaItem.builder()
        .merchantCode(TEST_MERCHANT_CODE)
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(sivaItem)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromSivaItemEventModel_withBlankMerchantCode_shouldExtractFromSku() {
    SivaItem sivaItem = SivaItem.builder()
        .merchantCode("")
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(sivaItem)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromSivaItemEventModel_withBlankItemSku_shouldReturnNull() {
    SivaItem sivaItem = SivaItem.builder()
        .merchantCode("")
        .itemSku("")
        .build();
    SivaItemCombinedUpsertEventModel eventModel = SivaItemCombinedUpsertEventModel.builder()
        .sivaItem(sivaItem)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSku_withNullSku_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCodeFromSku(null);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSku_withBlankSku_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCodeFromSku("");
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSku_withValidSku_shouldReturnMerchantCode() {
    String result = ModuleProductUtil.extractMerchantCodeFromSku(TEST_SKU_WITH_MERCHANT);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromSku_withSinglePartSku_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCodeFromSku(TEST_SKU_SINGLE_PART);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromSku_withInvalidSku_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCodeFromSku(TEST_SKU_WITHOUT_MERCHANT);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withNullEventModel_shouldReturnNull() {
    String result = ModuleProductUtil.extractMerchantCode((PickupPointUpsertCombinedEventModel) null);
    assertThat(result).isNull();
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withPickupPoint_shouldDelegateToPickupPointMethod() {
    PickupPoint pickupPoint = PickupPoint.builder()
        .merchantCode(TEST_MERCHANT_CODE)
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .pickupPoint(pickupPoint)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withInventoryInfoChange_shouldExtractFromItemSku() {
    InventoryInfoChange inventoryInfoChange = InventoryInfoChange.builder()
        .itemSku(TEST_SKU_WITH_MERCHANT)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .inventoryInfoChange(inventoryInfoChange)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(EXPECTED_MERCHANT_FROM_SKU);
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withStockUpdateSearchEvent_shouldReturnWebMerchantCode() {
    StockUpdateSearchEvent stockUpdateSearchEvent = new StockUpdateSearchEvent();
    stockUpdateSearchEvent.setWebMerchantCode(TEST_MERCHANT_CODE);
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .stockUpdateSearchEvent(stockUpdateSearchEvent)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withLevel2InventoryEvent_shouldReturnLevel2MerchantCode() {
    Level2InventoryQuantityChangedEvent level2Event = Level2InventoryQuantityChangedEvent.builder()
        .level2MerchantCode(TEST_MERCHANT_CODE)
        .build();
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .level2InventoryQuantityChangedEvent(level2Event)
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isEqualTo(TEST_MERCHANT_CODE);
  }

  @Test
  void testExtractMerchantCodeFromPickupPointEventModel_withNoRelevantFields_shouldReturnNull() {
    PickupPointUpsertCombinedEventModel eventModel = PickupPointUpsertCombinedEventModel.builder()
        .eventTrigger("some-trigger")
        .build();
    String result = ModuleProductUtil.extractMerchantCode(eventModel);
    assertThat(result).isNull();
  }

  @Test
  void testFromFlashsaleProductToAdjustmentProduct_whenFlashsaleProductIsNull_shouldReturnNull() {
    AdjustmentProduct result = ModuleProductUtil.fromFlashsaleProductToAdjustmentProduct(null);
    assertThat(result).isNull();
  }

  @Test
  void testFromFlashsaleProductToAdjustmentProduct_whenFlashsaleProductValid_shouldReturnAdjustmentProduct() {
    SivaFlashsaleSchedule schedule = SivaFlashsaleSchedule.builder()
        .start(FLASHSALE_START_DATE)
        .end(FLASHSALE_END_DATE)
        .timeBased(true)
        .build();
    FlashsaleProduct flashsaleProduct = FlashsaleProduct.builder()
        .itemSku(ITEM_SKU)
        .campaignCode(FLASHSALE_CAMPAIGN_CODE)
        .sessionId(FLASHSALE_SESSION_ID)
        .active(true)
        .pickupPointCode(PICKUP_POINT_CODE)
        .schedule(schedule)
        .build();

    AdjustmentProduct result = ModuleProductUtil.fromFlashsaleProductToAdjustmentProduct(flashsaleProduct);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getCampaignCode()).isEqualTo(FLASHSALE_CAMPAIGN_CODE);
    assertThat(result.getPromoType()).isEqualTo(CampaignType.FLASH_SALE);
    assertThat(result.getStartDate()).isEqualTo(FLASHSALE_START_DATE);
    assertThat(result.getEndDate()).isEqualTo(FLASHSALE_END_DATE);
    assertThat(result.getInitValue()).isEqualTo(0L);
    assertThat(result.getValue()).isEqualTo(0L);
    assertThat(result.isActivated()).isTrue();
    assertThat(result.isPromoCampaign()).isTrue();
    assertThat(result.getSessionId()).isEqualTo(FLASHSALE_SESSION_ID);
    assertThat(result.getPriority()).isEqualTo(CampaignPriority.FLASH_SALE);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
    assertThat(result.getInitBudgetOwners()).contains(CampaignOwner.BLIBLI);
    assertThat(result.getBudgetOwners()).contains(CampaignOwner.BLIBLI);
  }

  @Test
  void testFromAdjustmentProductToFlashsaleProduct_whenAdjustmentProductIsNull_shouldReturnNull() {
    FlashsaleProduct result = ModuleProductUtil.fromAdjustmentProductToFlashsaleProduct(null, FLASHSALE_PROMO_TYPES);
    assertThat(result).isNull();
  }

  @Test
  void testFromAdjustmentProductToFlashsaleProduct_whenPromoTypeNotMatching_shouldReturnNull() {
    AdjustmentProduct adjustmentProduct = AdjustmentProduct.builder()
        .itemSku(ITEM_SKU)
        .promoType(NON_FLASHSALE_PROMO_TYPE)
        .campaignCode(FLASHSALE_CAMPAIGN_CODE)
        .startDate(FLASHSALE_START_DATE)
        .endDate(FLASHSALE_END_DATE)
        .activated(true)
        .sessionId(FLASHSALE_SESSION_ID)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    FlashsaleProduct result = ModuleProductUtil.fromAdjustmentProductToFlashsaleProduct(adjustmentProduct, FLASHSALE_PROMO_TYPES);

    assertThat(result).isNull();
  }

  @Test
  void testFromAdjustmentProductToFlashsaleProduct_whenValidInputs_shouldReturnFlashsaleProduct() {
    AdjustmentProduct adjustmentProduct = AdjustmentProduct.builder()
        .itemSku(ITEM_SKU)
        .promoType(ADJUSTMENT_PROMO_TYPE)
        .campaignCode(FLASHSALE_CAMPAIGN_CODE)
        .startDate(FLASHSALE_START_DATE)
        .endDate(FLASHSALE_END_DATE)
        .activated(true)
        .sessionId(FLASHSALE_SESSION_ID)
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();

    FlashsaleProduct result = ModuleProductUtil.fromAdjustmentProductToFlashsaleProduct(adjustmentProduct, FLASHSALE_PROMO_TYPES);

    assertThat(result).isNotNull();
    assertThat(result.getItemSku()).isEqualTo(ITEM_SKU);
    assertThat(result.getPickupPointCode()).isEqualTo(PICKUP_POINT_CODE);
    assertThat(result.getCampaignCode()).isEqualTo(FLASHSALE_CAMPAIGN_CODE);
    assertThat(result.getSchedule()).isNotNull();
    assertThat(result.getSchedule().getStart()).isEqualTo(FLASHSALE_START_DATE);
    assertThat(result.getSchedule().getEnd()).isEqualTo(FLASHSALE_END_DATE);
    assertThat(result.getSchedule().isTimeBased()).isTrue();
    assertThat(result.isActive()).isTrue();
    assertThat(result.getSessionId()).isEqualTo(FLASHSALE_SESSION_ID);
  }

  @Test
  void testFromAdjustmentProductToCampaignProduct_whenAdjustmentProductIsNotNull_shouldReturnNull() {
    AdjustmentProduct adjustmentProduct = AdjustmentProduct.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .campaignCode(FLASHSALE_CAMPAIGN_CODE)
        .promoType(NON_FLASHSALE_PROMO_TYPE)
        .startDate(FLASHSALE_START_DATE)
        .endDate(FLASHSALE_END_DATE)
        .activated(true)
        .sessionId(FLASHSALE_SESSION_ID)
        .priority(1)
        .value(1000L)
        .build();

    CampaignProduct result = ModuleProductUtil.fromAdjustmentProductToCampaignProduct(
        adjustmentProduct, FLASHSALE_PROMO_TYPES);

    assertThat(result).isNull();
  }

  @Test
  void testFromAdjustmentProductToCampaignProduct_whenPromoTypeIsFlashsale_shouldReturnNull() {
    AdjustmentProduct adjustmentProduct = AdjustmentProduct.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .campaignCode(FLASHSALE_CAMPAIGN_CODE)
        .promoType(ADJUSTMENT_PROMO_TYPE)
        .startDate(FLASHSALE_START_DATE)
        .endDate(FLASHSALE_END_DATE)
        .activated(true)
        .sessionId(FLASHSALE_SESSION_ID)
        .priority(1)
        .value(1000L)
        .build();

    CampaignProduct result = ModuleProductUtil.fromAdjustmentProductToCampaignProduct(
        adjustmentProduct, FLASHSALE_PROMO_TYPES);

    assertThat(result).isNull();
  }

  @Test
  void testFilterDistributionPickupPoints_whenPickupPointsIsNull_shouldReturnNull() {
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(null, true);
    assertThat(result).isNull();
  }

  @Test
  void testFilterDistributionPickupPoints_whenPickupPointsIsEmpty_shouldReturnEmpty() {
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(new ArrayList<>(), true);
    assertThat(result).isEmpty();
  }

  @Test
  void testFilterDistributionPickupPoints_whenFilterFlagIsFalse_shouldReturnOriginalList() {
    PickupPoint distributionPickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .distribution(true)
        .build();
    List<PickupPoint> pickupPoints = List.of(distributionPickupPoint);
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(pickupPoints, false);
    assertThat(result).hasSize(1);
    assertThat(result.get(0).isDistribution()).isTrue();
  }

  @Test
  void testFilterDistributionPickupPoints_whenAllAreDistribution_shouldReturnAll() {
    PickupPoint distributionPickupPoint1 = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .distribution(true)
        .build();
    PickupPoint distributionPickupPoint2 = PickupPoint.builder()
        .itemSku(ITEM_SKU1)
        .pickupPointCode(PICKUP_POINT)
        .distribution(true)
        .build();
    List<PickupPoint> pickupPoints = List.of(distributionPickupPoint1, distributionPickupPoint2);
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(pickupPoints, true);
    assertThat(result).hasSize(2);
  }

  @Test
  void testFilterDistributionPickupPoints_whenMixedDistributionAndNonDistribution_shouldReturnOnlyNonDistribution() {
    PickupPoint distributionPickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .distribution(true)
        .build();
    PickupPoint nonDistributionPickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU1)
        .pickupPointCode(PICKUP_POINT)
        .distribution(false)
        .build();
    List<PickupPoint> pickupPoints = List.of(distributionPickupPoint, nonDistributionPickupPoint);
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(pickupPoints, true);
    assertThat(result).hasSize(1);
    assertThat(result.get(0).isDistribution()).isFalse();
    assertThat(result.get(0).getItemSku()).isEqualTo(ITEM_SKU1);
  }

  @Test
  void testFilterDistributionPickupPoints_whenNoneAreDistribution_shouldReturnAll() {
    PickupPoint nonDistributionPickupPoint1 = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .distribution(false)
        .build();
    PickupPoint nonDistributionPickupPoint2 = PickupPoint.builder()
        .itemSku(ITEM_SKU1)
        .pickupPointCode(PICKUP_POINT)
        .distribution(false)
        .build();
    List<PickupPoint> pickupPoints = List.of(nonDistributionPickupPoint1, nonDistributionPickupPoint2);
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(pickupPoints, true);
    assertThat(result).hasSize(2);
  }

  @Test
  void testFilterDistributionPickupPoints_whenListContainsNullElements_shouldFilterThemOut() {
    PickupPoint nonDistributionPickupPoint = PickupPoint.builder()
        .itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE)
        .distribution(false)
        .build();
    List<PickupPoint> pickupPoints = new ArrayList<>();
    pickupPoints.add(null);
    pickupPoints.add(nonDistributionPickupPoint);
    pickupPoints.add(null);
    List<PickupPoint> result = ModuleProductUtil.filterDistributionPickupPoints(pickupPoints, true);
    assertThat(result).hasSize(1);
    assertThat(result.get(0).getItemSku()).isEqualTo(ITEM_SKU);
  }

}