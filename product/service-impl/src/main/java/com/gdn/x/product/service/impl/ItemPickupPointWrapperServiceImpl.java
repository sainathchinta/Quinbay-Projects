package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.rest.web.model.dto.PickupPointDetailsDTO;
import com.gdn.x.product.rest.web.model.request.EanUpcPickupPointCodeRequest;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.SwitchContext;
import jakarta.validation.ConstraintViolationException;

import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemPickupPointDeleteRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointTransactionResponse;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequest;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequestList;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.DataSourceWrapperService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointMigrationService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.DataSourcePrimaryEnabledProperties;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ModelConverter;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.gdn.x.product.service.util.ValidationUtil;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ItemPickupPointWrapperServiceImpl implements ItemPickupPointWrapperService {

  private static final String BUNDLE_RECIPE_CHANGE = "BUNDLE_RECIPE_CHANGE";
  @Autowired
  private ItemPickupPointMigrationService itemPickupPointMigrationService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Autowired
  private ItemPickupPointSummaryServiceImpl itemPickupPointSummaryServiceImpl;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private DataSourceWrapperService dataSourceWrapperService;

  @Autowired
  private DataSourcePrimaryEnabledProperties dataSourcePrimaryEnabledProperties;

  @Autowired
  private ModelConverter modelConverter;

  @Autowired
  private ItemPriceService itemPriceService;

  @Autowired
  private SwitchContext switchContext;

  @Value("${max.items.for.transaction}")
  private int maxItemsForTransaction;

  @Value("${master.category.catalog.code}")
  private String masterCatalogCode;

  @Value("${item.pickup.point.fetch.size}")
  private int itemPickupPointFetchSize;

  @Value("${configured.max.cache.fetch.size}")
  private int configuredMaxCacheFetchSize;

  @Value("${main.image.from.main.image.url.for.unsync}")
  private boolean mainImageFromMainImageUrlForUnsync;

  @Value("${list.of.clients.to.exclude.item.catalog}")
  private String listOfClientsToExcludeItemCatalogs;

  @Value("${use.pcb.master.data}")
  private boolean usePcbMasterData;

  @Value("${ignore.sync.flag.while.setting.category.code}")
  private boolean ignoreSyncFlagWhileSettingCategoryCode;

  @Value("${delete.item.pickup.point.from.inventory}")
  private boolean deleteItemPickupPointFromInventory;

  @Value("${override.archive.flag.from.l3}")
  private boolean overrideArchiveFlagFromL3;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${product.detail.remove.inactive.sales.categories}")
  private boolean productDetailRemoveInactiveSalesCategories;

  @Value("${imei.attribute.code}")
  private String imeiAttributeCode;

  @Value("#{'${imei.attribute.allowed.values}'.split(',')}")
  private List<String> imeiAllowedValues;

  @Value("${item.summary.list.request.size}")
  private int itemSummaryListRequestSize;

  @Value("#{'${item.summary.list.response.cache.clients}'.split(',')}")
  private List<String> itemSummaryListResponseCacheClients;

  @Autowired
  private ApplicationContext applicationContext;

  @Override
  public void migrateItemPickupPointCollection(String storeId, String itemSku, String status) {
    try {
      if (StringUtils.isNotEmpty(itemSku)) {
        publishAndSaveForSingleItem(itemSku);
        return;
      }
      Map<String, Integer> switchesForL5Migration = fetchL5MigrationSwitches(storeId);
      int maxMigrationItems =
        switchesForL5Migration.get(SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH);
      if(maxMigrationItems <= 0) {
        log.warn("Exiting migration API due to max items size: {} ", maxMigrationItems);
        return;
      }
      int totalProducts = 0;
      while (totalProducts < maxMigrationItems) {
        int itemFetchSize =
          switchesForL5Migration.get(SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS);
        List<ItemPickupPointMigration> itemPickupPointMigrationList =
          fetchItemMigrationList(status, itemFetchSize, maxMigrationItems, totalProducts);
        if (CollectionUtils.isEmpty(itemPickupPointMigrationList)) {
          break;
        }
        updateStatusForMigrationEntry(itemPickupPointMigrationList);
        publishItemsForMigration(itemPickupPointMigrationList, switchesForL5Migration);
        totalProducts += itemPickupPointMigrationList.size();
        Thread.sleep(
          switchesForL5Migration.get(SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME));
      }
    } catch (Exception e) {
      log.error("Error on publishing of events for L5 migration, error - ", e);
    }
  }

  private void publishAndSaveForSingleItem(String itemSku) {
    ItemPickupPointMigration itemPickupPointMigration =
      this.itemPickupPointMigrationService.findByItemSku(itemSku);
    if (Objects.nonNull(itemPickupPointMigration)) {
      this.updateStatusForMigrationEntry(Collections.singletonList(itemPickupPointMigration));
    } else {
      ItemPickupPointMigration newItemPickupPointMigration =
        ItemPickupPointMigration.builder().itemSku(itemSku)
          .status(ItemMigrationStatus.PUBLISHED.name()).startTime(new Date()).build();
      this.itemPickupPointMigrationService.saveCollection(
        Collections.singletonList(newItemPickupPointMigration));
    }
    this.saveAndPublishService.publishItemsForMigration(Collections.singletonList(itemSku));
  }

  private void updateStatusForMigrationEntry(
    List<ItemPickupPointMigration> itemPickupPointMigrationList) {
    for(ItemPickupPointMigration itemPickupPointMigration : itemPickupPointMigrationList) {
      itemPickupPointMigration.setStatus(ItemMigrationStatus.PUBLISHED.name());
      itemPickupPointMigration.setStartTime(new Date());
    }
    this.itemPickupPointMigrationService.saveCollection(itemPickupPointMigrationList);
  }

  private Map<String, Integer> fetchL5MigrationSwitches(String storeId) {
    Map<String, Integer> migrationSwitches = new HashMap<>();
    migrationSwitches.put(SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH, Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH).getValue()));
    migrationSwitches.put(SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS, Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS).getValue()));
    migrationSwitches.put(SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE, Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE).getValue()));
    migrationSwitches.put(SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME, Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME).getValue()));
    migrationSwitches.put(SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME, Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME).getValue()));
    return migrationSwitches;
  }

  private List<ItemPickupPointMigration> fetchItemMigrationList(String status, int limit,
    int maxMigrationItems, int totalProducts) {
    if (totalProducts + limit < maxMigrationItems) {
      return this.itemPickupPointMigrationService.findByStatusAndLimit(status, limit);
    } else {
      return this.itemPickupPointMigrationService.findByStatusAndLimit(status,
        maxMigrationItems - totalProducts);
    }
  }

  private void publishItemsForMigration(List<ItemPickupPointMigration> itemPickupPointMigrationList,
    Map<String, Integer> switchesForL5Migration) throws Exception {
    List<List<ItemPickupPointMigration>> itemPickupPointMigrationBatches =
      Lists.partition(itemPickupPointMigrationList,
        switchesForL5Migration.get(SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE));
    for (List<ItemPickupPointMigration> itemPickupPointMigrations :
      itemPickupPointMigrationBatches) {
      this.saveAndPublishService.publishItemsForMigration(
        itemPickupPointMigrations.stream().map(ItemPickupPointMigration::getItemSku)
          .collect(Collectors.toList()));
      Thread.sleep(switchesForL5Migration.get(SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME));
    }
  }



  @Override
  public void processItemMigrationEvent(
    ItemPickupPointMigrationEvent itemPickupPointMigrationEvent) {
    Map<String, String> failedItemSku = new HashMap<>();
    List<String> successItemSku = new ArrayList<>();
    List<String> conflictItemSku = new ArrayList<>();
    this.itemPickupPointMigrationService.updateStatusByItemSku(
      itemPickupPointMigrationEvent.getItemSkuList(), ItemMigrationStatus.IN_PROGRESS.name());
    Map<String, Item> itemMap = Optional.ofNullable(
        this.itemService.getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
          new HashSet<>(itemPickupPointMigrationEvent.getItemSkuList()))).orElse(new ArrayList<>())
      .stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    List<ItemPickupPoint> itemPickupPointList =
      CommonUtil.toItemPickupPointList(new ArrayList<>(itemMap.values()));
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      try {
        this.itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
        successItemSku.add(itemPickupPoint.getItemSku());
      } catch (ConstraintViolationException | DuplicateKeyException cve) {
        try {
          log.warn("ConstraintViolationException for input : {}, error - ", itemPickupPoint, cve);
          ItemPickupPoint existingItemPickupPoint =
            this.itemPickupPointService.findByItemSkuAndPickupPointCode(
              itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(),
              itemPickupPoint.getPickupPointCode());
          this.itemPickupPointService.saveItemPickupPoint(CommonUtil.toItemPickupPointOnConflict(
            itemMap.get(existingItemPickupPoint.getItemSku()), existingItemPickupPoint));
          conflictItemSku.add(itemPickupPoint.getItemSku());
        } catch (Exception e) {
          log.error("Exception on setting itemPickupPoint for : {}, error - ", itemPickupPoint, e);
          failedItemSku.put(itemPickupPoint.getItemSku(), e.getMessage());
        }
      } catch (Exception e) {
        log.error("Exception on setting itemPickupPoint for : {}, error - ", itemPickupPoint, e);
        failedItemSku.put(itemPickupPoint.getItemSku(), e.getMessage());
      }
    }
    if (CollectionUtils.isNotEmpty(successItemSku)) {
      this.itemPickupPointMigrationService.updateStatusByItemSku(successItemSku,
        ItemMigrationStatus.COMPLETED.name());
    }
    if (MapUtils.isNotEmpty(failedItemSku)) {
      this.itemPickupPointMigrationService.updateFailedStatusByItemSku(failedItemSku);
    }
    if (CollectionUtils.isNotEmpty(conflictItemSku)) {
      this.itemPickupPointMigrationService.updateStatusByItemSku(conflictItemSku,
        ItemMigrationStatus.COMPLETED_WITH_CONFLICT.name());
    }
  }

  @Override
  public void updateItemPickupPointOnItemChange(ItemChange itemChange) throws Exception {
    Boolean itemChangeListenerSwitch = Boolean.parseBoolean(
      this.systemParameterService.findValueByStoreIdAndVariable(itemChange.getStoreId(),
        SystemParameterNames.ITEM_CHANGE_SWITCH).getValue());
    if (Boolean.FALSE.equals(itemChangeListenerSwitch)) {
      return;
    }
    ItemPickupPointMigration itemPickupPointMigration =
      this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku());
    if (Objects.nonNull(itemPickupPointMigration) && ItemMigrationStatus.PENDING.name()
      .equals(itemPickupPointMigration.getStatus())) {
      return;
    }
    List<ItemPickupPoint> itemPickupPointsToSave = new ArrayList<>();
    ItemPickupPoint deliveryItemPickupPoint =
        this.itemPickupPointService.findByItemSkuAndDelivery(itemChange.getStoreId(), itemChange.getItemSku());
    ItemPickupPoint itemPickupPoint =
      this.itemPickupPointService.findByItemSkuAndPickupPointCode(itemChange.getStoreId(),
        itemChange.getItemSku(), itemChange.getPickupPointCode());
    if (Objects.nonNull(itemPickupPoint)) {
      CommonUtil.setItemChangeToItemPickupPoint(itemChange, itemPickupPoint);
      itemPickupPointsToSave.add(itemPickupPoint);
      if (!deliveryItemPickupPoint.getPickupPointCode()
        .equals(itemPickupPoint.getPickupPointCode())) {
        deliveryItemPickupPoint.setDelivery(false);
        deliveryItemPickupPoint.getItemViewConfig().forEach(itemViewConfig -> {
          itemViewConfig.setBuyable(false);
          itemViewConfig.setDiscoverable(false);
        });
        if (!deliveryItemPickupPoint.isCncActive()) {
          deliveryItemPickupPoint.setMarkForDelete(true);
        }
      }

    } else {
      if (deliveryItemPickupPoint.isCncActive()) {
        deliveryItemPickupPoint.setDelivery(false);
        Item item = new Item();
        BeanUtils.copyProperties(item, itemChange);
        itemPickupPoint = CommonUtil.toItemPickupPointList(Collections.singletonList(item)).get(0);
        itemPickupPointsToSave.add(itemPickupPoint);
      } else {
        deliveryItemPickupPoint.setPrice(CommonUtil.toItemPrices(itemChange.getPrice()));
        deliveryItemPickupPoint.setItemViewConfig(
          CommonUtil.toItemViewConfigs(itemChange.getItemViewConfigs()));
        deliveryItemPickupPoint.setPickupPointCode(itemChange.getPickupPointCode());
        deliveryItemPickupPoint.setOfflineItemId(
          itemChange.getItemSku() + (StringUtils.isEmpty(itemChange.getPickupPointCode()) ?
            StringUtils.EMPTY :
            Constants.HYPHEN + itemChange.getPickupPointCode()));
      }
    }
    itemPickupPointsToSave.add(deliveryItemPickupPoint);
    this.itemPickupPointService.saveItemPickupPointCollection(itemPickupPointsToSave);
  }

  @Override
  public void updateItemPickupPointOnOfflineItemChange(OfflineItemChange offlineItemChange) {
    Boolean offlineItemChangeListenerSwitch = Boolean.parseBoolean(
      this.systemParameterService.findValueByStoreIdAndVariable(offlineItemChange.getStoreId(),
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH).getValue());
    if (Boolean.FALSE.equals(offlineItemChangeListenerSwitch)) {
      return;
    }
    ItemPickupPoint itemPickupPoint =
      this.itemPickupPointService.findByItemSkuAndPickupPointCode(offlineItemChange.getStoreId(),
        offlineItemChange.getItemSku(), offlineItemChange.getPickupPointCode());
    if (Objects.nonNull(itemPickupPoint)) {
      if (offlineItemChange.isMarkForDelete()) {
        itemPickupPoint.setCncActive(false);
        if (!itemPickupPoint.isDelivery()) {
          itemPickupPoint.setMarkForDelete(true);
        }
      } else {
        CommonUtil.setOfflineItemChangeToExistingItemPickupPoint(offlineItemChange,
          itemPickupPoint);
      }
    } else {
      if (!offlineItemChange.isMarkForDelete()) {
        itemPickupPoint = CommonUtil.setOfflineItemChangeToItemPickupPoint(offlineItemChange);
      }
    }
    if (Objects.nonNull(itemPickupPoint)) {
      this.itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
    }
  }

  @Override
  public void updateMarkForDeleteByMerchantCode(String storeId, String businessPartnerCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
      ErrorMessages.MERCHANT_CODE_EMPTY);
    List<ItemPickupPoint> itemPickupPointList =
      this.itemPickupPointService.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(storeId,
        businessPartnerCode);
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      if (itemPickupPoint.isCncActive()) {
        if (itemPickupPoint.isDelivery()) {
          itemPickupPoint.setCncActive(false);
        } else {
          itemPickupPoint.setMarkForDelete(true);
        }
      }
    }
    this.itemPickupPointService.saveItemPickupPointCollection(itemPickupPointList);
  }

  @Override
  public List<ItemPickupPointTransactionResponse> findProductForTransactionByItemSkusAndPickupPointCode(String storeId,
      String requestId, String username, List<ItemPickupPointRequest> itemPickupPointRequests) throws Exception {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(itemPickupPointRequests.size() < maxItemsForTransaction,
        ErrorMessages.ITEM_COUNT_LIMIT_FOR_TRANSACTION_API);
    ProductAndItemsUtil.validateItemPickupPointRequest(itemPickupPointRequests);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(storeId, itemPickupPointRequests, false);

    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      Map<String, Item> itemMap = fetchItemByItemSkusAndMarkForDeleteFalse(storeId, itemPickupPointList, false);
      Map<String, Product> productMap = fetchProductByProductSkusAndMarkForDeleteFalse(storeId, itemPickupPointList,
          false);
      Map<String, String> pickupPointNameMap = fetchBusinessPartnerPickupPointByPickupPointCodes(storeId, itemPickupPointList);
      overrideArchivedFlagFromL3(itemMap, productMap);

      // Call pcb for master data to when switch is on otherwise use migrated master data fields
      if (MapUtils.isNotEmpty(itemMap) && MapUtils.isNotEmpty(productMap)) {
        Set<String> withoutMasterDataProductSkus =
            setMasterData(storeId, username, requestId, productMap.values(), itemMap.values(),
              switchContext.isUsePcbMasterData());
        fetchAndSetItemCatalogs(productMap.values(), username, requestId, false, Constants.ALL);
        setCurrentItemViewConfig(itemPickupPointList, itemMap, null, new HashMap<>());
        setSettlementType(productMap.values(), itemMap.values());

        return ResponseHelper.toProductForTransactionVO(itemPickupPointList, itemMap, productMap, pickupPointNameMap,
            withoutMasterDataProductSkus, switchContext.isMainImageFromMainImageUrlForUnsync(),
          switchContext.isOverrideLateFulfillmentByProductType(),
            switchContext.isCncForWarehouseFeatureSwitch(), imeiAttributeCode, imeiAllowedValues);
      }
    }

    log.info("No item pickup point found with request : {} ", itemPickupPointRequests);
    return new ArrayList<>();
    }

  private void overrideArchivedFlagFromL3(Map<String, Item> itemMap, Map<String, Product> productMap) {
    if (overrideArchiveFlagFromL3) {
      for (Map.Entry<String, Item> entry : itemMap.entrySet()) {
        Item item = entry.getValue();
        if (productMap.containsKey(item.getProductSku())) {
          item.setArchived(productMap.get(item.getProductSku()).isArchived());
        }
      }
    }
  }

  @Override
  public List<ItemSummaryListResponse> fetchItemSummaryByItemSkusAndPickupPointCode(String storeId,
    String requestId, String username, boolean withValueAndValueTypes,
    List<ItemPickupPointRequest> itemPickupPointRequests, String fetchViewConfigByChannel,
    String clientId, String catalogType, boolean excludeDistributionPickupPoint) throws Exception {
    if (CommonUtil.isItemSummaryCacheEligible(itemPickupPointRequests, clientId,
      itemSummaryListResponseCacheClients, itemSummaryListRequestSize)) {
      String cacheKey =
          CommonUtil.generateItemSummaryCacheKey(storeId, itemPickupPointRequests, excludeDistributionPickupPoint);
      ItemPickupPointWrapperServiceImpl itemPickupPointWrapperService =
        applicationContext.getBean(ItemPickupPointWrapperServiceImpl.class);
      return itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCodeCached(
        cacheKey, storeId, requestId, username, withValueAndValueTypes, itemPickupPointRequests,
        fetchViewConfigByChannel, clientId, catalogType, excludeDistributionPickupPoint);
    } else {
      return findItemSummaryByItemSkusAndPickupPointCode(storeId, requestId, username,
        withValueAndValueTypes, itemPickupPointRequests, fetchViewConfigByChannel, clientId,
        catalogType, excludeDistributionPickupPoint);
    }
  }

  @Cacheable(cacheManager = Constants.ITEM_PICKUP_POINT_CACHE_MANAGER, value =
    CacheNames.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE_LIST, key = "#cacheKey",
    unless = "#result == null")
  public List<ItemSummaryListResponse> findItemSummaryByItemSkusAndPickupPointCodeCached(
    String cacheKey, String storeId, String requestId, String username,
    boolean withValueAndValueTypes, List<ItemPickupPointRequest> itemPickupPointRequests,
    String fetchViewConfigByChannel, String clientId, String catalogType, boolean excludeDistributionPickupPoint) throws Exception {
    return findItemSummaryByItemSkusAndPickupPointCode(storeId, requestId, username,
      withValueAndValueTypes, itemPickupPointRequests, fetchViewConfigByChannel, clientId,
      catalogType, excludeDistributionPickupPoint);
  }

  @Override
  public List<ItemSummaryListResponse> findItemSummaryByItemSkusAndPickupPointCode(String storeId, String requestId,
      String username, boolean withValueAndValueTypes, List<ItemPickupPointRequest> itemPickupPointRequests,
      String fetchViewConfigByChannel, String clientId, String catalogType, boolean excludeDistributionPickupPoint) throws Exception {
    ProductAndItemsUtil.validateItemPickupPointRequest(itemPickupPointRequests);
    List<ItemPickupPoint> itemPickupPointList =
      fetchItemPickupPointList(storeId, itemPickupPointRequests);
    if (excludeDistributionPickupPoint) {
      itemPickupPointList = itemPickupPointList.stream().filter(Predicate.not(ItemPickupPoint::isDistribution))
          .collect(Collectors.toList());
    }
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Price price = itemPickupPoint.getPrice().iterator().next();
      List<DiscountPrice> listOfDiscountPrices = price.getListOfDiscountPrices();
      DiscountPrice discountPrice = itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices);
      price.setListOfDiscountPrices(Collections.singletonList(discountPrice));
      DiscountPrice merchantPromoDiscount = Optional.ofNullable(price).map(Price::getMerchantPromoDiscountPrice)
          .filter(itemPriceService::validMerchantPromoDiscountPrice).orElse(null);
      price.setMerchantPromoDiscountPrice(merchantPromoDiscount);
      itemPickupPoint.setPrice(Collections.singleton(price));
    }
    if(CollectionUtils.isEmpty(itemPickupPointList)) {
      return  new ArrayList<>();
    }
    Map<String, Item> itemMap = fetchItemByItemSkus(storeId, itemPickupPointList);
    Map<String, Product> productMap = fetchProductByProductSkus(storeId, username, requestId, itemPickupPointList);
    Map<String, String> pickupPointNameMap = fetchBusinessPartnerPickupPointByPickupPointCodes(storeId, itemPickupPointList);
    setMasterData(storeId, username, requestId, productMap.values(), itemMap.values(),
        withValueAndValueTypes || switchContext.isUsePcbMasterData());
    populateItemCatalogsBasedOnClientId(requestId, username, clientId, productMap, catalogType);
    Map<String, ItemViewConfig> offlineItemIdAndViewConfigOriginalMap = new HashMap<>();
    Map<String, ItemViewConfig> itemViewConfigB2bMap = new HashMap<>();
    setCurrentItemViewConfig(itemPickupPointList, itemMap, offlineItemIdAndViewConfigOriginalMap, itemViewConfigB2bMap);
    Map<String, Map<String, Map<String, String>>> productAttributeValueAndValueTypeMap = populateValueTypeUsingProductAttributes(withValueAndValueTypes, productMap);
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      ResponseHelper.toItemSummaryListResponse(itemPickupPointList, itemMap, productMap,
        pickupPointNameMap, offlineItemIdAndViewConfigOriginalMap, itemViewConfigB2bMap,
        productAttributeValueAndValueTypeMap, switchContext, fetchViewConfigByChannel);
    return itemSummaryListResponseList;
  }

  private Map<String, Map<String, Map<String, String>>> populateValueTypeUsingProductAttributes(
      boolean withValueAndValueTypes, Map<String, Product> productMap) {
    Map<String, Map<String, Map<String, String>>> productAttributeValueAndValueTypeMap = new HashMap<>();
    if (withValueAndValueTypes) {
      for (Product product : productMap.values()) {
        Map<String, Map<String, String>> attributeCodeValueAndValueType = new HashMap<>();
        modelConverter.populateAttributeCodeValueAndValueType(product, attributeCodeValueAndValueType);
        productAttributeValueAndValueTypeMap.put(product.getProductSku(), attributeCodeValueAndValueType);
      }
    }
    return productAttributeValueAndValueTypeMap;
  }

  private void populateItemCatalogsBasedOnClientId(String requestId, String username, String clientId, Map<String, Product> productMap,
      String catalogType) {
    String[] excludeItemCatalogResponseClientIdList =
        StringUtils.defaultString(listOfClientsToExcludeItemCatalogs, StringUtils.EMPTY)
            .split(Constants.COMMA_DELIMITER);
    boolean excludeItemCatalogResponse = Arrays.stream(excludeItemCatalogResponseClientIdList).anyMatch(clientId::equalsIgnoreCase);
    if (excludeItemCatalogResponse) {
      productMap.values().forEach(product -> product.setItemCatalogs(new ArrayList<>()));
      log.warn("excluding populating item catalog for for clientId {} and requestId {} ", clientId, requestId);
    } else {
      fetchAndSetItemCatalogs(productMap.values(), username, requestId, productDetailRemoveInactiveSalesCategories, catalogType);
    }
  }

  private List<ItemPickupPoint> fetchItemPickupPointList(String storeId,
    List<ItemPickupPointRequest> itemPickupPointRequests) {
    if (useCacheForL5Fetch(itemPickupPointRequests)) {
      return itemPickupPointRequests.stream().map(
          itemPickupPointRequest -> itemPickupPointService.findByItemSkuAndPickupPointCode(storeId,
            itemPickupPointRequest.getItemSku(), itemPickupPointRequest.getPickupPointCode()))
        .filter(Objects::nonNull).filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete()).collect(Collectors.toList());
    }
    return itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(storeId,
      itemPickupPointRequests, false);
  }

  private boolean useCacheForL5Fetch(List<ItemPickupPointRequest> itemPickupPointRequests) {
    return CollectionUtils.isNotEmpty(itemPickupPointRequests)
      && itemPickupPointRequests.size() < configuredMaxCacheFetchSize;
  }


  private Map<String, Product> fetchProductByProductSkusAndMarkForDeleteFalse(String storeId,
      List<ItemPickupPoint> itemPickupPointList, boolean inAllProducts) {
    List<Product> productList;
    if (inAllProducts) {
      productList = Optional.ofNullable(this.productService.findByStoreIdAndProductSkuIn(storeId,
          ProductAndItemsUtil.getProductSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    } else {
      productList = Optional.ofNullable(this.productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
          ProductAndItemsUtil.getProductSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    }
    return productList.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
  }

  private Map<String, Item> fetchItemByItemSkusAndMarkForDeleteFalse(String storeId,
      List<ItemPickupPoint> itemPickupPointList, boolean inAllProducts) {
    List<Item> itemList;
    if (inAllProducts) {
      itemList = Optional.ofNullable(this.itemService.getItemsByStoreIdAndItemSkus(storeId,
          ProductAndItemsUtil.getItemSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    } else {
      itemList = Optional.ofNullable(this.itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(storeId,
          ProductAndItemsUtil.getItemSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    }
    return itemList.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
  }

  private Map<String, Product> fetchProductByProductSkus(String storeId, String username, String requestId,
      List<ItemPickupPoint> itemPickupPointList) {
    List<Product> productList = Optional.ofNullable(this.productService.findByStoreIdAndProductSkuIn(storeId,
        ProductAndItemsUtil.getProductSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    return productList.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
  }

  private Map<String, Item> fetchItemByItemSkus(String storeId, List<ItemPickupPoint> itemPickupPointList) {
    List<Item> itemList = Optional.ofNullable(this.itemService.getItemsByStoreIdAndItemSkus(storeId,
        ProductAndItemsUtil.getItemSkuFromItemPickupPoints(itemPickupPointList))).orElse(new ArrayList<>());
    for (Item item : itemList) {
      if (item.isPermanentDelete()) {
        throw new ApiIncorrectInputDataException(ApiErrorCodes.ITEM_DELETED.getErrorMessage(),
            ApiErrorCodes.ITEM_DELETED.getErrorCode());
      }
    }
    return itemList.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
  }

  private Map<String, String> fetchBusinessPartnerPickupPointByPickupPointCodes(String storeId,
      List<ItemPickupPoint> itemPickupPointList) {
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList = Optional.ofNullable(
            this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
                ProductAndItemsUtil.getPickupPointCodesFromItemPickupPoints(itemPickupPointList)))
        .orElse(new ArrayList<>());
    return businessPartnerPickupPointList.stream()
        .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::getName));
  }

  private void setSettlementType(Collection<Product> products, Collection<Item> items) {
    Map<String, Item> productSkuItemMap =
        items.stream().collect(Collectors.toMap(Item::getProductSku, Function.identity(), (v1, v2) -> v2));
    for (Product product : products) {
      Item item = Optional.ofNullable(productSkuItemMap.get(product.getProductSku())).orElse(new Item());
      product.setSettlementType(productHelperService.getSettlementType(product, item));
    }
  }

  private void fetchAndSetItemCatalogs(Collection<Product> productList, String username, String requestId,
      boolean excludeInactiveSalesCategories, String catalogType) {
    Set<String> categoryCodeSet = new HashSet<>();
    Map<String, List<String>> mapOfProductToCategoryCodeList = new HashMap<>();
    productList.forEach(
        product -> CommonUtil.fetchCategoryCodeList(product, categoryCodeSet, mapOfProductToCategoryCodeList,
            catalogType));
    if (CollectionUtils.isNotEmpty(categoryCodeSet)) {
      Map<String, List<ItemCatalogVO>> itemCategoryVOListMap =
          this.catalogService.getCategoryCodeToItemCatalogsMap(username, requestId, new ArrayList<>(categoryCodeSet));
      productList.forEach(product -> CommonUtil.setItemCatalogs(product, itemCategoryVOListMap,
          mapOfProductToCategoryCodeList.get(product.getProductSku()), excludeInactiveSalesCategories));
    }
  }

  private void setCurrentItemViewConfig(List<ItemPickupPoint> itemPickupPointList, Map<String, Item> itemMap,
      Map<String, ItemViewConfig> offlineItemIdAndViewConfigOriginalMap,
      Map<String, ItemViewConfig> itemViewConfigB2bChannelMap) throws Exception {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      if (Objects.nonNull(item) && CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
        if (Objects.nonNull(offlineItemIdAndViewConfigOriginalMap)) {
          ItemViewConfig itemViewConfigOriginal = new ItemViewConfig();
          itemViewConfigOriginal.setBuyable(
              productHelperService.getOriginalBuyableStatusForItem(itemPickupPoint.getItemViewConfig(),
                  item.isArchived()));
          itemViewConfigOriginal.setDiscoverable(
              productHelperService.getOriginalDiscoverableStatusForItem(itemPickupPoint.getItemViewConfig(),
                  item.isArchived()));
          offlineItemIdAndViewConfigOriginalMap.put(
              CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
              itemViewConfigOriginal);
        }
        Set<ItemViewConfig> itemViewConfigsB2b = itemPickupPoint.getItemViewConfigByChannel(Constants.B2B);
        if (CollectionUtils.isNotEmpty(itemViewConfigsB2b)) {
          itemViewConfigB2bChannelMap.put(
              CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
              itemViewConfigsB2b.stream().findFirst().get());
        }
        ItemViewConfig itemViewConfig = new ItemViewConfig();
        itemViewConfig.setChannel(itemPickupPoint.getItemViewConfig().iterator().next().getChannel());
        itemViewConfig.setItemBuyableSchedules(
            itemPickupPoint.getItemViewConfig().iterator().next().getItemBuyableSchedules());
        itemViewConfig.setItemDiscoverableSchedules(
            itemPickupPoint.getItemViewConfig().iterator().next().getItemDiscoverableSchedules());
        itemViewConfig.setBuyable(
            productHelperService.getCurrentBuyableStatusForItem(itemPickupPoint.getItemViewConfig(), Constants.DEFAULT,
                item.isArchived()));
        itemViewConfig.setDiscoverable(
            productHelperService.getCurrentDiscoverableStatusForItem(itemPickupPoint.getItemViewConfig(),
                Constants.DEFAULT, item.isArchived()));
        Set<ItemViewConfig> itemViewConfigSet = new LinkedHashSet<>();
        itemViewConfigSet.add(itemViewConfig);
        itemViewConfigSet.addAll(itemPickupPoint.getAllItemViewConfigs().stream()
            .filter(viewConfig -> !Constants.DEFAULT.equals(viewConfig.getChannel())).collect(Collectors.toSet()));
        itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      } else {
        ItemViewConfig itemViewConfig = new ItemViewConfig();
        itemViewConfig.setChannel(Constants.DEFAULT_CHANNEL);
        itemPickupPoint.setItemViewConfig(ImmutableSet.of(itemViewConfig));
      }
    }
  }

  private Set<String> setMasterData(String storeId, String username, String requestId, Collection<Product> products,
      Collection<Item> items, boolean usePcbMasterData) throws Exception {
    List<Product> productWithMasterDataDetails =
        ProductAndItemsUtil.filterProductWithMasterDataDetails(products, usePcbMasterData);
    List<Product> productWithoutMasterDataDetails =
        ProductAndItemsUtil.filterProductWithoutMasterDataDetails(products, usePcbMasterData);
    Set<String> filterdProductSkus = ProductAndItemsUtil.getProductSkuFromProduct(productWithoutMasterDataDetails);

    if (CollectionUtils.isNotEmpty(productWithoutMasterDataDetails)) {
      Map<String, List<Item>> itemListMap = items.stream().collect(Collectors.groupingBy(Item::getProductSku));
      for (Product product : productWithoutMasterDataDetails) {
        if (product.isSynchronized()) {
          masterDataConstructorService.constructProductAndItemWithMasterData(storeId, username, requestId, product,
              itemListMap.get(product.getProductSku()));
        }
      }
    }

    if (CollectionUtils.isNotEmpty(productWithMasterDataDetails)) {
      if (ignoreSyncFlagWhileSettingCategoryCode) {
        productWithMasterDataDetails.stream()
            .filter(product -> StringUtils.isNotEmpty(product.getCategoryCode()))
            .forEach(product -> product.setMasterCatalog(new MasterCatalog(masterCatalogCode,
                new Category(product.getCategoryCode(), product.getCategoryCode()))));
      } else {
        productWithMasterDataDetails.stream().filter(Product::isSynchronized)
            .forEach(product -> product.setMasterCatalog(new MasterCatalog(masterCatalogCode,
                new Category(product.getCategoryCode(), product.getCategoryCode()))));
      }
    }

    return filterdProductSkus;
  }

  @Override
  public void updateItemPickupPointViewConfigWithProductStatus(String storeId, String userName, String requestId,
      String productSku, ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception {
    checkArgument(Objects.nonNull(itemPickupPointViewConfigBaseRequest),
        ErrorMessages.ITEM_PICKUPPOINT_VIEW_CONFIG_MUST_NOT_BE_NULL);
    itemPickupPointService.
        updateItemPickupPointViewConfigWithProductStatus(storeId, userName, requestId, productSku,
        itemPickupPointViewConfigBaseRequest);
  }

  @Override
  public EditItemResponse updateItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, EditProductDetailDTO editProductDetailDTO) throws Exception {
    Map<String, Item> itemSkuAndItemMap = new HashMap<>();
    log.info("Update productSku {} with request : {} ", itemPickupPointUpdateRequestVo.getProductSku(),
        itemPickupPointUpdateRequestVo);
    Product product = null;
    if (Objects.nonNull(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())) {
      if (CollectionUtils.isNotEmpty(
          itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getAddVariantsList())) {
        product =
            getProduct(mandatoryRequestParam, itemPickupPointUpdateRequestVo.getProductSku(),
              dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled());
        List<Item> newlyAddedVariants = new ArrayList<>();
        for (AddVariantRequestVo variantRequestVo : itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo()
            .getAddVariantsList()) {
          Item newVariant = itemHelperService.convertToItem(variantRequestVo, product, mandatoryRequestParam);
          newlyAddedVariants.add(newVariant);
        }
        newlyAddedVariants.forEach(item -> item.setNewData(Boolean.TRUE));
        newlyAddedVariants = saveOperationService.saveNewlyAddedItems(newlyAddedVariants);
        itemSkuAndItemMap =
            newlyAddedVariants.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
        List<ItemPickupPointListingUpdateRequestVo> addPickupPointRequests =
            Optional.ofNullable(itemPickupPointUpdateRequestVo.getAddPickupPointRequests()).orElse(new ArrayList<>());
        for (AddVariantRequestVo addVariantRequestVo : itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo()
            .getAddVariantsList()) {
          addPickupPointRequests.addAll(addVariantRequestVo.getItemPickupPoints());
        }
      }
      List<ItemPickupPointDeleteRequestVo> deletePickupPointRequests =
          itemPickupPointUpdateRequestVo.getDeletePickupPointRequests();
      if (CollectionUtils.isNotEmpty(
          itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getDeleteVariantsList())) {
        List<ItemPickupPoint> itemPickupPointList =
          dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(
            mandatoryRequestParam.getStoreId(),
            itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getDeleteVariantsList(),
            dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled());
        deletePickupPointRequests.addAll(itemPickupPointList.stream().map(
            itemPickupPoint -> new ItemPickupPointDeleteRequestVo(itemPickupPoint.getItemSku(),
                itemPickupPoint.getPickupPointCode())).toList());
      }
    }

    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequestVo.getBundleRecipeRequests())) {
      List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
      List<Item> bundleRecipeUpdateList = new ArrayList<>();
      if (Objects.isNull(product)) {
        product =
            getProduct(mandatoryRequestParam, itemPickupPointUpdateRequestVo.getProductSku(),
              dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled());
      }

      if (Objects.nonNull(product)) {
        bundleRecipeUpdate(itemPickupPointUpdateRequestVo, product, itemSkuAndItemMap,
          bundleRecipeUpdateList, auditTrailDtoList,
          dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled());
        if (CollectionUtils.isNotEmpty(bundleRecipeUpdateList)) {
          saveOperationService.saveNewlyAddedItems(bundleRecipeUpdateList);
          itemService.updateRecipeForSharedProducts(mandatoryRequestParam.getStoreId(), bundleRecipeUpdateList);
          saveAndPublishService.publishProductBundleOneToOneMappingEvent(bundleRecipeUpdateList);
          itemPickupPointSummaryService.updateExternalHistoryInPBP(auditTrailDtoList);
        }
      }
    }

    return itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        itemSkuAndItemMap, CollectionUtils.isNotEmpty(itemPickupPointUpdateRequestVo.getBundleRecipeRequests()),
        dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled());
  }

  private void updateBundleRecipeInSharedProduct(String storeId, List<Item> bundleRecipeUpdateList, boolean readFromPrimary) {
    if (CollectionUtils.isNotEmpty(bundleRecipeUpdateList)) {
      Map<String, Item> itemMap = bundleRecipeUpdateList.stream()
          .collect(Collectors.toMap(Item::getItemCode, Function.identity(), (v1, v2) -> v2));

      List<Pair<String, BundleRecipeRequest>> bundlePair = itemMap.entrySet().stream().map(
          entry -> Pair.of(entry.getKey(), new BundleRecipeRequest(entry.getValue().getItemSku(),
              entry.getValue().getBundleRecipe().stream()
                  .map(bundleRecipe -> new BundleRecipeVo(bundleRecipe.getItemSku(), bundleRecipe.getQuantity()))
                  .collect(Collectors.toSet())))).collect(Collectors.toList());

      itemService.addRecipeToOtherSisterProductForSharedProduct(storeId, bundlePair, readFromPrimary);
    }
  }


  private Product getProduct(MandatoryRequestParam mandatoryRequestParam, String productSku, boolean primaryReadEnabled) {
    return dataSourceWrapperService.getProductByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(), productSku,
      primaryReadEnabled);
  }

  private void bundleRecipeUpdate(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Product product,
      Map<String, Item> itemSkuAndItemMap, List<Item> bundleRecipeUpdateList, List<AuditTrailDto> auditTrailDtoList,
      boolean readFromPrimary) {
    String oldValue = StringUtils.EMPTY;
    String newValue = StringUtils.EMPTY;
    for (BundleRecipeRequest bundleRecipeRequest : itemPickupPointUpdateRequestVo.getBundleRecipeRequests()) {
      Item item = getItem(product, bundleRecipeRequest.getItemSku(), itemSkuAndItemMap,
        readFromPrimary);
      if (Objects.nonNull(item)) {
        Set<BundleRecipe> bundleRecipeSet = Optional.ofNullable(item.getBundleRecipe()).orElse(new HashSet<>());
        oldValue = convertBundleRecipeListToHistory(bundleRecipeSet);
        Set<BundleRecipeVo> addBundleRecipeVoRequest =
            Optional.ofNullable(bundleRecipeRequest.getBundleRecipe()).orElse(new HashSet<>());
        Set<BundleRecipe> addBundleRecipeRequest =
            createAddBundleRecipeRequest(addBundleRecipeVoRequest, product.getStoreId(), readFromPrimary);
        Set<BundleRecipe> finalBundleRecipe = mergeBundleRecipes(bundleRecipeSet, addBundleRecipeRequest);
        item.setBundleRecipe(finalBundleRecipe);
        newValue = convertBundleRecipeListToHistory(finalBundleRecipe);
        bundleRecipeUpdateList.add(item);
        if (!StringUtils.equals(oldValue, newValue)) {
          AuditTrailDto auditTrailDto = new AuditTrailDto();
          auditTrailDto.setProductSku(product.getProductSku());
          auditTrailDto.setBusinessPartnerCode(product.getMerchantCode());
          auditTrailDto.setActionKey(BUNDLE_RECIPE_CHANGE);
          auditTrailDto.setGdnSku(item.getItemSku());
          auditTrailDto.setName(item.getGeneratedItemName());
          auditTrailDto.setOldValue(oldValue);
          auditTrailDto.setNewValue(newValue);
          auditTrailDto.setPickupPointCode(Constants.HYPHEN);
          auditTrailDtoList.add(auditTrailDto);
        }
      }
    }
  }

  public String convertBundleRecipeListToHistory(Set<BundleRecipe> productBundleRecipes) {
    if (CollectionUtils.isNotEmpty(productBundleRecipes)) {
      StringBuilder history = new StringBuilder();
      for (BundleRecipe recipe : productBundleRecipes) {
        history.append(recipe.getQuantity()).append('*').append(recipe.getItemSku()).append(" + ");
      }
      history.setLength(history.length() - 3);
      return history.toString();
    } else {
      return Constants.HYPHEN;
    }
  }

  private Item getItem(Product product, String itemSku, Map<String, Item> itemSkuAndItemMap, boolean readFromPrimary) {
    return itemPickupPointSummaryServiceImpl.getItem(product.getStoreId(), itemSku, itemSkuAndItemMap, readFromPrimary);
  }

  private Set<BundleRecipe> createAddBundleRecipeRequest(Set<BundleRecipeVo> addBundleRecipeVoRequest, String storeId, boolean readFromPrimary) {
    Set<BundleRecipe> addBundleRecipeRequest = new HashSet<>();
    Set<String> itemSkuList =
        addBundleRecipeVoRequest.stream().map(BundleRecipeVo::getItemSku).collect(Collectors.toSet());
    List<Item> items = dataSourceWrapperService.findItemByStoreIdAndItemSkus(storeId, itemSkuList, readFromPrimary);
    Map<String, String> itemskuAvaliblityMap =
        Optional.ofNullable(items).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
            .collect(Collectors.toMap(Item::getItemSku, Item::getItemCode));
    for (BundleRecipeVo bundleRecipeVo : addBundleRecipeVoRequest) {
      if (itemskuAvaliblityMap.containsKey(bundleRecipeVo.getItemSku())) {
        BundleRecipe bundleRecipe = new BundleRecipe();
        bundleRecipe.setItemSku(bundleRecipeVo.getItemSku());
        bundleRecipe.setQuantity(bundleRecipeVo.getQuantity());
        addBundleRecipeRequest.add(bundleRecipe);
      }
    }
    return addBundleRecipeRequest;
  }

  private Set<BundleRecipe> mergeBundleRecipes(Set<BundleRecipe> bundleRecipeSet,
      Set<BundleRecipe> addBundleRecipeRequest) {
    if (CollectionUtils.isNotEmpty(addBundleRecipeRequest)) {
      return addBundleRecipeRequest;
    } else {
      return bundleRecipeSet;
    }
  }

  @Override
  public List<ItemPickupPointPriceResponse> findPriceDetailsByItemSkuAndPickupPointCode(
    String storeId, List<ItemPickupPointRequest> itemPickupPointRequests) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    ProductAndItemsUtil.validateItemPickupPointRequest(itemPickupPointRequests);
    checkArgument(itemPickupPointRequests.size() < itemPickupPointFetchSize,
      String.format(ErrorMessages.ITEM_PICKUP_POINT_REQUEST_BEYOND_MAX_FETCH_SIZE,
        itemPickupPointRequests.size(), itemPickupPointFetchSize));
    List<ItemPickupPoint> response = new ArrayList<>();
    itemPickupPointRequests.forEach(itemPickupPointRequest -> {
      ItemPickupPoint itemPickupPoint =
        itemPickupPointService.findByItemSkuAndPickupPointCode(storeId,
          itemPickupPointRequest.getItemSku(), itemPickupPointRequest.getPickupPointCode());
      if (Objects.nonNull(itemPickupPoint) && !itemPickupPoint.isMarkForDelete()) {
        response.add(itemPickupPoint);
      }
    });
    return ResponseHelper.toPriceDetailResponse(response, cncForWarehouseFeatureSwitch);
  }

  @Override
  public List<DeleteItemPickupPointResponse> deleteItemPickupPointsByPickupPointCode(String storeId,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest) throws Exception {
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList = new ArrayList<>();
    Product product = productService.getProductDeletedOrUndeleted(storeId,
        deleteItemPickupPointRequest.getProductSku());
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(storeId,
            deleteItemPickupPointRequest.getProductSku(), deleteItemPickupPointRequest.getPickupPointCode());
    Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap =
        itemPickupPointList.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    List<String> itemSkus = itemPickupPointList.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toList());
    Map<String, Long> itemSkuToActiveL5CountMap = itemSkus.stream().collect(Collectors.toMap(itemSku -> itemSku,
        itemSku -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku)));
    Map<Integer, List<String>> deleteStatusItemSkuMap = new HashMap<>();
    getDeleteStatusMap(itemSkus, itemSkuToActiveL5CountMap, deleteStatusItemSkuMap);
    updatePPCodeAndDeleteItemPickupPoints(storeId, deleteItemPickupPointRequest, auditTrailDtoList,
        deleteItemPickupPointResponseList, product, itemSkuToItemPickupPointMap, itemSkus,
        deleteStatusItemSkuMap);
    log.info("Response for delete pickup point for productSku : {} and ppCode : {} , response : {}",
        deleteItemPickupPointRequest.getProductSku(), deleteItemPickupPointRequest.getPickupPointCode(),
        deleteItemPickupPointResponseList);
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
      auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
      auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
      auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
      auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
      auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
      auditTrailListResponse.setUpdateDirectly(true);
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY,auditTrailListResponse);
    }
    return deleteItemPickupPointResponseList;
  }

  private void updatePPCodeAndDeleteItemPickupPoints(String storeId,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest, List<AuditTrailDto> auditTrailDtoList,
      List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList, Product product,
      Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap, List<String> itemSkus,
      Map<Integer, List<String>> deleteStatusItemSkuMap) throws Exception {
    Set<Item> updatedItemSet = new HashSet<>();
    boolean updateFbbFalseAtL3 = updatePickupPointAndUpdateItems(storeId, deleteItemPickupPointRequest, auditTrailDtoList,
        deleteItemPickupPointResponseList, product, itemSkuToItemPickupPointMap, itemSkus, deleteStatusItemSkuMap,
        updatedItemSet);
    updateProduct(product, updatedItemSet,
        deleteItemPickupPointResponseList, deleteItemPickupPointRequest, updateFbbFalseAtL3);
  }

  private boolean updatePickupPointAndUpdateItems(String storeId, DeleteItemPickupPointRequest deleteItemPickupPointRequest,
      List<AuditTrailDto> auditTrailDtoList, List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList,
      Product product, Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap, List<String> itemSkus,
      Map<Integer, List<String>> deleteStatusItemSkuMap, Set<Item> updatedItemSet) throws Exception {
    Long fbbActivatedCountForProductSku = 0L;
    if(product.isFbbActivated()) {
      fbbActivatedCountForProductSku =
          itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(storeId,
              product.getProductSku(), true);
    }
    Integer totalFbbL5MadeFalse = 0;
    for (String itemSku : itemSkus) {
      Item item = itemService.findByStoreIdAndItemSku(storeId, itemSku);
      totalFbbL5MadeFalse += deleteAndUpdatePPCodeForItemPickupPoints(storeId, deleteItemPickupPointRequest, auditTrailDtoList,
          deleteItemPickupPointResponseList, product, itemSkuToItemPickupPointMap, deleteStatusItemSkuMap, item);
      updateCncAtItemLevel(deleteStatusItemSkuMap, updatedItemSet, item);
    }
    if(totalFbbL5MadeFalse >= fbbActivatedCountForProductSku) {
      return true;
    }
    return false;
  }

  private static void updateCncAtItemLevel(Map<Integer, List<String>> deleteStatusItemSkuMap, Set<Item> updatedItemSet,
      Item item) {
    if (deleteStatusItemSkuMap.get(0).contains(item.getItemSku())) {
      if (item.isCncActivated()) {
        item.setCncActivated(false);
        updatedItemSet.add(item);
      }
    }
  }

  private void updateProduct(Product product, Set<Item> updatedItemSet,
      List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest, boolean updateFbbFalseAtL3) {
    boolean cncChangedAtL3Level = false;
    boolean fbbChangedAtL3Level = false;
    if (CollectionUtils.isNotEmpty(updatedItemSet)) {
      cncChangedAtL3Level =
          itemPickupPointSummaryService.setCncActiveAtProductLevel(product.getProductSku(), updatedItemSet, product,
            false);
      itemService.saveItems(new ArrayList<>(updatedItemSet));
    }
    fbbChangedAtL3Level = isFbbChangedAtL3Level(product, updateFbbFalseAtL3);
    boolean pickupPointDeleteSuccess = true;
    boolean pickupPointUpdated = false;
    for (DeleteItemPickupPointResponse deleteItemPickupPointResponse : deleteItemPickupPointResponseList) {
      if (StringUtils.isNotEmpty(deleteItemPickupPointResponse.getReason())) {
        pickupPointDeleteSuccess = false;
      }
      if (StringUtils.isNotEmpty(deleteItemPickupPointResponse.getNewPickupPointCode())) {
        pickupPointUpdated = true;
      }
    }
    if (pickupPointDeleteSuccess) {
      Set<String> pickupPointCodes = product.getPickupPointCodes();
      pickupPointCodes.remove(deleteItemPickupPointRequest.getPickupPointCode());
      product.setPickupPointCodes(new HashSet<>(pickupPointCodes));
    }
    if (pickupPointUpdated) {
      product.getPickupPointCodes().add(deleteItemPickupPointRequest.getDefaultPickupPointCode());
    }
    log.info(
        "cncChangedAtL3Level : {} fbbChangedAtL3Level : {} pickupPointDeleteSuccess : {} pickupPointUpdated : {} for productSku {}",
        cncChangedAtL3Level, fbbChangedAtL3Level, pickupPointDeleteSuccess, pickupPointUpdated,
        product.getProductSku());
    saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level, pickupPointDeleteSuccess, pickupPointUpdated);
    saveAndPublishService.publishSolrUpdateEvent(Collections.singletonList(
        objectConverterService.convertToProductAndItemEventModel(
            new ProductAndItemsVO(product, new ArrayList<>(updatedItemSet)))));
  }

  public void saveProduct(Product product, boolean cncChangedAtL3Level, boolean fbbChangedAtL3Level,
      boolean pickupPointDeleteSuccess, boolean pickupPointUpdated) {
    if (cncChangedAtL3Level || fbbChangedAtL3Level || pickupPointDeleteSuccess || pickupPointUpdated) {
      productService.saveProductWithoutUpdatingSolr(product, (pickupPointDeleteSuccess || pickupPointUpdated) ?
          Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE) :
          Collections.EMPTY_LIST, Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
    }
  }

  public boolean isFbbChangedAtL3Level(Product product, boolean updateFbbFalseAtL3) {
    if (product.isFbbActivated() && updateFbbFalseAtL3) {
      product.setFbbActivated(false);
      return true;
    }
    return false;
  }

  private int deleteAndUpdatePPCodeForItemPickupPoints(String storeId, DeleteItemPickupPointRequest deleteItemPickupPointRequest,
      List<AuditTrailDto> auditTrailDtoList, List<DeleteItemPickupPointResponse> deleteItemPickupPointResponseList,
      Product product, Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap,
      Map<Integer, List<String>> deleteStatusItemSkuMap, Item item) throws Exception {
    int fbbTrueL5Updated = 0;
    if (deleteStatusItemSkuMap.get(1).contains(item.getItemSku())) {
      List<ItemPickupPoint> itemPickupPointListToBeUpdated =
          Collections.singletonList(itemSkuToItemPickupPointMap.get(item.getItemSku()));
      if (itemPickupPointListToBeUpdated.get(0).isFbbActivated()) {
        fbbTrueL5Updated = 1;
      }
      //delete first from x-inventory
      if (deleteItemPickupPointFromInventory) {
        try {
          inventoryOutbound.deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(),
              CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(itemPickupPointListToBeUpdated));
        } catch (Exception e) {
          log.error("Exception occurred with message : {} ", ErrorMessages.ERROR_WHILE_DELETING_PP_CODE_FROM_INVENTORY,
              e);
        }
      }

      //then delete it from x-product
      itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), storeId, Constants.DEFAULT_USERNAME,
          true, item, itemPickupPointListToBeUpdated, true);
      itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
          objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPointListToBeUpdated.get(0),
              false), Collections.EMPTY_MAP);
      auditTrailDtoList.add(new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), item.getItemSku(),
          Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY,
          null, product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false));
    } else if (deleteStatusItemSkuMap.get(0).contains(item.getItemSku())) {
      if (StringUtils.isNotEmpty(deleteItemPickupPointRequest.getDefaultPickupPointCode())) {
        ItemPickupPoint oldItemPickupPoint = itemSkuToItemPickupPointMap.get(item.getItemSku());
        if (oldItemPickupPoint.isFbbActivated() && !deleteItemPickupPointRequest.isDefaultPickupPointFbbActive()) {
          fbbTrueL5Updated = 1;
        }
        oldItemPickupPoint.setFbbActivated(deleteItemPickupPointRequest.isDefaultPickupPointFbbActive());
        oldItemPickupPoint.getItemViewConfig().forEach(itemViewConfig -> {
          itemViewConfig.setBuyable(false);
          itemViewConfig.setDiscoverable(false);
        });
        Map<String, ItemPickupPoint> evictItemPickupPointCacheMap =
            itemService.updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(),
                itemSkuToItemPickupPointMap.get(item.getItemSku()), item, new HashMap<>(), false, null, false);
        auditTrailDtoList.add(
            new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), item.getItemSku(),
                Constants.PICKUP_POINT_UPDATED, deleteItemPickupPointRequest.getPickupPointCode(),
                deleteItemPickupPointRequest.getDefaultPickupPointCode(), null, product.getProductSku(),
                item.getGeneratedItemName(), deleteItemPickupPointRequest.getPickupPointCode(), false));
        evictItemPickupPointCacheMap.get(CommonUtil.generatePickupPointKey(oldItemPickupPoint.getItemSku(),
            oldItemPickupPoint.getPickupPointCode())).setMarkForDelete(true);
        List<ItemPickupPoint> savedItemPickupPoints =
            this.itemPickupPointService.saveItemPickupPoint(new ArrayList<>(evictItemPickupPointCacheMap.values()));
        for (ItemPickupPoint itemPickupPoint : savedItemPickupPoints) {
          if (CommonUtil.generatePickupPointKey(item.getItemSku(),
              deleteItemPickupPointRequest.getDefaultPickupPointCode()).equals(
              CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()))) {
            itemPickupPoint.setNewData(true);
          }
        }
        saveAndPublishService.publishItemPickupPointDataChangeEvent(savedItemPickupPoints, new ArrayList<>(),
          Collections.EMPTY_MAP);
        evictItemPickupPointCacheMap.values().forEach(
            itemPickupPoint -> cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
                itemPickupPoint, itemPickupPoint.getPickupPointCode()));
        DeleteItemPickupPointResponse deleteItemPickupPointResponse = new DeleteItemPickupPointResponse();
        deleteItemPickupPointResponse.setItemSku(item.getItemSku());
        deleteItemPickupPointResponse.setPickupPointCode(deleteItemPickupPointRequest.getPickupPointCode());
        deleteItemPickupPointResponse.setNewPickupPointCode(deleteItemPickupPointRequest.getDefaultPickupPointCode());
        deleteItemPickupPointResponseList.add(deleteItemPickupPointResponse);
      } else {
        DeleteItemPickupPointResponse deleteItemPickupPointResponse = new DeleteItemPickupPointResponse();
        deleteItemPickupPointResponse.setItemSku(item.getItemSku());
        deleteItemPickupPointResponse.setPickupPointCode(deleteItemPickupPointRequest.getPickupPointCode());
        deleteItemPickupPointResponse.setReason(ErrorMessages.DELETE_PICKUPPOINT_NOT_ALLOWED_DEFAULT_PICKUP_POINT_EMPTY);
        deleteItemPickupPointResponseList.add(deleteItemPickupPointResponse);
      }
    }
    return fbbTrueL5Updated;
  }

  private void archiveProductAndDeleteL5s(String storeId, DeleteItemPickupPointRequest deleteItemPickupPointRequest,
      List<AuditTrailDto> auditTrailDtoList, List<Item> updatedItems, Product product,
      Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap, Item item) {
    List<ItemPickupPoint> itemPickupPointListToBeUpdated =
        Collections.singletonList(itemSkuToItemPickupPointMap.get(item.getItemSku()));
    if (item.isArchived()) {
      itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), storeId, Constants.DEFAULT_USERNAME,
          true, item, itemPickupPointListToBeUpdated, true);
      if (product.isSuspended()) {
        if (Boolean.TRUE != item.getArchivedBeforeSuspension()) {
          item.setArchivedBeforeSuspension(true);
          updatedItems.add(saveOperationService.saveItemWithoutUpdatingSolr(item, itemPickupPointListToBeUpdated, false, StringUtils.EMPTY, Collections.EMPTY_MAP));
        }
      } else {
          itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
              objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPointListToBeUpdated.get(0),
                  false), Collections.EMPTY_MAP);
      }
    } else {
      itemService.doArchivalAction(item, storeId, Constants.DEFAULT_USERNAME, true, itemPickupPointListToBeUpdated,
          true);
      updatedItems.add(saveOperationService.saveItemWithoutUpdatingSolr(item, itemPickupPointListToBeUpdated, false, StringUtils.EMPTY, Collections.EMPTY_MAP));
    }
    auditTrailDtoList.add(new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), item.getItemSku(),
        Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
        product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false));
  }

  private boolean isArchiveProduct(String storeId, DeleteItemPickupPointRequest deleteItemPickupPointRequest,
      boolean isArchiveProduct, Map<Integer, List<String>> deleteStatusItemSkuMap) {
    long countOfActiveL5ForProduct =
        itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
            deleteItemPickupPointRequest.getProductSku());
    if (countOfActiveL5ForProduct == deleteStatusItemSkuMap.get(0).size()) {
      isArchiveProduct = true;
      deleteStatusItemSkuMap.put(0, new ArrayList<>());
    }
    return isArchiveProduct;
  }

  private static void getDeleteStatusMap(List<String> itemSkus, Map<String, Long> itemSkuToActiveL5CountMap,
      Map<Integer, List<String>> deleteStatusItemSkuMap) {
    deleteStatusItemSkuMap.put(0, new ArrayList<>());
    deleteStatusItemSkuMap.put(1, new ArrayList<>());
    for (String itemSku : itemSkus) {
      if (itemSkuToActiveL5CountMap.get(itemSku) == 1) {
        List<String> failedDeleteItemSkus = deleteStatusItemSkuMap.get(0);
        failedDeleteItemSkus.add(itemSku);
        //0-not eligible to delete
        deleteStatusItemSkuMap.put(0, failedDeleteItemSkus);
      } else {
        List<String> deleteItemSkus = deleteStatusItemSkuMap.get(1);
        deleteItemSkus.add(itemSku);
        //1-eligible to delete
        deleteStatusItemSkuMap.put(1, deleteItemSkus);
      }
    }
  }

  @Override
  public CreateFbbPickupPointResponse createFbbPickupPoint(String storeId,
      CreateFbbPickupPointRequest createFbbPickupPointRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(createFbbPickupPointRequest.getBusinessPartnerCode()),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(createFbbPickupPointRequest.getItemSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(createFbbPickupPointRequest.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    Item item =
        itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, createFbbPickupPointRequest.getItemSku());
    GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_NOT_FOUND);
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setItemSku(createFbbPickupPointRequest.getItemSku());
    createFbbPickupPointResponse.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    createFbbPickupPointResponse.setProductSku(item.getProductSku());
    createFbbPickupPointResponse.setItemCode(item.getItemCode());
    try {
      createFbbPickupPointResponse =
          itemPickupPointService.createFbbPickupPoint(storeId, createFbbPickupPointRequest, item,
              createFbbPickupPointResponse);
      if (StringUtils.isNotBlank(createFbbPickupPointResponse.getReason())) {
        return createFbbPickupPointResponse;
      }
      Product product =
          productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, item.getProductSku());
      createFbbPickupPointResponse.setProductCode(product.getProductCode());
      boolean productUpdated = false;
      if (!product.isFbbActivated()) {
        product.setFbbActivated(true);
        productUpdated = true;
      }
      Set<String> existingProductPickupPointCodes = product.getPickupPointCodes();
      if (!existingProductPickupPointCodes.contains(createFbbPickupPointRequest.getPickupPointCode())) {
        existingProductPickupPointCodes.add(createFbbPickupPointRequest.getPickupPointCode());
        productUpdated = true;
      }
      if (productUpdated) {
        saveOperationService.saveProduct(product);
      }
    } catch (Exception e) {
      log.error("Error while creating FBB pickupPoint with itemSku = {} , pickupPointCode = {} ",
          createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), e);
      createFbbPickupPointResponse.setReason(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage());
      createFbbPickupPointResponse.setErrorCode(ProductErrorCodesEnum.INTERNAL_SERVER.getCode());
    }
    return createFbbPickupPointResponse;
  }

  @Override
  public List<ProductL5DetailResponse> findProductL5DetailByItemSkusAndPickupPointCode(
    String storeId, List<ItemPickupPointRequest> itemPickupPointRequests, boolean inAllProducts,
      String fetchViewConfigByChannel) throws Exception {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    ProductAndItemsUtil.validateItemPickupPointRequest(itemPickupPointRequests);
    List<ItemPickupPoint> itemPickupPointList =
      itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(storeId,
        itemPickupPointRequests, inAllProducts);
    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      Map<String, Item> itemMap =
        fetchItemByItemSkusAndMarkForDeleteFalse(storeId, itemPickupPointList, inAllProducts);
      Map<String, Product> productMap =
        fetchProductByProductSkusAndMarkForDeleteFalse(storeId, itemPickupPointList, inAllProducts);
      if (MapUtils.isNotEmpty(itemMap) && MapUtils.isNotEmpty(productMap)) {
        setCurrentItemViewConfig(itemPickupPointList, itemMap, null, new HashMap<>());
      }
      return ResponseHelper.toProductDetailL5Response(itemPickupPointList, productMap, itemMap,
          cncForWarehouseFeatureSwitch, fetchViewConfigByChannel);
    }
    log.info("No item pickup point found with request : {} ", itemPickupPointRequests);
    return new ArrayList<>();
  }

  @Override
  public AutoCreatePickupPointListResponse autoCreatePickupPoint(
      AutoCreatePickupPointRequestList autoCreatePickupPointRequestList, MandatoryRequestParam mandatoryRequestParam) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(mandatoryRequestParam.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    AutoCreatePickupPointListResponse autoCreatePickupPointListResponse = new AutoCreatePickupPointListResponse();
    for (AutoCreatePickupPointRequest autoCreatePickupPointRequest : autoCreatePickupPointRequestList.getAutoCreatePickupPointRequests()) {
      try {
        AutoCreatePickupPointResponse autoCreatePickupPointResponse = new AutoCreatePickupPointResponse();
        autoCreatePickupPointResponse.setMerchantCode(autoCreatePickupPointRequest.getMerchantCode());
        autoCreatePickupPointResponse.setWebItemSku(autoCreatePickupPointRequest.getWebItemSku());
        autoCreatePickupPointResponse.setPickupPointCode(autoCreatePickupPointRequest.getPickupPointCode());
        ValidationUtil.checkParameter(StringUtils.isNotBlank(autoCreatePickupPointRequest.getWebProductSku()),
            ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
        ValidationUtil.checkParameter(StringUtils.isNotBlank(autoCreatePickupPointRequest.getWebItemSku()),
            ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
        ValidationUtil.checkParameter(StringUtils.isNotBlank(autoCreatePickupPointRequest.getPickupPointCode()),
            ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
        Product product =
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
                autoCreatePickupPointRequest.getWebProductSku());
        ValidationUtil.checkParameter(Objects.nonNull(product), ApiErrorCodes.INVALID_PRODUCT.getErrorMessage());
        ValidationUtil.validateProduct(product);
        Item item = itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku());
        ValidationUtil.checkParameter(Objects.nonNull(item), ApiErrorCodes.ITEM_DELETED.getErrorMessage());
        itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
            autoCreatePickupPointResponse, product.getProductType());
        boolean productChanged = false;
        if (!product.isFbbActivated()) {
          product.setFbbActivated(true);
          productChanged = true;
        }
        if (!product.getPickupPointCodes().contains(autoCreatePickupPointRequest.getPickupPointCode())) {
          product.getPickupPointCodes().add(autoCreatePickupPointRequest.getPickupPointCode());
          productChanged = true;
        }
        if (productChanged) {
          saveOperationService.saveProduct(product);
        }
        autoCreatePickupPointResponse.setSuccess(true);
        autoCreatePickupPointListResponse.getData().add(autoCreatePickupPointResponse);
      } catch (ApiIncorrectInputDataException e) {
        getFailedAutoCreatePickupPointResponse(autoCreatePickupPointListResponse, autoCreatePickupPointRequest,
            e.getErrorMessage());
      } catch (ApplicationRuntimeException e) {
        getFailedAutoCreatePickupPointResponse(autoCreatePickupPointListResponse, autoCreatePickupPointRequest,
            e.getErrorMessage());
      } catch (Exception e) {
        getFailedAutoCreatePickupPointResponse(autoCreatePickupPointListResponse, autoCreatePickupPointRequest,
            e.getMessage());
      }
    }
    return autoCreatePickupPointListResponse;
  }

  private void getFailedAutoCreatePickupPointResponse(
      AutoCreatePickupPointListResponse autoCreatePickupPointListResponse,
      AutoCreatePickupPointRequest autoCreatePickupPointRequest, String errorMessage) {
    AutoCreatePickupPointResponse autoCreatePickupPointResponse = new AutoCreatePickupPointResponse();
    autoCreatePickupPointResponse.setWebItemSku(autoCreatePickupPointRequest.getWebItemSku());
    autoCreatePickupPointResponse.setPickupPointCode(autoCreatePickupPointRequest.getPickupPointCode());
    autoCreatePickupPointResponse.setMerchantCode(autoCreatePickupPointRequest.getMerchantCode());
    autoCreatePickupPointResponse.setSuccess(false);
    autoCreatePickupPointResponse.setErrorMessage(errorMessage);
    autoCreatePickupPointListResponse.getData().add(autoCreatePickupPointResponse);

  }

  @Override
  public List<ItemSkuPickupPointCodeResponse> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId,
      List<String> itemSkus) {
    return itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(storeId, itemSkus);
  }

  @Override
  public List<ItemPickupPointBasicResponse> fetchBasicDetailsByItemSkuAndPickupPointCodeList(
    String storeId, List<ItemPickupPointRequest> itemPickupPointRequest) {
    List<ItemPickupPoint> itemPickupPoints =
      itemPickupPointService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(storeId,
        itemPickupPointRequest);
    return ResponseHelper.toItemPickupPointBasicResponse(itemPickupPoints);
  }

  @Override
  public List<EanUpcPickupPointCodeResponse> fetchItemDetailsByEanUpcCode(String storeId,
      EanUpcPickupPointCodeRequest eanUpcPickupPointCodeRequest, String fetchViewConfigByChannel) {
    ValidationUtil.checkParameter(
        StringUtils.isNotEmpty(eanUpcPickupPointCodeRequest.getEanUpcCode()),
        ErrorMessages.EANUPC_CODE_MUST_NOT_BE_BLANK);
    Set<String> requestedMerchantCodes =
        eanUpcPickupPointCodeRequest.getPickupPointDetails().stream()
            .map(PickupPointDetailsDTO::getBusinessPartnerCode).filter(Objects::nonNull)
            .collect(Collectors.toSet());
    List<Item> items =
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(
            storeId, eanUpcPickupPointCodeRequest.getEanUpcCode(), requestedMerchantCodes);
    Map<String, List<String>> businessPartnerAndPpMap =
        groupByBusinessPartnerCode(eanUpcPickupPointCodeRequest.getPickupPointDetails());
    Set<String> offlineItemIds = getOfflineItemIds(items, businessPartnerAndPpMap);
    List<ItemPickupPoint> itemPickupPoint =
        getItemPickupPointsBasedOnChannel(storeId, fetchViewConfigByChannel, offlineItemIds);
    Map<String, String> itemSkuItemNameMap = getItemSkuItemNameMap(items);
    return ResponseHelper.getEanUpcPickupPointCodeResponse(itemPickupPoint, itemSkuItemNameMap);
  }

  private List<ItemPickupPoint> getItemPickupPointsBasedOnChannel(String storeId, String fetchViewConfigByChannel,
      Set<String> offlineItemIds) {
    List<ItemPickupPoint> itemPickupPoint;
    if (StringUtils.isNotBlank(fetchViewConfigByChannel) && !Constants.ALL.equals(fetchViewConfigByChannel)) {
      itemPickupPoint =
          itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(storeId,
              offlineItemIds, true, fetchViewConfigByChannel, Integer.parseInt(
                  systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
                      SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT).getValue()));
    } else {
      itemPickupPoint =
          itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(storeId,
              offlineItemIds, true, StringUtils.EMPTY, Integer.parseInt(
                  systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
                      SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT).getValue()));
    }
    return itemPickupPoint;
  }

  @Override
  public Page<ItemPickupPointL5Response> findItemPickupPointsByItemSkus(String storeId, SimpleListStringRequest simpleListStringRequest, int page,
      int size, String fetchViewConfigByChannel) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(simpleListStringRequest.getValue()),
        ErrorMessages.ITEM_LIST_NOT_EMPTY);
    Page<ItemPickupPoint> itemPickupPointByItemSkus =
        itemPickupPointService.findItemPickupPointByProductSkusOrItemSkusForFbb(storeId, new ArrayList<>(),
            simpleListStringRequest.getValue(), simpleListStringRequest.isFbbActivated(), page, size);
    List<ItemPickupPointL5Response> itemPickupPointL5Response;
    if (Objects.nonNull(itemPickupPointByItemSkus) && CollectionUtils.isNotEmpty(itemPickupPointByItemSkus.getContent())) {
      itemPickupPointL5Response = getItemPickupPointL5Responses(storeId, itemPickupPointByItemSkus,
          fetchViewConfigByChannel);
      return new PageImpl<>(itemPickupPointL5Response, PageRequest.of(page, size),
          itemPickupPointByItemSkus.getTotalElements());
    } else {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
  }

  private List<ItemPickupPointL5Response> getItemPickupPointL5Responses(String storeId,
      Page<ItemPickupPoint> itemPickupPointByItemSkus, String fetchViewConfigByChannel) {
    Map<String, BusinessPartnerPickupPoint> pickupPointMap =
        getBusinessPartnerPickupPointMap(storeId, itemPickupPointByItemSkus);
    return ResponseHelper.toItemPickupPointL5Response(itemPickupPointByItemSkus.getContent(),
        pickupPointMap, cncForWarehouseFeatureSwitch, fetchViewConfigByChannel);
  }

  private Map<String, BusinessPartnerPickupPoint> getBusinessPartnerPickupPointMap(String storeId,
      Page<ItemPickupPoint> itemPickupPointByItemSkus) {
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointByPickupPointCodes =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
            itemPickupPointByItemSkus.stream().map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toList()));
    return businessPartnerPickupPointByPickupPointCodes.stream()
        .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, Function.identity()));
  }

  private Map<String, List<String>> groupByBusinessPartnerCode(
      List<PickupPointDetailsDTO> pickupPointDetailsDTOS) {
    return pickupPointDetailsDTOS.stream().filter(Objects::nonNull)
        .filter(dto -> StringUtils.isNotBlank(dto.getBusinessPartnerCode()))
        .filter(dto -> StringUtils.isNotBlank(dto.getPickupPointCode())).collect(
            Collectors.groupingBy(PickupPointDetailsDTO::getBusinessPartnerCode,
                Collectors.mapping(PickupPointDetailsDTO::getPickupPointCode,
                    Collectors.toList())));
  }


  private Set<String> getOfflineItemIds(List<Item> items,
      Map<String, List<String>> merchantCodeAndPpMap) {
    return items.stream().flatMap(item -> generateOfflineItemIds(item,
            merchantCodeAndPpMap.getOrDefault(item.getMerchantCode(), List.of())).stream())
        .collect(Collectors.toSet());
  }

  private Collection<String> generateOfflineItemIds(Item item, List<String> pickupPoints) {
    return pickupPoints.stream()
        .map(pickupPoint -> CommonUtil.generateOfflineItemId(item.getItemSku(), pickupPoint))
        .collect(Collectors.toList());
  }

  private Map<String, String> getItemSkuItemNameMap(List<Item> items) {
    return items.stream().collect(Collectors.toMap(Item::getItemSku, Item::getGeneratedItemName));
  }

  @Override
  public boolean getCncAtL5ByProductSku(String storeId, String productSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointService.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(storeId,
            productSku, true);
    return Objects.nonNull(itemPickupPoint);
  }
}