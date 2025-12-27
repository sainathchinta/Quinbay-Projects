package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.ProductService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.util.CommonUtil;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ItemViewConfigServiceImpl implements ItemViewConfigService {

  private static final String ITEM_NOT_FOUND_WITH_STORE_ID_AND_ITEM_SKU =
      "item not found with storeId:{} and itemSku:{}";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";

  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "Item sku must not be blank";

  private static final String ITEM_NOT_FOUND = "Item not found ";

  private static final String ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL = "itemViewConfig must not be null";

  private static final String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchantCode must not be blank";

  private static final String ITEM_VIEW_CONFIG_WITH_THAT_CHANNEL_ALREADY_EXISTS =
      "itemViewConfig with that channel already exists";
  private static final String CHANNEL_MUST_NOT_BE_NULL = "channel must not be null";

  private static final String INVALID_ITEM_SKU_FORMAT = "Invalid item sku format : ";

  private static final String CHANNEL_DEFAULT_CANNOT_BE_DELETED =
      "channel default cannot be deleted";

  @Autowired
  private ChannelService channelService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Lazy
  @Autowired
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Autowired
  private ProductService productService;

  @Value("${takedown.changetype.enabled}")
  private boolean takeDownSwitch;

  @Value("${set.archived.before.edit.flag}")
  private boolean setArchivedBeforeEditFlag;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public boolean addItemViewConfig(String storeId, String itemSku, ItemViewConfig itemViewConfig) throws Exception {
    Item currItem = checkItemStateWithMarkForDeleteFalse(storeId, itemSku, itemViewConfig);
    checkState(!currItem.getItemViewConfigs().contains(itemViewConfig),
        ItemViewConfigServiceImpl.ITEM_VIEW_CONFIG_WITH_THAT_CHANNEL_ALREADY_EXISTS);
    currItem.getItemViewConfigs().add(itemViewConfig);
    this.saveOperationService.saveItemWithoutUpdatingSolr(currItem, null, false, StringUtils.EMPTY,
      Collections.EMPTY_MAP);
    this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(Arrays.asList(currItem));
    return true;
  }

  @Override
  public boolean addItemViewConfigByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, ItemViewConfig itemViewConfig) {
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemViewConfigServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ItemViewConfigServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(itemViewConfig != null,
        ItemViewConfigServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);

    List<Item> items =
        this.itemService.getItemsByMerchantSkuAndMerchantCode(storeId, merchantSku, merchantCode);
    for (Item item : items) {
      checkState(!item.getItemViewConfigs().contains(itemViewConfig),
          ItemViewConfigServiceImpl.ITEM_VIEW_CONFIG_WITH_THAT_CHANNEL_ALREADY_EXISTS);
      item.getItemViewConfigs().add(itemViewConfig);
      break;
    }
    this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(items, null,StringUtils.EMPTY);
    this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(items);
    return true;
  }

  @Override
  public boolean deleteItemViewConfig(String storeId, String itemSku, String channel)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemViewConfigServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSku(itemSku),
        ItemViewConfigServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    checkArgument(channel != null, ItemViewConfigServiceImpl.CHANNEL_MUST_NOT_BE_NULL);
    checkArgument(!this.channelService.getDefaultChannel().equals(channel),
        ItemViewConfigServiceImpl.CHANNEL_DEFAULT_CANNOT_BE_DELETED);

    Item currItem = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(currItem != null, ProductServiceImpl.ITEM_NOT_FOUND);

    for (ItemViewConfig currItemViewConfig : currItem.getItemViewConfigs()) {
      if (currItemViewConfig.getChannel().equals(channel)) {
        currItem.getItemViewConfigs().remove(currItemViewConfig);
        this.saveOperationService.saveItemWithoutUpdatingSolr(currItem, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
        this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(Collections.singletonList(currItem));
        return true;
      }
    }
    throw new Exception(ProductServiceImpl.CHANNEL_NOT_FOUND);
  }

  @Override
  public boolean updateBuyableSchedule(String storeId, String channel, String itemSku,
      ItemBuyableSchedule itemBuyableSchedule) {
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(item != null,
        String.format(ItemViewConfigServiceImpl.ITEM_NOT_FOUND_WITH_STORE_ID_AND_ITEM_SKU, storeId, itemSku));
    this.saveOperationService.updateItemPickupPointFieldsByItemSku(storeId, itemSku,
        ProductFieldNames.ITEM_PICKUP_POINT_DISPLAYABLE_SCHEDULES, itemBuyableSchedule, item);
    return true;
  }

  @Override
  public boolean updateDefaultBuyable(String storeId, String channel, String itemSku,
      boolean buyable) {
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(item != null,
        String.format(ItemViewConfigServiceImpl.ITEM_NOT_FOUND_WITH_STORE_ID_AND_ITEM_SKU, storeId, itemSku));
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    ItemViewConfig itemViewConfig =
        this.itemHelperService.getItemViewConfigs(itemPickupPoint.getItemViewConfig(), channel);
    itemViewConfig.setBuyable(buyable);
    this.saveOperationService.updateItemFieldByItemSkuAndPickupPoint(storeId, itemSku,
        ProductFieldNames.ITEM_VIEW_CONFIGS, itemPickupPoint.getItemViewConfig(), item);
    this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(Collections.singletonList(item));
    return true;
  }

  @Override
  public boolean updateDefaultDiscoverable(String storeId, String channel, String itemSku,
      boolean discoverable) {
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(item != null,
        String.format(ItemViewConfigServiceImpl.ITEM_NOT_FOUND_WITH_STORE_ID_AND_ITEM_SKU, storeId, itemSku));
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    ItemViewConfig itemViewConfig =
        this.itemHelperService.getItemViewConfigs(itemPickupPoint.getItemViewConfig(), channel);
    itemViewConfig.setDiscoverable(discoverable);
    this.saveOperationService.updateItemFieldByItemSkuAndPickupPoint(storeId, itemSku,
        ProductFieldNames.ITEM_VIEW_CONFIGS, itemPickupPoint.getItemViewConfig(), item);
    this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(Collections.singletonList(item));
    saveAndPublishService.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(item));
    return true;
  }

  @Override
  public boolean updateDiscoverableSchedule(String storeId, String channel, String itemSku,
      ItemDiscoverableSchedule itemDiscoverableSchedule) {
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(item != null,
        String.format(ItemViewConfigServiceImpl.ITEM_NOT_FOUND_WITH_STORE_ID_AND_ITEM_SKU, storeId, itemSku));
    this.saveOperationService.updateItemPickupPointFieldsByItemSku(storeId, itemSku,
        ProductFieldNames.ITEM_PICKUP_POINT_DISCOVERABLE_SCHEDULES, itemDiscoverableSchedule, item);
    return true;
  }

  @Override
  public boolean updateItemViewConfig(String storeId, String itemSku, ItemViewConfig itemViewConfig) {
    Item item = checkItemStateWithMarkForDeleteFalse(storeId, itemSku, itemViewConfig);
    ItemPickupPoint itemPickupPoint = checkItemPickupPointStateWithMarkForDeleteFalse(storeId, itemSku);
    updateProductItemViewConfig(itemViewConfig, item, itemPickupPoint);
    return true;
  }

  private void updateProductItemViewConfig(ItemViewConfig itemViewConfig, Item item, ItemPickupPoint itemPickupPoint) {
    boolean publishL5Event =
        this.productHelperService.updateItemViewConfigForExistingChannelItemPickupPoint(itemPickupPoint,
            itemViewConfig);
    if (publishL5Event) {
      ItemPickupPoint savedItemPickupPoint = this.itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
      cacheEvictHelperService.evictItemPickupPointData(savedItemPickupPoint.getStoreId(), savedItemPickupPoint,
          savedItemPickupPoint.getPickupPointCode());
      cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChangesByItemPickupPoint(
          Collections.singletonList(itemPickupPoint));
    }
    publishMerchantVoucherConfigChangeEvent(itemPickupPoint, item, publishL5Event);
  }

  private void publishMerchantVoucherConfigChangeEvent(ItemPickupPoint itemPickupPoint, Item item,
      boolean publishL5Event) {
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPointVo);
    ItemVo itemVo = new ItemVo();
    BeanUtils.copyProperties(item, itemVo);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    if (publishL5Event) {
      List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
      itemPickupPointList.add(itemPickupPoint);
      saveAndPublishService.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
        Collections.EMPTY_MAP);
    }
    saveAndPublishService.publishMerchantVoucherViewConfigChange(itemVo);
  }

  private Item checkItemStateWithMarkForDeleteFalse(String storeId, String itemSku, ItemViewConfig itemViewConfig) {
    checkItemParameters(storeId, itemSku, itemViewConfig);
    Item item = this.itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    checkState(item != null, ItemViewConfigServiceImpl.ITEM_NOT_FOUND);
    checkState(!item.isForceReview(), CommonUtil.ITEM_NOT_EDITABLE);
    return item;
  }

  private ItemPickupPoint checkItemPickupPointStateWithMarkForDeleteFalse(String storeId, String itemSku) {
    ItemPickupPoint itemPickupPoint = this.itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    checkState(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    return itemPickupPoint;
  }

  private Item checkItemState(String storeId, String itemSku, ItemViewConfig itemViewConfig) {
    checkItemParameters(storeId, itemSku, itemViewConfig);
    Item item = this.itemService.findByStoreIdAndItemSku(storeId, itemSku);
    checkState(item != null, ItemViewConfigServiceImpl.ITEM_NOT_FOUND);
    return item;
  }

  private void checkItemParameters(String storeId, String itemSku, ItemViewConfig itemViewConfig) {
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemViewConfigServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku),
        ItemViewConfigServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(itemViewConfig != null,
        ItemViewConfigServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
  }

  @Override
  public List<Item> updateItemViewConfigAndForceReview(String storeId,
      List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests, boolean forceReview, boolean isArchive,
      String productSku, boolean scheduleRemoval) {
    List<Item> savedItems = new ArrayList<>();
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    if (CollectionUtils.isEmpty(itemViewConfigAndItemSkuRequests) && StringUtils.isNotBlank(productSku)
        && Boolean.TRUE.equals(forceReview)) {
      savedItems = updateItemForceReview(storeId, productSku);
    } else {
      List<Item> itemList = new ArrayList<>();
      Map<String, Item> itemMap = new HashMap<>();
      Map<String, ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelMap = new HashMap<>();
      Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
      Set<String> cncChangeSkus = new HashSet<>();
      for (ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest : itemViewConfigAndItemSkuRequests) {
        boolean pureCNCStatusChange = false;
        boolean isDiscoverableChange = false;
        boolean isCncDiscoverableChange = false;
        boolean isCncChange = false;
        ItemViewConfig itemViewConfig = toItemViewConfig(itemViewConfigAndItemSkuRequest);
        Item item = itemMap.getOrDefault(itemViewConfigAndItemSkuRequest.getItemSku(), null);
        if (Objects.isNull(item)) {
          item = checkItemState(storeId, itemViewConfigAndItemSkuRequest.getItemSku(), itemViewConfig);
          if (item.isPermanentDelete()) {
            continue;
          }
          item.setForceReview(forceReview);
          item.setMarkForDelete(forceReview);
          if(!forceReview){
            item.setNewData(true);
          }
          if (forceReview || isArchive) {
            if (setArchivedBeforeEditFlag) {
              item.setArchivedBeforeEdit(item.isArchived());
            }
            item.setArchived(true);
            List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
            itemChangeEventTypes.add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
            item.setItemChangeEventTypes(itemChangeEventTypes);
          } else {
            if (setArchivedBeforeEditFlag) {
              item.setArchived(item.isArchivedBeforeEdit());
            } else {
              item.setArchived(false);
            }
          }
          itemList.add(item);
          itemMap.put(item.getItemSku(), item);
        }
        ItemPickupPoint itemPickupPoint = itemPickupPointMap.getOrDefault(
            CommonUtil.generatePickupPointKey(item.getItemSku(), itemViewConfigAndItemSkuRequest.getPickupPointCode()),
            itemPickupPointService.findByItemSkuAndPickupPointCode(storeId, item.getItemSku(),
                itemViewConfigAndItemSkuRequest.getPickupPointCode()));
        if (Objects.nonNull(itemPickupPoint)) {
          itemPickupPoint.setForceReview(forceReview);
          if (!cncForWarehouseFeatureSwitch) {
            isDiscoverableChange = CommonUtil.isDiscoverableChanged(itemViewConfig,
                itemPickupPoint.getItemViewConfig().iterator().next());
            pureCNCStatusChange = CommonUtil.isPureCNCStatusChange(itemViewConfig,
                itemPickupPoint.getItemViewConfig().iterator().next(), itemPickupPoint.isCncActive(),
                itemPickupPoint.isCncActive());
          } else {
            if (Constants.CNC.equals(itemViewConfig.getChannel())) {
              ItemViewConfig viewConfig =
                  itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.CNC);
              isCncDiscoverableChange = CommonUtil.isDiscoverableChanged(itemViewConfig, viewConfig);
              isCncChange = itemViewConfig.isBuyable() != viewConfig.isBuyable();
            } else if (Constants.DEFAULT_CHANNEL.equals(itemViewConfig.getChannel())) {
              isDiscoverableChange = CommonUtil.isDiscoverableChanged(itemViewConfig,
                  itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.DEFAULT_CHANNEL));
            }
          }
          ItemPickupPoint updatedItemPickupPoint =
              updateItemViewConfigForExistingChannel(itemPickupPoint, itemViewConfig, cncForWarehouseFeatureSwitch);
          ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
              objectConverterService.convertToItemPickupPointChangeEventModel(updatedItemPickupPoint,
                  pureCNCStatusChange);
          if(!forceReview){
            itemPickupPointDataChangeEventModel.setNewData(Boolean.TRUE);
          }
          List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList =
              itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
          if (isDiscoverableChange) {
            itemPickupPointChangeEventTypeList.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
          }
          if (isCncDiscoverableChange) {
            itemPickupPointChangeEventTypeList.add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
          }
          if (isCncChange) {
            cncChangeSkus.add(itemViewConfigAndItemSkuRequest.getItemSku());
          }
          if (scheduleRemoval) {
            CommonUtil.clearSchedule(auditTrailDtoList, item, itemPickupPoint, itemPickupPointChangeEventTypeList);
          }
          if (forceReview && takeDownSwitch) {
            itemPickupPointChangeEventTypeList.add(ItemPickupPointChangeEventType.TAKE_DOWN);
          }
          itemPickupPointDataChangeEventModelMap.put(
              CommonUtil.generatePickupPointKey(item.getItemSku(), itemViewConfigAndItemSkuRequest.getPickupPointCode()),
              itemPickupPointDataChangeEventModel);
          itemPickupPointMap.put(
              CommonUtil.generatePickupPointKey(item.getItemSku(), itemViewConfigAndItemSkuRequest.getPickupPointCode()),
              itemPickupPoint);
        }
      }

      if (CollectionUtils.isNotEmpty(itemList) && MapUtils.isNotEmpty(itemPickupPointMap)) {
        savedItems = this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemList);
        this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(itemList);
        List<ItemPickupPoint> updatedItemPickupPoints = itemPickupPointService.saveItemPickupPoint(new ArrayList<>(itemPickupPointMap.values()));
        objectConverterService.overrideL4DetailsFromL5(itemList, updatedItemPickupPoints);
        saveAndPublishService.publishMerchantVoucherViewConfigChange(updatedItemPickupPoints, itemList);
        saveAndPublishService.publishListOfItems(itemList, updatedItemPickupPoints,
            new ArrayList<>(itemPickupPointDataChangeEventModelMap.values()), false);
        itemPickupPointSummaryService.updateExternalHistoryInPBP(auditTrailDtoList);
        if (CollectionUtils.isNotEmpty(cncChangeSkus)) {
          savedItems = checkAndUpdateCncL3AndL4(cncChangeSkus, savedItems, storeId);
        }
      }
    }
    return savedItems;
  }

  private List<Item> checkAndUpdateCncL3AndL4(Set<String> cncChangeSkus, List<Item> savedItems,
      String storeId) {
    Map<String, Item> l4Map =
        savedItems.stream().collect(Collectors.toMap(Item::getItemSku, item -> item, (o, o2) -> o));
    List<Item> updateCncFlag = new ArrayList<>();
    cncChangeSkus.forEach(sku -> {
      Item item = l4Map.get(sku);
      if (Objects.nonNull(item)) {
        boolean l4CncFlag =
            itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
                storeId, sku, Constants.CNC);
        if (item.isCncActivated() != l4CncFlag) {
          item.setCncActivated(l4CncFlag);
          updateCncFlag.add(item);
        }
      }
    });
    if (CollectionUtils.isNotEmpty(updateCncFlag)) {
      List<Item> updatedItems =
          this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
              updateCncFlag);
      Map<String, Item> l4UpdatedMap = updatedItems.stream()
          .collect(Collectors.toMap(Item::getItemSku, item -> item, (o, o2) -> o));
      this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(updateCncFlag);
      savedItems =
          savedItems.stream().map(item -> l4UpdatedMap.getOrDefault(item.getItemSku(), item))
              .collect(Collectors.toList());
      checkAndUpdateCncL3(storeId, updatedItems);
    }
    return savedItems;
  }

  private void checkAndUpdateCncL3(String storeId, List<Item> updatedItems) {
    Set<String> l3Skus = updatedItems.stream().map(Item::getProductSku).collect(Collectors.toSet());
    List<Product> products =
        productService.findByStoreIdAndProductSkuIn(storeId, List.copyOf(l3Skus));
    Map<String, Product> l3Map = products.stream()
        .collect(Collectors.toMap(Product::getProductSku, product -> product, (o, o2) -> o));
    List<Product> updateCncFlag = new ArrayList<>();
    l3Skus.forEach(productSku -> {
      Product product = l3Map.get(productSku);
      if (Objects.nonNull(product)) {
        boolean l3CncFlag = itemService.existsRecordForStoreIdAndProductSkuAndCncActivated(storeId, productSku, Boolean.TRUE);
        if (l3CncFlag != product.isCncActivated()) {
          product.setCncActivated(l3CncFlag);
          Product savedProduct = this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
              StringUtils.EMPTY, Collections.EMPTY_MAP);
          updateCncFlag.add(savedProduct);
        }
      }
    });
    if (CollectionUtils.isNotEmpty(updateCncFlag)) {
      productAndItemSolrIndexerService.updateProductDetailsInSolr(updateCncFlag);
    }
  }

  private ItemViewConfig toItemViewConfig(ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest) {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest, itemViewConfig, "itemDiscoverableSchedules",
        "itemBuyableSchedules");
    if (Objects.nonNull(itemViewConfigAndItemSkuRequest.getItemBuyableSchedules())) {
      ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
      BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest.getItemBuyableSchedules(), itemBuyableSchedule);
      itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    }
    if (Objects.nonNull(itemViewConfigAndItemSkuRequest.getItemDiscoverableSchedules())) {
      ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
      BeanUtils.copyProperties(itemViewConfigAndItemSkuRequest.getItemDiscoverableSchedules(), itemDiscoverableSchedule);
      itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule);
    }
    return itemViewConfig;
  }

  private List<Item> updateItemForceReview(String storeId, String productSku) {
    List<Item> items = itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    for (Item item : items) {
      item.setForceReview(true);
      item.setMarkForDelete(true);
    }
    if (CollectionUtils.isNotEmpty(items)) {
      items = this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(items);
      saveAndPublishService.publishListOfItems(items);
    }
    return items;
  }


  private ItemPickupPoint updateItemViewConfigForExistingChannel(ItemPickupPoint itemPickupPoint, ItemViewConfig itemViewConfig,
      boolean cncForWarehouseFeatureSwitch) {
    checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(itemViewConfig), ErrorMessages.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    return CommonUtil.setItemViewConfigFromRequest(itemPickupPoint, itemViewConfig, cncForWarehouseFeatureSwitch);
  }


  /**
   * this api is not used anymore hence setting the list of item pickup points as empty. It's a pre mpp code. We can clean it up later
   */
  @Override
  public boolean updateItemViewConfigByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, ItemViewConfig newItemViewConfig) {
    checkArgument(StringUtils.isNotBlank(storeId),
        ItemViewConfigServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ItemViewConfigServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(newItemViewConfig != null,
        ItemViewConfigServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);

    List<Item> items =
        this.itemService.getItemsByMerchantSkuAndMerchantCode(storeId, merchantSku, merchantCode);
    for (Item item : items) {
      for (ItemViewConfig itemViewConfig : item.getItemViewConfigs()) {
        if (newItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
          itemViewConfig.setBuyable(newItemViewConfig.isBuyable());
          itemViewConfig.setDiscoverable(newItemViewConfig.isDiscoverable());
          itemViewConfig
              .setItemBuyableScheduleIfNotNull(newItemViewConfig.getItemBuyableSchedules());
          itemViewConfig.setItemDiscoverableScheduleIfNotNull(
              newItemViewConfig.getItemDiscoverableSchedules());
          break;
        }
      }
    }
    this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(items, null,StringUtils.EMPTY);
    this.productAndItemSolrIndexerService.updateSolrOnItemViewConfigChanges(items);
    saveAndPublishService.publishMerchantVoucherViewConfigChange(new ArrayList<>(), items);
    return true;
  }

  @Override
  public boolean isItemViewConfigChangeForExistingChannelChange(Item item,
      Set<ItemViewConfig> itemViewConfigs) {
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      if (this.isItemViewConfigChange(item.getItemViewConfigs().iterator().next(), itemViewConfig)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public void updateProductItemViewConfig(String storeId, String itemSku, boolean buyable,
      boolean discoverable) throws Exception {
    ItemViewConfig itemViewConfigRequest = new ItemViewConfig();
    itemViewConfigRequest.setDiscoverable(discoverable);
    itemViewConfigRequest.setBuyable(buyable);
    Item item = checkItemState(storeId, itemSku, itemViewConfigRequest);
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, item.getItemSku());
    if (!item.isArchived() && isItemBuyableDiscoverable(itemPickupPoint)) {
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
      updateProductItemViewConfig(itemViewConfigRequest, item, itemPickupPoint);
      log.info("Item viewConfig changes updated and item change event is published item : {}", itemSku);
    } else {
      log.warn("No item viewConfig change update is required : {}", itemSku);
    }
  }

  private boolean isItemBuyableDiscoverable(ItemPickupPoint itemPickupPoint) {
    if (itemPickupPoint.getItemViewConfig().stream().findFirst().get().isBuyable()) {
      return true;
    }
    if (itemPickupPoint.getItemViewConfig().stream().findFirst().get().isDiscoverable()) {
      return true;
    }
    return false;
  }

  private boolean isItemViewConfigChange(ItemViewConfig currItemViewConfig,
      ItemViewConfig itemViewConfig) {
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    return (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())
        && !(currItemViewConfig.isBuyable() == itemViewConfig.isBuyable()
        && currItemViewConfig.isDiscoverable() == itemViewConfig.isDiscoverable()))
        || isItemBuyableScheduleChange(currItemViewConfig.getItemBuyableSchedules(),
        itemViewConfig.getItemBuyableSchedules()) || isItemDiscoverableScheduleChange(
        currItemViewConfig.getItemDiscoverableSchedules(),
        itemViewConfig.getItemDiscoverableSchedules());
  }


  private boolean isItemDiscoverableScheduleChange(
      ItemDiscoverableSchedule currentItemDiscoverableSchedule,
      ItemDiscoverableSchedule newItemDiscoverableSchedule) {
    return Objects.nonNull(currentItemDiscoverableSchedule) &&
        !currentItemDiscoverableSchedule.equals(newItemDiscoverableSchedule);
  }

  private boolean isItemBuyableScheduleChange(ItemBuyableSchedule currentItemBuyableSchedule,
      ItemBuyableSchedule newItemBuyableSchedule) {
    return Objects.nonNull(currentItemBuyableSchedule) &&
        !currentItemBuyableSchedule.equals(newItemBuyableSchedule);
  }

}
