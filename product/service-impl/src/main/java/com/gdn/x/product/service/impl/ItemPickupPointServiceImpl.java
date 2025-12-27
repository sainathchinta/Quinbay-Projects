package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.domain.event.model.BlimartSubscriptionChangeRequest;
import com.gdn.x.product.domain.event.model.PricingPwpPromoEvent;
import com.gdn.x.product.rest.web.model.request.CogsUpdateRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;

import com.gdn.x.product.service.util.ValidationUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ReindexService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.exceptions.ValidationException;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ItemPickupPointServiceImpl implements ItemPickupPointService {

  private static final String ITEM_PICKUPOINT_MUST_NOT_BE_NULL = "item pickupPoint must not null";

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Lazy
  @Autowired
  private ItemService itemService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private SaveOperationService saveOperationSolrService;

  @Autowired
  private ItemPickupPointRepository itemPickupPointRepository;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private ItemPickupPointHelperService pickupPointHelperService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ReindexService reindexService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Value("${offline.item.excluded.username}")
  private String excludedUserNames;

  @Value("${item.pickup.point.fetch.size}")
  private int itemPickupPointFetchSize;

  @Value("${itemSku.pickuppoint.timerange.fetch.size}")
  private int itemSkuAndPickupPointInTimeRangeFetchSize;

  @Value("${inventory.delete.batch.size}")
  private int inventoryDeleteBatchSize;

  @Value("${item.pickup.point.max.fetch.size}")
  private int itemPickupPointMaxFetchSize;

  @Value("${validate.on.original.selling.price}")
  private boolean validateOnOriginalSellingPrice;

  @Value("${validate.on.original.selling.price.client.id}")
  private String validateOnOriginalSellingPriceClientId;

  @Value("${ignore.generating.merchant.promo.discount.when.promo.price.is.null}")
  private boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull;

  @Value("${newly.added.item.pickup.point.data.change.event.types}")
  private String newlyAddedItemPickupPointDataChangeEventTypes;

  @Value("${new.l5.data.change.event.types.enabled}")
  private boolean newL5DataChangeTypeEnabled;

  @Value("${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value("${skip.auto.create.skip.for.existing.L5}")
  private boolean skipAutoCreateForExistingL5;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${dormant.flow.skip.deleted.items}")
  private boolean dormantFlowSkipDeletedItems;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${exclude.distribution.l5}")
  private boolean excludeDistributionL5;

  @Value("${cogs.update.max.request.size}")
  private int cogsUpdateMaxRequestSize;

  @Override
  public ItemPickupPoint saveItemPickupPoint(ItemPickupPoint itemPickupPoint) {
    ItemPickupPoint updatedItemPickupPoint = this.itemPickupPointRepository.save(itemPickupPoint);
    this.cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(), itemPickupPoint,
        itemPickupPoint.getPickupPointCode());
    return updatedItemPickupPoint;
  }

  @Override
  public void saveItemPickupPointCollection(List<ItemPickupPoint> existingItemPickupPoints) {
    if (CollectionUtils.isNotEmpty(existingItemPickupPoints)) {
      itemPickupPointRepository.saveAll(existingItemPickupPoints);
      existingItemPickupPoints.forEach(
          itemPickupPoint -> this.cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
              itemPickupPoint, itemPickupPoint.getPickupPointCode()));
    }
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(businessPartnerCode),
        ErrorMessages.MERCHANT_CODE_EMPTY);
    return this.itemPickupPointRepository.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(storeId,
        businessPartnerCode);
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
      String storeId, Set<String> itemSkus, boolean cncActivated, String pickupPointCode) {
    return itemPickupPointRepository
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndAndCncActiveAndMarkForDeleteFalse(storeId, itemSkus,
            cncActivated, pickupPointCode);
  }

  @Override
  @Cacheable(value = {
      CacheNames.FIND_ONE_L5_BY_ITEM_SKU}, key = "#storeId + '_' + #itemSku", unless = "#result == null")
  public ItemPickupPoint findByItemSkuAndDelivery(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(storeId, itemSku);
  }

  @Override
  public List<ItemPickupPoint> findByItemSkuInAndDelivery(String storeId, List<String> itemSkuList, boolean delivery) {
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(storeId, itemSkuList, delivery);
  }

  @Override
  public Page<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      Pageable pageable) {
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, pageable);
  }

  @Override
  public Page<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId, String itemSku,
      boolean cncActive, boolean markForDelete, Pageable pageable) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, itemSku, cncActive, markForDelete, pageable);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndPickupPointCode(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndPickupPointCode(storeId, pickupPointCode);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
  }

  @Override
  public Long getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.countByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
  }

  @Override
  public void updateItemViewConfigByItemSku(List<AuditTrailDto> auditTrailDtoList, String storeId,
      String username, boolean doArchive, Item item, List<ItemPickupPoint> itemPickupPointList, boolean isRejected) {
    String merchantCode = item.getMerchantCode();
    String itemSku = item.getItemSku();
    checkArgument(StringUtils.isNotBlank(storeId), OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    try {
      if (doArchive) {
        archiveItemPickupPoint(auditTrailDtoList, storeId, username, item, itemPickupPointList, isRejected);
      }
    } catch (Exception e) {
      log.error("error while updating itemPickupPoint for merchantCode = {} , itemSku = {} ", merchantCode, itemSku, e);
    }
  }

  private void unarchiveItemPickupPoint(String storeId, String username, Item item,
      List<ItemPickupPoint> itemPickupPointList) {
    List<ItemPickupPointRequestVo> updatedItemPickupPoints = new ArrayList<>();

    //get businessPartner details
    BusinessPartner businessPartner =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(storeId, item.getMerchantCode());

    //update only if businessPartner is cnc
    if (businessPartner.isCncActivated()) {

      //get list of cnc activated pickupPointCodes
      List<String> pickupPointCodesFromItemPickupPoint =
          itemPickupPointList.stream().map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toList());
      Set<String> cncActivatedFalsePickupPointCodes =
          pickupPointService.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(storeId,
                  pickupPointCodesFromItemPickupPoint).stream().filter(pickupPoint -> !pickupPoint.isCncActivated())
              .map(PickupPoint::getPickupPointCode).collect(Collectors.toSet());

      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        //update if the pickupPointCode is cnc
        if (!cncActivatedFalsePickupPointCodes.contains(itemPickupPoint.getPickupPointCode())) {
          itemPickupPoint.setCncActive(true);
          itemPickupPoint.setMarkForDelete(false);
          item.setCncActivated(true);
          updatedItemPickupPoints.add(
              new ItemPickupPointRequestVo(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()));
        }
      }
    }

    if (CollectionUtils.isNotEmpty(updatedItemPickupPoints)) {

      //update cnc and mfd flag
      List<FieldUpdateRequestVo> fieldUpdateRequestVoList =
          Arrays.asList(new FieldUpdateRequestVo(ProductFieldNames.CNC_ACTIVE, true),
              new FieldUpdateRequestVo(ProductFieldNames.MARK_FOR_DELETE, false));

      int batchSize = Integer.parseInt(systemParameterService.findValueByStoreIdAndVariable(storeId,
          SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL).getValue());
      List<List<ItemPickupPointRequestVo>> updatedItemPickupPointsBatches = Lists.partition(updatedItemPickupPoints, batchSize);
      updatedItemPickupPointsBatches.forEach(updatedItemPickupPointsBatch -> itemPickupPointRepository
          .updateFieldsByItemSkuAndPickupPointCodes(storeId, username, updatedItemPickupPointsBatch, fieldUpdateRequestVoList));

      //clear itemPickupPoint cache
      cacheEvictHelperService.evictItemPickupPointCache(storeId, Arrays.asList(item), itemPickupPointList);

      //clear unique id cache and publish offline event
      clearCacheAndPublishOfflineEvents(itemPickupPointList);
    }

  }

  private void archiveItemPickupPoint(List<AuditTrailDto> auditTrailDtoList, String storeId, String username,
      Item item, List<ItemPickupPoint> itemPickupPointList, boolean isRejected) {
    item.setCncActivated(false);

    //set cncActive, buyable and discoverable as false
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      itemPickupPoint.setCncActive(false);
      if (isRejected) {
        itemPickupPoint.setMarkForDelete(true);
      }
      updateItemViewConfig(itemPickupPoint.getItemViewConfig());
      if (schedulesAddEditEnabled) {
        CommonUtil.clearSchedules(auditTrailDtoList, item, itemPickupPoint);
      }
      itemPickupPoint.getItemPickupPointDataChangeType()
          .add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName());

      if (cncForWarehouseFeatureSwitch) {
        itemPickupPoint.getItemPickupPointDataChangeType()
            .add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE.getName());
      }
    }

    //separate the list by itemViewConfig is empty or not
    List<ItemPickupPointRequestVo> cncAndViewConfigUpdateList = itemPickupPointList.stream()
        .filter(itemPickupPoint -> CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())).map(
            itemPickupPoint -> new ItemPickupPointRequestVo(itemPickupPoint.getItemSku(),
                itemPickupPoint.getPickupPointCode())).collect(Collectors.toList());
    List<ItemPickupPointRequestVo> cncUpdateList = itemPickupPointList.stream()
        .filter(itemPickupPoint -> CollectionUtils.isEmpty(itemPickupPoint.getItemViewConfig())).map(
            itemPickupPoint -> new ItemPickupPointRequestVo(itemPickupPoint.getItemSku(),
                itemPickupPoint.getPickupPointCode())).collect(Collectors.toList());
    int batchSize = Integer.parseInt(systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL)
        .getValue());

    //update cnc and itemViewConfigFlags
    if (CollectionUtils.isNotEmpty(cncAndViewConfigUpdateList)) {
      List<FieldUpdateRequestVo> fieldUpdateRequestVoList = new ArrayList<>();
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.CNC_ACTIVE, false));
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.ITEM_PICKUP_POINT_BUYABLE, false));
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.ITEM_PICKUP_POINT_DISCOVERABLE, false));
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.ITEM_PICKUP_POINT_ALL_DISPLAYABLE_SCHEDULES, null));
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.ITEM_PICKUP_POINT_ALL_DISCOVERABLE_SCHEDULES, null));
      if (isRejected) {
        fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.MARK_FOR_DELETE, true));
      }
      updateFieldsByItemSkuAndPickupPointCodeList(storeId, username, cncAndViewConfigUpdateList, batchSize,
          fieldUpdateRequestVoList);
    }

    //update cncFlag
    if (CollectionUtils.isNotEmpty(cncUpdateList)) {
      List<FieldUpdateRequestVo> fieldUpdateRequestVoList = new ArrayList<>();
      fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.CNC_ACTIVE, false));
      if (isRejected) {
        fieldUpdateRequestVoList.add(new FieldUpdateRequestVo(ProductFieldNames.MARK_FOR_DELETE, true));
      }
      updateFieldsByItemSkuAndPickupPointCodeList(storeId, username, cncUpdateList, batchSize,
          fieldUpdateRequestVoList);
    }

    //clear itemPickupPoint cache
    cacheEvictHelperService.evictItemPickupPointCache(storeId, Arrays.asList(item), itemPickupPointList);
  }

  private void updateFieldsByItemSkuAndPickupPointCodeList(String storeId, String username,
      List<ItemPickupPointRequestVo> updateList, int batchSize, List<FieldUpdateRequestVo> fieldUpdateRequestVoList) {
    List<List<ItemPickupPointRequestVo>> cncAndViewConfigUpdateBatches = Lists.partition(updateList, batchSize);
    cncAndViewConfigUpdateBatches.forEach(
        updateBatch -> itemPickupPointRepository.updateFieldsByItemSkuAndPickupPointCodes(storeId, username,
            updateBatch, fieldUpdateRequestVoList));
  }

  private void clearCacheAndPublishOfflineEvents(List<ItemPickupPoint> itemPickupPointList) {
    List<OfflineItem> offlineItemList = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
    saveAndPublishService.publishListOfOfflineItems(offlineItemList, null);
  }

  private void updateItemViewConfig(Set<ItemViewConfig> itemViewConfigs) {
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
    }
  }

  @Override
  public void updateItemViewConfigForExistingChannel(ItemPickupPoint itemPickupPoint,
      Set<ItemViewConfig> itemViewConfigSet) {
    for (ItemViewConfig itemViewConfig : itemViewConfigSet) {
      this.updateItemViewConfigForExistingChannel(itemPickupPoint, itemViewConfig);
    }
  }

  @Override
  public List<ItemPickupPoint> findByItemSkusAndDelivery(String storeId, List<String> itemSkus, boolean delivery) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(storeId, itemSkus,
            delivery);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints),
        ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    return itemPickupPoints;
  }

  @Override
  public ItemPickupPoint findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findFirstByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(storeId, pickupPointCode);
  }


  @Override
  public List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(String storeId,
      String merchantCode, boolean cncActivated, boolean markForDelete) {
    return itemPickupPointRepository
        .findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(storeId, merchantCode, cncActivated,
            markForDelete);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(String storeId,
      String pickupPointCode, boolean cncActivated, boolean markForDelete) {
    return itemPickupPointRepository
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(storeId, pickupPointCode, cncActivated,
            markForDelete);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
      String storeId, String pickupPointCode, boolean configFlagValue, boolean markForDelete,
      String channel) {
    return itemPickupPointRepository.findByStoreIdAndPickupPointCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
        storeId, pickupPointCode, configFlagValue, markForDelete, channel);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
      String storeId, String merchantCode, boolean configFlagValue, boolean markForDelete,
      String channel) {
    return itemPickupPointRepository.findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
        storeId, merchantCode, configFlagValue, markForDelete, channel);
  }

  @Override
  public List<ItemPickupPoint> saveItemPickupPoint(List<ItemPickupPoint> itemPickupPointList) {
    List<ItemPickupPoint> pickupPointList = itemPickupPointRepository.saveAll(itemPickupPointList);
    itemPickupPointList.forEach(
        itemPickupPoint -> this.cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
            itemPickupPoint, itemPickupPoint.getPickupPointCode()));
    return pickupPointList;
  }

  @Override
  public List<ItemPickupPoint> saveItemPickupPoint(List<ItemPickupPoint> itemPickupPointList, List<Item> itemList) {
    List<ItemPickupPoint> updatedItemPickupPoints = itemPickupPointRepository.saveAll(itemPickupPointList);
    this.cacheEvictHelperService.evictItemPickupPointData(updatedItemPickupPoints, itemList);
    return updatedItemPickupPoints;
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku) {
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId,
      String itemSku) {
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, true);
  }

  @Override
  public List<ItemPickupPointPriceVo> findByStoreIdAndMerchantCodeAndItemSku(String storeId,
      String merchantCode, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    List<ItemPickupPoint> itemPickupPointList =
        this.itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(
            storeId, itemSku, merchantCode);
    return CommonUtil.toItemPickupPointPriceVoList(itemPickupPointList);
  }

  @Override
  public List<ItemPickupPointPriceVo> findItemPickupPointPriceVoByItemSkus(String storeId,
      Set<String> itemSkusFromMultipleMerchants, Pageable pageable) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkusFromMultipleMerchants),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    Page<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointRepository.findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(
            storeId, itemSkusFromMultipleMerchants, pageable);
    if (Objects.isNull(itemPickupPoints)) {
      return new ArrayList<>();
    }
    return CommonUtil.toItemPickupPointPriceVoList(itemPickupPoints.getContent());
  }

  @Override
  public List<ItemPickupPoint> deleteItemPickupPointByStoreIdAndProductSkus(String storeId, Set<String> productSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkus), ErrorMessages.PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY);
    return itemPickupPointRepository.deleteByStoreIdAndProductSkuIn(storeId, productSkus);
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
      String storeId, Set<String> itemSkus, boolean cncActive) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return this.itemPickupPointRepository.findByStoreIdAndItemSkuInAndCncActiveAndMarkForDeleteFalse(storeId, itemSkus, cncActive);
  }

  @Override
  @Cacheable(value = CacheNames.FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE, key = "#storeId + '_' + #itemSku + '_' + #pickupPointCode", unless = "#result == null")
  public ItemPickupPoint findByItemSkuAndPickupPointCode(String storeId, String itemSku,
    String pickupPointCode) {
    verifyGdnPreCondition(storeId, ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(itemSku, ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(pickupPointCode, ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode, false);
    return itemPickupPoint;
  }

  @Override
  public ItemPickupPoint findByItemSkuAndPickupPointCodeReadFromPrimary(String storeId, String itemSku,
      String pickupPointCode) {
    verifyGdnPreCondition(storeId, ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(itemSku, ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(pickupPointCode, ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode, true);
    return itemPickupPoint;
  }

  @Override
  public ItemPickupPoint findByItemSkuAndPickupPointCodeFromDb(String storeId, String itemSku, String pickupPointCode) {
    verifyGdnPreCondition(storeId, ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(itemSku, ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(pickupPointCode, ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode, false);
  }

  @Override
  public List<ItemPickupPoint> updateItemPickupPointPriceByOfflineItem(String storeId, String merchantCode,
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest) {
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    verifyGdnPreCondition(storeId, ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(merchantCode, ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    verifyGdnPreCondition(updateOfflineItemPriceRequest.getItemSku(), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(storeId,
            updateOfflineItemPriceRequest.getItemSku(), merchantCode);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints),
        ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      try {
        itemService.validatePriceForDeliveryTrueItemPickupPoint(storeId, itemPickupPoint,
            updateOfflineItemPriceRequest);
      } catch (ApplicationRuntimeException e){
        log.error("failed to update price for itemPickupPoint with itemSku = {} due to campaign price validation",
            itemPickupPoint.getItemSku(), e);
        continue;
        }
      Price price = itemPickupPoint.getPrice().stream().findFirst().get();
      price.setListPrice(Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice()));
      if (Double.compare(price.getListPrice(), updateOfflineItemPriceRequest.getListPrice()) != 0
          || Double.compare(price.getOfferPrice(), updateOfflineItemPriceRequest.getOfferPrice()) != 0) {
        itemPickupPoint.setOfflineItemHistoryDetail(
            new OfflineItemHistoryDetailVO(price.getListPrice(), price.getOfferPrice(), true,
                Constants.DEFAULT_CLIENT_ID_X_PRODUCT, Constants.DEFAULT_REQUEST_ID));
        price.setListPrice(updateOfflineItemPriceRequest.getListPrice());
        price.setOfferPrice(updateOfflineItemPriceRequest.getOfferPrice());
        updatedItemPickupPoints.add(itemPickupPoint);
      }
    }
    List<String> offlineItemIds =
        updatedItemPickupPoints.stream().map(ItemPickupPoint::getOfflineItemId).collect(Collectors.toList());

    if (CollectionUtils.isNotEmpty(updatedItemPickupPoints)) {
      itemPickupPointRepository.updatePriceByOfflineItemIds(offlineItemIds,
          Optional.ofNullable(updateOfflineItemPriceRequest.getListPrice())
              .orElse(updateOfflineItemPriceRequest.getOfferPrice()), updateOfflineItemPriceRequest.getOfferPrice());
      cacheEvictHelperService.evictItemPickupPointCache(storeId, null, updatedItemPickupPoints);
    }
    return updatedItemPickupPoints;
  }

  private void verifyGdnPreCondition(String parameter, String errorMessage) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(parameter), errorMessage);
  }

  @Override
  public ItemPickupPoint updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(fieldName), ErrorMessages.FIELD_NAME_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(value), ErrorMessages.VALUE_MUST_NOT_BE_NULL);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.updateFieldByItemSku(storeId, itemSku, fieldName, value);
    evictItemPickupPointCache(Arrays.asList(itemPickupPoint));
    return itemPickupPoint;
  }

  @Override
  public List<ItemPickupPoint> updateFieldsByItemSku(String storeId, String itemSku, String fieldName, Object value) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(fieldName), ErrorMessages.FIELD_NAME_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(value), ErrorMessages.VALUE_MUST_NOT_BE_NULL);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.updateFieldsByItemSku(storeId, itemSku, fieldName, value);
    evictItemPickupPointCache(itemPickupPoints);
    return itemPickupPoints;
  }

  @Override
  public ItemPickupPoint findByItemSkuAndDelivery(String storeId, String itemSku,
      boolean delivery) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryAndMarkForDeleteFalse(storeId, itemSku, delivery);
  }

  @Override
  public GdnRestListResponse<ItemSkuAndPickupPointCodeResponse> getL5sCreatedInTimeRange(String storeId, Date startDate,
      Date endDate, int page, int size, String requestId) {
    checkArgumentsForFetchL5sInTimeRange(storeId, startDate, endDate, size);
    Pageable pageable = PageRequest.of(page, size);
    Page<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(storeId,
            startDate, endDate, pageable);
    List<ItemSkuAndPickupPointCodeResponse> itemSkuAndPickupPointCodeResponses =
        Optional.ofNullable(itemPickupPoints.getContent()).orElse(new ArrayList<>()).stream()
            .map(CommonUtil::getItemSkuAndPickupPointCodeResponse).collect(Collectors.toList());
    return new GdnRestListResponse<>(null, null, true, itemSkuAndPickupPointCodeResponses,
        new PageMetaData(size, page, itemPickupPoints.getTotalElements()), requestId);
  }

  private void checkArgumentsForFetchL5sInTimeRange(String storeId, Date startDate, Date endDate, int size) {
    checkArgument(size <= itemSkuAndPickupPointInTimeRangeFetchSize,
        String.format(ErrorMessages.PAGE_SIZE_LIMIT_EXCEEDED_ERROR, itemSkuAndPickupPointInTimeRangeFetchSize));
    checkArgument(Objects.nonNull(startDate) && Objects.nonNull(endDate),
        ErrorMessages.START_DATE_AND_END_DATE_SHOULD_NOT_BE_EMPTY);
    checkArgument(startDate.compareTo(endDate) < 0, ErrorMessages.START_DATE_SHOULD_BE_LESSER_THAN_END_DATE);
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
  }

  @Override
  public GdnRestListResponse<PriceUpdatedInTimeRangeL5Response> getL5sPriceUpdatedInTimeRange(String storeId,
      Date startDate, Date endDate, int page, int size, String requestId) {
    checkArgumentsForFetchL5sInTimeRange(storeId, startDate, endDate, size);
    Pageable pageable = PageRequest.of(page, size);
    Page<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findByStoreIdAndPriceUpdatedDateBetweenAndMarkForDeleteFalse(storeId, startDate,
            endDate, pageable);
    List<PriceUpdatedInTimeRangeL5Response> priceUpdatedInTimeRangeL5Responses =
        Optional.ofNullable(itemPickupPoints.getContent()).orElse(new ArrayList<>()).stream()
            .map(CommonUtil::toPriceUpdatedInTimeRangeL5Response).collect(Collectors.toList());
    return new GdnRestListResponse<>(null, null, true, priceUpdatedInTimeRangeL5Responses,
        new PageMetaData(size, page, itemPickupPoints.getTotalElements()), requestId);
  }

  @Override
  public ItemPickupPointPriceVo findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(
      String storeId, String offlineItemId) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(offlineItemId),
        ErrorMessages.OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK);
    ItemPickupPoint itemPickupPoint =
        this.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, offlineItemId);
    return Objects.nonNull(itemPickupPoint) ?
        CommonUtil.toItemPickupPointPriceVo(itemPickupPoint) : null;
  }

  @Override
  public ItemPickupPoint findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(String storeId,
      String uniqueId) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(uniqueId),
        ErrorMessages.OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK);
    ItemPickupPoint itemPickupPoint =
        this.itemPickupPointRepository.findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(storeId,
            uniqueId);
    return itemPickupPoint;
  }

  @Override
  public void delete(String storeId, String merchantCode,
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests, String username) throws Exception {
    validateDeleteRequest(storeId, merchantCode,deleteOfflineItemRequests);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    for (DeleteOfflineItemRequest request : deleteOfflineItemRequests) {
      ItemPickupPoint itemPickupPoint =
          findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(storeId,
              request.getItemSku(),
              request.getPickupPointCode(), Boolean.TRUE);
      if (StringUtils.equals(merchantCode, itemPickupPoint.getMerchantCode())) {
        itemPickupPointList.add(itemPickupPoint);
      }
    }
    List<ItemPickupPoint> changedCncPickupPoints = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      try {
        itemPickupPoint.setCncActive(Boolean.FALSE);
        if(!itemPickupPoint.isDelivery()){
          itemPickupPoint.setMarkForDelete(true);
        }
        changedCncPickupPoints.add(itemPickupPoint);
        cacheEvictItemService.evictUniqueIdTypeCheck(storeId, itemPickupPoint.getItemSku());
      } catch (Exception ex) {
        log.error("Error on making item as cnc false with merchantCode: {}, offlineItem: {}",
            merchantCode, itemPickupPoint, ex);
      }
    }
    saveItemPickupPointCollection(itemPickupPointList);
    validateAndUpdateProductItemPickupPointCncActive(storeId, username, changedCncPickupPoints);
    this.saveAndPublishService.publishListOfItemsPickupPoints(changedCncPickupPoints, null);
  }

  @Override
  public List<DeleteOfflineItemVO> bulkDelete(String storeId, String merchantCode,
      List<DeleteOfflineItemVO> offlineItemsToDelete, String username) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(offlineItemsToDelete),
        ErrorMessages.OFFLINE_ITEM_LIST_MUST_NOT_BE_EMPTY);

    List<DeleteOfflineItemVO> result = new ArrayList<>();
    List<ItemPickupPoint> changedCncPickupPoints = new ArrayList<>();
    Map<String, Item> itemSkuAndItemName = itemService.fetchItemMapByItemSkus(storeId, offlineItemsToDelete.stream()
        .map(DeleteOfflineItemVO::getItemSku).collect(Collectors.toSet()));
    Map<String, Integer> itemSkuAndL5CountMap = new HashMap<>();
    Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
    for (DeleteOfflineItemVO offlineItemToDelete : offlineItemsToDelete) {
      ItemPickupPoint itemPickupPoint;
      String L5Id = offlineItemToDelete.getItemSku() + Constants.HYPHEN + offlineItemToDelete.getPickupPointCode();
      if (!itemSkuAndL5CountMap.containsKey(offlineItemToDelete.getItemSku())) {
        fetchItemPickupPointAndUpdateL5Count(storeId, offlineItemToDelete.getItemSku(),
            itemPickupPointMap, itemSkuAndL5CountMap);
      }
      itemPickupPoint = itemPickupPointMap.get(L5Id);
      if (Objects.isNull(itemPickupPoint) ||
          !StringUtils.equals(merchantCode, itemPickupPoint.getMerchantCode())) {
        offlineItemToDelete.setSuccess(Boolean.FALSE);
        offlineItemToDelete.setErrorMessage(
          String.format(ErrorMessages.NO_ITEM_FOUND, offlineItemToDelete.getItemSku()));
      } else {
        try {
          checkIfDistributionPPIsBeingDeleted(itemPickupPoint);
          int l5Count = itemSkuAndL5CountMap.get(itemPickupPoint.getItemSku());
          if (l5Count > 1) {
            itemPickupPoint.setCncActive(Boolean.FALSE);
            itemPickupPoint.setMarkForDelete(Boolean.TRUE);
            offlineItemToDelete.setSuccess(Boolean.TRUE);
          } else {
            markL5OfflineAndCncInActive(itemPickupPoint, offlineItemToDelete);
          }
          itemSkuAndL5CountMap.put(itemPickupPoint.getItemSku(), l5Count - 1);
          changedCncPickupPoints.add(itemPickupPoint);
          cacheEvictItemService.evictUniqueIdTypeCheck(storeId, itemPickupPoint.getOfflineItemId());
          offlineItemToDelete.setProductSku(itemPickupPoint.getProductSku());
          offlineItemToDelete.setItemName(itemSkuAndItemName.get(offlineItemToDelete.getItemSku()).getGeneratedItemName());
        } catch (Exception e) {
          log.error("Error on delete offline item with merchantCode: {}, offlineItem: {}",
              merchantCode, itemPickupPoint, e);
          offlineItemToDelete.setSuccess(Boolean.FALSE);
          offlineItemToDelete.setErrorMessage(e.getMessage());
        }
      }
      result.add(offlineItemToDelete);
    }
    saveItemPickupPointCollection(changedCncPickupPoints);
    validateAndUpdateProductItemPickupPointCncActive(storeId, username, changedCncPickupPoints);
    saveAndPublishService.publishListOfItemsPickupPoints(changedCncPickupPoints, null);
    if (inventoryDeleteBatchSize > 0) {
      deleteItemSkuPickupPoint(result, merchantCode, username);
    }
    return result;
  }

  private void checkIfDistributionPPIsBeingDeleted(ItemPickupPoint itemPickupPoint) {
    if (ranchIntegrationEnabled) {
      checkArgument(!itemPickupPoint.isDistribution(),
          ErrorMessages.DISTRIBUTION_PICKUP_POINT_DELETE_NOT_ALLOWED);
    }
  }

  private void fetchItemPickupPointAndUpdateL5Count(String storeId, String itemSku,
      Map<String, ItemPickupPoint> itemPickupPointMap, Map<String, Integer> itemSkuAndL5CountMap) {
    List<ItemPickupPoint> itemPickupPoints = findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      itemSkuAndL5CountMap.put(itemSku, itemPickupPoints.size());
      for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
        itemPickupPointMap.put(itemPickupPoint.getItemSku() + Constants.HYPHEN +
            itemPickupPoint.getPickupPointCode(), itemPickupPoint);
      }
    }
  }

  private void markL5OfflineAndCncInActive(ItemPickupPoint itemPickupPoint, DeleteOfflineItemVO offlineItemToDelete) {
    if (itemPickupPoint.isCncActive()) {
      offlineItemToDelete.setCncUpdated(true);
      itemPickupPoint.setCncActive(Boolean.FALSE);
    }
    if (itemPickupPoint.getItemViewConfig().stream().findFirst().get().isBuyable()) {
      offlineItemToDelete.setBuyableUpdated(true);
      itemPickupPoint.getItemViewConfig().stream().findFirst().get().setBuyable(Boolean.FALSE);
    }
    if (itemPickupPoint.getItemViewConfig().stream().findFirst().get().isDiscoverable()) {
      offlineItemToDelete.setDiscoverableUpdated(true);
      itemPickupPoint.getItemViewConfig().stream().findFirst().get().setDiscoverable(Boolean.FALSE);
    }
    BusinessPartner businessPartner = businessPartnerService.getBusinessPartnerByBusinessPartnerCode(itemPickupPoint.getStoreId(),
        itemPickupPoint.getMerchantCode());
    offlineItemToDelete.setSuccess(Boolean.FALSE);
    if (businessPartner.isInternationalFlag()) {
      offlineItemToDelete.setErrorMessage(ErrorMessages.CAN_NOT_DELETE_L5_EN);
    } else {
      offlineItemToDelete.setErrorMessage(ErrorMessages.CAN_NOT_DELETE_L5_ID);
    }
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSkuAndDelivery(String storeId,
      String productSku, boolean delivery) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_CODE_AND_PRODUCT_SKU_MUST_NOT_BE_NULL);
    return itemPickupPointRepository
        .findItemPickupPointByStoreIdAndProductSkuAndDeliveryAndMarkForDeleteFalse(storeId, productSku,
            delivery);
  }

  @Override
  public ItemPickupPoint findByStoreIdAndOfflineItemId(String storeId, String offlineItemId) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(offlineItemId), ErrorMessages.OFFLINE_ITEM_IDS_MUST_NOT_BE_EMPTY);
    return itemPickupPointRepository.findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(storeId, offlineItemId);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndOfflineItemIds(String storeId, List<String> offlineItemIds) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(offlineItemIds), ErrorMessages.OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndOfflineItemIdInAndMarkForDeleteFalse(storeId, offlineItemIds);
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku,
      String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(pickupPointCode), ITEM_PICKUPOINT_MUST_NOT_BE_NULL);
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku, pickupPointCode, false);
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimary(String storeId,
      String itemSku, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(pickupPointCode), ITEM_PICKUPOINT_MUST_NOT_BE_NULL);
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku, pickupPointCode, true);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(String storeId, String itemSku,
      List<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), ITEM_PICKUPOINT_MUST_NOT_BE_NULL);
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(storeId, itemSku, pickupPointCodes);
  }

  private void updateItemViewConfigForExistingChannel(ItemPickupPoint itemPickupPoint, ItemViewConfig itemViewConfig) {
    checkArgument(itemPickupPoint != null, ITEM_PICKUPOINT_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    for (ItemViewConfig currItemViewConfig : itemPickupPoint.getItemViewConfig()) {
      if (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
        currItemViewConfig.setBuyable(itemViewConfig.isBuyable());
        currItemViewConfig.setDiscoverable(itemViewConfig.isDiscoverable());
        currItemViewConfig.setItemBuyableSchedules(null);
        currItemViewConfig.setItemDiscoverableSchedules(null);
      }
    }
  }

  private void validateDeleteRequest(String storeId, String merchantCode,
      List<DeleteOfflineItemRequest> deleteOfflineItemRequests) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(deleteOfflineItemRequests),
        ErrorMessages.ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY);
    for (DeleteOfflineItemRequest deleteOfflineItemRequest : deleteOfflineItemRequests) {
      checkArgument(StringUtils.isNotBlank(deleteOfflineItemRequest.getItemSku()),
          ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(deleteOfflineItemRequest.getPickupPointCode()),
          ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
  }

  public void validateAndUpdateProductItemPickupPointCncActive(String storeId, String username, List<ItemPickupPoint> updatedItemPickupPoints) throws Exception {
    if (!CollectionUtils.isEmpty(updatedItemPickupPoints)) {
      Map<String, List<ItemPickupPoint>> updatedItemPickupPointProductSkuMap =
          updatedItemPickupPoints.stream().collect(Collectors.groupingBy(ItemPickupPoint::getProductSku));
      for (String productSku : updatedItemPickupPointProductSkuMap.keySet()) {
        Set<ItemPickupPoint> itemPickupPointList =
            getAllItemPickupPointsByProductSku(storeId, productSku, updatedItemPickupPointProductSkuMap);
        if (CollectionUtils.isEmpty(itemPickupPointList)) {
          productService.toggleArchiveProduct(storeId, username, productSku, true, StringUtils.EMPTY);
        } else {
          productService.updateDistinctPickupPointCodesAndL5Count(storeId,
              username, productSku, itemPickupPointList, itemPickupPointList.size());
        }
      }
    }
  }


  private Set<ItemPickupPoint> getAllItemPickupPointsByProductSku(String storeId, String productSku,
      Map<String, List<ItemPickupPoint>> itemPickupPointProductSkuMap) {
    List<ItemPickupPoint> updatedItemPickupPoint = itemPickupPointProductSkuMap.get(productSku);
    Set<String> updatedIds = CommonUtil.getItemPickupPointIds(updatedItemPickupPoint);
    List<ItemPickupPoint> notUpdatedItemPickupPoint =
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(storeId,
            productSku, updatedIds);
    return CommonUtil.combineListOfUpdatedAndNotUpdatedPickupPoint(notUpdatedItemPickupPoint, updatedItemPickupPoint);
  }

  @Override
  public ItemPickupPoint findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean cncActive) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(storeId, itemSku, cncActive);
  }

  @Override
  public ItemPickupPoint findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean cncActive, Set<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(
        storeId, itemSku, cncActive, pickupPointCodes);
  }

  @Override
  public ItemPickupPoint findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String itemSku, Set<String> pickupPointCodes, boolean cncActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    return itemPickupPointRepository.findFirstByItemSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        storeId, itemSku, pickupPointCodes, cncActivated);
  }

  @Override
  public ItemPickupPoint findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActive) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findFirstByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalse(storeId, productSku, cncActive);
  }

  @Override
  public ItemPickupPoint findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActive, Set<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findFirstByStoreIdAndProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(storeId, productSku, cncActive, pickupPointCodes);
  }

  @Override
  public ItemPickupPoint findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String productSku, Set<String> pickupPointCodes, boolean cncActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    return itemPickupPointRepository.findFirstByProductSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        storeId, productSku, pickupPointCodes, cncActivated);
  }

  @Override
  public ItemPickupPoint findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    return itemPickupPointRepository.findFirstByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
  }

  private void reindexItemCncActivated(String storeId, Set<String> itemSkus, Boolean cncActivated,
      List<OfflineItem> updatedOfflineItems, boolean isPriceUpdate, String username) {
    boolean enableDeferredSolrReindex = Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX).getValue());
    List<Item> items = itemService.getItemsByStoreIdAndItemSkus(storeId, itemSkus);
    Map<String, Item> itemMap = Optional.ofNullable(items).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    List<Item> deferredReindexItems = new ArrayList<>();
    for (String itemSku : itemSkus) {
      try {
        if (itemMap.containsKey(itemSku)) {
          Item item = itemMap.get(itemSku);
          if (enableDeferredSolrReindex && CommonUtil.checkIfUsernameIsExcluded(username, excludedUserNames)) {
            deferredReindexItems.add(item);
          } else {
            productAndItemSolrIndexerService.applyItem(item);
            reindexService.reindexPendingL3SolrAndDatabase(
              Collections.singletonList(item.getProductSku()), new HashMap<>());
          }
        } else {
          log.warn("reindexItemCncActivated : Item Not found : {}", itemSku);
        }
      } catch (Exception e) {
        log.error("Error on reindex item after update offline item with itemSku: {}", itemSku, e);
      }
    }
    if (CollectionUtils.isNotEmpty(deferredReindexItems)) {
      saveOperationSolrService.deferredReindexItems(storeId, deferredReindexItems, true,
          ReindexType.OFFLINE_ITEM_REINDEX, true);
    }
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(
      String storeId, String itemSku, String pickupPointCode, boolean cncActive) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(storeId, itemSku,
            pickupPointCode, Boolean.TRUE);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(String storeId, String itemSku,
      boolean cncActive, boolean markForDelete) {
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, itemSku, cncActive, markForDelete);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(String storeId, String itemSku) {
    return itemPickupPointRepository
        .findByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(storeId, itemSku, channelService.getCncChannel());
  }

  @Override
  public void updateDiscountPriceInItemPickupPoint(ItemPickupPoint itemPickupPoint) {
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    List<ItemPickupPoint> updatedItemPickupPoint =
        itemPickupPointRepository.updateDiscountPriceInItemPickupPoint(itemPickupPoint);
    evictItemPickupPointCache(updatedItemPickupPoint);
  }

  @Override
  public void updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(ItemPickupPoint itemPickupPoint) {
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    List<ItemPickupPoint> updatedItemPickupPoint =
        itemPickupPointRepository.updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint);
    evictItemPickupPointCache(updatedItemPickupPoint);
  }

  @Override
  public boolean updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive) {
    List<ItemPickupPoint> updatedItemPickupPoint = itemPickupPointRepository.updateMerchantPromoDiscountFlag(storeId, itemSku, isPromoActive);
    evictItemPickupPointCache(updatedItemPickupPoint);
    return CollectionUtils.isNotEmpty(updatedItemPickupPoint);
  }

  @Override
  public List<ItemPickupPoint> updateFieldByItemSkusAndDelivery(String storeId, Collection<String> itemSkus,
      String fieldName, boolean delivery, Object value) {
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.updateFieldByItemSkusAndDelivery(storeId, itemSkus, fieldName, delivery, value);
    evictItemPickupPointCache(itemPickupPoints);
    return itemPickupPoints;
  }

  @Override
  public List<ItemPickupPoint> updateFieldByItemSkusAndPPCode(String storeId, List<ItemInfo> itemInfos,
      String fieldName, Object value) {
    List<ItemPickupPointRequestVo> itemPickupPointRequestVos = CommonUtil.toItemPickupPointRequestVos(itemInfos);
    itemPickupPointRepository.updateFieldsByItemSkuAndPickupPointCodes(storeId,
      GdnMandatoryRequestParameterUtil.getUsername(), itemPickupPointRequestVos,
      CommonUtil.toFieldUpdateRequestVos(fieldName, value));
    List<ItemPickupPoint> itemPickupPoints =
      itemPickupPointRepository.fetchItemPickupPointsByItemSkuAndPickupPointCode(storeId, itemPickupPointRequestVos,
          false);
    evictItemPickupPointCache(itemPickupPoints);
    return itemPickupPoints;
  }

  @Override
  public ItemPickupPoint updateActivePromoBundling(String storeId, String itemSku, String promoBundlingType,
      boolean clearPromoBundlings) {
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(storeId, itemSku);
    if (Objects.nonNull(itemPickupPoint)) {
      return getAndUpdateActivePromoBundlingsInItemPickupPoint(clearPromoBundlings, itemPickupPoint, promoBundlingType,
          storeId);
    }
    return null;
  }

  @Override
  public ItemPickupPoint updateActivePromoBundlingByItemSkuAndPPCode(String storeId, String itemSku,
      String promoBundlingType, boolean clearPromoBundlings, String ppCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(ppCode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku, ppCode, false);
    if (Objects.nonNull(itemPickupPoint)) {
      return getAndUpdateActivePromoBundlingsInItemPickupPoint(clearPromoBundlings, itemPickupPoint, promoBundlingType,
          storeId);
    }
    return null;
  }

  private ItemPickupPoint getAndUpdateActivePromoBundlingsInItemPickupPoint(boolean clearPromoBundlings,
      ItemPickupPoint itemPickupPoint, String promoBundlingType, String storeId) {
    if (clearPromoBundlings) {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
        itemPickupPoint.getActivePromoBundlings().remove(promoBundlingType);
      }
    } else {
      if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
        if (Constants.WHOLESALE_PRICE.equalsIgnoreCase(promoBundlingType)) {
          itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE);
        } else if (Constants.WHOLESALE.equalsIgnoreCase(promoBundlingType)) {
          itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        }
        itemPickupPoint.getActivePromoBundlings().add(promoBundlingType);
      } else {
        itemPickupPoint.setActivePromoBundlings(new HashSet<>());
        itemPickupPoint.getActivePromoBundlings().add(promoBundlingType);
      }
    }
    ItemPickupPointRequestVo itemPickupPointRequestVo =
      new ItemPickupPointRequestVo(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode());
    List<FieldUpdateRequestVo> fieldUpdateRequestVoList = new ArrayList<>();
    fieldUpdateRequestVoList.addAll(CommonUtil.toFieldUpdateRequestVos(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, itemPickupPoint.getActivePromoBundlings()));
    itemPickupPointRepository.updateFieldsByItemSkuAndPickupPointCodes(storeId,
        GdnMandatoryRequestParameterUtil.getUsername(), Collections.singletonList(itemPickupPointRequestVo),
        fieldUpdateRequestVoList);
    cacheEvictHelperService.evictItemPickupPointData(storeId, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    return itemPickupPoint;
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    return itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(storeId, itemSku);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSku(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndProductSku(storeId, productSku, false);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSkuReadFromPrimary(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndProductSku(storeId, productSku, true);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSku(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    return itemPickupPointRepository.findByStoreIdAndItemSku(storeId, itemSku);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, List<String> itemSkus) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus, false);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimary(String storeId,
      List<String> itemSkus) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus, true);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(
    String storeId, List<String> offlineItemIdList) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(offlineItemIdList), ErrorMessages.OFFLINE_ITEM_LIST_MUST_NOT_BE_EMPTY);
    return this.itemPickupPointRepository.findByStoreIdAndOfflineItemIdInAndCncActiveTrueAndMarkForDeleteFalse(
      storeId, offlineItemIdList);
  }

  @Override
  public Set<String> getActivePromoBundlingByItemSkus(String storeId, List<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    return itemPickupPointRepository.getActivePromoBundlingsByItemSkus(storeId, itemSkus);
  }

  @Override
  public boolean validatePriceChangeAndSet(ItemPickupPoint itemToSet, Set<Price> updatedPrices,
    String username) {
    boolean isPriceChanged = false;
    itemToSet.setMerchantPromoDiscount(CommonUtil.isMerchantPromoDiscountActive(itemToSet,
        ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull));
    if (CollectionUtils.isNotEmpty(updatedPrices)) {
      Set<String> clientIdList =
          Stream.of(validateOnOriginalSellingPriceClientId.split(Constants.COMMA)).collect(Collectors.toSet());
      if (validateOnOriginalSellingPrice && clientIdList.contains(GdnMandatoryRequestParameterUtil.getClientId())) {
        isPriceChanged =
            pickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemToSet, updatedPrices);
      } else {
        isPriceChanged = pickupPointHelperService.isItemPickupPointPriceChange(itemToSet, updatedPrices);
      }
      if (isPriceChanged) {
        if (pickupPointHelperService.isPriceEditDisabled(itemToSet)) {
          log.error("Price change is disabled for itemSku: {}", itemToSet.getItemSku());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.PRICE_CANNOT_BE_EDITED_FOR_ITEM);
        }
        ItemPickupPoint returnedItem =
          this.pickupPointHelperService.setItemPriceByChannel(itemToSet, updatedPrices, username);
        itemToSet.setPrice(returnedItem.getPrice());
        log.info("Price is changed for itemSku: {}", itemToSet.getItemSku());
      }
    }
    return isPriceChanged;
  }


  @Override
  public void publishItemPickupPointChangeEvent(ItemPickupPoint itemPickupPoint) {
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    BusinessPartner businessPartner =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(itemPickupPointDataChangeEventModel.getStoreId(),
            itemPickupPointDataChangeEventModel.getMerchantCode());
    itemPickupPointDataChangeEventModel.setSellerChannel(
        CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner));
    log.info("Publishing event : {}, itemPickupPointChangeEventModel : {}",
      ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      itemPickupPointDataChangeEventModel);
    setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(itemPickupPointDataChangeEventModel);
    kafkaPublisher.send(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode(),
      itemPickupPointDataChangeEventModel);
  }

  private void setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel) {
    if (newL5DataChangeTypeEnabled) {
      Optional.ofNullable(itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes())
        .ifPresent(types -> {
          List<String> eventTypesV2 = types.stream().map(Object::toString).collect(Collectors.toList());
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypesV2(eventTypesV2);
          List<ItemPickupPointChangeEventType> updatedEventTypes = CommonUtil.removeNewlyAddedEventTypesForL5DataChange(types,
              newlyAddedItemPickupPointDataChangeEventTypes);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(updatedEventTypes);
        });
    }
  }



  @Override
  public void publishItemPickupPointDataChangeEventWithPureCncStatusChange(
          ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel,
          Map<String, Set<String>> eventToBlackListedSellersMap) {
    if (ValidationUtil.checkIfBlacklistedSellerForSpecificEvent(eventToBlackListedSellersMap,
      itemPickupPointDataChangeEventModel.getMerchantCode(),
      ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME)) {
      log.info("Skipping publish. Merchant {} is blacklisted for event {}",
        itemPickupPointDataChangeEventModel.getMerchantCode(),
        ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME);
      return;
    }
    BusinessPartner businessPartner =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(itemPickupPointDataChangeEventModel.getStoreId(),
            itemPickupPointDataChangeEventModel.getMerchantCode());
    itemPickupPointDataChangeEventModel.setSellerChannel(
        CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner));
    log.info("Publishing event : {}, itemPickupPointChangeEventModel : {}",
        ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, itemPickupPointDataChangeEventModel);
    setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(itemPickupPointDataChangeEventModel);
    kafkaPublisher.send(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      itemPickupPointDataChangeEventModel.getItemSku() + Constants.HYPHEN
        + itemPickupPointDataChangeEventModel.getPickupPointCode(),
      itemPickupPointDataChangeEventModel);
  }

  @Override
  public Set<String> getActivePromoBundlingByItemSkusAndPickupPointCode(String storeId, List<String> itemSkus, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    return itemPickupPointRepository.getActivePromoBundlingsByItemSkusAndMarkForDeleteFalse(storeId, itemSkus, pickupPointCode);
  }

  private void evictItemPickupPointCache(List<ItemPickupPoint> itemPickupPoints) {
    Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream().filter(Objects::nonNull).forEach(
        itemPickupPoint -> cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
            itemPickupPoint, itemPickupPoint.getPickupPointCode()));
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPPcode(String storeId, String productSku,
      String ppcode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    checkArgument(StringUtils.isNotBlank(ppcode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId,
        productSku, ppcode);
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPPCodeWithShowDeleted(String storeId, String productSku,
      String ppcode, boolean showDeleted) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    checkArgument(StringUtils.isNotBlank(ppcode), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    if (showDeleted) {
      return itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCode(storeId,
          productSku, ppcode);
    } else {
      return itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId,
          productSku, ppcode);
    }
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodes(String storeId, String productSku,
      List<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(storeId, productSku, pickupPointCodes, false);
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimary(String storeId,
      String productSku, List<String> pickupPointCodes) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    checkArgument(CollectionUtils.isNotEmpty(pickupPointCodes), ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(storeId, productSku, pickupPointCodes, true);
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalse(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    return this.itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
      productSku, false);
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimary(String storeId,
      String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    return this.itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
        productSku, true);
  }

  @Override
  public List<ItemSkuPickupPointCodeResponse> findPickUpPointCodeByItemSkuInAndDelivery(String storeId, List<String> itemSkuList, boolean delivery) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkuList), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointRepository
        .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(storeId, itemSkuList, delivery);
    return itemPickupPoints.stream().map(item -> new ItemSkuPickupPointCodeResponse(item.getItemSku(),
        item.getPickupPointCode())).collect(Collectors.toList());
  }

  @Override
  public Page<ItemPickupPoint> findItemPickupPointForSummaryListing(String storeId, int page, int size,
      ItemPickupPointSummaryRequest itemPickupPointSummaryRequest, Set<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(itemPickupPointSummaryRequest),
        ErrorMessages.FILTER_SUMMARY_REQUEST_MUST_NOT_BE_NULL);
    return itemPickupPointRepository.getItemPickupPointListing(storeId, page, size,
        ProductAndItemsUtil.toItemPickupPointSummaryRequestVo(itemPickupPointSummaryRequest, itemSkus));
  }

  @Override
  public List<ItemPickupPoint> fetchItemPickupPointsByItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointRequest> itemPickupPointRequests, boolean inAllProducts) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPointRequests),
        ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_FOR_THE_REQUEST);
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoList =
        ProductAndItemsUtil.toItemPickupPointRequestVo(itemPickupPointRequests);
    return Lists.partition(itemPickupPointRequestVoList, itemPickupPointFetchSize).stream().flatMap(
        itemPickupPointRequestVoSubList -> itemPickupPointRepository.fetchItemPickupPointsByItemSkuAndPickupPointCode(
            storeId, itemPickupPointRequestVoSubList, inAllProducts).stream()).collect(Collectors.toList());
  }

  @Override
  public void updateItemPickupPointViewConfigWithProductStatus(String storeId, String userName, String requestId,
      String productSku, ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception {
      List<ItemPickupPoint> itemPickupPointList =
          itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
      if (dormantFlowSkipDeletedItems && CollectionUtils.isEmpty(itemPickupPointList)) {
        return;
      }
      List<Item> itemList = getAndUpdateCnCActivatedFlagAtL4(storeId, productSku, itemPickupPointViewConfigBaseRequest);
      Product product = getAndUpdateCnCActivatedAtL3(storeId, productSku, itemPickupPointViewConfigBaseRequest);
      List<ItemPickupPoint> itemPickupPointCncChangeList = new ArrayList<>();
      List<ItemPickupPoint> itemPickupPointViewConfigChangeList = new ArrayList<>();
      Map<String, Item> itemMap = new HashMap<>();
      List<Item> itemsForViewConfig = new ArrayList<>();
    Map<String, Boolean> offlineItemIdToPureCncStatusChangedMap = new HashMap<>();
    Map<String, List<ItemPickupPointChangeEventType>> offlineItemIdToItemPickupPointChangeEventTypeMap =
        new HashMap<>();
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
    itemPickupPointList = viewConfigChangeForL5MppEnabled(itemPickupPointViewConfigBaseRequest, itemPickupPointList,
        offlineItemIdToPureCncStatusChangedMap, offlineItemIdToItemPickupPointChangeEventTypeMap);
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
          objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint,
              offlineItemIdToPureCncStatusChangedMap.get(itemPickupPoint.getOfflineItemId()));
      itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(
          offlineItemIdToItemPickupPointChangeEventTypeMap.get(itemPickupPoint.getOfflineItemId()));
      itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
      }
    publishEventsForViewConfigChange(storeId, itemPickupPointDataChangeEventModelList, itemList, product,
        itemPickupPointCncChangeList, itemPickupPointViewConfigChangeList, itemMap, itemsForViewConfig);
  }

  private List<ItemPickupPoint> viewConfigChangeForL5MppEnabled(ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest,
      List<ItemPickupPoint> itemPickupPointList, Map<String, Boolean> offlineItemIdToPureCncStatusChangedMap,
      Map<String, List<ItemPickupPointChangeEventType>> offlineItemIdToItemPickupPointChangeEventTypeMap) {
    List<ItemPickupPoint> itemPickupPointNeedToUpdateList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      boolean needEdit =
        processAllItemViewConfigs(itemPickupPoint, itemPickupPointViewConfigBaseRequest,
          offlineItemIdToPureCncStatusChangedMap, offlineItemIdToItemPickupPointChangeEventTypeMap,
          itemPickupPointNeedToUpdateList);
      if (needEdit || (!cncForWarehouseFeatureSwitch && itemPickupPoint.isCncActive())) {
        itemPickupPointNeedToUpdateList.add(itemPickupPoint);
      }
    }
    itemPickupPointNeedToUpdateList.stream()
            .map(ItemPickupPoint::getAllItemViewConfigs)
            .flatMap(Collection::stream)
            .forEach(
                    itemViewConfig -> setItemViewConfigsByItemPickupPointViewConfigBaseRequest(itemViewConfig, itemPickupPointViewConfigBaseRequest)
            );
    if (!cncForWarehouseFeatureSwitch) {
      itemPickupPointNeedToUpdateList.forEach(itemPickupPoint -> itemPickupPoint.setCncActive(
          itemPickupPointViewConfigBaseRequest.isCncActivated()));
    }
    itemPickupPointRepository.saveAll(itemPickupPointNeedToUpdateList);
    return itemPickupPointNeedToUpdateList;
  }


  private boolean processAllItemViewConfigs(ItemPickupPoint itemPickupPoint,
    ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest,
    Map<String, Boolean> offlineItemIdToPureCncStatusChangedMap,
    Map<String, List<ItemPickupPointChangeEventType>> offlineItemIdToItemPickupPointChangeEventTypeMap,
    List<ItemPickupPoint> itemPickupPointNeedToUpdateList) {
    boolean needEdit = false;
    for (ItemViewConfig itemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
      if (!cncForWarehouseFeatureSwitch) {
        offlineItemIdToPureCncStatusChangedMap.put(itemPickupPoint.getOfflineItemId(),
          CommonUtil.isPureCNCStatusChange(
            ItemViewConfig.builder().isBuyable(itemPickupPointViewConfigBaseRequest.isBuyable())
              .isDiscoverable(itemPickupPointViewConfigBaseRequest.isDiscoverable()).build(),
            itemViewConfig, itemPickupPointViewConfigBaseRequest.isCncActivated(),
            itemPickupPoint.isCncActive()));
      }
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList =
        offlineItemIdToItemPickupPointChangeEventTypeMap.getOrDefault(
          itemPickupPoint.getOfflineItemId(), new ArrayList<>());
      if (CommonUtil.isDiscoverableChanged(
        ItemViewConfig.builder().isBuyable(itemPickupPointViewConfigBaseRequest.isBuyable())
          .isDiscoverable(itemPickupPointViewConfigBaseRequest.isDiscoverable()).build(),
        itemViewConfig)) {
        setL5DataChangeTypeForDiscoverableFlagChange(itemPickupPoint,
          itemPickupPointNeedToUpdateList, itemViewConfig, itemPickupPointChangeEventTypeList);
      }
      offlineItemIdToItemPickupPointChangeEventTypeMap.put(itemPickupPoint.getOfflineItemId(),
        itemPickupPointChangeEventTypeList);
      if (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) {
        needEdit = true;
      }
    }
    return needEdit;
  }

  private void setL5DataChangeTypeForDiscoverableFlagChange(ItemPickupPoint itemPickupPoint,
    List<ItemPickupPoint> itemPickupPointNeedToUpdateList, ItemViewConfig itemViewConfig,
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList) {
    if (cncForWarehouseFeatureSwitch && StringUtils.equals(itemViewConfig.getChannel(),
      Constants.CNC)) {
      itemPickupPointChangeEventTypeList.add(
        ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
      itemPickupPointNeedToUpdateList.add(itemPickupPoint);
    } else {
      itemPickupPointChangeEventTypeList.add(
        ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
    }
  }


  private void publishEventsForViewConfigChange(String storeId,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList, List<Item> itemList,
      Product product, List<ItemPickupPoint> itemPickupPointCncChangeList,
      List<ItemPickupPoint> itemPickupPointViewConfigChangeList, Map<String, Item> itemMap,
      List<Item> itemsForViewConfig) {
    saveOperationSolrService.saveProduct(product);
    saveAndPublishService.publishItemDataChangeEvent(itemList);
    itemPickupPointDataChangeEventModelList.stream().forEach(
        itemPickupPointDataChangeEventModel ->  publishItemPickupPointDataChangeEventWithPureCncStatusChange(
            itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP));
  }

  private void viewConfigChange(ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest,
      List<ItemPickupPoint> itemPickupPointViewConfigChangeList, ItemPickupPoint itemPickupPoint) {
    boolean viewConfigChange = false;
    for (ItemViewConfig itemViewConfig : itemPickupPoint.getItemViewConfig()) {
      if (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) {
        itemViewConfig.setBuyable(itemPickupPointViewConfigBaseRequest.isBuyable());
        itemViewConfig.setDiscoverable(itemPickupPointViewConfigBaseRequest.isDiscoverable());
        viewConfigChange = true;
      }
    }
    if (viewConfigChange) {
      itemPickupPointViewConfigChangeList.add(itemPickupPoint);
    }
  }

  private Product getAndUpdateCnCActivatedAtL3(String storeId, String productSku,
      ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception {
    Product product = productService.getProduct(storeId, productSku);
    GdnPreconditions.checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    if (product.isCncActivated()) {
      product.setCncActivated(itemPickupPointViewConfigBaseRequest.isCncActivated());
    }
    return product;
  }

  private List<Item> getAndUpdateCnCActivatedFlagAtL4(String storeId, String productSku,
      ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception {
    List<Item> uniqueItemList =
      itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku).stream()
        .filter(Predicate.not(Item::isMarkForDelete)).distinct().toList();
    List<Item> itemCncChangeList = uniqueItemList.stream().filter(Item::isCncActivated).collect(Collectors.toList());
    itemCncChangeList.forEach(item -> item.setCncActivated(itemPickupPointViewConfigBaseRequest.isCncActivated()));
    itemService.saveItems(itemCncChangeList);
    return itemCncChangeList;

  }

  private void setItemViewConfigsByItemPickupPointViewConfigBaseRequest(
      ItemViewConfig itemViewConfig, ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) {
    if (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) {
      itemViewConfig.setBuyable(itemPickupPointViewConfigBaseRequest.isBuyable());
      itemViewConfig.setDiscoverable(itemPickupPointViewConfigBaseRequest.isDiscoverable());
    }
  }

  @Override
  public Page<ItemPickupPoint> findItemPickupPointByItemSkus(String storeId, ItemRequestV2 itemRequestV2, int page, int pageSize,
      boolean excludeDistributionPickupPoint) {
    Pageable pageable;
    if (Constants.ITEM_SKU.equalsIgnoreCase(itemRequestV2.getSortField())) {
      if (Constants.DESC.equalsIgnoreCase(itemRequestV2.getSortOrder())) {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.DESC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID));
      } else {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.ASC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID));
      }
    } else if (Constants.CREATED_DATE.equalsIgnoreCase(itemRequestV2.getSortField())) {
      if (Constants.DESC.equalsIgnoreCase(itemRequestV2.getSortOrder())) {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.DESC, Constants.CREATED_DATE, Constants.OFFLINE_ITEM_ID));
      } else {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.ASC, Constants.CREATED_DATE, Constants.OFFLINE_ITEM_ID));
      }
    } else if (Constants.FBB_ACTIVATED.equalsIgnoreCase(itemRequestV2.getSortField())) {
      if (Constants.DESC.equalsIgnoreCase(itemRequestV2.getSortOrder())) {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.DESC, Constants.FBB_ACTIVATED, Constants.OFFLINE_ITEM_ID));
      } else {
        pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.ASC, Constants.FBB_ACTIVATED, Constants.OFFLINE_ITEM_ID));
      }
    } else {
      pageable =
        PageRequest.of(page, pageSize, Sort.by(Sort.DEFAULT_DIRECTION, Constants.OFFLINE_ITEM_ID));
    }
    if (excludeDistributionL5 && excludeDistributionPickupPoint) {
      return itemPickupPointRepository.findByStoreIdAndItemSkuInAndDistributionFalseOrMissingAndMarkForDeleteFalse(
          storeId, itemRequestV2.getItemSkuList(), pageable);
    }
    return itemPickupPointRepository
      .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemRequestV2.getItemSkuList(),
        pageable);
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByProductSkus(String storeId, List<String> productSku) {
    if (CollectionUtils.isNotEmpty(productSku)) {
      return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSku);
    }
    return new ArrayList<>();
  }

  @Override
  public List<ItemPickupPoint> findItemPickupPointByProductSkusAndPickupPointCodes(String storeId,
      List<String> productSku, List<String> pickupPointCodes) {
    if (CollectionUtils.isNotEmpty(productSku)) {
      if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
        return itemPickupPointRepository.findByStoreIdAndProductSkuInAndPickupPointCodeInAndMarkForDeleteFalse(
            storeId, productSku, pickupPointCodes);
      } else {
        return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSku);
      }
    }
    return new ArrayList<>();
  }

  @Override
  public Page<ItemPickupPoint> findItemPickupPointByProductSkusOrItemSkus(String storeId,
      List<String> productSku, List<String> itemSku, int page, int pageSize) {
    Pageable pageable = PageRequest.of(page,pageSize,Sort.by(Constants.OFFLINE_ITEM_ID));
    if(CollectionUtils.isNotEmpty(productSku)){
      return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
          productSku, pageable);
    } else {
      return itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
          itemSku, pageable);
    }
  }


  public Page<ItemPickupPoint> findAllItemPickupPointByProductSkusOrItemSkus(String storeId, List<String> productSku,
      List<String> itemSku, int page, int pageSize) {
    Pageable pageable = PageRequest.of(page, pageSize, Sort.by(Constants.OFFLINE_ITEM_ID));
    if (CollectionUtils.isNotEmpty(productSku)) {
      return itemPickupPointRepository.findByStoreIdAndProductSkuIn(storeId, productSku, pageable);
    } else {
      return itemPickupPointRepository.findByStoreIdAndItemSkuIn(storeId, itemSku, pageable);
    }
  }


  @Override
  public Page<ItemPickupPoint> findItemPickupPointByProductSkusOrItemSkusForFbb(String storeId,
      List<String> productSku, List<String> itemSku, boolean fbbActivated, int page, int pageSize) {
    Pageable pageable = PageRequest.of(page,pageSize,Sort.by(Constants.OFFLINE_ITEM_ID));
    if(CollectionUtils.isNotEmpty(productSku)){
      return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndFbbActivated(storeId,
          productSku, pageable, fbbActivated);
    } else {
      return itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(storeId,
          itemSku, pageable, fbbActivated);
    }
  }

  @Override
  public Page<ItemPickupPoint> findItemPickupPointByProductSkuOrItemSku(String storeId,
      String productSku, String itemSku, int page, int pageSize) {
    Pageable pageable = PageRequest.of(page,pageSize,Sort.by(Constants.OFFLINE_ITEM_ID));
    if(StringUtils.isNotBlank(productSku)){
      return itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, pageable);
    } else {
      return itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, pageable);
    }
  }

  private void validateItemPickupPointRequest(
    List<ItemPickupPointRequest> itemPickupPointRequestList, String storeId) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(itemPickupPointRequestList),
      ErrorMessages.ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY);
    for (ItemPickupPointRequest itemPickupPointRequest : itemPickupPointRequestList) {
      checkArgument(StringUtils.isNotBlank(itemPickupPointRequest.getItemSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(itemPickupPointRequest.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
  }


  @Override
  public void republishItemPickupPointToAgp(List<ItemPickupPointRequest> itemPickupPointRequestList,
    String storeId, boolean republishToAgp) {
    validateItemPickupPointRequest(itemPickupPointRequestList, storeId);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    for (ItemPickupPointRequest request : itemPickupPointRequestList) {
      ItemPickupPoint itemPickupPoint =
        findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, request.getItemSku(),
          request.getPickupPointCode());
      if (Objects.nonNull(itemPickupPoint)) {
        itemPickupPointList.add(itemPickupPoint);
      }
    }
    if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (republishToAgp) {
          saveAndPublishService.publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
        } else {
          saveAndPublishService.publishItemPickupPointDataChangeEvent(Collections.singletonList(itemPickupPoint),
              new ArrayList<>(), Collections.EMPTY_MAP);
        }
      }
    } else {
      log.error("Publishing data to AGP failed to Empty list received from the database ");
    }
  }

  @Override
  public Page<ItemPickupPoint> getItemPickupPointListingByProductSku(String storeId, int page, int size,
      ItemPickupPointListingRequestVo itemPickupPointListingRequestVo) {
    return itemPickupPointRepository.getItemPickupPointListingByProductSku(storeId, page, size,
        itemPickupPointListingRequestVo);
  }

  @Override
  public Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.countByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
  }

  @Override
  public Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.countByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, true);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(String storeId,
      Set<String> itemSkus, boolean fbbActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndFbbActivatedAndMarkForDeleteFalse(storeId, itemSkus,
        fbbActivated, false);
  }


  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimary(String storeId,
      Set<String> itemSkus, boolean fbbActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndFbbActivatedAndMarkForDeleteFalse(storeId, itemSkus,
        fbbActivated, true);
  }

  @Override
  public Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.countByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
  }

  @Override
  public Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(String storeId, String itemSku,
      boolean cncActive) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(storeId, itemSku, cncActive, false);
  }

  @Override
  public Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId, String itemSku,
      String cncChannel) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(storeId, itemSku, cncChannel, false);
  }

  @Override
  public Boolean existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId,
      String itemSku, String cncChannel) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.existsByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(
        storeId, itemSku, cncChannel);
  }

  @Override
  public Long findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimary(String storeId, String itemSku,
      boolean cncActive) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(storeId, itemSku, cncActive, true);
  }

  @Override
  public Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(String storeId, String productSku,
      boolean fbbActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository
        .countByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(storeId, productSku, fbbActivated);
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointByItemSkuInAndPickupPointCode(String storeId, Set<String> itemSkus,
      String pickupPointCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointCode),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSkus,
        pickupPointCode);
  }

  @Override
  public Map<String, List<ItemPickupPoint>> getItemPickupPointByProductSkuList(String storeId,
      List<String> productSkuList) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productSkuList), ErrorMessages.PRODUCT_SKU_EMPTY);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkuList);
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
        .collect(Collectors.groupingBy(ItemPickupPoint::getProductSku));
  }

  @Override
  public Map<String, List<ItemPickupPoint>> getItemPickupPointByProductSkuListAndDeliveryTrue(String storeId,
      List<String> productSkuList) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productSkuList), ErrorMessages.PRODUCT_SKU_EMPTY);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(storeId,
            productSkuList);
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
        .collect(Collectors.groupingBy(ItemPickupPoint::getProductSku));
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointByItemSkuInAndDeliveryTrue(String storeId, List<String> itemSkuList) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkuList), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointRepository.findByStoreIdAndItemSkuInAndDeliveryTrueAndMarkForDeleteFalse(storeId, itemSkuList);
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>());
  }

  @Override
  public Page<ItemPickupPoint> getItemPickupPointByStoreIdAndProductSkuInAndDeliveryTrue(
    String storeId, String productSku, int page, int size) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_CODE_AND_PRODUCT_SKU_MUST_NOT_BE_NULL);
    Pageable pageable = PageRequest.of(page, size, Sort.by(Constants.ITEM_SKU));
    Page<ItemPickupPoint> itemPickupPoints =  itemPickupPointRepository
      .findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(storeId, productSku, pageable);
    return Optional.ofNullable(itemPickupPoints).orElse(new PageImpl<>(Collections.emptyList()));
  }

@Override
public ItemBasicDetailResponse getBasicItemDetails(String storeId, int page, int size, String itemSku) {
  GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
  GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
  Page<ItemPickupPoint> itemPickupPoints =
    itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku,
      PageRequest.of(page, size, Sort.by(Constants.OFFLINE_ITEM_ID)));
  GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoints),
      ErrorMessages.CANNOT_FIND_ITEM_BY_ITEM_SKU);
  GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints.getContent()),
      ErrorMessages.CANNOT_FIND_ITEM_BY_ITEM_SKU);
  String productSku = itemPickupPoints.getContent().get(0).getProductSku();
  Product product = productService.getProduct(storeId, productSku);
  GdnPreconditions.checkArgument(Objects.nonNull(product),
      ErrorMessages.CANNOT_FIND_ITEM_BY_ITEM_SKU);
  return objectConverterService.convertItemPickupPointsAndProductToItemBasicDetailResponse(itemPickupPoints, product,
      itemSku);
}

  @Override
  public void deleteItemPickupPoints(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    itemPickupPointRepository.deleteItemPickupPointByItemSku(storeId, itemSku);
  }

  @Override
  public List<ProductSkuPickupPointResponse> getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(String storeId,
      List<String> productSkuList, boolean republish) {
    List<ProductSkuPickupPointResponse> productSkuPickupPointResponses = new ArrayList<>();
    for (String productSku : productSkuList) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(storeId, productSku, null,
              cncForWarehouseFeatureSwitch);
      if (Objects.nonNull(itemPickupPoint)) {
        ProductSkuPickupPointResponse productSkuPickupPointResponse = new ProductSkuPickupPointResponse();
        productSkuPickupPointResponse.setProductSku(productSku);
        productSkuPickupPointResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        productSkuPickupPointResponse.setItemSku(itemPickupPoint.getItemSku());
        productSkuPickupPointResponse.setItemViewConfig(itemPickupPoint.getAllItemViewConfigs());
        productSkuPickupPointResponses.add(productSkuPickupPointResponse);
        if (republish) {
          saveAndPublishService.publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
        }
      }
    }
    return productSkuPickupPointResponses;
  }

  @Override
  public List<ItemSkuPickupPointResponse> getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(String storeId,
      List<String> itemSkuList, boolean republish) {
    List<ItemSkuPickupPointResponse> itemSkuPickupPointResponses = new ArrayList<>();
    for (String itemSku : itemSkuList) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(storeId, null, itemSku,
              cncForWarehouseFeatureSwitch);
      if (Objects.nonNull(itemPickupPoint)) {
        ItemSkuPickupPointResponse itemSkuPickupPointResponse = new ItemSkuPickupPointResponse();
        itemSkuPickupPointResponse.setItemSku(itemPickupPoint.getItemSku());
        itemSkuPickupPointResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        itemSkuPickupPointResponse.setItemViewConfig(itemPickupPoint.getAllItemViewConfigs());
        itemSkuPickupPointResponses.add(itemSkuPickupPointResponse);
        if (republish) {
          saveAndPublishService.publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
        }
      }
    }
    return itemSkuPickupPointResponses;
  }

  @Override
  public Page<ProductSkuPickupPointResponseV2> getProductSkuListByBusinessPartnerAndPickupPointCode(
    String storeId, String merchantCode, String pickupPointCode, int page, int pageSize) {
    checkArgument(StringUtils.isNotBlank(merchantCode),
      ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode),
      ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    Page<ItemPickupPoint> itemPickupPointList = itemPickupPointRepository
      .findByStoreIdAndMerchantCodeAndPickupPointCodeAndMarkForDeleteFalse(storeId, merchantCode,
        pickupPointCode, PageRequest.of(page, pageSize, Sort.by(Constants.ITEM_SKU)));
    List<ProductSkuPickupPointResponseV2> productSkuPickupPointResponses =
      CommonUtil.getProductSkuPickUpPointList(itemPickupPointList.getContent());
    return new PageImpl<>(productSkuPickupPointResponses, PageRequest.of(page, pageSize),
      itemPickupPointList.getTotalElements());
  }

  @Override
  public MinMaxItemPriceResponse getMinAndMaxOfferPrice(String storeId, String productCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode),
      ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    Product product = productService.findByStoreIdAndProductCode(storeId,productCode).get(0);
    List<ItemPickupPoint> itemPickupPointList =
      itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, product.getProductSku(), false);
    List<Double> offerPrices = itemPickupPointList.stream().map(
        itemPickupPoint -> Optional.ofNullable(itemPickupPoint.getPrice()).orElse(new HashSet<>()))
      .collect(Collectors.toList()).stream().filter(CollectionUtils::isNotEmpty)
      .map(prices -> prices.iterator().next().getOfferPrice()).collect(Collectors.toList());
    return MinMaxItemPriceResponse.builder().maxPrice(
        offerPrices.stream().mapToDouble(Double::doubleValue).reduce(Math::max).getAsDouble())
      .minPrice(
        offerPrices.stream().mapToDouble(Double::doubleValue).reduce(Math::min).getAsDouble())
      .productSku(product.getProductSku()).build();
  }

  @Override
  public CreateFbbPickupPointResponse createFbbPickupPoint(String storeId,
      CreateFbbPickupPointRequest createFbbPickupPointRequest, Item item,
      CreateFbbPickupPointResponse createFbbPickupPointResponse) {
    ItemPickupPoint fbbTrueItemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId,
            createFbbPickupPointRequest.getItemSku());
    if (Objects.nonNull(fbbTrueItemPickupPoint)) {
      createFbbPickupPointResponse.setReason(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS);
      createFbbPickupPointResponse.setErrorCode(ApiErrorCode.FBB_PICKUP_POINT_ALREADY_EXISTS.getCode());
      return createFbbPickupPointResponse;
    }
    ItemPickupPoint existingItemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(storeId,
            createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), false);
    if (Objects.nonNull(existingItemPickupPoint) && !existingItemPickupPoint.isMarkForDelete()) {
      createFbbPickupPointResponse.setReason(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS);
      createFbbPickupPointResponse.setErrorCode(ApiErrorCode.FBB_PICKUP_POINT_ALREADY_EXISTS.getCode());
      return createFbbPickupPointResponse;
    }
    ItemPickupPoint defaultItemPickupPoint =
        itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId,
            createFbbPickupPointRequest.getItemSku());
    List<ItemPickupPoint> allExistingL5ForItemSku =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId,
            createFbbPickupPointRequest.getItemSku(), false);
    ItemPickupPoint newFbbItemPickupPoint =
        getNewFbbItemPickupPoint(storeId, createFbbPickupPointRequest, defaultItemPickupPoint,
            Optional.ofNullable(existingItemPickupPoint).orElse(new ItemPickupPoint()), allExistingL5ForItemSku);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(newFbbItemPickupPoint.getMerchantCode(), newFbbItemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_ADDED, StringUtils.EMPTY, newFbbItemPickupPoint.getPickupPointCode(), null,
            newFbbItemPickupPoint.getProductSku(), item.getGeneratedItemName(),
            newFbbItemPickupPoint.getPickupPointCode(), false);
    saveItemPickupPointAndPublishAndCaptureHistory(newFbbItemPickupPoint, item, List.of(auditTrailDto),
        Constants.DEFAULT_USERNAME, false);
    return createFbbPickupPointResponse;
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(
    String storeId, String itemSku) {
    return itemPickupPointRepository
      .findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId, itemSku);
  }

  private void saveItemPickupPointAndPublishAndCaptureHistory(ItemPickupPoint itemPickupPoint, Item item,
      List<AuditTrailDto> auditTrailDtoList, String changedBy, boolean priceFromInRegionPpCode) {
    if(StringUtils.isEmpty(itemPickupPoint.getMerchantSku())){
      itemPickupPoint.setMerchantSku(item.getMerchantSku());
    }
    itemPickupPoint = saveItemPickupPoint(itemPickupPoint);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    publishItemPickupPointDataChangeEventWithPureCncStatusChange(itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    publishEventForHistory(auditTrailDtoList, changedBy, priceFromInRegionPpCode);
  }

  private void publishEventForHistory(List<AuditTrailDto> auditTrailDtoList, String changedBy,
      boolean priceFromInRegionPpCode) {
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(changedBy);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    auditTrailListResponse.setPriceFromInRegionPpCode(priceFromInRegionPpCode);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }

  private static ItemPickupPoint getNewFbbItemPickupPoint(String storeId,
      CreateFbbPickupPointRequest createFbbPickupPointRequest, ItemPickupPoint defaultItemPickupPoint,
      ItemPickupPoint newFbbItemPickupPoint, List<ItemPickupPoint> existingL5s) {
    newFbbItemPickupPoint.setItemSku(createFbbPickupPointRequest.getItemSku());
    newFbbItemPickupPoint.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    ItemPickupPoint maxPriceFromExistingL5s = getMaximumPriceFromExistingL5s(existingL5s);
    // setting price for L5s based on the maximum price among all other existing L5s
    newFbbItemPickupPoint.setPrice(Collections.singleton(getPrice(Constants.DEFAULT_CHANNEL,
        maxPriceFromExistingL5s.getPrice().stream().findFirst()
            .orElseThrow(() -> new ValidationException("No Offer price found")).getOfferPrice(),
        maxPriceFromExistingL5s.getPrice().stream().findFirst()
            .orElseThrow(() -> new ValidationException("No List price found")).getListPrice())));
    ItemViewConfig itemViewConfig =
        getItemViewConfig(Constants.DEFAULT_CHANNEL, createFbbPickupPointRequest.isBuyable(),
            createFbbPickupPointRequest.isDiscoverable());
    newFbbItemPickupPoint.setItemViewConfig(Collections.singleton(itemViewConfig));
    newFbbItemPickupPoint.setMerchantSku(defaultItemPickupPoint.getMerchantSku());
    newFbbItemPickupPoint.setProductSku(defaultItemPickupPoint.getProductSku());
    newFbbItemPickupPoint.setMerchantCode(defaultItemPickupPoint.getMerchantCode());
    newFbbItemPickupPoint.setOfflineItemId(createFbbPickupPointRequest.getItemSku().concat(Constants.HYPHEN)
        .concat(createFbbPickupPointRequest.getPickupPointCode()));
    // this we are doing only in case that fbb l5 was mfd=true and some of these flags were true
    newFbbItemPickupPoint.setCncActive(false);
    newFbbItemPickupPoint.setMarkForDelete(false);
    newFbbItemPickupPoint.setWholesalePriceExists(false);
    newFbbItemPickupPoint.setFlashSaleActive(false);
    newFbbItemPickupPoint.setPromoBundling(false);
    newFbbItemPickupPoint.setMerchantPromoDiscount(false);
    newFbbItemPickupPoint.setFbbActivated(true);
    newFbbItemPickupPoint.setNewData(true);
    newFbbItemPickupPoint.setStoreId(storeId);
    return newFbbItemPickupPoint;
  }

  private static Price getPrice(String channel, double offerPrice, double listPrice) {
    Price price = new Price();
    price.setChannel(channel);
    price.setListPrice(listPrice);
    price.setOfferPrice(offerPrice);
    return price;
  }


  private static ItemPickupPoint getMaximumPriceFromExistingL5s(List<ItemPickupPoint> existingL5s) {
    return existingL5s.stream().max(Comparator.comparing(
            itemPickupPoint -> itemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getOfferPrice()))
        .orElse(ItemPickupPoint.builder().newData(true).price(new HashSet<>()).itemViewConfig(new HashSet<>()).build());
  }

  private static ItemViewConfig getItemViewConfig(String channel, boolean buyable, boolean discoverable) {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(buyable);
    itemViewConfig.setDiscoverable(discoverable);
    itemViewConfig.setChannel(channel);
    return itemViewConfig;
  }
  @Override
  public List<ItemPickupPoint> updateFbbFlag(List<ItemPickupPoint> itemPickupPointList) {
    List<ItemPickupPoint> newFbbTrueItemPickupPoints =
        itemPickupPointList.stream().filter(ItemPickupPoint::isFbbActivated).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(newFbbTrueItemPickupPoints)) {
      Set<String> newFbbTrueItemSkus =
          newFbbTrueItemPickupPoints.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet());
      Set<String> newFbbTrueItemPickupPointIds =
          newFbbTrueItemPickupPoints.stream().map(ItemPickupPoint::getId).collect(Collectors.toSet());
      List<ItemPickupPoint> existingFbbTruePickupPoints =
          itemPickupPointRepository.findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(newFbbTrueItemSkus);
      if (CollectionUtils.isNotEmpty(existingFbbTruePickupPoints)) {
        existingFbbTruePickupPoints.removeIf(
            itemPickupPoint -> newFbbTrueItemPickupPointIds.contains(itemPickupPoint.getId()));
        existingFbbTruePickupPoints.forEach(itemPickupPoint -> itemPickupPoint.setMarkForDelete(true));
        return existingFbbTruePickupPoints;
      } else {
        return new ArrayList<>();
      }
    } else {
      return new ArrayList<>();
    }
  }

  @Override
  public Set<String> pickupPointsAddedToOtherVariants(String productSku, Set<String> offlineItemIds) {
    if (CollectionUtils.isNotEmpty(offlineItemIds)) {
      return Optional.ofNullable(
              itemPickupPointRepository.findByProductSkuAndOfflineItemIdNotInAndMarkForDeleteFalse(
                  productSku, offlineItemIds)).orElse(new ArrayList<>()).stream().map(ItemPickupPoint::getPickupPointCode)
          .collect(Collectors.toSet());
    }
    return new HashSet<>();
  }

  @Override
  public Page<ItemPickupPoint> findItemPickupPointByProductSkus(String storeId,
      List<String> productSku, Boolean cncActivated, Integer page, Integer pageSize) {
    Pageable pageable = PageRequest.of(page, pageSize, Sort.by(Constants.OFFLINE_ITEM_ID));
    if (Boolean.TRUE.equals(cncActivated)) {
      if (cncForWarehouseFeatureSwitch) {
        return itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncBuyable(
            storeId, productSku, channelService.getCncChannel(), page, pageSize,
            Sort.by(Constants.OFFLINE_ITEM_ID));
      } else {
        return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndCncActiveTrue(
            storeId, productSku, pageable);
      }
    } else {
      return itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
          productSku, pageable);
    }
  }

  @Override
  public AutoCreatePickupPointResponse autoCreateL5(AutoCreatePickupPointRequest autoCreatePickupPointRequest,
      MandatoryRequestParam mandatoryRequestParam, Item item,
      AutoCreatePickupPointResponse autoCreatePickupPointResponse, ProductType productType) {
    ItemPickupPoint existingItemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku(), autoCreatePickupPointRequest.getPickupPointCode(), false);
    if (skipAutoCreateForExistingL5 && Objects.nonNull(existingItemPickupPoint)) {
      autoCreatePickupPointResponse.setNewlyCreated(false);
      return pickupPointHelperService.getAutoCreatePickupPointResponse(autoCreatePickupPointResponse, existingItemPickupPoint);
    }
    ItemPickupPoint itemPickupPoint = getAutoCreatedItemPickupPoint(autoCreatePickupPointRequest, mandatoryRequestParam,
        autoCreatePickupPointResponse, existingItemPickupPoint, item);
    boolean priceFromInRegionPpCode = false;
    if (CollectionUtils.isNotEmpty(autoCreatePickupPointRequest.getPpCodeInRegion()) && autoCreatePickupPointRequest.isDeliveryFlag()) {
      List<ItemPickupPoint> existingL5InRegion =
          getExistingOnlineItemPickupPointsInSameRegion(autoCreatePickupPointRequest, mandatoryRequestParam);
      if (CollectionUtils.isNotEmpty(existingL5InRegion)) {
        findMaxPriceItemPickupPointAndSetPriceAndViewConfig(existingL5InRegion, itemPickupPoint, autoCreatePickupPointRequest.isDeliveryFlag());
        priceFromInRegionPpCode = true;
        if (ProductType.BOPIS.equals(productType)) {
          setStatusAsOffline(itemPickupPoint, autoCreatePickupPointRequest.isDeliveryFlag());
        }
      } else {
        findAllExistingL5ForItemAndSettingPriceAndViewConfig(autoCreatePickupPointRequest, mandatoryRequestParam,
            itemPickupPoint);
      }
    } else {
      findAllExistingL5ForItemAndSettingPriceAndViewConfig(autoCreatePickupPointRequest, mandatoryRequestParam,
          itemPickupPoint);
    }
    AuditTrailDto auditTrailDto = new AuditTrailDto(itemPickupPoint.getMerchantCode(), itemPickupPoint.getItemSku(),
        Constants.PICKUP_POINT_AUTO_CREATED, StringUtils.EMPTY, itemPickupPoint.getPickupPointCode(), null,
        itemPickupPoint.getProductSku(), item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false);
    saveItemPickupPointAndPublishAndCaptureHistory(itemPickupPoint, item, List.of(auditTrailDto),
        mandatoryRequestParam.getUsername(), priceFromInRegionPpCode);
    return pickupPointHelperService.getAutoCreatePickupPointResponse(autoCreatePickupPointResponse, itemPickupPoint);
  }

  private void findAllExistingL5ForItemAndSettingPriceAndViewConfig(
      AutoCreatePickupPointRequest autoCreatePickupPointRequest, MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPoint itemPickupPoint) {
    List<ItemPickupPoint> allExistingL5ForItemSku =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku(), false);
    findMaxPriceItemPickupPointAndSetPriceAndViewConfig(allExistingL5ForItemSku, itemPickupPoint,
        autoCreatePickupPointRequest.isDeliveryFlag());
    setStatusAsOffline(itemPickupPoint, autoCreatePickupPointRequest.isDeliveryFlag());
  }

  private void findMaxPriceItemPickupPointAndSetPriceAndViewConfig(List<ItemPickupPoint> existingL5ToCompare,
      ItemPickupPoint itemPickupPoint, boolean deliveryFlag) {
    ItemPickupPoint resultMaxPriceItemPickupPoint = getMaxPriceItemPickupPoint(existingL5ToCompare);
    itemPickupPoint.setPrice(Collections.singleton(getPrice(channelService.getDefaultChannel(),
        resultMaxPriceItemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getOfferPrice(),
        resultMaxPriceItemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getListPrice())));
    pickupPointHelperService.setItemViewConfig(itemPickupPoint, deliveryFlag, resultMaxPriceItemPickupPoint);
  }

  private boolean isOnlineItemPickupPoint(ItemPickupPoint itemPickupPoint) {
    ItemViewConfig itemViewConfig =
        itemPickupPoint.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig());
    return itemViewConfig.isDiscoverable() && itemViewConfig.isBuyable();
  }

  private ItemPickupPoint getMaxPriceItemPickupPoint(List<ItemPickupPoint> itemPickupPoints) {
    return itemPickupPoints.stream().max(Comparator.comparing(
            itemPickupPoint -> itemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getOfferPrice()))
        .orElse(ItemPickupPoint.builder().newData(true).price(new HashSet<>()).itemViewConfig(new HashSet<>()).build());
  }

  private void setStatusAsOffline(ItemPickupPoint itemPickupPoint, boolean isDeliveryFlag) {

    if(cncForWarehouseFeatureSwitch) {
      ItemViewConfig defaultViewConfig = new ItemViewConfig();
      ItemViewConfig cncViewConfig = new ItemViewConfig();
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();

      if(Boolean.FALSE.equals(isDeliveryFlag)) {
        defaultViewConfig.setDiscoverable(false);
        defaultViewConfig.setBuyable(false);
        defaultViewConfig.setChannel(channelService.getDefaultChannel());
        itemViewConfigSet.add(defaultViewConfig);
        itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      } else {
        defaultViewConfig.setDiscoverable(false);
        defaultViewConfig.setBuyable(false);
        defaultViewConfig.setChannel(channelService.getDefaultChannel());
        cncViewConfig.setDiscoverable(false);
        cncViewConfig.setBuyable(false);
        cncViewConfig.setChannel(channelService.getDefaultChannel());
        itemViewConfigSet.add(defaultViewConfig);
        itemViewConfigSet.add(cncViewConfig);
        itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      }

    } else {
      itemPickupPoint.setItemViewConfig(
          itemPickupPoint.getItemViewConfig().stream().map(this::changeViewConfigOffline).collect(Collectors.toSet()));
    }
  }

  private ItemViewConfig changeViewConfigOffline(ItemViewConfig itemViewConfig) {
    itemViewConfig.setDiscoverable(false);
    itemViewConfig.setBuyable(false);
    return itemViewConfig;
  }

  private ItemPickupPoint getAutoCreatedItemPickupPoint(AutoCreatePickupPointRequest autoCreatePickupPointRequest,
      MandatoryRequestParam mandatoryRequestParam, AutoCreatePickupPointResponse autoCreatePickupPointResponse,
      ItemPickupPoint existingItemPickupPoint, Item item) {
    ItemPickupPoint itemPickupPoint = Optional.ofNullable(existingItemPickupPoint)
        .orElse(ItemPickupPoint.builder().newData(true).price(new HashSet<>()).itemViewConfig(new HashSet<>()).build());
    if (Boolean.TRUE.equals(itemPickupPoint.getNewData())) {
      autoCreatePickupPointResponse.setNewlyCreated(true);
      itemPickupPoint.setStoreId(mandatoryRequestParam.getStoreId());
      itemPickupPoint.setOfflineItemId(CommonUtil.generateOfflineItemId(autoCreatePickupPointRequest.getWebItemSku(),
          autoCreatePickupPointRequest.getPickupPointCode()));
      itemPickupPoint.setItemSku(autoCreatePickupPointRequest.getWebItemSku());
      itemPickupPoint.setPickupPointCode(autoCreatePickupPointRequest.getPickupPointCode());
      itemPickupPoint.setMerchantCode(autoCreatePickupPointRequest.getMerchantCode());
      itemPickupPoint.setProductSku(autoCreatePickupPointRequest.getWebProductSku());
    }
    itemPickupPoint.setNewData(true);
    itemPickupPoint.setFbbActivated(true);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setMerchantSku(item.getMerchantSku());
    return itemPickupPoint;
  }

  private List<ItemPickupPoint> getExistingOnlineItemPickupPointsInSameRegion(
      AutoCreatePickupPointRequest autoCreatePickupPointRequest, MandatoryRequestParam mandatoryRequestParam) {
    List<ItemPickupPoint> existingL5InRegion = Optional.ofNullable(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
            mandatoryRequestParam.getStoreId(), autoCreatePickupPointRequest.getWebItemSku(),
            new ArrayList<>(autoCreatePickupPointRequest.getPpCodeInRegion()))).orElse(new ArrayList<>());
    existingL5InRegion = existingL5InRegion.stream().filter(this::isOnlineItemPickupPoint).collect(Collectors.toList());
    return existingL5InRegion;
  }

  private void deleteItemSkuPickupPoint(List<DeleteOfflineItemVO> result, String merchantCode,
    String userName) {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> requestList =
      CommonUtil.formInvRequestForPickupPointDelete(result, merchantCode);
    List<List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO>> inventoryPartitionDeleteList =
      Lists.partition(requestList, inventoryDeleteBatchSize);
    for (List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> inventoryList : inventoryPartitionDeleteList) {
      try {
        inventoryOutbound.deleteByItemSkuAndPickupPointCode(Constants.DEFAULT_CLIENT_ID_X_PRODUCT, userName,
            inventoryList);
      } catch (Exception e) {
        log.error("Exception occurred with message : {} ", ErrorMessages.ERROR_WHILE_DELETING_PP_CODE_FROM_INVENTORY,
            e);
      }
    }
  }

  @Override
  public List<ItemSkuPickupPointCodeResponse> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId,
      List<String> itemSkus) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    GdnPreconditions.checkArgument(itemSkus.size() < itemPickupPointMaxFetchSize, ErrorMessages.ITEM_LIST_TOO_LARGE);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(storeId, itemSkus);
    List<ItemPickupPoint> result = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      ItemViewConfig itemViewConfig =
          itemPickupPoint.getItemViewConfig().stream().findFirst().orElseGet(ItemViewConfig::new);
      if (Boolean.TRUE.equals(CommonUtil.getBuyableFromItemViewConfigConfig(itemViewConfig))) {
        result.add(itemPickupPoint);
      }
    }
    return result.stream().map(item -> new ItemSkuPickupPointCodeResponse(item.getItemSku(), item.getPickupPointCode()))
        .collect(Collectors.toList());
  }

  @Override
  public Map<String, Boolean> getItemSkuFbbMap(String storeId, Set<String> itemSkus) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    Map<String, Boolean> itemSkuFbbMap = new HashMap<>();
    for (String itemSku : itemSkus) {
      ItemPickupPoint itemPickupPoints =
          itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId,
              itemSku);
      itemSkuFbbMap.put(itemSku, Optional.ofNullable(itemPickupPoints).orElse(new ItemPickupPoint()).isFbbActivated());
    }
    return itemSkuFbbMap;
  }

  @Override
  public void deleteItemPickupPointsFromInventory(List<ItemPickupPoint> itemPickupPointToBeDeleted) {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request =
        CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(itemPickupPointToBeDeleted);
    try {
      inventoryOutbound.deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), request);
    } catch (Exception e) {
      log.error("Exception occurred with itemPickupPointToBeDeleted : {} ", itemPickupPointToBeDeleted, e);
    }
  }

  @Override
  public List<ItemPickupPoint> findOneForEachItemSkuIn(String storeId, List<String> itemSkus) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    for (String itemSku : itemSkus) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
      itemPickupPoints.add(itemPickupPoint);
    }
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPoints),
        ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_EMPTY);
    return itemPickupPoints;
  }


  @Override
  public void updateSubscriptionFlag(BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest) {
    ItemPickupPoint itemPickupPoint = itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(
        blimartSubscriptionChangeRequest.getStoreId(), blimartSubscriptionChangeRequest.getItemSku(),
        blimartSubscriptionChangeRequest.getPickupPointCode(), true);
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    Item savedItem = this.itemService.findByStoreIdAndItemSku(blimartSubscriptionChangeRequest.getStoreId(),
        blimartSubscriptionChangeRequest.getItemSku());
    GdnPreconditions.checkArgument(Objects.nonNull(savedItem), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    itemPickupPoint.setSubscribable(blimartSubscriptionChangeRequest.isSubscribable());
    itemPickupPointRepository.save(itemPickupPoint);
    updateItem(blimartSubscriptionChangeRequest, savedItem);
    this.cacheEvictHelperService.evictItemCache(savedItem.getStoreId(), savedItem);
    this.cacheEvictHelperService.evictItemPickupPointData(savedItem.getStoreId(), itemPickupPoint,
        itemPickupPoint.getPickupPointCode());
  }

  @Override
  public List<ItemPickupPoint> fetchBasicDetailsByItemSkuAndPickupPointCodeList(String storeId,
    List<ItemPickupPointRequest> itemPickupPointRequest) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPointRequest),
      ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_FOR_THE_REQUEST);
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoList =
      ProductAndItemsUtil.toItemPickupPointRequestVo(itemPickupPointRequest);
    return Lists.partition(itemPickupPointRequestVoList, itemPickupPointFetchSize).stream().flatMap(
      itemPickupPointRequestVoSubList -> itemPickupPointRepository.fetchBasicDataByItemSkuAndPickupPointCode(
        storeId, itemPickupPointRequestVoSubList).stream()).collect(Collectors.toList());
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
      String storeId, Set<String> offlineItemIds, boolean buyable, String channel, int limit) {
    return itemPickupPointRepository.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
        storeId, offlineItemIds, buyable, channel, limit);
  }

  @Override
  public ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(String storeId,
      String productSku, boolean cncActivated) {
    return itemPickupPointRepository.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(
        storeId, productSku, cncActivated);
  }

  private void updateItem(BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest, Item savedItem) {
    boolean itemSaveRequired = false;
    if (blimartSubscriptionChangeRequest.isSubscribable() && !savedItem.isSubscribable()) {
      itemSaveRequired = true;
      savedItem.setSubscribable(true);
    }
    if (!blimartSubscriptionChangeRequest.isSubscribable()) {
      List<ItemPickupPoint> itemPickupPointList =
          itemPickupPointRepository.findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(
              blimartSubscriptionChangeRequest.getStoreId(), blimartSubscriptionChangeRequest.getItemSku(), true, true);
      if (CollectionUtils.isEmpty(itemPickupPointList)) {
        itemSaveRequired = true;
        savedItem.setSubscribable(false);
      }
    }
    if (itemSaveRequired) {
      itemService.saveItems(Collections.singletonList(savedItem));
    }
  }

  @Override
  public void updatePwpFlagsByItemSkuAndPickupPointCode(PricingPwpPromoEvent pricingPwpPromoEvent) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pricingPwpPromoEvent.getItemSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pricingPwpPromoEvent.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(pricingPwpPromoEvent.getStoreId(),
            pricingPwpPromoEvent.getItemSku(), pricingPwpPromoEvent.getPickupPointCode(), true);
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint),
        String.format(ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
            pricingPwpPromoEvent.getItemSku(), pricingPwpPromoEvent.getPickupPointCode()));
    setPwpPromoList(pricingPwpPromoEvent, itemPickupPoint);
    itemPickupPointRepository.save(itemPickupPoint);
    Product product =
        productService.findByStoreIdAndProductSku(pricingPwpPromoEvent.getStoreId(), itemPickupPoint.getProductSku());
    productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, new ArrayList<>(), false);
  }

  private static void setPwpPromoList(PricingPwpPromoEvent pricingPwpPromoEvent, ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.setActivePromoBundlings(
        Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>()));
    if (Boolean.TRUE.equals(pricingPwpPromoEvent.getIsPartOfPendingPWPAsMain())) {
      itemPickupPoint.getActivePromoBundlings().add(Constants.PWP_MAIN_PENDING);
    } else if (Boolean.FALSE.equals(pricingPwpPromoEvent.getIsPartOfPendingPWPAsMain())) {
      itemPickupPoint.getActivePromoBundlings().remove(Constants.PWP_MAIN_PENDING);
    }
    if (Boolean.TRUE.equals(pricingPwpPromoEvent.getIsPartOfActivePWPAsMain())) {
      itemPickupPoint.getActivePromoBundlings().add(Constants.PWP_MAIN_ACTIVE);
    } else if (Boolean.FALSE.equals(pricingPwpPromoEvent.getIsPartOfActivePWPAsMain())) {
      itemPickupPoint.getActivePromoBundlings().remove(Constants.PWP_MAIN_ACTIVE);
    }
    if (Boolean.TRUE.equals(pricingPwpPromoEvent.getIsPartOfPendingPWPAsAdditional())) {
      itemPickupPoint.getActivePromoBundlings().add(Constants.PWP_ADDITIONAL_PENDING);
    } else if (Boolean.FALSE.equals(pricingPwpPromoEvent.getIsPartOfPendingPWPAsAdditional())) {
      itemPickupPoint.getActivePromoBundlings().remove(Constants.PWP_ADDITIONAL_PENDING);
    }
    if (Boolean.TRUE.equals(pricingPwpPromoEvent.getIsPartOfActivePWPAsAdditional())) {
      itemPickupPoint.getActivePromoBundlings().add(Constants.PWP_ADDITIONAL_ACTIVE);
    } else if (Boolean.FALSE.equals(pricingPwpPromoEvent.getIsPartOfActivePWPAsAdditional())) {
      itemPickupPoint.getActivePromoBundlings().remove(Constants.PWP_ADDITIONAL_ACTIVE);
    }
  }

  @Override
  public void updateInsuredAmountInItemPickupPoint(String storeId, List<CogsUpdateRequest> cogsUpdateRequests) {
    GdnPreconditions.checkArgument(cogsUpdateRequests.size() <= cogsUpdateMaxRequestSize,
        ErrorMessages.ITEM_LIST_TOO_LARGE);
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    for (CogsUpdateRequest cogsUpdateRequest : cogsUpdateRequests) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId,
              cogsUpdateRequest.getItemSku(), cogsUpdateRequest.getPickupPointCode(), false);
      if (itemPickupPoint.getInsuredAmount() != cogsUpdateRequest.getInsuredAmount()) {
        log.info("Insured amount changed for L5 : {} amount {} ", itemPickupPoint.getOfflineItemId(),
            cogsUpdateRequest.getInsuredAmount());
        itemPickupPoint.setInsuredAmount(cogsUpdateRequest.getInsuredAmount());
        itemPickupPoints.add(itemPickupPoint);
      }
    }
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      itemPickupPointRepository.saveAll(itemPickupPoints);
      saveAndPublishService.publishItemPickupPointDataChangeEvent(itemPickupPoints,
          Collections.singletonList(ItemPickupPointChangeEventType.INSURED_AMOUNT_CHANGE),
        Collections.EMPTY_MAP);
    }
  }

  @Override
  public Page<CogsResponse> getCogsData(String storeId, String productSku, int page, int size) {
    Pageable pageable = PageRequest.of(page, size, Sort.by(Constants.OFFLINE_ITEM_ID));
    Page<ItemPickupPoint> itemPickupPoints =
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, pageable);
    return new PageImpl<>(
        Optional.ofNullable(itemPickupPoints).orElse(new PageImpl<>(new ArrayList<>())).getContent().stream().map(
            itemPickupPoint -> CogsResponse.builder().itemSku(itemPickupPoint.getItemSku())
                .pickupPointCode(itemPickupPoint.getPickupPointCode()).insuredAmount(itemPickupPoint.getInsuredAmount())
                .build()).collect(Collectors.toList()), PageRequest.of(page, size),
        Optional.ofNullable(itemPickupPoints).orElse(new PageImpl<>(new ArrayList<>())).getTotalElements());
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus) {
    return itemPickupPointRepository.findByStoreIdAndItemSkuInAndDistributionTrueAndMarkForDeleteFalse(storeId,
        itemSkus);
  }
}
