package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static java.util.function.Predicate.not;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.ProductPreOrderStatus;
import com.gdn.x.product.enums.Constants;

import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.ItemPickupPointDeleteRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemEditedDetailsDTO;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.DataSourceWrapperService;
import com.gdn.x.product.service.api.ItemPickupPointHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ItemPickupPointSummaryServiceImpl implements ItemPickupPointSummaryService {

  @Autowired
  private ItemPickupPointService pickupPointService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemPickupPointHelperService pickupPointHelperService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Value("${include.variant.update.enabled}")
  private boolean includeVariantUpdate;

  @Value("${include.b2b.field.change}")
  private boolean includeB2bFieldChange;

  @Value("${remove.duplicate.l5.before.save}")
  private boolean removeDuplicateL5BeforeSave;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private DataSourceWrapperService dataSourceWrapperService;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${missing.entity.switch.enabled}")
  private boolean missingEntitySwitch;

  @Value("${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value("${update.merchant.sku.from.newly.added.L5}")
  private boolean updatedMerchantSkuFromNewlyAddedL5;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Override
  public BasicProductAndItemEditedDetailsDTO updateItemPickupPointListing(String productSku, ProductType productType,
      List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVo, Set<Item> updatedItemSet,
      Map<String, Item> itemSkuAndItemMap, Map<String, ItemPickupPoint> itemPickupPointMap,
      Set<String> newlyAddedPickupPointCodes, Set<String> deletedPickupPointCodes, Boolean online, Boolean cncAtL3Level,
      EditItemResponse editItemResponse, boolean fbbChangedAtL5, Set<String> fbbActivatedPPsFromEditList,
      boolean deletedFbbTrueL5, AddDeleteVariantRequestVo addDeleteVariantRequestVo,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Product product, boolean isBundleProduct, boolean readFromPrimary) {
    checkArgument(StringUtils.isNotBlank(productSku), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    Map<String, Item> itemSkuToUpdatedItemMap = new HashMap<>();
    List<ItemPickupPoint> itemViewConfigUpdated = new ArrayList<>();
    List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    List<ItemPickupPoint> offlineItemChangeUpdatedEvent = new ArrayList<>();
    List<Item> itemsToReindexAndPublish = new ArrayList<>();
    List<Item> itemsToOnlyReindex = new ArrayList<>();
    List<ItemPickupPoint> allItemPickupPoints = new ArrayList<>();
    Map<String, ItemPickupPointListingUpdateRequestVo> updatedItemSkuAndRequestVoMap  = new HashMap<>();
    Set<String> alreadyUpdatedL5s = new HashSet<>();
    Map<String, List<ItemPickupPointChangeEventType>> itemPickupPointChangeEventMap = new HashMap<>();
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    Map<String, List<ItemPickupPoint>> allItemSkuAndItemPickupPointMap = new HashMap<>();
    boolean isOnlineFlagChanged = false;
    boolean pureCNCStatusChange = false;
    Map<String, Boolean> pickupPointCodeToPureCNCStatusChangeMap = new HashMap<>();
    boolean isCNCFlagMadeFalseAtL3Level = false;
    boolean isB2BFlagMadeFalseAtL3Level = false;
    boolean isB2CFlagMadeFalseAtL3Level = false;
    boolean isFbbChangedAtL3 = false;
    Boolean isB2bChangedAtL3 = false;
    boolean isB2cChangedAtL3 = false;
    boolean isB2bFieldsUpdated = false;
    boolean cncChanged = false;
    boolean buyableAndDiscoverableScheduleChanged = false;
    if (Objects.isNull(product)) {
        product = this.dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
            GdnMandatoryRequestParameterUtil.getStoreId(), productSku, readFromPrimary);
    }
    checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_MUST_NOT_BE_NULL);
    BusinessPartner businessPartner = businessPartnerService
        .getBusinessPartnerByBusinessPartnerCode(product.getStoreId(), product.getMerchantCode());
    String currentSalesChannel = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    if (Objects.nonNull(cncAtL3Level) && product.isCncActivated() && !cncAtL3Level) {
      isCNCFlagMadeFalseAtL3Level = true;
    }
    if (Objects.nonNull(itemPickupPointUpdateRequestVo.getB2bActivated()) && product.isB2bActivated()
        && !itemPickupPointUpdateRequestVo.getB2bActivated()) {
      isB2BFlagMadeFalseAtL3Level = true;
      product.setB2bActivated(false);
      isB2bChangedAtL3 = true;
    } else if (Boolean.TRUE.equals(itemPickupPointUpdateRequestVo.getB2bActivated()) && !product.isB2bActivated()) {
      product.setB2bActivated(true);
      isB2bChangedAtL3 = true;
    }
    if (Objects.nonNull(itemPickupPointUpdateRequestVo.getB2cActivated()) && product.getB2cActivated()
        && !itemPickupPointUpdateRequestVo.getB2cActivated()) {
      isB2CFlagMadeFalseAtL3Level = true;
      product.setB2cActivated(false);
      product.setOnline(false);
      isB2cChangedAtL3 = true;
      if (Boolean.TRUE.equals(online)) {
        online = false;
      }
    }
    if (Objects.nonNull(online) && product.isOnline() != online) {
      isOnlineFlagChanged = true;
    }
    boolean addOrDeletePPPerformed =
        CommonUtil.distributionSellerAndAddOrDeletePerformed(distributionSellerList,
            itemPickupPointUpdateRequestVo, product.getMerchantCode());
    if (fbbChangedAtL5 || isCNCFlagMadeFalseAtL3Level || isOnlineFlagChanged
        || isB2BFlagMadeFalseAtL3Level || addOrDeletePPPerformed) {
      allItemPickupPoints = dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
          GdnMandatoryRequestParameterUtil.getStoreId(), productSku, readFromPrimary);
    }
    if (isCNCFlagMadeFalseAtL3Level) {
      cncChanged = true;
      product.setCncActivated(false);
    } else {
      cncChanged =
          setCncActiveAtProductLevel(productSku, updatedItemSet, product, readFromPrimary);
    }

    boolean pickupCodesChanged = isPickupCodesChanged(newlyAddedPickupPointCodes, deletedPickupPointCodes, product);
    boolean productUpdated = false;

    for (ItemPickupPointListingUpdateRequestVo requestVo : itemPickupPointListingUpdateRequestVo) {
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
      if (Objects.nonNull(online) && Boolean.FALSE.equals(online)) {
        requestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setDiscoverable(false);
        requestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setBuyable(false);
      }
      String itemSku = requestVo.getItemSku();
      ItemPickupPoint itemPickupPoint;
      Item item =
          getItem(GdnMandatoryRequestParameterUtil.getStoreId(), itemSku, itemSkuAndItemMap,
              readFromPrimary);
      CommonUtil.validateItem(itemSku, item);
      boolean merchantSkuChanged;
      boolean savedItemIsArchived = item.isArchived();
        if (itemPickupPointMap.containsKey(requestVo.getItemSku() + Constants.DASH + requestVo.getPickupPointCode())) {
          itemPickupPoint =
              itemPickupPointMap.get(requestVo.getItemSku() + Constants.DASH + requestVo.getPickupPointCode());
        } else {
          itemPickupPoint = fetchItemPickupPointByItemSkuAndPickupPointCode(requestVo.getItemSku(),
            requestVo.getPickupPointCode(), readFromPrimary);
        }
        pureCNCStatusChange = CommonUtil.isPureCNCStatusChange(requestVo.getItemViewConfigs().iterator().next(),
            itemPickupPoint.getItemViewConfig().iterator().next(), requestVo.isCncActivated(),
            itemPickupPoint.isCncActive());
        pickupPointCodeToPureCNCStatusChangeMap.put(itemPickupPoint.getOfflineItemId(), pureCNCStatusChange);

      if (CommonUtil.isDiscoverableChanged(
          requestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT),
          itemPickupPoint.getSingleItemViewConfigByChannel(Constants.DEFAULT))) {
        itemPickupPointChangeEventTypes.add(
            ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
      }

      if (cncForWarehouseFeatureSwitch && CommonUtil.isDiscoverableChanged(
          requestVo.getSingleItemViewConfigByChannel(Constants.CNC),
          itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC))) {
        itemPickupPointChangeEventTypes.add(
            ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
      }

      buyableAndDiscoverableScheduleChanged =
          CommonUtil.isBuyableAndDiscoverableScheduleChanged(buyableAndDiscoverableScheduleChanged,
              requestVo, itemPickupPointChangeEventTypes, itemPickupPoint, schedulesAddEditEnabled);
      this.objectConverterService.overrideL4DetailsFromL5(Collections.singletonList(item),
        Collections.singletonList(itemPickupPoint));
      boolean wholeSalePriceActivatedFlagUpdated = checkAndUpdateWholeSalePromo(itemPickupPoint, requestVo);
      Pair<Boolean, Boolean> isItemDataAndItemPickupPointDataChanged = checkAndUpdateItemViewConfig(item, itemPickupPoint, requestVo);
      boolean L5StatusIsOnline = false;
      L5StatusIsOnline = CommonUtil.isL5StatusIsOnline(requestVo, L5StatusIsOnline);
      boolean isItemDataUpdated = isItemDataAndItemPickupPointDataChanged.getLeft();
      boolean isItemPickupPintDataChanged = isItemDataAndItemPickupPointDataChanged.getRight();
      boolean isCncFlagUpdated;
      if (!cncForWarehouseFeatureSwitch) {
        isCncFlagUpdated = itemPickupPoint.isCncActive() != requestVo.isCncActivated();
      } else {
        ItemViewConfig cncItemViewConfigInDb =
            itemPickupPoint.getSingleItemViewConfigByChannel(channelService.getCncChannel());
        ItemViewConfig cncItemViewConfigRequest = requestVo.getItemViewConfigs().stream()
            .filter(itemViewConfig -> channelService.getCncChannel().equals(itemViewConfig.getChannel())).findFirst()
            .orElse(new ItemViewConfig());
        isCncFlagUpdated = !cncItemViewConfigRequest.equals(cncItemViewConfigInDb);
      }
      boolean isFbbUpdated = itemPickupPoint.isFbbActivated() != requestVo.isFbbActivated();
      if (isCncFlagUpdated && !cncForWarehouseFeatureSwitch) {
        itemPickupPoint.setCncActive(requestVo.isCncActivated());
      }
      if (isFbbUpdated) {
        itemPickupPoint.setFbbActivated(requestVo.isFbbActivated());
      }
      merchantSkuChanged =
        checkAndUpdateMerchantSku(GdnMandatoryRequestParameterUtil.getStoreId(), item,
          requestVo.getMerchantSku(),isItemDataUpdated, itemPickupPoint, updatedItemPickupPoints);
      Set<ItemChangeEventType> itemChangeEventTypes = new HashSet<>();
      if (merchantSkuChanged) {
        itemPickupPoint.setMerchantSku(requestVo.getMerchantSku());
        itemsToReindexAndPublish.add(item);
        itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
      }
      Pair<Boolean, Boolean> b2bPainResultant =
          isB2bFieldsUpdated(auditTrailDtoList, isB2bChangedAtL3, product, requestVo, itemPickupPoint, item);
      isB2bFieldsUpdated = b2bPainResultant.getLeft();
      isB2bChangedAtL3 = b2bPainResultant.getRight();
      if (isB2bFieldsUpdated && includeB2bFieldChange) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.B2B_FIELD_CHANGE);
      }
      if (isItemPickupPintDataChanged) {
        if (savedItemIsArchived) {
          itemChangeEventTypes.add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
        }
        updatedItemSet.add(item);
        itemViewConfigUpdated.add(itemPickupPoint);
        if (!product.isOnline() && L5StatusIsOnline) {
          online = true;
          isOnlineFlagChanged = true;
        }
      }
      boolean isPriceUpdated = pickupPointService
        .validatePriceChangeAndSet(itemPickupPoint, requestVo.getPrice(),
          GdnMandatoryRequestParameterUtil.getUsername());
      if (isPriceUpdated) {
        itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
      }
      itemPickupPoint.setNewData(Boolean.FALSE);
      log.info(
          "Update data itemSku : {} , pickupPointCode : {} , priceUpdated : {} , itemDataUpdated : {} , wholesalePriceUpdated : {} , cncFlagUpdated : {}",
          itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), isPriceUpdated, isItemDataUpdated,
          wholeSalePriceActivatedFlagUpdated, isCncFlagUpdated);
      if (isPriceUpdated && itemPickupPoint.isCncActive()) {
        offlineItemChangeUpdatedEvent.add(itemPickupPoint);
      }
      if (CommonUtil.isEligibleForL5Update(isPriceUpdated, isItemDataUpdated, wholeSalePriceActivatedFlagUpdated,
          isCncFlagUpdated, isB2bFieldsUpdated, isItemPickupPintDataChanged, buyableAndDiscoverableScheduleChanged)) {
        itemsToOnlyReindex.add(item);
        if (isItemDataUpdated) {
          itemsToReindexAndPublish.add(item);
        }
        updatedItemPickupPointList.add(itemPickupPoint);
      } else if(merchantSkuChanged) {
        // Adding it here so that we don't add the same itemPickup point twice in list because of merchantSku and price update
        // Checking for mpp because with switch off as deliveryTrue item was already saved in checkAndUpdateMerchantSku
        updatedItemPickupPointList.add(itemPickupPoint);
      } else if (isFbbUpdated) {
        updatedItemPickupPointList.add(itemPickupPoint);
      }
      item.getItemChangeEventTypes().addAll(new ArrayList<>(itemChangeEventTypes));
      if (merchantSkuChanged) {
        updatedItemSkuAndRequestVoMap.putIfAbsent(requestVo.getItemSku(), requestVo);
        alreadyUpdatedL5s.add(requestVo.getItemSku() + Constants.HYPHEN + requestVo.getPickupPointCode());
        updatedItemSet.add(item);
      }
      itemPickupPointChangeEventMap.put(itemPickupPoint.getOfflineItemId(), itemPickupPointChangeEventTypes);
    }
    updateDataOnMerchantSkuUpdate(updatedItemPickupPointList, updatedItemSkuAndRequestVoMap, alreadyUpdatedL5s);
    updateDataOnOnlineFlagChange(online, updatedItemPickupPointList, allItemPickupPoints, isOnlineFlagChanged, product);
    updateDataOnCncFlagChangedAtL3Level(productSku, updatedItemSet, updatedItemPickupPointList, allItemPickupPoints,
        isCNCFlagMadeFalseAtL3Level);
    if (isB2BFlagMadeFalseAtL3Level) {
      isB2bChangedAtL3 = true;
    }
    if (isB2CFlagMadeFalseAtL3Level) {
      product.setB2cActivated(false);
      isB2cChangedAtL3 = true;
    } else if (product.isCncActivated() || product.isOnline() || (instoreNewFlowEnabled && Boolean.TRUE.equals(
        itemPickupPointUpdateRequestVo.getB2cActivated()))) {
      if (!product.getB2cActivated()) {
        isB2cChangedAtL3 = true;
        product.setB2cActivated(true);
      }
    }

    CommonUtil.publishProductFlagChangeHistoryEvent(product, isB2bChangedAtL3, !product.isB2bActivated(),
        product.isB2bActivated(), Constants.B2B_ACTIVATED, auditTrailDtoList);
    CommonUtil.publishProductFlagChangeHistoryEvent(product, isB2cChangedAtL3,
        !Optional.ofNullable(product).map(Product::getB2cActivated).orElse(true),
        Optional.ofNullable(product).map(Product::getB2cActivated).orElse(true), Constants.B2C_ACTIVATED,
        auditTrailDtoList);

    if (isBundleProduct) {
      product.setBundleProduct(true);
    }

    isFbbChangedAtL3 = updatingFbbAtL3AndL5(fbbActivatedPPsFromEditList, deletedFbbTrueL5,
        allItemPickupPoints, isFbbChangedAtL3, product);

    List<Item> updatedItemList = new ArrayList<>(updatedItemSet.stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (o1, o2) -> o1)).values());
    for (Item item : itemsToReindexAndPublish) {
      itemSkuToUpdatedItemMap.put(item.getItemSku(), item);
    }
    CommonUtil.setDistributionStatus(product, addOrDeletePPPerformed, allItemPickupPoints, distributionSellerList);
    log.info(
        "Saving product for productSku : {} , fbbFLagChange : {} , cncChanged : {} , productTypeChanged {}, ppCodes changed: {} , onlineFlagChanged : {}, addDeleteVaraints : {}, b2cChanged : {} , b2bChanged :{} ",
        product.getProductSku(), isFbbChangedAtL3, cncChanged, !product.getProductType().equals(productType),
        pickupCodesChanged, isOnlineFlagChanged, Objects.nonNull(addDeleteVariantRequestVo), isB2cChangedAtL3,
        isB2bChangedAtL3);
    if (isFbbChangedAtL3 || cncChanged || !product.getProductType().equals(productType) || pickupCodesChanged
        || isOnlineFlagChanged || Objects.nonNull(addDeleteVariantRequestVo) || isB2bChangedAtL3 || isB2cChangedAtL3 || isBundleProduct) {
      computeProductDefiningAttributes(addDeleteVariantRequestVo, product);
      productUpdated = true;
      product.setProductType(productType);
      product = saveOperationService.saveProductWithoutUpdatingSolr(product,
          Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    }
    String updatedSalesChannel = CommonUtil.getSalesChannelOfProductForMultiChannelSeller(product, businessPartner);
    CommonUtil.isSalesChannelChanged(currentSalesChannel, updatedSalesChannel, product, auditTrailDtoList);
    if (CollectionUtils.isNotEmpty(updatedItemList)) {
        saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(updatedItemList);
    }
    if (CollectionUtils.isNotEmpty(updatedItemPickupPointList)) {
      log.info("ItemPickupPoints going for save for combined edit for productSku : {} , updatedItemList : {} ",
          productSku, updatedItemPickupPointList);
      overrideViewConfigForDistributionPP(updatedItemPickupPointList);
      updatedItemPickupPointList = removeDuplicateItemPickupPoints(updatedItemPickupPointList);
      itemPickupPoints = pickupPointService.saveItemPickupPoint(updatedItemPickupPointList);
      updatedItemPickupPoints.addAll(itemPickupPoints);
    }
    updateExternalHistoryInPBP(auditTrailDtoList);
    for (Item item : updatedItemList) {
      if (!itemSkuToUpdatedItemMap.containsKey(item.getItemSku()) && item.isMarkForDelete()) {
        itemsToReindexAndPublish.add(item);
      }
    }
    objectConverterService.overrideL4DetailsFromL5(itemsToReindexAndPublish, updatedItemPickupPoints);
    if (CollectionUtils.isNotEmpty(itemsToReindexAndPublish)) {
      SystemParameter systemParameter = systemParameterService
          .findValueByStoreIdAndVariable(GdnMandatoryRequestParameterUtil.getStoreId(),
              SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
      boolean enableDifferedSolrReindex =
          !Objects.isNull(systemParameter.getValue()) && Boolean.parseBoolean(systemParameter.getValue());
      if (enableDifferedSolrReindex) {
        itemsToOnlyReindex.addAll(itemsToReindexAndPublish);
        this.saveOperationService
            .deferredReindexItems(GdnMandatoryRequestParameterUtil.getStoreId(), itemsToOnlyReindex, true,
                ReindexType.ITEM_REINDEX, false);
      } else {
        itemsToOnlyReindex.addAll(itemsToReindexAndPublish);
        productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, itemsToOnlyReindex, false);
      }
    } else if (productUpdated) {
      productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, new ArrayList<>(), false);
    }
    else if (CollectionUtils.isNotEmpty(itemsToOnlyReindex)) {
      productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, itemsToOnlyReindex, false);
    }
    if (CollectionUtils.isNotEmpty(updatedItemPickupPointList)) {
      for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
        List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList =
            itemPickupPointChangeEventMap.get(itemPickupPoint.getOfflineItemId());
        ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
            objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint,
                pickupPointCodeToPureCNCStatusChangeMap.getOrDefault(itemPickupPoint.getOfflineItemId(), false));
        itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypeList);
        pickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
            itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
      }
    }
    if (CollectionUtils.isNotEmpty(itemsToReindexAndPublish)) {
      saveAndPublishService.publishItemDataChangeEvent(itemsToReindexAndPublish);
    }
    for (ItemPickupPoint pickupPoint : offlineItemChangeUpdatedEvent) {
      OfflineItemChange offlineItemChange = CommonUtil.convertToOfflineItemChange(pickupPoint, itemSkuAndItemMap);
      saveAndPublishService.publishOfflineItemChangeSellerEvent(offlineItemChange);
    }
      for (ItemPickupPoint pickupPoint : itemViewConfigUpdated) {
        saveAndPublishService.publishViewConfigChange(pickupPoint);
    }
    editItemResponse.setL3Version(product.getVersion());
    return new BasicProductAndItemEditedDetailsDTO(updatedItemPickupPoints, isFbbChangedAtL3);
  }

  private void overrideViewConfigForDistributionPP(List<ItemPickupPoint> updatedItemPickupPointList) {
    if (ranchIntegrationEnabled) {
      updatedItemPickupPointList.stream().filter(ItemPickupPoint::isDistribution)
          .forEach(itemPickupPoint -> itemPickupPoint.getAllItemViewConfigs().forEach(itemViewConfig -> {
            itemViewConfig.setBuyable(false);
            itemViewConfig.setDiscoverable(false);
            itemViewConfig.setItemBuyableSchedules(null);
            itemViewConfig.setItemDiscoverableSchedules(null);
          }));
    }
  }

  private List<ItemPickupPoint> removeDuplicateItemPickupPoints(List<ItemPickupPoint> updatedItemPickupPointList) {
    if (removeDuplicateL5BeforeSave) {
      Map<String, ItemPickupPoint> itemPickupPointMap1 = new HashMap<>();
      for (ItemPickupPoint itemPickupPoint : updatedItemPickupPointList) {
        if (!itemPickupPointMap1.containsKey(itemPickupPoint.getOfflineItemId())) {
          itemPickupPointMap1.put(itemPickupPoint.getOfflineItemId(), itemPickupPoint);
        } else {
          log.error("Duplicate L5 discarded, existing L5 : {} , duplicate L5 : {} ",
              itemPickupPointMap1.get(itemPickupPoint.getOfflineItemId()), itemPickupPoint);
        }
      }
      updatedItemPickupPointList = new ArrayList<>(itemPickupPointMap1.values());
    }
    return updatedItemPickupPointList;
  }

  @Override
  public void updateExternalHistoryInPBP(List<AuditTrailDto> auditTrailDtoList) {
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
      auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
      auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
      auditTrailListResponse.setChangedBy(GdnMandatoryRequestParameterUtil.getUsername());
      auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
      auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
      auditTrailListResponse.setUpdateDirectly(true);
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
    }
  }


  public void publishPreorderEventAndDeleteL5FromInventory(Product product, EditProductDetailDTO editProductDetailDTO,
      List<ItemPickupPoint> updatedItemPickupPointList) {
    PreOrderVO preOrderVO = editProductDetailDTO.getPreOrderVO();
    List<ItemPickupPoint> itemPickupPointTobeDeleted =
        updatedItemPickupPointList.stream().filter(ItemPickupPoint::isMarkForDelete).collect(Collectors.toList());
    if (Objects.nonNull(preOrderVO)) {
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS, product.getProductSku(),
          new ProductPreOrderStatus(product.getProductCode(), product.getProductSku(), preOrderVO));
    }
    if (CollectionUtils.isNotEmpty(itemPickupPointTobeDeleted)) {
      this.deleteItemPickupPointsFromInventory(itemPickupPointTobeDeleted);
    }
    if (editProductDetailDTO.isProductScoreChanged()) {
      productService.saveHistoryForProductScoreUpdate(product,
          objectConverterService.toProductScore(editProductDetailDTO.getExistingProductScore()),
          product.getProductScore());
    }
  }

  private static Pair<Boolean, Boolean> isB2bFieldsUpdated(List<AuditTrailDto> auditTrailDtoList, Boolean isB2bChangedAtL3,
      Product product, ItemPickupPointListingUpdateRequestVo requestVo, ItemPickupPoint itemPickupPoint, Item item) {
    boolean isB2bFieldsUpdated = false;
    if (Objects.nonNull(requestVo.getB2bFieldsVo())) {
      if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
        B2bFields currentB2bFields = itemPickupPoint.getB2bFields();
        B2bFieldsVo requestB2bFields = requestVo.getB2bFieldsVo();
        ItemViewConfig b2bItemViewConfigRequest = requestVo.getB2bFieldsVo().getB2bItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B.equals(itemViewConfig.getChannel())).findFirst()
            .orElse(new ItemViewConfig());
        ItemViewConfig b2bItemViewConfigCurrent = itemPickupPoint.getAllItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B.equals(itemViewConfig.getChannel())).findFirst()
            .orElse(new ItemViewConfig());
        boolean isManagedFlagChanged =
            CommonUtil.isManagedChanged(currentB2bFields.isManaged(), requestB2bFields.isManaged());
        boolean isBasePriceChanged =
            CommonUtil.isBasePriceChanged(currentB2bFields.getBasePrice(), requestB2bFields.getBasePrice());
        boolean isBuyableChanged =
            CommonUtil.isB2bBuyableChanged(b2bItemViewConfigCurrent.isBuyable(), b2bItemViewConfigRequest.isBuyable());
        boolean isDisplayableChanged = CommonUtil.isB2bDiscoverableChanged(b2bItemViewConfigCurrent.isDiscoverable(),
            b2bItemViewConfigRequest.isDiscoverable());
        isB2bFieldsUpdated =
            CommonUtil.isB2bFieldsGotUpdated(isManagedFlagChanged, isBasePriceChanged, isBuyableChanged,
                isDisplayableChanged, auditTrailDtoList, item, itemPickupPoint, requestVo);
        isB2bChangedAtL3 = CommonUtil.isB2bFlagUpdatedAtL3Level(product, b2bItemViewConfigRequest.isDiscoverable(),
            b2bItemViewConfigRequest.isBuyable());
        itemPickupPoint.getB2bFields().setBasePrice(requestB2bFields.getBasePrice());
        itemPickupPoint.getB2bFields().setManaged(requestB2bFields.isManaged());
        Set<ItemViewConfig> updatedItemViewConfig = itemPickupPoint.getItemViewConfig();
        updatedItemViewConfig.add(b2bItemViewConfigRequest);
        itemPickupPoint.setItemViewConfig(updatedItemViewConfig);
      } else {
        B2bFields b2bFields = new B2bFields();
        b2bFields.setBasePrice(requestVo.getB2bFieldsVo().getBasePrice());
        auditTrailDtoList.add(
            CommonUtil.getAuditTrailDto(Constants.HYPHEN, String.valueOf(b2bFields.getBasePrice()), Constants.B2B_PRICE,
                itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(), item.getItemSku(),
                item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
        b2bFields.setManaged(requestVo.getB2bFieldsVo().isManaged());
        auditTrailDtoList.add(
            CommonUtil.getAuditTrailDto(Constants.HYPHEN, String.valueOf(b2bFields.isManaged()), Constants.B2B_MANAGED,
                itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(), item.getItemSku(),
                item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
        itemPickupPoint.setB2bFields(b2bFields);
        ItemViewConfig b2bItemViewConfig = requestVo.getB2bFieldsVo().getB2bItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B.equals(itemViewConfig.getChannel())).findFirst()
            .orElse(new ItemViewConfig());
        isB2bChangedAtL3 = CommonUtil.isB2bFlagUpdatedAtL3Level(product, b2bItemViewConfig.isDiscoverable(),
            b2bItemViewConfig.isBuyable());
        Set<ItemViewConfig> updatedItemViewConfig = itemPickupPoint.getItemViewConfig();
        updatedItemViewConfig.add(b2bItemViewConfig);
        itemPickupPoint.setItemViewConfig(updatedItemViewConfig);
        isB2bFieldsUpdated = true;
        auditTrailDtoList.add(
            CommonUtil.getAuditTrailDto(Constants.HYPHEN, String.valueOf(b2bItemViewConfig.isBuyable()),
                Constants.B2B_BUYABLE, itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(),
                item.getItemSku(), item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
        auditTrailDtoList.add(
            CommonUtil.getAuditTrailDto(Constants.HYPHEN, String.valueOf(b2bItemViewConfig.isDiscoverable()),
                Constants.B2B_DISCOVERABLE, itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(),
                item.getItemSku(), item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
      }
    }
    return Pair.of(isB2bFieldsUpdated, isB2bChangedAtL3);
  }

  private void computeProductDefiningAttributes(AddDeleteVariantRequestVo addDeleteVariantRequestVo, Product product) {
    if (Objects.nonNull(addDeleteVariantRequestVo)) {
      List<ProductAttribute> productDefiningAttributes =
          Optional.ofNullable(product.getDefiningAttributes()).orElse(new ArrayList<>());
      for (AddVariantRequestVo addVariantRequestVo : addDeleteVariantRequestVo.getAddVariantsList()) {
        ProductAttribute productAttribute = new ProductAttribute(addVariantRequestVo.getItemSku(),
            addVariantRequestVo.getDefiningAttributes().stream().map(
                    productAttributeDetailVo -> new ProductAttributeDetail(productAttributeDetailVo.getAttributeCode(),
                        productAttributeDetailVo.getAttributeName(), productAttributeDetailVo.getAttributeValue()))
                .collect(Collectors.toList()));
        productDefiningAttributes.add(productAttribute);
      }
      product.setDefiningAttributes(productDefiningAttributes);
      for (String itemSku : addDeleteVariantRequestVo.getDeleteVariantsList()) {
        productHelperService.deleteItemAttributeFromProductAttribute(product, itemSku);
      }
    }
  }

  public boolean updatingFbbAtL3AndL5(Set<String> fbbActivatedPPsFromEditList, boolean deletedFbbTrueL5,
      List<ItemPickupPoint> allItemPickupPoints, boolean isFbbChangedAtL3, Product product) {
    if (CollectionUtils.isNotEmpty(fbbActivatedPPsFromEditList) && !product.isFbbActivated()) {
      product.setFbbActivated(true);
      isFbbChangedAtL3 = true;
    } else if (deletedFbbTrueL5 && CollectionUtils.isNotEmpty(allItemPickupPoints)) {
      boolean fbbTrueL5Found = allItemPickupPoints.stream().anyMatch(ItemPickupPoint::isFbbActivated);
      if (!fbbTrueL5Found && product.isFbbActivated()) {
        product.setFbbActivated(false);
        isFbbChangedAtL3 = true;
      }
    }
    return isFbbChangedAtL3;
  }

  private void updateDataOnMerchantSkuUpdate(List<ItemPickupPoint> updatedItemPickupPointList,
      Map<String, ItemPickupPointListingUpdateRequestVo> updatedItemSkuAndRequestVoMap, Set<String> alreadyUpdatedL5s) {
    for (Map.Entry<String, ItemPickupPointListingUpdateRequestVo> updatedItemSkuAndMerchantSku : updatedItemSkuAndRequestVoMap
        .entrySet()) {
      List<ItemPickupPoint> allItemPickupPointsByItemSku =
        pickupPointService.findByStoreIdAndItemSku(GdnMandatoryRequestParameterUtil.getStoreId(),
          updatedItemSkuAndMerchantSku.getValue().getItemSku());
      allItemPickupPointsByItemSku.removeIf(
          pickupPoint -> isMerchantSkuUpdatedAtL5(pickupPoint, updatedItemSkuAndMerchantSku.getValue(),
              alreadyUpdatedL5s));
      allItemPickupPointsByItemSku.forEach(allItemPickupPoint -> allItemPickupPoint
          .setMerchantSku(updatedItemSkuAndMerchantSku.getValue().getMerchantSku()));
      updatedItemPickupPointList.addAll(allItemPickupPointsByItemSku);
    }
  }

  private boolean isMerchantSkuUpdatedAtL5(ItemPickupPoint itemPickupPoint,
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo, Set<String> alreadyUpdatedL5s) {
    return StringUtils.equals(itemPickupPointListingUpdateRequestVo.getMerchantSku(), itemPickupPoint.getMerchantSku())
        || alreadyUpdatedL5s.contains(
        CommonUtil.generateOfflineItemId(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()));
  }

  private void updateDataOnCncFlagChangedAtL3Level(String productSku, Set<Item> updatedItemSet,
      List<ItemPickupPoint> updatedItemPickupPointList, List<ItemPickupPoint> allItemPickupPoints,
      boolean isCNCFlagMadeFalseAtL3Level) {
    if (isCNCFlagMadeFalseAtL3Level) {
      List<Item> itemsByProductSku =
        cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(
          GdnMandatoryRequestParameterUtil.getStoreId(), productSku);
      Map<String, Item> updatedItemMap =
          updatedItemSet.stream().collect(Collectors.toMap(Item::getItemSku, item -> item, (a, b) -> a));
      Map<String, ItemPickupPoint> updatedItemPickupPointMap = updatedItemPickupPointList.stream().collect(
          Collectors.toMap(ItemPickupPoint::getOfflineItemId, itemPickupPoint -> itemPickupPoint, (a, b) -> b));
      for (Item item : itemsByProductSku) {
        if (item.isCncActivated()) {
          item.setCncActivated(false);
          if (!updatedItemMap.containsKey(item.getItemSku())) {
            updatedItemSet.add(item);
          }
        }
      }
      for(ItemPickupPoint itemPickupPoint : updatedItemPickupPointList) {
        itemPickupPoint.setCncActive(false);
      }
      for (ItemPickupPoint itemPickupPoint : allItemPickupPoints) {
        if (itemPickupPoint.isCncActive()) {
        itemPickupPoint.setCncActive(false);
        if (!updatedItemPickupPointMap.containsKey(itemPickupPoint.getOfflineItemId())) {
          updatedItemPickupPointList.add(itemPickupPoint);
        }
        }
      }
    }
  }


  private static void updateDataOnOnlineFlagChange(Boolean online, List<ItemPickupPoint> updatedItemPickupPointList,
      List<ItemPickupPoint> allItemPickupPoints, boolean isOnlineFlagChanged, Product product) {
    if (isOnlineFlagChanged) {
      if (Boolean.FALSE.equals(online) && product.isOnline()) {
        Map<String, ItemPickupPoint> updatedItemPickupPointMap = updatedItemPickupPointList.stream().collect(
            Collectors.toMap(ItemPickupPoint::getOfflineItemId, itemPickupPoint -> itemPickupPoint, (a, b) -> b));
        product.setOnline(false);
        for (ItemPickupPoint itemPickupPoint : allItemPickupPoints) {
          itemPickupPoint.getItemViewConfig().iterator().next().setBuyable(false);
          itemPickupPoint.getItemViewConfig().iterator().next().setDiscoverable(false);
          itemPickupPoint.getItemViewConfig().iterator().next().setItemBuyableSchedules(null);
          itemPickupPoint.getItemViewConfig().iterator().next().setItemDiscoverableSchedules(null);
          if (!updatedItemPickupPointMap.containsKey(itemPickupPoint.getOfflineItemId())) {
            updatedItemPickupPointList.add(itemPickupPoint);
          }
        }
      } else if (Boolean.TRUE.equals(online) && !product.isOnline()) {
        product.setOnline(true);
      }
    }
  }

  private boolean isPickupCodesChanged(Set<String> newlyAddedPickupPointCodes, Set<String> deletedPickupPointCodes,
      Product product) {
    boolean pickupCodesChanged = false;
    if (!newlyAddedPickupPointCodes.isEmpty() || !deletedPickupPointCodes.isEmpty()) {
      pickupCodesChanged = true;
      Set<String> pickupPointCodes = new HashSet<>(product.getPickupPointCodes());
      for (String deletedPickupPointCode : deletedPickupPointCodes) {
        pickupPointCodes.remove(deletedPickupPointCode);
      }
      product.setPickupPointCodes(pickupPointCodes);
      product.getPickupPointCodes().addAll(newlyAddedPickupPointCodes);
    }
    return pickupCodesChanged;
  }

  @Override
  public boolean setCncActiveAtProductLevel(String productSku, Set<Item> updatedItemList, Product product,
    boolean readFromPrimary) {
    boolean cncChanged = false;
    // Here the list won't be empty only when there is a change in cnc flag in any of the items
    if (CollectionUtils.isNotEmpty(updatedItemList)) {
      if (!product.isCncActivated()) {
        // Set as true when any L4 is updated as CNC_FLAG_UPDATE true
        product.setCncActivated(updatedItemList.stream().anyMatch(Item::isCncActivated));
        if (product.isCncActivated()) {
          cncChanged = true;
        }
      } else if (updatedItemList.stream().noneMatch(Item::isCncActivated)) {
        Long numberOfCncTrueL4sItemsCurrentlyPresent =
            dataSourceWrapperService.findItemCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(
              GdnMandatoryRequestParameterUtil.getStoreId(), productSku, true, readFromPrimary);
        // No need to filter cnc False items which are cnc false, as this line will be called
        // only when updatedItemList doesn't contain any cncTrue item i.e updatedItemList.stream().noneMatch(Item::isCncActivated)
        long numberOfL4sForWhichCncWillBeMadeFalse = updatedItemList.size();
        // Set as false when number of current active L4s is same as number of items which is being made as cnc false
        if (numberOfL4sForWhichCncWillBeMadeFalse >= numberOfCncTrueL4sItemsCurrentlyPresent) {
          product.setCncActivated(false);
          cncChanged = true;
        }
      }
    }
    return cncChanged;
  }

  public Item getItem(String storeId, String itemSku, Map<String, Item> itemSkuAndItemMap,
    boolean readFromPrimary) {
    Item item;
    // Using map here to prevent redundant gets for the same item
    if (!itemSkuAndItemMap.containsKey(itemSku)) {
      item =
        dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku,
          readFromPrimary);
      itemSkuAndItemMap.putIfAbsent(itemSku, item);
    } else {
      item = itemSkuAndItemMap.get(itemSku);
    }
    return item;
  }

  private boolean checkAndUpdateWholeSalePromo(ItemPickupPoint itemPickupPoint,
      ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo) {
    boolean wholeSalePriceActivatedFlagUpdated = false;
    if (Objects.nonNull(itemListingUpdateRequestVo.getWholesalePriceActivated())) {
      wholeSalePriceActivatedFlagUpdated = true;
      itemPickupPoint.setWholesalePriceExists(true);
      if (!itemListingUpdateRequestVo.getWholesalePriceActivated()) {
        if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings()) && itemPickupPoint
            .getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE)) {
          itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
        }
      } else {
        setWholesaleActivePromoBundling(itemPickupPoint);
      }
    }
    return wholeSalePriceActivatedFlagUpdated;
  }

  private void setWholesaleActivePromoBundling(ItemPickupPoint itemPickupPoint) {
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
      itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    } else {
      itemPickupPoint.setActivePromoBundlings(new HashSet<>());
      itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    }
  }

  private Pair<Boolean, Boolean> checkAndUpdateItemViewConfig(Item item, ItemPickupPoint itemPickupPoint,
    ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo) {
    boolean itemDataChanged = false;
    boolean itemPickupPointDataChanged = false;
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())
      && pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
      itemListingUpdateRequestVo.getItemViewConfigs())) {
      this.productHelperService.updateItemPickupPointViewConfigForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs());
      if (item.isArchived()) {
        item.setArchived(false);
        itemDataChanged = true;
      }
      itemPickupPointDataChanged = true;
    }
    return Pair.of(itemDataChanged, itemPickupPointDataChanged);
  }

  private ItemPickupPoint fetchItemPickupPointByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode, boolean readFromPrimary) {
    return dataSourceWrapperService
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(GdnMandatoryRequestParameterUtil.getStoreId(), itemSku,
            pickupPointCode, readFromPrimary);
  }

  @Override
  public EditItemResponse updateItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Map<String, Item> itemSkuAndItemMap,
    boolean isBundleProduct,
      boolean readFromPrimary) throws Exception {
    Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
    Set<Item> updatedItemList = new HashSet<>();
    List<String> newlyAddedL5s = new ArrayList<>();
    Set<String> newlyAddedCncTrueL5s = new HashSet<>();
    Set<String> newlyAddedCncTrueL4s = new HashSet<>();
    Set<ItemPickupPoint> existingL5sMadeCncFalse = new HashSet<>();
    EditItemResponse editItemResponse = new EditItemResponse();
    Set<String> deletedPickupPointCodes = new HashSet<>();
    Set<String> newlyAddedPickupPointCodes = new HashSet<>();
    List<ItemPickupPoint> nonMppPickupPointUpdate = new ArrayList<>();
    Set<String> itemsNewlyAddedL5 = new HashSet<>();
    Set<String> fbbActivatedToTruePps = new HashSet<>();
    boolean fbbChangedAtL5 = false;
    Product product = null;

    //Check if pp changed for non mpp merchant from bulk
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests())
      && (itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0)
      .isPpCodeChangedForNonMppSeller())) {
      //if pp code then calling update pp first then continue for other field updates
      //Forming pp update request
      PickupPointUpdateRequest pickupPointUpdateRequest = CommonUtil
        .toPickupPointUpdateRequest(
          itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0));

      ProductAndItemPickupPointDTO updatedProductAndItemPickupPointDTO =
          itemService.updatePickupPoints(mandatoryRequestParam.getStoreId(), pickupPointUpdateRequest,
            readFromPrimary);
      nonMppPickupPointUpdate = updatedProductAndItemPickupPointDTO.getItemPickupPoints();
      product = updatedProductAndItemPickupPointDTO.getProduct();
    } else if (!mppForWhEnabled) {
      validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editItemResponse, readFromPrimary);
    }

    if (Objects.nonNull(editItemResponse.getApiErrorCode())) {
      return editItemResponse;
    }

    // Adding new L5s
    Pair<Set<String>, Boolean> fbbActivatedPickupPointsAndFbbChangedFlag =
        addNewPickupPoints(mandatoryRequestParam, itemPickupPointUpdateRequestVo, itemSkuAndItemMap, updatedItemList,
            newlyAddedL5s, newlyAddedCncTrueL5s, newlyAddedCncTrueL4s, itemPickupPointUpdateRequestVo.getProductSku(),
            newlyAddedPickupPointCodes, itemsNewlyAddedL5, fbbChangedAtL5,
            readFromPrimary);
    fbbChangedAtL5 = fbbActivatedPickupPointsAndFbbChangedFlag.getRight();
    fbbActivatedToTruePps.addAll(fbbActivatedPickupPointsAndFbbChangedFlag.getLeft());

    // Validate cnc flags for update request
    validateCncFlagsForUpdateRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests(), itemPickupPointMap, existingL5sMadeCncFalse,
        itemSkuAndItemMap, updatedItemList, newlyAddedCncTrueL4s, nonMppPickupPointUpdate, readFromPrimary);

    // Deleting L5s
    boolean deletedFbbTrueL5 =
        deletePickupPoints(mandatoryRequestParam, itemPickupPointUpdateRequestVo, itemSkuAndItemMap, updatedItemList,
        newlyAddedCncTrueL4s, existingL5sMadeCncFalse, deletedPickupPointCodes, itemPickupPointUpdateRequestVo,
        itemsNewlyAddedL5, readFromPrimary);
    fbbChangedAtL5 = deletedFbbTrueL5 ? Boolean.TRUE : fbbChangedAtL5;

    // Updating existing L5s
    BasicProductAndItemEditedDetailsDTO basicProductAndItemEditedDetailsDTO =
        updatingExistingPickupPoints(itemPickupPointUpdateRequestVo, itemSkuAndItemMap, updatedItemList,
            itemPickupPointMap, newlyAddedPickupPointCodes, deletedPickupPointCodes, editItemResponse, fbbChangedAtL5,
            fbbActivatedToTruePps, deletedFbbTrueL5, product,
            isBundleProduct, readFromPrimary);

    List<ItemPickupPoint> combinedList = new ArrayList<>(basicProductAndItemEditedDetailsDTO.getItemPickupPoints());
    if (CollectionUtils.isNotEmpty(nonMppPickupPointUpdate)) {
      Set<String> nonMppPickupPointIds = CommonUtil.getItemPickupPointIds(combinedList);
      for (ItemPickupPoint updatedItemPickupPoint : nonMppPickupPointUpdate) {
        if (!nonMppPickupPointIds.contains(updatedItemPickupPoint.getId())) {
          combinedList.add(updatedItemPickupPoint);
        }
      }
    }

    editItemResponse.setUpdatedItemPickupPoints(combinedList);
    editItemResponse.setFbbFlagChangedAtL3Level(basicProductAndItemEditedDetailsDTO.isFbbFlagChangedAtL3Level());
    return editItemResponse;
  }

  public void validateFbbForNonMppSeller(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      MandatoryRequestParam mandatoryRequestParam) {
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo =
        itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0);
    ItemPickupPointListingUpdateRequestVo deleteEditPp = new ItemPickupPointListingUpdateRequestVo();
    if (itemPickupPointListingUpdateRequestVo.isFbbActivated()) {
      List<String> fbbActivatedValidPickupPointCodes =
          businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
              Collections.singleton(itemPickupPointListingUpdateRequestVo.getPickupPointCode()));
      if (CollectionUtils.isEmpty(fbbActivatedValidPickupPointCodes)) {
        log.error("The pickupPoint : {} not eligible for FBB, removing the L5 from the edit request. itemSku : {} , "
                + "requestId : {}", itemPickupPointListingUpdateRequestVo.getPickupPointCode(),
            itemPickupPointListingUpdateRequestVo.getItemSku(), mandatoryRequestParam.getRequestId());
        deleteEditPp = itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0);
      }
    }
    List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVos =
        new ArrayList<>(itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests());
    itemPickupPointListingUpdateRequestVos.remove(deleteEditPp);
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(itemPickupPointListingUpdateRequestVos);
  }

  public void validateForOneFbbTruePickupPointPerItem(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, EditItemResponse editItemResponse, boolean readFromPrimary) {
    Map<String, Set<String>> fbbTrueItemSkuAndPickupPointsMap = new HashMap<>();
    Set<String> itemSkuSet = itemPickupPointUpdateRequestVo.getAddPickupPointRequests().stream()
        .map(ItemPickupPointListingUpdateRequestVo::getItemSku).collect(Collectors.toSet());
    itemSkuSet.addAll(itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().stream()
        .map(ItemPickupPointDeleteRequestVo::getItemSku).collect(Collectors.toSet()));
    List<ItemPickupPoint> fbbTrueItemPickupPoint = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemSkuSet)) {
        fbbTrueItemPickupPoint =
            dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
                mandatoryRequestParam.getStoreId(), itemSkuSet, Boolean.TRUE, readFromPrimary);
    }
    for (ItemPickupPoint itemPickupPoint : fbbTrueItemPickupPoint) {
      fbbTrueItemSkuAndPickupPointsMap.put(itemPickupPoint.getItemSku(),
          new HashSet<>(Arrays.asList(itemPickupPoint.getPickupPointCode())));
    }
    populateFbbTrueItemSkuAndPpMapFromDeletePpRequest(itemPickupPointUpdateRequestVo, fbbTrueItemSkuAndPickupPointsMap);
    populateFbbTrueItemSkuAndPpMapFromAddPpRequest(mandatoryRequestParam,
      itemPickupPointUpdateRequestVo, fbbTrueItemSkuAndPickupPointsMap, editItemResponse);
  }

  private static void populateFbbTrueItemSkuAndPpMapFromAddPpRequest(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      Map<String, Set<String>> fbbTrueItemSkuAndPickupPointsMap, EditItemResponse editItemResponse) {
    for (ItemPickupPointListingUpdateRequestVo addPpRequest : itemPickupPointUpdateRequestVo.getAddPickupPointRequests()) {
      Set<String> fbbTruePPs =
          Optional.ofNullable(fbbTrueItemSkuAndPickupPointsMap.get(addPpRequest.getItemSku())).orElse(new HashSet<>());
      if (addPpRequest.isFbbActivated()) {
        fbbTruePPs.add(addPpRequest.getPickupPointCode());
        if (fbbTruePPs.size() > 1) {
          log.error("For this itemSku : {} already has a FBB true pickupPoint found in existing data or edit request, "
                  + "so removing this L5 from the add pickupPoint request. pickupPointCode : {} , requestId : {} ",
              addPpRequest.getItemSku(), addPpRequest.getPickupPointCode(), mandatoryRequestParam.getRequestId());
          editItemResponse.setApiErrorCode(ApiErrorCode.FBB_PICKUP_POINT_ALREADY_EXISTS);
        } else {
          fbbTrueItemSkuAndPickupPointsMap.put(addPpRequest.getItemSku(), fbbTruePPs);
        }
      }
    }
    List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVos =
        new ArrayList<>(itemPickupPointUpdateRequestVo.getAddPickupPointRequests());
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(itemPickupPointListingUpdateRequestVos);
  }

  private static void populateFbbTrueItemSkuAndPpMapFromDeletePpRequest(
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      Map<String, Set<String>> fbbTrueItemSkuAndPickupPointsMap) {
    for (ItemPickupPointDeleteRequestVo deletePpRequest : itemPickupPointUpdateRequestVo.getDeletePickupPointRequests()) {
      if (fbbTrueItemSkuAndPickupPointsMap.containsKey(deletePpRequest.getItemSku())
          && fbbTrueItemSkuAndPickupPointsMap.get(deletePpRequest.getItemSku())
          .contains(deletePpRequest.getPickupPointCode())) {
        fbbTrueItemSkuAndPickupPointsMap.put(deletePpRequest.getItemSku(), new HashSet<>());
      }
    }
  }

  private void validateCncFlagsForUpdateRequest(MandatoryRequestParam mandatoryRequestParam,
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
    List<ItemPickupPointListingUpdateRequestVo> quickEditUpdateRequests,
    Map<String, ItemPickupPoint> itemPickupPointMap, Set<ItemPickupPoint> existingL5sMadeCncFalse,
    Map<String, Item> itemSkuAndItemMap, Set<Item> updatedItemList,
    Set<String> newlyAddedCncTrueL4s, List<ItemPickupPoint> nonMppPickupPointUpdate,
    boolean readFromPrimary) {
    Set<String> cncActivatedPickupPointCodes = new HashSet<>();
    Map<String, ItemPickupPoint> updatedItemPickupPointMap =
      nonMppPickupPointUpdate.stream().collect(
      Collectors.toMap(itemPickupPoint -> itemPickupPoint.getItemSku() + Constants.HYPHEN
        + itemPickupPoint.getPickupPointCode(), Function.identity(), (a, b) -> a));
    for (ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo : quickEditUpdateRequests) {
      ItemPickupPoint itemPickupPoint =
          getItemPickupPoint(itemPickupPointListingUpdateRequestVo, itemPickupPointMap,
            updatedItemPickupPointMap, readFromPrimary);
      if ((!cncForWarehouseFeatureSwitch && itemPickupPointListingUpdateRequestVo.isCncActivated() && !itemPickupPoint.isCncActive()) ||
          (cncForWarehouseFeatureSwitch && checkCncViewConfig(
              itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.CNC))
              && !checkCncViewConfig(itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC)))) {
        // If cnc flag is being made as true now, need to validate the cnc status of the pp code
        cncActivatedPickupPointCodes.add(itemPickupPointListingUpdateRequestVo.getPickupPointCode());
      } else if ((!cncForWarehouseFeatureSwitch && !itemPickupPointListingUpdateRequestVo.isCncActivated() && itemPickupPoint.isCncActive()) ||
          (cncForWarehouseFeatureSwitch &&
              !checkCncViewConfig(itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.CNC)) &&
              checkCncViewConfig(itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC)))) {
        // If cnc is being made as false, adding to a set to later use it to determine cnc flag at L4 level
        existingL5sMadeCncFalse.add(itemPickupPoint);
      }
    }
    validateCncAndDeliveryPickupPoints(mandatoryRequestParam, itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests());
    for (ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo : itemPickupPointUpdateRequestVo
        .getQuickEditUpdateRequests()) {
      if ((!cncForWarehouseFeatureSwitch && itemPickupPointListingUpdateRequestVo.isCncActivated()) ||
          (cncForWarehouseFeatureSwitch &&
              checkCncViewConfig(itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.CNC)))) {
        setCncFlagAtL4Level(mandatoryRequestParam, itemSkuAndItemMap, updatedItemList, newlyAddedCncTrueL4s,
            itemPickupPointListingUpdateRequestVo, readFromPrimary);
      }
    }
  }

  private boolean checkCncViewConfig(ItemViewConfig itemViewConfig) {
    return Objects.nonNull(itemViewConfig) && (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable());
  }

  private void setCncFlagAtL4Level(MandatoryRequestParam mandatoryRequestParam, Map<String, Item> itemSkuAndItemMap,
      Set<Item> updatedItemList, Set<String> newlyAddedCncTrueL4s,
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo,
    boolean readFromPrimary) {
    Item item = getItem(mandatoryRequestParam.getStoreId(), itemPickupPointListingUpdateRequestVo.getItemSku(),
        itemSkuAndItemMap, readFromPrimary);
    // Making L4 as CNC_FLAG_UPDATE activated true
    if (CommonUtil.isNotCncActivatedItem(item)) {
      item.setCncActivated(true);
      updatedItemList.add(item);
      itemSkuAndItemMap.replace(item.getItemSku(), item);
      newlyAddedCncTrueL4s.add(item.getItemSku());
    }
  }

  private ItemPickupPoint getItemPickupPoint(ItemPickupPointListingUpdateRequestVo
      itemPickupPointListingUpdateRequestVo, Map<String, ItemPickupPoint> itemPickupPointMap,
    Map<String, ItemPickupPoint> updatedItemPickupPointMap, boolean readFromPrimary) {
    ItemPickupPoint itemPickupPoint;
      String offlineItemId = itemPickupPointListingUpdateRequestVo.getItemSku() + Constants.HYPHEN +
        itemPickupPointListingUpdateRequestVo.getPickupPointCode();
      itemPickupPoint = Optional.ofNullable(updatedItemPickupPointMap.get(offlineItemId))
        .orElseGet(() -> fetchItemPickupPointByItemSkuAndPickupPointCode(itemPickupPointListingUpdateRequestVo.getItemSku(),
            itemPickupPointListingUpdateRequestVo.getPickupPointCode(), readFromPrimary)
        );
    // With MPP on key is offfineItemId
    itemPickupPointMap.putIfAbsent(itemPickupPoint.getOfflineItemId(), itemPickupPoint);
    return itemPickupPoint;
  }

  private Pair<Set<String>, Boolean> addNewPickupPoints(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Map<String, Item> itemSkuAndItemMap,
      Set<Item> updatedItemList, List<String> newlyAddedL5s, Set<String> newlyAddedCncTrueL5s,
      Set<String> newlyAddedCncTrueL4s, String productSku, Set<String> newlyAddedPickupPointCodes,
      Set<String> itemsNewlyAddedL5, boolean fbbChangedAtL5, boolean readFromPrimary) {
    Set<String> fbbActivatedPickupPointCode = new HashSet<>();
    Map<String, String> merchantSkuUpdatedL4sMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequestVo.getAddPickupPointRequests())) {
      List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
      log.info("Adding new pickupPoints userName {}, requestId : {}", mandatoryRequestParam.getUsername(),
          mandatoryRequestParam.getRequestId());
      // Validate if pickup point is CNC_FLAG_UPDATE true or FBB true if it is present in request
      validateCncAndDeliveryPickupPoints(mandatoryRequestParam, itemPickupPointUpdateRequestVo.getAddPickupPointRequests());
      fbbActivatedPickupPointCode = itemPickupPointUpdateRequestVo.getAddPickupPointRequests().stream()
          .filter(ItemPickupPointListingUpdateRequestVo::isFbbActivated)
          .map(ItemPickupPointListingUpdateRequestVo::getPickupPointCode).collect(Collectors.toSet());
      validateFbbPickupPointsForAddPpRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
          fbbActivatedPickupPointCode);
      List<String> newVariants = new ArrayList<>();
      if (Objects.nonNull(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())) {
        newVariants = itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getAddVariantsList().stream()
            .map(AddVariantRequestVo::getItemSku).collect(Collectors.toList());
      }
      Map<String, Integer> itemSkuToL5Count = new HashMap<>();
      for (ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo : itemPickupPointUpdateRequestVo.getAddPickupPointRequests()) {
        itemSkuToL5Count.put(itemPickupPointListingUpdateRequestVo.getItemSku(),
            itemSkuToL5Count.getOrDefault(itemPickupPointListingUpdateRequestVo.getItemSku(), 0) + 1);
      }
      for (ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo : itemPickupPointUpdateRequestVo
          .getAddPickupPointRequests()) {
        if (itemPickupPointListingUpdateRequestVo.isFbbActivated()) {
          fbbChangedAtL5 = true;
        }
        itemsNewlyAddedL5.add(itemPickupPointListingUpdateRequestVo.getItemSku());
        setStatusFlag(itemPickupPointUpdateRequestVo, itemPickupPointListingUpdateRequestVo);
        itemPickupPointListingUpdateRequestVo.setProductSku(productSku);
        ItemPickupPoint itemPickupPoint =
            getItemPickupPoint(mandatoryRequestParam, itemPickupPointListingUpdateRequestVo,
              readFromPrimary);
        setDeliveryFLag(itemPickupPointListingUpdateRequestVo, newVariants, itemSkuToL5Count, itemPickupPoint);
        newlyAddedPickupPointCodes.add(itemPickupPoint.getPickupPointCode());
        updatedItemPickupPointList.add(itemPickupPoint);
        boolean merchantSkuUpdated =
            setMerchantSkuAtL4(mandatoryRequestParam, itemSkuAndItemMap, itemPickupPoint, readFromPrimary);
        if (merchantSkuUpdated) {
          merchantSkuUpdatedL4sMap.put(itemPickupPoint.getItemSku(), itemPickupPoint.getMerchantSku());
        }
      }
      setMerchantSkuAtL4AndL5(mandatoryRequestParam, itemPickupPointUpdateRequestVo, itemSkuAndItemMap, updatedItemList,
        merchantSkuUpdatedL4sMap, updatedItemPickupPointList);
      List<String> itemSkusNewlyAdded = new ArrayList<>();
      saveItemPickupPoints(itemPickupPointUpdateRequestVo, newlyAddedL5s, newlyAddedCncTrueL5s,
        updatedItemPickupPointList, itemSkusNewlyAdded);
      setCncFlagAtL4AfterAddingNewL5s(mandatoryRequestParam, itemSkuAndItemMap, updatedItemList, newlyAddedCncTrueL5s,
          newlyAddedCncTrueL4s, readFromPrimary);
    }
    return Pair.of(fbbActivatedPickupPointCode, fbbChangedAtL5);
  }

  private void setMerchantSkuAtL4AndL5(MandatoryRequestParam mandatoryRequestParam,
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
    Map<String, Item> itemSkuAndItemMap, Set<Item> updatedItemList,
    Map<String, String> merchantSkuUpdatedL4sMap,
    List<ItemPickupPoint> updatedItemPickupPointList) {
    if (updatedMerchantSkuFromNewlyAddedL5 && MapUtils.isNotEmpty(merchantSkuUpdatedL4sMap)) {
      log.info("MerchantSku updated for newly added items productSku : {}, map : {} ",
          itemPickupPointUpdateRequestVo.getProductSku(), merchantSkuUpdatedL4sMap);
      for (Map.Entry<String, String> itemSkuAndMerchantSku : merchantSkuUpdatedL4sMap.entrySet()) {
        Item item = getItem(mandatoryRequestParam.getStoreId(), itemSkuAndMerchantSku.getKey(), itemSkuAndItemMap, false);
        item.setMerchantSku(itemSkuAndMerchantSku.getValue());
        updatedItemList.add(item);
        setMerchantSkuForAllL5s(itemPickupPointUpdateRequestVo, updatedItemPickupPointList,
            itemSkuAndMerchantSku.getKey(), itemSkuAndMerchantSku.getValue());
      }
    }
  }

  private static void setStatusFlag(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo) {
    if (Objects.nonNull(itemPickupPointUpdateRequestVo.getOnline()) && Boolean.FALSE.equals(
        itemPickupPointUpdateRequestVo.getOnline())) {
      itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setBuyable(false);
      itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setDiscoverable(false);
      itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setItemDiscoverableSchedules(null);
      itemPickupPointListingUpdateRequestVo.getSingleItemViewConfigByChannel(Constants.DEFAULT_CHANNEL).setItemBuyableSchedules(null);
    }
  }

  private static void setDeliveryFLag(ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo,
      List<String> newVariants, Map<String, Integer> itemSkuToL5Count, ItemPickupPoint itemPickupPoint) {
    if (newVariants.contains(itemPickupPointListingUpdateRequestVo.getItemSku())
        && itemSkuToL5Count.get(itemPickupPointListingUpdateRequestVo.getItemSku()) == 1) {
      itemPickupPoint.setDelivery(true);
    }
  }

  private void saveItemPickupPoints(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, List<String> newlyAddedL5s,
      Set<String> newlyAddedCncTrueL5s, List<ItemPickupPoint> updatedItemPickupPointList, List<String> itemSkusNewlyAdded) {
    if (CollectionUtils.isNotEmpty(updatedItemPickupPointList)) {
      if (Objects.nonNull(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())
          && CollectionUtils.isNotEmpty(
          itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getAddVariantsList())) {
        itemSkusNewlyAdded =
            itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getAddVariantsList().stream()
                .map(AddVariantRequestVo::getItemSku).collect(Collectors.toList());
      }
        saveAndPublishNewlyAddedL5s(newlyAddedL5s, newlyAddedCncTrueL5s, updatedItemPickupPointList, itemSkusNewlyAdded);
    }
  }

  private void setMerchantSkuForAllL5s(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      List<ItemPickupPoint> updatedItemPickupPointList, String itemSku, String merchantSku) {
    if (CollectionUtils.isEmpty(itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests())) {
      List<ItemPickupPoint> itemPickupPoints =
          pickupPointService.findByStoreIdAndItemSku(GdnMandatoryRequestParameterUtil.getStoreId(), itemSku);
      itemPickupPoints.forEach(itemPickupPoint1 -> itemPickupPoint1.setMerchantSku(merchantSku));
      updatedItemPickupPointList.addAll(itemPickupPoints);
    }
  }

  private boolean setMerchantSkuAtL4(MandatoryRequestParam mandatoryRequestParam,
    Map<String, Item> itemSkuAndItemMap, ItemPickupPoint itemPickupPoint, boolean readFromPrimary) {
    Item item =
      getItem(mandatoryRequestParam.getStoreId(), itemPickupPoint.getItemSku(), itemSkuAndItemMap,
        readFromPrimary);
    if(Objects.isNull(item)){
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ITEM_NOT_FOUND);
    }
    if (!StringUtils.equals(itemPickupPoint.getMerchantSku(), item.getMerchantSku())) {
      item.setMerchantSku(itemPickupPoint.getMerchantSku());
      return true;
    }
    return false;
  }

  private void setCncFlagAtL4AfterAddingNewL5s(MandatoryRequestParam mandatoryRequestParam,
      Map<String, Item> itemSkuAndItemMap, Set<Item> updatedItemList, Set<String> newlyAddedCncTrueL5s,
      Set<String> newlyAddedCncTrueL4s, boolean readFromPrimary) {
    for (String itemSku : newlyAddedCncTrueL5s) {
      Item item = getItem(mandatoryRequestParam.getStoreId(), itemSku, itemSkuAndItemMap, readFromPrimary);
      // Making L4 as CNC_FLAG_UPDATE activated true
      if (!item.isCncActivated()) {
        item.setCncActivated(true);
        updatedItemList.add(item);
        itemSkuAndItemMap.replace(itemSku, item);
        newlyAddedCncTrueL4s.add(itemSku);
      }
    }
  }

  private void saveAndPublishNewlyAddedL5s(List<String> newlyAddedL5s, Set<String> newlyAddedCncTrueL5s,
      List<ItemPickupPoint> updatedItemPickupPointList, List<String> itemSkusNewlyAdded) {
    List<ItemPickupPoint> newlyAddedItemPickupPointList;
    overrideViewConfigForDistributionPP(updatedItemPickupPointList);
    newlyAddedItemPickupPointList = pickupPointService.saveItemPickupPoint(updatedItemPickupPointList);
    for (ItemPickupPoint itemPickupPoint : newlyAddedItemPickupPointList) {
      itemPickupPoint.setNewData(true);
      if (includeVariantUpdate && CollectionUtils.isNotEmpty(itemSkusNewlyAdded)) {
        if (itemSkusNewlyAdded.contains(itemPickupPoint.getItemSku())) {
          itemPickupPoint.setItemPickupPointDataChangeType(
              new ArrayList<>(Collections.singletonList(ItemPickupPointChangeEventType.VARIANT_CHANGE.getName())));
        }
      }
      if ((!cncForWarehouseFeatureSwitch && itemPickupPoint.isCncActive()) ||
          (cncForWarehouseFeatureSwitch && itemPickupPoint.getItemViewConfigByChannel(Constants.CNC).stream()
              .anyMatch(itemViewConfig -> itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()))) {
        newlyAddedCncTrueL5s.add(itemPickupPoint.getItemSku());
      }
      newlyAddedL5s.add(itemPickupPoint.getItemSku());
      pickupPointService.publishItemPickupPointChangeEvent(itemPickupPoint);
    }
  }

  private boolean deletePickupPoints(MandatoryRequestParam mandatoryRequestParam,
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
    Map<String, Item> itemSkuAndItemMap, Set<Item> updatedItemList,
    Set<String> newlyAddedCncTrueL4s, Set<ItemPickupPoint> existingL5sMadeCncFalse,
    Set<String> deletedPickupPointCodes, ItemPickupPointUpdateRequestVo pickupPointUpdateRequestVo,
    Set<String> itemsNewlyAddedL5, boolean readFromPrimary) {
    boolean deletedFbbTrueL5 = false;
    Map<String, List<ItemPickupPoint>> allItemSkuAndItemPickupPointMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemPickupPointUpdateRequestVo.getDeletePickupPointRequests())) {
      Set<String> fbbTruePickupPointToBeDeletedSet =
          checkFbbForDeleteRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
            readFromPrimary);
      Set<String> pickupPointCodesToBeDeleted = itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().stream()
          .map(ItemPickupPointDeleteRequestVo::getPickupPointCode).collect(Collectors.toSet());
      pickupPointCodesToBeDeleted.addAll(fbbTruePickupPointToBeDeletedSet);
      List<ItemPickupPoint>
        itemPickupPointList = dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(
            mandatoryRequestParam.getStoreId(), pickupPointUpdateRequestVo.getProductSku(),
            new ArrayList<>(pickupPointCodesToBeDeleted), readFromPrimary);
      if (itemPickupPointList.stream().anyMatch(ItemPickupPoint::isDistribution)) {
        log.error("Cannot delete pickup points for distribution productSku : {} , requestId : {} ",
            pickupPointUpdateRequestVo.getProductSku(), mandatoryRequestParam.getRequestId());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.DISTRIBUTION_PICKUP_POINT_DELETE_NOT_ALLOWED);
      }
      Set<String> L5PartOfDeleteRequest =
          getDeletedPickupPointCodes(itemPickupPointUpdateRequestVo, deletedPickupPointCodes, itemPickupPointList);
      deletedFbbTrueL5 = itemPickupPointList.stream().anyMatch(ItemPickupPoint::isFbbActivated);
      Map<String, List<String>> itemSkuAndPickupPointListMap =
          getItemSkuAndL5sToBeDeleteListMap(itemPickupPointList, L5PartOfDeleteRequest);
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointListMap = new HashMap<>();
      getItemSkuAndItemPickupPointListMap(itemPickupPointList, itemSkuAndItemPickupPointListMap, L5PartOfDeleteRequest);
      log.info("L5 that are getting deleted as part of delete request for productSku : {} are {} ",
          itemPickupPointUpdateRequestVo.getProductSku(), itemSkuAndItemPickupPointListMap);
      for (Map.Entry<String, List<String>> itemSkuAndPickupPointList : itemSkuAndPickupPointListMap.entrySet()) {
        String itemSku = itemSkuAndPickupPointList.getKey();
        List<String> deleteVariantsList = new ArrayList<>();
        if (Objects.nonNull(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())) {
          deleteVariantsList = itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getDeleteVariantsList();
        }
        if (!itemsNewlyAddedL5.contains(itemSku) && !deleteVariantsList.contains(itemSku)) {
          validateDeleteL5(mandatoryRequestParam, itemSkuAndPickupPointList, itemSku, readFromPrimary);
        }
        List<ItemPickupPoint> itemPickupPointToBeDeleted = itemSkuAndItemPickupPointListMap.get(itemSku);
        addDeletedAndCncTrueToL5sMadeCncFalseList(existingL5sMadeCncFalse, itemPickupPointToBeDeleted);
        Item item = getItem(mandatoryRequestParam.getStoreId(), itemSku, itemSkuAndItemMap,
            readFromPrimary);
        if (missingEntitySwitch && Objects.isNull(item)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ITEM_NOT_FOUND);
        }
        CommonUtil.deletePickupPointsPromoAndDiscountDetails(itemPickupPointToBeDeleted);
        setCncFlagAtL4ForDeleteRequest(mandatoryRequestParam, updatedItemList, newlyAddedCncTrueL4s, itemSku, item,
            existingL5sMadeCncFalse, itemSkuAndItemMap,
            Optional.ofNullable(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())
                .orElse(new AddDeleteVariantRequestVo()).getDeleteVariantsList(),
          allItemSkuAndItemPickupPointMap, readFromPrimary);
        List<String> itemSkusToBeDeleted = new ArrayList<>();
        if (Objects.nonNull(itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo())
            && CollectionUtils.isNotEmpty(
            itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getDeleteVariantsList())) {
          itemSkusToBeDeleted = itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo().getDeleteVariantsList();
        }
          saveAndPublishDeletedL5(itemPickupPointToBeDeleted, itemSkusToBeDeleted);
      }
    } else if (!existingL5sMadeCncFalse.isEmpty()) {
      Set<String> distinctItems =
          existingL5sMadeCncFalse.stream().map(ItemPickupPoint::getItemSku).distinct().collect(Collectors.toSet());
      for (String itemSku : distinctItems) {
        Item item = getItem(mandatoryRequestParam.getStoreId(), itemSku, itemSkuAndItemMap,
            readFromPrimary);
        if (missingEntitySwitch && Objects.isNull(item)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ITEM_NOT_FOUND);
        }
        setCncFlagAtL4ForDeleteRequest(mandatoryRequestParam, updatedItemList, newlyAddedCncTrueL4s, itemSku, item,
            existingL5sMadeCncFalse, itemSkuAndItemMap, new ArrayList<>(),
          allItemSkuAndItemPickupPointMap, readFromPrimary);
      }
    }
    return deletedFbbTrueL5;
  }

  private Set<String> getDeletedPickupPointCodes(ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      Set<String> deletedPickupPointCodes, List<ItemPickupPoint> itemPickupPointList) {
    Map<String, Integer> numberOfTimesPPCodesPresentInProductCount = new HashMap<>();
    Map<String, Integer> numberOfTimesPPCodesPresentInDeleteRequest = new HashMap<>();
    itemPickupPointList.forEach(itemPickupPoint -> numberOfTimesPPCodesPresentInProductCount.put(itemPickupPoint.getPickupPointCode(),
        numberOfTimesPPCodesPresentInProductCount.getOrDefault(itemPickupPoint.getPickupPointCode(), 0) + 1));
    itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().forEach(
        ItemPickupPointDeleteRequestVo -> numberOfTimesPPCodesPresentInDeleteRequest
            .put(ItemPickupPointDeleteRequestVo.getPickupPointCode(),
                numberOfTimesPPCodesPresentInDeleteRequest.getOrDefault(ItemPickupPointDeleteRequestVo.getPickupPointCode(), 0) + 1));
    Set<String> L5PartOfDeleteRequest = itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().stream().map(
        itemPickupPointDeleteRequestVo -> itemPickupPointDeleteRequestVo.getItemSku() + Constants.HYPHEN
            + itemPickupPointDeleteRequestVo.getPickupPointCode()).collect(Collectors.toSet());
    // Only if ppCode is getting deleted from all the associated L4s, we will consider it as deleted ppCode
    for (Map.Entry<String, Integer> ppCodeCount : numberOfTimesPPCodesPresentInProductCount.entrySet()) {
      if (numberOfTimesPPCodesPresentInDeleteRequest.containsKey(ppCodeCount.getKey())
          && ppCodeCount.getValue() <= numberOfTimesPPCodesPresentInDeleteRequest.get(ppCodeCount.getKey())) {
        deletedPickupPointCodes.add(ppCodeCount.getKey());
      }
    }
    return L5PartOfDeleteRequest;
  }

  public Set<String> checkFbbForDeleteRequest(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, boolean readFromPrimary) {
    Set<String> fbbTruePickupPointToBeDeletedSet = new HashSet<>();
    if (Objects.nonNull(itemPickupPointUpdateRequestVo.getFbbActivated()) && !itemPickupPointUpdateRequestVo
        .getFbbActivated()) {
      Product product =
        this.dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
          GdnMandatoryRequestParameterUtil.getStoreId(),
          itemPickupPointUpdateRequestVo.getProductSku(), readFromPrimary);
      if (product.isFbbActivated()) {
        log.info("FBB toggle is turned off at L3 level for productSku : {} ",
            itemPickupPointUpdateRequestVo.getProductSku());
        List<ItemPickupPoint> itemPickupPointList =
          dataSourceWrapperService.findItemPickupPointByStoreIdAndProductSku(
            mandatoryRequestParam.getStoreId(), product.getProductSku(), readFromPrimary);
        Set<String> L5PartOfDeleteRequest = itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().stream().map(
            itemPickupPointDeleteRequestVo -> itemPickupPointDeleteRequestVo.getItemSku() + Constants.HYPHEN
                + itemPickupPointDeleteRequestVo.getPickupPointCode()).collect(Collectors.toSet());
        // Since flag is turned off at L3, deleting all associated L5s for that product
        for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
          if (itemPickupPoint.isFbbActivated() && !L5PartOfDeleteRequest
              .contains(itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode())) {
            itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().add(
                new ItemPickupPointDeleteRequestVo(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()));
          }
        }
      }
    }
    return fbbTruePickupPointToBeDeletedSet;
  }

  private void addDeletedAndCncTrueToL5sMadeCncFalseList(Set<ItemPickupPoint> existingL5sMadeCncFalse,
      List<ItemPickupPoint> itemPickupPointToBeDeleted) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointToBeDeleted) {
      if (Objects.nonNull(itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC)) && (
          itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC).isBuyable()
              || itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC).isDiscoverable())) {
        existingL5sMadeCncFalse.add(itemPickupPoint);
      }
    }
  }

  private void getItemSkuAndItemPickupPointListMap(List<ItemPickupPoint> itemPickupPointList,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointListMap, Set<String> l5PartOfDeleteRequest) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      if (l5PartOfDeleteRequest
          .contains(itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode())) {
        if (itemSkuAndItemPickupPointListMap.containsKey(itemPickupPoint.getItemSku())) {
          itemSkuAndItemPickupPointListMap.get(itemPickupPoint.getItemSku()).add(itemPickupPoint);
        } else {
          itemSkuAndItemPickupPointListMap
              .put(itemPickupPoint.getItemSku(), new ArrayList<>(Collections.singletonList(itemPickupPoint)));
        }
      }
    }
  }

  private void saveAndPublishDeletedL5(List<ItemPickupPoint> itemPickupPointToBeDeleted, List<String> itemSkusToBeDeleted) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointToBeDeleted) {
      itemPickupPoint.setCncActive(false);
      itemPickupPoint.setFbbActivated(false);
      itemPickupPoint.setMarkForDelete(true);
      for(ItemViewConfig itemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
        itemViewConfig.setDiscoverable(false);
        itemViewConfig.setBuyable(false);
      }
    }
    itemPickupPointToBeDeleted = pickupPointService.saveItemPickupPoint(itemPickupPointToBeDeleted);
    if (CollectionUtils.isNotEmpty(itemPickupPointToBeDeleted)) {
      deleteItemPickupPointsFromInventory(itemPickupPointToBeDeleted);
    }
    for (ItemPickupPoint itemPickupPoint : itemPickupPointToBeDeleted) {
      if (includeVariantUpdate && CollectionUtils.isNotEmpty(itemSkusToBeDeleted)) {
        if (itemSkusToBeDeleted.contains(itemPickupPoint.getItemSku())) {
          itemPickupPoint.setItemPickupPointDataChangeType(
              new ArrayList<>(Collections.singletonList(ItemPickupPointChangeEventType.VARIANT_CHANGE.getName())));
        }
      }
      pickupPointService.publishItemPickupPointChangeEvent(itemPickupPoint);
    }
  }

  private void deleteItemPickupPointsFromInventory(List<ItemPickupPoint> itemPickupPointToBeDeleted) {
    pickupPointService.deleteItemPickupPointsFromInventory(itemPickupPointToBeDeleted);
  }

  public void setDeletedL5ForCombinedRequestInDTO(List<ItemPickupPoint> itemPickupPointToBeDeleted,
      List<String> itemSkusToBeDeleted, EditProductDetailDTO editProductDetailDTO) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointToBeDeleted) {
      itemPickupPoint.setCncActive(false);
      itemPickupPoint.setFbbActivated(false);
      itemPickupPoint.setMarkForDelete(true);
    }
    //To be deleted from inventory after save
    for (ItemPickupPoint itemPickupPoint : itemPickupPointToBeDeleted) {
      if (includeVariantUpdate && CollectionUtils.isNotEmpty(itemSkusToBeDeleted)) {
        if (itemSkusToBeDeleted.contains(itemPickupPoint.getItemSku())) {
          itemPickupPoint.getItemPickupPointDataChangeType()
              .add(ItemPickupPointChangeEventType.VARIANT_CHANGE.getName());
        }
      }
    }
    CommonUtil.setOfflineItemIdToItemPickupPointMap(itemPickupPointToBeDeleted, editProductDetailDTO);
  }

  private void setCncFlagAtL4ForDeleteRequest(MandatoryRequestParam mandatoryRequestParam, Set<Item> updatedItemList,
      Set<String> newlyAddedCncTrueL4s, String itemSku, Item item, Set<ItemPickupPoint> existingL5sMadeCncFalse,
      Map<String, Item> itemSkuAndItemMap, List<String> deleteVariantRequest,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPoint, boolean readFromPrimary) {
    // Item can be made cnc false when no new cncTrue item is getting added as part of add request
    if (item.isCncActivated() && !newlyAddedCncTrueL4s.contains(itemSku)) {
      int numberOfL5sMadeCncFalseForThisItemSku =
          (int) existingL5sMadeCncFalse.stream().filter(itemPickupPoint -> itemSku.equals(itemPickupPoint.getItemSku()))
              .count();
      Long countOfActiveCncTrueL5 = null;
        if (!cncForWarehouseFeatureSwitch) {
          countOfActiveCncTrueL5 =
              dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(
                  mandatoryRequestParam.getStoreId(), itemSku, true, readFromPrimary);
        } else {
          countOfActiveCncTrueL5 =
              dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(
                  mandatoryRequestParam.getStoreId(), itemSku);
        }

      // Set flag as false when existing CNC_FLAG_UPDATE true L5s are getting deleted + existing CNC_FLAG_UPDATE true made as false
      if (numberOfL5sMadeCncFalseForThisItemSku >= countOfActiveCncTrueL5) {
        item.setCncActivated(false);
      }
    }  if (deleteVariantRequest.contains(item.getItemSku())) {
      item.setMarkForDelete(true);
      item.setPermanentDelete(true);
      List<ItemChangeEventType> itemChangeEventTypeList =
          Optional.ofNullable(item.getItemChangeEventTypes()).filter(CollectionUtils::isNotEmpty).map(ArrayList::new)
              .orElse(new ArrayList<>());
      itemChangeEventTypeList.add(ItemChangeEventType.ITEM_DELETED);
      item.setItemChangeEventTypes(itemChangeEventTypeList);
      itemSkuAndItemMap.replace(item.getItemSku(), item);
      updatedItemList.add(item);
    }
  }

  public void validateDeleteL5(MandatoryRequestParam mandatoryRequestParam,
    Map.Entry<String, List<String>> itemSkuAndPickupPointList, String itemSku, boolean readFromPrimary) {
    Long countOfActiveL5s =
      dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemSku, readFromPrimary);

    // If number of L5s in delete request is equal to the current number of active L5s mapped to the
    // L4 we will throw error
    if (countOfActiveL5s <= itemSkuAndPickupPointList.getValue().size()) {
      log.error("Delete not allowed, product will be left with no active L5 itemSku : {}, pickupPointList : {}",
          itemSku, itemSkuAndPickupPointList.getValue());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.DELETE_NOT_ALLOWED);
    }
  }

  private Map<String, List<String>> getItemSkuAndL5sToBeDeleteListMap(List<ItemPickupPoint> itemPickupPointList,
      Set<String> l5PartOfDeleteRequest) {
    Map<String, List<String>> itemSkuAndPickupPointListMap = new HashMap<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      if (l5PartOfDeleteRequest
          .contains(itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode())) {
        if (itemSkuAndPickupPointListMap.containsKey(itemPickupPoint.getItemSku())) {
          itemSkuAndPickupPointListMap.get(itemPickupPoint.getItemSku()).add(itemPickupPoint.getPickupPointCode());
        } else {
          itemSkuAndPickupPointListMap.putIfAbsent(itemPickupPoint.getItemSku(),
              new ArrayList<>(Collections.singletonList(itemPickupPoint.getPickupPointCode())));
        }
      }
    }
    return itemSkuAndPickupPointListMap;
  }

  private ItemPickupPoint getItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo, boolean readFromPrimary) {
    ItemPickupPoint itemPickupPoint =
      fetchAllItemPickupPointFromDB(mandatoryRequestParam, itemPickupPointListingUpdateRequestVo,
        readFromPrimary);

    itemPickupPoint.setNewData(true);
    itemPickupPoint.setUpdatedBy(mandatoryRequestParam.getUsername());
    itemPickupPoint.setProductSku(itemPickupPointListingUpdateRequestVo.getProductSku());
    itemPickupPoint.setDistribution(Boolean.TRUE.equals(itemPickupPointListingUpdateRequestVo.getDistribution()));
    itemPickupPoint.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    itemPickupPoint.setPrice(itemPickupPointListingUpdateRequestVo.getPrice());
    itemPickupPoint.setPickupPointCode(itemPickupPointListingUpdateRequestVo.getPickupPointCode());
    itemPickupPoint.setDelivery(false);
    itemPickupPoint.setCncActive(itemPickupPointListingUpdateRequestVo.isCncActivated());
    itemPickupPoint.setFbbActivated(itemPickupPointListingUpdateRequestVo.isFbbActivated());
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setOfflineItemId(CommonUtil
        .generateOfflineItemId(itemPickupPointListingUpdateRequestVo.getItemSku(),
            itemPickupPointListingUpdateRequestVo.getPickupPointCode()));
    itemPickupPoint.setStoreId(mandatoryRequestParam.getStoreId());
    itemPickupPoint.setItemViewConfig(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemPickupPoint.setMerchantCode(itemPickupPointListingUpdateRequestVo.getMerchantCode());
    itemPickupPoint.setMerchantSku(itemPickupPointListingUpdateRequestVo.getMerchantSku());
    if (StringUtils.isBlank(itemPickupPoint.getItemViewConfig().iterator().next().getChannel())) {
      itemPickupPoint.getItemViewConfig().iterator().next().setChannel(channelService.getDefaultChannel());
    }
    if(Objects.nonNull(itemPickupPointListingUpdateRequestVo.getWholesalePriceActivated())) {
      itemPickupPoint.setWholesalePriceExists(itemPickupPointListingUpdateRequestVo.getWholesalePriceActivated());
      if(itemPickupPointListingUpdateRequestVo.getWholesalePriceActivated()) {
        setWholesaleActivePromoBundling(itemPickupPoint);
      }
    }
    CommonUtil.setSchedulesForAddPickupPoint(schedulesAddEditEnabled,
      itemPickupPointListingUpdateRequestVo, itemPickupPoint);
    if (Objects.nonNull(itemPickupPointListingUpdateRequestVo.getB2bFieldsVo())) {
      B2bFields b2bFields = new B2bFields();
      b2bFields.setBasePrice(itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getBasePrice());
      b2bFields.setManaged(itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().isManaged());
      Set<ItemViewConfig> b2bItemViewConfigs =
          itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getB2bItemViewConfigs();
      b2bItemViewConfigs.iterator().next().setChannel(channelService.getB2BChannel());
      itemPickupPoint.setB2bFields(b2bFields);
      Set<ItemViewConfig> updatedItemViewConfig = itemPickupPoint.getItemViewConfig();
      updatedItemViewConfig.addAll(b2bItemViewConfigs);
      itemPickupPoint.setItemViewConfig(updatedItemViewConfig);
    }
    return itemPickupPoint;
  }

  private ItemPickupPoint fetchAllItemPickupPointFromDB(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo, boolean readFromPrimary) {
    return Optional.ofNullable(
            dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
                itemPickupPointListingUpdateRequestVo.getItemSku(),
                itemPickupPointListingUpdateRequestVo.getPickupPointCode(), readFromPrimary))
        .orElse(ItemPickupPoint.builder().newData(true).price(new HashSet<>()).itemViewConfig(new HashSet<>()).build());
  }

  private BasicProductAndItemEditedDetailsDTO updatingExistingPickupPoints(
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Map<String, Item> itemSkuAndItemMap,
      Set<Item> updatedItemList, Map<String, ItemPickupPoint> itemPickupPointMap,
      Set<String> newlyAddedPickupPointCodes, Set<String> deletedPickupPointCodes, EditItemResponse editItemResponse,
      boolean fbbChangedAtL5, Set<String> fbbActivatedPPsFromEditList, boolean deletedFbbTrueL5, Product product,
       boolean isBundleProduct, boolean readFromPrimary) {
    return updateItemPickupPointListing(itemPickupPointUpdateRequestVo.getProductSku(),
        itemPickupPointUpdateRequestVo.getProductType(), itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests(),
        updatedItemList, itemSkuAndItemMap, itemPickupPointMap, newlyAddedPickupPointCodes, deletedPickupPointCodes,
        itemPickupPointUpdateRequestVo.getOnline(), itemPickupPointUpdateRequestVo.getCnc(), editItemResponse,
        fbbChangedAtL5, fbbActivatedPPsFromEditList, deletedFbbTrueL5,
        itemPickupPointUpdateRequestVo.getAddDeleteVariantRequestVo(), itemPickupPointUpdateRequestVo, product,
      isBundleProduct, readFromPrimary);
  }

  private void validateCncAndDeliveryPickupPoints(MandatoryRequestParam mandatoryRequestParam,
      List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVos) {
    Set<String> cncActivatedPickupPointCodes =
        itemPickupPointListingUpdateRequestVos.stream().filter(this::isItemPickupPointListingUpdateRequestVoCnc)
            .map(ItemPickupPointListingUpdateRequestVo::getPickupPointCode).collect(Collectors.toSet());
    Set<String> deliveryPickupPointCodes =
        itemPickupPointListingUpdateRequestVos.stream().filter(this::isItemPickupPointListingUpdateRequestVoDelivery)
            .map(ItemPickupPointListingUpdateRequestVo::getPickupPointCode).collect(Collectors.toSet());

    Set<String> pickupPointCodes = new HashSet<>();
    pickupPointCodes.addAll(cncActivatedPickupPointCodes);
    pickupPointCodes.addAll(deliveryPickupPointCodes);
    Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      businessPartnerPickupPointMap = businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
              mandatoryRequestParam.getStoreId(), new ArrayList<>(pickupPointCodes)).stream()
          .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, Function.identity()));
    }
    for (ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo : itemPickupPointListingUpdateRequestVos) {
      BusinessPartnerPickupPoint businessPartnerPickupPoint;
      if (isItemPickupPointListingUpdateRequestVoCnc(itemPickupPointListingUpdateRequestVo)) {
        businessPartnerPickupPoint =
            businessPartnerPickupPointMap.get(itemPickupPointListingUpdateRequestVo.getPickupPointCode());
        if (Objects.nonNull(businessPartnerPickupPoint) && !businessPartnerPickupPoint.isCncActivated()) {
          for (ItemViewConfig itemViewConfig : itemPickupPointListingUpdateRequestVo.getItemViewConfigs()) {
            if (channelService.getCncChannel().equals(itemViewConfig.getChannel())) {
              log.error("PickupPoint : {} is not cncActivated, cnc view configs will be made false {}, requestId : {}",
                  businessPartnerPickupPoint.getCode(), itemPickupPointListingUpdateRequestVo,
                  mandatoryRequestParam.getRequestId());
              itemViewConfig.setBuyable(false);
              itemViewConfig.setDiscoverable(false);
            }
            itemPickupPointListingUpdateRequestVo.setCncActivated(false);
          }
        }
      }
      if (cncForWarehouseFeatureSwitch) {
        if (isItemPickupPointListingUpdateRequestVoDelivery(itemPickupPointListingUpdateRequestVo)) {
          businessPartnerPickupPoint =
              businessPartnerPickupPointMap.get(itemPickupPointListingUpdateRequestVo.getPickupPointCode());
          if (Objects.nonNull(businessPartnerPickupPoint) && !businessPartnerPickupPoint.isDelivery()) {
            for (ItemViewConfig itemViewConfig : itemPickupPointListingUpdateRequestVo.getItemViewConfigs()) {
              if (channelService.getDefaultChannel().equals(itemViewConfig.getChannel())) {
                log.error(
                    "PickupPoint : {} is not enabled for delivery, default view configs will be made false {}, requestId : {}",
                    businessPartnerPickupPoint.getCode(), itemPickupPointListingUpdateRequestVo,
                    mandatoryRequestParam.getRequestId());
                itemViewConfig.setBuyable(false);
                itemViewConfig.setDiscoverable(false);
              }
            }
          }
        }
      }
    }
  }

  private void validateFbbPickupPointsForAddPpRequest(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Set<String> fbbActivatedPickupPointCodes) {
    if (CollectionUtils.isNotEmpty(fbbActivatedPickupPointCodes)) {
      Set<String> fbbActivatedValidPickupPointCodeSet = new HashSet<>(
          businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(fbbActivatedPickupPointCodes));
      List<ItemPickupPointListingUpdateRequestVo> deletePpList = new ArrayList<>();
      for (ItemPickupPointListingUpdateRequestVo addPpRequest : itemPickupPointUpdateRequestVo.getAddPickupPointRequests()) {
        if (addPpRequest.isFbbActivated()) {
          if (!fbbActivatedValidPickupPointCodeSet.contains(addPpRequest.getPickupPointCode())) {
            log.error("The PickupPoint : {} is not fbbActivated, so removing the FBB L5 from the add pickupPoint "
                    + "request, itemSku : {} , requestId : {} ", addPpRequest.getPickupPointCode(),
                addPpRequest.getItemSku(), mandatoryRequestParam.getRequestId());
            deletePpList.add(addPpRequest);
            fbbActivatedPickupPointCodes.remove(addPpRequest.getPickupPointCode());
          }
        }
      }
      List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVos = new ArrayList<>(itemPickupPointUpdateRequestVo.getAddPickupPointRequests());
      itemPickupPointListingUpdateRequestVos.removeAll(deletePpList);
      itemPickupPointUpdateRequestVo.setAddPickupPointRequests(itemPickupPointListingUpdateRequestVos);
    }
  }

  private boolean checkAndUpdateMerchantSku(String storeId, Item item, String newMerchantSku, boolean itemDataChanged,
      ItemPickupPoint itemPickupPoint, List<ItemPickupPoint> updatedItemPickupPoints) {
    if (Objects.nonNull(newMerchantSku) && !StringUtils
      .equals(itemPickupPoint.getMerchantSku(), newMerchantSku)) {
      String existingMerchantSku = itemPickupPoint.getMerchantSku();
      item.setMerchantSku(newMerchantSku);
      if (!itemDataChanged) {
        item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DATA_CHANGE);
      }
      return true;
    }
    return false;
  }

  @Override
  public BasicProductAndItemEditedDetailsDTO validateAndUpdateItemPickupPointListing(MandatoryRequestParam mandatoryRequestParam,
      String productSku, ProductType productType, List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVos) {
    Set<Item> updatedItemSet = new HashSet<>();
    Map<String, Item> itemSkuAndItemMap = new HashMap<>();
    Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
    validateCncAndDeliveryPickupPoints(mandatoryRequestParam, itemPickupPointListingUpdateRequestVos);
    setCncActiveAtItemLevel(itemPickupPointListingUpdateRequestVos, updatedItemSet, itemSkuAndItemMap,
        itemPickupPointMap);
    return updateItemPickupPointListing(productSku, productType, itemPickupPointListingUpdateRequestVos, updatedItemSet,
        itemSkuAndItemMap, itemPickupPointMap, new HashSet<>(), new HashSet<>(), null, null, new EditItemResponse(),
        false, null, false, null, new ItemPickupPointUpdateRequestVo(), null, false, false);
  }

  private boolean isItemPickupPointListingUpdateRequestVoCnc(
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo) {
    if (cncForWarehouseFeatureSwitch) {
      return itemPickupPointListingUpdateRequestVo.getItemViewConfigs().stream().anyMatch(
          itemViewConfig -> channelService.getCncChannel().equals(itemViewConfig.getChannel()) && (
              itemViewConfig.isDiscoverable() || itemViewConfig.isBuyable()));
    } else {
      return itemPickupPointListingUpdateRequestVo.isCncActivated();
    }
  }

  private boolean isItemPickupPointListingUpdateRequestVoDelivery(
      ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo) {
    return itemPickupPointListingUpdateRequestVo.getItemViewConfigs().stream().anyMatch(
        itemViewConfig -> channelService.getDefaultChannel().equals(itemViewConfig.getChannel()) && (
            itemViewConfig.isDiscoverable() || itemViewConfig.isBuyable()));
  }

  private void setCncActiveAtItemLevel(List<ItemPickupPointListingUpdateRequestVo>
      itemPickupPointListingUpdateRequestVo, Set<Item> updatedItemSet, Map<String, Item>
      itemSkuAndItemMap, Map<String, ItemPickupPoint> itemPickupPointMap) {
    Set<String> cncFalseSkus;
    Set<String> cncTrueSkus;
    if (cncForWarehouseFeatureSwitch) {
      cncFalseSkus = itemPickupPointListingUpdateRequestVo.stream().filter(not(this::isItemPickupPointListingUpdateRequestVoCnc))
          .map(ItemPickupPointListingUpdateRequestVo::getItemSku).collect(Collectors.toSet());
      cncTrueSkus = itemPickupPointListingUpdateRequestVo.stream().filter(this::isItemPickupPointListingUpdateRequestVoCnc)
          .map(ItemPickupPointListingUpdateRequestVo::getItemSku).collect(Collectors.toSet());
    } else {
      cncFalseSkus = itemPickupPointListingUpdateRequestVo.stream().filter(request -> !request.isCncActivated())
          .map(ItemPickupPointListingUpdateRequestVo::getItemSku).collect(Collectors.toSet());
      cncTrueSkus = itemPickupPointListingUpdateRequestVo.stream()
          .filter(ItemPickupPointListingUpdateRequestVo::isCncActivated)
          .map(ItemPickupPointListingUpdateRequestVo::getItemSku).collect(Collectors.toSet());
    }
    Set<String> finalCncTrueSkus = cncTrueSkus;
    Set<String> onlyCncFalseSkus =
        cncFalseSkus.stream().filter(sku -> !finalCncTrueSkus.contains(sku)).collect(Collectors.toSet());
    Map<String, Long> l5TrueCountMap = new HashMap<>();
    for (ItemPickupPointListingUpdateRequestVo request : itemPickupPointListingUpdateRequestVo) {
      updateCncActivatedFlagAtItemLevel(request, updatedItemSet, itemSkuAndItemMap, onlyCncFalseSkus,
          l5TrueCountMap, itemPickupPointMap);
    }
  }

  private void updateCncActivatedFlagAtItemLevel(ItemPickupPointListingUpdateRequestVo request,
      Set<Item> updatedItemSet, Map<String, Item> itemSkuAndItemMap, Set<String> onlyCncFalseSkus,
      Map<String, Long> l5TrueCountMap, Map<String, ItemPickupPoint> itemPickupPointMap)
      throws ApplicationRuntimeException {
    Item item =
        getItem(GdnMandatoryRequestParameterUtil.getStoreId(), request.getItemSku(), itemSkuAndItemMap, false);
    if (missingEntitySwitch && Objects.isNull(item)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ITEM_NOT_FOUND);
    }
    if (onlyCncFalseSkus.contains(item.getItemSku())) {
      if (item.isCncActivated()) {
        ItemPickupPoint itemPickupPoint =
            fetchItemPickupPointByItemSkuAndPickupPointCode(request.getItemSku(), request.getPickupPointCode(), false);
        Long currentL5CncTrueCount = generateCurrentL5CncTrueCount(request, l5TrueCountMap, item);
        ItemViewConfig cncViewConfig =
            itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.CNC);
        if (cncViewConfig.isBuyable() || cncViewConfig.isDiscoverable()) {
          currentL5CncTrueCount -= 1;
          if (currentL5CncTrueCount == 0) {
            item.setCncActivated(false);
            updatedItemSet.add(item);
          }
        }
        l5TrueCountMap.put(request.getItemSku(), currentL5CncTrueCount);
        itemPickupPointMap.put(request.getItemSku() + Constants.DASH + request.getPickupPointCode(), itemPickupPoint);
      }
    } else if (!item.isCncActivated()) {
      item.setCncActivated(true);
      updatedItemSet.add(item);
    }
  }

  private Long generateCurrentL5CncTrueCount(ItemPickupPointListingUpdateRequestVo request,
      Map<String, Long> l5TrueCountMap, Item item) {
    Long currentL5CncTrueCount;
    if (cncForWarehouseFeatureSwitch) {
      currentL5CncTrueCount = l5TrueCountMap.getOrDefault(request.getItemSku(),
          pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(item.getStoreId(),
              item.getItemSku(), channelService.getCncChannel()));
    } else {
      currentL5CncTrueCount = l5TrueCountMap.getOrDefault(request.getItemSku(),
          pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(item.getStoreId(),
              item.getItemSku(), true));
    }
    return currentL5CncTrueCount;
  }
}