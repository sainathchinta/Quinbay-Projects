package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.helper.GdnCommonHelper.getIfNotNull;
import static com.gdn.common.helper.GdnNumberHelper.is;
import static com.gdn.common.helper.constants.Comparator.GTE;
import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.OfflineItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.OfflineItemChangeResponse;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpsertOfflineItemPriceResponseVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.PickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ReindexService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.google.common.collect.ImmutableSet;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class OfflineItemServiceImpl implements OfflineItemService {

  private static final Logger LOG = LoggerFactory.getLogger(OfflineItemServiceImpl.class);

  private static final String LIST_OFFLINE_ITEM_MUST_NOT_BE_EMPTY = "list offline item must not be empty";
  public static final String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchantCode must not be blank";
  public static final String PAGEABLE_MUST_NOT_BE_NULL = "pageable must not be null";
  public static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  public static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";
  public static final String ITEM_SKUS_MUST_NOT_BE_EMPTY = "itemSkus must not be empty";
  public static final String MERCHANT_SKU_MUST_NOT_BE_BLANK = "merchantSku must not be blank";
  public static final String MERCHANT_CODES_MUST_NOT_BE_BLANK = "merchantCodes must not be blank";
  public static final String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "pickupPointCode must not be blank";
  private static final String EXTERNAL_PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "externalPickupPointCode must not be blank";
  private static final String LIST_PRICE_MUST_NOT_BE_NULL = "listPrice must not be null";
  private static final String LIST_PRICE_MUST_BE_GREATER_THAN_OR_EQ_TO = "listPrice must be greater than or equal to ";
  private static final String OFFER_PRICE_MUST_NOT_BE_NULL = "offerPrice must not be null";
  private static final String OFFER_PRICE_MUST_BE_GREATER_THAN_OR_EQ_TO = "offerPrice must be greater than or equal to ";
  private static final String PRODUCT_OR_ITEM_NOT_FOUND = "Product or Item not Found";
  private static final String PRODUCT_NOT_FOUND = "product not found";
  private static final String ITEM_NOT_FOUND = "Item not found";
  private static final String DEFAULT = "DEFAULT";
  private static final String OFFLINE_ITEM_ID_NOT_BE_BLANK = "offline item id must not be blank";
  public static final String UNIQUE_ID_MUST_NOT_BE_BLANK = "uniqueId must not be blank";
  public static final String UNIQUE_ID_NOT_FOUND = "uniqueId not found";
  public static final String ITEM_IS_ALREADY_ARCHIVED = "item is already archived";
  private static final String ITEM_SKU_NOT_FOUND_ID = "Item SKU tidak ditemukan.";

  @Autowired
  private OfflineItemRepository offlineItemRepository;

  @Autowired
  private PickupPointService pickupPointService;

  @Autowired
  @Lazy
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private SaveOperationService saveOperationSolrService;

  @Autowired
  private SkuValidatorImpl skuValidator;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  private GdnMapper gdnMapper;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ProductSearchHelperService productSearchHelper;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private ReindexService reindexService;

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${offline.item.excluded.username}")
  private String excludedUserNames;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private ExecutorService executorService;

  @Override
  public OfflineItem findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
      String storeId, String itemSku, String pickupPointCode) {

    return this.offlineItemRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        storeId, itemSku, pickupPointCode);
  }

  @Override
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode(String storeId, List<OfflineItem> offlineItems,
      boolean markForDelete, String merchantCode, String pickupPointCode) {

    checkArgument(StringUtils.isNotBlank(storeId), OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    try {
      List<OfflineItem> newOfflineItems =
          offlineItems.stream().filter(offlineItem -> merchantCode.equals(offlineItem.getMerchantCode()))
              .filter(offlineItem -> (markForDelete != offlineItem.isMarkForDelete())).collect(Collectors.toList());
      log.info("#updateMarkForDeleteByMerchantCodeAndPickupPointCode offlineItems : {}", newOfflineItems);
      newOfflineItems.forEach(offlineItem -> offlineItem.setMarkForDelete(markForDelete));
      offlineItemRepository.saveAll(newOfflineItems);
      saveAndPublishService.publishListOfOfflineItems(newOfflineItems, null);
    } catch (Exception e) {
      LOG.error(
          "Error on updateMarkForDeleteByMerchantCodeAndPickupPointCode with merchantCode:{}, " + "pickupPointCode:{}",
          merchantCode, pickupPointCode, e);
    }
  }

  @Override
  public void updateItemPickupPointForCNCDeactivation(String storeId,
      List<ItemPickupPoint> itemPickupPointList, String merchantCode, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    try {
      log.info(
          "#updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint , merchantCode : {} , pickupPointCode : {} , itemPickupPointList : {} ",
          merchantCode, pickupPointCode, itemPickupPointList);
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (StringUtils.equals(merchantCode, itemPickupPoint.getMerchantCode())) {
          Set<ItemViewConfig> itemViewConfigSet = itemPickupPoint.getAllItemViewConfigs();
          String cncChannel = channelService.getCncChannel();
          for (ItemViewConfig itemViewConfig : itemViewConfigSet) {
            if (cncChannel.equals(itemViewConfig.getChannel())) {
              itemViewConfig.setBuyable(false);
              itemViewConfig.setDiscoverable(false);
            }
          }
          itemPickupPoint.setItemViewConfig(itemViewConfigSet);
          itemPickupPoint.setCncActive(false);
        }
      }
      if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
        LOG.info(
            "#updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint merchantCode : {} , newItemPickupPoints : {} ",
            merchantCode, itemPickupPointList);
        itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
        saveAndPublishService.publishListOfItemsPickupPoints(itemPickupPointList, null);
      }
    } catch (Exception e) {
      LOG.error("Error on updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint with merchantCode : {} , "
          + "pickupPointCode : {} ", merchantCode, pickupPointCode, e);
    }
  }

  @Override
  public void updateItemPickupPointForDeliveryDeactivation(String storeId,
      List<ItemPickupPoint> itemPickupPointList, String merchantCode, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId),
        OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode),
        OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    try {
      log.info(
          "#updateItemPickupPointForDeliveryDeactivation , merchantCode : {} , pickupPointCode : "
              + "{} , itemPickupPointList : {} ", merchantCode, pickupPointCode,
          itemPickupPointList);
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (StringUtils.equals(merchantCode, itemPickupPoint.getMerchantCode())) {
          Set<ItemViewConfig> itemViewConfigSet = itemPickupPoint.getAllItemViewConfigs();
          String defaultChannel = channelService.getDefaultChannel();
          for (ItemViewConfig itemViewConfig : itemViewConfigSet) {
            if (defaultChannel.equals(itemViewConfig.getChannel())) {
              itemViewConfig.setBuyable(false);
              itemViewConfig.setDiscoverable(false);
            }
          }
          itemPickupPoint.setItemViewConfig(itemViewConfigSet);
        }
      }
      if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
        LOG.info("#updateItemPickupPointForDeliveryDeactivation merchantCode : {} , "
            + "newItemPickupPoints : {} ", merchantCode, itemPickupPointList);
        itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
        saveAndPublishService.publishListOfItemsPickupPoints(itemPickupPointList, null);
      }
    } catch (Exception e) {
      LOG.error("Error on updateItemPickupPointForDeliveryDeactivation with merchantCode : {} , "
          + "pickupPointCode : {} ", merchantCode, pickupPointCode, e);
    }
  }

  @Override
  public void updateMarkForDeleteByItemSkuAndPickupPointCode(String storeId, List<OfflineItem> offlineItems,
      boolean markForDelete, String merchantCode, String pickupPointCode) {

    checkArgument(StringUtils.isNotBlank(storeId), OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    try {
      List<OfflineItem> newOfflineItems =
          offlineItems.stream().filter(offlineItem -> merchantCode.equals(offlineItem.getMerchantCode()))
              .filter(offlineItem -> (markForDelete != offlineItem.isMarkForDelete())).collect(Collectors.toList());
      log.info("#updateMarkForDeleteByItemSkuAndPickupPointCode offlineItems : {}", newOfflineItems);
      Set<String> itemSkus = newOfflineItems.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

      Map<String, OfflineItem> itemSkuToOfflineItemMap =
          newOfflineItems.stream().collect(Collectors.toMap(OfflineItem::getItemSku, Function.identity()));

      List<Item> items = itemService.findByStoreIdAndItemSkus(storeId, itemSkus);

      List<OfflineItem> offlineItemsToSave = new ArrayList<>();
      for (Item item : items) {
        if (Objects.nonNull(item) && !item.isArchived() && itemSkuToOfflineItemMap.containsKey(item.getItemSku())) {
          OfflineItem offlineItem = itemSkuToOfflineItemMap.get(item.getItemSku());
          offlineItem.setMarkForDelete(markForDelete);
          offlineItem.setNewData(Boolean.TRUE);
          offlineItemsToSave.add(offlineItem);
        }
      }

      offlineItemRepository.saveAll(offlineItemsToSave);
      saveAndPublishService.publishListOfOfflineItems(offlineItemsToSave, null);
    } catch (Exception e) {
      LOG.error("Error on updateMarkForDeleteByItemSkuAndPickupPointCode with merchantCode:{}, " + "pickupPointCode:{}",
          merchantCode, pickupPointCode, e);
    }
  }

  @Override
  public void updateMarkForDeleteByMerchantCodeAndItemSku(String storeId, boolean markForDelete, Item item) {
    String merchantCode = item.getMerchantCode();
    String itemSku = item.getItemSku();
    checkArgument(StringUtils.isNotBlank(storeId),
        OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku),
        OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);

    try {
      List<OfflineItem> offlineItems = offlineItemRepository
          .findByStoreIdAndMerchantCodeAndItemSku(storeId, merchantCode, itemSku);
      if (!markForDelete){
        List<String> pickupPointCodesFromOfflineItem =
            offlineItems.stream().map(OfflineItem::getPickupPointCode).collect(Collectors.toList());

        List<String> pickupPointCodes = this.pickupPointService
            .findPickupPointListByPickupPointCodeInAndCncActivatedFalse(storeId,
                pickupPointCodesFromOfflineItem).stream().map(PickupPoint::getPickupPointCode)
            .collect(Collectors.toList());

        for (OfflineItem offlineItem : offlineItems){
          if (!pickupPointCodes.contains(offlineItem.getPickupPointCode())){
            offlineItem.setMarkForDelete(markForDelete);
          }
        }
        offlineItemRepository.saveAll(offlineItems);
      } else {
        offlineItems.forEach(offlineItem -> offlineItem.setMarkForDelete(markForDelete));
        offlineItemRepository.saveAll(offlineItems);
      }
      saveAndPublishService.publishListOfOfflineItems(offlineItems, null);
      OfflineItem offlineItem =
          offlineItems.stream().filter(offItem -> !offItem.isMarkForDelete()).findAny()
              .orElse(null);
      if (Objects.nonNull(offlineItem)) {
        item.setCncActivated(Boolean.TRUE);
        try {
          productAndItemSolrIndexerService.applyItem(item);
        } catch (Exception e) {
          LOG.error("Error on ProductAndItemSolrIndexer for applyItem with , item: {}", item, e);
        }
      }

    } catch (Exception e) {
      LOG.error("Error on updateMarkForDeleteByMerchantCodeAndItemSku with merchantCode:{}, " +
          "itemSku:{}", merchantCode, itemSku, e);
    }
  }

  @Override
  public List<UpsertOfflineItemPriceResponseVO> upsertOfflineItem(
      MandatoryRequestParam mandatoryRequestParam, String username, String merchantCode, List<UpsertOfflineItemRequest> upsertOfflineItemRequests) {

    List<UpsertOfflineItemPriceResponseVO> responses = new ArrayList<>();

    checkArgument(StringUtils.isNotBlank(mandatoryRequestParam.getStoreId()),
        OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(upsertOfflineItemRequests),
        OfflineItemServiceImpl.LIST_OFFLINE_ITEM_MUST_NOT_BE_EMPTY);

    Set<String> pickupPointIds = new HashSet<>();

    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList =
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(merchantCode);
    Set<String> pickupPointCodes =
        businessPartnerPickupPointList.stream().map(BusinessPartnerPickupPoint::getCode).collect(Collectors.toSet());
    Set<String> cncActivatedPickupPointCodes = businessPartnerPickupPointList.stream()
        .filter(businessPartnerPickupPoint -> businessPartnerPickupPoint.isCncActivated())
        .map(BusinessPartnerPickupPoint::getCode).collect(Collectors.toSet());
    Set<String> fbbActivatedPickupPointCodes = businessPartnerPickupPointList.stream()
      .filter(BusinessPartnerPickupPoint::isFbbActivated)
      .map(BusinessPartnerPickupPoint::getCode).collect(Collectors.toSet());

    Map<String, Item> itemMap = new HashMap<>();
    Map<String, List<ItemPickupPoint>> itemsMap = new HashMap<>();
    Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
    Map<String, Boolean> offlineItemIdToPureCNCStatusChangeMap = new HashMap<>();
    Map<String, Integer> itemSkuToCountOfExistingCncMadeFalse = new HashMap<>();
    List<AuditTrailDto> auditTrailResponseListUpdated = new ArrayList<>();
    List<String> existingL4MadeCncFalse = new ArrayList<>();
    List<String> l3MadeCncFalse = new ArrayList<>();
    Set<String> itemSkusWithNewCncTrueL5 = new HashSet<>();
    Map<String, String> itemSkuAndProductSkuMap = new HashMap<>();

    String minimumPriceSystemParameter = systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.MINIMUM_PRICE).getValue();
    Double minimumPrice = Double.parseDouble(minimumPriceSystemParameter);

    for (UpsertOfflineItemRequest upsertOfflineItemRequest : upsertOfflineItemRequests) {
      List<AuditTrailDto> auditTrailResponseList = new ArrayList<>();
      UpsertOfflineItemPriceResponseVO upsertOfflineItemPriceResponseVo =
          gdnMapper.deepCopy(upsertOfflineItemRequest, UpsertOfflineItemPriceResponseVO.class);
      try {
        checkArgument(StringUtils.isNotBlank(upsertOfflineItemPriceResponseVo.getItemSku()),
            OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
        checkArgument(StringUtils.isNotBlank(upsertOfflineItemPriceResponseVo.getPickupPointCode()),
            OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
        checkArgument(StringUtils.isNotBlank(upsertOfflineItemPriceResponseVo.getOfflineItemId()),
            OfflineItemServiceImpl.OFFLINE_ITEM_ID_NOT_BE_BLANK);

        String itemSkuPickupPointId =
            upsertOfflineItemRequest.getItemSku() + upsertOfflineItemRequest.getPickupPointCode();
        if (pickupPointIds.contains(itemSkuPickupPointId)) {
          upsertOfflineItemPriceResponseVo.setSuccess(Boolean.FALSE);
          upsertOfflineItemPriceResponseVo.setErrorMessage(ErrorMessages.DUPLICATE_UPSERT_OFFLINE_ITEM_ERROR);
          responses.add(upsertOfflineItemPriceResponseVo);
          continue;
        }
        pickupPointIds.add(itemSkuPickupPointId);
        this.isOfflineItemPriceValid(upsertOfflineItemPriceResponseVo.getListPrice(),
            upsertOfflineItemPriceResponseVo.getOfferPrice(), minimumPrice);

        ItemPickupPoint itemPickupPoint = Optional.ofNullable(
            itemPickupPointService.findByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
                upsertOfflineItemRequest.getItemSku(), upsertOfflineItemRequest.getPickupPointCode())).orElse(
            ItemPickupPoint.builder().newData(true).price(new HashSet<>()).itemViewConfig(new HashSet<>()).build());

        if (upsertOfflineItemRequest.isCncActive()) {
          if (!cncActivatedPickupPointCodes.contains(upsertOfflineItemPriceResponseVo.getPickupPointCode())) {
            upsertOfflineItemPriceResponseVo.setSuccess(Boolean.FALSE);
            upsertOfflineItemPriceResponseVo.setErrorMessage(ErrorMessages.PICKUP_POINT_NOT_ELIGIBLE_FOR_CNC);
            responses.add(upsertOfflineItemPriceResponseVo);
            continue;
          }
        } else if (!pickupPointCodes.contains(upsertOfflineItemPriceResponseVo.getPickupPointCode())) {
          upsertOfflineItemPriceResponseVo.setSuccess(Boolean.FALSE);
          upsertOfflineItemPriceResponseVo.setErrorMessage(ErrorMessages.INVALID_PICKUP_POINT);
          responses.add(upsertOfflineItemPriceResponseVo);
          continue;
        }
        else if (fbbActivatedPickupPointCodes.contains(upsertOfflineItemPriceResponseVo.getPickupPointCode())) {
          itemPickupPoint.setFbbActivated(true);
        }
        Item item = itemMap.containsKey(upsertOfflineItemRequest.getItemSku()) ?
            itemMap.get(upsertOfflineItemRequest.getItemSku()) :
            this.getItemByItemSku(mandatoryRequestParam, upsertOfflineItemRequest.getItemSku());
        itemPickupPoint.setProductSku(item.getProductSku());
        itemPickupPoint.setMerchantCode(item.getMerchantCode());
        itemPickupPoint.setMerchantSku(item.getMerchantSku());
        OfflineItemChangeResponse offlineItemChanged =
            this.checkOfflineItemChangedAndSetNewDataFlag(mandatoryRequestParam, upsertOfflineItemRequest,
                itemPickupPoint, offlineItemIdToPureCNCStatusChangeMap, auditTrailResponseList, merchantCode, item);
        if (Objects.nonNull(itemPickupPoint.getNewData()) && itemPickupPoint.getNewData()) {
          upsertOfflineItemPriceResponseVo.setNew(true);
          if (itemPickupPoint.isCncActive()) {
            itemSkusWithNewCncTrueL5.add(itemPickupPoint.getItemSku());
          }
        }
        if (offlineItemChanged.isPriceChanged()) {
          if (!itemService.validatePriceForDeliveryTrueItemPickupPoint(mandatoryRequestParam.getStoreId(), itemPickupPoint,
                  new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
                      upsertOfflineItemRequest.getListPrice(), upsertOfflineItemPriceResponseVo.getOfferPrice()))) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.FAILED_TO_UPDATE_OFFLINE_ITEM_PRICE);
            }
        }

        if (offlineItemChanged.isCncChanged()) {
          auditTrailResponseList.add(new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.CNC_FLAG_UPDATE,
              Boolean.toString(!itemPickupPoint.isCncActive()), Boolean.toString(itemPickupPoint.isCncActive()), null,
              itemPickupPoint.getProductSku(), item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false));
          if (!itemPickupPoint.isCncActive()) {
            int existingL5MadeCncFalseCount =
                itemSkuToCountOfExistingCncMadeFalse.getOrDefault(itemPickupPoint.getItemSku(), 0);
            itemSkuToCountOfExistingCncMadeFalse.put(itemPickupPoint.getItemSku(), existingL5MadeCncFalseCount + 1);
            itemSkuAndProductSkuMap.put(itemPickupPoint.getItemSku(), itemPickupPoint.getProductSku());
          }
        }

        ItemViewConfig itemViewConfig =
            itemPickupPoint.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig());

        if (item.isFreeSample() && (itemPickupPoint.isCncActive() || itemViewConfig.isDiscoverable() || itemViewConfig.isBuyable())) {
          upsertOfflineItemPriceResponseVo.setSuccess(Boolean.FALSE);
          upsertOfflineItemPriceResponseVo.setErrorMessage(
              ErrorMessages.FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE_OR_CNC);
          responses.add(upsertOfflineItemPriceResponseVo);
          continue;
        }
        itemMap.put(upsertOfflineItemRequest.getItemSku(), item);
        if (itemsMap.containsKey(item.getItemSku())) {
          List<ItemPickupPoint> pickupPointList = itemsMap.get(item.getItemSku());
          pickupPointList.add(itemPickupPoint);
          itemsMap.put(item.getItemSku(), pickupPointList);
        } else {
          List<ItemPickupPoint> pickupPointList = new ArrayList<>();
          pickupPointList.add(itemPickupPoint);
          itemsMap.put(item.getItemSku(), pickupPointList);
        }
        Set<String> pickupPointSet = productSkuAndPickupPointCodeMap.getOrDefault(itemPickupPoint.getProductSku(), new HashSet<>());
        pickupPointSet.add(itemPickupPoint.getPickupPointCode());
        productSkuAndPickupPointCodeMap.put(itemPickupPoint.getProductSku(), pickupPointSet);
        upsertOfflineItemPriceResponseVo.setProductSku(item.getProductSku());
        upsertOfflineItemPriceResponseVo.setItemName(item.getGeneratedItemName());
        upsertOfflineItemPriceResponseVo.setItemCode(item.getItemCode());
        upsertOfflineItemPriceResponseVo.setSuccess(Boolean.TRUE);
        upsertOfflineItemPriceResponseVo.setFbbActivated(itemPickupPoint.isFbbActivated());
        auditTrailResponseListUpdated.addAll(auditTrailResponseList);
        responses.add(upsertOfflineItemPriceResponseVo);
        ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
            objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint,
                offlineItemIdToPureCNCStatusChangeMap.get(itemPickupPoint.getOfflineItemId()));
        List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
            itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
        if (offlineItemChanged.isPriceChanged()) {
          itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
        }
        if (offlineItemChanged.isDiscoverableChanged()) {
          itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
        }

        if (cncForWarehouseFeatureSwitch && offlineItemChanged.isCncDiscoverableChanged()) {
          itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
        }

        itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
      } catch (Exception e) {
        LOG.error("Error on updateOfflineItemPrice with merchantCode: {}, upsertOfflineItemRequest: {} ", merchantCode,
            upsertOfflineItemRequest, e);
        upsertOfflineItemPriceResponseVo.setSuccess(Boolean.FALSE);
        upsertOfflineItemPriceResponseVo.setErrorMessage(e.getMessage());
        responses.add(upsertOfflineItemPriceResponseVo);
      }
    }
    getExistingL4MadeCncFalse(mandatoryRequestParam, itemSkuToCountOfExistingCncMadeFalse, existingL4MadeCncFalse,
        itemSkusWithNewCncTrueL5, itemSkuAndProductSkuMap, l3MadeCncFalse);

    if (MapUtils.isNotEmpty(itemsMap)) {
      updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(mandatoryRequestParam, username, itemsMap, itemMap,
          itemPickupPointDataChangeEventModelList, auditTrailResponseListUpdated, productSkuAndPickupPointCodeMap,
          existingL4MadeCncFalse, l3MadeCncFalse);
    }
    return responses;
  }

  private void getExistingL4MadeCncFalse(MandatoryRequestParam mandatoryRequestParam,
      Map<String, Integer> itemSkuToCountOfExistingCncMadeFalse, List<String> existingL4MadeCncFalse,
      Set<String> itemSkusWithNewCncTrueL5, Map<String, String> itemSkuAndProductSkuMap, List<String> l3MadeCncFalse) {
    Set<String> productSkuCncCheck = new HashSet<>();
    Map<String, Integer> productSkuSkuToCountOfExistingCncMadeFalse = new HashMap<>();
    for (Map.Entry<String, Integer> entry : itemSkuToCountOfExistingCncMadeFalse.entrySet()) {
      if (!itemSkusWithNewCncTrueL5.contains(entry.getKey())) {
        Long countOfActiveCncTrueL5 =
            itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(
                mandatoryRequestParam.getStoreId(), entry.getKey(), true);
        if (entry.getValue() >= countOfActiveCncTrueL5) {
          existingL4MadeCncFalse.add(entry.getKey());
          productSkuCncCheck.add(itemSkuAndProductSkuMap.get(entry.getKey()));
          int existingL4MadeCncFalseCount =
              productSkuSkuToCountOfExistingCncMadeFalse.getOrDefault(itemSkuAndProductSkuMap.get(entry.getKey()), 0);
          productSkuSkuToCountOfExistingCncMadeFalse.put(itemSkuAndProductSkuMap.get(entry.getKey()),
              existingL4MadeCncFalseCount + 1);
        }
      }
    }
    for (String productSku : productSkuCncCheck) {
      Long countOfActiveCncTrueL4 = itemService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(
          mandatoryRequestParam.getStoreId(), productSku, true);
      if (productSkuSkuToCountOfExistingCncMadeFalse.get(productSku) >= countOfActiveCncTrueL4) {
        l3MadeCncFalse.add(productSku);
      }
    }
  }

  private void updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      String username, Map<String, List<ItemPickupPoint>> pickupPointItemMap, Map<String, Item> itemMap,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
      List<AuditTrailDto> auditTrailResponseList, Map<String, Set<String>> productSkuAndPickupPointCodeMap,
      List<String> existingL4MadeCncFalse, List<String> l3MadeCncFalse) {
    Set<Item> itemsToReindex = new HashSet<>();
    List<Product> updatedProductList = saveOperationSolrService.updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(
        mandatoryRequestParam.getStoreId(), mandatoryRequestParam.getClientId(), pickupPointItemMap, itemMap,
        itemsToReindex, itemPickupPointDataChangeEventModelList, productSkuAndPickupPointCodeMap,existingL4MadeCncFalse,l3MadeCncFalse);
    if (CollectionUtils.isNotEmpty(auditTrailResponseList)) {
      AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse(auditTrailResponseList,
          Constants.DEFAULT, mandatoryRequestParam.getUsername(), mandatoryRequestParam.getClientId(),
          mandatoryRequestParam.getRequestId(), true);
      log.info("Publishing the event {} to update the AuditLogs", ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY);
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
    }
  }

  public void updatePickupPointCodesAtL3(Map<String, Set<String>> productSkuAndPickupPointCodeMap,
      Map<String, Product> productMap) {
    for (String productSku : productSkuAndPickupPointCodeMap.keySet()) {
      if (productMap.containsKey(productSku)) {
        Product product = productMap.get(productSku);
        Set<String> pickupPointCodes = productSkuAndPickupPointCodeMap.get(productSku);
        if (pickupPointCodes.stream().filter(pickupPointCode -> !product.getPickupPointCodes().contains(pickupPointCode))
            .count() > 0) {
          product.getPickupPointCodes().addAll(pickupPointCodes);
          productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
        }
      }
    }
  }

  private Item getItemByItemSku(MandatoryRequestParam mandatoryRequestParam, String itemSku) {
    Item item = this.cacheItemHelperService
        .findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemSku);
    checkArgument(Objects.nonNull(item) && !item.isMarkForDelete(), ITEM_NOT_FOUND);
    checkArgument(!item.isArchived(), ITEM_IS_ALREADY_ARCHIVED);
    return item;
  }

  private void isOfflineItemPriceValid(Double listPrice,
      Double offerPrice, Double minimumPrice) {
    checkArgument(Objects.nonNull(offerPrice), OfflineItemServiceImpl.OFFER_PRICE_MUST_NOT_BE_NULL);
    checkArgument(is(listPrice, GTE, minimumPrice),
        LIST_PRICE_MUST_BE_GREATER_THAN_OR_EQ_TO + minimumPrice);
    checkArgument(is(offerPrice, GTE, minimumPrice),
        OFFER_PRICE_MUST_BE_GREATER_THAN_OR_EQ_TO + minimumPrice);
    checkArgument(is(listPrice, GTE, offerPrice),
        LIST_PRICE_MUST_BE_GREATER_THAN_OR_EQ_TO + offerPrice);
  }

  private OfflineItemChangeResponse checkOfflineItemChangedAndSetNewDataFlag(MandatoryRequestParam mandatoryRequestParam,
      UpsertOfflineItemRequest upsertOfflineItemRequest, ItemPickupPoint itemPickupPoint,
      Map<String, Boolean> offlineItemIdToPureCNCStatusChangeMap, List<AuditTrailDto> auditTrailDtoList,
      String merchantCode, Item item) {
    String clientId = mandatoryRequestParam.getClientId();
    String requestId = mandatoryRequestParam.getRequestId();
    OfflineItemChangeResponse offlineItemChangeResponse = new OfflineItemChangeResponse();
    if (Boolean.TRUE.equals(itemPickupPoint.getNewData())) {
      Price price = new Price();
      price.setListPrice(upsertOfflineItemRequest.getListPrice());
      price.setOfferPrice(upsertOfflineItemRequest.getOfferPrice());
      price.setChannel(channelService.getDefaultChannel());
      itemPickupPoint.setNewData(true);
      itemPickupPoint.setUpdatedBy(mandatoryRequestParam.getUsername());
      itemPickupPoint.setOfflineItemHistoryDetail(
          CommonUtil.constructOfflineItemHistoryDetail(new UpsertOfflineItemRequest(), false, clientId, requestId));
      itemPickupPoint.setItemSku(upsertOfflineItemRequest.getItemSku());
      itemPickupPoint.getPrice().add(price);
      itemPickupPoint.setPickupPointCode(upsertOfflineItemRequest.getPickupPointCode());
      itemPickupPoint.setDelivery(false);
      itemPickupPoint.setOfflineItemId(CommonUtil.generateOfflineItemId(upsertOfflineItemRequest.getItemSku(),
          upsertOfflineItemRequest.getPickupPointCode()));
      itemPickupPoint.setStoreId(mandatoryRequestParam.getStoreId());
      Set<ItemViewConfig> itemViewConfigSet = itemPickupPoint.getItemViewConfig();
      itemViewConfigSet.add(new ItemViewConfig(upsertOfflineItemRequest.isBuyable(),
        upsertOfflineItemRequest.isDiscoverable(), channelService.getDefaultChannel(), null, null));
      itemViewConfigSet.add(new ItemViewConfig(upsertOfflineItemRequest.isBuyable(),
          upsertOfflineItemRequest.isDiscoverable(), channelService.getCncChannel(), null, null));
      itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      itemPickupPoint.setCncActive(upsertOfflineItemRequest.isCncActive());
      if (!cncForWarehouseFeatureSwitch) {
        offlineItemIdToPureCNCStatusChangeMap.putIfAbsent(itemPickupPoint.getOfflineItemId(), false);
      }
      auditTrailDtoList.add(new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.PICKUP_POINT_UPSERT,
          StringUtils.EMPTY, itemPickupPoint.getPickupPointCode(), null, itemPickupPoint.getProductSku(),
          item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false));
      return OfflineItemChangeResponse.builder().isNew(true).build();
    } else {
      Price price = itemPickupPoint.getPrice().stream().findFirst().orElseGet(() -> new Price());
      boolean listPriceUpdate = Double.compare(price.getListPrice(), upsertOfflineItemRequest.getListPrice()) != 0;
      boolean offerPriceUpdate = Double.compare(price.getOfferPrice(), upsertOfflineItemRequest.getOfferPrice()) != 0;
      if (listPriceUpdate || offerPriceUpdate) {
        itemPickupPoint.setNewData(false);
        itemPickupPoint.setUpdatedBy(mandatoryRequestParam.getUsername());
        if (channelService.getDefaultChannel().equals(price.getChannel())) {
          itemPickupPoint.setPriceUpdatedDate(new Date());
        }
        itemPickupPoint.setOfflineItemHistoryDetail(CommonUtil.constructOfflineItemHistoryDetail(
            new UpsertOfflineItemRequest(itemPickupPoint.getOfflineItemId(), itemPickupPoint.getItemSku(),
                itemPickupPoint.getPickupPointCode(), price.getListPrice(), price.getOfferPrice()), false, clientId,
            requestId));
        if (listPriceUpdate) {
          auditTrailDtoList.add(
              new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.NORMAL_PRICE_UPDATE,
                  Double.toString(price.getListPrice()), Double.toString(upsertOfflineItemRequest.getListPrice()), null,
                  itemPickupPoint.getProductSku(), item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(),
                  false));
        }
        if (offerPriceUpdate) {
          auditTrailDtoList.add(
              new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.SELLING_PRICE_UPDATE,
                  Double.toString(price.getOfferPrice()), Double.toString(upsertOfflineItemRequest.getOfferPrice()),
                  null, itemPickupPoint.getProductSku(), item.getGeneratedItemName(),
                  itemPickupPoint.getPickupPointCode(), false));
        }
        itemPickupPoint.setOfflineItemId(
            CommonUtil.generateOfflineItemId(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()));
        price.setListPrice(upsertOfflineItemRequest.getListPrice());
        price.setOfferPrice(upsertOfflineItemRequest.getOfferPrice());
        itemPickupPoint.getPrice().add(price);
        offlineItemChangeResponse.setPriceChanged(true);
      }
      if (itemPickupPoint.isMarkForDelete()) {
        offlineItemChangeResponse.setNew(true);
        itemPickupPoint.setNewData(true);
        offlineItemChangeResponse.setIfMFDChanged(true);
        itemPickupPoint.setMarkForDelete(false);
        itemPickupPoint.setDelivery(false);
        auditTrailDtoList.add(
            new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.PICKUP_POINT_UPSERT,
                StringUtils.EMPTY, itemPickupPoint.getPickupPointCode(), null, itemPickupPoint.getProductSku(),
                item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false));
      }
      if (!cncForWarehouseFeatureSwitch) {
        offlineItemIdToPureCNCStatusChangeMap.putIfAbsent(itemPickupPoint.getOfflineItemId(),
            CommonUtil.isPureCNCStatusChange(
                ItemViewConfig.builder().isBuyable(upsertOfflineItemRequest.isBuyable())
                    .isDiscoverable(upsertOfflineItemRequest.isDiscoverable()).build(),
                itemPickupPoint.getItemViewConfig().iterator().next(),
                upsertOfflineItemRequest.isCncActive(), itemPickupPoint.isCncActive()));
      }

      ItemViewConfig itemViewConfig =
          itemPickupPoint.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig());
      ItemViewConfig itemViewConfigCnc =
          itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.CNC);
        boolean buyableFlagUpdate = itemViewConfig.isBuyable() != upsertOfflineItemRequest.isBuyable();
        boolean discoverableFlagUpdate = itemViewConfig.isDiscoverable() != upsertOfflineItemRequest.isDiscoverable();
        boolean cncDiscoverableFlagUpdate = itemViewConfigCnc.isDiscoverable() != upsertOfflineItemRequest.isDiscoverable();
      if (buyableFlagUpdate || discoverableFlagUpdate) {
          if (buyableFlagUpdate) {
            auditTrailDtoList.add(
                new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.BUYABLE_FALG_UPDATE,
                    Boolean.toString(itemViewConfig.isBuyable()),
                    Boolean.toString(upsertOfflineItemRequest.isBuyable()), null, itemPickupPoint.getProductSku(),
                    item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false));
          }
          if (discoverableFlagUpdate) {
            offlineItemChangeResponse.setDiscoverableChanged(true);
            auditTrailDtoList.add(
                new AuditTrailDto(merchantCode, itemPickupPoint.getItemSku(), Constants.DISPLAYABLE_FLAG_UPDATE,
                    Boolean.toString(itemViewConfig.isDiscoverable()),
                    Boolean.toString(upsertOfflineItemRequest.isDiscoverable()), null, itemPickupPoint.getProductSku(),
                    item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode(), false));
          }

        if (cncDiscoverableFlagUpdate) {
          offlineItemChangeResponse.setCncDiscoverableChanged(true);
        }
          offlineItemChangeResponse.setViewConfigChanged(true);
          itemViewConfig.setDiscoverable(upsertOfflineItemRequest.isDiscoverable());
          itemViewConfig.setBuyable(upsertOfflineItemRequest.isBuyable());
          HashSet<ItemViewConfig> viewConfigHashSet = new HashSet();
          viewConfigHashSet.add(itemViewConfig);
          itemPickupPoint.setItemViewConfig(viewConfigHashSet);
        }
      if (itemPickupPoint.isCncActive() != upsertOfflineItemRequest.isCncActive()) {
        offlineItemChangeResponse.setCncChanged(true);
        itemPickupPoint.setCncActive(upsertOfflineItemRequest.isCncActive());
      }
    }
    return offlineItemChangeResponse;
  }

  private void validateDeleteRequest(String storeId, String merchantCode, List<OfflineItem> offlineItems) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(offlineItems), LIST_OFFLINE_ITEM_MUST_NOT_BE_EMPTY);

    for (OfflineItem offlineItem : offlineItems) {
      checkArgument(StringUtils.isNotBlank(offlineItem.getItemSku()), ITEM_SKU_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(offlineItem.getPickupPointCode()), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
  }

  @Override
  public List<OfflineItem> findByMerchantCodeAndItemSku(String storeId, String merchantCode,
      String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);
    List<OfflineItem> offlineItems = offlineItemRepository
        .findByStoreIdAndMerchantCodeAndItemSku(storeId, merchantCode, itemSku);
    offlineItems.forEach(offlineItem -> offlineItem.setListPrice(validateAndGetListPrice(offlineItem)));
    return offlineItems;
  }

  @Override
  public OfflineItem findByOfflineItemIdAndMarkForDeleteFalse(String storeId, String offlineItemId) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(offlineItemId), OFFLINE_ITEM_ID_NOT_BE_BLANK);
    return offlineItemRepository.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, offlineItemId);
  }

  @Override
  public List<OfflineItem> findByOfflineItemIds(String storeId,
      List<String> offlineItemIdList) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(offlineItemIdList), OFFLINE_ITEM_ID_NOT_BE_BLANK);
    return offlineItemRepository
        .findByStoreIdAndOfflineItemIdIn(storeId, offlineItemIdList);
  }

  @Override
  public void deleteOfflineItemByMerchantCode(String storeId, String merchantCode, String username) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    try {
      List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
      if (cncForWarehouseFeatureSwitch) {
        itemPickupPointList =
            this.itemPickupPointService.findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
                storeId, merchantCode, true, false, channelService.getCncChannel());
      } else {
        itemPickupPointList =
            this.itemPickupPointService.findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(
                storeId, merchantCode, Boolean.TRUE, Boolean.FALSE);
      }
        Set<String> itemSkuSet = new HashSet<>();
        Set<String> productSkuSet = new HashSet<>();
        itemPickupPointList.stream().forEach(itemPickupPoint -> {
          if (!cncForWarehouseFeatureSwitch) {
            itemPickupPoint.setCncActive(Boolean.FALSE);
          }
          ItemViewConfig cncViewConfig =
              itemPickupPoint.getSingleItemViewConfigByChannel(channelService.getCncChannel());
          if (Objects.nonNull(cncViewConfig)) {
            cncViewConfig.setDiscoverable(Boolean.FALSE);
            cncViewConfig.setBuyable(Boolean.FALSE);
          }
          itemSkuSet.add(itemPickupPoint.getItemSku());
          productSkuSet.add(itemPickupPoint.getProductSku());
        });
        for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
          ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
              objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, true);
          itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
            itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
        }
        LOG.info(
            "#deleteOfflineItemByMerchantCode with multiPickupPointEnabled true. merchantCode : {} , itemPickupPointList : {} ",
            merchantCode, itemPickupPointList);
        this.itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
        this.itemService.updateCncActivatedByItemSkusAndPublish(storeId, itemSkuSet, Boolean.FALSE, username);
        this.productService.updateCncActivatedByProductSkuAndSolrL3(storeId, productSkuSet, Boolean.FALSE, username);
    } catch (Exception e) {
      LOG.error("Error on delete offline item with merchantCode: {}", merchantCode, e);
    }
  }

  @Override
  public void deleteByPickupPointCode(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    try {
      List<OfflineItem> offlineItems = offlineItemRepository.findByStoreIdAndPickupPointCode(storeId, pickupPointCode);
      offlineItems.forEach(offlineItem -> offlineItem.setMarkForDelete(Boolean.TRUE));
      offlineItemRepository.deleteByStoreIdAndPickupPointCode(storeId, pickupPointCode);
      saveAndPublishService.publishListOfOfflineItems(offlineItems, null);
    } catch (Exception e) {
      LOG.error("Error on findAndRemoveByPickupPointCode with , pickupPointCode: {}", pickupPointCode, e);
    }
  }

  @Override
  public void deleteByPickupPointCode_ItemPickupPoint(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    try {
      List<ItemPickupPoint> itemPickupPointList = itemPickupPointService
          .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(storeId, pickupPointCode, true, false);
      List<ItemPickupPoint> deletedItemPickupPoints = new ArrayList<>();
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (!itemPickupPoint.isDelivery()) {
          itemPickupPoint.setMarkForDelete(Boolean.TRUE);
          deletedItemPickupPoints.add(itemPickupPoint);
        }
        itemPickupPoint.setCncActive(Boolean.FALSE);
      }
      LOG.info("#deleteByPickupPointCode_ItemPickupPoint pickupPointCode : {} , itemPickupPointList : {} ",
          pickupPointCode, itemPickupPointList);
      itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
      if (CollectionUtils.isNotEmpty(deletedItemPickupPoints)) {
        List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request =
            CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(deletedItemPickupPoints);
        inventoryOutbound.deleteByItemSkuAndPickupPointCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            request);
      }
      saveAndPublishService.publishListOfItemsPickupPoints(itemPickupPointList, null);
    } catch (Exception e) {
      LOG.error("Error on findAndRemoveByPickupPointCode with , pickupPointCode : {} ", pickupPointCode, e);
    }
  }

  @Override
  public void validateAndUpdateProductItemCncActivatedFalse(String storeId, List<OfflineItem> offlineItems,
      String username, String solrUsername) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);

    if (!CollectionUtils.isEmpty(offlineItems)) {
      Set<String> itemSkusToReindex = new HashSet<>();
      log.info("#validateAndUpdateProductItemCncActivatedFalse offlineItems : {}", offlineItems);
      for (OfflineItem offlineItem : offlineItems) {
        ItemPickupPoint itemPickupPoint;
        if (cncForWarehouseFeatureSwitch) {
          itemPickupPoint =
              itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
                  storeId, offlineItem.getItemSku(),
                  ImmutableSet.of(offlineItem.getPickupPointCode()), true);
        } else {
          itemPickupPoint =
              itemPickupPointService.findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(
                  storeId, offlineItem.getItemSku(), true,
                  ImmutableSet.of(offlineItem.getPickupPointCode()));
        }
        log.info("#validateAndUpdateProductItemCncActivatedFalse offlineItemFromDb : {}", itemPickupPoint);
        if (Objects.isNull(itemPickupPoint)) {
          itemService.updateCncActivated(storeId, offlineItem.getItemSku(), Boolean.FALSE, username);
        }
        itemSkusToReindex.add(offlineItem.getItemSku());
      }
    }
  }

  @Override
  public void validateAndUpdateProductCncActivatedFalse(String storeId, List<ItemPickupPoint> itemPickupPoints,
      String username, String solrUsername) {
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Set<String> productSkusToReindex = new HashSet<>();
      log.info("#validateAndUpdateProductCncActivatedFalse itemPickupPoints : {}", itemPickupPoints);
      Set<String> uniqueProductSkus =
          itemPickupPoints.stream().map(ItemPickupPoint::getProductSku).collect(Collectors.toSet());
      for (String productSku : uniqueProductSkus) {
        ItemPickupPoint cncTrueItemPickupPointByProduct;
        if (cncForWarehouseFeatureSwitch) {
          cncTrueItemPickupPointByProduct =
              itemPickupPointService.findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
                  storeId, productSku, ImmutableSet.of(
                      itemPickupPoints.stream().findFirst().map(ItemPickupPoint::getPickupPointCode)
                          .orElse(StringUtils.EMPTY)), true);
        } else {
          cncTrueItemPickupPointByProduct =
              itemPickupPointService.findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(
                  storeId, productSku, true, ImmutableSet.of(
                      itemPickupPoints.stream().findFirst().map(ItemPickupPoint::getPickupPointCode)
                          .orElse(StringUtils.EMPTY)));
        }
        log.info("#validateAndUpdateProductItemCncActivatedFalse offlineItemFromDb : {}", productSku);
        if (Objects.isNull(cncTrueItemPickupPointByProduct)) {
          productSkusToReindex.add(productSku);
        }
      }
      if (CollectionUtils.isNotEmpty(productSkusToReindex)) {
        productService.updateCncActivatedByProductSkuAndSolrL3(storeId, productSkusToReindex, false, username);
      }
    }
  }

  @Override
  public void updateProductItemCncActivatedTrue(String storeId, String pickupPointCode, String username,
      List<OfflineItem> offlineItems) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    log.info("#updateProductItemCncActivatedTrue offlineItems : {}", offlineItems);
    if (!CollectionUtils.isEmpty(offlineItems)) {
      Set<String> itemSkusToReindex = new HashSet<>();
      for (OfflineItem offlineItem : offlineItems) {
        itemService.updateCncActivated(storeId, offlineItem.getItemSku(), Boolean.TRUE, username);
        itemSkusToReindex.add(offlineItem.getItemSku());
      }
      reindexItemCncActivated(storeId, itemSkusToReindex, null, offlineItems, false, Constants.PICKUP_POINT_CHANGE);
    }
  }

  @Override
  public List<OfflineItem> findByMerchantCodeAndMerchantSku(
      String storeId, String merchantCode, String merchantSku) {

    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantSku), MERCHANT_SKU_MUST_NOT_BE_BLANK);

    return offlineItemRepository.findByStoreIdAndMerchantCodeAndMerchantSku(storeId, merchantCode, merchantSku);
  }

  @Override
  public List<ProductAndItemsVO> findOfflineItemsByProductSkus(
      String storeId, String username, String requestId, List<String> productSkus) throws Exception {

    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();

    for (String productSku : productSkus) {
      ProductAndItemsVO productAndItemsVO = null;
      try {
        productAndItemsVO = getProductAndItemsVOByProductSku(storeId, username, requestId, productSku);
      } catch (Exception e) {
        LOG.error("error when get product and item, with productSku : {}, error : {}", productSku, e.getMessage());
      }
      if (productAndItemsVO != null) {
        productAndItemsVOs.add(productAndItemsVO);
      }
    }

    if (productAndItemsVOs.size() == 0) {
      throw new RuntimeException(PRODUCT_OR_ITEM_NOT_FOUND);
    }

    return productAndItemsVOs;
  }

  private ProductAndItemsVO getProductAndItemsVOByProductSku(
      String storeId, String username, String requestId, String productSku) throws Exception {

    try {
      Product product = productService.getProduct(storeId, productSku);
      if (product == null) {
        throw new RuntimeException(PRODUCT_NOT_FOUND);
      }

      List<Item> items = getOfflineItems(storeId, username, requestId, productSku);
      if (isEmpty(items)) {
        throw new RuntimeException(ITEM_NOT_FOUND);
      }

      ProductAndItemsVO productAndItemsVO =
          this.masterDataConstructorService.constructProductAndItemWithMasterData(storeId, username,
              requestId, product, items, isProductVisibilityEnabled, false);
      this.productSearchHelper.setItemCatalogs(storeId, username, requestId, true,
          Collections.singletonList(productAndItemsVO), new HashMap<>());

      return productAndItemsVO;
    } catch (Exception e) {
      throw e;
    }
  }

  private List<Item> getOfflineItems(String storeId, String username, String requestId, String productSku) {
    List<Item> items = itemService.getItemsByProductSkuAndCncActivated(storeId, username, requestId, productSku, true);

    if (isNotEmpty(items)) {
      Iterator<Item> itemIterator = items.iterator();
      Set<String> itemSkus = items.stream().map(Item::getItemSku).collect(Collectors.toSet());
      List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
          storeId, itemSkus, true);

      Map<String, List<ItemPickupPoint>> mapOfItemSkuAndItemPickupPoints = new HashMap<>();
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (CollectionUtils.isEmpty(
          mapOfItemSkuAndItemPickupPoints.get(itemPickupPoint.getItemSku()))) {
          mapOfItemSkuAndItemPickupPoints.put(itemPickupPoint.getItemSku(), new ArrayList<>());
        }
        mapOfItemSkuAndItemPickupPoints.get(itemPickupPoint.getItemSku()).add(itemPickupPoint);
      }

      while (itemIterator.hasNext()) {
        Item item = itemIterator.next();

        List<ItemPickupPoint> itemPickupPointsByItemSku = mapOfItemSkuAndItemPickupPoints.get(item.getItemSku());

        if (isEmpty(itemPickupPointsByItemSku)) {
          itemIterator.remove();
        } else {
          for (ItemViewConfig itemViewConfig : item.getItemViewConfigs()) {
            itemViewConfig.setBuyable(true);
          }

          ItemPickupPoint cheapestItemPickupPoint = itemPickupPointsByItemSku.stream()
              .min(Comparator.comparing(itemPickupPoint -> itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice())).get();

          if (isEmpty(item.getPrice())) {
            item.setPrice(getDefaultOfflinePrice(cheapestItemPickupPoint));
          } else {
            item.getPrice().forEach(price -> {
              price.setLastUpdatedBy(cheapestItemPickupPoint.getUpdatedBy());
              price.setLastUpdatedDate(cheapestItemPickupPoint.getUpdatedDate());
              price.setListPrice(validateAndGetListPrice(
                cheapestItemPickupPoint.getPrice().stream().findFirst().get()));
              price.setOfferPrice(
                cheapestItemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
            });
          }

          item.setUniqueId(cheapestItemPickupPoint.getOfflineItemId());
        }
      }
    }
    return items;
  }

  private Set<Price> getDefaultOfflinePrice(ItemPickupPoint itemPickupPoint) {
    Set<Price> prices = new HashSet<>();
    Price price = new Price();
    price.setLastUpdatedBy(
      itemPickupPoint.getPrice().stream().findFirst().get().getLastUpdatedBy());
    price.setLastUpdatedDate(
      itemPickupPoint.getPrice().stream().findFirst().get().getLastUpdatedDate());
    price.setListPrice(
      validateAndGetListPrice(itemPickupPoint.getPrice().stream().findFirst().get()));
    price.setOfferPrice(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
    price.setChannel(DEFAULT);
    prices.add(price);
    return prices;
  }

  public List<OfflineItem> findByItemSku(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);

    return offlineItemRepository.findByStoreIdAndItemSku(storeId, itemSku);
  }

  private List<OfflineItem> findByItemSkuIn(String storeId, List<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ITEM_SKUS_MUST_NOT_BE_EMPTY);

    return offlineItemRepository.findByStoreIdAndItemSkuIn(storeId, itemSkus);
  }

  private Page<OfflineItem> findByMerchantCodesIn(String storeId, List<String> merchantCodes,
      Pageable pageable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(merchantCodes), MERCHANT_CODES_MUST_NOT_BE_BLANK);

    return offlineItemRepository.findByStoreIdAndMerchantCodeIn(storeId, merchantCodes, pageable);
  }

  private Page<OfflineItem> findByStoreId(String storeId, Pageable pageable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pageable != null, PAGEABLE_MUST_NOT_BE_NULL);

    return offlineItemRepository.findByStoreId(storeId, pageable);
  }

  @Override
  public List<OfflineItem> findByPickupPointCode(String storeId, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);

    return offlineItemRepository.findByStoreIdAndPickupPointCode(storeId, pickupPointCode);
  }

  @Override
  public OfflineItem findByItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    OfflineItem offlineItem = offlineItemRepository
        .findByStoreIdAndItemSkuAndPickupPointCode(storeId, itemSku, pickupPointCode);
    if (Objects.nonNull(offlineItem)) {
      offlineItem.setListPrice(validateAndGetListPrice(offlineItem));
    }
    return offlineItem;
  }

  private Double validateAndGetListPrice(OfflineItem offlineItem) {
    return Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getOfferPrice());
  }

  private Double validateAndGetListPrice(Price price) {
    return Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice());
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
          if (cncActivated != null) {
            item.setCncActivated(cncActivated);
          }
          if (enableDeferredSolrReindex && CommonUtil.checkIfUsernameIsExcluded(username, excludedUserNames)) {
            deferredReindexItems.add(item);
          } else {
            if (isPriceUpdate) {
              if (CollectionUtils.isNotEmpty(updatedOfflineItems)) {
                productAndItemSolrIndexerService.offlineItemPriceAtomicUpdate(itemSku, updatedOfflineItems);
              }
            } else {
              productAndItemSolrIndexerService.applyItem(item);
              reindexService.reindexPendingL3SolrAndDatabase(
                Collections.singletonList(item.getProductSku()), new HashMap<>());
            }
          }
        } else {
          LOG.warn("reindexItemCncActivated : Item Not found : {}", itemSku);
        }
      } catch (Exception e) {
        LOG.error("Error on reindex item after update offline item with itemSku: {}", itemSku, e);
      }
    }
    if (CollectionUtils.isNotEmpty(deferredReindexItems)) {
      saveOperationSolrService.deferredReindexItems(storeId, deferredReindexItems, true,
          ReindexType.OFFLINE_ITEM_REINDEX, true);
    }
  }

  private void reindexItemsCncActivated(String storeId, Set<Item> items, String username,
      boolean updateL3PickupPointCodes, List<Product> updatedProductList) {
    boolean enableDeferredSolrReindex = Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX).getValue());
    List<Item> defferedReindexItems = new ArrayList<>();
    Map<String, Product> productSkuAndProductMap =
        updatedProductList.stream().collect(Collectors.toMap(Product::getProductSku, product -> product, (a, b) -> a));
    for (Item item : items) {
      try {
        if (enableDeferredSolrReindex && CommonUtil.checkIfUsernameIsExcluded(username, excludedUserNames)) {
          defferedReindexItems.add(item);
        } else {
          productAndItemSolrIndexerService.applyItem(item);
          reindexService.reindexPendingL3SolrAndDatabase(Collections.singletonList(item.getProductSku()),
              productSkuAndProductMap);
        }
      } catch (Exception e) {
        LOG.error("Error on reindex item after update offline item with itemSku: {}", item.getItemSku(), e);
      }
    }
    if (CollectionUtils.isNotEmpty(defferedReindexItems)) {
      saveOperationSolrService.deferredReindexItems(storeId, defferedReindexItems, true,
          ReindexType.OFFLINE_ITEM_REINDEX, updateL3PickupPointCodes);
    }
  }

  @Override
  public List<OfflineItem> findByItemSkusAndMarkForDeleteFalse(String storeId, Set<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ITEM_SKUS_MUST_NOT_BE_EMPTY);

    return offlineItemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus);
  }

  @Override
  public List<OfflineItem> findByItemSkusAndPickupPointCodeAndMarkForDeleteFalse(String storeId,
      Set<String> itemSkus, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ITEM_SKUS_MUST_NOT_BE_EMPTY);

    return offlineItemRepository
        .findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSkus, pickupPointCode);
  }

  @Override
  public List<OfflineItem> findByItemSkusAndMarkForDeleteFalseWithLimit(String storeId,
      Set<String> itemSkus) {
    Page<OfflineItem> result = offlineItemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(
        storeId, itemSkus, this.generatePageable(storeId));
    result.getContent().forEach(offlineItem -> offlineItem.setListPrice(validateAndGetListPrice(offlineItem)));
    return result.getContent();
  }

  @Override
  public List<OfflineItem> findByItemSkusAndMarkForDeleteFalseWithLimitWithoutPagination(String storeId,
      Set<String> itemSkus, int limit) {
    List<OfflineItem> result = offlineItemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndLimit(
        storeId, itemSkus, limit);
    result.forEach(offlineItem -> offlineItem.setListPrice(validateAndGetListPrice(offlineItem)));
    return result;
  }

  @Override
  public Page<OfflineItem> findByItemSkuAndMarkForDeleteFalse(String storeId, String itemSku, Pageable pageable) {
    return offlineItemRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, pageable);
  }

  private Pageable generatePageable(String storeId) {
    SystemParameter limitOfflineItems = this.systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    Pageable pageable = PageRequest.of(
        0, Integer.valueOf(limitOfflineItems.getValue()),
        Sort.by(Sort.Direction.ASC, ProductFieldNames.OFFER_PRICE));
    return pageable;
  }

  @Override
  public List<ItemPickupPointPriceVo> findMultipleMerchantsOfflineItemByItemSku(String storeId, String itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);

    Set<String> itemSkusFromMultipleMerchants = getItemSkusFromAnotherMerchant(storeId, itemSku);
    if (CollectionUtils.isEmpty(itemSkusFromMultipleMerchants)) {
      return new ArrayList<>();
    }
    return this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(storeId,
      itemSkusFromMultipleMerchants, generatePageable(storeId));
  }

  private Set<String> getItemSkusFromAnotherMerchant(String storeId, String itemSku) throws Exception {
    Item item = itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    if (item == null) {
      return new HashSet<>();
    }

    List<Item> itemsFromMultipleMerchants = new ArrayList<>();
    if (!item.isSynchronized()) {
      itemsFromMultipleMerchants.add(item);
    } else {
      String pristineId = getIfNotNull(item.getPristineDataItem(), PristineDataItem::getPristineId);
      boolean pristineIdDataLookupEnabled = true;
      if (StringUtils.isNotBlank(pristineId)) {
        SystemParameter systemParameter = systemParameterService.findValueByStoreIdAndVariable(
            storeId, SystemParameterNames.PRISTINE_ID_DATA_LOOKUP_ENABLED);
        pristineIdDataLookupEnabled = Boolean.valueOf(systemParameter.getValue());
      }

      if (StringUtils.isNotBlank(pristineId) && pristineIdDataLookupEnabled) {
        Set<String> pristineIds = new HashSet<>(Arrays.asList(pristineId));
        itemsFromMultipleMerchants =
            itemService.getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
                storeId, pristineIds);
      } else {
        itemsFromMultipleMerchants =
            itemService.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
                storeId, item.getItemCode());
      }
    }

    return itemsFromMultipleMerchants
        .stream()
        .map(_item -> _item.getItemSku())
        .collect(Collectors.toSet());
  }

  @Override
  @Cacheable(cacheManager = Constants.UNIQUE_ID_CHECK_CACHE_MANAGER, value = {
      CacheNames.UNIQUE_ID_TYPE_CHECK}, key = "#storeId + '-' + #uniqueId", unless = "#result == null")
  public boolean isOfflineItem(String storeId, String uniqueId) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(uniqueId), UNIQUE_ID_MUST_NOT_BE_BLANK);
    if(skuValidator.isItemSku(uniqueId)) {
      Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, uniqueId);
      checkArgument(item != null, UNIQUE_ID_NOT_FOUND);
    } else if (skuValidator.isItemSkuL4OrL5(uniqueId)) {
      int hyphenLastIndex = uniqueId.lastIndexOf(Constants.HYPHEN);
      return Objects.nonNull(this.itemPickupPointService.findByItemSkuAndPickupPointCode(
          storeId, uniqueId.substring(0, hyphenLastIndex), uniqueId.substring(hyphenLastIndex + 1)));
    }
    return false;
  }

  @Override
  public void publishOfflineItems(String storeId, int pageSize, List<String> itemSkus) throws Exception {

    if (CollectionUtils.isNotEmpty(itemSkus)) {
      this.publishOfflineItemsByItemSkus(storeId, itemSkus);
    } else {
      log.warn("Input itemSkus for offline item publish is empty, request : {}", itemSkus);
    }
  }

  private void publishOfflineItemsByItemSkus(String storeId, List<String> itemSkus) throws Exception {

    List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(
      this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
        storeId, new HashSet<>(itemSkus), true), false, null);

    this.saveAndPublishService.publishListOfOfflineItemsWithoutFailsafe(offlineItems);
  }

  @Override
  public void republishOfflineItemsByMerchantCodes(String storeId, int pageSize,
      List<String> merchantCodes) throws Exception {
    SystemParameter runningFlagParam =
        this.validateRepublishByMerchantCodesJob(storeId);
    try {
      this.updateRunningFlagParam(runningFlagParam, true);
      executorService.execute(()-> {
        try {
          this.findAndRepublishOfflineItems(storeId, pageSize, merchantCodes);
        } catch (Exception e) {
          LOG.error("Error while republish offline item with error : {}", e.getMessage());
        }
      });
      executorService.shutdown();
    } catch (Exception e) {
      LOG.error("failed to republish offline items in batch with batch size {}",
          pageSize, e);
    } finally {
      this.updateRunningFlagParam(runningFlagParam, false);
    }
  }

  private void publishAllOfflineItemsFromOffset(String storeId, int pageSize) throws Exception {

    SystemParameter runningFlagParam = this.validateJob(storeId);

    SystemParameter offsetParam = this.systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.PUBLISH_OFFLINE_ITEMS_OFFSET);

    int offset = Integer.parseInt(offsetParam.getValue());

    try {
      this.updateRunningFlagParam(runningFlagParam, true);

      offset = this.findAndPublishAllOfflineItems(storeId, offset, pageSize);

      this.updateOffsetPostPublish(offsetParam, offset);
    } catch (Exception e) {
      LOG.error("failed to publish offline items in batch with offset {}, batch size {}", offset, pageSize, e);
    } finally {
      this.updateRunningFlagParam(runningFlagParam, false);
    }
  }

  private void findAndRepublishOfflineItems(String storeId, int pageSize,
      List<String> merchantCodes) throws Exception {

    int page = 0;
    int totalPages = 0;

    do {
      Page<OfflineItem> republishedOfflineItems =
          this.findOfflineItemsByMerchantCodesAndPublish(storeId, page, pageSize, merchantCodes);

      LOG.info("republishedOfflineItems: {}", republishedOfflineItems);

      totalPages = republishedOfflineItems.getTotalPages();

      page++;
    } while (page < totalPages);

  }

  private int findAndPublishAllOfflineItems(String storeId, int offset, int pageSize) throws Exception {

    int page = (offset / pageSize);
    offset = page * pageSize;
    int totalPages = 0;

    do {
      Page<OfflineItem> publishedOfflineItems = this.findCncItemsAndPublish(storeId, page, pageSize);

      LOG.info("publishedOfflineItems: {}", publishedOfflineItems);

      totalPages = publishedOfflineItems.getTotalPages();

      offset += publishedOfflineItems.getContent().size();

      page++;
    } while (page < totalPages);

    return offset;
  }

  private Page<OfflineItem> findCncItemsAndPublish(String storeId, int page, int pageSize) {

    try {
      Pageable pageable = PageRequest.of(page, pageSize, Sort.by(Sort.Direction.ASC,
        ProductFieldNames.CREATED_DATE));

      Page<OfflineItem> offlineItems = this.findByStoreId(storeId, pageable);

      this.saveAndPublishService.publishListOfOfflineItemsWithoutFailsafe(offlineItems.getContent());

      return offlineItems;
    } catch (Exception e) {
      LOG.error("failed to find or publish cnc items for page: {}, pageSize: {}", page, pageSize);
      return new PageImpl<OfflineItem>(new ArrayList<>());
    }
  }

  private Page<OfflineItem> findOfflineItemsByMerchantCodesAndPublish(String storeId, int page,
      int pageSize, List<String> merchantCodes) throws Exception{

    try {
      Pageable pageable = PageRequest.of(page, pageSize,
          Sort.by(Sort.Direction.ASC, ProductFieldNames.CREATED_DATE));

      Page<OfflineItem> offlineItems = this.findByMerchantCodesIn(storeId, merchantCodes, pageable);

      this.saveAndPublishService
          .publishListOfOfflineItemsWithoutFailsafe(offlineItems.getContent());

      return offlineItems;
    } catch (Exception e) {
      LOG.error("failed to find or publish offline items for page: {}, pageSize: {}", page,
          pageSize);
      return new PageImpl<OfflineItem>(new ArrayList<>());
    }
  }

  private void updateRunningFlagParam(SystemParameter runningFlagParam, boolean newValue) {

    runningFlagParam.setValue(String.valueOf(newValue));
    systemParameterService.update(runningFlagParam);
  }

  private void updateOffsetPostPublish(SystemParameter offsetParam, int offset) {

    offsetParam.setValue(String.valueOf(offset));
    systemParameterService.update(offsetParam);
  }

  private SystemParameter validateJob(String storeId) throws Exception {

    SystemParameter systemParameterIsRunning = systemParameterService.findValueByStoreIdAndVariable(
        storeId, SystemParameterNames.PUBLISH_OFFLINE_ITEMS_RUNNING_STATE);
    boolean isRunning = Boolean.valueOf(systemParameterIsRunning.getValue());

    if (isRunning) {
      throw new Exception("publishOfflineItems is still running!");
    }

    return systemParameterIsRunning;
  }

  private SystemParameter validateRepublishByMerchantCodesJob(String storeId) throws Exception {

    SystemParameter systemParameterIsRunning = systemParameterService
        .findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE);
    boolean isRunning = Boolean.valueOf(systemParameterIsRunning.getValue());

    if (isRunning) {
      throw new Exception("republishOfflineItems is still running!");
    }

    return systemParameterIsRunning;
  }

  @Override
  public void updateOfflineItemPriceByItemSku(MandatoryRequestParam mandatoryRequestParam, String merchantCode,
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest) throws Exception {
    String storeId = mandatoryRequestParam.getStoreId();
    checkArgument(StringUtils.isNotBlank(storeId), OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);

    String itemSku = updateOfflineItemPriceRequest.getItemSku();
    checkArgument(StringUtils.isNotBlank(itemSku), OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);

    Double listPrice = updateOfflineItemPriceRequest.getListPrice();
    Double offerPrice = updateOfflineItemPriceRequest.getOfferPrice();
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();

    String minimumPriceSystemParameter = systemParameterService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.MINIMUM_PRICE).getValue();
    if (StringUtils.isBlank(minimumPriceSystemParameter)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, String
          .format("value of system parameter %s is blank", SystemParameterNames.MINIMUM_PRICE));
    }
    Double minimumPrice = Double.parseDouble(minimumPriceSystemParameter);
    this.isOfflineItemPriceValid(listPrice, offerPrice, minimumPrice);

    List<ItemPickupPoint> updatedItemPickupPoints =
        itemPickupPointService.updateItemPickupPointPriceByOfflineItem(storeId, merchantCode,
            updateOfflineItemPriceRequest);
      itemPickupPointDataChangeEventModelList = updatedItemPickupPoints.stream().map(
              itemPickupPoint -> objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false))
          .collect(Collectors.toList());
    List<Item> items = itemService.findByStoreIdAndItemSkus(storeId,
        updatedItemPickupPoints.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet()));
    Map<String, Item> itemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    saveAndPublishService.publishListOfOfflineItems(updatedItemPickupPoints, itemMap,
        mandatoryRequestParam.getClientId(), itemPickupPointDataChangeEventModelList);
    reindexItemsCncActivated(storeId, items.stream().collect(Collectors.toSet()),
      Constants.DEFAULT_USERNAME, false, new ArrayList<>());
  }

  @Override
  public List<ItemPickupPointPriceVo> findItemPickupPointByMerchantCodeAndItemSku(String storeId,
    String merchantCode, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_BLANK);
    return this.itemPickupPointService.findByStoreIdAndMerchantCodeAndItemSku(storeId,
      merchantCode, itemSku);
  }

  @Override
  public ItemPickupPointPriceVo findItemPickupPointByOfflineItemId(String storeId,
    String offlineItemId) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(offlineItemId),
      ErrorMessages.OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK);
    return this.itemPickupPointService.findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId,
      offlineItemId);
  }

  private List<OfflineItem> publishOfflineItems(Double listPrice, Double offerPrice,
      List<OfflineItem> existingOfflineItems, MandatoryRequestParam mandatoryRequestParam) {
    String clientId = mandatoryRequestParam.getClientId();
    String requestId = mandatoryRequestParam.getRequestId();
    String username = mandatoryRequestParam.getUsername();
    List<OfflineItem> updatedOfflineItems = existingOfflineItems.stream().filter(
        offlineItem -> Double.compare(offlineItem.getListPrice(), listPrice) != 0
            || Double.compare(offlineItem.getOfferPrice(), offerPrice) != 0).map(updatedOfflineItem -> {
      updatedOfflineItem.setNewData(false);
      updatedOfflineItem
          .setOfflineItemHistoryDetail(constructHistoryDetail(updatedOfflineItem, Boolean.TRUE, clientId, requestId));
      updatedOfflineItem.setListPrice(listPrice);
      updatedOfflineItem.setOfferPrice(offerPrice);
      updatedOfflineItem.setUpdatedBy(username);
      return updatedOfflineItem;
    }).collect(Collectors.toList());
    saveAndPublishService.publishListOfOfflineItems(updatedOfflineItems, mandatoryRequestParam);
    return updatedOfflineItems;
  }

  private OfflineItemHistoryDetailVO constructHistoryDetail(OfflineItem offlineItem,
      boolean syncPriceAction, String clientId, String requestId) {
    return OfflineItemHistoryDetailVO.builder()
        .oldListPrice(Optional.ofNullable(offlineItem).map(OfflineItem::getListPrice).orElse(null))
        .oldOfferPrice(Optional.ofNullable(offlineItem).map(OfflineItem::getOfferPrice).orElse(null))
        .syncPriceAction(syncPriceAction)
        .clientId(clientId)
        .requestId(requestId)
        .build();
  }
}
