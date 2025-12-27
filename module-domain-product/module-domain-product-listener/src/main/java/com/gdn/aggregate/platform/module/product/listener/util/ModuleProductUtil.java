package com.gdn.aggregate.platform.module.product.listener.util;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.BaseData;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.event.IdTimestamp;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.UrlFriendlyUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.*;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomInventoryPickupPointInfo;
import com.gdn.aggregate.platform.module.product.listener.model.other.CalculatedItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointInventory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaItemCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.SivaProductCombinedUpsertEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.WarehouseInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.CampaignInfo;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Fbb;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Flashsale;
import com.gdn.aggregate.platform.module.product.listener.model.sub.FlashsaleInventory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Measurement;
import com.gdn.aggregate.platform.module.product.listener.model.sub.PickupPointLocation;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Review;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Stock;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductEnded;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductRemoved;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CheapestPriceDay;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomBusinessPartnerPickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomProductReview;
import com.gdn.aggregate.platform.module.product.listener.model.raw.FlashsaleProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaCampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleGroup;
import com.gdn.aggregate.platform.module.product.listener.model.semi.SivaFlashsaleSchedule;
import com.gdn.aggregate.platform.module.product.listener.model.util.CompleteItemData;

import com.gdn.aggregate.platform.module.product.listener.model.util.TerminatedSellerDeletionEventModel;
import com.google.common.collect.Lists;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.PageRequest;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.AbstractMap;
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
import java.util.TimeZone;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.kafka.clients.consumer.ConsumerRecord;

public class ModuleProductUtil {

  public static final int DEFAULT_SEQUENCE = 99999;
  public static final int DEFAULT_PRIORITY = 99999;
  public static final String CURRENCY = "Rp ";
  public static final String PRICE_SEPARATOR = " - ";
  public static final DecimalFormat formatter = new DecimalFormat("#,###");
  public static final int ONLINE_CNC_SCORE = 4000;
  public static final int ONLINE_SCORE = 3000;
  public static final int CNC_SCORE = 2000;
  public static final int NOT_AVAILABLE_SCORE = 1000;
  public static final int BUYABLE_SCORE = 100;
  public static final int DISCOVERABLE_SCORE = 25;
  public static final int BUYABLE_CNC_SCORE = 50;
  public static final int DISCOVERABLE_CNC_SCORE = 10;
  public static final Map<Integer,String> campaignPriority;
  private static final String HYPHEN = "-";
  private static final Logger log = LoggerFactory.getLogger(ModuleProductUtil.class);
  public static final String PRODUCT_REJECTED = "PRODUCT_REJECTED";
  private static final int ZERO = 0;
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String PICKUP_POINT_ID = "PICKUP_POINT_ID";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";



  static {
    campaignPriority = new HashMap<>();
    campaignPriority.put(CampaignPriority.FLASH_SALE,CampaignType.FLASH_SALE);
    campaignPriority.put(CampaignPriority.SPECIAL,CampaignType.SPECIAL);
    campaignPriority.put(CampaignPriority.REGULAR,CampaignType.REGULAR);
  }

  public static String getCampaignType(Integer priority) {
    return Optional.ofNullable(priority)
        .map(val -> campaignPriority.getOrDefault(val,CampaignType.NON_CAMPAIGN))
        .orElseGet(() -> CampaignType.NON_CAMPAIGN);
  }

  public static boolean priorityExpected(Integer expectedPriority, Integer priority) {
    if (CampaignPriority.FLASH_SALE.equals(expectedPriority)) {
      return CampaignPriority.FLASH_SALE.equals(priority);
    } else {
      return !CampaignPriority.FLASH_SALE.equals(priority);
    }
  }

  public static boolean isDirectUpdateSivaBoth(SaveParam saveParam) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getGroupId)
        .map(val -> val.contains(Topics.GROUP_ID_SIVA_BOTH))
        .orElseGet(() -> false);
  }

  public static boolean isDirectUpdateSivaProduct(SaveParam saveParam) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getGroupId)
        .map(val -> val.contains(Topics.GROUP_ID_SIVA_PRODUCT))
        .orElseGet(() -> false);
  }

  public static boolean isDirectUpdateSivaItem(SaveParam saveParam) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getGroupId)
        .map(val -> val.contains(Topics.GROUP_ID_SIVA_ITEM))
        .orElseGet(() -> false);
  }

  /*Id Converter*/
  public static String toAdjustmentProductId(String itemSku, String pickupPointCode, String promoType, String campaignCode) {
    return MainUtil.toId(toAdjustmentProductIds(itemSku,pickupPointCode,promoType,campaignCode));
  }

  public static List<String> toAdjustmentProductIds(String itemSku, String pickupPointCode, String promoType, String campaignCode) {
    return MainUtil.toCleanList(MainUtil.toList(itemSku,pickupPointCode,promoType,campaignCode));
  }

  public static String toMerchantDiscountPriceId(String itemSku, String pickupPointCode) {
    return MainUtil.toId(toMerchantDiscountPriceIds(itemSku,pickupPointCode));
  }

  public static List<String> toMerchantDiscountPriceIds(String itemSku, String pickupPointCode) {
    return MainUtil.toCleanList(MainUtil.toList(itemSku,pickupPointCode));
  }

  public static String toFlashsaleProductId(String productSku, String itemSku, String campaignCode, boolean timeBased) {
    return MainUtil.toId(toFlashsaleProductIds(productSku,itemSku,campaignCode,timeBased));
  }

  public static List<String> toFlashsaleProductIds(String productSku, String itemSku, String campaignCode, boolean timeBased) {
    return MainUtil.toCleanList(MainUtil.toList(productSku,itemSku,campaignCode,MainUtil.fromBooleanToString(timeBased)));
  }

  public static String toCampaignProductId(String campaignCode, String itemSku, String pickupPointCode) {
    return MainUtil.toId(toCampaignProductIds(campaignCode,itemSku,pickupPointCode));
  }

  public static List<String> toCampaignProductIds(String itemSku, String pickupPointCode, String campaignCode) {
    return MainUtil.toCleanList(MainUtil.toList(itemSku,pickupPointCode,campaignCode));
  }

  public static String toCheapestPriceDayId(String campaignCode, String itemSku, String pickupPointCode) {
    return MainUtil.toId(toCheapestPriceDayIds(campaignCode,itemSku,pickupPointCode));
  }

  public static List<String> toCheapestPriceDayIds(String itemSku, String pickupPointCode, String campaignCode) {
    return MainUtil.toCleanList(MainUtil.toList(itemSku,pickupPointCode,campaignCode));
  }

  public static String toCampaignCodeSessionId(String campaignCode, Integer session) {
    List<String> ids = MainUtil.toList(campaignCode,MainUtil.fromIntegerToString(session));
    return MainUtil.toId(MainUtil.toCleanList(ids));
  }

  public static String toPickupPointId(String itemSku, String pickupPointCode) {
    List<String> ids = MainUtil.toList(itemSku,pickupPointCode);
    return MainUtil.toId(MainUtil.toCleanList(ids));
  }

  public static String toScheduleId(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getId)
        .orElse(null);
  }

  public static String toSivaFlashsaleScheduleId(Long start, Long end) {
    List<String> ids = MainUtil.toList(MainUtil.toNotNullString(MainUtil.fromLongToString(start)),MainUtil.fromLongToString(end));
    return MainUtil.toId(MainUtil.toCleanList(ids));
  }

  public static String toSivaFlashsaleScheduleLogo(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
      .map(SivaFlashsaleSchedule::getLogo)
      .orElse(null);
  }

  public static SivaFlashsaleSchedule.Background toSivaFlashsaleScheduleBackground(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
      .map(SivaFlashsaleSchedule::getBackground)
      .orElse(null);
  }

  public static String toSivaFlashsaleScheduleSubFlashsaleUrl(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
      .map(SivaFlashsaleSchedule::getSubFlashsaleUrl)
      .orElse(null);
  }

  public static String toSivaCampaignProductId(SivaCampaignProduct sivaCampaignProduct) {
    return toCampaignCodeSessionId(sivaCampaignProduct.getCampaignCode(),sivaCampaignProduct.getActivateSession());
  }

  public static List<String> toSivaCampaignProductIds(SivaCampaignProduct sivaCampaignProduct) {
    return MainUtil.toList(toSivaCampaignProductId(sivaCampaignProduct));
  }

  public static List<String> toSivaItemIds(SivaItem sivaItem) {
    if (isSivaItemOnL2(sivaItem)) {
      return toSivaItemL2Ids(sivaItem);
    } else {
      return toSivaItemL4Ids(sivaItem);
    }
  }

  public static SivaItemCombinedUpsertEventModel toSivaItemCombinedUpsertEventModel(Item item, SaveParam saveParam,
    SivaItem sivaItem, boolean directSave, Set<String> eligibleDataSources) {
    return SivaItemCombinedUpsertEventModel.builder().item(item).saveParam(saveParam)
      .sivaItem(sivaItem).directSave(directSave).eligibleDataSourcesForUpsert(eligibleDataSources).build();
  }

  public static Optional<Map.Entry<ConsumerRecord<String, String>, SivaItemCombinedUpsertEventModel>> parseToEntry(
      ConsumerRecord<String, String> record, ObjectMapper objectMapper) {
    try {
      SivaItemCombinedUpsertEventModel eventModel = parseEventModel(record, objectMapper);
      return Optional.of(new AbstractMap.SimpleEntry<>(record, eventModel));
    } catch (Exception e) {
      log.error("Error parsing record: partition={}, offset={}", record.partition(),
          record.offset(), e);
      return Optional.empty();
    }
  }

  public static SivaItemCombinedUpsertEventModel parseEventModel(
      ConsumerRecord<String, String> record, ObjectMapper objectMapper)
      throws JsonProcessingException {
    return objectMapper.readValue(record.value(), SivaItemCombinedUpsertEventModel.class);
  }

  public static String extractItemSku(SivaItemCombinedUpsertEventModel model) {
    return Optional.ofNullable(model.getSivaItem()).map(SivaItem::getItemSku)
        .orElseGet(() -> Optional.ofNullable(model.getItem()).map(Item::getItemSku).orElse(null));
  }

  public static RawItemCombinedUpsertEventModel toRawItemCombinedUpsertEventModel(Item item, boolean migration,
      boolean directSave) {
    return RawItemCombinedUpsertEventModel.builder().item(item).migration(migration).directSave(directSave).build();
  }

  public static boolean isSivaItemOnL2(SivaItem sivaItem) {
    return MainUtil.toNotNullString(getSivaItemId(sivaItem)).equals(toSivaItemL2Id(sivaItem));
  }

  public static boolean isSivaItemOnL4(SivaItem sivaItem) {
    return MainUtil.toNotNullString(getSivaItemId(sivaItem)).equals(toSivaItemL4Id(sivaItem));
  }

  private static String getSivaItemId(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(SivaItem::getId)
        .orElse(null);
  }

  private static List<String> toSivaItemL2Ids(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(svItem -> MainUtil.toList(MainUtil.toNotNullString(svItem.getItemCode()),MainUtil.toNotNullString(svItem.getMerchantCode())))
        .orElse(null);
  }

  private static List<String> toSivaItemL4Ids(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(svItem -> Collections.singletonList(MainUtil.toNotNullString(svItem.getItemSku())))
        .orElse(null);
  }

  public static String toSivaItemL2Id(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(ModuleProductUtil::toSivaItemL2Ids)
        .map(MainUtil::toId)
        .orElse(null);
  }

  public static String toSivaItemL4Id(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(ModuleProductUtil::toSivaItemL4Ids)
        .map(MainUtil::toId)
        .orElse(null);
  }

  public static boolean isSivaItemId(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .filter(SivaItem::isIdValid)
        .map(val -> val.toId().equals(MainUtil.toId(toSivaItemL2Ids(val))) || val.toId().equals(MainUtil.toId(toSivaItemL4Ids(val))))
        .orElseGet(() -> false);
  }

  public static boolean isSivaItemMarkForDelete(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(SivaItem::isMarkForDelete)
        .orElseGet(() -> false);
  }

  public static boolean isL2SivaItemMarkForDelete(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .filter(ModuleProductUtil::isSivaItemOnL2)
        .map(ModuleProductUtil::isSivaItemMarkForDelete)
        .orElseGet(() -> false);
  }

  public static void setMarkForDeleteIfL2AndL4Exists(SivaItem l2SivaItem, SivaItem l4SivaItem) {
    if (Objects.nonNull(l2SivaItem) && Objects.nonNull(l4SivaItem)) {
      l2SivaItem.setMarkForDelete(true);
    }
  }
  /*End of Id Converter*/

  /*Time Converter*/
  public static long getCurrentTimestamp() {
    return Optional.of(new Date())
        .map(Date::getTime)
        .orElseGet(() -> 0L);
  }

  public static boolean isItOnToday(Long startDate, Long endDate, Long currentDate) {
    if(Objects.isNull(startDate) || Objects.isNull(endDate) || Objects.isNull(currentDate)) {
      return false;
    } else {
      return startDate.compareTo(currentDate) <= 0 && endDate.compareTo(currentDate) >= 0;
    }
  }

  public static boolean isItAlreadyStarted(Long startDate, Long currentDate) {
    if(Objects.isNull(startDate) || Objects.isNull(currentDate)) {
      return false;
    } else {
      return startDate.compareTo(currentDate) <= 0;
    }
  }

  public static boolean isItNotEnded(Long endDate, Long currentDate) {
    if(Objects.isNull(endDate) || Objects.isNull(currentDate)) {
      return false;
    } else {
      return endDate.compareTo(currentDate) >= 0;
    }
  }

  public static boolean isItSameTime(Long startTime, Long endTime) {
    if(Objects.isNull(startTime) || Objects.isNull(endTime)) {
      return false;
    } else {
      return startTime.compareTo(endTime) == 0;
    }
  }

  public static long toTruncatedExpiryTime(long expiryTime, DayOfWeek day) {
    LocalDateTime localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(expiryTime), TimeZone.getDefault().toZoneId()).truncatedTo(ChronoUnit.DAYS);
    if (Objects.nonNull(day)) {
      localDateTime = localDateTime.with(TemporalAdjusters.next(day));
    }
    return ZonedDateTime.of(localDateTime, ZoneId.systemDefault()).toInstant().toEpochMilli();
  }
  /*End of Time Converter*/

  /*Scheduler Converter*/
  public static boolean isPriceNotValid(SivaProduct sivaProduct) {
    return Optional.ofNullable(sivaProduct)
        .map(SivaProduct::getCampaignInfos)
        .orElseGet(HashMap::new)
        .values()
        .stream()
        .filter(Objects::nonNull)
        .map(CampaignInfo::getPrice)
        .filter(Objects::nonNull)
        .map(val -> MainUtil.toNotNullDouble(val.getOfferValue()))
        .anyMatch(val -> val <= 0);
  }

  public static boolean isItemCodeNotValid(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(SivaItem::getItemCode)
        .map(itemCode -> !StringUtils.isEmpty(itemCode))
        .orElseGet(() -> false);
  }

  public static List<Long> toNotifyTimes(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct.getSchedule())
        .map(SivaFlashsaleSchedule::getEnd)
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }
  /*Scheduler Converter*/

  /*Product Converter*/
  public static String toPickupPointCodeParam(CampaignInfo campaignInfo) {
    String param = Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPickupPointCode)
        .map(ppCode -> String.format("?pickupPointCode=%s", ppCode))
        .orElse(null);
    return MainUtil.toNotNullString(param);
  }

  public static MasterDataProduct getMasterDataProductFromProduct(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .orElse(null);
  }

  public static String toProductCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getProductCode)
        .orElseGet(() -> getProductCodeFromProduct(product));
  }

  public static String getProductCodeFromProduct(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(MasterDataProduct::getProductCode)
        .orElse(null);
  }

  public static String getProductTypeFromProduct(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getProductType)
        .orElse(null);
  }

  public static String toProductMerchantCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMerchantCode)
        .orElse(null);
  }

  public static boolean toProductSync(Product product) {
    return Optional.ofNullable(product)
        .map(Product::isSync)
        .orElseGet(() -> false);
  }

  public static boolean toProductMarkForDelete(Product product) {
    return Optional.ofNullable(product)
        .map(Product::isMarkForDelete)
        .orElseGet(() -> false);
  }

  public static boolean toProductMarkForDelete(Product product, boolean allItemsMarkForDeleteTrue) {
    return toProductMarkForDelete(product) || allItemsMarkForDeleteTrue;
  }

  public static boolean toProductOff2OnChannelActive(Product product) {
    return Optional.ofNullable(product)
        .map(Product::isOff2OnChannelActive)
        .orElseGet(() -> false);
  }

  public static String toProductName(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(MasterDataProduct::getProductName)
        .orElseGet(() -> toProductSku(product));
  }

  public static String toBrand(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(MasterDataProduct::getBrand)
        .orElse(null);
  }

  public static String toBrandLogoUrl(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(MasterDataProduct::getBrandLogoUrl)
        .orElse(null);
  }

  public static String toDescription(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterDataProduct)
        .map(MasterDataProduct::getDescription)
        .orElse(null);
  }

  public static String toProductUrlName(Product product) {
    return UrlFriendlyUtil.createUrlFriendlyString(toProductName(product));
  }

  public static String toProductUrl(Product product, CampaignInfo campaignInfo, String metaData) {
    return String.format("/p/%s/ps--%s%s%s",
        toProductUrlName(product),
        MainUtil.toNotNullString(toProductSku(product)),
        toPickupPointCodeParam(campaignInfo),
        MainUtil.toNotNullString(metaData));
  }

  public static String toProductSku(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getProductSku)
        .orElse(null);
  }
  /*End of Product Converter*/

  /*Item Converter*/
  public static MasterDataItem getMasterDataItemFromItem(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .orElse(null);
  }

  public static String toItemCode(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getItemCode)
        .orElseGet(() -> getItemCodeFromItem(item));
  }

  public static String getItemCodeFromItem(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(MasterDataItem::getSkuCode)
        .orElse(null);
  }

  public static String getProductCodeFromItem(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(MasterDataItem::getProductCode)
        .orElse(null);
  }

  public static Measurement toMeasurement(MasterDataItem masterDataItem, MasterDataProduct masterDataProduct) {
    return MainUtil.getOrDefault(toMeasurementByMasterDataItem(masterDataItem),toMeasurementByMasterDataProduct(masterDataProduct));
  }

  private static Measurement toMeasurementByMasterDataItem(MasterDataItem masterDataItem) {
    return Optional.ofNullable(masterDataItem)
        .filter(ModuleProductUtil::isMasterDataItemHasMeasurement)
        .map(val -> Measurement.builder()
            .length(val.getItemLength())
            .width(val.getItemWidth())
            .height(val.getItemHeight())
            .weight(val.getItemWeight())
            .build())
        .orElse(null);
  }

  public static boolean isMasterDataItemHasMeasurement(MasterDataItem masterDataItem) {
    return Optional.ofNullable(masterDataItem)
        .filter(val -> !MainUtil.isDoubleEqual(val.getItemLength(),0D) || !MainUtil.isDoubleEqual(val.getItemWidth(),0D) || !MainUtil.isDoubleEqual(val.getItemHeight(),0D) || !MainUtil.isDoubleEqual(val.getItemWeight(),0D))
        .isPresent();
  }

  private static Measurement toMeasurementByMasterDataProduct(MasterDataProduct masterDataProduct) {
    return Optional.ofNullable(masterDataProduct)
        .filter(ModuleProductUtil::isMasterDataProductHasMeasurement)
        .map(val -> Measurement.builder()
            .length(val.getLength())
            .width(val.getWidth())
            .height(val.getHeight())
            .weight(val.getWeight())
            .build())
        .orElse(null);
  }

  public static boolean isMasterDataProductHasMeasurement(MasterDataProduct masterDataProduct) {
    return Optional.ofNullable(masterDataProduct)
        .filter(val -> !MainUtil.isDoubleEqual(val.getLength(),0D) || !MainUtil.isDoubleEqual(val.getWidth(),0D) || !MainUtil.isDoubleEqual(val.getHeight(),0D) || !MainUtil.isDoubleEqual(val.getWeight(),0D))
        .isPresent();
  }

  public static boolean toItemOff2OnChannelActive(Item item) {
    return Optional.ofNullable(item)
        .map(Item::isOff2OnChannelActive)
        .orElseGet(() -> false);
  }

  public static boolean toItemSync(Item item) {
    return Optional.ofNullable(item)
        .map(Item::isSync)
        .orElseGet(() -> false);
  }

  public static boolean toItemArchived(Item item) {
    return Optional.ofNullable(item)
        .map(Item::isArchived)
        .orElseGet(() -> false);
  }

  public static boolean toItemsArchived(List<Item> items) {
    return Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .allMatch(Item::isArchived);
  }

  public static boolean toItemMarkForDelete(Item item) {
    return Optional.ofNullable(item)
        .map(Item::isMarkForDelete)
        .orElseGet(() -> false);
  }

  public static boolean toItemsMarkForDelete(List<Item> items) {
    return Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .allMatch(Item::isMarkForDelete);
  }

  public static boolean toItemMarkForDelete(Item item, Product product) {
    return toItemMarkForDelete(item) || toProductMarkForDelete(product);
  }

  public static boolean toItemSubscribable(Item item) {
    return Optional.ofNullable(item)
        .map(Item::isSubscribable)
        .orElseGet(() -> false);
  }

  public static String toItemName(Product product, Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(MasterDataItem::getGeneratedItemName)
        .orElseGet(() -> toFallbackItemName(product, item));
  }

  private static String toFallbackItemName(Product product, Item item) {
    return Optional.ofNullable(item)
        .map(Item::getGeneratedItemName)
        .orElseGet(() -> toProductName(product));
  }

  public static String toItemUrlName(Product product, Item item) {
    return UrlFriendlyUtil.createUrlFriendlyString(toItemName(product, item));
  }

  public static String toItemUrl(Product product, Item item, CampaignInfo campaignInfo, String metaData) {
    return String.format("/p/%s/is--%s%s%s",
        toItemUrlName(product, item),
        MainUtil.toNotNullString(toItemSku(item, campaignInfo)),
        toPickupPointCodeParam(campaignInfo),
        MainUtil.toNotNullString(metaData));
  }

  public static String toItemSku(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getItemSku)
        .orElse(null);
  }

  private static String toItemSku(Item item, CampaignInfo campaignInfo) {
    return Optional.ofNullable(item)
        .map(Item::getItemSku)
        .orElseGet(() -> toFirstItemSku(campaignInfo));
  }

  private static String toFirstItemSku(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getItemSkus)
        .map(Collection::stream)
        .map(Stream::findFirst)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .orElse(null);
  }

  public static String fromItemSkuToProductSku(String itemSku) {
    return Optional.ofNullable(itemSku)
        .map(val -> val.substring(0, val.lastIndexOf('-')))
        .orElse(null);
  }

  public static List<String> toItemSkus(List<Item> items) {
    List<String> result = Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> !val.isArchived())
        .filter(val -> !val.isMarkForDelete())
        .map(Item::getItemSku)
        .filter(Objects::nonNull)
        .distinct()
        .collect(Collectors.toList());
    return Optional.of(result)
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .orElseGet(() -> MainUtil.toList(toItemSku(MainUtil.toListFirstData(items))));
  }

  public static Item getItem(List<Item> items, String itemSku) {
    return items.stream()
        .filter(Objects::nonNull)
        .filter(val -> Objects.nonNull(val.getItemSku()))
        .filter(val -> val.getItemSku().equals(itemSku))
        .findFirst()
        .orElse(null);
  }

  public static List<MasterDataItem.MasterDataItemAttributeValue> toMasterDataItemAttributeValues(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(MasterDataItem::getMasterDataItemAttributeValues)
        .map(vals -> vals.stream()
            .filter(Objects::nonNull)
            .collect(Collectors.toList()))
        .orElse(null);
  }

  public static long toFirstTimestamp(List<Item> items) {
    return Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(Item::getTimestamp)
        .findFirst()
        .orElseGet(ModuleProductUtil::getCurrentTimestamp);
  }

  public static String toFirstProductSku(List<Item> items) {
    return Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(Item::getProductSku)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }
  /*End Item Converter*/

  /*Review Converter*/
  public static String toReviewId(CustomProductReview customProductReview) {
    return Optional.ofNullable(customProductReview)
        .map(CustomProductReview::getId)
        .orElse(null);
  }

  public static Review toReview(CustomProductReview customProductReview) {
    return Optional.ofNullable(customProductReview)
        .map(cpr -> {
          double rating = toDecimalRating(customProductReview);
          int count = toReviewCount(customProductReview);

          return new Review(rating, count);
        })
        .filter(ModuleProductUtil::isReviewValid)
        .orElse(null);
  }

  private static boolean isReviewValid(Review review) {
    return Optional.ofNullable(review)
        .filter(val -> Double.compare(val.getRating(),0.0D)!=0)
        .filter(val -> val.getCount()!=0)
        .isPresent();
  }

  private static Double toDecimalRating(CustomProductReview customProductReview) {
    return Optional.ofNullable(customProductReview)
        .map(CustomProductReview::getDecimalRating)
        .orElseGet(() -> 0.0D);
  }

  private static Integer toReviewCount(CustomProductReview customProductReview) {
    return Optional.ofNullable(customProductReview)
        .map(CustomProductReview::getReviewCount)
        .orElseGet(() -> 0);
  }
  /*End of Review Converter*/

  /*Quota Converter*/
  public static String getQuotaItemSku(Quota quota) {
    return Optional.ofNullable(quota)
        .map(Quota::getItemSku)
        .orElse(null);
  }

  public static String getQuotaPickupPointCode(Quota quota) {
    return Optional.ofNullable(quota)
        .map(Quota::getPickupPointCode)
        .orElse(null);
  }
  /*End of Quota Converter*/

  /*Stock Converter*/
  public static Stock initializeStock(String itemSku, String pickupPointCode) {
    return Stock.builder()
        .itemSku(itemSku)
        .pickupPointCode(pickupPointCode)
        .quota(0)
        .remaining(0)
        .exists(false)
        .status(StockStatus.OOS)
        .build();
  }

  public static boolean isStockAvailable(Stock stock) {
    return Optional.ofNullable(stock)
        .filter(Stock::isExists)
        .map(Stock::getStatus)
        .map(val -> !StockStatus.OOS.equals(val))
        .orElseGet(() -> true);
  }

  public static boolean isFlashsaleStockAvailable(Stock flashsaleStock) {
    return Optional.ofNullable(flashsaleStock)
        .map(Stock::getStatus)
        .map(val -> !StockStatus.OOS.equals(val))
        .orElseGet(() -> false);
  }

  public static boolean isAvailable(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(val -> !toMarkForDelete(val) && !PurchasedType.NOT_AVAILABLE.equals(toPurchasedType(val)) && isFlashsaleStockAvailable(val.getFlashsaleStock()))
        .orElseGet(() -> false);
  }

  public static Boolean isCampaignInStock(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getInventory)
        .map(Stock::getRemaining)
        .map(remaining -> remaining > 0)
        .orElseGet(() -> false);
  }

  public static Stock fromQuotaToStock(Quota quota) {
    return Optional.ofNullable(quota)
        .map(val -> Stock.builder()
            .itemSku(val.getItemSku())
            .pickupPointCode(val.getPickupPointCode())
            .quota(val.getQuota())
            .remaining(val.getRemaining())
            .exists(true)
            .status(convertStockStatus(val.getRemaining()))
            .build())
        .orElseGet(() -> initializeStock(getQuotaItemSku(quota),getQuotaPickupPointCode(quota)));
  }
  /*End of Stock Converter*/

  /*Tag Converter*/
  public static List<String> toTags(List<Item> items, boolean fbbActivated, boolean hasCncActivated, String purchasedType, int purchasedTypeScore, boolean onePartyActivated) {
    List<Item.Tag> tags = getTags(items);
    List<String> result = new ArrayList<>();

    if (isTagsContainsThd(tags)) {
      result.add(ProductTag.LOGISTIC_BADGE_VALUE);
    }
    if (isTagsContainsFbb(tags) || fbbActivated) {
      result.add(ProductTag.BLIBLI_SHIPPING);
    }
    if (isTagsContainsCnc(tags) || isPurchasedTypeSupportCnc(hasCncActivated,purchasedType,purchasedTypeScore,onePartyActivated)) {
      result.add(ProductTag.CNC_AVAILABLE);
    }
    if (isTagsContainsTradeIn(tags)) {
      result.add(ProductTag.TUKAR_TAMBAH);
    }

    return result;
  }

  public static List<Item.Tag> getTags(List<Item> items) {
    return Optional.ofNullable(items)
        .orElseGet(ArrayList::new)
        .stream()
        .map(ModuleProductUtil::getTag)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public static Item.Tag getTag(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getTag)
        .orElse(null);
  }

  public static boolean isTagsContainsThd(List<Item.Tag> tags) {
    return tags.stream().anyMatch(Item.Tag::isThd);
  }

  public static boolean isTagsContainsFbb(List<Item.Tag> tags) {
    return tags.stream().anyMatch(Item.Tag::isFbb);
  }

  public static boolean isTagsContainsCnc(List<Item.Tag> tags) {
    return tags.stream().anyMatch(Item.Tag::isCnc);
  }

  public static boolean isTagsContainsTradeIn(List<Item.Tag> tags) {
    return tags.stream().anyMatch(Item.Tag::isTradeIn);
  }

  public static boolean isPurchasedTypeSupportCnc(boolean hasCncActivated, String purchasedType, int purchasedTypeScore, boolean onePartyActivated) {
    if (!onePartyActivated) {
      return hasCncActivated;
    } else {
    return !PurchasedType.ONLINE.equals(purchasedType) && purchasedTypeScore!=NOT_AVAILABLE_SCORE;
    }
  }

  public static Item.Tag toTag(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getTag)
        .orElseGet(Item.Tag::new);
  }
  /*End of Tag Converter*/

  /*Merchant Converter*/
  public static String toMerchantDiscountPricePickupPointCode(MerchantDiscountPrice merchantDiscountPrice) {
    return Optional.ofNullable(merchantDiscountPrice)
        .map(MerchantDiscountPrice::getPickupPointCode)
        .orElse(null);
  }

  public static Long getMerchantDiscountPriceEnd(MerchantDiscountPrice merchantDiscountPrice) {
    return Optional.ofNullable(merchantDiscountPrice)
        .map(MerchantDiscountPrice::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .findFirst()
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .map(PickupPoint.DiscountPrice::getEndDateTime)
        .orElseGet(() -> 0L);
  }
  /*End of Merchant Converter*/

  /*FlashsaleProduct Converter*/
  public static String getSivaFlashsaleScheduleLogo(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
        .map(SivaFlashsaleSchedule::getLogo)
        .orElse(null);
  }

  public static String getSivaFlashsaleScheduleSubFlashsaleUrl(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
        .map(SivaFlashsaleSchedule::getSubFlashsaleUrl)
        .orElse(null);
  }

  public static SivaFlashsaleSchedule.Background getSivaFlashsaleScheduleBackground(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
        .map(SivaFlashsaleSchedule::getBackground)
        .orElse(null);
  }

  public static String getFlashsaleProductProductSku(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getProductSku)
        .orElse(null);
  }
  public static String getFlashsaleProductItemSku(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getItemSku)
        .orElse(null);
  }

  public static String getProductTypeFromFlashsaleProduct(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getProductType)
        .orElseGet(() -> ProductType.REGULAR);
  }

  public static Long getFlashsaleProductStart(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getStart)
        .orElseGet(() -> 0L);
  }

  public static Long getFlashsaleProductEnd(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getEnd)
        .orElseGet(() -> 0L);
  }

  public static boolean getFlashsaleExclusive(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::isExclusive)
        .orElseGet(() -> false);
  }

  public static String getFlashsaleProductLogo(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getLogo)
        .orElse(null);
  }

  public static String getFlashsaleProductSubFlashsaleUrl(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getSubFlashsaleUrl)
        .orElse(null);
  }

  public static SivaFlashsaleSchedule.Background getFlashsaleProductBackground(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::getBackground)
        .orElse(null);
  }

  public static boolean isFlashsaleProductActive(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::isActive)
        .orElseGet(() -> false);
  }

  public static boolean isFlashsaleProductTimeBased(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(FlashsaleProduct::getSchedule)
        .map(SivaFlashsaleSchedule::isTimeBased)
        .orElseGet(() -> false);
  }

  public static Flashsale toFlashsaleFromFlashsaleProduct(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(fsProduct -> Flashsale.builder()
            .sequence(fsProduct.getSequence())
            .scheduleId(ModuleProductUtil.toScheduleId(fsProduct))
            .campaignCode(fsProduct.getCampaignCode())
            .type(getProductTypeFromFlashsaleProduct(fsProduct))
            .start(ModuleProductUtil.getFlashsaleProductStart(fsProduct))
            .end(ModuleProductUtil.getFlashsaleProductEnd(fsProduct))
            .logo(ModuleProductUtil.getFlashsaleProductLogo(fsProduct))
            .subFlashsaleUrl(ModuleProductUtil.getFlashsaleProductSubFlashsaleUrl(fsProduct))
            .timeBased(ModuleProductUtil.isFlashsaleProductTimeBased(fsProduct))
            .active(fsProduct.isActive())
            .exclusive(fsProduct.isExclusive())
            .groupIds(fsProduct.getGroupIds())
            .build())
        .orElse(null);
  }

  public static Flashsale toFlashsaleFromCampaignInfo(CampaignInfo campaignInfo, boolean timeBased) {
    return Optional.ofNullable(campaignInfo)
        .map(cpgInfo -> Flashsale.builder()
            .sequence(ModuleProductUtil.DEFAULT_SEQUENCE)
            .scheduleId(String.format("%s-%s",cpgInfo.getStartTime(),cpgInfo.getEndTime()))
            .campaignCode(cpgInfo.getCampaignCode())
            .type(ProductType.REGULAR)
            .start(cpgInfo.getStartTime())
            .end(cpgInfo.getEndTime())
            .timeBased(timeBased)
            .active(true)
            .groupIds(new ArrayList<>())
            .build())
        .orElse(null);
  }

  public static FlashsaleProduct toFlashsaleProduct(CampaignProduct campaignProduct, SivaFlashsaleSchedule existingFlashsaleSchedule, boolean exclusive, SaveParam saveParam) {
    return Optional.ofNullable(campaignProduct)
        .filter(val -> isChainSaveFlashsaleProduct(val,saveParam))
        .map(val -> FlashsaleProduct.builder()
            .active(val.isActive())
            .exclusive(exclusive)
            .campaignCode(val.getCampaignCode())
            .sessionId(toCampaignCampaignSessionId(val))
            .timestamp(val.getTimestamp())
            .productSku(toCampaignProductSku(val))
            .itemSku(toCampaignItemSku(val))
            .pickupPointCode(toCampaignPickupPointCode(val))
            .schedule(toSivaFlashsaleSchedule(val, existingFlashsaleSchedule))
            .groupIds(toCampaignGroupIds(val))
            .flashsaleQuotas(toFlashsaleQuotas(val))
            .build())
        .orElse(null);
  }

  public static boolean isChainSaveFlashsaleProduct(CampaignProduct campaignProduct, SaveParam saveParam) {
    return CampaignType.FLASH_SALE.equals(campaignProduct.getCampaignType())
        && isCampaignProductPublishedGroup(saveParam);
  }

  public static boolean isCampaignProductPublishedGroup(SaveParam saveParam) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getGroupId)
        .map(val -> val.equals(Topics.GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED))
        .orElseGet(() -> false);
  }

  public static boolean filterNearestActiveFlashsaleProductByCampaignCode(FlashsaleProduct flashsaleProduct, String campaignCode) {
    return Optional.ofNullable(flashsaleProduct)
        .filter(val -> {
          if (StringUtils.isNotEmpty(val.getCampaignCode()) && StringUtils.isNotEmpty(campaignCode)) {
            return MainUtil.isStringEqual(val.getCampaignCode(),campaignCode);
          } else {
            return true;
          }
        })
        .isPresent();
  }

  public static boolean filterNearestActiveFlashsaleProductByItemSku(FlashsaleProduct flashsaleProduct, String itemSku) {
    return Optional.ofNullable(flashsaleProduct)
        .filter(val -> {
          if (StringUtils.isNotEmpty(val.getItemSku()) && StringUtils.isNotEmpty(itemSku)) {
            return MainUtil.isStringEqual(val.getItemSku(),itemSku);
          } else {
            return true;
          }
        })
        .isPresent();
  }

  public static SivaFlashsaleSchedule toSivaFlashsaleSchedule(CampaignProduct campaignProduct, SivaFlashsaleSchedule existingFlashsaleSchedule) {
    return SivaFlashsaleSchedule.builder()
        .id(toSivaFlashsaleScheduleId(campaignProduct.getPromotionStartTime(),campaignProduct.getPromotionEndTime()))
        .logo(toSivaFlashsaleScheduleLogo(existingFlashsaleSchedule))
        .background(toSivaFlashsaleScheduleBackground(existingFlashsaleSchedule))
        .subFlashsaleUrl(toSivaFlashsaleScheduleSubFlashsaleUrl(existingFlashsaleSchedule))
        .timeBased(toCampaignTimeBased(campaignProduct))
        .start(campaignProduct.getPromotionStartTime())
        .end(campaignProduct.getPromotionEndTime())
        .build();
  }

  public static List<FlashsaleProduct.FlashsaleQuota> toFlashsaleQuotas(CampaignProduct campaignProduct) {
    List<FlashsaleProduct.FlashsaleQuota> flashsaleProductQuotas = new ArrayList<>();
    Optional.ofNullable(toFlashsaleQuota(campaignProduct, CampaignOwner.MERCHANT))
        .ifPresent(flashsaleProductQuotas::add);
    Optional.ofNullable(toFlashsaleQuota(campaignProduct, CampaignOwner.BLIBLI))
        .ifPresent(flashsaleProductQuotas::add);

    return flashsaleProductQuotas;
  }

  public static FlashsaleProduct.FlashsaleQuota toFlashsaleQuota(CampaignProduct campaignProduct, String owner) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(sku -> FlashsaleProduct.FlashsaleQuota.builder()
            .owner(owner)
            .quota(owner.equals(CampaignOwner.BLIBLI) ? sku.getBlibliQuota() : sku.getQuota())
            .build())
        .orElse(null);
  }

  public static int toSchedulesSize(SivaFlashsaleGroup sivaFlashsaleGroup) {
    return Optional.ofNullable(sivaFlashsaleGroup)
        .map(SivaFlashsaleGroup::getSchedules)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList())
        .size();
  }

  public static boolean isAllSchedulesActive(SivaFlashsaleGroup sivaFlashsaleGroup) {
    return Optional.ofNullable(sivaFlashsaleGroup)
        .map(ModuleProductUtil::toNotEndedFlashsaleGroupSchedules)
        .map(vals -> !CollectionUtils.isEmpty(vals))
        .orElseGet(() -> false);
  }

  public static List<SivaFlashsaleSchedule> toNotEndedFlashsaleGroupSchedules(SivaFlashsaleGroup sivaFlashsaleGroup) {
    long currentDate = getCurrentTimestamp();
    return Optional.ofNullable(sivaFlashsaleGroup)
        .map(SivaFlashsaleGroup::getSchedules)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> isItNotEnded(val.getEnd(),currentDate))
        .collect(Collectors.toList());
  }

  public static AdjustmentProduct fromFlashsaleProductToAdjustmentProduct(FlashsaleProduct flashsaleProduct) {
    return Optional.ofNullable(flashsaleProduct)
        .map(val -> AdjustmentProduct.builder()
            .id(toCampaignProductId(val.getCampaignCode(),val.getItemSku(),val.getPickupPointCode()))
            .itemSku(val.getItemSku())
            .campaignCode(val.getCampaignCode())
            .promoType(CampaignType.FLASH_SALE)
            .startDate(getFlashsaleProductStart(val))
            .endDate(getFlashsaleProductEnd(val))
            .initValue(0L)
            .value(0L)
            .activated(val.isActive())
            .promoCampaign(true)
            .sessionId(flashsaleProduct.getSessionId())
            .priority(CampaignPriority.FLASH_SALE)
            .pickupPointCode(val.getPickupPointCode())
            .initBudgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI))
            .budgetOwners(MainUtil.toSet(CampaignOwner.BLIBLI))
            .build())
        .orElse(null);
  }
  /*End of FlashsaleProduct Converter*/

  public static FlashsaleProduct fromAdjustmentProductToFlashsaleProduct(
    AdjustmentProduct adjustmentProduct, List<String> flashsalePromoTypes) {
    if (Objects.isNull(adjustmentProduct)) {
      return null;
    }
    if (!isFlashsalePromoType(adjustmentProduct.getPromoType(), flashsalePromoTypes)) {
      return null;
    }

    // Create SivaFlashsaleSchedule for the flashsale product
    SivaFlashsaleSchedule schedule = SivaFlashsaleSchedule.builder().id(
        toSivaFlashsaleScheduleId(adjustmentProduct.getStartDate(), adjustmentProduct.getEndDate()))
      .start(adjustmentProduct.getStartDate()).end(adjustmentProduct.getEndDate())
      .timeBased(true) // Default to time-based for adjustment products
      .build();

    return FlashsaleProduct.builder().itemSku(adjustmentProduct.getItemSku())
      .pickupPointCode(adjustmentProduct.getPickupPointCode())
      .campaignCode(adjustmentProduct.getCampaignCode()).schedule(schedule)
      .active(adjustmentProduct.isActivated()).sessionId(adjustmentProduct.getSessionId()).build();
  }

  /**
   * Convert AdjustmentProduct to CampaignProduct
   */
  public static CampaignProduct fromAdjustmentProductToCampaignProduct(
    AdjustmentProduct adjustmentProduct, List<String> flashsalePromoTypes) {
    if (Objects.nonNull(adjustmentProduct) || isFlashsalePromoType(adjustmentProduct.getPromoType(),
      flashsalePromoTypes)) {
      return null;
    }
    CampaignProductPublished.ProductSkuEventModel sku =
      CampaignProductPublished.ProductSkuEventModel.builder()
        .itemSku(adjustmentProduct.getItemSku())
        .pickupPointCode(adjustmentProduct.getPickupPointCode())
        .discount(MainUtil.toNotNullDouble((double) adjustmentProduct.getValue())).finalPrice(0D)
        .markForDelete(!adjustmentProduct.isActivated()).sessionId(adjustmentProduct.getSessionId())
        .build();

    return CampaignProduct.builder().campaignCode(adjustmentProduct.getCampaignCode())
      .campaignCodeSessionId(toCampaignCodeSessionId(adjustmentProduct.getCampaignCode(),
        adjustmentProduct.getSessionId())).campaignName(adjustmentProduct.getCampaignCode())
      .promotionStartTime(adjustmentProduct.getStartDate())
      .promotionEndTime(adjustmentProduct.getEndDate()).active(adjustmentProduct.isActivated())
      .tagLabel(null).exclusive(adjustmentProduct.isExclusiveProduct())
      .retainData(false) // Default value
      .campaignType(adjustmentProduct.getPromoType())
      .flashSaleCampaignName(null) // for non flashSale setting it as null
      .priority(adjustmentProduct.getPriority()).backgroundImageUrl(null).sku(sku).build();
  }

  public static boolean isFlashsalePromoType(String promoType, List<String> flashsalePromoTypes) {
    if (MainUtil.toNotNullString(promoType).isEmpty() || CollectionUtils.isEmpty(
      flashsalePromoTypes)) {
      return false;
    }
    return flashsalePromoTypes.contains(promoType);
  }

  public static boolean isPromoCampaign(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct).filter(
      val -> CampaignType.CAMPAIGNS.contains(val.getPromoType()) && Objects.nonNull(
        val.getCampaignCode()) && !Default.CAMPAIGN_CODE.equals(val.getCampaignCode())).isPresent();
  }

  /*End of NEW AdjustmentProduct Converter*/

  /*Adjustment Converter*/
  public static String toAdjCampaignCode(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(val -> {
          if (CampaignType.NON_CAMPAIGN.equals(val.getPromoType())) {
            return null;
          } else {
            if (Default.CAMPAIGN_CODE.equals(val.getCampaignCode())) {
              return null;
            } else {
              return val.getCampaignCode();
            }
          }
        })
        .orElse(null);
  }

  public static String toRealCampaignCode(String campaignCode) {
    return Optional.ofNullable(campaignCode)
        .filter(val -> !Default.CAMPAIGN_CODE.equals(campaignCode))
        .orElse(null);
  }

  public static String toAdjPickupPointCode(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(AdjustmentProduct::getPickupPointCode)
        .orElse(null);
  }

  public static String toAdjPromoType(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .filter(val -> Objects.nonNull(val.getCampaignCode()) && !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()))
        .map(val -> getCampaignType(val.getPriority()))
        .orElseGet(() -> CampaignType.NON_CAMPAIGN);
  }

  public static boolean isNewBudgetOwnersMoreComplete(AdjustmentProduct existingData, AdjustmentProduct newData, boolean chain) {
    return toAdjBudgetOwnersSize(newData) >= toAdjBudgetOwnersSize(existingData) && !chain;
  }

  private static int toAdjBudgetOwnersSize(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(AdjustmentProduct::getBudgetOwners)
        .map(Set::size)
        .orElseGet(() -> 0);
  }

  public static Long toAdjProductEndDate(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(AdjustmentProduct::getEndDate)
        .orElse(null);
  }

  public static String toAdjQuotaStatus(List<Quota> quotas) {
    int remaining = Optional.ofNullable(quotas)
        .orElseGet(ArrayList::new)
        .stream()
        .map(Quota::getRemaining)
        .reduce(0,Integer::sum);
    return convertStockStatus(remaining);
  }

  public static Quota initializeQuota(String owner) {
    return Quota.builder()
        .owner(owner)
        .used(0)
        .quota(0)
        .remaining(0)
        .build();
  }

  public static void insertQuotaAdjustments(String owner, Quota quota, List<Quota> result) {
    Optional.ofNullable(quota)
        .filter(val -> owner.equals(val.getOwner()))
        .ifPresent(result::add);
  }

  public static AdjustmentProduct generateAdjMandatoryData(AdjustmentProduct adjustmentProduct) {
    adjustmentProduct.setPromoType(toAdjPromoType(adjustmentProduct));
    adjustmentProduct.setCampaignCode(toAdjCampaignCode(adjustmentProduct));
    adjustmentProduct.setId(adjustmentProduct.toId());
    adjustmentProduct.setPromoCampaign(isPromoCampaign(adjustmentProduct));
    return adjustmentProduct;
  }

  public static AdjustmentProduct toCleanAdjustmentProduct(AdjustmentProduct adjustmentProduct) {
    adjustmentProduct = generateAdjMandatoryData(adjustmentProduct);
    if (StringUtils.isEmpty(adjustmentProduct.getCampaignCode())) {
      adjustmentProduct.setCampaignCode(Default.CAMPAIGN_CODE);
    }
    return adjustmentProduct;
  }


  public static boolean isAdjustmentExists(List<AdjustmentProduct> adjustmentProducts, String campaignProductId) {
    return Optional.ofNullable(adjustmentProducts)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> toCampaignProductId(val.getCampaignCode(),val.getItemSku(),val.getPickupPointCode()))
        .filter(Objects::nonNull)
        .anyMatch(id -> id.equals(campaignProductId));
  }

  public static List<AdjustmentProduct> toCampaignAdjustments(List<AdjustmentProduct> adjustmentProducts) {
    return Optional.ofNullable(adjustmentProducts)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(ModuleProductUtil::isPromoCampaign)
        .collect(Collectors.toList());
  }

  public static List<AdjustmentProduct> toNonCampaignAdjustments(List<AdjustmentProduct> adjustmentProducts) {
    Long currentDate = getCurrentTimestamp();
    return Optional.ofNullable(adjustmentProducts)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(val -> !isPromoCampaign(val))
        .filter(val -> isItOnToday(val.getStartDate(),val.getEndDate(),currentDate))
        .collect(Collectors.toList());
  }

  public static String getAdjustmentProductQuotaCampaignCode(AdjustmentProductQuota adjustmentProductQuota) {
    return Optional.ofNullable(adjustmentProductQuota)
        .map(AdjustmentProductQuota::getCampaignCode)
        .orElse(null);
  }

  public static String getAdjustmentProductQuotaPickupPointCode(AdjustmentProductQuota adjustmentProductQuota) {
    return Optional.ofNullable(adjustmentProductQuota)
        .map(AdjustmentProductQuota::getPickupPointCode)
        .orElse(null);
  }
  /*End of Adjustment Converter*/

  /*Campaign Converter*/
  private static boolean isEndWithSession(CampaignProductEnded campaignProductEnded) {
    return Optional.ofNullable(campaignProductEnded)
        .map(CampaignProductEnded::getCampaignSessionList)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CampaignProductEnded.CampaignSession::getSession)
        .anyMatch(Objects::nonNull);
  }

  public static List<String> toCampaignCodeSessionIds(CampaignProductEnded campaignProductEnded) {
    Set<String> result = new HashSet<>();
    if (isEndWithSession(campaignProductEnded)) {
      result = Optional.ofNullable(campaignProductEnded)
          .map(CampaignProductEnded::getCampaignSessionList)
          .orElseGet(ArrayList::new)
          .stream()
          .filter(Objects::nonNull)
          .map(val -> toCampaignCodeSessionId(val.getCampaignCode(),val.getSession()))
          .filter(Objects::nonNull)
          .collect(Collectors.toSet());
    } else {
      result = Optional.ofNullable(campaignProductEnded)
          .map(val -> MainUtil.combineList(toCampaignCodeFromCampaignPriority(val),toCampaignCodeFromCampaignCode(val)))
          .map(MainUtil::fromListToSet)
          .orElseGet(HashSet::new)
          .stream()
          .filter(Objects::nonNull)
          .map(campaignCode -> toCampaignCodeSessionId(campaignCode,null))
          .collect(Collectors.toSet());
    }
    return MainUtil.fromSetToList(result);
  }

  private static List<String> toCampaignCodeFromCampaignPriority(CampaignProductEnded campaignProductEnded) {
    return Optional.ofNullable(campaignProductEnded)
        .map(CampaignProductEnded::getCampaignEndModelList)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CampaignProductEnded.CampaignEnd::getCampaignCode)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static List<String> toCampaignCodeFromCampaignCode(CampaignProductEnded campaignProductEnded) {
    return Optional.ofNullable(campaignProductEnded)
        .map(CampaignProductEnded::getCampaignCodeList)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public static List<String> toCampaignProductIds(CampaignProductRemoved campaignProductRemoved) {
    return Optional.ofNullable(campaignProductRemoved)
        .map(CampaignProductRemoved::getSkuList)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> toCampaignProductId(campaignProductRemoved.getCampaignCode(),val.getItemSku(),val.getPickupPointCode()))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public static String toCheapestPriceDayPickupPointCode(CheapestPriceDay cheapestPriceDay) {
    return Optional.ofNullable(cheapestPriceDay)
        .map(CheapestPriceDay::getPickupPointCode)
        .orElse(null);
  }

  public static String toCampaignProductSku(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::getProductSku)
        .orElse(null);
  }

  public static String toCampaignItemSku(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::getItemSku)
        .orElse(null);
  }

  public static String toCampaignPickupPointCode(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::getPickupPointCode)
        .orElse(null);
  }

  public static String toCampaignCampaignCode(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getCampaignCode)
        .orElse(null);
  }

  public static Integer toCampaignCampaignSessionId(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::getSessionId)
        .orElse(null);
  }

  public static List<String> toCampaignGroupIds(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::getGroupIds)
        .orElse(null);
  }

  public static boolean toCampaignTimeBased(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .map(CampaignProductPublished.ProductSkuEventModel::isTimeBased)
        .orElseGet(() -> false);
  }

  public static long toCampaignPromotionStartTime(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getPromotionStartTime)
        .orElseGet(() -> 0L);
  }

  public static long toCampaignPromotionEndTime(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getPromotionEndTime)
        .orElseGet(() -> 0L);
  }

  public static CampaignProductPublished.ProductSkuEventModel toCampaignSku(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getSku)
        .orElse(null);
  }
  /*End of Campaign Converter*/

  /*Pickup Point Converter*/
  public static Set<PickupPoint.ItemViewConfig> getNecessaryItemViewConfigs(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> Channel.NECESSARY_CHANNELS.contains(val.getChannel()))
        .collect(Collectors.toSet());
  }

  public static Set<PickupPoint.ItemViewConfig> getItemViewConfigs(PickupPoint pickupPoint, String channel) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getItemViewConfigs)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> channel.equals(val.getChannel()))
        .collect(Collectors.toSet());
  }

  public static boolean isItemViewConfigsContains(PickupPoint pickupPoint, String channel) {
    return CollectionUtils.isNotEmpty(getItemViewConfigs(pickupPoint, channel));
  }

  public static boolean getPickupPointWarehouseFlag(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::isWarehouse)
        .orElseGet(() -> false);
  }

  public static String getPickupPointId(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::toId)
        .orElse(null);
  }

  public static String getItemSku(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getItemSku)
        .orElse(null);
  }

  public static String getPickupPointCode(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPickupPointCode)
        .orElse(null);
  }

  public static String getExternalPickupPointCode(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getExternalPickupPointCode)
        .orElse(null);
  }

  public static String getPickupPointMerchantCode(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getMerchantCode)
        .orElse(null);
  }

  public static String getPickupPointCampaignCode(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(PickupPoint.Price::getCampaignCode)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public static String getPickupPointPurchasedType(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPurchasedType)
        .orElse(null);
  }

  public static int getPickupPointPurchasedTypeScore(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPurchasedTypeScore)
        .orElseGet(() -> 0);
  }

  public static int toPickupPointPurchasedTypeScore(PickupPoint pickupPoint) {
    boolean buyable = isPickupPointBuyable(pickupPoint,Channel.DEFAULT);
    boolean discoverable = isPickupPointDiscoverable(pickupPoint,Channel.DEFAULT);
    boolean buyableCnc = isPickupPointBuyable(pickupPoint,Channel.CNC);
    boolean discoverableCnc = isPickupPointDiscoverable(pickupPoint,Channel.CNC);

    int purchasedType = switch (getPickupPointPurchasedType(pickupPoint)) {
      case PurchasedType.ONLINE_CNC -> ONLINE_CNC_SCORE;
      case PurchasedType.ONLINE -> ONLINE_SCORE;
      case PurchasedType.CNC -> CNC_SCORE;
      case PurchasedType.NOT_AVAILABLE -> toPurchasedTypeNAScore(discoverable,discoverableCnc);
      default -> 0;
    };
    int itemViewConfigs = getBooleanScoreAsc(buyable) * BUYABLE_SCORE +
        getBooleanScoreAsc(discoverable) * DISCOVERABLE_SCORE +
        getBooleanScoreAsc(buyableCnc) * BUYABLE_CNC_SCORE +
        getBooleanScoreAsc(discoverableCnc) * DISCOVERABLE_CNC_SCORE;
    return purchasedType + itemViewConfigs;
  }

  public static int toPurchasedTypeNAScore(boolean discoverable, boolean discoverableCnc) {
    int score = NOT_AVAILABLE_SCORE;
    if (discoverable && discoverableCnc) {
      score+=400;//UPCOMING_ONLINE_CNC
    } else if (discoverableCnc) {
      score+=400;//UPCOMING_ONLINE_CNC
    } else if (discoverable) {
      score+=300;//UPCOMING_ONLINE
    }
    return score;
  }

  private static int getBooleanScoreAsc(boolean value) {
    if (value) {
      return 1;
    } else {
      return 0;
    }
  }

  private static int getBooleanScoreDesc(boolean value) {
    if (value) {
      return 0;
    } else {
      return 1;
    }
  }

  public static String getPickupPointCampaignType(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(PickupPoint.Price::getPriority)
        .filter(Objects::nonNull)
        .findFirst()
        .map(ModuleProductUtil::getCampaignType)
        .orElseGet(() -> CampaignType.NON_CAMPAIGN);
  }

  public static double getPickupPointFinalOfferPrice(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(PickupPoint.Price::getFinalOfferPrice)
        .findFirst()
        .orElseGet(() -> 0D);
  }

  public static boolean isAvailableUpdateQueuePickupPoint(PickupPoint pickupPoint, boolean enable, SaveParam saveParam) {
    return Objects.nonNull(pickupPoint) && enable && !ParamUtil.isFullReconstruct(saveParam);
  }

  public static boolean isAvailableDirectUpdateByPickupPoint(PickupPoint pickupPoint, Set<String> availableCampaignTypes, Set<String> restrictedMerchantCodes) {
    if (CollectionUtils.isEmpty(availableCampaignTypes) || CollectionUtils.isEmpty(restrictedMerchantCodes)) {
      return false;
    } else {
      return availableCampaignTypes.contains(getPickupPointCampaignType(pickupPoint)) && !restrictedMerchantCodes.contains(getPickupPointMerchantCode(pickupPoint));
    }
  }

  private static Set<PickupPoint.Price> toOnlinePrice(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new);
  }

  public static Set<PickupPoint.ItemViewConfig> toDefaultItemViewConfigs(boolean buyable, boolean discoverable) {
    PickupPoint.ItemViewConfig itemViewConfig = PickupPoint.ItemViewConfig.builder()
        .buyable(buyable)
        .discoverable(discoverable)
        .build();
    return MainUtil.toSet(itemViewConfig);
  }

  private static Set<PickupPoint.ItemViewConfig> toOnlineItemViewConfigs(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getItemViewConfigs)
        .filter(itemViewConfigs -> !CollectionUtils.isEmpty(itemViewConfigs))
        .orElseGet(() -> toDefaultItemViewConfigs(true,true));
  }

  public static boolean isPickupPointCodeSame(String comparator, String pp) {
    if (StringUtils.isEmpty(comparator)) {
      return true;
    } else {
      return comparator.equals(pp);
    }
  }

  public static boolean isPickupPointFbbActivated(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::isFbbActivated)
        .orElseGet(() -> false);
  }

  public static boolean isPickupPointPromoCampaign(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .findFirst()
        .map(PickupPoint.Price::isPromoCampaign)
        .orElseGet(() -> false);
  }

  public static PickupPoint getPickupPoint(List<PickupPoint> pickupPoints, String pickupPointId) {
    return pickupPoints.stream()
        .filter(Objects::nonNull)
        .filter(val -> val.toId().equals(pickupPointId))
        .findFirst()
        .orElse(null);
  }

  public static Double getLatitude(CustomBusinessPartnerPickupPoint.GeolocationVO geolocation) {
    return Optional.ofNullable(geolocation)
        .map(CustomBusinessPartnerPickupPoint.GeolocationVO::getLatitude)
        .orElse(null);
  }

  public static Double getLongitude(CustomBusinessPartnerPickupPoint.GeolocationVO geolocation) {
    return Optional.ofNullable(geolocation)
        .map(CustomBusinessPartnerPickupPoint.GeolocationVO::getLongitude)
        .orElse(null);
  }

  public static PickupPointLocation toPickupPointLocation(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(val -> PickupPointLocation.builder()
            .pickupPointCode(val.getPickupPointCode())
            .latitude(val.getLatitude())
            .longitude(val.getLongitude())
            .cityName(val.getCityName())
            .build())
        .orElse(null);
  }

  public static PageRequest toNearestPickupPointPageable(int nearestSize) {
    return Optional.of(nearestSize)
        .filter(size -> size>0)
        .map(size -> PageRequest.of(0,size))
        .orElse(null);
  }
  /*End of Pickup Point Converter*/

  /*CalculatedItem Converter*/
  public static boolean canCalculateItem(PickupPoint pickupPoint, boolean ignoreFilter) {
    if (ignoreFilter) {
      return true;
    } else {
      return !pickupPoint.isMarkForDelete() && !PurchasedType.NOT_AVAILABLE.equals(getPickupPointPurchasedType(pickupPoint));
    }
  }

  public static Double getFinalOffered(AdjustmentProduct adjustmentProduct, Double offered) {
    return Optional.ofNullable(offered)
        .map(val -> val - getAdjustmentValue(adjustmentProduct))
        .orElseGet(() -> 0d);
  }

  public static double getAdjustmentValue(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .map(val -> (double) val.getValue())
        .orElseGet(() -> 0d);
  }

  public static boolean isAdjustmentCurrentlyActive(AdjustmentProduct adjustmentProduct) {
    return Optional.ofNullable(adjustmentProduct)
        .filter(AdjustmentProduct::isActivated)
        .filter(val -> isItOnToday(val.getStartDate(),val.getEndDate(),getCurrentTimestamp()))
        .isPresent();
  }

  public static AdjustmentProduct combineCampaignAndNonCampaignAdjustment(AdjustmentProduct campaignAdjustment, List<AdjustmentProduct> nonCampaignAdjustments) {
    return Optional.ofNullable(nonCampaignAdjustments)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(val -> canCombineAdjustments(campaignAdjustment,val))
        .map(AdjustmentProduct::getValue)
        .reduce(Long::sum)
        .map(nonCampaignDiscount -> {
          campaignAdjustment.setValue(campaignAdjustment.getValue()+nonCampaignDiscount);
          return campaignAdjustment;
        })
        .orElseGet(() -> campaignAdjustment);
  }

  private static boolean canCombineAdjustments(AdjustmentProduct campaignAdjustment, AdjustmentProduct nonCampaignAdjustment) {
    return Optional.ofNullable(nonCampaignAdjustment)
        .filter(val -> val.getPriority()==campaignAdjustment.getPriority())
        .filter(val -> !MainUtil.toNotNullString(val.getCampaignCode()).equals(MainUtil.toNotNullString(campaignAdjustment.getCampaignCode())))
        .isPresent();
  }

  public static CalculatedItem.PriceDetail toPriceDetail(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(val -> CalculatedItem.PriceDetail.builder()
            .buyable(toScheduleFlag(val,Channel.DEFAULT,true))
            .discoverable(toScheduleFlag(val,Channel.DEFAULT,false))
            .buyableCnc(toScheduleFlag(val,Channel.CNC,true))
            .discoverableCnc(toScheduleFlag(val,Channel.CNC,false))
            .purchasedType(getPickupPointPurchasedType(val))
            .purchasedTypeScore(getPickupPointPurchasedTypeScore(val))
            .listed(getListed(val))
            .offered(getOffered(val))
            .finalOffered(getOffered(val))
            .build())
        .filter(val -> Objects.nonNull(val.getFinalOffered()))
        .filter(val -> val.getFinalOffered() > 0d)
        .orElse(null);
  }

  public static String toPurchasedType(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getPurchasedType)
        .orElse(null);
  }

  public static int toPurchasedTypeScore(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getPurchasedTypeScore)
        .orElseGet(() -> 0);
  }

  public static CalculatedItem toCheapestCalculatedItem(List<CalculatedItem> calculatedItems, PickupPoint pickupPoint) {
    String pickupPointId = getPickupPointId(pickupPoint);
    if (pickupPointId == null) {
      return null;
    }
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> Objects.nonNull(val.getItemSku()) && Objects.nonNull(val.getPickupPointCode()))
        .filter(val -> {
          String valPickupPointId = toPickupPointId(val.getItemSku(), val.getPickupPointCode());
          return Objects.nonNull(valPickupPointId) && pickupPointId.equals(valPickupPointId);
        })
        .findFirst()
        .orElse(null);
  }

  public static SivaProduct.ViewSchedule toViewSchedule(CalculatedItem calculatedItem, String channel, boolean buyable) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(val -> getScheduleFlag(val,channel,buyable))
        .map(val -> SivaProduct.ViewSchedule.builder()
            .value(val.isValue())
            .start(getTimestamp(val,true))
            .end(getTimestamp(val,false))
            .build())
        .orElseGet(ModuleProductUtil::getDefaultViewSchedule);
  }

  private static CalculatedItem.ScheduleFlag getScheduleFlag(CalculatedItem.PriceDetail price, String channel, boolean buyable) {
    if(buyable) {
      if (Channel.CNC.equals(channel)) {
        return price.getBuyableCnc();
      } else {
        return price.getBuyable();
      }
    } else {
      if (Channel.CNC.equals(channel)) {
        return price.getDiscoverableCnc();
      } else {
        return price.getDiscoverable();
      }
    }
  }

  private static Long getTimestamp(CalculatedItem.ScheduleFlag scheduleFlag, boolean startTime) {
    return Optional.ofNullable(scheduleFlag)
        .map(CalculatedItem.ScheduleFlag::getSchedule)
        .map(viewSchedule -> {
          if (startTime) {
            return viewSchedule.getStart();
          } else {
            return viewSchedule.getEnd();
          }
        })
        .orElse(null);
  }

  public static CalculatedItem.ScheduleFlag toScheduleFlag(PickupPoint pickupPoint, String channel, boolean buyable) {
    return Optional.ofNullable(pickupPoint)
        .map(val -> getItemViewConfigs(val,channel))
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> getScheduleFlag(val,buyable))
        .filter(Objects::nonNull)
        .min(Comparator
            .comparing(CalculatedItem.ScheduleFlag::isValue, Comparator.reverseOrder())
            .thenComparing(val -> Objects.nonNull(val.getSchedule()), Comparator.reverseOrder()))
        .orElseGet(ModuleProductUtil::getDefaultScheduleFlag);
  }

  private static CalculatedItem.ScheduleFlag getScheduleFlag(PickupPoint.ItemViewConfig itemViewConfig, boolean buyable) {
    if (buyable) {
      return Optional.ofNullable(itemViewConfig)
          .map(val -> CalculatedItem.ScheduleFlag.builder()
              .value(val.isBuyable())
              .schedule(getViewSchedule(val,buyable))
              .build())
          .orElse(null);
    } else {
      return Optional.ofNullable(itemViewConfig)
          .map(val -> CalculatedItem.ScheduleFlag.builder()
              .value(val.isDiscoverable())
              .schedule(getViewSchedule(val,buyable))
              .build())
          .orElse(null);
    }
  }

  private static SivaProduct.ViewSchedule getViewSchedule(PickupPoint.ItemViewConfig itemViewConfig, boolean buyable) {
    if (buyable) {
      return Optional.ofNullable(itemViewConfig)
          .map(PickupPoint.ItemViewConfig::getItemBuyableSchedules)
          .map(val -> SivaProduct.ViewSchedule.builder()
              .start(val.getStartDateTime())
              .end(val.getEndDateTime())
              .value(val.isBuyable())
              .build())
          .orElse(null);
    } else {
      return Optional.ofNullable(itemViewConfig)
          .map(PickupPoint.ItemViewConfig::getItemDiscoverableSchedules)
          .map(val -> SivaProduct.ViewSchedule.builder()
              .start(val.getStartDateTime())
              .end(val.getEndDateTime())
              .value(val.isDiscoverable())
              .build())
          .orElse(null);
    }
  }

  public static CalculatedItem.ScheduleFlag getDefaultScheduleFlag() {
    return CalculatedItem.ScheduleFlag.builder()
        .value(false)
        .schedule(getDefaultViewSchedule())
        .build();
  }

  public static SivaProduct.ViewSchedule getDefaultViewSchedule() {
    return SivaProduct.ViewSchedule.builder()
        .value(false)
        .build();
  }

  public static Double getListed(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .map(ModuleProductUtil::getListedPriceFromPrices)
        .orElse(0.0);
  }

  public static Double getOffered(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPrice)
        .map(ModuleProductUtil::getOfferedPriceFromPrices)
        .orElse(0.0);
  }

  public static double getListedPriceFromPrices(Set<PickupPoint.Price> prices) {
    return prices.stream()
        .filter(Objects::nonNull)
        .reduce(0d, (result, price) -> getListPrice(price), (result, price) -> price);
  }

  public static double getListPrice(PickupPoint.Price price) {
    return Optional.ofNullable(price)
        .map(PickupPoint.Price::getListPrice)
        .map(ModuleProductUtil::normalizePrice)
        .orElseGet(() -> 0d);
  }

  public static double normalizePrice(double price) {
    if (price < 0) {
      return 0;
    } else {
      return price;
    }
  }

  public static double getOfferedPriceFromPrices(Set<PickupPoint.Price> prices) {
    return prices.stream()
        .filter(Objects::nonNull)
        .reduce(0d, (result, price) -> getOfferPrice(price), (result, price) -> price);
  }

  public static double getOfferPrice(PickupPoint.Price price) {
    double offeredPrice;
    if (isMerchantDiscountActive(price)) {
      offeredPrice = getOfferPriceFromMerchantDiscountPrice(price);
    } else {
      offeredPrice = getOfferPriceFromDefault(price);
    }
    return normalizePrice(offeredPrice);
  }

  public static boolean isMerchantDiscountActive(PickupPoint.Price price) {
    Long startDateTime = getStartDateTime(price);
    Long endDateTime = getEndDateTime(price);
    Long currentDateTime = getCurrentTimestamp();

    return isItOnToday(startDateTime,endDateTime,currentDateTime);
  }

  private static Long getStartDateTime(PickupPoint.Price price) {
    return Optional.ofNullable(price)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .map(PickupPoint.DiscountPrice::getStartDateTime)
        .orElse(null);
  }

  private static Long getEndDateTime(PickupPoint.Price price) {
    return Optional.ofNullable(price)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .map(PickupPoint.DiscountPrice::getEndDateTime)
        .orElse(null);
  }

  private static double getOfferPriceFromMerchantDiscountPrice(PickupPoint.Price price) {
    return Optional.ofNullable(price)
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .map(PickupPoint.DiscountPrice::getDiscountPrice)
        .orElseGet(() -> 0d);
  }

  private static double getOfferPriceFromDefault(PickupPoint.Price price) {
    return Optional.ofNullable(price)
        .map(PickupPoint.Price::getOfferPrice)
        .orElseGet(() -> 0d);
  }

  public static boolean isPickupPointBuyable(PickupPoint pickupPoint, String channel) {
    return Optional.ofNullable(pickupPoint)
        .map(pp -> toScheduleFlag(pp,channel,true))
        .map(CalculatedItem.ScheduleFlag::isValue)
        .orElseGet(() -> false);
  }

  public static boolean isPickupPointDiscoverable(PickupPoint pickupPoint, String channel) {
    return Optional.ofNullable(pickupPoint)
        .map(pp -> toScheduleFlag(pp,channel,false))
        .map(CalculatedItem.ScheduleFlag::isValue)
        .orElseGet(() -> false);
  }

  public static PickupPoint toUpdatedFlags(PickupPoint pickupPoint, boolean onePartyActivated) {
    return Optional.ofNullable(pickupPoint)
        .map(pp -> {
          pp.setItemViewConfigs(getNecessaryItemViewConfigs(pp));
          pp.setBuyable(isPickupPointBuyable(pp,Channel.DEFAULT));
          pp.setDiscoverable(isPickupPointDiscoverable(pp,Channel.DEFAULT));
          pp.setBuyableCnc(isPickupPointBuyable(pp,Channel.CNC));
          pp.setDiscoverableCnc(isPickupPointDiscoverable(pp,Channel.CNC));
          pp.setPurchasedType(getPurchasedType(pp,onePartyActivated));
          pp.setPurchasedTypeScore(toPickupPointPurchasedTypeScore(pp));
          pp.setCncActive(isCncActivate(pp,onePartyActivated));
          return pp;
        })
        .orElseGet(() -> pickupPoint);
  }

  public static String getDisplay(double maxListPrice) {
    return maxListPrice >= 0 ? CURRENCY + formatter.format(maxListPrice) : "0";
  }

  public static String getPriceRange(double minOfferPrice, double maxOfferPrice) {
    return CURRENCY + formatter.format(Math.floor(minOfferPrice)) + PRICE_SEPARATOR + CURRENCY + formatter.format(Math.floor(maxOfferPrice));
  }

  public static String getStrikeTroughPriceDisplay(double maxListPrice, double minOfferPrice, double maxOfferPrice, Integer discount) {
    if (Objects.nonNull(discount) && Double.compare(maxListPrice,minOfferPrice)!=0 && Double.compare(minOfferPrice,maxOfferPrice)==0) {
      return getDisplay(maxListPrice);
    }
    return MainUtil.DEFAULT_STRING;
  }

  public static Double getListValue(double minListPrice, Integer discount) {
    if (Objects.nonNull(discount)) {
      return minListPrice;
    }
    return null;
  }

  public static String getPriceDisplay(double minOfferPrice, double maxOfferPrice) {
    if (Double.compare(minOfferPrice,maxOfferPrice)!=0) {
      return getPriceRange(minOfferPrice, maxOfferPrice);
    } else {
      return getDisplay(maxOfferPrice);
    }
  }

  public static SivaProduct.PriceSchedule toAdjustment(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(val -> SivaProduct.PriceSchedule.builder()
            .itemSku(val.getItemSku())
            .pickupPointCode(val.getPickupPointCode())
            .externalPickupPointCode(val.getExternalPickupPointCode())
            .adjustmentName(val.getAdjustmentName())
            .campaignCode(toRealCampaignCode(val.getCampaignCode()))
            .priority(val.getPriority())
            .start(val.getStart())
            .end(val.getEnd())
            .value(val.getValue())
            .activated(val.isActivated())
            .build())
        .orElse(null);
  }

  public static Integer toDiscount(double total, double value) {
    try {
      double discount = (Math.abs(value-total)*100)/total;
      if (discount<1) {
        return null;
      } else {
        if (discount<=99) {
          return BigDecimal.valueOf(discount).setScale(0,RoundingMode.HALF_UP).intValue();
        } else {
          return BigDecimal.valueOf(discount).setScale(0,RoundingMode.FLOOR).intValue();
        }
      }
    } catch (Exception e) {
      LoggerUtil.sendErrorExecuteLog(e,"toDiscount");
      return null;
    }
  }

  public static Integer toCheapestPriceDays(CalculatedItem calculatedItem, boolean forCampaignInfo) {
    return Optional.ofNullable(calculatedItem)
        .filter(val -> forCampaignInfo || val.isCurrentlyActive())
        .map(CalculatedItem::getCheapestPriceDays)
        .orElse(null);
  }

  private static String getPurchasedType1PDisabled(PickupPoint pickupPoint) {
    boolean buyable = isPickupPointBuyable(pickupPoint,Channel.DEFAULT);
    if (pickupPoint.isCncActive()) {
      return getOfflinePurchasedType(buyable);
    } else {
      return getOnlinePurchasedType(buyable);
    }
  }

  private static String getOfflinePurchasedType(boolean buyable) {
    if (buyable) {
      return PurchasedType.ONLINE_CNC;
    } else {
      return PurchasedType.CNC;
    }
  }

  private static String getOnlinePurchasedType(boolean buyable) {
    if (buyable) {
      return PurchasedType.ONLINE;
    } else {
      return PurchasedType.NOT_AVAILABLE;
    }
  }

  public static String getPurchasedType1PEnabled(PickupPoint pickupPoint) {
    String buyable = Boolean.valueOf(isPickupPointBuyable(pickupPoint,Channel.DEFAULT)).toString().toUpperCase();
    String discoverable = Boolean.valueOf(isPickupPointDiscoverable(pickupPoint,Channel.DEFAULT)).toString().toUpperCase();
    String buyableCnc = Boolean.valueOf(isPickupPointBuyable(pickupPoint,Channel.CNC)).toString().toUpperCase();
    String discoverableCnc = Boolean.valueOf(isPickupPointDiscoverable(pickupPoint,Channel.CNC)).toString().toUpperCase();

    try {
      String name = String.join("_",buyable,discoverable,buyableCnc,discoverableCnc);
      return PurchasedTypeMap.class.getField(name).get(name).toString();
    } catch (Exception e) {
      return PurchasedType.NOT_AVAILABLE;
    }
  }

  public static String getPurchasedType(PickupPoint pickupPoint, boolean onePartyActivated) {
    if (!onePartyActivated) {
      return getPurchasedType1PDisabled(pickupPoint);
    } else {
      return getPurchasedType1PEnabled(pickupPoint);
    }
  }

  private static boolean isCncActivate1PDisabled(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::isCncActive)
        .orElseGet(() -> false);
  }

  private static boolean isCncActivate1PEnabled(PickupPoint pickupPoint) {
    return Optional.ofNullable(pickupPoint)
        .map(PickupPoint::getPurchasedType)
        .map(purchasedType -> PurchasedType.CNC.equals(purchasedType) || PurchasedType.ONLINE_CNC.equals(purchasedType))
        .orElseGet(() -> false);
  }

  public static boolean isCncActivate(PickupPoint pickupPoint, boolean onePartyActivated) {
    if (!onePartyActivated) {
      return isCncActivate1PDisabled(pickupPoint);
    } else {
      return isCncActivate1PEnabled(pickupPoint);
    }
  }
  public static int getPurchasedTypeScore(PickupPoint pickupPoint) {
    String purchasedType = pickupPoint.getPurchasedType();
    int score;

    if (PurchasedType.ONLINE_CNC.equals(purchasedType)) {
      score = ONLINE_CNC_SCORE;
    } else if (PurchasedType.ONLINE.equals(purchasedType)) {
      score = ONLINE_SCORE;
    } else if (PurchasedType.CNC.equals(purchasedType)) {
      score = CNC_SCORE;
    } else {
      score = NOT_AVAILABLE_SCORE;
    }

    if (log.isDebugEnabled()) {
      log.debug("Pickup point type: {}, Score: {}, ItemSku: {}",
          purchasedType, score, pickupPoint.getItemSku());
    }

    return score;
  }
  /*End of CalculatedItem Converter*/

  /*CampaignInfos Converter*/
  public static boolean campaignInfoKeyValid(String campaignCode, Integer sessionId) {
    if (Default.CAMPAIGN_CODE.equals(campaignCode)) {
      return Objects.isNull(sessionId);
    } else {
      return true;
    }
  }

  public static String toCampaignInfoKey(String campaignCode, Integer sessionId) {
    if (Default.CAMPAIGN_CODE.equals(campaignCode)) {
      return campaignCode;
    } else {
      List<String> ids = MainUtil.toList(campaignCode,MainUtil.fromIntegerToString(sessionId));
      return MainUtil.toId(MainUtil.toCleanList(ids));
    }
  }

  public static SivaProduct.FinalPrice toCampaignInfoFinalPrice(List<CalculatedItem> calculatedItems, String campaignCode, boolean cnc) {
    return Optional.ofNullable(calculatedItems)
        .filter(org.apache.commons.collections.CollectionUtils::isNotEmpty)
        .map(vals -> toAvailableCalculatedItems(vals,campaignCode,cnc))
        .filter(org.apache.commons.collections.CollectionUtils::isNotEmpty)
        .map(vals -> {
          CalculatedItem minList = vals.stream()
              .min(Comparator
                  .<CalculatedItem,Double>comparing(ModuleProductUtil::toListed)
                  .thenComparing(CalculatedItem::getFlashsale,Comparator.reverseOrder()))
              .orElse(null);
          CalculatedItem minBaseOffer = vals.stream()
              .min(Comparator
                  .<CalculatedItem,Double>comparing(ModuleProductUtil::toOffered)
                  .thenComparing(CalculatedItem::getFlashsale,Comparator.reverseOrder()))
              .orElse(null);
          CalculatedItem minFinalOffer = vals.stream()
              .min(Comparator
                  .<CalculatedItem,Double>comparing(ModuleProductUtil::toFinalOffered)
                  .thenComparing(CalculatedItem::getFlashsale,Comparator.reverseOrder()))
              .orElse(null);
          CalculatedItem maxFinalOffer = vals.stream()
              .min(Comparator
                  .<CalculatedItem,Double>comparing(ModuleProductUtil::toFinalOffered,Comparator.reverseOrder())
                  .thenComparing(CalculatedItem::getFlashsale,Comparator.reverseOrder()))
              .orElse(null);

          double minListPrice = Optional.ofNullable(minList)
              .map(ModuleProductUtil::toListed)
              .orElseGet(() -> 0d);
          double minBaseOfferPrice = Optional.ofNullable(minBaseOffer)
              .map(ModuleProductUtil::toOffered)
              .orElseGet(() -> 0d);
          double minFinalOfferPrice = Optional.ofNullable(minFinalOffer)
              .map(ModuleProductUtil::toFinalOffered)
              .orElseGet(() -> 0d);
          double maxFinalOfferPrice = Optional.ofNullable(maxFinalOffer)
              .map(ModuleProductUtil::toFinalOffered)
              .orElseGet(() -> 0d);
          String cheapestPurchasedType = Optional.ofNullable(minFinalOffer)
              .map(CalculatedItem::getPurchasedType)
              .orElse(null);
          int cheapestPurchasedTypeScore = Optional.ofNullable(minFinalOffer)
              .map(CalculatedItem::getPurchasedTypeScore)
              .orElseGet(() -> 0);
          String cheapestItemSku = Optional.ofNullable(minFinalOffer)
              .map(CalculatedItem::getItemSku)
              .orElse(null);
          String cheapestPickupPointCode = Optional.ofNullable(minFinalOffer)
              .map(CalculatedItem::getPickupPointCode)
              .orElse(null);
          String cheapestExternalPickupPointCode = Optional.ofNullable(minFinalOffer)
              .map(CalculatedItem::getExternalPickupPointCode)
              .orElse(null);
          Integer discount = toDiscount(minListPrice,minFinalOfferPrice);

          return SivaProduct.FinalPrice.builder()
              .list(getStrikeTroughPriceDisplay(minListPrice,minFinalOfferPrice,maxFinalOfferPrice,discount))
              .offer(getPriceDisplay(minFinalOfferPrice,maxFinalOfferPrice))
              .adjustment(toAdjustment(minFinalOffer))
              .listValue(getListValue(minListPrice,discount))
              .baseOfferValue(minBaseOfferPrice)
              .offerValue(minFinalOfferPrice)
              .minOfferValue(minFinalOfferPrice)
              .maxOfferValue(maxFinalOfferPrice)
              .discount(discount)
              .uniqueId(null)
              .purchasedType(cheapestPurchasedType)
              .purchasedTypeScore(cheapestPurchasedTypeScore)
              .cheapestItemSku(cheapestItemSku)
              .cheapestPickupPointCode(cheapestPickupPointCode)
              .cheapestExternalPickupPointCode(cheapestExternalPickupPointCode)
              .cheapestPriceDays(toCheapestPriceDays(minFinalOffer,true))
              .build();
        })
        .orElse(null);
  }

  public static List<CalculatedItem> toAvailableCalculatedItems(List<CalculatedItem> unsortedCalculatedItems, String campaignCode, boolean cnc) {
    List<CalculatedItem> calculatedItems = toCalculatedItems(unsortedCalculatedItems,campaignCode,cnc);
    //consider priority value
    List<CalculatedItem> result = toPriority(calculatedItems);
    //consider buyable & stock flag
    if (CollectionUtils.isEmpty(result)) {
      result = toAvailables(calculatedItems);
    }
    //consider stock
    if (CollectionUtils.isEmpty(result)) {
      result = toHasStock(calculatedItems);
    }
    //consider any non empty
    if (CollectionUtils.isEmpty(result)) {
      result = toAny(calculatedItems);
    }
    return result;
  }

  private static List<CalculatedItem> toCalculatedItems(List<CalculatedItem> calculatedItems, String campaignCode, boolean cnc) {
    return Optional.ofNullable(toFilteredCalculatedItems(calculatedItems,cnc,false))
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .orElseGet(() -> toFilteredCalculatedItems(calculatedItems,cnc,true))
        .stream()
        .filter(val -> MainUtil.toNotNullString(val.getCampaignCode()).equalsIgnoreCase(MainUtil.toNotNullString(campaignCode)))
        .filter(ModuleProductUtil::priceExists)
        .sorted(Comparator
            .<CalculatedItem,Boolean>comparing(ModuleProductUtil::isAvailable, Comparator.reverseOrder())
            .thenComparing(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()), Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toCalculatedItemPriority)
            .thenComparing(CalculatedItem::isActivated, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toCalculatedItemPurchasedTypeScore, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toFinalOffered)
            .thenComparing(ModuleProductUtil::toOffered)
            .thenComparing(CalculatedItem::getItemSku))
        .collect(Collectors.toList());
  }

  private static List<CalculatedItem> toFilteredCalculatedItems(List<CalculatedItem> calculatedItems, boolean cnc, boolean na) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> {
          if (na) {
            return MainUtil.toList(PurchasedType.NOT_AVAILABLE).contains(val.getPurchasedType());
          } else if (cnc) {
            return MainUtil.toList(PurchasedType.CNC, PurchasedType.ONLINE_CNC).contains(val.getPurchasedType());
          } else if (!cnc) {
            return MainUtil.toList(PurchasedType.ONLINE, PurchasedType.ONLINE_CNC).contains(val.getPurchasedType());
          } else {
            return false;
          }
        })
        .collect(Collectors.toList());
  }

  private static boolean priceExists(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .isPresent();
  }

  public static boolean toMarkForDelete(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::isMarkForDelete)
        .orElseGet(() -> false);
  }

  public static boolean toCncActivated(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::isCncActivated)
        .orElseGet(() -> false);
  }

  public static boolean toBuyable(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getBuyable)
        .map(CalculatedItem.ScheduleFlag::isValue)
        .orElseGet(() -> false);
  }

  public static boolean toDiscoverable(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getDiscoverable)
        .map(CalculatedItem.ScheduleFlag::isValue)
        .orElseGet(() -> false);
  }

  public static Double toListed(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getListed)
        .orElse(null);
  }

  public static Double toOffered(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getOffered)
        .orElse(null);
  }

  public static Double toFinalOffered(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPrice)
        .map(CalculatedItem.PriceDetail::getFinalOffered)
        .orElse(null);
  }

  private static List<CalculatedItem> toPriority(List<CalculatedItem> calculatedItems) {
    return calculatedItems.stream()
        .filter(Objects::nonNull)
        .filter(val -> toCalculatedItemPriority(val)!=DEFAULT_PRIORITY)
        .findFirst()
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private static List<CalculatedItem> toAvailables(List<CalculatedItem> calculatedItems) {
    return calculatedItems.stream()
        .filter(CalculatedItem::getAvailable)
        .findFirst()
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private static List<CalculatedItem> toHasStock(List<CalculatedItem> calculatedItems) {
    return calculatedItems.stream()
        .filter(val -> isStockAvailable(val.getStock()))
        .findFirst()
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  private static List<CalculatedItem> toAny(List<CalculatedItem> calculatedItems) {
    return calculatedItems.stream()
        .findFirst()
        .map(MainUtil::toList)
        .orElseGet(ArrayList::new);
  }

  public static long toBaseCurrentDate(Long campaignStartTime, boolean forNextPriceTeaser) {
    return Optional.ofNullable(campaignStartTime)
        .filter(val -> forNextPriceTeaser)
        .map(val -> val + (10 * 60 * 1000))
        .orElseGet(ModuleProductUtil::getCurrentTimestamp);
  }

  public static String getCheapestL5IdForFullReconstructBySivaProduct(SivaProduct sivaProduct) {
    return Optional.ofNullable(sivaProduct)
        .map(val -> Optional.ofNullable(toPickupPointId(getCheapestItemSkuFinalPrice(val.getPrice()),getCheapestPickupPointCodeFinalPrice(val.getPrice())))
            .orElseGet(() -> toPickupPointId(MainUtil.toListFirstData(val.getItemSkus()),val.getPickupPointCode())))
        .orElse(null);
  }

  public static String getCheapestL5IdForFullReconstructBySivaItem(SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(val -> Optional.ofNullable(toPickupPointId(getCheapestItemSkuFinalPrice(val.getPrice()),getCheapestPickupPointCodeFinalPrice(val.getPrice())))
            .orElseGet(() -> toPickupPointId(val.getItemSku(),val.getPickupPointCode())))
        .orElse(null);
  }

  public static String getCheapestItemSkuFromSivaProduct(SivaProduct sivaProduct, boolean cnc) {
    return Optional.ofNullable(sivaProduct)
        .map(val -> (cnc) ? getCheapestItemSkuFinalPrice(val.getOfflinePrice()) : getCheapestItemSkuFinalPrice(val.getPrice()))
        .orElse(null);
  }

  public static String getCheapestItemSkuFinalPrice(SivaProduct.FinalPrice price) {
    return Optional.ofNullable(price)
        .map(SivaProduct.FinalPrice::getCheapestItemSku)
        .orElse(null);
  }

  public static String getCheapestPickupPointCodeFinalPrice(SivaProduct.FinalPrice price) {
    return Optional.ofNullable(price)
        .map(SivaProduct.FinalPrice::getCheapestPickupPointCode)
        .orElse(null);
  }

  public static String getCheapestItemSku(SivaProduct.FinalPrice price, List<String> itemSkus) {
    return Optional.ofNullable(getCheapestItemSkuFinalPrice(price))
        .orElseGet(() -> MainUtil.toListFirstData(itemSkus));
  }

  public static String getCheapestPickupPointCode(SivaProduct.FinalPrice price, List<String> pickupPointCodes) {
    return Optional.ofNullable(getCheapestPickupPointCodeFinalPrice(price))
        .orElseGet(() -> MainUtil.toListFirstData(pickupPointCodes));
  }

  public static String getRealCampaignCodeFinalPrice(SivaProduct.FinalPrice price) {
    return Optional.ofNullable(price)
        .map(SivaProduct.FinalPrice::getAdjustment)
        .map(SivaProduct.PriceSchedule::getCampaignCode)
        .map(ModuleProductUtil::toRealCampaignCode)
        .orElse(null);
  }

  public static String getCheapestItemSkuFromSivaItem(SivaItem sivaItem, boolean cnc) {
    return Optional.ofNullable(sivaItem)
        .map(val -> (cnc) ? getCheapestItemSkuFinalPrice(val.getOfflineItemPrice()) : getCheapestItemSkuPrice(val.getItemPrice()))
        .orElse(null);
  }

  public static String getCheapestItemSkuPrice(CalculatedItem.Price price) {
    return Optional.ofNullable(price)
        .map(CalculatedItem.Price::getCheapestItemSku)
        .orElse(null);
  }

  public static List<String> toCampaignItemSkus(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .map(vals -> vals.stream()
            .filter(Objects::nonNull)
            .map(CalculatedItem::getItemSku)
            .collect(Collectors.toSet()))
        .map(ArrayList::new)
        .orElseGet(ArrayList::new);
  }

  public static List<String> toCampaignPickupPointCodes(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .map(vals -> vals.stream()
            .filter(Objects::nonNull)
            .map(CalculatedItem::getPickupPointCode)
            .collect(Collectors.toSet()))
        .map(ArrayList::new)
        .orElseGet(ArrayList::new);
  }

  public static String toCampaignName(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getCampaignName)
        .orElse(null);
  }

  public static String toCampaignTagLabel(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::getTagLabel)
        .orElse(null);
  }

  public static String toCampaignCampaignCode(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CalculatedItem::getCampaignCode)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public static Integer toCampaignSessionId(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CalculatedItem::getSessionId)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public static List<String> toCampaignFlashsaleItemSkus(List<String> flashsaleItemSkus, Flashsale flashsale) {
    return Optional.ofNullable(flashsale)
        .map(val -> flashsaleItemSkus)
        .orElseGet(ArrayList::new);
  }

  public static String toCampaignCampaignCode(String campaignCode, Flashsale flashsale) {
    return Optional.ofNullable(flashsale)
        .map(Flashsale::getCampaignCode)
        .orElseGet(() -> campaignCode);
  }

  public static String toCampaignImage(Item cheapestItem) {
    return Optional.ofNullable(cheapestItem)
        .map(ModuleProductUtil::toItemImages)
        .map(ModuleProductUtil::toMainImageLocationPath)
        .orElse(null);
  }

  public static List<MasterData.ImageDomainEventModel> toItemImages(Item item) {
    return Optional.ofNullable(item)
        .map(Item::getMasterDataItem)
        .map(MasterDataItem::getMasterDataItemImages)
        .orElseGet(ArrayList::new)
        .stream()
        .map(ModuleProductUtil::toImageDomainEventModel)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(img -> MainUtil.toNotNullString(img.getLocationPath())))
        .collect(Collectors.toList());
  }

  public static MasterData.ImageDomainEventModel toImageDomainEventModel(MasterDataItem.MasterDataItemImage itemImage) {
    return Optional.ofNullable(itemImage)
        .map(val -> MasterData.ImageDomainEventModel.builder()
            .mainImage(val.isMainImage())
            .locationPath(val.getLocationPath())
            .sequence(val.getSequence())
            .build())
        .orElse(null);
  }

  public static String toMainImageLocationPath(List<MasterData.ImageDomainEventModel> images) {
    return Optional.ofNullable(images)
        .filter(org.apache.commons.collections.CollectionUtils::isNotEmpty)
        .map(vals -> vals.stream()
            .filter(Objects::nonNull)
            .sorted(Comparator
                .comparing(MasterData.ImageDomainEventModel::isMainImage,Comparator.reverseOrder())
                .thenComparingInt(MasterData.ImageDomainEventModel::getSequence))
            .map(MasterData.ImageDomainEventModel::getLocationPath)
            .findFirst()
            .orElse(null))
        .orElse(null);
  }

  public static Long toCampaignStart(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CalculatedItem::getStart)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public static Long toCampaignEnd(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(CalculatedItem::getEnd)
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  public static boolean isCampaignActivated(List<CalculatedItem> calculatedItems) {
    return Optional.ofNullable(calculatedItems)
        .orElseGet(ArrayList::new)
        .stream()
        .anyMatch(CalculatedItem::isActivated);
  }
  /*End of CampaignInfos Converter*/

  /*FlashsaleInventory Converter */
  public static FlashsaleInventory toFlashsaleInventory(Flashsale flashsale, List<Quota> adjustments, Stock inventory) {
    return FlashsaleInventory.builder()
        .quota(toFlashsaleQuota(toFlashsaleStockMultiple(adjustments,inventory)))
        .schedule(toFlashsaleSchedule(flashsale))
        .build();
  }

  private static FlashsaleInventory.FlashsaleQuota toFlashsaleQuota(Stock stock) {
    return Optional.ofNullable(stock)
        .map(val -> FlashsaleInventory.FlashsaleQuota.builder()
            .itemSku(val.getItemSku())
            .pickupPointCode(val.getPickupPointCode())
            .percentage(toPercentage(val.getQuota(),val.getRemaining()))
            .status(convertStockStatus(val.getRemaining()))
            .build())
        .orElse(null);
  }

  public static Stock toFlashsaleStockMultiple(List<Quota> adjustments, Stock stock) {
    Quota adjustment = findMaxAdjustment(adjustments);
    if(isSafeToCalculateFlashsaleStock(adjustment,stock) && isStockAvailable(stock)) {
      return toFlashsaleStockSingle(adjustment,stock);
    } else {
      String itemSku = MainUtil.getOrDefault(getItemSkuFromQuota(adjustment),getItemSkuFromStock(stock));
      String pickupPointCode = MainUtil.getOrDefault(getPickupPointCodeFromQuota(adjustment),getPickupPointCodeFromStock(stock));
      return ModuleProductUtil.initializeStock(itemSku,pickupPointCode);
    }
  }

  public static Stock toFlashsaleStockSingle(Quota adjustment, Stock stock) {
    if (adjustment.getRemaining() <= stock.getRemaining()) {
      return fromQuotaRemainingToStock(adjustment.getItemSku(),adjustment.getPickupPointCode(),adjustment.getQuota(),adjustment.getRemaining());
    } else {
      return fromQuotaRemainingToStock(stock.getItemSku(),stock.getPickupPointCode(),stock.getQuota(),stock.getRemaining());
    }
  }

  public static Stock fromQuotaRemainingToStock(String itemSku, String pickupPointCode, int quota, int remaining) {
    Stock flashsaleStock = Stock.builder()
        .itemSku(itemSku)
        .pickupPointCode(pickupPointCode)
        .quota(quota)
        .remaining(remaining)
        .exists(true)
        .build();
    flashsaleStock.setStatus(convertStockStatus(flashsaleStock.getRemaining()));
    return flashsaleStock;
  }

  public static FlashsaleInventory.FlashsaleSchedule toFlashsaleSchedule(Flashsale flashsale) {
    return Optional.ofNullable(flashsale)
        .map(val -> FlashsaleInventory.FlashsaleSchedule.builder()
            .id(val.getScheduleId())
            .end(val.getEnd())
            .start(val.getStart())
            .build())
        .orElse(null);
  }

  public static FlashsaleInventory toSafeFlashsaleInventory(Flashsale flashsale, Quota topPerformerAdjustment, Quota cheapestAdjustment, Stock topPerformerStock, Stock cheapestStock) {
    return FlashsaleInventory.builder()
        .quota(toFlashsaleQuota(toSafeFlashsaleStock(topPerformerAdjustment, cheapestAdjustment, topPerformerStock, cheapestStock)))
        .schedule(toFlashsaleSchedule(flashsale))
        .build();
  }

  public static Stock toSafeFlashsaleStock(Quota topPerformerAdjustment, Quota cheapestAdjustment, Stock topPerformerStock, Stock cheapestStock) {
    if(!isSafeToCalculateFlashsaleStock(topPerformerAdjustment,topPerformerStock)) {
      return toSafeFlashsaleStockFromCheapest(cheapestAdjustment, cheapestStock);
    }

    if(!isStockAvailable(topPerformerStock)) {
      return toSafeFlashsaleStockFromCheapest(cheapestAdjustment, cheapestStock);
    }

    return toFlashsaleStockSingle(topPerformerAdjustment,topPerformerStock);
  }

  public static Stock toSafeFlashsaleStockFromCheapest(Quota cheapestAdjustment, Stock cheapestStock) {
    if(!isSafeToCalculateFlashsaleStock(cheapestAdjustment,cheapestStock)) {
      return ModuleProductUtil.initializeStock(null, null);
    }

    if(!isStockAvailable(cheapestStock)) {
      String itemSku = MainUtil.getOrDefault(getItemSkuFromQuota(cheapestAdjustment),getItemSkuFromStock(cheapestStock));
      String pickupPointCode = MainUtil.getOrDefault(getPickupPointCodeFromQuota(cheapestAdjustment),getPickupPointCodeFromStock(cheapestStock));
      return ModuleProductUtil.initializeStock(itemSku,pickupPointCode);
    }

    return toFlashsaleStockSingle(cheapestAdjustment,cheapestStock);
  }

  public static boolean isSafeToCalculateFlashsaleStock(Quota adjustment, Stock stock) {
    return Objects.nonNull(adjustment) && Objects.nonNull(stock);
  }

  public static List<Quota> toSafeAdjustments(FlashsaleInventory flashsaleInventory, List<Quota> topPerformerAdjustments, PickupPoint cheapestPickupPoint, List<Quota> cheapestAdjustments) {
    return Optional.ofNullable(flashsaleInventory)
        .map(FlashsaleInventory::getQuota)
        .filter(quota -> Objects.nonNull(quota.getItemSku()))
        .filter(quota -> Objects.nonNull(quota.getPickupPointCode()))
        .filter(quota -> quota.getItemSku().equals(cheapestPickupPoint.getItemSku()))
        .filter(quota -> quota.getPickupPointCode().equals(cheapestPickupPoint.getPickupPointCode()))
        .map(fa -> cheapestAdjustments)
        .orElse(topPerformerAdjustments);
  }

  public static int toComparingPercentage(List<Quota> quotas) {
    return Optional.ofNullable(quotas)
        .map(ModuleProductUtil::findMaxAdjustment)
        .map(val -> toPercentage(val.getQuota(),val.getRemaining()))
        .filter(val -> val!=0)
        .orElseGet(() -> 999);
  }

  public static int toPercentage(int originalStock, int currentStock) {
    return originalStock > 0 ? (int) (Math.ceil(currentStock * 100.0 / originalStock)) : 0;
  }

  public static boolean isWarehouse(String pickupPointType) {
    return InventoryType.TYPE_ONLINE_WAREHOUSE.equals(pickupPointType);
  }

  public static String toPickupPointType(CustomInventoryPickupPointInfo inventoryPickupPointInfo) {
    return Optional.ofNullable(inventoryPickupPointInfo)
        .map(CustomInventoryPickupPointInfo::getWarehouseInfos)
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .map(val -> InventoryType.TYPE_ONLINE_WAREHOUSE)
        .orElseGet(() -> InventoryType.TYPE_ONLINE_MERCHANT);
  }

  public static String convertStockStatus(int stock) {
    if (stock >= StockStatus.QTY_AVAILABLE) {
      return StockStatus.AVAILABLE;
    } else if (stock > StockStatus.QTY_OOS) {
      return StockStatus.LIMITED;
    } else {
      return StockStatus.OOS;
    }
  }

  public static boolean isPickupPointInStock(Stock stock) {
    return Optional.ofNullable(stock)
        .map(Stock::getStatus)
        .map(val -> !StockStatus.OOS.equals(val))
        .orElseGet(() -> false);
  }

  public static boolean isPickupPointInQuota(Quota quota) {
    return Optional.ofNullable(quota)
        .map(Quota::getRemaining)
        .map(ModuleProductUtil::convertStockStatus)
        .map(val -> !StockStatus.OOS.equals(val))
        .orElseGet(() -> false);
  }

  public static boolean isFlashsaleTimeBased(Flashsale flashsale) {
    return Optional.ofNullable(flashsale)
        .filter(Flashsale::isTimeBased)
        .isPresent();
  }

  public static String getFlashsaleType(Flashsale flashsale) {
    return Optional.ofNullable(flashsale)
        .map(Flashsale::getType)
        .orElseGet(() -> ProductType.REGULAR);
  }

  public static Quota findMaxAdjustment(List<Quota> adjustments) {
    return Optional.ofNullable(adjustments)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .max(Comparator.comparingInt(Quota::getRemaining))
        .orElse(null);
  }

  public static Quota findAdjustmentByL5(List<Quota> adjustments, PickupPoint pickupPoint) {
    return Optional.ofNullable(adjustments)
      .orElseGet(ArrayList::new)
      .stream()
      .filter(Objects::nonNull)
      .filter(quota -> pickupPoint.getItemSku().equals(quota.getItemSku()))
      .filter(quota -> pickupPoint.getPickupPointCode().equals(quota.getPickupPointCode()))
      .max(Comparator.comparingInt(Quota::getRemaining))
      .orElse(null);
  }

  public static String getItemSkuFromQuota(Quota quota) {
    return Optional.ofNullable(quota)
      .map(Quota::getItemSku)
      .orElse(null);
  }

  public static String getPickupPointCodeFromQuota(Quota quota) {
    return Optional.ofNullable(quota)
      .map(Quota::getPickupPointCode)
      .orElse(null);
  }

  public static String getItemSkuFromStock(Stock stock) {
    return Optional.ofNullable(stock)
        .map(Stock::getItemSku)
        .orElse(null);
  }

  public static String getPickupPointCodeFromStock(Stock stock) {
    return Optional.ofNullable(stock)
        .map(Stock::getPickupPointCode)
        .orElse(null);
  }
  /*End of FlashsaleInventory Converter*/

  /*Processed Converter*/
  public static boolean isProductAndItemsExists(Product product, List<Item> items) {
    return Objects.nonNull(product) && !CollectionUtils.isEmpty(items);
  }

  public static String toDefaultCampaignInfoKey(Map<String, CampaignInfo> campaignInfos,
      boolean getDefaultCampaignInfoUsingV2) {
    Long currentDate = getCurrentTimestamp();
    return getDefaultCampaignInfoUsingV2 ?
        getDefaultCampaignInfoV2(campaignInfos, currentDate) :
        getDefaultCampaignInfoV1(campaignInfos, currentDate);
  }

  private static String getDefaultCampaignInfoV1(Map<String, CampaignInfo> campaignInfos, Long currentDate) {
    return campaignInfos.values().stream().filter(Objects::nonNull)
        .min(Comparator.<CampaignInfo, Boolean>comparing(val -> isItOnToday(val.getStartTime(), val.getEndTime(), currentDate), Comparator.reverseOrder())
            .thenComparing(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()), Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toCampaignInfoPriority)
            .thenComparing(CampaignInfo::isActivated, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toPurchasedTypeScore, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toOfferValue).thenComparing(ModuleProductUtil::toBaseOfferValue))
        .map(val -> toCampaignInfoKey(val.getCampaignCode(), val.getSessionId()))
        .orElseGet(() -> Default.CAMPAIGN_CODE);
  }

  private static String getDefaultCampaignInfoV2(Map<String, CampaignInfo> campaignInfos, Long currentDate) {
    return campaignInfos.values().stream().filter(Objects::nonNull)
        .min(Comparator.<CampaignInfo, Boolean>comparing(val -> Optional.ofNullable(val.getBuyable()).map(SivaProduct.ViewSchedule::isValue).orElse(false),
                Comparator.reverseOrder())
            .thenComparing(val -> Optional.ofNullable(val.getBuyableCnc()).map(SivaProduct.ViewSchedule::isValue).orElse(false),
                Comparator.reverseOrder())
            .thenComparing(val -> Optional.ofNullable(val.getInventory()).map(Stock::getStatus)
                .map(status -> !StockStatus.OOS.equals(status)).orElse(false), Comparator.reverseOrder())
            .thenComparing(val -> isItOnToday(val.getStartTime(), val.getEndTime(), currentDate), Comparator.reverseOrder())
            .thenComparing(val -> !Default.CAMPAIGN_CODE.equals(val.getCampaignCode()), Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toCampaignInfoPriority)
            .thenComparing(CampaignInfo::isActivated, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toPurchasedTypeScore, Comparator.reverseOrder())
            .thenComparing(ModuleProductUtil::toOfferValue).thenComparing(ModuleProductUtil::toBaseOfferValue))
        .map(val -> toCampaignInfoKey(val.getCampaignCode(), val.getSessionId()))
        .orElseGet(() -> Default.CAMPAIGN_CODE);
  }

  public static Integer toCampaignInfoPriority(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getAdjustment)
        .map(SivaProduct.PriceSchedule::getPriority)
        .orElseGet(() -> DEFAULT_PRIORITY);
  }

  public static Integer toCalculatedItemPriority(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPriority)
        .orElseGet(() -> DEFAULT_PRIORITY);
  }

  public static Integer toCalculatedItemPurchasedTypeScore(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPurchasedTypeScore)
        .orElseGet(() -> 0);
  }

  public static Integer toCalculatedItemSequence(CalculatedItem calculatedItem) {
    return Optional.ofNullable(calculatedItem)
        .map(CalculatedItem::getPriority)
        .orElseGet(() -> DEFAULT_SEQUENCE);
  }

  public static Integer toSafePriority(Integer priority) {
    return Optional.ofNullable(priority)
        .orElseGet(() -> DEFAULT_PRIORITY);
  }

  public static Integer toSafeSequence(Integer sequence) {
    return Optional.ofNullable(sequence)
        .orElseGet(() -> DEFAULT_SEQUENCE);
  }

  private static Double toOfferValue(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getOfferValue)
        .orElseGet(() -> 0d);
  }

  private static Double toBaseOfferValue(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getBaseOfferValue)
        .orElseGet(() -> 0d);
  }

  private static Integer toPurchasedTypeScore(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getPurchasedTypeScore)
        .orElseGet(() -> 0);
  }

  public static SivaProduct.FinalPrice toFinalPrice(CampaignInfo campaignInfo, boolean cnc) {
    return Optional.ofNullable(campaignInfo)
        .map(val -> (cnc) ? val.getOfflinePrice() : val.getPrice())
        .map(val -> {
          if (!isItOnToday(campaignInfo.getStartTime(),campaignInfo.getEndTime(),getCurrentTimestamp())) {
            val.setCheapestPriceDays(null);
          }
          return val;
        })
        .filter(val -> Objects.nonNull(val.getOfferValue()))
        .filter(val -> val.getOfferValue() > 0d)
        .orElse(null);
  }

  /*will be removed in SP17*/
  public static CalculatedItem.Price toFinalItemPrice(SivaProduct.FinalPrice price, SivaProduct.FinalPrice offlinePrice) {
    return Optional.ofNullable(MainUtil.getOrDefault(price,offlinePrice))
        .map(val -> CalculatedItem.Price.builder()
            .listed(val.getListValue())
            .offered(val.getBaseOfferValue())
            .finalOffered(val.getOfferValue())
            .adjustment(val.getAdjustment())
            .cheapestItemSku(val.getCheapestItemSku())
            .cheapestPickupPointCode(val.getCheapestPickupPointCode())
            .cheapestPriceDays(val.getCheapestPriceDays())
            .build())
        .orElse(null);
  }
  /*End of will be removed in SP17*/

  public static SivaProduct.PriceTeaser toCurrentPriceTeaser(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getCurrentPriceTeaser)
        .orElse(null);
  }

  private static CampaignInfo toFlashsaleCampaignInfo(Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(campaignInfos)
        .orElseGet(HashMap::new)
        .values()
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> Objects.nonNull(val.getFlashsale()))
        .min(Comparator.comparing(ModuleProductUtil::getFlashsaleStart).thenComparing(ModuleProductUtil::getFlashsaleEnd))
        .orElse(null);
  }

  private static Long getFlashsaleStart(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getFlashsale)
        .map(Flashsale::getStart)
        .orElseGet(() -> Long.MAX_VALUE);
  }

  private static Long getFlashsaleEnd(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getFlashsale)
        .map(Flashsale::getEnd)
        .orElseGet(() -> Long.MAX_VALUE);
  }

  private static CampaignInfo toSubFlashsaleCampaignInfo(Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(campaignInfos)
        .orElseGet(HashMap::new)
        .values()
        .stream()
        .filter(Objects::nonNull)
        .filter(val -> Objects.nonNull(val.getSubFlashsale()))
        .min(Comparator.comparing(ModuleProductUtil::getSubFlashsaleStart).thenComparing(ModuleProductUtil::getSubFlashsaleEnd))
        .orElse(null);
  }

  private static Long getSubFlashsaleStart(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getSubFlashsale)
        .map(Flashsale::getStart)
        .orElseGet(() -> Long.MAX_VALUE);
  }

  private static Long getSubFlashsaleEnd(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getSubFlashsale)
        .map(Flashsale::getEnd)
        .orElseGet(() -> Long.MAX_VALUE);
  }

  public static Flashsale toCampaignInfoFlashsale(CampaignInfo campaignInfo, Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(getCampaignInfoFlashsale(campaignInfo))
        .orElseGet(() -> getCampaignInfoFlashsale(toFlashsaleCampaignInfo(campaignInfos)));
  }

  private static Flashsale getCampaignInfoFlashsale(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getFlashsale)
        .orElse(null);
  }

  public static Flashsale toCampaignInfoSubFlashsale(CampaignInfo campaignInfo, Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(getCampaignInfoSubFlashsale(campaignInfo))
        .orElseGet(() -> getCampaignInfoSubFlashsale(toSubFlashsaleCampaignInfo(campaignInfos)));
  }

  private static Flashsale getCampaignInfoSubFlashsale(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getSubFlashsale)
        .orElse(null);
  }

  public static List<String> toCampaignInfoFlashsaleItemSkus(CampaignInfo campaignInfo, Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(getCampaignInfoFlashsaleItemSkus(campaignInfo))
        .filter(vals -> !CollectionUtils.isEmpty(vals))
        .orElseGet(() -> getCampaignInfoFlashsaleItemSkus(MainUtil.getOrDefault(toFlashsaleCampaignInfo(campaignInfos),toSubFlashsaleCampaignInfo(campaignInfos))));
  }

  public static String toCampaignInfoFlashsaleItemSku(CampaignInfo campaignInfo, Map<String, CampaignInfo> campaignInfos) {
    return toCampaignInfoFlashsaleItemSkus(campaignInfo,campaignInfos)
        .stream()
        .filter(Objects::nonNull)
        .findFirst()
        .orElse(null);
  }

  private static List<String> getCampaignInfoFlashsaleItemSkus(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getFlashsaleItemSkus)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public static FlashsaleInventory toCampaignInfoFlashsaleInventory(CampaignInfo campaignInfo, Map<String, CampaignInfo> campaignInfos) {
    return Optional.ofNullable(toCampaignInfoFlashsaleInventory(campaignInfo))
        .orElseGet(() -> toCampaignInfoFlashsaleInventoryDefault(MainUtil.getOrDefault(toFlashsaleCampaignInfo(campaignInfos),toSubFlashsaleCampaignInfo(campaignInfos))));
  }

  private static FlashsaleInventory toCampaignInfoFlashsaleInventory(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(val -> MainUtil.getOrDefault(val.getFlashsale(),val.getSubFlashsale()))
        .map(val -> toFlashsaleInventory(val,campaignInfo.getAdjustments(),campaignInfo.getInventory()))
        .orElse(null);
  }

  private static FlashsaleInventory toCampaignInfoFlashsaleInventoryDefault(CampaignInfo campaignInfo) {
    return toFlashsaleInventory(
        MainUtil.getOrDefault(getCampaignInfoFlashsale(campaignInfo),getCampaignInfoSubFlashsale(campaignInfo)),
        getCampaignInfoAdjustments(campaignInfo),
        getCampaignInfoInventory(campaignInfo)
    );
  }

  private static List<Quota> getCampaignInfoAdjustments(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getAdjustments)
        .orElseGet(ArrayList::new);
  }

  private static Stock getCampaignInfoInventory(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getInventory)
        .orElse(null);
  }

  public static List<String> toCampaignInfosCampaignCodes(Map<String, CampaignInfo> campaignInfos, boolean currentlyActive) {
    Set<String> result = Optional.ofNullable(campaignInfos)
        .map(Map::values)
        .map(ArrayList::new)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(val -> {
          if (currentlyActive) {
            return val.isActivated() && isItOnToday(val.getStartTime(),val.getEndTime(),getCurrentTimestamp());
          } else {
            return true;
          }
        })
        .map(CampaignInfo::getCampaignCode)
        .filter(campaignCode -> !Default.CAMPAIGN_CODE.equals(campaignCode))
        .collect(Collectors.toSet());
    return MainUtil.fromSetToList(result);
  }

  public static Fbb chooseFbb(Fbb currentFbb, Fbb fbbOrigin, Fbb fbbAlternative) {
    if (Objects.nonNull(fbbAlternative)) {
      fbbAlternative.setPickupPointCode(null);
    }
    Fbb choosenFbb = MainUtil.getOrDefault(fbbOrigin,fbbAlternative);
    if (isFbbActivated(currentFbb)) {
      if (isFbbActivated(choosenFbb)) {
        return choosenFbb;
      } else {
        return currentFbb;
      }
    } else {
      if (isFbbActivated(choosenFbb)) {
        return choosenFbb;
      } else {
        return Fbb.builder()
            .price(getFbbPrice(choosenFbb))
            .build();
      }
    }
  }

  private static boolean isFbbActivated(Fbb fbb) {
    return Optional.ofNullable(fbb)
        .map(Fbb::getPickupPointCode)
        .isPresent();
  }

  private static Double getFbbPrice(Fbb fbb) {
    return Optional.ofNullable(fbb)
        .map(Fbb::getPrice)
        .orElseGet(() -> 0.0D);
  }

  public static Fbb toFbb(CampaignInfo campaignInfo, Boolean fbbActivated) {
    return Optional.ofNullable(campaignInfo)
        .filter(val -> Boolean.compare(val.isFbbActivated(),fbbActivated)==0)
        .map(val -> Fbb.builder()
            .pickupPointCode(toFbbPickupPointCode(val))
            .price(toFbbPrice(val))
            .originalStock(toFbbOriginalStock(val))
            .availableStock(toFbbAvailableStock(val))
            .build())
        .orElse(null);
  }

  private static String toFbbPickupPointCode(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPickupPointCode)
        .orElse(null);
  }

  private static Double toFbbPrice(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getPrice)
        .map(SivaProduct.FinalPrice::getOfferValue)
        .orElseGet(() -> 0.0D);
  }

  private static int toFbbOriginalStock(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getInventory)
        .map(Stock::getQuota)
        .orElseGet(() -> 0);
  }

  private static int toFbbAvailableStock(CampaignInfo campaignInfo) {
    return Optional.ofNullable(campaignInfo)
        .map(CampaignInfo::getInventory)
        .map(Stock::getRemaining)
        .orElseGet(() -> 0);
  }
  /*End of Processed Converter*/

  /*IdTimestamp Converter*/
  public static IdTimestamp toIdTimestampSivaProduct(SivaProduct sivaProduct, SaveParam saveParam) {
    return Optional.ofNullable(sivaProduct)
        .map(val -> IdTimestamp.builder()
            .timestamp(val.getTimestamp())
            .createdTimestamp(val.getCreatedTimestamp())
            .updatedTimestamp(val.getTimestamp())
            .id(val.getProductSku())
            .pickupPointCode(val.getPickupPointCode())
            .externalPickupPointCode(val.getExternalPickupPointCode())
            .topicSource(ParamUtil.getTopicSource(saveParam))
            .build())
        .orElse(null);
  }

  public static IdTimestamp toIdTimestampSivaItem(SivaItem sivaItem, SaveParam saveParam) {
    return Optional.ofNullable(sivaItem)
        .map(val -> IdTimestamp.builder()
            .timestamp(val.getTimestamp())
            .createdTimestamp(val.getCreatedTimestamp())
            .updatedTimestamp(val.getTimestamp())
            .id(val.getItemSku())
            .pickupPointCode(val.getPickupPointCode())
            .externalPickupPointCode(val.getExternalPickupPointCode())
            .topicSource(ParamUtil.getTopicSource(saveParam))
            .build())
        .orElse(null);
  }

  public static IdTimestamp getFilteredIdTimestamp(IdTimestamp idTimestamp, SaveParam saveParam, Collection<String> filters) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getTopicSource)
        .map(topicSource -> {
          if(!CollectionUtils.isEmpty(filters) && filters.contains(topicSource)) {
            return idTimestamp;
          } else {
            return null;
          }})
        .orElse(null);
  }

  public static CampaignTeaserLive getFilteredCampaignProductTagLabel(CampaignTeaserLive campaignTeaserLive, SaveParam saveParam, Collection<String> filters, boolean allowed) {
    return Optional.ofNullable(saveParam)
        .map(SaveParam::getEventParam)
        .map(SaveParam.EventParam::getTopicSource)
        .map(topicSource -> {
          if(!CollectionUtils.isEmpty(filters) && filters.contains(topicSource) && Objects.nonNull(campaignTeaserLive) && allowed) {
            campaignTeaserLive.setPublishTimestamp(getCurrentTimestamp());
            return campaignTeaserLive;
          } else {
            return null;
          }})
        .orElse(null);
  }

  public static IdTimestamp getFixPriceIdTimestamp(IdTimestamp idTimestamp, SivaProduct sivaProduct) {
    return Optional.ofNullable(sivaProduct)
        .map(svProduct -> {
          if(isPriceNotValid(sivaProduct)) {
            return idTimestamp;
          } else {
            return null;
          }})
        .orElse(null);
  }

  public static IdTimestamp getFixItemCodeIdTimestamp(IdTimestamp idTimestamp, SivaItem sivaItem) {
    return Optional.ofNullable(sivaItem)
        .map(svProduct -> {
          if(isItemCodeNotValid(sivaItem)) {
            return idTimestamp;
          } else {
            return null;
          }})
        .orElse(null);
  }

  public static <T extends BaseData> boolean availableToPublish(T data, String topic) {
    boolean idValid = Optional.ofNullable(data)
        .map(val -> val.isIdValid())
        .orElseGet(() -> false);
    boolean topicValid = !StringUtils.isEmpty(topic);
    return idValid && topicValid;
  }
  /*End of IdTimestamp Converter*/

  /*SivaCampaignProduct Converter*/
  public static long toSivaCampaignProductEndDate(SivaCampaignProduct sivaCampaignProduct) {
    return Optional.ofNullable(sivaCampaignProduct)
        .map(SivaCampaignProduct::getPromotionEndTime)
        .orElseGet(() -> 0L);
  }
  /*End of SivaCampaignProduct Converter*/

  /*SivaFlashsaleGroup Converter*/
  public static long toSivaFlashsaleGroupEndDate(SivaFlashsaleGroup sivaFlashsaleGroup) {
    return Optional.ofNullable(sivaFlashsaleGroup)
        .map(SivaFlashsaleGroup::getSchedules)
        .orElseGet(ArrayList::new)
        .stream()
        .max(Comparator.comparing(SivaFlashsaleSchedule::getEnd))
        .map(SivaFlashsaleSchedule::getEnd)
        .orElseGet(() -> 0L);
  }
  /*End of SivaFlashsaleGroup Converter*/

  /*SivaFlashsaleSchedule Converter*/
  public static long toSivaFlashsaleScheduleEndDate(SivaFlashsaleSchedule sivaFlashsaleSchedule) {
    return Optional.ofNullable(sivaFlashsaleSchedule)
        .map(SivaFlashsaleSchedule::getEnd)
        .orElseGet(() -> 0L);
  }
  /*End of SivaFlashsaleSchedule Converter*/

  /*SivaProduct Converter*/
  public static String getSivaProductPickupPoint(SivaProduct sivaProduct) {
    return Optional.ofNullable(sivaProduct)
        .map(SivaProduct::getPickupPointCode)
        .orElse(null);
  }
  /*End of SivaProduct Converter*/

  public static RawProductCombinedUpsertEventModel getRawProductCombinedUpsertEventModel(Product product,
      SaveParam saveParam) {
    RawProductCombinedUpsertEventModel rawProductCombinedUpsertEventModel =
        new RawProductCombinedUpsertEventModel();
    rawProductCombinedUpsertEventModel.setEventTrigger(Topics.MASTER_DATA);
    rawProductCombinedUpsertEventModel.setProduct(product);
    rawProductCombinedUpsertEventModel.setSaveParam(saveParam);
    rawProductCombinedUpsertEventModel.setId(product.getProductSku());
    return rawProductCombinedUpsertEventModel;
  }

  public static void setMandatoryDataInSivaItem(SivaItem result, Item item, CompleteItemData completeItemData) {
    if(StringUtils.isBlank(result.getProductSku()) && !item.isMarkForDelete()){
      setProductSkuInSivaItem(result, item, completeItemData);
    }
    if(StringUtils.isBlank(result.getMerchantCode()) && !item.isMarkForDelete()){
      setMerchantCodeInSivaItem(result, item, completeItemData);
    }
    if (isAnyBlank(result.getBrand(), result.getProductName(), result.getProductCode(),
      result.getProductUrlName()) && !item.isMarkForDelete()) {
      setMasterDataFieldsIfBlank(result, completeItemData);
    }
  }

  public static String fromItemCodeToProductCode(String itemCode) {
    return Optional.ofNullable(itemCode).map(val -> val.substring(0, val.lastIndexOf(HYPHEN)))
      .orElse(null);
  }

  public static void setProductSkuInSivaItem(SivaItem result, Item item,
    CompleteItemData completeItemData) {
    String itemSku = Optional.ofNullable(item).map(Item::getItemSku).orElse(StringUtils.EMPTY);
    String productSku =
      Optional.ofNullable(item).map(Item::getProductSku).orElse(StringUtils.EMPTY);
    if (StringUtils.isNotBlank(productSku)) {
      result.setProductSku(productSku);
      log.info("BackFilling missing productSku for {} ", itemSku);
      return;
    }
    List<Item> allItems = completeItemData.getAllItems();
    if (CollectionUtils.isNotEmpty(allItems)) {
      productSku =
        allItems.stream().map(Item::getProductSku).filter(StringUtils::isNotBlank).findFirst()
          .orElse(fromItemSkuToProductSku(itemSku));
    } else {
      productSku = fromItemSkuToProductSku(itemSku);
    }
    log.info("BackFilling missing productSku for {} ", itemSku);
    result.setProductSku(productSku);
  }

  public static void setMerchantCodeInSivaItem(SivaItem result, Item item,
    CompleteItemData completeItemData) {
    String merchantCode =
      Optional.ofNullable(item).map(Item::getMerchantCode).orElse(StringUtils.EMPTY);
    String itemSku = Optional.ofNullable(item).map(Item::getItemSku).orElse(StringUtils.EMPTY);
    if (StringUtils.isNotBlank(merchantCode)) {
      result.setMerchantCode(merchantCode);
      log.info("BackFilling missing merchantCode for {} ", itemSku);
      return;
    }
    List<Item> allItems = completeItemData.getAllItems();
    if (CollectionUtils.isNotEmpty(allItems)) {
      merchantCode =
        allItems.stream().map(Item::getMerchantCode).filter(StringUtils::isNotBlank).findFirst()
          .orElse(null);
    }
    log.info("BackFilling missing merchantCode for {} ", itemSku);
    result.setMerchantCode(merchantCode);
  }

  public static void setMasterDataFieldsIfBlank(SivaItem result,
    CompleteItemData completeItemData) {
    List<MasterDataProduct> masterDataProducts =
      Optional.ofNullable(completeItemData).map(CompleteItemData::getAllMasterDataProduct)
        .orElse(Collections.emptyList()).stream().filter(Objects::nonNull).toList();
    if (CollectionUtils.isNotEmpty(masterDataProducts)) {
      if (StringUtils.isEmpty(result.getProductName())) {
        String name = masterDataProducts.stream().map(MasterDataProduct::getProductName)
          .filter(StringUtils::isNotBlank).findFirst().orElse(result.getProductSku());
        result.setProductName(name);
        log.info("BackFilling missing productName for {} with value : {} ", result.getItemSku(),
          name);
        result.setProductUrlName(UrlFriendlyUtil.createUrlFriendlyString(name));
      }
      if (StringUtils.isEmpty(result.getProductCode())) {
        String code = masterDataProducts.stream().map(MasterDataProduct::getProductCode)
          .filter(StringUtils::isNotBlank).findFirst()
          .orElseGet(() -> fromItemCodeToProductCode(result.getItemCode()));
        log.info("BackFilling missing productCode for {} with value {} ", result.getItemSku(),
          code);
        result.setProductCode(code);
      }
      if (StringUtils.isEmpty(result.getBrand())) {
        result.setBrand(masterDataProducts.getFirst().getBrand());
      }
    }
  }

  private static boolean isAnyBlank(String... values) {
    for (String value : values) {
      if (!org.springframework.util.StringUtils.hasText(value)) {
        return true;
      }
    }
    return false;
  }

  public static boolean validateEventRecordForDeletion(TerminatedSellerDeletionEventModel model) {
    return Optional.ofNullable(model).filter(record -> Objects.nonNull(record.getProductCode()))
      .filter(record -> Objects.nonNull(record.getProductSku())).isPresent();
  }

  public static boolean isProductRejected(List<String> changeTypes, String traceId, String id,
    String eventSource, boolean deleteProductDataOnTermination,
    List<String> changeTypesToSkipL4AndL5Events) {
    if (deleteProductDataOnTermination) {
      List<String> changeTypesFromEvent =
        Optional.ofNullable(changeTypes).orElse(Collections.emptyList());

      for (String changeType : changeTypesFromEvent) {
        if (changeTypesToSkipL4AndL5Events.contains(changeType)) {
          log.info("Skipping processing for changeType [{}] for {}. TraceId: {}, PickupPointId: {}",
            changeType, eventSource, traceId, id);
          return true;
        }
      }
    }
    return false;
  }

  public static TerminatedSellerDeletionEventModel toTerminatedSellerDeletionEventModel(Product event) {
    return Optional.ofNullable(event).map(
      val -> TerminatedSellerDeletionEventModel.builder().productCode(val.getProductCode())
        .productSku(val.getProductSku()).sellerCode(val.getMerchantCode())
        .sharedProduct(event.isSharedProduct()).build()).orElse(null);
  }

  public static List<List<ConsumerRecord<String, String>>> partitionConsumerRecords(
    List<ConsumerRecord<String, String>> records, int size) {
    return Lists.partition(records, size);
  }

  public static Stock toUpdatedStockFromStockUpdateSearchEvent(String itemSku,
    String pickupPointCode, StockUpdateSearchEvent stockUpdateSearchEvent, PickupPoint pickupPoint) {
    boolean nullIdsPresent = Stream.of(pickupPointCode, itemSku,
        stockUpdateSearchEvent.getWebItemSku(), stockUpdateSearchEvent.getPickupPointCode())
      .anyMatch(Objects::isNull);
    if (nullIdsPresent) {
      log.info("Initializing stock data due to null IDs in StockUpdateSearchEvent - itemSku: {}, "
        + "pickupPointCode: {}", itemSku, pickupPointCode);
      return initializeStockData(itemSku, pickupPointCode, pickupPoint);
    }
    return Optional.of(stockUpdateSearchEvent)
      .filter(event -> event.getWebItemSku().equalsIgnoreCase(itemSku))
      .filter(event -> event.getPickupPointCode().equalsIgnoreCase(pickupPointCode)).map(event -> {
        boolean available = !event.isOutOfStock();
        String status = ModuleProductUtil.convertStockStatusByAvailability(stockUpdateSearchEvent);

        return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode).warehouse(
            ModuleProductUtil.isWarehouse(
              ModuleProductUtil.toPickupPointTypeFromStockSearch(stockUpdateSearchEvent)))
          .status(status).exists(available).build();
      }).orElseGet(() -> {
        boolean available = !stockUpdateSearchEvent.isOutOfStock();
        String status = ModuleProductUtil.convertStockStatusByAvailability(stockUpdateSearchEvent);

        return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode).warehouse(
            ModuleProductUtil.isWarehouse(
              ModuleProductUtil.toPickupPointTypeFromStockSearch(stockUpdateSearchEvent)))
          .status(status).exists(available).build();
      });
  }

  public static Stock getCheapestStock(String itemSku, String pickupPointCode,
    CompleteItemData completeItemData, PickupPointInventory existingPickupPointInventory) {
    if (Objects.isNull(itemSku) || Objects.isNull(pickupPointCode)) {
      log.warn("Null input detected: itemSku={}, pickupPointCode={}", itemSku, pickupPointCode);
      return null;
    }
    if(Objects.nonNull(existingPickupPointInventory)){
      return getStockFromPickupPointInventory(itemSku, pickupPointCode, existingPickupPointInventory);
    }
    return Optional.ofNullable(completeItemData).map(CompleteItemData::getAllSivaItems)
      .filter(CollectionUtils::isNotEmpty).orElseGet(ArrayList::new).stream()
      .filter(sivaItem -> itemSku.equals(sivaItem.getItemSku()))
      .filter(sivaItem -> pickupPointCode.equals(sivaItem.getPickupPointCode()))
      .findFirst().map(sivaItem -> {
        Stock stock = new Stock();
        stock.setItemSku(sivaItem.getItemSku());
        stock.setPickupPointCode(sivaItem.getPickupPointCode());
        stock.setStatus(Optional.ofNullable(sivaItem.getInventory())
          .map(Stock::getStatus).orElse(StockStatus.OOS));
        return stock;
      }).orElse(null);
  }

  private static Stock getStockFromPickupPointInventory(String itemSku, String pickupPointCode,
    PickupPointInventory existingPickupPointInventory) {
    boolean inStock = existingPickupPointInventory.isInStock();
    boolean syncStock = existingPickupPointInventory.getSyncStock();
    return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode).warehouse(syncStock)
      .status(inStock ? StockStatus.AVAILABLE : StockStatus.OOS).exists(inStock).build();
  }

  public static String toPickupPointTypeFromStockSearch(
    StockUpdateSearchEvent stockUpdateSearchEvent) {
    return Optional.ofNullable(stockUpdateSearchEvent)
      .map(StockUpdateSearchEvent::getWarehouseDetails).filter(
        vals -> !CollectionUtils.isEmpty(vals.getNonOosWarehouseCodes())
          || CollectionUtils.isNotEmpty(vals.getOosWarehouseCodes()))
      .map(val -> InventoryType.TYPE_ONLINE_WAREHOUSE)
      .orElseGet(() -> InventoryType.TYPE_ONLINE_MERCHANT);
  }

  public static String convertStockStatusByAvailability(StockUpdateSearchEvent event) {
    if (Objects.isNull(event)) {
      return StockStatus.OOS;
    }
    boolean available;
    if (!event.isSyncStock()) {
      // webStock -> available when not out of stock
      available = !event.isOutOfStock();
    } else {
      // for warehouse -> available if there are any non-OOS warehouse codes
      List<String> nonOos = Objects.nonNull(event.getWarehouseDetails()) ?
        event.getWarehouseDetails().getNonOosWarehouseCodes() :
        Collections.emptyList();
      available = CollectionUtils.isNotEmpty(nonOos);
    }
    return available ? StockStatus.AVAILABLE : StockStatus.OOS;
  }

  private static String convertStockStatusByAvailability(
    Level2InventoryQuantityChangedEvent stockRepublishEvent) {
    boolean available;
    if (Boolean.TRUE.equals(stockRepublishEvent.getSyncStock())) {
      available = CollectionUtils.isNotEmpty(stockRepublishEvent.getWarehouseInfos())
        && isWareHouseStockAvailable(stockRepublishEvent);
    } else {
      available = stockRepublishEvent.getAvailableStock() > ZERO;
    }
    return available ? StockStatus.AVAILABLE : StockStatus.OOS;
  }

  private static boolean isWareHouseStockAvailable(
    Level2InventoryQuantityChangedEvent stockRepublishEvent) {
    return stockRepublishEvent.getWarehouseInfos().stream().map(WarehouseInfo::getAvailableStock)
      .anyMatch(stockValue -> Objects.nonNull(stockValue) && stockValue > ZERO);
  }

  public static Stock initializeStockData(String itemSku, String pickupPointCode,
    PickupPoint pickupPoint) {
    if(Objects.nonNull(pickupPoint)){
      boolean inStock = pickupPoint.isInStock();
      return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode).exists(inStock)
        .status(inStock ? StockStatus.AVAILABLE : StockStatus.OOS).build();
    }
    return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode).exists(false)
      .status(StockStatus.OOS).build();
  }

  public static void setStockFromLevel2InventoryQuantityChangedEvent(PickupPoint pickupPoint,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    if (validateStockRepublishEvent(pickupPoint, level2InventoryQuantityChangedEvent)) {
      if (Boolean.TRUE.equals(level2InventoryQuantityChangedEvent.getSyncStock())) {
        pickupPoint.setInStock(setInStockForWarehouseSkus(level2InventoryQuantityChangedEvent));
        pickupPoint.setWarehouse(true);
      }
      else {
        pickupPoint.setInStock(level2InventoryQuantityChangedEvent.getAvailableStock() > ZERO);
        pickupPoint.setWarehouse(false);
      }
    }
  }

  private static boolean setInStockForWarehouseSkus(
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    if (CollectionUtils.isEmpty(level2InventoryQuantityChangedEvent.getWarehouseInfos())) {
      // for pending inbound , warehouseInfos will be empty, but we still want to consider the
      // stock as OOS since web stock will not be relied on when syncStock is true
      return false;
    }
    // else check if any warehouse has available stock > 0
    return level2InventoryQuantityChangedEvent.getWarehouseInfos().stream().anyMatch(
      warehouseInfo -> Objects.nonNull(warehouseInfo) && Objects.nonNull(
        warehouseInfo.getAvailableStock()) && warehouseInfo.getAvailableStock() > ZERO);

  }

  private static boolean validateStockRepublishEvent(PickupPoint pickupPoint,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    return Objects.nonNull(pickupPoint) && Objects.nonNull(level2InventoryQuantityChangedEvent)
      && StringUtils.equalsIgnoreCase(pickupPoint.getItemSku(),
      level2InventoryQuantityChangedEvent.getLevel2Id()) && StringUtils.equalsIgnoreCase(
      pickupPoint.getPickupPointCode(), level2InventoryQuantityChangedEvent.getPickupPointCode());
  }

  public static Stock toUpdatedStockFromStockRepublishEvent(String itemSku, String pickupPointCode,
    Level2InventoryQuantityChangedEvent stockRepublishEvent, PickupPoint pickupPoint) {
    boolean nullIdsPresent = Stream.of(pickupPointCode, itemSku, stockRepublishEvent.getLevel2Id(),
      stockRepublishEvent.getPickupPointCode()).anyMatch(Objects::isNull);
    if (nullIdsPresent) {
      log.info("Initializing stock data due to null IDs in StockRepublishEvent - itemSku: {}, pickupPointCode: {}", itemSku, pickupPointCode);
      return initializeStockData(itemSku, pickupPointCode, pickupPoint);
    }
    return Optional.of(stockRepublishEvent)
      .filter(event -> event.getLevel2Id().equalsIgnoreCase(itemSku))
      .filter(event -> event.getPickupPointCode().equalsIgnoreCase(pickupPointCode)).map(
        event -> Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode)
          .warehouse(Boolean.TRUE.equals(event.getSyncStock()))
          .status(ModuleProductUtil.convertStockStatusByAvailability(stockRepublishEvent))
          .exists(true).build()).orElseGet(() -> {
            log.info("Initializing stock data due to non-matching StockRepublishEvent - itemSku: {}, pickupPointCode: {}", itemSku, pickupPointCode);
            return initializeStockData(itemSku, pickupPointCode, pickupPoint);
          });

  }

  public static PickupPointInventory toPickupPointInventoryFromPickupPoint(Optional<PickupPoint> pickupPoint) {
    if(pickupPoint.isPresent()){
      PickupPointInventory pickupPointInventory = new PickupPointInventory();
      pickupPointInventory.setPickupPointCode(pickupPoint.get().getPickupPointCode());
      pickupPointInventory.setItemSku(pickupPoint.get().getItemSku());
      pickupPointInventory.setId(pickupPoint.get().getId());
      pickupPointInventory.setSyncStock(pickupPoint.get().isWarehouse());
      pickupPointInventory.setInStock(pickupPoint.get().isInStock());
      return pickupPointInventory;
    }
    return null;
  }

  public static Stock buildBasicStock(String itemSku, String pickupPointCode,
    PickupPointInventory pickupPointInventory) {
    return Stock.builder().itemSku(itemSku).pickupPointCode(pickupPointCode)
      .warehouse(pickupPointInventory.getSyncStock())
      .status(pickupPointInventory.isInStock() ? StockStatus.AVAILABLE : StockStatus.OOS)
      .exists(true).build();
  }

  public static String extractMerchantCode(PickupPoint pickupPoint) {
    if (Objects.isNull(pickupPoint)) {
      return null;
    }
    String merchantCode = pickupPoint.getMerchantCode();
    if (StringUtils.isNotBlank(merchantCode)) {
      return merchantCode;
    }
    return extractMerchantCodeFromSku(pickupPoint.getItemSku());
  }

  public static String extractMerchantCode(SivaProductCombinedUpsertEventModel eventModel) {
    if (Optional.ofNullable(eventModel).map(SivaProductCombinedUpsertEventModel::getSivaProduct)
      .isEmpty()) {
      return null;
    }
    String merchantCode = eventModel.getSivaProduct().getMerchantCode();
    if (StringUtils.isNotBlank(merchantCode)) {
      return merchantCode;
    }
    return extractMerchantCodeFromSku(eventModel.getSivaProduct().getProductSku());
  }

  public static String extractMerchantCode(SivaItemCombinedUpsertEventModel eventModel) {
    if (Objects.isNull(eventModel)) {
      return null;
    }
    if (Objects.nonNull(eventModel.getSivaItem())) {
      String merchantCode = eventModel.getSivaItem().getMerchantCode();
      if (StringUtils.isNotBlank(merchantCode)) {
        return merchantCode;
      }
      String itemSku = eventModel.getSivaItem().getItemSku();
      if (StringUtils.isNotBlank(itemSku)) {
        return extractMerchantCodeFromSku(itemSku);
      }
    }
    return null;
  }

  public static String extractMerchantCodeFromSku(String sku) {
    if (StringUtils.isBlank(sku)) {
      return null;
    }
    String[] parts = sku.split(HYPHEN);
    if (parts.length >= 2) {
      return parts[0] + HYPHEN + parts[1];
    }

    return null;
  }

  public static List<PickupPoint> filterDistributionPickupPoints(List<PickupPoint> pickupPoints,
    boolean filterDistributionPickupPoints) {
    if (CollectionUtils.isEmpty(pickupPoints) || !filterDistributionPickupPoints) {
      return pickupPoints;
    }

    List<PickupPoint> nonDistributionPickupPoints = pickupPoints.stream().filter(Objects::nonNull)
      .filter(Predicate.not(PickupPoint::isDistribution)).collect(Collectors.toList());

    // If there are non-distribution pickup points, use them; otherwise use all (including distribution)
    return CollectionUtils.isNotEmpty(nonDistributionPickupPoints) ?
      nonDistributionPickupPoints :
      pickupPoints;
  }

  public static String extractMerchantCode(PickupPointUpsertCombinedEventModel eventModel) {
    if (Objects.isNull(eventModel)) {
      return null;
    }
    if (Objects.nonNull(eventModel.getPickupPoint())) {
      return extractMerchantCode(eventModel.getPickupPoint());
    }
    if (Objects.nonNull(eventModel.getInventoryInfoChange())) {
      return extractMerchantCodeFromSku(eventModel.getInventoryInfoChange().getItemSku());
    }
    if (Objects.nonNull(eventModel.getStockUpdateSearchEvent())) {
      return eventModel.getStockUpdateSearchEvent().getWebMerchantCode();
    }
    if (Objects.nonNull(eventModel.getLevel2InventoryQuantityChangedEvent())) {
      return eventModel.getLevel2InventoryQuantityChangedEvent().getLevel2MerchantCode();
    }
    
    return null;
  }

  public static void setPartitionKeyForPickupPointEvent(
    PickupPointUpsertCombinedEventModel pickupPointUpsertCombinedEventModel, String partitionKey) {
    String partitionKeyValue = switch (partitionKey) {
      case PICKUP_POINT_ID -> Optional.ofNullable(pickupPointUpsertCombinedEventModel)
        .map(PickupPointUpsertCombinedEventModel::getPickupPoint).map(
          pickupPoint -> Optional.ofNullable(pickupPoint.getItemSku()).orElse(StringUtils.EMPTY)
            .concat(HYPHEN).concat(
              Optional.ofNullable(pickupPoint.getPickupPointCode()).orElse(StringUtils.EMPTY)))
        .orElse(StringUtils.EMPTY);
      case ITEM_SKU -> pickupPointUpsertCombinedEventModel.getPickupPoint().getItemSku();
      default -> pickupPointUpsertCombinedEventModel.getPickupPoint().getProductSku();
    };
    if (StringUtils.isNotBlank(partitionKeyValue)) {
      pickupPointUpsertCombinedEventModel.setId(partitionKeyValue);
    }
  }
}
