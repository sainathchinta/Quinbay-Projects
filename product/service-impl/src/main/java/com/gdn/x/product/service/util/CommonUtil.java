package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
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
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.ItemDataChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.vo.ImageBasicInfoResponse;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ProductBasicInfoResponse;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.ReelProductListingRequestVo;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.ReelProductListingRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ReelProductDetailResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.google.common.collect.MapDifference;
import com.google.common.collect.Maps;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.SolrInputField;
import org.springframework.beans.BeanUtils;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentChangeEvent;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.enums.ItemChangeEventType;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.DiscountPrice;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointViewConfigChangeEvent;
import com.gdn.x.product.domain.event.model.ItemPriceChangeEventModel;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.ItemViewConfigWithArchivedChangeEvent;
import com.gdn.x.product.domain.event.model.MerchantPromoDiscountChangeEvent;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.ParentCategory;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.domain.event.model.ProductChange;
import com.gdn.x.product.enums.CacheNames;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.enums.SalesChannel;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.ItemAndPickupPointBasicDetailVo;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductCenterHistory;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.BuyableScheduleVo;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.DiscoverableScheduleVo;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemNameSkuVO;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.model.vo.ProductSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;
import com.gdn.x.product.rest.web.model.dto.SortedDefiningAttributeDTO;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemScoreRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateItemRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryUpdateRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.FailedRetryProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 22/04/2019 AD.
 */
@Slf4j
public class CommonUtil {

  private static final String STRING_FORMATTER = "%s%s%s";
  private static final String STRING_FORMATTER_PRICE = "%s%s%.0f";
  private static final String NEW_LINE = "\n";
  private static final String CARRIAGE_RETURN = "\r";
  private static final String DELIMETER = "#_#";
  public static final String ITEM_NOT_EDITABLE = "Item is not editable at the moment";
  public static final String PRODUCT_IS_NOT_EDITABLE = "Product is not editable at the moment";
  private static final String FAILED_RETRY_PUBLISH_ALERT = "failedRetryPublishAlert";
  private static final String FAILED_RETRY_PUBLISH_EMAIL_SUBJECT = " Blibli Seller Center - Failed Retry Publish";
  private static final String OBJECT = "object";
  private static final String EMAIL_OBJECT = "obj";
  private static final String TOTAL = "total";
  private static final String DOT = "[.]";
  public static final int BUYABLE_AND_DISCOVERABLE_CHANGE_TYPES = 2;
  private static final double ZERO_DIMENSIONS = 0.0;
  public static final String DESCRIPTIVE_ATTRIBUTE = "DESCRIPTIVE_ATTRIBUTE";
  public static final String PREDEFINED_ATTRIBUTE = "PREDEFINED_ATTRIBUTE";

  public static MerchantPromoDiscountChangeEvent toMerchantPromoDiscountChangeEvent(Item item) throws Exception {
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent = new MerchantPromoDiscountChangeEvent();
    merchantPromoDiscountChangeEvent.setMerchantCode(item.getMerchantCode());
    merchantPromoDiscountChangeEvent.setItemSku(item.getItemSku());
    merchantPromoDiscountChangeEvent.setProductSku(item.getProductSku());
    merchantPromoDiscountChangeEvent.setMarkForDelete(item.isMarkForDelete());
    merchantPromoDiscountChangeEvent.setStoreId(item.getStoreId());
    merchantPromoDiscountChangeEvent.setPrice(toPrices(item.getPrice(), item.getItemSku()));
    return merchantPromoDiscountChangeEvent;
  }

  public static ItemViewConfigWithArchivedChangeEvent toItemViewConfigWithArchivedChangeEvent(
      ItemVo itemVo, ItemPickupPointVo itemPickupPointVo, boolean cncForWarehouseFeatureSwitch) {
    ItemViewConfigWithArchivedChangeEvent mvChangeEvent = new ItemViewConfigWithArchivedChangeEvent();
    mvChangeEvent.setIsArchived(itemVo.isArchived());
    mvChangeEvent.setItemSku(itemPickupPointVo.getItemSku());
    mvChangeEvent.setPickupPointCode(itemPickupPointVo.getPickupPointCode());
    mvChangeEvent.setMarkForDelete(itemVo.isMarkForDelete());
    mvChangeEvent.setMerchantCode(itemPickupPointVo.getMerchantCode());
    mvChangeEvent.setStoreId(itemVo.getStoreId());
    mvChangeEvent.setItemViewConfigs(
        getItemViewConfigs(itemPickupPointVo, cncForWarehouseFeatureSwitch).stream()
            .map(CommonUtil::toItemViewConfigEventModel).collect(Collectors.toSet()));
    return mvChangeEvent;
  }

  public static ItemViewConfigWithArchivedChangeEvent toItemViewConfigWithArchivedChangeEvent(Item item,
      ItemPickupPoint itemPickupPoint, boolean cncForWarehouseFeatureSwitch) {
    ItemViewConfigWithArchivedChangeEvent mvChangeEvent = new ItemViewConfigWithArchivedChangeEvent();
    mvChangeEvent.setIsArchived(item.isArchived());
    mvChangeEvent.setItemSku(itemPickupPoint.getItemSku());
    mvChangeEvent.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    mvChangeEvent.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    mvChangeEvent.setMerchantCode(itemPickupPoint.getMerchantCode());
    mvChangeEvent.setStoreId(itemPickupPoint.getStoreId());
    mvChangeEvent.setItemViewConfigs(
        getItemViewConfigs(itemPickupPoint, cncForWarehouseFeatureSwitch).stream()
            .map(CommonUtil::toItemViewConfigEventModel).collect(Collectors.toSet()));
    return mvChangeEvent;
  }

  public static void settingPriceUpdatedDate(Item item, ItemPickupPoint itemPickupPoint, String defaultChannel) {
    com.gdn.x.product.model.entity.Price priceL4 =
        item.getPrice().stream().filter(itemPrice -> defaultChannel.equals(itemPrice.getChannel())).findFirst()
            .orElse(null);
    com.gdn.x.product.model.entity.Price priceL5 = itemPickupPoint.getPrice().stream()
        .filter(itemPickupPointPrice -> defaultChannel.equals(itemPickupPointPrice.getChannel())).findFirst()
        .orElse(null);
    if (Objects.nonNull(priceL4) && Objects.nonNull(priceL5)) {
      if ((Double.compare(priceL4.getListPrice(), priceL5.getListPrice()) != 0) || (
          Double.compare(priceL4.getOfferPrice(), priceL5.getOfferPrice()) != 0)) {
        itemPickupPoint.setPriceUpdatedDate(new Date());
      }
    }
  }

  public static ItemPickupPointViewConfigChangeEvent toItemPickupPointViewConfigChangeEvent(
    ItemPickupPoint pickupPoint, boolean cncForWarehouseFeatureSwitch) {
    ItemPickupPointViewConfigChangeEvent viewConfigChangeEvent =
      new ItemPickupPointViewConfigChangeEvent();
    viewConfigChangeEvent.setItemSku(pickupPoint.getItemSku());
    viewConfigChangeEvent.setMerchantCode(pickupPoint.getMerchantCode());
    viewConfigChangeEvent.setStoreId(pickupPoint.getStoreId());
    viewConfigChangeEvent.setMarkForDelete(pickupPoint.isMarkForDelete());
    viewConfigChangeEvent.setItemViewConfigs(
        getItemViewConfigs(pickupPoint, cncForWarehouseFeatureSwitch).stream()
            .map(CommonUtil::toItemViewConfigEventModel).collect(Collectors.toSet()));
    viewConfigChangeEvent.setPickupPointCode(pickupPoint.getPickupPointCode());
    return viewConfigChangeEvent;
  }

  private static Set<com.gdn.x.product.model.entity.ItemViewConfig> getItemViewConfigs(
      ItemPickupPoint pickupPoint, boolean cncForWarehouseFeatureSwitch) {
    return cncForWarehouseFeatureSwitch ?
        pickupPoint.getAllItemViewConfigs() :
        pickupPoint.getItemViewConfig();
  }

  private static ItemViewConfig toItemViewConfigEventModel(com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    ItemViewConfig itemViewConfigEvent = new ItemViewConfig();
    BeanUtils.copyProperties(itemViewConfig, itemViewConfigEvent);
    return itemViewConfigEvent;
  }

  private static Set<Price> toPrices(Set<com.gdn.x.product.model.entity.Price> prices, String itemSku) throws Exception {
    return prices.stream().map(itemPrice -> {
      try {
        Price price = new Price();
        BeanUtils.copyProperties(itemPrice, price, "merchantPromoDiscountPrice");
        if (Objects.nonNull(itemPrice.getMerchantPromoDiscountPrice())) {
          price.setMerchantPromoDiscountPrice(toDiscountPrice(itemPrice.getMerchantPromoDiscountPrice(), itemSku));
        }
        return price;
      } catch (Exception ex) {
        log.error("Error while copying Price, itemSku:{}, itemPrice", itemSku, itemPrice, ex);
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "Error while copying Item Price");
      }
    }).collect(Collectors.toSet());
  }

  private static DiscountPrice toDiscountPrice(com.gdn.x.product.model.entity.DiscountPrice discountPrice,
      String itemSku) {
    DiscountPrice discountPriceResult = new DiscountPrice();
    try {
      BeanUtils.copyProperties(discountPrice, discountPriceResult);
    } catch (Exception ex) {
      log.error("Error while copying DiscountPrice , itemSku:{}, discountPrice:{}", itemSku, discountPrice, ex);
    }
    return discountPriceResult;
  }

  public static ProductRequest overridePreOrderDateToJKT(ProductRequest productRequest) {
    if (Objects.nonNull(productRequest.getPreOrder()) && Objects.nonNull(productRequest.getPreOrder().getPreOrderDate())
        && productRequest.getPreOrder().isConvertToJKT()) {
      PreOrderDTO preOrder = productRequest.getPreOrder();
      Calendar calendar = Calendar.getInstance();
      calendar.setTime(preOrder.getPreOrderDate());
      calendar.add(Calendar.HOUR_OF_DAY, -7);
      preOrder.setPreOrderDate(calendar.getTime());
      productRequest.setPreOrder(preOrder);
    }
    return productRequest;
  }

  public static void validateMerchantPromoDiscountPriceChange(PromoAdjustmentChangeEvent promoAdjustmentChangeEvent) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoAdjustmentChangeEvent.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoAdjustmentChangeEvent.getItemSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(promoAdjustmentChangeEvent.getStartDate()) && Objects
        .nonNull(promoAdjustmentChangeEvent.getEndDate()), "start date and end date should not be empty");
  }

  public static String toOfflineItemId(String itemSku, String pickupPointCode) {
    return itemSku + Constants.HYPHEN + pickupPointCode;
  }

  public static SolrInputDocument toSolrInputDocument(ProductAndItemSolr productAndItemSolr) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, productAndItemSolr.getId());
    solrInputDocument.setField(SolrFieldNames.MARK_FOR_DELETE, productAndItemSolr.isMarkForDelete());
    solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE, productAndItemSolr.isCncActivated());
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED, productAndItemSolr.isArchived());
    solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE, productAndItemSolr.isFreeSample());
    solrInputDocument.setField(SolrFieldNames.IS_SUSPENDED, productAndItemSolr.isSuspended());
    solrInputDocument.setField(SolrFieldNames.IS_LATE_FULFILLMENT, productAndItemSolr.isLatefulfillment());
    solrInputDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, productAndItemSolr.isSynchronized());
    solrInputDocument.setField(SolrFieldNames.TRADING_PRODUCT, productAndItemSolr.isTradingProduct());
    solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productAndItemSolr.isMerchantPromoDiscount());
    solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED,
        productAndItemSolr.isMerchantPromoDiscountActivated());
    solrInputDocument.setField(SolrFieldNames.CURATION_STATUS, productAndItemSolr.getCurationStatus());
    solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, productAndItemSolr.isOff2OnChannelActive());
    solrInputDocument.setField(SolrFieldNames.PROMO_BUNDLING, productAndItemSolr.isPromoBundling());
    solrInputDocument.setField(SolrFieldNames.IS_PRE_ORDER_ACTIVE, productAndItemSolr.isPreOrderActive());
    solrInputDocument.setField(SolrFieldNames.TRADING_PRODUCT, productAndItemSolr.isTradingProduct());
    if (StringUtils.isNotBlank(productAndItemSolr.getItemSku())) {
      solrInputDocument.setField(SolrFieldNames.ITEM_SKU, productAndItemSolr.getItemSku());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getStoreId())) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID, productAndItemSolr.getStoreId());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getBrand())) {
      solrInputDocument.setField(SolrFieldNames.BRAND, productAndItemSolr.getBrand());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getItemCode())) {
      solrInputDocument.setField(SolrFieldNames.ITEM_CODE, productAndItemSolr.getItemCode());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getItemName())) {
      solrInputDocument.setField(SolrFieldNames.ITEM_NAME, productAndItemSolr.getItemName());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getMasterCatalog())) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, productAndItemSolr.getMasterCatalog());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getProductName())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME, productAndItemSolr.getProductName());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getMerchantCode())) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, productAndItemSolr.getMerchantCode());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getMerchantSku())) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_SKU, productAndItemSolr.getMerchantSku());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getPickupPointCode())) {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODE, productAndItemSolr.getPickupPointCode());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getProductCatentryId())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID, productAndItemSolr.getProductCatentryId());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getProductCode())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE, productAndItemSolr.getProductCode());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getProductSku())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productAndItemSolr.getProductSku());
    }
    if (StringUtils.isNotBlank(productAndItemSolr.getTicketTemplateCode())) {
      solrInputDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE, productAndItemSolr.getTicketTemplateCode());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getBuyable())) {
      solrInputDocument.setField(SolrFieldNames.BUYABLE, productAndItemSolr.getBuyable());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getDiscoverable())) {
      solrInputDocument.setField(SolrFieldNames.DISCOVERABLE, productAndItemSolr.getDiscoverable());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getListPrice())) {
      solrInputDocument.setField(SolrFieldNames.LIST_PRICE, productAndItemSolr.getListPrice());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getItemImages())) {
      solrInputDocument.setField(SolrFieldNames.ITEM_IMAGES, productAndItemSolr.getItemImages());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getOfferPrice())) {
      solrInputDocument.setField(SolrFieldNames.OFFER_PRICE, productAndItemSolr.getOfferPrice());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getOfflinePrices())) {
      solrInputDocument.setField(SolrFieldNames.OFFLINE_PRICES, productAndItemSolr.getOfflinePrices());
    }
    if (CollectionUtils.isNotEmpty(productAndItemSolr.getSalesCatalog())) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, productAndItemSolr.getSalesCatalog());
    }
    if (Objects.nonNull(productAndItemSolr.getCreatedDate())) {
      solrInputDocument.setField(SolrFieldNames.CREATED_DATE, productAndItemSolr.getCreatedDate());
    }
    if (Objects.nonNull(productAndItemSolr.getUpdatedDate())) {
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, productAndItemSolr.getUpdatedDate());
    }
    if (Objects.nonNull(productAndItemSolr.getProductType())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_TYPE, productAndItemSolr.getProductType());
    }
    if (Objects.nonNull(productAndItemSolr.getWholesalePriceActivated())) {
      solrInputDocument.setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED, productAndItemSolr.getWholesalePriceActivated());
    }
    if (Objects.nonNull(productAndItemSolr.getProductScoreTotal())) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, productAndItemSolr.getProductScoreTotal());
    }
    if (Objects.nonNull(productAndItemSolr.getPristineId())) {
      solrInputDocument.setField(SolrFieldNames.PRISTINE_ID, productAndItemSolr.getPristineId());
    }
    return solrInputDocument;
  }

  public static ProductAndItemSolr toProductAndItemSolr(SolrDocument solrDocument) {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setId((String) solrDocument.getFieldValue(SolrFieldNames.ID));
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED))) {
      productAndItemSolr.setSynchronized((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE))) {
      productAndItemSolr.setMarkForDelete((Boolean) solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CNC_ACTIVE))) {
      productAndItemSolr.setCncActivated((Boolean) solrDocument.getFieldValue(SolrFieldNames.CNC_ACTIVE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE))) {
      productAndItemSolr.setProductScoreTotal((double) solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_LATE_FULFILLMENT))) {
      productAndItemSolr.setLatefulfillment((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_LATE_FULFILLMENT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE))) {
      productAndItemSolr.setProductScoreTotal((Double) solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED))) {
      productAndItemSolr.setArchived((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TRADING_PRODUCT))) {
      productAndItemSolr.setTradingProduct((Boolean) solrDocument.getFieldValue(SolrFieldNames.TRADING_PRODUCT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT))) {
      productAndItemSolr
          .setMerchantPromoDiscount((Boolean) solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED))) {
      productAndItemSolr.setMerchantPromoDiscountActivated(
          (Boolean) solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PROMO_BUNDLING))) {
      productAndItemSolr.setPromoBundling((Boolean) solrDocument.getFieldValue(SolrFieldNames.PROMO_BUNDLING));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE))) {
      productAndItemSolr
          .setOff2OnChannelActive((Boolean) solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU))) {
      productAndItemSolr.setItemSku((String) solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))) {
      productAndItemSolr.setProductSku((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE))) {
      productAndItemSolr.setProductCode((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_CODE))) {
      productAndItemSolr.setItemCode((String) solrDocument.getFieldValue(SolrFieldNames.ITEM_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_SKU))) {
      productAndItemSolr.setMerchantSku((String) solrDocument.getFieldValue(SolrFieldNames.MERCHANT_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_NAME))) {
      productAndItemSolr.setItemName((String) solrDocument.getFieldValue(SolrFieldNames.ITEM_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_TYPE))) {
      productAndItemSolr.setProductType((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_TYPE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      productAndItemSolr.setProductName((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE))) {
      productAndItemSolr.setMerchantCode((String) solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG))) {
      productAndItemSolr.setMasterCatalog((String) solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODE))) {
      productAndItemSolr.setPickupPointCode((String) solrDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BRAND))) {
      productAndItemSolr.setBrand((String) solrDocument.getFieldValue(SolrFieldNames.BRAND));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CATENTRY_ID))) {
      productAndItemSolr.setProductCatentryId((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CATENTRY_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE))) {
      productAndItemSolr
          .setTicketTemplateCode((String) solrDocument.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.STORE_ID))) {
      productAndItemSolr.setStoreId((String) solrDocument.getFieldValue(SolrFieldNames.STORE_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      productAndItemSolr.setProductName((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE))) {
      productAndItemSolr.setCreatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE))) {
      productAndItemSolr.setUpdatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED))) {
      productAndItemSolr.setSuspended((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED))) {
      productAndItemSolr
          .setWholesalePriceActivated((Boolean) solrDocument.getFieldValue(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.OFFER_PRICE))) {
      productAndItemSolr.setOfferPrice((List<String>) solrDocument.getFieldValue(SolrFieldNames.OFFER_PRICE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.LIST_PRICE))) {
      productAndItemSolr.setListPrice((List<String>) solrDocument.getFieldValue(SolrFieldNames.LIST_PRICE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.BUYABLE))) {
      productAndItemSolr.setBuyable((List<String>) solrDocument.getFieldValue(SolrFieldNames.BUYABLE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.DISCOVERABLE))) {
      productAndItemSolr.setDiscoverable((List<String>) solrDocument.getFieldValue(SolrFieldNames.DISCOVERABLE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.SALES_CATALOG))) {
      productAndItemSolr.setSalesCatalog((List<String>) solrDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.ITEM_IMAGES))) {
      productAndItemSolr.setItemImages((List<String>) solrDocument.getFieldValue(SolrFieldNames.ITEM_IMAGES));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.OFFLINE_PRICES))) {
      productAndItemSolr.setOfflinePrices((List<String>) solrDocument.getFieldValue(SolrFieldNames.OFFLINE_PRICES));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.PRISTINE_ID))) {
      productAndItemSolr.setPristineId((String) solrDocument.getFieldValue(SolrFieldNames.PRISTINE_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE))) {
      productAndItemSolr.setFreeSample((Boolean) solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE));
    }
    return productAndItemSolr;
  }

  public static SolrInputDocument toSolrInputDocumentFromSolrDocument(SolrDocument solrDocument) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, solrDocument.getFieldValue(SolrFieldNames.ID));
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED))) {
      solrInputDocument
          .setField(SolrFieldNames.IS_SYNCHRONIZED, solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE))) {
      solrInputDocument
          .setField(SolrFieldNames.MARK_FOR_DELETE, solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE));
    } else if (Objects.nonNull(solrDocument.getFieldValue(SolrConstants.MARK_FOR_DELETE))) {
      solrInputDocument
          .setField(SolrFieldNames.MARK_FOR_DELETE, solrDocument.getFieldValue(SolrConstants.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CNC_ACTIVE))) {
      solrInputDocument
          .setField(SolrFieldNames.CNC_ACTIVE, solrDocument.getFieldValue(SolrFieldNames.CNC_ACTIVE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE))) {
      solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE, solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_LATE_FULFILLMENT))) {
      solrInputDocument
          .setField(SolrFieldNames.IS_LATE_FULFILLMENT, solrDocument.getFieldValue(SolrFieldNames.IS_LATE_FULFILLMENT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED))) {
      solrInputDocument
          .setField(SolrFieldNames.IS_ARCHIVED, solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TRADING_PRODUCT))) {
      solrInputDocument
          .setField(SolrFieldNames.TRADING_PRODUCT, solrDocument.getFieldValue(SolrFieldNames.TRADING_PRODUCT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT))) {
      solrInputDocument
          .setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT, solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED))) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED,
          solrDocument.getFieldValue(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PROMO_BUNDLING))) {
      solrInputDocument
          .setField(SolrFieldNames.PROMO_BUNDLING, solrDocument.getFieldValue(SolrFieldNames.PROMO_BUNDLING));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE))) {
      solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE,
          solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_SKU,
          solrDocument.getFieldValue(SolrFieldNames.ITEM_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_CODE))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_CODE,
          solrDocument.getFieldValue(SolrFieldNames.ITEM_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_SKU))) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_SKU,
          solrDocument.getFieldValue(SolrFieldNames.MERCHANT_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ITEM_NAME))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_NAME,
          solrDocument.getFieldValue(SolrFieldNames.ITEM_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_TYPE))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_TYPE,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_TYPE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE))) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE,
          solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG,
          solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODE))) {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODE,
          solrDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BRAND))) {
      solrInputDocument.setField(SolrFieldNames.BRAND,
          solrDocument.getFieldValue(SolrFieldNames.BRAND));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CATENTRY_ID))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CATENTRY_ID,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CATENTRY_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE))) {
      solrInputDocument.setField(SolrFieldNames.TICKET_TEMPLATE_CODE,
          solrDocument.getFieldValue(SolrFieldNames.TICKET_TEMPLATE_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.STORE_ID))) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID,
          solrDocument.getFieldValue(SolrFieldNames.STORE_ID));
    } else if (Objects.nonNull(solrDocument.getFieldValue(SolrConstants.STORE_ID))) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID,
          solrDocument.getFieldValue(SolrConstants.STORE_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE))) {
      solrInputDocument.setField(SolrFieldNames.CREATED_DATE,
          solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE))) {
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.OFFER_PRICE))) {
      solrInputDocument.setField(SolrFieldNames.OFFER_PRICE,
          solrDocument.getFieldValue(SolrFieldNames.OFFER_PRICE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.LIST_PRICE))) {
      solrInputDocument.setField(SolrFieldNames.LIST_PRICE,
          solrDocument.getFieldValue(SolrFieldNames.LIST_PRICE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.BUYABLE))) {
      solrInputDocument.setField(SolrFieldNames.BUYABLE,
          solrDocument.getFieldValue(SolrFieldNames.BUYABLE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.DISCOVERABLE))) {
      solrInputDocument.setField(SolrFieldNames.DISCOVERABLE,
          solrDocument.getFieldValue(SolrFieldNames.DISCOVERABLE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.SALES_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG,
          solrDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.ITEM_IMAGES))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_IMAGES,
          solrDocument.getFieldValue(SolrFieldNames.ITEM_IMAGES));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.OFFLINE_PRICES))) {
      solrInputDocument.setField(SolrFieldNames.OFFLINE_PRICES,
          solrDocument.getFieldValue(SolrFieldNames.OFFLINE_PRICES));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED))) {
      solrInputDocument.setField(SolrFieldNames.IS_SUSPENDED, solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRISTINE_ID))) {
      solrInputDocument.setField(SolrFieldNames.PRISTINE_ID, solrDocument.getFieldValue(SolrFieldNames.PRISTINE_ID));
    }
    return solrInputDocument;
  }


  // this is not used anywhere in current flow
  public static SolrInputDocument toSolrInputDocumentL3(SolrDocument solrDocument,
      ProductAndItemsVO productAndItemsVO) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    String productSku = (String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU);
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED))) {
      solrInputDocument
          .setField(SolrFieldNames.IS_SYNCHRONIZED, solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE))) {
      solrInputDocument
          .setField(SolrFieldNames.MARK_FOR_DELETE, solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
          solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE))) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE,
          solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG,
          solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BRAND))) {
      solrInputDocument.setField(SolrFieldNames.BRAND,
          solrDocument.getFieldValue(SolrFieldNames.BRAND));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.STORE_ID))) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID,
          solrDocument.getFieldValue(SolrFieldNames.STORE_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE))) {
      solrInputDocument.setField(SolrFieldNames.CREATED_DATE,
          solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE))) {
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.SALES_CATALOG))) {
      solrInputDocument
          .setField(SolrFieldNames.SALES_CATALOG, solrDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.ITEM_IMAGES))) {
      List<String> fieldValue = (List<String>) solrDocument.getFieldValue(SolrFieldNames.ITEM_IMAGES);
      Map<Boolean, String> imageMap = new HashMap<>();
      for (String value : fieldValue) {
        String[] values = value.split(DELIMETER);
        imageMap.put(Boolean.valueOf(values[0]), values[1]);
      }
      solrInputDocument.setField(SolrFieldNames.PRODUCT_MAIN_IMAGE, imageMap.get(Boolean.TRUE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED))) {
      solrInputDocument.setField(SolrFieldNames.IS_SUSPENDED, solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE))) {
      solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE, solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    }
    return solrInputDocument;
  }

  public static void generateL3AggregateDataFromItems(Product product, List<Item> items, SolrInputDocument solrInputDocument,
      List<ItemPickupPoint> itemPickupPointList, boolean isProductHasStock, boolean populateLabelForUpcomingPromo,
      boolean populateLabelForPwpPromo) {
    double variantCount = itemPickupPointList.stream().filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete())
        .map(ItemPickupPoint::getItemSku).distinct().count();
    itemPickupPointList = itemPickupPointList.stream().filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete())
        .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(product.getPickupPointCodes())) {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES, new ArrayList<>(product.getPickupPointCodes()));
    } else {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES,
          itemPickupPointList.stream().filter(itemPickupPoint -> Objects.nonNull(itemPickupPoint.getPickupPointCode()))
              .map(ItemPickupPoint::getPickupPointCode).distinct().collect(Collectors.toList()));
    }
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED, product.isArchived());
    solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE, product.isFreeSample());
    solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, product.isOff2OnChannelActive());
    solrInputDocument.setField(SolrFieldNames.IS_IN_STOCK, isProductHasStock);
    if (CollectionUtils.isNotEmpty(items)) {
      List<Item> mfdFalseItems = items.stream().filter(item -> !item.isMarkForDelete()).collect(Collectors.toList());
      solrInputDocument.setField(SolrFieldNames.VARIANT_COUNT, mfdFalseItems.size());
    } else {
      solrInputDocument.setField(SolrFieldNames.VARIANT_COUNT, variantCount);
    }
    solrInputDocument.setField(SolrFieldNames.L5_COUNT, itemPickupPointList.size());
    List<Double> itemPriceList = getMinMaxPrices(
        itemPickupPointList.stream().flatMap(itemPickupPoint -> itemPickupPoint.getPrice().stream())
            .collect(Collectors.toList()));
    solrInputDocument.setField(SolrFieldNames.MINIMUM_LIST_PRICE, itemPriceList.get(0));
    solrInputDocument.setField(SolrFieldNames.MAXIMUM_LIST_PRICE, itemPriceList.get(1));
    solrInputDocument.setField(SolrFieldNames.MINIMUM_SELLING_PRICE, itemPriceList.get(2));
    solrInputDocument.setField(SolrFieldNames.MAXIMUM_SELLING_PRICE, itemPriceList.get(3));
    Set<String> wholeSaleItemSkus = new HashSet<>();
    Set<String> promoItemSkus = new HashSet<>();
    itemPickupPointList.forEach(
        itemPickupPoint -> generateWholeSaleAndPromoItemSkus(itemPickupPoint, wholeSaleItemSkus, promoItemSkus,
            populateLabelForUpcomingPromo, populateLabelForPwpPromo));
    if (Objects.nonNull(product.getPreOrder()) && Objects.nonNull(product.getPreOrder().getIsPreOrder())) {
      solrInputDocument.setField(SolrFieldNames.IS_PRE_ORDER_ACTIVE, product.getPreOrder().getIsPreOrder());
    }
    if (CollectionUtils.isNotEmpty(wholeSaleItemSkus)) {
      solrInputDocument.setField(SolrFieldNames.WHOLESALE_ITEM_SKUS, wholeSaleItemSkus);
    }
    if (CollectionUtils.isNotEmpty(promoItemSkus)) {
      solrInputDocument.setField(SolrFieldNames.PROMO_ITEM_SKUS, promoItemSkus);
    }
  }

  private static void generateWholeSaleAndPromoItemSkus(Item item, List<String> wholeSaleItemSkus,
      List<String> promoItemSkus) {
    if (isItemAddedToWholesalePrice(item)) {
      wholeSaleItemSkus.add(item.getItemSku());
    }
    if (isItemAddedToPromo(item)) {
      promoItemSkus.add(item.getItemSku());
    }
  }

  private static void generateWholeSaleAndPromoItemSkus(ItemPickupPoint itemPickupPoint, Set<String> wholeSaleItemSkus,
      Set<String> promoItemSkus, boolean populateLabelForUpcomingPromo, boolean populateLabelForPwpPromo) {
    if (isItemAddedToWholesalePrice(itemPickupPoint)) {
      wholeSaleItemSkus.add(itemPickupPoint.getItemSku());
    }
    if (isItemAddedToPromo(itemPickupPoint, populateLabelForUpcomingPromo, populateLabelForPwpPromo)) {
      promoItemSkus.add(itemPickupPoint.getItemSku());
    }
  }

  public static boolean isItemAddedToPromo(Item item) {
    return (item.isPromoBundling() || item.isMerchantPromoDiscount() || isMerchantCampaignActive(item));
  }

  public static boolean isDiscoverableChanged(com.gdn.x.product.model.entity.ItemViewConfig itemViewConfigRequest,
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    return  Objects.nonNull(itemViewConfigRequest) && Objects.nonNull(itemViewConfig) && itemViewConfigRequest.isDiscoverable() != itemViewConfig.isDiscoverable();
  }

  private static boolean isCncChanged(boolean isCncActivatedRequest, boolean isCncActivated) {
    return isCncActivatedRequest && isCncActivatedRequest != isCncActivated;
  }

  public static boolean isPureCNCStatusChange(com.gdn.x.product.model.entity.ItemViewConfig requestVo,
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig, boolean isCncActiveRequest, boolean isCncActive) {
    if (isCncActive && CommonUtil.isDiscoverableChanged(requestVo, itemViewConfig)) {
      return true;
    } else
      return CommonUtil.isCncChanged(isCncActiveRequest, isCncActive);
  }

  public static boolean isItemAddedToPromo(ItemPickupPoint itemPickupPoint, boolean populateLabelForUpcomingPromo,
      boolean populateLabelForPwpPromo) {
    return (itemPickupPoint.isPromoBundling() || itemPickupPoint.isMerchantPromoDiscount() || (
        populateLabelForUpcomingPromo ?
            isMerchantCampaignActiveOrUpcoming(itemPickupPoint) :
            isMerchantCampaignActive(itemPickupPoint)) || (populateLabelForPwpPromo && isPartOfPwpPromo(
        itemPickupPoint)));
  }

  private static List<Double>  getMinMaxPrices(List<com.gdn.x.product.model.entity.Price> priceList) {
    double maxOfferPrice = Constants.DEFAULT_MAX_PRICE;
    double minOfferPrice = Double.MAX_VALUE;
    double maxListPrice = Constants.DEFAULT_MAX_PRICE;
    double minListPrice = Double.MAX_VALUE;
    for (com.gdn.x.product.model.entity.Price price : priceList) {
      if (price.getOfferPrice() > maxOfferPrice) {
        maxOfferPrice = price.getOfferPrice();
      }
      if (price.getOfferPrice() < minOfferPrice) {
        minOfferPrice = price.getOfferPrice();
      }
      if (price.getListPrice() > maxListPrice) {
        maxListPrice = price.getListPrice();
      }
      if (price.getListPrice() < minListPrice) {
        minListPrice = price.getListPrice();
      }
    }
    return Arrays.asList(minListPrice, maxListPrice, minOfferPrice, maxOfferPrice);
  }

  public static SolrInputDocument getSolrInputDocumentForNewFieldsL3Solr(String productSku, Product product,
      List<Item> itemList, boolean productInStock) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, product.getMerchantCode());
    solrInputDocument.setField(SolrFieldNames.VARIANT_COUNT,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemList.size()));
    solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES, Collections
        .singletonMap(SolrConstants.SET_CLAUSE,
            itemList.stream().filter(item -> Objects.nonNull(item.getPickupPointCode()))
                .map(Item::getPickupPointCode).distinct().collect(Collectors.toList())));
    List<Double> itemPriceList = getMinMaxPrices(
        itemList.stream().flatMap(item -> item.getPrice().stream()).collect(Collectors.toList()));
    solrInputDocument.setField(SolrFieldNames.MINIMUM_LIST_PRICE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPriceList.get(0)));
    solrInputDocument.setField(SolrFieldNames.MAXIMUM_LIST_PRICE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPriceList.get(1)));
    solrInputDocument.setField(SolrFieldNames.MINIMUM_SELLING_PRICE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPriceList.get(2)));
    solrInputDocument.setField(SolrFieldNames.MAXIMUM_SELLING_PRICE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPriceList.get(3)));
    solrInputDocument.setField(SolrFieldNames.IS_IN_STOCK,
      Collections.singletonMap(SolrConstants.SET_CLAUSE, productInStock));
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isArchived()));
    solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isOff2OnChannelActive()));
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    List<String> wholeSaleItemSkus = new ArrayList<>();
    List<String> promoItemSkus = new ArrayList<>();
    itemList.forEach(item -> generateWholeSaleAndPromoItemSkus(item, wholeSaleItemSkus, promoItemSkus));
    if (Objects.nonNull(product.getPreOrder()) && Objects.nonNull(product.getPreOrder().getIsPreOrder()))  {
      solrInputDocument.setField(SolrFieldNames.IS_PRE_ORDER_ACTIVE, product.getPreOrder().getIsPreOrder());
    }
    if (CollectionUtils.isNotEmpty(wholeSaleItemSkus)) {
      solrInputDocument.setField(SolrFieldNames.WHOLESALE_ITEM_SKUS, wholeSaleItemSkus);
    }
    if (CollectionUtils.isNotEmpty(promoItemSkus))  {
      solrInputDocument.setField(SolrFieldNames.PROMO_ITEM_SKUS, promoItemSkus);
    }
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForL3Solr(Product product) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, product.getProductSku());
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, product.getMerchantCode());
    solrInputDocument
        .setField(SolrFieldNames.IS_ARCHIVED, Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isArchived()));
    solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isOff2OnChannelActive()));
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static MasterDataProduct updateMasterDataWithDPCMasterData(MasterDataProduct masterDataProduct, MasterDataProduct masterDataProductDPC) {
    masterDataProduct.setDescription(masterDataProductDPC.getDescription());
    masterDataProduct.setLongDescription(masterDataProductDPC.getLongDescription());
    return masterDataProduct;
  }

  public static String getProductRedisKey(String productCode, String storeId) {
    return CacheNames.FIND_PRODUCT_BY_PRODUCT_CODE + Constants.COLON + storeId + Constants.HYPHEN + productCode;
  }

  public static List<ItemPickupPointRequestVo> toItemPickupPointRequestVos(List<ItemInfo> itemInfos) {
    return itemInfos.stream().map(CommonUtil::toItemPickupPointRequestVo).collect(Collectors.toList());
  }

  public static ItemPickupPointRequestVo toItemPickupPointRequestVo(ItemInfo itemInfo) {
    return new ItemPickupPointRequestVo(itemInfo.getItemSku(), itemInfo.getPickupPointCode());
  }
  public static SolrInputDocument toMerchantPromoDiscountSolrInputDocument(Item item, boolean skuStateChange,
      String solrStringDelimiter) {
    GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    if (skuStateChange) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isMerchantPromoDiscount()));
    } else {
      List<String> offerPrice = new ArrayList<String>();
      AtomicBoolean merchantPromoDiscountActivated = new AtomicBoolean(false);
      item.getPrice().forEach(price -> {
        if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
          offerPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter,
              price.getMerchantPromoDiscountPrice().getDiscountPrice()));
          merchantPromoDiscountActivated.set(true);
        } else {
          offerPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter, price.getOfferPrice()));
        }
      });
      solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, merchantPromoDiscountActivated.get()));
      solrInputDocument
          .setField(SolrFieldNames.OFFER_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, offerPrice));
    }
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentOnMerchantPromoDiscountActivatedChange(
      List<ItemPickupPoint> itemPickupPoints) {
    SolrInputDocument solrInputDocument = null;
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, itemPickupPoints.get(0).getItemSku());
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
      solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          itemPickupPoints.stream().anyMatch(itemPickupPoint -> isMPDActive(itemPickupPoint))));
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, itemPickupPoints.get(0).getMerchantCode());
    }
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnToggleArchiveItemAction(Item item,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    if (item.isArchived()) {
      getItemViewConfig(item, solrStringDelimiter, solrInputDocument);
    }
    solrInputDocument
        .setField(SolrFieldNames.IS_ARCHIVED, Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isArchived()));
    solrInputDocument.setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isOff2OnChannelActive()));
    solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isCncActivated()));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges(Item item,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    solrInputDocument.setField(SolrFieldNames.MARK_FOR_DELETE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isMarkForDelete()));
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isArchived()));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getMerchantCode()));
    getItemViewConfig(item, solrStringDelimiter, solrInputDocument);
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnItemViewConfigChanges(
      ItemPickupPoint itemPickupPoint, String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, itemPickupPoint.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPickupPoint.getUpdatedDate()));
    solrInputDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream().map(
            itemViewConfig -> String.format(STRING_FORMATTER, itemViewConfig.getChannel(), solrStringDelimiter,
                itemViewConfig.isBuyable())).collect(Collectors.toList())));
    solrInputDocument.setField(SolrFieldNames.DISCOVERABLE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream().map(
            itemViewConfig -> String.format(STRING_FORMATTER, itemViewConfig.getChannel(), solrStringDelimiter,
                itemViewConfig.isDiscoverable())).collect(Collectors.toList())));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, itemPickupPoint.getMerchantCode());
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnContentChange(Item item) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    if (Objects.isNull(item.getPristineDataItem())) {
      solrInputDocument
          .setField(SolrFieldNames.PRISTINE_ID, Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
    } else {
      solrInputDocument.setField(SolrFieldNames.PRISTINE_ID,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getPristineDataItem().getPristineId()));
    }
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  private static void getItemViewConfig(Item item, String solrStringDelimiter, SolrInputDocument solrInputDocument) {
    List<String> buyable = new ArrayList<>();
    List<String> discoverable = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(item.getItemViewConfigs())) {
      for (com.gdn.x.product.model.entity.ItemViewConfig viewConfig : item.getItemViewConfigs()) {
        buyable.add(String.format(STRING_FORMATTER, viewConfig.getChannel(), solrStringDelimiter, viewConfig.isBuyable()));
        discoverable
            .add(String.format(STRING_FORMATTER, viewConfig.getChannel(), solrStringDelimiter, viewConfig.isDiscoverable()));
      }
      solrInputDocument.setField(SolrFieldNames.BUYABLE, Collections.singletonMap(SolrConstants.SET_CLAUSE, buyable));
      solrInputDocument
          .setField(SolrFieldNames.DISCOVERABLE, Collections.singletonMap(SolrConstants.SET_CLAUSE, discoverable));
    }
  }

  public static List<SolrInputDocument> getSolrDocumentListForAtomicUpdateOnSyncUnsyncAction(ProductAndItemsVO productAndItemsVO) {
    if (CollectionUtils.isNotEmpty(productAndItemsVO.getItems())) {
      return productAndItemsVO.getItems().stream()
          .map(item -> getSolrInputDocumentForAtomicUpdateOnSyncUnsyncAction(item, productAndItemsVO.getProduct()))
          .collect(Collectors.toList());
    }
    return null;
  }

  public static List<SolrInputDocument> getSolrDocumentListForAtomicUpdateOnPromoBundlingFlagChange(List<Item> items,
      boolean promoBundling) {
    return items.stream().map(item -> getSolrInputDocumentForAtomicUpdateOnPromoBundlingFlagChange(item, promoBundling))
        .collect(Collectors.toList());
  }

  private static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnPromoBundlingFlagChange(Item item,
      boolean promoBundling) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    solrInputDocument
        .setField(SolrFieldNames.PROMO_BUNDLING, Collections.singletonMap(SolrConstants.SET_CLAUSE, promoBundling));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  public static List<SolrInputDocument> getSolrInputDocumentForAtomicUpdateOnPriceChanges(List<Item> items,
      String solrStringDelimiter) {
    return items.stream().map(item -> getSolrInputFieldsForPriceChange(item, solrStringDelimiter))
        .collect(Collectors.toList());
  }

  public static List<SolrInputDocument> getSolrInputDocumentForAtomicUpdateOnPristineChange(List<Item> items) {
    return items.stream().map(CommonUtil::getSolrInputFieldsForPristineChange).collect(Collectors.toList());
  }

  private static SolrInputDocument getSolrInputFieldsForPristineChange(Item item) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    if (Objects.nonNull(item.getPristineDataItem())) {
      solrInputDocument.setField(SolrFieldNames.PRISTINE_ID,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getPristineDataItem().getPristineId()));
    } else {
      solrInputDocument.setField(SolrFieldNames.PRISTINE_ID, Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
    }
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  private static SolrInputDocument getSolrInputFieldsForPriceChange(Item item, String solrStringDelimiter) {
    List<String> offerPrice = new ArrayList<>();
    List<String> listPrice = new ArrayList<>();
    boolean merchnatPromoDiscountActivated = false;
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    for (com.gdn.x.product.model.entity.Price price : item.getPrice()) {
      if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
        offerPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter,
            price.getMerchantPromoDiscountPrice().getDiscountPrice()));
        merchnatPromoDiscountActivated = true;
      } else {
        offerPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter, price.getOfferPrice()));
        merchnatPromoDiscountActivated = false;
      }
      listPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter, price.getListPrice()));
    }
    solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, merchnatPromoDiscountActivated));
    solrInputDocument
        .setField(SolrFieldNames.LIST_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, listPrice));
    solrInputDocument
        .setField(SolrFieldNames.OFFER_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, offerPrice));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
    return solrInputDocument;
  }

  private static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnSyncUnsyncAction(Item item, Product product) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
    if (item.isSynchronized()) {
      if (Objects.nonNull(product.getProductScore())) {
        solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getProductScore().getTotalScore()));
      }
      if (Objects.nonNull(item.getMasterDataItem())) {
        solrInputDocument.setField(SolrFieldNames.ITEM_NAME,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getMasterDataItem().getGeneratedItemName()));
      }
      if (Objects.nonNull(product.getMasterDataProduct())) {
        solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getMasterDataProduct().getProductName()));
      }
      if (Objects.isNull(item.getPristineDataItem())) {
        solrInputDocument
            .setField(SolrFieldNames.PRISTINE_ID, Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
      }
    }
    solrInputDocument.setField(SolrFieldNames.IS_SYNCHRONIZED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isSynchronized()));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, product.getMerchantCode());
    return solrInputDocument;
  }

  public static List<SolrInputDocument> getSolrInputDocumentsForAtomicUpdateOnMasterCatalogChanges(Product product,
      List<Item> items, String solrStringDelimiter) {
    return items.stream().map(
        item -> getSolrInputDocumentForMasterCatalogAtomicUpdate(product, item,
            solrStringDelimiter)).collect(Collectors.toList());

  }

  public static List<SolrInputDocument> getSolrInputDocumentsForAtomicUpdateOnSalesCatalogChanges(Product product,
      List<Item> items, String solrStringDelimiter) {
    return items.stream().map(
        item -> getSolrInputDocumentForSalesCatalogAtomicUpdate(product, item,
            solrStringDelimiter)).collect(Collectors.toList());

  }

  private static SolrInputDocument getSolrInputDocumentForSalesCatalogAtomicUpdate(Product product, Item item,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getUpdatedDate()));
    List<String> salesCatalogs = new ArrayList<>();
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      for (Category category : salesCatalog.getListOfCategories()) {
        salesCatalogs.add(String.format(STRING_FORMATTER, salesCatalog.getCatalogCode(),
            solrStringDelimiter, category.getCategoryCode()));
      }
    }
    solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE, salesCatalogs));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, product.getMerchantCode());
    return solrInputDocument;
  }

  private static SolrInputDocument getSolrInputDocumentForMasterCatalogAtomicUpdate(Product product, Item item,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    MasterCatalog masterCatalog = null;
    if (Objects.nonNull(product.getMasterCatalog())) {
      masterCatalog = product.getMasterCatalog();
    } else if (Objects.nonNull(product.getMasterDataProduct())) {
      masterCatalog = product.getMasterDataProduct().getMasterCatalog();
    }
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getUpdatedDate()));
    solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        constructMasterCatalogString(masterCatalog.getCatalogCode(), masterCatalog.getCategory().getCategoryCode(),
            solrStringDelimiter)));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, product.getMerchantCode());
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesForMasterCatalogAtomicUpdateL3Collection(Product product,
      String solrStringDelimiter) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    MasterCatalog masterCatalog = Objects.nonNull(product.getMasterCatalog()) ?
        product.getMasterCatalog() :
        product.getMasterDataProduct().getMasterCatalog();
    fieldsAndValues.put(SolrFieldNames.MASTER_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        constructMasterCatalogString(masterCatalog.getCatalogCode(), masterCatalog.getCategory().getCategoryCode(),
            solrStringDelimiter)));
    return fieldsAndValues;
  }

  public static SolrInputDocument getSolrInputDocumentForMasterCatalogAtomicUpdateL3Collection(Product product,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, product.getProductSku());
    MasterCatalog masterCatalog = Objects.nonNull(product.getMasterCatalog()) ?
        product.getMasterCatalog() :
        product.getMasterDataProduct().getMasterCatalog();
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getUpdatedDate()));
    solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        constructMasterCatalogString(masterCatalog.getCatalogCode(), masterCatalog.getCategory().getCategoryCode(),
            solrStringDelimiter)));
    return solrInputDocument;
  }

  private static String constructMasterCatalogString(String catalogCode, String categoryCode,
      String solrStringDelimiter) {
    return String.format(STRING_FORMATTER, catalogCode, solrStringDelimiter, categoryCode);
  }

  public static boolean checkForDisableUnSyncUpdate(String categoryCodesForDisableUnSync, String categoryCode) {
    if (StringUtils.isEmpty(categoryCodesForDisableUnSync)) {
      return false;
    }
    List<String> categoryCodeList = Arrays.asList(categoryCodesForDisableUnSync.split(Constants.COMMA_DELIMITER));
    if (categoryCodeList.contains(categoryCode)) {
      return true;
    }
    return false;
  }

  public static boolean compareProductTextField(String oldProductTextField, String newProductTextField) {
    oldProductTextField =
        Optional.ofNullable(oldProductTextField).orElse(StringUtils.EMPTY).replace(NEW_LINE, StringUtils.EMPTY)
            .replace(CARRIAGE_RETURN, StringUtils.EMPTY);
    newProductTextField =
        Optional.ofNullable(newProductTextField).orElse(StringUtils.EMPTY).replace(NEW_LINE, StringUtils.EMPTY)
            .replace(CARRIAGE_RETURN, StringUtils.EMPTY);
    if(StringUtils.equals(oldProductTextField, newProductTextField)) {
      return true;
    }
    return false;
  }

  public static SolrInputDocument getUpdateMarkForDelete(ProductSolr productSolr,
      boolean markForDelete) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    solrInputDocument
        .setField(SolrFieldNames.MARK_FOR_DELETE, Collections.singletonMap(SolrConstants.SET_CLAUSE, markForDelete));
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Calendar.getInstance().getTime()));
    return solrInputDocument;
  }

  public static ProductSolr toProductSolr(SolrDocument solrDocument) {
    ProductSolr productSolr = new ProductSolr();
    productSolr.setProductSku((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE))) {
      productSolr.setProductCode((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      productSolr.setProductName((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE))) {
      productSolr.setMerchantCode((String) solrDocument.getFieldValue(SolrFieldNames.MERCHANT_CODE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG))) {
      productSolr.setMasterCatalog((String) solrDocument.getFieldValue(SolrFieldNames.MASTER_CATALOG));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BRAND))) {
      productSolr.setBrand((String) solrDocument.getFieldValue(SolrFieldNames.BRAND));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.STORE_ID))) {
      productSolr.setStoreId((String) solrDocument.getFieldValue(SolrFieldNames.STORE_ID));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE))) {
      productSolr.setMarkForDelete((Boolean) solrDocument.getFieldValue(SolrFieldNames.MARK_FOR_DELETE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED))) {
      productSolr.setSynchronized((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_SYNCHRONIZED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED))) {
      productSolr.setSuspended((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_SUSPENDED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE))) {
      productSolr.setCreatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE))) {
      productSolr.setUpdatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE))) {
      productSolr.setProductScoreTotal((Double) solrDocument.getFieldValue(SolrFieldNames.TOTAL_SCORE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.SALES_CATALOG))) {
      productSolr.setSalesCatalog((List<String>) solrDocument.getFieldValue(SolrFieldNames.SALES_CATALOG));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.PRODUCT_MAIN_IMAGE))) {
      productSolr.setProductMainImage((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_MAIN_IMAGE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE))) {
      productSolr
          .setProductCenterUpdatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.PICKUP_POINT_CODES))) {
      productSolr
          .setPickupPointCodes((List<String>) solrDocument.getFieldValue(SolrFieldNames.PICKUP_POINT_CODES));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED))) {
      productSolr
          .setIsArchived((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_ARCHIVED));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE))) {
      productSolr
          .setOff2OnChannelActive((Boolean) solrDocument.getFieldValue(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.PROMO_ITEM_SKUS))) {
      productSolr
          .setPromoItemSkus((List<String>) solrDocument.getFieldValue(SolrFieldNames.PROMO_ITEM_SKUS));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK))) {
      productSolr
          .setInStock((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_IN_STOCK));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.IS_PRE_ORDER_ACTIVE))) {
      productSolr
          .setIsPreOrderActive((Boolean) solrDocument.getFieldValue(SolrFieldNames.IS_PRE_ORDER_ACTIVE));
    }
    if (CollectionUtils.isNotEmpty(solrDocument.getFieldValues(SolrFieldNames.WHOLESALE_ITEM_SKUS))) {
      productSolr
          .setWholesaleItemSkus((List<String>) solrDocument.getFieldValue(SolrFieldNames.WHOLESALE_ITEM_SKUS));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT))) {
      productSolr
          .setVariantCount((Integer) solrDocument.getFieldValue(SolrFieldNames.VARIANT_COUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE))) {
      productSolr
          .setMaximumSellingPrice((Double) solrDocument.getFieldValue(SolrFieldNames.MAXIMUM_SELLING_PRICE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE))) {
      productSolr
          .setMinimumSellingPrice((Double) solrDocument.getFieldValue(SolrFieldNames.MINIMUM_SELLING_PRICE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE))) {
      productSolr
          .setMaximumListPrice((Double) solrDocument.getFieldValue(SolrFieldNames.MAXIMUM_LIST_PRICE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE))) {
      productSolr
          .setMinimumListPrice((Double) solrDocument.getFieldValue(SolrFieldNames.MINIMUM_LIST_PRICE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE))) {
      productSolr
          .setFreeSample((Boolean) solrDocument.getFieldValue(SolrFieldNames.FREE_SAMPLE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.L5_COUNT))) {
      productSolr
        .setL5Count((int) solrDocument.getFieldValue(SolrFieldNames.L5_COUNT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CURATION_STATUS))) {
      productSolr.setCurationStatus((int) solrDocument.getFieldValue(SolrFieldNames.CURATION_STATUS));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.DISTRIBUTION_STATUS))) {
      productSolr.setDistributionStatus(
          (int) solrDocument.getFieldValue(SolrFieldNames.DISTRIBUTION_STATUS));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BUNDLE_PRODUCT))) {
      productSolr.setBundleProduct((Boolean) solrDocument.getFieldValue(SolrFieldNames.BUNDLE_PRODUCT));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.SIZE_CHART_CODE))) {
      productSolr.setSizeChartCode((String) solrDocument.getFieldValue(SolrFieldNames.SIZE_CHART_CODE));
    }
    return productSolr;
  }

  public static SolrInputDocument toSolrInputDocument(ProductSolr productSolr) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    if (StringUtils.isNotBlank(productSolr.getProductCode())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE, productSolr.getProductCode());
    }
    if (StringUtils.isNotBlank(productSolr.getProductName())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME, productSolr.getProductName());
    }
    if (StringUtils.isNotBlank(productSolr.getMerchantCode())) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, productSolr.getMerchantCode());
    }
    if (StringUtils.isNotBlank(productSolr.getMasterCatalog())) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, productSolr.getMasterCatalog());
    }
    if (StringUtils.isNotBlank(productSolr.getBrand())) {
      solrInputDocument.setField(SolrFieldNames.BRAND, productSolr.getBrand());
    }
    if (StringUtils.isNotBlank(productSolr.getStoreId())) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID, productSolr.getStoreId());
    }
    if(StringUtils.isNotBlank(productSolr.getSizeChartCode())){
      solrInputDocument.setField(SolrFieldNames.SIZE_CHART_CODE, productSolr.getSizeChartCode());
    }
    solrInputDocument.setField(SolrFieldNames.IS_SYNCHRONIZED, productSolr.isSynchronized());
    solrInputDocument.setField(SolrFieldNames.MARK_FOR_DELETE, productSolr.isMarkForDelete());
    solrInputDocument.setField(SolrFieldNames.IS_SUSPENDED, productSolr.isSuspended());
    solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE, productSolr.isFreeSample());
    solrInputDocument.setField(SolrFieldNames.CREATED_DATE, productSolr.getCreatedDate());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE, productSolr.getUpdatedDate());

    if (CollectionUtils.isNotEmpty(productSolr.getSalesCatalog())) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, productSolr.getSalesCatalog());
    }
    if (StringUtils.isNotBlank(productSolr.getProductMainImage())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_MAIN_IMAGE, productSolr.getProductMainImage());
    }
    if (Objects.nonNull(productSolr.getProductScoreTotal())) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, productSolr.getProductScoreTotal());
    }
    if (Objects.nonNull(productSolr.getProductCenterUpdatedDate())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE, productSolr.getProductCenterUpdatedDate());
    }
    solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE, productSolr.isCncActive());
    solrInputDocument.setField(SolrFieldNames.TRADING_PRODUCT, productSolr.isTradingProduct());
    if (Objects.nonNull(productSolr.getMaximumListPrice())) {
      solrInputDocument
        .setField(SolrFieldNames.MAXIMUM_LIST_PRICE, productSolr.getMaximumListPrice());
    }
    if (Objects.nonNull(productSolr.getMaximumSellingPrice())) {
      solrInputDocument
        .setField(SolrFieldNames.MAXIMUM_SELLING_PRICE, productSolr.getMaximumSellingPrice());
    }
    if (Objects.nonNull(productSolr.getMinimumSellingPrice())) {
      solrInputDocument
        .setField(SolrFieldNames.MINIMUM_SELLING_PRICE, productSolr.getMinimumSellingPrice());
    }
    if (Objects.nonNull(productSolr.getMinimumListPrice())) {
      solrInputDocument
        .setField(SolrFieldNames.MINIMUM_LIST_PRICE, productSolr.getMinimumListPrice());
    }
    if (CollectionUtils.isNotEmpty(productSolr.getPickupPointCodes())) {
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES, productSolr.getPickupPointCodes());
    }
    if (Objects.nonNull(productSolr.getIsPreOrderActive())) {
      solrInputDocument
        .setField(SolrFieldNames.IS_PRE_ORDER_ACTIVE, productSolr.getIsPreOrderActive());
    }
    if (Objects.nonNull(productSolr.getInStock())) {
      solrInputDocument.setField(SolrFieldNames.IS_IN_STOCK, productSolr.getInStock());
    }
    if (Objects.nonNull(productSolr.getOff2OnChannelActive())) {
      solrInputDocument
        .setField(SolrFieldNames.OFF2ON_CHANNEL_ACTIVE, productSolr.getOff2OnChannelActive());
    }
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED, productSolr.getIsArchived());
    solrInputDocument.setField(SolrFieldNames.FBB_ACTIVATED, productSolr.isFbbActivated());
    solrInputDocument.setField(SolrFieldNames.B2B_ACTIVATED, productSolr.isB2bActivated());
    solrInputDocument.setField(SolrFieldNames.B2C_ACTIVATED, productSolr.getB2cActivated());
    solrInputDocument.setField(SolrFieldNames.L5_COUNT, productSolr.getL5Count());
    solrInputDocument.setField(SolrFieldNames.VARIANT_COUNT, productSolr.getVariantCount());
    solrInputDocument.setField(SolrFieldNames.CURATION_STATUS, productSolr.getCurationStatus());
    solrInputDocument.setField(SolrFieldNames.BUNDLE_PRODUCT, productSolr.isBundleProduct());
    solrInputDocument.setField(SolrFieldNames.DISTRIBUTION_STATUS,
        productSolr.getDistributionStatus());
    return solrInputDocument;
  }

  /**
   * atomic update for the fields which are set in ProductSolrConstructorServiceImpl.constructProduct
   * Its will be used in current update apis to preserve the min-max prices, pickupPoint codes and instock flag
   */
  public static SolrInputDocument toSolrInputDocumentL3AtomicUpdate(ProductSolr productSolr) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    if (StringUtils.isNotBlank(productSolr.getProductCode())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CODE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductCode()));
    }
    if (StringUtils.isNotBlank(productSolr.getProductName())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductName()));
    }
    if (StringUtils.isNotBlank(productSolr.getMerchantCode())) {
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getMerchantCode()));
    }
    if (StringUtils.isNotBlank(productSolr.getMasterCatalog())) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getMasterCatalog()));
    }
    if (StringUtils.isNotBlank(productSolr.getBrand())) {
      solrInputDocument
          .setField(SolrFieldNames.BRAND, Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getBrand()));
    }
    if (StringUtils.isNotBlank(productSolr.getStoreId())) {
      solrInputDocument.setField(SolrFieldNames.STORE_ID,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getStoreId()));
    }
    solrInputDocument.setField(SolrFieldNames.IS_SYNCHRONIZED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.isSynchronized()));
    solrInputDocument.setField(SolrFieldNames.MARK_FOR_DELETE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.isMarkForDelete()));
    solrInputDocument.setField(SolrFieldNames.IS_SUSPENDED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.isSuspended()));
    solrInputDocument.setField(SolrFieldNames.FREE_SAMPLE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.isFreeSample()));
    solrInputDocument.setField(SolrFieldNames.CREATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getCreatedDate()));
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getUpdatedDate()));
    if (CollectionUtils.isNotEmpty(productSolr.getSalesCatalog())) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getSalesCatalog()));
    }
    if (StringUtils.isNotBlank(productSolr.getProductMainImage())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_MAIN_IMAGE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductMainImage()));
    }
    if (Objects.nonNull(productSolr.getProductScoreTotal())) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductScoreTotal()));
    }
    if (Objects.nonNull(productSolr.getProductCenterUpdatedDate())) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productSolr.getProductCenterUpdatedDate()));
    }
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForProductAndItemSolrFlagsUpdate(ProductAndItemSolr productAndItemSolr,
      boolean isSuspended, Map<String, Boolean> archiveItems) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, productAndItemSolr.getItemSku());
    solrInputDocument
        .setField(SolrFieldNames.IS_SUSPENDED, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    solrInputDocument
        .setField(SolrFieldNames.MARK_FOR_DELETE, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    boolean isArchived = archiveItems.get(productAndItemSolr.getItemSku());
    solrInputDocument
        .setField(SolrFieldNames.IS_ARCHIVED, Collections.singletonMap(SolrConstants.SET_CLAUSE, isArchived));
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Calendar.getInstance().getTime()));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, productAndItemSolr.getMerchantCode());
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentForProductSolrFlagsUpdate(ProductSolr productSolr,
      boolean isSuspended, boolean productArchivedFlag) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    solrInputDocument
        .setField(SolrFieldNames.IS_SUSPENDED, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    solrInputDocument
        .setField(SolrFieldNames.IS_ARCHIVED, Collections.singletonMap(SolrConstants.SET_CLAUSE, productArchivedFlag));
    solrInputDocument
        .setField(SolrFieldNames.MARK_FOR_DELETE, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Calendar.getInstance().getTime()));
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesForProductSolrFlagsUpdate(boolean isSuspended, boolean productArchivedFlag) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    fieldsAndValues.put(SolrFieldNames.IS_SUSPENDED, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    fieldsAndValues.put(SolrFieldNames.IS_ARCHIVED, Collections.singletonMap(SolrConstants.SET_CLAUSE, productArchivedFlag));
    fieldsAndValues.put(SolrFieldNames.MARK_FOR_DELETE, Collections.singletonMap(SolrConstants.SET_CLAUSE, isSuspended));
    return fieldsAndValues;
  }

  public static List<SolrInputDocument> getSolrInputDocumentsForL3ListAtomicUpdateOnSalesCatalogChanges(
      Map<String, Product> productMap, List<Item> items, String solrStringDelimiter) {
    return items.stream().map(
        item -> getSolrInputDocumentForSalesCatalogAtomicUpdate(productMap.get(item.getProductSku()), item,
            solrStringDelimiter)).collect(Collectors.toList());

  }

  public static List<SolrInputDocument> getSolrInputDocumentsForAtomicUpdateOnProductsSalesCatalogChanges(
      List<Product> products, String solrStringDelimiter) {
    return products.stream()
        .map(product -> getSolrInputDocumentsForAtomicUpdateOnProductSalesCatalogChanges(product, solrStringDelimiter))
        .collect(Collectors.toList());
  }

  public static Map<String, Object> getFieldsAndValuesForAtomicUpdateOnProductSalesCatalogChanges(Product product,
      String solrStringDelimiter) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    List<String> salesCatalogs = new ArrayList<>();
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      salesCatalogs.addAll(Optional.ofNullable(salesCatalog.getListOfCategories()).orElse(new ArrayList<>()).stream()
          .map(category -> String
              .format(STRING_FORMATTER, salesCatalog.getCatalogCode(), solrStringDelimiter, category.getCategoryCode()))
          .collect(Collectors.toList()));
    }
    fieldsAndValues.put(SolrFieldNames.SALES_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE, salesCatalogs));
    fieldsAndValues.put(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE, product.getProductCenterUpdatedDate());
    return fieldsAndValues;
  }

  public static SolrInputDocument getSolrInputDocumentsForAtomicUpdateOnProductSalesCatalogChanges(Product product,
      String solrStringDelimiter) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, product.getProductSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getUpdatedDate()));
    List<String> salesCatalogs = new ArrayList<>();
    for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
      salesCatalogs.addAll(Optional.ofNullable(salesCatalog.getListOfCategories()).orElse(new ArrayList<>()).stream()
          .map(category -> String
              .format(STRING_FORMATTER, salesCatalog.getCatalogCode(), solrStringDelimiter, category.getCategoryCode()))
          .collect(Collectors.toList()));
    }
    solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE, salesCatalogs));
    solrInputDocument.setField(SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getProductCenterUpdatedDate()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrDocumentListForProductAtomicUpdateOnSyncUnsyncAction(Product product) {
    if(Objects.isNull(product)) {
      return null;
    }
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, product.getProductSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getUpdatedDate()));
    if (product.isSynchronized()) {
      if (Objects.nonNull(product.getMasterDataProduct())) {
        solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getMasterDataProduct().getProductName()));
      }
      if (Objects.nonNull(product.getProductScore())) {
        solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getProductScore().getTotalScore()));
      }
    }
    solrInputDocument.setField(SolrFieldNames.IS_SYNCHRONIZED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isSynchronized()));
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesForProductAtomicUpdateOnSyncUnsyncAction(Product product) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    if (product.isSynchronized() && Objects.nonNull(product.getProductScore())) {
      fieldsAndValues.put(SolrFieldNames.TOTAL_SCORE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, product.getProductScore().getTotalScore()));
    }
    fieldsAndValues.put(SolrFieldNames.IS_SYNCHRONIZED, Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isSynchronized()));
    return fieldsAndValues;
  }

  public static MasterCatalog constructMasterCatalog(ProductCategoryResponse productCategoryResponse) {
    MasterCatalog masterCatalog = new MasterCatalog();
    if(Objects.nonNull(productCategoryResponse.getCategory())) {
      if(Objects.nonNull(productCategoryResponse.getCategory().getCatalog())) {
        masterCatalog.setCatalogCode(productCategoryResponse.getCategory().getCatalog().getCatalogCode());
      }
      masterCatalog.setCategory(new Category(productCategoryResponse.getCategory().getCategoryCode(),
          productCategoryResponse.getCategory().getCategoryCode()));
    }
    return masterCatalog;
  }

  public static void checkProductsForForceReview(List<Product> productList) {
    checkState(productList.stream().noneMatch(product -> product.isForceReview()),
        ErrorMessages.PRODUCT_AND_ITEMS_NOT_VIEWABLE);
  }

  public static void checkItemsForForceReview(List<Item> itemList) {
    checkState(itemList.stream().noneMatch(item -> item.isForceReview()), ErrorMessages.PRODUCT_AND_ITEMS_NOT_VIEWABLE);
  }

  public static ProductScoreRequest getProductScoreRequest(Product product, ProductDetailResponse productResponse)
      throws Exception {
    String categoryCode = "";
    try {
      if (CollectionUtils.isNotEmpty(productResponse.getProductCategoryResponses())) {
        categoryCode = productResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
      }
    } catch (Exception ex) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "CategoryCode is null or empty");
    }
    if (StringUtils.isBlank(categoryCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT, "CategoryCode can't be empty");
    }
    ProductScoreRequest productScoreRequest =
        ProductScoreRequest.builder().categoryCode(categoryCode).productCode(productResponse.getProductCode())
            .brand(productResponse.getBrand()).name(productResponse.getName())
            .uniqueSellingPoint(productResponse.getUniqueSellingPoint()).url(productResponse.getUrl())
            .itemRequests(new ArrayList<>()).synchronised(product.isSynchronized())
            .productAttributeRequests(new ArrayList<>()).build();
    if (Objects.nonNull(productResponse.getDescription())) {
      productScoreRequest
          .setDescription(Arrays.copyOf(productResponse.getDescription(), productResponse.getDescription().length));
    }
    Map<String, String> attributeCodeValueMap =
        Optional.ofNullable(product.getProductSpecialAttributes()).orElse(new ArrayList<>()).stream()
            .filter(Objects::nonNull).collect(
            Collectors.toMap(ProductSpecialAttribute::getAttributeCode, ProductSpecialAttribute::getAttributeValue));
    for (ProductAttributeResponse productAttributeResponse : productResponse.getProductAttributeResponses()) {
      AttributeScoreRequest attributeScoreRequest =
          AttributeScoreRequest.builder().attributeCode(productAttributeResponse.getAttribute().getAttributeCode())
              .attributeType(productAttributeResponse.getAttribute().getAttributeType())
              .basicView(productAttributeResponse.getAttribute().isBasicView())
              .mandatory(productAttributeResponse.getAttribute().isMandatory())
              .name(productAttributeResponse.getAttribute().getName())
              .skuValue(productAttributeResponse.getAttribute().isSkuValue())
              .variantCreation(productAttributeResponse.getAttribute().isVariantCreation()).values(new ArrayList<>())
              .build();
      if (attributeCodeValueMap.containsKey(productAttributeResponse.getAttribute().getAttributeCode())) {
        attributeScoreRequest.setValues(Collections
            .singletonList(attributeCodeValueMap.get(productAttributeResponse.getAttribute().getAttributeCode())));
      } else {
        List<String> values = new ArrayList<>();
        for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse
            .getProductAttributeValues()) {
          String value = "";
          if (Objects.nonNull(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
            if (DescriptiveAttributeValueType.SINGLE
                .equals(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
              value = productAttributeValueResponse.getDescriptiveAttributeValue();
            } else if (DescriptiveAttributeValueType.PREDEFINED
                .equals(productAttributeValueResponse.getDescriptiveAttributeValueType()) && Objects
                .nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
              value = productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue();
            } else if (DescriptiveAttributeValueType.NONE
                .equals(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
              value = productAttributeValueResponse.getAllowedAttributeValue().getValue();
            }
          }
          values.add(value);
        }
        attributeScoreRequest.setValues(values);
      }
      productScoreRequest.getProductAttributeRequests().add(attributeScoreRequest);
    }
    for (ProductItemResponse productItemResponse : productResponse.getProductItemResponses()) {
      ItemScoreRequest itemScoreRequest =
          ItemScoreRequest.builder().upcCode(productItemResponse.getUpcCode()).itemImages(new ArrayList<>()).build();
      for (Image image : productItemResponse.getImages()) {
        if (filterProcessedImages(image)) {
          MasterDataProductImageDTO imageRequest = new MasterDataProductImageDTO();
          imageRequest.setLocationPath(image.getLocationPath());
          imageRequest.setMainImage(image.isMainImages());
          imageRequest.setSequence(image.getSequence());
          imageRequest.setProductCode(productResponse.getProductCode());
          imageRequest.setCommonImage(image.isCommonImage());
          itemScoreRequest.getItemImages().add(imageRequest);
        }
      }
      productScoreRequest.getItemRequests().add(itemScoreRequest);
    }
    log.info("Generated product score request : {}", productScoreRequest);
    return productScoreRequest;
  }

  public static List<AttributeScoreRequest> generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(
      ProductDetailResponse productDetailResponse, Product product) {
    List<AttributeScoreRequest> attributeScoreRequestList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
        if (!productAttributeResponse.getAttribute().isSkuValue()) {
          AttributeScoreRequest attributeScoreRequest =
              AttributeScoreRequest.builder().attributeCode(productAttributeResponse.getAttribute().getAttributeCode())
                  .attributeType(productAttributeResponse.getAttribute().getAttributeType())
                  .basicView(productAttributeResponse.getAttribute().isBasicView())
                  .mandatory(productAttributeResponse.getAttribute().isMandatory())
                  .name(productAttributeResponse.getAttribute().getName())
                  .skuValue(productAttributeResponse.getAttribute().isSkuValue())
                  .variantCreation(productAttributeResponse.getAttribute().isVariantCreation()).build();
          List<String> values = new ArrayList<>();
          if (StringUtils.isNotEmpty(productAttributeResponse.getAttribute().getAttributeType())) {
            for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse
                .getProductAttributeValues()) {
              if (MasterDataAttributeType.DEFINING_ATTRIBUTE.name()
                  .equals(productAttributeResponse.getAttribute().getAttributeType())) {
                Optional.ofNullable(productAttributeValueResponse.getAllowedAttributeValue())
                  .map(AllowedAttributeValueResponse::getValue).ifPresent(values::add);
              } else if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.name()
                  .equals(productAttributeResponse.getAttribute().getAttributeType())) {
                values.add(productAttributeValueResponse.getDescriptiveAttributeValue());
              } else if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name()
                  .equals(productAttributeResponse.getAttribute().getAttributeType()) && Objects
                  .nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
                Optional.ofNullable(
                    productAttributeValueResponse.getPredefinedAllowedAttributeValue())
                  .map(PredefinedAllowedAttributeValueResponse::getValue).ifPresent(values::add);
              }
            }
          }
          attributeScoreRequest.setValues(values);
          attributeScoreRequestList.add(attributeScoreRequest);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(product.getProductSpecialAttributes())) {
      // Other fields are populated in ProductScoreUtilBean
      for (ProductSpecialAttribute productSpecialAttribute : product.getProductSpecialAttributes()) {
        AttributeScoreRequest attributeScoreRequest =
            AttributeScoreRequest.builder().attributeCode(productSpecialAttribute.getAttributeCode())
                .name(productSpecialAttribute.getAttributeName()).skuValue(true)
                .values(Collections.singletonList(productSpecialAttribute.getAttributeValue())).build();
        attributeScoreRequestList.add(attributeScoreRequest);
      }
    }
    return attributeScoreRequestList;
  }

  public static List<ItemScoreRequest> generateItemScoreRequestsWithoutImages(List<Item> items) {
    List<ItemScoreRequest> itemScoreRequestList = new ArrayList<>();
    for(Item item : items) {
      if(Objects.nonNull(item.getMasterDataItem())) {
        itemScoreRequestList.add(new ItemScoreRequest(item.getMasterDataItem().getUpcCode(), new ArrayList<>()));
      }
    }
    return itemScoreRequestList;
  }

  public static List<AttributeScoreRequest> generateAttributeScoreRequestsForUnsyncL3(Product product) {
    List<AttributeScoreRequest> attributeScoreRequestList = new ArrayList<>();
    if (Objects.nonNull(product.getMasterDataProduct()) && CollectionUtils
        .isNotEmpty(product.getMasterDataProduct().getMasterDataProductAttributes())) {
      for (com.gdn.x.product.model.entity.MasterDataProductAttribute masterDataProductAttribute : product
          .getMasterDataProduct().getMasterDataProductAttributes()) {
        if (!masterDataProductAttribute.getMasterDataAttribute().isSkuValue()) {
          AttributeScoreRequest attributeScoreRequest = AttributeScoreRequest.builder()
              .attributeCode(masterDataProductAttribute.getMasterDataAttribute().getAttributeCode()).attributeType(
                  Objects.nonNull(masterDataProductAttribute.getMasterDataAttribute().getAttributeType()) ?
                      masterDataProductAttribute.getMasterDataAttribute().getAttributeType().name() :
                      StringUtils.EMPTY).basicView(masterDataProductAttribute.getMasterDataAttribute().isBasicView())
              .mandatory(masterDataProductAttribute.getMasterDataAttribute().isMandatory())
              .name(masterDataProductAttribute.getMasterDataAttribute().getAttributeName())
              .skuValue(masterDataProductAttribute.getMasterDataAttribute().isSkuValue())
              .variantCreation(masterDataProductAttribute.getMasterDataAttribute().isVariantCreation()).build();
          List<String> values = new ArrayList<>();
          if (Objects.nonNull(masterDataProductAttribute.getMasterDataAttribute().getAttributeType())) {
            for (MasterDataProductAttributeValue masterDataProductAttributeValue : masterDataProductAttribute.getMasterDataProductAttributeValues()) {
              if (MasterDataAttributeType.DEFINING_ATTRIBUTE.name()
                  .equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeType().name())) {
                values.add(masterDataProductAttributeValue.getAllowedAttributeValue().getValue());
              } else if (MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.name()
                  .equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeType().name())) {
                values.add(masterDataProductAttributeValue.getDescriptiveAttributeValue());
              } else if (MasterDataAttributeType.PREDEFINED_ATTRIBUTE.name()
                  .equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeType().name()) && Objects
                  .nonNull(masterDataProductAttributeValue.getPredefinedAllowedAttributeValue())) {
                values.add(masterDataProductAttributeValue.getPredefinedAllowedAttributeValue().getValue());
              }
            }
          }
          attributeScoreRequest.setValues(values);
          attributeScoreRequestList.add(attributeScoreRequest);
        }
      }
    }
    Optional.ofNullable(product.getProductSpecialAttributes()).orElse(new ArrayList<>()).forEach(
        productSpecialAttribute -> attributeScoreRequestList.add(
            AttributeScoreRequest.builder().skuValue(true).name(productSpecialAttribute.getAttributeName())
                .attributeCode(productSpecialAttribute.getAttributeCode())
                .values(Collections.singletonList(productSpecialAttribute.getAttributeValue())).build()));
    return attributeScoreRequestList;
  }

  public static Map<String, FieldValueObject> checkIfSolrUpdateNeededForProductAndItemSolr(
      ProductAndItemSolr existingData, ProductAndItemSolr productAndItemSolr) {
    Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
    if (!existingData.getItemName().equals(productAndItemSolr.getItemName())) {
      fieldValueObjectMap.put(SolrFieldNames.ITEM_NAME,
          FieldValueObject.builder().oldValue(existingData.getItemName()).newValue(productAndItemSolr.getItemName())
              .build());
    }
    if (!existingData.getProductName().equals(productAndItemSolr.getProductName())) {
      fieldValueObjectMap.put(SolrFieldNames.PRODUCT_NAME,
          FieldValueObject.builder().oldValue(existingData.getProductName())
              .newValue(productAndItemSolr.getProductName()).build());
    }
    if (!existingData.getProductType().equals(productAndItemSolr.getProductType())) {
      fieldValueObjectMap.put(SolrFieldNames.PRODUCT_TYPE,
          FieldValueObject.builder().oldValue(existingData.getProductType())
              .newValue(productAndItemSolr.getProductType()).build());
    }
    if (!existingData.getBrand().equals(productAndItemSolr.getBrand())) {
      fieldValueObjectMap.put(SolrFieldNames.BRAND,
          FieldValueObject.builder().oldValue(existingData.getBrand()).newValue(productAndItemSolr.getBrand()).build());
    }
    if (!existingData.getMasterCatalog().equals(productAndItemSolr.getMasterCatalog())) {
      fieldValueObjectMap.put(SolrFieldNames.MASTER_CATALOG,
          FieldValueObject.builder().oldValue(existingData.getMasterCatalog())
              .newValue(productAndItemSolr.getMasterCatalog()).build());
    }
    if (!CommonUtil.isEqualList(existingData.getSalesCatalog(), productAndItemSolr.getSalesCatalog())) {
      fieldValueObjectMap.put(SolrFieldNames.SALES_CATALOG,
          FieldValueObject.builder().oldListValues(existingData.getSalesCatalog())
              .newListValues(productAndItemSolr.getSalesCatalog()).build());
    }
    if (!CommonUtil.isEqualList(existingData.getItemImages(), productAndItemSolr.getItemImages())) {
      fieldValueObjectMap.put(SolrFieldNames.ITEM_IMAGES,
          FieldValueObject.builder().oldListValues(existingData.getItemImages())
              .newListValues(productAndItemSolr.getItemImages()).build());
    }
    if (Objects.nonNull(productAndItemSolr.getProductScoreTotal()) && !productAndItemSolr.getProductScoreTotal()
        .equals(existingData.getProductScoreTotal())) {
      fieldValueObjectMap.put(SolrFieldNames.TOTAL_SCORE,
          FieldValueObject.builder().oldValue(String.valueOf(existingData.getProductScoreTotal()))
              .newValue(String.valueOf(productAndItemSolr.getProductScoreTotal())).build());
    }
    return fieldValueObjectMap;
  }

  public static SolrInputDocument getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductPublish(
      ProductAndItemSolr productAndItemSolr, Map<String, FieldValueObject> fieldValueObjectMap) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, productAndItemSolr.getId());
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.ITEM_NAME))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_NAME, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.ITEM_NAME).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.PRODUCT_NAME))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.PRODUCT_NAME).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.PRODUCT_TYPE))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_TYPE, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.PRODUCT_TYPE).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.MASTER_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.MASTER_CATALOG).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG).getNewListValues()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.ITEM_IMAGES))) {
      solrInputDocument.setField(SolrFieldNames.ITEM_IMAGES, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.ITEM_IMAGES).getNewListValues()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.BRAND))) {
      solrInputDocument.setField(SolrFieldNames.BRAND, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.BRAND).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          Double.parseDouble(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE).getNewValue())));
    }
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentFromProductAndItemSolrForAtomicUpdateOnProductScoreUpdatePublish(
      ProductAndItemSolr productAndItemSolr, Map<String, FieldValueObject> fieldValueObjectMap) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, productAndItemSolr.getId());
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          Double.parseDouble(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE).getNewValue())));
    }
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static Map<String, FieldValueObject> checkIfSolrUpdateNeededForProductSolr(ProductSolr existingProductSolr,
      ProductSolr productSolr) {
    Map<String, FieldValueObject> fieldValueObjectMap = new HashMap<>();
    if (!existingProductSolr.getProductName().equals(productSolr.getProductName())) {
      fieldValueObjectMap.put(SolrFieldNames.PRODUCT_NAME,
          FieldValueObject.builder().oldValue(existingProductSolr.getProductName())
              .newValue(productSolr.getProductName()).build());
    }

    if (!existingProductSolr.getBrand().equals(productSolr.getBrand())) {
      fieldValueObjectMap.put(SolrFieldNames.BRAND,
          FieldValueObject.builder().oldValue(existingProductSolr.getBrand()).newValue(productSolr.getBrand()).build());
    }
    if (!existingProductSolr.getMasterCatalog().equals(productSolr.getMasterCatalog())) {
      fieldValueObjectMap.put(SolrFieldNames.MASTER_CATALOG,
          FieldValueObject.builder().oldValue(existingProductSolr.getMasterCatalog())
              .newValue(productSolr.getMasterCatalog()).build());
    }

    if (!CommonUtil.isEqualList(existingProductSolr.getSalesCatalog(), productSolr.getSalesCatalog())) {
      fieldValueObjectMap.put(SolrFieldNames.SALES_CATALOG,
          FieldValueObject.builder().oldListValues(existingProductSolr.getSalesCatalog())
              .newListValues(productSolr.getSalesCatalog()).build());
    }
    if (!existingProductSolr.getProductMainImage().equals(productSolr.getProductMainImage())) {
      fieldValueObjectMap.put(SolrFieldNames.PRODUCT_MAIN_IMAGE,
          FieldValueObject.builder().oldValue(existingProductSolr.getProductMainImage())
              .newValue(productSolr.getProductMainImage()).build());
    }
    if (Objects.nonNull(productSolr.getProductScoreTotal()) && !productSolr.getProductScoreTotal()
        .equals(existingProductSolr.getProductScoreTotal())) {
      fieldValueObjectMap.put(SolrFieldNames.TOTAL_SCORE,
          FieldValueObject.builder().newValue(String.valueOf(productSolr.getProductScoreTotal()))
              .oldValue(String.valueOf(existingProductSolr.getProductScoreTotal())).build());
    }
    return fieldValueObjectMap;
  }

  public static SolrInputDocument getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductScoreUpdate(
      ProductSolr productSolr, Map<String, FieldValueObject> fieldValueObjectMap) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          Double.parseDouble(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE).getNewValue())));
    }
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentFromProductSolrForAtomicUpdateOnProductPublish(
      ProductSolr productSolr, Map<String, FieldValueObject> fieldValueObjectMap) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSolr.getProductSku());

    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.PRODUCT_NAME))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_NAME, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.PRODUCT_NAME).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.MASTER_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.MASTER_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.MASTER_CATALOG).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG))) {
      solrInputDocument.setField(SolrFieldNames.SALES_CATALOG, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.SALES_CATALOG).getNewListValues()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.BRAND))) {
      solrInputDocument.setField(SolrFieldNames.BRAND, Collections
          .singletonMap(SolrConstants.SET_CLAUSE, fieldValueObjectMap.get(SolrFieldNames.BRAND).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.PRODUCT_MAIN_IMAGE))) {
      solrInputDocument.setField(SolrFieldNames.PRODUCT_MAIN_IMAGE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          fieldValueObjectMap.get(SolrFieldNames.PRODUCT_MAIN_IMAGE).getNewValue()));
    }
    if (Objects.nonNull(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE))) {
      solrInputDocument.setField(SolrFieldNames.TOTAL_SCORE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
          Double.parseDouble(fieldValueObjectMap.get(SolrFieldNames.TOTAL_SCORE).getNewValue())));
    }
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static List<String> getPromoTypesByItem(ItemPickupPoint itemPickupPoint) {
    List<String> promoTypes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
      if (itemPickupPoint.isMerchantPromoDiscount()) {
        promoTypes.add(PromoType.PROMO_DISCOUNT.getDescription());
      }
      if (itemPickupPoint.isPromoBundling()) {
        promoTypes.add(PromoType.PROMO_BUNDLING.getDescription());
      }
      if (isMerchantCampaignActive(itemPickupPoint)) {
        promoTypes.add(PromoType.CAMPAIGN.getDescription());
      }
    }
    return promoTypes;
  }

  public static void setPromoLabelsAtL4(ProductSolr productSolr, ItemPickupPoint itemPickupPoint, Item item,
      ItemL4SummaryResponse itemL4SummaryResponse, boolean populateLabelForUpcomingPromo,
      boolean populateLabelForPwpPromo) {
    if (CommonUtil.isItemAddedToPromo(itemPickupPoint, populateLabelForUpcomingPromo, populateLabelForPwpPromo)) {
      itemL4SummaryResponse.getPromoLabels().add(SolrConstants.PROMO_PROMO_TYPE);
    }
    if (itemPickupPoint.isWholesalePriceExists() && CollectionUtils.isNotEmpty(
        itemPickupPoint.getActivePromoBundlings()) && itemPickupPoint.getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE)) {
      itemL4SummaryResponse.getPromoLabels().add(Constants.WHOLESALE_PRICE);
    }
    if (item.isOff2OnChannelActive()) {
      itemL4SummaryResponse.getPromoLabels().add(SolrConstants.IN_STORE_PROMO_TYPE);
    }
    if (Objects.nonNull(productSolr.getIsPreOrderActive()) && productSolr.getIsPreOrderActive()) {
      itemL4SummaryResponse.getPromoLabels().add(SolrConstants.PREORDER_PROMO_TYPE);
    }
    if (item.isFreeSample()) {
      itemL4SummaryResponse.getPromoLabels().add(SolrConstants.FREE_SAMPLE_TYPE);
    }
  }

  public static Set<String> getPriceEditDisabledReasons(Item item) {
    Set<String> priceEditDisabledReasons = new HashSet<>();
    if (CollectionUtils.isNotEmpty(item.getPrice())) {
      if (item.isMerchantPromoDiscount()) {
        priceEditDisabledReasons.add(PromoType.PROMO_DISCOUNT.getDescription());
      }
      if (isMerchantCampaignActive(item)) {
        priceEditDisabledReasons.add(PromoType.CAMPAIGN.getDescription());
      }
    }
    return priceEditDisabledReasons;
  }

  public static Set<String> getPriceEditDisabledReasons(ItemPickupPoint itemPickupPoint) {
    Set<String> priceEditDisabledReasons = new HashSet<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
      if (itemPickupPoint.isMerchantPromoDiscount()) {
        priceEditDisabledReasons.add(PromoType.PROMO_DISCOUNT.getDescription());
      }
      if (isMerchantCampaignActive(itemPickupPoint)) {
        priceEditDisabledReasons.add(PromoType.CAMPAIGN.getDescription());
      }
    }
    return priceEditDisabledReasons;
  }

  private static boolean isMerchantCampaignActive(Item item) {
    Date now = new Date();
    return item.getPrice()
        .stream()
        .anyMatch(price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices()
            .stream()
            .anyMatch(discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode())
                && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  private static boolean isMerchantCampaignActive(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.getPrice()
        .stream()
        .anyMatch(price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices()
            .stream()
            .anyMatch(discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode())
                && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  public static boolean isMerchantCampaignActiveOrUpcoming(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.getPrice()
        .stream()
        .anyMatch(price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices()
            .stream()
            .anyMatch(discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode())
                 && discountPrice.getEndDateTime().after(now))));
  }

  public static boolean isPartOfPwpPromo(ItemPickupPoint itemPickupPoint) {
    return Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>()).stream().anyMatch(
        promo -> Arrays.asList(Constants.PWP_ADDITIONAL_ACTIVE, Constants.PWP_ADDITIONAL_PENDING,
            Constants.PWP_MAIN_ACTIVE, Constants.PWP_MAIN_PENDING).contains(promo));
  }

  private static boolean isMPDActive(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.getPrice().stream().anyMatch(
        price -> Objects.nonNull(price.getMerchantPromoDiscountPrice()) && price.getMerchantPromoDiscountPrice()
            .getStartDateTime().before(now) && price.getMerchantPromoDiscountPrice().getEndDateTime().after(now));
  }

  public static ProductSummaryRequestVo getProductSummaryRequestVo(ProductSummaryRequest productSummaryRequest) {
    return ProductSummaryRequestVo.builder()
        .categoryCodes(productSummaryRequest.getCategoryCodes())
        .archived(productSummaryRequest.getArchived())
        .inStock(productSummaryRequest.getInStock())
        .keyword(productSummaryRequest.getKeyword())
        .merchantCode(productSummaryRequest.getMerchantCode())
        .pickupPointCodes(productSummaryRequest.getPickupPointCodes())
        .promoTypes(productSummaryRequest.getPromoTypes())
        .sortField(productSummaryRequest.getSortField())
        .suspended(productSummaryRequest.getSuspended()).sortOrder(productSummaryRequest.getSortOrder())
        .b2bActivated(productSummaryRequest.getB2bActivated()).b2cActivated(productSummaryRequest.getB2cActivated())
        .bundleProduct(productSummaryRequest.getBundleProduct()).sizeChartCode(productSummaryRequest.getSizeChartCode())
        .attributeCode(productSummaryRequest.getSizeAttributeCode())
        .fetchSizeChartDetails(productSummaryRequest.getFetchSizeChartDetails())
        .pureExternalUser(productSummaryRequest.getPureExternalUser())
        .build();
  }

  public static boolean isEqualList(List<String> list1, List<String> list2) {
    if (CollectionUtils.isEmpty(list1) && CollectionUtils.isEmpty(list2))
      return true;
    if ((CollectionUtils.isEmpty(list1) && CollectionUtils.isNotEmpty(list2)) || (CollectionUtils.isNotEmpty(list1)
        && CollectionUtils.isEmpty(list2)) || (list1.size() != list2.size())) {
      return false;
    }
    Collections.sort(list1);
    Collections.sort(list2);
    return list1.equals(list2);
  }

  public static List<MasterDataProductImageDTO> toMasterDataProductImageDTO(String productCode, List<Image> images) {
    List<MasterDataProductImageDTO> masterDataProductImageDTOS = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(images)) {
      for(Image image : images) {
        if (filterProcessedImages(image)) {
          MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
          masterDataProductImageDTO.setMainImage(image.isMainImages());
          masterDataProductImageDTO.setLocationPath(image.getLocationPath());
          masterDataProductImageDTO.setSequence(image.getSequence());
          masterDataProductImageDTO.setCommonImage(image.isCommonImage());
          masterDataProductImageDTO.setProductCode(productCode);
          masterDataProductImageDTOS.add(masterDataProductImageDTO);
        }
      }
    }
    return masterDataProductImageDTOS;
  }

  // If there is any change in this logic , need to update the same in PCB
  public static boolean filterProcessedImages(Image image) {
    if (image.isEdited()) {
      return image.isActive();
    }
    if (Objects.isNull(image.getOriginalImage())) {
      return true;
    } else {
      return !image.getOriginalImage();
    }
  }

  public static List<MasterDataProductImageDTO> convertItemImagesToMasterDataProductImageDTO(List<MasterDataItemImage> images) {
    List<MasterDataProductImageDTO> masterDataProductImageDTOS = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(images)) {
      for(MasterDataItemImage image : images) {
          MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
          masterDataProductImageDTO.setMainImage(image.isMainImage());
          masterDataProductImageDTO.setLocationPath(image.getLocationPath());
          masterDataProductImageDTO.setSequence(image.getSequence());
          masterDataProductImageDTOS.add(masterDataProductImageDTO);
      }
    }
    return masterDataProductImageDTOS;
  }

  public static List<ItemScoreRequest> setItemRequestsForNullProductCodeL4(List<Item> items) {
    List<ItemScoreRequest> itemScoreRequestList = new ArrayList<>();
    for (Item item : items) {
      List<MasterDataProductImageDTO> masterDataProductImageDTOList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(item.getMasterDataItem().getMasterDataItemImages())) {
        masterDataProductImageDTOList =
            item.getMasterDataItem().getMasterDataItemImages().stream().map(CommonUtil::toMasterDataProductImageDTO)
                .collect(Collectors.toList());
      }
      ItemScoreRequest itemScoreRequest = ItemScoreRequest.builder().itemImages(masterDataProductImageDTOList).build();
      itemScoreRequestList.add(itemScoreRequest);
    }
    return itemScoreRequestList;
  }

  private static MasterDataProductImageDTO toMasterDataProductImageDTO(MasterDataItemImage masterDataItemImage) {
    MasterDataProductImageDTO masterDataProductImageDTO = new MasterDataProductImageDTO();
    BeanUtils.copyProperties(masterDataItemImage, masterDataProductImageDTO);
    return masterDataProductImageDTO;
  }

  public static ProductCenterHistoryResponse toProductCenterHistoryResponse(ProductCenterHistory productCenterHistory) {
    ProductCenterHistoryResponse productCenterHistoryResponse = new ProductCenterHistoryResponse();
    BeanUtils.copyProperties(productCenterHistory, productCenterHistoryResponse);
    return productCenterHistoryResponse;
  }

  public static Pair<List<String>, List<ProductSkuSummaryResponse>> toProductSkuSummaryResponseList(
    List<ProductSolr> productSolrList, String solrStringDelimiter) {
    List<ProductSkuSummaryResponse> productSkuSummaryResponseList = new ArrayList<>();
    List<String> reindexEligibleProducts = new ArrayList<>();
    for (ProductSolr productSolr : productSolrList) {
      boolean isReindexNeeded =
        Stream.of(Optional.ofNullable(productSolr).map(ProductSolr::getProductName).orElse(null),
            Optional.ofNullable(productSolr).map(ProductSolr::getMasterCatalog)
              .filter(StringUtils::isNotBlank).orElse(null),
            Optional.ofNullable(productSolr).map(ProductSolr::getBrand).orElse(null))
          .anyMatch(Objects::isNull);
      if (isReindexNeeded) {
        reindexEligibleProducts.add(productSolr.getProductSku());
      } else {
        ProductSkuSummaryResponse productSkuSummaryResponse =
          ProductSkuSummaryResponse.builder().productName(productSolr.getProductName()).productSku(productSolr.getProductSku()).brand(productSolr.getBrand())
            .categoryCode(productSolr.getMasterCatalog().split(solrStringDelimiter)[1]).productImage(productSolr.getProductMainImage())
            .isArchived(Boolean.TRUE.equals(productSolr.getIsArchived())).productCode(productSolr.getProductCode()).build();
        productSkuSummaryResponseList.add(productSkuSummaryResponse);
      }
    }
    return Pair.of(reindexEligibleProducts,productSkuSummaryResponseList);
  }

  public static Map<String, Map<String, Object>> getFieldsAndValuesForPromoUpdates(List<Item> items, boolean isPromoUpdateOnly) {
    Map<String, List<Item>> productSkuItemMap = items.stream().collect(Collectors.groupingBy(Item::getProductSku, Collectors.toList()));
    Map<String, Map<String, Object>> productAndPromoDetails = new HashMap<>();
    for (String productSku : productSkuItemMap.keySet()) {
      productAndPromoDetails.put(productSku, getFieldsAndValuesForPromoUpdate(productSkuItemMap.get(productSku), isPromoUpdateOnly));
    }
    return productAndPromoDetails;
  }

  public static List<SolrInputDocument> getSolrInputDocsForPromoUpdates(List<Item> items, boolean isPromoUpdateOnly) {
    Map<String, List<Item>> productSkuItemMap =
        items.stream().collect(Collectors.groupingBy(Item::getProductSku, Collectors.toList()));
    return productSkuItemMap.keySet().stream().map(
        productSku -> getSolrInputDocumentForPromoUpdate(productSkuItemMap.get(productSku), productSku,
            isPromoUpdateOnly)).collect(Collectors.toList());
  }

  private static Map<String, Object> getFieldsAndValuesForPromoUpdate(List<Item> items, boolean isPromoUpdateOnly) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    List<String> addedPromoSkus = new ArrayList<>();
    List<String> removedPromoSkus = new ArrayList<>();
    List<String> addedWholesaleSkus = new ArrayList<>();
    List<String> removedWholesaleSkus = new ArrayList<>();
    for (Item item : items) {
      addOrRemovePromoItemSku(item, addedPromoSkus, removedPromoSkus);
      if (!isPromoUpdateOnly) {
        addOrRemoveWholeSaleItemSku(item, addedWholesaleSkus, removedWholesaleSkus);
      }
    }
    setPromoItemSkus(fieldsAndValues, addedPromoSkus, removedPromoSkus);
    if (!isPromoUpdateOnly) {
      setWholesaleItemSkus(fieldsAndValues, addedWholesaleSkus, removedWholesaleSkus);
    }
    return fieldsAndValues;
  }

  private static SolrInputDocument getSolrInputDocumentForPromoUpdate(List<Item> items, String productSku, boolean isPromoUpdateOnly) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    List<String> addedPromoSkus = new ArrayList<>();
    List<String> removedPromoSkus = new ArrayList<>();
    List<String> addedWholesaleSkus = new ArrayList<>();
    List<String> removedWholesaleSkus = new ArrayList<>();
    for (Item item : items) {
      addOrRemovePromoItemSku(item, addedPromoSkus, removedPromoSkus);
      if (!isPromoUpdateOnly) {
        addOrRemoveWholeSaleItemSku(item, addedWholesaleSkus, removedWholesaleSkus);
      }
    }
    setPromoItemSkus(solrInputDocument, addedPromoSkus, removedPromoSkus);
    if (!isPromoUpdateOnly) {
      setWholesaleItemSkus(solrInputDocument, addedWholesaleSkus, removedWholesaleSkus);
    }
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesL3ForPromoUpdate(Map<String, List<ItemPickupPoint>> itemPickupPoints,
      List<String> addedItemSkus, boolean populateLabelForUpcomingPromo, boolean populateLabelForPwpPromo) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    if (MapUtils.isEmpty(itemPickupPoints)) {
      setPromoItemSkus(fieldsAndValues, addedItemSkus, new ArrayList<>());
    } else {
      List<String> removedItemSkus = new ArrayList<>();
      for (String itemSku : itemPickupPoints.keySet()) {
        getAddedAndRemovedItemskusByItemPickupPoint(itemPickupPoints.get(itemSku), addedItemSkus, removedItemSkus,
            populateLabelForUpcomingPromo, populateLabelForPwpPromo);
      }
      setPromoItemSkus(fieldsAndValues, addedItemSkus, removedItemSkus);
    }
    return fieldsAndValues;
  }

  public static SolrInputDocument getSolrInputDocumentL3ForPromoUpdate(
      Map<String, List<ItemPickupPoint>> itemPickupPoints, List<String> addedItemSkus, String productSku,
      boolean populateLabelForUpcomingPromo, boolean populateLabelForPwpPromo) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    if (MapUtils.isEmpty(itemPickupPoints)) {
      setPromoItemSkus(solrInputDocument, addedItemSkus, new ArrayList<>());
    } else {
      List<String> removedItemSkus = new ArrayList<>();
      for (String itemSku : itemPickupPoints.keySet()) {
        getAddedAndRemovedItemskusByItemPickupPoint(itemPickupPoints.get(itemSku), addedItemSkus, removedItemSkus,
            populateLabelForUpcomingPromo, populateLabelForPwpPromo);
      }
      setPromoItemSkus(solrInputDocument, addedItemSkus, removedItemSkus);
    }
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static SolrInputDocument getSolrInputDocumentL3ForPromoAndWholesaleUpdate(
      List<ItemPickupPoint> itemPickupPoints, String productSku, boolean populateLabelForUpcomingPromo,
      boolean populateLabelForPwpPromo) {
    SolrInputDocument solrInputDocument = null;
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      List<String> addedItemSkus = new ArrayList<>();
      List<String> removedItemSkus = new ArrayList<>();
      List<String> addedWholesaleItemSkus = new ArrayList<>();
      List<String> removedWholesaleItemSkus = new ArrayList<>();
      solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
      getAddedAndRemovedItemskusByItemPickupPoint(itemPickupPoints, addedItemSkus, removedItemSkus,
          populateLabelForUpcomingPromo, populateLabelForPwpPromo);
      setPromoItemSkus(solrInputDocument, addedItemSkus, removedItemSkus);
      getAddedAndRemovedWholesaleItemskusByItemPickupPoint(itemPickupPoints, addedWholesaleItemSkus,
          removedWholesaleItemSkus, itemPickupPoints.get(0).getItemSku());
      setWholesaleItemSkus(solrInputDocument, addedWholesaleItemSkus, removedWholesaleItemSkus);
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    }
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesL3ForPromoAndWholesaleUpdate(List<ItemPickupPoint> itemPickupPoints,
      boolean populateLabelForUpcomingPromo, boolean populateLabelForPwpPromo) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      List<String> addedItemSkus = new ArrayList<>();
      List<String> removedItemSkus = new ArrayList<>();
      List<String> addedWholesaleItemSkus = new ArrayList<>();
      List<String> removedWholesaleItemSkus = new ArrayList<>();
      getAddedAndRemovedItemskusByItemPickupPoint(itemPickupPoints, addedItemSkus, removedItemSkus,
          populateLabelForUpcomingPromo, populateLabelForPwpPromo);
      setPromoItemSkus(fieldsAndValues, addedItemSkus, removedItemSkus);
      getAddedAndRemovedWholesaleItemskusByItemPickupPoint(itemPickupPoints, addedWholesaleItemSkus,
          removedWholesaleItemSkus, itemPickupPoints.get(0).getItemSku());
      setWholesaleItemSkus(fieldsAndValues, addedWholesaleItemSkus, removedWholesaleItemSkus);
    }
    return fieldsAndValues;
  }

  private static void getAddedAndRemovedItemskusByItemPickupPoint(List<ItemPickupPoint> itemPickupPoints, List<String> addedItemSkus,
      List<String> removedItemSkus, boolean populateLabelForUpcomingPromo, boolean populateLabelForPwpPromo) {
    addedItemSkus.addAll(itemPickupPoints.stream().filter(
        itemPickupPoint1 -> isItemAddedToPromo(itemPickupPoint1, populateLabelForUpcomingPromo,
            populateLabelForPwpPromo)).map(ItemPickupPoint::getItemSku).distinct().collect(Collectors.toList()));
    removedItemSkus.addAll(itemPickupPoints.stream().filter(
        itemPickupPoint -> !isItemAddedToPromo(itemPickupPoint, populateLabelForUpcomingPromo,
            populateLabelForPwpPromo)).map(ItemPickupPoint::getItemSku).distinct().collect(Collectors.toList()));
  }

  private static void getAddedAndRemovedWholesaleItemskusByItemPickupPoint(List<ItemPickupPoint> itemPickupPoints,
      List<String> addedItemSkus, List<String> removedItemSkus, String itemSku) {
    if (itemPickupPoints.stream().anyMatch(itemPickupPoint -> isItemAddedToWholesalePrice(itemPickupPoint))) {
      addedItemSkus.add(itemSku);
    } else {
      removedItemSkus.add(itemSku);
    }
  }

  private static void setPromoItemSkus(SolrInputDocument solrInputDocument, List<String> addedPromoSkus,
      List<String> removedPromoSkus) {
    Map<String, List<String>> promoMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(addedPromoSkus)) {
      promoMap.put(SolrConstants.ADD_CLAUSE, addedPromoSkus);
    }
    if (CollectionUtils.isNotEmpty(removedPromoSkus)) {
      promoMap.put(SolrConstants.REMOVE_CLAUSE, removedPromoSkus);
    }
    solrInputDocument.setField(SolrFieldNames.PROMO_ITEM_SKUS, promoMap);
  }

  private static void setPromoItemSkus(Map<String, Object> fieldsAndValues, List<String> addedPromoSkus,
      List<String> removedPromoSkus) {
    Map<String, List<String>> promoMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(addedPromoSkus)) {
      promoMap.put(SolrConstants.ADD_CLAUSE, addedPromoSkus);
    }
    if (CollectionUtils.isNotEmpty(removedPromoSkus)) {
      promoMap.put(SolrConstants.REMOVE_CLAUSE, removedPromoSkus);
    }
    fieldsAndValues.put(SolrFieldNames.PROMO_ITEM_SKUS, promoMap);
  }

  private static void setWholesaleItemSkus(SolrInputDocument solrInputDocument, List<String> addedWholesaleSkus,
      List<String> removedWholesaleSkus) {
    Map<String, List<String>> wholesaleMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(addedWholesaleSkus)) {
      wholesaleMap.put(SolrConstants.ADD_CLAUSE, addedWholesaleSkus);
    }
    if (CollectionUtils.isNotEmpty(removedWholesaleSkus)) {
      wholesaleMap.put(SolrConstants.REMOVE_CLAUSE, removedWholesaleSkus);
    }
    solrInputDocument.setField(SolrFieldNames.WHOLESALE_ITEM_SKUS, wholesaleMap);
  }

  private static void setWholesaleItemSkus(Map<String, Object> fieldsAndValues, List<String> addedWholesaleSkus,
      List<String> removedWholesaleSkus) {
    Map<String, List<String>> wholesaleMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(addedWholesaleSkus)) {
      wholesaleMap.put(SolrConstants.ADD_CLAUSE, addedWholesaleSkus);
    }
    if (CollectionUtils.isNotEmpty(removedWholesaleSkus)) {
      wholesaleMap.put(SolrConstants.REMOVE_CLAUSE, removedWholesaleSkus);
    }
    fieldsAndValues.put(SolrFieldNames.WHOLESALE_ITEM_SKUS, wholesaleMap);
  }

  private static void addOrRemoveWholeSaleItemSku(Item item, List<String> addedSkus, List<String> removedSkus) {
    if (isItemAddedToWholesalePrice(item)) {
      addedSkus.add(item.getItemSku());
    } else {
      removedSkus.add(item.getItemSku());
    }
  }

  private static void addOrRemovePromoItemSku(Item item, List<String> addedSkus, List<String> removedSkus) {
    if (isItemAddedToPromo(item)) {
      addedSkus.add(item.getItemSku());
    } else {
      removedSkus.add(item.getItemSku());
    }
  }

  public static boolean isItemAddedToWholesalePrice(Item item) {
    return (item.isWholesalePriceExists() && CollectionUtils.isNotEmpty(item.getActivePromoBundlings()) && item
        .getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
  }

  public static boolean isItemAddedToWholesalePrice(ItemPickupPoint itemPickupPoint) {
    return (itemPickupPoint.isWholesalePriceExists() && CollectionUtils.isNotEmpty(
        itemPickupPoint.getActivePromoBundlings()) && itemPickupPoint.getActivePromoBundlings()
        .contains(Constants.WHOLESALE_PRICE));
  }

  public static List<SolrInputDocument> getSolrInputDocumentsForListingUpdate(List<Item> items, ProductType productType,
      String solrStringDelimiter) {
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    for (Item item : items) {
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
      solrInputDocument.setField(SolrFieldNames.WHOLESALE_PRICE_ACTIVATED,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getWholesalePriceActivated()));
      solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.isArchived()));
      solrInputDocument.setField(SolrFieldNames.MERCHANT_SKU,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getMerchantSku()));
      solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getPickupPointCode()));
      solrInputDocument.setField(SolrFieldNames.PRODUCT_TYPE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productType.toString()));
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
      getItemViewConfig(item, solrStringDelimiter, solrInputDocument);
      setFieldForPriceChanges(solrInputDocument, item, solrStringDelimiter);
      solrInputDocuments.add(solrInputDocument);
    }
    return solrInputDocuments;
  }

  public static SolrInputDocument getL3SolrInputDocumentsForListingUpdate(String productSku, List<Item> items, boolean l3Archive) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, items.get(0).getUpdatedDate()));
    List<String> wholeSaleItemSkus =
        items.stream().filter(item -> isItemAddedToWholesalePrice(item)).map(Item::getItemSku).distinct()
            .collect(Collectors.toList());
    List<String> pickupPointCodes =
        items.stream().map(Item::getPickupPointCode).distinct().collect(Collectors.toList());
    solrInputDocument.setField(SolrFieldNames.WHOLESALE_ITEM_SKUS,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, wholeSaleItemSkus));
    solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
    solrInputDocument.setField(SolrFieldNames.IS_ARCHIVED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, l3Archive));
    setMinAndMaxPriceFields(items, solrInputDocument, null);
    return solrInputDocument;
  }

  public static Map<String, Object> getFieldsAndValuesForListingUpdate(List<Item> items, boolean l3Archive) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    List<String> wholeSaleItemSkus =
        items.stream().filter(item -> isItemAddedToWholesalePrice(item)).map(Item::getItemSku).distinct()
            .collect(Collectors.toList());
    List<String> pickupPointCodes =
        items.stream().map(Item::getPickupPointCode).distinct().collect(Collectors.toList());
    fieldsAndValues.put(SolrFieldNames.WHOLESALE_ITEM_SKUS,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, wholeSaleItemSkus));
    fieldsAndValues.put(SolrFieldNames.PICKUP_POINT_CODES,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
    fieldsAndValues.put(SolrFieldNames.IS_ARCHIVED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, l3Archive));
    setMinAndMaxPriceFields(items, null, fieldsAndValues);
    return fieldsAndValues;
  }

  public static Map<String, Object> getFieldsAndValuesForPriceAtomicUpdateL3(Item updatedItem, List<Item> items) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    if (isItemAddedToWholesalePrice(updatedItem)) {
      setWholesaleItemSkus(fieldsAndValues, Arrays.asList(updatedItem.getItemSku()), new ArrayList<>());
    } else {
      setWholesaleItemSkus(fieldsAndValues, new ArrayList<>(), Arrays.asList(updatedItem.getItemSku()));
    }
    items.add(updatedItem);
    setMinAndMaxPriceFields(items, null, fieldsAndValues);
    return fieldsAndValues;
  }

  public static SolrInputDocument getSolrInputDocumentForPriceAtomicUpdateL3(Item updatedItem, List<Item> items) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, updatedItem.getProductSku());
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, updatedItem.getUpdatedDate()));
    if (isItemAddedToWholesalePrice(updatedItem)) {
      setWholesaleItemSkus(solrInputDocument, Arrays.asList(updatedItem.getItemSku()), new ArrayList<>());
    } else {
      setWholesaleItemSkus(solrInputDocument, new ArrayList<>(), Arrays.asList(updatedItem.getItemSku()));
    }
    items.add(updatedItem);
    setMinAndMaxPriceFields(items, solrInputDocument, null);
    return solrInputDocument;
  }

  private static void setMinAndMaxPriceFields(List<Item> items, SolrInputDocument solrInputDocument, Map<String, Object> fieldsAndValues) {
    List<com.gdn.x.product.model.entity.Price> prices =
        items.stream().flatMap(item -> item.getPrice().stream()).collect(Collectors.toList());
    double maxOfferPrice = Double.MIN_VALUE;
    double minOfferPrice = Double.MAX_VALUE;
    double maxListPrice = Double.MIN_VALUE;
    double minListPrice = Double.MAX_VALUE;
    for (com.gdn.x.product.model.entity.Price price : prices) {
      if (price.getOfferPrice() > maxOfferPrice) {
        maxOfferPrice = price.getOfferPrice();
      }
      if (price.getOfferPrice() < minOfferPrice) {
        minOfferPrice = price.getOfferPrice();
      }
      if (price.getListPrice() > maxListPrice) {
        maxListPrice = price.getListPrice();
      }
      if (price.getListPrice() < minListPrice) {
        minListPrice = price.getListPrice();
      }
    }
    if (Objects.nonNull(solrInputDocument)) {
      solrInputDocument.setField(SolrFieldNames.MAXIMUM_SELLING_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, maxOfferPrice));
      solrInputDocument.setField(SolrFieldNames.MINIMUM_SELLING_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, minOfferPrice));
      solrInputDocument
          .setField(SolrFieldNames.MAXIMUM_LIST_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, maxListPrice));
      solrInputDocument
          .setField(SolrFieldNames.MINIMUM_LIST_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, minListPrice));
    }
    if (Objects.nonNull(fieldsAndValues)) {
      fieldsAndValues.put(SolrFieldNames.MAXIMUM_SELLING_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, maxOfferPrice));
      fieldsAndValues.put(SolrFieldNames.MINIMUM_SELLING_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, minOfferPrice));
      fieldsAndValues.put(SolrFieldNames.MAXIMUM_LIST_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, maxListPrice));
      fieldsAndValues.put(SolrFieldNames.MINIMUM_LIST_PRICE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, minListPrice));
    }
  }

  private static void setFieldForPriceChanges(SolrInputDocument solrInputDocument, Item item,
      String solrStringDelimiter) {
    List<String> offerPrice = new ArrayList<>();
    List<String> listPrice = new ArrayList<>();
    boolean merchnatPromoDiscountActivated = false;
    for (com.gdn.x.product.model.entity.Price price : item.getPrice()) {
      if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
        offerPrice.add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter,
            price.getMerchantPromoDiscountPrice().getDiscountPrice()));
        merchnatPromoDiscountActivated = true;
      } else {
        offerPrice
            .add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter, price.getOfferPrice()));
        merchnatPromoDiscountActivated = false;
      }
      listPrice
          .add(String.format(STRING_FORMATTER_PRICE, price.getChannel(), solrStringDelimiter, price.getListPrice()));
    }
    solrInputDocument.setField(SolrFieldNames.MERCHANT_PROMO_DISCOUNT_ACTIVATED,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, merchnatPromoDiscountActivated));
    solrInputDocument
        .setField(SolrFieldNames.LIST_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, listPrice));
    solrInputDocument
        .setField(SolrFieldNames.OFFER_PRICE, Collections.singletonMap(SolrConstants.SET_CLAUSE, offerPrice));
  }

  public static SolrInputDocument getpickupPointCodeAtomicUpdateInputDocument(Item item) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getPickupPointCode()));
    return solrInputDocument;
  }

  public static SolrInputDocument getpickupPointCodeAtomicUpdateInputDocumentL3(List<Item> items) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, items.get(0).getProductSku());
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        items.stream().map(Item::getPickupPointCode).distinct().collect(Collectors.toList())));
    return solrInputDocument;
  }

  public static void setItemCatalogs(Product product, Map<String, List<ItemCatalogVO>> itemCategoryVOListMap,
      List<String> categoryCodeList, boolean excludeInactiveSalesCategories) {
    List<ItemCatalogVO> itemCatalogVOList = new ArrayList<>();
    for (String categoryCode : Optional.ofNullable(categoryCodeList).orElse(new ArrayList<>())) {
      List<ItemCatalogVO> itemCatalogVOS =
          Optional.ofNullable(itemCategoryVOListMap.get(categoryCode)).orElse(new ArrayList<>());
      removeInactiveSalesCategories(product, itemCatalogVOS, excludeInactiveSalesCategories);
      itemCatalogVOList.addAll(itemCatalogVOS);
    }
    product.setItemCatalogs(itemCatalogVOList);
  }

  public static void removeInactiveSalesCategories(Product product, List<ItemCatalogVO> itemCatalogVOS,
      boolean excludeInactiveSalesCategories) {
    if (excludeInactiveSalesCategories) {
      Map<String, ItemCatalogVO> inactiveCategoryCodes = new HashMap<>();
      for (ItemCatalogVO itemCatalogVO : Optional.ofNullable(itemCatalogVOS).orElse(new ArrayList<>())) {
        String inactiveCategoryCode = getInactiveCategoryCode(itemCatalogVO);
        if (StringUtils.isNotEmpty(inactiveCategoryCode)) {
          inactiveCategoryCodes.put(inactiveCategoryCode, itemCatalogVO);
        }
      }

      Set<ItemCatalogVO> itemCatalogsToBeRemoved = new HashSet<>();

      for (SalesCatalog salesCatalog : Optional.ofNullable(product).map(Product::getSalesCatalogs)
          .orElse(new ArrayList<>())) {
        Set<Category> salesCategoryToBeRemoved = new HashSet<>();
        for (Category salesCategory : Optional.ofNullable(salesCatalog.getListOfCategories())
            .orElse(new ArrayList<>())) {
          if (inactiveCategoryCodes.containsKey(salesCategory.getCategoryCode())) {
            salesCategoryToBeRemoved.add(salesCategory);
            itemCatalogsToBeRemoved.add(inactiveCategoryCodes.get(salesCategory.getCategoryCode()));
          }
        }
        Optional.ofNullable(salesCatalog.getListOfCategories()).orElse(new ArrayList<>())
            .removeAll(salesCategoryToBeRemoved);
      }
      Optional.ofNullable(itemCatalogVOS).orElse(new ArrayList<>()).removeAll(itemCatalogsToBeRemoved);
    }
  }

  private static String getInactiveCategoryCode(ItemCatalogVO itemCatalogVO) {
    if (CollectionUtils.isNotEmpty(itemCatalogVO.getItemCategories())) {
      ItemCategoryVO firstItemCategoryVO = itemCatalogVO.getItemCategories().get(0);
      for (ItemCategoryVO itemCategoryVO : itemCatalogVO.getItemCategories()) {
        if (!itemCategoryVO.isActivated()) {
          return firstItemCategoryVO.getProductCategoryCode();
        }
      }
    }
    return StringUtils.EMPTY;
  }

  public static void fetchCategoryCodeList(Product product, Set<String> categoryCodeList,
      Map<String, List<String>> productToCategoryCodeList, String catalogType) {
    List<String> productCategoryList = new ArrayList<>();
    if (!Constants.SALES.equals(catalogType)) {
      if (Objects.isNull(product.getMasterCatalog())) {
        if (Objects.nonNull(product.getMasterDataProduct())) {
          product.setMasterCatalog(product.getMasterDataProduct().getMasterCatalog());
        } else {
          return;
        }
      }
      productCategoryList.add(product.getMasterCatalog().getCategory().getCategoryCode());
    }
    if (!Constants.MASTER.equals(catalogType)) {
      product.getSalesCatalogs().stream().map(SalesCatalog::getListOfCategories).flatMap(Collection::stream)
          .map(Category::getCategoryCode).distinct().collect(Collectors.toCollection(() -> productCategoryList));
    }
    categoryCodeList.addAll(productCategoryList);
    productToCategoryCodeList.put(product.getProductSku(), productCategoryList);
  }

  public static void fetchCategoryCodeListFromProduct(Product product, Set<String> categoryCodeList,
      Map<String, List<String>> productToCategoryCodeList) {
    List<String> productCategoryList = new ArrayList<>();
    productCategoryList.add(product.getCategoryCode());
    product.getSalesCatalogs().stream().map(SalesCatalog::getListOfCategories).flatMap(Collection::stream)
        .map(Category::getCategoryCode).distinct().collect(Collectors.toCollection(() -> productCategoryList));
    categoryCodeList.addAll(productCategoryList);
    productToCategoryCodeList.put(product.getProductSku(), productCategoryList);
  }

  public static List<String> getItemSkusNotFoundInFetch(List<Item> itemList,
      List<String> itemSkus) {
    List<String> itemSkuRemaining = new ArrayList<>();
    itemSkuRemaining.addAll(itemSkus);
    itemSkuRemaining
        .removeAll(itemList.stream().map(Item::getItemSku).collect(Collectors.toList()));
    return itemSkuRemaining;
  }

  public static Boolean getWholeSalePriceActivated(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.isWholesalePriceExists() ?
        Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
            .contains(Constants.WHOLESALE_PRICE) : null;
  }

  public static Boolean getWholeSalePriceActivated(Item item) {
    return item.isWholesalePriceExists() ?
        Optional.ofNullable(item.getActivePromoBundlings()).orElse(new HashSet<>())
            .contains(Constants.WHOLESALE_PRICE) :
        null;
  }

  public static SolrInputDocument getOfflinePriceAndCncUpdateSolrInputDocument(List<String> existingOfflinePrices,
      List<String> offlinePrices, Item item) {
    Map<String, List<String>> offlinePricesMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(existingOfflinePrices)) {
      offlinePricesMap.put(SolrConstants.REMOVE_CLAUSE, existingOfflinePrices);
    }
    if (CollectionUtils.isNotEmpty(offlinePrices)) {
      offlinePricesMap.put(SolrConstants.ADD_CLAUSE, offlinePrices);
    }
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
    solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE, item.isCncActivated());
    solrInputDocument.setField(SolrFieldNames.OFFLINE_PRICES, offlinePricesMap);
    solrInputDocument
        .setField(SolrFieldNames.UPDATED_DATE, Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    return solrInputDocument;
  }

  public static void setStatusOnFailure(ProductRetryEventPublish productRetryEventPublish,
      List<ProductRetryEventPublish> failedList, int maxRetryPublish) {
    if (productRetryEventPublish.getRetryCount() < maxRetryPublish) {
      productRetryEventPublish.setRetryCount(productRetryEventPublish.getRetryCount() + 1);
      productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.PENDING);
    } else {
      productRetryEventPublish.setRetryPublishStatus(RetryPublishStatus.FAILED);
      failedList.add(productRetryEventPublish);
    }
  }

  public static MessageEmailRequest setMailMessage(List<ProductRetryEventPublish> productRetryEventPublishes,
      String sendFailedRetryProductSummaryEmailAddress) {
    List<FailedRetryProductsResponse> failedRetryProductsResponseList = productRetryEventPublishes.stream().map(
        retryEventPublish -> new FailedRetryProductsResponse(retryEventPublish.getIdentifier(),
            retryEventPublish.getCreatedDate().toString().split(DOT)[0],
            retryEventPublish.getUpdatedDate().toString().split(DOT)[0])).collect(Collectors.toList());
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<>();
    mailObjectWrapper.put(OBJECT, failedRetryProductsResponseList);
    mailObjectWrapper.put(TOTAL, productRetryEventPublishes.size());
    emailObject.put(EMAIL_OBJECT, mailObjectWrapper);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(FAILED_RETRY_PUBLISH_ALERT);
    email.setMessageTo(sendFailedRetryProductSummaryEmailAddress);
    email.setMessageIdentifierKey(FAILED_RETRY_PUBLISH_ALERT);
    email.setMessageIdentifierValue(UUID.randomUUID().toString());
    email.setMessageSubject(FAILED_RETRY_PUBLISH_EMAIL_SUBJECT);
    email.setVariables(emailObject);
    return email;
  }

  public static boolean checkIfUsernameIsExcluded(String username, String excludedUserNames) {
    if (StringUtils.isNotBlank(excludedUserNames) && StringUtils.isNotBlank(username)) {
      if (Constants.ALL.equalsIgnoreCase(excludedUserNames)) {
        return true;
      } else if (Constants.NONE.equalsIgnoreCase(excludedUserNames)) {
        return false;
      } else {
        List<String> usernames =
            new ArrayList<>(Arrays.asList(excludedUserNames.toLowerCase().trim().split(Constants.COMMA_DELIMITER)));
        if (usernames.contains(username.toLowerCase())) {
          return true;
        }
      }
    }
    return false;
  }

  public static List<ProductL3SolrReindexStatus> toProductL3SolrReindexStatuses(Set<String> productSkus, String storeId,
      ProductReindexStatus productReindexStatus) {
    List<ProductL3SolrReindexStatus> productL3SolrReindexStatuses = new ArrayList<>();
    for (String productSku : productSkus) {
      ProductL3SolrReindexStatus productL3SolrReindexStatus = new ProductL3SolrReindexStatus();
      productL3SolrReindexStatus.setStoreId(storeId);
      productL3SolrReindexStatus.setProductReindexStatus(productReindexStatus);
      productL3SolrReindexStatus.setProductSku(productSku);
      productL3SolrReindexStatus.setCreatedBy(Constants.DEFAULT_UPDATED_BY);
      productL3SolrReindexStatus.setUpdatedBy(Constants.DEFAULT_UPDATED_BY);
      productL3SolrReindexStatus.setCreatedDate(new Date());
      productL3SolrReindexStatus.setUpdatedDate(new Date());
      productL3SolrReindexStatuses.add(productL3SolrReindexStatus);
    }
    return productL3SolrReindexStatuses;
  }

  public static ProductDetailVo toProductDetailVo(ProductSolr productSolr, Set<String> itemSkus, String solrStringDelimiter) {
    ProductDetailVo productDetailVo = new ProductDetailVo();
    productDetailVo.setProductSku(productSolr.getProductSku());
    productDetailVo.setProductCode(productSolr.getProductCode());
    productDetailVo.setProductName(productSolr.getProductName());
    productDetailVo.setMaxPrice(productSolr.getMaximumSellingPrice());
    productDetailVo.setMinPrice(productSolr.getMinimumSellingPrice());
    productDetailVo.setImageUrl(productSolr.getProductMainImage());
    productDetailVo.setMerchantCode(productSolr.getMerchantCode());
    productDetailVo.setBrand(productSolr.getBrand());
    productDetailVo.setCategoryCode(productSolr.getMasterCatalog().split(solrStringDelimiter)[1]);
    productDetailVo.setItemSkus(itemSkus);
    return productDetailVo;
  }

  public static void setActiveProductDetailVo(List<ActiveProductDetailVo> activeProductDetailVos,
      List<ProductSolr> productSolrs, Map<String, List<ItemNameSkuVO>> itemSkuMap,
      Map<String, List<ItemPickupPoint>> itemPickupPointMap, Map<String, Item> itemMap, String solrStringDelimiter) {
    for (ProductSolr productSolr : productSolrs) {
      ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
      activeProductDetailVo.setProductName(productSolr.getProductName());
      activeProductDetailVo.setProductSku(productSolr.getProductSku());
      activeProductDetailVo.setProductCode(productSolr.getProductCode());
      activeProductDetailVo.setBundleProduct(productSolr.isBundleProduct());
      activeProductDetailVo.setImageUrl(productSolr.getProductMainImage());
      activeProductDetailVo.setMerchantCode(productSolr.getMerchantCode());
      activeProductDetailVo.setItemCount(Objects.isNull(itemPickupPointMap.get(productSolr.getProductSku())) ?
          0 :
          itemPickupPointMap.get(productSolr.getProductSku()).stream().map(ItemPickupPoint::getItemSku)
              .collect(Collectors.toSet()).size());
      activeProductDetailVo.setItemDetailVOList(CollectionUtils.isEmpty(itemSkuMap.get(productSolr.getProductSku())) ?
          new ArrayList<>() :
          itemSkuMap.get(productSolr.getProductSku()));
      activeProductDetailVo.setMasterCatalog(
          StringUtils.isNotEmpty(productSolr.getMasterCatalog()) ?
              productSolr.getMasterCatalog().split(solrStringDelimiter)[1]: StringUtils.EMPTY);
      activeProductDetailVo.setL5Count(productSolr.getL5Count());
      setItemAndItemPickupPointDetailIfItsSingleVariant(activeProductDetailVo, productSolr, itemPickupPointMap, itemMap);
      activeProductDetailVos.add(activeProductDetailVo);
    }
  }

  private static void setItemAndItemPickupPointDetailIfItsSingleVariant(ActiveProductDetailVo activeProductDetailVo,
      ProductSolr productSolr, Map<String, List<ItemPickupPoint>> itemPickupPointMap, Map<String, Item> itemMap) {
    if (itemMap.containsKey(productSolr.getProductSku())) {
      Item item = itemMap.get(productSolr.getProductSku());
      ItemAndPickupPointBasicDetailVo setItemAndPickupPointBasicDetailVo =
          ItemAndPickupPointBasicDetailVo.builder().itemSku(item.getItemSku()).itemCode(item.getItemCode())
              .itemName(item.getGeneratedItemName()).mainImageUrl(item.getMainImageUrl()).build();
      if (productSolr.getL5Count() == Constants.SINGLE_VARIANT_L4_COUNT && itemPickupPointMap.containsKey(
          productSolr.getProductSku())) {
        setItemAndPickupPointBasicDetailVo.setPickupPointCode(itemPickupPointMap.get(productSolr.getProductSku()).stream().findFirst()
            .map(ItemPickupPoint::getPickupPointCode).orElse(null));
      }
      activeProductDetailVo.setItemAndPickupPointBasicDetailVo(setItemAndPickupPointBasicDetailVo);
    }
  }

  public static List<SolrInputDocument> getSolrInputDocumentsForProductTypeUpdate(List<Item> items,
      ProductType productType) {
    List<SolrInputDocument> solrInputDocuments = new ArrayList<>();
    for (Item item : items) {
      SolrInputDocument solrInputDocument = new SolrInputDocument();
      solrInputDocument.setField(SolrFieldNames.ID, item.getItemSku());
      solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getUpdatedDate()));
      solrInputDocument.setField(SolrFieldNames.PRODUCT_TYPE,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, productType.toString()));
      if (Objects.isNull(item.getPristineDataItem())) {
        solrInputDocument
            .setField(SolrFieldNames.PRISTINE_ID, Collections.singletonMap(SolrConstants.SET_CLAUSE, null));
      } else {
        solrInputDocument.setField(SolrFieldNames.PRISTINE_ID,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, item.getPristineDataItem().getPristineId()));
      }
      solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, item.getMerchantCode());
      solrInputDocuments.add(solrInputDocument);
    }
    return solrInputDocuments;
  }

  public static void setItemPickupPointDetailsInItem(Item item, ItemPickupPoint itemPickupPoint,  boolean validateViewConfig) {
    item.setPrice(itemPickupPoint.getPrice());
    if (validateViewConfig) {
      item.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs());
    } else {
      item.setItemViewConfigs(itemPickupPoint.getItemViewConfig());
    }
    item.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    item.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    item.setPromoBundling(itemPickupPoint.isPromoBundling());
    item.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    item.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    item.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
  }

  public static void setItemPickupPointDetailsInItemWithCncFlagUpdate(Item item, ItemPickupPoint itemPickupPoint, boolean validateViewConfig) {
    item.setOfflineItems(null);
    setItemPickupPointDetailsInItem(item, itemPickupPoint, validateViewConfig);
  }

  public static List<OfflineItem> getOfflineItemsByItemPickupPoint(List<ItemPickupPoint> itemPickupPointList,
      boolean newData, OfflineItemHistoryDetailVO offlineItemHistoryDetailVO) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream().map(
      itemPickupPoint -> getOfflineItemByPickupPoint(itemPickupPoint, newData,
        offlineItemHistoryDetailVO)).collect(Collectors.toList());
  }

  public static OfflineItem getOfflineItemByPickupPoint(ItemPickupPoint itemPickupPoint,
    boolean newData, OfflineItemHistoryDetailVO offlineItemHistoryDetailVO) {
    OfflineItem offlineItem = new OfflineItem();
    BeanUtils.copyProperties(itemPickupPoint, offlineItem);
    Iterator<com.gdn.x.product.model.entity.Price> priceIterator = itemPickupPoint.getPrice().iterator();
    com.gdn.x.product.model.entity.Price price = priceIterator.next();
    offlineItem.setOfferPrice(price.getOfferPrice());
    offlineItem.setListPrice(Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice()));
    offlineItem.setOfflineItemHistoryDetail(offlineItemHistoryDetailVO);
    offlineItem.setNewData(newData);
    offlineItem.setMarkForDelete(!itemPickupPoint.isCncActive());
    return offlineItem;
  }

  public static ItemSummaryResponseVO constructItemSummaryResponse(
      Map<String, Set<com.gdn.x.product.model.entity.Price>> mapOfPrices, Map<String, Double> mapOfOriginalPrice,
      Map<String, PreOrder> preOrderDetails, Map<String, Item> itemMap, ProductAndItemSolr productAndItem,
      ItemPickupPoint itemPickupPoint) {
    ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
    Item item = itemMap.get(productAndItem.getItemSku());
    Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigs = itemPickupPoint.getItemViewConfig();
    Set<com.gdn.x.product.model.entity.Price> prices = mapOfPrices.get(productAndItem.getItemSku());
    Double originalPrice = mapOfOriginalPrice.get(productAndItem.getItemSku());
    com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig =
        Optional.ofNullable(itemViewConfigs).orElse(new HashSet<>()).stream().findFirst()
            .orElse(new com.gdn.x.product.model.entity.ItemViewConfig());

    itemSummaryResponse.setStoreId(productAndItem.getStoreId());
    itemSummaryResponse.setVersion(itemPickupPoint.getVersion());
    itemSummaryResponse.setGeneratedItemName(productAndItem.getItemName());
    itemSummaryResponse.setItemCode(productAndItem.getItemCode());
    itemSummaryResponse.setItemSku(productAndItem.getItemSku());
    itemSummaryResponse.setItemViewConfigs(itemViewConfigs);
    itemSummaryResponse.setIsLateFulfillment(productAndItem.isLatefulfillment());
    itemSummaryResponse.setMerchantCode(productAndItem.getMerchantCode());
    itemSummaryResponse.setTicketTemplateCode(productAndItem.getTicketTemplateCode());
    itemSummaryResponse.setPrice(prices);
    itemSummaryResponse.setProductSku(productAndItem.getProductSku());
    itemSummaryResponse.setOff2OnChannelActive(item.isOff2OnChannelActive());
    itemSummaryResponse.setCreatedDate(productAndItem.getCreatedDate());
    itemSummaryResponse.setUpdatedDate(productAndItem.getUpdatedDate());
    itemSummaryResponse.setArchived(productAndItem.isArchived());
    itemSummaryResponse.setMarkForDelete(productAndItem.isMarkForDelete());
    itemSummaryResponse.setBrand(productAndItem.getBrand());
    itemSummaryResponse.setProductCode(productAndItem.getProductCode());
    itemSummaryResponse.setWholesalePriceActivated(productAndItem.getWholesalePriceActivated());
    itemSummaryResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemSummaryResponse.setCncActivated(productAndItem.isCncActivated());
    itemSummaryResponse.setMerchantPromoDiscountActivated(productAndItem.isMerchantPromoDiscountActivated());
    itemSummaryResponse.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    itemSummaryResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemSummaryResponse.setCncActive(itemPickupPoint.isCncActive());
    itemSummaryResponse.setFbbActivated(itemPickupPoint.isFbbActivated());
    itemSummaryResponse.setProductName(productAndItem.getProductName());
    itemSummaryResponse.setForceReview(item.isForceReview());
    itemSummaryResponse.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    itemSummaryResponse.setSuspended(productAndItem.isSuspended());
    itemSummaryResponse.setDiscoverable(itemViewConfig.isDiscoverable());
    itemSummaryResponse.setBuyable(itemViewConfig.isBuyable());
    itemSummaryResponse.setMerchantSku(itemPickupPoint.getMerchantSku());
    itemSummaryResponse.setFreeSample(productAndItem.isFreeSample());
    if (Objects.nonNull(productAndItem.getProductScoreTotal())) {
      itemSummaryResponse.setProductScore(productAndItem.getProductScoreTotal());
    }
    if (Objects.nonNull(productAndItem.getPristineId())) {
      itemSummaryResponse.setPristineId(productAndItem.getPristineId());
    }
    if (MapUtils.isNotEmpty(preOrderDetails)) {
      PreOrder preOrder = preOrderDetails.get(itemSummaryResponse.getProductSku());
      if (Objects.nonNull(preOrder)) {
        itemSummaryResponse.setPreOrder(Optional.ofNullable(preOrder.getIsPreOrder()).orElse(false));
        itemSummaryResponse.setPreOrderDate(preOrder.getPreOrderDate());
      }
    }
    if (Objects.nonNull(originalPrice)) {
      itemSummaryResponse.setOriginalSellingPrice(originalPrice);
    }
    try {
      itemSummaryResponse.setProductType(ProductType.valueOf(productAndItem.getProductType()));
    } catch (Exception e) {
      log.warn("Product Type is not recognize for : {} , for itemSku : {}", productAndItem.getProductType(),
          productAndItem.getItemSku(), e);
    }
    return itemSummaryResponse;
  }

  public static String generatePickupPointKey(String itemSku, String pickupPointCode) {
    return new StringBuilder(itemSku).append(Constants.COLON).append(pickupPointCode).toString();
  }

  public static String generateOfflineItemId(String itemSku, String pickupPointCode) {
    return new StringBuilder(itemSku).append(Constants.HYPHEN).append(pickupPointCode).toString();
  }

  public static ProductItemsVo toProductItemVo(Product product, List<Item> items) {
    ProductVo productVo = new ProductVo();
    BeanUtils.copyProperties(product, productVo);
    List<ItemVo> itemVoList = items.stream().map(CommonUtil::toItemVo).collect(Collectors.toList());
    return new ProductItemsVo(productVo, itemVoList);
  }

  public static ReelProductListingRequestVo toReelProductListingRequestVo(ReelProductListingRequest reelProductListingRequest) {
    ReelProductListingRequestVo reelProductListingRequestVo = new ReelProductListingRequestVo();
    BeanUtils.copyProperties(reelProductListingRequest, reelProductListingRequestVo);
    return reelProductListingRequestVo;
  }

  public static List<ReelProductDetailResponse> toReelProductDetailResponses(
      List<ProductSolr> productSolrs) {
    List<ReelProductDetailResponse> reelProductDetailResponses = new ArrayList<>();
    for (ProductSolr productSolr : productSolrs) {
      reelProductDetailResponses.add(
          ReelProductDetailResponse.builder().productName(productSolr.getProductName())
              .productSku(productSolr.getProductSku()).inStock(productSolr.getInStock())
              .mainImageUrl(productSolr.getProductMainImage())
              .maximumSellingPrice(productSolr.getMaximumSellingPrice())
              .minimumSellingPrice(productSolr.getMinimumSellingPrice())
              .archived(productSolr.getIsArchived()).suspended(productSolr.isSuspended()).build());
    }
    return reelProductDetailResponses;
  }

  public static ProductItemsVo toProductItemVoConverter(Product product, List<Item> items,
      Map<String, ItemPickupPoint> itemPickupPointMap, boolean overrideArchivalFlagAtl4ByL3) {
    ProductVo productVo = new ProductVo();
    BeanUtils.copyProperties(product, productVo);
    List<ItemVo> itemVoList = new ArrayList<>();
    for (Item item : items){
      ItemVo itemVo = toItemVo(item);
      itemVo.setCncActive(itemPickupPointMap.get(item.getItemSku()).isCncActive());
      ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
      BeanUtils.copyProperties(itemPickupPointMap.get(item.getItemSku()), itemPickupPointVo);
      itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
      if (overrideArchivalFlagAtl4ByL3) {
        itemVo.setArchived(productVo.isArchived());
      }
      itemVoList.add(itemVo);
    }
    return new ProductItemsVo(productVo, itemVoList);
  }


  public static ItemVo toItemVo(Item item) {
    ItemVo itemVo = new ItemVo();
    BeanUtils.copyProperties(item, itemVo);
    return itemVo;
  }

  public static MasterDataWithProductItemsVo toMasterDataWithProductItemsVo(
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo) {
    MasterDataWithProductItemsVo masterDataWithProductItemsVo = new MasterDataWithProductItemsVo();
    masterDataWithProductItemsVo.setMasterDataProducts(masterDataDetailWithProductAndItemsResponseVo.getMasterDataProducts());
    masterDataWithProductItemsVo.setMasterDataItems(masterDataDetailWithProductAndItemsResponseVo.getMasterDataItems());
    masterDataWithProductItemsVo.setProductItemsVos(
        masterDataDetailWithProductAndItemsResponseVo.getProductAndItems().stream().map(
            productAndItemsVO -> CommonUtil.toProductItemVo(productAndItemsVO.getProduct(), productAndItemsVO.getItems())).collect(Collectors.toList()));
    return masterDataWithProductItemsVo;
  }

  public static List<ItemPickupPoint> toItemPickupPointList(List<Item> itemList) {
    return itemList.stream().map(CommonUtil::toItemPickupPoint).collect(Collectors.toList());
  }

  private static ItemPickupPoint toItemPickupPoint(Item item) {
    ItemPickupPoint itemPickupPoint =
      ItemPickupPoint.builder().offlineItemId(item.getItemSku() + (StringUtils.isEmpty(item.getPickupPointCode()) ?
          StringUtils.EMPTY : Constants.HYPHEN + item.getPickupPointCode()))
        .itemSku(item.getItemSku())
        .merchantSku(item.getMerchantSku())
        .merchantCode(item.getMerchantCode())
        .pickupPointCode(item.getPickupPointCode())
        .cncActive(false)
        .delivery(true)
        .productSku(item.getProductSku())
        .price(item.getPrice())
        .itemViewConfig(item.getItemViewConfigs())
        .wholesalePriceExists(item.isWholesalePriceExists())
        .promoBundling(item.isPromoBundling())
        .activePromoBundlings(item.getActivePromoBundlings())
        .merchantPromoDiscount(item.isMerchantPromoDiscount())
        .flashSaleActive(item.isFlashSaleActive())
        .build();
    itemPickupPoint.setStoreId(item.getStoreId());
    return itemPickupPoint;
  }

  public static ItemPickupPoint toItemPickupPointOnConflict(Item item,
    ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.setDelivery(true);
    itemPickupPoint.setMarkForDelete(false);
    item.setWholesalePriceExists(item.isWholesalePriceExists());
    item.setPromoBundling(item.isPromoBundling());
    item.setActivePromoBundlings(item.getActivePromoBundlings());
    item.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
    item.setFlashSaleActive(item.isFlashSaleActive());
    itemPickupPoint.setItemViewConfig(item.getItemViewConfigs());
    if (CollectionUtils.isNotEmpty(item.getPrice()) && CollectionUtils.isNotEmpty(
      itemPickupPoint.getPrice())) {
      if (isItemPriceHigher(item.getPrice().stream().findFirst().get(),
        itemPickupPoint.getPrice().stream().findFirst().get())) {
        itemPickupPoint.setPrice(item.getPrice());
      }
    }
    return itemPickupPoint;
  }

  private static boolean isItemPriceHigher(com.gdn.x.product.model.entity.Price itemPrice,
    com.gdn.x.product.model.entity.Price itemPickupPointPrice) {
    if (itemPrice.getListPrice() > itemPickupPointPrice.getListPrice() ||
    itemPrice.getOfferPrice() > itemPickupPointPrice.getOfferPrice()) {
      return true;
    }
    return false;
  }

  public static void setItemChangeToItemPickupPoint(ItemChange itemChange,
    ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.setDelivery(true);
    itemPickupPoint.setMarkForDelete(itemChange.isForceReview());
    itemPickupPoint.setMerchantPromoDiscount(itemChange.isMerchantPromoDiscount());
    itemPickupPoint.setWholesalePriceExists(itemChange.isWholesalePriceExists());
    itemPickupPoint.setMerchantSku(itemChange.getMerchantSku());
    itemPickupPoint.setActivePromoBundlings(itemChange.getActivePromoBundlings());
    itemPickupPoint.setFlashSaleActive(itemChange.isFlashSaleActive());
    itemPickupPoint.setItemViewConfig(toItemViewConfigs(itemChange.getItemViewConfigs()));
    itemPickupPoint.setPrice(toItemPrices(itemChange.getPrice()));
    itemPickupPoint.setPromoBundling(itemChange.isPromoBundling());
  }

  public static Set<com.gdn.x.product.model.entity.Price> toItemPrices(Set<Price> priceEvents) {
    Set<com.gdn.x.product.model.entity.Price> itemPrices = new HashSet<>();
    for (Price priceEvent : priceEvents) {
      com.gdn.x.product.model.entity.Price itemPrice = new com.gdn.x.product.model.entity.Price();
      BeanUtils.copyProperties(priceEvent, itemPrice);
      itemPrices.add(itemPrice);
    }
    return itemPrices;
  }

  public static Set<com.gdn.x.product.model.entity.ItemViewConfig> toItemViewConfigs(
    Set<ItemViewConfig> itemViewConfigEvents) {
    Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigs = new HashSet<>();
    for (ItemViewConfig itemViewConfigEvent : itemViewConfigEvents) {
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig =
        new com.gdn.x.product.model.entity.ItemViewConfig();
      BeanUtils.copyProperties(itemViewConfigEvent, itemViewConfig);
      itemViewConfigs.add(itemViewConfig);
    }
    return itemViewConfigs;
  }

  public static ItemPickupPoint setOfflineItemChangeToItemPickupPoint(
    OfflineItemChange offlineItemChange) {
    ItemPickupPoint itemPickupPoint =
      ItemPickupPoint.builder().offlineItemId(offlineItemChange.getUniqueId())
        .itemSku(offlineItemChange.getItemSku())
        .merchantSku(offlineItemChange.getMerchantSku())
        .merchantCode(offlineItemChange.getMerchantCode())
        .pickupPointCode(offlineItemChange.getPickupPointCode())
        .cncActive(true)
        .delivery(false)
        .productSku(offlineItemChange.getProductSku())
        .externalPickupPointCode(offlineItemChange.getExternalPickupPointCode())
        .build();
    itemPickupPoint.setPrice(generateNewPriceFromOfflineItemChange(offlineItemChange));
    itemPickupPoint.setItemViewConfig(generateNewItemViewConfigForOfflineItemChange());
    itemPickupPoint.setStoreId(offlineItemChange.getStoreId());
    return itemPickupPoint;
  }

  private static Set<com.gdn.x.product.model.entity.ItemViewConfig> generateNewItemViewConfigForOfflineItemChange() {
    Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigSet = new HashSet<>();
    com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig =
      new com.gdn.x.product.model.entity.ItemViewConfig();
    itemViewConfig.setBuyable(false);
    itemViewConfig.setDiscoverable(false);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfigSet.add(itemViewConfig);
    return itemViewConfigSet;
  }

  private static Set<com.gdn.x.product.model.entity.Price> generateNewPriceFromOfflineItemChange(
    OfflineItemChange offlineItemChange) {
    Set<com.gdn.x.product.model.entity.Price> priceSet = new HashSet<>();
    com.gdn.x.product.model.entity.Price price = new com.gdn.x.product.model.entity.Price();
    price.setOfferPrice(offlineItemChange.getOfferPrice());
    price.setListPrice(offlineItemChange.getListPrice());
    price.setLastUpdatedBy(offlineItemChange.getUsername());
    price.setLastUpdatedDate(offlineItemChange.getUpdatedDate());
    priceSet.add(price);
    return priceSet;
  }

  public static void setOfflineItemChangeToExistingItemPickupPoint(
    OfflineItemChange offlineItemChange, ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.setMerchantSku(offlineItemChange.getMerchantSku());
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setPrice(generateNewPriceFromOfflineItemChange(offlineItemChange));
    itemPickupPoint.setExternalPickupPointCode(offlineItemChange.getExternalPickupPointCode());
  }

  public static List<ItemPickupPointPriceVo> toItemPickupPointPriceVoList(
    List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
      .map(CommonUtil::toItemPickupPointPriceVo).collect(Collectors.toList());
  }

  public static ItemPickupPointPriceVo toItemPickupPointPriceVo(ItemPickupPoint itemPickupPoint) {
    ItemPickupPointPriceVo itemPickupPointPriceVo = ItemPickupPointPriceVo.builder()
      .offlineItemId(itemPickupPoint.getOfflineItemId())
      .merchantCode(itemPickupPoint.getMerchantCode())
      .pickupPointCode(itemPickupPoint.getPickupPointCode())
      .itemSku(itemPickupPoint.getItemSku())
      .listPrice(itemPickupPoint.getPrice().stream().findFirst().get().getListPrice())
      .offerPrice(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice())
      .build();
    itemPickupPointPriceVo.setId(itemPickupPoint.getId());
    itemPickupPointPriceVo.setStoreId(itemPickupPoint.getStoreId());
    itemPickupPointPriceVo.setCreatedDate(itemPickupPoint.getCreatedDate());
    itemPickupPointPriceVo.setCreatedBy(itemPickupPoint.getCreatedBy());
    itemPickupPointPriceVo.setUpdatedDate(itemPickupPoint.getUpdatedDate());
    itemPickupPointPriceVo.setUpdatedBy(itemPickupPoint.getUpdatedBy());
    itemPickupPointPriceVo.setVersion(itemPickupPoint.getVersion());
    return itemPickupPointPriceVo;
  }

  public static OfflineItemHistoryDetailVO constructOfflineItemHistoryDetail(UpsertOfflineItemRequest upsertOfflineItemRequest,
      boolean syncPriceAction, String clientId, String requestId) {
    return OfflineItemHistoryDetailVO.builder().oldListPrice(
            Optional.ofNullable(upsertOfflineItemRequest).map(UpsertOfflineItemRequest::getListPrice).orElse(null))
        .oldOfferPrice(
            Optional.ofNullable(upsertOfflineItemRequest).map(UpsertOfflineItemRequest::getOfferPrice).orElse(null))
        .syncPriceAction(syncPriceAction).clientId(clientId).requestId(requestId).build();
  }

  public static OfflineItemChange convertToOfflineItemChange(ItemPickupPoint itemPickupPoint, Map<String, Item> itemMap) {
    com.gdn.x.product.model.entity.Price price =
        itemPickupPoint.getPrice().stream().findFirst().orElse(new com.gdn.x.product.model.entity.Price());
    Item item = itemMap.get(itemPickupPoint.getItemSku());

    OfflineItemChange offlineItemChange = new OfflineItemChange();
    offlineItemChange.setExternalPickupPointCode(itemPickupPoint.getExternalPickupPointCode());
    offlineItemChange.setItemSku(itemPickupPoint.getItemSku());
    offlineItemChange.setProductSku(itemPickupPoint.getProductSku());
    offlineItemChange.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    offlineItemChange.setMerchantCode(itemPickupPoint.getMerchantCode());
    offlineItemChange.setMerchantSku(itemPickupPoint.getMerchantSku());
    offlineItemChange.setListPrice(Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice()));
    offlineItemChange.setOfferPrice(price.getOfferPrice());
    offlineItemChange.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    offlineItemChange.setStoreId(itemPickupPoint.getStoreId());
    offlineItemChange.setUniqueId(itemPickupPoint.getOfflineItemId());
    offlineItemChange.setNewData(Optional.ofNullable(itemPickupPoint.getNewData()).orElse(false));
    offlineItemChange.setUpdatedDate(new Date());
    offlineItemChange.setItemCode(item.getItemCode());
    offlineItemChange.setUsername(itemPickupPoint.getUpdatedBy());
    if (Objects.nonNull(itemPickupPoint.getOfflineItemHistoryDetail())) {
      offlineItemChange.setOldListPrice(
          Optional.ofNullable(itemPickupPoint.getOfflineItemHistoryDetail().getOldListPrice())
              .orElse(itemPickupPoint.getOfflineItemHistoryDetail().getOldOfferPrice()));
      offlineItemChange.setOldOfferPrice(itemPickupPoint.getOfflineItemHistoryDetail().getOldOfferPrice());
      offlineItemChange.setSyncPriceAction(itemPickupPoint.getOfflineItemHistoryDetail().getSyncPriceAction());
      offlineItemChange.setRequestId(itemPickupPoint.getOfflineItemHistoryDetail().getRequestId());
      offlineItemChange.setClientId(itemPickupPoint.getOfflineItemHistoryDetail().getClientId());
    }

    return offlineItemChange;
  }

  public static boolean checkItemChanged(boolean isItemDataChanged, boolean isShippingChanged,
    boolean isOff2OnChannelActiveChanged, boolean isPriceChanged, boolean isPromoBundlingChanged) {
    return isItemDataChanged || isShippingChanged || isOff2OnChannelActiveChanged || isPriceChanged
      || isPromoBundlingChanged;
  }

  public static Map<String, Object> getFieldsAndValuesForUpdatedCNCActivaAndPickupPointAndVariant(List<ItemPickupPoint> itemPickupPoints) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    List<String> pickupPointCodes =
        itemPickupPoints.stream().map(ItemPickupPoint::getPickupPointCode).distinct().collect(Collectors.toList());
    fieldsAndValues.put(SolrFieldNames.PICKUP_POINT_CODES,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
    fieldsAndValues.put(SolrFieldNames.CNC_ACTIVE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        itemPickupPoints.stream().anyMatch(ItemPickupPoint::isCncActive)));
    fieldsAndValues.put(SolrFieldNames.L5_COUNT,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPickupPoints.size()));
    return fieldsAndValues;
  }

  public static SolrInputDocument getL3SolrInputDocumentsForUpdatedCNCActivaAndPickupPointAndVariant(String productSku, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.PRODUCT_SKU, productSku);
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, items.get(0).getUpdatedDate()));
    List<String> pickupPointCodes =
        itemPickupPoints.stream().map(ItemPickupPoint::getPickupPointCode).distinct().collect(Collectors.toList());
    solrInputDocument.setField(SolrFieldNames.PICKUP_POINT_CODES,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, pickupPointCodes));
    solrInputDocument.setField(SolrFieldNames.CNC_ACTIVE, Collections.singletonMap(SolrConstants.SET_CLAUSE,
        itemPickupPoints.stream().anyMatch(ItemPickupPoint::isCncActive)));
    solrInputDocument.setField(SolrFieldNames.L5_COUNT,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, itemPickupPoints.size()));
    return solrInputDocument;
  }

  public static Set<String> getDistinctPickupPointCodes(List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream()
      .map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toSet());
  }

  public static boolean isDefiningOrVariantCreating(ProductAttributeDomainEventModel masterDataProductAttribute) {
    return
        Objects.nonNull(masterDataProductAttribute.getAttribute()) && (MasterDataAttributeType.DEFINING_ATTRIBUTE.name()
            .equals(masterDataProductAttribute.getAttribute().getAttributeType())
            || masterDataProductAttribute.getAttribute().isVariantCreation());
  }

  public static boolean isDefiningOrVariantCreating(ProductItemAttributeValueResponse productItemAttributeValueResponse) {
    return Objects.nonNull(productItemAttributeValueResponse.getAttributeResponse()) && (
        MasterDataAttributeType.DEFINING_ATTRIBUTE.name()
            .equals(productItemAttributeValueResponse.getAttributeResponse().getAttributeType())
            || productItemAttributeValueResponse.getAttributeResponse().isVariantCreation());
  }

  public static List<ItemPriceChangeEventModel> convertToItemPriceChangeEventModel(String productCode,
      ParentCategory parentCategory, Product product, List<ItemPickupPoint> itemPickupPoints,
      Map<String, Item> itemMap) {
    List<ItemPriceChangeEventModel> itemPriceChangeEventModels = new ArrayList();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      ItemPriceChangeEventModel itemPriceChangeEventModel = new ItemPriceChangeEventModel();
      itemPriceChangeEventModel.setParentCategory(parentCategory);
      itemPriceChangeEventModel.setProductCode(productCode);
      itemPriceChangeEventModel.setMerchantCode(itemPickupPoint.getMerchantCode());
      itemPriceChangeEventModel.setItemCode(item.getItemCode());
      itemPriceChangeEventModel.setPristineId(
          Objects.nonNull(item.getPristineDataItem()) ? item.getPristineDataItem().getPristineId() : StringUtils.EMPTY);
      itemPriceChangeEventModel.setItemSku(itemPickupPoint.getItemSku());
      itemPriceChangeEventModel.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      itemPriceChangeEventModel.setArchived(product.isArchived());
      itemPriceChangeEventModel.setMarkForDelete(itemPickupPoint.isMarkForDelete());
      itemPriceChangeEventModel.setStoreId(itemPickupPoint.getStoreId());
      Set<com.gdn.x.product.domain.event.model.Price> prices = new HashSet<>();
      for (com.gdn.x.product.model.entity.Price itemPrice : itemPickupPoint.getPrice()) {
        com.gdn.x.product.domain.event.model.Price price = new com.gdn.x.product.domain.event.model.Price();
        org.springframework.beans.BeanUtils.copyProperties(itemPrice, price);
        prices.add(price);
      }
      itemPriceChangeEventModel.setPrice(prices);
      itemPriceChangeEventModels.add(itemPriceChangeEventModel);
    }
    return itemPriceChangeEventModels;
  }

  public static MerchantPromoDiscountChangeEvent toMerchantPromoDiscountChangeEventWithPickupPoint(
    ItemPickupPoint itemPickupPoint) throws Exception {
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent = new MerchantPromoDiscountChangeEvent();
    merchantPromoDiscountChangeEvent.setMerchantCode(itemPickupPoint.getMerchantCode());
    merchantPromoDiscountChangeEvent.setItemSku(itemPickupPoint.getItemSku());
    merchantPromoDiscountChangeEvent.setProductSku(itemPickupPoint.getProductSku());
    merchantPromoDiscountChangeEvent.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    merchantPromoDiscountChangeEvent.setStoreId(itemPickupPoint.getStoreId());
    merchantPromoDiscountChangeEvent.setPrice(toPrices(itemPickupPoint.getPrice(), itemPickupPoint.getItemSku()));
    merchantPromoDiscountChangeEvent.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    return merchantPromoDiscountChangeEvent;
  }

  public static List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(
      List<ItemPickupPoint> itemPickupPointList) {
    return itemPickupPointList.stream().map(CommonUtil::getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO)
        .collect(Collectors.toList());
  }

  private static WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(
      ItemPickupPoint itemPickupPoint) {
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setWebItemSku(itemPickupPoint.getItemSku());
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setWebMerchantCode(itemPickupPoint.getMerchantCode());
    return webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
  }

  public static List<MasterDataItemImageDTO> toMasterDataItemImageDTO(String mainImageUrl) {
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath(mainImageUrl);
    masterDataItemImageDTO.setMainImage(true);
    masterDataItemImageDTO.setSequence(0);
    return Collections.singletonList(masterDataItemImageDTO);
  }

  public static boolean isRequestHasItemCodeOrKeywordFilter(Set<String> itemCodes, String keyword) {
    return CollectionUtils.isNotEmpty(itemCodes) || StringUtils.isNotBlank(keyword);
  }

  public static boolean isItemSkuRequired(Set<String> itemCodes, String keyword, String itemSku) {
    return StringUtils.isNotEmpty(itemSku) || isRequestHasItemCodeOrKeywordFilter(itemCodes, keyword);
  }

  public static List<SolrInputDocument> getSolrDocumentListForAtomicUpdateOnPromoFlagChangeByItemSkus(
      List<String> itemSkus, boolean activated, String fieldName, String merchantCode) {
    return itemSkus.stream()
        .map(item -> getSolrInputDocumentForAtomicUpdateOnPromoFlagChangeByItemSku(item, activated, fieldName,
            merchantCode))
        .collect(Collectors.toList());
  }

  public static SolrInputDocument getSolrInputDocumentForAtomicUpdateOnPromoFlagChangeByItemSku(String itemSku,
      boolean activated, String fieldName, String merchantCode) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, itemSku);
    solrInputDocument.setField(SolrFieldNames.UPDATED_DATE,
        Collections.singletonMap(SolrConstants.SET_CLAUSE, new Date()));
    solrInputDocument.setField(fieldName, Collections.singletonMap(SolrConstants.SET_CLAUSE, activated));
    solrInputDocument.setField(SolrFieldNames.MERCHANT_CODE, merchantCode);
    return solrInputDocument;
  }

  public static Boolean getBuyableFromConfig(com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    Date now = new Date();
    return itemViewConfig.isBuyable() || (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())
        && itemViewConfig.getItemBuyableSchedules().getStartDateTime().before(now)
        && itemViewConfig.getItemBuyableSchedules().getEndDateTime().after(now)
        && itemViewConfig.getItemBuyableSchedules().isBuyable());
  }

  public static Boolean getBuyableFromItemViewConfigConfig(
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    Date now = new Date();
    return itemViewConfig.isBuyable() || (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())
        && Objects.nonNull(itemViewConfig.getItemBuyableSchedules().getStartDateTime())
        && Objects.nonNull(itemViewConfig.getItemBuyableSchedules().getEndDateTime())
        && itemViewConfig.getItemBuyableSchedules().getStartDateTime().before(now)
        && itemViewConfig.getItemBuyableSchedules().getEndDateTime().after(now)
        && itemViewConfig.getItemBuyableSchedules().isBuyable());
  }

  public static Boolean getDiscoverableFromConfig(com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    Date now = new Date();
    return itemViewConfig.isDiscoverable() || (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())
        && itemViewConfig.getItemDiscoverableSchedules().getStartDateTime().before(now)
        && itemViewConfig.getItemDiscoverableSchedules().getEndDateTime().after(now)
        && itemViewConfig.getItemDiscoverableSchedules().isDiscoverable());
  }

  //making common method to create map with key as itemSku and pp code
  public static Map<String, ItemPickupPoint> toItemPickupPointMap(
    List<ItemPickupPoint> itemPickupPointList) {
    Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
    itemPickupPointList.forEach(itemPickupPoint -> itemPickupPointMap
      .put(itemPickupPoint.getItemSku() + Constants.DASH + itemPickupPoint.getPickupPointCode(),
        itemPickupPoint));
    return itemPickupPointMap;
  }

  public static boolean isDimensionUpdated(ProductDetailResponse productDetailResponse, Item savedItem) {
    if (Objects.nonNull(productDetailResponse.getLength()) && productDetailResponse.getLength() != savedItem
        .getLength()) {
      return true;
    }
    if (Objects.nonNull(productDetailResponse.getWeight()) && productDetailResponse.getWeight() != savedItem
        .getWeight()) {
      return true;
    }
    if (Objects.nonNull(productDetailResponse.getWidth()) && productDetailResponse.getWidth() != savedItem.getWidth()) {
      return true;
    }
    if (Objects.nonNull(productDetailResponse.getHeight()) && productDetailResponse.getHeight() != savedItem
        .getHeight()) {
      return true;
    }
    return Objects.nonNull(productDetailResponse.getShippingWeight())
        && productDetailResponse.getShippingWeight() != savedItem.getShippingWeight();
  }

  public static List<FieldUpdateRequestVo> toFieldUpdateRequestVos(String fieldName, Object value) {
    FieldUpdateRequestVo fieldUpdateRequestVo = new FieldUpdateRequestVo(fieldName, value);
    List<FieldUpdateRequestVo> fieldUpdateRequestVos = new ArrayList<>();
    fieldUpdateRequestVos.add(fieldUpdateRequestVo);
    return fieldUpdateRequestVos;
  }

  public static void deletePickupPointsPromoAndDiscountDetails(List<ItemPickupPoint> itemPickupPointList) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      itemPickupPoint.setPromoBundling(false);
      itemPickupPoint.setActivePromoBundlings(new HashSet<>());
      itemPickupPoint.setMerchantPromoDiscount(false);
      for (com.gdn.x.product.model.entity.Price price : itemPickupPoint.getPrice()) {
        price.setListOfDiscountPrices(new ArrayList<>());
        price.setMerchantPromoDiscountPrice(null);
      }
    }
  }

  public static PickupPointUpdateRequest toPickupPointUpdateRequest(
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo) {
    return PickupPointUpdateRequest.builder()
      .productSku(itemPickupPointListingUpdateRequestVo.getProductSku()).differentLocation(true)
      .pickupPointUpdateItemRequestList((Collections.singletonList(
        PickupPointUpdateItemRequest.builder()
          .itemSku(itemPickupPointListingUpdateRequestVo.getItemSku())
            .pickupPointCode(itemPickupPointListingUpdateRequestVo.getPickupPointCode())
            .build())))
      .build();
  }

  public static Set<String> getItemPickupPointIds(Collection<ItemPickupPoint> itemPickupPoints) {
    return Optional.ofNullable(itemPickupPoints).orElseGet(() -> new ArrayList<>()).stream().map(ItemPickupPoint::getId)
        .collect(Collectors.toSet());
  }

  public static Set<ItemPickupPoint> combineListOfUpdatedAndNotUpdatedPickupPoint(
      Collection<ItemPickupPoint> notUpdatedItemPickupPoint, Collection<ItemPickupPoint> updatedItemPickupPoint) {
    Set<ItemPickupPoint> combinedList = new HashSet<>();

    if (CollectionUtils.isNotEmpty(notUpdatedItemPickupPoint)) {
      combinedList.addAll(notUpdatedItemPickupPoint);
    }

    if (CollectionUtils.isNotEmpty(updatedItemPickupPoint)) {
      combinedList.addAll(updatedItemPickupPoint.stream().filter(itemPickupPoint -> !itemPickupPoint.isMarkForDelete())
          .collect(Collectors.toList()));
    }

    return combinedList;
  }

  public static boolean isProductRejected(Product product) {
    if (product.isMarkForDelete() && !(product.isSuspended() || product.isForceReview())) {
      return true;
    }
    return false;
  }


  public static List<SortedDefiningAttributeDTO> getSortedDefiningAttributes(MasterDataProductDTO masterDataProductDTO) {
    List<SortedDefiningAttributeDTO> list = new ArrayList<>();
    try {
      for (MasterDataProductAttributeDTO masterDataProductAttributeDTO : masterDataProductDTO.getMasterDataProductAttributes()) {
        if (isDefiningAttribute(masterDataProductAttributeDTO)) {
          SortedDefiningAttributeDTO sortedDefiningAttributeFromMasterDataProductAttribute =
              getSortedDefiningAttributeFromMasterDataProductAttributeDto(masterDataProductAttributeDTO);
          list.add(sortedDefiningAttributeFromMasterDataProductAttribute);
        }
      }
    } catch (Exception e) {
      log.error("failed to sort definingAttributes with error = ", e);
    }
    return list;
  }

  public static SortedDefiningAttributeDTO getSortedDefiningAttributeFromMasterDataProductAttributeDto(
      MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    List<String> attributeValues = masterDataProductAttributeDTO.getMasterDataProductAttributeValues().stream()
        .map(CommonUtil::getDecriptiveValueOrAllowedAttributeValue).filter(Objects::nonNull)
        .map(CommonUtil::setMaxValueToSequenceIfNotPresent)
        .sorted(Comparator.comparing(MasterDataAllowedAttributeValueDTO::getSequence))
        .map(MasterDataAllowedAttributeValueDTO::getValue).collect(Collectors.toList());
    SortedDefiningAttributeDTO sortedDefiningAttributeDto = new SortedDefiningAttributeDTO();
    sortedDefiningAttributeDto.setAttributeName(
        masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeName());
    sortedDefiningAttributeDto.setDefiningAttributes(attributeValues);
    return sortedDefiningAttributeDto;
  }

  private static boolean isDefiningAttribute(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    return Objects.nonNull(masterDataProductAttributeDTO.getMasterDataAttribute())
        && MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
        masterDataProductAttributeDTO.getMasterDataAttribute().getAttributeType())
        || masterDataProductAttributeDTO.getMasterDataAttribute().isVariantCreation();
  }

  private static MasterDataAllowedAttributeValueDTO getDecriptiveValueOrAllowedAttributeValue(
      MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO) {
    if (Objects.isNull(masterDataProductAttributeValueDTO.getAllowedAttributeValue())) {
      MasterDataAllowedAttributeValueDTO masterDataAllowedAttributeValueDTO = new MasterDataAllowedAttributeValueDTO();
      masterDataAllowedAttributeValueDTO.setValue(masterDataProductAttributeValueDTO.getDescriptiveAttributeValue());
      return masterDataAllowedAttributeValueDTO;
    }
    return masterDataProductAttributeValueDTO.getAllowedAttributeValue();
  }

  private static MasterDataAllowedAttributeValueDTO setMaxValueToSequenceIfNotPresent(
      MasterDataAllowedAttributeValueDTO attributeValue) {
    if (Objects.isNull(attributeValue.getSequence())) {
      attributeValue.setSequence(Integer.MAX_VALUE);
    }
    return attributeValue;
  }

  public static boolean isPriceChanged(double listPriceRequest, double listPriceCurrent, double offerPriceRequest,
      double offerPriceCurrent) {
    return (Double.compare(listPriceCurrent, listPriceRequest) != 0
        || Double.compare(offerPriceCurrent, offerPriceRequest) != 0);
  }

  public static boolean isBasePriceChanged(Double basePriceCurrent, Double basePriceRequest) {
    if (Objects.isNull(basePriceCurrent) && Objects.isNull(basePriceRequest)) {
      return false;
    }
    if (Objects.isNull(basePriceCurrent) || Objects.isNull(basePriceRequest)) {
      return true;
    }
    return Double.compare(basePriceCurrent, basePriceRequest) != 0;
  }

  public static boolean isManagedChanged(boolean managedCurrent, boolean managedRequest) {
    return managedCurrent != managedRequest;
  }

  public static boolean isB2bBuyableChanged(boolean buyableCurrent, boolean buyableRequest) {
    return buyableCurrent != buyableRequest;
  }

  public static boolean isB2bDiscoverableChanged(boolean discoverableCurrent, boolean discoverableRequest) {
    return discoverableCurrent != discoverableRequest;
  }

  public static boolean isB2bFieldsGotUpdated(boolean isManagedFlagChanged, boolean isBasePriceChanged,
      boolean isB2bBuyableChanged, boolean isB2bDisplayableChanged, List<AuditTrailDto> auditTrailDtoList, Item item,
      ItemPickupPoint itemPickupPoint, ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo) {
    if (isManagedFlagChanged) {
      auditTrailDtoList.add(getAuditTrailDto(String.valueOf(itemPickupPoint.getB2bFields().isManaged()),
          String.valueOf(itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().isManaged()), Constants.B2B_MANAGED,
          itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(), item.getItemSku(),
          item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
    }
    if (isBasePriceChanged) {
      auditTrailDtoList.add(getAuditTrailDto(String.valueOf(itemPickupPoint.getB2bFields().getBasePrice()),
          String.valueOf(itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getBasePrice()), Constants.B2B_PRICE,
          itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(), item.getItemSku(),
          item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
    }
    if (isB2bBuyableChanged) {
      auditTrailDtoList.add(getAuditTrailDto(String.valueOf(
              !itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getB2bItemViewConfigs().iterator().next()
                  .isBuyable()), String.valueOf(
              itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getB2bItemViewConfigs().iterator().next().isBuyable()),
          Constants.B2B_BUYABLE, itemPickupPoint.getMerchantCode(), itemPickupPoint.getProductSku(), item.getItemSku(),
          item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode()));
    }

    if (isB2bDisplayableChanged) {
      auditTrailDtoList.add(getAuditTrailDto(String.valueOf(
              !itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getB2bItemViewConfigs().iterator().next()
                  .isDiscoverable()), String.valueOf(
              itemPickupPointListingUpdateRequestVo.getB2bFieldsVo().getB2bItemViewConfigs().iterator().next()
                  .isDiscoverable()), Constants.B2B_DISCOVERABLE, itemPickupPoint.getMerchantCode(),
          itemPickupPoint.getProductSku(), item.getItemSku(), item.getGeneratedItemName(),
          itemPickupPoint.getPickupPointCode()));
    }
    return isBasePriceChanged || isManagedFlagChanged || isB2bDisplayableChanged || isB2bBuyableChanged;
  }

  public static void isSalesChannelChanged(String currentSalesChannel, String newSalesChannel, Product product,
      List<AuditTrailDto> auditTrailDtoList) {
    if (!currentSalesChannel.equals(newSalesChannel)) {
      auditTrailDtoList.add(
          getAuditTrailDto(currentSalesChannel, newSalesChannel, Constants.B2C_ACTIVATED, product.getMerchantCode(),
              product.getProductSku(), Constants.DEFAULT, product.getProductName(), Constants.HYPHEN));
    }
  }

  public static String getSalesChannelOfProductForMultiChannelSeller(Product product, BusinessPartner businessPartner) {
    String resultantChannel = SalesChannel.NO_CHANNEL.getName();
    if (generateSellerChannelFromBusinessPartner(businessPartner).size() > 1) {
      if (product.isB2bActivated() && (product.getB2cActivated())) {
        resultantChannel = SalesChannel.COMBINED_CHANNEL.getName();
      } else if (product.isB2bActivated()) {
        resultantChannel = SalesChannel.B2B_CHANNEL.getName();
      } else if ((product.getB2cActivated())) {
        resultantChannel = SalesChannel.B2C_CHANNEL.getName();
      }
    }
    return resultantChannel;
  }

  public static AuditTrailDto getAuditTrailDto(String oldValue, String newValue, String action, String merchantCode,
      String productSku, String itemSku, String itemName, String pickupPointCode) {
    return new AuditTrailDto(merchantCode, itemSku, action, oldValue, newValue, null, productSku, itemName,
        pickupPointCode, false);
  }

  public static boolean isB2bFlagUpdatedAtL3Level(Product product, boolean discoverableRequest,
      boolean buyableRequest) {
    boolean isB2bFlagUpdatedAtL3Level = false;
    if (!product.isB2bActivated() && (discoverableRequest || buyableRequest)) {
      product.setB2bActivated(true);
      isB2bFlagUpdatedAtL3Level = true;
    }
    return isB2bFlagUpdatedAtL3Level;
  }

  public static List<ProductSkuPickupPointResponseV2> getProductSkuPickUpPointList(
    List<ItemPickupPoint> itemPickupPointList) {
    return Optional.ofNullable(itemPickupPointList).orElse(new ArrayList<>()).stream().map(
      CommonUtil::getProductSkuPickUpPoint).collect(Collectors.toList());
  }

  public static ItemSkuAndPickupPointCodeResponse getItemSkuAndPickupPointCodeResponse(
      ItemPickupPoint itemPickupPoint) {
    ItemSkuAndPickupPointCodeResponse itemSkuAndPickupPointCodeResponse =
        ItemSkuAndPickupPointCodeResponse.builder().itemSku(itemPickupPoint.getItemSku())
            .pickupPointCode(itemPickupPoint.getPickupPointCode()).build();
    itemSkuAndPickupPointCodeResponse.setStoreId(itemPickupPoint.getStoreId());
    itemSkuAndPickupPointCodeResponse.setCreatedDate(itemPickupPoint.getCreatedDate());
    itemSkuAndPickupPointCodeResponse.setUpdatedDate(itemPickupPoint.getUpdatedDate());
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
      itemSkuAndPickupPointCodeResponse.setBuyable(itemPickupPoint.getItemViewConfig().iterator().next().isBuyable());
      itemSkuAndPickupPointCodeResponse.setDiscoverable(
          itemPickupPoint.getItemViewConfig().iterator().next().isDiscoverable());
    }
    itemSkuAndPickupPointCodeResponse.setCncActive(itemPickupPoint.isCncActive());
    itemSkuAndPickupPointCodeResponse.setMarkForDelete(itemPickupPoint.isMarkForDelete());
    return itemSkuAndPickupPointCodeResponse;
  }

  public static PriceUpdatedInTimeRangeL5Response toPriceUpdatedInTimeRangeL5Response(ItemPickupPoint itemPickupPoint) {
    PriceUpdatedInTimeRangeL5Response priceUpdatedInTimeRangeL5Response =
        PriceUpdatedInTimeRangeL5Response.builder().itemSku(itemPickupPoint.getItemSku())
            .pickupPointCode(itemPickupPoint.getPickupPointCode())
            .priceUpdatedDate(itemPickupPoint.getPriceUpdatedDate()).cncActive(itemPickupPoint.isCncActive())
            .markForDelete(itemPickupPoint.isMarkForDelete()).build();
    priceUpdatedInTimeRangeL5Response.setStoreId(itemPickupPoint.getStoreId());
    priceUpdatedInTimeRangeL5Response.setCreatedDate(itemPickupPoint.getCreatedDate());
    priceUpdatedInTimeRangeL5Response.setUpdatedDate(itemPickupPoint.getUpdatedDate());
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
      priceUpdatedInTimeRangeL5Response.setBuyable(itemPickupPoint.getItemViewConfig().iterator().next().isBuyable());
      priceUpdatedInTimeRangeL5Response.setDiscoverable(
          itemPickupPoint.getItemViewConfig().iterator().next().isDiscoverable());
    }
    return priceUpdatedInTimeRangeL5Response;
  }

  private static ProductSkuPickupPointResponseV2 getProductSkuPickUpPoint(
    ItemPickupPoint itemPickupPoint) {
    ProductSkuPickupPointResponseV2 pickupPointResponse = new ProductSkuPickupPointResponseV2();
    BeanUtils.copyProperties(itemPickupPoint, pickupPointResponse);
    pickupPointResponse.setItemViewConfig(
      CommonUtil.toItemViewConfigDTOs(itemPickupPoint.getAllItemViewConfigs()));
    return pickupPointResponse;
  }

  public static Date toDateFromTimestampObject(Object timestamp) {
    if (Objects.nonNull(timestamp)) {
      return new Date(Long.valueOf(String.valueOf(timestamp)));
    }
    return null;
  }

  public static boolean isDateField(String field) {
    if (StringUtils.equals(field,SolrFieldNames.PRODUCT_CENTER_UPDATED_DATE)) {
      return true;
    }
    return false;
  }

  public static void checkArgumentApiIncorrectInputDataException(boolean expression,
      String errorMessage, String errorCode) {
    if (!expression) {
      throw new ApiIncorrectInputDataException(errorMessage, errorCode);
    }
  }

  public static void checkEligibilityForB2cAndB2bActivated(Product product, List<ItemVo> itemVos, BusinessPartner businessPartner) {
    List<String> salesChannel = new ArrayList<>();
    if (Objects.nonNull(businessPartner) && CollectionUtils.isNotEmpty(businessPartner.getSalesChannel())) {
      salesChannel = businessPartner.getSalesChannel();
    }
    if (!salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
      for (ItemVo itemVo : itemVos) {
        for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
          updateItemViewConfig(Constants.DEFAULT, itemPickupPointVo.getAllItemViewConfigs(), false, false);
        }
      }
      product.setB2cActivated(false);
    }
    if (!salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      for (ItemVo itemVo : itemVos) {
        for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
          itemPickupPointVo.setItemViewConfig(removeItemViewConfigByChannel(Constants.B2B,
              itemPickupPointVo.getAllItemViewConfigs()));
          itemPickupPointVo.setB2bFields(null);
        }
      }
      product.setB2bActivated(false);
    }
  }

  private static void updateItemViewConfig(String channel, Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigs,
      boolean buyable, boolean discoverable) {
    for (com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig : itemViewConfigs) {
      if (channel.equals(itemViewConfig.getChannel())) {
        itemViewConfig.setBuyable(buyable);
        itemViewConfig.setDiscoverable(discoverable);
      }
    }
  }

  private static Set<com.gdn.x.product.model.entity.ItemViewConfig> removeItemViewConfigByChannel(String channel,
      Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigs) {
    return itemViewConfigs.stream().filter(itemViewConfig -> !(channel.equals(itemViewConfig.getChannel())))
        .collect(Collectors.toSet());
  }

  public static ItemPickupPoint setItemViewConfigFromRequest(ItemPickupPoint itemPickupPoint,
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig,
      boolean cncForWarehouseFeatureSwitch) {
    for (com.gdn.x.product.model.entity.ItemViewConfig currItemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
      if (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
        currItemViewConfig.setBuyable(itemViewConfig.isBuyable());
        currItemViewConfig.setDiscoverable(itemViewConfig.isDiscoverable());
        currItemViewConfig.setItemBuyableScheduleIfNotNull(itemViewConfig.getItemBuyableSchedules());
        currItemViewConfig.setItemDiscoverableScheduleIfNotNull(itemViewConfig.getItemDiscoverableSchedules());
        return itemPickupPoint;
      }
    }
    if (cncForWarehouseFeatureSwitch) {
      itemPickupPoint.getAllItemViewConfigs().add(itemViewConfig);
      return itemPickupPoint;
    }
    return null;
  }

  public static List<String> generateSellerChannelForL5Event(ProfileResponse profileResponse,
      List<String> sellerChannel) {
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && Objects.nonNull(
        profileResponse.getCompany().getSalesChannel())) {
      if (profileResponse.getCompany().getSalesChannel().contains(Constants.B2B_SELLER_CHANNEL)) {
        sellerChannel.add(Constants.B2B);
      }
      if (profileResponse.getCompany().getSalesChannel().contains(Constants.B2C_SELLER_CHANNEL)) {
        sellerChannel.add(Constants.RETAIL);
      }
    }
    return sellerChannel;
  }

  public static List<String> generateDefaultSellerChannelForL5Event(String defaultSellerChannelValue,
      List<String> sellerChannel) {
    if (Constants.B2C_SELLER_CHANNEL.equals(defaultSellerChannelValue)) {
      sellerChannel.add(Constants.RETAIL);
    }
    return sellerChannel;
  }

  public static boolean isProductMasterDataDetailChanged(Product product, ProductDetailResponse productDetailResponse,
      boolean updateCategory) {
    return !StringUtils.equals(productDetailResponse.getBrand(), product.getBrand()) || !StringUtils.equals(
        productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode(),
        product.getCategoryCode()) || !StringUtils.equals(productDetailResponse.getName(), product.getProductName())
        || updateCategory || !StringUtils.equals(product.getUrl(), productDetailResponse.getUrl()) || isVideoUpdated(
        product.getVideo(), productDetailResponse.getVideoDTO());
  }

  private static boolean isVideoUpdated(Video video, VideoDTO videoDTO) {
    if (Objects.isNull(video) && Objects.isNull(videoDTO)) {
      return false;
    }
    video = Optional.ofNullable(video).orElse(new Video());
    videoDTO = Optional.ofNullable(videoDTO).orElse(new VideoDTO());
    return !StringUtils.equals(video.getVideoId(), videoDTO.getVideoId()) || !StringUtils.equals(video.getFinalUrl(),
        videoDTO.getFinalUrl()) || !StringUtils.equals(video.getVideoName(), videoDTO.getVideoName())
        || !StringUtils.equals(video.getCoverImagePath(), videoDTO.getCoverImagePath()) || !StringUtils.equals(
        video.getSourceUrl(), videoDTO.getSourceUrl());
  }

  public static boolean isItemMasterDataDetailChanged(Item item, ProductDetailResponse productDetailResponse,
      ProductItemResponse productItemResponse) {
    String mainImage = Optional.ofNullable(productItemResponse).map(ProductItemResponse::getImages).orElse(new ArrayList<>()).stream()
        .filter(image -> !image.isMarkForDelete()).filter(CommonUtil::filterProcessedImages)
        .filter(image -> image.isMainImages()).map(Image::getLocationPath).findAny().orElse(StringUtils.EMPTY);
    return isDimensionUpdated(productDetailResponse, item) || !StringUtils.equals(
        productItemResponse.getGeneratedItemName(), item.getGeneratedItemName()) || !StringUtils.equals(
        String.valueOf(productItemResponse.getDangerousGoodsLevel()), String.valueOf(item.getDangerousLevel()))
        || !StringUtils.equals(mainImage, item.getMainImageUrl());
  }

  public static List<String> generateSellerChannelFromBusinessPartner(BusinessPartner businessPartner) {
    List<String> sellerChannel = new ArrayList<>();
    if (Objects.nonNull(businessPartner) && CollectionUtils.isNotEmpty(businessPartner.getSalesChannel())) {
      if (businessPartner.getSalesChannel().contains(Constants.B2C_SELLER_CHANNEL)) {
        sellerChannel.add(Constants.RETAIL);
      }
      if (businessPartner.getSalesChannel().contains(Constants.B2B_SELLER_CHANNEL)) {
        sellerChannel.add(Constants.B2B);
      }
    }
    return sellerChannel;
  }

  public static boolean isItemPriceChange(Item item, Set<com.gdn.x.product.model.entity.Price> prices) {
    return CollectionUtils.isNotEmpty(prices) && prices.stream().anyMatch(updatePrice -> item.getPrice().stream()
        .anyMatch(currentPrice -> CommonUtil.isPriceChanged(updatePrice, currentPrice)));
  }

  public static boolean isPriceChanged(com.gdn.x.product.model.entity.Price updatePrice,
      com.gdn.x.product.model.entity.Price currentPrice) {
    return currentPrice.getChannel().equals(updatePrice.getChannel()) && (
        currentPrice.getListPrice() != updatePrice.getListPrice()
            || currentPrice.getOfferPrice() != updatePrice.getOfferPrice());
  }

  public static boolean isEligibleForL5Update(boolean isPriceUpdated, boolean isItemDataUpdated,
      boolean wholeSalePriceActivatedFlagUpdated, boolean isCncFlagUpdated, boolean isB2bFieldsUpdated,
      boolean isItemPickupPintDataChanged, boolean buyableAndDiscoverableScheduleChanged) {
    return Stream.of(isPriceUpdated, isItemDataUpdated, wholeSalePriceActivatedFlagUpdated, isCncFlagUpdated,
            isB2bFieldsUpdated, isItemPickupPintDataChanged, buyableAndDiscoverableScheduleChanged)
        .anyMatch(Boolean::booleanValue);
  }

  public static boolean isValidItemPickupPointSummaryRequest(
    ItemPickupPointSummaryRequest request) {
    if (StringUtils.isNotEmpty(request.getMerchantCode()) || StringUtils.isNotEmpty(
      request.getItemSku()) || StringUtils.isNotEmpty(request.getKeyword())) {
      return true;
    }
    if (CollectionUtils.isNotEmpty(request.getItemCodes())) {
      request.setItemCodes(request.getItemCodes().stream().filter(StringUtils::isNotBlank)
        .collect(Collectors.toSet()));
      return CollectionUtils.isNotEmpty(request.getItemCodes());
    }
    if (CollectionUtils.isNotEmpty(request.getProductSkuList())) {
      request.setProductSkuList(request.getProductSkuList().stream().filter(StringUtils::isNotBlank)
        .collect(Collectors.toList()));
      return CollectionUtils.isNotEmpty(request.getProductSkuList());
    }
    if (CollectionUtils.isNotEmpty(request.getPickupPointCodes())) {
      request.setPickupPointCodes(
        request.getPickupPointCodes().stream().filter(StringUtils::isNotBlank)
          .collect(Collectors.toList()));
      return CollectionUtils.isNotEmpty(request.getPickupPointCodes());
    }
    if (CollectionUtils.isNotEmpty(request.getItemPickupPointCode())) {
      request.setItemPickupPointCode(
        request.getItemPickupPointCode().stream().filter(Objects::nonNull).filter(
            itemPickupPointRequest -> StringUtils.isNotBlank(itemPickupPointRequest.getItemSku()))
          .filter(itemPickupPointRequest -> StringUtils.isNotBlank(
            itemPickupPointRequest.getPickupPointCode())).collect(Collectors.toList()));
      return CollectionUtils.isNotEmpty(request.getItemPickupPointCode());
    }
    return false;
  }
  public static String getActivityBasedOnStatus(String currentCurationStatus, String requestCurationStatus) {
    if (CurationStatus.APPROVED.name().equals(currentCurationStatus) || CurationStatus.REJECTED.name()
        .equals(currentCurationStatus)) {
      return Constants.EDIT_HALAL_PRODUCT;
    } else if (CurationStatus.APPROVED.name().equals(requestCurationStatus)) {
      return Constants.APPROVE_HALAL_PRODUCT;
    } else {
      return Constants.REJECT_HALAL_PRODUCT;
    }
  }

  public static int getCurationStatus(Product product) {
    return Objects.nonNull(product.getCurationStatus()) ?
        product.getCurationStatus().getValue() :
        CurationStatus.NONE.getValue();
  }


  public static HalalHistoryUpdateEventModel getHalalHistoryUpdateEventModel(String curationStatus, Product product,
      HalalHistoryUpdateEventModel halalHistoryUpdateEventModel, String storeId, String userName) {
    halalHistoryUpdateEventModel = new HalalHistoryUpdateEventModel();
    halalHistoryUpdateEventModel.setStoreId(storeId);
    halalHistoryUpdateEventModel.setUserName(userName);
    halalHistoryUpdateEventModel.setProductSku(product.getProductSku());
    halalHistoryUpdateEventModel.setActivity(
        getActivityBasedOnStatus(product.getCurationStatus().name(), curationStatus));
    halalHistoryUpdateEventModel.setPreviousValue(
        String.valueOf(getHalalFlagBasedOnStatus(product.getCurationStatus().name())));
    halalHistoryUpdateEventModel.setCurrentValue(String.valueOf(getHalalFlagBasedOnStatus(curationStatus)));
    return halalHistoryUpdateEventModel;
  }

  public static boolean getHalalFlagBasedOnStatus(String curationStatus) {
    return CurationStatus.APPROVED.name().equals(curationStatus);
  }

  public static SalesCategoryUpdateRequest getSalesCategoryUpdateRequest(
      CategoryReferenceResponse categoryReferenceResponse) {
    SalesCategoryUpdateRequest salesCategoryUpdateRequest = null;
    if (Objects.nonNull(categoryReferenceResponse.getHalalSalesCategoryReference()) && Objects.nonNull(
        categoryReferenceResponse.getHalalSalesCategoryReference().getCatalog())) {
      salesCategoryUpdateRequest = new SalesCategoryUpdateRequest();
      salesCategoryUpdateRequest.setCatalogCode(
          categoryReferenceResponse.getHalalSalesCategoryReference().getCatalog().getCatalogCode());
      salesCategoryUpdateRequest.setCategoryCode(
          categoryReferenceResponse.getHalalSalesCategoryReference().getCategoryCode());
    }
    return salesCategoryUpdateRequest;
  }

  public static boolean updateCurationStatusOfProduct(Product product,
      ProductCategoryResponse productCategoryResponse) {
    boolean curationStatusUpdate = false;
    if (Objects.nonNull(productCategoryResponse.getCategory())) {
      boolean halalCategory = productCategoryResponse.getCategory().isHalalCategory();
      if (halalCategory) {
        if (Objects.isNull(product.getCurationStatus())
            || CurationStatus.NONE.equals(product.getCurationStatus())) {
          product.setCurationStatus(CurationStatus.NEED_CURATION);
          curationStatusUpdate = true;
        }
      } else if (!(Objects.isNull(product.getCurationStatus()) || CurationStatus.NONE.equals(product
          .getCurationStatus()))) {
        product.setCurationStatus(CurationStatus.NONE);
        curationStatusUpdate = true;
      }
    }
    return curationStatusUpdate;
  }

  public static ProductCategoryResponse masterCategoryFromProductDetailResponse(ProductDetailResponse
      productDetailResponse, String salesCategoryCatalogCode) {
    return Optional.ofNullable(productDetailResponse.getProductCategoryResponses())
            .orElse(new ArrayList<>()).stream().filter(productCategoryResponse -> (
                !salesCategoryCatalogCode.equals(productCategoryResponse.getCategory().getCatalog().getCatalogCode())
                    && StringUtils.isNotEmpty(productCategoryResponse.getCategory().getCatalog().getCatalogCode())))
            .findFirst().orElse(new ProductCategoryResponse());
  }


  public static void setUpdatedItemSkuAndItemMap(List<Item> updatedItemList, EditProductDetailDTO editProductDetailDTO) {
    Map<String, Item> updatedItemMap = updatedItemList.stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldValue, newValue) -> newValue));
    editProductDetailDTO.getUpdatedItemMap().putAll(updatedItemMap);
    editProductDetailDTO.getAllItemMap().putAll(updatedItemMap);
  }

  public static void setOfflineItemIdToItemPickupPointMap(List<ItemPickupPoint> updatedItemPickupPoints,
      EditProductDetailDTO editProductDetailDTO) {
    Map<String, ItemPickupPoint> updatedItemPickupPointMap = updatedItemPickupPoints.stream().collect(
        Collectors.toMap(ItemPickupPoint::getOfflineItemId, Function.identity(), (oldValue, newValue) -> newValue));
    editProductDetailDTO.getUpdatedItemPickupPointMap().putAll(updatedItemPickupPointMap);
    editProductDetailDTO.getAllItemPickupPointMap().putAll(updatedItemPickupPointMap);
  }

  public static Optional<ItemPickupPoint> getItemPickupPointFromDTO(EditProductDetailDTO editProductDetailDTO,
      String itemSku, String pickupPointCode) {
    StringBuilder offlineItemId = new StringBuilder(itemSku);
    offlineItemId.append(Constants.HYPHEN).append(pickupPointCode);
    return Optional.ofNullable(
        editProductDetailDTO.getAllItemPickupPointMap().getOrDefault(offlineItemId.toString(), null));
  }

  public static Item getItemFromDTO(String itemSku, EditProductDetailDTO editProductDetailDTO) {
    Item item = null;
    Item itemFromDTO = editProductDetailDTO.getAllItemMap().getOrDefault(itemSku, null);
    if (Objects.nonNull(itemFromDTO) && !itemFromDTO.isMarkForDelete()) {
      item = itemFromDTO;
    } else {
      log.error("Cannot find item with itemSku : {} , and mfd false ", itemSku);
      throw new ApiIncorrectInputDataException(ErrorMessages.NO_ITEM_FOUND, ErrorCategory.DATA_NOT_FOUND.getCode());
    }
    return item;
  }

  public static boolean isProductUpdatedForCombinedEdit(boolean isCombinedEditRequest,
      EditProductDetailDTO editProductDetailDTO , boolean productUpdated) {
    return isCombinedEditRequest && (editProductDetailDTO.isProductUpdated() || productUpdated);
  }

  public static List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> formInvRequestForPickupPointDelete(
    List<DeleteOfflineItemVO> result, String merchantCode) {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> dtoList = new ArrayList<>();
    for (DeleteOfflineItemVO deleteOfflineItemVO : result) {
      if (deleteOfflineItemVO.isSuccess()) {
        WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webItemSkuAndPickupPointCodeDTO =
          new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
        webItemSkuAndPickupPointCodeDTO.setWebItemSku(deleteOfflineItemVO.getItemSku());
        webItemSkuAndPickupPointCodeDTO.setPickupPointCode(
          deleteOfflineItemVO.getPickupPointCode());
        webItemSkuAndPickupPointCodeDTO.setWebMerchantCode(merchantCode);
        dtoList.add(webItemSkuAndPickupPointCodeDTO);
      }
    }
    return dtoList;
  }

  public static ProductChange populateProductType(Product product, ProductChange productChange) {
    if (Objects.nonNull(product.getProductType())) {
      if (product.getProductType().equals(ProductType.BIG_PRODUCT)) {
        productChange.setProductType(com.gdn.x.product.domain.event.enums.ProductType.BIG_PRODUCT);
      } else if (product.getProductType().equals(ProductType.BOPIS)) {
        productChange.setProductType(com.gdn.x.product.domain.event.enums.ProductType.BOPIS);
      } else {
        productChange.setProductType(com.gdn.x.product.domain.event.enums.ProductType.REGULAR);
      }
    }
    return productChange;
  }

  public static boolean isOnlyContentUpdated(boolean isCombinedEditRequest, EditProductDetailDTO editProductDetailDTO) {
    return isCombinedEditRequest && !EditChangeType.ITEM_PICKUP_POINT.equals(editProductDetailDTO.getEditChangeType());
  }

  public static boolean isItemUpdated(List<Item> updatedItemList, EditProductDetailDTO editProductDetailDTO) {
    return CollectionUtils.isNotEmpty(updatedItemList) || CollectionUtils.isNotEmpty(
        Optional.ofNullable(editProductDetailDTO).orElse(new EditProductDetailDTO()).getUpdatedItemMap().values());
  }

  public static List<ItemChangeEventType> removeNewlyAddedEventTypes(List<ItemChangeEventType> itemChangeEventTypes,
      String newlyAddedItemDataChangeEventTypes) {
    return Optional.ofNullable(itemChangeEventTypes).orElse(new ArrayList<>()).stream()
        .filter(type -> !newlyAddedItemDataChangeEventTypes.contains(type.name()))
        .collect(Collectors.toList());
  }

  public static List<ItemPickupPointChangeEventType> removeNewlyAddedEventTypesForL5DataChange(
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    String newlyAddedItemDataChangeEventTypes) {
    Set<String> newlyAddedEventTypes =
      Stream.of(newlyAddedItemDataChangeEventTypes).filter(Objects::nonNull)
        .flatMap(newEventTypes -> Arrays.stream(newEventTypes.split(Constants.COMMA)))
        .collect(Collectors.toSet());
    return
      Optional.ofNullable(itemPickupPointChangeEventTypes).map(list -> list.stream().filter(
        type -> Objects.nonNull(type) && !newlyAddedEventTypes.contains(type.name()))
      .collect(Collectors.toList())).orElse(Collections.emptyList());
  }

  public static boolean shouldCreateMasterSku(List<String> categoryCodesHierarchy,
      String categoriesToGenerateMasterSkuInFlow2ProductCreation) {
    return CollectionUtils.isNotEmpty(categoryCodesHierarchy) && StringUtils.isNotBlank(
        categoriesToGenerateMasterSkuInFlow2ProductCreation) && Arrays.stream(
            categoriesToGenerateMasterSkuInFlow2ProductCreation.split(Constants.COMMA_DELIMITER))
        .anyMatch(categoryCode -> categoryCodesHierarchy.contains(categoryCode));
  }

  public static String findMasterSkuFromSourceItems(List<Item> sourceItems) {
    return Optional.ofNullable(sourceItems).orElse(new ArrayList<>()).stream()
        .map(Item::getMasterSku).filter(masterSku -> StringUtils.isNotBlank(masterSku))
        .findFirst().orElse(StringUtils.EMPTY);
  }

  public static boolean isDefiningOrVariantCreating(MasterDataItemAttributeValue masterDataItemAttributeValue) {
    return MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
        Optional.ofNullable(masterDataItemAttributeValue).map(MasterDataItemAttributeValue::getMasterDataAttribute)
            .map(MasterDataAttribute::getAttributeType).orElse(null)) || Optional.ofNullable(
            masterDataItemAttributeValue).map(MasterDataItemAttributeValue::getMasterDataAttribute)
        .map(MasterDataAttribute::isVariantCreation).orElse(false);
  }

  public static boolean isMerchantPromoDiscountActive(ItemPickupPoint itemPickupPoint,
      boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull) {
    return Optional.ofNullable(itemPickupPoint.getPrice())
        .orElse(new HashSet<>())
        .stream()
        .anyMatch(price -> merchantPromoDiscountActiveGeneration(itemPickupPoint,
            ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull,
            price));
  }

  private static boolean merchantPromoDiscountActiveGeneration(ItemPickupPoint itemPickupPoint,
      boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull,
      com.gdn.x.product.model.entity.Price price) {
    if (ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull
        && Objects.isNull(price.getMerchantPromoDiscountPrice())) {
      return itemPickupPoint.isMerchantPromoDiscount();
    }
    return Objects.nonNull(price.getMerchantPromoDiscountPrice())
        && Objects.nonNull(price.getMerchantPromoDiscountPrice().getEndDateTime())
        && new Date().compareTo(price.getMerchantPromoDiscountPrice().getEndDateTime()) < 0;
  }

  public static boolean isMerchantPromoDiscountActivated(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.isMerchantPromoDiscount()
        && Optional.ofNullable(itemPickupPoint.getPrice())
        .orElse(new HashSet<>())
        .stream()
        .anyMatch(price -> Objects.nonNull(price.getMerchantPromoDiscountPrice()));
  }

  public static SolrInputDocument transformSolrFullReindexInputDocumentToSolrAtomicUpdateInputDocument(
      SolrInputDocument solrInputFields, Boolean productHasStock) {
    SolrInputDocument atomicUpdateDocument = new SolrInputDocument();

    for (Map.Entry<String, SolrInputField> solrInputFieldEntry : solrInputFields.entrySet()) {
      if (SolrFieldNames.PRODUCT_SKU.equals(solrInputFieldEntry.getKey())) {
        atomicUpdateDocument.setField(SolrFieldNames.PRODUCT_SKU, solrInputFieldEntry.getValue().getValue());
      } else if (SolrFieldNames.IS_IN_STOCK.equals(solrInputFieldEntry.getKey()) && Objects.nonNull(productHasStock)) {
        atomicUpdateDocument.setField(SolrFieldNames.IS_IN_STOCK,
            Collections.singletonMap(SolrConstants.SET_CLAUSE, solrInputFieldEntry.getValue().getValue()));
      } else if (!SolrFieldNames.IS_IN_STOCK.equals(solrInputFieldEntry.getKey())) {
        atomicUpdateDocument.setField(solrInputFieldEntry.getKey(),
            Collections.singletonMap(SolrConstants.SET_CLAUSE, solrInputFieldEntry.getValue().getValue()));
      }
    }

    return atomicUpdateDocument;
  }

  public static void updateFieldsForCategoryBrandOrNameChange(List<Item> items, boolean updateCategory,
      boolean nameUpdated, boolean brandUpdated) {
    List<UpdatedFields> updatedFields = new ArrayList<>();
    if (updateCategory) {
      updatedFields.add(UpdatedFields.CATEGORY_UPDATE);
    }
    if (nameUpdated) {
      updatedFields.add(UpdatedFields.NAME_UPDATE);
    }
    if (brandUpdated) {
      updatedFields.add(UpdatedFields.BRAND_UPDATE);
    }
    if (CollectionUtils.isNotEmpty(updatedFields)) {
      addUpdatedFieldsInItem(items, updatedFields);
    }
  }

  public static List<BusinessPartnerPickupPoint> filterBusinessPartnerPickupPointBasedOnMerchantCode(String merchantCode,
      List<BusinessPartnerPickupPoint> businessPartnerPickupPointList, boolean validateBusinessPartnerCodeForSecurity) {
    if (validateBusinessPartnerCodeForSecurity && StringUtils.isNotBlank(merchantCode)) {
      businessPartnerPickupPointList = businessPartnerPickupPointList.stream().filter(
              businessPartnerPickupPoint -> businessPartnerPickupPoint.getBusinessPartnerCode().equals(merchantCode))
          .collect(Collectors.toList());
      log.info("businessPartnerPickupPointList after filtering based on merchantCode {} are {} ", merchantCode,
          businessPartnerPickupPointList);
    }
    return businessPartnerPickupPointList;
  }

  public static void addUpdatedFieldsInItem(List<Item> items, List<UpdatedFields> updatedFields) {
    if(CollectionUtils.isNotEmpty(updatedFields)) {
      List<String> updatedFieldsList = updatedFields.stream().map(UpdatedFields::name).collect(
          Collectors.toList());
      items.forEach(item -> item.getUpdatedFields().addAll(updatedFieldsList));
    }
  }

  public static boolean getLateFulfillmentFromProductType(ProductType productType,
      boolean overrideLateFulfillmentByProductType, Boolean lateFulfillment) {
    if (overrideLateFulfillmentByProductType && Objects.nonNull(productType)) {
      return List.of(ProductType.BOPIS, ProductType.BIG_PRODUCT).contains(productType);
    } else {
      return BooleanUtils.toBooleanDefaultIfNull(lateFulfillment, false);
    }
  }

  public static Set<BundleRecipe> toBundleRecipes(ProductBundleCreationRequest productBundleCreationRequest) {
    Set<BundleRecipe> bundleRecipes = new HashSet<>();
    for (BundleRecipeVo bundleRecipeVo : productBundleCreationRequest.getBundleRecipeList()) {
      BundleRecipe bundleRecipe = new BundleRecipe(bundleRecipeVo.getItemSku(), bundleRecipeVo.getQuantity());
      bundleRecipes.add(bundleRecipe);
    }
    return bundleRecipes;
  }

  public static CreateUpdateBillOfMaterialRecipeCommandRequest toCreateUpdateBillOfMaterialRecipeRequest(Item parentItem,
      ProductBundleCreationRequest productBundleCreationRequest, Map<String, String> childItemSkuAndItemCodeMap) {
    CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest =
        new CreateUpdateBillOfMaterialRecipeCommandRequest(parentItem.getItemCode(),
          new ArrayList<>(), GdnMandatoryRequestParameterUtil.getUsername());
    for (BundleRecipeVo bundleRecipeVo : productBundleCreationRequest.getBundleRecipeList()) {
      if (childItemSkuAndItemCodeMap.containsKey(bundleRecipeVo.getItemSku())) {
        createUpdateBillOfMaterialRecipeRequest.getBillOfMaterialSetup().add(
            new CreateUpdateBillOfMaterialRecipeCommandRequest.CreateUpdateBillOfMaterialSetupCommandRequest(
                childItemSkuAndItemCodeMap.get(bundleRecipeVo.getItemSku()), bundleRecipeVo.getQuantity(), null));
      } else {
        log.error("Item not found for sku : {} ", bundleRecipeVo.getItemSku());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.ITEM_NOT_FOUND);
      }
    }
    return createUpdateBillOfMaterialRecipeRequest;
  }
  public static ProductType getProductType(String productTypeValue) {
    if (StringUtils.isEmpty(productTypeValue)) {
      return null;
    }
    try {
      return ProductType.valueOf(productTypeValue.trim().toUpperCase());
    } catch (Exception e) {
      return null;
    }
  }

  public static void validateItem(String itemSku, Item item) {
    if (Objects.isNull(item)) {
      log.error("Cannot find item with itemSku : {} , and mfd false ", itemSku);
      throw new ApiIncorrectInputDataException(ErrorMessages.NO_ITEM_FOUND, ErrorCategory.DATA_NOT_FOUND.getCode());
    }
  }

  public static boolean isBuyableAndDiscoverableScheduleChanged(
    boolean buyableAndDiscoverableScheduleChanged,
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo,
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    ItemPickupPoint itemPickupPoint, boolean schedulesAddEditEnabled) {

    List<ItemPickupPointChangeEventType> modifiedSchedules =
      getEventChangeTypesForModifiedSchedules(itemPickupPoint, itemPickupPointListingUpdateRequestVo,
        schedulesAddEditEnabled);

    if (CollectionUtils.isNotEmpty(modifiedSchedules)) {
      addChangeTypesForSchedulesEdit(itemPickupPointChangeEventTypes, modifiedSchedules);
      return true;
    }

    return buyableAndDiscoverableScheduleChanged;
  }


  public static boolean processRemovalOfSchedules(boolean buyableAndDiscoverableScheduleChanged,
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    ItemPickupPoint itemPickupPoint) {
    for (com.gdn.x.product.model.entity.ItemViewConfig config : itemPickupPoint.getItemViewConfig()) {
      config.setItemBuyableSchedules(null);
      config.setItemDiscoverableSchedules(null);
      addChangeTypesForSchedulesEdit(itemPickupPointChangeEventTypes,
        List.of(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE,
          ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE));
      buyableAndDiscoverableScheduleChanged = true;
    }
    return buyableAndDiscoverableScheduleChanged;
  }


  private static void addChangeTypesForSchedulesEdit(
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    List<ItemPickupPointChangeEventType> updatedL5ChangeTypeForSchedules) {
    for (ItemPickupPointChangeEventType eventType : updatedL5ChangeTypeForSchedules) {
      if (!itemPickupPointChangeEventTypes.contains(eventType)) {
        itemPickupPointChangeEventTypes.add(eventType);
      }
    }
  }


  private static List<ItemPickupPointChangeEventType> getEventChangeTypesForModifiedSchedules(
    ItemPickupPoint itemPickupPoint,
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo,
    boolean schedulesAddEditEnabled) {
    List<ItemPickupPointChangeEventType> buyableXDiscoverableScheduleChangedList =
      new ArrayList<>(BUYABLE_AND_DISCOVERABLE_CHANGE_TYPES);

    if (schedulesAddEditEnabled) {
      BuyableScheduleVo buyableScheduleVo =
        itemPickupPointListingUpdateRequestVo.getBuyableScheduleVo();
      DiscoverableScheduleVo discoverableScheduleVo =
        itemPickupPointListingUpdateRequestVo.getDiscoverableScheduleVo();

      // Handle null requests for buyable and discoverable schedules
      ItemBuyableSchedule existingBuyableSchedule = getExistingBuyableSchedule(itemPickupPoint);
      ItemDiscoverableSchedule existingDiscoverableSchedule = getExistingDiscoverableSchedule(itemPickupPoint);

      // Update existing schedules or create new ones for buyable
      processBuyableScheduleUpdate(itemPickupPoint, buyableScheduleVo, existingBuyableSchedule,
          buyableXDiscoverableScheduleChangedList, itemPickupPointListingUpdateRequestVo.isScheduleRemoval());

      // Update existing schedules or create new ones for discoverable
      processDiscoverableScheduleUpdate(itemPickupPoint, discoverableScheduleVo, existingDiscoverableSchedule,
          buyableXDiscoverableScheduleChangedList, itemPickupPointListingUpdateRequestVo.isScheduleRemoval());
    }

    return buyableXDiscoverableScheduleChangedList;
  }

  private static void processDiscoverableScheduleUpdate(ItemPickupPoint itemPickupPoint,
    DiscoverableScheduleVo discoverableScheduleRequest,
    ItemDiscoverableSchedule existingDiscoverableSchedule,
    List<ItemPickupPointChangeEventType> buyableXDiscoverableScheduleChangedList, boolean scheduleRemoval) {
    ItemDiscoverableSchedule itemDiscoverableSchedule;
    if (Objects.nonNull(discoverableScheduleRequest) && Objects.isNull(existingDiscoverableSchedule)) {
      itemDiscoverableSchedule = new ItemDiscoverableSchedule();
      BeanUtils.copyProperties(discoverableScheduleRequest, itemDiscoverableSchedule);
      existingDiscoverableSchedule = new ItemDiscoverableSchedule();
      updateDiscoverableSchedule(existingDiscoverableSchedule, itemDiscoverableSchedule,
        itemPickupPoint, buyableXDiscoverableScheduleChangedList);
    } else if (isDisplayableScheduleModified(existingDiscoverableSchedule,
      discoverableScheduleRequest)) {
      itemDiscoverableSchedule = new ItemDiscoverableSchedule();
      BeanUtils.copyProperties(discoverableScheduleRequest, itemDiscoverableSchedule);
      updateDiscoverableSchedule(existingDiscoverableSchedule, itemDiscoverableSchedule,
        itemPickupPoint, buyableXDiscoverableScheduleChangedList);
    }
    else if(scheduleRemoval && Objects.isNull(discoverableScheduleRequest) && Objects.nonNull(existingDiscoverableSchedule)){
      setDiscoverableScheduleInItemPickupPoint(itemPickupPoint, null);
      buyableXDiscoverableScheduleChangedList.add(
        ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
    }
  }

  private static void processBuyableScheduleUpdate(ItemPickupPoint itemPickupPoint,
    BuyableScheduleVo buyableScheduleVo, ItemBuyableSchedule existingBuyableSchedule,
    List<ItemPickupPointChangeEventType> buyableXDiscoverableScheduleChangedList, boolean scheduleRemoval) {
    ItemBuyableSchedule itemBuyableSchedule;
    // if schedules is added for first time, initialize existing schedule and populate from request
    if (Objects.nonNull(buyableScheduleVo) && Objects.isNull(existingBuyableSchedule)) {
      itemBuyableSchedule = new ItemBuyableSchedule();
      BeanUtils.copyProperties(buyableScheduleVo, itemBuyableSchedule);
      existingBuyableSchedule = new ItemBuyableSchedule();
      updateBuyableSchedule(existingBuyableSchedule, itemBuyableSchedule, itemPickupPoint);
      log.info("New Buyable schedules added to L5 : {} ", itemPickupPoint.getOfflineItemId());
      buyableXDiscoverableScheduleChangedList.add(
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    }
    //if schedules are changed populate same at L5
    else if (isBuyableScheduleModified(existingBuyableSchedule, buyableScheduleVo)) {
      itemBuyableSchedule = new ItemBuyableSchedule();
      BeanUtils.copyProperties(buyableScheduleVo, itemBuyableSchedule);
      updateBuyableSchedule(existingBuyableSchedule, itemBuyableSchedule, itemPickupPoint);
      buyableXDiscoverableScheduleChangedList.add(
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    }
    else if (Objects.isNull(buyableScheduleVo)) {
      processRemovalOfSchedules(itemPickupPoint, existingBuyableSchedule, buyableXDiscoverableScheduleChangedList,
        scheduleRemoval);
    }
  }

  private static void processRemovalOfSchedules(ItemPickupPoint itemPickupPoint,
    ItemBuyableSchedule existingBuyableSchedule,
    List<ItemPickupPointChangeEventType> buyableXDiscoverableScheduleChangedList,
    boolean scheduleRemoval) {
    if (scheduleRemoval && Objects.nonNull(existingBuyableSchedule)) {
      setBuyableSchedulesInItemPickupPoint(null, itemPickupPoint);
      buyableXDiscoverableScheduleChangedList.add(
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    }
  }

  private static boolean isBuyableScheduleModified(ItemBuyableSchedule existingBuyableSchedule,
    BuyableScheduleVo buyableScheduleRequest) {
    // saved and request schedules are not null and not same
    return Objects.nonNull(existingBuyableSchedule) && Objects.nonNull(buyableScheduleRequest) && (
      !buyableScheduleRequest.getEndDateTime().equals(existingBuyableSchedule.getEndDateTime())
        || !buyableScheduleRequest.getStartDateTime()
        .equals(existingBuyableSchedule.getStartDateTime()));
  }

  private static boolean isDisplayableScheduleModified(
    ItemDiscoverableSchedule existingDiscoverableSchedule,
    DiscoverableScheduleVo discoverableScheduleRequest) {
    // saved and request schedules are not null and not same
    return Objects.nonNull(existingDiscoverableSchedule) && Objects.nonNull(
      discoverableScheduleRequest) && (!discoverableScheduleRequest.getEndDateTime()
      .equals(existingDiscoverableSchedule.getEndDateTime())
      || !discoverableScheduleRequest.getStartDateTime()
      .equals(existingDiscoverableSchedule.getStartDateTime()));
  }

  // retrieve existing schedules
  private static ItemBuyableSchedule getExistingBuyableSchedule(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getItemViewConfig().stream()
      .filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
      .map(com.gdn.x.product.model.entity.ItemViewConfig::getItemBuyableSchedules).orElse(null);
  }

  private static ItemDiscoverableSchedule getExistingDiscoverableSchedule(
    ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getItemViewConfig().stream()
      .filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
      .map(com.gdn.x.product.model.entity.ItemViewConfig::getItemDiscoverableSchedules)
      .orElse(null);
  }

  // update buyable schedule
  private static void updateBuyableSchedule(ItemBuyableSchedule existingSchedule,
    ItemBuyableSchedule requestSchedule, ItemPickupPoint itemPickupPoint) {
    // always consider request as source of truth
    existingSchedule.setStartDateTime(requestSchedule.getStartDateTime());
    existingSchedule.setEndDateTime(requestSchedule.getEndDateTime());
    existingSchedule.setBuyable(requestSchedule.isBuyable());
    setBuyableSchedulesInItemPickupPoint(existingSchedule, itemPickupPoint);
  }

  private static void setBuyableSchedulesInItemPickupPoint(ItemBuyableSchedule existingSchedule,
    ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.getItemViewConfig().stream()
      .filter(itemViewConfig -> Constants.DEFAULT_CHANNEL.equals(itemViewConfig.getChannel()))
      .findFirst()
      .ifPresent(itemViewConfig -> itemViewConfig.setItemBuyableSchedules(existingSchedule));
  }

  // update discoverable schedule
  private static void updateDiscoverableSchedule(ItemDiscoverableSchedule existingSchedule,
    ItemDiscoverableSchedule requestSchedule, ItemPickupPoint itemPickupPoint,
    List<ItemPickupPointChangeEventType> buyableXDiscoverableScheduleChangedList) {
    existingSchedule.setStartDateTime(requestSchedule.getStartDateTime());
    existingSchedule.setEndDateTime(requestSchedule.getEndDateTime());
    existingSchedule.setDiscoverable(requestSchedule.isDiscoverable());
    setDiscoverableScheduleInItemPickupPoint(itemPickupPoint, existingSchedule);
    buyableXDiscoverableScheduleChangedList.add(
      ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
  }

  private static void setDiscoverableScheduleInItemPickupPoint(ItemPickupPoint itemPickupPoint,
    ItemDiscoverableSchedule itemDiscoverableSchedule) {
    itemPickupPoint.getItemViewConfig().stream()
      .filter(itemViewConfig -> Constants.DEFAULT_CHANNEL.equals(itemViewConfig.getChannel()))
      .findFirst().ifPresent(itemViewConfig -> itemViewConfig.setItemDiscoverableSchedules(itemDiscoverableSchedule));
  }


  public static void setSchedulesForAddPickupPoint(boolean schedulesAddEditEnabled,
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo,
    ItemPickupPoint itemPickupPoint) {
    if (schedulesAddEditEnabled) {
      //validations done in PBP , add schedules to newly added L5
      if (Objects.nonNull(itemPickupPointListingUpdateRequestVo.getDiscoverableScheduleVo())) {
        ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
        BeanUtils.copyProperties(itemPickupPointListingUpdateRequestVo.getDiscoverableScheduleVo(),
          itemDiscoverableSchedule);
        setDiscoverableScheduleInItemPickupPoint(itemPickupPoint, itemDiscoverableSchedule);
      }
      if (Objects.nonNull(itemPickupPointListingUpdateRequestVo.getBuyableScheduleVo())) {
        ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
        BeanUtils.copyProperties(itemPickupPointListingUpdateRequestVo.getBuyableScheduleVo(),
          itemBuyableSchedule);
        setBuyableSchedulesInItemPickupPoint(itemBuyableSchedule, itemPickupPoint);
      }
    }
  }

  public static void clearSchedule(List<AuditTrailDto> auditTrailDtoList, Item item,
      ItemPickupPoint itemPickupPoint, List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList) {
      clearSchedules(auditTrailDtoList, item, itemPickupPoint);
    if (itemPickupPoint.getItemPickupPointDataChangeType()
        .contains(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName())) {
      itemPickupPointChangeEventTypeList.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    }
    if (itemPickupPoint.getItemPickupPointDataChangeType()
        .contains(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE.getName())) {
      itemPickupPointChangeEventTypeList.add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
    }
  }

  public static void clearSchedules(List<AuditTrailDto> auditTrailDtoList,
      Item item, ItemPickupPoint itemPickupPoint) {
    if (Objects.isNull(itemPickupPoint.getItemPickupPointDataChangeType())) {
      itemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>());
    }
    for (com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig : Optional.ofNullable(
        itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>())) {
      if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
        clearBuyableSchedules(auditTrailDtoList, item, itemPickupPoint, itemViewConfig);
      }
      if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
        clearDiscoverableSchedules(auditTrailDtoList, item, itemPickupPoint, itemViewConfig);
      }
    }
  }

  private static void clearBuyableSchedules(List<AuditTrailDto> auditTrailDtoList, Item item,
      ItemPickupPoint itemPickupPoint, com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    String oldBuyableScheduleValue = getScheduleHistory(itemViewConfig.getItemBuyableSchedules().getStartDateTime(),
        itemViewConfig.getItemBuyableSchedules().getEndDateTime());
    itemViewConfig.setItemBuyableSchedules(null);
    itemPickupPoint.getItemPickupPointDataChangeType()
        .add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName());
    auditTrailDtoList.add(convertToAuditTrailDto(oldBuyableScheduleValue, Constants.HYPHEN,
        Constants.BUYABLE_SCHEDULE_ACTION, item, itemPickupPoint));
  }

  private static void clearDiscoverableSchedules(List<AuditTrailDto> auditTrailDtoList, Item item,
      ItemPickupPoint itemPickupPoint, com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    String oldDiscoverableScheduleValue = getScheduleHistory(itemViewConfig.getItemDiscoverableSchedules()
            .getStartDateTime(), itemViewConfig.getItemDiscoverableSchedules().getEndDateTime());
    itemViewConfig.setItemDiscoverableSchedules(null);
    itemPickupPoint.getItemPickupPointDataChangeType()
        .add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE.getName());
    auditTrailDtoList.add(convertToAuditTrailDto(oldDiscoverableScheduleValue, Constants.HYPHEN,
        Constants.DISCOVERABLE_SCHEDULE_ACTION, item, itemPickupPoint));
  }

  private static AuditTrailDto convertToAuditTrailDto(String oldValue, String newValue,
      String action, Item item, ItemPickupPoint itemPickupPoint) {
    return getAuditTrailDto(oldValue, newValue, action, itemPickupPoint.getMerchantCode(),
        itemPickupPoint.getProductSku(), itemPickupPoint.getItemSku(),
        item.getGeneratedItemName(), itemPickupPoint.getPickupPointCode());
  }

  private static String getScheduleHistory(Date startDate, Date endDate) {
    return startDate + Constants.SPACE + Constants.HYPHEN + Constants.SPACE + endDate;
  }

  public static boolean isScheduleCompleted(Date endDate) {
    return Objects.isNull(endDate) || endDate.getTime() < new Date().getTime();
  }

  public static void clearSchedules(ItemPickupPoint itemPickupPoint,
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes) {
    for (com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig : Optional.ofNullable(
      itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>())) {
      if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
        itemViewConfig.setItemBuyableSchedules(null);
        itemPickupPointChangeEventTypes
          .add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
      }
      if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
        itemViewConfig.setItemDiscoverableSchedules(null);
        itemPickupPointChangeEventTypes
          .add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
      }
    }
  }


  public static ProductSkuSizeChartResponse toProductSkuSizeChartCode(SolrDocument solrDocument) {
    ProductSkuSizeChartResponse productSkuSizeChartResponse = new ProductSkuSizeChartResponse();
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU))) {
      productSkuSizeChartResponse.setProductSku((String) solrDocument.getFieldValue(SolrFieldNames.PRODUCT_SKU));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.SIZE_CHART_CODE))) {
      productSkuSizeChartResponse.setSizeChartCode((String) solrDocument.getFieldValue(SolrFieldNames.SIZE_CHART_CODE));
    }
    return productSkuSizeChartResponse;
  }

  public static boolean isProductUpdated(boolean isDimensionsMissingChanged, boolean isProductTypeChanged,
      boolean isCncAtL3Changed) {
    return isDimensionsMissingChanged || isProductTypeChanged || isCncAtL3Changed;
  }

  public static boolean updateCncAtL5(ProductAndL5MigrationRequest request, ItemPickupPoint itemPickupPoint) {
    if (Objects.nonNull(request.getCncActiveAtL5()) && !request.getCncActiveAtL5()
        .equals(itemPickupPoint.isCncActive())) {
      itemPickupPoint.setCncActive(request.getCncActiveAtL5());
      return true;
    }
    return false;
  }

  public static boolean checkAndUpdateStatusInItemViewConfig(ProductAndL5MigrationRequest request,
      boolean statusUpdated, Set<String> itemPickupPointDataChangeType,
      com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig) {
    if (Objects.nonNull(itemViewConfig)) {
      Date currentDate = new Date();
      boolean isDiscoverableChanged = request.isDiscoverable() != itemViewConfig.isDiscoverable();
      boolean isBuyableChanged = request.isBuyable() != itemViewConfig.isBuyable();
      if (isBuyableChanged || isDiscoverableChanged) {
        itemViewConfig.setBuyable(request.isBuyable());
        if (isDiscoverableChanged) {
          itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName());
        }
        itemViewConfig.setDiscoverable(request.isDiscoverable());
        if (hasActiveDiscoverableSchedule(itemViewConfig.getItemDiscoverableSchedules(), currentDate)) {
          itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE.getName());
        }
        itemViewConfig.setItemDiscoverableSchedules(null);
        if (hasActiveBuyableSchedule(itemViewConfig.getItemBuyableSchedules(), currentDate)) {
          itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName());
        }
        itemViewConfig.setItemBuyableSchedules(null);
        statusUpdated = true;
      }
    }
    return statusUpdated;
  }

  public static boolean hasActiveBuyableSchedule(ItemBuyableSchedule itemBuyableSchedule, Date currentDate) {
    return Optional.ofNullable(itemBuyableSchedule).map(ItemBuyableSchedule::getEndDateTime)
        .map(endDateTime -> endDateTime.after(currentDate)).orElse(false);
  }

  public static boolean hasActiveDiscoverableSchedule(ItemDiscoverableSchedule itemDiscoverableSchedule,
      Date currentDate) {
    return Optional.ofNullable(itemDiscoverableSchedule).map(ItemDiscoverableSchedule::getEndDateTime)
        .map(endDateTime -> endDateTime.after(currentDate)).orElse(false);
  }


  public static boolean checkAndSetDimensionsMissing(Product product, ProductAndL5MigrationRequest request) {
    if (Objects.nonNull(request.getDimensionsMissing()) && !request.getDimensionsMissing()
        .equals(product.getDimensionsMissing())) {
      product.setDimensionsMissing(request.getDimensionsMissing());
      return true;
    }
    return false;
  }

  public static boolean checkAndSetProductType(Product product, ProductAndL5MigrationRequest request) {
    if (Objects.nonNull(request.getProductType()) && !request.getProductType().equals(product.getProductType())) {
      product.setProductType(request.getProductType());
      return true;
    }
    return false;
  }

  public static boolean checkAndSetCncActivatedAtL3(Product product, ProductAndL5MigrationRequest request) {
    if (Objects.nonNull(request.getCncActiveAtL3()) && !request.getCncActiveAtL3().equals(product.isCncActivated())) {
      product.setCncActivated(request.getCncActiveAtL3());
      return true;
    }
    return false;
  }

  public static boolean checkAndSetLateFulfillment(List<Item> items, ProductAndL5MigrationRequest request) {
    boolean isLateFulfillmentChanged = false;
    if (Objects.nonNull(request.getProductType()) && ProductType.REGULAR.equals(request.getProductType())) {
      isLateFulfillmentChanged = true;
      for (Item item : items) {
        item.setLateFulfillment(false);
      }
    }
    return isLateFulfillmentChanged;
  }

  public static boolean validateForBopisCategoryRestriction(ProfileResponse profileResponse,
    String merchantTypesForBopisCategoryValidation, boolean bopisNotEligibleForCategory) {
    List<String> merchantsTypesValidation =
      Arrays.asList(merchantTypesForBopisCategoryValidation.split(","));
    String merchantType = Optional.ofNullable(profileResponse.getCompany()).orElse(new CompanyDTO()).getMerchantType();

    return merchantsTypesValidation.contains(merchantType) && bopisNotEligibleForCategory;
  }

  public static boolean fetchDimensionsMissingForBopisProduct(Item item, ProfileResponse profileResponse,
    String merchantTypesForBopisCategoryValidation, boolean bopisNotEligibleForCategory){
    boolean bopisCategoryRestriction =
      validateForBopisCategoryRestriction(profileResponse, merchantTypesForBopisCategoryValidation,
        bopisNotEligibleForCategory);
    return isAllDimensionsAreZero(item) && bopisCategoryRestriction;
  }

  public static boolean isAllDimensionsAreZero(Item item) {
    return Optional.ofNullable(item).map(
      i -> Stream.of(i.getHeight(), i.getLength(), i.getWidth(), i.getWeight())
        .allMatch(dimension -> Double.compare(dimension, ZERO_DIMENSIONS) == 0)).orElse(false);
  }

  public static ProductAndL5MigrationRequest getProductAndL5MigrationRequest(Product product) {
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    productAndL5MigrationRequest.setDimensionsMissing(true);
    productAndL5MigrationRequest.setProductSku(product.getProductSku());
    productAndL5MigrationRequest.setDiscoverable(false);
    productAndL5MigrationRequest.setBuyable(false);
    productAndL5MigrationRequest.setL5Updated(true);
    productAndL5MigrationRequest.setProductType(ProductType.REGULAR);
    return productAndL5MigrationRequest;
  }

  public static EanUpcPickupPointCodeResponse toEanUpcPickupPointCodeResponse(
      ItemPickupPoint itemPickupPoint, Map<String, String> itemSkuItemNameMap) {
    return EanUpcPickupPointCodeResponse.builder().itemSku(itemPickupPoint.getItemSku())
        .productSku(itemPickupPoint.getProductSku())
        .pickupPointCode(itemPickupPoint.getPickupPointCode())
        .businessPartnerCode(itemPickupPoint.getMerchantCode())
        .itemName(itemSkuItemNameMap.get(itemPickupPoint.getItemSku())).build();
  }

  public static void updateMissingNonMandatoryFieldsForProductActivation(ProductDetailResponse productDetailResponse,
      Product product) {
      Set<String> missingFields = new HashSet<>(5);
      ProductType productType =
        Optional.ofNullable(product).map(Product::getProductType).orElse(ProductType.REGULAR);
    productDetailResponse = Optional.ofNullable(productDetailResponse).orElse(new ProductDetailResponse());
    if (!ProductType.BOPIS.equals(productType) && hasZeroDimension(
        Optional.ofNullable(productDetailResponse.getLength()).orElse(Constants.DOUBLE_ZERO),
        Optional.ofNullable(productDetailResponse.getWeight()).orElse(Constants.DOUBLE_ZERO),
        Optional.ofNullable(productDetailResponse.getWidth()).orElse(Constants.DOUBLE_ZERO),
        Optional.ofNullable(productDetailResponse.getShippingWeight()).orElse(Constants.DOUBLE_ZERO))) {
      missingFields.add(Constants.DIMENSIONS_MISSING);
    }
      String descriptionSaved =
        Optional.of(productDetailResponse).map(ProductDetailResponse::getDescription)
          .map(String::new)
          .orElse(StringUtils.EMPTY);
      if(StringUtils.isBlank(descriptionSaved)){
        missingFields.add(Constants.DESCRIPTION_MISSING);
      }
    if (Objects.nonNull(product)) {
      product.setMissingFields(missingFields);
    }
  }

  private static boolean hasZeroDimension(double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension == ZERO_DIMENSIONS) {
        return true;
      }
    }
    return false;
  }
  public static boolean isNotCncActivatedItem(Item item) {
    return Objects.nonNull(item) && !item.isCncActivated();
  }

  public static boolean isL5StatusIsOnline(ItemPickupPointListingUpdateRequestVo requestVo, boolean L5StatusIsOnline) {
    if (CollectionUtils.isNotEmpty(requestVo.getItemViewConfigs())) {
      for (com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig : requestVo.getItemViewConfigs()) {
        if (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) {
          L5StatusIsOnline = true;
          break;
        }
      }
    }
    return L5StatusIsOnline;
  }


  public static void publishProductFlagChangeHistoryEvent(Product product, boolean isFlagChanged, boolean oldValue,
      boolean newValue, String actionKey, Collection<AuditTrailDto> auditTrailDtoList) {
    if (isFlagChanged) {
      AuditTrailDto auditTrailDto =
          new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, actionKey, String.valueOf(oldValue),
              String.valueOf(newValue), null, product.getProductSku(), product.getProductName());
      auditTrailDtoList.add(auditTrailDto);
    }
  }

  public static void setBrandAndCategoryCodeAndPreOrderForOdoo(Product product, ProductChange productChange) {
    productChange.setBrand(product.getBrand());
    productChange.setCategoryCode(product.getCategoryCode());
    productChange.setPreOrder(
        Optional.ofNullable(Optional.ofNullable(product.getPreOrder()).orElse(new PreOrder()).getIsPreOrder())
            .orElse(false));
  }

  public static Video convertVideoDTOToVideo(VideoDTO videoDTO) {
    if(Objects.isNull(videoDTO)) {
      return null;
    }
    Video video = new Video();
    BeanUtils.copyProperties(videoDTO, video);
    return video;
  }

  public static AuditTrailListResponse getAuditTrailListResponse(String newValue, String oldValue,
      Product product, String activity, String updatedBy) {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setNewValue(newValue);
    auditTrailDto.setOldValue(oldValue);
    auditTrailDto.setProductSku(product.getProductSku());
    auditTrailDto.setActionKey(activity);
    auditTrailDto.setPickupPointCode(Constants.HYPHEN);
    auditTrailDto.setName(product.getProductName());
    auditTrailDto.setBusinessPartnerCode(product.getMerchantCode());

    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    auditTrailListResponse.setAuditTrailResponseList(Collections.singletonList(auditTrailDto));
    auditTrailListResponse.setChangedBy(updatedBy);

    return auditTrailListResponse;
  }

  public static ProductBasicInfoResponse getProductBasicInfoResponse(Product product,
      BasicInfoProductResponse basicInfoProductResponse) {
    ProductBasicInfoResponse productBasicInfoResponse = new ProductBasicInfoResponse();
    productBasicInfoResponse.setProductSku(product.getProductSku());
    productBasicInfoResponse.setProductName(basicInfoProductResponse.getProductName());
    productBasicInfoResponse.setInstore(product.isOff2OnChannelActive() ? 1 : 0);
    productBasicInfoResponse.setBrand(basicInfoProductResponse.getBrand());
    productBasicInfoResponse.setCategoryCode(product.getCategoryCode());
    productBasicInfoResponse.setDescription(basicInfoProductResponse.getDescription());
    List<ImageBasicInfoResponse> imageBasicInfoResponsesList= new ArrayList<>();
    for (Image image : basicInfoProductResponse.getCommonImageList()) {
      ImageBasicInfoResponse imageBasicInfoResponse = new ImageBasicInfoResponse();
      imageBasicInfoResponse.setMainImage(image.isMainImages());
      imageBasicInfoResponse.setLocationPath(image.getLocationPath());
      imageBasicInfoResponsesList.add(imageBasicInfoResponse);
    }
    productBasicInfoResponse.setCommonImageList(imageBasicInfoResponsesList);
    if (StringUtils.isNotBlank(product.getUrl())) {
      productBasicInfoResponse.setVideoUrl(product.getUrl());
    } else {
      productBasicInfoResponse.setVideoUrl(Optional.ofNullable(product.getVideo())
          .map(video -> StringUtils.defaultIfEmpty(video.getFinalUrl(), video.getSourceUrl()))
          .orElse(StringUtils.EMPTY));
    }
    if (Objects.nonNull(product.getProductType())) {
      if (product.getProductType().getCode() == 1) {
        productBasicInfoResponse.setShippingType(Constants.REGULAR);
      } else if (product.getProductType().getCode() == 2) {
        productBasicInfoResponse.setShippingType(Constants.BIG_PRODUCT);
      } else {
        productBasicInfoResponse.setShippingType(Constants.BOPIS);
      }
    }
    productBasicInfoResponse.setLength(basicInfoProductResponse.getLength());
    productBasicInfoResponse.setHeight(basicInfoProductResponse.getHeight());
    productBasicInfoResponse.setWeight(basicInfoProductResponse.getWeight());
    productBasicInfoResponse.setWidth(basicInfoProductResponse.getWidth());
    productBasicInfoResponse.setShippingWeight(basicInfoProductResponse.getShippingWeight());
    productBasicInfoResponse.setSizeChartCode(product.getSizeChartCode());
    return productBasicInfoResponse;
  }

  public static void setRejectedChangeTypeForItemDataChangeEvent(ItemDataChange itemDataChange,
    boolean productRejected) {
    if (productRejected) {
      Optional.ofNullable(itemDataChange.getItemChangeEventTypesV2())
        .ifPresentOrElse(list -> list.add(ProductChangeEventType.PRODUCT_REJECTED), () -> {
          List<String> changeTypeList = new ArrayList<>();
          changeTypeList.add(ProductChangeEventType.PRODUCT_REJECTED);
          itemDataChange.setItemChangeEventTypesV2(changeTypeList);
        });
    }
  }

  public static void setRejectedChangeTypeForL5DataChangeEventModelList(
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
    boolean productRejected) {
    if (productRejected) {
      for (ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel : itemPickupPointDataChangeEventModelList) {
        Optional.ofNullable(
            itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypesV2())
          .ifPresentOrElse(list -> list.add(ProductChangeEventType.PRODUCT_REJECTED), () -> {
            List<String> changeTypeList = new ArrayList<>();
            changeTypeList.add(ProductChangeEventType.PRODUCT_REJECTED);
            itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypesV2(changeTypeList);
          });
      }
    }
  }


  public static void setSharedProduct(List<Product> products) {
    Map<String, Long> productCodeCounts = products.stream()
      .collect(Collectors.groupingBy(Product::getProductCode, Collectors.counting()));

    products.forEach(product -> product.setSharedProduct(
      productCodeCounts.getOrDefault(product.getProductCode(), 0L) > 1));
  }


  public static boolean isCncActivatedFalse(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> itemViewConfig.getChannel().equals(Constants.CNC))
        .noneMatch(itemViewConfig -> itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable());
  }

  public static void computeDistributionFlag(List<ItemVo> itemVos, Set<String> distributionSellerList,
      String merchantCode, Product product) {
    boolean allDistribution = Optional.ofNullable(itemVos).orElse(new ArrayList<>()).stream()
        .map(ItemVo::getItemPickupPointVoList).filter(CollectionUtils::isNotEmpty)
        .flatMap(List::stream).allMatch(ItemPickupPoint::isDistribution);

    boolean anyDistribution = Optional.ofNullable(itemVos).orElse(new ArrayList<>()).stream()
        .map(ItemVo::getItemPickupPointVoList).filter(CollectionUtils::isNotEmpty)
        .flatMap(List::stream).anyMatch(ItemPickupPoint::isDistribution);
    if (distributionSellerList.contains(merchantCode)) {
      setDistributionStatusBasedOnL5Flags(product, allDistribution, anyDistribution);
    } else {
      product.setDistributionStatus(DistributionStatus.NON_DISTRIBUTION);
    }
  }

  public static boolean distributionSellerAndAddOrDeletePerformed(
      Set<String> distributionSellerList,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      String merchantCode) {
    return distributionSellerList.contains(merchantCode) && Optional.ofNullable(
        itemPickupPointUpdateRequestVo).map(
        vo -> CollectionUtils.isNotEmpty(vo.getAddPickupPointRequests())
            || CollectionUtils.isNotEmpty(vo.getDeletePickupPointRequests()) || Optional.ofNullable(
                vo.getAddDeleteVariantRequestVo()).map(addDeleteVariantRequestVo ->
                CollectionUtils.isNotEmpty(addDeleteVariantRequestVo.getAddVariantsList())
                    || CollectionUtils.isNotEmpty(addDeleteVariantRequestVo.getDeleteVariantsList()))
            .orElse(false)).orElse(false);
  }

  public static void setDistributionStatus(Product product, boolean addOrDeletePPPerformed,
      List<ItemPickupPoint> allItemPickupPoints, Set<String> distributionSellerList) {
    boolean allAreDistribution;
    boolean anyIsDistribution;
    if (Optional.ofNullable(distributionSellerList).orElse(new HashSet<>()).contains(product.getMerchantCode())) {
      if (addOrDeletePPPerformed) {
        allAreDistribution = allItemPickupPoints.stream().allMatch(ItemPickupPoint::isDistribution);

        anyIsDistribution = allItemPickupPoints.stream().anyMatch(ItemPickupPoint::isDistribution);

        CommonUtil.setDistributionStatusBasedOnL5Flags(product, allAreDistribution, anyIsDistribution);
      }
    } else {
      product.setDistributionStatus(DistributionStatus.NON_DISTRIBUTION);
    }
  }

  public static void setDistributionStatusBasedOnL5Flags(Product product,
      boolean allAreDistribution, boolean anyIsDistribution) {
    if (allAreDistribution) {
      product.setDistributionStatus(DistributionStatus.PURE_DISTRIBUTION);
    } else if (anyIsDistribution) {
      product.setDistributionStatus(DistributionStatus.DISTRIBUTION);
    } else {
      product.setDistributionStatus(DistributionStatus.NON_DISTRIBUTION);
    }
  }

  public static List<AuditTrailDto> getHistoryListForOmniChannelSkuUpdate(Product product,
      Map<String, String> existingItemCodeAndOmniChannelSkuMap,
      Map<String, String> itemCodeAndOmniChannelSkuMap, List<Item> items,
      List<AuditTrailDto> auditTrailDtoList) {
    Map<String, String> itemCodeAndItemName = items.stream().collect(
        Collectors.toMap(Item::getItemCode, Item::getGeneratedItemName,
            (oldValue, newValue) -> newValue));
    Map<String, String> itemCodeAndItemSku = items.stream().collect(
        Collectors.toMap(Item::getItemCode, Item::getItemSku, (oldValue, newValue) -> newValue));

    MapDifference<String, String> omniChannelSkuMapDifference =
        Maps.difference(existingItemCodeAndOmniChannelSkuMap, itemCodeAndOmniChannelSkuMap);

    Map<String, String> newOmniChannelSkus = omniChannelSkuMapDifference.entriesOnlyOnRight();

    Map<String, MapDifference.ValueDifference<String>> updatedOmniChannelSkus =
        omniChannelSkuMapDifference.entriesDiffering();

    for (Map.Entry<String, MapDifference.ValueDifference<String>> entry :
        updatedOmniChannelSkus.entrySet()) {
      String itemCode = entry.getKey();
      String oldValue = entry.getValue().leftValue();
      String newValue = entry.getValue().rightValue();
      AuditTrailDto auditTrailDto = getBasicAuditTrailDTO(product, itemCodeAndItemName, itemCode);
      auditTrailDto.setGdnSku(itemCodeAndItemSku.get(itemCode));
      auditTrailDto.setOldValue(oldValue);
      auditTrailDto.setNewValue(newValue);
      auditTrailDtoList.add(auditTrailDto);
    }

    for (Map.Entry<String, String> entry : newOmniChannelSkus.entrySet()) {
      String itemCode = entry.getKey();
      String newSku = entry.getValue();  // newly added
      AuditTrailDto auditTrailDto = getBasicAuditTrailDTO(product, itemCodeAndItemName, itemCode);
      auditTrailDto.setOldValue(Constants.HYPHEN);
      auditTrailDto.setNewValue(newSku);
      auditTrailDtoList.add(auditTrailDto);
    }
    return auditTrailDtoList;
  }

  private static AuditTrailDto getBasicAuditTrailDTO(Product product,
      Map<String, String> itemCodeAndItemName, String itemCode) {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setActionKey(Constants.OMNI_CHANNEL_SKU_UPDATED);
    auditTrailDto.setBusinessPartnerCode(product.getMerchantCode());
    auditTrailDto.setOnlineStatus(product.isOnline());
    auditTrailDto.setPickupPointCode(Constants.DEFAULT);
    auditTrailDto.setProductSku(product.getProductSku());
    auditTrailDto.setName(itemCodeAndItemName.get(itemCode));
    return auditTrailDto;
  }

  public static String getAttributeValue(ProductAndAttributeDetailResponse productAndAttributeDetailResponse,
      List<String> attributeCodes) {
    return Optional.ofNullable(productAndAttributeDetailResponse.getProductAttributeResponses())
        .orElse(new ArrayList<>()).stream().filter(response -> isAttributeMatch(response, attributeCodes)).findFirst()
        .flatMap(CommonUtil::getProductAttributeValue).orElse(null);
  }

  private static boolean isAttributeMatch(ProductAttributeResponse response, List<String> attributeCodes) {
    AttributeResponse attribute = response.getAttribute();
    return Objects.nonNull(attribute) && attributeCodes.contains(attribute.getAttributeCode());
  }

  private static Optional<String> getProductAttributeValue(ProductAttributeResponse response) {
    return Optional.ofNullable(response.getProductAttributeValues()).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).findFirst().map(productAttributeValueResponse -> {
          switch (Optional.ofNullable(response.getAttribute().getAttributeType()).orElse(StringUtils.EMPTY)) {
            case DESCRIPTIVE_ATTRIBUTE -> {
              return productAttributeValueResponse.getDescriptiveAttributeValue();
            }
            case PREDEFINED_ATTRIBUTE -> {
              if (Objects.isNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
                return null;
              }
              return productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue();
            }
            default -> {
              return null;
            }
          }
        });
  }

  public static String generateItemSummaryCacheKey(String storeId,
    List<ItemPickupPointRequest> requests, boolean excludeDistributionPickupPoint) {
    String keySource = requests.stream().sorted(
        Comparator.comparing(ItemPickupPointRequest::getItemSku)
          .thenComparing(ItemPickupPointRequest::getPickupPointCode))
      .map(req -> req.getItemSku() + Constants.HYPHEN + req.getPickupPointCode())
      .collect(Collectors.joining(Constants.UNDERSCORE));
    return storeId + Constants.HYPHEN + excludeDistributionPickupPoint + Constants.HYPHEN + keySource.hashCode();
  }

  public static boolean isItemSummaryCacheEligible(
    List<ItemPickupPointRequest> itemPickupPointRequests, String clientId,
    List<String> itemSummaryListResponseCacheClients, int itemSummaryListRequestSize) {
    return itemSummaryListResponseCacheClients.contains(clientId)
      && itemPickupPointRequests.size() <= itemSummaryListRequestSize;
  }

  public static Set<ItemViewConfigDTO> toItemViewConfigDTOs(
    Set<com.gdn.x.product.model.entity.ItemViewConfig> allItemViewConfigs) {
    Set<ItemViewConfigDTO> itemViewConfigDTOS = new HashSet<>();
    for (com.gdn.x.product.model.entity.ItemViewConfig itemViewConfig : Optional.ofNullable(
      allItemViewConfigs).orElse(new HashSet<>())) {
      ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
      BeanUtils.copyProperties(itemViewConfig, itemViewConfigDTO);
      ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
      if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
        BeanUtils.copyProperties(itemViewConfig.getItemBuyableSchedules(), itemBuyableScheduleDTO);
      }
      itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
      ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
      if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
        BeanUtils.copyProperties(itemViewConfig.getItemDiscoverableSchedules(),
          itemDiscoverableScheduleDTO);
      }
      itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
      itemViewConfigDTOS.add(itemViewConfigDTO);
    }
    return itemViewConfigDTOS;
  }

  public static boolean isSharedProduct(List<Product> products, String sellerCode) {
    return Optional.ofNullable(products)
      .filter(p -> p.size() == Constants.ONE)
      .map(List::getFirst)
      .map(Product::getMerchantCode)
      .map(code -> !StringUtils.equals(code, sellerCode))
      .orElse(true);
  }
}
