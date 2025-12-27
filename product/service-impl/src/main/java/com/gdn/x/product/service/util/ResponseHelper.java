package com.gdn.x.product.service.util;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.ProductDataAutoFixHistoryDto;
import com.gdn.x.product.model.vo.ProductDataAutoFixHistoryListRequest;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.VideoDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PredefinedAllowedAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.SortedDefiningAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.VideoUrl;
import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;
import com.gdn.x.product.rest.web.model.response.BuyableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.DiscoverableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.SwitchContext;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.BeanUtils;

import com.gdn.x.product.enums.AdjustmentTypeEnum;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.PromoType;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemPickupPointTransactionResponse;
import com.gdn.x.product.model.vo.MasterDataItemAttributeVO;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.enums.ProductStatus;
import com.gdn.x.product.rest.web.model.response.B2BResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicMasterDataItemDTO;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CategoryDataResponse;
import com.gdn.x.product.rest.web.model.response.GeoLocationResponse;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponseV2;
import com.gdn.x.product.rest.web.model.response.PriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SkuCodeBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.util.Pair;

import static com.gdn.x.product.service.util.CommonUtil.toEanUpcPickupPointCodeResponse;
import static java.util.stream.Collectors.toList;

@Slf4j
public class ResponseHelper {

  private static final Integer DISCOUNT_PERCENTAGE = 100;
  private static final List<String> MULTI_VALUE_ATTRIBUTES =
      Arrays.asList(MasterDataAttributeType.DESCRIPTIVE_MULTIVALUE.name(),
          MasterDataAttributeType.PREDEFINED_MULTIVALUE.name());

  public static List<Off2OnPriceResponseV2> toOff2OnPriceResponseV2(List<ItemPickupPoint> itemPickupPointList,
      Map<String, Item> itemMap, Map<String, BusinessPartnerPromo> businessPartnerPromoMap) {
    List<Off2OnPriceResponseV2> off2OnPriceResponseV2List = new ArrayList<>();

    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Price price = new ArrayList<>(itemPickupPoint.getPrice()).get(0);
      Off2OnPriceResponseV2 off2OnPriceResponseV2 = new Off2OnPriceResponseV2();
      off2OnPriceResponseV2.setItemSku(itemPickupPoint.getItemSku());
      off2OnPriceResponseV2.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      off2OnPriceResponseV2.setListPrice(price.getListPrice());
      off2OnPriceResponseV2.setOfferPrice(price.getOfferPrice());
      off2OnPriceResponseV2.setOff2OnChannelActive(
          Optional.ofNullable(item).orElse(new Item()).isOff2OnChannelActive());
      off2OnPriceResponseV2.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
      if (businessPartnerPromoMap.containsKey(itemPickupPoint.getMerchantCode())) {
        off2OnPriceResponseV2.setSellerActivePromoBundlings(
            businessPartnerPromoMap.get(itemPickupPoint.getMerchantCode()).getActivePromoBundlings());
      }
      off2OnPriceResponseV2List.add(off2OnPriceResponseV2);
    }

    return off2OnPriceResponseV2List;
  }

  public static List<ItemResponseV2> toItemResponseV2(List<ItemPickupPoint> itemPickupPointList,
      Map<String, Item> itemMap, Map<String, Product> productMap,
      Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap,
      Map<String, List<CategoryResponse>> categoryHierarchyMap, boolean usePcbMasterData,
      boolean cncForWarehouseFeatureSwitch, String fetchViewConfigByChannel) {
    List<ItemResponseV2> itemResponseV2List = new ArrayList<>();

    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Product product = productMap.get(itemPickupPoint.getProductSku());

      if (Objects.isNull(item) || Objects.isNull(product)) {
        continue;
      }

      String categoryCode = ProductAndItemsUtil.getCategoryCode(product, usePcbMasterData);
      List<CategoryResponse> categoryResponseList = categoryHierarchyMap.get(categoryCode);

      ItemResponseV2 itemResponseV2 = new ItemResponseV2();
      itemResponseV2.setCategoryCode(categoryCode);
      itemResponseV2.setItemSku(itemPickupPoint.getItemSku());
      itemResponseV2.setMerchantCode(itemPickupPoint.getMerchantCode());
      itemResponseV2.setMerchantSku(itemPickupPoint.getMerchantSku());
      itemResponseV2.setPickUpPointCode(itemPickupPoint.getPickupPointCode());
      itemResponseV2.setPickUpPointName(
          Optional.ofNullable(businessPartnerPickupPointMap.get(itemPickupPoint.getPickupPointCode()))
              .orElse(new BusinessPartnerPickupPoint()).getName());
      itemResponseV2.setProductCode(product.getProductCode());
      itemResponseV2.setProductSku(itemPickupPoint.getProductSku());
      itemResponseV2.setSkuCode(item.getItemCode());
      itemResponseV2.setFbbActivated(itemPickupPoint.isFbbActivated());
      itemResponseV2.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
      itemResponseV2.setFreeSample(product.isFreeSample());
      itemResponseV2.setArchived(item.isArchived());
      itemResponseV2.setMarkForDelete(itemPickupPoint.isMarkForDelete());
      itemResponseV2.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
      itemResponseV2.setMerchantPromoDiscountActivated(
          Optional.ofNullable(itemPickupPoint.getPrice()).orElse(new HashSet<>()).stream()
              .anyMatch(price -> Objects.nonNull(price.getMerchantPromoDiscountPrice())));
      itemResponseV2.setOff2OnActiveFlag(item.isOff2OnChannelActive());
      itemResponseV2.setPromoBundling(itemPickupPoint.isPromoBundling());
      itemResponseV2.setWholesalePriceConfigEnabled(itemPickupPoint.isWholesalePriceExists());
      itemResponseV2.setWholesalePromoActivated(
          Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
              .contains(Constants.WHOLESALE));
      itemResponseV2.setWholesalePriceActivated(
          Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
              .contains(Constants.WHOLESALE_PRICE));
      itemResponseV2.setOriginalSellingPrice(
          itemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getOfferPrice());
      itemResponseV2.setActivePromoBundlings(
          new ArrayList<>(Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())));
      itemResponseV2.setPrices(setPriceResponse(itemPickupPoint, true));
      itemResponseV2.setViewConfigs(setViewConfigResponse(itemPickupPoint, fetchViewConfigByChannel,
          cncForWarehouseFeatureSwitch));
      itemResponseV2.setCncActive(
          setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(), cncForWarehouseFeatureSwitch,
              itemPickupPoint.isCncActive(), ItemViewConfig::getChannel,
              ItemViewConfig::isBuyable));
      itemResponseV2.setPromoTypes(setPromoTypes(itemPickupPoint));
      if (CollectionUtils.isNotEmpty(categoryResponseList)) {
        itemResponseV2.setCategoryId(categoryResponseList.get(0).getId());
        itemResponseV2.setCategoryName(categoryResponseList.get(0).getName());
        itemResponseV2.setCategoryHierarchy(getCategoryHierarchyString(categoryResponseList));
      }
      if (product.isSynchronized() && !usePcbMasterData && StringUtils.isNotBlank(product.getCategoryCode())) {
        setItemResponseMasterDataSync(itemResponseV2, product, item);
      } else {
        setItemResponseMasterDataUnSync(itemResponseV2, product, item);
      }
      itemResponseV2.setB2BResponse(setB2bResponse(itemPickupPoint));
      itemResponseV2.setSuspended(product.isSuspended());
      itemResponseV2.setPreOrder(getPreOrder(product.getPreOrder()));
      itemResponseV2List.add(itemResponseV2);
    }
    return itemResponseV2List;
  }

  private static B2BResponse setB2bResponse(ItemPickupPoint itemPickupPoint) {
    B2BResponse b2BResponse = new B2BResponse();
    if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
      b2BResponse.setBasePrice(itemPickupPoint.getB2bFields().getBasePrice());
      b2BResponse.setManaged(itemPickupPoint.getB2bFields().isManaged());
    }
    return b2BResponse;
  }

  private static List<PriceResponse> setPriceResponse(ItemPickupPoint itemPickupPoint,
      boolean replaceOfferPriceWithMerchantPromoPrice) {
    List<PriceResponse> priceResponses = new ArrayList<>();
    for (Price price : itemPickupPoint.getPrice()) {
      List<DiscountPrice> discountPrices =
          new ArrayList<>(Optional.of(price.getListOfDiscountPrices()).orElse(new ArrayList<>()));
      if (Optional.of(price).map(Price::getMerchantPromoDiscountPrice).isPresent()
        && itemPickupPoint.isMerchantPromoDiscount()) {
        PriceResponse priceResponse = new PriceResponse();
        priceResponse.setChannelId(price.getChannel());
        priceResponse.setPrice(price.getListPrice());
        setPriceResponseAndManageDiscount(replaceOfferPriceWithMerchantPromoPrice, price,
          priceResponse);
        priceResponses.add(priceResponse);
      }
      else if (CollectionUtils.isNotEmpty(discountPrices)) {
        DiscountPrice discountPrice = discountPrices.get(0);
        PriceResponse priceResponse = new PriceResponse();
        priceResponse.setChannelId(price.getChannel());
        priceResponse.setPromotionName(discountPrice.getAdjustmentName());
        priceResponse.setPrice(price.getListPrice());
        priceResponse.setSalePrice(price.getOfferPrice());
        priceResponse.setDiscountPercentage((discountPrice.getDiscountPrice() / price.getListPrice()) * DISCOUNT_PERCENTAGE);
        priceResponse.setDiscountAmount(discountPrice.getDiscountPrice());
        priceResponse.setDiscountStartDate(discountPrice.getStartDateTime());
        priceResponse.setDiscountEndDate(discountPrice.getEndDateTime());
        priceResponses.add(priceResponse);
      } else {
        PriceResponse priceResponse = new PriceResponse();
        priceResponse.setChannelId(price.getChannel());
        priceResponse.setPrice(price.getListPrice());
        priceResponse.setSalePrice(price.getOfferPrice());
        setDiscountPercentage(price, priceResponse, price.getOfferPrice());
        priceResponse.setDiscountAmount(price.getListPrice() - price.getOfferPrice());
        priceResponses.add(priceResponse);
      }
    }
    return priceResponses;
  }

  private static void setPriceResponseAndManageDiscount(boolean replaceOfferPriceWithMerchantPromoPrice, Price price,
    PriceResponse priceResponse) {
    if (replaceOfferPriceWithMerchantPromoPrice) {
      double sellingPriceForActiveMerchantPromoDiscount = price.getMerchantPromoDiscountPrice().getDiscountPrice();
      priceResponse.setSalePrice(sellingPriceForActiveMerchantPromoDiscount);
      setDiscountPercentage(price, priceResponse, sellingPriceForActiveMerchantPromoDiscount);
      priceResponse.setDiscountAmount(price.getListPrice() - sellingPriceForActiveMerchantPromoDiscount);
    } else {
      priceResponse.setSalePrice(price.getOfferPrice());
      setDiscountPercentage(price, priceResponse, price.getOfferPrice());
      priceResponse.setDiscountAmount(price.getListPrice() - price.getOfferPrice());
    }
  }

  private static void setDiscountPercentage(Price price, PriceResponse priceResponse,
    double offerPrice) {
    if (Double.compare(price.getListPrice(), 0) > 0) {
      priceResponse.setDiscountPercentage(
        BigDecimal.valueOf(price.getListPrice()).subtract(BigDecimal.valueOf(offerPrice))
          .divide(BigDecimal.valueOf(price.getListPrice()), 4, RoundingMode.HALF_UP)
          .multiply(BigDecimal.valueOf(DISCOUNT_PERCENTAGE)).setScale(4, RoundingMode.HALF_UP)
          .doubleValue());
    }
  }

  private static B2BResponse setB2BResponse(ItemPickupPoint itemPickupPoint) {
    if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
      B2BResponse b2BResponse = new B2BResponse();
      BeanUtils.copyProperties(itemPickupPoint.getB2bFields(), b2BResponse);
      return b2BResponse;
    }
    return null;
  }

  private static List<ViewConfigResponse> setViewConfigResponseForL5ListingResponse(
      ItemPickupPoint itemPickupPoint, String fetchViewConfigByChannel,
      boolean cncWarehouseFeatureSwitch) {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getAllItemViewConfigs())) {
      Pair<Set<String>, Boolean> allowedConfigSet =
          getRequestViewConfigSet(fetchViewConfigByChannel, cncWarehouseFeatureSwitch);
      for (ItemViewConfig itemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
        if (allowedConfigSet.getSecond() && !allowedConfigSet.getFirst()
            .contains(itemViewConfig.getChannel())) {
          continue;
        }
        ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
        viewConfigResponse.setChannelId(itemViewConfig.getChannel());
        viewConfigResponse.setBuyable(itemViewConfig.isBuyable());
        viewConfigResponse.setDisplay(itemViewConfig.isDiscoverable());
        setBuyableScheduleRequest(viewConfigResponse, itemViewConfig.getItemBuyableSchedules());
        setDiscoverableScheduleRequest(viewConfigResponse, itemViewConfig.getItemDiscoverableSchedules());
        viewConfigResponses.add(viewConfigResponse);
      }
    }
    else {
      ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
      viewConfigResponse.setChannelId(Constants.DEFAULT_CHANNEL);
      viewConfigResponses.add(viewConfigResponse);
    }
    return viewConfigResponses;
  }

  public static void setBuyableScheduleRequest(ViewConfigResponse viewConfigResponse,
      ItemBuyableSchedule itemBuyableSchedule) {
    if (Objects.nonNull(itemBuyableSchedule) && !CommonUtil.isScheduleCompleted(itemBuyableSchedule.getEndDateTime())) {
      BuyableScheduleResponse buyableScheduleResponse = new BuyableScheduleResponse();
      buyableScheduleResponse.setBuyable(itemBuyableSchedule.isBuyable());
      buyableScheduleResponse.setStartDateTime(itemBuyableSchedule.getStartDateTime());
      buyableScheduleResponse.setEndDateTime(itemBuyableSchedule.getEndDateTime());
      viewConfigResponse.setBuyableScheduleResponse(buyableScheduleResponse);
    }
  }

  public static void setDiscoverableScheduleRequest(ViewConfigResponse viewConfigResponse,
      ItemDiscoverableSchedule itemDiscoverableSchedule) {
    if (Objects.nonNull(itemDiscoverableSchedule) && !CommonUtil.isScheduleCompleted(itemDiscoverableSchedule.getEndDateTime())) {
      DiscoverableScheduleResponse discoverableScheduleResponse = new DiscoverableScheduleResponse();
      discoverableScheduleResponse.setDiscoverable(itemDiscoverableSchedule.isDiscoverable());
      discoverableScheduleResponse.setStartDateTime(itemDiscoverableSchedule.getStartDateTime());
      discoverableScheduleResponse.setEndDateTime(itemDiscoverableSchedule.getEndDateTime());
      viewConfigResponse.setDiscoverableScheduleResponse(discoverableScheduleResponse);
    }
  }

  public static <T> boolean setCncActivatedForBackward(Collection<T> viewConfigResponses,
      boolean cncWarehouseFeatureSwitch, boolean cncActivated, Function<T, String> getChannelName,
      Function<T, Boolean> isBuyable) {
    if (cncWarehouseFeatureSwitch) {
      return viewConfigResponses.stream().filter(
              viewConfigResponse -> getChannelName.apply(viewConfigResponse).equals(Constants.CNC))
          .findFirst().map(isBuyable).orElse(false);
    }
    return cncActivated;
  }
  private static List<ViewConfigResponse> setViewConfigResponse(ItemPickupPoint itemPickupPoint,
      String fetchViewConfigByChannel, boolean cncForWarehouseFeatureSwitch) {
    List<ViewConfigResponse> viewConfigResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getItemViewConfig())) {
      Pair<Set<String>, Boolean> allowedConfigSet =
          getRequestViewConfigSet(fetchViewConfigByChannel, cncForWarehouseFeatureSwitch);
      Set<ItemViewConfig> itemViewConfigs = itemPickupPoint.getAllItemViewConfigs();
      if (!cncForWarehouseFeatureSwitch) {
        itemViewConfigs.removeIf(
            itemViewConfig -> itemViewConfig.getChannel().equals(Constants.CNC));
      }

      for (ItemViewConfig itemViewConfig : itemViewConfigs) {
        if (allowedConfigSet.getSecond() && !allowedConfigSet.getFirst()
            .contains(itemViewConfig.getChannel())) {
          continue;
        }
        ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
        viewConfigResponse.setChannelId(itemViewConfig.getChannel());
        viewConfigResponse.setBuyable(itemViewConfig.isBuyable());
        viewConfigResponse.setDisplay(itemViewConfig.isDiscoverable());
        viewConfigResponses.add(viewConfigResponse);
      }
    }
    else {
      ViewConfigResponse viewConfigResponse = new ViewConfigResponse();
      viewConfigResponse.setChannelId(Constants.DEFAULT_CHANNEL);
      viewConfigResponses.add(viewConfigResponse);
    }
    return viewConfigResponses;
  }

  public static Pair<Set<String>, Boolean> getRequestViewConfigSet(String fetchViewConfigByChannel,
      boolean cncForWarehouseFeatureSwitch) {
    Set<String> channels;
    if (cncForWarehouseFeatureSwitch) {
      if (StringUtils.isNotEmpty(fetchViewConfigByChannel)) {
        channels = Set.of(fetchViewConfigByChannel.split(Constants.COMMA));
      } else {
        channels = Collections.singleton(Constants.DEFAULT_CHANNEL);
      }
    } else {
      channels = Collections.emptySet();
    }
    boolean filterNeeded = cncForWarehouseFeatureSwitch && !channels.contains(Constants.ALL);
    return Pair.of(channels, filterNeeded);
  }

  public static Set<ItemViewConfig> setViewConfigResponseByRequestString(Set<ItemViewConfig> itemViewConfigList,
      String fetchViewConfigByChannel, boolean cncForWarehouseFeatureSwitch) {
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    if (CollectionUtils.isNotEmpty(itemViewConfigList)) {
      Pair<Set<String>, Boolean> allowedConfigSet =
          getRequestViewConfigSet(fetchViewConfigByChannel, cncForWarehouseFeatureSwitch);
      if (!cncForWarehouseFeatureSwitch) {
        itemViewConfigList.removeIf(
            itemViewConfig -> itemViewConfig.getChannel().equals(Constants.CNC));
      }
      for (ItemViewConfig itemViewConfigItem : itemViewConfigList) {
        if (allowedConfigSet.getSecond() && !allowedConfigSet.getFirst()
            .contains(itemViewConfigItem.getChannel())) {
          continue;
        }
        ItemViewConfig itemViewConfig =  new ItemViewConfig();
        itemViewConfig.setChannel(itemViewConfigItem.getChannel());
        itemViewConfig.setBuyable(itemViewConfigItem.isBuyable());
        itemViewConfig.setDiscoverable(itemViewConfigItem.isDiscoverable());
        itemViewConfig.setItemBuyableSchedules(itemViewConfigItem.getItemBuyableSchedules());
        itemViewConfig.setItemDiscoverableSchedules(itemViewConfigItem.getItemDiscoverableSchedules());
        itemViewConfigSet.add(itemViewConfig);
      }
    }
    return itemViewConfigSet;
  }

  private static String getCategoryHierarchyString(List<CategoryResponse> categoryResponseList) {
    return StringUtils.join(
        Lists.reverse(categoryResponseList.stream().map(CategoryResponse::getName).collect(Collectors.toList())),
        Constants.CATEGORY_HIERARCHY_DELIMITER);
  }

  private static void setItemResponseMasterDataSync(ItemResponseV2 itemResponseV2, Product product, Item item) {
    itemResponseV2.setBrand(product.getBrand());
    itemResponseV2.setItemName(item.getGeneratedItemName());
    itemResponseV2.setMainImageURL(item.getMainImageUrl());
    itemResponseV2.setProductName(product.getProductName());
    itemResponseV2.setProductDetailPageLink(
        String.format(Constants.PRODUCT_DETAIL_PAGE_LINK, product.getProductName(), product.getProductSku()));
  }

  private static void setItemResponseMasterDataUnSync(ItemResponseV2 itemResponseV2, Product product, Item item) {
    if (Objects.nonNull(product.getMasterDataProduct())) {
      itemResponseV2.setBrand(product.getMasterDataProduct().getBrand());
      itemResponseV2.setProductName(product.getMasterDataProduct().getProductName());
      itemResponseV2.setProductDetailPageLink(
          String.format(Constants.PRODUCT_DETAIL_PAGE_LINK, product.getMasterDataProduct().getProductName(),
              product.getProductSku()));
    }
    if (Objects.nonNull(item.getMasterDataItem())) {
      itemResponseV2.setItemName(item.getMasterDataItem().getGeneratedItemName());
      itemResponseV2.setMainImageURL(
          item.getMasterDataItem().getMasterDataItemImages().stream().filter(MasterDataItemImage::isMainImage)
              .findFirst().orElse(new MasterDataItemImage()).getLocationPath());
    }
  }

  public static List<ItemPickupPointTransactionResponse> toProductForTransactionVO(
      List<ItemPickupPoint> itemPickupPointList, Map<String, Item> itemMap, Map<String, Product> productMap,
      Map<String, String> pickupPointNameMap, Set<String> withoutMasterDataProductSkus,
      boolean mainImageFromMainImageUrlForUnsync, boolean overrideLateFulfillmentByProductType,
      boolean cncWarehouseFeatureSwitch, String imeiAttributeCode, List<String> imeiAllowedValues)
      throws ParseException {
    List<ItemPickupPointTransactionResponse> itemPickupPointTransactionResponseList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Product product = productMap.get(itemPickupPoint.getProductSku());
      if (Objects.isNull(item) || Objects.isNull(product)) {
        continue;
      }

      ProductItemDetailVO productItemDetailVO = ProductItemDetailVO.builder().itemSku(itemPickupPoint.getItemSku())
          .itemCatentryId(itemPickupPoint.getItemSku()).productCatentryId(itemPickupPoint.getProductSku())
          .merchantCode(itemPickupPoint.getMerchantCode()).merchantSku(itemPickupPoint.getMerchantSku())
          .productTypeCode(product.getProductType().getCode())
          .productTypeName(product.getProductType().getDescription()).itemCatalogs(product.getItemCatalogs())
          .productSku(itemPickupPoint.getProductSku()).settlementType(product.getSettlementType())
          .pickupPointCode(itemPickupPoint.getPickupPointCode())
          .ticketTemplateCode(item.getTicketTemplateCode()).productCode(product.getProductCode())
          .installationRequired(product.isInstallationRequired()).off2OnChannelActive(product.isOff2OnChannelActive())
          .preOrder(checkPreOrder(product)).cncActive(
              setCncActivatedForBackward(itemPickupPoint.getItemViewConfigByChannel(Constants.CNC),
                  cncWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
                  ItemViewConfig::getChannel, ItemViewConfig::isBuyable))
          .isLateFulfillment(CommonUtil.getLateFulfillmentFromProductType(product.getProductType(),
              overrideLateFulfillmentByProductType, item.isLateFulfillment()))
          .offerPrice(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice())
          .listPrice(itemPickupPoint.getPrice().stream().findFirst().get().getListPrice())
          .buyable(itemPickupPoint.getItemViewConfig().stream().findFirst().get().isBuyable())
          .discoverable(itemPickupPoint.getItemViewConfig().stream().findFirst().get().isDiscoverable()).documentType(
              getDocumentTypeFromItemCatalogs(product, withoutMasterDataProductSkus)).pickupPointName(
                  pickupPointNameMap.get(itemPickupPoint.getPickupPointCode())).build(); setWarrantyInfo(productItemDetailVO, product);
      setPristineId(productItemDetailVO, item);
      setMasterDataItemAttributeValue(productItemDetailVO, product, item, withoutMasterDataProductSkus);
      if (StringUtils.isNotBlank(imeiAttributeCode)) {
        setImeiRequiredFlag(productItemDetailVO, product, imeiAttributeCode, imeiAllowedValues);
      }
      if (product.isSynchronized() && !withoutMasterDataProductSkus.contains(product.getProductSku())) {
        setMasterDataDetailsForSyncProduct(productItemDetailVO, product, item);
      } else {
        setMasterDataDetailsForUnSyncProduct(productItemDetailVO, product, item, mainImageFromMainImageUrlForUnsync);
      }
      itemPickupPointTransactionResponseList.add(
          new ItemPickupPointTransactionResponse(item.getItemSku(), item.getItemCode(), productItemDetailVO));
    }
    return itemPickupPointTransactionResponseList;
  }

  private static void setImeiRequiredFlag(ProductItemDetailVO productItemDetailVO, Product product,
      String imeiAttributeCode, List<String> imeiAllowedYesValues) {
    Optional.ofNullable(product.getProductSpecialAttributes()).filter(CollectionUtils::isNotEmpty).map(
        specialAttributes -> specialAttributes.stream().filter(Objects::nonNull)
            .filter(specialAttribute -> specialAttribute.getAttributeCode().equals(imeiAttributeCode)).collect(
                Collectors.toMap(ProductSpecialAttribute::getAttributeCode, ProductSpecialAttribute::getAttributeValue,
                    (old, newValue) -> newValue))).ifPresent(attributeMap -> {
      String imeiValue = attributeMap.get(imeiAttributeCode);
      productItemDetailVO.setImeiRequired(imeiAllowedYesValues.contains(imeiValue));
    });
  }

  public static List<ItemSummaryListResponse> toItemSummaryListResponse(
    List<ItemPickupPoint> itemPickupPointList, Map<String, Item> itemMap,
    Map<String, Product> productMap, Map<String, String> pickupPointNameMap,
    Map<String, ItemViewConfig> offlineItemIdAndViewConfigOriginalMap,
    Map<String, ItemViewConfig> itemViewConfigB2bMap,
    Map<String, Map<String, Map<String, String>>> productAttributeValueAndValueTypeMap,
    SwitchContext switchContext, String fetchViewConfigByChannel) {
    List<ItemSummaryListResponse> itemSummaryListResponseList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Product product = productMap.get(itemPickupPoint.getProductSku());
      if (skipProcessingL5(product, item, switchContext)) {
        continue;
      }
      ItemSummaryListResponse itemSummaryListResponse =
          ItemSummaryListResponse.builder().itemSku(itemPickupPoint.getItemSku())
              .markForDelete(itemPickupPoint.isMarkForDelete()).pickupPointCode(itemPickupPoint.getPickupPointCode())
              .itemCode(item.getItemCode()).merchantSku(itemPickupPoint.getMerchantSku())
              .productSku(itemPickupPoint.getProductSku()).merchantCode(itemPickupPoint.getMerchantCode())
              .cncActive(itemPickupPoint.isCncActive()).fbbActivated(itemPickupPoint.isFbbActivated())
              .pickupPointName(pickupPointNameMap.get(itemPickupPoint.getPickupPointCode()))
              .freeSample(item.isFreeSample()).off2OnChannelActive(item.isOff2OnChannelActive())
              .merchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount()).productType(product.getProductType())
              .brandName(product.getBrand()).forceReview(product.isForceReview()).lateFulfillment(
                  CommonUtil.getLateFulfillmentFromProductType(product.getProductType(),
                    switchContext.isOverrideLateFulfillmentByProductType(), item.isLateFulfillment())).archived(product.isArchived())
              .promoBundling(item.isPromoBundling())
              .wholesalePriceActivated(CommonUtil.getWholeSalePriceActivated(itemPickupPoint))
              .masterCategoryCode(ProductAndItemsUtil.getCategoryCode(product, switchContext.isUsePcbMasterData())).salesCategoryCode(
                  product.getSalesCatalogs().stream().flatMap(salesCatalog -> salesCatalog.getListOfCategories().stream())
                      .map(Category::getCategoryCode).distinct().collect(Collectors.toList()))
              .itemCatalogs(product.getItemCatalogs()).productCode(product.getProductCode())
            .b2bActivated(product.isB2bActivated()).suspended(product.isSuspended()).archived(product.isArchived())
              .halalProduct(CurationStatus.APPROVED.equals(product.getCurationStatus()))
              .sizeChartCode(product.getSizeChartCode())
              .url(product.getUrl())
              .distribution(itemPickupPoint.isDistribution())
              .build();
      if (Objects.nonNull(product.getVideo())) {
        itemSummaryListResponse.setUrl(StringUtils.EMPTY);
        itemSummaryListResponse.setVideo(new VideoUrl(
          StringUtils.defaultIfEmpty(product.getVideo().getFinalUrl(),
            product.getVideo().getSourceUrl()), product.getVideo().getCoverImagePath()));
      }
      if (switchContext.isCncForWarehouseFeatureSwitch()) {
        itemSummaryListResponse.setItemViewConfigs(
            getItemViewConfigs(fetchViewConfigByChannel, itemPickupPoint.getAllItemViewConfigs()));
      } else {
        setItemViewConfigIfCncSwitchFalse(itemSummaryListResponse, itemPickupPoint,
            itemViewConfigB2bMap, offlineItemIdAndViewConfigOriginalMap);
      }
      itemSummaryListResponse.setDimensionsMissing(product.getDimensionsMissing());
      itemSummaryListResponse.setCncActive(
          setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
              switchContext.isCncForWarehouseFeatureSwitch(), itemPickupPoint.isCncActive(),
              ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
      itemSummaryListResponse.setB2cActivated(product.getB2cActivated());
      itemSummaryListResponse.setPrice(
          itemPickupPoint.getPrice().stream().map(ResponseHelper::convertToItemPriceResponse)
              .collect(Collectors.toSet()));
      itemSummaryListResponse.setOriginalSellingPrice(
          itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
      itemSummaryListResponse.setMerchantPromoDiscountActivated(
          Optional.ofNullable(itemPickupPoint.getPrice()).orElse(new HashSet<>()).stream()
              .anyMatch(price -> Objects.nonNull(price.getMerchantPromoDiscountPrice())));
      itemSummaryListResponse.setB2bFields(toB2bFields(itemPickupPoint));
      if (switchContext.isSubscriptionAtL5Flow()) {
        itemSummaryListResponse.setSubscribable(item.isSubscribable());
        itemSummaryListResponse.setSubscribableAtL5Level(itemPickupPoint.isSubscribable());
      }
      if (product.isSynchronized() && !switchContext.isUsePcbMasterData() && StringUtils.isNotBlank(product.getCategoryCode())) {
        setMasterDataDetailsForSyncProduct(itemSummaryListResponse, product, item,
            productAttributeValueAndValueTypeMap);
      } else {
        setMasterDataDetailsForUnSyncProduct(itemSummaryListResponse, product, item,
          switchContext.isMainImageFromMainImageUrlForUnsync());
      }
      itemSummaryListResponse.setMissingFields(Optional.ofNullable(product.getMissingFields()).orElse(new HashSet<>()));
      itemSummaryListResponseList.add(itemSummaryListResponse);
    }
    return itemSummaryListResponseList;
  }

  private static boolean skipProcessingL5(Product product, Item item, SwitchContext switchContext) {
    if(Objects.isNull(product) || Objects.isNull(item)){
      return true;
    } else if (switchContext.isRemoveTakenDownL5FromResponse()) {
      return product.isForceReview() || product.isTakenDown() || item.isForceReview();
    }
    return false;
  }

  private  static void setItemViewConfigIfCncSwitchFalse(ItemSummaryListResponse itemSummaryListResponse,
      ItemPickupPoint itemPickupPoint, Map<String, ItemViewConfig> itemViewConfigB2bMap,
      Map<String, ItemViewConfig> offlineItemIdAndViewConfigOriginalMap) {
    itemSummaryListResponse.setItemViewConfigs(new HashSet<>(
        convertToProductViewConfigSetResponse(itemPickupPoint,
            offlineItemIdAndViewConfigOriginalMap)));
    ItemViewConfig itemViewConfigB2b = itemViewConfigB2bMap.get(
        CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(),
            itemPickupPoint.getPickupPointCode()));
    if (Objects.nonNull(itemViewConfigB2b)) {
      itemSummaryListResponse.getItemViewConfigs().add(toItemViewConfigDTO(itemViewConfigB2b));
    }
  }

  public static Set<ItemViewConfigDTO> getItemViewConfigs(String fetchViewConfigByChannel,
      Set<ItemViewConfig> itemViewConfigList) {
    Set<ItemViewConfigDTO> itemViewConfigDTOSet = new HashSet<>();
    if (CollectionUtils.isNotEmpty(itemViewConfigList)) {
      Pair<Set<String>, Boolean> allowedConfigSet =
          getRequestViewConfigSet(fetchViewConfigByChannel, Boolean.TRUE);
      for (ItemViewConfig itemViewConfig : itemViewConfigList) {
        if (allowedConfigSet.getSecond() && !allowedConfigSet.getFirst()
            .contains(itemViewConfig.getChannel())) {
          continue;
        }
        ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
        itemViewConfigDTO.setChannel(itemViewConfig.getChannel());
        itemViewConfigDTO.setBuyable(itemViewConfig.isBuyable());
        itemViewConfigDTO.setDiscoverable(itemViewConfig.isDiscoverable());
        itemViewConfigDTO.setBuyableOriginal(itemViewConfig.isBuyable());
        itemViewConfigDTO.setDiscoverableOriginal(itemViewConfig.isDiscoverable());
        if (itemViewConfig.getItemBuyableSchedules() != null) {
          ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
          BeanUtils.copyProperties(itemViewConfig.getItemBuyableSchedules(),
              itemBuyableScheduleDTO);
          itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
        }
        if (itemViewConfig.getItemDiscoverableSchedules() != null) {
          ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO =
              new ItemDiscoverableScheduleDTO();
          BeanUtils.copyProperties(itemViewConfig.getItemDiscoverableSchedules(),
              itemDiscoverableScheduleDTO);
          itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
        }
        itemViewConfigDTOSet.add(itemViewConfigDTO);
      }
    } else {
      ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
      itemViewConfigDTO.setChannel(Constants.DEFAULT_CHANNEL);
      itemViewConfigDTOSet.add(itemViewConfigDTO);
    }
    return itemViewConfigDTOSet;
  }

  private static void setMasterDataDetailsForSyncProduct(ProductItemDetailVO productItemDetailVO, Product product, Item item) {
    productItemDetailVO.setProductName(product.getProductName());
    productItemDetailVO.setItemName(item.getGeneratedItemName());
    productItemDetailVO.setShippingWeight(item.getShippingWeight());
    productItemDetailVO.setBrandName(product.getBrand());
    productItemDetailVO.setItemLength(item.getLength());
    productItemDetailVO.setItemWidth(item.getWidth());
    productItemDetailVO.setItemHeight(item.getHeight());
    productItemDetailVO.setItemWeight(item.getWeight());
    productItemDetailVO.setImageUrl(item.getMainImageUrl());
    productItemDetailVO.setDangerousLevel(Objects.nonNull(item.getDangerousLevel()) ? item.getDangerousLevel() : 0);
  }

  private static B2bFieldsDTO toB2bFields(ItemPickupPoint itemPickupPoint) {
    B2bFieldsDTO b2bFieldsDTO = new B2bFieldsDTO();
    if (Objects.nonNull(itemPickupPoint.getB2bFields())) {
      BeanUtils.copyProperties(itemPickupPoint.getB2bFields(), b2bFieldsDTO);
    }
    return b2bFieldsDTO;
  }

  public static ItemViewConfigDTO toItemViewConfigDTO(ItemViewConfig itemViewConfig) {
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    if (Objects.nonNull(itemViewConfig)) {
      itemViewConfigDTO.setChannel(itemViewConfig.getChannel());
      itemViewConfigDTO.setBuyable(itemViewConfig.isBuyable());
      itemViewConfigDTO.setDiscoverable(itemViewConfig.isDiscoverable());
    }
    return itemViewConfigDTO;
  }

  public static List<ItemPickupPointBasicResponse> toItemPickupPointBasicResponse(
    List<ItemPickupPoint> itemPickupPointList) {

    List<ItemPickupPointBasicResponse> itemPickupPointBasicResponseList = new ArrayList<>();

    itemPickupPointList.forEach(itemPickupPoint -> {
      ItemPickupPointBasicResponse response = new ItemPickupPointBasicResponse();
      response.setItemSku(itemPickupPoint.getItemSku());
      response.setPickupPointCode(itemPickupPoint.getPickupPointCode());

      itemPickupPoint.getItemViewConfig().stream().findFirst().ifPresent(itemViewConfig -> {
        ViewConfigResponse viewConfigResponse = new ViewConfigResponse();

        ItemDiscoverableSchedule discoverableSchedule =
          itemViewConfig.getItemDiscoverableSchedules();
        if (Objects.nonNull(discoverableSchedule)) {
          viewConfigResponse.setDiscoverableScheduleResponse(DiscoverableScheduleResponse.builder()
            .endDateTime(discoverableSchedule.getEndDateTime())
            .startDateTime(discoverableSchedule.getStartDateTime())
            .discoverable(discoverableSchedule.isDiscoverable()).build());
        }

        ItemBuyableSchedule buyableSchedule = itemViewConfig.getItemBuyableSchedules();
        if (Objects.nonNull(buyableSchedule)) {
          viewConfigResponse.setBuyableScheduleResponse(
            BuyableScheduleResponse.builder().startDateTime(buyableSchedule.getStartDateTime())
              .endDateTime(buyableSchedule.getEndDateTime()).buyable(buyableSchedule.isBuyable())
              .build());
        }

        viewConfigResponse.setBuyable(itemViewConfig.isBuyable());
        viewConfigResponse.setDisplay(itemViewConfig.isDiscoverable());
        viewConfigResponse.setChannelId(itemViewConfig.getChannel());
        response.setAllItemViewConfigDTO(
            itemPickupPoint.getAllItemViewConfigs().stream().map(ResponseHelper::convertToProductViewConfigResponse)
                .collect(Collectors.toList()));
        response.setViewConfigResponse(viewConfigResponse);
      });
      itemPickupPointBasicResponseList.add(response);
    });
    return itemPickupPointBasicResponseList;
  }


  private static void setMasterDataDetailsForUnSyncProduct(ProductItemDetailVO productItemDetailVO, Product product,
      Item item, boolean mainImageFromMainImageUrlForUnsync) {
    MasterDataProduct masterDataProduct = product.getMasterDataProduct();
    MasterDataItem masterDataItem = item.getMasterDataItem();
    if (Objects.nonNull(masterDataProduct)) {
      productItemDetailVO.setProductName(masterDataProduct.getProductName());
      productItemDetailVO.setBrandName(masterDataProduct.getBrand());
      productItemDetailVO.setShippingWeight(masterDataProduct.getShippingWeight());
      productItemDetailVO.setItemLength(masterDataProduct.getLength());
      productItemDetailVO.setItemWidth(masterDataProduct.getWidth());
      productItemDetailVO.setItemHeight(masterDataProduct.getHeight());
      productItemDetailVO.setItemWeight(masterDataProduct.getWeight());
    }
    if (Objects.nonNull(masterDataItem)) {
      productItemDetailVO.setItemName(masterDataItem.getGeneratedItemName());
      productItemDetailVO.setImageUrl(masterDataItem.getMasterDataItemImages().stream()
          .filter(masterDataItemImage -> masterDataItemImage.isMainImage()).findFirst()
          .orElse(new MasterDataItemImage()).getLocationPath());
      productItemDetailVO.setDangerousLevel(masterDataItem.getDangerousLevel());
    }
    if (mainImageFromMainImageUrlForUnsync) {
      productItemDetailVO.setImageUrl(item.getMainImageUrl());
    }
  }

  private static void setMasterDataDetailsForUnSyncProduct(ItemSummaryListResponse itemSummaryListResponse, Product product,
      Item item, boolean mainImageFromMainImageUrlForUnsync) {
    MasterDataProduct masterDataProduct = product.getMasterDataProduct();
    MasterDataItem masterDataItem = item.getMasterDataItem();
    if (Objects.nonNull(masterDataProduct)) {
      itemSummaryListResponse.setProductName(masterDataProduct.getProductName());
    }
    if (Objects.nonNull(masterDataItem)) {
      itemSummaryListResponse.setItemName(masterDataItem.getGeneratedItemName());
      if (mainImageFromMainImageUrlForUnsync) {
        itemSummaryListResponse.setMainImageUrl(item.getMainImageUrl());
      } else {
        itemSummaryListResponse.setMainImageUrl(masterDataItem.getMasterDataItemImages().stream()
            .filter(masterDataItemImage -> masterDataItemImage.isMainImage()).findFirst()
            .orElse(new MasterDataItemImage()).getLocationPath());
      }
      itemSummaryListResponse.setDefiningAttributes(masterDataItem.getMasterDataItemAttributeValues().stream()
          .filter(masterDataItemAttributeValue -> !masterDataItemAttributeValue.isMarkForDelete()).filter(CommonUtil::isDefiningOrVariantCreating)
          .map(masterDataItemAttributeValue -> new ProductAttributeDetailDTO(
              masterDataItemAttributeValue.getMasterDataAttribute().getAttributeCode(),
              masterDataItemAttributeValue.getMasterDataAttribute().getAttributeName(),
              masterDataItemAttributeValue.getAttributeValue(), null, null, null)).collect(Collectors.toList()));
    }
  }

  private static List<String> setPromoTypes(ItemPickupPoint itemPickupPoint) {
    List<String> promoTypes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
      if (itemPickupPoint.isMerchantPromoDiscount()) {
        promoTypes.add(PromoType.PROMO_DISCOUNT.getDescription());
      }
      if (itemPickupPoint.isPromoBundling()) {
        promoTypes.add(PromoType.PROMO_BUNDLING.getDescription());
      }
      Date now = new Date();
      if (itemPickupPoint.getPrice().stream().anyMatch(
          price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices()
              .stream().anyMatch(
                  discountPrice -> (org.apache.commons.lang3.StringUtils.isNotBlank(discountPrice.getCampaignCode())
                      && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))))) {
        promoTypes.add(PromoType.CAMPAIGN.getDescription());
      }
    }
    return promoTypes;
  }

  public static List<ProductSummaryResponseV2Vo> toProductSummaryResponseV2Vo(List<ProductSolr> productSolrList,
      String delimiter) {
    List<ProductSummaryResponseV2Vo> productSummaryResponseV2VoList = new ArrayList<>();
    for (ProductSolr productSolr : productSolrList) {
      ProductSummaryResponseV2Vo productSummaryResponseV2Vo = new ProductSummaryResponseV2Vo();
      productSummaryResponseV2Vo.setBrand(productSolr.getBrand());
      if (StringUtils.isNotBlank(productSolr.getMasterCatalog())) {
        productSummaryResponseV2Vo.setCategoryCode(productSolr.getMasterCatalog().split(delimiter)[1]);
      }
      productSummaryResponseV2Vo.setMerchantCode(productSolr.getMerchantCode());
      productSummaryResponseV2Vo.setProductCode(productSolr.getProductCode());
      productSummaryResponseV2Vo.setProductDetailPageLink(
          String.format(Constants.PRODUCT_DETAIL_PAGE_LINK, productSolr.getProductName(), productSolr.getProductSku()));
      productSummaryResponseV2Vo.setProductMainImage(productSolr.getProductMainImage());
      productSummaryResponseV2Vo.setProductName(productSolr.getProductName());
      productSummaryResponseV2Vo.setProductSku(productSolr.getProductSku());
      productSummaryResponseV2Vo.setArchived(Objects.nonNull(productSolr.getIsArchived()) ? productSolr.getIsArchived() : false);
      productSummaryResponseV2Vo.setInStock(Objects.nonNull(productSolr.getInStock()) ? productSolr.getInStock() : false);
      productSummaryResponseV2Vo.setOff2OnActiveFlag(Objects.nonNull(productSolr.getOff2OnChannelActive()) ?
          productSolr.getOff2OnChannelActive() : false);
      productSummaryResponseV2Vo.setMinNormalPrice(Objects.nonNull(productSolr.getMinimumListPrice()) ?
          productSolr.getMinimumListPrice() : Constants.DEFAULT_PRICE);
      productSummaryResponseV2Vo.setMinSellingPrice(Objects.nonNull(productSolr.getMinimumSellingPrice()) ?
          productSolr.getMinimumSellingPrice() : Constants.DEFAULT_PRICE);
      productSummaryResponseV2Vo.setMaxNormalPrice(Objects.nonNull(productSolr.getMaximumListPrice()) ?
          productSolr.getMaximumListPrice() : Constants.DEFAULT_PRICE);
      productSummaryResponseV2Vo.setMaxSellingPrice(Objects.nonNull(productSolr.getMaximumSellingPrice()) ?
          productSolr.getMaximumSellingPrice() : Constants.DEFAULT_PRICE);
      productSummaryResponseV2Vo.setPickupPointCodes(productSolr.getPickupPointCodes());
      productSummaryResponseV2VoList.add(productSummaryResponseV2Vo);
    }
    return productSummaryResponseV2VoList;
  }

  public static List<HalalDashboardProductsResponseVo> toHalalDashboardProductsResponseVo(
      List<ProductSolr> productSolrList, String delimiter) {
    List<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVos = new ArrayList<>();
    for (ProductSolr productSolr : productSolrList) {
      HalalDashboardProductsResponseVo halalDashboardProductsResponseVo = new HalalDashboardProductsResponseVo();
      halalDashboardProductsResponseVo.setBrand(productSolr.getBrand());
      if (StringUtils.isNotBlank(productSolr.getMasterCatalog())) {
        halalDashboardProductsResponseVo.setCategoryCode(productSolr.getMasterCatalog().split(delimiter)[1]);
      }
      halalDashboardProductsResponseVo.setCurationStatus(
          CurationStatus.fromValue(productSolr.getCurationStatus()).name());
      halalDashboardProductsResponseVo.setProductCode(productSolr.getProductCode());
      halalDashboardProductsResponseVo.setProductName(productSolr.getProductName());
      halalDashboardProductsResponseVo.setProductSku(productSolr.getProductSku());
      halalDashboardProductsResponseVo.setProductCreationDate(productSolr.getCreatedDate());
      halalDashboardProductsResponseVos.add(halalDashboardProductsResponseVo);
    }
    return halalDashboardProductsResponseVos;
  }

  public static List<HalalDashboardProductsResponse> toHalalDashboardProductResponseList(
      List<HalalDashboardProductsResponseVo> halalDashboardProductsResponseVoList) {
    return halalDashboardProductsResponseVoList.stream().map(ResponseHelper::toHalalDashboardProductsResponse)
        .collect(Collectors.toList());
  }

  public static HalalDashboardProductsResponse toHalalDashboardProductsResponse(
      HalalDashboardProductsResponseVo halalDashboardProductsResponseVo) {
    HalalDashboardProductsResponse halalDashboardProductsResponse = new HalalDashboardProductsResponse();
    BeanUtils.copyProperties(halalDashboardProductsResponseVo, halalDashboardProductsResponse);
    if (CurationStatus.APPROVED.name().equals(halalDashboardProductsResponseVo.getCurationStatus())) {
      halalDashboardProductsResponse.setHalalProduct(true);
    }
    return halalDashboardProductsResponse;
  }


  public static List<ProductSummaryResponseV2> toProductSummaryResponseV2(
      List<ProductSummaryResponseV2Vo> productSummaryResponseV2VoList,
      Map<String, List<CategoryResponse>> categoryHierarchyMap) {
    List<ProductSummaryResponseV2> productSummaryResponseV2List = new ArrayList<>();
    for (ProductSummaryResponseV2Vo productSummaryResponseV2Vo : productSummaryResponseV2VoList) {
      List<CategoryResponse> categoryResponseList =
          categoryHierarchyMap.get(productSummaryResponseV2Vo.getCategoryCode());
      ProductSummaryResponseV2 productSummaryResponseV2 = new ProductSummaryResponseV2();
      BeanUtils.copyProperties(productSummaryResponseV2Vo, productSummaryResponseV2);
      if (CollectionUtils.isNotEmpty(categoryResponseList)) {
        productSummaryResponseV2.setCategoryName(categoryResponseList.get(0).getName());
        productSummaryResponseV2.setCategoryHierarchy(getCategoryHierarchyString(categoryResponseList));
      }
      productSummaryResponseV2List.add(productSummaryResponseV2);
    }
    return productSummaryResponseV2List;
  }

  private static void setMasterDataDetailsForSyncProduct(ItemSummaryListResponse itemSummaryListResponse, Product product,
      Item item, Map<String, Map<String, Map<String, String>>> productAttributeValueAndValueTypeMap) {
    itemSummaryListResponse.setProductName(product.getProductName());
    itemSummaryListResponse.setMainImageUrl(item.getMainImageUrl());
    itemSummaryListResponse.setMasterCategoryCode(item.getCategoryCode());
    itemSummaryListResponse.setItemName((item.getGeneratedItemName()));
    itemSummaryListResponse.setDefiningAttributes(getDefiningAttributesFromProduct(product, item.getItemSku()).stream()
        .map(productAttributeDetail -> new ProductAttributeDetailDTO(productAttributeDetail.getAttributeCode(),
            productAttributeDetail.getAttributeName(), productAttributeDetail.getAttributeValue(),
            getValueType(product.getProductSku(), productAttributeDetail.getAttributeCode(),
                productAttributeDetail.getAttributeValue(), productAttributeValueAndValueTypeMap), null, null))
        .collect(Collectors.toList()));
  }

  private static String getValueType(String productSku, String attributeCode, String attributeValue,
      Map<String, Map<String, Map<String, String>>> productAttributeValueAndValueTypeMap) {
    return Optional.ofNullable(productAttributeValueAndValueTypeMap)
        .map(productAttributeValueAndValueType -> productAttributeValueAndValueType.get(productSku))
        .map(attributeCodeAndValueMap -> attributeCodeAndValueMap.get(attributeCode))
        .map(valueAndValueTypeMap -> valueAndValueTypeMap.get(attributeValue)).orElse(null);
  }

  private static PreOrderVO checkPreOrder(Product product) throws ParseException {
    PreOrderVO preOrderVO = new PreOrderVO();
    if (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE.equals(product.getPreOrder().getIsPreOrder())) {
      BeanUtils.copyProperties(product.getPreOrder(), preOrderVO);
      if (Constants.WEEK.equals(product.getPreOrder().getPreOrderType())) {
        preOrderVO.setPreOrderType(Constants.DAYS);
        preOrderVO.setPreOrderValue(product.getPreOrder().getPreOrderValue() * Constants.NUM_OF_DAYS_IN_WEEK);
      } else if (Constants.DAYS.equals(product.getPreOrder().getPreOrderType())) {
        preOrderVO.setPreOrderType(Constants.DAYS);
        preOrderVO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
      } else if (checkPreOrderDateStatus(product.getPreOrder().getPreOrderDate(), Constants.DATE_FORMAT)) {
        preOrderVO.setIsPreOrder(true);
      }
    } else {
      preOrderVO.setIsPreOrder(false);
    }
    return preOrderVO;
  }

  public static boolean checkPreOrderDateStatus(Date preOrderDate, String dateFormat) {
    Date currentDate = new Date();
    currentDate = getDate(currentDate, dateFormat);
    if (Objects.isNull(currentDate)) {
      return true;
    }
    if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
      return false;
    }
    return true;
  }

  public static Date getDate(Date currentDate, String dateFormat) {
    try {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat(dateFormat);
      currentDate = simpleDateFormat.parse(simpleDateFormat.format(currentDate));
    } catch (Exception e) {
      log.error("Error while parsing PreOrderDate : {} with error : ", currentDate, e);
      return null;
    }
    return currentDate;
  }

  private static void setMasterDataItemAttributeValue(ProductItemDetailVO productItemDetailVO, Product product,
      Item item, Set<String> withoutMasterDataProductSkus) {
    List<MasterDataItemAttributeVO> masterDataItemAttributeVOList = new ArrayList<>();
    if (withoutMasterDataProductSkus.contains(productItemDetailVO.getProductSku())) {
      for (MasterDataItemAttributeValue masterDataItemAttributeValue : item.getMasterDataItem()
          .getMasterDataItemAttributeValues()) {
        if (!masterDataItemAttributeValue.isMarkForDelete() && masterDataItemAttributeValue.getMasterDataAttribute()
            .isVariantCreation()) {
          masterDataItemAttributeVOList.add(
              new MasterDataItemAttributeVO(masterDataItemAttributeValue.getAttributeValue(),
                  masterDataItemAttributeValue.getMasterDataAttribute().getAttributeCode(),
                  masterDataItemAttributeValue.getMasterDataAttribute().getAttributeName()));
        }
      }
    } else {
      for (ProductAttributeDetail productAttributeDetail : getDefiningAttributesFromProduct(product, item.getItemSku())) {
        masterDataItemAttributeVOList.add(new MasterDataItemAttributeVO(productAttributeDetail.getAttributeValue(),
            productAttributeDetail.getAttributeCode(), productAttributeDetail.getAttributeName()));
      }
    }
    productItemDetailVO.setMasterDataItemAttributes(masterDataItemAttributeVOList);
  }

  public static List<ProductAttributeDetail> getDefiningAttributesFromProduct(Product product, String itemSku) {
    return product.getDefiningAttributes().stream()
        .filter(productAttribute -> StringUtils.equals(productAttribute.getItemSku(), itemSku))
        .flatMap(productAttribute -> productAttribute.getProductAttributeDetails().stream())
        .collect(Collectors.toList());
  }

  private static void setPristineId(ProductItemDetailVO productItemDetailVO, Item item) {
    if (Objects.nonNull(item.getPristineDataItem())) {
      productItemDetailVO.setPristineId(item.getPristineDataItem().getPristineId());
      if (StringUtils.isEmpty(item.getPristineDataItem().getPristineModel()) && StringUtils.isEmpty(
          item.getPristineDataItem().getPristineBrand())) {
        productItemDetailVO.setProductName(item.getGeneratedItemName());
      } else {
        productItemDetailVO.setProductName(item.getPristineDataItem().getPristineProductName());
      }
    }
  }

  private static void setWarrantyInfo(ProductItemDetailVO productItemDetailVO, Product product) {
    if (CollectionUtils.isNotEmpty(product.getProductSpecialAttributes())) {
      String guaranteeAttributeName = StringUtils.EMPTY;
      String guaranteeDurationAttributeName = StringUtils.EMPTY;
      for (ProductSpecialAttribute productSpecialAttribute : product.getProductSpecialAttributes()) {
        if (Objects.nonNull(productSpecialAttribute) && StringUtils.isNotEmpty(
            productSpecialAttribute.getAttributeName())) {
          if (Constants.GUARANTEE_ATTRIBUTE_NAME.contains(productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeAttributeName = productSpecialAttribute.getAttributeValue();
          } else if (Constants.GUARANTEE_DURATION_ATTRIBUTE_NAME.contains(
              productSpecialAttribute.getAttributeName().toLowerCase())) {
            guaranteeDurationAttributeName = productSpecialAttribute.getAttributeValue();
          }
        }
      }
      if (StringUtils.isNotEmpty(guaranteeAttributeName)) {
        if (StringUtils.isNotEmpty(guaranteeDurationAttributeName)) {
          StringBuilder warrantyInfo = new StringBuilder(guaranteeAttributeName).append(Constants.SPACE)
              .append(guaranteeDurationAttributeName);
          productItemDetailVO.setWarrantyInfo(warrantyInfo.toString());
        } else {
          productItemDetailVO.setWarrantyInfo(guaranteeAttributeName);
        }
      }
    }
  }

  private static String getCategoryCode(Product product, Set<String> withoutMasterDataProductSkus) {
    return withoutMasterDataProductSkus.contains(product.getProductSku()) ?
        product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode() :
        product.getCategoryCode();
  }

  private static List<String> getDocumentTypeFromItemCatalogs(Product product,Set<String> withoutMasterDataProductSkus) {
    String categoryCode = getCategoryCode(product, withoutMasterDataProductSkus);
    String documentType =
        product.getItemCatalogs().stream().flatMap(itemCatalogVO -> itemCatalogVO.getItemCategories().stream())
            .filter(itemCategoryVO -> categoryCode.equals(itemCategoryVO.getProductCategoryCode())).findFirst()
            .orElse(new ItemCategoryVO()).getDocumentType();
    return StringUtils.isNotEmpty(documentType) ? Arrays.asList(documentType.split(Constants.COMMA_DELIMITER)) : null;
  }

  private static ItemViewConfigDTO convertToProductViewConfigResponse(ItemViewConfig itemViewConfig) {
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setBuyable(itemViewConfig.isBuyable());
    itemViewConfigDTO.setChannel(itemViewConfig.getChannel());
    itemViewConfigDTO.setDiscoverable(itemViewConfig.isDiscoverable());
    if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
      ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
      itemBuyableScheduleDTO.setBuyable(itemViewConfig.getItemBuyableSchedules().isBuyable());
      itemBuyableScheduleDTO.setStartDateTime(itemViewConfig.getItemBuyableSchedules().getStartDateTime());
      itemBuyableScheduleDTO.setEndDateTime(itemViewConfig.getItemBuyableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
    }
    if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
      ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
      itemDiscoverableScheduleDTO.setDiscoverable(itemViewConfig.getItemDiscoverableSchedules().isDiscoverable());
      itemDiscoverableScheduleDTO.setStartDateTime(itemViewConfig.getItemDiscoverableSchedules().getStartDateTime());
      itemDiscoverableScheduleDTO.setEndDateTime(itemViewConfig.getItemDiscoverableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    }
    return itemViewConfigDTO;
  }

  private static Set<ItemViewConfigDTO> convertToProductViewConfigSetResponse(ItemPickupPoint itemPickupPoint,
      Map<String, ItemViewConfig> offlineItemIdAndViewConfigOriginalMap) {
    log.info("Setting the viewConfigDto. itemPickupPoint : {} , offlineItemIdAndViewConfigOriginalMap : {} ",
        itemPickupPoint, offlineItemIdAndViewConfigOriginalMap);
    return Collections.singleton(
        convertToProductOriginalViewConfigResponse(itemPickupPoint.getItemViewConfig().stream().findFirst().get(),
            offlineItemIdAndViewConfigOriginalMap.get(
                CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()))));
  }

  private static ItemViewConfigDTO convertToProductOriginalViewConfigResponse(ItemViewConfig itemViewConfig,
      ItemViewConfig itemViewConfigOriginal) {
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setBuyable(itemViewConfig.isBuyable());
    itemViewConfigDTO.setChannel(itemViewConfig.getChannel());
    itemViewConfigDTO.setDiscoverable(itemViewConfig.isDiscoverable());
    if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules())) {
      ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
      itemBuyableScheduleDTO.setBuyable(itemViewConfig.getItemBuyableSchedules().isBuyable());
      itemBuyableScheduleDTO.setStartDateTime(itemViewConfig.getItemBuyableSchedules().getStartDateTime());
      itemBuyableScheduleDTO.setEndDateTime(itemViewConfig.getItemBuyableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemBuyableSchedules(itemBuyableScheduleDTO);
    }
    if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules())) {
      ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
      itemDiscoverableScheduleDTO.setDiscoverable(itemViewConfig.getItemDiscoverableSchedules().isDiscoverable());
      itemDiscoverableScheduleDTO.setStartDateTime(itemViewConfig.getItemDiscoverableSchedules().getStartDateTime());
      itemDiscoverableScheduleDTO.setEndDateTime(itemViewConfig.getItemDiscoverableSchedules().getEndDateTime());
      itemViewConfigDTO.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);
    }
    itemViewConfigDTO.setBuyableOriginal(itemViewConfigOriginal.isBuyable());
    itemViewConfigDTO.setDiscoverableOriginal(itemViewConfigOriginal.isDiscoverable());
    return itemViewConfigDTO;
  }

  private static PriceDTO convertToItemPriceResponse(Price price) {
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setCurrency(price.getCurrency());
    priceDTO.setChannel(price.getChannel());
    priceDTO.setOfferPrice(price.getOfferPrice());
    priceDTO.setListPrice(price.getListPrice());
    priceDTO.setLastUpdatedBy(price.getLastUpdatedBy());
    priceDTO.setLastUpdatedDate(price.getLastUpdatedDate());
    priceDTO.setOriginalSellingPrice(price.getOfferPrice());
    if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
      priceDTO.setListOfDiscountPrices(
          price.getListOfDiscountPrices().stream().filter(Objects::nonNull).map(discountPrice -> convertToDiscountPriceDTO(discountPrice))
              .collect(Collectors.toList()));
    }
    if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
      priceDTO.setMerchantPromoDiscountPrice(convertToDiscountPriceDTO(price.getMerchantPromoDiscountPrice()));
    }
    return priceDTO;
  }

  private static DiscountPriceDTO convertToDiscountPriceDTO(DiscountPrice discountPrice) {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
      discountPriceDTO.setDiscountPrice(Optional.ofNullable(discountPrice.getDiscountPrice()).orElse(null));
      discountPriceDTO.setStartDateTime(Optional.ofNullable(discountPrice.getStartDateTime()).orElse(null));
      discountPriceDTO.setEndDateTime(Optional.ofNullable(discountPrice.getEndDateTime()).orElse(null));
      discountPriceDTO.setAdjustmentName(Optional.ofNullable(discountPrice.getAdjustmentName()).orElse(null));
      discountPriceDTO.setCampaignCode(Optional.ofNullable(discountPrice.getCampaignCode()).orElse(null));
      discountPriceDTO.setPriority(discountPrice.getPriority());
      if (Objects.nonNull(discountPrice.getAdjustmentType())) {
        discountPriceDTO.setAdjustmentType(AdjustmentTypeEnum.valueOf(discountPrice.getAdjustmentType().toString()));
      }
    return discountPriceDTO;
  }

  public static List<BusinessPartnerPickupPointResponse> toBusinessPartnerPickupPointResponse(
      List<BusinessPartnerPickupPoint> businessPartnerPickupPointList) {
    List<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList = new ArrayList<>();
    for (BusinessPartnerPickupPoint businessPartnerPickupPoint : businessPartnerPickupPointList) {
      BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse = new BusinessPartnerPickupPointResponse();
      BeanUtils.copyProperties(businessPartnerPickupPoint, businessPartnerPickupPointResponse);
      if (Objects.nonNull(businessPartnerPickupPoint.getGeolocation())) {
        GeoLocationResponse geoLocationResponse = new GeoLocationResponse();
        BeanUtils.copyProperties(businessPartnerPickupPoint.getGeolocation(), geoLocationResponse);
        businessPartnerPickupPointResponse.setGeolocation(geoLocationResponse);
      }
      businessPartnerPickupPointResponseList.add(businessPartnerPickupPointResponse);
    }
    return businessPartnerPickupPointResponseList;
  }

  public static List<ItemPickupPointListingResponse> toItemPickupPointListingResponse(
      List<ItemPickupPoint> itemPickupPointList, Map<String, Product> productMap, Map<String, Item> itemMap,
      Map<String, BusinessPartnerPickupPoint> businessPartnerPickupPointMap,
      boolean replaceOfferPriceWithMerchantPromoPrice, Map<String, Double> itemPickupAndOriginalPriceMap,
      boolean replaceMerchantPromoDiscountBasedOnEndDate,
      boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull,
      String fetchViewConfigByChannel,
      boolean cncForWarehouseFeatureSwitch, boolean populateNullForWholesalePriceActivated) {
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Product product = productMap.get(itemPickupPoint.getProductSku());
      if (Objects.isNull(item) || Objects.isNull(product)) {
        continue;
      }
      ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
      itemPickupPointListingResponse.setProductCode(product.getProductCode());
      itemPickupPointListingResponse.setProductSku(product.getProductSku());
      itemPickupPointListingResponse.setOnline(product.isOnline());
      itemPickupPointListingResponse.setArchiveFlagAtL3Level(product.isArchived());
      itemPickupPointListingResponse.setCncActiveAtL3Level(product.isCncActivated());
      itemPickupPointListingResponse.setFbbActiveAtL3Level(product.isFbbActivated());
      itemPickupPointListingResponse.setB2cActivatedAtL3Level(product.getB2cActivated());
      itemPickupPointListingResponse.setB2bActivatedAtL3Level(product.isB2bActivated());
      itemPickupPointListingResponse.setSkuCode(item.getItemCode());
      itemPickupPointListingResponse.setItemSku(itemPickupPoint.getItemSku());
      itemPickupPointListingResponse.setMerchantSku(itemPickupPoint.getMerchantSku());
      itemPickupPointListingResponse.setMerchantCode(itemPickupPoint.getMerchantCode());
      itemPickupPointListingResponse.setFbbActive(itemPickupPoint.isFbbActivated());
      itemPickupPointListingResponse.setItemName(item.getGeneratedItemName());
      itemPickupPointListingResponse.setCategoryCode(item.getCategoryCode());
      itemPickupPointListingResponse.setPickUpPointCode(itemPickupPoint.getPickupPointCode());
      itemPickupPointListingResponse.setPickUpPointName(
          Optional.ofNullable(businessPartnerPickupPointMap.get(itemPickupPoint.getPickupPointCode()))
              .orElse(new BusinessPartnerPickupPoint()).getName());
      itemPickupPointListingResponse.setPickupPointCncActive(
          Optional.ofNullable(businessPartnerPickupPointMap.get(itemPickupPoint.getPickupPointCode()))
              .orElse(new BusinessPartnerPickupPoint()).isCncActivated());
      itemPickupPointListingResponse.setPickupPointDeliveryActive(
          businessPartnerPickupPointMap.getOrDefault(itemPickupPoint.getPickupPointCode(),
              new BusinessPartnerPickupPoint()).isDelivery());
      itemPickupPointListingResponse.setProductScore(product.getProductScore().getTotalScore());
      itemPickupPointListingResponse.setItemNumber(Integer.valueOf(
          item.getItemSku().substring(item.getItemSku().lastIndexOf(Constants.DASH) + 1)));
      itemPickupPointListingResponse.setVersion(itemPickupPoint.getVersion());
      itemPickupPointListingResponse.setLateFulfillment(item.isLateFulfillment());
      itemPickupPointListingResponse.setArchived(item.isArchived());
      itemPickupPointListingResponse.setFreeSample(item.isFreeSample());
      itemPickupPointListingResponse.setSuspended(product.isSuspended());
      itemPickupPointListingResponse.setOff2OnActiveFlag(item.isOff2OnChannelActive());
      itemPickupPointListingResponse.setSuspended(product.isSuspended());
      itemPickupPointListingResponse.setOff2OnActiveFlag(item.isOff2OnChannelActive());
      itemPickupPointListingResponse.setEnableEdit(!product.isForceReview());
      itemPickupPointListingResponse.setProductType(product.getProductType());
      itemPickupPointListingResponse.setDimensionsMissing(Optional.ofNullable(product.getDimensionsMissing())
          .orElse(CollectionUtils.isNotEmpty(product.getMissingFields())));
      itemPickupPointListingResponse.setDistribution(itemPickupPoint.isDistribution());
      itemPickupPointListingResponse.setMissingFields(product.getMissingFields());
      populateMissingFields(itemPickupPointListingResponse, product);
      itemPickupPointListingResponse.setWholesalePromoActivated(
          Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
              .contains(Constants.WHOLESALE));
      if (populateNullForWholesalePriceActivated) {
        if (itemPickupPoint.isWholesalePriceExists()) {
          itemPickupPointListingResponse.setWholesalePriceActivated(
              Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
                  .contains(Constants.WHOLESALE_PRICE));
        }
      } else {
        itemPickupPointListingResponse.setWholesalePriceActivated(
            Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
                .contains(Constants.WHOLESALE_PRICE));
      }
      itemPickupPointListingResponse.setWholesalePriceConfigEnabled(itemPickupPoint.isWholesalePriceExists());
      itemPickupPointListingResponse.setFlashSaleActive(itemPickupPoint.isFlashSaleActive());
      itemPickupPointListingResponse.setCncActive(
          setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
              cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
              ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
      itemPickupPointListingResponse.setFbbActive(itemPickupPoint.isFbbActivated());
      itemPickupPointListingResponse.setProductSyncStatus(product.isSynchronized());
      itemPickupPointListingResponse.setProductType(product.getProductType());
      itemPickupPointListingResponse.setPromoTypes(setPromoTypes(itemPickupPoint));
      itemPickupPointListingResponse.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
      itemPickupPointListingResponse.setPrices(setPriceResponse(itemPickupPoint, replaceOfferPriceWithMerchantPromoPrice));
      itemPickupPointListingResponse.setOriginalSellingPrice(itemPickupAndOriginalPriceMap.getOrDefault(
          CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(),
              itemPickupPoint.getPickupPointCode()), Constants.DEFAULT_PRICE));
      itemPickupPointListingResponse.setViewConfigs(setViewConfigResponseForL5ListingResponse(itemPickupPoint, fetchViewConfigByChannel, cncForWarehouseFeatureSwitch));
      itemPickupPointListingResponse.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
      itemPickupPointListingResponse.setMerchantPromoDiscountActivated(
          Optional.ofNullable(itemPickupPoint.getPrice()).orElse(new HashSet<>()).stream()
              .anyMatch(price -> Objects.nonNull(price.getMerchantPromoDiscountPrice())));
      updateMerchantPromoDiscountFlags(itemPickupPoint, itemPickupPointListingResponse,
          replaceMerchantPromoDiscountBasedOnEndDate, ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull);
      itemPickupPointListingResponse.setPriceEditDisabled(isPriceEditDisabled(itemPickupPoint));
      itemPickupPointListingResponse.setPriceEditDisabledReason(CommonUtil.getPriceEditDisabledReasons(itemPickupPoint));
      itemPickupPointListingResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
      itemPickupPointListingResponse.setB2bFields(setB2BResponse(itemPickupPoint));
      itemPickupPointListingResponse.setMainImageUrl(item.getMainImageUrl());
      itemPickupPointListingResponse.setBrand(product.getBrand());
      populateAttributesMap(product, item, itemPickupPointListingResponse);
      itemPickupPointListingResponseList.add(itemPickupPointListingResponse);
    }
    return itemPickupPointListingResponseList;
  }

  public static void populateMissingFields(ItemPickupPointListingResponse itemPickupPointListingResponse,
      Product product) {
    if (Boolean.TRUE.equals(product.getDimensionsMissing())) {
      Set<String> missingFields =
          Optional.ofNullable(itemPickupPointListingResponse.getMissingFields()).orElse(new HashSet<>());
      missingFields.add(Constants.DIMENSIONS_MISSING);
      itemPickupPointListingResponse.setMissingFields(missingFields);
    }
  }

  private static void updateMerchantPromoDiscountFlags(ItemPickupPoint itemPickupPoint,
      ItemPickupPointListingResponse itemPickupPointListingResponse,
      boolean replaceMerchantPromoDiscountBasedOnEndDate,
      boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull) {
    if (replaceMerchantPromoDiscountBasedOnEndDate) {
      boolean merchantPromoDiscountActive = CommonUtil.isMerchantPromoDiscountActive(itemPickupPoint,
          ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull);
      itemPickupPointListingResponse.setMerchantPromoDiscount(merchantPromoDiscountActive);
      itemPickupPoint.setMerchantPromoDiscount(merchantPromoDiscountActive);
      itemPickupPointListingResponse.setMerchantPromoDiscountActivated(CommonUtil
          .isMerchantPromoDiscountActivated(itemPickupPoint));
    }
  }

  public static void updateMerchantPromoDiscountFlags(ItemPickupPoint itemPickupPoint,
      ItemL4SummaryResponse itemL4SummaryResponse, boolean replaceMerchantPromoDiscountBasedOnEndDate,
      boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull) {
    if (replaceMerchantPromoDiscountBasedOnEndDate) {
      boolean merchantPromoDiscountActive = CommonUtil.isMerchantPromoDiscountActive(itemPickupPoint,
          ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull);
      itemL4SummaryResponse.setMerchantPromoDiscount(merchantPromoDiscountActive);
      itemPickupPoint.setMerchantPromoDiscount(merchantPromoDiscountActive);
      itemL4SummaryResponse.setMerchantPromoDiscountActivated(CommonUtil
          .isMerchantPromoDiscountActivated(itemPickupPoint));
    }
  }

  private static void populateAttributesMap(Product product, Item item,
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    if (CollectionUtils.isNotEmpty(product.getDefiningAttributes())) {
      Map<String, List<ProductAttributeDetail>> productAttributesMap = product.getDefiningAttributes().stream()
          .filter(productAttribute -> StringUtils.isNotBlank(productAttribute.getItemSku()))
          .filter(productAttribute -> CollectionUtils.isNotEmpty(productAttribute.getProductAttributeDetails()))
          .collect(Collectors.toMap(ProductAttribute::getItemSku, ProductAttribute::getProductAttributeDetails));
      if (productAttributesMap.containsKey(item.getItemSku())) {
        List<ProductAttributeDetail> productAttributeDetails = productAttributesMap.get(item.getItemSku());
        Map<String, String> attributeMap = productAttributeDetails.stream()
            .filter(productAttributeDetail -> StringUtils.isNotBlank(productAttributeDetail.getAttributeCode()))
            .filter(productAttributeDetail -> StringUtils.isNotBlank(productAttributeDetail.getAttributeValue()))
            .collect(
                Collectors.toMap(ProductAttributeDetail::getAttributeCode, ProductAttributeDetail::getAttributeValue));
        itemPickupPointListingResponse.setAttributesMap(attributeMap);
      }
    }
  }

  public static List<ItemPickupPointListingResponse> toItemPickupPointListingResponseWithoutL5(Product product,
      Item item) {
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList = new ArrayList<>();
    ItemPickupPointListingResponse itemPickupPointListingResponse = new ItemPickupPointListingResponse();
    itemPickupPointListingResponse.setProductCode(product.getProductCode());
    itemPickupPointListingResponse.setProductSku(product.getProductSku());
    itemPickupPointListingResponse.setOnline(product.isOnline());
    itemPickupPointListingResponse.setArchiveFlagAtL3Level(product.isArchived());
    itemPickupPointListingResponse.setFbbActiveAtL3Level(product.isFbbActivated());
    itemPickupPointListingResponse.setCncActiveAtL3Level(product.isCncActivated());
    itemPickupPointListingResponse.setB2cActivatedAtL3Level(product.getB2cActivated());
    itemPickupPointListingResponse.setB2bActivatedAtL3Level(product.isB2bActivated());
    itemPickupPointListingResponse.setSkuCode(item.getItemCode());
    itemPickupPointListingResponse.setItemSku(item.getItemSku());
    itemPickupPointListingResponse.setMerchantSku(item.getMerchantSku());
    itemPickupPointListingResponse.setMerchantCode(product.getMerchantCode());
    itemPickupPointListingResponse.setItemName(item.getGeneratedItemName());
    itemPickupPointListingResponse.setCategoryCode(product.getCategoryCode());
    itemPickupPointListingResponse.setProductScore(Optional.ofNullable(product.getProductScore()).map(
      ProductScore::getTotalScore).orElse(0.0));
    itemPickupPointListingResponse.setItemNumber(
        Integer.valueOf(item.getItemSku().substring(item.getItemSku().lastIndexOf(Constants.DASH) + 1)));
    itemPickupPointListingResponse.setVersion(product.getVersion());
    itemPickupPointListingResponse.setLateFulfillment(item.isLateFulfillment());
    itemPickupPointListingResponse.setArchived(item.isArchived());
    itemPickupPointListingResponse.setFreeSample(item.isFreeSample());
    itemPickupPointListingResponse.setSuspended(product.isSuspended());
    itemPickupPointListingResponse.setOff2OnActiveFlag(item.isOff2OnChannelActive());
    itemPickupPointListingResponse.setSuspended(product.isSuspended());
    itemPickupPointListingResponse.setOff2OnActiveFlag(item.isOff2OnChannelActive());
    itemPickupPointListingResponse.setEnableEdit(!product.isForceReview());
    itemPickupPointListingResponse.setProductSyncStatus(product.isSynchronized());
    itemPickupPointListingResponse.setProductType(product.getProductType());
    itemPickupPointListingResponse.setBrand(product.getBrand());
    itemPickupPointListingResponse.setDimensionsMissing(product.getDimensionsMissing());
    itemPickupPointListingResponse.setMissingFields(product.getMissingFields());
    itemPickupPointListingResponseList.add(itemPickupPointListingResponse);
    return itemPickupPointListingResponseList;
  }

  private static boolean isPriceEditDisabled(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.isMerchantPromoDiscount() || itemPickupPoint.getPrice().stream().anyMatch(
        price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices().stream()
            .anyMatch(discountPrice -> (org.apache.commons.lang3.StringUtils.isNotBlank(discountPrice.getCampaignCode())
                && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  public static List<ItemPriceResponse> toItemPriceResponse(String pickupPointCode, List<Item> items,
      Map<String, ItemPickupPoint> itemPickupPointMap, Map<String, Set<Price>> priceMap) {
    List<ItemPriceResponse> itemPriceResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        ItemViewConfig itemViewConfig = Optional.ofNullable(
                Optional.ofNullable(itemPickupPointMap.get(item.getItemSku()))
                    .orElse(ItemPickupPoint.builder().itemViewConfig(new HashSet<>()).build()).getItemViewConfig())
            .orElse(new HashSet<>()).stream().findFirst().orElse(new ItemViewConfig());
        Price price = Optional.ofNullable(priceMap.get(item.getItemSku())).orElse(new HashSet<>()).stream().findFirst()
            .orElse(new Price());
        itemPriceResponses.add(new ItemPriceResponse.ItemPriceResponseBuilder().setItemSku(item.getItemSku())
            .setOfferPrice(price.getOfferPrice()).setListPrice(price.getListPrice())
            .setBuyable(itemViewConfig.isBuyable()).setMerchantCode(item.getMerchantCode())
            .setPickupPointCode(pickupPointCode).build());
      }
    }
    return itemPriceResponses;
  }

  public static List<ItemPickupPointPriceResponse> toPriceDetailResponse(
    List<ItemPickupPoint> response, boolean cncForWarehouseFeatureSwitch) {
    List<ItemPickupPointPriceResponse> itemPriceDetailResponses = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(response)) {
      for (ItemPickupPoint itemPickupPoint : response) {
        List<DiscountPrice> discountPrices = new ArrayList<>();
        ItemPickupPointPriceResponse itemPriceDetailResponse = new ItemPickupPointPriceResponse();
        BeanUtils.copyProperties(itemPickupPoint, itemPriceDetailResponse);
        setDiscountPriceList(
          Optional.ofNullable(itemPickupPoint.getPrice()).orElse(new HashSet<>()), discountPrices);
        itemPriceDetailResponse.setListOfDiscountPrices(
          discountPrices.stream().filter(Objects::nonNull)
            .map(ResponseHelper::convertToDiscountPriceDTO)
            .collect(Collectors.toList()));
        itemPriceDetailResponse.setListPrice(
          Objects.requireNonNull(itemPickupPoint.getPrice().stream().findFirst().orElse(null))
            .getListPrice());
        itemPriceDetailResponse.setOfferPrice(
          Objects.requireNonNull(itemPickupPoint.getPrice().stream().findFirst().orElse(null))
            .getOfferPrice());
        itemPriceDetailResponse.setDiscoverable(
          Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream()
            .map(ItemViewConfig::isDiscoverable).findFirst().orElse(false));
        itemPriceDetailResponse.setBuyable(
          Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream()
            .map(ItemViewConfig::isBuyable).findFirst().orElse(false));
        itemPriceDetailResponse.setLastUpdatedDate(
          Optional.ofNullable(itemPickupPoint.getPrice().iterator().next().getLastUpdatedDate())
            .orElse(itemPickupPoint.getUpdatedDate()));
        itemPriceDetailResponse.setLastUpdatedBy(
          Optional.ofNullable(itemPickupPoint.getPrice().iterator().next().getLastUpdatedBy())
            .orElse(itemPickupPoint.getUpdatedBy()));
        itemPriceDetailResponse.setCncActive(
            setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
                cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
                ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
        if (cncForWarehouseFeatureSwitch) {
          ItemViewConfig itemViewConfig = fetchItemViewConfigByChannel(itemPickupPoint.getAllItemViewConfigs(), Constants.CNC);
          itemPriceDetailResponse.setCncBuyable(itemViewConfig.isBuyable());
          itemPriceDetailResponse.setCncDiscoverable(itemViewConfig.isDiscoverable());
        }
        itemPriceDetailResponses.add(itemPriceDetailResponse);
      }
    }
    return itemPriceDetailResponses;
  }

  private static ItemViewConfig fetchItemViewConfigByChannel(
      Set<ItemViewConfig> allItemViewConfigs, String channel) {
    return org.apache.commons.collections4.CollectionUtils.emptyIfNull(allItemViewConfigs).stream()
        .filter(itemViewConfig -> itemViewConfig.getChannel().equals(channel)).findFirst()
        .orElse(new ItemViewConfig());
  }

  private static void setDiscountPriceList(Set<Price> price, List<DiscountPrice> discountPrices) {
    if (CollectionUtils.isNotEmpty(price.iterator().next().getListOfDiscountPrices())) {
      List<DiscountPrice> listOfDiscountPrices = price.iterator().next().getListOfDiscountPrices();
      discountPrices.addAll(listOfDiscountPrices);
    }
  }

  public static BasicItemDTO toBasicItemDTO(Item item, ItemPickupPoint itemPickupPoint) {
    BasicItemDTO basicItemDTO = new BasicItemDTO();
    basicItemDTO.setMerchantCode(item.getMerchantCode());
    basicItemDTO.setProductSku(item.getProductSku());
    basicItemDTO.setItemSku(item.getItemSku());
    basicItemDTO.setItemCode(item.getItemCode());
    basicItemDTO.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    basicItemDTO.setPrice(itemPickupPoint.getPrice().stream().map(ResponseHelper::convertToItemPriceResponse)
        .collect(Collectors.toSet()));
    basicItemDTO.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs().stream()
        .map(ResponseHelper::convertToProductViewConfigResponse).collect(Collectors.toSet()));
    basicItemDTO.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    addBasicMasterDataItem(basicItemDTO, item);
    return basicItemDTO;
  }

  private static void addBasicMasterDataItem(BasicItemDTO basicItemDTO, Item item){
    double shippingWeight = item.getShippingWeight();
    double height = item.getHeight();
    double length = item.getLength();
    double weight = item.getWeight();
    double width = item.getWidth();
    Integer dangerousLevel = item.getDangerousLevel();
    if (!item.isSynchronized() && Objects.nonNull(item.getMasterDataItem())) {
      dangerousLevel = item.getMasterDataItem().getDangerousLevel();
      height = Optional.ofNullable(item.getMasterDataItem().getItemHeight()).orElse(0.00);
      length = Optional.ofNullable(item.getMasterDataItem().getItemLength()).orElse(0.00);
      weight = Optional.ofNullable(item.getMasterDataItem().getItemWeight()).orElse(0.00);
      width = Optional.ofNullable(item.getMasterDataItem().getItemWidth()).orElse(0.00);
      if (Objects.nonNull(item.getMasterDataItem().getItemDeliveryWeight())) {
        shippingWeight = item.getMasterDataItem().getItemDeliveryWeight();
      }
    }
    if (Objects.nonNull(dangerousLevel)) {
      basicItemDTO.setMasterDataItem(new BasicMasterDataItemDTO(shippingWeight, height, length, weight, width, dangerousLevel));
    } else {
      basicItemDTO.setMasterDataItem(new BasicMasterDataItemDTO(shippingWeight, height, length, weight, width));
    }
  }

  public static List<ItemBasicDetailV2Response> toItemBasicDetailV2Response(List<Item> itemList,
      HashMap<String, ProductDataResponse> productSkuToProductDataMap,
      Map<String, List<CategoryResponse>> categoryHierarchyMap, Map<String, Item> bundleItemSkuToItemMap,
      Map<String, Boolean> itemSkuFbbMap, Map<String, Boolean> productSkuAndSharedProductMap) {
    List<ItemBasicDetailV2Response> itemBasicDetailV2Responses = new ArrayList<>();
    for (Item item : itemList) {
      ItemBasicDetailV2Response itemBasicDetailV2Response =
          ItemBasicDetailV2Response.builder().merchantCode(item.getMerchantCode()).merchantSku(item.getMerchantSku())
              .itemSku(item.getItemSku()).productSku(item.getProductSku())
              .generatedItemName(item.getGeneratedItemName()).itemCode(item.getItemCode())
              .mainImageUrl(item.getMainImageUrl())
              .categoryHierarchy(toCategoryDataResponse(categoryHierarchyMap, item.getCategoryCode()))
              .categoryCode(item.getCategoryCode()).length(item.getLength()).width(item.getWidth())
              .height(item.getHeight()).weight(item.getWeight()).shippingWeight(item.getShippingWeight())
              .dangerousLevel(item.getDangerousLevel())
              .bundleRecipeList(toBundleRecipeList(item, bundleItemSkuToItemMap, productSkuAndSharedProductMap))
              .productData(toProductDataResponse(productSkuToProductDataMap, item.getProductSku()))
              .markForDelete(item.isMarkForDelete()).permanentDelete(item.isPermanentDelete())
              .archived(item.isArchived()).categoryName(getCategoryName(categoryHierarchyMap, item.getCategoryCode()))
              .fbbActivated(itemSkuFbbMap.getOrDefault(item.getItemSku(), false)).brand(item.getBrand())
              .sharedProduct(productSkuAndSharedProductMap.getOrDefault(item.getProductSku(), false))
              .subscribable(item.isSubscribable()).build();
      itemBasicDetailV2Responses.add(itemBasicDetailV2Response);
    }
    return itemBasicDetailV2Responses;
  }

  public static List<CategoryDataResponse> toCategoryDataResponse(
      Map<String, List<CategoryResponse>> categoryHierarchyMap, String categoryCode) {
    List<CategoryResponse> categoryResponses =
        Optional.ofNullable(categoryHierarchyMap.get(categoryCode)).orElse(new ArrayList<>());
    AtomicInteger level = new AtomicInteger(categoryResponses.size());
    return Optional.ofNullable(categoryHierarchyMap.get(categoryCode)).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).map(
            categoryResponse -> new CategoryDataResponse(categoryResponse.getCategoryCode(), categoryResponse.getName(),
                categoryResponse.getNameEnglish(), categoryResponse.isActivated(), level.getAndDecrement()))
        .collect(Collectors.toList());
  }

  private static String getCategoryName(Map<String, List<CategoryResponse>> categoryHierarchyMap, String categoryCode) {
    return Optional.ofNullable(categoryHierarchyMap.get(categoryCode)).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).findFirst().map(CategoryResponse::getName).orElse(null);
  }

  private static ProductDataResponse toProductDataResponse(
      HashMap<String, ProductDataResponse> productSkuToProductDataMap, String productSku) {
    return Optional.ofNullable(productSkuToProductDataMap.get(productSku)).orElse(new ProductDataResponse());
  }

  private static List<BundleRecipeV2Response> toBundleRecipeList(Item item, Map<String, Item> bundleItemSkuToItemMap,
      Map<String, Boolean> productSkuAndSharedProductMap) {
    if (Objects.nonNull(item.getBundleRecipe()) && !bundleItemSkuToItemMap.isEmpty()) {
      return item.getBundleRecipe().stream()
          .map(bundleRecipe -> toBundleChildRecipe(bundleRecipe, bundleItemSkuToItemMap, productSkuAndSharedProductMap)).collect(Collectors.toList());
    } else {
      return new ArrayList<>();
    }
  }

  private static BundleRecipeV2Response toBundleChildRecipe(BundleRecipe bundleRecipe,
      Map<String, Item> bundleItemSkuToItemMap, Map<String, Boolean> productSkuAndSharedProductMap) {
    Item bundleItem = bundleItemSkuToItemMap.getOrDefault(bundleRecipe.getItemSku(), new Item());
    BundleRecipeV2Response bundleRecipeResponse = new BundleRecipeV2Response();
    bundleRecipeResponse.setMerchantCode(bundleItem.getMerchantCode());
    bundleRecipeResponse.setQuantity(bundleRecipe.getQuantity());
    bundleRecipeResponse.setItemCode(bundleItem.getItemCode());
    bundleRecipeResponse.setMainImageUrl(bundleItem.getMainImageUrl());
    bundleRecipeResponse.setGeneratedItemName(bundleItem.getGeneratedItemName());
    bundleRecipeResponse.setItemSku(bundleItem.getItemSku());
    bundleRecipeResponse.setProductSku(bundleItem.getProductSku());
    bundleRecipeResponse.setProductStatus(getProductStatus(bundleItem));
    bundleRecipeResponse.setSharedProduct(
        productSkuAndSharedProductMap.getOrDefault(bundleItem.getProductSku(), false));
    return bundleRecipeResponse;
  }

  public static String getProductStatus(Item item) {
    if (item.isMarkForDelete() || item.isArchived()) {
      return ProductStatus.INACTIVE.name();
    }
    return ProductStatus.ACTIVE.name();
  }

  public static List<ProductL5DetailResponse> toProductDetailL5Response(
    List<ItemPickupPoint> itemPickupPointList, Map<String, Product> productMap, Map<String, Item> itemMap,
      boolean cncForWarehouseFeatureSwitch, String fetchViewConfigByChannel) {
    List<ProductL5DetailResponse> productL5DetailResponsesList = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      Product product = productMap.get(itemPickupPoint.getProductSku());
      if (Objects.isNull(item) || Objects.isNull(product)) {
        continue;
      }
      ProductL5DetailResponse productL5DetailResponse = new ProductL5DetailResponse();
      productL5DetailResponse.setProductCode(product.getProductCode());
      productL5DetailResponse.setProductSku(product.getProductSku());
      productL5DetailResponse.setItemSku(itemPickupPoint.getItemSku());
      productL5DetailResponse.setMerchantSku(itemPickupPoint.getMerchantSku());
      productL5DetailResponse.setMerchantCode(itemPickupPoint.getMerchantCode());
      productL5DetailResponse.setFbbActivated(itemPickupPoint.isFbbActivated());
      productL5DetailResponse.setItemName(item.getGeneratedItemName());
      productL5DetailResponse.setSkuCode(item.getItemCode());
      productL5DetailResponse.setUpcCode(item.getUpcCode());
      productL5DetailResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      productL5DetailResponse.setArchived(product.isArchived());
      productL5DetailResponse.setFreeSample(product.isFreeSample());
      productL5DetailResponse.setEnableEdit(!product.isForceReview());
      productL5DetailResponse.setCategoryCode(item.getCategoryCode());
      productL5DetailResponse.setFbbActivated(itemPickupPoint.isFbbActivated());
      productL5DetailResponse.setProductSyncStatus(product.isSynchronized());
      productL5DetailResponse.setPrices(setPriceResponse(itemPickupPoint, true));
      productL5DetailResponse.setViewConfigs(
          setViewConfigResponse(itemPickupPoint, fetchViewConfigByChannel,
              cncForWarehouseFeatureSwitch));
      productL5DetailResponse.setCncActive(
          setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
              cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
              ItemViewConfig::getChannel, ItemViewConfig::isBuyable));
      productL5DetailResponse.setPriceEditDisabled(isPriceEditDisabled(itemPickupPoint));
      productL5DetailResponse.setPriceEditDisabledReason(CommonUtil.getPriceEditDisabledReasons(itemPickupPoint));
      productL5DetailResponse.setMarkForDelete(product.isMarkForDelete());
      productL5DetailResponse.setSuspended(product.isSuspended());
      productL5DetailResponse.setBrand(product.getBrand());
      productL5DetailResponse.setL5MarkForDelete(itemPickupPoint.isMarkForDelete());
      productL5DetailResponsesList.add(productL5DetailResponse);
    }
    return productL5DetailResponsesList;
  }

  public static List<ItemPickupPointL5Response> toItemPickupPointL5Response(List<ItemPickupPoint> itemPickupPointList,
      Map<String, BusinessPartnerPickupPoint> pickupPointMap, boolean cncForWarehouseFeatureSwitch,
      String fetchViewConfigByChannel) {
    return itemPickupPointList.stream().map(
        itemPickupPoint -> ItemPickupPointL5Response.builder()
            .productSku(itemPickupPoint.getProductSku())
            .itemSku(itemPickupPoint.getItemSku())
            .merchantSku(itemPickupPoint.getMerchantSku())
            .merchantCode(itemPickupPoint.getMerchantCode())
            .pickUpPointCode(itemPickupPoint.getPickupPointCode())
            .pickUpPointName(
                pickupPointMap.getOrDefault(itemPickupPoint.getPickupPointCode(), new BusinessPartnerPickupPoint())
                    .getName())
            .cncActive(
                setCncActivatedForBackward(itemPickupPoint.getAllItemViewConfigs(),
                    cncForWarehouseFeatureSwitch, itemPickupPoint.isCncActive(),
                    ItemViewConfig::getChannel, ItemViewConfig::isBuyable))
            .fbbActive(itemPickupPoint.isFbbActivated())
            .prices(setPriceResponse(itemPickupPoint, true))
            .viewConfigs(setViewConfigResponse(itemPickupPoint, fetchViewConfigByChannel, cncForWarehouseFeatureSwitch))
            .markForDelete(itemPickupPoint.isMarkForDelete()).build()).peek(
              itemPickupPointL5Response -> setCncActivatedForBackward(
                  itemPickupPointL5Response.getViewConfigs(), cncForWarehouseFeatureSwitch,
                  itemPickupPointL5Response.isCncActive(), ViewConfigResponse::getChannelId,
                  ViewConfigResponse::isBuyable)).collect(Collectors.toList());
  }

  public static void setMainImageForProducts(ProductResponse productResponse, List<ItemResponse> itemResponses,
      boolean setMainImageIfNotExists) {
    if (setMainImageIfNotExists) {
      //set main image at product level
      boolean isProductMainImagePresent =
          Optional.ofNullable(productResponse).map(ProductResponse::getMasterDataProduct)
              .map(MasterDataProductDTO::getMasterDataProductImages).orElse(new ArrayList<>()).stream()
              .anyMatch(MasterDataProductImageDTO::isMainImage);

      if (!isProductMainImagePresent) {
        Optional<MasterDataProductImageDTO> masterDataProductImageDTO =
            Optional.ofNullable(productResponse).map(ProductResponse::getMasterDataProduct)
                .map(MasterDataProductDTO::getMasterDataProductImages).orElse(new ArrayList<>()).stream().findFirst();
        if (masterDataProductImageDTO.isPresent()) {
          masterDataProductImageDTO.get().setMainImage(true);
        }
      }

      //set main image at item level
      for (ItemResponse itemResponse : Optional.ofNullable(itemResponses).orElse(new ArrayList<>())) {
        boolean isItemMainImagePresent = Optional.ofNullable(itemResponse).map(ItemResponse::getMasterDataItem)
            .map(MasterDataItemDTO::getMasterDataItemImages).orElse(new ArrayList<>()).stream()
            .anyMatch(MasterDataItemImageDTO::isMainImage);

        if (!isItemMainImagePresent) {
          Optional<MasterDataItemImageDTO> masterDataItemImageDTO =
              Optional.ofNullable(itemResponse).map(ItemResponse::getMasterDataItem)
                  .map(MasterDataItemDTO::getMasterDataItemImages).orElse(new ArrayList<>()).stream().findFirst();
          if (masterDataItemImageDTO.isPresent()) {
            masterDataItemImageDTO.get().setMainImage(true);
          }
        }
      }
    }
  }

  public static List<ItemPickupPointCodeResponse> toItemPickupPointCodeResponses(
      Collection<ItemPickupPoint> itemPickupPoints, ProductCollectionsVo productCollectionsVo) {
    Map<String, Date> productSkuToPreOrderDateMap =
        createProductSkuToPreOrderDateMap(productCollectionsVo);
    return itemPickupPoints.stream().map(
            itemPickupPoint -> toItemPickupPointCodeResponse(itemPickupPoint,
                productSkuToPreOrderDateMap.get(itemPickupPoint.getProductSku())))
        .collect(Collectors.toList());
  }

  private static Map<String, Date> createProductSkuToPreOrderDateMap(
      ProductCollectionsVo productCollectionsVo) {
    if (Objects.isNull(productCollectionsVo) || CollectionUtils.isEmpty(
        productCollectionsVo.getProducts())) {
      return Collections.emptyMap();
    }

    return productCollectionsVo.getProducts().stream()
        .filter(product -> Objects.nonNull(product.getPreOrder()))
        .filter(product -> Objects.nonNull(product.getPreOrder().getPreOrderDate()))
        .collect(
            Collectors.toMap(Product::getProductSku,
                product -> product.getPreOrder().getPreOrderDate(),
                (existing, replacement) -> existing));
  }

  private static ItemPickupPointCodeResponse toItemPickupPointCodeResponse(
      ItemPickupPoint itemPickupPoint, Date preOrderDate) {
    ItemPickupPointCodeResponse itemPickupPointCodeResponse = new ItemPickupPointCodeResponse();
    itemPickupPointCodeResponse.setItemSku(itemPickupPoint.getItemSku());
    itemPickupPointCodeResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemPickupPointCodeResponse.setPreOrderDate(preOrderDate);
    return itemPickupPointCodeResponse;
  }

  public static List<SharedProductBundleRecipeResponse> toSharedProductBundleRecipeResponse(List<Item> items,
      List<Item> bundleRecipeItems) {
    List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses = new ArrayList<>();

    // group all items by item code
    Map<String, List<Item>> itemCodeAndItemMap =
        Optional.ofNullable(items).orElse(new ArrayList<>()).stream().collect(Collectors.groupingBy(Item::getItemCode));

    // create mapping of item sku and item code
    Map<String, String> itemSkuAndItemCodeMap =
        Optional.ofNullable(bundleRecipeItems).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(Item::getItemSku, Item::getItemCode));

    // map item details and bundle recipe to response
    // if there is more than 1 item for a item code its a shared product
    for (Map.Entry<String, List<Item>> entry : itemCodeAndItemMap.entrySet()) {
      Item item = entry.getValue().stream().findFirst().get();
      SharedProductBundleRecipeResponse sharedProductBundleRecipeResponse =
          SharedProductBundleRecipeResponse.builder().itemCode(entry.getKey())
              .sharedProduct(entry.getValue().size() > Constants.ONE).bundleRecipe(
                  toSkuCodeBundleRecipeResponses(Optional.ofNullable(item.getBundleRecipe()).orElse(new HashSet<>()),
                      itemSkuAndItemCodeMap)).build();
      sharedProductBundleRecipeResponses.add(sharedProductBundleRecipeResponse);
    }

    return sharedProductBundleRecipeResponses;
  }

  private static Set<SkuCodeBundleRecipeResponse> toSkuCodeBundleRecipeResponses(Collection<BundleRecipe> bundleRecipes,
      Map<String, String> itemSkuAndItemCodeMap) {
    Set<SkuCodeBundleRecipeResponse> skuCodeBundleRecipeResponses = new HashSet<>();
    for (BundleRecipe bundleRecipe : bundleRecipes) {
      if (itemSkuAndItemCodeMap.containsKey(bundleRecipe.getItemSku())) {
        SkuCodeBundleRecipeResponse skuCodeBundleRecipeResponse =
            SkuCodeBundleRecipeResponse.builder().itemCode(itemSkuAndItemCodeMap.get(bundleRecipe.getItemSku()))
                .quantity(bundleRecipe.getQuantity()).build();
        skuCodeBundleRecipeResponses.add(skuCodeBundleRecipeResponse);
      }
    }
    return skuCodeBundleRecipeResponses;
  }


  public static List<ItemCodeBasicDetailResponse> toItemCodeBasicDetailResponse(
    List<Item> summaryResponsesByItemCodes, Map<String, Product> productSkuAndProductMap) {
    List<ItemCodeBasicDetailResponse> itemCodeBasicDetailResponses = new ArrayList<>();
    for (Item item : summaryResponsesByItemCodes) {
      ItemCodeBasicDetailResponse itemCodeBasicDetailResponse = new ItemCodeBasicDetailResponse();
      if (productSkuAndProductMap.containsKey(item.getProductSku())) {
        Product product = productSkuAndProductMap.get(item.getProductSku());
        itemCodeBasicDetailResponse.setArchivedAtL3(product.isArchived());
        itemCodeBasicDetailResponse.setTdSeller(product.isTradingProduct());
        itemCodeBasicDetailResponse.setSuspended(product.isSuspended());
      } else {
        log.info("Product SKU mapping missing for {}, while setting itemCodeBasicDetailResponses ",
          item.getProductSku());
      }
      itemCodeBasicDetailResponse.setArchivedAtL4(item.isArchived());
      itemCodeBasicDetailResponse.setItemName(item.getGeneratedItemName());
      itemCodeBasicDetailResponse.setItemCode(item.getItemCode());
      itemCodeBasicDetailResponse.setMerchantCode(item.getMerchantCode());
      itemCodeBasicDetailResponse.setProductSku(item.getProductSku());
      itemCodeBasicDetailResponse.setItemSku(item.getItemSku());
      itemCodeBasicDetailResponse.setMainImageUrl(item.getMainImageUrl());
      itemCodeBasicDetailResponses.add(itemCodeBasicDetailResponse);
    }
    return itemCodeBasicDetailResponses;
  }

  public static List<EanUpcPickupPointCodeResponse> getEanUpcPickupPointCodeResponse(
      List<ItemPickupPoint> itemPickupPointList, Map<String, String> itemSkuItemNameMap) {
    return itemPickupPointList.stream().map(
            itemPickupPoint -> toEanUpcPickupPointCodeResponse(itemPickupPoint, itemSkuItemNameMap))
        .collect(Collectors.toList());
  }

  public static void removeDuplicateProductAttributeValue(ProductAndItemsResponse productAndItemsResponse,
      boolean removeDuplicateProductAttribute) {
    if (removeDuplicateProductAttribute) {
        removeDuplicateAttributeAndValueFromMasterDataProductAttribute(productAndItemsResponse);
        removeDuplicateDescriptiveAttribute(productAndItemsResponse);
        removeDuplicateSortedDefiningAttribute(productAndItemsResponse);
    }
  }

  private static void removeDuplicateDescriptiveAttribute(ProductAndItemsResponse productAndItemsResponse) {
    Map<String, List<ProductAttributeDetailDTO>> descriptiveAttributeToProductAttributeDetailDTOMap =
        Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
            .map(ProductResponse::getDescriptiveAttributes).orElseGet(ArrayList::new).stream()
            .filter(Predicate.not(
                productAttributeDetailDTO -> MULTI_VALUE_ATTRIBUTES.contains(productAttributeDetailDTO.getAttributeType())))
            .collect(Collectors.groupingBy(ProductAttributeDetailDTO::getAttributeCode));
    List<ProductAttributeDetailDTO> updatedProductAttributeDetailDTOList = new ArrayList<>();
    List<ProductAttributeDetailDTO> multiValuedAttributes =
        Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
            .map(ProductResponse::getDescriptiveAttributes).orElseGet(ArrayList::new).stream().filter(
                productAttributeDetailDTO -> MULTI_VALUE_ATTRIBUTES.contains(productAttributeDetailDTO.getAttributeType()))
            .toList();
    for (Map.Entry<String, List<ProductAttributeDetailDTO>> entry : descriptiveAttributeToProductAttributeDetailDTOMap.entrySet()) {
      entry.getValue().stream().findFirst().ifPresent(updatedProductAttributeDetailDTOList::add);
    }
    Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
        .ifPresent(productResponse -> productResponse.setDescriptiveAttributes(updatedProductAttributeDetailDTOList));
    Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
        .ifPresent(productResponse -> productResponse.getDescriptiveAttributes().addAll(multiValuedAttributes));
  }

  private static void removeDuplicateSortedDefiningAttribute(ProductAndItemsResponse productAndItemsResponse) {
    Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
        .map(ProductResponse::getMasterDataProduct).map(MasterDataProductDTO::getSortedDefiningAttributes)
        .ifPresent(ResponseHelper::removeDuplicateSortedDefiningAttribute);
  }

  private static void removeDuplicateSortedDefiningAttribute(List<SortedDefiningAttributeDTO> sortedDefiningAttributeDTOS) {
    for (SortedDefiningAttributeDTO sortedDefiningAttributeDTO : sortedDefiningAttributeDTOS) {
      sortedDefiningAttributeDTO.setDefiningAttributes(
          Optional.ofNullable(sortedDefiningAttributeDTO.getDefiningAttributes()).orElseGet(ArrayList::new).stream()
              .distinct().collect(toList()));
    }
  }

  private static void removeDuplicateAttributeAndValueFromMasterDataProductAttribute(
      ProductAndItemsResponse productAndItemsResponse) {
    Map<String, List<MasterDataProductAttributeDTO>> attributeToMasterDataProductAttributeMap =
        Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
            .map(ProductResponse::getMasterDataProduct).map(MasterDataProductDTO::getMasterDataProductAttributes)
            .orElseGet(ArrayList::new).stream().collect(Collectors.groupingBy(ResponseHelper::getAttributeCode));
    List<MasterDataProductAttributeDTO> updatedMasterDataProductAttributeDTOList = new ArrayList<>();
    for (Map.Entry<String, List<MasterDataProductAttributeDTO>> entry : attributeToMasterDataProductAttributeMap.entrySet()) {
      MasterDataProductAttributeDTO masterDataProductAttributeDTO = entry.getValue().getFirst();
      removeDuplicateProductAttributeValues(masterDataProductAttributeDTO);
      updatedMasterDataProductAttributeDTOList.add(masterDataProductAttributeDTO);
    }
    Optional.ofNullable(productAndItemsResponse).map(ProductAndItemsResponse::getProduct)
        .map(ProductResponse::getMasterDataProduct).ifPresent(
            masterDataProductDTO -> masterDataProductDTO.setMasterDataProductAttributes(
                updatedMasterDataProductAttributeDTOList));
  }

  private static void removeDuplicateProductAttributeValues(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    Function<MasterDataProductAttributeValueDTO, String> groupingByKey =
        getGroupingKeyBasedOnAttributeType(masterDataProductAttributeDTO);
    if (Objects.nonNull(groupingByKey)) {
      Map<String, List<MasterDataProductAttributeValueDTO>> attributeValueToMasterDataAttributeValueMap =
          Optional.ofNullable(masterDataProductAttributeDTO.getMasterDataProductAttributeValues())
              .orElseGet(ArrayList::new).stream().collect(Collectors.groupingBy(groupingByKey));
      List<MasterDataProductAttributeValueDTO> updatedMasterDataProductAttributeValueDTO = new ArrayList<>();
      for (Map.Entry<String, List<MasterDataProductAttributeValueDTO>> entry : attributeValueToMasterDataAttributeValueMap.entrySet()) {
        entry.getValue().stream().findFirst().ifPresent(updatedMasterDataProductAttributeValueDTO::add);
      }
      masterDataProductAttributeDTO.setMasterDataProductAttributeValues(updatedMasterDataProductAttributeValueDTO);
    }
  }

  private static String getAttributeCode(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    return Optional.ofNullable(masterDataProductAttributeDTO).map(MasterDataProductAttributeDTO::getMasterDataAttribute)
        .map(MasterDataAttributeDTO::getAttributeCode).orElse(StringUtils.EMPTY);
  }

  private static boolean isDescriptiveAttribute(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    return Optional.ofNullable(masterDataProductAttributeDTO).map(MasterDataProductAttributeDTO::getMasterDataAttribute)
        .filter(masterDataAttributeDTO -> MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE.equals(
            masterDataAttributeDTO.getAttributeType())).isPresent();
  }

  private static boolean isPredefinedAttribute(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    return Optional.ofNullable(masterDataProductAttributeDTO).map(MasterDataProductAttributeDTO::getMasterDataAttribute)
        .filter(masterDataAttributeDTO -> MasterDataAttributeType.PREDEFINED_ATTRIBUTE.equals(
            masterDataAttributeDTO.getAttributeType())).isPresent();
  }

  private static boolean isDefiningAttribute(MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    return Optional.ofNullable(masterDataProductAttributeDTO).map(MasterDataProductAttributeDTO::getMasterDataAttribute)
        .filter(masterDataAttributeDTO -> MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
            masterDataAttributeDTO.getAttributeType())).isPresent();
  }

  private static Function<MasterDataProductAttributeValueDTO, String> getGroupingKeyBasedOnAttributeType(
      MasterDataProductAttributeDTO masterDataProductAttributeDTO) {
    if (isDescriptiveAttribute(masterDataProductAttributeDTO)) {
      return masterDataProductAttributeValueDTO -> Optional.ofNullable(masterDataProductAttributeValueDTO)
          .map(MasterDataProductAttributeValueDTO::getDescriptiveAttributeValue).orElse(StringUtils.EMPTY);
    } else if (isPredefinedAttribute(masterDataProductAttributeDTO)) {
      return masterDataProductAttributeValueDTO -> Optional.ofNullable(
              masterDataProductAttributeValueDTO.getPredefinedAllowedAttributeValue())
          .map(PredefinedAllowedAttributeValueDTO::getValue).orElse(StringUtils.EMPTY);
    } else if (isDefiningAttribute(masterDataProductAttributeDTO)) {
      return masterDataProductAttributeValueDTO -> Optional.ofNullable(
              masterDataProductAttributeValueDTO.getAllowedAttributeValue())
          .map(MasterDataAllowedAttributeValueDTO::getValue).orElse(StringUtils.EMPTY);
    }
    return null;
  }

  public static void setVideoUrl(List<Product> products, Map<String, SimpleMasterDataProductVO> masterDataProducts) {
    if (CollectionUtils.isNotEmpty(products)) {
      Map<String, Product> productCodeToProductMap = products.stream().filter(Objects::nonNull)
          .collect(Collectors.toMap(Product::getProductCode, product -> product));
      masterDataProducts.forEach((productCode, simpleMasterDataProductVO) -> {
        Product product = productCodeToProductMap.get(productCode);
        if (Objects.nonNull(product) && Objects.nonNull(product.getVideo())) {
          Video video = product.getVideo();
          String finalUrl = getFinalUrl(video);
          String coverImagePath = video.getCoverImagePath();
          VideoDTO videoDTO = new VideoDTO();
          videoDTO.setUrl(finalUrl);
          videoDTO.setCoverImagePath(coverImagePath);
          simpleMasterDataProductVO.setVideo(videoDTO);
          simpleMasterDataProductVO.setUrl(StringUtils.EMPTY);
        }
      });
    }
  }

  public static void setVideoUrlAndYoutubeUrl(ProductAndItemsResponse productAndItemsResponse, ProductVo productVo) {
    Optional.ofNullable(productAndItemsResponse.getProduct().getMasterDataProduct()).ifPresent(masterDataProductDTO -> {
      masterDataProductDTO.setUrl(productVo.getUrl());
      if (Objects.nonNull(productVo.getVideo())) {
        String finalUrl = getFinalUrl(productVo.getVideo());
        String coverImagePath = productVo.getVideo().getCoverImagePath();
        masterDataProductDTO.setVideo(new VideoUrl(finalUrl, coverImagePath));
        masterDataProductDTO.setUrl(StringUtils.EMPTY);
      }
    });
  }

  public static String getFinalUrl(Video video) {
    return StringUtils.defaultIfEmpty(video.getFinalUrl(), video.getSourceUrl());
  }

  public static void setVideoUrlAndCoverImagePath(VideoAddEditRequest videoAddEditRequest, ProductVo productVo) {
    if (Objects.nonNull(videoAddEditRequest)) {
      Video video = new Video();
      video.setVideoId(videoAddEditRequest.getVideoId());
      video.setSourceUrl(videoAddEditRequest.getVideoUrl());
      video.setCoverImagePath(videoAddEditRequest.getCoverImagePath());
      video.setVideoName(videoAddEditRequest.getVideoName());
      productVo.setVideo(video);
    }
  }

  public static ProductDataAutoFixHistoryListRequest convertToProductDataAutoFixHistoryListRequest(String productCode,
      String type, String additionalInfo) {
    ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistoryDto.setType(type);
    productDataAutoFixHistoryDto.setProductCode(productCode);
    productDataAutoFixHistoryDto.setAdditionalInfo(additionalInfo);
    List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList = new ArrayList<>();
    productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
    return new ProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDtoList);
  }

  public static PreOrderDTO getPreOrder(PreOrder preOrder) {
    if (Objects.nonNull(preOrder)) {
      PreOrderDTO preOrderDTO = new PreOrderDTO();
      BeanUtils.copyProperties(preOrder, preOrderDTO);
      return preOrderDTO;
    }
    return null;
  }
}
