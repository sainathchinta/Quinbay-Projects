package com.gdn.mta.product.util;

import static com.gdn.mta.product.util.ConverterUtil.getProductStatus;
import static com.gdn.mta.product.util.ConverterUtil.isPurchaseOrderPurchaseTerm;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT;
import static com.gdn.partners.pbp.commons.constants.Constants.ZERO;

import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AttributeCodeValueValueTypeDetails;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.EditFlagChangesDTO;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbAndCncDataChangeDto;
import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductDetailCompleteResponse;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Price;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3ViewConfig;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.BuyableScheduleDTO;
import com.gdn.partners.pbp.model.productlevel3.DiscoverableScheduleDTO;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.BuyableScheduleRequest;
import com.gdn.x.product.rest.web.model.request.DiscoverableScheduleRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.response.BuyableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.DiscoverableScheduleResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.response.ProductL3SummaryResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductCreationClientIds;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

import org.jetbrains.annotations.NotNull;
import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;
import org.slf4j.MDC;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CommonUtils {

  private static final String ruleStringFormat = "Quantity : %d  Discount : %.3f,";
  private static final String INTERNAL = "INTERNAL";
  private static final String PRODUCT_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}$";
  private static final String ITEM_SKU_PATTERN = "^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$";
  private static final String IFRAME = "iframe";

  private static final Double ZERO_DIMENSION = 0.0;
  public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  public static final String OMG_VALID_PREORDER_TYPE = "DATE";

  public static void validateWholesalePrice(List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests,
      Double price, Integer minimumPrice, int maxWholesalePriceRequests, String itemSku) {
    if (CollectionUtils.isNotEmpty(productItemWholesalePriceRequests)) {
      Set<Integer> productWholeSaleQuantity = productItemWholesalePriceRequests.stream().map(ProductItemWholesalePriceRequest::getQuantity).collect(Collectors.toSet());
      Set<Double> productWholeSaleDiscount = productItemWholesalePriceRequests.stream().map(ProductItemWholesalePriceRequest::getWholesaleDiscount).collect(Collectors.toSet());
      if (productItemWholesalePriceRequests.size() != productWholeSaleQuantity.size()) {
        throw new ApiIncorrectInputDataException(ErrorMessages.WHOLESALE_QUANTITY_IS_DUPLICATE,
            ApiErrorCode.WHOLESALE_QUANTITY_DUPLICATE);
      }
      if (productItemWholesalePriceRequests.size() != productWholeSaleDiscount.size()) {
        throw new ApiIncorrectInputDataException(ErrorMessages.WHOLESALE_DISCOUNT_IS_DUPLICATE,
            ApiErrorCode.WHOLESALE_DISCOUNT_DUPLICATE);
      }
      if (productItemWholesalePriceRequests.size() > maxWholesalePriceRequests) {
        if (StringUtils.isNotBlank(itemSku)) {
          throw new ApiIncorrectInputDataException(String.format(ErrorMessages.WHOLE_SETTING_GTE_5, maxWholesalePriceRequests, itemSku,
                  productItemWholesalePriceRequests.size()), ApiErrorCode.WHOLESALE_DISCOUNT_SETTING_GTE_5);
        } else {
          throw new ApiIncorrectInputDataException(String.format(ErrorMessages.WHOLE_SETTING_GTE_5_CREATE, maxWholesalePriceRequests,
                  productItemWholesalePriceRequests.size()), ApiErrorCode.WHOLESALE_DISCOUNT_SETTING_GTE_5);
        }
      }
      List<Integer> quantityList = productItemWholesalePriceRequests.stream()
          .sorted(Comparator.comparingInt(ProductItemWholesalePriceRequest::getQuantity))
          .map(ProductItemWholesalePriceRequest::getQuantity).collect(Collectors.toList());
      List<Integer> quantityListSortedOnDiscount = productItemWholesalePriceRequests.stream()
          .sorted(Comparator.comparingDouble(ProductItemWholesalePriceRequest::getWholesaleDiscount))
          .map(ProductItemWholesalePriceRequest::getQuantity).collect(Collectors.toList());
      List<Double> wholesaleDiscountList = productItemWholesalePriceRequests.stream().sorted(
              Comparator.comparingDouble(ProductItemWholesalePriceRequest::getWholesaleDiscount))
          .map(ProductItemWholesalePriceRequest::getWholesaleDiscount).collect(Collectors.toList());
      if (quantityList.get(0) < 2) {
        throw new ApiIncorrectInputDataException(ErrorMessages.MIN_QUANTITY_ALLOWED_FOR_WHOLESALE_PRICE_IS_2,
            ApiErrorCode.WHOLESALE_QUANTITY_LESS_THAN_2);
      }
      if (wholesaleDiscountList.get(0) <= 0) {
        throw new ApiIncorrectInputDataException(ErrorMessages.WHOLESALE_DISCOUNT_LTE_ZERO,
            ApiErrorCode.WHOLESALE_DISCOUNT_LTE_ZERO);
      }
      if (!quantityList.equals(quantityListSortedOnDiscount)) {
        throw new ApiIncorrectInputDataException(ErrorMessages.WHOLESALE_PRICE_TIER_RULE_VIOLATION,
            ApiErrorCode.WHOLESALE_PRICE_TIER_RULE_VIOLATION);
      }
      if (productItemWholesalePriceRequests.stream().anyMatch(productItemWholesalePriceRequest -> (
          price - ((productItemWholesalePriceRequest.getWholesaleDiscount() * price) / 100.00)
              < minimumPrice))) {
        throw new ApiIncorrectInputDataException(
            String.format(ErrorMessages.WHOLESALE_PRICE_CANT_BE_LESS_THAN_MINIMUM_PRICE,
                minimumPrice), ApiErrorCode.WHOLESALE_DISCOUNT_LTE_MINIMUM_DISCOUNT);
      }
    }
  }

  public static void validateWholesalePriceConfig(
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests, Double price,
      WholesaleMappingResponse wholesaleMappingResponse) {

    if (wholesaleMappingResponse.getConfigurationType().equalsIgnoreCase(Constants.PERCENTAGE)) {
      Map<Integer, Double> quantityPercentageMap =
          wholesaleMappingResponse.getWholesaleConfig().stream().collect(
              Collectors.toMap(response -> response.getQuantity(), response -> response.getMinWholesaleDiscount().get(0).getPercentage()));
      checkWholesaleConfigs(productItemWholesalePriceRequests, quantityPercentageMap);
    } else {
      List<Double> distinctPrices = wholesaleMappingResponse.getWholesaleConfig().stream()
          .map(wholesaleConfigResponse1 -> wholesaleConfigResponse1.getMinWholesaleDiscount()).flatMap(List::stream).map(minDiscountConfig -> minDiscountConfig.getPrice()).distinct()
          .sorted(Comparator.reverseOrder()).collect(Collectors.toList());
      if (price >= distinctPrices.get(distinctPrices.size() - 1)) {
        Double priceTier = distinctPrices.stream().filter(discountPrice -> discountPrice <= price).min(Comparator.reverseOrder()).orElse(null);
        Map<Integer, Double> quantityPercentageMap = new HashMap<>();
        for (WholesaleConfigResponse wholesaleConfigResponse : wholesaleMappingResponse.getWholesaleConfig()) {
          wholesaleConfigResponse.getMinWholesaleDiscount().stream().filter(
              minWholesaleDiscountResponse -> minWholesaleDiscountResponse.getPrice().equals(priceTier)).findFirst().ifPresent(
              minWholesaleDiscountResponse -> quantityPercentageMap.put(wholesaleConfigResponse.getQuantity(),
                  minWholesaleDiscountResponse.getPercentage()));
        }
        checkWholesaleConfigs(productItemWholesalePriceRequests, quantityPercentageMap);
      }
    }
  }

  private static void checkWholesaleConfigs(List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests,
      Map<Integer, Double> quantityPercentageMap) {
    List<Integer> quantities =
        quantityPercentageMap.keySet().stream().sorted(Comparator.reverseOrder()).collect(Collectors.toList());
    for (ProductItemWholesalePriceRequest productItemWholesalePriceRequest : productItemWholesalePriceRequests) {
      if (productItemWholesalePriceRequest.getQuantity() < quantities.get(quantities.size() - 1)) {
        continue;
      }
      if (productItemWholesalePriceRequest.getWholesaleDiscount() < quantityPercentageMap.get(
          quantityPercentageMap.keySet().stream()
              .filter(quantity -> (quantity <= productItemWholesalePriceRequest.getQuantity()))
              .min(Comparator.reverseOrder()).orElseThrow(
                  () -> new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorCategory.UNSPECIFIED.getMessage())))) {
        throw new ApiIncorrectInputDataException(ErrorMessages.WHOLESALE_FLAG_AUTO_DEACTIVATE,
            ApiErrorCode.DIFFERENT_THRESHOLD_ERROR_CODE);
      }
    }
  }

  public static void setProductCreationType(ProductCreationRequest request, String clientId, String flowType) {
    if (Objects.isNull(request.getProductCreationType())) {
      if (ProductCreationClientIds.PCU_EXTERNAL_APP.getProductCreationClientId().equalsIgnoreCase(clientId)) {
        if (ProductCreationType.FLOW1.getProductCreationType().equalsIgnoreCase(flowType)) {
          request.setProductCreationType(ProductCreationType.FLOW1_WEB);
        } else if (ProductCreationType.FLOW2.getProductCreationType().equalsIgnoreCase(flowType)) {
          request.setProductCreationType(ProductCreationType.FLOW2_WEB);
        } else if (StringUtils.isBlank(request.getBusinessPartnerCode())) {
          request.setProductCreationType(ProductCreationType.FLOW3_WEB);
        }
      } else if (ProductCreationClientIds.MTA_APP.getProductCreationClientId().equalsIgnoreCase(clientId)) {
        if (ProductCreationType.FLOW2.getProductCreationType().equalsIgnoreCase(flowType)) {
          request.setProductCreationType(ProductCreationType.FLOW2_APP);
        } else {
          request.setProductCreationType(ProductCreationType.FLOW1_APP);
        }
      } else if (clientId.startsWith(ProductCreationClientIds.MTA_API.getProductCreationClientId())) {
        if (ProductCreationType.FLOW2.getProductCreationType().equalsIgnoreCase(flowType)) {
          request.setProductCreationType(ProductCreationType.FLOW2_API);
        } else {
          request.setProductCreationType(ProductCreationType.FLOW1_API);
        }
      }
    }
  }

  public static String generateSpecificationDetail(ProductRequest request) {
    StringBuffer specificationDetail = new StringBuffer("<ul>");
    for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
      AttributeRequest attributeRequest = productAttributeRequest.getAttribute();
      if (!attributeRequest.isSkuValue()) {
        specificationDetail.append("<li>").append(attributeRequest.getName()).append("<ul>");
        if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attributeRequest.getAttributeType())) {
          ProductAttributeValueRequest productAttributeValueRequest =
              productAttributeRequest.getProductAttributeValues().get(0);
          specificationDetail.append("<li>").append(productAttributeValueRequest.getDescriptiveAttributeValue()).append("</li>");
        } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attributeRequest.getAttributeType())) {
          ProductAttributeValueRequest productAttributeValueRequest =
              productAttributeRequest.getProductAttributeValues().get(0);
          specificationDetail.append("<li>").append(productAttributeValueRequest.getPredefinedAllowedAttributeValue().getValue())
              .append("</li>");
        } else if (AttributeType.DEFINING_ATTRIBUTE.equals(attributeRequest.getAttributeType())) {
          for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest.getProductAttributeValues()) {
            specificationDetail.append("<li>").append(productAttributeValueRequest.getAllowedAttributeValue().getValue())
                .append("</li>");
          }
        }
        specificationDetail.append("</ul></li>");
      }
    }
    specificationDetail.append("</ul>");
    return specificationDetail.toString();
  }

  public static List<ProductL3SummaryResponse> toProductL3SummaryResponse(
      List<ProductSkuSummaryResponse> productSkuSummaryResponseList,
      Map<String, List<CategoryResponse>> categoryCodeAndCategoryHierarchyMap) {
    List<ProductL3SummaryResponse> productL3SummaryResponseList = new ArrayList<>();
    for (ProductSkuSummaryResponse productSkuSummaryResponse : productSkuSummaryResponseList) {
      ProductL3SummaryResponse productL3SummaryResponse =
          ProductL3SummaryResponse.builder().brand(productSkuSummaryResponse.getBrand())
              .categoryCode(productSkuSummaryResponse.getCategoryCode()).categoryName(
                  categoryCodeAndCategoryHierarchyMap.get(productSkuSummaryResponse.getCategoryCode()).get(0).getName()).productImage(productSkuSummaryResponse.getProductImage())
              .productSku(productSkuSummaryResponse.getProductSku())
              .productSkuName(productSkuSummaryResponse.getProductName())
              .isArchived(productSkuSummaryResponse.isArchived()).build();
      productL3SummaryResponse.setCategoryHierarchy(String.join(" > ",
          categoryCodeAndCategoryHierarchyMap.get(productSkuSummaryResponse.getCategoryCode())
              .stream().map(CategoryResponse::getName).collect(Collectors.toList())));
      productL3SummaryResponseList.add(productL3SummaryResponse);
    }
    return productL3SummaryResponseList;
  }

  public static void setNonNullListValuesForLogs(ProductCreationRequest request) {
    if (Objects.isNull(request.getProductItems())) {
      request.setProductItems(new ArrayList<>());
    }
    if (Objects.isNull(request.getProductCategories())) {
      request.setProductCategories(new ArrayList<>());
    }
    if (Objects.isNull(request.getProductAttributes())) {
      request.setProductAttributes(new ArrayList<>());
    }
  }

  public static void setMasterDataDetailsFromProductL3Response(ProductL3Response productL3Response,
      ProductBusinessPartner productBusinessPartner) {
    if (Objects.nonNull(productL3Response.getMasterDataProduct())) {
      productBusinessPartner.setProductName(
          productL3Response.getMasterDataProduct().getProductName());
      productBusinessPartner.setBrand(productL3Response.getMasterDataProduct().getBrand());
    }
    if (Objects.nonNull(productL3Response.getMasterCatalog()) && Objects.nonNull(
        productL3Response.getMasterCatalog().getCategory()) && CollectionUtils.isNotEmpty(
        productL3Response.getItemCatalogs())) {
      Optional<ItemCategoryDTO> itemCategoryDTO = productL3Response.getItemCatalogs().stream()
          .flatMap(itemCatalogDTO -> itemCatalogDTO.getItemCategories().stream()).filter(
              categoryDto -> productL3Response.getMasterCatalog().getCategory().getCategoryCode()
                  .equals(categoryDto.getProductCategoryCode())).findFirst();
      if (itemCategoryDTO.isPresent()) {
        productBusinessPartner.setCategoryCode(itemCategoryDTO.get().getCategoryId());
        productBusinessPartner.setCategoryName(itemCategoryDTO.get().getCategory());
      }
    }
  }

  public static List<UpdatedProductHistory> setOnlineStatusAndPickupPointCode(
      List<UpdatedProductHistory> updatedProductHistoryList, Map<String, String> itemSkuAndPickupPointCodeMap) {
    updatedProductHistoryList.stream().forEach(updatedProductHistory -> {
      updatedProductHistory.setOnlineStatus(true);
      updatedProductHistory.setPickupPointCode(
          itemSkuAndPickupPointCodeMap.getOrDefault(updatedProductHistory.getGdnSku(), Constants.HYPHEN));
    });
    return updatedProductHistoryList;
  }

  public static boolean validateProtectedBrand(BrandResponse brandResponse, String businessPartnerCode) {
    return brandResponse.isProtectedBrand() && (!INTERNAL.equalsIgnoreCase(businessPartnerCode));
  }

  public static boolean checkBooleanEquals(Boolean wholeSaleValueResponseFlag, Boolean wholeSaleValueRequestValue) {
    return Objects.isNull(wholeSaleValueResponseFlag) ?
        Objects.isNull(wholeSaleValueRequestValue) :
        (Objects.nonNull(wholeSaleValueRequestValue) ?
            wholeSaleValueResponseFlag == wholeSaleValueRequestValue :
            false);
  }

  public static String toL5Id(String itemSku, String pickupPointCode) {
    return new StringBuilder().append(itemSku).append(Constants.DASH_DELIMITER).append(pickupPointCode).toString();
  }

  public static boolean isPriceEditDisabled(boolean merchantPromoDiscount, Set<PriceDTO> itemPriceDTOSet) {
    Date now = new Date();
    return merchantPromoDiscount || itemPriceDTOSet.stream().anyMatch(
        priceDTO -> CollectionUtils.isNotEmpty(priceDTO.getListOfDiscountPrices()) && priceDTO.getListOfDiscountPrices().stream().anyMatch(
            discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode()) && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime()
                .after(now))));
  }

  public static List<InventoryDetailInfoRequestDTO> toSingleInventoryDetailInfoRequestDTOList(String merchantCode, String itemSku, String pickupPointCode) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setPickupPointCode(pickupPointCode);
    inventoryDetailInfoRequestDTO.setWebItemSku(itemSku);
    inventoryDetailInfoRequestDTO.setWebMerchantCode(merchantCode);
    return Collections.singletonList(inventoryDetailInfoRequestDTO);
  }

  public static ProductLevel3Summary generateProductLevel3SummarySingle(ItemSummaryListResponse itemSummaryListResponse, List<CategoryResponse> categoryData,
      PickupPointResponse pickupPointData, ProductLevel3Inventory inventory, ProfileResponse businessPartner) {
    ProductLevel3Summary product = new ProductLevel3Summary();
    product.setItemSku(itemSummaryListResponse.getItemSku());
    product.setSkuCode(itemSummaryListResponse.getItemCode());
    product.setMerchantSku(itemSummaryListResponse.getMerchantSku());
    product.setItemName(itemSummaryListResponse.getItemName());
    product.setCategoryCode(itemSummaryListResponse.getMasterCategoryCode());
    product.setProductType(itemSummaryListResponse.getProductType().getCode());
    product.setLateFulfillment(itemSummaryListResponse.isLateFulfillment());
    product.setCreatedDate(itemSummaryListResponse.getCreatedDate());
    product.setCreatedBy(itemSummaryListResponse.getCreatedBy());
    product.setUpdatedDate(itemSummaryListResponse.getUpdatedDate());
    product.setUpdatedBy(itemSummaryListResponse.getUpdatedBy());
    product.setIsArchived(itemSummaryListResponse.isArchived());
    product.setPromoBundling(itemSummaryListResponse.isPromoBundling());
    product.setMerchantPromoDiscount(itemSummaryListResponse.isMerchantPromoDiscount());
    product.setMerchantPromoDiscountActivated(itemSummaryListResponse.isMerchantPromoDiscountActivated());
    product.setWholesalePriceActivated(itemSummaryListResponse.getWholesalePriceActivated());
    if (Optional.ofNullable(businessPartner).map(ProfileResponse::isTrustedSeller).orElse(Boolean.FALSE)) {
      product.setForceReview(Boolean.FALSE);
    } else {
      product.setForceReview(itemSummaryListResponse.isForceReview());
    }
    product.setProductName(itemSummaryListResponse.getProductName());
    product.setProductSku(itemSummaryListResponse.getProductSku());
    product.setCncActive(itemSummaryListResponse.isCncActive());

    if (CollectionUtils.isNotEmpty(categoryData)) {
      String[] categoryNameAndHierarchy = generateCategoryNameIdAndHierarchy(categoryData);
      product.setCategoryName(categoryNameAndHierarchy[0]);
      product.setCategoryHierarchy(categoryNameAndHierarchy[1]);
    }

    if (Objects.nonNull(pickupPointData)) {
      product.setPickupPointCode(pickupPointData.getCode());
      product.setPickupPointName(pickupPointData.getName());
    }

    if (Objects.nonNull(inventory)) {
      product.setAvailableStockLevel1(inventory.getWarehouseAvailable());
      product.setReservedStockLevel1(inventory.getWarehouseReserved());
      product.setAvailableStockLevel2(inventory.getWebAvailable());
      product.setReservedStockLevel2(inventory.getWebReserved());
      product.setMinimumStockLevel2(inventory.getWebMinAlert());
      product.setSynchronizeStock(inventory.isWebSyncStock());
    }

    product.setPrices(new ArrayList<>());
    product.setViewConfigs(new ArrayList<>());
    product.setImages(new ArrayList<>());
    product.setOff2OnActiveFlag(itemSummaryListResponse.isOff2OnChannelActive());
    for (PriceDTO priceData : itemSummaryListResponse.getPrice()) {
      ProductLevel3Price price = new ProductLevel3Price();
      price.setChannelId(priceData.getChannel());
      price.setPrice(priceData.getListPrice());
      price.setSalePrice(priceData.getOfferPrice());
      if (CollectionUtils.isNotEmpty(priceData.getListOfDiscountPrices())) {
        price.setDiscountAmount(priceData.getListOfDiscountPrices().get(0).getDiscountPrice());
        price.setDiscountStartDate(priceData.getListOfDiscountPrices().get(0).getStartDateTime());
        price.setDiscountEndDate(priceData.getListOfDiscountPrices().get(0).getEndDateTime());
        price.setPromotionName(priceData.getListOfDiscountPrices().get(0).getAdjustmentName());
      }
      product.getPrices().add(price);
    }
    for (ItemViewConfigDTO viewConfigData : itemSummaryListResponse.getItemViewConfigs()) {
      ProductLevel3ViewConfig viewConfig = new ProductLevel3ViewConfig();
      viewConfig.setChannelId(viewConfigData.getChannel());
      viewConfig.setDisplay(viewConfigData.isDiscoverableOriginal());
      viewConfig.setBuyable(viewConfigData.isBuyableOriginal());
      setBuyableAndDiscoverableSchedules(viewConfigData, viewConfig);
      product.getViewConfigs().add(viewConfig);
    }
    return product;
  }

  private static void setBuyableAndDiscoverableSchedules(ItemViewConfigDTO viewConfigData,
      ProductLevel3ViewConfig viewConfig) {
    if (Objects.nonNull(viewConfigData.getItemBuyableSchedules())) {
      BuyableScheduleDTO buyableScheduleDTO = new BuyableScheduleDTO();
      buyableScheduleDTO.setBuyable(viewConfigData.getItemBuyableSchedules().isBuyable());
      buyableScheduleDTO.setStartDateTime(
          viewConfigData.getItemBuyableSchedules().getStartDateTime());
      buyableScheduleDTO.setEndDateTime(viewConfigData.getItemBuyableSchedules().getEndDateTime());
      viewConfig.setBuyableScheduleDTO(buyableScheduleDTO);
    }
    if (Objects.nonNull(viewConfigData.getItemDiscoverableSchedules())) {
      DiscoverableScheduleDTO discoverableScheduleDTO = new DiscoverableScheduleDTO();
      discoverableScheduleDTO.setDiscoverable(
          viewConfigData.getItemDiscoverableSchedules().isDiscoverable());
      discoverableScheduleDTO.setStartDateTime(
          viewConfigData.getItemDiscoverableSchedules().getStartDateTime());
      discoverableScheduleDTO.setEndDateTime(
          viewConfigData.getItemDiscoverableSchedules().getEndDateTime());
      viewConfig.setDiscoverableScheduleDTO(discoverableScheduleDTO);
    }
  }

  public static String[] generateCategoryNameIdAndHierarchy(List<CategoryResponse> categories) {
    StringBuilder categoryHierarchy = new StringBuilder();
    String categoryName = StringUtils.EMPTY;
    String categoryId = StringUtils.EMPTY;
    if (CollectionUtils.isNotEmpty(categories)) {
      int i = 0;
      ListIterator<CategoryResponse> iterator = categories.listIterator(categories.size());
      while (iterator.hasPrevious()) {
        CategoryResponse category = iterator.previous();
        if (i == categories.size() - 1) {
          categoryName = category.getName();
          categoryId = category.getId();
        }
        categoryHierarchy.append(category.getName());
        if (categories.size() > 1 && i < categories.size() - 1) {
          categoryHierarchy.append(Constants.CATEGORY_DELIMITER);
        }
        i++;
      }
    }
    return new String[] {categoryName, categoryHierarchy.toString(), categoryId};
  }

  public static boolean isProductNotActivatedBefore(ProductCollection productCollection) {
    return WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())
        && !productCollection.isPostLive() && !productCollection.isEdited();
  }

  public static boolean isProductActivatedBefore(ProductCollection productCollection) {
    return !isProductNotActivatedBefore(productCollection);
  }

  public static void generateWholesaleSkuStatusMap(Map<String, ProductItemWholesalePrice> productItemWholesalePriceMap,
      List<WholeSalePriceSkuStatusDto> wholeSalePriceSkuStatusDtoList) {
    if (CollectionUtils.isNotEmpty(wholeSalePriceSkuStatusDtoList)) {
      for (WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto : wholeSalePriceSkuStatusDtoList) {
        ProductItemWholesalePrice productItemWholesalePrice;
        productItemWholesalePrice = productItemWholesalePriceMap.get(
            wholeSalePriceSkuStatusDto.getItemSku() + Constants.HYPHEN + wholeSalePriceSkuStatusDto.getPickUpPointCode());
        if (Objects.nonNull(productItemWholesalePrice) && Constants.AUTO_INACTIVE.equals(
            wholeSalePriceSkuStatusDto.getSkuStatus())) {
          productItemWholesalePrice.setWholesalePriceActivated(false);
        }
      }
    }
  }

  public static void setMandatoryParameters(String storeId, String channelId, String clientId,
      String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }

  public static InProgressProductResponse toInProgressProductResponse(ProductBusinessPartner productBusinessPartner) {
    return InProgressProductResponse.builder().status(productBusinessPartner.getState()).productSku(productBusinessPartner.getGdnProductSku()).build();
  }

  public static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {
    Set<Object> seen = ConcurrentHashMap.newKeySet();
    return t -> seen.add(keyExtractor.apply(t));
  }

  public static boolean isMppEnabled(ProfileResponse profileResponse, String mppAllowedSellers) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && (
        (profileResponse.getCompany().isCncActivated()) || (
            Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) && mppAllowedSellers.contains(profileResponse.getCompany().getMerchantType())));
  }

  public static boolean isInternalSeller(ProfileResponse profileResponse, List<String> internalSellers) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && (internalSellers.contains(profileResponse.getCompany().getMerchantType()));
  }

  public static boolean getBusinessPartnerFlagValue(ProfileResponse profileResponse, String flagName) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getFlags).filter(MapUtils::isNotEmpty).map(flags -> flags.get(flagName)).map(Object::toString)
        .map(Boolean::parseBoolean).orElse(false);

  }

  public static InventoryDetailInfoRequestDTO toInvDetailInfoRequest(ItemLevel5Response itemLevel5ResponsesList) {
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO = new InventoryDetailInfoRequestDTO();
    inventoryDetailInfoRequestDTO.setWebItemSku(itemLevel5ResponsesList.getItemSku());
    inventoryDetailInfoRequestDTO.setPickupPointCode(itemLevel5ResponsesList.getPickupPointCode());
    inventoryDetailInfoRequestDTO.setWebMerchantCode(itemLevel5ResponsesList.getMerchantCode());
    return inventoryDetailInfoRequestDTO;
  }

  public static void setItemNameByItemPickupPoint(Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap,
      Map<String, String> historyMap, ItemPickupPointRequest modifiedItemPickupPoint, String key) {
    if (l5CodeAndResponseMap.containsKey(
        toL5Id(modifiedItemPickupPoint.getItemSku(), modifiedItemPickupPoint.getPickupPointId()))) {
      historyMap.put(key, l5CodeAndResponseMap.get(toL5Id(modifiedItemPickupPoint.getItemSku(), modifiedItemPickupPoint.getPickupPointId()))
          .getItemName());
    }
  }

  public static boolean isWholesaleFlagChanged(WholesalePriceSkuResponse wholesalePriceSkuResponse,
      Boolean wholesalePriceActivated) {
    return (Constants.ACTIVE_STATUS.equals(wholesalePriceSkuResponse.getSkuStatus()) && Boolean.FALSE.equals(wholesalePriceActivated)) || (
        Constants.INACTIVE_STATUS.equals(wholesalePriceSkuResponse.getSkuStatus()) && Boolean.TRUE.equals(wholesalePriceActivated));
  }

  public static void updateProductBusinessPartner(ProductBusinessPartner productBusinessPartner,
      ProductVariantUpdateRequest request) {
    if (Objects.nonNull(request.getOnline())) {
      productBusinessPartner.setOnline(request.getOnline());
    }
  }

  public static boolean isOnlineFlagChanged(boolean oldValue, Boolean newValue) {
    if (Objects.nonNull(newValue)) {
      return oldValue != newValue;
    }
    return false;
  }

  public static Map<String, String> generateWholesaleHistory(Boolean oldValue, Boolean newValue,
      String itemSku, String pickupPointCode, String itemName) {
    Map<String, String> historyMap = new HashMap<>();
    if (Objects.nonNull(newValue)) {
      if (Objects.nonNull(oldValue) && Boolean.compare(newValue, oldValue) != 0) {
        historyMap.put(Constants.PREVIOUS_VALUE, String.valueOf(oldValue));
        historyMap.put(Constants.CURRENT_VALUE, String.valueOf(newValue));
        historyMap.put(Constants.HISTORY_ACTIVITY,
            UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
        historyMap.put(Constants.ITEM_SKU, itemSku);
        historyMap.put(Constants.PICKUP_POINT_CODE, pickupPointCode);
        historyMap.put(Constants.ITEM_NAME, itemName);
      } else if (Objects.isNull(oldValue)) {
        historyMap.put(Constants.PREVIOUS_VALUE, StringUtils.EMPTY);
        historyMap.put(Constants.CURRENT_VALUE, String.valueOf(newValue));
        historyMap.put(Constants.HISTORY_ACTIVITY,
            UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc() + Constants.NEED_REVISION);
        historyMap.put(Constants.ITEM_SKU, itemSku);
        historyMap.put(Constants.PICKUP_POINT_CODE, pickupPointCode);
        historyMap.put(Constants.ITEM_NAME, itemName);
      }
    }
    return historyMap;
  }

  public static AuditTrailListRequest getAuditTrailRequestForL3History(String businessPartnerCode, String productSku,
      String productName, String activity, String attributeName, String oldValue, String newValue, String changedBy,
      String requestId, String clientHost, String gdnSku, String pickupPointCode) {
    AuditTrailDto auditTrailDto = getAuditTrailDto(activity, oldValue, newValue, gdnSku);
    auditTrailDto.setBusinessPartnerCode(businessPartnerCode);
    auditTrailDto.setProductSku(productSku);
    auditTrailDto.setAttributeName(attributeName);
    auditTrailDto.setName(productName);
    auditTrailDto.setPickupPointCode(pickupPointCode);
    auditTrailDto.setOnlineStatus(true);

    AuditTrailListRequest auditTrailListRequest = new AuditTrailListRequest();
    auditTrailListRequest.setChangedBy(changedBy);
    auditTrailListRequest.setRequestId(requestId);
    auditTrailListRequest.setClientId(clientHost);
    auditTrailListRequest.setUpdateDirectly(true);
    auditTrailListRequest.setAccessChannel(Constants.WEB_CHANNEL_ID);
    auditTrailListRequest.setAuditTrailResponseList(Arrays.asList(auditTrailDto));

    return auditTrailListRequest;
  }


  public static AuditTrailDto getAuditTrailDto(String activity, String oldValue, String newValue, String gdnSku) {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setActionKey(activity);
    auditTrailDto.setOldValue(oldValue);
    auditTrailDto.setNewValue(newValue);
    auditTrailDto.setGdnSku(gdnSku);
    return auditTrailDto;
  }

  public static List<UpdatedProductHistory> getUpdateHistoryFromAuditTrailRequest(
      AuditTrailListRequest auditTrailListRequest) {
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(auditTrailListRequest.getAuditTrailResponseList())) {
      List<AuditTrailDto> auditTrailDtos = auditTrailListRequest.getAuditTrailResponseList();
      auditTrailDtos.forEach(auditTrailDto -> {
        Calendar now = Calendar.getInstance();
        UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
        updatedProductHistory.setProductSku(auditTrailDto.getProductSku());
        updatedProductHistory.setRequestId(auditTrailListRequest.getRequestId());
        updatedProductHistory.setClientHost(auditTrailListRequest.getClientId());
        updatedProductHistory.setChangedBy(auditTrailListRequest.getChangedBy());
        updatedProductHistory.setNewValues(auditTrailDto.getNewValue());
        updatedProductHistory.setOldValues(auditTrailDto.getOldValue());
        updatedProductHistory.setActivity(auditTrailDto.getActionKey());
        updatedProductHistory.setAccessChannel(auditTrailListRequest.getAccessChannel());
        updatedProductHistory.setGdnName(auditTrailDto.getName());
        updatedProductHistory.setBusinessPartnerCode(auditTrailDto.getBusinessPartnerCode());
        updatedProductHistory.setAccessTime(new Date());
        updatedProductHistory.setGdnSku(auditTrailDto.getGdnSku());
        updatedProductHistory.setPickupPointCode(auditTrailDto.getPickupPointCode());
        updatedProductHistory.setOnlineStatus(auditTrailDto.isOnlineStatus());
        updatedProductHistory.setActivatedDate(now.getTime());
        updatedProductHistoryList.add(updatedProductHistory);
      });
    }
    return updatedProductHistoryList;
  }

  public static void validatePickupPoints(String key, Set<String> pickupPointCodesUsedInCreation,
      List<PickupPointResponse> pickupPointResponseList, SimpleStringResponse errorCode, String businessPartnerCode) {
    if (CollectionUtils.isEmpty(pickupPointResponseList)
        || pickupPointResponseList.size() < pickupPointCodesUsedInCreation.size() || pickupPointResponseList.stream().anyMatch(PickupPointResponse::isArchived)
        || pickupPointResponseList.stream()
        .anyMatch(response -> !response.getBusinessPartnerCode().equals(businessPartnerCode))) {
      log.error("Product creation failed for product : {} , pickup point request : {} is not valid ", key,
          pickupPointCodesUsedInCreation);
      errorCode.setResult(ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getDesc());
    }
  }

  public static void validatePickupPointsAndWaitingDeletion(String key, Set<String> pickupPointCodesUsedInCreation,
      List<PickupPointResponse> pickupPointResponseList, SimpleStringResponse errorCode, String businessPartnerCode) {
    if (isPickupPointInvalid(pickupPointCodesUsedInCreation, pickupPointResponseList,
        businessPartnerCode)) {
      log.error("Product creation failed for product : {} , pickup point request : {} is not valid ", key,
          pickupPointCodesUsedInCreation);
      errorCode.setResult(ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getDesc());
    }
  }

  public static boolean isPickupPointInvalid(Set<String> pickupPointCodesUsedInCreation,
      List<PickupPointResponse> pickupPointResponseList, String businessPartnerCode) {
    return CollectionUtils.isEmpty(pickupPointResponseList)
        || pickupPointResponseList.size() < pickupPointCodesUsedInCreation.size() || pickupPointResponseList.stream().anyMatch(PickupPointResponse::isArchived)
        || pickupPointResponseList.stream()
        .anyMatch(response -> !response.getBusinessPartnerCode().equals(businessPartnerCode))
        || pickupPointResponseList.stream().anyMatch(PickupPointResponse::isWaitingDeletion);
  }

  public static boolean isCncActive(ProductVariantUpdateRequest request, boolean cncActive,
      boolean cncForWarehouseFeatureSwitch) {
    if (!cncActive && hasCncActiveAddedPickupPoints(request, cncForWarehouseFeatureSwitch)) {
      cncActive = true;
    }
    return cncActive;
  }

  private static boolean hasCncActiveAddedPickupPoints(ProductVariantUpdateRequest request, boolean cncForWarehouseFeatureSwitch) {
    return CollectionUtils.isNotEmpty(request.getAddPickupPoints()) && request.getAddPickupPoints().stream().anyMatch(
        itemPickupPointRequest -> isItemPickupPointRequestCnc(itemPickupPointRequest,
            cncForWarehouseFeatureSwitch));
  }

  private static boolean isItemPickupPointRequestCnc(ItemPickupPointRequest itemPickupPointRequest,
      boolean cncForWarehouseFeatureSwitch) {
    return (!cncForWarehouseFeatureSwitch && itemPickupPointRequest.isCncActive()) || (
        cncForWarehouseFeatureSwitch && (itemPickupPointRequest.isCncBuyable())
            || itemPickupPointRequest.isCncDisplay());
  }

  public static void validateCreateRequestForFbb(ProductCreationRequest request,
      List<PickupPointResponse> pickupPointResponseList, ProfileResponse profileResponse, String mppAllowedSellers,
      boolean mppForWhEnabled) {
    Map<String, PickupPointResponse> pickupPointResponseMap = pickupPointResponseList.stream()
        .collect(Collectors.toMap(PickupPointResponse::getCode, pickupPointResponse -> pickupPointResponse, (a, b) -> a));
    request.getProductItemRequests().forEach(
        itemCreationRequest -> itemCreationRequest.getPickupPoints().forEach(
            pickupPointCreateRequest -> pickupPointCreateRequest.setFbbActivated(Boolean.TRUE.equals(
                pickupPointResponseMap.get(pickupPointCreateRequest.getPickupPointId()).isFbbActivated()))));
    for (ProductItemCreationRequest itemCreationRequest : request.getProductItemRequests()) {
      long countOfFbbPickupPoints = itemCreationRequest.getPickupPoints().stream().filter(
          pickupPointCreateRequest -> pickupPointResponseMap.get(pickupPointCreateRequest.getPickupPointId()).isFbbActivated()).count();
      if (countOfFbbPickupPoints > 1 && !isEligibleForMppForWH(mppForWhEnabled, profileResponse,
          mppAllowedSellers)) {
        log.error("Product creation failed for productCode : {} , because of L5 : {} , error : {} ",
            request.getProductCode(), itemCreationRequest, ApiErrorCode.ONLY_ONE_FBB_PICKUP_POINT_ALLOWED_PER_L4.getDesc());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.ONLY_ONE_FBB_PICKUP_POINT_ALLOWED_PER_L4.getDesc());
      }
    }
  }

  private static boolean isEligibleForMppForWH(boolean mppForWhEnabled, ProfileResponse profileResponse,
      String mppAllowedSellers) {
    return (mppForWhEnabled && isMppEnabled(profileResponse, mppAllowedSellers));
  }

  public static boolean isDataUpdated(boolean contentChanged, boolean contentUpdateForUrl,
      boolean contentUpdateForSpecialAttributes, boolean freeSampleFlagChanged, boolean off2OnChannelFlagChanged,
      boolean isDimensionUpdated, boolean isProductTypeChanged,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList, ProductLevel3 product, boolean isPreOrder) {
    return contentChanged || contentUpdateForUrl || contentUpdateForSpecialAttributes || !CollectionUtils.isEmpty(restrictedKeywordsByFieldList) || freeSampleFlagChanged
        || off2OnChannelFlagChanged || isDimensionUpdated || isProductTypeChanged || CollectionUtils.isNotEmpty(product.getNewlyAddedItems())
        || CollectionUtils.isNotEmpty(product.getDeletedItems()) || isPreOrder;
  }

  public static boolean isDataUpdated(ProductDetailEditDTO productDetailEditDTO, ProductLevel3 product) {
    return productDetailEditDTO.isContentChanged() ||
      productDetailEditDTO.isContentUpdateForUrl() ||
      productDetailEditDTO.isContentUpdateForSpecialAttributes() ||
      productDetailEditDTO.isFreeSampleFlagChanged() ||
      productDetailEditDTO.isOff2OnChannelFlagChanged() ||
      productDetailEditDTO.isDimensionUpdated() ||
      productDetailEditDTO.isProductTypeChanged() ||
      !CollectionUtils.isEmpty(productDetailEditDTO.getRestrictedKeywordsByFieldList()) ||
      !CollectionUtils.isEmpty(product.getNewlyAddedItems()) ||
      !CollectionUtils.isEmpty(product.getDeletedItems()) || productDetailEditDTO.isPreOrderChange()
      || Boolean.TRUE.equals(productDetailEditDTO.getVideoUpdated()) || product.isSizeChartChanged()
        || product.isDistributionInfoUpdated() || product.isBrandUpdated() || product.isCategoryUpdated();
  }

  public static boolean publishPDTContentEditEvent(boolean contentChanged,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList, boolean brandChanged) {
    return contentChanged || !CollectionUtils.isEmpty(restrictedKeywordsByFieldList) || brandChanged;
  }

  public static boolean eligibleForVariantImageUpdate(List<ProductItemImageRequest> updatedProductImageRequest,
      List<ProductItemImageRequest> newProductImageRequest, boolean addDeleteVariantSwitch) {
    return (!addDeleteVariantSwitch || CollectionUtils.isNotEmpty(updatedProductImageRequest)
        || CollectionUtils.isNotEmpty(newProductImageRequest));
  }

  public static boolean isEligibleForLogisticsUpdate(AddDeleteVariantRequest addDeleteVariantRequest) {
    return Objects.nonNull(addDeleteVariantRequest) && CollectionUtils.isNotEmpty(
        addDeleteVariantRequest.getAddVariantsList());
  }

  public static boolean isPreOrderChanged(boolean isPreOrderChange, PreOrderRequest preOrderRequest) {
    return isPreOrderChange || Objects.nonNull(preOrderRequest);
  }

  public static boolean checkForChangedRequest(boolean contentUpdateForSpecialAttributes, boolean freeSampleFlagChanged,
      boolean off2OnChannelFlagChanged, boolean isDimensionUpdated, boolean isProductTypeChanged,
      boolean isPreOrderChange, boolean sizeChartChanged, boolean videoUrlUpdated) {
    return contentUpdateForSpecialAttributes || freeSampleFlagChanged || off2OnChannelFlagChanged || isDimensionUpdated
        || isProductTypeChanged || isPreOrderChange || sizeChartChanged || videoUrlUpdated;
  }

  public static boolean checkIfB2bFieldsChanged(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointListingResponse itemPickupPointListingResponse, boolean isItemChanged) {
    if (Objects.nonNull(itemPickupPointRequest.getB2bFields())) {
      if (Objects.isNull(itemPickupPointListingResponse.getB2bFields())) {
        isItemChanged = true;
      } else {
        if (itemPickupPointRequest.getB2bFields().isManaged() != itemPickupPointListingResponse.getB2bFields().isManaged()) {
          isItemChanged = true;
        }
        if (!itemPickupPointRequest.getB2bFields().getPrice()
            .equals(itemPickupPointListingResponse.getB2bFields().getBasePrice())) {
          isItemChanged = true;
        }
      }
      ViewConfigResponse b2bViewConfigResponse =
          itemPickupPointListingResponse.getViewConfigs().stream().filter(viewConfig -> Constants.B2B_CHANNEL.equals(viewConfig.getChannelId()))
              .findFirst().orElse(new ViewConfigResponse());
      if (b2bViewConfigResponse.isBuyable() != itemPickupPointRequest.getB2bFields().isBuyable()) {
        isItemChanged = true;
      }
      if (b2bViewConfigResponse.isDisplay() != itemPickupPointRequest.getB2bFields().isDisplay()) {
        isItemChanged = true;
      }
    }
    return isItemChanged;
  }

  public static List<String> salesChannelFromProfileResponse(ProfileResponse profileResponse) {
    List<String> salesChannel = new ArrayList<>();
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && CollectionUtils.isNotEmpty(profileResponse.getCompany().getSalesChannel())) {
      salesChannel = profileResponse.getCompany().getSalesChannel();
    }
    return salesChannel;
  }

  public static void setB2cActivatedFromProfileResponse(ProductVariantUpdateRequest productVariantUpdateRequest,
      ProfileResponse profileResponse, boolean overrideFlagsFromSellerSalesChannel) {
    List<String> salesChannel = salesChannelFromProfileResponse(profileResponse);
    boolean pureB2bSeller = false;
    boolean b2bSeller = false;
    if (!salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
      productVariantUpdateRequest.setB2cActivated(false);
      pureB2bSeller = true;
    }
    if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      b2bSeller = true;
    } else {
      productVariantUpdateRequest.setB2bActivated(false);
    }
    if (overrideFlagsFromSellerSalesChannel) {
      log.info("Overriding flags for pure B2B seller : {} ", productVariantUpdateRequest.getProductSku());
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())) {
        for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest.getProductItems()) {
          if (CollectionUtils.isNotEmpty(productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())) {
            for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
              overrideFlagsBasedOnSellerChannel(pureB2bSeller, b2bSeller, itemPickupPointRequest);
            }
          }
        }
      }
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints())) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
          overrideFlagsBasedOnSellerChannel(pureB2bSeller, b2bSeller, itemPickupPointRequest);
        }
      }
    }
  }

  private static void overrideFlagsBasedOnSellerChannel(boolean pureB2bSeller, boolean b2bSeller,
      ItemPickupPointRequest itemPickupPointRequest) {
    if (pureB2bSeller) {
      itemPickupPointRequest.setDisplay(false);
      itemPickupPointRequest.setBuyable(false);
      itemPickupPointRequest.setCncActive(false);
    }
    if (!b2bSeller) {
      itemPickupPointRequest.setB2bFields(null);
    }
  }

  public static void setB2bAndB2cFLags(ProductVariantUpdateRequest productVariantUpdateRequest,
      ItemPickupPointListingResponse parentProductL5Response, EditFlagChangesDTO editFlagChangesDTO) {
    if (Objects.nonNull(productVariantUpdateRequest.getB2bActivated()) && !productVariantUpdateRequest.getB2bActivated()
        .equals(parentProductL5Response.isB2bActivatedAtL3Level())) {
      editFlagChangesDTO.setB2bFlagChangedAtL3Level(true);
    }
    if (Objects.nonNull(productVariantUpdateRequest.getB2cActivated()) && !productVariantUpdateRequest.getB2cActivated()
        .equals(parentProductL5Response.isB2cActivatedAtL3Level())) {
      editFlagChangesDTO.setB2cFlagChangedAtL3Level(true);
    }
  }

  public static void overrideAutoNeedRevisionAndTakeDownFlags(
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse) {
    if (Objects.nonNull(autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType())) {
      if (RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType()
          == autoNeedRevisionAndForceReviewResponse.getRestrictedKeywordsByFieldAndActionType().getAction()) {
        autoNeedRevisionAndForceReviewResponse.setForceReview(false);
        autoNeedRevisionAndForceReviewResponse.setSendProductToReview(false);
        autoNeedRevisionAndForceReviewResponse.setAutoNeedRevision(false);
      }
    }
  }

  public static ApiErrorCode validateAddDeleteRequest(ProductLevel3 product, ProductL3Response savedProductData,
      ApiErrorCode apiErrorCode, boolean addDeleteForWarehouseSwitch, ProfileResponse profileResponse,
      CategoryDetailResponse categoryDetailResponse, boolean validateAttributeAtProductAndCategoryLevel,
      AttributeCodeValueValueTypeDetails existingDefiningAttributeDetails, boolean uniqueValueTypeAdditionEnabled) {
    log.info("Validation add/delete variant request for productSku : {} ", product.getProductSku());
    List<ProductAttributeDTO> definingAttributes = savedProductData.getDefiningAttributes();
    List<String> existingItemAttributeValues = new ArrayList<>();
    getExistingItemAttributeValues(product, definingAttributes, existingItemAttributeValues,
        existingDefiningAttributeDetails);
    log.info("Validation add/delete variant request for productSku : {} existing items : {}  ",
        product.getProductSku(), existingItemAttributeValues);
    List<String> newlyAddedItems = new ArrayList<>();
    ApiErrorCode valueAndValueTypeError = ValueTypeUtil.validateValueAndValueType(product,
        existingDefiningAttributeDetails.isValueTypeAdditionForDefiningAttributes(),
        uniqueValueTypeAdditionEnabled);
    if (Objects.nonNull(valueAndValueTypeError)) {
      return valueAndValueTypeError;
    }
    apiErrorCode =
        validateNewlyAddedItems(product, apiErrorCode, existingItemAttributeValues, newlyAddedItems,
            existingDefiningAttributeDetails.getSizeChartValueTypeDelimiter(),
            existingDefiningAttributeDetails.isValueTypeAdditionForDefiningAttributes());
    if (Objects.nonNull(apiErrorCode)) {
      return apiErrorCode;
    }
    if (addDeleteForWarehouseSwitch && Objects.nonNull(product.getAttributes())) {
      apiErrorCode = validateAddDeleteForWarehouse(product, definingAttributes, profileResponse,
          categoryDetailResponse, validateAttributeAtProductAndCategoryLevel);
    }
    return apiErrorCode;
  }

  private static void getExistingItemAttributeValues(ProductLevel3 product,
      List<ProductAttributeDTO> definingAttributes, List<String> existingItemAttributeValues,
      AttributeCodeValueValueTypeDetails existingDefiningAttributeDetails) {
    if (CollectionUtils.isNotEmpty(definingAttributes)) {
      for (ProductAttributeDTO productAttributeDTO : definingAttributes) {
        String existingItem = product.getProductName();
        if (CollectionUtils.isNotEmpty(productAttributeDTO.getProductAttributeDetails())) {
          Set<String> sortedAttributeValues = new TreeSet<>();
          for (ProductAttributeDetailDTO productAttributeDetailDTO : productAttributeDTO.getProductAttributeDetails()) {
            sortedAttributeValues.add(ValueTypeUtil.getAttributeCodeValueAndValueTypeString(productAttributeDetailDTO,
                existingDefiningAttributeDetails));
          }
          for (String sortedAttributeValue : sortedAttributeValues) {
            existingItem = existingItem.concat(sortedAttributeValue);
          }
        }
        existingItemAttributeValues.add(existingItem);
      }
    }
  }

  private static ApiErrorCode validateNewlyAddedItems(ProductLevel3 product, ApiErrorCode apiErrorCode,
      List<String> existingItemAttributeValues, List<String> newlyAddedItems, String sizeChartValueTypeDelimiter,
      boolean valueTypeAdditionForDefiningAttributes) {
    for (ProductItemLevel3 productItemLevel3 : product.getNewlyAddedItems()) {
      String newlyAddedItem = product.getProductName();
      Set<String> sortedAttributeValues = new TreeSet<>();
      if (MapUtils.isNotEmpty(productItemLevel3.getItemAttributesMap())) {
        for (Map.Entry<String, String> itemAttributeMap : productItemLevel3.getItemAttributesMap().entrySet()) {
          sortedAttributeValues.add(ValueTypeUtil.getAttributeCodeValueAndValueTypeString(itemAttributeMap,
              productItemLevel3.getAttributesValueTypeMap(), sizeChartValueTypeDelimiter,
              valueTypeAdditionForDefiningAttributes));
        }
      }
      for (String sortedAttributeValue : sortedAttributeValues) {
        newlyAddedItem = newlyAddedItem.concat(sortedAttributeValue);
      }
      log.info("Validation add/delete variant request for productSku : {} new item : {}  ", product.getProductSku(),
          newlyAddedItem);
      if (existingItemAttributeValues.contains(newlyAddedItem)) {
        log.error(
            "Error when validating add/delete request for productSku : {} for item attributes : {} ",
            product.getProductSku(), newlyAddedItem);
        apiErrorCode = ApiErrorCode.INVALID_ADD_DELETE_REQUEST;
      } else if (newlyAddedItems.contains(newlyAddedItem)) {
        log.error(
            "Error when validating add/delete request for productSku : {} duplicate item addition : {} ",
            product.getProductSku(), newlyAddedItem);
        apiErrorCode = ApiErrorCode.INVALID_ADD_DELETE_DUPLICATE_REQUEST;
      } else {
        newlyAddedItems.add(newlyAddedItem);
      }
    }
    return apiErrorCode;
  }

  public static ApiErrorCode validateAddDeleteForWarehouse(ProductLevel3 productUpdateRequest,
      List<ProductAttributeDTO> savedDefiningAttributes, ProfileResponse profileResponse,
      CategoryDetailResponse categoryDetailResponse, boolean validateAttributeAtProductAndCategoryLevelSwitch) {
    ApiErrorCode apiErrorCode = null;
    if (Objects.nonNull(profileResponse.getCompany()) && profileResponse.getCompany().getInventoryFulfillment().equals(Constants.WAREHOUSE_SELLER_INVENTORY_FULFILLMENT)) {
      Set<String> productAttributes = new HashSet<>();
      List<String> productAttributeValues = new ArrayList<>();
      for (ProductLevel3Attribute productLevel3Attribute : productUpdateRequest.getAttributes()) {
        productAttributes.add(productLevel3Attribute.getAttributeCode());
        if (CollectionUtils.isNotEmpty(productLevel3Attribute.getValues())
            && productLevel3Attribute.isVariantCreation()) {
          productAttributeValues = productLevel3Attribute.getValues();
        }
      }
      if (isEmptyAttributeValues(productAttributeValues)) {
        return ApiErrorCode.EMPTY_ATTRIBUTE_VALUE;
      }
      // checking attribute codes which are existing but still present in add/delete request
      List<String> definingAttributeCode = new ArrayList<>();
      for (ProductAttributeDTO definingAttributes : savedDefiningAttributes) {
        for (ProductAttributeDetailDTO attributeDetailDTO : definingAttributes.getProductAttributeDetails()) {
          definingAttributeCode.add(attributeDetailDTO.getAttributeCode());
          if (validateAttributeAtProductAndCategoryLevel(categoryDetailResponse, attributeDetailDTO.getAttributeCode(),
              validateAttributeAtProductAndCategoryLevelSwitch) && !productAttributes.contains(
              attributeDetailDTO.getAttributeCode())) {
            log.info("Invalid add/delete request for attributeCode : {} ", attributeDetailDTO.getAttributeCode());
            apiErrorCode = ApiErrorCode.INVALID_DELETE_REQUEST;
            break;
          }
        }
      }
      // checking attribute codes which are requested for add/delete but are not existing one's
      for (ProductLevel3Attribute productLevel3Attribute : productUpdateRequest.getAttributes()) {
        if (validateAttributeAtProductAndCategoryLevel(categoryDetailResponse,
            productLevel3Attribute.getAttributeCode(),
            validateAttributeAtProductAndCategoryLevelSwitch) && !definingAttributeCode.contains(
            productLevel3Attribute.getAttributeCode())
            && productLevel3Attribute.isVariantCreation()) {
          log.info("Invalid add/delete request for attributeCode : {} ", productLevel3Attribute.getAttributeCode());
          apiErrorCode = ApiErrorCode.INVALID_ADD_REQUEST;
          break;
        }
      }
    }
    return apiErrorCode;
  }

  private static boolean validateAttributeAtProductAndCategoryLevel(CategoryDetailResponse categoryDetailResponse,
      String attributeCode, boolean validateCategoryAndProductAttribute) {
    boolean isAttributePresentAtProductAndCategory = true;
    List<String> categoryAttributeCodes = new ArrayList<>();
    if (validateCategoryAndProductAttribute) {
      if (Objects.nonNull(categoryDetailResponse)) {
        categoryAttributeCodes = categoryDetailResponse.getCategoryAttributes().stream().map(CategoryAttributeResponse::getAttribute).filter(Objects::nonNull)
            .map(AttributeResponse::getAttributeCode).collect(Collectors.toList());
      }
      if (!categoryAttributeCodes.contains(attributeCode)) {
        isAttributePresentAtProductAndCategory = false;
      }
    }
    return isAttributePresentAtProductAndCategory;
  }

  private static boolean isEmptyAttributeValues(List<String> productAttributeValues) {
    return productAttributeValues.stream().anyMatch(attribute -> attribute.equals(StringUtils.EMPTY));
  }

  public static ApiErrorCode validateAddDeleteVariantsRequest(ProductLevel3 product,
      ProductL3Response savedProductData, boolean addDeleteForWarehouseSwitch, ProfileResponse profileResponse,
      CategoryDetailResponse categoryDetailResponse, boolean validateAttributeAtProductAndCategory,
      AttributeCodeValueValueTypeDetails existingDefiningAttributeDetails, boolean deleteVariantValidationSwitch, boolean uniqueValueTypeAdditionEnabled,
      boolean ranchIntegrationEnabled) {
    ApiErrorCode apiErrorCode = null;

    // last attribute value delete check
    boolean isDeleteRequest = !product.getDeletedItems().isEmpty();
    if (isDeleteRequest && product.getNewlyAddedItems().isEmpty()
        && savedProductData.getDefiningAttributes().size() == Constants.SIZE_ONE) {
      log.error("Delete not allowed as it is the last existing value for ProductSku : {} ", product.getProductSku());
      return ApiErrorCode.DELETE_NOT_ALLOWED_FOR_LAST_ATTRIBUTE_VALUE;
    }

    if (deleteVariantValidationSwitch && isDeleteRequest && product.getNewlyAddedItems().isEmpty()
        && savedProductData.getItemCount() <= product.getDeletedItems().size()) {
      log.error("Delete not allowed as it is the last existing value for ProductSku : {} ", product.getProductSku());
      return ApiErrorCode.DELETE_NOT_ALLOWED_FOR_LAST_ATTRIBUTE_VALUE;
    }

    if (CollectionUtils.isNotEmpty(product.getNewlyAddedItems())) {
      apiErrorCode = CommonUtils.validateAddDeleteRequest(product, savedProductData, apiErrorCode,
          addDeleteForWarehouseSwitch, profileResponse, categoryDetailResponse,
          validateAttributeAtProductAndCategory, existingDefiningAttributeDetails,
          uniqueValueTypeAdditionEnabled);
    }
    if (ranchIntegrationEnabled) {
      if ((savedProductData.getDistributionMappingStatus().equalsIgnoreCase(Constants.DISTRIBUTION)
          || savedProductData.getDistributionMappingStatus().equalsIgnoreCase(Constants.PURE_DISTRIBUTION)) && (
          CollectionUtils.isNotEmpty(product.getDeletedItems()) || CollectionUtils.isNotEmpty(
              product.getNewlyAddedItems()))) {
        log.error("Add delete variants not allowed for a distribution product for productCode : {} ",
            savedProductData.getProductCode());
        apiErrorCode = ApiErrorCode.ADD_DELETE_VARIANTS_NOT_ALLOWED_DISTRIBUTION_PRODUCT;
      }
    }
    return apiErrorCode;
  }

  public static boolean isEligibleForAutoReject(EditProductResponse editResponse) {
    return RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType() == editResponse.getAction()
        && Objects.nonNull(editResponse.getProfileResponse()) && !editResponse.getProfileResponse().isTrustedSeller();
  }

  public static boolean isEligibleForAutoNeedRevision(EditProductResponse editResponse) {
    return RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType() == editResponse.getAction()
        && Objects.nonNull(editResponse.getProfileResponse()) && !editResponse.getProfileResponse().isTrustedSeller();
  }

  public static Map<String, Boolean> getPickupPointCodesWithFbbActivatedValue(
      List<PickupPointResponse> pickupPointCodeResponses) throws Exception {
    Map<String, Boolean> pickupPointCodesWithFbbActivatedValue = new HashMap<>();
    if (CollectionUtils.isNotEmpty(pickupPointCodeResponses)) {
      pickupPointCodeResponses.forEach(
          pickupPointResponse -> pickupPointCodesWithFbbActivatedValue.put(pickupPointResponse.getCode(),
              pickupPointResponse.isFbbActivated()));
    }
    return pickupPointCodesWithFbbActivatedValue;
  }

  public static boolean isItemNameEmptyForAnyNewlyAddedItem(ProductL3UpdateRequest productL3UpdateRequest, boolean overrideItemNameGeneration) {
    return Objects.nonNull(productL3UpdateRequest) && productL3UpdateRequest.getProductItems().stream().anyMatch(productItem -> productItem.isNewlyAddedItem() && (
        StringUtils.isBlank(productItem.getItemName()) || overrideItemNameGeneration));
  }

  public static Set<String> getNewlyAddedAttributeCodes(ProductL3UpdateRequest productL3UpdateRequest,
      Map<String, String> attributeCodeAndIdMapFromXProduct, String familyColorAttributeCode) {
    //attributeMap is map of attribute code and value
    return productL3UpdateRequest.getProductItems().stream().flatMap(
            productVariantPriceStockAndImagesRequest -> productVariantPriceStockAndImagesRequest.getAttributesMap()
                .keySet().stream().filter(attributeCode -> !attributeCodeAndIdMapFromXProduct.containsKey(attributeCode)))
        .filter(attributeCode -> !StringUtils.equals(familyColorAttributeCode, attributeCode))
        .collect(Collectors.toSet());
  }

  public static boolean isCombinedEditAndAutoReject(boolean isNeedCorrection, boolean combinedEditFlowEnabled) {
    return !isNeedCorrection && combinedEditFlowEnabled;
  }

  public static String extractMerchantCodeFromProductSku(String productSku) {
    int lastIndex = productSku.lastIndexOf(Constants.HYPHEN);
    return productSku.substring(0, lastIndex);
  }

  public static boolean isDefinitiveActionToBeSkipped(int action) {
    return action == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
        || action == RestrictedKeywordActionType.AUTO_REJECT.getRestrictedKeywordActionType();
  }

  public static boolean isItemSku(String itemSku) {
    if (StringUtils.isNotBlank(itemSku)) {
      Matcher matcher = Pattern.compile(ITEM_SKU_PATTERN).matcher(itemSku);
      return matcher.matches();
    }
    return false;
  }

  public static boolean isProductSku(String productSku) {
    if (StringUtils.isNotBlank(productSku)) {
      Matcher matcher = Pattern.compile(PRODUCT_SKU_PATTERN).matcher(productSku);
      return matcher.matches();
    }
    return false;
  }

  public static void overrideVariantNameForNewlyAddedVariants(ProductL3UpdateRequest productL3UpdateRequest,
      ProductVariantPriceStockAndImagesRequest productVariantUpdateRequest, Map<String, String> attributeCodeAndIdMapFromPCB, ProductL3Response savedProductData) {
    Map<String, String> attributeIdAndValueMap = new HashMap<>();
    for (Map.Entry<String, String> entry : productVariantUpdateRequest.getAttributesMap().entrySet()) {
      String attributeCode = entry.getKey();
      String attributeValue = entry.getValue();
      if (attributeCodeAndIdMapFromPCB.containsKey(attributeCode)) {
        attributeIdAndValueMap.put(attributeCodeAndIdMapFromPCB.get(attributeCode), attributeValue);
      }
    }
    // sort by keys i.e attribute Id and join the corresponding value by space
    String definingAttributesString =
        attributeIdAndValueMap.entrySet().stream().sorted(Map.Entry.comparingByKey()).map(Map.Entry::getValue).collect(Collectors.joining(StringUtils.SPACE));
    if (StringUtils.isNotBlank(definingAttributesString)) {
      StringBuilder newlyGeneratedItemName =
          getNewlyGeneratedItemName(productL3UpdateRequest, savedProductData,
              definingAttributesString);
      productVariantUpdateRequest.setItemName(newlyGeneratedItemName.toString());
    } else {
      productVariantUpdateRequest.setItemName(getProductName(productL3UpdateRequest, savedProductData));
    }
  }

  private static StringBuilder getNewlyGeneratedItemName(ProductL3UpdateRequest productL3UpdateRequest, ProductL3Response savedProductData,
      String definingAttributesString) {
    // get product name from request, if empty use name from master data in x-product
    String productName = getProductName(productL3UpdateRequest, savedProductData);
    return new StringBuilder().append(productName).append(StringUtils.SPACE).append(definingAttributesString);
  }

  private static String getProductName(ProductL3UpdateRequest productL3UpdateRequest,
      ProductL3Response savedProductData) {
    return Optional.ofNullable(productL3UpdateRequest).map(ProductL3UpdateRequest::getProductName)
        .filter(StringUtils::isNotBlank).orElseGet(() -> Optional.ofNullable(savedProductData).map(ProductL3Response::getMasterDataProduct)
            .map(MasterDataProductDTO::getProductName).filter(StringUtils::isNotBlank).orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK)));
  }

  public static Map<String, Boolean> getItemSkuMarkForDeleteMapping(
      List<ProductItemBusinessPartner> productItemBusinessPartners) {
    return productItemBusinessPartners.stream().filter(Objects::nonNull).collect(
        Collectors.toMap(ProductItemBusinessPartner::getGdnProductItemSku,
            ProductItemBusinessPartner::isMarkForDelete, (oldValue, newValue) -> oldValue));
  }

  public static Map<String, String> getItemIdItemSkuMap(List<ProductItemBusinessPartner> productItemBusinessPartners) {
    return productItemBusinessPartners.stream().filter(Objects::nonNull).collect(
        Collectors.toMap(ProductItemBusinessPartner::getProductItemId, ProductItemBusinessPartner::getGdnProductItemSku,
            (oldValue, newValue) -> oldValue));
  }


  public static Map<String, String> getItemSkuItemCodeMap(Map<String, String> itemIdItemSkuMap,
      SimpleStringMapResponse itemIdItemCodeMap) {
    Map<String, String> itemSkuItemCodeMap = new HashMap<>();
    for (Map.Entry<String, String> entry : itemIdItemCodeMap.getMapResponse().entrySet()) {
      String itemId = entry.getKey();
      String itemCode = entry.getValue();
      String itemSku = itemIdItemSkuMap.get(itemId);
      itemSkuItemCodeMap.put(itemSku, itemCode);
    }
    return itemSkuItemCodeMap;
  }

  public static void setProductLevel3InventoryList(ItemPickupPointDto itemPickupPointDto,
      Map<String, String> itemSkuItemCodeMap, ProfileResponse profileResponse,
      List<ProductLevel3Inventory> productLevel3InventoryList,
      Optional<PickupPointResponse> pickupPointResponse, Date preOrderDate, boolean mppForWhEnabled,
      boolean faasFeatureSwitch, boolean preOrderFeatureSwitch) {
    if (itemSkuItemCodeMap.containsKey(itemPickupPointDto.getItemSku())) {
      ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
      productLevel3Inventory.setWebItemSku(itemPickupPointDto.getItemSku());
      productLevel3Inventory.setWebPickupPointCode(itemPickupPointDto.getPickupPointCode());
      productLevel3Inventory.setWebSyncStock(pickupPointResponse.map(
          response -> CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, response.isFbbActivated(), profileResponse)).orElse(false));
      productLevel3Inventory.setWebAvailable(0);
      productLevel3Inventory.setWebMinAlert(0);
      productLevel3Inventory.setWebMerchantCode(profileResponse.getBusinessPartnerCode());
      productLevel3Inventory.setWarehouseItemSku(itemSkuItemCodeMap.get(itemPickupPointDto.getItemSku()));
      productLevel3Inventory.setProductSku(itemPickupPointDto.getProductSku());
      productLevel3Inventory.setFbbPP(
          pickupPointResponse.map(PickupPointResponse::isFbbActivated).orElse(false) && mppForWhEnabled);
      productLevel3Inventory.setDistributionPickupPoint(Boolean.TRUE.equals(
          pickupPointResponse.map(PickupPointResponse::getFlags).orElse(new HashMap<>())
              .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false)));
      CommonUtils.setPreOrderFields(preOrderFeatureSwitch, profileResponse, preOrderDate,
          productLevel3Inventory, ZERO);
      if (isPurchaseOrderPurchaseTerm(profileResponse)) {
        productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
      } else {
        productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
      }
      productLevel3InventoryList.add(productLevel3Inventory);
    }
  }


  public static Map<String, List<String>> getItemSkuAndPickupPointMap(
      Collection<ItemPickupPointCodeResponse> itemPickupPointCodeResponses) {
    return Optional.ofNullable(itemPickupPointCodeResponses).orElse(new ArrayList<>()).stream().collect(Collectors.groupingBy(ItemPickupPointCodeResponse::getItemSku,
            Collectors.mapping(ItemPickupPointCodeResponse::getPickupPointCode, Collectors.toList())));
  }

  public static void setPriceForQuickEditRequestSet(Set<PriceDTO> price, QuickEditV2Request quickEditV2Request) {
    ProductLevel3PriceRequest productLevel3PriceRequest = quickEditV2Request.getPrice();
    // set saved price data if it's not eligible for price update, so that x-product is not impacted
    price.stream().findFirst().ifPresent(priceDTO -> {
      BeanUtils.copyProperties(priceDTO, productLevel3PriceRequest, "price", "salePrice");
      productLevel3PriceRequest.setPrice(priceDTO.getListPrice());
      productLevel3PriceRequest.setSalePrice(priceDTO.getOfferPrice());
    });
    quickEditV2Request.setPrice(productLevel3PriceRequest);
  }

  public static void sanitizeSellerSku(boolean sanitizeProductNameAndSellerSku,
      ProductLevel3QuickEditRequest request) {
    if (sanitizeProductNameAndSellerSku && CollectionUtils.isNotEmpty(request.getQuickEditRequests())) {
      for (QuickEditRequest quickEditRequest : request.getQuickEditRequests()) {
        if (StringUtils.isNotBlank(quickEditRequest.getSellerSku())) {
          quickEditRequest.setSellerSku(ValidationUtil.validateDataForProductName(quickEditRequest.getSellerSku()));
        }
      }
    }
  }

  public static Boolean setSchedulesForL5EditRequest(ItemPickupPointRequest itemPickupPointRequest,
      ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest, Set<String> l5sWithActiveSchedules, Boolean scheduleRemovedForStatusUpdate,
      boolean cncForWarehouseFeatureSwitch) {

    //for newly added variants item sku will be blank and in that case control should not got in this block
    if (StringUtils.isNotBlank(itemPickupPointQuickEditRequest.getItemSku()) && l5sWithActiveSchedules.contains(
        itemPickupPointQuickEditRequest.getItemSku().concat(Constants.HYPHEN)
            .concat(itemPickupPointQuickEditRequest.getPickupPointCode()))) {
      // if there is active schedule and status in the update request is Offline , we will remove
      // schedule, validation on new schedule is done earlier and cannot be done for bulk
      if (!ProductLevel3Status.OFFLINE.name().equals(itemPickupPointQuickEditRequest.getStatus())
          || (cncForWarehouseFeatureSwitch && StringUtils.isNotBlank(itemPickupPointQuickEditRequest.getCncStatus())
          && !ProductLevel3Status.OFFLINE.name().equals(itemPickupPointQuickEditRequest.getCncStatus())
          && !ProductLevel3Status.ONLINE.name().equals(itemPickupPointQuickEditRequest.getCncStatus()))) {
        itemPickupPointRequest.setScheduleRemoval(true);
        itemPickupPointRequest.setBuyableSchedule(null);
        itemPickupPointRequest.setDiscoverableSchedule(null);
        scheduleRemovedForStatusUpdate = true;
      }
      // If CNC status is online, buyable and discoverable schedules should have same start and end time
      if (cncForWarehouseFeatureSwitch && ProductLevel3Status.ONLINE.name()
          .equals(itemPickupPointQuickEditRequest.getCncStatus()) && !itemPickupPointRequest.isScheduleRemoval()) {
        // If buyable is non-null, use it to set discoverable. Else if discoverable is non-null, use to set buyable
        // If both conditions are false, schedules are currently null
        if (Objects.nonNull(itemPickupPointRequest.getBuyableSchedule())) {
          itemPickupPointRequest.setDiscoverableSchedule(
              com.gda.mta.product.dto.DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(itemPickupPointRequest.getBuyableSchedule().getStartDateTime())
                  .endDateTime(itemPickupPointRequest.getBuyableSchedule().getEndDateTime()).build());
        } else if (Objects.nonNull(itemPickupPointRequest.getDiscoverableSchedule())) {
          itemPickupPointRequest.setBuyableSchedule(
              com.gda.mta.product.dto.BuyableScheduleRequest.builder().buyable(true).startDateTime(
                      itemPickupPointRequest.getDiscoverableSchedule().getStartDateTime()).endDateTime(itemPickupPointRequest.getDiscoverableSchedule().getEndDateTime())
                  .build());
        }
      }
    }

    itemPickupPointQuickEditRequest.setScheduleUpdate(itemPickupPointRequest.isScheduleRemoval());

    if (Objects.nonNull(itemPickupPointRequest.getBuyableSchedule())) {
      BuyableScheduleRequest buyableSchedule = new BuyableScheduleRequest();
      BeanUtils.copyProperties(itemPickupPointRequest.getBuyableSchedule(), buyableSchedule);
      buyableSchedule.setBuyable(true);
      itemPickupPointQuickEditRequest.setBuyableSchedule(buyableSchedule);
    }

    if (Objects.nonNull(itemPickupPointRequest.getDiscoverableSchedule())) {
      DiscoverableScheduleRequest discoverableScheduleRequest = new DiscoverableScheduleRequest();
      BeanUtils.copyProperties(itemPickupPointRequest.getDiscoverableSchedule(),
          discoverableScheduleRequest);
      discoverableScheduleRequest.setDiscoverable(true);
      itemPickupPointQuickEditRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    }

    return scheduleRemovedForStatusUpdate;
  }

  public static void getSchedulesUpdatedL5(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap,
      ItemPickupPointListingResponse itemPickupPointListingResponse, ItemPickupPointRequest itemPickupPointRequest, boolean schedulesAddEditEnabled,
      boolean cncForWarehouseFeatureSwitch) {
    String l5Code = itemPickupPointRequest.getItemSku() + Constants.HYPHEN + itemPickupPointRequest.getPickupPointId();
    if (schedulesAddEditEnabled && Objects.nonNull(itemPickupPointListingResponse)) {

      BuyableScheduleResponse buyableScheduleResponse = extractFirstBuyableScheduleResponse(itemPickupPointListingResponse).orElse(null);

      DiscoverableScheduleResponse discoverableScheduleResponse =
          extractFirstDiscoverableScheduleResponse(itemPickupPointListingResponse).orElse(null);
      // for schedule removal add both Change types and return immediately
      if (removeScheduleForStatusChange(itemPickupPointRequest, discoverableScheduleResponse,
          buyableScheduleResponse, cncForWarehouseFeatureSwitch)) {
        log.info("Processing removal of schedules for {} ",
            itemPickupPointRequest.getItemSku().concat(Constants.HYPHEN).concat(itemPickupPointRequest.getPickupPointId()));
        removeScheduleAndPopulateScheduleUpdateMap(l5XScheduleUpdateChangeTypeMap,
            buyableScheduleResponse, l5Code, discoverableScheduleResponse);
        return;
      }

      com.gda.mta.product.dto.BuyableScheduleRequest buyableScheduleRequest =
          Optional.of(itemPickupPointRequest).map(ItemPickupPointRequest::getBuyableSchedule).orElse(null);
      com.gda.mta.product.dto.DiscoverableScheduleRequest discoverableScheduleRequest =
          Optional.of(itemPickupPointRequest).map(ItemPickupPointRequest::getDiscoverableSchedule).orElse(null);
      processBuyableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code, buyableScheduleResponse,
          buyableScheduleRequest, itemPickupPointRequest.isScheduleRemoval());
      processDiscoverableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code,
          discoverableScheduleResponse, discoverableScheduleRequest, itemPickupPointRequest.isScheduleRemoval());
    }
  }

  private static void removeScheduleAndPopulateScheduleUpdateMap(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap,
      BuyableScheduleResponse buyableScheduleResponse, String l5Code, DiscoverableScheduleResponse discoverableScheduleResponse) {
    if (Objects.nonNull(buyableScheduleResponse)) {
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.BUYABLE_SCHEDULE_UPDATE);
    }
    if (Objects.nonNull(discoverableScheduleResponse)) {
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.DISCOVERABLE_SCHEDULE_UPDATE);
    }
  }

  private static boolean removeScheduleForStatusChange(ItemPickupPointRequest request,
      DiscoverableScheduleResponse discoverableSchedule, BuyableScheduleResponse buyableSchedule,
      boolean cncForWarehouseFeatureSwitch) {
    boolean isEligibleForRemoval =
        request.isBuyable() || request.isDisplay() || (cncForWarehouseFeatureSwitch && request.isCncDisplay() != request.isCncBuyable());
    boolean hasScheduleResponse = Objects.nonNull(buyableSchedule) || Objects.nonNull(discoverableSchedule);
    //updates status from offline to Not Offline (buyable link, discoverable , online)
    if (isEligibleForRemoval && hasScheduleResponse) {
      request.setScheduleRemoval(true);
      request.setDiscoverableSchedule(null);
      request.setBuyableSchedule(null);
      return true;
    }
    return false;
  }


  private static void populateL5XScheduleChangeTypeMap(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap, String l5Code,
      String scheduleChangeType) {
    l5XScheduleUpdateChangeTypeMap.computeIfAbsent(l5Code, k -> new HashSet<>()).add(scheduleChangeType);
  }

  private static Optional<BuyableScheduleResponse> extractFirstBuyableScheduleResponse(
      ItemPickupPointListingResponse response) {
    return response.getViewConfigs().stream().map(ViewConfigResponse::getBuyableScheduleResponse).filter(Objects::nonNull).findFirst();
  }

  private static Optional<DiscoverableScheduleResponse> extractFirstDiscoverableScheduleResponse(
      ItemPickupPointListingResponse response) {
    return response.getViewConfigs().stream().map(ViewConfigResponse::getDiscoverableScheduleResponse).filter(Objects::nonNull)
        .findFirst();
  }

  private static void processBuyableSchedule(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap, String l5Code,
      BuyableScheduleResponse buyableScheduleResponse, com.gda.mta.product.dto.BuyableScheduleRequest buyableScheduleRequest,
      boolean isScheduleRemoval) {
    if (Objects.isNull(buyableScheduleRequest) && Objects.isNull(buyableScheduleResponse)) {
      // If both request and response are null, no further processing is needed
      return;
    }
    if (isScheduleRemoval && Objects.isNull(buyableScheduleRequest)) {
      // If request is null, but response is not null, process the response with null request
      processChangeInBuyableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code, buyableScheduleResponse, null);
    } else if (Objects.isNull(buyableScheduleResponse)) {
      // If response is null, new schedules are added and nothing is updated
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.BUYABLE_SCHEDULE_UPDATE);
    } else {
      // Process the change with both request and response
      processChangeInBuyableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code, buyableScheduleResponse, buyableScheduleRequest);
    }
  }


  private static void processChangeInBuyableSchedule(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap, String l5Code,
      BuyableScheduleResponse buyableScheduleResponse, com.gda.mta.product.dto.BuyableScheduleRequest buyableScheduleRequest) {
    Optional<Date> requestEndDateTime = Optional.ofNullable(buyableScheduleRequest).map(com.gda.mta.product.dto.BuyableScheduleRequest::getEndDateTime);

    Optional<Date> requestStartDateTime = Optional.ofNullable(buyableScheduleRequest).map(com.gda.mta.product.dto.BuyableScheduleRequest::getStartDateTime);
    boolean endDateChanged =
        Optional.ofNullable(buyableScheduleResponse).map(BuyableScheduleResponse::getEndDateTime).filter(Predicate.not(endDate -> endDate.equals(requestEndDateTime.orElse(null))))
            .isPresent();

    boolean startDateChanged =
        Optional.ofNullable(buyableScheduleResponse).map(BuyableScheduleResponse::getStartDateTime).filter(Predicate.not(startDate -> startDate.equals(requestStartDateTime.orElse(null))))
            .isPresent();

    if (startDateChanged || endDateChanged) {
      // if start or end date is change existing buyable schedules are modified
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.BUYABLE_SCHEDULE_UPDATE);
    }
  }

  private static void processDiscoverableSchedule(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap, String l5Code,
      DiscoverableScheduleResponse discoverableScheduleResponse, com.gda.mta.product.dto.DiscoverableScheduleRequest discoverableScheduleRequest,
      boolean isScheduleRemoval) {

    // If both request and response are null, further processing not needed
    if (Objects.isNull(discoverableScheduleRequest) && Objects.isNull(discoverableScheduleResponse)) {
      return;
    }

    if (isScheduleRemoval && Objects.isNull(discoverableScheduleRequest)) {
      // If request is null, but response is not null, process the response with null request
      processChangeInDiscoverableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code,
          discoverableScheduleResponse, null);
    } else if (Objects.isNull(discoverableScheduleResponse)) {
      // If response is null, new schedules are added and nothing is updated
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.DISCOVERABLE_SCHEDULE_UPDATE);
    } else {
      // Process the change with both request and response
      processChangeInDiscoverableSchedule(l5XScheduleUpdateChangeTypeMap, l5Code,
          discoverableScheduleResponse, discoverableScheduleRequest);
    }
  }


  private static void processChangeInDiscoverableSchedule(Map<String, Set<String>> l5XScheduleUpdateChangeTypeMap,
      String l5Code, DiscoverableScheduleResponse discoverableScheduleResponse, com.gda.mta.product.dto.DiscoverableScheduleRequest discoverableScheduleRequest) {
    Optional<Date> requestEndDateTime = Optional.ofNullable(discoverableScheduleRequest).map(com.gda.mta.product.dto.DiscoverableScheduleRequest::getEndDateTime);

    Optional<Date> requestStartDateTime = Optional.ofNullable(discoverableScheduleRequest).map(com.gda.mta.product.dto.DiscoverableScheduleRequest::getStartDateTime);

    boolean endDateChanged = Optional.ofNullable(discoverableScheduleResponse).map(DiscoverableScheduleResponse::getEndDateTime)
        .filter(Predicate.not(endDate -> endDate.equals(requestEndDateTime.orElse(null)))).isPresent();

    boolean startDateChanged = Optional.ofNullable(discoverableScheduleResponse).map(DiscoverableScheduleResponse::getStartDateTime)
        .filter(Predicate.not(startDate -> startDate.equals(requestStartDateTime.orElse(null)))).isPresent();
    if (endDateChanged || startDateChanged) {
      // if start or end date is change existing discoverable schedules are modified
      populateL5XScheduleChangeTypeMap(l5XScheduleUpdateChangeTypeMap, l5Code, Constants.DISCOVERABLE_SCHEDULE_UPDATE);
    }
  }

  public static String getBuyableSchedule(List<ViewConfigResponse> viewConfigResponses) {
    return viewConfigResponses.stream().filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannelId())).findFirst()
        .map(ViewConfigResponse::getBuyableScheduleResponse).map(
            itemBuyableScheduleDTO -> itemBuyableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN
                + Constants.SPACE + itemBuyableScheduleDTO.getEndDateTime()).orElse(Constants.HYPHEN);
  }

  public static String getDiscoverableSchedule(List<ViewConfigResponse> viewConfigResponses) {
    return viewConfigResponses.stream().filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannelId())).findFirst()
        .map(ViewConfigResponse::getDiscoverableScheduleResponse).map(
            itemDiscoverableScheduleDTO -> itemDiscoverableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN + Constants.SPACE
                + itemDiscoverableScheduleDTO.getEndDateTime()).orElse(Constants.HYPHEN);
  }

  public static String getDiscoverableSchedule(Set<ItemViewConfig> itemViewConfig) {
    return itemViewConfig.stream().filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
        .map(ItemViewConfig::getItemDiscoverableSchedules).map(
            itemDiscoverableScheduleDTO -> itemDiscoverableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN + Constants.SPACE
                + itemDiscoverableScheduleDTO.getEndDateTime()).orElse(Constants.HYPHEN);
  }

  public static String getBuyableSchedule(Set<ItemViewConfig> itemViewConfig) {
    return itemViewConfig.stream().filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
        .map(ItemViewConfig::getItemBuyableSchedules).map(
            itemBuyableScheduleDTO -> itemBuyableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN + Constants.SPACE + itemBuyableScheduleDTO.getEndDateTime())
        .orElse(Constants.HYPHEN);
  }

  public static boolean isBuyableScheduleChanged(ItemBuyableScheduleDTO oldItemBuyableScheduleDTO,
      ItemBuyableScheduleDTO newItemBuyableScheduleDTO) {
    return Objects.nonNull(oldItemBuyableScheduleDTO) && Objects.nonNull(oldItemBuyableScheduleDTO.getStartDateTime())
        && Objects.nonNull(oldItemBuyableScheduleDTO.getEndDateTime()) && Objects.isNull(newItemBuyableScheduleDTO);
  }

  public static boolean isDiscoverableScheduleChanged(ItemDiscoverableScheduleDTO oldItemDiscoverableScheduleDTO,
      ItemDiscoverableScheduleDTO newItemDiscoverableScheduleDTO) {
    return Objects.nonNull(oldItemDiscoverableScheduleDTO) && Objects.nonNull(
        oldItemDiscoverableScheduleDTO.getStartDateTime()) && Objects.nonNull(
        oldItemDiscoverableScheduleDTO.getEndDateTime()) && Objects.isNull(
        newItemDiscoverableScheduleDTO);
  }

  public static void setWaitingDeletionForDeletePickupPoint(boolean setWaitingDeletionForDeletePickupPoint,
      PickupPointFilterRequest pickupPointFilterRequest) {
    if (setWaitingDeletionForDeletePickupPoint) {
      pickupPointFilterRequest.setWaitingDeletion(false);
    }
  }

  public static List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> getExistingSchedulesFromRequest(
      ProductL3UpdateRequest productL3UpdateRequest) {
    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> itemPickupPointRequests =
        new ArrayList<>();
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productL3UpdateRequest.getProductItems()) {
      for (com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        if (Objects.nonNull(itemPickupPointRequest.getDiscoverableSchedule()) || Objects.nonNull(
            itemPickupPointRequest.getBuyableSchedule())) {
          itemPickupPointRequests.add(
              com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest.builder().itemSku(productVariantPriceStockAndImagesRequest.getItemSku())
                  .pickupPointCode(itemPickupPointRequest.getPickupPointId()).build());
        }
      }
    }
    return itemPickupPointRequests;
  }


  public static Map<String, Set<String>> fetchL5WithActiveAndUnModifiedSchedules(List<ItemPickupPointBasicResponse> itemPickupPointBasicResponses,
      ProductL3UpdateRequest requestsForSchedulesValidations) {
    Map<String, Set<String>> offlineItemIdsWithActiveSchedules = new HashMap<>();
    Set<String> offlineItemIdsWithActiveBuyableSchedules = new HashSet<>();
    Set<String> offlineItemIdsWithActiveDiscoverableSchedules = new HashSet<>();
    offlineItemIdsWithActiveSchedules.put(Constants.BUYABLE_SCHEDULE_UPDATE,
        offlineItemIdsWithActiveBuyableSchedules);
    offlineItemIdsWithActiveSchedules.put(Constants.DISCOVERABLE_SCHEDULE_UPDATE,
        offlineItemIdsWithActiveDiscoverableSchedules);
    Map<String, com.gda.mta.product.dto.BuyableScheduleRequest> l5XBuyableRequest = new HashMap<>();
    Map<String, com.gda.mta.product.dto.DiscoverableScheduleRequest> l5XDiscoverableRequest = new HashMap<>();
    populateBuyableAndDiscoverableRequestMap(requestsForSchedulesValidations, l5XBuyableRequest,
        l5XDiscoverableRequest);

    Date currentDate = new Date();
    for (ItemPickupPointBasicResponse itemPickupPointBasicResponse : itemPickupPointBasicResponses) {
      ViewConfigResponse viewConfigResponse = itemPickupPointBasicResponse.getViewConfigResponse();
      String offlineItemId = itemPickupPointBasicResponse.getItemSku() + Constants.HYPHEN + itemPickupPointBasicResponse.getPickupPointCode();

      boolean hasActiveAndNonModifiedBuyableSchedule =
          validateActiveAndNonModifiedBuyableSchedule(viewConfigResponse, currentDate,
              l5XBuyableRequest, offlineItemId);

      boolean hasActiveAndNonModifiedDiscoverableSchedule =
          validateActiveAndNonModifiedDiscoverableSchedule(viewConfigResponse, currentDate,
              l5XDiscoverableRequest, offlineItemId);

      if (hasActiveAndNonModifiedBuyableSchedule) {
        offlineItemIdsWithActiveSchedules.get(Constants.BUYABLE_SCHEDULE_UPDATE).add(offlineItemId);
      }
      if (hasActiveAndNonModifiedDiscoverableSchedule) {
        offlineItemIdsWithActiveSchedules.get(Constants.DISCOVERABLE_SCHEDULE_UPDATE).add(offlineItemId);
      }
    }
    return offlineItemIdsWithActiveSchedules;
  }

  private static boolean validateActiveAndNonModifiedDiscoverableSchedule(ViewConfigResponse viewConfigResponse, Date currentDate,
      Map<String, com.gda.mta.product.dto.DiscoverableScheduleRequest> l5XDiscoverableRequest,
      String offlineItemId) {
    DiscoverableScheduleResponse discoverableScheduleResponse =
        Optional.ofNullable(viewConfigResponse).map(ViewConfigResponse::getDiscoverableScheduleResponse).orElse(null);

    boolean hasActiveAndNonModifiedDiscoverableSchedule =
        Optional.ofNullable(discoverableScheduleResponse).filter(response -> response.getEndDateTime().after(currentDate)).isPresent();

    if (l5XDiscoverableRequest.containsKey(offlineItemId) && Objects.nonNull(
        discoverableScheduleResponse)) {
      com.gda.mta.product.dto.DiscoverableScheduleRequest request = l5XDiscoverableRequest.get(offlineItemId);
      // check if schedules are edit, if yes iys valid scenario for date checks
      hasActiveAndNonModifiedDiscoverableSchedule = Optional.ofNullable(request.getStartDateTime())
          .filter(startTime -> startTime.equals(discoverableScheduleResponse.getStartDateTime()))
          .isPresent() && Optional.ofNullable(request.getEndDateTime())
          .filter(endTime -> endTime.equals(discoverableScheduleResponse.getEndDateTime()))
          .isPresent();
    }
    return hasActiveAndNonModifiedDiscoverableSchedule;
  }

  private static boolean validateActiveAndNonModifiedBuyableSchedule(ViewConfigResponse viewConfigResponse, Date currentDate,
      Map<String, com.gda.mta.product.dto.BuyableScheduleRequest> l5XBuyableRequest, String offlineItemId) {
    BuyableScheduleResponse buyableScheduleResponse =
        Optional.ofNullable(viewConfigResponse).map(ViewConfigResponse::getBuyableScheduleResponse).orElse(null);

    boolean hasActiveAndNonModifiedBuyableSchedule = Optional.ofNullable(buyableScheduleResponse).filter(response -> response.getEndDateTime().after(currentDate)).isPresent();

    if (l5XBuyableRequest.containsKey(offlineItemId) && Objects.nonNull(buyableScheduleResponse)) {
      com.gda.mta.product.dto.BuyableScheduleRequest request = l5XBuyableRequest.get(offlineItemId);
      // check if schedules are edit, if yes iys valid scenario for date checks
      hasActiveAndNonModifiedBuyableSchedule = Optional.ofNullable(request.getStartDateTime()).filter(startTime -> startTime.equals(buyableScheduleResponse.getStartDateTime()))
          .isPresent() && Optional.ofNullable(request.getEndDateTime()).filter(endTime -> endTime.equals(buyableScheduleResponse.getEndDateTime())).isPresent();
    }
    return hasActiveAndNonModifiedBuyableSchedule;
  }

  private static void populateBuyableAndDiscoverableRequestMap(ProductL3UpdateRequest requestsForSchedulesValidations,
      Map<String, com.gda.mta.product.dto.BuyableScheduleRequest> l5XBuyableRequest, Map<String, com.gda.mta.product.dto.DiscoverableScheduleRequest> l5XDiscoverableRequest) {
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : requestsForSchedulesValidations.getProductItems()) {
      String itemSku = productVariantPriceStockAndImagesRequest.getItemSku();
      if (StringUtils.isNotBlank(itemSku)) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          String l5Id = itemSku.concat(Constants.HYPHEN).concat(itemPickupPointRequest.getPickupPointId());
          if (Objects.nonNull(itemPickupPointRequest.getBuyableSchedule())) {
            l5XBuyableRequest.put(l5Id, itemPickupPointRequest.getBuyableSchedule());
          }
          if (Objects.nonNull(itemPickupPointRequest.getDiscoverableSchedule())) {
            l5XDiscoverableRequest.put(l5Id, itemPickupPointRequest.getDiscoverableSchedule());
          }
        }
      }
    }
  }

  public static void setSchedulesForQuickEditRequest(ItemSummaryListResponse itemSummaryListResponse,
      ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest, boolean cncForWarehouseFeatureSwitch) {
    Optional<ItemViewConfigDTO> viewConfigDTO =
        Optional.ofNullable(itemSummaryListResponse.getItemViewConfigs()).orElse(new HashSet<>()).stream()
            .filter(itemViewConfigDTO -> Constants.DEFAULT.equals(itemViewConfigDTO.getChannel())).findFirst();
    ItemViewConfigDTO cncItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
        .filter(itemViewConfigDTO -> Constants.CNC_CHANNEL.equals(itemViewConfigDTO.getChannel())).findFirst().orElse(null);
    String currentCncStatus;
    if (Objects.isNull(cncItemViewConfigDTO)) {
      currentCncStatus = ProductLevel3Status.OFFLINE.name();
    } else {
      currentCncStatus =
          getProductStatus(cncItemViewConfigDTO.isBuyable(), cncItemViewConfigDTO.isDiscoverable());
    }
    if (removeScheduleForStatusChange(itemPickupPointQuickEditRequest, viewConfigDTO,
        cncForWarehouseFeatureSwitch,
        !currentCncStatus.equalsIgnoreCase(itemPickupPointQuickEditRequest.getCncStatus()))) {
      log.info("Processing removal of schedule for status change for : {} ",
          itemPickupPointQuickEditRequest.getItemSku().concat(Constants.HYPHEN).concat(itemPickupPointQuickEditRequest.getPickupPointCode()));
      return;
    }

    viewConfigDTO.map(ItemViewConfigDTO::getItemBuyableSchedules).ifPresent(itemBuyableScheduleDTO -> {
      BuyableScheduleRequest buyableScheduleRequest = new BuyableScheduleRequest();
      buyableScheduleRequest.setBuyable(itemBuyableScheduleDTO.isBuyable());
      buyableScheduleRequest.setEndDateTime(itemBuyableScheduleDTO.getEndDateTime());
      buyableScheduleRequest.setStartDateTime(itemBuyableScheduleDTO.getStartDateTime());
      itemPickupPointQuickEditRequest.setBuyableSchedule(buyableScheduleRequest);
    });

    viewConfigDTO.map(ItemViewConfigDTO::getItemDiscoverableSchedules).ifPresent(itemDiscoverableScheduleDTO -> {
      DiscoverableScheduleRequest discoverableScheduleRequest = new DiscoverableScheduleRequest();
      discoverableScheduleRequest.setDiscoverable(itemDiscoverableScheduleDTO.isDiscoverable());
      discoverableScheduleRequest.setStartDateTime(itemDiscoverableScheduleDTO.getStartDateTime());
      discoverableScheduleRequest.setEndDateTime(itemDiscoverableScheduleDTO.getEndDateTime());
      itemPickupPointQuickEditRequest.setDiscoverableSchedule(discoverableScheduleRequest);
    });

  }

  private static boolean removeScheduleForStatusChange(ItemPickupPointQuickEditRequest quickEditRequest,
      Optional<ItemViewConfigDTO> viewConfigDTO, boolean cncForWarehouseFeatureSwitch, boolean cncStatusChanged) {
    // schedule is eligible for removal if status is online, buyable link or upcoming
    boolean isEligibleForRemoval =
        Objects.nonNull(quickEditRequest.getStatus()) && !ProductLevel3Status.OFFLINE.name().equals(quickEditRequest.getStatus()) || (
            // If switch is ON, cnc status has changed, and has changed to any status except OFFLINE
            cncForWarehouseFeatureSwitch && cncStatusChanged && Objects.nonNull(quickEditRequest.getCncStatus()) && (!ProductLevel3Status.OFFLINE.name()
                .equals(quickEditRequest.getCncStatus())));
    // Check if buyable or discoverable schedules exist
    boolean schedulesExist = viewConfigDTO.map(
        config -> Objects.nonNull(config.getItemBuyableSchedules()) || Objects.nonNull(config.getItemDiscoverableSchedules())).orElse(false);
    // Update the status if eligible for removal and schedules exist
    if (isEligibleForRemoval && schedulesExist) {
      quickEditRequest.setScheduleUpdate(true);
      quickEditRequest.setDiscoverableSchedule(null);
      quickEditRequest.setBuyableSchedule(null);
      return true;
    }

    return false;
  }

  public static boolean setForceReviewFlag(boolean existingForceReview, boolean updatedForceReview) {
    return existingForceReview || updatedForceReview;
  }

  public static String setImageQcViolation(String existingViolation, String updatedViolation) {
    updatedViolation =
        StringUtils.isNotEmpty(updatedViolation) ? updatedViolation : StringUtils.EMPTY;
    existingViolation = StringUtils.isNotEmpty(existingViolation) ?
        existingViolation.concat(Constants.COMMA) :
        StringUtils.EMPTY;
    String violation = existingViolation.concat(updatedViolation);
    Set<String> uniqueTags = Arrays.stream(violation.split(Constants.COMMA)).map(String::trim).collect(Collectors.toSet());
    return String.join(Constants.COMMA, uniqueTags);
  }

  public static void populateL5WithActiveSchedules(Set<String> l5sWithActiveSchedules, Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) {
    Date currentDate = new Date();

    for (Map.Entry<String, ItemPickupPointListingResponse> entry : savedItemPickupPointDataMap.entrySet()) {
      String offlineItemId = entry.getKey();
      ItemPickupPointListingResponse itemPickupPointListingResponse = entry.getValue();

      if (Objects.nonNull(itemPickupPointListingResponse) && CollectionUtils.isNotEmpty(
          itemPickupPointListingResponse.getViewConfigs())) {
        List<ViewConfigResponse> defaultChannelConfigs =
            itemPickupPointListingResponse.getViewConfigs().stream().filter(viewConfigResponse -> Constants.DEFAULT.equals(viewConfigResponse.getChannelId()))
                .collect(Collectors.toList());
        // Fetch buyable schedules that are active
        List<BuyableScheduleResponse> activeBuyableSchedules =
            defaultChannelConfigs.stream().map(ViewConfigResponse::getBuyableScheduleResponse).filter(Objects::nonNull).filter(
                buyableScheduleResponse -> buyableScheduleResponse.getEndDateTime().after(currentDate)).collect(Collectors.toList());
        // Fetch Discoverable schedules that are active
        List<DiscoverableScheduleResponse> activeDiscoverableSchedules =
            defaultChannelConfigs.stream().map(ViewConfigResponse::getDiscoverableScheduleResponse).filter(Objects::nonNull).filter(
                discoverableScheduleResponse -> discoverableScheduleResponse.getEndDateTime().after(currentDate)).collect(Collectors.toList());

        if (CollectionUtils.isNotEmpty(activeDiscoverableSchedules) || CollectionUtils.isNotEmpty(
            activeBuyableSchedules)) {
          l5sWithActiveSchedules.add(offlineItemId);
        }
      }
    }
  }

  public static String getMerchantTypeFromProfileResponse(ProfileResponse profileResponse) {
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && StringUtils.isNotBlank(profileResponse.getCompany().getMerchantType())) {
      return profileResponse.getCompany().getMerchantType();
    }
    return StringUtils.EMPTY;
  }

  public static boolean validateCncUpdateForBopis(QuickEditV2Request quickEditV2Request,
      ItemSummaryListResponse itemSummaryListResponse, boolean bopisCNCRestrictionEnabled,
      boolean cncForWarehouseFeatureSwitch) {
    // Product cannot be Bopis and CNC active at the same time
    if (cncForWarehouseFeatureSwitch) {
      return !ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getCncStatus()) && bopisCNCRestrictionEnabled
          && ProductType.BOPIS.equals(itemSummaryListResponse.getProductType());
    } else {
      return Boolean.TRUE.equals(quickEditV2Request.getCncActive()) && bopisCNCRestrictionEnabled
          && ProductType.BOPIS.equals(itemSummaryListResponse.getProductType());
    }
  }


  public static boolean validateStatusUpdateForBopis(QuickEditV2Request quickEditV2Request, String merchantType,
      ItemSummaryListResponse itemSummaryListResponse, boolean bopisCategoryRestrictionEnabled, List<String> bopisUnsupportedMerchantTypes) {
    return ProductLevel3Status.ONLINE.equals(quickEditV2Request.getStatus()) && bopisCategoryRestrictionEnabled
        && bopisUnsupportedMerchantTypes.contains(merchantType) && Boolean.TRUE.equals(itemSummaryListResponse.getDimensionsMissing());
  }

  public static boolean validateShippingTypeAndDimensionMissingForBopis(boolean bopisCategoryRestrictionEnabled,
      ItemSummaryListResponse itemSummaryListResponse) {
    return bopisCategoryRestrictionEnabled && ProductType.BOPIS.equals(itemSummaryListResponse.getProductType())
        && Boolean.TRUE.equals(itemSummaryListResponse.getDimensionsMissing());
  }

  public static void populateHistoryAuditMapForSchedulesUpdate(Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap,
      boolean schedulesAddEditEnabled, List<Map<String, String>> historyAuditMap,
      ItemPickupPointRequest modifiedItemPickupPoint, FbbAndCncDataChangeDto fbbAndCncDataChangeDto,
      boolean mppEnabled, boolean cncForWarehouseFeatureSwitch) {

    ItemPickupPointListingResponse itemPickupPointListingResponse = l5CodeAndResponseMap.get(
        mppEnabled ?
            modifiedItemPickupPoint.getItemSku() + Constants.HYPHEN + modifiedItemPickupPoint.getPickupPointId() :
            modifiedItemPickupPoint.getItemSku());

    if (Objects.isNull(itemPickupPointListingResponse)) {
      return;
    }

    boolean schedulesRemovalForNeedRevision =
        shouldRemoveSchedulesForRevision(schedulesAddEditEnabled, modifiedItemPickupPoint,
            cncForWarehouseFeatureSwitch);
    if (!schedulesRemovalForNeedRevision) {
      return;
    }

    List<ViewConfigResponse> defaultViewConfigResponses = getDefaultViewConfigResponses(itemPickupPointListingResponse);
    Date now = new Date();

    // Check active schedules and update history audit map accordingly
    processScheduleResponse(defaultViewConfigResponses, now, historyAuditMap, fbbAndCncDataChangeDto, l5CodeAndResponseMap, modifiedItemPickupPoint);
  }

  private static boolean shouldRemoveSchedulesForRevision(boolean schedulesAddEditEnabled,
      ItemPickupPointRequest modifiedItemPickupPoint, boolean cncForWarehouseFeatureSwitch) {
    return schedulesAddEditEnabled && (modifiedItemPickupPoint.isBuyable() || modifiedItemPickupPoint.isDisplay() || (cncForWarehouseFeatureSwitch && (
        modifiedItemPickupPoint.isCncBuyable() != modifiedItemPickupPoint.isCncDisplay())));
  }

  private static List<ViewConfigResponse> getDefaultViewConfigResponses(
      ItemPickupPointListingResponse itemPickupPointListingResponse) {
    return itemPickupPointListingResponse.getViewConfigs().stream().filter(viewConfigResponse -> DEFAULT.equals(viewConfigResponse.getChannelId()))
        .collect(Collectors.toList());
  }

  private static void processScheduleResponse(List<ViewConfigResponse> viewConfigResponses, Date now, List<Map<String, String>> historyAuditMap,
      FbbAndCncDataChangeDto fbbAndCncDataChangeDto, Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap,
      ItemPickupPointRequest modifiedItemPickupPoint) {

    boolean isBuyableScheduleActive =
        viewConfigResponses.stream().map(ViewConfigResponse::getBuyableScheduleResponse).filter(Objects::nonNull).anyMatch(
            buyableScheduleResponse -> buyableScheduleResponse.getEndDateTime().after(now));

    boolean isDiscoverableScheduleActive =
        viewConfigResponses.stream().map(ViewConfigResponse::getDiscoverableScheduleResponse).filter(Objects::nonNull).anyMatch(
            discoverableScheduleResponse -> discoverableScheduleResponse.getEndDateTime().after(now));

    if (isBuyableScheduleActive) {
      updateHistoryMap(historyAuditMap, fbbAndCncDataChangeDto, l5CodeAndResponseMap,
          modifiedItemPickupPoint, viewConfigResponses, true);
    }

    if (isDiscoverableScheduleActive) {
      updateHistoryMap(historyAuditMap, fbbAndCncDataChangeDto, l5CodeAndResponseMap,
          modifiedItemPickupPoint, viewConfigResponses, false);
    }
  }

  private static void updateHistoryMap(List<Map<String, String>> historyAuditMap,
      FbbAndCncDataChangeDto fbbAndCncDataChangeDto, Map<String, ItemPickupPointListingResponse> l5CodeAndResponseMap,
      ItemPickupPointRequest modifiedItemPickupPoint, List<ViewConfigResponse> viewConfigResponses,
      boolean isBuyable) {

    String previousValue = viewConfigResponses.stream().map(isBuyable ?
        ViewConfigResponse::getBuyableScheduleResponse :
        ViewConfigResponse::getDiscoverableScheduleResponse).filter(Objects::nonNull).findFirst().map(Object::toString).orElse(StringUtils.EMPTY);
    String activity = isBuyable ?
        UpdateProductActivity.BUYABLE_SCHEDULE.getDesc() :
        UpdateProductActivity.DISCOVERABLE_SCHEDULE.getDesc();
    Map<String, String> historyMap = new HashMap<>();
    historyMap.put(Constants.PREVIOUS_VALUE, previousValue);
    historyMap.put(Constants.CURRENT_VALUE, Constants.HYPHEN);
    historyMap.put(Constants.HISTORY_ACTIVITY, activity.concat(Constants.NEED_REVISION));
    CommonUtils.setItemNameByItemPickupPoint(l5CodeAndResponseMap, historyMap,
        modifiedItemPickupPoint, Constants.ITEM_NAME);
    historyAuditMap.add(historyMap);
    fbbAndCncDataChangeDto.setFieldChanged(Boolean.TRUE);
  }


  public static boolean merchantTypesForBopisCategoryValidation(ProfileResponse profileResponse,
      String merchantTypesForBopisCategoryValidation) {
    List<String> unsupportedMerchantTypesForBopis = Arrays.asList(merchantTypesForBopisCategoryValidation.split(","));
    String merchantType = Optional.ofNullable(profileResponse.getCompany()).orElse(new CompanyDTO())
        .getMerchantType();
    return unsupportedMerchantTypesForBopis.contains(merchantType);
  }

  public static void migrateProductAndL5DetailsByProductSku(List<ProductItemBusinessPartner> productItemBusinessPartners,
      ProductAndL5MigrationRequest productAndL5MigrationRequest, boolean b2cActivated, boolean b2bActivated, String username) {

    productItemBusinessPartners.forEach(productItemBusinessPartner -> {
      productItemBusinessPartner.setProductType(Optional.ofNullable(productAndL5MigrationRequest).map(ProductAndL5MigrationRequest::getProductType)
          .map(com.gdn.mta.product.enums.ProductType::getProductType).orElse(productItemBusinessPartner.getProductType()));

      productItemBusinessPartner.setCncActive(Optional.ofNullable(productAndL5MigrationRequest).map(ProductAndL5MigrationRequest::getCncActiveAtL3)
          .orElse(productItemBusinessPartner.isCncActive()));

      productItemBusinessPartner.setCncActivated(Optional.ofNullable(productAndL5MigrationRequest).map(ProductAndL5MigrationRequest::getCncActiveAtL5)
          .orElse(productItemBusinessPartner.isCncActivated()));

      boolean isBuyable = Optional.ofNullable(productAndL5MigrationRequest).map(ProductAndL5MigrationRequest::isBuyable)
          .orElse(productItemBusinessPartner.isBuyable());
      boolean isDiscoverable = Optional.ofNullable(productAndL5MigrationRequest).map(ProductAndL5MigrationRequest::isDiscoverable)
          .orElse(productItemBusinessPartner.isDisplay());
      if (b2cActivated) {
        productItemBusinessPartner.setBuyable(isBuyable);
        productItemBusinessPartner.setDisplay(isDiscoverable);
      }
      if (b2bActivated) {
        productItemBusinessPartner.setB2bBuyable(isBuyable);
        productItemBusinessPartner.setB2bDiscoverable(isDiscoverable);
      }
      productItemBusinessPartner.setUpdatedBy(username);
    });
  }

  public static boolean isDimensionLess(Double height, Double weight, Double length, Double shippingWeight) {
    return Stream.of(height, weight, length, shippingWeight).anyMatch(
        dimension -> Objects.isNull(dimension) || Double.compare(dimension, ZERO_DIMENSION) == ZERO);
  }

  public static void updateProductItemBusinessPartnerForAutoCategoryChange(List<ProductItemBusinessPartner> productItemBusinessPartners, boolean autoCategoryChange,
      boolean bopisEligible, boolean bopisCategoryActionOnCategoryChangeSwitch, Integer productType,
      EditFlagChangesDTO editFlagChangesDTO) {
    if (bopisCategoryActionOnCategoryChangeSwitch && autoCategoryChange && Boolean.FALSE.equals(
        bopisEligible) && Integer.valueOf(ProductType.BOPIS.getCode()).equals(productType)) {
      setProductItemBusinessPartnerData(productItemBusinessPartners);
      editFlagChangesDTO.setTakeActionOnShippingForAutoCategoryChange(true);
    }
  }

  public static void setProductItemBusinessPartnerData(List<ProductItemBusinessPartner> productItemBusinessPartners) {
    productItemBusinessPartners.forEach(CommonUtils::setProductItemBusinessPartnerData);
  }

  private static void setProductItemBusinessPartnerData(ProductItemBusinessPartner productItemBusinessPartner) {
    productItemBusinessPartner.setDisplay(false);
    productItemBusinessPartner.setBuyable(false);
    productItemBusinessPartner.setProductType(ProductType.REGULAR.getCode());
  }

  public static void setCncViewConfigFromProductVariantUpdateRequest(
      ProductVariantUpdateRequest productVariantUpdateRequest, boolean cncForWarehouseFeatureSwitch) {
    productVariantUpdateRequest.getProductItems().stream().flatMap(
        productVariantPriceStockAndImagesRequest -> productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()
            .stream()).forEach(modifiedItemPickupPoint -> setCncViewConfigs(cncForWarehouseFeatureSwitch,
        modifiedItemPickupPoint));
    productVariantUpdateRequest.getAddPickupPoints().stream().forEach(itemPickupPointRequest -> setCncViewConfigs(cncForWarehouseFeatureSwitch,
        itemPickupPointRequest));
  }

  private static void setCncViewConfigs(boolean cncForWarehouseFeatureSwitch, ItemPickupPointRequest itemPickupPointRequest) {
    if (!cncForWarehouseFeatureSwitch) {
      itemPickupPointRequest.setCncBuyable(itemPickupPointRequest.isCncActive());
      itemPickupPointRequest.setCncDisplay(itemPickupPointRequest.isCncActive());
    } else {
      if (!itemPickupPointRequest.isCncBuyable() && !itemPickupPointRequest.isCncDisplay()) {
        itemPickupPointRequest.setCncBuyable(itemPickupPointRequest.isCncActive());
        itemPickupPointRequest.setCncDisplay(itemPickupPointRequest.isCncActive());
      }
    }
  }

  public static com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest fetchRequestForActiveProductMigration(
      ProductAndL5MigrationRequest productAndL5MigrationRequest) {
    com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest request = new com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest();
    BeanUtils.copyProperties(productAndL5MigrationRequest, request);
    ProductType productType = Enum.valueOf(ProductType.class, productAndL5MigrationRequest.getProductType().name());
    request.setProductType(productType);
    return request;
  }

  public static void setSizeChartCodeAndSizeChartBusinessPartnerCode(String businessPartnerCode,
      ProductDetailCompleteResponse productDetailCompleteResponse,
      BasicSizeChartDetailMapResponse basicSizeChartDetailMapResponse) {
    Map<String, BasicSizeChartDetailResponse> basicSizeChartDetailResponseMap =
        basicSizeChartDetailMapResponse.getBasicSizeChartDetailResponseMap();
    if (StringUtils.isBlank(businessPartnerCode)) {
      businessPartnerCode = Constants.INTERNAL;
    }
    String sizechartCode = null;
    String internalSizeChatCode = null;
    // For a shared product where all the products may have different size chart code
    // Precedence will be given to size chart belonging to that seller , then internal
    for (Map.Entry<String, BasicSizeChartDetailResponse> entry : basicSizeChartDetailResponseMap.entrySet()) {
      BasicSizeChartDetailResponse detailResponse = entry.getValue();
      if (businessPartnerCode.equals(detailResponse.getBusinessPartnerCode())) {
        sizechartCode = entry.getKey();
        break;
      }
      if (Constants.INTERNAL.equals(detailResponse.getBusinessPartnerCode())) {
        internalSizeChatCode = entry.getKey();
      }
    }
    if (StringUtils.isBlank(sizechartCode)) {
      sizechartCode = internalSizeChatCode;
    }
    if (StringUtils.isNotBlank(sizechartCode)) {
      BasicSizeChartDetailResponse selectedSizeChartDetail = basicSizeChartDetailResponseMap.get(sizechartCode);
      productDetailCompleteResponse.setSizeChartCode(sizechartCode);
      productDetailCompleteResponse.setSizeChartBusinessPartnerCode(selectedSizeChartDetail.getBusinessPartnerCode());
      productDetailCompleteResponse.setSizeChartName(selectedSizeChartDetail.getSizeChartName());
    } else {
      productDetailCompleteResponse.setSizeChartCode(null);
      productDetailCompleteResponse.setSizeChartBusinessPartnerCode(null);
      productDetailCompleteResponse.setSizeChartName(null);
    }
  }

  public static void validateSyncStockForFaasSeller(QuickEditV2Request quickEditV2Request,
      ProfileResponse businessPartner, ItemSummaryListResponse itemSummaryListResponse, boolean faasFeatureSwitch) {
    if (faasFeatureSwitch && getBusinessPartnerFlagValue(businessPartner, Constants.FAAS_ACTIVATED)
        && Boolean.TRUE.equals(quickEditV2Request.getUseWarehouseStock())) {
      log.error("Error updating for product : {} ", itemSummaryListResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getDesc());
    }
  }

  public static ApiErrorCode validateSyncStockForFAASMerchantsInEditRequest(
      ProductL3UpdateRequest productL3UpdateRequest, ProfileResponse profileResponse, ApiErrorCode apiErrorCode) {
    //validate modified L5 request
    boolean modifiedL5requestHasSyncStockTrueL5 = productL3UpdateRequest.getProductItems().stream()
        .map(ProductVariantPriceStockAndImagesRequest::getModifiedItemPickupPoints).flatMap(List::stream).anyMatch(itemPickupPointRequest -> Boolean.TRUE.equals(
            itemPickupPointRequest.getSynchronizeStock()));
    boolean merchantFAASFlag = getBusinessPartnerFlagValue(profileResponse, Constants.FAAS_ACTIVATED);
    // validate add pp request
    boolean newlyAddedL5HasSyncStockTrue = productL3UpdateRequest.getAddPickupPoints().stream()
        .anyMatch(itemPickupPointRequest -> Boolean.TRUE.equals(itemPickupPointRequest.getSynchronizeStock()));
    if (merchantFAASFlag && (modifiedL5requestHasSyncStockTrueL5 || newlyAddedL5HasSyncStockTrue)) {
      apiErrorCode = ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR;
    }
    return apiErrorCode;
  }

  public static void validateSyncStockForFAASMerchantsInEditRequest(
      ProductVariantUpdateRequest productVariantUpdateRequest, ProfileResponse profileResponse,
      boolean faasFeatureSwitch) {
    boolean merchantFAASFlag = getBusinessPartnerFlagValue(profileResponse, Constants.FAAS_ACTIVATED);
    if (faasFeatureSwitch && merchantFAASFlag) {
      boolean modifiedL5requestHasSyncStockTrueL5 =
          Optional.ofNullable(productVariantUpdateRequest.getProductItems()).orElse(new ArrayList<>()).stream()
              .map(ProductVariantPriceStockAndImagesRequest::getModifiedItemPickupPoints).flatMap(List::stream).anyMatch(itemPickupPointRequest -> Boolean.TRUE.equals(
                  itemPickupPointRequest.getSynchronizeStock()));
      boolean newlyAddedL5HasSyncStockTrue =
          Optional.ofNullable(productVariantUpdateRequest.getAddPickupPoints()).orElse(new ArrayList<>()).stream().anyMatch(
              itemPickupPointRequest -> Boolean.TRUE.equals(itemPickupPointRequest.getSynchronizeStock()));
      if (modifiedL5requestHasSyncStockTrueL5 || newlyAddedL5HasSyncStockTrue) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.FAAS_SELLER_SYNC_STOCK_CHANGE_ERROR.getDesc());
      }
    }
  }

  public static boolean getSyncStockValueForFASSMerchants(boolean faasFeatureSwitch, boolean fbbActivated, ProfileResponse profileResponse) {
    if (faasFeatureSwitch && getBusinessPartnerFlagValue(profileResponse, Constants.FAAS_ACTIVATED)) {
      return false;
    }
    return fbbActivated;
  }

  public static boolean isFaasEligibleSeller(boolean faasFeatureSwitch, ProfileResponse profileResponse) {
    return faasFeatureSwitch && getBusinessPartnerFlagValue(profileResponse, Constants.FAAS_ACTIVATED);
  }

  public static void updateItemPickupPointSchedules(com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest) {
    // If all schedules are null, return to continue with next request
    if (Objects.isNull(itemPickupPointRequest.getBuyableSchedule()) && Objects.isNull(
        itemPickupPointRequest.getDiscoverableSchedule())) {
      return;
    }
    // If CNC view config is ONLINE and either of schedules are null, override with other schedules date time
    // If none are null, then set buyable schedule times for discoverable schedule
    if (itemPickupPointRequest.isCncDisplay() && itemPickupPointRequest.isCncBuyable()) {
      if (Objects.isNull(itemPickupPointRequest.getDiscoverableSchedule())) {
        itemPickupPointRequest.setDiscoverableSchedule(
            com.gda.mta.product.dto.DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(itemPickupPointRequest.getBuyableSchedule().getStartDateTime())
                .endDateTime(itemPickupPointRequest.getBuyableSchedule().getEndDateTime()).build());
      } else if (Objects.isNull(itemPickupPointRequest.getBuyableSchedule())) {
        itemPickupPointRequest.setBuyableSchedule(
            com.gda.mta.product.dto.BuyableScheduleRequest.builder().buyable(true).startDateTime(itemPickupPointRequest.getDiscoverableSchedule().getStartDateTime())
                .endDateTime(itemPickupPointRequest.getDiscoverableSchedule().getEndDateTime()).build());
      } else {
        itemPickupPointRequest.setDiscoverableSchedule(
            com.gda.mta.product.dto.DiscoverableScheduleRequest.builder().discoverable(true).startDateTime(itemPickupPointRequest.getBuyableSchedule().getStartDateTime())
                .endDateTime(itemPickupPointRequest.getBuyableSchedule().getEndDateTime()).build());
      }
    }
  }

  public static boolean isL5CncActive(ProductItemBusinessPartner productItemBusinessPartner,
      boolean cncForWarehouseFeatureSwitch) {
    return (!cncForWarehouseFeatureSwitch && productItemBusinessPartner.isCncActivated()) || (
        cncForWarehouseFeatureSwitch && (productItemBusinessPartner.isCncBuyable())
            || productItemBusinessPartner.isCncDiscoverable());
  }

  public static List<InventoryDetailInfoRequestDTO> getInventoryDetailInfoRequestDTOList(List<QuickEditV2Request> quickEditV2Requests, String businessPartnerCode) {
    return quickEditV2Requests.stream().map(quickEditV2Request -> {
      InventoryDetailInfoRequestDTO dto = new InventoryDetailInfoRequestDTO();
      dto.setWebItemSku(quickEditV2Request.getItemSku());
      dto.setPickupPointCode(quickEditV2Request.getPickupPointCode());
      dto.setWebMerchantCode(businessPartnerCode);
      return dto;
    }).collect(Collectors.toList());
  }

  public static boolean isPureInstoreProduct(boolean off2OnChannelActive, Boolean b2cActivated,
      boolean instore2FlowSwitch) {
    return instore2FlowSwitch && off2OnChannelActive && Boolean.FALSE.equals(BooleanUtils.toBooleanDefaultIfNull(b2cActivated, true));
  }

  public static boolean hasZeroDimension(double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension == 0) {
        return true;
      }
    }
    return false;
  }

  public static boolean isInvalidShipping(ProductLevel3UpdateRequest product, boolean instore2FlowSwitch) {
    return product.getProductType() != ProductType.BOPIS.getCode() && product.getShippingWeight() == 0 && !(isPureInstoreProduct(
        product.isOff2OnChannelActive(), product.getB2cActivated(), instore2FlowSwitch));
  }

  public static void setMissingFieldsForPureInstoreProduct(ProductLevel3 product, com.gdn.x.product.rest.web.model.request.ProductRequest productRequest,
      boolean instore2FlowSwitch) {
    boolean pureInstoreProduct =
        isPureInstoreProduct(product.isOff2OnChannelActive(), product.getB2cActivated(),
            instore2FlowSwitch);
    if (pureInstoreProduct) {
      Set<String> missingFields = new HashSet<>();
      if (CommonUtils.hasZeroDimension(product.getHeight(), product.getLength(), product.getWeight(),
          product.getShippingWeight()) && !product.getProductType()
          .equals(ProductType.BOPIS.getCode())) {
        missingFields.add(Constants.DIMENSIONS_MISSING);
      }
      if (StringUtils.isBlank(product.getDescription())) {
        missingFields.add(Constants.DESCRIPTION_MISSING);
      }
      productRequest.setMissingFields(missingFields);
    }
  }

  public static boolean eligibleToCheckIfDescriptionIsChanged(ProductLevel3 product, ProductLevel3 savedProduct,
      boolean instoreNewFlowEnabled) {
    if (instoreNewFlowEnabled) {
      if (Objects.isNull(savedProduct.getDescription())) {
        savedProduct.setDescription(StringUtils.EMPTY);
      }
      if (Objects.isNull(product.getDescription())) {
        product.setDescription(StringUtils.EMPTY);
      }
      return StringUtils.isNotBlank(savedProduct.getDescription()) || StringUtils.isNotBlank(product.getDescription());
    } else {
      return StringUtils.isNotBlank(savedProduct.getDescription()) && StringUtils.isNotBlank(product.getDescription());
    }
  }

  public static boolean isDescriptionChanged(ProductLevel3 product, ProductLevel3 savedProduct) {
    String savedDescription = formattedHTMLText(savedProduct.getDescription().trim());
    String description = formattedHTMLText(product.getDescription().trim());
    if (!description.equals(savedDescription)) {
      log.info(
          "Change in description for product sku : {}, savedDescription : {} , description: {} ",
          product.getProductSku(), savedDescription, description);
      return true;
    }
    return false;
  }

  private static String formattedHTMLText(String request) {
    if (request.contains("&lt;") || request.contains("&gt;") || request.contains("&amp;")) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), customWhitelist());
      return StringEscapeUtils.unescapeHtml3(request);
    } else {
      return StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, customWhitelist()));
    }
  }

  private static Whitelist customWhitelist() {
    return Whitelist.relaxed().addTags(IFRAME)
        .addAttributes(IFRAME, "align", "alt", "height", "src", "title", "width", "allowfullscreen");
  }

  public static boolean skipBrandModelForMerchantType(ProfileResponse profileResponse, String merchantTypeListToSkip) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::getMerchantType)
        .map(merchantType -> !merchantTypeListToSkip.contains(merchantType)).orElse(true);
  }

  public static ItemPickupPointListingRequest getItemPickupPointListingRequest(
      ItemPickupPointRequest itemPickupPointRequest, String productSku, String businessPartnerCode) {
    ItemPickupPointListingRequest request = new ItemPickupPointListingRequest();
    request.setItemSku(itemPickupPointRequest.getItemSku());
    request.setProductSku(productSku);
    request.setBusinessPartnerCode(businessPartnerCode);
    request.setPickupPointCodes(
        Optional.ofNullable(itemPickupPointRequest.getPickupPointId()).map(Set::of).orElse(Collections.emptySet()));
    return request;
  }

  public static List<ItemPickupPointListingRequest> getItemPickupPointListingRequestsFromAddPickupPointList(
      String businessPartnerCode, ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap) {
    return Optional.ofNullable(productVariantUpdateRequest.getAddPickupPoints()).orElse(Collections.emptyList()).stream()
        .filter(getItemPickupPointRequestPredicate(itemPickupPointListingResponseMap)).map(
            itemPickupPointRequest -> CommonUtils.getItemPickupPointListingRequest(
                itemPickupPointRequest, productVariantUpdateRequest.getProductSku(),
                businessPartnerCode)).collect(Collectors.toList());
  }

  private static Predicate<ItemPickupPointRequest> getItemPickupPointRequestPredicate(
      Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap) {
    return itemPickupPointRequest -> {
      String itemSku =
          Optional.ofNullable(itemPickupPointRequest.getItemSku()).orElse(StringUtils.EMPTY);
      String pickupPointCode =
          Optional.ofNullable(itemPickupPointRequest.getPickupPointId()).orElse(StringUtils.EMPTY);
      String offlineItemId = itemSku.concat(Constants.HYPHEN).concat(pickupPointCode);
      return !itemPickupPointListingResponseMap.containsKey(offlineItemId);
    };
  }

  public static SolrReviewProductCollectionAddEvent getSolrReviewProductCollectionAddEvent(String id,
      boolean brandApproved, String brandName) {
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent = new SolrReviewProductCollectionAddEvent();
    setFieldsAndValues(id, brandApproved, brandName, solrReviewProductCollectionAddEvent);
    return solrReviewProductCollectionAddEvent;
  }

  private static void setFieldsAndValues(String id, boolean brandApproved, String brandName,
      SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent) {
    Map<String, Object> fieldsAndValues = new HashMap<>();
    fieldsAndValues.put(com.gdn.partners.pbp.commons.util.SolrFieldNames.ID, id);
    fieldsAndValues.put(com.gdn.partners.pbp.commons.util.SolrFieldNames.BRAND_APPROVED, Collections.singletonMap(SolrConstants.SET, brandApproved));
    fieldsAndValues.put(SolrFieldNames.BRAND, brandName);
    solrReviewProductCollectionAddEvent.setFieldsAndValues(fieldsAndValues);
    solrReviewProductCollectionAddEvent.setSolrReviewProductCollectionAddEventFieldsList(new ArrayList<>());
  }

  public static SolrProductCollectionUpdateEvent getSolrProductCollectionUpdateEvent(String documentId) {
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent = new SolrProductCollectionUpdateEvent();
    solrProductCollectionUpdateEvent.setDocumentId(documentId);
    return solrProductCollectionUpdateEvent;
  }

  public static void resetRestrictedKeywordToActionTypeMapForMultipleCategoryChangeKeywords(Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap) {

    if (MapUtils.isEmpty(restrictedKeywordToActionTypeMap) || restrictedKeywordToActionTypeMap.size() <= Constants.ONE) {
      return;
    }

    // Step 1: Collect destination categories of entries with action category change
    Set<String> destinationCategories = new HashSet<>();
    List<String> keywordsList = new ArrayList<>();

    for (Map.Entry<String, RestrictedKeywordsMappedToCategoryResponse> entry : restrictedKeywordToActionTypeMap.entrySet()) {
      RestrictedKeywordsMappedToCategoryResponse response = entry.getValue();
      String destinationCategory = response.getDestinationCategory();
      if (RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType()
          == response.getAction() && StringUtils.isNotBlank(destinationCategory)) {
        destinationCategories.add(destinationCategory);
        keywordsList.add(entry.getKey());
      }
    }

    // Step 2: If more than one distinct destination category, reset matching entries in the map
    if (destinationCategories.size() > Constants.ONE) {
      for (String keyword : keywordsList) {
        RestrictedKeywordsMappedToCategoryResponse response = restrictedKeywordToActionTypeMap.get(keyword);
        response.setAction(Constants.ONE);
        response.setDestinationCategory(StringUtils.EMPTY);
        restrictedKeywordToActionTypeMap.put(keyword, response);
      }
    }
  }

  public static boolean isProductScoreGenerationNeeded(boolean upcCodeUpdate, boolean youTubeUrlUpdated, Boolean videoUpdated) {
    return Boolean.TRUE.equals(videoUpdated) || upcCodeUpdate || youTubeUrlUpdated;
  }


  public static void setYouTubeUrlUpdated(ProductLevel3 product, EditProductResponse editProductResponse,
      ProductLevel3 productLevel3) {
    String savedUrl = (Objects.isNull(editProductResponse.getProduct())) ?
        productLevel3.getUrl() :
        editProductResponse.getProduct().getUrl();

    boolean isUpdated = !StringUtils.equals(savedUrl, product.getUrl());
    editProductResponse.setYouTubeUrlUpdated(isUpdated);
  }

  public static boolean sizeChartCodeUpdated(ProductMasterDataEditRequest productMasterDataEditRequest) {
    return StringUtils.isNotBlank(productMasterDataEditRequest.getSizeChartCode()) && productMasterDataEditRequest.getMasterDataEditChangeTypes()
        .contains(L3InfoUpdateChangeType.SIZE_CHART_UPDATE);
  }

  public static void processRestrictedKeywords(MasterProductEditDTO masterProductEditDTO,
      ProductMasterDataEditRequest editRequest) throws JsonProcessingException {
    ProductCollection productCollection = masterProductEditDTO.getProductCollection();
    boolean hasRestrictedKeywords = !CollectionUtils.isEmpty(masterProductEditDTO.getRestrictedKeywordsByFieldList());
    boolean hasRestrictedKeywordsByFieldAndActionType = CollectionUtils.isNotEmpty(
        masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType().getRestrictedKeywordsByFieldList());
    if (hasRestrictedKeywords) {
      boolean reviewConfiguration = editRequest.isTrustedSeller() && Optional.ofNullable(
              masterProductEditDTO.getConfigurationStatusResponseList()).orElse(Collections.emptyList())
          .stream().map(ConfigurationStatusResponse::getReviewConfig).findFirst().map(Constants.POST_LIVE::equals).orElse(false);
      String allTheRestrictedKeywords = OBJECT_MAPPER.writeValueAsString(masterProductEditDTO.getRestrictedKeywordsByFieldList());
      productCollection.setPostLive(reviewConfiguration);
      productCollection.setRestrictedKeywordsPresent(true);
      productCollection.setRestrictedKeywordsDetected(allTheRestrictedKeywords);
      masterProductEditDTO.setAction(
          masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType().getAction());
      masterProductEditDTO.setCategoryRestrictedKeywordId(
          masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType().getCategoryRestrictedKeywordId());
      if (hasRestrictedKeywordsByFieldAndActionType) {
        for (RestrictedKeywordsByField restrictedKeywordsByField : masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType()
            .getRestrictedKeywordsByFieldList()) {
          masterProductEditDTO.getVendorErrorFields().add(restrictedKeywordsByField.getFieldIdentifier());
        }
      }
    } else if (hasRestrictedKeywordsByFieldAndActionType) {
      String restrictedKeywordsDetected = OBJECT_MAPPER.writeValueAsString(
          masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType().getRestrictedKeywordsByFieldList());
      productCollection.setRestrictedKeywordsDetected(restrictedKeywordsDetected);
      productCollection.setRestrictedKeywordsPresent(true);
    }
    Optional.of(productCollection).map(ProductCollection::getReviewType).ifPresent(reviewType -> {
      masterProductEditDTO.setReviewTypeList(
          Stream.of(reviewType.split(Constants.COMMA)).map(String::trim).collect(Collectors.toList()));
    });
  }

  public static void handleAutoApprovalOnMasterDataEdit(MasterProductEditDTO masterProductEditDTO) {

    ProductCollection productCollection = masterProductEditDTO.getProductCollection();
    AutoApprovalType autoApprovalType = masterProductEditDTO.getAutoApprovalType();

    if (AutoApprovalType.CONTENT_AND_IMAGE.equals(autoApprovalType)) {
      handleContentAndImageApproval(masterProductEditDTO, productCollection);
      return;
    }

    boolean shouldTriggerReview =
        publishPDTContentEditEvent(masterProductEditDTO.isContentChanged(), masterProductEditDTO.getRestrictedKeywordsByFieldList(),
            false);

    if (shouldTriggerReview) {
      markForContentEditReview(masterProductEditDTO, productCollection);
    } else if (productCollection.isReviewPending()) {
      handlePendingReview(masterProductEditDTO, productCollection);
    }
  }

  private static void handleContentAndImageApproval(MasterProductEditDTO masterProductEditDTO,
      ProductCollection productCollection) {

    masterProductEditDTO.setSaveInternalHistory(true);
    masterProductEditDTO.setAutoApprovalEligible(true);

    if (productCollection.isReviewPending()) {
      masterProductEditDTO.setContentType(EditedReviewTypeConstants.CONTENT_REFRESH);
      productCollection.setEdited(true);
    }
  }

  private static void markForContentEditReview(MasterProductEditDTO masterProductEditDTO,
      ProductCollection productCollection) {

    productCollection.setReviewPending(true);
    productCollection.setEdited(true);
    masterProductEditDTO.setAutoApprovalEligible(false);
    masterProductEditDTO.setContentType(EditedReviewTypeConstants.CONTENT_EDIT);

    if (!masterProductEditDTO.getReviewTypeList().contains(EditedReviewTypeConstants.CONTENT_EDIT)) {
      masterProductEditDTO.getReviewTypeList().add(EditedReviewTypeConstants.CONTENT_EDIT);
    }

    if (!productCollection.isPostLive()) {
      masterProductEditDTO.setTakenDownProduct(true);
    }
  }

  private static void handlePendingReview(MasterProductEditDTO masterProductEditDTO,
      ProductCollection productCollection) {

    productCollection.setEdited(true);
    masterProductEditDTO.setContentType(EditedReviewTypeConstants.CONTENT_REFRESH);
    masterProductEditDTO.setAutoApprovalEligible(false);

    if (!masterProductEditDTO.isPostLive()) {
      masterProductEditDTO.setTakenDownProduct(true);
    }
  }


  public static void generateModifiedFieldsForMasterDataEdit(Set<L3InfoUpdateChangeType> masterDataEditChangeTypes,
      MasterProductEditDTO masterProductEditDTO) {
    for (L3InfoUpdateChangeType changeType : masterDataEditChangeTypes) {
      Set<String> modifiedFields = Optional.ofNullable(masterProductEditDTO.getModifiedFields()).orElseGet(() -> {
        Set<String> initSet = new HashSet<>();
        masterProductEditDTO.setModifiedFields(initSet);
        return initSet;
      });
      switch (changeType) {
        case DESCRIPTION_UPDATE -> modifiedFields.add(UpdateProductActivity.PRODUCT_DESC.name());
        case PRODUCT_NAME_UPDATE -> modifiedFields.add(UpdateProductActivity.PRODUCT_NAME.name());
        case YOUTUBE_URL_UPDATE -> modifiedFields.add(Constants.URL_VIDEO_EDITED);
        case PRODUCT_TYPE_UPDATE -> modifiedFields.add(UpdateProductActivity.PRODUCT_TYPE.name());
      }
    }
  }

  public static Boolean evaluateLateFulfillmentOnShippingTypeChange(Integer productType, boolean isProductTypeChanged, Boolean lateFulfillment) {
    if (isProductTypeChanged && Objects.nonNull(productType)) {
      lateFulfillment = ProductType.REGULAR.getCode() != productType;
    }
    return lateFulfillment;
  }

  public static boolean isEligibleForAutoRejectOrAutoNeedRevision(int action, boolean trustedSeller) {
    EnumSet<RestrictedKeywordActionType> autoActions =
        EnumSet.of(RestrictedKeywordActionType.AUTO_REJECT, RestrictedKeywordActionType.AUTO_NEED_REVISION);
    return autoActions.stream().anyMatch(type -> type.getRestrictedKeywordActionType() == action) && !trustedSeller;
  }

  public static List<ImageRequest> getImageRequestForResizeEvent(ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception {
    List<ImageRequest> imageRequests = new ArrayList<>();
    List<ProductLevel3SummaryDetailsImageRequest> newImageRequests = getNewlyAddedImages(productMasterDataEditRequest);
    Map<String, String> imageLocationHashCodeMap = fetchImageLocationHashCodeMap(newImageRequests);
    for (ProductLevel3SummaryDetailsImageRequest imageRequest : newImageRequests) {
      ImageRequest request = new ImageRequest();
      request.setAbsoluteImagePath(imageRequest.getLocationPath());
      request.setCommonImage(true);
      request.setEdited(false);
      request.setHashCode(imageLocationHashCodeMap.get(imageRequest.getLocationPath()));
      imageRequests.add(request);
    }
    return imageRequests;
  }

  @NotNull
  private static List<ProductLevel3SummaryDetailsImageRequest> getNewlyAddedImages(
      ProductMasterDataEditRequest productMasterDataEditRequest) {
    return Optional.ofNullable(productMasterDataEditRequest.getProductLevel3SummaryDetailsImageRequests())
        .orElse(Collections.emptyList()).stream().filter(Objects::nonNull).filter(req -> Constants.NEW.equalsIgnoreCase(req.getReviewType())).toList();
  }

  @NotNull
  private static Map<String, String> fetchImageLocationHashCodeMap(List<ProductLevel3SummaryDetailsImageRequest> newImageRequests) throws Exception {
    List<Image> images = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImageRequest : newImageRequests) {
      Image productImage = new Image();
      productImage.setCommonImage(true);
      productImage.setMainImages(productLevel3SummaryDetailsImageRequest.getMainImage());
      productImage.setLocationPath(productLevel3SummaryDetailsImageRequest.getLocationPath());
      images.add(productImage);
    }
    ProductRequest productRequest = new ProductRequest();
    productRequest.setImages(images);
    ProductRequest productRequestsWithLocation = ApproveProductUtils.generateImageHashcode(productRequest);
    return productRequestsWithLocation.getImages().stream().collect(Collectors.toMap(Image::getLocationPath, Image::getHashCode));
  }

  public static void isEligibleForTakeDown(MasterProductEditDTO masterProductEditDTO,
      ProductMasterDataEditRequest productMasterDataEditRequest) {
    if (!masterProductEditDTO.isContentChanged() && CollectionUtils.isEmpty(getNewlyAddedImages(productMasterDataEditRequest))) {
      return;
    }
    boolean shouldTakeDown = !masterProductEditDTO.isPostLive();

    if (masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType().getAction() == RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType()
        && !productMasterDataEditRequest.isTrustedSeller()) {
      shouldTakeDown = true;
    }
    if (shouldTakeDown) {
      masterProductEditDTO.setTakenDownProduct(true);
    }
  }

  public static boolean isEligibleForDuplicateAttributeValidation(boolean validateAttributeAdditionForOnlyNewlyAddedAttributes, ProductLevel3 request) {
    if (validateAttributeAdditionForOnlyNewlyAddedAttributes) {
      return CollectionUtils.isNotEmpty(request.getNewlyAddedItems()) || CollectionUtils.isNotEmpty(
          request.getDeletedItems());
    } else {
      return true;
    }
  }

  public static void sanitiseDimensionsForBopisProductType(ProductMasterDataEditRequest productMasterDataEditRequest) {
    if (productMasterDataEditRequest.getMasterDataEditChangeTypes().contains(L3InfoUpdateChangeType.DIMENSIONS_UPDATE)
        && productMasterDataEditRequest.getProductType().equals(ProductType.BOPIS.getCode())) {
      productMasterDataEditRequest.setLength(ZERO_DIMENSION);
      productMasterDataEditRequest.setWidth(ZERO_DIMENSION);
      productMasterDataEditRequest.setHeight(ZERO_DIMENSION);
      productMasterDataEditRequest.setWeight(ZERO_DIMENSION);
      productMasterDataEditRequest.setShippingWeight(ZERO_DIMENSION);
    }
  }

  public static void overrideFlagsBasedOnDistributionPP(ProductCreationRequest request,
      List<PickupPointResponse> pickupPointResponseList) {
    List<String> distributionTruePickupPointCodes = pickupPointResponseList.stream().filter(
        pickupPointResponse -> Boolean.TRUE.equals(
            Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
                .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))).map(PickupPointResponse::getCode).toList();
    for (ProductItemCreationRequest itemCreationRequest : request.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : itemCreationRequest.getPickupPoints()) {
        if (distributionTruePickupPointCodes.contains(pickupPointCreateRequest.getPickupPointId())) {
          if (StringUtils.isBlank(itemCreationRequest.getMerchantSku())) {
            log.error("OmniChannel sku empty for productCode {} and item {} ", request.getProductCode(),
                itemCreationRequest.getItemGeneratedName());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.MANDATORY_OMNI_CHANNEL_SKU.getDesc());
          }
          pickupPointCreateRequest.setDistribution(true);
          pickupPointCreateRequest.setBuyable(false);
          pickupPointCreateRequest.setDisplay(false);
          pickupPointCreateRequest.setCncActive(false);
          pickupPointCreateRequest.setCncDisplay(false);
          pickupPointCreateRequest.setCncBuyable(false);
        } else {
          pickupPointCreateRequest.setDistribution(false);
        }
      }
    }
  }

  public static void overrideImageExtensionToWebp(ProductL3UpdateRequest productL3UpdateRequest) {
    generateNewWebpImagePathsForCommonImages(productL3UpdateRequest);
    generateNewWebpImagePathsAtItems(productL3UpdateRequest);
  }

  public static void overrideImageExtensionToWebpInMoreThan50VCase(ProductVariantUpdateRequest productVariantUpdateRequest) {
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())) {
      Optional.ofNullable(productVariantUpdateRequest.getProductItems()).orElse(new ArrayList<>())
          .forEach(productItem -> productItem.getImages().stream().filter(image -> Constants.NEW.equals(image.getReviewType())).forEach(image -> {
            if (StringUtils.isNotBlank(image.getLocationPath()) && !image.getLocationPath().endsWith(Constants.WEBP_EXTENSION)) {
              generateWebpImageName(image);
            }
          }));
    }
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getCopyToAllVariantImages())) {
      Optional.ofNullable(productVariantUpdateRequest.getCopyToAllVariantImages()).orElse(new ArrayList<>()).stream()
          .filter(image -> Constants.NEW.equals(image.getReviewType())).forEach(image -> {
            if (StringUtils.isNotBlank(image.getLocationPath()) && !image.getLocationPath().endsWith(Constants.WEBP_EXTENSION)) {
              generateWebpImageName(image);
            }
          });
    }
  }

  private static void generateNewWebpImagePathsForCommonImages(ProductL3UpdateRequest productL3UpdateRequest) {
    if (CollectionUtils.isNotEmpty(productL3UpdateRequest.getCommonImages())) {
      Optional.ofNullable(productL3UpdateRequest.getCommonImages()).orElse(new ArrayList<>()).stream().filter(image -> Constants.NEW.equals(image.getReviewType())).forEach(image -> {
        if (StringUtils.isNotBlank(image.getLocationPath()) && !image.getLocationPath().endsWith(Constants.WEBP_EXTENSION)) {
          generateWebpImageName(image);
        }
      });
    }
  }

  private static void generateNewWebpImagePathsAtItems(ProductL3UpdateRequest productL3UpdateRequest) {
    if (CollectionUtils.isNotEmpty(productL3UpdateRequest.getProductItems())) {
      Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>())
          .forEach(productItem -> productItem.getImages().stream().filter(image -> Constants.NEW.equals(image.getReviewType())).forEach(image -> {
            if (StringUtils.isNotBlank(image.getLocationPath()) && !image.getLocationPath().endsWith(Constants.WEBP_EXTENSION)) {
              generateWebpImageName(image);
            }
          }));
    }
  }

  private static void generateWebpImageName(ProductLevel3SummaryDetailsImageRequest image) {
    int extensionIndex = image.getLocationPath().lastIndexOf(Constants.DOT);
    String webpFileName = image.getLocationPath().substring(0, extensionIndex) + Constants.WEBP_EXTENSION;
    image.setLocationPath(webpFileName);
  }

  public static InventoryBaseRequest generateInventoryBaseRequest(String actionKey) {
    InventoryBaseRequest inventoryBaseRequest = new InventoryBaseRequest();
    inventoryBaseRequest.setActionKey(actionKey);
    inventoryBaseRequest.setCustomerLogonId(GdnMandatoryRequestParameterUtil.getUsername());
    inventoryBaseRequest.setTransactionDate(new Date());
    return inventoryBaseRequest;
  }

  public static boolean isPreOrderActiveByDate(Date preOrderEndDate) {
    return Objects.nonNull(preOrderEndDate) && new Date().before(preOrderEndDate);
  }

  public static boolean validateOmgSellerAndPreOrderType(String preOrderType) {
    return StringUtils.isNotBlank(preOrderType) && preOrderType.equals(OMG_VALID_PREORDER_TYPE);
  }

  public static boolean validateForPreOrderStockChecks(int stock, Date preOrderDate) {
    return isPreOrderActiveByDate(preOrderDate) && stock == 0;
  }

  public static ApiErrorCode validatePreOrderStockForEdit(List<ItemPickupPointRequest> itemPickupPointRequests,
      PreOrderRequest preOrderRequest, ApiErrorCode apiErrorCode) {
    for (ItemPickupPointRequest itemPickupPointRequest : itemPickupPointRequests) {
      if (!validateForPreOrderStockChecks(itemPickupPointRequest.getStock(), preOrderRequest.getPreOrderDate())) {
        return ApiErrorCode.INVALID_STOCK_UPDATE_PREORDER_OMG;
      }
    }
    return apiErrorCode;
  }

  public static boolean isPreOrderDateValid(Date preOrderDate) {
    return Objects.nonNull(preOrderDate) && preOrderDate.after(new Date());
  }

  public static boolean shouldPopulatePreOrderDetails(ProfileResponse businessPartner,
      Date preOrderDate) {
    if (!getBusinessPartnerFlagValue(businessPartner, Constants.BLIBLI_OMG)) {
      return false;
    }
    return isPreOrderDateValid(preOrderDate);
  }

  public static void setPreOrderFields(boolean preOrderQuotaFeatureSwitch,
      ProfileResponse profileResponse, Date preOrderDate,
      ProductLevel3Inventory productLevel3Inventory, Integer preOrderQuota) {
    if (preOrderQuotaFeatureSwitch && shouldPopulatePreOrderDetails(profileResponse,
        preOrderDate)) {
      productLevel3Inventory.setPreOrderDate(preOrderDate);
      productLevel3Inventory.setInitialPreOrderQuota(
          ObjectUtils.defaultIfNull(preOrderQuota, ZERO));
    }
  }

  public static String extractProductSkuFromItemSku(String itemSku) {
    if (StringUtils.isNotBlank(itemSku)) {
      int idx = -1;
      for (int count = 0; count < 3; count++) {
        idx = itemSku.indexOf(Constants.HYPHEN, idx + 1);
        if (idx == -1) {
          return StringUtils.EMPTY;
        }
      }
      return itemSku.substring(0, idx);
    }
    return StringUtils.EMPTY;
  }

  public static Map<String, OmniChannelSkuResponse> convertToOmniChannelSkuResponse(
      ValidOmniChannelSkuResponse validOmniChannelSkuResponse,
      Map<String, String> omniChannelSkuAndItemNameInRequestMap) {
    Map<String, OmniChannelSkuResponse> existingOmniChannelSkusAndProductDetailsMap = new HashMap<>();
    if (Objects.nonNull(validOmniChannelSkuResponse) && MapUtils.isNotEmpty(
        validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap())) {
      for (Map.Entry<String, ProductL1AndL2CodeResponse> omniChannelSkuResponseEntry : validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap()
          .entrySet()) {
        OmniChannelSkuResponse omniChannelSkuResponse = new OmniChannelSkuResponse();
        BeanUtils.copyProperties(omniChannelSkuResponseEntry.getValue(), omniChannelSkuResponse);
        omniChannelSkuResponse.setSellerSku(omniChannelSkuResponseEntry.getValue().getOmniChannelSku());
        omniChannelSkuResponse.setProductSku(
            CommonUtils.extractProductSkuFromItemSku(omniChannelSkuResponse.getItemSku()));
        omniChannelSkuResponse.setRequestedItemName(
            omniChannelSkuAndItemNameInRequestMap.get(omniChannelSkuResponse.getSellerSku()));
        existingOmniChannelSkusAndProductDetailsMap.put(omniChannelSkuResponse.getSellerSku(), omniChannelSkuResponse);
      }
    }
    return existingOmniChannelSkusAndProductDetailsMap;
  }

  public static boolean validateStockIncrementForSellerPenalty(ProfileResponse businessPartner,
    boolean sellerPenaltyEnabledPhase2, Integer stock) {
    return sellerPenaltyEnabledPhase2 && CommonUtils.getBusinessPartnerFlagValue(businessPartner,
      Constants.PRODUCT_CONSEQUENCE_LIMITATION) && Objects.nonNull(stock) && stock > 0;
  }

  public static ApiErrorCode checkCategoryBopisEligibility(Integer productType, CategoryDetailResponse category, String productCode) {
    if (Objects.isNull(productType) || Objects.isNull(category)) {
      return null;
    }
    boolean isBopisProduct = productType.equals(ProductType.BOPIS.getCode());
    boolean categoryEligible = Boolean.TRUE.equals(category.isBopisEligible());
    if (isBopisProduct && !categoryEligible) {
      log.error("Category {} is not eligible for BOPIS for productCode {} ", category.getCategoryCode(), productCode);
      return ApiErrorCode.BOPIS_CATEGORY_ELIGIBILTY_ERROR;
    }
    return null;
  }

  public static void validateIfDistributionInfoIsMissing(
      List<ProductItemDistributionInfoRequest> productItemDistributionInfoRequests,
      Map<String, String> distributionInfoRequest, String productCode) {
    if (MapUtils.isEmpty(distributionInfoRequest) || productItemDistributionInfoRequests.stream().anyMatch(
        productItemDistributionInfoRequest -> Objects.isNull(
            productItemDistributionInfoRequest.getDistributionItemInfoRequest()))
        || productItemDistributionInfoRequests.stream().anyMatch(
        productItemDistributionInfoRequest -> CollectionUtils.isEmpty(
            productItemDistributionInfoRequest.getDimensionsAndUOMRequest()))) {
      log.error("Distribution data is incomplete for product : {} distributionInfo : {} distributionItemInfo : {}",
          productCode, distributionInfoRequest, productItemDistributionInfoRequests);
      throw new ApiDataNotFoundException(ApiErrorCode.INCOMPLETE_DISTRIBUTION_INFO.getDesc(),
          ApiErrorCode.INCOMPLETE_DISTRIBUTION_INFO);
    }
  }

  public static boolean isBrandUpdated(String newBrandCode, String oldBrandCode) {
    return StringUtils.isNotEmpty(newBrandCode) && StringUtils.isNotEmpty(oldBrandCode)
        && !newBrandCode.equals(oldBrandCode);
  }

  public static boolean eligibleForAutoNeedRevision(boolean sendProductToAutoNROnBrandOrCategoryTakeDown,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse, ProfileResponse profileResponse,
      String whMerchantTypes) {
    return sendProductToAutoNROnBrandOrCategoryTakeDown && (autoNeedRevisionAndForceReviewResponse.isCategoryTakeDown()
        || autoNeedRevisionAndForceReviewResponse.isBrandTakeDown()) && !isWareHouseSeller(whMerchantTypes,
        Optional.of(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::getMerchantType)
            .orElse(StringUtils.EMPTY));
  }

  public static boolean isWareHouseSeller(String wHMerchantTypes, String commissionType) {
    return Optional.ofNullable(wHMerchantTypes).stream().flatMap(types -> Arrays.stream(types.split(Constants.COMMA)))
        .anyMatch(commissionType::equals);
  }

  public static boolean isBrandNameUpdated(String newBrandName, String oldBrandName) {
    return StringUtils.isNotEmpty(newBrandName) && StringUtils.isNotEmpty(oldBrandName)
        && !newBrandName.equals(oldBrandName);
  }

  public static boolean isCategoryCodeUpdated(String newCategoryCode, String oldCategoryCode) {
    return StringUtils.isNotEmpty(newCategoryCode) && StringUtils.isNotEmpty(oldCategoryCode)
        && !newCategoryCode.equals(oldCategoryCode);
  }

  // Convert system-local Date to UTC Date
  // Takes a Date that represents a local time in the system timezone and converts it to UTC
  // This method ensures that a date/time created with local time components is properly converted
  // to UTC for transmission in requests. Since Date stores UTC internally, we extract the local
  // time components using Calendar (which respects system timezone) and rebuild as UTC.
  public static Date convertToUtc(Date localDate) {
    if (localDate == null)
      return null;
    
    ZoneId systemZone = ZoneId.systemDefault();
    
    // The key insight: Date stores UTC internally. If it shows "10:08:01 WIB" (UTC+7),
    // the UTC value is actually "03:08:01 UTC" (7 hours earlier).
    // To convert "local time to UTC" (keep same displayed time but in UTC),
    // we need to ADD the timezone offset to the UTC value.
    // Example: "10:08:01 WIB" (which is 03:08:01 UTC) should become "10:08:01 UTC"
    // So: 03:08:01 UTC + 7 hours = 10:08:01 UTC
    // Get the timezone offset for this specific date (handles DST correctly)
    int offsetSeconds = systemZone.getRules().getOffset(localDate.toInstant()).getTotalSeconds();
    // Add the offset to convert local time to UTC (same displayed time, but UTC)
    Instant utcInstant = localDate.toInstant().plusSeconds(offsetSeconds);
    return Date.from(utcInstant);
  }

}
