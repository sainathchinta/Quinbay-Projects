package com.gdn.mta.product.service;

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
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.*;
import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.util.ProductLevel3InventoryUtil;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.VariantsErrorListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.ProductContentUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.commons.enums.MerchantType;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.rest.web.model.dto.UpdateDiscountDTO;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class VariantEditValidationServiceImpl implements VariantEditValidationService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3ServiceBean.class);

  @Value("${max.wholesale.price.requests}")
  private int maxWholesalePriceRequests;

  @Value("${inventory.L5.sync.stock.enabled}")
  private boolean inventoryL5SyncStockEnabled;

  @Value("${validate.newly.added.items}")
  private boolean validateNewlyAddedItems;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${product.bundling.max.number.of.skus}")
  private int productBundlingMaxNumberOfSkus;

  @Value("${validate.attribute.map.for.newly.added.items}")
  private boolean validateAttributeMapForNewlyAddedItems;

  @Value("${validate.item.sku.in.add.l5.request}")
  private boolean validateItemSkuInAddL5Requests;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Value("${inventory.batch.update.delete.batch.size}")
  private int inventoryBatchUpdateDeleteBatchSize;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private CampaignOutbound campaignOutbound;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductInventoryService productInventoryService;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  @Lazy
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value(value = "${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value("${warehouse.bom.activated}")
  private boolean warehouseBomActivated;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${validate.warehouse.variant.deletion.enabled}")
  private boolean validateWarehouseVariantDeletionEnabled;

  @Value("${validate.warehouse.deletion.eligible.sellers}")
  private String validateWarehouseDeletionEligibleSellers;

  @Value("${l5.inventory.fetch.batch.size}")
  private int l5InventoryFetchBatchSize;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${relax.distribution.l5.addition.from.bulk}")
  private boolean relaxDistributionL5AdditionFromBulk;

  @Override
  public void validateListOfVariants(List<ProductPriceStockAndImagesRequest> requests,
      List<ProductPriceStockAndImagesRequest> successRequests, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseMap) throws Exception {
    Integer minimumPrice = getMinimumPrice();
    List<String> validExtension = getvalidExtension();
    Map<String, String> skuCodeUpcCodeMap= getExistingUpcCodesForSkuCode(requests);
    for (ProductPriceStockAndImagesRequest request : requests) {
      try {
        ItemSummaryResponse itemSummaryResponse = itemSummaryResponseMap.get(request.getItemSku());
        validateVersion(request, failedRequests, itemSummaryResponse);
        validateUpcCode(request, failedRequests, skuCodeUpcCodeMap);
        validateAndUpdateWholesalePrice(request, failedRequests, itemSummaryResponse);
        validatePriceRequest(request, failedRequests, minimumPrice);
        validateImageRequest(request, failedRequests, validExtension);
        validateMerchantSku(request, failedRequests);
        successRequests.add(request);
      } catch (Exception e) {
        log.error("Exception while price and stock update validation {} ", e);
        continue;
      }
    }
  }

  @Override
  public void validateListOfVariantsL5(List<ProductVariantPriceStockAndImagesRequest> requests,
      Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
      boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse) throws Exception {
    Integer minimumPrice = getMinimumPrice();
    List<String> validExtension = getvalidExtension();
    for (ProductVariantPriceStockAndImagesRequest request : requests) {
      validateAndUpdateWholesalePriceL5(request, wholesalePriceSkuResponseMap,
          failedRequests, itemPickupPointListingResponseMap, minimumPrice,
        multiPickupPointEnabledForSeller, wholesaleMappingResponse);
      validateL5UpdateRequest(errorCode, request, minimumPrice, validExtension);
    }
  }

  private void validateL5UpdateRequest(SimpleStringResponse errorCode, ProductVariantPriceStockAndImagesRequest request,
      Integer minimumPrice, List<String> validExtension) {
    validatePriceRequestL5(request, minimumPrice, errorCode);
    validateImageRequestL5(request, validExtension, errorCode);
    validateMerchantSkuL5(request, errorCode);
  }

  @Override
  public void validateL5UpdateRequestNew(ProductVariantUpdateRequest productVariantUpdateRequest,
      SimpleStringResponse errorCode, List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests,
    ProfileResponse profileResponse) throws Exception {
    if(Objects.isNull(profileResponse)){
      profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(
        productVariantUpdateRequest.getBusinessPartnerCode());
    }
    validateL5UpdateRequest(productVariantUpdateRequest, errorCode, profileResponse);
    Integer minimumPrice = getMinimumPrice();
    validatePriceAndMerchantSkuForNewlyAddedItems(productVariantUpdateRequest, errorCode,
        newlyAddedProductItemRequests, minimumPrice);
    if (Boolean.TRUE.equals(productVariantUpdateRequest.isFreeSample())) {
      validateStatusAndCNCUpdateAtL5(productVariantUpdateRequest, errorCode);
    }
    for (ProductVariantPriceStockAndImagesRequest request : productVariantUpdateRequest.getProductItems()) {
      validatePriceAndMerchantSku(errorCode, request, minimumPrice);
    }
  }

  private void validateMerchantSku(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests) {
    if(StringUtils.isNotEmpty(request.getMerchantSku()) && request.getMerchantSku().length() > 255) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getCode(), ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc());
    }
  }

  private void validateMerchantSkuL5(ProductVariantPriceStockAndImagesRequest request, SimpleStringResponse errorCode) {
    for (ItemPickupPointRequest itemPickupPointRequest : request.getModifiedItemPickupPoints()) {
      validateSellerSku(request.getItemSku(), itemPickupPointRequest, errorCode);
    }
  }

  private void validateMerchantSkuL5(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests) {
    if(StringUtils.isNotEmpty(request.getMerchantSku()) && request.getMerchantSku().length() > 255) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getCode(), ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc());
    }
  }

  private void validateSellerSku(String itemSku, ItemPickupPointRequest itemPickupPointRequest,
      SimpleStringResponse errorCode) {
    if (StringUtils.isNotEmpty(itemPickupPointRequest.getSellerSku())
        && itemPickupPointRequest.getSellerSku().length() > 255) {
      errorCode.setResult(ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc() + "for Item " + itemSku);
    }
  }

  @Override
  public void validateAndSetFbbActiveFlagAtL5(ProductVariantUpdateRequest productVariantUpdateRequest,
      String businessPartnerCode, ProfileResponse profileResponse) throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequest =  productVariantUpdateRequest.getAddPickupPoints();
    if (CollectionUtils.isNotEmpty(itemPickupPointRequest)) {
      PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
      pickupPointFilterRequest.setBusinessPartnerCode(businessPartnerCode);
      CommonUtils.setWaitingDeletionForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint,
          pickupPointFilterRequest);
      pickupPointFilterRequest.setFbbActivated(true);
      Set<String> uniquePickupPointCodes =
          itemPickupPointRequest.stream().map(ItemPickupPointRequest::getPickupPointId).collect(Collectors.toSet());
      pickupPointFilterRequest.setCodes(uniquePickupPointCodes);
      List<PickupPointResponse> pickupPointResponses = Optional
          .ofNullable(businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest))
          .orElse(Collections.emptyList());
      Set<String> fbbActivatedPPCodes =
          pickupPointResponses.stream().map(PickupPointResponse::getCode).collect(Collectors.toSet());
      for (ItemPickupPointRequest pickupPointRequest : itemPickupPointRequest) {
        if (fbbActivatedPPCodes.contains(pickupPointRequest.getPickupPointId())) {
          pickupPointRequest.setFbbActive(true);
          pickupPointRequest.setSynchronizeStock(
              inventoryL5SyncStockEnabled || productInventoryService.isSyncedToLevel1Inventory(profileResponse));
        } else {
          if (!inventoryL5SyncStockEnabled) {
            pickupPointRequest.setSynchronizeStock(productInventoryService.isSyncedToLevel1Inventory(profileResponse));
          }
        }
      }
    }
    validateB2cAndB2bActivatedFlags(productVariantUpdateRequest, profileResponse);
  }

  public static void validateB2cAndB2bActivatedFlags(ProductVariantUpdateRequest request,
      ProfileResponse profileResponse) {
    validateEligibilityForB2cAndB2b(request, profileResponse);
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : request.getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        if (itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay() || itemPickupPointRequest.isCncActive()) {
          request.setB2cActivated(true);
        }
        if (Objects.nonNull(itemPickupPointRequest.getB2bFields()) && (itemPickupPointRequest.getB2bFields().isBuyable()
            || itemPickupPointRequest.getB2bFields().isDisplay())) {
          request.setB2bActivated(true);
        }
      }
    }
    for (ItemPickupPointRequest itemPickupPointRequest : request.getAddPickupPoints()) {
      if (itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay()
          || itemPickupPointRequest.isCncActive()) {
        request.setB2cActivated(true);
      }
      if (Objects.nonNull(itemPickupPointRequest.getB2bFields()) && (itemPickupPointRequest.getB2bFields().isBuyable()
          || itemPickupPointRequest.getB2bFields().isDisplay())) {
        request.setB2bActivated(true);
      }
    }
    if (Objects.nonNull(request.getB2cActivated())) {
      if (!request.getB2cActivated()) {
        request.setOnline(false);
      }
    }
  }

  private static void validateEligibilityForB2cAndB2b(ProductVariantUpdateRequest request,
      ProfileResponse profileResponse) {
    List<String> salesChannel = new ArrayList<>();
    if (Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::getSalesChannel).isPresent()) {
      salesChannel = Optional.ofNullable(profileResponse.getCompany().getSalesChannel()).orElse(new ArrayList<>());
    }
    if (!salesChannel.contains(Constants.B2C_SELLER_CHANNEL) && salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : request.getProductItems()) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          itemPickupPointRequest.setBuyable(false);
          itemPickupPointRequest.setDisplay(false);
          itemPickupPointRequest.setCncActive(false);
        }
      }
      for (ItemPickupPointRequest itemPickupPointRequest : request.getAddPickupPoints()) {
        itemPickupPointRequest.setBuyable(false);
        itemPickupPointRequest.setDisplay(false);
        itemPickupPointRequest.setCncActive(false);
      }
      request.setB2cActivated(false);
    }
    if (!salesChannel.contains(Constants.B2B_SELLER_CHANNEL) && salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : request.getProductItems()) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          itemPickupPointRequest.setB2bFields(null);
        }
      }
      for (ItemPickupPointRequest itemPickupPointRequest : request.getAddPickupPoints()) {
        itemPickupPointRequest.setB2bFields(null);
      }
      request.setB2bActivated(false);
    }
  }

  public void validateUpcCode(ProductPriceStockAndImagesRequest request, List<VariantsErrorListResponse> failedRequests,
      Map<String, String> skuCodeUpcCodeMap) {
    List<String> itemNameList = new ArrayList<>();
    if (isEligibleForUpcCodeUpdate(skuCodeUpcCodeMap, request)) {
      itemNameList = pcbFeign
          .getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              request.getUpcCode(), request.getProductCode(), request.getSkuCode()).getContent().get(0).getValue();
    }
    if (CollectionUtils.isNotEmpty(itemNameList)) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.UPC_CODE_UPDATE_FAILED.getCode(),
          ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc() + itemNameList.get(0)));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
    }
  }

  private void validateUpcCodeL5(ProductVariantPriceStockAndImagesRequest request, List<VariantsErrorListResponse> failedRequests,
      Map<String, String> skuCodeUpcCodeMap) {
    List<String> itemNameList = new ArrayList<>();
    if (StringUtils.isNotEmpty(request.getUpcCode()) && skuCodeUpcCodeMap.containsKey(request.getSkuCode())
        && !StringUtils.equals(request.getUpcCode(), skuCodeUpcCodeMap.get(request.getSkuCode()))) {
      itemNameList = pcbFeign
          .getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              request.getUpcCode(), request.getProductCode(), request.getSkuCode()).getContent().get(0).getValue();
    }
    if (CollectionUtils.isNotEmpty(itemNameList)) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.UPC_CODE_UPDATE_FAILED.getCode(),
          ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc() + itemNameList.get(0)));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
    }
  }

  @Override
  public void validateUpcCodeL5Level(Map<String, String> skuCodeUpcCodeMap, SimpleStringResponse errorCode,
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest) {
    List<String> itemNameList = new ArrayList<>();
    if (isEligibleForUpcCodeUpdate(skuCodeUpcCodeMap, productPriceStockAndImagesRequest)) {
      itemNameList =
          pcbFeign.getItemNameByUpcCodeAndProductCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              productPriceStockAndImagesRequest.getUpcCode(), productPriceStockAndImagesRequest.getProductCode(),
              productPriceStockAndImagesRequest.getSkuCode()).getContent().get(0).getValue();
    }
    if (CollectionUtils.isNotEmpty(itemNameList)) {
      errorCode.setResult(ApiErrorCode.UPC_CODE_UPDATE_FAILED.getCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc() + itemNameList.get(0));
    }
  }

  @Override
  public void validateUpcCodeL5LevelNew(Map<String, String> skuCodeUpcCodeMap, SimpleStringResponse errorCode,
      ProductVariantUpdateRequest productVariantUpdateRequest,
      List<ProductPriceStockAndImagesRequest> successValidationVariantList) {
    List<String> filteredItemCodes;
    boolean upcCodeUpdated = false;
    ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest = new ProductItemUpcCodesSkuCodesRequest();
    for (ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest : successValidationVariantList) {
      if (StringUtils.isNotEmpty(productPriceStockAndImagesRequest.getSkuCode())) {
        productItemUpcCodesSkuCodesRequest.getSkuCodes().add(productPriceStockAndImagesRequest.getSkuCode());
      }
      if (isEligibleForUpcCodeUpdate(skuCodeUpcCodeMap, productPriceStockAndImagesRequest)) {
        upcCodeUpdated = true;
        productItemUpcCodesSkuCodesRequest.getUpcCodes().add(productPriceStockAndImagesRequest.getUpcCode());
      }
    }
    if (upcCodeUpdated) {
      filteredItemCodes = checkIfUpcCodeAlreadyExists(productVariantUpdateRequest, productItemUpcCodesSkuCodesRequest);
      if (CollectionUtils.isNotEmpty(filteredItemCodes)) {
        errorCode.setResult(ApiErrorCode.UPC_CODE_UPDATE_FAILED.getCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc() + filteredItemCodes.get(0));
      }
    }
  }

  private List<String> checkIfUpcCodeAlreadyExists(ProductVariantUpdateRequest productVariantUpdateRequest,
      ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest) {
    List<String> filteredItemCodes = new ArrayList<>();
    List<String> itemCodesWithSameUpcCode = new ArrayList<>();
    List<String> deletedProductItemCodes = new ArrayList<>();
    itemCodesWithSameUpcCode =
        productOutbound.getItemCodesByUpcCodeAndProductCode(productVariantUpdateRequest.getProductCode(),
            productItemUpcCodesSkuCodesRequest);
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getDeletedProductItems())) {
      deletedProductItemCodes =
          productVariantUpdateRequest.getDeletedProductItems().stream().map(DeletedProductItems::getItemCode)
              .collect(Collectors.toList());
    }
    for (String itemCode : itemCodesWithSameUpcCode)
      if (!deletedProductItemCodes.contains(itemCode)) {
        filteredItemCodes.add(itemCode);
      }
    return filteredItemCodes;
  }

  private static boolean isEligibleForUpcCodeUpdate(Map<String, String> skuCodeUpcCodeMap,
      ProductPriceStockAndImagesRequest productPriceStockAndImagesRequest) {
    return StringUtils.isNotEmpty(productPriceStockAndImagesRequest.getUpcCode()) && skuCodeUpcCodeMap.containsKey(
        productPriceStockAndImagesRequest.getSkuCode()) && !StringUtils.equals(
        productPriceStockAndImagesRequest.getUpcCode(),
        skuCodeUpcCodeMap.get(productPriceStockAndImagesRequest.getSkuCode()));
  }

  public Map<String, String> getExistingUpcCodesForSkuCode(
      List<ProductPriceStockAndImagesRequest> successValidationVariantList) throws Exception {
    List<String> itemSkuCodes = successValidationVariantList.stream().map(ProductPriceStockAndImagesRequest::getSkuCode)
        .collect(Collectors.toList());
    return getSkuCodeProductSkuMap(itemSkuCodes);
  }

  public Map<String, String> getExistingUpcCodesForSkuCodeL5(
      List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList) throws Exception {
    List<String> itemSkuCodes = successValidationVariantList.stream().map(ProductVariantPriceStockAndImagesRequest::getSkuCode)
        .collect(Collectors.toList());
    SkuCodesRequest skuCodesRequest = ConverterUtil.convertItemSkusToSkuCodesRequest(itemSkuCodes);
    List<ProductItemResponse> productItemResponseList = productOutbound.getProductItemBySkuCodes(skuCodesRequest);
    return productItemResponseList.stream().collect(Collectors.toMap(ProductItemResponse::getSkuCode,
        productItemResponse -> Optional.ofNullable(productItemResponse.getUpcCode()).orElse(StringUtils.EMPTY)));
  }

  public Map<String, String> getExistingUpcCodesForSkuCodeFromSuccessValidationVariantList(
      List<ProductVariantPriceStockAndImagesRequest> successValidationVariantList) throws Exception {
    List<String> itemSkuCodes =
        successValidationVariantList.stream().map(ProductVariantPriceStockAndImagesRequest::getSkuCode)
            .collect(Collectors.toList());
    return getSkuCodeProductSkuMap(itemSkuCodes);
  }

  private Map<String, String> getSkuCodeProductSkuMap(List<String> itemSkuCodes) throws Exception {
    SkuCodesRequest skuCodesRequest = ConverterUtil.convertItemSkusToSkuCodesRequest(itemSkuCodes);
    List<ProductItemResponse> productItemResponseList = productOutbound.getProductItemBySkuCodes(skuCodesRequest);
    return productItemResponseList.stream().collect(Collectors.toMap(ProductItemResponse::getSkuCode,
        productItemResponse -> Optional.ofNullable(productItemResponse.getUpcCode()).orElse(StringUtils.EMPTY)));
  }

  @Override
  public void validatePriceLockCampaignRequest(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests) throws Exception {
    ProductSystemParameter productSystemParameter = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    if (Objects.nonNull(productSystemParameter) && Boolean.valueOf(productSystemParameter.getValue())) {
      UpdateDiscountDTO updateDiscountDTO = UpdateDiscountDTO.builder().itemSku(request.getItemSku())
          .sellingPrice(request.getPrices().get(0).getSalePrice()).categoryCode(request.getCategoryCode())
          .pickUpPointCode(request.getPickupPointCode()).build();
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest =
          CampaignUpdateDiscountRequest.builder().discountDTOList(Arrays.asList(updateDiscountDTO)).build();
      CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
          campaignOutbound.validateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
      if (Objects.nonNull(campaignUpdateDiscountResponse) && Objects
          .nonNull(campaignUpdateDiscountResponse.getItemSkuStatusMap()) && campaignUpdateDiscountResponse
          .getItemSkuStatusMap().containsKey(request.getItemSku())) {
        log.error("Campaign price validation failed for itemSku : {} with Error : {} ", request.getItemSku(),
            campaignUpdateDiscountResponse.getItemSkuStatusMap().get(request.getItemSku()));
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), ApiErrorCode.PRICE_UPDATE_FAILED.getDesc()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
      }
    }
  }

  @Override
  public void updatePriceLockCampaignRequestL5(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, String categoryCode, ItemPickupPointRequest modifiedItemPickupPoint) throws Exception {
    ProductSystemParameter productSystemParameter = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    if (Objects.nonNull(productSystemParameter) && Boolean.valueOf(productSystemParameter.getValue())) {
      List<UpdateDiscountDTO> updateDiscountDTOList = new ArrayList<>();
      UpdateDiscountDTO updateDiscountDTO =
          UpdateDiscountDTO.builder().itemSku(request.getItemSku()).sellingPrice(modifiedItemPickupPoint.getSalePrice())
              .categoryCode(categoryCode).pickUpPointCode(modifiedItemPickupPoint.getPickupPointId()).build();
      updateDiscountDTOList.add(updateDiscountDTO);
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest =
          CampaignUpdateDiscountRequest.builder().discountDTOList(updateDiscountDTOList).build();
      CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
          campaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
      if (MapUtils.isNotEmpty(campaignUpdateDiscountResponse.getItemSkuStatusMap())
        && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(request.getItemSku())) {
        log.error("L5 Campaign price update failed for itemSku : {}, pickupPointCode : {}, with Error : {}",
          request.getItemSku(), modifiedItemPickupPoint.getPickupPointId(),
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(request.getItemSku()));
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(),
            modifiedItemPickupPoint.getPickupPointId()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
      }
    }
  }

  @Override
  public void validatePriceLockCampaignRequestL5(ProductVariantPriceStockAndImagesRequest request,
    List<VariantsErrorListResponse> failedRequests, String categoryCode,
    ItemPickupPointRequest modifiedItemPickupPoint) throws Exception {
    ProductSystemParameter productSystemParameter = productSystemParameterService
      .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.CAMPAIGN_PRICE_VALIDATION);
    if (Objects.nonNull(productSystemParameter) && Boolean.valueOf(productSystemParameter.getValue())) {
      List<UpdateDiscountDTO> updateDiscountDTOList = new ArrayList<>();
      UpdateDiscountDTO updateDiscountDTO =
        UpdateDiscountDTO.builder().itemSku(request.getItemSku()).sellingPrice(modifiedItemPickupPoint.getSalePrice())
          .categoryCode(categoryCode).pickUpPointCode(modifiedItemPickupPoint.getPickupPointId()).build();
      updateDiscountDTOList.add(updateDiscountDTO);
      CampaignUpdateDiscountRequest campaignUpdateDiscountRequest =
        CampaignUpdateDiscountRequest.builder().discountDTOList(updateDiscountDTOList).build();
      CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        campaignOutbound.validateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
      if (Objects.nonNull(campaignUpdateDiscountResponse) && MapUtils.isNotEmpty(
        campaignUpdateDiscountResponse.getItemSkuStatusMap())
        && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(request.getItemSku())) {
        log.error(
          "L5 Campaign price validation failed for itemSku : {}, pickupPointCode : {}, with Error : {} ",
          request.getItemSku(), modifiedItemPickupPoint.getPickupPointId(),
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(request.getItemSku()));
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(),
          modifiedItemPickupPoint.getPickupPointId()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
      }
    }
  }

  @Override
  public List<ProductPriceStockAndImagesRequest> validateListOfVariantsForNeedCorrection(
      List<ProductPriceStockAndImagesRequest> requests, List<VariantsErrorListResponse> failedRequests)
      throws Exception {
    List<ProductPriceStockAndImagesRequest> successRequests = new ArrayList<>();
    Integer minimumPrice = getMinimumPrice();
    List<String> validExtension = getvalidExtension();
    Map<String, String> skuCodeUpcCodeMap = getExistingUpcCodesForSkuCode(requests);
    for (ProductPriceStockAndImagesRequest request : requests) {
      try {
        validateUpcCode(request, failedRequests, skuCodeUpcCodeMap);
        validatePriceRequest(request, failedRequests, minimumPrice);
        validateImageRequestNeedCorrection(request, failedRequests, validExtension);
        validateMerchantSku(request, failedRequests);
        successRequests.add(request);
      } catch (Exception e) {
        log.error("Exception while price and stock update validation itemSkU : {} error : ",request.getItemSku(), e);
        continue;
      }
    }
    return successRequests;
  }

  @Override
  public List<ProductVariantPriceStockAndImagesRequest> validateListOfVariantsForNeedCorrectionL5(
      List<ProductVariantPriceStockAndImagesRequest> requests, List<VariantsErrorListResponse> failedRequests)
      throws Exception {
    List<ProductVariantPriceStockAndImagesRequest> successRequests = new ArrayList<>();
    Integer minimumPrice = getMinimumPrice();
    List<String> validExtension = getvalidExtension();
    Map<String, String> skuCodeUpcCodeMap = getExistingUpcCodesForSkuCodeL5(requests);
    List<BusinessPartnerPickupPointResponse> pickupPointDetails = new ArrayList<>();
    if (cncForWarehouseFeatureSwitch) {
      List<String> pickupPointCodes =
          requests.stream().flatMap(variantRequest ->
                  Optional.ofNullable(variantRequest.getModifiedItemPickupPoints()).orElse(new ArrayList<>()).stream())
              .map(ItemPickupPointRequest::getPickupPointId).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
        pickupPointDetails = xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(pickupPointCodes);
      }
    }
    Map<String, BusinessPartnerPickupPointResponse> pickupPointResponseMap = Optional.ofNullable(pickupPointDetails)
        .orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(BusinessPartnerPickupPointResponse::getCode, Function.identity()));
    for (ProductVariantPriceStockAndImagesRequest request : requests) {
      try {
        validateUpcCodeL5(request, failedRequests, skuCodeUpcCodeMap);
        validateMerchantSkuL5(request, failedRequests);
        List<ItemPickupPointRequest> successItemPickupPointRequest = new ArrayList<>();
        for (ItemPickupPointRequest itemPickupPointRequest : request.getModifiedItemPickupPoints()) {
          itemPickupPointRequest.setItemSku(request.getItemSku());
          try {
            validatePriceRequestL5New(request, failedRequests, minimumPrice, itemPickupPointRequest);
            if (cncForWarehouseFeatureSwitch) {
              validateCncAndDeliveryFlag(request, failedRequests, itemPickupPointRequest,
                  pickupPointResponseMap.getOrDefault(itemPickupPointRequest.getPickupPointId(), null));
            }
            successItemPickupPointRequest.add(itemPickupPointRequest);
          } catch (Exception e) {
            log.error("Exception while price and stock update validation L5 itemSkU : {}, pickupPoint : {}, error : ",
                request.getItemSku(), itemPickupPointRequest.getPickupPointId(), e);
            continue;
          }
        }
        request.setModifiedItemPickupPoints(successItemPickupPointRequest);
        validateImageRequestNeedCorrectionL5(request, failedRequests, validExtension);
        successRequests.add(request);
      } catch (Exception e) {
        log.error("Exception while price and stock update validation L5 itemSkU : {} error : ", request.getItemSku(),
            e);
        continue;
      }
    }
    return successRequests;
  }

  private void validateImageRequestNeedCorrection(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, List<String> validExtension) {
    for (ProductLevel3SummaryDetailsImageRequest image : request.getImages()) {
      if (StringUtils.isEmpty(image.getLocationPath()) || Objects.isNull(image.getSequence()) || Objects.isNull(
          image.getMainImage()) || !validateImageExtension(image.getLocationPath(), validExtension)) {
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc());
      }
    }
  }

  private void validateImageRequestNeedCorrectionL5(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, List<String> validExtension) {
    for (ProductLevel3SummaryDetailsImageRequest image : request.getImages()) {
      if (StringUtils.isEmpty(image.getLocationPath()) || Objects.isNull(image.getSequence()) || Objects.isNull(
          image.getMainImage()) || !validateImageExtension(image.getLocationPath(), validExtension)) {
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc());
      }
    }
  }

  @Override
  public List<ProductPriceStockAndImagesRequest> validateListOfVariantsForMultiUsedProduct(
      List<ProductPriceStockAndImagesRequest> requests, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseMap)
      throws Exception {
    List<ProductPriceStockAndImagesRequest> successRequests = new ArrayList<>();
    Integer minimumPrice = getMinimumPrice();
    for (ProductPriceStockAndImagesRequest request : requests) {
      try {
        ItemSummaryResponse itemSummaryResponse = itemSummaryResponseMap.get(request.getItemSku());
        validateVersion(request, failedRequests, itemSummaryResponse);
        validateAndUpdateWholesalePrice(request, failedRequests, itemSummaryResponse);
        validatePriceRequest(request, failedRequests, minimumPrice);
        successRequests.add(request);
      } catch (Exception e) {
        log.error("Exception while price and stock update validation itemSkU : {} error : ",request.getItemSku(), e);
      }
    }
    return successRequests;
  }

  @Override
  public void validateListOfVariantsL5ForMultiUsedProduct(
    List<ProductVariantPriceStockAndImagesRequest> requests,
    Map<String, ItemPickupPointListingResponse> itemSummaryResponseMap,
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
    List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
    boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse) {
    Integer minimumPrice = getMinimumPrice();
    for (ProductVariantPriceStockAndImagesRequest request : requests) {
      validateAndUpdateWholesalePriceL5(request, wholesalePriceSkuResponseMap, failedRequests,
        itemSummaryResponseMap, minimumPrice, multiPickupPointEnabledForSeller, wholesaleMappingResponse);
      validatePriceAndMerchantSku(errorCode, request, minimumPrice);
    }
  }

  private void validatePriceAndMerchantSku(SimpleStringResponse errorCode, ProductVariantPriceStockAndImagesRequest request,
      Integer minimumPrice) {
    validatePriceRequestL5(request, minimumPrice, errorCode);
    validateMerchantSkuL5(request, errorCode);
  }

  public void validateVersion(ProductPriceStockAndImagesRequest request, List<VariantsErrorListResponse> failedRequests,
      ItemSummaryResponse itemSummaryResponse) {
    if (Objects.nonNull(itemSummaryResponse.getVersion()) && Objects.nonNull(request.getVersion())
        && request.getVersion() <= itemSummaryResponse.getVersion()) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.INVALID_STATE.getCode(), ApiErrorCode.INVALID_STATE.getDesc()));
      throw new ApplicationRuntimeException();
    }
  }

  @Override
  public void addToFailureList(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, String code, String message) {
    failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(), code, message));
  }

  @Override
  public ApiErrorCode checkArchivedSuspendedRejectedFaultyImageForUpdate(ProductAndItemsResponse savedProductData) {
    if (savedProductData.getItems().get(0).getArchived()) {
      return ApiErrorCode.ITEM_IS_ARCHIVED;
    } else if (savedProductData.getItems().get(0).isMarkForDelete()) {
      return ApiErrorCode.ITEM_IS_REJECTED;
    } else if (savedProductData.getProduct().isSuspended()) {
      return ApiErrorCode.ITEM_IS_SUSPENDED;
    } else if (savedProductData.getProduct().isForceReview()) {
      return ApiErrorCode.FAULTY_IMAGE_FORCE_REVIEW;
    } else {
      return null;
    }
  }

  private void validateAndUpdateWholesalePrice(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, ItemSummaryResponse itemSummaryResponse) throws Exception {
    if (Objects.nonNull(request.getWholesalePriceActivated()) && request.getWholesalePriceActivated()) {
      WholesalePriceSkuResponse wholesalePriceSkuResponse =
          productPricingOutbound.getWholesalePrice(request.getItemSku(), itemSummaryResponse.getPickupPointCode());
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
          wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
              key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
              .collect(Collectors.toList());
      ItemRequest itemRequest = new ItemRequest();
      itemRequest.setWholesalePriceActivated(request.getWholesalePriceActivated());
      itemRequest.setPrice(ConverterUtil.setOfferPrice(request.getPrices()));
      wholesaleValidationUtil
          .validateWholesaleConfigOnUpdate(itemSummaryResponse.getMasterCatalog().getCategory().getCategoryCode(),
              productItemWholesalePriceRequests, itemRequest, getMinimumPrice(), request.getItemSku(),
              request.getWholesalePriceActivated(), null);
      request.setWholesalePriceActivated(itemRequest.getWholesalePriceActivated());
      if (Boolean.FALSE.equals(itemRequest.getWholesalePriceActivated())) {
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.WHOLESALE_VALIDATION_FAILED.getCode(), ApiErrorCode.WHOLESALE_VALIDATION_FAILED.getDesc()));
      }
    }
  }

  private void validateAndUpdateWholesalePriceL5(
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
      List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemPickupPointListingResponse> itemPickupPointListingResponseMap, Integer minimumPrice,
    boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse) {
    for (ItemPickupPointRequest request : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
      ItemPickupPointListingResponse itemPickupPointListingResponse = multiPickupPointEnabledForSeller ?
          itemPickupPointListingResponseMap.get(
              productVariantPriceStockAndImagesRequest.getItemSku() + Constants.HYPHEN + request.getPickupPointId()) :
          itemPickupPointListingResponseMap.get(productVariantPriceStockAndImagesRequest.getItemSku());
      if (Objects.nonNull(request.getWholesalePriceActivated()) && request.getWholesalePriceActivated()) {
        String L5Code = request.getItemSku() + Constants.HYPHEN + request.getPickupPointId();
        WholesalePriceSkuResponse wholesalePriceSkuResponse = wholesalePriceSkuResponseMap.get(L5Code);
        if (Objects.nonNull(wholesalePriceSkuResponse)) {
        List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
            wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
                key -> new ProductItemWholesalePriceRequest(key,
                    wholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList());
        ItemRequest itemRequest = new ItemRequest();
        itemRequest.setWholesalePriceActivated(request.getWholesalePriceActivated());
        Set<PriceRequest> priceRequests = new HashSet<>();
        ConverterUtil.getPriceRequest(priceRequests, request.getSalePrice());
        itemRequest.setPrice(priceRequests);
        wholesaleValidationUtil.validateWholesaleConfigOnUpdate(itemPickupPointListingResponse.getCategoryCode(),
            productItemWholesalePriceRequests, itemRequest, minimumPrice, L5Code, request.getWholesalePriceActivated(),
            wholesaleMappingResponse);
        request.setWholesalePriceActivated(itemRequest.getWholesalePriceActivated());
        if (Boolean.FALSE.equals(itemRequest.getWholesalePriceActivated())) {
          failedRequests.add(new VariantsErrorListResponse(request.getItemSku(),
              productVariantPriceStockAndImagesRequest.getItemName(),
              ApiErrorCode.WHOLESALE_VALIDATION_FAILED.getCode(), ApiErrorCode.WHOLESALE_VALIDATION_FAILED.getDesc()));
        }
        }
      }
    }
  }

  @Override
  public Boolean isSameThreshold(ProductPriceStockAndImagesRequest request, String categoryCode, String itemSku,
      WholesalePriceSkuResponse wholesalePriceSkuResponse, WholesaleMappingResponse wholesaleMappingResponse) {
    Boolean response = null;
    if (Objects.isNull(wholesaleMappingResponse)) {
      wholesaleMappingResponse = pcbFeign
          .getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode)
          .getValue();
    }
    if (Objects.nonNull(wholesaleMappingResponse)) {
      if (!wholesaleMappingResponse.isWholesalePriceConfigEnabled()) {
        response = request.getWholesalePriceActivated();
      } else {
        try {
          if (Objects.nonNull(wholesalePriceSkuResponse) && Objects
              .nonNull(wholesalePriceSkuResponse.getWholesaleRules())) {
            WholesalePriceSkuResponse finalWholesalePriceSkuResponse = wholesalePriceSkuResponse;
            List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
                wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
                    key -> new ProductItemWholesalePriceRequest(key,
                        finalWholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList());
            CommonUtils.validateWholesalePrice(productItemWholesalePriceRequests, request.getPrices().get(0).getPrice(),
                getMinimumPrice(), maxWholesalePriceRequests, itemSku);
            CommonUtils
                .validateWholesalePriceConfig(productItemWholesalePriceRequests, request.getPrices().get(0).getPrice(),
                    wholesaleMappingResponse);
          }
          response = true;
        } catch (Exception e) {
          LOGGER.error(String.format(ErrorMessages.WHOLE_SALE_VALIDATION_FAILED, itemSku), e);
          response = false;
        }
      }
    }
    return response;
  }

  @Override
  public Boolean isSameThresholdL5(ItemPickupPointRequest request, String categoryCode, String itemSku,
      WholesalePriceSkuResponse wholesalePriceSkuResponse) {
    Boolean response = null;
    WholesaleMappingResponse wholesaleMappingResponse = pcbFeign
        .getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, categoryCode)
        .getValue();
    if (Objects.nonNull(wholesaleMappingResponse)) {
      if (!wholesaleMappingResponse.isWholesalePriceConfigEnabled()) {
        response = request.getWholesalePriceActivated();
      } else {
        try {
          if (Objects.nonNull(wholesalePriceSkuResponse) && Objects
              .nonNull(wholesalePriceSkuResponse.getWholesaleRules())) {
            WholesalePriceSkuResponse finalWholesalePriceSkuResponse = wholesalePriceSkuResponse;
            List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
                wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
                    key -> new ProductItemWholesalePriceRequest(key,
                        finalWholesalePriceSkuResponse.getWholesaleRules().get(key))).collect(Collectors.toList());
            CommonUtils.validateWholesalePrice(productItemWholesalePriceRequests, request.getPrice(),
                getMinimumPrice(), maxWholesalePriceRequests, itemSku);
            CommonUtils
                .validateWholesalePriceConfig(productItemWholesalePriceRequests, request.getPrice(),
                    wholesaleMappingResponse);
          }
          response = true;
        } catch (Exception e) {
          LOGGER.error(String.format(ErrorMessages.WHOLE_SALE_VALIDATION_FAILED, itemSku), e);
          response = false;
        }
      }
    }
    return response;
  }

  @Override
  public List<ProductPriceStockAndImagesRequest> validateListOfVariantsWithSuccess(
      List<ProductPriceStockAndImagesRequest> requests, List<VariantsErrorListResponse> failedRequests,
      Map<String, ItemSummaryResponse> itemSummaryResponseMap)
      throws Exception {
    List<ProductPriceStockAndImagesRequest> successRequests = new ArrayList<>();
    validateListOfVariants(requests, successRequests, failedRequests, itemSummaryResponseMap);
    return successRequests;
  }

  @Override
  public void validateL5UpdateRequest(ProductVariantUpdateRequest productVariantUpdateRequest,
      SimpleStringResponse errorCode, ProfileResponse profileResponse) throws Exception {
    boolean mppEnabled = false;
    if(Boolean.TRUE.equals(productVariantUpdateRequest.isFreeSample())){
      validateStatusAndCNCUpdateAtL5(productVariantUpdateRequest, errorCode);
    }
    if (Objects.isNull(profileResponse)) {
      profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(
          productVariantUpdateRequest.getBusinessPartnerCode());
    }
      mppEnabled = Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && (
          (profileResponse.getCompany().isCncActivated()) || (
              Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) && mppAllowedSellers
                  .contains(profileResponse.getCompany().getMerchantType())));
    if (!mppEnabled) {
      if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints()) || CollectionUtils
          .isNotEmpty(productVariantUpdateRequest.getDeletePickupPoints())) {
        errorCode.setResult(ApiErrorCode.OPERATION_NOT_PERMITTED.getCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.OPERATION_NOT_PERMITTED.getDesc());
      }
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest
          .getProductItems()) {
        errorCode.setResult(ApiErrorCode.OPERATION_NOT_PERMITTED.getCode());
        if (productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints().size() > 1) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.OPERATION_NOT_PERMITTED.getDesc());
        }
      }
    }
  }

  @Override
  public void validateStatusAndCNCUpdateAtL5(
    ProductVariantUpdateRequest productVariantUpdateRequest,
    SimpleStringResponse errorCode) {
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getAddPickupPoints())) {
      if (productVariantUpdateRequest.getAddPickupPoints().stream()
        .anyMatch(addL5Request -> addL5Request.isDisplay() || addL5Request.isBuyable())) {
        errorCode.setResult(ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getDesc());
      }
      if(productVariantUpdateRequest.getAddPickupPoints().stream().anyMatch(ItemPickupPointRequest::isCncActive)){
        errorCode.setResult(ApiErrorCode.FREE_SAMPLE_CNC_UPDATE_ERROR.getCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getDesc());
      }
      validatePickupPointsWithSchedules(productVariantUpdateRequest.getAddPickupPoints(),
        schedulesAddEditEnabled, ApiErrorCode.SCHEDULE_NOT_ALLOWED_FOR_FREE_SAMPLE);
    }
    if (CollectionUtils.isNotEmpty(productVariantUpdateRequest.getProductItems())) {
      for (ProductVariantPriceStockAndImagesRequest updateRequest : productVariantUpdateRequest.getProductItems()) {
        if (CollectionUtils.isNotEmpty(updateRequest.getModifiedItemPickupPoints())) {
          if (updateRequest.getModifiedItemPickupPoints().stream().anyMatch(
            (itemPickupPointRequest) -> itemPickupPointRequest.isDisplay()
              || itemPickupPointRequest.isBuyable())) {
            errorCode.setResult(ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getCode());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getDesc());
          }
          if (!cncForWarehouseFeatureSwitch && updateRequest.getModifiedItemPickupPoints().stream()
              .anyMatch(ItemPickupPointRequest::isCncActive)) {
            errorCode.setResult(ApiErrorCode.FREE_SAMPLE_CNC_UPDATE_ERROR.getCode());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getDesc());
          }
          if (cncForWarehouseFeatureSwitch && updateRequest.getModifiedItemPickupPoints().stream().anyMatch(
              (itemPickupPointRequest) -> itemPickupPointRequest.isCncDisplay()
                  || itemPickupPointRequest.isCncBuyable())) {
            errorCode.setResult(ApiErrorCode.FREE_SAMPLE_CNC_UPDATE_ERROR.getCode());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR.getDesc());
          }
          validatePickupPointsWithSchedules(updateRequest.getModifiedItemPickupPoints(),
            schedulesAddEditEnabled, ApiErrorCode.FREE_SAMPLE_STATUS_CHANGE_ERROR);
        }
      }
    }
  }

  @Override
  public boolean validateL5UpdateRequestWithErrorCode(ProductVariantUpdateRequest productVariantUpdateRequest)
      throws Exception {
    try {
      validateL5UpdateRequest(productVariantUpdateRequest, new SimpleStringResponse(), null);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      if (applicationRuntimeException.getErrorMessage().contains(ApiErrorCode.OPERATION_NOT_PERMITTED.getDesc())) {
        return false;
      } else {
        throw applicationRuntimeException;
      }
    } catch (Exception e) {
      throw e;
    }
    return true;
  }

  private void validatePickupPointsWithSchedules(List<ItemPickupPointRequest> pickupPoints,
    boolean schedulesAddEditEnabled, ApiErrorCode errorCode) {
    Optional<ItemPickupPointRequest> invalidPickupPoint = Optional.empty();
    for (ItemPickupPointRequest pickupPoint : pickupPoints) {
      if (Objects.nonNull(pickupPoint.getDiscoverableSchedule()) || Objects.nonNull(
        pickupPoint.getBuyableSchedule())) {
        invalidPickupPoint = Optional.of(pickupPoint);
        break;
      }
    }

    if (invalidPickupPoint.isPresent() && schedulesAddEditEnabled) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, errorCode.getDesc());
    }
  }

  @Override
  public void validateListOfVariantsL5WithSuccess(
    List<ProductVariantPriceStockAndImagesRequest> requests,
    Map<String, ItemPickupPointListingResponse> itemSummaryResponseMap,
    Map<String, WholesalePriceSkuResponse> wholesalePriceSkuResponseMap,
    List<VariantsErrorListResponse> failedRequests, SimpleStringResponse errorCode,
    boolean multiPickupPointEnabledForSeller, WholesaleMappingResponse wholesaleMappingResponse)
    throws Exception {
    validateListOfVariantsL5(requests, itemSummaryResponseMap, wholesalePriceSkuResponseMap,
      failedRequests, errorCode, multiPickupPointEnabledForSeller, wholesaleMappingResponse);
  }

  @Override
  public void validateNewL5AdditionRequests(ProductVariantUpdateRequest productVariantUpdateRequest,
      SimpleStringResponse errorCode, ProfileResponse profileResponse,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests,
      Map<String, ItemPickupPointListingResponse> savedItemPickupPointDataMap) throws Exception {
    Set<String> uniquePickupPointCodes =
        validatePriceAndMerchantSkuForNewlyAddedItems(productVariantUpdateRequest, errorCode,
            newlyAddedProductItemRequests, null);
    Set<String> uniqueAddPickupPointCodes = new HashSet<>();
    if (setWaitingDeletionForDeletePickupPoint) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
        uniqueAddPickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
      }
    }
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantUpdateRequest.getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        uniquePickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
      }
    }
    List<PickupPointResponse> pickupPointResponseList = pickupPointOutbound
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID, new ArrayList<>(uniquePickupPointCodes));
    Map<String, PickupPointResponse> pickupPointMap = pickupPointResponseList.stream()
        .collect(Collectors.toMap(PickupPointResponse::getCode, Function.identity(), (a, b) -> a));
    List<String> fbbActivatedPPCodes = new ArrayList<>();
    if (!uniquePickupPointCodes.isEmpty()) {
      CommonUtils.validatePickupPoints(productVariantUpdateRequest.getProductSku(), uniquePickupPointCodes,
          pickupPointResponseList, errorCode, profileResponse.getBusinessPartnerCode());
      validatePickupPointWaitingDeletion(productVariantUpdateRequest.getProductSku(), uniqueAddPickupPointCodes,
          pickupPointResponseList);
      fbbActivatedPPCodes =
          pickupPointResponseList.stream().filter(PickupPointResponse::isFbbActivated).map(PickupPointResponse::getCode)
              .collect(Collectors.toList());
    }
    List<String> itemSkusPresent = new ArrayList<>();
    if (validateItemSkuInAddL5Requests) {
      itemSkusPresent = savedItemPickupPointDataMap.values().stream().map(ItemPickupPointListingResponse::getItemSku)
          .collect(Collectors.toList());
    }
    for (ItemPickupPointRequest pickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      setFbbAndSyncStockFlag(profileResponse, pickupPointRequest, fbbActivatedPPCodes);
      checkIfItemSkuInAddRequestIsValid(pickupPointRequest, itemSkusPresent);
      distributionValidationOnlyForL5UpdateAPI(productVariantUpdateRequest, pickupPointRequest, pickupPointMap);
    }
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
      for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
        setFbbAndSyncStockFlag(profileResponse, itemPickupPointRequest, fbbActivatedPPCodes);
      }
    }
  }

  private void distributionValidationOnlyForL5UpdateAPI(ProductVariantUpdateRequest productVariantUpdateRequest,
      ItemPickupPointRequest pickupPointRequest, Map<String, PickupPointResponse> pickupPointMap) {
    if (ranchIntegrationEnabled && productVariantUpdateRequest.isOnlyL5Update()) {
      PickupPointResponse pickupPointResponse = pickupPointMap.get(pickupPointRequest.getPickupPointId());
      if (Boolean.TRUE.equals(Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
          .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))) {
        if (!relaxDistributionL5AdditionFromBulk) {
          log.error("Distribution L5 was being added from bulk for product : {} , itemSku : {} , pickupPointCode : {}",
              productVariantUpdateRequest.getProductSku(), pickupPointRequest.getItemSku(),
              pickupPointRequest.getPickupPointId());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.DISTRIBUTION_L5_CANNOT_BE_ADDED_FROM_THIS_FLOW.getDesc());
        } else {
          // This will only be used during migration to add L5s without UOM info
          pickupPointRequest.setDistribution(true);
        }
      }
    }
  }

  private void checkIfItemSkuInAddRequestIsValid(ItemPickupPointRequest pickupPointRequest, List<String> itemSkusPresent) {
    if (validateItemSkuInAddL5Requests) {
      if (!itemSkusPresent.contains(pickupPointRequest.getItemSku())) {
        log.error("ItemSku : {} invalid for add request : {} , valid itemSkus : {}", pickupPointRequest.getItemSku(),
            pickupPointRequest, itemSkusPresent);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.INVALID_ADD_L5_REQUEST.getDesc());
      }
    }
  }

  private Set<String> validatePriceAndMerchantSkuForNewlyAddedItems(ProductVariantUpdateRequest productVariantUpdateRequest,
      SimpleStringResponse errorCode, List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests,
      Integer minimumPrice) {
    if (Objects.isNull(minimumPrice)) {
      minimumPrice = getMinimumPrice();
    }
    Set<String> uniquePickupPointCodes = new HashSet<>();

    if (validateNewlyAddedItems) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
        for (ItemPickupPointRequest itemPickupPointRequest : productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()) {
          uniquePickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
          validatePriceRequest(minimumPrice, itemPickupPointRequest, errorCode);
          validateSellerSku(itemPickupPointRequest.getItemSku(), itemPickupPointRequest, errorCode);
        }
      }
    }

    ProductContentUtil.validateAttributeMapInProductItemRequest(validateAttributeMapForNewlyAddedItems,
        newlyAddedProductItemRequests);

    for (ItemPickupPointRequest itemPickupPointRequest : productVariantUpdateRequest.getAddPickupPoints()) {
      uniquePickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
      validatePriceRequest(minimumPrice, itemPickupPointRequest, errorCode);
      validateSellerSku(itemPickupPointRequest.getItemSku(), itemPickupPointRequest, errorCode);
    }
    return uniquePickupPointCodes;
  }

  @Override
  public void pickupPointCodeValidation(ProductL3UpdateRequest l3UpdateRequest) throws Exception {
    Set<String> uniquePickupPointCodes = new HashSet<>();
    Set<String> uniqueAddPickupPointCodes = new HashSet<>();

    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : l3UpdateRequest.getProductItems()) {
      for (ItemPickupPointRequest itemPickupPointRequest : Optional.ofNullable(
          productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints()).orElse(new ArrayList<>())) {
        uniquePickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
      }
    }

    for (ItemPickupPointRequest itemPickupPointRequest : Optional.ofNullable(l3UpdateRequest.getAddPickupPoints())
        .orElse(new ArrayList<>())) {
      itemPickupPointRequest.setDistribution(false);
      uniquePickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
      uniqueAddPickupPointCodes.add(itemPickupPointRequest.getPickupPointId());
    }

    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    if (!uniquePickupPointCodes.isEmpty()) {
      pickupPointResponseList =
          pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
              new ArrayList<>(uniquePickupPointCodes));
      CommonUtils.validatePickupPoints(l3UpdateRequest.getProductSku(), uniquePickupPointCodes, pickupPointResponseList,
          new SimpleStringResponse(), l3UpdateRequest.getBusinessPartnerCode());
      validatePickupPointWaitingDeletion(l3UpdateRequest.getProductSku(), uniqueAddPickupPointCodes, pickupPointResponseList);
    }

    if (ranchIntegrationEnabled) {
      setDistributionFlagBasedOnPPCode(l3UpdateRequest, pickupPointResponseList);
    }
  }

  private static void setDistributionFlagBasedOnPPCode(ProductL3UpdateRequest l3UpdateRequest,
      List<PickupPointResponse> pickupPointResponseList) {
    List<String> distributionTruePickupPointCodes =
        Optional.ofNullable(pickupPointResponseList).orElse(new ArrayList<>()).stream().filter(
            pickupPointResponse -> Boolean.TRUE.equals(
                Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
                    .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))).map(PickupPointResponse::getCode).toList();
    for (ItemPickupPointRequest itemPickupPointRequest : Optional.ofNullable(l3UpdateRequest.getAddPickupPoints())
        .orElse(new ArrayList<>())) {
      overrideFlagsForDistributionPp(itemPickupPointRequest, distributionTruePickupPointCodes);
    }
    for (ProductVariantPriceStockAndImagesRequest variantPriceStockAndImagesRequest : Optional.ofNullable(
        l3UpdateRequest.getProductItems()).orElse(new ArrayList<>())) {
      for (ItemPickupPointRequest itemPickupPointRequest : Optional.ofNullable(
          variantPriceStockAndImagesRequest.getModifiedItemPickupPoints()).orElse(new ArrayList<>())) {
        overrideFlagsForDistributionPp(itemPickupPointRequest, distributionTruePickupPointCodes);
      }
    }
    if (CollectionUtils.isNotEmpty(distributionTruePickupPointCodes)) {
      CommonUtils.validateIfDistributionInfoIsMissing(l3UpdateRequest.getDistributionAndUOMRequest(),
          l3UpdateRequest.getDistributionInfoRequest(), l3UpdateRequest.getProductCode());
    }
  }

  private static void overrideFlagsForDistributionPp(ItemPickupPointRequest itemPickupPointRequest,
      List<String> distributionTruePickupPointCodes) {
    if (distributionTruePickupPointCodes.contains(itemPickupPointRequest.getPickupPointId())) {
      itemPickupPointRequest.setDistribution(true);
      itemPickupPointRequest.setBuyable(false);
      itemPickupPointRequest.setDisplay(false);
      itemPickupPointRequest.setCncBuyable(false);
      itemPickupPointRequest.setCncDisplay(false);
    }
  }

  private void validatePickupPointWaitingDeletion(String productSku, Set<String> uniqueAddPickupPointCodes,
      List<PickupPointResponse> pickupPointResponseList) {
    if (setWaitingDeletionForDeletePickupPoint && CollectionUtils.isNotEmpty(uniqueAddPickupPointCodes)) {
      List<PickupPointResponse> addPickupPointResponseList = pickupPointResponseList.stream()
          .filter(pickupPointResponse -> uniqueAddPickupPointCodes.contains(pickupPointResponse.getCode()))
          .collect(Collectors.toList());
      if (addPickupPointResponseList.stream().anyMatch(PickupPointResponse::isWaitingDeletion)) {
        log.error(
            "Product creation failed for product : {} , pickup point request : {} is not valid , it has been deleted",
            productSku, addPickupPointResponseList);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getDesc());
      }
    }
  }

  private void setFbbAndSyncStockFlag(ProfileResponse profileResponse, ItemPickupPointRequest itemPickupPointRequest,
      List<String> fbbActivatedPPCodes) throws Exception {
    if (fbbActivatedPPCodes.contains(itemPickupPointRequest.getPickupPointId())) {
      itemPickupPointRequest.setFbbActive(true);
      if (!CommonUtils.isFaasEligibleSeller(faasFeatureSwitch, profileResponse)) {
        itemPickupPointRequest.setSynchronizeStock(
            productInventoryService.isSyncedToLevel1Inventory(profileResponse) || inventoryL5SyncStockEnabled);
      }
    } else {
      if (!inventoryL5SyncStockEnabled) {
        itemPickupPointRequest.setSynchronizeStock(
            productInventoryService.isSyncedToLevel1Inventory(profileResponse));
      }
    }
  }

  private void validatePriceRequest(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, Integer minimumPrice) {
    for (ProductLevel3PriceRequest price : request.getPrices()) {
      if (Objects.isNull(price.getPrice()) || Objects.isNull(price.getSalePrice()) || price.getPrice() < minimumPrice
          || price.getSalePrice() < minimumPrice || StringUtils.isEmpty(price.getChannelId())) {
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), ApiErrorCode.PRICE_UPDATE_FAILED.getDesc()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
      }
    }
  }

  private void validatePriceRequestL5(ProductVariantPriceStockAndImagesRequest request, Integer minimumPrice,
      SimpleStringResponse errorCode) {
    for (ItemPickupPointRequest itemPickupPointRequest : request.getModifiedItemPickupPoints()) {
      validatePriceRequest(minimumPrice, itemPickupPointRequest, errorCode);
      validateBasePriceForBFB(minimumPrice, itemPickupPointRequest, errorCode);
    }
  }

  private void validateBasePriceForBFB(Integer minimumPrice, ItemPickupPointRequest itemPickupPointRequest, SimpleStringResponse errorCode) {
    if (Optional.of(itemPickupPointRequest).map(ItemPickupPointRequest::getB2bFields).map(
      B2BFields::getPrice).isPresent()){
      if(Double.compare(itemPickupPointRequest.getB2bFields().getPrice(), minimumPrice) == -1){
        errorCode.setResult(ApiErrorCode.B2B_INVALID_BASE_PRICE.getCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.B2B_INVALID_BASE_PRICE.getDesc() + "for Item " + itemPickupPointRequest.getItemSku() + "-"
            + itemPickupPointRequest.getPickupPointId());
      }
    }
  }

  private void validatePriceRequestL5New(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, Integer minimumPrice,
      ItemPickupPointRequest itemPickupPointRequest) throws ApplicationRuntimeException {
    if (Objects.isNull(itemPickupPointRequest.getPrice()) || Objects.isNull(itemPickupPointRequest.getSalePrice())
        || itemPickupPointRequest.getPrice() < minimumPrice || itemPickupPointRequest.getSalePrice() < minimumPrice) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.PRICE_UPDATE_FAILED.getCode(), ApiErrorCode.PRICE_UPDATE_FAILED.getDesc(),
          itemPickupPointRequest.getPickupPointId()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
    }
  }

  private void validateCncAndDeliveryFlag(ProductVariantPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, ItemPickupPointRequest itemPickupPointRequest,
      BusinessPartnerPickupPointResponse pickupPointResponse) {
    boolean defaultStatus =
        itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay();
    boolean cncStatus =
        itemPickupPointRequest.isCncBuyable() || itemPickupPointRequest.isCncDisplay();
    if (Objects.isNull(pickupPointResponse)) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getCode(), ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getDesc(),
          itemPickupPointRequest.getPickupPointId()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PICKUP_POINT_IS_NOT_VALID.getDesc());
    }
    if (defaultStatus && !pickupPointResponse.isDelivery()) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.PICKUP_POINT_NOT_DELIVERY_ACTIVE.getCode(), ApiErrorCode.PICKUP_POINT_NOT_DELIVERY_ACTIVE.getDesc(),
          itemPickupPointRequest.getPickupPointId()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PICKUP_POINT_NOT_DELIVERY_ACTIVE.getDesc());
    }
    if (cncStatus && !pickupPointResponse.isCncActivated()) {
      failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
          ApiErrorCode.PICKUP_POINT_NOT_CNC_ACTIVE.getCode(), ApiErrorCode.PICKUP_POINT_NOT_CNC_ACTIVE.getDesc(),
          itemPickupPointRequest.getPickupPointId()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PICKUP_POINT_NOT_CNC_ACTIVE.getDesc());
    }
  }

  private void validatePriceRequest(Integer minimumPrice, ItemPickupPointRequest itemPickupPointRequest,
      SimpleStringResponse errorCode) {
    if (Objects.isNull(itemPickupPointRequest.getPrice()) || Objects.isNull(itemPickupPointRequest.getSalePrice())
        || itemPickupPointRequest.getPrice() < minimumPrice || itemPickupPointRequest.getSalePrice() < minimumPrice
        || itemPickupPointRequest.getSalePrice() > itemPickupPointRequest.getPrice()) {
      errorCode.setResult(ApiErrorCode.PRICE_UPDATE_FAILED.getCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.PRICE_UPDATE_FAILED.getDesc() + " for Item " + itemPickupPointRequest.getItemSku() + "-"
              + itemPickupPointRequest.getPickupPointId());
    }
  }

  //image path is present in location or not? FS?
  //Added validation for image extension
  private void validateImageRequest(ProductPriceStockAndImagesRequest request,
      List<VariantsErrorListResponse> failedRequests, List<String> validExtension) {
    for (ProductLevel3SummaryDetailsImageRequest image : request.getImages()) {
      if (isImageValid(validExtension, image)) {
        failedRequests.add(new VariantsErrorListResponse(request.getItemSku(), request.getItemName(),
            ApiErrorCode.IMAGE_UPDATE_FAILED.getCode(), ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc()));
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc());
      }
    }
  }

  private void validateImageRequestL5(ProductVariantPriceStockAndImagesRequest request, List<String> validExtension,
      SimpleStringResponse errorCode) {
    for (ProductLevel3SummaryDetailsImageRequest image : request.getImages()) {
      if (Constants.NEW.equals(image.getReviewType())) {
        if (isImageValid(validExtension, image)) {
          errorCode.setResult(ApiErrorCode.IMAGE_UPDATE_FAILED.getCode());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.IMAGE_UPDATE_FAILED.getDesc() + "for Item " + request.getItemSku());
        }
      }
    }
  }

  public boolean isImageValid(List<String> validExtension,
    ProductLevel3SummaryDetailsImageRequest image) {
    return StringUtils.isEmpty(image.getLocationPath()) || Objects.nonNull(
      fileStorageService.checkImageAvailability(fileStorageService.getImageRequest(image), true))
      || Objects.isNull(image.getSequence()) || Objects.isNull(image.getMainImage())
      || !validateImageExtension(image.getLocationPath(), validExtension);
  }

  private boolean validateImageExtension(String imagePath, List<String> validExtension) {
    return validExtension
        .contains(imagePath.substring(imagePath.lastIndexOf(Constants.DOT, imagePath.length())).toLowerCase());
  }

  private Integer getMinimumPrice() {
    ProductSystemParameter minimumPrice =
        productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.MINIMUM_PRICE);
    return Integer.valueOf(minimumPrice.getValue());
  }

  private List<String> getvalidExtension() {
    ProductSystemParameter validExtension = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    return Arrays.asList(validExtension.getValue().split(Constants.COMMA));
  }

  @Override
  public ProductEditValidationDTO validateBundleProduct(ProductL3UpdateRequest l3UpdateRequest,
      ProductL3Response savedProductData, ProductEditValidationDTO productEditValidationDTO,
      ProfileResponse profileResponse) {
    if (!productBundlingEnabled || !l3UpdateRequest.isBundleProduct()) {
      l3UpdateRequest.setProductBundleRecipe(new ArrayList<>());
    } else if (CollectionUtils.isNotEmpty(l3UpdateRequest.getProductBundleRecipe()) && Arrays.asList(
            productBundlingEligibleMerchantTypes.split(Constants.COMMA))
        .contains(profileResponse.getCompany().getMerchantType())) {

      removeRequestsWithEmptyItemSkus(l3UpdateRequest);

      boolean anyVariantWithMoreThanMaxSkusInBundleRecipe =
          isAnyVariantWithMoreThanMaxSkusInBundleRecipe(l3UpdateRequest);
      if (anyVariantWithMoreThanMaxSkusInBundleRecipe) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_MAX_SKUS);
        return productEditValidationDTO;
      }

      boolean anyVariantWithoutProperItemSkuInBundleRecipe =
          isAnyVariantWithoutProperItemSkuInBundleRecipe(l3UpdateRequest);
      if (anyVariantWithoutProperItemSkuInBundleRecipe) {
        productEditValidationDTO.getEditProductResponse()
            .setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCTS_SHOULD_BE_AT_VARIANT_LEVEL);
        return productEditValidationDTO;
      }

      boolean anyVariantWithLessThanEqualToZeroQuantity = isAnyVariantWithLessThanEqualToZeroQuantity(l3UpdateRequest);
      if (anyVariantWithLessThanEqualToZeroQuantity) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_LESS_THAN_EQUAL_TO_ZERO_QUANTITY);
        return productEditValidationDTO;
      }

      boolean anyVariantWithParentSkuInRecipe = isAnyVariantWithParentSkuInRecipe(l3UpdateRequest);
      if (anyVariantWithParentSkuInRecipe) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_SKU_IS_SAME_AS_PARENT_SKU);
        return productEditValidationDTO;
      }

      List<String> childProductSkus =
          Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
              .map(ProductBundleRecipeRequest::getBundleRecipe).flatMap(Set::stream).map(
                  bundleRecipeVo -> bundleRecipeVo.getItemSku()
                      .substring(0, bundleRecipeVo.getItemSku().lastIndexOf(Constants.HYPHEN))).distinct()
              .collect(Collectors.toList());

      List<ProductBasicResponse> productBasicResponseList = xProductOutbound.getProductBasicDetails(childProductSkus);
      Map<String, ProductBasicResponse> productBasicResponseMap =
          Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(ProductBasicResponse::getProductSku, Function.identity(), (v1, v2) -> v2));
      for (String childProductSku : childProductSkus) {
        if (productBasicResponseMap.containsKey(childProductSku)) {
          ProductBasicResponse productBasicResponse = productBasicResponseMap.get(childProductSku);
          if (!productBasicResponse.isProductExists()) {
            productEditValidationDTO.getEditProductResponse()
                .setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCT_NOT_FOUND);
            return productEditValidationDTO;
          }
        } else {
          productEditValidationDTO.getEditProductResponse()
              .setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCT_NOT_FOUND);
          return productEditValidationDTO;
        }
      }

      if (MerchantType.TD.name().equals(profileResponse.getCompany().getMerchantType())) {
        // verify all child sku is trading product
        boolean anyChildSkuWhichIsNotTradingProduct = isAnyChildSkuWhichIsNotTradingProduct(productBasicResponseList);
        if (anyChildSkuWhichIsNotTradingProduct) {
          productEditValidationDTO.getEditProductResponse()
              .setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_IS_NOT_TRADING_PRODUCT);
          return productEditValidationDTO;
        }
      } else {
        boolean anyChildSkuWhichDontBelongToCurrentSeller =
            isAnyChildSkuWhichDontBelongToCurrentSeller(profileResponse, productBasicResponseList);
        // verify all child sku belong to current seller
        if (anyChildSkuWhichDontBelongToCurrentSeller) {
          productEditValidationDTO.getEditProductResponse()
              .setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_DOES_NOT_BELONG_TO_CURRENT_SELLER);
          return productEditValidationDTO;
        }
      }

      boolean anyChildSkuDeleted = validateIfChildSkusAreDeleted(l3UpdateRequest);
      if (anyChildSkuDeleted) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(ApiErrorCode.BUNDLE_RECIPE_CHILD_IS_DELETED);
        return productEditValidationDTO;
      }

      if (warehouseBomActivated) {
        productLevel3V2Service.editRequestToBillOfMaterialRecipeRequest(l3UpdateRequest, savedProductData);
      }
    } else {
      l3UpdateRequest.setProductBundleRecipe(new ArrayList<>());
      l3UpdateRequest.setBundleProduct(false);
    }
    return productEditValidationDTO;
  }

  @Override
  public void validateEligibilityForVariantAndL5Deletion(ProfileResponse profileResponse,
    ProductL3UpdateRequest productL3UpdateRequest, ProductLevel3 savedProductData) throws Exception {
    if (validateWarehouseDeletionEligibleSellers.contains(
      Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
        .map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY))
      && validateWarehouseVariantDeletionEnabled) {
      List<String> itemsInEligibleForDeletion = new ArrayList<>();
      List<String> deletedPickupPointCodes = productL3UpdateRequest.getDeletePickupPoints().stream()
        .map(PickupPointDeleteRequest::getPickupPointId).distinct().collect(Collectors.toList());
      List<String> variantEligibleForDeletionValidation =
        productL3UpdateRequest.getDeletedProductItems().stream()
          .map(DeletedProductItems::getItemSku).distinct().collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(deletedPickupPointCodes)) {
        // L5 validation
        validatePPDeletionRequest(profileResponse, productL3UpdateRequest, deletedPickupPointCodes,
          itemsInEligibleForDeletion);
      }
      // L2 validation
      if(CollectionUtils.isNotEmpty(variantEligibleForDeletionValidation)){
        validateVariantDeletionFromInventory(profileResponse, productL3UpdateRequest);
      }
    }


  }

  private void validateVariantDeletionFromInventory(ProfileResponse profileResponse,
    ProductL3UpdateRequest productL3UpdateRequest)
    throws Exception {
    List<String> deletedItemSkus = fetchDeletedL4s(productL3UpdateRequest);
    // get all PP's at L3 and validate if any FBB True PP exists for product
    if (CollectionUtils.isNotEmpty(deletedItemSkus)) {
      List<String> deletedItemCodes = fetchDeletedFbbItemCodes(productL3UpdateRequest, deletedItemSkus);
      List<L2StockAvailabilityDTO> inventoryStockAvailabilityByItemCodes = new ArrayList<>();
      for (String itemCode : deletedItemCodes) {
        // Fetch stock availability for each deleted item code
        L2StockAvailabilityDTO stockAvailabilityByWarehouseItemSku =
          inventoryOutbound.findStockAvailabilityByWarehouseItemSku(
            ProductLevel3InventoryUtil.generateMandatoryRequestParam(),
            profileResponse.getBusinessPartnerCode(), itemCode);
        if (Objects.nonNull(stockAvailabilityByWarehouseItemSku)) {
          inventoryStockAvailabilityByItemCodes.add(stockAvailabilityByWarehouseItemSku);
        }
      }
      if (CollectionUtils.isNotEmpty(inventoryStockAvailabilityByItemCodes)) {
        // for the deleted item codes validate if stock exists
        ValidationUtil.validateWareHouseStockAvailabilityForL2Deletion(profileResponse,
          inventoryStockAvailabilityByItemCodes, deletedItemCodes);
      }
    }
  }

  private static List<String> fetchDeletedFbbItemCodes(ProductL3UpdateRequest productL3UpdateRequest,
    List<String> deletedItemSkus) {
    return productL3UpdateRequest.getDeletedProductItems().stream()
      .filter(deletedProductItems -> deletedItemSkus.contains(deletedProductItems.getItemSku()))
      .map(DeletedProductItems::getItemCode).distinct().collect(Collectors.toList());
  }

  private List<String> fetchDeletedL4s(ProductL3UpdateRequest request) {
    List<String> itemSkus =
      request.getDeletedProductItems().stream().map(DeletedProductItems::getItemSku)
        .filter(Objects::nonNull).collect(Collectors.toList());
    List<String> deletedFbbActiveItemSkus = new ArrayList<>();
    for (List<String> batchSkus : Lists.partition(itemSkus, inventoryBatchUpdateDeleteBatchSize)) {
      int currentPage = 0;
      boolean hasNextPage;
      do {
        try {
          GdnRestListResponse<ItemPickupPointL5Response> response =
            xProductOutbound.fetchL5ResponsesByItemSkus(batchSkus, currentPage, l5InventoryFetchBatchSize);
          if (Objects.isNull(response) || !response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
            log.error("No FBB L5 found for deleted items: {}", batchSkus);
            break;
          }
          response.getContent().stream().map(ItemPickupPointL5Response::getItemSku)
            .filter(Objects::nonNull).forEach(deletedFbbActiveItemSkus::add);
          hasNextPage = ResponseHelper.hasNextPage(response);
          currentPage++;

        } catch (ApplicationRuntimeException e) {
          log.error("Error fetching L5 responses for variant delete validation for items {}. Skipping current batch. Error: {}",
            batchSkus, e.getMessage(), e);
          break;
        }

      } while (hasNextPage);
    }
    return deletedFbbActiveItemSkus;
  }



  private void validatePPDeletionRequest(ProfileResponse profileResponse,
    ProductL3UpdateRequest productL3UpdateRequest, List<String> deletedPickupPointCodes,
    List<String> itemsInEligibleForDeletion) throws Exception {
    List<PickupPointDeleteRequest> ppDeleteForInventoryValidation = new ArrayList<>();
    List<InventoryDetailInfoResponseV2DTO> inventoryResponses = new ArrayList<>();
    GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryDetailInfoResponse;
    // fetch PP's for which we need to call inventory
    fetchPPEligibleForInventoryValidation(productL3UpdateRequest, deletedPickupPointCodes,
      ppDeleteForInventoryValidation);
    List<List<PickupPointDeleteRequest>> partitionPickupPointInventoryRequests =
      Lists.partition(ppDeleteForInventoryValidation, l5InventoryFetchBatchSize);
    for (List<PickupPointDeleteRequest> pickupPointDeleteRequests : partitionPickupPointInventoryRequests) {
      inventoryDetailInfoResponse = inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(
        ProductLevel3InventoryUtil.generateMandatoryRequestParam(),
        RequestHelper.toInventoryDetailInfoRequestDTO(pickupPointDeleteRequests,
          profileResponse.getBusinessPartnerCode()));
      inventoryResponses.addAll(
        Optional.ofNullable(inventoryDetailInfoResponse).map(GdnRestListResponse::getContent)
          .orElse(Collections.emptyList()));
    }
    if (CollectionUtils.isNotEmpty(inventoryResponses)) {
      // call inventory for unique PP and Item Sku requests
      ValidationUtil.validateWareHouseStockAvailabilityForPPDeletion(profileResponse,
        itemsInEligibleForDeletion, inventoryResponses, partitionPickupPointInventoryRequests);
    }
  }


  private void fetchPPEligibleForInventoryValidation(ProductL3UpdateRequest productL3UpdateRequest,
    List<String> deletedPickupPointCodes,
    List<PickupPointDeleteRequest> ppDeleteForInventoryValidation) throws Exception {
    if(CollectionUtils.isNotEmpty(deletedPickupPointCodes)){
      List<String> fbbActivatedDeletedPPCodes =
        fetchFbbActivePickupPoints(deletedPickupPointCodes);
      for (PickupPointDeleteRequest pickupPointDeleteRequest : productL3UpdateRequest.getDeletePickupPoints()) {
      // Add only FBB active deleted PP as eligible for inventory call
      if (fbbActivatedDeletedPPCodes.contains(pickupPointDeleteRequest.getPickupPointId())) {
        ppDeleteForInventoryValidation.add(pickupPointDeleteRequest);
      }
    }
    }
  }

  private List<String> fetchFbbActivePickupPoints(List<String> deletedPickupPointCodes) throws Exception {
    if(CollectionUtils.isNotEmpty(deletedPickupPointCodes)) {
      List<PickupPointResponse> pickupPointResponses =
        pickupPointOutbound.getByPickupPointCodes(Constants.PBP,
          deletedPickupPointCodes);
      return pickupPointResponses.stream().filter(PickupPointResponse::isFbbActivated)
        .map(PickupPointResponse::getCode).distinct().collect(Collectors.toList());
    }
    return Collections.emptyList();
  }


  private boolean validateIfChildSkusAreDeleted(ProductL3UpdateRequest l3UpdateRequest) {
    List<String> childItemSkus =
        Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
            .map(ProductBundleRecipeRequest::getBundleRecipe).flatMap(Set::stream).map(ProductBundleRecipe::getItemSku)
            .distinct().collect(Collectors.toList());
    List<ItemBasicDetailV2Response> itemBasicDetailV2Responses =
        xProductOutbound.getItemBasicDetailV2Response(childItemSkus, false);
    boolean isAnyItemDeleted = itemBasicDetailV2Responses.stream().anyMatch(ItemBasicDetailV2Response::isMarkForDelete);
    return isAnyItemDeleted || childItemSkus.size() != itemBasicDetailV2Responses.size();
  }

  private static boolean isAnyChildSkuWhichDontBelongToCurrentSeller(ProfileResponse profileResponse,
      List<ProductBasicResponse> productBasicResponseList) {
    return Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream().map(
            productBasicResponse -> productBasicResponse.getProductSku()
                .substring(0, productBasicResponse.getProductSku().lastIndexOf(Constants.HYPHEN)))
        .anyMatch(merchantCode -> !profileResponse.getBusinessPartnerCode().equals(merchantCode));
  }

  private static boolean isAnyChildSkuWhichIsNotTradingProduct(List<ProductBasicResponse> productBasicResponseList) {
    return Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream()
        .anyMatch(Predicate.not(ProductBasicResponse::isTradingProduct));
  }

  private static boolean isAnyVariantWithoutProperItemSkuInBundleRecipe(ProductL3UpdateRequest l3UpdateRequest) {
    return Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
        .anyMatch(bundleRecipeRequest -> !RequestHelper.isItemSku(bundleRecipeRequest.getItemSku()));
  }

  private boolean isAnyVariantWithMoreThanMaxSkusInBundleRecipe(ProductL3UpdateRequest l3UpdateRequest) {
    return Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
        .map(ProductBundleRecipeRequest::getBundleRecipe)
        .anyMatch(productBundleRecipes -> productBundleRecipes.size() > productBundlingMaxNumberOfSkus);
  }

  private boolean isAnyVariantWithLessThanEqualToZeroQuantity(ProductL3UpdateRequest l3UpdateRequest) {
    return Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
        .map(ProductBundleRecipeRequest::getBundleRecipe).flatMap(Set::stream)
        .anyMatch(bundleRecipeRequest -> bundleRecipeRequest.getQuantity() <= 0);
  }

  private boolean isAnyVariantWithParentSkuInRecipe(ProductL3UpdateRequest l3UpdateRequest) {
    return Optional.ofNullable(l3UpdateRequest.getProductBundleRecipe()).orElse(new ArrayList<>()).stream()
        .map(ProductBundleRecipeRequest::getBundleRecipe).flatMap(Set::stream).anyMatch(
            bundleRecipeRequest -> Optional.ofNullable(bundleRecipeRequest.getItemSku()).orElse(StringUtils.EMPTY)
                .contains(l3UpdateRequest.getProductSku()));
  }

  private static void removeRequestsWithEmptyItemSkus(ProductL3UpdateRequest l3UpdateRequest) {
    l3UpdateRequest.getProductBundleRecipe().removeIf(
        bundleRecipeRequest -> StringUtils.isBlank(bundleRecipeRequest.getItemSku()) || CollectionUtils.isEmpty(
            bundleRecipeRequest.getBundleRecipe()));
  }

}
