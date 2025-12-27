package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_STORE_ID;
import static com.gdn.partners.pbp.commons.constants.Constants.NEW;
import static com.gdn.partners.pbp.commons.constants.Constants.ORDER_ITEM_STATUS_OFF;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.OrderCancellationDto;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.ValueTypeUtil;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.partners.pbp.commons.constants.Constants;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.collections4.Predicate;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductLevel3V2ServiceWrapperBean implements ProductLevel3V2Wrapper {

  @Autowired
  private ProductLevel3V2Service productLevel3V2Service;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private VariantEditValidationService variantEditValidationService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductLevel3Helper productLevel3Helper;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Value("${generate.item.name.enabled}")
  private boolean generateItemNameEnabled;

  @Value("${family.color.attribute.code}")
  private String familyColorAttributeCode;

  @Value("${combined.edit.flow.enabled}")
  private boolean combinedEditFlowEnabled;

  @Value("${auto.fill.family.colour.attribute}")
  private boolean autoFillFamilyColourAttribute;

  @Value("${override.item.name.generation}")
  private boolean overrideItemNameGeneration;

  @Value("${validate.logistic.error.before.l5.update}")
  private boolean validateLogisticErrorBeforeL5Update;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${product.name.character.limit}")
  private int productNameCharacterLimit;

  @Value("${max.product.dimension.limit}")
  private int maxProductDimensionLimit;

  @Value("${update.instore.flag.edit.request.based.on.seller}")
  private boolean updateInstoreFlagEditRequestBasedOnSeller;

  @Value("${set.default.b2c.activated}")
  private boolean setDefaultB2CActivated;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Override
  public void updateL4AndL5ToOfflineForOrderCancellation(OrderCancellationDto orderCancellationDto) throws Exception {
    argumentCheckForOrderCancellation(orderCancellationDto);
    if (ORDER_ITEM_STATUS_OFF.equalsIgnoreCase(orderCancellationDto.getOrderItemStatus())) {
      ProductLevel3QuickEditV2Request request = updatingSkuOfflineForOrderCancellation(orderCancellationDto);
      settingUserNameBasedOnCancellationActor(orderCancellationDto);
      ItemPriceStockQuickUpdateResponse response =
          productLevel3V2Service.quickEditPatching(orderCancellationDto.getStoreId(),
              orderCancellationDto.getProductGdnSku(), request);
      if (Objects.nonNull(response.getApiErrorCode()) || CollectionUtils.isNotEmpty(response.getVariantsErrorList())) {
        log.error("While updating L4 and L5 for order cancellation an failure occurred. orderCancellationDto : "
            + "{} , itemPriceStockQuickUpdateResponse : {} ", orderCancellationDto, response);
      }
    }
  }

  @Override
  public ProductEditValidationDTO editProductDetails(String requestId,
      ProductL3UpdateRequest productL3UpdateWebRequest, boolean isOnlyExternal,
      boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate) throws Exception {
    ProductL3Response savedProductData = null;
    ProfileResponse profileResponse =
      businessPartnerRepository.filterDetailByBusinessPartnerCode(productL3UpdateWebRequest.getBusinessPartnerCode());
    if (updateInstoreFlagEditRequestBasedOnSeller) {
      updateOff2OnChannelActiveInRequest(productL3UpdateWebRequest, profileResponse);
    }
    if (setDefaultB2CActivated) {
      setDefaultB2CActivated(profileResponse, productL3UpdateWebRequest);
    }
    productL3UpdateWebRequest.setSellerOmg(CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.BLIBLI_OMG));
    ValueTypeUtil.separateValueTypeFromValue(productL3UpdateWebRequest,
        sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes);
    if (generateItemNameEnabled) {
      savedProductData = this.generateItemNameForNewlyAddedItem(productL3UpdateWebRequest);
    }
    productLevel3Helper.autoFillFamilyColourAttribute(productL3UpdateWebRequest);
    ProductLevel3 productLevel3EditRequest = productLevel3V2Service.generateProductLevel3(productL3UpdateWebRequest);
    EditProductResponse editProductResponse = new EditProductResponse();
    String storeId = mandatoryParameterHelper.getStoreId();
    ProductEditValidationDTO productEditValidationDTO;
    productEditValidationDTO =
        productLevel3V2Service.validationsForEdit(requestId, productL3UpdateWebRequest, productLevel3EditRequest,
            editProductResponse, savedProductData, profileResponse);
    if (productEditValidationDTO.isL5ValidationFailed() || Objects.nonNull(
      productEditValidationDTO.getEditProductResponse().getApiErrorCode())) {
      return productEditValidationDTO;
    }
    productL3UpdateWebRequest = productEditValidationDTO.getProductL3UpdateRequest();
    List<PickupPointDeleteRequest> pickupPointDeleteRequests;
    pickupPointDeleteRequests =
        productL3UpdateWebRequest.getDeletePickupPoints().stream().distinct().collect(Collectors.toList());
    boolean addingPickupPoints = CollectionUtils.isNotEmpty(productL3UpdateWebRequest.getAddPickupPoints());
    // Need revision update
    productEditValidationDTO = productLevel3V2Service.validationForProductL3ResponseAndNeedRevisionUpdate(
        productEditValidationDTO.getProductLevel3(), editProductResponse, profileResponse,
        combineContentAndLogisticsPcbUpdate, savedProductData, pickupPointDeleteRequests, productL3UpdateWebRequest,
        addingPickupPoints, isOnlyExternal);
    productEditValidationDTO.setProductL3UpdateRequest(productL3UpdateWebRequest);
    if (Objects.nonNull(productEditValidationDTO.getEditProductResponse().getApiErrorCode())) {
        return productEditValidationDTO;
      }
    List<AuditTrailDto> auditTrailDtoListForUomHistory =
        Optional.of(productEditValidationDTO.getEditProductResponse()).orElse(new EditProductResponse())
            .getAuditTrailDtoList();
    variantEditValidationService.validateEligibilityForVariantAndL5Deletion(profileResponse,
      productL3UpdateWebRequest, productLevel3EditRequest);
    variantEditValidationService.pickupPointCodeValidation(productL3UpdateWebRequest);
    productEditValidationDTO =
        variantEditValidationService.validateBundleProduct(productL3UpdateWebRequest, savedProductData,
            productEditValidationDTO, profileResponse);
    editProductResponse = productEditValidationDTO.getEditProductResponse();
    editProductResponse.setProfileResponse(profileResponse);
    if (Objects.nonNull(editProductResponse.getApiErrorCode())) {
      log.info("Exsting because of error, errorCode: {} ", editProductResponse.getApiErrorCode());
      return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build();
    }
    editProductResponse =
        processUpdateForEditFlow(isOnlyExternal, combineContentAndLogisticsPcbUpdate, combinePreOrderUpdate,
            editProductResponse, productEditValidationDTO, profileResponse, isNewImagesAdded(productL3UpdateWebRequest),
            productL3UpdateWebRequest);
        boolean eligibleForAutoReject = CommonUtils.isEligibleForAutoReject(editProductResponse);
        Optional.ofNullable(editProductResponse.getProductDetailEditDTO()).ifPresent(
          productDetailEditDTO -> productDetailEditDTO.setEligibleForAutoReject(
            eligibleForAutoReject));
        if (eligibleForAutoReject) {
          if (CommonUtils.isCombinedEditAndAutoReject(productEditValidationDTO.isNeedCorrection(),
              combinedEditFlowEnabled)) {
            productLevel3V2Service.updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(
              editProductResponse.getProductDetailEditDTO(), editProductResponse);
          }
          return performRestrictedKeywordAction(productL3UpdateWebRequest, storeId, editProductResponse);
        }
    if (CollectionUtils.isNotEmpty(auditTrailDtoListForUomHistory)) {
      productLevel3V2Service.publishProductLevelHistoryToPcbForDistributionUpdate(
          productL3UpdateWebRequest.getProductCode(), productL3UpdateWebRequest.getBusinessPartnerCode(),
          auditTrailDtoListForUomHistory);
    }
        ApiErrorCode logisticUpdateResponse =
          performL5AndLogisticUpdateAndCheckForTakeDown(productL3UpdateWebRequest, isOnlyExternal,
            combineContentAndLogisticsPcbUpdate, combinePreOrderUpdate, productLevel3EditRequest,
            editProductResponse, productEditValidationDTO, storeId);
        if (Objects.nonNull(logisticUpdateResponse)) {
          log.error("Error while updating the logistic info for productSku : {} and error code : {}",
            productLevel3EditRequest.getProductSku(), editProductResponse);
          editProductResponse.setApiErrorCode(logisticUpdateResponse);
          return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build();
        }
    return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build();
  }

  public static void updateOff2OnChannelActiveInRequest(ProductL3UpdateRequest productL3UpdateRequest,
      ProfileResponse profileResponse) {
    boolean sellerOfflineToOnlineFlag =
        Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(CompanyDTO::isOfflineToOnlineFlag)
            .orElse(false);
    if (!sellerOfflineToOnlineFlag) {
      productL3UpdateRequest.setOff2OnChannelActive(false);
    }
  }

  public static void setDefaultB2CActivated(ProfileResponse profileResponse,
      ProductL3UpdateRequest productL3UpdateRequest) {
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    if (salesChannel.size() == Constants.SIZE_ONE) {
      if (salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
        if (!Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
            .map(CompanyDTO::isOfflineToOnlineFlag).orElse(false)) {
          productL3UpdateRequest.setB2cActivated(true);
        }
      }
    }
  }

  @Override
  public ApiErrorCode editProductMasterData(String storeId, String requestId, String username,
    ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception {
    ApiErrorCode apiErrorCode;
    Pair<ApiErrorCode, MasterProductEditDTO> apiErrorCodeMasterProductEditDTOPair =
      performValidationOnMasterDataEdit(storeId, productMasterDataEditRequest);
    if (!ApiErrorCode.SUCCESS_VALIDATION.equals(apiErrorCodeMasterProductEditDTOPair.getKey())) {
      return apiErrorCodeMasterProductEditDTOPair.getKey();
    }
    MasterProductEditDTO masterProductEditDTO = apiErrorCodeMasterProductEditDTOPair.getValue();
    performRestrictedKeywordChecks(storeId, productMasterDataEditRequest, masterProductEditDTO);
    processAutoApproval(productMasterDataEditRequest, masterProductEditDTO);
    setReviewConfigOnContentChange(productMasterDataEditRequest, masterProductEditDTO);
    CommonUtils.processRestrictedKeywords(masterProductEditDTO, productMasterDataEditRequest);
    CommonUtils.handleAutoApprovalOnMasterDataEdit(masterProductEditDTO);
    // check if the product is eligible for take down
    CommonUtils.isEligibleForTakeDown(masterProductEditDTO, productMasterDataEditRequest);
    CommonUtils.sanitiseDimensionsForBopisProductType(productMasterDataEditRequest);
    apiErrorCode =
      productService.performMasterDataUpdate(productMasterDataEditRequest, masterProductEditDTO,
        storeId, requestId, username);
    if(masterProductEditDTO.isTakenDownProduct()){
      log.info("Product {} is taken down, not updating master data info in x-product",
        productMasterDataEditRequest.getProductCode());
      productLevel3Service.takeDownOrReactivateProduct(storeId,
        productMasterDataEditRequest.getProductSku(), true, null, null);
      productService.publishEditedEventToVendorAndPublishDimensionsRefreshEvent(
        productMasterDataEditRequest, masterProductEditDTO, storeId);
    }
    performRestrictedKeywordActionOnMasterDataEdit(storeId, productMasterDataEditRequest,
      masterProductEditDTO);
    updateCategoryForAutoCategoryChange(productMasterDataEditRequest, masterProductEditDTO);
    publishImageQCForContentChange(productMasterDataEditRequest, masterProductEditDTO);
    publishImageResizeEventIfRequired(productMasterDataEditRequest);
    return apiErrorCode;
  }

  @Override
  public void updateCategoryForAutoCategoryChange(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) throws Exception {
    if (masterProductEditDTO.isAutoCategoryChange()) {
      productServiceWrapper.updateCategoryInPcb(productMasterDataEditRequest.getProductCode(),
        masterProductEditDTO.getCategoryCode(), false,
        productMasterDataEditRequest.getBusinessPartnerCode());
    }
  }

  private void publishImageResizeEventIfRequired(
    ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception {
    if (isNewImageAddedForMasterProductUpdate(productMasterDataEditRequest)) {
      List<ImageRequest> imageRequestForResizeEvent =
        CommonUtils.getImageRequestForResizeEvent(productMasterDataEditRequest);
      updateImagePathsForNewlyAddedImages(imageRequestForResizeEvent);
      productPublisherService.publishEditImageResizeEvent(
        new EditedImageResizeEvent(productMasterDataEditRequest.getProductCode(), DEFAULT_STORE_ID,
          imageRequestForResizeEvent));
    }
  }

  private void updateImagePathsForNewlyAddedImages(List<ImageRequest> imageRequestForResizeEvent) {
    for (ImageRequest imageRequest : imageRequestForResizeEvent) {
      String location =
        fileStorageService.generateFinalImageFullPath(imageRequest.getAbsoluteImagePath());
      imageRequest.setAbsoluteImagePath(location);
      String imageName =
        location.substring(location.lastIndexOf(Constants.DELIMITER_SLASH) + Constants.ONE);
      imageRequest.setImageName(imageName);
    }
  }

  @Override
  public void publishImageQCForContentChange(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    productMasterDataEditRequest.setPublishImageQcForContentChange(
      masterProductEditDTO.isPublishImageQcForContentChange());
    if (productMasterDataEditRequest.isPublishImageQcForContentChange()
      && !isNewImageAddedForMasterProductUpdate(productMasterDataEditRequest)) {
      log.info("Publishing image qc event as content is changed but no new images added : {} ",
        productMasterDataEditRequest.getProductSku());
      CategoryResponse categoryResponse = new CategoryResponse();
      categoryResponse.setCategoryCode(masterProductEditDTO.getCategoryCode());
      categoryResponse.setName(masterProductEditDTO.getNewCategoryName());
      productService.publishImageQcEventForContentEdit(
        CollectionUtils.isNotEmpty(masterProductEditDTO.getCategoryResponses()) ?
          masterProductEditDTO.getCategoryResponses() :
          List.of(categoryResponse),
        masterProductEditDTO.getRestrictedKeywordsByFieldAndActionType(),
        fetchMinimalProductLevel3(productMasterDataEditRequest), false);
    }

  }

  private ProductLevel3 fetchMinimalProductLevel3(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    ProductLevel3 productLevel3 = new ProductLevel3();
    productLevel3.setProductCode(productMasterDataEditRequest.getProductCode());
    productLevel3.setProductName(productMasterDataEditRequest.getProductName());
    productLevel3.setBrand(productMasterDataEditRequest.getBrand());
    productLevel3.setUniqueSellingPoint(productLevel3.getUniqueSellingPoint());
    productLevel3.setDescription(productLevel3.getDescription());
    return productLevel3;
  }

  private boolean isNewImageAddedForMasterProductUpdate(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    return Optional.ofNullable(
        productMasterDataEditRequest.getProductLevel3SummaryDetailsImageRequests())
      .orElse(new ArrayList<>()).stream()
      .map(ProductLevel3SummaryDetailsImageRequest::getReviewType).filter(Objects::nonNull)
      .anyMatch(reviewType -> reviewType.equalsIgnoreCase(NEW));
  }

  private void performRestrictedKeywordActionOnMasterDataEdit(String storeId,
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) throws Exception {
    if (CommonUtils.isEligibleForAutoRejectOrAutoNeedRevision(masterProductEditDTO.getAction(),
      productMasterDataEditRequest.isTrustedSeller())) {
      productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(storeId,
        productMasterDataEditRequest.getProductCode(), masterProductEditDTO.getAction(),
        masterProductEditDTO.getCategoryRestrictedKeywordId(), false, false, false,
        new ArrayList<>(masterProductEditDTO.getVendorErrorFields()), false,
        productMasterDataEditRequest.getBusinessPartnerCode(), null, null, null);
    }
  }

  private void setReviewConfigOnContentChange(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    ProductCollection productCollection = masterProductEditDTO.getProductCollection();
    ConfigurationStatusResponse configurationStatusResponse = new ConfigurationStatusResponse();
    configurationStatusResponse.setReviewConfig(Constants.POST_LIVE);
    List<ConfigurationStatusResponse> configurationStatusResponseList = Collections.singletonList(configurationStatusResponse);
    if (masterProductEditDTO.isContentChanged()) {
      ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder()
        .categoryCode(productMasterDataEditRequest.getCategoryCode())
        .businessPartnerCode(productMasterDataEditRequest.getBusinessPartnerCode()).build();
      configurationStatusResponseList = productOutbound.getReviewConfiguration(
        Collections.singletonList(configurationStatusRequest));
    }
    boolean postLive =
      Constants.POST_LIVE.equals(configurationStatusResponseList.getFirst().getReviewConfig());
    productCollection.setPostLive(postLive);
    masterProductEditDTO.setPostLive(postLive);
    masterProductEditDTO.setConfigurationStatusResponseList(configurationStatusResponseList);
  }

  private void processAutoApproval(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) throws Exception {
    if (masterProductEditDTO.isContentChanged() && CollectionUtils.isEmpty(
      masterProductEditDTO.getRestrictedKeywordsByFieldList())) {
      Pair<AutoApprovalType, List<CategoryResponse>> autoApprovalTypeCategoryResponseListPair =
        productLevel3ServiceBean.processAutoApproval(masterProductEditDTO.getProductCollection(),
          productMasterDataEditRequest.getCategoryCode(),
          productMasterDataEditRequest.getUpdatedBy());
      masterProductEditDTO.setAutoApprovalType(autoApprovalTypeCategoryResponseListPair.getKey());
      masterProductEditDTO.setCategoryResponses(autoApprovalTypeCategoryResponseListPair.getRight());
    }
  }

  public void performRestrictedKeywordChecks(String storeId,
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) throws Exception {
    Set<L3InfoUpdateChangeType> changeTypesForCheck =
      Set.of(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE, L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    ProductCollection productCollection =
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        productMasterDataEditRequest.getProductCode());
    masterProductEditDTO.setProductCollection(productCollection);
    // set old category name and code and change same for auto category change
    masterProductEditDTO.setNewCategoryName(masterProductEditDTO.getOldCategoryName());
    masterProductEditDTO.setCategoryCode(productMasterDataEditRequest.getCategoryCode());
    if (productMasterDataEditRequest.getMasterDataEditChangeTypes().stream()
      .noneMatch(changeTypesForCheck::contains)) {
      return;
    }
    String productCode = productMasterDataEditRequest.getProductCode();
    RestrictedKeywordsByFieldAndActionType restrictedInfo =
      getRestrictedKeywordsByFieldAndActionType(productMasterDataEditRequest, productCode);

    List<RestrictedKeywordsByField> restrictedKeywordList =
      getRestrictedKeywordsByFields(productMasterDataEditRequest, restrictedInfo);

    // Handle auto category change
    boolean isAutoCategoryChange = restrictedInfo.getAction()
      == RestrictedKeywordActionType.CHANGE_CATEGORY_AND_AUTO_APPROVE.getRestrictedKeywordActionType();
    masterProductEditDTO.setOldCategoryName(productMasterDataEditRequest.getCategoryName());
    if (isAutoCategoryChange) {
      CategoryRestrictedKeywordResponse categoryKeywordResponse =
        productRepository.getCategoryRestrictedKeywordDetail(
          restrictedInfo.getCategoryRestrictedKeywordId());

      String destinationCategory = categoryKeywordResponse.getDestinationCategory();

      log.info(
        "Product eligible for auto category change, productCode: {}, destination category: {}",
        productCode, destinationCategory);

      if (!productMasterDataEditRequest.getCategoryCode().equals(destinationCategory)) {
        CategoryResponse newCategory =
          productOutbound.getCategoryBasicDetailByCategoryCode(destinationCategory);

        restrictedKeywordList =
          updateCategoryResponseOnAutoCategoryChange(productMasterDataEditRequest, masterProductEditDTO,
            newCategory, productCollection, restrictedInfo);
        fetchCategoryNamesOnUpdate(productMasterDataEditRequest, masterProductEditDTO, newCategory);
      }
    }

    productCollection.setRestrictedKeywordsPresent(
      CollectionUtils.isNotEmpty(restrictedInfo.getRestrictedKeywordsByFieldList()));

    // Final DTO population
    masterProductEditDTO.setContentChanged(true);
    masterProductEditDTO.setRestrictedKeywordsByFieldAndActionType(restrictedInfo);
    masterProductEditDTO.setRestrictedKeywordsByFieldList(restrictedKeywordList);
  }

  private void fetchCategoryNamesOnUpdate(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, CategoryResponse newCategory) {
    masterProductEditDTO.setAutoCategoryChange(true);
    CategoryDetailResponse categoryDetail =
      Optional.ofNullable(masterProductEditDTO.getCategoryDetailResponse()).orElseGet(() -> {
        CategoryDetailResponse detail = productOutbound.getCategoryDetailByCategoryCode(
          productMasterDataEditRequest.getCategoryCode());
        masterProductEditDTO.setCategoryDetailResponse(detail);
        return detail;
      });

    String categoryName =
      Optional.ofNullable(categoryDetail).map(CategoryDetailResponse::getName).orElse(null);
    masterProductEditDTO.setNewCategoryName(newCategory.getName());
    masterProductEditDTO.setCategoryCode(newCategory.getCategoryCode());
  }

  private static List<RestrictedKeywordsByField> updateCategoryResponseOnAutoCategoryChange(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, CategoryResponse newCategory,
    ProductCollection productCollection, RestrictedKeywordsByFieldAndActionType restrictedInfo) {
    List<RestrictedKeywordsByField> restrictedKeywordList;
    // Update category on request and collection
    productMasterDataEditRequest.setCategoryCode(newCategory.getCategoryCode());
    productMasterDataEditRequest.setCategoryName(newCategory.getName());

    productCollection.setCategoryCode(newCategory.getCategoryCode());
    productCollection.setCategoryName(newCategory.getName());

    masterProductEditDTO.setProductCollection(productCollection);

    // Reset restricted keywords since category has changed
    restrictedInfo.setRestrictedKeywordsByFieldList(Collections.emptyList());
    restrictedKeywordList = Collections.emptyList();
    return restrictedKeywordList;
  }

  private List<RestrictedKeywordsByField> getRestrictedKeywordsByFields(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    RestrictedKeywordsByFieldAndActionType restrictedInfo) {
    // Handle action if definitive
    if (CommonUtils.isDefinitiveActionToBeSkipped(restrictedInfo.getAction())) {
      productService.checkActiveOrderDataToSkipRestrictedKeywordAction(productMasterDataEditRequest,
        restrictedInfo);
    }

    List<RestrictedKeywordsByField> restrictedKeywordList = new ArrayList<>();
    if (restrictedInfo.getAction() != Constants.SKIP_ALL_ACTIONS) {
      restrictedKeywordList = restrictedInfo.getRestrictedKeywordsByFieldList();
    }
    return restrictedKeywordList;
  }

  private RestrictedKeywordsByFieldAndActionType getRestrictedKeywordsByFieldAndActionType(
    ProductMasterDataEditRequest productMasterDataEditRequest, String productCode) {
    // Construct minimal detail required for validation
    ProductDetailResponse productDetail = new ProductDetailResponse();
    productDetail.setProductCode(productCode);
    productDetail.setName(productMasterDataEditRequest.getProductName());
    productDetail.setDescription(
      productMasterDataEditRequest.getDescription().getBytes(StandardCharsets.UTF_8));
    productDetail.setUniqueSellingPoint(StringUtils.EMPTY);

    // Fetch restricted keyword metadata
    return productLevel3Helper.getRestrictedKeywordsWithActionTypeInProductDetails(productDetail,
      productMasterDataEditRequest.getCategoryCode());
  }


  private Pair<ApiErrorCode, MasterProductEditDTO> performValidationOnMasterDataEdit(String storeId,
    ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception {
    MasterProductEditDTO masterProductEditDTO = new MasterProductEditDTO();
    validateBasicProductDetails(storeId, productMasterDataEditRequest);
    masterProductEditDTO.setMaxProductDimensionLimit(maxProductDimensionLimit);
    // pureInstoreProduct is set to true if the product is instore and b2cActivated is false
    productMasterDataEditRequest.setPureInstoreProduct(
      CommonUtils.isPureInstoreProduct(productMasterDataEditRequest.isInstore(),
        productMasterDataEditRequest.getB2cActivated(), true));
    ValidationUtil.validateProductNameAndDescriptionAndUrl(productMasterDataEditRequest);
    // Validate product name and description
    ApiErrorCode apiErrorCode = validateProductNameAndDesciption(productMasterDataEditRequest);
    if (Objects.nonNull(apiErrorCode)) {
      return Pair.of(apiErrorCode, masterProductEditDTO);
    }
    fetchCategoryDetailsAndCncForProductTypeChange(productMasterDataEditRequest, masterProductEditDTO);
    // Validate product type and dimensions
    apiErrorCode =
      ValidationUtil.validateProductTypeAndDimensionsUpdate(productMasterDataEditRequest,
        masterProductEditDTO);
    if(Objects.nonNull(apiErrorCode)){
      return Pair.of(apiErrorCode, masterProductEditDTO);
    }
    // validate youtube url validity
    productLevel3V2Service.validateYoutubeUrlIsValid(productMasterDataEditRequest, null);
    if (CommonUtils.sizeChartCodeUpdated(productMasterDataEditRequest)) {
      apiErrorCode =
        productService.validateSizeChart(productMasterDataEditRequest.getSizeChartCode(),
          productMasterDataEditRequest.getStoreId());
      if (Objects.nonNull(apiErrorCode)) {
        return Pair.of(apiErrorCode, masterProductEditDTO);
      }
    }
    // Validate shipping and dimension
    apiErrorCode =
      productLevel3V2Service.validateShippingAndDimensionForEdit(productMasterDataEditRequest, false);
    if(Objects.nonNull(apiErrorCode)){
        return Pair.of(apiErrorCode, masterProductEditDTO);
    }
    return Pair.of(ApiErrorCode.SUCCESS_VALIDATION, masterProductEditDTO);
  }

  private void fetchCategoryDetailsAndCncForProductTypeChange(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO) {
    boolean bopisProduct =
      ProductType.BOPIS.getProductType().equals(productMasterDataEditRequest.getProductType());
    if (productMasterDataEditRequest.getMasterDataEditChangeTypes()
      .contains(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE)) {
      if (bopisProduct) {
        CategoryDetailResponse categoryDetailResponse =
          this.productOutbound.getCategoryDetailByCategoryCode(
            productMasterDataEditRequest.getCategoryCode());
        masterProductEditDTO.setCategoryDetailResponse(categoryDetailResponse);
      }
      masterProductEditDTO.setCncActiveAtL5(
        xProductOutbound.getCncAtL5ByProductSku(productMasterDataEditRequest.getProductSku())
          .getResult());
    }
  }

  private static void validateBasicProductDetails(String storeId,
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productMasterDataEditRequest.getProductSku()),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    checkArgument(StringUtils.isNotBlank(productMasterDataEditRequest.getProductCode()),
      ErrorMessages.PRODUCT_CODE_BLANK);
  }

  private ApiErrorCode validateProductNameAndDesciption(
    ProductMasterDataEditRequest productMasterDataEditRequest) throws Exception {
    try {
      ValidationUtil.validateProductNameAndDescription(productNameCharacterLimit,
        productMasterDataEditRequest);
      // Validate product name and description length
      productLevel3V2Service.validateDescriptionLength(
        productMasterDataEditRequest.getDescription());
    } catch (Exception e) {
      log.error(
        "Error while validating product name and description for productSku : {} , error : {}",
        productMasterDataEditRequest.getProductSku(), e.getMessage());
      throw e;
    }
    // order status validation on product name edit
    return productLevel3V2Service.validateExistingOrderOnProductNameEdit(
      productMasterDataEditRequest);
  }

  private static boolean isNewImagesAdded(ProductL3UpdateRequest productL3UpdateRequest) {
    return Optional.ofNullable(productL3UpdateRequest.getCommonImages()).orElse(new ArrayList<>())
      .stream().map(ProductLevel3SummaryDetailsImageRequest::getReviewType).filter(Objects::nonNull)
      .anyMatch(reviewType -> reviewType.equalsIgnoreCase(NEW)) || Optional.ofNullable(
        productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>()).stream()
      .map(ProductVariantPriceStockAndImagesRequest::getImages).filter(CollectionUtils::isNotEmpty)
      .flatMap(List::stream).map(ProductLevel3SummaryDetailsImageRequest::getReviewType)
      .filter(Objects::nonNull).anyMatch(reviewType -> reviewType.equalsIgnoreCase(NEW));
  }

  private ProductEditValidationDTO performRestrictedKeywordAction(
    ProductL3UpdateRequest productL3UpdateRequest, String storeId,
    EditProductResponse editProductResponse) throws Exception {
    // If product is eligible for auto reject, we don't want to do any updates
    log.info("Product eligible for Auto reject : productCode {} productSku : {} ",
        productL3UpdateRequest.getProductCode(), productL3UpdateRequest.getProductSku());
    performRestrictedKeywordsAction(storeId, productL3UpdateRequest, editProductResponse);
    log.info("Product Auto reject done : productCode {} productSku : {} ", productL3UpdateRequest.getProductCode(),
        productL3UpdateRequest.getProductSku());
    return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).build();
  }

  private ApiErrorCode performL5AndLogisticUpdateAndCheckForTakeDown(ProductL3UpdateRequest productL3UpdateRequest,
    boolean isOnlyExternal, boolean combineContentAndLogisticsPcbUpdate,
    boolean combinePreOrderUpdate, ProductLevel3 productLevel3Edit,
    EditProductResponse editProductResponse, ProductEditValidationDTO productEditValidationDTO,
    String storeId) throws Exception {
    ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    ConverterUtil.setProductLevel3LogisticsRequestAndPreOrderRequest(productL3UpdateRequest,
        productLevel3UpdateRequest);
    ApiErrorCode logisticUpdateResponse =
        this.productLevel3V2Service.updateLogistics(productLevel3UpdateRequest, isOnlyExternal,
            productLevel3Edit.isNeedCorrection(), combineContentAndLogisticsPcbUpdate,
          combinePreOrderUpdate);

    if (validateLogisticErrorBeforeL5Update && Objects.nonNull(logisticUpdateResponse)) {
      return logisticUpdateResponse;
    }

    editProductResponse.setProductLevel3LogisticsRequest(productLevel3UpdateRequest.getProductLevel3LogisticsRequest());
    log.info("Updating L5 and commom image update : productCode {} productSku : {} ",
        productL3UpdateRequest.getProductCode(), productL3UpdateRequest.getProductSku());
    ItemsPriceStockImagesUpdateResponse l5UpdateResponse =
        getItemsPriceStockImagesUpdateResponse(productEditValidationDTO, editProductResponse,
          storeId);
    takeDownProduct(productLevel3Edit.getProductCode(), editProductResponse, l5UpdateResponse,
      productL3UpdateRequest, storeId);
    return logisticUpdateResponse;
  }

  private ProductL3Response generateItemNameForNewlyAddedItem(ProductL3UpdateRequest productL3UpdateRequest)
      throws Exception {
    ProductL3Response savedProductData = null;
    if (CommonUtils.isItemNameEmptyForAnyNewlyAddedItem(productL3UpdateRequest, overrideItemNameGeneration)) {
      savedProductData =
          productLevel3V2Service.getProductL3ResponseFromXProduct(productL3UpdateRequest.getProductSku());
    }
    Map<String, String> attributeCodeAndIdMapFromXProduct = getAttributeCodeAndIdMap(savedProductData);
    Set<String> newAttributeCodes =
          CommonUtils.getNewlyAddedAttributeCodes(productL3UpdateRequest, attributeCodeAndIdMapFromXProduct,
              familyColorAttributeCode);
      if (CollectionUtils.isNotEmpty(newAttributeCodes)) {
        AttributeCodesRequest attributeCodesRequest = new AttributeCodesRequest();
        attributeCodesRequest.setAttributeCodes(new ArrayList<>(newAttributeCodes));
        Map<String, String> attributeCodeAndIdMapFromPCB =
          productLevel3V2Service.getAttributeCodeAndIdMap(true, attributeCodesRequest);
        attributeCodeAndIdMapFromXProduct.putAll(attributeCodeAndIdMapFromPCB);
      }
      // get list of variants eligible for name overriding
    List<ProductVariantPriceStockAndImagesRequest> newProductItems =
      generateItemNameForNewlyAddedVariants(productL3UpdateRequest);

    for(ProductVariantPriceStockAndImagesRequest productVariantUpdateRequest : newProductItems) {
      CommonUtils.overrideVariantNameForNewlyAddedVariants(productL3UpdateRequest, productVariantUpdateRequest,
        attributeCodeAndIdMapFromXProduct, savedProductData);
    }
    return savedProductData;
  }

  private List<ProductVariantPriceStockAndImagesRequest> generateItemNameForNewlyAddedVariants(
    ProductL3UpdateRequest productL3UpdateRequest) {
    //attributeCodeAndIdMapFromXProduct map has code and id for all , existing and new attributes
    //Forming map of Attribute id and value to sort based on id for each product item where item name is empty
    Predicate<ProductVariantPriceStockAndImagesRequest> isEligibleForNameRegeneration =
      item -> item.isNewlyAddedItem() && (StringUtils.isBlank(item.getItemName())
        || overrideItemNameGeneration);
    // Always Regenerate Variant name from backend for newly Added Variants
    List<ProductVariantPriceStockAndImagesRequest> newProductItems =
      productL3UpdateRequest.getProductItems().stream().filter(isEligibleForNameRegeneration::evaluate)
        .collect(Collectors.toList());
    return newProductItems;
  }

  public SortedMap<String, String> getAttributeIdAndValue(Map<String, String> attributeCodeAndIdMapFromXProduct,
      ProductVariantPriceStockAndImagesRequest productItem) {
    return productItem.getAttributesMap().entrySet().stream()
        .filter(entry -> !StringUtils.equals(familyColorAttributeCode, entry.getKey()))
        .filter(entry -> attributeCodeAndIdMapFromXProduct.containsKey(entry.getKey())).collect(
            Collectors.toMap(entry -> attributeCodeAndIdMapFromXProduct.get(entry.getKey()), Map.Entry::getValue,
                (oldValue, newValue) -> oldValue, TreeMap::new));
  }

  public Map<String, String> getAttributeCodeAndIdMap(ProductL3Response savedProductData) {
    Map<String, String> result = new HashMap<>();
    if (Objects.nonNull(savedProductData)) {
      MasterDataProductDTO masterDataProduct = savedProductData.getMasterDataProduct();
      if (Objects.nonNull(masterDataProduct) && CollectionUtils.isNotEmpty(
          masterDataProduct.getMasterDataProductAttributes())) {
        for (MasterDataProductAttributeDTO productAttribute : masterDataProduct.getMasterDataProductAttributes()) {
          MasterDataAttributeDTO masterDataAttribute = productAttribute.getMasterDataAttribute();
          if (Objects.nonNull(masterDataAttribute)) {
            result.put(masterDataAttribute.getAttributeCode(), masterDataAttribute.getId());
          }
        }
      }
    }
    return result;
  }

  public EditProductResponse processUpdateForEditFlow(boolean isOnlyExternal,
      boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate,
      EditProductResponse editProductResponse, ProductEditValidationDTO productEditValidationDTO,
      ProfileResponse profileResponse, boolean newImagesAdded, ProductL3UpdateRequest productL3UpdateRequest) throws Exception {
    EditProductResponse newEditProductResponse = editProductResponse;
    if (!productEditValidationDTO.isNeedCorrection()) {
      productEditValidationDTO.getProductLevel3()
          .setSellerOmg(productEditValidationDTO.getProductL3UpdateRequest().isSellerOmg());
      productEditValidationDTO.getProductLevel3().setDistributionInfoUpdated(editProductResponse.isDistributionInfoUpdated());
      newEditProductResponse =
          productLevel3V2Service.updateEditInfo(productEditValidationDTO.getProductLevel3(), isOnlyExternal,
              combineContentAndLogisticsPcbUpdate, combinePreOrderUpdate, profileResponse,
              productEditValidationDTO.getProductL3Response(), newImagesAdded,
              editProductResponse.getProductCollection(), productL3UpdateRequest);
      newEditProductResponse.setProduct(editProductResponse.getProduct());
    }
    return newEditProductResponse;
  }

  private ItemsPriceStockImagesUpdateResponse getItemsPriceStockImagesUpdateResponse(
      ProductEditValidationDTO productEditValidationDTO, EditProductResponse editProductResponse, String storeId)
      throws Exception {
    ItemsPriceStockImagesUpdateResponse l5UpdateResponse;
    ProductVariantUpdateRequest productVariantUpdateRequest =
        productLevel3V2Service.toProductVariantUpdateRequest(productEditValidationDTO.getProductL3UpdateRequest(),
            editProductResponse);
    //l5 and common image update
    l5UpdateResponse =
        productLevel3V2Service.editPriceStockVariantsInfo(storeId, productEditValidationDTO.getProductLevel3(),
            productVariantUpdateRequest, editProductResponse);
    editProductResponse.setProductReview(l5UpdateResponse.isProductReview() || editProductResponse.isProductReview());
    if (Objects.nonNull(l5UpdateResponse.getApiErrorCode())) {
      editProductResponse.setApiErrorCode(l5UpdateResponse.getApiErrorCode());
    }
    editProductResponse.setVariantsErrorList(l5UpdateResponse.getVariantsErrorList());
    if (StringUtils.isNotEmpty(l5UpdateResponse.getReviewType())) {
      editProductResponse.setReviewType(l5UpdateResponse.getReviewType());
    }
    return l5UpdateResponse;
  }

  private void takeDownProduct(String productCode, EditProductResponse editProductResponse,
      ItemsPriceStockImagesUpdateResponse l5UpdateResponse, ProductL3UpdateRequest productL3UpdateRequest,
      String storeId) throws Exception {
    log.info("L3 response for taking down product {} ", editProductResponse.isToTakeDown());
    log.info("L5 response for taking down product {} ", l5UpdateResponse.isTakeDown());
    // Product take down or Auto need revision
    takeDownProductOrSendProductToNeedRevision(storeId, productL3UpdateRequest, editProductResponse, l5UpdateResponse);
    // Only in case of auto need revision publish edited image resize event at last
    if (RestrictedKeywordActionType.AUTO_NEED_REVISION.getRestrictedKeywordActionType()
        == editProductResponse.getAction()) {
      productServiceWrapper.publishEditedImageResizeEvent(productCode,
          l5UpdateResponse.getEditedResizeAndImagesUpdateStatusResponse());
    }
  }

  private void takeDownProductOrSendProductToNeedRevision(String storeId, ProductL3UpdateRequest request,
      EditProductResponse editResponse, ItemsPriceStockImagesUpdateResponse l5UpdateResponse) throws Exception {
    if (CommonUtils.isEligibleForAutoNeedRevision(editResponse)) {
      log.info("Product eligible for Auto need revision : productCode {} productSku : {} ", request.getProductCode(),
          request.getProductSku());
      performRestrictedKeywordsAction(storeId, request, editResponse);
      log.info("Product Auto need revision done : productCode {} productSku : {} ", request.getProductCode(),
          request.getProductSku());
    } else if (editResponse.isToTakeDown() || l5UpdateResponse.isTakeDown()) {
      //take down product by l3 response
      log.info("Taking down product for sku {} ", request.getProductSku());
      productLevel3V2Service.takeDownProduct(storeId, request.getProductSku(), request.getProductName());
    }
  }

  private void performRestrictedKeywordsAction(String storeId, ProductL3UpdateRequest request,
      EditProductResponse editResponse) throws Exception {
    productServiceWrapper.performResultantActionBasedOnRestrictedKeywords(storeId, request.getProductCode(),
        editResponse.getAction(), editResponse.getCategoryRestrictedKeywordId(), false, false, false,
        new ArrayList<>(editResponse.getVendorErrorFields()), false, request.getBusinessPartnerCode(),
        editResponse.getProfileResponse(), editResponse.getItemCodeToItemNameMapping().keySet(), editResponse);
  }

  private static ProductLevel3QuickEditV2Request updatingSkuOfflineForOrderCancellation(
      OrderCancellationDto orderCancellationDto) {
    QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
    quickEditV2Request.setItemSku(orderCancellationDto.getItemGdnSku());
    quickEditV2Request.setPickupPointCode(orderCancellationDto.getPickupPointCode());
    quickEditV2Request.setItemPickupPointId(CommonUtils.toL5Id(orderCancellationDto.getItemGdnSku(),
        orderCancellationDto.getPickupPointCode()));
    quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
    quickEditV2Request.setPrice(new ProductLevel3PriceRequest());
    ProductLevel3QuickEditV2Request request = new ProductLevel3QuickEditV2Request();
    request.setQuickEditV2Requests(Arrays.asList(quickEditV2Request));
    return request;
  }

  private void settingUserNameBasedOnCancellationActor(OrderCancellationDto orderCancellationDto) {
    String userName;
    switch (orderCancellationDto.getCancellationActor().toLowerCase()) {
      case Constants.AUTO_CANCEL:
        userName = Constants.AUTO_CANCEL_ORDER_CANCELLATION;
        break;
      case Constants.SELLER:
        userName = orderCancellationDto.getStatusOFFUpdatedBy() + StringUtils.SPACE + Constants.ORDER_CANCELLATION;
        break;
      case Constants.MTA_API:
        userName =
            orderCancellationDto.getStatusOFFUpdatedBy() + StringUtils.SPACE + orderCancellationDto.getMtaApiPartner()
                + StringUtils.SPACE + Constants.ORDER_CANCELLATION;
        break;
      case Constants.ORDER_CENTER:
        userName =
            orderCancellationDto.getStatusOFFUpdatedBy() + StringUtils.SPACE + Constants.HYPHEN + StringUtils.SPACE
                + Constants.ORDER_CENTER_INTERNAL;
        break;
      case Constants.CANCELLATION_ACTOR_OFF:
        userName =
            orderCancellationDto.getStatusOFFUpdatedBy() + StringUtils.SPACE + Constants.HYPHEN + StringUtils.SPACE
                + Constants.ORDER_CENTER_INTERNAL;
        break;
      default:
        userName = Constants.DEFAULT_USERNAME;
    }
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
  }

  private static void argumentCheckForOrderCancellation(
      OrderCancellationDto orderCancellationDto) {
    checkArgument(StringUtils.isNotBlank(orderCancellationDto.getOrderItemStatus()),
        ErrorMessages.ORDER_ITEM_STATUS_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(orderCancellationDto.getItemGdnSku()),
        ErrorMessages.ITEM_GDN_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(orderCancellationDto.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(orderCancellationDto.getProductGdnSku()),
        ErrorMessages.PRODUCT_GDN_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(orderCancellationDto.getCancellationActor()),
        ErrorMessages.CANCELLATION_ACTOR_MUST_NOT_BE_BLANK);
  }

  @Override
  public OmniChannelMapAndSkuResponse checkIfOmniChannelSkuExists(String storeId,
      OmniChannelSkuRequest omniChannelSkuRequest) {
    ValidOmniChannelSkuResponse validOmniChannelSkuResponse =
        productLevel3V2Service.checkIfOmniChannelSkuExists(storeId, omniChannelSkuRequest);
    if (Objects.nonNull(validOmniChannelSkuResponse) && MapUtils.isNotEmpty(
        validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap())) {
      setItemSkuInResponse(storeId, validOmniChannelSkuResponse);
    }
    return new OmniChannelMapAndSkuResponse(CommonUtils.convertToOmniChannelSkuResponse(validOmniChannelSkuResponse,
        new HashMap<>()));
  }

  private void setItemSkuInResponse(String storeId, ValidOmniChannelSkuResponse validOmniChannelSkuResponse) {
    List<String> productItemIds =
        validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap().values().stream()
            .map(ProductL1AndL2CodeResponse::getProductItemId).filter(StringUtils::isNotBlank).toList();
    if (CollectionUtils.isNotEmpty(productItemIds)) {
      List<ProductItemBusinessPartner> productItemByProductItemIdIn =
          productItemBusinessPartnerService.findProductItemByProductItemIdIn(storeId, productItemIds);
      Map<String, ProductItemBusinessPartner> productItemBusinessPartnerMap = productItemByProductItemIdIn.stream()
          .collect(Collectors.toMap(ProductItemBusinessPartner::getProductItemId, Function.identity(), (a, b) -> a));
      validOmniChannelSkuResponse.getExistingOmniChannelSkusAndProductDetailsMap().forEach(
          (key, value) -> value.setItemSku(productItemBusinessPartnerMap.getOrDefault(
                  Optional.ofNullable(value.getProductItemId()).orElse(StringUtils.EMPTY), new ProductItemBusinessPartner())
              .getGdnProductItemSku()));
    }
  }

}
