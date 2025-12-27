package com.gdn.partners.pbp.workflow.product;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.util.BaseGenerator;
import com.gdn.mta.product.util.ProductContentUtil;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.request.DimensionAndUomDTO;
import com.gdn.x.productcategorybase.dto.request.DistributionItemInfoRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductReviewStatus;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.repository.BrandWipRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;
import com.google.common.collect.ImmutableSet;

/**
 * Created by Vishal on 28/06/18.
 */
@Slf4j
@Component(value = CreateProductWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class CreateProductWorkflowWorkerBean implements WorkflowWorker {

  private static final Logger LOGGER = LoggerFactory.getLogger(CreateProductWorkflowWorkerBean.class);

  private static final String REQUEST = "request";
  public static final String BEAN_NAME = "createProduct";
  private static final String PROCESS_CODE = "processCode";
  private static final String MPP_FLOW = "MPPFlow";
  private static final String POST_LIVE = "Post-live";
  private static final String INTERNAL = "INTERNAL";
  public static final String INVALID_EAN_UPC_FORMAT = "EAN UPC code is not in valid format";
  public static final String INACTIVE_PRODUCT_CATEGORY = "Product Category is inactive. Can't create product";
  public static final String INVALID_ATTRIBUTE = "Attribute is invalid/inactive";
  private static final String PRODUCT_CREATION_TYPE = "productCreationType";
  private Set<String> WEB_FLOW_PROCESS_TYPE =
      ImmutableSet.of(ProductCreationType.FLOW1_WEB.name(), ProductCreationType.FLOW2_WEB.name(),
          ProductCreationType.FLOW3_WEB.name(), ProductCreationType.CATEGORY_BULK_UPLOAD.name(),
          ProductCreationType.UNIFIED_BULK_UPLOAD.name(),
          ProductCreationType.EXTERNAL_BULK_UPLOAD.name(),
          ProductCreationType.CONVERTED_BULK_UPLOAD.name());

  private Set<String> PRODUCT_CREATION_TYPES_TO_IGNORE_MISSING_FIELDS =
      ImmutableSet.of(ProductCreationType.UNIFIED_BULK_UPLOAD.name(),
          ProductCreationType.CATEGORY_BULK_UPLOAD.name(), ProductCreationType.AUTO_UPLOAD.name(),
          ProductCreationType.STORE_COPY_FLOW_2.name(),
          ProductCreationType.UNIFIED_BULK_UPLOAD.name(),
          ProductCreationType.EXTERNAL_BULK_UPLOAD.name(),
          ProductCreationType.CONVERTED_BULK_UPLOAD.name());


  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private BrandWipRepository brandWipRepository;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${skip.review.switch}")
  private boolean isSkipReviewSwitch;

  @Value("${skip.screening.switch}")
  private boolean isSkipScreeningSwitch;

  @Value("${validate.create.attribute}")
  private boolean validateCreateAttribute;

  @Value("${default.trusted.seller.priority}")
  private int prioritySeller;

  @Value("${auto.fill.family.colour.attribute}")
  private boolean autoFillFamilyColourAttribute;

  @Value("${family.color.attribute.code}")
  private String familyColorAttributeCode;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${ean.upc.valid.length}")
  private List<Integer> eanUpcValidLength;

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    try {
    ProductCreationRequest request = (ProductCreationRequest) datas.get(REQUEST);
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    String productCreationType = String.valueOf(datas.get(PRODUCT_CREATION_TYPE));
    request.setViewable(Boolean.FALSE);
    request.setActivated(Boolean.FALSE);
    validateProductRequestForInternalUserOrSkipScreening(request);
    LOGGER.info("invoking product creation for product code : {}", request.getProductCode());
    ProductRequest productRequest = new ProductRequest();
    validateProductCategoryStatusAndUpdateShippingWeightAndAutoFillFamilyColour(request);
    validateBrandStatusAndBusinessPartner(request);
      if (ranchIntegrationEnabled && MapUtils.isNotEmpty(request.getDistributionInfoRequest())) {
        productRequest.setDistributionInfoRequest(request.getDistributionInfoRequest());
      }
    BeanUtils.copyProperties(request, productRequest, "productItemRequests", "productBusinessPartnerAttributes", "images");
    request.setCreatedDate(Calendar.getInstance().getTime());
    productRequest.setReviewPending(true);
    setCommonImages(request, productRequest);
    List<ProductItemRequest> productItemRequests = new ArrayList<>();
    request.getProductItemRequests().stream().forEach(productItemCreationRequest->{
      ProductItemRequest productItemRequest = new ProductItemRequest();
      TreeMap<String, String> attributeMap = productItemCreationRequest.getAttributesMap();
      if(MapUtils.isEmpty(attributeMap)) {
        attributeMap = new TreeMap<>();
        productItemCreationRequest.setAttributesMap(attributeMap);
      }
      if (StringUtils.isNotBlank(productItemCreationRequest.getUpcCode())) {
        if (!BarcodeGenerator.isValidUPCCode(productItemCreationRequest.getUpcCode(), eanUpcValidLength)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_EAN_UPC_FORMAT);
        }
      }
      productItemRequest.setUpcCode(productItemCreationRequest.getUpcCode());
      productItemRequest.setOmniChannelSku(productItemCreationRequest.getMerchantSku());
      productItemRequest.setSourceItemCode(productItemCreationRequest.getSourceItemCode());
      productItemRequest.setContentChanged(productItemCreationRequest.isContentChanged());
      productItemCreationRequest.setProductItemHashCode(Base64
          .encodeBase64String(attributeMap.toString().getBytes()));
      productItemRequest.setAttributesMap(attributeMap);
      productItemRequest.setImages(
          productItemCreationRequest.getImages().stream().map(this::setHashCodeAndOriginalImageFlag)
              .collect(Collectors.toList()));
      productItemRequest.setProductItemAttributeValues(productItemCreationRequest
          .getProductItemAttributeValueRequests());
      productItemRequest.setGeneratedItemName(productItemCreationRequest.getItemGeneratedName());
      if (ranchIntegrationEnabled) {
        setItemDistributionAndUomRequest(productItemCreationRequest, productItemRequest);
      }
      productItemRequests.add(productItemRequest);
    });
    productRequest.setProductItems(productItemRequests);
    if(Objects.isNull(productRequest.getCreatedMerchant())) {
      productRequest.setCreatedMerchant(request.getBusinessPartnerCode());
    }
      if (PRODUCT_CREATION_TYPES_TO_IGNORE_MISSING_FIELDS.contains(productCreationType)) {
        productRequest.setIgnoreMissingItems(true);
      } else {
        if (prioritySeller > 0 && !INTERNAL.equals(request.getBusinessPartnerCode())) {
          ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(request.getBusinessPartnerCode());
          if (profileResponse.isTrustedSeller()) {
            productRequest.setPrioritySeller(prioritySeller);
          }
        }
      }
    boolean skipReview = isSkipReview(request);
    if (skipReview) {
      productRequest.setSpecificationDetail(CommonUtils.generateSpecificationDetail(productRequest));
    }
    if (ProductCreationType.EXTERNAL_BULK_UPLOAD.name().equals(productCreationType)) {
      productRequest.setAiGeneratedFieldsResponse(new AiGeneratedFieldsResponse(true, true));
    }
    Map<String, String> itemHashCodeToItemIdMap =
        productRepository.createProduct(productRequest, !WEB_FLOW_PROCESS_TYPE.contains(productCreationType));
    ProductDetailResponse productDetailResponse =
        productRepository.findProductDetailByProductCode(productRequest.getProductCode());
    LOGGER.info("creating new entry in product-collection for product code : {}",
        request.getProductCode());
    List<ConfigurationStatusResponse> responseList = null;
    if (!INTERNAL.equalsIgnoreCase(request.getBusinessPartnerCode())) {
      responseList = this.productRepository.getConfigurationStatus(ConverterUtil
          .toConfigurationStatusRequestList(request.getBusinessPartnerCode(),
              request.getProductCategories().get(0).getCategory().getCategoryCode()));
    }
    boolean postLive = false;
    if (Objects.nonNull(responseList) && POST_LIVE.equalsIgnoreCase(responseList.get(0).getReviewConfig())) {
      postLive = true;
    }
    ProductCollection productCollection =
        productLevel1CollectionService.create(request.getBusinessPartnerCode(), request.getBusinessPartnerName(),
            productDetailResponse, request.getBrandCode(), request.getBrandApprovalStatus(), postLive, skipReview,
            productCreationType, isSkipScreeningSwitch, productRequest.getPrioritySeller());
    ProductReviewStatus productReviewStatus = ProductReviewStatus.SCREENING_APPROVED;
    if (!GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE.equals(request.getBusinessPartnerCode())) {
      productReviewStatus =
          isSkipScreeningSwitch ? ProductReviewStatus.SCREENING_APPROVED : ProductReviewStatus.IN_SCREENING;
      request.getProductItemRequests().stream().forEach(
          productItemCreationRequest->productItemCreationRequest
              .setProductItemId(itemHashCodeToItemIdMap.get(productItemCreationRequest.getProductItemHashCode())));
      boolean isMPPFlow = Boolean.valueOf(String.valueOf(datas.get(MPP_FLOW)));
      ProductBusinessPartner productBusinessPartner =
          convertProductBusinessPartnerRequestToProductBusinessPartner(request, productDetailResponse.getId(),
              isMPPFlow);
      validateCncPickupPoints(request, isMPPFlow, productBusinessPartner);
      setCncAtL3Request(productBusinessPartner);
      LOGGER.info("mapping product to business-partner to product code : {}", request.getProductCode());
      ProductBusinessPartner updatedProductBusinessPartner =
          productBusinessPartnerService.saveBusinessPartner(productBusinessPartner, productDetailResponse, isMPPFlow);
      Map<String, String> itemIdToGdnSkuMap = updatedProductBusinessPartner.getProductItemBusinessPartners().stream()
          .collect(Collectors.toMap(ProductItemBusinessPartner::getProductItemId,
              ProductItemBusinessPartner::getGdnProductItemSku, (a, b) -> a));
      Map<String, String> itemIdToCodeMap = productDetailResponse.getProductItemResponses().stream()
          .filter(productItemResponse -> !productItemResponse.isMarkForDelete())
          .filter(productItemResponse -> StringUtils.isNotBlank(productItemResponse.getSkuCode())).collect(Collectors
              .toMap(BaseDTOResponse::getId, ProductItemResponse::getSkuCode));
      List<ProductItemWholesalePrice> productItemWholesalePriceList = new ArrayList<>();
      if (isMPPFlow) {
        getWholeSalePriceRequestsAtL5(request, itemHashCodeToItemIdMap, itemIdToGdnSkuMap, itemIdToCodeMap,
            productItemWholesalePriceList);
      } else {
        getWholeSalePriceRequests(request, itemHashCodeToItemIdMap, itemIdToGdnSkuMap, itemIdToCodeMap,
            productItemWholesalePriceList);
      }
      if (!CollectionUtils.isEmpty(productItemWholesalePriceList)) {
        productItemWholesalePriceService.saveWholesalePrice(productItemWholesalePriceList);
      }
    }
    productLevel1HistoryService.create(productCollection, processCode, null);
  } catch (ApplicationRuntimeException e) {
      log.error("Error while product creation, product code : {} ", String.valueOf(datas.get(PROCESS_CODE)), e);
      throw e;
    }
  catch (Exception e) {
      log.error("Error while product creation, product code : {} ", String.valueOf(datas.get(PROCESS_CODE)), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "Failed to create product");
    }
  }

  private static void setItemDistributionAndUomRequest(ProductItemCreationRequest productItemCreationRequest,
      ProductItemRequest productItemRequest) {
    if (Objects.nonNull(productItemCreationRequest.getDistributionItemInfoRequest()) && CollectionUtils.isNotEmpty(
        productItemCreationRequest.getDimensionsAndUOMRequest())) {
      ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
      DistributionItemInfoRequest distributionItemInfoRequest = new DistributionItemInfoRequest();
      BeanUtils.copyProperties(productItemCreationRequest.getDistributionItemInfoRequest(),
          distributionItemInfoRequest);
      distributionItemInfoRequest.setOmniChannelSku(productItemCreationRequest.getMerchantSku());
      productItemUomInfoDTO.setDistributionItemInfoRequest(distributionItemInfoRequest);
      List<DimensionAndUomDTO> dimensionAndUomDTOList = new ArrayList<>();
      for (DimensionAndUomRequest dimensionAndUomRequest : productItemCreationRequest.getDimensionsAndUOMRequest()) {
        DimensionAndUomDTO dimensionAndUomDTO = new DimensionAndUomDTO();
        BeanUtils.copyProperties(dimensionAndUomRequest, dimensionAndUomDTO);
        dimensionAndUomDTOList.add(dimensionAndUomDTO);
      }
      productItemUomInfoDTO.setDimensionAndUomDTOList(dimensionAndUomDTOList);
      productItemRequest.setProductItemUomInfoDTO(productItemUomInfoDTO);
    }
  }

  private void setCncAtL3Request(ProductBusinessPartner productBusinessPartner) {
    boolean cncActivatedAtL3 = false;
    for (ProductItemBusinessPartner productItemBusinessPartner :
        productBusinessPartner.getProductItemBusinessPartners()) {
      if (!cncActivatedAtL3) {
        if (isL5Cnc(productItemBusinessPartner)) {
          cncActivatedAtL3 = true;
        }
      }
    }
    productBusinessPartner.setCncActivated(cncActivatedAtL3);
  }

  private void setCommonImages(ProductCreationRequest request, ProductRequest productRequest) {
    if (!CollectionUtils.isEmpty(request.getCommonImages())) {
      List<Image> commonImagesRequest = new ArrayList<>();
      for (Image image : request.getCommonImages()) {
        Image destinationImage = new Image();
        BeanUtils.copyProperties(image, destinationImage);
        commonImagesRequest.add(destinationImage);
      }
      productRequest.setCommonImages(commonImagesRequest);
    }
  }

  private void validateCncPickupPoints(ProductCreationRequest request, boolean isMPPFlow,
      ProductBusinessPartner productBusinessPartner) {
    if (isMPPFlow) {
      List<String> cncPickupPointsFromXProduct = new ArrayList<>();
      Set<String> cncPickupPointsToValidate = productBusinessPartner.getProductItemBusinessPartners().stream()
          .filter(this::isL5Cnc).map(ProductItemBusinessPartner::getPickupPointId)
          .collect(Collectors.toSet());
      if (!cncPickupPointsToValidate.isEmpty()) {
        List<BusinessPartnerPickupPointResponse> pickupPointDetailsByListOfPickupPointCodes =
            xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(new ArrayList<>(cncPickupPointsToValidate));
        if (!pickupPointDetailsByListOfPickupPointCodes.isEmpty()) {
          cncPickupPointsFromXProduct = pickupPointDetailsByListOfPickupPointCodes.stream()
              .filter(BusinessPartnerPickupPointResponse::isCncActivated)
              .map(BusinessPartnerPickupPointResponse::getCode).collect(Collectors.toList());
        }
        for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
            .getProductItemBusinessPartners()) {
          if (isL5Cnc(productItemBusinessPartner) && !cncPickupPointsFromXProduct
              .contains(productItemBusinessPartner.getPickupPointId())) {
            LOGGER.error("Setting CNC active as false for productCode : {} , itemSku : {} , pickupPointCode : {}  ",
                request.getProductCode(), productItemBusinessPartner.getGdnProductItemSku(),
                productItemBusinessPartner.getPickupPointId());
            if (cncForWarehouseFeatureSwitch) {
              productItemBusinessPartner.setCncBuyable(false);
              productItemBusinessPartner.setCncDiscoverable(false);
            } else {
              productItemBusinessPartner.setCncActivated(false);
            }
          }
        }
      }
    }
  }

  private boolean isL5Cnc(ProductItemBusinessPartner productItemBusinessPartner) {
    return (!cncForWarehouseFeatureSwitch && productItemBusinessPartner.isCncActivated()) || (
        cncForWarehouseFeatureSwitch && (productItemBusinessPartner.isCncBuyable()
            || productItemBusinessPartner.isCncDiscoverable()));
  }

  private void validateProductCategoryStatusAndUpdateShippingWeightAndAutoFillFamilyColour(
      ProductCreationRequest request) throws Exception {
    GdnRestSingleResponse<CategoryDetailResponse> categoryDetail =
        pcbFeign.getCategoryDetailByCategoryCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            request.getProductCategories().get(0).getCategory().getCategoryCode());
    if (Objects.isNull(categoryDetail) || Objects.isNull(categoryDetail.getValue())) {
      LOGGER.error("ERROR : Unknown category for : {}", request.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Category not found");
    }
    if (!categoryDetail.getValue().isActivated()) {
      LOGGER.error(INACTIVE_PRODUCT_CATEGORY + " : {}", request.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INACTIVE_PRODUCT_CATEGORY);
    }
    validateProductCreationAttributes(request, categoryDetail);
    Map<String, String> attributeCodeAndNameMap = Optional.ofNullable(categoryDetail.getValue().getCategoryAttributes())
        .map(categoryAttributes -> categoryAttributes.stream().map(CategoryAttributeResponse::getAttribute)
            .filter(Objects::nonNull).filter(attributeResponse -> StringUtils.isNotBlank(attributeResponse.getName()))
            .collect(Collectors.toMap(AttributeResponse::getAttributeCode, AttributeResponse::getName,
                (oldValue, newValue) -> newValue))).orElseGet(Collections::emptyMap);
    //Override valid attribute name in request from PCB
    overrideAttributeNameInRequest(request, attributeCodeAndNameMap);
    double shippingWeight =
        BaseGenerator.generateShippingWeight(request.getLength(), request.getWidth(), request.getHeight(),
            request.getWeight(), categoryDetail.getValue().getLogisticAdjustment());
    request.setShippingWeight(Math.round(shippingWeight * 1000D) / 1000D);

    ProductContentUtil.autoFillFamilyColourItemAttribute(request, categoryDetail.getValue(), familyColorAttributeCode,
        autoFillFamilyColourAttribute);
  }

  private static void overrideAttributeNameInRequest(ProductCreationRequest request,
      Map<String, String> attributeCodeAndNameMap) {
    List<ProductAttributeRequest> productAttributes = request.getProductAttributes();
    for (ProductAttributeRequest productAttributeRequest : productAttributes) {
      String attributeCode = productAttributeRequest.getAttribute().getAttributeCode();
      String attributeName = attributeCodeAndNameMap.getOrDefault(attributeCode, StringUtils.EMPTY);
      if (StringUtils.isNotEmpty(attributeName)) {
        productAttributeRequest.setProductAttributeName(attributeName);
        Optional.ofNullable(productAttributeRequest.getAttribute())
            .ifPresent(attribute -> attribute.setName(attributeName));
      }
    }
  }


  private void validateProductCreationAttributes(ProductCreationRequest request,
      GdnRestSingleResponse<CategoryDetailResponse> categoryDetail) {
    if (validateCreateAttribute) {
      Set<String> requestAttributeCodes = request.getProductItemRequests().stream().flatMap(
          productItemCreationRequest -> productItemCreationRequest.getProductItemAttributeValueRequests().stream())
          .map(productItemAttributeValueRequest -> productItemAttributeValueRequest.getAttribute().getAttributeCode())
          .collect(Collectors.toSet());
      Set<String> productAttributes = request.getProductAttributes().stream()
          .map(productAttributeRequest -> productAttributeRequest.getAttribute().getAttributeCode()).collect(Collectors.toSet());
      requestAttributeCodes.addAll(productAttributes);
      Set<String> categoryAttributeCodes =
          categoryDetail.getValue().getCategoryAttributes().stream().map(CategoryAttributeResponse::getAttribute)
              .map(AttributeResponse::getAttributeCode).collect(Collectors.toSet());
      if (!categoryAttributeCodes.containsAll(requestAttributeCodes)) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, INVALID_ATTRIBUTE);
      }
    }
  }

  private boolean isSkipReview(ProductCreationRequest productCreationRequest) {
    if (isSkipReviewSwitch) {
      boolean isContentChanged = productCreationRequest.getProductItemRequests().stream()
          .anyMatch(ProductItemCreationRequest::isContentChanged);
      boolean hasSourceItemCode = productCreationRequest.getProductItemRequests().stream().anyMatch(
          productItemCreationRequest -> StringUtils.isNotBlank(productItemCreationRequest.getSourceItemCode()));
      return hasSourceItemCode && !(isContentChanged || productCreationRequest.isImagesUpdated());
    } else {
      return false;
    }
  }

  private void validateBrandStatusAndBusinessPartner(ProductCreationRequest request) throws Exception {
    String channelId = mandatoryParameterHelper.getChannelId();
    String clientId = mandatoryParameterHelper.getClientId();
    String requestId = mandatoryParameterHelper.getRequestId();
    String username = mandatoryParameterHelper.getUsername();
    String brand = request.getBrand();
    String brandCode = request.getBrandCode();
    String businessPartnerCode = request.getBusinessPartnerCode();

    GdnRestSingleResponse<BrandResponse> brandResponse = pcbFeign
        .filterByBrandName(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, brand, false, true);
    if (Objects.isNull(brandResponse) || Objects.isNull(brandResponse.getValue())) {
      BrandWipResponse brandWipResponse =
          getBrandWipResponseAndValidateBrandStatus(brandCode, brand, businessPartnerCode);
      request.setBrand(brandWipResponse.getBrandName());
      request.setBrandApprovalStatus(BrandApprovalStatus.DRAFT.name());
    } else {
      checkForProtectionBrandAndAuthorisation(request.getStoreId(), brandResponse, businessPartnerCode, channelId, clientId, requestId,
          username);
      request.setBrand(brandResponse.getValue().getBrandName());
      request.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    }
  }

  public void checkForProtectionBrandAndAuthorisation(String storeId,
      GdnRestSingleResponse<BrandResponse> brandResponse, String businessPartnerCode,
      String channelId, String clientId, String requestId, String username) {
    if (CommonUtils.validateProtectedBrand(brandResponse.getValue(), businessPartnerCode)) {
      SimpleBooleanResponse brandAuthResponse =
          productOutbound.authoriseProtectedBrand(storeId, channelId, clientId, requestId, username,
              brandResponse.getValue().getBrandCode(), businessPartnerCode);
      if (!brandAuthResponse.getResult()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND);
      }
    }
  }

  public BrandWipResponse getBrandWipResponseAndValidateBrandStatus(String brandCode, String brand,
      String businessPartnerCode) throws Exception {
    BrandWipResponse brandWipResponse;
    if (StringUtils.isNotEmpty(brandCode)) {
      GdnRestSingleResponse<BrandWipResponse> response =
          this.pcbFeign.getBrandWipDetail(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              brandCode);
      if (Objects.isNull(response) || !response.isSuccess() || Objects.isNull(
          response.getValue())) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "Brand Wip not found");
      }
      brandWipResponse = response.getValue();
    } else {
      brandWipResponse = brandWipRepository.findBrandWipByBrandNameAndBusinessPartnerCode(brand,
          businessPartnerCode);
    }
    if (Objects.isNull(brandWipResponse) || BrandApprovalStatus.REJECTED.name()
        .equals(brandWipResponse.getState())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "Brand is Rejected");
    }
    if (StringUtils.isNotEmpty(brandWipResponse.getBusinessPartnerCode())
        && !brandWipResponse.getBusinessPartnerCode().equals(businessPartnerCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "Brand is created by different business partner code");
    }
    return brandWipResponse;
  }

  private void getWholeSalePriceRequests(ProductCreationRequest request, Map<String, String> itemIdMap,
      Map<String, String> itemIdToGdnSkuMap, Map<String, String> itemIdToCodeMap,
      List<ProductItemWholesalePrice> productItemWholesalePriceList) throws Exception {
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      if (!CollectionUtils.isEmpty(productItemCreationRequest.getProductItemWholesalePriceRequests())) {
        String itemSku = itemIdToGdnSkuMap.get(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
        String itemCode = itemIdToCodeMap.get(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
        if (StringUtils.isNotBlank(itemSku) && StringUtils.isNotBlank(itemCode)) {
          ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
          productItemWholesalePrice.setItemSku(itemSku);
          productItemWholesalePrice
              .setProductItemId(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
          if(Objects.nonNull(productItemCreationRequest.getWholesalePriceActivated())) {
            productItemWholesalePrice
                .setWholesalePriceActivated(productItemCreationRequest.getWholesalePriceActivated());
          }
          productItemWholesalePrice.setPickupPointCode(productItemCreationRequest.getPickupPointId());
          productItemWholesalePrice.setItemCode(itemCode);
          productItemWholesalePrice.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
          productItemWholesalePrice.setWholesaleRules(
              mapperUtil.mapRequestToString(productItemCreationRequest.getProductItemWholesalePriceRequests()));
          productItemWholesalePriceList.add(productItemWholesalePrice);
        }
      }
    }
  }

  private void getWholeSalePriceRequestsAtL5(ProductCreationRequest request, Map<String, String> itemIdMap,
      Map<String, String> itemIdToGdnSkuMap, Map<String, String> itemIdToCodeMap,
      List<ProductItemWholesalePrice> productItemWholesalePriceList) throws Exception {
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
        if (!CollectionUtils.isEmpty(pickupPointCreateRequest.getProductItemWholesalePriceRequests())) {
          String itemSku = itemIdToGdnSkuMap.get(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
          String itemCode = itemIdToCodeMap.get(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
          if (StringUtils.isNotBlank(itemSku) && StringUtils.isNotBlank(itemCode)) {
            ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
            productItemWholesalePrice.setItemSku(itemSku);
            productItemWholesalePrice
                .setProductItemId(itemIdMap.get(productItemCreationRequest.getProductItemHashCode()));
            productItemWholesalePrice.setItemCode(itemCode);
            productItemWholesalePrice.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
            if (Objects.nonNull(pickupPointCreateRequest.getWholesalePriceActivated())) {
              productItemWholesalePrice
                  .setWholesalePriceActivated(pickupPointCreateRequest.getWholesalePriceActivated());
            }
            productItemWholesalePrice.setWholesaleRules(
                mapperUtil.mapRequestToString(pickupPointCreateRequest.getProductItemWholesalePriceRequests()));
            productItemWholesalePrice.setPickupPointCode(pickupPointCreateRequest.getPickupPointId());
            productItemWholesalePriceList.add(productItemWholesalePrice);
          }
        }
      }
    }
  }


  /**
   * Set hash code and original image flag on product creation
   *
   * @param image
   * @return
   */
  private Image setHashCodeAndOriginalImageFlag(Image image) {
    image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(image.getLocationPath()));
    image.setOriginalImage(Boolean.TRUE);
    return image;
  }
  /**
   * if product created internally, then need to activate while Directly while product creation
   * @param request
   */
  private void validateProductRequestForInternalUserOrSkipScreening(ProductCreationRequest request) {
    if (StringUtils.isEmpty(request.getBusinessPartnerCode())) {
      request.setBusinessPartnerCode(GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_CODE);
      request.setBusinessPartnerName(GdnBaseLookup.INTERNAL_BUSINESS_PARTNER_NAME);
      request.setActivated(Boolean.TRUE);
    } else if (isSkipScreeningSwitch) {
      request.setActivated(Boolean.TRUE);
    }
  }

  private ProductBusinessPartner convertProductBusinessPartnerRequestToProductBusinessPartner(
      ProductCreationRequest request, String productId, Boolean MPPFlowEnabled) {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
    productBusinessPartner.setActivated(false);
    productBusinessPartner.setProductId(productId);
    productBusinessPartner.setProductName(request.getName());
    productBusinessPartner.setBrand(request.getBrand());
    productBusinessPartner.setCategoryName(request.getCategoryName());
    productBusinessPartner.setBundleProduct(request.isBundleProduct());
    if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
      PreOrderRequest preOrderRequest = request.getPreOrder();
      productBusinessPartner.setPreOrder(preOrderRequest.getIsPreOrder());
      productBusinessPartner.setPreOrderType(preOrderRequest.getPreOrderType());
      productBusinessPartner.setPreOrderValue(preOrderRequest.getPreOrderValue());
      productBusinessPartner.setPreOrderDate(preOrderRequest.getPreOrderDate());
    }
    if (Objects.nonNull(request.getProductCategories().get(0).getCategory().getCategoryCode())) {
      productBusinessPartner.setCategoryCode(request.getProductCategories().get(0).getCategory().getCategoryCode());
    }
    productBusinessPartner.setBusinessPartnerId(request.getBusinessPartnerCode());
    productBusinessPartner.setGdnProductSku(request.getGdnProductSku());
    productBusinessPartner.setFbbActivated(request.getProductItemRequests().stream().anyMatch(
        productItemCreationRequest -> productItemCreationRequest.getPickupPoints().stream()
            .anyMatch(PickupPointCreateRequest::isFbbActivated)));
    int tempItemSku = 0;
    for (ProductItemCreationRequest productItemBusinessPartnerRequest : request
        .getProductItemRequests()) {
      tempItemSku = tempItemSku + 1;
      if (MPPFlowEnabled) {
        int minStock =
            Optional.ofNullable(productItemBusinessPartnerRequest.getPickupPoints().get(0).getMinimumStock()).orElse(0);
        for (PickupPointCreateRequest pickupPointCreateRequest : productItemBusinessPartnerRequest.getPickupPoints()) {
          ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
          BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
          BeanUtils.copyProperties(pickupPointCreateRequest, productItemBusinessPartner);
          productItemBusinessPartner.setPickupPointId(pickupPointCreateRequest.getPickupPointId());
          setCncFlagsForProductItem(pickupPointCreateRequest, productItemBusinessPartner);
          productItemBusinessPartner.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
          productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
          productItemBusinessPartner.setMinimumStock(minStock);
          productItemBusinessPartner.setGdnProductItemSku(String.valueOf(tempItemSku));
          productItemBusinessPartner.setFbbActive(pickupPointCreateRequest.isFbbActivated());
          productItemBusinessPartner.setCncDiscoverable(pickupPointCreateRequest.isCncDisplay());
          if (Objects.nonNull(pickupPointCreateRequest.getB2bFields())) {
            productItemBusinessPartner.setB2bBuyable(pickupPointCreateRequest.getB2bFields().isBuyable());
            productItemBusinessPartner.setB2bDiscoverable(pickupPointCreateRequest.getB2bFields().isDisplay());
            productItemBusinessPartner.setB2bManaged(pickupPointCreateRequest.getB2bFields().isManaged());
            productItemBusinessPartner.setB2bPrice(pickupPointCreateRequest.getB2bFields().getPrice());
          }
          if (!CollectionUtils.isEmpty(productItemBusinessPartnerRequest.getBundleRecipe())) {
            try {
              productItemBusinessPartner.setBundleRecipe(
                  objectMapper.writeValueAsString(productItemBusinessPartnerRequest.getBundleRecipe()));
            } catch (JsonProcessingException e) {
              log.error("Error while converting bundle recipe to json", e);
            }
          }
          productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
        }
      } else {
        ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
        BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
        productItemBusinessPartner.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
        productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
        productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
      }
    }
    if (!CollectionUtils.isEmpty(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest : request
          .getProductBusinessPartnerAttributes()) {
        ProductBusinessPartnerAttribute productBusinessPartnerAttribute =
            new ProductBusinessPartnerAttribute();
        BeanUtils.copyProperties(productBusinessPartnerAttributeRequest,
            productBusinessPartnerAttribute);
        productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
        productBusinessPartner.getProductBusinessPartnerAttributes()
            .add(productBusinessPartnerAttribute);
      }
    }
    productBusinessPartner.setFreeSample(request.isFreeSample());
    productBusinessPartner.setOff2OnChannelActive(request.isOff2OnChannelActive());
    productBusinessPartner.setOnline(request.isOnline());
    productBusinessPartner.setB2cActivated(request.isB2cActivated());
    productBusinessPartner.setB2bActivated(request.isB2bActivated());
    productBusinessPartner.setSizeChartCode(request.getSizeChartCode());
    return productBusinessPartner;
  }

  private void setCncFlagsForProductItem(PickupPointCreateRequest pickupPointCreateRequest,
      ProductItemBusinessPartner productItemBusinessPartner) {
    if (cncForWarehouseFeatureSwitch) {
      if (!pickupPointCreateRequest.isCncBuyable()
          && !pickupPointCreateRequest.isCncDisplay()) {
        productItemBusinessPartner.setCncBuyable(pickupPointCreateRequest.isCncActive());
        productItemBusinessPartner.setCncDiscoverable(pickupPointCreateRequest.isCncActive());
      }
    } else {
      productItemBusinessPartner.setCncActivated(pickupPointCreateRequest.isCncActive());
      productItemBusinessPartner.setCncBuyable(pickupPointCreateRequest.isCncActive());
      productItemBusinessPartner.setCncDiscoverable(pickupPointCreateRequest.isCncActive());
    }
  }
}
