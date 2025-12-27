package com.gdn.mta.bulk.service;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.QueueHistoryResultDTO;
import com.gdn.mta.bulk.dto.WarningMessage;
import com.gdn.mta.bulk.dto.WarningMessageDTO;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.mta.bulk.dto.product.AllowedValueDtoResponse;
import com.gdn.mta.bulk.dto.product.AttributeMapDto;
import com.gdn.mta.bulk.dto.product.CreateProductV2Response;
import com.gdn.mta.bulk.dto.product.ProductItemRequestDto;
import com.gdn.mta.bulk.dto.product.ProductItemV2Request;
import com.gdn.mta.bulk.dto.product.ProductV2Request;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.repository.pcb.ProductAttributeRepository;
import com.gdn.mta.bulk.util.CreateProductV2Util;
import com.gdn.mta.bulk.util.FileManager;
import com.gdn.mta.bulk.util.ImageUtil;
import com.gdn.mta.bulk.util.LoggerTemplateUtil;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ProductImageValidator;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.mta.model.enums.ProductType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.common.collect.Lists;

/**
 * Service to process create product flow 1 request from MTA-API version 2
 *
 * @author agie.falah
 */
@Service("CreateProductV2ServiceBean")
public class CreateProductV2ServiceBean
    implements GeneralProcessorService<ProductV2Request, Void, Void> {

  private static final String URL = "url";
  private static final String DELIMITER = "\\|";

  @Autowired
  private ProductRepository productRepository;
  @Autowired
  private BusinessPartnerRepository bpRepository;
  @Autowired
  private CategoryRepository categoryRepository;
  @Autowired
  private GeneratorRepository generatorRepository;
  @Autowired
  private SystemParameter systemParameter;
  @Autowired
  private KafkaPublisher kafkaProducer;
  @Autowired
  private TrackerService trackerService;
  @Autowired
  private ProductImageValidator imageValidator;
  @Autowired
  private ImageUtil imageUtil;
  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private ProductAttributeRepository productAttributeRepository;
  @Autowired
  private FileStorageService fileStorageService;
  @Autowired
  private PickupPointService pickupPointService;
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("#{${allowed.image.formats.map}}")
  private Map<String, String> allowedImageFormatsMap;

  private static final Logger LOGGER = LoggerFactory.getLogger(CreateProductV2ServiceBean.class);

  @Override
  public Void process(ProductV2Request request) throws Exception {
    String productCode = StringUtils.EMPTY;
    ProductCreationRequest reqDto;
    QueueHistoryResultDTO result;
    GdnBaseRestResponse pbpResponse = new GdnBaseRestResponse();
    pbpResponse.setRequestId(request.getRequestId());

    try {
      processUrlImages(request);
      Set<String> uniqueImageList = this.validateImageTmp(request);
      reqDto = this.buildProductCreationRequest(request);
      this.fileStorageService.moveTmpImageToProductImage(request.getRequestId(), uniqueImageList,
          reqDto.getProductCode());
      productCode = reqDto.getProductCode();
      reqDto.setProductCreationType(ProductCreationType.FLOW1_API);
      pbpResponse =
          productRepository.createProduct(request.getRequestId(), request.getUsername(), reqDto);
      result = this.getQueueHistoryResult(pbpResponse.getErrorMessage(), pbpResponse.getErrorCode(),
          pbpResponse.getRequestId(), pbpResponse.isSuccess(), request.getBpCode(),
          this.buildResultValue(request, reqDto.getProductCode()), request.getWarningMessage());
      LOGGER.info(LoggerTemplateUtil.generateAuditMtaApiInfo(Constant.CREATE_PRD_API_FEATURE_NAME,
          request.getRequestId(), request.getUsername(), request.getBpCode(), productCode));
      trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
          TrackerConstants.CREATE_FLOW1_API_V2, TrackerConstants.HYPHEN,
          TrackerConstants.SUCCESS, request.getUsername());
    } catch (Exception e) {
      LOGGER.error(LoggerTemplateUtil.generateAuditMtaApiError(Constant.CREATE_PRD_API_FEATURE_NAME,
          request.getRequestId(), request.getUsername(), request.getBpCode(), productCode,
          e.getMessage()), e);
      result = this.getQueueHistoryResult(e.getMessage(), pbpResponse.getErrorCode(),
          pbpResponse.getRequestId(), pbpResponse.isSuccess(), request.getBpCode(),
          this.buildResultValue(request, productCode), request.getWarningMessage());
    }

    File sourceImage = new File(systemParameter.getMtaApiTmpImage() + File.separator
        + request.getRequestId() + File.separator);
    if (sourceImage.exists()) {
      FileManager.deleteDirectory(sourceImage.getAbsolutePath());
    }
    kafkaProducer.send(kafkaTopicProperties.getBulkCreateProductV2QueueFeedEvent(),
        result);
    return null;
  }

  private void processUrlImages(ProductV2Request productV2Request) throws Exception {
    for (ProductItemV2Request itemRequest : productV2Request.getProductItems()) {
      for (int i = 0; i < itemRequest.getImages().size(); i++) {
        if (itemRequest.getImages().get(i).startsWith(URL)) {
          String[] imageInfo = itemRequest.getImages().get(i).split(DELIMITER);
          String imageType = putImageFromUrl(imageInfo[1],
              productV2Request.getRequestId() + File.separator + imageInfo[2]);
          itemRequest.getImages().set(i, imageInfo[2] + "." + imageType);
        }
      }
    }
  }

  private String putImageFromUrl(String urlPath, String generatedImagePath) throws Exception {
    URL url = new URL(urlPath);
    URLConnection urlConnection = url.openConnection();
    urlConnection.setConnectTimeout(systemParameter.getDownloadImageUrlConnectTimeout());
    urlConnection.setReadTimeout(systemParameter.getDownloadImageUrlReadTimeout());
    InputStream input = urlConnection.getInputStream();
    byte[] imageBytes = IOUtils.toByteArray(input);
    String imageType = imageUtil.validateAndGetImageType(imageBytes, allowedImageFormatsMap);
    FileManager.createFile(imageBytes, systemParameter.getMtaApiTmpImage() + File.separator
        + generatedImagePath + "." + imageType);
    return imageType;
  }

  private Set<String> validateImageTmp(ProductV2Request request) {
    return request.getProductItems()
        .stream()
        .flatMap(item -> item.getImages().stream())
        .distinct()
        .map(imagePath -> validateImagePath(imagePath, request.getRequestId()))
        .collect(Collectors.toSet());
  }
  
  private String validateImagePath(String imagePath, String requestId) {
    File sourceImage = new File(systemParameter.getMtaApiTmpImage() + File.separator
        + requestId + File.separator + imagePath);
    imageValidator.validateImages(sourceImage);
    return imagePath;
  }

  private String buildResultValue(ProductV2Request request, String productCode) throws JsonProcessingException {
    CreateProductV2Response createProductV2Response = this.buildCreateProductV2Response(
        request, productCode);
    return this.objectMapper.writeValueAsString(createProductV2Response);
  }

  private CreateProductV2Response buildCreateProductV2Response(ProductV2Request request,
      String productCode) {
    List<String> sellerSkus = request.getProductItems()
        .stream()
        .map(ProductItemV2Request::getMerchantSku)
        .collect(Collectors.toList());
    CreateProductV2Response createProductV2Response = new CreateProductV2Response();
    createProductV2Response.setProductCode(productCode);
    createProductV2Response.setProductName(request.getName());
    createProductV2Response.setMerchantSkuList(sellerSkus);
    createProductV2Response.setSellerSkus(sellerSkus);
    return createProductV2Response;
  }

  /**
   * Generate queue feed information response, that will be consumed by MTA-API
   *
   * @param errorMsg
   * @param errorCode
   * @param requestId
   * @param isSuccess
   * @param bpCode
   * @param productCode
   * @return
   */
  private QueueHistoryResultDTO getQueueHistoryResult(String errorMsg, String errorCode,
      String requestId, boolean isSuccess, String bpCode, String productCode,
      List<WarningMessage> warningMessages) {
    QueueHistoryResultDTO result = new QueueHistoryResultDTO();
    result.setErrorMessage(errorMsg);
    result.setErrorCode(errorCode);
    result.setRequestId(requestId);
    result.setSuccess(isSuccess);
    result.setMerchantCode(bpCode);
    result.setValue(productCode);
    result.setWarningMessage(appendWarningMessageFromPBP(errorMsg, isSuccess, warningMessages));
    return result;
  }

  private List<WarningMessageDTO> appendWarningMessageFromPBP(String errorMsg, boolean isSuccess,
      List<WarningMessage> warningMessages) {
    if (isSuccess && Objects.nonNull(errorMsg)) {
      WarningMessage pbpWarningMsg =
          WarningMessage.builder()
              .message(errorMsg)
              .build();
      if (Objects.isNull(warningMessages)) {
        warningMessages = new ArrayList<>();
      }
      warningMessages.add(pbpWarningMsg);
    }
    return objectMapper.convertValue(warningMessages, new TypeReference<List<WarningMessageDTO>>() {
    });
  }

  /**
   * Build final request object that will be sent to PBP to create product flow 1
   *
   * @param request
   * @return
   * @throws Exception
   */
  private ProductCreationRequest buildProductCreationRequest(ProductV2Request request)
      throws Exception {
    ProfileResponse bp = bpRepository.filterByBusinessPartnerCodeV2(request.getStoreId(), request.getBpCode());
    List<PickupPointResponse> pickupPointResponseList = this.pickupPointService
      .getPickupPointSummaryFilter(0,
        PickupPointFilterRequest.builder().businessPartnerCode(request.getBpCode())
          .codes(new HashSet<>(Collections.singletonList(request.getPickupPointCode()))).build());
    CreateProductV2Util.validatePickupPoint(bp, pickupPointResponseList,
      request.getPickupPointCode());
    CategoryDetailResponse category =
        this.getCategoryDetail(request.getStoreId(), request.getCategoryCode());
    if(Objects.isNull(category)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          " Invalid category for " + request.getCategoryCode());
    }
    BrandResponse brandDetail =
        this.getBrandDetail(request.getUsername(), request.getRequestId(), request.getBrand());
    if(Objects.isNull(brandDetail)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          " Invalid brand value for " + request.getBrand());
    }
    Map<String, List<AllowedValueDtoResponse>> allowedValues = 
        this.getPredefinedAndDefiningAllowedAttributeValue(category, request);
    AttributeMapDto attrMap = CreateProductV2Util.populateCatAttr(category);

    GdnRestSimpleResponse<String> generatedProductCode =
        productRepository.generateProductCode(request.getRequestId(), request.getUsername());

    ProductCreationRequest productRequest = new ProductCreationRequest();
    productRequest.setStoreId(request.getStoreId());
    productRequest.setCreatedBy(request.getUsername());
    productRequest.setCreatedDate(new Date());
    productRequest.setBusinessPartnerId(bp.getId());
    productRequest.setBusinessPartnerCode(bp.getBusinessPartnerCode());
    productRequest.setBusinessPartnerName(bp.getCompany().getBusinessPartnerName());
    productRequest.setCategoryName(category.getName());
    productRequest.setProductItemRequests(CreateProductV2Util.buildProductItemRequest(request,
        buildProductItemRequestDto(attrMap, allowedValues, generatedProductCode.getValue(), request)));
    productRequest.setProductBusinessPartnerAttributes(
            CreateProductV2Util.buildPrdBPAttrReq(attrMap.getNonDefAttrMapSkuValue(), request));
    productRequest.setProductCode(generatedProductCode.getValue());
    productRequest.setName(request.getName());

    this.setProductDimension(request, productRequest);
    productRequest.setDescription(request.getDescription());
    productRequest.setLongDescription(request.getDescription());
    productRequest.setBrand(request.getBrand());
    productRequest.setUniqueSellingPoint(this.convertByteToString(request.getUniqueSellingPoint()));
    productRequest.setUom("PC");
    productRequest.setProductCategories(CreateProductV2Util.buildProductCategoryReq(category));
    productRequest.setProductAttributes(
        CreateProductV2Util.buildProductAttributeRequest(request, attrMap, allowedValues));
    productRequest.setActivated(false);
    productRequest.setViewable(false);
    productRequest.setProductStory(this.convertByteToString(request.getProductStory()));
    productRequest.setSpecificationDetail("-");
    productRequest.setUrl(request.getUrl());
    productRequest.setPromoSKU(false);

    productRequest.setBrandCode(brandDetail.getBrandCode());
    productRequest.setBrandApprovalStatus(Constant.BRAND_APPROVAL_STATUS_APPROVE);

    return productRequest;
  }

  private ProductItemRequestDto buildProductItemRequestDto(AttributeMapDto attrMap,
      Map<String, List<AllowedValueDtoResponse>> allowedValues, String productCode, 
      ProductV2Request request) throws Exception {
    return ProductItemRequestDto.builder()
        .defAttrMap(attrMap.getDefAttrMap())
        .nonDefAttrMap(attrMap.getNonDefAttrMap())
        .allowedValue(allowedValues)
        .productCode(productCode)
        .nonDefiningItemAttributeDetails(getAttributeDetailForProductNonDefiningAttributes(request))
        .build();
  }

  private List<AttributeResponse> getAttributeDetailForProductNonDefiningAttributes(ProductV2Request productV2Request)
      throws Exception {
    Set<String> attributeCodes = productV2Request.getProductItems()
        .stream()
        .map(productItemV2Request -> 
            Optional.ofNullable(productItemV2Request.getNonDefiningItemAttributes())
                .orElseGet(HashMap::new))
        .map(Map::keySet)
        .flatMap(Set::stream)
        .collect(Collectors.toSet());
    if (!attributeCodes.isEmpty()) {
      return productAttributeRepository
          .getAttributeDetailByAttributeCodes(productV2Request.getRequestId(), productV2Request.getUsername(),
              Lists.newArrayList(attributeCodes));
    }
    return new ArrayList<>();
  }

  private String convertByteToString(byte[] data) {
    if (data == null) {
      return null;
    } else {
      return new String(data);
    }
  }

  private BrandResponse getBrandDetail(String username, String requestId, String brandName)
      throws Exception { 
    return categoryRepository.getBrandDetail(username, requestId, brandName);
  }

  /**
   * Set product dimention based on productType value
   *
   * @param request
   * @param result
   * @throws Exception
   */
  private void setProductDimension(ProductV2Request request, ProductCreationRequest result)
      throws Exception {
    if (request.getProductType() == ProductType.BOPIS.getCode()) {
      result.setLength(0.0);
      result.setWidth(0.0);
      result.setHeight(0.0);
      result.setWeight(0.0);
      result.setShippingWeight(0.0);
    } else {
      Double shippingWeight = this.generateShippingWeight(request);
      result.setLength(request.getLength());
      result.setWidth(request.getWidth());
      result.setHeight(request.getHeight());
      result.setWeight(CreateProductV2Util.getKilogramFromGram(request.getWeight()));
      result.setShippingWeight(shippingWeight);
    }
  }

  /**
   * Generate shipping weight to X-Shipping
   *
   * @return
   * @throws Exception
   */
  private Double generateShippingWeight(ProductV2Request request) throws Exception {
    GenerateShippingWeightRequest shippingRequest = new GenerateShippingWeightRequest();
    shippingRequest.setLength(request.getLength());
    shippingRequest.setWidth(request.getWidth());
    shippingRequest.setHeight(request.getHeight());
    shippingRequest.setWeight(CreateProductV2Util.getKilogramFromGram(request.getWeight()));
    shippingRequest.setCategoryCode(request.getCategoryCode());
    GenerateShippingWeightResponse response =
        this.generatorRepository.generateShippingWeight(shippingRequest);

    return response.getShippingWeight();
  }

  /**
   * Get allowed attribute value for predefined and defining attribute
   *
   * @param category
   * @param productReq
   * @return
   * @throws Exception
   */
  private Map<String, List<AllowedValueDtoResponse>> getPredefinedAndDefiningAllowedAttributeValue(
      CategoryDetailResponse category, ProductV2Request productReq) throws Exception {
    Map<String, List<String>> clientAttrMap = 
        CreateProductV2Util.populateClientAttributeIntoMap(productReq);
    List<AllowedAttributeValueDtoRequest> request = category.getCategoryAttributes()
        .stream()
        .filter(catAttr -> !AttributeType.DESCRIPTIVE_ATTRIBUTE.toString()
            .equals(catAttr.getAttribute().getAttributeType()) && !catAttr.isMarkForDelete())
        .map(catAttr -> createAllowedAttributeValueDtoRequest(catAttr, clientAttrMap, productReq))
        .collect(Collectors.toList());
    List<AllowedAttributeValueDtoResponse> response =
        categoryRepository.getPredefinedAndDefiningAllowedAttributeValue(productReq.getUsername(),
            productReq.getRequestId(), productReq.getStoreId(), request);
    if(Objects.nonNull(clientAttrMap.get(productReq.getFamilyColorCode()))){
      validateFamilyColorValues(productReq, clientAttrMap, response);
    }
    return response.stream()
        .collect(Collectors.toMap(
            AllowedAttributeValueDtoResponse::getAttributeCode,
            AllowedAttributeValueDtoResponse::getAllowedValue));
  }

  private void validateFamilyColorValues(ProductV2Request productReq,
      Map<String, List<String>> clientAttrMap, List<AllowedAttributeValueDtoResponse> response)
      throws ApplicationRuntimeException {
    Optional<AllowedAttributeValueDtoResponse> familyColorAllowedValues =
        response.stream()
            .filter(allowedAttributeValueDto ->
                allowedAttributeValueDto.getAttributeCode().equals(productReq.getFamilyColorCode())
                    && CollectionUtils.isNotEmpty(allowedAttributeValueDto.getAllowedValue()))
            .findFirst();
    if(!familyColorAllowedValues.isPresent()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          " The attribute's values is not valid for attribute: " + productReq.getFamilyColorCode());
    }
    Optional<List<String>> invalidValues =
        familyColorAllowedValues.map(allowedAttributeValueDto ->
            allowedAttributeValueDto.getAllowedValue()
              .stream()
              .map(AllowedValueDtoResponse::getValue)
              .collect(Collectors.toList()))
        .filter(x -> Objects.nonNull(clientAttrMap.get(productReq.getFamilyColorCode())))
        .map(allowedValues -> clientAttrMap.get(productReq.getFamilyColorCode())
            .stream()
            .filter(attributeValue -> !allowedValues.contains(attributeValue))
            .collect(Collectors.toList()));

    if(invalidValues.isPresent() && CollectionUtils.isNotEmpty(invalidValues.get())){
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          " The attribute's value is not valid for attribute: " + productReq.getFamilyColorCode() + " "
              + "with value: " + invalidValues.get());
    }
  }

  private AllowedAttributeValueDtoRequest createAllowedAttributeValueDtoRequest(
      CategoryAttributeResponse catAttr, Map<String, List<String>> clientAttrMap, 
      ProductV2Request productReq) {
    List<String> attributeValues;
    if (CreateProductV2Util.ATTR_NAME_BRAND.equals(catAttr.getAttribute().getName())) {
      attributeValues = Collections.singletonList(productReq.getBrand());
    } else {
      attributeValues = clientAttrMap.get(catAttr.getAttribute().getAttributeCode());
    }
    return new AllowedAttributeValueDtoRequest(
        catAttr.getAttribute().getAttributeCode(),
        catAttr.getAttribute().getAttributeType(), 
        Optional.ofNullable(attributeValues)
            .orElseGet(ArrayList::new));
  }

  /**
   * Get category detail from PCB, include category' attributes and allowed values
   *
   * @param storeId
   * @param categoryCode
   * @return
   * @throws Exception
   */
  private CategoryDetailResponse getCategoryDetail(String storeId, String categoryCode)
      throws Exception {
    return categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(storeId, categoryCode);
  }
}
