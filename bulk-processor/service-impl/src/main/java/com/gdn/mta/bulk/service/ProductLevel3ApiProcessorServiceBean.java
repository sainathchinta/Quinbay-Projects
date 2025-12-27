package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerClient;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.models.MtaApiBaseQueueResult;
import com.gdn.mta.bulk.models.MtaApiCreateProductQueueResult;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.util.StreamUtils;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.rest.web.request.ImageApiRequest;
import com.gdn.x.mta.rest.web.request.ProductApiGeneralAttributeRequest;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableSet;

@Service("ProductLevel3ApiProcessorServiceBean")
public class ProductLevel3ApiProcessorServiceBean implements
    GeneralProcessorService<ProductApiRequest, Void, Void> {

  private static final Logger LOG = LoggerFactory.getLogger(ProductLevel3ApiProcessorServiceBean.class);
  public static final String DEFAULT_STORE_ID = "10001";
  public static final String VALUE_SEPARATOR = "|";
  public static final GdnRestListRequest ALL_REQUEST = new GdnRestListRequest(0, Integer.MAX_VALUE);
  public static final String ERROR_CODE_CREATE_EXCEPTION = "error.mta.bulk.create.product";
  public static final String ERROR_MESSAGE_CREATE_EXCEPTION = "error while creating product";
  public static final String ERROR_MESSAGE_BRAND_NOT_FOUND = "error while creating product: Brand Not Found";
  public static final String ERROR_MESSAGE_DEFINING_NOT_FOUND = "error while creating product: Variant Not Found";
  public static final String ERROR_MESSAGE_CATEGORY_NOT_FOUND = "error while creating product: Category Not Found";
  public static final String ERROR_MESSAGE_INVALID_PRODUCT_TYPE_REGULAR_WEIGHT_THRESHOLD =
      "shipping weight of product more than threshold (regular product type)";
  public static final int BIG_PRODUCT = 2;
  private static final Set<String> FEATURES = ImmutableSet
      .of(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString(),
          AttributeType.PREDEFINED_ATTRIBUTE.toString());
  public static final String BRAND_ATTRIBUTE_NAME = "Brand";
  
  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private TrackerService trackerService;
  
  @Autowired
  private GeneratorRepository generatorRepository;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;
  
  @Override
  public Void preProcess(ProductApiRequest data, Map<String, String> args) throws Exception {
    kafkaProducer.send(kafkaTopicProperties.getCreateProductEvent(), data);
    return null;
  }

  @Override
  public Void process(ProductApiRequest productApiRequest) throws Exception {
    MtaApiBaseQueueResult result = null;
    LoggerAttributeModel loggerModel = new LoggerAttributeModel(this, "process", productApiRequest.getMerchantCode(), 
        productApiRequest.getUsername(), productApiRequest.getRequestId(), null, LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(),
        productApiRequest.getProductName(), null);
    try {
      Double shippingWeight =
          this.generateShippingWeight(productApiRequest.getLength(), productApiRequest.getWidth(),
              productApiRequest.getHeight(), this.getKilogramFromGram(productApiRequest.getWeight()), productApiRequest.getCategoryCode());

      ProfileResponse bp =
          businessPartnerRepository.filterByBusinessPartnerCodeV2(
              DEFAULT_STORE_ID, productApiRequest.getMerchantCode());
      CreateProductRequest productCollectionRequest = prepareSaveProduct(productApiRequest, bp);
      // save product level 1
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, productApiRequest.getUsername());
      MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, productApiRequest.getRequestId());
      String productCode = "";
      try{
        if(bp.isAllCategory()){
      	  productCode = productRepository.create(productCollectionRequest);
        }else{
      	  productCode = productRepository.createViaApi(productCollectionRequest);
        }
      } catch(Exception e){
        LOG.error(LoggerStandard.convertErrorTemplate(this, "process.productRepository.create", loggerModel, e, null), e);
        throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, 
            "Create master product error: " + e.getMessage(), e);
      }
      
      ProductDetailResponse productDetailResponse =
          productRepository.findProductDetailByProductCodeAndMarkForDeleteFalse(productCode);
      ProductRequest productRequest = prepareUpdateProduct(productDetailResponse, productApiRequest, bp);
      
      try{
        // update product level 1 and item product
        productRepository.update(productRequest);
      } catch(Exception e){
        LOG.error(LoggerStandard.convertErrorTemplate(this, "process.productRepository.update", loggerModel, e, null), e);
        throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, 
            "Create product variant error: " + e.getMessage(), e);
      }
      
      String pbpId = null;
      try{
        ProductBusinessPartnerRequest productBusinessPartnerRequest =
            prepareSaveProductBusinessPartner(productRequest, productApiRequest, bp);
        pbpId = productBusinessPartnerRepository.saveWithActivatedFalseReturnId(productBusinessPartnerRequest);
      } catch(Exception e){
        LOG.error(LoggerStandard.convertErrorTemplate(this, "process.productBusinessPartnerRepository.saveWithActivatedFalseReturnId", loggerModel, e, null), e);
        throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, 
            "Create product business partner error: " + e.getMessage(), e);
      }
      
      if(productApiRequest.getStock() > 0) {
        trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API,
            TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, productApiRequest.getCategoryCode());
      }
      trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API,
          TrackerConstants.SUBMIT, TrackerConstants.SUCCESS, productApiRequest.getCategoryCode());
      ProductBusinessPartnerResponse productBusinessPartnerResponse =
          productBusinessPartnerRepository.getProductBusinessPartnerById(pbpId);
      String gdnPrdCode = null;
      if(productBusinessPartnerResponse!=null){
        gdnPrdCode = productBusinessPartnerResponse.getGdnProductSku();
      }
      result =
          getQueueHistoryResult(null, null, true, productApiRequest, 
              productCode,
              gdnPrdCode);
    } catch (Exception e) {
      trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_API,
          "-", TrackerConstants.FAILED, productApiRequest.getCategoryCode());
      
      LOG.error(LoggerStandard.convertErrorTemplate(this, "process", loggerModel, e, null), e);
      GdnBaseRestResponse response =
          new GdnBaseRestResponse(e.getMessage(), ERROR_CODE_CREATE_EXCEPTION,
              false, productApiRequest.getRequestId());
      result =
          getQueueHistoryResult(
              response.getErrorMessage(),
              response.getErrorCode(),
              false,
              productApiRequest,
              null, 
              null);
    }
    kafkaProducer.send(kafkaTopicProperties.getBulkProcessImageResponseEvent(), result);
    return null;
  }
  
  private MtaApiBaseQueueResult getQueueHistoryResult(String errorMessage, String errorCode, 
      boolean success, ProductApiRequest queueObj, 
      String productCode, String gdnSku) throws Exception {

    MtaApiCreateProductQueueResult value = new MtaApiCreateProductQueueResult();
    value.setType("createProduct");
    value.setProductName(queueObj.getProductName());
    value.setProductCode(productCode);
    value.setGdnSku(gdnSku);
    value.setMerchantSku(queueObj.getMerchantSku());
    
    MtaApiBaseQueueResult result = new MtaApiBaseQueueResult();
    result.setRequestId(queueObj.getRequestId());
    result.setMerchantCode(queueObj.getMerchantCode());
    result.setErrorMessage(errorMessage);
    result.setErrorCode(errorCode);
    result.setSuccess(success);
    result.setValue(objectMapper.writeValueAsString(value));
    
    return result;
  }
  
  private ProductRequest prepareUpdateProduct(ProductDetailResponse productDetailResponse,
      ProductApiRequest productApiRequest, ProfileResponse bp) throws Exception {
    ProductRequest productRequest = productRequestBuilder(productDetailResponse, productApiRequest);
    productRequest.setId(productDetailResponse.getId());
    productRequest.setStoreId(productDetailResponse.getStoreId());
    productRequest.setUpdatedDate(new Date());
    productRequest.setUpdatedBy(bp.getCompany().getBusinessPartnerName());
    productRequest.setShippingWeight(generateProductShippingWeight(productRequest));
    return productRequest;
  }
  
  private CreateProductRequest prepareSaveProduct(ProductApiRequest productApiRequest,
      ProfileResponse bp) throws Exception {
    CreateProductRequest productCollectionRequest = productCollectionRequestBuilder(productApiRequest);
    productCollectionRequest.setCreatedDate(new Date());
    productCollectionRequest.setCreatedBy(bp.getCompany().getBusinessPartnerName());
    productCollectionRequest.setUpdatedDate(new Date());
    productCollectionRequest.setUpdatedBy(bp.getCompany().getBusinessPartnerName());
    productCollectionRequest.setBusinessPartnerCode(productApiRequest.getMerchantCode());
    productCollectionRequest.setBusinessPartnerName(bp.getCompany().getBusinessPartnerName());
    productCollectionRequest.setShippingWeight(0D);
    return productCollectionRequest;
  }
  
  private ProductBusinessPartnerRequest prepareSaveProductBusinessPartner(ProductRequest productRequest, 
      ProductApiRequest productApiRequest, ProfileResponse bp) throws Exception {
    ProductBusinessPartnerRequest productBusinessPartnerRequest = new ProductBusinessPartnerRequest();
    productBusinessPartnerRequest.setStoreId(DEFAULT_STORE_ID);
    productBusinessPartnerRequest.setCreatedBy(bp.getCompany().getName());
    productBusinessPartnerRequest.setCreatedDate(new Date());
    productBusinessPartnerRequest.setBusinessPartnerId(bp.getBusinessPartnerCode());
    productBusinessPartnerRequest.setProductId(productRequest.getId());
    productBusinessPartnerRequest.setProductName(productRequest.getName());
    productBusinessPartnerRequest.setCategoryName(productRequest.getProductCategories().get(0).getCategory().getName());
    productBusinessPartnerRequest.setBrand(productRequest.getBrand());
    List<ProductItemBusinessPartnerRequest> productItemBusinessPartnerRequestList = new ArrayList<ProductItemBusinessPartnerRequest>();
    for (ProductItemRequest productItemRequest : productRequest.getProductItems()) {
      ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest = new ProductItemBusinessPartnerRequest();
      productItemBusinessPartnerRequest.setStoreId(DEFAULT_STORE_ID);
      productItemBusinessPartnerRequest.setProductItemId(productItemRequest.getId());
      productItemBusinessPartnerRequest.setSaleStartDate(null);
      productItemBusinessPartnerRequest.setSaleEndDate(null);
      productItemBusinessPartnerRequest.setPickupPointId(productApiRequest.getPickupPointCode());
      productItemBusinessPartnerRequest.setProductType(productApiRequest.getTipePenanganan());
      productItemBusinessPartnerRequest.setPrice(productApiRequest.getPrice());
      productItemBusinessPartnerRequest.setSalePrice(productApiRequest.getSalePrice());
      productItemBusinessPartnerRequest.setStock(productApiRequest.getStock());
      productItemBusinessPartnerRequest.setMinimumStock(productApiRequest.getMinimumStock());
      productItemBusinessPartnerRequest.setDisplay(productApiRequest.isDisplay());
      productItemBusinessPartnerRequest.setBuyable(productApiRequest.isBuyable());
      productItemBusinessPartnerRequest.setMerchantSku(productApiRequest.getMerchantSku());
      productItemBusinessPartnerRequestList.add(productItemBusinessPartnerRequest);
    }
    productBusinessPartnerRequest.setProductItemBusinessPartners(productItemBusinessPartnerRequestList);
    return productBusinessPartnerRequest;
  }
  
  private ProductRequest productRequestBuilder(ProductDetailResponse productDetailResponse,
      ProductApiRequest productApiRequest) throws Exception {
    ProductRequest productRequest = new ProductRequest();
    productRequest.setVersion(productDetailResponse.getVersion());
    productRequest.setProductCode(productDetailResponse.getProductCode());
    productRequest.setName(productDetailResponse.getName());
    productRequest.setLength(productDetailResponse.getLength());
    productRequest.setWidth(productDetailResponse.getWidth());
    productRequest.setHeight(productDetailResponse.getHeight());
    productRequest.setWeight(productDetailResponse.getWeight());
    productRequest.setDescription(productDetailResponse.getDescription());
    productRequest.setLongDescription(productDetailResponse.getLongDescription());
    productRequest.setBrand(productDetailResponse.getBrand());
    productRequest.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    productRequest.setUom(productDetailResponse.getUom());
    productRequest.setProductStory(productDetailResponse.getProductStory());
    productRequest.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    productRequest.setUrl(productDetailResponse.getUrl());
    List<ProductCategoryRequest> productCategoryRequestList = new ArrayList<ProductCategoryRequest>();
    for (ProductCategoryResponse productCategoryResponse : productDetailResponse.getProductCategoryResponses()) {
      CategoryDetailResponse categoryDetailResponse =
          categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(DEFAULT_STORE_ID,
              productCategoryResponse.getCategory().getCategoryCode());
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      CatalogRequest catalogRequest = new CatalogRequest();
      BeanUtils.copyProperties(productCategoryResponse.getCategory(), categoryRequest);
      BeanUtils.copyProperties(categoryDetailResponse.getCatalog(), catalogRequest);
      productCategoryRequest.setCategory(categoryRequest);
      productCategoryRequest.getCategory().setCatalog(catalogRequest);
      productCategoryRequestList.add(productCategoryRequest);
    }
    productRequest.setProductCategories(productCategoryRequestList);
    List<ProductAttributeRequest> productAttributeRequestList = new ArrayList<ProductAttributeRequest>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      BeanUtils.copyProperties(productAttributeResponse, productAttributeRequest, "attribute", "productAttributeValues");
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttributeResponse.getAttribute(), attributeRequest, "attributeType");
      List<ProductAttributeValueRequest> productAttributeValueRequestList = new ArrayList<ProductAttributeValueRequest>();
      for(ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()){
        ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
        PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest = new PredefinedAllowedAttributeValueRequest();
        AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
        BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValueRequest);
        if (AttributeType.PREDEFINED_ATTRIBUTE.toString().equalsIgnoreCase(
            productAttributeResponse.getAttribute().getAttributeType())) {
          attributeRequest.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE);
          BeanUtils.copyProperties(productAttributeValueResponse.getPredefinedAllowedAttributeValue(), 
              predefinedAllowedAttributeValueRequest);
        } else if (AttributeType.DEFINING_ATTRIBUTE.toString().equalsIgnoreCase(
            productAttributeResponse.getAttribute().getAttributeType())) {
          attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE);
          BeanUtils.copyProperties(productAttributeValueResponse.getAllowedAttributeValue(), 
              allowedAttributeValueRequest);
        } else {
          attributeRequest.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
        }
        productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
        productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
        productAttributeValueRequestList.add(productAttributeValueRequest);
      }
      productAttributeRequest.setAttribute(attributeRequest);
      productAttributeRequest.getProductAttributeValues().addAll(productAttributeValueRequestList);
      productAttributeRequestList.add(productAttributeRequest);
    }
    productRequest.setProductAttributes(productAttributeRequestList);
    List<Image> images = new ArrayList<Image>();
    for(ImageApiRequest imageApi : productApiRequest.getImages()){
      Image image = new Image();
      image.setLocationPath(imageApi.getLocationPath());
      image.setSequence(imageApi.getSequence());
      images.add(image);
    }
    productRequest.setImages(images);
    productRequest.setProductItems(new ArrayList<ProductItemRequest>());
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItemResponse, productItemRequest, "upcCode", "images", "productItemAttributeValueResponses");
      if (StringUtils.isEmpty(productApiRequest.getUpcCode())) {
        productItemRequest.setUpcCode(productRepository.generateBarcode(UUID.randomUUID().toString()));
      } else {
        productItemRequest.setUpcCode(productApiRequest.getUpcCode());
      }
      productItemRequest.setImages(productDetailResponse.getImages());
      productItemRequest.setProductItemAttributeValues(Collections
          .<ProductItemAttributeValueRequest>emptyList());
      productRequest.getProductItems().add(productItemRequest);
    }
    return productRequest;
  }
  
  private List<ProductApiGeneralAttributeRequest> copyProductVariationtoFeature(
      List<ProductApiGeneralAttributeRequest> featureList,
      List<ProductApiGeneralAttributeRequest> variationList){
    
    List<ProductApiGeneralAttributeRequest> combinedAttribute = new ArrayList<ProductApiGeneralAttributeRequest>();
    for(ProductApiGeneralAttributeRequest attribute : featureList){
      combinedAttribute.add(attribute);
    }
	  
    for(ProductApiGeneralAttributeRequest attribute : variationList){
      combinedAttribute.add(attribute);
    }
    return combinedAttribute;
  }
  
  private void validateProductAttributeFeatures(ProductApiRequest request,
      List<CategoryAttributeResponse> categoryAttributeResponses) throws ApplicationException {
    List<CategoryAttributeResponse> categoryAttributeFeatureResponses =
        categoryAttributeResponses
            .stream()
            .filter(
                categoryAttribute -> FEATURES.contains(categoryAttribute.getAttribute()
                    .getAttributeType())
                    && categoryAttribute.isMarkForDelete() == false
                    && categoryAttribute.getAttribute().isMarkForDelete() == false)
            .collect(Collectors.toList());
    if (request.getFeatures().size() != categoryAttributeFeatureResponses.size()) {
      StringBuilder allowedAttr = new StringBuilder("");
      if(CollectionUtils.isNotEmpty(categoryAttributeFeatureResponses)){
        for(CategoryAttributeResponse catAttr : categoryAttributeFeatureResponses){
          allowedAttr.append(catAttr.getAttribute().getName() + ", ");
        }
      }
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          "Request of featured attribute is not complete, it must be: " + allowedAttr.toString());
    } else {
      request.getFeatures().forEach(
          feature -> {
            boolean isExists =
                categoryAttributeFeatureResponses.stream().anyMatch(
                    categoryAttributeFeatureResponse -> categoryAttributeFeatureResponse
                        .getAttribute().getName().equals(feature.getName()));
            if (!isExists) {
              throw new IllegalStateException("Attribute variance " + feature.getName()
                  + " is not found!");
            }
          });
    }
  }

  private void validateProductAttributeVariances(ProductApiRequest request,
      List<CategoryAttributeResponse> categoryAttributeResponses) throws ApplicationException {
    List<ProductApiGeneralAttributeRequest> distinctVarians =
        request
            .getVariasi()
            .stream()
            .filter(
                StreamUtils.distinctByKey(variasi -> ((ProductApiGeneralAttributeRequest) variasi)
                    .getName())).collect(Collectors.toList());
    
    List<CategoryAttributeResponse> categoryAttributeDefiningResponses =
        categoryAttributeResponses
            .stream()
            .filter(
                categoryAttribute -> AttributeType.DEFINING_ATTRIBUTE.toString().equals(
                    categoryAttribute.getAttribute().getAttributeType())
                    && categoryAttribute.isMarkForDelete() == false
                    && categoryAttribute.getAttribute().isMarkForDelete() == false)
            .collect(Collectors.toList());
    if (distinctVarians.size() != categoryAttributeDefiningResponses.size()) {
      StringBuilder allowedAttr = new StringBuilder("");
      if(CollectionUtils.isNotEmpty(categoryAttributeDefiningResponses)){
        for(CategoryAttributeResponse catAttr : categoryAttributeDefiningResponses){
          allowedAttr.append(catAttr.getAttribute().getName() + ", ");
        }
      }
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          "Request of featured attribute is not complete, it must be: " + allowedAttr.toString());
    } else {
      distinctVarians.forEach(variance -> {
        boolean isExists =
            categoryAttributeDefiningResponses.stream().anyMatch(
                categoryAttributeDefiningResponse -> categoryAttributeDefiningResponse
                    .getAttribute().getName().equals(variance.getName()));
        if (!isExists) {
          throw new IllegalStateException("Attribute variance " + variance.getName()
              + " is not found!");
        }
      });
    }
  }
  
  private CreateProductRequest productCollectionRequestBuilder(ProductApiRequest productApiRequest)
      throws Exception {
    CreateProductRequest productCollectionRequest = new CreateProductRequest();
    productCollectionRequest.setVersion(0L);
    productCollectionRequest.setName(productApiRequest.getProductName());
    productCollectionRequest.setLength(productApiRequest.getLength());
    productCollectionRequest.setWidth(productApiRequest.getWidth());
    productCollectionRequest.setHeight(productApiRequest.getHeight());
    productCollectionRequest.setWeight(getKilogramFromGram(productApiRequest.getWeight()));
    productCollectionRequest.setDescription(productApiRequest.getDesc().getBytes());
    productCollectionRequest.setLongDescription(productApiRequest.getDesc().getBytes());
    productCollectionRequest.setUniqueSellingPoint(productApiRequest.getUniqueSellingPoint());
    productCollectionRequest.setProductStory(productApiRequest.getProductStory());
    productCollectionRequest.setUrl(productApiRequest.getUrl());
    productCollectionRequest.setSpecificationDetail(productApiRequest.getDesc());
    productCollectionRequest.setUom("PC");
    CategoryDetailResponse categoryDetailResponse = null;
    try {
      categoryDetailResponse = 
          categoryRepository.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(DEFAULT_STORE_ID,
              productApiRequest.getCategoryCode());
      if(categoryDetailResponse == null){
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, 
            "Category is not found: " + productApiRequest.getCategoryCode());
      }
      
      if(!categoryRepository.validateIsCnCategory(DEFAULT_STORE_ID, 
          categoryDetailResponse.getCategoryCode())){
        throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND, 
            " Category " + productApiRequest.getCategoryCode() + " is not last child category, "
                + "please select child category below : " + productApiRequest.getCategoryCode());
      }
    } catch (ApplicationException ae) {
      throw ae;
    } catch (Exception e) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ProductLevel3ApiProcessorServiceBean.ERROR_MESSAGE_CATEGORY_NOT_FOUND, e);
    }
    // build category
    productCollectionRequest.setProductCategories(new ArrayList<ProductCategoryRequest>());
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    productCategoryRequest.setStoreId(DEFAULT_STORE_ID);
    CategoryRequest categoryRequest = new CategoryRequest();
    BeanUtils.copyProperties(categoryDetailResponse, categoryRequest, "categoryAttributes", 
        "productCategories", "masterCategoryReferences", "salesCategoryReferences");
    productCategoryRequest.setCategory(categoryRequest);
    productCollectionRequest.getProductCategories().add(productCategoryRequest);
    
    // build attribute
    productCollectionRequest.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    validateProductAttributeFeatures(productApiRequest, categoryDetailResponse.getCategoryAttributes());
    validateProductAttributeVariances(productApiRequest, categoryDetailResponse.getCategoryAttributes());
    // combine the features and variasi as they are attributes
    productApiRequest.setFeatures(
        copyProductVariationtoFeature(productApiRequest.getFeatures(), productApiRequest.getVariasi())
    );
    for (CategoryAttributeResponse categoryAttrResponse : categoryDetailResponse.getCategoryAttributes()) {
      if (!categoryAttrResponse.isMarkForDelete()) {
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        productAttributeRequest.setProductAttributeValues(new ArrayList<ProductAttributeValueRequest>());
        for(ProductApiGeneralAttributeRequest feature : productApiRequest.getFeatures()){
          ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
          productAttributeValueRequest.setStoreId(categoryAttrResponse.getStoreId());
          // set predefined value
          if (categoryAttrResponse.getAttribute().getAttributeType()
              .equals(String.valueOf(AttributeType.PREDEFINED_ATTRIBUTE))
              && feature.getName().equalsIgnoreCase(categoryAttrResponse.getAttribute().getName())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
            productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
            PredefinedAllowedAttributeValueResponse predefinedValueResponse = null;
            try {
              List<PredefinedAllowedAttributeValueResponse> predefinedValueResponseList =
                  attributeRepository.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(UUID
                      .randomUUID().toString(), ALL_REQUEST, categoryAttrResponse.getAttribute().getId(), 
                      feature.getValue());
              if(CollectionUtils.isEmpty(predefinedValueResponseList)){
                throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, 
                    "Attribute " + feature.getName() + " contains invalid value for: " + feature.getValue());
              }
              for (PredefinedAllowedAttributeValueResponse Brandvalue : predefinedValueResponseList) {
                if (Brandvalue.getValue().equalsIgnoreCase(feature.getValue())) {
                  predefinedValueResponse = Brandvalue;
                  break;
                }
              }
            } catch (ApplicationRuntimeException rex) {
              throw rex;
            } catch (Exception e) {
              throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                  "Attribute " + categoryAttrResponse.getAttribute().getName() + " is not found", e);
            }
            
            if(feature.getName().equalsIgnoreCase(BRAND_ATTRIBUTE_NAME)){
              if(predefinedValueResponse != null){
                productCollectionRequest.setBrand(predefinedValueResponse.getValue());
              } else  {
                throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, 
                    "Brand is not found for: " + feature.getValue());
              }
            }
            
            productAttributeValueRequest.getPredefinedAllowedAttributeValue().setId(predefinedValueResponse.getId());
            productAttributeValueRequest.getPredefinedAllowedAttributeValue().setValue(predefinedValueResponse.getValue());
            productAttributeValueRequest.getPredefinedAllowedAttributeValue().setVersion(predefinedValueResponse.getVersion());
            productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
          }
          // set descriptive value
          if (categoryAttrResponse.getAttribute().getAttributeType()
              .equals(String.valueOf(AttributeType.DESCRIPTIVE_ATTRIBUTE))
              && feature.getName().equalsIgnoreCase(categoryAttrResponse.getAttribute().getName())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
            if (!categoryAttrResponse.getAttribute().isSkuValue()) {
              if (!StringUtils.isEmpty(feature.getValue())) {
                productAttributeValueRequest.setDescriptiveAttributeValue(feature.getValue());
              } else {
                productAttributeValueRequest.setDescriptiveAttributeValue("-");
              }
            }
            productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
          }
          // set defining value
          if (categoryAttrResponse.getAttribute().getAttributeType()
              .equals(String.valueOf(AttributeType.DEFINING_ATTRIBUTE))
              && feature.getName().equalsIgnoreCase(categoryAttrResponse.getAttribute().getName())) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
            productAttributeValueRequest.setAllowedAttributeValue(new AllowedAttributeValueRequest());
            AttributeResponse attributeResponse = 
                attributeRepository.findOne(DEFAULT_STORE_ID, categoryAttrResponse.getAttribute().getId());
            for (AllowedAttributeValueResponse AllowedAttributeValueResponse : attributeResponse.getAllowedAttributeValues()) {
              if (AllowedAttributeValueResponse.getValue().equalsIgnoreCase(feature.getValue())) {
                productAttributeValueRequest.getAllowedAttributeValue().setId(AllowedAttributeValueResponse.getId());
                productAttributeValueRequest.getAllowedAttributeValue().setValue(AllowedAttributeValueResponse.getValue());
                break;
              }
            }
            
            if (StringUtils
                .isEmpty(productAttributeValueRequest.getAllowedAttributeValue().getId())) {
              throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
                  ProductLevel3ApiProcessorServiceBean.ERROR_MESSAGE_DEFINING_NOT_FOUND + " ["
                      + feature.getName() + ":" + feature.getValue() + "]");
            }
            
            productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
          }
        }
        AttributeRequest attributeRequest = new AttributeRequest();
        BeanUtils.copyProperties(categoryAttrResponse.getAttribute(), attributeRequest);
        productAttributeRequest.setStoreId(categoryAttrResponse.getStoreId());
        productAttributeRequest.setAttribute(attributeRequest);
        productAttributeRequest.setProductAttributeName(categoryAttrResponse.getAttribute().getName());
        productCollectionRequest.getProductAttributes().add(productAttributeRequest);
      }
    }
    
    return productCollectionRequest;
  }
  
  private Double generateProductShippingWeight(ProductRequest product) throws Exception {
    if (product.getProductCategories().isEmpty()) {
      throw new ApplicationException(ErrorCategory.VALIDATION, "This product doesn't have category");
    }
    CategoryRequest categoryRequest = product.getProductCategories().get(0).getCategory();
    GenerateShippingWeightRequest generateShippingWeightRequest = new GenerateShippingWeightRequest();
    generateShippingWeightRequest.setLength(product.getLength());
    generateShippingWeightRequest.setWidth(product.getWidth());
    generateShippingWeightRequest.setHeight(product.getHeight());
    generateShippingWeightRequest.setWeight(product.getWeight());
    generateShippingWeightRequest.setCategoryCode(categoryRequest.getCategoryCode());
    GenerateShippingWeightResponse generateShippingWeightResponse =
        this.generatorRepository.generateShippingWeight(generateShippingWeightRequest);
    return generateShippingWeightResponse.getShippingWeight();
  }
  
  private Double generateShippingWeight(Double length, Double width, Double height, Double weight, String categoryCode)
      throws Exception {
    if (StringUtils.isEmpty(categoryCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "This product doesn't have category");
    }
    GenerateShippingWeightRequest request = new GenerateShippingWeightRequest();
    request.setLength(length);
    request.setWidth(width);
    request.setHeight(height);
    request.setWeight(weight);
    request.setCategoryCode(categoryCode);
    GenerateShippingWeightResponse response = this.generatorRepository.generateShippingWeight(request);
    return response.getShippingWeight();
  }
  
  private Double getKilogramFromGram(Double gram) {
    return gram / 1000;
  }
  
}
