package com.gdn.mta.product.service;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import jakarta.annotation.Resource;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.listener.ImageKafkaSubscriberBean;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.ScaleImageRequest;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.service.util.ImageCheckService;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

@Service("productLevel1Service")
public class ApproveProductServiceBean implements ApproveProductService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ApproveProductServiceBean.class);
  private static final String SKIP_RESCALE = "skipScaling";
  private static final String ELIGIBLE_FOR_RESCALE = "eligibleForRescale";

  @Value("${image.source.directory}")
  private String imageSourceDirectory;


  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductWfService productWorkflowService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  @Resource(name = "customGraphicsSettings")
  private List<CustomGraphicsSettings> fullMediumThumbGraphicsSetting;

  @Autowired
  private ImageProcessorService imageProcessorService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ImageCheckService imageCheckService;

  @Autowired
  private ImageKafkaSubscriberBean imageKafkaSubscriberBean;

  private ProductRequest generateEmptyDescriptiveAttributeValue(ProductRequest request) {
    for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
      AttributeRequest attributeRequest = productAttributeRequest.getAttribute();
      if (!attributeRequest.isSkuValue()
          && AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attributeRequest.getAttributeType())) {
        ProductAttributeValueRequest productAttributeValueRequest =
            productAttributeRequest.getProductAttributeValues().get(0);
        if (StringUtils.isEmpty(productAttributeValueRequest.getDescriptiveAttributeValue())) {
          productAttributeValueRequest.setDescriptiveAttributeValue("-");
        }
      }
    }
    return request;
  }

  private void validateCreateOrUpdateProduct(ProductRequest request) {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getName())),
        ApproveProductErrorMessages.NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getName().length() <= ApproveProductErrorMessages.MAXIMUM_PRODUCT_NAME_LENGTH,
        ApproveProductErrorMessages.NAME_LENGTH_MUST_NOT_EXCEED_MAX_ALLOWED);
    GdnPreconditions.checkArgument(!(request.getDescription() == null),
        ApproveProductErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getSpecificationDetail())),
        ApproveProductErrorMessages.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getLength() == null),
        ApproveProductErrorMessages.LENGTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getWidth() == null),
        ApproveProductErrorMessages.WIDTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getHeight() == null),
        ApproveProductErrorMessages.HEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getWeight() == null),
        ApproveProductErrorMessages.WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getShippingWeight() == null),
        ApproveProductErrorMessages.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getProductCategories().isEmpty()),
        ApproveProductErrorMessages.PRODUCT_CATEGORIES_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getProductAttributes().isEmpty()),
        ApproveProductErrorMessages.PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK);
  }

  @Override
  public void approveQc(String productCode) throws Exception {
    LOGGER.info("Product-workflow-tracker : approve qc for productCode : {}", productCode);
    productWorkflowService.approveQC(productCode);
  }

  @Override
  public void approveContent(ProductRequest request) throws ApplicationException{
    try{
      LOGGER.info("Product-workflow-tracker : Update content for productCode : {}", request.getProductCode());
      request =
          ApproveProductUtils.generateImageHashcode(generateEmptyDescriptiveAttributeValue(request));
      validateCreateOrUpdateProduct(request);
      request.setUpdateFromVendor(true);
      this.productService.updateProductContent(request);
      LOGGER.info("Product-workflow-tracker : approve product content for productCode : {}", request.getProductCode());
      productWorkflowService.approveContent(request.getProductCode());
    } catch(Exception e){
      LOGGER.error("Approve content error: for product with code {}", request.getProductCode(), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Approve content error: " + e.getMessage());
    }
  }

  @Override
  public void updateImage(ProductRequest request) throws Exception {
    LOGGER.info("Product-workflow-tracker : Update image for productCode : {}", request.getProductCode());
    request = ApproveProductUtils.generateImageHashcode(request);
    validateCreateOrUpdateProduct(request);
    this.productService.updateProductImage(request);
  }

  @Override
  public void updateImageAndAvoidHashCodeRegeneration(ProductRequest request) throws Exception {
    LOGGER.info("Product-workflow-tracker : Update image for productCode : {}", request.getProductCode());
    validateCreateOrUpdateProduct(request);
    this.productService.updateProductImage(request);
  }


  @Override
  public void approveImage(ProductRequest request, int prioritySeller) throws ApplicationException {
    try{
      this.updateImage(request);
      this.productWorkflowService.processImage(request.getProductCode());
    } catch(Exception e){
      LOGGER.error("Approve image error: for product with code {} ", request.getProductCode(), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Approve image error: " + e.getMessage());
    }
  }

  @Override
  public void processImageWithScalingEligibility(ProductRequest request, int prioritySeller,
    Map<String, Image> productImageHashCodeMapDB, Map<String, Image> itemImageHashCodeMapDB)
    throws Exception {
    LOGGER.info("Product-workflow-tracker : Process image for productCode : {}",
      request.getProductCode());
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    Map<String, Pair<ImageRequest, Image>> imageRequestMap = new HashMap<>();
    Pair<String, Map<String, CommonImagePathDto>> uniqueImagesAndGetInvalidImage =
      imageCheckService.addUniqueImagesAndGetInvalidImage(request, uniqueImages,
        imageSourceDirectory);
    String unavailableImageName = uniqueImagesAndGetInvalidImage.getLeft();
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      handleInvalidImage(request.getProductCode(), unavailableImageName);
    } else {
      uniqueImagesAndGetInvalidImage.getValue().entrySet().forEach(
        images -> createImageRequestForEligibleImages(images, productImageHashCodeMapDB,
          itemImageHashCodeMapDB, imageRequestMap));
      List<Image> skipRescaleRequests = getSkipRescaleRequests(imageRequestMap);
      List<ImageRequest> eligibleForRescaleRequests =
        getEligibleForRescaleRequests(imageRequestMap);
      if (CollectionUtils.isNotEmpty(eligibleForRescaleRequests)) {
        LOGGER.info("Product-workflow-tracker : Scale bulk images request sent to X-GP : {}",
          request.getProductCode());
        BulkImagesProcessRequest bulkImagesProcessRequest =
          createBulkImagesProcessRequest(request, eligibleForRescaleRequests, prioritySeller);
        this.imageProcessorService.scaleImage(bulkImagesProcessRequest);
      }
      else if (CollectionUtils.isNotEmpty(skipRescaleRequests)) {
        BulkImageProcessResponse bulkImageProcessResponse =
          createBulkImageProcessResponse(request, skipRescaleRequests);
        LOGGER.info("Skipped Image Scaling and processing Image status event for product : {} "
            + "with request : {} ", bulkImageProcessResponse.getGroupCode(),
          bulkImageProcessResponse);
        imageKafkaSubscriberBean.kafkaEventHelper(bulkImageProcessResponse,
          GdnMandatoryRequestParameterUtil.getRequestId());
      }
    }
  }


  private void createImageRequestForEligibleImages(Map.Entry<String, CommonImagePathDto> uniqueImage,
    Map<String, Image> productImageHashCodeMap, Map<String, Image> itemImageHashCodeMap,
    Map<String, Pair<ImageRequest, Image>> imageRequestMap) {
    ImageRequest imageRequest = new ImageRequest();

    Map<String, Image> hashCodesInDb = new HashMap<>(productImageHashCodeMap);
    itemImageHashCodeMap.forEach(hashCodesInDb::putIfAbsent);

    imageRequest.setHashCode(uniqueImage.getKey());
    imageRequest.setAbsoluteImagePath(
        imageSourceDirectory + "/" + uniqueImage.getValue().getLocationPath());
    String[] splitImageFilenameByDash = uniqueImage.getValue().getLocationPath().split("/");
    imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
    imageRequest.setCommonImage(uniqueImage.getValue().isCommonImage());
    Image image = hashCodesInDb.getOrDefault(imageRequest.getHashCode(), null);
    Pair<ImageRequest, Image> imageWithStatus = Pair.of(imageRequest, image);
    imageRequestMap.put(uniqueImage.getKey(), imageWithStatus);
  }

  private ImageResponse toImageResponse(Image image) {
    ImageResponse imageResponse =
      new ImageResponse(image.getLocationPath(), image.getHashCode(), true,
        Constants.PBP_SCALING_SKIPPED_ACTION, null);
    imageResponse.setCommonImage(image.isCommonImage());
    return imageResponse;
  }

  private BulkImageProcessResponse createBulkImageProcessResponse(ProductRequest request, List<Image> skipRescaleRequests) {
    BulkImageProcessResponse bulkImageProcessResponse = new BulkImageProcessResponse();
    bulkImageProcessResponse.setGroupCode(request.getProductCode());
    List<ImageResponse> imageResponsesForSkippedScaling = new ArrayList<>();
    for (Image image : skipRescaleRequests) {
      ImageResponse imageResponse = toImageResponse(image);
      imageResponsesForSkippedScaling.add(imageResponse);
    }
    bulkImageProcessResponse.setImageResponses(imageResponsesForSkippedScaling);
    bulkImageProcessResponse.setStoreId(request.getStoreId());
    bulkImageProcessResponse.setUsername(GdnMandatoryRequestParameterUtil.getUsername());
    return bulkImageProcessResponse;
  }

  private BulkImagesProcessRequest createBulkImagesProcessRequest(ProductRequest request,
    List<ImageRequest> eligibleForRescaleRequests, int prioritySeller)
    throws JsonProcessingException {
    BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
    bulkImagesProcessRequest.setGroupCode(request.getProductCode());
    bulkImagesProcessRequest.setCustomGraphicsSettings(
      objectMapper.writeValueAsString(fullMediumThumbGraphicsSetting));
    bulkImagesProcessRequest.setImageRequests(eligibleForRescaleRequests);
    bulkImagesProcessRequest.setPrioritySeller(prioritySeller);
    return bulkImagesProcessRequest;
  }

  private void handleInvalidImage(String productCode, String unavailableImageName) throws Exception {
    LOGGER.warn("Image does not exist for path {}/{}", imageSourceDirectory, productCode);
    this.productWorkflowService.rejectImage(productCode);
    throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
      "Image: " + unavailableImageName + " not found for product with code: " + productCode);
  }

  private static List<Image> getSkipRescaleRequests(
    Map<String, Pair<ImageRequest, Image>> imageRequestMap) {
    return imageRequestMap.values().stream().filter(pair -> Objects.nonNull(pair.getValue()))
      .map(Pair::getValue).collect(Collectors.toList());
  }

  private List<ImageRequest> getEligibleForRescaleRequests(
    Map<String, Pair<ImageRequest, Image>> imageRequestMap) {
    return imageRequestMap.values().stream()
      .filter(pair -> Objects.isNull(pair.getValue())).map(Pair::getKey)
      .collect(Collectors.toList());
  }



  @Override
  public void processImage(ProductRequest request, int prioritySeller) throws Exception {
    LOGGER.info("Product-workflow-tracker : Process image for productCode : {}", request.getProductCode());
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    String unavailableImageName = imageCheckService
      .addUniqueImagesAndGetInvalidImage(request, uniqueImages, imageSourceDirectory).getKey();
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      LOGGER.warn(
          "methodName={} action={} service=MTA status=WARN ref=product-creation productCode={} "
              + "desc=image is not exist for path {}/{} and store_id = {}" , "processImage", "validate-image",
          request.getProductCode(), imageSourceDirectory, request.getProductCode(), request.getStoreId());
      this.productWorkflowService.rejectImage(request.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "image: " + unavailableImageName + " not found for product with code :" + request.getProductCode());
    } else {
      BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
      bulkImagesProcessRequest.setGroupCode(request.getProductCode());
      bulkImagesProcessRequest.setCustomGraphicsSettings(
          this.objectMapper.writeValueAsString(this.fullMediumThumbGraphicsSetting));
      List<ImageRequest> imageRequests =
          uniqueImages.entrySet().stream().map(imageSet -> createImageRequest(imageSet)).collect(Collectors.toList());
      bulkImagesProcessRequest.setImageRequests(imageRequests);
      bulkImagesProcessRequest.setPrioritySeller(prioritySeller);
      LOGGER.info("Product-workflow-tracker : Scale bulk images request sent to X-GP : {}", request.getProductCode());
      this.imageProcessorService.scaleImage(bulkImagesProcessRequest);
    }
  }

  @Override
  public void processImageForRevisedProduct(ProductDetailResponse productDetailResponse) throws Exception {
    LOGGER.info("Product-workflow-tracker : Process image for revised product for productCode : {} , images : {}",
        productDetailResponse.getProductCode(), productDetailResponse.getImages());
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    String unavailableImageName = imageCheckService
        .addUniqueImagesAndGetInvalidImageForRevisedProduct(productDetailResponse, uniqueImages, imageSourceDirectory);
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      LOGGER.warn("methodName={} action={} service=MTA status=WARN ref=product-creation productCode={} "
              + "desc=image is not exist for path {}/{} and store_id = {}", "processImage", "validate-image",
          productDetailResponse.getProductCode(), imageSourceDirectory, productDetailResponse.getProductCode(),
          productDetailResponse.getStoreId());
      this.productWorkflowService.rejectImage(productDetailResponse.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "image: " + unavailableImageName + " not found for product with code :" + productDetailResponse
              .getProductCode());
    } else {
      BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
      bulkImagesProcessRequest.setRevised(true);
      bulkImagesProcessRequest.setGroupCode(productDetailResponse.getProductCode());
      bulkImagesProcessRequest
          .setCustomGraphicsSettings(this.objectMapper.writeValueAsString(this.fullMediumThumbGraphicsSetting));
      List<ImageRequest> imageRequests =
          uniqueImages.entrySet().stream().map(imageSet -> createImageRequest(imageSet)).collect(Collectors.toList());
      bulkImagesProcessRequest.setImageRequests(imageRequests);
      LOGGER.info("Product-workflow-tracker : Scale bulk images for revised product request sent to X-GP : {}",
          productDetailResponse.getProductCode());
      this.imageProcessorService.scaleImage(bulkImagesProcessRequest);
    }
  }

  @Override
  public void processEditedImage(ProductRequest request) throws Exception {
    LOGGER.info("Product-workflow-tracker : Process image for edited product productCode : {}", request.getProductCode());
    Map<String, Image> uniqueImages = new HashMap<>();
    String unavailableImageName =
      imageCheckService.addUniqueImagesAndGetInvalidImageForEditedProducts(request, uniqueImages, imageSourceDirectory);
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      LOGGER.warn(
          "methodName={} action={} service=MTA status=WARN ref=product-creation productCode={} "
              + "desc=image is not exist for path {}/{} and store_id = {}" , "processImage", "validate-image",
          request.getProductCode(), imageSourceDirectory, request.getProductCode(), request.getStoreId());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "image: " + unavailableImageName + " not found for product with code :" + request.getProductCode());
    } else {
      ScaleEditedImageRequest scaleEditedImageRequest = new ScaleEditedImageRequest();
      scaleEditedImageRequest.setProductCode(request.getProductCode());
      scaleEditedImageRequest.setCustomGraphicsSettings(
          this.objectMapper.writeValueAsString(this.fullMediumThumbGraphicsSetting));
      List<ScaleImageRequest> scaleImageRequests =
          uniqueImages.entrySet().stream().map(image -> createImageRequestForEditedProducts(image))
              .collect(Collectors.toList());
      scaleEditedImageRequest.setImageRequests(scaleImageRequests);
      LOGGER.info("Product-workflow-tracker : Scale bulk images request sent to X-GP : {}", request.getProductCode());
      this.imageProcessorService.scaleEditedImages(scaleEditedImageRequest);
    }
  }

  @Override
  public void processImage(ProductDetailResponse response) throws Exception {
    LOGGER.info("Product-workflow-tracker : Process image for productCode : {}", response.getProductCode());
    Map<String, CommonImagePathDto> uniqueImages = new HashMap<>();
    String unavailableImageName = imageCheckService
      .addUniqueImagesAndGetInvalidImage(response, uniqueImages, imageSourceDirectory);
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      LOGGER.warn(
          "methodName={} action={} service=MTA status=WARN ref=product-creation productCode={} "
              + "desc=image is not exist for path {}/{} and store_id = {}" , "processImage", "validate-image",
          response.getProductCode(), imageSourceDirectory, response.getProductCode(), response.getStoreId());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          "image: " + unavailableImageName + " not found for product with code :" + response.getProductCode());
    } else {
      BulkImagesProcessRequest bulkImagesProcessRequest = new BulkImagesProcessRequest();
      bulkImagesProcessRequest.setGroupCode(response.getProductCode());
      bulkImagesProcessRequest.setCustomGraphicsSettings(
          this.objectMapper.writeValueAsString(this.fullMediumThumbGraphicsSetting));
      List<ImageRequest> imageRequests =
          uniqueImages.entrySet().stream().map(imageSet -> createImageRequest(imageSet)).collect(Collectors.toList());
      bulkImagesProcessRequest.setImageRequests(imageRequests);
      LOGGER.info("Product-workflow-tracker : Scale bulk images request sent to X-GP : {}", response.getProductCode());
      this.imageProcessorService.scaleImage(bulkImagesProcessRequest);
    }
  }

  /**
   * create image request to send it to XGP
   * @param uniqueImage
   * @return
   */
  private ImageRequest createImageRequest(Map.Entry<String, CommonImagePathDto> uniqueImage) {
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setHashCode(uniqueImage.getKey());
    imageRequest.setAbsoluteImagePath(
        imageSourceDirectory + "/" + uniqueImage.getValue().getLocationPath());
    String[] splitImageFilenameByDash = uniqueImage.getValue().getLocationPath().split("/");
    imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
    imageRequest.setCommonImage(uniqueImage.getValue().isCommonImage());
    return imageRequest;
  }

  /**
   * create image request to send it to XGP
   * @param uniqueImage
   */
  private ScaleImageRequest createImageRequestForEditedProducts(Map.Entry<String, Image> uniqueImage) {
    ScaleImageRequest imageRequest = new ScaleImageRequest();
    imageRequest.setHashCode(uniqueImage.getKey());
    imageRequest.setImagePathLocation(uniqueImage.getValue().getLocationPath());
    imageRequest.setActive(uniqueImage.getValue().isActive());
    String[] splitImageFilenameByDash = uniqueImage.getValue().getLocationPath().split("/");
    imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
    imageRequest.setCommonImage(uniqueImage.getValue().isCommonImage());
    return imageRequest;
  }
}
