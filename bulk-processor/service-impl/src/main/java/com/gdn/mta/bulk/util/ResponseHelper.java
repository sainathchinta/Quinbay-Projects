package com.gdn.mta.bulk.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.request.BulkBasicInfoUpdateRequest;
import com.gdn.mta.bulk.models.L3InfoUpdateChangeType;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.bulk.models.VideoAddEditRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.ExcelHeaderNames;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthResponse;

import com.gdn.x.campaign.response.FailedProductsResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;

public class ResponseHelper {

  public static final String REVIEW_TYPE_UPDATE = "update";
  public static final String REVIEW_TYPE_NEW = "new";
  public static final String INSTORE_ON = "1";

  private ResponseHelper() {
  }

  public static List<RecatProcessSummaryResponse> toRecatProcessSummaryResponseList(
      List<RecatProcess> recatProcessList) {
    List<RecatProcessSummaryResponse> recatProcessSummaryResponseList = new ArrayList<>();
    for (RecatProcess recatProcess : recatProcessList) {
      RecatProcessSummaryResponse response =
          RecatProcessSummaryResponse.builder().recatRequestCode(recatProcess.getRecatRequestCode())
              .uploadDate(recatProcess.getCreatedDate()).scheduledDate(recatProcess.getScheduledTime())
              .status(recatProcess.getStatus()).productCount(0).completionDate(recatProcess.getEndTime())
              .initiator(recatProcess.getCreatedBy()).build();
      if (Objects.nonNull(recatProcess.getTotalCount())) {
        response.setProductCount(recatProcess.getTotalCount());
      }
      recatProcessSummaryResponseList.add(response);
    }
    return recatProcessSummaryResponseList;
  }

  public static List<BulkInternalProcessSummaryResponse> toBulkInternalProcessSummaryResponseList(
      List<BulkInternalProcess> bulkInternalProcessList) {
    List<BulkInternalProcessSummaryResponse> bulkInternalProcessSummaryResponseList = new ArrayList<>();
    for (BulkInternalProcess bulkInternalProcess : bulkInternalProcessList) {
      bulkInternalProcessSummaryResponseList.add(BulkInternalProcessSummaryResponse.builder()
          .internalProcessRequestCode(bulkInternalProcess.getInternalProcessRequestCode())
          .createdDate(bulkInternalProcess.getCreatedDate()).status(bulkInternalProcess.getStatus())
          .totalCount(bulkInternalProcess.getTotalCount()).endDate(bulkInternalProcess.getEndTime())
          .initiator(bulkInternalProcess.getCreatedBy()).sellerCode(bulkInternalProcess.getSellerCode())
          .sellerName(bulkInternalProcess.getSellerName()).fileName(bulkInternalProcess.getFileName())
          .errorFilePath(bulkInternalProcess.getErrorFilePath()).build());
    }
    return bulkInternalProcessSummaryResponseList;
  }

  public static void validateResponse(GdnRestSingleResponse<FailedProductsResponse> response) {
    if (Objects.isNull(response)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        ProductUpdateErrorMessages.CAMPAIGN_UPDATE_FAILED);
    }
    if (!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        ProductUpdateErrorMessages.CAMPAIGN_UPDATE_FAILED);
    }
  }

  public static void setBrandAuthResponse(List<BrandAuthResponse> brandAuthResponseList,
      List<BrandAuthFilterResponse> brandAuthFilterResponses, Map<String, String> sellerCodeNameMap,
      SimpleDateFormat dateFormat) {
    for (BrandAuthFilterResponse brandAuthFilterResponse : brandAuthFilterResponses) {
      BrandAuthResponse brandAuthResponse = new BrandAuthResponse();
      BeanUtils.copyProperties(brandAuthFilterResponse, brandAuthResponse, "authStartDate", "authEndDate");
      brandAuthResponse.setSellerName(
        sellerCodeNameMap.getOrDefault(brandAuthResponse.getSellerCode(), StringUtils.EMPTY));
      brandAuthResponse.setAuthStartDate(dateFormat.format(brandAuthFilterResponse.getAuthStartDate()));
      brandAuthResponse.setAuthEndDate(dateFormat.format(brandAuthFilterResponse.getAuthEndDate()));
      brandAuthResponseList.add(brandAuthResponse);
    }
  }

  public static Map<String, BulkProcessImage> buildImageUrlAndBulkProcessImageMap(
      List<BulkProcessImage> bulkProcessImages) {
    return Optional.ofNullable(bulkProcessImages).orElse(Collections.emptyList()).stream()
        .filter(Objects::nonNull)
        .filter(bulkProcessImage -> StringUtils.isNotBlank(bulkProcessImage.getImageURL())).collect(
            Collectors.toMap(BulkProcessImage::getImageURL, Function.identity(),
                (existing, replacement) -> existing
            ));
  }

  public static boolean checkImageForErrors(BulkProcessImage image) {
    return image.isCompleted() && StringUtils.isNotBlank(image.getErrorMessage());
  }

  public static boolean checkVideoForErrors(BulkProcessVideo video) {
    return video.isCompleted() && StringUtils.isNotBlank(video.getErrorMessage());
  }

  public static Map<String, BulkProcessVideo> buildVideoUrlAndBulkProcessVideoMap(
      List<BulkProcessVideo> bulkProcessVideo) {
    return Optional.ofNullable(bulkProcessVideo).orElse(Collections.emptyList()).stream()
        .filter(Objects::nonNull).filter(video -> StringUtils.isNotBlank(video.getUploadedURL()))
        .collect(Collectors.toMap(BulkProcessVideo::getUploadedURL, Function.identity(),
            (existing, replacement) -> existing));
  }

  public static BulkBasicInfoUpdateRequest getBulkBasicInfoUpdateRequest(
      Map<String, String> requestMap, String youtubeRegex) {
    Set<String> commonImagesSet = new LinkedHashSet<>();
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.MAIN_PHOTO));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_2));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_3));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_4));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_5));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_6));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_7));
    addIfNotBlank(commonImagesSet, requestMap.get(BulkParameters.COMMON_PHOTO_8));

    return BulkBasicInfoUpdateRequest.builder().commonImages(new ArrayList<>(commonImagesSet))
        .videoUrl(requestMap.get(BulkParameters.VIDEO_URL))
        .productSku(requestMap.get(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER))
        .productName(requestMap.get(BulkParameters.PARENT_PRODUCT_NAME_HEADER))
        .inStore(Double.compare(Double.parseDouble(requestMap.getOrDefault(BulkParameters.INSTORE, "0.0")), 1.0) == 0)
        .brand(requestMap.get(BulkParameters.BRAND_HEADER))
        .category(requestMap.get(BulkParameters.CATEGORY_HEADER))
        .description(requestMap.get(BulkParameters.DESCRIPTIONS))
        .shippingType(requestMap.get(BulkParameters.SHIPPING_TYPE))
        .length(Double.parseDouble(StringUtils.defaultIfEmpty(requestMap.get(BulkParameters.LENGTH_HEADER), "0.0")))
        .width(Double.parseDouble(StringUtils.defaultIfEmpty(requestMap.get(BulkParameters.WIDTH_HEADER), "0.0")))
        .height(Double.parseDouble(StringUtils.defaultIfEmpty(requestMap.get(BulkParameters.HEIGHT_HEADER), "0.0")))
        .weight(Double.parseDouble(StringUtils.defaultIfEmpty(requestMap.get(BulkParameters.ACTUAL_WEIGHT), "0.0")))
        .shippingWeight(Double.parseDouble(StringUtils.defaultIfEmpty(requestMap.get(BulkParameters.SHIPPING_WEIGHT), "0.0")))
        .sizeChartCode(requestMap.get(BulkParameters.SIZE_CHART))
        .mainImageUrl(requestMap.get(BulkParameters.MAIN_PHOTO)).youtubeUrl(
            StringUtils.isNotBlank(requestMap.get(BulkParameters.VIDEO_URL)) && ValidateUrlUtil.validateYouTubeRegex(
                requestMap.get(BulkParameters.VIDEO_URL), youtubeRegex)).build();
  }

  private static void addIfNotBlank(Collection<String> collection, String value) {
    if (StringUtils.isNotBlank(value)) {
      collection.add(value);
    }
  }

  public static String getErrorMessageForBulkProcessImage(BulkProcessImage bulkProcessImage) {
    return Objects.nonNull(bulkProcessImage) && StringUtils.isNotBlank(
        bulkProcessImage.getErrorMessage()) ?
        bulkProcessImage.getErrorMessage() :
        BulkProcessValidationErrorMessages.IMAGE_VALIDATION_FAILED;
  }

  public static String getErrorMessageForBulkProcessVideo(BulkProcessVideo bulkProcessVideo) {
    return Objects.nonNull(bulkProcessVideo) && StringUtils.isNotBlank(
        bulkProcessVideo.getErrorMessage()) ?
        bulkProcessVideo.getErrorMessage() :
        BulkProcessValidationErrorMessages.VIDEO_VALIDATION_FAILED;
  }

  public static Set<L3InfoUpdateChangeType> getChangeTypes(BulkBasicInfoUpdateRequest request,
      ProductL3Response l3Response, String videoStaticBaseUrlPrefix) {
    Set<L3InfoUpdateChangeType> changeTypes = new HashSet<>();
    if (!org.apache.commons.lang3.StringUtils.equals(request.getProductName(),
        l3Response.getMasterDataProduct().getProductName())) {
      changeTypes.add(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE);
    }
    if (request.isInStore() != l3Response.isOff2OnChannelActive()) {
      changeTypes.add(L3InfoUpdateChangeType.INSTORE_UPDATE);
    }
    if (!StringUtils.equals(request.getDescription(),
        l3Response.getMasterDataProduct().getDescription())) {
      changeTypes.add(L3InfoUpdateChangeType.DESCRIPTION_UPDATE);
    }
    if (getShippingType(request.getShippingType()) != l3Response.getProductType().getCode()) {
      changeTypes.add(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE);
    }

    if (checkDimensionAreUpdatedOrNot(request, l3Response)) {
      changeTypes.add(L3InfoUpdateChangeType.DIMENSIONS_UPDATE);
    }
    if (!StringUtils.equals(request.getSizeChartCode(), StringUtils.defaultString(l3Response.getSizeChartCode()))) {
      changeTypes.add(L3InfoUpdateChangeType.SIZE_CHART_UPDATE);
    }
    checkVideoOrYoutubeChanged(request, l3Response, videoStaticBaseUrlPrefix, changeTypes);

    if (request.isImagesUpdated()) {
      changeTypes.add(L3InfoUpdateChangeType.COMMON_IMAGE_UPDATE);
    }
    return changeTypes;
  }

  private static void checkVideoOrYoutubeChanged(BulkBasicInfoUpdateRequest request, ProductL3Response l3Response, String videoStaticBaseUrlPrefix, Set<L3InfoUpdateChangeType> changeTypes) {
    final String newUrl = request.getVideoUrl();
    final String existingVideoUrl = l3Response.getVideoUrl();
    final String existingYoutubeUrl = l3Response.getUrl();

    if (StringUtils.isNotBlank(newUrl)) {
      if (request.isYoutubeUrl()) {
        if (!StringUtils.equals(newUrl, existingYoutubeUrl)) {
          changeTypes.add(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE);
        }
      } else {
        String trimmedUploadedVideoUrl = CommonUtils.trimVideoUrlPrefix(newUrl, videoStaticBaseUrlPrefix);
        if (!StringUtils.equals(trimmedUploadedVideoUrl, existingVideoUrl)) {
          changeTypes.add(L3InfoUpdateChangeType.VIDEO_UPDATE);
        }
      }
    } else {
      if (StringUtils.isNotBlank(existingVideoUrl)) {
        changeTypes.add(L3InfoUpdateChangeType.VIDEO_UPDATE);
      }
      if (StringUtils.isNotBlank(existingYoutubeUrl)) {
        changeTypes.add(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE);
      }
    }
  }


  private static boolean checkDimensionAreUpdatedOrNot(BulkBasicInfoUpdateRequest request,
      ProductL3Response l3Response) {
    return Double.compare(request.getLength(), l3Response.getLength()) != 0
        || Double.compare(request.getWidth(), l3Response.getWidth()) != 0
        || Double.compare(request.getHeight(), l3Response.getHeight()) != 0
        || Double.compare(request.getWeight() / 1000, l3Response.getWeight()) != 0;
  }

  public static int getShippingType(String shippingType) {
    if (StringUtils.isBlank(shippingType)) {
      return 0;
    }
    switch (shippingType) {
      case ExcelHeaderNames.SHIPPED_BY_BLIBLI_ID:
        return 1;
      case ExcelHeaderNames.SHIPPED_BY_SELLER_ID:
        return 2;
      case ExcelHeaderNames.BOPIS_ID:
        return 3;
      default:
        return 0;
    }
  }

  public static void buildEditRequest(BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessVideo> videoUrlAndBulkProcessVideoMap,
      ProductMasterDataEditRequest productMasterDataEditRequest,
      List<ProductLevel3SummaryDetailsImageRequest> productL3CommonImageRequestList,
      ProfileResponse profileResponse, Set<L3InfoUpdateChangeType> changeTypes,
      ProductL3Response productL3Response) {
    String url = StringUtils.isNotBlank(request.getVideoUrl()) && request.isYoutubeUrl() ?
        request.getVideoUrl() :
        null;
    productMasterDataEditRequest.setUrl(url);
    VideoAddEditRequest videoAddEditRequest = null;
    if (StringUtils.isNotBlank(request.getVideoUrl())) {
      videoAddEditRequest =
          getVideoAddEditRequest(request, videoUrlAndBulkProcessVideoMap, productMasterDataEditRequest,
              videoAddEditRequest);
    } else {
      productMasterDataEditRequest.setVideoDelete(true);
      productMasterDataEditRequest.setVideoAddEditRequest(null);
    }
    productMasterDataEditRequest.setProductSku(request.getProductSku());
    productMasterDataEditRequest.setProductName(request.getProductName());
    productMasterDataEditRequest.setInstore(request.isInStore());
    productMasterDataEditRequest.setDescription(request.getDescription());
    productMasterDataEditRequest.setProductType(
        ResponseHelper.getShippingType(request.getShippingType()));
    productMasterDataEditRequest.setLength(request.getLength());
    productMasterDataEditRequest.setWidth(request.getWidth());
    productMasterDataEditRequest.setHeight(request.getHeight());
    productMasterDataEditRequest.setWeight(
        BulkCreationCommonUtil.getKilogramFromGram(request.getWeight()));
    productMasterDataEditRequest.setShippingWeight(request.getShippingWeight());
    productMasterDataEditRequest.setSizeChartCode(request.getSizeChartCode());
    productMasterDataEditRequest.setProductLevel3SummaryDetailsImageRequests(productL3CommonImageRequestList);
    productMasterDataEditRequest.setDangerousGoodsLevel(productL3Response.getDangerousGoodsLevel());
    productMasterDataEditRequest.setUrl(url);
    productMasterDataEditRequest.setVideoAddEditRequest(videoAddEditRequest);
    productMasterDataEditRequest.setB2cActivated(productL3Response.isB2cActivated());
    Optional.ofNullable(productL3Response.getMasterDataProduct())
        .map(MasterDataProductDTO::getBrand).ifPresent(productMasterDataEditRequest::setBrand);
    Optional.ofNullable(productL3Response.getMasterDataProduct())
        .map(MasterDataProductDTO::getUniqueSellingPoint)
        .ifPresent(productMasterDataEditRequest::setUniqueSellingPoint);
    if (Objects.nonNull(profileResponse)) {
      productMasterDataEditRequest.setOfficialStoreSeller(profileResponse.isOfficial());
      productMasterDataEditRequest.setSellerBopisFlag(
          Optional.ofNullable(profileResponse.getBopisFlag()).orElse(false));
      productMasterDataEditRequest.setSellerBigProductFlag(
          Optional.ofNullable(profileResponse.getBigProductFlag()).orElse(false));
      productMasterDataEditRequest.setTrustedSeller(profileResponse.isTrustedSeller());
    }
    productMasterDataEditRequest.setMasterDataEditChangeTypes(changeTypes);
  }

  private static VideoAddEditRequest getVideoAddEditRequest(BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessVideo> videoUrlAndBulkProcessVideoMap,
      ProductMasterDataEditRequest productMasterDataEditRequest, VideoAddEditRequest videoAddEditRequest) {
    if (!request.isYoutubeUrl()) {
      BulkProcessVideo bulkProcessVideo1 = videoUrlAndBulkProcessVideoMap.get(request.getVideoUrl());
      if (Objects.nonNull(bulkProcessVideo1)) {
        videoAddEditRequest = new VideoAddEditRequest();
        videoAddEditRequest.setVideoUrl(bulkProcessVideo1.getVideoUrl());
        videoAddEditRequest.setVideoName(bulkProcessVideo1.getVideoName());
        videoAddEditRequest.setCoverImagePath(bulkProcessVideo1.getCoverImagePath());
        videoAddEditRequest.setVideoId(bulkProcessVideo1.getVideoId());
      }
    } else {
      productMasterDataEditRequest.setVideoDelete(true);
      productMasterDataEditRequest.setVideoAddEditRequest(null);
    }
    return videoAddEditRequest;
  }

  public static List<ProductLevel3SummaryDetailsImageRequest> generateCommonImageRequestForEdit(
      ProductL3Response productL3Response, BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessImage> imageUrlAndBulkProcessImageMap, String imageStaticBaseUrlPrefix,
      String pathPrefix) {
    List<ProductLevel3SummaryDetailsImageRequest> result = new ArrayList<>();
    List<MasterDataProductImageDTO> existingImages =
        Optional.ofNullable(productL3Response.getMasterDataProduct().getMasterDataProductImages())
            .orElse(Collections.emptyList());
    Map<String, MasterDataProductImageDTO> existingImageMap = toImageMap(existingImages);
    Set<String> retainedImagePaths = new HashSet<>();
    processUploadedImagesForImageAdditionAndMainImageChange(request, imageUrlAndBulkProcessImageMap, existingImageMap, result,
        retainedImagePaths, imageStaticBaseUrlPrefix, productL3Response.getProductCode(), pathPrefix);
    processDeletedImages(existingImageMap, retainedImagePaths, result, request);
    return result;
  }

  private static Map<String, MasterDataProductImageDTO> toImageMap(
      List<MasterDataProductImageDTO> images) {
    return images.stream().filter(img -> StringUtils.isNotBlank(img.getLocationPath()))
        .collect(Collectors.toMap(MasterDataProductImageDTO::getLocationPath, Function.identity()));
  }

  private static void processUploadedImagesForImageAdditionAndMainImageChange(BulkBasicInfoUpdateRequest request,
      Map<String, BulkProcessImage> imageUrlAndBulkProcessImageMap,
      Map<String, MasterDataProductImageDTO> existingImageMap, List<ProductLevel3SummaryDetailsImageRequest> result,
      Set<String> retainedImagePaths, String imageStaticBaseUrlPrefix, String productCode, String pathPrefix) {

    List<String> uploadedImages =
        Optional.ofNullable(request.getCommonImages()).orElse(Collections.emptyList());
    String uploadedMainImageUrl = request.getMainImageUrl();
    if (uploadedMainImageUrl.startsWith(imageStaticBaseUrlPrefix)) {
      uploadedMainImageUrl = uploadedMainImageUrl.replace(imageStaticBaseUrlPrefix, StringUtils.EMPTY);
    }

    String existingMainImage =
        existingImageMap.entrySet().stream().filter(entry -> entry.getValue().isMainImage()).findFirst()
            .map(entry -> entry.getValue().getLocationPath()).orElse(StringUtils.EMPTY);

    boolean isExistingMainImageChanged = !StringUtils.equals(existingMainImage, uploadedMainImageUrl);
    for (String imageUrl : uploadedImages) {
      BulkProcessImage bulkImage = imageUrlAndBulkProcessImageMap.get(imageUrl);
      // Need to add condition
      String prefixRemovedImageUrl = imageUrl.startsWith(imageStaticBaseUrlPrefix) ?
          imageUrl.replace(imageStaticBaseUrlPrefix, StringUtils.EMPTY) :
          imageUrl;
      if (isExistingImage(existingImageMap, bulkImage, prefixRemovedImageUrl)) {
        if (StringUtils.equals(existingMainImage, prefixRemovedImageUrl) && isExistingMainImageChanged) {
          ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
          imageRequest.setLocationPath(existingMainImage);
          imageRequest.setMainImage(false);
          imageRequest.setMarkForDelete(false);
          imageRequest.setReviewType(REVIEW_TYPE_UPDATE);
          request.setImagesUpdated(true);
          result.add(imageRequest);

        } else if (isExistingMainImageChanged && StringUtils.equals(uploadedMainImageUrl, prefixRemovedImageUrl)) {
          ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
          imageRequest.setLocationPath(prefixRemovedImageUrl);
          imageRequest.setMainImage(true);
          imageRequest.setMarkForDelete(false);
          imageRequest.setReviewType(REVIEW_TYPE_UPDATE);
          imageRequest.setSequence(0);
          request.setImagesUpdated(true);
          result.add(imageRequest);
        }
        retainedImagePaths.add(prefixRemovedImageUrl);
      } else {
        String location = bulkImage.getLocation();
        boolean isMainImage = StringUtils.equals(prefixRemovedImageUrl, uploadedMainImageUrl);
        ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
        imageRequest.setLocationPath(pathPrefix + Constant.SLASH + productCode + Constant.SLASH + location);
        imageRequest.setMainImage(isMainImage);
        imageRequest.setMarkForDelete(false);
        imageRequest.setSequence(bulkImage.getSequence());
        imageRequest.setReviewType(REVIEW_TYPE_NEW);
        request.setImagesUpdated(true);
        retainedImagePaths.add(location);
        result.add(imageRequest);
      }
    }
  }

  public static boolean isExistingImage(Map<String, MasterDataProductImageDTO> existingImageMap, BulkProcessImage bulkImage,
      String prefixRemovedImageUrl) {
    return Objects.isNull(bulkImage) || existingImageMap.containsKey(prefixRemovedImageUrl);
  }

  private static void processDeletedImages(Map<String, MasterDataProductImageDTO> existingImageMap,
      Set<String> retainedImagePaths, List<ProductLevel3SummaryDetailsImageRequest> result,
      BulkBasicInfoUpdateRequest request) {
    for (Map.Entry<String, MasterDataProductImageDTO> entry : existingImageMap.entrySet()) {
      String existingPath = entry.getKey();
      if (retainedImagePaths.contains(existingPath)) {
        continue;
      }
      MasterDataProductImageDTO existingImage = entry.getValue();
      ProductLevel3SummaryDetailsImageRequest imageRequest = new ProductLevel3SummaryDetailsImageRequest();
      imageRequest.setLocationPath(existingPath);
      imageRequest.setMainImage(false);
      imageRequest.setMarkForDelete(true);
      imageRequest.setSequence(existingImage.getSequence());
      imageRequest.setReviewType(REVIEW_TYPE_UPDATE);
      result.add(imageRequest);
      request.setImagesUpdated(true);
    }
  }
}
