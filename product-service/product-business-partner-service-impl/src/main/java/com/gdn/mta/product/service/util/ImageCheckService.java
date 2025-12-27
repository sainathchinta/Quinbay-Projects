package com.gdn.mta.product.service.util;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.service.FileStorageService;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ImageCheckService {

  @Autowired
  private FileStorageService fileStorageService;

  public Pair<String, Map<String, CommonImagePathDto>> addUniqueImagesAndGetInvalidImage(
    ProductRequest request, Map<String, CommonImagePathDto> uniqueImages, String imageSourceDirectory) {
    Map<String, CommonImagePathDto> imageHashCodeLocationPathMap = Stream
      .concat(request.getImages().stream(),
        request.getProductItems().stream().map(ProductItemRequest::getImages).flatMap(List::stream))
      .filter(image -> !image.isMarkForDelete()).collect(Collectors.toMap(Image::getHashCode,
        image -> new CommonImagePathDto(image.getLocationPath(), image.isCommonImage()),
        (image1, image2) -> image1));
    uniqueImages.putAll(imageHashCodeLocationPathMap);
    return Pair.of(
      fileStorageService.checkImageAvailabilityAndGetUnavailableImages(imageSourceDirectory,
        imageHashCodeLocationPathMap), uniqueImages);
  }

  public String addUniqueImagesAndGetInvalidImageForEditedProducts(ProductRequest request,
    Map<String, Image> uniqueImages, String imageSourceDirectory) {
    Map<String, Image> imageHashCodeLocationPathMap = Stream.concat(request.getImages().stream(),
      request.getProductItems().stream().map(ProductItemRequest::getImages).flatMap(List::stream))
      .filter(image -> !image.isMarkForDelete()).collect(
        Collectors.toMap(Image::getHashCode, Function.identity(), (image1, image2) -> image1));
    uniqueImages.putAll(imageHashCodeLocationPathMap);
    return fileStorageService
      .checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(imageSourceDirectory,
        imageHashCodeLocationPathMap);
  }

  public String getUniqueImages(ProductDetailResponse response,
    Map<String, CommonImagePathDto> uniqueImages, String imageSourceDirectory) {
    Map<String, CommonImagePathDto> imageHashCodeLocationPathMap = Stream
      .concat(response.getImages().stream(),
        response.getProductItemResponses().stream().map(ProductItemResponse::getImages)
          .flatMap(List::stream))
      .filter(image -> !image.isMarkForDelete() && image.getOriginalImage()).collect(Collectors
        .toMap(Image::getHashCode,
          image -> new CommonImagePathDto(image.getLocationPath(), image.isCommonImage()),
          (image1, image2) -> image1));
    uniqueImages.putAll(imageHashCodeLocationPathMap);
    return fileStorageService.checkImageAvailabilityAndGetUnavailableImages(imageSourceDirectory,
      imageHashCodeLocationPathMap);
  }

  public String addUniqueImagesAndGetInvalidImage(ProductDetailResponse request,
    Map<String, CommonImagePathDto> uniqueImages, String imageSourceDirectory) {
    Map<String, CommonImagePathDto> imageHashCodeLocationPathMap = Stream
      .concat(request.getImages().stream(),
        request.getProductItemResponses().stream().map(ProductItemResponse::getImages)
          .flatMap(List::stream)).filter(image -> !image.isMarkForDelete()).collect(Collectors
        .toMap(Image::getHashCode,
          image -> new CommonImagePathDto(image.getLocationPath(), image.isCommonImage()),
          (image1, image2) -> image1));
    uniqueImages.putAll(imageHashCodeLocationPathMap);
    return fileStorageService.checkImageAvailabilityAndGetUnavailableImages(imageSourceDirectory,
      imageHashCodeLocationPathMap);
  }

  public String addUniqueImagesAndGetInvalidImageForRevisedProduct(ProductDetailResponse request,
    Map<String, CommonImagePathDto> uniqueImages, String imageSourceDirectory) {
    Map<String, CommonImagePathDto> imageHashCodeLocationPathMap =
      request.getImages().stream().filter(image -> !image.isMarkForDelete()).collect(Collectors
        .toMap(Image::getHashCode,
          image -> new CommonImagePathDto(image.getLocationPath(), image.isCommonImage()),
          (image1, image2) -> image1));
    uniqueImages.putAll(imageHashCodeLocationPathMap);
    return fileStorageService.checkImageAvailabilityAndGetUnavailableImages(imageSourceDirectory,
      imageHashCodeLocationPathMap);
  }
}