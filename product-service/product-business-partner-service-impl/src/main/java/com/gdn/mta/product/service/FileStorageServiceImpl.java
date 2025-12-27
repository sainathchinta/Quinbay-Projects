package com.gdn.mta.product.service;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.service.config.GcsProperties;
import com.gdn.mta.product.valueobject.CommonImagePathDto;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.common.collect.ImmutableSet;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class FileStorageServiceImpl implements FileStorageService {
  private static final Set<String> FLOW_2_CREATION_TYPE =
      ImmutableSet.of(ProductCreationType.FLOW2.getProductCreationType(),
          ProductCreationType.FLOW2_FBB.getProductCreationType(),
          ProductCreationType.FLOW2_WEB.getProductCreationType(),
          ProductCreationType.FLOW2_API.getProductCreationType(),
          ProductCreationType.FLOW2_APP.getProductCreationType());

  @Autowired
  private GcsService gcsService;

  @Autowired
  private GcsProperties gcsProperties;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.source.directory}")
  private String fullImageSourceDirectory;

  @Value("${validate.image.exists.on.internal.update}")
  private boolean validateImageExistsOnInternalUpdate;


  @Override
  public boolean isFileExists(String filePath, boolean isFinalImage) {
    log.info("Checking for filePath : {}, isFinalImage : {} ", filePath, isFinalImage);
    if (isFinalImage) {
      return gcsService.isFileExists(gcsProperties.getFinalImageBucketName(), filePath);
    } else {
      return gcsService.isFileExists(gcsProperties.getSourceImageBucketName(), filePath);
    }
  }

  @Override
  public String checkImageAvailabilityAndGetUnavailableImages(String imageSourceDirectory,
      Map<String, CommonImagePathDto> imageHashCodeLocationPathMap) {
    return imageHashCodeLocationPathMap.values().stream().map(CommonImagePathDto::getLocationPath).distinct()
        .map(locationPath -> appendLocationPath(imageSourceDirectory, locationPath)).filter(
        imageLocationPath -> isImageExist(imageLocationPath, false))
        .findFirst().orElse(null);
  }

  @Override
  public String checkImageAvailabilityAndGetUnavailableImagesForEditedProducts(String imageSourceDirectory,
      Map<String, Image> imageHashCodeLocationPathMap) {
    return imageHashCodeLocationPathMap.values().stream().distinct().filter(image -> !image.isActive())
        .map(Image::getLocationPath).map(locationPath -> appendLocationPath(imageSourceDirectory, locationPath))
        .filter(imageLocationPath -> isImageExist(imageLocationPath, false)).findFirst().orElse(null);
  }

  @Override
  public String checkImageAvailability(List<ImageRequest> imageRequests, boolean isFinalImage) {
    return imageRequests.stream().distinct().map(ImageRequest::getAbsoluteImagePath).distinct()
      .filter(imageLocationPath -> isImageExist(imageLocationPath, isFinalImage)).findFirst().orElse(null);
  }

  @Override
  public String checkImageAvailabilityInFileStoreOrGcs(List<ImageRequest> imageRequests) {
    return imageRequests.stream().distinct().map(ImageRequest::getAbsoluteImagePath).distinct()
      .filter(this::checkFilePresentInGcsOrFileStoreAfterMigration).findFirst().orElse(null);
  }

  @Override
  public String generateFinalImageFullPath(String fullImageLocationPath) {
    if (fullImageLocationPath.contains(gcsProperties.getPathPrefix())) {
      return (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties
        .getFinalFullImageDirectory() + File.separator + fullImageLocationPath)
        .replace("//", "/");
    } else {
      return (fullImageSourceDirectory + File.separator + fullImageLocationPath)
        .replace("//", "/");
    }
  }

  @Override
  public String checkImageAvailability(ProductCreationRequest productCreationRequest, String productCreationType) {
    List<Image> imageLocationPaths = Stream.concat(
            productCreationRequest.getProductItemRequests().stream().map(ProductItemCreationRequest::getImages)
                .flatMap(List::stream),
            Optional.ofNullable(productCreationRequest.getCommonImages()).orElse(new ArrayList<>()).stream())
        .filter(image -> !image.isMarkForDelete()).collect(Collectors.toList());
    Set<String> locationPathAlreadyVerified = new HashSet<>();
    for (Image image : imageLocationPaths) {
      if (image.isActive()) {
        String imagePathFinalDir = checkAndGetImageLocationPathFromFinalLocation(image.getLocationPath());
        if (!locationPathAlreadyVerified.contains(imagePathFinalDir) && !isFileExists(imagePathFinalDir, true)) {
          return image.getLocationPath();
        }
        locationPathAlreadyVerified.add(imagePathFinalDir);
      } else if (validateFlow2Creation(image, productCreationType)) {
        log.info("checking in final location with image : {} , productCreationType : {} ", image, productCreationType);
        String imagePathFinalDir = checkAndGetImageLocationPathFromFinalLocation(image.getLocationPath());
        boolean isFlow2FinalImageExists = isFileExists(imagePathFinalDir, true);
        if (!locationPathAlreadyVerified.contains(imagePathFinalDir) && !isFlow2FinalImageExists) {
          String imagePathSourceDir = checkAndGetImageLocationPathFromSourceLocation(image.getLocationPath());
          log.info("checking in source location with image : {} , productCreationType : {} ", image,
              productCreationType);
          boolean isFlow2SourceImageExists = isFileExists(imagePathSourceDir, false);
          if (!locationPathAlreadyVerified.contains(imagePathSourceDir) && !isFlow2SourceImageExists) {
            return image.getLocationPath();
          } else {
            image.setActive(false);
            locationPathAlreadyVerified.add(imagePathSourceDir);
          }
        } else {
          image.setActive(true);
          locationPathAlreadyVerified.add(imagePathFinalDir);
        }
      } else if (image.getLocationPath().contains(gcsProperties.getPathPrefix())) {
        String imagePathSourceDir = checkAndGetImageLocationPathFromSourceLocation(image.getLocationPath());
        if (!locationPathAlreadyVerified.contains(imagePathSourceDir) && !isFileExists(imagePathSourceDir, false)) {
          return image.getLocationPath();
        }
        locationPathAlreadyVerified.add(imagePathSourceDir);
      } else {
        return image.getLocationPath();
      }
    }
    return null;
  }

  private String checkAndGetImageLocationPathFromFinalLocation(String locationPath) {
    return (gcsProperties.getFinalImageDirectory().concat(File.separator)
        .concat(gcsProperties.getFinalFullImageDirectory()).concat(File.separator).concat(locationPath)).replace("//",
        "/");
  }

  private String checkAndGetImageLocationPathFromSourceLocation(String locationPath) {
    return (gcsProperties.getSourceImageDirectory().concat(File.separator).concat(locationPath)).replace("//", "/");
  }

  private boolean validateFlow2Creation(Image image, String productCreationType) {
    return FLOW_2_CREATION_TYPE.contains(
        Optional.ofNullable(productCreationType).map(String::toUpperCase).orElse(StringUtils.EMPTY));
  }


  private String appendLocationPath(String imageSourceDirectory, String imageLocationPath) {
    if (imageLocationPath.contains(gcsProperties.getPathPrefix())) {
      return (gcsProperties.getSourceImageDirectory() + File.separator + imageLocationPath).replace("//", "/");
    } else {
      return (imageSourceDirectory + File.separator + imageLocationPath).replace("//", "/");
    }
  }

  private boolean isImageExist(String imageLocationPath, boolean isFinalImage) {
    if (imageLocationPath.contains(gcsProperties.getPathPrefix())) {
      return !isFileExists(imageLocationPath, isFinalImage);
    } else {
      File file = new File(imageLocationPath);
      return !file.exists();
    }
  }

  @Override
  public void editImageNameIfGcsEnabled(ProductCreationRequest productCreationRequest) {
    if (gcsProperties.isSourceImageEnabled() && !gcsProperties.getWhiteListedChannelIds()
        .contains(mandatoryParameterHelper.getChannelId())) {
      if (CollectionUtils.isNotEmpty(productCreationRequest.getCommonImages())) {
        updateImagePathWithPrefixForCreation(productCreationRequest.getCommonImages(), gcsProperties.getPathPrefix(),
            productCreationRequest.getProductCode());
      }

      if (CollectionUtils.isNotEmpty(productCreationRequest.getImages())) {
        updateImagePathWithPrefixForCreation(productCreationRequest.getImages(), gcsProperties.getPathPrefix(),
            productCreationRequest.getProductCode());
      }

      if (CollectionUtils.isNotEmpty(productCreationRequest.getProductItemRequests())) {
        for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
          updateImagePathWithPrefixForCreation(productItemCreationRequest.getImages(), gcsProperties.getPathPrefix(),
              productCreationRequest.getProductCode());
        }
      }
    }
  }

  @Override
  public void editImageNameIfGcsEnabled(UpdateItemsPriceStockImagesRequest request) {
    Set<String> newlyAddedCommonImage = new HashSet<>();
    if (((request.isNeedCorrection() && gcsProperties.isSourceImageEnabled()) || (!request.isNeedCorrection()
        && gcsProperties.isFinalImageEnabled())) && !gcsProperties.getWhiteListedChannelIds()
        .contains(mandatoryParameterHelper.getChannelId())) {
      updatePathForCopyToAllVariantImages(request.getCopyToAllVariantImages(), gcsProperties.getPathPrefix(), newlyAddedCommonImage);
      if (CollectionUtils.isNotEmpty(request.getProductItems())) {
        List<ProductLevel3SummaryDetailsImageRequest> newImages =
            request.getProductItems().stream().map(ProductPriceStockAndImagesRequest::getImages)
                .filter(CollectionUtils::isNotEmpty).flatMap(List::stream).filter(
                    productLevel3SummaryDetailsImageRequest ->
                        Constants.NEW.equals(productLevel3SummaryDetailsImageRequest.getReviewType())
                            || newlyAddedCommonImage.contains(productLevel3SummaryDetailsImageRequest.getLocationPath()))
                .collect(Collectors.toList()); if (CollectionUtils.isNotEmpty(newImages)) {
          updateImagePathWithPrefixForEdit(newImages, gcsProperties.getPathPrefix());
        }
      }
    }
  }

  @Override
  public void editImageNameIfGcsEnabled(ProductVariantUpdateRequest request) {
    Set<String> newlyAddedCommonImage = new HashSet<>();
    if (((request.isNeedCorrection() && gcsProperties.isSourceImageEnabled()) || (!request.isNeedCorrection()
        && gcsProperties.isFinalImageEnabled())) && !gcsProperties.getWhiteListedChannelIds()
        .contains(mandatoryParameterHelper.getChannelId())) {
      if (CollectionUtils.isNotEmpty(request.getCopyToAllVariantImages())) {
        updatePathForCopyToAllVariantImages(request.getCopyToAllVariantImages(), gcsProperties.getPathPrefix(), newlyAddedCommonImage);
      }
      if (CollectionUtils.isNotEmpty(request.getProductItems())) {
        List<ProductLevel3SummaryDetailsImageRequest> newImages =
            request.getProductItems().stream().map(ProductVariantPriceStockAndImagesRequest::getImages)
                .filter(CollectionUtils::isNotEmpty).flatMap(List::stream).filter(
                productLevel3SummaryDetailsImageRequest ->
                    Constants.NEW.equals(productLevel3SummaryDetailsImageRequest.getReviewType())
                        || newlyAddedCommonImage.contains(productLevel3SummaryDetailsImageRequest.getLocationPath()))
                .collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(newImages)) {
          updateImagePathWithPrefixForEdit(newImages, gcsProperties.getPathPrefix());
        }
      }
    }
  }

  @Override
  public void editImageNameIfGcsEnabled(ProductImageEditRequest request) {
    if (request.isImageAdded() && gcsProperties.isFinalImageEnabled()) {
      request.setImagePath(
          (gcsProperties.getPathPrefix() + File.separator + request.getImagePath()).replace("//", "/"));
    }
  }

  @Override
  public void editImageNameIfGcsEnabled(Product newProduct, Product savedProduct) {
    if (CollectionUtils.isNotEmpty(newProduct.getProductImages())) {
      Set<String> existingProductImages =
          Optional.ofNullable(savedProduct.getProductImages()).orElse(new ArrayList<>()).stream()
              .map(ProductImage::getLocationPath).collect(Collectors.toSet());
      for (ProductImage productImage : newProduct.getProductImages()) {
        if (!existingProductImages.contains(productImage.getLocationPath()) && gcsProperties.isFinalImageEnabled()
            && !productImage.getLocationPath().contains(gcsProperties.getPathPrefix())) {
          productImage.setLocationPath(
              gcsProperties.getPathPrefix() + com.gdn.x.mta.distributiontask.model.Constants.DELIMITER_SLASH
                  + productImage.getLocationPath());
        }
      }
    }

    if (CollectionUtils.isNotEmpty(newProduct.getProductItems())) {
      Map<String, List<ProductItemImage>> productItemImageMap =
          Optional.ofNullable(savedProduct.getProductItems()).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(ProductItem::getId, ProductItem::getProductItemImages));
      for (ProductItem productItem : newProduct.getProductItems()) {
        Set<String> existingItemImage =
            Optional.ofNullable(productItemImageMap.get(productItem.getId())).orElse(new ArrayList<>()).stream()
                .map(ProductItemImage::getLocationPath).collect(Collectors.toSet());
        if (CollectionUtils.isNotEmpty(productItem.getProductItemImages())) {
          for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
            if (!existingItemImage.contains(productItemImage.getLocationPath()) && gcsProperties.isFinalImageEnabled()
                && !productItemImage.getLocationPath().contains(gcsProperties.getPathPrefix())) {
              productItemImage.setLocationPath(
                  gcsProperties.getPathPrefix() + com.gdn.x.mta.distributiontask.model.Constants.DELIMITER_SLASH
                      + productItemImage.getLocationPath());
            }
          }
        }
      }
    }
  }

  private static void updatePathForCopyToAllVariantImages(
      List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages, String prefix, Set<String> newlyAddedCommonImage) {
    List<ProductLevel3SummaryDetailsImageRequest> newCopyToAllVariantImages =
        Optional.ofNullable(copyToAllVariantImages).orElse(new ArrayList<>()).stream().filter(
            productLevel3SummaryDetailsImageRequest -> Constants.NEW.equals(
                productLevel3SummaryDetailsImageRequest.getReviewType())).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(newCopyToAllVariantImages)) {
      newlyAddedCommonImage.addAll(
          newCopyToAllVariantImages.stream().map(ProductLevel3SummaryDetailsImageRequest::getLocationPath)
              .collect(Collectors.toSet()));
      updateImagePathWithPrefixForEdit(newCopyToAllVariantImages, prefix);
    }
  }

  private static void updateImagePathWithPrefixForEdit(List<ProductLevel3SummaryDetailsImageRequest> images,
      String prefix) {
    for (ProductLevel3SummaryDetailsImageRequest image : images) {
      image.setLocationPath(prefix + Constants.DELIMITER_SLASH + image.getLocationPath());
    }
  }

  private static void updateImagePathWithPrefixForCreation(List<Image> images, String prefix, String productCode) {
    for (Image image : images) {
      if (image.getLocationPath().contains(productCode)) {
        image.setLocationPath(prefix + Constants.DELIMITER_SLASH + image.getLocationPath());
      }
    }
  }

  @Override
  public String getCompleteSourceUrlPrefix() {
    return gcsProperties.getGcsDomainUrlPath() + File.separator +
        gcsProperties.getSourceImageBucketName() + File.separator +
        gcsProperties.getSourceImageDirectory() + File.separator;
  }

  @Override
  public String getImagePathPrefix() {
    return gcsProperties.getPathPrefix();
  }

  @Override
  public List<ImageRequest> getRevisedImageRequests(List<Image> images, String imageSourceDirectory) {
    List<ImageRequest> revisedImageList = new ArrayList<>();
    for (Image image : images) {
      if (!image.isMarkForDelete() && image.isRevised()) {
        ImageRequest imageRequest = new ImageRequest(generateImageName(image.getLocationPath()),
            generateImagePath(image.getLocationPath(), imageSourceDirectory), image.getHashCode());
        imageRequest.setCommonImage(image.isCommonImage());
        revisedImageList.add(imageRequest);
      }
    }
    return revisedImageList;
  }

  private String generateImageName(String location) {
    String[] splitImageFilenameByDash = location.split(File.separator);
    return splitImageFilenameByDash[splitImageFilenameByDash.length - 1];
  }

  private String generateImagePath(String location, String imageSourceDirectory) {
    if (location.contains(gcsProperties.getPathPrefix())) {
      return (gcsProperties.getSourceImageDirectory() + File.separator + location).replace("//", "/");
    } else {
      return (imageSourceDirectory + File.separator + location).replace("//", "/");
    }
  }

  @Override
  public List<ImageRequest> getImageRequest(
    ProductLevel3SummaryDetailsImageRequest productLevel3SummaryDetailsImage) {
    ImageRequest imageRequest = new ImageRequest();
    String absoluteImagePath = StringUtils.EMPTY;
    absoluteImagePath = absoluteImagePath.concat(
            (productLevel3SummaryDetailsImage.getLocationPath().contains(gcsProperties.getPathPrefix()) ?
                gcsProperties.getFinalImageDirectory().concat(File.separator)
                    .concat(gcsProperties.getFinalFullImageDirectory()) :
                fullImageSourceDirectory).concat(File.separator).concat(productLevel3SummaryDetailsImage.getLocationPath()))
        .replace("//", "/");
    imageRequest.setAbsoluteImagePath(absoluteImagePath);
    String[] splitImageFilenameByDash =
      productLevel3SummaryDetailsImage.getLocationPath().split(File.separator);
    imageRequest.setImageName(splitImageFilenameByDash[splitImageFilenameByDash.length - 1]);
    return Arrays.asList(imageRequest);
  }

  @Override
  public boolean checkFilePresentInGcsOrFileStoreAfterMigration(String imageLocationPath) {
    boolean migrationCompleted = gcsProperties.isFileStoreToGcsMigrationCompleted();
    boolean fileFoundInGcs = isFileExists(
      gcsProperties.getFinalImageDirectory() + Constants.DELIMITER_SLASH + gcsProperties
        .getFinalFullImageDirectory() + Constants.DELIMITER_SLASH + imageLocationPath, true);
    if (!fileFoundInGcs && !migrationCompleted) {
      return !(new File(fullImageSourceDirectory + File.separator + imageLocationPath).exists());
    } else {
      return true;
    }
  }

  @Override
  public void validateImageExists(List<ProductFieldHistory> changeFieldList) {
    if (validateImageExistsOnInternalUpdate) {
      Set<String> locationPaths = Optional.ofNullable(changeFieldList).orElse(new ArrayList<>()).stream()
          .filter(productFieldHistory -> StringUtils.isNotEmpty(productFieldHistory.getFieldName()))
          .filter(productFieldHistory -> productFieldHistory.getFieldName().contains(Constants.ITEM_IMAGES_ADDED))
          .map(ProductFieldHistory::getNewValue).map(String::valueOf).collect(Collectors.toSet());
      if (CollectionUtils.isNotEmpty(locationPaths)) {
        for (String locationPath : locationPaths) {
          boolean fileExists = isFileExists(checkAndGetImageLocationPathFromFinalLocation(locationPath), true);
          GdnPreconditions.checkArgument(fileExists, String.format(ErrorMessages.IMAGE_FILE_NOT_FOUND, locationPath));
        }
      }
    }
  }
}
