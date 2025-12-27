package com.gdn.x.mta.distributiontask.service.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import com.gdn.x.mta.distributiontask.service.api.GcsService;
import com.gdn.x.mta.distributiontask.service.impl.PDTExceptions.ExceptionUtil;
import com.gdn.x.mta.distributiontask.service.impl.config.GcsProperties;
import com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class FileStorageServiceImpl implements FileStorageService {

  private static final String CATALOG_IMAGE = "catalog-image";

  @Autowired
  private GcsService gcsService;

  @Autowired
  private GcsProperties gcsProperties;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.final.directory}")
  private String fullImageDirectory;

  @Override
  public void deleteOriginalImages(String locationPath) {
    if (locationPath.contains(CATALOG_IMAGE)) {
      deleteFromGcs(locationPath);
    } else {
      File image = new File(imageSourceDirectory + locationPath);
      if (image.exists()) {
        image.delete();
      }
    }
  }

@Override
  public void deleteFromGcs(String locationPath) {
      String bucketName = gcsProperties.getSourceImageBucketName();
  boolean isDeleted = gcsService.deleteFile(bucketName,
      (gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
      if (isDeleted) {
        log.info("Deleted image from GCS with location : {}", locationPath);
      } else {
        log.error("Failed to delete image from GCS with location : {}", locationPath);
      }
  }

  @Override
  public void validateProductImages(Product product)
      throws Exception {
    //Validate for Non active images
    Set<String> imageLocationPathForNonActiveProductImages =
        Optional.ofNullable(product.getProductImages()).orElse(new ArrayList<>()).stream()
            .filter(image -> !image.isMarkForDelete()).filter(productImage -> !productImage.isActive())
            .map(ProductImage::getLocationPath).collect(Collectors.toSet());
    Set<String> imageLocationPathForNonActiveProductItemImages =
        Optional.ofNullable(product.getProductItems()).orElse(new ArrayList<>()).stream()
            .map(productItem -> Optional.ofNullable(productItem.getProductItemImages()).orElse(new ArrayList<>()))
            .flatMap(List::stream).filter(image -> !image.isMarkForDelete()).filter(image -> !image.isActive())
            .map(ProductItemImage::getLocationPath).collect(Collectors.toSet());
    imageLocationPathForNonActiveProductImages.addAll(imageLocationPathForNonActiveProductItemImages);
    String unavailableImageName =
        imageLocationPathForNonActiveProductImages.stream().filter(this::isSourceImageFileExist).findFirst()
            .orElse(StringUtils.EMPTY);
    ExceptionUtil.checkConditions(StringUtils.isEmpty(unavailableImageName),
        DistributionProductMessageUtil.IMAGE_NOT_FOUND + ": " + unavailableImageName);

    //Validate for active images
    Set<String> imageLocationPathForActiveProductImages =
        Optional.ofNullable(product.getProductImages()).orElse(new ArrayList<>()).stream()
            .filter(image -> !image.isMarkForDelete()).filter(ProductImage::isActive).map(ProductImage::getLocationPath)
            .collect(Collectors.toSet());
    Set<String> imageLocationPathForActiveProductItemImages =
        Optional.ofNullable(product.getProductItems()).orElse(new ArrayList<>()).stream()
            .map(productItem -> Optional.ofNullable(productItem.getProductItemImages()).orElse(new ArrayList<>()))
            .flatMap(List::stream).filter(image -> !image.isMarkForDelete()).filter(ProductItemImage::isActive)
            .map(ProductItemImage::getLocationPath).collect(Collectors.toSet());
    imageLocationPathForActiveProductImages.addAll(imageLocationPathForActiveProductItemImages);
    String unavailableActiveImageName =
        imageLocationPathForActiveProductImages.stream().filter(this::isFinalImageFileExist).findFirst()
            .orElse(StringUtils.EMPTY);
    ExceptionUtil.checkConditions(StringUtils.isEmpty(unavailableActiveImageName),
        DistributionProductMessageUtil.ACTIVE_IMAGE_NOT_FOUND + ": " + unavailableActiveImageName);
  }

  @Override
  public boolean isSourceImageFileExist(String imageLocationPath) {
    if (imageLocationPath.contains(gcsProperties.getPathPrefix())) {
      return !gcsService.isFileExists(gcsProperties.getSourceImageBucketName(),
          gcsProperties.getSourceImageDirectory() + Constants.DELIMITER_SLASH + imageLocationPath);
    } else {
      return !new File(imageSourceDirectory + File.separator + imageLocationPath).exists();
    }
  }

  @Override
  public boolean isFinalImageFileExist(String imagePath) {
    boolean migrationCompleted = gcsProperties.isFileStoreToGcsMigrationCompleted();
    boolean fileFoundInGcs = gcsService.isFileExists(gcsProperties.getFinalImageBucketName(),
        (gcsProperties.getFinalImageDirectory() + Constants.DELIMITER_SLASH + gcsProperties.getFinalFullImageDirectory() + Constants.DELIMITER_SLASH
            + imagePath).replaceAll("//","/"));
    if (!fileFoundInGcs && !migrationCompleted) {
      return !(new File(fullImageDirectory + File.separator + imagePath).exists());
    } else {
      return !fileFoundInGcs;
    }
  }

  @Override
  public void updateNewlyAddedImagePath(Product newProduct, Product existingProduct) {
    if (CollectionUtils.isNotEmpty(newProduct.getProductImages())) {
      Set<String> existingProductImages = Stream.concat(
          Optional.ofNullable(existingProduct.getProductImages()).orElse(new ArrayList<>()).stream()
              .map(ProductImage::getLocationPath).collect(Collectors.toSet()).stream(),
          Optional.ofNullable(existingProduct.getProductItems()).orElse(new ArrayList<>()).stream()
              .map(ProductItem::getProductItemImages).flatMap(List::stream).map(ProductItemImage::getLocationPath)
              .collect(Collectors.toSet()).stream()).collect(Collectors.toSet());
      for (ProductImage productImage : newProduct.getProductImages()) {
        if (!existingProductImages.contains(productImage.getLocationPath()) && !productImage.isActive()
          && gcsProperties.isSourceImageEnabled() && !productImage.getLocationPath()
            .contains(gcsProperties.getPathPrefix())) {
          productImage.setLocationPath(
              gcsProperties.getPathPrefix() + Constants.DELIMITER_SLASH + productImage.getLocationPath());
        }
      }
    }

    if (CollectionUtils.isNotEmpty(newProduct.getProductItems())) {
      Map<String, List<ProductItemImage>> productItemImageMap =
          Optional.ofNullable(existingProduct.getProductItems()).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(ProductItem::getId, ProductItem::getProductItemImages));
      for (ProductItem productItem : newProduct.getProductItems()) {
        Set<String> existingItemImage =
            Optional.ofNullable(productItemImageMap.get(productItem.getId())).orElse(new ArrayList<>()).stream()
                .map(ProductItemImage::getLocationPath).collect(Collectors.toSet());
        if (CollectionUtils.isNotEmpty(productItem.getProductItemImages())) {
          for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
            if (!existingItemImage.contains(productItemImage.getLocationPath()) && !productItemImage.isActive()
                && gcsProperties.isSourceImageEnabled() && !productItemImage.getLocationPath()
                .contains(gcsProperties.getPathPrefix())) {
              productItemImage.setLocationPath(
                  gcsProperties.getPathPrefix() + Constants.DELIMITER_SLASH + productItemImage.getLocationPath());
            }
          }
        }
      }
    }
  }
}
