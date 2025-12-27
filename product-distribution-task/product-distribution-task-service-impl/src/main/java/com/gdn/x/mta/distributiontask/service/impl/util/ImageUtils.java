package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ImageUtils {

  private static FileStorageService fileStorageService;

  public static void setFileStorageService(FileStorageService fileStorageService) {
    ImageUtils.fileStorageService = fileStorageService;
  }

  public static void setActiveFlagInProductAndItemImages(Product product) {
    List<ProductImage> inactiveProductImages = new ArrayList<>();
    List<ProductItemImage> inactiveProductItemImages = new ArrayList<>();
    Map<String, Boolean> locationAndActiveFlagMap = new HashMap<>();

    if (CollectionUtils.isNotEmpty(product.getProductImages())) {
      inactiveProductImages = product.getProductImages().stream().filter(productImage -> !productImage.isActive())
          .filter(productImage -> StringUtils.isNotEmpty(productImage.getLocationPath())).collect(Collectors.toList());
    }

    if (CollectionUtils.isNotEmpty(product.getProductItems())) {
      inactiveProductItemImages = product.getProductItems().stream()
          .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemImages()))
          .flatMap(productItem -> productItem.getProductItemImages().stream())
          .filter(productItemImage -> !productItemImage.isActive())
          .filter(productItemImage -> StringUtils.isNotEmpty(productItemImage.getLocationPath()))
          .collect(Collectors.toList());
    }

    Set<String> imagePaths = Stream.concat(inactiveProductImages.stream().map(ProductImage::getLocationPath),
        inactiveProductItemImages.stream().map(ProductItemImage::getLocationPath)).collect(Collectors.toSet());

    if (CollectionUtils.isNotEmpty(imagePaths)) {
      for (String imagePath : imagePaths) {
        boolean isActiveImageExist = fileStorageService.isFinalImageFileExist(imagePath);
        locationAndActiveFlagMap.put(imagePath, !isActiveImageExist);
      }

      inactiveProductImages.forEach(
          productImage -> productImage.setActive(locationAndActiveFlagMap.get(productImage.getLocationPath())));
      inactiveProductItemImages.forEach(productItemImage -> productItemImage.setActive(
          locationAndActiveFlagMap.get(productItemImage.getLocationPath())));
    }

  }

  public static boolean overrideMainImageIfNotPresent(boolean overrideMainImageFlag,
    List<DistributionProductImageResponse> images) {
    return overrideMainImageFlag && CollectionUtils.isNotEmpty(images) && images.stream()
      .noneMatch(DistributionProductImageResponse::isMainImage);
  }
}
