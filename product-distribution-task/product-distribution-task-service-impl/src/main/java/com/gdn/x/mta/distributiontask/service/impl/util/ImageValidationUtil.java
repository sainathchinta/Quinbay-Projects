package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.gdn.x.mta.distributiontask.model.ProductImage;

import org.apache.commons.lang3.tuple.Pair;

import com.gdn.x.mta.distributiontask.service.impl.PDTExceptions.ExceptionUtil;
import com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil;

public class ImageValidationUtil {

  public static void validateProductImages(List<ProductImage> productImages, String imageSourceDirectory,
      String fullImageDirectory) throws Exception {
    //Validate for Non active images
    Map<String, String> imageLocationPathForNonActiveImages = productImages.stream().filter(image -> !image.isMarkForDelete())
        .filter(productImage -> !productImage.isActive())
        .collect(Collectors.toMap(ProductImage::getLocationPath, ProductImage::getLocationPath));
    String unavailableImageName = imageLocationPathForNonActiveImages.values().stream().distinct().map(
        imageLocationPath -> Pair
            .of(new File(imageSourceDirectory + File.separator + imageLocationPath), imageLocationPath))
        .filter(imageFileAndLocationPathPair -> !imageFileAndLocationPathPair.getLeft().exists()).findFirst()
        .map(Pair::getRight).orElse(StringUtils.EMPTY);
    ExceptionUtil.checkConditions(StringUtils.isEmpty(unavailableImageName),
        DistributionProductMessageUtil.IMAGE_NOT_FOUND + ": " + unavailableImageName);

    //Validate for active images
    Map<String, String> imageLocationPathForActiveImages = productImages.stream().filter(image -> !image.isMarkForDelete())
        .filter(ProductImage::isActive)
        .collect(Collectors.toMap(ProductImage::getLocationPath, ProductImage::getLocationPath));
    String unavailableActiveImageName = imageLocationPathForActiveImages.values().stream().distinct().map(
        imageLocationPath -> Pair
            .of(new File(fullImageDirectory + File.separator + imageLocationPath), imageLocationPath))
        .filter(imageFileAndLocationPathPair -> !imageFileAndLocationPathPair.getLeft().exists()).findFirst()
        .map(Pair::getRight).orElse(StringUtils.EMPTY);
    ExceptionUtil.checkConditions(StringUtils.isEmpty(unavailableActiveImageName),
        DistributionProductMessageUtil.ACTIVE_IMAGE_NOT_FOUND + ": " + unavailableActiveImageName);
  }
}
