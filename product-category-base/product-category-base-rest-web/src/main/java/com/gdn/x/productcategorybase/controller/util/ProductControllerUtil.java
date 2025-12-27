package com.gdn.x.productcategorybase.controller.util;

import java.util.List;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;

public class ProductControllerUtil {

  public static List<ProductImageResponse> convertProductImagesResponse(List<ProductImageSingle> result,
      boolean mainImage) {
    List<ProductImageResponse> productImageResponses;
    if (mainImage) {
      productImageResponses = result.stream().filter(ProductImageSingle::isMainImages).map(
          img -> ProductImageResponse.builder().mainImage(img.isMainImages()).locationPath(img.getLocationPath())
              .productId(img.getProductId()).sequence(img.getSequence()).active(img.isActive())
              .originalImage(img.getOriginalImage()).build()).collect(Collectors.toList());
    } else {
      productImageResponses = result.stream().map(
          img -> ProductImageResponse.builder().mainImage(img.isMainImages()).locationPath(img.getLocationPath())
              .productId(img.getProductId()).sequence(img.getSequence()).active(img.isActive())
              .originalImage(img.getOriginalImage()).build()).collect(Collectors.toList());
    }
    return productImageResponses;
  }

  /**
   * Convert ProductImageDTO to ProductImageResponse
   * 
   * @param productImageDto
   * @return
   */
  public static List<ProductImageResponse> convertImageDtoToResponse(List<ProductImageDTO> productImageDto) {
    return productImageDto.stream()
      .map(product-> ProductImageResponse.builder()
          .productId(product.getProductId())
          .productCode(product.getProductCode())
          .locationPath(product.getLocationPath())
          .mainImage(product.isMainImage())
          .sequence(product.getSequence())
          .build())
      .collect(Collectors.toList());
  }
}
