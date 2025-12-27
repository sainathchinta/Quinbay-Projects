package com.gdn.x.mta.distributiontask.controller.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.mta.distributiontask.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.dto.BulkScreeningProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.BulkVendorProductActionsDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.RejectReasonDto;
import com.gdn.x.mta.distributiontask.model.type.DifficultyLevel;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkScreeningProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductItemRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Created by virajjasani on 21/09/16.
 */
@Component
@Slf4j
public class ModelConverterUtil {

  private static final String PRODUCT_IMAGES = "productImages";
  private static final String PRODUCT_ITEMS = "productItems";
  private static final String PRODUCT_ATTRIBUTES = "productAttributes";
  private static final String PRODUCT_ITEM_IMAGES = "productItemImages";
  private static final String PRODUCT_ITEM_ATTRIBUTES = "productItemAttributes";
  private static final String CURRENT_VENDOR = "currentVendor";
  private static final String IMAGE_DIFFICULTY_LEVEL = "imageDifficultyLevel";
  private static final String CONTENT_DIFFICULTY_LEVEL = "contentDifficultyLevel";
  private static final String BRAND = "Brand";
  private static final String CONTENT = "content";
  private static final String IMAGE = "image";
  private static final String IMAGE_VIOLATIONS = "imageViolations";
  private static final String PRODUCT_PREDICTION_SCORE = "productPredictionScore";
  private static final String EDITED = "edited";
  private static final String EDITED_STATE = "editedState";

  @Autowired
  ObjectMapper mapper;

  @Value("${throw.error.on.empty.item.images}")
  private boolean throwErrorOnEmptyItemImages;

  public Product convertProductDetailRequestToProductEntity(DistributionProductDetailRequest request,
      String approvalType) throws JsonProcessingException {
    Product product = new Product();
    try {
      BeanUtils.copyProperties(request, product, PRODUCT_IMAGES, PRODUCT_ITEMS, PRODUCT_ATTRIBUTES, CURRENT_VENDOR,
          IMAGE_DIFFICULTY_LEVEL, CONTENT_DIFFICULTY_LEVEL, IMAGE_VIOLATIONS, PRODUCT_PREDICTION_SCORE, EDITED_STATE,
          EDITED);
      setDifficultyLevel(request, approvalType, product);
      setProductImages(request, product);
      setProductAttributes(request, product);
      setProductNotes(request, product);
      setProductItems(request, product);
    } catch (Exception e) {
      log.error(
          "error while converting ProductDetailRequest to Product. productCode: {}",
          request.getProductCode(), e);
      throw e;
    }
    return product;
  }

  private void setProductNotes(DistributionProductDetailRequest request, Product product)
      throws JsonProcessingException {
    if (Objects.nonNull(request.getProductNotes())) {
      log.info("set product notes productCode: {} , productNotes {}", request.getProductCode(),
          request.getProductNotes());
      product.setProductNotes(mapper.writeValueAsString(request.getProductNotes()));
    }
  }

  private void setDifficultyLevel(DistributionProductDetailRequest request, String approvalType, Product product) {
    if (CONTENT.equalsIgnoreCase(approvalType)) {
      product.setContentDifficultyLevel(Enum.valueOf(DifficultyLevel.class, request.getDifficultyLevel()));
    } else if (IMAGE.equalsIgnoreCase(approvalType)) {
      product.setImageDifficultyLevel(Enum.valueOf(DifficultyLevel.class, request.getDifficultyLevel()));
    } else {
      product.setImageDifficultyLevel(Enum.valueOf(DifficultyLevel.class, DifficultyLevel.NA.name()));
      product.setContentDifficultyLevel(Enum.valueOf(DifficultyLevel.class, DifficultyLevel.NA.name()));
    }
  }

  private void setProductItems(DistributionProductDetailRequest request, Product product)
      throws JsonProcessingException {
    List<ProductItem> productItemList = new ArrayList<>();
    for (DistributionProductItemRequest distributionProductItemRequest : request
        .getProductItems()) {
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(distributionProductItemRequest, productItem, PRODUCT_ITEM_IMAGES,
          PRODUCT_ITEM_ATTRIBUTES);
      setProductItemImages(distributionProductItemRequest, productItem);
      setProductItemAttributes(distributionProductItemRequest, productItem);
      setProductItemNotes(distributionProductItemRequest, productItem);
      productItem.setProduct(product);
      productItemList.add(productItem);
    }
    product.setProductItems(productItemList);
  }

  private void setProductItemNotes(DistributionProductItemRequest distributionProductItemRequest,
      ProductItem productItem) throws JsonProcessingException {
    if (Objects.nonNull(distributionProductItemRequest.getItemNotes())) {
      log.info("set item notes for itemSkuCode : {}, itemNotes : {}", distributionProductItemRequest.getSkuCode(),
          distributionProductItemRequest.getItemNotes());
      productItem.setItemNotes(mapper.writeValueAsString(distributionProductItemRequest.getItemNotes()));
    }
  }

  private void setProductItemAttributes(
      DistributionProductItemRequest distributionProductItemRequest, ProductItem productItem) {
    List<ProductItemAttribute> productItemAttributeList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(distributionProductItemRequest.getProductItemAttributes())) {
      for (DistributionProductAttributeRequest distributionProductAttributeRequest : distributionProductItemRequest.getProductItemAttributes()) {
        ProductItemAttribute productItemAttribute = new ProductItemAttribute();
        BeanUtils.copyProperties(distributionProductAttributeRequest, productItemAttribute);
        productItemAttribute.setProduct(productItem);
        productItemAttributeList.add(productItemAttribute);
      }
      productItem.setProductItemAttributes(productItemAttributeList);
    }
  }

  private void setProductItemImages(DistributionProductItemRequest distributionProductItemRequest,
      ProductItem productItem) {
    List<ProductItemImage> productItemImageList = new ArrayList<>();
    if (throwErrorOnEmptyItemImages) {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(distributionProductItemRequest.getProductItemImages()),
          ErrorMessages.ITEM_IMAGE_MUST_NOT_BE_EMPTY);
    }
    for (DistributionProductImageRequest distributionProductImageRequest :
        distributionProductItemRequest.getProductItemImages()) {
      ProductItemImage productItemImage = new ProductItemImage();
      BeanUtils.copyProperties(distributionProductImageRequest, productItemImage, "originalImage");
      if (Objects.nonNull(distributionProductImageRequest.getOriginalImage())) {
        productItemImage.setOriginalImage(distributionProductImageRequest.getOriginalImage());
      }
      productItemImage.setProductItem(productItem);
      productItemImageList.add(productItemImage);
    }
    productItem.setProductItemImages(productItemImageList);
  }

  private void setProductAttributes(DistributionProductDetailRequest request, Product product) {
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    for (DistributionProductAttributeRequest distributionProductAttributeRequest : request
        .getProductAttributes()) {
      ProductAttribute productAttribute = new ProductAttribute();
      BeanUtils.copyProperties(distributionProductAttributeRequest, productAttribute);
      if (productAttribute.getName().equalsIgnoreCase(BRAND)
          && AttributeType.PREDEFINED_ATTRIBUTE.name().equalsIgnoreCase(
              productAttribute.getAttributeType())) {
        productAttribute.setValue(product.getBrand());
      }
      productAttribute.setProduct(product);
      productAttributeList.add(productAttribute);
    }
    product.setProductAttributes(productAttributeList);
  }

  private void setProductImages(DistributionProductDetailRequest request, Product product) {
    List<ProductImage> productImageList = new ArrayList<>();
    for (DistributionProductImageRequest distributionProductImageRequest : request
        .getProductImages()) {
      ProductImage productImage = new ProductImage();
      BeanUtils.copyProperties(distributionProductImageRequest, productImage, "originalImage");
      if (Objects.nonNull(distributionProductImageRequest.getOriginalImage())) {
        productImage.setOriginalImage(distributionProductImageRequest.getOriginalImage());
      }
      productImage.setProduct(product);
      productImageList.add(productImage);
    }
    product.setProductImages(productImageList);
  }

  public BulkVendorProductActionsDTO toBulkVendorProductActionsDTO(
      BulkVendorProductActionsRequest bulkVendorProductActionsRequest) {
    BulkVendorProductActionsDTO bulkVendorProductActionsDTO = new BulkVendorProductActionsDTO();
    bulkVendorProductActionsDTO.setBulkScreeningProductActionsRequests(
        Optional.ofNullable(bulkVendorProductActionsRequest.getBulkScreeningProductActionsRequests())
            .orElseGet(Collections::emptyList).stream().map(this::toBulkScreeningProductActionsDTO)
            .collect(Collectors.toList()));
    return bulkVendorProductActionsDTO;
  }

  private BulkScreeningProductActionsDTO toBulkScreeningProductActionsDTO(
      BulkScreeningProductActionsRequest bulkScreeningProductActionsRequest) {
    BulkScreeningProductActionsDTO bulkScreeningProductActionsDTO = new BulkScreeningProductActionsDTO();
    BeanUtils.copyProperties(bulkScreeningProductActionsRequest, bulkScreeningProductActionsDTO, "productCode");
    bulkScreeningProductActionsDTO.setProductCodes(bulkScreeningProductActionsRequest.getProductCodes());
    return bulkScreeningProductActionsDTO;
  }

  public static RejectProductDTO toRejectProductDTO(RejectProductVendorRequest rejectProductVendorRequest) {
    RejectProductDTO rejectProductDTO = new RejectProductDTO();
    rejectProductDTO.setProductCode(rejectProductVendorRequest.getProductCode());
    rejectProductDTO.setNotes(rejectProductVendorRequest.getNotes());
    rejectProductDTO.setBulkAction(rejectProductVendorRequest.isBulkAction());
    rejectProductDTO.setMerchantCommissionType(rejectProductVendorRequest.getMerchantCommissionType());
    RejectReasonRequest rejectReasonRequest = rejectProductVendorRequest.getRejectReasonRequest();
    RejectReasonDto rejectReasonDtoTarget = new RejectReasonDto();
    BeanUtils.copyProperties(rejectReasonRequest, rejectReasonDtoTarget);
    rejectProductDTO.setRejectReasonDto(rejectReasonDtoTarget);
    return rejectProductDTO;
  }

}
