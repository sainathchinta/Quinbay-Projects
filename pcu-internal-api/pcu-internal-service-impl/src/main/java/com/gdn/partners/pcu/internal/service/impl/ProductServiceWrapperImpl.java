package com.gdn.partners.pcu.internal.service.impl;

import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.CacheProductService;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 15/01/2019 AD.
 */
@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductServiceWrapperImpl implements ProductServiceWrapper {

  @Autowired
  private ProductService productService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private CacheProductService cacheProductService;

  @Override
  @Transactional(readOnly = false)
  public String updateProduct(String requestId, String userType, ProductRequest productRequest,
      boolean internalFlow3AddProduct, boolean isActive) throws Exception {
    String presentCategory;
    if (Constants.USER_TYPE_INTERNAL.equals(userType) && internalFlow3AddProduct) {
      presentCategory = updateProductForReview(productRequest);
    } else {
      presentCategory = updateProduct(requestId, productRequest, isActive);
    }
    String categoryCode = productRequest.getProductCategories().get(0).getCategory().getCategoryCode();
    String internalActivationInterval =
        this.categoryService.findInternalActivationIntervalInDaysByCategoryCode(categoryCode);
    this.cacheProductService.removeCurrentUserFromProductView(productRequest.getProductCode(),
        productRequest.getUpdatedBy());
    productService.saveHistoryIfCategoryChanged(productRequest.getProductCode(), presentCategory,
        productRequest.getProductCategories().get(0).getCategory().getCategoryCode());
    return internalActivationInterval;
  }

  private String updateProduct(String requestId, ProductRequest productRequest, boolean isActive) throws Exception{
    this.productService.updateProduct(generateImageHashCode(productRequest), isActive);
    ProductDetailResponse savedProduct = this.productService.findProduct(productRequest.getId());
    this.validateBeforeUpdateProductItems(requestId, savedProduct);
    if (productRequest.getProductItems().size() != savedProduct.getProductItemResponses().size()) {
      productRequest.setVersion(savedProduct.getVersion());
      productRequest.getProductItems().clear();
      savedProduct.getProductItemResponses().forEach(
          productItemResponse -> setSavedProductItemRequest(productItemResponse, productRequest));
      this.productService.updateProduct(generateImageHashCode(productRequest), isActive);
    }
    return savedProduct.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
  }

  private String updateProductForReview(ProductRequest productRequest) throws Exception{
    this.productService.updateProductAndPublishToPDT(generateImageHashCode(productRequest));
    ProductDetailResponse savedProduct = this.productService.findProduct(productRequest.getId());
    if (productRequest.getProductItems().size() != savedProduct.getProductItemResponses().size()) {
      productRequest.setVersion(savedProduct.getVersion());
      productRequest.getProductItems().clear();
      savedProduct.getProductItemResponses().forEach(
          productItemResponse -> setSavedProductItemRequest(productItemResponse, productRequest));
      this.productService.updateProductAndPublishToPDT(generateImageHashCode(productRequest));
    }
    return savedProduct.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
  }

  private ProductItemRequest setSavedProductItemRequest(ProductItemResponse savedProductItem,
      ProductRequest productRequest) {
    ProductItemRequest itemRequest = new ProductItemRequest();
    BeanUtils.copyProperties(savedProductItem, itemRequest);
    itemRequest.setImages(ResponseHelper.toImages(savedProductItem.getImages()));
    itemRequest.setActivated(productRequest.isActivated());
    itemRequest.setViewable(productRequest.isViewable());
    itemRequest.setMarkForDelete(productRequest.isMarkForDelete());
    itemRequest.setProductItemAttributeValues(
        this.convertProductItemAttributeValueResponseToProductItemAttributeValueRequest(
            savedProductItem.getProductItemAttributeValueResponses()));
    if (StringUtils.isEmpty(itemRequest.getUpcCode())) {
      itemRequest.setUpcCode(this.productService.generateBarcode());
    }
    productRequest.getProductItems().add(itemRequest);
    return itemRequest;
  }

  private List<ProductItemAttributeValueRequest> convertProductItemAttributeValueResponseToProductItemAttributeValueRequest(
      List<ProductItemAttributeValueResponse> source) {
    List<ProductItemAttributeValueRequest> target = new ArrayList<>();
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    for (ProductItemAttributeValueResponse productItemAttributeValueResponse : source) {
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productItemAttributeValueResponse.getAttributeResponse(),
          attributeRequest);
      productItemAttributeValueRequest.setAttribute(attributeRequest);
      BeanUtils.copyProperties(productItemAttributeValueResponse, productItemAttributeValueRequest);
      target.add(productItemAttributeValueRequest);
    }
    return target;
  }

  private ProductRequest generateImageHashCode(ProductRequest request) throws Exception {
    request.getImages().stream()
        .filter(image -> !image.isMarkForDelete())
        .forEach(image -> setImageHashCode(image));
    request.getProductItems().stream()
        .map(productItemRequest -> productItemRequest.getImages())
        .flatMap(List::stream)
        .filter(image -> !image.isMarkForDelete())
        .forEach(image -> setImageHashCode(image));
    return request;
  }

  private void setImageHashCode(Image image) {
    try {
      MessageDigest messageDigest = MessageDigest.getInstance("MD5");
      String[] splitLocationPathByDash = image.getLocationPath().split("/");
      messageDigest.update(splitLocationPathByDash[splitLocationPathByDash.length - 1].getBytes());
      image.setHashCode(generateHashCode(messageDigest));
    } catch (Exception ex) {
      log.error("Error while setting imageHashCode. imageId:{}", image.getId());
    }
  }

  private String generateHashCode(MessageDigest messageDigest) throws Exception {
    byte[] digestBytes = messageDigest.digest();
    StringBuilder hashcode = new StringBuilder();
    for (int i = 0; i < digestBytes.length; i++) {
      hashcode.append(Integer.toString((digestBytes[i] & 0xff) + 0x100, 16).substring(1));
    }
    return hashcode.toString();
  }

  private void validateBeforeUpdateProductItems(String requestId,
      ProductDetailResponse savedProduct) {
    if (Objects.isNull(savedProduct)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "product details not found!, requestId = " + requestId);
    } else if (CollectionUtils.isEmpty(savedProduct.getProductItemResponses())) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "product items details is empty!, requestId = " + requestId);
    }
  }
}
