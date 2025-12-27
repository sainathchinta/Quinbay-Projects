package com.gdn.x.mta.distributiontask.controller.util;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductListRequestDTO;
import com.gdn.x.mta.distributiontask.model.type.ProductLabels;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Created by virajjasani on 10/10/16.
 */
@Component
public class ProductConverterUtil {

  private static final String ID = "id";
  private static final String VERSION = "version";
  private static final String ORIGINAL_IMAGE = "originalImage";
  private static final String PDT_CHAR_ENCODING = "UTF-8";
  private static final String COMMA = ",";
  private final DateTimeFormatter DATE_TIME_FORMATTER =
      DateTimeFormat.forPattern("yyyy-MM-dd HH:mm");

  @Value("${vendor.product.labels.ordered}")
  private String vendorProductLabelsOrdered;


  public Product convertProductDetailResponseToProduct(ProductDetailResponse productDetailResponse,
      boolean isProductMigrated) throws Exception {
    Product product = new Product();
    BeanUtils.copyProperties(productDetailResponse, product, "productItemResponses",
        "productAttributeResponses", "productCategoryResponses", "categories", "name", "images",
        ID, VERSION);
    setProductCategoryInfo(productDetailResponse, product);
    product.setProductName(productDetailResponse.getName());
    product.setVideoUrl(productDetailResponse.getUrl());
    product.setProductCreatedDate(Calendar.getInstance().getTime());
    product.setRejectedCount(0);

    setProductImages(productDetailResponse, product);
    setProductAttributes(productDetailResponse, product);
    setProductItems(productDetailResponse, product);
    product.setCreatedBy(productDetailResponse.getCreatedBy());
    product.setCreatedDate(productDetailResponse.getCreatedDate());
    return product;
  }

  private void setProductItems(ProductDetailResponse productDetailResponse, Product product) {
    List<ProductItem> productItemList = new ArrayList<>();
    for (ProductItemResponse productItemResponse : productDetailResponse
        .getProductItemResponses()) {
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(productItemResponse, productItem, "images",
          "productItemAttributeValueResponses", ID, VERSION);
      List<ProductItemImage> productItemImageList = new ArrayList<>();
      for (Image image : productItemResponse.getImages()) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(image, productItemImage, ID, VERSION, ORIGINAL_IMAGE);
        if (Objects.nonNull(image.getOriginalImage())) {
          productItemImage.setOriginalImage(image.getOriginalImage());
        }
        productItemImage.setProductItem(productItem);
        productItemImage.setMainImage(image.isMainImages());
        productItemImageList.add(productItemImage);
      }
      productItem.setProduct(product);
      productItem.setProductItemImages(productItemImageList);
      productItemList.add(productItem);
    }
    product.setProductItems(productItemList);
  }

  private void setProductAttributes(ProductDetailResponse productDetailResponse, Product product) {
    List<ProductAttribute> productAttributeList = new ArrayList<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse
        .getProductAttributeResponses()) {
      ProductAttribute productAttribute = new ProductAttribute();
      BeanUtils.copyProperties(productAttributeResponse, productAttribute, "attribute",
          "productAttributeValues", ID, VERSION);
      productAttribute.setProduct(product);
      productAttribute.setName(productAttributeResponse.getAttribute().getName());
      productAttribute.setAttributeCode(productAttributeResponse.getAttribute().getAttributeCode());
      setProductAttributeValues(productAttributeResponse, productAttribute);
      productAttributeList.add(productAttribute);
    }
    product.setProductAttributes(productAttributeList);
  }

  private void setProductAttributeValues(ProductAttributeResponse productAttributeResponse,
      ProductAttribute productAttribute) {
    ProductAttributeValueResponse productAttributeValueResponse =
        productAttributeResponse.getProductAttributeValues().get(0);
    if (productAttributeValueResponse != null) {
      if (productAttributeValueResponse.getAllowedAttributeValue() != null) {
        productAttribute
            .setValue(productAttributeValueResponse.getAllowedAttributeValue().getValue());
        productAttribute.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
      } else if (productAttributeValueResponse.getPredefinedAllowedAttributeValue() != null) {
        productAttribute.setValue(
            productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue());
        productAttribute.setAttributeType(AttributeType.PREDEFINED_ATTRIBUTE.toString());
      } else {
        productAttribute.setValue(productAttributeValueResponse.getDescriptiveAttributeValue());
        productAttribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString());
      }
    }
  }

  private void setProductImages(ProductDetailResponse productDetailResponse, Product product) {
    List<ProductImage> productImageList = new ArrayList<>();
    for (Image image : productDetailResponse.getImages()) {
      ProductImage productImage = new ProductImage();
      BeanUtils.copyProperties(image, productImage, ID, VERSION, ORIGINAL_IMAGE);
      if (Objects.nonNull(image.getOriginalImage())) {
        productImage.setOriginalImage(image.getOriginalImage());
      }
      productImage.setMainImage(image.isMainImages());
      productImage.setProduct(product);
      productImageList.add(productImage);
    }
    product.setProductImages(productImageList);
  }

  private void setProductCategoryInfo(ProductDetailResponse productDetailResponse,
      Product product) {
    List<ProductCategoryResponse> productCategoryResponseList =
        productDetailResponse.getProductCategoryResponses();
    if (productCategoryResponseList != null && productCategoryResponseList.size() >= 1) {
      CategoryResponse categoryResponse =
          productCategoryResponseList.get(productCategoryResponseList.size() - 1).getCategory();
      product.setCategoryCode(categoryResponse.getCategoryCode());
      product.setCategoryName(categoryResponse.getName());
    }
  }

  public GdnRestListResponse<DistributionProductResponse> getFilterProductSummary(Pageable pageable, String requestId,
      Page<Product> productCollection) {
    long totalElements = productCollection.getTotalElements();
    List<DistributionProductResponse> response = new ArrayList<>();
    VendorDetailResponse vendorDetailResponse;
    for (Product collection : productCollection.getContent()) {
      DistributionProductResponse productListResponse = new DistributionProductResponse();
      BeanUtils.copyProperties(collection, productListResponse);
      if (Objects.nonNull(collection.getState())) {
        productListResponse.setState(WorkflowWebState.valueOf(collection.getState().toString()));
      }
      if (Objects.nonNull(collection.getCurrentVendor())) {
        vendorDetailResponse = new VendorDetailResponse();
        BeanUtils.copyProperties(collection.getCurrentVendor(), vendorDetailResponse);
        productListResponse.setCurrentVendor(vendorDetailResponse);
        productListResponse.setVendorName(collection.getCurrentVendor().getName());
      }
      productListResponse.setCreatedDate(collection.getProductCreatedDate());
      response.add(productListResponse);
    }
    return new GdnRestListResponse<>(null, null, true, response,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), totalElements), requestId);
  }

  public ProductListRequestDTO getProductListRequestServiceDTO(String storeId,
      ProductListRequest request) throws ParseException, UnsupportedEncodingException {
    ProductListRequestDTO productListRequestDTO = new ProductListRequestDTO();
    BeanUtils
        .copyProperties(request, productListRequestDTO, "startDate", "endDate", "workflowState");
    List<WorkflowWebState> statusList = request.getWorkflowWebState();
    if (!CollectionUtils.isEmpty(statusList)) {
      List<WorkflowState> workflowStates = new ArrayList<>();
      for (WorkflowWebState status : statusList) {
        workflowStates.add(WorkflowState.valueOf(status.toString()));
      }
      productListRequestDTO
          .setWorkflowState(workflowStates);
    }
    productListRequestDTO.setStoreId(storeId);
    if (request.getStartDate() != null) {
      productListRequestDTO.setStartDate(DATE_TIME_FORMATTER
          .parseDateTime(URLDecoder.decode(request.getStartDate(), PDT_CHAR_ENCODING)).toDate());
    }
    if (request.getEndDate() != null) {
      productListRequestDTO.setEndDate(DATE_TIME_FORMATTER
          .parseDateTime(URLDecoder.decode(request.getEndDate(), PDT_CHAR_ENCODING)).toDate());
    }
    return productListRequestDTO;
  }

  public List<DistributionProductResponse> convertProductsToDistributionProductResponse(Page<Product> productPage) {
    List<DistributionProductResponse> response = new ArrayList<>();
    for(Product product : productPage.getContent()) {
      DistributionProductResponse distributionProductResponse = setProductDetail(product, vendorProductLabelsOrdered);
      response.add(distributionProductResponse);
    }
    return response;
  }

  public List<DistributionProductResponse> convertToDistributionProductResponse(
      Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOPage, String vendorProductLabelsOrdered) {
    List<DistributionProductResponse> response = new ArrayList<>();
    for (ProductAndReviewerDetailsDTO productAndReviewerDetailsDTO :
      productAndReviewerDetailsDTOPage.getContent()) {
      DistributionProductResponse distributionProductResponse =
        setProductDetail(productAndReviewerDetailsDTO.getProduct(), vendorProductLabelsOrdered);
      setReviewerDetails(productAndReviewerDetailsDTO.getProductReviewer(), distributionProductResponse);
      response.add(distributionProductResponse);
    }
    return response;
  }

  private DistributionProductResponse setProductDetail(Product product, String vendorProductLabelsOrdered) {
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    BeanUtils.copyProperties(product, distributionProductResponse);
    if(Objects.nonNull(product.getState())) {
      distributionProductResponse.setState(WorkflowWebState.valueOf(product.getState().name()));
    }
    if(Objects.nonNull(product.getCurrentVendor())) {
      VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
      BeanUtils.copyProperties(product.getCurrentVendor(), vendorDetailResponse);
      vendorDetailResponse.setQcRequired(product.getCurrentVendor().isQcRequired());
      vendorDetailResponse.setAbleToReject(product.getCurrentVendor().isAbleToReject());
      distributionProductResponse.setCurrentVendor(vendorDetailResponse);
      distributionProductResponse.setVendorCode(vendorDetailResponse.getVendorCode());
      distributionProductResponse.setVendorName(vendorDetailResponse.getName());
    }
    distributionProductResponse.setRevised(product.isRevised());
    distributionProductResponse.setRestrictedKeywordsPresent(
      product.isRestrictedKeywordsPresent());
    distributionProductResponse.setAutoNeedRevision(product.isAutoNeedRevision());
    distributionProductResponse.setReviewType(
      Objects.nonNull(product.getReviewType()) ? product.getReviewType().name() : null);
    distributionProductResponse.setImageViolations(
        getOrderedImageViolations(product.getImageViolations() + COMMA + product.getTextViolations(),
            vendorProductLabelsOrdered));
    distributionProductResponse.setProductCreationType(product.getProductCreationType());
    return distributionProductResponse;
  }

  public List<String> getOrderedTextAndImageViolations(Product product, String vendorProductLabelsOrdered) {
    return getOrderedImageViolations(product.getImageViolations() + COMMA + product.getTextViolations(),
        vendorProductLabelsOrdered);
  }

  private List<String> getOrderedImageViolations(String imageViolations, String vendorProductLabelsOrdered) {
    if (StringUtils.isEmpty(imageViolations)) {
      return Collections.singletonList(ProductLabels.GOOD.getDescription());
    }
    List<String> imageViolationsSplit = Arrays.asList(imageViolations.split(COMMA));
    List<String> orderImageViolation = getProductLabels(vendorProductLabelsOrdered, imageViolationsSplit);
    if (imageViolationsSplit.contains(ProductLabels.PENDING.getDescription())) {
      return Collections.emptyList();
    }
    if (CollectionUtils.isEmpty(orderImageViolation)) {
      return Collections.singletonList(ProductLabels.GOOD.getDescription());
    }
    return orderImageViolation;
  }

  private List<String> getProductLabels(String vendorProductLabelsOrdered, List<String> imageViolationsSplit) {
    Map<String, String> predictionTypeAndDisplayNameMap = new LinkedHashMap<>();
    String[] vendorProductLabelList = vendorProductLabelsOrdered.split(COMMA);
    for (String vendorProductLabel : vendorProductLabelList) {
      String[] keyValue = vendorProductLabel.split(Constants.COLON);
      predictionTypeAndDisplayNameMap.put(keyValue[0], keyValue[1]);
    }
    return predictionTypeAndDisplayNameMap.entrySet().stream()
        .filter(predictionTypeAndDisplayName -> imageViolationsSplit.contains(predictionTypeAndDisplayName.getKey()))
        .map(Map.Entry::getValue).collect(Collectors.toList());
  }

  public DistributionProductResponse getProductListResponseWithoutProductId(String vendorCode, Product product) {
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    BeanUtils.copyProperties(product, distributionProductResponse);
    if (Objects.nonNull(product.getState())) {
      distributionProductResponse.setState(WorkflowWebState.valueOf(product.getState().toString()));
    }
    distributionProductResponse.setVendorCode(vendorCode);
    if (Objects.nonNull(product.getCurrentVendor())) {
      distributionProductResponse.setVendorName(product.getCurrentVendor().getName());
      Vendor productCurrentVendor = product.getCurrentVendor();
      VendorDetailResponse distributionProductResponseCurrentVendor = new VendorDetailResponse();
      BeanUtils.copyProperties(productCurrentVendor, distributionProductResponseCurrentVendor);
      distributionProductResponse.setCurrentVendor(distributionProductResponseCurrentVendor);
      distributionProductResponse.setVendorId(distributionProductResponseCurrentVendor.getId());
    }
    distributionProductResponse.setCreatedDate(product.getProductCreatedDate());
    WorkflowState workflowState = product.getState();
    if (Objects.nonNull(workflowState)) {
      setApprovalFlagsForProductResponse(distributionProductResponse, workflowState);
    }
    return distributionProductResponse;
  }

  private void setApprovalFlagsForProductResponse(DistributionProductResponse distributionProductResponse,
      WorkflowState workflowState) {
    if (WorkflowState.PASSED.equals(workflowState)) {
      distributionProductResponse.setProductApproved(true);
    }
  }

  public void setReviewerDetails(ProductReviewer productReviewer,
    DistributionProductDetailResponse response) {
    response.setProductApproverAssignee(productReviewer.getApproverAssignee());
    response.setProductApprovedDate(productReviewer.getApprovedDate());
    response.setProductAssignedDate(productReviewer.getAssignedDate());
  }

  public void setReviewerDetails(ProductReviewer productReviewer,
    DistributionProductResponse response) {
    response.setProductApproverAssignee(productReviewer.getApproverAssignee());
    response.setProductApprovedDate(productReviewer.getApprovedDate());
    response.setProductAssignedDate(productReviewer.getAssignedDate());
    response.setProductApproved(Objects.nonNull(productReviewer.getApprovedDate()));
  }
}
