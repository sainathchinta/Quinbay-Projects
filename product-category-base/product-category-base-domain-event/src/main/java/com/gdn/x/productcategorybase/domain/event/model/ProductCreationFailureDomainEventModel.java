package com.gdn.x.productcategorybase.domain.event.model;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCreationFailureDomainEventModel extends GdnBaseDomainEventModel {

  private String storeId;
  private String requestId;
  private String productCode;
  private String productName;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String description;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String uniqueSellingPoint;
  private String uom;
  private List<ProductCategoryDomainEventModel> productCategories;
  private List<ProductAttributeDomainEventModel> productAttributes;
  private List<ImageDomainEventModel> images;
  private List<ProductCreationFailureItemDomainEventModel> productItems;
  private boolean activated = false;
  private boolean viewable = false;
  private String url;
  private boolean promoSKU;
  private boolean isMarginExceed;
  private boolean forReview;
  private boolean postLive;
  private boolean reviewPending;
  private String createdMerchant;
  private String flowType;
  private Date createdDate;
  private String createdBy;
  private boolean markForDelete;
  private String errorMessage;
  private String sellerCode;
}
