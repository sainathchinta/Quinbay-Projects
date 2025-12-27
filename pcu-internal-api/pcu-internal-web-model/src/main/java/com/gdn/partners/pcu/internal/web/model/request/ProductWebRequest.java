package com.gdn.partners.pcu.internal.web.model.request;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 13/12/2018 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class ProductWebRequest extends BaseWebRequest{

  private String id;
  private Long version;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private byte[] description;
  private byte[] longDescription;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String uniqueSellingPoint;
  private String uom;
  private List<ProductCategoryWebRequest> productCategories;
  private List<ProductAttributeWebRequest> productAttributes;
  private List<ProductItemWebRequest> productItems;
  private boolean activated;
  private boolean viewable;
  private String productStory;
  private String specificationDetail;
  private String url;
  private List<ImageRequest> images;
  private boolean promoSKU;
  private String createdBy;
  private Date createdDate;
  private String notes;
  private boolean isMarginExceed;
  private boolean postLive;
  private boolean reviewPending;
  private String forceReviewNotes;
  private String difficultyLevel;
  private boolean edited;
  private Integer productType;
  private NeedRevisionNotesWebRequest needRevisionNotes;
}
