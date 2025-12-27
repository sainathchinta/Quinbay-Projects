package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductDetailWebResponse extends VendorProductDetailWebResponse {

  private String id;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double weight;
  private Double height;
  private Double shippingWeight;
  private byte[] description;
  private byte[] longDescription;
  private String brand;
  private String brandCode;
  private String brandApprovalStatus;
  private String uniqueSellingPoint;
  private String uom;
  private boolean activated;
  private boolean viewable;
  private String productStory;
  private String specificationDetail;
  private String url;
  private List<ImageWebResponse> images;
  private Integer productType;
  private boolean promoSKU;
  private Set<ProductItemWebResponse> productItemResponses;
  private List<ProductAttributeWebResponse> productAttributeResponses;
  private List<ProductCategoryWebResponse> productCategoryResponses;
  private List<String> categories;
  private List<String> categoriesEnglish;
  private String documentType;
  private String createdBy;
  private Date createdDate;
  private Long version;
  private String businessPartnerCode;
  private String reviewerNotes;
  private boolean postLive;
  private boolean reviewPending;
  private boolean restrictedKeywordsPresent;
  private boolean enableImageFeedback;
  private String commissionType;
  private boolean internationalFlag;
  private boolean edited;
  private PreOrderResponse preOrder;
  private NeedRevisionNotesWebResponse needRevisionNotes;
  private boolean revised;
  private List<RestrictedKeywordsDetectedWebResponse> restrictedKeywordsDetected;
  private List<String> revisedFields;
  private List<String> editedFields;
  private String reviewType;
  private String commonImageThumbnailPath;
  private boolean commonImageThumbnailActive;
  private List<String> imageViolations;
  private String predictedBrand;
  private boolean markForDelete;
  private boolean officialSeller;
  private String assignedTo;
  private String sellerStatus;
  private boolean appealedProduct;
  private boolean showProductUrl;
  private String sizeChartCode;
  private String sizeChartBusinessPartnerCode;
  private String sizeChartName;
  private boolean pureInstore;
  private String productDetailPageLink;
  private String sourceEn;
  private String sourceId;
  private String reason;
  private String productCreationType;
  private boolean aiGeneratedBrand;
  private boolean aiGeneratedCategory;
  private String officer;
}
