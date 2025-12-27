package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ItemLevel4ListingWebResponse {
  private static final long serialVersionUID = -1864286363046685128L;
  private String itemSku;
  private String itemCode;
  private String itemCatentryId;
  private String productSku;
  private String merchantCode;
  private String storeId;
  private String categoryCode;
  private Boolean isSynchronized;
  private Boolean isArchived;
  private Boolean cncActivated;
  private Boolean freeSample;
  private Boolean forceReview;
  private Boolean markForDelete;
  private Date createdDate;
  private Date updatedDate;
  private String generatedItemName;
  private String mainImageUrl;
  private boolean active;
  private String brand;
  private String createdBy;
  private String updatedBy;
  private Boolean off2OnChannelActive;
  private String sellerSku;
  private String upcCode;
  private String pdpUrl;
  private List<String> pickupPointCodes;
  private List<String> fbbPickupPointCodes;
  private List<BundleItemWebResponse> bundleItemResponses = new ArrayList<>();
  private boolean bundleRecipeEditable;
  private boolean sharedProduct;
}