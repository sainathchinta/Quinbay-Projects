package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemLevel4ListingResponse extends BaseResponse {

  private static final long serialVersionUID = 6264336584268102672L;
  private String itemSku;
  private String itemCode;
  private String itemCatentryId;
  private String productSku;
  private String sellerSku;
  private String upcCode;
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
  private String brand;
  private String createdBy;
  private String updatedBy;
  private Boolean off2OnChannelActive;
  private boolean sharedProduct;
  private List<String> pickupPointCodes = new ArrayList<String>();
  private List<String> fbbPickupPointCodes = new ArrayList<String>();
  private List<BundleItemResponse> bundleItemResponses = new ArrayList<>();
}