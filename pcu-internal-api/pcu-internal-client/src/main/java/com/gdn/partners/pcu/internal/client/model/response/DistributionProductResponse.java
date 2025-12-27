package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.RestrictedKeywordsByFieldVendor;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.util.Date;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class DistributionProductResponse extends BaseResponse {

  @Serial
  private static final long serialVersionUID = 3079319826887302359L;
  private Date productCreatedDate;
  private String productCode;
  private String productName;
  private String videoUrl;
  private String uom;
  private String categoryName;
  private String categoryCode;
  private String brand;
  private byte[] description;
  private byte[] longDescription;
  private String uniqueSellingPoint;
  private String productStory;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Double shippingWeight;
  private WorkflowWebState state;
  private Integer rejectedCount;
  private VendorDetailResponse currentVendor;
  private String vendorId;
  private String vendorName;
  private boolean productApproved;
  private String vendorCode;
  private String storeId;
  private String productId;
  private String productApproverAssignee;
  private Date productAssignedDate;
  private Date productApprovedDate;
  private boolean postLive;
  private boolean restrictedKeywordsPresent;
  private boolean enableImageFeedback;
  private boolean promoSKU;
  private String brandCode;
  private String brandApprovalStatus;
  private boolean edited;
  private ProductNotesResponse productNotes;
  private List<RestrictedKeywordsByFieldVendor> restrictedKeywordsDetected;
  private boolean revised;
  private boolean autoNeedRevision;
  private String reviewType;
  private List<String> imageViolations;
  private List<String> revisedFields;
  private List<String> editedFields;
  private Integer productType;
  private String predictedBrand;
  private Integer sellerType;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private boolean appealedProduct;
  private boolean showProductUrl;
  private String c1CategoryCode;
  private String c1CategoryName;
  private int distributionMappingStatus;
  private String productCreationType;
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;
}
