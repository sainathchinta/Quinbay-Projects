package com.gdn.x.mta.distributiontask.rest.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class FilterSummaryRequest implements Serializable {


  private static final long serialVersionUID = 9186043580601414443L;

  private String keyword;
  private TimeFilterType timeFilterType;
  private Boolean contentPending;
  private Boolean imagePending;
  private Boolean assignment;
  private String categoryCode;
  private Boolean isCnCategory;
  private String businessPartnerCode;
  private String assigneeEmailId;
  private String sortOrderByCreatedDate = "asc";
  private String vendorCode;
  private Boolean postLive;
  private String faultyImageType;
  private Boolean brandPending;
  private Boolean edited;
  private Boolean revised;
  private Boolean restrictedKeyword;
  private Boolean b2cActivated;
  private Boolean b2bActivated;
  private Boolean appealedProduct;
}
