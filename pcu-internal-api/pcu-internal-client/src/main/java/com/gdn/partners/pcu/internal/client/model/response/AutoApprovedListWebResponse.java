package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedListWebResponse extends BaseResponse {
  private static final long serialVersionUID = 3057241051094585314L;
  private String productCode;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String commissionType;
  private Long addedDate;
  private String assignedTo;
  private Long assignedDate;
  private boolean b2bActivated;
  private String sourceEn;
  private String sourceId;
  private String reason;
  private AutoApprovedSellerResponse seller;
}
