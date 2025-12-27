package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductDetailResponse extends BaseResponse {
  private static final long serialVersionUID = 1522710774237261789L;
  private String productCode;
  private String productSku;
  private String productName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String sellerBadge;
  private String imageUrl;
  private String state;
  private String assignedTo;
  private String pdpRedirectionUrl;
  private String bpContact;
  private boolean official;
  private EvidenceSubmittedDetailResponse evidenceSubmittedDetailResponse;
  private EvidenceRequestedDetailResponse evidenceRequestedDetailResponse;
  private List<HistoricalSellerDataResponse> historicalSellerData = new ArrayList<>();
  private String source;
  private BrandReportDetailResponse brandReportDetailResponse;
}
