package com.gdn.x.mta.distributiontask.rest.model.response;

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
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class IprProductDetailsResponse extends BaseResponse {

  private static final long serialVersionUID = 4763331809510174442L;
  private String productCode;
  private String productSku;
  private String productName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String sellerBadge;
  private boolean officialSeller;
  private String imageUrl;
  private String state;
  private String assignedTo;
  private  String source;
  private EvidenceSubmittedDetailResponse evidenceSubmittedDetailResponse;
  private EvidenceRequestedDetailResponse evidenceRequestedDetailResponse;
  private List<HistoricalSellerDataResponse> historicalSellerData = new ArrayList<>();
  private BrandReportDetailResponse brandReportDetailResponse;
}
