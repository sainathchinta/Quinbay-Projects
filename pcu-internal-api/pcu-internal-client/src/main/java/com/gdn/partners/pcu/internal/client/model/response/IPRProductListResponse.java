package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IPRProductListResponse extends BaseResponse {

  private static final long serialVersionUID = 7899468942586338067L;

  private String productCode;
  private String productSku;
  private String productName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String commissionType;
  private boolean officialSeller;
  private boolean internationalFlag;
  private String categoryCode;
  private String categoryName;
  private String brandCode;
  private String brandName;
  private String state;
  private String assignedTo;
  private Date assignedDate;
  private Date productAddedDate;
  private String pdpRedirectionUrl;
  private String source;
}