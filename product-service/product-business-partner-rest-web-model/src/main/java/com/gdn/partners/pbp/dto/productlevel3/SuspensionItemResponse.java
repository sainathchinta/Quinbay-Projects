package com.gdn.partners.pbp.dto.productlevel3;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SuspensionItemResponse extends BaseResponse {
  private String productCode;
  private String productName;
  private String productSku;
  private String itemSku;
  private String itemName;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Date bannedDate;
  private String reason;
  private String imageUrl;
  private String productDetailPageLink;

}
