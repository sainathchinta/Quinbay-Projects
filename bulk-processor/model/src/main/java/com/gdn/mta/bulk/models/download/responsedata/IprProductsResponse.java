package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class IprProductsResponse extends BaseResponse {

  private static final long serialVersionUID = 5299484662644901855L;
  private String productCode;
  private String productSku;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String brandName;
  private String brandCode;
  private String businessPartnerCode;
  private Date productAddedDate;
  private String state;
  private String assignedTo;
  private Date assignedDate;
  private String source;
}