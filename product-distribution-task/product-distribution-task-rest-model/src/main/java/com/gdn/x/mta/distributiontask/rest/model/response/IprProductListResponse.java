package com.gdn.x.mta.distributiontask.rest.model.response;

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
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class IprProductListResponse extends BaseResponse {

  private static final long serialVersionUID = 8298878311733852124L;
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
