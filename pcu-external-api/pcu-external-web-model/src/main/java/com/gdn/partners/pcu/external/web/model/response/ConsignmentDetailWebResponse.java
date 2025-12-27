package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConsignmentDetailWebResponse extends BaseResponse {
  private static final long serialVersionUID = -6165484136552588502L;
  private String consignmentCode;
  private Date createdDate;
  private ConsignmentStatusWeb status;

}