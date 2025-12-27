package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class HistoryUpdateResponse extends BaseResponse {

  private Date accessTime;
  private String gdnSku;
  private String gdnName;
  private String changedBy;
  private String oldValues;
  private String newValues;
  private String activity;
  private String pickupPointName;
  private String pickupPointCode;
}
