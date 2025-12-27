package com.gda.mta.product.dto.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class HistoryResponse extends BaseResponse {
  private Date accessTime;
  private String gdnSku;
  private String gdnName;
  private String changedBy;
  private String oldValues;
  private String newValues;
  private String activity;
}
