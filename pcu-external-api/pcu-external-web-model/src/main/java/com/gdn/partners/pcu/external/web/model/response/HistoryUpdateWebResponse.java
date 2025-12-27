package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
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
public class HistoryUpdateWebResponse {

  private Date accessTime;
  private String gdnSku;
  private String gdnName;
  private String changedBy;
  private String oldValues;
  private String newValues;
  private String activity;
  private String pickupPointName;
}
