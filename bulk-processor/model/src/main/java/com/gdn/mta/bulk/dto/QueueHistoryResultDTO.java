package com.gdn.mta.bulk.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class QueueHistoryResultDTO extends GdnBaseDomainEventModel {

  private String requestId;
  private String merchantCode;
  private String gdnSku;
  private Boolean success;
  private String errorMessage;
  private String errorCode;
  private String value;
  private String actionDetail;
  private List<WarningMessageDTO> warningMessage;
}