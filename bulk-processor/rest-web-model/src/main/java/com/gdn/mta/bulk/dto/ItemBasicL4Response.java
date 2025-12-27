package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonInclude
public class ItemBasicL4Response extends BaseResponse {
  private String productSku;
  private String itemSku;
  private String upcCode;
  private String itemName;
}
