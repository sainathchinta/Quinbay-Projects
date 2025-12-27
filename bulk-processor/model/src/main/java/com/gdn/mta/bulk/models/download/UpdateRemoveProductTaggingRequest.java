package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateRemoveProductTaggingRequest extends BaseRequest {
  private static final long serialVersionUID = 5755272951610203930L;
  private String itemPickUpPointId;
  private String productType;
}